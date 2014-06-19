unit LuaObject;

{
  TLuaObject
  Copyright (c) 2007 Jeremy Darling
  Modifications copyright (c) 2010-2014 Felipe Daragon
  
  License: MIT (http://opensource.org/licenses/mit-license.php)
  
  Changes:
  * 17.06.2014, FD - Changed to work with string instead of ansistring.
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Lua, Variants, pLuaObject, pLua;

type
  TLuaObject = class;

  { TLuaObject }

  TLuaObject = class
  protected
    L : PLua_State;
    FLuaReference : integer;
    FParent : TLuaObject;
    FChildren : TList;
    
    function  GetLuaProp(PropName : String): Variant;
    procedure SetLuaProp(PropName : String; const AValue: Variant);
    function  GetPropValue(propName : String): Variant; virtual;
    function  GetPropObject(propName: String) : Boolean; virtual;
    function  SetPropValue(PropName : String; const AValue: Variant) : Boolean; virtual;
    function  SetPropObject(propName: String) : Boolean; virtual;
    function  PropIsObject(propName : String): Boolean; virtual;
    procedure CommonCreate(LuaState : PLua_State; AParent : TLuaObject = nil); virtual;
  public
    constructor Create(LuaState : PLua_State; AParent : TLuaObject = nil); overload; virtual;
    constructor Create(LuaState: PLua_State; LuaClassName, LuaName: String); overload; virtual;
    destructor Destroy; override;

    procedure PushSelf;

    procedure CallEvent(EventName : String); overload;
    function  CallEvent(EventName : String; args : Array of Variant; Results: PVariantArray = nil) : Integer; overload;
    function  EventExists(EventName: String): Boolean;

    property LState : PLua_State read L;
    property LRef:integer read FLuaReference;

    property LuaProp[PropName : String] : Variant read GetLuaProp write SetLuaProp;
  end;

  TLuaObjectRegisterMethodsCallback = procedure(L : Plua_State; classTable : Integer);
  TLuaObjectNewCallback = function(L : PLua_State; AParent : TLuaObject=nil):TLuaObject;

var
  LuaObjects : TList;

procedure ClearObjects;
procedure LuaCopyTable(L: Plua_State; IdxFrom, IdxTo, MtTo : Integer);
function  LuaToTLuaObject(L: Plua_State; Idx : Integer) : TLuaObject;
procedure RegisterLuaObject(L: Plua_State);

procedure RegisterTLuaObject(L : Plua_State; ObjectName : String; CreateFunc : lua_CFunction; MethodsCallback : TLuaObjectRegisterMethodsCallback = nil);
procedure RegisterObjectInstance(L : Plua_State; aClassName, InstanceName : String; ObjectInstance : TLuaObject);
procedure RegisterMethod(L : Plua_State; TheMethodName : String; TheMethodAddress : lua_CFunction; classTable : Integer);
function  new_LuaObject(L : PLua_State; aClassName : String; NewCallback : TLuaObjectNewCallback) : Integer; cdecl;

procedure PushTLuaObject(L : PLua_State; ObjectInstance : TLuaObject);

function  new_TLuaObject(L : PLua_State) : Integer; cdecl;
function  index_TLuaObject(L : PLua_State) : Integer; cdecl;
function  newindex_TLuaObject(L : PLua_State) : Integer; cdecl;
function  gc_TLuaObject(L : PLua_State) : Integer; cdecl;
procedure RegisterClassTLuaObject(L : Plua_State);

implementation

uses
  typinfo, types;

const
  LuaTLuaObjectClassName = 'TLuaObject';

constructor TLuaObject.Create(LuaState : PLua_State; AParent : TLuaObject = nil);
begin
  CommonCreate(LuaState, nil);
  // Create a reference to the object table, this way lua won't GC its version
  FLuaReference := luaL_ref(L, LUA_REGISTRYINDEX);
  lua_rawgeti (L, LUA_REGISTRYINDEX, FLuaReference);
  LuaObjects.Add(Self);
end;

constructor TLuaObject.Create(LuaState: PLua_State; LuaClassName, LuaName: String);
begin
  CommonCreate(LuaState, nil);
  RegisterObjectInstance(LuaState, LuaClassName, LuaName, self);
end;

destructor TLuaObject.Destroy;
var
  lo : TLuaObject;
begin
  LuaObjects.Remove(Self);
  if assigned(FParent) then
    FParent.FChildren.Remove(Self);
  while FChildren.Count > 0 do
    begin
      lo := TLuaObject(FChildren[FChildren.Count-1]);
      FChildren.Delete(FChildren.Count-1);
      lo.Free;
    end;
  FChildren.Free;
  luaL_unref(L, LUA_REGISTRYINDEX, FLuaReference);
  inherited Destroy;
end;

procedure TLuaObject.PushSelf;
begin
  lua_rawgeti(L, LUA_REGISTRYINDEX, FLuaReference);
end;

procedure TLuaObject.CallEvent(EventName: String);
begin
  CallEvent(EventName, []);
end;

function TLuaObject.CallEvent(EventName : String; args: array of Variant; Results: PVariantArray) : Integer;
begin
  result := -1;
  if not EventExists(EventName) then
    exit;
  PushSelf;
  result := plua_callfunction(L, ansistring(EventName), args, results, lua_gettop(L));
end;

function TLuaObject.EventExists(EventName: String): Boolean;
begin
  PushSelf;
  result := plua_functionexists(L, ansistring(EventName), lua_gettop(L));
  lua_pop(L, 1);
end;

function TLuaObject.GetLuaProp(PropName : String): Variant;
var
  idx : Integer;
begin
  lua_rawgeti (L, LUA_REGISTRYINDEX, FLuaReference); // Place our object on the stack
  idx := lua_gettop(L);
  lua_pushliteral(L, PropName); // Place the event name on the stack
  lua_gettable(L, idx); // try to get the item
  result := plua_tovariant(L, lua_gettop(L));
  lua_pop(L, 2);
end;

procedure TLuaObject.SetLuaProp(PropName : String; const AValue: Variant);
var
  idx : Integer;
begin
  lua_rawgeti (L, LUA_REGISTRYINDEX, FLuaReference); // Place our object on the stack
  idx := lua_gettop(L);
  lua_pushstring(L, propName);
  plua_pushvariant(L, AValue);
  lua_rawset(L, idx);
end;

function TLuaObject.GetPropValue(propName: String): Variant;
begin
  if IsPublishedProp(self, propName) then
    result := typinfo.GetPropValue(self, propName)
  else
    result := NULL;
end;

function TLuaObject.GetPropObject(propName: String) : Boolean;
begin
 result := false;
end;

function TLuaObject.SetPropValue(PropName: String; const AValue: Variant) : Boolean;
begin
  result := IsPublishedProp(self, propName);
  if result then
    typinfo.SetPropValue(self, propName, AValue);
end;

function TLuaObject.SetPropObject(propName: String) : Boolean;
begin
  result := false;
end;

function TLuaObject.PropIsObject(propName: String): Boolean;
begin
  result := false;
end;

procedure TLuaObject.CommonCreate(LuaState: PLua_State; AParent: TLuaObject);
begin
  L := LuaState;
  FParent := AParent;
  if assigned(FParent) then
    FParent.FChildren.Add(Self);
  FChildren := TList.Create;
end;

{ Global LUA Methods }

procedure LuaCopyTable(L: Plua_State; IdxFrom, IdxTo, MtTo : Integer);
var
  id:Integer;
  tbl : Integer;
  key, val : Variant;
  cf : lua_CFunction;
begin
  lua_pushnil(L);
  while(lua_next(L, IdxFrom)<>0)do
    begin
      key := plua_tovariant(L, -2);
      if CompareText(key, '__') = 1 then
        tbl := MtTo
      else
        tbl := IdxTo;
      case lua_type(L, -1) of
        LUA_TFUNCTION : begin
          cf := lua_tocfunction(L, -1);
          plua_pushvariant(L, key);
          lua_pushcfunction(L, cf);
          lua_rawset(L, tbl);
        end;
        LUA_TTABLE    : begin
          id := lua_gettop(L);
          LuaCopyTable(L, id, IdxTo, MtTo);
        end;
      else
        val := plua_tovariant(L, -1);
        plua_pushvariant(L, key);
        plua_pushvariant(L, val);
        lua_rawset(L, tbl);
      end;
      lua_pop(L, 1);
    end;
end;

function LuaToTLuaObject(L: Plua_State; Idx : Integer) : TLuaObject;
begin
  result := nil;
  if lua_type(L, Idx) = LUA_TTABLE then
    begin
      Idx := plua_absindex(L, Idx);
      lua_pushstring(L, '_Self');
      lua_gettable(L, Idx);
      result := TLuaObject(ptrint(lua_tointeger(L, -1)));
      lua_pop(L, 1);
    end
  else
    luaL_error(L, PAnsiChar('Class table expected.'));
end;

procedure PushTLuaObject(L: PLua_State; ObjectInstance: TLuaObject);
begin
  lua_rawgeti(L, LUA_REGISTRYINDEX, ObjectInstance.FLuaReference);
end;

function new_TLuaObject(L : PLua_State) : Integer; cdecl;
var
  P, E : TLuaObject;
  n, idx, idx2, mt : Integer;
begin
  n := lua_gettop(L);
  if lua_type(L, 1) <> LUA_TTABLE then
    lua_remove(L, 1);
  if n = 1 then
    P := LuaToTLuaObject(L, 1)
  else
    P := nil;
    
  lua_newtable(L);
  E := TLuaObject.Create(L, P);
  idx := lua_gettop(L);

  lua_pushliteral(L, '_Self');
  lua_pushinteger(L, PtrInt(Pointer(E)));
  lua_rawset(L, idx);

  lua_newtable(L);
  mt := lua_gettop(L);

  lua_pushliteral(L, LuaTLuaObjectClassName);
  lua_gettable(L, LUA_GLOBALSINDEX);
  idx2 := lua_gettop(L);

  LuaCopyTable(L, idx2, idx, mt);
  lua_setmetatable(L, idx);
  
  lua_pop(L, 1);

  result := 1;
end;

function index_TLuaObject(L : PLua_State) : Integer; cdecl;
var
  E : TLuaObject;
  propName : String;
  v : Variant;
begin
  E := LuaToTLuaObject(L, 1);
  lua_remove(L, 1);
  if E = nil then
    begin
      result := 0;
      exit;
    end;
  propName := string(plua_tostring(L, 1));
  index_TLuaObject := 1;
  if E.PropIsObject(propName) then
    begin
      if not E.GetPropObject(propName) then
        index_TLuaObject := 0;
    end
  else
    begin
      v := E.GetPropValue(propName);
      if v = NULL then
        index_TLuaObject := 0
      else
        plua_pushvariant(L, v);
    end;
end;

function newindex_TLuaObject(L : PLua_State) : Integer; cdecl;
var
  TableIndex, ValueIndex : Integer;
  E : TLuaObject;
  propName : String;
begin
  result := 0;
  E := LuaToTLuaObject(L, 1);
  if E = nil then
    begin
      exit;
    end;
  propName := string(plua_tostring(L, 2));
  if E.PropIsObject(propName) and E.SetPropObject(propName) then
  else if not E.SetPropValue(propName, plua_tovariant(L, 3)) then
    begin
    // This is a standard handler, no value was found in the object instance
    // so we push the value into the Lua Object reference.
      TableIndex := plua_absindex(L, 1);
      ValueIndex := plua_absindex(L, 3);
      lua_pushstring(L, propName);
      lua_pushvalue(L, ValueIndex);
      lua_rawset(L, TableIndex);
    end;
end;

function gc_TLuaObject(L : PLua_State) : Integer; cdecl;
var
  E : TLuaObject;
begin
  E := LuaToTLuaObject(L, 1);
  // Release the object
  if assigned(E) then
    E.Free;
  result := 0;
end;

procedure RegisterObjectInstance(L: Plua_State; aClassName, InstanceName: String; ObjectInstance : TLuaObject);
var
  idx, idx2, mt : Integer;
begin
  lua_pushliteral(L, InstanceName);
  lua_newtable(L);

  ObjectInstance.FLuaReference := luaL_ref(L, LUA_REGISTRYINDEX);
  lua_rawgeti (L, LUA_REGISTRYINDEX, ObjectInstance.FLuaReference);
  LuaObjects.Add(ObjectInstance);
  idx := lua_gettop(L);

  lua_pushliteral(L, '_Self');
  lua_pushinteger(L, PtrInt(Pointer(ObjectInstance)));
  lua_rawset(L, idx);

  lua_newtable(L);
  mt := lua_gettop(L);

  lua_pushliteral(L, aClassName);
  lua_gettable(L, LUA_GLOBALSINDEX);
  idx2 := lua_gettop(L);

  LuaCopyTable(L, idx2, idx, mt);
  lua_setmetatable(L, idx);

  lua_pop(L, 1);

  lua_settable(L, LUA_GLOBALSINDEX);
end;

procedure RegisterMethod(L : Plua_State; TheMethodName : String; TheMethodAddress : lua_CFunction; classTable : Integer);
begin
  lua_pushliteral(L, TheMethodName);
  lua_pushcfunction(L, TheMethodAddress);
  lua_rawset(L, classTable);
end;

function new_LuaObject(L : PLua_State; aClassName : String; NewCallback : TLuaObjectNewCallback): Integer; cdecl;
var
  P, E : TLuaObject;
  n, idx, idx2, mt : Integer;
begin
  n := lua_gettop(L);
  if lua_type(L, 1) <> LUA_TTABLE then
    lua_remove(L, 1);
  if n > 1 then
    P := LuaToTLuaObject(L, 2)
  else
    P := nil;

  lua_newtable(L);
  E := NewCallback(L, P);
  idx := lua_gettop(L);

  lua_pushliteral(L, '_Self');
  lua_pushinteger(L, PtrInt(Pointer(E)));
  lua_rawset(L, idx);

  lua_newtable(L);
  mt := lua_gettop(L);

  lua_pushliteral(L, aClassName);
  lua_gettable(L, LUA_GLOBALSINDEX);
  idx2 := lua_gettop(L);

  LuaCopyTable(L, idx2, idx, mt);
  lua_setmetatable(L, idx);

  lua_pop(L, 1);

  result := 1;
end;

procedure RegisterClassTLuaObject(L : Plua_State);
var
  classTable : Integer;
begin
  lua_pushstring(L, LuaTLuaObjectClassName);
  lua_newtable(L);
  classTable := lua_gettop(L);

  RegisterMethod(L, '__index', @index_TLuaObject, classTable);
  RegisterMethod(L, '__newindex', @newindex_TLuaObject, classTable);
  RegisterMethod(L, '__call', @new_TLuaObject, classTable);
  RegisterMethod(L, '__gc', @gc_TLuaObject, classTable);
  RegisterMethod(L, 'release', @gc_TLuaObject, classTable);
  RegisterMethod(L, 'new', @new_TLuaObject, classTable);

  lua_settable(L, LUA_GLOBALSINDEX);
end;

{ Global Management Methods }

procedure RegisterTLuaObject(L: Plua_State; ObjectName : String;
  CreateFunc : lua_CFunction;
  MethodsCallback: TLuaObjectRegisterMethodsCallback);
var
  classTable : Integer;
begin
  lua_pushstring(L, ObjectName);
  lua_newtable(L);
  classTable := lua_gettop(L);

  RegisterMethod(L, '__index', @index_TLuaObject, classTable);
  RegisterMethod(L, '__newindex', @newindex_TLuaObject, classTable);
  RegisterMethod(L, '__call', CreateFunc, classTable);
  RegisterMethod(L, '__gc', @gc_TLuaObject, classTable);
  RegisterMethod(L, 'release', @gc_TLuaObject, classTable);
  RegisterMethod(L, 'new', CreateFunc, classTable);

  if Assigned(MethodsCallback) then
    MethodsCallback(L, classTable);

  lua_settable(L, LUA_GLOBALSINDEX);
end;

procedure ClearObjects;
begin
  while LuaObjects.Count > 0 do
    TLuaObject(LuaObjects[LuaObjects.Count-1]).Free;
end;

procedure RegisterLuaObject(L: Plua_State);
begin
  RegisterClassTLuaObject(L);
end;

initialization
  LuaObjects := TList.Create;

finalization
  ClearObjects;
  LuaObjects.Free;

end.
