unit pLuaRecord;

{
  Copyright (c) 2007 Jeremy Darling
  Modifications copyright (c) 2010-2014 Felipe Daragon
  
  License: MIT (http://opensource.org/licenses/mit-license.php)

  Changes:
  * 26.06.2014, FD - Changed to work with string instead of ansistring.
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$TYPEDADDRESS ON}
{$ENDIF}

interface

{$I Lua.inc}

uses
  Classes, SysUtils, Lua, pLua, uWordList, pLuaObject, Types;
  
type
  PLuaRecordInfo = ^TLuaRecordInfo;
  PLuaRecordInstanceInfo = ^TLuaRecordInstanceInfo;

  plua_RecordMethodWrapper  = function(RecordPointer : pointer; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
  plua_RecordPropertyReader = plua_RecordMethodWrapper;
  plua_RecordPropertyWriter = plua_RecordMethodWrapper;
  plua_RecordConstructor    = function(l : Plua_State; paramidxstart, paramcount : integer; InstanceInfo : PLuaRecordInstanceInfo) : Pointer;
  plua_RecordDestructor     = procedure( RecordPointer : pointer; l : Plua_State );

  PLuaRecordProperty= ^TLuaRecordProperty;
  TLuaRecordProperty = record
    PropName : String;
    Reader   : plua_RecordPropertyReader;
    Writer   : plua_RecordPropertyWriter;
  end;

  TLuaRecordInfo = record
    Parent      : PLuaRecordInfo;
    RecordName  : String;
    PropHandlers: TWordList;
    New         : plua_RecordConstructor;
    Release     : plua_RecordDestructor;
    Properties  : Array of TLuaRecordProperty;
  end;
  
  TLuaRecordInstanceInfo = record
    OwnsInstance : Boolean;
    LuaRef       : Integer;
    RecordInfo   : PLuaRecordInfo;
    l            : PLua_state;
    RecordPointer: Pointer;
  end;

  { TLuaRecordList }

  TLuaRecordList = class
    fItems : TList;
  private
    function GetRecordInfo(index : integer): PLuaRecordInfo;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    function  GetPropReader(aRecordInfo : PLuaRecordInfo; aPropertyName : String) : plua_RecordPropertyReader;
    function  GetPropWriter(aRecordInfo : PLuaRecordInfo; aPropertyName : String; out ReadOnly : Boolean) : plua_RecordPropertyWriter;

    function  GetInfo(l : PLua_State; RecordPointer: Pointer) : PLuaRecordInstanceInfo;

    function  Add(aRecordInfo : TLuaRecordInfo) : Integer;
    procedure Remove(aRecordName : String);
    function  IndexOf(aRecordName : String) : Integer;
    procedure Clear;
    property  Count : integer read GetCount;
    property  RecordInfo[index : integer]:PLuaRecordInfo read GetRecordInfo; default;
  end;

  { TLuaClassTypesList }

  TLuaRecordTypesList = class
    fItems : TWordList;
    fItemList : TList;
  private
    function GetCount: Integer;
    function GetIndexedItem(index : integer): PLuaRecordInfo;
    function GetItem(ItemName : String): PLuaRecordInfo;
  public
    constructor Create;
    destructor Destroy; override;

    function  Add(ItemName : String; LuaParent : PLuaRecordInfo = nil) : PLuaRecordInfo;
    procedure Remove(ItemName : String);
    procedure Clear;

    procedure RegisterTo(L : PLua_State);

    property Item[ItemName : String] : PLuaRecordInfo read GetItem; default;
    property IndexedItem[index : integer] : PLuaRecordInfo read GetIndexedItem;
    property Count : Integer read GetCount;
  end;

procedure plua_registerRecordType( l : PLua_State; RecordInfo : TLuaRecordInfo);
procedure plua_newRecordInfo( var RecordInfoPointer : PLuaRecordInfo);
procedure plua_initRecordInfo( var RecordInfo : TLuaRecordInfo);
procedure plua_releaseRecordInfo( var RecordInfoPointer : PLuaRecordInfo);

function plua_registerExistingRecord( l : PLua_State; InstanceName : String;
                                      RecordPointer: Pointer;
                                      RecordInfo : PLuaRecordInfo;
                                      FreeOnGC : Boolean = false) : PLuaRecordInstanceInfo;

function plua_pushexisting( l : PLua_State;
                            RecordPointer: Pointer;
                            RecordInfo : PLuaRecordInfo;
                            FreeOnGC : Boolean = false) : PLuaRecordInstanceInfo;

procedure plua_AddRecordProperty( var RecordInfo : TLuaRecordInfo;
                                 propertyName : String;
                                 Reader   : plua_RecordPropertyReader;
                                 Writer   : plua_RecordPropertyWriter );

function plua_getRecord( l : PLua_State; idx : Integer) : Pointer;
function plua_getRecordInfo( l : PLua_State; idx : Integer) : PLuaRecordInstanceInfo; overload;
procedure plua_PushRecord(RecordInfo : PLuaRecordInstanceInfo);
function  plua_GetRecordInfo( l : PLua_State; RecordPointer : Pointer) : PLuaRecordInstanceInfo; overload;

procedure plua_PushRecordToTable( L : PLua_State; RecordPointer : Pointer;
                                  RecordInfo : PLuaRecordInfo );

procedure plua_ClearRecords( L : PLua_State );

var
  LuaRecords : TLuaRecordList;
  RecordTypesList : TLuaRecordTypesList;
  
implementation

var
  intLuaRecords : TList;

function plua_gc_record(l : PLua_State) : integer; cdecl; forward;

function plua_index_record(l : PLua_State) : integer; cdecl;
var
  propName : String;
  propValueStart : Integer;
  rec      : pointer;
  rInfo    : PLuaRecordInstanceInfo;
  reader   : plua_RecordPropertyReader;
  pcount   : Integer;
begin
  result := 0;
  pcount := lua_gettop(l);
  if not lua_istable(l, 1) then
    exit;

  rInfo := plua_GetRecordInfo(l, 1);
  if not assigned(rInfo) then
    exit;
  rec := rInfo^.RecordPointer;

  propName := lua_tostring(l, 2);
  propValueStart := 3;
  reader := LuaRecords.GetPropReader(rInfo^.recordInfo, propName);
  if assigned(reader) then
    result := reader(rec, l, propValueStart, pcount);
end;

function plua_newindex_record(l : PLua_State) : integer; cdecl;
var
  propName : String;
  propValueStart : Integer;
  rec      : pointer;
  rInfo    : PLuaRecordInstanceInfo;
  writer   : plua_RecordPropertyWriter;
  bReadOnly: Boolean;
  pcount   : Integer;
begin
  result := 0;
  pcount := lua_gettop(l);
  if not lua_istable(l, 1) then
    exit;

  rInfo := plua_GetRecordInfo(l, 1);
  if not assigned(rInfo) then
    exit;
  rec := rInfo^.RecordPointer;

  propName := lua_tostring(l, 2);
  propValueStart := 3;
  writer := LuaRecords.GetPropWriter(rInfo^.recordInfo, propName, bReadOnly);
  if assigned(writer) then
    result := writer(rec, l, propValueStart, pcount)
  else
    begin
      if not bReadOnly then
        begin
          lua_pushstring(l, propName);
          lua_pushvalue(l, propValueStart);
          lua_rawset(l, 1);
        end;
    end;
end;

function plua_new_record(l : PLua_State) : integer; cdecl;
var
  n, tidx, oidx    : Integer;
  recordPTR: Pointer;
  rInfo   : PLuarecordInfo;
  instance: PLuaRecordInstanceInfo;
  pcount  : integer;
begin
  result := 0;
  pcount := lua_gettop(l);
  n := lua_gettop(l);
  if (n < 1) or (not (lua_istable(l, 1))) then
    exit;

  tidx := 1;

  lua_pushstring(l, '__recordPTR');
  lua_rawget(l, tidx);
  recordPTR := pointer(PtrInt(lua_tointeger(l, -1)));
  rInfo := PLuarecordInfo(recordPTR);
  lua_pop(l, 1);

  new(instance);
  instance^.OwnsInstance := true;
  instance^.recordInfo := rInfo;
  instance^.l := l;
  instance^.RecordPointer := rInfo^.New(l, 2, pcount, instance);
  intLuaRecords.Add(pointer(instance));

  lua_newtable(L);
  instance^.LuaRef := luaL_ref(L, LUA_REGISTRYINDEX);
  lua_rawgeti(l, LUA_REGISTRYINDEX, instance^.LuaRef);
  oidx := lua_gettop(L);

  lua_pushliteral(L, '__instance');
  lua_pushinteger(L, PtrInt(instance));
  lua_rawset(l, oidx);

  lua_pushstring(L, 'release');
  lua_pushcfunction(L, @plua_gc_record);
  lua_rawset(L, oidx);

  luaL_getmetatable(l, PAnsiChar(AnsiString(rInfo^.recordName)+'_mt'));
  lua_setmetatable(l, -2);

  result := 1;
end;

function plua_gc_record(l : PLua_State) : integer; cdecl;
var
  nfo : PLuaRecordInstanceInfo;
begin
  result := 0;
  nfo := plua_GetRecordInfo(l, 1);
  if not assigned(nfo) then
    exit;
  intLuaRecords.Remove(nfo);
  if nfo^.OwnsInstance then
    begin
      nfo^.RecordInfo^.Release(nfo^.RecordPointer, l);
      nfo^.RecordPointer := nil;
    end;
  luaL_unref(L, LUA_REGISTRYINDEX, nfo^.LuaRef);
  freemem(nfo);
end;

procedure plua_registerRecordType(l: PLua_State; RecordInfo: TLuaRecordInfo);
var
  lidx, tidx, midx: integer;
  ci   : PLuaRecordInfo;
begin
  lidx := LuaRecords.Add(RecordInfo);

  lua_pushstring(l, RecordInfo.RecordName);
  lua_newtable(l);

  luaL_newmetatable(l, PAnsiChar(AnsiString(RecordInfo.RecordName)+'_mt'));
  lua_setmetatable(l, -2);
  lua_settable(l, LUA_GLOBALSINDEX);

  luaL_getmetatable(l, PAnsiChar(AnsiString(RecordInfo.RecordName)+'_mt'));
  midx := lua_gettop(l);

  lua_pushstring(l, RecordInfo.RecordName);
  lua_gettable(l, LUA_GLOBALSINDEX);
  tidx := lua_gettop(l);

  lua_pushstring(L, '__call');
  lua_pushcfunction(L, @plua_new_record);
  lua_rawset(L, midx);
  lua_pushstring(L, '__gc');
  lua_pushcfunction(L, @plua_gc_record);
  lua_rawset(L, midx);

  lua_pushstring(L, 'new');
  lua_pushcfunction(L, @plua_new_record);
  lua_rawset(L, tidx);

  lua_pushstring(L, '__recordID');
  lua_pushinteger(L, lidx);
  lua_rawset(L, tidx);
  lua_pushstring(L, '__recordPTR');
  ci := LuaRecords.RecordInfo[lidx];
  lua_pushinteger(L, PtrInt(ci));
  lua_rawset(L, tidx);

  lua_pushstring(L, '__index');
  lua_pushcfunction(L, @plua_index_record);
  lua_rawset(L, midx);
  lua_pushstring(L, '__newindex');
  lua_pushcfunction(L, @plua_newindex_record);
  lua_rawset(L, midx);
end;

procedure plua_newRecordInfo(var RecordInfoPointer: PLuaRecordInfo);
begin
  if RecordInfoPointer = nil then
    new(RecordInfoPointer);
  plua_initRecordInfo(RecordInfoPointer^);
end;

procedure plua_initRecordInfo(var RecordInfo: TLuaRecordInfo);
begin
  RecordInfo.RecordName := '';
  RecordInfo.Parent     := nil;
  RecordInfo.PropHandlers := TWordList.Create;
  RecordInfo.New        := nil;
  RecordInfo.Release    := nil;
  SetLength(RecordInfo.Properties, 0);
end;

procedure plua_releaseRecordInfo(var RecordInfoPointer: PLuaRecordInfo);
begin
  RecordInfoPointer^.PropHandlers.Free;
  Freemem(RecordInfoPointer);
end;

function plua_registerExistingRecord(l: PLua_State; InstanceName: String;
  RecordPointer: Pointer; RecordInfo: PLuaRecordInfo; FreeOnGC: Boolean
  ): PLuaRecordInstanceInfo;
var
  oidx    : Integer;
  rInfo   : PLuaRecordInfo;
  instance: PLuaRecordInstanceInfo;
begin
  rInfo := RecordInfo;
  
  instance := plua_GetRecordInfo(l, RecordPointer);
  if assigned(instance) then
    begin
      result := instance;
      exit;
    end;

  new(instance);
  result := instance;
  instance^.OwnsInstance := FreeOnGC;
  instance^.RecordInfo := rInfo;
  instance^.l := l;

  instance^.RecordPointer := RecordPointer;
  intLuaRecords.Add(pointer(instance));

  lua_pushstring(l, InstanceName);
  lua_newtable(L);
  instance^.LuaRef := luaL_ref(L, LUA_REGISTRYINDEX);
  lua_rawgeti(l, LUA_REGISTRYINDEX, instance^.LuaRef);
  oidx := lua_gettop(L);

  lua_pushliteral(L, '__instance');
  lua_pushinteger(L, PtrInt(instance));
  lua_rawset(l, oidx);

  lua_pushstring(L, 'release');
  lua_pushcfunction(L, @plua_gc_record);
  lua_rawset(L, oidx);

  luaL_getmetatable(l, PAnsiChar(AnsiString(rInfo^.RecordName)+'_mt'));
  lua_setmetatable(l, -2);

  lua_settable(l, LUA_GLOBALSINDEX );
end;

function plua_pushexisting(l: PLua_State; RecordPointer: Pointer;
  RecordInfo: PLuaRecordInfo; FreeOnGC: Boolean): PLuaRecordInstanceInfo;
var
  oidx    : Integer;
  rInfo   : PLuaRecordInfo;
  instance: PLuaRecordInstanceInfo;
begin
  instance := plua_GetRecordInfo(l, RecordPointer);
  if assigned(instance) then
    begin
      plua_PushRecord(instance);
      result := instance;
      exit;
    end;

  rInfo := RecordInfo;

  new(instance);
  result := instance;
  instance^.OwnsInstance := FreeOnGC;
  instance^.RecordInfo := rInfo;
  instance^.l := l;
  instance^.RecordPointer := RecordPointer;

  intLuaRecords.Add(pointer(instance));

  lua_newtable(L);
  instance^.LuaRef := luaL_ref(L, LUA_REGISTRYINDEX);
  lua_rawgeti(l, LUA_REGISTRYINDEX, instance^.LuaRef);
  oidx := lua_gettop(L);

  lua_pushliteral(L, '__instance');
  lua_pushinteger(L, PtrInt(instance));
  lua_rawset(l, oidx);

  lua_pushstring(L, 'release');
  lua_pushcfunction(L, @plua_gc_record);
  lua_rawset(L, oidx);

  luaL_getmetatable(l, PAnsiChar(AnsiString(rinfo^.RecordName)+'_mt'));
  lua_setmetatable(l, -2);
end;

procedure plua_AddRecordProperty(var RecordInfo: TLuaRecordInfo;
  propertyName: String; Reader: plua_RecordPropertyReader;
  Writer: plua_RecordPropertyWriter);
var
  idx : integer;
begin
  idx := Length(RecordInfo.Properties);
  SetLength(RecordInfo.Properties, idx+1);
  RecordInfo.Properties[idx].PropName := propertyName;
  RecordInfo.Properties[idx].Reader   := Reader;
  RecordInfo.Properties[idx].Writer   := Writer;
  RecordInfo.PropHandlers.AddWord(ansistring(propertyName))^.data := pointer(PtrInt(idx));
end;

function plua_getRecord(l: PLua_State; idx: Integer): Pointer;
var
  instance : PLuaRecordInstanceInfo;
begin
  result := nil;
  lua_pushstring(l, '__instance');
  lua_rawget(l, plua_absindex(l, idx));
  instance := PLuaRecordInstanceInfo(ptrint(lua_tointeger(l, -1)));
  lua_pop(l, 1);
  if assigned(instance) and assigned(instance^.RecordPointer) then
    result := instance^.RecordPointer;
end;

function plua_getRecordInfo(l: PLua_State; idx: Integer
  ): PLuaRecordInstanceInfo;
begin
  //result := nil;
  lua_pushstring(l, '__instance');
  lua_rawget(l, plua_absindex(l, idx));
  result := PLuaRecordInstanceInfo(ptrint(lua_tointeger(l, -1)));
  lua_pop(l, 1);
end;

procedure plua_PushRecord(RecordInfo: PLuaRecordInstanceInfo);
begin
  lua_rawgeti(RecordInfo^.l, LUA_REGISTRYINDEX, RecordInfo^.LuaRef);
end;

function plua_GetRecordInfo(l : PLua_State; RecordPointer: Pointer): PLuaRecordInstanceInfo;
begin
  result := LuaRecords.GetInfo(l, RecordPointer);
end;

procedure plua_PushRecordToTable(L: PLua_State; RecordPointer: Pointer;
  RecordInfo: PLuaRecordInfo);
var
  i, tblIdx : Integer;
begin
  lua_newtable(L);
  tblIdx := lua_gettop(L);
  for i := 0 to Length(RecordInfo^.Properties) -1 do
    if assigned(RecordInfo^.Properties[i].Writer) then
      begin
        lua_pushstring(L, RecordInfo^.Properties[i].PropName);
        RecordInfo^.Properties[i].Writer(RecordPointer, L, 0, 0);
        lua_settable(l, tblidx);
      end;
end;

procedure plua_ClearRecords(L: PLua_State);
var
  i   : Integer;
  nfo : PLuaRecordInstanceInfo;
begin
  i := intLuaRecords.Count-1;
  while i > -1 do
    begin
      nfo := PLuaRecordInstanceInfo(intLuaRecords[i]);
      if nfo^.l = l then
        intLuaRecords.Remove(nfo);
      dec(i);
    end;
end;

{ TLuaRecordList }

function TLuaRecordList.GetRecordInfo(index: integer): PLuaRecordInfo;
begin
  result := PLuaRecordInfo(fItems[index]);
end;

function TLuaRecordList.GetCount: integer;
begin
  result := fItems.Count;
end;

constructor TLuaRecordList.Create;
begin
  fItems := TList.Create;
end;

destructor TLuaRecordList.Destroy;
begin
  Clear;
  fItems.Free;
  inherited Destroy;
end;

function TLuaRecordList.GetPropReader(aRecordInfo: PLuaRecordInfo;
  aPropertyName: String): plua_RecordPropertyReader;
var
  pi : PtrInt;
  ei : PWordListSymbol;
begin
// TODO - Add parent property calls in
  result := nil;
  ei := aRecordInfo^.PropHandlers.WordSymbol[ansistring(aPropertyName)];
  if not assigned(ei) then
    exit;
  pi := PtrInt(ei^.data);
  if (pi >= 0) and (pi < length(aRecordInfo^.Properties)) then
    result := aRecordInfo^.Properties[pi].Reader;
end;

function TLuaRecordList.GetPropWriter(aRecordInfo: PLuaRecordInfo;
  aPropertyName: String; out ReadOnly: Boolean): plua_RecordPropertyWriter;
var
  pi : PtrInt;
  ei : PWordListSymbol;
begin
// TODO - Add parent property calls in
  ReadOnly := false;
  result := nil;
  ei := aRecordInfo^.PropHandlers.WordSymbol[ansistring(aPropertyName)];
  if not assigned(ei) then
    exit;
  pi := PtrInt(ei^.data);
  if (pi >= 0) and (pi < length(aRecordInfo^.Properties)) then
    begin
      ReadOnly := @aRecordInfo^.Properties[pi].Writer = nil;
      result := aRecordInfo^.Properties[pi].Writer;
    end;
end;

function TLuaRecordList.GetInfo(l : PLua_State; RecordPointer: Pointer
  ): PLuaRecordInstanceInfo;
var
  i : Integer;
begin
  result := nil;
  i := 0;
  while (result = nil) and (i < intLuaRecords.Count) do
    begin
      if (PLuaRecordInstanceInfo(intLuaRecords[i])^.RecordPointer = RecordPointer) and
         (PLuaRecordInstanceInfo(intLuaRecords[i])^.l = l) then
        result := PLuaRecordInstanceInfo(intLuaRecords[i]);
      inc(i);
    end;
end;

function TLuaRecordList.Add(aRecordInfo: TLuaRecordInfo): Integer;
var
  ri  : PLuaRecordInfo;
begin
  result := IndexOf(aRecordInfo.RecordName);
  if result = -1 then
    begin
      new(ri);
      result := fItems.Add(ri);
    end
  else
    ri := RecordInfo[result];
  ri^ := aRecordInfo;
end;

procedure TLuaRecordList.Remove(aRecordName: String);
var
  idx : integer;
  ri  : PLuaRecordInfo;
begin
  idx := IndexOf(aRecordName);
  if idx > -1 then
    begin
      ri := RecordInfo[idx];
      fItems.Delete(idx);
      Freemem(ri);
    end;
end;

function TLuaRecordList.IndexOf(aRecordName: String): Integer;
var
  i : Integer;
begin
  result := -1;
  i := 0;
  while (result = -1) and (i < count) do
    begin
      if CompareText(aRecordName, RecordInfo[i]^.RecordName) = 0 then
        result := i;
      inc(i);
    end;
end;

procedure TLuaRecordList.Clear;
var
  ri : PLuaRecordInfo;
begin
  while count > 0 do
    begin
      ri := RecordInfo[count-1];
      fItems.Delete(count-1);
      Freemem(ri);
    end;
end;

{ TLuaRecordTypesList }

function TLuaRecordTypesList.GetCount: Integer;
begin
  result := fItemList.Count;
end;

function TLuaRecordTypesList.GetIndexedItem(index : integer): PLuaRecordInfo;
begin
  result := PLuaRecordInfo(fItemList[index]);
end;

function TLuaRecordTypesList.GetItem(ItemName : String): PLuaRecordInfo;
begin
  result := PLuaRecordInfo(fItems.WordData[AnsiString(ItemName)]);
end;

constructor TLuaRecordTypesList.Create;
begin
  fItems := TWordList.Create;
  fItemList := TList.Create;
end;

destructor TLuaRecordTypesList.Destroy;
begin
  Clear;
  fItems.Free;
  fItemList.Free;
  inherited Destroy;
end;

function TLuaRecordTypesList.Add(ItemName: String; LuaParent : PLuaRecordInfo = nil): PLuaRecordInfo;
begin
  result := PLuaRecordInfo(fItems.WordData[AnsiString(ItemName)]);
  if not assigned(result) then
    begin
      plua_newRecordInfo(result);
      result^.Parent := LuaParent;
      result^.RecordName := ItemName;
      fItems.AddWord(AnsiString(ItemName))^.data := result;
      fItemList.Add(result);
    end;
end;

procedure TLuaRecordTypesList.Remove(ItemName: String);
var
  wd : PWordListSymbol;
  ci : PLuaRecordInfo;
begin
  wd := fItems.WordSymbol[AnsiString(ItemName)];
  if (assigned(wd)) and (assigned(wd^.data)) and (wd^.eow) then
    begin
      ci := PLuaRecordInfo(wd^.data);
      fItemList.Remove(wd^.data);
      wd^.data := nil;
      wd^.eow := false;
      plua_releaseRecordInfo(ci);
    end;
end;

procedure TLuaRecordTypesList.Clear;
begin
  while Count > 0 do
    Remove(IndexedItem[Count-1]^.RecordName);
end;

procedure TLuaRecordTypesList.RegisterTo(L: PLua_State);
var
  i : Integer;
begin
  for i := 0 to Count-1 do
    plua_registerRecordType(l, IndexedItem[i]^);
end;

var
  instance : PLuaRecordInstanceInfo;

initialization
  RecordTypesList := TLuaRecordTypesList.Create;
  LuaRecords := TLuaRecordList.Create;
  intLuaRecords := TList.Create;

finalization
  RecordTypesList.Free;
  LuaRecords.Free;
  LuaRecords := nil;
  while intLuaRecords.Count > 0 do
    begin
      instance := PLuaRecordInstanceInfo(intLuaRecords[intLuaRecords.Count-1]);
      intLuaRecords.Delete(intLuaRecords.Count-1);
      if instance^.OwnsInstance then
        instance^.RecordInfo^.Release(instance, nil);
      Freemem(instance);
    end;
  intLuaRecords.Free;
  intLuaRecords := nil;

end.
