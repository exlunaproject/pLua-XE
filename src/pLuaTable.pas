{
  Useful functions for getting/setting the value of Lua table fields

  Copyright (c) 2003-2020 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

unit pLuaTable;

interface

{$I Lua.inc}

uses
  Classes, Lua, pLua;

type
  TLuaTable = class
  private
    fLuaState: PLua_State;
    fTableIndex: integer;
  public
    function ReadString(FieldName: string; ADefaultValue: string = ''): string;
    function ReadInteger(FieldName: string; ADefaultValue: integer): integer;
    function ReadBool(FieldName: string; ADefaultValue: boolean): boolean;
    function ReadVariant(FieldName: string; ADefaultValue: Variant): Variant;
    procedure GetIndexFromTop;
    constructor Create(L: PLua_State; GetIdxFromTop: boolean = false);
    destructor Destroy; override;
    property TableIndex: integer read fTableIndex;
  end;

  // Gets the value of a table field
function plua_GetFieldValueStr(L: PLua_State; idx: integer; FieldName: string;
  ADefaultValue: string = ''): string;
function plua_GetFieldValueInt(L: PLua_State; idx: integer; FieldName: string;
  ADefaultValue: integer): integer;
function plua_GetFieldValueBool(L: PLua_State; idx: integer; FieldName: string;
  ADefaultValue: boolean): boolean;
function plua_GetFieldValueType(L: PLua_State; idx: integer; FieldName: string):
  integer;
function plua_GetFieldValueVariant(L: PLua_State; idx: integer;
  FieldName: string; ADefaultValue: Variant): Variant;

// Sets the value of a table field
procedure plua_SetFieldValue(L: PLua_State; FieldName: string;
  AValue: string); overload;
procedure plua_SetFieldValue(L: PLua_State; FieldName: string;
  AValue: integer); overload;
procedure plua_SetFieldValue(L: PLua_State; FieldName: string;
  AValue: boolean); overload;
procedure plua_SetFieldValue(L: PLua_State; FieldName: string;
  ATable: plual_reg); overload;
procedure plua_SetFieldValue(L: PLua_State; Name: string; Reader: lua_CFunction;
  Writer: lua_CFunction); overload;
procedure plua_SetFieldValueV(L: PLua_State; FieldName: string;
  AValue: Variant);

implementation

procedure plua_SetFieldValue(L: PLua_State; FieldName: string;
  AValue: string); overload;
begin
  lua_pushstring(L, AValue);
  lua_setfield(L, -2, FieldName);
end;

procedure plua_SetFieldValue(L: PLua_State; FieldName: string;
  AValue: integer); overload;
begin
  plua_pushintnumber(L, AValue);
  lua_setfield(L, -2, FieldName);
end;

procedure plua_SetFieldValue(L: PLua_State; FieldName: string;
  AValue: boolean); overload;
begin
  lua_pushboolean(L, AValue);
  lua_setfield(L, -2, FieldName);
end;

procedure plua_SetFieldValue(L: PLua_State; FieldName: string;
  ATable: plual_reg); overload;
begin
  lua_newtable(L);
  lual_register(L, nil, ATable);
  lua_setfield(L, -2, FieldName);
end;

procedure plua_SetFieldValue(L: PLua_State; Name: string; Reader: lua_CFunction;
  Writer: lua_CFunction); overload;
var
  tidx, midx: integer;
begin
  lua_newtable(L);
  tidx := lua_gettop(L);

  lua_newtable(L);
  midx := lua_gettop(L);

  lua_pushstring(L, '__index');
  lua_pushcfunction(L, Reader);
  lua_rawset(L, midx);
  lua_pushstring(L, '__newindex');
  lua_pushcfunction(L, Writer);
  lua_rawset(L, midx);
  lua_setmetatable(L, tidx);
  lua_setfield(L, -2, Name);
end;

procedure plua_SetFieldValueV(L: PLua_State; FieldName: string;
  AValue: Variant);
begin
  plua_pushvariant(L, AValue);
  lua_setfield(L, -2, FieldName);
end;

function plua_GetFieldValueStr(L: PLua_State; idx: integer; FieldName: string;
  ADefaultValue: string = ''): string;
begin
  lua_pushstring(L, FieldName);
  lua_gettable(L, idx);
  if lua_isnil(L, -1) then
    Result := ADefaultValue
  else
    Result := lua_tostring(L, -1);
end;

function plua_GetFieldValueInt(L: PLua_State; idx: integer; FieldName: string;
  ADefaultValue: integer): integer;
begin
  lua_pushstring(L, FieldName);
  lua_gettable(L, idx);
  if lua_isnil(L, -1) then
    Result := ADefaultValue
  else
    Result := lua_tointeger(L, -1);
end;

function plua_GetFieldValueBool(L: PLua_State; idx: integer; FieldName: string;
  ADefaultValue: boolean): boolean;
begin
  lua_pushstring(L, FieldName);
  lua_gettable(L, idx);
  if lua_isnil(L, -1) then
    Result := ADefaultValue
  else
    Result := lua_toboolean(L, -1);
end;

function plua_GetFieldValueType(L: PLua_State; idx: integer; FieldName: string)
  : integer;
begin
  lua_pushstring(L, FieldName);
  lua_gettable(L, idx);
  result := lua_type(L, -1);
end;

function plua_GetFieldValueVariant(L: PLua_State; idx: integer;
  FieldName: string; ADefaultValue: Variant): Variant;
begin
  lua_pushstring(L, FieldName);
  lua_gettable(L, idx);
  if lua_isnil(L, -1) then
    Result := ADefaultValue
  else
    Result := plua_tovariant(L, -1);
end;

{ TLuaTable }

function TLuaTable.ReadString(FieldName: string;
  ADefaultValue: string = ''): string;
begin
  Result := plua_GetFieldValueStr(fLuaState, fTableIndex, FieldName,
    ADefaultValue);
end;

function TLuaTable.ReadInteger(FieldName: string;
  ADefaultValue: integer): integer;
begin
  Result := plua_GetFieldValueInt(fLuaState, fTableIndex, FieldName,
    ADefaultValue);
end;

function TLuaTable.ReadBool(FieldName: string; ADefaultValue: boolean): boolean;
begin
  Result := plua_GetFieldValueBool(fLuaState, fTableIndex, FieldName,
    ADefaultValue);
end;

function TLuaTable.ReadVariant(FieldName: string;
  ADefaultValue: Variant): Variant;
begin
  Result := plua_GetFieldValueVariant(fLuaState, fTableIndex, FieldName,
    ADefaultValue);
end;

procedure TLuaTable.GetIndexFromTop;
begin
  fTableIndex := lua_gettop(fLuaState);
end;

constructor TLuaTable.Create(L: PLua_State; GetIdxFromTop: boolean = false);
begin
  fLuaState := L;
  if GetIdxFromTop then
    GetIndexFromTop;
end;

destructor TLuaTable.Destroy;
begin
  inherited;
end;

end.
