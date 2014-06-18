unit pLua;

{
  Copyright (c) 2007 Jeremy Darling
  Modifications copyright (c) 2010-2014 Felipe Daragon

  License: MIT (http://opensource.org/licenses/mit-license.php)
  Same as the original code by Jeremy Darling.

  Changes:
  * 18.06.2014, FD - Added several functions for getting/setting the
  value of Lua table fields and local/global variables
  * 17.06.2014, FD - Added plua_dostring
  * 19.05.2014, FD - Added backwards compatibility with non-unicode Delphi.
  * 06.05.2013, FD - Added support for Delphi XE2 or higher.
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, lua;

Type // FD, 16/05/2010
  PtrInt = Integer;
  PtrUint = Cardinal;

type
  TVariantArray = array of Variant;
  PVariantArray = ^TVariantArray;

  LuaException = class(Exception)
  end;

function plua_tostring(L: PLua_State; Index: Integer): ansistring;
procedure plua_pushstring(L: PLua_State; AString: ansistring);

procedure plua_RegisterLuaTable(L: PLua_State; Name: ansistring;
  Reader: lua_CFunction = nil; Writer: lua_CFunction = nil;
  TableIndex: Integer = LUA_GLOBALSINDEX);

function plua_functionexists(L: PLua_State; FunctionName: ansistring;
  TableIndex: Integer = LUA_GLOBALSINDEX): boolean;

function plua_callfunction(L: PLua_State; FunctionName: ansistring;
  const args: Array of Variant; results: PVariantArray = nil;
  TableIndex: Integer = LUA_GLOBALSINDEX): Integer;

procedure plua_pushvariant(L: PLua_State; v: Variant);

function plua_TableToVariantArray(L: PLua_State; Index: Integer;
  Keys: TStrings = nil): Variant;
function plua_tovariant(L: PLua_State; Index: Integer): Variant;

function plua_absindex(L: PLua_State; Index: Integer): Integer;

procedure plua_spliterrormessage(const ErrMsg: string; out Title: ansistring;
  out Line: Integer; out Msg: ansistring);

procedure plua_CopyTable(L: PLua_State; IdxFrom, IdxTo: Integer);

procedure plua_RegisterMethod(L: PLua_State; aMethodName: ansistring;
  MethodPtr: lua_CFunction; totable: Integer = LUA_GLOBALSINDEX);

procedure plua_GetTableKey(L: PLua_State; TableIndex: Integer;
  KeyName: ansistring);

{ FD Additions }

procedure plua_dostring(L: PLua_State; AString: String);

function plua_AnyToString(L: PLua_State; idx: Integer): string;

// Gets or sets the value of local and global Lua variables
function plua_GetLuaVar(L: PLua_State; idx: Integer): Variant;
function plua_GetGlobal(L: PLua_State; valName: ansistring): Variant;
function plua_GetLocal(L: PLua_State; valName: ansistring): Variant;
procedure plua_SetGlobal(L: PLua_State; valName: ansistring;
  const AValue: Variant);
procedure plua_SetLocal(L: PLua_State; valName: ansistring;
  const AValue: Variant);

// Gets the value of a table field
function plua_GetFieldStr(L: PLua_State; idx: Integer; FieldName: PAnsiChar;
  ADefaultValue: string = ''): string;
function plua_GetFieldInt(L: PLua_State; idx: Integer; FieldName: PAnsiChar;
  ADefaultValue: Integer): Integer;
function plua_GetFieldBool(L: PLua_State; idx: Integer; FieldName: PAnsiChar;
  ADefaultValue: boolean): boolean;
function plua_GetFieldVariant(L: PLua_State; idx: Integer; FieldName: PAnsiChar;
  ADefaultValue: Variant): Variant;

// Sets the value of a table field
procedure plua_SetFieldValue(L: PLua_State; FieldName: PAnsiChar; AValue: string); overload;
procedure plua_SetFieldValue(L: PLua_State; FieldName: PAnsiChar;
  AValue: Integer); overload;
procedure plua_SetFieldValue(L: PLua_State; FieldName: PAnsiChar;
  AValue: boolean); overload;
procedure plua_SetFieldValueVariant(L: PLua_State; FieldName: PAnsiChar;
  AValue: Variant);

implementation

uses
  Variants;

function plua_tostring(L: PLua_State; Index: Integer): ansistring;
var
  Size: Integer;
begin
  Size := lua_strlen(L, Index);
  SetLength(Result, Size);
  if (Size > 0) then
    Move(lua_tostringP(L, Index)^, Result[1], Size);
end;

procedure plua_pushstring(L: PLua_State; AString: ansistring);
begin
  lua_pushstring(L, PAnsiChar(AString));
end;

procedure plua_RegisterLuaTable(L: PLua_State; Name: ansistring;
  Reader: lua_CFunction; Writer: lua_CFunction; TableIndex: Integer);
var
  tidx, midx: Integer;
begin
  lua_gettable(L, TableIndex);
  if (lua_type(L, -1) <> LUA_TTABLE) then
  begin
    lua_pushliteral(L, PAnsiChar(Name));
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
    lua_settable(L, TableIndex);
  end;
end;

function plua_functionexists(L: PLua_State; FunctionName: ansistring;
  TableIndex: Integer): boolean;
begin
  plua_pushstring(L, FunctionName);
  lua_rawget(L, TableIndex);
  Result := lua_isfunction(L, lua_gettop(L));
  if Result then
  begin
    Result := not lua_iscfunction(L, lua_gettop(L));
    lua_pop(L, 1);
  end;
end;

function plua_callfunction(L: PLua_State; FunctionName: ansistring;
  const args: Array of Variant; results: PVariantArray = nil;
  TableIndex: Integer = LUA_GLOBALSINDEX): Integer;
var
  NArgs, offset, i: Integer;
  Msg: ansistring;
begin
  offset := lua_gettop(L);
  plua_pushstring(L, FunctionName);
  lua_rawget(L, TableIndex);
  NArgs := High(args);
  for i := 0 to NArgs do
    plua_pushvariant(L, args[i]);
  if lua_pcall(L, NArgs + 1, LUA_MULTRET, 0) <> 0 then
  begin
    Msg := plua_tostring(L, -1);
    lua_pop(L, 1);
    raise LuaException.create(string(Msg));
  end;
  Result := lua_gettop(L) - offset;
  if (results <> Nil) then
  begin
    SetLength(results^, Result);
    for i := 0 to Result - 1 do
      results^[Result - i - 1] := plua_tovariant(L, -(i + 1));
  end;
end;

procedure plua_pushvariant(L: PLua_State; v: Variant);
var
  h, c: Integer;
begin
  case VarType(v) of
    varEmpty, varNull:
      lua_pushnil(L);
    varBoolean:
      lua_pushboolean(L, v);
    varStrArg, varOleStr, varString{$IFDEF UNICODE}, varUString{$ENDIF} :
      plua_pushstring(L, ansistring(v)); // FD: 06/05/2013, added unicode
    varDate:
      plua_pushstring(L, ansistring(DateTimeToStr(VarToDateTime(v))));
    varArray:
      begin
        h := VarArrayHighBound(v, 1);
        lua_newtable(L);
        for c := 0 to h do
        begin
          lua_pushinteger(L, c + 1);
          plua_pushvariant(L, v[c]);
          lua_settable(L, -3);
        end;
      end;
  else
    lua_pushnumber(L, Double(VarAsType(v, varDouble)));
  end;
end;

function plua_TableToVariantArray(L: PLua_State; Index: Integer;
  Keys: TStrings = nil): Variant;
var
  cnt: Integer;
  va: array of Variant;
begin
  Index := plua_absindex(L, Index);
  if Assigned(Keys) then
    Keys.Clear;

  lua_pushnil(L);
  cnt := 0;
  while (lua_next(L, Index) <> 0) do
  begin
    SetLength(va, cnt + 1);
    if Assigned(Keys) then
      Keys.Add(string(plua_tostring(L, -2)));
    va[cnt] := plua_tovariant(L, -1);
    lua_pop(L, 1);
    inc(cnt);
  end;

  if cnt > 0 then
  begin
    Result := VarArrayCreate([0, cnt - 1], varvariant);
    while cnt > 0 do
    begin
      dec(cnt);
      Result[cnt] := va[cnt];
    end;
  end
  else
    Result := VarArrayCreate([0, 0], varvariant);
end;

function plua_tovariant(L: PLua_State; Index: Integer): Variant;
Var
  dataType: Integer;
  dataNum: Double;
begin
  dataType := lua_type(L, Index);
  case dataType of
    LUA_TSTRING:
      Result := VarAsType(plua_tostring(L, Index), varString);
    LUA_TUSERDATA, LUA_TLIGHTUSERDATA:
      Result := VarAsType(PtrInt(lua_touserdata(L, Index)), varInteger);
    LUA_TNONE, LUA_TNIL:
      Result := varNull;
    LUA_TBOOLEAN:
      Result := VarAsType(lua_toboolean(L, Index), varBoolean);
    LUA_TNUMBER:
      begin
        dataNum := lua_tonumber(L, Index);
        if (Abs(dataNum) > MAXINT) then
          Result := VarAsType(dataNum, varDouble)
        else
        begin
          if (Frac(dataNum) <> 0) then
            Result := VarAsType(dataNum, varDouble)
          else
            Result := Round(VarAsType(dataNum, varDouble));
        end;
      end;
    LUA_TTABLE:
      Result := plua_TableToVariantArray(L, Index);
  else
    Result := NULL;
  end;
end;

function plua_absindex(L: PLua_State; Index: Integer): Integer;
begin
  if (index > -1) or ((index = LUA_GLOBALSINDEX) or (index = LUA_REGISTRYINDEX))
  then
    Result := index
  else
    Result := index + lua_gettop(L) + 1
end;

procedure plua_spliterrormessage(const ErrMsg: string; out Title: ansistring;
  out Line: Integer; out Msg: ansistring);
const
  Term = #$00;
  function S(Index: Integer): Char;
  begin
    if (Index <= Length(ErrMsg)) then
      Result := ErrMsg[Index]
    else
      Result := Term;
  end;
  function IsDigit(c: Char): boolean;
  begin
    Result := ('0' <= c) and (c <= '9');
  end;
  function PP(var Index: Integer): Integer;
  begin
    inc(Index);
    Result := Index;
  end;

var
  i, Start, Stop: Integer;
  LS: string;
  Find: boolean;
begin
  Title := '';
  Line := 0;
  Msg := ansistring(ErrMsg);
  Find := False;
  i := 1 - 1;
  Stop := 0;
  repeat
    while (S(PP(i)) <> ':') do
      if (S(i) = Term) then
        Exit;
    Start := i;
    if (not IsDigit(S(PP(i)))) then
      Continue;
    while (IsDigit(S(PP(i)))) do
      if (S(i - 1) = Term) then
        Exit;
    Stop := i;
    if (S(i) = ':') then
      Find := True;
  until (Find);
  Title := ansistring(Copy(ErrMsg, 1, Start - 1));
  LS := Copy(ErrMsg, Start + 1, Stop - Start - 1);
  Line := StrToIntDef(LS, 0);
  Msg := ansistring(Copy(ErrMsg, Stop + 1, Length(ErrMsg)));
end;

procedure plua_CopyTable(L: PLua_State; IdxFrom, IdxTo: Integer);
var
  id: Integer;
  key: ansistring;
begin
  lua_pushnil(L);
  while (lua_next(L, IdxFrom) <> 0) do
  begin
    key := plua_tostring(L, -2);
    case lua_type(L, -1) of
      LUA_TTABLE:
        begin
          id := lua_gettop(L);
          plua_CopyTable(L, id, IdxTo);
        end;
    else
      lua_pushliteral(L, PAnsiChar(key));
      lua_pushvalue(L, -2);
      lua_rawset(L, IdxTo);
    end;
    lua_pop(L, 1);
  end;
end;

procedure plua_RegisterMethod(L: PLua_State; aMethodName: ansistring;
  MethodPtr: lua_CFunction; totable: Integer);
begin
  lua_pushliteral(L, PAnsiChar(aMethodName));
  lua_pushcfunction(L, MethodPtr);
  lua_settable(L, totable);
end;

procedure plua_GetTableKey(L: PLua_State; TableIndex: Integer;
  KeyName: ansistring);
begin
  TableIndex := plua_absindex(L, TableIndex);
  plua_pushstring(L, KeyName);
  lua_gettable(L, TableIndex);
end;

{ FD Additions }

procedure plua_dostring(L: PLua_State; AString: String);
begin
  luaL_loadbuffer(L, PAnsiChar(ansistring(AString)), Length(ansistring(AString)
    ), PAnsiChar(ansistring(emptystr)));
  lua_pcall(L, 0, 0, 0);
end;

function plua_GetFieldStr(L: PLua_State; idx: Integer; FieldName: PAnsiChar;
  ADefaultValue: string = ''): string;
begin
  lua_pushstring(L, FieldName);
  lua_gettable(L, idx);
  if lua_isnil(L, -1) then
    Result := ADefaultValue
  else
    Result := lua_tostring(L, -1);
end;

function plua_GetFieldInt(L: PLua_State; idx: Integer; FieldName: PAnsiChar;
  ADefaultValue: Integer): Integer;
begin
  lua_pushstring(L, FieldName);
  lua_gettable(L, idx);
  if lua_isnil(L, -1) then
    Result := ADefaultValue
  else
    Result := lua_tointeger(L, -1);
end;

function plua_GetFieldBool(L: PLua_State; idx: Integer; FieldName: PAnsiChar;
  ADefaultValue: boolean): boolean;
begin
  lua_pushstring(L, FieldName);
  lua_gettable(L, idx);
  if lua_isnil(L, -1) then
    Result := ADefaultValue
  else
    Result := lua_toboolean(L, -1);
end;

function plua_GetFieldVariant(L: PLua_State; idx: Integer; FieldName: PAnsiChar;
  ADefaultValue: Variant): Variant;
begin
  lua_pushstring(L, FieldName);
  lua_gettable(L, idx);
  if lua_isnil(L, -1) then
    Result := ADefaultValue
  else
    Result := plua_tovariant(L, -1);
end;

function plua_AnyToString(L: PLua_State; idx: Integer): string;
var
  ltype: Integer;
begin
  Result := emptystr;
  ltype := lua_type(L, idx);
  case ltype of
    LUA_TSTRING:
      Result := string(PAnsiChar(ansistring(lua_tostring(L, idx))));
    LUA_TBOOLEAN:
      begin
        if lua_toboolean(L, idx) = True then
          Result := 'true'
        else
          Result := 'false';
      end;
    LUA_TNUMBER:
      begin
        if TVarData(plua_tovariant(L, idx)).vType = varDouble then
          Result := floattostr(lua_tonumber(L, idx))
        else
          Result := inttostr(lua_tointeger(L, idx));
      end;
  end;
end;

procedure plua_SetFieldValue(L: PLua_State; FieldName: PAnsiChar; AValue: string); overload;
begin
  lua_pushstring(L, AValue);
  lua_setfield(L, -2, FieldName);
end;

procedure plua_SetFieldValue(L: PLua_State; FieldName: PAnsiChar;
  AValue: Integer); overload;
begin
  lua_pushinteger(L, AValue);
  lua_setfield(L, -2, FieldName);
end;

procedure plua_SetFieldValue(L: PLua_State; FieldName: PAnsiChar;
  AValue: boolean); overload;
begin
  lua_pushboolean(L, AValue);
  lua_setfield(L, -2, FieldName);
end;

procedure plua_SetFieldValueVariant(L: PLua_State; FieldName: PAnsiChar;
  AValue: Variant);
begin
  plua_pushvariant(L, AValue);
  lua_setfield(L, -2, FieldName);
end;

function plua_GetLuaVar(L: PLua_State; idx: Integer): Variant;
var
  ltype: Integer;
  v: Variant;
  S: string;
begin
  ltype := lua_type(L, idx);
  case ltype of
    LUA_TSTRING:
      begin
        S := lua_tostring(L, idx);
        v := S;
      end;
    LUA_TBOOLEAN:
      v := lua_toboolean(L, idx);
    LUA_TNUMBER:
      v := lua_tointeger(L, idx);
  else
    v := plua_tovariant(L, idx);
  end;
  Result := v;
end;

function plua_GetGlobal(L: PLua_State; valName: ansistring): Variant; // working
var
  v: Variant;
begin
  Result := NULL;
  lua_pushstring(L, PAnsiChar(valName));
  lua_rawget(L, LUA_GLOBALSINDEX);
  try
    // writeln('getting '+valname);
    v := plua_GetLuaVar(L, -1); // plua_tovariant(L, -1);
    Result := v;
    // writeln('got '+valname+': '+result);
  finally
    lua_pop(L, 1);
  end;
end;

function plua_GetLocal(L: PLua_State; valName: ansistring): Variant;
var
  ar: plua_Debug; // use plua_debug, not lua_debug!
  VarName: PAnsiChar;
  VarValue: Variant;
  current: Integer;
  found: boolean;
  Name: string;
begin
  Result := NULL;
  found := False;
  Name := string(valName);

  lua_getglobal(L, 'tostring'); // this fixes ocasional crash with lua_getstack
  if lua_getstack(L, 1, @ar) <> 1 then
  begin
    Exit;
  end;
  current := 1;
  VarName := lua_getlocal(L, @ar, current);
  while VarName <> nil do
  begin
    // lua_pop(L,1);
    // writeln('Matching var:'+varname);
    if VarName = PAnsiChar(ansistring(Name)) then
    begin
      found := True;
      if found = True then
      begin // hides H2077 compiler warning
      end;
      // writeln('Found var:'+varname);
      try
        VarValue := plua_GetLuaVar(L, -1); // plua_tovariant(L, -1);
      finally
        lua_pop(L, 1);
      end;
      // writeln('found!'+varname+';'+varvalue);
      Result := VarValue;
      Exit;
    end;
    lua_pop(L, 1);
    VarName := lua_getlocal(L, @ar, current);
    inc(current);
  end;
  if found = False then
  begin // local not found, tries to get global with the same name
    // writeln('not found locally:'+valname);
    try
      VarValue := plua_GetGlobal(L, valName);
    except
    end;
    Result := VarValue;
    // writeln('global search for '+valname+' returned):'+result);
  end;
end;

procedure plua_SetLocal(L: PLua_State; valName: ansistring;
  const AValue: Variant);
var
  ar: plua_Debug;
  VarName: PAnsiChar;
  current: Integer;
  found: boolean;
  Name: string;
  NewValue: Variant;
begin
  found := False;
  Name := string(valName);
  NewValue := AValue;

  if lua_getstack(L, 1, @ar) <> 1 then
  begin
    Exit;
  end;
  current := 1;
  VarName := lua_getlocal(L, @ar, current);
  while VarName <> nil do
  begin
    if VarName = PAnsiChar(ansistring(Name)) then
    begin
      found := True;
      if found = True then
      begin // hides H2077 compiler warning
      end;
      // writeln('Found var:'+varname+' changing to:'+newvalue);
      // lua_pop(L,1);
      try
        plua_pushvariant(L, NewValue);
        lua_setlocal(L, @ar, current);
      finally
        lua_pop(L, 1);
      end;
      // writeln('Changed var:'+varname+' to:'+newvalue);
      Exit;
    end;
    lua_pop(L, 1);
    inc(current);
    VarName := lua_getlocal(L, @ar, current);

  end;
  if found = False then
  begin // new, local not found, tries to set global with the same name
    // writeln('not found locally:'+valname);
    plua_SetGlobal(L, valName, NewValue);
  end;
end;

procedure plua_SetGlobal(L: PLua_State; valName: ansistring;
  const AValue: Variant);
begin
  // writeln('setting glob:'+valname+'; new value: '+avalue);
  if VarIsType(AValue, varString) then
  begin
    lua_pushliteral(L, PAnsiChar(valName));
    lua_pushstring(L, PAnsiChar(ansistring(AValue)));
    lua_settable(L, LUA_GLOBALSINDEX);
  end
  else
  begin
    lua_pushliteral(L, PAnsiChar(valName));
    plua_pushvariant(L, AValue);
    lua_settable(L, LUA_GLOBALSINDEX);
  end;
end;

end.
