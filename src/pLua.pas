unit pLua;

{
  Copyright (c) 2007 Jeremy Darling
  Modifications copyright (c) 2010-2020 Felipe Daragon

  License: MIT (http://opensource.org/licenses/mit-license.php)
  Same as the original code by Jeremy Darling.

  Changes:

  * 21.09.2020, FD - Added strict type validation functions
  * 20.09.2020, FD - Added plua_validateargsets and plua_validateargscount,
    and improved validation functions.
  * 18.09.2020, FD - Added plua_validateargs and plua_validatetype functions.
  * 17.09.2020, FD - plua_functionexists now checks C function.
  Older function renamed to plua_functionexists_noc.
                   - Added plua_pushintnumber
  * 16.09.2020, FD - Fixed occasional crash with plua_SetLocal.
  * 30.11.2015, FD - Fixed occasional crash with plua_functionexists.
  * 26.06.2014, FD - Changed to work with string instead of ansistring.
  * 18.06.2014, FD - Added several functions for getting/setting the
  value of local/global Lua variables
  * 17.06.2014, FD - Added plua_dostring
  * 19.05.2014, FD - Added backwards compatibility with non-unicode Delphi.
  * 06.05.2013, FD - Added support for Delphi XE2 or higher.
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, Variants, Lua;

const
  cLuaGlobalVariableStr = '[LUA_GLOBALSINDEX]';
var
  DefaultMaxTable, SubTableCount: Integer;

type
  PtrInt = Integer;
  PtrUint = Cardinal;
  TVariantArray = array of Variant;
  PVariantArray = ^TVariantArray;

type
  LuaException = class(Exception)
  end;

procedure plua_RegisterLuaTable(L: PLua_State; Name: string;
  Reader: lua_CFunction = nil; Writer: lua_CFunction = nil;
  TableIndex: Integer = LUA_GLOBALSINDEX);

function plua_functionexists(L: PLua_State; FunctionName: string;
  TableIndex: Integer = LUA_GLOBALSINDEX): boolean;
function plua_functionexists_noc(L: PLua_State; FunctionName: string;
  TableIndex: Integer): boolean;

function plua_callfunction(L: PLua_State; FunctionName: string;
  const args: Array of Variant; results: PVariantArray = nil;
  TableIndex: Integer = LUA_GLOBALSINDEX): Integer;

procedure plua_pushintnumber(L: PLua_State; N: Integer);

procedure plua_pushvariant(L: PLua_State; v: Variant);

function plua_TableToVariantArray(L: PLua_State; Index: Integer;
  Keys: TStrings = nil): Variant;
function plua_tovariant(L: PLua_State; Index: Integer): Variant;

function plua_absindex(L: PLua_State; Index: Integer): Integer;

procedure plua_spliterrormessage(const ErrMsg: string; out Title: string;
  out Line: Integer; out Msg: string);

procedure plua_CopyTable(L: PLua_State; IdxFrom, IdxTo: Integer);

procedure plua_RegisterMethod(L: PLua_State; aMethodName: string;
  MethodPtr: lua_CFunction; totable: Integer = LUA_GLOBALSINDEX);

procedure plua_GetTableKey(L: PLua_State; TableIndex: Integer; KeyName: string);

procedure plua_pushansistring(L: PLua_State; AString: ansistring);
function plua_toansistring(L: PLua_State; Index: Integer): ansistring;

{ FD: Additions by Felipe Daragon }

type
  TLuaTypeRange = 1..9; // LUA_TSTRING, etc.
  TLuaTypeSet = set of 1..9; // LUA_TSTRING, etc.

type
  TLuaValidationResult = record
    OK:boolean;
    ErrorMessage:string;
    ArgsCount:integer;
  end;

procedure plua_dostring(L: PLua_State; AString: String);
function plua_AnyToString(L: PLua_State; idx: Integer): string;
function plua_typetokeyword(const LuaType: integer): string;
function plua_typesettokeyword(const ts: TLuaTypeSet): string;
function plua_keywordtotype(const keyword: string): integer;

// Gets or sets the value of local and global Lua variables
function plua_GetLuaVar(L: PLua_State; idx: Integer): Variant;
function plua_GetGlobal(L: PLua_State; varName: string): Variant;
function plua_GetLocal(L: PLua_State; varName: string): Variant;
procedure plua_SetGlobal(L: PLua_State; varName: string; const AValue: Variant);
procedure plua_SetLocal(L: PLua_State; varName: string; const AValue: Variant);

// LuaStackToStr method from old LuaUtils project
function plua_LuaStackToStr(L: Plua_State; Index: Integer; MaxTable: Integer;
  SubTableMax: Integer): string;
function plua_dequote(const QuotedStr: string): string;

{ Lua argument validation functions
  Proudly present these functions that simplify the process of validating
  arguments passed to a C function so it can be done with a SINGLE line of code
}
function plua_matchtypeset(L: plua_State;const idx:integer;const ts:TLuaTypeSet;
  const stricttype:boolean=false):boolean;
function plua_validateargs(L: plua_State; var luaresult:integer;
  const p:array of TLuaTypeRange;const optional:integer=0;
  const stricttype:boolean=false):TLuaValidationResult;
function plua_validateargs_strict(L: plua_State; var luaresult:integer;
  const p:array of TLuaTypeRange;const optional:integer=0):TLuaValidationResult;
function plua_validateargsets(L: plua_State; var luaresult:integer;
  const p:array of TLuaTypeSet;const optional:integer=0;
  const stricttype:boolean=false):TLuaValidationResult;
function plua_validateargsets_strict(L: plua_State; var luaresult:integer;
  const p:array of TLuaTypeSet;const optional:integer=0):TLuaValidationResult;
function plua_validateargscount(L: plua_State; var luaresult:integer;
  const max_args:integer; const optional:integer=0):TLuaValidationResult;
function plua_validatetype(L: plua_State; const idx, expectedluatype:integer;
  const stricttype:boolean=false):boolean;


implementation

{
  This function makes very easy to validate arguments passed to a C function
  with just a single line

  Validation example:
  function str_after(L: plua_State): integer; cdecl;
  begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then
    lua_pushstring(L, after(lua_tostring(L, 1), lua_tostring(L, 2)));
  end;

  If you have optional arguments, remember to pass the number of optional
  arguments as the fourth parameter

  If you need strict type validation, for better code readability, use the
  plua_validateargs_strict() function instead of calling this function
  with its last parameter set to true.
}
function plua_validateargs(L: plua_State; var luaresult:integer;
  const p:array of TLuaTypeRange;const optional:integer=0;
  const stricttype:boolean=false):TLuaValidationResult;
var
 i, idx:integer;
begin
  result := plua_validateargscount(L, luaresult, (high(p) +1), optional);
  // Use ArgsCount instead of high(p) because we only want to validate provided arguments
  for i := low(p) to result.ArgsCount-1 do
  begin
    idx := i+1;
    if plua_validatetype(L, idx, p[i], stricttype) = false then begin
      result.OK := false;
      result.ErrorMessage := 'argument #'+IntToStr(idx)+' must be '+plua_typetokeyword(p[i]);
      luaL_typerror(L, idx, PAnsiChar(AnsiString(plua_typetokeyword(p[i]))));
    end;
  end;
end;

// Same as above but with strict Lua type validation
// This means for example that a number passed to C function that expects a string
// will not be allowed to be converted to string
function plua_validateargs_strict(L: plua_State; var luaresult:integer;
  const p:array of TLuaTypeRange;const optional:integer=0):TLuaValidationResult;
begin
  result := plua_validateargs(L, luaresult, p, optional, true);
end;

// Same as above but allowing arguments to have different types
// Usage example:
// function somefunction(L: plua_State): integer; cdecl;
// const firstarg = [LUA_TSTRING, LUA_TLUATABLE];
// begin
//   if plua_validateargsets(L, result, [firstarg, [LUA_TSTRING]]).OK then begin
//     ...
//   end;
// end;
function plua_validateargsets(L: plua_State; var luaresult:integer;
  const p:array of TLuaTypeSet;const optional:integer=0;
  const stricttype:boolean=false):TLuaValidationResult;
var
  i, idx:integer;
begin
  result := plua_validateargscount(L, luaresult, (high(p) +1), optional);
  // Use ArgsCount instead of high(p) because we only want to validated provided arguments
  for i := low(p) to result.ArgsCount-1 do
  begin
    idx := i+1;
    if plua_matchtypeset(L, idx, p[i], stricttype) = false then begin
      result.OK := false;
      result.ErrorMessage := 'argument #'+IntToStr(idx)+' must be '+plua_typesettokeyword(p[i]);
      luaL_typerror(L, idx, PAnsiChar(AnsiString(plua_typesettokeyword(p[i]))));
    end;
  end;
end;

// Same as above but with strict Lua type validation
function plua_validateargsets_strict(L: plua_State; var luaresult:integer;
  const p:array of TLuaTypeSet;const optional:integer=0):TLuaValidationResult;
begin
  result := plua_validateargsets(L, luaresult, p, optional, true);
end;

function plua_typesettokeyword(const ts: TLuaTypeSet): string;
  procedure add(const s:string);
  begin
    if result = emptystr then
    result := s else
    result := result + ' or ' + s;
  end;
begin
  result := emptystr;
  if LUA_TSTRING in ts then
      add('string');
  if LUA_TBOOLEAN in ts then
      add('boolean');
  if LUA_TNUMBER in ts then
      add('integer');
  if LUA_TTABLE in ts then
      add('table');
  if LUA_TFUNCTION in ts then
      add('function');
  if LUA_TTHREAD in ts then
      add('thread');
  if LUA_TLIGHTUSERDATA in ts then
      add('lightuserdata');
  if LUA_TUSERDATA in ts then
      add('userdata');
end;

function plua_validateargscount(L: plua_State; var luaresult:integer;
  const max_args:integer; const optional:integer=0):TLuaValidationResult;
var
  min_args, num_args:integer;
begin
  luaresult := 1;
  num_args := lua_gettop(L);
  result.ArgsCount := num_args;
  result.OK := true;
  min_args := max_args -optional;
  if num_args < min_args then begin
    result.OK := false;
    if optional > 0 then
    result.ErrorMessage := 'missing arguments, '+IntToStr(max_args)+' expected, '+IntToStr(optional)+' optional). ' else
    result.ErrorMessage := 'missing arguments, '+IntToStr(min_args)+' expected';
    luaL_error(L, PAnsiChar(AnsiString(result.ErrorMessage)));
  end;
  if num_args > max_args then begin
    result.OK := false;
    result.ErrorMessage := 'too many arguments, max '+IntToStr(max_args)+' allowed';
    luaL_error(L, PAnsiChar(AnsiString(result.ErrorMessage)));
  end;
end;

function plua_validatetype(L: plua_State; const idx, expectedluatype:integer;
  const stricttype:boolean=false):boolean;
 function IsInteger(const s: string): Boolean;
 var
   v, c: integer;
 begin
   Val(s, v, c);
   if v = 0 then
   begin // avoid compiler warning
   end;
   result := c = 0;
 end;
var curluatype: integer;
begin
  curluatype := lua_type(L, idx);
  result := curluatype = expectedluatype;
  if (stricttype = false) and (result = false) then begin
    // Expected a string, got number. Lua will convert number to string automatically
    // Example: string.upper(10) in Lua returns "10"
    if (expectedluatype = LUA_TSTRING) and (curluatype = LUA_TNUMBER) then
    result := true;
    // Expected a number, got string, check if string is number and if true, allow it
    // Example: math.sqrt("100") in Lua returns 100, but math.sqrt("A") will raise error
    if (expectedluatype = LUA_TNUMBER) and (curluatype = LUA_TSTRING) and (IsInteger(lua_tostring(L, idx))) then
    result := true;
  end;
end;

function plua_matchtypeset(L: plua_State;const idx:integer;const ts:TLuaTypeSet;
  const stricttype:boolean=false):boolean;
  procedure validate(const lt:integer);
  begin
    if plua_validatetype(L, idx, lt, stricttype) then
      result := true;
  end;
begin
  result := false;
  if LUA_TSTRING in ts then
    validate(LUA_TSTRING);
  if LUA_TBOOLEAN in ts then
    validate(LUA_TBOOLEAN);
  if LUA_TNUMBER in ts then
    validate(LUA_TNUMBER);
  if LUA_TTABLE in ts then
    validate(LUA_TTABLE);
  if LUA_TFUNCTION in ts then
    validate(LUA_TFUNCTION);
  if LUA_TTHREAD in ts then
    validate(LUA_TTHREAD);
  if LUA_TLIGHTUSERDATA in ts then
    validate(LUA_TLIGHTUSERDATA);
  if LUA_TUSERDATA in ts then
    validate(LUA_TUSERDATA);
end;

function plua_typetokeyword(const LuaType: integer): string;
begin
  result := emptystr;
  case LuaType of
    LUA_TNIL:
      result := 'nil';
    LUA_TSTRING:
      result := 'string';
    LUA_TBOOLEAN:
      result := 'boolean';
    LUA_TNUMBER:
      result := 'integer';
    LUA_TTABLE:
      result := 'table';
    LUA_TFUNCTION:
      result := 'function';
    LUA_TTHREAD:
      result := 'thread';
    LUA_TLIGHTUSERDATA:
      result := 'lightuserdata';
    LUA_TUSERDATA:
      result := 'userdata';
  end;
end;

function plua_keywordtotype(const keyword: string): integer;
begin
  result := LUA_TNONE;
  if keyword = 'none' then
    result := LUA_TNONE else
  if keyword = 'nil' then
    result := LUA_TNIL else
  if keyword = 'string' then
    result := LUA_TSTRING else
  if keyword = 'boolean' then
    result := LUA_TBOOLEAN else
  if keyword = 'integer' then
    result := LUA_TNUMBER else
  if keyword = 'table' then
    result := LUA_TTABLE else
  if keyword = 'function' then
    result := LUA_TFUNCTION else
  if keyword = 'thread' then
    result := LUA_TTHREAD else
  if keyword = 'lightuserdata' then
    result := LUA_TLIGHTUSERDATA else
  if keyword = 'userdata' then
    result := LUA_TUSERDATA;
end;

function plua_toansistring(L: PLua_State; Index: Integer): ansistring;
var
  Size: Integer;
begin
  Size := lua_strlen(L, Index);
  SetLength(Result, Size);
  if (Size > 0) then
    Move(lua_tostringP(L, Index)^, Result[1], Size);
end;

procedure plua_pushansistring(L: PLua_State; AString: ansistring);
begin
  lua_pushstring(L, PAnsiChar(AString));
end;

procedure plua_RegisterLuaTable(L: PLua_State; Name: string;
  Reader: lua_CFunction; Writer: lua_CFunction; TableIndex: Integer);
var
  tidx, midx: Integer;
begin
  lua_gettable(L, TableIndex);
  if (lua_type(L, -1) <> LUA_TTABLE) then
  begin
    lua_pushliteral(L, Name);
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

function plua_functionexists(L: PLua_State; FunctionName: string;
  TableIndex: Integer): boolean;
begin
  if TableIndex = LUA_GLOBALSINDEX then
    lua_getglobal(L, 'tostring'); // FD: fixes global function sometimes not being located
  lua_pushstring(L, FunctionName);
  lua_rawget(L, TableIndex);

  try
  Result := (not lua_isnil(L, lua_gettop(L))) and lua_isfunction(L, lua_gettop(L));
  finally
   lua_pop(L, 1); // FD: added lua_isnil check and lua_pop. Fixes occasional exception
  end;

end;

function plua_functionexists_noc(L: PLua_State; FunctionName: string;
  TableIndex: Integer): boolean;
begin
  if TableIndex = LUA_GLOBALSINDEX then
    lua_getglobal(L, 'tostring'); // FD: fixes global function sometimes not being located
  lua_pushstring(L, FunctionName);
  lua_rawget(L, TableIndex);
  try
  Result := (not lua_isnil(L, lua_gettop(L))) and lua_isfunction(L, lua_gettop(L));
  finally
    lua_pop(L, 1); // FD: added lua_isnil check and lua_pop. Fixes occasional exception
  end;

  if Result then
  begin
    try
    Result := not lua_iscfunction(L, lua_gettop(L));
    finally
      lua_pop(L, 1);
    end;
  end;
end;

function plua_callfunction(L: PLua_State; FunctionName: string;
  const args: Array of Variant; results: PVariantArray = nil;
  TableIndex: Integer = LUA_GLOBALSINDEX): Integer;
var
  NArgs, offset, i: Integer;
  Msg: string;
begin
  offset := lua_gettop(L);
  lua_pushstring(L, FunctionName);
  lua_rawget(L, TableIndex);
  NArgs := High(args);
  for i := 0 to NArgs do
    plua_pushvariant(L, args[i]);
  if lua_pcall(L, NArgs + 1, LUA_MULTRET, 0) <> 0 then
  begin
    Msg := lua_tostring(L, -1);
    lua_pop(L, 1);
    raise LuaException.create(Msg);
  end;
  Result := lua_gettop(L) - offset;
  if (results <> Nil) then
  begin
    SetLength(results^, Result);
    for i := 0 to Result - 1 do
      results^[Result - i - 1] := plua_tovariant(L, -(i + 1));
  end;
end;

// Just an alias for lua_pushnumber() - Use this if you experience conversion
// issues with negative integers and Lua 64-bit when using lua_pushinteger()
procedure plua_pushintnumber(L: PLua_State; N: Integer);
begin
  lua_pushnumber(L, N);
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
      lua_pushstring(L, string(v)); // FD: 06/05/2013, added unicode
    varDate:
      lua_pushstring(L, DateTimeToStr(VarToDateTime(v)));
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
      Keys.Add(lua_tostring(L, -2));
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
      Result := VarAsType(lua_tostring(L, Index), varString);
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

procedure plua_spliterrormessage(const ErrMsg: string; out Title: string;
  out Line: Integer; out Msg: string);
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
  Msg := ErrMsg;
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
  Title := Copy(ErrMsg, 1, Start - 1);
  LS := Copy(ErrMsg, Start + 1, Stop - Start - 1);
  Line := StrToIntDef(LS, 0);
  Msg := Copy(ErrMsg, Stop + 1, Length(ErrMsg));
end;

procedure plua_CopyTable(L: PLua_State; IdxFrom, IdxTo: Integer);
var
  id: Integer;
  key: string;
begin
  lua_pushnil(L);
  while (lua_next(L, IdxFrom) <> 0) do
  begin
    key := lua_tostring(L, -2);
    case lua_type(L, -1) of
      LUA_TTABLE:
        begin
          id := lua_gettop(L);
          plua_CopyTable(L, id, IdxTo);
        end;
    else
      lua_pushliteral(L, key);
      lua_pushvalue(L, -2);
      lua_rawset(L, IdxTo);
    end;
    lua_pop(L, 1);
  end;
end;

procedure plua_RegisterMethod(L: PLua_State; aMethodName: string;
  MethodPtr: lua_CFunction; totable: Integer);
begin
  lua_pushliteral(L, aMethodName);
  lua_pushcfunction(L, MethodPtr);
  lua_settable(L, totable);
end;

procedure plua_GetTableKey(L: PLua_State; TableIndex: Integer; KeyName: string);
begin
  TableIndex := plua_absindex(L, TableIndex);
  lua_pushstring(L, KeyName);
  lua_gettable(L, TableIndex);
end;

{ FD Additions }

procedure plua_dostring(L: PLua_State; AString: String);
begin
  luaL_loadbuffer(L, PAnsiChar(ansistring(AString)), Length(ansistring(AString)
    ), PAnsiChar(ansistring(emptystr)));
  lua_pcall(L, 0, 0, 0);
end;

// This is similar to lua_tostring but covers boolean and number Lua types
function plua_AnyToString(L: PLua_State; idx: Integer): string;
var
  ltype: Integer;
begin
  Result := emptystr;
  ltype := lua_type(L, idx);
  case ltype of
    LUA_TSTRING:
      Result := lua_tostring(L, idx);
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

// This is a simpler implementation of the plua_tovariant function
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

function plua_GetGlobal(L: PLua_State; varName: string): Variant;
var
  v: Variant;
begin
  Result := NULL;
  lua_pushstring(L, varName);
  lua_rawget(L, LUA_GLOBALSINDEX);
  try
    // writeln('getting '+varname);
    v := plua_GetLuaVar(L, -1); // plua_tovariant(L, -1);
    Result := v;
    // writeln('got '+varname+': '+result);
  finally
    lua_pop(L, 1);
  end;
end;

function plua_GetLocal(L: PLua_State; varName: string): Variant;
var
  ar: plua_Debug; // use plua_debug instead of lua_debug
  vn: PAnsiChar;
  value: Variant;
  i: Integer;
  found: boolean;
begin
  Result := NULL;
  found := False;

  lua_getglobal(L, 'tostring'); // this fixes occasional crash with lua_getstack
  if lua_getstack(L, 1, @ar) <> 1 then
    Exit;
  i := 1;
  // lua_pop(L, 1);
  vn := lua_getlocal(L, @ar, i);
  while vn <> nil do
  begin
    // lua_pop(L,1);
    //writeln('Matching against var:'+varName);
    if strpas(vn) = ansistring(varName) then
    begin
      found := True;
      if found = True then
      begin // hides H2077 compiler warning
      end;
      // writeln('Found var:'+vn);
      try
        value := plua_GetLuaVar(L, -1); // plua_tovariant(L, -1);
      finally
        lua_pop(L, 1);
      end;
      //writeln('found!'+vn+';'+value);
      Result := value;
      Exit;
    end;
    lua_pop(L, 1);
    vn := lua_getlocal(L, @ar, i);
    inc(i);
  end;
  if found = False then
  begin // local not found, tries to get global with the same name
    //writeln(varname+' not found locally:'+vn);
    try
      value := plua_GetGlobal(L, varName);
    except
    end;
    Result := value;
    //writeln('global search for '+vn+' returned):'+result);
  end;
end;

procedure plua_SetLocal(L: PLua_State; varName: string; const AValue: Variant);
var
  ar: plua_Debug;
  vn: PAnsiChar;
  i: Integer;
  found: boolean;
  NewValue: Variant;
begin
  found := False;
  NewValue := AValue;
  lua_getglobal(L, 'tostring'); // this fixes occasional crash with lua_getstack
  if lua_getstack(L, 1, @ar) <> 1 then
    Exit;
  i := 1;
  //lua_pop(L, 1);
  vn := lua_getlocal(L, @ar, i);

  while vn <> nil do
  begin
    if strpas(vn) = ansistring(varName) then
    begin
      found := True;
      if found = True then
      begin // hides H2077 compiler warning
      end;
      //writeln('Found var:'+varname+' changing to:'+newvalue);
      // lua_pop(L,1);
      try
        plua_pushvariant(L, NewValue);
        lua_setlocal(L, @ar, i);
      finally
        lua_pop(L, 1);
      end;
      //writeln('Changed var:'+varname+' to:'+newvalue);
      Exit;
    end;
    lua_pop(L, 1);
    inc(i);
    vn := lua_getlocal(L, @ar, i);

  end;
  if found = False then
  begin // new, local not found, tries to set global with the same name
    //writeln('not found locally:'+varname);
    plua_SetGlobal(L, varName, NewValue);
  end;
end;

procedure plua_SetGlobal(L: PLua_State; varName: string; const AValue: Variant);
begin
  // writeln('setting glob:'+varname+'; new value: '+avalue);
  if VarIsType(AValue, varString) then
  begin
    lua_pushliteral(L, varName);
    lua_pushstring(L, string(AValue));
    lua_settable(L, LUA_GLOBALSINDEX);
  end
  else
  begin
    lua_pushliteral(L, varName);
    plua_pushvariant(L, AValue);
    lua_settable(L, LUA_GLOBALSINDEX);
  end;
end;

// Convert the last item at 'Index' from the stack to a string
// nil    : nil
// Number : FloatToStr
// Boolean: True/False
// stirng : "..."
// Table  : { Key1=Value Key2=Value }
// LuaStackToStr from LuaUtils project
function plua_dequote(const QuotedStr: string): string;
begin
  Result := AnsiDequotedStr(QuotedStr, '"');
end;

function plua_LuaStackToStr(L: Plua_State; Index: Integer; MaxTable: Integer; SubTableMax: Integer): string;
var
  pGLobalsIndexPtr: Pointer;
  function TableToStr(Index: Integer): string;
  var
    Key, Value: string;
    Count: Integer;

  begin
    Result := '{ ';
    Count := 0;
    lua_pushnil(L);

    // Go through the current table
    while (lua_next(L, Index) <> 0) do
    begin
      Inc(Count);
      if (Count > MaxTable) then
      begin
        Result := Result + '... ';
        lua_pop(L, 2);
        Break;
      end;

      // Key to string
      if lua_type(L, -2) = LUA_TNUMBER then
        Key := '[' + plua_dequote(plua_LuaStackToStr(L, -2, MaxTable, SubTableMax)) + ']'
      else
        Key := plua_dequote(plua_LuaStackToStr(L, -2, MaxTable, SubTableMax));

      // Value to string...
      if ((Key = '_G') or (lua_topointer(L, -1) = pGLobalsIndexPtr)) then
        Value := cLuaGlobalVariableStr
      else
        Value := plua_LuaStackToStr(L, -1, MaxTable, SubTableMax);

      if (lua_type(L, -1) = LUA_TFUNCTION) then
        Result := Result + Format('%s()=%p ', [Key, lua_topointer(L, -1)])
      else
        Result := Result + Format('%s=%s ', [Key, Value]);

      // Pop current value from stack leaving current key on top of the stack for lua_next
      lua_pop(L, 1);
    end;

    Result := Result + '}';
  end;

begin
  if (MaxTable < 0) then
    MaxTable := DefaultMaxTable;

  pGLobalsIndexPtr := lua_topointer(L, LUA_GLOBALSINDEX); // Retrieve globals index poiner for later conditions
  lua_checkstack(L, SubTableMax * 3); // Ensure there is enough space on stack to work with according to user's setting
  Index := plua_absindex(L, Index);

  case (lua_type(L, Index)) of
  LUA_TNIL:
    Result := 'nil';
  LUA_TNUMBER:
    Result := Format('%g', [lua_tonumber(L, Index)]);
  LUA_TBOOLEAN:
    Result := BoolToStr(lua_toboolean(L, Index) <> false, True);
  LUA_TSTRING:
    Result := '"'+lua_tostring(L, Index)+'"';
  LUA_TTABLE:
  begin
    if SubTableCount < SubTableMax then
    begin
      SubTableCount := SubTableCount + 1;
      Result := TableToStr(Index);
      SubTableCount := SubTableCount - 1;
    end
    else
      Result := '[SUB_TABLE_MAX_LEVEL_HAS_BEEN_REACHED]';
  end;
  LUA_TFUNCTION:
    if (lua_iscfunction(L, Index) <> false) then
      Result := Format('CFUNC:%p', [Pointer(lua_tocfunction(L, Index))])
    else
      Result := Format('FUNC:%p', [lua_topointer(L, Index)]);
  LUA_TUSERDATA:
    Result := Format('USERDATA:%p', [lua_touserdata(L, Index)]);
  LUA_TTHREAD:
    Result := Format('THREAD:%p', [lua_tothread(L, Index)]);
  LUA_TLIGHTUSERDATA:
    Result := Format('LIGHTUSERDATA:%p', [lua_touserdata(L, Index)]);
  else
    Assert(False);
  end;
end;

initialization
  DefaultMaxTable := 256;

end.
