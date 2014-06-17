unit pLua;

{
  Copyright (c) 2007 Jeremy Darling
  Modifications copyright (c) 2010-2014 Felipe Daragon
  
  License: MIT (http://opensource.org/licenses/mit-license.php)
  Same as the original code by Jeremy Darling.
  
  Changes:
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
   PtrInt=Integer; 
   PtrUint=Cardinal;

type
  TVariantArray =array of Variant;
  PVariantArray =^TVariantArray;
  LuaException = class(Exception)
  end;

procedure plua_dostring(L: PLua_State; AString: String);

function  plua_tostring(L: PLua_State; Index: Integer): ansistring;
procedure plua_pushstring(L: PLua_State; AString : AnsiString);

procedure plua_RegisterLuaTable( l:PLua_State; Name : AnsiString;
                                 Reader : lua_CFunction = nil;
                                 Writer : lua_CFunction = nil;
                                 TableIndex : Integer = LUA_GLOBALSINDEX);

function plua_functionexists( L: PLua_State; FunctionName : AnsiString;
                              TableIndex : Integer = LUA_GLOBALSINDEX) : boolean;
                            
function plua_callfunction( L: PLua_State; FunctionName : AnsiString;
                            const args : Array of Variant;
                            results : PVariantArray = nil;
                            TableIndex : Integer = LUA_GLOBALSINDEX) : Integer;

procedure plua_pushvariant( L : PLua_State; v : Variant);

function  plua_TableToVariantArray( L: Plua_State; Index: Integer;
                                    Keys : TStrings = nil) : variant;
function plua_tovariant(L: Plua_State; Index: Integer): Variant;

function plua_absindex(L: Plua_State; Index: Integer): integer;

procedure plua_spliterrormessage(const ErrMsg: string; out Title: ansistring; out Line: Integer; out Msg: ansistring);

procedure plua_CopyTable(L: Plua_State; IdxFrom, IdxTo : Integer);

procedure plua_RegisterMethod( l : Plua_State; aMethodName : AnsiString;
                               MethodPtr : lua_CFunction;
                               totable : Integer = LUA_GLOBALSINDEX);

procedure plua_GetTableKey( l : PLua_State; TableIndex : Integer; KeyName : AnsiString );

implementation

uses
  Variants;
  
type
{$IFDEF UNICODE}
  lwPCha_r = PAnsiChar;
{$ELSE}
  lwPCha_r = PChar;
{$ENDIF}

procedure plua_dostring(L: PLua_State; AString: String);
begin
 luaL_loadbuffer(L, lwPCha_r(ansistring(AString)), Length(ansistring(AString)), lwPCha_r(emptystr));
 lua_pcall(L, 0, 0, 0);
end;

function plua_tostring(L: PLua_State; Index: Integer): ansistring;
var
  Size: Integer;
begin
  Size := lua_strlen(L, Index);
  SetLength(Result, Size);
  if (Size > 0) then
    Move(lua_tostringP(L, Index)^, Result[1], Size);
end;

procedure plua_pushstring(L: PLua_State; AString: AnsiString);
begin
  lua_pushstring(l, lwPCha_r(AString));
end;

procedure plua_RegisterLuaTable(l: PLua_State; Name: AnsiString;
  Reader: lua_CFunction; Writer: lua_CFunction; TableIndex: Integer);
var
  tidx, midx : Integer;
begin
  lua_gettable(l, TableIndex);
  if (lua_type(L, -1) <> LUA_TTABLE) then
    begin
      lua_pushliteral(L, lwPCha_r(Name));
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
      lua_setmetatable(l, tidx);
      lua_settable(l, TableIndex);
    end;
end;

function plua_functionexists(L: PLua_State; FunctionName: AnsiString;
  TableIndex: Integer): boolean;
begin
  plua_pushstring(L, FunctionName);
  lua_rawget(L, TableIndex);
  result := lua_isfunction(L, lua_gettop(L));
  if result then
    begin
      result := not lua_iscfunction(L, lua_gettop(L));
      lua_pop(L, 1);
    end;
end;

function plua_callfunction( L: PLua_State; FunctionName : AnsiString;
                            const args : Array of Variant;
                            results : PVariantArray = nil;
                            TableIndex : Integer = LUA_GLOBALSINDEX) : Integer;
var
   NArgs, offset,
   i :Integer;
   msg : AnsiString;
begin
  offset := lua_gettop(l);
  plua_pushstring(L, FunctionName);
  lua_rawget(L, TableIndex);
  NArgs := High(Args);
  for i:=0 to NArgs do
    plua_pushvariant(l, args[i]);
  if lua_pcall(l, NArgs+1, LUA_MULTRET, 0) <> 0 then
    begin
      msg := plua_tostring(l, -1);
      lua_pop(l, 1);
      raise LuaException.create(msg);
    end;
  result := lua_gettop(l) - offset;
  if (Results<>Nil) then
    begin
      SetLength(Results^, Result);
      for i:=0 to Result-1 do
        Results^[Result-i-1] := plua_tovariant(L, -(i+1));
    end;
end;

procedure plua_pushvariant(L: PLua_State; v: Variant);
var
  h, c : Integer;
begin
  case VarType(v) of
    varEmpty,
    varNull    : lua_pushnil(L);
    varBoolean : lua_pushboolean(L, v);
    varStrArg,
    varOleStr,
    varString{$IFDEF UNICODE},varUString{$ENDIF}  : plua_pushstring(L, v);  // FD: 06/05/2013, added unicode
    varDate    : plua_pushstring(L, DateTimeToStr(VarToDateTime(v)));
    varArray   : begin
                   h := VarArrayHighBound(v, 1);
                   lua_newtable(L);
                   for c := 0 to h do
                     begin
                       lua_pushinteger(L, c+1);
                       plua_pushvariant(L, v[c]);
                       lua_settable(L, -3);
                     end;
                 end;
  else
    lua_pushnumber(L, Double(VarAsType(v, varDouble)));
  end;
end;

function  plua_TableToVariantArray( L: Plua_State; Index: Integer;
                                    Keys : TStrings = nil) : variant;
var
  cnt : Integer;
  va : array of Variant;
begin
  Index := plua_absindex(L, Index);
  if Assigned(Keys) then
    Keys.Clear;

  lua_pushnil(L);
  cnt := 0;
  while (lua_next(L, Index) <> 0) do
    begin
      SetLength(va, cnt+1);
      if assigned(Keys) then
        Keys.Add(plua_tostring(L, -2));
      va[cnt] := plua_tovariant(l, -1);
      lua_pop(L, 1);
      inc(cnt);
    end;

  if cnt > 0 then
    begin
      result := VarArrayCreate([0,cnt-1], varvariant);
      while cnt > 0 do
        begin
          dec(cnt);
          result[cnt] := va[cnt];
        end;
    end
  else
    result := VarArrayCreate([0,0], varvariant);
end;

function plua_tovariant(L: Plua_State; Index: Integer): Variant;
Var
  dataType :Integer;
  dataNum  :Double;
begin
  dataType :=lua_type(L, Index);
  case dataType of
    LUA_TSTRING          : Result := VarAsType(plua_tostring(L, Index), varString);
    LUA_TUSERDATA,
    LUA_TLIGHTUSERDATA   : Result := VarAsType(PtrInt(lua_touserdata(L, Index)), varInteger);
    LUA_TNONE,
    LUA_TNIL             : Result := varNull;
    LUA_TBOOLEAN         : Result := VarAsType(lua_toboolean(L, Index), varBoolean);
    LUA_TNUMBER          : begin
                             dataNum :=lua_tonumber(L, Index);
                             if (Abs(dataNum)>MAXINT) then
                               Result :=VarAsType(dataNum, varDouble)
                             else
                               begin
                                 if (Frac(dataNum)<>0) then
                                   Result :=VarAsType(dataNum, varDouble)
                                 else
                                   Result :=Round(VarAsType(dataNum, varDouble));
                               end;
                           end;
    LUA_TTABLE           : result := plua_TableToVariantArray(L, Index);
  else
    result := NULL;
  end;
end;

function plua_absindex(L: Plua_State; Index: Integer): integer;
begin
  if (index > -1) or ((index = LUA_GLOBALSINDEX) or (index = LUA_REGISTRYINDEX)) then
    result := index
  else
    result := index + lua_gettop(L) + 1
end;

procedure plua_spliterrormessage(const ErrMsg: string; out Title: ansistring; out Line: Integer; out Msg: ansistring);
const
  Term = #$00;
  function S(Index: Integer): Char;
  begin
    if (Index <= Length(ErrMsg)) then
      Result := ErrMsg[Index]
    else
      Result := Term;
  end;
  function IsDigit(C: Char): Boolean;
  begin
    Result := ('0' <= C) and (C <= '9');
  end;
  function PP(var Index: Integer): Integer;
  begin
    Inc(Index);
    Result := Index;
  end;
var
  I, Start, Stop: Integer;
  LS: string;
  Find: Boolean;
begin
  Title := '';
  Line := 0;
  Msg := ErrMsg;
  Find := False;
  I := 1 - 1;
  Stop := 0;
  repeat
    while (S(PP(I)) <> ':') do
      if (S(I) = Term) then
        Exit;
    Start := I;
    if (not IsDigit(S(PP(I)))) then
      Continue;
    while (IsDigit(S(PP(I)))) do
      if (S(I - 1) = Term) then
        Exit;
    Stop := I;
    if (S(I) = ':') then
      Find := True;
  until (Find);
  Title := Copy(ErrMsg, 1, Start - 1);
  LS := Copy(ErrMsg, Start + 1, Stop - Start - 1);
  Line := StrToIntDef(LS, 0);
  Msg := Copy(ErrMsg, Stop + 1, Length(ErrMsg));
end;

procedure plua_CopyTable(L: Plua_State; IdxFrom, IdxTo: Integer);
var
  id:Integer;
  key : AnsiString;
begin
  lua_pushnil(L);
  while(lua_next(L, IdxFrom)<>0)do
    begin
      key := plua_tostring(L, -2);
      case lua_type(L, -1) of
        LUA_TTABLE    : begin
          id := lua_gettop(L);
          plua_CopyTable(L, id, IdxTo);
        end;
      else
        lua_pushliteral(l, lwPCha_r(key));
        lua_pushvalue(l, -2);
        lua_rawset(L, IdxTo);
      end;
      lua_pop(L, 1);
    end;
end;

procedure plua_RegisterMethod(l: Plua_State; aMethodName: AnsiString;
  MethodPtr: lua_CFunction; totable : Integer);
begin
  lua_pushliteral(l, lwPCha_r(aMethodName));
  lua_pushcfunction(l, MethodPtr);
  lua_settable(l, totable);
end;

procedure plua_GetTableKey(l: PLua_State; TableIndex: Integer;
  KeyName: AnsiString);
begin
  TableIndex := plua_absindex(l, TableIndex);
  plua_pushstring(l, KeyName);
  lua_gettable(l, TableIndex);
end;

end.

