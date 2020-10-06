unit pLuaMin;

{
  Minimal version of pLua.pas with most used functions only

  Copyright (c) 2007 Jeremy Darling
  Modifications copyright (c) 2010-2018 Felipe Daragon

  License: MIT (http://opensource.org/licenses/mit-license.php)
  Same as the original code by Jeremy Darling.

}

interface

{$I Lua.inc}

uses
  Lua;


procedure plua_dostring(L: PLua_State; AString: String);
procedure plua_pushansistring(L: PLua_State; AString : AnsiString);
procedure plua_RegisterLuaTable(L: PLua_State; Name: string;
  Reader: lua_CFunction = nil; Writer: lua_CFunction = nil;
  TableIndex: Integer = LUA_GLOBALSINDEX);
  
implementation

procedure plua_dostring(L: PLua_State; AString: String);
begin
  luaL_loadbuffer(L, PAnsiChar(ansistring(AString)), Length(ansistring(AString)
    ), PAnsiChar(ansistring('')));
  lua_pcall(L, 0, 0, 0);
end;

procedure plua_pushansistring(L: PLua_State; AString: AnsiString);
begin
  lua_pushstring(l, pansichar(AString));
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

end.

