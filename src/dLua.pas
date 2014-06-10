unit dLua;

{
  Use this instead of Lua.pas to get ride of string cast warnings
  This is temporary (final solution will probably involve merging pLua
  and https://github.com/felipedaragon/LuaUtils)
  
  Copyright (c) 2014 Felipe Daragon

  License: same as Lua 5.1
}

interface

uses Lua;

type
 Plua_State = Lua.Plua_State;
 
const
  (*
  ** basic types
  *)
  LUA_TNONE          = -1;

  LUA_TNIL           = 0;
  LUA_TBOOLEAN       = 1;
  LUA_TLIGHTUSERDATA = 2;
  LUA_TNUMBER        = 3;
  LUA_TSTRING        = 4;
  LUA_TTABLE         = 5;
  LUA_TFUNCTION      = 6;
  LUA_TUSERDATA	     = 7;
  LUA_TTHREAD        = 8;

  (* minimum Lua stack available to a C function *)
  LUA_MINSTACK = 20;

function lua_isnone(L : Plua_State; n : Integer) : Boolean;
function lua_type(L : Plua_State; idx : Integer) : Integer;
procedure lua_newtable(L : Plua_State);
procedure lua_pushboolean(L : Plua_State; b : Boolean);
procedure lua_pushinteger(L : Plua_State; n : lua_Integer);
procedure lua_pushstring(L: PLua_State; AString : string);
function lua_toboolean(L : Plua_State; idx : Integer) : Boolean;
function lua_tointeger(L : Plua_State; idx : Integer) : lua_Integer;
function lua_tostring(L : Plua_State; idx : Integer) : string;
function lua_tonumber(L : Plua_State; idx : Integer) : lua_Number;
procedure lua_register(L : Plua_State; n : string; f : lua_CFunction);

implementation

function lua_type(L : Plua_State; idx : Integer) : Integer;
begin
 result:=Lua.lua_type(L, idx);
end;

function lua_isnone(L : Plua_State; n : Integer) : Boolean;
begin
 result:=Lua.lua_isnone(L, n);
end;

procedure lua_newtable(L : Plua_State);
begin
 Lua.lua_newtable(L);
end;

function lua_tointeger(L : Plua_State; idx : Integer) : lua_Integer;
begin
 result := Lua.lua_tointeger(L, idx);
end;

function lua_tonumber(L : Plua_State; idx : Integer) : lua_Number;
begin
 result:= Lua.lua_tonumber(L,idx);
end;

procedure lua_pushinteger(L : Plua_State; n : lua_Integer);
begin
 Lua.lua_pushinteger(L, n);
end;

procedure lua_pushboolean(L : Plua_State; b : Boolean);
begin
 Lua.lua_pushboolean(L, b);
end;

procedure lua_pushstring(L: PLua_State; AString: String);
begin
 Lua.lua_pushstring(l, lwPCha_r(ansistring(string(AString))));
end;

function lua_toboolean(L : Plua_State; idx : Integer) : Boolean;
begin
 result:=Lua.lua_toboolean(L, idx);
end;

function lua_tostring(L : Plua_State; idx : Integer) : string;
begin
 result:=string(Lua.lua_tostring(L, idx));
end;

procedure lua_register(L : Plua_State; n : string; f : lua_CFunction);
begin
 Lua.lua_register(L, lwPCha_r(ansistring(string(n))), f);
end;

end.