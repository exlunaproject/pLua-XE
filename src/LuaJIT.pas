unit LuaJIT;
{
  Lua JIT Extensions
  Modifications copyright (c) 2009-2017 Denis Golovan

  License: same as Lua 5.1 (license at the end of this file).
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I Lua.inc}

uses 
  Lua;

{$IFDEF LUAJIT}
// internal LuaJIT types/structs
const
  LuaJIT_CTID_CTYPEID = 21;         // see CTTYDEF in lj_ctypes.h
  LuaJIT_CTID_INVALID = $ffffffff;

  LuaJIT_CTYPEDID_INT64 = 11;       // ctype for int64
type
  LuaJIT_CTypeID   = UInt32;
  PLuaJIT_CTypeID  = ^LuaJIT_CTypeID;

  LuaJIT_CTypeID1  = UInt16;
  PLuaJIT_CTypeID1 = ^LuaJIT_CTypeID1;

  {$IFDEF FPC}
    {$push}
    {$packrecords C} // TODO: Delphi support
  {$ENDIF}
  LuaJIT_GCRef = record  // see GCRef in lj_obj.h
    {$IFDEF LUAJIT_GC64}
    gcptr64: UInt64;     // True 64 bit pointer.
    {$ELSE}
    gcptr32: UInt32;     // Pseudo 32 bit pointer.
    {$ENDIF}
  end;

  LuaJIT_GCcdata  = record        // see GCcdata in lj_obj.h
    // record requires C packing!!!

    //   gcheader : LuaJIT_GCHeader;  // see GCHeader macro in lj_obj.h
    nextgc:LuaJIT_GCRef;
    marked:UInt8;
    gct:UInt8;
    //   gcheader : LuaJIT_GCHeader;

    ctypeid : LuaJIT_CTypeID1;
  end;
  PLuaJIT_GCcdata = ^LuaJIT_GCcdata;

  {$IFDEF LUAJIT_GC64}
    {$IF sizeof(LuaJIT_GCcdata) <> 16}
      {$Fatal LuaJIT_GCcdata size invalid}
    {$ENDIF}
  {$ELSE}
    {$IF sizeof(LuaJIT_GCcdata) <> 8}
      {$Fatal LuaJIT_GCcdata size invalid}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC}
    {$pop} // restore packrecords setting
  {$ENDIF}
{$ENDIF}

{$IFDEF LUAJIT}
// see LuaJIT_CTYPEDID_* constants for ctypeid standard types
function luajit_tocdata(L : Plua_State; idx : Integer; out ctypeid : LuaJIT_CTypeID) : Pointer;
function luajit_getctypeid(L : Plua_State; typename : PAnsiChar; out ctypeid : LuaJIT_CTypeID):boolean;
{$ENDIF}

{$IFDEF LUAJIT_DUMPX}
function lua_dumpx(L : Plua_State; idx : Integer; writer : lua_Writer; data: Pointer; strip: Integer) : Integer;
  cdecl; external LuaDLL;
{$ENDIF}

implementation

{$IFDEF LUAJIT}
function luajit_tocdata(L : Plua_State; idx : Integer; out ctypeid : LuaJIT_CTypeID) : Pointer;
var T:Integer;
    p : Pointer;
    cd : PLuaJIT_GCcdata;
begin
  Result := nil;
  ctypeid:= LuaJIT_CTID_INVALID;

  T:=lua_type(l, idx);
  if T <> LUA_TCDATA then
    Exit;

  p := lua_topointer(l, idx);
  if p = nil then
    Exit;

  cd := PLuaJIT_GCcdata( PByte(p) - sizeof(LuaJIT_GCcdata) );
  ctypeid:=cd^.ctypeid;

  Result := p;
end;

function luajit_getctypeid(L : Plua_State; typename : PAnsiChar; out ctypeid : LuaJIT_CTypeID):boolean;
var idx : Integer;
    p : Pointer;
    typeof_success : boolean;
begin
  Result := false;
  ctypeid := LuaJIT_CTID_INVALID;

  idx := lua_gettop(L);
  try
    // Get ref to ffi.typeof
    luaL_loadstring(L, 'return require("ffi").typeof');

    // lua_call must be wrapped by try .. except
    typeof_success := false;
    try
      lua_call(L, 0, 1);
      if not lua_isfunction(L, -1) then
        Exit;
      // Push the first argument to ffi.typeof
      lua_pushstring(L, typename);
      // Call ffi.typeof()
      lua_call(L, 1, 1);
      typeof_success := true;
    except
    end;
    if not typeof_success then
      Exit;

    // Returned type should be LUA_TCDATA with CTID_CTYPEID
    p:=luajit_tocdata(L, -1, ctypeid);
    if (p = nil) or (ctypeid <> LuaJIT_CTID_CTYPEID) then
      Exit;

    ctypeid := PLuaJIT_CTypeID(p)^;
    Result := True;
  finally
    // balance Lua stack
    lua_settop(L, idx);
  end;
end;
{$ENDIF}

end.