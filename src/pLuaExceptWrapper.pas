unit pLuaExceptWrapper;
// TODO: Unfinished 
// Exception handling code by MageSlayer, worked as an include file
// For better code maintainability, Need to have a way to work as unit and not 
// as include

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$I Lua.inc}

{$IFDEF LUAJIT}
  //Currently only x64 exception natively by LuaJit itself
  //other platform need try/except on Pascal->Lua boundary
  {$IFDEF CPU64}
   {$DEFINE LUAJIT_EXCEPTION_SUPPORT}
  {$ENDIF}
  {$IFDEF CPUX64}
   {$DEFINE LUAJIT_EXCEPTION_SUPPORT}
  {$ENDIF}
{$ENDIF}

interface

uses 
  Lua;
  
{$IFDEF LUAJIT_EXCEPTION_SUPPORT}  
function plua_call_except_wrapper(l : PLua_State) : integer;
{$ELSE}
function plua_call_except_wrapper(l : PLua_State; out exc_message:pchar; out exception_caught:boolean) : integer;
{$ENDIF}

implementation
// Warning!!!
// Expects pLuaExceptActual to point to the actual handler

{$IFDEF LUAJIT_EXCEPTION_SUPPORT}
function plua_call_except_wrapper(l : PLua_State) : integer;
begin
  Result:=pLuaExceptActual(l);
end;

function plua_call_method(l : PLua_State) : integer; extdecl;
begin
  try
    Result:=plua_call_except_wrapper(l);
  except
    Result:=0;
    plua_ClearStack(l); //remove both parameters and any non-complete return values.

    raise;
  end;
end;
{$ENDIF}

{$IFNDEF LUAJIT_EXCEPTION_SUPPORT}
function plua_call_except_wrapper(l : PLua_State; out exc_message:pchar; out exception_caught:boolean) : integer;
begin
  exc_message:=nil;
  exception_caught:=False;

  try
    Result:=pLuaExceptActual(l);
  except
    on E:Exception do
      begin
        exception_caught:=True;
        exc_message:=StrToPChar(E.Message);
      end;
  end;
end;

{$IMPLICITEXCEPTIONS OFF}
function plua_call_method(l : PLua_State) : integer; extdecl;
var
  exc_message:PChar;
  exception_caught:boolean;
begin
  {
    Using lua_error in functions having automatically-generated "finally" code causes access violations in 32-bit programs.
    So all exceptions are caught in plua_call_class_method_except_wrapper function and
    returned as a simple PChar strings to avoid any automatic release.
    This function forcibly lacks "finally" code - {$IMPLICITEXCEPTIONS OFF}
  }
  Result:=plua_call_except_wrapper(l, exc_message, exception_caught);

  if exception_caught then
    begin
      Result:=0;
      plua_ClearStack(l); //remove both parameters and any non-complete return values.

      if exc_message = nil then
        lua_pushstring(L, 'Unknown exception from native code')
        else
        begin
          lua_pushstring(L, exc_message);
          StrDispose(exc_message);
        end;

      lua_error(L); //does longjmp, almost the same as exception raising
    end;
end;
{$IMPLICITEXCEPTIONS ON}
{$ENDIF}

end.