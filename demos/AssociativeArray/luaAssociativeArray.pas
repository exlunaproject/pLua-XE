unit luaAssociativeArray;

interface

uses
  Lua, Classes, SysUtils, uAssociativeArray;

procedure RegisterAssociativeArray(L : Plua_State);
procedure RegisterExistingAssociativeArray(L : Plua_State; ar : TAssociativeArray; instanceName : AnsiString);

implementation

uses
  pLua, pLuaObject;

var
  AssociativeArrayInfo : TLuaClassInfo;

function newAssociativeArray(l : PLua_State; paramidxstart, paramcount : Integer; InstanceInfo : PLuaInstanceInfo) : TObject;
begin
  result := TAssociativeArray.Create;
end;

function _indexAssociativeArray(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  vName : String;
begin
  vName := lua_tostring(l, paramidxstart-1);
  plua_pushvariant(l, TAssociativeArray(target).Values[vName]);
  result := 1;
end;

function _newindexAssociativeArray(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  vName : String;
  vValue: Variant;
begin
  vName := lua_tostring(l, paramidxstart-1);
  vValue := plua_tovariant(l, paramidxstart);
  TAssociativeArray(target).Values[vName] := vValue;
  result := 0;
end;

procedure RegisterAssociativeArray(L: Plua_State);
begin
  plua_registerclass(L, AssociativeArrayInfo);
end;

procedure RegisterExistingAssociativeArray(L: Plua_State;
  ar: TAssociativeArray; instanceName: AnsiString);
begin
  plua_registerExisting(l, instanceName, ar, @AssociativeArrayInfo, false);
end;

function setAssociativeArrayInfo : TLuaClassInfo;
begin
  plua_initClassInfo(result);
  result.ClassName := 'TAssociativeArray';
  result.New := @newAssociativeArray;
  result.UnhandledReader := @_indexAssociativeArray;
  result.UnhandledWriter := @_newindexAssociativeArray;
end;

initialization
  AssociativeArrayInfo := setAssociativeArrayInfo;
  
end.

