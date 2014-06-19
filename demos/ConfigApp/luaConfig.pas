unit luaConfig;

interface

uses
  Classes, SysUtils, lua, pLua, pLuaRecord;

implementation

uses
  MainForm;

function GetCaption(RecordPointer : pointer; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  // Get the value of the caption and put it on the stack
  lua_pushstring(l, frmMain.Caption);
  result := 1;
end;

function SetCaption(RecordPointer : pointer; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  // Get the new caption from the stack and set frmMain.Caption to it
  frmMain.Caption := lua_tostring(L, paramidxstart);
  result := 0;
end;

function GetColor(RecordPointer : pointer; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  // Get the value of the Color and put it on the stack
  lua_pushinteger(l, frmMain.Color);
  result := 1;
end;

function SetColor(RecordPointer : pointer; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  // Get the new Color from the stack and set frmMain.Color to it
  frmMain.Color := lua_tointeger(L, paramidxstart);
  result := 0;
end;

procedure Init;
var
  ri : PLuaRecordInfo;
begin
  // Create a virtual "Config" global variable (record) that will allow
  // the lua script to access application properties.
  ri := RecordTypesList.Add('TConfig');
  plua_AddRecordProperty(ri^, 'Caption', @GetCaption, @SetCaption);
  plua_AddRecordProperty(ri^, 'Color', @GetColor, @SetColor);
end;

initialization

Init;

finalization

end.

