unit MainForm;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, lua, pLua,
  LuaWrapper;

type
  { TfrmMain }
  TfrmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Lua : TLua;
  end; 

var
  frmMain: TfrmMain;

implementation

uses
  luaConfig, pluaRecord;
  
{$R *.dfm}

function lua_HexToInt(L : PLua_State) : integer; cdecl;
begin
  result := 1;
  lua_pushinteger(L, StrToInt('$'+lua_tostring(L, 1)));
end;

function lua_SetConfig(L : PLua_State) : integer; cdecl;
begin
  result := 0;
  lua_pushliteral(L, 'Caption');
  lua_gettable(L, -2);
  if not lua_isnil(L, -1) then
    frmMain.Caption := lua_tostring(L, -1);
  lua_pop(L, 1);
  lua_pushliteral(L, 'Color');
  lua_gettable(L, -2);
  if not lua_isnil(L, -1) then
    frmMain.Color := lua_tointeger(L, -1);
  lua_pop(L, 1);
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Lua := TLua.Create(self);
  Lua.LoadFile('config.lua');
  Lua.RegisterLuaMethod('HexToInt', @lua_HexToInt);
  Lua.RegisterLuaMethod('SetConfig', @lua_SetConfig);
  // Create a "new" version of our virtual record type and register it to the lua
  // global name of "Config"
  plua_registerExistingRecord(Lua.LuaState, 'Config', nil, RecordTypesList['TConfig']);
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  Lua.Execute;
end;

end.

