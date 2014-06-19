unit MainForm;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LuaWrapper;

type
  { TfrmMain }
  TfrmMain = class(TForm)
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
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
  lua, plua, LuaButton, LuaObject;
  
{$R *.dfm}

function lua_ShowMessage(l : PLua_State) : integer; cdecl;
var
  n, i : Integer;
  msg : String;
begin
  result := 0;
  n := lua_gettop(l);
  if n > 0 then
    begin
      msg := '';
      for i := 1 to n do
        msg := msg + lua_tostring(L, i);
      ShowMessage(msg);
    end;
end;

{ TfrmMain }

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  ClearObjects;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Lua := TLua.Create(self);
  Lua.RegisterLuaMethod('ShowMessage', @lua_ShowMessage);
  RegisterLuaButton(Lua.LuaState);
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if FileExists('script.lua') then
    begin
      Lua.LoadFile('script.lua');
      Lua.Execute;
    end;
end;

end.

