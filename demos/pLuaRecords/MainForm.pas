unit MainForm;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LuaWrapper,
  pLuaRecord, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Lua : TLua;
  end;

var
  Form1: TForm1; 

implementation

uses
  lua, plua, RecTest;
  
{$R *.dfm}

var
  r : TMyRecord;

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

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Lua := TLua.Create(self);
  Lua.RegisterLuaMethod('ShowMessage', @lua_ShowMessage);
  RegisterMyRecordType(Lua.LuaState);
  RegisterExistingMyRecord(Lua.LuaState, 'r', @r);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if FileExists('script.lua') then
    Lua.Execute;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if FileExists('script.lua') then
    begin
      try
        Lua.LoadFile('script.lua');
        Lua.Execute;
      except
        on e : Exception do
          Caption := e.message;
      end;
    end
  else
    Caption := 'No script.lua file found in path.';
end;

end.

