unit MainForm;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uAssociativeArray, luaAssociativeArray, LuaWrapper, Variants;

type
  { TfrmMain }
  TfrmMain = class(TForm)
    btnRead: TButton;
    btnWrite: TButton;
    edName: TEdit;
    edValue: TEdit;
    lblName: TLabel;
    lblValue: TLabel;
    procedure btnReadClick(Sender: TObject);
    procedure btnWriteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    ar : TAssociativeArray;
    lua: TLua;
  end; 

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ar := TAssociativeArray.Create;
  lua := TLua.Create(self);
  if FileExists(ExtractFilePath(ParamStr(0))+'script.lua') then
    begin
      RegisterAssociativeArray(Lua.LuaState);
      RegisterExistingAssociativeArray(lua.LuaState, ar, 'ar');
      lua.LoadFile(ExtractFilePath(ParamStr(0))+'script.lua');
      lua.Execute;
    end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  ar.Free;
end;

procedure TfrmMain.btnReadClick(Sender: TObject);
var
  v : Variant;
begin
  if trim(edName.Text) = '' then
    exit;
  v := ar.Values[edName.Text];
  if v <> NULL then
    edValue.Text := String(v)
  else
    edValue.Text := '';
end;

procedure TfrmMain.btnWriteClick(Sender: TObject);
begin
  if trim(edName.Text) = '' then
    exit;
  ar.Values[edName.Text] := edValue.Text;
end;

end.

