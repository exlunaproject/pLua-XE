program LuaObjects;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  LuaButton;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

