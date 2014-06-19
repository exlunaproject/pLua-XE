program ConfigApp;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  luaConfig;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

