program ConfigApp;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

