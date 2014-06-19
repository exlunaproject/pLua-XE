program AssociativeArrayDemo;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  uAssociativeArray,
  luaAssociativeArray;
  
{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

