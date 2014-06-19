program pLuaRecords;

uses
  Forms,
  MainForm in 'MainForm.pas' {Form1},
  RecTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

