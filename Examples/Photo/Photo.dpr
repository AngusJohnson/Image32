program Photo;

uses
  Forms,
  main in 'main.pas' {MainForm},
  ZipEx in '..\ZipEx.pas',
  DialogsEx in '..\DialogsEx.pas';

{$R *.res}

begin
  //ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
