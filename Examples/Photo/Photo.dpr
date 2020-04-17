program Photo;

uses
  Forms,
  main in 'main.pas' {MainForm},
  ZipEx in '..\ZipEx.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
