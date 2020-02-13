program Photo;

uses
  Forms,
  main in 'main.pas' {MainForm},
  ZipHelper in 'ZipHelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
