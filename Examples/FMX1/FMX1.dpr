program FMX1;



uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {MainForm};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
