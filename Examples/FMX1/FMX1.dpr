program FMX1;

uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {MainForm};

{$I Image32.inc}
{$R ..\Resources.res}

begin
{$IFDEF REPORTMEMORYLEAKS}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
