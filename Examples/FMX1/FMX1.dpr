program FMX1;

{$R ..\Resources.res}
{$I Image32.inc}

uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {MainForm};

begin
{$IFDEF REPORTMEMORYLEAKS}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
