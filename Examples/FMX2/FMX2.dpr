program FMX2;





uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {MainForm};

{$I Img32.inc}
{$R ..\Resources.res}

begin
{$IFDEF REPORTMEMORYLEAKS}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

