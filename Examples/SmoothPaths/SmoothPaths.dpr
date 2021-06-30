program SmoothPaths;

uses
  Forms,
  Unit1 in 'Unit1.pas' {FrmMain};

{$I Image32.inc}
{$R ..\Resources.res}

begin
{$IFDEF REPORTMEMORYLEAKS}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
  Application.Initialize;
{$IFDEF MAINFORMONTASKBAR}
  Application.MainFormOnTaskbar := True;
{$ENDIF}
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
