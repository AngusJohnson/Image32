program SmoothPaths;

{$R ..\Resources.res}
{$I Image32.inc}

uses
  Forms,
  Unit1 in 'Unit1.pas' {FrmMain};

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
