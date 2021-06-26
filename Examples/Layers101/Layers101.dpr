program Layers101;

{$R ..\Resources.res}
{$I Image32.inc}

uses
  Forms,
  Unit1 in 'Unit1.pas' {MainForm};

begin
{$IFDEF REPORTMEMORYLEAKS}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
  Application.Initialize;
{$IFDEF MAINFORMONTASKBAR}
  Application.MainFormOnTaskbar := True;
{$ENDIF}
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
