program Interpolation;

uses
  Forms,
  Main in 'Main.pas' {MainForm};

{$I Img32.inc}
{$R ..\Resources.res}

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
