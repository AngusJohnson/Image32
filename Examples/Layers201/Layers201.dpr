program Layers201;

{$R ..\Resources.res}
{$I Image32.inc}

uses
  Forms,
  main in 'main.pas' {MainForm};

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
