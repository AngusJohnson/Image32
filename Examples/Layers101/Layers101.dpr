program Layers101;

uses
  Forms,
  Unit1 in 'Unit1.pas' {MainForm};

{$R *.res}
{$I Image32.inc}

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
