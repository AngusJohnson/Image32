program Vectorize;

{$R ..\Resources.res}
{$I Image32.inc}

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

begin
{$IFDEF REPORTMEMORYLEAKS}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
{$ENDIF}
  Application.Initialize;
{$IFDEF MAINFORMONTASKBAR}
  Application.MainFormOnTaskbar := True;
{$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
