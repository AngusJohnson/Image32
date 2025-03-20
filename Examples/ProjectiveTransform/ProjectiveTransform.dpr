program ProjectiveTransform;

uses
  Forms,
  main in 'main.pas' {Form1};

{$I Img32.inc}
{$R ..\Resources.res}

begin
{$IFDEF REPORTMEMORYLEAKS}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  Application.Initialize;
{$IFDEF MAINFORMONTASKBAR}
  Application.MainFormOnTaskbar := True;
{$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
