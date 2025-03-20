program Svg101;

{$I Img32.inc}

uses
  {$IFDEF FPC}
  Interfaces,
  {$ENDIF}
  Forms,
  main in 'main.pas' {Form1};

{$R ..\resources.res}

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
