program RasterToSVG;



uses
  Forms,
  Unit1 in 'Unit1.pas' {MainForm},
  GifImage in 'GifImage.pas',
  Image32_GIF in 'Image32_GIF.pas',
  DialogsEx in '..\DialogsEx.pas';

{$R *.res}
{$I Image32.inc}

begin
{$IFDEF REPORTMEMORYLEAKS}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
{$ENDIF}
  Application.Initialize;
{$IFDEF MAINFORMONTASKBAR}
  Application.MainFormOnTaskbar := True;
{$ENDIF}
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
