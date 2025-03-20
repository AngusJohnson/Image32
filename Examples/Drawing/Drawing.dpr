program Drawing;

uses
  Forms,
  Main in 'Main.pas' {fmMain},
  Color32Dialog in '..\Color32Dialog.pas' {Color32DialogForm},
  DialogsEx in '..\DialogsEx.pas';


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
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.