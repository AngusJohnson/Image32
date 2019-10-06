program LayersSmoothPaths;

uses
  Forms,
  Unit1 in 'Unit1.pas' {FrmMain};

{$R *.res}
{$I Image32.inc}

begin
  Application.Initialize;
{$IFDEF MAINFORMONTASKBAR}
  Application.MainFormOnTaskbar := True;
{$ENDIF}
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
