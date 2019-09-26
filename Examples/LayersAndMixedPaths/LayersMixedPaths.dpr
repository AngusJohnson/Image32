program LayersMixedPaths;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {FrmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
