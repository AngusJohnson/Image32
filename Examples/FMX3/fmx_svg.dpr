program fmx_svg;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  unit1 in 'unit1.pas' {Form1};

{$I Img32.inc}
{$R *.res}

begin
{$IFDEF REPORTMEMORYLEAKS}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
