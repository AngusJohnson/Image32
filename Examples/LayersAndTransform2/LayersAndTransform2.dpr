program LayersAndTransform2;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}
{$I Image32.inc}

begin
  Application.Initialize;
{$IFDEF MAINFORMONTASKBAR}
  Application.MainFormOnTaskbar := True;
{$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
