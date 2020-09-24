program Image32_Laz_Demo;

{$R *.dres}

uses
  Forms, Interfaces, main;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
