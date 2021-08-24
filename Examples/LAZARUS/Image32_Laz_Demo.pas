program Image32_Laz_Demo;

uses
  Forms, Interfaces, main;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
