unit Img32.Panels.Reg;

// Design-time registration for the Image32 VCL components.
// Img32.Panels is compiled into the Img32_Library runtime package so that its
// dcu is available to applications on every platform. This unit only exists
// so the design package has a Register procedure for the IDE to call.

interface

procedure Register;

implementation

uses
  Img32.Panels;

procedure Register;
begin
  Img32.Panels.Register;
end;

end.
