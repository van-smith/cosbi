program nBody;

uses
  Forms,
  uNBody in 'uNBody.pas' {frmNBody},
  CosbiCpuid in '..\..\COSBI\CosbiCpuid.pas',
  COSBI_Common in '..\..\COSBI\COSBI_Common.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'OpenGL 4-Body Simulation';
  Application.CreateForm(TfrmNBody, frmNBody);
  Application.Run;
end.
