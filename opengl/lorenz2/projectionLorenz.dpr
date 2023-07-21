program projectionLorenz;

uses
  Forms,
  uprojectionLorentz in 'uprojectionLorentz.pas' {frmLorenzAttractor};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Lorenz Attractor OpenGL';
  Application.CreateForm(TfrmLorenzAttractor, frmLorenzAttractor);
  Application.Run;
end.
