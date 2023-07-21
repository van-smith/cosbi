program msrTest;

uses
  Forms,
  uMsrTest in 'uMsrTest.pas' {Form1},
  COSBI_Common in '..\COSBI\COSBI_Common.pas',
  CosbiCpuid in '..\COSBI\CosbiCpuid.pas',
  uWinRing0 in 'uWinRing0.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
