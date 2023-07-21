program MSR;

uses
  Forms,
  ufrmMSR in 'ufrmMSR.pas' {frmMsrTest},
  COSBI_Common in '..\COSBI\COSBI_Common.pas',
  uMsr in 'uMsr.pas',
  CosbiCpuid in '..\COSBI\CosbiCpuid.pas',
  WbemScripting_TLB in '..\COSBI\WbemScripting_TLB.pas',
  ActiveDs_TLB in '..\COSBI\ActiveDs_TLB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMsrTest, frmMsrTest);
  Application.Run;
end.
