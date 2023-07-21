program MemLatencyPlus;



uses
  Forms,
  frmMemLatencyPlus in 'frmMemLatencyPlus.pas' {frmMemLate},
//  NonGraphicsBenchmarks in '..\COSBI\NonGraphicsBenchmarks.pas',
  COSBI_Common in '..\COSBI\COSBI_Common.pas',
  uCOSBI_SystemInfo in '..\COSBI\uCOSBI_SystemInfo.pas',
  uStopWatch in '..\COSBI\uStopWatch.pas',
  uMsr in '..\msr\uMsr.pas',
  COSBI_Status in '..\COSBI\COSBI_Status.pas' {frmStatus},
  ActiveDs_TLB in '..\COSBI\ActiveDs_TLB.pas',
  WbemScripting_TLB in '..\COSBI\WbemScripting_TLB.pas',
  CosbiCpuid in '..\COSBI\CosbiCpuid.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMemLate, frmMemLate);
  Application.Run;
end.
