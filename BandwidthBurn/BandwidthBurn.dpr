program BandwidthBurn;



uses
  Forms,
  frmBandwidthBurn in 'frmBandwidthBurn.pas' {frmCOSBIBandwidthBurn},
  COSBI_Common in '..\COSBI\COSBI_Common.pas',
  uCOSBI_SystemInfo in '..\COSBI\uCOSBI_SystemInfo.pas',
  uBandwidthBurn in 'uBandwidthBurn.pas',
  CosbiCpuid in '..\COSBI\CosbiCpuid.pas',
  COSBI_Status in '..\COSBI\COSBI_Status.pas' {frmStatus},
  uStopWatch in '..\COSBI\uStopWatch.pas',
  uMsr in '..\msr\uMsr.pas',
  ActiveDs_TLB in '..\COSBI\ActiveDs_TLB.pas',
  WbemScripting_TLB in '..\COSBI\WbemScripting_TLB.pas';

{$R *.RES}
{$MAXSTACKSIZE 33554432}
begin
  Application.Initialize;
  Application.Title := 'COSBI BandwidthBurn';
  Application.CreateForm(TfrmCOSBIBandwidthBurn, frmCOSBIBandwidthBurn);
  Application.Run;
end.
