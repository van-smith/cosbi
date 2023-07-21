program FileCopy;

uses
  Forms,
  uFileCopy in 'uFileCopy.pas' {frmCopyFile},
  COSBI_Common in '..\COSBI\COSBI_Common.pas',
  uCOSBI_SystemInfo in '..\COSBI\uCOSBI_SystemInfo.pas',
  CosbiCpuid in '..\COSBI\CosbiCpuid.pas',
  COSBI_Status in '..\COSBI\COSBI_Status.pas' {frmStatus},
  uStopWatch in '..\COSBI\uStopWatch.pas',
  uMsr in '..\msr\uMsr.pas',
  ActiveDs_TLB in '..\COSBI\ActiveDs_TLB.pas',
  WbemScripting_TLB in '..\COSBI\WbemScripting_TLB.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'COSBI File Copy Benchmark';
  Application.CreateForm(TfrmCopyFile, frmCopyFile);
  Application.Run;
end.
