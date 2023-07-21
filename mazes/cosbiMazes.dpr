program cosbiMazes;

uses
  Forms,
  uCosbiMazes in 'uCosbiMazes.pas' {frmCosbiMazes},
  COSBI_Common in '..\COSBI\COSBI_Common.pas',
  uCOSBI_SystemInfo in '..\COSBI\uCOSBI_SystemInfo.pas',
  Maze in '..\Maze\Maze.pas',
  uCOSBI_TTest in '..\COSBI\uCOSBI_TTest.pas',
  COSBI_Status in '..\COSBI\COSBI_Status.pas' {frmStatus},
  uCOSBI_TGraphicsTest in '..\COSBI\uCOSBI_TGraphicsTest.pas',
  uTests in '..\OSMark\uTests.pas',
  CosbiCpuid in '..\COSBI\CosbiCpuid.pas',
  uOutput in '..\OSMark\uOutput.pas' {frmOutput},
  uStopWatch in '..\COSBI\uStopWatch.pas',
  uMsr in '..\msr\uMsr.pas',
  ActiveDs_TLB in '..\COSBI\ActiveDs_TLB.pas',
  WbemScripting_TLB in '..\COSBI\WbemScripting_TLB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmCosbiMazes, frmCosbiMazes);
  Application.CreateForm(TfrmOutput, frmOutput);
  //  Application.CreateForm(TfrmOutput, frmOutput);
  Application.Run;
end.
