program WhetBurn;

uses
  Forms,
  ufrmWhetBurn in 'ufrmWhetBurn.pas' {frmWhetBurn},
  COSBI_Common in '..\COSBI\COSBI_Common.pas',
  uCOSBI_TTest in '..\COSBI\uCOSBI_TTest.pas',
  uTests in '..\OSMark\uTests.pas',
  Whetstone in '..\DhryWhet\Whetstone.pas',
  COSBI_Status in '..\COSBI\COSBI_Status.pas' {frmStatus},
  uOutput in '..\OSMark\uOutput.pas' {frmOutput},
  Maze in '..\Maze\Maze.pas',
  uCOSBI_TGraphicsTest in '..\COSBI\uCOSBI_TGraphicsTest.pas',
  uStopWatch in '..\COSBI\uStopWatch.pas',
  uMsr in '..\msr\uMsr.pas',
  CosbiCpuid in '..\COSBI\CosbiCpuid.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'WhetBurn: A program to detect thermal throttling';
  Application.CreateForm(TfrmWhetBurn, frmWhetBurn);
  Application.CreateForm(TfrmOutput, frmOutput);
  Application.Run;
end.
