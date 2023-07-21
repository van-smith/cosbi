program MazeMaker;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  Maze in 'Maze.pas',
  COSBI_Common in '..\COSBI\COSBI_Common.pas',
  uStopWatch in '..\COSBI\uStopWatch.pas',
  uMsr in '..\msr\uMsr.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
