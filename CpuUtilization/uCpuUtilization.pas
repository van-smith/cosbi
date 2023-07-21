unit uCpuUtilization;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, {cxCpu40,} ExtCtrls, TeeProcs, TeEngine, Chart, Series,
  Gauges, ufrmGraph, Menus, COSBI_Common, ufrmLaunchExternalProgram;

type
  TfrmCpuUtilization = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Actions1: TMenuItem;
    Start1: TMenuItem;
    Stop1: TMenuItem;
    Clear1: TMenuItem;
    Save1: TMenuItem;
    SaveCharts1: TMenuItem;
    Arrange1: TMenuItem;
    ileVertically1: TMenuItem;
    ileHorizontally1: TMenuItem;
    Cascade1: TMenuItem;
    ArrangeAll1: TMenuItem;
    TimerMain: TTimer;
    LaunchprogramandtrackCPUutilization1: TMenuItem;
    BrieflydeployartificalCPUload1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Start1Click(Sender: TObject);
    procedure Stop1Click(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure TimerMainTimer(Sender: TObject);
    procedure ileVertically1Click(Sender: TObject);
    procedure ileHorizontally1Click(Sender: TObject);
    procedure Cascade1Click(Sender: TObject);
    procedure ArrangeAll1Click(Sender: TObject);
    procedure LaunchprogramandtrackCPUutilization1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure SaveCharts1Click(Sender: TObject);
    procedure BrieflydeployartificalCPUload1Click(Sender: TObject);
  private
    { Private declarations }
    fNumberOfProcessors : Cardinal;
    fFrmGraph: array[0..7] of TfrmGraph;
    fNbrOfThreadsRunning : integer;
    procedure ChartClear;
    procedure ChartStart;
    procedure ChartStop;
    procedure CountProcessors;
    procedure InitializeChart;
    procedure ThreadDone(Sender: TObject);
  public
    { Public declarations }
  end;


  // Class name: TExternalProgramThread
  // Author: Van Smith
  // Date: October 4, 2004
  TExternalProgramThread = class(TThread)
  private
    fProgramCommand: string;
    fIterations : integer;
  protected
    procedure execute; override;
  public
    constructor Create;
    property Iterations : integer read fIterations write fIterations;
    property ProgramCommand : string read fProgramCommand write fProgramCommand;
  end; // TExternalProgramThread..................................................

  // Class name: TFibThread
  // Author: Van Smith
  // Date: January 16, 2004
  TFibThread = class(TThread)
  private
    Fn: integer;
  protected
    procedure Execute; override;
    function fib(n : integer): integer;
  public
    constructor Create(n: integer);
  end; // TFibThread............................................................

var
  frmCpuUtilization: TfrmCpuUtilization;

implementation

{$R *.dfm}

// TExternalProgramThread.........................................................
// Class name: TExternalProgramThread
// Author: Van Smith
// Date: October 4, 2004
constructor TExternalProgramThread.Create;
begin
  FreeOnTerminate := True;
  fIterations := 1;
  inherited Create(TRUE);  // creates suspended
end;

procedure TExternalProgramThread.Execute;
var
  i : integer;
begin
  for i := 1 to fIterations do begin
    StartProgramWait( fProgramCommand, SW_NORMAL );
  end; //for
end; //procedure TExternalProgramThread.Execute;
// TExternalProgramThread ends....................................................

// TFibThread...................................................................
constructor TFibThread.Create(n: integer);
begin
  Fn := n;
  FreeOnTerminate := True;
  inherited Create(TRUE);  // creates suspended
end;

procedure TFibThread.Execute;
var
  n : integer;
begin
  n := Fib(Fn);
end; //procedure TFibThread.Execute;

function TFibThread.Fib( n : integer ) : integer;
begin
  if Terminated then begin
    result := -1;
    Exit;
  end; // if
  if n > 2 then
      result := Fib( n - 1 ) + Fib( n - 2 )
  else
      result := 1;
end;
// TFibThread ends..............................................................

// TfrmCpuUtilization begins....................................................
procedure TfrmCpuUtilization.CountProcessors;
var
  liBufferSize : integer;
  lsEnvironmentString : string;
  lsNumberOfProcessors : string;
begin
  fNumberOfProcessors := GetNumberOfProcessors;
end; // procedure TForm1.CountProcessors;

{
procedure TfrmCPUInfo.btnGetCPUInfoClick(Sender: TObject);
begin
  memo1.Lines.Clear;
  memo1.Lines.Add( 'Processor name: ' + cxCpu[0].Name.AsString );
  memo1.Lines.Add( 'Processor signature: ' + cxCpu[0].Signature.Generic );
  memo1.Lines.Add( 'Processor vendor: ' + cxCpu[0].Vendor.Name );
  memo1.Lines.Add( 'Processor type: ' + cxCpu[0].Signature.CpuType.Value.AsString );
  memo1.Lines.Add( 'Processor family: ' + cxCpu[0].Signature.Family.Value.AsString );
  memo1.Lines.Add( 'Processor model: ' + cxCpu[0].Signature.Model.Value.AsString );
  memo1.Lines.Add( 'Processor stepping: ' + cxCpu[0].Signature.Stepping.Value.AsString );
  memo1.Lines.Add( 'Number of processors: ' + cxCpu.ProcessorCount.Available.AsString );
  memo1.Lines.Add( 'Number of virtual processors: ' + cxCpu.ProcessorCount.Logical.AsString );
//  memo1.Lines.Add( 'Vendor: ' + cxCpu[0].Vendor.Name );
  memo1.Lines.Add( 'Vendor signature: ' + cxCpu[0].Vendor.Signature );
//  memo1.Lines.Add( 'Vendor feature set: ' + cxCpu[0].Signature.Brand.Value.AsString );
//  memo1.Lines.Add( 'Processor brand: ' + cxCpu[0].Signature.Brand.Value.AsString );
  memo1.Lines.Add( 'Level 1 instruction cache size: ' + cxCpu.Processors[0].Cache.Level1.Code.Size.FormatBytes );
  memo1.Lines.Add( 'Level 1 instruction cache associativity: ' + cxCpu.Processors[0].Cache.Level1.Code.Associativity.Name );
  memo1.Lines.Add( 'Level 1 data cache size: ' + cxCpu.Processors[0].Cache.Level1.Data.Size.FormatBytes );
  memo1.Lines.Add( 'Level 1 data cache associativity: ' + cxCpu.Processors[0].Cache.Level1.Data.Associativity.Name );
  memo1.Lines.Add( 'Trace cache size: ' + cxCpu.Processors[0].Cache.Trace.Size.FormatBytes );
  memo1.Lines.Add( 'Level 1 unified cache size: ' + cxCpu.Processors[0].Cache.Level1.Unified.Size.FormatBytes );
  memo1.Lines.Add( 'Level 2 cache size: ' + cxCpu.Processors[0].Cache.Level2.Size.FormatBytes );
  memo1.Lines.Add( 'Level 2 cache size: ' + cxCpu.Processors[0].Cache.Level2.Associativity.Name );
  memo1.Lines.Add( 'Level 3 data cache size: ' + cxCpu.Processors[0].Cache.Level3.Size.FormatBytes );
end;
}

procedure TfrmCpuUtilization.FormCreate(Sender: TObject);
var
  i : integer;
begin
  CountProcessors;
  TimerMain.Enabled := TRUE;
  fNbrOfThreadsRunning := 0;
end;

procedure TfrmCpuUtilization.InitializeChart;
var
  i : integer;
begin
  for i := 0 to fNumberOfProcessors do begin
    fFrmGraph[ i ].ChartInitialize;
  end;
end; // procedure TForm1.InitializeChart;

procedure TfrmCpuUtilization.ChartClear;
var
  i : integer;
begin
  for i := 0 to fNumberOfProcessors - 1 do begin
    fFrmGraph[ i ].ChartClear;
  end;
end; // procedure ChartClear;

procedure TfrmCpuUtilization.ChartStart;
var
  i : integer;
begin
  for i := 0 to fNumberOfProcessors - 1 do begin
    fFrmGraph[ i ].ChartStart;
  end;
end; // procedure ChartStart;

procedure TfrmCpuUtilization.ChartStop;
var
  i : integer;
begin
  for i := 0 to fNumberOfProcessors - 1 do begin
    fFrmGraph[ i ].ChartStop;
  end;
end; // procedure ChartStart;

{
procedure TfrmCPUInfo.TimerSetInterval;
var
  liInterval : integer;
begin
  try
    liInterval := StrToInt( Trim( edInterval.Text ) );
    if liInterval < 10 then begin
      showmessage( 'Please enter an interval > 10 ms.' );
    end else if liInterval > 10000 then begin
      showmessage( 'Please enter an interval <= 10000 ms.' );
    end else begin
      Timer1.Enabled := FALSE;
      Timer1.Interval := liInterval;
      Timer1.Enabled := TRUE;
    end;
  except
    ShowMessage( '"' + edInterval.Text + '" is not numeric data for the timer interval.' );
    edInterval.Text := '1000';
  end; // try...
end; // procedure TimerSetInterval;
}

procedure TfrmCpuUtilization.FormDestroy(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to fNumberOfProcessors - 1 do begin
    fFrmGraph[ i ].Release;
  end;
end;

procedure TfrmCpuUtilization.Start1Click(Sender: TObject);
begin
  ChartStart;
end;

procedure TfrmCpuUtilization.Stop1Click(Sender: TObject);
begin
  ChartStop;
end;

procedure TfrmCpuUtilization.Clear1Click(Sender: TObject);
begin
  ChartClear;
end;

procedure TfrmCpuUtilization.TimerMainTimer(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to fNumberOfProcessors - 1 do begin
    fFrmGraph[ i ] := TfrmGraph.Create( frmCpuUtilization );
    fFrmGraph[ i ].CPUNumber := i;
    fFrmGraph[ i ].ChartInitialize;
    fFrmGraph[ i ].ChartStart;
  end;
  TimerMain.Enabled := FALSE;
  TileMode := tbVertical;
  Tile;
end;

procedure TfrmCpuUtilization.ileVertically1Click(Sender: TObject);
begin
  TileMode := tbVertical;
  Tile;
end;

procedure TfrmCpuUtilization.ileHorizontally1Click(Sender: TObject);
begin
  TileMode := tbHorizontal;
  Tile;
end;

procedure TfrmCpuUtilization.Cascade1Click(Sender: TObject);
begin
  Cascade;
end;

procedure TfrmCpuUtilization.ArrangeAll1Click(Sender: TObject);
begin
  ArrangeIcons;
end;

procedure TfrmCpuUtilization.ThreadDone(Sender: TObject);
begin
  dec( fNbrOfThreadsRunning );
  application.ProcessMessages;
end; // procedure TfrmCpuUtilization.FibThreadDone;

procedure TfrmCpuUtilization.LaunchprogramandtrackCPUutilization1Click(
  Sender: TObject);
var
  ffrmLaunchExternalProgram : TfrmLaunchExternalProgram;
  lsProgramPath : string;
  lExternalProgramThread : TExternalProgramThread;
begin
  // Stop tracking cpu utilization
  ChartStop;
  // clear charts
  ChartClear;
  // open form to allow user to enter and launch program
  ffrmLaunchExternalProgram := TfrmLaunchExternalProgram.Create( self );
  try
    ffrmLaunchExternalProgram.ShowModal;
    lsProgramPath := ffrmLaunchExternalProgram.ProgramFilePath;
  finally
    ffrmLaunchExternalProgram.Release;
  end; // try...finally
  if lsProgramPath = '' then exit;
  // start tracking cpu utilization
  ChartStart;
  application.processmessages;
  // launch external program in an seperate thread
  lExternalProgramThread := TExternalProgramThread.Create;
  lExternalProgramThread.ProgramCommand := lsProgramPath;
  lExternalProgramThread.OnTerminate := ThreadDone;
  fNbrOfThreadsRunning := 1;
  lExternalProgramThread.Resume;
  // wait until thread is finished
  while ( fNbrOfThreadsRunning = 1 ) do begin
    sleep( 10 );
    application.processmessages;
  end; // while
  // stop charts
  ChartStop;
end;

procedure TfrmCpuUtilization.Save1Click(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to fNumberOfProcessors - 1 do begin
    fFrmGraph[ i ].SaveData;
  end;
end;

procedure TfrmCpuUtilization.SaveCharts1Click(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to fNumberOfProcessors - 1 do begin
    fFrmGraph[ i ].SaveGraph;
  end;
end;


procedure TfrmCpuUtilization.BrieflydeployartificalCPUload1Click(
  Sender: TObject);
var
  lFibThread : TFibThread;
begin
  lFibThread := TFibThread.Create( 45 );
  try
    lFibThread.OnTerminate := ThreadDone;
    lFibThread.Resume;
    inc( fNbrOfThreadsRunning );
    // wait until thread is finished
    while ( fNbrOfThreadsRunning = 1 ) do begin
      sleep( 10 );
      application.processmessages;
    end; // while
  finally

  end; // try..finally
end;

end.
