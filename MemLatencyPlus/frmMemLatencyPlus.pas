unit frmMemLatencyPlus;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Gauges, StdCtrls, COSBI_Common, ExtCtrls, TeEngine, Series, TeeProcs,
  Chart, Mask, jpeg, uCOSBI_SystemInfo, uStopWatch;

const
  ARRAY_SIZE        = 2000000;
  REPEAT_COUNT      = 20;
  DEFAULT_FILENAME  = 'MemLatency_OUT';
  VERSION           = '0.75';
  GRAPH_PAD_MAX     = 1.01;

type
  TBigArray = array[0..ARRAY_SIZE] of Integer;

  TfrmMemLate = class(TForm)
    GaugeProgress: TGauge;
    edAvg: TEdit;
    Panel1: TPanel;
    chartBandwidth: TChart;
    cboxGraphRealTime: TCheckBox;
    edMinMax: TEdit;
    btnScale: TButton;
    Series1: TLineSeries;
    btnGraphLatency: TButton;
    btnSaveGraph: TButton;
    btnSave: TButton;
    edStatus: TEdit;
    edResult: TEdit;
    btnLatency: TButton;
    btnOverhead: TButton;
    lblIterations: TLabel;
    cboIterations: TComboBox;
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnBandwidth: TButton;
    Bevel3: TBevel;
    btnReset: TButton;
    SaveDialog1: TSaveDialog;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnLatencyClick(Sender: TObject);
    procedure btnOverheadClick(Sender: TObject);
    procedure btnBandwidthClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnScaleClick(Sender: TObject);
    procedure btnSaveGraphClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnGraphLatencyClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
  private
    { Private declarations }
    fHaveGraphData    : Boolean;
    fLatencyGraph     : Boolean;
    StopWatch         : TStopWatch;
    fRefCntBtnDisable : integer;
    fSystemInfo       : TSystemInfo;
    procedure MeasureOverhead;
    procedure InitGraph(astr_Title, astr_YAxisTitle, astr_XAxisTitle : string );
    procedure MeasureMemLatency;
    procedure GraphMemLatency;
    procedure GraphBandwidth;
    procedure DisableButtons;
    procedure EnableButtons;
    procedure ClearText;
    procedure ResetGraph;
    procedure SaveGraphData(as_Title    : string;
                            as_Y_label  : string;
                            as_X_label  : string;
                            as_Filename : string );
  public
    { Public declarations }
    property SystemInfo: TSystemInfo        read  fSystemInfo
                                            write  fSystemInfo;
  end;

var
  frmMemLate: TfrmMemLate;

implementation

{$R *.DFM}

var
   e_Overhead     : Extended;

procedure TfrmMemLate.InitGraph(astr_Title,
                                astr_YAxisTitle,
                                astr_XAxisTitle : string );
begin

  chartBandwidth.Title.Text.Clear;
  chartBandwidth.Title.Text.Add( astr_Title );
  chartBandwidth.LeftAxis.Title.Caption := astr_YAxisTitle;
  chartBandwidth.BottomAxis.Title.Caption := astr_XAxisTitle;
  chartBandwidth.SeriesList[0].Clear;

end; // procedure TfrmMemLate.InitGraph

procedure TfrmMemLate.btnLatencyClick(Sender: TObject);
begin
  MeasureMemLatency;
end;

procedure TfrmMemLate.MeasureOverhead;
var
   li_iterate     : integer;
   li_fill        : integer;
   li_FromElement : integer;
   li_ToElement   : integer;
   le_Time        : extended;

begin
  edResult.Text := 'Measuring overhead...';
  Refresh;
  GaugeProgress.MaxValue := REPEAT_COUNT;
  GaugeProgress.Progress := 0;

  DisableButtons;

  StopWatch.StartTimer;
  for li_iterate := 1 to REPEAT_COUNT do
  begin
    for li_fill := 1 to 10*ARRAY_SIZE do
    begin
      li_FromElement := Random(ARRAY_SIZE);
      li_ToElement := Random(ARRAY_SIZE);
    end;
    GaugeProgress.Progress := GaugeProgress.Progress + 1;
  end;
  le_Time := StopWatch.StopTimer;

  e_Overhead := le_Time;
  GaugeProgress.Progress := GaugeProgress.MaxValue;
  edResult.Text := 'Overhead: ' + FloatToStrF(le_Time,ffNumber, 10, 4) + ' seconds';
  Refresh;

  EnableButtons;

end;

procedure TfrmMemLate.btnOverheadClick(Sender: TObject);
begin
  MeasureOverhead;
end;

procedure TfrmMemLate.btnBandwidthClick(Sender: TObject);
begin
  AlphaBlend := FALSE;
  GraphBandwidth;
  AlphaBlend := TRUE;
end;

procedure TfrmMemLate.FormCreate(Sender: TObject);
begin
  StopWatch := TStopWatch.Create;
  StopWatch.Resolution := swresCPU;

  Caption := Caption + ' (Version ' + VERSION + '), CPU Clockspeed: ' +
    FloatToStrF( StopWatch.GetCPUClockspeed( FALSE ), ffNumber, 10, 0) + ' Hz';

  cboIterations.ItemIndex := 3;
  fHaveGraphData := FALSE;
  fLatencyGraph := FALSE;

  fRefCntBtnDisable := 0;
end;

procedure TfrmMemLate.FormDestroy(Sender: TObject);
begin
  StopWatch.Free;
end;

procedure TfrmMemLate.btnScaleClick(Sender: TObject);
begin
  if chartBandwidth.LeftAxis.Automatic then begin
    chartBandwidth.LeftAxis.Automatic := FALSE;
    chartBandwidth.LeftAxis.Minimum := 0;
  end else begin
    chartBandwidth.LeftAxis.Automatic := TRUE;
  end;
end;

procedure TfrmMemLate.btnSaveGraphClick(Sender: TObject);
begin
  SaveDialog1.DefaultExt := 'bmp';
  if SaveDialog1.Execute then begin
    chartBandwidth.SaveToBitmapFile(SaveDialog1.FileName);
  end; //if
end; // procedure TfrmMemLate.btnSaveGraphClick

procedure TfrmMemLate.btnSaveClick(Sender: TObject);
begin
  SaveDialog1.DefaultExt := 'txt';
  if SaveDialog1.Execute then begin
    if fLatencyGraph then begin
      SaveGraphData('Memory Latency Output File',
                    'Size(kB)',
                    'Assignments / s',
                    SaveDialog1.Filename);
    end else begin
      SaveGraphData('Repeatable Memory Bandwidth Output File',
                    'B/s     ',
                    'Iteration',
                    SaveDialog1.Filename);
    end; // if fLatencyGraph
  end; // if SaveDialog1
end; // procedure TfrmMemLate.btnSaveClick

procedure TfrmMemLate.MeasureMemLatency;
var
   l_A            : TBigArray;
   l_B            : TDynamicIntegerArray;
   li_iterate     : integer;
   li_fill        : integer;
   li_FromElement : integer;
   li_ToElement   : integer;
   le_Time        : extended;
   ls_Overhead    : string;

begin
  DisableButtons;

  if e_Overhead = 0 then
  begin
    MeasureOverhead;
  end;
  ls_Overhead := edResult.Text;
  ClearText;
  edResult.Text := ls_Overhead;

  edStatus.Text := 'Testing...';
  Refresh;
  GaugeProgress.MaxValue := REPEAT_COUNT;
  GaugeProgress.Progress := 0;

  SetLength(l_B, ARRAY_SIZE);
  RandomizeArray(l_B);

  StopWatch.StartTimer;
  for li_iterate := 1 to REPEAT_COUNT do
  begin
    for li_fill := 1 to 10*ARRAY_SIZE do
    begin
      li_FromElement := Random(ARRAY_SIZE);
      li_ToElement := Random(ARRAY_SIZE);
      l_A[li_ToElement] := l_B[li_FromElement];
    end;
    GaugeProgress.Progress := GaugeProgress.Progress + 1;
  end;
  le_Time := StopWatch.StopTimer;

  GaugeProgress.Progress := GaugeProgress.MaxValue;
  edStatus.Text := 'Finished in ' + FloatToStrF(le_Time,ffNumber, 10, 4) +
    ' seconds';
  edAvg.Text := 'Adjusted time: ' +
    FloatToStrF((le_Time - e_Overhead), ffNumber, 10, 4) + ' seconds';

  EnableButtons;

end; // procedure MeasureMemLatency;

procedure TfrmMemLate.GraphMemLatency;
const
  ONE_K = 256; // 1 kB in memory of long integers
  MAX_SIZE = 2 * 1048576; // go through 2M element (8 MB) arrays
  STEP_SIZE = 4 * ONE_K; // increase array size in 4kB increments
  ASSIGN_COUNT = 200000; // the number of latency intensive assignments
  UNROLL_FACTOR = 8; // unroll the loop
var
   // main arrays to move data between to measure latency
   l_A            : array[0..MAX_SIZE] of Integer;
   l_B            : TDynamicIntegerArray;

   // main loop variables
   li_iterate     : integer;
   li_RepeatCount : integer;
   ld_Sum         : double; // accumulate results

   li_fill        : integer;
   li_fill_count  : integer;
   li_FromElement : integer;
   li_ToElement   : integer;
   le_Time        : extended;
   le_TotalTime   : extended;

   li_size        : integer; // current array size (in elements) being stressed
   li_max_size    : integer; // maximum array size (in elements) being stressed
   ld_AssignPerSec: double;

begin
  // initialize text fields:
  ClearText;
  edStatus.Text := 'Testing...';

  ResetGraph;

  // disable buttons:
  DisableButtons;
  // initialize graph:
  InitGraph( 'Memory Latency',
             'assignments per second',
             'memory block size (kB)' );
  // make sure user sees these updates:
  Refresh;
  // initialize bar graph:
  GaugeProgress.MaxValue := round( MAX_SIZE / STEP_SIZE );
  GaugeProgress.Progress := 0;
  // set up source array:
  SetLength(l_B, MAX_SIZE);
  RandomizeArray(l_B);

  // initialize variables:
  le_TotalTime := 0;
  ld_Sum := 0;
  li_size := 0;

  // calculate the number of iterations:
  li_RepeatCount := MAX_SIZE div STEP_SIZE;

  // calculate the number of assignment loops:
  li_fill_count := ASSIGN_COUNT div UNROLL_FACTOR;

  for li_iterate := 1 to li_RepeatCount do begin

    // now let's vary the memory footprint we test over.
    li_size := li_size + STEP_SIZE;

    // here is the crucial code to test latency:
    StopWatch.StartTimer;
    for li_fill := 1 to li_fill_count do begin

      li_FromElement := Random(li_size);
      li_ToElement := Random(li_size);
      l_A[li_ToElement] := l_B[li_FromElement];

      li_FromElement := Random(li_size);
      li_ToElement := Random(li_size);
      l_A[li_ToElement] := l_B[li_FromElement];

      li_FromElement := Random(li_size);
      li_ToElement := Random(li_size);
      l_A[li_ToElement] := l_B[li_FromElement];

      li_FromElement := Random(li_size);
      li_ToElement := Random(li_size);
      l_A[li_ToElement] := l_B[li_FromElement];

      li_FromElement := Random(li_size);
      li_ToElement := Random(li_size);
      l_A[li_ToElement] := l_B[li_FromElement];

      li_FromElement := Random(li_size);
      li_ToElement := Random(li_size);
      l_A[li_ToElement] := l_B[li_FromElement];

      li_FromElement := Random(li_size);
      li_ToElement := Random(li_size);
      l_A[li_ToElement] := l_B[li_FromElement];

      li_FromElement := Random(li_size);
      li_ToElement := Random(li_size);
      l_A[li_ToElement] := l_B[li_FromElement];

    end;
    le_Time := StopWatch.StopTimer;
    le_TotalTime := le_TotalTime + le_Time;

    ld_AssignPerSec := ASSIGN_COUNT / le_Time;
    ld_Sum := ld_Sum + ld_AssignPerSec;
    // now graph result:
    chartBandwidth.SeriesList[0].Add(ld_AssignPerSec ,
      ' ' + IntToStr(2 * li_size div ONE_K) + ' ', clRed);

    chartBandwidth.LeftAxis.Maximum :=
      GRAPH_PAD_MAX * chartBandwidth.SeriesList[0].MaxYValue;

    edMinMax.Text := 'Max = ' +
      FloatToStrF(chartBandwidth.SeriesList[0].MaxYValue,
        ffNumber, 10, 1) + ', Min = ' +
        FloatToStrF(chartBandwidth.SeriesList[0].MinYValue,
        ffNumber, 10, 1);

    edAvg.Text := 'Avg = ' + FloatToStrF(ld_Sum / li_iterate, ffNumber, 10, 1);
    GaugeProgress.Progress := GaugeProgress.Progress + 1;

    if (cboxGraphRealTime.State = cbChecked) and (li_size > STEP_SIZE) then
      Application.ProcessMessages;

  end; //for li_iterate := 1 to li_RepeatCount do begin

  GaugeProgress.Progress := GaugeProgress.MaxValue;
  edStatus.Text := 'Finished in ' + FloatToStrF(le_TotalTime,ffNumber, 10, 4) +
    ' seconds';

  fHaveGraphData := TRUE;
  EnableButtons;
  fLatencyGraph := TRUE;

end; // GraphMemLatency

procedure TfrmMemLate.btnGraphLatencyClick(Sender: TObject);
begin
  AlphaBlend := FALSE;
  GraphMemLatency;
  AlphaBlend := TRUE;
end; // procedure TfrmMemLate.btnGraphLatencyClick

procedure TfrmMemLate.DisableButtons;
begin
  fRefCntBtnDisable := fRefCntBtnDisable + 1;
  if fRefCntBtnDisable = 1 then begin
    btnGraphLatency.Enabled   := FALSE;
    btnBandwidth.Enabled      := FALSE;
    btnScale.Enabled          := FALSE;
    btnLatency.Enabled        := FALSE;
    btnOverhead.Enabled       := FALSE;
    btnSave.Enabled           := FALSE;
    btnSaveGraph.Enabled      := FALSE;
    btnReset.Enabled          := FALSE;
  end;
end; // procedure TfrmMemLate.DisableButtons

procedure TfrmMemLate.EnableButtons;
begin
  fRefCntBtnDisable := fRefCntBtnDisable - 1;
  if fRefCntBtnDisable <= 0 then begin
    btnGraphLatency.Enabled   := TRUE;
    btnBandwidth.Enabled      := TRUE;
    btnScale.Enabled          := TRUE;
    btnLatency.Enabled        := TRUE;
    btnOverhead.Enabled       := TRUE;
    btnReset.Enabled          := TRUE;
    if fHaveGraphData then begin
      btnSave.Enabled           := TRUE;
      btnSaveGraph.Enabled      := TRUE;
    end;
    if fRefCntBtnDisable < 0 then fRefCntBtnDisable := 0;
  end;
end; // procedure TfrmMemLate.EnableButtons;

procedure TfrmMemLate.ClearText;
begin
  edAvg.Text      := '';
  edMinMax.Text   := '';
  edStatus.Text   := '';
  edResult.Text   := '';
end; // procedure TfrmMemLate.ClearText;

procedure TfrmMemLate.GraphBandwidth;

var
   l_A              : TDynamicIntegerArray;
   l_B              : TDynamicIntegerArray;
   li_iterate       : integer;
   li_RepeatCount   : integer;
   le_Time          : Extended;
   ld_Bandwidth     : Double;
   ld_MemoryMoved   : Double;
   lb_GraphNow      : Boolean;
   l_Results        : Array[1..500] of double;

const

  UNROLL_FACTOR = 4;

begin
  DisableButtons;
  ClearText;

  edStatus.Text := 'Testing...';
  Refresh;
  InitGraph( 'Dynamic Memory Bandwidth',
             'Bandwidth (B /s)',
             'Iteration' );

  ResetGraph;
  
  SetLength(l_B, ARRAY_SIZE);
  RandomizeArray(l_B);

  le_Time := 0;
  chartBandwidth.SeriesList[0].Clear;

  li_RepeatCount := StrToInt( Trim( cboIterations.Text ) );

  GaugeProgress.MaxValue := li_RepeatCount;
  GaugeProgress.Progress := 0;

  ld_MemoryMoved := UNROLL_FACTOR * 2 * ARRAY_SIZE * 4;

  lb_GraphNow := (cboxGraphRealTime.State = cbChecked);
  Application.ProcessMessages;

  for li_iterate := 1 to li_RepeatCount do
  begin
    StopWatch.StartTimer;

    l_A := Copy(l_B,0,ARRAY_SIZE);
    l_B := Copy(l_A,0,ARRAY_SIZE);
    l_A := Copy(l_B,0,ARRAY_SIZE);
    l_B := Copy(l_A,0,ARRAY_SIZE);

    StopWatch.StopTimer;
    le_Time := le_Time + StopWatch.ElapsedTime;

    ld_Bandwidth := ( ld_MemoryMoved ) / StopWatch.ElapsedTime;

    if lb_GraphNow then begin

      chartBandwidth.SeriesList[0].Add(ld_Bandwidth,
        ' ' + IntToStr(li_iterate) + ' ', clRed);

      chartBandwidth.LeftAxis.Maximum :=
        GRAPH_PAD_MAX * chartBandwidth.SeriesList[0].MaxYValue;
        
      edMinMax.Text := 'Max = ' +
        FloatToStrF(chartBandwidth.SeriesList[0].MaxYValue,
          ffNumber, 10, 1) +
        ', Min = ' +
        FloatToStrF(chartBandwidth.SeriesList[0].MinYValue,
          ffNumber, 10, 1);

      GaugeProgress.Progress := GaugeProgress.Progress + 1;

    end else begin
      l_Results[ li_iterate ] := ld_Bandwidth;
    end;

    if li_iterate > 1 then Application.ProcessMessages;

  end;

  GaugeProgress.Progress := GaugeProgress.MaxValue;
  edStatus.Text := 'Copy time ' + FloatToStrF((le_Time), ffNumber, 6, 6) + ' s';

  ld_Bandwidth := ( ld_MemoryMoved * li_RepeatCount ) / ( le_time * 1000000 );
  edAvg.Text := 'Avg bw: ' +
    FloatToStrF( ld_Bandwidth, ffNumber, 10, 3) + ' MB/s';

  if not lb_GraphNow then begin

    for li_iterate := 1 to li_RepeatCount do begin
      chartBandwidth.SeriesList[0].Add(l_Results[ li_iterate ], '', clRed);
    end;

    edMinMax.Text := 'Max = ' +
      FloatToStrF(chartBandwidth.SeriesList[0].MaxYValue,
        ffNumber, 10, 1) +
      ', Min = ' +
      FloatToStrF(chartBandwidth.SeriesList[0].MinYValue,
        ffNumber, 10, 1);

    chartBandwidth.LeftAxis.Maximum :=
      GRAPH_PAD_MAX * chartBandwidth.SeriesList[0].MaxYValue;
      
  end;

  fHaveGraphData := TRUE;
  EnableButtons;
  fLatencyGraph := FALSE;

end; // procedure TfrmMemLate.GraphBandwidth;

procedure TfrmMemLate.SaveGraphData(as_Title    : string;
                                    as_Y_label  : string;
                                    as_X_label  : string;
                                    as_Filename : string );
var
  ltxtfile_out  : TextFile;
  ls_FileName   : string;
  ls_OutLine    : string;
  ld_x          : double;
  n             : integer;
begin
  if not assigned( fSystemInfo ) then begin
    fSystemInfo := TSystemInfo.Create( self );
    fSystemInfo.ShowStatus := TRUE;
    fSystemInfo.StopWatch := StopWatch;
    fSystemInfo.Initialize;
  end; // if
  ls_FileName := Trim( as_Filename );
  if ls_FileName = '' then begin
    ls_FileName := DEFAULT_FILENAME;
    ls_FileName := ls_FileName + '.txt';
  end; // if
  AssignFile( ltxtfile_out, ls_FileName );
  Rewrite( ltxtfile_out );
  WriteLn( ltxtfile_out, as_Title);
  ls_OutLine := 'Date: ' + DateToStr( Date ) + ', Time: ' + TimeToStr( Time );
  WriteLn( ltxtfile_out, ls_OutLine );
  WriteLn( ltxtfile_out, '' );
  WriteLn( fSystemInfo.GetSystemSummary );

  WriteLn( ltxtfile_out, '' );
  ls_OutLine := edAvg.Text + ', ' + edMinMax.Text + ', ' + edStatus.Text;
  WriteLn( ltxtfile_out, ls_OutLine );
  WriteLn( ltxtfile_out, as_Y_label + '  ' + as_X_label);
  WriteLn( ltxtfile_out, '========  ===============');
  for n := 0 to chartBandwidth.Series[0].Count - 1 do begin
    ld_x := StrToFloat( chartBandwidth.Series[0].XLabel[n] );
    ls_Outline := Format('%6n' + Chr(9) + '   %11n',
      [ld_x, chartBandwidth.Series[0].YValue[n]]);
    WriteLn( ltxtfile_out, ls_Outline);
  end;
  CloseFile( ltxtfile_out );
end;

procedure TfrmMemLate.ResetGraph;
begin
  chartBandwidth.LeftAxis.Automatic := FALSE;
  chartBandwidth.LeftAxis.Minimum := 0;
  if chartBandwidth.SeriesList[0].MaxYValue > 0 then begin
    chartBandwidth.LeftAxis.Maximum :=
      GRAPH_PAD_MAX * chartBandwidth.SeriesList[0].MaxYValue;
  end;
  chartBandwidth.BottomAxis.Automatic := TRUE;
end;

procedure TfrmMemLate.btnResetClick(Sender: TObject);
begin
  ResetGraph;
end;

procedure TfrmMemLate.FormActivate(Sender: TObject);
begin
  UnFadeFast( self );
end;

procedure TfrmMemLate.FormDeactivate(Sender: TObject);
begin
  FadeFast50( self );
end;

procedure TfrmMemLate.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FadeFast100( self );
end;

procedure TfrmMemLate.FormShow(Sender: TObject);
begin
  UnFadeFast( self );
end;

end.
