unit ufrmWhetBurn;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Gauges, StdCtrls, COSBI_Common, ExtCtrls, TeEngine, Series, TeeProcs,
  Chart, Mask, Whetstone, Buttons, jpeg, uStopWatch;

const
  ARRAY_SIZE = 1000000;
  REPEAT_COUNT = 20;

type
  TBigArray = array[0..ARRAY_SIZE] of Integer;

  TfrmWhetBurn = class(TForm)
    GaugeProgress: TGauge;
    btnBurn: TButton;
    edBandwidth: TEdit;
    edTotalTime: TEdit;
    chartTestTimes: TChart;
    Series1: TLineSeries;
    lblIterations: TLabel;
    cboIterations: TComboBox;
    cboxGraphRealTime: TCheckBox;
    edMinMax: TEdit;
    btnScale: TButton;
    lblN: TLabel;
    cboN: TComboBox;
    rbFib: TRadioButton;
    rbWhetstone: TRadioButton;
    rbPi: TRadioButton;
    BitBtn1: TBitBtn;
    btnSaveGraph: TButton;
    btnSaveData: TButton;
    Panel1: TPanel;
    btnPrint: TButton;
    SaveDialog1: TSaveDialog;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnBurnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnScaleClick(Sender: TObject);
    procedure rbWhetstoneClick(Sender: TObject);
    procedure cboIterationsChange(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure BitBtn1Click(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure btnSaveGraphClick(Sender: TObject);
    procedure btnSaveDataClick(Sender: TObject);
    procedure cboNChange(Sender: TObject);
  private
    { Private declarations }
    StopWatch: TStopWatch;
    fN : integer;
    fIterations : integer;
    procedure HandleRBChange;
    procedure InterationsChanged;
    procedure EnableDisableButtons( abEnabled : Boolean );
    function ComputePi(NumDigits: Integer): string;
  public
    { Public declarations }
  end;

var
  frmWhetBurn: TfrmWhetBurn;

implementation

{$R *.DFM}

const
  VERSION = 'Version: 0.6';

procedure TfrmWhetBurn.btnBurnClick(Sender: TObject);
// We want to be able to read and write to an array section varying the size
// we are working with starting at 1kB (256 integer elements) and increasing
// by 1kB steps until we reach 500 kB.  At each step we are going to push through
// around 100 MB.  We will determine the number of iterations by "div" our
// section size with 100MB (we should allow this number to be user defined).
// We need to report the report bandwidth rate at each step.
var
  i                : integer;
  li_RepeatCount   : integer;
  le_Time          : Extended;
  ld_AvgTime       : Double;
  lb_GraphNow      : Boolean;
  l_Results        : Array[1..500] of double;
  lsPi       : string;
  lWhetstone : TWhetstone;
  ld_MaxTime : double;
  ld_MinTime : double;
  procedure Initialize;
  begin
    edBandwidth.Text := 'Testing...';
    ld_MaxTime := -1;
    ld_MinTime := 100000000;
    Refresh;
    EnableDisableButtons( FALSE );
    le_Time := 0;
    chartTestTimes.SeriesList[0].Clear;
    chartTestTimes.LeftAxis.AutomaticMinimum := FALSE;
    chartTestTimes.LeftAxis.AutomaticMaximum := FALSE;
    chartTestTimes.LeftAxis.Minimum := 0;
    li_RepeatCount := StrToInt( Trim( cboIterations.Text ) );
    fN := StrToInt( Trim( cboN.Text ) );
    GaugeProgress.MaxValue := li_RepeatCount;
    GaugeProgress.Progress := 0;
    lb_GraphNow := (cboxGraphRealTime.State = cbChecked);
    Application.ProcessMessages;
  end; // procedure Initialize;
  procedure RunTest;
  begin
    StopWatch.StartTimer;
    if rbWhetstone.Checked then begin
      lWhetstone.ShowStatus := FALSE;
      lWhetstone.StopWatch := StopWatch;
      lWhetstone.NumberOfRuns := fN;
      lWhetstone.Run;
    end else if rbFib.Checked then begin
      Fibonacci( fN );
    end else if rbPi.Checked then begin
      lsPi := ComputePi(fN);
    end; // if
    StopWatch.StopTimer;
    le_Time := le_Time + StopWatch.ElapsedTime;
  end; // procedure RunTest;
  procedure UpdateGraph;
  begin
    GaugeProgress.Progress := GaugeProgress.Progress + 1;
    if StopWatch.ElapsedTime > ld_MaxTime then begin
      ld_MaxTime := 1.05 * StopWatch.ElapsedTime;
      chartTestTimes.LeftAxis.Maximum := ld_MaxTime;
      chartTestTimes.LeftAxis.Increment := (ld_MaxTime - chartTestTimes.LeftAxis.Minimum) / 10;
    end else if StopWatch.ElapsedTime < ld_MinTime then begin
      ld_MinTime := 0.95 * StopWatch.ElapsedTime;
      chartTestTimes.LeftAxis.Minimum := ld_MinTime;
      chartTestTimes.LeftAxis.Increment := (chartTestTimes.LeftAxis.Maximum - ld_MinTime) / 10;
    end;
    chartTestTimes.SeriesList[0].Add(StopWatch.ElapsedTime, '', clRed);
    edMinMax.Text := 'Max = ' +
          FloatToStrF(chartTestTimes.SeriesList[0].MaxYValue, ffNumber, 10, 5) +
          ' s, Min = ' +
          FloatToStrF(chartTestTimes.SeriesList[0].MinYValue, ffNumber, 10, 5)
          + ' s';
    edTotalTime.Text := 'Total time ' + FloatToStrF((le_Time), ffNumber, 6, 6) + ' s';
    Application.ProcessMessages;
  end; // procedure UpdateGraph;
  procedure OutputResults;
  var
    i : integer;
  begin
    GaugeProgress.Progress := GaugeProgress.MaxValue;
    edTotalTime.Text := 'Total time ' + FloatToStrF((le_Time), ffNumber, 6, 6) + ' s';
    ld_AvgTime := (le_time ) / (li_RepeatCount);
    edBandwidth.Text := 'Avg time: ' +
                        FloatToStrF( ld_AvgTime, ffNumber, 10, 3) + ' s';
    if not lb_GraphNow then begin
      for i := 1 to li_RepeatCount do begin
        chartTestTimes.SeriesList[0].Add(l_Results[ i ], '', clRed);
      end;
      edMinMax.Text := 'Max = ' +
                     FloatToStrF(chartTestTimes.SeriesList[0].MaxYValue, ffNumber, 10, 5)
                     + ' s, Min = ' +
                     FloatToStrF(chartTestTimes.SeriesList[0].MinYValue, ffNumber, 10, 5)
                     + ' s';
    end; // if
  end; // procedure OutputResults
begin
  Initialize;
  lWhetstone := TWhetStone.Create;
  try
    StopWatch.Resolution := swresHigh;
    for i := 1 to li_RepeatCount do
    begin
      RunTest;
      if lb_GraphNow then begin
        UpdateGraph;
      end else begin
        l_Results[ i ] := StopWatch.ElapsedTime;
      end;
    end;
  finally
    freeAndNil( lWhetstone );
  end; // try
  OutputResults;
  EnableDisableButtons( TRUE );
end;

procedure TfrmWhetBurn.EnableDisableButtons( abEnabled : Boolean );
begin
  btnBurn.Enabled := abEnabled;
  btnScale.Enabled := abEnabled;
  btnSaveGraph.Enabled := abEnabled;
  btnSaveData.Enabled := abEnabled;
  btnPrint.Enabled := abEnabled;
end; // procedure TfrmWhetBurn.EnableDisableButtons

procedure TfrmWhetBurn.FormCreate(Sender: TObject);
begin
  StopWatch := TStopWatch.Create;
  cboIterations.ItemIndex := 4;
  cboN.ItemIndex := 13;
  HandleRBChange;
  InterationsChanged;
  Caption := Caption + ', ' + VERSION;
end;

procedure TfrmWhetBurn.FormDestroy(Sender: TObject);
begin
  StopWatch.Free;
end;

procedure TfrmWhetBurn.btnScaleClick(Sender: TObject);
begin
  if chartTestTimes.LeftAxis.Automatic then begin
    chartTestTimes.LeftAxis.Automatic := FALSE;
    chartTestTimes.LeftAxis.Minimum := 0;
  end else begin
    chartTestTimes.LeftAxis.Automatic := TRUE;
  end;
end;

function TfrmWhetBurn.ComputePi(NumDigits: Integer): string;
var
  A: array of LongInt;
  I, J, K, P, Q, X, Nines, Predigit: Integer;
  PiLength: Integer;
begin
  SetLength(A, 10*NumDigits div 3);
  SetLength(Result, NumDigits+1);
  PiLength := 1;
  for I := Low(A) to High(A) do
    A[I] := 2;
  Nines := 0;
  Predigit := 0;
  for J := 0 to NumDigits-1 do
  begin
    Q := 0;
    P := 2 * High(A) + 1;
    for I := High(A) downto Low(A) do
    begin
      X := 10*A[I] + Q*(I+1);
      A[I] := X mod P;
      Q := X div P;
      P := P - 2;
    end;
    A[Low(A)] := Q mod 10;
    Q := Q div 10;
    if Q = 9 then
      Inc(Nines)
    else if Q = 10 then
    begin
      Result[PiLength] := Chr(Predigit + 1 + Ord('0'));
      for K := 1 to Nines do
        Result[PiLength+K] := '0';
      PiLength := PiLength + Nines + 1;
      Predigit := 0;
      Nines := 0;
    end
    else
    begin
      Result[PiLength] := Chr(Predigit + Ord('0'));
      Predigit := Q;
      for K := 1 to Nines do
        Result[PiLength+K] := '9';
      PiLength := PiLength + Nines + 1;
      Nines := 0;
    end;
  end;
  Result[PiLength] := Chr(Predigit + Ord('0'));
end;

procedure TfrmWhetBurn.HandleRBChange;
begin
  if rbWhetstone.Checked then begin
    cboN.Items.Clear;
    cboN.Items.Add( '5000' );
    cboN.Items.Add( '10000' );
    cboN.Items.Add( '20000' );
    cboN.Items.Add( '30000' );
    cboN.Items.Add( '40000' );
    cboN.Items.Add( '50000' );
    cboN.Items.Add( '100000' );
    cboN.Items.Add( '200000' );
    cboN.Items.Add( '500000' );
    cboN.Text := '50000';
    chartTestTimes.Title.Text.Clear;
    chartTestTimes.Title.Text.Add( 'Whetstone x 50,000 runs (in seconds) versus Iterations' );
  end else if rbFib.Checked then begin
    cboN.Items.Clear;
    cboN.Items.Add( '20' );
    cboN.Items.Add( '25' );
    cboN.Items.Add( '30' );
    cboN.Items.Add( '35' );
    cboN.Items.Add( '40' );
    cboN.Items.Add( '45' );
    cboN.Items.Add( '50' );
    cboN.Items.Add( '60' );
    cboN.Items.Add( '70' );
    cboN.Items.Add( '80' );
    cboN.Items.Add( '90' );
    cboN.Text := '40';
    chartTestTimes.Title.Text.Clear;
    chartTestTimes.Title.Text.Add( 'Fibonacci( 40 ) (in seconds) versus Iterations' );
  end else if rbPi.Checked then begin
    cboN.Items.Clear;
    cboN.Items.Add( '1000' );
    cboN.Items.Add( '2000' );
    cboN.Items.Add( '5000' );
    cboN.Items.Add( '10000' );
    cboN.Items.Add( '20000' );
    cboN.Items.Add( '50000' );
    cboN.Text := '5000';
    chartTestTimes.Title.Text.Clear;
    chartTestTimes.Title.Text.Add( 'Calculate Pi to the 5,000 digits (in seconds) versus Iterations' );
  end;
end;

procedure TfrmWhetBurn.rbWhetstoneClick(Sender: TObject);
begin
  HandleRBChange;
end;

procedure TfrmWhetBurn.cboIterationsChange(Sender: TObject);
begin
  InterationsChanged;
end;

procedure TfrmWhetBurn.InterationsChanged;
begin
  fIterations := StrToInt( Trim( cboIterations.Text ) );
  chartTestTimes.BottomAxis.AutomaticMaximum := FALSE;
  chartTestTimes.BottomAxis.Maximum := fIterations - 1;
end;


procedure TfrmWhetBurn.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = KEY_ESC then halt;
end;

procedure TfrmWhetBurn.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmWhetBurn.btnPrintClick(Sender: TObject);
begin
  chartTestTimes.Print;
end;

procedure TfrmWhetBurn.btnSaveGraphClick(Sender: TObject);
begin
  SaveDialog1.Filter := 'Bitmap files (*.bmp)|*.BMP';
  if SaveDialog1.Execute then begin
    chartTestTimes.SaveToBitmapFile(SaveDialog1.FileName);
  end; // if
end;

procedure TfrmWhetBurn.btnSaveDataClick(Sender: TObject);
var
  ltxtfileOut   : TextFile;
  lsFileName    : string;
  lsOutLine     : string;
  x             : double;
  i             : integer;
  lSeries       : TChartSeries;
begin
  lSeries := chartTestTimes.Series[0];
  SaveDialog1.Filter := 'text files (*.txt)|*.TXT';
  SaveDialog1.DefaultExt := '.txt';
  SaveDialog1.FileName := 'WhetBurn.txt';
  if SaveDialog1.Execute then begin
    AssignFile( ltxtfileOut, SaveDialog1.FileName );
    Rewrite( ltxtfileOut );
    for i := 0 to lSeries.Count - 1 do begin
      x := StrToFloat( lSeries.XLabel[ i ] );
      lsOutLine := Format( '%6n' + Chr(9) + '   %11n', [ x, lSeries.YValue[ i ] ] );
      WriteLn( ltxtfileOut, lsOutline);
    end; // for
  end; // if
end;

procedure TfrmWhetBurn.cboNChange(Sender: TObject);
var
  ldN : double;
begin
  fN := StrToInt( cboN.Text );
  ldN := fN;
  if rbFib.Checked then begin
    chartTestTimes.Title.Text.Clear;
    chartTestTimes.Title.Text.Add(
      Format( 'Fibonacci( %3.0n ) (in seconds) versus Iterations',
             [ ldN ] ) );
  end else if rbWhetstone.Checked then begin
    chartTestTimes.Title.Text.Clear;
    chartTestTimes.Title.Text.Clear;
    chartTestTimes.Title.Text.Add(
      Format( 'Whetstone x %4.0n runs (in seconds) versus Iterations',
              [ ldN ] ) );
  end else if rbPi.Checked then begin
    chartTestTimes.Title.Text.Clear;
    chartTestTimes.Title.Text.Add(
      Format( 'Calculate Pi to the %4.0n digits (in seconds) versus Iterations',
              [ ldN ] ) );
  end; // if
end;

procedure TfrmWhetBurn.FormActivate(Sender: TObject);
begin
  UnFadeFast( self );
  AlphaBlend := FALSE;
end;

procedure TfrmWhetBurn.FormDeactivate(Sender: TObject);
begin
  FadeFast50( self );
end;

procedure TfrmWhetBurn.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FadeFast100( self );
end;

end.
