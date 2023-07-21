unit ufrmMSR;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, COSBI_Common, ComCtrls, Grids, uMsr, Buttons, ExtCtrls,
  Gauges, CosbiCpuid, TeeProcs, TeEngine, Chart, WbemScripting_TLB, ActiveDs_TLB,
  ActiveX, Series;

type
  TfrmMsrTest = class(TForm)
    MemoOutput: TMemo;
    CheckBoxStayOnTop: TCheckBox;
    TrackBarAlpha: TTrackBar;
    LabeledEditMSR: TLabeledEdit;
    LabeledEdit1: TLabeledEdit;
    SpeedButton1: TSpeedButton;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    Gauge1: TGauge;
    Button2: TButton;
    ChartClockSpeed: TChart;
    TimerCharts: TTimer;
    Series1: TLineSeries;
    procedure TimerChartsTimer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton1Click(Sender: TObject);
    procedure TrackBarAlphaChange(Sender: TObject);
    procedure CheckBoxStayOnTopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fCpuidTransmetaPerformanceData : TCpuidTransmetaPerformanceData;
    fCpuidCentaurPerformanceInfo   : TCpuidCentaurPerformanceInfo;
    fIsC7 : Boolean;
    fMaxClockSpeedAxis      : integer;
    fMsrDriver : TMsrDriver;
    fMsrRegister : T64bitRegister;
    fiElapsedTime : integer;
    //fSeries                 : TChartSeries;
    // wmi stuff
    fLocator: TSWbemLocator;
    fSinkClasses: TSWbemSink;
    fServices   : ISWbemServices;
    fObjectSet  : ISWbemObjectSet;
    fSObject    : ISWbemObject;
    fPropSet    : ISWbemPropertySet;
    fSProp      : ISWbemProperty;
    fPropEnum    : IEnumVariant;
    fEnum        : IEnumVariant;
    ftempObj     : OleVariant;
    fTicks       : integer;
    // end wmi
    function GetC7ClockSpeed : integer;
    function GetCpuMhz: integer;
    procedure InitializeChart;
    procedure InitializeFieldVariables;
    procedure OutputMinPState;
  public
    { Public declarations }
  end;

var
  frmMsrTest: TfrmMsrTest;

implementation

{$R *.dfm}

const
  WINDOW_SIZE = 60;

procedure TfrmMsrTest.FormCreate(Sender: TObject);
var
  MsrRegister : T64bitRegister;
  CpuMultiplier : integer;
begin
  InitializeFieldVariables;
  fMsrDriver := TMsrDriver.Create;
  MemoOutput.Lines.Add('Attempting to open backdoor...');
  fMsrDriver.OpenDriver;
  try
    MsrRegister := fMsrDriver.ReadMsr( 0, $10 );
    MemoOutput.Lines.Add('Backdoor opened successfully.');
  except
    ShowMessage('Backdoor failed to open');
    abort;
  end; // try
  TrackBarAlpha.Position := AlphaBlendValue;
  if not CpuidIsCentaurC7 then begin
    fIsC7 := FALSE;
    fLocator := TSWbemLocator.Create( self );
    fSinkClasses := TSWbemSink.Create( self );
    fServices := fLocator.ConnectServer('', 'root\CIMV2', '', '', '', '', 0, nil);
  end else begin
    fIsC7 := TRUE;
  end;
  InitializeChart;
  //TimerCharts.Enabled := TRUE;
end;

procedure TfrmMsrTest.OutputMinPState;
var
  low, high, liLowMultiplier, liHighMultiplier : integer;
  ldLowVcc, ldHighVcc : double;
begin
end; //procedure TfrmMsrTest.OutputMinPState;

procedure TfrmMsrTest.CheckBoxStayOnTopClick(Sender: TObject);
begin
  if CheckBoxStayOnTop.Checked then begin
    self.FormStyle := fsStayOnTop;
  end else begin
    self.FormStyle := fsNormal;
  end;
end;

procedure TfrmMsrTest.TrackBarAlphaChange(Sender: TObject);
begin
  AlphaBlendValue := TrackBarAlpha.Position;
end;

procedure TfrmMsrTest.SpeedButton1Click(Sender: TObject);
var
  liMSR : integer;
begin
  liMSR := StrToInt( LabeledEditMSR.Text );
  fMsrRegister := fMsrDriver.ReadMsr( 0, liMSR );
  MemoOutput.Lines.Add( IntToHex( fMsrRegister.AsInt64, 16 ) );
  MemoOutput.Lines.Add( fMsrRegister.str );
end;

procedure TfrmMsrTest.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fMsrDriver.CloseDriver;
  FreeAndNil( fMsrDriver );
  if not fIsC7 then begin
    fLocator.Disconnect;
  end;
end;

procedure TfrmMsrTest.Button1Click(Sender: TObject);
var
  lSpeedStepState : TSpeedStepState;
  lMinMaxPStates : TMinMaxSpeedStepStates;
begin
  lSpeedStepState := fMsrDriver.GetSpeedStepState( 0 );
  MemoOutput.Lines.Add( 'FSB Multiplier = ' + IntToStr( lSpeedStepState.FsbMultiplier ) );
  MemoOutput.Lines.Add( 'Vcc = ' + FloatToStr( lSpeedStepState.Vcc ) );
  fMsrDriver.SetSpeedStepState( 0,lSpeedStepState );
  lMinMaxPStates := fMsrDriver.GetMinMaxSpeedStepStates( 0 );
  MemoOutput.Lines.Add( 'Min FSB Multiplier = ' + IntToStr( lMinMaxPStates.MinState.FsbMultiplier ) );
  MemoOutput.Lines.Add( 'Min Vcc = ' + FloatToStr( lMinMaxPStates.MinState.Vcc ) );
  MemoOutput.Lines.Add( 'Max FSB Multiplier = ' + IntToStr( lMinMaxPStates.MaxState.FsbMultiplier ) );
  MemoOutput.Lines.Add( 'Max Vcc = ' + FloatToStr( lMinMaxPStates.MaxState.Vcc ) );
end;

procedure TfrmMsrTest.Button2Click(Sender: TObject);
begin
  MemoOutput.Lines.Add( 'Max Vcc = ' + FloatToStr( fMsrDriver.GetMaxVcc( 0 ) ) );
  MemoOutput.Lines.Add( 'Max CPU Multiplier = ' + FloatToStr( fMsrDriver.GetMaxCpuMultiplier( 0 ) ) );
end;

procedure TfrmMsrTest.TimerChartsTimer(Sender: TObject);
var
  li64Ticks    : int64;
  liClockSpeed : integer;
  ldElapsedTime : double;
  procedure CheckChartScroll;
  begin
    fTicks := fTicks + 1;
    if (fTicks mod WINDOW_SIZE = 0) and (fTicks <> 0) then begin
      Application.ProcessMessages;
      chartClockSpeed.BottomAxis.Scroll( WINDOW_SIZE, FALSE );
      Application.ProcessMessages;
    end;
  end; // procedure CheckScroll;
begin
  if not fIsC7 then begin
    try
      liClockSpeed := GetCpuMhz;
    except
      TimerCharts.Enabled := FALSE;
      MemoOutput.Lines.Add( 'An error occurred while trying to get the clock speed!' );
    end;
  end else begin
    liClockSpeed := GetC7ClockSpeed;
    //CpuidGetC7Temperature;
  end;
  ChartClockSpeed.SeriesList[ 0 ].Add( liClockSpeed, '', clRed);
  ChartClockSpeed.Title.Text.Add( IntToStr( liClockSpeed ) + ' MHz' );
  Application.ProcessMessages;
  CheckChartScroll;
      TimerCharts.Enabled := FALSE;
//  if ckbxAuto.Checked then begin
//    if fIsTransmeta then begin
//      ldClockSpeed := GetTransmetaClockSpeed;
//      fiElapsedTime := fiElapsedTime + ( tmrSpeedUpdate.Interval DIV 1000);
//      fSeries.Add(ldClockSpeed, '', clRed);
//      lblClockspeed.Caption := FloatToStrF( ldClockSpeed, ffNumber, 11, 0 ) +
//                 ' MHz';
//      CheckChartScroll;
//      fiElapsedTime := inc( fiElapsedTime );
//    end else begin
//      if fi64_StartTickTmr <> 0 then begin
//        ldElapsedTime := StopWatch.StopTimer;
//        fi64_StopTickTmr := GetCycleCount;
//        ldClockSpeed := ( fi64_StopTickTmr - fi64_StartTickTmr ) / ldElapsedTime;
//        lblClockspeed.Caption := FloatToStrF( ldClockSpeed, ffNumber, 11, 0 ) +
//                 ' Hz';
//        fiElapsedTime := fiElapsedTime + round( ldElapsedTime );
//        fSeries.Add( round( ldClockSpeed / ONE_MILLION ), '' , clRed);
//        CheckChartScroll;
//        fi64_StartTickTmr := GetCycleCount;
//        StopWatch.StartTimer;
//      end else begin
//        lblClockspeed.Caption := 'Updating...';
//        fi64_StartTickTmr := GetCycleCount;
//        StopWatch.StartTimer;
//      end;
//    end; // if
//    if fIsC7 or fIsTransmeta then begin
//      if fMaxClockSpeedAxis < ( ldClockSpeed ) then begin
//        fMaxClockSpeedAxis := round( 1.1 * ( ldClockSpeed ) );
//        fMaxClockSpeed := round( chartClockSpeed.SeriesList.Series[0].MaxYValue );
//        chartClockSpeed.LeftAxis.Maximum := fMaxClockSpeedAxis;
//        chartClockSpeed.LeftAxis.Minimum := 0;
//        chartClockSpeed.MaxPointsPerPage := WINDOW_SIZE;
//        chartClockSpeed.BottomAxis.Increment := 5;
//      end; // if
//    end else if fMaxClockSpeedAxis < ( ldClockSpeed / ONE_MILLION ) then begin
//      fMaxClockSpeedAxis := round( 1.1 * ( ldClockSpeed / ONE_MILLION ) );
//      fMaxClockSpeed := round( chartClockSpeed.SeriesList.Series[0].MaxYValue );
//      chartClockSpeed.LeftAxis.Maximum := fMaxClockSpeedAxis;
//      chartClockSpeed.LeftAxis.Minimum := 0;
//      chartClockSpeed.MaxPointsPerPage := WINDOW_SIZE;
//      chartClockSpeed.BottomAxis.Increment := 5;
//    end;
//    UpdateCPUSpeed;
//  end; // if ckbxAuto.Checked then begin
end;

function TfrmMsrTest.GetC7ClockSpeed : integer;
begin
  fCpuidCentaurPerformanceInfo.Initialize;
  result := round( fCpuidCentaurPerformanceInfo.CurrentClockMultiplier *
            fCpuidCentaurPerformanceInfo.FrontSideBusClock );
end; // function TfrmCPUSpeed.GetTransmetaClockSpeed

function TfrmMsrTest.GetCpuMhz: integer;
var
  i, Value     : Cardinal;
begin
  fObjectSet := fServices.ExecQuery('SELECT CurrentClockSpeed FROM Win32_Processor', 'WQL', wbemFlagReturnImmediately, nil);
  fEnum :=  ( fObjectSet._NewEnum ) as IEnumVariant;
  // go to the first object (cpu); must pass Value for reference:
  if (fEnum.Next( 1, ftempObj, Value ) = S_OK) then begin
    fSObject := IUnknown( ftempObj ) as SWBemObject;
    fPropSet := fSObject.Properties_;
    fpropEnum := ( fPropSet._NewEnum ) as IEnumVariant;
    // goto the first property (should be "CurrentClockSpeed"):
    if ( fpropEnum.Next(1, ftempObj, Value ) = S_OK ) then begin
      fSProp := IUnknown(ftempObj) as SWBemProperty;
      if fSProp.Name = 'CurrentClockSpeed' then begin
        result := fSProp.Get_Value;
      end; // if
    end; // if
  end; // if
end; //function TfrmMsrTest.GetCpuMhz: integer;

procedure TfrmMsrTest.InitializeChart;
begin
//  if not assigned( Series1 ) then begin
//    Series1 := chartClockSpeed.SeriesList.Series[0];
//  end; // if
  Series1.Clear;
  chartClockSpeed.LeftAxis.Maximum := 3500; // fMaxClockSpeedAxis;
  chartClockSpeed.LeftAxis.Minimum := 0;
  chartClockSpeed.MaxPointsPerPage := WINDOW_SIZE;
  chartClockSpeed.BottomAxis.Increment := 10;
end; //procedure TfrmMsrTest.InitializeChart;

procedure TfrmMsrTest.InitializeFieldVariables;
begin
  fIsC7 := FALSE;
//  fMaxClockSpeed := 0;
  fTicks := -1;
//  fi64_StartTickTmr := 0;
  fiElapsedTime := 0;
//  fCpuVendor := DetectCpuVendor;
//  fLogArrayIndex := 0;
//  case fCpuVendor of
//  eIntel : lsCpuVendor := 'Intel';
//  eAMD : lsCpuVendor := 'AMD';
//  eVIA : lsCpuVendor := 'VIA';
//  eTransmeta : lsCpuVendor := 'Transmeta';
//  end; // case
//  if not assigned( StopWatch ) then StopWatch := TStopWatch.Create;
//  StopWatch.Resolution := swresHigh;
//  if not assigned( StopWatchLog ) then StopWatchLog := TStopWatch.Create;
//  StopWatchLog.Resolution := swresHigh;
//  if fIsTransmeta then begin
//    fCpuidTransmetaPerformanceData := TCpuidTransmetaPerformanceData.Create;
//  end else if fIsC7 then begin
//    fCpuidCentaurPerformanceInfo := TCpuidCentaurPerformanceInfo.Create;
//    lsCpuVendor := lsCpuVendor + ' C7';
//  end;
//  StatusBar0Update( lsCpuVendor + ' CPU detected.' );
end; // procedure TfrmCPUSpeed.InitializeFieldVariables;


end.
