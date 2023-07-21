unit uCPUSpeed;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  COSBI_Common, StdCtrls, ExtCtrls, ComCtrls, TeEngine, Series, TeeProcs,
  Chart, uCOSBI_SystemInfo, CosbiLogFile, CosbiCpuid, jpeg, Menus,
  uCpuClockSpeed, uCpuSpeedMsr, uMsr, uStopWatch;

type
  enumPanels = ( eGraph, eGetCpuSpeed, eViaRatio );
  setPanels = set of enumPanels;

  TfrmCPUSpeed = class(TForm)
    lblClockspeed: TLabel;
    chartClockSpeed: TChart;
    StatusBar1: TStatusBar;
    SaveDialog1: TSaveDialog;
    Series1: TLineSeries;
    btnScrollLeft: TButton;
    btnScrollRight: TButton;
    btnSave: TButton;
    btnSaveGraph: TButton;
    ChartTemperature: TChart;
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    Button5: TButton;
    LineSeries1: TLineSeries;
    MainMenuCpuSpeed: TMainMenu;
    miFile: TMenuItem;
    miExit: TMenuItem;
    miSettings: TMenuItem;
    miMSR: TMenuItem;
    miCpuRatio: TMenuItem;
    miTools: TMenuItem;
    miCPUID: TMenuItem;
    miManualCpuFrequency: TMenuItem;
    Splitter1: TSplitter;
    miLoadDownCpu: TMenuItem;
    miRDTSC: TMenuItem;
    miWMI: TMenuItem;
    miMsrCalc: TMenuItem;
    mi4x: TMenuItem;
    mi5x: TMenuItem;
    mi6x: TMenuItem;
    mi7x: TMenuItem;
    mi8x: TMenuItem;
    mi9x: TMenuItem;
    mi10x: TMenuItem;
    mi11x: TMenuItem;
    mi12x: TMenuItem;
    mi13x: TMenuItem;
    mi14x: TMenuItem;
    mi15x: TMenuItem;
    mi16x: TMenuItem;
    mi17x: TMenuItem;
    mi18x: TMenuItem;
    mi19x: TMenuItem;
    mi20x: TMenuItem;
    mi21x: TMenuItem;
    mi22x: TMenuItem;
    mi23x: TMenuItem;
    mi24x: TMenuItem;
    mi25x: TMenuItem;
    miVcc: TMenuItem;
    miVcc0: TMenuItem;
    miVcc2: TMenuItem;
    miVcc3: TMenuItem;
    miVcc4: TMenuItem;
    miVcc5: TMenuItem;
    miVcc6: TMenuItem;
    miVcc7: TMenuItem;
    miVcc8: TMenuItem;
    miVcc9: TMenuItem;
    miVcc10: TMenuItem;
    miVcc11: TMenuItem;
    miVcc12: TMenuItem;
    miVcc13: TMenuItem;
    miVcc14: TMenuItem;
    miVcc15: TMenuItem;
    miAllowOverclock: TMenuItem;
    miVcc1: TMenuItem;
    miVcc16: TMenuItem;
    miVcc17: TMenuItem;
    miVcc18: TMenuItem;
    miVcc19: TMenuItem;
    miVcc20: TMenuItem;
    miVcc21: TMenuItem;
    miVcc22: TMenuItem;
    miVcc23: TMenuItem;
    miVcc24: TMenuItem;
    miVcc25: TMenuItem;
    miVcc26: TMenuItem;
    miVcc27: TMenuItem;
    miVcc28: TMenuItem;
    miVcc29: TMenuItem;
    miVcc30: TMenuItem;
    miVcc31: TMenuItem;
    miVcc32: TMenuItem;
    miVcc33: TMenuItem;
    miVcc34: TMenuItem;
    miVcc35: TMenuItem;
    miLogging: TMenuItem;
    miVcc39: TMenuItem;
    miVcc38: TMenuItem;
    miVcc36: TMenuItem;
    miVcc37: TMenuItem;
    miVcc40: TMenuItem;
    miVcc41: TMenuItem;
    miVcc42: TMenuItem;
    miVcc43: TMenuItem;
    miVcc44: TMenuItem;
    miVcc45: TMenuItem;
    miVcc46: TMenuItem;
    miVcc47: TMenuItem;
    miVcc48: TMenuItem;
    procedure miVcc48Click(Sender: TObject);
    procedure miVcc47Click(Sender: TObject);
    procedure miVcc46Click(Sender: TObject);
    procedure miVcc45Click(Sender: TObject);
    procedure miVcc44Click(Sender: TObject);
    procedure miVcc43Click(Sender: TObject);
    procedure miVcc42Click(Sender: TObject);
    procedure miVcc41Click(Sender: TObject);
    procedure miVcc40Click(Sender: TObject);
    procedure miVcc39Click(Sender: TObject);
    procedure miVcc38Click(Sender: TObject);
    procedure miVcc37Click(Sender: TObject);
    procedure miVcc36Click(Sender: TObject);
    procedure miVcc35Click(Sender: TObject);
    procedure miVcc34Click(Sender: TObject);
    procedure miVcc33Click(Sender: TObject);
    procedure miVcc32Click(Sender: TObject);
    procedure miVcc31Click(Sender: TObject);
    procedure miVcc30Click(Sender: TObject);
    procedure miVcc29Click(Sender: TObject);
    procedure miVcc28Click(Sender: TObject);
    procedure miVcc27Click(Sender: TObject);
    procedure miVcc26Click(Sender: TObject);
    procedure miVcc25Click(Sender: TObject);
    procedure miVcc24Click(Sender: TObject);
    procedure miVcc23Click(Sender: TObject);
    procedure miVcc22Click(Sender: TObject);
    procedure miVcc21Click(Sender: TObject);
    procedure miVcc20Click(Sender: TObject);
    procedure miVcc19Click(Sender: TObject);
    procedure miVcc18Click(Sender: TObject);
    procedure miVcc17Click(Sender: TObject);
    procedure miVcc16Click(Sender: TObject);
    procedure miVcc15Click(Sender: TObject);
    procedure miVcc14Click(Sender: TObject);
    procedure miVcc13Click(Sender: TObject);
    procedure miVcc12Click(Sender: TObject);
    procedure miVcc11Click(Sender: TObject);
    procedure miVcc10Click(Sender: TObject);
    procedure miVcc9Click(Sender: TObject);
    procedure miVcc8Click(Sender: TObject);
    procedure miVcc7Click(Sender: TObject);
    procedure miVcc6Click(Sender: TObject);
    procedure miVcc5Click(Sender: TObject);
    procedure miVcc4Click(Sender: TObject);
    procedure miVcc3Click(Sender: TObject);
    procedure miVcc2Click(Sender: TObject);
    procedure miVcc1Click(Sender: TObject);
    procedure miVcc0Click(Sender: TObject);
    procedure mi25xClick(Sender: TObject);
    procedure mi24xClick(Sender: TObject);
    procedure mi23xClick(Sender: TObject);
    procedure mi22xClick(Sender: TObject);
    procedure mi21xClick(Sender: TObject);
    procedure mi20xClick(Sender: TObject);
    procedure mi19xClick(Sender: TObject);
    procedure mi18xClick(Sender: TObject);
    procedure mi17xClick(Sender: TObject);
    procedure mi16xClick(Sender: TObject);
    procedure mi15xClick(Sender: TObject);
    procedure mi14xClick(Sender: TObject);
    procedure mi13xClick(Sender: TObject);
    procedure mi12xClick(Sender: TObject);
    procedure mi11xClick(Sender: TObject);
    procedure mi10xClick(Sender: TObject);
    procedure mi9xClick(Sender: TObject);
    procedure mi8xClick(Sender: TObject);
    procedure mi7xClick(Sender: TObject);
    procedure mi6xClick(Sender: TObject);
    procedure mi5xClick(Sender: TObject);
    procedure mi4xClick(Sender: TObject);
    procedure miAllowOverclockClick(Sender: TObject);
    procedure miMsrCalcClick(Sender: TObject);
    procedure miWMIClick(Sender: TObject);
    procedure miRDTSCClick(Sender: TObject);
    procedure miCPUIDClick(Sender: TObject);
    procedure miMSRClick(Sender: TObject);
    procedure miLoadDownCpuClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure btnSaveGraphClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnScrollLeftClick(Sender: TObject);
    procedure btnScrollRightClick(Sender: TObject);
    procedure AppOnMinimize(Sender: TObject);
    procedure AppOnRestore(Sender: TObject);
  private
    { Private declarations }
    fiChartSampleTime       : integer;
    fi64_CPU_Ticks_per_sec  : int64;
    fi64_StartTick          : int64;
    fi64_StopTick           : int64;
    fi64_StartTickTmr       : int64;
    fi64_StopTickTmr        : int64;
    fiElapsedTime           : integer;
    fdClockRatio            : double;
    fSeries                 : TChartSeries;
    fIsCentaur              : Boolean;
    fIsTransmeta            : Boolean; // Transmeta needs to use a different method
    fIsC7                   : Boolean; // Centaur C7 exposes clock through CPUID
    fIsCn                   : Boolean;
    fIsEnhancedSpeedStepAvailable : Boolean;
    StopWatch               : TStopWatch;
    StopWatchLog            : TStopWatch;
    fTicks                  : integer;
    fMaxClockSpeed          : int64;
    fMaxMHz                 : integer;
    fMaxClockSpeedAxis      : integer;
    fShowPanels             : setPanels;
    fCpuVendor              : enumCpuVendor;
    fNbrOfThreadsRunning    : integer;
    fMsrDriver              : TMsrDriver;
    fMsrRegister            : T64bitRegister;
    ffrmMSR                 : TfrmMSR;
    fMinimized              : Boolean;
    fCaption                : string;
    fMaxC7TempAxis          : double;
    fMaxC7Temperature       : double;
    fCpuClockSpeed          : TCpuClockSpeed;
    fsetCpuSpeedCalcSupport : TCpuClockSpeed.setCpuSpeedCalcSupport;
    fUnknownSpeedStepVoltage : Boolean;
    procedure ParseCommandLine;
    procedure InitializeFieldVariables;
    procedure SetChartSampleTime( Value : integer );
    procedure OnCpuSpeedTimer( Sender : TObject );
    procedure UpdateCharts;
    procedure SetCalculationMethod( Value : TCpuClockSpeed.enumCpuSpeedCalcType );
    procedure InitVccMultMenu;
    procedure SetSpeedStepMenus;
  public
    { Public declarations }
    function ClockRatioSettable : Boolean;
    procedure UpdateCPUSpeed;
    function DetermineMaximumClockSpeed: int64;
    procedure InitializeChart;
    procedure SaveChartData;
    procedure StatusBar0Update( astrMessage : string );
    procedure StatusBar1Update( astrMessage : string );
//    procedure ShowHideCpuSpeedGraph( abVisible : Boolean );
  end;

var
  frmCPUSpeed: TfrmCPUSpeed;

implementation

{$R *.DFM}

const
  VERSION = 'v1.5';
  WINDOW_SIZE = 60;

procedure TfrmCPUSpeed.FormCreate(Sender: TObject);
var
  ldClockSpeed : double;
begin
  fMinimized := FALSE;
  Height := 400;
  fMsrDriver := TMsrDriver.Create;
  fMsrDriver.OpenDriver;
  InitializeFieldVariables;
  //TrackBarAlpha.Position := AlphaBlendValue;
  if ParamCount > 0 then begin
    ParseCommandLine;
  end; // if
  fMaxClockSpeed := DetermineMaximumClockSpeed;
  fMaxMHz := round( fMaxClockSpeed / ONE_MILLION );
  ldClockSpeed := fMaxClockSpeed;
  if fIsC7 or fIsCn or fIsTransmeta then begin
    StatusBar1Update( 'Max CPU speed: ~' +
      FloatToStrF( ldClockSpeed, ffNumber, 11, 0 )  + ' MHz' );
  end else begin
    StatusBar1Update( 'Max CPU speed: ~' +
      FloatToStrF( ldClockSpeed, ffNumber, 11, 0 )  + ' Hz' );
  end; // if
  InitializeChart;
  caption := caption + ' ' + VERSION;
  if CpuidIsCentaurC7 then begin
    chartTemperature.Visible := TRUE;
  end else begin
    chartTemperature.Visible := FALSE;
  end; // if
  // set up the Cpu Speed timer:
  fCpuClockSpeed.Timer.Interval := 1000;
  fCpuClockSpeed.Timer.Enabled := TRUE;
  InitVccMultMenu;
  // set up routines to show clock speed when minimized:
  application.OnMinimize := AppOnMinimize;
  application.OnRestore := AppOnRestore;
end; // procedure TfrmCPUSpeed.FormCreate(Sender: TObject);

procedure TfrmCPUSpeed.InitVccMultMenu;
var
  lMinMaxSpeedStepStates : TMinMaxSpeedStepStates;
  liSetMul : set of 0..25;
  i : integer;
  procedure InitMultiplier;
  var
    liMinMul : integer;
    liMaxMul : integer;
  begin
    //get min and max multipliers
    liMinMul := lMinMaxSpeedStepStates.MinState.FsbMultiplier;
    liMaxMul := lMinMaxSpeedStepStates.MaxState.FsbMultiplier;
    mi4x.Enabled := 4 in [ liMinMul..liMaxMul ];
    mi5x.Enabled := 5 in [ liMinMul..liMaxMul ];
    mi6x.Enabled := 6 in [ liMinMul..liMaxMul ];
    mi7x.Enabled := 7 in [ liMinMul..liMaxMul ];
    mi8x.Enabled := 8 in [ liMinMul..liMaxMul ];
    mi9x.Enabled := 9 in [ liMinMul..liMaxMul ];
    mi10x.Enabled := 10 in [ liMinMul..liMaxMul ];
    mi11x.Enabled := 11 in [ liMinMul..liMaxMul ];
    mi12x.Enabled := 12 in [ liMinMul..liMaxMul ];
    mi13x.Enabled := 13 in [ liMinMul..liMaxMul ];
    mi14x.Enabled := 14 in [ liMinMul..liMaxMul ];
    mi15x.Enabled := 15 in [ liMinMul..liMaxMul ];
    mi16x.Enabled := 16 in [ liMinMul..liMaxMul ];
    mi17x.Enabled := 17 in [ liMinMul..liMaxMul ];
    mi18x.Enabled := 18 in [ liMinMul..liMaxMul ];
    mi19x.Enabled := 19 in [ liMinMul..liMaxMul ];
    mi20x.Enabled := 20 in [ liMinMul..liMaxMul ];
    mi21x.Enabled := 21 in [ liMinMul..liMaxMul ];
    mi22x.Enabled := 22 in [ liMinMul..liMaxMul ];
    mi23x.Enabled := 23 in [ liMinMul..liMaxMul ];
    mi24x.Enabled := 24 in [ liMinMul..liMaxMul ];
    mi25x.Enabled := 25 in [ liMinMul..liMaxMul ];
  end;
  procedure InitVcc;
  var
    ldMinVcc : double;
    ldMaxVcc : double;
    liMinVcc : integer;
    liMaxVcc : integer;
  begin
    ldMinVcc := lMinMaxSpeedStepStates.MinState.Vcc;
    liMinVcc := lMinMaxSpeedStepStates.MinState.VccStep;
    ldMaxVcc := lMinMaxSpeedStepStates.MaxState.Vcc;
    liMaxVcc := lMinMaxSpeedStepStates.MaxState.VccStep;
    miVcc0.Enabled := 0 in [ liMinVcc..liMaxVcc ];
    miVcc1.Enabled := 1 in [ liMinVcc..liMaxVcc ];
    miVcc2.Enabled := 2 in [ liMinVcc..liMaxVcc ];
    miVcc3.Enabled := 3 in [ liMinVcc..liMaxVcc ];
    miVcc4.Enabled := 4 in [ liMinVcc..liMaxVcc ];
    miVcc5.Enabled := 5 in [ liMinVcc..liMaxVcc ];
    miVcc6.Enabled := 6 in [ liMinVcc..liMaxVcc ];
    miVcc7.Enabled := 7 in [ liMinVcc..liMaxVcc ];
    miVcc8.Enabled := 8 in [ liMinVcc..liMaxVcc ];
    miVcc9.Enabled := 9 in [ liMinVcc..liMaxVcc ];
    miVcc10.Enabled := 10 in [ liMinVcc..liMaxVcc ];
    miVcc11.Enabled := 11 in [ liMinVcc..liMaxVcc ];
    miVcc12.Enabled := 12 in [ liMinVcc..liMaxVcc ];
    miVcc13.Enabled := 13 in [ liMinVcc..liMaxVcc ];
    miVcc14.Enabled := 14 in [ liMinVcc..liMaxVcc ];
    miVcc15.Enabled := 15 in [ liMinVcc..liMaxVcc ];
    miVcc16.Enabled := 16 in [ liMinVcc..liMaxVcc ];
    miVcc17.Enabled := 17 in [ liMinVcc..liMaxVcc ];
    miVcc18.Enabled := 18 in [ liMinVcc..liMaxVcc ];
    miVcc19.Enabled := 19 in [ liMinVcc..liMaxVcc ];
    miVcc20.Enabled := 20 in [ liMinVcc..liMaxVcc ];
    miVcc21.Enabled := 21 in [ liMinVcc..liMaxVcc ];
    miVcc22.Enabled := 22 in [ liMinVcc..liMaxVcc ];
    miVcc23.Enabled := 23 in [ liMinVcc..liMaxVcc ];
    miVcc24.Enabled := 24 in [ liMinVcc..liMaxVcc ];
    miVcc25.Enabled := 25 in [ liMinVcc..liMaxVcc ];
    miVcc26.Enabled := 26 in [ liMinVcc..liMaxVcc ];
    miVcc27.Enabled := 27 in [ liMinVcc..liMaxVcc ];
    miVcc28.Enabled := 28 in [ liMinVcc..liMaxVcc ];
    miVcc29.Enabled := 29 in [ liMinVcc..liMaxVcc ];
    miVcc30.Enabled := 30 in [ liMinVcc..liMaxVcc ];
    miVcc31.Enabled := 31 in [ liMinVcc..liMaxVcc ];
    miVcc32.Enabled := 32 in [ liMinVcc..liMaxVcc ];
    miVcc33.Enabled := 33 in [ liMinVcc..liMaxVcc ];
    miVcc34.Enabled := 34 in [ liMinVcc..liMaxVcc ];
    miVcc35.Enabled := 35 in [ liMinVcc..liMaxVcc ];
    miVcc36.Enabled := 36 in [ liMinVcc..liMaxVcc ];
    miVcc37.Enabled := 37 in [ liMinVcc..liMaxVcc ];
    miVcc38.Enabled := 38 in [ liMinVcc..liMaxVcc ];
    miVcc39.Enabled := 39 in [ liMinVcc..liMaxVcc ];
    miVcc40.Enabled := 40 in [ liMinVcc..liMaxVcc ];
    miVcc41.Enabled := 41 in [ liMinVcc..liMaxVcc ];
    miVcc42.Enabled := 42 in [ liMinVcc..liMaxVcc ];
    miVcc43.Enabled := 43 in [ liMinVcc..liMaxVcc ];
    miVcc44.Enabled := 44 in [ liMinVcc..liMaxVcc ];
    miVcc45.Enabled := 45 in [ liMinVcc..liMaxVcc ];
    miVcc46.Enabled := 46 in [ liMinVcc..liMaxVcc ];
    miVcc47.Enabled := 47 in [ liMinVcc..liMaxVcc ];
    miVcc48.Enabled := 48 in [ liMinVcc..liMaxVcc ];
    miVcc0.Caption := FloatToStr( DecodeSpeedStepValue( 0 ) ) + 'V';
    miVcc1.Caption := FloatToStr( DecodeSpeedStepValue( 1 ) ) + 'V';
    miVcc2.Caption := FloatToStr( DecodeSpeedStepValue( 2 ) ) + 'V';
    miVcc3.Caption := FloatToStr( DecodeSpeedStepValue( 3 ) ) + 'V';
    miVcc4.Caption := FloatToStr( DecodeSpeedStepValue( 4 ) ) + 'V';
    miVcc5.Caption := FloatToStr( DecodeSpeedStepValue( 5 ) ) + 'V';
    miVcc6.Caption := FloatToStr( DecodeSpeedStepValue( 6 ) ) + 'V';
    miVcc7.Caption := FloatToStr( DecodeSpeedStepValue( 7 ) ) + 'V';
    miVcc8.Caption := FloatToStr( DecodeSpeedStepValue( 8 ) ) + 'V';
    miVcc9.Caption := FloatToStr( DecodeSpeedStepValue( 9 ) ) + 'V';
    miVcc10.Caption := FloatToStr( DecodeSpeedStepValue( 10 ) ) + 'V';
    miVcc11.Caption := FloatToStr( DecodeSpeedStepValue( 11 ) ) + 'V';
    miVcc12.Caption := FloatToStr( DecodeSpeedStepValue( 12 ) ) + 'V';
    miVcc13.Caption := FloatToStr( DecodeSpeedStepValue( 13 ) ) + 'V';
    miVcc14.Caption := FloatToStr( DecodeSpeedStepValue( 14 ) ) + 'V';
    miVcc15.Caption := FloatToStr( DecodeSpeedStepValue( 15 ) ) + 'V';
    miVcc16.Caption := FloatToStr( DecodeSpeedStepValue( 16 ) ) + 'V';
    miVcc17.Caption := FloatToStr( DecodeSpeedStepValue( 17 ) ) + 'V';
    miVcc18.Caption := FloatToStr( DecodeSpeedStepValue( 18 ) ) + 'V';
    miVcc19.Caption := FloatToStr( DecodeSpeedStepValue( 19 ) ) + 'V';
    miVcc20.Caption := FloatToStr( DecodeSpeedStepValue( 20 ) ) + 'V';
    miVcc21.Caption := FloatToStr( DecodeSpeedStepValue( 21 ) ) + 'V';
    miVcc22.Caption := FloatToStr( DecodeSpeedStepValue( 22 ) ) + 'V';
    miVcc23.Caption := FloatToStr( DecodeSpeedStepValue( 23 ) ) + 'V';
    miVcc24.Caption := FloatToStr( DecodeSpeedStepValue( 24 ) ) + 'V';
    miVcc25.Caption := FloatToStr( DecodeSpeedStepValue( 25 ) ) + 'V';
    miVcc26.Caption := FloatToStr( DecodeSpeedStepValue( 26 ) ) + 'V';
    miVcc27.Caption := FloatToStr( DecodeSpeedStepValue( 27 ) ) + 'V';
    miVcc28.Caption := FloatToStr( DecodeSpeedStepValue( 28 ) ) + 'V';
    miVcc29.Caption := FloatToStr( DecodeSpeedStepValue( 29 ) ) + 'V';
    miVcc30.Caption := FloatToStr( DecodeSpeedStepValue( 30 ) ) + 'V';
    miVcc31.Caption := FloatToStr( DecodeSpeedStepValue( 31 ) ) + 'V';
    miVcc32.Caption := FloatToStr( DecodeSpeedStepValue( 32 ) ) + 'V';
    miVcc33.Caption := FloatToStr( DecodeSpeedStepValue( 33 ) ) + 'V';
    miVcc34.Caption := FloatToStr( DecodeSpeedStepValue( 34 ) ) + 'V';
    miVcc35.Caption := FloatToStr( DecodeSpeedStepValue( 35 ) ) + 'V';
    miVcc36.Caption := FloatToStr( DecodeSpeedStepValue( 36 ) ) + 'V';
    miVcc37.Caption := FloatToStr( DecodeSpeedStepValue( 37 ) ) + 'V';
    miVcc38.Caption := FloatToStr( DecodeSpeedStepValue( 38 ) ) + 'V';
    miVcc39.Caption := FloatToStr( DecodeSpeedStepValue( 39 ) ) + 'V';
    miVcc40.Caption := FloatToStr( DecodeSpeedStepValue( 40 ) ) + 'V';
    miVcc41.Caption := FloatToStr( DecodeSpeedStepValue( 41 ) ) + 'V';
    miVcc42.Caption := FloatToStr( DecodeSpeedStepValue( 42 ) ) + 'V';
    miVcc43.Caption := FloatToStr( DecodeSpeedStepValue( 43 ) ) + 'V';
    miVcc44.Caption := FloatToStr( DecodeSpeedStepValue( 44 ) ) + 'V';
    miVcc45.Caption := FloatToStr( DecodeSpeedStepValue( 45 ) ) + 'V';
    miVcc46.Caption := FloatToStr( DecodeSpeedStepValue( 46 ) ) + 'V';
    miVcc47.Caption := FloatToStr( DecodeSpeedStepValue( 47 ) ) + 'V';
    miVcc48.Caption := FloatToStr( DecodeSpeedStepValue( 48 ) ) + 'V';
  end;
begin
  if CpuidIsEnhancedSpeedStepAvailable then begin
    lMinMaxSpeedStepStates := fMsrDriver.GetMinMaxSpeedStepStates( 0 );
    miCpuRatio.Enabled := TRUE;
    miVcc.Enabled := TRUE;
    liSetMul := [];
    for i := lMinMaxSpeedStepStates.MinState.FsbMultiplier to lMinMaxSpeedStepStates.MaxState.FsbMultiplier do begin
      liSetMul := liSetMul + [i];
    end;
    InitMultiplier;
    InitVcc;
  end else begin
    miCpuRatio.Enabled := FALSE;
    miVcc.Enabled := FALSE;
    miMsrCalc.Enabled := FALSE;
  end; // if
end; // procedure TfrmCPUSpeed.InitVccMultMenu;

procedure TfrmCPUSpeed.ParseCommandLine;
var
  i             : integer;
  lb_LogFile    : Boolean;
  lb_Run        : Boolean;
  le_CpuClockSpeed  : extended;
  li_EndMhz     : integer;
  li_StartMhz   : integer;
  ls_LogFile    : string;
  ls_LogMsg     : string;
begin
  i := 1;
  lb_Run := FALSE;
  while i <= ParamCount do begin
    if LowerCase( ParamStr(i) ) = 'cr' then begin
      i := i + 1;
//      ClockRatio := StrToFloat( ParamStr( i ) );
      lb_Run := TRUE;
    end else if LowerCase( ParamStr(i) ) = 'l' then begin
      i := i + 1;
      lb_LogFile := TRUE;
      Try
        ls_LogFile := ParamStr( i );
      Except
        ls_LogFile := '';
      end;
    end else begin
      ShowMessage(
        'Commandline parameters: cr nn.n = Set VIA clockratio to nn.n;'  +
        CR_LF +
        'l "filename" = log to new file specified by "filename"');
      exit;
    end; // if
    i := i + 1;
  end; // while
  if lb_Run then begin
    StopWatch.SleepTime := 100;
    le_CpuClockSpeed := StopWatch.GetCPUClockspeed( TRUE );
    li_StartMHz := Round( le_CpuClockSpeed / ONE_MILLION );
    // update cpu ratio and get new clockspeed:
//    li_EndMHz := ChangeClockRatio;
//    if lb_LogFile then begin
//      ls_LogMsg  := 'Clock ratio has been changed.' + CR_LF +
//        Format(  'Start MHz = %d, end MHz = %d', [li_StartMHz, li_EndMHz] );
//      CreateLogFile(ls_LogMsg, ls_LogFile);
//    end;
    Application.Terminate;
  end; // if
end; // procedure TfrmCPUSpeed.ParseCommandLine

procedure TfrmCPUSpeed.FormDestroy(Sender: TObject);
begin
  FreeAndNil( fCpuClockSpeed );
  FreeAndNil( StopWatchLog );
  if assigned( ffrmMSR ) then ffrmMSR.Release;
end;

procedure TfrmCPUSpeed.SetCalculationMethod( Value : TCpuClockSpeed.enumCpuSpeedCalcType );
begin
  case Value of
  eRDTSC:
  begin
    miRDTSC.Checked := TRUE;
    fCpuClockSpeed.CpuSpeedCalculationType := eRDTSC;
  end;
  eCPUID:
  begin
    miCPUID.Checked := TRUE;
    fCpuClockSpeed.CpuSpeedCalculationType := eCPUID;
  end;
  eWMI:
  begin
    miWMI.Checked := TRUE;
    fCpuClockSpeed.CpuSpeedCalculationType := eWMI;
  end;
  eMSR:
  begin
    miMsrCalc.Checked := TRUE;
    fCpuClockSpeed.CpuSpeedCalculationType := eMSR;
  end;
  else
    showmessage( 'procedure TfrmCPUSpeed.SetCalculationMethod: unknown calculation type.' );
  end;
end;

procedure TfrmCPUSpeed.UpdateCharts;
var
  li64Ticks    : int64;
  ldClockSpeed : double;
  ldElapsedTime : double;
  ldC5J_Temperature : double;
  procedure CheckChartScroll;
  begin
    fTicks := fTicks + 1;
    if (fTicks mod WINDOW_SIZE = 0) and (fTicks <> 0) then begin
      Application.ProcessMessages;
      chartClockSpeed.BottomAxis.Scroll( WINDOW_SIZE, FALSE );
      Application.ProcessMessages;
      if fIsC7 or fIsCn then begin
        chartTemperature.BottomAxis.Scroll( WINDOW_SIZE, FALSE );
      end;
    end;
  end; // procedure CheckScroll;
begin
  ldClockSpeed := 0;
  ldClockSpeed := fCpuClockSpeed.GetCPUSpeed;
  fiElapsedTime := fiElapsedTime + ( fCpuClockSpeed.Timer.Interval DIV 1000);
  fSeries.Add(ldClockSpeed, '', clLime);
  lblClockspeed.Caption := FloatToStrF( ldClockSpeed, ffNumber, 11, 0 ) + ' MHz';
  if ldClockSpeed > fMaxMHz then begin
    fMaxMHz := round( ldClockSpeed );
    fMaxClockSpeedAxis := round( 1.1 * ( ldClockSpeed ) );
    chartClockSpeed.LeftAxis.Maximum := fMaxClockSpeedAxis;
  end;
  if fIsC7 or fIsCn then begin
    ldC5J_Temperature := fCpuClockSpeed.GetC7Temperature;
    ChartTemperature.Series[0].Add(ldC5J_Temperature, '', clRed);
    ChartTemperature.Title.Text.Clear;
    ChartTemperature.Title.Text.Add( FloatToStrF( ldC5J_Temperature, ffNumber,
                                      11, 0 ) + ' C' );
    if fMaxC7TempAxis < ldC5J_Temperature then begin
      fMaxC7TempAxis := round( 1.1 * ( ldC5J_Temperature ) );
      fMaxC7Temperature := round( ChartTemperature.SeriesList[0].MaxYValue );
      ChartTemperature.LeftAxis.Maximum := fMaxC7TempAxis;
      ChartTemperature.LeftAxis.Minimum := 0;
      ChartTemperature.MaxPointsPerPage := WINDOW_SIZE;
      ChartTemperature.BottomAxis.Increment := 5;
    end; // if
  end; // if fIsC7
  CheckChartScroll;
  if fMinimized then begin
    application.Title := FloatToStrF( ldClockSpeed, ffNumber, 11, 0 ) + ' MHz';
  end;
  if fIsEnhancedSpeedStepAvailable then SetSpeedStepMenus;
end;

procedure TfrmCPUSpeed.SetSpeedStepMenus;
var
  lSpeedStepState : TSpeedStepState;
  procedure SetMultiplierMenu;
  var
    liMul : integer;
  begin
    //get min and max multipliers
    liMul := lSpeedStepState.FsbMultiplier;
    case liMul of
    4: mi4x.Checked := TRUE;
    5: mi5x.Checked := TRUE;
    6: mi6x.Checked := TRUE;
    7: mi7x.Checked := TRUE;
    8: mi8x.Checked := TRUE;
    9: mi9x.Checked := TRUE;
    10: mi10x.Checked := TRUE;
    11: mi11x.Checked := TRUE;
    12: mi12x.Checked := TRUE;
    13: mi13x.Checked := TRUE;
    14: mi14x.Checked := TRUE;
    15: mi15x.Checked := TRUE;
    16: mi16x.Checked := TRUE;
    17: mi17x.Checked := TRUE;
    18: mi18x.Checked := TRUE;
    19: mi19x.Checked := TRUE;
    20: mi20x.Checked := TRUE;
    21: mi21x.Checked := TRUE;
    22: mi22x.Checked := TRUE;
    23: mi23x.Checked := TRUE;
    24: mi24x.Checked := TRUE;
    25: mi25x.Checked := TRUE;
    else
      ShowMessage( 'procedure TfrmCPUSpeed.SetSpeedStepMenus: Unknown Multiplier encountered!'
                   + CR_LF + IntToStr( liMul ) + 'x');
    end;
  end;
  procedure SetVccMenu;
  var
    liVcc : integer;
  begin
    liVcc := lSpeedStepState.VccStep;
    case liVcc of
    0 : miVcc0.Checked := TRUE;
    1 : miVcc1.Checked := TRUE;
    2 : miVcc2.Checked := TRUE;
    3 : miVcc3.Checked := TRUE;
    4 : miVcc4.Checked := TRUE;
    5 : miVcc5.Checked := TRUE;
    6 : miVcc6.Checked := TRUE;
    7 : miVcc7.Checked := TRUE;
    8 : miVcc8.Checked := TRUE;
    9 : miVcc9.Checked := TRUE;
    10 : miVcc10.Checked := TRUE;
    11 : miVcc11.Checked := TRUE;
    12 : miVcc12.Checked := TRUE;
    13 : miVcc13.Checked := TRUE;
    14 : miVcc14.Checked := TRUE;
    15 : miVcc15.Checked := TRUE;
    16 : miVcc16.Checked := TRUE;
    17 : miVcc17.Checked := TRUE;
    18 : miVcc18.Checked := TRUE;
    19 : miVcc19.Checked := TRUE;
    20 : miVcc20.Checked := TRUE;
    21 : miVcc21.Checked := TRUE;
    22 : miVcc22.Checked := TRUE;
    23 : miVcc23.Checked := TRUE;
    24 : miVcc24.Checked := TRUE;
    25 : miVcc25.Checked := TRUE;
    26 : miVcc26.Checked := TRUE;
    27 : miVcc27.Checked := TRUE;
    28 : miVcc28.Checked := TRUE;
    29 : miVcc29.Checked := TRUE;
    30 : miVcc30.Checked := TRUE;
    31 : miVcc31.Checked := TRUE;
    32 : miVcc32.Checked := TRUE;
    33 : miVcc33.Checked := TRUE;
    34 : miVcc34.Checked := TRUE;
    35 : miVcc35.Checked := TRUE;
    36 : miVcc36.Checked := TRUE;
    37 : miVcc37.Checked := TRUE;
    38 : miVcc38.Checked := TRUE;
    39 : miVcc39.Checked := TRUE;
    40 : miVcc40.Checked := TRUE;
    41 : miVcc41.Checked := TRUE;
    42 : miVcc42.Checked := TRUE;
    43 : miVcc43.Checked := TRUE;
    44 : miVcc44.Checked := TRUE;
    45 : miVcc45.Checked := TRUE;
    46 : miVcc46.Checked := TRUE;
    47 : miVcc47.Checked := TRUE;
    48 : miVcc48.Checked := TRUE;
    else
      if not fUnknownSpeedStepVoltage then begin
        fUnknownSpeedStepVoltage := TRUE;
        ShowMessage( 'procedure TfrmCPUSpeed.SetSpeedStepMenus: Unknown Vcc encountered!'
                   + CR_LF + FloatToStr( lSpeedStepState.Vcc ) + 'V');
      end; //if
    end;
  end;

begin
  lSpeedStepState := fMsrDriver.GetSpeedStepState( 0 );
  SetMultiplierMenu;
  SetVccMenu;
end; // procedure TfrmCPUSpeed.tmrSpeedUpdateTimer

procedure TfrmCPUSpeed.OnCpuSpeedTimer(Sender: TObject);
begin
  UpdateCharts;
end; // procedure TfrmCPUSpeed.tmrSpeedUpdateTimer

procedure TfrmCPUSpeed.UpdateCPUSpeed;
var
  ldClockSpeed   : double;
begin
  ldClockSpeed := fCpuClockSpeed.GetCpuSpeed;
  lblClockspeed.Caption := FloatToStrF( ldClockSpeed, ffNumber, 11, 0 ) + ' Hz';
end;

procedure TfrmCPUSpeed.btnSaveClick(Sender: TObject);
begin
  SaveChartData;
end;

procedure TfrmCPUSpeed.SetChartSampleTime( Value : integer );
var
  s : string;
begin
  fiChartSampleTime := Value;
//  tmrSpeedUpdate.Interval := fiChartSampleTime * 100;
  s := FloatToStr( fiChartSampleTime / 10 );
  chartClockSpeed.BottomAxis.Title.Caption := 'Sample number x '
    + s + ' second(s)';
//  lblUpdatePeriod.Caption := s + 's';
end;

// The clock ratio can only be changed for certain processors (VIA) and only
//  if CHANGE_RATIO_PROGRAM is in the same directory. }
function TfrmCPUSpeed.ClockRatioSettable: Boolean;
begin
  result := fIsCentaur;
end; // function TfrmCPUSpeed.ClockRatioSettable: Boolean;

procedure TfrmCPUSpeed.InitializeFieldVariables;
var
  lsCpuVendor : string;
  procedure SetCalcMethodMenu;
  begin
    miCPUID.Enabled := eCPUID in fsetCpuSpeedCalcSupport;
    miRDTSC.Enabled := eRDTSC in fsetCpuSpeedCalcSupport;
    miWMI.Enabled := eWMI in fsetCpuSpeedCalcSupport;
  end;
begin
  fIsEnhancedSpeedStepAvailable := CpuidIsEnhancedSpeedStepAvailable;
  fUnknownSpeedStepVoltage := FALSE;
  fMaxC7TempAxis := 0;
  fMaxC7Temperature := 0;
  fMaxClockSpeed := 0;
  fTicks := -1;
  if not assigned( fCpuClockSpeed ) then begin
    fCpuClockSpeed := TCpuClockSpeed.Create( self );
    fCpuClockSpeed.MsrDriver := fMsrDriver;
    // find out which clockspeed algorithms are available:
    fsetCpuSpeedCalcSupport := fCpuClockSpeed.CpuSpeedCalculationSupport;
    // set the type of algorythm to use:
    if eCPUID in fsetCpuSpeedCalcSupport then begin
//      SetCalculationMethod( eCPUID );
      // because of processor bug, default to MSR
      SetCalculationMethod( eMSR );
    end else if fIsEnhancedSpeedStepAvailable then begin
      SetCalculationMethod( eMSR );
    end else if eWMI in fsetCpuSpeedCalcSupport then begin
      SetCalculationMethod( eWMI );
    end else begin
      SetCalculationMethod( eRDTSC );
    end;
    // now we need to connect the event handlers:
    fCpuClockSpeed.OnTimerUpdate := OnCpuSpeedTimer;
  end; // if
  SetCalcMethodMenu;
  fCpuVendor := fCpuClockSpeed.DetectCpuVendor;
  fIsC7 := fCpuClockSpeed.IsC7;
  fIsCn := fCpuClockSpeed.IsCn;
  case fCpuVendor of
  eCpuVendorIntel : lsCpuVendor := 'Intel';
  eCpuVendorAMD : lsCpuVendor := 'AMD';
  eCpuVendorVIA : if fIsC7 then lsCpuVendor := 'VIA C7'
                  else if fIsCn then lsCpuVendor := 'VIA CN'
                  else lsCpuVendor := 'VIA';
  eCpuVendorTransmeta : lsCpuVendor := 'Transmeta';
  end; // case
  StatusBar0Update( lsCpuVendor + ' CPU detected.' );
end; // procedure TfrmCPUSpeed.InitializeFieldVariables;

function TfrmCPUSpeed.DetermineMaximumClockSpeed: int64;
begin
  result := fCpuClockSpeed.DetermineMaximumClockSpeed;
  fMaxClockSpeedAxis := round( 1.1 * ( result / ONE_MILLION ) );
end; // function TfrmCPUSpeed.DetermineMaximumClockSpeed

procedure TfrmCPUSpeed.InitializeChart;
begin
  if not assigned( fSeries ) then fSeries := chartClockSpeed.SeriesList[0];
  fSeries.Clear;
  chartClockSpeed.LeftAxis.Maximum := fMaxClockSpeedAxis;
  chartClockSpeed.LeftAxis.Minimum := 0;
  chartClockSpeed.MaxPointsPerPage := WINDOW_SIZE;
  chartClockSpeed.BottomAxis.Increment := 10;
end;

procedure TfrmCPUSpeed.SaveChartData;
var
  lTextFileOut  : TextFile;
  lsFileName    : string;
  lsOutLine     : string;
  s             : string;
  x             : double;
  n             : integer;
const
  DEFAULT_FILENAME = 'CpuClockSpeed_OUT.txt';
begin
  SaveDialog1.Filter := '*.txt';
  SaveDialog1.FileName := DEFAULT_FILENAME;
  if SaveDialog1.Execute then begin
    lsFileName := Trim( SaveDialog1.FileName );
    if lsFileName = '' then lsFileName := DEFAULT_FILENAME;
    AssignFile( lTextFileOut, lsFileName );
    try
      Rewrite( lTextFileOut );
      WriteLn( lTextFileOut, 'Cpu Speed Output File');
      WriteLn( lTextFileOut, '' );
      lsOutLine := 'Date: ' + DateToStr( Date ) + ', Time: ' + TimeToStr( Time );
      WriteLn( lTextFileOut, lsOutLine );
      WriteLn( lTextFileOut, '' );
      WriteLn( lTextFileOut, 'Sample    Clock speed (MHz)');
      WriteLn( lTextFileOut, '========  =================');
      for n := 0 to fSeries.Count - 1 do begin
        s := fSeries.XLabel[n];
        x := n;
        lsOutline := Format('%6.0n' + Chr(9) + '   %11n', [ x, fSeries.YValue[n] ]);
        WriteLn( lTextFileOut, lsOutline);
      end; // for
    finally
      CloseFile( lTextFileOut );
    end; // try..finally
  end; // if
end; // procedure TfrmCPUSpeed.SaveChartData

procedure TfrmCPUSpeed.StatusBar0Update( astrMessage : string );
begin
  StatusBar1.Panels[0].Text := astrMessage;
end; // procedure TfrmCPUSpeed.StatusBar0Update

procedure TfrmCPUSpeed.StatusBar1Update( astrMessage : string );
begin
  StatusBar1.Panels[1].Text := astrMessage;
end; // procedure TfrmCPUSpeed.StatusBar0Update

procedure TfrmCPUSpeed.btnScrollLeftClick(Sender: TObject);
begin
  if chartClockSpeed.BottomAxis.Automatic then begin
    chartClockSpeed.BottomAxis.Automatic := FALSE;
    chartClockSpeed.BottomAxis.SetMinMax(0, 60);
  end else begin
    chartClockSpeed.BottomAxis.Scroll( - ( WINDOW_SIZE div 2 ), TRUE );
  end; // if
  if chartTemperature.BottomAxis.Automatic then begin
    chartTemperature.BottomAxis.Automatic := FALSE;
    chartTemperature.BottomAxis.SetMinMax(0, 60);
  end else begin
    chartTemperature.BottomAxis.Scroll( - ( WINDOW_SIZE div 2 ), TRUE );
  end; // if
end;

procedure TfrmCPUSpeed.btnScrollRightClick(Sender: TObject);
begin
  if chartClockSpeed.BottomAxis.Automatic then begin
    chartClockSpeed.BottomAxis.Automatic := FALSE;
    chartClockSpeed.BottomAxis.SetMinMax(0, 60);
  end else begin
    chartClockSpeed.BottomAxis.Scroll( WINDOW_SIZE div 2, FALSE );
  end; // if
  if chartTemperature.BottomAxis.Automatic then begin
    chartTemperature.BottomAxis.Automatic := FALSE;
    chartTemperature.BottomAxis.SetMinMax(0, 60);
  end else begin
    chartTemperature.BottomAxis.Scroll( WINDOW_SIZE div 2, FALSE );
  end; // if
end;

procedure TfrmCPUSpeed.FormClose(Sender: TObject; var Action: TCloseAction);
begin
//  fMsrDriver.CloseDriver;
  try
    FreeAndNil( fMsrDriver );
  except
    ShowMessage( 'Error destroying MSRDriver.' );
  end;
end;
//
//procedure TfrmCPUSpeed.cboxStayOnTopClick(Sender: TObject);
//begin
//  if CboxStayOnTop.Checked then begin
//    self.FormStyle := fsStayOnTop;
//  end else begin
//    self.FormStyle := fsNormal;
//  end;
//end;
//
//procedure TfrmCPUSpeed.cboxAlphaClick(Sender: TObject);
//begin
//  TrackBarAlpha.Enabled := cboxAlpha.Checked;
//  self.AlphaBlend := cboxAlpha.Checked;
//end;

procedure TfrmCPUSpeed.AppOnMinimize(Sender: TObject);
begin
  fCaption := application.Title;
  fMinimized := TRUE;
end;

procedure TfrmCPUSpeed.AppOnRestore(Sender: TObject);
begin
  fMinimized := FALSE;
  application.Title := fCaption;
end;

procedure TfrmCPUSpeed.btnSaveGraphClick(Sender: TObject);
var
  lsFileName    : string;
const
  DEFAULT_FILENAME = 'CpuClockSpeed.bmp';
begin
  SaveDialog1.Filter := '*.bmp';
  SaveDialog1.FileName := DEFAULT_FILENAME;
  if SaveDialog1.Execute then begin
    lsFileName := Trim( SaveDialog1.FileName );
    if lsFileName = '' then lsFileName := DEFAULT_FILENAME;
    chartClockSpeed.SaveToBitmapFile( lsFileName );
  end; // if
end;

procedure TfrmCPUSpeed.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmCPUSpeed.miLoadDownCpuClick(Sender: TObject);
begin
  fCpuClockSpeed.LoadDownCpu(43);
end;

procedure TfrmCPUSpeed.miMSRClick(Sender: TObject);
begin
  if not assigned( ffrmMSR ) then ffrmMSR := TfrmMSR.Create( self );
  ffrmMSR.Show;
end;

procedure TfrmCPUSpeed.miCPUIDClick(Sender: TObject);
begin
  SetCalculationMethod( eCPUID );
end;

procedure TfrmCPUSpeed.miRDTSCClick(Sender: TObject);
begin
  SetCalculationMethod( eRDTSC );
end;

procedure TfrmCPUSpeed.miWMIClick(Sender: TObject);
begin
  SetCalculationMethod( eWMI );
end;

procedure TfrmCPUSpeed.miMsrCalcClick(Sender: TObject);
begin
  SetCalculationMethod( eMSR );
end;

procedure TfrmCPUSpeed.miAllowOverclockClick(Sender: TObject);
var
  lBitsToSet, lBitsToUnset : T64bits;
begin
  if not fIsC7 then exit;
  if miAllowOverclock.Checked then begin
    showmessage( 'Overclocking / overvolting can damage your system.' + CR_LF +
                 'Use at your own risk!' );
    lBitsToSet := [bit55];
    lBitsToUnset := [];
    fMsrDriver.SetUnsetMsrBits(0, $1107, lBitsToSet, lBitsToUnset);
    mi4x.Enabled := TRUE;
    mi5x.Enabled := TRUE;
    mi6x.Enabled := TRUE;
    mi7x.Enabled := TRUE;
    mi8x.Enabled := TRUE;
    mi9x.Enabled := TRUE;
    mi10x.Enabled := TRUE;
    mi11x.Enabled := TRUE;
    mi12x.Enabled := TRUE;
    mi13x.Enabled := TRUE;
    mi14x.Enabled := TRUE;
    mi15x.Enabled := TRUE;
    mi16x.Enabled := TRUE;
    mi17x.Enabled := TRUE;
    mi18x.Enabled := TRUE;
    mi19x.Enabled := TRUE;
    mi20x.Enabled := TRUE;
    mi21x.Enabled := TRUE;
    mi22x.Enabled := TRUE;
    mi23x.Enabled := TRUE;
    mi24x.Enabled := TRUE;
    mi25x.Enabled := TRUE;
    // now Vcc:
    miVcc0.Enabled := TRUE;
    miVcc1.Enabled := TRUE;
    miVcc2.Enabled := TRUE;
    miVcc3.Enabled := TRUE;
    miVcc4.Enabled := TRUE;
    miVcc5.Enabled := TRUE;
    miVcc6.Enabled := TRUE;
    miVcc7.Enabled := TRUE;
    miVcc8.Enabled := TRUE;
    miVcc9.Enabled := TRUE;
    miVcc10.Enabled := TRUE;
    miVcc11.Enabled := TRUE;
    miVcc12.Enabled := TRUE;
    miVcc13.Enabled := TRUE;
    miVcc14.Enabled := TRUE;
    miVcc15.Enabled := TRUE;
    miVcc16.Enabled := TRUE;
    miVcc17.Enabled := TRUE;
    miVcc18.Enabled := TRUE;
    miVcc19.Enabled := TRUE;
    miVcc20.Enabled := TRUE;
    miVcc21.Enabled := TRUE;
    miVcc22.Enabled := TRUE;
    miVcc23.Enabled := TRUE;
    miVcc24.Enabled := TRUE;
    miVcc25.Enabled := TRUE;
    miVcc26.Enabled := TRUE;
    miVcc27.Enabled := TRUE;
    miVcc28.Enabled := TRUE;
    miVcc29.Enabled := TRUE;
    miVcc30.Enabled := TRUE;
    miVcc31.Enabled := TRUE;
    miVcc32.Enabled := TRUE;
    miVcc33.Enabled := TRUE;
    miVcc34.Enabled := TRUE;
    miVcc35.Enabled := TRUE;
    miVcc36.Enabled := TRUE;
    miVcc37.Enabled := TRUE;
    miVcc38.Enabled := TRUE;
    miVcc39.Enabled := TRUE;
    miVcc40.Enabled := TRUE;
    miVcc41.Enabled := TRUE;
    miVcc42.Enabled := TRUE;
    miVcc43.Enabled := TRUE;
    miVcc44.Enabled := TRUE;
    miVcc45.Enabled := TRUE;
    miVcc46.Enabled := TRUE;
    miVcc47.Enabled := TRUE;
    miVcc48.Enabled := TRUE;
  end else begin
    lBitsToSet := [];
    lBitsToUnset := [bit55];
    fMsrDriver.SetUnsetMsrBits(0, $1107, lBitsToSet, lBitsToUnset);
    InitVccMultMenu;
  end;
end;

procedure TfrmCPUSpeed.mi4xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0, 4);
end;

procedure TfrmCPUSpeed.mi5xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0, 5);
end;

procedure TfrmCPUSpeed.mi6xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0, 6);
end;

procedure TfrmCPUSpeed.mi7xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0, 7);
end;

procedure TfrmCPUSpeed.mi8xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0, 8);
end;

procedure TfrmCPUSpeed.mi9xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0, 9);
end;

procedure TfrmCPUSpeed.mi10xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0, 10);
end;

procedure TfrmCPUSpeed.mi11xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0,11);
end;

procedure TfrmCPUSpeed.mi12xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0, 12);
end;

procedure TfrmCPUSpeed.mi13xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0, 13);
end;

procedure TfrmCPUSpeed.mi14xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0, 14);
end;

procedure TfrmCPUSpeed.mi15xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0, 15);
end;

procedure TfrmCPUSpeed.mi16xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0, 16);
end;

procedure TfrmCPUSpeed.mi17xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0, 17);
end;

procedure TfrmCPUSpeed.mi18xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0, 18);
end;

procedure TfrmCPUSpeed.mi19xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0, 19);
end;

procedure TfrmCPUSpeed.mi20xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0, 20);
end;

procedure TfrmCPUSpeed.mi21xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0, 21);
end;

procedure TfrmCPUSpeed.mi22xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0, 22);
end;

procedure TfrmCPUSpeed.mi23xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0, 23);
end;

procedure TfrmCPUSpeed.mi24xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0, 24);
end;

procedure TfrmCPUSpeed.mi25xClick(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepMultiplier(0, 25);
end;

procedure TfrmCPUSpeed.miVcc0Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 0);
end;

procedure TfrmCPUSpeed.miVcc1Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 1);
end;

procedure TfrmCPUSpeed.miVcc2Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 2);
end;

procedure TfrmCPUSpeed.miVcc3Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 3);
end;

procedure TfrmCPUSpeed.miVcc4Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 4);
end;

procedure TfrmCPUSpeed.miVcc5Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 5);
end;

procedure TfrmCPUSpeed.miVcc6Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 6);
end;

procedure TfrmCPUSpeed.miVcc7Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 7);
end;

procedure TfrmCPUSpeed.miVcc8Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 8);
end;

procedure TfrmCPUSpeed.miVcc9Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 9);
end;

procedure TfrmCPUSpeed.miVcc10Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 10);
end;

procedure TfrmCPUSpeed.miVcc11Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 11);
end;

procedure TfrmCPUSpeed.miVcc12Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 12);
end;

procedure TfrmCPUSpeed.miVcc13Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 13);
end;

procedure TfrmCPUSpeed.miVcc14Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 14);
end;

procedure TfrmCPUSpeed.miVcc15Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 15);
end;

procedure TfrmCPUSpeed.miVcc16Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 16);
end;

procedure TfrmCPUSpeed.miVcc17Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 17);
end;

procedure TfrmCPUSpeed.miVcc18Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 18);
end;

procedure TfrmCPUSpeed.miVcc19Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 19);
end;

procedure TfrmCPUSpeed.miVcc20Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 20);
end;

procedure TfrmCPUSpeed.miVcc21Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 21);
end;

procedure TfrmCPUSpeed.miVcc22Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 22);
end;

procedure TfrmCPUSpeed.miVcc23Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 23);
end;

procedure TfrmCPUSpeed.miVcc24Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 24);
end;

procedure TfrmCPUSpeed.miVcc25Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 25);
end;

procedure TfrmCPUSpeed.miVcc26Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 26);
end;

procedure TfrmCPUSpeed.miVcc27Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 27);
end;

procedure TfrmCPUSpeed.miVcc28Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 28);
end;

procedure TfrmCPUSpeed.miVcc29Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 29);
end;

procedure TfrmCPUSpeed.miVcc30Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 30);
end;

procedure TfrmCPUSpeed.miVcc31Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 31);
end;

procedure TfrmCPUSpeed.miVcc32Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 32);
end;

procedure TfrmCPUSpeed.miVcc33Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 33);
end;

procedure TfrmCPUSpeed.miVcc34Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 34);
end;

procedure TfrmCPUSpeed.miVcc35Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 35);
end;

procedure TfrmCPUSpeed.miVcc36Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 36);
end;

procedure TfrmCPUSpeed.miVcc37Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 37);
end;

procedure TfrmCPUSpeed.miVcc38Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 38);
end;

procedure TfrmCPUSpeed.miVcc39Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 39);
end;

procedure TfrmCPUSpeed.miVcc40Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 40);
end;

procedure TfrmCPUSpeed.miVcc41Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 41);
end;

procedure TfrmCPUSpeed.miVcc42Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 42);
end;

procedure TfrmCPUSpeed.miVcc43Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 43);
end;

procedure TfrmCPUSpeed.miVcc44Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 44);
end;

procedure TfrmCPUSpeed.miVcc45Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 45);
end;

procedure TfrmCPUSpeed.miVcc46Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 46);
end;

procedure TfrmCPUSpeed.miVcc47Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 47);
end;

procedure TfrmCPUSpeed.miVcc48Click(Sender: TObject);
begin
  fMsrDriver.SetSpeedStepVccStep(0, 48);
end;

end.
