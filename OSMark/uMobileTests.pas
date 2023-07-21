unit uMobileTests;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, COSBI_Common, StdCtrls, uCOSBI_SystemInfo, uTests, uOutput,
  Gauges, GR32, Dhrystone, Whetstone, uOSMarkSuite, uCOSBI_TTest,
  ZipForge, uOSMarkResults, ufrmCPUInfo, Buttons, jpeg, ExtCtrls, FileCtrl,
  ComCtrls, IniFiles, frmBandwidthBurn, frmMemLatencyPlus, uFileCopy,
  ufrmWhetBurn, MPlayer, uMobileTestSelect, uAbout, CosbiCpuid, ufrmOSMarkOptions,
  uOSMarkSelectTests, uOSMarkINI, uStopWatch;

type
  TOSMarkMobileTests = class( TComponent )
  const
    CRITICAL_POWER_LEVEL = 10;
    DEFAULT_MOBILE_FILENAME_NORMAL = 'OSMarkMobileResults_Normal.txt';
    DEFAULT_MOBILE_FILENAME_MAX_POWER = 'OSMarkMobileResults_MaxPower.txt';
    DEFAULT_MOBILE_FILENAME_MIN_POWER = 'OSMarkMobileResults_MinPower.txt';
    DEFAULT_MOBILE_FILENAME_PCWB5 = 'OSMarkMobileResults_PCWB5.txt';
    FOREVER = FALSE;
    REFERENCE_NB_PERFORMANCE_SCORE = 748.4; // Score for 1.5GHz Banias in Dell Inspiron 600m w/ 52.2WHr
    REFERENCE_NB_RUNDOWN_TIME = 8518.3; // in seconds for 1.5GHz Banias in Dell Inspiron 600m w/ 52.2WHr battery
    REFERENCE_NB_RUNDOWN_MAX_POWER_TIME = 5557.5; // in seconds for 1.5GHz Banias in Dell Inspiron 600m w/ 52.2WHr battery
  private
    fBatteryLevel         : integer;
    fBatteryLifeScore     : double;
    fMobileScore          : double;
    fNumberOfOfficialRunTests : cardinal;
    fOfficialRun          : Boolean;
    fPerformanceScore     : double;
    fRunTime              : extended;
    fSleepMinutes         : cardinal;
    fSpinUp               : Boolean;
    fStopWatch            : TStopWatch;
    fResultFileName       : string;
    // property field variables:
    fIterations           : integer;
    fMobileTestType       : TMobileTestType;
    fOSMarkTestSuite      : TOSMarkTestSuite;
    fOutputString         : string;
    fSystemInfo           : TSystemInfo;
    fTargetDrive          : char;
    fTotalNumberOfTestsToRun : cardinal;
    // events
    fOnCriticalPowerLevel : TNotifyEvent;
    fOnIterationEnd       : TNotifyEvent;
    fOnIterationStart     : TNotifyEvent;
    fOnNumberOfTestToRun  : TNotifyEvent;
    fOnOutputStatus       : TNotifyEvent;
    fOnOutputString       : TNotifyEvent;
    fOnTestBegins         : TNotifyEvent;
    fOnTestEnds           : TNotifyEvent;
    fOnTestSuiteRunEnd    : TNotifyEvent;
    fOnTestSuiteRunStart  : TNotifyEvent;
  protected
    procedure ComputeScore;
    procedure InitializeOSMarkTestSuiteMaxPowerDraw;
    procedure InitializeOSMarkTestSuiteNormal( abResetPerformance : Boolean = FALSE );
    procedure LinkEventHandlers;
    procedure MinPowerTest;
    procedure OutputScore;
    procedure PCWB5Mobile;
    procedure RunBatteryDepletionTest( abSleepBetweenTests : Boolean = TRUE );
    procedure RunSelectedTests( aiInterate : integer; aSelectedTests: TQTestSet );
    procedure UpdateScore;
    // raise events:
    procedure EventOutputStatus( asOutputString : string );
    procedure EventOutputString( asOutputString : string );
//    procedure EventTestBegins( asOutputString : string );
    // event handlers:
    procedure MobileOfficialRun;
    procedure OSMarkTestSuiteOnOutputString( Sender: TObject );
    procedure SleepForAwhile;
    procedure WaitUntilUnplugged;
  public
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; Override;
    // methods:
    procedure RunMaxPowerMobileTest;
    procedure RunMinPowerMobileTest;
    procedure RunNormalMobileTest;
    procedure RunPCWB5MobileTest;
    procedure SystemShutDown;
    // properties:
    property Iterations : integer read fIterations write fIterations;
    property MobileTestType : TMobileTestType read fMobileTestType write fMobileTestType;
    property OSMarkTestSuite : TOSMarkTestSuite read fOSMarkTestSuite;
    property OutputString : string read fOutputString write fOutputString;
    property ResultFileName : string read fResultFileName write fResultFileName;
    property SleepMinutes : cardinal read fSleepMinutes write fSleepMinutes;
    property SpinUp : Boolean read fSpinUp write fSpinUp;
    property SystemInfo : TSystemInfo read fSystemInfo write fSystemInfo;
    property TargetDrive : char read fTargetDrive write fTargetDrive;
    property TotalNumberOfTestsToRun : cardinal read fTotalNumberOfTestsToRun;
    // events:
    property OnCriticalPowerLevel : TNotifyEvent read fOnCriticalPowerLevel
                                           write fOnCriticalPowerLevel;
    property OnOutputString : TNotifyEvent read fOnOutputString
                                           write fOnOutputString;
    property OnOutputStatus : TNotifyEvent read fOnOutputString
                                           write fOnOutputString;
    property OnTestBegins : TNotifyEvent read fOnTestBegins write fOnTestBegins;
    property OnTestEnds : TNotifyEvent read fOnTestEnds  write fOnTestEnds;
    property OnNumberOfTestToRun : TNotifyEvent read fOnNumberOfTestToRun
                                                write fOnNumberOfTestToRun;
    property OnTestSuiteRunStart : TNotifyEvent read fOnTestSuiteRunStart
                                                 write fOnTestSuiteRunStart;
    property OnTestSuiteRunEnd : TNotifyEvent read fOnTestSuiteRunEnd
                                              write fOnTestSuiteRunEnd;
    property OnIterationStart : TNotifyEvent read fOnIterationStart
                                             write fOnIterationStart;
    property OnIterationEnd : TNotifyEvent read fOnIterationEnd
                                           write fOnIterationEnd;

  end;

implementation

constructor TOSMarkMobileTests.Create( AOwner : TComponent );
var
  lqtTest : TQTests;
begin
  inherited;
  fOfficialRun          := FALSE;
  fStopWatch            := TStopWatch.Create;
  fStopWatch.Resolution := swresHigh;
  fTargetDrive          := 'c';
  fSpinUp               := FALSE;
  fSleepMinutes         := 1;
  fResultFileName       := '';
  // count the number of tests in an official run:
  fNumberOfOfficialRunTests := 0;
  for lqtTest in OFFICIAL_RUN_TESTS do begin
   inc( fNumberOfOfficialRunTests );
  end; // for
end; // constructor

destructor TOSMarkMobileTests.Destroy;
begin
  freeAndNil( fStopWatch );
  inherited;
end; // constructor TOSMarkMobileTests.Create;

procedure TOSMarkMobileTests.OSMarkTestSuiteOnOutputString( Sender: TObject );
begin
  EventOutputStatus( OSMarkTestSuite.OutputString );
  application.processmessages;
end; // procedure UpdateStatusMemo( asOutputLine : string );


procedure TOSMarkMobileTests.InitializeOSMarkTestSuiteMaxPowerDraw;
begin
  if not assigned( fSystemInfo ) then raise exception.create( 'You did not pass the MobileTest object a system info object.' );
  fOSMarkTestSuite.Initialize( fSystemInfo );
  fOSMarkTestSuite.ShowStatus       := FALSE;
  fOSMarkTestSuite.SpinUp           := TRUE;
  fOSMarkTestSuite.ProcessIdleTasks := FALSE;
  fOSMarkTestSuite.DiskDefrag       := FALSE;
  fOSMarkTestSuite.TargetDisk       := fTargetDrive;
  fOSMarkTestSuite.ThreadCount      := tcAuto;
  LinkEventHandlers;
  fPerformanceScore := 0;
  fBatteryLifeScore := 0;
  fMobileScore := 0;
end; // procedure TfrmOSMark.InitializeOSMarkTestSuite;

procedure TOSMarkMobileTests.InitializeOSMarkTestSuiteNormal( abResetPerformance : Boolean = FALSE );
begin
  if not assigned( fSystemInfo ) then raise exception.create( 'You did not pass the MobileTest object a system info object.' );
  fOSMarkTestSuite.Initialize( fSystemInfo );
  fOSMarkTestSuite.ShowStatus       := TRUE;
  fOSMarkTestSuite.SpinUp           := fSpinUp;
  fOSMarkTestSuite.ProcessIdleTasks := FALSE;
  fOSMarkTestSuite.DiskDefrag       := FALSE;
  fOSMarkTestSuite.TargetDisk       := fTargetDrive;
  fOSMarkTestSuite.ThreadCount      := tcAuto;
  LinkEventHandlers;
  if abResetPerformance then fPerformanceScore := 0;
  fBatteryLifeScore := 0;
  fMobileScore := 0;
end; // procedure TfrmOSMark.InitializeOSMarkTestSuite;

procedure TOSMarkMobileTests.LinkEventHandlers;
begin
  fOSMarkTestSuite.OnOutputString := OSMarkTestSuiteOnOutputString;
  fOSMarkTestSuite.OnTestSuiteRunStart := OnTestSuiteRunStart;
  fOSMarkTestSuite.OnIterationStart := OnIterationStart;
  fOSMarkTestSuite.OnTestBegins := OnTestBegins;
  fOSMarkTestSuite.OnTestEnds := OnTestEnds;
  fOSMarkTestSuite.OnTestSuiteRunEnd := OnTestSuiteRunEnd;
end; // procedure TOSMarkMobileTests.LinkEventHandlers;

procedure TOSMarkMobileTests.EventOutputString( asOutputString : string );
begin
  fOutputString := asOutputString;
  if assigned( OnOutputString ) then OnOutputString( self );
  Application.ProcessMessages;
end; //procedure TOSMarkMobileTests.OutputString( var asOutputString : string )

procedure TOSMarkMobileTests.EventOutputStatus( asOutputString : string );
begin
  fOutputString := asOutputString;
  if assigned( OnOutputStatus ) then OnOutputStatus( self );
  Application.ProcessMessages;
end; //procedure TOSMarkMobileTests.OutputString( var asOutputString : string )

procedure TOSMarkMobileTests.OutputScore;
var s : string;
begin
  s := CR_LF + '**********' + CR_LF;
  case fMobileTestType of
    mttMaxPowerDraw: begin
      s := s + 'Max Power Battery depletion time ' + TAB + FloatToStrF(fRunTime / 60, ffNumber,6, 3) + ' minutes' + CR_LF;
      s := s + 'Performance score ' + TAB +  FloatToStrF( fPerformanceScore, ffNumber,6, 3) + CR_LF;
      s := s + 'Max Power Battery life score ' + TAB + FloatToStrF( fBatteryLifeScore, ffNumber,6, 3) + CR_LF;
      s := s + 'OVERALL MAX POWER MOBILE SCORE ' + TAB + FloatToStrF( fMobileScore, ffNumber,6, 3) + CR_LF;
    end;
  else
    s := s + 'Battery depletion time ' + TAB + FloatToStrF(fRunTime / 60, ffNumber,6, 3) + ' minutes' + CR_LF;
    s := s + 'Performance score ' + TAB +  FloatToStrF( fPerformanceScore, ffNumber,6, 3) + CR_LF;
    s := s + 'Battery life score ' + TAB + FloatToStrF( fBatteryLifeScore , ffNumber,6, 3) + CR_LF;
    s := s + 'OVERALL MOBILE SCORE ' + TAB + FloatToStrF( fMobileScore , ffNumber,6, 3) + CR_LF;
  end; // case
  EventOutputString( s );
  fBatteryLevel := GetBatteryLevel;
  EventOutputString( 'Battery level: ' + TAB +  IntToStr( fBatteryLevel ) );
  if (fBatteryLevel <= CRITICAL_POWER_LEVEL) then  begin
    if assigned( fOnCriticalPowerLevel ) then begin
      fOnCriticalPowerLevel( self );
      Application.ProcessMessages;
    end; // if
  end; // if
end; // OutputScore;

procedure TOSMarkMobileTests.MobileOfficialRun;
begin
  fOfficialRun := TRUE;
  fTotalNumberOfTestsToRun := 3 * fNumberOfOfficialRunTests;
  fOSMarkTestSuite := TOSMarkTestSuite.Create( self );
  try
    case fMobileTestType of
      mttMaxPowerDraw: InitializeOSMarkTestSuiteMaxPowerDraw;
    else
      InitializeOSMarkTestSuiteNormal( TRUE );
    end; // case
    fOSMarkTestSuite.OfficialRun;
    fPerformanceScore := 1000 *
      fOSMarkTestSuite.OSMarkTestResults.GetCosbiScore( FALSE ) /
      REFERENCE_NB_PERFORMANCE_SCORE;
  finally
    freeAndNil( fOSMarkTestSuite );
  end; // try...finally
  fOfficialRun := FALSE;
end; // procedure MobileOfficialRun;

procedure TOSMarkMobileTests.RunSelectedTests( aiInterate : integer;
                                       aSelectedTests: TQTestSet );
begin
  fOSMarkTestSuite := TOSMarkTestSuite.Create( self );
  try
    InitializeOSMarkTestSuiteNormal( FALSE );
    fOSMarkTestSuite.RunTheseTests(aSelectedTests, aiInterate);
    Application.ProcessMessages;
  finally
    freeAndNil(   fOSMarkTestSuite );
  end; // try...finally
end; // procedure TOSMarkMobileTests.RunSelectedTests


procedure TOSMarkMobileTests.ComputeScore;
begin
  // calculate battery life score:
  fRunTime := fStopWatch.SplitTime;
  if fMobileTestType = mttMaxPowerDraw then begin
    fBatteryLifeScore := ( fRunTime / REFERENCE_NB_RUNDOWN_MAX_POWER_TIME ) * 1000;
  end else begin
    fBatteryLifeScore := ( fRunTime / REFERENCE_NB_RUNDOWN_TIME ) * 1000;
  end;
  fMobileScore := (fPerformanceScore + fBatteryLifeScore) / 2;
end;

procedure TOSMarkMobileTests.UpdateScore;
begin
  ComputeScore;
  OutputScore;
end;

procedure TOSMarkMobileTests.SleepForAwhile;
var
  s : string;
begin
  if fSleepMinutes = 1 then begin
    s := CR_LF + 'Sleeping for 1 minute between tests...';
  end else begin
    s := CR_LF + 'Sleeping for ' + intToStr( fSleepMinutes ) + ' minutes between tests...';
  end; // if
  EventOutputString( s );
  sleep( 1000 * 60 * fSleepMinutes );
end;

procedure TOSMarkMobileTests.RunBatteryDepletionTest( abSleepBetweenTests : Boolean );
var
  lqtTest : TQTests;
begin
  // need to set this for progressbar:
  fTotalNumberOfTestsToRun := 1;
  // we need to run tests spaced at one minute apart until the battery runs down:
  repeat
    for lqtTest in OFFICIAL_RUN_TESTS do begin
      RunSelectedTests( 1 , [lqtTest] );
      // write out score
      UpdateScore;
      // on normal test, sleep for a minute between tests, otherwise proceed to
      // next test:
      if abSleepBetweenTests then begin
        // wait one minute
        EventOutputStatus( 'Sleeping between tests...' );
        application.ProcessMessages;
        SleepForAwhile;
      end; // if
    end; // for
  until FOREVER;
end;

procedure TOSMarkMobileTests.RunMaxPowerMobileTest;
begin
  fMobileTestType := mttMaxPowerDraw;
  EventOutputStatus( 'Press <Esc> to terminate.' );
  WaitUntilUnplugged;
  fStopWatch.StartTimer;
  fOSMarkTestSuite := TOSMarkTestSuite.Create( self );
  try
    InitializeOSMarkTestSuiteMaxPowerDraw;
    MobileOfficialRun;
  finally
    freeAndNil( fOSMarkTestSuite );
  end; // try...finally
  // Issue an official run:
  UpdateScore;
  // there is no need to spin up prior to tests for battery drain measurement:
  fSpinUp := FALSE;
  EventOutputStatus( 'Running battery depletion tests...' );
  RunBatteryDepletionTest( FALSE );
end;

procedure TOSMarkMobileTests.RunMinPowerMobileTest;
begin
  MinPowerTest;
end;

procedure TOSMarkMobileTests.WaitUntilUnplugged;
var
  lbShowUnplugMessage : Boolean;
begin
  lbShowUnplugMessage := TRUE;
  while not IsOnBatteryPower do begin
    sleep( 1000 );
    if lbShowUnplugMessage then begin
      EventOutputStatus( 'Waiting for you to unplug the notebook...' );
      lbShowUnplugMessage := FALSE;
    end;
    beep;
    application.ProcessMessages;
  end;
end;

procedure TOSMarkMobileTests.RunNormalMobileTest;
begin
  fMobileTestType := mttNormal;
  EventOutputStatus( 'Press <Esc> to terminate.' );
  WaitUntilUnplugged;
  fStopWatch.StartTimer;
  fOSMarkTestSuite := TOSMarkTestSuite.Create( self );
  try
    MobileOfficialRun;
  finally
    freeAndNil( fOSMarkTestSuite );
  end; // try...finally
  // Issue an official run:
  UpdateScore;
  // there is no need to spin up prior to tests for battery drain measurement:
  fSpinUp := FALSE;
  EventOutputStatus( 'Running battery depletion tests...' );
  RunBatteryDepletionTest( TRUE );
end;

procedure TOSMarkMobileTests.RunPCWB5MobileTest;
begin
  PCWB5Mobile;
end;

procedure TOSMarkMobileTests.MinPowerTest;
var
  i, j : integer;
const
  ONE_SECOND = 1000;
  ONE_MINUTE = 60 * ONE_SECOND;
begin
  fMobileTestType := mttLowPowerDraw;
  EventOutputStatus( 'Press <Esc> to terminate.' );
  WaitUntilUnplugged;
  fStopWatch.StartTimer;
  EventOutputString( 'Starting minimum power draw mobile test (press <Esc> to abort)...' );
  for i := 1 to 10000 do begin
    EventOutputString( 'Sleeping for one minute. Iteration ' + IntToStr( i ) );
    for j := 1 to 60 do begin
      sleep( ONE_SECOND );
      application.ProcessMessages;
    end;
    EventOutputString( 'Total elapsed time (s): ' + TAB +  FloatToStrF( fStopWatch.SplitTime, ffNumber,6, 3) );
    EventOutputString( 'Total elapsed time (m): ' + TAB +  FloatToStrF( fStopWatch.SplitTime / 60, ffNumber,6, 3) );
    EventOutputString( 'Total elapsed time (h): ' + TAB +  FloatToStrF( fStopWatch.SplitTime / 3600, ffNumber,6, 3) );
    EventOutputString( 'Battery level: ' + TAB +  IntToStr( GetBatteryLevel ) );
    application.ProcessMessages;
    fBatteryLevel := GetBatteryLevel;
    EventOutputString( 'Battery level: ' + TAB +  IntToStr( fBatteryLevel ) );
    if (fBatteryLevel <= 8) then  begin
      if assigned( fOnCriticalPowerLevel ) then begin
        fOnCriticalPowerLevel( self );
        Application.ProcessMessages;
      end; // if
    end; // if
  end;
end; // procedure TOSMarkMobileTests.MinPowerTest( asSaveFileName : string );


procedure TOSMarkMobileTests.PCWB5Mobile;
var
  i : integer;
const
  TRACE_FILE = 'C:\WorldBench\Results\msoffice.trc';
  PCWB5_OFFICE_SCRIPT = 'c:\worldbench\mtrun.exe /c /s c:\worldbench\msoffice.pc6';
  INIT_PCWB5_OFFICE_WORK_FILES = 'PCWB5_Initialize.bat';
  FRESH_OFFICE_WORK_FILES = 'c:\pcwb5_msoffice_work\*.*';
  FIVE_MINUTES_SLEEP = 5 * 60 * 1000;

  procedure InitializeOfficeWorkFiles;
  begin
    DeleteFile( TRACE_FILE );
    Application.ProcessMessages;
    sleep( 1000 );
    StartProgramWait( 'PCWB5_Initialize.bat', SW_NORMAL );
  end;

  procedure LaunchPCWB5OfficeTest;
  begin
    Application.ProcessMessages;
    // sleep 2 seconds:
    sleep( 2000 );
    StartProgramWait( PCWB5_OFFICE_SCRIPT, SW_NORMAL );
  end;

  procedure UpdatePCWB5Score;
  var
    leSplitTime : extended;
  begin
    leSplitTime := fStopWatch.SplitTime;
    EventOutputString( 'Total elapsed time (s): ' + TAB +  FloatToStrF( leSplitTime, ffNumber,6, 3) );
    EventOutputString( 'Total elapsed time (m): ' + TAB +  FloatToStrF( leSplitTime / 60, ffNumber,6, 3) );
    EventOutputString( 'Total elapsed time (h): ' + TAB +  FloatToStrF( leSplitTime / 3600, ffNumber,6, 3) );
    fBatteryLevel := GetBatteryLevel;
    EventOutputString( 'Battery level: ' + TAB +  IntToStr( fBatteryLevel ) );
    if (fBatteryLevel <= CRITICAL_POWER_LEVEL) then  begin
      if assigned( fOnCriticalPowerLevel ) then begin
        fOnCriticalPowerLevel( self );
        Application.ProcessMessages;
      end; // if
    end; // if
  end;

begin
  fMobileTestType := mttPCWB5;
  EventOutputStatus( 'Press <Esc> to terminate.' );
  WaitUntilUnplugged;
  // we need to time how long the system is running:
  fStopWatch.StartTimer;
  for i := 1 to fIterations do begin
    EventOutputString( 'Initializing MS Office work files...' );
    InitializeOfficeWorkFiles;
    EventOutputString( 'Launching PCWorldBench5 Office test for iteration ' + IntToStr( i ) );
    UpdatePCWB5Score;
    LaunchPCWB5OfficeTest;
    EventOutputString( 'PCWorldBench5 Office test complete for iteration ' + IntToStr( i ) );
    UpdatePCWB5Score;
    EventOutputString( 'Sleeping for five minutes...' );
    sleep( FIVE_MINUTES_SLEEP );
    EventOutputString( 'Finished Sleeping. Total elapsed time (s): ' + TAB +  FloatToStrF( fStopWatch.SplitTime, ffNumber,6, 3) );
    UpdatePCWB5Score;
  end;
end; // procedure TOSMarkMobileTests.PCWB5Mobile;

procedure TOSMarkMobileTests.SystemShutDown;
begin
  EventOutputString( 'Shutting down...  Final data:');
  EventOutputString( 'Total elapsed time (s): ' + TAB +  FloatToStrF( fStopWatch.SplitTime, ffNumber,6, 3) );
  EventOutputString( 'Total elapsed time (m): ' + TAB +  FloatToStrF( fStopWatch.SplitTime / 60, ffNumber,6, 3) );
  EventOutputString( 'Total elapsed time (h): ' + TAB +  FloatToStrF( fStopWatch.SplitTime / 3600, ffNumber,6, 3) );
  EventOutputString( 'Battery level: ' + TAB +  IntToStr( GetBatteryLevel ) );
  if fMobileTestType in [mttNormal, mttMaxPowerDraw] then UpdateScore;
end; // procedure TOSMarkMobileTests.SystemShutDown;

end.
