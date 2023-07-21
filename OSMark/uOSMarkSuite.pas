unit uOSMarkSuite;
{
  COSBI: Comprehensive Open Source Benchmarking Initiative
  Copyright (c) 2000, 2001, 2002, 2003, 2004, 2005 Van Smith

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

  The current web address of the GNU General Public License is:
  http://www.gnu.org/licenses/gpl.html

  You can contact the authors of this software at:
  cosbi@vanshardware.com
  See www.vanshardware.com or www.cosbi.org for more contact details.
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Grids, COSBI_Common, GR32_Blend, GR32_Image,
  GR32, ComCtrls, Gauges, Maze, uCOSBI_TTest, uCOSBI_TGraphicsTest, uOutput,
  uTests, Dhrystone, Whetstone, math, uCOSBI_SystemInfo, COSBI_Status,
  uThinClientTests, uStopWatch;

Type

  TQTestSet = set of TQTests;

  TOSMarkTestResult = record
    TestType : TQTests;
    TestName : string;
    TestTime : double;
    TestScore : double;
    TestRawScore : double;
    TestRawUnits  : string;
    PerformanceVectors : TPerformanceVectors;
  end; // TOSMarkTestResult = record

  TOSMarkTestResultArray = array of TOSMarkTestResult;

  TOSMarkTestResults = class( TComponent )
  private
    fOfficialRun : Boolean;
  protected
    fHardwareInfo : THardwareInfo;
    fOsInfo : TOS_Info;
    fRunDateTime : TDateTime;
    fSystemInfo : TSystemInfo;
    fTestResults : TOSMarkTestResultArray;
  public
    constructor Create( AOwner : TComponent ); override;
    function GetAverageScore( aQTest : TQTests ): double;
    function GetAverageTime( aQTest : TQTests ): double;
    function GetBestResult( aQTest : TQTests; var adBestScore : double ): string;
    function GetBestScore( aQTest : TQTests ): double;
    function GetBestSuiteResults( aQTestSet : TQTestSet ): string;
    function GetBestTime( aQTest : TQTests ): double;
    function GetBestPerfVectorResult( aQTest : TQTests;
                                     aPerfVector : TPerformanceVector ): double;
    function GetCosbiScore(abGeometricMean : Boolean): double;
    function GetPerfVectorResults( aQTestSet : TQTestSet;
                              aPerfVector : TPerformanceVector ): double;
    function GetWorstScore( aQTest : TQTests ): double;
    function GetWorstTime( aQTest : TQTests ): double;
    procedure AddResult( aTestResult : TOSMarkTestResult );
    procedure Initialize( aSystemInfo: TSystemInfo );
    property HardwareInfo : THardwareInfo read fHardwareInfo;
    property OfficialRun : Boolean read fOfficialRun write fOfficialRun;
    property OsInfo : TOS_Info read fOsInfo;
    property RunDateTime : TDateTime read fRunDateTime write fRunDateTime;
    property SystemInfo : TSystemInfo read fSystemInfo write fSystemInfo;
    property TestResults: TOSMarkTestResultArray read fTestResults;
  end; //TOSMarkTestResults

  TOSMarkTestSuite = Class( TComponent )
  private
  protected
    fStopWatch            : TStopwatch;
    fSelectedTests        : TQTestSet;
    fSelectAll            : Boolean;
    fOfficialRun          : Boolean;
    fOSMarkTestResults    : TOSMarkTestResults;
    fSystemInfo : TSystemInfo;
    fShowStatus : Boolean;
    fSpinUp : Boolean;
    fProcessIdleTasks : Boolean;
    fDiskDefrag : Boolean;
    fTargetDisk : char;
    fOutputString : string;
    fOnOutputString : TNotifyEvent;
    fTestName : string;
    fIteration : integer;
    fIterationFinal : integer;
    fOnTestBegins : TNotifyEvent;
    fTestScore : integer;
    fTestTime  : double;
    fOnTestEnds : TNotifyEvent;
    fTotalNumberOfTestsToRun : integer;
    fOnNumberOfTestToRun : TNotifyEvent;
    fOnTestSuiteRunStart : TNotifyEvent;
    fOnTestSuiteRunEnd : TNotifyEvent;
    fOnIterationStart : TNotifyEvent;
    fOnIterationEnd : TNotifyEvent;
    fThreadCount : TThreadCount;
    function CountTests   : integer;
    procedure TestSuiteRun( aiRunCount : integer );
    procedure OutputResultHeader( aiIterations : integer );
    procedure OutputSuiteResultHeader( aiIterations : integer );
    procedure OutputResultFooter;
    procedure LogResults( aQuickTest: TTest );
    procedure RunTest( var aTest : TTest;
                       aiRunCount : integer );
    procedure SetSystemInfo( Value : TSystemInfo );
  public
    constructor Create( AOwner : TComponent ); override;
    procedure Initialize( aSystemInfo: TSystemInfo);
    destructor Destroy; Override;
    procedure OutString( asOutputString : string );
    procedure RunTheseTests( aQTSet: TQTestSet; aiIterations: integer );
    procedure OfficialRun;
    procedure HandleDiskDefrag;
    procedure HandleProcessIdleTasks;
    procedure CheckDriveSpace;
    property SelectAll : Boolean read fSelectAll write fSelectAll;
    property SelectedTests : TQTestSet read fSelectedTests
                                       write fSelectedTests;
    property OSMarkTestResults : TOSMarkTestResults
                           read fOSMarkTestResults
                           write fOSMarkTestResults;
    property SystemInfo : TSystemInfo read fSystemInfo write SetSystemInfo;
    property ShowStatus : Boolean read fShowStatus write fShowStatus;
    property SpinUp : Boolean read fSpinUp write fSpinUp;
    property ProcessIdleTasks : Boolean read fProcessIdleTasks write fProcessIdleTasks;
    property DiskDefrag : Boolean read fDiskDefrag write fDiskDefrag;
    property TargetDisk : char read fTargetDisk write fTargetDisk;
    property OutputString :string read fOutputString write fOutputString;
    property Iteration : integer read fIteration write fIteration;
    property IterationFinal : integer read fIterationFinal write fIterationFinal;
    property TestName : string read fTestName write fTestName;
    property TestScore : integer read fTestScore write fTestScore;
    property TestTime : double read fTestTime write fTestTime;
    property TotalNumberOfTestsToRun : integer read fTotalNumberOfTestsToRun
                                               write fTotalNumberOfTestsToRun;
    property OnOutputString : TNotifyEvent read fOnOutputString
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
    property ThreadCount : TThreadCount read fThreadCount write fThreadCount;
  end; // TOSMarkTestSuite

const
  OFFICIAL_RUN_TESTS = [qt7zip, qtAlphaBlend, qtAlphaDots, qtBandwidthBP64,
    qtPi, qtCircles, qtCppCompiler, qtDhrystoneThreads, qtDrawEllipses,
    qtEncryptDecrypt,
    qtFern, qtFibThreads, qtFileCopy, qtGogoEncode, qtGrid, qtGridFP,
    qtIdenticalThreads,
    qtImageResize, qtImageRotate,  qtJpgDecode, qtLame, qtDrawLines, qtLorenz,
    qtMandelbrotThreads, qtMaze, qtMemLatency,  qtMetaballs, qtMontMul, qtNBody,
    qtNBodyOpenGL, qtOgg, qtOrthogonalThreads, qtPiThreads, qtPlotTrig,
    qtPlotTrig2,
    qtPlotLines, qtPngOut, qtRandomDots, qtDrawRectangles, qtRichEd, qtSse3,
    qtSha1, qtSha256, qtUpx, qtWebPageLoad, qtWhetstoneThreads, qtZipCompress
    ];

  OFFICIAL_RUN_ITERATIONS = 3;  //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ERR_TEST_TYPE_NOT_FOUND = ': You specified a test type that I could not find.';
  ERR_DRIVE_IS_TOO_SMALL = ': The drive you have chosen has too little free space to run drive tests.';

implementation

  constructor TOSMarkTestResults.Create( AOwner : TComponent );
  begin
    inherited;
  end; // constructor

  procedure TOSMarkTestResults.Initialize( aSystemInfo: TSystemInfo );
  begin
    if not assigned( aSystemInfo ) then begin
      raise exception.Create('TOSMarkTestResults.Initialize: Pass me an instantiated SystemInfo object, you dork!' );
    end; // if
    fSystemInfo := aSystemInfo;
    fSystemInfo.ShowStatus := TRUE;
    if not fSystemInfo.Initialized then begin
      fSystemInfo.Initialize;
    end;
    fHardwareInfo := fSystemInfo.HardwareInfo;
    fOsInfo := fSystemInfo.OsInfo;
    fRunDateTime := now;
  end;

  function TOSMarkTestResults.GetBestScore( aQTest : TQTests ): double;
  var
    i : integer;
  begin
    result := -1;
    for i := low( fTestResults ) to high( fTestResults ) do begin
      if (fTestResults[i].TestType = aQTest) then begin
        if fTestResults[i].TestScore > result then begin
          result := fTestResults[i].TestScore;
        end; // if
      end; // if
    end; // for
  end; //function TOSMarkTestResults.GetBestScore

  function TOSMarkTestResults.GetAverageScore( aQTest : TQTests ): double;
  var
    i : integer;
    liCount : integer;
    ldAccumulate : double;
  begin
    result := -1;
    liCount := 0;
    ldAccumulate := 0;
    for i := low( fTestResults ) to high( fTestResults ) do begin
      if (fTestResults[i].TestType = aQTest) then begin
        ldAccumulate := ldAccumulate + fTestResults[i].TestScore;
        inc( liCount );
      end; // if
    end; // for
    if liCount > 0 then result := ldAccumulate / liCount;
  end; //function TOSMarkTestResults.GetAverageScore

  function TOSMarkTestResults.GetWorstScore( aQTest : TQTests ): double;
  var
    i : integer;
  begin
    result := 10000000000000000;
    for i := low( fTestResults ) to high( fTestResults ) do begin
      if (fTestResults[i].TestType = aQTest) then begin
        if fTestResults[i].TestScore < result then begin
          result := fTestResults[i].TestScore;
        end; // if
      end; // if
    end; // for
  end; //function TOSMarkTestResults.GetWorstScore

  // GetCosbiScore will return the average score unless geometric mean is
  // specified.
  function TOSMarkTestResults.GetCosbiScore(abGeometricMean : Boolean): double;
  var
    lTest : TQTests;
    lFirstTest : TQTests;
    lLastTest : TQTests;
    liCount : integer;
    ldAccumulate : double;
  begin
    // geometric mean = (x1 * x2 * x3 ... * xn) ^ (1/n)
    result := -1;
    liCount := 0;
    if abGeometricMean then begin
      ldAccumulate := 1;
    end else begin
      ldAccumulate := 0;
    end;
    if fOfficialRun then begin
      lFirstTest := succ( qtFirstTest );
      lLastTest := pred( qtLastTest );
      for lTest := lFirstTest to lLastTest do begin
        if lTest in OFFICIAL_RUN_TESTS then begin
          if abGeometricMean then begin
            ldAccumulate := ldAccumulate * GetBestScore( lTest );
          end else begin
            ldAccumulate := ldAccumulate + GetBestScore( lTest );
          end;
          inc( liCount );
        end; // if lTest in OFFICIAL_RUN_TESTS
      end; // for lTest := qtFirstTest
      if abGeometricMean then begin
        if liCount > 0 then result := power( ldAccumulate, (1 / liCount) );
      end else begin
        if liCount > 0 then result := ldAccumulate / liCount;
      end;
    end; // if fOfficialRun
  end; //function TOSMarkTestResults.GetWorstScore

  function TOSMarkTestResults.GetBestTime( aQTest : TQTests ): double;
  var
    i : integer;
  begin
    result := 1000000000;
    for i := low( fTestResults ) to high( fTestResults ) do begin
      if (fTestResults[i].TestType = aQTest) then begin
        if fTestResults[i].TestTime < result then begin
          result := fTestResults[i].TestTime;
        end; // if
      end; // if
    end; // for
  end; //function TOSMarkTestResults.GetBestTime

  function TOSMarkTestResults.GetAverageTime( aQTest : TQTests ): double;
  var
    i : integer;
    liCount : integer;
    ldAccumulate : double;
  begin
    result := -1;
    liCount := 0;
    ldAccumulate := 0;
    for i := low( fTestResults ) to high( fTestResults ) do begin
      if (fTestResults[i].TestType = aQTest) then begin
        ldAccumulate := ldAccumulate + fTestResults[i].TestTime;
        inc( liCount );
      end; // if
    end; // for
    if liCount > 0 then result := ldAccumulate / liCount;
  end; //function TOSMarkTestResults.GetAverageTime

  function TOSMarkTestResults.GetWorstTime( aQTest : TQTests ): double;
  var
    i : integer;
  begin
    result := -1;
    for i := low( fTestResults ) to high( fTestResults ) do begin
      if (fTestResults[i].TestType = aQTest) then begin
        if fTestResults[i].TestTime > result then begin
          result := fTestResults[i].TestTime;
        end; // if
      end; // if
    end; // for
  end; //function TOSMarkTestResults.GetWorstTime

  procedure TOSMarkTestResults.AddResult( aTestResult : TOSMarkTestResult );
  var
    liArrayLength : integer;
  begin
    liArrayLength := high( fTestResults ) + 1; // zero based so add one
    SetLength( fTestResults, liArrayLength + 1 ); // bump up one element
    fTestResults[ liArrayLength ] := aTestResult; // store in last element
  end; // procedure TOSMarkTestResults.AddResult

  // GetBestResult returns a string with the test name, test score and test
  // time for the best result.
  function TOSMarkTestResults.GetBestResult( aQTest : TQTests; var adBestScore : double ): string;
  var
    i : integer;
  begin
    result := '';
    adBestScore := -1;
    for i := low( fTestResults ) to high( fTestResults ) do begin
      if (fTestResults[i].TestType = aQTest) then begin
        if fTestResults[i].TestScore > adBestScore then begin
          adBestScore := fTestResults[i].TestScore;
          result := fTestResults[i].TestName + ', '  +
            FloatToStrF( fTestResults[i].TestScore, ffGeneral, 6, 0 ) + ', ' +
            FloatToStrF( fTestResults[i].TestTime, ffGeneral, 6, 3 );
        end; // if
      end; // if
    end; // for
    if result = '' then raise exception.Create( 'TOSMarkTestResults.GetBestResult' +
                                                ERR_TEST_TYPE_NOT_FOUND );
  end; //function TOSMarkTestResults.GetBestResult

  // GetBestPerfVectorResult returns the best test score for the specified
  // test if it falls within the performance vector.
  function TOSMarkTestResults.GetBestPerfVectorResult( aQTest : TQTests;
           aPerfVector : TPerformanceVector ): double;
  var
    i : integer;
  begin
    result := -1;
    for i := low( fTestResults ) to high( fTestResults ) do begin
      if (fTestResults[i].TestType = aQTest)
          and (aPerfVector in fTestResults[i].PerformanceVectors) then begin
        if fTestResults[i].TestScore > result then begin
          result := fTestResults[i].TestScore;
        end; // if
      end; // if
    end; // for
  end; //function TOSMarkTestResults.GetBestPerfVectorResult

  // GetBestSuiteResults returns a string with the test name, test score and test
  // time for the best result of each test in the suite.
  function TOSMarkTestResults.GetBestSuiteResults( aQTestSet : TQTestSet ): string;
  var
    lTest : TQTests;
    lFirstTest : TQTests;
    lLastTest : TQTests;
    lstrSuiteResult : string;
    ldBestScore, ldScoreSum, ldScoreAverage : double;
    liNumberOfScores : integer;
    procedure GenerateVectors;
    var
      ldScoreProduct : double;
    begin
      // obtain performance vectors:
      // initialize accumulators
      liNumberOfScores := 0;
      ldScoreSum := 0;
      ldScoreProduct := 1;
      // get the vectors
      ldBestScore := GetPerfVectorResults( aQTestSet, pvResponsiveness );
      if ldBestScore > -1 then begin
        ldScoreSum := ldScoreSum + (ldBestScore * ldBestScore);
        ldScoreProduct := ldScoreProduct * ldBestScore;
        inc( liNumberOfScores );
        result := result + '* Responsiveness Performance Vector, '
          + FloatToStrF(ldBestScore, ffGeneral, 4, 0) + CR_LF;
      end;
      ldBestScore := GetPerfVectorResults( aQTestSet, pv3dGaming );
      if ldBestScore > -1 then begin
        ldScoreSum := ldScoreSum + (ldBestScore * ldBestScore);
        ldScoreProduct := ldScoreProduct * ldBestScore;
        inc( liNumberOfScores );
        result := result + '* 3d Gaming Performance Vector, '
          + FloatToStrF(ldBestScore, ffGeneral, 4, 0) + CR_LF;
      end;
      ldBestScore := GetPerfVectorResults( aQTestSet, pvArtificialIntelligence );
      if ldBestScore > -1 then begin
        ldScoreSum := ldScoreSum + (ldBestScore * ldBestScore);
        ldScoreProduct := ldScoreProduct * ldBestScore;
        inc( liNumberOfScores );
        result := result + '* Artificial Intelligence Performance Vector, '
          + FloatToStrF(ldBestScore, ffGeneral, 4, 0) + CR_LF;
      end;
      ldBestScore := GetPerfVectorResults( aQTestSet, pvDataSetTransform );
      if ldBestScore > -1 then begin
        ldScoreSum := ldScoreSum + (ldBestScore * ldBestScore);
        ldScoreProduct := ldScoreProduct * ldBestScore;
        inc( liNumberOfScores );
        result := result + '* Data Set Transforms Performance Vector, '
          + FloatToStrF(ldBestScore, ffGeneral, 4, 0) + CR_LF;
      end;
      ldBestScore := GetPerfVectorResults( aQTestSet, pvMathScienceEngineering );
      if ldBestScore > -1 then begin
        ldScoreSum := ldScoreSum + (ldBestScore * ldBestScore);
        ldScoreProduct := ldScoreProduct * ldBestScore;
        inc( liNumberOfScores );
        result := result + '* Math/Science/Engineering Performance Vector, '
          + FloatToStrF(ldBestScore, ffGeneral, 4, 0) + CR_LF;
      end;
      ldBestScore := GetPerfVectorResults( aQTestSet, pvSecurity );
      if ldBestScore > -1 then begin
        ldScoreSum := ldScoreSum + (ldBestScore * ldBestScore);
        ldScoreProduct := ldScoreProduct * ldBestScore;
        inc( liNumberOfScores );
        result := result + '* Security Performance Vector, '
          + FloatToStrF(ldBestScore, ffGeneral, 4, 0) + CR_LF;
      end;
//      ldScoreAverage := sqrt( ldScoreSum );
//      result := result + '**** Resultant Performance Vector: '
//        + FloatToStrF(ldScoreAverage, ffGeneral, 4, 0) + CR_LF;
      ldScoreAverage := power( ldScoreProduct, (1 / liNumberOfScores) );
      result := result + '*** Vector Geometric Mean, '
        + FloatToStrF(ldScoreAverage, ffGeneral, 4, 0) + CR_LF;
    end;
  begin
    result := '';
    ldScoreSum := 0;
    liNumberOfScores := 0;
    lFirstTest := succ( qtFirstTest );
    lLastTest := pred( qtLastTest );
    for lTest := lFirstTest to lLastTest do begin
      if lTest in aQTestSet then begin
        result := result + GetBestResult( lTest, ldBestScore ) + CR_LF;
        ldScoreSum := ldScoreSum + ldBestScore;
        inc( liNumberOfScores );
      end; // if lTest in OFFICIAL_RUN_TESTS
    end; // for lTest := qtFirstTest
    if fOfficialRun then begin
      lstrSuiteResult := '*** COSBI OSMark, ' +
        FloatToStrF(GetCosbiScore( FALSE ), ffGeneral, 4, 0);
      result := result + lstrSuiteResult + CR_LF;
      GenerateVectors;
    end else begin
      ldScoreAverage := ldScoreSum / liNumberOfScores;
      lstrSuiteResult := '*** Average score for all tests, ' +
        FloatToStrF(ldScoreAverage, ffGeneral, 4, 0);
      result := result + lstrSuiteResult + CR_LF;
    end; // if fOfficialRun
  end; //function TOSMarkTestResults.GetBestSuiteResults

  function TOSMarkTestResults.GetPerfVectorResults( aQTestSet : TQTestSet;
                              aPerfVector : TPerformanceVector ): double;
  var
    lTest : TQTests;
    lFirstTest : TQTests;
    lLastTest : TQTests;
    lstrSuiteResult : string;
    ldBestScore, ldScoreSum, ldScoreAverage : double;
    liNumberOfScores : integer;
  begin
    result := -1;
    ldScoreSum := 0;
    liNumberOfScores := 0;
    lFirstTest := succ( qtFirstTest );
    lLastTest := pred( qtLastTest );
    for lTest := lFirstTest to lLastTest do begin
      if lTest in aQTestSet then begin
        ldBestScore := GetBestPerfVectorResult( lTest, aPerfVector );
        if ldBestScore >= 0 then begin
          result := result + ldBestScore;
          inc( liNumberOfScores );
        end; // if
      end; // if lTest in OFFICIAL_RUN_TESTS
    end; // for lTest := qtFirstTest
    result := result / liNumberOfScores;
  end; //function TOSMarkTestResults.GetBestResult
// TOSMarkTestResults ends...........................................................

// TOSMarkTestSuite begins.......................................................

constructor TOSMarkTestSuite.Create( AOwner : TComponent );
begin
  fShowStatus := FALSE;
  fSpinUp := FALSE;
  fProcessIdleTasks := FALSE;
  fDiskDefrag := FALSE;
  fTargetDisk := 'c';
  inherited;
end; // constructor


procedure TOSMarkTestSuite.Initialize( aSystemInfo: TSystemInfo);
begin
  if not assigned( aSystemInfo ) then begin
    raise exception.Create('TOSMarkTestSuite.Initialize: Pass me an instantiated SystemInfo object, you dork!' );
  end; // if
  fSystemInfo := aSystemInfo;
  fOSMarkTestResults := TOSMarkTestResults.Create(self);
  fOSMarkTestResults.Initialize( aSystemInfo );
  fOSMarkTestResults.SystemInfo := fSystemInfo;
  fStopWatch := fSystemInfo.StopWatch;
end; // procedure TOSMarkTestSuite.Initialize;

destructor TOSMarkTestSuite.Destroy;
begin
  FreeAndNil( fOSMarkTestResults );
  inherited;
end; // constructor TOSMarkTestSuite.Create;

procedure TOSMarkTestSuite.SetSystemInfo( Value : TSystemInfo );
begin
  fSystemInfo := Value;
end; // procedure TOSMarkTestSuite.SetSystemInfo( Value : TStopWatch );

procedure TOSMarkTestSuite.OutString( asOutputString : string );
begin
  fOutputString := asOutputString;
  if assigned( OnOutputString ) then OnOutputString( self );
  Application.ProcessMessages;
end; //procedure TOSMarkTestSuite.OutputString( var asOutputString : string )

procedure TOSMarkTestSuite.HandleDiskDefrag;
var
  i : integer;
  lfrmStatus : TfrmStatus;
begin
  // defrag drive:
  if fDiskDefrag then begin
    OutString( 'Defragging drive ' + fTargetDisk + ':...' );
    lfrmStatus := TfrmStatus.Create( nil );
    try
      lfrmStatus.Clear;
      lfrmStatus.Show;
      lfrmStatus.AddLine('Please be patient.');
      lfrmStatus.AddLine('This could take awhile.');
      lfrmStatus.AddLine('Defragmenting drive five times...');
      for i := 1 to 5 do begin
        lfrmStatus.AddLine('Starting defrag #' + IntToStr( i ) );
        RunDiskDefrag( fTargetDisk, 1 );
        lfrmStatus.AddLine('Finished defrag #:' + IntToStr( i ) );
      end; // for
    finally
      lfrmStatus.Release;
      freeAndNil( lfrmStatus );
    end; // try..finally
  end; // if
end; // procedure TOSMarkTestSuiteHandleDiskDefrag;

procedure TOSMarkTestSuite.HandleProcessIdleTasks;
var
  lfrmStatus : TfrmStatus;
begin
  // process idle task:
  if fProcessIdleTasks then begin
    OutString( 'Forcing XP to process idle tasks... ' );
    lfrmStatus := TfrmStatus.Create( nil );
    try
      lfrmStatus.Clear;
      lfrmStatus.Show;
      lfrmStatus.AddLine('Please be patient.');
      lfrmStatus.AddLine('Processing idle tasks...');
      RunProcessIdleTasks;
    finally
      lfrmStatus.Release;
      freeAndNil( lfrmStatus );
    end; // try..finally
  end; // if
end; // procedure TOSMarkTestSuite.HandleProcessIdleTasks;

procedure TOSMarkTestSuite.CheckDriveSpace;
begin
  if qtFileCopy in fSelectedTests then begin
    if not DriveIsBigEnough( fTargetDisk, ONE_BILLION ) then begin
      raise Exception.Create( 'TOSMarkTestSuite.CheckDriveSpace' + ERR_DRIVE_IS_TOO_SMALL );
    end; // if
  end; // if
end; //

procedure TOSMarkTestSuite.RunTheseTests( aQTSet: TQTestSet; aiIterations: integer );
var
  i : integer;
  nbr_tests : integer;
begin
  fSelectedTests := aQTSet;
  nbr_tests := CountTests;
  if nbr_tests = 0 then begin
    showmessage( 'TOSMarkTestSuite.RunTests: You have not selected any tests.');
    exit;
  end; // if

  fIterationFinal := aiIterations;
  fTotalNumberOfTestsToRun := nbr_tests * aiIterations;
  if assigned( OnTestSuiteRunStart ) then OnTestSuiteRunStart( self );

  CheckDriveSpace;
  HandleDiskDefrag;
  HandleProcessIdleTasks;

  OutputResultHeader( aiIterations );
  for i := 1 to aiIterations do begin
    fIteration := i;
    if assigned( OnIterationStart ) then OnIterationStart( self );
    application.ProcessMessages;
    TestSuiteRun( i );
    if assigned( OnIterationEnd ) then OnIterationEnd( self );
    application.ProcessMessages;
  end; // for i...
//  if aiIterations > 1 then begin
    OutputSuiteResultHeader( aiIterations );
    OutString( fOSMarkTestResults.GetBestSuiteResults( aQTSet ) );
//  end; // if
  OutputResultFooter;
  if assigned( OnTestSuiteRunEnd ) then OnTestSuiteRunEnd( self );
end; // procedure TOSMarkTestSuite.RunTests

procedure TOSMarkTestSuite.OutputResultHeader( aiIterations : integer );
begin
  OutString('');
  if fSpinUp then begin
    OutString('>> "Spin up" CPU selected');
  end; // if
  if aiIterations = 1 then
    OutString('*** Test Starts (' + IntToStr(aiIterations) + ' iteration) ***')
  else
    OutString('*** Test Starts (' + IntToStr(aiIterations) + ' iterations) ***');
  OutString('');
  OutString('Test, Score, Time(s)');
  OutString('====  =====  =======');
end; // procedure TOSMarkTestSuite.OutputResultHeader;

procedure TOSMarkTestSuite.OutputSuiteResultHeader( aiIterations : integer );
begin
  OutString('');
  OutString('**** Best Results for ' + IntToStr(aiIterations) + ' iteration(s) ****');
  OutString('');
  OutString('Test, Score, Time(s)');
  OutString('====  =====  =======');
end; // procedure TOSMarkTestSuite.OutputResultHeader;

procedure TOSMarkTestSuite.OutputResultFooter;
begin
  OutString('');
  OutString('*** Test Ends ***');
end; // procedure TOSMarkTestSuite.OutputResultFooter;

procedure TOSMarkTestSuite.LogResults( aQuickTest: TTest );
var
  lTestResult : TOSMarkTestResult;
begin
  lTestResult.TestType := aQuickTest.QTestType;
  lTestResult.TestName := aQuickTest.TestName;
  lTestResult.TestTime := aQuickTest.MinTime;
  lTestResult.TestScore := aQuickTest.GetScore(0);
  lTestResult.TestRawScore := aQuickTest.RawScore;
  lTestResult.TestRawUnits := aQuickTest.RawUnits;
  lTestResult.PerformanceVectors := aQuickTest.PerformanceVectors;
  fOSMarkTestResults.AddResult( lTestResult );
end; // procedure TOSMarkTestSuite.LogResult;

function TOSMarkTestSuite.CountTests;
var
  lQuickTest : TQTests;
begin
  result := 0;
  for lQuickTest := qtFirstTest to qtLastTest do begin
    if lQuickTest in fSelectedTests then begin
      inc( result );
    end; // if
  end; // for
end; // procedure TOSMarkTestSuite.CountTests;

procedure TOSMarkTestSuite.RunTest( var aTest : TTest;
                                   aiRunCount : integer );
begin
  with aTest do begin
    StopWatch := fStopWatch;
    if aiRunCount > 0 then RunCount := aiRunCount;
    ShowStatus := fShowStatus;
    SpinUp := fSpinUp;
    fTestName := aTest.TestName;
    if assigned( OnTestBegins ) then OnTestBegins( self );
    application.ProcessMessages;
    Run;
    fTestScore := GetScore(0);
    fTestTime := MinTime;
    if assigned( OnTestEnds ) then OnTestEnds( self );
    application.ProcessMessages;
    OutString( TestName + ', ' + IntToStr(GetScore(0)) + ', ' +
      GetFormattedMinTime );
    LogResults( aTest );
  end; // with
end; // procedure TOSMarkTestSuite.RunTest;

procedure TOSMarkTestSuite.TestSuiteRun( aiRunCount : integer );
var
  lTest : TTest;
begin
  if qt7zip in fSelectedTests then begin
    lTest := T7zipThreadTest.Create;
    try
      lTest.ThreadCount := fThreadCount;
      T7zipThreadTest( lTest ).TargetDrive := fTargetDisk;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qt7zip in fSelectedTests
  if qtAlphaBlend in fSelectedTests then begin
    lTest := TAlphaBlendTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtAlphaBlend in fSelectedTests
  if qtAlphaDots in fSelectedTests then begin
    lTest := TAlphaDotsTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtAlphaDots in fSelectedTests
  if qtBandwidthBP64 in fSelectedTests then begin
    lTest := TBandwidthBP64ThreadTest.Create;
    try
      lTest.ThreadCount := fThreadCount;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtBandwidthBP64
  if qtPi in fSelectedTests then begin
    lTest := TPiTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtPi in fSelectedTests
  if qtCircles in fSelectedTests then begin
    lTest := TFilledCircleTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; //if qtCircles in fSelectedTests
  if qtCppCompiler in fSelectedTests then begin
    lTest := TCppCompilerThreadTest.Create;
    try
      lTest.ThreadCount := fThreadCount;
      TCppCompilerThreadTest( lTest ).TargetDrive := fTargetDisk;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if TCppCompilerThreadTest in fSelectedTests
  if qtDhrystone in fSelectedTests then begin
    lTest := TDhrystone.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtDhrystone in fSelectedTests
  if qtDhrystoneThreads in fSelectedTests then begin
    lTest := TDhrystoneThreadTest.Create;
    try
      lTest.ThreadCount := fThreadCount;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtDhrystoneThreads
  if qtDrawEllipses in fSelectedTests then begin
    lTest := TDrawEllipsesTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtDrawEllipses
  if qtEncryptDecrypt in fSelectedTests then begin
    lTest := TAesThreadTest.Create;
    try
      lTest.ThreadCount := fThreadCount;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtEncryptDecrypt
  if qtFern in fSelectedTests then begin
    lTest := TFernTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtFern in fSelectedTests
  if qtFernThreads in fSelectedTests then begin
    lTest := TFernThreadTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtFernThreads
  if qtFib in fSelectedTests then begin
    lTest := TFibonacciTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtFib in fSelectedTests
  if qtFibThreads in fSelectedTests then begin
    lTest := TFibThreadTest.Create;
    try
      lTest.ThreadCount := fThreadCount;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtFibThreadTest
  if qtFileCopy in fSelectedTests then begin
    lTest := TFileCopyTest.Create;
    try
      TFileCopyTest( lTest ).TargetDrive := fTargetDisk;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtFileCopy
  if qtFPMultiply in fSelectedTests then begin
    lTest := TFPMultiplyTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtFPMultiply
  if qtGogoEncode in fSelectedTests then begin
    lTest := TGogoEncoderTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtMp3Encode
  if qtGrid in fSelectedTests then begin
    lTest := TGridBlastTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtGrid in fSelectedTests
  if qtGridFP in fSelectedTests then begin
    lTest := TGridBlastFPTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtGrid in fSelectedTests
  if qtIdenticalThreads in fSelectedTests then begin
    lTest := TIdenticalThreadTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtIndenticalThreads
  if qtImageResize in fSelectedTests then begin
    lTest := TImageResizeTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtIndenticalThreads
  if qtImageRotate in fSelectedTests then begin
    lTest := TImageRotateTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtIndenticalThreads
  if qtJpgDecode in fSelectedTests then begin
    lTest := TJpgDecodeTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtJpgDecode
  if qtLame in fSelectedTests then begin
    lTest := TLameEncoderThreadTest.Create;
    try
      lTest.ThreadCount := fThreadCount;
      TLameEncoderThreadTest( lTest ).TargetDrive := fTargetDisk;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtLame in fSelectedTests
  if qtDrawLines in fSelectedTests then begin
    lTest := TDrawLinesTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtDrawLines
  if qtLorenz in fSelectedTests then begin
    lTest := TLorenzTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtLorenz
  if qtMaze in fSelectedTests then begin
    lTest := TMazeTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; //if qtMaze in fSelectedTests
  if qtMandelbrotThreads in fSelectedTests then begin
    lTest := TMandelbrotThreadTest.Create;
    try
      lTest.ThreadCount := fThreadCount;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtMandelbrotThreads
  if qtMazeThreads in fSelectedTests then begin
    lTest := TMazeThreadTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtMazeThreads in fSelectedTests
  if qtMemLatency in fSelectedTests then begin
    lTest := TMemLatencyThreadTest.Create;
    try
      lTest.ThreadCount := fThreadCount;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtMemLatency
  if qtMetaballs in fSelectedTests then begin
    lTest := TMetaballsThreadTest.Create;
    try
      lTest.ThreadCount := fThreadCount;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtMetaballs
  if qtMontMul in fSelectedTests then begin
    lTest := TMontgomeryMultiplierTest.Create;
    try
      lTest.ThreadCount := fThreadCount;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtMontMul in fSelectedTests
  if qtNBody in fSelectedTests then begin
    lTest := TnBodyTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtNBody in fSelectedTests
  if qtNBodyFPU in fSelectedTests then begin
    lTest := TnBodyTest.Create;
    try
      TnBodyTest( lTest ).NBodyTestType := nbttFPU;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtNBodyFPU in fSelectedTests
  if qtNBodyOpenGL in fSelectedTests then begin
    lTest := TNBodyOpenGL.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtNBodyOpenGL
  if qtNBodySSE2 in fSelectedTests then begin
    lTest := TnBodyTest.Create;
    try
      TnBodyTest( lTest ).NBodyTestType := nbttSSE2;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtNBodySSE2 in fSelectedTests
  if qtNBodySSE2Scalar in fSelectedTests then begin
    lTest := TnBodyTest.Create;
    try
      TnBodyTest( lTest ).NBodyTestType := nbttSSE2Scalar;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtNBodySSE2Scalar in fSelectedTests
  if qtNBodySSE3 in fSelectedTests then begin
    lTest := TnBodyTest.Create;
    try
      TnBodyTest( lTest ).NBodyTestType := nbttSSE3;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtNBodySSE3 in fSelectedTests
  if qtOgg in fSelectedTests then begin
    lTest := TOggEncoderThreadTest.Create;
    try
      lTest.ThreadCount := fThreadCount;
      TOggEncoderThreadTest( lTest ).TargetDrive := fTargetDisk;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtOgg in fSelectedTests
  if qtOrthogonalThreads in fSelectedTests then begin
    lTest := TOrthogonalThreadTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtOrthogonalThreads
  if qtPiThreads in fSelectedTests then begin
    lTest := TPiThreadTest.Create;
    try
      lTest.ThreadCount := fThreadCount;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtPiThreads
  if qtPlotLines in fSelectedTests then begin
    lTest := TPlotLinesTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtPlotLines in fSelectedTests
  if qtPlotTrig in fSelectedTests then begin
    lTest := TTrigTest.Create;
    try
      RunTest( lTest, aiRunCount  );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtPlotTrig in fSelectedTests
  if qtPlotTrig2 in fSelectedTests then begin
    lTest := TTrigTest2.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtPlotTrig2 in fSelectedTests
  if qtPngOut in fSelectedTests then begin
    lTest := TPngOutThreadTest.Create;
    try
      lTest.ThreadCount := fThreadCount;
      TPngOutThreadTest( lTest ).TargetDrive := fTargetDisk;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtPngOut in fSelectedTests
  if qtRandomDots in fSelectedTests then begin
    lTest := TRandomDotsTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtRandomDots in fSelectedTests
  if qtDrawRectangles in fSelectedTests then begin
    lTest := TDrawRectanglesTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtDrawRectangles
  if qtRichEd in fSelectedTests then begin
    lTest := TRichEditTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtRichEd in fSelectedTests
  if qtSha1 in fSelectedTests then begin
    lTest := TSha1Test.Create;
    try
      lTest.ThreadCount := fThreadCount;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtSha1 in fSelectedTests
  if qtSha256 in fSelectedTests then begin
    lTest := TSha256Test.Create;
    try
      lTest.ThreadCount := fThreadCount;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtSha256 in fSelectedTests
  if qtSse3 in fSelectedTests then begin
    lTest := TSse3ThreadTest.Create;
    try
      lTest.ThreadCount := fThreadCount;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtSse3 in fSelectedTests
  if qtUpx in fSelectedTests then begin
    lTest := TUpxThreadTest.Create;
    try
      lTest.ThreadCount := fThreadCount;
      TUpxThreadTest( lTest ).TargetDrive := fTargetDisk;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtUpx in fSelectedTests
  if qtWebPageLoad in fSelectedTests then begin
    lTest := TWebPageLoad.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtWebPageLoad
  if qtWhetstone in fSelectedTests then begin
    lTest := TWhetstone.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtWhetstone in fSelectedTests
  if qtWhetstoneThreads in fSelectedTests then begin
    lTest := TWhetstoneThreadTest.Create;
    try
      lTest.ThreadCount := fThreadCount;
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtWhetstoneThreads
  if qtZipCompress in fSelectedTests then begin
    lTest := TZipCompressTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtZipCompress
// THIN CLIENT TESTS:
  if qtTCCircles in fSelectedTests then begin
    lTest := TtcFilledCircleTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtTCCircles in fSelectedTests
  if qtTCDrawEllipses in fSelectedTests then begin
    lTest := TtcDrawEllipsesTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtTCDrawEllipses in fSelectedTests
  if qtTCFern in fSelectedTests then begin
    lTest := TtcFernTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtTCFern in fSelectedTests
  if qtTCGrid in fSelectedTests then begin
    lTest := TtcGridBlastTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtTCGrid in fSelectedTests
  if qtTCImageResize in fSelectedTests then begin
    lTest := TtcImageResizeTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtTCImageResize in fSelectedTests
  if qtTCImageRotate in fSelectedTests then begin
    lTest := TtcImageRotateTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtTCImageRotate in fSelectedTests
  if qtTCDrawLines in fSelectedTests then begin
    lTest := TtcDrawLinesTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtTCDrawLines in fSelectedTests
  if qtTCMaze in fSelectedTests then begin
    lTest := TtcMazeTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtTCMaze in fSelectedTests
  if qtTCPlotLines in fSelectedTests then begin
    lTest := TtcPlotLinesTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtTCPlotLines in fSelectedTests
  if qtTCDrawRectangles in fSelectedTests then begin
    lTest := TtcDrawRectanglesTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtTCDrawRectangles in fSelectedTests
  if qtTCRichEd in fSelectedTests then begin
    lTest := TtcRichEditTest.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtTCRichEd in fSelectedTests
  if qtTCWebPageLoad in fSelectedTests then begin
    lTest := TtcWebPageLoad.Create;
    try
      RunTest( lTest, aiRunCount );
    finally
      FreeAndNil( lTest );
    end;
  end; // if qtTCWebPageLoad in fSelectedTests
end; // procedure TestRun( aiRunCount : integer );

procedure TOSMarkTestSuite.OfficialRun;
begin
  fOfficialRun := TRUE;
  fOSMarkTestResults.OfficialRun := TRUE;
  RunTheseTests( OFFICIAL_RUN_TESTS, OFFICIAL_RUN_ITERATIONS );
  fOfficialRun := FALSE;
end; // procedure TOSMarkTestSuite.btnOfficialRunClick(Sender: TObject);

end.
