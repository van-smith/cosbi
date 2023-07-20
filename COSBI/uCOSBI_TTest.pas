unit uCOSBI_TTest;
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
//==============================================================================
// Unit name: uCOSBI_TTest
// Unit description: TTest is the base class for all COSBI tests.
// Author: Van Smith
// Date: March 24, 2003
// OS dependent: Yes: Windows
// Resolution dependent: No, but resolution and color depth may impact scores.
// External unit dependencies: COSBI_Common, COSBI_Status
//==============================================================================
// Modification History
// ------------ -------
// Ver  Date   Who    Description
// ==== ====== ====== ==========================================================
// 1.0 020503 Van     Created.
// 1.1 030324 Van     Created separate unit and updated to support Status
//                    window.
// 1.2 031017 Van     Added TQTests and fQTestType. Increased status display
//                    time. Added OutputForm.
// 1.3 040128 Van     Added a multitude of new tests.  Added raw score and
//                    units.
//==============================================================================

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, COSBI_Common, COSBI_Status, uOutput, uStopWatch;

type

  TQTests = (
    qtFirstTest, // must always be first!
    // the order of these tests will be reflected in how the results are reported.
    qt7zip, qtAlphaBlend, qtAlphaDots, qtBandwidthBP64, qtBandwidthBP64old,
    qtPi, qtCircles, qtCppCompiler, qtDhrystone, qtDhrystoneThreads,
    qtDrawEllipses, qtEncryptDecrypt, qtEncryptDecryptOld, qtFern,
    qtFernThreads, qtFib, qtFibThreads, qtFileCopy, qtFPMultiply, qtGogoEncode,
    qtGrid, qtGridFP, qtIdenticalThreads, qtImageResize, qtImageRotate,
    qtJpgDecode, qtLame, qtLaunchProgramThreads, qtDrawLines, qtLorenz,
    qtMandelbrotThreads, qtMaze, qtMazeThreads, qtMemLatency, qtMemLatencyOld,
    qtMetaballs, qtMontMul, qtNBody, qtNBodyFPU, qtNBodyOpenGL, qtNBodySSE2,
    qtNBodySSE2Scalar, qtNBodySSE3, qtOgg, qtOrthogonalThreads, qtPiThreads,
    qtPlotLines, qtPlotTrig, qtPlotTrig2, qtPngOut, qtRandomDots,
    qtDrawRectangles, qtRichEd, qtSha1, qtSha256, qtSse3, qtStream,
    //thin client tests:
    qtTCCircles, qtTCDrawEllipses, qtTCFern, qtTCGrid, qtTCImageResize,
    qtTCImageRotate, qtTCDrawLines, qtTCMaze, qtTCPlotLines, qtTCDrawRectangles,
    qtTCRichEd, qtTCWebPageLoad,
    // end thin client tests.
    qtUpx, qtWebPageLoad, qtWhetstone, qtWhetstoneThreads, qtZipCompress
    , qtLastTest // must always be last!
    );

  TTestType = (ttCPU, ttFPU, tt3dGraphics, tt2dGraphics, ttDisk,
               ttMemory, ttSystem, ttSSE, ttSSE2, ttSSE3, tt3dNow, ttThreads,
               ttThinClient, ttCryptography);

  TThreadCount = (tcAuto, tc1, tc2, tc4, tc8, tc16, tc32);

  TPerformanceVector = (pvResponsiveness, pvDataSetTransform, pvSecurity,
                        pv3dGaming, pvMathScienceEngineering,
                        pvArtificialIntelligence );

  TPerformanceVectors = set of TPerformanceVector;

  TTest = class( TObject )
  private
  protected
    ffrmStatus  : TfrmStatus;
    fInvalidRun : Boolean;
    fLastTime   : double;
    fMaxNbrOfThreads : integer;
    fMaxTime    : double;
    fMinTime    : double;
    fNbrOfThreadsRunning : integer;
    fOutputForm  : TfrmOutput;
    fQTestType   : TQTests;
    fPerformanceVectors : TPerformanceVectors;
    fRawScore : double;
    fRawUnits : string;
    fRunCount   : integer;
    fReferenceTime : double;
    fReferenceTime2 : double;
    fShowStatus : Boolean;
    fShowStatusTime : integer;
    fSpinUp : Boolean;
    fStopWatch  : TStopWatch;
    fTestDescription : string;
    fTestName   : string;
    fTestType   : TTestType;
    fTestVersion: string;
    fTestAuthor : string;
    fThreadCount : TThreadCount;
    fTotalTime  : double;
    function GetAverageTime : double;
    function GetMaxTime : double;
    function GetMinTime : double;
    function GetTestName : string; Virtual;
    function GetTestDescription : string; Virtual;
    function GetRawScore : double; Virtual;
    function GetRawUnits : string;
    procedure IntializeFPU;
    procedure CreateStatusFull(ai_PauseMilliSec : integer);
    procedure SetRunCount( Value : integer );
  public
    constructor Create; Overload;
    function GetFormattedMinTime : string;
    function GetScore(ai_index : integer) : integer; Virtual;
    procedure Run; Virtual;
    procedure Clear; Virtual;
    procedure BeforeTest; Virtual;
    procedure RunTest; Virtual;
    procedure AfterTest; Virtual;
    property RunCount:   integer     read fRunCount write SetRunCount;
    property TotalTime:  double      read fTotalTime;
    property MaxTime:    double      read GetMaxTime;
    property MinTime:    double      read GetMinTime;
    property LastTime:   double      read fLastTime;
    property PerformanceVectors : TPerformanceVectors
                                  read fPerformanceVectors
                                  write fPerformanceVectors;
    property StopWatch:  TStopWatch  read fStopWatch write fStopWatch;
    property AverageTime  : double   read GetAverageTime;
    property TestName     : string  read GetTestName write fTestName;
    property TestDescription : string read GetTestDescription write fTestDescription;
    property TestVersion  : string read fTestVersion write fTestVersion;
    property TestType     : TTestType read fTestType write fTestType;
    property TestAuthor   : string read fTestAuthor write fTestAuthor;
    property ShowStatus : Boolean read fShowStatus write fShowStatus;
    property ShowStatusTime : integer read fShowStatusTime write fShowStatusTime;
    property QTestType   : TQTests read fQTestType write fQTestType;
    property OutputForm : TfrmOutput read fOutputForm write fOutputForm;
    property RawScore : double read GetRawScore;
    property RawUnits : string read GetRawUnits;
    property SpinUp : Boolean read fSpinUp write fSpinUp;
    property ThreadCount : TThreadCount read fThreadCount write fThreadCount;
  end; // TTest ................................................

const
  PI = 3.141592653589793238462643383279502884197;
  ERR_NO_STOPWATCH = ': You need to give me a StopWatch before I can run!';

implementation

const
  DEFAULT_STATUS_TIME = 3000; // ms
  SPIN_UP_FIB_NUMBER = 39;

// TTest begins............................................................

  constructor TTest.Create;
  begin
    inherited;
    fPerformanceVectors := [];
    fInvalidRun := FALSE;
    fSpinUp := FALSE;
    fThreadCount := tcAuto;
    Clear;
    fShowStatus := TRUE;
    fShowStatusTime := DEFAULT_STATUS_TIME;
    // explcitly initializing the FPU causes performance on the VIA C3 to
    // improve by 150% or more
    IntializeFPU;
  end; // constructor TTest.Create;

  function TTest.GetFormattedMinTime : string;
  begin
    if not fInvalidRun then begin
      result := FloatToStrF(fMinTime,ffNumber,6, 3);
    end else begin
      fMinTime := 0;
      result := FloatToStrF(fMinTime,ffNumber,6, 3);
    end; // if
  end; // function TTest.GetFormattedMinTime : string;

  function TTest.GetMinTime : double;
  begin
    if not fInvalidRun then begin
      result := fMinTime;
    end else begin
      fMinTime := 0;
      result := fMinTime;
    end; // if
  end; // function TTest.GetMinTime : string;

  function TTest.GetMaxTime : double;
  begin
    if not fInvalidRun then begin
      result := fMaxTime;
    end else begin
      fMaxTime := 0;
      result := fMaxTime;
    end; // if
  end; // function TTest.GetMinTime : string;

  function TTest.GetTestName : string;
  begin
    result := fTestName;
  end; // function TTest.GetTestName : string;

  function TTest.GetTestDescription : string;
  begin
    result := fTestDescription;
  end; // function TTest.GetTestName : string;

  procedure TTest.IntializeFPU;
  asm
    finit
    wait
  end;

  procedure TTest.Run;
  begin
    if fStopWatch = nil then begin
      raise Exception.Create( fTestName + ERR_NO_STOPWATCH );
    end;
    try
      BeforeTest;
      fStopWatch.StartTimer;
      RunTest;
      fStopWatch.StopTimer;
      fRunCount := fRunCount + 1;
      fLastTime := fStopWatch.ElapsedTime;
      fTotalTime := fTotalTime + fLastTime;
      if fLastTime > fMaxTime then fMaxTime := fLastTime;
      if fLastTime < fMinTime then fMinTime := fLastTime;
    finally
      AfterTest;
    end;
  end; // procedure TTest.Run;

  function TTest.GetScore( ai_index : integer) : integer;
  begin
    if ai_index = 0 then begin
      result := Round(1000 * ( fReferenceTime / fMinTime ));
    end else begin
      result := Round(1000 * ( fReferenceTime2 / fMinTime ));
    end;
  end;

  procedure TTest.SetRunCount( Value : integer );
  begin
    fRunCount := Value - 1;
  end; // procedure SetRunCount( Value : integer );

  procedure TTest.Clear;
  begin
    // initialize field variables:
    fTotalTime  := 0;
    fMaxTime    := 0;
    fMinTime    := 1e100;
    fLastTime   := 0;
    fRawScore   := 0;
  end; //procedure TTest.Clear;

  function TTest.GetAverageTime;
  begin
    if fRunCount = 0 then begin
      result := 0
    end else begin
      result := fTotalTime / fRunCount;
    end;
  end; //procedure TTest.AverageTime;

  procedure TTest.RunTest;
  begin
    // This is a placeholder for the actual test
  end; //procedure TTest.RunTest;

  procedure TTest.CreateStatusFull(ai_PauseMilliSec : integer);
  var
    ls_TestType : string;
  begin
    ffrmStatus := TfrmStatus.Create( nil );
    ffrmStatus.Show;
    ffrmStatus.Clear;
    ffrmStatus.AddLine('Test name: ' + fTestName);
    ffrmStatus.AddLine('Test description: ' + TestDescription);
    ffrmStatus.AddLine('Test version: ' + fTestVersion);
    ffrmStatus.AddLine('Test authors: ' + fTestAuthor);
    case fTestType of
    tt2dGraphics: ls_TestType := '2d-Graphics';
    tt3dGraphics: ls_TestType := '3d-Graphics';
    ttCPU: ls_TestType := 'CPU';
    ttCryptography: ls_TestType := 'Cryptography';
    ttDisk: ls_TestType := 'Disk';
    ttFPU: ls_TestType := 'FPU';
    ttMemory:  ls_TestType := 'Memory';
    ttSystem: ls_TestType := 'System';
    ttSSE: ls_TestType := 'SSE';
    ttSSE2: ls_TestType := 'SSE2';
    ttSSE3: ls_TestType := 'SSE3';
    tt3dNow: ls_TestType := '3dNow!';
    ttThreads: ls_TestType := 'Threads';
    ttThinClient: ls_TestType := 'Thin Client';
    else
      ls_TestType := 'Unknown';
    end; // case
    ffrmStatus.AddLine('Test type: ' + ls_TestType);
    ffrmStatus.AddLine('Run #: ' + IntToStr( fRunCount + 1) );
    if not fSpinUp then sleep( ai_PauseMilliSec );
  end; // procedure CreateStatusFull

  procedure TTest.BeforeTest;
  begin
    if fShowStatus then begin
      CreateStatusFull( fShowStatusTime );
    end; // if
    if fThreadCount = tcAuto then begin
      fMaxNbrOfThreads := GetNumberOfProcessors;
    end else begin
      case fThreadCount of
        tc1   : fMaxNbrOfThreads := 1;
        tc2   : fMaxNbrOfThreads := 2;
        tc4   : fMaxNbrOfThreads := 4;
        tc8   : fMaxNbrOfThreads := 8;
        tc16  : fMaxNbrOfThreads := 16;
        tc32  : fMaxNbrOfThreads := 32;
      end; // case
    end; // if
    if fSpinUp then Fibonacci(SPIN_UP_FIB_NUMBER);
  end; //procedure TTest.BeforeTest;

  procedure TTest.AfterTest;
  var
    ld_Score : double;
  begin
    // This is a placeholder
    if fShowStatus then begin
      ffrmStatus.AddLine('TEST COMPLETE');
      ld_Score := GetScore( 0 );
      ffrmStatus.AddLine(Format('Score: %6.0n', [ld_Score]));
      if not fSpinUp then sleep( fShowStatusTime ) else Fibonacci(SPIN_UP_FIB_NUMBER);
    end; // if
    freeAndNil( ffrmStatus );
  end; //procedure TTest.AfterTest;

  function TTest.GetRawScore : double;
  begin
    result := fRawScore;
  end; // function TTest.GetRawScore

  function TTest.GetRawUnits : string;
  begin
    result := fRawUnits;
  end; // function TTest.GetRawUnits

// TTest ends...................................................................

end.
