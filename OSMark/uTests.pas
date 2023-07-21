unit uTests;
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
// Unit name: uTests
// Unit description: This unit houses most of the tests used in QuickTests.
// Author: Van Smith
// Date: March 24, 2003
// OS dependent: Yes: Windows
// Resolution dependent: No, but resolution and color depth will impact scores.
// External unit dependencies: COSBI_Common, uCOSBI_TestBaseClass, GR32_Blend,
//   GR32_Image, GR32, Maze
//==============================================================================
// Modification History
// ------------ -------
// Ver  Date   Who    Description
// ==== ====== ====== ==========================================================
// 1.0 020503 Van     Created.
// 1.1 030324 Van     Updated to support Status window and created separate
//                    unit.
// 1.2 030530 Van     Added memory and thread tests, overall score, priority
//                    radio buttons.
// 1.3 031010 Van     Added MazeThread test.
//==============================================================================

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Grids, COSBI_Common, GR32_Blend, GR32_Image,
  GR32, ComCtrls, Gauges, Maze, uCOSBI_TTest, uCOSBI_TGraphicsTest, uOutput,
  jpeg, SHDocVw, OleCtrls, ZipForge, CosbiCpuid;

type

  // Class name: TFibonacciTest
  // Author: Van Smith
  // Date: May 3, 2002
  TFibonacciTest = class( TTest )
  private
    fn    : integer;
  protected
    function GetTestName : string; Override;
    function GetTestDescription : string; Override;
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    procedure Clear; Override;
    function Fibonacci( n : integer ) : integer;
    property n:     integer read fn write fn;
  end; // TFibonacciTest ...............................................

  TArray16MB = array[ 0 .. 4 * ONE_MILLION ] of integer; //16MB array
  TIntScanLineArray = array [0..2400] of integer;

  // Class name: TBandwidthBP64Test
  // Author: Van Smith
  // Date: October 21, 2003
  TBandwidthBP64Test = class( TTest )
  private
  protected
    fDummy : cardinal;
    fA : TDynamicIntegerArray;
    fArraySize : integer;
    function BlockPrefetch64( ai_RepeatCount : integer;
                              ai_ArraySize : integer ): Extended;
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    procedure Clear; Override;
    procedure BeforeTest; Override;
    procedure AfterTest; Override;
  end; // TBandwidthBP64Test ...............................................

  // Class name: TMemLantencyTest
  // Author: Van Smith
  // Date: October 21, 2003
  TMemLatencyTest = class( TTest )
  private
  protected
    //fA            : TArray16MB;
    fA, fB            : TDynamicIntegerArray;
    fArraySize    : integer;
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    procedure Clear; Override;
    procedure BeforeTest; Override;
    procedure AfterTest; Override;
  end; // TMemLatencyTest ...............................................

  // Class name: TTrigTest
  // Author: Van Smith
  // Date: May 3, 2002
  TTrigTest = class( TGraphicsTest )
  private
    fNbrOfLoopSteps : integer;
  protected
  public
    constructor Create; Overload;
    procedure BeforeTest; Override;
    procedure RunTest; Override;
    procedure Clear; Override;
  end; // TTrigTest.......................................................

  // Class name: TTrigTest2
  // Author: Van Smith
  // Date: October 10, 2003
  TTrigTest2 = class( TTrigTest )
  private
  protected
  public
    constructor Create; Overload;
    procedure Clear; Override;
  end; // TTrigTest2.......................................................

  // Class name: TFernTest
  // Author: Van Smith
  // Date: May 3, 2002
  TFernTest = class( TGraphicsTest )
  private
    fPiOver180 : double;
  protected
    procedure DrawFern( x, y, r, theta: double );
  public
    constructor Create; Overload;
    procedure RunTest; Override;
  end; // TFernTest.........................................................

  // Class name: TMazeTest
  // Author: Van Smith
  // Date: May 3, 2002
  TMazeTest = class( TGraphicsTest )
  private
    fMaze : TMaze;
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    procedure BeforeTest; Override;
    procedure AfterTest; Override;
  end; // TMazeTest.........................................................

  // Class name: TPlotLinesTest
  // Author: Van Smith
  // Date: May 3, 2002
  TPlotLinesTest = class( TGraphicsTest )
  private
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
  end; // TPlotLinesTest.........................................................

  // Class name: TRandomDotsTest
  // Author: Van Smith
  // Date: May 3, 2002
  TRandomDotsTest = class( TGraphicsTest )
  private
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
  end; // TRandomDotsTest.........................................................

  // Class name: TFilledCircleTest
  // Author: Van Smith
  // Date: May 3, 2002
  TFilledCircleTest = class( TGraphicsTest )
  private
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
  end; // TFilledCircleTest.....................................................

  TNBodyTestType = (nbttFPU, nbttSSE2, nbttSSE2Scalar, nbttSSE3);
  // Class name: TnBodySSE2Test
  // Author: Van Smith
  // Date: July 2, 2002
  TnBodyTest = class( TGraphicsTest )
  const
    TEST_DESCRIPTION = 'Simulation of 4 bodies interacting via gravity in a plane.';
    TEST_NAME = 'N-Body';
  type
    TBody = packed record
    case byte of
    1: (
      x:   double;
      y:   double;
      Vx:  double;
      Vy:  double;
      Fx:  double;
      Fy:  double;
      m:   double;
      fill: double);    // fill is to ensure 16-byte alignment
    2: (
      xy:  Txmm;
      Vxy: Txmm;
      Fxy: Txmm;
      mm:  Txmm);
    end; // TnBodySSE2Test
  private
    fNBodyTestType : TNBodyTestType;
    procedure RunTestFPU;
    procedure RunTestSSE2;
    procedure RunTestSSE3;
    procedure RunTestSse2Scalar;
    procedure SetNBodyTestType( Value : TNBodyTestType );
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    property NBodyTestType : TNBodyTestType read fNBodyTestType write SetNBodyTestType;
  end; // TnBodyTest.....................................................

  // Class name: TGridBlastTest
  // Author: Van Smith
  // Date: May 3, 2002
  TGridBlastTest = class( TGraphicsTest )
  private
    fGrid : TStringGrid;
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    procedure BeforeTest; Override;
    procedure AfterTest; Override;
    property Grid : TStringGrid read fGrid write fGrid;
  end; // TGridBlastTest.....................................................

  // Class name: TGridBlastFPTest
  // Author: Van Smith
  // Date: October 21, 2003
  TGridBlastFPTest = class( TGraphicsTest )
  private
    fGrid : TStringGrid;
    fstrInitialValue : string;
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    procedure BeforeTest; Override;
    procedure AfterTest; Override;
    property Grid : TStringGrid read fGrid write fGrid;
  end; // TGridBlastTest.....................................................

  // Class name: TRichEditTest
  // Author: Van Smith
  // Date: May 3, 2002
  TRichEditTest = class( TGraphicsTest )
  private
    fRichEdit   : TRichEdit;
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    procedure BeforeTest; Override;
    procedure AfterTest; Override;
  end; // TRichEditTest.....................................................

  // Class name: TPiTest
  // Author: Van Smith
  // Date: May 3, 2002
  TPiTest = class( TGraphicsTest )
  private
    fMemo   : TMemo;
    function ComputePi(NumDigits: Integer): string;
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    procedure BeforeTest; Override;
    procedure AfterTest; Override;
  end; // TPiTest.....................................................

  // Class name: TFernThread
  // Author: Van Smith
  // Date: May 28, 2003
  TFernThread = class(TThread)
  private
//    Fn: integer;
    Fx, Fy, Fr, Ftheta : double;
    Fcolor : TColor;
    fPaintBox32 : TPaintBox32;
    fPiOver180 : double;
  protected
    procedure Execute; override;
    procedure DrawFern( x, y, r, theta: double );
  public
    constructor Create;
    property x : double read Fx write Fx;
    property y : double read Fy write Fy;
    property r : double read Fr write Fr;
    property theta : double read Ftheta write Ftheta;
    property color : TColor read Fcolor write Fcolor;
    property PaintBox32 : TPaintBox32 read fPaintBox32 write fPaintBox32;
  end; // TFernThread...........................................................

  // Class name: TFernThreadTest
  // Author: Van Smith
  // Date: May 28, 2003
  TFernThreadTest = class( TGraphicsTest )
  private
    fOutputForm  : TForm;
    fFernThread1 : TFernThread;
    fFernThread2 : TFernThread;
    fNbrOfThreadsRunning : integer;
    procedure FernThreadDone(Sender: TObject);
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    procedure BeforeTest; Override;
//    procedure AfterTest; Override;
    property OutputForm : TForm read fOutputForm write fOutputForm;
  end; // TFernThreadTest.....................................................

  // Class name: TMazeThread
  // Author: Van Smith
  // Date: October 8, 2003
  TMazeThread = class(TThread)
  private
    fPaintBox32 : TPaintBox32;
    fMaze : TMaze;
  protected
    procedure Execute; override;
  public
    constructor Create;
    property PaintBox32 : TPaintBox32 read fPaintBox32 write fPaintBox32;
  end; // TFernThread...........................................................

  // Class name: TMazeThreadTest
  // Author: Van Smith
  // Date: October 8, 2003
  TMazeThreadTest = class( TGraphicsTest )
  private
    fOutputForm1  : TfrmOutput;
    fOutputForm2  : TfrmOutput;
    fMazeThread1 : TMazeThread;
    fMazeThread2 : TMazeThread;
    fNbrOfThreads : integer;
    fNbrOfThreadsRunning : integer;
    procedure MazeThreadDone(Sender: TObject);
  protected
  public
    constructor Create; Overload;
    procedure BeforeTest; Override;
    procedure AfterTest; Override;
    procedure RunTest; Override;
    property OutputForm1 : TfrmOutput read fOutputForm1 write fOutputForm1;
    property OutputForm2 : TfrmOutput read fOutputForm2 write fOutputForm2;
    property NbrOfThreads : integer read fNbrOfThreads write fNbrOfThreads;
  end; // TFernThreadTest.....................................................

  // Class name: TPiThread
  // Author: Van Smith
  // Date: January 16, 2004
  TPiThread = class(TThread)
  private
    Fn: integer;
  protected
    procedure execute; override;
    function ComputePi(NumDigits : integer): string;
  public
    constructor Create(n: integer);
  end; // TPiThread.............................................................

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

  // Class name: TOrthogonalThreadTest
  // Author: Van Smith
  // Date: January 16, 2004
  TOrthogonalThreadTest = class( TTest )
  private
    fNbrOfThreadsRunning : integer;
    fFibThread  : TFibThread;
    fPiThread   : TPiThread;
    fFibThreadRunning : Boolean;
    fPiThreadRunning : Boolean;
    procedure FibThreadDone(Sender: TObject);
    procedure PiThreadDone(Sender: TObject);
  protected
  public
    constructor Create; Overload;
    procedure BeforeTest; Override;
    procedure AfterTest; Override;
    procedure RunTest; Override;
  end; // TOrthogonalThreadTest.................................................

{ TSortThread } // this sort code originated from a Borland Delphi example
//  PSortArray = ^TSortArray;
//  TSortArray =  array[ 0 .. 5*ONE_MILLION ] of Integer;

  TSortThread = class(TThread)
  private
    FSortArray : array of integer; //TSortArray;   //PSortArray;
    FSize : Integer;
  protected
    procedure Execute; override;
    procedure Sort(var A: array of integer); virtual; abstract;
  public
    procedure RandomizeArray(var SortArray: array of integer);
    constructor Create( aiRandomSeed : integer );
    destructor Destroy; override;
  end;

{ TQuickSort } // this sort code originated from an Borland Delphi example

  TQuickSort = class(TSortThread)
  protected
    procedure Sort(var A: array of integer); override;
  end;

  // Class name: TIdenticalThreadTest
  // Author: Van Smith
  // Date: January 16, 2004
  TIdenticalThreadTest = class( TTest )
  private
    fNbrOfThreadsRunning : integer;
    fQuickSortThread1  : TQuickSort;
    fQuickSortThread2  : TQuickSort;
  protected
    procedure ThreadDone1(Sender: TObject);
    procedure ThreadDone2(Sender: TObject);
  public
    constructor Create; Overload;
    destructor Destroy; override;
    procedure BeforeTest; Override;
    procedure RunTest; Override;
  end; // TOrthogonalThreadTest.................................................

  // Class name:  TGogoEncoderTest
  // Author: Van Smith
  // Date: January 23, 2004
  // Renamed from TMp3EncoderTest by Van on Sept. 14, 2005
  TGogoEncoderTest = class( TTest )
  private
    // we don't need any private field variables or procedures
  protected
    // ditto
  public
    constructor Create; Overload;
    procedure RunTest; Override;
  end; // TGogoEncoderTest.......................................................

  // Class name: TJpgDecodeTest
  // Author: Van Smith
  // Date: January 23, 2004
  TJpgDecodeTest = class( TGraphicsTest )
  private
    // we don't need any private field variables or procedures
  protected
    fImage : TImage; //we will use Delphi's Image component to render the JPG files.
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    procedure BeforeTest; Override;
    procedure AfterTest; Override;
  end; // TJpgDecodeTest....................................................

  // Class name: TImageRotateTest
  // Author: Van Smith
  // Date: January 23, 2004
  TImageRotateTest = class( TGraphicsTest )
  private
    // we don't need any private field variables or procedures
  protected
    fImage32 : TImage32; //Image32 is more flexible than the TImage component
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    procedure BeforeTest; Override;
    procedure AfterTest; Override;
  end; // TImageRotateTest....................................................

  // Class name: TImageResizeTest
  // Author: Van Smith
  // Date: January 23, 2004
  TImageResizeTest = class( TGraphicsTest )
  private
    // we don't need any private field variables or procedures
  protected
    fImage32 : TImage32; //Image32 is more flexible than the TImage component
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    procedure BeforeTest; Override;
    procedure AfterTest; Override;
  end; // TImageResizeTest....................................................

  // Class name: TWebPageLoad
  // Author: Van Smith
  // Date: January 26, 2004
  TWebPageLoad = class( TGraphicsTest )
  private
    // we don't need any private field variables or procedures
  protected
    fWebBrowser : TWebBrowser;
    fIsOffline : Boolean;
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    procedure BeforeTest; Override;
    procedure AfterTest; Override;
  end; // TWebPageLoad.........................................................

  // Class name:  TEncryptDecryptTest
  // Author: Van Smith
  // Date: January 23, 2004
  TEncryptDecryptTest = class( TTest )
  private
    // we don't need any private field variables or procedures
  protected
    // ditto
  public
    constructor Create; Overload;
    procedure RunTest; Override;
  end; // TEncryptDecryptTest...................................................

  // Class name:  TZipCompressTest
  // Author: Van Smith
  // Date: January 23, 2004
  TZipCompressTest = class( TTest )
  private
    // we don't need any private field variables or procedures
  protected
    fZipForge : TZipForge;
    fTargetFile : string;
    fSourceDir : string;
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    procedure BeforeTest; Override;
    procedure AfterTest; Override;
  end; // TZipCompressTest......................................................

  // Class name:  TFileCopyTest
  // Author: Van Smith
  // Date: January 27, 2004
  TFileCopyTest = class( TTest )
  private
    // we don't need any private field variables or procedures
  protected
    fTargetDrive : char;
    fTargetFile1 : string;
    fTargetFile2 : string;
    fTargetFile3 : string;
    fTargetFile4 : string;
    fTargetFile5 : string;
    fSourceFile : string;
    fFileSize : extended;
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    procedure BeforeTest; Override;
    procedure AfterTest; Override;
    procedure CopyFileToTarget( as_sourceFile: string; as_targetFile: string );
    procedure CreateSource(as_filename: string);
    function EnoughDriveSpace(achr_Drive : char): Boolean;
    property TargetDrive : char read fTargetDrive write fTargetDrive;
  end; // TFileCopyTest.........................................................

  TLorenzTest = class( TTest )
  private
    // we don't need any private field variables or procedures
  protected
    // ditto
  public
    constructor Create; Overload;
    procedure RunTest; Override;
  end; // TLorenzTest...................................................

  TNBodyOpenGL = class( TTest )
  private
    // we don't need any private field variables or procedures
  protected
    // ditto
  public
    constructor Create; Overload;
    procedure RunTest; Override;
  end; // TNBodyOpenGL...................................................


  // Class name:  TFPMultiplyTest
  // Author: Van Smith
  // Date: January 23, 2004
  TFPMultiplyTest = class( TTest )
  private
    // we don't need any private field variables or procedures
  protected
    fDoubleArray : array[ 1 .. 10 * ONE_KB ] of double;
  public
    constructor Create; Overload;
    procedure BeforeTest; Override;
    procedure RunTest; Override;
  end; // TFPMultiplyTest......................................................

  // Class name: TLaunchProgramThread
  // Author: Van Smith
  // Date: October 4, 2004
  TLaunchProgramThread = class(TThread)
  private
    fProgramCommand: string;
    fIterations : integer;
    fReturnCode : integer;
  protected
    procedure execute; override;
  public
    constructor Create;
    property Iterations : integer read fIterations write fIterations;
    property ProgramCommand : string read fProgramCommand write fProgramCommand;
    property ReturnCode : integer read fReturnCode;
  end; // TLaunchProgramThread..................................................

  // Class name: TLaunchProgramThreadTest
  // Author: Van Smith
  // Date: October 4, 2004
  TLaunchProgramThreadTest = class( TTest )
  private
    fProgramThread1  : TLaunchProgramThread;
    fProgramThread2  : TLaunchProgramThread;
    fProgramThread3  : TLaunchProgramThread;
    fProgramThread4  : TLaunchProgramThread;
    fProgramThread5  : TLaunchProgramThread;
    fProgramThread6  : TLaunchProgramThread;
    fProgramThread7  : TLaunchProgramThread;
    fProgramThread8  : TLaunchProgramThread;
    fProgramThread9  : TLaunchProgramThread;
    fProgramThread10  : TLaunchProgramThread;
    fProgramThread11  : TLaunchProgramThread;
    fProgramThread12  : TLaunchProgramThread;
    fProgramThread13  : TLaunchProgramThread;
    fProgramThread14  : TLaunchProgramThread;
    fProgramThread15  : TLaunchProgramThread;
    fProgramThread16  : TLaunchProgramThread;
    fProgramCommand1 : string;
    fProgramCommand2 : string;
    fProgramCommand3 : string;
    fProgramCommand4 : string;
    fProgramCommand5 : string;
    fProgramCommand6 : string;
    fProgramCommand7 : string;
    fProgramCommand8 : string;
    fProgramCommand9 : string;
    fProgramCommand10 : string;
    fProgramCommand11 : string;
    fProgramCommand12 : string;
    fProgramCommand13 : string;
    fProgramCommand14 : string;
    fProgramCommand15 : string;
    fProgramCommand16 : string;
  protected
    procedure ThreadDone(Sender: TObject);
  public
    constructor Create; Overload;
    destructor Destroy; override;
    procedure BeforeTest; Override;
    procedure RunTest; Override;
    property ProgramCommand1 : string read fProgramCommand1 write fProgramCommand1;
    property ProgramCommand2 : string read fProgramCommand2 write fProgramCommand2;
    property ProgramCommand3 : string read fProgramCommand3 write fProgramCommand3;
    property ProgramCommand4 : string read fProgramCommand4 write fProgramCommand4;
    property ProgramCommand5 : string read fProgramCommand5 write fProgramCommand5;
    property ProgramCommand6 : string read fProgramCommand6 write fProgramCommand6;
    property ProgramCommand7 : string read fProgramCommand7 write fProgramCommand7;
    property ProgramCommand8 : string read fProgramCommand8 write fProgramCommand8;
    property ProgramCommand9 : string read fProgramCommand9 write fProgramCommand9;
    property ProgramCommand10 : string read fProgramCommand10 write fProgramCommand10;
    property ProgramCommand11 : string read fProgramCommand11 write fProgramCommand11;
    property ProgramCommand12 : string read fProgramCommand12 write fProgramCommand12;
    property ProgramCommand13 : string read fProgramCommand13 write fProgramCommand13;
    property ProgramCommand14 : string read fProgramCommand14 write fProgramCommand14;
    property ProgramCommand15 : string read fProgramCommand15 write fProgramCommand15;
    property ProgramCommand16 : string read fProgramCommand16 write fProgramCommand16;
  end; // TLaunchProgramThreadTest..............................................

  // Class name: TPiThreadTest
  // Author: Van Smith
  // Date: December 16, 2004
  TPiThreadTest = class( TTest )
  private
    fPiThread : array[ 1..32 ] of TPiThread;
    procedure ThreadDone(Sender: TObject);
  protected
  public
    constructor Create; Overload;
    procedure BeforeTest; Override;
    function GetScore(ai_index : integer) : integer; override;
    procedure RunTest; Override;
  end; // TPiThreadTest..................................................

  // Class name: TDrawLinesTest
  // Author: Van Smith
  // Date: December 16, 2004
  TDrawLinesTest = class( TGraphicsTest )
  private
    // we don't need any private field variables or procedures
    fNumberOfLines : integer;
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
  end; // TDrawLinesTest....................................................

  // Class name: TDrawEllipsesTest
  // Author: Van Smith
  // Date: December 16, 2004
  TDrawEllipsesTest = class( TGraphicsTest )
  private
    // we don't need any private field variables or procedures
    fNumberOfEllipses : integer;
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
  end; // TDrawEllipsesTest....................................................

  // Class name: TDrawRectanglesTest
  // Author: Van Smith
  // Date: December 16, 2004
  TDrawRectanglesTest = class( TGraphicsTest )
  private
    // we don't need any private field variables or procedures
    fNumberOfRectangles : integer;
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
  end; // TDrawRectanglesTest....................................................

  // Class name: TMandelbrotThread
  // Author: Van Smith
  // Date: December 16, 2004
  TMandelbrotThread = class(TThread)
  private
    FxOffset  : double;
    FyOffset  : double;
    FxScale   : double;
    FyScale   : double;
    Fi        : integer;
    Fy        : integer;
    fStartY   : integer;
    fEndY     : integer;
    FScanline : TIntScanLineArray;
    fWidth    : integer;
    fHeight   : integer;
    fFinished : Boolean;
    fPaintBox32  : TPaintBox32;
    fColor    : integer;
  protected
    fMandelbrotThreadDone: TNotifyEvent;
    procedure execute; override;
    procedure ComputeMandelbrot;
    procedure PlotScanLine;
  public
    constructor Create( aPaintBox32 : TPaintBox32 );
    property i       : integer read Fi write Fi;
    property Color : integer read fColor write fColor;
    property Finished: Boolean read fFinished write fFinished;
    property PaintBox32: TPaintBox32 read fPaintBox32 write fPaintBox32;
    property MandelbrotThreadDone: TNotifyEvent read fMandelbrotThreadDone
                                                write fMandelbrotThreadDone;
    property StartY : integer read fStartY write fStartY;
    property EndY : integer read fEndY write fEndY;
  end; // TMandelbrotThread

  // Class name: TMandelbrotThreadTest
  // Author: Van Smith
  // Date: December 16, 2004
  TMandelbrotThreadTest = class( TGraphicsTest )
  private
    fMandelbrotThread : array[ 1..32 ] of TMandelbrotThread;
    procedure ThreadDone(Sender: TObject);
  protected
  public
    constructor Create; Overload;
    procedure BeforeTest; Override;
    function GetScore(ai_index : integer) : integer; override;
    procedure RunTest; Override;
  end; // TDhrystoneThreadTest..................................................

  // Class name: TFibThreadTest
  // Author: Van Smith
  // Date: December 20, 2004
  TFibThreadTest = class( TTest )
  private
    fFibThread : array[ 1..32 ] of TFibThread;
    procedure ThreadDone(Sender: TObject);
  protected
  public
    constructor Create; Overload;
    procedure BeforeTest; Override;
    function GetScore(ai_index : integer) : integer; override;
    procedure RunTest; Override;
  end; // TPiThreadTest..................................................


  // Class name: TBandwidthBP64Thread
  // Author: Van Smith
  // Date: March 18, 2005
  TBandwidthBP64Thread = class(TThread)
  private
    fDummy : cardinal;
    fA : TDynamicIntegerArray;
    fArraySize : integer;
    function BlockPrefetch64( ai_RepeatCount : integer;
                              ai_ArraySize : integer ): Extended;
  protected
    procedure Execute; override;
  public
    constructor Create; Overload;
//    destructor Destroy; override;
  end; // TBandwidthBP64Thread..................................................

  // Class name: TBandwidthBP64ThreadTest
  // Author: Van Smith
  // Date: March 18, 2005
  TBandwidthBP64ThreadTest = class( TTest )
  private
    fBandwidthBP64Thread : array[ 1..32 ] of TBandwidthBP64Thread;
    procedure ThreadDone(Sender: TObject);
  protected
  public
    constructor Create; Overload;
    procedure BeforeTest; Override;
    function GetScore(ai_index : integer) : integer; override;
    procedure RunTest; Override;
  end; // TBandwidthBP64ThreadTest..................................................

  // Class name: TMemLatencyThread
  // Author: Van Smith
  // Date: March 29, 2005
  TMemLatencyThread = class(TThread)
  private
    fDummy : cardinal;
    fA : TDynamicIntegerArray;
    fB : TDynamicIntegerArray;
    fArraySize : integer;
    procedure MemLatencyTest;
  protected
    procedure Execute; override;
  public
    constructor Create; Overload;
//    destructor Destroy; override;
  end; // TMemLatencyThread..................................................

  // Class name: TMemLatencyThreadTest
  // Author: Van Smith
  // Date: March 29, 2005
  TMemLatencyThreadTest = class( TTest )
  private
    fMemLatencyThread : array[ 1..32 ] of TMemLatencyThread;
    procedure ThreadDone(Sender: TObject);
  protected
  public
    constructor Create; Overload;
    procedure BeforeTest; Override;
    function GetScore(ai_index : integer) : integer; override;
    procedure RunTest; Override;
  end; // TMemLatencyThreadTest..................................................

  // Class name: TSse3ThreadTest
  // Author: Van Smith
  // Date: August 29, 2005
  TSse3ThreadTest = class( TTest )
  private
    fLaunchSse3Thread : array[ 1..32 ] of TLaunchProgramThread;
    procedure ThreadDone(Sender: TObject);
  protected
  public
    constructor Create; Overload;
    procedure BeforeTest; Override;
    function GetScore(ai_index : integer) : integer; override;
    procedure RunTest; Override;
  end; // TSse3ThreadTest..................................................

  // Class name: TMontgomeryMultiplierTest
  // Author: Van Smith
  // Date: August 30, 2005
  TMontgomeryMultiplierTest = class( TTest )
  private
    fLaunchMMThread : array[ 1..32 ] of TLaunchProgramThread;
    procedure ThreadDone(Sender: TObject);
  protected
  public
    constructor Create; Overload;
    procedure BeforeTest; Override;
    function GetScore(ai_index : integer) : integer; override;
    procedure RunTest; Override;
  end; // TMontgomeryMultiplierTest.............................................

  // Class name: TSha1Test
  // Author: Van Smith
  // Date: August 30, 2005
  TSha1Test = class( TTest )
  private
    fLaunchSha1Thread : array[ 1..32 ] of TLaunchProgramThread;
    procedure ThreadDone(Sender: TObject);
  protected
  public
    constructor Create; Overload;
    procedure BeforeTest; Override;
    function GetScore(ai_index : integer) : integer; override;
    procedure RunTest; Override;
  end; // TSha1Test.............................................

  // Class name: TSha256Test
  // Author: Van Smith
  // Date: August 30, 2005
  TSha256Test = class( TTest )
  private
    fLaunchSha256Thread : array[ 1..32 ] of TLaunchProgramThread;
    procedure ThreadDone(Sender: TObject);
  protected
  public
    constructor Create; Overload;
    procedure BeforeTest; Override;
    function GetScore(ai_index : integer) : integer; override;
    procedure RunTest; Override;
  end; // TSha256Test.............................................

  // Class name: TAlphaBlendTest
  // Author: Van Smith
  // Date: August 31, 2005
  TAlphaBlendTest = class( TGraphicsTest )
  private
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
  end; // TAlphaBlendTest.........................................................

  // Class name: TAlphaDotsTest
  // Author: Van Smith
  // Date: August 31, 2005
  TAlphaDotsTest = class( TGraphicsTest )
  private
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
  end; // TAlphaDotsTest.........................................................

  // Class name: TPngOutThreadTest
  // Author: Van Smith
  // Date: May 17, 2005
  // modified: 9/13/2005 (converted to array)
  TPngOutThreadTest = class( TTest )
  private
    fTargetDrive      : char;
    fOutputFileSpec   : string;
    fLaunchPngOutThread : array[ 1..32 ] of TLaunchProgramThread;
    procedure ThreadDone(Sender: TObject);
    procedure DeleteOutputFiles;
  protected
  public
    constructor Create; Overload;
    destructor Destroy; override;
    procedure BeforeTest; Override;
    function GetScore(ai_index : integer) : integer; override;
    procedure RunTest; Override;
    property TargetDrive : char read fTargetDrive write fTargetDrive;
  end; // TPngOutThreadTest.............................................

  // Class name: TUpxThreadTest
  // Author: Van Smith
  // Date: May 18, 2005
  // modified: 9/13/2005 (converted to array)
  TUpxThreadTest = class( TTest )
  private
    fTargetDrive      : char;
    fOutputFileSpec   : string;
    fLaunchUpxThread : array[ 1..32 ] of TLaunchProgramThread;
    procedure ThreadDone(Sender: TObject);
    procedure DeleteOutputFiles;
  protected
  public
    constructor Create; Overload;
    destructor Destroy; override;
    procedure BeforeTest; Override;
    function GetScore(ai_index : integer) : integer; override;
    procedure RunTest; Override;
    property TargetDrive : char read fTargetDrive write fTargetDrive;
  end; // TUpxThreadTest.............................................

  // Class name: TAesThreadTest
  // Author: Van Smith
  // Date: March 17, 2005
  // modified: 9/13/2005 (converted to array)
  TAesThreadTest = class( TTest )
  private
    fLaunchAesThread : array[ 1..32 ] of TLaunchProgramThread;
    procedure ThreadDone(Sender: TObject);
  protected
  public
    constructor Create; Overload;
    destructor Destroy; override;
    procedure BeforeTest; Override;
    function GetScore(ai_index : integer) : integer; override;
    procedure RunTest; Override;
  end; // TAesThreadTest.............................................

  // Class name: T7zipThreadTest
  // Author: Van Smith
  // Date: May 18, 2005
  // modified: 9/13/2005 (converted to array)
  T7zipThreadTest = class( TTest )
  private
    fTargetDrive      : char;
    fOutputFileSpec   : string;
    fLaunch7zipThread : array[ 1..32 ] of TLaunchProgramThread;
    procedure DeleteOutputFiles;
  protected
    procedure ThreadDone(Sender: TObject);
  public
    constructor Create; Overload;
    destructor Destroy; override;
    procedure BeforeTest; Override;
    procedure RunTest; Override;
    property TargetDrive : char read fTargetDrive write fTargetDrive;
    function GetScore(ai_index : integer) : integer; override;
  end; // T7zipThreadTest..............................................

  // Class name: TLameEncoderThreadTest
  // Author: Van Smith
  // Date: September 14, 2005
  TLameEncoderThreadTest = class( TTest )
  private
    fTargetDrive      : char;
    fOutputFileSpec   : string;
    fLaunchLameThread : array[ 1..32 ] of TLaunchProgramThread;
    procedure DeleteOutputFiles;
  protected
    procedure ThreadDone(Sender: TObject);
  public
    constructor Create; Overload;
    destructor Destroy; override;
    procedure BeforeTest; Override;
    procedure RunTest; Override;
    property TargetDrive : char read fTargetDrive write fTargetDrive;
    function GetScore(ai_index : integer) : integer; override;
  end; // TLameEncoderThreadTest..............................................

  // Class name: TOggEncoderThreadTest
  // Author: Van Smith
  // Date: September 14, 2005
  TOggEncoderThreadTest = class( TTest )
  private
    fTargetDrive      : char;
    fOutputFileSpec   : string;
    fLaunchOggThread : array[ 1..32 ] of TLaunchProgramThread;
    procedure DeleteOutputFiles;
  protected
    procedure ThreadDone(Sender: TObject);
  public
    constructor Create; Overload;
    destructor Destroy; override;
    procedure BeforeTest; Override;
    procedure RunTest; Override;
    property TargetDrive : char read fTargetDrive write fTargetDrive;
    function GetScore(ai_index : integer) : integer; override;
  end; // TOggEncoderThreadTest..............................................

  // Class name: TCppCompilerThreadTest
  // Author: Van Smith
  // Date: September 14, 2005
  TCppCompilerThreadTest = class( TTest )
  private
    fOriginalDirectory: string;
    fTargetDrive      : char;
    fOutputFileSpec   : string;
    fLaunchCompilerThread : array[ 1..32 ] of TLaunchProgramThread;
    procedure DeleteOutputFiles;
  protected
    procedure ThreadDone(Sender: TObject);
  public
    constructor Create; Overload;
    destructor Destroy; override;
    procedure AfterTest; Override;
    procedure BeforeTest; Override;
    procedure RunTest; Override;
    property TargetDrive : char read fTargetDrive write fTargetDrive;
    function GetScore(ai_index : integer) : integer; override;
  end; // TCppCompilerThreadTest..............................................

  // Class name: TMetaballsThreadTest
  // Author: Ty Smith, Van Smith
  // Date: September 8, 2007
  TMetaballsThreadTest = class( TTest )
  private
    fLaunchMetaballsThread : array[ 1..32 ] of TLaunchProgramThread;
    procedure ThreadDone(Sender: TObject);
  protected
  public
    constructor Create; Overload;
    destructor Destroy; override;
    procedure BeforeTest; Override;
    function GetScore(ai_index : integer) : integer; override;
    procedure RunTest; Override;
  end; // TMetaballsThreadTest.............................................

  implementation   //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
// <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

// TFibonacciTest begins........................................................

  constructor TFibonacciTest.Create;
  begin
    inherited;
    fTestName   := 'Fibonacci sequence';
    fTestDescription := 'A recursive routine, this test calculates the Fibonacci sequence';
    fTestVersion:= '1.0';
    fTestType   := ttCpu;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 1.211;
    fQTestType := qtFib;
    fPerformanceVectors := [ pvMathScienceEngineering ];
  end;

  function TFibonacciTest.GetTestName : string;
  begin
    result := fTestName + ' (' + intToStr( fn ) + ')';
  end; // GetTestName

  function TFibonacciTest.GetTestDescription : string;
  begin
    result := fTestDescription + ' (' + intToStr( fn ) + ')';
  end; // GetTestName

  function TFibonacciTest.Fibonacci( n : integer ) : integer;
  begin
    if n > 2 then
      result := Fibonacci( n - 1 ) + Fibonacci( n - 2 )
    else
      result := 1;
  end;

  procedure TFibonacciTest.RunTest;
  begin
    Fibonacci( fn );
  end;

  procedure TFibonacciTest.Clear;
  begin
    inherited;
    // initialize field variables:
    fn  := 40;
  end; //procedure TFibonacciTest.Clear;

// TFibonacciTest ends..........................................................

// TTrigTest begins.............................................................

  constructor TTrigTest.Create;
  begin
    inherited;
    fTestName   := 'Trig Curves';
    fTestDescription := 'Plots sine, cosine and tangent curves at every interval';
    fTestVersion:= '1.0';
    fTestType   := ttFpu;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 1.68429;
    fQTestType := qtPlotTrig;
    fPerformanceVectors := [ pvMathScienceEngineering, pv3dGaming ];
  end;

  procedure TTrigTest.BeforeTest;
  begin
    inherited;
    // set canvas scale for output:
    PlotScale( -3.1 * PI, 3.1 * PI, -3, 3 );
    // set loop step size:
    loop_step := 6 * PI / fNbrOfLoopSteps;
  end; // procedure TTrigTest.BeforeTest

  procedure TTrigTest.RunTest;
  const
    X_OFFSET = -3 * PI;
  var
    xx  : double;
    yy  : double;
    n   : integer;
    lbPlotNow : boolean;
  begin
    with fPaintBox32 do begin
      for n := 0 to fNbrOfLoopSteps do begin
        lbPlotNow := n mod fPlotStep = 0;
        xx := X_OFFSET + ( n * loop_step );
        // draw sine curve:
        yy := sin( xx );
        if lbPlotNow then Plot(xx, yy, clRed);
        // plot cosine curve:
        yy := cos( xx );
        if lbPlotNow then Plot(xx, yy, clWhite);
        // plot tangent curve:
        // avoid division by zero error:
        if abs(yy) > 0.00000000000001 then begin
          yy := sin( xx ) / cos( xx ); // tangent
          if lbPlotNow then Plot(xx, yy, clYellow);
        end;
      end; // for
    end; // with
  end; // procedure TTrigTest.RunTest;

  procedure TTrigTest.Clear;
  begin
    inherited;
    // initialize field variables:
    fPlotStep       := 1;
    fNbrOfLoopSteps := 500000;
  end; //procedure TTrigTest.Clear;

// TTrigTest ends...............................................................

// TTrigTest2 begins............................................................

  constructor TTrigTest2.Create;
  begin
    inherited;
    fTestName   := 'Trig Curves2';
    fTestDescription := 'Plots sine, cosine and tangent curves every 1 out of 100 steps';
    fTestVersion:= '1.0';
    fTestType   := ttFpu;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 0.129561;
    fQTestType := qtPlotTrig2;
    fPerformanceVectors := [ pvMathScienceEngineering, pv3dGaming ];
  end;

  procedure TTrigTest2.Clear;
  begin
    inherited;
    // initialize field variables:
    fPlotStep       := 100;
    fNbrOfLoopSteps := 500000;
  end; //procedure TTrigTest.Clear;

// TTrigTest2 ends..............................................................

// TFernTest begins.............................................................

  constructor TFernTest.Create;
  begin
    inherited;
    fTestName   := 'Fern Fractal';
    fTestDescription := 'Calculates and plots two fern fractals';
    fTestVersion:= '1.0';
    fTestType   := ttFpu;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 1.50;
    fQTestType := qtFern;
    fPiOver180 := PI / 180;
    fPerformanceVectors := [ pvMathScienceEngineering, pv3dGaming,
                             pvResponsiveness ];
  end;

{  procedure TFernTest.DrawFern( x, y, r, theta: double );
  const
    F1 = 0.4;
    F2 = 0.85;
    W1 = 35;
    W2 = 5;
  var
    x1, y1, t : double;
  begin
    // this program meassure theta clockwise relative to the y=axis
    with fPaintBox32 do begin
      t := fPiOver180;
      asm
        movsd   xmm0, t
        mulsd   xmm0, theta
        movsd   t,    xmm0
        fld     t
        fld     st(0)
        fsin
      end;
//      t := theta * fPiOver180;
      x1 := x + r * sin( t );
      y1 := y - r * cos( t );
      Canvas.MoveTo(round(x), round(y));
      Canvas.LineTo(round(x1),round(y1));
      if r > 0.5 then begin
        DrawFern(x1, y1, r * F1, theta + W1);
        DrawFern(x1, y1, r * F1, theta - W1);
        DrawFern(x1, y1, r * F2, theta + W2);
      end; // if
    end; // with
  end; // procedure TFernTest.DrawFern
}

  procedure TFernTest.DrawFern( x, y, r, theta: double );
  const
    F1 = 0.4;
    F2 = 0.85;
    W1 = 35;
    W2 = 5;
  var
    x1, y1, t : double;
  begin
    // this program measures theta clockwise relative to the y=axis
    with fPaintBox32 do begin
      t := theta * fPiOver180;
      x1 := x + r * sin( t );
      y1 := y - r * cos( t );
      Canvas.MoveTo(round(x), round(y));
      Canvas.LineTo(round(x1),round(y1));
      if r > 0.5 then begin
        DrawFern(x1, y1, r * F1, theta + W1);
        DrawFern(x1, y1, r * F1, theta - W1);
        DrawFern(x1, y1, r * F2, theta + W2);
      end; // if
    end; // with
  end; // procedure TFernTest.DrawFern

  procedure TFernTest.RunTest;
  begin
    fPaintBox32.Canvas.Pen.Color := clLime;
    DrawFern(100, 130, 160, 100);
    fPaintBox32.Canvas.Pen.Color := clRed;
    DrawFern(10, 500, 160, 60);
  end; // procedure TFernTest.RunTest;
// TFernTest ends...............................................................

// TMazeTest begins.............................................................
  constructor TMazeTest.Create;
  begin
    inherited;
    fTestName   := 'Maze';
    fTestDescription := 'Generates mazes and graphically solves them';
    fTestVersion:= '1.0';
    fTestType   := ttCpu;
    fTestAuthor := 'Van Smith, Chris Yeager';
    fReferenceTime := 5.143;
    fQTestType := qtMaze;
    fPerformanceVectors := [ pvArtificialIntelligence ];
  end;

  procedure TMazeTest.BeforeTest;
  begin
    inherited;
    fMaze := TMaze.Create( fPaintBox32 );
  end; // procedure TMazeTest.BeforeTest

  procedure TMazeTest.RunTest;
  var
    n : integer;
  begin
    RandSeed := -1;
    for n := 1 to 20 do begin
      with fMaze do begin
        Make(250, 250);
        Solve(True);
      end;// with lMaze
    end;
  end; // procedure TMazeTest.RunTest;

  procedure TMazeTest.AfterTest;
  begin
    inherited;
    FreeAndNil( fMaze );
  end; // procedure TMazeTest.BeforeTest
// TMazeTest ends...............................................................

// TPlotLinesTest begins........................................................
  constructor TPlotLinesTest.Create;
  begin
    inherited;
    fTestName   := 'Plot Lines';
    fTestDescription := 'Plots lines a pixel at a time';
    fTestVersion:= '1.0';
    fTestType   := tt2dGraphics;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 5.71459;
    fQTestType := qtPlotLines;
    fPerformanceVectors := [ pvResponsiveness ];
  end;

  procedure TPlotLinesTest.RunTest;
  var
    x : integer;
    y : integer;
  begin
    with fPaintBox32 do begin
      for x := Width downto 0 do
        for y := Height downto 0 do
          Canvas.Pixels[x, y] := clRed;
      for x := Width downto 0 do
        for y := Height downto 0 do
          Canvas.Pixels[x, y] := clBlack;
      Application.ProcessMessages;
      for y := Height downto 0 do
        for x := Width downto 0 do
          Canvas.Pixels[x, y] := clRed;
      for y := Height downto 0 do
        for x := Width downto 0 do
          Canvas.Pixels[x, y] := clBlack;
      Application.ProcessMessages;
      for x := 0 to Width do
        for y := 0 to Height do
          Canvas.Pixels[x, y] := clRed;
      for x := 0 to Width do
        for y := 0 to Height do
          Canvas.Pixels[x, y] := clBlack;
      Application.ProcessMessages;
      for y := 0 to Height do
        for x := 0 to Width do
          Canvas.Pixels[x, y] := clRed;
      for y := 0 to Height do
        for x := 0 to Width do
          Canvas.Pixels[x, y] := clBlack;
    end; // with
  end; // procedure TPlotLinesTest.RunTest;
// TPlotLinesTest ends..........................................................

// TRandomDotsTest begins.......................................................
  constructor TRandomDotsTest.Create;
  begin
    inherited;
    fTestName   := 'Random Dots';
    fTestDescription := 'Plots dots randomly over canvas';
    fTestVersion:= '1.0';
    fTestType   := ttCpu;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 10.7719;
    fQTestType := qtRandomDots;
    fPerformanceVectors := [ pvResponsiveness ];
  end;

  procedure TRandomDotsTest.RunTest;
  var
    x : integer;
    y : integer;
    z : integer;
  begin
    with fPaintBox32 do begin
      // Random dots:
      RandSeed := 1;
      for z := 1 to 10000000 do
      begin
        x := Random(Width);
        y := Random(Height);
        case Random(6) of
          0: Canvas.Pixels[x, y] := clBlack;
          1: Canvas.Pixels[x, y] := clGreen;
          2: Canvas.Pixels[x, y] := clBlue;
          3: Canvas.Pixels[x, y] := clRed;
          4: Canvas.Pixels[x, y] := clWhite;
          5: Canvas.Pixels[x, y] := clYellow;
        end;
      end;
    end; // with
  end; // procedure TRandomDotsTest.RunTest;
// TRandomDotsTest ends.........................................................

// TFilledCircleTest begins.....................................................
  constructor TFilledCircleTest.Create;
  begin
    inherited;
    fTestName   := 'Filled Circles';
    fTestDescription := 'Paints random filled circles';
    fTestVersion:= '1.0';
    fTestType   := tt2dGraphics;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 2.06;
    fQTestType := qtCircles;
    fPerformanceVectors := [ pvResponsiveness ];
  end;

  procedure TFilledCircleTest.RunTest;
  var
    x : integer;
    y : integer;
    z : integer;
    r : integer;
  begin
    with fPaintBox32 do begin
      Canvas.Pen.Color := clRed;
      Canvas.Brush.Color := clBlack;
      Canvas.Brush.Style := bsSolid;
      RandSeed := 1;
      for z := 1 to 10000 do
      begin
        x := Random(Width);
        y := Random(Height);
        r := Random(Height div 2);
        case Random(6) of
          0: Canvas.Brush.Color := clBlack;
          1: Canvas.Brush.Color := clGreen;
          2: Canvas.Brush.Color := clBlue;
          3: Canvas.Brush.Color := clRed;
          4: Canvas.Brush.Color := clWhite;
          5: Canvas.Brush.Color := clYellow;
        end;
        Canvas.Ellipse(x - r, y - r, x + r, y + r);
        //Canvas.Pixels[x, y] := clWhite;
      end;
    end; // with
  end; // procedure TFilledCircleTest.RunTest;
// TFilledCircleTest ends.......................................................

// TGridBlastTest begins........................................................
  constructor TGridBlastTest.Create;
  begin
    inherited;
    fTestName   := 'Grid Blast';
    fTestDescription := 'Populates a grid and iterates through data';
    fTestVersion:= '1.0';
    fTestType   := ttSystem;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 1.18;
    fQTestType := qtGrid;
    fPerformanceVectors := [ pvResponsiveness ];
  end;

  procedure TGridBlastTest.RunTest;
  const
    ITERATIONS = 30;
  var
    li_row      : integer;
    li_column   : integer;
    li_repeat   : integer;
    li_temp     : integer;
    x, y, z     : integer;

    procedure InitGrid;
    var
      li_row      : integer;
      li_column   : integer;
    begin
      with fGrid do begin
        for li_row := 1 to RowCount - 1 do begin
          for li_column := 1 to ColCount - 1 do begin
            Cells[li_column, li_row] := '1';
          end;
        end;
      end;
      Application.ProcessMessages;
    end;

  begin
    with fGrid do begin
      Visible := TRUE;
      RowCount := 81;
      ColCount := 21;
      InitGrid;
      // sum
      for li_repeat := 1 to ITERATIONS do begin
        for li_row := 1 to RowCount - 1 do begin
          for li_column := 1 to ColCount - 1 do begin
            li_temp := StrToInt(Cells[li_column, li_row]) + 1;
            Cells[li_column, li_row] := IntToStr( li_temp );
          end;
        end;
        Application.ProcessMessages;
      end;
      // subtract
      for li_repeat := 1 to ITERATIONS do begin
        for li_row := 1 to RowCount - 1 do begin
          for li_column := 1 to ColCount - 1 do begin
            li_temp := StrToInt(Cells[li_column, li_row]) - 1;
            Cells[li_column, li_row] := IntToStr( li_temp );
          end;
        end;
        Application.ProcessMessages;
      end;
      InitGrid;
      // multiply
      for li_repeat := 1 to ITERATIONS do begin
        for li_row := 1 to RowCount - 1 do begin
          for li_column := 1 to ColCount - 1 do begin
            li_temp := StrToInt(Cells[li_column, li_row]) * 2;
            Cells[li_column, li_row] := IntToStr( li_temp );
          end;
        end;
        Application.ProcessMessages;
      end;
      // integer divide
      for li_repeat := 1 to ITERATIONS do begin
        for li_row := 1 to RowCount - 1 do begin
          for li_column := 1 to ColCount - 1 do begin
            li_temp := StrToInt(Cells[li_column, li_row]) div 2;
            Cells[li_column, li_row] := IntToStr( li_temp );
          end;
        end;
        Application.ProcessMessages;
      end;
    end; // with
    fGrid.Visible := FALSE;
  end; // procedure TGridBlastTest.RunTest;

  procedure TGridBlastTest.BeforeTest;
  begin
    inherited;
    fGrid := TStringGrid.Create( fOutputForm );
    fGrid.Parent := fOutputForm;
    fGrid.BringToFront;
    fGrid.Align := alClient;
    fGrid.SetFocus;
    fGrid.ColCount := 20;
    fGrid.DefaultColWidth := 48;
    fGrid.DefaultRowHeight := 16;
    fGrid.FixedCols := 1;
    fGrid.FixedRows := 1;
    fGrid.RowCount := 1000;
    fGrid.ScrollBars := ssBoth;
  end; // procedure TGridBlastTest.BeforeTest

  procedure TGridBlastTest.AfterTest;
  begin
    inherited;
    FreeAndNil( fGrid );
  end; // procedure TGridBlastTest.BeforeTest
// TGridBlastTest ends..........................................................

// TGridBlastFPTest begins........................................................
  constructor TGridBlastFPTest.Create;
  begin
    inherited;
    fTestName   := 'Grid Blast: Floating Point';
    fTestDescription := 'Populates a grid with floating point data and performs various calculations.';
    fTestVersion:= '1.0';
    fTestType   := ttFPU;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 1.26;
    fQTestType := qtGridFP;
    fPerformanceVectors := [ pvResponsiveness, pvMathScienceEngineering ];
  end;

  procedure TGridBlastFPTest.RunTest;
  const
    ITERATIONS = 30;
  var
    li_row      : integer;
    li_column   : integer;
    li_repeat   : integer;
    ld_temp     : double;

    procedure InitGrid;
    var
      li_row      : integer;
      li_column   : integer;
    begin
      with fGrid do begin
        for li_row := 1 to RowCount - 1 do begin
          for li_column := 1 to ColCount - 1 do begin
            Cells[li_column, li_row] := fstrInitialValue;
          end;
        end;
      end;
      Application.ProcessMessages;
    end;

  begin
    with fGrid do begin
      Visible := TRUE;
      RowCount := 81;
      ColCount := 21;
      InitGrid;
      // sum
      for li_repeat := 1 to ITERATIONS do begin
        for li_row := 1 to RowCount - 1 do begin
          for li_column := 1 to ColCount - 1 do begin
            ld_temp := StrToFloat(Cells[li_column, li_row]) + 1.01;
            Cells[li_column, li_row] := FloatToStr( ld_temp );
          end;
        end;
        Application.ProcessMessages;
      end;
      // subtract
      for li_repeat := 1 to ITERATIONS do begin
        for li_row := 1 to RowCount - 1 do begin
          for li_column := 1 to ColCount - 1 do begin
            ld_temp := StrToFloat(Cells[li_column, li_row]) - 1.01;
            Cells[li_column, li_row] := FloatToStr( ld_temp );
          end;
        end;
        Application.ProcessMessages;
      end;
      InitGrid;
      // multiply
      for li_repeat := 1 to ITERATIONS do begin
        for li_row := 1 to RowCount - 1 do begin
          for li_column := 1 to ColCount - 1 do begin
            ld_temp := StrToFloat(Cells[li_column, li_row]) * 2.01;
            Cells[li_column, li_row] := FloatToStr( ld_temp );
          end;
        end;
        Application.ProcessMessages;
      end;
      // divide
      for li_repeat := 1 to ITERATIONS do begin
        for li_row := 1 to RowCount - 1 do begin
          for li_column := 1 to ColCount - 1 do begin
            ld_temp := StrToFloat(Cells[li_column, li_row]) / 2.01;
            Cells[li_column, li_row] := FloatToStr( ld_temp );
          end;
        end;
        Application.ProcessMessages;
      end;
      Visible := FALSE;
    end; // with
  end; // procedure TGridBlastTest.RunTest;

  procedure TGridBlastFPTest.BeforeTest;
  begin
    inherited;
    fGrid := TStringGrid.Create( fOutputForm );
    fGrid.Parent := fOutputForm;
    fGrid.BringToFront;
    fGrid.Align := alClient;
    fGrid.SetFocus;
    fGrid.ColCount := 20;
    fGrid.DefaultColWidth := 48;
    fGrid.DefaultRowHeight := 16;
    fGrid.FixedCols := 1;
    fGrid.FixedRows := 1;
    fGrid.RowCount := 1000;
    fGrid.ScrollBars := ssBoth;
    fstrInitialValue := FloatToStr( 1.01 );
  end; // procedure TGridBlastTest.BeforeTest

  procedure TGridBlastFPTest.AfterTest;
  begin
    inherited;
    FreeAndNil( fGrid );
  end; // procedure TGridBlastTest.BeforeTest
// TGridBlastFPTest ends..........................................................

// TRichEditTest begins.........................................................
  constructor TRichEditTest.Create;
  begin
    inherited;
    fTestName   := 'RichEdit';
    fTestDescription := 'Performs common text editing tasks. The RichEdit control is used throughout Windows and is the basis for WordPad and some word processors.';
    fTestVersion:= '1.0';
    fTestType   := ttSystem;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 6.51626;
    fQTestType := qtRichEd;
    fPerformanceVectors := [ pvResponsiveness ];
  end; // constructor TRichEditTest.Create;

  procedure TRichEditTest.RunTest;
  const
    CR_LF = #13#10;
  var
    i, j : integer;

    function RandomWord(abol_Uppercase : boolean): string;
    var
      WordLength : integer;
      i           : integer;
      randomChar  : integer;
    begin
      result := '';
      WordLength := Random(11) + 1;
      for i := 1 to WordLength do begin
        randomChar := Random(25);
        if (i = 1) and abol_Uppercase then begin
          result := result + chr(randomChar + 65);
        end else begin
          result := result + chr(randomChar + 97);
        end; // if
      end; // for
    end; // function RandomWord

    function RandomSentence: string;
    var
      SentenceLength : integer;
      i              : integer;
    begin
      result := '';
      SentenceLength := random(10) + 1;
      for i := 1 to SentenceLength do begin
        result := result + RandomWord(i = 1);
        if i < SentenceLength then begin
          result := result + ' ';
        end else begin
          result := result + '.';
        end; // if
      end; // for
    end; // function RandomSentence

    function RandomParagraph: string;
    var
      ParagraphLength : integer;
      i               : integer;
    begin
      result := '';
      ParagraphLength := random(16) + 1;
      for i := 1 to ParagraphLength do begin
        result := result + RandomSentence;
        if i <> 1 then begin
          result := result + '  ';
        end else begin
          result := result + CR_LF + CR_LF;
        end; // if
      end; // for
    end; // function RandomParagraph

    function RandomChapter: string;
    var
      ChapterLength   : integer;
      i               : integer;
    begin
      result := '';
      ChapterLength := random(16) + 4;
      for i := 1 to ChapterLength do begin
        fRichEdit.Lines.Add(RandomParagraph);
        fRichEdit.Lines.Add(CR_LF);
        fRichEdit.Lines.Add(CR_LF);
        Application.ProcessMessages;
      end; // for
    end; // function RandomChapter

  begin
    randseed := 1;
    with fRichEdit do begin
      Visible := TRUE;
      for i := 1 to 100 do begin
        Paragraph.Numbering := nsNone;
        SelAttributes.Size := 20;
        SelAttributes.Style := [fsBold];
        Paragraph.Alignment := taCenter;
        Lines.Add('Chapter ' + inttostr( i ));
        Paragraph.Alignment := taLeftJustify;
        SelAttributes.Size := 12;
        SelAttributes.Style := [];
        RandomChapter;
        Paragraph.Numbering := nsBullet;
        for j := 1 to 10 do begin
          SelAttributes.Style := [fsItalic];
          Lines.Add(RandomSentence);
        end; //for
      end; //for
    end; // with
  end; // procedure TRichEditTest.RunTest;

  procedure TRichEditTest.BeforeTest;
  begin
    inherited;
    fRichEdit := TRichEdit.Create(fOutputForm);
    fRichEdit.Parent := fOutputForm;
    fRichEdit.BringToFront;
    fRichEdit.Align := alClient;
    fRichEdit.Lines.Clear;
    fRichEdit.SetFocus;
    fRichEdit.DefAttributes.Name := 'Times New Roman';
    fRichEdit.DefAttributes.Size := 12;
  end; // procedure TRichEditTest.BeforeTest

  procedure TRichEditTest.AfterTest;
  begin
    inherited;
    fRichEdit.ClearUndo;
    fRichEdit.Lines.Clear;
    fRichEdit.Clear;
    if assigned( fRichEdit ) then fRichEdit.Free;
    fRichEdit := NIL;
  end; // procedure TRichEditTest.BeforeTest
// TRichEditTest ends...............................................................

// TPiTest begins.............................................................
  constructor TPiTest.Create;
  begin
    inherited;
    fTestName   := 'PiTest';
    fTestDescription := 'Calculates pi using Ray Lischner''s Delphi algorithm. See http://www.tempest-sw.com for a look at some of Ray Lischner''s famous books.';
    fTestVersion:= '1.0';
    fTestType   := ttSystem;
    fTestAuthor := 'Ray Lischner, Van Smith';
    fReferenceTime := 3.46;
    fQTestType := qtPi;
    fPerformanceVectors := [ pvMathScienceEngineering ];
  end;

  function TPiTest.ComputePi(NumDigits: Integer): string;
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

  procedure TPiTest.RunTest;
  var
    i           : integer;
    li_PiDigits : integer;
  begin
    with fMemo do begin
      Visible := TRUE;
      for i := 1 to 5 do begin
        li_PiDigits := 1000 * i;
        Lines.Add(' ');
        Lines.Add( 'ComputePi( ' + intToStr(li_PiDigits) + ' ) = ' );
        Application.ProcessMessages;
        Lines.Add( ComputePi( li_PiDigits ) );
        Application.ProcessMessages;
      end; //for
    end; // with
  end; // procedure TPiTest.RunTest;

  procedure TPiTest.BeforeTest;
  begin
    inherited;
    fMemo := TMemo.Create(fOutputForm);
    fMemo.Parent := fOutputForm;
    fMemo.BringToFront;
    fMemo.Align := alClient;
    fMemo.Lines.Clear;
    fMemo.SetFocus;
    fMemo.Font.Name :=  'Times New Roman';
    fMemo.Font.Size := 12;
  end; // procedure TPiTest.BeforeTest

  procedure TPiTest.AfterTest;
  begin
    inherited;
    fMemo.Clear;
    FreeAndNil( fMemo );
  end; // procedure TPiTest.BeforeTest
// TPiTest ends...............................................................

//TFernThread begins............................................................
  constructor TFernThread.Create;
  begin
    FreeOnTerminate := TRUE;
    inherited Create( TRUE );
    Priority := tpNormal;
    fPiOver180 := PI / 180;
  end; // constructor TFibThread.Create(n: integer);

  procedure TFernThread.DrawFern( x, y, r, theta: double );
  const
    F1 = 0.4;
    F2 = 0.85;
    W1 = 35;
    W2 = 5;
  var
    x1, y1, t : double;
  begin
    // this program measures theta clockwise relative to the y=axis
    with fPaintBox32 do begin
      t := theta * fPiOver180;
      x1 := x + r * sin( t );
      y1 := y - r * cos( t );
      Canvas.Lock;
      Canvas.Pen.Color := fColor;
      Canvas.MoveTo(round(x), round(y));
      Canvas.LineTo(round(x1),round(y1));
      Canvas.Refresh;
      Canvas.Unlock;
      if r > 0.5 then begin
        DrawFern(x1, y1, r * F1, theta + W1);
        DrawFern(x1, y1, r * F1, theta - W1);
        DrawFern(x1, y1, r * F2, theta + W2);
      end; // if
    end; // with
  end; // procedure TFernThread.DrawFern

  procedure TFernThread.execute;
  begin
    DrawFern( Fx, Fy, Fr, Ftheta );
  end;
//TFernThread ends..............................................................

// TFernThreadTest begins.......................................................
  constructor TFernThreadTest.Create;
  begin
    inherited;
    fTestName   := 'Fern Fractal thread test';
    fTestDescription := 'Calculates and simultaneously plots multiple fern fractals';
    fTestVersion:= '1.0';
    fTestType   := ttThreads;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 3.30302;
    fQTestType := qtFernThreads;
    fPerformanceVectors := [ pvMathScienceEngineering ];
  end;

  procedure TFernThreadTest.BeforeTest;
  begin
    inherited;
    // thread 1:
    fFernThread1 := TFernThread.Create;
    fFernThread1.OnTerminate := FernThreadDone;
    fFernThread1.PaintBox32 := fPaintBox32;
    fFernThread1.color := clLime;
    fFernThread1.x := 100;
    fFernThread1.y := 130;
    fFernThread1.r := 160;
    fFernThread1.theta := 100;
    // thread 2:
    fFernThread2 := TFernThread.Create;
    fFernThread2.OnTerminate := FernThreadDone;
    fFernThread2.PaintBox32 := fPaintBox32;
    fFernThread2.color := clRed;
    fFernThread2.x := 10;
    fFernThread2.y := 500;
    fFernThread2.r := 160;
    fFernThread2.theta := 60;

  end; // procedure TRichEditTest.BeforeTest

  procedure TFernThreadTest.RunTest;
  begin
    fFernThread1.Resume;
    inc( fNbrOfThreadsRunning );
    fFernThread2.Resume;
    inc( fNbrOfThreadsRunning );
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 100 );
      application.ProcessMessages;
    end;
  end; // procedure TFernThreadTest.RunTest;

  procedure TFernThreadTest.FernThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    if fNbrOfThreadsRunning = 0 then begin
      fPaintBox32.Canvas.Refresh;
    end;
    application.ProcessMessages;
  end; // procedure TFernThreadTest.FernThreadDone;

// TFernThreadTest ends.........................................................

// TMazeThread begins...........................................................

  // Class name: TMazeThread
  // Author: Van Smith
  // Date: October 8, 2003
  constructor TMazeThread.Create;
  begin
    FreeOnTerminate := TRUE;
    RandSeed := -1;
    inherited Create( TRUE ); // creates suspended
    Priority := tpNormal;
  end; // constructor TMazeThread.Create(n: integer);

  procedure TMazeThread.execute;
  var
    n : integer;
  begin
    fMaze := TMaze.Create( fPaintBox32 );
    try
      for n := 1 to 20 do begin
        with fMaze do begin
          Make(125, 250);
          Solve(True);
        end;// with lMaze
      end;
    finally
      FreeAndNil( fMaze );
    end; // try
  end; // procedure TMazeThread.execute;
// TMazeThread ends.............................................................

// TMazeThreadTest begins.......................................................
  constructor TMazeThreadTest.Create;
  begin
    inherited;
    fTestName   := 'Maze threads test';
    fTestDescription := 'generates up to two threads that create and then solve mazes';
    fTestVersion:= '1.0';
    fTestType   := ttCpu;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 17.9339;
    fQTestType := qtMazeThreads;
    // default to two threads:
    fNbrOfThreads := 2;
    fPerformanceVectors := [ pvArtificialIntelligence ];
  end;

  procedure TMazeThreadTest.BeforeTest;
  begin
    inherited;
    // thread 1:
    // create first output form:
    fOutputForm1 := TfrmOutput.Create( nil );
    fOutputForm1.Position := poDesigned;
    fOutputForm1.Width := fOutputForm1.Width div 2;
    fOutputForm1.Left := 10;
    fOutputForm1.Top := 0;
    // create first thread:
    fOutputForm1.Visible := TRUE;
    fMazeThread1 := TMazeThread.Create;
    fMazeThread1.OnTerminate := MazeThreadDone;
    fMazeThread1.PaintBox32 := fOutputForm1.PaintBoxOutput;
    if fNbrOfThreads = 2 then begin
      // thread 2:
      // create second output form:
      fOutputForm2 := TfrmOutput.Create( nil );
      fOutputForm2.Position := poDesigned;
      fOutputForm2.Width := fOutputForm1.Width;
      fOutputForm2.Top := fOutputForm1.Top;
      fOutputForm2.Left := fOutputForm1.Left + fOutputForm1.Width + 1;
      fOutputForm2.Visible := TRUE;
      // create second thread:
      fMazeThread2 := TMazeThread.Create;
      fMazeThread2.OnTerminate := MazeThreadDone;
      fMazeThread2.PaintBox32 := fOutputForm2.PaintBoxOutput;
    end;
    // pause
    Application.ProcessMessages;
    sleep( 100 );
  end; // procedure TMazeThreadTest.BeforeTest

  procedure TMazeThreadTest.RunTest;
  begin
    fMazeThread1.Resume;
    inc( fNbrOfThreadsRunning );
    if fNbrOfThreads = 2 then begin
      fMazeThread2.Resume;
      inc( fNbrOfThreadsRunning );
    end;
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure TMazeThreadTest.RunTest;

  procedure TMazeThreadTest.MazeThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    if fNbrOfThreadsRunning = 0 then begin
      fPaintBox32.Canvas.Refresh;
    end;
    application.ProcessMessages;
  end; // procedure TMazeThreadTest.FernThreadDone;

  procedure TMazeThreadTest.AfterTest;
  begin
    if assigned( fOutputForm1 ) then fOutputForm1.Release;
    if assigned( fOutputForm2 ) then fOutputForm2.Release;
    inherited;
  end; // procedure TMazeThreadTest.BeforeTest
// TMazeThreadTest ends.........................................................

// TBandwidthBP64Test begins........................................................
  constructor TBandwidthBP64Test.Create;
  begin
    inherited;
    fTestName   := 'Memory bandwidth(BP64)';
    fTestDescription := 'Measures main memory bandwidth over 16MB block using the Block Prefetch64 technique.';
    fTestVersion:= '1.0';
    fTestType   := ttMemory;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 2.35;
    fQTestType := qtBandwidthBP64old;
    fPerformanceVectors := [ pv3dGaming, pvDataSetTransform,
                             pvMathScienceEngineering ];
  end;

  procedure TBandwidthBP64Test.BeforeTest;
  begin
    inherited;
    // set canvas scale for output:
    fArraySize := 4 * ONE_MEGABYTE;
    SetLength(fA, fArraySize);
  end; // procedure TMemLatencyTest.BeforeTest

  procedure TBandwidthBP64Test.RunTest;
  var
    leTime : extended;
  begin
    leTime := BlockPrefetch64( 100, fArraySize );
  end; // procedure TBandwidthBP64Test.RunTest;

  procedure TBandwidthBP64Test.AfterTest;
  begin
    fA := NIL;
    inherited;
  end; // procedure TMemLatencyTest.AfterTest;

  procedure TBandwidthBP64Test.Clear;
  begin
    inherited;
  end; //procedure TBandwidthBP64Test.Clear;

  function TBandwidthBP64Test.BlockPrefetch64( ai_RepeatCount : integer;
                                               ai_ArraySize   : integer
                                               ): Extended;
  var
    li64_StartCycle   : Int64;
    li64_EndCycle     : Int64;
    li_RandomData     : integer;
    lc_temp           : integer;
    le_elapsed_time   : extended;
    i, j              : integer;
    li_iterate        : integer;
  begin
    // initialize variables
    le_elapsed_time := 0;
    lc_temp := 0;
    Randomize;
    li_RandomData   := Random( 1000000 );
    for li_iterate := 1 to ai_RepeatCount do begin
      // at each step we need to populate the array:
      i := 0;
      while i < ai_ArraySize do begin
        // start timer
        li64_StartCycle := GetCycleCount;
        // block prefetch 8 64-byte lines:
        fA[i] := li_RandomData;
        fA[i + 16] := li_RandomData;
        fA[i + 32] := li_RandomData;
        fA[i + 48] := li_RandomData;
        fA[i + 64] := li_RandomData;
        fA[i + 80] := li_RandomData;
        fA[i + 96] := li_RandomData;
        fA[i + 112] := li_RandomData;

        // copy data
  //      fA[i] := li_RandomData;
        fA[i + 1] := li_RandomData;
        fA[i + 2] := li_RandomData;
        fA[i + 3] := li_RandomData;
        fA[i + 4] := li_RandomData;

        fA[i + 5] := li_RandomData;
        fA[i + 6] := li_RandomData;
        fA[i + 7] := li_RandomData;
        fA[i + 8] := li_RandomData;
        fA[i + 9] := li_RandomData;

        fA[i + 10] := li_RandomData;
        fA[i + 11] := li_RandomData;
        fA[i + 12] := li_RandomData;
        fA[i + 13] := li_RandomData;
        fA[i + 14] := li_RandomData;

        fA[i + 15] := li_RandomData;
  //      fA[i + 16] := li_RandomData;
        fA[i + 17] := li_RandomData;
        fA[i + 18] := li_RandomData;
        fA[i + 19] := li_RandomData;

        fA[i + 20] := li_RandomData;
        fA[i + 21] := li_RandomData;
        fA[i + 22] := li_RandomData;
        fA[i + 23] := li_RandomData;
        fA[i + 24] := li_RandomData;

        fA[i + 25] := li_RandomData;
        fA[i + 26] := li_RandomData;
        fA[i + 27] := li_RandomData;
        fA[i + 28] := li_RandomData;
        fA[i + 29] := li_RandomData;

        fA[i + 30] := li_RandomData;
        fA[i + 31] := li_RandomData;
  //      fA[i + 32] := li_RandomData;
        fA[i + 33] := li_RandomData;
        fA[i + 34] := li_RandomData;

        fA[i + 35] := li_RandomData;
        fA[i + 36] := li_RandomData;
        fA[i + 37] := li_RandomData;
        fA[i + 38] := li_RandomData;
        fA[i + 39] := li_RandomData;

        fA[i + 40] := li_RandomData;
        fA[i + 41] := li_RandomData;
        fA[i + 42] := li_RandomData;
        fA[i + 43] := li_RandomData;
        fA[i + 44] := li_RandomData;

        fA[i + 45] := li_RandomData;
        fA[i + 46] := li_RandomData;
        fA[i + 47] := li_RandomData;
  //      fA[i + 48] := li_RandomData;
        fA[i + 49] := li_RandomData;

        fA[i + 50] := li_RandomData;
        fA[i + 51] := li_RandomData;
        fA[i + 52] := li_RandomData;
        fA[i + 53] := li_RandomData;
        fA[i + 54] := li_RandomData;

        fA[i + 55] := li_RandomData;
        fA[i + 56] := li_RandomData;
        fA[i + 57] := li_RandomData;
        fA[i + 58] := li_RandomData;
        fA[i + 59] := li_RandomData;

        fA[i + 60] := li_RandomData;
        fA[i + 61] := li_RandomData;
        fA[i + 62] := li_RandomData;
        fA[i + 63] := li_RandomData;
  //      fA[i + 64] := li_RandomData;

        fA[i + 65] := li_RandomData;
        fA[i + 66] := li_RandomData;
        fA[i + 67] := li_RandomData;
        fA[i + 68] := li_RandomData;
        fA[i + 69] := li_RandomData;

        fA[i + 70] := li_RandomData;
        fA[i + 71] := li_RandomData;
        fA[i + 72] := li_RandomData;
        fA[i + 73] := li_RandomData;
        fA[i + 74] := li_RandomData;

        fA[i + 75] := li_RandomData;
        fA[i + 76] := li_RandomData;
        fA[i + 77] := li_RandomData;
        fA[i + 78] := li_RandomData;
        fA[i + 79] := li_RandomData;

  //      fA[i + 80] := li_RandomData;
        fA[i + 81] := li_RandomData;
        fA[i + 82] := li_RandomData;
        fA[i + 83] := li_RandomData;
        fA[i + 84] := li_RandomData;

        fA[i + 85] := li_RandomData;
        fA[i + 86] := li_RandomData;
        fA[i + 87] := li_RandomData;
        fA[i + 88] := li_RandomData;
        fA[i + 89] := li_RandomData;

        fA[i + 90] := li_RandomData;
        fA[i + 91] := li_RandomData;
        fA[i + 92] := li_RandomData;
        fA[i + 93] := li_RandomData;
        fA[i + 94] := li_RandomData;

        fA[i + 95] := li_RandomData;
  //      fA[i + 96] := li_RandomData;
        fA[i + 97] := li_RandomData;
        fA[i + 98] := li_RandomData;
        fA[i + 99] := li_RandomData;

        fA[i + 100] := li_RandomData;
        fA[i + 101] := li_RandomData;
        fA[i + 102] := li_RandomData;
        fA[i + 103] := li_RandomData;
        fA[i + 104] := li_RandomData;

        fA[i + 105] := li_RandomData;
        fA[i + 106] := li_RandomData;
        fA[i + 107] := li_RandomData;
        fA[i + 108] := li_RandomData;
        fA[i + 109] := li_RandomData;

        fA[i + 110] := li_RandomData;
        fA[i + 111] := li_RandomData;
  //      fA[i + 112] := li_RandomData;
        fA[i + 113] := li_RandomData;
        fA[i + 114] := li_RandomData;

        fA[i + 115] := li_RandomData;
        fA[i + 116] := li_RandomData;
        fA[i + 117] := li_RandomData;
        fA[i + 118] := li_RandomData;
        fA[i + 119] := li_RandomData;

        fA[i + 120] := li_RandomData;
        fA[i + 121] := li_RandomData;
        fA[i + 122] := li_RandomData;
        fA[i + 123] := li_RandomData;
        fA[i + 124] := li_RandomData;

        fA[i + 125] := li_RandomData;
        fA[i + 126] := li_RandomData;
        fA[i + 127] := li_RandomData;

        li64_EndCycle := GetCycleCount;
        le_elapsed_time := le_elapsed_time +
          ( ( li64_EndCycle - li64_StartCycle )) /
          StopWatch.GetCPUClockspeed(FALSE);

        i := i + 128;

      end; // while...

      // read back data: must accumulate or optimizer will remove
      lc_temp := 0;
      j := 0;
      while j < ai_ArraySize do begin

        // start timer
        li64_StartCycle := GetCycleCount;

        // block prefetch 8 64-byte lines:
        lc_temp := fA[j] - lc_temp;
        lc_temp := fA[ j + 16 ] + lc_temp;
        lc_temp := fA[ j + 32 ] - lc_temp;
        lc_temp := fA[ j + 48 ] + lc_temp;
        lc_temp := fA[ j + 64 ] - lc_temp;
        lc_temp := fA[ j + 80 ] + lc_temp;
        lc_temp := fA[ j + 96 ] - lc_temp;
        lc_temp := fA[ j + 112 ] + lc_temp;

        // copy data
  //      lc_temp := fA[ j ] + lc_temp;
        lc_temp := fA[ j + 1 ] - lc_temp;
        lc_temp := fA[ j + 2 ] + lc_temp;
        lc_temp := fA[ j + 3 ] - lc_temp;
        lc_temp := fA[ j + 4 ] + lc_temp;

        lc_temp := fA[ j + 5 ] - lc_temp;
        lc_temp := fA[ j + 6 ] + lc_temp;
        lc_temp := fA[ j + 7 ] - lc_temp;
        lc_temp := fA[ j + 8 ] + lc_temp;
        lc_temp := fA[ j + 9 ] - lc_temp;

        lc_temp := fA[ j + 10 ] + lc_temp;
        lc_temp := fA[ j + 11 ] - lc_temp;
        lc_temp := fA[ j + 12 ] + lc_temp;
        lc_temp := fA[ j + 13 ] - lc_temp;
        lc_temp := fA[ j + 14 ] + lc_temp;

        lc_temp := fA[ j + 15 ] - lc_temp;
  //      lc_temp := fA[j+16] + lc_temp;
        lc_temp := fA[ j + 17 ] + lc_temp;
        lc_temp := fA[ j + 18 ] - lc_temp;
        lc_temp := fA[ j + 19 ] + lc_temp;

        lc_temp := fA[ j + 20 ] - lc_temp;
        lc_temp := fA[ j + 21 ] + lc_temp;
        lc_temp := fA[ j + 22 ] - lc_temp;
        lc_temp := fA[ j + 23 ] + lc_temp;
        lc_temp := fA[ j + 24 ] - lc_temp;

        lc_temp := fA[ j + 25 ] + lc_temp;
        lc_temp := fA[ j + 26 ] - lc_temp;
        lc_temp := fA[ j + 27 ] + lc_temp;
        lc_temp := fA[ j + 28 ] - lc_temp;
        lc_temp := fA[ j + 29 ] + lc_temp;

        lc_temp := fA[ j + 30 ] - lc_temp;
        lc_temp := fA[ j + 31 ] + lc_temp;
  //      lc_temp := fA[j+32] + lc_temp;
        lc_temp := fA[ j + 33 ] - lc_temp;
        lc_temp := fA[ j + 34 ] + lc_temp;

        lc_temp := fA[ j + 35 ] - lc_temp;
        lc_temp := fA[ j + 36 ] + lc_temp;
        lc_temp := fA[ j + 37 ] - lc_temp;
        lc_temp := fA[ j + 38 ] + lc_temp;
        lc_temp := fA[ j + 39 ] - lc_temp;

        lc_temp := fA[ j + 40 ] + lc_temp;
        lc_temp := fA[ j + 41 ] - lc_temp;
        lc_temp := fA[ j + 42 ] + lc_temp;
        lc_temp := fA[ j + 43 ] - lc_temp;
        lc_temp := fA[ j + 44 ] + lc_temp;

        lc_temp := fA[ j + 45 ] - lc_temp;
        lc_temp := fA[ j + 46 ] + lc_temp;
        lc_temp := fA[ j + 47 ] - lc_temp;
  //      lc_temp := fA[j +48] + lc_temp;
        lc_temp := fA[ j + 49 ] + lc_temp;

        lc_temp := fA[ j + 50 ] - lc_temp;
        lc_temp := fA[ j + 51 ] + lc_temp;
        lc_temp := fA[ j + 52 ] - lc_temp;
        lc_temp := fA[ j + 53 ] + lc_temp;
        lc_temp := fA[ j + 54 ] - lc_temp;

        lc_temp := fA[ j + 55 ] + lc_temp;
        lc_temp := fA[ j + 56 ] - lc_temp;
        lc_temp := fA[ j + 57 ] + lc_temp;
        lc_temp := fA[ j + 58 ] - lc_temp;
        lc_temp := fA[ j + 59 ] + lc_temp;

        lc_temp := fA[ j + 60 ] - lc_temp;
        lc_temp := fA[ j + 61 ] + lc_temp;
        lc_temp := fA[ j + 62 ] - lc_temp;
        lc_temp := fA[ j + 63 ] + lc_temp;
  //      lc_temp := fA[j+64] + lc_temp;

        lc_temp := fA[ j + 65 ] - lc_temp;
        lc_temp := fA[ j + 66 ] + lc_temp;
        lc_temp := fA[ j + 67 ] - lc_temp;
        lc_temp := fA[ j + 68 ] + lc_temp;
        lc_temp := fA[ j + 69 ] - lc_temp;

        lc_temp := fA[ j + 70 ] + lc_temp;
        lc_temp := fA[ j + 71 ] - lc_temp;
        lc_temp := fA[ j + 72 ] + lc_temp;
        lc_temp := fA[ j + 73 ] - lc_temp;
        lc_temp := fA[ j + 74 ] + lc_temp;

        lc_temp := fA[ j + 75 ] - lc_temp;
        lc_temp := fA[ j + 76 ] + lc_temp;
        lc_temp := fA[ j + 77 ] - lc_temp;
        lc_temp := fA[ j + 78 ] + lc_temp;
        lc_temp := fA[ j + 79 ] - lc_temp;

  //      lc_temp := fA[j+80] + lc_temp;
        lc_temp := fA[ j + 81 ] + lc_temp;
        lc_temp := fA[ j + 82 ] - lc_temp;
        lc_temp := fA[ j + 83 ] + lc_temp;
        lc_temp := fA[ j + 84 ] - lc_temp;

        lc_temp := fA[ j + 85 ] + lc_temp;
        lc_temp := fA[ j + 86 ] - lc_temp;
        lc_temp := fA[ j + 87 ] + lc_temp;
        lc_temp := fA[ j + 88 ] - lc_temp;
        lc_temp := fA[ j + 89 ] + lc_temp;

        lc_temp := fA[ j + 90 ] - lc_temp;
        lc_temp := fA[ j + 91 ] + lc_temp;
        lc_temp := fA[ j + 92 ] - lc_temp;
        lc_temp := fA[ j + 93 ] + lc_temp;
        lc_temp := fA[ j + 94 ] - lc_temp;

        lc_temp := fA[ j + 95 ] + lc_temp;
  //      lc_temp := fA[j+96] + lc_temp;
        lc_temp := fA[ j + 97 ] - lc_temp;
        lc_temp := fA[ j + 98 ] + lc_temp;
        lc_temp := fA[ j + 99 ] - lc_temp;

        lc_temp := fA[ j + 100 ] + lc_temp;
        lc_temp := fA[ j + 101 ] - lc_temp;
        lc_temp := fA[ j + 102 ] + lc_temp;
        lc_temp := fA[ j + 103 ] - lc_temp;
        lc_temp := fA[ j + 104 ] + lc_temp;

        lc_temp := fA[ j + 105 ] - lc_temp;
        lc_temp := fA[ j + 106 ] + lc_temp;
        lc_temp := fA[ j + 107 ] - lc_temp;
        lc_temp := fA[ j + 108 ] + lc_temp;
        lc_temp := fA[ j + 109 ] - lc_temp;

        lc_temp := fA[ j + 110 ] + lc_temp;
        lc_temp := fA[ j + 111 ] - lc_temp;
  //      lc_temp := fA[j+112] + lc_temp;
        lc_temp := fA[ j + 113 ] + lc_temp;
        lc_temp := fA[ j + 114 ] - lc_temp;

        lc_temp := fA[ j + 115 ] + lc_temp;
        lc_temp := fA[ j + 116 ] - lc_temp;
        lc_temp := fA[ j + 117 ] + lc_temp;
        lc_temp := fA[ j + 118 ] - lc_temp;
        lc_temp := fA[ j + 119 ] + lc_temp;

        lc_temp := fA[ j + 120 ] - lc_temp;
        lc_temp := fA[ j + 121 ] + lc_temp;
        lc_temp := fA[ j + 122 ] - lc_temp;
        lc_temp := fA[ j + 123 ] + lc_temp;
        lc_temp := fA[ j + 124 ] - lc_temp;

        lc_temp := fA[ j + 125 ] + lc_temp;
        lc_temp := fA[ j + 126 ] - lc_temp;
        lc_temp := fA[ j + 127 ] + lc_temp;

        li64_EndCycle := GetCycleCount;
        le_elapsed_time := le_elapsed_time +
          ( ( li64_EndCycle - li64_StartCycle ) ) /
          StopWatch.GetCPUClockspeed(FALSE);

        j := j + 128;

      end; // while...

    end; // for...

    // assign to dummy variable or optimizer will remove:
    FDummy := lc_temp;
    result := le_elapsed_time;

  end; // function TBandwidthBP64Test.BlockPrefetch64
// TBandwidthBP64Test ends......................................................

// TMemLatencyTest begins......................................................
  constructor TMemLatencyTest.Create;
  begin
    inherited;
    fTestName   := 'Memory latency';
    fTestDescription := 'Measures main memory latency by making random 32-bit copies over two 16MB blocks.';
    fTestVersion:= '1.0';
    fTestType   := ttMemory;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 7.560;
    fQTestType := qtMemLatency;
    fPerformanceVectors := [ pvResponsiveness, pvArtificialIntelligence,
      pv3dGaming ];
  end;

  procedure TMemLatencyTest.AfterTest;
  begin
    fA := NIL;
    fB := NIL;
    inherited;
  end; // procedure TMemLatencyTest.AfterTest;

  procedure TMemLatencyTest.BeforeTest;
  begin
    inherited;
    // set canvas scale for output:
    fArraySize := 4 * ONE_MILLION;
    SetLength(fB, fArraySize);
    SetLength(fA, fArraySize);
    RandomizeArray(fB);
  end; // procedure TMemLatencyTest.BeforeTest

  procedure TMemLatencyTest.RunTest;
  var
    li_FromElement : integer;
    li_ToElement   : integer;
    i : integer;
  begin
    for i := 1 to 10 * fArraySize do
    begin
      li_FromElement := Random( fArraySize );
      li_ToElement := Random( fArraySize );
      fA[li_ToElement] := fB[li_FromElement];
    end;
  end; // procedure TMemLatencyTest.RunTest;

  procedure TMemLatencyTest.Clear;
  begin
    inherited;
  end; //procedure TMemLatencyTest.Clear;
// TMemLatencyTest ends......................................................

// TFibThread...................................................................
constructor TFibThread.Create(n: integer);
begin
  Fn := n;
  FreeOnTerminate := TRUE;
  inherited Create(TRUE);  // creates suspended
  Priority := tpNormal;
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

// TPiThread....................................................................
constructor TPiThread.Create(n: integer);
begin
  Fn := n;
  FreeOnTerminate := TRUE;
  inherited Create(TRUE);  // creates suspended
  Priority := tpNormal;
end;

procedure TPiThread.Execute;
var
  ls_pi : string;
begin
  ls_pi := ComputePi(Fn);
end; //procedure TPiThread.Execute;

function TPiThread.ComputePi( NumDigits : integer ) : string;
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
    if Terminated then Exit;
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
end; //function TPiThread.ComputePi
// TPIThread ends...............................................................

// TOrthogonalThreadTest begins.................................................
  constructor TOrthogonalThreadTest.Create;
  begin
    inherited;
    fTestName   := 'Orthogonal threads test';
    fTestDescription := 'Executes two threads (Fibonacci, Pi) that share neither code nor data, which minimizes interprocessor traffic.';
    fTestVersion:= '1.0';
    fTestType   := ttThreads;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 1.31352;
    fQTestType := qtOrthogonalThreads;
    fPerformanceVectors := [ pvMathScienceEngineering ];
  end; // constructor TOrthogonalThreadTest.Create;

  procedure TOrthogonalThreadTest.BeforeTest;
  begin
    inherited;
    // thread 1:
    fFibThread := TFibThread.Create( 40 );
    fFibThread.OnTerminate := FibThreadDone;
    // thread 2:
    fPiThread := TPiThread.Create( 4000 );
    fPiThread.OnTerminate := PiThreadDone;
  end; // procedure TOrthogonalThreadTest.BeforeTest

  procedure TOrthogonalThreadTest.RunTest;
  begin
    fFibThread.Resume;
    fFibThreadRunning := TRUE;
    inc( fNbrOfThreadsRunning );
    fPiThread.Resume;
    fPiThreadRunning := TRUE;
    inc( fNbrOfThreadsRunning );
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure TOrthogonalThreadTest.RunTest;

  procedure TOrthogonalThreadTest.FibThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    fFibThreadRunning := FALSE;
    fFibThread.CleanupInstance;
    if fNbrOfThreadsRunning = 0 then begin
//
    end else if fNbrOfThreadsRunning = 1 then begin
       if assigned( ffrmStatus ) then ffrmStatus.AddLine('Fib thread complete.');
    end;
    application.ProcessMessages;
  end; // procedure TOrthogonalThreadTest.FibThreadDone;

  procedure TOrthogonalThreadTest.PiThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    fPiThreadRunning := FALSE;
    fPiThread.CleanupInstance;
    if fNbrOfThreadsRunning = 0 then begin
//
    end else if fNbrOfThreadsRunning = 1 then begin
       if assigned( ffrmStatus ) then ffrmStatus.AddLine('Pi thread complete.');
    end;
    application.ProcessMessages;
  end; // procedure TOrthogonalThreadTest.FibThreadDone;

  procedure TOrthogonalThreadTest.AfterTest;
  begin
    fFibThread := NIL;
    fPiThread := NIL;
    inherited;
  end; // procedure TOrthogonalThreadTest.BeforeTest
// TOrthogonalThreadTest ends...................................................

// TSortThread starts...........................................................
constructor TSortThread.Create( aiRandomSeed : integer );
begin
  RandSeed := aiRandomSeed;
  SetLength( FSortArray, 5 * ONE_MILLION );
  RandomizeArray( FSortArray );
  FSize := High( FSortArray ) - Low( FSortArray ) + 1;
  FreeOnTerminate := FALSE; // free by hand
  inherited Create( TRUE ); // creates suspended
  Priority := tpNormal;
end;

destructor TSortThread.Destroy;
begin
  FSortArray := NIL;
  inherited;
end;

{ The Execute method is called when the thread starts }

procedure TSortThread.Execute;
begin
  Sort( FSortArray );
end; // procedure TSortThread.Execute

procedure TSortThread.RandomizeArray(var SortArray: array of integer);
var
  I: Integer;
begin
    for I := Low(SortArray) to High(SortArray) do
      SortArray[I] := Random(1000000000);
end; // procedure TSortThread.RandomizeArray;
// TSortThread ends.............................................................

// TQuickSort starts............................................................
procedure TQuickSort.Sort(var A: array of integer);

  procedure QuickSort(var A: array of integer; iLo, iHi: Integer);
  var
    Lo, Hi, Mid, T: Integer;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := A[(Lo + Hi) div 2];
    repeat
      while A[Lo] < Mid do Inc(Lo);
      while A[Hi] > Mid do Dec(Hi);
      if Lo <= Hi then
      begin
        T := A[Lo];
        A[Lo] := A[Hi];
        A[Hi] := T;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then QuickSort(A, iLo, Hi);
    if Lo < iHi then QuickSort(A, Lo, iHi);
    if Terminated then Exit;
  end;

begin
  QuickSort(A, Low(A), High(A));
end; // procedure TQuickSort.Sort

// TIdenticalThreadTest begins.................................................

  constructor TIdenticalThreadTest.Create;
  begin
    inherited;
    fTestName   := 'Identical threads test';
    fTestDescription := 'Executes two identical quick sort threads with each sorting 5,000,000 elements.';
    fTestVersion:= '1.0';
    fTestType   := ttThreads;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 0.708;
    fQTestType := qtIdenticalThreads;
    fPerformanceVectors := [ pvDataSetTransform ];
  end; // constructor TIdenticalThreadTest.Create;

  destructor TIdenticalThreadTest.Destroy;
  begin
    if assigned( fQuickSortThread1 ) then begin
      fQuickSortThread1.CleanupInstance;
      fQuickSortThread1.Free;
      fQuickSortThread1 := NIL;
    end;
    if assigned( fQuickSortThread2 ) then begin
      fQuickSortThread2.CleanupInstance;
      fQuickSortThread2.Free;
      fQuickSortThread2 := NIL;
    end;
    inherited Destroy;
  end; // destructor TIdenticalThreadTest.Destroy;

  procedure TIdenticalThreadTest.BeforeTest;
  begin
    inherited;
    RandSeed := 121;
    // thread 1:
    fQuickSortThread1 := TQuickSort.Create( 3834 );
    fQuickSortThread1.OnTerminate := ThreadDone1;
    // thread 2:
    fQuickSortThread2 := TQuickSort.Create( 934383 );
    fQuickSortThread2.OnTerminate := ThreadDone2;
    fNbrOfThreadsRunning := 0;
  end; // procedure TIdenticalThreadTest.BeforeTest

  procedure TIdenticalThreadTest.RunTest;
  begin
    fQuickSortThread1.Resume;
    inc( fNbrOfThreadsRunning );
    fQuickSortThread2.Resume;
    inc( fNbrOfThreadsRunning );
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure TIdenticalThreadTest.RunTest;

  procedure TIdenticalThreadTest.ThreadDone1(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    fQuickSortThread1.CleanupInstance;
    if fNbrOfThreadsRunning = 0 then begin

    end else if fNbrOfThreadsRunning = 1 then begin
      if assigned( ffrmStatus ) then ffrmStatus.AddLine('Thread 1 complete.');
    end;
    application.ProcessMessages;
  end; // procedure TIdenticalThreadTest.FibThreadDone;

  procedure TIdenticalThreadTest.ThreadDone2(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    fQuickSortThread2.CleanupInstance;
    if fNbrOfThreadsRunning = 0 then begin

    end else if fNbrOfThreadsRunning = 1 then begin
      if assigned( ffrmStatus ) then ffrmStatus.AddLine('Thread 1 complete.');
    end;
    application.ProcessMessages;
  end; // procedure TIdenticalThreadTest.FibThreadDone;
// TIdenticalThreadTest ends...................................................

// TGogoEncoderTest..............................................................
  constructor TGogoEncoderTest.Create;
  begin
    inherited;
    fTestName   := 'Gogo MP3 Encoder';
    fTestDescription := 'Times the built-in 128kbps MP3 encoding benchmark that is part of GoGo encoder.';
    fTestVersion:= '1.0';
    fTestType   := ttCPU;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 5.01783;
    fQTestType := qtGogoEncode;
    fPerformanceVectors := [ pvDataSetTransform ];
  end;

  procedure TGogoEncoderTest.RunTest;
  const
    // assumes that gogo is in the same directory as the calling program:
    GOGO_FILE_BM_COMMAND = 'osmfiles\gogo.exe -test 600';
  var
    lstrAppPath : string;
    liReturnCode : integer;
  begin
    lstrAppPath := ExtractFilePath(Application.ExeName);
    // run the GOGO
    liReturnCode := StartProgramWait( lstrAppPath + GOGO_FILE_BM_COMMAND, SW_MAXIMIZE );
    if liReturnCode <> 0 then raise exception.create( 'Gogo terminated with an error!' );
  end; // procedure TGogoEncoderTest.RunTest;
// TGogoEncoderTest ends.........................................................

// TJpgDecodeTest...............................................................
  constructor TJpgDecodeTest.Create;
  begin
    inherited;
    fTestName   := 'JPG Decode';
    fTestDescription := 'Benchmarks JPG image decoding performance,';
    fTestVersion:= '1.0';
    fTestType   := ttCPU;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 18.64;
    fQTestType := qtJpgDecode;
    fPerformanceVectors := [ pvDataSetTransform ];
  end; // constructor TJpgDecodeTest.Create;

  procedure TJpgDecodeTest.BeforeTest;
  begin
    inherited;
    // PaintBox32 does not get along with the image component, so make it invisible:
    fPaintBox32.Visible := FALSE;
    // create the image componest which will do the JPG decoding
    fImage := TImage.Create(fOutputForm);
    // setting the parent to the output to ensure the component is freed
    fImage.Parent := fOutputForm;
    // bring the component to the front of any other controls on the form
    fImage.BringToFront;
    // make the image take over the entire form
    fImage.Align := alClient;
    // since images are usually stretched/compressed to fit the screen, when
    // users look at them let's set stretch to TRUE
    fImage.Stretch := TRUE;
  end; // procedure TJpgDecodeTest.BeforeTest

  procedure TJpgDecodeTest.RunTest;
  const
    // assumes both of the pictures are in the same folder as the calling program.
    JPG_FILE1 = 'osmfiles\pic8.jpg';
    JPG_FILE2 = 'osmfiles\pic9.jpg';
    JPG_FILE3 = 'osmfiles\pic1.jpg';
    JPG_FILE4 = 'osmfiles\pic2.jpg';
    JPG_FILE5 = 'osmfiles\pic5.jpg';
    JPG_FILE6 = 'osmfiles\pic6.jpg';
    JPG_FILE7 = 'osmfiles\pic7.jpg';
    JPG_FILE8 = 'osmfiles\pic10.jpg';
    JPG_FILE9 = 'osmfiles\pic11.jpg';
    JPG_FILE10 = 'osmfiles\pic12.jpg';
  var
    i : integer;
    lsFilePath : string;
  begin
    lsFilePath := ExtractFilePath(application.ExeName);
    for i := 1 to 5 do begin
      fImage.Picture.LoadFromFile( lsFilePath + JPG_FILE1 );
      // allow the screen to update:
      application.ProcessMessages;
      fImage.Picture.LoadFromFile( lsFilePath + JPG_FILE2 );
      // allow the screen to update:
      application.ProcessMessages;
      fImage.Picture.LoadFromFile( lsFilePath + JPG_FILE3 );
      // allow the screen to update:
      application.ProcessMessages;
      fImage.Picture.LoadFromFile( lsFilePath + JPG_FILE4 );
      // allow the screen to update:
      application.ProcessMessages;
      fImage.Picture.LoadFromFile( lsFilePath + JPG_FILE8 );
      // allow the screen to update:
      application.ProcessMessages;
      fImage.Picture.LoadFromFile( lsFilePath + JPG_FILE5 );
      // allow the screen to update:
      application.ProcessMessages;
      fImage.Picture.LoadFromFile( lsFilePath + JPG_FILE6 );
      // allow the screen to update:
      application.ProcessMessages;
      fImage.Picture.LoadFromFile( lsFilePath + JPG_FILE7 );
      // allow the screen to update:
      application.ProcessMessages;
      fImage.Picture.LoadFromFile( lsFilePath + JPG_FILE9 );
      // allow the screen to update:
      application.ProcessMessages;
      fImage.Picture.LoadFromFile( lsFilePath + JPG_FILE10 );
      // allow the screen to update:
      application.ProcessMessages;
    end; // for
  end; // procedure TRichEditTest.RunTest;

  procedure TJpgDecodeTest.AfterTest;
  begin
    inherited;
    // dispose of the image component to prevent memory leaks
    FreeAndNil( fImage );
    // restore fPaintBox32 visibility:
    fPaintBox32.Visible := TRUE;
  end; // procedure TJpgDecodeTest.BeforeTest
// TJpgDecodeTest ends..........................................................

// TImageRotateTest.............................................................
  constructor TImageRotateTest.Create;
  begin
    inherited;
    fTestName   := 'Image Rotate';
    fTestDescription := 'Rotates an image 90 degrees at a time.';
    fTestVersion:= '1.0';
    fTestType   := ttCPU;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 0.871;
    fQTestType := qtImageRotate;
    fPerformanceVectors := [ pvDataSetTransform ];
  end; // constructor TImageRotateTest.Create;

  procedure TImageRotateTest.BeforeTest;
  const
    // assumes both of the pictures are in the same folder as the calling program.
    JPG_FILE = 'osmfiles\pic3.jpg';
  var
    lsFilePath : string;
  begin
    inherited;
    // PaintBox32 does not get along with the image component, so make it invisible:
    fPaintBox32.Visible := FALSE;
    // create the image componest which will do the JPG decoding
    fImage32 := TImage32.Create(fOutputForm);
    // setting the parent to the output to ensure the component is freed
    fImage32.Parent := fOutputForm;
    // bring the component to the front of any other controls on the form
    fImage32.BringToFront;
    // make the image take over the entire form
    fImage32.Align := alClient;
    // since images are usually stretched/compressed to fit the screen, when
    // users look at them let's set stretch to TRUE
    fImage32.ScaleMode := smStretch;
    // load the image:
    lsFilePath := extractFilePath( application.ExeName );
    fImage32.Bitmap.LoadFromFile( lsFilePath + JPG_FILE );
    // allow the screen to update:
    application.ProcessMessages;
  end; // procedure TImageRotateTest.BeforeTest

  procedure TImageRotateTest.RunTest;
  var
    i : integer;
  begin
    for i := 1 to 12 do begin
      fImage32.Bitmap.Rotate90();
      // allow the screen to update:
      application.ProcessMessages;
    end; // for
  end; // procedure TImageRotateTest.RunTest;

  procedure TImageRotateTest.AfterTest;
  begin
    inherited;
    // dispose of the image component to prevent memory leaks
    FreeAndNil( fImage32 );
    // restore fPaintBox32 visibility:
    fPaintBox32.Visible := TRUE;
  end; // procedure TImageRotateTest.BeforeTest
// TImageRotateTest ends........................................................

// TImageResizeTest.............................................................
  constructor TImageResizeTest.Create;
  begin
    inherited;
    fTestName   := 'Image Resize';
    fTestDescription := 'gradually resizes an image larger then smaller';
    fTestVersion:= '1.0';
    fTestType   := ttCPU;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 1.07;
    fQTestType := qtImageResize;
    fPerformanceVectors := [ pvDataSetTransform ];
  end; // constructor TImageResizeTest.Create;

  procedure TImageResizeTest.BeforeTest;
  const
    // assumes both of the pictures are in the same folder as the calling program.
    JPG_FILE = 'osmfiles\pic4.jpg';
  var
    lsFilePath : string;
  begin
    inherited;
    // PaintBox32 does not get along with the image component, so make it invisible:
    fPaintBox32.Visible := FALSE;
    // create the image componest which will do the JPG decoding
    fImage32 := TImage32.Create(fOutputForm);
    // setting the parent to the output to ensure the component is freed
    fImage32.Parent := fOutputForm;
    // bring the component to the front of any other controls on the form
    fImage32.BringToFront;
    // make the image take over the entire form
    fImage32.Align := alClient;
    // set to scale mode to enable zoom:
    fImage32.ScaleMode := smScale;
    // make image little:
    fImage32.Scale := 0.001;
    // place image in the center of view:
    fImage32.BitmapAlign := baCenter;
    // load the image:
    lsFilePath := ExtractFilePath( application.exename );
    fImage32.Bitmap.LoadFromFile( lsFilePath + JPG_FILE );
    // allow the screen to update:
    application.ProcessMessages;
  end; // procedure TImageResizeTest.BeforeTest

  procedure TImageResizeTest.RunTest;
  var
    i : integer;
  begin
    for i := 1 to 200 do begin
      fImage32.Scale := i / 100;
      // allow the screen to update:
      application.ProcessMessages;
    end; // for
    for i := 200 downto 1 do begin
      fImage32.Scale := i / 100;
      // allow the screen to update:
      application.ProcessMessages;
    end; // for
  end; // procedure TImageResizeTest.RunTest;

  procedure TImageResizeTest.AfterTest;
  begin
    inherited;
    // dispose of the image component to prevent memory leaks
    FreeAndNil( fImage32 );
    // restore fPaintBox32 visibility:
    fPaintBox32.Visible := TRUE;
  end; // procedure TImageResizeTest.BeforeTest
// TImageResizeTest ends........................................................

// TWebPageLoad.................................................................
  constructor TWebPageLoad.Create;
  begin
    inherited;
    fTestName   := 'Web Page Load';
    fTestDescription := 'This test measures system performance loading web pages';
    fTestVersion:= '1.0';
    fTestType   := ttCPU;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 9.8817;
    fQTestType := qtWebPageLoad;
    fPerformanceVectors := [ pvResponsiveness ];
  end; // constructor TWebPageLoad.Create;

  procedure TWebPageLoad.BeforeTest;
  begin
    inherited;
    // PaintBox32 does not get along with the some components, so make it invisible:
    fPaintBox32.Visible := FALSE;
    // create the browser component
    fWebBrowser := TWebBrowser.Create(fOutputForm);
    TWinControl(fWebBrowser).parent := fOutputForm;
    // bring the component to the front of any other controls on the form
    fWebBrowser.BringToFront;
    fWebBrowser.Visible := TRUE;
    // make the browser take over the entire form
    fWebBrowser.Align := alClient;
    fIsOffline := IsGlobalOffline;
    if not fIsOffline then SetGlobalOffline( TRUE );
    application.ProcessMessages;
  end; // procedure TWebPageLoad.BeforeTest

  procedure TWebPageLoad.RunTest;
  const
    WEB_PAGE_1 = 'osmfiles\html\Van''s Hardware Journal  Home.htm';
    WEB_PAGE_2 = 'osmfiles\pic7.jpg';
    WEB_PAGE_3 = 'osmfiles\html\011116_Xbox.htm';
    WEB_PAGE_4 = 'osmfiles\OSMarkDocumentation.html';
    WEB_PAGE_5 = 'osmfiles\html\020822_AthlonXP2600.htm';
    WEB_PAGE_6 = 'osmfiles\html\020107_AthlonXP2000.htm';
    WEB_PAGE_7 = 'osmfiles\html\010709_Arcos_Jukebox_6000.htm';
  var
    i : integer;
    s : string;
  begin
    s := ExtractFilePath(Application.ExeName);
    for i := 1 to 10 do begin
      fWebBrowser.Navigate(s + WEB_PAGE_1);
      application.ProcessMessages;
      while (fWebBrowser.ReadyState <> READYSTATE_COMPLETE) do begin
        application.ProcessMessages;
      end; // do
      application.ProcessMessages;
      fWebBrowser.Navigate(s + WEB_PAGE_2);
      application.ProcessMessages;
      while (fWebBrowser.ReadyState <> READYSTATE_COMPLETE) do begin
        application.ProcessMessages;
      end; // do
      application.ProcessMessages;
      fWebBrowser.Navigate(s + WEB_PAGE_3);
      application.ProcessMessages;
      while (fWebBrowser.ReadyState <> READYSTATE_COMPLETE) do begin
        application.ProcessMessages;
      end; // do
      application.ProcessMessages;
      fWebBrowser.Navigate(s + WEB_PAGE_4);
      application.ProcessMessages;
      while (fWebBrowser.ReadyState <> READYSTATE_COMPLETE) do begin
        application.ProcessMessages;
      end; // do
      application.ProcessMessages;
      fWebBrowser.Navigate(s + WEB_PAGE_5);
      application.ProcessMessages;
      while (fWebBrowser.ReadyState <> READYSTATE_COMPLETE) do begin
        application.ProcessMessages;
      end; // do
      application.ProcessMessages;
      fWebBrowser.Navigate(s + WEB_PAGE_6);
      application.ProcessMessages;
      while (fWebBrowser.ReadyState <> READYSTATE_COMPLETE) do begin
        application.ProcessMessages;
      end; // do
      application.ProcessMessages;
      fWebBrowser.Navigate(s + WEB_PAGE_7);
      application.ProcessMessages;
      while (fWebBrowser.ReadyState <> READYSTATE_COMPLETE) do begin
        application.ProcessMessages;
      end; // do
      application.ProcessMessages;
    end; // for
  end; // procedure TWebPageLoad.RunTest;

  procedure TWebPageLoad.AfterTest;
  begin
    inherited;
    // dispose of the image component to prevent memory leaks
    FreeAndNil( fWebBrowser );
    // restore fPaintBox32 visibility:
    fPaintBox32.Visible := TRUE;
    if not fIsOffline then begin
      while IsGlobalOffline do begin
        SetGlobalOffline( FALSE );
        application.ProcessMessages;
      end; // while
    end; // if
  end; // procedure TWebPageLoad.BeforeTest
// TWebPageLoad ends........................................................

// TEncryptDecryptTest..........................................................
  constructor TEncryptDecryptTest.Create;
  begin
    inherited;
    fTestName   := 'AES Encrypt / Decrypt';
    fTestDescription := 'Performs AES Encryption / Decryption based on Dr. Brian Gladman''s routines.  VIA processors use ACE unit.';
    fTestVersion:= '1.0';
    fTestType   := ttCPU;
    fTestAuthor := 'Van Smith, Tom Crispin';
    fReferenceTime := 10.1;
    fQTestType := qtEncryptDecrypt;
    fPerformanceVectors := [ pvSecurity ];
  end;

  procedure TEncryptDecryptTest.RunTest;
  const
    // assumes that gogo is in the same directory as the calling program:
    AES_FILENAME = 'osmfiles\AESBench.exe';
  var
    lstrAppPath : string;
    liReturnCode : integer;
  begin
    lstrAppPath := ExtractFilePath(Application.ExeName);
    // run the AES benchmark:
    liReturnCode := StartProgramWait( lstrAppPath + AES_FILENAME, SW_MAXIMIZE );
    if liReturnCode <> 0 then raise exception.create( 'Gogo terminated with an error!' );
  end; // procedure TEncryptDecryptTest.RunTest;
// TEncryptDecryptTest ends.....................................................

// TZipCompressTest..........................................................
  constructor TZipCompressTest.Create;
  begin
    inherited;
    fTestName   := 'Zip File Compress';
    fTestDescription := 'Using the free ZipForge component (http://www.componentace.com) this test measures file compression performance.';
    fTestVersion:= '1.0';
    fTestType   := ttCPU;
    fTestAuthor := 'Van Smith';
    fReferenceTime :=  61.5558;
    fQTestType := qtZipCompress;
    fPerformanceVectors := [ pvDataSetTransform ];
  end; //

  procedure TZipCompressTest.BeforeTest;
  begin
    inherited;
    fTargetFile := 'c:\osmark_delete_me.zip';
    fSourceDir := ExtractFilePath(Application.ExeName) + 'osmfiles';
    // create ZipForge component:
    fZipForge := TZipForge.Create(fOutputForm);
    application.ProcessMessages;
  end; // procedure TZipCompressTest.BeforeTest

  procedure TZipCompressTest.RunTest;
  var
    i : integer;
  begin
    // because file I/O is always so variable, we must compress several times
    for i := 1 to 5 do begin
      fZipForge.FileName := fTargetFile;
      fZipForge.CompressionLevel := clMax;
      // Create a new archive file
      fZipForge.OpenArchive(fmCreate);
      // Set path to folder with some text files to BaseDir
      fZipForge.BaseDir := fSourceDir;
      // Add all files and directories from fSourceDir to the archive
      fZipForge.AddFiles('*.*');
      application.ProcessMessages;
      fZipForge.CloseArchive;
      application.ProcessMessages;
    end; // for
  end; // procedure TZipCompressTest.RunTest;

  procedure TZipCompressTest.AfterTest;
  begin
    inherited;
    // don't forget to delete the zip file:
    DeleteFile( fTargetFile );
    application.ProcessMessages;
    // dispose of the ZipForge component to prevent memory leaks
    FreeAndNil( fZipForge );
    application.ProcessMessages;
  end; // procedure TZipCompressTest.BeforeTest
// TZipCompressTest ends.....................................................

// TFileCopyTest................................................................
  constructor TFileCopyTest.Create;
  begin
    inherited;
    fTestName   := 'File Copy Test';
    fTestDescription := 'Using file streaming calls, this test measures file copy performance with a 100MB source file copied to five targets.';
    fTestVersion:= '1.0';
    fTestType   := ttDisk;
    fTestAuthor := 'Van Smith';
    fTargetDrive := 'c';
    fReferenceTime := 17.9;
    fQTestType := qtFileCopy;
    fPerformanceVectors := [ pvResponsiveness ];
  end; // constructor TFileCopyTest.Create;

  procedure TFileCopyTest.BeforeTest;
  begin
    inherited;
    fTargetFile1 := fTargetDrive + ':\delete_me_target1.txt';
    fTargetFile2 := fTargetDrive + ':\delete_me_target2.txt';
    fTargetFile3 := fTargetDrive + ':\delete_me_target3.txt';
    fTargetFile4 := fTargetDrive + ':\delete_me_target4.txt';
    fTargetFile5 := fTargetDrive + ':\delete_me_target5.txt';
    fSourceFile := fTargetDrive + ':\delete_me_source.txt';
    fFileSize := 100 * ONE_MEGABYTE;
    if not EnoughDriveSpace( fTargetDrive ) then raise exception.Create('TFileCopyTest.BeforeTest: Not enough drive space!');
    // create initial file:
    CreateSource( fSourceFile );
    if assigned( ffrmStatus ) then ffrmStatus.AddLine('Test begins...');
    application.ProcessMessages;
  end; // procedure TFileCopyTest.BeforeTest

  procedure TFileCopyTest.CopyFileToTarget( as_sourceFile: string;
                                            as_targetFile: string
                                           );
  var
    lfs_SourceFile  : TFileStream;
    lfs_TargetFile  : TFileStream;
  begin
    lfs_SourceFile := TFileStream.Create( as_sourceFile, fmOpenRead );
    try
      lfs_TargetFile := TFileStream.Create(as_targetFile, fmCreate or fmShareDenyRead);
      try
        lfs_TargetFile.CopyFrom(lfs_SourceFile, lfs_SourceFile.Size);
      finally
        FreeAndNil( lfs_TargetFile );
      end;// try..finally
//    CopyFile(PChar(as_sourceFile), PChar(as_targetFile), FALSE);
    finally
      FreeAndNil( lfs_SourceFile );
    end; // try..finally
    Application.ProcessMessages;
  end; // procedure TFileCopyTest.CopyFileToTarget

  procedure TFileCopyTest.CreateSource(as_filename: string
                                       );
  var
    ltxtfile_out  : TextFile;
    ls_OutLine    : array[ 0..99 ] of string; // 100 strings
    n, i, j       : integer;
    li_randomChar : integer;
    li_iterate    : integer;
    li_blockSize  : integer;
  begin
    if assigned( ffrmStatus ) then ffrmStatus.AddLine('Creating source file...');
    AssignFile( ltxtfile_out, as_filename );
    Rewrite( ltxtfile_out );
    li_blockSize := ONE_KILOBYTE;
    li_iterate := round( fFileSize / li_blockSize );
    // build 100 random strings, each element of size li_blockSize:
    for j := 0 to 99 do begin
      // initialize string:
      ls_OutLine[j] := '';
      // build output string of size li_blockSize:
      for i := 1 to li_blockSize do begin
        li_randomChar := Random(94) + 32;
        ls_OutLine[j] := ls_OutLine[j] + chr(li_randomChar);
      end; // for i
    end; // for
    for n := 1 to li_iterate do begin
      // write random 1kB random character string:
      i := Random( 100 );
      Write( ltxtfile_out, ls_OutLine[ i ] );
    end;  // for n
    CloseFile( ltxtfile_out );
    if assigned( ffrmStatus ) then ffrmStatus.AddLine('Source file complete.');
  end;  // procedure TFileCopyTest.CreateSource

  function TFileCopyTest.EnoughDriveSpace(achr_Drive : char): Boolean;
  var
    le_SpaceNeeded : extended;
    li_DriveNbr : integer;
    le_AmtFree : extended;
    le_TotalSpace : extended;
    ls_line : string;

  begin
    result := TRUE;
    li_DriveNbr := DriveNbr( achr_Drive );
    le_AmtFree := DiskFree( li_DriveNbr );
    le_TotalSpace := DiskSize( li_DriveNbr );
    le_SpaceNeeded := 6 * fFileSize;
    ls_line := Format('Drive "%s": %14.0n free bytes; %14.0n total bytes.',
                      [achr_Drive, le_AmtFree, le_TotalSpace]);
    ls_line := ls_line + CR_LF;
    ls_line := ls_line + Format('Space needed: %11.0n bytes; # of files: 6; file size: %11.0n',
               [le_SpaceNeeded, fFileSize]);
    ls_line := ls_line + CR_LF;
    if (le_SpaceNeeded > le_AmtFree) then begin
      ls_line := ls_line + Format('You do not have enough free space on drive "%s" to run this test.',
        [achr_Drive]);
      ShowMessage( ls_line );
      result := FALSE;
    end; // if
  end; // function TFileCopyTest.EnoughDriveSpace

  procedure TFileCopyTest.RunTest;
  begin
    // because file I/O is always so variable, we must copy several times
    CopyFileToTarget(fSourceFile, fTargetFile1);
    if assigned( ffrmStatus ) then ffrmStatus.AddLine('File 1 of 5 copied...');
    application.ProcessMessages;
    CopyFileToTarget(fSourceFile, fTargetFile2);
    if assigned( ffrmStatus ) then ffrmStatus.AddLine('File 2 of 5 copied...');
    application.ProcessMessages;
    CopyFileToTarget(fSourceFile, fTargetFile3);
    if assigned( ffrmStatus ) then ffrmStatus.AddLine('File 3 of 5 copied...');
    application.ProcessMessages;
    CopyFileToTarget(fSourceFile, fTargetFile4);
    if assigned( ffrmStatus ) then ffrmStatus.AddLine('File 4 of 5 copied...');
    application.ProcessMessages;
    CopyFileToTarget(fSourceFile, fTargetFile5);
    if assigned( ffrmStatus ) then ffrmStatus.AddLine('File 5 of 5 copied...');
    application.ProcessMessages;
  end; // procedure TFileCopyTest.RunTest;

  procedure TFileCopyTest.AfterTest;
  begin
    inherited;
    // don't forget to delete the files:
    DeleteFile( fTargetFile1 );
    DeleteFile( fTargetFile2 );
    DeleteFile( fTargetFile3 );
    DeleteFile( fTargetFile4 );
    DeleteFile( fTargetFile5 );
    DeleteFile( fSourceFile );
    application.ProcessMessages;
  end; // procedure TFileCopyTest.BeforeTest
// TFileCopyTest ends.....................................................

// TFPMultiplyTest..............................................................
  constructor TFPMultiplyTest.Create;
  begin
    inherited;
    fTestName   := 'Floating Point Multiply test';
    fTestDescription := 'This tests multiplies each element of a floating point array by another floating point value.';
    fTestVersion:= '1.0';
    fTestType   := ttFPU;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 3.1;
    fQTestType := qtFPMultiply;
    fPerformanceVectors := [ pvMathScienceEngineering, pv3dGaming ];
  end;

  procedure TFPMultiplyTest.BeforeTest;
  begin
    inherited;
    RandomizeArray( fDoubleArray );
  end; // procedure TFPMultiplyTest.BeforeTest

  procedure TFPMultiplyTest.RunTest;
  const
    ARRAY_SIZE = 10 * ONE_KB;
  var
    i : integer;
    j : integer;
    x : double;
  begin
    for j := 1 to 100000 do begin
      x := pi / (random( 10000 ) + 1);
      i := 1;
      while i < ARRAY_SIZE do begin
        fDoubleArray[ i ] := fDoubleArray[ i ] * x;
        fDoubleArray[ i + 1 ] := fDoubleArray[ i ] * x;
        fDoubleArray[ i + 2 ] := fDoubleArray[ i ] * x;
        fDoubleArray[ i + 3 ] := fDoubleArray[ i ] * x;
        fDoubleArray[ i + 4 ] := fDoubleArray[ i ] * x;
        fDoubleArray[ i + 5 ] := fDoubleArray[ i ] * x;
        fDoubleArray[ i + 6 ] := fDoubleArray[ i ] * x;
        fDoubleArray[ i + 7 ] := fDoubleArray[ i ] * x;
        i := i + 8;
      end; // while i
    end; // for j
  end; // procedure TFPMultiplyTest.RunTest;
// TFPMultiplyTest ends.........................................................

// TLorenzTest..........................................................
  constructor TLorenzTest.Create;
  begin
    inherited;
    fTestName   := 'Lorenz Attractor';
    fTestDescription := 'This OpenGL test is a 3d animated model of the chaotic behavior of a Lorenz attractor.  This test utilizes GLScene.';
    fTestVersion:= '1.0';
    fTestType   := tt3dGraphics;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 24.5;
    fQTestType := qtLorenz;
    fPerformanceVectors := [ pv3dGaming, pvMathScienceEngineering ];
  end;

  procedure TLorenzTest.RunTest;
  const
    // assumes that osmfiles is in the same directory as the calling program:
    RUN_FILENAME = 'osmfiles\projectionLorenz.exe';
  var
    lstrAppPath : string;
    liReturnCode : integer;
  begin
    if assigned( ffrmStatus ) then ffrmStatus.Visible := FALSE;
    try
      lstrAppPath := ExtractFilePath(Application.ExeName);
      // run the benchmark:
      liReturnCode := StartProgramWait( lstrAppPath + RUN_FILENAME, SW_MAXIMIZE );
      if liReturnCode <> 0 then raise exception.Create('The Lorenz test terminated abnormally.');
    finally
      if assigned( ffrmStatus ) then ffrmStatus.Visible := fShowStatus;
    end; //try..finally
  end; // procedure TLorenzTest.RunTest;
// TLorenzTest ends.....................................................

// TNBodyOpenGL..........................................................
  constructor TNBodyOpenGL.Create;
  begin
    inherited;
    fTestName   := 'N-Body OpenGL';
    fTestDescription := 'This is OpenGL version of the N-Body test.  This test utilizes GLScene.';
    fTestVersion:= '1.0';
    fTestType   := tt3dGraphics;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 8.6663;
    fQTestType := qtNBodyOpenGL;
    fPerformanceVectors := [ pv3dGaming, pvMathScienceEngineering ];
  end;

  procedure TNBodyOpenGL.RunTest;
  const
    // assumes that osmfiles is in the same directory as the calling program:
    RUN_FILENAME = 'osmfiles\nBody.exe';
  var
    lstrAppPath : string;
    liReturnCode : integer;
  begin
    if assigned( ffrmStatus ) then ffrmStatus.Visible := FALSE;
    try
      lstrAppPath := ExtractFilePath(Application.ExeName);
      // run the benchmark:
      liReturnCode := StartProgramWait( lstrAppPath + RUN_FILENAME, SW_MAXIMIZE );
      if liReturnCode <> 0 then raise exception.Create('The N-Body OpenGL test terminated abnormally.');
    finally
      if assigned( ffrmStatus ) then ffrmStatus.Visible := fShowStatus;
    end; //try..finally
  end; // procedure TNBodyOpenGL.RunTest;
// TNBodyOpenGL ends.....................................................


// TLaunchProgramThread.........................................................
// Class name: TLaunchProgramThread
// Author: Van Smith
// Date: October 4, 2004
constructor TLaunchProgramThread.Create;
begin
  FreeOnTerminate := TRUE;
  fIterations := 1;
  inherited Create(TRUE);  // creates suspended
  Priority := tpNormal;
  fReturnCode := 1; // 0 = okay; <>0 is error
end;

procedure TLaunchProgramThread.Execute;
var
  i : integer;
begin
  for i := 1 to fIterations do begin
    fReturnCode := StartProgramWait( fProgramCommand, SW_NORMAL );
  end; //for
end; //procedure TLaunchProgramThread.Execute;
// TLaunchProgramThread ends....................................................

// TLaunchProgramThreadTest.....................................................
  // Class name: TLaunchProgramThreadTest
  // Author: Van Smith
  // Date: October 4, 2004
  constructor TLaunchProgramThreadTest.Create;
  begin
    inherited;
    fTestName   := 'Launch Programs as Threads test';
    fTestDescription := 'This class is intended for launching specified programs from a multithreaded harness.';
    fTestVersion:= '1.0';
    fTestType   := ttThreads;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 142.6;
    fShowStatus := FALSE;
    fQTestType := qtLaunchProgramThreads;
  end; // constructor TLaunchProgramThreadTest.Create;

  destructor TLaunchProgramThreadTest.Destroy;
  begin
    inherited Destroy;
  end; // destructor TLaunchProgramThreadTest.Destroy;

  procedure TLaunchProgramThreadTest.BeforeTest;
  begin
    inherited;
    if ( fMaxNbrOfThreads < 1 )
        or
        ( fProgramCommand1 = '' ) then begin
      ShowMessage( 'You need to specify the number of threads to run and corresponding commands.' + CR_LF +
                   'The score that follows is invalid!' );
      exit;
    end;
    if ( fMaxNbrOfThreads >= 1 )
       and
       ( fProgramCommand1 <> '' ) then begin
      fProgramThread1 := TLaunchProgramThread.Create;
      fProgramThread1.OnTerminate := ThreadDone;
      fProgramThread1.ProgramCommand := fProgramCommand1;
    end; //if
    if ( fMaxNbrOfThreads >= 2 )
       and
       ( fProgramCommand1 <> '' ) then begin
      fProgramThread2 := TLaunchProgramThread.Create;
      fProgramThread2.OnTerminate := ThreadDone;
      fProgramThread2.ProgramCommand := fProgramCommand2;
    end; //if
    if ( fMaxNbrOfThreads >= 3 )
       and
       ( fProgramCommand3 <> '' ) then begin
      fProgramThread3 := TLaunchProgramThread.Create;
      fProgramThread3.OnTerminate := ThreadDone;
      fProgramThread3.ProgramCommand := fProgramCommand3;
    end; //if
    if ( fMaxNbrOfThreads >= 4 )
       and
       ( fProgramCommand4 <> '' ) then begin
      fProgramThread4 := TLaunchProgramThread.Create;
      fProgramThread4.OnTerminate := ThreadDone;
      fProgramThread4.ProgramCommand := fProgramCommand4;
    end; //if
    if ( fMaxNbrOfThreads >= 5 )
       and
       ( fProgramCommand5 <> '' ) then begin
      fProgramThread5 := TLaunchProgramThread.Create;
      fProgramThread5.OnTerminate := ThreadDone;
      fProgramThread5.ProgramCommand := fProgramCommand5;
    end; //if
    if ( fMaxNbrOfThreads >= 6 )
       and
       ( fProgramCommand6 <> '' ) then begin
      fProgramThread6 := TLaunchProgramThread.Create;
      fProgramThread6.OnTerminate := ThreadDone;
      fProgramThread6.ProgramCommand := fProgramCommand6;
    end; //if
    if ( fMaxNbrOfThreads >= 7 )
       and
       ( fProgramCommand7 <> '' ) then begin
      fProgramThread7 := TLaunchProgramThread.Create;
      fProgramThread7.OnTerminate := ThreadDone;
      fProgramThread7.ProgramCommand := fProgramCommand7;
    end; //if
    if ( fMaxNbrOfThreads >= 8 )
       and
       ( fProgramCommand8 <> '' ) then begin
      fProgramThread8 := TLaunchProgramThread.Create;
      fProgramThread8.OnTerminate := ThreadDone;
      fProgramThread8.ProgramCommand := fProgramCommand8;
    end; //if
    if ( fMaxNbrOfThreads >= 9 )
       and
       ( fProgramCommand9 <> '' ) then begin
      fProgramThread9 := TLaunchProgramThread.Create;
      fProgramThread9.OnTerminate := ThreadDone;
      fProgramThread9.ProgramCommand := fProgramCommand9;
    end; //if
    if ( fMaxNbrOfThreads >= 10 )
       and
       ( fProgramCommand10 <> '' ) then begin
      fProgramThread10 := TLaunchProgramThread.Create;
      fProgramThread10.OnTerminate := ThreadDone;
      fProgramThread10.ProgramCommand := fProgramCommand10;
    end; //if
    if ( fMaxNbrOfThreads >= 11 )
       and
       ( fProgramCommand11 <> '' ) then begin
      fProgramThread11 := TLaunchProgramThread.Create;
      fProgramThread11.OnTerminate := ThreadDone;
      fProgramThread11.ProgramCommand := fProgramCommand11;
    end; //if
    if ( fMaxNbrOfThreads >= 12 )
       and
       ( fProgramCommand12 <> '' ) then begin
      fProgramThread12 := TLaunchProgramThread.Create;
      fProgramThread12.OnTerminate := ThreadDone;
      fProgramThread12.ProgramCommand := fProgramCommand12;
    end; //if
    if ( fMaxNbrOfThreads >= 13 )
       and
       ( fProgramCommand13 <> '' ) then begin
      fProgramThread13 := TLaunchProgramThread.Create;
      fProgramThread13.OnTerminate := ThreadDone;
      fProgramThread13.ProgramCommand := fProgramCommand13;
    end; //if
    if ( fMaxNbrOfThreads >= 14 )
       and
       ( fProgramCommand14 <> '' ) then begin
      fProgramThread14 := TLaunchProgramThread.Create;
      fProgramThread14.OnTerminate := ThreadDone;
      fProgramThread14.ProgramCommand := fProgramCommand14;
    end; //if
    if ( fMaxNbrOfThreads >= 15 )
       and
       ( fProgramCommand15 <> '' ) then begin
      fProgramThread15 := TLaunchProgramThread.Create;
      fProgramThread15.OnTerminate := ThreadDone;
      fProgramThread15.ProgramCommand := fProgramCommand15;
    end; //if
    if ( fMaxNbrOfThreads >= 16 )
       and
       ( fProgramCommand16 <> '' ) then begin
      fProgramThread16 := TLaunchProgramThread.Create;
      fProgramThread16.OnTerminate := ThreadDone;
      fProgramThread16.ProgramCommand := fProgramCommand16;
    end; //if

    fNbrOfThreadsRunning := 0;
  end; // procedure TLaunchProgramThreadTest.BeforeTest

  procedure TLaunchProgramThreadTest.RunTest;
  begin
    if fMaxNbrOfThreads >= 1 then begin
      fProgramThread1.Resume;
      inc( fNbrOfThreadsRunning );
    end; //if
    if fMaxNbrOfThreads >= 2 then begin
      fProgramThread2.Resume;
      inc( fNbrOfThreadsRunning );
    end; //if
    if fMaxNbrOfThreads >= 3 then begin
      fProgramThread3.Resume;
      inc( fNbrOfThreadsRunning );
    end; //if
    if fMaxNbrOfThreads >= 4 then begin
      fProgramThread4.Resume;
      inc( fNbrOfThreadsRunning );
    end; //if
    if fMaxNbrOfThreads >= 5 then begin
      fProgramThread5.Resume;
      inc( fNbrOfThreadsRunning );
    end; //if
    if fMaxNbrOfThreads >= 6 then begin
      fProgramThread6.Resume;
      inc( fNbrOfThreadsRunning );
    end; //if
    if fMaxNbrOfThreads >= 7 then begin
      fProgramThread7.Resume;
      inc( fNbrOfThreadsRunning );
    end; //if
    if fMaxNbrOfThreads >= 8 then begin
      fProgramThread8.Resume;
      inc( fNbrOfThreadsRunning );
    end; //if
    if fMaxNbrOfThreads >= 9 then begin
      fProgramThread9.Resume;
      inc( fNbrOfThreadsRunning );
    end; //if
    if fMaxNbrOfThreads >= 10 then begin
      fProgramThread10.Resume;
      inc( fNbrOfThreadsRunning );
    end; //if
    if fMaxNbrOfThreads >= 11 then begin
      fProgramThread11.Resume;
      inc( fNbrOfThreadsRunning );
    end; //if
    if fMaxNbrOfThreads >= 12 then begin
      fProgramThread12.Resume;
      inc( fNbrOfThreadsRunning );
    end; //if
    if fMaxNbrOfThreads >= 13 then begin
      fProgramThread13.Resume;
      inc( fNbrOfThreadsRunning );
    end; //if
    if fMaxNbrOfThreads >= 14 then begin
      fProgramThread14.Resume;
      inc( fNbrOfThreadsRunning );
    end; //if
    if fMaxNbrOfThreads >= 15 then begin
      fProgramThread15.Resume;
      inc( fNbrOfThreadsRunning );
    end; //if
    if fMaxNbrOfThreads >= 16 then begin
      fProgramThread16.Resume;
      inc( fNbrOfThreadsRunning );
    end; //if
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure TLaunchProgramThreadTest.RunTest;

  procedure TLaunchProgramThreadTest.ThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    if TLaunchProgramThread( Sender ).ReturnCode <> 0 then begin
      raise EThread.Create('Thread terminated abnormally.');
    end; // if
    if fNbrOfThreadsRunning = 0 then begin
      // might need to add code here after last thread ends
    end else if fNbrOfThreadsRunning >= 1 then begin
      if assigned( ffrmStatus ) then begin
        ffrmStatus.AddLine( 'Thread complete.' +
                            IntToStr( fNbrOfThreadsRunning ) +
                            ' threads running.' );
      end;
    end;
    application.ProcessMessages;
  end; // procedure TLaunchProgramThreadTest.FibThreadDone;
// TLaunchProgramThreadTest ends ...............................................

// TPiThreadTest begins.................................................
  constructor TPiThreadTest.Create;
  begin
    inherited;
    fTestName   := 'Pi threads test';
    fTestDescription := 'Each thread calculates pi to 10,000 digits.  ' +
      'The number of Pi threads automatically corresponds ' +
      'to the number of logical processors in the system.  The Pi test is a port of ' +
      'Ray Lischner''s Delphi algorithm. See http://www.tempest-sw.com for a look at some of Ray Lischner''s famous books.';
    fTestVersion:= '1.0';
    fTestType   := ttThreads;
    fTestAuthor := 'Van Smith, Ray Lischner';
    fReferenceTime := 6.677 / 2;
    fQTestType := qtPiThreads;
    fPerformanceVectors := [ pvMathScienceEngineering ];
  end; // constructor TDhrystoneThreadTest.Create;

  procedure TPiThreadTest.BeforeTest;
  var
    i : integer;
  begin
    inherited;
    // create a thread for each processor:
    for i := 1 to fMaxNbrOfThreads do begin
      fPiThread[ i ] := TPiThread.Create( 10000 );
      fPiThread[ i ].OnTerminate := ThreadDone;
    end;
    if assigned( ffrmStatus ) then ffrmStatus.AddLine( 'Spawning ' +
                            IntToStr( fMaxNbrOfThreads ) +
                            ' thread(s)...' );
    application.ProcessMessages;
  end; // procedure TPiThreadTest.BeforeTest

  procedure TPiThreadTest.RunTest;
  var
    i : integer;
  begin
    for i := 1 to fMaxNbrOfThreads do begin
      fPiThread[ i ].Resume;
      inc( fNbrOfThreadsRunning );
    end; // for
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure TPiThreadTest.RunTest;

  procedure TPiThreadTest.ThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    application.ProcessMessages;
  end; // procedure TPiThreadTest.ThreadDone;

  function TPiThreadTest.GetScore(ai_index : integer) : integer;
  begin
    result := inherited GetScore( ai_index );
    result := fMaxNbrOfThreads * result;
  end; // function TPiThreadTest.GetScore

// TPiThreadTest ends...................................................

// TMandelbrotThread begins............................................................
constructor TMandelbrotThread.Create(aPaintBox32 : TPaintBox32);
begin
  FreeOnTerminate := TRUE;
  fFinished := TRUE;
  fPaintBox32 := aPaintBox32;
  inherited Create(TRUE);  // creates suspended
  Priority := tpNormal;
end;

procedure TMandelbrotThread.Execute;
//const
//  RADIUS =  0.0019531; //0.0625; //4.7684e-7; //0.03125; //0.3808854167;
//  X_START = -0.5622395833 - RADIUS; //-0.560833333333 - RADIUS; //-1.7664334551 - RADIUS; //-1.7764583333 - RADIUS; //-1.2563541667 - RADIUS;
//  Y_START = 0.6428320312 -RADIUS; //0.642708333333 - RADIUS; //0.0418227943 - RADIUS; //0 - RADIUS; //0.3808854167 - RADIUS;
var
  ldRadius : double;
  ldXStart : double;
  ldYStart : double;
  h : double;
  w : double;
  procedure RunFractal1;
  begin
    ldRadius :=  0.0019531; //0.0625; //4.7684e-7; //0.03125; //0.3808854167;
    ldXStart := -0.5622395833 - ldRadius; //-0.560833333333 - RADIUS; //-1.7664334551 - RADIUS; //-1.7764583333 - RADIUS; //-1.2563541667 - RADIUS;
    ldYStart := 0.6428320312 - ldRadius; //0.642708333333 - RADIUS; //0.0418227943 - RADIUS; //0 - RADIUS; //0.3808854167 - RADIUS;
//    fColor  := $11111;; //$000002;
    fFinished := FALSE;
    with fPaintBox32 do begin
      if width > height then begin
        h := 2 * ldRadius;  //2.8; << // original value
        w := ( Width / Height ) * h;
        fyScale := Height / h;
        fxScale := fyScale; //2.8 / Width;
      end else begin
        w := 2 * ldRadius;  //2.8; << // original value
        h := (height / width ) * w;
        fxScale := width / w;
        fyScale := fxScale; // maintain 1-to-1
      end; // if
      fxOffset := -fxScale * ldXStart;
      fyOffset := -fyScale * ldYStart;
      fWidth := Width;
      fHeight := Height;
    end; // with
    ComputeMandelbrot;
  end; // procedure RunFractal1;
  procedure RunFractal2;
  begin
    ldRadius :=  0.0625; //4.7684e-7; //0.03125; //0.3808854167;
    ldXStart := -0.560833333333 - ldRadius; //-0.560833333333 - RADIUS; //-1.7664334551 - RADIUS; //-1.7764583333 - RADIUS; //-1.2563541667 - RADIUS;
    ldYStart := 0.642708333333 - ldRadius; //0.642708333333 - RADIUS; //0.0418227943 - RADIUS; //0 - RADIUS; //0.3808854167 - RADIUS;
//    fColor := $000002;
    fFinished := FALSE;
    with fPaintBox32 do begin
      if width > height then begin
        h := 2 * ldRadius;  //2.8; << // original value
        w := ( Width / Height ) * h;
        fyScale := Height / h;
        fxScale := fyScale; //2.8 / Width;
      end else begin
        w := 2 * ldRadius;  //2.8; << // original value
        h := (height / width ) * w;
        fxScale := width / w;
        fyScale := fxScale; // maintain 1-to-1
      end; // if
      fxOffset := -fxScale * ldXStart;
      fyOffset := -fyScale * ldYStart;
      fWidth := Width;
      fHeight := Height;
    end; // with
    ComputeMandelbrot;
  end; // procedure RunFractal1;
begin
//  RunFractal2;
  RunFractal1;
  if assigned( fMandelbrotThreadDone ) then fMandelbrotThreadDone( self );
end; //procedure TMandelbrotThread.Execute;

procedure TMandelbrotThread.ComputeMandelbrot;
var
  x, y, i : integer;
  lComplexC, lComplexN, lComplexNminus1 : TComplexNumber;
//  lComplexC, lComplexN, lComplexNminus1 : variant;
{ Algorithm taken from:
  http://pass.maths.org.uk/issue9/features/mandelbrot/index.html
  1. Let c be a complex constant defined according to the position on screen.
     c = a + bi = xMandelbrot + yMandelbrot * i
  2. Let z(0) = 0 be our first complex iteration.
  3. Iterate by letting z(n) = z(n-1) ^ 2 + c.
  4. Repeat step 3 until one of the following occurs:
    * (a) The modulus ("length" determined by sqrt( a^2 + b^2)) of z(n) exceeds
          a given value.  This is our test for divergence.
    * (b) The number of iterations exceeds a given value.
          This is how we assume convergence.
  5. Draw the point with colour determined by the number of iterations. Often the convergent points are drawn in black.
}
const
  CONVERGENCE_THRESHOLD = 512;
  DIVERGENCE_THRESHOLD = 4;
begin
  for y := fStartY to fEndY do begin
    for x := 0 to fWidth do begin
      // translate screen coordinate to mandelbrot space
      lComplexC.Real := (x - FxOffset) / FxScale;
      lComplexC.Imaginary := (fHeight - y - FyOffset) / FyScale;
      lComplexNminus1 := lComplexC;
      // initialize loop counter:
      i := 0;
      repeat
        // z(n) := z(n-1)^2 + c:
        lComplexN := ComplexNumberAdd(
                                       ComplexNumberSquare( lComplexNminus1 ),
                                       lComplexC
                                      );
        // store z(n) into z(n-1) for next loop:
        lComplexNminus1 := lComplexN;
        // increment loop counter:
        i := i + 1;
        // i > CONVERGENCE_THRESHOLD: we have convergence,
        // if modulus( z(n) ) > DIVERGENCE_THRESHOLD : we have divergence:
      until ( i > CONVERGENCE_THRESHOLD )
         or ( ComplexNumberModulus( lComplexN ) > DIVERGENCE_THRESHOLD );
      // if the loop exited because the CONVERGENCE_THRESHOLD was  exceeded,
      // we assume that the point converges and assign it black:
      if i > CONVERGENCE_THRESHOLD then i := 0;
      Fy := y;
      FScanline[ x ] := (i-32) * fColor; //i * $11111; ////32 + (i * $fffff) ;
    end; // for x
    Synchronize( PlotScanLine );
  end; // for y
end; //function TMandelbrotThread.ComputeMandelbrot

procedure TMandelbrotThread.PlotScanLine;
var
  x : integer;
begin
  // plot scanline
  with fPaintBox32 do begin
    for x := 0 to FWidth do begin
      Canvas.Pixels[ x, Fy ] := FScanline[ x ] ;
    end; //for
    Canvas.Refresh;
    application.ProcessMessages;
    Canvas.Refresh;
  end; // with
end;


// TMandelbrotThread ends............................................................

// TMandelbrotThreadTest begins.................................................
  constructor TMandelbrotThreadTest.Create;
  begin
    inherited;
    fTestName   := 'Mandelbrot threads test';
    fTestDescription := 'Generates the famous Mandelbrot fractal.';
    fTestVersion:= '1.0';
    fTestType   := ttThreads;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 3.603;  // not x2 because workload is divided
    fQTestType := qtMandelbrotThreads;
    fPerformanceVectors := [ pvMathScienceEngineering, pv3dGaming ];
  end; // constructor TMandelbrotThreadTest.Create;

  procedure TMandelbrotThreadTest.BeforeTest;
  var
    i : integer;
    liYSlice : integer;
    liColor : integer;
  begin
    inherited;
    liYSlice := fPaintBox32.Height div fMaxNbrOfThreads;
    Randomize;
    if random( 2 ) = 0 then begin
      liColor := random( $ffffff );
    end else begin
      i := random( 10000 );
      if i < 3333 then begin
        liColor  := $11111;
      end else if i > 6666 then begin
        liColor  := $000002;
      end else begin
        liColor := $30303;
      end; // if
    end; // if
    // create the threads:
    for i := 1 to fMaxNbrOfThreads do begin
      fMandelbrotThread[ i ] := TMandelbrotThread.Create( fPaintBox32 );
      fMandelbrotThread[ i ].OnTerminate := ThreadDone;
      fMandelbrotThread[ i ].StartY := (i - 1) * liYSlice;
      if i < fMaxNbrOfThreads then begin
        fMandelbrotThread[ i ].EndY := ( i * liYSlice ) - 1;
      end else begin
        fMandelbrotThread[ i ].EndY := fPaintBox32.Height;
      end; // if
      fMandelbrotThread[ i ].Color := liColor;
    end;

    if assigned( ffrmStatus ) then ffrmStatus.AddLine( 'Spawning ' +
                            IntToStr( fMaxNbrOfThreads ) +
                            ' thread(s)...' );
    application.ProcessMessages;
  end; // procedure TMandelbrotThreadTest.BeforeTest

  procedure TMandelbrotThreadTest.RunTest;
  var
    i : integer;
  begin
    for i := 1 to fMaxNbrOfThreads do begin
      fMandelbrotThread[ i ].Resume;
      inc( fNbrOfThreadsRunning );
    end; // for
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure TMandelbrotThreadTest.RunTest;

  procedure TMandelbrotThreadTest.ThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    // there's no need to update the status window since all of the threads
    // should end at almost the same time.
//    if fNbrOfThreadsRunning = 0 then begin
//      // might need to add code here after last thread ends
//    end else if fNbrOfThreadsRunning >= 1 then begin
//      if assigned( ffrmStatus ) then begin
//        ffrmStatus.AddLine( 'Thread complete.' +
//                            IntToStr( fNbrOfThreadsRunning ) +
//                            ' threads running.' );
//      end;
//    end;
    application.ProcessMessages;
  end; // procedure TMandelbrotThreadTest.ThreadDone;

  function TMandelbrotThreadTest.GetScore(ai_index : integer) : integer;
  begin
    result := inherited GetScore( ai_index );
  end; // function TMandelbrotThreadTest.GetScore

// TMandelbrotThreadTest ends...................................................

// TDrawLinesTest begins.......................................................
  constructor TDrawLinesTest.Create;
  begin
    inherited;
    fNumberOfLines := 1000000;
    fTestName   := 'Draw Lines Test';
    fTestDescription := 'Draws ' + intToStr( fNumberOfLines ) + ' lines randomly over canvas';
    fTestVersion:= '1.0';
    fTestType   := ttCpu;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 3.63;
    fQTestType := qtDrawLines;
    fPerformanceVectors := [ pvResponsiveness ];
  end;

  procedure TDrawLinesTest.RunTest;
  var
    i, x1, y1, x2, y2, lImageHeight, lImageWidth : integer;
    lRed, lGreen, lBlue : byte;
    lColor : TColor;
  begin
    RandSeed := 1;
    with fPaintBox32 do begin
      lImageHeight := Height;
      lImageWidth := Width;
      for i := 0 to fNumberOfLines do begin
        x1 := Random( lImageWidth );
        x2 := Random( lImageWidth );
        y1 := Random( lImageHeight );
        y2 := Random( lImageHeight );
        lRed := random( 256 );
        lGreen := random( 256 );
        lBlue := random( 256 );
        lColor := lBlue * $010000 + lGreen * $000100 + lRed;
        Canvas.Pen.Color := lColor;
        Canvas.MoveTo( x1, y1 );
        Canvas.LineTo( x2, y2 );
      end; // for
    end; // width
  end; // procedure TDrawLinesTest.RunTest;
// TDrawLinesTest ends.........................................................

// TDrawEllipsesTest begins.......................................................
  constructor TDrawEllipsesTest.Create;
  begin
    inherited;
    fNumberOfEllipses := 100000;
    fTestName   := 'Draw Ellipses';
    fTestDescription := 'Draws ' + intToStr( fNumberOfEllipses ) + ' ellipses randomly over canvas';
    fTestVersion:= '1.0';
    fTestType   := ttCpu;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 5.73937;
    fQTestType := qtDrawEllipses;
    fPerformanceVectors := [ pvResponsiveness ];
  end;

  procedure TDrawEllipsesTest.RunTest;
  var
    i, x1, y1, x2, y2 : integer;
    lRed, lGreen, lBlue : byte;
    lColor : TColor;
    lImageHeight : integer;
    lImageWidth : integer;
  begin
    with fPaintBox32 do begin
      // Random dots:
      RandSeed := 1;
      lImageHeight := Height;
      lImageWidth := Width;
      for i := 0 to 100000 do begin
        x1 := Random( lImageWidth );
        x2 := Random( lImageWidth );
        y1 := Random( lImageHeight );
        y2 := Random( lImageHeight );
        lRed := random( 256 );
        lGreen := random( 256 );
        lBlue := random( 256 );
        lColor := lBlue * $010000 + lGreen * $000100 + lRed;
        Canvas.Pen.Color := lColor;
        Canvas.Brush.Style := bsClear;
        Canvas.Ellipse( x1, y1, x2, y2 );
      end; // for
    end; // with
  end; // procedure TDrawEllipsesTest.RunTest;
// TDrawEllipsesTest ends.......................................................

// TDrawRectanglesTest begins.......................................................
  constructor TDrawRectanglesTest.Create;
  begin
    inherited;
    fNumberOfRectangles := 1000000;
    fTestName   := 'Draw Rectangles';
    fTestDescription := 'Draws ' + intToStr( fNumberOfRectangles ) + ' rectangles randomly over canvas';
    fTestVersion:= '1.0';
    fTestType   := ttCpu;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 5.05168;
    fQTestType := qtDrawRectangles;
    fPerformanceVectors := [ pvResponsiveness ];
  end;

  procedure TDrawRectanglesTest.RunTest;
  var
    i, x1, y1, x2, y2 : integer;
    lRed, lGreen, lBlue : byte;
    lColor : TColor;
    lImageHeight : integer;
    lImageWidth : integer;
  begin
    with fPaintBox32 do begin
      // Random dots:
      RandSeed := 1;
      lImageHeight := Height;
      lImageWidth := Width;
      for i := 0 to fNumberOfRectangles do begin
        x1 := Random( lImageWidth );
        x2 := Random( lImageWidth );
        y1 := Random( lImageHeight );
        y2 := Random( lImageHeight );
        lRed := random( 256 );
        lGreen := random( 256 );
        lBlue := random( 256 );
        lColor := lBlue * $010000 + lGreen * $000100 + lRed;
        Canvas.Pen.Color := lColor;
        Canvas.Brush.Style := bsClear;
        Canvas.Rectangle( x1, y1, x2, y2 );
      end; // for
    end; // with
  end; // procedure TDrawRectanglesTest.RunTest;
// TDrawRectanglesTest ends.........................................................

// TFibThreadTest begins.................................................
  constructor TFibThreadTest.Create;
  begin
    inherited;
    fTestName   := 'Fibonacci threads test';
    fTestDescription := 'Each thread calculates Fibonacci( 40 ) sequence.  ' +
      'The number of threads spawned automatically corresponds ' +
      'to the number of logical processors in the system.';
    fTestVersion:= '1.0';
    fTestType   := ttThreads;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 3.31852 / 2;
    fQTestType := qtFibThreads;
    fPerformanceVectors := [ pvMathScienceEngineering ];
  end; // constructor TDhrystoneThreadTest.Create;

  procedure TFibThreadTest.BeforeTest;
  var
    i : integer;
  begin
    inherited;
    // create a thread for each processor:
    for i := 1 to fMaxNbrOfThreads do begin
      fFibThread[ i ] := TFibThread.Create( 40 );
      fFibThread[ i ].OnTerminate := ThreadDone;
    end;
    if assigned( ffrmStatus ) then ffrmStatus.AddLine( 'Spawning ' +
                            IntToStr( fMaxNbrOfThreads ) +
                            ' thread(s)...' );
    application.ProcessMessages;
  end; // procedure TFibThreadTest.BeforeTest

  procedure TFibThreadTest.RunTest;
  var
    i : integer;
  begin
    for i := 1 to fMaxNbrOfThreads do begin
      fFibThread[ i ].Resume;
      inc( fNbrOfThreadsRunning );
    end; // for
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure TFibThreadTest.RunTest;

  procedure TFibThreadTest.ThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    // there's no need to update the status window since all of the threads
    // should end at almost the same time.
//    if fNbrOfThreadsRunning = 0 then begin
//      // might need to add code here after last thread ends
//    end else if fNbrOfThreadsRunning >= 1 then begin
//      if assigned( ffrmStatus ) then begin
//        ffrmStatus.AddLine( 'Thread complete.' +
//                            IntToStr( fNbrOfThreadsRunning ) +
//                            ' threads running.' );
//      end;
//    end;
    application.ProcessMessages;
  end; // procedure TFibThreadTest.ThreadDone;

  function TFibThreadTest.GetScore(ai_index : integer) : integer;
  begin
    result := inherited GetScore( ai_index );
    result := fMaxNbrOfThreads * result;
  end; // function TFibThreadTest.GetScore

// TFibThreadTest ends...................................................


// TBandwidthBP64Thread.........................................................
constructor TBandwidthBP64Thread.Create;
begin
  FreeOnTerminate := TRUE;
  fArraySize := 2 * ONE_MEGABYTE;
  SetLength(fA, fArraySize);
  inherited Create(TRUE);  // creates suspended
  Priority := tpNormal;
end;

procedure TBandwidthBP64Thread.Execute;
var
  leTime : extended;
begin
  leTime := BlockPrefetch64( 200, fArraySize );  // 4000000
end; //procedure TBandwidthBP64Thread.Execute;

function TBandwidthBP64Thread.BlockPrefetch64( ai_RepeatCount : integer;
                                             ai_ArraySize   : integer
                                             ): Extended;
var
  li64_StartCycle   : Int64;
  li64_EndCycle     : Int64;
  li_RandomData     : integer;
  lc_temp           : integer;
  le_elapsed_time   : extended;
  i, j              : integer;
  li_iterate        : integer;
begin
  // initialize variables
  le_elapsed_time := 0;
  lc_temp := 0;
  Randomize;
  li_RandomData   := Random( 1000000 );
  for li_iterate := 1 to ai_RepeatCount do begin
    // at each step we need to populate the array:
    i := 0;
    while i < ai_ArraySize do begin
      // start timer
      li64_StartCycle := GetCycleCount;
      // block prefetch 8 64-byte lines:
      fA[i] := li_RandomData;
      fA[i + 16] := li_RandomData;
      fA[i + 32] := li_RandomData;
      fA[i + 48] := li_RandomData;
      fA[i + 64] := li_RandomData;
      fA[i + 80] := li_RandomData;
      fA[i + 96] := li_RandomData;
      fA[i + 112] := li_RandomData;

      // copy data
//      fA[i] := li_RandomData;
      fA[i + 1] := li_RandomData;
      fA[i + 2] := li_RandomData;
      fA[i + 3] := li_RandomData;
      fA[i + 4] := li_RandomData;

      fA[i + 5] := li_RandomData;
      fA[i + 6] := li_RandomData;
      fA[i + 7] := li_RandomData;
      fA[i + 8] := li_RandomData;
      fA[i + 9] := li_RandomData;

      fA[i + 10] := li_RandomData;
      fA[i + 11] := li_RandomData;
      fA[i + 12] := li_RandomData;
      fA[i + 13] := li_RandomData;
      fA[i + 14] := li_RandomData;

      fA[i + 15] := li_RandomData;
//      fA[i + 16] := li_RandomData;
      fA[i + 17] := li_RandomData;
      fA[i + 18] := li_RandomData;
      fA[i + 19] := li_RandomData;

      fA[i + 20] := li_RandomData;
      fA[i + 21] := li_RandomData;
      fA[i + 22] := li_RandomData;
      fA[i + 23] := li_RandomData;
      fA[i + 24] := li_RandomData;

      fA[i + 25] := li_RandomData;
      fA[i + 26] := li_RandomData;
      fA[i + 27] := li_RandomData;
      fA[i + 28] := li_RandomData;
      fA[i + 29] := li_RandomData;

      fA[i + 30] := li_RandomData;
      fA[i + 31] := li_RandomData;
//      fA[i + 32] := li_RandomData;
      fA[i + 33] := li_RandomData;
      fA[i + 34] := li_RandomData;

      fA[i + 35] := li_RandomData;
      fA[i + 36] := li_RandomData;
      fA[i + 37] := li_RandomData;
      fA[i + 38] := li_RandomData;
      fA[i + 39] := li_RandomData;

      fA[i + 40] := li_RandomData;
      fA[i + 41] := li_RandomData;
      fA[i + 42] := li_RandomData;
      fA[i + 43] := li_RandomData;
      fA[i + 44] := li_RandomData;

      fA[i + 45] := li_RandomData;
      fA[i + 46] := li_RandomData;
      fA[i + 47] := li_RandomData;
//      fA[i + 48] := li_RandomData;
      fA[i + 49] := li_RandomData;

      fA[i + 50] := li_RandomData;
      fA[i + 51] := li_RandomData;
      fA[i + 52] := li_RandomData;
      fA[i + 53] := li_RandomData;
      fA[i + 54] := li_RandomData;

      fA[i + 55] := li_RandomData;
      fA[i + 56] := li_RandomData;
      fA[i + 57] := li_RandomData;
      fA[i + 58] := li_RandomData;
      fA[i + 59] := li_RandomData;

      fA[i + 60] := li_RandomData;
      fA[i + 61] := li_RandomData;
      fA[i + 62] := li_RandomData;
      fA[i + 63] := li_RandomData;
//      fA[i + 64] := li_RandomData;

      fA[i + 65] := li_RandomData;
      fA[i + 66] := li_RandomData;
      fA[i + 67] := li_RandomData;
      fA[i + 68] := li_RandomData;
      fA[i + 69] := li_RandomData;

      fA[i + 70] := li_RandomData;
      fA[i + 71] := li_RandomData;
      fA[i + 72] := li_RandomData;
      fA[i + 73] := li_RandomData;
      fA[i + 74] := li_RandomData;

      fA[i + 75] := li_RandomData;
      fA[i + 76] := li_RandomData;
      fA[i + 77] := li_RandomData;
      fA[i + 78] := li_RandomData;
      fA[i + 79] := li_RandomData;

//      fA[i + 80] := li_RandomData;
      fA[i + 81] := li_RandomData;
      fA[i + 82] := li_RandomData;
      fA[i + 83] := li_RandomData;
      fA[i + 84] := li_RandomData;

      fA[i + 85] := li_RandomData;
      fA[i + 86] := li_RandomData;
      fA[i + 87] := li_RandomData;
      fA[i + 88] := li_RandomData;
      fA[i + 89] := li_RandomData;

      fA[i + 90] := li_RandomData;
      fA[i + 91] := li_RandomData;
      fA[i + 92] := li_RandomData;
      fA[i + 93] := li_RandomData;
      fA[i + 94] := li_RandomData;

      fA[i + 95] := li_RandomData;
//      fA[i + 96] := li_RandomData;
      fA[i + 97] := li_RandomData;
      fA[i + 98] := li_RandomData;
      fA[i + 99] := li_RandomData;

      fA[i + 100] := li_RandomData;
      fA[i + 101] := li_RandomData;
      fA[i + 102] := li_RandomData;
      fA[i + 103] := li_RandomData;
      fA[i + 104] := li_RandomData;

      fA[i + 105] := li_RandomData;
      fA[i + 106] := li_RandomData;
      fA[i + 107] := li_RandomData;
      fA[i + 108] := li_RandomData;
      fA[i + 109] := li_RandomData;

      fA[i + 110] := li_RandomData;
      fA[i + 111] := li_RandomData;
//      fA[i + 112] := li_RandomData;
      fA[i + 113] := li_RandomData;
      fA[i + 114] := li_RandomData;

      fA[i + 115] := li_RandomData;
      fA[i + 116] := li_RandomData;
      fA[i + 117] := li_RandomData;
      fA[i + 118] := li_RandomData;
      fA[i + 119] := li_RandomData;

      fA[i + 120] := li_RandomData;
      fA[i + 121] := li_RandomData;
      fA[i + 122] := li_RandomData;
      fA[i + 123] := li_RandomData;
      fA[i + 124] := li_RandomData;

      fA[i + 125] := li_RandomData;
      fA[i + 126] := li_RandomData;
      fA[i + 127] := li_RandomData;

      li64_EndCycle := GetCycleCount;
      le_elapsed_time := ( li64_EndCycle - li64_StartCycle );

      i := i + 128;

    end; // while...

    // read back data: must accumulate or optimizer will remove
    lc_temp := 0;
    j := 0;
    while j < ai_ArraySize do begin

      // start timer
      li64_StartCycle := GetCycleCount;

      // block prefetch 8 64-byte lines:
      lc_temp := fA[j] - lc_temp;
      lc_temp := fA[ j + 16 ] + lc_temp;
      lc_temp := fA[ j + 32 ] - lc_temp;
      lc_temp := fA[ j + 48 ] + lc_temp;
      lc_temp := fA[ j + 64 ] - lc_temp;
      lc_temp := fA[ j + 80 ] + lc_temp;
      lc_temp := fA[ j + 96 ] - lc_temp;
      lc_temp := fA[ j + 112 ] + lc_temp;

      // copy data
//      lc_temp := fA[ j ] + lc_temp;
      lc_temp := fA[ j + 1 ] - lc_temp;
      lc_temp := fA[ j + 2 ] + lc_temp;
      lc_temp := fA[ j + 3 ] - lc_temp;
      lc_temp := fA[ j + 4 ] + lc_temp;

      lc_temp := fA[ j + 5 ] - lc_temp;
      lc_temp := fA[ j + 6 ] + lc_temp;
      lc_temp := fA[ j + 7 ] - lc_temp;
      lc_temp := fA[ j + 8 ] + lc_temp;
      lc_temp := fA[ j + 9 ] - lc_temp;

      lc_temp := fA[ j + 10 ] + lc_temp;
      lc_temp := fA[ j + 11 ] - lc_temp;
      lc_temp := fA[ j + 12 ] + lc_temp;
      lc_temp := fA[ j + 13 ] - lc_temp;
      lc_temp := fA[ j + 14 ] + lc_temp;

      lc_temp := fA[ j + 15 ] - lc_temp;
//      lc_temp := fA[j+16] + lc_temp;
      lc_temp := fA[ j + 17 ] + lc_temp;
      lc_temp := fA[ j + 18 ] - lc_temp;
      lc_temp := fA[ j + 19 ] + lc_temp;

      lc_temp := fA[ j + 20 ] - lc_temp;
      lc_temp := fA[ j + 21 ] + lc_temp;
      lc_temp := fA[ j + 22 ] - lc_temp;
      lc_temp := fA[ j + 23 ] + lc_temp;
      lc_temp := fA[ j + 24 ] - lc_temp;

      lc_temp := fA[ j + 25 ] + lc_temp;
      lc_temp := fA[ j + 26 ] - lc_temp;
      lc_temp := fA[ j + 27 ] + lc_temp;
      lc_temp := fA[ j + 28 ] - lc_temp;
      lc_temp := fA[ j + 29 ] + lc_temp;

      lc_temp := fA[ j + 30 ] - lc_temp;
      lc_temp := fA[ j + 31 ] + lc_temp;
//      lc_temp := fA[j+32] + lc_temp;
      lc_temp := fA[ j + 33 ] - lc_temp;
      lc_temp := fA[ j + 34 ] + lc_temp;

      lc_temp := fA[ j + 35 ] - lc_temp;
      lc_temp := fA[ j + 36 ] + lc_temp;
      lc_temp := fA[ j + 37 ] - lc_temp;
      lc_temp := fA[ j + 38 ] + lc_temp;
      lc_temp := fA[ j + 39 ] - lc_temp;

      lc_temp := fA[ j + 40 ] + lc_temp;
      lc_temp := fA[ j + 41 ] - lc_temp;
      lc_temp := fA[ j + 42 ] + lc_temp;
      lc_temp := fA[ j + 43 ] - lc_temp;
      lc_temp := fA[ j + 44 ] + lc_temp;

      lc_temp := fA[ j + 45 ] - lc_temp;
      lc_temp := fA[ j + 46 ] + lc_temp;
      lc_temp := fA[ j + 47 ] - lc_temp;
//      lc_temp := fA[j +48] + lc_temp;
      lc_temp := fA[ j + 49 ] + lc_temp;

      lc_temp := fA[ j + 50 ] - lc_temp;
      lc_temp := fA[ j + 51 ] + lc_temp;
      lc_temp := fA[ j + 52 ] - lc_temp;
      lc_temp := fA[ j + 53 ] + lc_temp;
      lc_temp := fA[ j + 54 ] - lc_temp;

      lc_temp := fA[ j + 55 ] + lc_temp;
      lc_temp := fA[ j + 56 ] - lc_temp;
      lc_temp := fA[ j + 57 ] + lc_temp;
      lc_temp := fA[ j + 58 ] - lc_temp;
      lc_temp := fA[ j + 59 ] + lc_temp;

      lc_temp := fA[ j + 60 ] - lc_temp;
      lc_temp := fA[ j + 61 ] + lc_temp;
      lc_temp := fA[ j + 62 ] - lc_temp;
      lc_temp := fA[ j + 63 ] + lc_temp;
//      lc_temp := fA[j+64] + lc_temp;

      lc_temp := fA[ j + 65 ] - lc_temp;
      lc_temp := fA[ j + 66 ] + lc_temp;
      lc_temp := fA[ j + 67 ] - lc_temp;
      lc_temp := fA[ j + 68 ] + lc_temp;
      lc_temp := fA[ j + 69 ] - lc_temp;

      lc_temp := fA[ j + 70 ] + lc_temp;
      lc_temp := fA[ j + 71 ] - lc_temp;
      lc_temp := fA[ j + 72 ] + lc_temp;
      lc_temp := fA[ j + 73 ] - lc_temp;
      lc_temp := fA[ j + 74 ] + lc_temp;

      lc_temp := fA[ j + 75 ] - lc_temp;
      lc_temp := fA[ j + 76 ] + lc_temp;
      lc_temp := fA[ j + 77 ] - lc_temp;
      lc_temp := fA[ j + 78 ] + lc_temp;
      lc_temp := fA[ j + 79 ] - lc_temp;

//      lc_temp := fA[j+80] + lc_temp;
      lc_temp := fA[ j + 81 ] + lc_temp;
      lc_temp := fA[ j + 82 ] - lc_temp;
      lc_temp := fA[ j + 83 ] + lc_temp;
      lc_temp := fA[ j + 84 ] - lc_temp;

      lc_temp := fA[ j + 85 ] + lc_temp;
      lc_temp := fA[ j + 86 ] - lc_temp;
      lc_temp := fA[ j + 87 ] + lc_temp;
      lc_temp := fA[ j + 88 ] - lc_temp;
      lc_temp := fA[ j + 89 ] + lc_temp;

      lc_temp := fA[ j + 90 ] - lc_temp;
      lc_temp := fA[ j + 91 ] + lc_temp;
      lc_temp := fA[ j + 92 ] - lc_temp;
      lc_temp := fA[ j + 93 ] + lc_temp;
      lc_temp := fA[ j + 94 ] - lc_temp;

      lc_temp := fA[ j + 95 ] + lc_temp;
//      lc_temp := fA[j+96] + lc_temp;
      lc_temp := fA[ j + 97 ] - lc_temp;
      lc_temp := fA[ j + 98 ] + lc_temp;
      lc_temp := fA[ j + 99 ] - lc_temp;

      lc_temp := fA[ j + 100 ] + lc_temp;
      lc_temp := fA[ j + 101 ] - lc_temp;
      lc_temp := fA[ j + 102 ] + lc_temp;
      lc_temp := fA[ j + 103 ] - lc_temp;
      lc_temp := fA[ j + 104 ] + lc_temp;

      lc_temp := fA[ j + 105 ] - lc_temp;
      lc_temp := fA[ j + 106 ] + lc_temp;
      lc_temp := fA[ j + 107 ] - lc_temp;
      lc_temp := fA[ j + 108 ] + lc_temp;
      lc_temp := fA[ j + 109 ] - lc_temp;

      lc_temp := fA[ j + 110 ] + lc_temp;
      lc_temp := fA[ j + 111 ] - lc_temp;
//      lc_temp := fA[j+112] + lc_temp;
      lc_temp := fA[ j + 113 ] + lc_temp;
      lc_temp := fA[ j + 114 ] - lc_temp;

      lc_temp := fA[ j + 115 ] + lc_temp;
      lc_temp := fA[ j + 116 ] - lc_temp;
      lc_temp := fA[ j + 117 ] + lc_temp;
      lc_temp := fA[ j + 118 ] - lc_temp;
      lc_temp := fA[ j + 119 ] + lc_temp;

      lc_temp := fA[ j + 120 ] - lc_temp;
      lc_temp := fA[ j + 121 ] + lc_temp;
      lc_temp := fA[ j + 122 ] - lc_temp;
      lc_temp := fA[ j + 123 ] + lc_temp;
      lc_temp := fA[ j + 124 ] - lc_temp;

      lc_temp := fA[ j + 125 ] + lc_temp;
      lc_temp := fA[ j + 126 ] - lc_temp;
      lc_temp := fA[ j + 127 ] + lc_temp;

      li64_EndCycle := GetCycleCount;
      le_elapsed_time := le_elapsed_time +
        ( li64_EndCycle - li64_StartCycle );

      j := j + 128;

    end; // while...

  end; // for...

  // assign to dummy variable or optimizer will remove:
  FDummy := lc_temp;
  result := le_elapsed_time;
end; // function TBandwidthBP64Thread.BlockPrefetch64
// TBandwidthBP64Thread ends...............................................................

// TBandwidthBP64ThreadTest begins.................................................
  constructor TBandwidthBP64ThreadTest.Create;
  begin
    inherited;
    fTestName   := 'BandwidthBP64 threads test';
    fTestDescription := 'Each thread tests main memory bandwidth over 8MB blocks using the Block Prefetch64 technique..' +
      'The number of threads spawned automatically corresponds ' +
      'to the number of logical processors in the system.';
    fTestVersion:= '1.0';
    fTestType   := ttMemory;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 4.65022 / 2;
    fQTestType := qtBandwidthBP64;
    fPerformanceVectors := [ pvDataSetTransform, pvMathScienceEngineering, pv3dGaming ];
  end; // constructor TBandwidthBP64ThreadTest.Create;

  procedure TBandwidthBP64ThreadTest.BeforeTest;
  var
    i : integer;
  begin
    inherited;
    // create a thread for each processor:
    for i := 1 to fMaxNbrOfThreads do begin
      fBandwidthBP64Thread[ i ] := TBandwidthBP64Thread.Create;
      fBandwidthBP64Thread[ i ].OnTerminate := ThreadDone;
    end;
    if assigned( ffrmStatus ) then ffrmStatus.AddLine( 'Spawning ' +
                            IntToStr( fMaxNbrOfThreads ) +
                            ' thread(s)...' );
    application.ProcessMessages;
  end; // procedure TBandwidthBP64ThreadTest.BeforeTest

  procedure TBandwidthBP64ThreadTest.RunTest;
  var
    i : integer;
  begin
    for i := 1 to fMaxNbrOfThreads do begin
      fBandwidthBP64Thread[ i ].Resume;
      inc( fNbrOfThreadsRunning );
    end; // for
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure TBandwidthBP64ThreadTest.RunTest;

  procedure TBandwidthBP64ThreadTest.ThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    // there's no need to update the status window since all of the threads
    // should end at almost the same time.
//    if fNbrOfThreadsRunning = 0 then begin
//      // might need to add code here after last thread ends
//    end else if fNbrOfThreadsRunning >= 1 then begin
//      if assigned( ffrmStatus ) then begin
//        ffrmStatus.AddLine( 'Thread complete.' +
//                            IntToStr( fNbrOfThreadsRunning ) +
//                            ' threads running.' );
//      end;
//    end;
    application.ProcessMessages;
  end; // procedure TBandwidthBP64ThreadTest.ThreadDone;

  function TBandwidthBP64ThreadTest.GetScore(ai_index : integer) : integer;
  begin
    result := inherited GetScore( ai_index );
    result := fMaxNbrOfThreads * result;
  end; // function TBandwidthBP64ThreadTest.GetScore

// TBandwidthBP64ThreadTest ends...................................................

// TMemLatencyThread.........................................................
constructor TMemLatencyThread.Create;
begin
  FreeOnTerminate := TRUE;
  fArraySize := 4 * ONE_MILLION;
  SetLength(fB, fArraySize);
  SetLength(fA, fArraySize);
  RandomizeArray(fB);
  inherited Create(TRUE);  // creates suspended
  Priority := tpNormal;
end;

procedure TMemLatencyThread.Execute;
begin
  MemLatencyTest;
end; //procedure TMemLatencyThread.Execute;

procedure TMemLatencyThread.MemLatencyTest;
  var
    li_FromElement : integer;
    li_ToElement   : integer;
    i : integer;
  begin
    for i := 1 to 10 * fArraySize do
    begin
      li_FromElement := Random( fArraySize );
      li_ToElement := Random( fArraySize );
      fA[li_ToElement] := fB[li_FromElement];
    end;
end; // function TMemLatencyThread.MemLatencyTest
// TMemLatencyThread ends...............................................................

// TMemLatencyThreadTest begins.................................................
  constructor TMemLatencyThreadTest.Create;
  begin
    inherited;
    fTestName   := 'Memory latency threads test';
    fTestDescription := 'Measures main memory latency by making random 32-bit copies over two 16MB blocks.' +
      'The number of threads spawned automatically corresponds ' +
      'to the number of logical processors in the system.';
    fTestVersion:= '1.0';
    fTestType   := ttMemory;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 7.263 / 2;
    fQTestType := qtMemLatency;
    fPerformanceVectors := [ pvResponsiveness, pv3dGaming, pvArtificialIntelligence ];
  end; // constructor TMemLatencyThreadTest.Create;

  procedure TMemLatencyThreadTest.BeforeTest;
  var
    i : integer;
  begin
    inherited;
    // create a thread for each processor:
    for i := 1 to fMaxNbrOfThreads do begin
      fMemLatencyThread[ i ] := TMemLatencyThread.Create;
      fMemLatencyThread[ i ].OnTerminate := ThreadDone;
    end;
    if assigned( ffrmStatus ) then ffrmStatus.AddLine( 'Spawning ' +
                            IntToStr( fMaxNbrOfThreads ) +
                            ' thread(s)...' );
    application.ProcessMessages;
  end; // procedure TMemLatencyThreadTest.BeforeTest

  procedure TMemLatencyThreadTest.RunTest;
  var
    i : integer;
  begin
    for i := 1 to fMaxNbrOfThreads do begin
      fMemLatencyThread[ i ].Resume;
      inc( fNbrOfThreadsRunning );
    end; // for
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure TMemLatencyThreadTest.RunTest;

  procedure TMemLatencyThreadTest.ThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    application.ProcessMessages;
  end; // procedure TMemLatencyThreadTest.ThreadDone;

  function TMemLatencyThreadTest.GetScore(ai_index : integer) : integer;
  begin
    result := inherited GetScore( ai_index );
    result := fMaxNbrOfThreads * result;
  end; // function TMemLatencyThreadTest.GetScore
// TMemLatencyThreadTest ends...................................................

// TnBodyTest begins............................................................
  constructor TnBodyTest.Create;
  begin
    inherited;
    fTestName   := TEST_NAME;
    fTestDescription := TEST_DESCRIPTION;
    fTestVersion:= '1.0';
    fTestType   := ttSSE2;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 5.0628;
    fQTestType := qtNBody; // qtNBodyFPU, qtNBodySSE2, qtNBodySSE2Scalar, qtNBodySSE3 virtual types
    fPerformanceVectors := [ pv3dGaming, pvMathScienceEngineering ];
    if CpuidIsSse3Supported and (DetectCpuVendor <> eCpuVendorVIA) then begin
      fNBodyTestType := nbttSSE3;
      fTestDescription := TEST_DESCRIPTION + ' Defaulting to SSE3 codepath.';
    end else if CpuidIsSse2Supported then begin
      if (DetectCpuVendor = eCpuVendorVIA) then begin
        fNBodyTestType := nbttSSE2Scalar;
        fTestDescription := TEST_DESCRIPTION + ' Defaulting to SSE2 scalar codepath.';
      end else begin
        fNBodyTestType := nbttSSE2;
        fTestDescription := TEST_DESCRIPTION + ' Defaulting to SSE2 vector codepath.';
      end; //if
    end else begin
      fNBodyTestType := nbttFPU;
      fTestDescription := TEST_DESCRIPTION + ' Defaulting to FPU codepath.';
    end; // if
  end; // TnBodyTest.Create

  procedure TnBodyTest.SetNBodyTestType( Value : TNBodyTestType );
  begin
    fNBodyTestType := Value;
    case fNBodyTestType of
      nbttSSE3: begin
        fTestName := TEST_NAME + ' (forced SSE3)';
        fTestDescription := TEST_DESCRIPTION + ' Forcing SSE3 codepath.';
        fTestType := ttSSE3;
        fQTestType := qtNBodySSE3;
      end;
      nbttSSE2: begin
        fTestDescription := TEST_DESCRIPTION + ' Forcing SSE2 vector codepath.';
        fTestName := TEST_NAME + ' (forced SSE2 vector)';
        fTestType := ttSSE2;
        fQTestType := qtNBodySSE2;
      end;
      nbttSSE2Scalar : begin
        fTestDescription := TEST_DESCRIPTION + ' Forcing SSE2 scalar codepath.';
        fTestName := TEST_NAME + ' (forced SSE2 scalar)';
        fTestType := ttSSE2;
        fQTestType := qtNBodySSE2Scalar;
      end;
      nbttFPU: begin
        fTestDescription := TEST_DESCRIPTION + ' Forcing FPU codepath.';
        fTestName := TEST_NAME + ' (forced FPU)';
        fTestType := ttFPU;
        fQTestType := qtNBodyFPU;
      end;
    end; // case
  end; //   procedure TnBodyTest.SetTestType

  procedure TnBodyTest.RunTestSSE2;
  const
    NUM_BODIES    = 3;
    END_TIME      = 30;
    BLEND_FREQ    = 100000;
  var
    i     : integer;
    n     : integer;
    p     : array[0..NUM_BODIES] of TBody;
    fSinTheta, fCosTheta : double;
    t     : double;
    theta : double;
    dVx, dVy : double;
    G     : double;
    F     : double;
    x     : double;
    dtdt  : Txmm;
    dt    : double;
    // temp variables for SSE2:
    p_i_xy, p_i_VxVy, p_i_FxFy, p_i_mm, p_n_xy, p_n_Vxy, p_n_Fxy, p_n_mm : ^Txmm;
    PP: PColor32;
    timesteps : integer;
    j     : integer;
  begin
    PlotScale(-8, 11, -2, 12);
    G := 0.01;
    // intitial conditions
    //mass 0:
    p[0].m  := 2000;
    p[0].fill := p[0].m;
    p[0].x  := 0;
    p[0].y  := 0;
    p[0].Vx := 0;
    p[0].Vy := 0.15; //-0.084;
    //mass 1:
    p[1].m  := 0.02 * p[0].m;
    p[1].fill := p[1].m;
    p[1].x  := 2;
    p[1].y  := 0;
    p[1].Vx := 0;
    p[1].Vy := 0.9 * sqrt( G * p[0].m / 2 );
    //mass 2:
    p[2].m  := 0.002 * p[0].m; //0.00001 * p[0].m;//0.002 * p[0].m; //
    p[2].fill := p[2].m;
    p[2].x  := 2.1;
    p[2].y  := 0;
    p[2].Vx := 0;
    p[2].Vy := p[1].Vy + sqrt( G * p[1].m / 0.1);
    //mass 3:
    p[3].m  := 0.06 * p[0].m;
    p[3].fill := p[3].m;
    p[3].x  := -2.5;
    p[3].y  := 0;
    p[3].Vx := 0;
    p[3].Vy := sqrt( G * p[0].m / 2.5 );
    dt := 0.00001;
    t := 0;
    dtdt.x := dt; dtdt.y := dt;
    timesteps := round( END_TIME / dt );
    for j := 1 to timesteps do begin
      //t := t + dt;
      asm
        movsd xmm0, t
        addsd xmm0, dt
        movsd t, xmm0
      end;
      // zero out forces for all bodies:
      for n := 0 TO NUM_BODIES do begin
        p[n].Fx := 0;
        p[n].Fy := 0;
      end;
      for n := 0 TO NUM_BODIES do begin
        for i := n to NUM_BODIES do begin
          if n <> i then begin
            p_i_xy := @p[i].xy;
            p_n_xy := @p[n].xy;
            p_i_mm := @p[i].m;
            p_n_mm := @p[n].m;
            asm
              // calculate [x, y]:
              mov     eax,   p_i_xy
              movupd  xmm0,  Txmm [eax] // xmm0 := [p[i].x, p[i].y]
              mov     eax,   p_n_xy
              movupd  xmm1,  Txmm [eax] // xmm1 := [p[n].x, p[n].y]
              subpd   xmm0,  xmm1    // xmm0 := [x, y] := [p[i].x - p[n].x, p[i].y - p[n].y]
              // calculate y/x:
              movhlps xmm5,  xmm0    // xmm5 := [y, 0], xmm0 := [x, y]
              divsd   xmm5,  xmm0    // xmm5 := y/x
              movsd   theta, xmm5    // theta := y/x
              // store x for theta correction below:
              movsd   x,     xmm0
              // calculate r^ := x^2 + y^2:
              mulpd   xmm0,  xmm0    // xmm0 := [x^2, y^2]
              movhlps xmm1,  xmm0    // xmm1 := [y^2, 0], xmm0 := [x^2, y^2]
              addsd   xmm0,  xmm1    // xmm0 := x^2 + y^2 = r^2
              // calculate force component:
              mov     eax,   p_i_mm
              movsd   xmm1,  [eax]   // xmm1 := p[i].m
              mov     eax,   p_n_mm
              mulsd   xmm1,  [eax]   // xmm1 := p[i].m * p[n].m
              mulsd   xmm1,  G       // xmm1 := G * p[i].m * p[n].m
              divsd   xmm1,  xmm0    // xmm1 := G * p[i].m * p[n].m / r^2
              movsd   F,     xmm1    // F := G * p[i].m * p[n].m / r^2;
            end;

{
//            p_i_x := @p[i].x;
//            p_n_x := @p[n].x;
//            p_i_m := @p[i].m;
//            p_n_m := @p[n].m;
//            p_i_y := @p[i].y;
//            p_n_y := @p[n].y;
            asm
              // calculate x:
              mov   eax,  p_i_xy
              movsd xmm0, [eax]   // xmm0 := p[i].x
              movsd xmm2, [eax+8] // xmm2 := p[i].y
              mov   eax,  p_n_xy
              subsd xmm0, [eax]   // xmm0 := x := p[i].x - p[n].x
              // calculate y:
              subsd xmm2, [eax+8]  // xmm2 := y := p[i].y - p[n].y
              // store x in xmm4:
              movsd xmm4, xmm0    // xmm4 := x
              // store x for later use in arctan correction:
              movsd x, xmm0
              // square x:
              mulsd xmm0, xmm0    // xmm0: x^2
              // store y in xmm5:
              movsd xmm5, xmm2    // xmm5 := y
              // square y:
              mulsd xmm2, xmm2    // xmm2 := y^2
              // calculate r^2:
              addsd xmm0, xmm2    // xmm0 := x^2 + y^2
              // calculate y/x:
              divsd xmm5, xmm4    // xmm5 := y/x
              // store y/x in theta for arctan calculation:
              movsd theta, xmm5   // theta := y/x
              // calculate force component:
              mov   eax,  p_i_mm
              movsd xmm1, [eax]   // xmm1 := p[i].m
              mov   eax,  p_n_mm
              mulsd xmm1, [eax]   // xmm1 := p[i].m * p[n].m
              mulsd xmm1, G       // xmm1 := G * p[i].m * p[n].m
              divsd xmm1, xmm0    // xmm1 := G * p[i].m * p[n].m / r^2
              movsd F, xmm1       // F := G * p[i].m * p[n].m / r^2;
            end;
}
            theta := ArcTan( theta ); // theta := ArcTan( y/x )
            //y := p[i].y - p[n].y; see xmm5 above
            //x := p[i].x - p[n].x; see xmm4 above
            //theta := ArcTan(y / x); see above
            if x < 0 then theta := theta + PI;
            // the force on the body by another
            fCosTheta := F * cos(theta);
            fSinTheta := F * sin(theta);
            p[n].Fx := p[n].Fx + fCosTheta;
            p[n].Fy := p[n].Fy + fSinTheta;
            // the force that the body exerts on another
            p[i].Fx := p[i].Fx - fCosTheta;
            p[i].Fy := p[i].Fy - fSinTheta;
          end; // if n <> i
        end; // next i;
        //dVx := p[n].Fx * dt / p[n].m;
        //dVy := p[n].Fy * dt / p[n].m;
        p_n_Fxy  := @p[n].Fxy;
        p_n_Vxy  := @p[n].Vxy;
        p_n_xy   := @p[n].xy;
        p_n_mm   := @p[n].mm;
        asm
          movupd xmm7, dtdt      // xmm7 := dt, dt
          movapd xmm6, xmm7      // xmm6 := dt, dt
          mov    eax,  p_n_mm
          movupd xmm5, Txmm [eax]   // xmm5 := [p[n].m, p[n].m]
          divpd  xmm7, xmm5    // xmm7 := [ dt / p[n].m, dt / p[n].m]
          mov    eax,  p_n_Fxy
          movupd xmm0, Txmm [eax] // xmm0 := [p[n].Fx, p[n].Fy]
          mulpd  xmm0, xmm7    // xmm0 := [p[n].Fx * dt / p[n].m, p[n].Fy * dt / p[n].m] := [dVx, dVy]
          mov    eax,  p_n_Vxy
          movupd xmm1, Txmm [eax] // xmm1 := [p[n].Vx, p[n].Vy]
          addpd  xmm0, xmm1    // xmm0 := [p[n].Vx + dVx, p[n].Vy + dVy]
          movupd Txmm [eax], xmm0 // store p[n].Vx, p[n].Vy
          mulpd  xmm0, xmm6    // xmm0 := [p[n].Vx * dt, p[n].Vy * dt]
          mov    eax,  p_n_xy
          movupd xmm1, Txmm [eax] // xmm1 := [p[n].x, p[n].y]
          addpd  xmm0, xmm1    // xmm0 := [p[n].x + p[n].Vx * dt, p[n].y + p[n].Vy * dt]
          movupd Txmm [eax], xmm0 // store [p[n].x, store p[n].y
        end;

        if j mod 100 = 0 then begin
          case n of
            0:  begin
                  PlotAndBuffer(p[n].x, p[n].y, clRed, clRed32);
                end;
            1:  begin
                  PlotAndBuffer(p[n].x, p[n].y, clAqua, clAqua32);
                end;
            2:  begin
                  PlotAndBuffer(p[n].x, p[n].y, clYellow, clYellow32);
                end;
            3:  begin
                  PlotAndBuffer(p[n].x, p[n].y, clLime, clLime32);
                end;
            4:  begin
                  PlotAndBuffer(p[n].x, p[n].y, clWhite, clWhite32);
                end;
          end; // case n
          // fade image at the last body after BLEND_FREQ number of steps
          if (j mod BLEND_FREQ = 0) and (n = NUM_BODIES) then begin
            with fPaintBox32.Buffer do begin
              PP := @Bits[0];
              for I := 0 to Width * Height do
              begin
                BlendMem($10000000, PP^);
                Inc(PP);
              end; // for I
              EMMS;
              fPaintBox32.Invalidate;
              Application.ProcessMessages;
            end; // with fPaintBox32.Buffer
          end; // if (j mod 100000 = 0) and (n = NUM_BODIES)
        end; // if j mod 100 = 0 then
      end; // NEXT n
    end; // NEXT j
  end; // procedure TnBodyTest.RunTestSSE2;

  procedure TnBodyTest.RunTestFPU;
  const
    G             = 0.01;
    NUM_BODIES    = 3;
    END_TIME      = 30;
    BLEND_FREQ    = 100000;
  var
    p     : array[0..NUM_BODIES] of TBody;
    i     : integer;
    j     : integer;
    timesteps : integer;
    n     : integer;
    dt    : double;
    t     : double;
    theta : double;
    F     : double;
    x,y,r2 : double;
    dVx, dVy : double;
    PP: PColor32;
    FCosTheta, FSinTheta : double;
  begin
    // intitial conditions
    //mass 0:
    p[0].m  := 2000;
    p[0].x  := 0;
    p[0].y  := 0;
    p[0].Vx := 0;
    p[0].Vy := 0.15; //-0.084;
    //mass 1:
    p[1].m  := 0.02 * p[0].m;
    p[1].x  := 2;
    p[1].y  := 0;
    p[1].Vx := 0;
    p[1].Vy := 0.9 * sqrt( G * p[0].m / 2 );
    //mass 2:
    p[2].m  := 0.002 * p[0].m; //0.00001 * p[0].m;//0.002 * p[0].m; //
    p[2].x  := 2.1;
    p[2].y  := 0;
    p[2].Vx := 0;
    p[2].Vy := p[1].Vy + sqrt( G * p[1].m / 0.1);
    //mass 3:
    p[3].m  := 0.06 * p[0].m;
    p[3].x  := -2.5;
    p[3].y  := 0;
    p[3].Vx := 0;
    p[3].Vy := sqrt( G * p[0].m / 2.5 );
    PlotScale(-8, 11, -2, 12);
    dt := 0.00001;
    t := 0;
    timesteps := round( END_TIME / dt );
    for j := 1 to timesteps do begin
      t := t + dt;
      // zero out forces for all bodies:
      for n := 0 TO NUM_BODIES do begin
        p[n].Fx := 0;
        p[n].Fy := 0;
      end;
      for n := 0 TO NUM_BODIES do begin
        for i := n to NUM_BODIES do begin
          if n <> i then begin
            y := p[i].y - p[n].y;
            x := p[i].x - p[n].x;
            r2 := x * x + y * y; //r := sqrt( x * x + y * y );
            F := G * p[i].m * p[n].m / r2; //(r * r);
            theta := ArcTan(y / x);
            if x < 0 then theta := theta + PI;
            FCosTheta := F * cos(theta);
            FSinTheta := F * sin(theta);
            // the force on the body by another
            p[n].Fx := p[n].Fx + FCosTheta;
            p[n].Fy := p[n].Fy + FSinTheta;
            // the force that the body exerts on another
            p[i].Fx := p[i].Fx - FCosTheta;
            p[i].Fy := p[i].Fy - FSinTheta;
          end; // if n <> i
        end; // next i;
        dVx := p[n].Fx * dt / p[n].m;
        dVy := p[n].Fy * dt / p[n].m;
        p[n].Vx := p[n].Vx + dVx;
        p[n].Vy := p[n].Vy + dVy;
        p[n].x := p[n].x + p[n].Vx * dt;
        p[n].y := p[n].y + p[n].Vy * dt;
        if j mod 100 = 0 then begin
          case n of
            0:  begin
                  PlotAndBuffer(p[n].x, p[n].y, clRed, clRed32);
                end;
            1:  begin
                  PlotAndBuffer(p[n].x, p[n].y, clAqua, clAqua32);
                end;
            2:  begin
                  PlotAndBuffer(p[n].x, p[n].y, clYellow, clYellow32);
                end;
            3:  begin
                  PlotAndBuffer(p[n].x, p[n].y, clLime, clLime32);
                end;
            4:  begin
                  PlotAndBuffer(p[n].x, p[n].y, clWhite, clWhite32);
                end;
          end; // case n
          // fade image at the last body after BLEND_FREQ number of steps
          if (j mod BLEND_FREQ = 0) and (n = NUM_BODIES) then begin
            with fPaintBox32.Buffer do begin
              PP := @Bits[0];
              for I := 0 to Width * Height do
              begin
                BlendMem($10000000, PP^);
                Inc(PP);
              end; // for I
              EMMS;
              fPaintBox32.Invalidate;
              Application.ProcessMessages;
            end; // with fPaintBox32.Buffer
          end; // if (j mod 100000 = 0) and (n = NUM_BODIES)
        end; // if j mod 100 = 0 then
      end; // NEXT n
    end; // NEXT j
  end; // procedure TnBodyTest.RunTestFPU;

  procedure TnBodyTest.RunTestSSE3;
  const
    NUM_BODIES    = 3;
    END_TIME      = 30;
    BLEND_FREQ    = 100000;
  var
    p     : array[0..NUM_BODIES] of TBody;
    dtdt  : Txmm;
    i     : integer;
    j     : integer;
    timesteps : integer;
    n     : integer;
    dt    : double;
    t     : double;
    theta : double;
    F     : double;
    x,y,r : double;
    dVx, dVy : double;
    PP: PColor32;
    // temp variables for SSE2:
    p_i_xy, p_i_VxVy, p_i_FxFy, p_i_mm : ^Txmm;
    p_n_xy, p_n_Vxy, p_n_Fxy, p_n_mm : ^Txmm;
    fCosTheta : double;
    fSinTheta : double;
    G : double;
  begin
    G := 0.01;
    // intitial conditions
    //mass 0:
    p[0].m  := 2000;
    p[0].fill := p[0].m;
    p[0].x  := 0;
    p[0].y  := 0;
    p[0].Vx := 0;
    p[0].Vy := 0.15; //-0.084;
    //mass 1:
    p[1].m  := 0.02 * p[0].m;
    p[1].fill := p[1].m;
    p[1].x  := 2;
    p[1].y  := 0;
    p[1].Vx := 0;
    p[1].Vy := 0.9 * sqrt( G * p[0].m / 2 );
    //mass 2:
    p[2].m  := 0.002 * p[0].m; //0.00001 * p[0].m;//0.002 * p[0].m; //
    p[2].fill := p[2].m;
    p[2].x  := 2.1;
    p[2].y  := 0;
    p[2].Vx := 0;
    p[2].Vy := p[1].Vy + sqrt( G * p[1].m / 0.1);
    //mass 3:
    p[3].m  := 0.06 * p[0].m;
    p[3].fill := p[3].m;
    p[3].x  := -2.5;
    p[3].y  := 0;
    p[3].Vx := 0;
    p[3].Vy := sqrt( G * p[0].m / 2.5 );
    PlotScale(-8, 11, -2, 12);
    dt := 0.00001;
    dtdt.x := dt;  dtdt.y := dt;
    t := 0;
    timesteps := round( END_TIME / dt );
    for j := 1 to timesteps do begin
      //t := t + dt;
      asm
        movsd xmm0, t
        addsd xmm0, dt
        movsd t, xmm0
      end;
      // zero out forces for all bodies:
      for n := 0 TO NUM_BODIES do begin
        p[n].Fx := 0;
        p[n].Fy := 0;
      end;
      for n := 0 TO NUM_BODIES do begin
        for i := n to NUM_BODIES do begin
          if n <> i then begin
            p_i_xy := @p[i].xy;
            p_n_xy := @p[n].xy;
            p_i_mm := @p[i].mm;
            p_n_mm := @p[n].mm;
            asm
              // calculate [x, y]:
              mov     eax,    p_i_xy
              movupd  xmm0,   Txmm [eax]  // xmm0 := [p[i].x, p[i].y]
              mov     eax,    p_n_xy
              movupd  xmm1,   Txmm [eax]  // xmm1 := p[n].x
              subpd   xmm0,   xmm1        // xmm0 := [x, y] := [p[i].x - p[n].x, p[i].y - p[n].y]
              // calculate y/x:
              movhlps xmm5,   xmm0        // xmm5 := [y, 0], xmm4 := [x, y]
              divsd   xmm5,   xmm0        // xmm5 := y/x
              movsd   theta,  xmm5        // theta := y/x
              // store x for theta correction below:
              movsd   x,      xmm0
              // calculate r^ := x^2 + y^2:
              mulpd   xmm0,   xmm0        // xmm0 := [x^2, y^2]
              //SSE3:
              haddpd  xmm0,   xmm0          // xmm0 := [x^2 + y^2, x^2 + y^2] := [r^2, r^2]
              // calculate force component:
              mov     eax,    p_i_mm
              movsd   xmm1,   [eax]       // xmm1 := p[i].m
              mov     eax,    p_n_mm
              mulsd   xmm1,   [eax]       // xmm1 := p[i].m * p[n].m
              mulsd   xmm1,   G           // xmm1 := G * p[i].m * p[n].m
              divsd   xmm1,   xmm0        // xmm1 := G * p[i].m * p[n].m / r^2
              movsd   F,      xmm1        // F := G * p[i].m * p[n].m / r^2;
            end;
            theta := ArcTan( theta ); // theta := ArcTan( y/x )
            //y := p[i].y - p[n].y; see xmm5 above
            //x := p[i].x - p[n].x; see xmm4 above
            //theta := ArcTan(y / x); see above
            if x < 0 then theta := theta + PI;
            // the force on the body by another
            fCosTheta := F * cos(theta);
            fSinTheta := F * sin(theta);
            p[n].Fx := p[n].Fx + fCosTheta;
            p[n].Fy := p[n].Fy + fSinTheta;
            // the force that the body exerts on another
            p[i].Fx := p[i].Fx - fCosTheta;
            p[i].Fy := p[i].Fy - fSinTheta;
          end; // if n <> i
        end; // next i;
        //dVx := p[n].Fx * dt / p[n].m;
        //dVy := p[n].Fy * dt / p[n].m;
        p_n_Fxy := @p[n].Fx;
        p_n_Vxy := @p[n].Vx;
        p_n_xy := @p[n].x;
        p_n_mm := @p[n].m;
        asm
          movupd xmm7, dtdt    // xmm7 := dt, dt
          movapd xmm6, xmm7    // xmm6 := dt, dt
          mov    eax,  p_n_mm
          movupd xmm5, [eax]   // xmm5 := [ p[n].m, p[n].m]
          divpd  xmm7, xmm5    // xmm7 := [ dt / p[n].m, dt / p[n].m ]
          mov    eax,  p_n_Fxy
          movupd xmm0, Txmm [eax] // xmm0 := [p[n].Fx, p[n].Fy]
          mulpd  xmm0, xmm7    // xmm0 := [p[n].Fx * dt / p[n].m, p[n].Fy * dt / p[n].m] = [dVx, dVy]
          mov    eax,  p_n_Vxy
          movupd xmm1, Txmm [eax] // xmm1 := [p[n].Vx, p[n].Vy]
          addpd  xmm0, xmm1    // xmm0 := [p[n].Vx + dVx, p[n].Vy + dVy]
          movupd Txmm [eax], xmm0 // store p[n].Vx, p[n].Vy
          mulpd  xmm0, xmm6    // xmm0 := [p[n].Vx * dt, p[n].Vy * dt]
          mov    eax,  p_n_xy
          movupd xmm1, Txmm [eax] // xmm1 := [p[n].x, p[n].y]
          addpd  xmm0, xmm1    // xmm0L := [p[n].x + p[n].Vx * dt, p[n].y + p[n].Vy * dt]
          movupd Txmm [eax], xmm0 // store p[n].x, p[n].y
        end;

        if j mod 100 = 0 then begin
          case n of
            0:  begin
                  PlotAndBuffer(p[n].x, p[n].y, clRed, clRed32);
                end;
            1:  begin
                  PlotAndBuffer(p[n].x, p[n].y, clAqua, clAqua32);
                end;
            2:  begin
                  PlotAndBuffer(p[n].x, p[n].y, clYellow, clYellow32);
                end;
            3:  begin
                  PlotAndBuffer(p[n].x, p[n].y, clLime, clLime32);
                end;
            4:  begin
                  PlotAndBuffer(p[n].x, p[n].y, clWhite, clWhite32);
                end;
          end; // case n
          // fade image at the last body after BLEND_FREQ number of steps
          if (j mod BLEND_FREQ = 0) and (n = NUM_BODIES) then begin
            with fPaintBox32.Buffer do begin
              PP := @Bits[0];
              for I := 0 to Width * Height do
              begin
                BlendMem($10000000, PP^);
                Inc(PP);
              end; // for I
              EMMS;
              fPaintBox32.Invalidate;
              Application.ProcessMessages;
            end; // with fPaintBox32.Buffer
          end; // if (j mod 100000 = 0) and (n = NUM_BODIES)
        end; // if j mod 100 = 0 then
      end; // NEXT n
    end; // NEXT j
  end; // procedure TnBodyTest.RunTestSSE3;

  procedure TnBodyTest.RunTestSse2Scalar;
  const
    NUM_BODIES    = 3;
    END_TIME      = 30;
    BLEND_FREQ    = 100000;
  var
    p     : array[0..NUM_BODIES] of TBody;
    i     : integer;
    j     : integer;
    timesteps : integer;
    n     : integer;
    dt    : double;
    t     : double;
    theta : double;
    F     : double;
    x,y,r : double;
    dVx, dVy : double;
    PP: PColor32;
    // temp variables for SSE2:
    p_i_m, p_i_x, p_i_y, p_i_Vx, p_i_Vy, p_i_Fx, p_i_Fy : ^double;
    p_n_m, p_n_x, p_n_y, p_n_Vx, p_n_Vy, p_n_Fx, p_n_Fy : ^double;
    fCosTheta : double;
    fSinTheta : double;
    G : double;
  begin
    G := 0.01;
    // intitial conditions
    //mass 0:
    p[0].m  := 2000;
    p[0].x  := 0;
    p[0].y  := 0;
    p[0].Vx := 0;
    p[0].Vy := 0.15; //-0.084;
    //mass 1:
    p[1].m  := 0.02 * p[0].m;
    p[1].x  := 2;
    p[1].y  := 0;
    p[1].Vx := 0;
    p[1].Vy := 0.9 * sqrt( G * p[0].m / 2 );
    //mass 2:
    p[2].m  := 0.002 * p[0].m; //0.00001 * p[0].m;//0.002 * p[0].m; //
    p[2].x  := 2.1;
    p[2].y  := 0;
    p[2].Vx := 0;
    p[2].Vy := p[1].Vy + sqrt( G * p[1].m / 0.1);
    //mass 3:
    p[3].m  := 0.06 * p[0].m;
    p[3].x  := -2.5;
    p[3].y  := 0;
    p[3].Vx := 0;
    p[3].Vy := sqrt( G * p[0].m / 2.5 );
    PlotScale(-8, 11, -2, 12);
    dt := 0.00001;
    t := 0;
    timesteps := round( END_TIME / dt );
    for j := 1 to timesteps do begin
      //t := t + dt;
      asm
        movsd xmm0, t
        addsd xmm0, dt
        movsd t, xmm0
      end;
      // zero out forces for all bodies:
      for n := 0 TO NUM_BODIES do begin
        p[n].Fx := 0;
        p[n].Fy := 0;
      end;
      for n := 0 TO NUM_BODIES do begin
        for i := n to NUM_BODIES do begin
          if n <> i then begin
            p_i_x := @p[i].x;
            p_n_x := @p[n].x;
            p_i_m := @p[i].m;
            p_n_m := @p[n].m;
            p_i_y := @p[i].y;
            p_n_y := @p[n].y;
            asm
              // calculate x:
              mov   eax,  p_i_x
              movsd xmm0, [eax]   // xmm0 := p[i].x
              mov   eax,  p_n_x
              subsd xmm0, [eax]   // xmm0 := x := p[i].x - p[n].x
              // store x in xmm4:
              movsd xmm4, xmm0    // xmm4 := x
              // store x for later use in arctan correction:
              movsd x, xmm0
              // square x:
              mulsd xmm0, xmm0    // xmm0: x^2
              // calculate y:
              mov   eax,  p_i_y
              movsd xmm2, [eax]   // xmm2 := p[i].y
              mov   eax,  p_n_y
              subsd xmm2, [eax]   // xmm2 := y := p[i].y - p[n].y
              // store y in xmm5:
              movsd xmm5, xmm2    // xmm5 := y
              // square y:
              mulsd xmm2, xmm2    // xmm2 := y^2
              // calculate r^2:
              addsd xmm0, xmm2    // xmm0 := x^2 + y^2
              // calculate y/x:
              divsd xmm5, xmm4    // xmm5 := y/x
              // store y/x in theta for arctan calculation:
              movsd theta, xmm5   // theta := y/x
              // calculate force component:
              mov   eax,  p_i_m
              movsd xmm1, [eax]   // xmm1 := p[i].m
              mov   eax,  p_n_m
              mulsd xmm1, [eax]   // xmm1 := p[i].m * p[n].m
              mulsd xmm1, G       // xmm1 := G * p[i].m * p[n].m
              divsd xmm1, xmm0    // xmm1 := G * p[i].m * p[n].m / r^2
              movsd F, xmm1       // F := G * p[i].m * p[n].m / r^2;
            end;
            theta := ArcTan( theta ); // theta := ArcTan( y/x )
            //y := p[i].y - p[n].y; see xmm5 above
            //x := p[i].x - p[n].x; see xmm4 above
            //theta := ArcTan(y / x); see above
            if x < 0 then theta := theta + PI;
            // the force on the body by another
            fCosTheta := F * cos(theta);
            fSinTheta := F * sin(theta);
            p[n].Fx := p[n].Fx + fCosTheta;
            p[n].Fy := p[n].Fy + fSinTheta;
            // the force that the body exerts on another
            p[i].Fx := p[i].Fx - fCosTheta;
            p[i].Fy := p[i].Fy - fSinTheta;
          end; // if n <> i
        end; // next i;
        //dVx := p[n].Fx * dt / p[n].m;
        //dVy := p[n].Fy * dt / p[n].m;
        p_n_Fx  := @p[n].Fx;
        p_n_m   := @p[n].m;
        p_n_Fy  := @p[n].Fy;
        p_n_Vx  := @p[n].Vx;
        p_n_Vy  := @p[n].Vy;
        p_n_x   := @p[n].x;
        p_n_y   := @p[n].y;
        asm
          movsd xmm7,   dt      // xmm7 := dt
          movsd xmm6,   xmm7    // xmm6 := dt
          mov   eax,    p_n_m
          divsd xmm6,   [eax]   // xmm6 := dt / p[n].m
          mov   eax,    p_n_Fx
          movsd xmm0,   [eax]   // xmm0 := p[n].Fx
          mulsd xmm0,   xmm6    // xmm0 := dVx := p[n].Fx * dt / p[n].m
          mov   eax,    p_n_Vx
          addsd xmm0,   [eax]   // xmm0 := p[n].Vx + dVx := new p[n].Vx
          movsd [eax],  xmm0    // store p[n].Vx
          // p[n].x := p[n].x + p[n].Vx * dt;
          mulsd xmm0,   xmm7    // xmm0 := p[n].Vx * dt
          mov   eax,    p_n_x
          addsd xmm0,   [eax]   // xmm0 := p[n].x + p[n].Vx * dt := new p[n].x
          movsd [eax],  xmm0    // store p[n].x
          // now calculate the y components:
          mov   eax,    p_n_Fy
          movsd xmm1,   [eax]   // xmm1 := p[n].Fy
          mulsd xmm1,   xmm6    // xmm1 := p[n].Fy * dt / p[n].m
          // p[n].Vy := p[n].Vy + dVy;
          mov   eax,    p_n_Vy
          addsd xmm1,   [eax]   // xmm1 := p[n].Vy := p[n].Vy + dVy
          movsd [eax],  xmm1    // store p[n].Vy
          // p[n].y := p[n].y + p[n].Vy * dt;
          mulsd xmm1,   xmm7    // xmm1 := p[n].Vy * dt
          mov   eax,    p_n_y
          addsd xmm1,   [eax]   // xmm1 := p[n].y + p[n].Vy * dt := new p[n].y
          movsd [eax],  xmm1    // store p[n].y
        end;
        if j mod 100 = 0 then begin
          case n of
            0:  begin
                  PlotAndBuffer(p[n].x, p[n].y, clRed, clRed32);
                end;
            1:  begin
                  PlotAndBuffer(p[n].x, p[n].y, clAqua, clAqua32);
                end;
            2:  begin
                  PlotAndBuffer(p[n].x, p[n].y, clYellow, clYellow32);
                end;
            3:  begin
                  PlotAndBuffer(p[n].x, p[n].y, clLime, clLime32);
                end;
            4:  begin
                  PlotAndBuffer(p[n].x, p[n].y, clWhite, clWhite32);
                end;
          end; // case n
          // fade image at the last body after BLEND_FREQ number of steps
          if (j mod BLEND_FREQ = 0) and (n = NUM_BODIES) then begin
            with fPaintBox32.Buffer do begin
              PP := @Bits[0];
              for I := 0 to Width * Height do
              begin
                BlendMem($10000000, PP^);
                Inc(PP);
              end; // for I
              EMMS;
              fPaintBox32.Invalidate;
              Application.ProcessMessages;
            end; // with fPaintBox32.Buffer
          end; // if (j mod 100000 = 0) and (n = NUM_BODIES)
        end; // if j mod 100 = 0 then
      end; // NEXT n
    end; // NEXT j
  end; // procedure TnBodyTest.RunTestSse2Scalar;

  procedure TnBodyTest.RunTest;
  begin
    case fNBodyTestType of
      nbttSSE3: RunTestSSE3;
      nbttSSE2: RunTestSSE2;
      nbttSSE2Scalar : RunTestSSE2Scalar;
      nbttFPU:  RunTestFPU;
    end; // case
  end; //procedure TnBodyTest.RunTest;

// TnBodyTest ends...............................................................

// TSse3ThreadTest begins.................................................
  constructor TSse3ThreadTest.Create;
  begin
    inherited;
    fTestName   := 'SSE3 threads test';
    fTestDescription := 'Each thread runs a variety of SSE3 tests. Normalized against AMD Turion 64 ML-34 (1800MHz).';
    fTestVersion:= '1.0';
    fTestType   := ttSSE3;
    fTestAuthor := 'Vijay Hariharan, Van Smith';
    fReferenceTime := 3.28447 / 2;
    fQTestType := qtSse3;
    fPerformanceVectors := [ pv3dGaming, pvDataSetTransform,
                             pvMathScienceEngineering ];
  end; // constructor TSse3ThreadTest.Create;

  procedure TSse3ThreadTest.BeforeTest;
  var
    i : integer;
  begin
    inherited;
    // create a thread for each processor:
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunchSse3Thread[ i ] := TLaunchProgramThread.Create;
      fLaunchSse3Thread[ i ].OnTerminate := ThreadDone;
      fLaunchSse3Thread[ i ].ProgramCommand := 'osmfiles\SSE3Test.exe';
    end;
    if assigned( ffrmStatus ) then ffrmStatus.AddLine( 'Spawning ' +
                            IntToStr( fMaxNbrOfThreads ) +
                            ' thread(s)...' );
    application.ProcessMessages;
  end; // procedure TSse3ThreadTest.BeforeTest

  procedure TSse3ThreadTest.RunTest;
  var
    i : integer;
  begin
    if CpuidIsSse3Supported then begin
      for i := 1 to fMaxNbrOfThreads do begin
        fLaunchSse3Thread[ i ].Resume;
        inc( fNbrOfThreadsRunning );
      end; // for
      while fNbrOfThreadsRunning > 0 do begin
        sleep( 10 );
        application.ProcessMessages;
      end;
    end else begin
      fInvalidRun := TRUE;
    end; // if
  end; // procedure TSse3ThreadTest.RunTest;

  procedure TSse3ThreadTest.ThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    application.ProcessMessages;
    if TLaunchProgramThread( Sender ).ReturnCode <> 0 then begin
      fInvalidRun := TRUE;
    end; // if
  end; // procedure TSse3ThreadTest.ThreadDone;

  function TSse3ThreadTest.GetScore(ai_index : integer) : integer;
  begin
    if fInvalidRun then begin
      result := 0;
    end else begin
      result := inherited GetScore( ai_index );
      result := fMaxNbrOfThreads * result;
    end; // if
  end; // function TSse3ThreadTest.GetScore
// TSse3ThreadTest ends...................................................

// TMontgomeryMultiplierTest begins.................................................
  constructor TMontgomeryMultiplierTest.Create;
  begin
    inherited;
    fTestName   := 'Montgomery Multipler test';
    fTestDescription := 'This threaded test stresses the Montgomery Multiplier algorithm which is used in RSA encryption.';
    fTestVersion:= '1.0';
    fTestType   := ttCryptography;
    fTestAuthor := 'Van Smith, Vijay Hariharan';
    fReferenceTime := 21.5096 / 2;
    fQTestType := qtMontMul;
    fPerformanceVectors := [ pvSecurity, pvMathScienceEngineering ];
  end; // constructor TMontgomeryMultiplierTest.Create;

  procedure TMontgomeryMultiplierTest.BeforeTest;
  var
    i : integer;
  begin
    inherited;
    // create a thread for each processor:
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunchMMThread[ i ] := TLaunchProgramThread.Create;
      fLaunchMMThread[ i ].OnTerminate := ThreadDone;
      if CpuidIsCentaurMontMulSupported then begin
        fLaunchMMThread[ i ].ProgramCommand := 'osmfiles\MMTest.exe -hardware';
      end else begin
        fLaunchMMThread[ i ].ProgramCommand := 'osmfiles\MMTest.exe -software';
      end; // if
    end;
    if assigned( ffrmStatus ) then ffrmStatus.AddLine( 'Spawning ' +
                            IntToStr( fMaxNbrOfThreads ) +
                            ' thread(s)...' );
    application.ProcessMessages;
  end; // procedure TMontgomeryMultiplierTest.BeforeTest

  procedure TMontgomeryMultiplierTest.RunTest;
  var
    i : integer;
  begin
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunchMMThread[ i ].Resume;
      inc( fNbrOfThreadsRunning );
    end; // for
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure TMontgomeryMultiplierTest.RunTest;

  procedure TMontgomeryMultiplierTest.ThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    application.ProcessMessages;
    if TLaunchProgramThread( Sender ).ReturnCode <> 0 then begin
      fInvalidRun := TRUE;
    end; // if
  end; // procedure TMontgomeryMultiplierTest.ThreadDone;

  function TMontgomeryMultiplierTest.GetScore(ai_index : integer) : integer;
  begin
    if fInvalidRun then begin
      result := 0;
    end else begin
      result := inherited GetScore( ai_index );
      result := fMaxNbrOfThreads * result;
    end; // if
  end; // function TMontgomeryMultiplierTest.GetScore

// TMontgomeryMultiplierTest ends...................................................

// TSha1Test begins.................................................
  constructor TSha1Test.Create;
  begin
    inherited;
    fTestName   := 'SHA-1 cryptographic hashing test';
    fTestDescription := 'The SHA-1 test is threaded. ' +
     'The SHA (Secure Hash Algorithm) family is a set of related cryptographic ' +
     'hash functions. The most commonly used function in the family, SHA-1 ' +
     'is employed in a large variety of popular security applications and ' +
     'protocols, including TLS, SSL, PGP, SSH, S/MIME, and IPSec.';
    fTestVersion:= '1.0';
    fTestType   := ttCryptography;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 12.7 / 2;
    fQTestType := qtSha1;
    fPerformanceVectors := [ pvSecurity ];
  end; // constructor TSha1Test.Create;

  procedure TSha1Test.BeforeTest;
  var
    i : integer;
  begin
    inherited;
    // create a thread for each processor:
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunchSha1Thread[ i ] := TLaunchProgramThread.Create;
      fLaunchSha1Thread[ i ].OnTerminate := ThreadDone;
//      if CpuidIsCentaurHashEngineSupported then begin
        fLaunchSha1Thread[ i ].ProgramCommand := 'osmfiles\miniBench.exe Sha1';
//      end else begin
//        fLaunchSha1Thread[ i ].ProgramCommand := 'osmfiles\Sha1.exe -software';
//      end; // if
    end;
    if assigned( ffrmStatus ) then ffrmStatus.AddLine( 'Spawning ' +
                            IntToStr( fMaxNbrOfThreads ) +
                            ' thread(s)...' );
    application.ProcessMessages;
  end; // procedure TSha1Test.BeforeTest

  procedure TSha1Test.RunTest;
  var
    i : integer;
  begin
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunchSha1Thread[ i ].Resume;
      inc( fNbrOfThreadsRunning );
    end; // for
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure TSha1Test.RunTest;

  procedure TSha1Test.ThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    application.ProcessMessages;
    if TLaunchProgramThread( Sender ).ReturnCode <> 0 then begin
      fInvalidRun := TRUE;
    end; // if
  end; // procedure TSha1Test.ThreadDone;

  function TSha1Test.GetScore(ai_index : integer) : integer;
  begin
    if fInvalidRun then begin
      result := 0;
    end else begin
      result := inherited GetScore( ai_index );
      result := fMaxNbrOfThreads * result;
    end; // if
  end; // function TSha1Test.GetScore

// TSha1Test ends...................................................

// TSha256Test begins.................................................
  constructor TSha256Test.Create;
  begin
    inherited;
    fTestName   := 'SHA-256 cryptographic hashing test';
    fTestDescription := 'SHA-256 is a member of the Secure Hash Algorithm family with a 256-bit long digest. This test is threaded';
    fTestVersion:= '1.0';
    fTestType   := ttCryptography;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 18.1 / 2;
    fQTestType := qtSha256;
    fPerformanceVectors := [ pvSecurity ];
  end; // constructor TSha256Test.Create;

  procedure TSha256Test.BeforeTest;
  var
    i : integer;
  begin
    inherited;
    // create a thread for each processor:
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunchSha256Thread[ i ] := TLaunchProgramThread.Create;
      fLaunchSha256Thread[ i ].OnTerminate := ThreadDone;
//      if CpuidIsCentaurHashEngineSupported then begin
        fLaunchSha256Thread[ i ].ProgramCommand := 'osmfiles\miniBench.exe Sha256';
//      end else begin
//        fLaunchSha256Thread[ i ].ProgramCommand := 'osmfiles\Sha256.exe -software';
//      end; // if
    end;
    if assigned( ffrmStatus ) then ffrmStatus.AddLine( 'Spawning ' +
                            IntToStr( fMaxNbrOfThreads ) +
                            ' thread(s)...' );
    application.ProcessMessages;
  end; // procedure TSha256Test.BeforeTest

  procedure TSha256Test.RunTest;
  var
    i : integer;
  begin
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunchSha256Thread[ i ].Resume;
      inc( fNbrOfThreadsRunning );
    end; // for
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure TSha256Test.RunTest;

  procedure TSha256Test.ThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    application.ProcessMessages;
    if TLaunchProgramThread( Sender ).ReturnCode <> 0 then begin
      fInvalidRun := TRUE;
    end; // if
  end; // procedure TSha256Test.ThreadDone;

  function TSha256Test.GetScore(ai_index : integer) : integer;
  begin
    if fInvalidRun then begin
      result := 0;
    end else begin
      result := inherited GetScore( ai_index );
      result := fMaxNbrOfThreads * result;
    end; // if
  end; // function TSha256Test.GetScore
// TSha256Test ends...................................................

// TAlphaBlendTest begins.......................................................
  constructor TAlphaBlendTest.Create;
  begin
    inherited;
    fTestName   := 'AlphaBlend';
    fTestDescription := 'Tests alpha blending performance by making the output form fade and unfade. Alpha blending performance is important in Windows Vista.';
    fTestVersion:= '1.0';
    fTestType   := ttCPU;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 3.15;
    fQTestType := qtAlphaBlend;
    fPerformanceVectors := [ pv3dGaming ];
  end;

  procedure TAlphaBlendTest.RunTest;
  var
    i : integer;
  begin
    for i := 1 to 1 do begin
      FadeDown( fOutputForm, 0, 1 );
      UnFade( fOutputForm, 255, 1 );
    end; // for
  end; // procedure TAlphaBlendTest.RunTest;
// TAlphaBlendTest ends.........................................................

// TAlphaDotsTest begins.......................................................
  constructor TAlphaDotsTest.Create;
  begin
    inherited;
    fTestName   := 'Alpha Dots';
    fTestDescription := 'Plots dots randomly over canvas on a form alpha blended at 50%. Alpha blending performance is important in Windows Vista.';
    fTestVersion:= '1.0';
    fTestType   := ttCpu;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 7.43;
    fQTestType := qtAlphaDots;
    fPerformanceVectors := [ pv3dGaming ];
  end;

  procedure TAlphaDotsTest.RunTest;
  var
    x : integer;
    y : integer;
    z : integer;
  begin
    FadeFast50( fOutputForm );
    with fPaintBox32 do begin
      // Random dots:
      RandSeed := 1;
      for z := 1 to 10000000 do
      begin
        x := Random(Width);
        y := Random(Height);
        case Random(6) of
          0: Canvas.Pixels[x, y] := clBlack;
          1: Canvas.Pixels[x, y] := clGreen;
          2: Canvas.Pixels[x, y] := clBlue;
          3: Canvas.Pixels[x, y] := clRed;
          4: Canvas.Pixels[x, y] := clWhite;
          5: Canvas.Pixels[x, y] := clYellow;
        end;
      end;
    end; // with
  end; // procedure TAlphaDotsTest.RunTest;
// TAlphaDotsTest ends.........................................................

// TPngOutThreadTest.....................................................
// Class name: TPngOutThreadTest
// Author: Van Smith
// Date: May 17, 2005
  constructor TPngOutThreadTest.Create;
  begin
    inherited;
    fTestName   := 'PngOut Threads Test';
    fTestDescription := 'The PngOut test launches an instance of PNGOut for each logical processor in the system. ' +
      'Each thread converts the same JPG file into PNG format.';
    fTestVersion:= '1.0';
    fTestType   := ttSystem;
    fTestAuthor := 'Van Smith, Kalumba';
    fReferenceTime :=  4.44389 / 2;
    fShowStatus := FALSE;
    fQTestType := qtPngOut;
    fTargetDrive := 'c';
    fPerformanceVectors := [ pvDataSetTransform ];
  end; // constructor TPngOutThreadTest.Create;

  procedure TPngOutThreadTest.DeleteOutputFiles;
  var
    i : integer;
    lsFileToDelete : string;
  begin
    for i := 1 to 16 do begin
      lsFileToDelete := fOutputFileSpec + intToStr( i ) + '.png';
      if FileExists( lsFileToDelete ) then begin
        DeleteFile( lsFileToDelete );
        application.ProcessMessages;
        sleep( 100 );
        application.ProcessMessages;
      end;
    end;
  end; // procedure TPngOutThreadTest.DeleteFiles;

  destructor TPngOutThreadTest.Destroy;
  begin
    DeleteOutputFiles;
    inherited Destroy;
  end; // destructor TPngOutThreadTest.Destroy;

  function TPngOutThreadTest.GetScore(ai_index : integer) : integer;
  begin
    if fInvalidRun then begin
      result := 0;
    end else begin
      result := inherited GetScore( ai_index );
      result := fMaxNbrOfThreads * result;
    end; // if
  end; // function TPngOutThreadTest.GetScore

  procedure TPngOutThreadTest.BeforeTest;
  const
    PNG_OUT_FILE_SPEC = 'osmfiles\pngout.exe ';
    TARGET_FILE_SPEC = 'osmfiles\COSBI.jpg ';
  var
    i : integer;
  begin
    inherited;
    fOutputFileSpec := fTargetDrive + ':\delete_me_';
    DeleteOutputFiles;
    // create a thread for each processor:
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunchPngOutThread[ i ] := TLaunchProgramThread.Create;
      fLaunchPngOutThread[ i ].OnTerminate := ThreadDone;
      fLaunchPngOutThread[ i ].ProgramCommand :=
        '"' + ExtractFilePath( Application.ExeName ) + PNG_OUT_FILE_SPEC + '" '
        + ' "' + ExtractFilePath( Application.ExeName )
        + TARGET_FILE_SPEC + '" '
        + fOutputFileSpec + intToStr( i ) + '.png';
    end;
    if assigned( ffrmStatus ) then ffrmStatus.AddLine( 'Spawning ' +
                            IntToStr( fMaxNbrOfThreads ) +
                            ' thread(s)...' );
    application.ProcessMessages;
    fNbrOfThreadsRunning := 0;
  end; // procedure TSha1Test.BeforeTest

  procedure TPngOutThreadTest.RunTest;
  var
    i : integer;
  begin
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunchPngOutThread[ i ].Resume;
      inc( fNbrOfThreadsRunning );
    end; // for
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure TPngOutThreadTest.RunTest;

  procedure TPngOutThreadTest.ThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    if TLaunchProgramThread( Sender ).ReturnCode <> 0 then begin
      fInvalidRun := TRUE;
    end; // if
    if fNbrOfThreadsRunning = 0 then begin
      // might need to add code here after last thread ends
    end else if fNbrOfThreadsRunning >= 1 then begin
      if assigned( ffrmStatus ) then begin
        ffrmStatus.AddLine( 'Thread complete.' +
                            IntToStr( fNbrOfThreadsRunning ) +
                            ' threads running.' );
      end;
    end;
    application.ProcessMessages;
  end; // procedure TPngOutThreadTest.ThreadDone;
// TPngOutThreadTest ends ...............................................

// TUpxThreadTest.....................................................
// Class name: TPngOutThreadTest
// Author: Van Smith
// Date: May 19, 2005
  constructor TUpxThreadTest.Create;
  begin
    inherited;
    fTestName   := 'Upx Threads Test';
    fTestDescription := 'This tests launches n instances of a Upx test where n = # of logical processors. ' +
      'Each test compresses "CosbiOpenSourceMark.exe".';
    fTestVersion:= '1.0';
    fTestType   := ttSystem;
    fTestAuthor := 'Van Smith, Kalumba';
    fReferenceTime :=  4.69697 / 2;
    fShowStatus := FALSE;
    fQTestType := qtUpx;
    fTargetDrive := 'c';
    fPerformanceVectors := [ pvDataSetTransform ];
  end; // constructor TUpxThreadTest.Create;

  procedure TUpxThreadTest.DeleteOutputFiles;
  var
    i : integer;
    lsFileToDelete : string;
  begin
    for i := 1 to 16 do begin
      lsFileToDelete := fOutputFileSpec + intToStr( i ) + '.exe';
      DeleteFile( lsFileToDelete );
    end;
  end; // procedure TUpxThreadTest.DeleteFiles;

  destructor TUpxThreadTest.Destroy;
  begin
    DeleteOutputFiles;
    inherited Destroy;
  end; // destructor TUpxThreadTest.Destroy;

  procedure TUpxThreadTest.BeforeTest;
  var
    lsUpxFileSpec : string;
    lsFilesToUpx   : string;
  var
    i : integer;
  begin
    inherited;
    // create command string: osmfiles\upx.exe --best --crp-ms=999999 --nrv2b -o c:\osmark_delete_me_1.exe ..\CosbiOpenSourceMark.exe
    lsFilesToUpx := '"' + Application.ExeName + '"';
    lsUpxFileSpec := '"' + ExtractFilePath( Application.ExeName ) + 'osmfiles\upx.exe" --best --crp-ms=999999 --nrv2b -o ';
    fOutputFileSpec := fTargetDrive + ':\osmark_delete_me_';
    DeleteOutputFiles;
    // create a thread for each processor:
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunchUpxThread[ i ] := TLaunchProgramThread.Create;
      fLaunchUpxThread[ i ].OnTerminate := ThreadDone;
      fLaunchUpxThread[ i ].ProgramCommand :=  lsUpxFileSpec + fOutputFileSpec
        + intToStr( i ) + '.exe ' + lsFilesToUpx;
    end;
    if assigned( ffrmStatus ) then ffrmStatus.AddLine( 'Spawning ' +
                            IntToStr( fMaxNbrOfThreads ) +
                            ' thread(s)...' );
    application.ProcessMessages;
    fNbrOfThreadsRunning := 0;
  end; // procedure TUpxThreadTest.BeforeTest

  procedure TUpxThreadTest.RunTest;
  var
    i : integer;
  begin
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunchUpxThread[ i ].Resume;
      inc( fNbrOfThreadsRunning );
    end; // for
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure TUpxThreadTest.RunTest;

  function TUpxThreadTest.GetScore(ai_index : integer) : integer;
  begin
    if fInvalidRun then begin
      result := 0;
    end else begin
      result := inherited GetScore( ai_index );
      result := fMaxNbrOfThreads * result;
    end; // if
  end; // function TUpxThreadTest.GetScore

  procedure TUpxThreadTest.ThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    if TLaunchProgramThread( Sender ).ReturnCode <> 0 then begin
      fInvalidRun := TRUE;
    end; // if
    if fNbrOfThreadsRunning = 0 then begin
      // might need to add code here after last thread ends
    end else if fNbrOfThreadsRunning >= 1 then begin
      if assigned( ffrmStatus ) then begin
        ffrmStatus.AddLine( 'Thread complete.' +
                            IntToStr( fNbrOfThreadsRunning ) +
                            ' threads running.' );
      end;
    end;
    application.ProcessMessages;
  end; // procedure TUpxThreadTest.ThreadDone;
// TUpxThreadTest ends ...............................................

// TAesThreadTest.....................................................
// Class name: TAesThreadTest
// Author: Van Smith
// Date: March 17, 2005
  constructor TAesThreadTest.Create;
  begin
    inherited;
    fTestName   := 'AES Encrypt/Decrypt Thread Test';
    fTestDescription := 'This tests launches n instances of the AES Encrypt/Decrypt test were n = number of virtual processers.'
      + CR_LF + '100,000 repetitions on 60kB blocks.' + CR_LF
      + 'SCORE IS NORMALIZED AGAINST A 1.83GHz VIA C7.';
    fTestVersion:= '1.0';
    fTestType   := ttCryptography;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 5.28;
    fShowStatus := FALSE;
    fQTestType := qtEncryptDecrypt;
    fPerformanceVectors := [ pvSecurity ];
  end; // constructor TAesThreadTest.Create;

  destructor TAesThreadTest.Destroy;
  begin
    inherited Destroy;
  end; // destructor TAesThreadTest.Destroy;

  procedure TAesThreadTest.BeforeTest;
  const
    AES_FILE_SPEC = 'osmfiles\AESBench.exe';
  var
    i : integer;
  begin
    inherited;
    // create a thread for each processor:
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunchAesThread[ i ] := TLaunchProgramThread.Create;
      fLaunchAesThread[ i ].OnTerminate := ThreadDone;
      fLaunchAesThread[ i ].ProgramCommand :=
        '"' + ExtractFilePath( Application.ExeName ) + AES_FILE_SPEC + '"'
        + ' -kb 60';
    end;
    if assigned( ffrmStatus ) then ffrmStatus.AddLine( 'Spawning ' +
                            IntToStr( fMaxNbrOfThreads ) +
                            ' thread(s)...' );
    application.ProcessMessages;
    fNbrOfThreadsRunning := 0;
  end; // procedure TAesThreadTest.BeforeTest

  procedure TAesThreadTest.RunTest;
  var
    i : integer;
  begin
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunchAesThread[ i ].Resume;
      inc( fNbrOfThreadsRunning );
    end; // for
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure TAesThreadTest.RunTest;

  function TAesThreadTest.GetScore(ai_index : integer) : integer;
  begin
    if fInvalidRun then begin
      result := 0;
    end else begin
      result := inherited GetScore( ai_index );
      result := fMaxNbrOfThreads * result;
    end; // if
  end; // function TAesThreadTest.GetScore

  procedure TAesThreadTest.ThreadDone(Sender: TObject);
  begin
    if TLaunchProgramThread( Sender ).ReturnCode <> 0 then begin
      fInvalidRun := TRUE;
    end; // if
    dec( fNbrOfThreadsRunning );
    if fNbrOfThreadsRunning = 0 then begin
      // might need to add code here after last thread ends
    end else if fNbrOfThreadsRunning >= 1 then begin
      if assigned( ffrmStatus ) then begin
        ffrmStatus.AddLine( 'Thread complete.' +
                            IntToStr( fNbrOfThreadsRunning ) +
                            ' thread(s) running.' );
      end;
    end;
    application.ProcessMessages;
  end; // procedure TAesThreadTest.ThreadDone;
// TAesThreadTest ends ...............................................

// T7zipThreadTest.....................................................
// Class name: TPngOutThreadTest
// Author: Van Smith
// Date: May 18, 2005
  constructor T7zipThreadTest.Create;
  begin
    inherited;
    fTestName   := '7zip Thread Test';
    fTestDescription := 'This tests launches n instances of a 7zip test where n = # of logical processors. ' +
      'Each test compresses the "osmfiles" directory.';
    fTestVersion:= '1.0';
    fTestType   := ttSystem;
    fTestAuthor := 'Van Smith, Kalumba';
    fReferenceTime :=  61.3 / 2;
    fShowStatus := FALSE;
    fQTestType := qt7zip;
    fTargetDrive := 'c';
    fPerformanceVectors := [ pvDataSetTransform ];
  end; // constructor T7zipThreadTest.Create;

  procedure T7zipThreadTest.DeleteOutputFiles;
  var
    i : integer;
    lsFileToDelete : string;
  begin
    for i := 1 to 16 do begin
      lsFileToDelete := fOutputFileSpec + intToStr( i ) + '.zip';
      DeleteFile( lsFileToDelete );
    end;
  end; // procedure T7zipThreadTest.DeleteFiles;

  destructor T7zipThreadTest.Destroy;
  begin
    DeleteOutputFiles;
    inherited Destroy;
  end; // destructor T7zipThreadTest.Destroy;

  function T7zipThreadTest.GetScore(ai_index : integer) : integer;
  begin
    if fInvalidRun then begin
      result := 0;
    end else begin
      result := inherited GetScore( ai_index );
      result := fMaxNbrOfThreads * result;
    end; // if
  end; // function T7zipThreadTest.GetScore

  procedure T7zipThreadTest.BeforeTest;
  var
    ls7zipFileSpec : string;
    lsFilesToZip   : string;
    i : integer;
  begin
    inherited;
    lsFilesToZip :=  '"' + ExtractFilePath( Application.ExeName ) + '"';
    ls7zipFileSpec := '"' + ExtractFilePath( Application.ExeName ) + 'osmfiles\7za.exe" a ';
    fOutputFileSpec := fTargetDrive + ':\osmark_delete_me_';
    DeleteOutputFiles;
    // create a thread for each processor:
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunch7zipThread[ i ] := TLaunchProgramThread.Create;
      fLaunch7zipThread[ i ].OnTerminate := ThreadDone;
      fLaunch7zipThread[ i ].ProgramCommand := ls7zipFileSpec + fOutputFileSpec
        + intToStr( i ) + '.zip ' + lsFilesToZip;;
    end;
    if assigned( ffrmStatus ) then ffrmStatus.AddLine( 'Spawning ' +
                            IntToStr( fMaxNbrOfThreads ) +
                            ' thread(s)...' );
    application.ProcessMessages;
    fNbrOfThreadsRunning := 0;
  end; // procedure T7zipThreadTest.BeforeTest

  procedure T7zipThreadTest.RunTest;
  var
    i : integer;
  begin
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunch7zipThread[ i ].Resume;
      inc( fNbrOfThreadsRunning );
    end; // for
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure T7zipThreadTest.RunTest;

  procedure T7zipThreadTest.ThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    if TLaunchProgramThread( Sender ).ReturnCode <> 0 then begin
      fInvalidRun := TRUE;
    end; // if
    if fNbrOfThreadsRunning = 0 then begin
      // might need to add code here after last thread ends
    end else if fNbrOfThreadsRunning >= 1 then begin
      if assigned( ffrmStatus ) then begin
        ffrmStatus.AddLine( 'Thread complete.' +
                            IntToStr( fNbrOfThreadsRunning ) +
                            ' threads running.' );
      end;
    end;
    application.ProcessMessages;
  end; // procedure T7zipThreadTest.ThreadDone;
// T7zipThreadTest ends ...............................................

// TLameEncoderThreadTest begins...............................................
// Class name: TLameEncoderThreadTest
// Author: Van Smith
// Date: September 14, 2005
  constructor TLameEncoderThreadTest.Create;
  begin
    inherited;
    fTestName   := 'Lame MP3 Encoder Thread Test';
    fTestDescription := 'This tests launches n instances of the Lame mp3 encoder test where n = # of logical processors.';
    fTestVersion:= '1.0';
    fTestType   := ttCpu;
    fTestAuthor := 'Van Smith';
    fReferenceTime :=  11.3 / 2;
    fShowStatus := FALSE;
    fQTestType := qtLame;
    fTargetDrive := 'c';
    fPerformanceVectors := [ pvDataSetTransform ];
  end; // constructor TLameEncoderThreadTest.Create;

  procedure TLameEncoderThreadTest.DeleteOutputFiles;
  var
    i : integer;
    lsFileToDelete : string;
  begin
    for i := 1 to 16 do begin
      lsFileToDelete := fOutputFileSpec + intToStr( i ) + '.mp3';
      DeleteFile( lsFileToDelete );
    end;
  end; // procedure TLameEncoderThreadTest.DeleteFiles;

  destructor TLameEncoderThreadTest.Destroy;
  begin
    DeleteOutputFiles;
    inherited Destroy;
  end; // destructor TLameEncoderThreadTest.Destroy;

  function TLameEncoderThreadTest.GetScore(ai_index : integer) : integer;
  begin
    if fInvalidRun then begin
      result := 0;
    end else begin
      result := inherited GetScore( ai_index );
      result := fMaxNbrOfThreads * result;
    end; // if
  end; // function TLameEncoderThreadTest.GetScore

  procedure TLameEncoderThreadTest.BeforeTest;
  var
    lsLameFileSpec : string;
    lsFileToEncode : string;
    i : integer;
  begin
    inherited;
    lsFileToEncode :=  '"' + ExtractFilePath( Application.ExeName )
      + 'osmfiles\rockne_JusticeDay.wav" ';
    lsLameFileSpec := '"' + ExtractFilePath( Application.ExeName )
      + 'osmfiles\lame.exe" ';
    fOutputFileSpec := fTargetDrive + ':\osmark_delete_me_';
    DeleteOutputFiles;
    // create a thread for each processor:
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunchLameThread[ i ] := TLaunchProgramThread.Create;
      fLaunchLameThread[ i ].OnTerminate := ThreadDone;
      fLaunchLameThread[ i ].ProgramCommand := lsLameFileSpec + lsFileToEncode
        + fOutputFileSpec + intToStr( i ) + '.mp3 ';
    end;
    if assigned( ffrmStatus ) then ffrmStatus.AddLine( 'Spawning ' +
                            IntToStr( fMaxNbrOfThreads ) +
                            ' thread(s)...' );
    application.ProcessMessages;
    fNbrOfThreadsRunning := 0;
  end; // procedure TLameEncoderThreadTest.BeforeTest

  procedure TLameEncoderThreadTest.RunTest;
  var
    i : integer;
  begin
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunchLameThread[ i ].Resume;
      inc( fNbrOfThreadsRunning );
    end; // for
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure TLameEncoderThreadTest.RunTest;

  procedure TLameEncoderThreadTest.ThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    if TLaunchProgramThread( Sender ).ReturnCode <> 0 then begin
      fInvalidRun := TRUE;
    end; // if
    if fNbrOfThreadsRunning = 0 then begin
      // might need to add code here after last thread ends
    end else if fNbrOfThreadsRunning >= 1 then begin
      if assigned( ffrmStatus ) then begin
        ffrmStatus.AddLine( 'Thread complete.' +
                            IntToStr( fNbrOfThreadsRunning ) +
                            ' threads running.' );
      end;
    end;
    application.ProcessMessages;
  end; // procedure TLameEncoderThreadTest.ThreadDone;
// TLameEncoderThreadTest ends ...............................................

// TOggEncoderThreadTest begins...............................................
// Class name: TOggEncoderThreadTest
// Author: Van Smith
// Date: September 14, 2005
  constructor TOggEncoderThreadTest.Create;
  begin
    inherited;
    fTestName   := 'Ogg Vorbis Audio Encoder Thread Test';
    fTestDescription := 'This tests launches n instances of the Ogg Vorbis encoder "oggenc2" test where n = # of logical processors.';
    fTestVersion:= '1.0';
    fTestType   := ttCpu;
    fTestAuthor := 'Van Smith';
    fReferenceTime :=  14.5499 / 2;
    fShowStatus := FALSE;
    fQTestType := qtOgg;
    fTargetDrive := 'c';
    fPerformanceVectors := [ pvDataSetTransform ];
  end; // constructor TOggEncoderThreadTest.Create;

  procedure TOggEncoderThreadTest.DeleteOutputFiles;
  var
    i : integer;
    lsFileToDelete : string;
  begin
    for i := 1 to 16 do begin
      lsFileToDelete := fOutputFileSpec + intToStr( i ) + '.ogg';
      DeleteFile( lsFileToDelete );
    end;
  end; // procedure TOggEncoderThreadTest.DeleteFiles;

  destructor TOggEncoderThreadTest.Destroy;
  begin
    DeleteOutputFiles;
    inherited Destroy;
  end; // destructor TOggEncoderThreadTest.Destroy;

  function TOggEncoderThreadTest.GetScore(ai_index : integer) : integer;
  begin
    if fInvalidRun then begin
      result := 0;
    end else begin
      result := inherited GetScore( ai_index );
      result := fMaxNbrOfThreads * result;
    end; // if
  end; // function TOggEncoderThreadTest.GetScore

  procedure TOggEncoderThreadTest.BeforeTest;
  var
    lsOggFileSpec : string;
    lsFileToEncode : string;
    i : integer;
  begin
    inherited;
    lsFileToEncode :=  '"' + ExtractFilePath( Application.ExeName )
      + 'osmfiles\rockne_JusticeDay.wav" -o ';
    lsOggFileSpec := '"' + ExtractFilePath( Application.ExeName )
      + 'osmfiles\oggenc2.exe" ';
    fOutputFileSpec := fTargetDrive + ':\osmark_delete_me_';
    DeleteOutputFiles;
    // create a thread for each processor:
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunchOggThread[ i ] := TLaunchProgramThread.Create;
      fLaunchOggThread[ i ].OnTerminate := ThreadDone;
      fLaunchOggThread[ i ].ProgramCommand := lsOggFileSpec + lsFileToEncode
        + fOutputFileSpec + intToStr( i ) + '.ogg ';
    end;
    if assigned( ffrmStatus ) then ffrmStatus.AddLine( 'Spawning ' +
                            IntToStr( fMaxNbrOfThreads ) +
                            ' thread(s)...' );
    application.ProcessMessages;
    fNbrOfThreadsRunning := 0;
  end; // procedure TOggEncoderThreadTest.BeforeTest

  procedure TOggEncoderThreadTest.RunTest;
  var
    i : integer;
  begin
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunchOggThread[ i ].Resume;
      inc( fNbrOfThreadsRunning );
    end; // for
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure TOggEncoderThreadTest.RunTest;

  procedure TOggEncoderThreadTest.ThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    if TLaunchProgramThread( Sender ).ReturnCode <> 0 then begin
      fInvalidRun := TRUE;
    end; // if
    if fNbrOfThreadsRunning = 0 then begin
      // might need to add code here after last thread ends
    end else if fNbrOfThreadsRunning >= 1 then begin
      if assigned( ffrmStatus ) then begin
        ffrmStatus.AddLine( 'Thread complete.' +
                            IntToStr( fNbrOfThreadsRunning ) +
                            ' threads running.' );
      end;
    end;
    application.ProcessMessages;
  end; // procedure TOggEncoderThreadTest.ThreadDone;
// TOggEncoderThreadTest ends ...............................................

// TCppCompilerThreadTest begins...............................................
// Class name: TCppCompilerThreadTest
// Author: Van Smith
// Date: October 11, 2005
  constructor TCppCompilerThreadTest.Create;
  begin
    inherited;
    fTestName   := 'C++ Compiler Thread Test';
    fTestDescription := 'Using the g++ (mingw) compiler, this test compiles the miniBench C++ benchmark.  The test spawns a thread for each logical processor in the system.';
    fTestVersion:= '1.0';
    fTestType   := ttCpu;
    fTestAuthor := 'Van Smith';
    fReferenceTime :=  2.13166 / 2;
    fShowStatus := FALSE;
    fQTestType := qtCppCompiler;
    fTargetDrive := 'c';
    fPerformanceVectors := [ pvDataSetTransform ];
  end; // constructor TCppCompilerThreadTest.Create;

  procedure TCppCompilerThreadTest.DeleteOutputFiles;
  var
    i : integer;
    lsFileToDelete : string;
  begin
    for i := 1 to 16 do begin
      lsFileToDelete := fOutputFileSpec + intToStr( i ) + '.exe';
      DeleteFile( lsFileToDelete );
    end;
  end; // procedure TCppCompilerThreadTest.DeleteFiles;

  destructor TCppCompilerThreadTest.Destroy;
  begin
    DeleteOutputFiles;
    inherited Destroy;
  end; // destructor TCppCompilerThreadTest.Destroy;

  function TCppCompilerThreadTest.GetScore(ai_index : integer) : integer;
  begin
    if fInvalidRun then begin
      result := 0;
    end else begin
      result := inherited GetScore( ai_index );
      result := fMaxNbrOfThreads * result;
    end; // if
  end; // function TCppCompilerThreadTest.GetScore

  procedure TCppCompilerThreadTest.AfterTest;
  begin
    inherited;
    ChDir( fOriginalDirectory );
  end; // procedure TMazeTest.BeforeTest

  procedure TCppCompilerThreadTest.BeforeTest;
  var
    lsCompilerFileSpec : string;
    lsPath : string;
    i : integer;
  begin
    inherited;
    fOriginalDirectory := GetCurrentDir;
    lsPath := ExtractFilePath( Application.ExeName ) + 'osmfiles\compile\';
    ChDir( lsPath );
    lsCompilerFileSpec := 'g++ main.cpp -L"' + lsPath + '" -o ';
    fOutputFileSpec := fTargetDrive + ':\osmark_mb_delete_me_';
    DeleteOutputFiles;
    // create a thread for each processor:
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunchCompilerThread[ i ] := TLaunchProgramThread.Create;
      fLaunchCompilerThread[ i ].OnTerminate := ThreadDone;
      fLaunchCompilerThread[ i ].ProgramCommand := lsCompilerFileSpec
        + fOutputFileSpec + intToStr( i ) + '.exe -O3';
    end;
    if assigned( ffrmStatus ) then ffrmStatus.AddLine( 'Spawning ' +
                            IntToStr( fMaxNbrOfThreads ) +
                            ' thread(s)...' );
    application.ProcessMessages;
    fNbrOfThreadsRunning := 0;
  end; // procedure TCppCompilerThreadTest.BeforeTest

  procedure TCppCompilerThreadTest.RunTest;
  var
    i : integer;
  begin
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunchCompilerThread[ i ].Resume;                                                              
      inc( fNbrOfThreadsRunning );
    end; // for
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure TCppCompilerThreadTest.RunTest;

  procedure TCppCompilerThreadTest.ThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    if TLaunchProgramThread( Sender ).ReturnCode <> 0 then begin
      fInvalidRun := TRUE;
    end; // if
    if fNbrOfThreadsRunning = 0 then begin
      // might need to add code here after last thread ends
    end else if fNbrOfThreadsRunning >= 1 then begin
      if assigned( ffrmStatus ) then begin
        ffrmStatus.AddLine( 'Thread complete.' +
                            IntToStr( fNbrOfThreadsRunning ) +
                            ' threads running.' );
      end;
    end;
    application.ProcessMessages;
  end; // procedure TCppCompilerThreadTest.ThreadDone;
// TCppCompilerThreadTest ends ...............................................

// TMetaballsThreadTest.....................................................
// Class name: TMetaballsThreadTest
// Author: Ty Smith, Van Smith
// Date: March 17, 2005
  constructor TMetaballsThreadTest.Create;
  begin
    inherited;
    fTestName   := 'Metaballs DirectX 9 Thread Test';
    fTestDescription := 'A simple DirectX 9 rendering test.';
    fTestVersion:= '1.0';
    fTestType   := tt3dGraphics;
    fTestAuthor := 'Ty Smith, Van Smith';
    fReferenceTime := 23.87 / 2;
    fShowStatus := FALSE;
    fQTestType := qtMetaballs;
    fPerformanceVectors := [ pv3dGaming, pvMathScienceEngineering ];
  end; // constructor TMetaballsThreadTest.Create;

  destructor TMetaballsThreadTest.Destroy;
  begin
    inherited Destroy;
  end; // destructor TMetaballsThreadTest.Destroy;

  procedure TMetaballsThreadTest.BeforeTest;
  const
    METABALLS_FILE_SPEC = 'osmfiles\metaballs.exe';
  var
    i : integer;
  begin
    inherited;
    // create a thread for each processor:
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunchMetaballsThread[ i ] := TLaunchProgramThread.Create;
      fLaunchMetaballsThread[ i ].OnTerminate := ThreadDone;
      fLaunchMetaballsThread[ i ].ProgramCommand :=
        '"' + ExtractFilePath( Application.ExeName ) + METABALLS_FILE_SPEC;
    end;
    if assigned( ffrmStatus ) then ffrmStatus.AddLine( 'Spawning ' +
                            IntToStr( fMaxNbrOfThreads ) +
                            ' thread(s)...' );
    application.ProcessMessages;
    fNbrOfThreadsRunning := 0;
  end; // procedure TMetaballsThreadTest.BeforeTest

  procedure TMetaballsThreadTest.RunTest;
  var
    i : integer;
  begin
    for i := 1 to fMaxNbrOfThreads do begin
      fLaunchMetaballsThread[ i ].Resume;
      inc( fNbrOfThreadsRunning );
    end; // for
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure TMetaballsThreadTest.RunTest;

  function TMetaballsThreadTest.GetScore(ai_index : integer) : integer;
  begin
    if fInvalidRun then begin
      result := 0;
    end else begin
      result := inherited GetScore( ai_index );
      result := fMaxNbrOfThreads * result;
    end; // if
  end; // function TMetaballsThreadTest.GetScore

  procedure TMetaballsThreadTest.ThreadDone(Sender: TObject);
  begin
    if TLaunchProgramThread( Sender ).ReturnCode <> 0 then begin
      fInvalidRun := TRUE;
    end; // if
    dec( fNbrOfThreadsRunning );
    if fNbrOfThreadsRunning = 0 then begin
      // might need to add code here after last thread ends
    end else if fNbrOfThreadsRunning >= 1 then begin
      if assigned( ffrmStatus ) then begin
        ffrmStatus.AddLine( 'Thread complete.' +
                            IntToStr( fNbrOfThreadsRunning ) +
                            ' thread(s) running.' );
      end;
    end;
    application.ProcessMessages;
  end; // procedure TMetaballsThreadTest.ThreadDone;
// TMetaballsThreadTest ends ...............................................

end.
