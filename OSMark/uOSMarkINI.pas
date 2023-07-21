unit uOSMarkINI;
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
// Unit name: uOSMarkINI
// Unit description: houses all of the INI file logic
// Author: Van Smith
// Date: August 30, 2005
// OS dependent: Yes: Windows
// Resolution dependent: No.
// External unit dependencies: COSBI_Common, uCOSBI_TestBaseClass, GR32_Blend,
//   GR32_Image, GR32, Maze
//==============================================================================
// Modification History
// ------------ -------
// Ver  Date   Who    Description
// ==== ====== ====== ==========================================================
// 1.0 050830 Van     Created.
//==============================================================================

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, IniFiles, uOSMarkSuite,
  uCOSBI_TTest, CosbiCpuid;

type

TOSMarkINI = class( TObject )
private
  fIterations     : integer;
  fProcessIdleTasks : Boolean;
  fSpinUp         : Boolean;
  fShowStatus     : Boolean;
  fDefrag         : Boolean;
  fTargetDrive    : char;
  fSelectedTests  : TQTestSet;
  fThreadCountNbr : integer;
  fThreadCount    : TThreadCount;
  function GetThreadCount : TThreadCount;
  procedure SetThreadCount( Value : TThreadCount );
protected
public
  procedure ReadIni;
  procedure WriteIniFile;
  property Defrag         : Boolean read fDefrag write fDefrag;
  property Iterations     : integer read fIterations write fIterations;
  property ProcessIdleTasks : Boolean read fProcessIdleTasks write fProcessIdleTasks;
  property SelectedTests : TQTestSet read fSelectedTests write fSelectedTests;
  property ShowStatus     : Boolean read fShowStatus write fShowStatus;
  property SpinUp         : Boolean read fSpinUp write fSpinUp;
  property TargetDrive    : char read fTargetDrive write fTargetDrive;
  property ThreadCount : TThreadCount read GetThreadCount write SetThreadCount;
end; // TOSMarkINI

implementation

function TOSMarkINI.GetThreadCount : TThreadCount;
begin
  case fThreadCountNbr of
    0 : result := tcAuto;
    1 : result := tc1;
    2 : result := tc2;
    4 : result := tc4;
    8 : result := tc8;
    16: result := tc16;
    32: result := tc32;
  end; // case
end;

procedure TOSMarkINI.SetThreadCount( Value : TThreadCount );
begin
  fThreadCount := Value;
  case Value of
    tcAuto  : fThreadCountNbr := 0;
    tc1     : fThreadCountNbr := 1;
    tc2     : fThreadCountNbr := 2;
    tc4     : fThreadCountNbr := 4;
    tc8     : fThreadCountNbr := 8;
    tc16    : fThreadCountNbr := 16;
    tc32    : fThreadCountNbr := 32;
  end; // case
end;

procedure TOSMarkINI.ReadIni;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
  try
    // read settings
    fIterations       := Ini.ReadInteger('Settings', 'Iterations', 1 );
    fSpinUp           := Ini.ReadBool( 'Settings', 'SpinUp', TRUE );
    fShowStatus       := Ini.ReadBool( 'Settings', 'Status', TRUE );
    fDefrag           := Ini.ReadBool( 'Settings', 'Defrag', FALSE );
    fTargetDrive      := Ini.ReadString( 'Settings', 'TargetDrive', 'c' )[1];
    fProcessIdleTasks := Ini.ReadBool( 'Settings', 'ProcessIdleTasks', FALSE );
    fThreadCountNbr   := Ini.ReadInteger('Settings', 'ThreadCount', 0 );
    // read tests
    fSelectedTests          := [];
    if Ini.ReadBool( 'Tests', 'GridBlast', TRUE ) then fSelectedTests := fSelectedTests + [qtGrid];
    if Ini.ReadBool( 'Tests', 'Fib', TRUE ) then fSelectedTests := fSelectedTests + [qtFib];
    if Ini.ReadBool( 'Tests', 'GridBlastFP', TRUE ) then fSelectedTests := fSelectedTests + [qtGridFP];
    if Ini.ReadBool( 'Tests', 'NBody', TRUE ) then fSelectedTests := fSelectedTests + [qtNBody];
    if Ini.ReadBool( 'Tests', 'PlotTrig', TRUE ) then fSelectedTests := fSelectedTests + [qtPlotTrig];
    if Ini.ReadBool( 'Tests', 'PlotTrig2', TRUE ) then fSelectedTests := fSelectedTests + [qtPlotTrig2];
    if Ini.ReadBool( 'Tests', 'PlotLines', TRUE ) then fSelectedTests := fSelectedTests + [qtPlotLines];
    if Ini.ReadBool( 'Tests', 'RandomDots', TRUE ) then fSelectedTests := fSelectedTests + [qtRandomDots];
    if Ini.ReadBool( 'Tests', 'RandomCircles', TRUE ) then fSelectedTests := fSelectedTests + [qtCircles];
    if Ini.ReadBool( 'Tests', 'Mazes', TRUE ) then fSelectedTests := fSelectedTests + [qtMaze];
    if Ini.ReadBool( 'Tests', 'MazeThreads', TRUE ) then fSelectedTests := fSelectedTests + [qtMazeThreads];
    if Ini.ReadBool( 'Tests', 'Fern', TRUE ) then fSelectedTests := fSelectedTests + [qtFern];
    if Ini.ReadBool( 'Tests', 'RichEd', TRUE ) then fSelectedTests := fSelectedTests + [qtRichEd];
    if Ini.ReadBool( 'Tests', 'CalculatePi', TRUE ) then fSelectedTests := fSelectedTests + [qtPi];
    if Ini.ReadBool( 'Tests', 'Dhrystone', TRUE ) then fSelectedTests := fSelectedTests + [qtDhrystone];
    if Ini.ReadBool( 'Tests', 'Whetstone', TRUE ) then fSelectedTests := fSelectedTests + [qtWhetstone];
    if Ini.ReadBool( 'Tests', 'BandwidthBP64', TRUE ) then fSelectedTests := fSelectedTests + [qtBandwidthBP64];
    if Ini.ReadBool( 'Tests', 'MemLatency', TRUE ) then fSelectedTests := fSelectedTests + [qtMemLatency];
    if Ini.ReadBool( 'Tests', 'OThreads', TRUE ) then fSelectedTests := fSelectedTests + [qtOrthogonalThreads];
    if Ini.ReadBool( 'Tests', 'IThreads', TRUE ) then fSelectedTests := fSelectedTests + [qtIdenticalThreads];
    if Ini.ReadBool( 'Tests', 'JpgDecode', TRUE ) then fSelectedTests := fSelectedTests + [qtJpgDecode];
    if Ini.ReadBool( 'Tests', 'ImageResize', TRUE ) then fSelectedTests := fSelectedTests + [qtImageResize];
    if Ini.ReadBool( 'Tests', 'ImageRotate', TRUE ) then fSelectedTests := fSelectedTests + [qtImageRotate];
    if Ini.ReadBool( 'Tests', 'Mp3Encode', TRUE ) then fSelectedTests := fSelectedTests + [qtGogoEncode];
    if Ini.ReadBool( 'Tests', 'WebPageLoad', TRUE ) then fSelectedTests := fSelectedTests + [qtWebPageLoad];
    if Ini.ReadBool( 'Tests', 'ZipCompress', TRUE ) then fSelectedTests := fSelectedTests + [qtZipCompress];
    if Ini.ReadBool( 'Tests', 'EncryptDecrypt', TRUE ) then fSelectedTests := fSelectedTests + [qtEncryptDecrypt];
    if Ini.ReadBool( 'Tests', 'FileCopy', TRUE ) then fSelectedTests := fSelectedTests + [qtFileCopy];
    if Ini.ReadBool( 'Tests', 'Lorenz', TRUE ) then fSelectedTests := fSelectedTests + [qtLorenz];
    if Ini.ReadBool( 'Tests', 'NBodyOpenGL', TRUE ) then fSelectedTests := fSelectedTests + [qtNBodyOpenGL];
    if Ini.ReadBool( 'Tests', 'DhrystoneThreadTest', TRUE ) then fSelectedTests := fSelectedTests + [qtDhrystoneThreads];
    if Ini.ReadBool( 'Tests', 'WhetstoneThreadTest', TRUE ) then fSelectedTests := fSelectedTests + [qtWhetstoneThreads];
    if Ini.ReadBool( 'Tests', 'PiThreadTest', TRUE ) then fSelectedTests := fSelectedTests + [qtPiThreads];
    if Ini.ReadBool( 'Tests', 'MandelbrotTest', TRUE ) then fSelectedTests := fSelectedTests + [qtMandelbrotThreads];
    if Ini.ReadBool( 'Tests', 'EllipsesTest', TRUE ) then fSelectedTests := fSelectedTests + [qtDrawEllipses];
    if Ini.ReadBool( 'Tests', 'RectanglesTest', TRUE ) then fSelectedTests := fSelectedTests + [qtDrawRectangles];
    if Ini.ReadBool( 'Tests', 'LinesTest', TRUE ) then fSelectedTests := fSelectedTests + [qtDrawLines];
    if Ini.ReadBool( 'Tests', 'FibThreadsTest', TRUE ) then fSelectedTests := fSelectedTests + [qtFibThreads];
    if Ini.ReadBool( 'Tests', 'PngOutThreadsTest', TRUE ) then fSelectedTests := fSelectedTests + [qtPngOut];
    if Ini.ReadBool( 'Tests', '7zipThreadsTest', TRUE ) then fSelectedTests := fSelectedTests + [qt7zip];
    if Ini.ReadBool( 'Tests', 'UpxThreadsTest', TRUE ) then fSelectedTests := fSelectedTests + [qtUpx];
    if Ini.ReadBool( 'Tests', 'NBodyFPU', TRUE )  then fSelectedTests := fSelectedTests + [qtNBodyFPU];
    if Ini.ReadBool( 'Tests', 'NBodySSE2', TRUE ) then fSelectedTests := fSelectedTests + [qtNBodySSE2];
    if Ini.ReadBool( 'Tests', 'NBodySSE2Scalar', TRUE ) then fSelectedTests := fSelectedTests + [qtNBodySSE2Scalar];
    if Ini.ReadBool( 'Tests', 'NBodySSE3', TRUE ) then fSelectedTests := fSelectedTests + [qtNBodySSE3];
    if Ini.ReadBool( 'Tests', 'SSE3Threads', TRUE ) then fSelectedTests := fSelectedTests + [qtSSE3];
    if Ini.ReadBool( 'Tests', 'MontMul', TRUE ) then fSelectedTests := fSelectedTests + [qtMontMul];
    if Ini.ReadBool( 'Tests', 'Sha1', TRUE ) then fSelectedTests := fSelectedTests + [qtSha1];
    if Ini.ReadBool( 'Tests', 'Sha256', TRUE ) then fSelectedTests := fSelectedTests + [qtSha256];
    if Ini.ReadBool( 'Tests', 'AlphaBlend', TRUE ) then fSelectedTests := fSelectedTests + [qtAlphaBlend];
    if Ini.ReadBool( 'Tests', 'AlphaDots', TRUE ) then fSelectedTests := fSelectedTests + [qtAlphaDots];
    if Ini.ReadBool( 'Tests', 'Lame', TRUE ) then fSelectedTests := fSelectedTests + [qtLame];
    if Ini.ReadBool( 'Tests', 'Ogg', TRUE ) then fSelectedTests := fSelectedTests + [qtOgg];
    if Ini.ReadBool( 'Tests', 'CppCompiler', TRUE ) then fSelectedTests := fSelectedTests + [qtCppCompiler];
    if Ini.ReadBool( 'Tests', 'Metaballs', TRUE ) then fSelectedTests := fSelectedTests + [qtMetaballs];
    if NOT CpuidIsSse2Supported then begin
      fSelectedTests := fSelectedTests - [qtNBodySSE2, qtNBodySSE2Scalar, qtNBodySSE3];
    end else if NOT CpuidIsSse3Supported then begin
      fSelectedTests := fSelectedTests - [qtNBodySSE3];
    end; //if
  finally
    FreeAndNil( Ini );
  end; // try..finally
end; // procedure TOSMarkINI.ReadIni;

procedure TOSMarkINI.WriteIniFile;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
  try
    try
      // write settings
      Ini.WriteInteger( 'Settings', 'Iterations', fIterations );
      Ini.WriteBool(    'Settings', 'SpinUp', fSpinUp );
      Ini.WriteBool(    'Settings', 'Status', fShowStatus );
      Ini.WriteBool(    'Settings', 'Defrag', fDefrag  );
      Ini.WriteString(  'Settings', 'TargetDrive', fTargetDrive );
      Ini.WriteBool(    'Settings', 'ProcessIdleTasks', fProcessIdleTasks );
      // write tests
      Ini.WriteBool( 'Tests', 'GridBlast', qtGrid in fSelectedTests );
      Ini.WriteBool( 'Tests', 'GridBlastFP', qtGridFP in fSelectedTests );
      Ini.WriteBool( 'Tests', 'Fib', qtFib in fSelectedTests );
      Ini.WriteBool( 'Tests', 'NBody', qtNBody in fSelectedTests );
      Ini.WriteBool( 'Tests', 'PlotTrig', qtPlotTrig in fSelectedTests );
      Ini.WriteBool( 'Tests', 'PlotTrig2', qtPlotTrig2 in fSelectedTests );
      Ini.WriteBool( 'Tests', 'PlotLines', qtPlotLines in fSelectedTests );
      Ini.WriteBool( 'Tests', 'RandomDots', qtRandomDots in fSelectedTests );
      Ini.WriteBool( 'Tests', 'RandomCircles', qtCircles in fSelectedTests );
      Ini.WriteBool( 'Tests', 'Mazes', qtMaze in fSelectedTests );
      Ini.WriteBool( 'Tests', 'Fern', qtFern in fSelectedTests );
      Ini.WriteBool( 'Tests', 'RichEd', qtRichEd in fSelectedTests );
      Ini.WriteBool( 'Tests', 'CalculatePi', qtPi in fSelectedTests );
      Ini.WriteBool( 'Tests', 'Dhrystone', qtDhrystone in fSelectedTests );
      Ini.WriteBool( 'Tests', 'Whetstone', qtWhetstone in fSelectedTests );
      Ini.WriteBool( 'Tests', 'MazeThreads', qtMazeThreads in fSelectedTests );
      Ini.WriteBool( 'Tests', 'BandwidthBP64', qtBandwidthBP64 in fSelectedTests );
      Ini.WriteBool( 'Tests', 'MemLatency', qtMemLatency in fSelectedTests );
      Ini.WriteBool( 'Tests', 'OThreads', qtOrthogonalThreads in fSelectedTests );
      Ini.WriteBool( 'Tests', 'IThreads', qtIdenticalThreads in  fSelectedTests );
      Ini.WriteBool( 'Tests', 'JpgDecode', qtJpgDecode in fSelectedTests );
      Ini.WriteBool( 'Tests', 'ImageResize', qtImageResize in fSelectedTests );
      Ini.WriteBool( 'Tests', 'ImageRotate', qtImageRotate in fSelectedTests );
      Ini.WriteBool( 'Tests', 'Mp3Encode', qtGogoEncode in fSelectedTests );
      Ini.WriteBool( 'Tests', 'WebPageLoad', qtWebPageLoad in fSelectedTests );
      Ini.WriteBool( 'Tests', 'ZipCompress', qtZipCompress in fSelectedTests );
      Ini.WriteBool( 'Tests', 'EncryptDecrypt', qtEncryptDecrypt in fSelectedTests );
      Ini.WriteBool( 'Tests', 'FileCopy', qtFileCopy in fSelectedTests );
      Ini.WriteBool( 'Tests', 'Lorenz', qtLorenz in fSelectedTests );
      Ini.WriteBool( 'Tests', 'NBodyOpenGL', qtNBodyOpenGL in fSelectedTests );
      Ini.WriteBool( 'Tests', 'DhrystoneThreadTest', qtDhrystoneThreads in fSelectedTests );
      Ini.WriteBool( 'Tests', 'WhetstoneThreadTest', qtWhetstoneThreads in fSelectedTests);
      Ini.WriteBool( 'Tests', 'PiThreadTest', qtPiThreads in fSelectedTests );
      Ini.WriteBool( 'Tests', 'MandelbrotTest', qtMandelbrotThreads in fSelectedTests );
      Ini.WriteBool( 'Tests', 'EllipsesTest', qtDrawEllipses in fSelectedTests );
      Ini.WriteBool( 'Tests', 'RectanglesTest', qtDrawRectangles in fSelectedTests );
      Ini.WriteBool( 'Tests', 'LinesTest', qtDrawLines in fSelectedTests );
      Ini.WriteBool( 'Tests', 'FibThreadsTest', qtFibThreads in fSelectedTests );
      Ini.WriteBool( 'Tests', 'PngOutThreadsTest', qtPngOut in fSelectedTests );
      Ini.WriteBool( 'Tests', '7zipThreadsTest', qt7zip in fSelectedTests );
      Ini.WriteBool( 'Tests', 'UpxThreadsTest', qtUpx in fSelectedTests );
      Ini.WriteBool( 'Tests', 'NBodyFPU', qtNBodyFPU in fSelectedTests );
      Ini.WriteBool( 'Tests', 'NBodySSE2', qtNBodySSE2 in fSelectedTests );
      Ini.WriteBool( 'Tests', 'NBodySSE2Scalar', qtNBodySSE2Scalar in fSelectedTests );
      Ini.WriteBool( 'Tests', 'NBodySSE3', qtNBodySSE3 in fSelectedTests );
      Ini.WriteBool( 'Tests', 'SSE3Threads', qtSSE3 in fSelectedTests );
      Ini.WriteBool( 'Tests', 'MontMul', qtMontMul in fSelectedTests );
      Ini.WriteBool( 'Tests', 'Sha1', qtSha1 in fSelectedTests );
      Ini.WriteBool( 'Tests', 'Sha256', qtSha256 in fSelectedTests );
      Ini.WriteBool( 'Tests', 'AlphaBlend', qtAlphaBlend in fSelectedTests );
      Ini.WriteBool( 'Tests', 'AlphaDots', qtAlphaDots in fSelectedTests );
      Ini.WriteBool( 'Tests', 'Lame', qtLame in fSelectedTests );
      Ini.WriteBool( 'Tests', 'Ogg', qtOgg in fSelectedTests );
      Ini.WriteBool( 'Tests', 'CppCompiler', qtCppCompiler in fSelectedTests );
      Ini.WriteBool( 'Tests', 'Metaballs', qtMetaballs in fSelectedTests );
    except
      raise exception.Create('An error occurred while trying to write to the INI file.');
    end; // try...except
  finally
    FreeAndNil( Ini );
  end; // try..finally
end;// procedure TOSMarkINI.WriteIniFile;

end.
