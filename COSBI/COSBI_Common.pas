unit COSBI_Common;
{
  COSBI: Comprehensive Open Source Benchmarking Initiative
  Copyright (c) 2004 Van Smith

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
// Unit name: COSBI_Common
// Unit description: Contains a library of common routines
// Author: Van Smith
// Date: March 31, 2004
// OS dependent: Yes: Windows
// Resolution dependent: There are screen res routines.
// External unit dependencies: math
//==============================================================================
// Modification History
// ------------ -------
// Ver  Date   Who    Description
// ==== ====== ====== ==========================================================
// 1.0  040331 Van     Created.
//==============================================================================

interface

uses
  // if you use Delphi7, add DateUtils unit in uses clause.
  Windows, Messages, SysUtils, Classes, ShellAPI, Forms, StdCtrls, DateUtils,
  Dialogs, math, Registry, wininet;

type
  TComplexNumber = record
    Real : double;
    Imaginary : double;
  end; // record

  TDynamicIntegerArray = array of Integer;
  //EAbort = class( Exception );

  Bits = (Bit0,  Bit1,  Bit2,  Bit3,  Bit4,  Bit5,  Bit6,  Bit7,
          Bit8,  Bit9,  Bit10, Bit11, Bit12, Bit13, Bit14, Bit15,
          Bit16, Bit17, Bit18, Bit19, Bit20, Bit21, Bit22, Bit23,
          Bit24, Bit25, Bit26, Bit27, Bit28, Bit29, Bit30, Bit31,
          Bit32, Bit33, Bit34, Bit35, Bit36, Bit37, Bit38, Bit39,
          Bit40, Bit41, Bit42, Bit43, Bit44, Bit45, Bit46, Bit47,
          Bit48, Bit49, Bit50, Bit51, Bit52, Bit53, Bit54, Bit55,
          Bit56, Bit57, Bit58, Bit59, Bit60, Bit61, Bit62, Bit63);

  T8Bits  = set of Bit0..Bit7;
  T16Bits = set of Bit0..Bit15;
  T32Bits = set of Bit0..Bit31;
  T64Bits = set of Bit0..Bit63;

  T64bitWord = record
  case byte of
     1: (int64:  int64);
     2: (low32, high32: dword);
  end;

  TRDTSCTimeStamp = record
  case byte of
     1: (int64:  int64);
     2: (low32, high32: dword);
  end;

  Tx86_32_GPRegisters = record
  case byte of
    1: (_eax : LongWord;
        _ebx : LongWord;
        _ecx : LongWord;
        _edx : LongWord);
    2: (str : array [0..15] of AnsiChar);
    3: (str_eax : array [0..3] of AnsiChar;
        str_ebx : array [0..3] of AnsiChar;
        str_ecx : array [0..3] of AnsiChar;
        str_edx : array [0..3] of AnsiChar);
    4: (bits_eax : T32Bits;
        bits_ebx : T32Bits;
        bits_ecx : T32Bits;
        bits_edx : T32Bits);
  end; // Tx86_32_GPRegisters

  T64bitRegister = record
  case byte of
    1: (AsInt64       : Int64);
    2: (LowWord       : LongWord;
        HighWord      : LongWord);
    3: (charArray : array [0..7] of char);
    4: (strLowWord    : array [0..3] of char;
        strHighWord   : array [0..3] of char);
    5: (bitsLowWord   : T32Bits;
        bitsHighWord  : T32Bits);
    6: (bits64   : T64Bits);
    7: (byteArray     : array [0..7] of byte);
  end; // T64BitRegister

  Txmm = packed record
    x: double;
    y: double;
  end; // Txmm

// MSR SECTION=====================================
//  TSpeedStepState = record
//    FsbMultiplier : integer;
//    Vcc : double;
//  end;
//
//  TMinMaxSpeedStepStates = record
//    MinState : TSpeedStepState;
//    MaxState : TSpeedStepState;
//  end;
//
//  TMsrDriver = class(TObject)
//  private
//    { Private declarations }
//  protected
//    function DecodeSpeedStepVcc( EncodedVcc : integer ): double;
//    function EncodeSpeedStepVcc( Vcc : double ): integer;
//  public
//    { Public declarations }
//    function CloseDriver: Boolean;
//    function GetMaxCpuMultiplier( cpu : integer ): cardinal;
//    function GetMaxVcc( cpu : integer ): double;
//    function GetMinMaxSpeedStepStates(cpu: integer): TMinMaxSpeedStepStates;
//    function GetNumberOfCpus: integer;
//    function GetSpeedStepState( cpu : integer ) : TSpeedStepState;
//    function OpenDriver: Boolean;
//    function ReadMsr(cpu, msr: integer): T64bitRegister;
//    function WriteMsr(cpu, msr: integer;
//                      Msr64bitRegister : T64bitRegister
//                      ): T64bitRegister;
//    procedure CachesOff(cpu: integer);
//    procedure CachesOn(cpu: integer);
//    procedure SetSpeedStepMultiplier( cpu, aiCpuMultiplier : integer );
//    procedure SetSpeedStepState(cpu : integer; NewState: TSpeedStepState);
//    procedure SetUnsetMsrBits(cpu, msr: integer;
//                              BitsToSet, BitsToUnset : T64bits);
//    procedure ToggleMsrBits(cpu, msr : integer;
//                            BitsToToggle : T64bitRegister);
//    procedure WriteBackInvalidate( cpu : integer );
//  end;

// process code take from:
// http://www.thedelphimagazine.com/samples/1328/1328.htm
// example usage:
//procedure TForm1.RadioGroup1Click(Sender: TObject);
//var
//  i : integer;
//  LibHandle : THandle;
//  MemUsageStart : integer;
//  DllName : string;
//  ProcessID : DWord;
//begin
//  ...
//  ProcessID := GetCurrentProcessID;
//  // Dummy call to force PSAPI.DLL to be loaded and added to
//  // the workingset
//  MemUsageStart := ProcessMemoryUsage(ProcessID);
//  // Dummy Load and unload the DLL once to get a more
//  // accurate picture of the full workingset
//  LibHandle := Loadlibrary(PChar(DllName));
//  if LibHandle <> 0 then
//    FreeLibrary(LibHandle)
//  else
//    RaiseLastWin32Error;
//  MemUsageStart := ProcessMemoryUsage(ProcessID);
//  for i := 1 to 200 do begin
//    LibHandle := Loadlibrary(PChar(DllName));
//      if LibHandle <> 0 then
//        FreeLibrary(LibHandle)
//      else
//        RaiseLastWin32Error;
//      Caption := 'Memory Lost '+ IntToStr(
//        (ProcessMemoryUsage(ProcessID) - MemUsageStart)
//        div 1024) +' kbytes Loaded '+IntToStr(i);
//      Application.ProcessMessages;
//    end;
//  ...
//end;
  PPROCESS_MEMORY_COUNTERS = ^PROCESS_MEMORY_COUNTERS;
  PROCESS_MEMORY_COUNTERS = record
    cb : DWORD;
    PageFaultCount : DWORD;
    PeakWorkingSetSize : DWORD;
    WorkingSetSize : DWORD; //Task managers MemUsage number
    QuotaPeakPagedPoolUsage : DWORD;
    QuotaPagedPoolUsage : DWORD;
    QuotaPeakNonPagedPoolUsage : DWORD;
    QuotaNonPagedPoolUsage : DWORD;
    PagefileUsage : DWORD; //TaskMan's VM Size number
    PeakPagefileUsage : DWORD;
  end;
  TProcessMemoryCounters = PROCESS_MEMORY_COUNTERS;
//... end process memory stuff

  procedure RandomizeArray(FillMe: TDynamicIntegerArray); overload;
  procedure RandomizeArray(var FillMe: array of Int64); overload;
  procedure RandomizeArray(var FillMe: array of Double); overload;
  procedure RandomizeArray(var FillMe: array of Extended); overload;
  procedure StartProgram(const ProgramName: String;
                         const Parameters: String = '';
                         const WorkDir: String = '';
                         const ShowCmd: Integer = SW_SHOWNORMAL);
  function StartProgramWait(asProgramName  : String;
                             awVisible     : Word ):integer;
  function  GetCycleCount:  Int64;
  function  Fibonacci( n : integer ) : integer;
  procedure SaveStringListToFile(aStrings : TStrings;
                               asFileName : string;
                               abAppend : Boolean = FALSE;
                               abOverWrite : Boolean = FALSE);
  procedure SaveStringListViaFileBrowser(aStrings : TStrings;
                             asDefaultFileName : string = 'DefaultFileName.txt';
                             abAppend : Boolean = FALSE);
  procedure KillProcess( ahwndKillMe : Hwnd );
  procedure ShowTaskBar(abShow: Boolean);
  procedure DisableScreenSaver;
  procedure EnableScreenSaver;
  procedure SystemKeys(aDisable: Boolean);
  function GetScreenRes: string;
  function ExtractBitValue( aDoubleWord : DWord;
                            aStartBit, aEndBit : integer ) : cardinal;
  function ExtractBitValue64( aInt64 : Int64;
                            aStartBit, aEndBit : bits ) : Int64;
  function BooleanToYesNo( aBoolean : Boolean): string;
  function BooleanToTrueFalse( aBoolean : Boolean): string;
  function IsByteInDWord( aByte : Byte;
                          aDWord : DWord;
                          aiSkip : integer = 0 ): Boolean;
  function GetPhysicalMemoryInBytes: int64;
  function GetPhysicalVirtualMemoryInBytes: int64;
  function GetAvailableMemoryInBytes: int64;
  function GetMemoryLoad: integer;
  procedure SetWinMHz(Value : integer);
  procedure RunProcessIdleTasks;
  procedure RunDiskDefrag( acDriveLetter : char; aiNumberOfTimes : integer );
  function GetDriveFreeSpace( acDriveLetter : char ): Int64;
  function DriveIsBigEnough( acDriveLetter : char; ai64SizeNeeded : int64 ): Boolean;
  function DriveNbr( acDriveLetter : char ): integer;
  function ReadEnvironmentVariable( asEnvironmentVariableName : string ): string;
  function GetNumberOfProcessors: integer;
  procedure FadeDown( aFormToFade : TForm; aiFinalAlphaValue, aiStepSize : integer );
  procedure UnFade( aFormToFade : TForm; aiFinalAlphaValue, aiStepSize : integer );
  procedure FadeFast50( aFormToFade : TForm );
  procedure FadeFast100( aFormToFade : TForm );
  procedure UnFadeFast( aFormToFade : TForm );
  function IsBatteryAvailable: Boolean;
  function IsOnBatteryPower: Boolean;
  function GetBatteryLevel: Integer;
  procedure ShutdownWindows;
  procedure Hibernate( Handle : word );
  // process code take from:
  // http://www.thedelphimagazine.com/samples/1328/1328.htm
  function GetProcessMemoryInfo(Process : THandle;
   var MemoryCounters : TProcessMemoryCounters;
   cb : DWORD) : BOOL; stdcall;
  function ProcessMemoryUsage(ProcessID : DWORD): DWORD;
  // end process code
  function RandomCentaurInteger( aiEDX :integer ): integer;
  function ComplexNumberModulus( aComplexNumber : TComplexNumber ): double;
  function ComplexNumberAdd( aComplexNumber1,
                           aComplexNumber2 : TComplexNumber ): TComplexNumber;
  function ComplexNumberSquare( aComplexNumber : TComplexNumber ): TComplexNumber;
  procedure ChangeSize(Handle: Hwnd; dHeight, dWidth: Integer);
  // ExitWindows taken from http://www.swissdelphicenter.ch/en/showcode.php?id=168:
  function ExitWindows(RebootParam: Longword): Boolean;
  function IsGlobalOffline: Boolean;
  procedure SetGlobalOffline(abGoOffline: Boolean);
  function GetTempDirectory: string;
  procedure MemoMoveToLine(aMemo: TMemo; LineNo: integer);
  function InsertBitValue64( aInt64Target, aInt64Value : Int64;
                             aStartBit, aEndBit : bits ) : Int64;

const
  // MSR
  SPEEDSTEP_CURRENT_PSTATE_MSR = $198;
  SPPEDSTEP_SET_PSTATE_MSR = $199;
  // end MSR
  KEY_ESC       = #27;
  CR_LF         = #13 + #10;
  CRLF          = CR_LF;
  TAB           = #9;
  ONE_BILLION   = 1000000000;
  ONE_KB        = 1024;
  ONE_MB        = ONE_KB * ONE_KB;
  ONE_MEGABYTE  = ONE_MB;
  ONE_KILOBYTE  = ONE_KB;
  ONE_MILLION   = 1000000;
  PI = 3.141592653589793238462643383279502884197;

implementation

const

  PROCESS_KILL = $0001;
  REG_KEY_PROCESSOR   = 'HARDWARE\DESCRIPTION\System\CentralProcessor\0';
  REG_VALUE_CPU_NAME  = 'ProcessorNameString';
  REG_VALUE_VENDOR_ID = 'VendorIdentifier';
  REG_VALUE_CORE_ID   = 'Identifier';
  REG_VALUE_MHZ       = '~Mhz';

  procedure MemoMoveToLine(aMemo: TMemo; LineNo: integer);
  begin
      with aMemo do
      begin
          SelStart := Perform(EM_LINEINDEX, LineNo, 0);
          Perform(EM_SCROLLCARET, LineNo, 0);
      end;
  end;

  procedure KillProcess( ahwndKillMe : Hwnd );
  var
     lcPID: cardinal;
     lhandleProcess: THandle;
  begin
    GetWindowThreadProcessId( ahwndKillMe, @lcPID );
    lhandleProcess := OpenProcess( PROCESS_KILL, FALSE, lcPID );
    TerminateProcess( lhandleProcess, 4 );
  end; // procedure KillProcess

  {
   Supply a fully qualified path name in ProgramName
   and any arguments on the command line. As the help file
   states: "If lpApplicationName is NULL, the first white space-delimited
   token of the command line specifies the module name..." In English,
   the characters before the first space encountered (or if no space is
   encountered as in a single program call) is interpreted as the
   EXE to execute. The rest of the string is the argument line.
   Note that awVisible should be one of the SW constants like SW_HIDE,
   SW_MAXIMIZE, SW_MINIMIZE, SW_RESTORE, SW_SHOW, SW_SHOWDEFAULT, etc.
   Derived from: http://www.delphicorner.f9.co.uk/articles/wapi4.htm
   also see:
     ShellExecute(Handle, 'Open', 'notepad', nil, nil, SW_SHOW);
     WinExec( ls_RunProgram SW_HIDE);
     ShellExecute(Handle, 'Open', 'ChgRatio.exe', PAnsiChar(edClockRatio.Text), nil, SW_HIDE);
     CreateProcess(PAnsiChar(ls_RunProgram),
   }
  function StartProgramWait(asProgramName  : String;
                             awVisible      : Word) : integer;
  var
    StartInfo  : TStartupInfo;
    ProcInfo   : TProcessInformation;
    CreateOK   : Boolean;
    lcExitCode : Cardinal;
  begin

    { fill with known state }
    FillChar(StartInfo,SizeOf(TStartupInfo),#0);
    FillChar(ProcInfo,SizeOf(TProcessInformation),#0);

    with StartInfo do begin
      cb := SizeOf(TStartupInfo);
      lpReserved := nil;
      lpDesktop := nil;
      lpTitle := nil;
      dwFlags := STARTF_USESHOWWINDOW;
      wShowWindow := awVisible;
      cbReserved2 := 0;
      lpReserved2 := nil;
    end;

    CreateOK := CreateProcess(nil, PChar(asProgramName), nil, nil,False,
              CREATE_NEW_PROCESS_GROUP+NORMAL_PRIORITY_CLASS,
              nil, nil, StartInfo, ProcInfo);

    { check to see if successful }
    if CreateOK then begin
      //may or may not be needed. Usually wait for child processes
      WaitForSingleObject(ProcInfo.hProcess, INFINITE);
      GetExitCodeProcess(ProcInfo.hProcess, lcExitCode);
      result := lcExitCode;
      CloseHandle(ProcInfo.hProcess);
    end else begin
      result := 2;
      raise Exception.CreateFmt('Error starting program ' + asProgramName +
            '.Error Code %d', [GetLastError]);
    end;
  end; // StartProgramWait

  // from http://www.alistairkeys.co.uk/hints.shtml
  procedure StartProgram(const ProgramName: String;
                         const Parameters: String = '';
                         const WorkDir: String = '';
                         const ShowCmd: Integer = SW_SHOWNORMAL);
  begin
    ShellExecute(0, 'open', PChar(ProgramName), PChar(Parameters), PChar(WorkDir), ShowCmd);
  end; // procedure StartProgram

  // only works on WinXP and above
  procedure RunProcessIdleTasks;
  var
    lsCommandString : string;
  begin
    lsCommandString := 'rundll32.exe advapi32.dll,ProcessIdleTasks';
    StartProgramWait( lsCommandString, SW_MAXIMIZE );
  end; // procedure RunProcessIdleTasks;

  // only works on WinXP and above
  procedure RunDiskDefrag( acDriveLetter : char; aiNumberOfTimes : integer );
  var
    lsCommandString : string;
    i : integer;
  begin
    if acDriveLetter = '' then acDriveLetter := 'c';
    if aiNumberOfTimes < 1 then aiNumberOfTimes := 1;
    if aiNumberOfTimes > 1000 then aiNumberOfTimes := 1000; // set limit to 1000 defrags;
    lsCommandString := 'defrag ' + acDriveLetter + ': -v';
    for i := 1 to aiNumberOfTimes do begin
      StartProgramWait( lsCommandString, SW_SHOWNORMAL );
    end; // for
  end; // procedure RunDiskDefrag

  function DriveNbr( acDriveLetter : char ): integer;
  var
    lsDrive : string;
  begin
    lsDrive := UpperCase( acDriveLetter );
    acDriveLetter := lsDrive[1];
    result := ord( acDriveLetter ) - 64;
  end; //function DriveNbr

  function GetDriveFreeSpace( acDriveLetter : char ): Int64;
  var
    liDriveNbr : integer;
  begin
    result := -1;
    liDriveNbr := DriveNbr( acDriveLetter );
    result := DiskFree( liDriveNbr );
  end; // function GetDriveFreeSpace

  function DriveIsBigEnough( acDriveLetter : char; ai64SizeNeeded : int64 ): Boolean;
  begin
    result := FALSE;
    if GetDriveFreeSpace( acDriveLetter ) > ai64SizeNeeded then result := TRUE;
  end; // function IsDriveBigEnough( acDriveLetter : char; ai64SizeNeeded : int64 );

  function Fibonacci( n : integer ) : integer;
  begin
    if n > 2 then
      result := Fibonacci( n - 1 ) + Fibonacci( n - 2 )
    else
      result := 1;
  end;

  function GetCycleCount:  Int64;
  asm
    RDTSC;
  end;

  procedure RandomizeArray(FillMe: TDynamicIntegerArray); overload;
  var
    I: Integer;
    li_ArraySize : integer;
  begin
//    RandSeed := 1;
    li_ArraySize := High(FillMe);
    for I := Low(FillMe) to li_ArraySize do
    begin
      FillMe[I] := Random(li_ArraySize);
    end;
  end;

  procedure RandomizeArray(var FillMe: array of Int64); overload;
  var
    I: Integer;
    li_ArraySize : integer;
  begin
//    RandSeed := 1;
    li_ArraySize := High(FillMe);
    for I := Low(FillMe) to li_ArraySize do
    begin
      FillMe[I] := Random(li_ArraySize);
    end;
  end;

  procedure RandomizeArray(var FillMe: array of Double); overload;
  var
    I: Integer;
    li_ArraySize : integer;
  begin
//    RandSeed := 1;
    li_ArraySize := High(FillMe);
    for I := Low(FillMe) to li_ArraySize do
    begin
      FillMe[I] := Random(li_ArraySize * 777) / PI;
    end;
  end;

  procedure RandomizeArray(var FillMe: array of Extended); overload;
  var
    I: Integer;
    li_ArraySize : integer;
  begin
//    RandSeed := 1;
    li_ArraySize := High(FillMe);
    for I := Low(FillMe) to li_ArraySize do
    begin
      FillMe[I] := Random(li_ArraySize);
    end;
  end;

procedure SaveStringListViaFileBrowser(aStrings : TStrings;
                             asDefaultFileName : string = 'DefaultFileName.txt';
                             abAppend : Boolean = FALSE);
var
  lSaveDialog : TSaveDialog;
const
  FILE_SUFFIX = '.txt';
begin
  // set default filename:
  lSaveDialog := TSaveDialog.Create(Application);
  with lSaveDialog do try
    FileName := asDefaultFileName;
    //execute the save dialog:
    if Execute then begin
      asDefaultFileName := FileName;
      if Pos('.', asDefaultFileName) = 0 then begin
        asDefaultFileName := asDefaultFileName + FILE_SUFFIX;
      end; // if
      SaveStringListToFile(aStrings, asDefaultFileName, abAppend);
    end; // if
  finally
    FreeAndNil(lSaveDialog);
  end;
end; // procedure SaveStringListDataBrowse

procedure SaveStringListToFile(aStrings : TStrings;
                                   asFileName : string;
                                   abAppend : Boolean = FALSE;
                                   abOverWrite : Boolean = FALSE);
var
  ltxtfile_out  : TextFile;
  li_YesNo      : integer;
  ls_Msg        : string;
begin
  if asFileName = '' then begin
    ShowMessage('"' + asFileName + '" is not a valid file name.');
    exit;
  end; // if
  if FileExists(asFileName) then begin
    // if we are intending to append to a file then we are good to go, but
    // if the file exists, and we are not trying to append to a file then we
    // need to warn the user that the old file will be erased if they continue:
    if (not abAppend) AND (not abOverWrite) then begin
      ls_Msg := 'A file already exists with the name: "' + asFileName
        + '".' + CR_LF + 'Save anyway (WARNING: old file will be overwritten)?';
      li_YesNo := Application.MessageBox(PWideChar(ls_Msg),
        'File already exists!', MB_YESNO);
      // exit procedure if the answer is "no":
      if li_YesNo = IDNO then exit;
    end; // if not
  end else begin // file does not exist
    // if the file does not exist and we are supposed to append to it, then we
    // need to warn the user that a new file will have to be created:
    if abAppend then begin
      ls_Msg := 'I cannot append to a file named: "' + asFileName
        + '" because the file does not exist.' + CR_LF
        + 'Do you want me to create a new file and save anyway?';
      li_YesNo := Application.MessageBox(PWideChar(ls_Msg),
        'File does not exist for append!', MB_YESNO);
      // exit procedure if the answer is "no":
      if li_YesNo = IDNO then exit;
      // turn off append in order to create a new file:
      abAppend := FALSE;
    end; // if abAppend
  end; // if  FileExists(asFileName)
  AssignFile( ltxtfile_out, asFileName );
  if abAppend then begin
    Append( ltxtfile_out );
  end else begin
    Rewrite( ltxtfile_out );
  end; // if
  Write( ltxtfile_out, aStrings.Text);
  Flush( ltxtfile_out );
  CloseFile( ltxtfile_out );
end; // procedure SaveStringListDataToFile

procedure DisableScreenSaver;
begin
  SystemParametersInfo(SPI_SETSCREENSAVEACTIVE, 0, nil, 0);
end; // DisableScreenSaver

procedure EnableScreenSaver;
begin
  SystemParametersInfo(SPI_SETSCREENSAVEACTIVE, 1, nil, 0);
end; // EnableScreenSaver

function GetScreenRes: string;
var
  liWidth, liHeight, liColorDepth, liVerticalRefreshRate : integer;
begin
  liWidth := Screen.Width;
  liHeight := Screen.Height;
  liColorDepth := GetDeviceCaps( GetDc( GetDesktopWindow ), BITSPIXEL );
  try
    liVerticalRefreshRate :=
      GetDeviceCaps( GetWindowDC( GetDesktopWindow ), VREFRESH );
  except
    liVerticalRefreshRate := -1;
  end; //try
  result := intToStr( liWidth ) + 'x'
     + intToStr( liHeight ) + 'x'
     + intToStr( liColorDepth ) + '@'
     + intToStr( liVerticalRefreshRate ) + 'Hz';
end; // function GetScreenRes

function ExtractBitValue( aDoubleWord : DWord;
                          aStartBit, aEndBit : integer ) : cardinal;
var
  i, liMask : cardinal;
begin
  if ( aStartBit < 0 ) or ( aStartBit > aEndBit ) or ( aEndBit > 31 ) then begin
   raise exception.Create(
     'ExtractBitValue: You passed me invalid start, stop bits!');
  end; // if
  liMask := 0;
  for i := aStartBit to aEndBit do begin
    liMask := liMask + round( power( 2, i ) );
  end; // for
  Result := aDoubleWord and liMask;
  Result := Result shr aStartBit;
end; // function

function ExtractBitValue64( aInt64 : Int64;
                            aStartBit, aEndBit : bits ) : Int64;
var
  lbits : bits;
  l64Mask : T64bitRegister;
begin
  if ( aStartBit > aEndBit ) then begin
   raise exception.Create(
     'ExtractBitValue64: You passed me invalid start, stop bits!');
  end; // if
  l64Mask.AsInt64 := 0;
  for lbits := aStartBit to aEndBit do begin
    l64Mask.bits64 := l64Mask.bits64 + [lbits];
  end; // for
  Result := aInt64 and l64Mask.AsInt64;
  Result := Result shr integer( aStartBit );
end; // function

function InsertBitValue64( aInt64Target, aInt64Value : Int64;
                           aStartBit, aEndBit : bits ) : Int64;
var
  lbits : bits;
  lMaskInvert : T64bitRegister;
begin
  if ( aStartBit > aEndBit ) then begin
   raise exception.Create(
     'InsertBitValue64: You passed me invalid start, stop bits!');
  end; // if
  lMaskInvert.AsInt64 := 0;
  for lbits := bit0 to bit63 do begin
    if ( lbits < aStartBit ) or ( lbits > aEndBit ) then begin
      lMaskInvert.bits64 := lMaskInvert.bits64 + [lbits];
    end;
  end;
  // cut a hole for the bits:
  aInt64Target := aInt64Target and lMaskInvert.AsInt64;
  // now shift the value to the correct bit position:
  aInt64Value := aInt64Value shl integer( aStartBit );
  // now insert the value:
  result := aInt64Target or aInt64Value;
end; // function

function BooleanToYesNo( aBoolean : Boolean): string;
begin
  if aBoolean then result := 'Yes' else result := 'No';
end; // function BooleanToYesNo( aBoolean : Boolean): string;

function BooleanToTrueFalse( aBoolean : Boolean): string;
begin
  if aBoolean then result := 'True' else result := 'False';
end; // function BooleanToTrueFalse( aBoolean : Boolean): string;

function IsByteInDWord( aByte : Byte;
                             aDWord : Dword;
                             aiSkip : integer = 0 ): Boolean;
var
  i, liStartBit, liEndBit : integer;
begin
  result := FALSE;
  for i := 1 to 4 do begin
    if i > aiSkip then begin
      liStartBit := ( i - 1 ) * 8;
      liEndBit := ( i * 8 ) - 1;
      if ExtractBitValue( aDWord, liStartBit, liEndBit) = aByte then begin
        result := TRUE;
        exit;
      end; // if
    end; // if
  end; // for
end; // function LookForByteInDWord

// diables system keys if aDisable = TRUE
procedure SystemKeys(aDisable: Boolean);
 var OldVal : LongInt;
begin
 SystemParametersInfo(SPI_SCREENSAVERRUNNING, Word(aDisable), @OldVal, 0);
end; // procedure SystemKeys(Disable: Boolean);

// process code take from:
// http://www.thedelphimagazine.com/samples/1328/1328.htm
function GetProcessMemoryInfo; external 'psapi.dll';
function ProcessMemoryUsage(ProcessID : DWORD): DWORD;
var
  ProcessHandle : THandle;
  MemCounters   : TProcessMemoryCounters;
begin
  Result := 0;
  ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or
    PROCESS_VM_READ, false, ProcessID );
  try
    if GetProcessMemoryInfo( ProcessHandle, MemCounters,
      sizeof(MemCounters))  then
      Result := MemCounters.WorkingSetSize;
  finally
    CloseHandle( ProcessHandle );
  end;
end; // function ProcessMemoryUsage(ProcessID : DWORD): DWORD;

function GetPhysicalMemoryInBytes: int64;
var
  lMemoryStatus: TMemoryStatus;
begin
  GlobalMemoryStatus(lMemoryStatus);
  result := lMemoryStatus.dwTotalPhys;
end; // function GetPhysicalMemoryInBytes: int64;

function GetPhysicalVirtualMemoryInBytes: int64;
var
  lMemoryStatus: TMemoryStatus;
begin
  GlobalMemoryStatus(lMemoryStatus);
  result := lMemoryStatus.dwTotalVirtual;
end; // function GetPhysicalVirtualMemoryInBytes: int64;

function GetAvailableMemoryInBytes: int64;
var
  lMemoryStatus: TMemoryStatus;
begin
  GlobalMemoryStatus(lMemoryStatus);
  result := lMemoryStatus.dwAvailPhys;
end; // function GetAvailableMemoryInBytes: int64;

function GetMemoryLoad: integer;
var
  lMemoryStatus: TMemoryStatus;
begin
  GlobalMemoryStatus(lMemoryStatus);
  result := lMemoryStatus.dwMemoryLoad;
end; // function GetAvailableMemoryInBytes: int64;

// show / hide taskbar:
procedure ShowTaskBar(abShow: Boolean);
begin
  if abShow then begin
   ShowWindow(FindWindow('Shell_TrayWnd',nil), SW_SHOWNA)
  end else begin
   ShowWindow(FindWindow('Shell_TrayWnd',nil), SW_HIDE);
  end; // if
end; // procedure ShowTaskBar(abShow: Boolean);

procedure SetWinMHz(Value : integer);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    with Reg do begin
      try
        RootKey := HKEY_LOCAL_MACHINE;
        OpenKey( REG_KEY_PROCESSOR, TRUE);
        WriteInteger(REG_VALUE_MHZ, Value);
        CloseKey;
      except
        ShowMessage( 'SetWinMHz: Failed to set ~MHz!' );
      end; // try..except
    end; // with
  finally
    Reg.Free;
  end;
end; // procedure THardwareInfo.SetWinMHz(Value : integer);

function RandomCentaurInteger( aiEDX :integer ): integer;
var
  lipResultPointer : ^integer;
begin
  lipResultPointer := @result;
  asm
    mov edi, lipResultPointer // move address of result variable into edi
    mov edx, aiEDX  // random bit rate (accumulate every 2^n bits)
    mov ecx, 4  // number of bytes that we need
    db  $f3,$0f,$a7,$c0  // REPXSTORE
  end;
end;// procedure RandomCentaurInteger;

function ReadEnvironmentVariable( asEnvironmentVariableName : string ): string;
var
  liBufferSize : integer;
begin
  // Get required buffer size (inc. terminal #0)
  liBufferSize := GetEnvironmentVariable(
    PChar( asEnvironmentVariableName ), nil, 0 );
  if liBufferSize > 0 then begin
    // Read env var value into result string
    SetLength( result, liBufferSize - 1 );
    GetEnvironmentVariable( PChar( asEnvironmentVariableName ),
                            PChar( result ),
                            liBufferSize );
  end
  else begin
    // No such environment variable
    result := '';
  end;
end; // function ReadEnvironmentVariable

function GetNumberOfProcessors: integer;
const
  NUMBER_OF_PROCESSORS = 'NUMBER_OF_PROCESSORS';
var
  lsNumberOfProcessors : string;
begin
  lsNumberOfProcessors := ReadEnvironmentVariable( NUMBER_OF_PROCESSORS );
  if lsNumberOfProcessors = '' then begin
    result := 1;
  end else begin
    result := StrToInt( lsNumberOfProcessors );
  end; // if
end; // function GetNumberOfProcessors: integer;

function ComplexNumberModulus( aComplexNumber : TComplexNumber ): double;
begin
  result := sqrt( sqr(aComplexNumber.Real) + sqr(aComplexNumber.Imaginary) );
end; //

function ComplexNumberAdd( aComplexNumber1,
                           aComplexNumber2 : TComplexNumber ): TComplexNumber;
begin
  result.Real := aComplexNumber1.Real + aComplexNumber2.Real;
  result.Imaginary := aComplexNumber1.Imaginary + aComplexNumber2.Imaginary;
end;

function ComplexNumberSquare( aComplexNumber : TComplexNumber ): TComplexNumber;
begin
  result.Real := sqr( aComplexNumber.Real ) - sqr( aComplexNumber.Imaginary );
  result.Imaginary := 2 * aComplexNumber.Real * aComplexNumber.Imaginary;
end;

procedure FadeDown( aFormToFade : TForm; aiFinalAlphaValue, aiStepSize : integer );
var
  i : integer;
begin
  if aiFinalAlphaValue > 255 then exit;
  if aiFinalAlphaValue < 0 then aiFinalAlphaValue := 0;
  if not aFormToFade.AlphaBlend then begin
    aFormToFade.AlphaBlendValue := 255;
    aFormToFade.AlphaBlend := TRUE;
    application.ProcessMessages;
  end;
  i := 255;
  repeat
    i := i - aiStepSize;
    if i < aiFinalAlphaValue then i := aiFinalAlphaValue;
    aFormToFade.AlphaBlendValue := i;
    application.ProcessMessages;
  until i <= aiFinalAlphaValue; // for
end; // FadeDown

procedure UnFade( aFormToFade : TForm; aiFinalAlphaValue, aiStepSize : integer );
var
  i, startValue : integer;
begin
  startValue := aFormToFade.AlphaBlendValue;
  if startValue >= aiFinalAlphaValue then exit;
  if aiFinalAlphaValue > 255 then aiFinalAlphaValue := 255;
  i := startValue;
  repeat
    i := i + aiStepSize;
    if i > aiFinalAlphaValue then i := aiFinalAlphaValue;
    aFormToFade.AlphaBlendValue := i;
    application.ProcessMessages;
  until i >= aiFinalAlphaValue; // for
end; // UnFade

procedure FadeFast50( aFormToFade : TForm );
begin
  FadeDown( aFormToFade, 128, 32 );
end; // FadeFast50

procedure FadeFast100( aFormToFade : TForm );
var
  i : integer;
begin
  FadeDown( aFormToFade, 0, 32 );
end; // FadeFast100

procedure UnFadeFast( aFormToFade : TForm );
begin
  UnFade( aFormToFade, 255, 32 );
end; // UnFadeFast

function IsBatteryAvailable: Boolean;
var
  lSystemPowerStatus: TSystemPowerStatus;
begin
  GetSystemPowerStatus( lSystemPowerStatus );
  // BatteryFlag:
  //    1 = High power
  //    2 = Low power
  //    4 = Critical
  //    8 = Charging
  //    128 = No status available
  //    256 = Unknown status
  result := (lSystemPowerStatus.BatteryFlag  < 128);
end;

function IsOnBatteryPower: Boolean;
var
  lSystemPowerStatus: TSystemPowerStatus;
begin
  result := FALSE;
  if IsBatteryAvailable then begin
    GetSystemPowerStatus( lSystemPowerStatus );
    result := (lSystemPowerStatus.ACLineStatus  <> 1);
  end;
end; // function IsOnBatteryPower: Boolean;

function GetBatteryLevel: Integer;
var
  lSystemPowerStatus: TSystemPowerStatus;
begin
  GetSystemPowerStatus( lSystemPowerStatus );
  result := lSystemPowerStatus.BatteryLifePercent;
  if (result < 0) or (result > 100) then result := -1;
end; // function GetBatteryLevel: Integer;

procedure ShutdownWindows;
begin
//  ExitWindowsEx( EWX_SHUTDOWN, 0 );
  ExitWindows(EWX_POWEROFF or EWX_FORCE);
end; // procedure ShutdownWindows;

{
  Taken from: http://www.swissdelphicenter.ch/torry/showcode.php?id=331
  The function ChangeSize does the following:

  SomeWindow.Height = SomeWindow.Height + dHeight
  (where dHeight is change in Height)
  SomeWindow.Width = SomeWindow.Width + dWidth

  ChangeSize doesn't change the top/left of the specific window.

  Die Funktion ChangeSize macht folgendes:

  EinFenster.Height = EinFenster.Height + dHeight
  (Wobei dHeight die den Höhenunterschied angibt)
  EinFenster.Width = EinFenster.Width + dWidth

  Die Funktion ChangeSize ändert Top/Left des Fensters nicht.
}

procedure ChangeSize(Handle: Hwnd; dHeight, dWidth: Integer);
var
  P: TRect;
  DlgWidth: integer;
  DlgHeight: integer;
begin
  GetWindowRect(Handle, P);
  DlgWidth  := P.Right - P.Left;
  DlgHeight := P.Bottom - P.Top;
  MoveWindow(Handle, P.Left, P.Top, DlgWidth + dHeight, DlgHeight + dWidth, True);
end;

{ Taken from http://www.swissdelphicenter.ch/en/showcode.php?id=168
Example to logoff Windows:
 ExitWindows( EWX_LOGOFF or EWX_FORCE );
}
function ExitWindows(RebootParam: Longword): Boolean;
var
  TTokenHd: THandle;
  TTokenPvg: TTokenPrivileges;
  cbtpPrevious: DWORD;
  rTTokenPvg: TTokenPrivileges;
  pcbtpPreviousRequired: DWORD;
  tpResult: Boolean;
const
  SE_SHUTDOWN_NAME = 'SeShutdownPrivilege';
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    tpResult := OpenProcessToken(GetCurrentProcess(),
      TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,
      TTokenHd);
    if tpResult then
    begin
      tpResult := LookupPrivilegeValue(nil,
                                       SE_SHUTDOWN_NAME,
                                       TTokenPvg.Privileges[0].Luid);
      TTokenPvg.PrivilegeCount := 1;
      TTokenPvg.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
      cbtpPrevious := SizeOf(rTTokenPvg);
      pcbtpPreviousRequired := 0;
      if tpResult then
        Windows.AdjustTokenPrivileges(TTokenHd,
                                      False,
                                      TTokenPvg,
                                      cbtpPrevious,
                                      rTTokenPvg,
                                      pcbtpPreviousRequired);
    end;
  end;
  Result := ExitWindowsEx(RebootParam, 0);
end;

procedure Hibernate( Handle : word );
begin
 ShellExecute( Handle, 'open', 'rundll32.exe',
  PChar( 'Powrprof.dll,SetSuspendState' ), nil, SW_SHOWNORMAL);
end;

// IsGlobalOffline, SetGlobalOffline from:
// http://www.swissdelphicenter.ch/torry/showcode.php?id=905
function IsGlobalOffline: Boolean;
var
  ldwState: dword;
  ldwSize: dword;
begin
  result    := FALSE;
  ldwState  := 0;
  ldwSize   := SizeOf(dword);
  result    := (InternetQueryOption(nil, INTERNET_OPTION_CONNECTED_STATE, @ldwState, ldwSize))
            and
            ( (ldwState and INTERNET_STATE_DISCONNECTED_BY_USER) <> 0 );
end;

//Set offline state
procedure SetGlobalOffline(abGoOffline: Boolean);
var
  ci: INTERNET_CONNECTED_INFO;
begin
  if abGoOffline then begin
    ci.dwConnectedState := INTERNET_STATE_DISCONNECTED_BY_USER;
    ci.dwFlags          := ISO_FORCE_DISCONNECTED;
  end else begin
    ci.dwConnectedState := INTERNET_STATE_CONNECTED;
    ci.dwFlags := 0;
  end; // if...else
  InternetSetOption(nil, INTERNET_OPTION_CONNECTED_STATE, @ci, SizeOf(ci));
end;

// MSR Starts===============================
//function BackdoorOpen : Boolean; stdcall; external 'msr.dll';
//function BackdoorClose : Boolean; stdcall; external 'msr.dll';
//function BackdoorReadMsr (cpu : integer;
//                          index : integer;
//                          flags : integer;
//                          var low : LongWord;
//                          var high : LongWord
//                          ) : integer; stdcall; external 'msr.dll';
//function BackdoorWriteMsr(cpu   : integer;
//                          index : integer;
//                          flags : integer;
//                          low   : LongWord;
//                          high  : LongWord
//                          ) : integer; stdcall; external 'msr.dll';
//function BackdoorRmwMsr(cpu   : integer;
//                        index : integer;
//                        flags : integer;
//                        andlo : LongWord;
//                        andhi : LongWord;
//                        orlo  : LongWord;
//                        orhi  : LongWord;
//                        var prevlo : LongWord;
//                        var prevhi : LongWord
//                        ) : integer; stdcall; external 'msr.dll';
//function BackdoorNumberOfCpus: integer; stdcall; external 'msr.dll';
//procedure BackdoorWbInvd(cpu : integer); stdcall; external 'msr.dll';
//procedure BackdoorCache(cpu : integer;
//                        state : integer ); stdcall; external 'msr.dll';
//
//function TMsrDriver.OpenDriver: Boolean;
//begin
//  result := BackdoorOpen;
//end;
//
//function TMsrDriver.CloseDriver: Boolean;
//begin
//  result := BackdoorClose;
//end;
//
//function TMsrDriver.GetNumberOfCpus: integer;
//begin
//  result := BackdoorNumberOfCpus;
//end;
//
//function TMsrDriver.ReadMsr(cpu, msr: integer): T64bitRegister;
//var
//  ReturnCode : integer;
//begin
//  result.AsInt64 := -1;
//  ReturnCode := BackdoorReadMsr(cpu, msr, 0, result.LowWord, result.HighWord);
//end;
//
//procedure TMsrDriver.SetUnsetMsrBits(cpu, msr: integer;
//                                     BitsToSet, BitsToUnset : T64bits);
//var
//  MsrRegisterValue : T64bitRegister;
//  ReturnCode : integer;
//begin
//  ReturnCode := BackdoorReadMsr(cpu, msr, 0, MsrRegisterValue.LowWord, MsrRegisterValue.HighWord);
//  MsrRegisterValue.bits64 := MsrRegisterValue.bits64 + BitsToSet - BitsToUnset;
//  ReturnCode := BackdoorWriteMsr(cpu, msr, 0, MsrRegisterValue.LowWord, MsrRegisterValue.HighWord);
//end;
//
//procedure TMsrDriver.WriteBackInvalidate( cpu : integer );
//begin
//  BackdoorWbInvd( cpu );
//end;
//
//function TMsrDriver.WriteMsr(cpu, msr: integer;
//                             Msr64bitRegister : T64bitRegister
//                             ): T64bitRegister;
//var
//  ReturnCode : integer;
//begin
//  result.AsInt64 := -1;
//  ReturnCode := BackdoorWriteMsr(cpu, msr, 0, Msr64bitRegister.LowWord, Msr64bitRegister.HighWord);
//  result := ReadMsr( cpu, msr );
//end;
//
//procedure TMsrDriver.ToggleMsrBits(cpu, msr : integer;
//                                   BitsToToggle : T64bitRegister);
//var
//  MsrRegisterValue : T64bitRegister;
//  ReturnCode : integer;
//begin
//  ReturnCode := BackdoorReadMsr(cpu, msr, 0, MsrRegisterValue.LowWord, MsrRegisterValue.HighWord);
//  MsrRegisterValue.AsInt64 := MsrRegisterValue.AsInt64 xor BitsToToggle.AsInt64;
//  ReturnCode := BackdoorWriteMsr(cpu, msr, 0, MsrRegisterValue.LowWord, MsrRegisterValue.HighWord);
//end;
//
//procedure TMsrDriver.CachesOff(cpu: integer);
//begin
//  BackdoorCache( cpu, 0 );
//end;
//
//procedure TMsrDriver.CachesOn(cpu: integer);
//begin
//  BackdoorCache( cpu, 1 );
//end;
//
//function TMsrDriver.GetSpeedStepState( cpu : integer ) : TSpeedStepState;
//var
//  MsrRegisterValue : T64bitRegister;
//  ReturnCode : integer;
//begin
//  ReturnCode := BackdoorReadMsr(cpu, $0198, 0, MsrRegisterValue.LowWord, MsrRegisterValue.HighWord);
//  result.FsbMultiplier := ExtractBitValue( MsrRegisterValue.LowWord, 8, 15);
//  result.Vcc := 0.7 + ExtractBitValue( MsrRegisterValue.LowWord, 0, 7) * 0.016;
//end;
//
//procedure TMsrDriver.SetSpeedStepState(cpu : integer; NewState: TSpeedStepState);
//var
//  MsrRegisterValue : T64bitRegister;
//  liVccEncoding, liFsbEncoding : integer;
//begin
//  liVccEncoding := EncodeSpeedStepVcc(NewState.Vcc);
//  liFsbEncoding := (NewState.FsbMultiplier shl 8);
//  MsrRegisterValue.AsInt64 := liFsbEncoding + liVccEncoding;
//  showmessage( 'Vcc: ' + IntToHex( liVccEncoding, 4 ) + ' FSB : ' + IntToHex( liFsbEncoding, 4 ) + ' msr : ' + intToHex( MsrRegisterValue.AsInt64, 8 ) );
//end;
//
//function TMsrDriver.GetMinMaxSpeedStepStates(
//  cpu: integer): TMinMaxSpeedStepStates;
//var
//  MsrRegisterValue : T64bitRegister;
//  ReturnCode : integer;
//begin
//  ReturnCode := BackdoorReadMsr(cpu, $0198, 0, MsrRegisterValue.LowWord, MsrRegisterValue.HighWord);
//  result.MinState.FsbMultiplier := ExtractBitValue( MsrRegisterValue.HighWord, 24, 31);
//  result.MinState.Vcc := DecodeSpeedStepVcc( ExtractBitValue( MsrRegisterValue.HighWord, 16, 23) );
//  result.MaxState.FsbMultiplier := ExtractBitValue( MsrRegisterValue.HighWord, 8, 15);
//  result.MaxState.Vcc := DecodeSpeedStepVcc( ExtractBitValue( MsrRegisterValue.HighWord, 0, 7) );
//end;
//
//function TMsrDriver.DecodeSpeedStepVcc( EncodedVcc : integer ): double;
//begin
//  result := 0.7 + EncodedVcc * 0.016;
//end;
//
//function TMsrDriver.EncodeSpeedStepVcc( Vcc : double ): integer;
//begin
//  result := Round ((Vcc - 0.7) / 0.016 );
//end;
//
//function TMsrDriver.GetMaxVcc( cpu : integer ): double;
//var
//  lMinMaxSpeedStepStates : TMinMaxSpeedStepStates;
//begin
//  lMinMaxSpeedStepStates := GetMinMaxSpeedStepStates( cpu );
//  result := lMinMaxSpeedStepStates.MaxState.Vcc;
//end; // function GetMaxVcc
//
//function TMsrDriver.GetMaxCpuMultiplier( cpu : integer ): cardinal;
//var
//  lMinMaxSpeedStepStates : TMinMaxSpeedStepStates;
//begin
//  lMinMaxSpeedStepStates := GetMinMaxSpeedStepStates( cpu );
//  result := lMinMaxSpeedStepStates.MaxState.FsbMultiplier;
//end; // function GetMaxVcc
//
//procedure TMsrDriver.SetSpeedStepMultiplier( cpu, aiCpuMultiplier : integer );
//var
//  lSpeedStepState : TSpeedStepState;
//begin
//  if aiCpuMultiplier > GetMaxCpuMultiplier( cpu ) then begin
//    if application.MessageBox('The multiplier that you are setting exceeds the maximum multiplier for this cpu.' + CR_LF
//      + 'Do you want to continue?', 'Multiplier Exceeds Maximum!', MB_YESNO or MB_ICONQUESTION ) = IDNO then begin
//      exit;
//    end; // if
//  end; // if
//  lSpeedStepState := GetSpeedStepState( cpu );
//  lSpeedStepState.FsbMultiplier := aiCpuMultiplier;
//  SetSpeedStepState( cpu, lSpeedStepState );
//end; // procedure TMsrDriver.SetSpeedStepMultiplier( aiCpuMultiplier : integer );

  function GetTempDirectory: string;
  begin
    result := GetEnvironmentVariable( 'TEMP' );
  end;


end.
