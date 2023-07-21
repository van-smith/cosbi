unit uCpuClockSpeed;
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
// Unit name: uCpuClockSpeed
// Unit description: This unit holds classes that obtain the current CPU clock
//                   speed.
// Author: Van Smith
// Date: March 31, 2004
// OS dependent: No. Should compile under Linux.
// Resolution dependent: No.
// External unit dependencies: COSBI_Common, math.
//==============================================================================
// Modification History
// ------------ -------
// Ver  Date   Who    Description
// ==== ====== ====== ==========================================================
// 1.0  060130 Van     Created.
//==============================================================================

interface

uses
  uMsr, COSBI_Common, CosbiCpuid, uStopWatch,
  Classes, SysUtils, math, dialogs, WbemScripting_TLB, ActiveDs_TLB, ActiveX,
  ExtCtrls, forms, windows;

type

  TCpuClockSpeed = class( TComponent )
  const
    FIB_NUMBER = 38;
  type
    enumCpuSpeedCalcType    = (eRDTSC, eCPUID, eWMI, eMSR);
    setCpuSpeedCalcSupport  = set of enumCpuSpeedCalcType;
    enumRunPriority         = (eRPNormal, eRPHigh, eRPRealTime);
    //enumCpuVendor           = (eIntel, eAMD, eVIA, eTransmeta);
  private
    fCpuSpeedCalculationType  : enumCpuSpeedCalcType;
    fCpuidCentaur     : TCpuidCentaur;
    fCpuidCentaurPerformanceInfo : TCpuidCentaurPerformanceInfo;
    fCpuidLevel0      : TCpuidLevel0;
    fCpuidTransmetaPerformanceData : TCpuidTransmetaPerformanceData;
    fCpuSpeedCalculationSupport : setCpuSpeedCalcSupport;
    fCpuTemperatureAvailable : Boolean;
    fCpuVendor        : enumCpuVendor;
    fEaxInput         : cardinal;
    fi64StartTick     : Int64;
    fi64StopTick      : Int64;
    fIsCpuTemperatureAvailable : Boolean;
    fIsC7             : Boolean;
    fIsCn             : Boolean;
    fIsCentaur        : Boolean;
    fIsEfficeon       : Boolean;
    fIsTransmeta      : Boolean;
    fNbrOfThreadsRunning : integer;
    fStopWatch        : TStopWatch;
    ftmrSpeedUpdate   : TTimer;
    fx86Reg           : Tx86_32_GPRegisters;
    fFsbMhz           : integer;
    fMsrDriver        : TMsrDriver;
    fMsrRegister      : T64bitRegister;
    fEnhancedSpeedStep: Boolean;
    // wmi stuff
    fLocator      : TSWbemLocator;
    fSinkClasses  : TSWbemSink;
    fServices     : ISWbemServices;
    fObjectSet    : ISWbemObjectSet;
    fSObject      : ISWbemObject;
    fPropSet      : ISWbemPropertySet;
    fSProp        : ISWbemProperty;
    fPropEnum     : IEnumVariant;
    fEnum         : IEnumVariant;
    ftempObj      : OleVariant;
    fOnTimerUpdate: TNotifyEvent;
    // end wmi
    function GetCpuSpeedCalculationType : enumCpuSpeedCalcType;
    function GetCpuSpeedCalculationSupport: setCpuSpeedCalcSupport;
    function GetFsbMhz : integer;
    procedure SetCpuSpeedCalculationType( Value : enumCpuSpeedCalcType );
    procedure ThreadDone(Sender: TObject);
    procedure SetPriority( aRunPriority : enumRunPriority );
    procedure SetMsrDriver( value : TMsrDriver );
    procedure TimerUpdate( Sender : TObject );
  public
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; Override;
    function GetTransmetaClockSpeed : int64;
    function GetC7ClockSpeed : int64;
    function GetC7Temperature : integer;
    function GetCpuTemperature : integer;
    function DetectCpuVendor : enumCpuVendor;
//    function GetCPUSpeed(var ai64TickCount : int64; var adTime : double ): int64;
    function GetCPUSpeed: int64;
    function GetCpuSpeedByCpuid: int64;
    function GetCPUSpeedByRDTSC: int64;
    function GetCPUSpeedByWmi: int64;
    function GetCpuSpeedByMsr: int64;
    function DetermineMaximumClockSpeed: int64;
    function IsCpuTemperatureAvailable: Boolean;
    procedure LoadDownCpu( n : integer );
    property CpuSpeedCalculationSupport : setCpuSpeedCalcSupport
             read fCpuSpeedCalculationSupport;
    property CpuSpeedCalculationType : enumCpuSpeedCalcType
             read GetCpuSpeedCalculationType
             write SetCpuSpeedCalculationType;
    property CpuTemperatureAvailable : Boolean read fCpuTemperatureAvailable;
    property FsbMhz : integer read GetFsbMhz;
    property OnTimerUpdate: TNotifyEvent read fOnTimerUpdate write fOnTimerUpdate;
    property Timer : TTimer read ftmrSpeedUpdate write ftmrSpeedUpdate;
    property IsC7 : Boolean read fIsC7;
    property IsCn : Boolean read fIsCn;
    property MsrDriver : TMsrDriver read fMsrDriver write SetMsrDriver;
  end; // TCpuid

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

implementation

constructor TCpuClockSpeed.Create( AOwner : TComponent );
var
  lCpuidLevel1 : TCpuidLevel1;
begin
  inherited;
  fCpuidCentaurPerformanceInfo := TCpuidCentaurPerformanceInfo.Create;
  fNbrOfThreadsRunning := 0;
  fi64StartTick    := 0;
  fi64StopTick     := 0;
  fLocator := TSWbemLocator.Create( self );
  fSinkClasses := TSWbemSink.Create( self );
  fServices := fLocator.ConnectServer('', 'root\CIMV2', '', '', '', '', 0, nil);
  fStopWatch := TStopWatch.Create;
  fStopWatch.Resolution := swresHigh;
  ftmrSpeedUpdate := TTimer.Create( self );
  ftmrSpeedUpdate.Enabled := FALSE;
  ftmrSpeedUpdate.OnTimer := TimerUpdate;
  fCpuidLevel0 := TCpuidLevel0.Create;
  fCpuVendor := DetectCpuVendor;
  fCpuSpeedCalculationSupport := GetCpuSpeedCalculationSupport;
  lCpuidLevel1 := TCpuidLevel1.Create;
  try
    fEnhancedSpeedStep := lCpuidLevel1.EnhancedSpeedStep;
  finally
    freeAndNil( lCpuidLevel1 );
  end;
end; // constructor

destructor TCpuClockSpeed.Destroy;
begin
  FreeAndNil( fStopWatch );
  FreeAndNil( fCpuidCentaurPerformanceInfo );
  try
    fLocator.Disconnect;
  except
    ShowMessage( 'Error disconnecting WMI Locator.' );
  end;
  inherited;
end; //destructor TCpuClockSpeed.Destroy;

function TCpuClockSpeed.GetFsbMhz : integer;
begin

end;

function TCpuClockSpeed.GetCpuSpeedCalculationType : enumCpuSpeedCalcType;
begin
  result := fCpuSpeedCalculationType;
end;

procedure TCpuClockSpeed.SetMsrDriver( value : TMsrDriver );
begin
  fMsrDriver := Value;
  fCpuSpeedCalculationSupport := GetCpuSpeedCalculationSupport;
end;

procedure TCpuClockSpeed.SetCpuSpeedCalculationType( Value : enumCpuSpeedCalcType );
begin
  if Value in fCpuSpeedCalculationSupport then begin
    fCpuSpeedCalculationType := Value
  end else begin
    raise exception.Create('This CPU speed calculation type is not supported for this CPU');
  end;
end;

procedure TCpuClockSpeed.SetPriority( aRunPriority : enumRunPriority );
var
  H : THandle;
begin
  H := GetCurrentProcess();
  case aRunPriority of
  eRPNormal  : SetPriorityClass( H, NORMAL_PRIORITY_CLASS );
  eRPHigh    : SetPriorityClass( H, HIGH_PRIORITY_CLASS );
  eRPRealTime: SetPriorityClass( H, REALTIME_PRIORITY_CLASS );
  else raise exception.Create('Unknown program priority.');
  end; // case
end; // procedure TCpuClockSpeed.SetPriority

function TCpuClockSpeed.GetCpuSpeedCalculationSupport: setCpuSpeedCalcSupport;
var
  lCpuidLevel1 : TCpuidLevel1;
begin
  result := [eWMI, eRDTSC];
  if ( fIsC7 or fIsCn or fIsEfficeon ) then result := result + [eCPUID];
  if assigned( fMsrDriver ) then begin
    lCpuidLevel1 := TCpuidLevel1.Create;
    try
      if lCpuidLevel1.EnhancedSpeedStep then result := result + [eMSR];
    finally
      freeAndNil( lCpuidLevel1 );
    end;
  end; // if
end; //function TCpuClockSpeed.GetCpuSpeedCalculationSupport: setCpuSpeedCalcSupport;

function TCpuClockSpeed.GetCPUSpeed: int64;
begin
  // there are three ways to determine clock speed:
  //   1: RDTSC -- f = dTSC / dt.  This class has a built-in timer
  //          so that polling can occur at regular intervals.  With >= C3-States
  //          RDTSC, will not be incremented on many processors. This does yield
  //          the true numbers of CPU cycles per second, but does not reflect
  //          the clock frequency when the CPU is active.
  //   2: WMI -- we poll what Windows thinks the clock speed is.  WMI is often
  //          wrong, but it does accomodate Deep and Deeper sleep states
  //          (C3, C4).
  //   3: CPUID -- New VIA processors and Transmeta efficeons report the current
  //          multiplier through a CPUID call
  //   4: MSR -- SpeedStep and PowerSaver multipliers can be read through a MSR.
  case fCpuSpeedCalculationType of
  eRDTSC  : result := GetCPUSpeedByRDTSC div ONE_MILLION;
  eWMI    : result := GetCPUSpeedByWmi;
  eCPUID  : result := GetCpuSpeedByCpuid div ONE_MILLION;
  eMSR    : result := GetCpuSpeedByMsr;
  else
    raise exception.Create('Unhandled CPU Speed calculation type');
  end; // case

//  ai64TickCount := -1;
//  adTime := -1;
end; // function TfrmCPUSpeed.GetCPUSpeed

function TCpuClockSpeed.GetCpuSpeedByCpuid: int64;
begin
  if fIsEfficeon then begin
    result := GetTransmetaClockSpeed;
  end else if fIsC7 or fIsCn then begin
    result := GetC7ClockSpeed;
  end; // if
end;

function TCpuClockSpeed.GetCpuSpeedByMsr: int64;
begin
  result := fFsbMhz * fMsrDriver.GetSpeedStepState( 0 ).FsbMultiplier;
end;

function TCpuClockSpeed.GetCPUSpeedByRDTSC: int64;
var
  ldElapsedTime : double;
begin
  //   1: RDTSC -- f = dTSC / dt.  This class has a built-in timer
  //          so that polling can occur at regular intervals.  With >= C3-States
  //          RDTSC, will not be incremented on many processors. This does yield
  //          the true numbers of CPU cycles per second, but does not reflect
  //          the clock frequency when the CPU is active.
  if fi64StartTick = 0 then begin
    fi64StartTick := GetCycleCount;
    fStopWatch.StartTimer;
    Sleep( 10 );
  end; // if
  ldElapsedTime := fStopWatch.StopTimer;
  fi64StopTick := GetCycleCount;
  result := round( (fi64StopTick - fi64StartTick) / ldElapsedTime );
  fi64StartTick := GetCycleCount;
  fStopWatch.StartTimer;
end; // function TCpuClockSpeed.GetCPUSpeed

function TCpuClockSpeed.DetermineMaximumClockSpeed: int64;
var
  li64StartTick : int64;
  li64StopTick  : int64;
  li64TickCount : int64;
  ldTime        : double;
  lCpuidLevel1 : TCpuidLevel1;
begin
//    result := round( fCpuidCentaurPerformanceInfo.HighestClockMultiplier *
//              fCpuidCentaurPerformanceInfo.FrontSideBusClock ) * ONE_MILLION;
  if fEnhancedSpeedStep then begin
    result := round( fStopWatch.GetCPUClockspeed( FALSE ) );
    if fIsC7 or fIsCn then begin
      fCpuidCentaurPerformanceInfo.Initialize;
      fFsbMhz := round( result / ( fCpuidCentaurPerformanceInfo.CurrentClockMultiplier * ONE_MILLION ) );
      if round( 10 * ( fFsbMhz / fCpuidCentaurPerformanceInfo.FrontSideBusClock) ) <> 10 then begin
        showmessage( 'Calculated FSB does not equal reported FSB.' );
      end else begin
        fFsbMhz := round( fCpuidCentaurPerformanceInfo.FrontSideBusClock );
      end;
    end else begin
      fFsbMhz :=  round( result / ( fMsrDriver.GetSpeedStepState( 0 ).FsbMultiplier * ONE_MILLION ) );
    end; // if
  end else if fIsEfficeon  then begin
    SetPriority( eRPRealTime );
    Fibonacci( FIB_NUMBER );
    result := GetTransmetaClockSpeed;
    SetPriority( eRPNormal );
  end else begin
    result := round( fStopWatch.GetCPUClockspeed( FALSE ) );
//    Fibonacci( FIB_NUMBER );
//    StopWatch.Resolution := swresHigh;
//    li64StartTick := T64bitRegister( fMSRDriver.ReadMsr( 0, 16 ) ).AsInt64;
//    StopWatch.StartTimer;
//    Fibonacci( FIB_NUMBER );
//    ldTime := StopWatch.StopTimer;
//    li64StopTick := T64bitRegister( fMSRDriver.ReadMsr( 0, 16 ) ).AsInt64;
//    li64TickCount := li64StopTick - li64StartTick;
//    result := Round( li64TickCount / ldTime );
  end; // if
end; // function TCpuClockSpeed.DetermineMaximumClockSpeed

function TCpuClockSpeed.GetC7Temperature: integer;
var
  li64StartTick    : Int64;
  li64StopTick     : Int64;
begin
  if ( fIsC7 or fIsCn ) then begin
    result := CpuidGetC7Temperature;
  end;
end; // function TCpuClockSpeed.GetCPUSpeed

function TCpuClockSpeed.GetCPUSpeedByWmi: int64;
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
end; //function TfrmCPUSpeed.GetCpuMhz: integer;

function TCpuClockSpeed.DetectCpuVendor : enumCpuVendor;
var
  lCpuidProcessorName : TCpuidProcessorName;
  lCpuidLevel1 : TCpuidLevel1;
begin
  if not assigned( fCpuidLevel0 ) then begin
    fCpuidLevel0 := TCpuidLevel0.Create;
  end;
  if fCpuidLevel0.VendorID = 'GenuineTMx86' then begin
    result := eCpuVendorTransmeta;
    fIsTransmeta := TRUE;
    lCpuidProcessorName := TCpuidProcessorName.Create;
    try
      if (pos('Efficeon', lCpuidProcessorName.ProcessorName) <> 0) then begin
        fIsEfficeon := TRUE;
      end else begin
        fIsEfficeon := FALSE;
      end;
    finally
      freeAndNil( lCpuidProcessorName );
    end; // try
  end else if fCpuidLevel0.VendorID = 'CentaurHauls' then begin
    result := eCpuVendorVIA;
    fIsCentaur := TRUE;
    lCpuidLevel1 := TCpuidLevel1.Create;
    try
      if ( lCpuidLevel1.Family = 6 ) then
        if ( lCpuidLevel1.Model = 10 ) or ( lCpuidLevel1.Model = 13 )
        then fIsC7 := TRUE
        else if ( lCpuidLevel1.Model = 15 ) then fIsCn := TRUE;
    finally
      FreeAndNil( lCpuidLevel1 );
    end; // try..finally
//    fCpuidCentaur := TCpuidCentaur.Create;
//    try
//      if IntToHex( fCpuidCentaur.MaxSupportedLevel, 8 ) >= 'C0000002' then begin
//        fIsC7 := TRUE;
//        fIsCpuTemperatureAvailable := TRUE;
//      end;
//    finally
//      FreeAndNil( fCpuidCentaur );
//    end;
  end else if Pos( 'AMD', fCpuidLevel0.VendorID ) > 0 then begin
    result := eCpuVendorAMD;
  end else if Pos( 'Intel', fCpuidLevel0.VendorID ) > 0 then begin
    result := eCpuVendorIntel;
  end; // if
end; // function TCpuClockSpeed.DetectCpuVendor

function TCpuClockSpeed.GetTransmetaClockSpeed : int64;
begin
  fCpuidTransmetaPerformanceData := TCpuidTransmetaPerformanceData.Create;
  try
    fCpuidTransmetaPerformanceData.Initialize;
    result := fCpuidTransmetaPerformanceData.CpuMHz * ONE_MILLION;
  finally
    freeAndNil( fCpuidTransmetaPerformanceData );
  end; // try
end; // function TCpuClockSpeed.GetTransmetaClockSpeed

function TCpuClockSpeed.GetC7ClockSpeed : int64;
begin
  fCpuidCentaurPerformanceInfo.Initialize;
  result := round( fCpuidCentaurPerformanceInfo.CurrentClockMultiplier *
            fCpuidCentaurPerformanceInfo.FrontSideBusClock ) * ONE_MILLION;
end; // function TCpuClockSpeed.GetTransmetaClockSpeed

procedure TCpuClockSpeed.LoadDownCpu( n : integer );
var
  lFibThread : TFibThread;
begin
  // we can set the thread court to zero because we are going to wait until the
  // thread completes.
  fNbrOfThreadsRunning := 0;
  lFibThread := TFibThread.Create( n );
  try
    lFibThread.OnTerminate := ThreadDone;
    lFibThread.Resume;
    inc( fNbrOfThreadsRunning );
    // wait until thread is finished
    while ( fNbrOfThreadsRunning = 1 ) do begin
      sleep( 10 );
      application.processmessages;
    end; // while
  finally
    // no need to free thread
  end; // try..finally
end;

procedure TCpuClockSpeed.ThreadDone(Sender: TObject);
begin
  dec( fNbrOfThreadsRunning );
  application.ProcessMessages;
end; // procedure TCpuClockSpeed.FibThreadDone;

function TCpuClockSpeed.GetCpuTemperature : integer;
begin
  if fIsCpuTemperatureAvailable then begin
    GetC7Temperature;
  end;
end; // function TCpuClockSpeed.GetCpuTemperature : integer;

function TCpuClockSpeed.IsCpuTemperatureAvailable: Boolean;
begin
  result := fIsCpuTemperatureAvailable;
end; //function TCpuClockSpeed.IsCpuTemperatureAvailable: Boolean;

procedure TCpuClockSpeed.TimerUpdate(Sender : TObject);
begin
  // update clock speed:

  // update CPUTemperature:

  if assigned( OnTimerUpdate ) then OnTimerUpdate( Sender );
end;

// TFibThread...................................................................
constructor TFibThread.Create(n: integer);
begin
  Fn := n;
  FreeOnTerminate := True;
  inherited Create(TRUE);  // creates suspended
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

end.
