unit uOls;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, COSBI_Common, CosbiCpuid;

type

  TSpeedStepState = record
    FsbMultiplier : integer;
    Vcc : double;
    VccStep : integer;
  end;

  TMinMaxSpeedStepStates = record
    MinState : TSpeedStepState;
    MaxState : TSpeedStepState;
  end;

  TOls = class(TObject)
  private
    { Private declarations }
  protected
    fenumSpeedStepVersion : enumSpeedStepVersion;
    function GetSpeedStepVersion: enumSpeedStepVersion;
  public
    { Public declarations }
    constructor Create;
    procedure CloseOls;
//    function DecodeSpeedStepVcc( aiSpeedStepValue : integer ): double;
//    function EncodeSpeedStepVcc( adSpeedStepVcc : double ): integer;
//    function EncodeSpeedStepVccBanias( adSpeedStepVcc : double ): integer;
//    function EncodeSpeedStepVccYonah( adSpeedStepVcc : double ): integer;
    function GetMaxCpuMultiplier( cpu : integer ): cardinal;
    function GetMaxVcc( cpu : integer ): double;
    function GetMinMaxSpeedStepStates(cpu: integer): TMinMaxSpeedStepStates;
    function GetNumberOfCpus: integer;
    function GetSpeedStepState( cpu : integer ) : TSpeedStepState;
    function OpenOls: Boolean;
    function ReadMsr(cpu, msr: integer): T64bitRegister;
    function WriteMsr(cpu, msr: integer;
                      Msr64bitRegister : T64bitRegister
                      ): T64bitRegister;
//    procedure CachesOff(cpu: integer);
//    procedure CachesOn(cpu: integer);
    procedure SetSpeedStepMultiplier( cpu, aiCpuMultiplier : integer );
    procedure SetSpeedStepState(cpu : integer; NewState: TSpeedStepState);
    procedure SetSpeedStepVccStep( cpu, aiCpuVccStep : integer );
    procedure SetUnsetMsrBits(cpu, msr: integer;
                              BitsToSet, BitsToUnset : T64bits);
    procedure ToggleMsrBits(cpu, msr : integer;
                            BitsToToggle : T64bitRegister);
//    procedure WriteBackInvalidate( cpu : integer );
  end;

const
  SPEEDSTEP_CURRENT_PSTATE_MSR = $198;
  SPEEDSTEP_SET_PSTATE_MSR = $199;




implementation

function InitializeOls: Boolean; stdcall; external 'WinRing0.dll';

procedure DeinitializeOls; stdcall; external 'WinRing0.dll';

function GetDllVersion(
	var major : byte;
	var minor : byte;
	var revision : byte;
	var release : byte
): integer; stdcall; external 'WinRing0.dll';

function RdmsrEx(
	msrIndex : cardinal;
	var lowWord : cardinal;
	var highWord : cardinal;
	var cpuNumber : cardinal
): Boolean; stdcall; external 'WinRing0.dll';

function WrmsrEx(
	msrIndex : cardinal;
	lowWord : cardinal;
	highWord : cardinal;
	cpuNumber : cardinal
): Boolean; stdcall; external 'WinRing0.dll';

function RdpmcEx(
	pmcIndex : cardinal;
	lowWord : cardinal;
	highWord : cardinal;
	cpuNumber : cardinal
): Boolean; stdcall; external 'WinRing0.dll';

function CpuidEx(
	cpuidIndex : integer;
	var eax : dword;
	var ebx : dword;
	var ecx : dword;
	var edx : dword;
	cpuNumber : cardinal
): Boolean; stdcall; external 'WinRing0.dll';

function RdtscEx(
	var lowWord : cardinal;
	var highWord : cardinal;
	var cpuNumber : cardinal
): Boolean; stdcall; external 'WinRing0.dll';

end.
