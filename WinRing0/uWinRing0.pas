unit uWinRing0;

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

  TWinRing0 = class(TObject)
  private
    { Private declarations }
    fMsrRegister : T64bitRegister;
  protected
    fenumSpeedStepVersion : enumSpeedStepVersion;
    function GetSpeedStepVersion: enumSpeedStepVersion;
    procedure CloseWinRing0;
    function InitializeWinRing0: Boolean;
    procedure ChangeVendorString( aiCpu : integer;
                                  ai64FirstLongWord : int64;
                                  aiLastWord: integer );
    procedure ChangeCpuNameString( aiCpu : integer;
                aReg1, aReg2, aReg3, aReg4, aReg5, aReg6 : T64bitRegister);
  public
    { Public declarations }
    constructor Create;
    destructor Destroy;
    function ReadMsr(cpu, msr: cardinal; var msrValue: T64bitRegister): Boolean;
    function WriteMsr(cpu, msr: cardinal; var msrValue : T64bitRegister
                      ): Boolean; // the ending value will be returned
    function GetNumberOfCpus: integer;
    function Rdtsc( cpu : cardinal ): T64bitRegister;
    function ToggleMsrBits(cpu, msr : cardinal;
                           BitsToToggle : T64bitRegister): Boolean;
    function DecodeSpeedStepVcc( aiSpeedStepValue : integer ): double;
    function EncodeSpeedStepVcc( adSpeedStepVcc : double ): integer;
    function EncodeSpeedStepVccBanias( adSpeedStepVcc : double ): integer;
    function EncodeSpeedStepVccYonah( adSpeedStepVcc : double ): integer;
    function GetMaxCpuMultiplier( cpu : integer ): cardinal;
    function GetMaxVcc( cpu : integer ): double;
    function GetMinMaxSpeedStepStates(cpu: integer): TMinMaxSpeedStepStates;
    function GetSpeedStepState( cpu : integer ) : TSpeedStepState;
//    procedure CachesOff(cpu: integer);
//    procedure CachesOn(cpu: integer);
    procedure SetSpeedStepMultiplier( cpu, aiCpuMultiplier : integer );
    procedure SetSpeedStepState(cpu : integer; NewState: TSpeedStepState);
    procedure SetSpeedStepVccStep( cpu, aiCpuVccStep : integer );
    function SetUnsetMsrBits(cpu, msr: integer;
                             BitsToSet, BitsToUnset : T64bits): Boolean;
    procedure MakeCustomVendorString( aiCpu : integer; asVendorString : string );
    procedure MakeVendorAMD( aiCpu : integer );
    procedure MakeVendorIntel( aiCpu : integer );
    procedure MakeVendorVia( aiCpu : integer );
    procedure MakeCustomCpuNameString( aiCpu : integer; asCpuNameString : string );
    procedure ChangeCpuStepping( aiCpu, aiStepping : integer);
    procedure ChangeCpuModel( aiCpu, aiModel : integer);
    procedure ChangeCpuFamily( aiCpu, aiFamily : integer);
    function GetMaxVccStep( cpu : integer ): integer;
//    procedure WriteBackInvalidate( cpu : integer );
  end;

const
  SPEEDSTEP_CURRENT_PSTATE_MSR = $198;
  SPEEDSTEP_SET_PSTATE_MSR = $199;
  // for c7
  MSR1108 = $1108;
  MSR1109 = $1109;
  // for cn
  MSR1204 = $1204;
  MSR1206 = $1206;
  MSR1207 = $1207;
  MSR1208 = $1208;
  MSR1209 = $1209;
  MSR120A = $120A;
  MSR120B = $120B;
  MSR120C = $120C;
  MSR120D = $120D;

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

constructor TWinRing0.Create;
begin
  inherited;
  if not InitializeOls then begin
    exception.Create('I could not initialize WinRing0. Please ensure that the' + CRLF
               + 'WinRing0 DLL and SYS files are in the same directory as the executable.' );
    Destroy;
  end;
end;

destructor TWinRing0.Destroy;
begin
  CloseWinRing0;
  inherited;
end;

procedure TWinRing0.CloseWinRing0;
begin
  DeinitializeOls;
end;

function TWinRing0.InitializeWinRing0: Boolean;
begin
  result := InitializeOls;
end;

function TWinRing0.ReadMsr(cpu, msr: cardinal; var msrValue: T64bitRegister): Boolean;
begin
  if RdmsrEx( msr, msrValue.LowWord, msrValue.HighWord, cpu ) then begin
    result := TRUE;
  end else begin
    exception.Create( 'Sorry! I could not read MSR 0x' + IntToHex(msr, 8) + '.' );
    result := FALSE;
  end;
end;

function TWinRing0.WriteMsr(cpu, msr: cardinal;
                            var MsrValue : T64bitRegister
                           ): Boolean;
begin
  if not WrmsrEx( msr, MsrValue.LowWord, MsrValue.HighWord, cpu ) then begin
    ShowMessage( 'Sorry! I could not write to MSR 0x' + IntToHex(msr, 8) + '.' );
    result := FALSE;
  end else if not RdmsrEx( msr, MsrValue.LowWord, MsrValue.HighWord, cpu ) then begin
    ShowMessage( 'Sorry! Although I could write to it, I could not read MSR 0x'
                + IntToHex(msr, 8) + '.' );
    MsrValue.AsInt64 := 0;
    result := FALSE;
  end else result := TRUE;
end;

function TWinRing0.GetNumberOfCpus: integer;
begin
  result := strToInt( GetEnvironmentVariable( 'NUMBER_OF_PROCESSORS' ) );
end;

function TWinRing0.Rdtsc( cpu: cardinal ): T64bitRegister;
begin
  if not RdtscEx( result.LowWord, result.HighWord, cpu ) then begin
    ShowMessage( 'Sorry! I could not read the time stamp counter.' );
    result.AsInt64 := 0;
  end; // if
end;

function TWinRing0.ToggleMsrBits(cpu, msr : cardinal;
                                  BitsToToggle : T64bitRegister): Boolean;
var
  MsrValue : T64bitRegister;
  ReturnCode : integer;
begin
  if ReadMsr(cpu, msr, MsrValue) then begin
    MsrValue.AsInt64 := MsrValue.AsInt64 xor BitsToToggle.AsInt64;
    if WriteMsr(cpu, msr, MsrValue) then begin
      result := TRUE;
    end else result := FALSE;
  end else result := FALSE;;
end;

function TWinRing0.SetUnsetMsrBits(cpu, msr: integer;
                                   BitsToSet, BitsToUnset : T64bits): Boolean;
var
  MsrRegisterValue : T64bitRegister;
  ReturnCode : integer;
begin
  if ReadMsr(cpu, msr, MsrRegisterValue) then begin
    MsrRegisterValue.bits64 := MsrRegisterValue.bits64 + BitsToSet - BitsToUnset;
    result := WriteMsr(cpu, msr, MsrRegisterValue);
  end; // if
end;

function TWinRing0.GetSpeedStepState( cpu : integer ) : TSpeedStepState;
var
  MsrRegisterValue : T64bitRegister;
  ReturnCode : integer;
begin
  if ReadMsr(cpu, $0198, MsrRegisterValue) then begin
    result.FsbMultiplier := ExtractBitValue( MsrRegisterValue.LowWord, 8, 15);
    //result.Vcc := DecodeSpeedStepVcc( ExtractBitValue( MsrRegisterValue.LowWord, 0, 7) );
    result.VccStep := ExtractBitValue( MsrRegisterValue.LowWord, 0, 7);
  end;
end;

procedure TWinRing0.SetSpeedStepState(cpu : integer; NewState: TSpeedStepState);
var
  MsrRegisterValue : T64bitRegister;
  liVccEncoding, liFsbEncoding : integer;
begin
  liVccEncoding := NewState.VccStep;
  liFsbEncoding := (NewState.FsbMultiplier shl 8);
  MsrRegisterValue.AsInt64 := liFsbEncoding + liVccEncoding;
//  showmessage( 'Vcc: ' + IntToHex( liVccEncoding, 4 ) + ' FSB : ' + IntToHex( liFsbEncoding, 4 ) + ' msr : ' + intToHex( MsrRegisterValue.AsInt64, 8 ) );
  WriteMsr( 0, $199, MsrRegisterValue );
end;

function TWinRing0.GetMinMaxSpeedStepStates(
  cpu: integer): TMinMaxSpeedStepStates;
var
  MsrRegisterValue : T64bitRegister;
begin
  if ReadMsr(cpu, $0198, MsrRegisterValue) then begin
    result.MinState.FsbMultiplier := ExtractBitValue( MsrRegisterValue.HighWord, 24, 31);
    //result.MinState.Vcc := DecodeSpeedStepVcc( ExtractBitValue( MsrRegisterValue.HighWord, 16, 23) );
    result.MinState.VccStep := ExtractBitValue( MsrRegisterValue.HighWord, 16, 23);
    result.MaxState.FsbMultiplier := ExtractBitValue( MsrRegisterValue.HighWord, 8, 15);
    //result.MaxState.Vcc := DecodeSpeedStepVcc( ExtractBitValue( MsrRegisterValue.HighWord, 0, 7) );
    result.MaxState.VccStep := ExtractBitValue( MsrRegisterValue.HighWord, 0, 7);
  end;
end;

//function TMsrDriver.DecodeSpeedStepVcc( EncodedVcc : integer ): double;
//begin
//  result := 0.7 + EncodedVcc * 0.016;
//end;

//function TMsrDriver.EncodeSpeedStepVcc( Vcc : double ): integer;
//begin
//  result := Round ((Vcc - 0.7) / 0.016 );
//end;

function TWinRing0.GetMaxVcc( cpu : integer ): double;
var
  lMinMaxSpeedStepStates : TMinMaxSpeedStepStates;
begin
  lMinMaxSpeedStepStates := GetMinMaxSpeedStepStates( cpu );
  result := lMinMaxSpeedStepStates.MaxState.Vcc;
end; // function GetMaxVcc

function TWinRing0.GetMaxVccStep( cpu : integer ): integer;
var
  lMinMaxSpeedStepStates : TMinMaxSpeedStepStates;
begin
  lMinMaxSpeedStepStates := GetMinMaxSpeedStepStates( cpu );
  result := lMinMaxSpeedStepStates.MaxState.VccStep;
end; // function GetMaxVcc

function TWinRing0.GetMaxCpuMultiplier( cpu : integer ): cardinal;
var
  lMinMaxSpeedStepStates : TMinMaxSpeedStepStates;
begin
  lMinMaxSpeedStepStates := GetMinMaxSpeedStepStates( cpu );
  result := lMinMaxSpeedStepStates.MaxState.FsbMultiplier;
end; // function GetMaxVcc

procedure TWinRing0.SetSpeedStepMultiplier( cpu, aiCpuMultiplier : integer );
var
  lSpeedStepState : TSpeedStepState;
begin
  if aiCpuMultiplier > GetMaxCpuMultiplier( cpu ) then begin
    if application.MessageBox('The multiplier that you are setting exceeds the maximum multiplier for this cpu.' + CR_LF
      + 'Do you want to continue?', 'Multiplier Exceeds Maximum!', MB_YESNO or MB_ICONQUESTION ) = IDNO then begin
      exit;
    end; // if
  end; // if
  lSpeedStepState := GetSpeedStepState( cpu );
  lSpeedStepState.FsbMultiplier := aiCpuMultiplier;
  SetSpeedStepState( cpu, lSpeedStepState );
end; // procedure TMsrDriver.SetSpeedStepMultiplier( aiCpuMultiplier : integer );

procedure TWinRing0.SetSpeedStepVccStep( cpu, aiCpuVccStep : integer );
var
  lSpeedStepState : TSpeedStepState;
begin
//  if DecodeSpeedStepVcc( aiCpuVccStep  ) > GetMaxVcc( cpu ) then begin
//    if application.MessageBox('The Vcc that you are setting exceeds the maximum Vcc for this cpu.' + CR_LF
//      + 'Do you want to continue?', 'Vcc Exceeds Maximum!', MB_YESNO or MB_ICONQUESTION ) = IDNO then begin
//      exit;
//    end; // if
//  end; // if
  if aiCpuVccStep > GetMaxVccStep( cpu ) then begin
    if application.MessageBox('The Vcc that you are setting exceeds the maximum Vcc for this cpu.' + CR_LF
      + 'Do you want to continue?', 'Vcc Exceeds Maximum!', MB_YESNO or MB_ICONQUESTION ) = IDNO then begin
      exit;
    end; // if
  end; // if
  lSpeedStepState := GetSpeedStepState( cpu );
  lSpeedStepState.VccStep := aiCpuVccStep;
  lSpeedStepState.Vcc := DecodeSpeedStepVcc( aiCpuVccStep );
  SetSpeedStepState( cpu, lSpeedStepState );
end; // procedure TMsrDriver.SetSpeedStepVccStep( aiCpuMultiplier : integer );

// GetSpeedStepVersion has to go in this class because C7 can be either Banias
// or Yonah SS
function TWinRing0.GetSpeedStepVersion: enumSpeedStepVersion;
begin
  result := eBanias;
  if CpuidIsEnhancedSpeedStepAvailable then begin
    if ( CpuidIsIntelCore or CpuidIsIntelCore2 ) then begin   // <<<< NOT COMPLETE!!!!!!
      result := eYonah;
    end;
  end else begin
    result := eUnknown;
  end;
end;

{
  DecodeSpeedStepValue accepts the integer SpeedStep value and returns a
  double which contains the corresponding Vcc.
}
function TWinRing0.DecodeSpeedStepVcc( aiSpeedStepValue : integer ): double;
var
  ldBaseVcc : double;
  ldIncrementVcc : double;
begin
  if CpuidIsIntelCore or CpuidIsIntelCore2 or CpuidIsCentaurCn then begin
    ldBaseVcc := 1.5;
    ldIncrementVcc := -0.0125;
  end else begin
    ldBaseVcc := 0.7;
    ldIncrementVcc := 0.016;
  end;
  result := ldBaseVcc + aiSpeedStepValue * ldIncrementVcc;
  if CpuidIsCentaurC7 and ( result > 1.8 ) then begin
    ldBaseVcc := 1.5;
    ldIncrementVcc := -0.0125;
    result := ldBaseVcc + aiSpeedStepValue * ldIncrementVcc;
  end;
end;  // function DecodeSpeedStepValue

{
  EncodeSpeedStepValue accepts the integer SpeedStep value and returns a
  double which contains the corresponding Vcc.
}
function TWinRing0.EncodeSpeedStepVcc( adSpeedStepVcc : double ): integer;
var
  ldBaseVcc : double;
  ldIncrementVcc : double;
begin
  if CpuidIsIntelCore or CpuidIsIntelCore2 or CpuidIsCentaurCn   // <<<<< fix this !!!!!!!!
     or CpuidIsCentaurC7 then begin
    ldBaseVcc := -0.0875;
    ldIncrementVcc := 0.0125;
  end else begin
    ldBaseVcc := 0.7;
    ldIncrementVcc := 0.016;
  end;
  result := round( ( adSpeedStepVcc - ldBaseVcc ) - ldBaseVcc ); // <<<<< fix this !!!!!!!!
//  if CpuidIsCentaurC7 and ( DecodeSpeedStepVcc( adSpeedStepVcc )  > 1.9 ) then begin
//    ldBaseVcc := 0.7;
//    ldIncrementVcc := 0.016;
//    result := round( ( adSpeedStepVcc - ldBaseVcc ) - ldBaseVcc; // <<<<< fix this !!!!!!!!
//  end;
end;  // function DecodeSpeedStepValue

function TWinRing0.EncodeSpeedStepVccBanias( adSpeedStepVcc : double ): integer;
var
  ldBaseVcc : double;
  ldIncrementVcc : double;
begin
  ldBaseVcc := 0.7;
  ldIncrementVcc := 0.016;
  result := round ( ( adSpeedStepVcc - ldBaseVcc ) - ldBaseVcc ); // <<<<< fix this !!!!!!!!
end;  // function EncodeSpeedStepVccBanias

function TWinRing0.EncodeSpeedStepVccYonah( adSpeedStepVcc : double ): integer;
var
  ldBaseVcc : double;
  ldIncrementVcc : double;
begin
  ldBaseVcc := -0.0875;
  ldIncrementVcc := 0.0125;
  result := round(( adSpeedStepVcc - ldBaseVcc ) - ldBaseVcc ); // <<<<< fix this !!!!!!!!
end;  // function EncodeSpeedStepVccBanias

procedure TWinRing0.MakeCustomCpuNameString( aiCpu : integer; asCpuNameString : string );
var
  lstr : array [0..47] of char;
  lsTemp : string;
  i : integer;
  lReg1 : T64bitRegister;
  lReg2 : T64bitRegister;
  lReg3 : T64bitRegister;
  lReg4 : T64bitRegister;
  lReg5 : T64bitRegister;
  lReg6 : T64bitRegister;
begin
  // initialize char array to blanks:
  for i := 0 to 47 do begin
    lstr[ i ] := ' ';
  end;
  for i := 1 to length( asCpuNameString ) do begin
    lsTemp := Copy( asCpuNameString, i, 1 );
    lstr[ i - 1 ] := lsTemp[1];
  end;
  for i := 0 to 7 do begin
    lReg1.byteArray[ i ] := byte( lstr[ i ] );
  end;
  for i := 8 to 15 do begin
    lReg2.byteArray[ i - 8 ] := byte( lstr[ i ] );
  end;
  for i := 16 to 23 do begin
    lReg3.byteArray[ i - 16 ] := byte( lstr[ i ] );
  end;
  for i := 24 to 31 do begin
    lReg4.byteArray[ i - 24 ] := byte( lstr[ i ] );
  end;
  for i := 32 to 39 do begin
    lReg5.byteArray[ i - 32 ] := byte( lstr[ i ] );
  end;
  for i := 40 to 47 do begin
    lReg6.byteArray[ i - 40 ] := byte( lstr[ i ] );
  end;
  ChangeCpuNameString( aiCpu, lReg1, lReg2, lReg3, lReg4, lReg5, lReg6 );
end;

procedure TWinRing0.ChangeCpuNameString( aiCpu : integer;
          aReg1, aReg2, aReg3, aReg4, aReg5, aReg6 : T64bitRegister);
// this procedure changes the CPU name strings for VIA CPUs.
// the string must be encoded in reverse word order.
// for instance:
// "CentuarHauls" ==> MSR1109 = "uarHCent", MSR1108 = "auls" + untouched 4 lower words
// The routine only works for CN processors.
begin
//  if CpuidIsCentaurCN then begin
    // first we need to turn on the ability to write a new cpu string:
    // we only need to toggle one bit in MSR 0x1204, so read that MSR first:
    ReadMsr( aiCpu, MSR1204 , fMsrRegister );
    // now turn on bit 9:
    fMsrRegister.bits64 := fMsrRegister.bits64 + [bit9];
    // and turn off bit 10 to stop reading the values from the fuses:
    //fMsrRegister.bits64 := fMsrRegister.bits64 - [bit10];
    // now write the new value to enable changing the vendor string:
    WriteMsr( aiCpu, MSR1204, fMsrRegister );
    // now we can change the processor string
    WriteMsr( aiCpu, MSR1208, aReg1 );
    WriteMsr( aiCpu, MSR1209, aReg2 );
    WriteMsr( aiCpu, MSR120A, aReg3 );
    WriteMsr( aiCpu, MSR120B, aReg4 );
    WriteMsr( aiCpu, MSR120C, aReg5 );
    WriteMsr( aiCpu, MSR120D, aReg6 );
//  end;
end;

procedure TWinRing0.MakeCustomVendorString( aiCpu : integer; asVendorString : string );
var
  lstr : array [0..11] of char;
  lsTemp : string;
  i : integer;
  lReg1 : T64bitRegister;
  lReg2 : T64bitRegister;
begin
  // initialize char array to blanks:
  for i := 0 to 11 do begin
    lstr[ i ] := ' ';
  end;
  for i := 1 to length( asVendorString ) do begin
    lsTemp := Copy( asVendorString, i, 1 );
    lstr[ i - 1 ] := lsTemp[1];
  end;
  for i := 0 to 3 do begin
    lReg1.byteArray[ i + 4 ] := byte( lstr[ i ] );
  end;
  for i := 4 to 7 do begin
    lReg1.byteArray[ i - 4 ] := byte( lstr[ i ] );
  end;
  for i := 8 to 11 do begin
    lReg2.byteArray[ i - 4 ] := byte( lstr[ i ] );
  end;
  ChangeVendorString( aiCpu, lReg1.AsInt64, lReg2.HighWord );
end;

procedure TWinRing0.ChangeVendorString( aiCpu : integer;
                                        ai64FirstLongWord : int64;
                                        aiLastWord: integer );
// this procedure changes the CPUID vendor strings for VIA CPUs.
// the string must be encoded in reverse word order.
// for instance:
// "CentuarHauls" ==> MSR1109 = "uarHCent", MSR1108 = "auls" + untouched 4 lower words
var
  liSwapWord : integer;
begin
  if CpuidIsCentaurC7 then begin
    // first we need to turn on the ability to write a new vendor string:
    // we only need to toggle one bit in MSR 0x1108, so read that MSR first:
    ReadMsr( aiCpu, MSR1108, fMsrRegister );
    // now turn on bit 14:
    fMsrRegister.bits64 := fMsrRegister.bits64 + [bit14];
    // now write the new value to enable changing the vendor string:
    WriteMsr( aiCpu, MSR1108, fMsrRegister );
    // now we can change the vendor string
    fMsrRegister.HighWord := aiLastWord;
    WriteMsr( aiCpu, MSR1108, fMsrRegister );
    fMsrRegister.AsInt64 := ai64FirstLongWord;
    WriteMsr( aiCpu, MSR1109, fMsrRegister );
  end else if CpuidIsCentaurCN then begin
    // first we need to turn on the ability to write a new vendor string:
    // we only need to toggle one bit in MSR 0x1204, so read that MSR first:
    ReadMsr( aiCpu, MSR1204, fMsrRegister );
    // now turn on bit 8:
    fMsrRegister.bits64 := fMsrRegister.bits64 + [bit8];
    // now write the new value to enable changing the vendor string:
    WriteMsr( aiCpu, MSR1204, fMsrRegister );
    // now we can change the vendor string
    fMsrRegister.AsInt64 := ai64FirstLongWord;
    liSwapWord := fMsrRegister.LowWord;
    fMsrRegister.LowWord := fMsrRegister.HighWord;
    fMsrRegister.HighWord := liSwapWord;
    WriteMsr( aiCpu, MSR1206, fMsrRegister );
    fMsrRegister.LowWord := aiLastWord;
    WriteMsr( aiCpu, MSR1207, fMsrRegister );
  end;
end;

procedure TWinRing0.MakeVendorIntel( aiCpu : integer );
begin
  ChangeVendorString( aiCpu, $756e654749656e69, $6c65746e );
end;

procedure TWinRing0.MakeVendorVia( aiCpu : integer );
begin
  ChangeVendorString( aiCpu, $746e654348727561, $736c7561 );
end;

procedure TWinRing0.MakeVendorAmd( aiCpu : integer );
begin
  ChangeVendorString( aiCpu, $6874754169746E65, $444D4163 );
end;

procedure TWinRing0.ChangeCpuStepping( aiCpu, aiStepping : integer);
var
  MsrRegisterValue : T64bitRegister;
begin
  // get the current value for MSR 0x1204
  if ReadMsr( aiCPU, MSR1204, MsrRegisterValue ) then begin
    // insert the new stepping value:
    MsrRegisterValue.AsInt64 :=
      InsertBitValue64( MsrRegisterValue.AsInt64, aiStepping, bit32, bit35);
    // now write back the value:
    WriteMsr( aiCpu, MSR1204, MsrRegisterValue );
  end; // if
end;

procedure TWinRing0.ChangeCpuModel( aiCpu, aiModel : integer);
var
  MsrRegisterValue : T64bitRegister;
begin
  // get the current value for MSR 0x1204
  if ReadMsr( aiCPU, MSR1204, MsrRegisterValue ) then begin
    // insert the new stepping value:
    MsrRegisterValue.AsInt64 :=
      InsertBitValue64( MsrRegisterValue.AsInt64, aiModel, bit36, bit39 );
    // now write back the value:
    WriteMsr( aiCpu, MSR1204, MsrRegisterValue );
  end; // if
end;

procedure TWinRing0.ChangeCpuFamily( aiCpu, aiFamily : integer);
var
  MsrRegisterValue : T64bitRegister;
begin
  // get the current value for MSR 0x1204
  if ReadMsr( aiCPU, MSR1204, MsrRegisterValue ) then begin
    // insert the new stepping value:
    MsrRegisterValue.AsInt64 :=
      InsertBitValue64( MsrRegisterValue.AsInt64, aiFamily, bit40, bit43);
    // now write back the value:
    WriteMsr( aiCpu, MSR1204, MsrRegisterValue );
  end; // if
end;

end.
