unit uMsr;
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
// Unit name: uMsr
// Unit description: Wrappers the MSR.dll dynamic MSR driver dll.
// Author: Van Smith
// Date: August 29, 2005
// OS dependent: Yes: Windows
// Resolution dependent: None
// External unit dependencies: COSBI_Common
//==============================================================================
// Modification History
// ------------ -------
// Ver  Date   Who    Description
// ==== ====== ====== ==========================================================
// 1.0  050829 Van     Created.
//==============================================================================

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

  TMsrDriver = class(TObject)
  private
    { Private declarations }
  protected
    fenumSpeedStepVersion : enumSpeedStepVersion;
    function GetSpeedStepVersion: enumSpeedStepVersion;
  public
    { Public declarations }
    constructor Create;
    function CloseDriver: Boolean;
    function DecodeSpeedStepVcc( aiSpeedStepValue : integer ): double;
    function EncodeSpeedStepVcc( adSpeedStepVcc : double ): integer;
    function EncodeSpeedStepVccBanias( adSpeedStepVcc : double ): integer;
    function EncodeSpeedStepVccYonah( adSpeedStepVcc : double ): integer;
    function GetMaxCpuMultiplier( cpu : integer ): cardinal;
    function GetMaxVcc( cpu : integer ): double;
    function GetMinMaxSpeedStepStates(cpu: integer): TMinMaxSpeedStepStates;
    function GetNumberOfCpus: integer;
    function GetSpeedStepState( cpu : integer ) : TSpeedStepState;
    function OpenDriver: Boolean;
    function ReadMsr(cpu, msr: integer): T64bitRegister;
    function WriteMsr(cpu, msr: integer;
                      Msr64bitRegister : T64bitRegister
                      ): T64bitRegister;
    procedure CachesOff(cpu: integer);
    procedure CachesOn(cpu: integer);
    procedure SetSpeedStepMultiplier( cpu, aiCpuMultiplier : integer );
    procedure SetSpeedStepState(cpu : integer; NewState: TSpeedStepState);
    procedure SetSpeedStepVccStep( cpu, aiCpuVccStep : integer );
    procedure SetUnsetMsrBits(cpu, msr: integer;
                              BitsToSet, BitsToUnset : T64bits);
    procedure ToggleMsrBits(cpu, msr : integer;
                            BitsToToggle : T64bitRegister);
    procedure WriteBackInvalidate( cpu : integer );
  end;

const
  SPEEDSTEP_CURRENT_PSTATE_MSR = $198;
  SPEEDSTEP_SET_PSTATE_MSR = $199;


implementation

function BackdoorOpen : Boolean; stdcall; external 'msr.dll';
function BackdoorClose : Boolean; stdcall; external 'msr.dll';
function BackdoorReadMsr (cpu : integer;
                          index : integer;
                          flags : integer;
                          var low : LongWord;
                          var high : LongWord
                          ) : integer; stdcall; external 'msr.dll';
function BackdoorWriteMsr(cpu   : integer;
                          index : integer;
                          flags : integer;
                          low   : LongWord;
                          high  : LongWord
                          ) : integer; stdcall; external 'msr.dll';
function BackdoorRmwMsr(cpu   : integer;
                        index : integer;
                        flags : integer;
                        andlo : LongWord;
                        andhi : LongWord;
                        orlo  : LongWord;
                        orhi  : LongWord;
                        var prevlo : LongWord;
                        var prevhi : LongWord
                        ) : integer; stdcall; external 'msr.dll';
function BackdoorNumberOfCpus: integer; stdcall; external 'msr.dll';
procedure BackdoorWbInvd(cpu : integer); stdcall; external 'msr.dll';
procedure BackdoorCache(cpu : integer;
                        state : integer ); stdcall; external 'msr.dll';

constructor TMsrDriver.Create;
begin
  inherited;
  fenumSpeedStepVersion := GetSpeedStepVersion;
end; // constructor

function TMsrDriver.OpenDriver: Boolean;
begin
  result := BackdoorOpen;
end;

function TMsrDriver.CloseDriver: Boolean;
begin
  result := BackdoorClose;
end;

function TMsrDriver.GetNumberOfCpus: integer;
begin
  result := BackdoorNumberOfCpus;
end;

function TMsrDriver.ReadMsr(cpu, msr: integer): T64bitRegister;
var
  ReturnCode : integer;
begin
  result.AsInt64 := -1;
  ReturnCode := BackdoorReadMsr(cpu, msr, 0, result.LowWord, result.HighWord);
end;

procedure TMsrDriver.SetUnsetMsrBits(cpu, msr: integer;
                                     BitsToSet, BitsToUnset : T64bits);
var
  MsrRegisterValue : T64bitRegister;
  ReturnCode : integer;
begin
  ReturnCode := BackdoorReadMsr(cpu, msr, 0, MsrRegisterValue.LowWord, MsrRegisterValue.HighWord);
  MsrRegisterValue.bits64 := MsrRegisterValue.bits64 + BitsToSet - BitsToUnset;
  ReturnCode := BackdoorWriteMsr(cpu, msr, 0, MsrRegisterValue.LowWord, MsrRegisterValue.HighWord);
end;

procedure TMsrDriver.WriteBackInvalidate( cpu : integer );
begin
  BackdoorWbInvd( cpu );
end;

function TMsrDriver.WriteMsr(cpu, msr: integer;
                             Msr64bitRegister : T64bitRegister
                             ): T64bitRegister;
var
  ReturnCode : integer;
begin
  result.AsInt64 := -1;
  ReturnCode := BackdoorWriteMsr(cpu, msr, 0, Msr64bitRegister.LowWord, Msr64bitRegister.HighWord);
  result := ReadMsr( cpu, msr );
end;

procedure TMsrDriver.ToggleMsrBits(cpu, msr : integer;
                                   BitsToToggle : T64bitRegister);
var
  MsrRegisterValue : T64bitRegister;
  ReturnCode : integer;
begin
  ReturnCode := BackdoorReadMsr(cpu, msr, 0, MsrRegisterValue.LowWord, MsrRegisterValue.HighWord);
  MsrRegisterValue.AsInt64 := MsrRegisterValue.AsInt64 xor BitsToToggle.AsInt64;
  ReturnCode := BackdoorWriteMsr(cpu, msr, 0, MsrRegisterValue.LowWord, MsrRegisterValue.HighWord);
end;

procedure TMsrDriver.CachesOff(cpu: integer);
begin
  BackdoorCache( cpu, 0 );
end;

procedure TMsrDriver.CachesOn(cpu: integer);
begin
  BackdoorCache( cpu, 1 );
end;

function TMsrDriver.GetSpeedStepState( cpu : integer ) : TSpeedStepState;
var
  MsrRegisterValue : T64bitRegister;
  ReturnCode : integer;
begin
  ReturnCode := BackdoorReadMsr(cpu, $0198, 0, MsrRegisterValue.LowWord, MsrRegisterValue.HighWord);
  result.FsbMultiplier := ExtractBitValue( MsrRegisterValue.LowWord, 8, 15);
  result.Vcc := DecodeSpeedStepVcc( ExtractBitValue( MsrRegisterValue.LowWord, 0, 7) );
  result.VccStep := ExtractBitValue( MsrRegisterValue.LowWord, 0, 7);
end;

procedure TMsrDriver.SetSpeedStepState(cpu : integer; NewState: TSpeedStepState);
var
  MsrRegisterValue : T64bitRegister;
  liVccEncoding, liFsbEncoding : integer;
begin
  liVccEncoding := EncodeSpeedStepVcc(NewState.Vcc);
  liFsbEncoding := (NewState.FsbMultiplier shl 8);
  MsrRegisterValue.AsInt64 := liFsbEncoding + liVccEncoding;
  showmessage( 'Vcc: ' + IntToHex( liVccEncoding, 4 ) + ' FSB : ' + IntToHex( liFsbEncoding, 4 ) + ' msr : ' + intToHex( MsrRegisterValue.AsInt64, 8 ) );
  WriteMsr( 0, $199, MsrRegisterValue );
end;

function TMsrDriver.GetMinMaxSpeedStepStates(
  cpu: integer): TMinMaxSpeedStepStates;
var
  MsrRegisterValue : T64bitRegister;
  ReturnCode : integer;
begin
  ReturnCode := BackdoorReadMsr(cpu, $0198, 0, MsrRegisterValue.LowWord, MsrRegisterValue.HighWord);
  result.MinState.FsbMultiplier := ExtractBitValue( MsrRegisterValue.HighWord, 24, 31);
  result.MinState.Vcc := DecodeSpeedStepVcc( ExtractBitValue( MsrRegisterValue.HighWord, 16, 23) );
  result.MinState.VccStep := ExtractBitValue( MsrRegisterValue.HighWord, 16, 23);
  result.MaxState.FsbMultiplier := ExtractBitValue( MsrRegisterValue.HighWord, 8, 15);
  result.MaxState.Vcc := DecodeSpeedStepVcc( ExtractBitValue( MsrRegisterValue.HighWord, 0, 7) );
  result.MaxState.VccStep := ExtractBitValue( MsrRegisterValue.HighWord, 0, 7);
end;

//function TMsrDriver.DecodeSpeedStepVcc( EncodedVcc : integer ): double;
//begin
//  result := 0.7 + EncodedVcc * 0.016;
//end;

//function TMsrDriver.EncodeSpeedStepVcc( Vcc : double ): integer;
//begin
//  result := Round ((Vcc - 0.7) / 0.016 );
//end;

function TMsrDriver.GetMaxVcc( cpu : integer ): double;
var
  lMinMaxSpeedStepStates : TMinMaxSpeedStepStates;
begin
  lMinMaxSpeedStepStates := GetMinMaxSpeedStepStates( cpu );
  result := lMinMaxSpeedStepStates.MaxState.Vcc;
end; // function GetMaxVcc

function TMsrDriver.GetMaxCpuMultiplier( cpu : integer ): cardinal;
var
  lMinMaxSpeedStepStates : TMinMaxSpeedStepStates;
begin
  lMinMaxSpeedStepStates := GetMinMaxSpeedStepStates( cpu );
  result := lMinMaxSpeedStepStates.MaxState.FsbMultiplier;
end; // function GetMaxVcc

procedure TMsrDriver.SetSpeedStepMultiplier( cpu, aiCpuMultiplier : integer );
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

procedure TMsrDriver.SetSpeedStepVccStep( cpu, aiCpuVccStep : integer );
var
  lSpeedStepState : TSpeedStepState;
begin
  if DecodeSpeedStepVcc( aiCpuVccStep  ) > GetMaxVcc( cpu ) then begin
    if application.MessageBox('The Vcc that you are setting exceeds the maximum Vcc for this cpu.' + CR_LF
      + 'Do you want to continue?', 'Vcc Exceeds Maximum!', MB_YESNO or MB_ICONQUESTION ) = IDNO then begin
      exit;
    end; // if
  end; // if
  lSpeedStepState := GetSpeedStepState( cpu );
  lSpeedStepState.VccStep := aiCpuVccStep;
  lSpeedStepState.Vcc := DecodeSpeedStepVcc( aiCpuVccStep );
  SetSpeedStepState( cpu, lSpeedStepState );
end; // procedure TMsrDriver.SetSpeedStepMultiplier( aiCpuMultiplier : integer );

// GetSpeedStepVersion has to go in this class because C7 can be either Banias
// or Yonah SS
function TMsrDriver.GetSpeedStepVersion: enumSpeedStepVersion;
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
function TMsrDriver.DecodeSpeedStepVcc( aiSpeedStepValue : integer ): double;
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
function TMsrDriver.EncodeSpeedStepVcc( adSpeedStepVcc : double ): integer;
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

function TMsrDriver.EncodeSpeedStepVccBanias( adSpeedStepVcc : double ): integer;
var
  ldBaseVcc : double;
  ldIncrementVcc : double;
begin
  ldBaseVcc := 0.7;
  ldIncrementVcc := 0.016;
  result := round ( ( adSpeedStepVcc - ldBaseVcc ) - ldBaseVcc ); // <<<<< fix this !!!!!!!!
end;  // function EncodeSpeedStepVccBanias

function TMsrDriver.EncodeSpeedStepVccYonah( adSpeedStepVcc : double ): integer;
var
  ldBaseVcc : double;
  ldIncrementVcc : double;
begin
  ldBaseVcc := -0.0875;
  ldIncrementVcc := 0.0125;
  result := round(( adSpeedStepVcc - ldBaseVcc ) - ldBaseVcc ); // <<<<< fix this !!!!!!!!
end;  // function EncodeSpeedStepVccBanias

end.
