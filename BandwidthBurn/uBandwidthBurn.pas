unit uBandwidthBurn;
{
  COSBI: Comprehensive Open Source Benchmarking Initiative
  Copyright (c) 2003 Van Smith

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
  Windows, Messages, SysUtils, Classes, Forms, COSBI_Common, uStopWatch;

const
  ALL_BITS_INT  = $FFFFFFFF;
  ALL_BITS_CARD = $FFFFFFFF;
  ARRAY_SIZE    = 4194304;
  ASSIGN_VERIFY_SIMPLE = 'Verify Simple';
  REPEAT_COUNT  = 20;
  ONE_KB        = 1024;
  CR_LF         = #13 + #10;

type

  TBigArray = array[0..ARRAY_SIZE] of Cardinal; //Integer;

  TAssignType = (assSimple, assMult, assRead, assWrite, assToggle, assToggle5x,
                 assBlockPrefetch32, assBlockPrefetch64, assBlockPrefetch128,
                 assBP32Write, assVerifySimple, assOOSimple, assOORead,
                 assOOWrite, assOOBP32, assOOBP64,
                 assOOBP128, assOOBP32Write, assOOBPRead,
                 assOOSimpleTightLoop, assSkip1_64B_CacheLine);

  EReadDoesNotMatchWrite = Class( Exception );

  TBandwidthBurn = class( TObject )
  private
    fStopWatch      : TStopWatch;
//    fA              : TBigArray;
    fDummyCardinal  : cardinal;
    fBreak          : Boolean;
    fSWOverhead     : extended;

  protected
    procedure MeasureSWOverhead;

  public

    constructor Create; Overload;
    destructor Destroy; Overload;

    property StopWatch:  TStopWatch  read fStopWatch write fStopWatch;
    property Break: Boolean read fBreak write fBreak;

    function ReadTestIterate(aiRepeatCount : integer;
                             aiArraySize   : integer;
                             var A : TBigArray
                            ): Extended;
    function ReadTestIterateBP32(aiRepeatCount : integer;
                             aiArraySize   : integer;
                             var A : TBigArray
                            ): Extended;
    function ReadTestIterateBP64(aiRepeatCount : integer;
                             aiArraySize   : integer;
                             var A : TBigArray
                            ): Extended;
    function ReadTestIterateBP128(aiRepeatCount : integer;
                             aiArraySize   : integer;
                             var A : TBigArray
                            ): Extended;
    function ReadTest(aiArraySize : integer;
                      var A : TBigArray ): Extended;
    function ReadTestBP32(aiArraySize : integer;
                          var A : TBigArray ): Extended;
    function ReadTestBP64(aiArraySize : integer; var A : TBigArray ): Extended;
    function ReadTestBP128(aiArraySize : integer;
                           var A : TBigArray ): Extended;
    function WriteTestIterate(aiRepeatCount : integer;
                              aiArraySize   : integer;
                              ac_WriteValue  : cardinal;
                              ab_RandomValue : Boolean;
                              var A : TBigArray
                              ): Extended;
    function WriteTestIterateBP32(aiRepeatCount : integer;
                              aiArraySize   : integer;
                              ac_WriteValue  : cardinal;
                              ab_RandomValue : Boolean;
                              var A : TBigArray
                              ): Extended;
    function WriteTestIterateBP64(aiRepeatCount : integer;
                              aiArraySize   : integer;
                              ac_WriteValue  : cardinal;
                              ab_RandomValue : Boolean;
                              var A : TBigArray
                              ): Extended;
    function WriteTestIterateBP128(aiRepeatCount : integer;
                              aiArraySize   : integer;
                              ac_WriteValue  : cardinal;
                              ab_RandomValue : Boolean;
                              var A : TBigArray
                              ): Extended;
    function WriteTest(aiArraySize   : integer;
                       ac_WriteValue  : cardinal;
                       var A : TBigArray
                       ): Extended;
    function WriteTestBP32(aiArraySize   : integer;
                       ac_WriteValue  : cardinal;
                       var A : TBigArray
                       ): Extended;
    function WriteTestBP64(aiArraySize   : integer;
                       ac_WriteValue  : cardinal;
                       var A : TBigArray
                       ): Extended;
    function WriteTestBP128(aiArraySize   : integer;
                       ac_WriteValue  : cardinal;
                       var A : TBigArray
                       ): Extended;
    function ReadWriteTest(aiRepeatCount : integer;
                           aiArraySize   : integer;
                           var A : TBigArray
                           ): Extended;
    function ReadWriteTestBP32(aiRepeatCount : integer;
                           aiArraySize   : integer;
                           var A : TBigArray
                           ): Extended;
    function ReadWriteTestBP64(aiRepeatCount : integer;
                           aiArraySize   : integer;
                           var A : TBigArray
                           ): Extended;
    function ReadWriteTestBP128(aiRepeatCount : integer;
                           aiArraySize   : integer;
                           var A : TBigArray
                           ): Extended;
    function SimpleTightLoop(aiRepeatCount : integer;
                           aiArraySize   : integer;
                           var A : TBigArray
                           ): Extended;
    procedure RandomizeBigArray(var A: TBigArray);

  end;  // TBandwidthBurn

implementation

constructor TBandwidthBurn.Create;
begin

  inherited;

//  RandomizeBigArray( fA );
  Break := FALSE;
  MeasureSWOverhead;

end; // constructor TBandwidthBurn.Create;

destructor TBandwidthBurn.Destroy;
begin
//  FreeAndNil(fStopWatch);
end;

procedure TBandwidthBurn.MeasureSWOverhead;
var
  n                 : integer;
  li64StartCycles, li64EndCycles  : Int64;
begin
  fSWOverhead := 0;

  for n := 1 to 100 do begin
    li64StartCycles := GetCycleCount;
    li64EndCycles := GetCycleCount;
    fSWOverhead := (li64EndCycles - li64StartCycles) + fSWOverhead;
  end;

  fSWOverhead := fSWOverhead / 100;

end; // procedure TBandwidthBurn.MeasurefSWOverhead;

procedure TBandwidthBurn.RandomizeBigArray(var A: TBigArray);
var
  i: integer;
begin
  for i:= 0 to ARRAY_SIZE do begin
    A[i] := Random(1000);
  end;
end; // procedure TfrmCOSBIBandwidthBurn.RandomizeBigArray


function TBandwidthBurn.ReadTest( aiArraySize : integer;
                                  var A : TBigArray): Extended;
var
  i             : integer;
  lcAccumulate : cardinal;
  li64StartCycle   : Int64;
  li64EndCycle     : Int64;
  leElapsedTime   : extended;

begin

  lcAccumulate := 0;
  leElapsedTime := 0;

  // at each step we need to repeat populating the array:
  i := 1;
  while i <= aiArraySize do begin

    // start timer
    li64StartCycle := GetCycleCount;

    // copy data: must accumulate or optimizer will remove
    lcAccumulate := A[i] + lcAccumulate;
    lcAccumulate :=A[i+1] + lcAccumulate;
    lcAccumulate :=A[i+2] + lcAccumulate;
    lcAccumulate :=A[i+3] + lcAccumulate;
    lcAccumulate :=A[i+4] + lcAccumulate;

    lcAccumulate :=A[i+5] + lcAccumulate;
    lcAccumulate :=A[i+6] + lcAccumulate;
    lcAccumulate :=A[i+7] + lcAccumulate;
    lcAccumulate :=A[i+8] + lcAccumulate;
    lcAccumulate :=A[i+9] + lcAccumulate;

    lcAccumulate :=A[i+10] + lcAccumulate;
    lcAccumulate :=A[i+11] + lcAccumulate;
    lcAccumulate :=A[i+12] + lcAccumulate;
    lcAccumulate :=A[i+13] + lcAccumulate;
    lcAccumulate :=A[i+14] + lcAccumulate;

    lcAccumulate :=A[i+15] + lcAccumulate;
    lcAccumulate :=A[i+16] + lcAccumulate;
    lcAccumulate :=A[i+17] + lcAccumulate;
    lcAccumulate :=A[i+18] + lcAccumulate;
    lcAccumulate :=A[i+19] + lcAccumulate;

    lcAccumulate :=A[i+20] + lcAccumulate;
    lcAccumulate :=A[i+21] + lcAccumulate;
    lcAccumulate :=A[i+22] + lcAccumulate;
    lcAccumulate :=A[i+23] + lcAccumulate;
    lcAccumulate :=A[i+24] + lcAccumulate;

    lcAccumulate :=A[i+25] + lcAccumulate;
    lcAccumulate :=A[i+26] + lcAccumulate;
    lcAccumulate :=A[i+27] + lcAccumulate;
    lcAccumulate :=A[i+28] + lcAccumulate;
    lcAccumulate :=A[i+29] + lcAccumulate;

    lcAccumulate :=A[i+30] + lcAccumulate;
    lcAccumulate :=A[i+31] + lcAccumulate;
    lcAccumulate :=A[i+32] + lcAccumulate;
    lcAccumulate :=A[i+33] + lcAccumulate;
    lcAccumulate :=A[i+34] + lcAccumulate;

    lcAccumulate :=A[i+35] + lcAccumulate;
    lcAccumulate :=A[i+36] + lcAccumulate;
    lcAccumulate :=A[i+37] + lcAccumulate;
    lcAccumulate :=A[i+38] + lcAccumulate;
    lcAccumulate :=A[i+39] + lcAccumulate;

    lcAccumulate :=A[i+40] + lcAccumulate;
    lcAccumulate :=A[i+41] + lcAccumulate;
    lcAccumulate :=A[i+42] + lcAccumulate;
    lcAccumulate :=A[i+43] + lcAccumulate;
    lcAccumulate :=A[i+44] + lcAccumulate;

    lcAccumulate :=A[i+45] + lcAccumulate;
    lcAccumulate :=A[i+46] + lcAccumulate;
    lcAccumulate :=A[i+47] + lcAccumulate;
    lcAccumulate :=A[i+48] + lcAccumulate;
    lcAccumulate :=A[i+49] + lcAccumulate;

    lcAccumulate :=A[i+50] + lcAccumulate;
    lcAccumulate :=A[i+51] + lcAccumulate;
    lcAccumulate :=A[i+52] + lcAccumulate;
    lcAccumulate :=A[i+53] + lcAccumulate;
    lcAccumulate :=A[i+54] + lcAccumulate;

    lcAccumulate :=A[i+55] + lcAccumulate;
    lcAccumulate :=A[i+56] + lcAccumulate;
    lcAccumulate :=A[i+57] + lcAccumulate;
    lcAccumulate :=A[i+58] + lcAccumulate;
    lcAccumulate :=A[i+59] + lcAccumulate;

    lcAccumulate :=A[i+60] + lcAccumulate;
    lcAccumulate :=A[i+61] + lcAccumulate;
    lcAccumulate :=A[i+62] + lcAccumulate;
    lcAccumulate :=A[i+63] + lcAccumulate;
    lcAccumulate :=A[i+64] + lcAccumulate;

    lcAccumulate :=A[i+65] + lcAccumulate;
    lcAccumulate :=A[i+66] + lcAccumulate;
    lcAccumulate :=A[i+67] + lcAccumulate;
    lcAccumulate :=A[i+68] + lcAccumulate;
    lcAccumulate :=A[i+69] + lcAccumulate;

    lcAccumulate :=A[i+70] + lcAccumulate;
    lcAccumulate :=A[i+71] + lcAccumulate;
    lcAccumulate :=A[i+72] + lcAccumulate;
    lcAccumulate :=A[i+73] + lcAccumulate;
    lcAccumulate :=A[i+74] + lcAccumulate;

    lcAccumulate :=A[i+75] + lcAccumulate;
    lcAccumulate :=A[i+76] + lcAccumulate;
    lcAccumulate :=A[i+77] + lcAccumulate;
    lcAccumulate :=A[i+78] + lcAccumulate;
    lcAccumulate :=A[i+79] + lcAccumulate;

    lcAccumulate :=A[i+80] + lcAccumulate;
    lcAccumulate :=A[i+81] + lcAccumulate;
    lcAccumulate :=A[i+82] + lcAccumulate;
    lcAccumulate :=A[i+83] + lcAccumulate;
    lcAccumulate :=A[i+84] + lcAccumulate;

    lcAccumulate :=A[i+85] + lcAccumulate;
    lcAccumulate :=A[i+86] + lcAccumulate;
    lcAccumulate :=A[i+87] + lcAccumulate;
    lcAccumulate :=A[i+88] + lcAccumulate;
    lcAccumulate :=A[i+89] + lcAccumulate;

    lcAccumulate :=A[i+90] + lcAccumulate;
    lcAccumulate :=A[i+91] + lcAccumulate;
    lcAccumulate :=A[i+92] + lcAccumulate;
    lcAccumulate :=A[i+93] + lcAccumulate;
    lcAccumulate :=A[i+94] + lcAccumulate;

    lcAccumulate :=A[i+95] + lcAccumulate;
    lcAccumulate :=A[i+96] + lcAccumulate;
    lcAccumulate :=A[i+97] + lcAccumulate;
    lcAccumulate :=A[i+98] + lcAccumulate;
    lcAccumulate :=A[i+99] + lcAccumulate;

    lcAccumulate :=A[i+100] + lcAccumulate;
    lcAccumulate :=A[i+101] + lcAccumulate;
    lcAccumulate :=A[i+102] + lcAccumulate;
    lcAccumulate :=A[i+103] + lcAccumulate;
    lcAccumulate :=A[i+104] + lcAccumulate;

    lcAccumulate :=A[i+105] + lcAccumulate;
    lcAccumulate :=A[i+106] + lcAccumulate;
    lcAccumulate :=A[i+107] + lcAccumulate;
    lcAccumulate :=A[i+108] + lcAccumulate;
    lcAccumulate :=A[i+109] + lcAccumulate;

    lcAccumulate :=A[i+110] + lcAccumulate;
    lcAccumulate :=A[i+111] + lcAccumulate;
    lcAccumulate :=A[i+112] + lcAccumulate;
    lcAccumulate :=A[i+113] + lcAccumulate;
    lcAccumulate :=A[i+114] + lcAccumulate;

    lcAccumulate :=A[i+115] + lcAccumulate;
    lcAccumulate :=A[i+116] + lcAccumulate;
    lcAccumulate :=A[i+117] + lcAccumulate;
    lcAccumulate :=A[i+118] + lcAccumulate;
    lcAccumulate :=A[i+119] + lcAccumulate;

    lcAccumulate :=A[i+120] + lcAccumulate;
    lcAccumulate :=A[i+121] + lcAccumulate;
    lcAccumulate :=A[i+122] + lcAccumulate;
    lcAccumulate :=A[i+123] + lcAccumulate;
    lcAccumulate :=A[i+124] + lcAccumulate;

    lcAccumulate :=A[i+125] + lcAccumulate;
    lcAccumulate :=A[i+126] + lcAccumulate;
    lcAccumulate :=A[i+127] + lcAccumulate;

    li64EndCycle := GetCycleCount;
    leElapsedTime := leElapsedTime +
      ( ( li64EndCycle - li64StartCycle ) - fSWOverhead ) /
      fStopWatch.GetCPUClockspeed(FALSE);

    i := i + 128;

  end; // while...

  fDummyCardinal := lcAccumulate;
  result := leElapsedTime;

end; // function TBandwidthBurn.ReadTest

function TBandwidthBurn.ReadTestBP32(aiArraySize : integer;
                                          var A : TBigArray): Extended;
var
  i             : integer;
  lcAccumulate : cardinal;
  li64StartCycle   : Int64;
  li64EndCycle     : Int64;
  leElapsedTime   : extended;

begin

  lcAccumulate := 0;
  leElapsedTime := 0;

  // at each step we need to repeat populating the array:
  i := 1;
  while i <= aiArraySize do begin

    // start timer
    li64StartCycle := GetCycleCount;

    // copy data: we must accumulate or optimizer will remove
    // block prefetch 16 32-byte lines:
    lcAccumulate := A[i] + lcAccumulate;
    lcAccumulate := A[i + 8] + lcAccumulate;
    lcAccumulate := A[i + 16] + lcAccumulate;
    lcAccumulate := A[i + 24] + lcAccumulate;
    lcAccumulate := A[i + 32] + lcAccumulate;
    lcAccumulate := A[i + 40] + lcAccumulate;
    lcAccumulate := A[i + 48] + lcAccumulate;
    lcAccumulate := A[i + 56] + lcAccumulate;
    lcAccumulate := A[i + 64] + lcAccumulate;
    lcAccumulate := A[i + 72] + lcAccumulate;
    lcAccumulate := A[i + 80] + lcAccumulate;
    lcAccumulate := A[i + 88] + lcAccumulate;
    lcAccumulate := A[i + 96] + lcAccumulate;
    lcAccumulate := A[i + 104] + lcAccumulate;
    lcAccumulate := A[i + 112] + lcAccumulate;
    lcAccumulate := A[i + 120] + lcAccumulate;

    // copy data: must accumulate or optimizer will remove
//    lcAccumulate := A[i] + lcAccumulate;
    lcAccumulate :=A[i+1] + lcAccumulate;
    lcAccumulate :=A[i+2] + lcAccumulate;
    lcAccumulate :=A[i+3] + lcAccumulate;
    lcAccumulate :=A[i+4] + lcAccumulate;

    lcAccumulate :=A[i+5] + lcAccumulate;
    lcAccumulate :=A[i+6] + lcAccumulate;
    lcAccumulate :=A[i+7] + lcAccumulate;
//    lcAccumulate :=A[i+8] + lcAccumulate;
    lcAccumulate :=A[i+9] + lcAccumulate;

    lcAccumulate :=A[i+10] + lcAccumulate;
    lcAccumulate :=A[i+11] + lcAccumulate;
    lcAccumulate :=A[i+12] + lcAccumulate;
    lcAccumulate :=A[i+13] + lcAccumulate;
    lcAccumulate :=A[i+14] + lcAccumulate;

    lcAccumulate :=A[i+15] + lcAccumulate;
//    lcAccumulate :=A[i+16] + lcAccumulate;
    lcAccumulate :=A[i+17] + lcAccumulate;
    lcAccumulate :=A[i+18] + lcAccumulate;
    lcAccumulate :=A[i+19] + lcAccumulate;

    lcAccumulate :=A[i+20] + lcAccumulate;
    lcAccumulate :=A[i+21] + lcAccumulate;
    lcAccumulate :=A[i+22] + lcAccumulate;
    lcAccumulate :=A[i+23] + lcAccumulate;
//    lcAccumulate :=A[i+24] + lcAccumulate;

    lcAccumulate :=A[i+25] + lcAccumulate;
    lcAccumulate :=A[i+26] + lcAccumulate;
    lcAccumulate :=A[i+27] + lcAccumulate;
    lcAccumulate :=A[i+28] + lcAccumulate;
    lcAccumulate :=A[i+29] + lcAccumulate;

    lcAccumulate :=A[i+30] + lcAccumulate;
    lcAccumulate :=A[i+31] + lcAccumulate;
//    lcAccumulate :=A[i+32] + lcAccumulate;
    lcAccumulate :=A[i+33] + lcAccumulate;
    lcAccumulate :=A[i+34] + lcAccumulate;

    lcAccumulate :=A[i+35] + lcAccumulate;
    lcAccumulate :=A[i+36] + lcAccumulate;
    lcAccumulate :=A[i+37] + lcAccumulate;
    lcAccumulate :=A[i+38] + lcAccumulate;
    lcAccumulate :=A[i+39] + lcAccumulate;

//    lcAccumulate :=A[i+40] + lcAccumulate;
    lcAccumulate :=A[i+41] + lcAccumulate;
    lcAccumulate :=A[i+42] + lcAccumulate;
    lcAccumulate :=A[i+43] + lcAccumulate;
    lcAccumulate :=A[i+44] + lcAccumulate;

    lcAccumulate :=A[i+45] + lcAccumulate;
    lcAccumulate :=A[i+46] + lcAccumulate;
    lcAccumulate :=A[i+47] + lcAccumulate;
//    lcAccumulate :=A[i+48] + lcAccumulate;
    lcAccumulate :=A[i+49] + lcAccumulate;

    lcAccumulate :=A[i+50] + lcAccumulate;
    lcAccumulate :=A[i+51] + lcAccumulate;
    lcAccumulate :=A[i+52] + lcAccumulate;
    lcAccumulate :=A[i+53] + lcAccumulate;
    lcAccumulate :=A[i+54] + lcAccumulate;

    lcAccumulate :=A[i+55] + lcAccumulate;
//    lcAccumulate :=A[i+56] + lcAccumulate;
    lcAccumulate :=A[i+57] + lcAccumulate;
    lcAccumulate :=A[i+58] + lcAccumulate;
    lcAccumulate :=A[i+59] + lcAccumulate;

    lcAccumulate :=A[i+60] + lcAccumulate;
    lcAccumulate :=A[i+61] + lcAccumulate;
    lcAccumulate :=A[i+62] + lcAccumulate;
    lcAccumulate :=A[i+63] + lcAccumulate;
//    lcAccumulate :=A[i+64] + lcAccumulate;

    lcAccumulate :=A[i+65] + lcAccumulate;
    lcAccumulate :=A[i+66] + lcAccumulate;
    lcAccumulate :=A[i+67] + lcAccumulate;
    lcAccumulate :=A[i+68] + lcAccumulate;
    lcAccumulate :=A[i+69] + lcAccumulate;

    lcAccumulate :=A[i+70] + lcAccumulate;
    lcAccumulate :=A[i+71] + lcAccumulate;
//    lcAccumulate :=A[i+72] + lcAccumulate;
    lcAccumulate :=A[i+73] + lcAccumulate;
    lcAccumulate :=A[i+74] + lcAccumulate;

    lcAccumulate :=A[i+75] + lcAccumulate;
    lcAccumulate :=A[i+76] + lcAccumulate;
    lcAccumulate :=A[i+77] + lcAccumulate;
    lcAccumulate :=A[i+78] + lcAccumulate;
    lcAccumulate :=A[i+79] + lcAccumulate;

//    lcAccumulate :=A[i+80] + lcAccumulate;
    lcAccumulate :=A[i+81] + lcAccumulate;
    lcAccumulate :=A[i+82] + lcAccumulate;
    lcAccumulate :=A[i+83] + lcAccumulate;
    lcAccumulate :=A[i+84] + lcAccumulate;

    lcAccumulate :=A[i+85] + lcAccumulate;
    lcAccumulate :=A[i+86] + lcAccumulate;
    lcAccumulate :=A[i+87] + lcAccumulate;
//    lcAccumulate :=A[i+88] + lcAccumulate;
    lcAccumulate :=A[i+89] + lcAccumulate;

    lcAccumulate :=A[i+90] + lcAccumulate;
    lcAccumulate :=A[i+91] + lcAccumulate;
    lcAccumulate :=A[i+92] + lcAccumulate;
    lcAccumulate :=A[i+93] + lcAccumulate;
    lcAccumulate :=A[i+94] + lcAccumulate;

    lcAccumulate :=A[i+95] + lcAccumulate;
//    lcAccumulate :=A[i+96] + lcAccumulate;
    lcAccumulate :=A[i+97] + lcAccumulate;
    lcAccumulate :=A[i+98] + lcAccumulate;
    lcAccumulate :=A[i+99] + lcAccumulate;

    lcAccumulate :=A[i+100] + lcAccumulate;
    lcAccumulate :=A[i+101] + lcAccumulate;
    lcAccumulate :=A[i+102] + lcAccumulate;
    lcAccumulate :=A[i+103] + lcAccumulate;
//    lcAccumulate :=A[i+104] + lcAccumulate;

    lcAccumulate :=A[i+105] + lcAccumulate;
    lcAccumulate :=A[i+106] + lcAccumulate;
    lcAccumulate :=A[i+107] + lcAccumulate;
    lcAccumulate :=A[i+108] + lcAccumulate;
    lcAccumulate :=A[i+109] + lcAccumulate;

    lcAccumulate :=A[i+110] + lcAccumulate;
    lcAccumulate :=A[i+111] + lcAccumulate;
//    lcAccumulate :=A[i+112] + lcAccumulate;
    lcAccumulate :=A[i+113] + lcAccumulate;
    lcAccumulate :=A[i+114] + lcAccumulate;

    lcAccumulate :=A[i+115] + lcAccumulate;
    lcAccumulate :=A[i+116] + lcAccumulate;
    lcAccumulate :=A[i+117] + lcAccumulate;
    lcAccumulate :=A[i+118] + lcAccumulate;
    lcAccumulate :=A[i+119] + lcAccumulate;

//    lcAccumulate :=A[i+120] + lcAccumulate;
    lcAccumulate :=A[i+121] + lcAccumulate;
    lcAccumulate :=A[i+122] + lcAccumulate;
    lcAccumulate :=A[i+123] + lcAccumulate;
    lcAccumulate :=A[i+124] + lcAccumulate;

    lcAccumulate :=A[i+125] + lcAccumulate;
    lcAccumulate :=A[i+126] + lcAccumulate;
    lcAccumulate :=A[i+127] + lcAccumulate;

    li64EndCycle := GetCycleCount;
    leElapsedTime := leElapsedTime +
      ( ( li64EndCycle - li64StartCycle ) - fSWOverhead ) /
      fStopWatch.GetCPUClockspeed(FALSE);

    i := i + 128;

  end; // while...

  fDummyCardinal := lcAccumulate;
  result := leElapsedTime;

end; // function TBandwidthBurn.ReadTestBP32

function TBandwidthBurn.ReadTestBP64(aiArraySize : integer;
                                     var A : TBigArray): Extended;
var
  i             : integer;
  lcAccumulate : cardinal;
  li64StartCycle   : Int64;
  li64EndCycle     : Int64;
  leElapsedTime   : extended;

begin

  lcAccumulate := 0;
  leElapsedTime := 0;

  // at each step we need to repeat populating the array:
  i := 1;
  while i <= aiArraySize do begin

    // start timer
    li64StartCycle := GetCycleCount;

    // copy data: we must accumulate or optimizer will remove
    // block prefetch 16 32-byte lines:
    lcAccumulate := A[i] + lcAccumulate;
    lcAccumulate := A[i + 16] + lcAccumulate;
    lcAccumulate := A[i + 32] + lcAccumulate;
    lcAccumulate := A[i + 48] + lcAccumulate;
    lcAccumulate := A[i + 64] + lcAccumulate;
    lcAccumulate := A[i + 80] + lcAccumulate;
    lcAccumulate := A[i + 96] + lcAccumulate;
    lcAccumulate := A[i + 112] + lcAccumulate;

    // copy data: must accumulate or optimizer will remove
//    lcAccumulate := A[i] + lcAccumulate;
    lcAccumulate :=A[i+1] + lcAccumulate;
    lcAccumulate :=A[i+2] + lcAccumulate;
    lcAccumulate :=A[i+3] + lcAccumulate;
    lcAccumulate :=A[i+4] + lcAccumulate;

    lcAccumulate :=A[i+5] + lcAccumulate;
    lcAccumulate :=A[i+6] + lcAccumulate;
    lcAccumulate :=A[i+7] + lcAccumulate;
    lcAccumulate :=A[i+8] + lcAccumulate;
    lcAccumulate :=A[i+9] + lcAccumulate;

    lcAccumulate :=A[i+10] + lcAccumulate;
    lcAccumulate :=A[i+11] + lcAccumulate;
    lcAccumulate :=A[i+12] + lcAccumulate;
    lcAccumulate :=A[i+13] + lcAccumulate;
    lcAccumulate :=A[i+14] + lcAccumulate;

    lcAccumulate :=A[i+15] + lcAccumulate;
//    lcAccumulate :=A[i+16] + lcAccumulate;
    lcAccumulate :=A[i+17] + lcAccumulate;
    lcAccumulate :=A[i+18] + lcAccumulate;
    lcAccumulate :=A[i+19] + lcAccumulate;

    lcAccumulate :=A[i+20] + lcAccumulate;
    lcAccumulate :=A[i+21] + lcAccumulate;
    lcAccumulate :=A[i+22] + lcAccumulate;
    lcAccumulate :=A[i+23] + lcAccumulate;
    lcAccumulate :=A[i+24] + lcAccumulate;

    lcAccumulate :=A[i+25] + lcAccumulate;
    lcAccumulate :=A[i+26] + lcAccumulate;
    lcAccumulate :=A[i+27] + lcAccumulate;
    lcAccumulate :=A[i+28] + lcAccumulate;
    lcAccumulate :=A[i+29] + lcAccumulate;

    lcAccumulate :=A[i+30] + lcAccumulate;
    lcAccumulate :=A[i+31] + lcAccumulate;
//    lcAccumulate :=A[i+32] + lcAccumulate;
    lcAccumulate :=A[i+33] + lcAccumulate;
    lcAccumulate :=A[i+34] + lcAccumulate;

    lcAccumulate :=A[i+35] + lcAccumulate;
    lcAccumulate :=A[i+36] + lcAccumulate;
    lcAccumulate :=A[i+37] + lcAccumulate;
    lcAccumulate :=A[i+38] + lcAccumulate;
    lcAccumulate :=A[i+39] + lcAccumulate;

    lcAccumulate :=A[i+40] + lcAccumulate;
    lcAccumulate :=A[i+41] + lcAccumulate;
    lcAccumulate :=A[i+42] + lcAccumulate;
    lcAccumulate :=A[i+43] + lcAccumulate;
    lcAccumulate :=A[i+44] + lcAccumulate;

    lcAccumulate :=A[i+45] + lcAccumulate;
    lcAccumulate :=A[i+46] + lcAccumulate;
    lcAccumulate :=A[i+47] + lcAccumulate;
//    lcAccumulate :=A[i+48] + lcAccumulate;
    lcAccumulate :=A[i+49] + lcAccumulate;

    lcAccumulate :=A[i+50] + lcAccumulate;
    lcAccumulate :=A[i+51] + lcAccumulate;
    lcAccumulate :=A[i+52] + lcAccumulate;
    lcAccumulate :=A[i+53] + lcAccumulate;
    lcAccumulate :=A[i+54] + lcAccumulate;

    lcAccumulate :=A[i+55] + lcAccumulate;
    lcAccumulate :=A[i+56] + lcAccumulate;
    lcAccumulate :=A[i+57] + lcAccumulate;
    lcAccumulate :=A[i+58] + lcAccumulate;
    lcAccumulate :=A[i+59] + lcAccumulate;

    lcAccumulate :=A[i+60] + lcAccumulate;
    lcAccumulate :=A[i+61] + lcAccumulate;
    lcAccumulate :=A[i+62] + lcAccumulate;
    lcAccumulate :=A[i+63] + lcAccumulate;
//    lcAccumulate :=A[i+64] + lcAccumulate;

    lcAccumulate :=A[i+65] + lcAccumulate;
    lcAccumulate :=A[i+66] + lcAccumulate;
    lcAccumulate :=A[i+67] + lcAccumulate;
    lcAccumulate :=A[i+68] + lcAccumulate;
    lcAccumulate :=A[i+69] + lcAccumulate;

    lcAccumulate :=A[i+70] + lcAccumulate;
    lcAccumulate :=A[i+71] + lcAccumulate;
    lcAccumulate :=A[i+72] + lcAccumulate;
    lcAccumulate :=A[i+73] + lcAccumulate;
    lcAccumulate :=A[i+74] + lcAccumulate;

    lcAccumulate :=A[i+75] + lcAccumulate;
    lcAccumulate :=A[i+76] + lcAccumulate;
    lcAccumulate :=A[i+77] + lcAccumulate;
    lcAccumulate :=A[i+78] + lcAccumulate;
    lcAccumulate :=A[i+79] + lcAccumulate;

//    lcAccumulate :=A[i+80] + lcAccumulate;
    lcAccumulate :=A[i+81] + lcAccumulate;
    lcAccumulate :=A[i+82] + lcAccumulate;
    lcAccumulate :=A[i+83] + lcAccumulate;
    lcAccumulate :=A[i+84] + lcAccumulate;

    lcAccumulate :=A[i+85] + lcAccumulate;
    lcAccumulate :=A[i+86] + lcAccumulate;
    lcAccumulate :=A[i+87] + lcAccumulate;
    lcAccumulate :=A[i+88] + lcAccumulate;
    lcAccumulate :=A[i+89] + lcAccumulate;

    lcAccumulate :=A[i+90] + lcAccumulate;
    lcAccumulate :=A[i+91] + lcAccumulate;
    lcAccumulate :=A[i+92] + lcAccumulate;
    lcAccumulate :=A[i+93] + lcAccumulate;
    lcAccumulate :=A[i+94] + lcAccumulate;

    lcAccumulate :=A[i+95] + lcAccumulate;
//    lcAccumulate :=A[i+96] + lcAccumulate;
    lcAccumulate :=A[i+97] + lcAccumulate;
    lcAccumulate :=A[i+98] + lcAccumulate;
    lcAccumulate :=A[i+99] + lcAccumulate;

    lcAccumulate :=A[i+100] + lcAccumulate;
    lcAccumulate :=A[i+101] + lcAccumulate;
    lcAccumulate :=A[i+102] + lcAccumulate;
    lcAccumulate :=A[i+103] + lcAccumulate;
    lcAccumulate :=A[i+104] + lcAccumulate;

    lcAccumulate :=A[i+105] + lcAccumulate;
    lcAccumulate :=A[i+106] + lcAccumulate;
    lcAccumulate :=A[i+107] + lcAccumulate;
    lcAccumulate :=A[i+108] + lcAccumulate;
    lcAccumulate :=A[i+109] + lcAccumulate;

    lcAccumulate :=A[i+110] + lcAccumulate;
    lcAccumulate :=A[i+111] + lcAccumulate;
//    lcAccumulate :=A[i+112] + lcAccumulate;
    lcAccumulate :=A[i+113] + lcAccumulate;
    lcAccumulate :=A[i+114] + lcAccumulate;

    lcAccumulate :=A[i+115] + lcAccumulate;
    lcAccumulate :=A[i+116] + lcAccumulate;
    lcAccumulate :=A[i+117] + lcAccumulate;
    lcAccumulate :=A[i+118] + lcAccumulate;
    lcAccumulate :=A[i+119] + lcAccumulate;

    lcAccumulate :=A[i+120] + lcAccumulate;
    lcAccumulate :=A[i+121] + lcAccumulate;
    lcAccumulate :=A[i+122] + lcAccumulate;
    lcAccumulate :=A[i+123] + lcAccumulate;
    lcAccumulate :=A[i+124] + lcAccumulate;

    lcAccumulate :=A[i+125] + lcAccumulate;
    lcAccumulate :=A[i+126] + lcAccumulate;
    lcAccumulate :=A[i+127] + lcAccumulate;

    li64EndCycle := GetCycleCount;
    leElapsedTime := leElapsedTime +
      ( ( li64EndCycle - li64StartCycle ) - fSWOverhead ) /
      fStopWatch.GetCPUClockspeed(FALSE);

    i := i + 128;

  end; // while...

  fDummyCardinal := lcAccumulate;
  result := leElapsedTime;

end; // function TBandwidthBurn.ReadTestBP64

function TBandwidthBurn.ReadTestBP128(aiArraySize : integer;
                                          var A : TBigArray): Extended;
var
  i             : integer;
  lcAccumulate : cardinal;
  li64StartCycle   : Int64;
  li64EndCycle     : Int64;
  leElapsedTime   : extended;

begin

  lcAccumulate := 0;
  leElapsedTime := 0;

  // at each step we need to repeat populating the array:
  i := 1;
  while i <= aiArraySize do begin

    // start timer
    li64StartCycle := GetCycleCount;

    // copy data: we must accumulate or optimizer will remove
    // block prefetch 16 32-byte lines:
    lcAccumulate := A[i] + lcAccumulate;
    lcAccumulate := A[i + 32] + lcAccumulate;
    lcAccumulate := A[i + 64] + lcAccumulate;
    lcAccumulate := A[i + 96] + lcAccumulate;

    // copy data: must accumulate or optimizer will remove
//    lcAccumulate := A[i] + lcAccumulate;
    lcAccumulate :=A[i+1] + lcAccumulate;
    lcAccumulate :=A[i+2] + lcAccumulate;
    lcAccumulate :=A[i+3] + lcAccumulate;
    lcAccumulate :=A[i+4] + lcAccumulate;

    lcAccumulate :=A[i+5] + lcAccumulate;
    lcAccumulate :=A[i+6] + lcAccumulate;
    lcAccumulate :=A[i+7] + lcAccumulate;
    lcAccumulate :=A[i+8] + lcAccumulate;
    lcAccumulate :=A[i+9] + lcAccumulate;

    lcAccumulate :=A[i+10] + lcAccumulate;
    lcAccumulate :=A[i+11] + lcAccumulate;
    lcAccumulate :=A[i+12] + lcAccumulate;
    lcAccumulate :=A[i+13] + lcAccumulate;
    lcAccumulate :=A[i+14] + lcAccumulate;

    lcAccumulate :=A[i+15] + lcAccumulate;
    lcAccumulate :=A[i+16] + lcAccumulate;
    lcAccumulate :=A[i+17] + lcAccumulate;
    lcAccumulate :=A[i+18] + lcAccumulate;
    lcAccumulate :=A[i+19] + lcAccumulate;

    lcAccumulate :=A[i+20] + lcAccumulate;
    lcAccumulate :=A[i+21] + lcAccumulate;
    lcAccumulate :=A[i+22] + lcAccumulate;
    lcAccumulate :=A[i+23] + lcAccumulate;
    lcAccumulate :=A[i+24] + lcAccumulate;

    lcAccumulate :=A[i+25] + lcAccumulate;
    lcAccumulate :=A[i+26] + lcAccumulate;
    lcAccumulate :=A[i+27] + lcAccumulate;
    lcAccumulate :=A[i+28] + lcAccumulate;
    lcAccumulate :=A[i+29] + lcAccumulate;

    lcAccumulate :=A[i+30] + lcAccumulate;
    lcAccumulate :=A[i+31] + lcAccumulate;
//    lcAccumulate :=A[i+32] + lcAccumulate;
    lcAccumulate :=A[i+33] + lcAccumulate;
    lcAccumulate :=A[i+34] + lcAccumulate;

    lcAccumulate :=A[i+35] + lcAccumulate;
    lcAccumulate :=A[i+36] + lcAccumulate;
    lcAccumulate :=A[i+37] + lcAccumulate;
    lcAccumulate :=A[i+38] + lcAccumulate;
    lcAccumulate :=A[i+39] + lcAccumulate;

    lcAccumulate :=A[i+40] + lcAccumulate;
    lcAccumulate :=A[i+41] + lcAccumulate;
    lcAccumulate :=A[i+42] + lcAccumulate;
    lcAccumulate :=A[i+43] + lcAccumulate;
    lcAccumulate :=A[i+44] + lcAccumulate;

    lcAccumulate :=A[i+45] + lcAccumulate;
    lcAccumulate :=A[i+46] + lcAccumulate;
    lcAccumulate :=A[i+47] + lcAccumulate;
    lcAccumulate :=A[i+48] + lcAccumulate;
    lcAccumulate :=A[i+49] + lcAccumulate;

    lcAccumulate :=A[i+50] + lcAccumulate;
    lcAccumulate :=A[i+51] + lcAccumulate;
    lcAccumulate :=A[i+52] + lcAccumulate;
    lcAccumulate :=A[i+53] + lcAccumulate;
    lcAccumulate :=A[i+54] + lcAccumulate;

    lcAccumulate :=A[i+55] + lcAccumulate;
    lcAccumulate :=A[i+56] + lcAccumulate;
    lcAccumulate :=A[i+57] + lcAccumulate;
    lcAccumulate :=A[i+58] + lcAccumulate;
    lcAccumulate :=A[i+59] + lcAccumulate;

    lcAccumulate :=A[i+60] + lcAccumulate;
    lcAccumulate :=A[i+61] + lcAccumulate;
    lcAccumulate :=A[i+62] + lcAccumulate;
    lcAccumulate :=A[i+63] + lcAccumulate;
//    lcAccumulate :=A[i+64] + lcAccumulate;

    lcAccumulate :=A[i+65] + lcAccumulate;
    lcAccumulate :=A[i+66] + lcAccumulate;
    lcAccumulate :=A[i+67] + lcAccumulate;
    lcAccumulate :=A[i+68] + lcAccumulate;
    lcAccumulate :=A[i+69] + lcAccumulate;

    lcAccumulate :=A[i+70] + lcAccumulate;
    lcAccumulate :=A[i+71] + lcAccumulate;
    lcAccumulate :=A[i+72] + lcAccumulate;
    lcAccumulate :=A[i+73] + lcAccumulate;
    lcAccumulate :=A[i+74] + lcAccumulate;

    lcAccumulate :=A[i+75] + lcAccumulate;
    lcAccumulate :=A[i+76] + lcAccumulate;
    lcAccumulate :=A[i+77] + lcAccumulate;
    lcAccumulate :=A[i+78] + lcAccumulate;
    lcAccumulate :=A[i+79] + lcAccumulate;

    lcAccumulate :=A[i+80] + lcAccumulate;
    lcAccumulate :=A[i+81] + lcAccumulate;
    lcAccumulate :=A[i+82] + lcAccumulate;
    lcAccumulate :=A[i+83] + lcAccumulate;
    lcAccumulate :=A[i+84] + lcAccumulate;

    lcAccumulate :=A[i+85] + lcAccumulate;
    lcAccumulate :=A[i+86] + lcAccumulate;
    lcAccumulate :=A[i+87] + lcAccumulate;
    lcAccumulate :=A[i+88] + lcAccumulate;
    lcAccumulate :=A[i+89] + lcAccumulate;

    lcAccumulate :=A[i+90] + lcAccumulate;
    lcAccumulate :=A[i+91] + lcAccumulate;
    lcAccumulate :=A[i+92] + lcAccumulate;
    lcAccumulate :=A[i+93] + lcAccumulate;
    lcAccumulate :=A[i+94] + lcAccumulate;

    lcAccumulate :=A[i+95] + lcAccumulate;
//    lcAccumulate :=A[i+96] + lcAccumulate;
    lcAccumulate :=A[i+97] + lcAccumulate;
    lcAccumulate :=A[i+98] + lcAccumulate;
    lcAccumulate :=A[i+99] + lcAccumulate;

    lcAccumulate :=A[i+100] + lcAccumulate;
    lcAccumulate :=A[i+101] + lcAccumulate;
    lcAccumulate :=A[i+102] + lcAccumulate;
    lcAccumulate :=A[i+103] + lcAccumulate;
    lcAccumulate :=A[i+104] + lcAccumulate;

    lcAccumulate :=A[i+105] + lcAccumulate;
    lcAccumulate :=A[i+106] + lcAccumulate;
    lcAccumulate :=A[i+107] + lcAccumulate;
    lcAccumulate :=A[i+108] + lcAccumulate;
    lcAccumulate :=A[i+109] + lcAccumulate;

    lcAccumulate :=A[i+110] + lcAccumulate;
    lcAccumulate :=A[i+111] + lcAccumulate;
    lcAccumulate :=A[i+112] + lcAccumulate;
    lcAccumulate :=A[i+113] + lcAccumulate;
    lcAccumulate :=A[i+114] + lcAccumulate;

    lcAccumulate :=A[i+115] + lcAccumulate;
    lcAccumulate :=A[i+116] + lcAccumulate;
    lcAccumulate :=A[i+117] + lcAccumulate;
    lcAccumulate :=A[i+118] + lcAccumulate;
    lcAccumulate :=A[i+119] + lcAccumulate;

    lcAccumulate :=A[i+120] + lcAccumulate;
    lcAccumulate :=A[i+121] + lcAccumulate;
    lcAccumulate :=A[i+122] + lcAccumulate;
    lcAccumulate :=A[i+123] + lcAccumulate;
    lcAccumulate :=A[i+124] + lcAccumulate;

    lcAccumulate :=A[i+125] + lcAccumulate;
    lcAccumulate :=A[i+126] + lcAccumulate;
    lcAccumulate :=A[i+127] + lcAccumulate;

    li64EndCycle := GetCycleCount;
    leElapsedTime := leElapsedTime +
      ( ( li64EndCycle - li64StartCycle ) - fSWOverhead ) /
      fStopWatch.GetCPUClockspeed(FALSE);

    i := i + 128;

  end; // while...

  fDummyCardinal := lcAccumulate;
  result := leElapsedTime;

end; // function TBandwidthBurn.ReadTestBP128

function TBandwidthBurn.ReadTestIterate(aiRepeatCount : integer;
                                        aiArraySize   : integer;
                                          var A : TBigArray
                                        ): Extended;
var
  i                 : integer;
  leElapsedTime   : extended;
  le_block_time     : extended;
  leMinTime : extended;
begin
  // initialize variables
  leElapsedTime := 0;
  leMinTime := 99999999999999;
  for i := 1 to aiRepeatCount do begin
    le_block_time := ReadTest( aiArraySize, A );
    leElapsedTime:= leElapsedTime + le_block_time;
    if leMinTime > leElapsedTime then begin
      leMinTime := leElapsedTime;
    end; // if
    leElapsedTime := 0;
  end;
  result := leMinTime;
end; // function TBandwidthBurn.ReadTest

function TBandwidthBurn.ReadTestIterateBP32(aiRepeatCount : integer;
                                        aiArraySize   : integer;
                                          var A : TBigArray
                                        ): Extended;
var
  i                 : integer;
  leElapsedTime   : extended;
  le_block_time     : extended;
  leMinTime : extended;
begin
  // initialize variables
  leElapsedTime := 0;
  leMinTime := 99999999999999;
  for i := 1 to aiRepeatCount do begin
    le_block_time := ReadTestBP32( aiArraySize, A );
    leElapsedTime:= leElapsedTime + le_block_time;
    if leMinTime > leElapsedTime then begin
      leMinTime := leElapsedTime;
    end; // if
    leElapsedTime := 0;
  end;
  result := leMinTime;
end; // function TBandwidthBurn.ReadTestBP32

function TBandwidthBurn.ReadTestIterateBP64(aiRepeatCount : integer;
                                        aiArraySize   : integer;
                                          var A : TBigArray
                                        ): Extended;
var
  i                 : integer;
  leElapsedTime   : extended;
  le_block_time     : extended;
  leMinTime : extended;
begin
  // initialize variables
  leElapsedTime := 0;
  leMinTime := 99999999999999;
  for i := 1 to aiRepeatCount do begin
    le_block_time := ReadTestBP64( aiArraySize, A );
    leElapsedTime:= leElapsedTime + le_block_time;
    if leMinTime > leElapsedTime then begin
      leMinTime := leElapsedTime;
    end; // if
    leElapsedTime := 0;
  end;
  result := leMinTime;
end; // function TBandwidthBurn.ReadTestBP64

function TBandwidthBurn.ReadTestIterateBP128(aiRepeatCount : integer;
                                        aiArraySize   : integer;
                                          var A : TBigArray
                                        ): Extended;
var
  i                 : integer;
  leElapsedTime   : extended;
  le_block_time     : extended;
  leMinTime : extended;
begin
  // initialize variables
  leElapsedTime := 0;
  leMinTime := 99999999999999;
  for i := 1 to aiRepeatCount do begin
    le_block_time := ReadTestBP128( aiArraySize, A );
    leElapsedTime:= leElapsedTime + le_block_time;
    if leMinTime > leElapsedTime then begin
      leMinTime := leElapsedTime;
    end; // if
    leElapsedTime := 0;
  end;
  result := leMinTime;
end; // function TBandwidthBurn.ReadTestBP128

function TBandwidthBurn.WriteTest(aiArraySize   : integer;
                                  ac_WriteValue  : cardinal;
                                          var A : TBigArray
                                  ): Extended;
var
  li64StartCycle   : Int64;
  li64EndCycle     : Int64;
  leElapsedTime   : extended;
  i                 : integer;
begin

  leElapsedTime := 0;

  // at each step we need to repeat populating the array:
  i := 1;
  while i <= aiArraySize do begin

    // start timer
    li64StartCycle := GetCycleCount;

    // copy data
     A[i] := ac_WriteValue;
     A[i + 1] := ac_WriteValue;
     A[i + 2] := ac_WriteValue;
     A[i + 3] := ac_WriteValue;
     A[i + 4] := ac_WriteValue;

     A[i + 5] := ac_WriteValue;
     A[i + 6] := ac_WriteValue;
     A[i + 7] := ac_WriteValue;
     A[i + 8] := ac_WriteValue;
     A[i + 9] := ac_WriteValue;

     A[i + 10] := ac_WriteValue;
     A[i + 11] := ac_WriteValue;
     A[i + 12] := ac_WriteValue;
     A[i + 13] := ac_WriteValue;
     A[i + 14] := ac_WriteValue;

     A[i + 15] := ac_WriteValue;
     A[i + 16] := ac_WriteValue;
     A[i + 17] := ac_WriteValue;
     A[i + 18] := ac_WriteValue;
     A[i + 19] := ac_WriteValue;

     A[i + 20] := ac_WriteValue;
     A[i + 21] := ac_WriteValue;
     A[i + 22] := ac_WriteValue;
     A[i + 23] := ac_WriteValue;
     A[i + 24] := ac_WriteValue;

     A[i + 25] := ac_WriteValue;
     A[i + 26] := ac_WriteValue;
     A[i + 27] := ac_WriteValue;
     A[i + 28] := ac_WriteValue;
     A[i + 29] := ac_WriteValue;

     A[i + 30] := ac_WriteValue;
     A[i + 31] := ac_WriteValue;
     A[i + 32] := ac_WriteValue;
     A[i + 33] := ac_WriteValue;
     A[i + 34] := ac_WriteValue;

     A[i + 35] := ac_WriteValue;
     A[i + 36] := ac_WriteValue;
     A[i + 37] := ac_WriteValue;
     A[i + 38] := ac_WriteValue;
     A[i + 39] := ac_WriteValue;

     A[i + 40] := ac_WriteValue;
     A[i + 41] := ac_WriteValue;
     A[i + 42] := ac_WriteValue;
     A[i + 43] := ac_WriteValue;
     A[i + 44] := ac_WriteValue;

     A[i + 45] := ac_WriteValue;
     A[i + 46] := ac_WriteValue;
     A[i + 47] := ac_WriteValue;
     A[i + 48] := ac_WriteValue;
     A[i + 49] := ac_WriteValue;

     A[i + 50] := ac_WriteValue;
     A[i + 51] := ac_WriteValue;
     A[i + 52] := ac_WriteValue;
     A[i + 53] := ac_WriteValue;
     A[i + 54] := ac_WriteValue;

     A[i + 55] := ac_WriteValue;
     A[i + 56] := ac_WriteValue;
     A[i + 57] := ac_WriteValue;
     A[i + 58] := ac_WriteValue;
     A[i + 59] := ac_WriteValue;

     A[i + 60] := ac_WriteValue;
     A[i + 61] := ac_WriteValue;
     A[i + 62] := ac_WriteValue;
     A[i + 63] := ac_WriteValue;
     A[i + 64] := ac_WriteValue;

     A[i + 65] := ac_WriteValue;
     A[i + 66] := ac_WriteValue;
     A[i + 67] := ac_WriteValue;
     A[i + 68] := ac_WriteValue;
     A[i + 69] := ac_WriteValue;

     A[i + 70] := ac_WriteValue;
     A[i + 71] := ac_WriteValue;
     A[i + 72] := ac_WriteValue;
     A[i + 73] := ac_WriteValue;
     A[i + 74] := ac_WriteValue;

     A[i + 75] := ac_WriteValue;
     A[i + 76] := ac_WriteValue;
     A[i + 77] := ac_WriteValue;
     A[i + 78] := ac_WriteValue;
     A[i + 79] := ac_WriteValue;

     A[i + 80] := ac_WriteValue;
     A[i + 81] := ac_WriteValue;
     A[i + 82] := ac_WriteValue;
     A[i + 83] := ac_WriteValue;
     A[i + 84] := ac_WriteValue;

     A[i + 85] := ac_WriteValue;
     A[i + 86] := ac_WriteValue;
     A[i + 87] := ac_WriteValue;
     A[i + 88] := ac_WriteValue;
     A[i + 89] := ac_WriteValue;

     A[i + 90] := ac_WriteValue;
     A[i + 91] := ac_WriteValue;
     A[i + 92] := ac_WriteValue;
     A[i + 93] := ac_WriteValue;
     A[i + 94] := ac_WriteValue;

     A[i + 95] := ac_WriteValue;
     A[i + 96] := ac_WriteValue;
     A[i + 97] := ac_WriteValue;
     A[i + 98] := ac_WriteValue;
     A[i + 99] := ac_WriteValue;

     A[i + 100] := ac_WriteValue;
     A[i + 101] := ac_WriteValue;
     A[i + 102] := ac_WriteValue;
     A[i + 103] := ac_WriteValue;
     A[i + 104] := ac_WriteValue;

     A[i + 105] := ac_WriteValue;
     A[i + 106] := ac_WriteValue;
     A[i + 107] := ac_WriteValue;
     A[i + 108] := ac_WriteValue;
     A[i + 109] := ac_WriteValue;

     A[i + 110] := ac_WriteValue;
     A[i + 111] := ac_WriteValue;
     A[i + 112] := ac_WriteValue;
     A[i + 113] := ac_WriteValue;
     A[i + 114] := ac_WriteValue;

     A[i + 115] := ac_WriteValue;
     A[i + 116] := ac_WriteValue;
     A[i + 117] := ac_WriteValue;
     A[i + 118] := ac_WriteValue;
     A[i + 119] := ac_WriteValue;

     A[i + 120] := ac_WriteValue;
     A[i + 121] := ac_WriteValue;
     A[i + 122] := ac_WriteValue;
     A[i + 123] := ac_WriteValue;
     A[i + 124] := ac_WriteValue;

     A[i + 125] := ac_WriteValue;
     A[i + 126] := ac_WriteValue;
     A[i + 127] := ac_WriteValue;

    li64EndCycle := GetCycleCount;
    leElapsedTime := leElapsedTime +
      ( ( li64EndCycle - li64StartCycle ) - fSWOverhead ) /
      StopWatch.GetCPUClockspeed(FALSE);

    i := i + 128;

  end; // while...

  result := leElapsedTime;

end; // function TBandwidthBurn.WriteTest

function TBandwidthBurn.WriteTestBP32(aiArraySize   : integer;
                                  ac_WriteValue  : cardinal;
                                          var A : TBigArray
                                  ): Extended;
var
  li64StartCycle   : Int64;
  li64EndCycle     : Int64;
  leElapsedTime   : extended;
  i                 : integer;
begin

  leElapsedTime := 0;

  // at each step we need to repeat populating the array:
  i := 1;
  while i <= aiArraySize do begin

    // start timer
    li64StartCycle := GetCycleCount;

    // block prefetch 16 32-byte lines:
    A[i] := ac_WriteValue;
    A[i + 8] := ac_WriteValue;
    A[i + 16] := ac_WriteValue;
    A[i + 24] := ac_WriteValue;
    A[i + 32] := ac_WriteValue;
    A[i + 40] := ac_WriteValue;
    A[i + 48] := ac_WriteValue;
    A[i + 56] := ac_WriteValue;
    A[i + 64] := ac_WriteValue;
    A[i + 72] := ac_WriteValue;
    A[i + 80] := ac_WriteValue;
    A[i + 88] := ac_WriteValue;
    A[i + 96] := ac_WriteValue;
    A[i + 104] := ac_WriteValue;
    A[i + 112] := ac_WriteValue;
    A[i + 120] := ac_WriteValue;

    // copy data
//    A[i] := ac_WriteValue;
    A[i + 1] := ac_WriteValue;
    A[i + 2] := ac_WriteValue;
    A[i + 3] := ac_WriteValue;
    A[i + 4] := ac_WriteValue;

    A[i + 5] := ac_WriteValue;
    A[i + 6] := ac_WriteValue;
    A[i + 7] := ac_WriteValue;
//    A[i + 8] := ac_WriteValue;
    A[i + 9] := ac_WriteValue;

    A[i + 10] := ac_WriteValue;
    A[i + 11] := ac_WriteValue;
    A[i + 12] := ac_WriteValue;
    A[i + 13] := ac_WriteValue;
    A[i + 14] := ac_WriteValue;

    A[i + 15] := ac_WriteValue;
//    A[i + 16] := ac_WriteValue;
    A[i + 17] := ac_WriteValue;
    A[i + 18] := ac_WriteValue;
    A[i + 19] := ac_WriteValue;

    A[i + 20] := ac_WriteValue;
    A[i + 21] := ac_WriteValue;
    A[i + 22] := ac_WriteValue;
    A[i + 23] := ac_WriteValue;
//    A[i + 24] := ac_WriteValue;

    A[i + 25] := ac_WriteValue;
    A[i + 26] := ac_WriteValue;
    A[i + 27] := ac_WriteValue;
    A[i + 28] := ac_WriteValue;
    A[i + 29] := ac_WriteValue;

    A[i + 30] := ac_WriteValue;
    A[i + 31] := ac_WriteValue;
//    A[i + 32] := ac_WriteValue;
    A[i + 33] := ac_WriteValue;
    A[i + 34] := ac_WriteValue;

    A[i + 35] := ac_WriteValue;
    A[i + 36] := ac_WriteValue;
    A[i + 37] := ac_WriteValue;
    A[i + 38] := ac_WriteValue;
    A[i + 39] := ac_WriteValue;

//    A[i + 40] := ac_WriteValue;
    A[i + 41] := ac_WriteValue;
    A[i + 42] := ac_WriteValue;
    A[i + 43] := ac_WriteValue;
    A[i + 44] := ac_WriteValue;

    A[i + 45] := ac_WriteValue;
    A[i + 46] := ac_WriteValue;
    A[i + 47] := ac_WriteValue;
//    A[i + 48] := ac_WriteValue;
    A[i + 49] := ac_WriteValue;

    A[i + 50] := ac_WriteValue;
    A[i + 51] := ac_WriteValue;
    A[i + 52] := ac_WriteValue;
    A[i + 53] := ac_WriteValue;
    A[i + 54] := ac_WriteValue;

    A[i + 55] := ac_WriteValue;
//    A[i + 56] := ac_WriteValue;
    A[i + 57] := ac_WriteValue;
    A[i + 58] := ac_WriteValue;
    A[i + 59] := ac_WriteValue;

    A[i + 60] := ac_WriteValue;
    A[i + 61] := ac_WriteValue;
    A[i + 62] := ac_WriteValue;
    A[i + 63] := ac_WriteValue;
//    A[i + 64] := ac_WriteValue;

    A[i + 65] := ac_WriteValue;
    A[i + 66] := ac_WriteValue;
    A[i + 67] := ac_WriteValue;
    A[i + 68] := ac_WriteValue;
    A[i + 69] := ac_WriteValue;

    A[i + 70] := ac_WriteValue;
    A[i + 71] := ac_WriteValue;
//    A[i + 72] := ac_WriteValue;
    A[i + 73] := ac_WriteValue;
    A[i + 74] := ac_WriteValue;

    A[i + 75] := ac_WriteValue;
    A[i + 76] := ac_WriteValue;
    A[i + 77] := ac_WriteValue;
    A[i + 78] := ac_WriteValue;
    A[i + 79] := ac_WriteValue;

//    A[i + 80] := ac_WriteValue;
    A[i + 81] := ac_WriteValue;
    A[i + 82] := ac_WriteValue;
    A[i + 83] := ac_WriteValue;
    A[i + 84] := ac_WriteValue;

    A[i + 85] := ac_WriteValue;
    A[i + 86] := ac_WriteValue;
    A[i + 87] := ac_WriteValue;
//    A[i + 88] := ac_WriteValue;
    A[i + 89] := ac_WriteValue;

    A[i + 90] := ac_WriteValue;
    A[i + 91] := ac_WriteValue;
    A[i + 92] := ac_WriteValue;
    A[i + 93] := ac_WriteValue;
    A[i + 94] := ac_WriteValue;

    A[i + 95] := ac_WriteValue;
//    A[i + 96] := ac_WriteValue;
    A[i + 97] := ac_WriteValue;
    A[i + 98] := ac_WriteValue;
    A[i + 99] := ac_WriteValue;

    A[i + 100] := ac_WriteValue;
    A[i + 101] := ac_WriteValue;
    A[i + 102] := ac_WriteValue;
    A[i + 103] := ac_WriteValue;
//    A[i + 104] := ac_WriteValue;

    A[i + 105] := ac_WriteValue;
    A[i + 106] := ac_WriteValue;
    A[i + 107] := ac_WriteValue;
    A[i + 108] := ac_WriteValue;
    A[i + 109] := ac_WriteValue;

    A[i + 110] := ac_WriteValue;
    A[i + 111] := ac_WriteValue;
//    A[i + 112] := ac_WriteValue;
    A[i + 113] := ac_WriteValue;
    A[i + 114] := ac_WriteValue;

    A[i + 115] := ac_WriteValue;
    A[i + 116] := ac_WriteValue;
    A[i + 117] := ac_WriteValue;
    A[i + 118] := ac_WriteValue;
    A[i + 119] := ac_WriteValue;

//    A[i + 120] := ac_WriteValue;
    A[i + 121] := ac_WriteValue;
    A[i + 122] := ac_WriteValue;
    A[i + 123] := ac_WriteValue;
    A[i + 124] := ac_WriteValue;

    A[i + 125] := ac_WriteValue;
    A[i + 126] := ac_WriteValue;
    A[i + 127] := ac_WriteValue;

    li64EndCycle := GetCycleCount;
    leElapsedTime := leElapsedTime +
      ( ( li64EndCycle - li64StartCycle ) - fSWOverhead ) /
      StopWatch.GetCPUClockspeed(FALSE);

    i := i + 128;

  end; // while...

  result := leElapsedTime;

end; // function TBandwidthBurn.WriteTestBP32


function TBandwidthBurn.WriteTestBP64(aiArraySize   : integer;
                                  ac_WriteValue  : cardinal;
                                  var A : TBigArray
                                  ): Extended;
var
  li64StartCycle   : Int64;
  li64EndCycle     : Int64;
  leElapsedTime   : extended;
  i                 : integer;
begin

  leElapsedTime := 0;

  // at each step we need to repeat populating the array:
  i := 1;
  while i <= aiArraySize do begin

    // start timer
    li64StartCycle := GetCycleCount;

    // block prefetch 16 32-byte lines:
    A[i] := ac_WriteValue;
    A[i + 16] := ac_WriteValue;
    A[i + 32] := ac_WriteValue;
    A[i + 48] := ac_WriteValue;
    A[i + 64] := ac_WriteValue;
    A[i + 80] := ac_WriteValue;
    A[i + 96] := ac_WriteValue;
    A[i + 112] := ac_WriteValue;

    // copy data
//     A[i] := ac_WriteValue;
     A[i + 1] := ac_WriteValue;
     A[i + 2] := ac_WriteValue;
     A[i + 3] := ac_WriteValue;
     A[i + 4] := ac_WriteValue;

     A[i + 5] := ac_WriteValue;
     A[i + 6] := ac_WriteValue;
     A[i + 7] := ac_WriteValue;
     A[i + 8] := ac_WriteValue;
     A[i + 9] := ac_WriteValue;

     A[i + 10] := ac_WriteValue;
     A[i + 11] := ac_WriteValue;
     A[i + 12] := ac_WriteValue;
     A[i + 13] := ac_WriteValue;
     A[i + 14] := ac_WriteValue;

     A[i + 15] := ac_WriteValue;
//     A[i + 16] := ac_WriteValue;
     A[i + 17] := ac_WriteValue;
     A[i + 18] := ac_WriteValue;
     A[i + 19] := ac_WriteValue;

     A[i + 20] := ac_WriteValue;
     A[i + 21] := ac_WriteValue;
     A[i + 22] := ac_WriteValue;
     A[i + 23] := ac_WriteValue;
     A[i + 24] := ac_WriteValue;

     A[i + 25] := ac_WriteValue;
     A[i + 26] := ac_WriteValue;
     A[i + 27] := ac_WriteValue;
     A[i + 28] := ac_WriteValue;
     A[i + 29] := ac_WriteValue;

     A[i + 30] := ac_WriteValue;
     A[i + 31] := ac_WriteValue;
//     A[i + 32] := ac_WriteValue;
     A[i + 33] := ac_WriteValue;
     A[i + 34] := ac_WriteValue;

     A[i + 35] := ac_WriteValue;
     A[i + 36] := ac_WriteValue;
     A[i + 37] := ac_WriteValue;
     A[i + 38] := ac_WriteValue;
     A[i + 39] := ac_WriteValue;

     A[i + 40] := ac_WriteValue;
     A[i + 41] := ac_WriteValue;
     A[i + 42] := ac_WriteValue;
     A[i + 43] := ac_WriteValue;
     A[i + 44] := ac_WriteValue;

     A[i + 45] := ac_WriteValue;
     A[i + 46] := ac_WriteValue;
     A[i + 47] := ac_WriteValue;
//     A[i + 48] := ac_WriteValue;
     A[i + 49] := ac_WriteValue;

     A[i + 50] := ac_WriteValue;
     A[i + 51] := ac_WriteValue;
     A[i + 52] := ac_WriteValue;
     A[i + 53] := ac_WriteValue;
     A[i + 54] := ac_WriteValue;

     A[i + 55] := ac_WriteValue;
     A[i + 56] := ac_WriteValue;
     A[i + 57] := ac_WriteValue;
     A[i + 58] := ac_WriteValue;
     A[i + 59] := ac_WriteValue;

     A[i + 60] := ac_WriteValue;
     A[i + 61] := ac_WriteValue;
     A[i + 62] := ac_WriteValue;
     A[i + 63] := ac_WriteValue;
//     A[i + 64] := ac_WriteValue;

     A[i + 65] := ac_WriteValue;
     A[i + 66] := ac_WriteValue;
     A[i + 67] := ac_WriteValue;
     A[i + 68] := ac_WriteValue;
     A[i + 69] := ac_WriteValue;

     A[i + 70] := ac_WriteValue;
     A[i + 71] := ac_WriteValue;
     A[i + 72] := ac_WriteValue;
     A[i + 73] := ac_WriteValue;
     A[i + 74] := ac_WriteValue;

     A[i + 75] := ac_WriteValue;
     A[i + 76] := ac_WriteValue;
     A[i + 77] := ac_WriteValue;
     A[i + 78] := ac_WriteValue;
     A[i + 79] := ac_WriteValue;

//     A[i + 80] := ac_WriteValue;
     A[i + 81] := ac_WriteValue;
     A[i + 82] := ac_WriteValue;
     A[i + 83] := ac_WriteValue;
     A[i + 84] := ac_WriteValue;

     A[i + 85] := ac_WriteValue;
     A[i + 86] := ac_WriteValue;
     A[i + 87] := ac_WriteValue;
     A[i + 88] := ac_WriteValue;
     A[i + 89] := ac_WriteValue;

     A[i + 90] := ac_WriteValue;
     A[i + 91] := ac_WriteValue;
     A[i + 92] := ac_WriteValue;
     A[i + 93] := ac_WriteValue;
     A[i + 94] := ac_WriteValue;

     A[i + 95] := ac_WriteValue;
//     A[i + 96] := ac_WriteValue;
     A[i + 97] := ac_WriteValue;
     A[i + 98] := ac_WriteValue;
     A[i + 99] := ac_WriteValue;

     A[i + 100] := ac_WriteValue;
     A[i + 101] := ac_WriteValue;
     A[i + 102] := ac_WriteValue;
     A[i + 103] := ac_WriteValue;
     A[i + 104] := ac_WriteValue;

     A[i + 105] := ac_WriteValue;
     A[i + 106] := ac_WriteValue;
     A[i + 107] := ac_WriteValue;
     A[i + 108] := ac_WriteValue;
     A[i + 109] := ac_WriteValue;

     A[i + 110] := ac_WriteValue;
     A[i + 111] := ac_WriteValue;
//     A[i + 112] := ac_WriteValue;
     A[i + 113] := ac_WriteValue;
     A[i + 114] := ac_WriteValue;

     A[i + 115] := ac_WriteValue;
     A[i + 116] := ac_WriteValue;
     A[i + 117] := ac_WriteValue;
     A[i + 118] := ac_WriteValue;
     A[i + 119] := ac_WriteValue;

     A[i + 120] := ac_WriteValue;
     A[i + 121] := ac_WriteValue;
     A[i + 122] := ac_WriteValue;
     A[i + 123] := ac_WriteValue;
     A[i + 124] := ac_WriteValue;

     A[i + 125] := ac_WriteValue;
     A[i + 126] := ac_WriteValue;
     A[i + 127] := ac_WriteValue;

    li64EndCycle := GetCycleCount;
    leElapsedTime := leElapsedTime +
      ( ( li64EndCycle - li64StartCycle ) - fSWOverhead ) /
      StopWatch.GetCPUClockspeed(FALSE);

    i := i + 128;

  end; // while...

  result := leElapsedTime;

end; // function TBandwidthBurn.WriteTestBP64

function TBandwidthBurn.WriteTestBP128(aiArraySize   : integer;
                                  ac_WriteValue  : cardinal;
                                          var A : TBigArray
                                  ): Extended;
var
  li64StartCycle   : Int64;
  li64EndCycle     : Int64;
  leElapsedTime   : extended;
  i                 : integer;
begin

  leElapsedTime := 0;

  // at each step we need to repeat populating the array:
  i := 1;
  while i <= aiArraySize do begin

    // start timer
    li64StartCycle := GetCycleCount;

    // block prefetch 4 128-byte lines:
    A[i] := ac_WriteValue;
    A[i + 32] := ac_WriteValue;
    A[i + 64] := ac_WriteValue;
    A[i + 96] := ac_WriteValue;

    // copy data
//     A[i] := ac_WriteValue;
     A[i + 1] := ac_WriteValue;
     A[i + 2] := ac_WriteValue;
     A[i + 3] := ac_WriteValue;
     A[i + 4] := ac_WriteValue;

     A[i + 5] := ac_WriteValue;
     A[i + 6] := ac_WriteValue;
     A[i + 7] := ac_WriteValue;
     A[i + 8] := ac_WriteValue;
     A[i + 9] := ac_WriteValue;

     A[i + 10] := ac_WriteValue;
     A[i + 11] := ac_WriteValue;
     A[i + 12] := ac_WriteValue;
     A[i + 13] := ac_WriteValue;
     A[i + 14] := ac_WriteValue;

     A[i + 15] := ac_WriteValue;
     A[i + 16] := ac_WriteValue;
     A[i + 17] := ac_WriteValue;
     A[i + 18] := ac_WriteValue;
     A[i + 19] := ac_WriteValue;

     A[i + 20] := ac_WriteValue;
     A[i + 21] := ac_WriteValue;
     A[i + 22] := ac_WriteValue;
     A[i + 23] := ac_WriteValue;
     A[i + 24] := ac_WriteValue;

     A[i + 25] := ac_WriteValue;
     A[i + 26] := ac_WriteValue;
     A[i + 27] := ac_WriteValue;
     A[i + 28] := ac_WriteValue;
     A[i + 29] := ac_WriteValue;

     A[i + 30] := ac_WriteValue;
     A[i + 31] := ac_WriteValue;
//     A[i + 32] := ac_WriteValue;
     A[i + 33] := ac_WriteValue;
     A[i + 34] := ac_WriteValue;

     A[i + 35] := ac_WriteValue;
     A[i + 36] := ac_WriteValue;
     A[i + 37] := ac_WriteValue;
     A[i + 38] := ac_WriteValue;
     A[i + 39] := ac_WriteValue;

     A[i + 40] := ac_WriteValue;
     A[i + 41] := ac_WriteValue;
     A[i + 42] := ac_WriteValue;
     A[i + 43] := ac_WriteValue;
     A[i + 44] := ac_WriteValue;

     A[i + 45] := ac_WriteValue;
     A[i + 46] := ac_WriteValue;
     A[i + 47] := ac_WriteValue;
     A[i + 48] := ac_WriteValue;
     A[i + 49] := ac_WriteValue;

     A[i + 50] := ac_WriteValue;
     A[i + 51] := ac_WriteValue;
     A[i + 52] := ac_WriteValue;
     A[i + 53] := ac_WriteValue;
     A[i + 54] := ac_WriteValue;

     A[i + 55] := ac_WriteValue;
     A[i + 56] := ac_WriteValue;
     A[i + 57] := ac_WriteValue;
     A[i + 58] := ac_WriteValue;
     A[i + 59] := ac_WriteValue;

     A[i + 60] := ac_WriteValue;
     A[i + 61] := ac_WriteValue;
     A[i + 62] := ac_WriteValue;
     A[i + 63] := ac_WriteValue;
//     A[i + 64] := ac_WriteValue;

     A[i + 65] := ac_WriteValue;
     A[i + 66] := ac_WriteValue;
     A[i + 67] := ac_WriteValue;
     A[i + 68] := ac_WriteValue;
     A[i + 69] := ac_WriteValue;

     A[i + 70] := ac_WriteValue;
     A[i + 71] := ac_WriteValue;
     A[i + 72] := ac_WriteValue;
     A[i + 73] := ac_WriteValue;
     A[i + 74] := ac_WriteValue;

     A[i + 75] := ac_WriteValue;
     A[i + 76] := ac_WriteValue;
     A[i + 77] := ac_WriteValue;
     A[i + 78] := ac_WriteValue;
     A[i + 79] := ac_WriteValue;

     A[i + 80] := ac_WriteValue;
     A[i + 81] := ac_WriteValue;
     A[i + 82] := ac_WriteValue;
     A[i + 83] := ac_WriteValue;
     A[i + 84] := ac_WriteValue;

     A[i + 85] := ac_WriteValue;
     A[i + 86] := ac_WriteValue;
     A[i + 87] := ac_WriteValue;
     A[i + 88] := ac_WriteValue;
     A[i + 89] := ac_WriteValue;

     A[i + 90] := ac_WriteValue;
     A[i + 91] := ac_WriteValue;
     A[i + 92] := ac_WriteValue;
     A[i + 93] := ac_WriteValue;
     A[i + 94] := ac_WriteValue;

     A[i + 95] := ac_WriteValue;
//     A[i + 96] := ac_WriteValue;
     A[i + 97] := ac_WriteValue;
     A[i + 98] := ac_WriteValue;
     A[i + 99] := ac_WriteValue;

     A[i + 100] := ac_WriteValue;
     A[i + 101] := ac_WriteValue;
     A[i + 102] := ac_WriteValue;
     A[i + 103] := ac_WriteValue;
     A[i + 104] := ac_WriteValue;

     A[i + 105] := ac_WriteValue;
     A[i + 106] := ac_WriteValue;
     A[i + 107] := ac_WriteValue;
     A[i + 108] := ac_WriteValue;
     A[i + 109] := ac_WriteValue;

     A[i + 110] := ac_WriteValue;
     A[i + 111] := ac_WriteValue;
     A[i + 112] := ac_WriteValue;
     A[i + 113] := ac_WriteValue;
     A[i + 114] := ac_WriteValue;

     A[i + 115] := ac_WriteValue;
     A[i + 116] := ac_WriteValue;
     A[i + 117] := ac_WriteValue;
     A[i + 118] := ac_WriteValue;
     A[i + 119] := ac_WriteValue;

     A[i + 120] := ac_WriteValue;
     A[i + 121] := ac_WriteValue;
     A[i + 122] := ac_WriteValue;
     A[i + 123] := ac_WriteValue;
     A[i + 124] := ac_WriteValue;

     A[i + 125] := ac_WriteValue;
     A[i + 126] := ac_WriteValue;
     A[i + 127] := ac_WriteValue;

    li64EndCycle := GetCycleCount;
    leElapsedTime := leElapsedTime +
      ( ( li64EndCycle - li64StartCycle ) - fSWOverhead ) /
      StopWatch.GetCPUClockspeed(FALSE);

    i := i + 128;

  end; // while...

  result := leElapsedTime;

end; // function TBandwidthBurn.WriteTestBP128

function TBandwidthBurn.WriteTestIterate(aiRepeatCount : integer;
                                         aiArraySize   : integer;
                                         ac_WriteValue  : cardinal;
                                         ab_RandomValue : Boolean;
                                         var A : TBigArray
                                        ): Extended;
var
  leElapsedTime, le_block_time   : extended;
  i                 : integer;
  leMinTime : extended;
begin
  // initialize variables
  leElapsedTime := 0;
  leMinTime := 99999999999999;
  for i := 1 to aiRepeatCount do begin
    if ab_RandomValue then begin
      ac_WriteValue   := Random(1000000);
    end;
    le_block_time := WriteTest(aiArraySize, ac_WriteValue, A);
    leElapsedTime := leElapsedTime + le_block_time;
    if leMinTime > leElapsedTime then begin
      leMinTime := leElapsedTime;
    end; // if
    leElapsedTime := 0;
  end;
  result := leMinTime;
end; // function TBandwidthBurn.WriteTestIterate

function TBandwidthBurn.WriteTestIterateBP32(aiRepeatCount : integer;
                                         aiArraySize   : integer;
                                         ac_WriteValue  : cardinal;
                                         ab_RandomValue : Boolean;
                                         var A : TBigArray
                                        ): Extended;
var
  leElapsedTime, le_block_time   : extended;
  i                 : integer;
  leMinTime : extended;
begin
  // initialize variables
  leElapsedTime := 0;
  leMinTime := 99999999999999;
  for i := 1 to aiRepeatCount do begin
    if ab_RandomValue then begin
      ac_WriteValue   := Random(1000000);
    end;
    le_block_time := WriteTestBP32(aiArraySize, ac_WriteValue, A);
    leElapsedTime := leElapsedTime + le_block_time;
    if leMinTime > leElapsedTime then begin
      leMinTime := leElapsedTime;
    end; // if
    leElapsedTime := 0;
  end;
  result := leMinTime;
end; // function TBandwidthBurn.WriteTestIterateBP32

function TBandwidthBurn.WriteTestIterateBP64(aiRepeatCount : integer;
                                         aiArraySize   : integer;
                                         ac_WriteValue  : cardinal;
                                         ab_RandomValue : Boolean;
                                         var A : TBigArray
                                        ): Extended;
var
  leElapsedTime, le_block_time   : extended;
  i                 : integer;
  leMinTime : extended;
begin
  // initialize variables
  leElapsedTime := 0;
  leMinTime := 99999999999999;
  for i := 1 to aiRepeatCount do begin
    if ab_RandomValue then begin
      ac_WriteValue   := Random(1000000);
    end;
    le_block_time := WriteTestBP64(aiArraySize, ac_WriteValue, A);
    leElapsedTime := leElapsedTime + le_block_time;
    if leMinTime > leElapsedTime then begin
      leMinTime := leElapsedTime;
    end; // if
    leElapsedTime := 0;
  end;
  result := leMinTime;
end; // function TBandwidthBurn.WriteTestIterateBP64

function TBandwidthBurn.WriteTestIterateBP128(aiRepeatCount : integer;
                                         aiArraySize   : integer;
                                         ac_WriteValue  : cardinal;
                                         ab_RandomValue : Boolean;
                                         var A : TBigArray
                                        ): Extended;
var
  leElapsedTime, le_block_time   : extended;
  i                 : integer;
  leMinTime : extended;
begin
  // initialize variables
  leElapsedTime := 0;
  leMinTime := 99999999999999;
  for i := 1 to aiRepeatCount do begin
    if ab_RandomValue then begin
      ac_WriteValue   := Random(1000000);
    end;
    le_block_time := WriteTestBP128(aiArraySize, ac_WriteValue, A);
    leElapsedTime := leElapsedTime + le_block_time;
    if leMinTime > leElapsedTime then begin
      leMinTime := leElapsedTime;
    end; // if
    leElapsedTime := 0;
  end;
  result := leMinTime;
end; // function TBandwidthBurn.WriteTestIterateBP128

function TBandwidthBurn.ReadWriteTest(aiRepeatCount : integer;
                                      aiArraySize   : integer;
                                      var A : TBigArray
                                      ): Extended;
var
  j : integer;
  liRandomData     : integer;
  leElapsedTime, le_block_time   : extended;
  leMinTime : extended;
begin
  // initialize variables
  leElapsedTime := 0;
  leMinTime := 99999999999999;
  for j := 1 to aiRepeatCount do begin
    // at each step we need to repeatedly populate the array:
    liRandomData   := Random(1000);
    le_block_time   := ReadTest( aiArraySize, A );
    leElapsedTime := leElapsedTime + le_block_time;
    le_block_time   := WriteTest(aiArraySize, liRandomData, A);
    leElapsedTime := leElapsedTime + le_block_time;
    if leMinTime > leElapsedTime then begin
      leMinTime := leElapsedTime;
    end; // if
    leElapsedTime := 0;
  end;
  result := leMinTime;
end; // function TBandwidthBurn.ReadWriteTest

function TBandwidthBurn.ReadWriteTestBP32(aiRepeatCount : integer;
                                          aiArraySize   : integer;
                                          var A : TBigArray
                                          ): Extended;
var
  liRandomData     : integer;
  leElapsedTime, le_test_time : extended;
  i              : integer;
  leMinTime : extended;
begin
  // initialize variables
  leElapsedTime := 0;
  leMinTime := 99999999999999;
  for i := 1 to aiRepeatCount do begin
    liRandomData   := Random(1000);
    le_test_time    := ReadTestBP32(aiArraySize, A);
    leElapsedTime := leElapsedTime + le_test_time;
    le_test_time    := WriteTestBP32(aiArraySize, liRandomData, A);
    leElapsedTime := leElapsedTime + le_test_time;
    if leMinTime > leElapsedTime then begin
      leMinTime := leElapsedTime;
    end; // if
    leElapsedTime := 0;
  end;
  result := leMinTime;
end; // function TBandwidthBurn.BlockPrefetch32ReadWriteTest

function TBandwidthBurn.ReadWriteTestBP64(aiRepeatCount : integer;
                                          aiArraySize   : integer;
                                          var A : TBigArray
                                          ): Extended;
var
  i : integer;
  liRandomData : integer;
  leElapsedTime, le_test_time : extended;
  leMinTime : extended;
begin
  // initialize variables
  leElapsedTime := 0;
  leMinTime := 99999999999999;
  for i := 1 to aiRepeatCount do begin
    liRandomData   := Random(1000);
    le_test_time    := ReadTestBP64(aiArraySize, A);
    leElapsedTime := leElapsedTime + le_test_time;
    le_test_time    := WriteTestBP64(aiArraySize, liRandomData, A);
    leElapsedTime := leElapsedTime + le_test_time;
    if leMinTime > leElapsedTime then begin
      leMinTime := leElapsedTime;
    end; // if
    leElapsedTime := 0;
  end;
  result := leMinTime;
end; // function TBandwidthBurn.ReadWriteTestBP64

function TBandwidthBurn.ReadWriteTestBP128(aiRepeatCount : integer;
                                          aiArraySize   : integer;
                                          var A : TBigArray
                                          ): Extended;
var
  i : integer;
  liRandomData  : integer;
  leElapsedTime, le_test_time : extended;
  leMinTime : extended;
begin
  // initialize variables
  leElapsedTime := 0;
  leMinTime := 99999999999999;
  for i := 1 to aiRepeatCount do begin
    liRandomData    := Random(1000);
    le_test_time    := ReadTestBP128(aiArraySize, A);
    leElapsedTime   := leElapsedTime + le_test_time;
    le_test_time    := WriteTestBP128(aiArraySize, liRandomData, A);
    leElapsedTime   := leElapsedTime + le_test_time;
    if leMinTime > leElapsedTime then begin
      leMinTime := leElapsedTime;
    end; // if
    leElapsedTime := 0;
  end;
  result := leMinTime;
end; // function TBandwidthBurn.ReadWriteTestBP128

function TBandwidthBurn.SimpleTightLoop(  aiRepeatCount : integer;
                                          aiArraySize   : integer;
                                          var A : TBigArray
                                        ): Extended;
var
  i, j : integer;
  liRandomData : integer;
  leElapsedTime : extended;
  li64StartCycle   : Int64;
  li64EndCycle     : Int64;
  lcAccumulate : cardinal;
  leAccumulatedTime : extended;
  leMinTime         : extended;
  leClockspeed      : extended;
begin
  // initialize variables
  leElapsedTime := 0;
  leClockspeed := StopWatch.GetCPUClockspeed(FALSE);
  leElapsedTime := 0;
  leMinTime := 99999999999999;
  // perform the benchmark tests for the specified number of times:
  for i := 1 to aiRepeatCount do begin
    leAccumulatedTime := 0;
    liRandomData   := Random(1000);
    lcAccumulate := 0;
    // start timer
    li64StartCycle := GetCycleCount;
    // write test:
    for j := 1 to aiArraySize do begin
      A[j] := liRandomData;
    end;
    // read test (must accumulate or optimizer will remove):
    for j := 1 to aiArraySize do begin
      lcAccumulate := A[j] + lcAccumulate;
    end; // for
    li64EndCycle := GetCycleCount;
    // calculate elapsed time:
    leElapsedTime :=
       ( ( li64EndCycle - li64StartCycle ) - fSWOverhead ) / leClockspeed;
    leAccumulatedTime := leAccumulatedTime + leElapsedTime;
    fDummyCardinal := lcAccumulate;
    // see if this is the fastest iteration:
    if leMinTime > leAccumulatedTime then begin
      leMinTime := leAccumulatedTime;
    end; // if
  end;
  result := leMinTime;
end; // function TBandwidthBurn.SimpleTightLoop

end.
