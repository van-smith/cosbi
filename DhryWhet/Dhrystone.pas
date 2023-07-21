unit Dhrystone;
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
// Unit name: Dhrystone.pas
// Unit description: Contains an object oriented version of Dhrystone 2.1
//                   descended from COSBI's TTest. Dhrystone 2.1 was originally
//                   written by Reinhold P. Weicker and can be obtained here:
//  http://ground.ecn.uiowa.edu/apple2/Mirrors/wustl/Source/dhrystone.source.txt
// Author: Bayu Prasetio, Van Smith
// Date: March 22, 2003
// OS dependent: No
// Resolution dependent: No
// External unit dependencies: COSBI_Common
//==============================================================================
// Modification History
// ------------ -------
// Ver  Date   Who    Description
// ==== ====== ====== ==========================================================
// 1.0 032203 Van     Simple port of Bayu's Dhrystone code to a class descended
//                    from COSBI TTest.
//==============================================================================

interface

uses
  SysUtils, Classes, COSBI_Common, uCOSBI_TTest, forms, uStopWatch;

type

  Enumeration           = (Ident1, Ident2, Ident3, Ident4, Ident5);

  OneToThirty           = 1..30;
  OneToFifty            = 1..50;
  CapitalLetter         = 'A'..'Z';

  String30              = packed array [OneToThirty] of char;

  Array1DimInteger      = array [OneToFifty] of integer;
  Array2DimInteger      = array [OneToFifty, OneToFifty] of integer;

  RecordPointer         = ^RecordType;

  RecordType            =
      record
        PointerComp:   RecordPointer;
        case Discr:    Enumeration of
          Ident1:         (* only this variant is used,           *)
                          (* but in some cases discriminant       *)
                          (* checks are necessary                 *)
            (EnumComp:      Enumeration;
             IntComp:       OneToFifty;
             StringComp:    String30);
          Ident2:
            (Enum2Comp:    Enumeration;
             String2Comp:  String30);
          Ident3, Ident4, Ident5:
            (Char1Comp,
             Char2Comp:    char);
      end; (* record *)

  TDhrystone = class( TTest )
  private
    //Bayu's global variables are now private field variables:
    (* Ada version: Variables local in Proc_0 *)
    Int1Glob,
    Int2Glob,
    Int3Glob:       OneToFifty;
    CharIndex:      char;
    EnumGlob:       Enumeration;
    String1Glob,
    String2Glob:    String30;
    (* Ada version: Variables global in Pack_1 *)
    PointerGlob,
    NextPointerGlob: RecordPointer;
    IntGlob:         integer;
    BoolGlob:        boolean;
    Char1Glob,
    Char2Glob:       char;
    Array1Glob:      Array1DimInteger;
    Array2Glob:      Array2DimInteger;
    // new field variable to time benchmark:
    fNumberOfRuns : integer;                                             //+v1.0
    fTimePerCycle : extended;                                            //+v1.0
    fDhrystonesPerSecond : double;                                       //+v1.0
    //Bayu's nested procedures are now private methods:
    procedure Proc1 ( PointerParVal: RecordPointer);
    procedure Proc2 (var IntParRef:     OneToFifty);
    procedure Proc3 (var PointerParRef: RecordPointer);
    procedure Proc4;
    (* without parameters *)
    procedure Proc5;
    (* without parameters *)
    procedure Proc6 (    EnumParVal:    Enumeration;
                     var EnumParRef:    Enumeration);

    procedure Proc7 (    Int1ParVal,
                         Int2ParVal:    OneToFifty;
                     var IntParRef:     OneToFifty);

    procedure Proc8 (var Array1ParRef:  Array1DimInteger;
                     var Array2ParRef:  Array2DimInteger;
                         Int1ParVal,
                         Int2ParVal:    integer);
    function Func1  (    Char1ParVal,
                         Char2ParVal:   CapitalLetter): Enumeration;
    function Func2  (var String1ParRef,
                         String2ParRef: String30): boolean;
    function Func3  (    EnumParVal:    Enumeration): boolean;
  protected
  public
    constructor Create;     Overload;                                    //+v1.0
    procedure   Clear;      Override;                                    //+v1.0
    procedure   BeforeTest; Override;                                    //+v1.0
    procedure   RunTest;    Override;                                    //+v1.0
    procedure   AfterTest;  Override;                                    //+v1.0
    property NumberOfRuns: integer read fNumberOfRuns write fNumberOfRuns;//+v1.0
    property TimePerCycle : extended read fTimePerCycle;                 //+v1.0
    property DhrystonesPerSecond : double read fDhrystonesPerSecond;     //+v1.0
  end; // TDhrystone = class( TDhrystone )                               //+v1.0

  TDhrystoneThread = class(TThread)
  private
    fStopWatch : TStopWatch;
  protected
    procedure Execute; override;
  public
    constructor Create;
  end; // TDhrystoneThread......................................................

  TDhrystoneThreadTest = class( TTest )
  private
    fDhrystoneThread : array[ 1..32 ] of TDhrystoneThread;
    procedure ThreadDone(Sender: TObject);
  protected
  public
    constructor Create; Overload;
    procedure BeforeTest; Override;
    function GetScore(ai_index : integer) : integer; override;
    procedure RunTest; Override;
  end; // TDhrystoneThreadTest..................................................

implementation

const

  DEFAULT_ITERATIONS = 25000000;

  constructor TDhrystone.Create;
  begin
    inherited;
    fTestName         := 'Dhrystone 2.1';
    fTestDescription  := 'This is a port of Bayu Prasetio''s Dhrystone test ' +
       'which is itself based upon Reinhold P. Weicker''s original Pascal code.';
    fTestVersion      := '1.0';
    fTestType         := ttCpu;
    fTestAuthor       := 'Van Smith, Bayu Prasetio';
    fReferenceTime    := 3.99 / 2;
    fQTestType        := qtDhrystone;
  end; // constructor TDhrystone.Create;

  procedure TDhrystone.Clear;
  begin
    inherited;
    fNumberOfRuns         := DEFAULT_ITERATIONS;
    fTimePerCycle         := 0;                                          //+v1.0
    fDhrystonesPerSecond  := 0;
  end; // procedure TDhrystone.Clear;

  procedure TDhrystone.BeforeTest;                                       //+v1.0
  begin
    inherited;
  (* Main and Proc_0 in the Ada version             *)
  (* Initializations *)
    new (NextPointerGlob);
    new (PointerGlob);
    PointerGlob^.PointerComp := NextPointerGlob;
    PointerGlob^.Discr       := Ident1;
    PointerGlob^.EnumComp    := Ident3;
    PointerGlob^.IntComp     := 40;
    PointerGlob^.StringComp  := 'DHRYSTONE PROGRAM, SOME STRING';
    String1Glob := 'DHRYSTONE PROGRAM, 1''ST STRING';
    Array2Glob [8,7] := 10;
  end; // procedure TDhrystone.BeforeTest

  procedure TDhrystone.AfterTest;
  var
    ld_LastDhryPerSec : double;
  begin
    inherited;

    // track peak Dhrystone score
    ld_LastDhryPerSec := fNumberOfRuns / fLastTime;
    if ld_LastDhryPerSec > fDhrystonesPerSecond then begin
      // time per cycle:
      fTimePerCycle := fLastTime / fNumberOfRuns;
      // Result most commonly reported:
      fDhrystonesPerSecond := ld_LastDhryPerSec;
    end; // if ld_LastDhryPerSec

    // free memory:
    dispose(NextPointerGlob);
    dispose(PointerGlob);
  end; // procedure TDhrystone.AfterTest

  procedure TDhrystone.Proc1( PointerParVal: RecordPointer );
  (* executed once *)
  begin
   with PointerParVal^.PointerComp^ (* = PointerGlobNext *) do
   begin
     PointerParVal^.PointerComp^ := PointerGlob^;
     PointerParVal^.IntComp := 5;
     IntComp := PointerParVal^.IntComp;
     PointerComp := PointerParVal^.PointerComp;
     Proc3 (PointerComp);
     (* PointerParVal^.PointerComp^.PointerComp = PointerGlob^.PointerComp *)
     if Discr = Ident1 then (* executed *)
     begin
       IntComp := 6;
       Proc6 (PointerParVal^.EnumComp, EnumComp);
       PointerComp := PointerGlob^.PointerComp;
       Proc7 (IntComp, 10, IntComp);
     end (* then *)
     else (* not executed *)
       PointerParVal^ := PointerParVal^.PointerComp^;
   end; (* with *)
  end; (* Proc1 *)

  procedure TDhrystone.Proc2; (* (var IntParRef: OneToFifty) *)
  (* executed once *)
  (* InParRef = 3, becomes 7 *)
  var
    IntLoc:  OneToFifty;
    EnumLoc: Enumeration;

  begin
    IntLoc := IntParRef + 10;
    repeat (* executed once *)
      if Char1Glob = 'A' then (* executed *)
      begin
        IntLoc := IntLoc - 1;
        IntParRef := IntLoc - IntGlob;
        EnumLoc := Ident1;
      end (* if *)
    until EnumLoc = Ident1; (* true *)
  end; (* Proc2 *)

  procedure TDhrystone.Proc3; (* (var PointerParRef: RecordPointer) *)
  (* executed once *)
  (* PointerParRef becomes PointerGlob *)
  begin
    if PointerGlob <> nil then (* executed *)
      PointerParRef := PointerGlob^.PointerComp;
    Proc7 (10, IntGlob, PointerGlob^.IntComp);
  end; (* Proc3 *)

  procedure TDhrystone.Proc4; (* without parameters *)
  (* executed once *)
  var
    BoolLoc: boolean;
  begin
    BoolLoc := Char1Glob = 'A';
    BoolGlob := BoolLoc or BoolGlob;
    Char2Glob := 'B';
  end; (* Proc4 *)

  procedure TDhrystone.Proc5; (* without parameters *)
  (* executed once *)
  begin
    Char1Glob := 'A';
    BoolGlob := false;
  end; (* Proc5 *)

  procedure TDhrystone.Proc6; (* (    EnumParVal:     Enumeration;
                       var EnumParRef:     Enumeration) *)
  (* executed once *)
  (* EnumParVal = Ident3, EnumParRef becomes Ident2 *)
  begin
    EnumParRef := EnumParVal;
    if not Func3 (EnumParVal) then (* not executed *)
      EnumParRef := Ident4;
    case EnumParVal of
      Ident1: EnumParRef := Ident1;
      Ident2: if IntGlob > 100  then
                EnumParRef := Ident1
              else
                EnumParRef := Ident4;
      Ident3: EnumParRef := Ident2;    (* executed *)
      Ident4: ;
      Ident5: EnumParRef := Ident3;
    end; (* case *)
  end; (* Proc6 *)

  procedure TDhrystone.Proc7; (* (    Int1ParVal,
                           Int2ParVal:    OneToFifty;
                       var IntParRef:     OneToFifty) *)
  (* executed three times                               *)
  (* first call:      Int1ParVal = 2, Int2ParVal = 3,   *)
  (*                  IntParRef becomes 7               *)
  (* second call:     Int1ParVal = 10, Int2ParVal = 5,  *)
  (*                  IntParRef becomes 17              *)
  (* third call:      Int1ParVal = 6, Int2ParVal = 10,  *)
  (*                  IntParRef becomes 18              *)
  var
    IntLoc: OneToFifty;
  begin
    IntLoc := Int1ParVal + 2;
    IntParRef := Int2ParVal + IntLoc;
  end; (* Proc7 *)

  procedure TDhrystone.Proc8; (* (var Array1ParRef: Array1DimInteger;
                       var Array2ParRef: Array2DimInteger;
                           Int1ParVal,
                           Int2ParVal:    integer)          *)
  (* executed once  *)
  (* Int1ParVal = 3 *)
  (* Int2ParVal = 7 *)
  var
    IntIndex,
    IntLoc:   OneToFifty;
  begin
    IntLoc := Int1ParVal + 5;
    Array1ParRef [IntLoc] := Int2ParVal;
    Array1ParRef [IntLoc+1] := Array1ParRef [IntLoc];
    Array1ParRef [IntLoc+30] := IntLoc;
    for IntIndex := IntLoc to IntLoc+1 do
      Array2ParRef [IntLoc, IntIndex] := IntLoc;
    Array2ParRef [IntLoc, IntLoc-1] := Array2ParRef [IntLoc, IntLoc-1] + 1;
    Array2ParRef [IntLoc+20, IntLoc] := Array1ParRef [IntLoc];
    IntGlob := 5;
  end; (* Proc8 *)

  function TDhrystone.Func1; (* (Char1ParVal,
                      Char2ParVal: CapitalLetter): Enumeration *)
  (* executed three times, returns always Ident1              *)
  (* first call:      Char1ParVal = 'H', Char2ParVal = 'R'    *)
  (* second call:     Char1ParVal = 'A', Char2ParVal = 'C'    *)
  (* third call:      Char1ParVal = 'B', Char2ParVal = 'C'    *)
  var
    Char1Loc, Char2Loc: CapitalLetter;
  begin
    Char1Loc := Char1ParVal;
    Char2Loc := Char1Loc;
    if Char2Loc <> Char2ParVal then  (* executed *)
      Func1 := Ident1
    else  (* not executed *)
    begin
      Char1Glob := Char1Loc;
      Func1 := Ident2;
    end;
  end; (* Func1 *)

  function TDhrystone.Func2; (* (var String1ParRef,
                          String2ParRef: String30): boolean *)
  (* executed once, returns false              *)
  (* String1ParRef = 'DHRYSTONE PROGRAM, 1''ST STRING' *)
  (* String2ParRef = 'DHRYSTONE PROGRAM, 2''ND STRING' *)
  var
    IntLoc:  OneToThirty;
    CharLoc: CapitalLetter;
  begin
    IntLoc := 2;
    while IntLoc <= 2 do (* loop body executed once *)
      if Func1 (String1ParRef[IntLoc], String2ParRef[IntLoc+1]) = Ident1 then (* executed *)
      begin
        CharLoc := 'A';
        IntLoc := IntLoc + 1;
      end; (* if, while *)

    if (CharLoc >= 'W') and (CharLoc < 'Z') then (* not executed *)
      IntLoc := 7;
    if CharLoc = 'R' then (* not executed *)
      Func2 := true
    else (* executed *)
    begin
      if String1ParRef > String2ParRef then (* not executed *)
      begin
        IntLoc := IntLoc + 7;
        IntGlob := IntLoc;
        Func2 := true;
      end
      else (* executed *)
        Func2 := false;
    end; (* if CharLoc *)
  end; (* Func2 *)

  function TDhrystone.Func3; (* (EnumParVal: Enumeration): boolean *)
  (* executed once, returns true      *)
  (* EnumParVal = Ident3              *)
  var
    EnumLoc:  Enumeration;
  begin
    EnumLoc := EnumParVal;
    if EnumLoc = Ident3 then (* executed *)
      Func3 := true
    else (* not executed *)
      Func3 := false;
  end; (* Func3 *)

  procedure TDhrystone.RunTest;
  var
    CharIndex : char;
    RunIndex : integer;

  begin (* main program, corresponds to procedures        *)
    for RunIndex := 1 to fNumberOfRuns do
    begin
      Proc5;
      Proc4;
      (* Char1Glob = 'A', Char2Glob = 'B', BoolGlob = false *)
      Int1Glob := 2;
      Int2Glob := 3;
      String2Glob := 'DHRYSTONE PROGRAM, 2''ND STRING';
      EnumGlob := Ident2;
      BoolGlob := not Func2 (String1Glob, String2Glob);
      (* BoolGlob = true *)
      while Int1Glob < Int2Glob do  (* loop body executed once *)
      begin
        Int3Glob := 5 * Int1Glob - Int2Glob;
        (* Int3Glob = 7 *)
        Proc7 (Int1Glob, Int2Glob, Int3Glob);
        (* Int3Glob = 7 *)
        Int1Glob := Int1Glob + 1;
      end; (* while *)
      (* Int1Glob = 3 *)
      Proc8 (Array1Glob, Array2Glob, Int1Glob, Int3Glob);
      (* IntGlob = 5 *)
      Proc1 (PointerGlob);
      for CharIndex := 'A' to Char2Glob do   (* loop body executed twice *)
       if EnumGlob = Func1 (CharIndex, 'C') then (* not executed *)
       begin
         Proc6 (Ident1, EnumGlob);
         String2Glob := 'DHRYSTONE PROGRAM, 3''RD STRING';
         Int2Glob := RunIndex;
         IntGlob := RunIndex;
       end;
      (* Int1Glob = 3, Int2Glob = 3, Int3Glob = 7 *)
      Int2Glob := Int2Glob * Int1Glob;
      Int1Glob := Int2Glob div Int3Glob;
      Int2Glob := 7 * (Int2Glob - Int3Glob) - Int1Glob;
      (* Int1Glob = 1, Int2Glob = 13, Int3Glob = 7 *)
      Proc2 (Int1Glob);
      (* Int1Glob = 5 *)
    end; (* for RunIndex *)
  end; // procedure TDhrystone.RunTest;

// TDhrystoneThread...................................................................
constructor TDhrystoneThread.Create;
begin
  FreeOnTerminate := True;
  fStopWatch := TStopWatch.Create;
  inherited Create(TRUE);  // creates suspended
end;

procedure TDhrystoneThread.Execute;
var
  lDhrystone : TDhrystone;
begin
  lDhrystone := TDhrystone.Create;
  lDhrystone.ShowStatus := FALSE;
  lDhrystone.StopWatch := fStopWatch;
  lDhrystone.Run;
end; //procedure TDhrystoneThread.Execute;
// TDhrystoneThread ends..............................................................

// TDhrystoneThreadTest begins.................................................
  constructor TDhrystoneThreadTest.Create;
  begin
    inherited;
    fTestName   := 'Dhrystone threads test';
    fTestDescription := 'Executes a number of Dhrystone threads that automatically corresponds ' +
      'to the number of logical processors in the system.  The Dhrystone test is a port of ' +
      'Reinhold P. Weicker''s original code by Van Smith and Bayu Prasetio.';
    fTestVersion:= '1.0';
    fTestType   := ttThreads;
    fTestAuthor := 'Van Smith, Bayu Prasetio';
    fReferenceTime := 3.99 / 2;
    fQTestType := qtDhrystoneThreads;
    fPerformanceVectors := [ pvMathScienceEngineering ];
  end; // constructor TDhrystoneThreadTest.Create;

  procedure TDhrystoneThreadTest.BeforeTest;
  var
    i : integer;
  begin
    inherited;
    // create a thread for each processor:
    for i := 1 to fMaxNbrOfThreads do begin
      fDhrystoneThread[ i ] := TDhrystoneThread.Create;
      fDhrystoneThread[ i ].OnTerminate := ThreadDone;
    end;
    if assigned( ffrmStatus ) then ffrmStatus.AddLine( 'Spawning ' +
                            IntToStr( fMaxNbrOfThreads ) +
                            ' thread(s)...' );
    application.ProcessMessages;
  end; // procedure TDhrystoneThreadTest.BeforeTest

  procedure TDhrystoneThreadTest.RunTest;
  var
    i : integer;
  begin
    for i := 1 to fMaxNbrOfThreads do begin
      fDhrystoneThread[ i ].Resume;
      inc( fNbrOfThreadsRunning );
    end; // for
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure TDhrystoneThreadTest.RunTest;

  procedure TDhrystoneThreadTest.ThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    application.ProcessMessages;
  end; // procedure TDhrystoneThreadTest.FibThreadDone;

  function TDhrystoneThreadTest.GetScore(ai_index : integer) : integer;
  begin
    result := inherited GetScore( ai_index );
    result := fMaxNbrOfThreads * result;
  end; // function TDhrystoneThreadTest.GetScore

// TDhrystoneThreadTest ends...................................................

end.


