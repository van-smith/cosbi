{/////////////////////////////////////////////////////////////////

  CPUID32 - The Processor ID Detector for Win32 Console
  Coded, written, and (c) by Bayu Prasetio

  unit name: bench.pas
  function : provide cpu benchmark function

  History:
  * Version 1.30 (August 2002)
    => Add Whetstone benchmark function

  * Version 1.22 (June 2002)
    => Improve Dhrystone function

  * Version 1.20 (June 2002)
    => Initial version
/////////////////////////////////////////////////////////////////}

unit bench;

interface
uses
  vars;

  function Dhrystone(NumberOfRuns: integer; var TimeElapsedForEachCycle: real): real;
  function Whetstone(NumberOfRuns: integer; var TimeElapsedForEachCycle: real): real;

implementation
uses
  Windows, funcs;

  const (* for measurement *)
    MicrosecondsPerClock  = 1000;
    ClocksPerSecond       = 1000;
        (* In Berkeley UNIX Pascal, the function "clock"        *)
        (* returns milliseconds                                 *)
    TooSmallTime          = 2000;
        (* Measurements should last at least 2 seconds          *)

var
  PriorityClass, Priority: Integer;
    (* Variables for measurement *)
    RunIndex: integer;
    BeginClock,
    EndClock: TSystemTime;
    SumClocks: real;

    (* end of variables for measurement *)

  procedure StartPriority;
  begin
    // save our priority class and current thread priority
    PriorityClass := GetPriorityClass(GetCurrentProcess);
    Priority := GetThreadPriority(GetCurrentThread);

    // set to real time and critical priority to get more precicious result
    SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
    SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_TIME_CRITICAL);

    GetLocalTime(BeginClock);
  end;

  procedure ResetPriority;
  begin
    GetLocalTime(EndClock);

    // restore our priority class and current thread priority
    SetThreadPriority(GetCurrentThread, Priority);
    SetPriorityClass(GetCurrentProcess, PriorityClass);
  end;


  function Dhrystone(NumberOfRuns: integer; var TimeElapsedForEachCycle: real): real;
  type

  (* Global type definitions *)

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

  var

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

    procedure Proc1 (    PointerParVal: RecordPointer);     forward;
    procedure Proc2 (var IntParRef:     OneToFifty);        forward;
    procedure Proc3 (var PointerParRef: RecordPointer);     forward;
    procedure Proc4;                                        forward;
    (* without parameters *)
    procedure Proc5;                                        forward;
    (* without parameters *)
    procedure Proc6 (    EnumParVal:    Enumeration;
                     var EnumParRef:    Enumeration);       forward;

    procedure Proc7 (    Int1ParVal,
                         Int2ParVal:    OneToFifty;
                     var IntParRef:     OneToFifty);        forward;

    procedure Proc8 (var Array1ParRef:  Array1DimInteger;
                     var Array2ParRef:  Array2DimInteger;
                         Int1ParVal,
                         Int2ParVal:    integer);            forward;

    function Func1  (    Char1ParVal,
                         Char2ParVal:   CapitalLetter): Enumeration; forward;

    function Func2  (var String1ParRef,
                         String2ParRef: String30): boolean;   forward;

    function Func3  (    EnumParVal:    Enumeration): boolean;   forward;


    procedure Proc1; (* (PointerParVal: RecordPointer) *)
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


    procedure Proc2; (* (var IntParRef: OneToFifty) *)
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


    procedure Proc3; (* (var PointerParRef: RecordPointer) *)
    (* executed once *)
    (* PointerParRef becomes PointerGlob *)
    begin
      if PointerGlob <> nil then (* executed *)
        PointerParRef := PointerGlob^.PointerComp;
      Proc7 (10, IntGlob, PointerGlob^.IntComp);
    end; (* Proc3 *)


    procedure Proc4; (* without parameters *)
    (* executed once *)
    var
      BoolLoc: boolean;
    begin
      BoolLoc := Char1Glob = 'A';
      BoolGlob := BoolLoc or BoolGlob;
      Char2Glob := 'B';
    end; (* Proc4 *)


    procedure Proc5; (* without parameters *)
    (* executed once *)
    begin
      Char1Glob := 'A';
      BoolGlob := false;
    end; (* Proc5 *)

    procedure Proc6; (* (    EnumParVal:     Enumeration;
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

    procedure Proc7; (* (    Int1ParVal,
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


    procedure Proc8; (* (var Array1ParRef: Array1DimInteger;
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

    function Func1; (* (Char1ParVal,
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


    function Func2; (* (var String1ParRef,
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


    function Func3; (* (EnumParVal: Enumeration): boolean *)
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

  begin (* main program, corresponds to procedures        *)
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

    StartPriority;

    for RunIndex := 1 to NumberOfRuns do
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

    ResetPriority;

//    writeln ('Execution ends');
//    writeln;
//    writeln ('Final values of the variables used in the benchmark:');
//    writeln;

//    writeln ('IntGlob:                      ', IntGlob : 5);
//    writeln ('        should be:                5');
//    write ('BoolGlob:                      ');
//    if BoolGlob = true then
//      writeln ('TRUE')
//    else
//      writeln ('FALSE');
//    writeln ('        should be:             TRUE');
//    writeln ('Char1Glob:                        ', Char1Glob);
//    writeln ('        should be:                A');
//    writeln ('Char2Glob:                        ', Char2Glob);
//    writeln ('        should be:                B');
//    writeln ('Array1Glob [8]:               ', Array1Glob [8] : 5);
//    writeln ('        should be:                7');
//    writeln ('Array2Glob [8,7]:             ', Array2Glob [8,7] : 5);
//    writeln ('        should be:                NumberOfRuns + 10');
//    writeln ('PointerGlob^.Discr:           ', ord (PointerGlob^.Discr) : 5);
//    writeln ('        should be:                0');
//    writeln ('PointerGlob^.EnumComp:        ', ord (PointerGlob^.EnumComp) : 5);
//    writeln ('        should be:                2');
//    writeln ('PointerGlob^.IntComp  :       ', PointerGlob^.IntComp : 5);
//    writeln ('        should be:               17');
//    write   ('PointerGlob^.StringComp:     ');
//    for I := 1 to 30 do
//      write (PointerGlob^.StringComp [I]);
//    writeln;
//    writeln ('        should be:           DHRYSTONE PROGRAM, SOME STRING');
//    writeln ('NextPointerGlob^.Discr:       ', ord (NextPointerGlob^.Discr) : 5);
//    writeln ('        should be:                0');
//    writeln ('NextPointerGlob^.EnumComp:    ',
//                      ord (NextPointerGlob^.EnumComp) : 5);
//    writeln ('        should be:                1');
//    writeln ('NextPointerGlob^.IntComp:     ', NextPointerGlob^.IntComp : 5);
//    writeln ('        should be:               18');
//    write   ('NextPointerGlob^.StringComp: ');
//    for I := 1 to 30 do
//      write (NextPointerGlob^.StringComp [I]);
//    writeln;
//    writeln ('        should be:           DHRYSTONE PROGRAM, SOME STRING');
//    writeln ('Int1Glob:                     ', Int1Glob : 5);
//    writeln ('        should be:                5');
//    writeln ('Int2Glob:                     ', Int2Glob : 5);
//    writeln ('        should be:               13');
//    writeln ('Int3Glob:                     ', Int3Glob : 5);
//    writeln ('        should be:                7');
//    writeln ('EnumGlob:                     ', ord (EnumGlob) : 5);
//    writeln ('        should be:                1');
//    write   ('String1Glob:                 ');
//    for I := 1 to 30 do
//      write (String1Glob [I]);
//    writeln;
//    writeln ('        should be:           DHRYSTONE PROGRAM, 1''ST STRING');
//    write   ('String2Glob:                 ');
//    for I := 1 to 30 do
//      write (String2Glob [I]);
//    writeln;
//    writeln ('        should be:           DHRYSTONE PROGRAM, 2''ND STRING');
//    writeln;
//    writeln;

    SumClocks := EncodeTime(EndClock.wHour, EndClock.wMinute, EndClock.wSecond, EndClock.wMilliseconds) -
                 EncodeTime(BeginClock.wHour, BeginClock.wMinute, BeginClock.wSecond, BeginClock.wMilliseconds);

    if SumClocks < (TooSmallTime / 1000) then
      Result := -1
    else
    begin
      TimeElapsedForEachCycle := SumClocks * (MicrosecondsPerClock / NumberOfRuns);
                                (* Brackets to prevent integer overflow *)
      Result := NumberOfRuns * (ClocksPerSecond / SumClocks);
    end;
  end;


  ///--------------------------------------------------------------------------
  ///--------------------------------------------------------------------------


  function Whetstone(NumberOfRuns: integer; var TimeElapsedForEachCycle: real): real;
  const
//    im = 1.0;        (* how many million whetstones to perform *)
    t = 0.499975;
    t1 = 0.50025;
    t2 = 2.0;

  type
    ArgArray = array[1..4] of real;

  var
    e1 : ArgArray;
    x, y, z,
    x1, x2, x3, x4 : real;

    i, j, k, l,
    n1, n2, n3, n4, n5,
    n6, n7, n8, n9, n10, n11 : integer;

  {$R-}               {disable range checking}

  procedure pa(var e: ArgArray);
  var
    j : integer;

  begin
   j := 0;

   repeat
     e[1] := (e[1] + e[2] + e[3] - e[4]) * t;
     e[2] := (e[1] + e[2] - e[3] + e[4]) * t;
     e[3] := (e[1] - e[2] + e[3] + e[4]) * t;
     e[4] := (-e[1] + e[2] + e[3] + e[4]) / t2;
     j := j + 1;
   until j >= 6;
  end;  (* procedure pa*)


  procedure p0;
  begin
   e1[j] := e1[k];
   e1[k] := e1[l];
   e1[l] := e1[j]
  end;   (* procedure p0 *)

  procedure p3(x, y: real; var z: real);
  begin
   x := t * (x + y);
   y := t * (x + y);
   z := (x + y) / t2
  end;  (* procedure p3 *)

  procedure module1; (* module 1: simple identifiers *)
  var
    i : integer;
  begin
    x1 := 1.0;
    x2 := -1.0;
    x3 := -1.0;
    x4 := -1.0;

    for i := 1 to n1 do
    begin
      x1 := (x1 + x2 + x3 - x4) * t;
      x2 := (x1 + x2 - x3 + x4) * t;
      x3 := (x1 - x2 + x3 + x4) * t;
      x4 := (-x1 + x2 + x3 + x4) * t
    end;
  end; (* module 1 *)

  procedure module2; (* module 2: array elements *)
  var
    i : integer;
  begin
   e1[1] := 1.0;
   e1[2] := -1.0;
   e1[3] := -1.0;
   e1[4] := -1.0;

   for i := 1 to n2 do
   begin
     e1[1] := (e1[1] + e1[2] + e1[3] - e1[4]) * t;
     e1[2] := (e1[1] + e1[2] - e1[3] + e1[4]) * t;
     e1[3] := (e1[1] - e1[2] + e1[3] + e1[4]) * t;
     e1[4] := (-e1[1] + e1[2] + e1[3] + e1[4]) * t
   end;
  end;  (* module 2 *)

  procedure module4; (* module 4: conditional jumps *)
  var
    i : integer;

  begin
    j := 1;
    for i := 1 to n4 do
    begin
      if j = 1 then
        j := 2
      else
        j := 3;

      if j > 1 then
        j := 0
      else
        j := 1;

      if j < 2 then
        j := 1
      else
        j := 0;
    end;
  end; (* module 4 *)

  procedure module6; (* integer arithmetic *)
  var
    i : integer;
  begin
    j := 1;
    k := 2;
    l := 3;

    for i := 1 to n6 do
    begin
      j := j * (k - j) * (l - k);
      k := l * k - (l - j) * k;
      l := (l - k) * k + j;
      e1[l-1] := j + k + l;
      e1[k-1] := j * k * l
    end;
  end; (* module 6 *)

  procedure module7; (* module 7: trig functions *)
  var
    i    : integer;
    temp : real;

  begin
    x := 0.5;
    y := 0.5;
    for i := 1 to n7 do
    begin
      temp := cos(x + y) + cos(x - y) - 1.0;
      x := t * arctan(t2 * sin(x) * cos(x) / temp);
      temp := cos(x + y) + cos(x - y) - 1.0;
      y := t * arctan(t2 * sin(y) * cos(y)/ temp);
    end;
  end; (* module 7 *)

  procedure module8; (* module 8: procedure calls *)
  var
    i  : integer;

  begin
    x := 1.0;
    y := 1.0;
    z := 1.0;

    for i := 1 to n8 do
      p3(x, y, z)
  end; (* module 8 *)

  procedure module10; (* module 10: integer artihmetic *)
  var
    i  : integer;
  begin
    j := 2;
    k := 3;
    for i := 1 to n10 do
    begin
      j := j + k;
      k := j + k;
      j := k - j;
      k := k - j - j
    end;
  end; (* module 10 *)

  procedure module11; (* module 11: standard functions *)
  var
    i  : integer;
  begin
    x := 0.75;

    for i := 1 to n11 do
      x := sqrt(exp(ln(x)/t1));
  end; (* module 11 *)

  procedure pout(var n, j, k: integer; var x1, x2, x3, x4: real);
  begin
   if Verbose then begin
    write(n:7, j:6, k:6);
    writeln(x1:11:3, x2:12:3, x3:12:3, x4:12:3);
   end;
  end;   (* procedure pout *)

  begin  (* start whetstone *)
    i := NumberOfRuns;
    n1 :=0;
    n2 := 12 * i;
    n3 := 14 * i;
    n4 := 345 * i;
    n5 := 0;
    n6 := 210 * i;
    n7 := 32 * i;
    n8 := 899 * i;
    n9 := 616 * i;
    n10 := 0;
    n11 := 93 *i ;

    StartPriority;

    (* modular programming is used to reduce the length of main code *)

    module1; (* simple identifiers *)
//    pout(n1, n1, n1, x1, x2, x3, x4);

    module2; (* array elements *)
//    pout(n2, n3, n2, e1[1], e1[2], e1[3], e1[4]);

    (* module 3: array as a parameter *)
    for i := 1 to n3 do
      pa(e1);
//    pout(n3, n2, n2, e1[1], e1[2], e1[3], e1[4]);
    (* end of module 3 *)

    module4; (* conditional jumps *)
//    pout(n4, j, j, x1, x2, x3, x4);

    module6; (* integer arithmetic *)
//    pout(n6, j, k, e1[1], e1[2], e1[3], e1[4]);

    module7; (* trig functions *)
//    pout(n7, j, k, x, x, y, y);

    module8; (* procedure calls *)
//    pout(n8, j, k, x, y, z, z);

    (* module 9: array references *)
    j := 1;
    k := 2;
    l := 3;
    e1[1] := 1.0;
    e1[2] := 2.0;
    e1[3] := 3.0;

    for i := 1 to n9 do p0;
//    pout(n9, j, k, e1[1], e1[2], e1[3], e1[4]);
    (* end of module 9 *)

    module10; (* integer arithmetic *)
//    pout(n10, j, k, x1, x2, x3, x4);

    module11; (* standard functions *)
//    pout(n11, j, k, x, x, x, x);

    ResetPriority;

    SumClocks := EncodeTime(EndClock.wHour, EndClock.wMinute, EndClock.wSecond, EndClock.wMilliseconds) -
                 EncodeTime(BeginClock.wHour, BeginClock.wMinute, BeginClock.wSecond, BeginClock.wMilliseconds);

    if SumClocks < (TooSmallTime / 1000000) then
      Result := -1
    else
    begin
      TimeElapsedForEachCycle := SumClocks * (MicrosecondsPerClock / NumberOfRuns);
                                (* Brackets to prevent integer overflow *)
      Result := NumberOfRuns * (ClocksPerSecond / SumClocks);
    end;

{    writeln('end of whetstone, ', trunc(NumberOfRuns * 10.0)/10.0:4:1,
            ' million whetstones performed');
    Result := trunc(NumberOfRuns * 10.0)/10.0;
}  end;

end.
