unit Whetstone;
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
// Unit name: Whetstone.pas
// Unit description: Contains an object oriented version of Whetstone
//                   descended from COSBI's TTest.
// Author: Bayu Prasetio, Van Smith
// Date: March 22, 2003
// OS dependent: No
// Resolution dependent: No
// External unit dependencies: COSBI_Common, uTests
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

  ArgArray = array[1..4] of real;

  TWhetstone = class( TTest )
  private
    e1 : ArgArray;
    x, y, z,
    x1, x2, x3, x4 : real;
    j, k, l,
    n1, n2, n3, n4, n5,
    n6, n7, n8, n9, n10, n11 : integer;
    fNumberOfRuns : integer;
    fTimePerCycle : extended;
    fWhetstonesPerSecond : double;
    procedure pa(var e: ArgArray);
    procedure p0;
    procedure p3(x, y: real; var z: real);
    procedure module1; (* module 1: simple identifiers *)
    procedure module2; (* module 2: array elements *)
    procedure module4; (* module 4: conditional jumps *)
    procedure module6; (* integer arithmetic *)
    procedure module7; (* module 7: trig functions *)
    procedure module8; (* module 8: procedure calls *)
    procedure module10; (* module 10: integer artihmetic *)
    procedure module11; (* module 11: standard functions *)
//    procedure pout(var n, j, k: integer; var x1, x2, x3, x4: real);
  protected
  public
    procedure   RunTest;    Override;                                    //+v1.0
    constructor Create;     Overload;                                    //+v1.0
    procedure   Clear;      Override;                                    //+v1.0
    procedure   BeforeTest; Override;                                    //+v1.0
    procedure   AfterTest;  Override;                                    //+v1.0
    property NumberOfRuns: integer read fNumberOfRuns write fNumberOfRuns;//+v1.0
    property TimePerCycle : extended read fTimePerCycle;                 //+v1.0
    property WhetstonesPerSecond : double read fWhetstonesPerSecond;     //+v1.0
  end; // TWhetstone............................................................

  TWhetstoneThread = class(TThread)
  private
    fStopWatch : TStopWatch;
  protected
    procedure Execute; override;
  public
    constructor Create;
  end; // TWhetstoneThread......................................................

  TWhetstoneThreadTest = class( TTest )
  private
    fWhetstoneThread : array[ 1..32 ] of TWhetstoneThread;
    procedure ThreadDone(Sender: TObject);
  protected
  public
    constructor Create; Overload;
    procedure BeforeTest; Override;
    function GetScore(ai_index : integer) : integer; override;
    procedure RunTest; Override;
  end; // TWhetstoneThreadTest..................................................


implementation

const
  t = 0.499975;
  t1 = 0.50025;
  t2 = 2.0;
  DEFAULT_ITERATIONS = 50000;

  constructor TWhetstone.Create;
  begin
    inherited;
    fTestName         := 'Whetstone';
    fTestDescription  := 'This is a port of Bayu Prasetio''s Whetstone test.';
    fTestVersion      := '1.0';
    fTestType         := ttFpu;
    fTestAuthor       := 'Van Smith, Bayu Prasetio';
    fReferenceTime    := 3.00772 / 2;
    fQTestType        := qtWhetstone;
  end; // constructor TWhetstone.Create;

  procedure TWhetstone.Clear;
  begin
    inherited;
    fNumberOfRuns         := DEFAULT_ITERATIONS;
    fTimePerCycle         := 0;                                          //+v1.0
    fWhetstonesPerSecond  := 0;
  end; // procedure TWhetstone.Clear;

  procedure TWhetstone.BeforeTest;                                       //+v1.0
  var
    i : integer;
  begin
    inherited;
    i := fNumberOfRuns;
    n1 := 0;
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
  end; // procedure TWhetstone.BeforeTest

  procedure TWhetstone.AfterTest;
  var
    ld_LastWhetPerSec : double;
  begin
    inherited;
    // track peak TWhetstone score
    ld_LastWhetPerSec := fNumberOfRuns / fLastTime;
    if ld_LastWhetPerSec > fWhetstonesPerSecond then begin
      // time per cycle:
      fTimePerCycle := fLastTime / fNumberOfRuns;
      // Result most commonly reported:
      fWhetstonesPerSecond := ld_LastWhetPerSec;
    end; // if ld_LastWhetPerSec
  end; // procedure TWhetstone.AfterTest

  procedure TWhetstone.pa(var e: ArgArray);
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


  procedure TWhetstone.p0;
  begin
   e1[j] := e1[k];
   e1[k] := e1[l];
   e1[l] := e1[j]
  end;   (* procedure p0 *)

  procedure TWhetstone.p3(x, y: real; var z: real);
  begin
   x := t * (x + y);
   y := t * (x + y);
   z := (x + y) / t2
  end;  (* procedure p3 *)

  procedure TWhetstone.module1; (* module 1: simple identifiers *)
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

  procedure TWhetstone.module2; (* module 2: array elements *)
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

  procedure TWhetstone.module4; (* module 4: conditional jumps *)
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

  procedure TWhetstone.module6; (* integer arithmetic *)
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

  procedure TWhetstone.module7; (* module 7: trig functions *)
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

  procedure TWhetstone.module8; (* module 8: procedure calls *)
  var
    i  : integer;

  begin
    x := 1.0;
    y := 1.0;
    z := 1.0;

    for i := 1 to n8 do
      p3(x, y, z)
  end; (* module 8 *)

  procedure TWhetstone.module10; (* module 10: integer artihmetic *)
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

  procedure TWhetstone.module11; (* module 11: standard functions *)
  var
    i  : integer;
  begin
    x := 0.75;

    for i := 1 to n11 do
      x := sqrt(exp(ln(x)/t1));
  end; (* module 11 *)

//  procedure TWhetstone.pout(var n, j, k: integer; var x1, x2, x3, x4: real);
//  begin
//   if Verbose then begin
//    write(n:7, j:6, k:6);
//    writeln(x1:11:3, x2:12:3, x3:12:3, x4:12:3);
//   end;
//  end;   (* procedure pout *)

  procedure TWhetstone.RunTest;
  var
    i : integer;
  begin
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

  end; //procedure TWhetstone.RunTest;
// TWhetstone ends................................................................

// TWhetstoneThread...................................................................
constructor TWhetstoneThread.Create;
begin
  FreeOnTerminate := True;
  fStopWatch := TStopWatch.Create;
  inherited Create(TRUE);  // creates suspended
end;

procedure TWhetstoneThread.Execute;
var
  lWhetstone : TWhetstone;
begin
  lWhetstone := TWhetstone.Create;
  lWhetstone.ShowStatus := FALSE;
  lWhetstone.StopWatch := fStopWatch;
  lWhetstone.Run;
end; //procedure TWhetstoneThread.Execute;
// TWhetstoneThread ends..............................................................

// TWhetstoneThreadTest begins.................................................
  constructor TWhetstoneThreadTest.Create;
  begin
    inherited;
    fTestName   := 'Whetstone threads test';
    fTestDescription := 'Executes a number of Whetstone threads that automatically corresponds ' +
      'to the number of logical processors in the system.  The Whetstone code used was ported by ' +
      'Van Smith and Bayu Prasetio.';
    fTestVersion:= '1.0';
    fTestType   := ttThreads;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 3.00772 / 2;
    fQTestType := qtWhetstoneThreads;
    fPerformanceVectors := [ pvMathScienceEngineering ];
  end; // constructor TWhetstoneThreadTest.Create;

  procedure TWhetstoneThreadTest.BeforeTest;
  var
    i : integer;
  begin
    inherited;
    // create a thread for each processor:
    for i := 1 to fMaxNbrOfThreads do begin
      fWhetstoneThread[ i ] := TWhetstoneThread.Create;
      fWhetstoneThread[ i ].OnTerminate := ThreadDone;
    end;
    if assigned( ffrmStatus ) then ffrmStatus.AddLine( 'Spawning ' +
                            IntToStr( fMaxNbrOfThreads ) +
                            ' thread(s)...' );
    application.ProcessMessages;
  end; // procedure TWhetstoneThreadTest.BeforeTest

  procedure TWhetstoneThreadTest.RunTest;
  var
    i : integer;
  begin
    for i := 1 to fMaxNbrOfThreads do begin
      fWhetstoneThread[ i ].Resume;
      inc( fNbrOfThreadsRunning );
    end; // for
    while fNbrOfThreadsRunning > 0 do begin
      sleep( 10 );
      application.ProcessMessages;
    end;
  end; // procedure TWhetstoneThreadTest.RunTest;

  procedure TWhetstoneThreadTest.ThreadDone(Sender: TObject);
  begin
    dec( fNbrOfThreadsRunning );
    application.ProcessMessages;
  end; // procedure TWhetstoneThreadTest.FibThreadDone;

  function TWhetstoneThreadTest.GetScore(ai_index : integer) : integer;
  begin
    result := inherited GetScore( ai_index );
    result := fMaxNbrOfThreads * result;
  end; // function TWhetstoneThreadTest.GetScore

// TDhrystoneThreadTest ends...................................................

end.
