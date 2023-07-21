unit SortThds;
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
  Classes; //, COSBI_Common;

type

{ TSortThread }

  PSortArray = ^TSortArray;
//  TSortArray = array[0..MaxInt div SizeOf(Integer) - 1] of Integer;
  TSortArray =  array[0..1000000] of Integer; //array[0..33000] of Integer;


  TSortThread = class(TThread)
  private
    FSortArray: PSortArray;
    FSize: Integer;
    FA, FB, FI, FJ: Integer;
    SortArray : TSortArray;
  protected
    procedure Execute; override;
    procedure Sort(var A: array of Integer); virtual; abstract;
  public
    procedure RandomizeArray;
    constructor Create; //(var SortArray: array of Integer);
  end;

{TFibThread}
  TFibThread = class(TThread)
  private
    Fn: integer;
  protected
    procedure execute; override;
    function fib(n : integer): integer;
  public
    constructor Create(n: integer);
  end;

{TPiThread}
  TPiThread = class(TThread)
  private
    Fn: integer;
  protected
    procedure execute; override;
    function ComputePi(NumDigits : integer): string;
  public
    constructor Create(n: integer);
  end;

{ TBubbleSort }

  TBubbleSort = class(TSortThread)
  protected
    procedure Sort(var A: array of Integer); override;
  end;

{ TSelectionSort }

  TSelectionSort = class(TSortThread)
  protected
    procedure Sort(var A: array of Integer); override;
  end;

{ TQuickSort }

  TQuickSort = class(TSortThread)
  protected
    procedure Sort(var A: array of Integer); override;
  end;

implementation

{ TSortThread }

constructor TSortThread.Create; //(var SortArray: array of Integer);
begin
  RandomizeArray;
  FSortArray := @SortArray;
  FSize := High(SortArray) - Low(SortArray) + 1;
  FreeOnTerminate := True;
  inherited Create(False);
end;

{ The Execute method is called when the thread starts }

procedure TSortThread.Execute;
begin
  Sort(Slice(FSortArray^, FSize));
end; // procedure TSortThread.Execute

procedure TSortThread.RandomizeArray;
var
  I: Integer;
begin
    RandSeed := 1;
    for I := Low(SortArray) to High(SortArray) do
      SortArray[I] := Random(1000000000);
end; // procedure TSortThread.RandomizeArray;

{ TFibThread }

constructor TFibThread.Create(n: integer);
begin
  Fn := n;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TFibThread.Execute;
var
  n : integer;
begin
  n := Fib(Fn);
end; //procedure TFibThread.Execute;

function TFibThread.Fib( n : integer ) : integer;
begin
  if Terminated then Exit;
  if n > 2 then
      result := Fib( n - 1 ) + Fib( n - 2 )
  else
      result := 1;
end;

{ TPiThread }

constructor TPiThread.Create(n: integer);
begin
  Fn := n;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TPiThread.Execute;
var
  ls_pi : string;
begin
  ls_pi := ComputePi(Fn);
end; //procedure TPiThread.Execute;

function TPiThread.ComputePi( NumDigits : integer ) : string;
var
  A: array of LongInt;
  I, J, K, P, Q, X, Nines, Predigit: Integer;
  PiLength: Integer;
begin
  SetLength(A, 10*NumDigits div 3);
  SetLength(Result, NumDigits+1);
  PiLength := 1;
  for I := Low(A) to High(A) do
    A[I] := 2;
  Nines := 0;
  Predigit := 0;
  for J := 0 to NumDigits-1 do
  begin
    if Terminated then Exit;
    Q := 0;
    P := 2 * High(A) + 1;
    for I := High(A) downto Low(A) do
    begin
      X := 10*A[I] + Q*(I+1);
      A[I] := X mod P;
      Q := X div P;
      P := P - 2;
    end;
    A[Low(A)] := Q mod 10;
    Q := Q div 10;
    if Q = 9 then
      Inc(Nines)
    else if Q = 10 then
    begin
      Result[PiLength] := Chr(Predigit + 1 + Ord('0'));
      for K := 1 to Nines do
        Result[PiLength+K] := '0';
      PiLength := PiLength + Nines + 1;
      Predigit := 0;
      Nines := 0;
    end
    else
    begin
      Result[PiLength] := Chr(Predigit + Ord('0'));
      Predigit := Q;
      for K := 1 to Nines do
        Result[PiLength+K] := '9';
      PiLength := PiLength + Nines + 1;
      Nines := 0;
    end;
  end;
  Result[PiLength] := Chr(Predigit + Ord('0'));
end; //function TPiThread.ComputePi

{ TBubbleSort }

procedure TBubbleSort.Sort(var A: array of Integer);
var
  I, J, T: Integer;
begin
  for I := High(A) downto Low(A) do
    for J := Low(A) to High(A) - 1 do
      if A[J] > A[J + 1] then
      begin
        T := A[J];
        A[J] := A[J + 1];
        A[J + 1] := T;
        if Terminated then Exit;
      end;
end;

{ TSelectionSort }

procedure TSelectionSort.Sort(var A: array of Integer);
var
  I, J, T: Integer;
begin
  for I := Low(A) to High(A) - 1 do
    for J := High(A) downto I + 1 do
      if A[I] > A[J] then
      begin
        T := A[I];
        A[I] := A[J];
        A[J] := T;
        if Terminated then Exit;
      end;
end;

{ TQuickSort }

procedure TQuickSort.Sort(var A: array of Integer);

  procedure QuickSort(var A: array of Integer; iLo, iHi: Integer);
  var
    Lo, Hi, Mid, T: Integer;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := A[(Lo + Hi) div 2];
    repeat
      while A[Lo] < Mid do Inc(Lo);
      while A[Hi] > Mid do Dec(Hi);
      if Lo <= Hi then
      begin
        T := A[Lo];
        A[Lo] := A[Hi];
        A[Hi] := T;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then QuickSort(A, iLo, Hi);
    if Lo < iHi then QuickSort(A, Lo, iHi);
    if Terminated then Exit;
  end;

begin
  QuickSort(A, Low(A), High(A));
end;

end.
