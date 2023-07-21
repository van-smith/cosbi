unit ThSort;
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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, COSBI_Common;

type
  TThreadSortForm = class(TForm)
    StartBtn: TButton;
    edCount: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure StartBtnClick(Sender: TObject);
  private
    ThreadsRunning : integer;
    procedure RandomizeArrays;
    procedure RunSorts;
    procedure ThreadDone(Sender: TObject);
  public
  end;

var
  ThreadSortForm: TThreadSortForm;

implementation

uses SortThds;

{$R *.DFM}

type
  PSortArray = ^TSortArray;
  TSortArray =  array[0..33000] of Integer; //array[0..66000] of Integer;

var
  ArraysRandom: Boolean;
  BubbleSortArray, SelectionSortArray, QuickSortArray: TSortArray;
  iCount : integer;
  iTickCount : integer;

{ TThreadSortForm }

procedure TThreadSortForm.FormCreate(Sender: TObject);
begin
  RandomizeArrays;
end;

procedure TThreadSortForm.RunSorts;
begin
  ThreadsRunning := 3;
  RandomizeArrays;
  with TSelectionSort.Create(BubbleSortArray) do
    OnTerminate := ThreadDone;
  with TSelectionSort.Create(SelectionSortArray) do
    OnTerminate := ThreadDone;
  with TSelectionSort.Create(QuickSortArray) do
    OnTerminate := ThreadDone;
end;

procedure TThreadSortForm.StartBtnClick(Sender: TObject);
var
  s : string;
begin
  iCount := StrToInt(edCount.Text);
  StartBtn.Enabled := False;
  iTickCount := GetTickCount;
  RunSorts;
end;

procedure TThreadSortForm.RandomizeArrays;
var
  I: Integer;
begin
  if not ArraysRandom then
  begin
    //Randomize;
    RandSeed := 1;
    for I := Low(BubbleSortArray) to High(BubbleSortArray) do
      BubbleSortArray[I] := Random(100000);
    SelectionSortArray := BubbleSortArray;
    QuickSortArray := BubbleSortArray;
    ArraysRandom := True;
  end;
end;

procedure TThreadSortForm.ThreadDone(Sender: TObject);
var
   dTime : Double;
begin
  Dec(ThreadsRunning);
  if ThreadsRunning = 0 then
  begin
    Dec(iCount);
    ArraysRandom := False;
    if iCount = 0 then
    begin
      StartBtn.Enabled := True;
      dTime := (GetTickCount - iTickCount)/1000;
      ShowMessage(FloatToStr(dTime));
      RandomizeArrays;
    end
    else
    begin
      RunSorts;
    end;
  end;
end;

end.
