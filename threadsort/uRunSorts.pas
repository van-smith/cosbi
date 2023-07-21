unit uRunSorts;

interface

uses
  Classes, SortThds, COSBI_Common;

type

  TSortRunning = (sortBubble, sortSelection, sortQuick, sortNone);

  TSortBench = Class( TObject )
  private
    ThreadsRunning  : Integer;
    FSortRunning    : TSortRunning;
    FStopWatch      : TStopWatch;
    FOnSortsComplete: TNotifyEvent;
    procedure RandomizeArrays;
    procedure ThreadDone(Sender: TObject);
  public
    procedure RunSorts;
  published
    property OnSortsComplete(BubbleTime, SelectionTime, QuickTime : Extended)
             read FOnSortsComplete write FOnSortsComplete;
  end;

implementation

end.
