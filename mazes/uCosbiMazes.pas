unit uCosbiMazes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uTests, uOutput, Gauges, GR32, uCOSBI_SystemInfo,
  COSBI_Common, uStopWatch;

type
  TfrmCosbiMazes = class(TForm)
    btnGo: TButton;
    rbOneThread: TRadioButton;
    rbTwoThreads: TRadioButton;
    procedure btnGoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fStopWatch1: TStopWatch;
//    fStopWatch2: TStopWatch;
  public
    { Public declarations }
  end;

var
  frmCosbiMazes: TfrmCosbiMazes;

implementation

{$R *.dfm}

procedure TfrmCosbiMazes.btnGoClick(Sender: TObject);
var
  lfrmOutput1 : TfrmOutput;
  lfrmOutput2 : TfrmOutput;
  liNbrOfThreads : integer;
  lbKillForm2 : Boolean;
begin
      lbKillForm2 := FALSE;
      with TMazeThreadTest.Create do begin
        try
          lfrmOutput1 := TfrmOutput.Create( self );
          lfrmOutput1.Position := poDesigned;
          lfrmOutput1.Width := lfrmOutput1.Width div 2;
          lfrmOutput1.Left := 10;
          lfrmOutput1.Top := 20;
          OutputForm1 := lfrmOutput1;
          lfrmOutput1.Visible := TRUE;
          PaintBox32 := lfrmOutput1.PaintBoxOutput;
          StopWatch := fStopWatch1;
          liNbrOfThreads := 1;
          if rbTwoThreads.Checked then begin
            lfrmOutput2 := TfrmOutput.Create( self );
            lbKillForm2 := TRUE;
            lfrmOutput2.Position := poDesigned;
            lfrmOutput2.Width := lfrmOutput1.Width;
            lfrmOutput2.Top := lfrmOutput1.Top;
            lfrmOutput2.Left := lfrmOutput1.Left + lfrmOutput1.Width + 1;
            lfrmOutput2.Visible := TRUE;
            OutputForm2 := lfrmOutput2;
            liNbrOfThreads := 2;
          end; // if rbTwoThreads
//          for i := 1 to iterate do begin
            NbrOfThreads := liNbrOfThreads;
            Run;
//            pBar.Progress := pBar.Progress + 1;
            Application.ProcessMessages;
            Sleep(100);
//          end; // for
//          memoResults.Lines.Add(TestName + ', ' + IntToStr(GetScore(0)) + ', ' +
//            GetFormattedMinTime);
        finally
          FreeAndNil(  lfrmOutput1 );
          if lbKillForm2 then FreeAndNil(  lfrmOutput2 );
          Free;
        end;
      end;//with
//    end; //if

end;

procedure TfrmCosbiMazes.FormCreate(Sender: TObject);
begin
  fStopWatch1 := TStopWatch.Create;
  fStopWatch1.Resolution := swresHigh;
//  fStopWatch2 := TStopWatch.Create;
//  fStopWatch2.Resolution := swresHigh;
  Left := 0; Top := 0;
end;

end.
