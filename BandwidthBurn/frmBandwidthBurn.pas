unit frmBandwidthBurn;
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
  Gauges, StdCtrls, COSBI_Common, ExtCtrls, TeEngine, Series, TeeProcs,
  Chart, Mask, jpeg, uCOSBI_SystemInfo, uBandwidthBurn, ComCtrls, uStopWatch;

const
  ALL_BITS_INT  = $FFFFFFFF;
  ALL_BITS_CARD = $FFFFFFFF;
  ASSIGN_VERIFY_SIMPLE = 'Verify Simple';
  REPEAT_COUNT  = 20;
  GRAPH_PAD_MAX = 1.01;
  VERSION       = '0.93';
  ONE_KB        = 1024;
  CR_LF         = #13 + #10;
  GRAPH_FILE_SUFFIX = '.bmp';
  FILE_SUFFIX   = '.txt';
  GRAPH_Y_AXIS_LABEL  = 'Array Size (kB)';
  GRAPH_X_AXIS_LABEL = 'Bandwidth (MB/s = 10^6 B/s)';
  GRAPH_TITLE = 'COSBI BandwidthBurn v' + VERSION +
                ' -- Bandwidth versus Dataset size';
  MAX_DATASET_THRESHOLD = 3072;


type

  EReadDoesNotMatchWrite = Class( Exception );

  TfrmCOSBIBandwidthBurn = class(TForm)
    GaugeProgress: TGauge;
    btnBandwidth: TButton;
    chartBandwidth: TChart;
    lblThroughput: TLabel;
    cboIterations: TComboBox;
    cboxGraphRealTime: TCheckBox;
    btnScale: TButton;
    lblStepSize: TLabel;
    cboStepSize: TComboBox;
    Series1: TLineSeries;
    cboAssign: TComboBox;
    Label1: TLabel;
    btnSave: TButton;
    edSave: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    cboMaxDataset: TComboBox;
    Label4: TLabel;
    edSaveGraph: TEdit;
    btnSaveGraph: TButton;
    btnBrowseSaveData: TButton;
    btnBrowseSaveGraph: TButton;
    OpenDialog1: TOpenDialog;
    btnChangeBackground: TButton;
    SaveDialog1: TSaveDialog;
    Bevel1: TBevel;
    Bevel2: TBevel;
    StatusBar1: TStatusBar;
    cboxSpinUp: TCheckBox;
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnBandwidthClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnScaleClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cboAssignChange(Sender: TObject);
    procedure cboIterationsChange(Sender: TObject);
    procedure cboStepSizeChange(Sender: TObject);
    procedure cboxGraphRealTimeClick(Sender: TObject);
    procedure cboMaxDatasetChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnSaveGraphClick(Sender: TObject);
    procedure btnBrowseSaveDataClick(Sender: TObject);
    procedure btnBrowseSaveGraphClick(Sender: TObject);
    procedure btnChangeBackgroundClick(Sender: TObject);
  private
    { Private declarations }
    StopWatch                 : TStopWatch;
    SWOverhead                : Extended;
    fAssignType               : TAssignType;
    fDummy                    : integer;
    fSeries                   : TChartSeries;
    fGraphRealtime            : Boolean;
    fInterationsPerStep       : integer;
    fStepSize                 : integer;
    fMaxDatasetSize           : integer;
    fBreak                    : Boolean;
    fBandwidthBurn            : TBandwidthBurn;
    fPanelStatus              : TStatusPanel;
    fPanelCopyTime            : TStatusPanel;
    fPanelMinMax              : TStatusPanel;
    fPanelLoop                : TStatusPanel;
    fSystemInfo               : TSystemInfo;
    fFreeOnClose              : Boolean;
    procedure DisableButtons;
    procedure EnableButtons;
    procedure MeasureSWOverhead;
    procedure GraphMemoryBandwidth;
    procedure ResetGraph;
    function  ReadWriteTest(var A : TBigArray;
                            var ai_RepeatCount : integer;
                            var ai_ArraySize   : integer
                            ): Extended;
    function  ReadWriteMulTest(var A : TBigArray;
                              var ai_RepeatCount : integer;
                              var ai_ArraySize   : integer
                              ): Extended;
    function  ReadTest(var A : TBigArray;
                       var ai_RepeatCount : integer;
                       var ai_ArraySize   : integer
                       ): Extended;
    function  WriteTest(var A : TBigArray;
                        var ai_RepeatCount : integer;
                        var ai_ArraySize   : integer
                        ): Extended;
    function BlockPrefetch32(var A : TBigArray;
                             var ai_RepeatCount : integer;
                             var ai_ArraySize   : integer
                             ): Extended;
    function BlockPrefetch64(var A : TBigArray;
                             var ai_RepeatCount : integer;
                             var ai_ArraySize   : integer
                             ): Extended;
    function BP32Write(var A : TBigArray;
                       var ai_RepeatCount : integer;
                       var ai_ArraySize   : integer
                       ): Extended;
    function ReadWriteVerifyTest(var A : TBigArray;
                                 var ai_RepeatCount : integer;
                                 var ai_ArraySize   : integer
                                 ): Extended;
    function Skip1Line( var A : TBigArray;
                        var ai_RepeatCount : integer;
                        var ai_ArraySize   : integer
                       ): Extended;

    function  GetStatus: string;
    procedure SetStatus( Value : string );
    procedure SetGraphRealtime(Value: Boolean);
    function  GetAssignmentType: TAssignType;
    function  GetGraphRealtime: Boolean;
    procedure SetAssignmentType(Value: TAssignType);
    function  GetIterationsPerStep: integer;
    procedure SetIterationsPerStep(Value: Integer);
    function  GetFilename: string;
    procedure SetFilename(Value: string);
    function  GetStepSize: integer;
    procedure SetStepSize(Value: integer);
    function  GetMaxDatasetSize: integer;
    procedure SetMaxDatasetSize(Value: integer);
    procedure InitializeValues;
    procedure ClearText;
    procedure RandomizeBigArray(var A: TBigArray);
    procedure ParseCommandLine;
    procedure SaveData(astrSaveMsg : string);
//    procedure 
  public
    { Public declarations }
    procedure MeasureBandwidth;
    property Status: string                 read GetStatus
                                            write SetStatus;
    property AssignmentType: TAssignType    read  GetAssignmentType
                                            write SetAssignmentType;
    property GraphRealtime: Boolean         read  GetGraphRealtime
                                            write SetGraphRealtime;
    property IterationsPerStep: integer     read  GetIterationsPerStep
                                            write SetIterationsPerStep;
    property Filename: string               read  GetFilename
                                            write SetFilename;
    property StepSize: integer              read  GetStepSize
                                            write SetStepSize;
    property MaxDatasetSize: integer        read  GetMaxDatasetSize
                                            write SetMaxDatasetSize;
    property SystemInfo: TSystemInfo        read  fSystemInfo
                                            write  fSystemInfo;
    property FreeOnClose : Boolean read fFreeOnClose write fFreeOnClose;
    end;

var
  frmCOSBIBandwidthBurn: TfrmCOSBIBandwidthBurn;

implementation

{$R *.DFM}

function TfrmCOSBIBandwidthBurn.GetStatus: string;
begin
  result := fPanelStatus.Text;
end;

procedure TfrmCOSBIBandwidthBurn.SetStatus( Value : string );
begin
  fPanelStatus.Text := Value;
end;

procedure TfrmCOSBIBandwidthBurn.btnBandwidthClick(Sender: TObject);
begin
  AlphaBlend := FALSE;
  MeasureBandwidth;
  AlphaBlend := TRUE;
end;

procedure TfrmCOSBIBandwidthBurn.MeasureBandwidth;
begin
  DisableButtons;
  ClearText;
  try
    try
      GraphMemoryBandwidth;
    except
      on E: Exception do begin
        ShowMessage( E.Message );
        Status := 'Status: ERROR -- ' + E.Message;
      end;
    end;
  finally
    EnableButtons;
  end; // try...finally
end; // procedure TfrmCOSBIBandwidthBurn.MeasureBandwidth;


procedure TfrmCOSBIBandwidthBurn.FormCreate(Sender: TObject);
begin
  fFreeOnClose := TRUE;
  fBandwidthBurn := TBandwidthBurn.Create;
  fBreak := FALSE; // allows user to break out of the program.
  StopWatch := TStopWatch.Create;
  StopWatch.Resolution := swresCPU;
//  fSystemInfo := TSystemInfo.Create( self );
//  fSystemInfo.ShowStatus := TRUE;
//  fSystemInfo.StopWatch := StopWatch;
//  fSystemInfo.Initialize;
  MeasureSWOverhead;
  fBandwidthBurn.StopWatch := StopWatch;
  Caption := Caption + ', Version ' + VERSION;
  Caption := Caption + ' -- Your CPU Speed is ' +
    FloatToStrF( Round( StopWatch.GetCPUClockspeed(FALSE) ), ffNumber, 10, 0 ) +
            ' Hz';
  fSeries := chartBandwidth.SeriesList[0];
  fPanelStatus := StatusBar1.Panels[0];
  fPanelCopyTime := StatusBar1.Panels[1];
  fPanelMinMax := StatusBar1.Panels[2];
  fPanelLoop := StatusBar1.Panels[3];
  InitializeValues;
  if ParamCount > 0 then begin
    ParseCommandLine;
  end; // if
end;

procedure TfrmCOSBIBandwidthBurn.ParseCommandLine;
var
  i             : integer;
  lb_Run        : Boolean;
  lb_Save       : Boolean;
  ls_SaveMsg    : string;
  li_LoopCount  : integer;
begin
  i := 1;
  lb_Run  := FALSE;
  lb_Save := FALSE;
  li_LoopCount := 1;
  while i <= ParamCount do begin
    if LowerCase( ParamStr(i) ) = 'v' then begin
      AssignmentType := assVerifySimple;
      GraphRealTime     := TRUE;
      IterationsPerStep := 500;
      MaxDatasetSize := 1024;
      lb_Run := TRUE;
    end else if LowerCase( ParamStr(i) ) = 's' then begin
      AssignmentType := assSimple;
      lb_Run := TRUE;
    end else if LowerCase( ParamStr(i) ) = 'bp32' then begin
      AssignmentType := assBlockPrefetch32;
      lb_Run := TRUE;
    end else if LowerCase( ParamStr(i) ) = 'bp64' then begin
      AssignmentType := assBlockPrefetch64;
      lb_Run := TRUE;
    end else if LowerCase( ParamStr(i) ) = 'oos' then begin
      AssignmentType := assOOSimple;
      lb_Run := TRUE;
    end else if LowerCase( ParamStr(i) ) = 'oobp32' then begin
      AssignmentType := assOOBP32;
      lb_Run := TRUE;
    end else if LowerCase( ParamStr(i) ) = 'oobp64' then begin
      AssignmentType := assOOBP64;
      lb_Run := TRUE;
    end else if LowerCase( ParamStr(i) ) = 'oobp128' then begin
      AssignmentType := assOOBP128;
      lb_Run := TRUE;
    end else if LowerCase( ParamStr(i) ) = 'stl' then begin
      AssignmentType := assOOSimpleTightLoop;
      lb_Run := TRUE;
    end else if LowerCase( ParamStr(i) ) = 'sf' then begin
      lb_Save := TRUE;
    end else if LowerCase( ParamStr(i) ) = 'g' then begin
      GraphRealtime := TRUE;
    end else if LowerCase( ParamStr(i) ) = 'gn' then begin
      GraphRealtime := FALSE;
    end else if LowerCase( ParamStr(i) ) = 'm' then begin
      i := i + 1;
      MaxDatasetSize := StrToInt( ParamStr( i ) );
    end else if LowerCase( ParamStr(i) ) = 'i' then begin
      i := i + 1;
      IterationsPerStep := StrToInt( ParamStr( i ) );
    end else if LowerCase( ParamStr(i) ) = 'fn' then begin
      i := i + 1;
      lb_Save := TRUE;
      edSave.Text := ParamStr( i );
    end else if LowerCase( ParamStr(i) ) = 'ss' then begin
      i := i + 1;
      StepSize := StrToInt( ParamStr( i ) );
    end else if LowerCase( ParamStr(i) ) = 'l' then begin
      i := i + 1;
      li_LoopCount := StrToInt( ParamStr( i ) );
    end else begin
      ShowMessage( 'Commandline parameters: v=verify;' + CR_LF +
        's=simple; bp32=BlockPrefetch32; bp64=BlockPrefetch64;' + CR_LF +
        'oos=Object-Oriented Simple; oobp32=Object-Oriented BlockPrefetch32' + CR_LF +
        'oobp64=Object-Oriented BlockPrefetch64' + CR_LF +
        'oobp128=Object-Oriented BlockPrefetch128' + CR_LF +
        'stl=Object-Oriented Simple Tight Loop' + CR_LF +
        'sf=Save after run; g=graph real-time; gn=don''t graph real-time' + CR_LF +
        'm nnnn=Max dataset of nnnn kB; i nnnn=nnnn iterations per step' + CR_LF +
        'ss nn = nn kb step size; l n=loop n times; fn "filename" = output filename' );
      exit;
    end; // if
    i := i + 1;
  end; // while
  if lb_Run then begin
    Show;
    Application.ProcessMessages;
    try
      for i := 1 to li_LoopCount do begin
        fPanelLoop.Text := 'Loop ' + IntToStr( i ) + ' of ' + IntToStr( li_LoopCount );
        MeasureBandwidth;
      end;
    except
      on E: Exception do begin
        ls_SaveMsg := 'ERROR during loop: ' + IntToStr(li_LoopCount) + CR_LF +
          'Error message: ' + E.Message;
      end;
    end; //try...except
    if lb_Save then SaveData(ls_SaveMsg);
    Sleep(1000);
    Application.Terminate;
  end;
end; // procedure TfrmCOSBIBandwidthBurn.ParseCommandLine;

procedure TfrmCOSBIBandwidthBurn.MeasureSWOverhead;
var
  n                 : integer;
  li64_ClockCycles  : Int64;
begin
  SWOverhead := 0;
  for n := 1 to 100 do begin
    li64_ClockCycles := GetCycleCount;
    SWOverhead := (GetCycleCount - li64_ClockCycles) + SWOverhead;
  end;
  SWOverhead := SWOverhead / 100;
end;


procedure TfrmCOSBIBandwidthBurn.FormDestroy(Sender: TObject);
begin
  FreeAndNil(StopWatch);
  FreeAndNil(fBandwidthBurn);
end;

procedure TfrmCOSBIBandwidthBurn.btnScaleClick(Sender: TObject);
begin
  with chartBandwidth.LeftAxis do begin
    if Automatic then begin
      Automatic := FALSE;
      Minimum := 0;
    end else begin
      Automatic := TRUE;
    end;
  end;
end; // procedure TfrmMemLate.btnScaleClick

procedure TfrmCOSBIBandwidthBurn.btnSaveClick(Sender: TObject);
begin
  SaveData('');
end;

procedure TfrmCOSBIBandwidthBurn.SaveData(astrSaveMsg : string);
var
  ltxtfile_out  : TextFile;
  ls_FileName   : string;
  ls_OutLine    : string;
  ld_x          : double;
  n             : integer;
const
  DEFAULT_FILENAME = 'BB_OUT';
begin
  ls_FileName := Trim( edSave.Text );
  if ls_FileName = '' then begin
    ls_FileName := DEFAULT_FILENAME;
  end else begin
    if Pos('.', ls_FileName) = 0 then begin
      ls_FileName := ls_FileName + FILE_SUFFIX;
    end; // if
  end; // if ls_FileName = ''
  AssignFile( ltxtfile_out, ls_FileName );
  Rewrite( ltxtfile_out );
  WriteLn( ltxtfile_out, 'Bandwidth Burn v' + VERSION + ' Output File' );
  WriteLn( ltxtfile_out, '' );
  if astrSaveMsg <> '' then begin
    WriteLn( ltxtfile_out, astrSaveMsg );
    WriteLn( ltxtfile_out, '' );
  end;
  if not assigned( fSystemInfo ) then begin
    fSystemInfo := TSystemInfo.Create( self );
    fSystemInfo.ShowStatus := TRUE;
    fSystemInfo.StopWatch := StopWatch;
    fSystemInfo.Initialize;
  end; // if
  WriteLn( fSystemInfo.GetSystemSummary );
  WriteLn( ltxtfile_out, '' );
  ls_OutLine := 'Assignment: ' + cboAssign.Text + ', Step Size(B): ' +
                cboStepSize.Text + ', Iterations/step: ' + cboIterations.Text;
  WriteLn( ltxtfile_out, ls_OutLine );
  ls_OutLine := 'Date: ' + DateToStr( Date ) + ', Time: ' + TimeToStr( Time );
  WriteLn( ltxtfile_out, ls_OutLine );
  WriteLn( ltxtfile_out, '' );
  WriteLn( ltxtfile_out, 'Size(kB)  Bandwidth(MB/s)');
  WriteLn( ltxtfile_out, '========  ===============');
  for n := 0 to fSeries.Count - 1 do begin
//    ld_x := StrToFloat( fSeries.XLabel[n] );
//    ls_Outline := Format('%6n' + Chr(9) + '   %11n', [ld_x, fSeries.YValue[n]]);
    ls_Outline := fSeries.XLabel[n] + Chr(9) + '     ' + IntToStr( round( fSeries.YValue[n] ) );
    WriteLn( ltxtfile_out, ls_Outline);
  end;
  CloseFile( ltxtfile_out );
end;

function TfrmCOSBIBandwidthBurn.ReadWriteTest(var A : TBigArray;
                                   var ai_RepeatCount : integer;
                                   var ai_ArraySize   : integer
                                   ): Extended;
var
  li64_StartCycle   : Int64;
  li64EndCycle      : Int64;
  liRandomData      : integer;
  lcTemp            : cardinal;
  i, j              : integer;
  liIterate         : integer;
  leElapsedTime     : extended;
  leAccumulatedTime : extended;
  leMinTime         : extended;
  leClockspeed      : extended;
begin
  // initialize variables
  leClockspeed := StopWatch.GetCPUClockspeed(FALSE);
  leElapsedTime := 0;
  leMinTime := 99999999999999;
  lcTemp := 0;
  liRandomData   := Random(1000);
  for liIterate := 1 to ai_RepeatCount do begin
    leAccumulatedTime := 0;
    // at each step we need to repeat populating the array:
    i := 1;
    while i <= ai_ArraySize do begin
      // start timer
      li64_StartCycle := GetCycleCount;
      // copy data
      A[i] := liRandomData;
      A[i + 1] := liRandomData;
      A[i + 2] := liRandomData;
      A[i + 3] := liRandomData;
      A[i + 4] := liRandomData;

      A[i + 5] := liRandomData;
      A[i + 6] := liRandomData;
      A[i + 7] := liRandomData;
      A[i + 8] := liRandomData;
      A[i + 9] := liRandomData;

      A[i + 10] := liRandomData;
      A[i + 11] := liRandomData;
      A[i + 12] := liRandomData;
      A[i + 13] := liRandomData;
      A[i + 14] := liRandomData;

      A[i + 15] := liRandomData;
      A[i + 16] := liRandomData;
      A[i + 17] := liRandomData;
      A[i + 18] := liRandomData;
      A[i + 19] := liRandomData;

      A[i + 20] := liRandomData;
      A[i + 21] := liRandomData;
      A[i + 22] := liRandomData;
      A[i + 23] := liRandomData;
      A[i + 24] := liRandomData;

      A[i + 25] := liRandomData;
      A[i + 26] := liRandomData;
      A[i + 27] := liRandomData;
      A[i + 28] := liRandomData;
      A[i + 29] := liRandomData;

      A[i + 30] := liRandomData;
      A[i + 31] := liRandomData;
      A[i + 32] := liRandomData;
      A[i + 33] := liRandomData;
      A[i + 34] := liRandomData;

      A[i + 35] := liRandomData;
      A[i + 36] := liRandomData;
      A[i + 37] := liRandomData;
      A[i + 38] := liRandomData;
      A[i + 39] := liRandomData;

      A[i + 40] := liRandomData;
      A[i + 41] := liRandomData;
      A[i + 42] := liRandomData;
      A[i + 43] := liRandomData;
      A[i + 44] := liRandomData;

      A[i + 45] := liRandomData;
      A[i + 46] := liRandomData;
      A[i + 47] := liRandomData;
      A[i + 48] := liRandomData;
      A[i + 49] := liRandomData;

      A[i + 50] := liRandomData;
      A[i + 51] := liRandomData;
      A[i + 52] := liRandomData;
      A[i + 53] := liRandomData;
      A[i + 54] := liRandomData;

      A[i + 55] := liRandomData;
      A[i + 56] := liRandomData;
      A[i + 57] := liRandomData;
      A[i + 58] := liRandomData;
      A[i + 59] := liRandomData;

      A[i + 60] := liRandomData;
      A[i + 61] := liRandomData;
      A[i + 62] := liRandomData;
      A[i + 63] := liRandomData;
      A[i + 64] := liRandomData;

      A[i + 65] := liRandomData;
      A[i + 66] := liRandomData;
      A[i + 67] := liRandomData;
      A[i + 68] := liRandomData;
      A[i + 69] := liRandomData;

      A[i + 70] := liRandomData;
      A[i + 71] := liRandomData;
      A[i + 72] := liRandomData;
      A[i + 73] := liRandomData;
      A[i + 74] := liRandomData;

      A[i + 75] := liRandomData;
      A[i + 76] := liRandomData;
      A[i + 77] := liRandomData;
      A[i + 78] := liRandomData;
      A[i + 79] := liRandomData;

      A[i + 80] := liRandomData;
      A[i + 81] := liRandomData;
      A[i + 82] := liRandomData;
      A[i + 83] := liRandomData;
      A[i + 84] := liRandomData;

      A[i + 85] := liRandomData;
      A[i + 86] := liRandomData;
      A[i + 87] := liRandomData;
      A[i + 88] := liRandomData;
      A[i + 89] := liRandomData;

      A[i + 90] := liRandomData;
      A[i + 91] := liRandomData;
      A[i + 92] := liRandomData;
      A[i + 93] := liRandomData;
      A[i + 94] := liRandomData;

      A[i + 95] := liRandomData;
      A[i + 96] := liRandomData;
      A[i + 97] := liRandomData;
      A[i + 98] := liRandomData;
      A[i + 99] := liRandomData;

      A[i + 100] := liRandomData;
      A[i + 101] := liRandomData;
      A[i + 102] := liRandomData;
      A[i + 103] := liRandomData;
      A[i + 104] := liRandomData;

      A[i + 105] := liRandomData;
      A[i + 106] := liRandomData;
      A[i + 107] := liRandomData;
      A[i + 108] := liRandomData;
      A[i + 109] := liRandomData;

      A[i + 110] := liRandomData;
      A[i + 111] := liRandomData;
      A[i + 112] := liRandomData;
      A[i + 113] := liRandomData;
      A[i + 114] := liRandomData;

      A[i + 115] := liRandomData;
      A[i + 116] := liRandomData;
      A[i + 117] := liRandomData;
      A[i + 118] := liRandomData;
      A[i + 119] := liRandomData;

      A[i + 120] := liRandomData;
      A[i + 121] := liRandomData;
      A[i + 122] := liRandomData;
      A[i + 123] := liRandomData;
      A[i + 124] := liRandomData;

      A[i + 125] := liRandomData;
      A[i + 126] := liRandomData;
      A[i + 127] := liRandomData;

      li64EndCycle := GetCycleCount;
      // calculate elapsed time:
      leElapsedTime :=
        ( ( li64EndCycle - li64_StartCycle ) - SWOverhead ) / leClockspeed;
      leAccumulatedTime := leAccumulatedTime + leElapsedTime;
      // increment index:
      i := i + 128;
    end; // while...

    // read back data: must accumulate or optimizer will remove
    lcTemp := 0;
    j := 1;
    while j <= ai_ArraySize do begin
      // start timer
      li64_StartCycle := GetCycleCount;

      // copy data: must accumulate or optimizer will remove
      lcTemp := A[j] + lcTemp;
      lcTemp := A[j+1] - lcTemp;
      lcTemp := A[j+2] + lcTemp;
      lcTemp := A[j+3] - lcTemp;
      lcTemp := A[j+4] + lcTemp;

      lcTemp := A[j+5] - lcTemp;
      lcTemp := A[j+6] + lcTemp;
      lcTemp := A[j+7] - lcTemp;
      lcTemp := A[j+8] + lcTemp;
      lcTemp := A[j+9] - lcTemp;

      lcTemp := A[j+10] + lcTemp;
      lcTemp := A[j+11] - lcTemp;
      lcTemp := A[j+12] + lcTemp;
      lcTemp := A[j+13] - lcTemp;
      lcTemp := A[j+14] + lcTemp;

      lcTemp := A[j+15] - lcTemp;
      lcTemp := A[j+16] + lcTemp;
      lcTemp := A[j+17] - lcTemp;
      lcTemp := A[j+18] + lcTemp;
      lcTemp := A[j+19] - lcTemp;

      lcTemp := A[j+20] + lcTemp;
      lcTemp := A[j+21] - lcTemp;
      lcTemp := A[j+22] + lcTemp;
      lcTemp := A[j+23] - lcTemp;
      lcTemp := A[j+24] + lcTemp;

      lcTemp := A[j+25] - lcTemp;
      lcTemp := A[j+26] + lcTemp;
      lcTemp := A[j+27] - lcTemp;
      lcTemp := A[j+28] + lcTemp;
      lcTemp := A[j+29] - lcTemp;

      lcTemp := A[j+30] + lcTemp;
      lcTemp := A[j+31] - lcTemp;
      lcTemp := A[j+32] + lcTemp;
      lcTemp := A[j+33] - lcTemp;
      lcTemp := A[j+34] + lcTemp;

      lcTemp := A[j+35] - lcTemp;
      lcTemp := A[j+36] + lcTemp;
      lcTemp := A[j+37] - lcTemp;
      lcTemp := A[j+38] + lcTemp;
      lcTemp := A[j+39] - lcTemp;

      lcTemp := A[j+40] + lcTemp;
      lcTemp := A[j+41] - lcTemp;
      lcTemp := A[j+42] + lcTemp;
      lcTemp := A[j+43] - lcTemp;
      lcTemp := A[j+44] + lcTemp;

      lcTemp := A[j+45] - lcTemp;
      lcTemp := A[j+46] + lcTemp;
      lcTemp := A[j+47] - lcTemp;
      lcTemp := A[j+48] + lcTemp;
      lcTemp := A[j+49] - lcTemp;

      lcTemp := A[j+50] + lcTemp;
      lcTemp := A[j+51] - lcTemp;
      lcTemp := A[j+52] + lcTemp;
      lcTemp := A[j+53] - lcTemp;
      lcTemp := A[j+54] + lcTemp;

      lcTemp := A[j+55] - lcTemp;
      lcTemp := A[j+56] + lcTemp;
      lcTemp := A[j+57] - lcTemp;
      lcTemp := A[j+58] + lcTemp;
      lcTemp := A[j+59] - lcTemp;

      lcTemp := A[j+60] + lcTemp;
      lcTemp := A[j+61] - lcTemp;
      lcTemp := A[j+62] + lcTemp;
      lcTemp := A[j+63] - lcTemp;
      lcTemp := A[j+64] + lcTemp;

      lcTemp := A[j+65] - lcTemp;
      lcTemp := A[j+66] + lcTemp;
      lcTemp := A[j+67] - lcTemp;
      lcTemp := A[j+68] + lcTemp;
      lcTemp := A[j+69] - lcTemp;

      lcTemp := A[j+70] + lcTemp;
      lcTemp := A[j+71] - lcTemp;
      lcTemp := A[j+72] + lcTemp;
      lcTemp := A[j+73] - lcTemp;
      lcTemp := A[j+74] + lcTemp;

      lcTemp := A[j+75] - lcTemp;
      lcTemp := A[j+76] + lcTemp;
      lcTemp := A[j+77] - lcTemp;
      lcTemp := A[j+78] + lcTemp;
      lcTemp := A[j+79] - lcTemp;

      lcTemp := A[j+80] + lcTemp;
      lcTemp := A[j+81] - lcTemp;
      lcTemp := A[j+82] + lcTemp;
      lcTemp := A[j+83] - lcTemp;
      lcTemp := A[j+84] + lcTemp;

      lcTemp := A[j+85] - lcTemp;
      lcTemp := A[j+86] + lcTemp;
      lcTemp := A[j+87] - lcTemp;
      lcTemp := A[j+88] + lcTemp;
      lcTemp := A[j+89] - lcTemp;

      lcTemp := A[j+90] + lcTemp;
      lcTemp := A[j+91] - lcTemp;
      lcTemp := A[j+92] + lcTemp;
      lcTemp := A[j+93] - lcTemp;
      lcTemp := A[j+94] + lcTemp;

      lcTemp := A[j+95] - lcTemp;
      lcTemp := A[j+96] + lcTemp;
      lcTemp := A[j+97] - lcTemp;
      lcTemp := A[j+98] + lcTemp;
      lcTemp := A[j+99] - lcTemp;

      lcTemp := A[j+100] + lcTemp;
      lcTemp := A[j+101] - lcTemp;
      lcTemp := A[j+102] + lcTemp;
      lcTemp := A[j+103] - lcTemp;
      lcTemp := A[j+104] + lcTemp;

      lcTemp := A[j+105] - lcTemp;
      lcTemp := A[j+106] + lcTemp;
      lcTemp := A[j+107] - lcTemp;
      lcTemp := A[j+108] + lcTemp;
      lcTemp := A[j+109] - lcTemp;

      lcTemp := A[j+110] + lcTemp;
      lcTemp := A[j+111] - lcTemp;
      lcTemp := A[j+112] + lcTemp;
      lcTemp := A[j+113] - lcTemp;
      lcTemp := A[j+114] + lcTemp;

      lcTemp := A[j+115] - lcTemp;
      lcTemp := A[j+116] + lcTemp;
      lcTemp := A[j+117] - lcTemp;
      lcTemp := A[j+118] + lcTemp;
      lcTemp := A[j+119] - lcTemp;

      lcTemp := A[j+120] + lcTemp;
      lcTemp := A[j+121] - lcTemp;
      lcTemp := A[j+122] + lcTemp;
      lcTemp := A[j+123] - lcTemp;
      lcTemp := A[j+124] + lcTemp;

      lcTemp := A[j+125] - lcTemp;
      lcTemp := A[j+126] + lcTemp;
      lcTemp := A[j+127] - lcTemp;

      li64EndCycle := GetCycleCount;
      // calculate elapsed time:
      leElapsedTime :=
        ( ( li64EndCycle - li64_StartCycle ) - SWOverhead ) / leClockspeed;
      leAccumulatedTime := leAccumulatedTime + leElapsedTime;
      // increment read index:
      j := j + 128;
    end; // while...
    // see if this is the fastest iteration:
    if leMinTime > leAccumulatedTime then begin
      leMinTime := leAccumulatedTime;
    end; // if
  end; // for...
  // assign to dummy variable or optimizer will remove:
  FDummy := lcTemp;
  result := leMinTime;
end; // function

function TfrmCOSBIBandwidthBurn.ReadWriteVerifyTest(var A : TBigArray;
                                   var ai_RepeatCount : integer;
                                   var ai_ArraySize   : integer
                                   ): Extended;
var
  li64_StartCycle   : Int64;
  li64EndCycle     : Int64;
  leElapsedTme   : extended;
  i, j              : integer;
  liIterate        : integer;
  lcTemp           : cardinal;
  lc_AssignValue    : cardinal;

begin
  // initialize variables
  leElapsedTme := 0;
  lcTemp := 0;
  for liIterate := 1 to ai_RepeatCount do begin
    if liIterate mod 2 = 0 then begin
      lc_AssignValue := ALL_BITS_INT;
    end else begin
      lc_AssignValue := 0;
    end;
    // at each step we need to repeat populating the array:
    i := 1;
    while i <= ai_ArraySize do begin
      // start timer
      li64_StartCycle := GetCycleCount;

      // copy data
      A[i] := lc_AssignValue;
      A[i + 1] := lc_AssignValue;
      A[i + 2] := lc_AssignValue;
      A[i + 3] := lc_AssignValue;
      A[i + 4] := lc_AssignValue;

      A[i + 5] := lc_AssignValue;
      A[i + 6] := lc_AssignValue;
      A[i + 7] := lc_AssignValue;
      A[i + 8] := lc_AssignValue;
      A[i + 9] := lc_AssignValue;

      A[i + 10] := lc_AssignValue;
      A[i + 11] := lc_AssignValue;
      A[i + 12] := lc_AssignValue;
      A[i + 13] := lc_AssignValue;
      A[i + 14] := lc_AssignValue;

      A[i + 15] := lc_AssignValue;
      A[i + 16] := lc_AssignValue;
      A[i + 17] := lc_AssignValue;
      A[i + 18] := lc_AssignValue;
      A[i + 19] := lc_AssignValue;

      A[i + 20] := lc_AssignValue;
      A[i + 21] := lc_AssignValue;
      A[i + 22] := lc_AssignValue;
      A[i + 23] := lc_AssignValue;
      A[i + 24] := lc_AssignValue;

      A[i + 25] := lc_AssignValue;
      A[i + 26] := lc_AssignValue;
      A[i + 27] := lc_AssignValue;
      A[i + 28] := lc_AssignValue;
      A[i + 29] := lc_AssignValue;

      A[i + 30] := lc_AssignValue;
      A[i + 31] := lc_AssignValue;
      A[i + 32] := lc_AssignValue;
      A[i + 33] := lc_AssignValue;
      A[i + 34] := lc_AssignValue;

      A[i + 35] := lc_AssignValue;
      A[i + 36] := lc_AssignValue;
      A[i + 37] := lc_AssignValue;
      A[i + 38] := lc_AssignValue;
      A[i + 39] := lc_AssignValue;

      A[i + 40] := lc_AssignValue;
      A[i + 41] := lc_AssignValue;
      A[i + 42] := lc_AssignValue;
      A[i + 43] := lc_AssignValue;
      A[i + 44] := lc_AssignValue;

      A[i + 45] := lc_AssignValue;
      A[i + 46] := lc_AssignValue;
      A[i + 47] := lc_AssignValue;
      A[i + 48] := lc_AssignValue;
      A[i + 49] := lc_AssignValue;

      A[i + 50] := lc_AssignValue;
      A[i + 51] := lc_AssignValue;
      A[i + 52] := lc_AssignValue;
      A[i + 53] := lc_AssignValue;
      A[i + 54] := lc_AssignValue;

      A[i + 55] := lc_AssignValue;
      A[i + 56] := lc_AssignValue;
      A[i + 57] := lc_AssignValue;
      A[i + 58] := lc_AssignValue;
      A[i + 59] := lc_AssignValue;

      A[i + 60] := lc_AssignValue;
      A[i + 61] := lc_AssignValue;
      A[i + 62] := lc_AssignValue;
      A[i + 63] := lc_AssignValue;
      A[i + 64] := lc_AssignValue;

      A[i + 65] := lc_AssignValue;
      A[i + 66] := lc_AssignValue;
      A[i + 67] := lc_AssignValue;
      A[i + 68] := lc_AssignValue;
      A[i + 69] := lc_AssignValue;

      A[i + 70] := lc_AssignValue;
      A[i + 71] := lc_AssignValue;
      A[i + 72] := lc_AssignValue;
      A[i + 73] := lc_AssignValue;
      A[i + 74] := lc_AssignValue;

      A[i + 75] := lc_AssignValue;
      A[i + 76] := lc_AssignValue;
      A[i + 77] := lc_AssignValue;
      A[i + 78] := lc_AssignValue;
      A[i + 79] := lc_AssignValue;

      A[i + 80] := lc_AssignValue;
      A[i + 81] := lc_AssignValue;
      A[i + 82] := lc_AssignValue;
      A[i + 83] := lc_AssignValue;
      A[i + 84] := lc_AssignValue;

      A[i + 85] := lc_AssignValue;
      A[i + 86] := lc_AssignValue;
      A[i + 87] := lc_AssignValue;
      A[i + 88] := lc_AssignValue;
      A[i + 89] := lc_AssignValue;

      A[i + 90] := lc_AssignValue;
      A[i + 91] := lc_AssignValue;
      A[i + 92] := lc_AssignValue;
      A[i + 93] := lc_AssignValue;
      A[i + 94] := lc_AssignValue;

      A[i + 95] := lc_AssignValue;
      A[i + 96] := lc_AssignValue;
      A[i + 97] := lc_AssignValue;
      A[i + 98] := lc_AssignValue;
      A[i + 99] := lc_AssignValue;

      A[i + 100] := lc_AssignValue;
      A[i + 101] := lc_AssignValue;
      A[i + 102] := lc_AssignValue;
      A[i + 103] := lc_AssignValue;
      A[i + 104] := lc_AssignValue;

      A[i + 105] := lc_AssignValue;
      A[i + 106] := lc_AssignValue;
      A[i + 107] := lc_AssignValue;
      A[i + 108] := lc_AssignValue;
      A[i + 109] := lc_AssignValue;

      A[i + 110] := lc_AssignValue;
      A[i + 111] := lc_AssignValue;
      A[i + 112] := lc_AssignValue;
      A[i + 113] := lc_AssignValue;
      A[i + 114] := lc_AssignValue;

      A[i + 115] := lc_AssignValue;
      A[i + 116] := lc_AssignValue;
      A[i + 117] := lc_AssignValue;
      A[i + 118] := lc_AssignValue;
      A[i + 119] := lc_AssignValue;

      A[i + 120] := lc_AssignValue;
      A[i + 121] := lc_AssignValue;
      A[i + 122] := lc_AssignValue;
      A[i + 123] := lc_AssignValue;
      A[i + 124] := lc_AssignValue;

      A[i + 125] := lc_AssignValue;
      A[i + 126] := lc_AssignValue;
      A[i + 127] := lc_AssignValue;

      li64EndCycle := GetCycleCount;
      leElapsedTme := leElapsedTme +
        ( ( li64EndCycle - li64_StartCycle ) - SWOverhead ) /
        StopWatch.GetCPUClockspeed(FALSE);

      i := i + 128;

    end; // while...

    j := 1;
    while j <= ai_ArraySize do begin

      // start timer
      li64_StartCycle := GetCycleCount;

      // verify each array element:
      lcTemp := j;
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp]) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // next five
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end five

      // last 3 (128 altogether)
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      inc( lcTemp );
      if A[lcTemp] <> lc_AssignValue then
        raise EReadDoesNotMatchWrite.Create(
          'Read does not match write. Array index '
          + IntToStr(lcTemp)
          + '. Value written: '
          + IntToStr( lc_AssignValue )
          + '. Value read: '
          + IntToStr( A[lcTemp] ) );
      // end last 3 (128 altogether)

      li64EndCycle := GetCycleCount;
      leElapsedTme := leElapsedTme +
        ( ( li64EndCycle - li64_StartCycle ) - SWOverhead ) /
        StopWatch.GetCPUClockspeed(FALSE);

      j := j + 128;

    end; // while...


  end; // for...

  // assign to dummy variable or optimizer will remove:
  FDummy := lcTemp;
  result := leElapsedTme;

end; // function

procedure TfrmCOSBIBandwidthBurn.RandomizeBigArray(var A: TBigArray);
var
  i: integer;
begin
  for i:= 0 to ARRAY_SIZE do begin
    A[i] := Random(1000);
  end;
end; // procedure TfrmCOSBIBandwidthBurn.RandomizeBigArray

procedure TfrmCOSBIBandwidthBurn.ClearText;
begin
  Status := 'Status: Ready.';
  fPanelMinMax.Text := 'Min/Max:';
  fPanelCopyTime.Text := 'Total Copy Time:';
end;

function TfrmCOSBIBandwidthBurn.GetStepSize: integer;
begin
  result := fStepSize;
end;

procedure TfrmCOSBIBandwidthBurn.SetStepSize(Value: integer);
begin
  cboStepSize.Text := IntToStr( Value );
  fStepSize := Value;
end;

procedure TfrmCOSBIBandwidthBurn.InitializeValues;
begin
  GraphRealtime     := TRUE;
  AssignmentType    := assSimple;
  StepSize          := 1;
  IterationsPerStep := 10;
  MaxDatasetSize    := 512;
end;

function TfrmCOSBIBandwidthBurn.GetFilename: string;
begin
  result := edSave.Text;
end;

procedure TfrmCOSBIBandwidthBurn.SetFilename(Value: string);
begin
  edSave.Text := Value;
end;

function TfrmCOSBIBandwidthBurn.GetAssignmentType: TAssignType;
begin
  result := fAssignType;
end;

function TfrmCOSBIBandwidthBurn.GetGraphRealtime: Boolean;
begin
  result := fGraphRealtime;
end;

procedure TfrmCOSBIBandwidthBurn.SetAssignmentType(Value: TAssignType);
begin
  case Value of
    assMult: cboAssign.Text       := 'Multiply';
    assRead: cboAssign.Text       := 'Read';
    assSimple: cboAssign.Text     := 'Simple';
    assToggle: cboAssign.Text     := 'Toggle';
    assToggle5x: cboAssign.Text   := '5xToggle';
    assWrite: cboAssign.Text      := 'Write';
    assBlockPrefetch32: cboAssign.Text    := 'Block Prefetch32';
    assBlockPrefetch64: cboAssign.Text    := 'Block Prefetch64';
    assBP32Write: cboAssign.Text  := 'BP32 Write';
    assVerifySimple: begin
      cboAssign.Text    := ASSIGN_VERIFY_SIMPLE;
    end;
  else
    Exception.Create('Unknown assignment type');
  end;
  fAssignType := Value;
end;

function TfrmCOSBIBandwidthBurn.GetIterationsPerStep: integer;
begin
  result := fInterationsPerStep;
end;

procedure TfrmCOSBIBandwidthBurn.SetIterationsPerStep(Value: Integer);
begin
  cboIterations.Text  := IntToStr( Value );
  fInterationsPerStep := Value;
end;

procedure TfrmCOSBIBandwidthBurn.SetGraphRealtime(Value: Boolean);
begin
  cboxGraphRealTime.OnClick := nil;
  cboxGraphRealTime.Checked := Value;
  cboxGraphRealTime.OnClick := cboxGraphRealTimeClick;
  fGraphRealtime := Value;
end;

function TfrmCOSBIBandwidthBurn.ReadTest(var A : TBigArray;
                              var ai_RepeatCount : integer;
                              var ai_ArraySize   : integer
                              ): Extended;
var
  i                 : integer;
  liIterate        : integer;
  li64StartCycle   : Int64;
  li64EndCycle     : Int64;
  lcTemp           : cardinal;
  leElapsedTime     : extended;
  leAccumulatedTime : extended;
  leMinTime         : extended;
  leClockspeed      : extended;


begin
  // initialize variables
  leClockspeed := StopWatch.GetCPUClockspeed(FALSE);
  leElapsedTime := 0;
  leMinTime := 99999999999999;
  leElapsedTime := 0;
  lcTemp := 0;

  for liIterate := 1 to ai_RepeatCount do begin
    leAccumulatedTime := 0;
    // this is the critical loop where we populate the array:
    i := 1;
    while i <= ai_ArraySize do begin
      // start timer
      li64StartCycle := GetCycleCount;

      // copy data: must accumulate or optimizer will remove
      lcTemp := A[i] + lcTemp;
      lcTemp := A[i+1] - lcTemp;
      lcTemp := A[i+2] + lcTemp;
      lcTemp := A[i+3] - lcTemp;
      lcTemp := A[i+4] + lcTemp;

      lcTemp := A[i+5] - lcTemp;
      lcTemp := A[i+6] + lcTemp;
      lcTemp := A[i+7] - lcTemp;
      lcTemp := A[i+8] + lcTemp;
      lcTemp := A[i+9] - lcTemp;

      lcTemp := A[i+10] + lcTemp;
      lcTemp := A[i+11] - lcTemp;
      lcTemp := A[i+12] + lcTemp;
      lcTemp := A[i+13] - lcTemp;
      lcTemp := A[i+14] + lcTemp;

      lcTemp := A[i+15] - lcTemp;
      lcTemp := A[i+16] + lcTemp;
      lcTemp := A[i+17] - lcTemp;
      lcTemp := A[i+18] + lcTemp;
      lcTemp := A[i+19] - lcTemp;

      lcTemp := A[i+20] + lcTemp;
      lcTemp := A[i+21] - lcTemp;
      lcTemp := A[i+22] + lcTemp;
      lcTemp := A[i+23] - lcTemp;
      lcTemp := A[i+24] + lcTemp;

      lcTemp := A[i+25] - lcTemp;
      lcTemp := A[i+26] + lcTemp;
      lcTemp := A[i+27] - lcTemp;
      lcTemp := A[i+28] + lcTemp;
      lcTemp := A[i+29] - lcTemp;

      lcTemp := A[i+30] + lcTemp;
      lcTemp := A[i+31] - lcTemp;
      lcTemp := A[i+32] + lcTemp;
      lcTemp := A[i+33] - lcTemp;
      lcTemp := A[i+34] + lcTemp;

      lcTemp := A[i+35] - lcTemp;
      lcTemp := A[i+36] + lcTemp;
      lcTemp := A[i+37] - lcTemp;
      lcTemp := A[i+38] + lcTemp;
      lcTemp := A[i+39] - lcTemp;

      lcTemp := A[i+40] + lcTemp;
      lcTemp := A[i+41] - lcTemp;
      lcTemp := A[i+42] + lcTemp;
      lcTemp := A[i+43] - lcTemp;
      lcTemp := A[i+44] + lcTemp;

      lcTemp := A[i+45] - lcTemp;
      lcTemp := A[i+46] + lcTemp;
      lcTemp := A[i+47] - lcTemp;
      lcTemp := A[i+48] + lcTemp;
      lcTemp := A[i+49] - lcTemp;

      lcTemp := A[i+50] + lcTemp;
      lcTemp := A[i+51] - lcTemp;
      lcTemp := A[i+52] + lcTemp;
      lcTemp := A[i+53] - lcTemp;
      lcTemp := A[i+54] + lcTemp;

      lcTemp := A[i+55] - lcTemp;
      lcTemp := A[i+56] + lcTemp;
      lcTemp := A[i+57] - lcTemp;
      lcTemp := A[i+58] + lcTemp;
      lcTemp := A[i+59] - lcTemp;

      lcTemp := A[i+60] + lcTemp;
      lcTemp := A[i+61] - lcTemp;
      lcTemp := A[i+62] + lcTemp;
      lcTemp := A[i+63] - lcTemp;
      lcTemp := A[i+64] + lcTemp;

      lcTemp := A[i+65] - lcTemp;
      lcTemp := A[i+66] + lcTemp;
      lcTemp := A[i+67] - lcTemp;
      lcTemp := A[i+68] + lcTemp;
      lcTemp := A[i+69] - lcTemp;

      lcTemp := A[i+70] + lcTemp;
      lcTemp := A[i+71] - lcTemp;
      lcTemp := A[i+72] + lcTemp;
      lcTemp := A[i+73] - lcTemp;
      lcTemp := A[i+74] + lcTemp;

      lcTemp := A[i+75] - lcTemp;
      lcTemp := A[i+76] + lcTemp;
      lcTemp := A[i+77] - lcTemp;
      lcTemp := A[i+78] + lcTemp;
      lcTemp := A[i+79] - lcTemp;

      lcTemp := A[i+80] + lcTemp;
      lcTemp := A[i+81] - lcTemp;
      lcTemp := A[i+82] + lcTemp;
      lcTemp := A[i+83] - lcTemp;
      lcTemp := A[i+84] + lcTemp;

      lcTemp := A[i+85] - lcTemp;
      lcTemp := A[i+86] + lcTemp;
      lcTemp := A[i+87] - lcTemp;
      lcTemp := A[i+88] + lcTemp;
      lcTemp := A[i+89] - lcTemp;

      lcTemp := A[i+90] + lcTemp;
      lcTemp := A[i+91] - lcTemp;
      lcTemp := A[i+92] + lcTemp;
      lcTemp := A[i+93] - lcTemp;
      lcTemp := A[i+94] + lcTemp;

      lcTemp := A[i+95] - lcTemp;
      lcTemp := A[i+96] + lcTemp;
      lcTemp := A[i+97] - lcTemp;
      lcTemp := A[i+98] + lcTemp;
      lcTemp := A[i+99] - lcTemp;

      lcTemp := A[i+100] + lcTemp;
      lcTemp := A[i+101] - lcTemp;
      lcTemp := A[i+102] + lcTemp;
      lcTemp := A[i+103] - lcTemp;
      lcTemp := A[i+104] + lcTemp;

      lcTemp := A[i+105] - lcTemp;
      lcTemp := A[i+106] + lcTemp;
      lcTemp := A[i+107] - lcTemp;
      lcTemp := A[i+108] + lcTemp;
      lcTemp := A[i+109] - lcTemp;

      lcTemp := A[i+110] + lcTemp;
      lcTemp := A[i+111] - lcTemp;
      lcTemp := A[i+112] + lcTemp;
      lcTemp := A[i+113] - lcTemp;
      lcTemp := A[i+114] + lcTemp;

      lcTemp := A[i+115] - lcTemp;
      lcTemp := A[i+116] + lcTemp;
      lcTemp := A[i+117] - lcTemp;
      lcTemp := A[i+118] + lcTemp;
      lcTemp := A[i+119] - lcTemp;

      lcTemp := A[i+120] + lcTemp;
      lcTemp := A[i+121] - lcTemp;
      lcTemp := A[i+122] + lcTemp;
      lcTemp := A[i+123] - lcTemp;
      lcTemp := A[i+124] + lcTemp;

      lcTemp := A[i+125] - lcTemp;
      lcTemp := A[i+126] + lcTemp;
      lcTemp := A[i+127] - lcTemp;

      li64EndCycle := GetCycleCount;
      // calculate elapsed time:
      leElapsedTime :=
        ( ( li64EndCycle - li64StartCycle ) - SWOverhead ) / leClockspeed;
      leAccumulatedTime := leAccumulatedTime + leElapsedTime;
      // increment index:
      i := i + 128;
    end; // while...
    // see if this is the fastest iteration:
    if leMinTime > leAccumulatedTime then begin
      leMinTime := leAccumulatedTime;
    end; // if
  end; // for...

  // assign to dummy variable or optimizer will remove:
  FDummy := lcTemp;
  result := leMinTime;
end; // function TfrmMemLate.ReadTest

function TfrmCOSBIBandwidthBurn.WriteTest(var A : TBigArray;
                               var ai_RepeatCount : integer;
                               var ai_ArraySize   : integer
                               ): Extended;
var
  i                 : integer;
  liIterate        : integer;
  li64StartCycle   : Int64;
  li64EndCycle     : Int64;
  liRandomData     : cardinal;
  leElapsedTime     : extended;
  leAccumulatedTime : extended;
  leMinTime         : extended;
  leClockspeed      : extended;
begin
  // initialize variables
  leClockspeed := StopWatch.GetCPUClockspeed(FALSE);
  leElapsedTime := 0;
  leMinTime := 99999999999999;
  liRandomData   := Random(1000000);
  // we iterate over RepeatCount to find best time
  for liIterate := 1 to ai_RepeatCount do begin
    leAccumulatedTime := 0;
    // this is the critical loop where we populate the array:
    i := 1;
    while i <= ai_ArraySize do begin
      // start timer
      li64StartCycle := GetCycleCount;
      // write data to array:
      A[i] := liRandomData;
      A[i + 1] := liRandomData;
      A[i + 2] := liRandomData;
      A[i + 3] := liRandomData;
      A[i + 4] := liRandomData;

      A[i + 5] := liRandomData;
      A[i + 6] := liRandomData;
      A[i + 7] := liRandomData;
      A[i + 8] := liRandomData;
      A[i + 9] := liRandomData;

      A[i + 10] := liRandomData;
      A[i + 11] := liRandomData;
      A[i + 12] := liRandomData;
      A[i + 13] := liRandomData;
      A[i + 14] := liRandomData;

      A[i + 15] := liRandomData;
      A[i + 16] := liRandomData;
      A[i + 17] := liRandomData;
      A[i + 18] := liRandomData;
      A[i + 19] := liRandomData;

      A[i + 20] := liRandomData;
      A[i + 21] := liRandomData;
      A[i + 22] := liRandomData;
      A[i + 23] := liRandomData;
      A[i + 24] := liRandomData;

      A[i + 25] := liRandomData;
      A[i + 26] := liRandomData;
      A[i + 27] := liRandomData;
      A[i + 28] := liRandomData;
      A[i + 29] := liRandomData;

      A[i + 30] := liRandomData;
      A[i + 31] := liRandomData;
      A[i + 32] := liRandomData;
      A[i + 33] := liRandomData;
      A[i + 34] := liRandomData;

      A[i + 35] := liRandomData;
      A[i + 36] := liRandomData;
      A[i + 37] := liRandomData;
      A[i + 38] := liRandomData;
      A[i + 39] := liRandomData;

      A[i + 40] := liRandomData;
      A[i + 41] := liRandomData;
      A[i + 42] := liRandomData;
      A[i + 43] := liRandomData;
      A[i + 44] := liRandomData;

      A[i + 45] := liRandomData;
      A[i + 46] := liRandomData;
      A[i + 47] := liRandomData;
      A[i + 48] := liRandomData;
      A[i + 49] := liRandomData;

      A[i + 50] := liRandomData;
      A[i + 51] := liRandomData;
      A[i + 52] := liRandomData;
      A[i + 53] := liRandomData;
      A[i + 54] := liRandomData;
      A[i + 55] := liRandomData;
      A[i + 56] := liRandomData;
      A[i + 57] := liRandomData;
      A[i + 58] := liRandomData;
      A[i + 59] := liRandomData;

      A[i + 60] := liRandomData;
      A[i + 61] := liRandomData;
      A[i + 62] := liRandomData;
      A[i + 63] := liRandomData;
      A[i + 64] := liRandomData;

      A[i + 65] := liRandomData;
      A[i + 66] := liRandomData;
      A[i + 67] := liRandomData;
      A[i + 68] := liRandomData;
      A[i + 69] := liRandomData;

      A[i + 70] := liRandomData;
      A[i + 71] := liRandomData;
      A[i + 72] := liRandomData;
      A[i + 73] := liRandomData;
      A[i + 74] := liRandomData;

      A[i + 75] := liRandomData;
      A[i + 76] := liRandomData;
      A[i + 77] := liRandomData;
      A[i + 78] := liRandomData;
      A[i + 79] := liRandomData;

      A[i + 80] := liRandomData;
      A[i + 81] := liRandomData;
      A[i + 82] := liRandomData;
      A[i + 83] := liRandomData;
      A[i + 84] := liRandomData;

      A[i + 85] := liRandomData;
      A[i + 86] := liRandomData;
      A[i + 87] := liRandomData;
      A[i + 88] := liRandomData;
      A[i + 89] := liRandomData;

      A[i + 90] := liRandomData;
      A[i + 91] := liRandomData;
      A[i + 92] := liRandomData;
      A[i + 93] := liRandomData;
      A[i + 94] := liRandomData;

      A[i + 95] := liRandomData;
      A[i + 96] := liRandomData;
      A[i + 97] := liRandomData;
      A[i + 98] := liRandomData;
      A[i + 99] := liRandomData;

      A[i + 100] := liRandomData;
      A[i + 101] := liRandomData;
      A[i + 102] := liRandomData;
      A[i + 103] := liRandomData;
      A[i + 104] := liRandomData;

      A[i + 105] := liRandomData;
      A[i + 106] := liRandomData;
      A[i + 107] := liRandomData;
      A[i + 108] := liRandomData;
      A[i + 109] := liRandomData;

      A[i + 110] := liRandomData;
      A[i + 111] := liRandomData;
      A[i + 112] := liRandomData;
      A[i + 113] := liRandomData;
      A[i + 114] := liRandomData;

      A[i + 115] := liRandomData;
      A[i + 116] := liRandomData;
      A[i + 117] := liRandomData;
      A[i + 118] := liRandomData;
      A[i + 119] := liRandomData;

      A[i + 120] := liRandomData;
      A[i + 121] := liRandomData;
      A[i + 122] := liRandomData;
      A[i + 123] := liRandomData;
      A[i + 124] := liRandomData;

      A[i + 125] := liRandomData;
      A[i + 126] := liRandomData;
      A[i + 127] := liRandomData;

      li64EndCycle := GetCycleCount;
      // calculate elapsed time:
      leElapsedTime :=
        ( ( li64EndCycle - li64StartCycle ) - SWOverhead ) / leClockspeed;
      leAccumulatedTime := leAccumulatedTime + leElapsedTime;
      // increment index:
      i := i + 128;
    end; // while...
    // see if this is the fastest iteration:
    if leMinTime > leAccumulatedTime then begin
      leMinTime := leAccumulatedTime;
    end; // if
  end; // for...
  result := leMinTime;
end; // function TfrmMemLate.WriteTest

function TfrmCOSBIBandwidthBurn.ReadWriteMulTest(var A : TBigArray;
                                      var ai_RepeatCount : integer;
                                      var ai_ArraySize   : integer
                                      ): Extended;
var
  lcTemp           : cardinal;
  leElapsedTime   : extended;
  i, j              : integer;
  liIterate        : integer;
  li64StartCycle   : Int64;
  li64EndCycle     : Int64;
  liRandomData     : cardinal;
  leAccumulatedTime : extended;
  leMinTime         : extended;
  leClockspeed      : extended;
begin
  // initialize variables
  // initialize variables
  leClockspeed := StopWatch.GetCPUClockspeed(FALSE);
  leElapsedTime := 0;
  leMinTime := 99999999999999;
  lcTemp := 0;
  liRandomData   := Random(2);
  for liIterate := 1 to ai_RepeatCount do begin
    leAccumulatedTime := 0;
    // this is the critical loop where we populate the array:
    i := 1;
    while i <= ai_ArraySize do begin
      // start timer
      li64StartCycle := GetCycleCount;

      // copy data
      A[i] := liRandomData * liIterate;
      A[i + 1] := liRandomData * liIterate;
      A[i + 2] := liRandomData * liIterate;
      A[i + 3] := liRandomData * liIterate;
      A[i + 4] := liRandomData * liIterate;

      A[i + 5] := liRandomData * liIterate;
      A[i + 6] := liRandomData * liIterate;
      A[i + 7] := liRandomData * liIterate;
      A[i + 8] := liRandomData * liIterate;
      A[i + 9] := liRandomData * liIterate;

      A[i + 10] := liRandomData * liIterate;
      A[i + 11] := liRandomData * liIterate;
      A[i + 12] := liRandomData * liIterate;
      A[i + 13] := liRandomData * liIterate;
      A[i + 14] := liRandomData * liIterate;

      A[i + 15] := liRandomData * liIterate;
      A[i + 16] := liRandomData * liIterate;
      A[i + 17] := liRandomData * liIterate;
      A[i + 18] := liRandomData * liIterate;
      A[i + 19] := liRandomData * liIterate;

      A[i + 20] := liRandomData * liIterate;
      A[i + 21] := liRandomData * liIterate;
      A[i + 22] := liRandomData * liIterate;
      A[i + 23] := liRandomData * liIterate;
      A[i + 24] := liRandomData * liIterate;

      A[i + 25] := liRandomData * liIterate;
      A[i + 26] := liRandomData * liIterate;
      A[i + 27] := liRandomData * liIterate;
      A[i + 28] := liRandomData * liIterate;
      A[i + 29] := liRandomData * liIterate;

      A[i + 30] := liRandomData * liIterate;
      A[i + 31] := liRandomData * liIterate;
      A[i + 32] := liRandomData * liIterate;
      A[i + 33] := liRandomData * liIterate;
      A[i + 34] := liRandomData * liIterate;

      A[i + 35] := liRandomData * liIterate;
      A[i + 36] := liRandomData * liIterate;
      A[i + 37] := liRandomData * liIterate;
      A[i + 38] := liRandomData * liIterate;
      A[i + 39] := liRandomData * liIterate;

      A[i + 40] := liRandomData * liIterate;
      A[i + 41] := liRandomData * liIterate;
      A[i + 42] := liRandomData * liIterate;
      A[i + 43] := liRandomData * liIterate;
      A[i + 44] := liRandomData * liIterate;

      A[i + 45] := liRandomData * liIterate;
      A[i + 46] := liRandomData * liIterate;
      A[i + 47] := liRandomData * liIterate;
      A[i + 48] := liRandomData * liIterate;
      A[i + 49] := liRandomData * liIterate;

      A[i + 50] := liRandomData * liIterate;
      A[i + 51] := liRandomData * liIterate;
      A[i + 52] := liRandomData * liIterate;
      A[i + 53] := liRandomData * liIterate;
      A[i + 54] := liRandomData * liIterate;

      A[i + 55] := liRandomData * liIterate;
      A[i + 56] := liRandomData * liIterate;
      A[i + 57] := liRandomData * liIterate;
      A[i + 58] := liRandomData * liIterate;
      A[i + 59] := liRandomData * liIterate;

      A[i + 60] := liRandomData * liIterate;
      A[i + 61] := liRandomData * liIterate;
      A[i + 62] := liRandomData * liIterate;
      A[i + 63] := liRandomData * liIterate;
      A[i + 64] := liRandomData * liIterate;

      A[i + 65] := liRandomData * liIterate;
      A[i + 66] := liRandomData * liIterate;
      A[i + 67] := liRandomData * liIterate;
      A[i + 68] := liRandomData * liIterate;
      A[i + 69] := liRandomData * liIterate;

      A[i + 70] := liRandomData * liIterate;
      A[i + 71] := liRandomData * liIterate;
      A[i + 72] := liRandomData * liIterate;
      A[i + 73] := liRandomData * liIterate;
      A[i + 74] := liRandomData * liIterate;

      A[i + 75] := liRandomData * liIterate;
      A[i + 76] := liRandomData * liIterate;
      A[i + 77] := liRandomData * liIterate;
      A[i + 78] := liRandomData * liIterate;
      A[i + 79] := liRandomData * liIterate;

      A[i + 80] := liRandomData * liIterate;
      A[i + 81] := liRandomData * liIterate;
      A[i + 82] := liRandomData * liIterate;
      A[i + 83] := liRandomData * liIterate;
      A[i + 84] := liRandomData * liIterate;

      A[i + 85] := liRandomData * liIterate;
      A[i + 86] := liRandomData * liIterate;
      A[i + 87] := liRandomData * liIterate;
      A[i + 88] := liRandomData * liIterate;
      A[i + 89] := liRandomData * liIterate;

      A[i + 90] := liRandomData * liIterate;
      A[i + 91] := liRandomData * liIterate;
      A[i + 92] := liRandomData * liIterate;
      A[i + 93] := liRandomData * liIterate;
      A[i + 94] := liRandomData * liIterate;

      A[i + 95] := liRandomData * liIterate;
      A[i + 96] := liRandomData * liIterate;
      A[i + 97] := liRandomData * liIterate;
      A[i + 98] := liRandomData * liIterate;
      A[i + 99] := liRandomData * liIterate;

      A[i + 100] := liRandomData * liIterate;
      A[i + 101] := liRandomData * liIterate;
      A[i + 102] := liRandomData * liIterate;
      A[i + 103] := liRandomData * liIterate;
      A[i + 104] := liRandomData * liIterate;

      A[i + 105] := liRandomData * liIterate;
      A[i + 106] := liRandomData * liIterate;
      A[i + 107] := liRandomData * liIterate;
      A[i + 108] := liRandomData * liIterate;
      A[i + 109] := liRandomData * liIterate;

      A[i + 110] := liRandomData * liIterate;
      A[i + 111] := liRandomData * liIterate;
      A[i + 112] := liRandomData * liIterate;
      A[i + 113] := liRandomData * liIterate;
      A[i + 114] := liRandomData * liIterate;

      A[i + 115] := liRandomData * liIterate;
      A[i + 116] := liRandomData * liIterate;
      A[i + 117] := liRandomData * liIterate;
      A[i + 118] := liRandomData * liIterate;
      A[i + 119] := liRandomData * liIterate;

      A[i + 120] := liRandomData * liIterate;
      A[i + 121] := liRandomData * liIterate;
      A[i + 122] := liRandomData * liIterate;
      A[i + 123] := liRandomData * liIterate;
      A[i + 124] := liRandomData * liIterate;

      A[i + 125] := liRandomData * liIterate;
      A[i + 126] := liRandomData * liIterate;
      A[i + 127] := liRandomData * liIterate;

      li64EndCycle := GetCycleCount;
      // calculate elapsed time:
      leElapsedTime :=
        ( ( li64EndCycle - li64StartCycle ) - SWOverhead ) / leClockspeed;
      leAccumulatedTime := leAccumulatedTime + leElapsedTime;
      // increment index:
      i := i + 128;
    end; // while...

    // now read back the data:
    lcTemp := 2;
    j := 1;
    while j <= ai_ArraySize do begin
      // start timer
      li64StartCycle := GetCycleCount;

      lcTemp := A[j] * lcTemp;
      lcTemp := A[j+1] * lcTemp;
      lcTemp := A[j+2] * lcTemp;
      lcTemp := A[j+3] * lcTemp;
      lcTemp := A[j+4] * lcTemp;

      lcTemp := A[j+5] * lcTemp;
      lcTemp := A[j+6] * lcTemp;
      lcTemp := A[j+7] * lcTemp;
      lcTemp := A[j+8] * lcTemp;
      lcTemp := A[j+9] * lcTemp;

      lcTemp := A[j+10] * lcTemp;
      lcTemp := A[j+11] * lcTemp;
      lcTemp := A[j+12] * lcTemp;
      lcTemp := A[j+13] * lcTemp;
      lcTemp := A[j+14] * lcTemp;

      lcTemp := A[j+15] * lcTemp;
      lcTemp := A[j+16] * lcTemp;
      lcTemp := A[j+17] * lcTemp;
      lcTemp := A[j+18] * lcTemp;
      lcTemp := A[j+19] * lcTemp;

      lcTemp := A[j+20] * lcTemp;
      lcTemp := A[j+21] * lcTemp;
      lcTemp := A[j+22] * lcTemp;
      lcTemp := A[j+23] * lcTemp;
      lcTemp := A[j+24] * lcTemp;

      lcTemp := A[j+25] * lcTemp;
      lcTemp := A[j+26] * lcTemp;
      lcTemp := A[j+27] * lcTemp;
      lcTemp := A[j+28] * lcTemp;
      lcTemp := A[j+29] * lcTemp;

      lcTemp := A[j+30] * lcTemp;
      lcTemp := A[j+31] * lcTemp;
      lcTemp := A[j+32] * lcTemp;
      lcTemp := A[j+33] * lcTemp;
      lcTemp := A[j+34] * lcTemp;

      lcTemp := A[j+35] * lcTemp;
      lcTemp := A[j+36] * lcTemp;
      lcTemp := A[j+37] * lcTemp;
      lcTemp := A[j+38] * lcTemp;
      lcTemp := A[j+39] * lcTemp;

      lcTemp := A[j+40] * lcTemp;
      lcTemp := A[j+41] * lcTemp;
      lcTemp := A[j+42] * lcTemp;
      lcTemp := A[j+43] * lcTemp;
      lcTemp := A[j+44] * lcTemp;

      lcTemp := A[j+45] * lcTemp;
      lcTemp := A[j+46] * lcTemp;
      lcTemp := A[j+47] * lcTemp;
      lcTemp := A[j+48] * lcTemp;
      lcTemp := A[j+49] * lcTemp;

      lcTemp := A[j+50] * lcTemp;
      lcTemp := A[j+51] * lcTemp;
      lcTemp := A[j+52] * lcTemp;
      lcTemp := A[j+53] * lcTemp;
      lcTemp := A[j+54] * lcTemp;

      lcTemp := A[j+55] * lcTemp;
      lcTemp := A[j+56] * lcTemp;
      lcTemp := A[j+57] * lcTemp;
      lcTemp := A[j+58] * lcTemp;
      lcTemp := A[j+59] * lcTemp;

      lcTemp := A[j+60] * lcTemp;
      lcTemp := A[j+61] * lcTemp;
      lcTemp := A[j+62] * lcTemp;
      lcTemp := A[j+63] * lcTemp;
      lcTemp := A[j+64] * lcTemp;

      lcTemp := A[j+65] * lcTemp;
      lcTemp := A[j+66] * lcTemp;
      lcTemp := A[j+67] * lcTemp;
      lcTemp := A[j+68] * lcTemp;
      lcTemp := A[j+69] * lcTemp;

      lcTemp := A[j+70] * lcTemp;
      lcTemp := A[j+71] * lcTemp;
      lcTemp := A[j+72] * lcTemp;
      lcTemp := A[j+73] * lcTemp;
      lcTemp := A[j+74] * lcTemp;

      lcTemp := A[j+75] * lcTemp;
      lcTemp := A[j+76] * lcTemp;
      lcTemp := A[j+77] * lcTemp;
      lcTemp := A[j+78] * lcTemp;
      lcTemp := A[j+79] * lcTemp;

      lcTemp := A[j+80] * lcTemp;
      lcTemp := A[j+81] * lcTemp;
      lcTemp := A[j+82] * lcTemp;
      lcTemp := A[j+83] * lcTemp;
      lcTemp := A[j+84] * lcTemp;

      lcTemp := A[j+85] * lcTemp;
      lcTemp := A[j+86] * lcTemp;
      lcTemp := A[j+87] * lcTemp;
      lcTemp := A[j+88] * lcTemp;
      lcTemp := A[j+89] * lcTemp;

      lcTemp := A[j+90] * lcTemp;
      lcTemp := A[j+91] * lcTemp;
      lcTemp := A[j+92] * lcTemp;
      lcTemp := A[j+93] * lcTemp;
      lcTemp := A[j+94] * lcTemp;

      lcTemp := A[j+95] * lcTemp;
      lcTemp := A[j+96] * lcTemp;
      lcTemp := A[j+97] * lcTemp;
      lcTemp := A[j+98] * lcTemp;
      lcTemp := A[j+99] * lcTemp;

      lcTemp := A[j+100] * lcTemp;
      lcTemp := A[j+101] * lcTemp;
      lcTemp := A[j+102] * lcTemp;
      lcTemp := A[j+103] * lcTemp;
      lcTemp := A[j+104] * lcTemp;

      lcTemp := A[j+105] * lcTemp;
      lcTemp := A[j+106] * lcTemp;
      lcTemp := A[j+107] * lcTemp;
      lcTemp := A[j+108] * lcTemp;
      lcTemp := A[j+109] * lcTemp;

      lcTemp := A[j+110] * lcTemp;
      lcTemp := A[j+111] * lcTemp;
      lcTemp := A[j+112] * lcTemp;
      lcTemp := A[j+113] * lcTemp;
      lcTemp := A[j+114] * lcTemp;

      lcTemp := A[j+115] * lcTemp;
      lcTemp := A[j+116] * lcTemp;
      lcTemp := A[j+117] * lcTemp;
      lcTemp := A[j+118] * lcTemp;
      lcTemp := A[j+119] * lcTemp;

      lcTemp := A[j+120] * lcTemp;
      lcTemp := A[j+121] * lcTemp;
      lcTemp := A[j+122] * lcTemp;
      lcTemp := A[j+123] * lcTemp;
      lcTemp := A[j+124] * lcTemp;

      lcTemp := A[j+125] * lcTemp;
      lcTemp := A[j+126] * lcTemp;
      lcTemp := A[j+127] * lcTemp;

      li64EndCycle := GetCycleCount;
      // calculate elapsed time:
      leElapsedTime :=
        ( ( li64EndCycle - li64StartCycle ) - SWOverhead ) / leClockspeed;
      leAccumulatedTime := leAccumulatedTime + leElapsedTime;
      // increment read index:
      j := j + 128;
    end; // while...
    if leMinTime > leAccumulatedTime then begin
      leMinTime := leAccumulatedTime;
    end; // if
  end; // for...
  // assign to dummy variable or optimizer will remove code:
  FDummy := lcTemp;
  result := leMinTime;
end; // function ReadWriteTest

procedure TfrmCOSBIBandwidthBurn.cboAssignChange(Sender: TObject);
begin

  if cboAssign.Text = 'Simple' then
    AssignmentType := assSimple
  else if cboAssign.Text = 'Read' then
    AssignmentType := assRead
  else if cboAssign.Text = 'Write' then
    AssignmentType := assWrite
  else if cboAssign.Text = 'Toggle' then
    AssignmentType := assToggle
  else if cboAssign.Text = '5xToggle' then
    AssignmentType := assToggle5x
  else if cboAssign.Text = 'Multiply' then
    AssignmentType := assMult
  else if cboAssign.Text = 'Block Prefetch32' then
    AssignmentType := assBlockPrefetch32
  else if cboAssign.Text = 'Block Prefetch64' then
    AssignmentType := assBlockPrefetch64
  else if cboAssign.Text = 'BP32 Write' then
    AssignmentType := assBP32Write
  else if cboAssign.Text = ASSIGN_VERIFY_SIMPLE then
    AssignmentType := assVerifySimple
  else if cboAssign.Text = 'oo Simple' then
    AssignmentType := assOOSimple
  else if cboAssign.Text = 'oo Read' then
    AssignmentType := assOORead
  else if cboAssign.Text = 'oo Write' then
    AssignmentType := assOOWrite
  else if cboAssign.Text = 'oo Block Prefetch32' then
    AssignmentType := assOOBP32
  else if cboAssign.Text = 'oo Block Prefetch64' then
    AssignmentType := assOOBP64
  else if cboAssign.Text = 'oo Block Prefetch128' then
    AssignmentType := assOOBP128
  else if cboAssign.Text = 'oo BP32 Write' then
    AssignmentType := assBP32Write
  else if cboAssign.Text = 'oo Tight Loop Simple' then
    AssignmentType := assOOSimpleTightLoop
  else if cboAssign.Text = 'Skip 1 64-B CacheLine' then
    AssignmentType := assSkip1_64B_CacheLine
  else Exception.Create('Unknown assignment type');

end;

procedure TfrmCOSBIBandwidthBurn.GraphMemoryBandwidth;
// We want to be able to read and write to an array section varying the size
// we are working with starting at 1kB (256 integer elements) and increasing
// by 1kB steps until we reach 500 kB.  At each step we are going to push through
// around 100 MB.  We will determine the number of iterations by "div" our
// section size with 100MB (we should allow this number to be user defined).
// We need to report the report bandwidth rate at each step.
var
   l_A              : TBigArray;  // this is our target array
   li_StepSize      : integer;    // step in # elements to move to next array size
   le_Time          : Extended;   // accumulated total time
   le_Bandwidth     : Extended;   // bandwidth for each step
   li_array_size    : integer;    // size of array to stress
   ls_MemWinSize    : string;     // Memory window size x-axis label
   li_ResultIndex   : integer;
   l_Results        : Array[1..4800] of double; //Array[1..1200] of double;
   le_LoopTime      : Extended;
   le_TestTime      : Extended;
   lb_Toggle        : Boolean;
   li_ToggleCount   : Integer;
   li_ToggleFactor  : Integer;
   lAssignType      : TAssignType;
   ld_MaxBandwidth  : double;
   li_ProcessMsgs   : integer;
   ending_size      : integer;

const
  BEGINNING_SIZE = 256;     // 1 kB

  procedure InitializeTest;
  begin
    // update status bar:
    Status := 'Status: Testing...';
    ending_size := (fMaxDatasetSize div 4) * ONE_KB;
    // initialize variables:
    le_Time := 0;
    ld_MaxBandwidth := 0;
    RandomizeBigArray(l_A);
    li_ProcessMsgs := 0;
    li_ToggleCount := 0;
    ResetGraph;
    chartBandwidth.BottomAxis.Maximum := fMaxDatasetSize / fStepSize;
    chartBandwidth.BottomAxis.Minimum := 0;
    chartBandwidth.UndoZoom;
    case FAssignType of
      assToggle: begin
        lAssignType := assSimple;
        lb_Toggle := TRUE;
        li_ToggleFactor := 1;
      end;
      assToggle5x: begin
        lAssignType := assSimple;
        lb_Toggle := TRUE;
        li_ToggleFactor := 5;
      end;
    else
      lAssignType := FAssignType;
      lb_Toggle := FALSE;
      li_ToggleFactor := 0;
    end;
    li_StepSize := fStepSize * ONE_KB div 4;
    // calculate the maximum value for the progress
    GaugeProgress.MaxValue := ending_size div li_StepSize;
    GaugeProgress.Progress := 0;
    Application.ProcessMessages;
  end; // procedure InitializeTest;

  procedure PerformBandwidthTest;
  begin
    case lAssignType of
      assMult: begin
        le_LoopTime := ReadWriteMulTest(l_A, fInterationsPerStep, li_array_size);
      end;
      assSimple: begin
        le_LoopTime := ReadWriteTest(l_A, fInterationsPerStep, li_array_size);
      end;
      assRead: begin
        le_LoopTime := ReadTest(l_A, fInterationsPerStep, li_array_size);
      end;
      assWrite: begin
        le_LoopTime := WriteTest(l_A, fInterationsPerStep, li_array_size);
      end;
      assBlockPrefetch32: begin
        le_LoopTime := BlockPrefetch32(l_A, fInterationsPerStep, li_array_size);
      end;
      assBlockPrefetch64: begin
        le_LoopTime := BlockPrefetch64(l_A, fInterationsPerStep, li_array_size);
//        le_TestTime := BlockPrefetch64(l_A, fInterationsPerStep, li_array_size);
//        le_LoopTime := le_LoopTime + le_TestTime;
      end;
      assBP32Write: begin
        le_LoopTime := BP32Write(l_A, fInterationsPerStep, li_array_size);
      end;
      assVerifySimple: begin
        le_TestTime := ReadWriteVerifyTest(l_A, fInterationsPerStep, li_array_size);
        le_LoopTime := le_LoopTime + le_TestTime;
      end;
      assOOSimple: begin
        le_LoopTime := fBandwidthBurn.ReadWriteTest(fInterationsPerStep, li_array_size, l_A);
      end;
      assOORead: begin
        le_LoopTime := fBandwidthBurn.ReadTestIterate(fInterationsPerStep, li_array_size, l_A);
      end;
      assOOWrite: begin
        le_LoopTime := fBandwidthBurn.WriteTestIterate(fInterationsPerStep, li_array_size, 0, TRUE, l_A);
      end;
      assOOBP32: begin
        le_LoopTime := fBandwidthBurn.ReadWriteTestBP32(fInterationsPerStep, li_array_size, l_A);
      end;
      assOOBP64: begin
        le_LoopTime := fBandwidthBurn.ReadWriteTestBP64(fInterationsPerStep, li_array_size, l_A);
      end;
      assOOBP128: begin
        le_LoopTime := fBandwidthBurn.ReadWriteTestBP128(fInterationsPerStep, li_array_size, l_A);
      end;
      assOOBP32Write: begin
        le_LoopTime := fBandwidthBurn.WriteTestIterateBP32(fInterationsPerStep, li_array_size, 0, TRUE, l_A);
      end;
      assOOSimpleTightLoop: begin
        le_LoopTime := fBandwidthBurn.SimpleTightLoop(fInterationsPerStep, li_array_size, l_A);
      end;
      assSkip1_64B_CacheLine: begin
        le_LoopTime := Skip1Line(l_A, fInterationsPerStep, li_array_size);
      end;
    else
      exception.Create('Unhandled assignment type.');
    end; // case
  end; // procedure PerformBandwidthTest;

  procedure CalculateBandwidth;
  begin
    // calculate bandwidth:
    if lAssignType in
      [
        assToggle, assToggle5x,
        assBlockPrefetch128, assVerifySimple
      ] then begin
      // we do a read + write, so double the memory tested:
      le_Bandwidth := ( 2 * li_array_size * 4 ) * fInterationsPerStep
        / (le_LoopTime * 1000000); // MB/s
    end else if  lAssignType in
      [
        assSimple, assBlockPrefetch64, assMult, assBlockPrefetch32,
        assOOSimpleTightLoop, assOOSimple, assOOBP32, assOOBP64, assOOBP128
      ] then begin
      le_Bandwidth := (  2 * li_array_size * 4 ) / (le_LoopTime * 1000000); // MB/s
    end else begin
      le_Bandwidth := ( li_array_size * 4 ) / (le_LoopTime * 1000000); // MB/s
    end;
  end; // procedure CalculateBandwidth;

  procedure UpdateChartData;
  begin
    // format current dataset size as a string to be added to chart series:
    ls_MemWinSize := '  ' + FloatToStrF( (li_array_size * 4) / 1024, ffNumber, 5, 0) + '  ';
    // update graph if necessary
    if fGraphRealtime then begin
      GaugeProgress.Progress := GaugeProgress.Progress + 1;
//      if fSeries.Count = 0 then begin
//        fSeries.Add( 0, ' 0 ', clYellow );
//      end;
      fSeries.Add(le_Bandwidth, ls_MemWinSize, clYellow);
      if ld_MaxBandwidth < fSeries.MaxYValue then begin
        ld_MaxBandwidth := fSeries.MaxYValue;
        chartBandwidth.LeftAxis.Maximum := GRAPH_PAD_MAX * ld_MaxBandwidth;
      end; // if ld_MaxBandwidth
      fPanelMinMax.Text := 'Max = ' +
            FloatToStrF(fSeries.MaxYValue, ffNumber, 10, 1) +
            ', Min = ' +
            FloatToStrF(fSeries.MinYValue, ffNumber, 10, 1);
      inc( li_ProcessMsgs );
      if (li_ProcessMsgs mod 10) = 0 then Application.ProcessMessages;
    end else begin
      // if we are not doing real-time graphing then just add the data point
      // to the results array:
      l_Results[ li_ResultIndex ] := le_Bandwidth;
    end; // if...else
  end; // procedure UpdateChartData;

  procedure HandleToggle;
  begin
    // Handle toggling
    if lb_Toggle then begin
      if ( li_ToggleCount mod li_ToggleFactor ) = 0 then begin
        if lAssignType = assSimple then begin
          lAssignType := assMult;
        end else begin
          lAssignType := assSimple;
        end;
      end;
      li_ToggleCount := li_ToggleCount + 1;
    end; // if lb_Toggle
  end; // procedure HandleToggle;

  procedure FinalizeTest;
  begin
    // we are finished with the test, so update progress bar to max value:
    GaugeProgress.Progress := GaugeProgress.MaxValue;
    // output total test time:
    fPanelCopyTime.Text := 'Total time ' + FloatToStrF((le_Time), ffNumber, 6, 6) + ' s';
    // update status:
    Status := 'Status: Ready.';
    // if we did not have real-time graphing then we need to add the series data
    // now:
    if not fGraphRealtime then begin
      li_array_size := BEGINNING_SIZE;
      li_ResultIndex := 1;
      while li_array_size <= ENDING_SIZE do begin
        // format current dataset size as a string to be added to chart series:
        ls_MemWinSize := '  ' + FloatToStrF( (li_array_size * 4) / 1024, ffNumber, 5, 0) + '  ';
        le_Bandwidth := l_Results[ li_ResultIndex ];
//        fSeries.AddXY(li_ResultIndex * ONE_KB * fStepSize, l_Results[ li_ResultIndex ],
//          '  ' + IntToStr( li_array_size * 4  Div 1024 ) + '  ', clRed);
        fSeries.Add(le_Bandwidth, ls_MemWinSize, clRed);
        li_array_size := li_array_size + li_StepSize;
        li_ResultIndex := li_ResultIndex + 1;
      end;
      chartBandwidth.LeftAxis.Maximum := GRAPH_PAD_MAX * fSeries.MaxYValue;
      fPanelMinMax.Text := 'Max = ' +
        FloatToStrF(fSeries.MaxYValue, ffNumber, 10, 1)
        + ', Min = ' +
        FloatToStrF(fSeries.MinYValue, ffNumber, 10, 1);
    end;
    EnableButtons;
  end; // procedure FinalizeTest;

begin
  InitializeTest;
  // we need to loop through our array while increasing dataset sizes:
  li_array_size := BEGINNING_SIZE;
  li_ResultIndex := 1;
  if cboxSpinUp.Checked then Fibonacci( 36 );
  while (li_array_size <= ENDING_SIZE) and not fBreak do begin
    //StopWatch.StartTimer;
    le_LoopTime := 0;
    PerformBandwidthTest;
    le_Time := le_Time + le_LoopTime;
    CalculateBandwidth;
    UpdateChartData;
    // increment loop variables for next pass:
    li_array_size := li_array_size + li_StepSize;
    li_ResultIndex := li_ResultIndex + 1;
    HandleToggle;
  end; // while
  FinalizeTest;
end; // TfrmMemLate.GraphMemoryBandwidth

procedure TfrmCOSBIBandwidthBurn.ResetGraph;
begin
  fSeries.Clear;
  with chartBandwidth do begin
    LeftAxis.Automatic := FALSE;
    LeftAxis.Minimum := 0;
    LeftAxis.Maximum := 1;
    BottomAxis.Automatic := FALSE;     // changed
  end; // with
end; // procedure TfrmMemLate.ResetGraph;

procedure TfrmCOSBIBandwidthBurn.DisableButtons;
begin
  btnBandwidth.Enabled  := FALSE;
  btnScale.Enabled      := FALSE;
  btnSave.Enabled       := FALSE;
  btnBrowseSaveData.Enabled := FALSE;
  btnBrowseSaveGraph.Enabled := FALSE;
  btnSaveGraph.Enabled := FALSE;
  btnChangeBackground.Enabled := FALSE;
end; // procedure TfrmMemLate.DisableButtons

procedure TfrmCOSBIBandwidthBurn.EnableButtons;
begin
  btnBandwidth.Enabled := TRUE;
  btnScale.Enabled := TRUE;
  btnSave.Enabled := TRUE;
  btnBrowseSaveData.Enabled := TRUE;
  btnBrowseSaveGraph.Enabled := TRUE;
  btnSaveGraph.Enabled := TRUE;
  btnChangeBackground.Enabled := TRUE;
end; // procedure TfrmMemLate.EnableButtons


procedure TfrmCOSBIBandwidthBurn.cboIterationsChange(Sender: TObject);
begin
  try
    fInterationsPerStep := StrToInt( cboIterations.Text );
  except
    cboIterations.Text := '1';
  end; //try
end;

procedure TfrmCOSBIBandwidthBurn.cboStepSizeChange(Sender: TObject);
begin
  fStepSize := StrToInt( cboStepSize.Text );
  if ( fMaxDatasetSize > MAX_DATASET_THRESHOLD ) and ( fStepSize < 5 ) then begin
    ShowMessage( 'Datasets over 3MB have to have step sizes >= 10' );
    fStepSize := 10;
    cboStepSize.Text := '10';
  end; // if
end;

procedure TfrmCOSBIBandwidthBurn.cboxGraphRealTimeClick(Sender: TObject);
begin
  GraphRealtime := cboxGraphRealTime.Checked;
end;

function TfrmCOSBIBandwidthBurn.BlockPrefetch32(var A : TBigArray;
                                   var ai_RepeatCount : integer;
                                   var ai_ArraySize   : integer
                                   ): Extended;
var
  li64StartCycle   : Int64;
  li64EndCycle     : Int64;
  liRandomData     : integer;
  lcTemp           : cardinal;
  i, j             : integer;
  liIterate        : integer;
  leElapsedTime    : extended;
  leAccumulatedTime : extended;
  leMinTime         : extended;
  leClockspeed      : extended;
begin
  // initialize variables
  leClockspeed := StopWatch.GetCPUClockspeed(FALSE);
  leElapsedTime := 0;
  leMinTime := 99999999999999;
  lcTemp := 0;
  liRandomData   := Random(1000);
  for liIterate := 1 to ai_RepeatCount do begin
    leAccumulatedTime := 0;
    // this is the critical loop where we populate the array:
    i := 1;
    while i <= ai_ArraySize do begin
      // start timer
      li64StartCycle := GetCycleCount;
      // block prefetch 16 32-byte lines:
      A[i] := liRandomData;
      A[i + 8] := liRandomData;
      A[i + 16] := liRandomData;
      A[i + 24] := liRandomData;
      A[i + 32] := liRandomData;
      A[i + 40] := liRandomData;
      A[i + 48] := liRandomData;
      A[i + 56] := liRandomData;
      A[i + 64] := liRandomData;
      A[i + 72] := liRandomData;
      A[i + 80] := liRandomData;
      A[i + 88] := liRandomData;
      A[i + 96] := liRandomData;
      A[i + 104] := liRandomData;
      A[i + 112] := liRandomData;
      A[i + 120] := liRandomData;

      // now initialize array:
//      A[i] := liRandomData;
      A[i + 1] := liRandomData;
      A[i + 2] := liRandomData;
      A[i + 3] := liRandomData;
      A[i + 4] := liRandomData;

      A[i + 5] := liRandomData;
      A[i + 6] := liRandomData;
      A[i + 7] := liRandomData;
//      A[i + 8] := liRandomData;
      A[i + 9] := liRandomData;

      A[i + 10] := liRandomData;
      A[i + 11] := liRandomData;
      A[i + 12] := liRandomData;
      A[i + 13] := liRandomData;
      A[i + 14] := liRandomData;

      A[i + 15] := liRandomData;
//      A[i + 16] := liRandomData;
      A[i + 17] := liRandomData;
      A[i + 18] := liRandomData;
      A[i + 19] := liRandomData;

      A[i + 20] := liRandomData;
      A[i + 21] := liRandomData;
      A[i + 22] := liRandomData;
      A[i + 23] := liRandomData;
//      A[i + 24] := liRandomData;

      A[i + 25] := liRandomData;
      A[i + 26] := liRandomData;
      A[i + 27] := liRandomData;
      A[i + 28] := liRandomData;
      A[i + 29] := liRandomData;

      A[i + 30] := liRandomData;
      A[i + 31] := liRandomData;
//      A[i + 32] := liRandomData;
      A[i + 33] := liRandomData;
      A[i + 34] := liRandomData;

      A[i + 35] := liRandomData;
      A[i + 36] := liRandomData;
      A[i + 37] := liRandomData;
      A[i + 38] := liRandomData;
      A[i + 39] := liRandomData;

//      A[i + 40] := liRandomData;
      A[i + 41] := liRandomData;
      A[i + 42] := liRandomData;
      A[i + 43] := liRandomData;
      A[i + 44] := liRandomData;

      A[i + 45] := liRandomData;
      A[i + 46] := liRandomData;
      A[i + 47] := liRandomData;
//      A[i + 48] := liRandomData;
      A[i + 49] := liRandomData;

      A[i + 50] := liRandomData;
      A[i + 51] := liRandomData;
      A[i + 52] := liRandomData;
      A[i + 53] := liRandomData;
      A[i + 54] := liRandomData;

      A[i + 55] := liRandomData;
//      A[i + 56] := liRandomData;
      A[i + 57] := liRandomData;
      A[i + 58] := liRandomData;
      A[i + 59] := liRandomData;

      A[i + 60] := liRandomData;
      A[i + 61] := liRandomData;
      A[i + 62] := liRandomData;
      A[i + 63] := liRandomData;
//      A[i + 64] := liRandomData;

      A[i + 65] := liRandomData;
      A[i + 66] := liRandomData;
      A[i + 67] := liRandomData;
      A[i + 68] := liRandomData;
      A[i + 69] := liRandomData;

      A[i + 70] := liRandomData;
      A[i + 71] := liRandomData;
//      A[i + 72] := liRandomData;
      A[i + 73] := liRandomData;
      A[i + 74] := liRandomData;

      A[i + 75] := liRandomData;
      A[i + 76] := liRandomData;
      A[i + 77] := liRandomData;
      A[i + 78] := liRandomData;
      A[i + 79] := liRandomData;

//      A[i + 80] := liRandomData;
      A[i + 81] := liRandomData;
      A[i + 82] := liRandomData;
      A[i + 83] := liRandomData;
      A[i + 84] := liRandomData;

      A[i + 85] := liRandomData;
      A[i + 86] := liRandomData;
      A[i + 87] := liRandomData;
//      A[i + 88] := liRandomData;
      A[i + 89] := liRandomData;

      A[i + 90] := liRandomData;
      A[i + 91] := liRandomData;
      A[i + 92] := liRandomData;
      A[i + 93] := liRandomData;
      A[i + 94] := liRandomData;

      A[i + 95] := liRandomData;
//      A[i + 96] := liRandomData;
      A[i + 97] := liRandomData;
      A[i + 98] := liRandomData;
      A[i + 99] := liRandomData;

      A[i + 100] := liRandomData;
      A[i + 101] := liRandomData;
      A[i + 102] := liRandomData;
      A[i + 103] := liRandomData;
//      A[i + 104] := liRandomData;

      A[i + 105] := liRandomData;
      A[i + 106] := liRandomData;
      A[i + 107] := liRandomData;
      A[i + 108] := liRandomData;
      A[i + 109] := liRandomData;

      A[i + 110] := liRandomData;
      A[i + 111] := liRandomData;
//      A[i + 112] := liRandomData;
      A[i + 113] := liRandomData;
      A[i + 114] := liRandomData;

      A[i + 115] := liRandomData;
      A[i + 116] := liRandomData;
      A[i + 117] := liRandomData;
      A[i + 118] := liRandomData;
      A[i + 119] := liRandomData;

//      A[i + 120] := liRandomData;
      A[i + 121] := liRandomData;
      A[i + 122] := liRandomData;
      A[i + 123] := liRandomData;
      A[i + 124] := liRandomData;

      A[i + 125] := liRandomData;
      A[i + 126] := liRandomData;
      A[i + 127] := liRandomData;

      li64EndCycle := GetCycleCount;
      // calculate elapsed time:
      leElapsedTime :=
        ( ( li64EndCycle - li64StartCycle ) - SWOverhead ) / leClockspeed;
      leAccumulatedTime := leAccumulatedTime + leElapsedTime;
      // increment index:
      i := i + 128;
    end; // while...

    // now we need to read the data.
    lcTemp := 0;
    j := 1;
    while j <= ai_ArraySize do begin

      // start timer
      li64StartCycle := GetCycleCount;

      // copy data: we must accumulate or optimizer will remove
      // block prefetch 16 32-byte lines:
      lcTemp := A[j] + lcTemp;
      lcTemp := A[j + 8] + lcTemp;
      lcTemp := A[j + 16] + lcTemp;
      lcTemp := A[j + 24] + lcTemp;
      lcTemp := A[j + 32] + lcTemp;
      lcTemp := A[j + 40] + lcTemp;
      lcTemp := A[j + 48] + lcTemp;
      lcTemp := A[j + 56] + lcTemp;
      lcTemp := A[j + 64] + lcTemp;
      lcTemp := A[j + 72] + lcTemp;
      lcTemp := A[j + 80] + lcTemp;
      lcTemp := A[j + 88] + lcTemp;
      lcTemp := A[j + 96] + lcTemp;
      lcTemp := A[j + 104] + lcTemp;
      lcTemp := A[j + 112] + lcTemp;
      lcTemp := A[j + 120] + lcTemp;

      // now accumulate through array data :
//      lcTemp := A[j] + lcTemp;
      lcTemp := A[j+1] + lcTemp;
      lcTemp := A[j+2] + lcTemp;
      lcTemp := A[j+3] + lcTemp;
      lcTemp := A[j+4] + lcTemp;

      lcTemp := A[j+5] + lcTemp;
      lcTemp := A[j+6] + lcTemp;
      lcTemp := A[j+7] + lcTemp;
//      lcTemp := A[j+8] + lcTemp;
      lcTemp := A[j+9] + lcTemp;

      lcTemp := A[j+10] + lcTemp;
      lcTemp := A[j+11] + lcTemp;
      lcTemp := A[j+12] + lcTemp;
      lcTemp := A[j+13] + lcTemp;
      lcTemp := A[j+14] + lcTemp;

      lcTemp := A[j+15] + lcTemp;
//      lcTemp := A[j+16] + lcTemp;
      lcTemp := A[j+17] + lcTemp;
      lcTemp := A[j+18] + lcTemp;
      lcTemp := A[j+19] + lcTemp;

      lcTemp := A[j+20] + lcTemp;
      lcTemp := A[j+21] + lcTemp;
      lcTemp := A[j+22] + lcTemp;
      lcTemp := A[j+23] + lcTemp;
//      lcTemp := A[j+24] + lcTemp;

      lcTemp := A[j+25] + lcTemp;
      lcTemp := A[j+26] + lcTemp;
      lcTemp := A[j+27] + lcTemp;
      lcTemp := A[j+28] + lcTemp;
      lcTemp := A[j+29] + lcTemp;

      lcTemp := A[j+30] + lcTemp;
      lcTemp := A[j+31] + lcTemp;
//      lcTemp := A[j+32] + lcTemp;
      lcTemp := A[j+33] + lcTemp;
      lcTemp := A[j+34] + lcTemp;

      lcTemp := A[j+35] + lcTemp;
      lcTemp := A[j+36] + lcTemp;
      lcTemp := A[j+37] + lcTemp;
      lcTemp := A[j+38] + lcTemp;
      lcTemp := A[j+39] + lcTemp;

//      lcTemp := A[j+40] + lcTemp;
      lcTemp := A[j+41] + lcTemp;
      lcTemp := A[j+42] + lcTemp;
      lcTemp := A[j+43] + lcTemp;
      lcTemp := A[j+44] + lcTemp;

      lcTemp := A[j+45] + lcTemp;
      lcTemp := A[j+46] + lcTemp;
      lcTemp := A[j+47] + lcTemp;
//      lcTemp := A[j+48] + lcTemp;
      lcTemp := A[j+49] + lcTemp;

      lcTemp := A[j+50] + lcTemp;
      lcTemp := A[j+51] + lcTemp;
      lcTemp := A[j+52] + lcTemp;
      lcTemp := A[j+53] + lcTemp;
      lcTemp := A[j+54] + lcTemp;

      lcTemp := A[j+55] + lcTemp;
//      lcTemp := A[j+56] + lcTemp;
      lcTemp := A[j+57] + lcTemp;
      lcTemp := A[j+58] + lcTemp;
      lcTemp := A[j+59] + lcTemp;

      lcTemp := A[j+60] + lcTemp;
      lcTemp := A[j+61] + lcTemp;
      lcTemp := A[j+62] + lcTemp;
      lcTemp := A[j+63] + lcTemp;
//      lcTemp := A[j+64] + lcTemp;

      lcTemp := A[j+65] + lcTemp;
      lcTemp := A[j+66] + lcTemp;
      lcTemp := A[j+67] + lcTemp;
      lcTemp := A[j+68] + lcTemp;
      lcTemp := A[j+69] + lcTemp;

      lcTemp := A[j+70] + lcTemp;
      lcTemp := A[j+71] + lcTemp;
//      lcTemp := A[j+72] + lcTemp;
      lcTemp := A[j+73] + lcTemp;
      lcTemp := A[j+74] + lcTemp;

      lcTemp := A[j+75] + lcTemp;
      lcTemp := A[j+76] + lcTemp;
      lcTemp := A[j+77] + lcTemp;
      lcTemp := A[j+78] + lcTemp;
      lcTemp := A[j+79] + lcTemp;

//      lcTemp := A[j+80] + lcTemp;
      lcTemp := A[j+81] + lcTemp;
      lcTemp := A[j+82] + lcTemp;
      lcTemp := A[j+83] + lcTemp;
      lcTemp := A[j+84] + lcTemp;

      lcTemp := A[j+85] + lcTemp;
      lcTemp := A[j+86] + lcTemp;
      lcTemp := A[j+87] + lcTemp;
//      lcTemp := A[j+88] + lcTemp;
      lcTemp := A[j+89] + lcTemp;

      lcTemp := A[j+90] + lcTemp;
      lcTemp := A[j+91] + lcTemp;
      lcTemp := A[j+92] + lcTemp;
      lcTemp := A[j+93] + lcTemp;
      lcTemp := A[j+94] + lcTemp;

      lcTemp := A[j+95] + lcTemp;
//      lcTemp := A[j+96] + lcTemp;
      lcTemp := A[j+97] + lcTemp;
      lcTemp := A[j+98] + lcTemp;
      lcTemp := A[j+99] + lcTemp;

      lcTemp := A[j+100] + lcTemp;
      lcTemp := A[j+101] + lcTemp;
      lcTemp := A[j+102] + lcTemp;
      lcTemp := A[j+103] + lcTemp;
//      lcTemp := A[j+104] + lcTemp;

      lcTemp := A[j+105] + lcTemp;
      lcTemp := A[j+106] + lcTemp;
      lcTemp := A[j+107] + lcTemp;
      lcTemp := A[j+108] + lcTemp;
      lcTemp := A[j+109] + lcTemp;

      lcTemp := A[j+110] + lcTemp;
      lcTemp := A[j+111] + lcTemp;
//      lcTemp := A[j+112] + lcTemp;
      lcTemp := A[j+113] + lcTemp;
      lcTemp := A[j+114] + lcTemp;

      lcTemp := A[j+115] + lcTemp;
      lcTemp := A[j+116] + lcTemp;
      lcTemp := A[j+117] + lcTemp;
      lcTemp := A[j+118] + lcTemp;
      lcTemp := A[j+119] + lcTemp;

//      lcTemp := A[j+120] + lcTemp;
      lcTemp := A[j+121] + lcTemp;
      lcTemp := A[j+122] + lcTemp;
      lcTemp := A[j+123] + lcTemp;
      lcTemp := A[j+124] + lcTemp;

      lcTemp := A[j+125] + lcTemp;
      lcTemp := A[j+126] + lcTemp;
      lcTemp := A[j+127] + lcTemp;

      li64EndCycle := GetCycleCount;
      // calculate elapsed time:
      leElapsedTime :=
        ( ( li64EndCycle - li64StartCycle ) - SWOverhead ) / leClockspeed;
      leAccumulatedTime := leAccumulatedTime + leElapsedTime;
      // increment read index:
      j := j + 128;
    end; // while...
    // see if this is the fastest iteration:
    if leMinTime > leAccumulatedTime then begin
      leMinTime := leAccumulatedTime;
    end; // if
  end; // for...
  // assign to dummy variable or optimizer will remove code:
  FDummy := lcTemp;
  result := leMinTime;
end; // function TfrmCOSBIBandwidthBurn.BlockPrefetch32

function TfrmCOSBIBandwidthBurn.BP32Write(var A : TBigArray;
                                   var ai_RepeatCount : integer;
                                   var ai_ArraySize   : integer
                                   ): Extended;
var
  i                 : integer;
  li64StartCycle   : Int64;
  li64EndCycle     : Int64;
  liRandomData     : cardinal;
  lcTemp           : cardinal;
  liIterate        : integer;
  leElapsedTime     : extended;
  leAccumulatedTime : extended;
  leMinTime         : extended;
  leClockspeed      : extended;
begin
  // initialize variables
  leClockspeed := StopWatch.GetCPUClockspeed(FALSE);
  leElapsedTime := 0;
  leMinTime := 99999999999999;
  liRandomData   := Random(1000);
  // we iterate over RepeatCount to find best time
  for liIterate := 1 to ai_RepeatCount do begin
    leAccumulatedTime := 0;
    // at each step we need to repeat populating the array:
    i := 1;
    while i <= ai_ArraySize do begin
      lcTemp := 0;
      // start timer
      li64StartCycle := GetCycleCount;
      // block prefetch 4 32-byte lines:
      A[i] := liRandomData;
      A[i + 8] := liRandomData;
      A[i + 16] := liRandomData;
      A[i + 24] := liRandomData;

      // block prefetch 4 32-byte lines:
      A[i + 32] := liRandomData;
      A[i + 40] := liRandomData;
      A[i + 48] := liRandomData;
      A[i + 56] := liRandomData;

      // block prefetch 4 32-byte lines:
      A[i + 64] := liRandomData;
      A[i + 72] := liRandomData;
      A[i + 80] := liRandomData;
      A[i + 88] := liRandomData;

      // block prefetch 4 32-byte lines:
      A[i + 96] := liRandomData;
      A[i + 104] := liRandomData;
      A[i + 112] := liRandomData;
      A[i + 120] := liRandomData;

      // fill array
//      A[i] := liRandomData;
      A[i + 1] := liRandomData;
      A[i + 2] := liRandomData;
      A[i + 3] := liRandomData;
      A[i + 4] := liRandomData;

      A[i + 5] := liRandomData;
      A[i + 6] := liRandomData;
      A[i + 7] := liRandomData;
//      A[i + 8] := liRandomData;
      A[i + 9] := liRandomData;

      A[i + 10] := liRandomData;
      A[i + 11] := liRandomData;
      A[i + 12] := liRandomData;
      A[i + 13] := liRandomData;
      A[i + 14] := liRandomData;

      A[i + 15] := liRandomData;
//      A[i + 16] := liRandomData;
      A[i + 17] := liRandomData;
      A[i + 18] := liRandomData;
      A[i + 19] := liRandomData;

      A[i + 20] := liRandomData;
      A[i + 21] := liRandomData;
      A[i + 22] := liRandomData;
      A[i + 23] := liRandomData;
//      A[i + 24] := liRandomData;

      A[i + 25] := liRandomData;
      A[i + 26] := liRandomData;
      A[i + 27] := liRandomData;
      A[i + 28] := liRandomData;
      A[i + 29] := liRandomData;

      A[i + 30] := liRandomData;
      A[i + 31] := liRandomData;
//      A[i + 32] := liRandomData;
      A[i + 33] := liRandomData;
      A[i + 34] := liRandomData;

      A[i + 35] := liRandomData;
      A[i + 36] := liRandomData;
      A[i + 37] := liRandomData;
      A[i + 38] := liRandomData;
      A[i + 39] := liRandomData;

//      A[i + 40] := liRandomData;
      A[i + 41] := liRandomData;
      A[i + 42] := liRandomData;
      A[i + 43] := liRandomData;
      A[i + 44] := liRandomData;

      A[i + 45] := liRandomData;
      A[i + 46] := liRandomData;
      A[i + 47] := liRandomData;
//      A[i + 48] := liRandomData;
      A[i + 49] := liRandomData;

      A[i + 50] := liRandomData;
      A[i + 51] := liRandomData;
      A[i + 52] := liRandomData;
      A[i + 53] := liRandomData;
      A[i + 54] := liRandomData;

      A[i + 55] := liRandomData;
//      A[i + 56] := liRandomData;
      A[i + 57] := liRandomData;
      A[i + 58] := liRandomData;
      A[i + 59] := liRandomData;

      A[i + 60] := liRandomData;
      A[i + 61] := liRandomData;
      A[i + 62] := liRandomData;
      A[i + 63] := liRandomData;
//      A[i + 64] := liRandomData;

      A[i + 65] := liRandomData;
      A[i + 66] := liRandomData;
      A[i + 67] := liRandomData;
      A[i + 68] := liRandomData;
      A[i + 69] := liRandomData;

      A[i + 70] := liRandomData;
      A[i + 71] := liRandomData;
//      A[i + 72] := liRandomData;
      A[i + 73] := liRandomData;
      A[i + 74] := liRandomData;

      A[i + 75] := liRandomData;
      A[i + 76] := liRandomData;
      A[i + 77] := liRandomData;
      A[i + 78] := liRandomData;
      A[i + 79] := liRandomData;

//      A[i + 80] := liRandomData;
      A[i + 81] := liRandomData;
      A[i + 82] := liRandomData;
      A[i + 83] := liRandomData;
      A[i + 84] := liRandomData;

      A[i + 85] := liRandomData;
      A[i + 86] := liRandomData;
      A[i + 87] := liRandomData;
//      A[i + 88] := liRandomData;
      A[i + 89] := liRandomData;

      A[i + 90] := liRandomData;
      A[i + 91] := liRandomData;
      A[i + 92] := liRandomData;
      A[i + 93] := liRandomData;
      A[i + 94] := liRandomData;

      A[i + 95] := liRandomData;
//      A[i + 96] := liRandomData;
      A[i + 97] := liRandomData;
      A[i + 98] := liRandomData;
      A[i + 99] := liRandomData;

      A[i + 100] := liRandomData;
      A[i + 101] := liRandomData;
      A[i + 102] := liRandomData;
      A[i + 103] := liRandomData;
//      A[i + 104] := liRandomData;

      A[i + 105] := liRandomData;
      A[i + 106] := liRandomData;
      A[i + 107] := liRandomData;
      A[i + 108] := liRandomData;
      A[i + 109] := liRandomData;

      A[i + 110] := liRandomData;
      A[i + 111] := liRandomData;
//      A[i + 112] := liRandomData;
      A[i + 113] := liRandomData;
      A[i + 114] := liRandomData;

      A[i + 115] := liRandomData;
      A[i + 116] := liRandomData;
      A[i + 117] := liRandomData;
      A[i + 118] := liRandomData;
      A[i + 119] := liRandomData;

//      A[i + 120] := liRandomData;
      A[i + 121] := liRandomData;
      A[i + 122] := liRandomData;
      A[i + 123] := liRandomData;
      A[i + 124] := liRandomData;

      A[i + 125] := liRandomData;
      A[i + 126] := liRandomData;
      A[i + 127] := liRandomData;

      li64EndCycle := GetCycleCount;
      // calculate elapsed time:
      leElapsedTime :=
        ( ( li64EndCycle - li64StartCycle ) - SWOverhead ) / leClockspeed;
      leAccumulatedTime := leAccumulatedTime + leElapsedTime;
      // increment index:
      i := i + 128;
    end; // while...
    // see if this is the fastest iteration:
    if leMinTime > leAccumulatedTime then begin
      leMinTime := leAccumulatedTime;
    end; // if
  end; // for...
  // assign to dummy variable or optimizer will remove code:
  FDummy := lcTemp;
  result := leMinTime;
end; // function TfrmCOSBIBandwidthBurn.BP32Write

function TfrmCOSBIBandwidthBurn.BlockPrefetch64(var A : TBigArray;
                                   var ai_RepeatCount : integer;
                                   var ai_ArraySize   : integer
                                   ): Extended;
var
  li64StartCycle   : Int64;
  li64EndCycle     : Int64;
  liRandomData     : cardinal;
  lcTemp           : cardinal;
  i, j              : integer;
  liIterate        : integer;
  leElapsedTime     : extended;
  leAccumulatedTime : extended;
  leMinTime         : extended;
  leClockspeed      : extended;
begin
  // initialize variables
  leClockspeed := StopWatch.GetCPUClockspeed(FALSE);
  leElapsedTime := 0;
  leMinTime := 99999999999999;
  lcTemp := 0;
  liRandomData   := Random(1000);
  // we iterate over RepeatCount to find best time
  for liIterate := 1 to ai_RepeatCount do begin
    leAccumulatedTime := 0;
    // this is the critical loop where we populate the array:
    i := 1;
    while i <= ai_ArraySize do begin
      // start timer
      li64StartCycle := GetCycleCount;
      // block prefetch 8 64-byte lines:
      A[i] := liRandomData;
      A[i + 16] := liRandomData;
      A[i + 32] := liRandomData;
      A[i + 48] := liRandomData;
      A[i + 64] := liRandomData;
      A[i + 80] := liRandomData;
      A[i + 96] := liRandomData;
      A[i + 112] := liRandomData;

      // now initialize array
//      A[i] := liRandomData;
      A[i + 1] := liRandomData;
      A[i + 2] := liRandomData;
      A[i + 3] := liRandomData;
      A[i + 4] := liRandomData;

      A[i + 5] := liRandomData;
      A[i + 6] := liRandomData;
      A[i + 7] := liRandomData;
      A[i + 8] := liRandomData;
      A[i + 9] := liRandomData;

      A[i + 10] := liRandomData;
      A[i + 11] := liRandomData;
      A[i + 12] := liRandomData;
      A[i + 13] := liRandomData;
      A[i + 14] := liRandomData;

      A[i + 15] := liRandomData;
//      A[i + 16] := liRandomData;
      A[i + 17] := liRandomData;
      A[i + 18] := liRandomData;
      A[i + 19] := liRandomData;

      A[i + 20] := liRandomData;
      A[i + 21] := liRandomData;
      A[i + 22] := liRandomData;
      A[i + 23] := liRandomData;
      A[i + 24] := liRandomData;

      A[i + 25] := liRandomData;
      A[i + 26] := liRandomData;
      A[i + 27] := liRandomData;
      A[i + 28] := liRandomData;
      A[i + 29] := liRandomData;

      A[i + 30] := liRandomData;
      A[i + 31] := liRandomData;
//      A[i + 32] := liRandomData;
      A[i + 33] := liRandomData;
      A[i + 34] := liRandomData;

      A[i + 35] := liRandomData;
      A[i + 36] := liRandomData;
      A[i + 37] := liRandomData;
      A[i + 38] := liRandomData;
      A[i + 39] := liRandomData;

      A[i + 40] := liRandomData;
      A[i + 41] := liRandomData;
      A[i + 42] := liRandomData;
      A[i + 43] := liRandomData;
      A[i + 44] := liRandomData;

      A[i + 45] := liRandomData;
      A[i + 46] := liRandomData;
      A[i + 47] := liRandomData;
//      A[i + 48] := liRandomData;
      A[i + 49] := liRandomData;

      A[i + 50] := liRandomData;
      A[i + 51] := liRandomData;
      A[i + 52] := liRandomData;
      A[i + 53] := liRandomData;
      A[i + 54] := liRandomData;

      A[i + 55] := liRandomData;
      A[i + 56] := liRandomData;
      A[i + 57] := liRandomData;
      A[i + 58] := liRandomData;
      A[i + 59] := liRandomData;

      A[i + 60] := liRandomData;
      A[i + 61] := liRandomData;
      A[i + 62] := liRandomData;
      A[i + 63] := liRandomData;
//      A[i + 64] := liRandomData;

      A[i + 65] := liRandomData;
      A[i + 66] := liRandomData;
      A[i + 67] := liRandomData;
      A[i + 68] := liRandomData;
      A[i + 69] := liRandomData;

      A[i + 70] := liRandomData;
      A[i + 71] := liRandomData;
      A[i + 72] := liRandomData;
      A[i + 73] := liRandomData;
      A[i + 74] := liRandomData;

      A[i + 75] := liRandomData;
      A[i + 76] := liRandomData;
      A[i + 77] := liRandomData;
      A[i + 78] := liRandomData;
      A[i + 79] := liRandomData;

//      A[i + 80] := liRandomData;
      A[i + 81] := liRandomData;
      A[i + 82] := liRandomData;
      A[i + 83] := liRandomData;
      A[i + 84] := liRandomData;

      A[i + 85] := liRandomData;
      A[i + 86] := liRandomData;
      A[i + 87] := liRandomData;
      A[i + 88] := liRandomData;
      A[i + 89] := liRandomData;

      A[i + 90] := liRandomData;
      A[i + 91] := liRandomData;
      A[i + 92] := liRandomData;
      A[i + 93] := liRandomData;
      A[i + 94] := liRandomData;

      A[i + 95] := liRandomData;
//      A[i + 96] := liRandomData;
      A[i + 97] := liRandomData;
      A[i + 98] := liRandomData;
      A[i + 99] := liRandomData;

      A[i + 100] := liRandomData;
      A[i + 101] := liRandomData;
      A[i + 102] := liRandomData;
      A[i + 103] := liRandomData;
      A[i + 104] := liRandomData;

      A[i + 105] := liRandomData;
      A[i + 106] := liRandomData;
      A[i + 107] := liRandomData;
      A[i + 108] := liRandomData;
      A[i + 109] := liRandomData;

      A[i + 110] := liRandomData;
      A[i + 111] := liRandomData;
//      A[i + 112] := liRandomData;
      A[i + 113] := liRandomData;
      A[i + 114] := liRandomData;

      A[i + 115] := liRandomData;
      A[i + 116] := liRandomData;
      A[i + 117] := liRandomData;
      A[i + 118] := liRandomData;
      A[i + 119] := liRandomData;

      A[i + 120] := liRandomData;
      A[i + 121] := liRandomData;
      A[i + 122] := liRandomData;
      A[i + 123] := liRandomData;
      A[i + 124] := liRandomData;

      A[i + 125] := liRandomData;
      A[i + 126] := liRandomData;
      A[i + 127] := liRandomData;

      li64EndCycle := GetCycleCount;
      // calculate elapsed time:
      leElapsedTime :=
        ( ( li64EndCycle - li64StartCycle ) - SWOverhead ) / leClockspeed;
      leAccumulatedTime := leAccumulatedTime + leElapsedTime;
      // increment index:
      i := i + 128;
    end; // while...

    // read back data: must accumulate or optimizer will remove
    lcTemp := 0;
    j := 1;
    while j <= ai_ArraySize do begin
      // start timer
      li64StartCycle := GetCycleCount;

      // block prefetch 8 64-byte lines:
      lcTemp := A[i] + lcTemp;
      lcTemp := A[i + 16] + lcTemp;
      lcTemp := A[i + 32] + lcTemp;
      lcTemp := A[i + 48] + lcTemp;
      lcTemp := A[i + 64] + lcTemp;
      lcTemp := A[i + 80] + lcTemp;
      lcTemp := A[i + 96] + lcTemp;
      lcTemp := A[i + 112] + lcTemp;

      // copy data
//      lcTemp := A[i] + lcTemp;
      lcTemp := A[i+1] + lcTemp;
      lcTemp := A[i+2] - lcTemp;
      lcTemp := A[i+3] + lcTemp;
      lcTemp := A[i+4] - lcTemp;

      lcTemp := A[i+5] + lcTemp;
      lcTemp := A[i+6] - lcTemp;
      lcTemp := A[i+7] + lcTemp;
      lcTemp := A[i+8] - lcTemp;
      lcTemp := A[i+9] + lcTemp;

      lcTemp := A[i+10] - lcTemp;
      lcTemp := A[i+11] + lcTemp;
      lcTemp := A[i+12] - lcTemp;
      lcTemp := A[i+13] + lcTemp;
      lcTemp := A[i+14] - lcTemp;

      lcTemp := A[i+15] + lcTemp;
//      lcTemp := A[i+16] + lcTemp;
      lcTemp := A[i+17] - lcTemp;
      lcTemp := A[i+18] + lcTemp;
      lcTemp := A[i+19] - lcTemp;

      lcTemp := A[i+20] + lcTemp;
      lcTemp := A[i+21] - lcTemp;
      lcTemp := A[i+22] + lcTemp;
      lcTemp := A[i+23] - lcTemp;
      lcTemp := A[i+24] + lcTemp;

      lcTemp := A[i+25] - lcTemp;
      lcTemp := A[i+26] + lcTemp;
      lcTemp := A[i+27] - lcTemp;
      lcTemp := A[i+28] + lcTemp;
      lcTemp := A[i+29] - lcTemp;

      lcTemp := A[i+30] + lcTemp;
      lcTemp := A[i+31] - lcTemp;
//      lcTemp := A[i+32] + lcTemp;
      lcTemp := A[i+33] + lcTemp;
      lcTemp := A[i+34] - lcTemp;

      lcTemp := A[i+35] + lcTemp;
      lcTemp := A[i+36] - lcTemp;
      lcTemp := A[i+37] + lcTemp;
      lcTemp := A[i+38] - lcTemp;
      lcTemp := A[i+39] + lcTemp;

      lcTemp := A[i+40] - lcTemp;
      lcTemp := A[i+41] + lcTemp;
      lcTemp := A[i+42] - lcTemp;
      lcTemp := A[i+43] + lcTemp;
      lcTemp := A[i+44] - lcTemp;

      lcTemp := A[i+45] + lcTemp;
      lcTemp := A[i+46] - lcTemp;
      lcTemp := A[i+47] + lcTemp;
//      lcTemp := A[i+48] + lcTemp;
      lcTemp := A[i+49] - lcTemp;

      lcTemp := A[i+50] + lcTemp;
      lcTemp := A[i+51] - lcTemp;
      lcTemp := A[i+52] + lcTemp;
      lcTemp := A[i+53] - lcTemp;
      lcTemp := A[i+54] + lcTemp;

      lcTemp := A[i+55] - lcTemp;
      lcTemp := A[i+56] + lcTemp;
      lcTemp := A[i+57] - lcTemp;
      lcTemp := A[i+58] + lcTemp;
      lcTemp := A[i+59] - lcTemp;

      lcTemp := A[i+60] + lcTemp;
      lcTemp := A[i+61] - lcTemp;
      lcTemp := A[i+62] + lcTemp;
      lcTemp := A[i+63] - lcTemp;
//      lcTemp := A[i+64] + lcTemp;

      lcTemp := A[i+65] + lcTemp;
      lcTemp := A[i+66] - lcTemp;
      lcTemp := A[i+67] + lcTemp;
      lcTemp := A[i+68] - lcTemp;
      lcTemp := A[i+69] + lcTemp;

      lcTemp := A[i+70] - lcTemp;
      lcTemp := A[i+71] + lcTemp;
      lcTemp := A[i+72] - lcTemp;
      lcTemp := A[i+73] + lcTemp;
      lcTemp := A[i+74] - lcTemp;

      lcTemp := A[i+75] + lcTemp;
      lcTemp := A[i+76] - lcTemp;
      lcTemp := A[i+77] + lcTemp;
      lcTemp := A[i+78] - lcTemp;
      lcTemp := A[i+79] + lcTemp;

//      lcTemp := A[i+80] + lcTemp;
      lcTemp := A[i+81] - lcTemp;
      lcTemp := A[i+82] + lcTemp;
      lcTemp := A[i+83] - lcTemp;
      lcTemp := A[i+84] + lcTemp;

      lcTemp := A[i+85] - lcTemp;
      lcTemp := A[i+86] + lcTemp;
      lcTemp := A[i+87] - lcTemp;
      lcTemp := A[i+88] + lcTemp;
      lcTemp := A[i+89] - lcTemp;

      lcTemp := A[i+90] + lcTemp;
      lcTemp := A[i+91] - lcTemp;
      lcTemp := A[i+92] + lcTemp;
      lcTemp := A[i+93] - lcTemp;
      lcTemp := A[i+94] + lcTemp;

      lcTemp := A[i+95] - lcTemp;
//      lcTemp := A[i+96] + lcTemp;
      lcTemp := A[i+97] + lcTemp;
      lcTemp := A[i+98] - lcTemp;
      lcTemp := A[i+99] + lcTemp;

      lcTemp := A[i+100] - lcTemp;
      lcTemp := A[i+101] + lcTemp;
      lcTemp := A[i+102] - lcTemp;
      lcTemp := A[i+103] + lcTemp;
      lcTemp := A[i+104] - lcTemp;

      lcTemp := A[i+105] + lcTemp;
      lcTemp := A[i+106] - lcTemp;
      lcTemp := A[i+107] + lcTemp;
      lcTemp := A[i+108] - lcTemp;
      lcTemp := A[i+109] + lcTemp;

      lcTemp := A[i+110] - lcTemp;
      lcTemp := A[i+111] + lcTemp;
//      lcTemp := A[i+112] + lcTemp;
      lcTemp := A[i+113] - lcTemp;
      lcTemp := A[i+114] + lcTemp;

      lcTemp := A[i+115] - lcTemp;
      lcTemp := A[i+116] + lcTemp;
      lcTemp := A[i+117] - lcTemp;
      lcTemp := A[i+118] + lcTemp;
      lcTemp := A[i+119] - lcTemp;

      lcTemp := A[i+120] + lcTemp;
      lcTemp := A[i+121] - lcTemp;
      lcTemp := A[i+122] + lcTemp;
      lcTemp := A[i+123] - lcTemp;
      lcTemp := A[i+124] + lcTemp;

      lcTemp := A[i+125] - lcTemp;
      lcTemp := A[i+126] + lcTemp;
      lcTemp := A[i+127] - lcTemp;

      li64EndCycle := GetCycleCount;
      // calculate elapsed time:
      leElapsedTime :=
        ( ( li64EndCycle - li64StartCycle ) - SWOverhead ) / leClockspeed;
      leAccumulatedTime := leAccumulatedTime + leElapsedTime;
      // increment read index:
      j := j + 128;
    end; // while...
    // see if this is the fastest iteration:
    if leMinTime > leAccumulatedTime then begin
      leMinTime := leAccumulatedTime;
    end; // if
  end; // for...
  // assign to dummy variable or optimizer will remove code:
  FDummy := lcTemp;
  result := leMinTime;
end; // function TfrmCOSBIBandwidthBurn.BlockPrefetch64

function TfrmCOSBIBandwidthBurn.GetMaxDatasetSize: integer;
begin
  result := fMaxDatasetSize;
end; // function TfrmCOSBIBandwidthBurn.GetMaxDatasetSize: integer;

procedure TfrmCOSBIBandwidthBurn.SetMaxDatasetSize(Value : integer);
begin
  cboMaxDataset.Text := IntToStr( Value );
  fMaxDatasetSize := Value;
end; // procedure TfrmCOSBIBandwidthBurn.SetMaxDatasetSize

procedure TfrmCOSBIBandwidthBurn.cboMaxDatasetChange(Sender: TObject);
begin
  MaxDatasetSize := StrToInt( cboMaxDataset.Text );
  if ( MaxDatasetSize > MAX_DATASET_THRESHOLD ) and ( fStepSize < 10 ) then begin
    ShowMessage( 'I need to adjust the step size to 10 kB because you have chosen a dataset above 3MB.' ); 
    fStepSize := 10;
    cboStepSize.Text := '10';
  end; // if
end; // procedure TfrmCOSBIBandwidthBurn.cboMaxDatasetChange

procedure TfrmCOSBIBandwidthBurn.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FadeFast100( self );
  if fFreeOnClose then begin
    Application.Terminate;
    fBreak := TRUE;
    Application.ProcessMessages;
  end else begin
//    Release;
  end;
end; // procedure TfrmCOSBIBandwidthBurn.FormClose

procedure TfrmCOSBIBandwidthBurn.btnSaveGraphClick(Sender: TObject);
begin
  chartBandwidth.SaveToBitmapFile( edSaveGraph.Text );
end;

procedure TfrmCOSBIBandwidthBurn.btnBrowseSaveDataClick(Sender: TObject);
begin
  if SaveDialog1.Execute then edSave.Text := SaveDialog1.FileName;
  SaveData('');
end; // procedure TfrmCOSBIBandwidthBurn.btnBrowseSaveDataClick

procedure TfrmCOSBIBandwidthBurn.btnBrowseSaveGraphClick(Sender: TObject);
var
  ls_filename : string;
begin
  if SaveDialog1.Execute then begin
    ls_filename := SaveDialog1.FileName;
    if Pos(GRAPH_FILE_SUFFIX, LowerCase( ls_filename )) = 0 then begin
      ls_filename := ls_filename + GRAPH_FILE_SUFFIX;
    end;
    edSaveGraph.Text := ls_filename;
    chartBandwidth.SaveToBitmapFile( edSaveGraph.Text );
  end; // if
end; // procedure TfrmCOSBIBandwidthBurn.btnBrowseSaveGraphClick

procedure TfrmCOSBIBandwidthBurn.btnChangeBackgroundClick(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    chartBandwidth.BackImage.LoadFromFile( OpenDialog1.FileName );
  end; // if
end;


function TfrmCOSBIBandwidthBurn.Skip1Line(var A : TBigArray;
                                   var ai_RepeatCount : integer;
                                   var ai_ArraySize   : integer
                                   ): Extended;
var
  li64StartCycle   : Int64;
  li64EndCycle     : Int64;
  liRandomData     : cardinal;
  lcTemp           : cardinal;
  i, j              : integer;
  liIterate        : integer;
  leElapsedTime     : extended;
  leAccumulatedTime : extended;
  leMinTime         : extended;
  leClockspeed      : extended;
begin
  // initialize variables
  leClockspeed := StopWatch.GetCPUClockspeed(FALSE);
  leElapsedTime := 0;
  leMinTime := 99999999999999;
  lcTemp := 0;
  liRandomData   := Random(1000);
  // we iterate over RepeatCount to find best time
  for liIterate := 1 to ai_RepeatCount do begin
    leAccumulatedTime := 0;
    // this is the critical loop where we populate the array:
    i := 1;
    while i <= ai_ArraySize do begin
      // start timer
      li64StartCycle := GetCycleCount;
//      // block prefetch 4 64-byte lines:
//      A[i] := liRandomData;
//      A[i + 16] := liRandomData;
//      A[i + 32] := liRandomData;
//      A[i + 48] := liRandomData;
//      A[i + 64] := liRandomData;
//      A[i + 80] := liRandomData;
//      A[i + 96] := liRandomData;
//      A[i + 112] := liRandomData;

      // now initialize array
      A[i] := liRandomData;
      A[i + 1] := liRandomData;
      A[i + 2] := liRandomData;
      A[i + 3] := liRandomData;
      A[i + 4] := liRandomData;

      A[i + 5] := liRandomData;
      A[i + 6] := liRandomData;
      A[i + 7] := liRandomData;
      A[i + 8] := liRandomData;
      A[i + 9] := liRandomData;

      A[i + 10] := liRandomData;
      A[i + 11] := liRandomData;
      A[i + 12] := liRandomData;
      A[i + 13] := liRandomData;
      A[i + 14] := liRandomData;

      A[i + 15] := liRandomData;
      A[i + 16] := liRandomData;
      A[i + 17] := liRandomData;
      A[i + 18] := liRandomData;
      A[i + 19] := liRandomData;

      A[i + 20] := liRandomData;
      A[i + 21] := liRandomData;
      A[i + 22] := liRandomData;
      A[i + 23] := liRandomData;
      A[i + 24] := liRandomData;

      A[i + 25] := liRandomData;
      A[i + 26] := liRandomData;
      A[i + 27] := liRandomData;
      A[i + 28] := liRandomData;
      A[i + 29] := liRandomData;

      A[i + 30] := liRandomData;
      A[i + 31] := liRandomData;
      A[i + 32] := liRandomData;
      A[i + 33] := liRandomData;
      A[i + 34] := liRandomData;

      A[i + 35] := liRandomData;
      A[i + 36] := liRandomData;
      A[i + 37] := liRandomData;
      A[i + 38] := liRandomData;
      A[i + 39] := liRandomData;

      A[i + 40] := liRandomData;
      A[i + 41] := liRandomData;
      A[i + 42] := liRandomData;
      A[i + 43] := liRandomData;
      A[i + 44] := liRandomData;

      A[i + 45] := liRandomData;
      A[i + 46] := liRandomData;
      A[i + 47] := liRandomData;
      A[i + 48] := liRandomData;
      A[i + 49] := liRandomData;

      A[i + 50] := liRandomData;
      A[i + 51] := liRandomData;
      A[i + 52] := liRandomData;
      A[i + 53] := liRandomData;
      A[i + 54] := liRandomData;

      A[i + 55] := liRandomData;
      A[i + 56] := liRandomData;
      A[i + 57] := liRandomData;
      A[i + 58] := liRandomData;
      A[i + 59] := liRandomData;

      A[i + 60] := liRandomData;
      A[i + 61] := liRandomData;
      A[i + 62] := liRandomData;
      A[i + 63] := liRandomData;
//      A[i + 64] := liRandomData;
//
//      A[i + 65] := liRandomData;
//      A[i + 66] := liRandomData;
//      A[i + 67] := liRandomData;
//      A[i + 68] := liRandomData;
//      A[i + 69] := liRandomData;
//
//      A[i + 70] := liRandomData;
//      A[i + 71] := liRandomData;
//      A[i + 72] := liRandomData;
//      A[i + 73] := liRandomData;
//      A[i + 74] := liRandomData;
//
//      A[i + 75] := liRandomData;
//      A[i + 76] := liRandomData;
//      A[i + 77] := liRandomData;
//      A[i + 78] := liRandomData;
//      A[i + 79] := liRandomData;
//
////      A[i + 80] := liRandomData;
//      A[i + 81] := liRandomData;
//      A[i + 82] := liRandomData;
//      A[i + 83] := liRandomData;
//      A[i + 84] := liRandomData;
//
//      A[i + 85] := liRandomData;
//      A[i + 86] := liRandomData;
//      A[i + 87] := liRandomData;
//      A[i + 88] := liRandomData;
//      A[i + 89] := liRandomData;
//
//      A[i + 90] := liRandomData;
//      A[i + 91] := liRandomData;
//      A[i + 92] := liRandomData;
//      A[i + 93] := liRandomData;
//      A[i + 94] := liRandomData;
//
//      A[i + 95] := liRandomData;
////      A[i + 96] := liRandomData;
//      A[i + 97] := liRandomData;
//      A[i + 98] := liRandomData;
//      A[i + 99] := liRandomData;
//
//      A[i + 100] := liRandomData;
//      A[i + 101] := liRandomData;
//      A[i + 102] := liRandomData;
//      A[i + 103] := liRandomData;
//      A[i + 104] := liRandomData;
//
//      A[i + 105] := liRandomData;
//      A[i + 106] := liRandomData;
//      A[i + 107] := liRandomData;
//      A[i + 108] := liRandomData;
//      A[i + 109] := liRandomData;
//
//      A[i + 110] := liRandomData;
//      A[i + 111] := liRandomData;
////      A[i + 112] := liRandomData;
//      A[i + 113] := liRandomData;
//      A[i + 114] := liRandomData;
//
//      A[i + 115] := liRandomData;
//      A[i + 116] := liRandomData;
//      A[i + 117] := liRandomData;
//      A[i + 118] := liRandomData;
//      A[i + 119] := liRandomData;
//
//      A[i + 120] := liRandomData;
//      A[i + 121] := liRandomData;
//      A[i + 122] := liRandomData;
//      A[i + 123] := liRandomData;
//      A[i + 124] := liRandomData;
//
//      A[i + 125] := liRandomData;
//      A[i + 126] := liRandomData;
//      A[i + 127] := liRandomData;

      li64EndCycle := GetCycleCount;
      // calculate elapsed time:
      leElapsedTime :=
        ( ( li64EndCycle - li64StartCycle ) - SWOverhead ) / leClockspeed;
      leAccumulatedTime := leAccumulatedTime + leElapsedTime;
      // increment index:
      i := i + 128;
    end; // while...

    // read back data: must accumulate or optimizer will remove
    lcTemp := 0;
    j := 1;
    while j <= ai_ArraySize do begin
      // start timer
      li64StartCycle := GetCycleCount;

      // block prefetch 8 64-byte lines:
//      lcTemp := A[i] + lcTemp;
//      lcTemp := A[i + 16] + lcTemp;
//      lcTemp := A[i + 32] + lcTemp;
//      lcTemp := A[i + 48] + lcTemp;
//      lcTemp := A[i + 64] + lcTemp;
//      lcTemp := A[i + 80] + lcTemp;
//      lcTemp := A[i + 96] + lcTemp;
//      lcTemp := A[i + 112] + lcTemp;

      // copy data
      lcTemp := A[i] + lcTemp;
      lcTemp := A[i+1] + lcTemp;
      lcTemp := A[i+2] - lcTemp;
      lcTemp := A[i+3] + lcTemp;
      lcTemp := A[i+4] - lcTemp;

      lcTemp := A[i+5] + lcTemp;
      lcTemp := A[i+6] - lcTemp;
      lcTemp := A[i+7] + lcTemp;
      lcTemp := A[i+8] - lcTemp;
      lcTemp := A[i+9] + lcTemp;

      lcTemp := A[i+10] - lcTemp;
      lcTemp := A[i+11] + lcTemp;
      lcTemp := A[i+12] - lcTemp;
      lcTemp := A[i+13] + lcTemp;
      lcTemp := A[i+14] - lcTemp;

      lcTemp := A[i+15] + lcTemp;
      lcTemp := A[i+16] + lcTemp;
      lcTemp := A[i+17] - lcTemp;
      lcTemp := A[i+18] + lcTemp;
      lcTemp := A[i+19] - lcTemp;

      lcTemp := A[i+20] + lcTemp;
      lcTemp := A[i+21] - lcTemp;
      lcTemp := A[i+22] + lcTemp;
      lcTemp := A[i+23] - lcTemp;
      lcTemp := A[i+24] + lcTemp;

      lcTemp := A[i+25] - lcTemp;
      lcTemp := A[i+26] + lcTemp;
      lcTemp := A[i+27] - lcTemp;
      lcTemp := A[i+28] + lcTemp;
      lcTemp := A[i+29] - lcTemp;

      lcTemp := A[i+30] + lcTemp;
      lcTemp := A[i+31] - lcTemp;
      lcTemp := A[i+32] + lcTemp;
      lcTemp := A[i+33] + lcTemp;
      lcTemp := A[i+34] - lcTemp;

      lcTemp := A[i+35] + lcTemp;
      lcTemp := A[i+36] - lcTemp;
      lcTemp := A[i+37] + lcTemp;
      lcTemp := A[i+38] - lcTemp;
      lcTemp := A[i+39] + lcTemp;

      lcTemp := A[i+40] - lcTemp;
      lcTemp := A[i+41] + lcTemp;
      lcTemp := A[i+42] - lcTemp;
      lcTemp := A[i+43] + lcTemp;
      lcTemp := A[i+44] - lcTemp;

      lcTemp := A[i+45] + lcTemp;
      lcTemp := A[i+46] - lcTemp;
      lcTemp := A[i+47] + lcTemp;
      lcTemp := A[i+48] + lcTemp;
      lcTemp := A[i+49] - lcTemp;

      lcTemp := A[i+50] + lcTemp;
      lcTemp := A[i+51] - lcTemp;
      lcTemp := A[i+52] + lcTemp;
      lcTemp := A[i+53] - lcTemp;
      lcTemp := A[i+54] + lcTemp;

      lcTemp := A[i+55] - lcTemp;
      lcTemp := A[i+56] + lcTemp;
      lcTemp := A[i+57] - lcTemp;
      lcTemp := A[i+58] + lcTemp;
      lcTemp := A[i+59] - lcTemp;

      lcTemp := A[i+60] + lcTemp;
      lcTemp := A[i+61] - lcTemp;
      lcTemp := A[i+62] + lcTemp;
      lcTemp := A[i+63] - lcTemp;
////      lcTemp := A[i+64] + lcTemp;
//
//      lcTemp := A[i+65] + lcTemp;
//      lcTemp := A[i+66] - lcTemp;
//      lcTemp := A[i+67] + lcTemp;
//      lcTemp := A[i+68] - lcTemp;
//      lcTemp := A[i+69] + lcTemp;
//
//      lcTemp := A[i+70] - lcTemp;
//      lcTemp := A[i+71] + lcTemp;
//      lcTemp := A[i+72] - lcTemp;
//      lcTemp := A[i+73] + lcTemp;
//      lcTemp := A[i+74] - lcTemp;
//
//      lcTemp := A[i+75] + lcTemp;
//      lcTemp := A[i+76] - lcTemp;
//      lcTemp := A[i+77] + lcTemp;
//      lcTemp := A[i+78] - lcTemp;
//      lcTemp := A[i+79] + lcTemp;
//
////      lcTemp := A[i+80] + lcTemp;
//      lcTemp := A[i+81] - lcTemp;
//      lcTemp := A[i+82] + lcTemp;
//      lcTemp := A[i+83] - lcTemp;
//      lcTemp := A[i+84] + lcTemp;
//
//      lcTemp := A[i+85] - lcTemp;
//      lcTemp := A[i+86] + lcTemp;
//      lcTemp := A[i+87] - lcTemp;
//      lcTemp := A[i+88] + lcTemp;
//      lcTemp := A[i+89] - lcTemp;
//
//      lcTemp := A[i+90] + lcTemp;
//      lcTemp := A[i+91] - lcTemp;
//      lcTemp := A[i+92] + lcTemp;
//      lcTemp := A[i+93] - lcTemp;
//      lcTemp := A[i+94] + lcTemp;
//
//      lcTemp := A[i+95] - lcTemp;
////      lcTemp := A[i+96] + lcTemp;
//      lcTemp := A[i+97] + lcTemp;
//      lcTemp := A[i+98] - lcTemp;
//      lcTemp := A[i+99] + lcTemp;
//
//      lcTemp := A[i+100] - lcTemp;
//      lcTemp := A[i+101] + lcTemp;
//      lcTemp := A[i+102] - lcTemp;
//      lcTemp := A[i+103] + lcTemp;
//      lcTemp := A[i+104] - lcTemp;
//
//      lcTemp := A[i+105] + lcTemp;
//      lcTemp := A[i+106] - lcTemp;
//      lcTemp := A[i+107] + lcTemp;
//      lcTemp := A[i+108] - lcTemp;
//      lcTemp := A[i+109] + lcTemp;
//
//      lcTemp := A[i+110] - lcTemp;
//      lcTemp := A[i+111] + lcTemp;
////      lcTemp := A[i+112] + lcTemp;
//      lcTemp := A[i+113] - lcTemp;
//      lcTemp := A[i+114] + lcTemp;
//
//      lcTemp := A[i+115] - lcTemp;
//      lcTemp := A[i+116] + lcTemp;
//      lcTemp := A[i+117] - lcTemp;
//      lcTemp := A[i+118] + lcTemp;
//      lcTemp := A[i+119] - lcTemp;
//
//      lcTemp := A[i+120] + lcTemp;
//      lcTemp := A[i+121] - lcTemp;
//      lcTemp := A[i+122] + lcTemp;
//      lcTemp := A[i+123] - lcTemp;
//      lcTemp := A[i+124] + lcTemp;
//
//      lcTemp := A[i+125] - lcTemp;
//      lcTemp := A[i+126] + lcTemp;
//      lcTemp := A[i+127] - lcTemp;

      li64EndCycle := GetCycleCount;
      // calculate elapsed time:
      leElapsedTime :=
        ( ( li64EndCycle - li64StartCycle ) - SWOverhead ) / leClockspeed;
      leAccumulatedTime := leAccumulatedTime + leElapsedTime;
      // increment read index:
      j := j + 128;
    end; // while...
    // see if this is the fastest iteration:
    if leMinTime > leAccumulatedTime then begin
      leMinTime := leAccumulatedTime;
    end; // if
  end; // for...
  // assign to dummy variable or optimizer will remove code:
  FDummy := lcTemp;
  result := leMinTime;
end; // function TfrmCOSBIBandwidthBurn.BlockPrefetch64

procedure TfrmCOSBIBandwidthBurn.FormActivate(Sender: TObject);
begin
  UnFadeFast( self );
end;

procedure TfrmCOSBIBandwidthBurn.FormDeactivate(Sender: TObject);
begin
  FadeFast50( self );
end;

end.
