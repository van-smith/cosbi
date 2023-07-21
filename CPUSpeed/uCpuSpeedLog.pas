unit uCpuSpeedLog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, COSBI_Common;

type
  TfrmCpuSpeedLog = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fLogArray               : array[0..ONE_MILLION] of int64;
    fLogArrayIndex          : integer;
    fLogStartTick           : int64;
    fLogStopTick            : int64;
  public
    { Public declarations }
  end;

var
  frmCpuSpeedLog: TfrmCpuSpeedLog;

implementation

{$R *.dfm}

procedure TfrmCpuSpeedLog.FormCreate(Sender: TObject);
begin
  fLogArrayIndex := 0;

end;

//procedure TfrmCPUSpeed.SaveLogData;
//var
//  lTextFileOut  : TextFile;
//  lsFileName    : string;
//  lsOutLine     : string;
//  x             : double;
//  n             : integer;
//const
//  DEFAULT_FILENAME = 'CpuClockSpeed_LOG.txt';
//begin
//  SaveDialog1.Filter := '*.txt';
//  SaveDialog1.FileName := DEFAULT_FILENAME;
//  if SaveDialog1.Execute then begin
//    lsFileName := Trim( SaveDialog1.FileName );
//    if lsFileName = '' then lsFileName := DEFAULT_FILENAME;
//    AssignFile( lTextFileOut, lsFileName );
//    try
//      Rewrite( lTextFileOut );
//      WriteLn( lTextFileOut, 'Cpu Speed Log File');
//      WriteLn( lTextFileOut, '' );
//      lsOutLine := 'Date: ' + DateToStr( Date ) + ', Time: ' + TimeToStr( Time )
//        + ', Update Interval: ' + cboLogPeriod.Text + ' seconds';
//      WriteLn( lTextFileOut, lsOutLine );
//      WriteLn( lTextFileOut, '' );
//      WriteLn( lTextFileOut, 'Sample    Clock speed (MHz)');
//      WriteLn( lTextFileOut, '========  =================');
//      for n := 0 to fLogArrayIndex do begin
//        x := n;
//        lsOutline := Format('%6.0n' + Chr(9) + '   %11d', [ x, fLogArray[n] ]);
//        WriteLn( lTextFileOut, lsOutline);
//      end; // for
//    finally
//      CloseFile( lTextFileOut );
//    end; // try
//  end; // if
//end; // procedure TfrmCPUSpeed.SaveChartData
//
//procedure TfrmCPUSpeed.StartLogging;
//begin
//  fLogArrayIndex := -1;
//  fLogStartTick := 0;
//  TimerLog.Interval := round( StrToFloat( cboLogPeriod.Text ) * 1000 );
//  TimerLog.Enabled := TRUE;
//  btnLogCpuSpeed.Caption := 'Stop Logging';
//  btnSaveLogFile.Enabled := FALSE;
//  cboLogPeriod.Enabled := FALSE;
//end;
//
//procedure TfrmCPUSpeed.StopLogging;
//begin
//  TimerLog.Enabled := FALSE;
//  btnLogCpuSpeed.Caption := 'Start Logging';
//  btnSaveLogFile.Enabled := TRUE;
//  cboLogPeriod.Enabled := TRUE;
//end;

//procedure TfrmCPUSpeed.btnLogCpuSpeedClick(Sender: TObject);
//begin
//  if TimerLog.Enabled then begin
//    StopLogging;
//  end else begin
//    StartLogging;
//  end; //  if
//end;

//procedure TfrmCPUSpeed.UpdateLog;
//var
//  ldElapsedTime : double;
//begin
//  fLogArrayIndex := fLogArrayIndex + 1;
//  if fIsTransmeta then begin
//    fLogArray[ fLogArrayIndex ] := GetTransmetaClockSpeed;
//  end else if fIsC7 then begin
//    fLogArray[ fLogArrayIndex ] := GetC7ClockSpeed;
//  end else begin
////    fLogArray[ fLogArrayIndex ] := GetCpuMhz;
////    if fLogStartTick <> 0 then begin
////      ldElapsedTime := StopWatchLog.StopTimer;
////      fLogStopTick  := GetCycleCount;
////      fLogArray[ fLogArrayIndex ] := round(
////        ( fLogStopTick - fLogStartTick ) / ldElapsedTime );
////      fLogStartTick := GetCycleCount;
////      StopWatchLog.StartTimer;
////    end else begin
////      fLogStartTick   := GetCycleCount;
////      fLogArrayIndex  := -1;
////      StopWatchLog.StartTimer;
////    end;
//  end; // if
//end; // procedure TfrmCPUSpeed.UpdateLog;
//
//procedure TfrmCPUSpeed.btnSaveLogFileClick(Sender: TObject);
//begin
//  SaveLogData;
//end;
//
//procedure TfrmCPUSpeed.TimerLogTimer(Sender: TObject);
//begin
//  UpdateLog;
//end; // procedure TfrmCPUSpeed.TimerLogTimer(Sender: TObject);

end.
