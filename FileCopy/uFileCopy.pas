unit uFileCopy;
{
  COSBI: Comprehensive Open Source Benchmarking Initiative
  Copyright (c) 2004 Van Smith

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
  StdCtrls, COSBI_Common, ExtCtrls, FileCtrl, XPMan, uCOSBI_SystemInfo,
  ComCtrls, uStopWatch;

type
  TfrmCopyFile = class(TForm)
    btnRun: TButton;
    memoResults: TMemo;
    driveComboSource: TDriveComboBox;
    driveComboTarget: TDriveComboBox;
    lblSourceDrive: TLabel;
    Label3: TLabel;
    cboSize: TComboBox;
    lblSize: TLabel;
    StatusBar1: TStatusBar;
    cboxDetails: TCheckBox;
    btnSave: TButton;
    btnScoring: TButton;
    pbar_overall: TProgressBar;
    pbar_test: TProgressBar;
    lblIterations: TLabel;
    cboIterations: TComboBox;
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure driveComboSourceChange(Sender: TObject);
    procedure driveComboTargetChange(Sender: TObject);
    procedure cboSizeChange(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure btnScoringClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cboIterationsChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    StopWatch : TStopWatch;
    fStatusBar : TStatusPanel;
    fSourceDrive : string;
    fTargetDrive : string;
    fFileSize : int64;
    fFileSizeMB: integer;
    // performance data fields
    fCreateTimeMin : extended;
    fCreateTimeAvg : extended;
    fCreateRateMax : double;
    fCreateRateAvg : double;
    fCopyTimeMin  : extended;
    fCopyTimeAvg  : extended;
    fCopyRateMax  : double;
    fCopyRateAvg  : double;
    fIterations   : integer;
    fSystemInfo   : TSystemInfo;
    procedure EnableDisableControls(abol_Enable : Boolean);
    procedure CreateSource(as_filename: string;
                          ai_filesize : integer;
                          var ae_time : extended;
                          var ad_rate : double
                          );
    procedure CopyFileToTarget( as_sourceFile: string;
                                as_targetFile: string;
                                var ae_time : extended;
                                var ad_rate : double
                                );
    procedure RunTests;
    procedure OutputSystemInfo;
  public
    { Public declarations }
    property SystemInfo: TSystemInfo        read  fSystemInfo
                                            write  fSystemInfo;
  end;

var
  frmCopyFile: TfrmCopyFile;
  gpbar_test : TProgressBar;

implementation

{$R *.DFM}

const
  DEFAULT_SOURCE_FILENAME = 'DeleteMe_source.txt';
  DEFAULT_OUTPUT_FILENAME = 'DeleteMe_target.txt';
  ONE_KILOBYTE = 1024;
  ONE_MEGABYTE = ONE_KILOBYTE * ONE_KILOBYTE;
  ITERATION_COUNT = 6;

  BASELINE_FILE_CREATION_RATE_MAX    =  36.709 * ONE_MEGABYTE;
  BASELINE_FILE_CREATION_RATE_AVG    =  36.145 * ONE_MEGABYTE;
  BASELINE_FILE_COPY_RATE_MAX        =   7.922 * ONE_MEGABYTE;
  BASELINE_FILE_COPY_RATE_AVG        =   6.933 * ONE_MEGABYTE;

  BASELINE_FILE_CREATION_RATE_AVG_1  =  43.328 * ONE_MEGABYTE;
  BASELINE_FILE_COPY_RATE_AVG_1      = 149.459 * ONE_MEGABYTE;

  BASELINE_FILE_CREATION_RATE_AVG_10 =  44.021 * ONE_MEGABYTE;
  BASELINE_FILE_COPY_RATE_AVG_10     = 184.093 * ONE_MEGABYTE;

  BASELINE_FILE_CREATION_RATE_AVG_100 = 39.568 * ONE_MEGABYTE;
  BASELINE_FILE_COPY_RATE_AVG_100     = 23.665 * ONE_MEGABYTE;

  DEFAULT_FILENAME = 'COSBI_FileCopy.txt';
  FILE_SUFFIX = '.txt';
  VERSION = 'v0.6';

// this function is used as callback function that is called each time another
// portion of the file has been copied
// this function is only supported on WinNT platform.
function CopyProgressCallBack(TotalFileSize : LARGE_INTEGER;
                              TotalBytesTransferred : LARGE_INTEGER;
                              StreamSize : LARGE_INTEGER;
                              StreamBytesTransferred : LARGE_INTEGER;
                              dwStreamNumber : DWORD;
                              dwCallbackReason : DWORD;
                              hSourceFile : THandle;
                              hDestinationFile : THandle;
                              lpData : DWORD) : DWORD; far; stdcall;
var
  RatioProgress : Int64;

begin
  Case dwCallBackReason of
    CALLBACK_STREAM_SWITCH :
      begin
        //this value is passed whenever the
        //callback is initialized for each file.
//        with afrmCopyFile do begin
          gpbar_test.Position := 0;
          gpbar_test.Min := 0;
          gpbar_test.Max := 100;
          gpbar_test.Refresh;
//        end; // with
        CopyProgressCallback := PROGRESS_CONTINUE;
       end;

    CALLBACK_CHUNK_FINISHED :
      begin
        //called when a block has been copied
        RatioProgress :=  TotalBytesTransferred.QuadPart * 100 div TotalFileSize.QuadPart;
        gpbar_test.Position := RatioProgress;

        //optional. While the app is copying it
        //will not respond to input for canceling.
        CopyProgressCallback := PROGRESS_CONTINUE;
      end;
  end; // case
end; // function CopyProgressCallBack

procedure TfrmCopyFile.FormCreate(Sender: TObject);
begin
  Caption := Caption + ', ' + VERSION;
  memoResults.Lines.Add(Caption);
  fSourceDrive := driveComboSource.Drive + ':\';
  fTargetDrive := driveComboTarget.Drive + ':\';
  cboSize.ItemIndex := 12;
  cboIterations.ItemIndex := ITERATION_COUNT - 1;
  fIterations := ITERATION_COUNT;
  StopWatch := TStopWatch.Create;
  StopWatch.Resolution := swresHigh;
  driveComboSource.Drive := 'C';
  driveComboTarget.Drive := 'C';
  fFileSizeMB := StrToInt( cboSize.Text );   // file size in MB
  fFileSize := fFileSizeMB * ONE_MEGABYTE;   // file size in bytes
  fStatusBar := StatusBar1.Panels.Items[0];
  fStatusBar.Text := 'Status: Waiting to obtain system info...';
  OutputSystemInfo;
  //timer1.Enabled := TRUE;
end;

procedure TfrmCopyFile.OutputSystemInfo;
begin
  if not assigned( fSystemInfo ) then begin
    fSystemInfo := TSystemInfo.Create( self );
    try
      fSystemInfo.StopWatch := StopWatch;
      fSystemInfo.ShowStatus := TRUE;
      memoResults.Lines.Add( fSystemInfo.GetSystemSummary )
    finally
      freeAndNil( fSystemInfo );
    end; // try..finally
  end else begin
    memoResults.Lines.Add( fSystemInfo.GetSystemSummary )
  end; // if
  memoResults.Lines.Add('');
  memoResults.Lines.Add('>>>>> Note: The default test can take up to an hour to run on slow drives.');
  memoResults.Lines.Add('!!!!! WARNING: Running thousands of iterations on flash memory can significantly shorten device life!!!!!');
  memoResults.Lines.Add('');
  fStatusBar.Text := 'Status: Ready';
end; // procedure TfrmCopyFile.OutputSystemInfo;

procedure TfrmCopyFile.CreateSource(as_filename: string;
                                   ai_filesize : integer;
                                    var ae_time : extended;
                                    var ad_rate : double
                                   );
var
  ltxtfile_out  : TextFile;
  ls_OutLine    : array[ 0..99 ] of string; // 100 strings
  n, i, j       : integer;
  li_randomChar : integer;
  li_iterate    : integer;
  li_blockSize  : integer;
begin
  if (as_filename = '') then begin
    as_filename := fSourceDrive + DEFAULT_SOURCE_FILENAME;
  end; //if
  AssignFile( ltxtfile_out, as_filename );
  Rewrite( ltxtfile_out );
  li_blockSize := ONE_KILOBYTE;
  li_iterate := round( fFileSize / li_blockSize );
  // build 100 random strings, each element of size li_blockSize:
  for j := 0 to 99 do begin
    // initialize string:
    ls_OutLine[j] := '';
    // build output string of size li_blockSize:
    for i := 1 to li_blockSize do begin
      li_randomChar := Random(94) + 32;
      ls_OutLine[j] := ls_OutLine[j] + chr(li_randomChar);
    end; // for i
  end; // for
  pbar_test.Max := li_iterate;
  StopWatch.StartTimer;
  for n := 1 to li_iterate do begin
    // write random 1kB random character string:
    i := Random( 100 );
    Write( ltxtfile_out, ls_OutLine[ i ] );
    // update progessbar:
    if n mod 32 = 0 then begin
      pbar_test.Position := n;
      application.ProcessMessages;
    end; // if n
  end;  // for n
  CloseFile( ltxtfile_out );
  ae_time := StopWatch.StopTimer;
  ad_rate := ai_filesize / ae_time;
end;  // procedure TfrmCopyFile.CreateSource

procedure TfrmCopyFile.driveComboSourceChange(Sender: TObject);
begin
  fSourceDrive := driveComboSource.Drive + ':\';
end;

procedure TfrmCopyFile.driveComboTargetChange(Sender: TObject);
begin
  fTargetDrive := driveComboTarget.Drive + ':\';
end;

procedure TfrmCopyFile.cboSizeChange(Sender: TObject);
var
  li64_size : int64;
begin
  li64_size := StrToInt64( cboSize.Text );
  fFileSizeMB := li64_size;
  fFileSize := li64_size * ONE_MEGABYTE;   // file size in MB
  case li64_size of
  1 : showmessage( 'To obtain scores you must specify 1,000 iterations for a file size of 1 MB.' );
  10 : showmessage( 'To obtain scores you must specify 100 iterations for a file size of 10 MB.' );
  100 : showmessage( 'To obtain scores you must specify 25 iterations for a file size of 100 MB.' );
  1000 : showmessage( 'To obtain scores you must specify 6 iterations for a file size of 1,000 MB.' );
  end; // case
end;

procedure TfrmCopyFile.btnRunClick(Sender: TObject);
begin
  RunTests;
end; // procedure TfrmCopyFile.btnRunClick(Sender: TObject);

procedure TfrmCopyFile.RunTests;
var
  i : integer;
  ls_line : string;
  ls_sourceFile : string;
  ls_outputFile : string;
  le_time : extended;
  ld_rate : double;
  ld_copyAvgScore : double;
  ld_createAvgScore : double;
  le_AmtFree : extended;
  le_TotalSpace : extended;
  li_DriveNbr : integer;
  procedure InitializeVariables;
  begin
    fCreateTimeMin := 9e99;
    fCreateRateMax := -1;
    fCreateRateAvg := 0;
    fCopyTimeMin   := 9e99;
    fCopyRateMax   := -1;
    fCopyRateAvg   := 0;
    fCreateTimeAvg := 0;
   fCopyTimeAvg := 0;
  end;  // procedure InitializeVariables;
  procedure HandleException(msg : string);
  begin
      ls_line := 'An error occurred: "' + msg + '"';
      ShowMessage( ls_line );
      fStatusBar.Text := ls_line;
      try
        DeleteFile(ls_sourceFile);
        DeleteFile(ls_outputFile);
      except
      end; // try..except
  end;
  function DriveNbr( achr_DriveLetter : char ): integer;
  var
    ls_drive : string;
  begin
    ls_drive := UpperCase( achr_DriveLetter );
    achr_DriveLetter := ls_drive[1];
    result := ord( achr_DriveLetter ) - 64;
  end; //function DriveNbr
  function EnoughDriveSpace(achr_Drive : char; abol_Single: Boolean): Boolean;
  var
    le_SpaceNeeded : extended;
  begin
    result := TRUE;
    li_DriveNbr := DriveNbr( achr_Drive );
    le_AmtFree := DiskFree( li_DriveNbr );
    le_TotalSpace := DiskSize( li_DriveNbr );
    if abol_Single then begin
      le_SpaceNeeded := 2*fFileSize
    end else begin
      le_SpaceNeeded := fFileSize
    end;
    ls_line := Format('Drive "%s": %14.0n free bytes; %14.0n total bytes.',
                      [achr_Drive, le_AmtFree, le_TotalSpace]);
    memoResults.Lines.Add(ls_line);
    ls_line := Format('Space needed: %11.0n bytes; iterations: %d; file size: %d',
               [le_SpaceNeeded, fIterations, fFileSizeMB]);
    memoResults.Lines.Add(ls_line);
    if (le_SpaceNeeded > le_AmtFree) then begin
      ls_line := Format('You do not have enough free space on drive "%s" to run this test.',
        [achr_Drive]);
      ShowMessage( ls_line );
      memoResults.Lines.Add(ls_line);
      memoResults.Lines.Add('');
      result := FALSE;
    end; // if
  end; // procedure CheckDriveSpace(achr_Drive : char)
  procedure OutputTimesAndRates;
  begin
    ls_line := '************************';
    memoResults.Lines.Add(ls_line);
    ls_line := 'Minimum creation time (s), ' + FloatToStrF(fCreateTimeMin, ffFixed, 10, 4);
    memoResults.Lines.Add(ls_line);
    ls_line := 'Maximum file creation rate (MB/s), ' +
      FloatToStrF(fCreateRateMax / ONE_MEGABYTE, ffFixed, 10, 3);
    memoResults.Lines.Add(ls_line);
    ls_line := 'Average creation time (s), ' + FloatToStrF(fCreateTimeAvg, ffFixed, 10, 4);
    memoResults.Lines.Add(ls_line);
    ls_line := 'Average file creation rate (MB/s), ' +
      FloatToStrF( fCreateRateAvg / ONE_MEGABYTE, ffFixed, 10, 3 );
    memoResults.Lines.Add(ls_line);
    ls_line := 'Minimum copy time (s), ' + FloatToStrF( fCopyTimeMin, ffFixed, 10, 4 );
    memoResults.Lines.Add(ls_line);
    ls_line := 'Maximum copy rate (MB/s), ' +
      FloatToStrF( fCopyRateMax / ONE_MEGABYTE, ffFixed, 10, 3 );
    memoResults.Lines.Add(ls_line);
    ls_line := 'Average copy time (s), ' + FloatToStrF( fCopyTimeAvg, ffFixed, 10, 4 );
    memoResults.Lines.Add(ls_line);
    ls_line := 'Average copy rate (MB/s), ' +
      FloatToStrF( fCopyRateAvg / ONE_MEGABYTE, ffFixed, 10, 3 );
    memoResults.Lines.Add(ls_line);
  end; // procedure OutputTimesAndRates;
  procedure OutputResults;
  begin
      memoResults.Lines.Add('');
      memoResults.Lines.Add('******** SCORES ********');
      if (fFileSizeMB = 1000) and (fIterations = ITERATION_COUNT) then begin
        ld_createAvgScore := 1000 * fCreateRateAvg / BASELINE_FILE_CREATION_RATE_AVG;
        ls_line := 'File creation score, ' + FloatToStrF( ld_createAvgScore, ffFixed, 10, 0 );
        memoResults.Lines.Add(ls_line);
        ld_copyAvgScore :=  1000 * fCopyRateAvg / BASELINE_FILE_COPY_RATE_AVG;
        ls_line := 'File copy score, ' + FloatToStrF( ld_copyAvgScore, ffFixed, 10, 0 );
        memoResults.Lines.Add(ls_line);
        ls_line := 'COSBI score, ' +
          FloatToStrF( (ld_copyAvgScore + ld_createAvgScore) / 2, ffFixed, 10, 0 );
        memoResults.Lines.Add(ls_line);
      end else if (fFileSizeMB = 1) and (fIterations = 1000) then begin
        ld_createAvgScore := 1000 * fCreateRateAvg / BASELINE_FILE_CREATION_RATE_AVG_1;
        ls_line := 'File creation score, ' + FloatToStrF( ld_createAvgScore, ffFixed, 10, 0 );
        memoResults.Lines.Add(ls_line);
        ld_copyAvgScore :=  1000 * fCopyRateAvg / BASELINE_FILE_COPY_RATE_AVG_1;
        ls_line := 'File copy score, ' + FloatToStrF( ld_copyAvgScore, ffFixed, 10, 0 );
        memoResults.Lines.Add(ls_line);
        ls_line := 'COSBI score, ' +
          FloatToStrF( (ld_copyAvgScore + ld_createAvgScore) / 2, ffFixed, 10, 0 );
        memoResults.Lines.Add(ls_line);
        ls_line := 'NOTE: The smaller the file size, the less drive specific the result.';
        memoResults.Lines.Add(ls_line);
      end else if (fFileSizeMB = 10) and (fIterations = 100) then begin
        ld_createAvgScore := 1000 * fCreateRateAvg / BASELINE_FILE_CREATION_RATE_AVG_10;
        ls_line := 'File creation score, ' + FloatToStrF( ld_createAvgScore, ffFixed, 10, 0 );
        memoResults.Lines.Add(ls_line);
        ld_copyAvgScore :=  1000 * fCopyRateAvg / BASELINE_FILE_COPY_RATE_AVG_10;
        ls_line := 'File copy score, ' + FloatToStrF( ld_copyAvgScore, ffFixed, 10, 0 );
        memoResults.Lines.Add(ls_line);
        ls_line := 'COSBI score, ' +
          FloatToStrF( (ld_copyAvgScore + ld_createAvgScore) / 2, ffFixed, 10, 0 );
        memoResults.Lines.Add(ls_line);
        ls_line := 'NOTE: The smaller the file size, the less drive specific the result.';
        memoResults.Lines.Add(ls_line);
      end else if (fFileSizeMB = 100) and (fIterations = 25) then begin
        ld_createAvgScore := 1000 * fCreateRateAvg / BASELINE_FILE_CREATION_RATE_AVG_100;
        ls_line := 'File creation score, ' + FloatToStrF( ld_createAvgScore, ffFixed, 10, 0 );
        memoResults.Lines.Add(ls_line);
        ld_copyAvgScore :=  1000 * fCopyRateAvg / BASELINE_FILE_COPY_RATE_AVG_100;
        ls_line := 'File copy score, ' + FloatToStrF( ld_copyAvgScore, ffFixed, 10, 0 );
        memoResults.Lines.Add(ls_line);
        ls_line := 'COSBI score, ' +
          FloatToStrF( (ld_copyAvgScore + ld_createAvgScore) / 2, ffFixed, 10, 0 );
        memoResults.Lines.Add(ls_line);
        ls_line := 'NOTE: The smaller the file size, the less drive specific the result.';
        memoResults.Lines.Add(ls_line);
      end else begin
        ls_line := 'Scores are only reported for "Size (MB)" = 1000 and Iterations = 6, or ' +
                   '"Size (MB)" = 1 and Iterations = 1000, or "Size (MB)" = 10 and Iterations = 100, or ' +
                   '"Size (MB)" = 100 and Iterations = 25.';
        memoResults.Lines.Add(ls_line);
      end; //if
      ls_line := '************************';
      memoResults.Lines.Add(ls_line);
      memoResults.Lines.Add('');
  end; // procedure OutputResults;
  procedure CreateSourceFile;
  begin
    ls_sourceFile := fSourceDrive + DEFAULT_SOURCE_FILENAME;
    if cboxDetails.Checked then begin
      ls_line := Format('Creating file %s of size %d bytes. --> Iteration %d of %d',
                        [ls_sourceFile, fFileSize, i, fIterations]);
      memoResults.Lines.Add(ls_line);
    end; //if
    Application.ProcessMessages;
    CreateSource( ls_sourceFile, fFileSize, le_time, ld_rate );
    if cboxDetails.Checked then begin
      ls_line := Format('Creation time for iteration %d = %n s', [i, le_time]);
      memoResults.Lines.Add(ls_line);
      ls_line := Format('File creation rate: %n MB/s', [ld_rate / ONE_MEGABYTE]);
      memoResults.Lines.Add(ls_line);
    end; //if
    if le_time < fCreateTimeMin then begin
      fCreateTimeMin := le_time;
      fCreateRateMax := ld_rate;
    end; // if
    fCreateTimeAvg := fCreateTimeAvg + le_time / fIterations;
    fCreateRateAvg := fCreateRateAvg + ld_rate / fIterations;
  end; // procedure CreateSourceFile;
  procedure CopyFileTest;
  begin
    ls_outputFile := fTargetDrive + DEFAULT_OUTPUT_FILENAME;
    if cboxDetails.Checked then begin
      ls_line := Format('Copying file %s to file %s.',
                        [ls_sourceFile, ls_outputFile]);
      memoResults.Lines.Add(ls_line);
      Application.ProcessMessages;
    end; //if
    CopyFileToTarget( ls_sourceFile, ls_outputFile, le_time, ld_rate );
    if cboxDetails.Checked then begin
      ls_line := Format('Copy time for iteration %d = %n s', [i, le_time]);
      memoResults.Lines.Add(ls_line);
      ls_line := Format('Copy rate: %n MB/s', [ld_rate / ONE_MEGABYTE]);
      memoResults.Lines.Add(ls_line);
    end; //if
    if le_time < fCopyTimeMin then begin
      fCopyTimeMin := le_time;
      fCopyRateMax := ld_rate;
    end; // if
    fCopyTimeAvg := fCopyTimeAvg + le_time / fIterations;
    fCopyRateAvg := fCopyRateAvg + ld_rate / fIterations;
  end; // procedure CopyFileTest;
begin
  InitializeVariables;
  fStatusBar.Text := 'Testing. Please wait...';
  pbar_overall.Max := fIterations * 2;
  pbar_overall.Position := 0;
  // check drive space:
  if driveComboSource.Drive <> driveComboTarget.Drive then begin
    if not EnoughDriveSpace( driveComboSource.Drive, FALSE ) then exit;
    if not EnoughDriveSpace( driveComboTarget.Drive, FALSE ) then exit;
  end else begin
    if not EnoughDriveSpace( driveComboSource.Drive, TRUE ) then exit;
  end; // if driveComboSource.Drive
  // run tests:
  try
    EnableDisableControls( FALSE );
    try
      // loop through specified number of iterations:
      for i := 1 to fIterations do begin
        // create source file:
        CreateSourceFile;
        pbar_overall.Position := pbar_overall.Position + 1;
        Application.ProcessMessages;
        // copy file:
        CopyFileTest;
        pbar_overall.Position := pbar_overall.Position + 1;
        Application.ProcessMessages;
        // delete files:
        DeleteFile(ls_sourceFile);
        DeleteFile(ls_outputFile);
      end;  // do
      // output results:
      OutputTimesAndRates;
      OutputResults;
      fStatusBar.Text := 'Ready';
    except
      on E: Exception do HandleException( E.Message );
    end; // try...except
  finally
    EnableDisableControls( TRUE );
  end; // try..finally
end; // procedure TfrmCopyFile.RunTests;

procedure TfrmCopyFile.CopyFileToTarget( as_sourceFile: string;
                                        as_targetFile: string;
                                        var ae_time : extended;
                                        var ad_rate : double
                                        );
var
  lfs_SourceFile  : TFileStream;
  ld_Size         : double;
  lbool_CanBeCancelled  : LongBool;
begin
  lbool_CanBeCancelled := FALSE;
  lfs_SourceFile := TFileStream.Create( as_sourceFile, fmOpenRead );
  try
    ld_Size := lfs_SourceFile.Size;
  finally
    freeAndNil(lfs_SourceFile);
  end;
  Application.ProcessMessages;
  StopWatch.StartTimer;
  // detect the OS Platform, use CopyFileEx if WINNT platform detected,
  // or CopyFile if something else
  if Win32Platform = VER_PLATFORM_WIN32_NT then begin
    gpbar_test := self.pbar_test;
    CopyFileEx(PChar(as_sourceFile),
               PChar(as_targetFile),
               @CopyProgressCallBack,
               NIL,
               @lbool_CanBeCancelled,
               COPY_FILE_RESTARTABLE
               );
    gpbar_test := nil;
  end else begin
    pbar_test.Max := 10;
    pbar_test.Position := 1;
    CopyFile(PChar(as_sourceFile), PChar(as_targetFile), FALSE);
  end; // if Win32Platform =
  ae_time := StopWatch.StopTimer;
  ad_rate := ld_Size / ae_time;
  pbar_test.Position := pbar_test.Max;
  Application.ProcessMessages;
end; // procedure TfrmCopyFile.CopyFileToTarget

procedure TfrmCopyFile.btnScoringClick(Sender: TObject);
begin
  ShowMessage( 'Scores are normalized against a Dell Dimension 340, a 2.53GHz Intel Pentium 4' + CR_LF +
               'system which uses a 20GB, 7200 rpm, ATA100 Seagate Barracuda drive.' + CR_LF +
               'The Dell system will average scores of 1000 on all tests.' + CR_LF + CR_LF +
               'COSBI score = (copy score + create score) / 2' + CR_LF + CR_LF +
               'Scores will only be generated for:' + CR_LF +
               '  file size = 1,000 MB and iterations = 6 (default - RECOMMENDED), or' + CR_LF +
               '  file size = 100 MB and iterations = 25, or' + CR_LF +
               '  file size = 10 MB and iterations = 100, or' + CR_LF +
               '  file size = 1 MB and iterations = 1,000.' + CR_LF + CR_LF +
               'While file creation results apply specifically to the source drive,' + CR_LF +
               'file copy results are impacted by both the source and target drives.' + CR_LF + CR_LF +
               'NOTE: For small file sizes, results become more influenced by memory' + CR_LF +
               'and buffer bandwidth.' );
end;

procedure TfrmCopyFile.btnSaveClick(Sender: TObject);
var
  ltxtfile_out  : TextFile;
  ls_FileName   : string;
const
  FILE_SUFFIX = '.txt';
begin
  SaveDialog1.FileName := DEFAULT_FILENAME;
  ls_FileName := DEFAULT_FILENAME;
  if SaveDialog1.Execute then begin
    ls_FileName := SaveDialog1.FileName;
    if (ls_FileName = '') then begin
      ls_FileName := DEFAULT_FILENAME;
    end; // if
    if Pos('.', ls_FileName) = 0 then begin
      ls_FileName := ls_FileName + FILE_SUFFIX;
    end; // if
    AssignFile( ltxtfile_out, ls_FileName );
    Rewrite( ltxtfile_out );
    Write( ltxtfile_out, memoResults.Text);
    CloseFile( ltxtfile_out );
  end; // if
end;

procedure TfrmCopyFile.cboIterationsChange(Sender: TObject);
begin
  fIterations := StrToInt(cboIterations.Text);
end; // procedure TfrmCopyFile.cboIterationsChange

procedure TfrmCopyFile.EnableDisableControls(abol_Enable : Boolean);
begin
  btnRun.Enabled            := abol_Enable;
  cboSize.Enabled           := abol_Enable;
  cboxDetails.Enabled       := abol_Enable;
  btnScoring.Enabled        := abol_Enable;
  btnSave.Enabled           := abol_Enable;
  driveComboSource.Enabled  := abol_Enable;
  driveComboTarget.Enabled  := abol_Enable;
  cboIterations.Enabled     := abol_Enable;
end; // procedure TfrmCopyFile.DisableControls;

procedure TfrmCopyFile.Timer1Timer(Sender: TObject);
begin
  //OutputSystemInfo;
  Timer1.Enabled := FALSE;
end;

procedure TfrmCopyFile.FormActivate(Sender: TObject);
begin
  UnFadeFast( self );
end;

procedure TfrmCopyFile.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FadeFast100( self );
end;

procedure TfrmCopyFile.FormDeactivate(Sender: TObject);
begin
  FadeFast50( self );
end;

end.
