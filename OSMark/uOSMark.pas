unit uOSMark;
{
  COSBI: Comprehensive Open Source Benchmarking Initiative
  Copyright (c) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007 Van Smith

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
// Unit name: uOSMark
// Unit description: This unit is the form unit for OSMark.
// Author: Van Smith
// Date: March 24, 2003
// OS dependent: Yes: Windows
// Resolution dependent: No, but resolution and color depth will impact scores.
// External unit dependencies: COSBI_Common, uCOSBI_TestBaseClass, GR32_Blend,
//   GR32_Image, GR32, Maze
//==============================================================================
// Modification History
// ------------ -------
// Ver  Date   Who    Description
// ==== ====== ====== ==========================================================
// 1.0 020503 Van     Created.
// 1.1 070214 Van     ZipForge v2.77
// 1.1 070908 Van     ZipForge v3.03
//==============================================================================

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, COSBI_Common, StdCtrls, uCOSBI_SystemInfo, uTests, uOutput,
  GR32, Dhrystone, Whetstone, uOSMarkSuite, uCOSBI_TTest, uStopWatch, ExtCtrls,
  ComCtrls, MPlayer, Buttons, jpeg, Gauges,
  ZipForge, uOSMarkResults, ufrmCPUInfo, FileCtrl,
  IniFiles, frmBandwidthBurn, frmMemLatencyPlus, uFileCopy,
  ufrmWhetBurn, uMobileTestSelect, uAbout, CosbiCpuid, ufrmOSMarkOptions,
  uOSMarkSelectTests, ufrmSleeping, CategoryButtons, ButtonGroup, Tabs, uOSMarkINI,
  uMobileTests, uThinClientTests, uMSR,
  WbemScripting_TLB, ActiveDs_TLB, ActiveX;

type

  TfrmOSMark = class(TForm)
    memoResults: TMemo;
    pBar: TGauge;
    SaveDialog1: TSaveDialog;
    Image1: TImage;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    MediaPlayerAbout: TMediaPlayer;
    SpeedButtonOfficalRun: TSpeedButton;
    SpeedButtonThinClientRun: TSpeedButton;
    SpeedButtonMobileRun: TSpeedButton;
    SpeedButtonStabilityTest: TSpeedButton;
    SpeedButtonSave: TSpeedButton;
    SpeedButtonResultViewer: TSpeedButton;
    SpeedButtonQuit: TSpeedButton;
    SpeedButtonAbout: TSpeedButton;
    SpeedButtonCPU: TSpeedButton;
    SpeedButtonHelp: TSpeedButton;
    SpeedButtonOptions: TSpeedButton;
    SpeedButtonCustomRun: TSpeedButton;
    SpeedButtonBandwidthBurn: TSpeedButton;
    SpeedButtonMemLantecyPlus: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButtonFileCopy: TSpeedButton;
    SpeedButtonWhetBurn: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButtonCpuUtilization: TSpeedButton;
    Bevel1: TBevel;
    SpeedButton8: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MemoInfo: TMemo;
    Image2: TImage;
    pbarBattery: TProgressBar;
    TimerBatteryLife: TTimer;
    lblBattery: TLabel;
    procedure memoResultsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SpeedButtonHelpClick(Sender: TObject);
    procedure TimerBatteryLifeTimer(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure SpeedButtonPCWB5MobileMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton8MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SpeedButtonSaveMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SpeedButtonFileCopyMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure SpeedButtonMemLantecyPlusMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButtonBandwidthBurnMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButtonWhetBurnMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure SpeedButton3MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SpeedButtonStabilityTestMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButtonAboutMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SpeedButtonQuitMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SpeedButtonHelpMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SpeedButtonResultViewerMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButtonOptionsMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure SpeedButtonThinClientRunMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButtonMobileRunMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure SpeedButtonCustomRunMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure SpeedButtonOfficalRunMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButtonCpuUtilizationMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton6MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SpeedButtonCPUMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SpeedButtonCustomRunClick(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButtonCpuUtilizationClick(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButtonWhetBurnClick(Sender: TObject);
    procedure SpeedButtonMemLantecyPlusClick(Sender: TObject);
    procedure SpeedButtonFileCopyClick(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButtonBandwidthBurnClick(Sender: TObject);
    procedure SpeedButtonOptionsClick(Sender: TObject);
    procedure SpeedButtonCPUClick(Sender: TObject);
    procedure SpeedButtonAboutClick(Sender: TObject);
    procedure SpeedButtonQuitClick(Sender: TObject);
    procedure SpeedButtonResultViewerClick(Sender: TObject);
    procedure SpeedButtonSaveClick(Sender: TObject);
    procedure SpeedButtonStabilityTestClick(Sender: TObject);
    procedure SpeedButtonMobileRunClick(Sender: TObject);
    procedure SpeedButtonThinClientRunClick(Sender: TObject);
    procedure SpeedButtonOfficalRunClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ClearOutput;
    procedure btnSaveClick(Sender: TObject);
    procedure InitializeOSMarkTestSuite;
    procedure UpdateStatusMemo( Sender: TObject );
    procedure OnCriticalBatteryLevel( Sender: TObject);
    procedure OnTestSuiteRunStart( Sender: TObject );
    procedure OnIterationStart( Sender: TObject );
    procedure OnTestBegins( Sender: TObject );
    procedure OnTestEnds( Sender: TObject );
    procedure OnTestSuiteRunEnd( Sender: TObject );
    procedure UpdateStatusMemoMobile( Sender: TObject );
    procedure OnTestSuiteRunStartMobile( Sender: TObject );
    procedure OnIterationStartMobile( Sender: TObject );
    procedure OnTestBeginsMobile( Sender: TObject );
    procedure OnTestEndsMobile( Sender: TObject );
    procedure OnTestSuiteRunEndMobile( Sender: TObject );
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    fbCommandLineRun : Boolean;
    fbOfficialRun : Boolean;
    fFade : Boolean;
    frmBandwidthBurn : TfrmCOSBIBandwidthBurn;
    frmCopyFile : TfrmCopyFile;
    frmMemLatencyPlus : TfrmMemLate;
    frmOutput: TfrmOutput;
    frmWhetBurn :  TfrmWhetBurn;
    fOldBatteryCaption : string;
    fOSMarkINI : TOSMarkINI;
    fOSMarkTestSuite : TOSMarkTestSuite;
    fOSMarkMobileTests : TOSMarkMobileTests;
    fProgramPriority  : TOSM_ProgramPriority;
    fResultFileName : string;
    fStopWatch: TStopWatch;
    fSystemInfo: TSystemInfo;
    fTestStart : TDateTime;
    fTimerResolution  : TOSM_TimerResolution;
    // wmi stuff
    fLocator      : TSWbemLocator;
    fSinkClasses  : TSWbemSink;
    fServices     : ISWbemServices;
    fObjectSet    : ISWbemObjectSet;
    fSObject      : ISWbemObject;
    fPropSet      : ISWbemPropertySet;
    fSProp        : ISWbemProperty;
    fPropEnum     : IEnumVariant;
    fEnum         : IEnumVariant;
    ftempObj      : OleVariant;
    // end wmi
    // msr driver stuff
    fMsrDriver        : TMsrDriver;
    fMsrRegister      : T64bitRegister;
    // end msr driver stuff
    function GetCpuMhz: integer;
    function GetMaxCpuMhz: integer;
    function OpenOSMarkOptions: Boolean;
    procedure CheckBattery;
    procedure ClearStatusBar;
    procedure Defrag;
    procedure InitializeOSMarkTestSuiteCL( abShowStatus, abSpinUp: Boolean );
    procedure LinkEventHandlers;
    procedure LinkMobileEventHandlers;
    procedure OfficialRun;
    procedure OfficalRunGUI;
    procedure OpenBandwidthBurn;
    procedure OpenFileCopy;
    procedure OpenMemLatencyPlus;
    procedure OpenWhetBurn;
    procedure OpenCpuSpeed;
    procedure OpenCpuUtilization;
    procedure ParseCommandLine;
    procedure ProcessIdleTasks;
    procedure RunMobileTests;
    procedure RunSelectedTests( aiInterate : integer; aSelectedTests: TQTestSet );
    procedure RunThreadWrecker;
    procedure SetFade( Value : Boolean );
    procedure SetPriority;
    procedure SetTimerResolution;
    procedure SystemShutDown;
    procedure TestPStates;
    procedure WMEndSession(var Msg: TWMEndSession); message WM_ENDSESSION;
  public
    { Public declarations }
    property Fade : Boolean read fFade write SetFade;
  end;

const
  VERSION = '1.0';
  DEFAULT_FILENAME = 'OSMarkResults.txt';
  PANEL_MAIN = 0;
  PANEL_LAST_RESULT = 1;
  PANEL_ITERATION = 2;
  PANEL_TIME = 3;
  PANEL_LOOP = 4;

var
  frmOSMark: TfrmOSMark;

implementation

{$R *.DFM}

procedure TfrmOSMark.LinkEventHandlers;
begin
  fOSMarkTestSuite.OnOutputString := UpdateStatusMemo;
  fOSMarkTestSuite.OnTestSuiteRunStart := OnTestSuiteRunStart;
  fOSMarkTestSuite.OnIterationStart := OnIterationStart;
  fOSMarkTestSuite.OnTestBegins := OnTestBegins;
  fOSMarkTestSuite.OnTestEnds := OnTestEnds;
  fOSMarkTestSuite.OnTestSuiteRunEnd := OnTestSuiteRunEnd;
end; // procedure TfrmOSMark.LinkEventHandlers;

procedure TfrmOSMark.LinkMobileEventHandlers;
begin
  fOSMarkMobileTests.OnOutputString := UpdateStatusMemoMobile;
  fOSMarkMobileTests.OnTestSuiteRunStart := OnTestSuiteRunStartMobile;
  fOSMarkMobileTests.OnIterationStart := OnIterationStartMobile;
  fOSMarkMobileTests.OnTestBegins := OnTestBeginsMobile;
  fOSMarkMobileTests.OnTestEnds := OnTestEndsMobile;
  fOSMarkMobileTests.OnTestSuiteRunEnd := OnTestSuiteRunEndMobile;
  fOSMarkMobileTests.OnCriticalPowerLevel := OnCriticalBatteryLevel;
end; // procedure TfrmOSMark.LinkMobileEventHandlers;

procedure TfrmOSMark.ProcessIdleTasks;
begin
  StatusBar1.Panels[ PANEL_MAIN ].Text := 'Processing idle tasks...';
  memoResults.Lines.Add( 'Processing idle tasks...' );
  application.ProcessMessages;
  RunProcessIdleTasks;
  StatusBar1.Panels[ PANEL_MAIN ].Text := 'Finished processing idle tasks.';
  memoResults.Lines.Add( 'Finished processing idle tasks.' );
end; // procedure TfrmOSMark.ProcessIdleTasks;

procedure TfrmOSMark.Defrag;
var
  i : integer;
  lcTargetDrive : char;
begin
  lcTargetDrive := fOSMarkINI.TargetDrive;
  StatusBar1.Panels[ PANEL_MAIN ].Text := 'Defragging drive:' + lcTargetDrive;
  memoResults.Lines.Add( 'Defragging drive: ' + lcTargetDrive );
  for i := 1 to 5 do begin
    StatusBar1.Panels[ PANEL_MAIN ].Text := 'Defrag ' + intToStr( i ) + ' of 5';
    application.ProcessMessages;
    RunDiskDefrag( lcTargetDrive, 1 );
  end; // for
  StatusBar1.Panels[ PANEL_MAIN ].Text := 'Finished disk defrag.';
  StatusBar1.Panels[ PANEL_MAIN ].Text := '';
  memoResults.Lines.Add( 'Defrag complete.' );
  application.ProcessMessages;
end; // procedure TfrmOSMark.Defrag;

procedure TfrmOSMark.SetPriority;
var
  H : THandle;
begin
  H := GetCurrentProcess();
  if fProgramPriority = osmppNormal then begin
    SetPriorityClass( H, NORMAL_PRIORITY_CLASS );
  end else if fProgramPriority = osmppHigh then begin
    SetPriorityClass( H, HIGH_PRIORITY_CLASS );
  end else if fProgramPriority = osmppRealTime then begin
    SetPriorityClass( H, REALTIME_PRIORITY_CLASS );
  end else raise exception.Create('Unknown program priority.');
end;

procedure TfrmOSMark.FormCreate(Sender: TObject);
begin
  fFade := TRUE;
  fbOfficialRun := FALSE;
  Left := 0; Top := 0;
  Caption := Caption + ', Version ' + Version;
  SetPriority;
  // msr driver:
  fMsrDriver := TMsrDriver.Create;
  fMsrDriver.OpenDriver;
  // end msr
  fStopWatch := TStopWatch.Create;
  fStopWatch.MSRDriver := fMsrDriver;
  fStopWatch.Resolution := swresHigh;
  fSystemInfo := TSystemInfo.Create( self );
  fSystemInfo.ShowStatus := TRUE;
  fSystemInfo.StopWatch := fStopWatch;
  fSystemInfo.Initialize;
  memoResults.Lines.Add( 'COSBI OSMark Version ' + Version );
  memoResults.Lines.Add( 'Date: ' + DateTimeToStr( Now ) );
  memoResults.Lines.Add( fSystemInfo.GetSystemSummary );
  memoResults.Lines.Add( '' );
  fOSMarkINI := TOSMarkINI.Create;
  try
    fOSMarkINI.ReadIni;
  except
    showmessage( 'I could not read the INI file.' + CRLF
      + 'Don''t run OpenSourceMark from a zipped archive or from a read-only network location.');
  end; // try
  memoResults.Lines.Add( 'Ready.' );
  fbCommandLineRun := FALSE;
  Timer1.Enabled := TRUE;
  try
    MediaPlayerAbout.Enabled := TRUE;
  except
    ShowMessage( 'You do not have audio drivers installed.  Some tests may not function' );
  end;
  fTimerResolution := osmtrHigh;
  fOldBatteryCaption := '';
  CheckBattery;
  //wmi:
  fLocator := TSWbemLocator.Create( self );
  fSinkClasses := TSWbemSink.Create( self );
  fServices := fLocator.ConnectServer('', 'root\CIMV2', '', '', '', '', 0, nil);
  // end wmi
end;

procedure TfrmOSMark.InitializeOSMarkTestSuiteCL( abShowStatus, abSpinUp: Boolean );
begin
  fOSMarkTestSuite.Initialize( fSystemInfo );
  with fOSMarkINI do begin
    fOSMarkTestSuite.ShowStatus       := ShowStatus;
    fOSMarkTestSuite.SpinUp           := SpinUp;
    fOSMarkTestSuite.ProcessIdleTasks := FALSE;
    fOSMarkTestSuite.DiskDefrag       := FALSE;
    fOSMarkTestSuite.TargetDisk       := TargetDrive;
  end;
  LinkEventHandlers;
end; // InitializeOSMarkTestSuiteCL;


procedure TfrmOSMark.ParseCommandLine;
var
  i                   : integer;
  lbOfficialRun       : Boolean;
  lbRun               : Boolean;
  lbDefrag            : Boolean;
  lbProcessIdleTasks  : Boolean;
  lbSave              : Boolean;
  lbShowStatus        : Boolean;
  lbSpinUp            : Boolean;
  lbExit              : Boolean;
  lbAbort             : Boolean;
  liIterations        : integer;
  lsSaveFileName      : string;
  ldTime              : double;
  ldSeconds           : double;

  procedure InitializeLocalVariables;
  begin
    i := 1;
    lbRun  := FALSE;
    lbOfficialRun := FALSE;
    lbSave := FALSE;
    lbDefrag := FALSE;
    lbShowStatus := FALSE;
    lbSpinUp := FALSE;
    lbExit  := FALSE;
    lbProcessIdleTasks := FALSE;
    liIterations := 1;
    lsSaveFileName := DEFAULT_FILENAME;
    lbAbort := FALSE;
  end; // InitializeLocalVariables

  procedure DoParse;
  begin
    // parse commandline:
    while i <= ParamCount do begin
      if LowerCase( ParamStr(i) ) = 'or' then begin
        lbOfficialRun := TRUE;
      end else if LowerCase( ParamStr(i) ) = 'r' then begin
        lbRun := TRUE;
      end else if LowerCase( ParamStr(i) ) = 'i' then begin
        inc( i );
        liIterations := StrToInt( ParamStr( i ) );
      end else if LowerCase( ParamStr(i) ) = 'defrag' then begin
        lbDefrag := TRUE;
      end else if LowerCase( ParamStr(i) ) = 'pit' then begin
        lbProcessIdleTasks := TRUE;
      end else if LowerCase( ParamStr(i) ) = 'fn' then begin
        i := i + 1;
        lbSave := TRUE;
        lsSaveFileName := ParamStr( i );
      end else if LowerCase( ParamStr(i) ) = 'su' then begin
        lbSpinUp := TRUE;
      end else if LowerCase( ParamStr(i) ) = 's' then begin
        lbSave := TRUE;
      end else if LowerCase( ParamStr(i) ) = 'ss' then begin
        lbShowStatus := TRUE;
      end else if LowerCase( ParamStr(i) ) = 'e' then begin
        lbExit := TRUE;
      end else begin
        ShowMessage( 'Command line parameters:' + CR_LF
          + '   or = official run' + CR_LF
          + '   r = run tests specified in ini file (will be ignored if "or" is specified)' + CR_LF
          + '   defrag = defrag disk' + CR_LF
          + '   pit = process idle tasks' + CR_LF
          + '   su = spin up cpu' + CR_LF
          + '   ss = Show status window between tests' + CR_LF
          + '   e = exit after run' + CR_LF
          + '   i nnnn = iterate nnnn times' + CR_LF
          + '   s = save result output to file' + CR_LF
          + '   fn "filename" = output filename' );
        lbAbort := TRUE;
      end; // if
      i := i + 1;
    end; // while
  end; // DoParse

begin
  if ParamCount < 1 then exit;
  fbCommandLineRun := TRUE;
  InitializeLocalVariables;
  DoParse;
  if lbAbort then begin
    application.Terminate;
    application.ProcessMessages;
    exit;
  end; // if
  // process command line selections:
  if lbDefrag then begin
    Defrag;
  end;
  if lbProcessIdleTasks then begin
    ProcessIdleTasks;
  end;
  fTestStart := Now;
  StatusBar1.Panels[ PANEL_TIME ].Text := 'Test starts: ' +
    DateTimeToStr( fTestStart );
  for i := 1 to liIterations do begin
    if lbOfficialRun then begin
      StatusBar1.Panels[ PANEL_LOOP ].Text :=
        'Loop ' + IntToStr( i ) + ' of ' + IntToStr( liIterations );
      fbOfficialRun := TRUE;
      fOSMarkTestSuite := TOSMarkTestSuite.Create( self );
      try
        Fade := FALSE;
        InitializeOSMarkTestSuiteCL( lbShowStatus, lbSpinUp );
        fOSMarkTestSuite.OfficialRun;
      finally
        freeAndNil( fOSMarkTestSuite );
        Fade := TRUE;
      end; // try...finally
      fbOfficialRun := FALSE;
    end else if lbRun then begin
      StatusBar1.Panels[ PANEL_LOOP ].Text :=
        'Loop ' + IntToStr( i ) + ' of ' + IntToStr( liIterations );
      fOSMarkTestSuite := TOSMarkTestSuite.Create( self );
      try
        Fade := FALSE;
        InitializeOSMarkTestSuiteCL( lbShowStatus, lbSpinUp );
        fOSMarkTestSuite.RunTheseTests( fOSMarkINI.SelectedTests, 1);
        Application.ProcessMessages;
      finally
        freeAndNil(   fOSMarkTestSuite );
        Fade := TRUE;
      end; // try...finally
    end; // if lbOfficialRun
  end; // for
  ldTime := ( Now - fTestStart );
  ldSeconds := 24 * 60 * 60 * ldTime;
  StatusBar1.Panels[ PANEL_TIME ].Text := 'Run time (s): ' +
    FloatToStr( ldSeconds );
  application.ProcessMessages;
  if lbSave then begin
    SaveStringListToFile(memoResults.Lines , lsSaveFileName, FALSE);
  end; // if lbSave
  if lbExit then begin
    application.Terminate;
  end else begin
    fbCommandLineRun := FALSE;
  end;
end; // procedure TfrmOSMark.ParseCommandLine;

procedure TfrmOSMark.FormDestroy(Sender: TObject);
begin
  FreeAndNil( fStopWatch );
  FreeAndNil( fMsrDriver );
end;

procedure TfrmOSMark.RunSelectedTests( aiInterate : integer;
                                       aSelectedTests: TQTestSet );
begin
  fOSMarkTestSuite := TOSMarkTestSuite.Create( self );
  try
    InitializeOSMarkTestSuite;
    fOSMarkTestSuite.RunTheseTests(aSelectedTests, aiInterate);
    Application.ProcessMessages;
  finally
    freeAndNil(   fOSMarkTestSuite );
  end; // try...finally
end; // procedure TfrmOSMark.RunSelectedTests

procedure TfrmOSMark.InitializeOSMarkTestSuite;
begin
  fOSMarkTestSuite.Initialize( fSystemInfo );
  with fOSMarkINI do begin
    fOSMarkTestSuite.ShowStatus       := ShowStatus;
    fOSMarkTestSuite.SpinUp           := SpinUp;
    fOSMarkTestSuite.ProcessIdleTasks := ProcessIdleTasks;
    fOSMarkTestSuite.DiskDefrag       := Defrag;
    fOSMarkTestSuite.TargetDisk       := TargetDrive;
    fOSMarkTestSuite.ThreadCount      := ThreadCount;
  end; // with
  LinkEventHandlers;
end; // procedure TfrmOSMark.InitializeOSMarkTestSuite;

procedure TfrmOSMark.ClearOutput;
begin
  frmOutput.PaintBoxOutput.Buffer.Clear(clBlack32);
  frmOutput.PaintBoxOutput.Invalidate;
  Application.ProcessMessages;
end;

procedure TfrmOSMark.SetTimerResolution;
begin
  if fTimerResolution = osmtrLow  then begin
    fStopWatch.Resolution := swresLow;
  end else if fTimerResolution = osmtrHigh then begin
    fStopWatch.Resolution := swresHigh;
  end else if fTimerResolution = osmtrCPU then begin
    fStopWatch.Resolution := swresCPU;
  end; // if radioLow.Checked
end;

procedure TfrmOSMark.btnSaveClick(Sender: TObject);
begin

end; // procedure TfrmOSMark.btnSaveClick(Sender: TObject);

procedure TfrmOSMark.OfficialRun;
var
  lfrmOSMResults : TfrmOSMResults;
begin
  fbOfficialRun := TRUE;
  fOSMarkTestSuite := TOSMarkTestSuite.Create( self );
  try
    Fade := FALSE;
    InitializeOSMarkTestSuite;
    fOSMarkTestSuite.OfficialRun;
    lfrmOSMResults := TfrmOSMResults.Create(self);
    try
      lfrmOSMResults.OSMarkTestResults := fOSMarkTestSuite.OSMarkTestResults;
      lfrmOSMResults.ShowModal;
    finally
      lfrmOSMResults.Release;
      FreeAndNil( lfrmOSMResults );
    end;
  finally
    freeAndNil( fOSMarkTestSuite );
    Fade := TRUE;
  end; // try...finally
  fbOfficialRun := FALSE;
end; // procedure TfrmOSMark.OfficialRun;

procedure TfrmOSMark.UpdateStatusMemo( Sender: TObject );
begin
  memoResults.Lines.Add( fOSMarkTestSuite.OutputString );
  application.processmessages;
end; // procedure UpdateStatusMemo( asOutputLine : string );

procedure TfrmOSMark.OnTestSuiteRunStart( Sender: TObject );
begin
  pBar.Progress := 0;
  pBar.MaxValue := fOSMarkTestSuite.TotalNumberOfTestsToRun;
  ClearStatusBar;
  if not fbCommandLineRun then begin
    fTestStart := Now;
    StatusBar1.Panels[ PANEL_TIME ].Text := 'Test starts: ' +
      DateTimeToStr( fTestStart );
  end; // if
end; // procedure TfrmOSMark.OnTestSuiteRunStart

procedure TfrmOSMark.OnIterationStart( Sender: TObject );
begin
  StatusBar1.Panels[ PANEL_ITERATION ].Text := 'Iteration ' +
    intToStr( fOSMarkTestSuite.Iteration ) + ' of ' +
    intToStr( fOSMarkTestSuite.IterationFinal );
  application.processmessages;
end; //procedure OnIterationStart( Sender: TObject );

procedure TfrmOSMark.OnTestBegins( Sender: TObject );
begin
  StatusBar1.Panels[ PANEL_MAIN ].text := 'Running ' +
    fOSMarkTestSuite.TestName + '...';
  application.ProcessMessages;
end;

procedure TfrmOSMark.OnTestEnds( Sender: TObject );
begin
  pBar.Progress := pBar.Progress + 1;
  StatusBar1.Panels[ PANEL_MAIN ].text := '';
  StatusBar1.Panels[ PANEL_LAST_RESULT ].text := fOSMarkTestSuite.TestName +
    ' score = ' + IntToStr( fOSMarkTestSuite.TestScore );
  application.ProcessMessages;
end; // procedure OnTestEnds( Sender: TObject );

procedure TfrmOSMark.OnTestSuiteRunEnd( Sender: TObject );
var
  ldTime : double;
  ldSeconds : double;
begin
  pBar.Progress := pBar.MaxValue;
  StatusBar1.Panels[ PANEL_MAIN ].text := 'Test Complete. Ready...';
  if not fbCommandLineRun then begin
    ldTime := ( Now - fTestStart );
    ldSeconds := 24 * 60 * 60 * ldTime;
    StatusBar1.Panels[ PANEL_TIME ].Text := 'Run time (s): ' +
      FloatToStr( ldSeconds );
    application.ProcessMessages;
  end; // if
end; // procedure OnTestEnds( Sender: TObject );

procedure TfrmOSMark.ClearStatusBar;
begin
  StatusBar1.Panels[ PANEL_MAIN ].Text := '';
  StatusBar1.Panels[ PANEL_LAST_RESULT ].Text := '';
  StatusBar1.Panels[ PANEL_ITERATION ].Text := '';
  if not fbCommandLineRun then StatusBar1.Panels[ PANEL_TIME ].Text := '';
end; //procedure TfrmOSMark.ClearStatusBar;

procedure TfrmOSMark.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    fOSMarkINI.WriteIniFile;
  except
    showmessage( 'I could not write out the INI file.' + CRLF
     + 'Don''t run OpenSourceMark from a zipped archive or from a read-only network location.');
  end; // try
  freeAndNil( fOSMarkINI );
end;

procedure TfrmOSMark.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := FALSE;
  application.ProcessMessages;
  ParseCommandLine;
end;

procedure TfrmOSMark.OpenBandwidthBurn;
var
  SaveCursor : TCursor;
begin
  SpeedButtonBandwidthBurn.Enabled := FALSE;
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if assigned( frmBandwidthBurn ) then begin
      try
        frmBandwidthBurn.Show;
        exit;
      except
        frmBandwidthBurn := NIL;
      end; // try
    end;
    try
      if ( not assigned( frmBandwidthBurn ) ) or
         ( not frmBandwidthBurn.Visible ) then begin
        frmBandwidthBurn := TfrmCOSBIBandwidthBurn.Create( Self );
        frmBandwidthBurn.FreeOnClose := FALSE;
        frmBandwidthBurn.SystemInfo := fSystemInfo;
      end;
    finally
      frmBandwidthBurn.Show;
    end; //try...finally
  finally
    Screen.Cursor := SaveCursor;
    SpeedButtonBandwidthBurn.Enabled := TRUE;
  end; // try..finally
end; // procedure TfrmOSMark.OpenBandwidthBurn;

procedure TfrmOSMark.OpenMemLatencyPlus;
var
  SaveCursor : TCursor;
begin
  SpeedButtonMemLantecyPlus.Enabled := FALSE;
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if ( not assigned( frmMemLatencyPlus ) ) or
       ( not frmMemLatencyPlus.Visible ) then begin
      if assigned( frmMemLatencyPlus ) then begin
        try
          FreeAndNil( frmMemLatencyPlus );
        except
          frmMemLatencyPlus := NIL;
        end; // try
      end; // if
      frmMemLatencyPlus := TfrmMemLate.Create( self );
      frmMemLatencyPlus.SystemInfo := fSystemInfo;
    end;
  finally
    frmMemLatencyPlus.Show;
    Screen.Cursor := SaveCursor;
    SpeedButtonMemLantecyPlus.Enabled := TRUE;
  end; //try...finally
end; // procedure TfrmOSMark.OpenMemLatencyPlus;

procedure TfrmOSMark.OpenFileCopy;
var
  SaveCursor : TCursor;
begin
  SpeedButtonFileCopy.Enabled := FALSE;
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if ( not assigned( frmCopyFile ) ) or
       ( not frmCopyFile.Visible ) then begin
      if assigned( frmCopyFile ) then begin
        try
          FreeAndNil( frmCopyFile );
        except
          frmCopyFile := NIL;
        end; // try
      end; // if
      frmCopyFile := TfrmCopyFile.Create( self );
      frmCopyFile.SystemInfo := fSystemInfo;
    end;
  finally
    frmCopyFile.Show;
    Screen.Cursor := SaveCursor;
    SpeedButtonFileCopy.Enabled := TRUE;
  end; //try...finally
end; //procedure TfrmOSMark.OpenFileCopy;

procedure TfrmOSMark.OpenWhetBurn;
var
  SaveCursor : TCursor;
begin
  SpeedButtonWhetBurn.Enabled := FALSE;
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if ( not assigned( frmWhetBurn ) ) or
       ( not frmWhetBurn.Visible ) then begin
      if assigned( frmWhetBurn ) then begin
        try
          FreeAndNil( frmWhetBurn );
        except
          frmWhetBurn := NIL;
        end; // try
      end; // if
      frmWhetBurn := TfrmWhetBurn.Create( self );
    end;
  finally
    frmWhetBurn.Show;
    Screen.Cursor := SaveCursor;
    SpeedButtonWhetBurn.Enabled := TRUE;
  end; //try...finally
end; //procedure TfrmOSMark.OpenWhetBurnClick;

procedure TfrmOSMark.OpenCpuSpeed;
var
  SaveCursor : TCursor;
  lstrAppPath : string;
const
  CPU_SPEED_FILE = 'osmfiles\CPUSpeed.exe';
begin
  SpeedButtonCpuUtilization.Enabled := FALSE;
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    lstrAppPath := ExtractFilePath(Application.ExeName);
    StartProgram( lstrAppPath + CPU_SPEED_FILE );
  finally
    Screen.Cursor := SaveCursor;
    SpeedButtonCpuUtilization.Enabled := TRUE;
  end; //try...finally
end;

procedure TfrmOSMark.OpenCpuUtilization;
var
  SaveCursor : TCursor;
  lstrAppPath : string;
const
  CPU_UTILIZATION_FILE_COMMAND = 'osmfiles\CosbiCpuUtilization.exe';
begin
  SpeedButtonCpuUtilization.Enabled := FALSE;
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    lstrAppPath := ExtractFilePath(Application.ExeName);
    StartProgram( lstrAppPath + CPU_UTILIZATION_FILE_COMMAND );
  finally
    Screen.Cursor := SaveCursor;
    SpeedButtonCpuUtilization.Enabled := TRUE;
  end; //try...finally
end;

procedure TfrmOSMark.RunThreadWrecker;
var
  lTest : TLaunchProgramThreadTest;
  lsAppPath : string;
  lsLorenzFilePath : string;
  lsNBodyFilePath : string;
  lsAesFilePath : string;
  lsGogoFilePath : string;
  liScore : integer;
begin
  memoResults.Lines.Add( 'Running "ThreadWrecker".' );
  lTest := TLaunchProgramThreadTest.Create;
  try
    lTest.StopWatch := fStopWatch;
    lsAppPath := ExtractFilePath( Application.ExeName );
    lsLorenzFilePath := lsAppPath + 'osmfiles\projectionLorenz.exe';
    lsAesFilePath := lsAppPath + 'osmfiles\AESBench.exe';
    lsGogoFilePath := lsAppPath + 'osmfiles\gogo.exe -test 600';
    lsNBodyFilePath := lsAppPath + 'osmfiles\nBody.exe';
    memoResults.Lines.Add( 'Spawning ' + lsGogoFilePath );
    lTest.ProgramCommand1 := lsGogoFilePath;
    memoResults.Lines.Add( 'Spawning ' + lsAesFilePath );
    lTest.ProgramCommand2 := lsAesFilePath;
    memoResults.Lines.Add( 'Spawning ' + lsLorenzFilePath );
    lTest.ProgramCommand3 := lsLorenzFilePath;
    memoResults.Lines.Add( 'Spawning ' + lsNBodyFilePath );
    lTest.ProgramCommand4 := lsNBodyFilePath;
    memoResults.Lines.Add( 'Spawning ' + lsGogoFilePath );
    lTest.ProgramCommand5 := lsGogoFilePath;
    memoResults.Lines.Add( 'Spawning ' + lsAesFilePath );
    lTest.ProgramCommand6 := lsAesFilePath;
    memoResults.Lines.Add( 'Spawning ' + lsGogoFilePath );
    lTest.ProgramCommand7 := lsGogoFilePath;
    memoResults.Lines.Add( 'Spawning ' + lsAesFilePath );
    lTest.ProgramCommand8 := lsAesFilePath;
    lTest.ThreadCount := tc8;
    lTest.Run;
    liScore := lTest.GetScore(0);
    memoResults.Lines.Add( 'ThreadWrecker score' + TAB + intToStr( liScore ) );
    memoResults.Lines.Add( 'ThreadWrecker time (s)' + TAB + lTest.GetFormattedMinTime );
  finally
    freeAndNil( lTest );
  end; // try...finally
end;

procedure TfrmOSMark.RunMobileTests;
var
  lfrmMobileTest   : TfrmMobileTest;
  lMobileTestType  : TMobileTestType;
  liPCWB5Iterations : integer;
begin
  fResultFileName := '';
  lfrmMobileTest := TfrmMobileTest.Create(Self);
  try
    fadefast50( self );
    if lfrmMobileTest.ShowModal = mrCancel then begin
      exit;
    end;
    lMobileTestType := lfrmMobileTest.MobileTestType;
    if lfrmMobileTest.ResultsFileName <> '' then begin
      fResultFileName := lfrmMobileTest.ResultsFileName;
      liPCWB5Iterations := lfrmMobileTest.PCWB5Iterations;
    end; // if
  finally
    unfadefast( self );
    lfrmMobileTest.Release;
    FreeAndNil( lfrmMobileTest );
  end;
  // The mobile score is composed of two components: an official run for
  // performance and a battery run down cycle.
  // When the mobile run is issued, it starts with an official performance
  // run.  The OpenSourceMark score is retained while OSMark tests are then
  // issued spaced one minute apart until the battery is depleted.
  fOSMarkMobileTests := TOSMarkMobileTests.Create( self );
  try
    Fade := FALSE;
    LinkMobileEventHandlers;
    fOSMarkMobileTests.SpinUp := fOSMarkINI.SpinUp;
    fOSMarkMobileTests.TargetDrive := fOSMarkINI.TargetDrive;
    fOSMarkMobileTests.SystemInfo := fSystemInfo;
    case lMobileTestType of
      mttNormal: begin
        if fResultFileName <> '' then begin
          fOSMarkMobileTests.ResultFileName := fResultFileName;
        end else begin
          fOSMarkMobileTests.ResultFileName := fOSMarkMobileTests.DEFAULT_MOBILE_FILENAME_NORMAL;
        end; // if
        fOSMarkMobileTests.RunNormalMobileTest;
      end;
      mttMaxPowerDraw: begin
        if fResultFileName <> '' then begin
          fOSMarkMobileTests.ResultFileName := fResultFileName;
        end else begin
          fOSMarkMobileTests.ResultFileName := fOSMarkMobileTests.DEFAULT_MOBILE_FILENAME_MAX_POWER;
        end; // if
        fOSMarkMobileTests.RunMaxPowerMobileTest;
      end;
      mttLowPowerDraw: begin
        if fResultFileName <> '' then begin
          fOSMarkMobileTests.ResultFileName := fResultFileName;
        end else begin
          fOSMarkMobileTests.ResultFileName := fOSMarkMobileTests.DEFAULT_MOBILE_FILENAME_MIN_POWER;
        end; // if
        fOSMarkMobileTests.RunMinPowerMobileTest;
      end;
      mttPCWB5: begin
        if fResultFileName <> '' then begin
          fOSMarkMobileTests.ResultFileName := fResultFileName;
        end else begin
          fOSMarkMobileTests.ResultFileName := fOSMarkMobileTests.DEFAULT_MOBILE_FILENAME_PCWB5;
        end; // if
        fOSMarkMobileTests.Iterations := liPCWB5Iterations;
        Visible := FALSE;
        fOSMarkMobileTests.RunPCWB5MobileTest;
        Visible := TRUE;
      end;
    end; // case
  finally
    freeAndNil( fOSMarkMobileTests );
    Fade := TRUE;
  end; // try
end;

procedure TfrmOSMark.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = KEY_ESC then begin
    halt;
  end;
end;

procedure TfrmOSMark.SpeedButtonOfficalRunClick(Sender: TObject);
begin
  OfficalRunGUI;
end;

procedure TfrmOSMark.OfficalRunGUI;
begin
  if OpenOSMarkOptions then begin
    Fade := FALSE;
    Application.ProcessMessages;
    OfficialRun;
    Fade := TRUE;
    Application.ProcessMessages;
  end; // if
end; // OfficalRunGUI

procedure TfrmOSMark.SpeedButtonThinClientRunClick(Sender: TObject);
begin
  Fade := FALSE;
  Application.ProcessMessages;
  fOSMarkINI.Iterations := 3;
  fOSMarkINI.Defrag := FALSE;
  fOSMarkINI.ProcessIdleTasks := FALSE;
  RunSelectedTests(fOSMarkINI.Iterations, THIN_CLIENT_TESTS);
  Fade := TRUE;
  Application.ProcessMessages;
end;

procedure TfrmOSMark.SpeedButtonMobileRunClick(Sender: TObject);
begin
  RunMobileTests;
end;

procedure TfrmOSMark.SpeedButtonStabilityTestClick(Sender: TObject);
var i : integer;
    ldTime : double;
    ldSeconds : double;
begin
  Defrag;
  ProcessIdleTasks;
  fTestStart := Now;
  StatusBar1.Panels[ PANEL_TIME ].Text := 'Test starts: ' +
    DateTimeToStr( fTestStart );
  for i := 1 to 500 do begin
    StatusBar1.Panels[ PANEL_LOOP ].Text :=
      'Loop ' + IntToStr( i ) + ' of ' + IntToStr( 500 );
    fbOfficialRun := TRUE;
    fOSMarkTestSuite := TOSMarkTestSuite.Create( self );
    try
      InitializeOSMarkTestSuiteCL( FALSE, FALSE );
      Fade := False;
      fOSMarkTestSuite.OfficialRun;
    finally
      freeAndNil( fOSMarkTestSuite );
      Fade := TRUE;
    end; // try...finally
    fbOfficialRun := FALSE;
  end; // for
  ldTime := ( Now - fTestStart );
  ldSeconds := 24 * 60 * 60 * ldTime;
  StatusBar1.Panels[ PANEL_TIME ].Text := 'Run time (s): ' +
    FloatToStr( ldSeconds );
  application.ProcessMessages;
end;

procedure TfrmOSMark.SpeedButtonSaveClick(Sender: TObject);
begin
  SaveStringListViaFileBrowser( memoResults.Lines, DEFAULT_FILENAME );
end;

procedure TfrmOSMark.SpeedButtonResultViewerClick(Sender: TObject);
var
  lfrmResults : TfrmOSMResults;
begin
  lfrmResults := TfrmOSMResults.Create(self);
  try
    fadefast50( self );
    application.ProcessMessages;
    lfrmResults.LoadDefaultDatabase;
    lfrmResults.ShowModal;
  finally
    lfrmResults.Release;
    freeAndNil( lfrmResults );
    unfadefast( self );
  end; // try..finally
end;

procedure TfrmOSMark.SpeedButtonQuitClick(Sender: TObject);
begin
  halt;
end;

procedure TfrmOSMark.SpeedButtonAboutClick(Sender: TObject);
var
  lfrmAbout : TfrmAbout;
  i : integer;
begin
  try
    MediaPlayerAbout.AutoOpen := TRUE;
    if MediaPlayerAbout.Enabled then begin
      MediaPlayerAbout.Open;
      MediaPlayerAbout.Previous;
      MediaPlayerAbout.Play;
    end; // if
  except
  end; // try...except
  FadeDown( self, 128, 4 );
  lfrmAbout := TfrmAbout.Create(self);
  try
    lfrmAbout.ShowModal;
  finally
    lfrmAbout.Release;
    FreeAndNil(lfrmAbout);
  end;
  unfadefast( self );
  if MediaPlayerAbout.Enabled then begin
    MediaPlayerAbout.Stop;
  end; // if
end;

procedure TfrmOSMark.SpeedButtonCPUClick(Sender: TObject);
var
  frmCPUInfo : TfrmCPUInfo;
begin
  frmCPUInfo := TfrmCPUInfo.Create( Self );
  with frmCPUInfo do begin
    try
      fadefast50( self );
      ShowModal;
    finally
      release;
      freeandnil( frmCPUInfo );
      unfadefast( self );
    end; // try
  end; // with
end;

procedure TfrmOSMark.SpeedButtonOptionsClick(Sender: TObject);
begin
  OpenOSMarkOptions;
end; // procedure TfrmOSMark.SpeedButtonOptionsClick

function TfrmOSMark.OpenOSMarkOptions: Boolean;
var
  lfrmOSMarkOptions : TfrmOSMarkOptions;
begin
  lfrmOSMarkOptions := TfrmOSMarkOptions.Create( self );
  try
    FadeFast50( self );
    lfrmOSMarkOptions.SpinUp            := fOSMarkINI.SpinUp;
    lfrmOSMarkOptions.ShowStatus        := fOSMarkINI.ShowStatus;
    lfrmOSMarkOptions.Defrag            := fOSMarkINI.Defrag;
    lfrmOSMarkOptions.ProcessIdleTasks  := fOSMarkINI.ProcessIdleTasks;
    lfrmOSMarkOptions.TargetDrive       := fOSMarkINI.TargetDrive;
    lfrmOSMarkOptions.ThreadCount       := fOSMarkINI.ThreadCount;
    lfrmOSMarkOptions.ProgramPriority   := fProgramPriority;
    lfrmOSMarkOptions.TimerResolution   := fTimerResolution;
    if lfrmOSMarkOptions.ShowModal = mrCancel then begin
      result := FALSE;
      exit;
    end;
    result := TRUE;
    fOSMarkINI.SpinUp           := lfrmOSMarkOptions.SpinUp;
    fOSMarkINI.ShowStatus       := lfrmOSMarkOptions.ShowStatus;
    fOSMarkINI.Defrag           := lfrmOSMarkOptions.Defrag;
    fOSMarkINI.ProcessIdleTasks := lfrmOSMarkOptions.ProcessIdleTasks;
    fOSMarkINI.TargetDrive      := lfrmOSMarkOptions.TargetDrive;
    fOSMarkINI.ThreadCount      := lfrmOSMarkOptions.ThreadCount;
    fProgramPriority  := lfrmOSMarkOptions.ProgramPriority;
    fTimerResolution  := lfrmOSMarkOptions.TimerResolution;
  finally
    UnFadeFast( self );
    lfrmOSMarkOptions.Release;
    FreeAndNil( lfrmOSMarkOptions );
  end; // try
  SetTimerResolution;
end; // procedure TfrmOSMark.OpenOSMarkOptions;

procedure TfrmOSMark.SpeedButtonBandwidthBurnClick(Sender: TObject);
begin
  OpenBandwidthBurn;
end;

procedure TfrmOSMark.SpeedButton3Click(Sender: TObject);
begin
  RunThreadWrecker;
end;

procedure TfrmOSMark.SpeedButtonFileCopyClick(Sender: TObject);
begin
  OpenFileCopy;
end;

procedure TfrmOSMark.SpeedButtonMemLantecyPlusClick(Sender: TObject);
begin
  OpenMemLatencyPlus;
end;

procedure TfrmOSMark.SpeedButtonWhetBurnClick(Sender: TObject);
begin
  OpenWhetBurn;
end;

procedure TfrmOSMark.SpeedButton6Click(Sender: TObject);
begin
  OpenCpuSpeed;
end;

procedure TfrmOSMark.SpeedButtonCpuUtilizationClick(Sender: TObject);
begin
  OpenCpuUtilization;
end;

procedure TfrmOSMark.SpeedButton8Click(Sender: TObject);
begin
  memoResults.SelectAll;
  memoResults.CopyToClipboard;
end;

procedure TfrmOSMark.SpeedButtonCustomRunClick(Sender: TObject);
var
  lfrmOSMarkSelectTests : TfrmOSMarkSelectTests;
begin
  if not OpenOSMarkOptions then exit;
  lfrmOSMarkSelectTests := TfrmOSMarkSelectTests.Create( self );
  try
    FadeFast50( self );
    Application.ProcessMessages;
    lfrmOSMarkSelectTests.SelectedTests := fOSMarkINI.SelectedTests;
    lfrmOSMarkSelectTests.Iterations := fOSMarkINI.Iterations;
    if lfrmOSMarkSelectTests.ShowModal = mrCancel then begin
      exit;
    end;
    fOSMarkINI.SelectedTests := lfrmOSMarkSelectTests.SelectedTests;
    fOSMarkINI.Iterations := lfrmOSMarkSelectTests.Iterations;
  finally
    UnFadeFast( self );
    lfrmOSMarkSelectTests.Release;
    FreeAndNil( lfrmOSMarkSelectTests );
    Application.ProcessMessages;
  end; // try
  Fade := FALSE;
  Application.ProcessMessages;
  RunSelectedTests(fOSMarkINI.Iterations, fOSMarkINI.SelectedTests);
  Fade := TRUE;
  Application.ProcessMessages;
end; // procedure TfrmOSMark.OpenOSMarkOptions;

procedure TfrmOSMark.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if MemoInfo.Visible then MemoInfo.Visible := FALSE;
end;

procedure TfrmOSMark.SpeedButtonCPUMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  memoinfo.Visible := TRUE;
  memoinfo.Text := 'Click the "CPU" button to see a full CPUID report.';
end;

procedure TfrmOSMark.SpeedButton6MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  memoinfo.Visible := TRUE;
  memoinfo.Text := '"CPUSpeed" graphs the current CPU clock speed for saving and logging.';
end;

procedure TfrmOSMark.SpeedButtonCpuUtilizationMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  memoinfo.Visible := TRUE;
  memoinfo.Text := 'Keep track of CPU usage with the "CpuUtilization" utility.  This program automatically monitors all logical CPUs.  Save the history to a file or even launch and track external programs.';
end;

procedure TfrmOSMark.SpeedButtonOfficalRunMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  memoinfo.Visible := TRUE;
  memoinfo.Text := '"Official Run" will kick off three iterations of the full set of official tests.  At the end of an official run a score will be produced and the Result Viewer will be displayed';
end;

procedure TfrmOSMark.SpeedButtonCustomRunMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  memoinfo.Visible := TRUE;
  memoinfo.Text := 'If you want to specify your own set of tests, click "Custom Run."';
end;

procedure TfrmOSMark.SpeedButtonMobileRunMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  memoinfo.Visible := TRUE;
  memoinfo.Text := 'The "Mobile Run" button will launch a dialog that will allow you to select between three different mobile tests: "Max Power Draw," "Normal," and "Min Power Draw.';
end;

procedure TfrmOSMark.SpeedButtonThinClientRunMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  memoinfo.Visible := TRUE;
  memoinfo.Text := 'A thin client is a PC or other computing device that behaves as a terminal connected over a network or phone line to a server. '
                + 'The OpenSourceMark thin client benchmark contains a suite of tests that are specifically made for evaluating thin client performance. '
                + 'To run the tests, connect to the Windows server from the thin client to be tested. Launch OpenSourceMark on the server and then simply click Thin Client Run.';
end;

procedure TfrmOSMark.SpeedButtonOptionsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  memoinfo.Visible := TRUE;
  memoinfo.Text := 'The "Options" button lets you set important configuration options.';
end;

procedure TfrmOSMark.SpeedButtonResultViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  memoinfo.Visible := TRUE;
  memoinfo.Text := 'The "Result Viewer" is a powerful tool for examining and comparing "Official Run" results.';
end;

procedure TfrmOSMark.SpeedButtonHelpMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  memoinfo.Visible := TRUE;
  memoinfo.Text := 'The "Help" button launches the OpenSourceMark help file.';
end;

procedure TfrmOSMark.SpeedButtonQuitMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  memoinfo.Visible := TRUE;
  memoinfo.Text := 'The "Quit" button exits OpenSourceMark.';
end;

procedure TfrmOSMark.SpeedButtonAboutMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  memoinfo.Visible := TRUE;
  memoinfo.Text := 'Find out who is behind OpenSourceMark by clicking the "About" button.';
end;

procedure TfrmOSMark.SpeedButtonStabilityTestMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  memoinfo.Visible := TRUE;
  memoinfo.Text := 'The "Stability Test" pushes your system to the limit by continuously running OpenSourceMark tests up to several days.';
end;

procedure TfrmOSMark.SpeedButton3MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  memoinfo.Visible := TRUE;
  memoinfo.Text := '"ThreadWrecker" is a very demanding test that spawns a large number of diverse threads.';
end;

procedure TfrmOSMark.SpeedButtonWhetBurnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  memoinfo.Visible := TRUE;
  memoinfo.Text := 'Does your system seem sluggish?  Could it be running hot?  Test for CPU thermal throttling with the "WhetBurn" tool.';
end;

procedure TfrmOSMark.SpeedButtonBandwidthBurnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  memoinfo.Visible := TRUE;
  memoinfo.Text := 'Although commonly copied, "BandwidthBurn" was the first PC utility to graph the full memory subsystem bandwidth.  BandwidthBurn has been successfully used in studies to enhance CPU bandwidth performance.';
end;

procedure TfrmOSMark.SpeedButtonMemLantecyPlusMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  memoinfo.Visible := TRUE;
  memoinfo.Text := 'Before there was the HPCChallenge RandomAccess tests and before the Memory Latency tests in PCMark, there was "MemLatencyPlus".';
end;

procedure TfrmOSMark.SpeedButtonFileCopyMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  memoinfo.Visible := TRUE;
  memoinfo.Text := 'The highly customizable "FileCopy" benchmarking utility is a real-world test of Windows file copy performance.';
end;

procedure TfrmOSMark.SpeedButtonSaveMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  memoinfo.Visible := TRUE;
  memoinfo.Text := 'Click the "Save" button to save the results output, below, to a file.';
end;

procedure TfrmOSMark.SpeedButton8MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  memoinfo.Visible := TRUE;
  memoinfo.Text := 'Use the "Copy to Clipboard" button to copy all of the results output, below, to the clipboard.';
end;

procedure TfrmOSMark.SpeedButtonPCWB5MobileMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  memoinfo.Visible := TRUE;
  memoinfo.Text := 'Click this button to iterate through the PCWB5 office test.  The completion times will be tracked in a file.';
end;

procedure TfrmOSMark.FormActivate(Sender: TObject);
begin
  if fFade then UnFadeFast( self );
end;

procedure TfrmOSMark.FormDeactivate(Sender: TObject);
begin
  if fFade then FadeFast50( self );
end;

procedure TfrmOSMark.SetFade( Value : Boolean );
begin
  fFade := Value;
  AlphaBlend := Value;
end; // setfade

procedure TfrmOSMark.WMEndSession(var Msg: TWMEndSession);
begin
//  if Msg.EndSession = True then begin
  memoResults.Lines.Add('');
  memoResults.Lines.Add('The system is being shut down. Dumping results...');
  SystemShutDown;
//  end; // if
  inherited;
end;

procedure TfrmOSMark.SystemShutDown;
begin
  if assigned( fOSMarkMobileTests ) then begin
    fOSMarkMobileTests.SystemShutDown;
    SaveStringListToFile( memoResults.Lines, fOSMarkMobileTests.ResultFileName, FALSE, TRUE );
  end; // if
  inherited;
end; //procedure TfrmOSMark.SystemShutDown;

procedure TfrmOSMark.UpdateStatusMemoMobile( Sender: TObject );
begin
  memoResults.Lines.Add( fOSMarkMobileTests.OutputString );
  application.processmessages;
end; // procedure UpdateStatusMemo( asOutputLine : string );

procedure TfrmOSMark.OnTestSuiteRunStartMobile( Sender: TObject );
begin
  pBar.Progress := 0;
  pBar.MaxValue := fOSMarkMobileTests.TotalNumberOfTestsToRun;
  ClearStatusBar;
  if not fbCommandLineRun then begin
    fTestStart := Now;
    StatusBar1.Panels[ PANEL_TIME ].Text := 'Test starts: ' +
      DateTimeToStr( fTestStart );
  end; // if
end; // procedure TfrmOSMark.OnTestSuiteRunStart

procedure TfrmOSMark.OnIterationStartMobile( Sender: TObject );
begin
  StatusBar1.Panels[ PANEL_ITERATION ].Text := 'Iteration ' +
    intToStr( fOSMarkMobileTests.OSMarkTestSuite.Iteration ) + ' of ' +
    intToStr( fOSMarkMobileTests.OSMarkTestSuite.IterationFinal );
  application.processmessages;
end; //procedure OnIterationStart( Sender: TObject );

procedure TfrmOSMark.OnTestBeginsMobile( Sender: TObject );
begin
  StatusBar1.Panels[ PANEL_MAIN ].text := 'Running ' +
    fOSMarkMobileTests.OSMarkTestSuite.TestName + '...';
  application.ProcessMessages;
end;

procedure TfrmOSMark.OnTestEndsMobile( Sender: TObject );
begin
  pBar.Progress := pBar.Progress + 1;
  StatusBar1.Panels[ PANEL_MAIN ].text := '';
  StatusBar1.Panels[ PANEL_LAST_RESULT ].text := fOSMarkMobileTests.OSMarkTestSuite.TestName +
    ' score = ' + IntToStr( fOSMarkMobileTests.OSMarkTestSuite.TestScore );
  application.ProcessMessages;
end; // procedure OnTestEnds( Sender: TObject );

procedure TfrmOSMark.OnTestSuiteRunEndMobile( Sender: TObject );
var
  ldTime : double;
  ldSeconds : double;
begin
  pBar.Progress := pBar.MaxValue;
  StatusBar1.Panels[ PANEL_MAIN ].text := 'Test Complete. Ready...';
  if not fbCommandLineRun then begin
    ldTime := ( Now - fTestStart );
    ldSeconds := 24 * 60 * 60 * ldTime;
    StatusBar1.Panels[ PANEL_TIME ].Text := 'Run time (s): ' +
      FloatToStr( ldSeconds );
    application.ProcessMessages;
  end; // if
end; // procedure OnTestEnds( Sender: TObject );

procedure TfrmOSMark.OnCriticalBatteryLevel( Sender : TObject );
begin
  memoResults.Lines.Add( 'Battery is at critical levels! Dumping results to file!' );
  application.ProcessMessages;
  SaveStringListToFile( memoResults.Lines, fOSMarkMobileTests.ResultFileName, FALSE, TRUE );
end;

procedure TfrmOSMark.CheckBattery;
const
  SHUTDOWN_BATTERY_LEVEL = 3;
var
  i : integer;
  s : string;
begin
  if IsBatteryAvailable then begin
    i := GetBatteryLevel;
    if IsOnBatteryPower then s := 'Battery: ' else s := 'Plugged In: ';
    s := s  + intToStr( i ) + '%';
    if fOldBatteryCaption <> s then begin
      pbarBattery.Position := i;
      lblBattery.Caption := s;
      fOldBatteryCaption := s;
    end; // if
    if i <= SHUTDOWN_BATTERY_LEVEL then begin
      if assigned( fOSMarkMobileTests ) then begin
        fOSMarkMobileTests.SystemShutDown;
        SaveStringListToFile( memoResults.Lines, fOSMarkMobileTests.ResultFileName, FALSE, TRUE );
      end; // if
      ShutdownWindows;
    end;
  end else begin
    TimerBatteryLife.Enabled := FALSE;
    lblBattery.Visible := FALSE;
    pbarBattery.Visible := FALSE;
  end; // if
end; // CheckBattery

procedure TfrmOSMark.TimerBatteryLifeTimer(Sender: TObject);
begin
  CheckBattery;
end;

procedure TfrmOSMark.SpeedButtonHelpClick(Sender: TObject);
begin
  StartProgram( '.\osmfiles\OSMarkDocumentation.html' );
end;

procedure TfrmOSMark.memoResultsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Line, Column : integer;
  s : string;
  inputLine : array[0..99] of string;
  liParamCount : integer;
  liMSR : integer;
  li64MSRValue : int64;
  liCPU : integer;

  function GetDirectoryListing : string;
  var
    SR: TSearchRec;
    lsFileName : string;
    lsFilter : string;
    lFile : file of word;
    li64FileBytes : Int64;
    li64TotalBytes : Int64;
    liNumberOfFiles : integer;
  begin
    result := 'File listing for "' + GetCurrentDir + '":';
    li64TotalBytes := 0;
    liNumberOfFiles := 0;
    lsFilter := inputLine[ 1 ];
    if lsFilter = '' then lsFilter := '*.*';
    if FindFirst(lsFilter, faAnyFile, SR) = 0 then
    begin
      repeat
        lsFileName := SR.Name;
        if (SR.Attr = faDirectory) then begin
          lsFileName := lsFileName + ' (directory)';
        end else begin
          inc( liNumberOfFiles );
          li64FileBytes := SR.Size;
          li64TotalBytes := li64TotalBytes + li64FileBytes;
          lsFileName := lsFileName + ', ' + IntToStr( li64FileBytes )
            + ' bytes, ' + DateTimeToStr( FileDateToDateTime( SR.Time ) );
        end;
        result := result + CR_LF + lsFileName;
      until FindNext(SR) <> 0;
      FindClose(SR);
    end else begin
      result := result + CR_LF + 'No file found in current directory.';
    end; // if
    result := result + CR_LF + IntToStr( li64TotalBytes ) + ' bytes used in '
      + IntToStr( liNumberOfFiles ) + ' files.';
    result := result + CR_LF + 'Directory listing complete.' + CR_LF;
  end; // function

  procedure ParseInputLine;
  var
    i : integer;
    liLineLength : integer;
    lcCurrentChar : char;
  begin
    liParamCount := 0;
    i := 1;
    liLineLength := strLen( pchar( s ) );
    while i <= liLineLength do begin
      lcCurrentChar := s[ i ];
      if (lcCurrentChar <> ' ') then begin
        inputLine[ liParamCount ] := inputLine[ liParamCount ] + lcCurrentChar;
        if lcCurrentChar = '"' then begin // need to find ending quote
          inc( i );
          lcCurrentChar := s[ i ];
          while (lcCurrentChar <> '"') and (i <= liLineLength) do begin
            inputLine[ liParamCount ] := inputLine[ liParamCount ] + lcCurrentChar;
            inc( i );
            lcCurrentChar := s[ i ];
          end;
          inputLine[ liParamCount ] := inputLine[ liParamCount ] + '"';
        end; // if
        inc( i );
      end else begin
        inc( liParamCount ); // move to next parameter
        inc( i );
        while s[ i ] = ' ' do begin // skip contiguous spaces
          inc( i );
        end;
      end;
    end; // for
  end; //ParseInputLine

begin
  if key = 13 then begin
    key := 0;
    With memoResults do begin
       Line := Perform(EM_LINEFROMCHAR,SelStart, 0) ;
       Column := SelStart - Perform(EM_LINEINDEX, Line, 0) ;
    end;
    s := lowercase( memoResults.Lines[ Line ] );
    ParseInputLine;
    if inputLine[ 0 ] = 'getcurrentdir' then begin
      s := GetCurrentDir;
      memoResults.Lines.Add( 'The current directory is ' + s );
    end else if inputLine[ 0 ] = 'dir' then begin
      memoResults.Lines.Add( GetDirectoryListing );
    end else if inputLine[ 0 ] = 'officialrun' then begin
      OfficalRunGUI;
    end else if inputLine[ 0 ] = 'pstatetest' then begin
      TestPStates;
    end else if inputLine[ 0 ] = 'cd' then begin
      chDir( inputLine[ 1 ] );
      s := GetCurrentDir;
      memoResults.Lines.Add( 'The current directory is now ' + s );
    end else if inputLine[ 0 ] = 'rdmsr' then begin
      liMSR := StrToInt( inputLine[ 1 ] );
      if inputLine[ 2 ] <> '' then begin
        liCPU := StrToInt( inputLine[ 2 ] );
      end else begin
        liCPU := 0;
      end;
      fMsrRegister := fMSRDriver.ReadMsr( liCPU , liMSR );
      s := 'The value of MSR $' + IntToHex( liMSR, 8 ) + ' for CPU '
           + intToStr( liCPU ) + ' is  $'
           + IntToHex( fMsrRegister.HighWord, 8) + ' $'
           + IntToHex( fMsrRegister.LowWord, 8) ;
      memoResults.Lines.Add( s );
    end else if inputLine[ 0 ] = 'getnumberofcpus' then begin
      s := 'The number of logical CPUs in the system is: '
           + IntToStr( fMsrDriver.GetNumberOfCpus );
      memoResults.Lines.Add( s );
    end else if inputLine[ 0 ] = 'wrmsr' then begin
      liMSR := StrToInt( inputLine[ 1 ] );
      li64MSRValue := StrToInt( inputLine[ 2 ] );
      fMsrRegister.AsInt64 := li64MSRValue;
      if inputLine[ 3 ] <> '' then begin
        liCPU := StrToInt( inputLine[ 3 ] );
      end else begin
        liCPU := 0;
      end;
      fMsrRegister := fMSRDriver.WriteMsr( liCPU , liMSR, fMsrRegister );
      s := 'The value of MSR $' + IntToHex( liMSR, 8 ) + ' for CPU '
           + intToStr( liCPU ) + ' is  $'
           + IntToHex( fMsrRegister.HighWord, 8) + ' $'
           + IntToHex( fMsrRegister.LowWord, 8) ;
      memoResults.Lines.Add( s );
    end else if inputLine[ 0 ] = 'run' then begin
      StartProgram( inputLine[ 1 ] );
      s := 'Attempted to execute : ' + inputLine[ 1 ];
      memoResults.Lines.Add( s );
    end else if inputLine[ 0 ] = 'cmd' then begin
      StartProgram( 'cmd' );
      s := 'Attempted to open command line.';
      memoResults.Lines.Add( s );
    end; // if
    memoResults.Lines.Add( '.' );
    memoResults.Lines.Add( 'Ready. ' );
  end; // if
//    memoResults.Lines. .Count;
end;

function TfrmOSMark.GetCpuMhz: integer;
var
  i, Value     : Cardinal;
begin
  fObjectSet := fServices.ExecQuery('SELECT CurrentClockSpeed FROM Win32_Processor', 'WQL', wbemFlagReturnImmediately, nil);
  fEnum :=  ( fObjectSet._NewEnum ) as IEnumVariant;
  // go to the first object (cpu); must pass Value for reference:
  if (fEnum.Next( 1, ftempObj, Value ) = S_OK) then begin
    fSObject := IUnknown( ftempObj ) as SWBemObject;
    fPropSet := fSObject.Properties_;
    fpropEnum := ( fPropSet._NewEnum ) as IEnumVariant;
    // goto the first property (should be "CurrentClockSpeed"):
    if ( fpropEnum.Next(1, ftempObj, Value ) = S_OK ) then begin
      fSProp := IUnknown(ftempObj) as SWBemProperty;
      if fSProp.Name = 'CurrentClockSpeed' then begin
        result := fSProp.Get_Value;
      end; // if
    end; // if
  end; // if
end; //function TfrmOSMark.GetCpuMhz: integer;

function TfrmOSMark.GetMaxCpuMhz: integer;
var
  i, Value      : Cardinal;
  lSObject      : ISWbemObject;
  lObjectSet    : ISWbemObjectSet;
begin
  lObjectSet := fServices.ExecQuery('SELECT MaxClockSpeed FROM Win32_Processor', 'WQL', wbemFlagReturnImmediately, nil);
  fEnum :=  ( fObjectSet._NewEnum ) as IEnumVariant;
//  for lSObject in lObjectSet do begin
//    result := fSObject.Properties_.Item('MaxClockSpeed');
//    exit;
//  end;
//  fEnum :=  ( fObjectSet._NewEnum ) as IEnumVariant;
  // go to the first object (cpu); must pass Value for reference:
  if (fEnum.Next( 1, ftempObj, Value ) = S_OK) then begin
    fSObject := IUnknown( ftempObj ) as SWBemObject;
    fPropSet := fSObject.Properties_;
    fpropEnum := ( fPropSet._NewEnum ) as IEnumVariant;
    // goto the first property (should be "MaxClockSpeed"):
    if ( fpropEnum.Next(1, ftempObj, Value ) = S_OK ) then begin
      fSProp := IUnknown(ftempObj) as SWBemProperty;
      while fSProp.Name <> 'MaxClockSpeed' do begin
        //fpropEnum. .Next(1, ftempObj, Value );
      end;
      result := fSProp.Get_Value;
    end; // if
  end; // if
end; //function TfrmOSMark.GetCpuMhz: integer;

procedure TfrmOSMark.TestPStates;
var
  liMinMHz, liCurrentMHz, liMaxMHz : integer;
  i, j : integer;
  lPStateTimes: array[0..99] of extended;
  lPStateTimeAverage : extended;
  procedure WaitUntilMinMHz;
  begin
    Repeat
      sleep( 100 );
      liCurrentMHz := GetCpuMhz;
    Until liCurrentMHz = liMinMHz;
  end; // WaitUntilMinMHz
  procedure DriveToMaxPState;
  begin
    repeat
      liCurrentMHz := GetCpuMhz;
      if fStopWatch.SplitTime > 5 then raise exception.Create( 'Timeout occurred!' );
    until liCurrentMHz >= liMaxMHz;
  end;
begin
  AlphaBlend := FALSE;
  try
    memoResults.Lines.Add( 'Testing P-States. Please wait...' );
    liMaxMHz := trunc( fStopWatch.GetCPUClockspeed( FALSE ) / 1000000);
    memoResults.Lines.Add( 'Maximum CPU frequency appears to be around ' +
      IntToStr(liMaxMHz) + ' MHz.'  );
    memoResults.Lines.Add( 'Trying to find min P-State frequency...' );
    liMinMHz := liMaxMHz;
    for i := 1 to 10 do begin
      sleep( 1000 );
      liCurrentMHz := GetCpuMhz;
      if liCurrentMHz < liMinMHz then liMinMHz := liCurrentMHz;
      memoResults.Lines.Add( 'Windows reports a CPU frequency of ' +
        IntToStr(liCurrentMHz) + ' MHz.'  );
      application.ProcessMessages;
    end; // for
    if liMinMHz >= liMaxMHz then begin
      memoResults.Lines.Add( 'Trying one last time to get min P-State frequency...' );
      application.ProcessMessages;
      sleep( 2000 );
      application.ProcessMessages;
      liCurrentMHz := GetCpuMhz;
      if liCurrentMHz < liMinMHz then liMinMHz := liCurrentMHz;
    end; // if
    if liMinMHz >= liMaxMHz then begin
      memoResults.Lines.Add( 'P-States do not seem to be working.' );
      exit;
    end; // if
    memoResults.Lines.Add( 'The minimum P-State frequency appears to be ' +
        IntToStr(liMinMHz) + ' MHz.'  );
    lPStateTimeAverage := 0;
    for i := 0 to 99 do begin
      application.ProcessMessages;
      WaitUntilMinMHz;
      fStopWatch.StartTimer;
      DriveToMaxPState;
      lPStateTimes[ i ] := fStopWatch.StopTimer;
      lPStateTimeAverage := lPStateTimeAverage + lPStateTimes[ i ];
      memoResults.Lines.Add( 'P-State Time '
        + intToStr( i ) + ': ' + FloatToStr( lPStateTimes[ i ] ) );
      application.ProcessMessages;
    end; // for
    lPStateTimeAverage := lPStateTimeAverage / 100;
    memoResults.Lines.Add( 'P-State Average Time: '
      + FloatToStr( lPStateTimeAverage ) );
    application.ProcessMessages;
  finally
    AlphaBlend := TRUE;
  end;
end; // procedure TfrmOSMark.TestPStates;

end.
