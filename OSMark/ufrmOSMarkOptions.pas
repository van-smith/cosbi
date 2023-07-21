unit ufrmOSMarkOptions;
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

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, FileCtrl, uCOSBI_TTest, COSBI_Common;

type
  TOSM_ProgramPriority = (osmppNormal, osmppHigh, osmppRealTime);
  TOSM_TimerResolution = (osmtrLow, osmtrHigh, osmtrCPU);

  TfrmOSMarkOptions = class(TForm)
    GroupBox1: TGroupBox;
    cboxSpinUp: TCheckBox;
    cboxStatus: TCheckBox;
    cboxProcessIdleTasks: TCheckBox;
    cboxDefragDrive: TCheckBox;
    Label1: TLabel;
    DriveComboBoxTarget: TDriveComboBox;
    Timer: TGroupBox;
    radioLow: TRadioButton;
    radioHigh: TRadioButton;
    radioCPU: TRadioButton;
    GroupBox_Priority: TGroupBox;
    radioNormalP: TRadioButton;
    radioHighP: TRadioButton;
    radioRealtimeP: TRadioButton;
    SpeedButtonOk: TSpeedButton;
    SpeedButtonCancel: TSpeedButton;
    GroupBoxThreadCount: TGroupBox;
    RadioAuto: TRadioButton;
    Radio1: TRadioButton;
    Radio8: TRadioButton;
    Radio4: TRadioButton;
    Radio2: TRadioButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure SpeedButtonCancelClick(Sender: TObject);
    procedure SpeedButtonOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fSpinUp           : Boolean;
    fShowStatus       : Boolean;
    fDefrag           : Boolean;
    fProcessIdleTasks : Boolean;
    fProgramPriority  : TOSM_ProgramPriority;
    fTimerResolution  : TOSM_TimerResolution;
    fTargetDrive      : char;
    fThreadCount      : TThreadCount;
    procedure SetProperties;
    procedure SetSpinUp( Value : Boolean );
    procedure SetShowStatus( Value : Boolean );
    procedure SetDefrag( Value : Boolean );
    procedure SetProcessIdleTasks( Value : Boolean );
    procedure SetProgramPriority( Value : TOSM_ProgramPriority );
    procedure SetTimerResolution( Value : TOSM_TimerResolution );
    procedure SetTargetDrive( Value : char );
    procedure SetThreadCount( Value : TThreadCount );
  public
    { Public declarations }
    property SpinUp : Boolean read fSpinUp write SetSpinUp;
    property ShowStatus : Boolean read fShowStatus write SetShowStatus;
    property Defrag : Boolean read fDefrag write SetDefrag;
    property ProcessIdleTasks : Boolean read fProcessIdleTasks write SetProcessIdleTasks;
    property ProgramPriority : TOSM_ProgramPriority read fProgramPriority write SetProgramPriority;
    property TimerResolution  : TOSM_TimerResolution read fTimerResolution write SetTimerResolution;
    property TargetDrive : char read fTargetDrive write SetTargetDrive;
    property ThreadCount : TThreadCount read fThreadCount write SetThreadCount;
  end;

implementation

{$R *.dfm}

procedure TfrmOSMarkOptions.SetSpinUp( Value : Boolean );
begin
  fSpinUp := Value;
  cboxSpinUp.Checked := Value;
end; // procedure TfrmOSMarkOptions.SetSpinUp

procedure TfrmOSMarkOptions.SetShowStatus( Value : Boolean );
begin
  fShowStatus := Value;
  cboxStatus.Checked := Value;
end; // procedure TfrmOSMarkOptions.SetShowStatus

procedure TfrmOSMarkOptions.SetDefrag( Value : Boolean );
begin
  fDefrag := Value;
  cboxDefragDrive.Checked := Value;
end; // procedure TfrmOSMarkOptions.SetDefrag

procedure TfrmOSMarkOptions.SetProcessIdleTasks( Value : Boolean );
begin
  fProcessIdleTasks := Value;
  cboxProcessIdleTasks.Checked := Value;
end; // procedure TfrmOSMarkOptions.SetProcessIdleTasks

procedure TfrmOSMarkOptions.SetProgramPriority( Value : TOSM_ProgramPriority );
begin
  fProgramPriority := Value;
  case Value of
    osmppNormal: radioNormalP.Checked := TRUE;
    osmppHigh: radioHighP.Checked := TRUE;
    osmppRealTime: radioRealTimeP.Checked := TRUE;
  end; // case
end; // procedure TfrmOSMarkOptions.SetProgramPriority

procedure TfrmOSMarkOptions.SetTimerResolution( Value : TOSM_TimerResolution );
begin
  fTimerResolution := Value;
  case Value of
    osmtrLow: radioLow.Checked := TRUE;
    osmtrHigh: radioHigh.Checked := TRUE;
    osmtrCPU: radioCpu.Checked := TRUE;
  end; // case
end; // procedure TfrmOSMarkOptions.SetTimerResolution

procedure TfrmOSMarkOptions.SetTargetDrive( Value : char );
begin
  fTargetDrive := Value;
  DriveComboBoxTarget.Drive := Value;
end; // procedure TfrmOSMarkOptions.SetProcessIdleTasks

procedure TfrmOSMarkOptions.SetThreadCount( Value : TThreadCount );
begin
  fThreadCount := Value;
  case Value of
    tcAuto: RadioAuto.Checked := TRUE;
    tc1: Radio1.Checked := TRUE;
    tc2: Radio2.Checked := TRUE;
    tc4: Radio4.Checked := TRUE;
    tc8: Radio8.Checked := TRUE;
  end; // case
end; // procedure TfrmOSMarkOptions.SetTimerResolution

procedure TfrmOSMarkOptions.FormCreate(Sender: TObject);
begin
  fSpinUp           := FALSE;
  fShowStatus       := TRUE;
  fDefrag           := FALSE;
  fProcessIdleTasks := FALSE;
  fProgramPriority  :=  osmppNormal;
  fTimerResolution  := osmtrHigh;
  fTargetDrive      := 'c';
  fThreadCount      := tcAuto;
end; //procedure TfrmOSMarkOptions.FormCreate

procedure TfrmOSMarkOptions.SetProperties;
begin
  fSpinUp           := cboxSpinUp.Checked;
  fShowStatus       := cboxStatus.Checked;
  fDefrag           := cboxDefragDrive.Checked;
  fProcessIdleTasks := cboxProcessIdleTasks.Checked;
  if radioNormalP.Checked then begin
    fProgramPriority  :=  osmppNormal;
  end else if radioHighP.Checked then begin
    fProgramPriority  :=  osmppHigh;
  end else if radioRealtimeP.Checked then begin
    fProgramPriority  :=  osmppRealTime;
  end; // if
  if radioLow.Checked then begin
    fTimerResolution  := osmtrLow;
  end else if radioHigh.Checked then begin
    fTimerResolution  := osmtrHigh;
  end else if radioCPU.Checked then begin
    fTimerResolution  := osmtrCPU;
  end; // if
  fTargetDrive      := DriveComboBoxTarget.Drive;

  if RadioAuto.Checked then begin
    fThreadCount      := tcAuto;
  end else if Radio1.Checked then begin
    fThreadCount      := tc1;
  end else if Radio2.Checked then begin
    fThreadCount      := tc2;
  end else if Radio4.Checked then begin
    fThreadCount      := tc4;
  end else if Radio8.Checked then begin
    fThreadCount      := tc8;
  end;
end; // procedure TfrmOSMarkOptions.SetProperties;

procedure TfrmOSMarkOptions.SpeedButtonOkClick(Sender: TObject);
begin
  SetProperties;
  ModalResult := mrOK;
end;

procedure TfrmOSMarkOptions.SpeedButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmOSMarkOptions.FormActivate(Sender: TObject);
begin
  UnFadeFast( self );
end;

procedure TfrmOSMarkOptions.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FadeFast100( self );
end;

end.
