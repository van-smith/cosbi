unit uCpuSpeedChangeCpuRatio;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TfrmChangeCpuRatio = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmChangeCpuRatio: TfrmChangeCpuRatio;

implementation

{$R *.dfm}

//function TfrmCPUSpeed.ChangeClockRatio: integer;
//var
//  ls_RunProgram : string;
//  le_CpuClockSpeed : extended;
//  li_MHz : integer;
//begin
//  tmrSpeedUpdate.Enabled := FALSE;
//  StartProgramWait( ls_RunProgram , SW_SHOWMINIMIZED );
//  StopWatch.SleepTime := 100;
//  le_CpuClockSpeed := StopWatch.GetCPUClockspeed( TRUE );
//  li_MHz := Round( le_CpuClockSpeed / ONE_MILLION );
//  SetWinMHz( li_MHz );
//  fi64_StartTickTmr := 0;
//  tmrSpeedUpdate.Enabled := TRUE;
//  result := li_MHz;
//end; //procedure TfrmCPUSpeed.ChangeClockRatio


end.
