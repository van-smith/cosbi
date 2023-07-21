unit uCpuSpeedManual;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, COSBI_Common;

type
  TfrmCpuSpeedManual = class(TForm)
    pnlGetCpuSpeed: TPanel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    btnGetCPUSpeed: TButton;
    btnCounter: TButton;
    edTicks: TEdit;
    procedure btnCounterClick(Sender: TObject);
  private
    { Private declarations }
    fi64StartTick : int64;
    fi64StopTick : int64;
//    procedure ShowCpuSpeedSample;
  public
    { Public declarations }
  end;

var
  frmCpuSpeedManual: TfrmCpuSpeedManual;

implementation

{$R *.dfm}

const
  BTN_START = 'Start';
  BTN_END   = 'Stop';

procedure TfrmCpuSpeedManual.btnCounterClick(Sender: TObject);
var
  li64TickCount    : Int64;
  ld_TickCount      : Double;
begin
  if btnCounter.Caption = BTN_START then begin
    edTicks.Text := 'Counting... Press "Stop" for tick count.';
    btnCounter.Caption := BTN_END;
    fi64StartTick := GetCycleCount;
  end else begin
    fi64StopTick := GetCycleCount;
    li64TickCount := fi64StopTick - fi64StartTick;
    ld_TickCount := li64TickCount;
    ShowMessage( 'Elapsed CPU cycles: ' + Format('%n', [ld_TickCount] ) );
    edTicks.Text := 'Ticks: ' + IntToStr( li64TickCount );
    btnCounter.Caption := BTN_START;
  end; // if btnCounter.Caption := BTN_START
end;


//procedure TfrmCPUSpeed.ShowCpuSpeedSample;
//var
//  ldClockSpeed   : double;
//  li64TickCount    : Int64;
//  ldTime           : double;
//begin
//  ckbxAuto.Checked := FALSE;
//  ldClockSpeed := GetCPUSpeed( li64TickCount, ldTime );
//  if fIsTransmeta then begin
//    ShowMessage( 'Transmeta Clockspeed = ' +
//                 FloatToStrF( ldClockSpeed, ffNumber, 11, 0 ) +
//                 ' MHz' );
//  end else if fIsC7 then begin
//    ShowMessage( 'VIA C7 Clockspeed = ' +
//                 FloatToStrF( ldClockSpeed, ffNumber, 11, 0 ) +
//                 ' MHz' );
//  end else begin
//    ShowMessage( 'Measured time = ' + FloatToStr( ldTime ) +
//                 's, Ticks = ' + IntToStr( li64TickCount ) +
//                 ', Clockspeed = ' +
//                 FloatToStrF( ldClockSpeed, ffNumber, 11, 0 ) +
//                 ' Hz' );
//  end; // if
//end; // procedure TfrmCPUSpeed.ShowCpuSpeedSample;

end.
