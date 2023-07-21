program CPUSpeed;

uses
  Forms,
  uCPUSpeed in 'uCPUSpeed.pas' {frmCPUSpeed},
  COSBI_Common in '..\COSBI\COSBI_Common.pas',
  uCOSBI_SystemInfo in '..\COSBI\uCOSBI_SystemInfo.pas',
  CosbiCpuid in '..\COSBI\CosbiCpuid.pas',
  COSBI_Status in '..\COSBI\COSBI_Status.pas' {frmStatus},
  CosbiLogFile in '..\COSBI\CosbiLogFile.pas',
  uMsr in '..\msr\uMsr.pas',
  uCpuClockSpeed in 'uCpuClockSpeed.pas',
  uCpuSpeedSettings in 'uCpuSpeedSettings.pas' {Form1},
  uCpuSpeedMsr in 'uCpuSpeedMsr.pas' {frmMSR},
  uCpuSpeedManual in 'uCpuSpeedManual.pas' {frmCpuSpeedManual},
  uCpuSpeedLog in 'uCpuSpeedLog.pas' {frmCpuSpeedLog},
  uCpuSpeedChangeCpuRatio in 'uCpuSpeedChangeCpuRatio.pas' {frmChangeCpuRatio},
  WbemScripting_TLB in '..\COSBI\WbemScripting_TLB.pas',
  ActiveDs_TLB in '..\COSBI\ActiveDs_TLB.pas',
  uStopWatch in '..\COSBI\uStopWatch.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'COSBI CPU Speed';
  Application.CreateForm(TfrmCPUSpeed, frmCPUSpeed);
  Application.CreateForm(TForm1, Form1);
  //  Application.CreateForm(TfrmMSR, frmMSR);
//  Application.CreateForm(TfrmCpuSpeedManual, frmCpuSpeedManual);
//  Application.CreateForm(TfrmCpuSpeedLog, frmCpuSpeedLog);
//  Application.CreateForm(TfrmChangeCpuRatio, frmChangeCpuRatio);
  Application.Run;
end.
