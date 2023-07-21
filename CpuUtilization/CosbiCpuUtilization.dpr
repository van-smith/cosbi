program CosbiCpuUtilization;

uses
  Forms,
  uCpuUtilization in 'uCpuUtilization.pas' {frmCpuUtilization},
  ufrmGraph in 'ufrmGraph.pas' {frmGraph},
  COSBI_Common in '..\COSBI\COSBI_Common.pas',
  cxCpu40 in '..\cxcpu40_full\Source Code\cxCpu40.pas',
  adCpuUsage in '..\cxcpu40_full\Source Code\adCpuUsage.pas',
  ufrmLaunchExternalProgram in '..\COSBI\ufrmLaunchExternalProgram.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'COSBI CPU Usage Tracker';
  Application.CreateForm(TfrmCpuUtilization, frmCpuUtilization);
  //  Application.CreateForm(TfrmLaunchExternalProgram, frmLaunchExternalProgram);
  Application.Run;
end.
