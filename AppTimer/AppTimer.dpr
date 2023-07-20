program AppTimer;

uses
  Forms,
  uAppTimer in 'uAppTimer.pas' {frmTimeApp},
  COSBI_Common in '..\COSBI\COSBI_Common.pas',
  uCOSBI_SystemInfo in '..\COSBI\uCOSBI_SystemInfo.pas',
  CosbiCpuid in '..\COSBI\CosbiCpuid.pas',
  COSBI_Status in '..\COSBI\COSBI_Status.pas' {frmStatus};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmTimeApp, frmTimeApp);
  Application.Run;
end.
