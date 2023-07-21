program DhryWhet;

uses
  Forms,
  DhryWhetTest in 'DhryWhetTest.pas' {Form1},
  Dhrystone in 'Dhrystone.pas',
  COSBI_Common in '..\COSBI\COSBI_Common.pas',
  uCOSBI_SystemInfo in '..\COSBI\uCOSBI_SystemInfo.pas',
  Whetstone in 'Whetstone.pas',
  COSBI_Status in '..\COSBI\COSBI_Status.pas' {frmStatus},
  uCOSBI_TTest in '..\COSBI\uCOSBI_TTest.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
