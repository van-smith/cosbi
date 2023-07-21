program ThrdDemo;

uses
  Forms,
  ThSort in 'ThSort.pas' {ThreadSortForm},
  SortThds in 'SortThds.pas',
  uRunSorts in 'uRunSorts.pas',
  COSBI_Common in '..\COSBI\COSBI_Common.pas';

{$R *.RES}

begin
  Application.CreateForm(TThreadSortForm, ThreadSortForm);
  Application.Run;
end.
