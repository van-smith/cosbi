program floraPuzzle;

uses
  Forms,
  uFloraPuzzle in 'uFloraPuzzle.pas' {frmKathySudoku},
  uKathySudokuHelper in 'uKathySudokuHelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmKathySudoku, frmKathySudoku);
  Application.Run;
end.
