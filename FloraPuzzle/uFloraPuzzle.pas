unit uFloraPuzzle;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, uKathySudokuHelper;

type
  TfrmKathySudoku = class(TForm)
    StringGridOutput: TStringGrid;
    btnInitialize: TButton;
    StringGridInput: TStringGrid;
    btnProcessInput: TButton;
    btnClearPuzzle: TButton;
    procedure StringGridInputClick(Sender: TObject);
    procedure StringGridInputKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnClearPuzzleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnProcessInputClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure btnInitializeClick(Sender: TObject);
  private
    { Private declarations }
    fKathySudokuHelper : TKathySudokuHelper;
    procedure ProcessInput;
    procedure LinkEventHandlers;
    procedure UpdateOutputGrid;
    procedure OnProcessPassComplete( Sender: TObject);
  public
    { Public declarations }
  end;

var
  frmKathySudoku: TfrmKathySudoku;

implementation

{$R *.dfm}

const
  VERSION = ' Version 0.5';

procedure TfrmKathySudoku.LinkEventHandlers;
begin
  fKathySudokuHelper.OnProcessPassComplete := OnProcessPassComplete;
end; // procedure TfrmKathySudoku.LinkEventHandlers;

procedure TfrmKathySudoku.OnProcessPassComplete( Sender: TObject );
begin
  UpdateOutputGrid;
end;

procedure TfrmKathySudoku.UpdateOutputGrid;
var
  row, column, i : integer;
  s : string;
begin
  for row := 1 to 9 do begin
    for column := 1 to 9 do begin
      // build string:
      s := '[';
      for i := 1 to 9 do begin
        if i in fKathySudokuHelper.ValueMatrix[row, column] then begin
          s := s + intToStr( i );
        end;
      end;
      s := s + ']';
      StringGridOutput.Cells[row - 1, column - 1] := s;
    end; // for
  end; // for
end;

procedure TfrmKathySudoku.btnInitializeClick(Sender: TObject);
begin
  fKathySudokuHelper.FillArray;
  UpdateOutputGrid;
end;

procedure TfrmKathySudoku.btnUpdateClick(Sender: TObject);
begin
  UpdateOutputGrid;
end;

procedure TfrmKathySudoku.ProcessInput;
var
  row, column, value : integer;
  s : string;
begin
  for row := 1 to 9 do begin
    for column := 1 to 9 do begin
      s := StringGridInput.Cells[column - 1, row - 1];
      if s <> '' then begin
        value := StrToInt( s );
        if (value >= 1) and (value <= 9) then begin
          fKathySudokuHelper.ValueMatrix[column, row] := [value];
        end else begin
          StringGridOutput.Cells[column - 1, row - 1] := '';
        end;
      end;
    end; // for
  end; // for
end;

procedure TfrmKathySudoku.btnProcessInputClick(Sender: TObject);
begin
  fKathySudokuHelper.FillArray;
  ProcessInput;
  fKathySudokuHelper.ProcessInput;
end;

procedure TfrmKathySudoku.FormCreate(Sender: TObject);
begin
  Caption := Caption + VERSION;
  fKathySudokuHelper := TKathySudokuHelper.Create;
  LinkEventHandlers;
  fKathySudokuHelper.FillArray;
  UpdateOutputGrid;
end;

procedure TfrmKathySudoku.btnClearPuzzleClick(Sender: TObject);
var
  row, column : integer;
begin
  for row := 1 to 9 do begin
    for column := 1 to 9 do begin
      StringGridInput.Cells[column - 1, row - 1] := '';
    end;
  end;
  fKathySudokuHelper.FillArray;
  UpdateOutputGrid;
end;

procedure TfrmKathySudoku.StringGridInputKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  fKathySudokuHelper.FillArray;
  ProcessInput;
  fKathySudokuHelper.ProcessInput;
  StringGridOutput.Col := StringGridInput.Col;
  StringGridOutput.Row := StringGridInput.Row;
end;

procedure TfrmKathySudoku.StringGridInputClick(Sender: TObject);
begin
  StringGridOutput.Col := StringGridInput.Col;
  StringGridOutput.Row := StringGridInput.Row;
end;

end.
