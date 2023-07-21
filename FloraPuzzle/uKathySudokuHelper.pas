unit uKathySudokuHelper;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TCellValue = set of 1..9;
  // TSudokuArray =  array[1..9, 1..9] of TCellValue;

  TKathySudokuHelper = class(TObject)
  private
    fOnProcessPassComplete : TNotifyEvent;
    fValueMatrix : array[1..9, 1..9] of TCellValue;
    fValueMatrixChanged : Boolean;
    function GetValueMatrixValue( column, row : integer ): TCellValue;
    procedure SetValueMatrixValue( column, row : integer; Value : TCellValue );
  public
    constructor Create;
    destructor Destroy; Override;
    procedure ProcessInput;
    procedure FillArray;
    procedure EmptyArray;
    procedure ReduceValuesThatOccurOnceInRow;
    procedure ReduceValuesThatOccurOnceInColumn;
    procedure ExcludeSingleValuesFromColumn;
    procedure ExcludeSingleValuesFromRow;
    procedure ExcludeSingleValuesFrom3x3Block;
    procedure EliminateValuesFromBlockThatOccurOnlyInColumnInBlockColumn;
    procedure EliminateValuesFromBlockThatOccurOnlyInRowInBlockRow;
    procedure EliminateValueFromRowUniqueToBlockRowInBlock;
    procedure EliminateValueFromColumnUniqueToBlockColumnInBlock;
    procedure ReduceValuesThatOccurOnceInBlock;
    procedure ReduceDoubletPairs;
    property ValueMatrix[ column, row : integer ]: TCellValue
      read GetValueMatrixValue
      write SetValueMatrixValue;
    property OnProcessPassComplete : TNotifyEvent
      read fOnProcessPassComplete
      write fOnProcessPassComplete;
  end;

implementation

constructor TKathySudokuHelper.Create;
begin
  inherited;
end; // constructor

destructor TKathySudokuHelper.Destroy;
begin
  inherited;
end; // destructor TKathySudokuHelper.Destroy;

function TKathySudokuHelper.GetValueMatrixValue( column, row : integer ): TCellValue;
begin
  result := fValueMatrix[column, row];
end;

procedure TKathySudokuHelper.SetValueMatrixValue( column, row : integer; Value : TCellValue );
begin
  fValueMatrix[column, row] := Value;
end;

procedure TKathySudokuHelper.EmptyArray;
var
  row, column : integer;
begin
  for row := 1 to 9 do begin
    for column := 1 to 9 do begin
      fValueMatrix[column, row] := [];
    end; // for
  end; // for
end;

procedure TKathySudokuHelper.FillArray;
var
  row, column : integer;
begin
  for row := 1 to 9 do begin
    for column := 1 to 9 do begin
      fValueMatrix[column, row] := [1,2,3,4,5,6,7,8,9];
    end; // for
  end; // for
end;

procedure TKathySudokuHelper.ExcludeSingleValuesFromColumn;
var
  row, column, value : integer;

  procedure ExcludeValueFromColumn;
  var
    i : integer;
  begin
    for i := 1 to 9 do begin
      if i <> row then begin
        if value in fValueMatrix[column, i] then begin
          fValueMatrix[column, i] := fValueMatrix[column, i] - [value];
          fValueMatrixChanged := TRUE;
        end; // if
      end;
    end;
  end;

begin
  for row := 1 to 9 do begin
    for column := 1 to 9 do begin
      value := 1;
      while value <= 9 do begin
        if value in fValueMatrix[column, row] then begin
          if (fValueMatrix[column, row] - [value] = []) then
          begin
            ExcludeValueFromColumn;
          end; // if
          value := 10;
        end else begin
          value := value + 1;
        end; // if
      end; // for
    end; // for
  end; // for
end;

procedure TKathySudokuHelper.ExcludeSingleValuesFromRow;
var
  row, column, value : integer;

  procedure ExcludeValueFromRow;
  var
    i : integer;
  begin
    for i := 1 to 9 do begin
      if i <> column then begin
        if value in fValueMatrix[i, row] then begin
          fValueMatrix[i, row] := fValueMatrix[i, row] - [value];
          fValueMatrixChanged := TRUE;
        end; // if
      end;
    end;
  end;

begin
  for row := 1 to 9 do begin
    for column := 1 to 9 do begin
      value := 1;
      while value <= 9 do begin
        if value in fValueMatrix[column, row] then begin
          if (fValueMatrix[column, row] - [value] = []) then
          begin
            ExcludeValueFromRow;
          end; // if
          value := 10;
        end else begin
          value := value + 1;
        end; // if
      end; // for
    end; // for
  end; // for
end;

procedure TKathySudokuHelper.ExcludeSingleValuesFrom3x3Block;
var
  row, column, value : integer;

  procedure ExcludeValueFrom3x3block;
  var blockColumn, blockRow,
      blockStartColumn, blockEndColumn,
      blockStartRow, blockEndRow: integer;
  begin
    blockStartColumn := ((column - 1) div 3) * 3 + 1;
    blockEndColumn := blockStartColumn + 2;
    blockStartRow := ((row - 1) div 3) * 3 + 1;
    blockEndRow := blockStartRow + 2;
    for blockColumn := blockStartColumn to blockEndColumn do begin
      for blockRow := blockStartRow to blockEndRow do begin
        if (blockColumn <> column) and (blockRow <> row) then begin
          if value in fValueMatrix[blockColumn, blockRow] then begin
            fValueMatrix[blockColumn, blockRow] := fValueMatrix[blockColumn, blockRow] - [value];
            fValueMatrixChanged := TRUE;
          end; // if
        end;
      end;
    end;
  end;

begin
  for row := 1 to 9 do begin
    for column := 1 to 9 do begin
      value := 1;
      while value <= 9 do begin
        if value in fValueMatrix[column, row] then begin
          if (fValueMatrix[column, row] - [value] = []) then
          begin
            ExcludeValueFrom3x3block;
          end; // if
          value := 10;
        end else begin
          value := value + 1;
        end; // if
      end; // for
    end; // for
  end; // for
end;

procedure TKathySudokuHelper.EliminateValueFromColumnUniqueToBlockColumnInBlock;
var
  row, column, value : integer;
  s : string;

  function ValueIsUniqueToBlockColumn : Boolean;
  var blockColumn, blockRow,
      blockStartColumn, blockEndColumn,
      blockStartRow, blockEndRow: integer;
  begin
    result := TRUE;
    blockStartColumn := ((column - 1) div 3) * 3 + 1;
    blockEndColumn := blockStartColumn + 2;
    blockStartRow := ((row - 1) div 3) * 3 + 1;
    blockEndRow := blockStartRow + 2;
    for blockColumn := blockStartColumn to blockEndColumn do begin
      for blockRow := blockStartRow to blockEndRow do begin
        if (blockColumn <> column) then begin
          if value in fValueMatrix[blockColumn, blockRow] then begin
            result := FALSE;
          end;
        end;
      end;
    end;
  end;

  procedure EliminateElementFromOtherBlocksInSameColumn;
  var blockColumn, blockRow,
      blockStartColumn, blockEndColumn,
      blockStartRow, blockEndRow: integer;
  begin
    //blockStartColumn := ((column - 1) div 3) * 3 + 1;
    //blockEndColumn := blockStartColumn + 2;
    blockStartRow := ((row - 1) div 3) * 3 + 1;
    blockEndRow := blockStartRow + 2;
    //for blockColumn := blockStartColumn to blockEndColumn do begin
      for blockRow := 1 to 9 do begin
        if not (blockRow in [blockStartRow..blockEndRow]) then begin
          if value in fValueMatrix[column, blockRow] then begin
            fValueMatrix[column, blockRow] := fValueMatrix[column, blockRow] - [value];
            fValueMatrixChanged := TRUE;
          end; // if
        end;
      end;
  end;

begin
  for row := 1 to 9 do begin
    for column := 1 to 9 do begin
      for value := 1 to 9 do begin
        if value in fValueMatrix[column, row] then begin
          if ValueIsUniqueToBlockColumn then begin
            // eliminate element from same column in other blocks
            EliminateElementFromOtherBlocksInSameColumn;
          end; // if
        end; // if
      end; // for
    end; // for
  end; // for
end;

procedure TKathySudokuHelper.EliminateValueFromRowUniqueToBlockRowInBlock;
var
  row, column, value : integer;
  s : string;

  function ValueIsUniqueToBlockRow : Boolean;
  var blockColumn, blockRow,
      blockStartColumn, blockEndColumn,
      blockStartRow, blockEndRow: integer;
  begin
    result := TRUE;
    blockStartColumn := ((column - 1) div 3) * 3 + 1;
    blockEndColumn := blockStartColumn + 2;
    blockStartRow := ((row - 1) div 3) * 3 + 1;
    blockEndRow := blockStartRow + 2;
    for blockColumn := blockStartColumn to blockEndColumn do begin
      for blockRow := blockStartRow to blockEndRow do begin
        if (blockRow <> row) then begin
          if value in fValueMatrix[blockColumn, blockRow] then begin
            result := FALSE;
          end;
        end;
      end;
    end;
  end;

  procedure EliminateElementFromOtherBlocksInSameRow;
  var blockColumn, blockRow,
      blockStartColumn, blockEndColumn,
      blockStartRow, blockEndRow: integer;
  begin
    blockStartColumn := ((column - 1) div 3) * 3 + 1;
    blockEndColumn := blockStartColumn + 2;
    for blockColumn := 1 to 9 do begin
      if not (blockColumn in [blockStartColumn..blockEndColumn]) then begin
        if value in fValueMatrix[blockColumn, Row] then begin
          fValueMatrix[blockColumn, Row] := fValueMatrix[blockColumn, Row] - [value];
          fValueMatrixChanged := TRUE;
        end; // if
      end;
    end;
  end;

begin
  for row := 1 to 9 do begin
    for column := 1 to 9 do begin
      for value := 1 to 9 do begin
        if value in fValueMatrix[column, row] then begin
          if ValueIsUniqueToBlockRow then begin
            // eliminate element from same row in other blocks
            EliminateElementFromOtherBlocksInSameRow;
          end; // if
        end; // if
      end; // for
    end; // for
  end; // for
end;

// ReduceValuesThatOccurOnceInRow reduces cells to a single value that contain a
// number that only occurs in that cell within that cell's row.
procedure TKathySudokuHelper.ReduceValuesThatOccurOnceInRow;
var
  row, column, value : integer;
  s : string;
  function ValueIsUniqueInRow : Boolean;
  var blockColumn: integer;
  begin
    result := TRUE;
    for blockColumn := 1 to 9 do begin
      if (blockColumn <> column) then begin
        if value in fValueMatrix[blockColumn, row] then begin
          result := FALSE;
        end;
      end;
    end;
  end;
begin
  for row := 1 to 9 do begin
    for column := 1 to 9 do begin
      for value := 1 to 9 do begin
        if value in fValueMatrix[column, row] then begin
          if fValueMatrix[column, row] - [value] <> [] then begin
            if ValueIsUniqueInRow then begin
              // assign value to element
              fValueMatrix[column, row] := [value];
              fValueMatrixChanged := TRUE;
            end; // if
          end;
        end; // if
      end; // for
    end; // for
  end; // for
end;

// ReduceValuesThatOccurOnceInColumn reduces cells to a single value that contain a
// number that only occurs in that cell within that cell's column.
procedure TKathySudokuHelper.ReduceValuesThatOccurOnceInColumn;
var
  row, column, value : integer;
  s : string;
  function ValueIsUniqueInColumn : Boolean;
  var searchRows: integer;
  begin
    result := TRUE;
    for searchRows := 1 to 9 do begin
      if (searchRows <> row) then begin
        if value in fValueMatrix[column, searchRows] then begin
          result := FALSE;
        end;
      end;
    end;
  end;
begin
  for row := 1 to 9 do begin
    for column := 1 to 9 do begin
      value := 1;
      while value <= 9 do begin
        // is the value in the cell?
        if value in fValueMatrix[column, row] then begin
          // if the cell is already reduced to this number then move on:
          if fValueMatrix[column, row] - [value] = [] then begin
            // cell is already reduced so move on to next cell:
            value := 10;
          end else begin
            // if the value only occurs once in the column:
            if ValueIsUniqueInColumn then begin
              // assign value to element
              fValueMatrix[column, row] := [value];
              fValueMatrixChanged := TRUE;
            end; // if
          end; // if
        end;
        value := value + 1;
      end; // while
    end; // for
  end; // for
end;

// ReduceValuesThatOccurOnceInBlock reduces cells to a single value that contain a
// number that only occurs in that cell within that cell's 3x3 block.
procedure TKathySudokuHelper.ReduceValuesThatOccurOnceInBlock;
var
  row, column, value : integer;
  s : string;
  function ValueIsUniqueIn3x3Block : Boolean;
  var blockColumn, blockRow,
      blockStartColumn, blockEndColumn,
      blockStartRow, blockEndRow: integer;
  begin
    result := TRUE;
    blockStartColumn := ((column - 1) div 3) * 3 + 1;
    blockEndColumn := blockStartColumn + 2;
    blockStartRow := ((row - 1) div 3) * 3 + 1;
    blockEndRow := blockStartRow + 2;
    for blockColumn := blockStartColumn to blockEndColumn do begin
      for blockRow := blockStartRow to blockEndRow do begin
        if (blockColumn <> column) or (blockRow <> row) then begin
          if value in fValueMatrix[blockColumn, blockRow] then begin
            result := FALSE;
          end;
        end;
      end;
    end;
  end;
begin
  for row := 1 to 9 do begin
    for column := 1 to 9 do begin
      for value := 1 to 9 do begin
        if value in fValueMatrix[column, row] then begin
          if ValueIsUniqueIn3x3Block then begin
            // assign value to element if it is not already set
            if fValueMatrix[column, row] - [value] <> [] then begin;
              fValueMatrix[column, row] := [value];
              fValueMatrixChanged := TRUE;
            end; // if
          end; // if
        end; // if
      end; // for
    end; // for
  end; // for
end;

// EliminateValuesFromBlockThatOccurOnlyInColumnInBlockColumn culls values from a
// block that occur only in that block's column within a column.
procedure TKathySudokuHelper.EliminateValuesFromBlockThatOccurOnlyInColumnInBlockColumn;
var
  row, column, value : integer;
  s : string;

  function ValueIsUniqueInColumnOutsideBlock : Boolean;
  var searchRows: integer;
      blockStartRow, blockEndRow: integer;
  begin
    result := TRUE;
    blockStartRow := ((row - 1) div 3) * 3 + 1;
    blockEndRow := blockStartRow + 2;
    for searchRows := 1 to 9 do begin
      if (searchRows < blockStartRow) or (searchRows > blockEndRow) then begin
        if value in fValueMatrix[column, searchRows] then begin
          result := FALSE;
        end;
      end;
    end;
  end;

  procedure ExcludeValueFromBlockOutsideColumn;
  var blockColumn, blockRow,
      blockStartColumn, blockEndColumn,
      blockStartRow, blockEndRow: integer;
  begin
    blockStartColumn := ((column - 1) div 3) * 3 + 1;
    blockEndColumn := blockStartColumn + 2;
    blockStartRow := ((row - 1) div 3) * 3 + 1;
    blockEndRow := blockStartRow + 2;
    for blockColumn := blockStartColumn to blockEndColumn do begin
      for blockRow := blockStartRow to blockEndRow do begin
        if (blockColumn <> column) then begin
          if value in fValueMatrix[blockColumn, blockRow] then begin
            fValueMatrix[blockColumn, blockRow] := fValueMatrix[blockColumn, blockRow] - [value];
            fValueMatrixChanged := TRUE;
          end; // if
        end;
      end;
    end;
  end;

begin
  for row := 1 to 9 do begin
    for column := 1 to 9 do begin
      value := 1;
      while value <= 9 do begin
        // is the value in the cell?
        if value in fValueMatrix[column, row] then begin
          // if the cell is already reduced to this number then move on:
          if fValueMatrix[column, row] - [value] = [] then begin
            // cell is already reduced so move on to next cell:
            value := 10;
          end else begin
            // if the value only occurs once in the column:
            if ValueIsUniqueInColumnOutsideBlock then begin
              ExcludeValueFromBlockOutsideColumn;
            end; // if
          end; // if
        end;
        value := value + 1;
      end; // while
    end; // for
  end; // for
end;

// EliminateValuesFromBlockThatOccurOnlyInRowInBlockRow culls values from a
// block that occur only in that block's row within a row.
procedure TKathySudokuHelper.EliminateValuesFromBlockThatOccurOnlyInRowInBlockRow;
var
  row, column, value : integer;
  s : string;

  function ValueIsUniqueInRowOutsideBlock : Boolean;
  var searchColumn: integer;
      blockStartColumn, blockEndColumn: integer;
  begin
    result := TRUE;
    blockStartColumn := ((row - 1) div 3) * 3 + 1;
    blockEndColumn := blockStartColumn + 2;
    for searchColumn := 1 to 9 do begin
      if (searchColumn < blockStartColumn) or (searchColumn > blockEndColumn) then begin
        if value in fValueMatrix[searchColumn, row] then begin
          result := FALSE;
        end;
      end;
    end;
  end;

  procedure ExcludeValueFromBlockOutsideRow;
  var blockColumn, blockRow,
      blockStartColumn, blockEndColumn,
      blockStartRow, blockEndRow: integer;
  begin
    blockStartColumn := ((column - 1) div 3) * 3 + 1;
    blockEndColumn := blockStartColumn + 2;
    blockStartRow := ((row - 1) div 3) * 3 + 1;
    blockEndRow := blockStartRow + 2;
    for blockColumn := blockStartColumn to blockEndColumn do begin
      for blockRow := blockStartRow to blockEndRow do begin
        if (blockRow <> row) then begin
          if value in fValueMatrix[blockColumn, blockRow] then begin
            fValueMatrix[blockColumn, blockRow] := fValueMatrix[blockColumn, blockRow] - [value];
            fValueMatrixChanged := TRUE;
          end; // if
        end;
      end;
    end;
  end;

begin
  for row := 1 to 9 do begin
    for column := 1 to 9 do begin
      value := 1;
      while value <= 9 do begin
        // is the value in the cell?
        if value in fValueMatrix[column, row] then begin
          // if the cell is already reduced to this number then move on:
          if fValueMatrix[column, row] - [value] = [] then begin
            // cell is already reduced so move on to next cell:
            value := 10;
          end else begin
            // if the value only occurs once in the column:
            if ValueIsUniqueInRowOutsideBlock then begin
              ExcludeValueFromBlockOutsideRow;
            end; // if
          end; // if
        end;
        value := value + 1;
      end; // while
    end; // for
  end; // for
end;

// ReduceDoubletPairs searches for matched pairs of numbers in rows, columns
// and blocks and reduces them.
procedure TKathySudokuHelper.ReduceDoubletPairs;
var
  row1, column1, value1,
  row2, column2, value2 : integer;
  s : string;
  valuePair : TCellValue;

//  function ValueIsUniqueInRowOutsideBlock : Boolean;
//  var searchColumn: integer;
//      blockStartColumn, blockEndColumn: integer;
//  begin
//    result := TRUE;
//    blockStartColumn := ((row - 1) div 3) * 3 + 1;
//    blockEndColumn := blockStartColumn + 2;
//    for searchColumn := 1 to 9 do begin
//      if (searchColumn < blockStartColumn) or (searchColumn > blockEndColumn) then begin
//        if value in fValueMatrix[searchColumn, row] then begin
//          result := FALSE;
//        end;
//      end;
//    end;
//  end;
//
//  procedure ExcludeValueFromBlockOutsideRow;
//  var blockColumn, blockRow,
//      blockStartColumn, blockEndColumn,
//      blockStartRow, blockEndRow: integer;
//  begin
//    blockStartColumn := ((column - 1) div 3) * 3 + 1;
//    blockEndColumn := blockStartColumn + 2;
//    blockStartRow := ((row - 1) div 3) * 3 + 1;
//    blockEndRow := blockStartRow + 2;
//    for blockColumn := blockStartColumn to blockEndColumn do begin
//      for blockRow := blockStartRow to blockEndRow do begin
//        if (blockRow <> row) then begin
//          if value in fValueMatrix[blockColumn, blockRow] then begin
//            fValueMatrix[blockColumn, blockRow] := fValueMatrix[blockColumn, blockRow] - [value];
//            fValueMatrixChanged := TRUE;
//          end; // if
//        end;
//      end;
//    end;
//  end;

  function ValuePairMateInRow : Boolean;
  var
    iterateColumn, numberOfPairs : integer;
  begin
    numberOfPairs := 1;
    for iterateColumn := 1 to 9 do begin
      if ((fValueMatrix[iterateColumn, row1] + ValuePair) = fValueMatrix[iterateColumn, row1])
          and (iterateColumn <> column1) then begin
        numberOfPairs := numberOfPairs + 1;
        column2 := iterateColumn;
        row2 := row1;
      end; // if
    end; // for
    result := (numberOfPairs = 2);
  end;

  function ValuePairMateInColumn : Boolean;
  var
    iterateRow, numberOfPairs : integer;
  begin
    numberOfPairs := 1;
    for iterateRow := 1 to 9 do begin
      if ((fValueMatrix[column1, iterateRow] + ValuePair) = fValueMatrix[column1, iterateRow])
          and (iterateRow <> row1) then begin
        numberOfPairs := numberOfPairs + 1;
        column2 := column1;
        row2 := iterateRow;
      end; // if
    end; // for
    result := (numberOfPairs = 2);
  end;

  procedure ReduceValuePair;
  begin
    fValueMatrix[column1, row1] := [value1, value2];
    fValueMatrix[column2, row2] := [value1, value2];
  end; // procedure ReduceValuePair;

  procedure EliminateValuePairFromRow;
  var
    iterateColumn : integer;
  begin
    for iterateColumn := 1 to 9 do begin
      if ( iterateColumn <> column1 ) and (iterateColumn <> column2) then begin
        fValueMatrix[iterateColumn, row1] := fValueMatrix[iterateColumn, row1] - [value1, value2];
      end; // if
    end; // for
  end; // procedure EliminateValuePairFromRow;

  procedure EliminateValuePairFromColumn;
  var
    iterateRow : integer;
  begin
    for iterateRow := 1 to 9 do begin
      if ( iterateRow <> row1 ) and (iterateRow <> row2) then begin
        fValueMatrix[column1, iterateRow] := fValueMatrix[column1, iterateRow] - [value1, value2];
      end; // if
    end; // for
  end; // procedure EliminateValuePairFromRow;

begin
  for row1 := 1 to 9 do begin
    for column1 := 1 to 9 do begin
      value1 := 1;
      while value1 <= 9 do begin
        // is value1 in the cell?
        if value1 in fValueMatrix[column1, row1] then begin
          // find a pair
          for value2 := (value1 + 1) to 9 do begin
            // find a match for the pair
            if value2 in fValueMatrix[column1, row1] then begin
              ValuePair := [value1, value2];
              // search for value pair mate in row
              if ValuePairMateInRow then begin
                ReduceValuePair;
                EliminateValuePairFromRow;
              end; // if
              if ValuePairMateInColumn then begin;
                ReduceValuePair;
                EliminateValuePairFromColumn;
              end; // if
//              if ValuePairMateInBlock then begin
//                ReduceValuePair;
//                EliminateValuePairInBlock;
//              end; // if
            end; // if
          end; // for
        end; // if
        value1 := value1 + 1;
      end; // while
    end; // for
  end; // for
end;

procedure TKathySudokuHelper.ProcessInput;
begin
  fValueMatrixChanged := TRUE;
  while fValueMatrixChanged do begin
    fValueMatrixChanged := FALSE;
    ExcludeSingleValuesFromColumn;
    ExcludeSingleValuesFromRow;
    ExcludeSingleValuesFrom3x3Block;
    EliminateValueFromRowUniqueToBlockRowInBlock;
    EliminateValueFromColumnUniqueToBlockColumnInBlock;
    EliminateValuesFromBlockThatOccurOnlyInColumnInBlockColumn;
    EliminateValuesFromBlockThatOccurOnlyInRowInBlockRow;
    ReduceValuesThatOccurOnceInRow;
    ReduceValuesThatOccurOnceInColumn;
    ReduceValuesThatOccurOnceInBlock;
    if assigned( fOnProcessPassComplete ) then begin
      fOnProcessPassComplete( self );
      Application.ProcessMessages;
    end;
  end; // while;
    //ReduceDoubletPairs;
    if assigned( fOnProcessPassComplete ) then begin
      fOnProcessPassComplete( self );
      Application.ProcessMessages;
    end;
end;

end.
