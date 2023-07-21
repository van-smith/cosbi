object frmKathySudoku: TfrmKathySudoku
  Left = 0
  Top = 0
  Width = 859
  Height = 569
  Caption = 'Kathy'#39's Sudoku Helper'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object StringGridOutput: TStringGrid
    Left = 8
    Top = 288
    Width = 833
    Height = 241
    TabStop = False
    ColCount = 9
    DefaultColWidth = 90
    FixedCols = 0
    RowCount = 9
    FixedRows = 0
    TabOrder = 0
  end
  object btnInitialize: TButton
    Left = 8
    Top = 592
    Width = 65
    Height = 17
    Caption = 'Initialize'
    TabOrder = 1
    Visible = False
    OnClick = btnInitializeClick
  end
  object StringGridInput: TStringGrid
    Left = 6
    Top = 28
    Width = 835
    Height = 237
    ColCount = 9
    DefaultColWidth = 90
    FixedCols = 0
    RowCount = 9
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    TabOrder = 2
    OnClick = StringGridInputClick
    OnKeyUp = StringGridInputKeyUp
  end
  object btnProcessInput: TButton
    Left = 80
    Top = 6
    Width = 75
    Height = 17
    Caption = 'Process Input'
    TabOrder = 3
    Visible = False
    OnClick = btnProcessInputClick
  end
  object btnClearPuzzle: TButton
    Left = 8
    Top = 5
    Width = 65
    Height = 17
    Caption = 'Clear Puzzle'
    TabOrder = 4
    TabStop = False
    OnClick = btnClearPuzzleClick
  end
end
