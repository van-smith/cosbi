object frmCosbiMazes: TfrmCosbiMazes
  Left = 1034
  Top = 18
  BorderStyle = bsDialog
  Caption = 'COSBI Mazes'
  ClientHeight = 27
  ClientWidth = 238
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnGo: TButton
    Left = 192
    Top = 4
    Width = 41
    Height = 18
    Caption = 'Go!'
    Default = True
    TabOrder = 0
    OnClick = btnGoClick
  end
  object rbOneThread: TRadioButton
    Left = 7
    Top = 6
    Width = 85
    Height = 17
    Caption = 'One Thread'
    TabOrder = 1
  end
  object rbTwoThreads: TRadioButton
    Left = 96
    Top = 7
    Width = 89
    Height = 17
    Caption = 'Two Threads'
    Checked = True
    TabOrder = 2
    TabStop = True
  end
end
