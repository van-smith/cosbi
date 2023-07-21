object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 385
  ClientWidth = 564
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Get DLL Version'
    TabOrder = 0
    OnClick = Button1Click
  end
  object memoStatus: TMemo
    Left = 124
    Top = 8
    Width = 432
    Height = 361
    TabOrder = 1
  end
  object Button2: TButton
    Left = 8
    Top = 39
    Width = 105
    Height = 25
    Caption = 'Get TSC'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 70
    Width = 105
    Height = 25
    Caption = 'Read MSR 0x198'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 8
    Top = 101
    Width = 105
    Height = 25
    Caption = 'Read MSR 0x1203'
    TabOrder = 4
    OnClick = Button4Click
  end
end
