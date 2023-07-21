object Form1: TForm1
  Left = 998
  Top = 156
  Width = 582
  Height = 119
  Caption = 'DhryWhet'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 104
    Top = 14
    Width = 46
    Height = 13
    Caption = 'Iterations:'
  end
  object Label2: TLabel
    Left = 232
    Top = 14
    Width = 43
    Height = 13
    Caption = 'Min Time'
  end
  object Label3: TLabel
    Left = 104
    Top = 52
    Width = 46
    Height = 13
    Caption = 'Iterations:'
  end
  object Label4: TLabel
    Left = 232
    Top = 54
    Width = 43
    Height = 13
    Caption = 'Min Time'
  end
  object Label5: TLabel
    Left = 392
    Top = 16
    Width = 63
    Height = 13
    Caption = 'Dhrystones/s'
  end
  object Label6: TLabel
    Left = 392
    Top = 52
    Width = 67
    Height = 13
    Caption = 'Whetstones/s'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Run Dhrystone'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 160
    Top = 10
    Width = 65
    Height = 21
    TabOrder = 1
    Text = '10000000'
  end
  object edDTime: TEdit
    Left = 289
    Top = 10
    Width = 89
    Height = 21
    TabOrder = 2
  end
  object btnWhetstone: TButton
    Left = 8
    Top = 48
    Width = 89
    Height = 25
    Caption = 'Run Whetstone'
    TabOrder = 3
    OnClick = btnWhetstoneClick
  end
  object edWIterate: TEdit
    Left = 160
    Top = 50
    Width = 65
    Height = 21
    TabOrder = 4
    Text = '25000'
  end
  object edWTime: TEdit
    Left = 289
    Top = 50
    Width = 89
    Height = 21
    TabOrder = 5
  end
  object edDhrystones: TEdit
    Left = 472
    Top = 10
    Width = 97
    Height = 21
    TabOrder = 6
  end
  object edWhetstones: TEdit
    Left = 472
    Top = 49
    Width = 97
    Height = 21
    TabOrder = 7
  end
end
