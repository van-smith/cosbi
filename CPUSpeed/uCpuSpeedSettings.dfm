object Form1: TForm1
  Left = 0
  Top = 0
  Width = 434
  Height = 320
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel
    Left = 18
    Top = 119
    Width = 104
    Height = 13
    Caption = 'Sample time (0.1-10s):'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lblUpdatePeriod: TLabel
    Left = 208
    Top = 119
    Width = 11
    Height = 13
    Caption = '1s'
  end
  object Label10: TLabel
    Left = 107
    Top = 72
    Width = 57
    Height = 13
    Caption = 'AlphaBlend:'
  end
  object ckbxAuto: TCheckBox
    Left = 21
    Top = 10
    Width = 95
    Height = 17
    Caption = 'Realtime Clock'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object cboxStayOnTop: TCheckBox
    Left = 24
    Top = 42
    Width = 81
    Height = 17
    Caption = 'Stay On Top'
    TabOrder = 1
  end
  object cboxAlpha: TCheckBox
    Left = 19
    Top = 74
    Width = 78
    Height = 17
    Caption = 'AlphaBlend'
    TabOrder = 2
  end
  object tbTime: TTrackBar
    Left = 128
    Top = 117
    Width = 81
    Height = 19
    Max = 100
    Min = 1
    Frequency = 5
    Position = 10
    TabOrder = 3
    ThumbLength = 15
    TickMarks = tmTopLeft
    TickStyle = tsNone
  end
  object TrackBarAlpha: TTrackBar
    Left = 169
    Top = 69
    Width = 97
    Height = 17
    Max = 255
    TabOrder = 4
    ThumbLength = 15
  end
end
