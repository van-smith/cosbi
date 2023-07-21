object frmMsrTest: TfrmMsrTest
  Left = 0
  Top = 0
  AlphaBlend = True
  AlphaBlendValue = 220
  BorderStyle = bsSizeToolWin
  Caption = 'MSR Test'
  ClientHeight = 695
  ClientWidth = 545
  Color = clBtnFace
  TransparentColor = True
  TransparentColorValue = clBtnFace
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
  object SpeedButton1: TSpeedButton
    Left = 368
    Top = 320
    Width = 65
    Height = 25
    Caption = 'Read MSR'
    Flat = True
    OnClick = SpeedButton1Click
  end
  object Gauge1: TGauge
    Left = 72
    Top = 408
    Width = 185
    Height = 17
    Progress = 0
  end
  object MemoOutput: TMemo
    Left = 8
    Top = 488
    Width = 529
    Height = 201
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -10
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object CheckBoxStayOnTop: TCheckBox
    Left = 0
    Top = 464
    Width = 81
    Height = 17
    Caption = 'Stay On Top'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = CheckBoxStayOnTopClick
  end
  object TrackBarAlpha: TTrackBar
    Left = 192
    Top = 456
    Width = 329
    Height = 25
    Max = 255
    Frequency = 32
    TabOrder = 2
    OnChange = TrackBarAlphaChange
  end
  object LabeledEditMSR: TLabeledEdit
    Left = 72
    Top = 320
    Width = 97
    Height = 21
    EditLabel.Width = 51
    EditLabel.Height = 13
    EditLabel.Caption = 'MSR (Hex)'
    LabelPosition = lpLeft
    TabOrder = 3
  end
  object LabeledEdit1: TLabeledEdit
    Left = 232
    Top = 320
    Width = 121
    Height = 21
    EditLabel.Width = 50
    EditLabel.Height = 13
    EditLabel.Caption = 'MSR Value'
    LabelPosition = lpLeft
    TabOrder = 4
  end
  object Button1: TButton
    Left = 368
    Top = 384
    Width = 65
    Height = 25
    Caption = 'Button1'
    TabOrder = 5
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 48
    Top = 360
    Width = 209
    Height = 33
    TabOrder = 6
  end
  object Button2: TButton
    Left = 368
    Top = 416
    Width = 65
    Height = 25
    Caption = 'GetMaxVcc'
    TabOrder = 7
    OnClick = Button2Click
  end
  object ChartClockSpeed: TChart
    Left = 8
    Top = 0
    Width = 289
    Height = 177
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Legend.Visible = False
    Title.Text.Strings = (
      'TChart')
    View3D = False
    Color = clWhite
    TabOrder = 8
    object Series1: TLineSeries
      Marks.Callout.Brush.Color = clBlack
      Marks.Visible = False
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  object TimerCharts: TTimer
    Enabled = False
    OnTimer = TimerChartsTimer
    Left = 24
    Top = 200
  end
end
