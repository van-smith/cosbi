object frmOSMarkOptions: TfrmOSMarkOptions
  Left = 0
  Top = 0
  AlphaBlend = True
  AlphaBlendValue = 1
  Caption = 'OpenSourceMark Run Options'
  ClientHeight = 394
  ClientWidth = 268
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 22
    Top = 172
    Width = 59
    Height = 14
    Caption = 'Drive target:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object SpeedButtonOk: TSpeedButton
    Left = 56
    Top = 348
    Width = 65
    Height = 25
    Caption = 'O&K'
    Flat = True
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
      555555555555555555555555555555555555555555FF55555555555559055555
      55555555577FF5555555555599905555555555557777F5555555555599905555
      555555557777FF5555555559999905555555555777777F555555559999990555
      5555557777777FF5555557990599905555555777757777F55555790555599055
      55557775555777FF5555555555599905555555555557777F5555555555559905
      555555555555777FF5555555555559905555555555555777FF55555555555579
      05555555555555777FF5555555555557905555555555555777FF555555555555
      5990555555555555577755555555555555555555555555555555}
    NumGlyphs = 2
    OnClick = SpeedButtonOkClick
  end
  object SpeedButtonCancel: TSpeedButton
    Left = 128
    Top = 348
    Width = 73
    Height = 25
    Caption = 'Cancel'
    Flat = True
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333FFFFF3333333333999993333333333F77777FFF333333999999999
      3333333777333777FF3333993333339993333377FF3333377FF3399993333339
      993337777FF3333377F3393999333333993337F777FF333337FF993399933333
      399377F3777FF333377F993339993333399377F33777FF33377F993333999333
      399377F333777FF3377F993333399933399377F3333777FF377F993333339993
      399377FF3333777FF7733993333339993933373FF3333777F7F3399933333399
      99333773FF3333777733339993333339933333773FFFFFF77333333999999999
      3333333777333777333333333999993333333333377777333333}
    NumGlyphs = 2
    OnClick = SpeedButtonCancelClick
  end
  object GroupBox1: TGroupBox
    Left = 18
    Top = 20
    Width = 231
    Height = 137
    Caption = 'Run Options'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object cboxSpinUp: TCheckBox
      Left = 12
      Top = 20
      Width = 189
      Height = 27
      Caption = '"Spin up" CPU before each test'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      WordWrap = True
    end
    object cboxStatus: TCheckBox
      Left = 12
      Top = 51
      Width = 213
      Height = 22
      Caption = 'Show status window between tests'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 1
      WordWrap = True
    end
    object cboxProcessIdleTasks: TCheckBox
      Left = 12
      Top = 104
      Width = 116
      Height = 17
      Caption = 'Process Idle Tasks'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object cboxDefragDrive: TCheckBox
      Left = 12
      Top = 78
      Width = 113
      Height = 17
      Caption = 'Defragment drive'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
  end
  object DriveComboBoxTarget: TDriveComboBox
    Left = 86
    Top = 169
    Width = 137
    Height = 20
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object Timer: TGroupBox
    Left = 14
    Top = 246
    Width = 235
    Height = 41
    Caption = 'Timer Resolution'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    object radioLow: TRadioButton
      Left = 13
      Top = 16
      Width = 42
      Height = 17
      Caption = 'Low'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object radioHigh: TRadioButton
      Left = 94
      Top = 16
      Width = 46
      Height = 17
      Caption = 'High'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      TabStop = True
    end
    object radioCPU: TRadioButton
      Left = 184
      Top = 16
      Width = 44
      Height = 17
      Caption = 'CPU'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
  end
  object GroupBox_Priority: TGroupBox
    Left = 14
    Top = 200
    Width = 235
    Height = 41
    Caption = 'Program Priority'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    object radioNormalP: TRadioButton
      Left = 13
      Top = 16
      Width = 60
      Height = 17
      Caption = 'Normal'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      TabStop = True
    end
    object radioHighP: TRadioButton
      Left = 95
      Top = 16
      Width = 47
      Height = 17
      Caption = 'High'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object radioRealtimeP: TRadioButton
      Left = 157
      Top = 16
      Width = 68
      Height = 17
      Caption = 'Real-Time'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
  end
  object GroupBoxThreadCount: TGroupBox
    Left = 14
    Top = 292
    Width = 235
    Height = 41
    Caption = 'Thread Count'
    TabOrder = 4
    object RadioAuto: TRadioButton
      Left = 13
      Top = 16
      Width = 49
      Height = 17
      Caption = 'Auto'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object Radio1: TRadioButton
      Left = 69
      Top = 16
      Width = 25
      Height = 17
      Caption = '1'
      TabOrder = 1
    end
    object Radio8: TRadioButton
      Left = 189
      Top = 16
      Width = 25
      Height = 17
      Caption = '8'
      TabOrder = 2
    end
    object Radio4: TRadioButton
      Left = 149
      Top = 16
      Width = 25
      Height = 17
      Caption = '4'
      TabOrder = 3
    end
    object Radio2: TRadioButton
      Left = 109
      Top = 16
      Width = 25
      Height = 17
      Caption = '2'
      TabOrder = 4
    end
  end
end
