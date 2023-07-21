object frmOSMarkSelectTests: TfrmOSMarkSelectTests
  Left = 0
  Top = 0
  AlphaBlend = True
  AlphaBlendValue = 1
  BorderIcons = [biHelp]
  Caption = 'OpenSourceMark Select Tests'
  ClientHeight = 590
  ClientWidth = 712
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
  object lblIterations: TLabel
    Left = 30
    Top = 8
    Width = 49
    Height = 16
    AutoSize = False
    Caption = 'Iterations:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object SpeedButtonOk: TSpeedButton
    Left = 498
    Top = 532
    Width = 65
    Height = 25
    Caption = '&Run!'
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
    Left = 570
    Top = 532
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
  object cboIterations: TComboBox
    Left = 86
    Top = 5
    Width = 48
    Height = 22
    Style = csDropDownList
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    TabOrder = 0
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '10'
      '20'
      '30'
      '40'
      '50'
      '100'
      '150'
      '200'
      '250')
  end
  object gbSelectTests: TGroupBox
    Left = 30
    Top = 32
    Width = 645
    Height = 481
    Caption = 'Select Tests (strongly threaded tests are underlined)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Label1: TLabel
      Left = 239
      Top = 20
      Width = 140
      Height = 14
      Caption = '==== Official Run Tests ===='
    end
    object Bevel1: TBevel
      Left = 3
      Top = 335
      Width = 628
      Height = 5
    end
    object Label2: TLabel
      Left = 252
      Top = 346
      Width = 111
      Height = 14
      Caption = '==== Other Tests ===='
    end
    object cboxGridBlast: TCheckBox
      Left = 263
      Top = 101
      Width = 100
      Height = 13
      HelpType = htKeyword
      Caption = 'GridBlast'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 0
    end
    object cboxRandomCircle: TCheckBox
      Left = 13
      Top = 74
      Width = 89
      Height = 13
      Caption = 'Circles'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 8
    end
    object cboxPlotTrig: TCheckBox
      Left = 13
      Top = 209
      Width = 89
      Height = 13
      Caption = 'Plot Trig'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 4
    end
    object cboxPlotLines: TCheckBox
      Left = 513
      Top = 182
      Width = 100
      Height = 13
      Caption = 'Plot Lines'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 6
    end
    object cboxRandDots: TCheckBox
      Left = 388
      Top = 209
      Width = 89
      Height = 13
      Caption = 'Random dots'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 7
    end
    object cboxFib: TCheckBox
      Left = 138
      Top = 388
      Width = 94
      Height = 20
      Caption = 'Fibonacci(40)'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 2
    end
    object cboxNBody: TCheckBox
      Left = 13
      Top = 182
      Width = 72
      Height = 13
      Caption = 'N-Body'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 3
    end
    object cboxMaze: TCheckBox
      Left = 263
      Top = 155
      Width = 117
      Height = 13
      Caption = 'Mazes'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 9
    end
    object cboxTrig2: TCheckBox
      Left = 138
      Top = 209
      Width = 89
      Height = 13
      Caption = 'Plot Trig2'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 5
    end
    object cboxFern: TCheckBox
      Left = 513
      Top = 74
      Width = 89
      Height = 13
      Caption = 'Fern Fractal'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 10
    end
    object cboxRichEd: TCheckBox
      Left = 13
      Top = 236
      Width = 89
      Height = 13
      Caption = 'Rich Edit'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 11
    end
    object cboxCalculatePi: TCheckBox
      Left = 513
      Top = 47
      Width = 100
      Height = 13
      Caption = 'Calculate Pi'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 12
    end
    object cboxDhry: TCheckBox
      Left = 13
      Top = 388
      Width = 94
      Height = 20
      Caption = 'Dhrystone 2.1'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 13
    end
    object cboxWhet: TCheckBox
      Left = 513
      Top = 418
      Width = 120
      Height = 20
      Caption = 'Whetstone'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 14
    end
    object cboxBandwidthBP64: TCheckBox
      Left = 388
      Top = 47
      Width = 100
      Height = 13
      Caption = 'BandwidthBP64'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      State = cbChecked
      TabOrder = 15
    end
    object cboxLatency: TCheckBox
      Left = 388
      Top = 155
      Width = 117
      Height = 13
      Caption = 'MemLatency'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      State = cbChecked
      TabOrder = 16
    end
    object cboxMazeThreads: TCheckBox
      Left = 388
      Top = 388
      Width = 94
      Height = 20
      Caption = 'Maze Threads'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      State = cbChecked
      TabOrder = 17
    end
    object cboxGridBlastFP: TCheckBox
      Left = 388
      Top = 101
      Width = 89
      Height = 13
      Caption = 'GridBlastFP'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 1
    end
    object cboxOThreads: TCheckBox
      Left = 263
      Top = 182
      Width = 100
      Height = 13
      Hint = 'Orthogonal Threads'
      Caption = 'O. Threads'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      State = cbChecked
      TabOrder = 18
    end
    object cboxIThreads: TCheckBox
      Left = 138
      Top = 128
      Width = 89
      Height = 13
      Hint = 'Identical Threads'
      Caption = 'I. Threads'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      State = cbChecked
      TabOrder = 19
    end
    object cboxImageRotate: TCheckBox
      Left = 13
      Top = 128
      Width = 89
      Height = 13
      Caption = 'Image Rotate'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 22
    end
    object cboxJpgDecode: TCheckBox
      Left = 263
      Top = 128
      Width = 89
      Height = 13
      Caption = 'JPG Decode'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 20
    end
    object cboxImageResize: TCheckBox
      Left = 513
      Top = 101
      Width = 100
      Height = 13
      Caption = 'Image Resize'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 21
    end
    object cboxMP3Encode: TCheckBox
      Left = 262
      Top = 292
      Width = 94
      Height = 20
      Caption = 'GoGo Encoder'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      State = cbChecked
      TabOrder = 23
    end
    object cboxWebPageLoad: TCheckBox
      Left = 13
      Top = 263
      Width = 100
      Height = 13
      Caption = 'Web Page Load'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      State = cbChecked
      TabOrder = 24
    end
    object cboxEncryptDecrypt: TCheckBox
      Left = 388
      Top = 74
      Width = 100
      Height = 13
      Caption = 'Encrypt/Decrypt'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      State = cbChecked
      TabOrder = 26
    end
    object cboxZipCompress: TCheckBox
      Left = 263
      Top = 263
      Width = 93
      Height = 13
      Caption = 'Zip Compress'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 25
    end
    object cboxFileCopy: TCheckBox
      Left = 138
      Top = 101
      Width = 81
      Height = 13
      Caption = 'File Copy'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 27
    end
    object cboxLorenz: TCheckBox
      Left = 13
      Top = 155
      Width = 117
      Height = 17
      Caption = 'Lorenz Attractor'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 28
    end
    object cboxNBodyOpenGL: TCheckBox
      Left = 138
      Top = 182
      Width = 100
      Height = 13
      Caption = 'N-Body OpenGL'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 29
    end
    object cboxDhrystoneThreads: TCheckBox
      Left = 138
      Top = 74
      Width = 114
      Height = 13
      Caption = 'Dhrystone threads'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      State = cbChecked
      TabOrder = 30
    end
    object cboxWhetstoneThreads: TCheckBox
      Left = 138
      Top = 263
      Width = 112
      Height = 13
      Caption = 'Whetstone threads'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      State = cbChecked
      TabOrder = 31
    end
    object cboxPiThreads: TCheckBox
      Left = 388
      Top = 182
      Width = 96
      Height = 13
      Caption = 'Pi threads'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      State = cbChecked
      TabOrder = 32
    end
    object cboxMandelbrot: TCheckBox
      Left = 138
      Top = 155
      Width = 88
      Height = 13
      Caption = 'Mandelbrot'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      State = cbChecked
      TabOrder = 33
    end
    object cboxEllipses: TCheckBox
      Left = 263
      Top = 74
      Width = 96
      Height = 13
      Caption = 'Ellipses'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 34
    end
    object cboxRectangles: TCheckBox
      Left = 513
      Top = 209
      Width = 104
      Height = 13
      Caption = 'Rectangles'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 35
    end
    object cboxLines: TCheckBox
      Left = 513
      Top = 128
      Width = 72
      Height = 13
      Caption = 'Lines'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 36
    end
    object cboxFibThreads: TCheckBox
      Left = 13
      Top = 101
      Width = 109
      Height = 13
      Caption = 'Fibonacci threads'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      State = cbChecked
      TabOrder = 37
    end
    object MediaPlayer1: TMediaPlayer
      Left = 408
      Top = -32
      Width = 253
      Height = 30
      TabOrder = 38
    end
    object cboxPngOut: TCheckBox
      Left = 263
      Top = 209
      Width = 94
      Height = 13
      Caption = 'PNGOut'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      State = cbChecked
      TabOrder = 39
    end
    object cbox7zip: TCheckBox
      Left = 13
      Top = 47
      Width = 85
      Height = 13
      Caption = '7zip'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      State = cbChecked
      TabOrder = 40
    end
    object cboxUpx: TCheckBox
      Left = 513
      Top = 236
      Width = 59
      Height = 13
      Caption = 'Upx'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      State = cbChecked
      TabOrder = 41
    end
    object cboxNbodyFPU: TCheckBox
      Left = 513
      Top = 388
      Width = 94
      Height = 20
      Caption = 'nBody FPU'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 42
    end
    object cboxNBodySSE2Scalar: TCheckBox
      Left = 138
      Top = 418
      Width = 120
      Height = 20
      Caption = 'nBody SSE2 Scalar'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 43
    end
    object cboxNBodySSE2: TCheckBox
      Left = 13
      Top = 418
      Width = 120
      Height = 20
      Caption = 'nBody SSE2'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 44
    end
    object cboxNBodySSE3: TCheckBox
      Left = 263
      Top = 418
      Width = 120
      Height = 20
      Caption = 'nBody SSE3'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 45
    end
    object cboxAlphaBlend: TCheckBox
      Left = 138
      Top = 47
      Width = 93
      Height = 13
      Caption = 'Alpha Blend'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 46
    end
    object cboxSSE3: TCheckBox
      Left = 388
      Top = 236
      Width = 56
      Height = 13
      Caption = 'SSE3'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      State = cbChecked
      TabOrder = 47
    end
    object cboxMontMul: TCheckBox
      Left = 513
      Top = 155
      Width = 117
      Height = 13
      Caption = 'MontgomeryMultiplier'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      State = cbChecked
      TabOrder = 48
    end
    object cboxSha256: TCheckBox
      Left = 263
      Top = 236
      Width = 93
      Height = 13
      Caption = 'SHA-256'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      State = cbChecked
      TabOrder = 49
    end
    object cboxSha1: TCheckBox
      Left = 138
      Top = 236
      Width = 93
      Height = 13
      Caption = 'SHA-1'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      State = cbChecked
      TabOrder = 50
    end
    object cboxAlphaDots: TCheckBox
      Left = 263
      Top = 47
      Width = 93
      Height = 13
      Caption = 'Alpha Dots'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 51
    end
    object cboxLame: TCheckBox
      Left = 388
      Top = 128
      Width = 112
      Height = 13
      Caption = 'Lame MP3 Encode'
      Checked = True
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentColor = False
      ParentFont = False
      State = cbChecked
      TabOrder = 52
    end
    object cboxOgg: TCheckBox
      Left = 513
      Top = 292
      Width = 120
      Height = 20
      Caption = 'Ogg Vorbis Encode'
      Checked = True
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentColor = False
      ParentFont = False
      State = cbChecked
      TabOrder = 53
    end
    object cboxCppCompile: TCheckBox
      Left = 388
      Top = 292
      Width = 81
      Height = 20
      Caption = 'C++ Compile'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 54
    end
    object cboxMetaballs: TCheckBox
      Left = 388
      Top = 263
      Width = 117
      Height = 13
      Caption = 'Metaballs (D3D)'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsUnderline]
      ParentFont = False
      State = cbChecked
      TabOrder = 55
    end
  end
  object btnSelectNone: TButton
    Left = 511
    Top = 21
    Width = 72
    Height = 17
    Caption = 'Select &None'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = btnSelectNoneClick
  end
  object btnSelectAll: TButton
    Left = 582
    Top = 21
    Width = 72
    Height = 17
    Caption = 'Select A&ll'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = btnSelectAllClick
  end
end
