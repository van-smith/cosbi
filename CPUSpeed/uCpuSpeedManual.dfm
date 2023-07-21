object frmCpuSpeedManual: TfrmCpuSpeedManual
  Left = 0
  Top = 0
  Width = 434
  Height = 257
  Caption = 'Manual Cpu Speed'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlGetCpuSpeed: TPanel
    Left = 0
    Top = 0
    Width = 426
    Height = 230
    Align = alBottom
    TabOrder = 0
    Visible = False
    object Label1: TLabel
      Left = 8
      Top = 24
      Width = 363
      Height = 52
      Caption = 
        'Note: As with nearly every other program of this type, "CPU Spee' +
        'd" uses the system clock and the CPU counter.  If the input cloc' +
        'k signal is not correctly calibrated to 14.318 MHz, the values g' +
        'enerated from this program will be correspondingly in error.'
      WordWrap = True
    end
    object Label3: TLabel
      Left = 8
      Top = 80
      Width = 352
      Height = 26
      Caption = 
        'Indications that this might be a problem is if the system clock ' +
        'either gains of loses time.'
      WordWrap = True
    end
    object Label4: TLabel
      Left = 8
      Top = 111
      Width = 350
      Height = 52
      Caption = 
        'If the system clock is unreliable, a simple way to get a more ce' +
        'rtain estimate of true CPU clock speed is to use the button belo' +
        'w along with a handheld stopwatch to measure the CPU count and t' +
        'hen divide by elapsed time in seconds.'
      WordWrap = True
    end
    object Label5: TLabel
      Left = 3
      Top = 170
      Width = 252
      Height = 26
      Caption = 
        'This button will only be enabled if realtime updating is uncheck' +
        'ed! The button is set to default.'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      WordWrap = True
    end
    object btnGetCPUSpeed: TButton
      Left = 9
      Top = 5
      Width = 89
      Height = 18
      Caption = 'Get CPU Speed'
      TabOrder = 0
    end
    object btnCounter: TButton
      Left = 5
      Top = 201
      Width = 65
      Height = 25
      Caption = 'Start'
      Default = True
      Enabled = False
      TabOrder = 1
      OnClick = btnCounterClick
    end
    object edTicks: TEdit
      Left = 77
      Top = 203
      Width = 225
      Height = 21
      TabOrder = 2
      Text = 'Ticks'
    end
  end
end
