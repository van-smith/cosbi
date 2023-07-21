object frmMSR: TfrmMSR
  Left = 0
  Top = 0
  Caption = 'MSR Tool'
  ClientHeight = 573
  ClientWidth = 402
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
  object Label2: TLabel
    Left = 38
    Top = 21
    Width = 21
    Height = 13
    Caption = 'MSR'
  end
  object Label12: TLabel
    Left = 167
    Top = 21
    Width = 20
    Height = 13
    Caption = 'CPU'
  end
  object Label11: TLabel
    Left = 303
    Top = 21
    Width = 80
    Height = 13
    Caption = '$ = Hexadecimal'
  end
  object Label1: TLabel
    Left = 40
    Top = 115
    Width = 17
    Height = 13
    Caption = 'Bits'
  end
  object Label9: TLabel
    Left = 7
    Top = 76
    Width = 50
    Height = 13
    Caption = 'MSR Value'
  end
  object Bevel1: TBevel
    Left = 0
    Top = 56
    Width = 401
    Height = 4
  end
  object Bevel2: TBevel
    Left = 1
    Top = 512
    Width = 401
    Height = 4
  end
  object edMSR: TEdit
    Left = 71
    Top = 19
    Width = 90
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Text = '$'
    OnKeyDown = edMSRKeyDown
  end
  object edCpu: TEdit
    Left = 191
    Top = 19
    Width = 17
    Height = 21
    TabOrder = 1
    Text = '0'
  end
  object btnReadMSR: TButton
    Left = 217
    Top = 19
    Width = 65
    Height = 22
    Caption = 'Read MSR'
    Default = True
    TabOrder = 2
    OnClick = btnReadMSRClick
  end
  object cbox0: TCheckBox
    Left = 72
    Top = 115
    Width = 40
    Height = 17
    Caption = '0'
    TabOrder = 3
    OnClick = cbox0Click
  end
  object cbox1: TCheckBox
    Left = 72
    Top = 139
    Width = 40
    Height = 17
    Caption = '1'
    TabOrder = 4
    OnClick = cbox0Click
  end
  object cbox2: TCheckBox
    Left = 72
    Top = 163
    Width = 40
    Height = 17
    Caption = '2'
    TabOrder = 5
    OnClick = cbox0Click
  end
  object cbox3: TCheckBox
    Left = 72
    Top = 187
    Width = 40
    Height = 17
    Caption = '3'
    TabOrder = 6
    OnClick = cbox0Click
  end
  object cbox4: TCheckBox
    Left = 72
    Top = 211
    Width = 40
    Height = 17
    Caption = '4'
    TabOrder = 7
    OnClick = cbox0Click
  end
  object cbox5: TCheckBox
    Left = 72
    Top = 235
    Width = 40
    Height = 17
    Caption = '5'
    TabOrder = 8
    OnClick = cbox0Click
  end
  object cbox6: TCheckBox
    Left = 72
    Top = 259
    Width = 40
    Height = 17
    Caption = '6'
    TabOrder = 9
    OnClick = cbox0Click
  end
  object cbox7: TCheckBox
    Left = 72
    Top = 283
    Width = 40
    Height = 17
    Caption = '7'
    TabOrder = 10
    OnClick = cbox0Click
  end
  object cbox8: TCheckBox
    Left = 72
    Top = 307
    Width = 40
    Height = 17
    Caption = '8'
    TabOrder = 11
    OnClick = cbox0Click
  end
  object cbox9: TCheckBox
    Left = 72
    Top = 331
    Width = 40
    Height = 17
    Caption = '9'
    TabOrder = 12
    OnClick = cbox0Click
  end
  object cbox10: TCheckBox
    Left = 72
    Top = 355
    Width = 40
    Height = 17
    Caption = '10'
    TabOrder = 13
    OnClick = cbox0Click
  end
  object cbox11: TCheckBox
    Left = 72
    Top = 379
    Width = 40
    Height = 17
    Caption = '11'
    TabOrder = 14
    OnClick = cbox0Click
  end
  object cbox12: TCheckBox
    Left = 72
    Top = 403
    Width = 40
    Height = 17
    Caption = '12'
    TabOrder = 15
    OnClick = cbox0Click
  end
  object cbox13: TCheckBox
    Left = 72
    Top = 427
    Width = 40
    Height = 17
    Caption = '13'
    TabOrder = 16
    OnClick = cbox0Click
  end
  object cbox14: TCheckBox
    Left = 72
    Top = 451
    Width = 40
    Height = 17
    Caption = '14'
    TabOrder = 17
    OnClick = cbox0Click
  end
  object cbox15: TCheckBox
    Left = 72
    Top = 475
    Width = 40
    Height = 17
    Caption = '15'
    TabOrder = 18
    OnClick = cbox0Click
  end
  object cbox16: TCheckBox
    Left = 132
    Top = 115
    Width = 40
    Height = 17
    Caption = '16'
    TabOrder = 19
    OnClick = cbox0Click
  end
  object cbox17: TCheckBox
    Left = 132
    Top = 139
    Width = 40
    Height = 17
    Caption = '17'
    TabOrder = 20
    OnClick = cbox0Click
  end
  object cbox18: TCheckBox
    Left = 132
    Top = 163
    Width = 40
    Height = 17
    Caption = '18'
    TabOrder = 21
    OnClick = cbox0Click
  end
  object cbox19: TCheckBox
    Left = 132
    Top = 187
    Width = 40
    Height = 17
    Caption = '19'
    TabOrder = 22
    OnClick = cbox0Click
  end
  object cbox20: TCheckBox
    Left = 132
    Top = 211
    Width = 40
    Height = 17
    Caption = '20'
    TabOrder = 23
    OnClick = cbox0Click
  end
  object cbox21: TCheckBox
    Left = 132
    Top = 235
    Width = 40
    Height = 17
    Caption = '21'
    TabOrder = 24
    OnClick = cbox0Click
  end
  object cbox22: TCheckBox
    Left = 132
    Top = 259
    Width = 40
    Height = 17
    Caption = '22'
    TabOrder = 25
    OnClick = cbox0Click
  end
  object cbox23: TCheckBox
    Left = 132
    Top = 283
    Width = 40
    Height = 17
    Caption = '23'
    TabOrder = 26
    OnClick = cbox0Click
  end
  object cbox24: TCheckBox
    Left = 132
    Top = 307
    Width = 40
    Height = 17
    Caption = '24'
    TabOrder = 27
    OnClick = cbox0Click
  end
  object cbox25: TCheckBox
    Left = 132
    Top = 331
    Width = 40
    Height = 17
    Caption = '25'
    TabOrder = 28
    OnClick = cbox0Click
  end
  object cbox26: TCheckBox
    Left = 132
    Top = 355
    Width = 40
    Height = 17
    Caption = '26'
    TabOrder = 29
    OnClick = cbox0Click
  end
  object cbox27: TCheckBox
    Left = 132
    Top = 379
    Width = 40
    Height = 17
    Caption = '27'
    TabOrder = 30
    OnClick = cbox0Click
  end
  object cbox28: TCheckBox
    Left = 132
    Top = 403
    Width = 40
    Height = 17
    Caption = '28'
    TabOrder = 31
    OnClick = cbox0Click
  end
  object cbox29: TCheckBox
    Left = 132
    Top = 427
    Width = 40
    Height = 17
    Caption = '29'
    TabOrder = 32
    OnClick = cbox0Click
  end
  object cbox30: TCheckBox
    Left = 132
    Top = 451
    Width = 40
    Height = 17
    Caption = '30'
    TabOrder = 33
    OnClick = cbox0Click
  end
  object cbox31: TCheckBox
    Left = 132
    Top = 475
    Width = 40
    Height = 17
    Caption = '31'
    TabOrder = 34
    OnClick = cbox0Click
  end
  object edMsrValue: TEdit
    Left = 71
    Top = 74
    Width = 138
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 35
    Text = '$'
    OnChange = edMsrValueChange
  end
  object btnWriteMsr: TButton
    Left = 217
    Top = 75
    Width = 65
    Height = 22
    Caption = 'Write MSR'
    Enabled = False
    TabOrder = 36
    OnClick = btnWriteMsrClick
  end
  object cboxAllowWrites: TCheckBox
    Left = 304
    Top = 76
    Width = 81
    Height = 17
    Caption = 'Allow Writes'
    TabOrder = 37
    OnClick = cboxAllowWritesClick
  end
  object cbox32: TCheckBox
    Left = 245
    Top = 115
    Width = 40
    Height = 17
    Caption = '32'
    TabOrder = 38
    OnClick = cbox0Click
  end
  object cbox33: TCheckBox
    Left = 245
    Top = 139
    Width = 40
    Height = 17
    Caption = '33'
    TabOrder = 39
    OnClick = cbox0Click
  end
  object cbox34: TCheckBox
    Left = 245
    Top = 163
    Width = 40
    Height = 17
    Caption = '34'
    TabOrder = 40
    OnClick = cbox0Click
  end
  object cbox35: TCheckBox
    Left = 245
    Top = 187
    Width = 40
    Height = 17
    Caption = '35'
    TabOrder = 41
    OnClick = cbox0Click
  end
  object cbox36: TCheckBox
    Left = 245
    Top = 211
    Width = 40
    Height = 17
    Caption = '36'
    TabOrder = 42
    OnClick = cbox0Click
  end
  object cbox37: TCheckBox
    Left = 245
    Top = 235
    Width = 40
    Height = 17
    Caption = '37'
    TabOrder = 43
    OnClick = cbox0Click
  end
  object cbox38: TCheckBox
    Left = 245
    Top = 259
    Width = 40
    Height = 17
    Caption = '38'
    TabOrder = 44
    OnClick = cbox0Click
  end
  object cbox39: TCheckBox
    Left = 245
    Top = 283
    Width = 40
    Height = 17
    Caption = '39'
    TabOrder = 45
    OnClick = cbox0Click
  end
  object cbox40: TCheckBox
    Left = 245
    Top = 307
    Width = 40
    Height = 17
    Caption = '40'
    TabOrder = 46
    OnClick = cbox0Click
  end
  object cbox41: TCheckBox
    Left = 245
    Top = 331
    Width = 40
    Height = 17
    Caption = '41'
    TabOrder = 47
    OnClick = cbox0Click
  end
  object cbox42: TCheckBox
    Left = 245
    Top = 355
    Width = 40
    Height = 17
    Caption = '42'
    TabOrder = 48
    OnClick = cbox0Click
  end
  object cbox43: TCheckBox
    Left = 245
    Top = 379
    Width = 40
    Height = 17
    Caption = '43'
    TabOrder = 49
    OnClick = cbox0Click
  end
  object cbox44: TCheckBox
    Left = 245
    Top = 403
    Width = 40
    Height = 17
    Caption = '44'
    TabOrder = 50
    OnClick = cbox0Click
  end
  object cbox45: TCheckBox
    Left = 245
    Top = 427
    Width = 40
    Height = 17
    Caption = '45'
    TabOrder = 51
    OnClick = cbox0Click
  end
  object cbox46: TCheckBox
    Left = 245
    Top = 451
    Width = 40
    Height = 17
    Caption = '46'
    TabOrder = 52
    OnClick = cbox0Click
  end
  object cbox47: TCheckBox
    Left = 245
    Top = 475
    Width = 40
    Height = 17
    Caption = '47'
    TabOrder = 53
    OnClick = cbox0Click
  end
  object cbox48: TCheckBox
    Left = 305
    Top = 115
    Width = 40
    Height = 17
    Caption = '48'
    TabOrder = 54
    OnClick = cbox0Click
  end
  object cbox49: TCheckBox
    Left = 305
    Top = 139
    Width = 40
    Height = 17
    Caption = '49'
    TabOrder = 55
    OnClick = cbox0Click
  end
  object cbox50: TCheckBox
    Left = 305
    Top = 163
    Width = 40
    Height = 17
    Caption = '50'
    TabOrder = 56
    OnClick = cbox0Click
  end
  object cbox51: TCheckBox
    Left = 305
    Top = 187
    Width = 40
    Height = 17
    Caption = '51'
    TabOrder = 57
    OnClick = cbox0Click
  end
  object cbox52: TCheckBox
    Left = 305
    Top = 211
    Width = 40
    Height = 17
    Caption = '52'
    TabOrder = 58
    OnClick = cbox0Click
  end
  object cbox53: TCheckBox
    Left = 305
    Top = 235
    Width = 40
    Height = 17
    Caption = '53'
    TabOrder = 59
    OnClick = cbox0Click
  end
  object cbox54: TCheckBox
    Left = 305
    Top = 259
    Width = 40
    Height = 17
    Caption = '54'
    TabOrder = 60
    OnClick = cbox0Click
  end
  object cbox55: TCheckBox
    Left = 305
    Top = 283
    Width = 40
    Height = 17
    Caption = '55'
    TabOrder = 61
    OnClick = cbox0Click
  end
  object cbox56: TCheckBox
    Left = 305
    Top = 307
    Width = 40
    Height = 17
    Caption = '56'
    TabOrder = 62
    OnClick = cbox0Click
  end
  object cbox57: TCheckBox
    Left = 305
    Top = 331
    Width = 40
    Height = 17
    Caption = '57'
    TabOrder = 63
    OnClick = cbox0Click
  end
  object cbox58: TCheckBox
    Left = 305
    Top = 355
    Width = 40
    Height = 17
    Caption = '58'
    TabOrder = 64
    OnClick = cbox0Click
  end
  object cbox59: TCheckBox
    Left = 305
    Top = 379
    Width = 40
    Height = 17
    Caption = '59'
    TabOrder = 65
    OnClick = cbox0Click
  end
  object cbox60: TCheckBox
    Left = 305
    Top = 403
    Width = 40
    Height = 17
    Caption = '60'
    TabOrder = 66
    OnClick = cbox0Click
  end
  object cbox61: TCheckBox
    Left = 305
    Top = 427
    Width = 40
    Height = 17
    Caption = '61'
    TabOrder = 67
    OnClick = cbox0Click
  end
  object cbox62: TCheckBox
    Left = 305
    Top = 451
    Width = 40
    Height = 17
    Caption = '62'
    TabOrder = 68
    OnClick = cbox0Click
  end
  object cbox63: TCheckBox
    Left = 305
    Top = 475
    Width = 40
    Height = 17
    Caption = '63'
    TabOrder = 69
    OnClick = cbox0Click
  end
  object BitBtnClose: TBitBtn
    Left = 303
    Top = 534
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Close'
    TabOrder = 70
    OnClick = BitBtnCloseClick
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00388888888877
      F7F787F8888888888333333F00004444400888FFF444448888888888F333FF8F
      000033334D5007FFF4333388888888883338888F0000333345D50FFFF4333333
      338F888F3338F33F000033334D5D0FFFF43333333388788F3338F33F00003333
      45D50FEFE4333333338F878F3338F33F000033334D5D0FFFF43333333388788F
      3338F33F0000333345D50FEFE4333333338F878F3338F33F000033334D5D0FFF
      F43333333388788F3338F33F0000333345D50FEFE4333333338F878F3338F33F
      000033334D5D0EFEF43333333388788F3338F33F0000333345D50FEFE4333333
      338F878F3338F33F000033334D5D0EFEF43333333388788F3338F33F00003333
      4444444444333333338F8F8FFFF8F33F00003333333333333333333333888888
      8888333F00003333330000003333333333333FFFFFF3333F00003333330AAAA0
      333333333333888888F3333F00003333330000003333333333338FFFF8F3333F
      0000}
    NumGlyphs = 2
    Style = bsNew
  end
  object btnCustomVendorString: TButton
    Left = 16
    Top = 538
    Width = 153
    Height = 17
    Caption = 'Custom CPUID Vendor String'
    TabOrder = 71
    Visible = False
    OnClick = btnCustomVendorStringClick
  end
  object edVendorString: TEdit
    Left = 175
    Top = 536
    Width = 81
    Height = 21
    MaxLength = 12
    TabOrder = 72
    Visible = False
  end
end
