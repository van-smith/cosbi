object frmStatus: TfrmStatus
  Left = 788
  Top = 213
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'frmStatus'
  ClientHeight = 298
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnKeyPress = memoStatusKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object memoStatus: TMemo
    Left = 0
    Top = 0
    Width = 460
    Height = 298
    Align = alClient
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clYellow
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    Lines.Strings = (
      'sdfdsfdsf')
    ParentFont = False
    TabOrder = 0
    OnKeyPress = memoStatusKeyPress
  end
end
