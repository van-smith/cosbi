object frmCpuUtilization: TfrmCpuUtilization
  Left = 470
  Top = 164
  Width = 673
  Height = 560
  Caption = 'COSBI CPU Usage Tool'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIForm
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object MainMenu1: TMainMenu
    Left = 8
    object File1: TMenuItem
      Caption = 'File'
      object Save1: TMenuItem
        Caption = '&Save Data'
        OnClick = Save1Click
      end
      object SaveCharts1: TMenuItem
        Caption = 'Save C&harts'
        OnClick = SaveCharts1Click
      end
    end
    object Actions1: TMenuItem
      Caption = '&Actions'
      object Start1: TMenuItem
        Caption = 'Star&t'
        OnClick = Start1Click
      end
      object Stop1: TMenuItem
        Caption = 'Sto&p'
        OnClick = Stop1Click
      end
      object Clear1: TMenuItem
        Caption = 'C&lear'
        OnClick = Clear1Click
      end
      object LaunchprogramandtrackCPUutilization1: TMenuItem
        Caption = '&Launch external program and track CPU utilization'
        OnClick = LaunchprogramandtrackCPUutilization1Click
      end
      object BrieflydeployartificalCPUload1: TMenuItem
        Caption = 'Briefly deploy artifical CPU load'
        OnClick = BrieflydeployartificalCPUload1Click
      end
    end
    object Arrange1: TMenuItem
      Caption = 'A&rrange'
      object ileVertically1: TMenuItem
        Caption = 'Tile &Vertically'
        OnClick = ileVertically1Click
      end
      object ileHorizontally1: TMenuItem
        Caption = 'Tile &Horizontally'
        OnClick = ileHorizontally1Click
      end
      object Cascade1: TMenuItem
        Caption = 'Cascade'
        OnClick = Cascade1Click
      end
      object ArrangeAll1: TMenuItem
        Caption = 'Arrange All'
        OnClick = ArrangeAll1Click
      end
    end
  end
  object TimerMain: TTimer
    Interval = 100
    OnTimer = TimerMainTimer
    Left = 40
  end
end
