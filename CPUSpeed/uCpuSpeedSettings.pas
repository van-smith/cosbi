unit uCpuSpeedSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    ckbxAuto: TCheckBox;
    cboxStayOnTop: TCheckBox;
    cboxAlpha: TCheckBox;
    Label6: TLabel;
    tbTime: TTrackBar;
    lblUpdatePeriod: TLabel;
    Label10: TLabel;
    TrackBarAlpha: TTrackBar;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
