unit uOEM;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, jpeg, ExtCtrls, Buttons, Spin;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Memo1: TMemo;
    BitBtnHalt: TBitBtn;
    btnProjectName: TButton;
    btnProjectDir: TButton;
    btnTargetIP: TButton;
    Timer1: TTimer;
    BitBtnGo: TBitBtn;
    btnWorkload: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    MemoTooltip: TMemo;
    SpinEdit1: TSpinEdit;
    Label1: TLabel;
    procedure BitBtnHaltClick(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure BitBtnGoClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Memo1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure btnWorkloadMouseEnter(Sender: TObject);
    procedure BitBtnHaltMouseEnter(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BitBtnGoClick(Sender: TObject);
begin
  btnProjectName.Enabled := FALSE;
  btnProjectDir.Enabled := FALSE;
  btnTargetIP.Enabled := FALSE;
  btnWorkload.Enabled := FALSE;
  BitBtnHalt.Visible := TRUE;
  BitBtnGo.Visible := FALSE;
end;

procedure TForm1.BitBtnHaltClick(Sender: TObject);
begin
  btnProjectName.Enabled := TRUE;
  btnProjectDir.Enabled := TRUE;
  btnTargetIP.Enabled := TRUE;
  btnWorkload.Enabled := TRUE;
  BitBtnHalt.Visible := FALSE;
  BitBtnGo.Visible := TRUE;
end;

procedure TForm1.BitBtnHaltMouseEnter(Sender: TObject);
begin
  MemoToolTip.Visible := FALSE;
  MemoToolTip.Text := 'Clicking the Halt button will halt the workload and enable the project buttons. It also transforms into the Go button (try it).';
  MemoToolTip.Left := BitBtnHalt.Left - 120;
  MemoTooltip.Top := BitBtnHalt.Top + BitBtnHalt.Height + 10;
  MemoToolTip.Visible := TRUE;
end;

procedure TForm1.btnWorkloadMouseEnter(Sender: TObject);
begin
  MemoToolTip.Visible := FALSE;
  MemoToolTip.Text := 'The user can select predefined workloads as well as create custom workloads. This means that we will need to create self-sufficient scripts.';
  MemoToolTip.Left := btnWorkload.Left + 10;
  MemoTooltip.Top := btnWorkload.Top + btnWorkload.Height + 10;
  MemoToolTip.Visible := TRUE;
end;

procedure TForm1.FormClick(Sender: TObject);
begin
  memo1.SelStart := memo1.GetTextLen;
  memo1.SelLength := 0;

end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  MemoToolTip.Visible := FALSE;
  MemoToolTip.Text := 'The chart should implement dynamic zoom allowing the user to draw a rubber box around the area to be magnified.';
  MemoToolTip.Left := X - 10;
  MemoTooltip.Top := Image1.Top + Y - 10;
  MemoToolTip.Visible := TRUE;
end;

procedure TForm1.Memo1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  MemoToolTip.Visible := FALSE;
  MemoToolTip.Text := 'The log window should reflect everything sent to the log file.';
  MemoToolTip.Left := Memo1.Left + X - 10;
  MemoTooltip.Top := Memo1.Top + Y - 10;
  MemoToolTip.Visible := TRUE;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  memo1.SelStart := memo1.GetTextLen;
  memo1.SelLength := 0;
  Timer1.Enabled := FALSE;
end;

end.
