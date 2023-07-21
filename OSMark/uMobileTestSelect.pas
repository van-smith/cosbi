unit uMobileTestSelect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, jpeg, ExtCtrls, COSBI_Common;

type
  TMobileTestType = ( mttNormal, mttMaxPowerDraw, mttLowPowerDraw, mttPCWB5 );

  TfrmMobileTest = class(TForm)
    GroupBoxTestType: TGroupBox;
    RadioButtonNormal: TRadioButton;
    RadioButtonMaxPowerDraw: TRadioButton;
    RadioButtonLowPowerDraw: TRadioButton;
    Image1: TImage;
    Memo1: TMemo;
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    BitBtnSave: TBitBtn;
    SaveDialog1: TSaveDialog;
    RadioButtonPCWB5: TRadioButton;
    ComboBoxPCWB5Iterations: TComboBox;
    procedure RadioButtonLowPowerDrawClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ComboBoxPCWB5IterationsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtnSaveClick(Sender: TObject);
    procedure BitBtnOkClick(Sender: TObject);
    procedure BitBtnCancelClick(Sender: TObject);
  private
    { Private declarations }
    fMobileTestType : TMobileTestType;
    fResultsFileName : string;
    fPCWB5Iterations : integer;
  public
    { Public declarations }
    property MobileTestType : TMobileTestType read fMobileTestType;
    property ResultsFileName : string read fResultsFileName;
    property PCWB5Iterations : integer read fPCWB5Iterations write fPCWB5Iterations; 
  end;

implementation

{$R *.dfm}

procedure TfrmMobileTest.BitBtnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmMobileTest.BitBtnOkClick(Sender: TObject);
begin
  if RadioButtonNormal.Checked then fMobileTestType := mttNormal
  else if RadioButtonMaxPowerDraw.Checked then fMobileTestType := mttMaxPowerDraw
  else if RadioButtonLowPowerDraw.Checked then fMobileTestType := mttLowPowerDraw
  else if RadioButtonPCWB5.Checked then fMobileTestType := mttPCWB5;
  ModalResult := mrOK;
end;

procedure TfrmMobileTest.BitBtnSaveClick(Sender: TObject);
begin
  with SaveDialog1 do begin
    FileName := '';
    Filter := 'Text file|*.txt';
    FilterIndex := 1;
    DefaultExt := '.txt';
    //execute the save dialog:
    if Execute then begin
      fResultsFileName := FileName;
      // add a ".txt" suffix if a file suffix is not specified:
      if Pos('.', fResultsFileName) = 0 then begin
        fResultsFileName := fResultsFileName + '.txt';
      end; // if
    end; // if
  end; // with
end;

procedure TfrmMobileTest.FormCreate(Sender: TObject);
begin
  ComboBoxPCWB5Iterations.ItemIndex := ComboBoxPCWB5Iterations.Items.Count - 1;
  fPCWB5Iterations := StrToInt( ComboBoxPCWB5Iterations.Text );
  fMobileTestType := mttNormal;
  fResultsFileName := '';
  if FileExists( 'c:\worldbench\mtrun.exe' ) then begin
    RadioButtonPCWB5.Enabled := TRUE;
    ComboBoxPCWB5Iterations.Enabled := TRUE;
  end;
end;

procedure TfrmMobileTest.ComboBoxPCWB5IterationsChange(Sender: TObject);
begin
  fPCWB5Iterations := StrToInt( ComboBoxPCWB5Iterations.Text );
  RadioButtonPCWB5.Checked := TRUE;
end;

procedure TfrmMobileTest.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FadeFast100( self );
end;

procedure TfrmMobileTest.FormActivate(Sender: TObject);
begin
  UnfadeFast( self );
end;

procedure TfrmMobileTest.RadioButtonLowPowerDrawClick(Sender: TObject);
var
  s : string;
begin
  if RadioButtonLowPowerDraw.Checked then begin
    s := 'Before continuing, make sure that the screensaver is turned off.' + CR_LF + CR_LF;
    s := s + 'Also, from "Power Options Properties" opened from the "Display ';
    s := s + 'Properties | Screen Saver" tab:' + CR_LF;
    s := s + '* Choose the "Portable / Laptop" power scheme.' + CR_LF;
    s := s + '* Set the monitor to TURN OFF after one minute when running on batteries.' + CR_LF;
    s := s + '* When on batteries, turn off hard disks at 3 minutes.' + CR_LF;
    s := s + '* The system should never enter standby when on batteries.' + CR_LF + CR_LF;
    s := s + 'On the "Alarms" tab:' + CR_LF;
    s := s + '* Uncheck "Low battery alarm"' + CR_LF;
    s := s + '* Uncheck "Critical battery alarm."' + CR_LF + CR_LF;
    s := s + 'The test will run until the battery has 3% charge.  Upon reconnecting power and ';
    s := s + 'restarting the notebook, the mobile results can be found at the end of the file ';
    s := s + 'that you selected (default: OSMarkMobileResults_xxxxxxx.txt).' + CR_LF + CR_LF;
    s := s + 'Once the tests are underway, you can press <Esc> to abort them if ';
    s := s + 'necessary.' + CR_LF + CR_LF;
    s := s + 'When ready, please disconnect the power adapter from the notebook and ';
    s := s + 'then click the "OK" button.';
    Memo1.Lines.Clear;
    Memo1.Color := clBlack;
    Memo1.Font.Color := clWhite;
    Memo1.Lines.Add( s );
  end else begin
    s := 'Before continuing, make sure that the screensaver is turned off.' + CR_LF + CR_LF;
    s := s + 'Also, from "Power Options Properties" opened from the "Display ';
    s := s + 'Properties | Screen Saver" tab:' + CR_LF;
    s := s + '* Choose the "Portable / Laptop" power scheme.' + CR_LF;
    s := s + '* Set the monitor to TURN OFF after one minute when running on batteries. (Setting the monitor to never turn off when running on batteries is also a valid condition.)' + CR_LF;
    s := s + '* When on batteries, turn off hard disks at 3 minutes.' + CR_LF;
    s := s + '* The system should never enter standby when on batteries.' + CR_LF + CR_LF;
    s := s + 'On the "Alarms" tab:' + CR_LF;
    s := s + '* Uncheck "Low battery alarm"' + CR_LF;
    s := s + '* Uncheck "Critical battery alarm."' + CR_LF + CR_LF;
    s := s + 'The test will run until the battery has 3% charge.  Upon reconnecting power and ';
    s := s + 'restarting the notebook, the mobile results can be found at the end of the file ';
    s := s + 'that you selected (default: OSMarkMobileResults_xxxxxxx.txt).' + CR_LF + CR_LF;
    s := s + 'Once the tests are underway, you can press <Esc> to abort them if ';
    s := s + 'necessary.' + CR_LF + CR_LF;
    s := s + 'When ready, please disconnect the power adapter from the notebook and ';
    s := s + 'then click the "OK" button.';
    Memo1.Color := clWhite;
    Memo1.Font.Color := clBlack;
    Memo1.Lines.Clear;
    Memo1.Lines.Add( s );
  end; // if
end;

end.
