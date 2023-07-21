unit uCpuSpeedMsr;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, COSBI_Common, uMSR, math, Mask,
  strUtils, CosbiCpuid;

type
  TfrmMSR = class(TForm)
    Label2: TLabel;
    edMSR: TEdit;
    Label12: TLabel;
    edCpu: TEdit;
    btnReadMSR: TButton;
    Label11: TLabel;
    cbox0: TCheckBox;
    cbox1: TCheckBox;
    cbox2: TCheckBox;
    cbox3: TCheckBox;
    cbox4: TCheckBox;
    cbox5: TCheckBox;
    cbox6: TCheckBox;
    cbox7: TCheckBox;
    cbox8: TCheckBox;
    cbox9: TCheckBox;
    cbox10: TCheckBox;
    cbox11: TCheckBox;
    cbox12: TCheckBox;
    cbox13: TCheckBox;
    cbox14: TCheckBox;
    cbox15: TCheckBox;
    cbox16: TCheckBox;
    cbox17: TCheckBox;
    cbox18: TCheckBox;
    cbox19: TCheckBox;
    cbox20: TCheckBox;
    cbox21: TCheckBox;
    cbox22: TCheckBox;
    cbox23: TCheckBox;
    cbox24: TCheckBox;
    cbox25: TCheckBox;
    cbox26: TCheckBox;
    cbox27: TCheckBox;
    cbox28: TCheckBox;
    cbox29: TCheckBox;
    cbox30: TCheckBox;
    cbox31: TCheckBox;
    cbox32: TCheckBox;
    cbox33: TCheckBox;
    cbox34: TCheckBox;
    cbox35: TCheckBox;
    cbox36: TCheckBox;
    cbox37: TCheckBox;
    cbox38: TCheckBox;
    cbox39: TCheckBox;
    cbox40: TCheckBox;
    cbox41: TCheckBox;
    cbox42: TCheckBox;
    cbox43: TCheckBox;
    cbox44: TCheckBox;
    cbox45: TCheckBox;
    cbox46: TCheckBox;
    cbox47: TCheckBox;
    cbox48: TCheckBox;
    cbox49: TCheckBox;
    cbox50: TCheckBox;
    cbox51: TCheckBox;
    cbox52: TCheckBox;
    cbox53: TCheckBox;
    cbox54: TCheckBox;
    cbox55: TCheckBox;
    cbox56: TCheckBox;
    cbox57: TCheckBox;
    cbox58: TCheckBox;
    cbox59: TCheckBox;
    cbox60: TCheckBox;
    cbox61: TCheckBox;
    cbox62: TCheckBox;
    cbox63: TCheckBox;
    Label1: TLabel;
    Label9: TLabel;
    edMsrValue: TEdit;
    btnWriteMsr: TButton;
    cboxAllowWrites: TCheckBox;
    Bevel1: TBevel;
    Bevel2: TBevel;
    BitBtnClose: TBitBtn;
    btnCustomVendorString: TButton;
    edVendorString: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnCustomVendorStringClick(Sender: TObject);
    procedure btnMakeViaClick(Sender: TObject);
    procedure btnMakeAMDClick(Sender: TObject);
    procedure btnMakeIntelClick(Sender: TObject);
    procedure cboxAllowWritesClick(Sender: TObject);
    procedure edMsrValueChange(Sender: TObject);
    procedure cbox0Click(Sender: TObject);
    procedure BitBtnCloseClick(Sender: TObject);
    procedure edMSRKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnWriteMsrClick(Sender: TObject);
    procedure btnReadMSRClick(Sender: TObject);
  private
    fCpu          : integer;
    fMsr          : integer;
    fMsrDriver    : TMsrDriver;
    fMsrRegister  : T64bitRegister;
    fMsrValueToBits : Boolean;
    { Private declarations }
    procedure BitsToMsrValue;
    procedure MsrValueToBits;
    procedure ParseCpu;
    procedure ParseMsr;
    procedure ParseMsrValue;
    procedure ReadMsr;
    procedure WriteMsr;
    procedure MakeIntel;
    procedure MakeVia;
    procedure MakeAmd;
    procedure ChangeVendorString( ai64FirstLongWord : int64;
                                  aiLastWord : integer );
  public
    { Public declarations }
    property MsrDriver : TMsrDriver read fMsrDriver write fMsrDriver;
  end;

implementation

{$R *.dfm}

procedure TfrmMSR.MsrValueToBits;
begin
  ParseMsrValue;
  fMsrValueToBits := FALSE;
  cbox0.Checked := (bit0 in fMsrRegister.bits64);
  cbox1.Checked := (bit1 in fMsrRegister.bits64);
  cbox2.Checked := (bit2 in fMsrRegister.bits64);
  cbox3.Checked := (bit3 in fMsrRegister.bits64);
  cbox4.Checked := (bit4 in fMsrRegister.bits64);
  cbox5.Checked := (bit5 in fMsrRegister.bits64);
  cbox6.Checked := (bit6 in fMsrRegister.bits64);
  cbox7.Checked := (bit7 in fMsrRegister.bits64);
  cbox8.Checked := (bit8 in fMsrRegister.bits64);
  cbox9.Checked := (bit9 in fMsrRegister.bits64);
  cbox10.Checked := (bit10 in fMsrRegister.bits64);
  cbox11.Checked := (bit11 in fMsrRegister.bits64);
  cbox12.Checked := (bit12 in fMsrRegister.bits64);
  cbox13.Checked := (bit13 in fMsrRegister.bits64);
  cbox14.Checked := (bit14 in fMsrRegister.bits64);
  cbox15.Checked := (bit15 in fMsrRegister.bits64);
  cbox16.Checked := (bit16 in fMsrRegister.bits64);
  cbox17.Checked := (bit17 in fMsrRegister.bits64);
  cbox18.Checked := (bit18 in fMsrRegister.bits64);
  cbox19.Checked := (bit19 in fMsrRegister.bits64);
  cbox20.Checked := (bit20 in fMsrRegister.bits64);
  cbox21.Checked := (bit21 in fMsrRegister.bits64);
  cbox22.Checked := (bit22 in fMsrRegister.bits64);
  cbox23.Checked := (bit23 in fMsrRegister.bits64);
  cbox24.Checked := (bit24 in fMsrRegister.bits64);
  cbox25.Checked := (bit25 in fMsrRegister.bits64);
  cbox26.Checked := (bit26 in fMsrRegister.bits64);
  cbox27.Checked := (bit27 in fMsrRegister.bits64);
  cbox28.Checked := (bit28 in fMsrRegister.bits64);
  cbox29.Checked := (bit29 in fMsrRegister.bits64);
  cbox30.Checked := (bit30 in fMsrRegister.bits64);
  cbox31.Checked := (bit31 in fMsrRegister.bits64);
  cbox32.Checked := (bit32 in fMsrRegister.bits64);
  cbox33.Checked := (bit33 in fMsrRegister.bits64);
  cbox34.Checked := (bit34 in fMsrRegister.bits64);
  cbox35.Checked := (bit35 in fMsrRegister.bits64);
  cbox36.Checked := (bit36 in fMsrRegister.bits64);
  cbox37.Checked := (bit37 in fMsrRegister.bits64);
  cbox38.Checked := (bit38 in fMsrRegister.bits64);
  cbox39.Checked := (bit39 in fMsrRegister.bits64);
  cbox40.Checked := (bit40 in fMsrRegister.bits64);
  cbox41.Checked := (bit41 in fMsrRegister.bits64);
  cbox42.Checked := (bit42 in fMsrRegister.bits64);
  cbox43.Checked := (bit43 in fMsrRegister.bits64);
  cbox44.Checked := (bit44 in fMsrRegister.bits64);
  cbox45.Checked := (bit45 in fMsrRegister.bits64);
  cbox46.Checked := (bit46 in fMsrRegister.bits64);
  cbox47.Checked := (bit47 in fMsrRegister.bits64);
  cbox48.Checked := (bit48 in fMsrRegister.bits64);
  cbox49.Checked := (bit49 in fMsrRegister.bits64);
  cbox50.Checked := (bit50 in fMsrRegister.bits64);
  cbox51.Checked := (bit51 in fMsrRegister.bits64);
  cbox52.Checked := (bit52 in fMsrRegister.bits64);
  cbox53.Checked := (bit53 in fMsrRegister.bits64);
  cbox54.Checked := (bit54 in fMsrRegister.bits64);
  cbox55.Checked := (bit55 in fMsrRegister.bits64);
  cbox56.Checked := (bit56 in fMsrRegister.bits64);
  cbox57.Checked := (bit57 in fMsrRegister.bits64);
  cbox58.Checked := (bit58 in fMsrRegister.bits64);
  cbox59.Checked := (bit59 in fMsrRegister.bits64);
  cbox60.Checked := (bit60 in fMsrRegister.bits64);
  cbox61.Checked := (bit61 in fMsrRegister.bits64);
  cbox62.Checked := (bit62 in fMsrRegister.bits64);
  cbox63.Checked := (bit63 in fMsrRegister.bits64);
  fMsrValueToBits := TRUE;
end; // procedure TfrmMSR.BitsToMsrValue;

procedure TfrmMSR.BitsToMsrValue;
begin
  fMsrRegister.AsInt64 := 0;
  fMsrValueToBits := FALSE;
  if cbox0.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit0];
  if cbox1.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit1];
  if cbox2.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit2];
  if cbox3.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit3];
  if cbox4.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit4];
  if cbox5.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit5];
  if cbox6.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit6];
  if cbox7.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit7];
  if cbox8.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit8];
  if cbox9.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit9];
  if cbox10.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit10];
  if cbox11.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit11];
  if cbox12.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit12];
  if cbox13.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit13];
  if cbox14.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit14];
  if cbox15.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit15];
  if cbox16.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit16];
  if cbox17.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit17];
  if cbox18.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit18];
  if cbox19.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit19];
  if cbox20.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit20];
  if cbox21.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit21];
  if cbox22.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit22];
  if cbox23.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit23];
  if cbox24.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit24];
  if cbox25.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit25];
  if cbox26.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit26];
  if cbox27.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit27];
  if cbox28.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit28];
  if cbox29.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit29];
  if cbox30.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit30];
  if cbox31.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit31];
  if cbox32.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit32];
  if cbox33.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit33];
  if cbox34.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit34];
  if cbox35.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit35];
  if cbox36.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit36];
  if cbox37.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit37];
  if cbox38.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit38];
  if cbox39.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit39];
  if cbox40.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit40];
  if cbox41.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit41];
  if cbox42.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit42];
  if cbox43.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit43];
  if cbox44.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit44];
  if cbox45.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit45];
  if cbox46.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit46];
  if cbox47.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit47];
  if cbox48.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit48];
  if cbox49.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit49];
  if cbox50.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit50];
  if cbox51.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit51];
  if cbox52.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit52];
  if cbox53.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit53];
  if cbox54.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit54];
  if cbox55.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit55];
  if cbox56.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit56];
  if cbox57.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit57];
  if cbox58.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit58];
  if cbox59.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit59];
  if cbox60.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit60];
  if cbox61.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit61];
  if cbox62.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit62];
  if cbox63.Checked then fMsrRegister.bits64 := fMsrRegister.bits64 + [bit63];
  edMsrValue.Text := '$' + IntToHex( fMsrRegister.AsInt64, 16 );
  fMsrValueToBits := TRUE;
end;

procedure TfrmMSR.ParseCpu;
begin
  try
    fCpu := StrToInt( edCpu.Text );
  except
    fCpu := 0;
    edCpu.Text := '0';
    raise exception.Create( 'Invalid CPU number. Reducing to CPU 0. Try again.' );
  end; // try
  if fCpu > ( fMsrDriver.GetNumberOfCpus - 1 ) then begin
    edCpu.Text := IntToStr( fMsrDriver.GetNumberOfCpus - 1 );
    edMsrValue.Text := '$';
    raise exception.Create( 'Invalid CPU number. Reducing to max CPU. Try again.' );
  end;
end;

procedure TfrmMSR.ParseMsr;
begin
  try
    fMsr := StrToInt( edMsr.Text );
  except
    raise exception.Create( 'Invalid MSR value: ' + edMsr.Text );
  end;
end;

procedure TfrmMSR.ParseMsrValue;
begin
  try
    fMsrRegister.AsInt64 := StrToInt64( edMsrValue.Text );
  except
    raise exception.Create( '"' + edMsrValue.Text + '" is an invalid MSR value.' );
  end; //
end;

procedure TfrmMSR.ReadMSR;
var
  li64MsrValue : int64;
begin
  ParseMsr;
  ParseCpu;
  fMsrRegister := fMsrDriver.ReadMsr( fCpu, fMSR );
  fMsrValueToBits := FALSE;
  edMsrValue.Text := '$' + IntToHex( fMsrRegister.AsInt64, 16 );
  fMsrValueToBits := TRUE;
  MsrValueToBits;
end;

procedure TfrmMSR.WriteMSR;
begin
  ParseMsr;
  ParseCpu;
  ParseMsrValue;
  fMsrRegister := fMsrDriver.WriteMsr( fCpu, fMSR, fMsrRegister );
  edMsrValue.Text := '$' + IntToHex( fMsrRegister.AsInt64, 16 );
  showMessage( edMsrValue.Text );
end;

procedure TfrmMSR.btnReadMSRClick(Sender: TObject);
begin
  ReadMSR;
end;

procedure TfrmMSR.btnWriteMsrClick(Sender: TObject);
begin
  WriteMsr;
end;

procedure TfrmMSR.edMSRKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  liKey : cardinal;
begin
  if Key = 13 then begin
    ReadMsr;
  end; // ifend;
end;

procedure TfrmMSR.BitBtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMSR.cbox0Click(Sender: TObject);
begin
  if fMsrValueToBits then BitsToMsrValue;
end;

procedure TfrmMSR.edMsrValueChange(Sender: TObject);
begin
  if fMsrValueToBits then begin
    fMsrValueToBits := FALSE;
    MsrValueToBits;
    fMsrValueToBits := TRUE;
  end;
end;

procedure TfrmMSR.cboxAllowWritesClick(Sender: TObject);
begin
  btnWriteMsr.Enabled := cboxAllowWrites.Checked;
  if cboxAllowWrites.Checked then showmessage( 'You can easily crash or even damage your computer by writing indiscriminately to MSRs.  Use at your own risk!' );
end;

procedure TfrmMSR.MakeIntel;
begin
  ChangeVendorString( $756e654749656e69, $6c65746e );
end;

procedure TfrmMSR.MakeVia;
begin
  ChangeVendorString( $746e654348727561, $736c7561 );
end;

procedure TfrmMSR.MakeAmd;
begin
  ChangeVendorString( $6874754169746E65, $444D4163 );
end;

procedure TfrmMSR.ChangeVendorString( ai64FirstLongWord : int64;
                                      aiLastWord: integer );
// this procedure changes the CPUID vendor strings for VIA CPUs.
// the string must be encoded in reverse word order.
// for instance:
// "CentuarHauls" ==> MSR1109 = "uarHCent", MSR1108 = "auls" + untouched 4 lower words 
const
  MSR1108 = $1108;
  MSR1109 = $1109;
begin
  ParseCpu;
  // first we need to turn on the ability to write a new vendor string:
  // we only need to toggle one bit in MSR 0x1108, so read that MSR first:
  fMsrRegister := fMsrDriver.ReadMsr( fCpu, MSR1108 );
  // now turn on bit 14:
  fMsrRegister.bits64 := fMsrRegister.bits64 + [bit14];
  // now write the new value to enable changing the vendor string:
  fMsrRegister := fMsrDriver.WriteMsr( fCpu, MSR1108, fMsrRegister );
  // now we can change the vendor string
  fMsrRegister.HighWord := aiLastWord;
  fMsrRegister := fMsrDriver.WriteMsr( fCpu, MSR1108, fMsrRegister );
  fMsrRegister.AsInt64 := ai64FirstLongWord;
  fMsrRegister := fMsrDriver.WriteMsr( fCpu, MSR1109, fMsrRegister );
  ShowMessage( 'CPUID Vendor ID string is now: ' + CpuidGetVendorIDString );
end;

procedure TfrmMSR.btnMakeIntelClick(Sender: TObject);
begin
  MakeIntel;
end;

procedure TfrmMSR.btnMakeAMDClick(Sender: TObject);
begin
  MakeAmd;
end;

procedure TfrmMSR.btnMakeViaClick(Sender: TObject);
begin
  MakeVia;
end;

procedure TfrmMSR.btnCustomVendorStringClick(Sender: TObject);
var
  lsVendorString : string;
  lstr : array [0..11] of char;
  lsTemp : string;
  i : integer;
  lReg1 : T64bitRegister;
  lReg2 : T64bitRegister;
begin
  // capture text:
  lsVendorString := edVendorString.Text;
  // initialize char array to blanks:
  for i := 0 to 11 do begin
    lstr[ i ] := ' ';
  end;
  for i := 1 to length( lsVendorString ) do begin
    lsTemp := Copy( lsVendorString, i, 1 );
    lstr[ i - 1 ] := lsTemp[1];
  end;
  for i := 0 to 3 do begin
    lReg1.str[ i + 4 ] := lstr[ i ];
  end;
  for i := 4 to 7 do begin
    lReg1.str[ i - 4 ] := lstr[ i ];
  end;
  for i := 8 to 11 do begin
    lReg2.str[ i - 4 ] := lstr[ i ];
  end;
  ChangeVendorString( lReg1.AsInt64, lReg2.HighWord );
end;

procedure TfrmMSR.FormCreate(Sender: TObject);
begin
  if CpuidIsCentaurC7 then begin
    //btnMakeIntel.Visible := TRUE;
    //btnMakeAMD.Visible := TRUE;
    //btnMakeVia.Visible := TRUE;
    btnCustomVendorString.Visible := TRUE;
    edVendorString.Visible := TRUE;
  end;
end;

end.

