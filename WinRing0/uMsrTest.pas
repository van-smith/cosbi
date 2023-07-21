unit uMsrTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, COSBI_Common;

type
  TForm1 = class(TForm)
    Button1: TButton;
    memoStatus: TMemo;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function InitializeOls: Boolean; stdcall; external 'WinRing0.dll';
procedure DeinitializeOls; stdcall; external 'WinRing0.dll';

function GetDllVersion(
	var major : byte;
	var minor : byte;
	var revision : byte;
	var release : byte
): integer; stdcall; external 'WinRing0.dll';

function RdmsrEx(
	msrIndex : integer;
	var lowWord : integer;
	var highWord : integer;
	var affinityMask : integer
): Boolean; stdcall; external 'WinRing0.dll';


procedure TForm1.Button1Click(Sender: TObject);
var
  lbyteMajor : byte;
  lbyteMinor : byte;
  lbyteRevision : byte;
  lbyteRelease : byte;
  liResult : integer;
begin
  liResult := GetDllVersion( lbyteMajor, lbyteMinor, lbyteRevision, lbyteRelease );
  memoStatus.Lines.Add(' DLL Version: ' + intToStr(lbyteMajor) + '.'
                       + intToStr(lbyteMinor) + '.' + intToStr(lbyteRevision)
                       + intToStr(lbyteRelease) );
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  l64bitWord : T64bitRegister;
  lLowWord : integer;
  lHighWord : integer;
  liCpu : integer;
begin
  liCpu := 0;
  if RdmsrEx( $10, lLowWord, lHighWord, liCpu ) then begin
    l64bitWord.LowWord := lLowWord;
    l64bitWord.HighWord := lHighWord;
    memoStatus.Lines.Add('TSC: ' + intToStr(l64bitWord.AsInt64))
  end else memoStatus.Lines.Add( 'RdMsrEx failed' );
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  l64bitWord : T64bitWord;
  lLowWord : integer;
  lHighWord : integer;
  liCpu : integer;
begin
  liCpu := 0;
  if RdmsrEx( $198, lLowWord, lHighWord, liCpu ) then begin
    l64bitWord.low32 := lLowWord;
    l64bitWord.high32 := lHighWord;
    memoStatus.Lines.Add('MSR 0x198: ' + intToHex(l64bitWord.int64, 16))
  end else memoStatus.Lines.Add( 'RdMsrEx failed' );
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  l64bitWord : T64bitWord;
  lLowWord : integer;
  lHighWord : integer;
  liCpu : integer;
begin
  liCpu := 0;
  if RdmsrEx( $1203, lLowWord, lHighWord, liCpu ) then begin
    l64bitWord.low32 := lLowWord;
    l64bitWord.high32 := lHighWord;
    memoStatus.Lines.Add('MSR 0x1203: ' + intToHex(l64bitWord.int64, 16))
  end else memoStatus.Lines.Add( 'RdMsrEx failed' );
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DeinitializeOls;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  memoStatus.Lines.Clear;
  if InitializeOls then memoStatus.Lines.Add('OLS successfully initialized!')
  else memoStatus.Lines.Add('OLS initialization failed!');

end;

end.
