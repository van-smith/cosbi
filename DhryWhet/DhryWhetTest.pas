unit DhryWhetTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Dhrystone, Whetstone, COSBI_Common, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    edDTime: TEdit;
    Label2: TLabel;
    btnWhetstone: TButton;
    Label3: TLabel;
    edWIterate: TEdit;
    Label4: TLabel;
    edWTime: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    edDhrystones: TEdit;
    edWhetstones: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure btnWhetstoneClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  lStopWatch : TStopWatch;
  lDhrystone : TDhrystone;
begin
  try
    lStopWatch := TStopWatch.Create;
    lDhrystone := TDhrystone.Create;
    lStopWatch.Resolution := swresHigh;
    with lDhrystone do begin
      StopWatch := lStopWatch;
//      NumberOfRuns := StrToInt( Edit1.Text );
      Run;
      Run;
      Run;
      ShowMessage(Format('Dhrystones score: %d ', [GetScore(0)]));
      edDhrystones.Text := FloatToStr( DhrystonesPerSecond );
      edDTime.Text := FloatToStr( MinTime );
    end;
  finally
    FreeAndNil( lStopWatch );
    FreeAndNil( lDhrystone );
  end; // try..finally
end;

procedure TForm1.btnWhetstoneClick(Sender: TObject);
var
  lStopWatch : TStopWatch;
  lWhetstone : TWhetstone;
begin
  try
    lStopWatch := TStopWatch.Create;
    lWhetstone := TWhetstone.Create;
    lStopWatch.Resolution := swresHigh;
    with lWhetstone do begin
      StopWatch := lStopWatch;
//      NumberOfRuns := StrToInt( edWIterate.Text );
      Run;
      Run;
      Run;
      ShowMessage(Format('Whetstone score: %d', [GetScore(0)]));
      edWhetstones.Text := FloatToStr( WhetstonesPerSecond );
      edWTime.Text := FloatToStr( MinTime );
    end;
  finally
    FreeAndNil( lStopWatch );
    FreeAndNil( lWhetstone );
  end; // try..finally
end;

end.
