unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, ExtCtrls, COSBI_Common, uStopWatch,
  Maze, GR32_Image;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label1: TLabel;
    edtWidth: TMaskEdit;
    edtHeight: TMaskEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    PaintBox321: TPaintBox32;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure PaintBox321Resize(Sender: TObject);
  private
    { Private declarations }
    FMaze : TMaze;

    FStopWatch  : TStopWatch;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}
procedure TForm1.Button1Click(Sender: TObject);
var
  le_time : extended;

begin
  FStopWatch.StartTimer;

  with FMaze do begin
    Make( StrToInt( Trim ( edtWidth.Text )),
          StrToInt( Trim( edtHeight.Text )));
  end;

  le_time := FStopWatch.StopTimer;

  ShowMessage( 'Elapsed time to create maze: ' +
               FloatToStrF( le_time, ffNumber, 6, 4 ) + ' seconds.');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Fmaze := TMaze.Create( PaintBox321 );
  FStopWatch := TStopWatch.Create;
  FStopWatch.Resolution := swresCPU ;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FMaze.Free;
  FStopWatch.Free;
end;

procedure TForm1.FormResize(Sender: TObject);
var
  rect : TRect;
begin
  rect.Top    := 0;
  rect.Left   := 0;
  rect.Right  := PaintBox321.Width;
  rect.Bottom := PaintBox321.Height;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FMaze.Print( CheckBox2.Checked );
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
{  FStopWatch.StartTimer;

  Application.ProcessMessages;
}
  FMaze.Solve( CheckBox1.Checked );

end;

procedure TForm1.PaintBox321Resize(Sender: TObject);
begin
  FMaze.Draw;
end;

end.
