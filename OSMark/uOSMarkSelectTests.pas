unit uOSMarkSelectTests;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, MPlayer, Buttons, uOSMarkSuite, uCOSBI_TTest,
  CosbiCpuid, COSBI_Common;

type
  TfrmOSMarkSelectTests = class(TForm)
    lblIterations: TLabel;
    cboIterations: TComboBox;
    gbSelectTests: TGroupBox;
    cboxGridBlast: TCheckBox;
    cboxRandomCircle: TCheckBox;
    cboxPlotTrig: TCheckBox;
    cboxPlotLines: TCheckBox;
    cboxRandDots: TCheckBox;
    cboxFib: TCheckBox;
    cboxNBody: TCheckBox;
    cboxMaze: TCheckBox;
    cboxTrig2: TCheckBox;
    cboxFern: TCheckBox;
    cboxRichEd: TCheckBox;
    cboxCalculatePi: TCheckBox;
    cboxDhry: TCheckBox;
    cboxWhet: TCheckBox;
    cboxBandwidthBP64: TCheckBox;
    cboxLatency: TCheckBox;
    cboxMazeThreads: TCheckBox;
    cboxGridBlastFP: TCheckBox;
    cboxOThreads: TCheckBox;
    cboxIThreads: TCheckBox;
    cboxImageRotate: TCheckBox;
    cboxJpgDecode: TCheckBox;
    cboxImageResize: TCheckBox;
    cboxMP3Encode: TCheckBox;
    cboxWebPageLoad: TCheckBox;
    cboxEncryptDecrypt: TCheckBox;
    cboxZipCompress: TCheckBox;
    cboxFileCopy: TCheckBox;
    cboxLorenz: TCheckBox;
    cboxNBodyOpenGL: TCheckBox;
    cboxDhrystoneThreads: TCheckBox;
    cboxWhetstoneThreads: TCheckBox;
    cboxPiThreads: TCheckBox;
    cboxMandelbrot: TCheckBox;
    cboxEllipses: TCheckBox;
    cboxRectangles: TCheckBox;
    cboxLines: TCheckBox;
    cboxFibThreads: TCheckBox;
    MediaPlayer1: TMediaPlayer;
    cboxPngOut: TCheckBox;
    cbox7zip: TCheckBox;
    cboxUpx: TCheckBox;
    cboxNbodyFPU: TCheckBox;
    cboxNBodySSE2Scalar: TCheckBox;
    cboxNBodySSE2: TCheckBox;
    cboxNBodySSE3: TCheckBox;
    btnSelectNone: TButton;
    btnSelectAll: TButton;
    SpeedButtonOk: TSpeedButton;
    SpeedButtonCancel: TSpeedButton;
    cboxAlphaBlend: TCheckBox;
    cboxSSE3: TCheckBox;
    cboxMontMul: TCheckBox;
    cboxSha256: TCheckBox;
    cboxSha1: TCheckBox;
    Label1: TLabel;
    Bevel1: TBevel;
    Label2: TLabel;
    cboxAlphaDots: TCheckBox;
    cboxLame: TCheckBox;
    cboxOgg: TCheckBox;
    cboxCppCompile: TCheckBox;
    cboxMetaballs: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnSelectNoneClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButtonOkClick(Sender: TObject);
    procedure SpeedButtonCancelClick(Sender: TObject);
  private
    { Private declarations }
    fSelectedTests : TQTestSet;
    fIterations : integer;
    function  GetIterations : integer;
    function  GetSelectedTests: TQTestSet;
    function  CountTests: integer;
    procedure SetSelectedTests( Value : TQTestSet );
    procedure SetIterations( Value : integer );
    procedure CheckForInstructionSetSupport;
    procedure CheckUncheckBoxes(ab_Checked : Boolean);
  public
    { Public declarations }
    property SelectedTests : TQTestSet read GetSelectedTests write SetSelectedTests;
    property Iterations : integer read GetIterations write SetIterations;
  end;

implementation

{$R *.dfm}

function TfrmOSMarkSelectTests.GetSelectedTests: TQTestSet;
begin
  fSelectedTests := [];
  if cboxFib.Checked then fSelectedTests := fSelectedTests + [qtFib];
  if cboxGridBlast.Checked then fSelectedTests := fSelectedTests + [qtGrid];
  if cboxGridBlastFP.Checked then fSelectedTests := fSelectedTests + [qtGridFP];
  if cboxNBody.Checked then fSelectedTests := fSelectedTests + [qtNBody];
  if cboxPlotLines.Checked then fSelectedTests := fSelectedTests + [qtPlotLines];
  if cboxPlotTrig.Checked then fSelectedTests := fSelectedTests + [qtPlotTrig];
  if cboxRandDots.Checked then fSelectedTests := fSelectedTests + [qtRandomDots];
  if cboxRandomCircle.Checked then fSelectedTests := fSelectedTests + [qtCircles];
  if cboxMaze.Checked then fSelectedTests := fSelectedTests + [qtMaze];
  if cboxTrig2.Checked then fSelectedTests := fSelectedTests + [qtPlotTrig2];
  if cboxFern.Checked then fSelectedTests := fSelectedTests + [qtFern];
  if cboxRichEd.Checked then fSelectedTests := fSelectedTests + [qtRichEd];
  if cboxCalculatePi.Checked then fSelectedTests := fSelectedTests + [qtPi];
  if cboxDhry.Checked then fSelectedTests := fSelectedTests + [qtDhrystone];
  if cboxWhet.Checked then fSelectedTests := fSelectedTests + [qtWhetstone];
  if cboxBandwidthBP64.Checked then fSelectedTests := fSelectedTests + [qtBandwidthBP64];
  if cboxLatency.Checked then fSelectedTests := fSelectedTests + [qtMemLatency];
  if cboxMazeThreads.Checked then fSelectedTests := fSelectedTests + [qtMazeThreads];
  if cboxOThreads.Checked then fSelectedTests := fSelectedTests + [qtOrthogonalThreads];
  if cboxIThreads.Checked then fSelectedTests := fSelectedTests + [qtIdenticalThreads];
  if cboxJpgDecode.Checked then fSelectedTests := fSelectedTests + [qtJpgDecode];
  if cboxImageResize.Checked then fSelectedTests := fSelectedTests + [qtImageResize];
  if cboxImageRotate.Checked then fSelectedTests := fSelectedTests + [qtImageRotate];
  if cboxMp3Encode.Checked then fSelectedTests := fSelectedTests + [qtGogoEncode];
  if cboxWebPageLoad.Checked then fSelectedTests := fSelectedTests + [qtWebPageLoad];
  if cboxZipCompress.Checked then fSelectedTests := fSelectedTests + [qtZipCompress];
  if cboxEncryptDecrypt.Checked then fSelectedTests := fSelectedTests + [qtEncryptDecrypt];
  if cboxFileCopy.Checked then fSelectedTests := fSelectedTests + [qtFileCopy];
  if cboxLorenz.Checked then fSelectedTests := fSelectedTests + [qtLorenz];
  if cboxNBodyOpenGL.Checked then fSelectedTests := fSelectedTests + [qtNBodyOpenGL];
  if cboxDhrystoneThreads.Checked then fSelectedTests := fSelectedTests + [qtDhrystoneThreads];
  if cboxWhetstoneThreads.Checked then fSelectedTests := fSelectedTests + [qtWhetstoneThreads];
  if cboxPiThreads.Checked then fSelectedTests := fSelectedTests + [qtPiThreads];
  if cboxMandelbrot.Checked then fSelectedTests := fSelectedTests + [qtMandelbrotThreads];
  if cboxEllipses.Checked then fSelectedTests := fSelectedTests + [qtDrawEllipses];
  if cboxRectangles.Checked then fSelectedTests := fSelectedTests + [qtDrawRectangles];
  if cboxLines.Checked then fSelectedTests := fSelectedTests + [qtDrawLines];
  if cboxFibThreads.Checked then fSelectedTests := fSelectedTests + [qtFibThreads];
  if cboxPngOut.Checked then fSelectedTests := fSelectedTests + [qtPngOut];
  if cbox7zip.Checked then fSelectedTests := fSelectedTests + [qt7zip];
  if cboxUpx.Checked then fSelectedTests := fSelectedTests + [qtUpx];
  if cboxNbodyFPU.Checked then fSelectedTests := fSelectedTests + [qtNBodyFPU];
  if cboxNbodySSE2.Checked then fSelectedTests := fSelectedTests + [qtNBodySSE2];
  if cboxNbodySSE2Scalar.Checked then fSelectedTests := fSelectedTests + [qtNBodySSE2Scalar];
  if cboxNbodySSE3.Checked then fSelectedTests := fSelectedTests + [qtNBodySSE3];
  if cboxSSE3.Checked then fSelectedTests := fSelectedTests + [qtSSE3];
  if cboxMontMul.Checked then fSelectedTests := fSelectedTests + [qtMontMul];
  if cboxSha1.Checked then fSelectedTests := fSelectedTests + [qtSha1];
  if cboxSha256.Checked then fSelectedTests := fSelectedTests + [qtSha256];
  if cboxAlphaBlend.Checked then fSelectedTests := fSelectedTests + [qtAlphaBlend];
  if cboxAlphaDots.Checked then fSelectedTests := fSelectedTests + [qtAlphaDots];
  if cboxLame.Checked then fSelectedTests := fSelectedTests + [qtLame];
  if cboxOgg.Checked then fSelectedTests := fSelectedTests + [qtOgg];
  if cboxCppCompile.Checked then fSelectedTests := fSelectedTests + [qtCppCompiler];
  if cboxMetaballs.Checked then fSelectedTests := fSelectedTests + [qtMetaballs];
  result := fSelectedTests;
end; // function GetSelectedTests

procedure TfrmOSMarkSelectTests.SetSelectedTests( Value : TQTestSet );
  procedure SetCheckBox( aCheckBox : TCheckBox; aqtTest : TQTests );
  begin
    if aCheckBox.Enabled then begin
      aCheckBox.Checked := aqtTest in fSelectedTests;
    end else begin
      aCheckBox.Checked := FALSE;
      fSelectedTests := fSelectedTests - [aqtTest];
    end; // if
  end;
begin
  fSelectedTests := Value;
  SetCheckBox( cboxFib, qtFib );
  SetCheckBox( cboxGridBlast, qtGrid );
  SetCheckBox( cboxGridBlastFP, qtGridFP );
  SetCheckBox( cboxNBody, qtNBody );
  SetCheckBox( cboxPlotLines, qtPlotLines );
  SetCheckBox( cboxPlotTrig, qtPlotTrig );
  SetCheckBox( cboxRandDots, qtRandomDots );
  SetCheckBox( cboxRandomCircle, qtCircles );
  SetCheckBox( cboxMaze, qtMaze );
  SetCheckBox( cboxTrig2, qtPlotTrig2 );
  SetCheckBox( cboxFern, qtFern );
  SetCheckBox( cboxRichEd, qtRichEd );
  SetCheckBox( cboxCalculatePi, qtPi );
  SetCheckBox( cboxDhry, qtDhrystone );
  SetCheckBox( cboxWhet, qtWhetstone );
  SetCheckBox( cboxBandwidthBP64, qtBandwidthBP64 );
  SetCheckBox( cboxLatency, qtMemLatency );
  SetCheckBox( cboxMazeThreads, qtMazeThreads );
  SetCheckBox( cboxOThreads, qtOrthogonalThreads );
  SetCheckBox( cboxIThreads, qtIdenticalThreads );
  SetCheckBox( cboxJpgDecode, qtJpgDecode );
  SetCheckBox( cboxImageResize, qtImageResize );
  SetCheckBox( cboxImageRotate, qtImageRotate );
  SetCheckBox( cboxMp3Encode, qtGogoEncode );
  SetCheckBox( cboxWebPageLoad, qtWebPageLoad );
  SetCheckBox( cboxZipCompress, qtZipCompress );
  SetCheckBox( cboxEncryptDecrypt, qtEncryptDecrypt );
  SetCheckBox( cboxFileCopy, qtFileCopy );
  SetCheckBox( cboxLorenz, qtLorenz );
  SetCheckBox( cboxNBodyOpenGL, qtNBodyOpenGL );
  SetCheckBox( cboxDhrystoneThreads, qtDhrystoneThreads );
  SetCheckBox( cboxWhetstoneThreads, qtWhetstoneThreads );
  SetCheckBox( cboxPiThreads, qtPiThreads );
  SetCheckBox( cboxMandelbrot, qtMandelbrotThreads );
  SetCheckBox( cboxEllipses, qtDrawEllipses );
  SetCheckBox( cboxRectangles, qtDrawRectangles );
  SetCheckBox( cboxLines, qtDrawLines );
  SetCheckBox( cboxFibThreads, qtFibThreads );
  SetCheckBox( cboxPngOut, qtPngOut );
  SetCheckBox( cbox7zip, qt7zip );
  SetCheckBox( cboxUpx, qtUpx );
  SetCheckBox( cboxNbodyFPU, qtNBodyFPU );
  SetCheckBox( cboxNbodySSE2, qtNBodySSE2 );
  SetCheckBox( cboxNbodySSE2Scalar, qtNBodySSE2Scalar );
  SetCheckBox( cboxNbodySSE3, qtNBodySSE3 );
  SetCheckBox( cboxSSE3, qtSse3 );
  SetCheckBox( cboxMontMul, qtMontMul );
  SetCheckBox( cboxSha1, qtSha1 );
  SetCheckBox( cboxSha256, qtSha256 );
  SetCheckBox( cboxAlphaBlend, qtAlphaBlend );
  SetCheckBox( cboxAlphaDots, qtAlphaDots );
  SetCheckBox( cboxLame, qtLame );
  SetCheckBox( cboxOgg, qtOgg );
  SetCheckBox( cboxCppCompile, qtCppCompiler );
  SetCheckBox( cboxMetaballs, qtMetaballs );
end; //procedure SetSelectedTests

procedure TfrmOSMarkSelectTests.SetIterations( Value : integer );
begin
  fIterations := Value;
  cboIterations.ItemIndex := cboIterations.Items.IndexOf( IntToStr( fIterations ) );
end;

procedure TfrmOSMarkSelectTests.SpeedButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TfrmOSMarkSelectTests.GetIterations : integer;
begin
  fIterations := strToInt( cboIterations.text );
  result := fIterations;
end; // function TfrmOSMarkSelectTests.GetIterations

procedure TfrmOSMarkSelectTests.SpeedButtonOkClick(Sender: TObject);
begin
  GetIterations;
  if fIterations < 1 then begin
    showmessage( 'Invalid choice for iterations: ' + IntToStr( fIterations ) );
    exit;
  end; // if
  ModalResult := mrOk;
end;

procedure TfrmOSMarkSelectTests.FormCreate(Sender: TObject);
begin
  CheckForInstructionSetSupport;
end;

procedure TfrmOSMarkSelectTests.CheckForInstructionSetSupport;
begin
  if NOT CpuidIsSse2Supported then begin
    fSelectedTests := fSelectedTests - [qtNBodySSE2, qtNBodySSE2Scalar, qtNBodySSE3];
    cboxNBodySSE2Scalar.Enabled := FALSE;
    cboxNBodySSE2Scalar.Checked := FALSE;
    cboxNBodySSE2.Enabled := FALSE;
    cboxNBodySSE2.Checked := FALSE;
    cboxNBodySSE3.Enabled := FALSE;
    cboxNBodySSE3.Checked := FALSE;
  end else if NOT CpuidIsSse3Supported then begin
    fSelectedTests := fSelectedTests - [qtNBodySSE3];
    cboxNBodySSE3.Enabled := FALSE;
    cboxNBodySSE3.Checked := FALSE;
  end; //if
end; // procedure TfrmOSMarkSelectTests.CheckForInstructionSetSupport;

procedure TfrmOSMarkSelectTests.CheckUncheckBoxes(ab_Checked : Boolean);
var
  i : integer;
  ChildControl : TControl;
begin
  for i := 0 to gbSelectTests.ControlCount - 1 do begin
    ChildControl := gbSelectTests.Controls[ i ];
    if ChildControl.ClassName = 'TCheckBox' then begin
      If ChildControl.Enabled then TCheckBox( ChildControl ).Checked := ab_Checked;
    end; // if
  end; // for
end; //procedure TfrmOSMarkSelectTests.ChechUncheckBoxes


procedure TfrmOSMarkSelectTests.btnSelectNoneClick(Sender: TObject);
begin
  CheckUncheckBoxes( FALSE );
end;

procedure TfrmOSMarkSelectTests.btnSelectAllClick(Sender: TObject);
begin
  CheckUncheckBoxes( TRUE );
end;

function TfrmOSMarkSelectTests.CountTests: integer;
var
  i : integer;
  ChildControl : TControl;
begin
  result := 0;
  for i := 0 to gbSelectTests.ControlCount - 1 do begin
    ChildControl := gbSelectTests.Controls[ i ];
    if ChildControl.ClassName = 'TCheckBox' then begin
      inc( result );
    end; // if
  end; // for
end; // function  TfrmOSMarkSelectTests.CountTests

procedure TfrmOSMarkSelectTests.FormActivate(Sender: TObject);
begin
  UnfadeFast( self );
end;

procedure TfrmOSMarkSelectTests.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FadeFast100( self );
end;

end.
