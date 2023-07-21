unit uThinClientTests;
{
  COSBI: Comprehensive Open Source Benchmarking Initiative
  Copyright (c) 2004 Van Smith

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

  The current web address of the GNU General Public License is:
  http://www.gnu.org/licenses/gpl.html

  You can contact the authors of this software at:
  cosbi@vanshardware.com
  See www.vanshardware.com or www.cosbi.org for more contact details.
}

//==============================================================================
// Unit name: uRdpTests
// Unit description: This unit houses most of the tests used in QuickTests.
// Author: Van Smith
// Date: October 29, 2004
// OS dependent: Yes: Windows
// Resolution dependent: No, but resolution and color depth will impact scores.
// External unit dependencies: COSBI_Common, uCOSBI_TestBaseClass, GR32_Blend,
//   GR32_Image, GR32, Maze, uTests
//==============================================================================
// Modification History
// ------------ -------
// Ver  Date   Who    Description
// ==== ====== ====== ==========================================================
// 1.0 041029 Van     Created.
//==============================================================================

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Grids, COSBI_Common, GR32_Blend, GR32_Image,
  GR32, ComCtrls, Gauges, Maze, uCOSBI_TTest, uCOSBI_TGraphicsTest, uOutput,
  jpeg, SHDocVw, OleCtrls, ZipForge, uTests;

type

  TThinClientTest = class( TGraphicsTest )
  private
  protected
  public
    procedure Run; override;
  end; // TRDPTest .............................................................

  // Class name: TtcPlotLinesTest
  // Author: Van Smith
  // Date: May 3, 2002
  TtcPlotLinesTest = class( TThinClientTest )
  private
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
  end; // TtcPlotLinesTest.........................................................

  // Class name: TtcMazeTest
  // Author: Van Smith
  // Date: May 3, 2002
  TtcMazeTest = class( TThinClientTest )
  private
    fMaze : TMaze;
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    procedure BeforeTest; Override;
    procedure AfterTest; Override;
  end; // TtcMazeTest.........................................................

  // Class name: TtcFilledCircleTest
  // Author: Van Smith
  // Date: May 3, 2002
  TtcFilledCircleTest = class( TThinClientTest )
  private
    fMaze : TMaze;
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
//    procedure BeforeTest; Override;
//    procedure AfterTest; Override;
  end; // TtcFilledCircleTest...................................................

  // Class name: TtcFernTest
  // Author: Van Smith
  // Date: September 24, 2002
  TtcFernTest = class( TThinClientTest )
  private
    fPiOver180 : double;
  protected
    procedure DrawFern( x, y, r, theta: double );
  public
    constructor Create; Overload;
    procedure RunTest; Override;
  end; // TtcFernTest.........................................................

  // Class name: TtcGridBlastTest
  // Author: Van Smith
  // Date: September 24, 2002
  TtcGridBlastTest = class( TThinClientTest )
  private
    fGrid : TStringGrid;
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    procedure BeforeTest; Override;
    procedure AfterTest; Override;
    property Grid : TStringGrid read fGrid write fGrid;
  end; // TtcGridBlastTest.....................................................

  // Class name: TtcRichEditTest
  // Author: Van Smith
  // Date: September 24, 2002
  TtcRichEditTest = class( TThinClientTest )
  private
    fRichEdit   : TRichEdit;
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    procedure BeforeTest; Override;
    procedure AfterTest; Override;
  end; // TtcRichEditTest.....................................................

  // Class name: TtcImageRotateTest
  // Author: Van Smith
  // Date: January 23, 2004
  TtcImageRotateTest = class( TThinClientTest )
  private
    // we don't need any private field variables or procedures
  protected
    fImage32 : TImage32; //Image32 is more flexible than the TImage component
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    procedure BeforeTest; Override;
    procedure AfterTest; Override;
  end; // TtcImageRotateTest....................................................

  // Class name: TtcImageResizeTest
  // Author: Van Smith
  // Date: January 23, 2004
  TtcImageResizeTest = class( TThinClientTest )
  private
    // we don't need any private field variables or procedures
  protected
    fImage32 : TImage32; //Image32 is more flexible than the TImage component
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    procedure BeforeTest; Override;
    procedure AfterTest; Override;
  end; // TtcImageResizeTest....................................................

  // Class name: TWebPageLoad
  // Author: Van Smith
  // Date: January 26, 2004
  TtcWebPageLoad = class( TThinClientTest )
  private
    // we don't need any private field variables or procedures
  protected
    fWebBrowser : TWebBrowser;
  public
    constructor Create; Overload;
    procedure RunTest; Override;
    procedure BeforeTest; Override;
    procedure AfterTest; Override;
  end; // TtcWebPageLoad.........................................................

  // Class name: TDrawLinesTest
  // Author: Van Smith
  // Date: December 16, 2004
  TtcDrawLinesTest = class( TThinClientTest )
  private
    // we don't need any private field variables or procedures
    fNumberOfLines : integer;
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
  end; // TtcDrawLinesTest....................................................

  // Class name: TtcDrawEllipsesTest
  // Author: Van Smith
  // Date: December 16, 2004
  TtcDrawEllipsesTest = class( TThinClientTest )
  private
    // we don't need any private field variables or procedures
    fNumberOfEllipses : integer;
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
  end; // TtcDrawEllipsesTest....................................................

  // Class name: TtcDrawRectanglesTest
  // Author: Van Smith
  // Date: December 16, 2004
  TtcDrawRectanglesTest = class( TThinClientTest )
  private
    // we don't need any private field variables or procedures
    fNumberOfRectangles : integer;
  protected
  public
    constructor Create; Overload;
    procedure RunTest; Override;
  end; // TtcDrawRectanglesTest....................................................

const
  THIN_CLIENT_TESTS = [qtTCCircles, qtTCDrawEllipses, qtTCFern, qtTCGrid,
    qtTCImageResize, qtTCImageRotate, qtTCDrawLines, qtTCMaze, qtTCPlotLines,
    qtTCDrawRectangles, qtTCRichEd, qtTCWebPageLoad];

implementation

// TThinClientTest begins........................................................
  procedure TThinClientTest.Run;
    procedure RandomDots;
    var
      x, y, z : integer;
    begin
      Application.ProcessMessages;
      with fPaintBox32 do begin
        // Random dots:
        RandSeed := 1;
        for z := 1 to 20000 do
        begin
          x := Random(Width);
          y := Random(Height);
          case Random(6) of
            0: Canvas.Pixels[x, y] := clBlack;
            1: Canvas.Pixels[x, y] := clGreen;
            2: Canvas.Pixels[x, y] := clBlue;
            3: Canvas.Pixels[x, y] := clRed;
            4: Canvas.Pixels[x, y] := clWhite;
            5: Canvas.Pixels[x, y] := clYellow;
          end;
        end;
      end; // with
      Application.ProcessMessages;
    end; // procedure RandomDots;
  begin
    if fStopWatch = nil then begin
      raise Exception.Create( fTestName + ERR_NO_STOPWATCH );
    end;
    try
      BeforeTest;
      fStopWatch.StartTimer;
      RunTest;
      // try to force RDP server to wait:
      RandomDots;
      fStopWatch.StopTimer;
      fRunCount := fRunCount + 1;
      fLastTime := fStopWatch.ElapsedTime;
      fTotalTime := fTotalTime + fLastTime;
      if fLastTime > fMaxTime then fMaxTime := fLastTime;
      if fLastTime < fMinTime then fMinTime := fLastTime;
    finally
      AfterTest;
    end;
  end; // procedure TThinClientTest.Run;
// TThinClientTest ends.........................................................

// TtcPlotLinesTest begins........................................................
  constructor TtcPlotLinesTest.Create;
  begin
    inherited;
    fTestName   := 'Thin Client Plot Lines';
    fTestDescription := 'Plots lines a pixel at a time';
    fTestVersion:= '1.0';
    fTestType   := ttThinClient;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 7.708;
    fQTestType := qtTCPlotLines;
  end;

  procedure TtcPlotLinesTest.RunTest;
  var
    x : integer;
    y : integer;
  begin
    with fPaintBox32 do begin
      for x := Width downto 0 do
        for y := Height downto 0 do
          Canvas.Pixels[x, y] := clRed;
      for x := Width downto 0 do
        for y := Height downto 0 do
          Canvas.Pixels[x, y] := clBlack;
      Application.ProcessMessages;
      for y := Height downto 0 do
        for x := Width downto 0 do
          Canvas.Pixels[x, y] := clRed;
      for y := Height downto 0 do
        for x := Width downto 0 do
          Canvas.Pixels[x, y] := clBlack;
      Application.ProcessMessages;
      for x := 0 to Width do
        for y := 0 to Height do
          Canvas.Pixels[x, y] := clRed;
      for x := 0 to Width do
        for y := 0 to Height do
          Canvas.Pixels[x, y] := clBlack;
      Application.ProcessMessages;
      for y := 0 to Height do
        for x := 0 to Width do
          Canvas.Pixels[x, y] := clRed;
      for y := 0 to Height do
        for x := 0 to Width do
          Canvas.Pixels[x, y] := clBlack;
    end; // with
  end; // procedure TtcPlotLinesTest.RunTest;
// TtcPlotLinesTest ends..........................................................

// TtcMazeTest begins.............................................................
  constructor TtcMazeTest.Create;
  begin
    inherited;
    fTestName   := 'Thin Client Maze';
    fTestDescription := 'Generates mazes and graphically solves them';
    fTestVersion:= '1.0';
    fTestType   := ttThinClient;
    fTestAuthor := 'Van Smith, Chris Yeager';
    fReferenceTime := 7.082;
    fQTestType := qtTCMaze;
  end;

  procedure TtcMazeTest.BeforeTest;
  begin
    inherited;
    fMaze := TMaze.Create( fPaintBox32 );
  end; // procedure TtcMazeTest.BeforeTest

  procedure TtcMazeTest.RunTest;
  var
    n : integer;
  begin
    RandSeed := -1;
    for n := 1 to 20 do begin
      with fMaze do begin
        Make(250, 250);
        Solve(True);
      end;// with lMaze
    end;
  end; // procedure TtcMazeTest.RunTest;

  procedure TtcMazeTest.AfterTest;
  begin
    inherited;
    FreeAndNil( fMaze );
  end; // procedure TtcMazeTest.BeforeTest
// TtcMazeTest ends...............................................................

// TtcFilledCircleTest begins.....................................................
  constructor TtcFilledCircleTest.Create;
  begin
    inherited;
    fTestName   := 'Thin Client Filled Circles';
    fTestDescription := 'Paints random filled circles';
    fTestVersion:= '1.0';
    fTestType   := ttThinClient;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 2.196;
    fQTestType := qtTCCircles;
  end;

  procedure TtcFilledCircleTest.RunTest;
  var
    x : integer;
    y : integer;
    z : integer;
    r : integer;
  begin
    with fPaintBox32 do begin
      Canvas.Pen.Color := clRed;
      Canvas.Brush.Color := clBlack;
      Canvas.Brush.Style := bsSolid;
      RandSeed := 1;
      for z := 1 to 10000 do
      begin
        x := Random(Width);
        y := Random(Height);
        r := Random(Height div 2);
        case Random(6) of
          0: Canvas.Brush.Color := clBlack;
          1: Canvas.Brush.Color := clGreen;
          2: Canvas.Brush.Color := clBlue;
          3: Canvas.Brush.Color := clRed;
          4: Canvas.Brush.Color := clWhite;
          5: Canvas.Brush.Color := clYellow;
        end;
        Canvas.Ellipse(x - r, y - r, x + r, y + r);
        //Canvas.Pixels[x, y] := clWhite;
      end;
    end; // with
  end; // procedure TtcFilledCircleTest.RunTest;
// TtcFilledCircleTest ends.......................................................

// TtcFernTest begins.............................................................
  constructor TtcFernTest.Create;
  begin
    inherited;
    fTestName   := 'Thin Client Fern Fractal';
    fTestDescription := 'Calculates and plots two fern fractals';
    fTestVersion:= '1.0';
    fTestType   := ttThinClient;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 2.23817;
    fQTestType := qtTCFern;
    fPiOver180 := PI / 180;
  end;

  procedure TtcFernTest.DrawFern( x, y, r, theta: double );
  const
    F1 = 0.4;
    F2 = 0.85;
    W1 = 35;
    W2 = 5;
  var
    x1, y1, t : double;
  begin
    // this program measures theta clockwise relative to the y=axis
    with fPaintBox32 do begin
      t := theta * fPiOver180;
      x1 := x + r * sin( t );
      y1 := y - r * cos( t );
      Canvas.MoveTo(round(x), round(y));
      Canvas.LineTo(round(x1),round(y1));
      if r > 0.5 then begin
        DrawFern(x1, y1, r * F1, theta + W1);
        DrawFern(x1, y1, r * F1, theta - W1);
        DrawFern(x1, y1, r * F2, theta + W2);
      end; // if
    end; // with
  end; // procedure TtcFernTest.DrawFern

  procedure TtcFernTest.RunTest;
  begin
    fPaintBox32.Canvas.Pen.Color := clLime;
    DrawFern(100, 130, 160, 100);
    fPaintBox32.Canvas.Pen.Color := clRed;
    DrawFern(10, 500, 160, 60);
  end; // procedure TtcFernTest.RunTest;
// TtcFernTest ends...............................................................

// TtcGridBlastTest begins........................................................
  constructor TtcGridBlastTest.Create;
  begin
    inherited;
    fTestName   := 'Thin Client Grid Blast';
    fTestDescription := 'Populates a grid and iterates through data';
    fTestVersion:= '1.0';
    fTestType   := ttThinClient;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 1.8727;
    fQTestType := qtTCGrid;
  end;

  procedure TtcGridBlastTest.RunTest;
  const
    ITERATIONS = 30;
  var
    li_row      : integer;
    li_column   : integer;
    li_repeat   : integer;
    li_temp     : integer;
    x, y, z     : integer;

    procedure InitGrid;
    var
      li_row      : integer;
      li_column   : integer;
    begin
      with fGrid do begin
        for li_row := 1 to RowCount - 1 do begin
          for li_column := 1 to ColCount - 1 do begin
            Cells[li_column, li_row] := '1';
          end;
        end;
      end;
      Application.ProcessMessages;
    end;

  begin
    with fGrid do begin
      Visible := TRUE;
      RowCount := 81;
      ColCount := 21;
      InitGrid;
      // sum
      for li_repeat := 1 to ITERATIONS do begin
        for li_row := 1 to RowCount - 1 do begin
          for li_column := 1 to ColCount - 1 do begin
            li_temp := StrToInt(Cells[li_column, li_row]) + 1;
            Cells[li_column, li_row] := IntToStr( li_temp );
          end;
        end;
        Application.ProcessMessages;
      end;
      // subtract
      for li_repeat := 1 to ITERATIONS do begin
        for li_row := 1 to RowCount - 1 do begin
          for li_column := 1 to ColCount - 1 do begin
            li_temp := StrToInt(Cells[li_column, li_row]) - 1;
            Cells[li_column, li_row] := IntToStr( li_temp );
          end;
        end;
        Application.ProcessMessages;
      end;
      InitGrid;
      // multiply
      for li_repeat := 1 to ITERATIONS do begin
        for li_row := 1 to RowCount - 1 do begin
          for li_column := 1 to ColCount - 1 do begin
            li_temp := StrToInt(Cells[li_column, li_row]) * 2;
            Cells[li_column, li_row] := IntToStr( li_temp );
          end;
        end;
        Application.ProcessMessages;
      end;
      // integer divide
      for li_repeat := 1 to ITERATIONS do begin
        for li_row := 1 to RowCount - 1 do begin
          for li_column := 1 to ColCount - 1 do begin
            li_temp := StrToInt(Cells[li_column, li_row]) div 2;
            Cells[li_column, li_row] := IntToStr( li_temp );
          end;
        end;
        Application.ProcessMessages;
      end;
      Visible := FALSE;
      Application.ProcessMessages;
    end; // with
  end; // procedure TGridBlastTest.RunTest;

  procedure TtcGridBlastTest.BeforeTest;
  begin
    inherited;
    fGrid := TStringGrid.Create( fOutputForm );
    fGrid.Parent := fOutputForm;
    fGrid.BringToFront;
    fGrid.Align := alClient;
    fGrid.SetFocus;
    fGrid.ColCount := 20;
    fGrid.DefaultColWidth := 48;
    fGrid.DefaultRowHeight := 16;
    fGrid.FixedCols := 1;
    fGrid.FixedRows := 1;
    fGrid.RowCount := 1000;
    fGrid.ScrollBars := ssBoth;
  end; // procedure TGridBlastTest.BeforeTest

  procedure TtcGridBlastTest.AfterTest;
  begin
    inherited;
    FreeAndNil( fGrid );
  end; // procedure TGridBlastTest.BeforeTest
// TtcGridBlastTest ends..........................................................

// TtcRichEditTest begins.........................................................
  constructor TtcRichEditTest.Create;
  begin
    inherited;
    fTestName   := 'Thin Client RichEdit';
    fTestDescription := 'Performs common text editing tasks. The RichEdit control is used throughout Windows and is the basis for WordPad and some word processors.';
    fTestVersion:= '1.0';
    fTestType   := ttThinClient;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 5.35328;
    fQTestType := qtTCRichEd;
  end; // constructor TtcRichEditTest.Create;

  procedure TtcRichEditTest.RunTest;
  const
    CR_LF = #13#10;
  var
    i, j : integer;

    function RandomWord(abol_Uppercase : boolean): string;
    var
      WordLength : integer;
      i           : integer;
      randomChar  : integer;
    begin
      result := '';
      WordLength := Random(11) + 1;
      for i := 1 to WordLength do begin
        randomChar := Random(25);
        if (i = 1) and abol_Uppercase then begin
          result := result + chr(randomChar + 65);
        end else begin
          result := result + chr(randomChar + 97);
        end; // if
      end; // for
    end; // function RandomWord

    function RandomSentence: string;
    var
      SentenceLength : integer;
      i              : integer;
    begin
      result := '';
      SentenceLength := random(10) + 1;
      for i := 1 to SentenceLength do begin
        result := result + RandomWord(i = 1);
        if i < SentenceLength then begin
          result := result + ' ';
        end else begin
          result := result + '.';
        end; // if
      end; // for
    end; // function RandomSentence

    function RandomParagraph: string;
    var
      ParagraphLength : integer;
      i               : integer;
    begin
      result := '';
      ParagraphLength := random(16) + 1;
      for i := 1 to ParagraphLength do begin
        result := result + RandomSentence;
        if i <> 1 then begin
          result := result + '  ';
        end else begin
          result := result + CR_LF + CR_LF;
        end; // if
      end; // for
    end; // function RandomParagraph

    function RandomChapter: string;
    var
      ChapterLength   : integer;
      i               : integer;
    begin
      result := '';
      ChapterLength := random(16) + 4;
      for i := 1 to ChapterLength do begin
        fRichEdit.Lines.Add(RandomParagraph);
        fRichEdit.Lines.Add(CR_LF);
        fRichEdit.Lines.Add(CR_LF);
        Application.ProcessMessages;
      end; // for
    end; // function RandomChapter

  begin
    randseed := 1;
    with fRichEdit do begin
      Visible := TRUE;
      for i := 1 to 100 do begin
        Paragraph.Numbering := nsNone;
        SelAttributes.Size := 20;
        SelAttributes.Style := [fsBold];
        Paragraph.Alignment := taCenter;
        Lines.Add('Chapter ' + inttostr( i ));
        Paragraph.Alignment := taLeftJustify;
        SelAttributes.Size := 12;
        SelAttributes.Style := [];
        RandomChapter;
        Paragraph.Numbering := nsBullet;
        for j := 1 to 10 do begin
          SelAttributes.Style := [fsItalic];
          Lines.Add(RandomSentence);
        end; //for
      end; //for
      Visible := FALSE;
      Application.ProcessMessages;
    end; // with
  end; // procedure TtcRichEditTest.RunTest;

  procedure TtcRichEditTest.BeforeTest;
  begin
    inherited;
    fRichEdit := TRichEdit.Create(fOutputForm);
    fRichEdit.Parent := fOutputForm;
    fRichEdit.BringToFront;
    fRichEdit.Align := alClient;
    fRichEdit.Lines.Clear;
    fRichEdit.SetFocus;
    fRichEdit.DefAttributes.Name := 'Times New Roman';
    fRichEdit.DefAttributes.Size := 12;
  end; // procedure TRichEditTest.BeforeTest

  procedure TtcRichEditTest.AfterTest;
  begin
    inherited;
    fRichEdit.ClearUndo;
    fRichEdit.Lines.Clear;
    fRichEdit.Clear;
    if assigned( fRichEdit ) then fRichEdit.Free;
    fRichEdit := NIL;
  end; // procedure TtcRichEditTest.BeforeTest
// TtcRichEditTest ends...............................................................

// TtcImageRotateTest.............................................................
  constructor TtcImageRotateTest.Create;
  begin
    inherited;
    fTestName   := 'Thin Client Image Rotate';
    fTestDescription := 'Rotates an image 90 degrees at a time.';
    fTestVersion:= '1.0';
    fTestType   := ttThinClient;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 6.227;
    fQTestType := qtTCImageRotate;
  end; // constructor TtcImageRotateTest.Create;

  procedure TtcImageRotateTest.BeforeTest;
  const
    // assumes both of the pictures are in the same folder as the calling program.
    JPG_FILE = 'osmfiles\pic3.jpg';
  var
    lsFilePath : string;
  begin
    inherited;
    // PaintBox32 does not get along with the image component, so make it invisible:
    fPaintBox32.Visible := FALSE;
    // create the image componest which will do the JPG decoding
    fImage32 := TImage32.Create(fOutputForm);
    // setting the parent to the output to ensure the component is freed
    fImage32.Parent := fOutputForm;
    // bring the component to the front of any other controls on the form
    fImage32.BringToFront;
    // make the image take over the entire form
    fImage32.Align := alClient;
    // since images are usually stretched/compressed to fit the screen, when
    // users look at them let's set stretch to TRUE
    fImage32.ScaleMode := smStretch;
    // load the image:
    lsFilePath := extractFilePath( application.ExeName );
    fImage32.Bitmap.LoadFromFile( lsFilePath + JPG_FILE );
    // allow the screen to update:
    application.ProcessMessages;
  end; // procedure TtcImageRotateTest.BeforeTest

  procedure TtcImageRotateTest.RunTest;
  var
    i : integer;
  begin
    for i := 1 to 12 do begin
      fImage32.Bitmap.Rotate90();
      // allow the screen to update:
      application.ProcessMessages;
    end; // for
    fImage32.Visible := FALSE;
    fPaintBox32.Visible := TRUE;
    application.ProcessMessages;
  end; // procedure TtcImageRotateTest.RunTest;

  procedure TtcImageRotateTest.AfterTest;
  begin
    inherited;
    // dispose of the image component to prevent memory leaks
    FreeAndNil( fImage32 );
  end; // procedure TtcImageRotateTest.BeforeTest
// TtcImageRotateTest ends........................................................

// TtcImageResizeTest.............................................................
  constructor TtcImageResizeTest.Create;
  begin
    inherited;
    fTestName   := 'Thin Client Image Resize';
    fTestDescription := 'gradually resizes an image larger then smaller';
    fTestVersion:= '1.0';
    fTestType   := ttThinClient;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 3.0785;
    fQTestType := qtTCImageResize;
  end; // constructor TtcImageResizeTest.Create;

  procedure TtcImageResizeTest.BeforeTest;
  const
    // assumes both of the pictures are in the same folder as the calling program.
    JPG_FILE = 'osmfiles\pic4.jpg';
  var
    lsFilePath : string;
  begin
    inherited;
    // PaintBox32 does not get along with the image component, so make it invisible:
    fPaintBox32.Visible := FALSE;
    // create the image componest which will do the JPG decoding
    fImage32 := TImage32.Create(fOutputForm);
    // setting the parent to the output to ensure the component is freed
    fImage32.Parent := fOutputForm;
    // bring the component to the front of any other controls on the form
    fImage32.BringToFront;
    // make the image take over the entire form
    fImage32.Align := alClient;
    // set to scale mode to enable zoom:
    fImage32.ScaleMode := smScale;
    // make image little:
    fImage32.Scale := 0.001;
    // place image in the center of view:
    fImage32.BitmapAlign := baCenter;
    // load the image:
    lsFilePath := ExtractFilePath( application.exename );
    fImage32.Bitmap.LoadFromFile( lsFilePath + JPG_FILE );
    // allow the screen to update:
    application.ProcessMessages;
  end; // procedure TtcImageResizeTest.BeforeTest

  procedure TtcImageResizeTest.RunTest;
  var
    i : integer;
  begin
    for i := 1 to 200 do begin
      fImage32.Scale := i / 100;
      // allow the screen to update:
      application.ProcessMessages;
    end; // for
    for i := 200 downto 1 do begin
      fImage32.Scale := i / 100;
      // allow the screen to update:
      application.ProcessMessages;
    end; // for
    fImage32.Visible := FALSE;
    fPaintBox32.Visible := TRUE;
    application.ProcessMessages;
  end; // procedure TtcImageResizeTest.RunTest;

  procedure TtcImageResizeTest.AfterTest;
  begin
    inherited;
    // dispose of the image component to prevent memory leaks
    FreeAndNil( fImage32 );
  end; // procedure TtcImageResizeTest.BeforeTest
// TtcImageResizeTest ends........................................................

// TtcWebPageLoad.................................................................
  constructor TtcWebPageLoad.Create;
  begin
    inherited;
    fTestName   := 'Thin Client Web Page Load';
    fTestDescription := 'This test measures system performance loading web pages';
    fTestVersion:= '1.0';
    fTestType   := ttThinClient;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 12.6553;
    fQTestType := qtTCWebPageLoad;
  end; // constructor TWebPageLoad.Create;

  procedure TtcWebPageLoad.BeforeTest;
  begin
    inherited;
    // PaintBox32 does not get along with the some components, so make it invisible:
    fPaintBox32.Visible := FALSE;
    // create the browser component
    fWebBrowser := TWebBrowser.Create(fOutputForm);
    TWinControl(fWebBrowser).parent := fOutputForm;
    // bring the component to the front of any other controls on the form
    fWebBrowser.BringToFront;
    fWebBrowser.Visible := TRUE;
    // make the browser take over the entire form
    fWebBrowser.Align := alClient;
    application.ProcessMessages;
  end; // procedure TtcWebPageLoad.BeforeTest

  procedure TtcWebPageLoad.RunTest;
  const
    WEB_PAGE_1 = 'osmfiles\html\Van''s Hardware Journal  Home.htm';
    WEB_PAGE_2 = 'osmfiles\pic7.jpg';
    WEB_PAGE_3 = 'osmfiles\html\011116_Xbox.htm';
    WEB_PAGE_4 = 'osmfiles\OSMarkDocumentation.html';
    WEB_PAGE_5 = 'osmfiles\html\020822_AthlonXP2600.htm';
    WEB_PAGE_6 = 'osmfiles\html\020107_AthlonXP2000.htm';
    WEB_PAGE_7 = 'osmfiles\html\010709_Arcos_Jukebox_6000.htm';
  var
    i : integer;
    s : string;
  begin
    s := ExtractFilePath(Application.ExeName);
    for i := 1 to 10 do begin
      fWebBrowser.Navigate(s + WEB_PAGE_1);
      application.ProcessMessages;
      while (fWebBrowser.ReadyState <> READYSTATE_COMPLETE) do begin
        application.ProcessMessages;
      end; // do
      application.ProcessMessages;
      fWebBrowser.Navigate(s + WEB_PAGE_2);
      application.ProcessMessages;
      while (fWebBrowser.ReadyState <> READYSTATE_COMPLETE) do begin
        application.ProcessMessages;
      end; // do
      application.ProcessMessages;
      fWebBrowser.Navigate(s + WEB_PAGE_3);
      application.ProcessMessages;
      while (fWebBrowser.ReadyState <> READYSTATE_COMPLETE) do begin
        application.ProcessMessages;
      end; // do
      application.ProcessMessages;
      fWebBrowser.Navigate(s + WEB_PAGE_4);
      application.ProcessMessages;
      while (fWebBrowser.ReadyState <> READYSTATE_COMPLETE) do begin
        application.ProcessMessages;
      end; // do
      application.ProcessMessages;
      fWebBrowser.Navigate(s + WEB_PAGE_5);
      application.ProcessMessages;
      while (fWebBrowser.ReadyState <> READYSTATE_COMPLETE) do begin
        application.ProcessMessages;
      end; // do
      application.ProcessMessages;
      fWebBrowser.Navigate(s + WEB_PAGE_6);
      application.ProcessMessages;
      while (fWebBrowser.ReadyState <> READYSTATE_COMPLETE) do begin
        application.ProcessMessages;
      end; // do
      application.ProcessMessages;
      fWebBrowser.Navigate(s + WEB_PAGE_7);
      application.ProcessMessages;
      while (fWebBrowser.ReadyState <> READYSTATE_COMPLETE) do begin
        application.ProcessMessages;
      end; // do
      application.ProcessMessages;
    end; // for
    fWebBrowser.Visible := TRUE;
    // make the browser take over the entire form
    fWebBrowser.Visible := FALSE;
    FreeAndNil( fWebBrowser );
    // restore fPaintBox32 visibility:
    fPaintBox32.Visible := TRUE;
    application.ProcessMessages;
  end; // procedure TtcWebPageLoad.RunTest;

  procedure TtcWebPageLoad.AfterTest;
  begin
    inherited;
  end; // procedure TtcWebPageLoad.BeforeTest
// TtcWebPageLoad ends........................................................

// TtcDrawLinesTest begins.......................................................
  constructor TtcDrawLinesTest.Create;
  begin
    inherited;
    fNumberOfLines := 1000000;
    fTestName   := 'Thin Client Draw Lines Test';
    fTestDescription := 'Draws ' + intToStr( fNumberOfLines ) + ' lines randomly over canvas';
    fTestVersion:= '1.0';
    fTestType   := ttThinClient;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 5.91642;
    fQTestType := qtTCDrawLines;
  end;

  procedure TtcDrawLinesTest.RunTest;
  var
    i, x1, y1, x2, y2, lImageHeight, lImageWidth : integer;
    lRed, lGreen, lBlue : byte;
    lColor : TColor;
  begin
    RandSeed := 1;
    with fPaintBox32 do begin
      lImageHeight := Height;
      lImageWidth := Width;
      for i := 0 to fNumberOfLines do begin
        x1 := Random( lImageWidth );
        x2 := Random( lImageWidth );
        y1 := Random( lImageHeight );
        y2 := Random( lImageHeight );
        lRed := random( 256 );
        lGreen := random( 256 );
        lBlue := random( 256 );
        lColor := lBlue * $010000 + lGreen * $000100 + lRed;
        Canvas.Pen.Color := lColor;
        Canvas.MoveTo( x1, y1 );
        Canvas.LineTo( x2, y2 );
      end; // for
    end; // width
  end; // procedure TtcDrawLinesTest.RunTest;
// TtcDrawLinesTest ends.........................................................

// TtcDrawEllipsesTest begins.......................................................
  constructor TtcDrawEllipsesTest.Create;
  begin
    inherited;
    fNumberOfEllipses := 100000;
    fTestName   := 'Thin Client Draw Ellipses';
    fTestDescription := 'Draws ' + intToStr( fNumberOfEllipses ) + ' ellipses randomly over canvas';
    fTestVersion:= '1.0';
    fTestType   := ttThinClient;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 5.73937;
    fQTestType := qtTCDrawEllipses;
  end;

  procedure TtcDrawEllipsesTest.RunTest;
  var
    i, x1, y1, x2, y2 : integer;
    lRed, lGreen, lBlue : byte;
    lColor : TColor;
    lImageHeight : integer;
    lImageWidth : integer;
  begin
    with fPaintBox32 do begin
      // Random dots:
      RandSeed := 1;
      lImageHeight := Height;
      lImageWidth := Width;
      for i := 0 to 100000 do begin
        x1 := Random( lImageWidth );
        x2 := Random( lImageWidth );
        y1 := Random( lImageHeight );
        y2 := Random( lImageHeight );
        lRed := random( 256 );
        lGreen := random( 256 );
        lBlue := random( 256 );
        lColor := lBlue * $010000 + lGreen * $000100 + lRed;
        Canvas.Pen.Color := lColor;
        Canvas.Brush.Style := bsClear;
        Canvas.Ellipse( x1, y1, x2, y2 );
      end; // for
    end; // with
  end; // procedure TtcDrawEllipsesTest.RunTest;
// TtcDrawEllipsesTest ends.......................................................

// TtcDrawRectanglesTest begins.......................................................
  constructor TtcDrawRectanglesTest.Create;
  begin
    inherited;
    fNumberOfRectangles := 1000000;
    fTestName   := 'Thin Client Draw Rectangles';
    fTestDescription := 'Draws ' + intToStr( fNumberOfRectangles ) + ' rectangles randomly over canvas';
    fTestVersion:= '1.0';
    fTestType   := ttThinClient;
    fTestAuthor := 'Van Smith';
    fReferenceTime := 7.28314;
    fQTestType := qtTCDrawRectangles;
  end;

  procedure TtcDrawRectanglesTest.RunTest;
  var
    i, x1, y1, x2, y2 : integer;
    lRed, lGreen, lBlue : byte;
    lColor : TColor;
    lImageHeight : integer;
    lImageWidth : integer;
  begin
    with fPaintBox32 do begin
      // Random dots:
      RandSeed := 1;
      lImageHeight := Height;
      lImageWidth := Width;
      for i := 0 to fNumberOfRectangles do begin
        x1 := Random( lImageWidth );
        x2 := Random( lImageWidth );
        y1 := Random( lImageHeight );
        y2 := Random( lImageHeight );
        lRed := random( 256 );
        lGreen := random( 256 );
        lBlue := random( 256 );
        lColor := lBlue * $010000 + lGreen * $000100 + lRed;
        Canvas.Pen.Color := lColor;
        Canvas.Brush.Style := bsClear;
        Canvas.Rectangle( x1, y1, x2, y2 );
      end; // for
    end; // with
  end; // procedure TtcDrawRectanglesTest.RunTest;
// TtcDrawRectanglesTest ends.........................................................

end.
