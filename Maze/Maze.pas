unit Maze;
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
interface

uses
  Classes, ExtCtrls, Math, Graphics, Forms, Dialogs, SysUtils, COSBI_Common,
  GR32_Image, GR32;

type
  TMaze = class( TObject )
  private
    { Private declarations }
    FImage     : TPaintBox32;
    FWidth     : integer;
    FHeight    : integer;
    FMaze      : array of array of Boolean; // Array telling if the cell is used
    FMazeWalls : array of array of Byte;    // Array telling what walls.
    Fmap       : array of integer;
    Fmappos    : integer;
    FpxScale, FpyScale : Double;
    Fpx, Fpy           : Double;
    FMidpx, FMidpy     : Double;
    FxScale, FyScale   : Double;
    FmxSize, FmySize   : Double;

    procedure   SetupWalls;
    procedure   GenerateMaze;
    function    GetChoicesForCell( x, y : integer;
                                   var Achs : array of integer) : Integer;
    function    GetChoicesForSolve( x, y : integer;
                                  var Achs : array of integer) : Integer;
    procedure   DrawWall( x, y, w : integer);
    procedure   DrawSolveLine( x, y, d : integer);
  public
    { Public declarations }
    constructor Create( AImage : TPaintBox32 );
    destructor  Destroy; override;
    procedure   Draw;
    procedure   Print( solution : boolean );
    procedure   Solve( ShowProg : boolean );
    procedure   Make( AWidth, AHeight : integer );
  end;

//****************************************************************************//
//**  Description of walls...                                               **//
//**                                                                        **//
//**        5           To store in one byte of information which wall is   **//
//**       ____         on and which one is off using binary to give each   **//
//**    4 /    \ 0      wall a value.  The value is calculated by taking    **//
//**     /      \       2 to the power of the wall number...                **//
//**     \      /                                                           **//
//**    3 \____/ 1      Wall    Value                                       **//
//**        2            0       1                                          **//
//**                     1       2                                          **//
//**                     2       4                                          **//
//**                     3       8                                          **//
//**                     4       16                                         **//
//**                     5       32                                         **//
//**                            =====                                       **//
//**                             63  ---> All walls                         **//
//**                                                                        **//
//****************************************************************************//
//**  The grid is calculated like this...                                   **//
//**                                                                        **//
//**    ____        ____        ____        ____                            **//
//**   /    \      /    \      /    \      /    \     The sum of the two    **//
//**  / 0, 0 \____/ 0, 2 \____/ 0, 4 \____/ 0, 6 \    coordinates is        **//
//**  \      /    \      /    \      /    \      /    always an even        **//
//**   \____/ 1, 1 \____/ 1, 3 \____/ 1, 5 \____/     number.               **//
//**   /    \      /    \      /    \      /    \                           **//
//**  / 2, 0 \____/ 2, 2 \____/ 2, 4 \____/ 2, 6 \    The coordinates are   **//
//**  \      /    \      /    \      /    \      /    shown in Y or row     **//
//**   \____/ 3, 1 \____/ 3, 3 \____/ 3, 5 \____/     then X or column      **//
//**   /    \      /    \      /    \      /    \     form.                 **//
//**  / 4, 0 \____/ 4, 2 \____/ 4, 4 \____/ 4, 6 \                          **//
//**  \      /    \      /    \      /    \      /                          **//
//**   \____/ 5, 1 \____/ 5, 3 \____/ 5, 5 \____/                           **//
//**   /    \      /    \      /    \      /    \                           **//
//**  / 6, 0 \____/ 6, 2 \____/ 6, 4 \____/ 6, 6 \                          **//
//**  \      /    \      /    \      /    \      /                          **//
//**   \____/ 7, 1 \____/ 7, 3 \____/ 7, 5 \____/                           **//
//**   /    \      /    \      /    \      /    \                           **//
//**  / 8, 0 \____/ 8, 2 \____/ 8, 4 \____/ 8, 6 \                          **//
//**  \      /    \      /    \      /    \      /                          **//
//**   \____/      \____/      \____/      \____/                           **//
//**                                                                        **//
//****************************************************************************//



const
  ALL_WALLS                     = 63;

  XDIR : array[0..5] of integer = (  1,  1,  0, -1, -1,  0 );
  YDIR : array[0..5] of integer = ( -1,  1,  2,  1, -1, -2 );
  REVD : array[0..5] of integer = (  3,  4,  5,  0,  1,  2 );
  XHEX : array[0..6] of integer = (  5,  7,  5,  2,  0,  2,  5 );
  YHEX : array[0..6] of integer = (  0,  3,  6,  6,  3,  0,  0 );
  WALL : array[0..5] of integer = (  1,  2,  4,  8, 16, 32 );

  WALL_COLOR                    = clBlack;
  BACK_COLOR                    = clWhite;
  SOLVE_COLOR                   = clBlue;
  WALL_COLOR32                  = clBlack32;
  BACK_COLOR32                  = clWhite32;
  SOLVE_COLOR32                 = clBlue32;

implementation

uses
  Printers;

constructor TMaze.Create( AImage : TPaintBox32 );
begin
  inherited Create;
  FImage := AImage;
end;

destructor TMaze.Destroy;
begin
  inherited Destroy;
end;

procedure TMaze.Make( AWidth, AHeight : integer );
begin

  FWidth  := AWidth;
  FHeight := AHeight;

  // Get some scaling information.
  FxScale  := FImage.Width  / 100;
  FyScale  := FImage.Height / 100;

  FmxSize  := 98 * FxScale;
  FmySize  := 97 * FyScale;

  FpxScale := 5 * FmxSize / ( 2 + 5 * FWidth );
  FpyScale := FmySize / ( FHeight + 1 );

  Fpx      := FpxScale / 5;
  Fpy      := FpyScale / 3;

  FMidpx   := 3.5;
  FMidpy   := 3.0;

  // Redimension the arrays to the right size.
  SetLength( FMaze, FHeight, FWidth );
  SetLength( FMazeWalls, FHeight, FWidth );
  SetLength( Fmap, FHeight * FWidth );
  // Setup the walls.
  SetupWalls;

  // Make the maze.
  GenerateMaze;

  Draw;

end;

procedure TMaze.GenerateMaze;
var
  y,  x   : Integer;
  lx, ly  : Integer;
  stack   : Integer;
  fstack  : Integer;
  mazes   : array of Integer;
  choices : integer;
  chs     : array[0..5] of Integer;
  d       : integer;
  backup  : boolean;
  xd, yd  : Integer;
begin
  // Start here.
  x      := 0;
  y      := 0;
  stack  := 0;
  fstack := 0;
  // Set stack size.
  SetLength( mazes, FHeight * FWidth );

  // Mark this cell as being used.
  FMaze[y, x] := True;

  // Remove walls from the first and last cell.  ( Entry and exit. )
  lx := FWidth  - 1;
  ly := FHeight - 1;
  if ((ly + lx) mod 2) <> 0 then begin
    ly := ly - 1;
  end;
  FMazeWalls[y, x] := FMazeWalls[0, 0] - WALL[4];
  FMazeWalls[ly, lx] := FMazeWalls[ly, lx] - WALL[1];

  // Get the initial directions.
  choices := GetChoicesForCell( x, y, chs );

  // Put this cell on the stack.
  stack := stack + 1;
  mazes[ stack ] := y * FWidth + x;

  backup := False;

  while stack > fstack - 1 do begin
    if not backup then begin
      // Get the random direction.
      d  := Random( choices + 1 );
      xd := XDIR[chs[d]];
      yd := YDIR[chs[d]];
      x  := x + xd;
      y  := y + yd;
      case chs[d] of
        // Remove wall 1 from last cell.
        0: begin
          FMazeWalls[y - yd, x - xd] := FMazeWalls[y - yd, x - xd] - WALL[0];
        end;
        // Remove wall 2 from last cell.
        1: begin
          FMazeWalls[y - yd, x - xd] := FMazeWalls[y - yd, x - xd] - WALL[1];
        end;
        // Remove wall 3 from last cell.
        2: begin
          FMazeWalls[y - yd, x - xd] := FMazeWalls[y - yd, x - xd] - WALL[2];
        end;
        // Remove wall 1 from current cell.
        3: begin
          FMazeWalls[y, x] := FMazeWalls[y, x] - WALL[0];
        end;
        // Remove wall 2 from current cell.
        4: begin
          FMazeWalls[y, x] := FMazeWalls[y, x] - WALL[1];
        end;
        // Remove wall 3 from current cell.
        5: begin
          FMazeWalls[y, x] := FMazeWalls[y, x] - WALL[2];
        end;
      end;
      // Set this cell as used.
      FMaze[y, x] := True;
    end; // If not backup.

    // Get the availiable directions.
    choices := GetChoicesForCell( x, y, chs );
    // If there is more than one choice then put this cell on the stack.
    if choices > 0 then begin
      stack := stack + 1;
      mazes[ stack ] := y * FWidth + x;
    end;
    backup := False;
    // If there were no choices, backup...
    if choices = -1 then begin
      backup := True;
      // Get the value on the stack.
      y := mazes[fstack] div FWidth;
      x := mazes[fstack] mod FWidth;
      fstack := fstack + 1;
    end;
  end;
end;

function TMaze.GetChoicesForCell( x, y : integer;
                                  var Achs : array of integer) : Integer;
var
  d : integer;
begin
  result := -1;
  // Loop for directions.
  for d := 0 to 5 do begin
    // Check if still on the grid.
    if ( x + XDIR[d] > -1 ) and ( x + XDIR[d] < FWidth )  and
       ( y + YDIR[d] > -1 ) and ( y + YDIR[d] < FHeight ) then begin
      // Maze cell to move to not already occupied?
      if FMaze[y + YDIR[d], x + XDIR[d]] = False then begin
        result := result + 1;
        Achs[result] := d;
      end;
    end;
  end;
end;

procedure TMaze.Draw;
var
  y, x, w : Integer;
begin

  with FImage.Buffer do begin
    // Clear the image first.
    Clear(BACK_COLOR32);
    PenColor := WALL_COLOR32;
  end;
  with FImage.Canvas do begin
    // Clear the image first.
    Pen.Width := 1;
    Brush.Color := BACK_COLOR;
    FillRect( ClipRect );
    Pen.Color   := WALL_COLOR;
  end;

  // Loop for columns and rows...
  for y := 0 to FHeight - 1 do begin
    for x := 0 to FWidth - 1 do begin
      // Only the cells coordinates when added are even number are used.
      if ((y + x) mod 2) = 0 then begin
        // Loop for walls.
        for w := 0 to 5 do begin
          // See if the given wall should be drawn.
          if ( FMazeWalls[y, x] and WALL[w] ) = WALL[w]  then begin
            DrawWall( x, y, w );
          end; // If wall on
        end; // Wall loop
      end; // If even cell
    end; // X loop
  end; // Y loop
end;

procedure TMaze.DrawWall( x, y, w : integer);
var
  x1, x2, y1, y2   : Integer;
begin
  x1 := Trunc( FxScale + x * FpxScale + Fpx * XHEX[w] );
  y1 := Trunc( FyScale + y * FpyScale + Fpy * YHEX[w] );
  x2 := Trunc( FxScale + x * FpxScale + Fpx * XHEX[w + 1] );
  y2 := Trunc( FyScale + y * FpyScale + Fpy * YHEX[w + 1] );

  with FImage.Canvas do begin
    // Draw wall.
    MoveTo( x1, y1 );
    LineTo( x2, y2 );
    Pixels[x2, y2] := Pen.Color;
  end; // With image.
  with FImage.Buffer do begin
    // Draw wall.
    MoveTo( x1, y1 );
    LineToAS( x2, y2 );
    Pixels[x2, y2] := PenColor;
  end; // With image.
end;

procedure TMaze.SetupWalls;
var
  y, x, w : integer;
begin
  // Loop for the rows and columns.
  for y := 0 to FHeight - 1 do begin
    for x := 0 to FWidth - 1 do begin
      // Only the cells whos coordinates when added are an even number are used.
      if ((y + x) mod 2) = 0 then begin
        FMaze[y, x]      := False;
        FMazeWalls[y, x] := ALL_WALLS;
        // Subtract the walls that aren't needed.
        for w := 3 to 5 do begin
          if ( x + XDIR[w] > -1 )     and ( y + YDIR[w] > -1 )      and
             ( x + XDIR[w] < FWidth ) and ( y + YDIR[w] < FHeight ) then begin
            FMazeWalls[y, x] := FMazeWalls[y, x] - WALL[w];
          end;
        end;
      end;
    end;
  end;
end;

procedure TMaze.Print( solution : boolean );
var
  y, x, w          : Integer;
  prt              : TPrinter;
  pxScale, pyScale : Double;
  px, py           : Double;
  xScale, yScale   : Double;
  mxSize, mySize   : Double;
  d                : Integer;

  procedure PrintWall( x, y, w : integer);
  var
    x1, x2, y1, y2   : Integer;
  begin
    x1 := Trunc( xScale + x * pxScale + px * XHEX[w] );
    y1 := Trunc( yScale + y * pyScale + py * YHEX[w] );
    x2 := Trunc( xScale + x * pxScale + px * XHEX[w + 1] );
    y2 := Trunc( yScale + y * pyScale + py * YHEX[w + 1] );

    with prt.Canvas do begin
      // Draw wall.
      MoveTo( x1, y1 );
      LineTo( x2, y2 );
      Pixels[x2, y2] := clRed; //Pen.Color;
    end; // With image.
  end;

  procedure PrintSolveLine( x, y, d : integer);
  var
    lx, ly           : Integer;
    x1, x2, y1, y2   : Integer;
  begin
    lx := x - XDIR[d];
    ly := y - YDIR[d];

    x1 := Trunc( xScale + lx * pxScale + px * FMidpx );
    y1 := Trunc( yScale + ly * pyScale + py * FMidpy );
    x2 := Trunc( xScale + x * pxScale + px * FMidpx );
    y2 := Trunc( yScale + y * pyScale + py * FMidpy );

    with prt.Canvas do begin
      // Draw wall.
      MoveTo( x1, y1 );
      LineTo( x2, y2 );
      Pixels[x2, y2] := clRed; //Pen.Color;
    end; // With image.
  end;

begin

  prt := Printer;

  // Get some scaling information.
  xScale  := prt.PageWidth  / 100;
  yScale  := prt.PageHeight / 100;

  mxSize  := 98 * xScale;
  mySize  := 97 * yScale;

  pxScale := 5 * mxSize / ( 2 + 5 * FWidth );
  pyScale := mySize / ( FHeight + 1 );

  px      := pxScale / 5;
  py      := pyScale / 3;

  prt.BeginDoc;

  with prt.Canvas do begin
    // Clear the image first.
    Brush.Color := clWhite;
    FillRect( ClipRect );
    Pen.Color   := clBlack;
    // Loop for columns and rows...
    for y := 0 to FHeight - 1 do begin
      for x := 0 to FWidth - 1 do begin
        // Only the cells coordinates when added are even number are used.
        if ((y + x) mod 2) = 0 then begin
          // Loop for walls.
          for w := 0 to 5 do begin
            // See if the given wall should be drawn.
            if ( FMazeWalls[y, x] and WALL[w] ) = WALL[w]  then begin
              PrintWall( x, y, w );
            end; // If wall on
          end; // Wall loop
        end; // If even cell
      end; // X loop
    end; // Y loop

    // Print the solution?
    if solution then begin
      Pen.Color   := SOLVE_COLOR;
      x := 0;
      y := 0;
      for d := 1 to Fmappos do begin
        PrintSolveLine( x, y, Fmap[d-1] );
        x := x + XDIR[Fmap[d]];
        y := y + YDIR[Fmap[d]];
      end;
      PrintSolveLine( x, y, Fmap[Fmappos] );
    end;
  end; // With image.

  prt.EndDoc;
end;

procedure TMaze.Solve( ShowProg : boolean );
var
  choices : integer;
  chs     : array[0..5] of integer;
  x, y    : Integer;
  lx, ly  : Integer;
  d       : Integer;
begin

  // Reset maze used cells for the solver to use.
  // Loop for the rows and columns.
  for y := 0 to FHeight - 1 do begin
    for x := 0 to FWidth - 1 do begin
      // Only the cells whos coordinates when added are an even number are used.
      if ((y + x) mod 2) = 0 then begin
        FMaze[y, x] := False;
      end;
    end;
  end;

  // Start here.
  x := 0;
  y := 0;
  // End here.
  lx := FWidth  - 1;
  ly := FHeight - 1;
  if ((ly + lx) mod 2) <> 0 then begin
    ly := ly - 1;
  end;

  Fmappos      := 0;
  Fmap[Fmappos] := 1;
  FMaze[y, x] := True;

  FImage.Canvas.Pen.Width := 1;
  if ShowProg then begin
    FImage.Canvas.Pen.Color := SOLVE_COLOR;
    FImage.Buffer.PenColor := SOLVE_COLOR32;
    DrawSolveLine( x, y, Fmap[Fmappos] );
  end;

  // Loop until x and y equal the goal.
  while ( x <> lx ) or ( y <> ly ) do begin
    // Get the choices.
    choices := GetChoicesForSolve( x, y, chs );
    // If there is a choice...
    if choices > -1 then begin
      d := Random( choices + 1 );
      Fmappos := Fmappos + 1;
      Fmap[Fmappos] := chs[d];
      x := x + XDIR[Fmap[Fmappos]];
      y := y + YDIR[Fmap[Fmappos]];
      FMaze[y, x] := True;

      if ShowProg then begin
        FImage.Canvas.Pen.Color := SOLVE_COLOR;
        FImage.Buffer.PenColor := SOLVE_COLOR32;
        DrawSolveLine( x, y, Fmap[Fmappos] );
      end;
      continue;
    end;
    // No choices, so this is a dead end.  (backup)
    x := x - XDIR[Fmap[Fmappos]];
    y := y - YDIR[Fmap[Fmappos]];

    if ShowProg then begin
      FImage.Canvas.Pen.Color := clRed;
      FImage.Buffer.PenColor := clRed32;
      DrawSolveLine( x, y, REVD[Fmap[Fmappos]] );
    end;

    Fmappos := Fmappos - 1;
  end;
  Fmappos := Fmappos + 1;
  Fmap[Fmappos] := 1;
  x := x + XDIR[Fmap[Fmappos]];
  y := y + YDIR[Fmap[Fmappos]];

  if ShowProg then begin
    FImage.Canvas.Pen.Color := SOLVE_COLOR;
    FImage.Buffer.PenColor := SOLVE_COLOR32;
    DrawSolveLine( x, y, Fmap[Fmappos] );
  end;

  if not ShowProg then begin
    x := 0;
    y := 0;
    FImage.Canvas.Pen.Color := SOLVE_COLOR;
    for d := 1 to Fmappos do begin
      DrawSolveLine( x, y, Fmap[d-1] );
      x := x + XDIR[Fmap[d]];
      y := y + YDIR[Fmap[d]];
    end;
    DrawSolveLine( x, y, Fmap[Fmappos] );
  end;

end;

function TMaze.GetChoicesForSolve( x, y : integer;
                                  var Achs : array of integer) : Integer;
var
  d    : integer;
  wl   : integer;
begin
  wl := 0;
  result := -1;
  // Loop for directions.
  for d := 0 to 5 do begin
    // Check if still on the grid.
    if ( x + XDIR[d] > -1 ) and ( x + XDIR[d] < FWidth )  and
       ( y + YDIR[d] > -1 ) and ( y + YDIR[d] < FHeight ) then begin
      // Maze cell to move to not already been to?
      if FMaze[y + YDIR[d], x + XDIR[d]] = False then begin
        // Is there a wall in the way?
        case d of
          0: begin
            wl := ( FMazeWalls[y, x] and WALL[0] );
          end;
          1: begin
            wl := ( FMazeWalls[y, x] and WALL[1] );
          end;
          2: begin
            wl := ( FMazeWalls[y, x] and WALL[2] );
          end;
          3: begin
            wl := ( FMazeWalls[y + YDIR[d], x + XDIR[d]] and WALL[0] );
          end;
          4: begin
            wl := ( FMazeWalls[y + YDIR[d], x + XDIR[d]] and WALL[1] );
          end;
          5: begin
            wl := ( FMazeWalls[y + YDIR[d], x + XDIR[d]] and WALL[2] );
          end;
        end;
        if wl = 0 then begin
          result := result + 1;
          Achs[result] := d;
        end;
      end;
    end;
  end;
end;

procedure TMaze.DrawSolveLine( x, y, d : integer);
var
  lx, ly           : Integer;
  x1, x2, y1, y2   : Integer;
begin
  lx := x - XDIR[d];
  ly := y - YDIR[d];

  x1 := Trunc( FxScale + lx * FpxScale + Fpx * FMidpx );
  y1 := Trunc( FyScale + ly * FpyScale + Fpy * FMidpy );
  x2 := Trunc( FxScale + x * FpxScale + Fpx * FMidpx );
  y2 := Trunc( FyScale + y * FpyScale + Fpy * FMidpy );

  with FImage.Canvas do begin
    // Draw wall.
    MoveTo( x1, y1 );
    LineTo( x2, y2 );
    Pixels[x2, y2] := Pen.Color;

  end; // With image.

  with FImage.Buffer do begin
    // Draw wall.
    MoveTo( x1, y1 );
    LineToAS( x2, y2 );
    Pixels[x2, y2] := PenColor;

  end; // With image.

end;

end.
