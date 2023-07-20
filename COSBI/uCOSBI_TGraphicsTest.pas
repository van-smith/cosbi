unit uCOSBI_TGraphicsTest;
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
// Unit name: uCOSBI_TGraphicsTest
// Unit description: TGraphicsTest is the base class for all tests with grapical
//                   elements.
// Author: Van Smith
// Date: March 24, 2003
// OS dependent: Yes: Windows
// Resolution dependent: No, but resolution and color depth will impact scores.
// External unit dependencies: COSBI_Common, uCOSBI_TestBaseClass, GR32_Blend,
//   GR32_Image, GR32,
//==============================================================================
// Modification History
// ------------ -------
// Ver  Date   Who    Description
// ==== ====== ====== ==========================================================
// 1.0 020503 Van     Created.
// 1.1 030324 Van     Updated to support Status window and created separate
//                    unit.
// 1.2 040713 Van     Autocreate/destroy output form.
//==============================================================================

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Grids, COSBI_Common, GR32_Blend, GR32_Image,
  GR32, ComCtrls, Gauges, Maze, uCOSBI_TTest, COSBI_Status, jpeg, uOutput;

type

  TGraphicsTest = class( TTest )
  private
  protected
    fPlotStep     : integer;
    fPaintBox32   : TPaintBox32;
    sx            : double;
    sy            : double;
    ox            : double;
    oy            : double;
    loop_step     : double;
  public
    constructor Create; Overload;
    destructor Destroy; Override;
    procedure AfterTest; Override;
    procedure BeforeTest; Override;
    procedure Clear; Override;
    procedure Plot(x, y: double; PlotColor: TColor); Virtual;
    procedure PlotAndBuffer(x, y: double; PlotColor: TColor;
                            BufferColor: TColor32); Virtual;
    procedure PlotScale(x_left, x_right, y_bottom, y_top : double); Virtual;
    procedure ClearOutput; Virtual;
    property PlotStep:     integer read fPlotStep write fPlotStep;
    property PaintBox32:  TPaintBox32 read fPaintBox32 write fPaintBox32;
  end; // TGraphicsTest ..............................................

implementation

// TGraphicsTest begins........................................................

  constructor TGraphicsTest.Create;
  begin
    inherited;
    fOutputForm  := TfrmOutput.Create( NIL );
    fOutputForm.Visible := TRUE;
    application.ProcessMessages;
  end; // constructor TGraphicsTest.Create;

  destructor TGraphicsTest.Destroy;
  begin
    fOutputForm.Release;
    fOutputForm := NIL;
    inherited;
  end; // destructor

  procedure TGraphicsTest.AfterTest;
  begin
    if fShowStatus then begin
      ffrmStatus.Show;
    end; // if fShowStatus
    inherited;
  end; // procedure TGraphicsTest.AfterTest;

  procedure TGraphicsTest.BeforeTest;
  const
    ERR_NO_PAINTBOX =
      'TGraphicsTest.BeforeTest: ' +
      'Cannot run test because you have not given me a Paintbox32.';
  begin
    inherited;
    if assigned( ffrmStatus ) and ffrmStatus.Visible then begin
      ffrmStatus.Visible := FALSE;
    end; // if fShowStatus
    if fPaintBox32 = nil then begin
      fPaintBox32 := fOutputForm.PaintBoxOutput;
      if fPaintBox32 = nil then raise Exception.Create( ERR_NO_PAINTBOX );
    end;
    ClearOutput;
  end; // procedure TTrigTest.BeforeTest

  procedure TGraphicsTest.Clear;
  begin
    inherited;
    // initialize field variables:
    fPlotStep  := 1;
    sx         := 1;
    sy         := 1;
    ox         := 0;
    oy         := 0;
  end; //procedure TGraphicsTest.Clear;

  procedure TGraphicsTest.ClearOutput;
  begin
    fPaintBox32.Buffer.Clear(clBlack32);
    fPaintBox32.Invalidate;
    Application.ProcessMessages;
  end; //procedure TGraphicsTest.ClearOutput;

  procedure TGraphicsTest.PlotScale(x_left, x_right, y_bottom, y_top : double);
  begin
    with fPaintBox32 do begin
      sx := Width / (x_right - x_left);
      sy := -Height / (y_bottom - y_top);
      ox := Round( sx * x_left );
      oy := Round( sy * y_top );
    end; // with

  end; //procedure TGraphicsTest.PlotScale;

  procedure TGraphicsTest.Plot(x, y: double; PlotColor: TColor);
  var
    px, py : integer;
  begin
    px := round( (x * sx) - ox );
    py := round( -(y * sy) + oy );
    fPaintBox32.Canvas.Pixels[px, py] := PlotColor;
  end; // procedure TGraphicsTest.Plot

  procedure TGraphicsTest.PlotAndBuffer(x, y: double; PlotColor: TColor;
                                        BufferColor: TColor32);
  var
    px, py : integer;
  begin
    px := round( (x * sx) - ox );
    py := round( -(y * sy) + oy );
    fPaintBox32.Canvas.Pixels[px, py] := PlotColor;
    fPaintBox32.Buffer.Pixels[px, py] := BufferColor;
  end; // procedure TGraphicsTest.Plot

// TGraphicsTest ends...........................................................

end.


