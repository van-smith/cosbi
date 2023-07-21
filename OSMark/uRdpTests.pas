unit uRdpTests;
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

  TRDPTest = class( TGraphicsTest )
  private
  protected
  public
    procedure AfterTest; Override;
  end; // TGraphicsTest ..............................................

implementation

end.
 