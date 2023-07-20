unit COSBI_Status;
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
// Unit name: COSBI_Common
// Unit description: Contains a library of common routines
// Author: Van Smith
// Date: March 31, 2004
// OS dependent: Yes: Windows
// Resolution dependent: No.
// External unit dependencies: COSBI_Common
//==============================================================================
// Modification History
// ------------ -------
// Ver  Date   Who    Description
// ==== ====== ====== ==========================================================
// 1.0  040331 Van     Created.
//==============================================================================

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, COSBI_Common;

type
  TfrmStatus = class(TForm)
    memoStatus: TMemo;
    procedure memoStatusKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Clear;
    procedure AddLine( asLine : string );
  end;

implementation

{$R *.dfm}

  procedure TfrmStatus.AddLine( asLine : string );
  begin
    memoStatus.Lines.Add( asLine );
    application.ProcessMessages;
  end; // procedure TfrmStatus.AddLine( asLine : string );

  procedure TfrmStatus.Clear;
  begin
    memoStatus.Clear;
    application.ProcessMessages;
  end;

procedure TfrmStatus.memoStatusKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = KEY_ESC then begin
    halt;
  end;
end;

end.
