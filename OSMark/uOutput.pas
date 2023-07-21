unit uOutput;
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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, ExtCtrls, GR32_Image, StdCtrls, ComCtrls, jpeg, SHDocVw, OleCtrls,
  ZipForge, COSBI_Common;

type
  TfrmOutput = class(TForm)
    sgridTest: TStringGrid;
    PaintBoxOutput: TPaintBox32;
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxOutputDblClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmOutput: TfrmOutput;

implementation

{$R *.DFM}

procedure TfrmOutput.FormCreate(Sender: TObject);
begin
  Left := 15;
  Top := 0;
end;

procedure TfrmOutput.PaintBoxOutputDblClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmOutput.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = KEY_ESC then begin
    halt;
  end;
end;

end.
