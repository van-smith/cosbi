unit ufrmCPUInfo;
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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, CosbiCpuid, COSBI_Common;

type
  TfrmCPUInfo = class(TForm)
    MemoCPUInfo: TMemo;
    BitBtnOK: TBitBtn;
    BitBtnCopyToClipboard: TBitBtn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure BitBtnCopyToClipboardClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCPUInfo: TfrmCPUInfo;

implementation

{$R *.dfm}

procedure TfrmCPUInfo.FormCreate(Sender: TObject);
begin
  with MemoCPUInfo do begin
    Lines.Clear;
    Lines.Add( CpuidDumpAll );
  end; // with
end;

procedure TfrmCPUInfo.BitBtnOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmCPUInfo.BitBtnCopyToClipboardClick(Sender: TObject);
begin
  MemoCPUInfo.SelectAll;
  MemoCPUInfo.CopyToClipboard;
end;

procedure TfrmCPUInfo.FormActivate(Sender: TObject);
begin
  UnfadeFast( self );
end;

procedure TfrmCPUInfo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FadeFast100( self );
end;

end.
