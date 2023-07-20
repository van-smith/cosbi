unit CosbiLogFile;
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
   Windows, COSBI_Common, uCOSBI_SystemInfo, CosbiCpuid, Forms, SysUtils;

  procedure CreateLogFile( asMessage : string; asFileSpec : string );

implementation

procedure CreateLogFile( asMessage : string; asFileSpec : string );
var
  ltxtfile_out  : TextFile;
  ls_OutLine    : string;
  lSystemInfo : TSystemInfo;
const
  DEFAULT_FILENAME = 'LOG_FILE_';
  FILE_SUFFIX = '.txt';
begin
  if asFileSpec = '' then begin
    ls_OutLine := FloatToStr( Now );
    asFileSpec := DEFAULT_FILENAME
                  + Application.Title
                  + '_'
                  + ls_OutLine
                  + FILE_SUFFIX;
  end else begin
    if Pos('.', asFileSpec) = 0 then begin
      asFileSpec := asFileSpec + FILE_SUFFIX;
    end; // if
  end; // if asFileSpec = ''
  AssignFile( ltxtfile_out, asFileSpec );
  Rewrite( ltxtfile_out );
  if asMessage <> '' then begin
    WriteLn( ltxtfile_out, asMessage );
    WriteLn( ltxtfile_out, '' );
  end;
  ls_OutLine := 'Date: ' + DateToStr( Date ) + ', Time: ' + TimeToStr( Time );
  WriteLn( ltxtfile_out, ls_OutLine );
  lSystemInfo := TSystemInfo.Create( nil );
  try
    try
      WriteLn( lSystemInfo.GetSystemSummary );
    except
      // do nothing
    end;
  finally
    FreeAndNil( lSystemInfo );
  end;
  CloseFile( ltxtfile_out );
end; // procedure CreateLogFile


end.
