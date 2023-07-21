program SystemInfoConsole;
{$APPTYPE CONSOLE}
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
uses
  SysUtils,
  COSBI_Common in '..\COSBI\COSBI_Common.pas',
  COSBI_Status in '..\COSBI\COSBI_Status.pas' {frmStatus},
  CosbiCpuid in '..\COSBI\CosbiCpuid.pas',
  uCOSBI_SystemInfo in '..\COSBI\uCOSBI_SystemInfo.pas',
  ActiveX,
  forms,
  uMsr in '..\msr\uMsr.pas',
  uStopWatch in '..\COSBI\uStopWatch.pas',
  ActiveDs_TLB in '..\COSBI\ActiveDs_TLB.pas',
  WbemScripting_TLB in '..\COSBI\WbemScripting_TLB.pas';

var
  SystemInfo : TSystemInfo;
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  SystemInfo := TSystemInfo.Create( nil );
  try
    SystemInfo.ShowStatus := TRUE;
    SystemInfo.Initialize;
    WriteLn( SystemInfo.GetCpuName );
    WriteLn( SystemInfo.GetSystemSummary );
  finally
    freeAndNil( SystemInfo );
  end;
  CoUninitialize;
end.
