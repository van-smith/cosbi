program COSBIOpenSourceMark;
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

{%File 'ModelSupport\ufrmSleeping\ufrmSleeping.txvpck'}
{%File 'ModelSupport\uOSMarkResults\uOSMarkResults.txvpck'}
{%File 'ModelSupport\CosbiLogFile\CosbiLogFile.txvpck'}
{%File 'ModelSupport\adCpuUsage\adCpuUsage.txvpck'}
{%File 'ModelSupport\uCOSBI_TTest\uCOSBI_TTest.txvpck'}
{%File 'ModelSupport\ufrmExportResults\ufrmExportResults.txvpck'}
{%File 'ModelSupport\frmBandwidthBurn\frmBandwidthBurn.txvpck'}
{%File 'ModelSupport\ActiveDs_TLB\ActiveDs_TLB.txvpck'}
{%File 'ModelSupport\Dhrystone\Dhrystone.txvpck'}
{%File 'ModelSupport\uCPUSpeed\uCPUSpeed.txvpck'}
{%File 'ModelSupport\ufrmFilter\ufrmFilter.txvpck'}
{%File 'ModelSupport\uOSMarkSelectTests\uOSMarkSelectTests.txvpck'}
{%File 'ModelSupport\Whetstone\Whetstone.txvpck'}
{%File 'ModelSupport\WbemScripting_TLB\WbemScripting_TLB.txvpck'}
{%File 'ModelSupport\uAbout\uAbout.txvpck'}
{%File 'ModelSupport\uFileCopy\uFileCopy.txvpck'}
{%File 'ModelSupport\frmMemLatencyPlus\frmMemLatencyPlus.txvpck'}
{%File 'ModelSupport\COSBI_Common\COSBI_Common.txvpck'}
{%File 'ModelSupport\CosbiCpuid\CosbiCpuid.txvpck'}
{%File 'ModelSupport\uTests\uTests.txvpck'}
{%File 'ModelSupport\Maze\Maze.txvpck'}
{%File 'ModelSupport\uMobileTest\uMobileTest.txvpck'}
{%File 'ModelSupport\ufrmOSMarkOptions\ufrmOSMarkOptions.txvpck'}
{%File 'ModelSupport\uOSMark\uOSMark.txvpck'}
{%File 'ModelSupport\uOSMarkSuite\uOSMarkSuite.txvpck'}
{%File 'ModelSupport\uCOSBI_TGraphicsTest\uCOSBI_TGraphicsTest.txvpck'}
{%File 'ModelSupport\cxCpu40\cxCpu40.txvpck'}
{%File 'ModelSupport\uBandwidthBurn\uBandwidthBurn.txvpck'}
{%File 'ModelSupport\ufrmWhetBurn\ufrmWhetBurn.txvpck'}
{%File 'ModelSupport\COSBI_Status\COSBI_Status.txvpck'}
{%File 'ModelSupport\uCOSBI_SystemInfo\uCOSBI_SystemInfo.txvpck'}
{%File 'ModelSupport\ufrmGraph\ufrmGraph.txvpck'}
{%File 'ModelSupport\WMIEXTENSIONLib_TLB\WMIEXTENSIONLib_TLB.txvpck'}
{%File 'ModelSupport\uThinClientTests\uThinClientTests.txvpck'}
{%File 'ModelSupport\ufrmCPUInfo\ufrmCPUInfo.txvpck'}
{%File 'ModelSupport\uOutput\uOutput.txvpck'}
{%File 'ModelSupport\default.txvpck'}
{%File 'ModelSupport\uMobileTestSelect\uMobileTestSelect.txvpck'}
{%File 'ModelSupport\uOSMarkINI\uOSMarkINI.txvpck'}
{%File 'ModelSupport\uMobileTests\uMobileTests.txvpck'}

uses
  Forms,
  uOSMark in 'uOSMark.pas' {frmOSMark},
  COSBI_Common in '..\COSBI\COSBI_Common.pas',
  uOutput in 'uOutput.pas' {frmOutput},
  Maze in '..\Maze\Maze.pas',
  uTests in 'uTests.pas',
  uCOSBI_SystemInfo in '..\COSBI\uCOSBI_SystemInfo.pas',
  uCOSBI_TTest in '..\COSBI\uCOSBI_TTest.pas',
  uCOSBI_TGraphicsTest in '..\COSBI\uCOSBI_TGraphicsTest.pas',
  COSBI_Status in '..\COSBI\COSBI_Status.pas' {frmStatus},
  Whetstone in '..\DhryWhet\Whetstone.pas',
  Dhrystone in '..\DhryWhet\Dhrystone.pas',
  uOSMarkSuite in 'uOSMarkSuite.pas',
  uOSMarkResults in '..\ResultViewer\uOSMarkResults.pas' {frmOSMResults},
  ufrmFilter in '..\ResultViewer\ufrmFilter.pas' {frmFilter},
  adCpuUsage in '..\cxcpu40_full\Source Code\adCpuUsage.pas',
  ufrmCPUInfo in 'ufrmCPUInfo.pas' {frmCPUInfo},
  ufrmExportResults in '..\ResultViewer\ufrmExportResults.pas' {frmExportResults},
  CosbiCpuid in '..\COSBI\CosbiCpuid.pas',
  uBandwidthBurn in '..\BandwidthBurn\uBandwidthBurn.pas',
  frmBandwidthBurn in '..\BandwidthBurn\frmBandwidthBurn.pas' {frmCOSBIBandwidthBurn},
  frmMemLatencyPlus in '..\MemLatencyPlus\frmMemLatencyPlus.pas' {frmMemLate},
  uFileCopy in '..\FileCopy\uFileCopy.pas' {frmCopyFile},
  ufrmWhetBurn in '..\WhetBurn\ufrmWhetBurn.pas' {frmWhetBurn},
//  uCPUSpeed in '..\CPUSpeed\uCPUSpeed.pas' {frmCPUSpeed},
  CosbiLogFile in '..\COSBI\CosbiLogFile.pas',
  ufrmGraph in '..\CpuUtilization\ufrmGraph.pas' {frmGraph},
  cxCpu40 in '..\cxcpu40_full\Source Code\cxCpu40.pas',
  uThinClientTests in 'uThinClientTests.pas',
  ActiveDs_TLB in '..\COSBI\ActiveDs_TLB.pas',
  WbemScripting_TLB in '..\COSBI\WbemScripting_TLB.pas',
  WMIEXTENSIONLib_TLB in '..\COSBI\WMIEXTENSIONLib_TLB.pas',
  uMobileTestSelect in 'uMobileTestSelect.pas' {frmMobileTest},
  uAbout in 'uAbout.pas' {frmAbout},
  ufrmOSMarkOptions in 'ufrmOSMarkOptions.pas' {frmOSMarkOptions},
  uOSMarkSelectTests in 'uOSMarkSelectTests.pas' {frmOSMarkSelectTests},
  ufrmSleeping in 'ufrmSleeping.pas' {frmSleeping},
  uOSMarkINI in 'uOSMarkINI.pas',
  uMobileTests in 'uMobileTests.pas',
  uMsr in '..\msr\uMsr.pas',
  uStopWatch in '..\COSBI\uStopWatch.pas';

{$R *.RES}
{$MAXSTACKSIZE 43554432} //33554432}  100000000
begin
  Application.Initialize;
  Application.Title := 'COSBI OpenSourceMark';
  Application.CreateForm(TfrmOSMark, frmOSMark);
  //Application.CreateForm(TfrmCpuUtilization, frmCpuUtilization);
  //Application.CreateForm(TfrmCPUSpeed, frmCPUSpeed);
  //Application.CreateForm(TfrmWhetBurn, frmWhetBurn);
  //  Application.CreateForm(TfrmCOSBIBandwidthBurn, frmCOSBIBandwidthBurn);
  //  Application.CreateForm(TfrmMemLate, frmMemLate);
  //  Application.CreateForm(TfrmCopyFile, frmCopyFile);
  Application.Run;
end.
