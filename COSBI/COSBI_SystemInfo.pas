unit COSBI_SystemInfo;
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
  Dialogs, StdCtrls, Registry, cxCpu40, COSBI_Common, CosbiCpuid;

type

  TStandardCPUIDFlags = record
    FPU          : Boolean;
    VME          : Boolean;
    DE           : Boolean;
    PSE          : Boolean;
    TSC          : Boolean;
    MSR          : Boolean;
    PAE          : Boolean;
    MCE          : Boolean;
    CX8          : Boolean;
    APIC         : Boolean;
    SEP          : Boolean;
    MTRR         : Boolean;
    PGE          : Boolean;
    MCA          : Boolean;
    CMOV         : Boolean;
    PAT          : Boolean;
    PSE36        : Boolean;
    PSN          : Boolean;
    CLFSH        : Boolean;
    DTES         : Boolean;
    ACPI         : Boolean;
    MMX          : Boolean;
    FXSR         : Boolean;
    SSE          : Boolean;
    SSE2         : Boolean;
    SS           : Boolean;
    HTT          : Boolean;
    TM           : Boolean;
    IA64         : Boolean;
    SBF          : Boolean;
  end;  // TStandardCPUIDFlags

  TExtendedCPUIDFlags = record
    FPU          : Boolean;
    VME          : Boolean;
    DE           : Boolean;
    PSE          : Boolean;
    TSC          : Boolean;
    MSR          : Boolean;
    PAE          : Boolean;
    MCE          : Boolean;
    CX8          : Boolean;
    APIC         : Boolean;
    SEP          : Boolean;
    MTRR         : Boolean;
    PGE          : Boolean;
    MCA          : Boolean;
    CMOV         : Boolean;
    PAT          : Boolean;
    NP           : Boolean;
    NX           : Boolean;
    MMXplus      : Boolean;
    MMX          : Boolean;
    FXSR         : Boolean;
    LM           : Boolean;
    AMD3dNowPlus : Boolean;
    AMD3dNow     : Boolean;
  end;  // TExtendedCPUIDFlags

  TPowerCPUIDFlags = record
    TS           : Boolean;
    FID          : Boolean;
    VID          : Boolean;
    TPP          : Boolean;
    TM           : Boolean;
    STC          : Boolean;
  end;  // TExtendedCPUIDFlags

  THardwareInfo = class( TComponent )
  private
    fPlatformName     : string;
    fCPU              : string;
    fCPUSignature     : string;
    fCPUVendor        : string;
    fCPUFamily        : integer;
    fCPUModel         : integer;
    fCPUStepping      : integer;
    fCPUWinMHz        : integer;
    fCPUClockSpeed    : extended;
    fCPUFSB           : double;
    fCPUCount         : integer;
    fVirtualCPUCount  : integer;
    fCPUL1d           : integer;
    fCPUL1dAss        : string;
    fCPUL1dLineSize   : integer;
    fCPUL1i           : integer;
    fCPUL1iAss        : string;
    fCPUL1iLineSize   : integer;
    fCPUTraceCache    : integer;
    fCPUL2            : integer;
    fCPUL2ass         : string;
    fCPUL2LineSize    : integer;
    fCPUL3            : integer;
    fStandardCPUIDFlags: TStandardCPUIDFlags;
    fExtendedCPUIDFlags: TExtendedCPUIDFlags;
    fPowerCPUIDFlags  : TPowerCPUIDFlags;
    fNorthBridge      : string;
    fNBVendor         : string;
    fSouthBridge      : string;
    fSBVendor         : string;
    fChipsetDriver    : string;
    fGraphicsAdapter  : string;
    fGraphicsMemory   : double;
    fGraphicsDriver   : string;
    fVideoResolution  : string;
    fMemoryType       : string;
    fMemorySpeed      : double;
    fMemoryAmount     : double;
    fHardDrive        : string;
    fHardDriveVendor  : string;
    fMotherboard      : string;
    fMBVendor         : string;
    fMotherboardBIOS  : string;
    fCOSBIClockSpeed  : extended;
    procedure SetWinMHz(Value : integer);
    procedure GetCPUInfo;
    procedure GetSystemInfo;
    procedure GetStandardCPUIDFlags;
    procedure GetExtendedCPUIDFlags;
    procedure GetPowerCPUIDFlags;
    function GetWinMHz : integer;
    function GetPlatformName  : string;
  public
    procedure InitializeData;
    constructor Create(AOwner: TComponent); overload;
    property PlatformName   : string read fPlatformName write fPlatformName;
    property CPU            : string read fCPU;
    property CPUSignature   : string read fCPUSignature;
    property CPUVendor      : string read fCPUVendor;
    property CPUFamily      : integer read fCPUFamily;
    property CPUModel       : integer read fCPUModel;
    property CPUStepping    : integer read fCPUStepping;
    property CPUWinMHz      : integer read fCPUWinMHz;
    property CPUClockSpeed  : extended read fCPUClockSpeed write fCPUClockSpeed;
    property CPUFSB         : double read fCPUFSB;
    property CPUCount       : integer read fCPUCount;
    property VirtualCPUCount: integer read fVirtualCPUCount;
    property CPUL1d         : integer read fCPUL1d;
    property CPUL1dLineSize : integer read fCPUL1dLineSize;
    property CPUL1dAss      : string read fCPUL1dAss;
    property CPUL1i         : integer read fCPUL1i;
    property CPUL1iLineSize : integer read fCPUL1iLineSize;
    property CPUL1iAss      : string read fCPUL1iAss;
    property CPUTraceCache  : integer read fCPUTRaceCache;
    property CPUL2          : integer read fCPUL2;
    property CPUL2Ass       : string read fCPUL2Ass;
    property CPUL2LineSize  : integer read fCPUL2LineSize;
    property CPUL3          : integer read fCPUL3;
    property StandardCPUIDFlags: TStandardCPUIDFlags read fStandardCPUIDFlags;
    property ExtendedCPUIDFlags: TExtendedCPUIDFlags read fExtendedCPUIDFlags;
    property PowerCPUIDFlags: TPowerCPUIDFlags read fPowerCPUIDFlags;
    property NorthBridge    : string read fNorthBridge;
    property NBVendor       : string read fNBVendor;
    property SouthBridge    : string read fSouthBridge;
    property SBVendor       : string read fSBVendor;
    property ChipsetDriver  : string read fChipsetDriver;
    property GraphicsAdapter: string read fGraphicsAdapter;
    property GraphicsMemory : double read fGraphicsMemory;
    property GraphicsDriver : string read fGraphicsDriver;
    property VideoResolution: string read fVideoResolution;
    property MemoryType     : string read fMemoryType;
    property MemorySpeed    : double read fMemorySpeed;
    property MemoryAmount   : double read fMemoryAmount;
    property HardDrive      : string read fHardDrive;
    property HardDriveVendor: string read fHardDriveVendor;
    property Motherboard    : string read fMotherboard;
    property MBVendor       : string read fMBVendor;
    property MotherboardBIOS: string read fMotherboardBIOS;
    property COSBIClockSpeed: extended read fCOSBIClockSpeed write fCOSBIClockSpeed;
  end; // THardwareInfo

  TOS_Info = class( TComponent )
  private
    fOSName                 : string;
    fVersion                : string;
    fBuildNumber            : string;
    fRegisteredOwner        : string;
    fRegisteredOrganization : string;
  public
    procedure InitializeData;
    constructor Create(AOwner: TComponent); overload;
    property OSName                 : string read fOSName;
    property Version                : string read fVersion;
    property BuildNumber            : string read fBuildNumber;
    property RegisteredOwner        : string read fRegisteredOwner;
    property RegisteredOrganization : string read fRegisteredOrganization;
  end; // TOS_Info

  procedure GetSystemInfo(var aMemo : TMemo; var aStopWatch : TStopWatch );

const

  // THardwareInfo constants:
  REG_KEY_PROCESSOR   = 'HARDWARE\DESCRIPTION\System\CentralProcessor\0';
  REG_VALUE_CPU_NAME  = 'ProcessorNameString';
  REG_VALUE_VENDOR_ID = 'VendorIdentifier';
  REG_VALUE_CORE_ID   = 'Identifier';
  REG_VALUE_MHZ       = '~Mhz';

  // OS Info constants:
  REG_KEY_OS          = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion';
  REG_VALUE_OS_NAME   = 'ProductName';
  REG_VALUE_VERSION   = 'CurrentVersion';
  REG_VALUE_BUILD     = 'CurrentBuildNumber';
  REG_VALUE_OWNER     = 'RegisteredOwner';
  REG_VALUE_ORGANIZATION = 'RegisteredOrganization';

implementation

// THardwareInfo begins.........................................................

  constructor THardwareInfo.Create(AOwner: TComponent);
  begin
    inherited;
//    InitializeData;
  end;//constructor THardwareInfo.Create(Owner: TComponent);

  procedure THardwareInfo.SetWinMHz(Value : integer);
  var
    Reg: TRegistry;
  begin
    Reg := TRegistry.Create;
    try
      with Reg do begin
        try
          RootKey := HKEY_LOCAL_MACHINE;
          OpenKey( REG_KEY_PROCESSOR, TRUE);
          WriteInteger(REG_VALUE_MHZ, Value);
          CloseKey;
        except
          ShowMessage( 'THardwareInfo.SetWinMHz: Failed to set ~MHz!' );
        end; // try..except
      end; // with
    finally
      Reg.Free;
    end;
  end; // procedure THardwareInfo.SetWinMHz(Value : integer);

  function THardwareInfo.GetWinMHz;
  var
    Reg: TRegistry;
  begin
    Reg := TRegistry.Create;
    try
      with Reg do begin
        RootKey := HKEY_LOCAL_MACHINE;
        OpenKeyReadOnly( REG_KEY_PROCESSOR );
        try
          result := ReadInteger( REG_VALUE_MHZ );
        except
          result := -1;
        end; //try..except
      end; // with
    finally
      Reg.Free;
    end;
  end; // function THardwareInfo.GetWinMHz;

  procedure THardwareInfo.GetCPUInfo;
  begin
    fPlatformName         := GetPlatformName;
    fCPU                  := cxCpu[0].Name.AsString;
    fCPUSignature         := cxCpu[0].Signature.Generic;
    fCPUVendor            := cxCpu[0].Vendor.Name;
    fCPUFamily            := cxCpu[0].Signature.Family.Value.AsNumber;
    fCPUModel             := cxCpu[0].Signature.Model.Value.AsNumber;
    fCPUStepping          := cxCpu[0].Signature.Stepping.Value.AsNumber;
    fCPUWinMHz            := GetWinMHz;
    fCPUClockSpeed        := fCOSBIClockSpeed;
    fCPUFSB               := -1;
    fCPUCount             := cxCpu.ProcessorCount.Available.AsNumber;
    fVirtualCPUCount      := cxCpu.ProcessorCount.Logical.AsNumber;
    fCPUL1d               := cxCpu.Processors[0].Cache.Level1.Data.Size.AsNumber;
    fCPUL1dAss            := cxCpu.Processors[0].Cache.Level1.Data.Associativity.Name;
    fCPUL1dLineSize       := cxCpu.Processors[0].Cache.Level1.Data.LineSize.AsNumber;
    fCPUL1i               := cxCpu.Processors[0].Cache.Level1.Code.Size.AsNumber;
    fCPUL1dAss            := cxCpu.Processors[0].Cache.Level1.Code.Associativity.Name;
    fCPUL1iLineSize       := cxCpu.Processors[0].Cache.Level1.Code.LineSize.AsNumber;;
    fCPUTraceCache        := cxCpu.Processors[0].Cache.Trace.Size.AsNumber;
    fCPUL2                := cxCpu.Processors[0].Cache.Level2.Size.AsNumber;
    fCPUL2ass             := cxCpu.Processors[0].Cache.Level2.Associativity.Name;
    fCPUL2LineSize        := cxCpu.Processors[0].Cache.Level2.LineSize.AsNumber;
    fCPUL3                := cxCpu.Processors[0].Cache.Level3.Size.AsNumber;
  end; // procedure THardwareInfo.GetCPUInfo

  procedure THardwareInfo.GetSystemInfo;
  begin
    fNorthBridge      := 'UNKNOWN';
    fNBVendor         := 'UNKNOWN';
    fSouthBridge      := 'UNKNOWN';
    fSBVendor         := 'UNKNOWN';
    fChipsetDriver    := 'UNKNOWN';
    fGraphicsAdapter  := 'UNKNOWN';
    fGraphicsMemory   := -1;
    fGraphicsDriver   := 'UNKNOWN';
    fVideoResolution  := 'UNKNOWN';
    fMemoryType       := 'UNKNOWN';
    fMemorySpeed      := -1;
    fMemoryAmount     := -1;
    fHardDrive        := 'UNKNOWN';
    fHardDriveVendor  := 'UNKNOWN';
    fMotherboard      := 'UNKNOWN';
    fMBVendor         := 'UNKNOWN';
    fMotherboardBIOS  := 'UNKNOWN';
  end; // procedure THardwareInfo.GetSystemInfo;

  procedure THardwareInfo.GetStandardCPUIDFlags;
    function GetFlagValueByName(asName : string): Boolean;
    var
      i : integer;
    begin
      for i := 0 to 31 do begin
        if cxCpu.Processors[0].Features.Standard.ByIndex(i).Mnemonic <> 'RES' then begin
          if (cxCpu.Processors[0].Features.Standard.ByIndex(i).Mnemonic = asName) then begin
             result := cxCpu.Processors[0].Features.Standard.ByIndex(i).Available.AsBoolean;
             exit;
          end; // if
        end; // if
      end; // for i
    end; // function
  begin
//    with cxCpu[0].Features.Standard do begin   <<< something is wrong with "ByName"
//      fStandardCPUIDFlags.FPU   := ByName('FPU').Available.AsBoolean;      <<< something is wrong with "ByName"
//      fStandardCPUIDFlags.VME   := ByName('VME').Available.AsBoolean;
//      fStandardCPUIDFlags.DE    := ByName('DE').Available.AsBoolean;
//      fStandardCPUIDFlags.PSE   := ByName('PSE').Available.AsBoolean;
//      fStandardCPUIDFlags.TSC   := ByName('TSC').Available.AsBoolean;
//      fStandardCPUIDFlags.MSR   := ByName('MSR').Available.AsBoolean;
//      fStandardCPUIDFlags.PAE   := ByName('PAE').Available.AsBoolean;
//      fStandardCPUIDFlags.MCE   := ByName('MCE').Available.AsBoolean;
//      fStandardCPUIDFlags.CX8   := ByName('CX8').Available.AsBoolean;
//      fStandardCPUIDFlags.APIC  := ByName('APIC').Available.AsBoolean;
//      fStandardCPUIDFlags.SEP   := ByName('SEP').Available.AsBoolean;
//      fStandardCPUIDFlags.MTRR  := ByName('MTRR').Available.AsBoolean;
//      fStandardCPUIDFlags.PGE   := ByName('PGE').Available.AsBoolean;
//      fStandardCPUIDFlags.MCA   := ByName('MCA').Available.AsBoolean;
//      fStandardCPUIDFlags.CMOV  := ByName('CMOV').Available.AsBoolean;
//      fStandardCPUIDFlags.PAT   := ByName('PAT').Available.AsBoolean;
//      fStandardCPUIDFlags.PSE36 := ByName('PSE36').Available.AsBoolean;
//      fStandardCPUIDFlags.PSN   := ByName('PSN').Available.AsBoolean;
//      fStandardCPUIDFlags.CLFSH := ByName('CLFSH').Available.AsBoolean;
//      fStandardCPUIDFlags.DTES  := ByName('DTES').Available.AsBoolean;
//      fStandardCPUIDFlags.ACPI  := ByName('ACPI').Available.AsBoolean;
//      fStandardCPUIDFlags.MMX   := ByName('MMX').Available.AsBoolean;
//      fStandardCPUIDFlags.FXSR  := ByName('FXSR').Available.AsBoolean;
//      fStandardCPUIDFlags.SSE   := ByName('SSE').Available.AsBoolean;
//      fStandardCPUIDFlags.SSE2  := ByName('SSE2').Available.AsBoolean;
//      fStandardCPUIDFlags.SS    := ByName('SS').Available.AsBoolean;
//      fStandardCPUIDFlags.HTT   := ByName('HTT').Available.AsBoolean;
//      fStandardCPUIDFlags.TM    := ByName('TM').Available.AsBoolean;
//      fStandardCPUIDFlags.IA64  := ByName('IA64').Available.AsBoolean;
//      fStandardCPUIDFlags.SBF   := ByName('SBF').Available.AsBoolean;
//   end;

      fStandardCPUIDFlags.FPU   := GetFlagValueByName('FPU');
      fStandardCPUIDFlags.VME   := GetFlagValueByName('VME');
      fStandardCPUIDFlags.DE    := GetFlagValueByName('DE');
      fStandardCPUIDFlags.PSE   := GetFlagValueByName('PSE');
      fStandardCPUIDFlags.TSC   := GetFlagValueByName('TSC');
      fStandardCPUIDFlags.MSR   := GetFlagValueByName('MSR');
      fStandardCPUIDFlags.PAE   := GetFlagValueByName('PAE');
      fStandardCPUIDFlags.MCE   := GetFlagValueByName('MCE');
      fStandardCPUIDFlags.CX8   := GetFlagValueByName('CX8');
      fStandardCPUIDFlags.APIC  := GetFlagValueByName('APIC');
      fStandardCPUIDFlags.SEP   := GetFlagValueByName('SEP');
      fStandardCPUIDFlags.MTRR  := GetFlagValueByName('MTRR');
      fStandardCPUIDFlags.PGE   := GetFlagValueByName('PGE');
      fStandardCPUIDFlags.MCA   := GetFlagValueByName('MCA');
      fStandardCPUIDFlags.CMOV  := GetFlagValueByName('CMOV');
      fStandardCPUIDFlags.PAT   := GetFlagValueByName('PAT');
      fStandardCPUIDFlags.PSE36 := GetFlagValueByName('PSE36');
      fStandardCPUIDFlags.PSN   := GetFlagValueByName('PSN');
      fStandardCPUIDFlags.CLFSH := GetFlagValueByName('CLFSH');
      fStandardCPUIDFlags.DTES  := GetFlagValueByName('DTES');
      fStandardCPUIDFlags.ACPI  := GetFlagValueByName('ACPI');
      fStandardCPUIDFlags.MMX   := GetFlagValueByName('MMX');
      fStandardCPUIDFlags.FXSR  := GetFlagValueByName('FXSR');
      fStandardCPUIDFlags.SSE   := GetFlagValueByName('SSE');
      fStandardCPUIDFlags.SSE2  := GetFlagValueByName('SSE2');
      fStandardCPUIDFlags.SS    := GetFlagValueByName('SS');
      fStandardCPUIDFlags.HTT   := GetFlagValueByName('HTT');
      fStandardCPUIDFlags.TM    := GetFlagValueByName('TM');
      fStandardCPUIDFlags.IA64  := GetFlagValueByName('IA64');
      fStandardCPUIDFlags.SBF   := GetFlagValueByName('SBF');

  end; //   procedure THardwareInfo.GetStandardCPUIDFlags;

  procedure THardwareInfo.GetExtendedCPUIDFlags;
    function GetFlagValueByName(asName : string): Boolean;
    var
      i : integer;
    begin
      for i := 0 to 31 do begin
        if cxCpu.Processors[0].Features.Extended.ByIndex(i).Mnemonic <> 'RES' then begin
          if (cxCpu.Processors[0].Features.Extended.ByIndex(i).Mnemonic = asName) then begin
             result := cxCpu.Processors[0].Features.Extended.ByIndex(i).Available.AsBoolean;
             exit;
          end; // if
        end; // if
      end; // for i
    end; // function
  begin
//    with cxCpu[0].Features.Extended do begin     <<<< Sometimes this block of code generates access violations
//      fExtendedCPUIDFlags.FPU   := ByName('FPU').Available.AsBoolean;   <<< something is wrong with "ByName"
//      fExtendedCPUIDFlags.VME   := ByName('VME').Available.AsBoolean;
//      fExtendedCPUIDFlags.DE    := ByName('DE').Available.AsBoolean;
//      fExtendedCPUIDFlags.PSE   := ByName('PSE').Available.AsBoolean;
//      fExtendedCPUIDFlags.TSC   := ByName('TSC').Available.AsBoolean;
//      fExtendedCPUIDFlags.MSR   := ByName('MSR').Available.AsBoolean;
//      fExtendedCPUIDFlags.PAE   := ByName('PAE').Available.AsBoolean;
//      fExtendedCPUIDFlags.MCE   := ByName('MCE').Available.AsBoolean;
//      fExtendedCPUIDFlags.CX8   := ByName('CX8').Available.AsBoolean;
//      fExtendedCPUIDFlags.APIC  := ByName('APIC').Available.AsBoolean;
//      fExtendedCPUIDFlags.SEP   := ByName('SEP').Available.AsBoolean;
//      fExtendedCPUIDFlags.MTRR  := ByName('MTRR').Available.AsBoolean;
//      fExtendedCPUIDFlags.PGE   := ByName('PGE').Available.AsBoolean;
//      fExtendedCPUIDFlags.MCA   := ByName('MCA').Available.AsBoolean;
//      fExtendedCPUIDFlags.CMOV  := ByName('CMOV').Available.AsBoolean;
//      fExtendedCPUIDFlags.PAT   := ByName('PAT').Available.AsBoolean;
//      fExtendedCPUIDFlags.NP    := ByName('NP').Available.AsBoolean;
//      fExtendedCPUIDFlags.NX    := ByName('NX').Available.AsBoolean;
//      fExtendedCPUIDFlags.MMXplus:= ByName('MMX+').Available.AsBoolean;
//      fExtendedCPUIDFlags.MMX   := ByName('MMX').Available.AsBoolean;
//      fExtendedCPUIDFlags.FXSR  := ByName('FXSR').Available.AsBoolean;
//      fExtendedCPUIDFlags.LM    := ByName('LM').Available.AsBoolean;
//      fExtendedCPUIDFlags.AMD3dNowPlus:= ByName('AMD3dNowPlus').Available.AsBoolean;
//      fExtendedCPUIDFlags.AMD3dNow:= ByName('AMD3dNow').Available.AsBoolean;
//    end;
      fExtendedCPUIDFlags.FPU   := GetFlagValueByName('FPU');
      fExtendedCPUIDFlags.VME   := GetFlagValueByName('VME');
      fExtendedCPUIDFlags.DE    := GetFlagValueByName('DE');
      fExtendedCPUIDFlags.PSE   := GetFlagValueByName('PSE');
      fExtendedCPUIDFlags.TSC   := GetFlagValueByName('TSC');
      fExtendedCPUIDFlags.MSR   := GetFlagValueByName('MSR');
      fExtendedCPUIDFlags.PAE   := GetFlagValueByName('PAE');
      fExtendedCPUIDFlags.MCE   := GetFlagValueByName('MCE');
      fExtendedCPUIDFlags.CX8   := GetFlagValueByName('CX8');
      fExtendedCPUIDFlags.APIC  := GetFlagValueByName('APIC');
      fExtendedCPUIDFlags.SEP   := GetFlagValueByName('SEP');
      fExtendedCPUIDFlags.MTRR  := GetFlagValueByName('MTRR');
      fExtendedCPUIDFlags.PGE   := GetFlagValueByName('PGE');
      fExtendedCPUIDFlags.MCA   := GetFlagValueByName('MCA');
      fExtendedCPUIDFlags.CMOV  := GetFlagValueByName('CMOV');
      fExtendedCPUIDFlags.PAT   := GetFlagValueByName('PAT');
      fExtendedCPUIDFlags.NP    := GetFlagValueByName('NP');
      fExtendedCPUIDFlags.NX    := GetFlagValueByName('NX');
      fExtendedCPUIDFlags.MMXplus:= GetFlagValueByName('MMX+');
      fExtendedCPUIDFlags.MMX   := GetFlagValueByName('MMX');
      fExtendedCPUIDFlags.FXSR  := GetFlagValueByName('FXSR');
      fExtendedCPUIDFlags.LM    := GetFlagValueByName('LM');
      fExtendedCPUIDFlags.AMD3dNowPlus:= GetFlagValueByName('AMD3dNowPlus');
      fExtendedCPUIDFlags.AMD3dNow:= GetFlagValueByName('AMD3dNow');
  end; //   procedure THardwareInfo.GetExtendedCPUIDFlags;

  procedure THardwareInfo.GetPowerCPUIDFlags;
    function GetFlagValueByName(asName : string): Boolean;
    var
      i : integer;
    begin
      for i := 0 to 31 do begin
        if cxCpu.Processors[0].Features.Power.ByIndex(i).Mnemonic <> 'RES' then begin
          if (cxCpu.Processors[0].Features.Power.ByIndex(i).Mnemonic = asName) then begin
             result := cxCpu.Processors[0].Features.Power.ByIndex(i).Available.AsBoolean;
             exit;
          end; // if
        end; // if
      end; // for i
    end; // function
  begin
    fPowerCPUIDFlags.TS    := GetFlagValueByName('TS');
    fPowerCPUIDFlags.FID   := GetFlagValueByName('FID');
    fPowerCPUIDFlags.VID   := GetFlagValueByName('VID');
    fPowerCPUIDFlags.TPP   := GetFlagValueByName('TPP');
    fPowerCPUIDFlags.TM    := GetFlagValueByName('TM');
    fPowerCPUIDFlags.STC   := GetFlagValueByName('STC');
  end; //   procedure THardwareInfo.GetPowerCPUIDFlags;

  function THardwareInfo.GetPlatformName: string;
  begin
    result := 'My ' + cxCpu[0].Name.AsString;
  end; // procedure THardwareInfo.InitializeData;

  procedure THardwareInfo.InitializeData;
  begin
    GetCPUInfo;
    GetSystemInfo;
    GetStandardCPUIDFlags;
    GetExtendedCPUIDFlags;
    GetPowerCPUIDFlags;
    cxCpu.CleanupInstance;
  end; // procedure THardwareInfo.InitializeData;

// TCPU_Info ends..............................................................

// TOS_Info begins............................................................
  constructor TOS_Info.Create(AOwner: TComponent);
  begin
    inherited;
    InitializeData;
  end;//constructor TOS_Info.Create(Owner: TComponent);

  procedure TOS_Info.InitializeData;
  var
    Reg: TRegistry;
  begin
    Reg := TRegistry.Create;
    try
      try
        with Reg do begin
          RootKey := HKEY_LOCAL_MACHINE;
          OpenKeyReadOnly( REG_KEY_OS );
          fOSName           := ReadString( REG_VALUE_OS_NAME );
          fVersion          := ReadString( REG_VALUE_VERSION );
          fBuildNumber      := ReadString( REG_VALUE_BUILD );
          fRegisteredOwner  := ReadString( REG_VALUE_OWNER );
          fRegisteredOrganization := ReadString( REG_VALUE_ORGANIZATION );
          CloseKey;
        end; // with
      except
        fOSName                 := 'UNKNOWN';
        fVersion                := 'UNKNOWN';
        fBuildNumber            := 'UNKNOWN';
        fRegisteredOwner        := 'UNKNOWN';
        fRegisteredOrganization := 'UNKNOWN';
      end; // try..except
    finally
      Reg.Free;
    end;
  end; // procedure TOS_Info.InitializeData;

// TOS_Info ends................................................................


procedure GetSystemInfo(var aMemo : TMemo; var aStopWatch : TStopWatch );
begin
  with THardwareInfo.Create(nil) do begin
    try
      aMemo.Lines.Add('CPU Name: ' + CPU);
      aMemo.Lines.Add('Vendor ID: ' + CPUVendor);
      aMemo.Lines.Add('Core ID: ' + CPUSignature);
      aMemo.Lines.Add('Windows MHz: ' + IntToStr(CPUWinMHz));
      aMemo.Lines.Add('COSBI CPU Clock speed is ' +
        FloatToStrF( aStopWatch.GetCPUClockspeed(FALSE), ffNumber, 11, 0 ) +
        ' Hz');
    finally
      free;
    end; // try...finally
  end; // with
  with TOS_Info.Create(nil) do begin
    try
      aMemo.Lines.Add('OS Name: ' + OSName);
      aMemo.Lines.Add('Version: ' + Version);
      aMemo.Lines.Add('Build: ' + BuildNumber);
      aMemo.Lines.Add('Registered Owner: ' + RegisteredOwner);
      aMemo.Lines.Add('Registered Organization: ' + RegisteredOrganization);
    finally
      free;
    end; // try...finally
  end; // with
end; // procedure GetSystemInfo

end.
