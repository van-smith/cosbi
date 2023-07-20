unit uCOSBI_SystemInfo;
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
// Unit name: uCOSBI_SystemInfo
// Unit description: Pulls together a lot of system information and exposes it
//  through an handfull of classes and functions.
// Author: Van Smith
// Date: March 31, 2004
// OS dependent: Yes: Windows
// Resolution dependent: No, but resolution and color depth may impact scores.
// External unit dependencies: COSBI_Common, COSBI_Status, CosbiCpuid.
//   You will also need to import two type libraries for WbemScripting_TLB,
//   ActiveDs_TLB: Active DS Type Library and Microsoft WMI Scripting V1.2
//   Library
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
  Dialogs, StdCtrls, Registry, COSBI_Common, CosbiCpuid, WbemScripting_TLB,
  ActiveDs_TLB, ActiveX, COSBI_Status, uStopWatch;

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
    SSE3         : Boolean;
    SS           : Boolean;
    HTT          : Boolean;
    TM           : Boolean;
    IA64         : Boolean;
    EnhancedSpeedStep : Boolean;
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
    MP           : Boolean;
    NX           : Boolean;
    MMXplus      : Boolean;
    MMX          : Boolean;
    FXSR         : Boolean;
    AMD64LM      : Boolean;
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

  TWmiInfo = class( TComponent )
  private
    fLocator: TSWbemLocator;
    fSinkClasses: TSWbemSink;
  public
    constructor Create(AOwner: TComponent); override;
    function GetWmiValue(strKey: String; strValue: String) : String;
  end; // TWmiInfo

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
    fCPUL1dAss        : integer;
    fCPUL1dLineSize   : integer;
    fCPUL1i           : integer;
    fCPUL1iAss        : integer;
    fCPUL1iLineSize   : integer;
    fCPUTraceCache    : string;
    fCPUL2            : integer;
    fCPUL2ass         : integer;
    fCPUL2LineSize    : integer;
    fCPUL3            : string;
    fCpuSocket        : string;
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
    fStopWatch        : TStopWatch;
    fShowStatus       : Boolean;
    ffrmStatus        : TfrmStatus;
    fMsrRegister      : T64bitRegister;
    procedure GetCPUInfo;
    procedure GetSystemInfo;
    procedure GetStandardCPUIDFlags;
    procedure GetExtendedCPUIDFlags;
    procedure GetPowerCPUIDFlags;
    function GetWinMHz : integer;
    function GetPlatformName  : string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetWinMHz(Value : integer);
    procedure InitializeData;
    property PlatformName   : string read GetPlatformName write fPlatformName;
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
    property CPUL1dAss      : integer read fCPUL1dAss;
    property CPUL1i         : integer read fCPUL1i;
    property CPUL1iLineSize : integer read fCPUL1iLineSize;
    property CPUL1iAss      : integer read fCPUL1iAss;
    property CPUTraceCache  : string read fCPUTRaceCache;
    property CPUL2          : integer read fCPUL2;
    property CPUL2Ass       : integer read fCPUL2Ass;
    property CPUL2LineSize  : integer read fCPUL2LineSize;
    property CPUL3          : string read fCPUL3;
    property CpuSocket     : string read fCpuSocket;
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
    property StopWatch      : TStopWatch read fStopWatch write fStopWatch;
    property ShowStatus     : Boolean read fShowStatus write fShowStatus;
    property frmStatus      : TfrmStatus read ffrmStatus write ffrmStatus;
  end; // THardwareInfo

  TOS_Info = class( TComponent )
  private
    fOSName                 : string;
    fVersion                : string;
    fBuildNumber            : string;
    fRegisteredOwner        : string;
    fRegisteredOrganization : string;
    fServicePack            : string;
    fShowStatus             : Boolean;
    ffrmStatus              : TfrmStatus;
  public
    procedure InitializeData;
    constructor Create(AOwner: TComponent); override;
    property OSName                 : string read fOSName;
    property Version                : string read fVersion;
    property BuildNumber            : string read fBuildNumber;
    property RegisteredOwner        : string read fRegisteredOwner;
    property RegisteredOrganization : string read fRegisteredOrganization;
    property ServicePack            : string read fServicePack;
    property ShowStatus     : Boolean read fShowStatus write fShowStatus;
    property frmStatus      : TfrmStatus read ffrmStatus write ffrmStatus;
  end; // TOS_Info

  TSystemInfo = class( TComponent )
  private
    fHardwareInfo : THardwareInfo;
    fOsInfo       : TOS_Info;
    fInitialized  : Boolean;
    fShowStatus   : Boolean;
    fStopWatch    : TStopWatch;
    ffrmStatus    : TfrmStatus;
  public
    constructor Create(AOwner: TComponent); override;
    function GetSystemSummary: string;
    function GetCpuName: string;
    function GetCpuFeatures: string;
    procedure Initialize;
    property OsInfo : TOS_Info read fOsInfo;
    property HardwareInfo : THardwareInfo read fHardwareInfo;
    property ShowStatus : Boolean read fShowStatus write fShowStatus;
    property StopWatch  : TStopWatch read fStopWatch write fStopWatch;
    property Initialized : Boolean read fInitialized;
  end; // TSystemInfo

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
  REG_VALUE_SERVICE_PACK_VERSION = 'CSDVersion';

implementation

constructor TWmiInfo.Create(AOwner: TComponent);
begin
  inherited;
  fLocator := TSWbemLocator.Create( self );
  fSinkClasses := TSWbemSink.Create( self );
end;

function TWmiInfo.GetWmiValue(strKey: String; strValue: String) : String;
var
  lServices   : ISWbemServices;
  lObjectSet  : ISWbemObjectSet;
  lSObject    : ISWbemObject;
  lPropSet    : ISWbemPropertySet;
  lSProp      : ISWbemProperty;
  propEnum    : IEnumVariant;
  Enum        : IEnumVariant;
  tempObj     : OleVariant;
  Count       : Cardinal;
  Value       : Cardinal;
  sValue      : String;

begin
  Screen.Cursor := crHourGlass;
  try
    fSinkClasses.Cancel;
    lServices := fLocator.ConnectServer('', 'root\CIMV2', '', '', '', '', 0, nil);
    lObjectSet := lServices.ExecQuery('SELECT ' + strValue + ' FROM ' + strKey, 'WQL', wbemFlagReturnImmediately, nil);
    Enum :=  ( lObjectSet._NewEnum ) as IEnumVariant;
    while (Enum.Next( 1, tempObj, Value ) = S_OK) do begin
      lSObject := IUnknown( tempObj ) as SWBemObject;
      lPropSet := lSObject.Properties_;
      propEnum := ( lPropSet._NewEnum ) as IEnumVariant;
      while ( propEnum.Next(1, tempObj, Value ) = S_OK ) do begin
        lSProp := IUnknown(tempObj) as SWBemProperty;
        if lSProp.Name = strValue then begin
          sValue := '';
          if VarIsNull(lSProp.Get_Value) then begin
            sValue := '-1';
          end else begin
            case lSProp.CIMType of
            wbemCimtypeSint8, wbemCimtypeUint8, wbemCimtypeSint16, wbemCimtypeUint16,
            wbemCimtypeSint32, wbemCimtypeUint32, wbemCimtypeSint64: begin
              if VarIsArray(lSProp.Get_Value) then begin
                if VarArrayHighBound( lSProp.Get_Value, 1 ) > 0 then begin
                  for Count := 1 to VarArrayHighBound( lSProp.Get_Value, 1 ) do begin
                    sValue := sValue + ' ' + IntToStr( lSProp.Get_Value[Count] );
                  end; // for
                end; // if
              end else sValue := IntToStr( lSProp.Get_Value );
            end;
            wbemCimtypeReal32, wbemCimtypeReal64: sValue := FloatToStr( lSProp.Get_Value );
            wbemCimtypeBoolean: sValue := BooleanToTrueFalse( lSProp.Get_Value );
            wbemCimtypeString, wbemCimtypeUint64: begin
              if VarIsArray( lSProp.Get_Value ) then begin
                if VarArrayHighBound( lSProp.Get_Value, 1 ) > 0 then begin
                  for Count := 1 to VarArrayHighBound( lSProp.Get_Value, 1 ) do begin
                    sValue := sValue + ' ' + lSProp.Get_Value[Count];
                  end // for
                end // if
              end else begin
                sValue :=  lSProp.Get_Value;
              end; // if
            end; // wbemCimtypeString, wbemCimtypeUint64
            wbemCimtypeDatetime:  sValue :=  lSProp.Get_Value;
            wbemCimtypeReference: begin
              sValue := lSProp.Get_Value;
              sValue := sValue + ': '
                + lServices.Get( lSProp.get_Value, 0, nil).GetObjectText_(0 );
            end;
            wbemCimtypeChar16: sValue := '<16-bit character>';
            wbemCimtypeObject: sValue := '<CIM Object>';
            else sValue := 'Unknown type';
            end; // case
          end; // if if VarIsNull
          Result := trim( svalue );
          exit;
        end; // if sprop
      end; // while (propEnum.Next
    end; // while Enum
  finally
    fLocator.Disconnect;
    Screen.Cursor := crDefault;
  end; // try..finally
end; //
// THardwareInfo begins.........................................................

// THardwareInfo begins.........................................................
  constructor THardwareInfo.Create( AOwner: TComponent );
  begin
    inherited;
    fShowStatus := FALSE;
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
  var
    lCpuidProcessorName : TCpuidProcessorName;
    lCpuidLevel1    : TCpuidLevel1;
    lCpuidL1Cache   : TCpuidL1Cache;
    lCpuidLevel2    : TCpuidLevel2;
    lCpuidL2Cache   : TCpuidL2Cache;
  begin
    fPlatformName         := GetPlatformName;
    lCpuidProcessorName := TCpuidProcessorName.Create;
    try
      fCPU := lCpuidProcessorName.ProcessorName;
    finally
      FreeAndNil( lCpuidProcessorName );
    end; // try..finally
    fCPUVendor            := CpuidGetVendorIDString;
    lCpuidLevel1          := TCpuidLevel1.Create;
    try
      fCPUFamily        := lCpuidLevel1.Family;
      fCPUModel         := lCpuidLevel1.Model;
      fCPUStepping      := lCpuidLevel1.Stepping;
      fCPUSignature     := 'Family: ' + IntToStr( fCPUFamily )
        + ', Model: ' + IntToStr( fCPUModel )
        + ', Stepping: ' + IntToStr( fCPUStepping );
      // this following is a hack!!!!
      if (lCpuidLevel1.HyperThreading) and ( lCpuidLevel1.CpuCount >= 2 ) then begin
        // if HyperThreading is detected and the number of logical CPUs is equal
        // to or greater than two, then assume that HT is turned on
        fCPUCount         := GetNumberOfProcessors div lCpuidLevel1.CpuCount;
        fVirtualCPUCount  := GetNumberOfProcessors;
      end else begin
        fCPUCount         := GetNumberOfProcessors;
        fVirtualCPUCount  := fCPUCount;
      end; // if
    finally
      FreeAndNil( lCpuidLevel1 );
    end; // try..finally
    fCPUWinMHz            := GetWinMHz;
    if not assigned( fStopWatch ) then fStopWatch := TStopWatch( Self );
    fCPUClockSpeed        := fStopWatch.GetCPUClockspeed(FALSE);
    if DetectCpuVendor = eCpuVendorIntel then begin
      lCpuidLevel2 := TCpuidLevel2.Create;
      try
        fCPUTraceCache        := lCpuidLevel2.TraceCache;
        fCPUL3                := lCpuidLevel2.L3;
        fCPUL2                := lCpuidLevel2.L2_CacheSize;
        fCPUL2ass             := lCpuidLevel2.L2_CacheAss;
        fCPUL2LineSize        := lCpuidLevel2.L2_CacheLinesSize;
        fCPUL1d               := lCpuidLevel2.L1_DataCacheSize;
        fCPUL1dAss            := lCpuidLevel2.L1_DataCacheAss;
        fCPUL1dLineSize       := lCpuidLevel2.L1_DataCacheLinesSize;
        fCPUL1i               := lCpuidLevel2.L1_CodeCacheSize;
        fCPUL1iAss            := lCpuidLevel2.L1_CodeCacheAss;
        fCPUL1iLineSize       := lCpuidLevel2.L1_CodeCacheLinesSize;
      finally
        FreeAndNil( lCpuidLevel2 );
      end; // try..finally
    end else begin
      lCpuidL1Cache         := TCpuidL1Cache.Create;
      try
        fCPUL1d               := lCpuidL1Cache.L1_DataCacheSize;
        fCPUL1dAss            := lCpuidL1Cache.L1_DataCacheAss;
        fCPUL1dLineSize       := lCpuidL1Cache.L1_DataCacheLinesSize;
        fCPUL1i               := lCpuidL1Cache.L1_CodeCacheSize;
        fCPUL1iAss            := lCpuidL1Cache.L1_CodeCacheAss;
        fCPUL1iLineSize       := lCpuidL1Cache.L1_CodeCacheLinesSize;
      finally
        FreeAndNil( lCpuidL1Cache );
      end; // try..finally
      lCpuidL2Cache := TCpuidL2Cache.Create;
      try
        if fCPUL2 = 0 then fCPUL2 := lCpuidL2Cache.L2_CacheSize;
        fCPUL2ass             := lCpuidL2Cache.L2_CacheAss;
        fCPUL2LineSize        := lCpuidL2Cache.L2_CacheLinesSize;
      finally
        FreeAndNil( lCpuidL2Cache );
      end; // try..finally
    end; // if
  end; // procedure THardwareInfo.GetCPUInfo

  procedure THardwareInfo.GetSystemInfo;
  var
    lWmiInfo : TWmiInfo;
    function GetFormattedSize( asSize : string): string;
    var
      ldSize : double;
    begin
      ldSize := StrToFloat( asSize ) / ONE_BILLION;
      result :=  FloatToStrF( ldSize, ffNumber, 11, 1 ) + ' GB'
    end; // function
  begin
    lWmiInfo := TWmiInfo.Create( nil );
    with lWmiInfo do begin
      try
        fCPUFSB := StrToInt( GetWmiValue('Win32_Processor','ExtClock') );
        if fShowStatus then ffrmStatus.AddLine( 'Probing...' );
        fCpuSocket := GetWmiValue('Win32_Processor','SocketDesignation');
        fNorthBridge      := 'UNKNOWN';
        fNBVendor         := 'UNKNOWN';
        fSouthBridge      := 'UNKNOWN';
        fSBVendor         := 'UNKNOWN';
        fChipsetDriver    := 'UNKNOWN';
        fGraphicsAdapter  := GetWmiValue('Win32_VideoController','Caption');
        if fShowStatus then ffrmStatus.AddLine( 'Probing...' );
        fGraphicsMemory   := StrToInt( GetWmiValue('Win32_VideoController','AdapterRAM') );
        if fShowStatus then ffrmStatus.AddLine( 'Probing...' );
        fGraphicsDriver   := GetWmiValue('Win32_VideoController','DriverVersion');
        if fShowStatus then ffrmStatus.AddLine( 'Probing...' );
        fVideoResolution  := GetScreenRes;
        fMemoryType       := 'UNKNOWN';
        fMemorySpeed      := -1;
        fMemoryAmount     := Round( GetPhysicalMemoryInBytes / ONE_MB );
        fHardDrive        := GetWmiValue('Win32_DiskDrive','Model')
          + ', Size: ' + GetFormattedSize( GetWmiValue('Win32_DiskDrive','Size') );
        if fShowStatus then ffrmStatus.AddLine( 'Probing...' );
        fHardDriveVendor  := 'UNKNOWN';
        fMotherboard      := GetWmiValue('Win32_ComputerSystem','Manufacturer')
          + ', Model ' + GetWmiValue('Win32_ComputerSystem','Model');
        if fShowStatus then ffrmStatus.AddLine( 'Probing...' );
        fMBVendor         := 'UNKNOWN';
        fMotherboardBIOS  := GetWmiValue('Win32_Bios','Name')
          + ', Version: ' + GetWmiValue('Win32_Bios','Version');
      finally
        FreeAndNil( lWmiInfo );
      end; // try
    end; // with
  end; // procedure THardwareInfo.GetSystemInfo;

  procedure THardwareInfo.GetStandardCPUIDFlags;
  var
    lCpuidLevel1    : TCpuidLevel1;
  begin
    lCpuidLevel1    := TCpuidLevel1.Create;
    try
      fStandardCPUIDFlags.FPU   := lCpuidLevel1.FPU;
      fStandardCPUIDFlags.VME   := lCpuidLevel1.VME;
      fStandardCPUIDFlags.DE    := lCpuidLevel1.DE;
      fStandardCPUIDFlags.PSE   := lCpuidLevel1.PSE;
      fStandardCPUIDFlags.TSC   := lCpuidLevel1.TSC;
      fStandardCPUIDFlags.MSR   := lCpuidLevel1.MSR;
      fStandardCPUIDFlags.PAE   := lCpuidLevel1.PAE;
      fStandardCPUIDFlags.MCE   := lCpuidLevel1.MCE;
      fStandardCPUIDFlags.CX8   := lCpuidLevel1.Cx8;
      fStandardCPUIDFlags.APIC  := lCpuidLevel1.Apic;
      fStandardCPUIDFlags.SEP   := lCpuidLevel1.SEP;
      fStandardCPUIDFlags.MTRR  := lCpuidLevel1.MTRR;
      fStandardCPUIDFlags.PGE   := lCpuidLevel1.PGE;
      fStandardCPUIDFlags.MCA   := lCpuidLevel1.MCA;
      fStandardCPUIDFlags.CMOV  := lCpuidLevel1.CMOV;
      fStandardCPUIDFlags.PAT   := lCpuidLevel1.PAT;
      fStandardCPUIDFlags.PSE36 := lCpuidLevel1.PSE36;
      fStandardCPUIDFlags.PSN   := lCpuidLevel1.ProcSerialNum;
      fStandardCPUIDFlags.CLFSH := lCpuidLevel1.CLFL;
      fStandardCPUIDFlags.DTES  := lCpuidLevel1.DebugTraceEmon;
      fStandardCPUIDFlags.ACPI  := lCpuidLevel1.AcpiThermCtrlMSR;
      fStandardCPUIDFlags.MMX   := lCpuidLevel1.MMX;
      fStandardCPUIDFlags.FXSR  := lCpuidLevel1.FXSR;
      fStandardCPUIDFlags.SSE   := lCpuidLevel1.SSE;
      fStandardCPUIDFlags.SSE2  := lCpuidLevel1.SSE2;
      fStandardCPUIDFlags.SSE3  := lCpuidLevel1.SSE3;
      fStandardCPUIDFlags.SS    := lCpuidLevel1.SelfSnoop;
      fStandardCPUIDFlags.HTT   := lCpuidLevel1.HyperThreading;
      fStandardCPUIDFlags.TM    := lCpuidLevel1.Therm1;
      fStandardCPUIDFlags.IA64  := lCpuidLevel1.IA64;
      fStandardCPUIDFlags.EnhancedSpeedStep   := lCpuidLevel1.EnhancedSpeedStep;
    finally
      FreeAndNil( lCpuidLevel1 );
    end; // try
  end; //   procedure THardwareInfo.GetStandardCPUIDFlags;

  procedure THardwareInfo.GetExtendedCPUIDFlags;
  var
    lCpuidLevel8000_0001h : TCpuidLevel8000_0001h;
  begin
    lCpuidLevel8000_0001h := TCpuidLevel8000_0001h.Create;
    try
      fExtendedCPUIDFlags.FPU   := lCpuidLevel8000_0001h.FPU;
      fExtendedCPUIDFlags.VME   := lCpuidLevel8000_0001h.VME;
      fExtendedCPUIDFlags.DE    := lCpuidLevel8000_0001h.DE;
      fExtendedCPUIDFlags.PSE   := lCpuidLevel8000_0001h.PSE;
      fExtendedCPUIDFlags.TSC   := lCpuidLevel8000_0001h.TSC;
      fExtendedCPUIDFlags.MSR   := lCpuidLevel8000_0001h.MSR;
      fExtendedCPUIDFlags.PAE   := lCpuidLevel8000_0001h.PAE;
      fExtendedCPUIDFlags.MCE   := lCpuidLevel8000_0001h.MCE;
      fExtendedCPUIDFlags.CX8   := lCpuidLevel8000_0001h.Cx8;
      fExtendedCPUIDFlags.APIC  := lCpuidLevel8000_0001h.Apic;
      fExtendedCPUIDFlags.SEP   := lCpuidLevel8000_0001h.SEP;
      fExtendedCPUIDFlags.MTRR  := lCpuidLevel8000_0001h.MTRR;
      fExtendedCPUIDFlags.PGE   := lCpuidLevel8000_0001h.PGE;
      fExtendedCPUIDFlags.MCA   := lCpuidLevel8000_0001h.MCA;
      fExtendedCPUIDFlags.CMOV  := lCpuidLevel8000_0001h.CMOV;
      fExtendedCPUIDFlags.PAT   := lCpuidLevel8000_0001h.PAT;
      fExtendedCPUIDFlags.MP    := lCpuidLevel8000_0001h.MP;
      fExtendedCPUIDFlags.NX    := lCpuidLevel8000_0001h.NX;
      fExtendedCPUIDFlags.MMXplus:= lCpuidLevel8000_0001h.MMXPlus;
      fExtendedCPUIDFlags.MMX   := lCpuidLevel8000_0001h.MMX;
      fExtendedCPUIDFlags.FXSR  := lCpuidLevel8000_0001h.MMXPlus; // exclusive of MMX+
      fExtendedCPUIDFlags.AMD64LM    := lCpuidLevel8000_0001h.AMD64LM;
      fExtendedCPUIDFlags.AMD3dNowPlus:= lCpuidLevel8000_0001h._3dNowPlus;
      fExtendedCPUIDFlags.AMD3dNow:= lCpuidLevel8000_0001h._3dNow;
    finally
      FreeAndNil( lCpuidLevel8000_0001h );
    end; // try
  end; //   procedure THardwareInfo.GetExtendedCPUIDFlags;

  procedure THardwareInfo.GetPowerCPUIDFlags;
  var
    lCpuidEnhancedPowerManagement : TCpuidEnhancedPowerManagement;
  begin
    lCpuidEnhancedPowerManagement := TCpuidEnhancedPowerManagement.Create;
    try
      fPowerCPUIDFlags.TS    := lCpuidEnhancedPowerManagement.TemperatureSensor;
      fPowerCPUIDFlags.FID   := lCpuidEnhancedPowerManagement.FrequencyIDControl;
      fPowerCPUIDFlags.VID   := lCpuidEnhancedPowerManagement.VoltageIDControl;
      fPowerCPUIDFlags.TPP   := lCpuidEnhancedPowerManagement.ThermalTrip;
      fPowerCPUIDFlags.TM    := lCpuidEnhancedPowerManagement.ThermalMonitoring;
      fPowerCPUIDFlags.STC   := lCpuidEnhancedPowerManagement.SoftwareThermalControl;
    finally
      FreeAndNil( lCpuidEnhancedPowerManagement );
    end;
  end; //   procedure THardwareInfo.GetPowerCPUIDFlags;

  function THardwareInfo.GetPlatformName: string;
  begin
    result := 'My ' + trim( fCPUVendor ) + ' ' + trim( fCPU );
  end; // procedure THardwareInfo.InitializeData;

  procedure THardwareInfo.InitializeData;
  begin
    if fShowStatus then begin
      ffrmStatus.AddLine('Probing for System Information...');
    end; // if
    GetCPUInfo;
    if fShowStatus then ffrmStatus.AddLine( 'Probing...' );
    GetStandardCPUIDFlags;
    if fShowStatus then ffrmStatus.AddLine( 'Probing...' );
    GetExtendedCPUIDFlags;
    if fShowStatus then ffrmStatus.AddLine( 'Probing...' );
    GetSystemInfo;
    if fShowStatus then ffrmStatus.AddLine( 'Probing...' );
    GetPowerCPUIDFlags;
    if fShowStatus then ffrmStatus.AddLine( 'Probing...' );
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
          fServicePack      := ReadString( REG_VALUE_SERVICE_PACK_VERSION );
          CloseKey;
        end; // with
      except
        fOSName                 := 'Pre-WinXP';
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

// TSystemInfo begins...........................................................
constructor TSystemInfo.Create(AOwner: TComponent);
begin
  inherited;
  fShowStatus := FALSE;
  fInitialized := FALSE;
end;

procedure TSystemInfo.Initialize;
begin
  if fInitialized then exit;;
  if not assigned( fStopWatch ) then
  begin
    fStopWatch := TStopWatch.Create;
  end; // if
  if fShowStatus then begin
    if not assigned( ffrmStatus ) then begin
      ffrmStatus := TfrmStatus.Create( self );
    end; // if
    ffrmStatus.Show;
    ffrmStatus.Clear;
  end; // if
  try
    fHardwareInfo := THardwareInfo.Create( self );
    fHardwareInfo.ShowStatus := fShowStatus;
    fHardwareInfo.frmStatus := ffrmStatus;
    fHardwareInfo.StopWatch := fStopWatch;
    fHardwareInfo.InitializeData;
    fOsInfo := TOS_Info.Create( self );
    fOsInfo.ShowStatus := fShowStatus;
    fOsInfo.frmStatus := ffrmStatus;
    fOsInfo.InitializeData;
  finally
    fInitialized := TRUE;
    if Assigned( ffrmStatus ) then begin
      ffrmStatus.Release;
      ffrmStatus := nil;
    end; // if
  end; // try

end; //

function TSystemInfo.GetSystemSummary: string;
begin
  result := '';
  if not assigned( fHardwareInfo ) then Initialize;
  with fHardwareInfo do begin
    Result := Result + '* CPU Name: ' + CPU + ' (' + GetCpuName + ')';
    Result := Result + CRLF + '* Vendor ID: ' + CPUVendor;
    Result := Result + CRLF + '* Core ID: ' + CPUSignature;
    Result := Result + CRLF + '* Number of CPUs (real, virtual): ' +
      intToStr( fHardwareInfo.CPUCount ) + ', ' +
      intToStr( fHardwareInfo.VirtualCPUCount );
    Result := Result + CRLF + '* CPU Features: ' + GetCpuFeatures;
    Result := Result + CRLF + '* Windows MHz: ' + IntToStr(CPUWinMHz);
    Result := Result + CRLF + '* COSBI CPU Clock speed is ' +
      FloatToStrF( CPUClockSpeed, ffNumber, 11, 0 ) + ' Hz';
    Result := Result + CRLF + '* CPU External Clock: ' + IntToStr( round( CPUFSB ) ) + ' MHz';
    Result := Result + CRLF + '* Motherboard: ' + Motherboard;
    Result := Result + CRLF + '* BIOS: ' + MotherboardBIOS;
    Result := Result + CRLF + '* Graphics adapter: ' + GraphicsAdapter;
    Result := Result + CRLF + '* Graphics adapter memory: '
      + FloatToStrF( fGraphicsMemory / ONE_MEGABYTE, ffNumber, 11, 0 ) + ' MB';
    Result := Result + CRLF + '* Graphics driver: ' + GraphicsDriver;
    Result := Result + CRLF + '* Memory Amount (MB): ' + FloatToStr( MemoryAmount );
    Result := Result + CRLF + '* Hard drive model: ' + HardDrive;
    Result := Result + CRLF + '* Screen Resolution: ' + VideoResolution;
  end; // with
  with fOsInfo do begin
    Result := Result + CRLF + '* OS Name: ' + OSName;
    Result := Result + CRLF + '* Version: ' + Version;
    Result := Result + CRLF + '* Build: ' + BuildNumber;
    Result := Result + CRLF + '* Service Pack: ' + ServicePack;
    Result := Result + CRLF + '* Registered Owner: ' + RegisteredOwner;
    Result := Result + CRLF + '* Registered Organization: ' + RegisteredOrganization;
  end; // with
end; // function TSystemInfo.GetSystemSummary

function TSystemInfo.GetCpuName: string;
  function GetVendor : string;
  begin
    result := '';
    if fHardwareInfo.CPUVendor = 'AuthenticAMD' then begin
      result := 'AMD';
    end else if fHardwareInfo.CPUVendor = 'GenuineIntel' then begin
      result := 'Intel';
    end else if fHardwareInfo.CPUVendor = 'GenuineTMx86' then begin
      result := 'Transmeta';
    end else if fHardwareInfo.CPUVendor = 'CentaurHauls' then begin
      result := 'VIA';
    end else if fHardwareInfo.CPUVendor = 'Geode by NSC' then begin
      result := 'NationalSemi';
    end else begin
      result := 'Unknown Vendor'
    end; // if
  end; // function GetVendor : string
  function GetCpu : string;
  begin
    result := '';
    if Pos( 'Geode', fHardwareInfo.CPU) <> 0 then begin
      result := 'Geode';
    end else if Pos( 'Opteron', fHardwareInfo.CPU) <> 0 then begin
      result := 'Opteron';
      if fHardwareInfo.CPUModel = 5 then begin
        if fHardwareInfo.CPUStepping = 1 then begin
          result := result + ' (SH7-B3)';
        end else if fHardwareInfo.CPUStepping = 8 then begin
          result := result + ' (SH7-C0)';
        end else if fHardwareInfo.CPUStepping = 10 then begin
          result := result + ' (SH7-CG)';
        end else begin
          result := result + ' (unknown)';
        end;
      end; // if
    end else if Pos( 'Turion', fHardwareInfo.CPU) <> 0 then begin
      result := 'Turion';
      if Pos( 'Mobile', fHardwareInfo.CPU) <> 0 then begin
        result := 'Mobile ' + result;
      end;
    end else if Pos( 'Athlon(tm) 64', fHardwareInfo.CPU) <> 0 then begin
      result := 'Athlon 64';
      if Pos( 'Mobile', fHardwareInfo.CPU) <> 0 then begin
        result := 'Mobile ' + result;
      end;
      if fHardwareInfo.CPUModel in [12, 14] then begin
        result := result + ' (Newcastle DH7-CG 754)';
      end else if fHardwareInfo.CPUModel = 15 then begin
        result := result + ' (Winchester DH7-CG 939)';
      end else if fHardwareInfo.CPUModel = 11 then begin
        result := result + ' (CH7-CG 939)';
      end else if fHardwareInfo.CPUModel = 4 then begin
        if fHardwareInfo.CPUStepping = 8 then begin
          result := result + ' (SH7-C0)';
        end else if fHardwareInfo.CPUStepping = 10 then begin
          result := result + ' (SH7-CG 754)';
        end else begin
          result := result + ' (unknown)';
        end;
      end else if fHardwareInfo.CPUModel = 5 then begin
        if fHardwareInfo.CPUStepping = 8 then begin
          result := result + 'FX (SH7-C0)';
        end else if fHardwareInfo.CPUStepping = 10 then begin
          result := result + 'FX (SH7-CG 940)';
        end else begin
          result := result + ' (unknown)';
        end;
      end else if fHardwareInfo.CPUModel = 7 then begin
        result := result + '(SH7-CG 939)';
      end else begin
        result := result + ' (unknown)';
      end;
    end else if Pos( 'AMD Athlon(tm) XP', fHardwareInfo.CPU) <> 0 then begin
      result := 'Athlon XP';
      if fHardwareInfo.CPUModel = 6 then begin
        result := result + ' (Palamino)';
      end else if fHardwareInfo.CPUModel = 8 then begin
        result := result + ' (Thoroughbred)';
      end else if fHardwareInfo.CPUModel = 10 then begin
        result := result + ' (Barton)';
      end else begin
        result := result + ' (unknown)';
      end;
    end else if Pos( 'Athlon(tm) Processor', fHardwareInfo.CPU) <> 0 then begin
      result := 'Athlon';
      if fHardwareInfo.CPUModel in [0, 1] then begin
        result := result + ' (0.25 micron)';
      end else if fHardwareInfo.CPUModel = 2 then begin
        result := result + ' (0.18 micron)';
      end else if fHardwareInfo.CPUModel = 4 then begin
        result := result + ' (Thunderbird)';
      end else begin
        result := result + ' (unknown)';
      end;
    end else if Pos( 'Sempron', fHardwareInfo.CPU) <> 0 then begin
      result := 'Sempr0n';
    end else if Pos( 'Duron', fHardwareInfo.CPU) <> 0 then begin
      result := 'Duron';
      if fHardwareInfo.CPUModel = 3 then begin
        result := result + ' (1st generation)';
      end else if fHardwareInfo.CPUModel = 7 then begin
        result := result + ' (Morgan)';
      end else begin
        result := result + ' (unknown)';
      end;
    end else if Pos( 'Celeron', fHardwareInfo.CPU) <> 0 then begin
      if fHardwareInfo.CPUFamily = 6 then begin // piii / pm
        if fHardwareInfo.CPUModel = 11 then begin
          result := 'Celeron-Tualatin';
        end else if fHardwareInfo.CPUModel = 7 then begin
          result := 'Celeron-Mendocino';
        end else if fHardwareInfo.CPUModel = 8 then begin
          result := 'Celeron-Cu';
        end else if fHardwareInfo.CPUModel = 9 then begin
          result := 'Celeron-Banias';
        end else if fHardwareInfo.CPUModel = $D then begin
          result := 'Celeron-Dothan';
        end else if fHardwareInfo.CPUModel < 7 then begin
          result := 'Celeron-PII';
        end else begin
          result := 'Celeron';
        end;
      end else if fHardwareInfo.CPUFamily = 15 then begin // p4
        if fHardwareInfo.CPUModel = 3 then begin
          result := 'Celeron-Prescott';
        end else if fHardwareInfo.CPUModel = 2 then begin
          result := 'Celeron-Northwood';
        end else begin
          result := 'Celeron-P4';
        end;
      end else begin
        result := 'Celeron';
      end; // if  fHardwareInfo.CPUFamily =
    end else if Pos( 'Nehemiah', fHardwareInfo.CPU) <> 0 then begin
      result := 'C3-Nehemiah';
      case fHardwareInfo.CPUStepping of
      0: result := result + ' (C5XL0A)';
      1: result := result + ' (C5XL0B)';
      2: result := result + ' (C5XL0E)';
      3: result := result + ' (C5XL0H)';
      4: result := result + ' (C5XL0K)';
      5: result := result + ' (C5XL0M)';
      8: result := result + ' (C5P0D)';
      9: result := result + ' (C5P0C)';
      10: result := result + ' (C5P0E)';
      else
        result := result + ' (C5XL/C5P-unknown)';
      end;
    end else if Pos( 'Samuel', fHardwareInfo.CPU) <> 0 then begin
      if fHardwareInfo.CPUModel = 6 then begin
        result := 'C3-Samuel';
        case fHardwareInfo.CPUStepping of
        0: result := result + ' (C5A0M)';
        1: result := result + ' (C5A0O)';
        2: result := result + ' (C5A0R)';
        3: result := result + ' (C5A0U)';
        4: result := result + ' (C5A0K)';
        else
          result := result + ' (C5A-unknown)';
        end; // case
      end else begin
        result := 'C3-Samuel 2';
        case fHardwareInfo.CPUStepping of
        0: result := result + ' (C5B0H)';
        1: result := result + ' (C5B0Q)';
        2: result := result + ' (C5B0R)';
        3: result := result + ' (C5B0T)';
        8: result := result + ' (C5C0A)';
        9: result := result + ' (C5C0B)';
        else
          result := result + ' (C5B/C5C-unknown)';
        end; // case
      end; // if
    end else if Pos( 'Ezra', fHardwareInfo.CPU) <> 0 then begin
      result := 'C3-Ezra';
      if fHardwareInfo.CPUModel = 7 then begin
        case fHardwareInfo.CPUStepping of
        8: result := result + ' (C5C0C)';
        9: result := result + ' (C5C0K)';
        10: result := result + ' (C5C0M)';
        else
          result := result + ' (C5C-unknown)';
        end; // case
      end else begin
        case fHardwareInfo.CPUStepping of
        0: result := result + ' (C5M0A)';
        1: result := result + ' (C5M0C)';
        2: result := result + ' (C5M0E)';
        8: result := result + ' (C5N0A)';
        9: result := result + ' (C5N0B)';
        10: result := result + ' (C5N0D)';
        11: result := result + ' (C5N0E)';
        else
          result := result + ' (C5C/M/N-unknown)';
        end; // case
      end; // if
    end else if Pos( 'Esther', fHardwareInfo.CPU) <> 0 then begin
      result := 'C7-Esther';
      case fHardwareInfo.CPUStepping of
      0: result := result + ' (C5J)';
      8: result := result + ' (C5J)';
      9: result := result + ' (C5J)';
      10: result := result + ' (C5J)';
      else
        result := result + ' (C5J-unknown)';
      end;
    end else if Pos( 'Crusoe', fHardwareInfo.CPU) <> 0 then begin
      result := 'Crusoe';
    end else if Pos( 'Efficeon', fHardwareInfo.CPU) <> 0 then begin
      result := 'Efficeon';
    if fHardwareInfo.ExtendedCPUIDFlags.NX  then begin
      result := result + ' (90nm)';
    end else begin
      result := result + ' (130nm)';
    end;
    end else if Pos( 'Pentium(R) M', fHardwareInfo.CPU) <> 0 then begin
      if fHardwareInfo.CPUModel = 9 then begin
        result := 'PentiumM-Banias';
      end else if fHardwareInfo.CPUModel = $D then begin
        result := 'PentiumM-Dothan';
      end else begin
        result := 'Pentium-M';
      end;
    end else if Pos( 'Pentium(R) 4', fHardwareInfo.CPU) <> 0 then begin
      if fHardwareInfo.CPUModel = 4 then begin
        if fHardwareInfo.CPUStepping = 4 then begin
          result := 'Pentium D-Prescott';
        end else if fHardwareInfo.CPUStepping = 3 then begin
          result := 'Pentium 4-670';
        end else if fHardwareInfo.CPUStepping = 1 then begin
          result := 'Pentium 4-570';
        end else begin
          result := 'Pentium D / P4 unknown';
        end; // if
      end else if fHardwareInfo.CPUModel = 3 then begin
        result := 'P4-Prescott';
      end else if fHardwareInfo.CPUModel = 2 then begin
        result := 'P4-Northwood';
      end else if fHardwareInfo.CPUModel = 1 then begin
        result := 'P4-Willie';
      end else begin
        result := 'P4';
      end;
    end else begin
      result := fHardwareInfo.CPU;
    end; // if
  end; // GetCpu
  function GetMHz: string;
  begin
    result := intToStr( Round( fHardwareInfo.CPUClockSpeed / ONE_MILLION ) ) + 'MHz';
  end; // GetMHz
begin
  if not assigned( fHardwareInfo ) then Initialize;
  result := GetVendor + ' ' + GetCpu + ' ' + GetMHz;
end; // function TSystemInfo.GetCpuName: string;

function TSystemInfo.GetCpuFeatures: string;
begin
  result := '';
  with fHardwareInfo do begin
    if CPUL1i > 0 then begin
      result := 'L1i (kB): ' + intToStr( CPUL1i );
    end else if fCPUTraceCache <> '' then begin
      result := fCPUTraceCache;
    end;
    result := result + '; L1d (kB): ' + intToStr( CPUL1d );
    result := result + '; L2 (kB): ' + intToStr( CPUL2 );
    if StandardCPUIDFlags.FPU  then begin
      result := result + '; FPU';
    end;
    if StandardCPUIDFlags.CX8  then begin
      result := result + '; CX8';
    end;
    if StandardCPUIDFlags.APIC  then begin
      result := result + '; APIC';
    end;
    if StandardCPUIDFlags.PSN  then begin
      result := result + '; Processor serial number';
    end;
    if StandardCPUIDFlags.CMOV  then begin
      result := result + '; CMOV';
    end;
    if StandardCPUIDFlags.PAE  then begin
      result := result + '; PAE';
    end;
    if StandardCPUIDFlags.PAT  then begin
      result := result + '; PAT';
    end;
    if StandardCPUIDFlags.MMX  then begin
      result := result + '; MMX';
    end;
    if StandardCPUIDFlags.SSE  then begin
      result := result + '; SSE';
    end;
    if StandardCPUIDFlags.SSE2  then begin
      result := result + '; SSE2';
    end;
    if StandardCPUIDFlags.SSE3  then begin
      result := result + '; SSE3';
    end;
    if StandardCPUIDFlags.HTT  then begin
      result := result + '; HyperThreading';
    end;
    if StandardCPUIDFlags.EnhancedSpeedStep  then begin
      result := result + '; EnhancedSpeedStep';
    end;
    if ExtendedCPUIDFlags.NX  then begin
      result := result + '; NX';
    end;
    if ExtendedCPUIDFlags.MMXplus  then begin
      result := result + '; MMX+';
    end;
    if ExtendedCPUIDFlags.AMD3dNow  then begin
      result := result + '; AMD3dNow!';
    end;
    if ExtendedCPUIDFlags.AMD3dNowPlus  then begin
      result := result + '; AMD3dNow!+';
    end;
    if ExtendedCPUIDFlags.AMD64LM  then begin
      result := result + '; AMD64';
    end;
    if CpuidIsCentaurRngSupported  then begin
      result := result + '; CentaurRandomNbrGenerator supported and enabled';
    end;
    if CpuidIsCentaurAesSupported  then begin
      result := result + '; CentaurAdvancedCryptographyEngine supported and enabled';
    end;
    if CpuidIsCentaurAes2Supported  then begin
      result := result + '; CentaurAdvancedCryptographyEngine2 supported and enabled';
    end;
    if CpuidIsCentaurMontMulSupported then begin
      result := result + '; CentaurMontgomeryMultiplier supported and enabled';
    end;
    if CpuidIsCentaurHashEngineSupported then begin
      result := result + '; CentaurHashEngine supported and enabled';
    end;
  end; // with
end; // function TSystemInfo.GetCpuFeatures: string;

// TSystemInfo ends.............................................................


end.
