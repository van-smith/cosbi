unit CosbiCpuid;
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
// Unit name: CosbiCpuid
// Unit description: Abstracts the x86 CPUID functions through a series of
//    classes. This comprehensive implementation is base upon data published
//    at www.sandpile.org.
// Author: Van Smith
// Date: March 31, 2004
// OS dependent: No. Should compile under Linux.
// Resolution dependent: No.
// External unit dependencies: COSBI_Common, math.
//==============================================================================
// Modification History
// ------------ -------
// Ver  Date   Who    Description
// ==== ====== ====== ==========================================================
// 1.0  040331 Van     Created.
//==============================================================================

interface

uses
  COSBI_Common, SysUtils, math, dialogs;

type
  enumCpuVendor = (eCpuVendorIntel, eCpuVendorAMD, eCpuVendorVIA,
                   eCpuVendorTransmeta, eCpuVendorOther);
  enumSpeedStepVersion = (eBanias, eYonah, eUnknown);

  TCpuid = class( TObject )
  private
    fx86Reg           : Tx86_32_GPRegisters;
    fEaxInput         : cardinal;
    procedure Initialize; Virtual;
    function GetResultsAsString : string; Virtual;
    function DumpX86GPRegisters( aX86Reg : Tx86_32_GPRegisters ) : string;
  public
    constructor Create; Overload;
    property EaxInput : cardinal read fEaxInput;
    property ResultsAsString : string read GetResultsAsString;
    property x86Reg : Tx86_32_GPRegisters read fx86Reg;
  end; // TCpuid

  TCpuidLevel0 = class( TCpuid )
  private
    fVendor : string;
    fMaxLevel : cardinal;
    fMaxLevelAsHex : string;
    function GetVendorID : string;
    procedure Initialize; override;
    function GetResultsAsString : string; override;
  public
    property VendorID: string read GetVendorID;
    property MaxLevelAsHex : string read fMaxLevelAsHex;
  end; // TCpuidLevel0

  TCpuidLevel1 = class( TCpuid )
  private
    fExtendedFamily   : integer;
    fExtendedModel    : integer;
    fCpuType          : integer;
    fFamily           : integer;
    fModel            : integer;
    fStepping         : integer;
    fBrandID          : integer;
    fCLFlush          : integer;
    fCpuCount         : integer;
    fApicId           : integer;
    fAvx              : Boolean;
    fAes              : Boolean;
    fSse42            : Boolean;
    fSse41            : Boolean;
    fCx16             : Boolean;
    fCID              : Boolean;
    fTherm2           : Boolean;
    fEnhancedSpeedStep: Boolean;
    fDSCPL            : Boolean;
    fMon              : Boolean;
    fSSE3             : Boolean;
    fPBE              : Boolean;
    fIA64             : Boolean;
    fTherm1           : Boolean;
    fHyperThreading   : Boolean;
    fSelfSnoop        : Boolean;
    fSSE2             : Boolean;
    fSSE              : Boolean;
    fFXSR             : Boolean;
    fMMX              : Boolean;
    fAcpiThermCtrlMSR : Boolean;
    fDebugTraceEmon   : Boolean;
    fCLFL             : Boolean;
    fProcSerialNum    : Boolean;
    fPSE36            : Boolean;
    fPAT              : Boolean;
    fCMOV             : Boolean;
    fMCA              : Boolean;
    fPGE              : Boolean;
    fMTRR             : Boolean;
    fSEP              : Boolean;
    fApic             : Boolean;
    fCx8              : Boolean;
    fMCE              : Boolean;
    fPAE              : Boolean;
    fMSR              : Boolean;
    fTSC              : Boolean;
    fPSE              : Boolean;
    fDE               : Boolean;
    fVME              : Boolean;
    fFPU              : Boolean;
    procedure initialize; override;
    function GetResultsAsString : string; override;
  public
    property ExtendedFamily : integer read fExtendedFamily;
    property ExtendedModel : integer read fExtendedModel;
    property CpuType : integer read fCpuType;
    property Family : integer read fFamily;
    property Model : integer read fModel;
    property Stepping : integer read fStepping;
    property BrandID : integer read fBrandID;
    property CLFlush : integer read fCLFlush;
    property CpuCount : integer read fCpuCount;
    property ApicId : integer read fApicId;
    property CID : Boolean read fCID;
    property Therm2 : Boolean read fTherm2;
    property EnhancedSpeedStep: Boolean read fEnhancedSpeedStep;
    property DSCPL : Boolean read fDSCPL;
    property Mon : Boolean read fMon;
    property SSE3 : Boolean read fSSE3;
    property PBE : Boolean read fPBE;
    property IA64 : Boolean read fIA64;
    property Therm1 : Boolean read fTherm1;
    property HyperThreading : Boolean read fHyperThreading;
    property SelfSnoop : Boolean read fSelfSnoop;
    property SSE2 : Boolean read fSSE2;
    property SSE : Boolean read fSSE;
    property FXSR : Boolean read fFXSR;
    property MMX : Boolean read fMMX;
    property AcpiThermCtrlMSR : Boolean read fAcpiThermCtrlMSR;
    property DebugTraceEmon : Boolean read fDebugTraceEmon;
    property CLFL : Boolean read fCLFL;
    property ProcSerialNum : Boolean read fProcSerialNum;
    property PSE36 : Boolean read fPSE36;
    property PAT : Boolean read fPAT;
    property CMOV : Boolean read fCMOV;
    property MCA : Boolean read fMCA;
    property PGE : Boolean read fPGE;
    property MTRR : Boolean read fMTRR;
    property SEP : Boolean read fSEP;
    property Apic : Boolean read fApic;
    property Cx8 : Boolean read fCx8;
    property MCE : Boolean read fMCE;
    property PAE : Boolean read fPAE;
    property MSR : Boolean read fMSR;
    property TSC : Boolean read fTSC;
    property PSE : Boolean read fPSE;
    property DE : Boolean read fDE;
    property VME : Boolean read fVME;
    property FPU : Boolean read fFPU;
  end; // TCpuidLevel1

  TCpuidLevel2Descriptor = record
    Value : Byte;
    Description : string;
    Found : Boolean;
  end; // TCpuidLevel2Descriptor

  // increased by one for Dothan
  TCpuidLevel2DescriptorArray = array [ 1..255 ] of TCpuidLevel2Descriptor;

  TCpuidLevel2 = class( TCpuid )
  private
    fNbrOfTimesToQuery: integer;
    fDescriptors : TCpuidLevel2DescriptorArray;
    fTraceCache : string;
    fL3 : string;
    // 4/2MB L1 TLB:
    fL1_DataTlb_4_2MB_Ass : integer;
    fL1_DataTlb_4_2MB_Entries : integer;
    fL1_CodeTlb_4_2MB_Ass : integer;
    fL1_CodeTlb_4_2MB_Entries : integer;
    // 4kB L1 TLB:
    fL1_DataTlb_4kB_Ass : integer;
    fL1_DataTlb_4kB_Entries : integer;
    fL1_CodeTlb_4kB_Ass : integer;
    fL1_CodeTlb_4kB_Entries : integer;
    // L1-d
    fL1_DataCacheSize : integer;
    fL1_DataCacheAss : integer;
    fL1_DataCacheLinesPerTag : integer;
    fL1_DataCacheLinesSize : integer;
    // L1-code
    fL1_CodeCacheSize : integer;
    fL1_CodeCacheAss : integer;
    fL1_CodeCacheLinesPerTag : integer;
    fL1_CodeCacheLinesSize : integer;
    // 4/2MB L2 TLB:
    fL2_DataTlb_4_2MB_Ass : integer;
    fL2_DataTlb_4_2MB_Entries : integer;
    fL2_CodeTlb_4_2MB_Ass : integer;
    fL2_CodeTlb_4_2MB_Entries : integer;
    // 4kB L2 TLB:
    fL2_DataTlb_4kB_Ass : integer;
    fL2_DataTlb_4kB_Entries : integer;
    fL2_CodeTlb_4kB_Ass : integer;
    fL2_CodeTlb_4kB_Entries : integer;
    // L2
    fL2_CacheSize : integer;
    fL2_CacheAss : integer;
    fL2_CacheLinesPerTag : integer;
    fL2_CacheLinesSize : integer;
    // L3
    fL3_CacheSize : integer;
    fL3_CacheAss : integer;
    fL3_CacheLinesSize : integer;
    procedure Initialize; override;
    procedure InitializeDescriptors;
    procedure PopulateDescriptors;
    function GetResultsAsString : string; override;
    function IsDescriptorInRegisters(
               aDescriptor : TCpuidLevel2Descriptor ): Boolean;
  public
    property Descriptors : TCpuidLevel2DescriptorArray read fDescriptors;
    property TraceCache : string read fTraceCache;
    property L3 : string read fL3;
    // 4/2MB L1 TLB:
    property L1_DataTlb_4_2MB_Ass : integer read fL1_DataTlb_4_2MB_Ass;
    property L1_DataTlb_4_2MB_Entries : integer read fL1_DataTlb_4_2MB_Entries;
    property L1_CodeTlb_4_2MB_Ass : integer read fL1_CodeTlb_4_2MB_Ass;
    property L1_CodeTlb_4_2MB_Entries : integer read fL1_CodeTlb_4_2MB_Entries;
    // 4kB L1 TLB:
    property L1_DataTlb_4kB_Ass : integer read fL1_DataTlb_4kB_Ass;
    property L1_DataTlb_4kB_Entries : integer read fL1_DataTlb_4kB_Entries;
    property L1_CodeTlb_4kB_Ass : integer read fL1_CodeTlb_4kB_Ass;
    property L1_CodeTlb_4kB_Entries : integer read fL1_CodeTlb_4kB_Entries;
    // L1-d
    property L1_DataCacheSize : integer read fL1_DataCacheSize;
    property L1_DataCacheAss : integer read fL1_DataCacheAss;
    property L1_DataCacheLinesPerTag : integer read fL1_DataCacheLinesPerTag;
    property L1_DataCacheLinesSize : integer read fL1_DataCacheLinesSize;
    // L1-code
    property L1_CodeCacheSize : integer read fL1_CodeCacheSize;
    property L1_CodeCacheAss : integer read fL1_CodeCacheAss;
    property L1_CodeCacheLinesPerTag : integer read fL1_CodeCacheLinesPerTag;
    property L1_CodeCacheLinesSize : integer read fL1_CodeCacheLinesSize;
    // 4/2MB L2 TLB:
    property L2_DataTlb_4_2MB_Ass : integer read fL2_DataTlb_4_2MB_Ass;
    property L2_DataTlb_4_2MB_Entries : integer read fL2_DataTlb_4_2MB_Entries;
    property L2_CodeTlb_4_2MB_Ass : integer read fL2_CodeTlb_4_2MB_Ass;
    property L2_CodeTlb_4_2MB_Entries : integer read fL2_CodeTlb_4_2MB_Entries;
    // 4kB L1 TLB:
    property L2_DataTlb_4kB_Ass : integer read fL2_DataTlb_4kB_Ass;
    property L2_DataTlb_4kB_Entries : integer read fL2_DataTlb_4kB_Entries;
    property L2_CodeTlb_4kB_Ass : integer read fL2_CodeTlb_4kB_Ass;
    property L2_CodeTlb_4kB_Entries : integer read fL2_CodeTlb_4kB_Entries;
    // L2:
    property L2_CacheSize : integer read fL2_CacheSize;
    property L2_CacheAss : integer read fL2_CacheAss;
    property L2_CacheLinesPerTag : integer read fL2_CacheLinesPerTag;
    property L2_CacheLinesSize : integer read fL2_CacheLinesSize;
    // L3:
    property L3_CacheSize : integer read fL2_CacheSize;
    property L3_CacheAss : integer read fL2_CacheAss;
    property L3_CacheLinesSize : integer read fL2_CacheLinesSize;
  end; // TCpuidLevel1

  TCpuidProccessorSerialNumber = class( TCpuid )
  private
    fVendor : string;
    procedure Initialize; override;
    function GetProcessorSerialNumber : string;
    function GetResultsAsString : string; override;
  public
    property ProcessorSerialNumber: string read GetProcessorSerialNumber;
  end; // TCpuidProccessorSerialNumber

  TCpuidLevel8000_0000h = class( TCpuid )
  private
    fVendor : string;
    fMaxLevel : cardinal;
    fMaxLevelAsHex : string;
    procedure Initialize; override;
    function GetVendorID : string;
    function GetResultsAsString : string; override;
  public
    property VendorID: string read GetVendorID;
    property MaxExtendedLevelAsHex : string read fMaxLevelAsHex;
  end; // TCpuidProccessorSerialNumber

  TCpuidLevel8000_0001h = class( TCpuid )
  private
    fExtendedFamily   : integer;
    fExtendedModel    : integer;
    fFamily           : integer;
    fModel            : integer;
    fStepping         : integer;
    fBrandID          : integer;
    f3dNow            : Boolean;
    f3dNowPlus        : Boolean;
    fAMD64LM          : Boolean;
    fFXSR             : Boolean;
    fMMX              : Boolean;
    fMMXPlus          : Boolean;
    fNX               : Boolean;
    fMP               : Boolean;
    fPSE36            : Boolean;
    fPAT              : Boolean;
    fCMOV             : Boolean;
    fMCA              : Boolean;
    fPGE              : Boolean;
    fMTRR             : Boolean;
    fSEP              : Boolean;
    fApic             : Boolean;
    fCx8              : Boolean;
    fMCE              : Boolean;
    fPAE              : Boolean;
    fMSR              : Boolean;
    fTSC              : Boolean;
    fPSE              : Boolean;
    fDE               : Boolean;
    fVME              : Boolean;
    fFPU              : Boolean;
    procedure initialize; override;
    function GetResultsAsString : string; override;
  public
    property ExtendedFamily : integer read fExtendedFamily;
    property ExtendedModel : integer read fExtendedModel;
    property Family : integer read fFamily;
    property Model : integer read fModel;
    property Stepping : integer read fStepping;
    property BrandID : integer read fBrandID;
    property _3dNow : Boolean read f3dNow;
    property _3dNowPlus : Boolean read f3dNowPlus;
    property AMD64LM : Boolean read fAMD64LM;
    property FXSR : Boolean read fFXSR;
    property MMX : Boolean read fMMX;
    property MMXPlus : Boolean read fMMXPlus;
    property NX : Boolean read fNX;
    property MP : Boolean read fMP;
    property PSE36 : Boolean read fPSE36;
    property PAT : Boolean read fPAT;
    property CMOV : Boolean read fCMOV;
    property MCA : Boolean read fMCA;
    property PGE : Boolean read fPGE;
    property MTRR : Boolean read fMTRR;
    property SEP : Boolean read fSEP;
    property Apic : Boolean read fApic;
    property Cx8 : Boolean read fCx8;
    property MCE : Boolean read fMCE;
    property PAE : Boolean read fPAE;
    property MSR : Boolean read fMSR;
    property TSC : Boolean read fTSC;
    property PSE : Boolean read fPSE;
    property DE : Boolean read fDE;
    property VME : Boolean read fVME;
    property FPU : Boolean read fFPU;
  end; // TCpuidLevel8000_0001h

  TCpuidProcessorName = class( TCpuid )
  private
    fProcessorName : string;
    fx86Reg1 : Tx86_32_GPRegisters;
    fx86Reg2 : Tx86_32_GPRegisters;
    fx86Reg3 : Tx86_32_GPRegisters;
    procedure Initialize; override;
    function GetResultsAsString : string; override;
  public
    property ProcessorName: string read fProcessorName;
  end; // TCpuidProcessorName

  TCpuidL1Cache = class( TCpuid )
  private
    // 4/2MB L1 TLB:
    fL1_DataTlb_4_2MB_Ass : integer;
    fL1_DataTlb_4_2MB_Entries : integer;
    fL1_CodeTlb_4_2MB_Ass : integer;
    fL1_CodeTlb_4_2MB_Entries : integer;
    // 4kB L1 TLB:
    fL1_DataTlb_4kB_Ass : integer;
    fL1_DataTlb_4kB_Entries : integer;
    fL1_CodeTlb_4kB_Ass : integer;
    fL1_CodeTlb_4kB_Entries : integer;
    // L1-d
    fL1_DataCacheSize : integer;
    fL1_DataCacheAss : integer;
    fL1_DataCacheLinesPerTag : integer;
    fL1_DataCacheLinesSize : integer;
    // L1-code
    fL1_CodeCacheSize : integer;
    fL1_CodeCacheAss : integer;
    fL1_CodeCacheLinesPerTag : integer;
    fL1_CodeCacheLinesSize : integer;
    procedure Initialize; override;
    function GetResultsAsString : string; override;
  public
    // 4/2MB L1 TLB:
    property L1_DataTlb_4_2MB_Ass : integer read fL1_DataTlb_4_2MB_Ass;
    property L1_DataTlb_4_2MB_Entries : integer read fL1_DataTlb_4_2MB_Entries;
    property L1_CodeTlb_4_2MB_Ass : integer read fL1_CodeTlb_4_2MB_Ass;
    property L1_CodeTlb_4_2MB_Entries : integer read fL1_CodeTlb_4_2MB_Entries;
    // 4kB L1 TLB:
    property L1_DataTlb_4kB_Ass : integer read fL1_DataTlb_4kB_Ass;
    property L1_DataTlb_4kB_Entries : integer read fL1_DataTlb_4kB_Entries;
    property L1_CodeTlb_4kB_Ass : integer read fL1_CodeTlb_4kB_Ass;
    property L1_CodeTlb_4kB_Entries : integer read fL1_CodeTlb_4kB_Entries;
    // L1-d
    property L1_DataCacheSize : integer read fL1_DataCacheSize;
    property L1_DataCacheAss : integer read fL1_DataCacheAss;
    property L1_DataCacheLinesPerTag : integer read fL1_DataCacheLinesPerTag;
    property L1_DataCacheLinesSize : integer read fL1_DataCacheLinesSize;
    // L1-code
    property L1_CodeCacheSize : integer read fL1_CodeCacheSize;
    property L1_CodeCacheAss : integer read fL1_CodeCacheAss;
    property L1_CodeCacheLinesPerTag : integer read fL1_CodeCacheLinesPerTag;
    property L1_CodeCacheLinesSize : integer read fL1_CodeCacheLinesSize;
  end; // TCpuidL1Cache

  TCpuidL2Cache = class( TCpuid )
  private
    // 4/2MB L2 TLB:
    fL2_DataTlb_4_2MB_Ass : integer;
    fL2_DataTlb_4_2MB_Entries : integer;
    fL2_CodeTlb_4_2MB_Ass : integer;
    fL2_CodeTlb_4_2MB_Entries : integer;
    // 4kB L2 TLB:
    fL2_DataTlb_4kB_Ass : integer;
    fL2_DataTlb_4kB_Entries : integer;
    fL2_CodeTlb_4kB_Ass : integer;
    fL2_CodeTlb_4kB_Entries : integer;
    // L2
    fL2_CacheSize : integer;
    fL2_CacheAss : integer;
    fL2_CacheLinesPerTag : integer;
    fL2_CacheLinesSize : integer;
    procedure Initialize; override;
    function GetResultsAsString : string; override;
  public
    // 4/2MB L2 TLB:
    property L2_DataTlb_4_2MB_Ass : integer read fL2_DataTlb_4_2MB_Ass;
    property L2_DataTlb_4_2MB_Entries : integer read fL2_DataTlb_4_2MB_Entries;
    property L2_CodeTlb_4_2MB_Ass : integer read fL2_CodeTlb_4_2MB_Ass;
    property L2_CodeTlb_4_2MB_Entries : integer read fL2_CodeTlb_4_2MB_Entries;
    // 4kB L1 TLB:
    property L2_DataTlb_4kB_Ass : integer read fL2_DataTlb_4kB_Ass;
    property L2_DataTlb_4kB_Entries : integer read fL2_DataTlb_4kB_Entries;
    property L2_CodeTlb_4kB_Ass : integer read fL2_CodeTlb_4kB_Ass;
    property L2_CodeTlb_4kB_Entries : integer read fL2_CodeTlb_4kB_Entries;
    // L2:
    property L2_CacheSize : integer read fL2_CacheSize;
    property L2_CacheAss : integer read fL2_CacheAss;
    property L2_CacheLinesPerTag : integer read fL2_CacheLinesPerTag;
    property L2_CacheLinesSize : integer read fL2_CacheLinesSize;
  end; // TCpuidL2Cache

  TCpuidEnhancedPowerManagement = class( TCpuid )
  private
    fSoftwareThermalControl : Boolean;
    fThermalMonitoring : Boolean;
    fThermalTrip : Boolean;
    fVoltageIDControl : Boolean;
    fFrequencyIDControl : Boolean;
    fTemperatureSensor : Boolean;
    procedure Initialize; override;
    function GetResultsAsString : string; override;
  public
    property SoftwareThermalControl : Boolean read fSoftwareThermalControl;
    property ThermalMonitoring : Boolean read fThermalMonitoring;
    property ThermalTrip : Boolean read fThermalTrip;
    property VoltageIDControl : Boolean read fVoltageIDControl;
    property FrequencyIDControl : Boolean read fFrequencyIDControl;
    property TemperatureSensor : Boolean read fTemperatureSensor;
  end; // TCpuidEnhancedPowerManagement

  TCpuidAMD64AddressSize = class( TCpuid )
  private
    fVirtualAddress : integer;
    fPhysicalAddressBits : integer;
    procedure Initialize; override;
    function GetResultsAsString : string; override;
  public
    property VirtualAddress : integer read fVirtualAddress;
    property PhysicalAddressBits : integer read fPhysicalAddressBits;
  end; // TCpuidAMD64AddressSize

  TCpuidCentaur = class( TCpuid )
  private
    fMaxSupportedLevel : integer;
    procedure Initialize; Override;
    function GetResultsAsString : string; Override;
  public
    property MaxSupportedLevel : integer read fMaxSupportedLevel;
  end; // TCpuidCentaur

  TCpuidCentaurInfo = class( TCpuid )
  private
    fAdvancedCryptographyEngineEnabled : Boolean;
    fAdvancedCryptographyEngine : Boolean;
    fAdvancedCryptographyEngine2Enabled : Boolean;
    fAdvancedCryptographyEngine2 : Boolean;
    fFEMMS : Boolean;
    fLongHaul : Boolean;
    fRandomNumberGeneratorEnabled : Boolean;
    fRandomNumberGenerator : Boolean;
    fAlternateInstructionSetEnabled : Boolean;
    fAlternateInstructionSet : Boolean;
    fPadlockHashEngine : Boolean;
    fPadlockHashEngineEnabled : Boolean;
    fPadlockMontgomeryMultiplier : Boolean;
    fPadlockMontgomeryMultiplierEnabled : Boolean;
    procedure Initialize; Override;
    function GetResultsAsString : string; Override;
  public
    property AdvancedCryptographyEngineEnabled : Boolean read fAdvancedCryptographyEngineEnabled;
    property AdvancedCryptographyEngine : Boolean read fAdvancedCryptographyEngine;
    property AdvancedCryptographyEngine2Enabled : Boolean read fAdvancedCryptographyEngine2Enabled;
    property AdvancedCryptographyEngine2 : Boolean read fAdvancedCryptographyEngine2;
    property FEMMS : Boolean read fFEMMS;
    property LongHaul : Boolean read fLongHaul;
    property RandomNumberGeneratorEnabled : Boolean read fRandomNumberGeneratorEnabled;
    property RandomNumberGenerator : Boolean read fRandomNumberGenerator;
    property AlternateInstructionSetEnabled : Boolean read fAlternateInstructionSetEnabled;
    property AlternateInstructionSet : Boolean read fAlternateInstructionSet;
    property PadlockHashEngine : Boolean read fPadLockHashEngine;
    property PadlockHashEngineEnabled : Boolean read fPadLockHashEngineEnabled;
    property PadlockMontgomeryMultiplier : Boolean read fPadlockMontgomeryMultiplier;
    property PadlockMontgomeryMultiplierEnabled : Boolean read fPadlockMontgomeryMultiplierEnabled;
  end; // TCpuidCentaurInfo

  TCpuidCentaurPerformanceInfo = class( TCpuid )
  private
    fCurrentTemperature : double;
    fCurrentMilliVolts : integer;
    fCurrentClockMultiplier : integer;
    fLowestClockRatio : integer;
    fHighestMilliVoltageEncoding : integer;
    fHighestClockMultiplier : integer;
    fLowestVoltageEncoding : integer;
    fLowestClockMultiplier : integer;
    fFrontSideBusClock : double;
    fCurrentClockMultiplierEDX : integer;
    function GetResultsAsString : string; Override;
  public
    procedure Initialize; Override;
    property CurrentTemperature : double read fCurrentTemperature;
    property CurrentMilliVolts : integer read fCurrentMilliVolts;
    property CurrentClockMultiplier : integer read fCurrentClockMultiplier;
    property LowestClockRatio : integer read fLowestClockRatio;
    property HighestMilliVoltageEncoding : integer read fHighestMilliVoltageEncoding;
    property HighestClockMultiplier : integer read fHighestClockMultiplier;
    property LowestVoltageEncoding : integer read fLowestVoltageEncoding;
    property LowestClockMultiplier : integer read fLowestClockMultiplier;
    property FrontSideBusClock : double read fFrontSideBusClock;
    property CurrentClockMultiplierEDX : integer read fCurrentClockMultiplierEDX;
  end; // TCpuidCentaurPerformanceInfo

  TCpuidTransmeta = class( TCpuid )
  private
    fMaxSupportedLevel : integer;
    fVendorIDString : string;
    procedure Initialize; Override;
    function GetResultsAsString : string; Override;
  public
    property MaxSupportedLevel : integer read fMaxSupportedLevel;
    property VendorIDString : string read fVendorIDString;
  end; // TCpuidTransmeta

  TCpuidTransmeta1 = class( TCpuID )
  private
    fFamily           : integer;
    fModel            : integer;
    fStepping         : integer;
    fHardwareRevA     : integer;
    fHardwareRevB     : integer;
    fHardwareRevC     : integer;
    fHardwareRevD     : integer;
    fHardwareRev      : string;
    fNominalMHz       : cardinal;
    fLongRunTableInt  : Boolean;
    fUnknown          : Boolean;
    fLongRun          : Boolean;
    fRecoveryCMSActive: Boolean;
    procedure Initialize; Override;
    function GetResultsAsString : string; override;
  public
    property Family : integer read fFamily;
    property Model : integer read fModel;
    property Stepping : integer read fStepping;
    property HardwareRev : string read fHardwareRev;
    property NominalMHz : cardinal read fNominalMHz;
    property LongRunTableInt : Boolean read fLongRunTableInt;
    property Unknown : Boolean read fUnknown;
    property LongRun : Boolean read fLongRun;
    property RecoveryCMSActive : Boolean read fRecoveryCMSActive;
  end; // TCpuidTransmeta1

  TCpuidTransmetaInfoString = class( TCpuid )
  private
    fTransmetaInfoString : string;
    fx86Reg1 : Tx86_32_GPRegisters;
    fx86Reg2 : Tx86_32_GPRegisters;
    fx86Reg3 : Tx86_32_GPRegisters;
    procedure Initialize; Override;
    function GetResultsAsString : string; Override;
  public
    property TransmetaInfoString: string read fTransmetaInfoString;
  end; // TCpuidTransmetaInfoString

  TCpuidTransmetaPerformanceData = class( TCpuID )
  private
    fCpuMHz                         : integer;
    fCpuMilliVoltLevel              : integer;
    fLongRunPerformancePercentage   : integer;
    fCurrentGateDelayInFemtoSeconds : integer;
    function GetResultsAsString : string; override;
  public
    procedure Initialize; Override;
    property CpuMHz : integer read fCpuMHz;
    property CpuMilliVoltLevel : integer read fCpuMilliVoltLevel;
    property LongRunPerformancePercentage : integer read fLongRunPerformancePercentage;
    property CurrentGateDelayInFemtoSeconds : integer read fCurrentGateDelayInFemtoSeconds;
  end; // TCpuidTransmetaPerformanceData

  function CpuidIsSupported: Boolean;
  function CpuidExecute( adwLevel : cardinal ) : Tx86_32_GPRegisters;
  function CpuidGetVendorIDString: string;
  function CpuidGetProcessorNameString: string;
  function CpuidDumpAll : string;
  function DetectCpuVendor : enumCpuVendor;
  function CpuidIsCentaurRngSupported: Boolean;
  function CpuidIsCentaurAesSupported: Boolean;
  function CpuidIsCentaurAes2Supported: Boolean;
  function CpuidIsCentaurMontMulSupported: Boolean;
  function CpuidIsCentaurHashEngineSupported: Boolean;
  function CpuidIsEnhancedSpeedStepAvailable : Boolean;
  function CpuidIsSseSupported: Boolean;
  function CpuidIsSse2Supported: Boolean;
  function CpuidIsSse3Supported: Boolean;
  function CpuidGetC7Temperature: integer;
  function CpuidGetL2Size: integer;
  function CpuidIsCentaurC7 : Boolean;
  function CpuidIsCentaurCN : Boolean;
  function CpuidGetTransmetaEfficeonClockSpeed : integer;
//  function CpuidGetSpeedStepVersion: enumSpeedStepVersion;
  function CpuidIsIntelCore: Boolean;
  function CpuidIsIntelCore2: Boolean;
  function CpuidGetCpuFamily: integer;
  function CpuidGetCpuModel: integer;
  function CpuidGetCpuStepping: integer;
  function CpuidGetFamilyModelSteppingEncoding: integer;
//  function DecodeSpeedStepVcc( aiSpeedStepValue : integer ): double;

implementation

function CpuidExecute( adwLevel : cardinal ) : Tx86_32_GPRegisters;
var
  _eax, _ebx, _ecx, _edx : cardinal;
begin
  asm
      push esi              // preserve these not and ye shall crash
      push ebx
      push ebp
      mov     eax, adwLevel // get cpuid level argument ready
      cpuid             // execute cpuid function
      mov     _eax, eax // grab cpuid results
      mov     _ebx, ebx // grab cpuid results
      mov     _edx, edx // grab cpuid results
      mov     _ecx, ecx // grab cpuid results
      pop ebp
      pop ebx
      pop esi
  end; //asm
  result._eax := _eax;
  result._ebx := _ebx;
  result._ecx := _ecx;
  result._edx := _edx;
end; //

function DetectCpuVendor : enumCpuVendor;
var
  lCpuidLevel0 : TCpuidLevel0;
begin
  lCpuidLevel0 := TCpuidLevel0.Create;
  try
    result := eCpuVendorOther;
    if lCpuidLevel0.VendorID = 'GenuineTMx86' then begin
      result := eCpuVendorTransmeta;
    end else if lCpuidLevel0.VendorID = 'CentaurHauls' then begin
      result := eCpuVendorVIA;
    end else if Pos( 'AMD', lCpuidLevel0.VendorID ) > 0 then begin
      result := eCpuVendorAMD;
    end else if Pos( 'Intel', lCpuidLevel0.VendorID ) > 0 then begin
      result := eCpuVendorIntel;
    end; // if
  finally
    freeAndNil( lCpuidLevel0 );
  end; // try..finally
end; // function DetectCpuVendor

function CpuidGetL2Size: integer;
var
  lCpuidL2Cache : TCpuidL2Cache;
begin
  result := -1;
  lCpuidL2Cache := TCpuidL2Cache.Create;
  try
    lCpuidL2Cache.Initialize;
    result := lCpuidL2Cache.L2_CacheSize;
  finally
    FreeAndNil(lCpuidL2Cache);
  end;
end;

function CpuidIsCentaurC7 : boolean;
var
  lCpuidProcessorName : TCpuidProcessorName;
begin
  result := FALSE;
  //if DetectCpuVendor = eCpuVendorVIA then begin
    lCpuidProcessorName := TCpuidProcessorName.Create;
    try
      lCpuidProcessorName.Initialize;
      if ( Pos( 'Esther', lCpuidProcessorName.ProcessorName ) > 0 ) or
         ( Pos( 'C7', lCpuidProcessorName.ProcessorName ) > 0 ) then begin
        result := TRUE;
      end else begin
        result := FALSE;
      end; // if
    finally
      freeAndNil( lCpuidProcessorName );
    end; // try
  //end else begin
  //  result := FALSE;
  //end; // if
end; // function CpuidIsCentaurC7

function CpuidIsCentaurCN : boolean;
var
  lCpuidLevel1 : TCpuidLevel1;
  lCpuidProcessorName : TCpuidProcessorName;
begin
  result := FALSE;
  //if DetectCpuVendor = eCpuVendorVIA then begin
  lCpuidProcessorName := TCpuidProcessorName.Create;
  try
    lCpuidProcessorName.Initialize;
    if ( Pos( 'VIA', lCpuidProcessorName.ProcessorName ) > 0 )then begin
      lCpuidLevel1 := TCpuidLevel1.Create;
      try
        if ( lCpuidLevel1.Family = 6 ) and ( lCpuidLevel1.Model = 15 )
        then result := TRUE;
      finally
        FreeAndNil( lCpuidLevel1 );
      end; // try..finally
    end;
  finally
    FreeAndNil( lCpuidProcessorName );
  end; // try..finally
end; // function CpuidIsCentaurCN : boolean;

function CpuidIsSupported: Boolean;
{ If the CPUID bit in the EFLAGS register can be toggled, then the chip supports
  the CPUID instruction.
}
const
  CPUID_PRESENT = $200000; // bit 21
asm
    pushad            // store original reg contents onto stack
    pushfd            // push original eflags onto stack
    pop     eax       // get original eflags
    mov     ecx, eax  // save original eflags into ecx
    xor     eax, CPUID_PRESENT // toggle bit 21 of eflags copy that is in EAX
    push    eax       // push modified eflags onto stack
    popfd             // grab modified eflags from stack and apply them
    pushfd            // put new eflags on the stack to see if bit stuck
    pop     eax       // pop eflags from stack into eax
    cmp     eax, ecx  // compare original eflags against eax contents
    mov     [result], FALSE // set result to FALSE in case the bit did not flip
    jz      @finished // exit routine and return FALSE if eflags values are equal
    mov     [result], TRUE // set result to TRUE because bit flipped and CPUID present
    @finished:        // finished, so clean up
    push    ecx       // push original eflags onto stack
    popfd             // restore original eflags
    popad             // restore original reg contents
end;

function CpuidGetProcessorNameString: string;
var
  lCpuidProcessorName : TCpuidProcessorName;
begin
  result := 'Unknown Microprocessor';
  lCpuidProcessorName := TCpuidProcessorName.create;
  try
    result := lCpuidProcessorName.ProcessorName;
  finally
    freeAndNil( lCpuidProcessorName );
  end;
end;

function CpuidGetVendorIDString: string;
var
  x86reg : Tx86_32_GPRegisters;
begin
  result := '';
  x86reg := CpuidExecute( 0 );
  result := trim( x86reg.str_ebx + x86reg.str_edx + x86reg.str_ecx );
end;

function CpuidIsCentaurRngSupported: boolean;
var
  lCpuidCentaurInfo : TCpuidCentaurInfo;
begin
  result := FALSE;
  if CpuidIsSupported then begin
    lCpuidCentaurInfo := TCpuidCentaurInfo.Create;
    try
      if lCpuidCentaurInfo.RandomNumberGenerator and
         lCpuidCentaurInfo.RandomNumberGeneratorEnabled then begin
        result := TRUE;
      end;
    finally
      FreeAndNil( lCpuidCentaurInfo );
    end; // try...finally
  end; // if
end; // function CpuidIsCentaurRngSupported: boolean;

function CpuidIsCentaurAesSupported: boolean;
var
  lCpuidCentaurInfo : TCpuidCentaurInfo;
begin
  result := FALSE;
  if CpuidIsSupported then begin
    lCpuidCentaurInfo := TCpuidCentaurInfo.Create;
    try
      if lCpuidCentaurInfo.AdvancedCryptographyEngine and
         lCpuidCentaurInfo.AdvancedCryptographyEngineEnabled then begin
        result := TRUE;
      end;
    finally
      FreeAndNil( lCpuidCentaurInfo );
    end; // try...finally
  end; // if
end; // function CpuidIsCentaurAesSupported: boolean;

function CpuidIsCentaurAes2Supported: boolean;
var
  lCpuidCentaurInfo : TCpuidCentaurInfo;
begin
  result := FALSE;
  if CpuidIsSupported then begin
    lCpuidCentaurInfo := TCpuidCentaurInfo.Create;
    try
      if lCpuidCentaurInfo.AdvancedCryptographyEngine2 and
         lCpuidCentaurInfo.AdvancedCryptographyEngine2Enabled then begin
        result := TRUE;
      end;
    finally
      FreeAndNil( lCpuidCentaurInfo );
    end; // try...finally
  end; // if
end; // function CpuidIsCentaurAes2Supported: boolean;

function CpuidIsCentaurMontMulSupported: boolean;
var
  lCpuidCentaurInfo : TCpuidCentaurInfo;
begin
  result := FALSE;
  if DetectCpuVendor = eCpuVendorVIA then begin
    lCpuidCentaurInfo := TCpuidCentaurInfo.Create;
    try
      if lCpuidCentaurInfo.PadlockMontgomeryMultiplier and
         lCpuidCentaurInfo.PadlockMontgomeryMultiplierEnabled then begin
        result := TRUE;
      end;
    finally
      FreeAndNil( lCpuidCentaurInfo );
    end; // try...finally
  end; // if
end; // function CpuidIsCentaurMontMulSupported: boolean;

function CpuidIsCentaurHashEngineSupported: boolean;
var
  lCpuidCentaurInfo : TCpuidCentaurInfo;
begin
  result := FALSE;
  if CpuidIsSupported then begin
    lCpuidCentaurInfo := TCpuidCentaurInfo.Create;
    try
      if lCpuidCentaurInfo.PadlockHashEngine and
         lCpuidCentaurInfo.PadlockHashEngineEnabled then begin
        result := TRUE;
      end;
    finally
      FreeAndNil( lCpuidCentaurInfo );
    end; // try...finally
  end; // if
end; // function CpuidIsCentaurMontMulSupported: boolean;

function CpuidIsSseSupported: boolean;
var
  lCpuidLevel1 : TCpuidLevel1;
begin
  result := FALSE;
  if CpuidIsSupported then begin
    lCpuidLevel1 := TCpuidLevel1.Create;
    try
      result := lCpuidLevel1.SSE;
    finally
      FreeAndNil( lCpuidLevel1 );
    end; // try...finally
  end; // if
end; // function CpuidIsSseSupported: boolean;

function CpuidIsSse2Supported: boolean;
var
  lCpuidLevel1 : TCpuidLevel1;
begin
  result := FALSE;
  if CpuidIsSupported then begin
    lCpuidLevel1 := TCpuidLevel1.Create;
    try
      result := lCpuidLevel1.SSE2;
    finally
      FreeAndNil( lCpuidLevel1 );
    end; // try...finally
  end; // if
end; // function CpuidIsSse2Supported: boolean;

function CpuidIsSse3Supported: boolean;
var
  lCpuidLevel1 : TCpuidLevel1;
begin
  result := FALSE;
  if CpuidIsSupported then begin
    lCpuidLevel1 := TCpuidLevel1.Create;
    try
      result := lCpuidLevel1.SSE3;
    finally
      FreeAndNil( lCpuidLevel1 );
    end; // try...finally
  end; // if
end; // function CpuidIsSse3Supported: boolean;

function CpuidIsEnhancedSpeedStepAvailable : Boolean;
var
  lCpuidLevel1 : TCpuidLevel1;
begin
  lCpuidLevel1 := TCpuidLevel1.Create;
  try
    result := lCpuidLevel1.EnhancedSpeedStep;
  finally
    freeAndNil( lCpuidLevel1 );
  end;
end;

function CpuidIsIntelCore: Boolean;
var
  lCpuidLevel1 : TCpuidLevel1;
begin
  result := FALSE;
  if DetectCpuVendor = eCpuVendorIntel then begin
    lCpuidLevel1 := TCpuidLevel1.Create;
    try
      if ( lCpuidLevel1.fFamily = 6 ) and ( lCpuidLevel1.Model = 14 ) then begin
        result := TRUE;
      end; // if
    finally
      FreeAndNil( lCpuidLevel1 );
    end; // try..finally
  end; // if
end; //function CpuidIsYonah: Boolean;

function CpuidIsIntelCore2: Boolean;
var
  lCpuidLevel1 : TCpuidLevel1;
begin
  result := FALSE;
  if DetectCpuVendor = eCpuVendorIntel then begin
    lCpuidLevel1 := TCpuidLevel1.Create;
    try
      if ( lCpuidLevel1.fFamily = 6 ) and ( lCpuidLevel1.Model = 14 )
        and ( lCpuidLevel1.Stepping = 8 ) then begin
        result := TRUE;
      end; // if
    finally
      FreeAndNil( lCpuidLevel1 );
    end; // try..finally
  end; // if
end; //function CpuidIsYonah: Boolean;

function CpuidGetCpuFamily: integer;
var
  lCpuidLevel1 : TCpuidLevel1;
begin
  result := -1;
  lCpuidLevel1 := TCpuidLevel1.Create;
  try
    result := lCpuidLevel1.Family;
  finally
    FreeAndNil( lCpuidLevel1 );
  end; // try..finally
end; //function CpuidGetCpuFamily: integer;

function CpuidGetCpuModel: integer;
var
  lCpuidLevel1 : TCpuidLevel1;
begin
  result := -1;
  lCpuidLevel1 := TCpuidLevel1.Create;
  try
    result := lCpuidLevel1.Model;
  finally
    FreeAndNil( lCpuidLevel1 );
  end; // try..finally
end; //function CpuidGetCpuModel: integer;

function CpuidGetCpuStepping: integer;
var
  lCpuidLevel1 : TCpuidLevel1;
begin
  result := -1;
  lCpuidLevel1 := TCpuidLevel1.Create;
  try
    result := lCpuidLevel1.Stepping;
  finally
    FreeAndNil( lCpuidLevel1 );
  end; // try..finally
end; //function CpuidGetCpuStepping: integer;

function CpuidGetFamilyModelSteppingEncoding: integer;
var
  lCpuidLevel1 : TCpuidLevel1;
begin
  result := -1;
  lCpuidLevel1 := TCpuidLevel1.Create;
  try
    result := lCpuidLevel1.Family * $100
            + lCpuidLevel1.Model * $10
            + lCpuidLevel1.Stepping;
  finally
    FreeAndNil( lCpuidLevel1 );
  end; // try..finally
end; //function CpuidGetCpuStepping: integer;

function CpuidGetC7Temperature: integer;
var
  lCpuidCentaurPerformanceInfo : TCpuidCentaurPerformanceInfo;
begin
  result := -99999;
  lCpuidCentaurPerformanceInfo := TCpuidCentaurPerformanceInfo.Create;
  try
    result := round( lCpuidCentaurPerformanceInfo.CurrentTemperature);
  finally
    FreeAndNil( lCpuidCentaurPerformanceInfo );
  end; // try...finally
end; // function CpuidIsSse3Supported: boolean;

function CpuidGetTransmetaEfficeonClockSpeed : integer;
var
  lCpuidTransmetaPerformanceData : TCpuidTransmetaPerformanceData;
begin
  lCpuidTransmetaPerformanceData := TCpuidTransmetaPerformanceData.Create;
  try
    lCpuidTransmetaPerformanceData.Initialize;
    result := lCpuidTransmetaPerformanceData.CpuMHz;
  finally
    freeAndNil( lCpuidTransmetaPerformanceData );
  end; // try
end; // function CpuidGetTransmetaEfficeonClockSpeed : integer;

function CpuidDumpAll : string;
var
  lCpuidLevel0 : TCpuidLevel0;
  lCpuidLevel1 : TCpuidLevel1;
  lCpuidLevel2 : TCpuidLevel2;
  lCpuidProccessorSerialNumber : TCpuidProccessorSerialNumber;
  lCpuidLevel8000_0000h : TCpuidLevel8000_0000h;
  lCpuidLevel8000_0001h : TCpuidLevel8000_0001h;
  lCpuidProcessorName : TCpuidProcessorName;
  lCpuidL1Cache : TCpuidL1Cache;
  lCpuidL2Cache : TCpuidL2Cache;
  lCpuidEnhancedPowerManagement : TCpuidEnhancedPowerManagement;
  lCpuidAMD64AddressSize : TCpuidAMD64AddressSize;
  lCpuidCentaur : TCpuidCentaur;
  lCpuidCentaurInfo : TCpuidCentaurInfo;
  lCpuidCentaurPerformanceInfo : TCpuidCentaurPerformanceInfo;
  lCpuidTransmeta : TCpuidTransmeta;
  lCpuidTransmeta1 : TCpuidTransmeta1;
  lCpuidTransmetaInfoString : TCpuidTransmetaInfoString;
  lCpuidTransmetaPerformanceData : TCpuidTransmetaPerformanceData;
  s : string;
begin
  if not CpuidIsSupported then begin
    result := 'Cpuid is not supported on your processor';
    exit;
  end; // if not CpuidIsSupported

  result := '';
  lCpuidLevel0 := TCpuidLevel0.create;
  try
    s := lCpuidLevel0.ResultsAsString;
  finally
    freeAndNil( lCpuidLevel0 );
  end;
  result := result + s;

  lCpuidLevel1 := TCpuidLevel1.create;
  try
    s := lCpuidLevel1.ResultsAsString;
  finally
    freeAndNil( lCpuidLevel1 );
  end;
  result := result + CRLF + CRLF + s;

  lCpuidLevel2 := TCpuidLevel2.create;
  try
    s := lCpuidLevel2.ResultsAsString;
  finally
    freeAndNil( lCpuidLevel2 );
  end;
  result := result + CRLF + CRLF + s;

  lCpuidProccessorSerialNumber := TCpuidProccessorSerialNumber.Create;
  try
    s := lCpuidProccessorSerialNumber.ResultsAsString;
  finally
    freeAndNil( lCpuidProccessorSerialNumber );
  end;
  result := result + CRLF + CRLF + s;

  lCpuidLevel8000_0000h := TCpuidLevel8000_0000h.Create;
  try
    s := lCpuidLevel8000_0000h.ResultsAsString;
  finally
    FreeAndNil( lCpuidLevel8000_0000h );
  end; // try..finally
  result := result + CRLF + CRLF + s;

  lCpuidLevel8000_0001h := TCpuidLevel8000_0001h.create;
  try
    s := lCpuidLevel8000_0001h.ResultsAsString;
  finally
    freeAndNil( lCpuidLevel8000_0001h );
  end;
  result := result + CRLF + CRLF + s;

  lCpuidProcessorName := TCpuidProcessorName.create;
  try
    s := lCpuidProcessorName.ResultsAsString;
  finally
    freeAndNil( lCpuidProcessorName );
  end;
  result := result + CRLF + CRLF + s;

  lCpuidL1Cache := TCpuidL1Cache.create;
  try
    s := lCpuidL1Cache.ResultsAsString;
  finally
    freeAndNil( lCpuidL1Cache );
  end;
  result := result + CRLF + CRLF + s;

  lCpuidL2Cache := TCpuidL2Cache.create;
  try
    s := lCpuidL2Cache.ResultsAsString;
  finally
    freeAndNil( lCpuidL2Cache );
  end;
  result := result + CRLF + CRLF + s;

  lCpuidEnhancedPowerManagement := TCpuidEnhancedPowerManagement.create;
  try
    s := lCpuidEnhancedPowerManagement.ResultsAsString;
  finally
    freeAndNil( lCpuidEnhancedPowerManagement );
  end;
  result := result + CRLF + CRLF + s;

  lCpuidAMD64AddressSize := TCpuidAMD64AddressSize.create;
  try
    s := lCpuidAMD64AddressSize.ResultsAsString;
  finally
    freeAndNil( lCpuidAMD64AddressSize );
  end;
  result := result + CRLF + CRLF + s;

  lCpuidCentaur := TCpuidCentaur.create;
  try
    s := lCpuidCentaur.ResultsAsString;
  finally
    freeAndNil( lCpuidCentaur );
  end;
  result := result + CRLF + CRLF + s;

  lCpuidCentaurInfo := TCpuidCentaurInfo.create;
  try
    s := lCpuidCentaurInfo.ResultsAsString;
  finally
    freeAndNil( lCpuidCentaurInfo );
  end;
  result := result + CRLF + CRLF + s;

  lCpuidCentaurPerformanceInfo := TCpuidCentaurPerformanceInfo.Create;
  try
    s := lCpuidCentaurPerformanceInfo.ResultsAsString;
  finally
    freeAndNil( lCpuidCentaurPerformanceInfo );
  end;
  result := result + CRLF + CRLF + s;

  lCpuidTransmeta := TCpuidTransmeta.create;
  try
    s := lCpuidTransmeta.ResultsAsString;
  finally
    freeAndNil( lCpuidTransmeta );
  end;
  result := result + CRLF + CRLF + s;

  lCpuidTransmeta1 := TCpuidTransmeta1.create;
  try
    s := lCpuidTransmeta1.ResultsAsString;
  finally
    freeAndNil( lCpuidTransmeta1 );
  end;
  result := result + CRLF + CRLF + s;

  lCpuidTransmetaInfoString := TCpuidTransmetaInfoString.create;
  try
    s := lCpuidTransmetaInfoString.ResultsAsString;
  finally
    freeAndNil( lCpuidTransmetaInfoString );
  end;
  result := result + CRLF + CRLF + s;

  lCpuidTransmetaPerformanceData := TCpuidTransmetaPerformanceData.create;
  try
    s := lCpuidTransmetaPerformanceData.ResultsAsString;
  finally
    freeAndNil( lCpuidTransmetaPerformanceData );
  end;
  result := result + CRLF + CRLF + s;

end; // function CpuidDumpAll : string;

//==============================================================================
//TCpuid begins...........................................................
constructor TCpuid.Create;
begin
  inherited;
  Initialize;
end; // constructor

procedure TCpuid.Initialize;
begin
end; // procedure TCpuid.Initialize;

function TCpuid.GetResultsAsString : string;
begin
end; // function TCpuid.GetResultsAsString : string;

function TCpuid.DumpX86GPRegisters( ax86Reg : Tx86_32_GPRegisters ) : string;
var
  s : string;
begin
  result := '';
  s := IntToStr( ax86Reg._eax ) + ' (Hex: $' + IntToHex( ax86Reg._eax, 8 ) + ')';
  result := result + CRLF + 'raw eax: ' + s;
  s := IntToStr( ax86Reg._ebx ) + ' (Hex: $' + IntToHex( ax86Reg._ebx, 8 ) + ')';
  result := result + CRLF + 'raw ebx: ' + s;
  s := IntToStr( ax86Reg._ecx ) + ' (Hex: $' + IntToHex( ax86Reg._ecx, 8 ) + ')';
  result := result + CRLF + 'raw ecx: ' + s;
  s := IntToStr( ax86Reg._edx ) + ' (Hex: $' + IntToHex( ax86Reg._edx, 8 ) + ')';
  result := result + CRLF + 'raw edx: ' + s;
end; // function TCpuid.DumpX86GPRegisters

//TCpuid ends.............................................................
//==============================================================================

//==============================================================================
//TCpuidLevel0 begins..................................................
procedure TCpuidLevel0.Initialize;
begin
  fEaxInput := 0;
  fx86Reg := CpuidExecute( fEaxInput );
  fMaxLevel := fx86reg._eax;
  fMaxLevelAsHex := IntToHex( fMaxLevel, 8 );
  fVendor := trim( fx86reg.str_ebx + fx86reg.str_edx + fx86reg.str_ecx );
end; // Initialize

function TCpuidLevel0.GetResultsAsString: string;
begin
  result := '= Dump of CPUID Level 0000 0000h =';
  result := result + DumpX86GPRegisters( fx86Reg );
  result := result + CRLF + 'Vendor ID string: ' + fVendor;
  result := result + CRLF + 'Maximum supported standard CPUID level: $' + fMaxLevelAsHex;
end;

function TCpuidLevel0.GetVendorID: string;
begin
  result := fVendor;
end;
//TCpuidLevel0 ends.............................................................
//==============================================================================

//==============================================================================
//TCpuidLevel1 begins...........................................................
procedure TCpuidLevel1.initialize;
begin
  fEaxInput := 1;
  fx86Reg :=  CpuidExecute( fEaxInput );
  fExtendedFamily   := ExtractBitValue( fx86Reg._eax, 20, 27 );
  fExtendedModel    := ExtractBitValue( fx86Reg._eax, 16, 19 );
  fCpuType          := ExtractBitValue( fx86Reg._eax, 12, 13 );
  fFamily           := ExtractBitValue( fx86Reg._eax, 8, 11 );
  fModel            := ExtractBitValue( fx86Reg._eax, 4, 7 );
  fStepping         := ExtractBitValue( fx86Reg._eax, 0, 3 );
  fBrandID          := ExtractBitValue( fx86Reg._ebx, 0, 7 );
  fCLFlush          := ExtractBitValue( fx86Reg._eax, 8, 15 );
  // This is actually the number of virtual processors per physical processor:
  fCpuCount         := ExtractBitValue( fx86Reg._ebx, 16, 23 );
  fApicId           := ExtractBitValue( fx86Reg._eax, 24, 31 );
  fAvx              := Bit28 in fx86Reg.bits_ecx;
  fAes              := Bit25 in fx86Reg.bits_ecx;
  fSse42            := Bit20 in fx86Reg.bits_ecx;
  fSse41            := Bit19 in fx86Reg.bits_ecx;
  fCx16             := Bit13 in fx86Reg.bits_ecx;
  fCID              := Bit10 in fx86Reg.bits_ecx;
  fTherm2           := Bit8 in fx86Reg.bits_ecx;
  fEnhancedSpeedStep:= Bit7 in fx86Reg.bits_ecx;
  fDSCPL            := Bit4 in fx86Reg.bits_ecx;
  fMon              := Bit3 in fx86Reg.bits_ecx;
  fSSE3             := Bit0 in fx86Reg.bits_ecx;
  fPBE              := Bit31 in fx86Reg.bits_edx;
  fIA64             := Bit30 in fx86Reg.bits_edx;
  fTherm1           := Bit29 in fx86Reg.bits_edx;
  fHyperThreading   := Bit28 in fx86Reg.bits_edx;
  fSelfSnoop        := Bit27 in fx86Reg.bits_edx;
  fSSE2             := Bit26 in fx86Reg.bits_edx;
  fSSE              := Bit25 in fx86Reg.bits_edx;
  fFXSR             := Bit24 in fx86Reg.bits_edx;
  fMMX              := Bit23 in fx86Reg.bits_edx;
  fAcpiThermCtrlMSR := Bit22 in fx86Reg.bits_edx;
  fDebugTraceEmon   := Bit21 in fx86Reg.bits_edx;
  fCLFL             := Bit19 in fx86Reg.bits_edx;
  fProcSerialNum    := Bit18 in fx86Reg.bits_edx;
  fPSE36            := Bit17 in fx86Reg.bits_edx;
  fPAT              := Bit16 in fx86Reg.bits_edx;
  fCMOV             := Bit15 in fx86Reg.bits_edx;
  fMCA              := Bit14 in fx86Reg.bits_edx;
  fPGE              := Bit13 in fx86Reg.bits_edx;
  fMTRR             := Bit12 in fx86Reg.bits_edx;
  fSEP              := Bit11 in fx86Reg.bits_edx;
  fApic             := Bit9 in fx86Reg.bits_edx;
  fCx8              := Bit8 in fx86Reg.bits_edx;
  fMCE              := Bit7 in fx86Reg.bits_edx;
  fPAE              := Bit6 in fx86Reg.bits_edx;
  fMSR              := Bit5 in fx86Reg.bits_edx;
  fTSC              := Bit4 in fx86Reg.bits_edx;
  fPSE              := Bit3 in fx86Reg.bits_edx;
  fDE               := Bit2 in fx86Reg.bits_edx;
  fVME              := Bit1 in fx86Reg.bits_edx;
  fFPU              := Bit0 in fx86Reg.bits_edx;
end; // procedure TCpuidLevel1.initialize;

function TCpuidLevel1.GetResultsAsString : string;
var
  s : string;
begin
  result := '= Dump of CPUID Level 1 =';
  result := result + DumpX86GPRegisters( fx86Reg );
  s := IntToStr( fExtendedFamily );
  result := result + CRLF + 'extended family: ' + s;
  s := IntToStr( fExtendedModel );
  result := result + CRLF + 'extended model: ' + s;
  s := IntToStr( fCpuType );
  result := result + CRLF +  'type: ' + s;
  s := IntToStr( fFamily );
  result := result + CRLF + 'family: ' + s;
  s := IntToStr( fModel );
  result := result + CRLF + 'model: ' + s;
  s := IntToStr( fStepping );
  result := result + CRLF +  'stepping: ' + s;
  s := IntToStr( GetNumberOfProcessors );
  result := result + CRLF + 'Cpu Count: ' + s;
  s := BooleanToYesNo( fAvx );
  result := result + CRLF + 'AVX: ' + s;
  s := BooleanToYesNo( fAes );
  result := result + CRLF + 'AES: ' + s;
  s := BooleanToYesNo( fSse42 );
  result := result + CRLF + 'SSE4.2: ' + s;
  s := BooleanToYesNo( fSse41 );
  result := result + CRLF + 'SSE4.1: ' + s;
  s := BooleanToYesNo( fCx16 );
  result := result + CRLF + 'CmpXChg16B: ' + s;
  s := BooleanToYesNo( fCID );
  result := result + CRLF + 'CID: ' + s;
  s := BooleanToYesNo( fTherm2 );
  result := result + CRLF + 'Therm2: ' + s;
  s := BooleanToYesNo( fEnhancedSpeedStep );
  result := result + CRLF + 'EnhancedSpeedStep: ' + s;
  s := BooleanToYesNo( fDSCPL );
  result := result + CRLF + 'DSCPL: ' + s;
  s := BooleanToYesNo( fMon );
  result := result + CRLF + 'Mon: ' + s;
  s := BooleanToYesNo( fSSE3 );
  result := result + CRLF + 'SSE3: ' + s;
  s := BooleanToYesNo( fPBE );
  result := result + CRLF + 'PBE: ' + s;
  s := BooleanToYesNo( fIA64 );
  result := result + CRLF + 'IA64: ' + s;
  s := BooleanToYesNo( fTherm1 );
  result := result + CRLF + 'Therm1: ' + s;
  s := BooleanToYesNo( fHyperThreading );
  result := result + CRLF + 'HyperThreading: ' + s;
  s := BooleanToYesNo( fSelfSnoop );
  result := result + CRLF + 'SelfSnoop: ' + s;
  s := BooleanToYesNo( fSSE2 );
  result := result + CRLF + 'SSE2: ' + s;
  s := BooleanToYesNo( fSSE );
  result := result + CRLF + 'SSE: ' + s;
  s := BooleanToYesNo( fFXSR );
  result := result + CRLF + 'FXSR: ' + s;
  s := BooleanToYesNo( fMMX );
  result := result + CRLF + 'MMX: ' + s;
  s := BooleanToYesNo( fAcpiThermCtrlMSR );
  result := result + CRLF + 'AcpiThermCtrlMSR: ' + s;
  s := BooleanToYesNo( fDebugTraceEmon );
  result := result + CRLF + 'DebugTraceEmon: ' + s;
  s := BooleanToYesNo( fCLFL );
  result := result + CRLF + 'CLFlush: ' + s;
  s := BooleanToYesNo( fProcSerialNum );
  result := result + CRLF + 'ProcSerialNum: ' + s;
  s := BooleanToYesNo( fPSE36 );
  result := result + CRLF + 'PSE36: ' + s;
  s := BooleanToYesNo( fPAT );
  result := result + CRLF + 'PAT: ' + s;
  s := BooleanToYesNo( fCMOV );
  result := result + CRLF + 'CMOV: ' + s;
  s := BooleanToYesNo( fMCA );
  result := result + CRLF + 'MCA: ' + s;
  s := BooleanToYesNo( fPGE );
  result := result + CRLF + 'PGE: ' + s;
  s := BooleanToYesNo( fMTRR );
  result := result + CRLF + 'MTRR: ' + s;
  s := BooleanToYesNo( fSEP );
  result := result + CRLF + 'SEP: ' + s;
  s := BooleanToYesNo( fApic );
  result := result + CRLF + 'APIC: ' + s;
  s := BooleanToYesNo( fCx8 );
  result := result + CRLF + 'CmpXChg8B: ' + s;
  s := BooleanToYesNo( fMCE );
  result := result + CRLF + 'MCE: ' + s;
  s := BooleanToYesNo( fPAE );
  result := result + CRLF + 'PAE: ' + s;
  s := BooleanToYesNo( fMSR );
  result := result + CRLF + 'MSR: ' + s;
  s := BooleanToYesNo( fTSC );
  result := result + CRLF + 'TSC: ' + s;
  s := BooleanToYesNo( fPSE );
  result := result + CRLF + 'PSE: ' + s;
  s := BooleanToYesNo( fDE );
  result := result + CRLF + 'DE: ' + s;
  s := BooleanToYesNo( fVME );
  result := result + CRLF + 'VME: ' + s;
  s := BooleanToYesNo( fFPU );
  result := result + CRLF + 'FPU: ' + s;

end; // function TCpuidLevel1.GetResultsAsString;
//TCpuidLevel1 ends.............................................................
//==============================================================================

//==============================================================================
//TCpuidLevel2 updated Feb. 4, 2008, Van Smith
//TCpuidLevel2 begins...........................................................
procedure TCpuidLevel2.initialize;
var
  i : integer;
begin
  InitializeDescriptors;
  fEaxInput := 2;
  fx86Reg :=  CpuidExecute( fEaxInput );
  fNbrOfTimesToQuery := ExtractBitValue( fx86Reg._eax, 0, 7 );
  for i := 0 to fNbrOfTimesToQuery do begin
    PopulateDescriptors;
  end;
end; // procedure TCpuidLevel1.initialize;

function TCpuidLevel2.GetResultsAsString : string;
var
  i : integer;
begin
  result := '= Dump of CPUID Level 2 =';
  result := result + DumpX86GPRegisters( fx86Reg );
  for i := low( fDescriptors ) to high( fDescriptors ) do begin
    if fDescriptors[ i ].Found then begin
      result := result + CRLF + fDescriptors[ i ].Description;
//      if i in [$70..$73] then   fTraceCache := fDescriptors[ i ].Description;
//      if i in [$88..$8D] then fL3 := fDescriptors[ i ].Description;
    end; // if
  end;// for
end; // function TCpuidLevel2.GetResultsAsString;

function TCpuidLevel2.IsDescriptorInRegisters(
                                aDescriptor : TCpuidLevel2Descriptor ): Boolean;
begin
  result := FALSE;
  // you need to skip the first byte in eax because it contains the number of
  // times that cpuid level 2 must be called.
  if IsByteInDword( aDescriptor.Value, fx86Reg._eax, 1 ) then begin
    result := TRUE;
    exit;
  end; // if
  if IsByteInDword( aDescriptor.Value, fx86Reg._ebx, 0 ) then begin
    result := TRUE;
    exit;
  end; // if
  if IsByteInDword( aDescriptor.Value, fx86Reg._ecx, 0 ) then begin
    result := TRUE;
    exit;
  end; // if
  if IsByteInDword( aDescriptor.Value, fx86Reg._edx, 0 ) then begin
    result := TRUE;
    exit;
  end; // if
end; // function TCpuidLevel2.IsDescriptorInRegister

procedure TCpuidLevel2.InitializeDescriptors;
var
  i : integer;
begin
  // descriptions taken from: http://www.sandpile.org/ia32/cpuid.htm
  fDescriptors[ 1 ].Value := $01;
  fDescriptors[ 1 ].Description := 'code TLB, 4K pages, 4 ways, 32 entries';
  fDescriptors[ 2 ].Value := $02;
  fDescriptors[ 2 ].Description := 'code TLB, 4M pages, fully, 2 entries';
  fDescriptors[ 3 ].Value := $03;
  fDescriptors[ 3 ].Description := 'data TLB, 4K pages, 4 ways, 64 entries';
  fDescriptors[ 4 ].Value := $04;
  fDescriptors[ 4 ].Description := 'data TLB, 4M pages, 4 ways, 8 entries';
  fDescriptors[ 5 ].Value := $05;
  fDescriptors[ 5 ].Description := 'data TLB, 4M pages, 4 ways, 32 entries';
  fDescriptors[ 6 ].Value := $06;
  fDescriptors[ 6 ].Description := 'code L1 cache, 8 KB, 4 ways, 32 byte lines';
  fDescriptors[ 7 ].Value := $07;
  fDescriptors[ 7 ].Description := 'unknown descriptor encountered: $07';
  fDescriptors[ 8 ].Value := $08;
  fDescriptors[ 8 ].Description := 'code L1 cache, 16 KB, 4 ways, 32 byte lines';
  fDescriptors[ 9 ].Value := $09;
  fDescriptors[ 9 ].Description := 'unknown descriptor encountered: $09';
  fDescriptors[ $A ].Value := $0A;
  fDescriptors[ $A ].Description := 'data L1 cache, 8 KB, 2 ways, 32 byte lines';
  fDescriptors[ $B ].Value := $B;
  fDescriptors[ $B ].Description := 'code TLB, 4M pages, 4 ways, 4 entries';
  fDescriptors[ $C ].Value := $0C;
  fDescriptors[ $C ].Description := 'data L1 cache, 16 KB, 4 ways, 32 byte lines';
  fDescriptors[ $D ].Value := $0D;
  fDescriptors[ $D ].Description := 'unknown descriptor encountered: $0D';
  fDescriptors[ $E ].Value := $0E;
  fDescriptors[ $E ].Description := 'unknown descriptor encountered: $0E';
  fDescriptors[ $F ].Value := $F;
  fDescriptors[ $F ].Description := 'unknown descriptor encountered: $0F';
  fDescriptors[ $10 ].Value := $10;
  fDescriptors[ $10 ].Description := 'data L1 cache, 16 KB, 4 ways, 32 byte lines (IA-64)';
  fDescriptors[ $11 ].Value := $11;
  fDescriptors[ $11 ].Description := 'unknown descriptor encountered: $11';
  fDescriptors[ $12 ].Value := $12;
  fDescriptors[ $12 ].Description := 'unknown descriptor encountered: $12';
  fDescriptors[ $13 ].Value := $13;
  fDescriptors[ $13 ].Description := 'unknown descriptor encountered: $13';
  fDescriptors[ $14 ].Value := $14;
  fDescriptors[ $14 ].Description := 'unknown descriptor encountered: $14';
  fDescriptors[ $15 ].Value := $15;
  fDescriptors[ $15 ].Description := 'code L1 cache, 16 KB, 4 ways, 32 byte lines (IA-64)';
  fDescriptors[ $16 ].Value := $16;
  fDescriptors[ $16 ].Description := 'unknown descriptor encountered: $16';
  fDescriptors[ $17 ].Value := $17;
  fDescriptors[ $17 ].Description := 'unknown descriptor encountered: $17';
  fDescriptors[ $18 ].Value := $18;
  fDescriptors[ $18 ].Description := 'unknown descriptor encountered: $18';
  fDescriptors[ $19 ].Value := $19;
  fDescriptors[ $19 ].Description := 'unknown descriptor encountered: $19';
  fDescriptors[ $1A ].Value := $1A;
  fDescriptors[ $1A ].Description := 'code and data L2 cache, 96 KB, 6 ways, 64 byte lines (IA-64)';
  fDescriptors[ $1B ].Value := $1B;
  fDescriptors[ $1B ].Description := 'unknown descriptor encountered: $1B';
  fDescriptors[ $1C ].Value := $1C;
  fDescriptors[ $1C ].Description := 'unknown descriptor encountered: $1C';
  fDescriptors[ $1D ].Value := $1D;
  fDescriptors[ $1D ].Description := 'unknown descriptor encountered: $1D';
  fDescriptors[ $1E ].Value := $1E;
  fDescriptors[ $1E ].Description := 'unknown descriptor encountered: $1E';
  fDescriptors[ $1F ].Value := $1F;
  fDescriptors[ $1F ].Description := 'unknown descriptor encountered: $1F';
  fDescriptors[ $20 ].Value := $20;
  fDescriptors[ $20 ].Description := 'unknown descriptor encountered: $20';
  fDescriptors[ $21 ].Value := $21;
  fDescriptors[ $21 ].Description := 'unknown descriptor encountered: $21';
  fDescriptors[ $22 ].Value := $22;
  fDescriptors[ $22 ].Description := 'code and data L3 cache, 512 KB, 4 ways (!), 64 byte lines, dual-sectored';
  fDescriptors[ $23 ].Value := $23;
  fDescriptors[ $23 ].Description := 'code and data L3 cache, 1024 KB, 8 ways, 64 byte lines, dual-sectored';
  i := $24;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  fDescriptors[ $25 ].Value := $25;
  fDescriptors[ $25 ].Description := 'code and data L3 cache, 2048 KB, 8 ways, 64 byte lines, dual-sectored';
  i := $26;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $27;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $28;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  fDescriptors[ $29 ].Value := $29;
  fDescriptors[ $29 ].Description := 'code and data L3 cache, 4096 KB, 8 ways, 64 byte lines, dual-sectored';
  i := $2A;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $2B;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  fDescriptors[ $2C ].Value := $2C;
  fDescriptors[ $2C ].Description := 'data L1 cache, 32 KB, 8 ways, 64 byte lines';
  i := $2D;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $2E;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $2F;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  fDescriptors[ $30 ].Value := $30;
  fDescriptors[ $30 ].Description := 'code L1 cache, 32 KB, 8 ways, 64 byte lines';
  i := $31;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $32;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $33;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $34;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $35;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $36;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $37;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $38;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  fDescriptors[ $39 ].Value := $39;
  fDescriptors[ $39 ].Description := 'code and data L2 cache, 128 KB, 4 ways, 64 byte lines, sectored';
  fDescriptors[ $3A ].Value := $3A;
  fDescriptors[ $3A ].Description := 'code and data L2 cache, 192 KB, 6 ways, 64 byte lines, sectored';
  fDescriptors[ $3B ].Value := $3B;
  fDescriptors[ $3B ].Description := 'code and data L2 cache, 128 KB, 2 ways, 64 byte lines, sectored';
  fDescriptors[ $3C ].Value := $3C;
  fDescriptors[ $3C ].Description := 'code and data L2 cache, 256 KB, 4 ways, 64 byte lines, sectored';
  fDescriptors[ $3D ].Value := $3D;
  fDescriptors[ $3D ].Description := 'code and data L2 cache, 384 KB, 6 ways, 64 byte lines, sectored';
  fDescriptors[ $3E ].Value := $3E;
  fDescriptors[ $3E ].Description := 'code and data L2 cache, 512 KB, 4 ways, 64 byte lines, sectored';
  i := $3F;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  fDescriptors[ $40 ].Value := $40;
  fDescriptors[ $40 ].Description := 'no integrated L2 cache (P6 core) or L3 cache (P4 core)';
  fDescriptors[ $41 ].Value := $41;
  fDescriptors[ $41 ].Description := 'code and data L2 cache, 128 KB, 4 ways, 32 byte lines';
  fDescriptors[ $42 ].Value := $42;
  fDescriptors[ $42 ].Description := 'code and data L2 cache, 256 KB, 4 ways, 32 byte lines';
  fDescriptors[ $43 ].Value := $43;
  fDescriptors[ $43 ].Description := 'code and data L2 cache, 512 KB, 4 ways, 32 byte lines';
  fDescriptors[ $44 ].Value := $44;
  fDescriptors[ $44 ].Description := 'code and data L2 cache, 1024 KB, 4 ways, 32 byte lines';
  fDescriptors[ $45 ].Value := $45;
  fDescriptors[ $45 ].Description := 'code and data L2 cache, 2048 KB, 4 ways, 32 byte lines';
  fDescriptors[ $46 ].Value := $46;
  fDescriptors[ $46 ].Description := 'code and data L3 cache, 4096 KB, 4 ways, 64 byte lines';
  fDescriptors[ $47 ].Value := $47;
  fDescriptors[ $47 ].Description := 'code and data L3 cache, 8192 KB, 8 ways, 64 byte lines';
  fDescriptors[ $48 ].Value := $48;
  fDescriptors[ $48 ].Description := 'code and data L2 cache, 3072 KB, 12 ways, 64 byte lines';
  fDescriptors[ $49 ].Value := $49;
  if CpuidIsIntelCore2 then begin
    fDescriptors[ $49 ].Description := 'code and data L2 cache, 4096 KB, 16 ways, 64 byte lines (Core 2)';
  end else begin
    fDescriptors[ $49 ].Description := 'code and data L3 cache, 4096 KB, 16 ways, 64 byte lines (P4)';
  end;
  fDescriptors[ $4A ].Value := $4A;
  fDescriptors[ $4A ].Description := 'code and data L3 cache, 6144 KB, 12 ways, 64 byte lines';
  fDescriptors[ $4B ].Value := $4B;
  fDescriptors[ $4B ].Description := 'code and data L3 cache, 8192 KB, 16 ways, 64 byte lines';
  fDescriptors[ $4C ].Value := $4C;
  fDescriptors[ $4C ].Description := 'code and data L3 cache, 12288 KB, 12 ways, 64 byte lines';
  fDescriptors[ $4D ].Value := $4D;
  fDescriptors[ $4D ].Description := 'code and data L3 cache, 16384 KB, 16 ways, 64 byte lines';
  fDescriptors[ $4E ].Value := $4E;
  fDescriptors[ $4E ].Description := 'code and data L2 cache, 6144 KB, 24 ways, 64 byte lines';
  i := $4F;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  fDescriptors[ $50 ].Value := $50;
  fDescriptors[ $50 ].Description := 'code TLB, 4K/4M/2M pages, fully, 64 entries';
  fDescriptors[ $51 ].Value := $51;
  fDescriptors[ $51 ].Description := 'code TLB, 4K/4M/2M pages, fully, 128 entries';
  fDescriptors[ $52 ].Value := $52;
  fDescriptors[ $52 ].Description := 'code TLB, 4K/4M/2M pages, fully, 256 entries';
  i := $53;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $54;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $55;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  fDescriptors[ $56 ].Value := $56;
  fDescriptors[ $56 ].Description := 'L0 data TLB, 4M pages, 4 ways, 16 entries';
  fDescriptors[ $57 ].Value := $57;
  fDescriptors[ $57 ].Description := 'L0 data TLB, 4K pages, 4 ways, 16 entries';
  i := $58;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $59;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $5A;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  fDescriptors[ $5B ].Value := $5B;
  fDescriptors[ $5B ].Description := 'data TLB, 4K/4M pages, fully, 64 entries';
  fDescriptors[ $5C ].Value := $5C;
  fDescriptors[ $5C ].Description := 'data TLB, 4K/4M pages, fully, 128 entries';
  fDescriptors[ $5D ].Value := $5D;
  fDescriptors[ $5D ].Description := 'data TLB, 4K/4M pages, fully, 256 entries';
  i := $5F;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  // added for Prescott:
  i := $60;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'data L1 cache, 16 KB, 8 ways, 64 byte lines, sectored';
  i := $66;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'data L1 cache, 8 KB, 4 ways, 64 byte lines, sectored';
  i := $67;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'data L1 cache, 16 KB, 4 ways, 64 byte lines, sectored';
  i := $68;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'data L1 cache, 32 KB, 4 ways, 64 byte lines, sectored';
  i := $69;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $6A;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $6B;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $6C;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $6D;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $6E;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $6F;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $70;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'trace L1 cache, 12 K microOPs, 8 ways';
  i := $71;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'trace L1 cache, 16 K microOPs, 8 ways';
  i := $72;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'trace L1 cache, 32 K microOPs, 8 ways';
  i := $73;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'trace L1 cache, 64 K�OPs, 8 ways';
  i := $74;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $75;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $76;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := 77;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code L1 cache, 16 KB, 4 ways, 64 byte lines, sectored (IA-64)';
  i := $78;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code and data L2 cache, 1024 KB, 4 ways, 64 byte lines';
  i := $79;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code and data L2 cache, 128 KB, 8 ways, 64 byte lines, dual-sectored';
  i := $7A;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code and data L2 cache, 256 KB, 8 ways, 64 byte lines, dual-sectored';
  i := $7B;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code and data L2 cache, 512 KB, 8 ways, 64 byte lines, dual-sectored';
  i := $7C;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code and data L2 cache, 1024 KB, 8 ways, 64 byte lines, dual-sectored';
  // added for Dothan:
  i := $7D;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code and data L2 cache, 2048 KB, 8 ways, 64 byte lines';
  i := $7E;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code and data L2 cache, 256 KB, 8 ways, 128 byte lines, sect. (IA-64)';
  i := $7F;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code and data L2 cache, 512 KB, 2 ways, 64 byte lines';
  i := $80;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $81;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code and data L2 cache, 128 KB, 8 ways, 32 byte lines';
  i := $82;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code and data L2 cache, 256 KB, 8 ways, 32 byte lines';
  i := $83;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code and data L2 cache, 512 KB, 8 ways, 32 byte lines';
  i := $84;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code and data L2 cache, 1024 KB, 8 ways, 32 byte lines';
  i := $85;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code and data L2 cache, 2048 KB, 8 ways, 32 byte lines';
  i := $86;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code and data L2 cache, 512 KB, 4 ways, 64 byte lines';
  i := $87;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code and data L2 cache, 1024 KB, 8 ways, 64 byte lines';
  i := $88;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code and data L3 cache, 2048 KB, 4 ways, 64 byte lines (IA-64)';
  i := $89;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code and data L3 cache, 4096 KB, 4 ways, 64 byte lines (IA-64)';
  i := $8A;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code and data L3 cache, 8192 KB, 4 ways, 64 byte lines (IA-64)';
  i := $8B;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $8C;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $8D;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code and data L3 cache, 3096 KB, 12 ways, 128 byte lines (IA-64)';
  i := $8E;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $8F;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $90;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code TLB, 4K...256M pages, fully, 64 entries (IA-64)';
  i := $91;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $92;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $93;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $94;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $95;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $96;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'data L1 TLB, 4K...256M pages, fully, 32 entries (IA-64)';
  i := $97;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $98;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $99;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $9A;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $9B;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'data L2 TLB, 4K...256M pages, fully, 96 entries (IA-64)';
  i := $9C;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $9D;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $9E;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $9F;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $A0;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $A1;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $A2;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $A3;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $A4;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $A5;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $A6;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $A7;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $A8;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $A9;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $AA;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $AB;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $AC;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $AD;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $AE;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $AF;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $B0;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'code TLB, 4K pages, 4 ways, 128 entries';
  i := $B1;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := ' code TLB, 4M pages, 4 ways, 4 entries and code TLB, 2M pages, 4 ways, 8 entries';
  i := $B2;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $B3;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'data TLB, 4K pages, 4 ways, 128 entries';
  i := $B4;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'data TLB, 4K pages, 4 ways, 256 entries';
  i := $B5;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $B6;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $B7;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $B8;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $B9;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $BA;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $BB;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $BC;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $BD;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $BE;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $BF;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $C0;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $C1;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $C2;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $C3;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $C4;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $C5;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $C6;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $C7;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $C8;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $C9;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $CA;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $CB;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $CC;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $CD;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $CE;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $CF;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $D0;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $D1;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $D2;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $D3;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $D4;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $D5;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $D6;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $D7;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $D8;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $D9;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $DA;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $DB;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $DC;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $DD;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $DE;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $DF;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $E0;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $E1;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $E2;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $E3;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $E4;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $E5;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $E6;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $E7;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $E8;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $E9;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $EA;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $EB;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $EC;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $ED;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $EE;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $EF;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $F0;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := '64 byte prefetching';
  i := $F1;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := '128 byte prefetching';
  i := $F2;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $F3;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $F4;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $F5;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $F6;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $F7;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $F8;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $F9;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $FA;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $FB;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $FC;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $FD;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $FE;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
  i := $FF;
  fDescriptors[ i ].Value := i;
  fDescriptors[ i ].Description := 'unknown descriptor encountered: ' + intToStr( i );
end; // procedure TCpuidLevel2.InitializeDescriptors;

procedure TCpuidLevel2.PopulateDescriptors;
var
  i : integer;
begin
  for i := low( fDescriptors ) to high( fDescriptors ) do begin
    if IsDescriptorInRegisters( fDescriptors[ i ] ) then begin
      fDescriptors[ i ].Found := TRUE;
      case i of
      $1 :
        begin
          fL1_CodeTlb_4kB_Ass := 4;
          fL1_CodeTlb_4kB_Entries := 32;
        end;
      $2 :
        begin

        end;
      $3 :
        begin
        end;
      $4 :
        begin
        end;
      $6 :
        begin
          fL1_CodeCacheSize := 8;
          fL1_CodeCacheAss := 4;
          fL1_CodeCacheLinesSize := 32;
        end;
      $8, $15 :
        begin
          fL1_CodeCacheSize := 16;
          fL1_CodeCacheAss := 4;
          fL1_CodeCacheLinesSize := 32;
        end;
      $A :
        begin
          fL1_DataCacheSize := 8;
          fL1_DataCacheAss := 2;
          fL1_DataCacheLinesSize := 32;
        end;
      $C, $D :
        begin
          fL1_DataCacheSize := 16;
          fL1_DataCacheAss := 4;
          fL1_DataCacheLinesSize := 32;
        end;
      $1A :
        begin
          fL2_CacheSize := 96;
          fL2_CacheAss := 6;
          fL2_CacheLinesSize := 64;
        end;
      $22 :
        begin
          fL3 := 'code and data L3 cache, 512 KB, 4 ways (!), 64 byte lines, dual-sectored';
        end;
      $23 :
        begin
          fL3 := 'code and data L3 cache, 1024 KB, 8 ways, 64 byte lines, dual-sectored';
        end;
      $25 :
        begin
          fL3 := 'code and data L3 cache, 2048 KB, 8 ways, 64 byte lines, dual-sectored';
        end;
      $29 :
        begin
          fL3 := 'code and data L3 cache, 4096 KB, 8 ways, 64 byte lines, dual-sectored';
        end;
      $2C :
        begin
          fL1_DataCacheSize := 32;
          fL1_DataCacheAss := 8;
          fL1_DataCacheLinesSize := 64;
        end;
      $30 :
        begin
          fL1_CodeCacheSize := 32;
          fL1_CodeCacheAss := 8;
          fL1_CodeCacheLinesSize := 64;
        end;
      $39 :
        begin
          fL2_CacheSize := 128;
          fL2_CacheAss := 4;
          fL2_CacheLinesSize := 64;
        end;
      $3A :
        begin
          fL2_CacheSize := 192;
          fL2_CacheAss := 6;
          fL2_CacheLinesSize := 64;
        end;
      $3B :
        begin
          fL2_CacheSize := 128;
          fL2_CacheAss := 2;
          fL2_CacheLinesSize := 64;
        end;
      $3C :
        begin
          fL2_CacheSize := 256;
          fL2_CacheAss := 4;
          fL2_CacheLinesSize := 64;
        end;
      $3D :
        begin
          fL2_CacheSize := 384;
          fL2_CacheAss := 6;
          fL2_CacheLinesSize := 64;
        end;
      $3E :
        begin
          fL2_CacheSize := 512;
          fL2_CacheAss := 4;
          fL2_CacheLinesSize := 64;
        end;
       $40:
        begin
          fL2_CacheSize := 0;
          fL2_CacheAss := 0;
          fL2_CacheLinesSize := 0;
        end;
      $41 :
        begin
          fL2_CacheSize := 128;
          fL2_CacheAss := 4;
          fL2_CacheLinesSize := 32;
        end;
      $42 :
        begin
          fL2_CacheSize := 256;
          fL2_CacheAss := 4;
          fL2_CacheLinesSize := 32;
        end;
      $43 :
        begin
          fL2_CacheSize := 512;
          fL2_CacheAss := 4;
          fL2_CacheLinesSize := 32;
        end;
      $44 :
        begin
          fL2_CacheSize := 1024;
          fL2_CacheAss := 4;
          fL2_CacheLinesSize := 32;
        end;
      $45 :
        begin
          fL2_CacheSize := 2048;
          fL2_CacheAss := 4;
          fL2_CacheLinesSize := 32;
        end;
      $48 :
        begin
          fL2_CacheSize := 3072;
          fL2_CacheAss := 12;
          fL2_CacheLinesSize := 64;
        end;
      $49 :
        begin
          if CpuidIsIntelCore2 then begin
            fL2_CacheSize := 3072;
            fL2_CacheAss := 12;
            fL2_CacheLinesSize := 64;
          end; // if
        end;
      $4E :
        begin
          fL2_CacheSize := 6144;
          fL2_CacheAss := 24;
          fL2_CacheLinesSize := 64;
        end;
      $60 :
        begin
          fL1_DataCacheSize := 16;
          fL1_DataCacheAss := 8;
          fL1_DataCacheLinesSize := 64;
        end;
      $66 :
        begin
          fL1_DataCacheSize := 8;
          fL1_DataCacheAss := 4;
          fL1_DataCacheLinesSize := 64;
        end;
      $67 :
        begin
          fL1_DataCacheSize := 16;
          fL1_DataCacheAss := 4;
          fL1_DataCacheLinesSize := 64;
        end;
      $68 :
        begin
          fL1_DataCacheSize := 32;
          fL1_DataCacheAss := 4;
          fL1_DataCacheLinesSize := 64;
        end;
      $70 :
        begin
          fTraceCache := 'trace L1 cache, 12 K microOPs, 8 ways';
        end;
      $71 :
        begin
          fTraceCache := 'trace L1 cache, 16 K microOPs, 8 ways';
        end;
      $72 :
        begin
          fTraceCache := 'trace L1 cache, 32 K microOPs, 8 ways';
        end;
      $73 :
        begin
          fTraceCache := 'trace L1 cache, 64 K microOPs, 8 ways';
        end;
      $77 :
        begin
          fL1_CodeCacheSize := 16;
          fL1_CodeCacheAss := 4;
          fL1_CodeCacheLinesSize := 64;
        end;
      $78 :
        begin
          fL2_CacheSize := 1024;
          fL2_CacheAss := 4;
          fL2_CacheLinesSize := 64;
        end;
      $79 :
        begin
          fL2_CacheSize := 128;
          fL2_CacheAss := 8;
          fL2_CacheLinesSize := 64;
        end;
      $7A :
        begin
          fL2_CacheSize := 256;
          fL2_CacheAss := 8;
          fL2_CacheLinesSize := 64;
        end;
      $7B :
        begin
          fL2_CacheSize := 512;
          fL2_CacheAss := 8;
          fL2_CacheLinesSize := 64;
        end;
      $7C :
        begin
          fL2_CacheSize := 1024;
          fL2_CacheAss := 8;
          fL2_CacheLinesSize := 64;
        end;
      $7D :
        begin
          fL2_CacheSize := 2048;
          fL2_CacheAss := 8;
          fL2_CacheLinesSize := 64;
        end;
      $7E :
        begin
          fL2_CacheSize := 256;
          fL2_CacheAss := 8;
          fL2_CacheLinesSize := 128;
        end;
      $7F :
        begin
          fL2_CacheSize := 512;
          fL2_CacheAss := 2;
          fL2_CacheLinesSize := 64;
        end;
      $81 :
        begin
          fL2_CacheSize := 128;
          fL2_CacheAss := 8;
          fL2_CacheLinesSize := 32;
        end;
      $82 :
        begin
          fL2_CacheSize := 256;
          fL2_CacheAss := 8;
          fL2_CacheLinesSize := 32;
        end;
      $83 :
        begin
          fL2_CacheSize := 512;
          fL2_CacheAss := 8;
          fL2_CacheLinesSize := 32;
        end;
      $84 :
        begin
          fL2_CacheSize := 1024;
          fL2_CacheAss := 8;
          fL2_CacheLinesSize := 32;
        end;
      $85 :
        begin
          fL2_CacheSize := 2048;
          fL2_CacheAss := 8;
          fL2_CacheLinesSize := 32;
        end;
      $86 :
        begin
          fL2_CacheSize := 512;
          fL2_CacheAss := 4;
          fL2_CacheLinesSize := 64;
        end;
      $87 :
        begin
          fL2_CacheSize := 1024;
          fL2_CacheAss := 8;
          fL2_CacheLinesSize := 64;
        end;
      $88 :
        begin
          fL3_CacheSize := 2048;
          fL3_CacheAss := 4;
          fL3_CacheLinesSize := 64;
        end;
      $89 :
        begin
          fL3_CacheSize := 4096;
          fL3_CacheAss := 4;
          fL3_CacheLinesSize := 64;
        end;
      $8A :
        begin
          fL3_CacheSize := 8192;
          fL3_CacheAss := 4;
          fL3_CacheLinesSize := 64;
        end;
      $8D :
        begin
          fL3_CacheSize := 3096;
          fL3_CacheAss := 12;
          fL3_CacheLinesSize := 128;
        end;
      end; // case
    end; // if
  end;// for
end; // procedure TCpuidLevel2.PopulateDescriptors;
//TCpuidLevel1 ends.............................................................
//==============================================================================

//==============================================================================
//TCpuidProccessorSerialNumber begins...........................................
procedure TCpuidProccessorSerialNumber.Initialize;
begin
  fEaxInput := 3;
  fx86Reg :=  CpuidExecute( fEaxInput );
  fVendor :=  CpuidGetVendorIDString;
end; // procedure TCpuidProccessorSerialNumber.Initialize;

function TCpuidProccessorSerialNumber.GetProcessorSerialNumber : string;
begin
  result := '';
  if fVendor = 'GenuineTMx86' then begin
    result := IntToHex( fx86Reg._ebx, 8 );
  end;
  result := result + IntToHex( fx86Reg._ecx, 8 );
  result := result + IntToHex( fx86Reg._edx, 8 );
end; //function GetProcessorSerialNumber : string;

function TCpuidProccessorSerialNumber.GetResultsAsString : string;
begin
  result := '= Dump of CPUID Level 3 (Processor Serial Number) =';
  result := result + DumpX86GPRegisters( fx86Reg );
  result := result + CRLF + 'Processor Serial Number (hex):';
  result := result + CRLF + GetProcessorSerialNumber;
end; // function GetResultsAsString : string;
//TCpuidProccessorSerialNumber ends.............................................
//==============================================================================

//==============================================================================
//TCpuidLevel8000_0000h begins..................................................
procedure TCpuidLevel8000_0000h.Initialize;
begin
  fEaxInput := $80000000;
  fx86Reg := CpuidExecute( fEaxInput );
  fMaxLevelAsHex := IntToHex( fx86reg._eax, 8 );
  fMaxLevel := fx86reg._eax;
  fVendor := trim( fx86reg.str_ebx + fx86reg.str_edx + fx86reg.str_ecx );
end; // Initialize

function TCpuidLevel8000_0000h.GetResultsAsString: string;
begin
  result := '= Dump of CPUID Level 8000 0000h (Extended Vendor ID) =';
  result := result + DumpX86GPRegisters( fx86Reg );
  result := result + CRLF + 'Vendor ID string: ' + fVendor;
  result := result + CRLF + 'Maximum supported extended level: ' + fMaxLevelAsHex;
end;

function TCpuidLevel8000_0000h.GetVendorID: string;
begin
  result := fVendor;
end;
//TCpuidLevel8000_0000h ends....................................................
//==============================================================================


//==============================================================================
//TCpuidLevel8000_0001h begins..................................................
procedure TCpuidLevel8000_0001h.initialize;
begin
  fEaxInput := $80000001;
  fx86Reg :=  CpuidExecute( fEaxInput );
  fExtendedFamily   := ExtractBitValue( fx86Reg._eax, 20, 27 );
  fExtendedModel    := ExtractBitValue( fx86Reg._eax, 16, 19 );
  fFamily           := ExtractBitValue( fx86Reg._eax, 8, 11 );
  fModel            := ExtractBitValue( fx86Reg._eax, 4, 7 );
  fStepping         := ExtractBitValue( fx86Reg._eax, 0, 3 );
  fBrandID          := ExtractBitValue( fx86Reg._ebx, 0, 11 );
  f3dNow            := Bit31 in fx86Reg.bits_edx;
  f3dNowPlus        := Bit30 in fx86Reg.bits_edx;
  fAMD64LM          := Bit29 in fx86Reg.bits_edx;
  fFXSR             := Bit24 in fx86Reg.bits_edx;
  fMMX              := Bit23 in fx86Reg.bits_edx;
  fMMXPlus          := Bit22 in fx86Reg.bits_edx; 
  fNX               := Bit20 in fx86Reg.bits_edx;
  fMP               := Bit19 in fx86Reg.bits_edx;
  fPSE36            := Bit17 in fx86Reg.bits_edx;
  fPAT              := Bit16 in fx86Reg.bits_edx;
  fCMOV             := Bit15 in fx86Reg.bits_edx;
  fMCA              := Bit14 in fx86Reg.bits_edx;
  fPGE              := Bit13 in fx86Reg.bits_edx;
  fMTRR             := Bit12 in fx86Reg.bits_edx;
  fSEP              := Bit11 in fx86Reg.bits_edx;
  fApic             := Bit9 in fx86Reg.bits_edx;
  fCx8              := Bit8 in fx86Reg.bits_edx;
  fMCE              := Bit7 in fx86Reg.bits_edx;
  fPAE              := Bit6 in fx86Reg.bits_edx;
  fMSR              := Bit5 in fx86Reg.bits_edx;
  fTSC              := Bit4 in fx86Reg.bits_edx;
  fPSE              := Bit3 in fx86Reg.bits_edx;
  fDE               := Bit2 in fx86Reg.bits_edx;
  fVME              := Bit1 in fx86Reg.bits_edx;
  fFPU              := Bit0 in fx86Reg.bits_edx;
end; // procedure TCpuidLevel8000_0001h.initialize;

function TCpuidLevel8000_0001h.GetResultsAsString : string;
var
  s : string;
begin
  result := '= Dump of CPUID Level $' + IntToHex(fEaxInput, 8) + ' =';
  result := result + DumpX86GPRegisters( fx86Reg );
  s := IntToStr( fExtendedFamily );
  result := result + CRLF + 'extended family: ' + s;
  s := IntToStr( fExtendedModel );
  result := result + CRLF + 'extended model: ' + s;
  s := IntToStr( fFamily );
  result := result + CRLF + 'family: ' + s;
  s := IntToStr( fModel );
  result := result + CRLF + 'model: ' + s;
  s := IntToStr( fStepping );
  result := result + CRLF +  'stepping: ' + s;
  s := IntToStr( fBrandID );
  result := result + CRLF +  'BrandID: ' + s;
  s := BooleanToYesNo( f3dNow );
  result := result + CRLF + '3dNow: ' + s;
  s := BooleanToYesNo( f3dNowPlus );
  result := result + CRLF + '3dNowPlus: ' + s;
  s := BooleanToYesNo( fAMD64LM );
  result := result + CRLF + 'AMD64LM: ' + s;
  s := BooleanToYesNo( fMMXPlus );
  result := result + CRLF + 'MMXPlus (AMD: FXSR): ' + s;
  s := BooleanToYesNo( fMMX );
  result := result + CRLF + 'MMX: ' + s;
  s := BooleanToYesNo( fNX );
  result := result + CRLF + 'NX: ' + s;
  s := BooleanToYesNo( fMP );
  result := result + CRLF + 'MP: ' + s;
  s := BooleanToYesNo( fPSE36 );
  result := result + CRLF + 'PSE36: ' + s;
  s := BooleanToYesNo( fPAT );
  result := result + CRLF + 'PAT: ' + s;
  s := BooleanToYesNo( fCMOV );
  result := result + CRLF + 'CMOV: ' + s;
  s := BooleanToYesNo( fMCA );
  result := result + CRLF + 'MCA: ' + s;
  s := BooleanToYesNo( fPGE );
  result := result + CRLF + 'PGE: ' + s;
  s := BooleanToYesNo( fMTRR );
  result := result + CRLF + 'MTRR: ' + s;
  s := BooleanToYesNo( fSEP );
  result := result + CRLF + 'SEP: ' + s;
  s := BooleanToYesNo( fApic );
  result := result + CRLF + 'APIC: ' + s;
  s := BooleanToYesNo( fCx8 );
  result := result + CRLF + 'CmpXChg8B: ' + s;
  s := BooleanToYesNo( fMCE );
  result := result + CRLF + 'MCE: ' + s;
  s := BooleanToYesNo( fPAE );
  result := result + CRLF + 'PAE: ' + s;
  s := BooleanToYesNo( fMSR );
  result := result + CRLF + 'MSR: ' + s;
  s := BooleanToYesNo( fTSC );
  result := result + CRLF + 'TSC: ' + s;
  s := BooleanToYesNo( fPSE );
  result := result + CRLF + 'PSE: ' + s;
  s := BooleanToYesNo( fDE );
  result := result + CRLF + 'DE: ' + s;
  s := BooleanToYesNo( fVME );
  result := result + CRLF + 'VME: ' + s;
  s := BooleanToYesNo( fFPU );
  result := result + CRLF + 'FPU: ' + s;
end; // function TCpuidLevel8000_0001h.GetResultsAsString;

//TCpuidLevel8000_0001h ends....................................................
//==============================================================================

//TCpuidProcessorName begins....................................................
//==============================================================================
procedure TCpuidProcessorName.Initialize;
begin
  fEaxInput := $80000002;
  fx86Reg1 := CpuidExecute( fEaxInput );
  fx86Reg2 := CpuidExecute( fEaxInput + 1 );
  fx86Reg3 := CpuidExecute( fEaxInput + 2 );
  fProcessorName := trim( fx86Reg1.str + fx86Reg2.str + fx86Reg3.str );
end; //TCpuidProcessorName.Initialize

function TCpuidProcessorName.GetResultsAsString : string;
begin
  result := '= Dump of CPUID Processor Name =';
  result := result + CRLF + 'Call 1:';
  result := result + DumpX86GPRegisters( fx86Reg1 );
  result := result + CRLF + 'Call 2:';
  result := result + DumpX86GPRegisters( fx86Reg2 );
  result := result + CRLF + 'Call 3:';
  result := result + DumpX86GPRegisters( fx86Reg3 );
  result := result + CRLF + 'Processor Name: ' + fProcessorName;
end; // function TCpuidProcessorName.GetResultsAsString
//TCpuidProcessorName ends......................................................
//==============================================================================

//==============================================================================
//TCpuidL1Cache begin...........................................................
procedure TCpuidL1Cache.Initialize;
begin
  fEaxInput := $80000005;
  fx86Reg := CpuidExecute( fEaxInput );
  // 4/2MB L1 TLB:
  fL1_DataTlb_4_2MB_Ass := ExtractBitValue( fx86Reg._eax, 24, 31 );
  fL1_DataTlb_4_2MB_Entries := ExtractBitValue( fx86Reg._eax, 16, 23 );
  fL1_CodeTlb_4_2MB_Ass := ExtractBitValue( fx86Reg._eax, 8, 15 );
  fL1_CodeTlb_4_2MB_Entries := ExtractBitValue( fx86Reg._eax, 0, 7 );
  // 4kB L1 TLB:
  fL1_DataTlb_4kB_Ass := ExtractBitValue( fx86Reg._ebx, 24, 31 );
  fL1_DataTlb_4kB_Entries := ExtractBitValue( fx86Reg._ebx, 16, 23 );
  fL1_CodeTlb_4kB_Ass := ExtractBitValue( fx86Reg._ebx, 8, 15 );
  fL1_CodeTlb_4kB_Entries := ExtractBitValue( fx86Reg._ebx, 0, 7 );
  // L1-d
  fL1_DataCacheSize := ExtractBitValue( fx86Reg._ecx, 24, 31 );
  fL1_DataCacheAss := ExtractBitValue( fx86Reg._ecx, 16, 23 );
  fL1_DataCacheLinesPerTag := ExtractBitValue( fx86Reg._ecx, 8, 15 );
  fL1_DataCacheLinesSize := ExtractBitValue( fx86Reg._ecx, 0, 7 );
  // L1-code
  fL1_CodeCacheSize := ExtractBitValue( fx86Reg._edx, 24, 31 );
  fL1_CodeCacheAss := ExtractBitValue( fx86Reg._edx, 16, 23 );
  fL1_CodeCacheLinesPerTag := ExtractBitValue( fx86Reg._edx, 8, 15 );
  fL1_CodeCacheLinesSize := ExtractBitValue( fx86Reg._edx, 0, 7 );
end; // procedure TCpuidLevel8000_0005h.Initialize;

function TCpuidL1Cache.GetResultsAsString : string;
var
  s : string;
begin
  result := '= Dump of CPUID L1 Info (Level 8000 0005h) =';
  result := result + DumpX86GPRegisters( fx86Reg );

  s := 'L1 Data TLB 4/2 MB associativity: ' + IntToStr( fL1_DataTlb_4_2MB_Ass );
  result := result + CRLF + s;
  s := 'L1 Data TLB 4/2 MB entries: ' + IntToStr( fL1_DataTlb_4_2MB_Entries );
  result := result + CRLF + s;
  s := 'L1 Code TLB 4/2 MB associativity: ' + IntToStr( fL1_CodeTlb_4_2MB_Ass );
  result := result + CRLF + s;
  s := 'L1 Code TLB 4/2 MB entries: ' + IntToStr( fL1_CodeTlb_4_2MB_Entries );
  result := result + CRLF + s;

  s := 'L1 Data TLB 4 kB associativity: ' + IntToStr( fL1_DataTlb_4kB_Ass );
  result := result + CRLF + s;
  s := 'L1 Data TLB 4 kB entries: ' + IntToStr( fL1_DataTlb_4kB_Entries );
  result := result + CRLF + s;
  s := 'L1 Code TLB 4 kB associativity: ' + IntToStr( fL1_CodeTlb_4kB_Ass );
  result := result + CRLF + s;
  s := 'L1 Code TLB 4 kB entries: ' + IntToStr( fL1_CodeTlb_4kB_Entries );
  result := result + CRLF + s;

  s := 'L1 data cache size (kB): ' + IntToStr( fL1_DataCacheSize );
  result := result + CRLF + s;
  s := 'L1 data cache associativity: ' + IntToStr( fL1_DataCacheAss );
  result := result + CRLF + s;
  s := 'L1 data cache lines per tag: ' + IntToStr( fL1_DataCacheLinesPerTag );
  result := result + CRLF + s;
  s := 'L1 data cache lines size in bytes: ' + IntToStr( fL1_DataCacheLinesSize );
  result := result + CRLF + s;

  s := 'L1 code cache size (kB): ' + IntToStr( fL1_CodeCacheSize );
  result := result + CRLF + s;
  s := 'L1 code cache associativity: ' + IntToStr( fL1_CodeCacheAss );
  result := result + CRLF + s;
  s := 'L1 code cache lines per tag: ' + IntToStr( fL1_CodeCacheLinesPerTag );
  result := result + CRLF + s;
  s := 'L1 code cache lines size in bytes: ' + IntToStr( fL1_CodeCacheLinesSize );
  result := result + CRLF + s;
end;//function TCpuidL1Cache.GetResultsAsString
//TCpuidL1Cache begin...................................................
//==============================================================================

//==============================================================================
//TCpuidL2Cache begin...........................................................
procedure TCpuidL2Cache.Initialize;
begin
  fEaxInput := $80000006;
  fx86Reg := CpuidExecute( fEaxInput );
  // 4/2MB L2 TLB:

  fL2_DataTlb_4_2MB_Ass := ExtractBitValue( fx86Reg._eax, 28, 31 );
  if (fL2_DataTlb_4_2MB_Ass = 6) then fL2_DataTlb_4_2MB_Ass := 8
  else if (fL2_DataTlb_4_2MB_Ass = 8) then fL2_DataTlb_4_2MB_Ass := 16
  else if (fL2_DataTlb_4_2MB_Ass = 10) then fL2_DataTlb_4_2MB_Ass := 32; //for centaur c5j

  fL2_DataTlb_4_2MB_Entries := ExtractBitValue( fx86Reg._eax, 16, 27 );

  fL2_CodeTlb_4_2MB_Ass := ExtractBitValue( fx86Reg._eax, 12, 15 );
  if (fL2_CodeTlb_4_2MB_Ass = 6) then fL2_CodeTlb_4_2MB_Ass := 8
  else if (fL2_CodeTlb_4_2MB_Ass = 8) then fL2_CodeTlb_4_2MB_Ass := 16
  else if (fL2_CodeTlb_4_2MB_Ass = 10) then fL2_CodeTlb_4_2MB_Ass := 32; //for centaur c5j

  fL2_CodeTlb_4_2MB_Entries := ExtractBitValue( fx86Reg._eax, 0, 11 );

  // 4kB L2 TLB:
  fL2_DataTlb_4kB_Ass := ExtractBitValue( fx86Reg._ebx, 28, 31 );
  if (fL2_DataTlb_4kB_Ass = 6) then fL2_DataTlb_4kB_Ass := 8
  else if (fL2_DataTlb_4kB_Ass = 8) then fL2_DataTlb_4kB_Ass := 16
  else if (fL2_DataTlb_4kB_Ass = 10) then fL2_DataTlb_4kB_Ass := 32; //for centaur c5j

  fL2_DataTlb_4kB_Entries := ExtractBitValue( fx86Reg._ebx, 16, 27 );

  fL2_CodeTlb_4kB_Ass := ExtractBitValue( fx86Reg._ebx, 12, 15 );
  if (fL2_CodeTlb_4kB_Ass = 6) then fL2_CodeTlb_4kB_Ass := 8
  else if (fL2_CodeTlb_4kB_Ass = 8) then fL2_CodeTlb_4kB_Ass := 16
  else if (fL2_CodeTlb_4kB_Ass = 10) then fL2_CodeTlb_4kB_Ass := 32; //for centaur c5j

  fL2_CodeTlb_4kB_Entries := ExtractBitValue( fx86Reg._ebx, 0, 11 );
  // L2
  fL2_CacheSize := ExtractBitValue( fx86Reg._ecx, 16, 31 );
  fL2_CacheAss := ExtractBitValue( fx86Reg._ecx, 12, 15 );
  if (fL2_CacheAss = 6) then fL2_CacheAss := 8
  else if (fL2_CacheAss = 8) then fL2_CacheAss := 16
  else if (fL2_CacheAss = 10) then fL2_CacheAss := 32; //for centaur c5j
  fL2_CacheLinesPerTag := ExtractBitValue( fx86Reg._ecx, 8, 11 );
  fL2_CacheLinesSize := ExtractBitValue( fx86Reg._ecx, 0, 7 );
  
end; // procedure TCpuidL2Cache.Initialize;

function TCpuidL2Cache.GetResultsAsString : string;
var
  s : string;
begin
  result := '= Dump of CPUID L2 Info (Level 8000 0006h) =';
  result := result + DumpX86GPRegisters( fx86Reg );

  s := 'L2 Data TLB 4/2 MB associativity: ' + IntToStr( fL2_DataTlb_4_2MB_Ass );
  result := result + CRLF + s;
  s := 'L2 Data TLB 4/2 MB entries: ' + IntToStr( fL2_DataTlb_4_2MB_Entries );
  result := result + CRLF + s;
  s := 'L2 Code TLB 4/2 MB associativity: ' + IntToStr( fL2_CodeTlb_4_2MB_Ass );
  result := result + CRLF + s;
  s := 'L2 Code TLB 4/2 MB entries: ' + IntToStr( fL2_CodeTlb_4_2MB_Entries );
  result := result + CRLF + s;

  s := 'L2 Data TLB 4 kB associativity: ' + IntToStr( fL2_DataTlb_4kB_Ass );
  result := result + CRLF + s;
  s := 'L2 Data TLB 4 kB entries: ' + IntToStr( fL2_DataTlb_4kB_Entries );
  result := result + CRLF + s;
  s := 'L2 Code TLB 4 kB associativity: ' + IntToStr( fL2_CodeTlb_4kB_Ass );
  result := result + CRLF + s;
  s := 'L2 Code TLB 4 kB entries: ' + IntToStr( fL2_CodeTlb_4kB_Entries );
  result := result + CRLF + s;

  s := 'L2 cache size (kB): ' + IntToStr( fL2_CacheSize );
  result := result + CRLF + s;
  s := 'L2 cache associativity: ' + IntToStr( fL2_CacheAss );
  result := result + CRLF + s;
  s := 'L2 cache lines per tag: ' + IntToStr( fL2_CacheLinesPerTag );
  result := result + CRLF + s;
  s := 'L2 data cache lines size in bytes: ' + IntToStr( fL2_CacheLinesSize );
  result := result + CRLF + s;
end;//function TCpuidL2Cache.GetResultsAsString
//TCpuidL2Cache end...................................................
//==============================================================================

//==============================================================================
//TCpuidEnhancedPowerManagement begin...................................................
procedure TCpuidEnhancedPowerManagement.Initialize;
begin
  fEaxInput := $80000007;
  fx86Reg :=  CpuidExecute( fEaxInput );
  fSoftwareThermalControl := Bit5 in fx86Reg.bits_edx;
  fThermalMonitoring      := Bit4 in fx86Reg.bits_edx;
  fThermalTrip            := Bit3 in fx86Reg.bits_edx;
  fVoltageIDControl       := Bit2 in fx86Reg.bits_edx;
  fFrequencyIDControl     := Bit1 in fx86Reg.bits_edx;
  fTemperatureSensor      := Bit0 in fx86Reg.bits_edx;
end; // procedure TCpuidEnhancedPowerManagement.initialize;

function TCpuidEnhancedPowerManagement.GetResultsAsString : string;
var
  s : string;
begin
  result := '= Dump of CPUID Enhanced Power Management (8000 0007h) =';
  result := result + DumpX86GPRegisters( fx86Reg );

  s := BooleanToYesNo( fSoftwareThermalControl );
  result := result + CRLF + 'Software Thermal Control: ' + s;
  s := BooleanToYesNo( fThermalMonitoring );
  result := result + CRLF + 'Thermal Monitoring: ' + s;
  s := BooleanToYesNo( fThermalTrip );
  result := result + CRLF + 'Thermal Trip: ' + s;
  s := BooleanToYesNo( fVoltageIDControl );
  result := result + CRLF + 'Voltage ID Control: ' + s;
  s := BooleanToYesNo( fFrequencyIDControl );
  result := result + CRLF + 'Frequency ID Control: ' + s;
  s := BooleanToYesNo( fTemperatureSensor );
  result := result + CRLF + 'Temperature Sensor: ' + s;
end; // function TCpuidEnhancedPowerManagement.GetResultsAsString;
//TCpuidEnhancedPowerManagement end...................................................
//==============================================================================

//==============================================================================
//TCpuidAMD64AddressSize begin...................................................
// This CPUID call only works for AMD64 processors
procedure TCpuidAMD64AddressSize.Initialize;
begin
  fEaxInput := $80000008;
  fx86Reg :=  CpuidExecute( fEaxInput );
  fVirtualAddress       := ExtractBitValue( fx86Reg._eax, 8, 15 );
  fPhysicalAddressBits  := ExtractBitValue( fx86Reg._eax, 0, 7 );
end; // procedure TCpuidAMD64AddressSize.initialize;

function TCpuidAMD64AddressSize.GetResultsAsString : string;
var
  s : string;
begin
  result := '= Dump of CPUID Address Size ($' + IntToHex( fEaxInput, 8 ) + ') (AMD64 specific) =';
  result := result + DumpX86GPRegisters( fx86Reg );

  s := IntToStr( fVirtualAddress );
  result := result + CRLF + 'Virtual Address bits: ' + s;
  s := FloatToStrF( power( 2, fVirtualAddress ), ffNumber, 15, 0);
  result := result + CRLF + 'Virtual Address bytes: ' + s;
  s := IntToStr( fPhysicalAddressBits );
  result := result + CRLF + 'Physical Address bits: ' + s;
  s := FloatToStrF( power( 2, fPhysicalAddressBits ), ffNumber, 15, 0);
  result := result + CRLF + 'Physical Address bytes: ' + s;
end; // function TCpuidAddressSize.GetResultsAsString;
//TCpuidAMD64AddressSize end...................................................
//==============================================================================

//==============================================================================
//TCpuidCentaur begin...........................................................
procedure TCpuidCentaur.Initialize;
begin
  fEaxInput := $C0000000;
  fx86Reg :=  CpuidExecute( fEaxInput );
  fMaxSupportedLevel := fx86Reg._eax;
end; // procedure TCpuidCentaur.initialize;

function TCpuidCentaur.GetResultsAsString : string;
var
  s : string;
begin
  result := '= Dump of CPUID Centaur (C000 0000h) =';
  result := result + DumpX86GPRegisters( fx86Reg );

  s := IntToHex( fMaxSupportedLevel, 8 );
  result := result + CRLF + 'Maximum Supported level (hex): ' + s;
end; // function TCpuidCentaur.GetResultsAsString;
//TCpuidCentaur end...................................................
//==============================================================================

//==============================================================================
//TCpuidCentaurInfo begin.......................................................
procedure TCpuidCentaurInfo.Initialize;
begin
  fEaxInput := $C0000001;
  fx86Reg :=  CpuidExecute( fEaxInput );
  fAdvancedCryptographyEngineEnabled    := Bit7 in fx86Reg.bits_edx;
  fAdvancedCryptographyEngine           := Bit6 in fx86Reg.bits_edx;
  fFEMMS                                := Bit5 in fx86Reg.bits_edx;
  fLongHaul                             := Bit4 in fx86Reg.bits_edx;
  fRandomNumberGeneratorEnabled         := Bit3 in fx86Reg.bits_edx;
  fRandomNumberGenerator                := Bit2 in fx86Reg.bits_edx;
  fAlternateInstructionSetEnabled       := Bit1 in fx86Reg.bits_edx;
  fAlternateInstructionSet              := Bit0 in fx86Reg.bits_edx;
  fAdvancedCryptographyEngine2          := Bit8 in fx86Reg.bits_edx;
  fAdvancedCryptographyEngine2Enabled   := Bit9 in fx86Reg.bits_edx;
  fPadlockHashEngine                    := Bit10 in fx86Reg.bits_edx;
  fPadlockHashEngineEnabled             := Bit11 in fx86Reg.bits_edx;
  fPadlockMontgomeryMultiplier          := Bit12 in fx86Reg.bits_edx;
  fPadlockMontgomeryMultiplierEnabled   := Bit13 in fx86Reg.bits_edx;
end; // procedure TCpuidCentaurInfo.initialize;

function TCpuidCentaurInfo.GetResultsAsString : string;
var
  s : string;
begin
  result := '= Dump of CPUID Centaur Info (C000 0001h) =';
  result := result + DumpX86GPRegisters( fx86Reg );

  s := BooleanToYesNo( fRandomNumberGenerator );
  result := result + CRLF + 'Random Number Generator: ' + s;
  s := BooleanToYesNo( fRandomNumberGeneratorEnabled );
  result := result + CRLF + 'Random Number Generator Enabled: ' + s;
  s := BooleanToYesNo( fAdvancedCryptographyEngine );
  result := result + CRLF + 'Advanced Cryptography Engine: ' + s;
  s := BooleanToYesNo( fAdvancedCryptographyEngineEnabled );
  result := result + CRLF + 'Advanced Cryptography Engine enabled: ' + s;
  s := BooleanToYesNo( fAdvancedCryptographyEngine2 );
  result := result + CRLF + 'Advanced Cryptography Engine2: ' + s;
  s := BooleanToYesNo( fAdvancedCryptographyEngine2Enabled );
  result := result + CRLF + 'Advanced Cryptography Engine2 enabled: ' + s;
  s := BooleanToYesNo( fPadlockHashEngine );
  result := result + CRLF + 'Padlock Hash Engine: ' + s;
  s := BooleanToYesNo( fPadlockHashEngineEnabled );
  result := result + CRLF + 'Padlock Hash Engine Enabled: ' + s;
  s := BooleanToYesNo( fPadlockMontgomeryMultiplier );
  result := result + CRLF + 'Padlock Montgomery Multiplier: ' + s;
  s := BooleanToYesNo( fPadlockMontgomeryMultiplierEnabled );
  result := result + CRLF + 'Padlock Montgomery Multiplier Enabled: ' + s;
  s := BooleanToYesNo( fFEMMS );
  result := result + CRLF + 'FEMMS: ' + s;
  s := BooleanToYesNo( fLongHaul );
  result := result + CRLF + 'Long Haul: ' + s;
  s := BooleanToYesNo( fAlternateInstructionSetEnabled );
  result := result + CRLF + 'Alternate Instruction Set Enabled: ' + s;
  s := BooleanToYesNo( fAlternateInstructionSet );
  result := result + CRLF + 'Alternate Instruction Set: ' + s;
end; // function TCpuidCentaurInfo.GetResultsAsString;
//TCpuidCentaurInfo end...................................................
//==============================================================================

//==============================================================================
//TCpuidCentaurPerformanceInfo begin.......................................................

  procedure TCpuidCentaurPerformanceInfo.Initialize;
  var
    liFSB : integer;
  begin
    fEaxInput := $C0000002;
    fx86Reg :=  CpuidExecute( fEaxInput );
    fCurrentTemperature := fx86Reg._eax / 256;
    fCurrentMilliVolts := ( ExtractBitValue( fx86Reg._ebx, 0, 7 ) * 16 ) + 700;
    fCurrentClockMultiplier := ExtractBitValue( fx86Reg._ebx, 8, 15 );
    fLowestClockRatio := ExtractBitValue( fx86Reg._ebx, 24, 31 ) ;
    fHighestMilliVoltageEncoding :=  ( ExtractBitValue( fx86Reg._ecx, 0, 7 ) * 16 ) + 700;
    fHighestClockMultiplier := ExtractBitValue( fx86Reg._ecx, 8, 15 );
    fLowestVoltageEncoding := ExtractBitValue( fx86Reg._ecx, 16, 23 );
    fLowestClockMultiplier := ExtractBitValue( fx86Reg._ecx, 24, 31 );
    liFSB := ExtractBitValue( fx86Reg._edx, 18, 19);
    case liFSB of
      0: fFrontSideBusClock := 100;
      1: fFrontSideBusClock := 133.33333333333333333333;
      2: fFrontSideBusClock := 166.66666666666666666666;
      3: fFrontSideBusClock := 200;
    else
      fFrontSideBusClock := -1;
    end; // case
    fCurrentClockMultiplierEDX := ExtractBitValue( fx86Reg._edx, 22, 26 );
  end; // procedure TCpuidCentaurPerformanceInfo.initialize;

  function TCpuidCentaurPerformanceInfo.GetResultsAsString : string;
  var
    s : string;
  begin
    result := '= Dump of CPUID Centaur Performance Info (C000 0002h) =';
    result := result + DumpX86GPRegisters( fx86Reg );

    s := FloatToStr( fCurrentTemperature );
    result := result + CRLF + 'Current CPU Temperature: ' + s;
    s := IntToStr( fCurrentMilliVolts );
    result := result + CRLF + 'Current Vcc (milliVolts): ' + s;
    s := IntToStr( fCurrentClockMultiplier );
    result := result + CRLF + 'Current CPU Clock Multiplier: ' + s;
     s := IntToStr( fLowestClockRatio );
    result := result + CRLF + 'Lowest CPU Clock Ratio: ' + s;
    s := IntToStr( fHighestMilliVoltageEncoding );
    result := result + CRLF + 'Highest CPU mV Encoding: ' + s;
    s := IntToStr( fHighestClockMultiplier );
    result := result + CRLF + 'Highest CPU Clock Multiplier: ' + s;
    s := IntToStr( fLowestVoltageEncoding );
    result := result + CRLF + 'Lowest CPU Voltage Encoding: ' + s;
    s := IntToStr( fLowestClockMultiplier );
    result := result + CRLF + 'Lowest CPU Clock Multiplier: ' + s;
    s := FloatToStr( fFrontSideBusClock );
    result := result + CRLF + 'Frontside Bus Clock (MHz): ' + s;
    s := IntToStr( fCurrentClockMultiplierEDX );
    result := result + CRLF + 'Current Clock Multiplier (EDX): ' + s;
  end; // function TCpuidCentaurPerformanceInfo.GetResultsAsString;

//TCpuidCentaurPerformanceInfo end...................................................
//==============================================================================



//==============================================================================
//TCpuidTransmeta begin...........................................................
procedure TCpuidTransmeta.Initialize;
begin
  fEaxInput := $80860000;
  fx86Reg :=  CpuidExecute( fEaxInput );
  fMaxSupportedLevel := fx86Reg._eax;
  fVendorIDString := fx86Reg.str_ebx + fx86Reg.str_edx + fx86Reg.str_ecx;
end; // procedure TCpuidTransmeta.initialize;

function TCpuidTransmeta.GetResultsAsString : string;
var
  s : string;
begin
  result := '= Dump of CPUID Transmeta (8086 0000h) =';
  result := result + DumpX86GPRegisters( fx86Reg );

  s := IntToHex( fMaxSupportedLevel, 8 );
  result := result + CRLF + 'Maximum Supported level (hex): ' + s;
  s := fVendorIDString;
  result := result + CRLF + 'Vendor ID string: ' + s;
end; // function TCpuidTransmeta.GetResultsAsString;
//TCpuidTransmeta end...................................................
//==============================================================================

//==============================================================================
//TCpuidTransmeta1 begin...........................................................
procedure TCpuidTransmeta1.Initialize;
begin
  fEaxInput := $80860001;
  fx86Reg :=  CpuidExecute( fEaxInput );

  fFamily := ExtractBitValue( fx86Reg._eax, 8, 11 );
  fModel := ExtractBitValue( fx86Reg._eax, 4, 7 );
  fStepping := ExtractBitValue( fx86Reg._eax, 0, 3 );
  if fx86Reg._ebx = $20000000 then begin
    fHardwareRev := 'See level 8086_002h instead';
  end else begin
    fHardwareRevA := ExtractBitValue( fx86Reg._ebx, 24, 31 );
    fHardwareRevB := ExtractBitValue( fx86Reg._ebx, 16, 23 );
    fHardwareRevC := ExtractBitValue( fx86Reg._ebx, 8, 15 );
    fHardwareRevD := ExtractBitValue( fx86Reg._ebx, 0, 7 );
    fHardwareRev := IntToStr( fHardwareRevA ) + '-' +
                    IntToStr( fHardwareRevB ) + '-' +
                    IntToStr( fHardwareRevC ) + '-' +
                    IntToStr( fHardwareRevD );
  end; // if
  fNominalMHz := fx86Reg._ecx;
  fLongRunTableInt := Bit3 in fx86Reg.bits_edx;
  fUnknown := Bit2 in fx86Reg.bits_edx;
  fLongRun := Bit1 in fx86Reg.bits_edx;
  fRecoveryCMSActive := Bit0 in fx86Reg.bits_edx;
end; // procedure TCpuidTransmeta1.initialize;

function TCpuidTransmeta1.GetResultsAsString : string;
var
  s : string;
begin
  result := '= Dump of CPUID Transmeta1 (8086 0001h) =';
  result := result + DumpX86GPRegisters( fx86Reg );
  s := IntToStr( fFamily );
  result := result + CRLF + 'Family: ' + s;
  s := IntToStr( fModel );
  result := result + CRLF + 'Model: ' + s;
  s := IntToStr( fStepping );
  result := result + CRLF + 'Stepping: ' + s;
  result := result + CRLF + 'Hardware Revision: ' + fHardwareRev;
  s := intToStr( fNominalMHz );
  result := result + CRLF + 'Nominal MHz: ' + s;
  result := result + CRLF + 'LongRun Table Interface: ' + BooleanToYesNo( fLongRunTableInt );
  result := result + CRLF + 'Unknown: ' + BooleanToYesNo( fUnknown );
  result := result + CRLF + 'LongRun: ' + BooleanToYesNo( fLongRun );
  result := result + CRLF + 'Recovery CMS Active (BAD): ' + BooleanToYesNo( fRecoveryCMSActive );
end; // function TCpuidTransmeta1.GetResultsAsString;
//TCpuidTransmeta1 end...................................................
//==============================================================================

//TCpuidTransmetaInfoString begins..............................................
//==============================================================================
procedure TCpuidTransmetaInfoString.Initialize;
begin
  fEaxInput := $80860003;
  fx86Reg  := CpuidExecute( fEaxInput );
  fx86Reg1 := CpuidExecute( fEaxInput + 1 );
  fx86Reg2 := CpuidExecute( fEaxInput + 2 );
  fx86Reg3 := CpuidExecute( fEaxInput + 3 );
  fTransmetaInfoString := trim( fx86Reg.str + fx86Reg1.str + fx86Reg2.str + fx86Reg3.str );
end; //TCpuidTransmetaInfoString.Initialize

function TCpuidTransmetaInfoString.GetResultsAsString : string;
begin
  result := '= Dump of CPUID Transmeta Info String =';
  result := result + CRLF + 'Call 0:';
  result := result + DumpX86GPRegisters( fx86Reg );
  result := result + CRLF + 'Call 1:';
  result := result + DumpX86GPRegisters( fx86Reg1 );
  result := result + CRLF + 'Call 2:';
  result := result + DumpX86GPRegisters( fx86Reg2 );
  result := result + CRLF + 'Call 3:';
  result := result + DumpX86GPRegisters( fx86Reg3 );
  result := result + CRLF + 'Transmeta info string: ' + fTransmetaInfoString;
end; // function TCpuidTransmetaInfoString.GetResultsAsString
//TCpuidTransmetaInfoString ends................................................
//==============================================================================

//==============================================================================
//TCpuidTransmetaPerformanceData begin..........................................
procedure TCpuidTransmetaPerformanceData.Initialize;
begin
  fEaxInput := $80860007;
  fx86Reg :=  CpuidExecute( fEaxInput );
  fCpuMHz := fx86Reg._eax;
  fCpuMilliVoltLevel := fx86Reg._ebx;
  fLongRunPerformancePercentage := fx86Reg._ecx;
  fCurrentGateDelayInFemtoSeconds := fx86Reg._edx;
end; // procedure TCpuidTransmetaPerformanceData.initialize;

function TCpuidTransmetaPerformanceData.GetResultsAsString : string;
var
  s : string;
begin
  result := '= Dump of CPUID Transmeta Performance Data (8086 0007h) =';
  result := result + DumpX86GPRegisters( fx86Reg );

  s := IntToStr( fCpuMHz );
  result := result + CRLF + 'Current Cpu MHz: ' + s;
  s := IntToStr( fCpuMilliVoltLevel );
  result := result + CRLF + 'Cpu Core Voltage (mV): ' + s;
  s := IntToStr( fLongRunPerformancePercentage );
  result := result + CRLF + 'LongRun Performance Percentage: ' + s;
  s := IntToStr( fCurrentGateDelayInFemtoSeconds );
  result := result + CRLF + 'Current Gate Delay (fs): ' + s;
end; // function TCpuidTransmetaPerformanceData.GetResultsAsString;
//TCpuidTransmetaPerformanceData end...................................................
//==============================================================================

end.
