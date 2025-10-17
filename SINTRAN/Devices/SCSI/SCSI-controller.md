## C# implementation of controller

Located in E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusDiscControllerSCSI.cs

## SCSI Interface IOX adddresses

internal enum Register
{

	/// <summary>
	/// READ MEMORY ADDRESS REGISTER BITS 0-15
	/// </summary>
	RLMAR = 00,

	/// <summary>
	///  WRITE MEMORY ADDRESS REGISTER BITS 0-15
	/// </summary>
	WLMAR = 01,

	/// <summary>
	/// READ DATA 
	/// These commands are only used in IOX modus, not in DMA modus. 16 bits are used.
	/// </summary>
	REDAT = 02,

	/// <summary>
	/// WRITE DATA 
	/// These commands are only used in IOX modus, not in DMA modus. 16 bits are used.
	/// </summary>
	WRDAT = 03,

	/// <summary>
	/// READ STATUS
	/// </summary>
	RSTAU = 04,

	/// <summary>
	/// WRITE CONTROL
	/// </summary>
	WCONT = 05,

	/// <summary>
	/// READ MEMORY ADDRESS REGISTER BITS 16-23
	/// </summary>
	RHMAR = 06,

	/// <summary>
	/// WRITE MEMORY ADDRESS REGISTER BITS 16-23
	/// </summary>
	WHMAR = 07,

	/// <summary>
	/// Read extemal wordcount bits 16-23
	/// 
	/// This IOX is only used with the ND-3204 controller.
	/// The External Wordcount consists of 24 bits, so it has to be read in two operations.
	/// </summary>
	RXWC_HI = 0x08,


	// 0x9 - NOT USED - WRITE
	NOT_USED_09 = 0x09,

	/// <summary>
	/// Read extemal wordcount bits 0-15
	/// 
	/// This IOX is only used with the ND-3204 controller.
	/// </summary>
	RXWC = 0x0A,

	// 0xB-0x1F - NOT USED

	/// <summary>
	/// READ NCR DATA REGISTER o40
	///
	/// The Data Register is used to transfer SCSI commands, data, status and message bytes between ND-100 and the SCSI bus.
	/// This is an 8-bit register, which is doubly buffered in order to support maximum throughput.
	/// </summary>
	RNDAT = 0x20,

	/// <summary>
	///  WRITE NCR DATA REGISTER  041
	/// </summary>
	WNDAT = 0x21,

	/// <summary>
	/// READ NCR COMMAND REGISTER o42
	///
	/// The Command Register is an 8 bit register used to give commands to then SCSI chip. These commands are described in section 3.3.
	/// Writing to the Command Register causes the chip to execute the comand that is written.
	/// The Command Register can be read, but the chip resets the Command Register when it sets an interrupt
	/// Therefore, you cannot quarentee that the data in the register will be correct after loading an interrupting command or enabling Selection or Reselection.
	/// </summary>
	RNCOM = 0x22,

	/// <summary>
	/// WRITE NCR COMMAND REGISTER o43
	/// </summary>
	WNCOM = 0x23,

	/// <summary>
	///  READ NCR CONTROL REGISTER o44
	///
	/// This 8-bit register is used to enable certain modes of operation for the SCSI Protocol Controller.
	/// Bits
	///		0 - Select enable
	///		1 - Reselect enable
	///	  	2 - Parity enable
	///  	3-7 - Not used
	///
	/// After being Reset and completing selfdiagnostics, the control register will contain all zeros
	/// </summary>
	RNCNT = 0x24,

	/// <summary>
	///  WRITE NCR CONTROL REGISTER o45
	/// </summary>
	WNCNT = 0x25,

	/// <summary>
	///  READ DESTINATION ID REGISTER o46
	/// </summary>
	RDESI = 0x26,

	/// <summary>
	/// WRITE DESTINATION ID REGISTER o47
	/// </summary>
	WDESI = 0x27,

	/// <summary>
	/// READ AUXILIARY STATUS o50
	/// </summary>
	RAUXS = 0x28,

	/// <summary>
	/// WRITE AUXILIARY STATUS o51 
	/// </summary> 
	WAUXS = 0x29,

	/// <summary>
	/// READ OWN ID NUMBER o52
	/// </summary>
	ROIDN = 0x2A,

	/// <summary>
	/// WRITE OWN ID NUMBER o53
	///
	/// The ID Register operates in two configurations, the "strapped ID" mode or the "programmed ID" mode.
	///
	/// If the ID Register is written before the Control Register is written, the NCR 5386 assumes the "programmed ID" mode.
	///
	/// In the “progranmed ID" mode, pin 14 becomes the data parity signal D(P) and pins 12 and 13 are not used.
	/// If the Control Register is initialized and the ID register has not been written previously,
	/// then the device will assume the "strapped ID" mode which is identical to the NCR 5385E operation.
	///
	/// Before ID register or Control Register is written, the chip will be in an unknown mode.
	/// </summary>
	WOIDN = 0x2B,

	/// <summary>
	/// READ INTERRUPT REGISTER o54
	/// </summary> 
	RITRG = 0x2c,

	//o55  0x2D - NOT USED
	NOT_USED_2D = 0x2D,

	/// <summary>
	/// READ SOURCE ID o56
	/// </summary>
	RSOUI = 0x2E,


	// 057-61 0x2F-0x31 NOT USED


	/// <summary>
	/// READ DIAGNOSTIC STATUS o62
	/// </summary>
	RDIST = 0x32,

	/// <summary>
	/// READ TRANSFER COUNTER MSB o70
	/// </summary>
	RTCM = 0x38,

	/// <summary>
	/// WRITE TRANSFER COUNTER MSB o71
	/// </summary>
	WTCM = 0x39,

	/// <summary>
	/// READ TRANSFER COUNTER 2ND. o72
	/// </summary>
	RTC2 = 0x3A,

	/// <summary>
	/// WRITE TRANSFER COUNTER 2ND. o73
	/// </summary>
	WTC2 = 0x3B,

	/// <summary>
	/// READ TRANSFER COUNTER LEAST o74
	/// </summary>
	RTCL = 0x3C,

	/// <summary>
	/// WRITE TRANSFER COUNTER LEAST o75
	/// </summary>
	WTCL = 0x3D,


	// o76-077 0x3E-0x3F NOT USED
}


## Register details

### WCONT - Write CONTROL WORD (on 3201 controller) 

		Bits
			O Enable Interrupt (to control word bit 0)
			1 not used
			2 Activate
			3 Test
			4 Clear Device
			5 ND-100 DMA enable
			6 Write ND-100 Memory
			7 not used
			8 not used, must be zero
			9 not used, must be zero
			10 Reset SCSI to bus
			11-15 not used

### RSTAU - Read status

		Bits
		0	Enabled Interrupt (from control word bit 0)
		1	not used
		2	Busy (Active)
		3	Ready for transfer
		4	Or of errors (only bit 11)
		5	Reset on SCSI bus					(*)
		6	NCR 5386 disabled
		7	Single ended SCSI driver selected
		8	Data request from NCR 5386
		9	Interrupt from NCR 5386			(*)
		10	Data acknowledge to NCR 5386
		11	BERROR (ND-100 Bus DMA error)	(x)  <= Will never happen in the emulator
		12	BSY from the SCSI bus.
		13	REQ from the SCSI bus
		14	ACK from the SCSI bus
		15	Differential SCSI receivers selected


		Unused bits are set to zero.
		(*) Gives Interrupt to ND-100 if bit O (Enable Interrupt) is set.        


## Thumbweel settings and memory IOX base address and identcode

switch (thumbwheel)
{
	case 0:
	case 0x04:
	case 0x08:
	case 0x0c:
		// Logical device 2202
		base.NDBusAddressBase = Numeric.ParseIntValue("144300");
		base.IdentCode = (ushort)Numeric.ParseIntValue("140440");
		base.SetMemoryName($"SCSI Bus 1. TW2=0x{thumbwheel:x2}");
		break;

	case 1:
	case 5:
	case 9:
	case 0x0D:
		// Logical device 2203
		base.NDBusAddressBase = Numeric.ParseIntValue("144400");
		base.IdentCode = (ushort)Numeric.ParseIntValue("140441");
		base.SetMemoryName($"SCSI Bus 2. TW2=0x{thumbwheel:x2}");
		break;

	case 2:
	case 6:
	case 0x0A:
	case 0x0E:
		// Logical device 2204
		base.NDBusAddressBase = Numeric.ParseIntValue("144500");
		base.IdentCode = (ushort)Numeric.ParseIntValue("140442");
		base.SetMemoryName($"SCSI Bus 3. TW2=0x{thumbwheel:x2}");
		break;

	case 3:
	case 7:
	case 0x0B:
	case 0x0F:
		// Logical device 2205
		base.NDBusAddressBase = Numeric.ParseIntValue("144600");
		base.IdentCode = (ushort)Numeric.ParseIntValue("140443");
		base.SetMemoryName($"SCSI Bus 4. TW2=0x{thumbwheel:x2}");
		break;


## SCSI Bus details
			base.NDBusAddressLength = 63;
			base.InterruptLevel = 11; // Output channel interrupt = 11 (disk)
