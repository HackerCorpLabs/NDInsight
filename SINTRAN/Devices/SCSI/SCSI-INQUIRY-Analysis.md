# SCSI INQUIRY Command Analysis - SINTRAN III Limitations

## Summary

**SINTRAN III has NO vendor-specific restrictions for SCSI devices.** The operating system only validates the **Device Type** field from the INQUIRY response. Vendor ID and Product ID fields are **completely ignored** by the driver.

---

## INQUIRY Command Usage in SINTRAN

### When INQUIRY is Executed

INQUIRY (Function 42 or 36 in the driver API) is automatically performed when:

1. **First access to an uninitialized LUN** - Flag bit `5SCIN` (bit 5 = SCSI INitialized) is checked
2. **After UNIT ATTENTION with Sense Key 6** - Forces re-initialization
3. **Explicit INQUIRY request** from user applications (Function 75)

**Source:** `IP-P2-SCSI-DISK.NPL:1124` and `IP-P2-SCSI-DISK.NPL:1222`

```npl
IF A=42 OR =36 GO FAR INQUI    % AUTOMATIC INQUIRY (line 1124)
```

```npl
CACOB: A:=L=:"HOME"
       IF SUTYP NBIT 5SCIN GO FAR INQUI     % START INQUIRY (line 3354)
```

---

## INQUIRY Response Processing

### Step 1: Send INQUIRY Command

**Location:** `IP-P2-SCSI-DISK.NPL:1222-1227`

```npl
INQUI: SUTYP/\SMASK\/77400=:SUTYP           % ZERO OLD STATUS
       T:=SMBP1; X:=SMBP2                   % PHYSICAL ADDRESS OF COMMAND BUFFER
       11000; *STATX 00; STZTX 10           % COMMAND (0x12 = INQUIRY)
       "SINBL" SHZ 11; *STATX 20            % ALLOCATION LENGTH
       *STZTX 30; STZTX 40; STZTX 50
       CALL EXINT; CALL SCEIO; CALL ERRFATAL; X:=:B
```

### Step 2: Parse Device Type (Byte 0)

**Location:** `IP-P2-SCSI-DISK.NPL:1230-1242`

```npl
T:=SMBP1; X:=SMBP2+SINBS; *LDATX 00
A\/377/\SUTYP=:SUTYP                 % UPDATE DEVICE TYPE INFORMATION
IF A SHZ -10=0 OR =3 OR =4 THEN
   SUTYP BONE 5SCDA=:SUTYP           % DIRECT ACCESS DEVICE
   22400                             % READ CAPACITY
ELSE
IF A=1 THEN
   2400                              % READ BLOCK SIZE (Sequential)
ELSE
IF A=177 THEN
   T:=NOLUN; GO FAR ERREX            % NO SUCH LUN (0x7F)
ELSE
   SUSI3+1=:SUSI1; GO FAR INFIN      % SHIFT INSTRUCTIONS (other types)
```

**Device Type Values Accepted:**

| Value (Octal) | Value (Hex) | Device Type | SINTRAN Action |
|---------------|-------------|-------------|----------------|
| 0 | 0x00 | Direct Access (Disk) | Marks as Direct Access, reads capacity |
| 1 | 0x01 | Sequential Access (Tape) | Reads block size |
| 3 | 0x03 | Printer | Marks as Direct Access |
| 4 | 0x04 | Write Once (Optical) | Marks as Direct Access, reads capacity |
| 177 | 0x7F | No Device Present | Returns error "NO SUCH LUN" |
| Other | Other | Unknown | Continues with default settings |

### Step 3: **NO VENDOR VALIDATION**

**Critical Finding:** The SINTRAN SCSI driver **DOES NOT READ** or **VALIDATE** the following INQUIRY fields:

- **Byte 8-15:** Vendor Identification (8 ASCII bytes)
- **Byte 16-31:** Product Identification (16 ASCII bytes)
- **Byte 32-35:** Product Revision Level (4 ASCII bytes)

**Evidence:** Complete analysis of `INQUI` routine (lines 1222-1289) shows only byte 0 is read for device classification.

---

## Vendor Compatibility

### Confirmed Working Devices

From `scsi_inquiry_explained.md` and historical documentation:

| Vendor | Product | Device Type | Works? |
|--------|---------|-------------|--------|
| NDMICROP | 1375 | Direct Access | Yes |
| TANDBERG | TDC 3600 | Sequential (streamer) | Yes |
| OSI | LD 1200 SCSI | Write Once (optical) | Yes |
| NDCDC | EMD 97201 (736) | Direct Access | Yes |
| NDCDC | EMD 97201 (368) | Direct Access | Yes |
| NDSTK | 2925 | Sequential (magtape) | Yes |
| ARCHIVE | VIPER 150 21247 | Sequential (streamer) | Yes |

### ANY Vendor Will Work

**Conclusion:** SINTRAN will accept **ANY SCSI device** from **ANY vendor** as long as:

1. Device responds to INQUIRY command (0x12)
2. Byte 0 (Peripheral Device Type) contains a valid value:
   - 0x00, 0x03, or 0x04 for disk-like devices
   - 0x01 for tape-like devices
3. Device implements standard SCSI-1 or SCSI-2 command set

**Modern emulators can use ANY vendor string** - SINTRAN ignores it completely.

---

## Implementation Recommendations for Emulators

### C# Emulator Implementation

When implementing a SCSI disk/tape emulator for SINTRAN, the INQUIRY response should follow this format:

```csharp
byte[] InquiryResponse = new byte[36];

// Byte 0: Peripheral Device Type
InquiryResponse[0] = 0x00;  // 0x00 = Direct Access Device
                             // 0x01 = Sequential Access Device (Tape)
                             // 0x04 = Write Once Device (Optical)

// Byte 1: Removable Media Bit (RMB)
InquiryResponse[1] = 0x00;  // 0x00 = Non-removable, 0x80 = Removable

// Byte 2: ANSI Version
InquiryResponse[2] = 0x01;  // SCSI-1 compliance

// Byte 3: Response Data Format
InquiryResponse[3] = 0x01;  // Standard INQUIRY response format

// Byte 4: Additional Length
InquiryResponse[4] = 31;    // Additional bytes (36 total - 5 header)

// Bytes 5-7: Reserved
InquiryResponse[5] = 0x00;
InquiryResponse[6] = 0x00;
InquiryResponse[7] = 0x00;

// Bytes 8-15: Vendor Identification (8 ASCII bytes, space-padded)
// SINTRAN IGNORES THIS - use any value
string vendor = "NDMICROP";  // Or "EMULATOR", "GENERIC", anything!
Array.Copy(Encoding.ASCII.GetBytes(vendor.PadRight(8)), 0, InquiryResponse, 8, 8);

// Bytes 16-31: Product Identification (16 ASCII bytes, space-padded)
// SINTRAN IGNORES THIS - use any value
string product = "Virtual Disk    ";
Array.Copy(Encoding.ASCII.GetBytes(product.PadRight(16)), 0, InquiryResponse, 16, 16);

// Bytes 32-35: Product Revision Level (4 ASCII bytes)
// SINTRAN IGNORES THIS - use any value
string revision = "1.00";
Array.Copy(Encoding.ASCII.GetBytes(revision), 0, InquiryResponse, 32, 4);
```

### Recommended Values for Authenticity

For historical accuracy (optional - SINTRAN doesn't care):

**For Hard Disks:**
- Vendor: `"NDMICROP"` (Norsk Data Micropolis)
- Product: `"1375            "` (1.3GB SCSI disk)
- Revision: `"1.00"`

**For Tape Streamers:**
- Vendor: `"TANDBERG"` (Norwegian tape manufacturer)
- Product: `"TDC 3600        "` (QIC tape streamer)
- Revision: `"1.00"`

**For Optical Disks:**
- Vendor: `"OSI     "` (Optical Storage International)
- Product: `"LD 1200 SCSI    "` (Write-once optical)
- Revision: `"1.00"`

---

## Critical Discovery: No Vendor Whitelisting

**Unlike some operating systems** (e.g., certain proprietary Unix systems that check for specific vendor strings), **SINTRAN has no vendor whitelist or blacklist.**

This means:
- ✅ Modern SATA-to-SCSI bridges work
- ✅ Generic SCSI emulators work
- ✅ Virtual SCSI devices work
- ✅ Any SCSI device that implements the basic command set works

The only requirement is **correct SCSI command implementation**, not vendor identity.

---

## INQUIRY-Related Error Codes

| Error Code (Octal) | Error Name | Trigger Condition |
|--------------------|------------|-------------------|
| NOLUN | No Such LUN | Device Type = 0x7F (No device present) |
| TYPER | Type Error | Device Type doesn't match expected type (checked later during operations) |
| ILSEN | Illegal Sense | Sense data format incorrect after INQUIRY failure |
| RQSER | Request Sense Error | Cannot retrieve sense after INQUIRY failure |

**Source:** `IP-P2-SCSI-DISK.NPL:1240-1242`, `1207`

---

## Testing Checklist for Emulators

When implementing SCSI device emulation for SINTRAN:

### Minimum Requirements
- [ ] Respond to INQUIRY command (0x12) with 36-byte response
- [ ] Set byte 0 to correct device type (0x00 for disk, 0x01 for tape)
- [ ] Return CHECK CONDITION status if LUN doesn't exist (sets Device Type = 0x7F in sense)
- [ ] Implement READ CAPACITY for disk devices
- [ ] Implement READ/WRITE commands appropriate for device type

### Optional (SINTRAN Doesn't Check)
- [ ] Set Vendor ID (bytes 8-15)
- [ ] Set Product ID (bytes 16-31)
- [ ] Set Revision (bytes 32-35)

### Advanced Testing
- [ ] Test with INQUIRY returning device type 0x00 → Should work as disk
- [ ] Test with INQUIRY returning device type 0x01 → Should work as tape
- [ ] Test with INQUIRY returning device type 0x7F → Should return "NO SUCH LUN" error
- [ ] Test with INQUIRY returning device type 0x04 → Should work as optical disk
- [ ] Test changing Vendor ID to "GENERIC" → Should still work (SINTRAN ignores it)

---

## Related Source Code Files

| File | Purpose |
|------|---------|
| `IP-P2-SCSI-DISK.NPL` | Main SCSI disk driver with INQUIRY handling |
| `IP-P2-SCSI-DRIV.NPL` | Low-level SCSI protocol driver |
| `scsi_inquiry_explained.md` | INQUIRY command format reference |
| `SCSI-controller.md` | Hardware interface documentation |

---

## Conclusion

SINTRAN III is **vendor-agnostic** for SCSI devices. The operating system performs **zero validation** of vendor strings, making it **highly compatible** with modern SCSI emulators, virtual devices, and generic SCSI hardware.

The **only hard requirement** is implementing the correct **SCSI command set** for the declared **device type**.

---

**Document Path:** `Z:\NorskData\Source Code\Sintran L\NPL\SCSI-INQUIRY-Analysis.md`

**Analysis Date:** 2025-10-13
**Analyzer:** Based on SINTRAN III NPL source code analysis
