# SCSI INQUIRY Command Explained

The **INQUIRY** command provides essential identification information from a SCSI device, including Vendor, Product, and Device Type. The data in the sample table below originates from this command.

```
Vendor   Product          Device type
-------- ---------------- ------------------------------
NDMICROP 1375             Direct
TANDBERG TDC 3600         Sequential (streamer)
OSI      LD 1200 SCSI     Write Once (optical disk)
NDCDC    EMD 97201 (736)  Direct
NDCDC    EMD 97201 (368)  Direct
NDSTK    2925             Sequential (magtape)
ARCHIVE  VIPER 150 21247  Sequential (streamer)
```

---

## INQUIRY (Opcode 0x12)

### Purpose
The **INQUIRY** command allows a host (initiator) to retrieve identification data from a target device. Returned information includes:

- Peripheral Device Type (e.g., Direct Access, Sequential Access)
- Vendor Identification
- Product Identification
- Product Revision Level

---

## Command Descriptor Block (CDB)

| Byte | Bits | Description | Example |
|------|------|--------------|----------|
| 0 | 7:0 | Operation Code | `0x12` |
| 1 | 7 | EVPD (Enable Vital Product Data) | `0` for standard inquiry |
| 2 | 7:0 | Page Code (ignored if EVPD=0) | `0x00` |
| 3–4 | 15:0 | Allocation Length (bytes to return) | `0x0024` (36 bytes typical) |
| 5 | 7:0 | Control | `0x00` |

**Example CDB (hex):**
```
12 00 00 00 24 00
```

---

## Standard INQUIRY Data Format

| Offset | Length | Field | Description | Example |
|---------|---------|--------|--------------|----------|
| 0 | 1 | Peripheral Device Type | 0x00 = Direct Access, 0x01 = Sequential Access, etc. | 0x00 |
| 8 | 8 | Vendor Identification | ASCII string | `NDMICROP` |
| 16 | 16 | Product Identification | ASCII string | `1375` |
| 32 | 4 | Product Revision Level | ASCII string | `1.00` |

These fields correspond directly to the data presented in the summary table.

---

## Device Type Values

| Code | Meaning | Example |
|------|----------|----------|
| 00h | Direct Access (Disk) | `NDMICROP 1375` |
| 01h | Sequential Access (Tape/Streamer) | `TANDBERG TDC 3600` |
| 04h | Write Once (Optical Disk) | `OSI LD 1200 SCSI` |
| 05h | CD-ROM | – |
| 07h | Optical Memory | – |
| 08h | Medium Changer | – |
| 1Fh | Unknown/No device | – |

---

## Example Command Usage (Linux)

Using the **sg_inq** utility from the `sg3_utils` package:

```bash
sg_inq /dev/sda
```

**Output Example:**
```
standard INQUIRY:
  PQual=0  Device_type=0  RMB=0  version=0x05  [SPC-3]
  [Vendor identification: NDMICROP]
  [Product identification: 1375]
  [Revision level: 1.00]
```

Which corresponds directly to:
```
Vendor   Product          Device type
-------- ---------------- ------------------------------
NDMICROP 1375             Direct
```

---

## Summary Table

| Information | Source | SCSI Command |
|--------------|---------|---------------|
| Vendor ID | Standard INQUIRY bytes 8–15 | INQUIRY (0x12) |
| Product ID | Standard INQUIRY bytes 16–31 | INQUIRY (0x12) |
| Device Type | Peripheral Device Type bits 4–0 of byte 0 | INQUIRY (0x12) |

---

## Optional Extension
Would you like to include an annotated **hex dump example** of an INQUIRY response showing byte-by-byte decoding (for use with bus traces or emulators)?

