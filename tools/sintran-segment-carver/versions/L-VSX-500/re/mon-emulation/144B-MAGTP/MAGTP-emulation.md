# MON 144B DeviceFunction (MAGTP) - emulation notes

Behaviour-focused model for an emulator author. Handler entry `MAGTP = 26354B` in
`006-S3FS.bin` (byte-verified, `144B-MAGTP.ASM`). The function-code list is taken
from the SINTRAN source `IP-P2-SCSI-MAGTP.NPL` (function-code header block) and the
DeviceFunction parameter contract from ND-860228 (Monitor Calls).

## Parameters

```
  param 1 : function code                 INT  (in)
  param 2 : buffer used for data transfer ARR  (in; used for READ/WRITE record ops)
  param 3 : logical device number         INT  (in; the tape/streamer LDN)
  param 4 : 1st device-dependent param    INT  (in)
  param 5 : 2nd device-dependent param    INT  (in)
```

Return: status in A (device/driver status word; 0 = OK, non-zero = device error
code / status bits). For READ-type functions the data lands in the caller's buffer
(param 2); READ STATUS / READ ERROR COUNTERS return their values through the
buffer and/or the device-dependent parameters.

## Function codes (octal) - from IP-P2-SCSI-MAGTP.NPL

| Code | Function                                   |
|------|--------------------------------------------|
| 0    | READ (one record into buffer)              |
| 1    | WRITE (one record from buffer)             |
| 2    | READ PARITY                                |
| 7    | ERASE                                      |
| 12   | WRITE EOF (tape mark)                       |
| 13   | REWIND                                      |
| 14   | WRITE SKIP                                  |
| 20   | READ STATUS (TEST UNIT READY)              |
| 24   | READ LAST STATUS                           |
| 25   | READ ERROR COUNTERS                        |
| 26   | READ BYTE RECORD                           |
| 27   | WRITE BYTE RECORD                          |
| 37   | READ EXTENDED STATUS                        |
| 42   | READ FORMAT                                |
| 50   | READ MULTIPLE RECORDS                      |
| 51   | WRITE MULTIPLE RECORDS                     |
| 60   | READ WITH DOUBLE AMOUNT                    |
| 61   | WRITE WITH DOUBLE AMOUNT                   |
| 62   | READ DOUBLE AMOUNT BYTE RECORD            |
| 63   | WRITE DOUBLE AMOUNT BYTE RECORD           |
| 66   | READ WITH DOUBLE AMOUNT, DON'T CLEAR CACHE|
| 73   | TEST UNIT READY                            |
| 75   | INQUIRY (READ DEVICE TYPE)                 |

The handler at `26446B..26467B` bounds-checks the code (`SAT 100`, `SAT 177`) and
masks with `AND 66` to pick a device-function group before indexing the driver's
jump table (`LDT I 62`). Codes >= `100B` are an extended/aliased range.

## What the linker needs

The ND linker touches MON 144B but it is genuinely unimplemented in the emulator.
For a first pass the emulator can:
- Accept the 5-parameter block, read `function code` (param 1) and `LDN` (param 3).
- If the emulated "device" is not a tape/streamer, return a device-error status in
  A (non-zero) rather than crashing - the linker's normal path does not depend on a
  real tape, it just probes the device.
- If it must appear to succeed, implement the harmless status functions:
  `20B/73B TEST UNIT READY` -> return "ready" status (0), and
  `75B INQUIRY` -> return a device-type word in the buffer.
- READ/WRITE record functions (`0/1/26/27/50/51/60/61`) move `param 4/5` bytes
  between the caller's buffer (param 2) and the device.

## Pseudocode

```
function MAGTP(func, buf, ldn, p1, p2) -> A:
    if not device_is_tape_like(ldn):
        return DEVICE_ERROR_STATUS
    group = func & 0o66                 # handler masks the code
    switch func:
        0, 26, 50, 60, 62, 66: return tape_read(ldn, buf, p1, p2)
        1, 27, 51, 61, 63:     return tape_write(ldn, buf, p1, p2)
        12:  return tape_write_eof(ldn)
        13:  return tape_rewind(ldn)
        7:   return tape_erase(ldn)
        14:  return tape_write_skip(ldn)
        20, 73: return tape_test_unit_ready(ldn)      # status in A
        24:  return tape_last_status(ldn, buf)
        25:  return tape_error_counters(ldn, buf)
        37:  return tape_extended_status(ldn, buf)
        42:  return tape_read_format(ldn, buf)
        75:  return tape_inquiry(ldn, buf)            # device type into buf
        default: return DEVICE_ERROR_STATUS
```

## C sketch

```c
/* Returns device status word (0 = OK). buf is the caller's transfer buffer. */
int sintran_magtp(int func, uint8_t *buf, size_t bufbytes,
                  int ldn, int p1, int p2)
{
    if (!device_is_tape_like(ldn))
        return DEV_NOT_RESERVED;          /* non-zero status; do not crash */

    switch (func) {
    case 000: case 026: case 050:
    case 060: case 062: case 066:
        return tape_read(ldn, buf, bufbytes, p1, p2);
    case 001: case 027: case 051:
    case 061: case 063:
        return tape_write(ldn, buf, bufbytes, p1, p2);
    case 012: return tape_write_eof(ldn);
    case 013: return tape_rewind(ldn);
    case 007: return tape_erase(ldn);
    case 014: return tape_write_skip(ldn);
    case 020: case 073: return tape_test_unit_ready(ldn);
    case 024: return tape_last_status(ldn, buf, bufbytes);
    case 025: return tape_error_counters(ldn, buf, bufbytes);
    case 037: return tape_extended_status(ldn, buf, bufbytes);
    case 042: return tape_read_format(ldn, buf, bufbytes);
    case 075: return tape_inquiry(ldn, buf, bufbytes);
    default:  return DEV_ILLEGAL_FUNCTION; /* non-zero status */
    }
}
```

## Not verified / open items

- The exact per-function jump-table offsets and each function's buffer layout live
  in the device driver (`IP-P2-SCSI-MAGTP.NPL` and the tape/streamer drivers),
  outside the 96-word MAGTP dispatch window; only the dispatch/validation was
  byte-traced here.
- The precise meaning of the `AND 66` mask and the `100B..177B` extended range was
  not decoded to individual functions.
