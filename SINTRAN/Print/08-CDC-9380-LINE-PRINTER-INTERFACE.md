# 08 - The CDC 9380 Line-Printer Interface (device 430)

This document is the register-level reference for the classic **parallel line
printer** on the NORD-100: device number **430 octal**, the "type 2 / parallel /
CDC" printer in SINTRAN's config. It reproduces the programming specification so
an emulator author or driver writer has the exact bit layout.

Source: `../../Reference-Manuals/ND-06.016.01_NORD-100_Input_Output_System.md`
Appendix B.3 "Specification of Line Printer Interface for CDC 9380 for
NORD-10/100", and the I/O System Example 1. Register roles cross-checked with the
byte-verified driver path in `../Devices/LINE-PRINTER-CONFIG-INSPECTION.md`.

---

## 1. Identity

| Property | Value |
|----------|-------|
| Standard device number | **0430 octal** (block 0430-0433) |
| Number of device numbers | 4 |
| Standard interrupt level | 10 (decimal) |
| Standard ident code | 3 |
| Channels | One - output only |
| SINTRAN logical device number | 5 (line printer 1); 15 octal (line printer 2, device 0434) |

Line printer 2 is the next block: device **0434 octal** (0434-0437), ident code
23 octal, logical device 15 octal.

---

## 2. Registers

The interface follows the ND standard register numbering. The IOX device
register address is `device number + register number`:

| Register | Reg. no. | IOX address (LP1) | Direction |
|----------|----------|-------------------|-----------|
| Read data (loopback, test only) | 0 | IOX 0430 | read |
| Write data | 1 | IOX 0431 | write |
| Read status | 2 | IOX 0432 | read |
| Write control | 3 | IOX 0433 | write |

(The I/O System Example 1 lists these as control=3, status=2, data=1, input-data
(test loopback)=0.)

---

## 3. Write Control Word (IOX dev.no. + 3, i.e. IOX 0433)

| Bit | Meaning |
|-----|---------|
| 0 | Enable interrupt on "ready for transfer" |
| 1 | Enable interrupt on error |
| 2 | **Activate device** (print the character now in the buffer) |
| 3 | Test (loopback: lets you read back the data written) |
| 4 | Device and interface clear |
| 5-15 | Not used |

Example (I/O System Problem 2): writing 7 octal to the control register of line
printer 2 -> `SAA 7 ; IOX 0437` (0434 + 3 = 0437). Bits 0,1,2 set = enable both
interrupts and activate.

---

## 4. Read Status Word (IOX dev.no. + 2, i.e. IOX 0432)

| Bit | Meaning |
|-----|---------|
| 0 | Interrupt enabled on ready |
| 1 | Interrupt enabled on error |
| 2 | Not used |
| 3 | **Ready for transfer** |
| 4 | **Error** (bit 5 or 6 set) |
| 5 | Line printer not ready |
| 6 | Out of paper |
| 7 | Compressed pitch |
| 8 | LP9 is on: data on the lines is format information, interpreted as control code |
| 9 | Inhibit - illegal character in buffer |
| 10 | Not used |
| 11-12 | Band detect (character band installed - see below) |
| 13-15 | Not used |

**Band detect (bits 11-12)** - which print band/character set is fitted:

| Bit 11 | Bit 12 | Band |
|--------|--------|------|
| 0 | 0 | 128 characters |
| 1 | 0 | 96 characters |
| 0 | 1 | 64 characters |
| 1 | 1 | 48 characters |

Note from the manual: "This interface is only handling 64, 96 character
printers."

---

## 5. Write Data Word (IOX dev.no. + 1, i.e. IOX 0431)

Writes one character into the interface's buffer register. Then you set bit 2 of
the control word to actually print it.

**Character rules (from the spec):** all codes 0-37 octal are *illegal and
ignored* by the interface, **except** the following control codes:

| Code (octal) | Control | Effect |
|--------------|---------|--------|
| 11 | HT | Gives a space in the CDC controller |
| 12 | LF | Line feed |
| 14 | FF | Form feed |
| 15 | CR | Carriage return |
| 20-33 | (VFU) | Vertical Format Unit channels: give LP9 and disable LP5 |
| 20 | VFU channel 1 | (= form feed) |
| ... | ... | ... |
| 33 | VFU channel 12 | |

So printable characters are the normal ASCII range; only the listed control
codes have an effect, and other low codes are dropped by the hardware.

---

## 6. Read Data Word (IOX dev.no. + 0, i.e. IOX 0430)

Only meaningful in **test mode** (control-word bit 3 set): it reads back the data
you wrote to the buffer, for loopback testing. Not used in normal printing.

---

## 7. The minimal driver loop

Putting the registers together, printing one character is:

```
   1. Write the character   -> IOX 0431   (data register)
   2. Activate + enable IE  -> IOX 0433   (control: bit 2 set, bit 0 set)
   3. Wait for the level-10 "ready for transfer" interrupt
      (ident code 3 dispatches to the LP datafield)
   4. Read status           -> IOX 0432   (check bit 3 ready, bit 4 error)
   5. Repeat for the next character
```

This matches the byte-verified SINTRAN print path (CONFIG-INSPECTION doc): the
driver `TLPRINT` writes `IOX 0431`, then `IOX 0433` (activate + interrupt
enable), waits for the level-10 ready interrupt, whose ident code 3 routes
through the ident table `ITB10` back to the printer datafield.

---

## 8. Emulator note

The RetroCore emulator implements this device as printer **type 2 (parallel,
DMLP), ident code 3** (CONFIG-INSPECTION doc). If `@COPY-FILE LINE-PRINTER,file`
returns error 33 "NO SUCH LOGICAL UNIT", the cause is almost always
`LPSELECTION = 0` in the config table (printer not enabled at system
generation), not the register handling - see
`../Devices/LINE-PRINTER-CONFIG-INSPECTION.md` for the full diagnosis and the
patch (set LPSELECTION = 2).

---

## 9. Related

- [02-HARDWARE-DEVICES.md](02-HARDWARE-DEVICES.md) - the printer hardware
  landscape and the three line-printer interface types.
- `../Devices/LINE-PRINTER-CONFIG-INSPECTION.md` - byte-verified boot/config path
  and the error-33 fix.
