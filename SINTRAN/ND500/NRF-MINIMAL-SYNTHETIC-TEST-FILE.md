# Minimal Synthetic :NRF Test File (ND-500 Loader Fixture)

**Purpose**: the smallest possible ND Relocatable Format file that NLL will accept with LOAD-SEGMENT and that produces a runnable program which immediately terminates (MON 0 / LEAVE). Fixture for the domain-handling unit tests (see `ND500-DOMAIN-HANDLING-TEST-COMMAND-SEQUENCE.md`, Phase H).

**Sources**:
- NRF format: ND-60.136.04A chapter 12 (sections 12.1-12.3) - VERIFIED against the manual text.
- CALL instruction encoding + segment-31 MON convention: RetroCore emulator sources (`E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\ND500\Instructions\CALL\Call.cs` opcode 0xC3, `CpuND500.IndirectSegments.cs` segment = bits 31:27, MON n = offset n on segment 31, i.e. address 0xF8000000+n; `Sintran\MON_0_LEAVE.cs` MON 0 = program exit).
- Open items are marked DERIVED or TO-VALIDATE below. Nothing here has run against real NLL yet.

---

## 1. NRF group encoding (manual section 12.1)

Each NRF group = one control byte, optionally followed by a numeric field and/or symbolic field:

```
control byte = | control number (5 bits) | numeric length NL (3 bits) |
numeric field = NL bytes (0-7), signed, 2's complement
symbol field  = 1 length byte + 1-255 ASCII chars (only for some controls)
```

**CONFIRMED (bit packing)**: `control_byte = (ctrl << 3) | NL` - VERIFIED 2026-08-10 against
a real compiler-produced NRF file, `E:\Dev\Ronny\ND500\Microcode\nd-500-apf-lib-e.nrf` (an
APF vector-math library). Three independent exact matches (MSG group's 17-char message,
LIB group's 7-char symbol `VADDXXX`, DEF group's 10-char hidden symbol `#(+PROG0+)`), each
confirmed by the following byte matching its expected symbol-length exactly - see
`../File-Formats/NRF-FILE-FORMAT.md` for the full byte-by-byte table. This closes item 1 of
the validation checklist below.

Control numbers used here (manual section 12.2, numbers are octal):

| Ctrl | Oct | Dec | Meaning | Key property we exploit |
|------|-----|-----|---------|--------------------------|
| BEG | 1 | 1 | Start of module; numeric bytes = priority, language (0=assembly), ADL | After BEG, load mode = program (PMO) |
| MSA | 3 | 3 | Main start address := current byte address (+NV) | NL=0 -> start = here |
| LDI | 21 | 17 | Load NL immediate bytes at current pointer | Raw machine-code injection, max 7 bytes/group |
| END | 2 | 2 | End of module, checksum in numeric field | **NL=0 -> NO checksum test** (manual: "If numeric length is 0, no checksum test is performed") |
| EOF | 26 | 22 | End of NRF file | |

## 2. The payload program

One instruction: terminate via MON 0 (LEAVE). On the ND-500 a monitor call is an ordinary subroutine call into indirect segment 37B (31 decimal); MON n lives at byte offset n (EQU 37B9+n). Segment sits in address bits 31:27, so MON 0 = address 0xF8000000.

```
CALL 0xF8000000, 0        ; call MON 0 (LEAVE) with zero arguments
```

Encoding (opcode from Call.cs; address is a direct 4-byte operand; ND-500 is big-endian):

```
C3 F8 00 00 00 00
^  ^---------- ^-- argument count = 0
|  4-byte subroutine address 0xF8000000 (MSB first)
CALL opcode
```

**VALIDATED against the emulator decoder (2026-07-19)**: the arg-count byte 0x00 is an operand ADDRESS CODE that decodes as CONSTANT_SHORT with value 0 (address codes 0x00-0x3F = constant short, value in bits 5:0) - so the byte VALUE 0x00 is correct, and its interpretation is a specifier-encoded constant, not a bare byte. Unit test `E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests.ND500\TestND500_NrfMinimalFixture.cs` (4 tests, all green) proves via `CpuND500.FetchInstruction`/`Disassemble`: opcode 0x00C3 CALL, TotalLength 6, OperandCount 2, operand 0 = big-endian direct 0xF8000000 (segment 31, offset 0 = MON 0), operand 1 = constant 0, and the disassembly text renders CALL. The test also pins `BuildMinimalNrf()` to the documented 14-byte stream.

Note: segment-31 calls skip entry-point validation (handled before the ENTS/ENTM check in Callg/Call), so no entry instruction is needed at the target - and MON 0 never returns, so nothing is needed after the CALL either.

## 3. The complete file - 14 bytes

| Offset | Bytes (hex) | Group | Meaning |
|--------|-------------|-------|---------|
| 0 | `0B` | BEG, NL=3 | (1<<3)\|3 - module start |
| 1 | `00 00 04` | | priority=0, language=0 (assembly), ADL=4 (32-bit word addresses) |
| 4 | `18` | MSA, NL=0 | (3<<3)\|0 - main start address := PP (= 0, start of program segment) |
| 5 | `8E` | LDI, NL=6 | (17<<3)\|6 - load 6 immediate bytes to program segment |
| 6 | `C3 F8 00 00 00 00` | | CALL 0xF8000000, 0 args = MON 0 LEAVE |
| 12 | `10` | END, NL=0 | (2<<3)\|0 - end of module, NO checksum (NL=0) |
| 13 | `B0` | EOF, NL=0 | (22<<3)\|0 - end of NRF file |

Full stream:

```
0B 00 00 04 18 8E C3 F8 00 00 00 00 10 B0
```

Design choices, all chosen to minimize format risk:

- END with NL=0 avoids computing the checksum at all (explicitly allowed by section 12.2).
- LDI groups carry max 7 bytes; the 6-byte payload fits one group. A larger payload = a sequence of LDI groups.
- No DEF/DDF/REF symbols, no data segment (mode stays PMO from BEG), no library controls.
- ADL=4: BEG's third numeric byte; default would be 1, but REF/APA/ADA sizing and address alignment work in ADL units, and 4 matches the 32-bit word. TO-VALIDATE against a real compiler NRF's BEG bytes.

## 4. C# fixture builder (unit-test snippet)

```csharp
/// <summary>
/// Builds the minimal synthetic :NRF fixture: one module whose program is
/// a single CALL to segment 37B offset 0 = MON 0 (LEAVE / exit program).
/// Byte layout documented in NRF-MINIMAL-SYNTHETIC-TEST-FILE.md - keep the
/// two files in sync. Wrapper format: ND-60.136.04A chapter 12.
/// </summary>
public static byte[] BuildMinimalNrf()
{
    // control byte = (ctrl << 3) | NL  -- DERIVED packing, see doc section 1
    byte[] nrf = new byte[14];
    int i = 0;
    nrf[i++] = (1 << 3) | 3;   // BEG, NL=3
    nrf[i++] = 0;              //   priority 0
    nrf[i++] = 0;              //   language 0 = assembly
    nrf[i++] = 4;              //   ADL = 4 (32-bit addresses)
    nrf[i++] = (3 << 3) | 0;   // MSA, NL=0: start address = current PP = 0
    nrf[i++] = (17 << 3) | 6;  // LDI, NL=6: six immediate program bytes follow
    nrf[i++] = 0xC3;           //   CALL (direct 4-byte address operand)
    nrf[i++] = 0xF8;           //   address 0xF8000000 = segment 31 (37B), offset 0
    nrf[i++] = 0x00;           //   ... big-endian (ND-500: MSB at lowest address)
    nrf[i++] = 0x00;
    nrf[i++] = 0x00;
    nrf[i++] = 0x00;           //   argument count = 0 (TO-VALIDATE: bare byte vs specifier)
    nrf[i++] = (2 << 3) | 0;   // END, NL=0: no checksum test performed
    nrf[i++] = (22 << 3) | 0;  // EOF
    return nrf;
}
```

Write the bytes to a SINTRAN file named e.g. `TEST-CODE:NRF` on the test disk image (LOAD-SEGMENT's default file type is :NRF, section 6.3.1).

## 5. Expected behavior in the Phase H tests

- `NLL: LOAD-SEGMENT TEST-CODE` -> load report; program bytes land at P:0-5; MSA registers start address 0.
- `NLL: EXIT` (H2) or `NLL: RUN` (H1) -> the program executes CALL -> segment-31 indirect -> MON 0 -> normal termination -> back to the NLL:/N500: prompt.
- Any trap (ISE/IOS) instead of clean exit points at the arg-count encoding or the packing assumption - fix per the TO-VALIDATE notes and update this file.

## 6. Validation checklist before trusting the fixture

1. [ ] Bit packing: first byte of a real compiler :NRF decodes as BEG under `(ctrl<<3)|NL`.
2. [x] Payload: RetroCore decoder/disassembler renders the 6 payload bytes as CALL 0xF8000000 with 0 args, length 6. DONE 2026-07-19 - unit test `TestND500_NrfMinimalFixture` in `E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests.ND500`, 4/4 passing (decode structure, segment/MON derivation, disassembly text, byte-stream pin).
3. [ ] BEG numeric bytes (priority/language/ADL) match a real compiler NRF's BEG group.
4. [ ] Live NLL (once available): LOAD-SEGMENT accepts the file with no error and WRITE-SEGMENT-STATUS shows a 6-byte program segment with start address 0.
