# ENCOS-SER 68000 Firmware - Stripped Binaries

Raw Motorola 68000 firmware for the **ND-110063 Ethernet Controller** (COSMOS
Ethernet II Option, ND product 210580, version B01, dated 1987-02-27),
extracted from the four `ENCOS-SER-Bn-B01:BPUN` distribution files in the
parent folder (`..`). The BPUN files originate from the distribution floppy
`210580B01-XX-01D` (see the PD sheet
[ND-210580-02-EN.md](../ND-210580-02-EN.md)).

Extraction performed 2026-07-07. All statements below are marked VERIFIED
(checked against the actual bytes or official documentation) or ASSUMPTION.

---

## Files

| File | Size (bytes) | Content |
|------|--------------|---------|
| `encos-ser-b0-68k.bin` | 131072 | Bank 0: vectors + main code. Non-zero 0x00002-0x1E1C8 |
| `encos-ser-b1-68k.bin` | 131072 | Bank 1: initialized data. Non-zero 0x0D266-0x1636B |
| `encos-ser-b2-68k.bin` | 131072 | Bank 2: all zeros (BSS / buffer space) |
| `encos-ser-b3-68k.bin` | 131072 | Bank 3: data/tables. Non-zero 0x05B6C-0x172BD |
| `encos-ser-all-banks-68k.bin` | 524288 | Concatenation b0\|b1\|b2\|b3 (see Memory model) |

Each bank is 64K words = 128KB. Byte order is big-endian throughout
(both the BPUN container word format and 68000 convention).

---

## Source BPUN container format (VERIFIED)

Each `ENCOS-SER-Bn-B01:BPUN` file (131205 bytes) parses per the standard ND
BPUN (Bootable Punched Tape) format:

| Offset (bytes) | Field | Value in these files |
|----------------|-------|----------------------|
| 0-62 | Preamble / Start / Boot sections (ASCII) | 63 binary zeros = empty; B (start) = 0, C (boot) = 0; no bootstrap loader present |
| 63 | `!` (0x21) start-of-data delimiter | present |
| 64-65 | E: load address, 16-bit BE word | 0x0000 |
| 66-67 | F: word count, 16-bit BE word | 0x0000 = **65536 words** (see quirk) |
| 68-131139 | G: data, 65536 words BE | the raw bank payload (extracted here) |
| 131140-131141 | H: checksum = 16-bit arithmetic sum of all data words | VERIFIED for every bank (see table) |
| 131142-131143 | I: action code | 0x0000 |
| 131144-131204 | padding | zeros |

Checksums (computed word-sum mod 2^16 vs stored):

| Bank | Stored | Computed | Match | Non-zero payload bytes |
|------|--------|----------|-------|------------------------|
| B0 | 0x7AF4 | 0x7AF4 | yes | 59568 |
| B1 | 0xC319 | 0xC319 | yes | 154 |
| B2 | 0x0000 | 0x0000 | yes (all-zero data) | 0 |
| B3 | 0xB5BC | 0xB5BC | yes | 19071 |

### Word-count quirk (VERIFIED, important)

The F (word count) field is 0x0000 but the payload is verifiably 65536 words:
the stored checksum matches the sum over exactly 131072 bytes. 65536 mod 2^16
= 0, so **a count of 0 must be interpreted as 65536** for these files. A naive
BPUN reader (e.g. the simh nd100 `sim_load`) would read zero words. Any tool
consuming these files must special-case F = 0.

These files have no bootstrap preamble and action = 0; they were never meant
to run stand-alone on an ND-100. They are staged onto ND-100 segments
(ENCS0B0-B3, via RT-loader `READ-BINARY`) and pushed to the controller by the
ND-100-side supervisor at server start.

---

## Memory model (VERIFIED)

The four banks form one flat 512KB 68000 address space, in concatenation
order:

| Bank | 68000 address range |
|------|---------------------|
| B0 | 0x00000 - 0x1FFFF |
| B1 | 0x20000 - 0x3FFFF |
| B2 | 0x40000 - 0x5FFFF |
| B3 | 0x60000 - 0x7FFFF |

`encos-ser-all-banks-68k.bin` is built in this order.

VERIFICATION (2026-07-07): a live capture of the controller RAM taken from a
running SINTRAN system after driver load (`D:\ND\ETHII\RAM_00000000.BIN`,
524288 bytes) is byte-for-byte IDENTICAL to `encos-ser-all-banks-68k.bin`
(all 524288 bytes match, zero differences). This proves the flat consecutive
bank mapping, proves the BPUN stripping offsets, and shows the ND-100-side
loader writes the banks into controller memory unmodified (no relocation or
patching at load time). Supporting evidence from static analysis (bank 0 code
contains 848 longword references into bank 1's range and 240 into bank 3's)
is consistent.

This 512KB is the memory shared between the ND-100 and the controller's
68000 (mapped on the ND-100 side as a PIOC/Ether memory range from the
generation-time MMPIOCS table).

---

## 68000 entry state (VERIFIED from bank 0 bytes)

Exception vector table at address 0:

- Initial SSP (vector 0) = 0x000005C8
- Initial PC  (vector 1) = 0x00001CFE
- Vectors 2/3 (bus error / address error) = 0x0000211C / 0x00002136
- Vectors from 4 onward point into a dense table at 0x1F24, 0x1F26, ...
  (2 bytes apart - shared-handler dispatch of some kind, not yet analyzed)

First instructions at reset PC 0x1CFE:

```
23 C8 00 00 05 00    MOVE.L  A0,($500).L
41 F9 00 00 04 0A    LEA     ($40A).L,A0
31 7C 00 01 00 04    MOVE.W  #1,4(A0)
31 7C 00 00 00 02    MOVE.W  #0,2(A0)
41 F9 00 00 04 06    LEA     ($406).L,A0
30 BC ...            MOVE.W  #...,(A0)
```

Early writes to the 0x400-0x500 area look like hardware/mailbox
initialization (interpretation, not verified).

---

## Ghidra import

- File: `encos-ser-all-banks-68k.bin` (or `encos-ser-b0-68k.bin` alone)
- Language: 68000 (big-endian), e.g. `68000:BE:32:default`
- Image base: 0x00000000
- Disassemble from 0x1CFE (reset PC); mark 0x0-0x3FF as the vector table

---

## Related pieces of the puzzle

- **ND-100-side supervisor**: `ENCOS-ERR-0-B01:BRF` (in `..`), loaded on
  segment ENCOSE0 as RT program ENNS0. BRF symbols (decoded per
  [BRF-FILE-FORMAT.md](../../../../SINTRAN/File-Formats/BRF-FILE-FORMAT.md)):
  `ENNS0` (MAIN), `POSUERR`, `READPIO`, `SEGLOAD`, `UNLOAD`, `UEIEDIN`.
  The startup failure message "POSU Error during startup / Check if RTCOMMON
  is in interface memory / Error in communicating with XROUT" comes from this
  code. A linked absolute image can be produced on the running system with
  RT-loader `BINARY-DUMP "(UTILITY)ENCOSE0-DUMP:BPUN",ENCOSE0,0,47777`
  (segment ENCOSE0 spans 0-47777 octal).
- **Load path**: mode file stages the 4 BPUNs onto ND-100 segments 143-146;
  at `START-NETWORK-SERVER ENNS0` the supervisor copies them into controller
  memory through the interface window and handshakes with the 68000.
  Reversing `SEGLOAD`/`READPIO` (ND-100 side) plus the reset code at 0x1CFE
  (68000 side) yields the mailbox protocol - the surface an emulated
  ND-110063 device must implement.
- PD sheet / install doc: [ND-210580-02-EN.md](../ND-210580-02-EN.md)
