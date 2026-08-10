# ND 211185 TCP/IP Gateway B05 - 68000 firmware image for Ghidra

Built 2026-07-30. **Every source bank was checksum-verified before assembly** - see
[verification](#verification) below. This is the DIX-2.0 image ND shipped for the ND-110063
Ethernet II controller, the same board as the COSMOS ENCOS firmware.

## Files

| File | Size | Contents |
|---|---|---|
| `tcp-ser-all-banks-b05-68k.bin` | 524288 (512 KB) | **load this one** - all four banks concatenated |
| `tcp-ser-b0-b05-68k.bin` | 131072 | bank 0 |
| `tcp-ser-b1-b05-68k.bin` | 131072 | bank 1 |
| `tcp-ser-b2-b05-68k.bin` | 131072 | bank 2 |
| `tcp-ser-b3-b05-68k.bin` | 131072 | bank 3 |

## CORRECTION 2026-07-30 - the banks are NOT one subsystem each

An earlier revision of this file said bank 0 = PIOCOS, bank 1 = AIP, bank 2 = TCP, bank 3 = TCP +
symbol table. **That is wrong and it will mislead a disassembly.** It was inferred from where the
banner *strings* land, which says nothing about where the *code* is.

**[V] Measured layout.** All executable code is ONE contiguous span from about `0x4660` to
`0x23F38`, crossing the 128 KB boundary freely. Every PLANC routine prologue (`2F0E 2C56 2D4F 0008`)
in the image - 430 of them - falls in that range: 376 in bank 0, 54 in bank 1, **none above**.
Verified against the symbol table: `INITLANCE 0x4884`, `RCVCOMPLET 0x602E`, `AIPINIT 0xC942` and
`TCPINPUT 0x10AC8` all sit on an exact prologue. So the media-access, AIP and TCP subsystems are
interleaved in the low code span, **not** one per bank.

Non-zero density per 64 KB confirms it:

```
0x000000-0x00FFFF   72.4%   code
0x010000-0x01FFFF   77.6%   code
0x020000-0x02FFFF   18.4%   tail of code (ends 0x23F38), then tables - port names at 0x24A5C
0x030000-0x03FFFF    5.7%   strings ($AIP text near 0x3CBB0)
0x040000-0x04FFFF     0.1%  essentially empty (TCP banner at 0x469AF)
0x070000-0x07FFFF   18.3%   embedded symbol table (0x7C3A0 onward)
```

**The 128 KB boundaries are EPROM device boundaries, not software modules.** Do not apply any
per-bank offset when relating symbol addresses to file offsets - symbol address == file offset,
flat, base 0.

`sha256(tcp-ser-all-banks-b05-68k.bin)` =
`6d2a76f061859612f0134fda7fead17d1472c646a5c5f5438ab7667667fd04a4`

## Ghidra load parameters

```
Language      : 68000 big endian  (MC68000, default compiler spec)
Base address  : 0x00000000        <- hex, not octal
Length        : 0x80000  (512 KB)
```

Bank N occupies `N * 0x20000` in the file. Same flat model already used for
`encos-ser-all-banks-68k.bin`, so addresses are directly comparable between the COSMOS and TCP/IP
firmwares. **Symbol address == file offset**; see the correction above.

**[V] The 68000 vector table is real and confirmed** at `0x0-0x3FF`: initial SSP `0x5C8`, reset PC
`0x1CFE`. **TRAP #0 and TRAP #2 both vector to `0x3498`** - the same address as ENCOS's PIOC-OS
dispatcher, so the kernel-call gate is shared between the two firmwares. Unused slots point into a
run of 2-byte stubs at `0x1F24+`, the same house pattern as ENCOS. Run `M68kVectorTable.java` to type
and label it.

**CORRECTION 2026-07-30 - there is ONE flat address space.** An earlier revision of this file warned
that CODE (`0x10`) and DRAM (`0x16`) symbols occupy separate, overlapping address spaces, citing
`END_PIOCOS` CODE `0x4660` against `BUFFER_END` DRAM `0x1A00`. **That was wrong** - those two do not
collide, `0x1A00` is simply *below* `0x4660`. **[V]** Zero of the 134 defined DRAM symbols fall
inside the code span, corroborated by absolute `lea` instructions reaching those exact addresses. All
134 were applied successfully. Symbol address == file offset for both kinds.

The measured map is coherent and contiguous:

```
0x00000-0x003FF   68000 vector table
0x00400-0x0465F   low DRAM / BSS  (PIOC_NUMBE 0x64C, REALTIME 0xFC2, BUFFER_STA 0x12F4,
                                   BUFFER_END 0x1A00), plus the 2-byte stub run at 0x1F24+
0x04660-0x23F38   CODE, one contiguous span, all subsystems interleaved
0x24000 onward    tables and strings (dispatch table 0x24A86, port names 0x24A5C)
0x7C3A0 onward    embedded symbol table
```

## Embedded symbol table

Bank 3 carries ND's own symbol table - **463 records** (317 CODE defined, 134 DRAM defined, 12
markers). An earlier count of 437 here came from a stricter scanning filter and undercounted; a
proper walk of the table finds 463. Extent: `0x7C3A0-0x7FBA0` (448 records), then an 8-byte
misalignment, then `0x7FBA8-0x7FD88` (15 records). Record layout (32 bytes):

```
+0x00  4  self/next pointer, increments by 0x20
+0x04  1  name length (1..12)
+0x06  1  0x02 = defined, 0xFF = undefined / marker
+0x07  1  segment: 0x10 = CODE, 0x16 = DRAM, 0x11 = other
+0x08  4  address, big-endian
+0x10 12  name (10 characters in practice)
```

Note this sits **4 bytes later** than the layout recorded for the ENCOS symbol table.

Media-access routines appear under the same names as the ENCOS carve, at this build's addresses -
useful as a second data point: `RCVCOMPLET 0x602E` (ENCOS `0x5C42`), `XMTRINGAPP 0x6600` (ENCOS
`0x6054`), plus `INITLANCE 0x4884`, `STARTMA 0x5C46`, `STOPMA 0x5C6E`, `INTLANCE 0x8198`,
`LNMAINIT 0x7FBC`. TCP-layer names include `TCPINPUT`, `TCPPROCESS`, `INITTCPCB`, `TCPINCKSUM`,
`TCPIPSEND`, `SENDARP`, `ARPINPUT`, `AIPINIT`.

## Provenance and verification

Source: `(TCP-IP)TCP-SER-B0..B3-B05:BPUN` recovered from the Tingo MFM hard-disk dump. Payload
extracted as `bpun[0x44 : 0x44+0x20000]` and concatenated b0..b3 - the identical recipe used for
`encos-ser-all-banks-68k.bin`.

Each source bank passes the documented BPUN checksum (arithmetic sum of all words in the Data field,
modulo 2^16, big-endian; data field `0x40..0x20043`, checksum word at `0x20044`):

| Bank | Marker `0x3F` | Stored | Computed | Result |
|---|---|---|---|---|
| b0 | `0x21` | `0xd998` | `0xd998` | OK |
| b1 | `0x21` | `0x25f3` | `0x25f3` | OK |
| b2 | `0x21` | `0x471e` | `0x471e` | OK |
| b3 | `0x21` | `0x42ce` | `0x42ce` | OK |

Content lands in the expected bank: `APRIL 21, 1986` at `0x0004A2` (bank 0), `$AIP` at `0x03CBB0`
(bank 1), `Transmission Control Protocol` at `0x0469AF` (bank 2), `FSMR.TcpTemplate` at `0x07BA18`
(bank 3).

**A version D02 (1992) installation also exists**, on a different pack, but its BPUNs are damaged -
only bank 3 could be recovered and verified. **No D02 image is provided**, because three of its four
banks cannot be validated. See
`../../../../SINTRAN/XMSG/DOC/COSMOS-RE/TCPIP-D02-SEGMENT-RECOVERY-2026-07-30.md`.

## Related

- `../../../../SINTRAN/XMSG/DOC/COSMOS-RE/TCPIP-211185-B05-MEDIA-RECOVERED-2026-07-30.md`
- `../../../../SINTRAN/XMSG/DOC/COSMOS-RE/WRITING-A-TCPIP-STACK-ON-SINTRAN.md`
- `encos-ser-all-banks-68k.bin` - the COSMOS firmware for the same board
