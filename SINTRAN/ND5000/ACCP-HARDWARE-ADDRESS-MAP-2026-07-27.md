# ACCP hardware address map - full sweep

**Date**: 2026-07-27
**Image**: `E:\Dev\Ronny\NDInsight\Installation\Communication\OctobusAccp\eprom\octo.bin`
**Method**: exhaustive byte sweep of the code region `0x000000-0x0114FF` for every longword
of the form `0x00NN00xx` where NN is a **replicated nibble**, then manual confirmation of
each hit against real disassembly.

**Read the confidence column before using anything here.** The sweep produces candidates.
Several turned out to be false positives - byte sequences that only look like an absolute
address - and they are called out rather than quietly dropped.

---

## 0. CORRECTIONS - read before section 2

Written the same day, after re-reading `ACCP-324716-FIRMWARE-RE-2026-07-27.md`, which is the
write-up of record and already contained more than this sweep did.

- **Select 0x77 is not new.** Section 2 calls `0x770000`/`0x770004`/`0x770006`/`0x770007`
  "CONFIRMED, NEW". Only the sweep's coverage is new - **RE section 2.4b already had
  `0x770004`**, from `movea.l #0x770004,A1` at 0x069E, plus the fact that data arrives into it
  from `0x440000` and that `0x770007` bits 3 **and 4** form a handshake **with a retry count
  of 10**.
- **`0x220000` is a general command/function port, not "the MF-bus command port".** The
  function code selects which target the `0x440000`/`0x550000` pair addresses. Known codes:
  `0x300F`/`0x400A`/`0x400C`/`0x000F` MF-bus memory, `0x0005` AOB, `0x0018` control store,
  plus `0x0001`, `0x0007`, `0x0010`, `0x0015`, `0x0017`, `0x2018`, `0x3010`, `0x4016`,
  `0x8013` observed.
- **`0x330000`/`0x330001` are write-only latches with RAM shadows** at `0x001144EE` /
  `0x001144EF`; the firmware never reads them back. Calling 0x33 an "octobus control latch"
  is wrong. `0x330000` bit 6 = write strobe, bit 2 = control-store gate. Reads of these two
  addresses never happen, so an emulator's read value for them is irrelevant - read the
  shadows instead.
- **`0x660000`/`0x660001` is a shared status byte pair whose bits belong to different
  functions**: `0x660001` bit 1 = AOB busy, bit 2 = message available, bit 4 = MF-bus
  complete; `0x660000` bit 0 = control-store operation OK, bit 3 and bit 5 also tested.
- **`0x00900007` is REAL and this sweep missed it**, because `0x90` is not a replicated
  nibble and the scan filtered on that. `move.b (0x00900007).l,(0x001143B8).l` at 0x07D2.
  **So the nibble-replication rule in section 1 is a strong tendency, not a law, and this
  sweep is therefore NOT exhaustive** - any peripheral at a non-replicated select was
  invisible to it.

---

## 1. The decode pattern

Every peripheral on this card sits at `0xNN0000` with **NN a repeated nibble**. That is a
cheap address decoder: one comparator per nibble pair. Selects seen: 11, 22, 33, 44, 55, 66,
77, 88, AA, BB, DD.

Registers are then at `0xNN0000 + offset`. For the DUART the offset is odd (an 8-bit part on
the low data lane); for the others it is mostly zero or a small even offset.

---

## 2. The map

| Address | Hits | Confidence | What it is |
|---|---|---|---|
| `0x110000-0x117FFF` | 3 | **PROVEN** | **SRAM**, 32 KB, walk-tested by the reset routine at 0x0BD6 as two 16 KB halves |
| `0x220000` | 58 | **PROVEN** | **MF-bus / BADAP command port.** Carved at 0x70CC: `0x300F` open, `0x400A`/`0x400C` sub-function, `0x000F` strobe. The most-touched register in the image |
| `0x220011` | 2 | candidate | |
| `0x220050`, `0x220056` | 1 each | candidate | |
| `0x2200DD` | 1 | likely | `move.b #imm,(0x2200DD).l` - the encoding is unambiguous |
| `0x330000` | 18 | **likely** | **octobus (OBCON) control latch.** Some hits are false positives (`44 EE 00 33` is `move (0x33,A6),CCR`, not an address) - the real ones are `move.b Dn,(0x330000).l` |
| `0x330001` | 33 | **likely** | second byte of the same latch; same false-positive caveat |
| `0x440000` | 16 | **PROVEN** | **MF-bus data, LOW half** |
| `0x550000` | 13 | **PROVEN** | **MF-bus data, HIGH half** (`swap D0` between the two writes) |
| `0x660000` | 12 | **PROVEN** | **status**, read as a word |
| `0x660001` | 14 | **PROVEN** | **status byte, bit 4 = transaction complete** - polled with a countdown; timeout prints `"$MF-bus memory timeout$"` |
| `0x770000` | 5 | **CONFIRMED, NEW** | loaded with `lea (0x770000).l,A1` at five sites in 0x11030-0x11230 - a **window/buffer base**, then accessed indexed |
| `0x770004` | 3 | **CONFIRMED, NEW** | `movea.l #0x770004,A1` (0x4EA, 0x69A) and `move.w D0,(0x770004)` (0x78A2) - a data register |
| `0x770006` | 1 | **CONFIRMED, NEW** | `move.w #4,(0x770006)` at 0xDAC - a command/mode write |
| `0x770007` | 1 | **CONFIRMED, NEW** | `btst #3,(0x770007)` at 0x789A followed by `beq.b -10` - a **ready/busy poll loop** |
| `0x880000` | 5 | **CONFIRMED** | read as a word (`move.w (0x880000).l,D0` at 0x510, inside `Vec27_AutoIrq3`) - FIFO-like drain |
| `0xAA0000` | 3 | **CONFIRMED, NEW** | `move.w (0x20,A6),(0xAA0000).l` at 0x7AE6, 0x7AFA, 0x7B0E - three identical writes of a routine parameter |
| `0xBB0000` | 1 | **CONFIRMED** | `move.w #0,(0xBB0000).l` at 0x88A - a clear/reset |
| `0xBB00DD` | 2 | **CONFIRMED** | `move.b #imm,(0xBB00DD).l` at 0x16B8 and 0x16E0, in the console/DUART code |
| `0xDD0001`..`0xDD001F` | 40 | **PROVEN** | **SCN2681 DUART** - see section 3 |
| ~~`0xFF0011`~~ | 1 | **FALSE POSITIVE** | `0C 39 00 FF 00 11 xx xx` is `cmpi.b #0xFF,(0x0011xxxx).l` - an access to **SRAM**, not to 0xFF0011 |
| ~~`0xFF0020`~~ | 1 | **FALSE POSITIVE** | `33 7C 00 FF 00 20` is `move.w #0x00FF,(0x20,A1)` - an immediate and a displacement, not an address |

**Selects 0x77 and 0xAA were not in any previous list.** Neither was 0x44 or 0x55 before
0x70CC was hand-disassembled. Assume the list is still incomplete until every routine is
carved.

---

## 3. The SCN2681 DUART at 0x00DD0000

Register **N** is at **`0xDD0000 + 2N + 1`**. Every offset the sweep found maps onto a real
SCN2681 register, with no gaps and nothing left over - which is what makes this a proof
rather than a guess.

| Offset | Reg | SCN2681 register (read / write) | Hits |
|---|---|---|---|
| 0x01 | 0 | MR1A / MR2A | 2 |
| 0x03 | 1 | SRA / CSRA | 4 |
| 0x05 | 2 | — / CRA | 7 |
| 0x07 | 3 | RHRA / **THRA** | 2 |
| 0x09 | 4 | IPCR / ACR | 1 |
| 0x0B | 5 | ISR / IMR | 6 |
| 0x0D | 6 | CTU | 1 |
| 0x0F | 7 | CTL | 1 |
| 0x11 | 8 | MR1B / MR2B | 2 |
| 0x13 | 9 | SRB / CSRB | 3 |
| 0x15 | 10 | — / CRB | 8 |
| 0x17 | 11 | RHRB / **THRB** | 2 |
| 0x1F | 15 | — / OPCR, Set Output Port | 1 |

Channel A is the ACCP console. Channel B is a second line whose purpose is still unknown -
though `SET-SERIAL-LINE <Enable ND100-communication via serial line ? (y/n)>` (command 0x35)
strongly suggests **channel B carries ACCP-to-ND100 communication as an alternative to the
octobus**. Not yet proven from code.

TX rings: A at 0x0011307E, B at 0x00112EC2 (+0x10 limit, +0x12 count). Busy flags in the
byte at 0x001131D8, bit 0 = A, bit 4 = B.

---

## 4. What is still unknown

- **The MF-bus function codes.** `0x3`, `0x4`, `0x0` and sub-functions `0x0A`, `0x0C` are
  transcribed but their meanings are not established.
- **Which select is the octobus (OBCON).** 0x33 is the strongest candidate on access density
  and on sitting inside the 0x6A74-0x7C14 driver region, but 0x77, 0x88 and 0xAA are all
  unassigned and any could be part of it. **Read ND-14001-1-EN chapter 4 before naming
  these** - it documents OBCON, the frame format both directions, the acknowledge bits and
  the INT7 OCTObus Message Reset Register.
- **0xBB00DD and 0x2200DD** - a 0xDD offset under two different selects is odd enough to be
  worth explaining rather than assuming.

---

## Provenance

The sweep is mechanical and complete over 0x000000-0x0114FF. Every entry marked CONFIRMED or
NEW was checked by reading the actual instruction bytes at the listed address. Entries marked
"candidate" have not been; entries marked FALSE POSITIVE were rejected after checking. The
PROVEN entries were established earlier by disassembly (reset routine, 0x70CC, 0x1D4C) and
are restated here.
