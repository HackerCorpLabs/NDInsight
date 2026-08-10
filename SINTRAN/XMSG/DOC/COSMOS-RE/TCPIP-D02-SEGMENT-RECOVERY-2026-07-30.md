# Recovering a damaged TCP/IP card image from SINTRAN's segment file (2026-07-30)

Source image: `D:\ND\HDD\extract-ronny.img` (SMD pack `PACK-ONE`, 1053 files, 26 users).
All work was **read-only** on the image; extraction went to a scratch directory.

Companion to [TCPIP-211185-B05-MEDIA-RECOVERED-2026-07-30.md](TCPIP-211185-B05-MEDIA-RECOVERED-2026-07-30.md),
which covers the *intact* B05 media from the Tingo pack. This document covers a **newer D02
installation whose BPUN files are damaged**, and the technique used to recover part of it.

Convention: **[V]** verified by measurement. **[I]** inferred. **[U]** unknown.

---

## 1. TL;DR

1. **[V] This pack carries ND 211185 version D02 (January 20, 1992)** - newer than the Tingo B05
   (July 5, 1988) - plus the **SINTRAN Socket Library (SLIB)**, the **Super Kernel Package (SKP)**,
   a **TCPIP-MONITOR** program, and a full installation log.
2. **[V] All four D02 on-card BPUNs fail their checksum.** The damage does reach the TCP-IP user.
3. **[V] The idea of recovering them from the loaded segments works.** Bank 3 was fully recovered
   from `SEGFIL0:DATA` and **verified against the damaged file's own stored checksum**.
4. **[V] Bank 0's payload was located** (page 5015, exact page alignment) but its content does
   **not** reproduce the stored checksum - so the segment copy is not the original either.
5. **[V] Banks 1 and 2 cannot be verified at all** - their stored checksum words are themselves
   zeroed, so there is nothing left to check a candidate against.

---

## 2. Damage assessment

`--fsck` reports **455 errors and 26 warnings**. The damage is real: `FORTRAN-100-G02:PROG` has an
index block overwritten with text, so its "block numbers" decode as ASCII (`373309728`, `695410785`,
...). Affected files include `FORTRAN-100-G02`, `DMAC-1915G:BPUN`, `DIR-BACKUP-D00:BPUN`,
`PICDISP-10:PROG`, and many `OTS-*` / `COMIX` / `PROBAS` files.

**[V] No TCP-IP file appears in the fsck error list** - yet all four BPUNs fail their checksum.
**This is the important lesson**: `fsck` validates block *pointers*, not block *contents*. Pointers
in range, data behind them wrong. A clean fsck on a user is not evidence that the user's files are
good.

### 2.1 The checksums

Using the documented BPUN rule (arithmetic sum of all words in the Data field, modulo 2^16,
big-endian; data field `0x40..0x20043`, checksum word at `0x20044`):

| File | Stored | Computed | Result | Marker at `0x3F` |
|---|---|---|---|---|
| `TCP-SER-B0-D02.BPUN` | `0x2090` | `0x9bfe` | FAIL | `0x00` - **should be `0x21`** |
| `TCP-SER-B1-D02.BPUN` | `0x0000` | `0x28a5` | FAIL | `0x00` - **should be `0x21`** |
| `TCP-SER-B2-D02.BPUN` | `0x0000` | `0xde27` | FAIL | `0x21` ok |
| `TCP-SER-B3-D02.BPUN` | `0x99ee` | `0xd188` | FAIL | `0x21` ok |

Banks 0 and 1 have lost their **first page** outright - the BPUN start marker is gone. Banks 1 and 2
have lost their checksum word as well.

### 2.2 Ruling out the extraction tool first

**[V]** Before concluding damage, `ndtool` was run against the **Tingo** image, whose BPUNs are known
good. All four came out **byte-identical to the reference copies and checksum-valid**. The tool is
not the problem.

---

## 3. The recovery technique

`tcp-ip-lo-D02:MODE` loads each bank into a SINTRAN segment:

```
NEW-SEGMENT TCPS0B0,,,,,,,,        NEW SEGMENT NO:   146
READ-BI (TCP-IP)TCP-SER-B0-D02:BPUN
SE-LO-AD,,177777
```

So **the segment file holds a second copy of every bank**. Segment numbers, from the `:LIST` output
of the actual mode run (SINTRAN prints them in octal):

| Segment | Number (octal) | Loaded from |
|---|---|---|
| `TCPS0B0` | 146 | `TCP-SER-B0-D02:BPUN` |
| `TCPS0B1` | 147 | `TCP-SER-B1-D02:BPUN` |
| `TCPS0B2` | 150 | `TCP-SER-B2-D02:BPUN` |
| `TCPS0B3` | 151 | `TCP-SER-B3-D02:BPUN` |
| `TCPPS` | 145 | `tcpp-D02:PROG`, mass addr `11225` octal |
| `FTPS0` | 152 | - |
| `popwr` | 153 | `PO-PWRFAIL-D02:PROG` |

**[V] Layout**: the BPUN data field is a **4-byte header followed by a page-aligned 128 KB payload**.
The segment copy corresponds to that payload. So a candidate is validated by computing
`wsum(header4 + segment_payload)` and comparing against the damaged file's **stored** checksum -
which survives independently of the payload.

### 3.1 What does NOT work

**[V] Searching by checksum alone is useless.** A 16-bit sum over ~37M candidate offsets produces
~570 false positives by chance; the actual scan returned 519, 1000, 451 and 427 hits. The checksum
is a *verifier*, not a *locator*.

**[V] Segment adjacency does not hold.** The four segments are not consecutive 64-page blocks.
Measured positions: bank 0 content near page 5017, bank 1 near 5124, bank 3 at 5548. There are also
**two** generations present (`TCP PIOC` occurs at pages 5249 and 5590), because C07 and D02 were both
installed and their segment numbers overlap - the C07 run used 150-153, the D02 run 146-151.

**[V] Using the intact B05 files as a positional template is unreliable across versions.** It gave an
exactly page-aligned answer for bank 0 (payload start = page 5015.00, a 1-in-2048 coincidence) but
non-aligned nonsense for bank 3 (page 5195.67 / 5536.67) even though bank 3's true location is known.
D02's layout differs from B05's.

### 3.2 What does work

Scan every **64-page-aligned** window in `SEGFIL0:DATA` and test
`wsum(header4 + payload) == stored`. For bank 3 this returned **exactly one** window, page 5548.

---

## 4. Result

### Bank 3 - RECOVERED [V]

```
rebuilt data-field checksum = 0x99ee
stored checksum             = 0x99ee     MATCH
marker at 0x3F              = 0x21       valid container
non-zero bytes: 3008 recovered vs 2082 in the damaged file
```

Roughly a third of the bank had been lost and is back. Because the recovered payload reproduces the
**file's** stored checksum, the segment copy is byte-identical to the original file content - the
running system had not patched this bank.

Content confirms it is genuine D02:

```
"Ethernet II  January 20, 1992"        <- D02 build date
"TCP PIOC"   "FSMR.TcpTemplate" "FSMR.TcpExtractOob" "FSMR.SendPacket"
"*TCP0*TCPGATE.*TCP"                   <- the XMSG port names the clients look up
"Telnet Server  on   available.No more free connection. Connection closed."
"Lost SK message" / "ProcessSK: error from SKPreceive" / "Fatal internal SLib error"
```

**[V] The `*TCP.` / `*TCPGATE` port names appear in the card image itself**, corroborating the
documentary claim (ND0106) that clients locate the stack by XMSG name.

Written to `TCP-SER-B3-D02-RECOVERED.BPUN`, 131205 bytes, checksum-valid.

### Bank 0 - located but NOT recovered [V]

Payload start is page **5015** of `SEGFIL0:DATA`, established by exact page alignment. But
`wsum = 0x63ac` against a stored `0x2090`. The damaged file itself computes `0x9bfe`. **Three
different values - neither source is the original.** Saved separately as
`TCP-SER-B0-D02-SEGMENT-COPY-UNVERIFIED.BPUN` and must be treated as **[U]**.

**[I]** Most likely explanation: the surviving segment is from a different install generation than
the surviving file, since C07 and D02 overlap in segment numbers. Not established.

### Banks 1 and 2 - unverifiable [V]

Their stored checksum words are zeroed, so no candidate can be validated even if the data is present.
Bank 1's AIP content **is** in the segment file near page 5124. Any reconstruction would be
unfalsifiable, so none was attempted.

---

## 5. Everything else recovered from this pack

Undamaged and readable, none checksum-verifiable (**[V]** `.prog`, `.brf`, `.mode`, `.symb` carry no
checksum - treat anything derived from them as **[U]**):

| File | Why it matters |
|---|---|
| `SLIB.DEFS`, `SLIB.IMPT`, `SLIB-NRE/REE-1B/2B-B01.BRF` | **SINTRAN Socket Library (211566)** - the socket API surface, previously known only from a summary sheet |
| `TCPIP-MONITOR.PROG` | **[I]** almost certainly the PIOC-MONITOR from 211185 diskette 2 |
| `SKP-C00.DEFS/.IMPT/.INTL` | Super Kernel Package |
| `NK-*.BRF/.DEFS/.IMPT` | NUCLEUS libraries |
| `IN-TCP-IP-XX-D02.LOGG` | 16 KB installation log |
| `AIP-CONFIG.SYMB` | absent from Tingo; documents the per-controller config line |
| `TCP-IP-LO-D02.LIST`, `TCP-IP-LO-C07.LIST` | **complete transcripts of real TCP/IP installations**, including RT-LOADER's replies and segment numbers |
| `DEFINE-FTPRT-D02.MODE` | FTPRT installed as a **SINTRAN system server** via DMAC patch |
| `(TCP-COMM)UDP-TCCOM.SYMB` etc. | site-written TCP/UDP source |

**[V] `AIP-CONFIG:SYMB` documents the controller table format**, which the Tingo copy lacks:

```
# TCP number    E-II number    IP address    IP gateway    Subnet bits
0              0              192.168.1.40   000.000.000.000  0
```

**[V] `DEFINE-FTPRT-D02:MODE`** patches the system server table at index `10B` and writes the name
`'FTPRT'` onto the command segment, via `DMAC` / `)CLOAD S3CP`. Its sibling `DEFINE-TCPP-D02:MODE` is
destroyed - same directory, same size, one readable and one garbage. That is the shape of this disk.

---

## 6. Reusable lessons

1. **A clean `fsck` is not an integrity check.** Pointer validity and content validity are different
   questions. Checksum the payload.
2. **`.prog` / `.brf` / `.mode` / `.symb` have no checksum.** Only BPUNs are self-validating. Scope
   integrity claims accordingly.
3. **SINTRAN's segment file is a second copy of every loaded BPUN**, and the BPUN's own stored
   checksum survives independently of its payload - so a damaged file can validate its own
   replacement. This generalises to any product loaded with `READ-BINARY` + `SE-LO-AD`.
4. **Read the `:LIST` output of the mode run.** It gives segment numbers, mass addresses and
   RT-LOADER's actual replies - far better than inferring layout by content search.
5. **A 16-bit checksum verifies, it does not locate.** Expect ~1 false positive per 65536 offsets.

## 7. Next steps

1. Read the SINTRAN segment table to get each segment's mass address and length directly, instead of
   inferring page numbers. That is the clean route to banks 0 and 1.
2. Compare the recovered D02 bank 3 against B05 bank 3 - both are TCP, four years apart.
3. Read `SLIB.DEFS` for the socket API, and `IN-TCP-IP-XX-D02:LOGG` for the real install dialogue.
4. Check whether `TCPIP-MONITOR.PROG` is the PIOC-MONITOR.

---

## Related documents

- [TCPIP-211185-B05-MEDIA-RECOVERED-2026-07-30.md](TCPIP-211185-B05-MEDIA-RECOVERED-2026-07-30.md) - the intact B05 media
- [HOW-ND-SHIPPED-TCPIP-PRODUCT-EVIDENCE-2026-07-26.md](HOW-ND-SHIPPED-TCPIP-PRODUCT-EVIDENCE-2026-07-26.md) - documentary background
- [WRITING-A-TCPIP-STACK-ON-SINTRAN.md](WRITING-A-TCPIP-STACK-ON-SINTRAN.md) - the build guide
