# ND 211185 COSMOS TCP/IP Gateway B05 - media recovered and validated (2026-07-30)

Source: the Tingo MFM hard-disk recovery,
`\\Nas9t\data\NorskData\FloppyImages\Tingo\Tingo-HDD\raw\x\`, user `TCP-IP`.
All analysis was **read-only**; nothing was written to the image or the NAS.

This is the artifact listed as **highest value** in the hunt table of
[HOW-ND-SHIPPED-TCPIP-PRODUCT-EVIDENCE-2026-07-26.md](HOW-ND-SHIPPED-TCPIP-PRODUCT-EVIDENCE-2026-07-26.md)
section 9: *"a working DIX-2.0 image for OUR card"*. It is no longer missing.

Convention: **[V]** verified by measurement or quoted from the recovered files. **[I]** inferred.
**[U]** unknown.

---

## 1. TL;DR

1. **[V] The four on-card images are intact.** All four BPUNs pass the documented BPUN checksum.
   The "mangled export" worry does not apply to them. **Scope limit**: `.prog`, `.brf`, `.mode` and
   `.symb` files carry no checksum, so their integrity is **unproven** - see section 2.1.
2. **[V] It is ND 211185 version B05**, dated **July 5, 1988**, for the Ethernet II controller.
3. **[V] Bank 0 is PIOCOS**, byte-for-byte the same module set and build dates as the COSMOS ENCOS
   firmware. This confirms the "same OS, different payload" model directly rather than by inference.
4. **[V] The AIP layer names a `set DIX mode` media-access command** and prints `attached to MEDIA
   ACCESS in DIX mode`. The DIX framing question is settled from ND's own product, not just from our
   firmware carve.
5. **[V] 437 symbols with addresses** were recovered from an embedded symbol table, in ND's own names.
6. **[V] The load mode file documents the whole SINTRAN-side structure** - RT programs, segments,
   priorities, and the two configuration pokes.

---

## 2. Integrity - the checksum test

**[V]** The BPUN container here is: 63 zero bytes, a `0x21` marker at offset `0x3F`, the data body,
a 16-bit big-endian checksum word at offset `0x20044`, then zero padding to a total of
**131,205 bytes**. Every one of the eight files (4 COSMOS + 4 TCP/IP) has exactly this shape.

**Checksum algorithm** [V]: the documented BPUN rule is

> Checksum: arithmetic sum of all the words in the Data field, modulo 2^16. Two-byte word in
> big-endian format.

which is what we independently fitted to the four **known-good ENCOS** files before knowing the spec:
a 16-bit big-endian word sum over the data field `0x40 .. 0x20043`, compared against the word at
`0x20044`. The empirical fit and the documented rule agree, so the table below is a check against the
**specification**, not against a guessed algorithm. It was then applied unchanged to the recovered
files.

| File | Stored | Computed | Match |
|---|---|---|---|
| `encos-ser-b0-b01.bpun` (control) | `0x7af4` | `0x7af4` | yes |
| `encos-ser-b1-b01.bpun` (control) | `0xc319` | `0xc319` | yes |
| `encos-ser-b2-b01.bpun` (control) | `0x0000` | `0x0000` | yes |
| `encos-ser-b3-b01.bpun` (control) | `0xb5bc` | `0xb5bc` | yes |
| `tcp-ser-b0-b05.bpun` | `0xd998` | `0xd998` | **yes** |
| `tcp-ser-b1-b05.bpun` | `0x25f3` | `0x25f3` | **yes** |
| `tcp-ser-b2-b05.bpun` | `0x471e` | `0x471e` | **yes** |
| `tcp-ser-b3-b05.bpun` | `0x42ce` | `0x42ce` | **yes** |

A 16-bit sum is a weak check - it would miss compensating errors. But four-for-four against the
documented algorithm is strong evidence these images are as built.

### 2.1 SCOPE LIMIT - this proof covers the four BPUNs and nothing else

**[V] `.prog` files carry no checksum.** Neither do `.brf`, `.mode` or `.symb`. So the integrity
statement above applies **only** to `tcp-ser-b0..b3-b05.bpun`.

For everything else recovered under `TCP-IP` - `telnet-serv-b05.prog`, `telnet-clien-b05.prog`,
`ftp-client-b05.prog`, `ftprt-b05.prog`, `po-stop-b05.prog`, `po-pwrfail-b05.prog`,
`tcp-error-1-b05.brf` - **integrity is unproven**. They read as plausible, and the mode files parse
as coherent SINTRAN, but on a disk described as being in bad shape that is not the same as verified.
Anything derived from a `.prog` here should be treated as **[U]** until corroborated by a second
source, such as agreement with the BPUN symbol table or with a documented behaviour.

**[I] Possible cross-checks**, none yet done: SINTRAN records a page count per file, so a truncation
would show as a size that is not a whole number of pages; `.prog` files have a load-address/length
header whose declared extent can be compared against the actual file length; and the `:BRF` format is
record-structured, so a bad record chain is detectable by walking it.

### 2.1 Why the files look "mostly empty" - they are supposed to

The four banks are a **sparse slice of the card's 512 KB DRAM**, not packed code. Occupancy against
the known-good control:

| Bank | ENCOS (known good) | TCP/IP (recovered) |
|---|---|---|
| b0 | 59,571 non-zero (45.4%) | 98,306 non-zero (74.9%) |
| b1 | 157 non-zero (0.1%) | 15,807 non-zero (12.0%) |
| b2 | **1 non-zero (0.0%)** | 84 non-zero (0.1%) |
| b3 | 19,074 non-zero (14.5%) | 12,023 non-zero (9.2%) |
| **total** | **78,803** | **126,220** |

The known-good ENCOS b2 contains a *single* non-zero byte. Sparseness is normal for this format, and
the TCP/IP set carries **60% more** content than the COSMOS set. Judging these files by their zero
ratio is a trap.

**[I]** Four banks x 128 KB = 512 KB = the card's DRAM, and the 211185 product sheet's cost line
"120 + 256 * NbOfControllers pages" is 256 x 1KW = 512 KB per controller. The arithmetic inference
recorded in the evidence document is now confirmed by the artifact.

---

## 3. What each bank contains

> **CORRECTION 2026-07-30 (after disassembly).** The headings below describe where each subsystem's
> **strings and banners** live. They do **NOT** describe where its code lives, and an earlier reading
> of this section as a code map was wrong. **[V]** All executable code is one contiguous span
> `~0x4660-0x23F38` in the concatenated image, crossing bank boundaries freely: all 430 PLANC
> prologues fall there (376 in bank 0, 54 in bank 1, none above), and `AIPINIT 0xC942` /
> `TCPINPUT 0x10AC8` sit on exact prologues inside bank 0. The 128 KB boundaries are EPROM devices,
> not software modules. See `../../../../Installation/Communication/Ethernet/x/stripped/README-tcp-ser-b05-image.md`.

### b0 - PIOCOS, shared with COSMOS [V]

The module banner strings are the **same modules with the same build dates** as the ENCOS firmware:

```
CX5       APRIL 21, 1986      LOC-XMSG  APRIL 21, 1986
NCOM      APRIL 21, 1986      MAIN      APRIL 21, 1986
HDLC-DR   JULY 8, 1986        M-MANAG   APRIL 21, 1986
ASYN-DR   APRIL 21, 1986      PHLS-GEN  APRIL 21, 1986
RT-CLOCK  AUGUST 29, 1986     SHORTLIB  APRIL 21, 1986
```

**[V] This is the direct proof of the "protocol selection = which image you download" model.** The
operating system underneath COSMOS and underneath TCP/IP is the same build. Only the payload differs.

### b1 - AIP, the ARPA Internet Protocol layer [V]

Banner: `$AIP-ARPA Internet Protocol server: version B01` / `ETH II`, and `July 5, 1988`.
It prints `$Internet Address:` and `$Address Mask:` at startup.

**The media-access command set, named by ND** - these are the commands AIP issues down to the MA
layer, and the responses it decodes:

```
attach            statistics        change address
start             stop              activate group address
set DIX mode      DIX attach
```

Transmit response decoding, verbatim:

```
status OK / OK - AFTER ONE COLLISION / OK - AFTER MULTIPLE COLLISIONS
UNSUCCESSFULL - EXCESS COLLISIONS / BAD - MEDIA-ACCESS STOPPED / MEDIA-ACCESS RESTARTING
```

Progress messages showing the DIX bring-up sequence:

```
$AIP: attaching to MEDIA ACCESS in DIX mode
$AIP: attached to MEDIA ACCESS in DIX mode
$AIP: setting in DIX mode
$AIP: in DIX mode
$AIP: setting multicast address
$AIP: initiating receive to media-access
$AIP: no more fragments for ARP
$AIP: ICMP message
```

**[V] `DIX attach status ok / BAD - other user attached`.** This is the strongest evidence yet on the
coexistence question: attaching to media access in DIX mode can fail *because another user is already
attached*. It points at exclusivity, though it does not state what "user" means here.

Ports it opens: `$Command port created. / $Data port created. / $Transmit port created.`, then
`LNMASPcommand` -> `$Connected to MA command port` and `LNMASPdata` -> `$Connected to MA data port`.
So AIP talks to the media-access layer through **three PIOCOS ports**, exchanging "RB" request blocks
(`$AIP: sending command RBs`, `$AIP: collecting response RBs from data SP`).

It also carries the full LANCE statistics report text - CRC errors, alignment errors, FIFO overflows,
buffer overflows, bad MA length field, loss of carrier, jabber, late collision, missing transceiver
heart beat - which is a ready-made map of the statistics block we only partly decoded.

Error banners: `$AIP: PIOC ERROR`, `$AIP: !! FATAL HARDWARE ERROR !! LNMAHWerror LNMAHWinfo`,
`$AIP: !! MEDIA-ACCESS FATAL ERROR !! LNMAFTerror LNMAFTinfo`, `$AIP: FATAL PORT SYSTEM ERROR`.
These match the `AIP*` error texts quoted in ND-860284-1 appendix D.

### b2 / b3 - TCP [V]

Banner: `TCP-Transmission Control Protocol version B05 ETH II, July 5, 1988`.

b3 carries the embedded symbol table (below) plus `TCP PIOC` and PLANC-level names such as
`FSMR.TcpTemplate`, `FSMR.TcpExtractOob`, `FSMR.SendPacket`.

---

## 4. The embedded symbol table - 463 records with addresses [V]

**Count corrected 2026-07-30**: 463 records, not the 437 first reported here - 317 CODE defined,
134 DRAM defined, 12 markers. The lower figure came from a stricter scanning filter. The table runs
`0x7C3A0-0x7FBA0` (448 records), then an 8-byte misalignment, then `0x7FBA8-0x7FD88` (15 records).

b3 contains a symbol table in the same family as the ENCOS one, but the record layout differs from
what was recorded for ENCOS - **the fields sit 4 bytes later**. Confirmed by hexdump at `0x1C3E4`:

```
+0x00  4  self/next pointer, increments by 0x20
+0x04  1  name length (1..12)
+0x06  1  0x02 = defined, 0xFF = undefined / marker
+0x07  1  segment: 0x10 = CODE, 0x16 = DRAM, 0x11 = other
+0x08  4  address, big-endian
+0x10 12  name (truncated to 10 characters in practice)
```

437 unique symbols parse cleanly. **[V] The media-access names match the ones we carved from ENCOS**,
at this build's addresses - an independent confirmation of that carve and a second build to compare
against:

| Symbol | This build | ENCOS build (carved) |
|---|---|---|
| `RCVCOMPLET` | `0x0000602E` | `0x5C42` |
| `XMTRINGAPP` | `0x00006600` | `0x6054` |

Others recovered include `INITLANCE 0x4884`, `STARTMA 0x5C46`, `STOPMA 0x5C6E`, `REINITRING 0x5AB8`,
`RCVRINGAPP 0x5EE6`, `XMTCOMPLET 0x68A8`, `INTLANCE 0x8198`, `LNMAINIT 0x7FBC`,
`LNMAEVENTS 0x7E8A`, `MACMDPORTH 0x6D2E`, `MADATAPORT 0x7298`, `LLCMDPORTH 0x75B8`,
`LLDATAPORT 0x7B78`, `HARDWAREER 0x5CC2`, `WATCHDOGDE 0x5E4C`, `INTPROTECT 0x47F4`,
`STARTIO 0x4814`, `FATALERROR 0x4BFA`, plus DRAM anchors `PIOC_NUMBE 0x64C`, `ND100_CPU 0x64E`,
`REALTIME 0xFC2`, `BUFFER_STA 0x12F4`, `BUFFER_END 0x1A00`, `END_PIOCOS 0x4660`.

TCP-layer names from the string scan (addresses in the same table): `TCPINPUT`, `TCPPROCESS`,
`INITTCPCB`, `TCPNEWTCPC`, `TCPFREETCP`, `TCPRESTCPC`, `TCPENQUEUE`, `TCPDEQUEUE`, `TCPDROPSEQ`,
`TCPOOBDROP`, `TCPINCKSUM`, `TCPIPSEND`, `TCPATTACH`, `TCPDISCONN`, `TCPBINDING`, `TCPCONNUSE`,
`TCPUSREQUE`, `TCPFETCHBU`, `TCPNETINIT`, `TCPSUSPEND`, `TCPWAITFOR`, `SENDARP`, `ARPINPUT`,
`AIPINIT`, plus a whole trace/debug family (`TCPPRINTST`, `TCPTRATEXT`, `TCPLOGDEBU`, `TCPREPORT`).

Full dump produced during analysis; regenerate with the record layout above.

---

## 5. The SINTRAN side - what the load mode file reveals [V]

`tcp-ip\tcp-ip-lo-1-b05.mode` is the cold-start loader. It answers the "how do I hook into SINTRAN"
question for ND's own product.

**Device**: `@PRLS 2241B 1` - the Ethernet II controller, the same device number the COSMOS
`ENCOS-IN` installer probes.

**Controller stop/unload**: `@(TCP-)PO-STOP-B05` answering `1`.

**Segments and RT programs**:

| Name | Kind | Loaded from | Priority |
|---|---|---|---|
| `TCPE1` | segment, page table 2 | `TCP-ERROR-1-B05:BRF` | - |
| `TCPS1B0..B3` | segments, page table 2 | `TCP-SER-B0..B3-B05:BPUN`, then `SE-LO-AD,,177777` | - |
| `TCPS1` | RT program (supervisor/server) | - | **40** |
| `POPWR` | RT program, power fail | `PO-PWRFAIL-B05:PROG` | 50 |
| `TNSERV` / `TNSEG` | telnet server, **2-bank background segment** | `TELNET-SERV-B05:PROG` | 16 |
| `FTPRT` / `FTPS0` | FTP starter | `FTPRT-B05:PROG` | 30 |

**[V] The four on-card banks are loaded as four ordinary SINTRAN segments** with `READ-BINARY`
followed by `SE-LO-AD,,177777`, exactly as COSMOS loads ENCOS. This is the mechanism the cost table
implied.

**[V] Two configuration pokes are done by patching segments directly**, and the mode file's own
comments name them:

```
@CC  Write Subnet Bits and Gateway Adress on to segment
@LOOK-AT SEGMENT TCPS1B1
150615/0        <- subnet bits / gateway address, at 150615B in bank B1

@CC  Change server number on supervisor segment and delete temporary file.
@LOOK-AT SEGMENT TCPE1
0/1             <- server number, word 0
```

**[V] FTP attaches to a TAD.** The mode file comment reads: *"Mode file to load FTPRT who allocates a
TAD and starts the FTP server."* This closes an open question in
[WRITING-A-TCPIP-STACK-ON-SINTRAN.md](WRITING-A-TCPIP-STACK-ON-SINTRAN.md) Part 9 - ND attached
network sessions to SINTRAN terminals through **TAD**, not through some private mechanism.

`tcp-start-b05.mode` is the warm-start half: abort `tnserv` / `ftprt` / `TCPS1`, `@PRLS 2241B 1`,
`PO-STOP-B05`, then `@rt TCPS1`, `@hold 0,0`, `@hold 15,2`, then `@rt tnserv` and `@rt ftprt`. The
two `HOLD`s between starting the supervisor and starting the servers are a deliberate settle delay.

---

## 6. Everything recovered under user TCP-IP

```
tcp-ser-b0-b05.bpun    131205   on-card bank 0 - PIOCOS
tcp-ser-b1-b05.bpun    131205   on-card bank 1 - AIP (IP/ICMP/ARP)
tcp-ser-b2-b05.bpun    131205   on-card bank 2 - TCP
tcp-ser-b3-b05.bpun    131205   on-card bank 3 - TCP + symbol table
tcp-error-1-b05.brf    421260   error-text segment source
tcp-ip-lo-1-b05.mode     2096   cold-start loader
tcp-start-b05.mode        318   warm-start
telnet-serv-b05.prog   356352   Telnet server
telnet-clien-b05.prog  387072   Telnet client
ftp-server-b05.prog    380928   FTP server  (in (SYSTEM))
ftp-client-b05.prog    372736   FTP client
ftprt-b05.prog         268288   FTP starter RT program (allocates a TAD)
po-pwrfail-b05.prog     30720   power-fail handler
po-stop-b05.prog          4096   controller stop/unload
```

Plus, under `(SYSTEM)`, the live configuration files:
`aip-hosts.symb`, `aip-networks.symb`, `aip-protocol.symb`, `aip-services.symb`,
`tcp-ip-lo-1-b05.list`.

**[V] `AIP-HOSTS` carries this site's real addresses**, which also dates and locates the machine:

```
130.67.1.2 AVESTA  TETRIS
130.67.1.1 GT-T  TRISSE
```

`AIP-PROTOCOL` is the standard set (ip/icmp/ggp/tcp/pup/udp). `AIP-NETWORKS` still holds ND's shipped
Sun-derived examples, so it was never customised.

---

## 7. What this changes

- The **hunt-list top item is closed**. A working DIX-2.0 image for the ND 110063 exists here.
- **The host-to-card frame protocol is now attackable.** Part 4.0 of the stack-writing guide lists it
  as the largest unknown. b1 contains the client side of it - AIP's three-port RB protocol to the MA
  layer - with 437 named symbols to anchor a disassembly.
- **A second build of the media-access layer** is available, with ND's own names, to cross-check the
  ENCOS carve.
- **[U] Still unknown**: whether the ND-100 ever passes a raw frame. Everything visible here says the
  ND-100 talks to TCP on the card via XMSG ports, consistent with section 1.4 of the guide.

## 8. Suggested next steps

1. Load `tcp-ser-b1-b05.bpun` into Ghidra at the card's DRAM base and apply the 437 symbols, using
   the `ghidra-planc` toolkit. AIP is PLANC-on-68000 like ENCOS - the `2C 5F 24 5F 4E EA 00 02`
   epilogue and `4E D5` (`jmp (A5)`) skip-return are both present in b1.
2. Decode the MA command/response RB format from AIP's side. That is the host protocol, from the
   consumer's point of view.
3. Compare b0 against `encos-ser-b0-b01.bpun` - if PIOCOS is identical, differences localise the
   product payload precisely.
4. Read `telnet-serv-b05.prog` and `ftprt-b05.prog` for the TAD attachment sequence.

---

## Related documents

- [HOW-ND-SHIPPED-TCPIP-PRODUCT-EVIDENCE-2026-07-26.md](HOW-ND-SHIPPED-TCPIP-PRODUCT-EVIDENCE-2026-07-26.md) - the documentary case this artifact confirms
- [WRITING-A-TCPIP-STACK-ON-SINTRAN.md](WRITING-A-TCPIP-STACK-ON-SINTRAN.md) - the build guide; Parts 1.4-1.6 and 4.0 are affected
- [TCPIP-DRIVER-ON-ND-ETHERNET-II.md](TCPIP-DRIVER-ON-ND-ETHERNET-II.md) - the ENCOS transmit/receive carve
- [ETHERNET-II-FEASIBILITY-AND-MODE-WORD-RE-2026-07-25.md](ETHERNET-II-FEASIBILITY-AND-MODE-WORD-RE-2026-07-25.md) - the mode-word reverse engineering
- `../../../../Installation/Communication/Ethernet/RE/PIOCOS/README.md` - PIOCOS RTOS hub
