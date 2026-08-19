# Which firmware serves `*ENUM0`? — Ethernet controller firmware identity

Date: 2026-08-10
Author: static file inspection only (no Ghidra, no live machine)

## The hypothesis being tested

The ND-500 machine, through the NDIX Unix port (repo `nd500x` / source in `NDIX-C`),
talks to an Ethernet controller over XMSG by attaching to a media server named `*ENUM0`.
Separately, the COSMOS ENCOS Ethernet II firmware we have decoded serves a DIFFERENT
server named `*XM-ENNS0`. The question: which firmware actually serves `*ENUM0` — the
ENCOS firmware (some bank/mode we missed), the ND TCP/IP firmware (product 211185), a
firmware present on disk but not yet analysed, or a firmware we do not have at all.

## Method

- `grep -a` / `grep -aoE` over every raw firmware image (`.bin`, `.bpun`, `.brf`,
  `.prog`, `.dseg`) under the Ethernet install tree.
- Read of the NDIX C source in `E:\Dev\Ronny\NDIX-C\kernel\MASTER\if\`.
- No Ghidra, no emulator, no live machine. Static inspection only.

---

## Table: firmware image -> server name(s) found

Command used per file:
`grep -aoE "\*?(ENUM|ENNS|XM-ENNS|TCP)[A-Z0-9-]*" <file> | sort | uniq -c`

| Firmware image (full path) | `*ENUM0` | `*XM-ENNS0` | `*TCP` | Notes |
|---|---|---|---|---|
| `E:\Dev\Ronny\NDInsight\Installation\Communication\Ethernet\x\stripped\encos-ser-all-banks-68k.bin` | NO | **YES (1)** | no | ENCOS 512KB image (4 DRAM banks) |
| `...\x\stripped\encos-ser-b0-68k.bin` | NO | no | no | |
| `...\x\stripped\encos-ser-b1-68k.bin` | NO | **YES (1)** | no | the name lives in bank 1 |
| `...\x\stripped\encos-ser-b2-68k.bin` | NO | no | no | |
| `...\x\stripped\encos-ser-b3-68k.bin` | NO | no | no | |
| `...\x\encos-ser-b1-b01.bpun` | NO | **YES (1)** | no | raw on-card BPUN, bank 1 |
| `...\x\encos-ser-i-b01.dseg` | NO | **YES (1)** | no | |
| `...\x\encos-mon-i-b01.prog` | NO | **YES (ENNS0..3)** | no | SINTRAN-side loader; names all 4 cards |
| `...\x\encos-mon-ii-b01.prog` | NO | **YES (ENNS0..3)** | no | SINTRAN-side loader |
| `...\x\encos-in-b01.prog` | NO | `ENNS0` (bare) | no | |
| `...\x\encos-err-i-b01.brf` / `encos-err-ii-b01.brf` | NO | `ENNS0` (bare) | no | |
| `...\x\stripped\tcp-ser-all-banks-b05-68k.bin` | **NO** | no | **YES `*TCP` (1)** | TCP/IP 211185 B05 512KB image |
| `...\x\stripped\tcp-ser-b3-b05-68k.bin` | NO | no | **YES `*TCP`** | host seam name lives in bank 3 |
| `...\x\stripped\tcp-ser-b0/b1/b2-b05-68k.bin` | NO | no | (bare `TCP` only) | |

**No firmware image on disk contains the string `*ENUM0` (or `ENUM` at all).**
The only server names present in any ND Ethernet controller image we hold are
`*XM-ENNS0` (ENCOS) and `*TCP` (TCP/IP 211185).

### VERIFIED evidence for the ENCOS binary (the anchor fact)

File: `E:\Dev\Ronny\NDInsight\Installation\Communication\Ethernet\x\stripped\encos-ser-all-banks-68k.bin`
(524288 bytes = 512KB, confirmed by `ls -l`).

`grep -aoE "\*(ENUM|ENNS|XM-ENNS)[A-Z0-9-]*"` returns exactly one line: `*XM-ENNS0`.
`grep -aoE "E(NUM|NNS)[A-Z0-9-]*"` returns exactly one line: `ENNS0`.
No `ENUM` substring exists anywhere in the image. VERIFIED.

---

## NDIX attach evidence — what name NDIX actually asks for

Source dir: `E:\Dev\Ronny\NDIX-C\kernel\MASTER\if\`

### The name is built, not stored literally — `*ENUMi` with `i` = unit number

`E:\Dev\Ronny\NDIX-C\kernel\MASTER\if\if_etregs.h`, lines 55-56 (VERIFIED):

```
#define ET_NAM  "*ENUMi"
#define SZ_NAM  (sizeof(ET_NAM) - 1)
```

So `ET_NAM` is the 6-character template `*ENUMi` and `SZ_NAM` = 6.

`E:\Dev\Ronny\NDIX-C\kernel\MASTER\if\if_et.c`, in `etinit()` (VERIFIED, lines 375-376):

```
bcopy(ET_NAM, port, SZ_NAM);
port[SZ_NAM - 1] = '0' + unit;          /* "*ENUMi" */
```

The last character (`i`) is overwritten with `'0' + unit`. For the first/only
controller (unit 0) the name becomes exactly **`*ENUM0`**. VERIFIED.

The same construction appears three times in the file for the three operations:
- detach request build (lines 303-304),
- attach request build (lines 375-376),
- the down/detach path (line 1331).

### How NDIX attaches (the handshake)

In `etinit()` (VERIFIED from `if_et.c`):

1. Opens/keeps an XMSG port (`XFOPN`, line 341; port number saved in `es->es_portno`).
2. Builds an XROUT **letter** with an XMSG header of subtype `XSLET` and one parameter
   block carrying the server name `*ENUM0` (`xmh(&xh,0,XSLET,SZ_NAM+2)` line 370;
   the name is copied into the letter at line 378).
3. Appends an "Attach To Server" request block (`struct ac_areq`) whose header type is
   `aq.EXMHDtype = EXMTYattach` (line 357), carrying the 6-byte Ethernet address
   (`bcopy(es->es_addr, aq.EXMHDaddress, 6)`, line 360).
4. Writes the letter (`XFWRI`, line 388) then sends it to XROUT
   (`XFSND|XFROU`, line 393).
5. Waits for the reply on `XFRRE|XFWTF` (line 398) and checks the reply header type is
   `EXMTYstatus` (line 404).

So NDIX reaches the controller's media server purely as an **XMSG/XROUT named-server
lookup** ("attach to the server called `*ENUM0`"). It does not use raw shared memory for
this seam. The request-block shape (`EXMTYattach` / `EXMTYdetach` / `EXMTYstatus`,
`EXMHDaddress`) is defined in `E:\Dev\Ronny\NDIX-C\kernel\MASTER\if\if_access.h`.

### Framing: NDIX expects DIX / Ethernet-II, not 802.3

VERIFIED from `if_et.c` and `E:\Dev\Ronny\NDIX-C\kernel\MASTER\netinet\if_ether.h`:

- It switches on the 16-bit `ether_type` field: `ETHERTYPE_IP` (0x0800),
  `ETHERTYPE_ARP` (0x0806), and trailer types `ETHERTYPE_TRAIL` (0x1000)
  (`if_et.c` lines 691-697, 735, 740; values in `if_ether.h` lines 23-31). A live
  16-bit EtherType field in the header is the DIX / Ethernet-II format, not the 802.3
  length+LLC format.
- Trailer protocol handling (`ETHERTYPE_TRAIL .. TRAIL+NTRAILER`) is present but marked
  in the code as "minimal functionality and not used by anyone" (line 695). Inbound
  trailer packets are decoded; the transmit path just pads.
- Minimum frame: `ETHERMIN` = `(60-14)` = 46 payload bytes (`if_ether.h` line 35),
  i.e. the standard DIX 60-byte-minimum frame after the 14-byte header. `etoutput()`
  pads short frames up to this minimum (`if_et.c` lines 954-955). This is the standard
  DIX minimum, NOT a 58-byte value. (The "58-byte minimum" figure in the task brief is
  NOT what the NDIX source shows; I did not find 58 anywhere.)

### XFWAK bit — a KNOWN, already-settled vintage difference (not re-litigated)

`E:\Dev\Ronny\NDIX-C\kernel\MASTER\if\xmsg.h` line 112:
`#define XFWAK 0x8000` (used in `if_et.c` lines 486, 758 as `XFRREN|XFWAK|XFRMR`).
NDIX therefore uses bit 15 (0x8000) for XFWAK, whereas official ND / ENCOS uses bit 14
(0x4000). This is the previously-settled difference — noted here only as a signal that
NDIX was built against a **different / older XMSG option-bit convention** than the ENCOS
firmware we hold. Not re-analysed.

---

## Conclusion

**VERIFIED facts:**

1. NDIX attaches to a server whose name is built as `*ENUM0` (template `*ENUMi` in
   `if_etregs.h`, last char set to `'0'+unit` in `if_et.c` `etinit()`). It reaches it as
   an XMSG/XROUT named-server "attach" (`EXMTYattach`) and expects a DIX / Ethernet-II
   framed interface (live 16-bit EtherType, IP/ARP, DIX 60-byte minimum).
2. The ENCOS Ethernet II firmware serves `*XM-ENNS0` and contains no `ENUM` string at
   all — confirmed on the 512KB all-banks image, the per-bank images, and the raw BPUN.
3. The ND TCP/IP 211185 B05 firmware serves `*TCP` (host seam name in bank 3) and
   contains no `ENUM` string.
4. No firmware image we hold under the Ethernet install tree contains `*ENUM0` /
   `ENUM`. The only controller server names present on disk are `*XM-ENNS0` and `*TCP`.

**Answer to the hypothesis — option (d), with high confidence:**

`*ENUM0` is served by a firmware we do NOT have on disk. It is **not** the ENCOS
firmware (that serves `*XM-ENNS0`, and has no `ENUM` bytes in any bank or mode we can
see), and it is **not** the TCP/IP 211185 firmware (that serves `*TCP`). Ruling out (a)
and (b) is VERIFIED by the byte evidence above. Ruling out (c) — "present on disk but not
yet analysed" — is VERIFIED to the extent of the files under the Ethernet install tree:
none of them contain the string. So the controller NDIX was written to talk to ran a
third, distinct media firmware that registered the XMSG server `*ENUM0`.

**SPECULATION (clearly marked, not verified):**

- The three server-name families look like three different products/vintages of the same
  ND-110063 Ethernet II board: `*XM-ENNS<n>` (ENCOS / COSMOS 802.3), `*TCP` (211185
  TCP/IP DIX gateway), and `*ENUM<n>` (the NDIX/ND-500-Unix media server). This is
  consistent with the memory note that PIOCOS is byte-identical across ENCOS and TCP
  builds ("same OS, different payload"), suggesting `*ENUM0` is yet another payload on
  the same PIOC-OS base. I have NOT verified this — it is a plausible pattern, not a
  proven fact.
- The `*ENUM` name plus DIX framing plus the XFWAK=0x8000 (older bit) convention suggests
  the NDIX media firmware is an ND-internal Unix-project build that predates or forks the
  COSMOS ENCOS line. UNVERIFIED.

**What would settle it:** obtain the on-card firmware image that came with the ND-500 /
NDIX Unix distribution (the media server that registers `*ENUM0`). Search the NDIX
distribution media / root images (`E:\Dev\Ronny\NDIX-C\rootfs_dev*.img` and any ND-500
Unix floppy/HDD dumps) for a downloadable controller image and grep it for `ENUM`. That
image is the missing firmware; none of the COSMOS Ethernet install files are it.
