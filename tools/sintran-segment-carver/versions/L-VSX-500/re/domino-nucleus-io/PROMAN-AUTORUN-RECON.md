# PROMAN AUTO-RUN RECON (S0-4, SIN-F9) - 2026-07-20

Question (SIN-F9, was [UNVERIFIED]): does THIS SINTRAN L07 image auto-run PROMAN
(the ND-100 RT program that boots DOMINO controllers over the octobus) when a
DOMINO device is generated/configured - i.e. would an emulated SCSI DIOC station
at 10B-13B receive EchoTest/IdentY/SetBxP/BxDoLd/RegMod/Go-On boot-protocol
traffic?

SHORT ANSWER: **(a) SAFE. PROMAN exists on this image but does NOT run at boot.**
Live evidence: after a full harness boot (XMSG started, XROUT/XMFIDO demonstrably
running), PROMAN is PASSIVE with P-REG = 0B - it has never executed a single
instruction. No PMA-CONFIG file and no PMA-* image files exist on the pack.
Even if manually started, PROMAN discovers DOMINO controllers via MF-bus
controllers (crate interrogation) + the (SYSTEM)PMA-CONFIG file, not by blind
station probing. Details and tags below.

Evidence tags: [V] = byte/live-output verified, [NPL-V] = verified in NPL source
listing (logic only), [MAN] = manual statement (NOT proof about this image),
[I] = inference, [OPEN] = unresolved.

---

## 1. Segment identity (strings = decisive)

Catalog (`../../segments/README.md` + meta.json, all load address 30000B = 0x3000,
52 pages, ND-100 big-endian):

| Seg | Name | meta description |
|-----|------|------------------|
| 120 | S3SPRMA | Save of Processor Manager server |
| 121 | S3IPRMA | Image of Processor Manager server |
| 124 | S3SBOPC | Save of Bopcom Server |
| 125 | S3IBOPC | Image of Bopcom Server |

[V] ASCII strings prove 120/121 = PROMAN and 124/125 = BOPCOM. Save/image twins
carry identical strings at identical offsets (sibling coherence). Reproduce
(byte offsets into the .bin; Git Bash, from `versions/L-VSX-500/segments/`):

```
dd if=121-S3IPRMA.bin bs=1 skip=26146 count=14 2>/dev/null   # "PROMAN started"
dd if=120-S3SPRMA.bin bs=1 skip=26146 count=14 2>/dev/null   # same in save twin
dd if=121-S3IPRMA.bin bs=1 skip=72682 count=18 2>/dev/null   # "(SYSTEM)PMA-CONFIG"
dd if=121-S3IPRMA.bin bs=1 skip=25680 count=27 2>/dev/null   # "Impossible to get echo-test"
dd if=121-S3IPRMA.bin bs=1 skip=26354 count=19 2>/dev/null   # "THIS IS AN ECHOTEST"
dd if=125-S3IBOPC.bin bs=1 skip=20787 count=13 2>/dev/null   # "Bopcom-Server"
dd if=124-S3SBOPC.bin bs=1 skip=20787 count=13 2>/dev/null   # same in save twin
```

Other identity strings seen in 121 ([V], `strings -n 5`): "(SYSTEM)PMA-ERS-BUFFER",
"(UTILITY)PMA-" (boot-image filename prefix), "PMA-EVENT-LOG", "PMAservicePort",
"PMAersGateWay", "PMAhomePort", the whole boot-session error ladder ("Impossible
to get ident from controlller" / "...stop controller" / "...set mailbox for
controller" / "...download block to controller" / "...set start address in
controller" / "...start program in controller" - exactly the EchoTest/IdentY/
Stop/SetBxP/BxDoLd/RegMod/Go-On sequence of manual section 2.6.2), the Domino
session state names "DomIdle ComTest Ident Dstop Mbox Load Setup Ready Running
Check Pwrfail", and the hardware-module-type table "VMEI...IPI3 SCSI ETH3 FPS5
TERM GRAP MFCC VMEC DMAC..." (manual table 1 module-number -> image-name lookup;
"SCSI" at byte offset 76228 in 121). Also "No controllers found in system",
"This ND-100 is not master in system".

CAUTION: the `strings` in PATH is Sysinternals strings.exe - its two banner
lines ("Strings v2.54...", "Copyright (C) ... Mark Russinovich") appear in the
output and are NOT segment content.

---

## 2. Activation analysis

### 2.1 RT-descriptions exist [NPL-V + V]

NPL assembly listing `SINTRAN/NPL-SOURCE/s3vs-4.symb` lines 1179-1226: system
RT-program descriptions NKSER, NKNAM, ERSWD, PROMA, EVMES, BOPCO, MTSER. The
five servers NKSER/PROMA/EVMES/BOPCO/MTSER all share the same start address
symbol SPSRS = 030020 (offset 20B into each server's own 30000B-based segment)
and each names its own segment (PROMA -> 5PROM = segment 121):

```
PROMA,0;2;36;36;0;0;0;0;SPSRS;5PROM;0
```

L07 addresses (SYMBOLS/L07/SYMBOL-2-LIST.SYMB.TXT): NKSER=014513, NKNAM=014541,
PROMA=014615, EVMES=014643, BOPCO=014671, MTSER=014717 (octal; PROMA = 0x198D).

[V] The resident RT-loader area holds the RT name table with these description
addresses: `resident/MACM-AREA-DATA_rtloader.bin` byte 0x1ad00-0x1ad07 =
`0004 123c d04e 198d` (packed name + desc addr 0x198D = PROMA). NKSER 0x194B at
0x1acee, EVMES 0x19A3 at 0x1ad0e, BOPCO 0x19B9 at 0x1ad16, MTSER 0x19CF at
0x1ad1e. Same table image inside segment 037-S3RTD at byte 0x34ee-0x351f and an
8-word-per-entry variant in 010-S3RTFIL at bytes 0x300-0x36f.

```
dd if=../../resident/MACM-AREA-DATA_rtloader.bin bs=1 skip=109824 count=8 2>/dev/null | od -A n -t x1
# skip=109824 = 0x1ad00; expect: 00 04 12 3c d0 4e 19 8d  (PROMA name-table entry, desc addr 0x198D)
```

### 2.2 No kernel-side auto-start in the NPL tree [NPL-V]

Exhaustive grep of `SINTRAN/NPL-SOURCE/NPL/*.NPL` for PROMA/SPSRS/5PROM finds
ONLY: (a) the RT-description table itself, (b) OPPSTART's cold-start save->image
segment copy ("COPY PROCESSOR MANAGER SERVER", PH-P2-OPPSTART.NPL lines 828-829),
(c) CLEALLSEGS marking 5PROMAN/5BOPC "segment not OK" (RP-P2-MONCALLS.NPL 322-323).
OCSTART (octobus start-up) and NUCST (NUCLEUS start-up) in PH-P2-OPPSTART only
allocate buffer pools/mailboxes - neither starts any RT program. There is NO
RTON/activation of PROMA anywhere in the available NPL tree. (Known caveat: the
NPL tree has gaps; the server-start code is evidently in a module we do not have
in source.)

### 2.3 A server-start table exists in the command processor [V, consumer OPEN]

Segments 003-S3CP (byte 0xbb60) and 013-S3SCP (byte 0xc360, identical twin)
contain a word table pairing exactly the six server RT-description addresses
with a flag word, terminated by 0x8dce/0x8002:

```
194b 0002  1961 0000  198d 0002  19a3 0000  19b9 0002  19cf 0002  8dce 8002
NKSERV,2   NKNAME,0   PROMAN,2   EVMESG,0   BOPCOM,2   MTSERV,2   (end)
```

Reproduce:
```
dd if=003-S3CP.bin  bs=1 skip=47968 count=28 2>/dev/null | od -A n -t x1   # 0xbb60
dd if=013-S3SCP.bin bs=1 skip=50016 count=28 2>/dev/null | od -A n -t x1   # 0xc360
```

[I] This is consistent with the manual's claim that "during start-up of SINTRAN,
the servers are started by SINTRAN itself" (ND-820026-1c line 5474, about the
NUCLEUS servers; ND-820026.1 line 917: PROMAN "is started immediately after
system start"). [OPEN] Which code walks this table, and what the flag words
(2/0) and the gating condition mean, is not carved - it would take a disasm of
the surrounding 013-S3SCP code. Not needed for the harness answer, because:

### 2.4 Live activation state: PROMAN has NEVER run on this image [V - decisive]

`inputs/list-rt-programs.txt` (live `@LIST-RT-PROGRAMS` on THIS L07 system,
captured after XMSG was started):

```
  NKSERV  14513B     30 PASSIVE      0B   0B  0B
  NKNAME  14541B     30 PASSIVE      0B   0B  0B
  PROMAN  14615B     30 PASSIVE      0B   0B  0B
  EVMESG  14643B     30 PASSIVE      0B   0B  0B
  BOPCOM  14671B     30 PASSIVE      0B   0B  0B
  MTSERV  14717B     30 PASSIVE      0B   0B  0B
```

P-REG = 0B means never executed. Calibration in the SAME listing: XROUT is
IO-WAIT with P=2631B and XMFIDO is READY with P=114427B - servers that DID start
show nonzero P. So whatever the CP start table is for, it did NOT start PROMAN
(nor NKSERV/BOPCOM) on this boot. The RT-desc addresses in the listing match the
L07 symbols exactly (14615B = 0x198D = PROMA), tying the listing to this image.

### 2.5 No PROMAN runtime files on the pack [V]

`ndtool -t` full listing of `F:\ND\SINTRAN-L - 2026\HDD\BIGDISK0-L.IMG`
(PACK-ONE, 70 files, all users listed): there is NO (SYSTEM)PMA-CONFIG:DATA,
NO (SYSTEM)PMA-ERS-BUFFER:DATA, NO (UTILITY)PMA-*:* boot images, and no DOMINO
Maintenance Kit files (PMA-MONITOR etc.). The DOMINO software kit was never
installed on this pack.

---

## 3. What triggers PROMAN traffic toward a station [MAN + strings-V]

Per manual section 2.6.1 (algorithm) corroborated by the segment's own strings:
PROMAN, once running, (1) checks it is on the right configuration (ND-5000) and
that this ND-100 is master, (2) initializes octobus + NUCLEUS communication,
(3) discovers configuration by finding all MF-bus controllers (one per card
crate) and asking EACH MF-bus controller for its crate configuration
(investigate-bank / list-configuration), (4) overlays that with the
(SYSTEM)PMA-CONFIG file, then (5) starts one boot-thread per DISCOVERED
DOMINO controller. Boot-protocol frames (EchoTest, IdentY, Stop, SetBxP,
BxDoLd, RegMod, Go-On) go only to controllers found in step 3/4.

[I] Therefore a DIOC station at 10B-13B receives PROMAN traffic only if an
MF-bus controller reports it during crate interrogation (or PMA-CONFIG names
it). PROMAN does not blind-probe octobus stations. Byte-level confirmation of
the discovery addressing would need a live trace of a started PROMAN - not
possible statically and NOT needed for the current harness question given 2.4.

This is fully consistent with the already-carved bring-up truth
(SINTRAN-OCTOBUS-MESSAGE-CATALOG.md sections 6.1-6.3): SINTRAN's own bring-up
sends MFPREPARE to stations 2-6 only and NOTHING to 10B-13B.

---

## 4. Three-way answer for the RetroCore boot harness

**(a) SAFE to configure an emulated SCSI DIOC station at 10B-13B: no PROMAN
boot-protocol traffic will arrive on this image as currently configured.** [V]

Grounds (in strength order):
1. [V] Live `@LIST-RT-PROGRAMS` on this image after full boot: PROMAN PASSIVE,
   P-REG=0B (never ran). Same for NKSERV/BOPCOM/EVMESG/MTSERV.
2. [V] The pack has no PMA-CONFIG, no PMA-* images, no DOMINO kit - PROMAN
   could not boot anything even if started.
3. [MAN+I] Even a manually started PROMAN talks only to controllers discovered
   via MF-bus-controller crate interrogation or PMA-CONFIG - not to arbitrary
   stations.

The answer flips toward (b) ONLY if someone deliberately: starts the PROMAN RT
program (e.g. operator/mode file/whatever consumes the CP start table) AND
provides discovery (an emulated MF-bus controller that reports the DIOC in its
crate, or a PMA-CONFIG file) AND puts (UTILITY)PMA-SCSI-style images on the
pack. None of these are present today. If the harness later emulates MF-bus
crate interrogation, re-open this finding.

Residual [OPEN] items (do not block the harness):
- Consumer + flag semantics of the CP server-start table (003-S3CP@0xbb60 /
  013-S3SCP@0xc360) and why it did not fire on this boot (most plausible: the
  start is conditional on NUCLEUS/DOMINO hardware presence or a start command
  never issued; [I], needs 013-S3SCP disasm or live watchpoint on 14615B).
- The exact wire format of PROMAN's boot-protocol frames remains [OPEN]
  (would need PMA image + live trace); irrelevant while answer is (a).

---

## 5. Sources

- Segments + meta: `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\segments\` (120/121/124/125, 003/013, 010, 037)
- Resident RT-loader area: `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\resident\MACM-AREA-DATA_rtloader.bin`
- Live listing: `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\inputs\list-rt-programs.txt`
- NPL: `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\s3vs-4.symb`, `NPL\PH-P2-OPPSTART.NPL`, `NPL\RP-P2-MONCALLS.NPL`
- Symbols: `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\L07\SYMBOL-2-LIST.SYMB.TXT`, `l07-kallsyms.txt`
- Manuals: `E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-820026.1 EN DOMINO and NUCLEUS Software Guide.md` (sections 2.6.1/2.6.2, PROMAN pages), `ND-820026-1c-EN DOMINO and NUCLEUS Software Guide.md` (line 5474 server start claim)
- Pack: `F:\ND\SINTRAN-L - 2026\HDD\BIGDISK0-L.IMG` via `E:\Dev\Ronny\norskdata-ndfs\ndfs-c\build-win\ndtool.exe -t`
- Prior carve (not contradicted): `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\SINTRAN-OCTOBUS-MESSAGE-CATALOG.md` sections 6.1-6.3
