# BDIO record address model - findings (2026-07-23)

Analysis done while building the RetroCore SCSI DIOC emulation
(`OctobusScsiDiocStation` + `BdioRecord`, phase S3). Resolves the two address
questions a `BdioEngine` must answer to transfer blocks between a disk image and
the shared MPM window, and pins the one that is still open.

Evidence tags: [V] byte-verified, [I] inference, [OPEN] unresolved.
Overlay/context for the record itself: see `BDIO-DOMINO-DRIVER-CARVE.md`
(017-S3SMPIT = 026-S3IMPIT, load base 032000B).

---

## 1. The BDIO record IS an ABSTrans (ABSolute TRansfer) message  [V]

Confirmed three independent ways:

- The carve marks the record with `DSOW1 = 1` = "abstrans message marker"
  (MBUILD 073734-073735, `BDIO-DOMINO-DRIVER-CARVE.md` record table).  [V]
- The field roles match the ABSTrans queue-element layout documented in
  `Reference-Manuals/ND-820023-1-EN SINTRAN III-VSX System Documentation.md`
  (Disk I/O, "Page 126/127"; ND-500 swapper section).
- ABFUN / MEMA1 / ABP21 / ABP31 line up field-for-field.

Field mapping (carve name -> ABSTrans name -> role):

| carve (msg rel word, decimal) | ABSTrans (ND-820023) | role |
|---|---|---|
| DSCMD  (10)     | ABFUN | function 166B read / 167B write / 213B compare [V] |
| DMYAD  (20-21)  | MEMA1 (24-bit init memory address) | DOMINO memory byte address, bit 31 set [V] |
| DSTBL  (18-19)  | ABP21 (ABSTrans parameter 2) | media / disk address [V role, OPEN unit] |
| DNRPG  (24-25)  | ABP31 (ABSTrans parameter 3) | number of pages [V] |

Producer offsets in the caller's queue-DF (QUDF):
`ABFUN=14 MEMAD=15-16 ABPA2=17-20 ABP31=21` (carve section, QUDF layout).  [V]

---

## 2. Memory address (DMYAD / ADRZERO) - RESOLVED (derived + code-confirmed)

The DCNVA rule (byte-verified, `CARVING-HANDOFF.md` + BDIO carve):

```
DMYAD = ((word_addr - (ADRZERO << 10)) << 1) | bit31
```

where `ADRZERO` is the ND-100 PAGE number of the first MPM page
(`N500D = 051767` [SYMBOL-2], `ADRZE = +60` [SYMBOL-1]; N500M fn 40B DEFM
writes it - `Installation/OS/versions/SINTRAN-L.md`).

Because the MPM window base byte address is `mpmStart = ADRZERO * 2048`, the
ADRZERO term cancels:

```
DMYAD & 0x7FFFFFFF = (word_addr - ADRZERO*1024) * 2 = window-relative BYTE offset
=>  MPM physical byte = mpmStart + (DMYAD & 0x7FFFFFFF)
```

So the DIOC does NOT need ADRZERO's value at all: it strips bit 31 and adds its
own attached `mpm.Start`. This is not an assumption - it is algebra on the [V]
DCNVA rule, and it is exactly what the existing octobus mailbox servicer already
does for window-relative links:

```
IServicerHost.ResolveMailboxLink(link) => _mpmStart + (link & 0xFFFFFF)
```
(`Emulated.HW/ND/CPU/ND500/Servicer/IServicerHost.cs`,
implemented in `OctobusND5000Station`).  [V-derived]

Concrete value in the RetroCore octobus harness: `MpmStart = 0x00420000`, so
`ADRZERO = 0x420000 / 2048 = 004100B (2112 decimal)`
(`Nd100SintranNd5000OctobusBootHarnessTests.cs`: "ADRZERO page 004100B =
0x420000 = MpmStart"). The DIOC never reads it; it is recorded here only to
explain why the cancellation holds.

---

## 3. Media address unit (DSTBL / ABP21) - RESOLVED as a logical 2KB-page index

UPDATE 2026-07-23 (see `QUDF-ABPA2-PRODUCER-CARVE-2026-07-23.md`): carved the
producer path. `DSTBL` is the ABSTrans caller's media address copied VERBATIM
(no scaling) by both param copiers `GAPFU 000744B` / `GAPFD 034006B`
(`LDD I,B 2 / STD ,X 17`, xxd-verified) and then by MBUILD - AND the DOMINO
`BDMTR` path deliberately SKIPS the `TOSECT` geometry conversion the SMD path
uses. So SINTRAN hands the DIOC a LOGICAL address unchanged; on a device whose
whole world is 2KB pages that is a 2KB-page/block index. Disk byte offset =
`DSTBL * 2048`, transfer length = `DNRPG * 2048`. [V driver path + strong-I unit]
The original [OPEN] framing below is kept for the record.

What is known:

- A DOMINO block = 2048 bytes; the whole pool / directory / image system is in
  2-Kbyte pages ("The page size is 2 Kbytes"; "Disk capacities in number of
  2 Kbyte pages"; pool extents in 2048-byte blocks -
  `Reference-Manuals/500/ND-814009-1-EN DOMINO SCSI Operator Guide.md`;
  "A block has 2048 bytes" - DOMINO software guide image format).  [V]
- DNRPG (ABP31) is explicitly the NUMBER OF PAGES, and the memory-side transfer
  is `DNRPG * 1024 words = DNRPG * 2048 bytes`.  [V]

Why it is NOT yet certain that `DSTBL` is a 2KB-page index:

- The docs label ABP21 only as "ABSTrans parameter 2 / media address" - they do
  NOT state its unit for the BDIO / DOMINO-SCSI path.
- The SIBLING SMD-disk path in the SAME document (`ND-820023-1-EN`) addresses the
  disk in SECTORS, not pages: `DBLOC` = "Number of sectors (words) to transfer",
  `DKFUN` = "...most significant bits of disk address", `SECTP` = "sectors in
  page". So a sector-addressed disk path demonstrably exists in SINTRAN; ABP21
  being sector-based cannot be ruled out from docs alone.

Current model used in code (marked [I]): `DSTBL` is a 2KB-block/page index, so
`disk byte offset = DSTBL * 2048`, transfer length = `DNRPG * 2048` bytes. This
is the coherent, evidence-backed reading (uniform 2KB world, DNRPG in pages) but
is INFERENCE, not byte-verified.

### How to close it definitively

1. CARVE the ABP21 producer: the pool / file-system worker that fills
   `QUDF.ABPA2` for the DOMINO device (the fn 11B-14B worker bodies are the
   [OPEN] tail in this folder's README). Its computation reveals the unit by
   construction. Preferred - static, no live machine.
2. LIVE TRACE (SCSI plan S4-2): capture a real STRBDIO record for a read of a
   KNOWN file page in a known pool; correlate `DSTBL` against that page's known
   directory address. Confirms the unit AND doubles as the S4 acceptance trace.

Do NOT ship a `BdioEngine` disk transfer as "verified" until one of these lands;
the DSTBL scaling is the last unproven step.

---

## 4. Consumers in RetroCore

- `Emulated.HW/ND/CPU/NDBUS/BdioRecord.cs` - decodes the [V] record fields at the
  carved word offsets (DSCMD/DSTBL/DMYAD/DNRPG/DXPOO). Deterministic tests in
  `Emulated.Tests.ND100/ControllerOctobus/BdioRecordTests.cs`.
- `Emulated.HW/ND/CPU/NDBUS/OctobusScsiDiocStation.cs` - the DIOC station (S1).
- Backing store to reuse (no new abstraction): the
  `DeviceControllerBase.BlockReadDelegate` / `BlockWriteDelegate` contract wired
  through `MachineStorageManager`, same as the Hawk/Winchester/SCSI ND-100
  controllers.
