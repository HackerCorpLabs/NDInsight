# SCSI DIOC / BDIO - RetroCore implementation handoff (2026-07-23)

State of the DOMINO SCSI DIOC emulation in RetroCore after this session. Read
this first when resuming; it says what is built + verified, what is [OPEN] and
why, and exactly how to close the tail on the live machine.

Companion docs (the byte-verified RE this rests on):
- `tools/sintran-segment-carver/versions/L-VSX-500/re/domino-nucleus-io/BDIO-DOMINO-DRIVER-CARVE.md`
- `.../domino-nucleus-io/BDIO-ADDRESS-MODEL-FINDINGS-2026-07-23.md`
- `.../domino-nucleus-io/QUDF-ABPA2-PRODUCER-CARVE-2026-07-23.md`
- `.../domino-nucleus-io/NUCLEUS-PRIMITIVES-CARVE.md`
- Plan: `SINTRAN/ND5000/SCSI-DIOC-OCTOBUS-EMULATION-PLAN-2026-07-20.md`

## 1. What the DIOC is (mental model)

To the ND-100 the DIOC is a BLACK BOX. SINTRAN never touches SCSI targets / LUNs
/ CDBs. What crosses the octobus is only:
- a NUCLEUS message in shared MPM whose payload is a BDIO / ABSTrans record, and
- octobus KICK 1 as the doorbell in each direction (S0 CONKI carve: NOT kick 5).

So the DIOC is a NUCLEUS message endpoint that moves 2KB pages between a disk
image and the MPM window. The SCSI-bus chip emulation (Buses.SCSI, Mac/Sun) is
NOT involved.

## 2. RetroCore components (all under Emulated.HW\ND\CPU\NDBUS\ unless noted)

| File | Role | Status |
|---|---|---|
| `MpmWindow.cs` | shared MPM view: bounds-guarded big-endian word/double access, byte-range DMA, TSET lock (X5SEM + NUCLEUS 070000B) | done; composed by BOTH the ND-5000 station and the DIOC |
| `OctobusMultibyteCollector.cs` | shared SOMB/EOMB reassembly | done; both stations use it |
| `OctobusScsiDiocStation.cs` | the DIOC octobus station (module 21B, station 13B): OMD-0 test protocol, kick-1 doorbell -> BDIO -> completion kick to station 1 | S1 + engine wiring done |
| `BdioRecord.cs` | byte-verified decode of the BDIO/ABSTrans record (DSCMD/DSTBL/DMYAD/DNRPG/DXPOO) | done |
| `BdioEngine.cs` | read/write page transfer disk<->MPM + DSSTS=(0,0) completion; reuses DeviceControllerBase.BlockRead/WriteDelegate | Read/Write done; Compare = [OPEN] |
| `BdioRecordScanner.cs` | S4-2 live-trace tool: finds a BDIO record in MPM by signature (DSOW1=1 + function + DMYAD bit31) | done |
| `NucleusStructures.cs` | byte-verified NUCLEUS offset table (master/descriptor/port/message/buffer/kick) | done (offsets [V]) |
| `NucleusClient.cs` | S2 scaffold: typed accessors over a descriptor's PHYSICAL address + TSET lock | scaffold; ID-resolution + provisioning = [OPEN] |
| `Emulated.Machines\ND\ND100\ND100Machine.ScsiDioc.cs` | `AttachScsiDioc` / `MountScsiDiocImage` machine wiring, disk via MachineStorageManager | done |

Tests (Emulated.Tests.ND100\ControllerOctobus\ + Emulated.Tests\ND100\):
`BdioRecordTests`, `BdioEngineTests`, `BdioRecordScannerTests`, `NucleusClientTests`,
`OctobusScsiDiocS1Tests`, `OctobusScsiDiocBdioE2ETests`, `OctobusScsiDiocMachineTests`.
135 octobus/BDIO/machine tests green; ND-5000 boot harness unaffected.

## 3. What is VERIFIED (no assumptions)

- BDIO record IS an ABSTrans message; fields map to ABFUN/MEMA1/ABP21/ABP31. [V]
- Memory address: `mpmByte = mpm.Start + (DMYAD & 0x7FFFFFFF)` - DMYAD is
  window-relative once bit 31 is stripped, ADRZERO cancels. [V-derived]
- Media address DSTBL = logical 2KB-page index (verbatim, no scaling; DOMINO path
  skips the SMD TOSECT). disk byte offset = DSTBL*2048, length = DNRPG*2048. [V]
- Completion contract DSSTS=(0,0)=OK. [V REBDIO]
- NUCLEUS structure offsets (NucleusStructures). [V]
- Doorbell = kick 1 both directions. [V S0 CONKI]

## 4. What is [OPEN] (blocked on the live machine, NOT guessable)

1. **NUCLEUS record-base discovery (S2).** The DIOC learns WHERE in MPM the BDIO
   record landed from the NUCLEUS message (port -> message -> buffer walk). The
   MON-347 provisioning answers (create-port / open-port-by-name) and the
   connect-answer fields (DIPOO / OPAIX / ARESZ) are not carved. Today the station
   is told the record base via `SetPendingBdioRecord` (test seam).
2. **Live SINTRAN mount acceptance (S4).** Needs a real DOMINO disk + SINTRAN
   reaching a BDIO transfer.
3. **Compare (213B) mismatch status + BDIO failure status codes.** The DIOC's
   error DSSTS values are not carved; the engine returns result codes and does
   NOT fabricate a SINTRAN status word.
4. **Card routing to a DIOC station without an ND-5000 CPU attached** - whether
   SINTRAN's octobus SENDS reach a DIOC station in the card's default mode is
   unconfirmed (part of the live path).

## 5. HOW TO CLOSE THE TAIL (the next live session)

The tool to close S2 + confirm the model in one live boot is already built:

1. In a boot harness (model on `Nd100SintranNd5000OctobusBootHarnessTests`),
   after building the machine call `machine.AttachScsiDioc(octobus)` and
   `machine.MountScsiDiocImage(<domino disk>)`.
2. Subscribe `attachment.Station.FrameObserved` + `.KickReceived` to log what
   SINTRAN sends the DIOC.
3. On a kick-1 (or periodically), run `BdioRecordScanner.Scan(station.Mpm)` and
   log the hits - each candidate prints its decoded fn/disk/mem/pages.
4. Correlate a hit's DSTBL against a KNOWN file's directory page to confirm the
   2KB-page unit end to end, and note the record base + the NUCLEUS message that
   pointed at it -> that pins the S2 record-discovery path.
5. Feed the discovered record base to `station.SetPendingBdioRecord` (or wire the
   real discovery) and let `BdioEngine` service it -> the mount acceptance gate.

## 6. Commit state (IMPORTANT)

- Committed earlier this session: `de2b95ab3` (shared MpmWindow/collector + DIOC
  skeleton, by a concurrent agent) and `b18b4efaf` (BdioRecord/BdioEngine + MPM
  DMA + station wiring + first tests).
- UNCOMMITTED at handoff time: the `/simplify` cleanup (MpmWindow
  ReadDouble/WriteDouble/ReadWords, ND-double dedup, SendKick on the base,
  decimal NucleusStructures, nullable pending-record), plus `BdioRecordScanner`,
  `NucleusStructures`, `NucleusClient`, `ND100Machine.ScsiDioc.cs`, and their
  tests. RetroCore branch: `ethernet-ii-controller-fixes` (shared tree, other
  sessions committing autonomously - stage ONLY DIOC/BDIO files).

## 7. Architecture rules honored

No code duplication (shared helpers, not copies); no LINQ / no foreach; named
delegates; reuse of the existing block-storage contract; every un-carved value is
a result code / [OPEN] marker, never a fabricated byte.
