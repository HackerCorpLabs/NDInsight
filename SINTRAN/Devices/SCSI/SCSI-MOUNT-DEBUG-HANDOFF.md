# SCSI Disk Mount Bug - Live-Debug Handoff (resume state)

**Full path:** `SINTRAN/Devices/SCSI/SCSI-MOUNT-DEBUG-HANDOFF.md`
**Status:** ACTIVE investigation, paused for context reset. This is the single source of truth to
resume. Related docs (same dir):
`SINTRAN/Devices/SCSI/mount-gate-diff.md`,
`SINTRAN/Devices/SCSI/scsi-open-last-block-read.md`.

---

## 0. THE BUG (one line)
`@ENTER-DIRECTORY,,DISC-SCSI-1,0` fails on a SCSI disk (does not enter; `@dir` shows nothing) but
SMD works. Fails on SINTRAN K, L, M.

> **CORRECTION 2026-07-14 (static carve, byte-verified - see Section 9).** The framing in this
> Section 1 is SUPERSEDED. The last-block `READ_6 lba=129311` is **NOT** a mistaken attempt to read
> the directory master at the wrong LBA. It is the **SCSI function-42 (READ FORMAT) read of the
> CONTROL RECORD**, which lives in the last 1024-byte block by design (NPL `IP-P2-SCSI-DISK` line
> 364 `% ADDRESS OF CONTROLL RECORD`). **Function 42 SUCCEEDS** (control record valid, XOR=0,
> NPART=8, returns `T=0` and `UHLIM=121560`). The block-0 read is a **separate** function-0 request
> that the device-agnostic connect/mount path must issue AFTERWARD, and never does. Read Section 9
> for the byte-verified chain; the text below is kept for history only.

## 1. THE ANSWER SO FAR (SUPERSEDED - see the correction above and Section 9)
The SCSI enter-directory issues **only**: `INQUIRY → READ CAPACITY(→129311) → READ_6 lba=129311`
(the LAST block). It **never reads block 0** (the real PACK-ONE directory master) and **never
writes** the "entered" bit. It reads the block at `lastLBA` (129311) expecting the directory
master/index there, but on this disk that block holds a **geometry/area-map**, not a master - so
the entry can't complete and block 0 is never read.

- The `129311` in that `READ_6` is **copied straight from the READ CAPACITY reply**, not computed
  (proven earlier: `0xF91F` reaches memory only via the capacity DMA, never in a CPU register/store
  before the CDB is built).
- **Open question (the fix hinges on this):** WHY does SINTRAN treat `lastLBA` as the master
  location for a SCSI disk? That decision is in the device datafield built during `COLDE`
  (cold-enter). Next step = read that datafield live (see §7).

## 2. ENVIRONMENT / SETUP
- Emulator: **RetroCore** on Windows. Source repo: `/mnt/e/Dev/Repos/Ronny/RetroCore/` (SEPARATE
  repo, do NOT commit there without asking).
- DAP debugger: reachable from WSL at **`172.23.176.1:4712`** (the Windows host IP; `127.0.0.1`
  does NOT work from WSL2). Port may change - find host IP via `ip route show default`.
- Running SINTRAN = **L-VSX-500** (VERIFIED by byte-match of resident commoncode at `011300₈`;
  the DAP `program` field says `BIGDISK0-M.IMG` but that name is MISLEADING - it is L).
- Correct symbol set = **L07** (aligned to the L-VSX-500 binary; verified `ENDIR=140176` etc. land
  on `021xxx STD I` entries). K→K03, M→M06. See
  `/home/ronny/.claude/projects/-mnt-e-Dev-Ronny-NDInsight/memory/reference_sintran_version_symbol_alignment.md`.
- Disk under test: `DISC-SCSI-1` = the SCSI image, directory **PACK-ONE**, 129312 blocks
  (LBA 0..129311). Block 0 = valid PACK-ONE master (capacity 61036 pages). Last block 129311 =
  SINTRAN disk area/layout table (w0=2048 page-bytes, w1=`0x54D9` magic, area descriptors
  `{flag 0x8000/0xC000/0xE000, size-dword}`, usable=122072 blocks, physical=129311; XOR of all 512
  words = 0 = valid checksum). Disk = memory byte-identical (no corruption).

## 3. DAP TOOLING QUIRKS (RetroCore) - CRITICAL, learned the hard way
- Address = **hex of the ND word address** (`0xC07E` = `140176₈`). Convert octal→hex.
- **Data vs instruction space:** `B`/datafield pointers are DATA space - read with the `dspace:`
  prefix (`debug_read_memory address="dspace:0xCD20"`). Plain read = instruction space (returns
  `0xAAAA` fill for data pages).
- **Breakpoints ACCUMULATE and cannot be removed.** Empty `[]` does not clear; reconnect does not
  clear the emulator side. **Only an emulator RESTART clears breakpoints.** Plan the full breakpoint
  set before running; use a restart to reset.
- **`continue` re-triggers the current-PC breakpoint** (you get the same PC twice). Budget **two
  `continue`s per breakpoint**: first re-hits, second advances.
- **No single-stepping** - `step_over`/`next` returns "Debugger not enabled or CPU not available".
- **Use HIGH addresses only.** Low addresses (`0x3xxx`, e.g. CHDSI/RXDIR/RCBLO) are overlay-shared;
  background loops trip them constantly and you can't remove the breakpoint. Breakpoint the
  high-address routines (`0xB4xx`/`0xC0xx`) instead.
- **Console ownership:** the user can only type SINTRAN commands while the CPU is **running**
  (after `continue`). While paused at a breakpoint the RetroCore console is owned by DAP and the
  user can't type. Workflow: connect → set breakpoints → `continue` (console free) → user types
  command → breakpoints fire.
- **Do NOT `debug_disconnect`** to hand the console back - it left the CPU stuck. Restart instead.
- Reconnect from WSL: `debug_connect(host="172.23.176.1", port=4712)`.

## 4. KEY L07 ADDRESSES (verified routine entries)
| routine | octal | hex | role |
|---|---|---|---|
| ENDIR | 140176 | 0xC07E | enter-directory worker |
| COLDE | 132072 | 0xB43A | cold-enter directory (device-connect) |
| XCOLD | 132060 | 0xB430 | cold-enter variant |
| DCOLD | 132070 | 0xB438 | cold-enter variant |
| GDIRE | 131732 | 0xB3DA | get directory entry |
| CHDSI | 37763 | 0x3FF3 | check disk info (LOW-overlay) |
| RXDIR | 37643 | 0x3FA3 | read directory block 0 (LOW-overlay) |
| WXDIR | 37702 | 0x3FC2 | write directory block 0 (LOW-overlay) |
| RCBLO | 35766 | 0x3BF6 | read disc block (LOW-overlay) |
| ACCRT | 012377 | 0xA7F  | accounting record write |

ENDIR error ladder (all high, clean): `MON 124` reserve @`140252`=`0xC0AA`; `SAA 147` @`140254`=
`0xC0AC`; `SAA 145` @`140261`=`0xC0B1`; `SAA 42` @`140315`=`0xC0CD`; `SAA 32` @`140370`=`0xC0F8`.
Disassembly: `/mnt/d/ND/t/re/segments-ref/006-S3FS/006-S3FS.asm` (L07, matches running L).

## 5. WHAT WE TESTED (live DAP + SCSI trace)
- Live: SCSI mount **hits `ENDIR` → `COLDE` → `GDIRE`** (all fired), then runs free.
- Live: it does **NOT** hit ENDIR's error ladder (147/145/42/32) nor the `MON 124` reserve.
- SCSI device trace of the mount: exactly **INQUIRY, READ CAPACITY (lastLBA 129311), READ_6
  lba=129311** - and only `readBlock 129311`. **No block-0 read. No WRITE CDB.**
- Emulator SCSI write path verified functional (`scsi_put_data → writeBlock → BlockWriteCallback`
  persists to the image) - so a missing entered-bit is because SINTRAN never issues the write, not
  because writes fail.

## 6. WHAT WE RULED OUT / CORRECTED
- ❌ "ENDIR / mount worker never runs" - FALSE (was an L07-symbols-on-a-K-trace artifact). ENDIR runs.
- ❌ Error `243` "APPROACHING END OF ACCOUNTING FILE" is the cause - FALSE. It is a **non-fatal
  warning** (NPL `RP-P2-ACCRT`: `IF A>DMAX THEN 243=:ERFL` - no abort; only `244`=file-full aborts
  via `GO FAR SUIT6`). It's emitted by the accounting-record write, which the fresh SCSI entry
  reaches. **SCSI-specific only because `DISC-75-1` (SMD) is the already-entered BOOT directory**
  (fast path, no fresh entry, no accounting write) - so the SMD-vs-SCSI comparison is CONFOUNDED.
  243 is coincident.
- ❌ Device misidentification - FALSE. LDN `2210` = SCSI HD-1 decoded correctly.
- ❌ Disk-content corruption - FALSE. Disk=memory byte-identical, last-block checksum XOR=0.
- ❌ The old READ-CAPACITY-as-usable hack (report `122071`) - reverted; it made SINTRAN reject via
  the `ECAPD` capacity-consistency check. Report **raw `129311`** (already reverted in
  `/mnt/e/Dev/Repos/Ronny/RetroCore/Emulated.HW/Common/SCSI/SCSIHDD.cs`).
- ✅ Controller fixes still in place & working: #1 interrupt-ack moved RSTAU→RITRG; #2 Own-ID
  register (`IDRegister`) - in `NDBusDiscControllerSCSI.cs`.

## 6b. LIVE EVIDENCE 2026-07-13 (current build) - supersedes 4b, re-ranks causes

Read from the live trace `/mnt/c/Users/ronny/AppData/Local/trace/file-trace.txt` (mount at
20:33:47) plus the current C# in `NDBusDiscControllerSCSI.cs`:

1. **The mount is NOT a hang.** Console reconstruction of the run:
   `@ENTER-DIRECTORY,,DISC-SCSI-1,0` -> `APPROACHING END OF ACCOUNTING FILE` -> **returns to `@`**.
   The later `stop-system` + `WAIT with IONI off / CPU stopped` was the **operator typing
   `@stop-system`**, not a driver deadlock. => **case 4b (lost completion IRQ / timeout hang) is
   demoted**; the command completes and returns.
2. **The old bit-4 "error summary" root cause is already fixed.** RSTAU now reads `0x0208 / 0x3208
   / 0x5208` across the transaction - **bit 4 clear**. The C# RSTAU assembler
   (`NDBusDiscControllerSCSI.cs:874-946`) never sets bit 4 (DMA error) or bit 15 (differential).
   The `STATUS 100020` (0x8010) analysed in `scsi-transfer-status.md` was a STALE build.
3. **The last-block read now completes cleanly.** CDB sequence this run:
   INQUIRY -> READ CAPACITY (op `0x25`, lastLBA=129311) -> `SC_READ_6 lba=129311` -> `readBlock
   129311` returns `08 00 54 D9 80 ...` (the checksummed area/layout table), DMA to ND OK, IRQ
   acked on RITRG. (`op 0x25` is mislabeled `SC_GET_WINDOW` in the C# enum but handled correctly
   as READ CAPACITY - cosmetic.)
4. **Still exactly ONE data CDB. Block 0 is never read; no WRITE CDB.** Unchanged and central.

### Root-cause ranking after this evidence
- **H1 (strongest): a datafield/consistency check after the area-table parse silently aborts the
  connect before block 0 is queued.** The command returns to `@` with a soft warning, no directory.
  Candidate inconsistency: block-0 PACK-ONE master claims capacity **61036 pages = 122072 blocks**,
  but READ CAPACITY reports physical **129312 blocks (lastLBA 129311)**; the last-block area table
  itself states usable=122072 / physical=129311. A capacity/geometry-consistency field derived from
  these is the likely gate. NOTE: reporting usable `122071` was already tried and REJECTED by the
  `ECAPD` check, and raw `129311` also does not mount - so the value SINTRAN wants is a THIRD thing,
  pin it live, do not keep guessing capacity numbers.
- H2: block-0 read is issued on a path the emulator drops (no CDB). Unlikely - only one data CDB
  total ever appears.
- ~~H3 (was 4b): lost completion interrupt~~ - demoted; the mount returns to `@`, it does not hang.

### The ONE unpinned span (every prior doc stopped short of it)
All prior work pinned (a) the pre-CDB device-router gate (`126305`, now retracted as a legit
fast-path vs general-path router) and (b) the last-block read itself. **Nobody has traced the
instructions between "area-table read completes" and "return to `@`"** - that is where the block-0
read should be queued and is not. Pin that branch and the fix follows.

## 7. NEXT STEP (resume here) - DECISIVE PLAN (needs RetroCore restart; machine is halted)
1. Restart RetroCore (clears breakpoints), boot **L** with the SCSI disk, to `@`.
2. `debug_connect(host="172.23.176.1", port=4712)`; sanity-read resident `0x12C0` and byte-match L
   (see `versions/L-VSX-500/resident/SINTRAN-DATA_commoncode.bin` word `011300₈`).
3. Set HIGH-address breakpoints only: `["0xB43A"(COLDE), "0xB3DA"(GDIRE), "0xC07E"(ENDIR)]`. `continue`.
4. User runs `@ENTER-DIRECTORY,,DISC-SCSI-1,0`. Advance (2 continues/bp) to **`COLDE`**.
5. At COLDE, read registers; `B` = COLDE datafield. Read the device datafield via `dspace:` and find
   the **master-block / first-page / directory-index field** = the value that sends the read to
   `129311` instead of block 0. Compare against what a correct SINTRAN SCSI disk should present.
6. That field's origin (controller geometry / disk-parameter setup) is the emulator-side fix.
   Alternative probe: on K the device type/flags word was `103325`(SCSI) vs `012314`(SMD) - the
   device-type signature; find the L equivalent.

### 7b. DECISIVE additions (2026-07-13) - how to actually close H1

The step-5 datafield read is right but was never carried through to the abort branch. Do these,
in order, in ONE mount run:

- **Get a fresh full opcode+SCSI trace first (cheapest, no console-ownership fight).** Delete
  `/mnt/c/Users/ronny/AppData/Local/trace/file-trace.txt`, ensure `Logger.EnableLogger(Device,File)`
  + `SCSIDevice.ScsiTrace=true` (already default true), run the mount, `@stop-system`. This trace
  ALONE likely pins the abort branch without any breakpoint.
- **In that trace, find the L resident-SCSI-driver span that copies lastLBA and builds the
  area-table read** (K equivalent was PC `114504-114530`; the `0xF91F` reappears only after the
  READ-CAPACITY DMA). Then read FORWARD past the area-table parse to the **first conditional branch
  that decides read-block-0 vs return** - that is the unpinned gate (Section 6b). Dump its tested
  word and both arms.
- **Use DATA watchpoints, not instruction breakpoints, for the datafield** (instruction BPs on the
  low overlay-shared addresses can't be removed - see §3). `debug_set_data_breakpoints` IS available
  on this DAP. Capture `B` (datafield base) at the one high-address BP `0xC07E`(ENDIR)/`0xB43A`(COLDE),
  then watchpoint the datafield words the area-table parse writes and see which one is read at the
  abort branch.
- **Field-by-field diff SCSI datafield vs the working SMD datafield** (decode against
  `SINTRAN/SINTRAN Structures/SINTRAN-STRUCTURES.md` device-datafield
  layout). The one differing word that the abort branch tests is the fix target; trace its origin
  to the emulated controller ident / READ CAPACITY geometry.
- **Do NOT keep trying capacity numbers blind.** `122071` was rejected by `ECAPD`; raw `129311`
  does not mount. Pin what SINTRAN compares against before changing any value in
  `SCSIHDD.cs` / `SCSIHDDMicropolis.cs`.

**Blocking dependency:** the CPU is currently HALTED (operator `stop-system`); RAM is intact and
readable over DAP, but a fresh mount requires a RetroCore restart and the operator typing
`@ENTER-DIRECTORY,,DISC-SCSI-1,0` while the CPU runs (console ownership, §3).

## 8. FILE INDEX (full paths)
- This handoff: `SINTRAN/Devices/SCSI/SCSI-MOUNT-DEBUG-HANDOFF.md`
- SCSI docs: `SINTRAN/Devices/SCSI/mount-gate-diff.md`,
  `SINTRAN/Devices/SCSI/scsi-open-last-block-read.md`
- Emulator SCSI: `/mnt/e/Dev/Repos/Ronny/RetroCore/Emulated.HW/Common/SCSI/SCSIHDD.cs`,
  `/mnt/e/Dev/Repos/Ronny/RetroCore/Emulated.HW/ND/CPU/NDBUS/NDBusDiscControllerSCSI.cs`
- L07 disasm+symbols: `/mnt/d/ND/t/re/segments-ref/006-S3FS/006-S3FS.asm`,
  `/mnt/d/ND/t/re/segments-ref/006-S3FS/006-S3FS.symbols.txt`
- L07 symbol tables: `SINTRAN/NPL-SOURCE/SYMBOLS/L07/`
- L carve bins: `tools/sintran-segment-carver/versions/L-VSX-500/`
- Accounting NPL (243 proof): `SINTRAN/NPL-SOURCE/NPL/RP-P2-ACCRT.NPL` (~line 172)
- SCSI trace output: `/mnt/c/Users/ronny/AppData/Local/trace/file-trace.txt`
  (Windows `C:\Users\ronny\AppData\Local\trace\file-trace.txt`)
- DAP MCP server: `/home/ronny/repos/libdap/mcp-dap-server/`

---

## 9. CARVE FINDINGS 2026-07-14 (static, byte-verified) - what the code actually does

Six focused carves of the L07 binary (all in
`tools/sintran-segment-carver/versions/L-VSX-500/re/kernel-carving/`:
`ENTER-DIRECTORY/`, `RCBLO/`, `PRSRV-124B/`, `SCSI-DRIVER/`, `FUNCTION-42-RETURN/`,
`SCSDISK-TRANSFER/`, `SMD-DRIVER-BASELINE/`, `COLDE-CONNECT/`) settled several things the live-debug work above had
only hypothesized. Every claim below was reproduced with `dd` against the L-VSX-500 segment bins.
- `COLDE-CONNECT/` - byte-verified: COLDE (132072B) issues NO device transfer (it is a directory/name walker); the block-0 read is the single `CHDSI 040000B -> RXDIR 037652B -> RCBLO 036135B (JPL I ,B 10)` dispatch, never retried by FILSYS, so the read-vs-fn42 fork is in the SCSI driver (065-S3SIPIT), not in COLDE.

### 9.1 The dispatch is device-agnostic (confirms the "one code, many drivers" model)
`ENDIR 140176 -> GDIRA (datafield) -> MON 124B PRSRV -> CHDSI 037763 -> RXDIR 037643 (block:=0)
-> RCBLO 035766 -> JPL I ,B 10 via device datafield `,X 14` -> per-device transfer primitive`.
The filesystem asks for "read page 0" **identically** for every disk type. The ONLY per-device
difference is what `,X 14` points to (SCSI = `SCSDISK`). ENTER-DIRECTORY uses exactly **one** MON
call: `MON 124B PRSRV` (force-reserve the unit).

### 9.2 PRSRV / unit-reserve is NOT the abort (byte-proven at the ENDIR side)
ENDIR's own reserve check: `140252 MON 124 ; 140253 JAP 3 (A>=0 success -> proceed) ; 140254 SAA
147 ; 140255 JMP I error`. The console never prints `147B`, so ENDIR did **not** take the
reserve-fail path - it proceeded past the reserve toward CHDSI/block-0. MON 124B PRSRV itself is a
2-word trampoline (`LDA 3 ; JMP I -> LEAV2`); it raises no error. (This matches Section 5's live
observation that the error ladder never fires.)

### 9.3 The last-block read = function 42 = CONTROL RECORD, and it SUCCEEDS
SCSI function 42 (READ FORMAT) reads the control record from the last block, validates it
(`XOR=0`, `2<NPART<=NCOPA`, NPART=8), extracts the 48-word partition table, sets `UHLIM=121560`,
and **returns `T=0` (success)**. It is NOT a geometry probe and NOT a mis-located master read.
Return-value table (VERIFIED, disk layer at NPL-label + 376B, `SCSID=062217B` in `065-S3SIPIT`):
`T=0` OK / `1` TYPER / `4` ILAOP / `5` BADPA (out of UHLIM bounds -> user err 174B) / `11` NOCRC.
Our run produces **`T=0`**.

### 9.4 The function-42 return value / init-state does NOT cause the skip (hypothesis disproved)
- Function 42 clears `5SCIN` on entry, INQUI SETS it on success; **after function 42 returns,
  `5SCIN = SET`**. The only next consumer (the fn-0 re-entry gate at `063752B`) does NOT divert
  when `5SCIN` is set - it PERMITS block 0 through to `SCLLD/SCWAQ`.
- So neither the return code (`T=0`, same success sense as SMD) nor the `5SCIN` state can cause the
  block-0 skip. The specific "SCSI function 42 returns a different value that branches away block 0"
  hypothesis is **REFUTED for this run**.

### 9.5 SMD vs SCSI: the real divergence (why SMD works)
- **SMD/Winchester function 42** (`IP-P2-DISK-START`) is a near-no-op: it returns a **static format
  number** from a table (`HTABL/DISPN` via `*DEPO`), does **no disk I/O**, leaves no follow-up
  state. There is even a device-type branch `IF A=42 AND "TRNSF" >< "BDISK" GO FAR FIN` ("READ
  FORMAT NOT LEGAL IN DRIVER, no error message"). So on SMD the mount blows past function 42 and
  goes straight to the block-0 read.
- **SCSI function 42** does the whole INQUIRY / READ CAPACITY / control-record I/O and returns
  disk-derived data. Same success sense, but a completely different amount of work and device state.
- **The divergence is the WORK and the OUTPUT DATA, not the return code.** Both report success.

### 9.6 There is NO branch inside the SCSI driver/disk-layer that skips block 0
Function 42 terminates cleanly via `RETEX -> RETOP` (`T=0`) and never chains a block-0 read. Block
0 is a **separate function-0 request** from the device-agnostic connect/mount path. So the failure
is in **how that connect/mount caller consumes function-42's output and decides to issue (or not
issue) the block-0 request.** The SCSI driver enqueue path is exonerated: work enters the driver
ONLY via `SCLLD -> INITO -> SCWAQ`, and SCLLD is never called for block 0 (the live SCWAQ-empty
signature). Nothing in `SCSDISK`/`SCSID` drops it silently up to that point.

### 9.7 Where the fault must be (two remaining cases) + the ONE decisive check
The block-0 request is either:
- **Case 1** - never issued: the connect/mount caller (`COLDE` cold-enter, high address `0xB43A`)
  aborts or returns after function-42 success WITHOUT advancing to CHDSI/block-0. (Section 6b's
  "unpinned span between area-table read and return to @" is exactly this.)
- **Case 2** - issued but rejected pre-transfer: a fn-0 block-0 request enters the SCSI disk layer
  but is rejected before `SCLLD` (e.g. `BADPA T=5` at the `UHLIM` bounds check), producing no wire
  I/O plus an error. This is the only remaining thread that could connect to an address/scaling bug.

**Decisive live check (settles Case 1 vs Case 2 in one mount):** breakpoints at
`COLDE 0xB43A` (does the connect even advance?) and `CHDSI 0x3FF3`. If **CHDSI is never hit** ->
Case 1: the connect overlay (`COLDE`) never advances -> carve/trace `COLDE`. If CHDSI IS hit but no
block-0 CDB -> follow RXDIR/RCBLO -> `,X 14 -> SCSDISK` and watch for a `BADPA` reject (Case 2).
Per Section 3, CHDSI/RXDIR/RCBLO are LOW overlay-shared addresses - prefer the `COLDE 0xB43A` HIGH
breakpoint + a DATA watchpoint on the datafield word the area-table parse writes, rather than
instruction BPs on the low addresses.

### 9.8 The accounting message (re-confirmed, still a red herring for the abort)
"APPROACHING END OF ACCOUNTING FILE" = error `243B`, set only in RT-accounting (`RP-P2-ACCRT`) at
logout/accounting-write; **no call edge from the mount path** (agrees with Section 6). Non-fatal.
Keep the "emulator error-string table mis-renders the real error code" possibility open, but it is
not on the mount abort path.

### 9.9 Corrections folded in
- The earlier `SCSI-DRIVER/` carve disassembled function 42 at the WRONG (no-offset) NPL addresses
  and got garbage. The SCSI **disk layer** (SCSID/function 42/INQUI) is at **NPL-label + 376B** in
  `065-S3SIPIT`; the SCSI **driver** (SCLLD/INITO/SELEC) aligns with its NPL labels directly. The
  `FUNCTION-42-RETURN/` carve is the authoritative function-42 analysis.
- `scsi-mount-geometry.md` (which framed the last-block read as the control-record (function-42 connect)) was corrected: it
  is the control-record read.

**Net:** static carving has narrowed the bug to a single locus - the `COLDE` connect/mount overlay
deciding whether to issue the separate block-0 request after function-42 success. The one DAP run
in 9.7 distinguishes the two remaining cases. Reserve, function-42 success, the return value, the
`5SCIN` state, DMA, disk content, and the driver enqueue path are all cleared with evidence.
