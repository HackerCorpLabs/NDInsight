# D4 RUN Blocker - Integration Brief for the Architect + the ND-5000/Microcode LLM (2026-07-19)

Audience: (1) the RetroCore emulator architect, (2) the ND-5000 / microengine LLM.
Purpose: hand off everything learned/changed this session about running a real ND-500 domain
(LINKAGE-LOAD-H02) under real SINTRAN III L on the ND-5800-provisioned image (the "D4" goal =
RUN reaches the `NLL:` prompt). All claims below are VERIFIED from instruction traces + the 3022
register log + a byte scan of the placed image, unless marked INFERRED.

> ## CORRECTION NOTICE (2026-07-19, from Ronny) - READ BEFORE THE REST
>
> An earlier revision of this brief claimed the ND-5800 swapper "is control-store microcode, so no
> macro-code swapper exists and the functional CpuND500 can never run it". **That claim is WRONG and
> is withdrawn.** Wherever this document says or implies "the swapper is microcode", read the
> following instead:
>
> - **Microcode** = what "> Loading Control Store" downloads into the **CPU's control storage** (the
>   CPU's internal 128-bit microengine firmware). That is the CPU's own decoding firmware, nothing else.
> - **ND-500 code - the swapper AND every domain - is NOT microcode.** It is ordinary ND-500
>   executable code in an ordinary **executable memory area / segment**, executed straight out of
>   memory by the functional `CpuND500` macro interpreter. **The swapper runs in its own segment.**
> - The byte scan that triggered the wrong conclusion only showed that ONE set of 44 RESIWR transfers
>   held page tables + zero pages. It says nothing about microcode - the swapper's code segment is
>   simply placed elsewhere and still has to be located.
> - Direct counter-evidence: injecting the K-rev `SWAPPER-K01.PSEG` at the derived PSEG base and
>   setting P to its entry made `CpuND500` **execute real ND-500 instructions** (PC 0x08000004 ->
>   0x08000021) before diverging on a K-vs-L data-layout mismatch. A microcode wall makes that
>   impossible.
>
> **Consequence for both audiences:** the primary path to D4 is to LOCATE the L swapper's executable
> segment in the placed image, map it (`MapExistingPhysicalRegion` / `InstallSwapperMapping` already
> exist and are byte-verified to map correctly), and set P to its real entry - i.e. RUN THE REAL
> SWAPPER. The C# functional swapper of section 5 is a BACKSTOP, not the plan of record. Section 4
> ("for the microcode LLM") is retained only for the genuine control-store / microengine questions;
> its swapper-is-microcode framing does not apply.

> ## UPDATE - THE SWAPPER NOW RUNS (later the same day; supersedes section 1 below)
>
> Everything in the CORRECTION NOTICE was confirmed by execution. Current state, all verified:
>
> - **Swapper code LOCATED:** SINTRAN places the real swapper executable itself at MPM physical
>   `0x06F800` (19 dense pages) - byte-for-byte identical to `SWAPPER-K01.PSEG` (38,161 bytes,
>   `REV`/`-K01` strings). It arrives via "> Loading Swapper", NOT via any 14B RESIWR.
> - **How to find it (no guessing):** SINTRAN DMAs its page tables and they name the layout -
>   RESIWR page `0x6E800` = PROGRAM table (PFNs `00DF 00E0 ...`, `0xDF << 11` = `0x06F800`,
>   19 pages), `0x6E000` = DATA table (`0049 004A ...` = `0x24800`, 107 pages). `DeriveSwapperRegions`
>   now reads those tables. Deriving from RESIWR extents was the original mis-map.
> - **The swapper EXECUTES real ND-500 code**, PC progressing `0x04` -> `0x11` -> `0x52` ->
>   `0x080082EE` (running in segment 1). Three real bugs were fixed to get there: data capability
>   belongs in segment 1 (not P's segment 0); segment 1 also needs a PROGRAM capability over the same
>   PSEG; and - a genuine `CpuND500` loader defect - `MapExistingPhysicalRegion` wrote `pte |= 0x1`
>   treating bit 0 as a present bit, when bit 0 is PROTECTION (`PG_W`=0 / `PG_R`=1) and validity is
>   `PFN != 0`, so every mapped page was read-only.
> - **The swapper injection / `AnnounceSwapperAlive` fake / 3MONCO "parked but alive" intercept are
>   all DELETED.** Nothing is faked on this path any more.
>
> **NEW BLOCKER, and it is architectural (for the architect):** `CpuND500`'s MMU does not use
> memory-resident tables. `InitializeMMU` (`CpuND500.MMU.cs:293`) allocates `PST` and `PCBTable` as
> C# arrays and `TranslateVirtualAddress` indexes them directly; `PSTP` takes no part in translation.
> Real hardware walks a PCB and PST that live IN MEMORY. So every capability SINTRAN or the swapper
> builds in memory is invisible, and the bring-up needs a hand-built capability per newly-touched
> segment (current stop: write to `VA 0x00000002`, data segment 0, `cap=0x0000`).
> Ronny's decision: implement the memory-resident walk properly.
>
> **What is missing to do that - and the specific ask:** the walk needs the PST base. SINTRAN sends
> `PS` in the 21B image (`reg[18] = 0x48480003`; register 18 = `regs.PS`) which the spec says is the
> anchor (`PST[PS]` -> PCB table address), but it sends **no `PSTP`** anywhere in the block
> (`0=P .. 18=PS .. 24=CAD`), and the image's halfword order is `INFERRED [D]`, so `PS` is either
> `0x48480003` or `0x00034848`. **ND-5000/microcode LLM: if the microcode contains the MMU
> table-walk microroutines (PST/PCB fetch, capability decode, and where PSTP/PS are loaded during
> process start - e.g. from the context block), that is the authoritative ground truth needed here.**
>
> Spec basis already located (`Emulated.HW\ND\CPU\ND500\spec\ND500_MMU_SPECIFICATION.md`): PSTE =
> 4 bytes, bits 1-0 index mode, bits 31-2 PFN (lines 266-281); PCB = 256 bytes/domain, `pcb_pc[32]`
> at offset 0 and `pcb_dc[32]` at offset 64 (lines 312-346); walk sequence at lines 293-301.
>
> Also open: the trap fields we report to SINTRAN are wrong - SINTRAN printed "DATA segment READ
> access / Logical address 1 100645B" for what the CPU records as an INSTRUCTION fetch.

Companion docs (same folder):
- `ND500-D4-RUN-BLOCKER-FINDING-2026-07-19.md` sections 12/12a/12b/12c + **12d/12e/12f** (full detail + citations)
- `ND500-MAILBOX-MESSAGE-CATALOG.md` sections 7c / 7c-bis / 7d (mailbox protocol + swapper contract)

---

## 1. Executive summary - where D4 stands now

The D4 chain advanced from "stuck in a mailbox loop" all the way to "the swapper is started and
the domain is confirmed runnable", by fixing two real emulator gaps and proving one architectural
fact:

1. **CS-load COMPLETES** (was wrongly believed to be the blocker). MICRO-START fires, 5CLOST clears.
2. **Mailbox function 17B = 3DEPR (DEPOSIT REGISTER) was unhandled** -> answered 5ERANSWER ->
   SINTRAN re-sent the whole bring-up cycle forever. IMPLEMENTED a handler; the bring-up now runs
   LINEARLY through to `23B 3START` (the swapper start).
3. **The swapper's MMU mapping was missing** on the 23B start path. IMPLEMENTED it, and the
   illegal-instruction trap MOVED from 0x800 (unmapped garbage) to 0x04 (the real logical entry) =
   the mapping is CORRECT and the CPU now fetches the placed image. The 44 RESIWR transfers scanned
   there held only page tables + zero pages, so the swapper's CODE SEGMENT IS PLACED ELSEWHERE and
   must be located (see the CORRECTION NOTICE above - this is NOT a microcode wall).
4. **The DOMAIN (LINKAGE-LOAD-H02) IS real ND-500 macro-code** (87.6% dense, ND-500 opcode-byte
   distribution, user-program at VA 0xB0000000 / segment 22). So a FUNCTIONAL swapper (emulate the
   swapper's effect, don't execute microcode) CAN reach `NLL:` - the swapper is only infrastructure;
   the domain is what must execute, and it can.

Net (after the correction above): the path of record to D4 is LOCATE + MAP + RUN THE REAL SWAPPER
SEGMENT. The FUNCTIONAL SWAPPER (section 5) is the backstop, and the ND-5800 microengine track
(section 6) is only about the CPU's own control storage. Nothing from this session is committed yet.

---

## 2. Verified fixes IMPLEMENTED this session (for the architect)

All in the canonical tree `E:\Dev\Repos\Ronny\RetroCore\`. NOT committed.

### 2a. Mailbox function 17B = 3DEPR DEPOSIT REGISTER
- Carve: N5XXC dispatch slot 17 = `DEPRG` (`MP-P2-N500.NPL:397`), symbol `3DEPR=000017`; queue-only,
  answer needs only ANSWER(3), NO data read-back (same contract as the 21B 3WREG twin). Twin 16B =
  `3EXAR` EXAMINE REGISTER. CLASSIC-500 register family (21B is MSG_ILLEG on the 5800 B30 image;
  16B/17B are classic-only by family). Single-register message layout is INFERRED, not byte-pinned.
- Change: added `ExamineRegister=14` / `DepositRegister=15` to `N5MicroFunction`
  (`Emulated.HW\ND\CPU\ND500\Servicer\N5MailboxProtocol.cs`) + a servicer case
  (`Nd500MicrocodeServicer.cs`): Classic answers ANSWER(3) (queue-only ACK; does NOT mutate the
  register stash because the layout is unverified), Samson5800 answers 5ERANSWER (MSG_ILLEG, matches
  the B30 microcode). Added MICFU names for 16B/17B/20B/21B to the trace helper.
- Result: servicer/mailbox tests 94/94 green (no regression). Bring-up cycle no longer loops.

### 2b. Swapper PSEG/DSEG MMU mapping on the 23B start path
- Root cause: SINTRAN starts the swapper via 23B 3START with a 21B register image that sets P=0x04
  but carries NO segment/MMU descriptor. `StartProcessFromRegisterImage` / `OnStartProcess` set P
  but installed NO PST/PCB/page-table state and never enabled the MMU, so logical P resolved to
  unmapped memory (the CPU executed 0x55 garbage and trapped at 0x800).
- Change: `CpuND500.MapExistingPhysicalRegion(domain, seg, isProgram, physByteBase, byteLen,
  writable)` (`Emulated.HW\ND\CPU\ND500\CpuND500.Loader.cs`) - builds a PS_ASI page table pointing
  at ALREADY-POPULATED physical frames (no allocate/copy), installs PST + domain capability; the
  frames route to the MPM window automatically (RouteToMpm), the page table lives in local SystemBus
  memory (the MMU walk reads PTEs via SystemBus, not the MPM route).
  `Nd500CpuProcessBridge.InstallSwapperMapping()` - on the first 23B 3START, derives PSEG (largest
  contiguous 14B run) + DSEG (lowest dest up to PSEG) from the servicer's new `ResiwrLog`, maps
  program seg-0 -> PSEG and data seg-0 -> DSEG (I/D split), enables program+data MMU. Once only.
- Result: the swapper's illegal-instruction trap moved from 0x800 (unmapped garbage) to 0x04 (the
  real logical entry) = the mapping is CORRECT and the CPU now fetches the placed image.

### 2c. Diagnostics added to the boot harness (remove/guard before commit)
`E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests\ND100\Nd100SintranNd500BootHarnessTests.cs`:
`_traceNd500Swapper` (streams the ND-500 CPU trace to file at attach), `DumpSwapperMapping()` (dumps
the 21B image + CpuND500 regs + the 14B RESIWR dest layout + a non-zero byte scan of every placed
page). Plus `Nd500MicrocodeServicer.ResiwrLog` (records every 14B dest for the mapping derivation).
Also `CpuBase.SetTraceFileRange(lo,hi)` PC-range trigger gate for offline instruction tracing.

---

## 3. The verified placement data (for both audiences)

From the D4 harness `DumpSwapperMapping` on a real run:

- 21B register image: `firstReg=0`, `regCount=44`; **reg[0]=P=0x04**, CAD=CED=0 (domain 0, segment 0).
  Other non-zero: reg[16]=2, reg[18]=0x48480003, reg[25]=1, reg[26]=1. No segment/MMU descriptor.
- 44x 14B RESIWR placement (into the MPM window; ND-500 addr = window offset):
  - DSEG: one 256-byte block at 0x24800 (the rest of the ~218 KB DSEG is built at runtime).
  - PSEG: 43 contiguous 2 KB pages at 0x5A000 - 0x6F7FF. (DSEG base 0x24800 .. PSEG base 0x5A000
    are contiguous = the classic I/D split, both logical-base 0.)
- Byte scan of all 44 placed pages: **40 pages are entirely ZERO; only 4 have content, and it is
  TABLES not code:**
  - `0x6E000`: `49 00 4A 00 4B 00 ...` = sequential PFNs (0x49 = 0x24800>>11 = the DSEG base frame)
    -> the swapper's page/segment table SINTRAN built.
  - `0x6E800`: `DF 00 E0 00 ...` = higher PFNs (0xDF = 0x6F800>>11 = the PSEG top).
  - `0x6F000` (`02 C0 ...`) + `0x24800`+0xFB (`03 ...`) = small descriptors.
- The swapper entry (logical 0x04 -> phys 0x5A004) lands in a ZERO page: opcode 0x00 = illegal.

**Conclusion (VERIFIED): SINTRAN places only the swapper's PAGE TABLES + zero data pages. There is NO
executable macro-code in the swapper image. The swapper's instructions are the control-store
microcode loaded by "> Loading Control Store".**

---

## 4. FOR THE ND-5000 / MICROCODE LLM

This is the definitive confirmation that the swapper on the ND-5800 image is 128-bit control-store
MICROCODE, not ND-500 macro-code:

- SINTRAN loads the swapper in two parts: (a) "> Loading Control Store" = the 128-bit microcode
  (the actual swapper instructions), and (b) 44x 14B RESIWR into the MPM window = the swapper's DATA
  (page tables + zero-initialized runtime data, NOT code - proven by the byte scan in section 3).
- The functional CpuND500 is a MACRO interpreter; it cannot execute the 128-bit control store. When
  started (23B 3START, P=0x04) it fetches the DATA image (zeros) and traps illegal-instruction.
- What a real ND-5800 microengine would need to do at swapper start (the contract SINTRAN expects -
  byte-cited in `ND500-MAILBOX-MESSAGE-CATALOG.md` 7d):
  1. Run the swapper microcode (process 0) from its control-store entry.
  2. The swapper self-announces via a MON 377B (N5SWAP) trap: STOPR@11B:=MOCALL(1), NUMPA@12B:=4,
     MCNO@13B:=377B, first parameter VALUE = 1 (at message HW 0o101), N5STA:=3 (ANSWER), doorbell.
  3. SINTRAN's SWPDECODER then sets `SWMSG.N5STA := PSWWAIT(7)` = "swapper free" - the observable
     success state that stops "Loading Swapper" recurring and lets placement proceed.
  4. On RUN, the swapper microcode must SWAP IN the domain: bring the domain's PSEG/DSEG (from the
     placed image) into ND-500 memory at the domain's SA and set up the domain's MMU mapping, so the
     ND-500 CPU can execute the domain macro-code at SA = 0xB0000DD1 (segment 22).
- The mailbox micro-function protocol the microengine must speak is fully cataloged in
  `ND500-MAILBOX-MESSAGE-CATALOG.md` (13B/14B resident r/w, 12B cache, 16B/17B/20B/21B register
  examine/deposit/read/write, 22B/23B/24B/25B/26B start/continue, 3RMICV watchdog). The register
  family 06/07,16/17,20/21,32/33 is CLASSIC-500; on the 5800 B30 image 20B/21B are MSG_ILLEG.
- If the microengine track produces a genuine ND-5800 microcode swapper, it slots straight into the
  existing 3022/servicer wiring - the ND-100 side (SINTRAN + the emulated 3022) is proven correct
  through 23B 3START.

---

## 5. FOR THE ARCHITECT - the FUNCTIONAL SWAPPER plan (path to D4 without microcode)

Since the domain is real macro-code (section 3 / the LINKAGE-LOAD-H02 PSEG is 87.6% dense ND-500
code at VA 0xB0000000), we can reach `NLL:` by emulating the swapper's EFFECT instead of executing
its microcode. The pieces already exist in the servicer:

1. **Fake the swapper alive.** When the swapper's 23B 3START fires (distinguish it: P=0x04 /
   segment 0 / first start, vs the domain start whose P is in segment 22), DO NOT run CpuND500.
   Instead synthesize the swapper's MON 377B self-announce into its activation message. The writer
   already exists: `Nd500MicrocodeServicer.AnswerMonitorCallStop(savedP, monNumber=0o377, argCount=4,
   argAddresses, argValues={1,...})` (`Nd500MicrocodeServicer.cs:738`) writes STOPR:=MOCALL(1),
   NUMPA:=argc, MCNO:=377B, the strided param arrays, N5STA:=3, doorbell - exactly the 7d contract.
   SINTRAN's SWPDECODER then sets PSWWAIT(7) = "swapper free".
2. **Emulate the swap-in on RUN.** When RUN activates the domain, the swapper (which we faked) would
   normally page the domain segments into ND-500 memory. Emulate that: load the domain PSEG/DSEG
   (the extracted files at `...\scratchpad\nll-extract\LINKAGE-LOAD-H02.PSEG/.DSEG`, or the placed
   image) into the MPM window at the domain's physical location and install the domain's MMU mapping
   via the same `MapExistingPhysicalRegion` used for the swapper (domain=?, segment=22, P=SA).
3. **Run the domain.** The CpuND500 executes the domain macro-code at SA=0xB0000DD1 -> its MON calls
   route through the existing bridge (`OnMonitorCall` -> `AnswerMonitorCallStop`) -> terminal I/O ->
   the `NLL:` prompt.

OPEN / INFERRED items to pin during the build:
- Exactly how RUN requests the swap-in (which mailbox message / what the domain's activation msg
  carries) - needs a trace of the RUN path once the swapper is faked-alive (the "learn what messages
  it sends next" incremental loop).
- The domain's physical placement + segment-22 MMU geometry (from the DESCRIPTION-FILE / the domain
  PSEG/DSEG headers; DSEG file offset = VA-0xB0000000+0x57800 per prior carve).
- The 7d MON-377B arg addresses (VALUES are the gate: first value=1; addresses at HW 0o40+2k are
  written but SINTRAN does not read them at stop time per carver R2).

---

## 6. Open decisions / coordination

- **Who owns the swapper?** Functional swapper (architect, C#, this brief section 5) reaches D4 now;
  the ND-5800 microengine (5000 LLM) is the "real" long-term path. They are compatible - the
  functional swapper can be replaced by the microengine later without touching the ND-100 side.
- **Commit state:** nothing from this session is committed. The 17B fix + MMU mapping are clean wins
  worth a checkpoint; the harness diagnostics should be guarded/removed first.
- **The MapExistingPhysicalRegion / InstallSwapperMapping code is CORRECT for a genuine classic-500
  macro image** (it would run the swapper if the image had code); it is inert on this data-only 5800
  image, and is reused for the domain swap-in in section 5.

---

Prepared from this session's traces/scans; full citations in
`ND500-D4-RUN-BLOCKER-FINDING-2026-07-19.md` (sec 12a-12c) and
`ND500-MAILBOX-MESSAGE-CATALOG.md` (sec 7c-bis / 7d).
