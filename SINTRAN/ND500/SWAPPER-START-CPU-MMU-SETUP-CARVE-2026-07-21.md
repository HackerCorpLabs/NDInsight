# ND-5000 B30 Microcode: Process-Start CPU / MMU / Address Setup Carve

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\SWAPPER-START-CPU-MMU-SETUP-CARVE-2026-07-21.md`

**Question answered:** when a process (the swapper = process 0, or any process) is started via the
octobus mailbox (MICFU 23B/25B 3START, or 22B start-process-0), how does the microcode set up
the CPU registers, the MMU, and the code/data addresses?

**Ground truth:** `E:\Dev\Ronny\ND5000UC\microcode\MICRO-5800-B30.md` (lossless disassembly,
one 128-bit microword per row, B-series ND-5800/SAMSON image). Companion carve:
`E:\Dev\Ronny\ND5000UC\microcode\MAILBOX-MICROCODE-PSEUDOCODE.md`. Where the two disagree, the
microword listing wins.

## Evidence tags (strict)

| Tag | Meaning |
|---|---|
| **[V]** | VERIFIED directly in the microword listing (operand / destination / jump target explicit). |
| **[INFERRED]** | Reasoned from verified data flow plus a documented rule; NOT proven by the bytes alone. |
| **UNVERIFIED** | Could not establish from this listing. Do not build on it. |

Address-arithmetic decode rules used (from PSEUDOCODE.md section 3.10, calibrated on known
offsets): a memory op (`RD,POF`/`WR,POF`) on microword N uses the address computed by the
`ADACT` on microword **N-1**; `AA=2`=DISP(=DPA), `AA=7`=EA3 running pointer; `ORCON`=byte
displacement; units are BYTES (halfword offset = byte/2); `EA3SAVE` latches EA3 := the address.

---

## SUMMARY: the start -> context -> MMU -> execute flow

```
MSG_START (015671, MICFU 23B/25B)
  -> CPU_AVAIL? gate (015671-673)                     [V]
  -> MSG_START1 (015674): call NEWCNTXT                [V]
        NEWCNTXT (014660): SC12 := SRF11 (process #)   [V]
          if switching process -> CNTXTSAVE old, then CNTXTLOAD new
        CNTXTLOAD (014742): ctx = 0o4000 + 0o400*proc BYTES   [V]
          restore P, L, descriptor regs, X1-X4, A1-A4, E1-E4, status, MOD
          load per-process MMU: MM,PS / MM,PHS (segment),
                                MM,DOM / MM,ADOM (domain)      [V]
  -> EXECUTE (014636): arm traps, SET_RUNNING, enable I-cache,
        resume MACRO execution at the restored P            [V]
```

**One-line answer to the core MMU question:** the page/segment-table pointers `MM,PSTP` /
`MM,PUWP` are written ONLY with small CONSTANTS at CPU cold init (INIT_SAM and the macro-start
init), never per-process and never derived from the DMA'd image address. Per-process address
translation is switched entirely by loading the DOMAIN registers (`MM,DOM` / `MM,ADOM`) and
SEGMENT registers (`MM,PS` / `MM,PHS`) from the context block. [V]

---

## Deliverable 1: Context-block layout

### 1.1 Base-address formula [V]

The prompt hypothesis `ctx = 0o4000 + 0o400*idx BYTES` is CONFIRMED, with `idx = SRF11`
(the current-process number), NOT `X5CPU+1`:

| uWord | Field decode | Meaning |
|---|---|---|
| `014660` NEWCNTXT | `ALU,A A,SRF11 D,SC12` | SC12 := SRF11 (process number) [V] |
| `014742`-`014745` CNTXTLOAD | 4x `ALU,A+B,*2 A,SC12 B,SC12 D,SC12` | SC12 := ((((p*2)*2)*2)*2)... each `A+B,*2`=(SC12+SC12)*2=4*SC12; 4 words = 4^4 = **256** -> SC12 := 256*proc [V] |
| `000020` OFFSET | `ALU,A A,LARG LARG=00000004000 D,SC13 T,RETURN` | SC13 := **0o4000** constant [V] |
| `014747` | `ALU,A+B A,SC12 B,SC13 D,DAC,DPA` | DPA := SC12 + SC13 = **0o4000 + 256*proc** BYTES [V] |

CNTXTSAVE uses the identical computation (`014666`-`014671` 4x `A+B,*2`; `014672` calls OFFSET;
`014673` `DPA := SC12+SC13`) [V]. So the block base is symmetric for save and load.

Note `0o400` octal = 256 decimal; the block stride is 256 BYTES = 128 halfwords = 64 words per
process. [V]

**Correction to the mailbox-track prior:** the mailbox `idx = X5CPU+1` formula is for the per-CPU
MAILINK extension block in shared memory (poll/#CPUDF), a DIFFERENT structure. The per-process
CONTEXT block is indexed by `SRF11` (process number) and lives at `0o4000 + 0o400*proc` in
ND-500-local memory reached via `D,DAC,DPA`. [V distinction]

**GET_CNTXT is a different structure [V]:** `GET_CNTXT` (`013370`-`013372`) does only TWO
`A+B,*2` doublings (=*16) then `T,RETURN` via OFFSET. Stride 16 bytes, used by the TRAP path
(TRAP_FIND `013152`), NOT by process start. Do not confuse it with the 256-byte register
context block. [V]

### 1.2 Context-block offset -> CPU state (BYTE offsets from block base)

Verified from CNTXTLOAD reads (`014750`-`015000`) and CNTXTSAVE writes (`014701`-`014721`);
both agree on every slot.

| Byte off | Halfword off | CPU state | SAVE uWord | LOAD uWord |
|---|---|---|---|---|
| 0x00 | 0 | **P** (program counter = code entry) | `014702` WR SC3 (SC3:=IAC,P @014677) | `014751` RD->SC3; `014757` `IAC,P := SC3` [V] |
| 0x04 | 2 | **L** (link/return register) | `014703` WR SC4 (SC4:=IAC,L @014700) | `014752` RD->SC4; `014760` `IAC,L := SC4` [V] |
| 0x08 | 4 | descriptor B (`DAC,B`) | `014704` WR SC5 (SC5:=DAC,B @014674) | `014753` RD->SC5; `014755` `SC5->DAC,REG04` [V dest; name INFERRED] |
| 0x0C | 6 | restart/transfer descriptor (`DAC,XFER`/`LDRES`) | `014705` WR SC6 (SC6:=DAC,XFER @014675) | `014754` RD->SC6; `014756` `SC6->DAC,LDRES` [V dest; name INFERRED] |
| 0x10 | 010 | **X1** | `014706` WR X1 | `014762` RD->X1 [V] |
| 0x14 | 012 | **X2** | `014707` WR X2 | `014763` RD->X2 [V] |
| 0x18 | 014 | **X3** | `014710` WR X3 | `014764` RD->X3 [V] |
| 0x1C | 016 | **X4** | `014711` WR X4 | `014765` RD->X4 [V] |
| 0x20 | 020 | **A1** (address/base reg) | `014712` WR A1 | `014766` RD->A1 [V] |
| 0x24 | 022 | **A2** | `014713` WR A2 | `014767` RD->A2 [V] |
| 0x28 | 024 | **A3** | `014714` WR A3 | `014770` RD->A3 [V] |
| 0x2C | 026 | **A4** | `014715` WR A4 | `014771` RD->A4 [V] |
| 0x30 | 030 | **E1** | `014716` WR E1 | `014772` RD->E1 [V] |
| 0x34 | 032 | **E2** | `014717` WR E2 | `014773` RD->E2 [V] |
| 0x38 | 034 | **E3** | `014720` WR E3 | `014774` RD->E3 [V] |
| 0x3C | 036 | **E4** | `014721` WR E4 (IX*2) | `014775` RD->E4 [V] |
| 0x40 region | 040+ | status word(s) + domain bytes + MOD (second 32-word IX*2 block) | `014722`+ (READST/status) | `014776`-`015000` RD->SC3/SC4/SC5 [V regs; exact offsets INFERRED due to IX*2 scaling] |

The registers are the ND-500 macro register file: X1-X4 (index registers), A1-A4 (address /
base registers), E1-E4 (extended registers). "General registers R1..Rn" in the prompt = these
X/A/E banks. [V that these 12 registers + P + L + 2 descriptor cells are saved/restored]

### 1.3 The second (status/domain/MOD) sub-block [V regs, INFERRED offsets]

After E4, CNTXTLOAD reads three more words (IX*2 scaled) and dispatches them:

| uWord | Action |
|---|---|
| `014776` RD -> SC3 | first status/domain word from ctx [V read] |
| `014777` RD -> SC4 | second [V] |
| `015000` RD -> SC5 | third [V] |
| `015001` `SC3 -> WRITEST1` | reconstruct ALU/MIC/IDU status registers from SC3 (`015030`-`015035`) [V] |
| `015004` `SC13 := SC3; call NEW_PS_1` | load program segment (see Deliverable 3) [V] |
| `015005` `SPEC,MOD := SC12` | restore the MODUS register [V] |
| `015011` `SC13 := SC3 (byte); call NEW_CED` | current domain (see Deliverable 3) [V] |
| `015012` `SC13 := SC4 (byte); call NEW_CAD` | alternative domain [V] |

The exact byte offset that supplies the domain vs the status bits is IX*2-scaled and not cleanly
readable per word; the register DESTINATIONS are [V], the ctx byte offsets in this sub-block are
[INFERRED].

---

## Deliverable 2: CPU register setup at start (EXECUTE and the load path)

The load itself is done by CNTXTLOAD (Deliverable 1). EXECUTE (`014636`) then *begins running*:

| uWord | Field decode | Effect |
|---|---|---|
| `014636` EXECUTE | `ALU,FZRO AAP1,CLEAR D,IXC; call TRAP_ARM1` | clear index-cache pointer, arm traps [V] |
| `014637` | `AAP2,CLEAR; call SET_RUNNING` | mark CPU running (srf run-state flag) [V] |
| `014640`-`014642` | build IDU status mask from SRF10 | pending-trap merge [V] |
| `014645` EXECUTE_1 | `ISAMP; ... ADR_MOD` | sample / MOD address setup [V] |
| `014646`-`014647` | `SC5 := RF2; SC4 := SPEC,MOD` | stage MOD [V] |
| `014650` | `ALU,A SLOW2 A,IAC,P ... LOADLA` | **load look-ahead from P** = begin instruction fetch at the restored P [V] |
| `014651` / `014656` | `call ENA_IC` (`014656` ENA_IC: `MOD |= BM26`) | enable instruction cache and resume MACRO execution [V] |

**Program counter (P = code entry):** comes from **context-block offset 0x00**
(`014757 IAC,P := SC3`, SC3 = `mem[ctx+0]`). EXECUTE resumes fetch at that P
(`014650`/`014654` `A,IAC,P ... LOADLA`). [V]

**Link register L:** context-block offset 0x04 (`014760 IAC,L := SC4`). [V]

**Data base:** the ND-500 has no single "data base" register; data addressing uses the
**A1-A4 address registers**, restored from context-block offsets 0x20-0x2C
(`014766`-`014771`). Which segment/domain those logical addresses map through is set by the
MMU domain/segment registers (Deliverable 3). [V registers; "A-registers are the data base"
is standard ND-500 architecture, INFERRED from register naming]

**Not from an SRF cell, not computed:** every architectural register at start comes from the
per-process CONTEXT BLOCK in ND-500-local memory, read word-by-word by CNTXTLOAD. The only
SRF involvement is SRF11 (process number, selects the block) and SRF13/14/15/17 (scratch and
saved domain shadow). [V]

---

## Deliverable 3: MMU setup (THE core question)

### 3.1 Every WRITE to a page/segment-table pointer in the whole image [V, exhaustive grep]

`D,MM,PSTP`, `D,MM,PUWP`, `D,DMM,PSTP`, `D,DMM,PUWP`, `D,IMM,PSTP`, `D,IMM,PUWP`:

| uWord | Write | Source | Context |
|---|---|---|---|
| `014572` INIT_SAM_3 | `D,MM,PUWP` | `A,SC3` | CPU cold init (INIT_SAM chain) [V] |
| `014573` | `D,MM,PSTP` | `A,SC13` | CPU cold init [V] |
| `017731` | `D,MM,PSTP` | `A,BM00` via `ALU,FZRO` = **0** | macro-start / SIM_EXEC init [V] |
| `017732` | `D,MM,PUWP` | `A,BM02` = **4** | macro-start / SIM_EXEC init [V] |

There are **NO** writes anywhere to `DMM,PSTP`, `DMM,PUWP`, `IMM,PSTP`, or `IMM,PUWP` by those
names. [V - exhaustive]

The `017534`-`017557` block that mentions `A,DMM,PSTP` / `A,IMM,PSTP` etc. is the **LOOK_SRF
debug-read** path (`... D,SC1 -> LOOK_HW_WRITE`): those registers are READ (as ALU A source)
to display them, never written there. [V]

### 3.2 What PSTP/PUWP actually receive [V writes; values partly traced]

- INIT_SAM_3: `MM,PUWP := SC3`, `MM,PSTP := SC13`. Just upstream, `PSTBASE` (`000021`) loads
  `SC13 := 2` constant (`LARG=00000000002`), and `SC3 := SC13` at `014564`. The exact SC3/SC13
  values at `014572` depend on the conditional path through `014564`-`014571`; both derive from
  **small constants (PSTBASE=2, OFFSET=0o4000)**, NOT from any image address. [V that the
  sources are constant-fed; exact final value INFERRED / path-dependent]
- Macro-start init: `MM,PSTP := 0`, `MM,PUWP := 4` (BM02). Small constants. [V]

**Decisive fact:** PSTP/PUWP are loaded with SMALL CONSTANTS (0, 2, 4, 0o4000-class), so they
are configuration/base values into a FIXED system table region, not physical byte addresses
computed from where the swapper image was DMA'd. [V]

### 3.3 What DOES change per process: the DOMAIN and SEGMENT registers [V]

CNTXTLOAD loads these from the context block via three helper routines:

| Helper | uWord | Write | Meaning |
|---|---|---|---|
| NEW_PS_1 | `015043` | `MM,PS := SC13` (halfword) | **program segment** [V] |
| NEW_PS_1 | `015044` | `MM,PHS := SC13` (halfword) | physical segment [V] |
| NEW_CED | `015053`-`015054` | `SRF14 := SC13; MM,DOM := SC13` (byte) | **current execution domain** [V] |
| NEW_CAD | `015055`-`015056` | `SRF15 := SC13; MM,ADOM := SC13` (byte) | **alternative domain** [V] |

The caller sequence inside CNTXTLOAD: `015004 SC13:=SC3; call NEW_PS_1` (segment),
`015011 SC13:=SC3(byte); call NEW_CED` (current domain), `015012 SC13:=SC4(byte); call NEW_CAD`
(alt domain). SRF14/SRF15 keep a software shadow of the two domain bytes. [V]

### 3.4 The MMU model this proves

- The ND-5000 MMU is **domain + segment** organized. `PSTP` (page/segment table pointer) and
  `PUWP` point at a FIXED table structure in ND-500-local memory. [V that PSTP is set once from
  a constant]
- Per-process, the microcode does NOT re-point PSTP/PUWP; it switches the **DOMAIN**
  (`MM,DOM`/`MM,ADOM`) and **SEGMENT** (`MM,PS`/`MM,PHS`) registers, which index into the
  PSTP-rooted table to select this process's page mappings. [V that only domain/segment change;
  the "index into a domain-keyed table" mechanism is INFERRED from the register roles and the
  fact PSTP stays constant]
- Instruction space (I-space, IMM) and data space (D-space, DMM) are two VIEWS through the same
  written `MM,*` registers. Because there is no separate write to `IMM,PSTP`/`DMM,PSTP`, the
  hardware evidently fans the single `MM,PSTP`/`MM,PUWP`/`MM,PS`/`MM,DOM` write out to both the
  I-side and D-side units, or `MM,*` is the write alias and `IMM,*`/`DMM,*` are the read-back
  views. [INFERRED - the listing shows only `MM,*` writes and `IMM,*`/`DMM,*` reads]

---

## Deliverable 4: Code vs data address origin

**Where the code (PSEG) address comes from:** the process's **P** register = `mem[ctx+0]`
(`014757`). This is a LOGICAL ND-500 address. It is translated by the I-space MMU using the
loaded `MM,PS` (program segment) and `MM,DOM` (domain), which index the PSTP-rooted tables. [V
for P source and for MM,PS/MM,DOM load; translation mechanism INFERRED]

**Where the data (DSEG) address comes from:** data references go through the A1-A4 address
registers (`mem[ctx+0x20..0x2C]`) and are translated by the D-space MMU using `MM,DOM`/`MM,ADOM`
(current + alternative domain). [V for A-register source and domain load]

**Does the microcode tie PSTP/PUWP to the image load address?** **NO.** [V] PSTP/PUWP are set
from constants at init only (Deliverable 3.2) and are never recomputed from where the 14B/RESIWR
copy family placed the image. Therefore:

> After the swapper image is DMA'd into ND-500 memory, the microcode does NOT itself learn where
> PSEG/DSEG physically live. It assumes the page/segment tables (rooted at the constant PSTP) and
> the per-domain/per-segment descriptors ALREADY EXIST in ND-500-local memory, built by
> SINTRAN/ACCP. The microcode's whole job at start is to select the right domain+segment for the
> process (MM,DOM/MM,ADOM/MM,PS from the context block) and resume at P. [V for "microcode does
> not derive it"; "SINTRAN/ACCP build the tables" is INFERRED - not provable from this listing,
> but it is the only remaining producer]

This matches the D4 RUN-blocker finding: real placement (page/segment tables) must be built by
SINTRAN's genuine swapper/placement path; a faked swapper leaves the tables unbuilt, so a started
domain has no valid PSTP-rooted mapping.

---

## Deliverable 5: INIT_SAM chain (full trace)

**Entry:** `INIT_SAMSON` (`014517`), reached from the reset vector `SAMSON` (`000000`,
`COND,MSEXO ... INIT_SAMSON`) and from `014516` (`G,OOPS -> INIT_SAMSON`). This is CPU COLD
INIT, run at control-store start / micro-start, BEFORE any process executes. NOT per-process. [V]

Sequence (all [V] destinations):

| uWord | Action |
|---|---|
| `014517` | `LC := BM14-1` (loop count) |
| `014520` | `MIC,CNT32 := 0` |
| `014521`-`014522` INIT_SRF | zero the SRF register file (`RF2D` loop, LCDECR / LCZ) |
| `014524`-`014525` | `SPEC,MOD := BM21`; `RF2 := BM21` |
| `014527` INIT_FROM17 | `SRF11 := 0; call SET_IDLE` |
| `014530`-`014531` | clear CPU-available cell (ADR_CPUAVA) |
| `014532` | call INIT_CLRSTS (`014606`: clears MIC,TE / IDU,TE / MIC,STS / IDU,STS / MIC,MISTS / MM,CTRP / SPEC,TRPCLR) |
| `014533` | call INIT_REG (`014621`: seeds SRF0..SRF7 with math constants; register file base) |
| `014534` | call INIT_ADRP (`025646`: computes `#CPUDF = START_MESS + SAMSON_CPU<<8`, writes srf[2017]) |
| `014535`-`014541` | clear caches: CLR_DC, CLR_IC, CLR_DTSB, CLR_ITSB; TRAP_ARM |
| `014542`-`014546` FILL_CC | fill the cache-control array (`SPEC,MIB`, `SPEC,CC`), zero-instruction fill |
| `014552`-`014561` | set up SYSPAR SRF cells; call SYS_READ (`017111`: 3 system-param words), SYS_DATAF (`025630`: X5MXF/X5FIF ring header) |
| `014564` | `SC3 := SC13` (conditional, INVSEQ COND,MZRO -> INIT_SAM_1) |
| `014567` INIT_SAM_1 | `SC4 := SC13; call PSTBASE` (PSTBASE `000021`: SC13 := 2 const) |
| `014570` | `SC5 := SC13` |
| `014572` INIT_SAM_3 | **`MM,PUWP := SC3`** [V] |
| `014573` | **`MM,PSTP := SC13`** [V] |
| `014574`-`014575` | call CPU_READ; `SRF11 := SC14-1` (=-1, no runnable process) |
| `014602` INIT_SAM_4 | `SC12 := SC14; call PRNOWR`; `call UNLOCK_QUE`; `-> IDLE` (`014605`) |

So INIT_SAM: (1) zeroes SRF and status/trap state, (2) clears all four caches, (3) establishes
the shared-memory comm block pointer `#CPUDF`, (4) reads system parameters and the notification
ring header, (5) sets the MMU page/segment table pointers `MM,PSTP`/`MM,PUWP` from **constants**
(PSTBASE=2 class), (6) marks "no process running" (SRF11 := -1) and drops into the IDLE mailbox
loop. It runs ONCE at micro-start; process starts thereafter go through
MSG_START -> NEWCNTXT -> CNTXTLOAD -> EXECUTE and never revisit PSTP/PUWP. [V]

**Second MMU-init site (`017725`-`017737`, MACRO_ST1 / SIM_EXEC path):** a "bare macro state"
reset that zeroes `MM,CTSB`, sets `MM,PSTP := 0`, `MM,PUWP := 4`, `MM,PS := 0`, `MM,DOM := 0`,
`MM,ADOM := 0`, `MM,PHS := 0`, then `SPEC,MOD := SC2` and jumps to MACRO_SETP / SIM_EXEC_1
(`017771`, which SET_RUNNINGs). This is the alternate cold macro-entry (e.g. first-ever start /
CS-load hand-off), also constant-fed. [V]

---

## POISONED PRIORS / OPEN QUESTIONS

1. **`X5CPU+1` context-base prior is WRONG for the register context block.** The per-process
   register CONTEXT block is `0o4000 + 0o400*SRF11` (process number), verified. `X5CPU+1` is the
   per-CPU MAILINK extension block (mailbox poll), a different structure. [V correction]

2. **Exact PSTP/PUWP numeric values at INIT_SAM_3 are path-dependent.** SC3/SC13 flow through
   conditional words `014564`-`014571`; both derive from constants (PSTBASE=2, OFFSET=0o4000),
   but the final value on any given cold start is INFERRED, not byte-pinned. The *point* (small
   constant, not image address) is [V].

3. **The domain-keyed page-table walk is INFERRED.** The listing proves PSTP stays constant and
   only MM,DOM/MM,ADOM/MM,PS/MM,PHS change per process. The claim that PSTP roots a table indexed
   by domain+segment is the natural reading of the register roles but is not shown as an explicit
   memory walk in the start path (the walk is in hardware/the fault path, not the context load).

4. **"SINTRAN/ACCP build the page/segment tables" is INFERRED.** The microcode never builds them
   during process start; it assumes they exist. The producer is not in this listing. This is the
   microcode-side confirmation of the D4 RUN blocker (tables must come from SINTRAN's real
   placement, not the faked swapper).

5. **IMM vs DMM PSTP/PUWP fan-out is UNVERIFIED at the RTL level.** Only `MM,*` is written;
   `IMM,*`/`DMM,*` appear only as read sources (LOOK_SRF). Whether `MM,PSTP` physically fans to
   both units or `IMM,*`/`DMM,*` are read-back aliases of the same register is not decidable from
   the microword listing alone. The C# model (`MmsUnit.cs`) should treat a `MM,PSTP` write as
   updating BOTH the I-space and D-space table pointer until proven otherwise.

6. **Context second-block byte offsets (0x40+) are INFERRED.** The IX*2 scaling on
   `014776`-`015000` prevents clean per-word offset reading; the register destinations
   (status/domain/MOD) are [V], the exact ctx byte positions are not.

7. **Descriptor cells at ctx+0x08 / ctx+0x0C** (`DAC,B` / `DAC,XFER`/`LDRES`) are saved/restored
   [V] but their architectural meaning (segment descriptor B / transfer-restart descriptor) is
   INFERRED from the DAC mnemonic.
