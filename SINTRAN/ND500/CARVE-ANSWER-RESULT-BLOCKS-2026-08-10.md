# CARVE ANSWER — the MON 60B "answer result block" (offsets 40B-47B) decoded, per-command layouts

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\CARVE-ANSWER-RESULT-BLOCKS-2026-08-10.md`
**Date:** 2026-08-10. **Question (P4 of the ND-500 plan):** carve the per-MICFU answer RESULT
BLOCK layouts ("mailbox message word offsets 40B-47B and any others") for the MON 60B commands
that return data, so they can be modeled in the Classic `Nd500MicrocodeServicer`.

**Sources (all offsets reproduced from bytes with a direct read of the carve before publishing):**
- `tools\sintran-segment-carver\versions\L-VSX-500\segments\030-S3SM5.bin` (base `40000B`,
  big-endian) + `re\030-S3SM5.dis` (regenerated 2026-07-31) — the ND-500 System Monitor.
- `SINTRAN\NPL-SOURCE\NPL\5P-P2-MON60.NPL` (grade NPL — logic only, different revision).
- `SINTRAN\NPL-SOURCE\SYMBOLS\L07\N500-SYMBOLS.SYMB.TXT` (grade SYMBOL).
- `E:\Dev\Ronny\ND500UC\docs\MC\CONT-STORE-10611.md` — the REAL classic ND-500 microcode
  disassembly (round-trip verified 2026-08-10), for the microcode-side cross-check.
- `SINTRAN\ND500\nd-500-mon\mon60-callers\STATUS\` (caller side, prior carve).

Grades: **[V]** = read in the L07 bytes this session, **[SYMBOL]**, **[NPL]**, **[MC]** = read in
the CONT-STORE-10611 microcode listing, **[I]** = inferred, **[OPEN]** = not established.

---

## 0. HEADLINE — the "result block at 40B-47B" is NOT in the 5MPM mailbox message

The `,X 40`/`,X 43`/`,X 46`/`,X 47` stores in the FUNCS bodies land in the **MON 60 info block**
(the `ZPREG..ZPREG+5DFSIZE` block of the `N500DF` datafield, working copy `S500DF`), **not** in
the 5MPM message. The numeric collision with the message's `5PPA1=40/5PPA2=42/OSTRA=44`
parameter-address region (catalog §8 R1) is exactly that — a collision between two different
structures that both use octal-40-ish offsets.

**Layout of the result block [SYMBOL, L07 `N500-SYMBOLS.SYMB.TXT`]** — a 5-entry array of
3-word parameter records starting at info-block offset 40B:

| n | `5DDn` (32-bit VALUE, hi word first) | `5Pn` (user ADDRESS the value is copied to) |
|---|---|---|
| 1 | `5DD1=40` (`5D11=40` hi, `5D12=41` lo) | `5P1=42` |
| 2 | `5DD2=43` (`5D21=43` hi, `5D22=44` lo) | `5P2=45` |
| 3 | `5DD3=46` (`5D31=46` hi, `5D32=47` lo) | `5P3=50` |
| 4 | `5DD4=51` (`5D41=51` hi, `5D42=52` lo) | `5P4=53` |
| 5 | `5DD5=54` (`5D51=54` hi, `5D52=55` lo) | `5P5=56` |

Context symbols: `ZPREG=7, ZXREG=10, ZTREG=11, ZAREG=12, ZDREG=13, ZLREG=14, ZBREG=16,
5FUNC(TION)=20, 5DFSIZE=50` — so the info block copied around the system-monitor call covers
datafield offsets 7..56B, which contains the whole 40B-56B result array.

**Poisoned priors corrected by this carve:**
1. `re\ND500-SYSTEM-MONITOR\FUNCS-BODIES\README.md` said "`LDX ,B -11` = the message pointer".
   WRONG — `B-11` is the **info block** (see §1); the message pointer is **`B-67`**. (Corrected
   in that README in this change.)
2. `ND500-MAILBOX-MESSAGE-CATALOG.md` UNKNOWN #2 said the 40B-47B answer block "has NO symbol
   evidence — treat as NON-EXISTENT". Half right: it is not a *message* structure, but it very
   much exists — it is the `5DDn/5Pn` array above, and every FUNCS `,X 40+` store is one of its
   slots. (Catalog updated to point here.)
3. The nd-500-bus-interface skill's "VERSION (057B RMVER) ... returns CPU-DF descriptor-CACHED
   values (no hardware access)" is **wrong for the classic path**: RMVER's classic branch does a
   LIVE `3RMICV` mailbox round trip (§4.4). GETCP (170B) is the purely cached one.

---

## 1. The plumbing — how a FUNCS result reaches the MON 60 caller [V + NPL]

```
caller (nd-500-mon)             MON 60 params: addresses of caller locals
  -> N500M 5IFUNC[code]         marshals params into the info block (values -> 5DDn, addrs -> 5Pn)
  -> 5NOPAR                     MOVAA: N500DF+ZPREG (50B words) -> S500DF        [NPL 034460]
  -> FPT2ENTRY -> 5FP2E@142231  the ND-500 System Monitor (ND-100 code in 030-S3SM5)
  -> FUNCS[code]                performs the operation; WRITES RESULT VALUES INTO 5DDn
                                (the ",X 40/43/46/47" stores, X = B-11 = S500DF-ZPREG)
  <- return
  -> 5PT2RET                    MOVAA: S500DF -> N500DF+ZPREG                    [NPL 034757]
  -> post-return FUNCS(X)       the NPL "( 5 ) F U N C S" table @034534 - per-code routine
                                copies 5DDn -> [5Pn] (STDS0) or block-copies (TOUSMOVE)
  -> RET5                       back to the caller; caller reads its locals
```

There are **two different tables both named FUNCS** — do not confuse them:
- the system monitor's dispatch `FUNCS @142031B` in 030-S3SM5 (does the work);
- the NPL post-return table `FUNCS @034534` in `5P-P2-MON60.NPL` (copies results to the user).

**5FP2E frame map [V, entry code 142231-142267 + pool words byte-verified]:**

| Frame cell | Contents | Evidence |
|---|---|---|
| `B-11` | **info block base = `S500DF-ZPREG` = static cell `165777B`** (pool word `M[142345]=165777`) | `142247 LDX 76; 142250 STX ,B -11`; every result store `LDX ,B -11; STD ,X 40/43/46` |
| `B-67` | **logical (window) address of the caller process's 5MPM MESSAGE** (MESSBUFF) | computed 142261-142264 from the message bank field; every message field access `LDX ,B -67; ST?/LD? ,X 7/10/11/13/14` |
| `B-65` | `ABUFA` = message[140B-141B] = com-buffer ND-100 physical address (32-bit) | `142266 LDD ,X 140; STD ,B -65`; used as msg[11B-12B] in transfer builders |
| `B-56` | current ND-500 **CPU-DF** (descriptor) | `142270-142273` via GCPUDF-pool call; `LDT ,X -3` = IOX base for `IOXT` |
| `B-57` | secondary descriptor (used by CSLOA with offsets `57, -22, -30..-32, +50`) | `142231-142233` |

**Common helpers (pool words byte-verified):** `063007` = build message / set `MICFU := A`;
`063026` = second prep helper; `104236` = **send + wait for the mailbox answer**;
`141567` = success exit; `141600` = error exit; `142441` = register-number range check;
`051116` = WRTAG; `051052` = TAG read-back helper.

---

## 2. The post-return copy table — which 5DDn/5Pn each data-returning command uses [NPL 035117-035703]

Every MON 60B command that returns data does it through one of these routines (`RET5` = returns
nothing). This is the authoritative per-command *result-block* usage:

| code | NPL routine | copies to user |
|---|---|---|
| 000 RREG | `FRREG` | `5DD2 -> [5P2]` (register value, 32-bit) |
| 002/003/032/121 PMREAD/DMREAD/DAMR/RSWPDATA | `FPMREAD` | `5DD4 -> [5P4]` (no. of bytes read); block `TOUSMOVE(dest=5P3, len=5D42)` (the read data, from the MON60/com-buffer) |
| 010 RRGS | `FRRGS` | block `TOUSMOVE(dest=5P1, len=NREGS*4)` (all registers) |
| 012 PRSTART (RUNN) | `FPRSTART` | `5DD1 -> [5P1]` (STOP REASON); block `TOUSMOVE(dest=5P2, len=200B)` ("stop (trap) info") |
| 013 CONNFI | `FCONNFI` | `5DD5 -> [5P5]` (open-file number) |
| 015 RES5 | `FRES5` | block `TOUSMOVE(dest=5P2, len=11B)` |
| 016/116 REL5/XN5REL | `FREL5` | if process in BREAK state: same copies as `FPRSTART` |
| 023/157 RCNT (read control store) | `FRCNTS` | block `TOUSMOVE(dest=5P3, len=5D22*2)` |
| 026/030/067/130 DMEXA/PMEXA/SPRTE/SFSYDOM | `FDMEXA` | `5DD2 -> [5P2]` (examined 4 bytes / entry) |
| 041 RSTATU (STATUS) | `FRSTATU` | `5DD1 -> [5P1]`, `5DD2 -> [5P2]`, `5DD3 -> [5P3]` |
| 051/057 RIREG/RMICV | `FRMICV` | `5DD1 -> [5P1]` |
| 060 LIMEM | `FLIMEM` | block `TOUSMOVE(dest=5P1, len=74B)` (memory-conf table) |
| 070 GPRTE | `FGPRTE` | block `TOUSMOVE(dest=5P2, len=250B)` (process table entry) |
| 071/072 SSGTE/GSGTE | `FSSGTE`/`FGSGTE` | block `TOUSMOVE(dest=5P2, len=154B)` (phys-segment entry) |
| 073 RPHSG | `FRPHSG` | `5DD5 -> [5P5]`; block `TOUSMOVE(dest=5P4, len=5D32)` |
| 154 DBUGSW | `FDBUGSW` | no copy (releases CPU if `5D12=1`) |
| 156 NRMICV (read system info) | `FNRMICV` | block `TOUSMOVE(dest=5P1, len=134B)` |
| 166 RDTRACE | `FRDTRACE` | block `TOUSMOVE(dest=5P2, len=5D12*24B)` (160 bits/trace word) |
| 170 GCPUTYPE | `FGCPUTYPE` | `5DD1 -> [5P1]` (CPU type), `5DD2 -> [5P2]` (mic version + parameter) |
| 172 RSCRREG | `FRSCRREG` | block `TOUSMOVE(dest=5P3, len=5D22*4)` (4 bytes/SRF register) |

(`STDS0` = store the 32-bit `AD` at the user address; `TOUSMOVE` = block copy to the user from
the MON60 buffer.) Histogram (062-066), RPROC 077 (read message) and the log families are
handled in `5IFUNC` on the resident side and copy independently (e.g. `IREAHIST` copies
`5HIDATA` via `TOUSMOVE(5P1, 200B)` + `5HIOUTSIDE -> [5P1+200B]` [NPL 032303-032312]).

---

## 3. Which FUNCS bodies write those slots, and where the data comes from [V bytes]

### 3.1 Mailbox-answer commands — the message offsets read AFTER the answer

These are the commands whose result the **microcode** must produce. All message offsets are
ND-100 word offsets within the 5MPM message (`X = B-67`).

| FUNCS | MICFU sent [V] | request fields written [V] | answer fields READ after `104236` send+wait [V] | lands in |
|---|---|---|---|---|
| `000 REGRE @142365` | **16B `3EXAR`** (`SAA 16`) | `msg[7]` = register number (`5D12`, range-checked @142441) | **`msg[10B-11B]` = 32-bit register value** (`142403-142406`) | `5DD2` |
| `001 REGWR @142410` | **17B `3DEPR`** (`SAA 17`) | `msg[7]` = register number, `msg[10B-11B]` = 32-bit value (`5DD2`) | none (status-only answer) | — |
| `026 DMEXA @153051` | **06B `3EXAD`** (`SAA 6`) | `msg[7-10B]` = 32-bit address (`5DD1`), `msg[14B]` = word from `[B+55]` (qualifier, identity [OPEN]) | **`msg[11B-12B]` = examined 32-bit value** (`153073-153076`) | `5DD2` |
| `030 PMEXA @153042` | **32B** (`SAA 32`) | same as DMEXA | same: `msg[11B-12B]` | `5DD2` |
| `027 DMDEP @153107` | **07B `3DEPD`** (`SAA 7`) | `msg[7-10B]` = address, `msg[11B-12B]` = value (`5DD2`), `msg[14B]` = `[B+55]` | none | — |
| `031 PMDEP @153100` | **33B** (`SAA 33`) | same as DMDEP | none | — |
| `002/003/032/121 PMREA/DMREA/AMEMR/FSWPR @142456-142616` | **34B / 10B / (per-variant)** (`SAA 34/10/...`) | via shared builder: `msg[7-10B]` = ND-500 address (`5DD2`), `msg[11B-12B]` = `ABUFA` com-buffer physical address (`B-65`), `msg[13B]` = byte count | answer data is **DMA'd by the microcode into the com-buffer** at `msg[11B-12B]`; post-return `TOUSMOVE` hands it to the user | user buffer `5P3` |
| `057 RMVER @164652` (classic branch @164727) | **1 `3RMICV`** (`SAA 1` @164733, gated on `CPUDF[10B]` bit 0) | header only | **`msg[7]` = microprogram version halfword** (`164740-164744`: -> `5D12`, `5D11` zeroed) | `5DD1` |
| `037 CSLOA @153441` tail (@154503-154530) | **1 `3RMICV`** (`SAA 1` @154512, after load + micro start) | header only | **`msg[7]` compared against `CPUDF[-7]`** (the version word taken from the control-store FILE); mismatch -> error via pool 23; **`msg[10B]` -> `CPUDF[20B]`** (CPU parameter cache) | CPU-DF cache |
| `166 DUMPT @174626` | trace family (`SAA 74`) | `msg[7]`, `msg[10B-...]` params | trace data block via com-buffer | user via `5P2` |
| `172 RSCRR @174776` | **77B LOOKSRF** (`SAA 77`) | `msg[7]` = first SRF register, `msg[10B]`, `msg[11B-12B]` | SRF data via com-buffer | user via `5P3` |
| `162-165 INITR/CLRTR/ARMTR/DISAR @174526+` | **70B family** (`SAA 70` @174552) | `msg[7], msg[10B], msg[11B], msg[12B]` params (`174536-174552`) | status-only | — |

Notes:
- **REGWR byte-pins the catalog §7c "layout (INFERRED)" for 17B `3DEPR`:** word 7 = register
  number, words 10B-11B = the 32-bit value. And REGRE shows the single-register READ twin is
  **16B `3EXAR`** (not 20B `3RREG`, which is the register-BLOCK read used by REGSR/REGSW
  10B/11B), answering into words 10B-11B.
- `msg[14B]` (the `MSWMC` slot) is written on the examine/deposit paths with a word from the
  monitor's own datafield (`LDA ,B 55`); its meaning (process/domain qualifier?) is [OPEN].

### 3.2 RSTAT (041B) — the STATUS command, incl. the "third word" (target 1)

The caller (`nd-500-mon` STATUS @127551) passes THREE result addresses; `FRSTATU` copies
`5DD1/5DD2/5DD3` back. `RSTAT @156064` fills them **without any mailbox traffic** [V]:

```
156064 JPL I 12 -> sub1 @156102 (status + third word; skip-return on success)
156066 LDX ,B -11
156067 STD ,X 40        ; 5DD1 := status double
156070 STZ ,X 46        ; 5D31 := 0
156071 STT ,X 47        ; 5D32 := T = the "third word"
156072 JPL I 6 -> sub2 @156154 (MAR)
156074 STD ,X 43        ; 5DD2 := MAR
156075 JMP I 4 -> 141567 (OK exit)
```

- **sub1 @156102**: if `CPUDF[27B] & 7 == 3` (SAMSON — mask word `M[156146]=000007` [V]) then
  status halves := 0/0 and third word := -1. Else (classic): one half = live **`IOXT` read of
  `RSTA5`** (`LDT ,X -3; AAT 2; IOXT` — CPU-DF holds the IOX base at offset -3); the other half
  = a 5015-side register read through TAG: `WRTAG(14B CNTR); WRTAG(3 DUCLK); read @051052`
  (= the **CSCNT register** read back); the **third word** = `WRTAG(12B WAR); WRTAG(3 DUCLK);
  read @051052`, masked with `M[156152]=037777B` (= the **5015 WA register**, 14 bits).
- **sub2 @156154**: SAMSON -> 0; classic -> **two `IOXT` reads at offset 0 (`RMAR5`, LS first),
  MS part masked `M[156205]=000377B`** -> the 24-bit MAR.

So the three STATUS results are: **(1)** interface status double = {5015 CSCNT half, RSTA5
half} (which half is hi vs lo not pinned here — the caller's decode loops print "ND-100" bits
from one half, "ND-500" bits from the other [caller carve]); **(2)** MAR (24-bit);
**(3)** the 5015 **WA register & 37777B** (`-1` on ND-5000). The *meaning* of the WA read-back
as a status ("last control-store address touched"?) is **[OPEN]** — the identity of the read is
[V], its interpretation is not. This resolves the STATUS folder's "UNKNOWN: identity of the
third returned word" at the mechanism level.

### 3.3 GETCP (170B) — pure cache read [V @174676-174722]

```
5DD1 := (0, CPUDF[27B] & 7)                       ; CPU TYPE (1=OLD500, 3=SAMSON; mask word = 000007)
if CPUDF[10B] bit 0:  5DD2 := (CPUDF[20B] << 16) | CPUDF[-7]   ; (CPU parameter, micro version)
else:                 5DD2 := 0
```

No hardware or mailbox access at all. The cache cells are:
- **`CPUDF[-7]` = microprogram VERSION** — written by CSLOA **from the control-store FILE
  image** (`154034 SAX 7; LDA I ,X 144` = file word 7 in one loader branch; `155126 SAX 10;
  LDA I ,X 112` = file word 10B in the other; buffer base pool `M[154200]=M[155240]=166056B`
  [V]) and range-classified into a type code merged into `CPUDF[27B]` low 3 bits (mask
  `M[155243]=177770B` [V]).
- **`CPUDF[20B]` = CPU PARAMETER** — zeroed at load (`155132 STZ ,X 20`), then written from
  **`msg[10B]` of the post-load 3RMICV answer** (`154525-154530`).
- `CPUDF[10B]` bit 0 = the "version valid" gate; its writer was not located this session [OPEN].

### 3.4 SPRES (043B) — target 2 verdict: it stores INTO the request, returns nothing

`FUNCS[043] = ERRFP` (no system-monitor op); post-return `FUNCS[043] = RET5` (no copy);
`ISRES` [NPL] tests `5D11/5D12` (=0,0 -> reserve this CPU only; else whole system) and exits
via `5OKRET`. The single `STD ,X 6` the J04 caller performs (mon60-callers/043B-SPRES) is
therefore **parameter 1 IN (the scope selector consumed as `5DD1`)**, not a returned word.
No answer-block modeling is needed for SPRES/SPREL. The "presence"-flavored data commands are
GETCP (§3.3) and RSTAT (§3.2).

---

## 4. Classic microcode cross-check — CONT-STORE-10611 [MC]

The real classic control store confirms the mailbox answer model from the other side:

- **Function dispatch = `JMPREL` into a table at `007636 + fn`** (`007632-007635`: MICFU masked
  16-bit, **range-checked against `50B`**, `A+1 ... JMPREL`). Entries beyond the implemented set
  jump to `007740`.
- **Illegal function @007740**: `SARG=4 -> AL#25; JMP 011405` — answers with status **4
  (`5ERANSWER`)**. `011405` is the common completion: `AL#25 -> AM#20; JSR 007550` writes the
  status halfword to ND-100 memory, then TAG status/reset-activate ops + doorbell.
- **The TAG-OUT DMA helper family @007540-007564** matches the carved TAG truth exactly:
  `007543/007544` = DMA READ of ND-100 memory at MAR (TAG codes `206/006`, MOST+LEAST),
  `007546/007550` = DMA WRITE (codes `207/007`), `007552/007553` = read MAR (codes `000/200`),
  `IODIN/IODOUT` = the data latch.
- **fn 1 (3RMICV) @007637 -> 011430**: `JSR 007777` loads **`LARG 24563B`** into AM#20 —
  **24563B = decimal 10611 = the image's own identifier stamped in microword 0 bits 15-0** —
  then `JSR 007550` (ONE halfword DMA write), then `JMP 011405` (answer status 3). So the
  classic version answer **is sourced from the loaded image** and is the same value CSLOA
  extracted from the file, which is exactly why CSLOA's `msg[7] == CPUDF[-7]` verify passes.
  In THIS image the CPU-parameter halfword is **not written** by fn 1 [MC] — `msg[10B]`
  keeps whatever it held (SINTRAN caches it regardless). The 5800/B30 microcode writes both.
- **fn 44B (3RPREG, histogram) @007702 -> 007721**: samples **`A,P`** (the macro P register),
  writes it with `007546` (a MOST+LEAST pair = 32 bits), then two more halfword writes
  (`AM#10 - 1` = current process, and `AM#15`) before `011405`. Matches SINTRAN's HISTSAMPLE,
  which reads the sampled P as a 32-bit double at **`N500A = msg[7-10B]`** [NPL 135103].
- The landing offsets of these sequential DMA writes depend on where the fetch loop leaves MAR;
  the SINTRAN read side ([V]: `msg[7]`/`msg[10B]`) pins them in practice. The microcode fetch
  loop's MAR bookkeeping was not fully decoded [OPEN — cosmetic].

---

## 5. What the Classic `Nd500MicrocodeServicer` must model (consequences)

1. **3RMICV (MICFU 1):** write the version halfword to `msg[7]` and the CPU-parameter halfword
   to `msg[10B]`, then `N5STA:=3`. The version MUST equal the version word of the **loaded
   control-store image** (classic 144-bit/9-part image: the 16-bit part holding microword-0
   bits 15-0, which SINTRAN's CSLOA reads out of the file at word offset 10B in the classic
   loader branch; = `24563B` for CONT-STORE-10611) — otherwise **CSLOA fails its post-load
   verify and LOAD-CONTROL-STORE errors out even though the store loaded**. CPU parameter: the
   10611 image would leave it unchanged; writing 0 (or a modeled parameter) is safe — SINTRAN
   only caches it into `CPUDF[20B]` for GETCP to report.
2. **16B `3EXAR` (register examine):** answer the 32-bit register value at `msg[10B-11B]`.
   **17B `3DEPR` (register deposit):** consume `msg[7]` (reg number) + `msg[10B-11B]` (value);
   status-only answer. (Byte-pinned; upgrades catalog §7c.)
3. **06B/32B examine:** answer the 32-bit value at `msg[11B-12B]` (request address at
   `msg[7-10B]`). **07B/33B deposit:** consume `msg[11B-12B]`; status-only.
4. **Memory reads (34B/10B family):** DMA the data to the ND-100 physical address in
   `msg[11B-12B]` (the com-buffer `ABUFA`), byte count `msg[13B]` — same shape as the already
   modeled 13B/14B RESIRD/RESIWR.
5. **44B histogram:** write the sampled P as a 32-bit double at `msg[7-10B]` (per SINTRAN's
   consumer; the extra process/AM#15 halfwords are unread by SINTRAN).
6. **Answers carrying data for STATUS/GETCP need NO microcode support**: RSTAT is IOX + TAG
   read-back on the 3022/5015 (the emulator's register model already covers RSTA5/RMAR5; the
   `WRTAG CNTR/WAR + DUCLK + read` path must return the CSCNT and WA registers), and GETCP is
   pure ND-100-side cache.
7. **Illegal MICFU:** status 4 (`5ERANSWER`) — the classic microcode does exactly this
   (`007740`), including for out-of-range codes > 50B.

---

## 6. Open items (marked, not guessed)

- Meaning of the RSTAT third word (5015 **WA** register & `37777B`) as a status quantity; and
  which half of `5DD1` is the ND-500 vs ND-100 status half (order inside sub1's epilogue not
  traced).
- `msg[14B]` qualifier written from `[B+55]` on examine/deposit paths.
- Writer of the `CPUDF[10B]` bit-0 "version valid" gate.
- Who writes `5DD4` (bytes-read count) for the memory-read family — the microcode answer or the
  ND-100 body; not traced this session.
- RMVER's two non-classic branches (octobus/`[B-57-22]` bit-3 path with the 4-word `MOVEW` from
  `[B-57+50B]`; region 164701-164726 partially data-garbled in the listing).
- The `SAA 32/33` examine/deposit codes used by PMEXA/PMDEP vs the N5XXC table's
  32=EXAMP/33=DEPMP naming ("program-memory examine" riding the P-space codes) — naming only.

## 7. Evidence appendix

Pool/mask words byte-verified directly from `030-S3SM5.bin` (python struct read, base 40000B):
`M[156076]=156102, M[156077]=141600, M[156100]=156154, M[156101]=141567, M[156146]=7,
M[156152]=37777, M[156204]=7, M[156205]=377, M[174721]=7, M[142433]=63007, M[142434]=63026,
M[142435]=141600, M[142436]=142441, M[142437]=104236, M[142440]=141567, M[156150]=51116,
M[156151]=51052, M[164771]=63007, M[164772]=104236, M[164773]=141600, M[164775]=141567,
M[154602]=63007, M[154603]=104236, M[154200]=166056, M[155240]=166056, M[155243]=177770,
M[142345]=165777` (all octal).

Cross-references: `re\ND500-SYSTEM-MONITOR\FUNCS-dispatch-table.md` (table addresses),
`mon60-callers\STATUS\README.md` (caller side), `ND500-MAILBOX-MESSAGE-CATALOG.md` §§7c/8,
`E:\Dev\Ronny\ND500UC\docs\MC\CONT-STORE-10611.md` + `docs\DISASSEMBLER-VALIDATION-2026-08-10.md`
(image identifier 24563B=10611, parity proof).
