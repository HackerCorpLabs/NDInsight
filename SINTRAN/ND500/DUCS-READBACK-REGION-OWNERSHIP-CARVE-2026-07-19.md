# DUCS read-back: region-23 / region-25 ownership carve (2026-07-19)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\DUCS-READBACK-REGION-OWNERSHIP-CARVE-2026-07-19.md`

Primary source (every octal address below is a word-address inside this segment, base 40000B):
`E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\segments-ref\030-S3SM5\030-S3SM5.asm`

Grade: **[V]** = byte-cited in the disassembly · **[I]** = inferred from the code logic · **[UNVERIFIED]** = could not confirm.

---

## 0. Framing correction that changes both answers

The routines at 044505-046103 are **the ACCP command library**, and the `SAA <n>` at each
entry is the **ACCP command code**, NOT a "region number". Proof (the codes line up exactly with the
octobus/ACCP command table in `CARVE-ANSWER-OCTOBUS-CSLOAD-MICROSTART-2026-07-18.md`):

| routine | entry | `SAA` | ACCP command |
|---|---|---|---|
| LPARP | 045467 | `SAA 21` (045473) | 021B CMLPA Load-Parameter-Pointer [V] |
| VPARP | 045531 | `SAA 22` (045544) | 022B CMVER Verify-Parameter-Pointer [V] |
| **JWWCS** | 045603 | `SAA 23` (045610) | **023B CMWWC Write-Control-Store** [V] |
| DIRWW | 045701 | `SAA 24` (045705) | 024B [V] |
| **JRWCS** | 045771 | `SAA 25` (045776) | **025B CMRWC / DUCS Dump-Control-Store** [V] |
| DIRRW | 046104 | `SAA 26` (046110) | 026B [V] |
| RUNSE | 046210 | `SAA 33` (046214) | **033B `CMRUN` = RUNTST** [V, corrected 2026-08-03] — was "033B STARTMIC [V]", and that name was WRONG. `0o33` is `CMRUN` in `N500-SYMBOLS.SYMB`, and the carved octobus arm `0x1B` reads no parameters and runs the self-test body. STARTMIC is `0o66` = `CMMIC` = arm `0x36`. The `SAA 33` observation was always right; only the name was wrong, read off adjacent manual sections (5.3.23/5.3.24). `RUNSE` issuing `CMRUN` is also the more coherent reading. See `SINTRAN/ND5000/CM-SYMBOLS-ARE-THE-OCTOBUS-ARM-CODES-2026-08-03.md`. |
| JSTOM | 046241 | `SAA 34` (046245) | 034B STOPMIC [V] |
| MCLSA/CONTA/STOPA/TERMA | 045220.. | `SAA 41..44` | emergency 241B/242B/.../244B TERMINATE [V] |

The **descriptor indices 23/24/25/26** are a *different* numbering: they are indices into the ND-100-side
descriptor table `control_block[21]`, hard-coded inside the four data-mover primitives:

| primitive | entry | descr. index | op | role |
|---|---|---|---|---|
| ABSWR | 044505 | `LDX 26` (044521) | `MOVEW` (044534) | block **write** → region 26 |
| ABSST | 044551 | `LDX 24` (044574) | `STATX` (044575) | single-word **store**, indexed → region 24 |
| ABSRE | 044613 | `LDX 25` (044627) | `MOVEW` (044641) | block move ← region 25 |
| ABSLD | 044656 | `LDX 23` (044672) | `LDATX` (044701) | single-word **load**, indexed ← region 23 |

Shared-memory role of each region (all in the X5OCT / octobus buffer + MFbus data areas):
- **region 26** = microcode block written by SINTRAN (CMWWC data). ND-100 WRITES it.
- **region 25** = microcode read-back block (CMRWC/DUCS data). ND-100 only READS it (ABSRE = move-in).
- **region 24** = per-command parameter block `{N, CS-address, ..., checksum-addend}`. ND-100 WRITES it
  (ABSST) before TRANS, for both the write and the read command.
- **region 23** = the read-back checksum **addend**. ND-100 only READS it (ABSLD). No ND-100 code path
  ever writes region 23 (ABSLD is the *only* accessor and it is a load). [V]

Command send/receive plumbing: `PUT8` 044251 appends a byte to the command buffer (descr. index 40);
`TRANS` 045053 transmits to the ACCP; `RECEI` 045145 waits for the reply (`MON 267` TMOUT at 045157).

---

## Q1 — Who writes region-23 (the addend), and what value?

**Answer: the ACCP writes region-23, as a RESULT of the CMRWC/DUCS command. SINTRAN never writes it.
This REFUTES the "SINTRAN writes region-23 during the write phase" hypothesis.** [V for owner, I for "= sum"]

### The addend source (exact), byte-cited in JRWCS @045771
```
045776  SAA 25            ; command = 025B CMRWC / DUCS
045777  JPL I 73 -> PUT8  ; command byte 25 into cmd buffer
046001  LDA ,B -100 ; 046002 JPL I 71 -> ABSST   ; store N   into region-24 param block
046004  LDA ,B -76  ; 046005 JPL I 66 -> ABSST   ; store CS-addr into region-24 param block
046007  JPL I 65 -> TRANS   ; *** transmit CMRWC to the ACCP, ACCP executes now ***
046011  JPL I 64 -> RECEI   ; receive reply (returns N in local -100)
046013  JPL I 63 -> GMESS   ; check reply / build error on nak
046022  LDT ,B -100 ; 046023 SHT ZIN 3           ; count = N*8  (each microword = 8×16b = 128b)
046026  JPL I 51 -> ABSRE   ; move region-25 read-back block into local buffer @ -77
046030-046045  running_sum = 0; for X in 0..N*8-1:  running_sum += buffer[X]  (16-bit ADD, wraps)
046046  JPL I 32 -> [ptr @046100 = 044656 = ABSLD]   ; *** read the ADDEND ***
046050  STA ,B -176        ; addend := ABSLD result
046052  LDT ,B -177        ; T := running_sum
046053  SKP IF DA UEQ ST   ; skip if addend != running_sum
046054  JMP 10 -> 046064   ;   (equal) -> MIN ,B -74  = SUCCESS
046055  SAA 4 ...          ;   (unequal) -> build checksum error (EILOCS)
```
- The 046046 helper `JPL I 32` resolves through the pointer word at **046100 = `044656`** (line 3307),
  which is **ABSLD**. ABSLD uses descriptor index **23** (`LDX 23` @044672) and `LDATX` @044701, index
  `local-100 = 0` (set `STZ ,B 100` @046025). So the addend = **region-23[0]**. [V]
- The read of region-23 happens **after** `TRANS` (046007) has already run the ACCP command and after
  `ABSRE` (046026) has pulled the read-back words out of region-25. Region-23 is therefore an **output of
  the CMRWC command**, produced by the ACCP, not an input SINTRAN staged. [V]
- **Required value:** JRWCS passes iff `region23[0] == (Σ region25[i], i=0..N*8-1) mod 2^16`. i.e. the
  ACCP must place in region-23 the plain 16-bit additive sum of exactly the N*8 words it deposited into
  region-25. (The compare @046053 is `UEQ`; the sum @046041-046043 is a wrapping 16-bit `ADD`.) [V]

### Contrast with the WRITE side (JWWCS = CMWWC), so the asymmetry is explicit
```
045624  JPL I 47 -> ABSWR   ; block-write the N*8 microcode words to region-26
045634-045643  running_sum = Σ(those words)
045644  RADD AD1 CM1 CLD SA DA   ; A := -running_sum  (two's-complement NEGATE)
045646  JPL I 24 -> ABSST   ; store the NEGATED sum into the region-24 param block (the addend)
045650  JPL I 24 -> TRANS   ; then transmit CMWWC
```
So on write, SINTRAN stores a **negated** addend into **region-24** (so the ACCP can check
`Σdata + addend == 0`). On read, SINTRAN reads a **positive** addend from **region-23** and compares it
to its own positive sum. Different region, different sign — region-23 cannot be a left-over of region-24,
and there is no code that copies one to the other. [V]

**Q1 verdict:** region-23 owner = **the ACCP** (during CMRWC/DUCS). Addend value the emulator must supply
= **the 16-bit sum of the N*8 read-back words it writes into region-25**. [V owner / I "=sum" — logic-forced]

---

## Q2 — How does the ACCP learn region-25's runtime address at CMRWC time?

**Answer: via SINTRAN, not by the ACCP walking ND-100 structures. The region addresses are conveyed to
the ACCP through the MFbus parameter area whose base SINTRAN handed over once with LPARP (021B), pointer
`0x00018000` = X5OCT octobus-buffer base. This is option (i).** [V mechanism / [UNVERIFIED] exact byte map]

Evidence:
- Region-25's ND-100-side address is a runtime descriptor resolution `LDDTX(control_block[21]+25)`
  (ABSRE @044625-044632: `LDX ,B -56` / `LDD ,X 21` / `LDX 25` / `RADD SD DX` / `LDDTX`). That resolved
  value is a **physical MFbus address**, which the ACCP can also see — but the ACCP has no way to address
  the ND-100 segment-relative `control_block[21]` table itself, so it cannot repeat this resolution. The
  only cross-processor handle is the pointer SINTRAN gave it. [V for the resolution, I for "ACCP can't repeat it"]
- That handle is **LPARP 021B**, observed on the wire as `00 01 80 00` = `0x00018000`
  (`CARVE-ANSWER-OCTOBUS-CSLOAD-MICROSTART-2026-07-18.md` §4, wire step 13). `0x18000` is the **X5OCT
  OCTOBUS-buffer window** (`SINTRAN\OS\06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md:341`:
  `0x018000 | ~32KB | OCTOBUS Buffers | X5OCT`). The command/param buffers (regions 40/33/24) live here.
  VPARP 022B then round-trips a 32-bit test word through this same area to prove the pointer works. [V]
- So the flow is: **LPARP once conveys the parameter-area base → the parameter block at that base lists
  the MFbus addresses/descriptors of the data regions (25, 26, 23, 24) → every subsequent CMWWC/CMRWC
  references those.** The per-command region-24 block that JRWCS writes before TRANS only carries
  `{N, CS-address}` (046001/046004); the region **addresses** are the static part set up around LPARP. [I strong]

### Observed param block decode
Given block: `{N=1, csWord=0, 0040 0000  0001 8000  0000 0000  0080 0000  0040 0000  0001 8000 ...}`.
Read as 16-bit N, 16-bit CS-word, then 32-bit MFbus descriptors (MS word first):
`0x00400000, 0x00018000, 0x00000000, 0x00800000, 0x00400000, 0x00018000`.
- `0x00018000` recurs = the LPARP parameter-area / octobus-buffer base (X5OCT). [V — matches the wire pointer]
- The block is a list of `{descriptor, base}`-shaped 32-bit pairs, one pair per data region. [I]
- **[UNVERIFIED]:** which pair is region-25, and the exact field semantics. The values `0x00400000` /
  `0x00800000` do **not** equal the observed region-25 base `0x0045D800` (MPM window `0x00420000` +
  `0x3D800`), so they are size/flags or a differently-scaled/relative encoding — I cannot map them to
  `0x0045D800` from the material in hand. The `LDDTX` descriptor-table format and the ACCP firmware
  (never dumped) would be needed to close this. **Do not treat the byte layout above as decoded.**

**Q2 verdict:** mechanism = **(i)** conveyed via the LPARP parameter pointer + MFbus parameter block; the
ACCP does not independently resolve `control_block[21]+25`. Exact param-block field → `0x0045D800`
mapping is **[UNVERIFIED]**.

---

## Emulator recipe (C# ACCP)

The emulated ACCP already keeps a copy of every microword written via **CMWWC (023B)** and already must
locate **region-26/region-24** in shared MFbus memory to service that command. **Reuse the identical
region-location mechanism for CMRWC** — region-25/region-23 are found the same way region-26/region-24 are.

On **CMRWC / DUCS (025B/0x15)**:
1. Read `N` and `CS-start-address` from the CMRWC parameter block (region-24 param area at the LPARP
   pointer — same place CMWWC reads its `N`/`CS-addr`).
2. Resolve the MFbus physical addresses of **region-25** (read-back destination) and **region-23**
   (checksum destination) via the same LPARP-conveyed descriptor list used to find region-26/24. If that
   resolution is not yet wired, the addresses are observable in shared MPM (region-25 base seen at
   word-address `0x0045D800`); prefer descriptor resolution over a hard-coded constant.
3. Write the `N` stored microwords (from the CMWWC copy, starting at `CS-start-address`), **8×16-bit words
   each (128 bits)**, sequentially into region-25 → `N*8` sixteen-bit words total.
4. Compute `sum16 = (Σ of those N*8 words) mod 2^16` and write it as a single word into **region-23[0]**.
   (Compute it over exactly the words you wrote to region-25 → self-consistent, passes independent of
   absolute addressing.)
5. Reply **Messack** on the command's srcOMD.

Result: JRWCS sums region-25 (046030-046045), reads region-23[0] as the addend (046046), and
`addend == sum` (046053) → success, no EILOCS. For the paired **CMWWC (023B)** just Messack and store the
words (no need to verify the negated region-24 addend — per prior carve, canned Messack on the load path).

---

## UNVERIFIED / open

- **[UNVERIFIED]** Exact param-block field layout and which 32-bit pair encodes region-25's `0x0045D800`
  base; the `LDDTX` descriptor format. (Needs the descriptor-table setup code and/or ACCP firmware.)
- **[I, not [V]]** That region-23's required value is *specifically* the positive 16-bit sum: forced by
  JRWCS's compare logic, but not observed against a live ACCP that actually fills region-23.
- **[I]** That the region-25/26/23/24 addresses are the *static* part of the LPARP parameter block rather
  than re-emitted per command; the per-command region-24 writes only carry `{N, CS-addr}`, consistent with
  this, but the one-time setup writer of the address list was not carved here.
- **[V-adjacent]** CSREA/CSWRI (FUNCS 023/024 @152165/152373) are the **3022-bus IOX** control-store
  path; the JWWCS/JRWCS + regions 23-26 path analysed here is the **ACCP/octobus** path. Do not conflate.
