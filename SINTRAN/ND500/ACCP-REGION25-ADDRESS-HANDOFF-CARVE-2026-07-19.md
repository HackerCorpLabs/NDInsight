# ACCP region-25/23 address handoff — carve result (2026-07-19)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ACCP-REGION25-ADDRESS-HANDOFF-CARVE-2026-07-19.md`

**Question:** How does the ND-5800 ACCP (MC68000 octobus card) learn the address of the
control-store read-back buffer ("region-25") so it can DMA the dumped microwords into it during
the DUCS / CMRWC (025B) command?

Grades: **[V]** = byte/NPL-cited here · **[I]** = inferred from cited code logic ·
**[UNVERIFIED]** = could not confirm from the material in hand.

Primary sources:
- ASM: `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\segments-ref\030-S3SM5\030-S3SM5.asm`
  (word-addresses, base 40000B).
- NPL: `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\RP-P2-N500.NPL` (XMSINIT buffer-base builder),
  `5P-P2-MON60.NPL` (buffer-pool allocation), `DP-P2-VARIABLES.NPL` (pool vars).
- Prior carves in this folder: `DUCS-READBACK-REGION-OWNERSHIP-CARVE-*`, `LDDTX-REGION-RESOLUTION-CARVE-*`,
  `..\ND5000\CARVE-ANSWER-ND5000-ACTIVATION-WORKFLAG.md`, `..\OS\06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md`.

---

## 0. VERDICT — mechanism (d), conveyed as (b). NOT (a), NOT (c).

**The read-back buffers live in the shared 5MPM memory. Each processor addresses that same
memory in its OWN address space: the ACCP in ND-500 (5MPM-relative) addresses, the ND-100 in
window-physical addresses. The ACCP is handed the base of its address view ONCE, at LPARP time,
as `X5OCT` (the octobus-buffer base). It finds region-25/23 at fixed offsets from that base. The
absolute ND-100-physical value (region-25 ≈ 0x00B98xxx) never travels to the ACCP and the ACCP
never needs it.** This is architecturally option **(d)** (buffers in shared memory, ACCP knows
them by its own addressing), with the base conveyed by option **(b)** (LPARP `X5OCT` + fixed layout).

- **(a) REFUTED** — the CMRWC/CMWWC command carries NO region address. Proof below (§1).
- **(c) REFUTED** — the ACCP never reads the ND-100-private region table `control_block[21]`.
  Proof: prior `LDDTX` carve — that table is reached only via the ND-100 `B` register and ND-100
  virtual memory, neither visible to the ACCP. [V, prior carve]

---

## 1. (a) is refuted: the command transmits only {N, CS-addr}

`JWWCS` (CMWWC 023B) and `JRWCS` (CMRWC/DUCS 025B) stage exactly two scalars into the per-command
parameter block (region-24) before `TRANS`, and never a region address:

JWWCS @045603 (030-S3SM5.asm):
```
045610  SAA 23             ; command byte = 023B  -> PUT8
045613  LDA ,B -100 ; 045614 JPL I 56 -> ABSST   ; store N        into region-24[k]
045616  LDA ,B -76  ; 045617 JPL I 53 -> ABSST   ; store CS-addr   into region-24[k+1]
045621  LDA ,B -77  ; block-write source; 045624 JPL I 47 -> ABSWR (region-26)
```
JRWCS @045771 (per prior DUCS carve): `LDA ,B -100 -> ABSST` (N), `LDA ,B -76 -> ABSST` (CS-addr),
then `TRANS`. **The only per-command payload is `{N, CS-address}`.** [V — 030-S3SM5 045613-045624]

`ABSST` (@044551) writes region-24 at a *progressively incrementing* index (`LDA ,B 100; AAA 1;
... STATX; ... STA ,B 100`, @044554-044600), i.e. consecutive scalars land in consecutive slots —
but they are still only N and CS-addr. No `LDDTX`-resolved region base is ever pushed into the
command. **Mechanism (a) is impossible: region-25's address is not a field of the command.** [V]

---

## 2. The region primitives resolve an ND-100-PRIVATE physical address (why the ACCP can't repeat it)

`ABSRE` (region-25 mover) @044613-044641:
```
044625  LDX ,B -56     ; X := control-block base  (needs ND-100 B register)
044626  LDD ,X 21      ; (A:D) := control_block[21] = P, 32-bit PHYSICAL region-table base
044627  LDX 25         ; region index 25
044630  RADD SD DX     ; X := 25 + P_low
044631  RADD CLD SA DT ; T := P_high
044632  LDDTX          ; (A:D) := phys[P+25] = region-25 32-bit base
044633- ... MOVEW      ; physical block move using that base + per-word cursor
```
The base `P = control_block[21]` is reached through the ND-100 `B` register and an ordinary paged
virtual read — both ND-100-private. The ACCP has no handle to `B`, to the control block, or to `P`.
So the ACCP cannot replay `LDDTX(control_block[21]+25)`. [V — 030-S3SM5 044625-044641; prior LDDTX carve]
**Confirms (c) is refuted and forces the address to reach the ACCP by some OTHER channel.**

---

## 3. THE CHANNEL — buffer bases built once at init, stored in shared 5MPM, handed to the ACCP by LPARP

### 3a. The builder (P-table / buffer-base builder) — FOUND

`XMSINIT`, `RP-P2-N500.NPL:760-767` (word-addr `131231`-`131267`). This is the code that allocates
the ACCP/OCTOBUS/HW buffer regions and writes their bases into the per-CPU mailbox extension block
in shared 5MPM (`STDTX` = physical double-word store):
```
131231  MAXOCTBUF+1 SH -1 + MAXACCPBUFF+2000 SH -12      % pages-per-CPU
131237  T:=MSCPUNO; *RMPY ST DA                          % × this CPU's number
131241  5FPACCPBUF; D+A; A:=0; AD SH 12                  % base = (5FPACCPBUF + n*perCPU) << 12
131245  T:=5MBBANK; X:=MSMLINK; *AAX X5ACC; STDTX        % -> X5ACC  (ACCP buffer base)
131251  A:=:D; A+MAXACCPBUFF; D:=D+C:=:A                 % + MAXACCPBUFF
131255  *AAX X5OCT-X5ACC; STDTX                          % -> X5OCT  (OCTOBUS buffer base)
131257  MSCPUNO SH 1 + 5FPHWBUF=:D; A:=0; AD SH 12
131265  *AAX X5HWB-X5OCT; STDTX                          % -> X5HWB  (HW buffer base)
```
So, byte-verified: [V]
- `X5ACC = (5FPACCPBUF + MSCPUNO·perCPU) << 12`  (ACCP buffer base, per-CPU)
- **`X5OCT = X5ACC + MAXACCPBUFF`**  (OCTOBUS buffer base)
- `X5HWB = (MSCPUNO·2 + 5FPHWBUF) << 12`  (HW buffer base)

The page pools `5FPACCPBUF` / `5FPHWBUF` are allocated by `5P-P2-MON60.NPL:504-507,627-631` from the
5MPM pool at DEFINE-MEMORY-CONFIGURATION time; `5FPACCPBUF`/`5FPHWBUF` declared in
`DP-P2-VARIABLES.NPL:114-115`. [V]

These bases are **ND-500-address-space (5MPM-relative) values**, stored in the shared mailbox where
the ND-500 microcode / ACCP can read them. Per `CARVE-ANSWER-ND5000-ACTIVATION-WORKFLAG.md:96-98`
the mailbox extension block holds `X5ACC@20-21`, `X5OCT@22-23`, `X5HWB@24-25`, all "init only". [V]

### 3b. LPARP hands the ACCP exactly ONE 32-bit pointer

`LPARP` (CMLPA 021B) @045467 (030-S3SM5.asm):
```
045472  STZ ,B 77
045473  SAA 21             ; command byte = 021B          -> PUT8 (@045474 JPL I 26 = 044251)
045476  LDD ,B -77         ; (A:D) := mem[B-77] = the 32-bit parameter pointer
045477  JPL I 24           ; -> 044354  (append the 32-bit pointer to the command)
045501  JPL I 23 -> 045053 = TRANS   ; transmit 021B + pointer to the ACCP
045503  JPL I 22 -> 045145 = RECEI
```
LPARP transmits the command byte 021B followed by the single 32-bit word `mem[B-77]`. [V]

### 3c. That pointer is `X5OCT = 0x00018000`

The value observed on the wire for 021B is `0x00018000`
(`DUCS-READBACK-REGION-OWNERSHIP-CARVE` §Q2; `CARVE-ANSWER-OCTOBUS-CSLOAD-MICROSTART` wire step 13).
The 5MPM layout doc pins that exact number to `X5OCT`:
`OS\06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md:341` — *"0x018000 | ~32KB | OCTOBUS Buffers | X5OCT"*.
So **LPARP conveys `X5OCT` = the octobus-buffer base in the ACCP's own 5MPM address space = 0x018000.**
[V that wire=0x18000 and that 0x18000=X5OCT's documented 5MPM offset; **[I]** that `mem[B-77]` is
sourced from the `X5OCT` cell — the LPARP caller's assignment of `mem[B-77]` was not byte-traced, but
the numeric identity is decisive.]

### 3d. Two views of ONE allocation (the unifying fact)

The ND-100 region table `control_block[21]` and the ACCP's `X5OCT` describe the **same physical 5MPM
memory** in two address spaces, both derived from the single XMSINIT allocation (§3a):
- **ACCP view:** ND-500/5MPM address. Base `X5OCT = 0x018000`. Region-25/23 = `X5OCT + fixed sub-offset`.
- **ND-100 view:** window-physical byte address = `ADRZERO (0x420000) + CNVBYADR(5MPM offset)`. `ADRZERO`
  is the "ND-100 page for ND-500 phys addr 0" window base (`CARVE-ANSWER-OCTOBUS-MAILBOX-ACTIVATION`
  §1, live `004100B` = `0x420000`). The `CNVBYADR`/`CNVWADR` "convert multi-port address" routines
  (used all through the NPL, e.g. `RP-P2-N500.NPL:751`) perform exactly this 5MPM→ND-100-physical map.
  The ND-100's `control_block[21]` slots hold these converted physical bases. [V for ADRZERO + CNVBYADR
  role; **[I]** that the slots are specifically the converted X5OCT sub-region bases — the ND-100-side
  writer of `control_block[21]` slots 23-26 was not located in this pass.]

**Because both views are computed from one allocation, no cross-processor address transfer of
region-25 is needed at command time. The ACCP writes `X5OCT + offset`; the ND-100 reads the
CNVBYADR image of the same cell. They rendezvous on the same 5MPM word.**

---

## 4. Emulator implication (what the ACCP model must do — no guessing)

1. **Do NOT expect region-25's address in the CMRWC command.** The command carries only `{N, CS-addr}`
   (region-24). [V]
2. **Take the ACCP's buffer base from LPARP (021B):** record the 32-bit pointer LPARP transmits
   (`X5OCT`, observed `0x018000`). That is the base of the ACCP's octobus-buffer view in 5MPM. [V]
3. **Region-25 (read-back dest) and region-23 (checksum) are at FIXED offsets from that base** in the
   ACCP's address space. Compute them as `X5OCT + offset_region25` / `+ offset_region23`. The exact
   offsets are **[UNVERIFIED]** here (see §5); until carved, prefer the checksum-self-consistent
   recipe below so the absolute address is not needed.
4. **Checksum path needs no absolute address at all** (from the DUCS carve): on CMRWC, write the `N`
   stored microwords (8×16-bit each) sequentially into region-25, compute `sum16 = Σwords mod 2^16`,
   write it to region-23[0], reply Messack. `JRWCS` re-sums the same words and compares — passes
   regardless of where region-25 physically sits. [V logic]
5. If a faithful absolute placement IS wanted: model the shared 5MPM once, let `X5OCT` (from LPARP) be
   the ACCP base, and let the ND-100 side reach the identical cells via `ADRZERO (0x420000) +
   CNVBYADR(offset)`. Both processors then hit the same array. Do not hard-code `0x00B98xxx`; derive it.

---

## 5. UNVERIFIED / open

- **[UNVERIFIED]** The byte sub-offsets of region-25 / region-23 / region-24 / region-26 WITHIN the
  octobus/ACCP buffer (i.e. `offset_region25` from `X5OCT`). Not carved. Needs the octobus-buffer
  sub-layout constants or a live map of `X5OCT + n`.
- **[UNVERIFIED]** Numeric reconciliation of `X5OCT = 0x018000` (5MPM offset) with the observed
  **ND-100-physical region-25 ≈ 0x00B98xxx**. `0xB98xxx` is far above the 32KB octobus-buffer window
  at 5MPM `0x018000`, which suggests the microcode read-back may use a *separate, larger* DMA region
  rather than the 32KB octobus buffer — this could not be settled from the code. The
  region-index → address assignment itself is inconsistent between the prior carve (region-25 =
  0x0045D800) and the task's runtime note (region-25 ≈ 0x00B98xxx; 0x0045D8xx = region-26); treat the
  exact per-index address as unsettled.
- **[I, not V]** That LPARP conveys `X5OCT` specifically (vs `X5ACC`): forced by wire `0x018000`
  matching the documented `X5OCT` 5MPM offset, but the LPARP caller's `mem[B-77]` source cell was not
  byte-traced to the `X5OCT` field.
- **[UNVERIFIED / not located]** The ND-100-side writer that fills `control_block[21]` slots 23-26 with
  the CNVBYADR-converted region bases. Confirmed it is NOT in `030-S3SM5`; the XMSINIT builder (§3a)
  writes the 5MPM-side `X5ACC/X5OCT/X5HWB`, but the mapping of those into the ND-100 per-process
  region table was not found in this pass.
