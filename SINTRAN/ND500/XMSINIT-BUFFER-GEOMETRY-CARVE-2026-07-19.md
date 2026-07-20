# XMSINIT buffer geometry — numeric carve (2026-07-19)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\XMSINIT-BUFFER-GEOMETRY-CARVE-2026-07-19.md`

Every value is cited to `file:line` and given in octal AND hex. Grades:
**[V]** = byte/symbol-cited here · **[I]** = inferred from cited code · **[UNVERIFIED]** = not found in material.

Primary sources:
- `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\RP-P2-N500.NPL` (XMSINIT, 730-772).
- `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\5P-P2-MON60.NPL` (INZ500 allocator 616-644; CHMEMDEF/ADRZERO 587).
- `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\DP-P2-VARIABLES.NPL` (pool variable decls 111-115).
- `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\M06\{N5000-SYMBOLS,N500-SYMBOLS,SYMBOL-1-LIST,SYMBOL-2-LIST}.SYMB.TXT`.
- ASM: `...\re\segments-ref\030-S3SM5\030-S3SM5.asm` (ABS* primitives 044505-044716; JRWCS/JWWCS 045603-046064).

---

## 1. Symbol values (octal / hex) with citation

| Symbol (5-char) | Kind | Octal | Hex | Dec | Citation |
|---|---|---|---|---|---|
| `MAXACCPBUFF` (MAXAC) | **constant** | `002010` | `0x408` | 1032 | M06/SYMBOL-1-LIST.SYMB.TXT:`MAXAC=002010`; N500-SYMBOLS.SYMB.TXT |
| `MAXOCTBUF` (MAXOC) | **constant** | `000377` | `0xFF` | 255 | M06/SYMBOL-1-LIST.SYMB.TXT:`MAXOC=000377` |
| `5EXTDFSIZE` (5EXTD) | constant | `000200` | `0x80` | 128 | M06/SYMBOL-1-LIST.SYMB.TXT:`5EXTD=000200` |
| `5CPUDFSZ` (5CPUD) | constant | `000046` | `0x26` | 38 | M06/SYMBOL-1-LIST.SYMB.TXT:`5CPUD=000046` |
| `X5ACC` | ext-block word offset | `000020` | `0x10` | 16 | M06/N5000-SYMBOLS.SYMB.TXT:`X5ACC=000020` |
| `X5OCT` | ext-block word offset | `000022` | `0x12` | 18 | M06/N5000-SYMBOLS.SYMB.TXT:`X5OCT=000022` |
| `X5HWB` | ext-block word offset | `000024` | `0x14` | 20 | M06/N5000-SYMBOLS.SYMB.TXT:`X5HWB=000024` |
| `CNVBYADR` | routine word-addr | `055034` | `0x5A1C` | — | M06/N5000-SYMBOLS.SYMB.TXT:`CNVBYADR=055034` |
| `CNVWADR` | routine word-addr | `055160` | `0x5A70` | — | M06/N5000-SYMBOLS.SYMB.TXT:`CNVWADR=055160` |
| `ADRZERO` (ADRZE) | **field offset in N500D** | `000060` | `0x30` | 48 | M06/SYMBOL-1-LIST.SYMB.TXT:`ADRZE=000060` |
| `5FPACCPBUF` (5FPAC) | **runtime variable** (var addr `011252`) | — | — | — | DP-P2-VARIABLES.NPL:114 `INTEGER 5FPACCPBUF` |
| `5FPHWBUF` (5FPHW) | runtime variable (var addr `011253`) | — | — | — | DP-P2-VARIABLES.NPL:115 |

**Critical facts, [V]:**

- **`5FPACCPBUF` is NOT a constant.** It is an `INTEGER` cell (DP-P2-VARIABLES.NPL:114) whose value is
  the *first page number* of a block allocated at runtime by `5GBUFF`, in `INZ500`
  (5P-P2-MON60.NPL:627-629):
  ```
  027032  MAXOCTBUF+1 SH -1 + MAXACCPBUFF+2000 SH -12   % pages-per-CPU
  027040  A*NCPU; CALL 5GBUFF; GO FAR 0INZERET          % × NCPU, allocate from 5MPM pool
  027043  A=:5FPACCPBUF                                  % first page of ACCP+OCTOBUS region
  ```
  Therefore **X5ACC/X5OCT have no static numeric value** — their absolute value depends on where
  `5GBUFF` places the pool in a given boot. Only the *relationship* below is static.

- **`ADRZERO`** the *symbol* is a **field offset** `060B`(=48) inside the N500 datafield `N500D`
  (used as `X.ADRZERO`, e.g. s3vs-4.symb:22127). Its **runtime value** = "ND-100 physical page of ND-500
  phys-0 window", assigned `5D12=:ADRZERO` (5P-P2-MON60.NPL:587). Prior live carve pins that window base
  to `0x00420000` (= page `004100B`; `CARVE-ANSWER-OCTOBUS-MAILBOX-ACTIVATION`). [V offset / prior-carve runtime]

- **`CNVBYADR`** = "convert multi-port (5MPM) address → ND-100 window-physical byte address"; `CNVWADR`
  = word-address variant. Routine entry word-addresses as above; both `)KILL`-ed after init
  (s3vs-4.symb:43564). The conversion itself is `ADRZERO-window-base + scaled 5MPM offset`.
  Exact scale factor **[UNVERIFIED here]** (routine body not disassembled this pass).

- **`SH 12` is octal** → shift by `0o12`=10 bits = ×1024 = ND page→word. Confirmed by the `5MBBANK`
  derivation `5FPMAILBOX=:D:=0; AD SH 12; A=:5MBBANK` (RP-P2-N500.NPL:737): the high word `A` then holds
  `page>>6` = the 64K-word bank, which is only correct if `SH 12`=×1024. [V]

- **perCPU stride** (RP-P2-N500.NPL:760 / 5P-P2-MON60.NPL:627):
  `(MAXOCTBUF+1)>>1  +  (MAXACCPBUFF+2000)>>0o12`
  = `(0377+1)>>1 + (02010+02000)>>10` = `0200 + 2` = **`0202`B = 130 pages/CPU** (128 octobus + 2 accp). [V]

---

## 2. X5ACC / X5OCT for n=0, and the 0x0800 / 0x018000 / 0x0B98xx reconciliation

XMSINIT builder, byte-cited (RP-P2-N500.NPL:760-767, word-addr 131231-131267):
```
131241  5FPACCPBUF; D+A; A:=0; AD SH 12   % X5ACC = (5FPACCPBUF + n·130) << 10   (n=MSCPUNO)
131245  *AAX X5ACC; STDTX                  % store 32-bit X5ACC
131251  A:=:D; A+MAXACCPBUFF; D:=D+C:=:A  % X5OCT = X5ACC + MAXACCPBUFF
131255  *AAX X5OCT-X5ACC; STDTX            % store 32-bit X5OCT
```

**For n=0 (MSCPUNO=0):**
- `X5ACC = 5FPACCPBUF × 1024` → **page-aligned** (low 10 bits = 0). [V]
- `X5OCT = X5ACC + MAXACCPBUFF = X5ACC + 1032` = `X5ACC + 0x408`. [V]

**Modular fingerprint (the decisive check):** since X5ACC ≡ 0 (mod 0x400),
> **`X5OCT ≡ 0x008 (mod 0x400)`**, i.e. the true X5OCT low hex must end in `…008` (or `…408` mod 0x800).
1032 = 1024+8, so X5OCT is 8 words into the *second* page of the accp sub-block.

**Reconciliation of the three cited numbers:**

| Cited value | ≡ mod 0x400 | Can it literally be X5OCT (≡0x008)? | Verdict |
|---|---|---|---|
| `0x00000800` (LPARP wire) | `0x000` | **No** | emulator STUB / relative param-area offset, not the SINTRAN X5OCT |
| `0x00018000` (doc map) | `0x000` | **No** | documentation map slot (`OS\06-…:341` "0x018000 …OCTOBUS…X5OCT"), an approximation |
| `~0x00B98xxx` (runtime region-25) | unknown low | **possibly** | CNVBYADR window-physical of a *real* allocation; the only "real" derived address |

**Conclusion [V arithmetic / I attribution]:** the true `X5OCT` is `X5ACC+0x408` at a *runtime-allocated*
page — it cannot equal `0x800` or `0x018000` (both ≡0 mod 0x400). Those two are a harness stub and a
documentation label, NOT the byte-exact SINTRAN value. The `0x0B98xxx` runtime observation is the
CNVBYADR window-physical image of the real allocation and is the only one consistent with the formula
(its low digits were not captured, so it is not contradicted). **X5OCT has no fixed absolute value; it is
`(5FPACCPBUF·1024)+0x408` for the boot in question.**

---

## 3. control_block[21] slots 23-26 — the region-base table

**Model (byte-verified from the ABS* primitives, 030-S3SM5.asm 044505-044716):** the four regions are
**NOT** sub-offsets of X5OCT. They are four **independent 32-bit physical base addresses**, each stored in
its own slot of a table `T`, where `P = control_block[21]` (32-bit physical pointer) and the ND-100 reads
`phys[P + index]` via `LDDTX`:

```
ABSxx:  LDX ,B -56     ; control block (needs ND-100 B register)
        LDD ,X 21      ; P = control_block[21]  (32-bit PHYSICAL pointer)
        LDX <index>    ; index ∈ {23,24,25,26}
        RADD SD DX / RADD CLD SA DT / LDDTX   ; (A:D) := phys[P+index] = region base
```

| index | slot | primitive @entry | op after LDDTX | role | ND-100 does | runtime addr |
|---|---|---|---|---|---|---|
| **23** | `phys[P+23]` | ABSLD @044656 (`LDX 23`@044672) | `LDATX` | **checksum ADDEND** source | READS `region23[N·8]` — **corrected 2026-07-20, was `[0]`; ABSRE's epilogue advances the shared index** (see note below) | resolves to the same field as region-25 |
| **24** | `phys[P+24]` | ABSST @044551 (`LDX 24`@044574) | `STATX` | command param `{N, CS-addr}` | WRITES | param area |
| **25** | `phys[P+25]` | ABSRE @044613 (`LDX 25`@044627) | `MOVEW` | read-back block | READS `N·8` words | **≈0x00B98xxx** |
| **26** | `phys[P+26]` | ABSWR @044505 (`LDX 26`@044521) | `MOVEW` | write-source block | WRITES `N·8` words | **≈0x0045D8xx** |

So **which "base"?** — each region has its OWN base in `phys[P+index]`; they are *not* `X5ACC+δ` or
`X5OCT+δ`. `P` itself (`control_block[21]`) is a 32-bit **physical** pointer (used only via `LDDTX`,
paging-independent), most plausibly the physical self-address of the control block so `+23..26` can be
reached physically. [V for the table model; I for "P = physical self-pointer"]

**Writer of slots 23-26: [UNVERIFIED — NOT LOCATED].** Confirmed NOT in 030-S3SM5. This pass searched the
whole tree: candidate descriptor-copy routines exist (`SEGTO`@072230 writes dest[20/22/23/24/26/…] via
`LDX ,B -67`; `IN5FD`@053513; `INITS`) but **none is byte-verified as the CNVBYADR-based physical
region-base writer** that produces `0x0045D8xx`/`0x00B98xxx`. The XMSINIT builder (§2) writes only the
5MPM-side `X5ACC/X5OCT/X5HWB`; the mapping of those into `control_block[21]`+23..26 was not found.
This remains the open "Rosetta stone".

---

## 4. Read-back block & checksum-addend sub-offsets

From JRWCS (CMRWC/DUCS 025B), 030-S3SM5.asm 045771-046064 [V]:
- **Read-back block** = **region-25**, starting at offset **0** of the region-25 base, length **`N·8`
  sixteen-bit words** (each microword = 8×16b = 128b; count set `LDT ,B -100; SHT ZIN 3` @046022-046023,
  i.e. `N<<3`). Pulled in by `ABSRE`@046026.
- **Checksum addend** = region-23 base **+ N·8**, i.e. the word immediately AFTER the read-back block —
  **NOT** `region23[0]`. See the correction note below.
- **Pass condition:** `addend == (Σ region25[i], i=0..N·8-1) mod 2^16` (`UEQ` compare @046053). [V]

> **CORRECTION 2026-07-20 (live instruction trace, RetroCore octobus boot harness).** This section
> previously read "**Checksum addend = region-23[0]** — the *first word* of the region-23 base", arguing
> that `STZ ,B 100` @046025 sets the ABSLD index to 0. **That is wrong**, and the reason is one
> instruction the static pass missed: `STZ ,B 100` zeroes the index *before* `ABSRE`@046026, but
> **ABSRE's own epilogue advances it** —
> ```
> 044642 LDA ,B 100 ; 044643 ADD ,B -100 ; 044644 STA ,B 100   ; index += count
> ```
> — so by the time `ABSLD`@046046 runs, `mem[B+100]` holds **N·8**, not 0. `ABSLD` (044656-044701) adds
> that index to its resolved base exactly as `ABSRE` does (`LDX ,B 100` / `RADD SD DX` before `LDATX`).
> The addend is therefore read at **base + N·8**.
>
> **Live evidence** [V]: with the ACCP writing the addend at `pb + N·8` (one contiguous parameter field),
> `ABSLD`@044701 returns `0xC7EA`, the compare @046053 is equal, and control reaches the success path
> @046064; the "Checksum error" (EILOCS) console message goes to zero occurrences. Committed as
> RetroCore `bf957b46c`. Previously the ND-100 read a stale `0x0080` (= N=128 left over from CMWWC)
> against a correct sum `0xC7EA`.
>
> **Consequence for the region model:** region-23 and region-25 are not usefully "independent bases"
> here — whatever `phys[P+23]` and `phys[P+25]` resolve to, they resolve to the **same field** in
> practice, which is what makes the manual's §5.3.20 wording ("N microwords + checksum addend in the
> parameter field", singular) literally correct. The table row above already hedged this with
> "(with region-25)"; the live trace settles it. The independent-bases reading is retained below only
> as the general ABS\* mechanism, which is correct — it is the *index* that was mis-tracked, not the
> base resolution.

(Paired write side JWWCS/CMWWC 023B @045603: writes N·8 words to **region-26**, stores the **negated**
16-bit sum into **region-24** param block as the addend. Different region, different sign. [V])

---

## 5. Deterministic emulator recipe

Given: LPARP-conveyed pointer `LP`, command `N`, `csWord`.

**Fact wall [V]:** region-25/23 absolute addresses are resolved by an ND-100-**private** chain
(`B`→`control_block`→`phys[P+idx]`); they are **not computable ACCP-side** from shared MPM, and are **not**
carried in the CMRWC command (which carries only `{N, CS-addr}` in region-24). So there are two honest paths:

**Path A — self-consistent checksum (needs no absolute address):**
1. On CMWWC (023B): store the N·8 microwords the ND-100 block-writes (they arrive in region-26).
2. On CMRWC (025B): read `N`, `csWord` from the region-24 param block.
3. Write those `N·8` sixteen-bit words sequentially into region-25 (wherever region-25 physically is).
4. `sum16 = (Σ those N·8 words) mod 2^16`; write `sum16` into **region-23[0]**.
5. Reply Messack. JRWCS re-sums region-25 and compares to region-23[0] → passes **regardless of the
   absolute address**. (This is the recommended path; matches prior DUCS carve.)

**Path B — faithful absolute placement (if wanted):** do NOT hard-code. Let the ND-100 guest perform the
`MOVEW`(ABSRE)/`LDATX`(ABSLD) and **observe the physical addresses on the bus**:
- region-25 base = the `phys[P+25]` value the guest resolves (runtime ≈ `0x00B98xxx`); write microwords
  to `region25_base + 0` .. `+ (N·8−1)` (word stride).
- region-23 base = `phys[P+23]`; write `sum16` to `region23_base + 0`.
- Both are single ND-100 physical writes into the 5MPM window (`ADRZERO=0x00420000` + CNVBYADR offset).
Do **not** derive these from `LP=0x800`/`0x018000` — they are not X5OCT and don't yield the region bases.

---

## 6. UNVERIFIED list

- **[UNVERIFIED]** Absolute value of `5FPACCPBUF` (⇒ of X5ACC/X5OCT): runtime `5GBUFF` allocation, not static.
- **[UNVERIFIED]** The ND-100-side **writer of `control_block[21]` slots 23-26** (the CNVBYADR physical
  region-base table producing `0x0045D8xx`/`0x00B98xxx`). Confirmed NOT in 030-S3SM5; not found tree-wide.
  Candidates SEGTO/IN5FD/INITS handle descriptors but none byte-verified as this writer.
- **[UNVERIFIED]** Whether `P=control_block[21]` is the physical self-address of the control block (inferred).
- **[UNVERIFIED]** `CNVBYADR` exact scale factor / body (routine not disassembled this pass; entry `055034`B).
- **[UNVERIFIED]** Low digits of the runtime region-25 `0x00B98xxx` (needed to confirm the `≡0x008 mod 0x400`
  fingerprint against the live allocation).
- **[I, not V]** LPARP conveys `X5OCT` specifically (numeric identity only; `mem[B-77]` source cell not traced).
```
