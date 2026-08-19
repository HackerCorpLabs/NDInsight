# CARVE ANSWER: the "MPM BASE register programming" at 051302B-051450B does not exist (2026-08-10)

**Question asked** (ND500-BUS-INTERFACE-DESIGN.md section 9a TODO 2): disassemble
`030-S3SM5` `051302B`-`051450B` to recover the IOXT sequence with which `DEFMC`
(DEFINE-MEMORY-CONFIGURATION, MON 60B subfunction 040B) programs the MPM BASE / limit
registers, so the register can be modeled in
`E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusND500IF.cs`.

**Answer: the premise is false. The range contains NO `IOX`/`IOXT` instruction at all** — it is
four software memory-accounting routines (`RLSWM`, `RELAA`, `RELAL`, `PTOS3`). `DEFMC` never
touches any hardware register, and the MPM-5 port BASE/limit registers are not programmable
from the ND-100 at all: per the manual they are set only from the MPM cabinet's own Test and
Maintenance Program console. **No new emulator register is needed.**

All addresses octal. Source binary:
`E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\segments\030-S3SM5.bin`
(big-endian, load base `40000B` per `030-S3SM5.meta.json`). Symbols: L07 `N500-SYMBOLS`.
Every word quoted below was re-read from the raw `.bin` in this session (word offset =
addr − 40000, byte offset = 2×that).

---

## 1. How DEFMC reaches 051302B / 051367B `[V bytes]`

- `FUNCS` dispatch table @`142031`; entry 040B = `M[142071]` = **`155742`** = `DEFMC`
  (matches symbol `DEFMC=155742`).
- Inside DEFMC:
  - `156027: 135027 JPL I 27` → pool `M[156056] = 051367` = **`RELAA`**
  - `156031: 135026 JPL I 26` → pool `M[156057] = 051302` = **`RLSWM`**
- So yes — `051302` and `051367` are exactly the two "helpers" the design doc named. They are
  not IOX-driver helpers; they are release/bookkeeping routines (below).
- DEFMC's other callees (pool words, byte-verified): `M[156054]=056737` **FLGAN** (flag AND,
  called twice), `M[156060]=073535` **OPSYD**, `M[156061]=052670` **CHPZE**,
  `M[156062]=056727` **FLGOR**, plus the shared error/exit helpers `GETSY=064224`,
  `GETNA=064234`, `GETDS=064265`, `N5FAT=043660`. None of these lie in an IOXT-bearing region
  (see the whole-segment inventory, section 4).
- Value source read by DEFMC: `155745: 054721 LDX ,B -57` (the per-CPU N500 datafield pointer —
  the same `B -57` base whose `,X -3` slot is `HDEV` in the carved 3022 IOX driver) then
  `155746: 046060 LDA ,X 60` = **N500DF offset 60B = `ADRZE`/ADRZERO** — the ND-100 page number
  where ND-500 address zero starts. `[V bytes + SYMBOL]` for the read; the `B -57 = N500DF`
  identification is `[I]` (consistent driver-wide usage, incl. the documented `LDT ,X -3` HDEV
  pattern at `051023`+).

## 2. Annotated carve of 051302B-051450B `[V bytes; symbol names 5-char truncated]`

Common shapes used below: every FUNCS body starts `STF ,B -54` / `A:=L` / `JPL` to the shared
prologue helper (`044030` / `043740` pool values); `144400` after a `JPL` is the skip-return
slot; `MIN ,B -74` marks success; error exits go through `GETDS=064265` / `N5FAT=043660`.

### RLSWM @051302 (called 2nd by DEFMC) — release swap-memory accounting

```
051302 030724 STF ,B -54          ; prologue
051303 146145 RADD CLD SL DA      ; A := L
051304 135046 JPL I 46            ; -> M[051352]=044030  shared prologue helper
051305 054721 LDX ,B -57          ; X := N500DF (CPU datafield)
051306 046360 LDA ,X -20          ; A := N500DF[-20B]  (swap-memory amount field)
051307 131007 JAZ 7  -> 051316    ; nothing configured -> skip release
051310 135043 JPL I 43            ; -> M[051353]=051266 = RELPA  release pages
051311 144400 (skip-return slot)
051312 002360 STZ ,X -20          ; N500DF[-20B] := 0
051313 046362 LDA ,X -16          ; A := N500DF[-16B]  (running total)
051314 146665 RSUB ST DA          ; A := A - T   (T = amount RELPA released)
051315 006362 STA ,X -16          ; write back
051316 135036 JPL I 36            ; -> M[051354]=064234 = GETNA
051317 144400
051320 001035 STZ I 35            ; M[M[051355]] := 0 ; M[051355]=000044 (resident cell) [I]
051321 171024 SAT 24              ; T := 24B
051322 044472 LDA ,B 72           ; A := local (CPU index)
051323 120033 MPY 33              ; A := A * M[051356]=000024
051324 060033 ADD 33              ; A += M[051357]  (table base)
051325 146157 RADD CLD SA DX      ; X := A
051326 156001 SHT ZIN 1           ; T := T<<1
051327 174366 BSET ONE 160 DT     ; T bit14 := 1
051330 170400 SAA 0               ; A := 0
051331 140130 BFILL               ; zero-fill the per-CPU block at X  (pure memory)
051332 124000 JMP 0               ; (BFILL restart convention)
051333 135025 JPL I 25            ; -> M[051360]=064265 = GETDS
051334 144400
051335 146106 RADD CLD 0 DT       ; T := 0
051336 135023 JPL I 23            ; -> M[051361]=036545  resident helper (below 40000B)
051337 144400
051340 044721 LDA ,B -57
051341 060021 ADD 21              ; A += M[051362]=177756 (-22B) -> A := N500DF-22 (flag word)
051342 146157 RADD CLD SA DX      ; X := N500DF-22
051343 170777 SAA -1
051344 174035 BSET ZRO 30 DA      ; clear bit 3 in mask
051345 174075 BSET ZRO 70 DA      ; clear bit 7 in mask
051346 135015 JPL I 15            ; -> M[051363]=056737 = FLGAN  (AND mask into flag word)
051347 144400
051350 040704 MIN ,B -74          ; success
051351 125013 JMP I 13            ; -> M[051364]=044054  common epilogue
051352-051366 pool words (044030, 051266, 064234, ..., 056737, 044054)
```

Not one I/O instruction. It clears the CPU's swap-memory record and flags.

### RELAA @051367 (called 1st by DEFMC) — release all 5 memory areas

```
051367 030724 STF ,B -54 ; 051370 A:=L
051371 135141 JPL I 141           ; -> M[051532]=043740  shared prologue helper
051372 000606 STZ ,B -172         ; released-total := 0
051373 045140 LDA I 140           ; A := M[M[051533]=004322]  (resident cell) [I]
051374 004603 STA ,B -175
051375 146105 RADD CLD 0 DA       ; A := 0
051376 135136 JPL I 136           ; -> M[051534]=051436 = PTOS3   area 0
051377 124032 JMP 32 -> 051431    ; error -> join
051400 170401 SAA 1
051401 135134 JPL I 134           ; -> M[051535]=051436 = PTOS3   area 1
051402 124027 JMP 27 -> 051431
051403 170402 SAA 2
051404 135132 JPL I 132           ; -> M[051536]=051436 = PTOS3   area 2
051405 124024 JMP 24 -> 051431
051406 170403 SAA 3
051407 135130 JPL I 130           ; -> M[051537]=051436 = PTOS3   area 3
051410 124021 JMP 21 -> 051431
051411 170404 SAA 4
051412 135126 JPL I 126           ; -> M[051540]=051436 = PTOS3   area 4
051413 124016 JMP 16 -> 051431
051414 040704 MIN ,B -74          ; success
051415 124014 JMP 14 -> 051431    ; join (tail shared with RELAL below)
```

Five unrolled calls to `PTOS3` with A = area index 0..4 (pool words `051534`-`051541` ALL hold
`051436`, byte-verified each). Shared tail:

```
051431 054721 LDX ,B -57          ; X := N500DF
051432 046361 LDA ,X -17          ; A := N500DF[-17B] (allocated total)
051433 064606 SUB ,B -172         ; minus released amount
051434 006361 STA ,X -17
051435 125105 JMP I 105           ; -> M[051542]=043774  common exit
```

### RELAL @051416 — release one area (the `,X -14` one) via the same PTOS3

```
051416 prologue as above; 051420 JPL I 112 -> M[051532]=043740
051421 000606 STZ ,B -172
051422 045111 LDA I 111           ; -> same resident cell M[051533]=004322
051423 004603 STA ,B -175
051424 054722 LDX ,B -56          ; X := second datafield (X500DF/shared ext.) [I]
051425 046364 LDA ,X -14
051426 135113 JPL I 113           ; -> M[051541]=051436 = PTOS3
051427 124002 JMP 2 -> 051431     ; error -> shared tail
051430 040704 MIN ,B -74          ; success -> falls into shared tail 051431
```

### PTOS3 @051436 (head; body runs to ~051553) — the actual page-release worker

```
051436 146146 RADD CLD SL DT      ; T := L (save link)
051437 010605 STT ,B -173
051440 120103 MPY 103             ; A := A * M[051543]=002000  (area index * 1024 pages)
051441 060103 ADD 103             ; A += M[051544]=000000
051442 004604 STA ,B -174         ; start index
051443 000600 STZ ,B -200 ; 051444 STZ ,B -177 ; 051445 STZ ,B -176   (loop vars)
051446 135077 JPL I 77            ; -> M[051545]=064224 = GETSY
051447 124061 JMP 61 -> 051530    ; error path
051450 044600 LDA ,B -200         ; loop head  <- END OF THE ASKED RANGE
```

The rest of PTOS3 (051450-051515) walks the area's page entries with `EXR ST` +
`LDATX/STATX/STZTX` — physical-memory table edits (page bookkeeping), still zero I/O.

## 3. Where the "BASE" value really comes from and goes `[V bytes / I as marked]`

- The only in-segment writer of N500DF-style offset 60B (`STA ,X 60` = word `006060`) is
  **`SSYSE` @170705** (whole-segment scan: exactly one hit). The stored value is the return of
  **`ALLOC` @171076** (`170702: JPL I 40` → pool `M[170742]=171076`, then the store). I.e. the
  ND-500-address-zero placement is **allocated by software** — matching the live observation
  (MEM-CONF: first page above local memory, page `4100B` = emulator byte `0x420000`). The
  "X = N500DF" identification at the SSYSE store site is `[I]` (X comes from `B -166`, not
  traced to its producer).
- `DEFMC` reads that field (`155746 LDA ,X 60`) and does table/flag work with it. It never
  writes it to any device register.

## 4. Whole-segment IOXT inventory — nothing left over `[V bytes]`

Complete scan of `030-S3SM5.bin` for `150415` (IOXT). Every site attributed:

| IOXT sites | Routine | What it is (already documented) |
|---|---|---|
| 051040-051137 | WADR/WRDAT/RDATL/REDAT/WRTAG 3022 IOX driver | `ND500-3022-IOX-INTERFACE.md` |
| 051737-051761 | `TSTPO` | 5POWOF latch clear (RSTA5/UNLC5/LCON5:=10/LSTA5/LCON5:=0/RSTA5) |
| 052013-052041 | `CHKST` | status check/clear, same CLE5STATUS shape |
| 103577 | `XXTWA` | RSTA5 read in a wait loop |
| 152270-153333 | CS-load / MPSTA / MPSTO / 5MCLE FUNCS bodies | `FUNCS-controlstore-micro.ASM` |
| 154762, 155255-155327, 155724 | `CSLOA`/`OLDLO`/`LWRTG` (FUNCS[037] block 153441-155600) | control-store loader: RFILE + DATAX (offset 13) + write-tag (offset 11) + RSTA5 checks |
| 156122, 156172-156176 | `RSTAT` (FUNCS[041]) + neighbour | live RSTA5 reads |

Bare-`IOX` (164000-167777) hits all fall in data/pool regions (e.g. the FUNCS dispatch table
at 142106+, pool word `067551=164415` = a callee address) — value-scan noise, each checked.

Register offsets used anywhere in the segment stay inside the 3022's own 0-17B file. The
test-mode limit loads that DO occur (offsets 10-13) belong to the CS loader and bound the
**3022 DMA**, not the MPM window.

## 5. The manual closes the loop: the MPM-5 BASE register is not ND-100-programmable `[V manual]`

`Reference-Manuals\500\ND-10.004.01-MPM 5 Technical Description.md`:

- §1 (p.9): the port's address window (lower/upper limit) is set "with the test and maintenance
  program on the controller module."
- §2 (p.33): "**All the registers on the port module and the dynamic RAM module are programmed
  from the Multiport-5 Test and Maintenance Program.** This program appears at the console
  terminal connected to the controller module." (= the MPM cabinet's own maintenance processor,
  commands CONFIGURE-SLOT / LIST-CONFIGURATION etc.)

So the "BASE = 2's complement of (base − lower limit), 64KW steps" formula (bus-reference §8.4)
describes a register an ND field engineer sets from the MPM cabinet console at installation —
not something SINTRAN, DEFMC, or any IOX can reach. This also explains bus-reference §3.3:
SINTRAN never programs the 3022 limit registers either.

## 6. What NDBusND500IF.cs needs (the deliverable facts)

1. **No new register.** Do not model an "MPM BASE register" on the 3022/5015; nothing in
   SINTRAN can write one. Design-doc TODO 2's instruction "model the MPM BASE register ... so
   DEFINE-MEMORY-CONFIGURATION works" is based on the false premise this carve retires.
2. **The MPM window base is machine configuration** (per-installation constant, like the real
   maintenance-console setting). Keep it a constructor/config value with the current default
   (byte `0x420000`); "the window MUST move to wherever LLOW5/LUPP5/BASE writes put it" is
   retired — no such writes exist.
3. **DEFMC (MON 60B subfn 040B) is a pure software answer**: release previously configured
   areas (RELAA→PTOS3 ×5, RLSWM), update the N500DF fields (ADRZERO @60B et al.), flags via
   FLGAN/FLGOR. Emulator behavior: service it through the normal FUNCS/answer machinery with
   NO hardware side effects. Reset defaults unchanged.
4. **Read-back (MEM-CONF etc.) comes from SINTRAN's own tables/datafields**, not from any
   hardware register — same contract as VERSION/READ-CPU-TYPE (descriptor-cached). The live
   MEM-CONF output (addr zero page 4100B, register block 4212B, PST 4252B, WIP/PGU 4211B)
   remains the acceptance data, produced by software allocation (ALLOC @171076 → SSYSE store),
   not by register emulation.
5. The one genuine hardware coupling: the emulator's fixed window base must agree with what
   SINTRAN's allocator records, which the current `0x420000` default already does for the
   observed 2MW configuration.

## 7. Provenance

- Binary: `030-S3SM5.bin` (L-VSX-500), base `40000B`, big-endian; all quoted words re-read
  from raw bytes 2026-08-10.
- Listing used for context: `re\segments-ref\030-S3SM5\030-S3SM5.asm` (spot-verified against
  raw bytes at 25 addresses incl. every pool word cited).
- Symbols: L07 `N500-SYMBOLS.SYMB.TXT` (`DEFMC=155742`, `RLSWM=051302`, `RELAA=051367`,
  `RELAL=051416`, `PTOS3=051436`, `RELPA=051266`, `FLGAN=056737`, `FLGOR=056727`,
  `OPSYD=073535`, `CHPZE=052670`, `GETSY=064224`, `GETNA=064234`, `GETDS=064265`,
  `N5FAT=043660`, `SSYSE=170537`, `ALLOC=171076`, `TSTPO=051721`, `CHKST=051773`).
- Manual: `ND-10.004.01-MPM 5 Technical Description.md` §1.4.4, §2.
- Consumer: `ND500-BUS-INTERFACE-DESIGN.md` section 9a TODO 2 (RetroCore) — needs its TODO
  text updated to this result (not done here; that doc is the bus session's).
