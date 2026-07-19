# CHDSI / WXDIR - directory master-block processing (COMPLETE carve)

Byte-verified reverse-engineering of the SINTRAN III **L07 / L-VSX-500**
directory master-block ("extended-info") processing in segment **006-S3FS**
(load base **26000B**). Covers EVERY path through:

| Addr | Symbol | Role |
|------|--------|------|
| **37702B** | `WXDIR` | recompute additive checksum + write block 0 back |
| **37763B** | `CHDSI` | read block 0, checksum, capacity, owner interlock, rebuild, stamp, write-back |
| **40162B** | `REENB` | release directory (clear entered bit + write back) - mirror of the stamp |

All addresses **octal**. On-disk multi-byte values are **big-endian words**.
Every claim is graded **VERIFIED** (proven from carved bytes),
**INFERRED** (strong reasoning), or **OPEN** (crosses an uncarved boundary).

- Full disassembly: [`CHDSI-COMPLETE.ASM`](CHDSI-COMPLETE.ASM)
- Readable pseudo-C: [`CHDSI-COMPLETE.pseudo.c`](CHDSI-COMPLETE.pseudo.c)
- Source listing (byte-identity): [`../../segments-ref/006-S3FS/006-S3FS.asm`](../../segments-ref/006-S3FS/006-S3FS.asm) lines 5192-5401
- Binary: [`../../../segments/006-S3FS.bin`](../../../segments/006-S3FS.bin)
  (sha256 `b4a563d0715a6304a35167ec5ff0090c92b0b99dd743aef159ac6b182e080585`)

Offset of octal address A in the .bin: `(A - 26000B) * 2` bytes.

---

## 1. The master block (extended-info, 8 words at front of directory page 0)

Disk words **1750B..1757B**:

| Word | Meaning | Touched by |
|------|---------|-----------|
| 0 | **additive checksum** of words 1..7 | CHDSI recompute, WXDIR store |
| 1..3 | directory identity / dates (not read here) | - |
| 4 | **flag word**; **bit15** (field `170 DA`) = "entered" | CHDSI set, REENB clear |
| 5 | **owner system number** | CHDSI stamp |
| 6..7 | **capacity** = pages available (32-bit) | CHDSI compare / rebuild |

The checksum is a plain **16-bit additive sum**, NOT XOR - proven twice:
the compare loop in CHDSI (040011 `ADD ,X 0`) and the recompute loop in WXDIR
(037716 `ADD ,X 0`) are identical `ADD` loops over words 1..7. VERIFIED.

---

## 2. CHDSI - complete path map (each branch -> outcome)

Entry: `T` = entering system number (ENDIR local 26, referenced as local 34/50 here).

```
CHDSI 37763B
  037767  prologue (003752, OPEN)
  037770  JPL 050323  get param/datafield + resource word (OPEN)
            |-- fail --> 040135 error exit (propagate callee A)
  037772  local16 := param word (option/resource bits)
  037773  BSKP ZRO 100 DA  (bit6 of param = resource-reserve bit)
            |-- bit6 SET --> 040131 EARLY SUCCESS (no page-0 I/O at all)
  040000  JPL RXDIR 37643B   read directory BLOCK 0 (D=0) into 8-word block
            |-- fail --> 040135 error exit (propagate RXDIR A)
  --- checksum recompute (sum of words 1..7) + compare word0 ---
  040017  computed == stored ?
            |-- NO  (040020) --> 040063 REBUILD
            |-- sum==0 (040021) --> 040063 REBUILD
            |-- YES --> capacity compare
  --- GOOD checksum: capacity compare vs device geometry (040022-040061) ---
  040023  JPL 037101  get geometry capacity (in-seg)   fail-> 040135
  040030-040033  stored cap == geometry cap ?
            |-- MATCH --> 040062 --> 040100 owner interlock
            |-- DIFFER --> resident capacity-adjust sequence:
                 040036 JPL 050124  (OPEN)  fail-> 040135
                 040044 JPL 050223  (OPEN)  fail-> 040135 ; bit4 set-> 040134 generic
                 040050 JPL 037565  (in-seg) fail-> 040135
                 040060 JPL 050226  (OPEN)  fail-> 040135
                 040062 --> 040100 owner interlock
  --- BAD/zero checksum: REBUILD / self-heal (040063-040077) ---
  040063-040071  zero all 8 words of the block
  040075  JPL 037101  get geometry capacity            fail-> 040135
  040077  words 6-7 := geometry capacity
          fall through --> 040100 owner interlock
  --- OWNER INTERLOCK (join 040100) ---
  040102-040104  param bit3 SET  -> STAMP (force/override, bypass interlock)
  040105-040107  entering system == 0 -> STAMP (no interlock for system 0)
  040110-040112  flag bit15 clear (not entered) -> STAMP
  040113-040114  owner word == 0 (unowned) -> STAMP
  040115-040116  owner == entering system (re-enter) -> STAMP
  040117         else REJECT: LDA (=003203 sentinel) --> 040135 error exit
  --- STAMP (040121-040127) ---
  040122  word5 := entering system (owner)
  040124  word4 bit15 := 1 (entered)
  040127  JPL WXDIR 37702B  recompute checksum + write block 0 back
            |-- fail --> 040135 (WXDIR returns 35B)
  040131  MIN ,B 4 -> SUCCESS return
```

### Owner-interlock accept/reject logic (VERIFIED control flow)

The mount is **REJECTED** (returns the 003203 sentinel at 040117) **only when
ALL** of the following hold:

1. param bit3 (force/override) is **clear**, AND
2. entering system number is **non-zero**, AND
3. flag word **bit15 "entered" is set**, AND
4. stored owner word is **non-zero**, AND
5. stored owner **differs** from the entering system number.

If any one fails, control reaches the **STAMP** (accept). So: a force/override
request, system 0, a not-yet-entered block, an unowned block, or a re-enter by
the same owner all **accept**; only a genuine cross-system collision rejects.
(The two leading overrides - param bit3 and system==0 - are additional guards
above the flag/owner check documented in `enter-directory.md` section 5.4.)

---

## 3. WXDIR - complete path map

```
WXDIR 37702B
  037706  prologue (003752, OPEN)
  037714-037721  checksum recompute loop (ADD words 1..7)
  037723  word0 := computed checksum
  037727  JPL CL1DB 35240B   release/flush (OPEN edge)
  037730  JPL RCBLO 35766B   reserve cache buffer for block 0
            |-- fail (037731) --> 037750 error
  037733-037736  copy the 8-word block into the page buffer (JPL 001224, OPEN)
  037741  JPL WCBLO 36357B   WRITE cache block 0 back to device
            |-- fail (037742) --> 037746 error
  037743  MIN ,B 4 -> SUCCESS
  037746  JPL 047365 (resident error handler, OPEN)
  037747  SAA 35  -> return ERROR 35B "master block transfer error"
```

Note vs prior anchor: the actual **WCBLO** call is at **037741** (pool word
037760 = 036357B); instruction **037727** is the preceding **CL1DB** call. The
`SAA 35` (error 35B) at **037747** is confirmed.

---

## 4. REENB - release (mirror of the stamp)

```
REENB 40162B
  040167  JPL 050323 param helper        fail-> 040210
  040171  resource bit6 set -> 040205 early success
  040176  JPL RXDIR 37643B  read block 0 fail-> 040210
  040201  BSET ZRO 170 DA   CLEAR flag bit15 "entered"
  040203  JPL WXDIR 37702B  write back   fail-> 040210
  040205  MIN ,B 4 -> SUCCESS
```

VERIFIED: release re-reads the block, clears only bit15, and writes back via
WXDIR (so the checksum stays valid). It does NOT clear the owner word.

---

## 5. Error / return codes (each code + trigger)

| Code | Where | Trigger | Verdict |
|------|-------|---------|---------|
| **35B** | WXDIR 037747 (`SAA 35`) | RCBLO reserve OR WCBLO write of block 0 fails | **VERIFIED** (byte `170435` dd-checked) |
| CHDSI 040127->040130 | propagates WXDIR **35B** when the stamp write-back fails | **VERIFIED** |
| **003203** | CHDSI 040117 (`LDA 37` -> word 040156) | cross-system owner reject (interlock, all 5 conditions) | value **VERIFIED** (`003203` dd-checked); its operator-visible errno mapping is **OPEN** - it is a resident-pointer-shaped word, not a clean file-system errno |
| **003204** | CHDSI 040134 (`LDA 25` -> word 040161) | generic sub-step failure in the capacity-adjust sequence (040047, 040052) | value **VERIFIED** (`003204`); errno mapping **OPEN** |
| propagated A | CHDSI 040135 (`STA ,B 2`) reached from 037771/040001/040024/040037/040045/040051/040061/040076/040130 | the failing sub-call's own A is stored and returned | **VERIFIED** (control flow) |
| REENB -16 / status | REENB 040210 | RXDIR or WXDIR failure during release | **VERIFIED** (control flow) |

Important consequence (already noted in `enter-directory.md` 5.3): a **bad or
zero checksum does NOT raise a mount error** - CHDSI self-heals (zeroes the
block, writes geometry capacity) and proceeds to stamp + WXDIR. So a garbage
page-0 read surfaces as a **write** failure (35B) or a re-fail on next enter,
not as a "checksum error".

The 003203/003204 sentinels are the single **OPEN** item in this carve: the
bytes decisively show *what value is loaded and returned*, but that value does
not map to a small file-system errno in the routine's own literal pool. The
reference-manual candidates for the owner reject remain 032B "Directory
entered" / 034B "Unit occupied" (INFERRED, pending a live DAP trace of the
value the command interpreter prints for a cross-system enter).

---

## 6. VERIFIED / INFERRED / OPEN summary

| Item | Verdict |
|------|---------|
| Checksum = 16-bit additive sum of words 1..7 (CHDSI compare + WXDIR recompute) | VERIFIED |
| CHDSI reads block 0 via RXDIR before any validation | VERIFIED |
| Bad/zero checksum -> zero 8 words + write geometry capacity (self-heal, not reject) | VERIFIED |
| Good checksum -> capacity compare; on mismatch run resident adjust sequence | VERIFIED (flow); adjust-helper semantics OPEN (resident 050124/050223/050226) |
| Owner interlock: reject iff (not-override) AND (sys!=0) AND entered AND owned AND owner!=me | VERIFIED (control flow) |
| param bit3 = force/override bypass; param bit6 = resource-reserve early-success | INFERRED (meaning); branch VERIFIED |
| Stamp: word5 := owner, word4 bit15 := 1 | VERIFIED |
| WXDIR: recompute -> store word0 -> RCBLO -> copy -> WCBLO write block 0 | VERIFIED |
| WXDIR error 35B on transfer failure | VERIFIED (byte) |
| REENB clears bit15 only, keeps owner, writes back | VERIFIED |
| Owner-reject return code 003203 / generic 003204 | value VERIFIED; errno mapping OPEN |
| Resident prologue 003752 / epilogue 003776 / helper 050323 / copy 001224 / handler 047365 | OPEN (uncarved resident segment) |
| RCBLO / WCBLO / CL1DB device transfer of block 0 (via datafield `,X 14`) | VERIFIED request; driver primitive OPEN (see RCBLO carve) |

---

## 7. dd spot-check (verified against 006-S3FS.bin)

Command form: `off = (A-26000B)*2`; read 2 big-endian bytes.

| Addr | Off | Bytes | Octal | Instruction | OK |
|------|-----|-------|-------|-------------|----|
| 037763 | 10214 | 22 6c | 021154 | CHDSI entry `STD I 154` | OK |
| 037702 | 10116 | 22 28 | 021050 | WXDIR entry `STD I 50` | OK |
| 037747 | 10190 | f1 1d | 170435 | WXDIR `SAA 35` (err 35B) | OK |
| 037773 | 10230 | fa 45 | 175105 | CHDSI `BSKP ZRO 100 DA` (resource bit6) | OK |
| 040011 | 10258 | 64 00 | 062000 | CHDSI `ADD ,X 0` (checksum add) | OK |
| 040117 | 10398 | 48 1f | 044037 | CHDSI `LDA 37` (reject code load) | OK |
| 040124 | 10408 | f8 fd | 174375 | CHDSI `BSET ONE 170 DA` (set entered bit15) | OK |
| 040143 | 10438 | 3f a3 | 037643 | pool: RXDIR ptr | OK |
| 040157 | 10462 | 3f c2 | 037702 | pool: WXDIR ptr | OK |
| 037760 | 10208 | 3c ef | 036357 | pool: WCBLO ptr | OK |
| 040156 | 10460 | 06 83 | 003203 | reject sentinel word | OK |
| 040161 | 10466 | 06 84 | 003204 | generic-error word | OK |
| 040142 | 10436 | 00 06 | 000006 | const 6 (datafield offset) | OK |

All 13 words reproduce byte-for-byte.

---

## 8. Cross-links

- [`../ENTER-DIRECTORY/README.md`](../ENTER-DIRECTORY/README.md) - the full
  mount chain (ENDIR -> GDIRA -> CHDSI -> RXDIR -> RCBLO); `CHDSI` is called
  from ENDIR 140402B, entering system in T.
- [`../RCBLO/README.md`](../RCBLO/README.md) - the page-0 device transfer that
  RXDIR/WXDIR dispatch through the datafield pointer `,X 14`.
- [`../../../../../../../SINTRAN/Filesystem/code-logic/enter-directory.md`](../../../../../../../SINTRAN/Filesystem/code-logic/enter-directory.md)
  sections 5.1-5.5 - the original scattered CHDSI notes this carve completes.
- [`../../../../../../../SINTRAN/Filesystem/on-disk-format/extended-info-block.md`](../../../../../../../SINTRAN/Filesystem/on-disk-format/extended-info-block.md)
  - the 8-word block field-by-field with the checksum numeric proof.
