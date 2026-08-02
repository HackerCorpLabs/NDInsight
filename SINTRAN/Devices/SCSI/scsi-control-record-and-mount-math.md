# SCSI control record + the ENTER-DIRECTORY mount math (error 243B)

**What this covers.** `@ENTER-DIRECTORY,,DISC-SCSI-1,0` on a SCSI disk aborts
with error **243B** (octal, = `0xA3` = 163 decimal) **before block 0 is ever
read**. This document (1) decodes the last block (the SCSI control record) field
by field from the real image bytes, (2) maps every field the mount consumes to
the exact geometry/validation math in segment `006-S3FS`, using the ACTUAL
register values from the live instruction trace, and (3) pinpoints the exact
field + formula that yields the 243B abort.

> **CORRECTION 2026-07-14.** An earlier version of this doc claimed the emulator
> mis-orders the double word so RDIV #2 sees a dividend of `0xEE6C0000` (a
> "SWAP / double-word ordering bug"). **That was a misread of the trace and is
> RETRACTED.** The verified trace (lines 297260-297293 of `file-trace.txt`) shows
> RDIV #2 correctly enters with `A=0x0000, D=0xEE6C` -> dividend `0x0000EE6C` =
> 61036, divisor 1. It overflows because the **quotient** 61036 is >= 32768, which
> is a signed-16-bit divide overflow. The load-bearing fact is the **divisor = 1**,
> not any word-ordering fault. See sections 3 and 5.

**Companion docs**

- Physical-layer field spec of the last block:
  [`scsi-disk-format.md`](scsi-disk-format.md) section 4 (cross-linked here).
- Why the mount reads the last block (the function-42 control-record connect):
  [`../../Filesystem/code-logic/scsi-mount-geometry.md`](../../Filesystem/code-logic/scsi-mount-geometry.md).
- Kernel carving of the mount path:
  `tools/sintran-segment-carver/versions/L-VSX-500/re/kernel-carving/`.

**Rule of evidence**

- **VERIFIED** - proven from real disk bytes, the carved `006-S3FS` L07 bytes, or
  the live instruction+register trace; or from arithmetic that is fully
  determined.
- **INFERRED** - strong reasoning from the bytes + architecture, not one decisive
  source.
- **OPEN** - not decidable from the material at hand; the one further check that
  would close it is named.

ND addresses and values are **octal** unless a `0x` / "hex" tag says otherwise;
SCSI LBAs, block counts and disk-record fields are **hex/decimal** as marked.
1 block = 1024 bytes; 1 ND page = 2048 bytes = 2 blocks.

**Provenance of the executed values**

- Disk image: `E:\Dev\Ronny\RetroFS\demo\test-images\ndfs\tor-disk.img`
  (132,415,488 bytes = 129,312 blocks of 1024 B).
- Trace: `C:\Users\ronny\AppData\Local\trace\file-trace.txt` (the failing mount).
  The geometry routine executes once, at trace lines **297260-297293**
  (timestamp 17:18:34.267), when segment `006-S3FS` is the mapped overlay at
  virtual `0x3F25`. (The many OTHER `0x3F25` hits earlier in the trace are a
  DIFFERENT overlay sharing that low virtual address - do not read them as this
  routine. This is the overlay trap.)
- Carved code: segment `006-S3FS` (load base 26000B), disassembly
  `tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/006-S3FS/006-S3FS.asm`,
  bytes `...\versions\L-VSX-500\segments\006-S3FS.bin`.

---

## 0. TL;DR - the pinpoint

The abort is driven by ONE test at `037467 SKP IF DD EQL 0` (runtime `$3F37`): if
the 32-bit quotient computed by the geometry routine equals 0, the code loads
error **243B** and parks it, aborting the mount before block 0.

That quotient is `(UHLIM / 2) / divisor`, computed as a two-step 32-bit-by-16-bit
long division:

- `UHLIM = 0x0001DCD8 = 122072` blocks (control-record header word[3]). **VERIFIED**
  (dd + trace line 297260: `A[0001] D[DCD8]`).
- `UHLIM / 2 = 61036 = 0x0000EE6C`. **VERIFIED** (trace: after `037445 SAD ZIN SHR
  1`, A:D = `0x0000:0xEE6C`, line 297261).
- `divisor T = (,B 9 field = 0x0411) AND 0111 = 0x0001` (= 1). **VERIFIED**
  (trace lines 297262-297264).
- The routine then divides `61036 / 1` with a signed 16-bit `RDIV`. The true
  quotient 61036 does **not fit a signed 16-bit result** (>= 32768), so `RDIV`
  raises a divide overflow, sets Z + the IID error bit, and **does not write the
  quotient** (A stays 0). `,B 14 := 0`. **VERIFIED** (trace lines 297273-297275,
  incl. the CPU log line `"Z was set, now IID bit 7 (ERROR) is set"`).
- Combined quotient `,B 13:,B 14 = 0` -> `SKP IF DD EQL 0` sees 0 -> falls into
  the error path -> `LDA 243B` -> parks it. **VERIFIED** (trace 297281-297287).

**ROOT CAUSE (2026-07-14): the emulator's `RDIV` did not write A/D on what it
wrongly believed was an overflow.** `RDIV` divides `61036 / 1 = 61036`. RetroCore
early-returned without writing A/D, so `,B 14` got 0 instead of `0xEE6C`,
`SKP IF DD EQL 0` saw a zero quotient, and the mount aborted with 243B. Fixed in
`E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\ND100\Instructions.RegisterOperations.cs`
(`RDIV()`), which now reproduces the microcode for both signs.

> ### CORRECTED 2026-08-02 — the explanation above was wrong, though the fix was right
>
> The earlier text said *"that quotient exceeds a signed 16-bit A (>= 32768), which is an
> overflow"* and *"On real ND-100 hardware, RDIV on overflow STILL writes A = low 16 bits of
> the quotient and D = remainder, then sets Z"*, marked **VERIFIED on a microcode emulator**
> with `A=0, D=0xEE6C, T=1 -> A=0xEE6C, D=0, STS=010010`.
>
> **`61036 / 1` is not an overflow at all.** Read from the real ND microcode
> (`E:\Dev\Repos\Ronny\ND120CPUEMU\ND120CPU\ROM\ND-110-RASK.LISTING.TXT`, identical in the
> ND-120 DELILAH-L listing), the overflow test at `RDIV2` CS `000436` is
>
> ```
> RDIV2:    A,R1      B,A     ALUF,B-A    ALUD,B      COND,CRY
>           B,R7      ALUF,PASSQ  ALUD,B   T,JMP      RDIVZ  CONDENABL;
> ```
>
> i.e. **`|dividend|_high − |divisor|`, branching on carry** — the predicate is
> `|dividend|_high >= |divisor|`, *not* `|quotient| >= 32768`. With `A=0, T=1` the high word
> is 0 and the divisor 1, so `0 >= 1` is false: the routine takes the normal 16-step loop and
> produces `A=0xEE6C, D=0` **without ever entering the overflow path**. The quotient path is
> effectively unsigned 16-bit, so 32768..65535 are valid results.
>
> And on a genuine overflow the hardware does **not** write a quotient. `RDIVZ` at CS
> `000461`-`000463` touches nothing but `STS`:
>
> ```
> RDIVZ:    B,10    ALUF,PASSD  ALUD,Q      IDBS,BARG      ; Q := 10B (bit 3 = Z)
>           B,STS   ALUF,ORDQ   ALUD,B      IDBS,STS       ; STS |= Q
>           A,STS   ALUF,PASSA  ALUD,NONE   STS,LO  COMM,CONTINUE
> ```
>
> What A and D hold afterwards is the residue of the pre-check: `ALUD,B` at `000436` already
> wrote `|dividend|_high − |divisor|` back into A, and D holds the `|dividend|` low word.
>
> **Why the "VERIFIED" tag was worthless:** the test used **divisor = 1**, where the quotient
> equals the dividend and the remainder is zero. `A=0xEE6C, D=0` is predicted by at least four
> different models, so the observation could not discriminate. It also never reached the code
> path it claimed to be testing. (The reported `STS=010010` is unexplained — the microcode
> should not set Z for this case; how that value was captured was not recorded.)
>
> **The manuals do not settle this.** `ND-06.014.2A EN ND-100 Reference Manual` lines 5350-5369
> and `ND-06.029.1 EN ND-110 Instruction Set` lines 2152-2177 say only *"If the division causes
> overflow, the error indicator Z is set to one"* and list `Affected: (A), (D)`. Neither states
> the register contents on the overflow path. The microcode is the only authority here.
>
> **Current code status:** `RDIV()` in RetroCore already implements the microcode faithfully —
> predicate `dividendMagHigh >= divisorMag`, and on that path it writes
> `A = dividendMagHigh − divisorMag`, `D = |dividend| low`, `Z` set. Its own comment records
> that the earlier `Math.Abs(quotient) >= 32768` test was wrong. **No code change is needed;
> only this document was stale.** The mount still works because `61036 / 1` takes the normal
> loop under the correct predicate too, and yields the same `A=0xEE6C, D=0`.

The disk data is entirely correct: `,B 9 = 0x0411` (masked `0111octal` -> divisor
1) is real disk data demand-paged from the control-record buffer, `UHLIM = 122072`
is correct, and `divisor = 1` is a legitimate value the routine handles fine ON
CORRECT HARDWARE (the quotient 61036 is nonzero once RDIV writes it). Nothing on
the disk needed to change.

---

## 1. The control record (last block) - full field table

Real bytes, `tor-disk.img` LBA **129311**, byte offset `0x07E47C00`, reproduced
with `dd`:

```
$ dd if=tor-disk.img bs=1 skip=132414464 count=32 | xxd
00000000: 0800 54d9 8000 0000 0000 0000 0001 dcd8  header (16 bytes)
00000010: c000 0000 0001 f91f 0000 0001 c000 0000  entry0 + entry1 start
```

All 32-bit values are **big-endian** on disk. This is the SCSI control record read
by the driver's function-42 connect/init to learn the disk geometry; it is the
correct and expected read (see the framing in
[`scsi-disk-format.md`](scsi-disk-format.md) section 4).

### 1.1 Header (16 bytes = 4 x 32-bit BE)

| Off | Word | Value (this disk) | Name | Meaning | Grade |
|-----|------|-------------------|------|---------|-------|
| `0x00` | word[0] | `0x080054D9` | signature / magic | fixed-looking signature (`0x54D9` low, `0x0800` high). Not a plain sum/xor of the block. `0x08` high byte also read as **NPART = 8** by function 42. | VERIFIED value / OPEN meaning |
| `0x04` | word[1] | `0x80000000` | flags | bit 31 = table valid/present | VERIFIED value / INFERRED meaning |
| `0x08` | word[2] | `0x00000000` | reserved | reserved (or high word of a 64-bit usable count = 0) | VERIFIED value / INFERRED meaning |
| `0x0C` | word[3] | `0x0001DCD8` = **122072** | **UHLIM** (usable block count) | the load-bearing field: usable filesystem blocks = 61036 pages x 2. This becomes the ND-100 double **UHLIM** the geometry routine divides. | **VERIFIED** (= directory pages x 2, and = the trace's `A[0001] D[DCD8]`) |

### 1.2 Extent entries (12 bytes each = 3 x 32-bit BE) - `{flag, physical-LBA, run}`

| # | Off | flag | LBA (dec) | run | Role | Grade |
|---|-----|------|-----------|-----|------|-------|
| 0 | `0x10` | `C0000000` | 129311 | 1 | the control-record block itself | VERIFIED |
| 1 | `0x1C` | `C0000000` | 129310 | 1 | parameter block | VERIFIED |
| 2 | `0x28` | `E0000000` | 129309 | 1 | spare-pool extent | VERIFIED |
| 3 | `0x34` | `E0000000` | 129289 | 20 | spare-pool extent | VERIFIED |
| 4 | `0x40` | `E0000000` | 129269 | 20 | spare-pool extent | VERIFIED |
| 5 | `0x4C` | `00000000` | - | - | null slot (skip, not terminator) | VERIFIED |
| 6 | `0x58` | `E0000000` | 129098 | 171 | spare-pool extent | VERIFIED |

`C0...` = controller metadata blocks; `E0...` = spare-pool reserved extents. The
six live extents tile `129098..129311` (214 blocks). This block-map is **not**
consumed by the failing geometry routine (which only uses the header UHLIM and a
partition/geometry field); it is documented in full in
[`scsi-disk-format.md`](scsi-disk-format.md) section 4.

---

## 2. From control record to variables (function 42) - INFERRED chain

The SCSI driver's **function 42** (NPL
`SINTRAN/NPL-SOURCE/NPL/IP-P2-SCSI-DISK.NPL`, area
57074-57210; binary +376B in `065-S3SIPIT`) parses the control record: it
validates **NPART** (high byte of word[0] = `0x08` = 8), checks the XOR / the
`L<=2 OR L>NCOPA` guard, sets **UHLIM** from header word[3] = `0x0001DCD8`, and
copies the `NPART*6`-word partition table to **CMADR**. The `006-S3FS` geometry
routine then consumes UHLIM (the header word[3] double) and a geometry field
(which lands in `,B 9`). **INFERRED** (NPL is a different revision; used for logic
only). The two values the failing routine actually reads are both **VERIFIED**
from the trace: `UHLIM = 0x0001DCD8`, and `,B 9 = 0x0411`.

The origin of `,B 9 = 0x0411` (control-record field vs SCSI-driver geometry vs a
computation) is under active trace investigation; see section 5.4.

---

## 3. The failing geometry routine (segment 006-S3FS) - VERIFIED against the trace

Entry `037445` (segment 006-S3FS, load base 26000B). At the executed instance it
is mapped at virtual `$3F25`. Words are byte-verified against `006-S3FS.bin`; the
executed register values are quoted from `file-trace.txt` lines 297260-297287.
`,B N` offsets are **octal** in the disassembly, **hex** in the raw trace
(`,B 11` octal = `$9,B`; `,B 17`=`$F,B`; `,B 20`=`$10,B`; `,B 13`=`$B,B`;
`,B 14`=`$C,B`).

**Register convention:** in the trace, the registers printed on a line are the
state **before** that instruction executes (i.e. the result of the previous one).

| Addr (oct) | Runtime | Mnemonic | Executed effect (from trace) | Trace line |
|-----------|---------|----------|------------------------------|-----------|
| 037445 | `$3F25` | `SAD ZIN SHR 1` | `A:D := UHLIM>>1`: `0x0001DCD8 -> 0x0000EE6C` (61036) | 297260 |
| 037446 | `$3F26` | `STD ,B 17` | store double 61036 -> `,B 17`(=0000) / `,B 20`(=EE6C) | 297261 |
| 037447 | `$3F27` | `LDA ,B 11` | `A := ,B 11` = geometry field = `0x0411` | 297262 |
| 037450 | `$3F28` | `AND 111` | `A := 0x0411 AND 0x0049 = 0x0001` = divisor | 297263 |
| 037451 | `$3F29` | `COPY SA DT` | `T := A` = 1 | 297264 |
| 037452 | `$3F2A` | `JAF 2` | `A != 0` so JUMP (skip the fallback) -> keeps T=1 | 297265 |
| (037453 | `$3F2B` | `SAT 10` | fallback `T := 8` - **skipped** because A!=0) | - |
| 037454 | `$3F2C` | `LDA ,B 17` | `A := ,B 17` = high word of 61036 = `0x0000` | 297266 |
| 037455 | `$3F2D` | `COPY SA DD` | `D := A` = `0x0000` | 297267 |
| 037456 | `$3F2E` | `RCLR DA` | `A := 0` (clear) | 297268 |
| 037457 | `$3F2F` | `RDIV ST` | RDIV #1: `(0:0)/1 = 0` rem 0 | 297269 |
| 037460 | `$3F30` | `STA ,B 13` | `,B 13 := q_high = 0` | 297270 |
| 037461 | `$3F31` | `LDA ,B 20` | `A := ,B 20` = low word = `0xEE6C` | 297271 |
| 037462 | `$3F32` | `SWAP SD DA` | swap: `A=EE6C,D=0000 -> A=0000,D=EE6C` | 297272 |
| 037463 | `$3F33` | `RDIV ST` | RDIV #2: dividend `(0:EE6C)=61036`, `/1=61036` **>= 32768 -> OVERFLOW** | 297273 |
| - | - | *CPU log* | `"Z was set, now IID bit 7 (ERROR) is set"`; quotient NOT written | 297274 |
| 037464 | `$3F34` | `STA ,B 14` | `,B 14 := 0` (A never got the quotient) | 297275 |
| 037465 | `$3F35` | `LDD ,B 13` | `A:D := ,B 13:,B 14 = 0x00000000` | 297277 |
| 037466 | `$3F36` | `JAF 5` | `A == 0` so NO jump (fall through) | 297279 |
| 037467 | `$3F37` | `SKP IF DD EQL 0` | `A:D == 0` -> condition true -> **skip the continue** | 297281 |
| (037470 | `$3F38` | continue | **skipped** -> error path taken) | - |
| 037471 | `$3F39` | `LDA 71` | `A := mem[+0x39] = 0x00A3 = 243B` (error) | 297283 |
| 037472 | `$3F3A` | `JMP 52` | -> `$3F64 STA ,B 2` : park 243B, then return | 297285-287 |

The routine computes `quotient = (UHLIM / 2) / divisor` and treats a **zero
quotient as illegal geometry**, returning 243B.

---

## 4. The mount math table - control-record field -> formula -> pass/fail

| Field | Value | Read at | Formula / validation | Actual trace values | Pass condition | Result |
|-------|-------|---------|----------------------|---------------------|----------------|--------|
| header word[3] = **UHLIM** | `0x0001DCD8` = 122072 | 037445 | `N := UHLIM >> 1` | `0x0001DCD8 -> 0x0000EE6C` (61036) | - | N = 61036 **VERIFIED** |
| geometry field **`,B 9`** | `0x0411` | 037447 `LDA ,B 11` | `T := (,B 9) AND 0111`, else 8 | `0x0411 AND 0x0049 = 0x0001`; `JAF 2` taken (A!=0) so no fallback | want T such that N/T < 32768 (=> T>=2) | T = 1 **VERIFIED (too small)** |
| N high word | `0x0000` | 037454 | RDIV #1 `(0:0)/1` | `= 0`, rem 0 -> `,B 13 = 0` | q_high = N_high/T | q_high = 0 (correct) **VERIFIED** |
| N low word | `0xEE6C` | 037461 | RDIV #2 `(0:0xEE6C)/1` | `61036/1 = 61036` >= 32768 -> **signed overflow**; A not written | q_low fits signed 16-bit | q_low **not written = 0** **VERIFIED** |
| combined quotient `,B 13:,B 14` | `0x00000000` | 037465 | `SKP IF DD EQL 0` | `0` -> error path | quotient != 0 | **0 -> abort 243B** **VERIFIED** |

**Impact on ENTER-DIRECTORY.** The abort at 037471 / `$3F64` parks 243B and
returns up the mount path **before** the directory master (block 0) is ever
requested - so the disk is never mounted, exactly the "block 0 never read"
symptom.

---

## 5. THE PINPOINT - divisor, RDIV overflow, and the two candidate fixes

### 5.1 The failing operation and its executed operands (VERIFIED from trace)

- `N = UHLIM/2 = 61036 = 0x0000EE6C`. (297260 -> 297261.)
- `divisor T = (,B 9 = 0x0411) AND 0111 = 1`; fallback (T:=8) skipped because the
  masked value is nonzero. (297262-297265.)
- RDIV #1 `(0:0)/1 = 0`. (297269.)
- RDIV #2 enters with **`A=0x0000, D=0xEE6C, T=0x0001`** -> dividend `0x0000EE6C`
  = 61036, divisor 1. (297273 - the register line printed BEFORE the RDIV.)
- The true quotient 61036 is `>= 32768`, so it cannot be represented as a signed
  16-bit result. `RDIV` raises the overflow: sets Z, sets IID bit 7 (ERROR), and
  **skips writing A**. `,B 14` stays 0. (297274-297275.)
- `SKP IF DD EQL 0` sees quotient 0 and diverts to `LDA 243B`. (297281-297283.)

### 5.2 Why the quotient is zero (the specific detail)

There is **no** word-ordering / SWAP bug (the earlier claim is retracted): the
double word is placed correctly, RDIV #2's dividend is exactly `61036`. The
quotient is zero purely because **`61036 / 1 = 61036` overflows a signed 16-bit
RDIV**, and on overflow the quotient register is left unchanged (0).

The chain of "why is it 1" is:

1. `(,B 9 = 0x0411) AND 0111octal = 1` -> divisor 1.
2. The fallback that would substitute 8 only fires when the masked field is 0;
   here it is 1, so the fallback is skipped.
3. The two-step RDIV cannot represent `61036` in one signed 16-bit step, so any
   divisor `< 2` overflows for this UHLIM.

### 5.3 Root cause: RDIV overflow write (PROVEN)

The 243B abort is caused by the **emulator's `RDIV` overflow handling**, proven by
running the exact opcode on a microcode emulator.

**Microcode reference result (VERIFIED).** `RDIV ST` (opcode `141660`) with
`A=000000, D=167154 (0xEE6C), T=1`:
- `A = 167154 (0xEE6C = 61036)` - the quotient (low 16 bits) IS written
- `D = 0` - remainder
- `STS = 010010` - Z (bit 3) set

So real ND-100 hardware, on a signed-16-bit overflow, **writes the low 16 bits of
the quotient to A and the remainder to D, then sets Z.** This is consistent with
the ND-100 Reference Manual (ND-06.014.2A, RDIV): "if the division causes overflow,
the error indicator Z is set to one" + "Affected: (A),(D)" - the manual never says
A/D are left untouched.

**The emulator bug.** RetroCore `RDIV()` did:
`if (Math.Abs(result) >= 32768) { STS.Z = true; return; }` - it early-returned on
overflow WITHOUT writing A/D, leaving A at its stale value (0 from RDIV #1's
`RCLR`). So `,B 14` (`STA` of A) got 0 instead of `0xEE6C`, the combined quotient
`,B 13:,B 14` read 0, `SKP IF DD EQL 0` diverted to the 243B error, and the mount
aborted before block 0.

**The fix (applied).** In
`E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\ND100\Instructions.RegisterOperations.cs`,
`RDIV()`: on overflow set Z (and trigger the IID interrupt) but ALWAYS fall
through and write `A = (ushort)quotient`, `D = (ushort)remainder`. Builds clean.
With that, `,B 14 = 0xEE6C`, the quotient is nonzero, `SKP IF DD EQL 0` is not
taken, and the mount proceeds to read block 0.

The divisor `T=1` (from disk word `0xF003 = 0x0411`, masked `0111octal`), UHLIM,
and all control-record data are correct and needed no change - divisor 1 is a value
the routine handles fine on correct hardware.

### 5.4 Provenance notes

- **Origin of `,B 9 = 0x0411`:** traced to disk data - it is demand-paged from a
  control-record buffer (buffer base `0xEFF8`, word `0xF003 = 0x0411`) and copied
  via `MOVEW` into the mount datafield, NOT computed by the CPU. Correct disk data;
  not the fault.
- **RDIV overflow semantics:** now settled by the microcode-emulator test above -
  hardware writes A/D then sets Z; the earlier RetroCore early-return was the bug.

---

## 6. VERIFIED / INFERRED / OPEN summary

| Claim | Verdict |
|-------|---------|
| Control-record header word[3] = UHLIM = `0x0001DCD8` = 122072 | VERIFIED (dd + trace 297260) |
| Header word[0] high byte = NPART = 8; word[0] signature `0x080054D9` | VERIFIED value / OPEN meaning |
| Geometry routine runs at trace 297260-297287, mapped at `$3F25` (006-S3FS overlay) | VERIFIED (trace) |
| `N = UHLIM >> 1 = 61036 = 0x0000EE6C` | VERIFIED (trace 297261) |
| `divisor T = (,B 9 = 0x0411) AND 0111 = 1`; fallback (T:=8) skipped | VERIFIED (trace 297262-297265) |
| RDIV #2 dividend is `0x0000EE6C` (61036), NOT `0xEE6C0000` (no SWAP bug) | VERIFIED (trace 297273) |
| RDIV #2 overflows because quotient 61036 >= 32768; sets Z+IID, skips A write | VERIFIED (trace 297273-297275) |
| Quotient 0 -> `SKP IF DD EQL 0` -> 243B parked at `,B 2`, mount aborts | VERIFIED (trace 297281-297287) |
| ROOT CAUSE: emulator RDIV early-returned on overflow without writing A/D | VERIFIED (microcode test: RDIV ST A=0,D=0xEE6C,T=1 -> A=0xEE6C,D=0,Z set) |
| Fix: RDIV must write A=low16 quotient + D=remainder, THEN set Z | APPLIED (Instructions.RegisterOperations.cs, builds clean) |
| Divisor T=1 and `,B 9 = 0x0411` are correct disk data (demand-paged), not the fault | VERIFIED (trace-back) |
| Function-42 parse: NPART/XOR/UHLIM/CMADR mapping (NPL, different revision) | INFERRED |

---

## See also

- [`scsi-disk-format.md`](scsi-disk-format.md) - physical-layer field spec of the
  last block (section 4) and the parameter block (section 5)
- [`../../Filesystem/code-logic/scsi-mount-geometry.md`](../../Filesystem/code-logic/scsi-mount-geometry.md)
  - the last-block control-record connect and raw-vs-usable reconcile
- `tools/sintran-segment-carver/versions/L-VSX-500/re/kernel-carving/` - the
  carved mount path
- `SINTRAN/NPL-SOURCE/NPL/IP-P2-SCSI-DISK.NPL` - function 42
  control-record parse (logic reference; different revision)
