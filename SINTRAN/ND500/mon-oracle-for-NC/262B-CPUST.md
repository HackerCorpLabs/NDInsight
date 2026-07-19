# MON 262B - CPUST / GetSystemInfo - L07 oracle for NC

**This file:** `SINTRAN/ND500/mon-oracle-for-NC/262B-CPUST.md`

Reverse-engineering oracle for another team's emulator. Everything below is grounded in
the carved **SINTRAN III VSX/500 L07** bytes (segment/resident carves under `/mnt/d/ND/t/`).
Every claim is tagged **VERIFIED** (proven from carved bytes) or **INFERRED** (manual +
architecture, not provable from the static carve). No value is guessed silently.

---

## 1. What the call does (manual, VERIFIED against L07 manual in repo)

`MON 262B` = `GetSystemInfo` (internal name **CPUST**). Input: register A = a number
(must be 0), X = address of a **12-word / 24-byte** buffer. On return the buffer holds
system identity + system-generation time. Layout, from
`Reference-Manuals/ND-860228-2-EN SINTRAN III Monitor Calls-WEB.md` (page 286):

| Offset (bytes) | Field | Manual definition |
|----------------|-------|-------------------|
| 0:1  | System number | normally the CPU number (16 bits) |
| 2    | CPU type | 2 = ND-100 /48-bit FP; 3 = ND-100 /32-bit FP; 4 = ND-110/CX or ND-120/CX /48-bit FP; 5 = ND-110/CX or ND-120/CX /32-bit FP |
| 3    | Instruction set | 0 = ND-100 std; 1 = ND-100/CE; 2 = ND-100/CX (4-PIT micro-seg admin); 3 = ND-100/CX, ND-110/CX or ND-120/CX (16-PIT micro-seg admin) |
| 4:5  | Microprogram version | ND-110/CX or ND-120/CX microprogram version |
| 6:7  | System type | 100, 500, 502, 5561, ... (set at sysgen / install) |
| 8    | Operating system | 1 = VSE, 2 = VSE-500, 3 = RTP, 4 = VSX, **5 = VSX-500** |
| 9    | OS version | ASCII char A-Z without parity |
| 10:11| (unused / reserved) | not defined by manual |
| 12:13| Patch level indicator | system-dependent coding |
| 14:15| Sysgen time - minutes | |
| 16:17| Sysgen time - hours | |
| 18:19| Sysgen time - day | |
| 20:21| Sysgen time - month | |
| 22:23| Sysgen time - year | |

**The manual defines bytes 2 and 3 for ND-100-family CPUs only. There is no ND-500 encoding
for these bytes.** That is the crux of the top-priority question and is resolved in section 5.

---

## 2. The two "CPUST" symbols are BOTH name-coincidences, not the 262B worker (VERIFIED)

The task noted CPUST appears twice. Both were decoded from carved bytes; **neither builds the
24-byte GetSystemInfo buffer.**

### 2a. `006-S3FS @ 63022B` (FILSYS-SYMBOLS) = a file-system routine

This handler does the `LDA 77` / `LDA 71` + `MOVEW` pattern, but the source tables it copies
are a **file-path string literal**, not numeric system-info.

- `063050  044077  LDA 77`  -> EA = 063050+77 = **063147**; word@063147 = `063005` (pointer)
- `063057  044071  LDA 71`  -> EA = 063057+71 = **063150**; word@063150 = `063016` (pointer)
- MOVEW copies 9 words from 063005 (`SAA 11` -> L=9) and 3 words from 063016 (`SAA 3`).

Decoding the 12 source words `63005..63020` from `/mnt/d/ND/t/segments/006-S3FS.bin`
(base 26000B, big-endian, high byte first):

```
63005 024123 "(S"   63006 054523 "YS"   63007 052105 "TE"   63010 046451 "M)"
63011 046501 "MA"   63012 041515 "CM"   63013 026501 "-A"   63014 051105 "RE"
63015 040447 "A'"   63016 042101 "DA"   63017 052101 "TA"   63020 023400 "'\0"
```

= the ASCII file path **`(SYSTEM)MACM-AREA'DATA'`**. The neighbouring FILSYS symbols
(`CPTYP`, `GDIRA`=get-dir-addr, `GNAMA`=get-name-addr, `EULOC`) are all directory/file
helpers. So `006-S3FS:CPUST` is a **file routine**, unrelated to GetSystemInfo. VERIFIED.

### 2b. `030-S3SM5 / 026-S3IMPIT @ 124256B` (N500-SYMBOLS) = an ND-500 System-Monitor command

At 124256B (in the resident ND-500 monitor image, executing as ND-100 code) the disassembly
is a **JMP jump-table** (`124256 124024 JMP 24 ->124302`, `124257 170401 SAA 1`,
`124260 135124 JPL I 124 ->124404`, ...), i.e. a small dispatch, and its symbol neighbours are
the ND-500 System-Monitor's own service routines:

```
124256B CPUST      124314B GDEVT (get device)    124351B TMOUT (timeout)
124406B RDPAG (read page)   124410B WDPAG (write page)
```

That is the ND-500 **System Monitor's** internal CPU/page/device command set - not the
SINTRAN `MON 262B` GetSystemInfo call. VERIFIED.

---

## 3. Real MON 262B dispatch - and why the worker body is NOT in the static carve (VERIFIED)

The ND-100 monitor dispatches `MON N` through **GOTAB** (symbol `MGOTA`), base **71233B**,
stride 1, index = N. Cross-checked against known calls
(`GOTAB[5B]@71240B=120355B`, `GOTAB[13B]@71246B=120454B`, `GOTAB[45B]@71300B=121075B`,
`GOTAB[67B]@71322B=121372B` - all byte-exact in
`/mnt/d/ND/t/resident/SINTRAN-DATA_commoncode.bin`).

For 262B:

```
GOTAB[262B] @ 71233B + 262B = 71515B   ->  word = 66262B  (symbol F1737)
```

66262B is **inside a resident pointer table** F1731..F1742 (66240B..66276B), a run of
monotonically increasing address words. Following it:

```
66262B (F1737) = 071511B   ->   071511B = 066256B   ->  ... (further pointer hops)
```

The chain crosses into the **uncarved resident CALLPROC bridge** - the resident monitor
image that fills the buffer is **not present in these carves** (a known limitation of this
carve set: `commoncode.bin` is zero / pointer-only in the resident-monitor body region).

**Consequence: the per-byte "which instruction writes which field" cannot be recovered from
the static L07 bytes for MON 262B.** This is a bytes-grounded negative result, not a gap in
effort. There is no `MOVEW`-from-fixed-table block for 262B in the carved code (unlike the
false-positive file routine in 2a).

Corroboration (VERIFIED): scanning **all** carves
(`commoncode` + `006-S3FS` + `025-S3IRPIT` + `026-S3IMPIT` + `030-S3SM5`) for the system-type
constant 500 (=0764B) shows it **never** occurs adjacent to a plausible OS-type+version word.
The 24-byte block is therefore **not** stored as a static template anywhere in the carve; the
fields are assembled at runtime from separate sysgen variables + live CPU identification.

---

## 4. Best-effort 24-byte layout for a real L07 VSX-500 return

Legend: **V** = VERIFIED from carved bytes; **I** = INFERRED (manual + L07/VSX-500 architecture);
**RT** = produced at runtime, not a static constant in this carve.

| Off | Field | Value for L07 VSX-500 | Source / status |
|-----|-------|-----------------------|-----------------|
| 0:1  | System number (CPU no.) | the machine's CPU/system number | resident var `SYSNO` (commoncode 4051B); **RT/I** (sysgen-set) |
| **2** | **CPU type** | **front-end ND-100-family code (see sec 5), NOT an ND-500 value** | live CPU identity; **RT** - no static constant in carve |
| **3** | **Instruction set** | **front-end ND-100-family code (see sec 5)** | live CPU identity; **RT** - no static constant in carve |
| 4:5  | Microprogram version | front-end ND-110/120 microprogram version | **RT** (read from CPU) |
| 6:7  | System type | **500** | sysgen/install constant; **I** (this is a VSX-500 build) |
| 8    | OS type | **5** (VSX-500) | **I** (VSX-500 = OS 5 per manual; matches this build) |
| 9    | OS version | **'L'** (ASCII 0x4C) | **I** - this carve is release **L07** |
| 10:11| reserved | (undefined by manual) | - |
| 12:13| Patch level | build-specific patch coding | **RT/I** (L07 patch state) |
| 14:15| Sysgen minutes | build timestamp | resident sysgen block near `GENDA` (commoncode 4060B); **RT** |
| 16:17| Sysgen hours | build timestamp | as above; **RT** |
| 18:19| Sysgen day | build timestamp | as above; **RT** |
| 20:21| Sysgen month | build timestamp | as above; **RT** |
| 22:23| Sysgen year | build timestamp | as above; **RT** |

Candidate resident source variables located by symbol (commoncode region 4051B..4060B:
`SYSNO`, `HWINF`, `SINVE`=SINTRAN version, `REVLE`=revision level, `GENDA`=gen date). These
addresses hold the identity/sysgen values, but that carved region is data/code-ambiguous, so
their exact runtime contents are **not** asserted here as constants. INFERRED linkage only.

---

## 5. BOTTOM LINE - bytes 2 and 3 for a real ND-500 process

**Bytes 2-3 do NOT describe the ND-500.** `MON 262B`/CPUST is a SINTRAN III monitor call, and
SINTRAN III runs on the **ND-100-family front-end CPU**; the ND-500 is a coprocessor with no
independent OS. The manual encodings for byte 2 (CPU type 2-5) and byte 3 (instruction set
0-3) are defined **only** for ND-100/110/120 CPUs, and that is exactly what these two bytes
report - the identity of the **front-end** processor that runs SINTRAN, regardless of whether
the requesting process is an ND-100 or ND-500 process.

- There is **no ND-500 value** for bytes 2-3, and no static ND-500 constant exists in the
  carve (VERIFIED: no system-info template, no `MOVEW`-from-table block for 262B). 
- The values are read at **runtime from the front-end CPU's identification** (microprogram /
  hardware), so they depend on the physical front end, not on a byte in the L07 image.
- For a typical L07 **VSX-500** machine the front end is an ND-110/CX-class CPU, which per the
  manual table yields **byte 2 = 4** (48-bit FP) or **5** (32-bit FP) and **byte 3 = 3**
  (16-PIT micro-segment administration). This is **INFERRED** from the manual + the VSX-500
  target; the emulator should treat bytes 2-3 as **"front-end CPU identity, supplied by the
  emulated ND-100/110 CPU model,"** not as a fixed ND-500 constant.

**For the NC emulator:** return byte 2 / byte 3 from whatever ND-100-family CPU the emulator
presents as the SINTRAN host (e.g. 4/3 or 5/3 for an ND-110/CX front end). Do **not** invent
an ND-500-specific value - real L07 has none. Bytes 6:7 = 500, byte 8 = 5, byte 9 = 'L' are
the safe VSX-500 identity fields.
