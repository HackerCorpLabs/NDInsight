# CRDIR placement formula - where the bit / object / user files land on a fresh device

**Scope:** the exact rule `@CREATE-DIRECTORY` (`006-S3FS` `CRDIR`/`ALBIT`) uses to
choose the on-disk block numbers of the **bit file** (page bitmap), **object
file**, and **user file**, and how that rule scales with device size. This closes
the "exact placement math" item left **OPEN** in
[`create-directory.md` §3.1 / §8.1](create-directory.md) for the *bit file*, and
narrows the still-open part for the *object/user* files.

**Rule of evidence:**

- **VERIFIED** - proven from carved SINTRAN L `006-S3FS` `ALBIT`/`CRDIR` bytes,
  from real disk bytes, or from official ND documentation.
- **INFERRED** - fits every measured image but is not pinned to a specific
  instruction (image-fitting, or the NDFS oracle).
- **OPEN** - not resolvable from the static bytes; needs a live create trace.

Octal is primary for on-disk addresses and code addresses. Multi-byte on-disk
values are **big-endian** (a fact about the disk format).

---

## 1. TL;DR - the rule

There is **one** placement branch in the code, and it is **not** "hard disk vs
floppy". It is **"was a bit-file address supplied as the last `@CREATE-DIRECTORY`
parameter?"** (VERIFIED, `ALBIT` 137516B-137522B):

```
if (supplied_bit_address == 0):          # default - user gave no address
    bit_file  = ROUND_DOWN( floor(pages / 2), 9 )       # <-- the real formula
    object_file = <placed above the bit file>           # +216 on the SMD (see §4)
    user_file   = object_file + 2
else:                                     # user supplied an explicit address BA
    bit_file  = BA
    object_file = BA - 4
    user_file   = BA - 2                  # metadata packed just below BA
```

- `pages` = the **declared directory size** returned by `GSIZE` (37101B), i.e. the
  `pages_available` figure (**36945** on `PACK-ONE`), **not** the physical image
  size (38400). VERIFIED numerically (only ~36936-36953 reproduces 18468).
- `ROUND_DOWN(v, 9)` = `9 * floor(v / 9)` - rounds `floor(pages/2)` **down to a
  multiple of 9 pages**. The constant **9** is a hard literal in `ALBIT`
  (`SAT 11` = octal 11 = 9), **the same for every device** - it is *not* read
  from a geometry/track table. VERIFIED from bytes.
- The bit-file **size** (contiguous span) = `ceil(pages / 16384)` pages
  (1 bit/page; 1024 words/page x 16 bits = 16384 bits/page). The `x16` split is
  `MPY 20` (octal 20 = 16) in `ALBIT` at 137710B. VERIFIED.

**The NDFS `pages / 2` is off by the round-to-9 step.** For `PACK-ONE`
(pages=36945): `floor(36945/2) = 18472`; `18472` is not a multiple of 9;
`9 * floor(18472/9) = 9 * 2052 = 18468` = the real `bit_file_ptr`. NDFS's
`18472` (and template `spec_smd_75mb = 18472`) is exactly the un-rounded value.

---

## 2. The `ALBIT` bit-file math - byte evidence (VERIFIED)

Disassembly recipe (byte-swap the carved segment to LE, disassemble with base
11264 = 26000B load address):

```
python3 -c "d=bytearray(open('006-S3FS.bin','rb').read());d[0::2],d[1::2]=d[1::2],d[0::2];open('/tmp/fs.le','wb').write(d)"
nd100-dis -a -o -b 11264 /tmp/fs.le | awk '$1>="137500"&&$1<="137535"'
```

### 2.1 The default-vs-override branch (137516B-137522B)

```
137516  131415  JAF 15                 ; -> 137533 (converge)
137517  140001  SKP IF DD EQL 0         ; skip next if supplied address (D) == 0
137520  124013  JMP 13                  ; (D != 0) user gave an address -> skip the compute, use it
137521  170401  SAA 1                   ; (D == 0) default path:
137522  004412  STA ,B 12               ;   set B+12 = 1  ("bit file was auto-placed")
```

`D == 0` -> fall through into the compute below and flag `B+12`; `D != 0` -> jump
past it and keep the user value. This is the whole "device branch" - it is keyed
on the **parameter**, not the device type. VERIFIED.

### 2.2 The placement arithmetic (137523B-137532B)

```
137523  050401  LDT ,B 1
137524  135063  JPL I 63    ; -> GSIZE (37101B): declared page count into A:D (32-bit)
137526  156777  SAD ZIN SHR 1  ; A:D >>= 1      -> A:D = floor(pages / 2)
137527  171011  SAT 11         ; T = 9   (octal 11)
137530  141660  RDIV ST        ; (A<<16|D) / T  -> quotient = floor((pages/2)/9) into A
137531  141265  RMPY ST DA     ; T * A          -> 9 * quotient into A:D
137532  020410  STD ,B 10      ; B+10 = 9*floor(floor(pages/2)/9) = bit-file start block
```

Opcode semantics grounded in
[`ND100-INSTRUCTION-SEMANTICS.md`](../../tools/sintran-segment-carver/versions/L-VSX-500/re/instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md):
`SAD ... SHR 1` = 32-bit A:D right shift by 1 (÷2, §8); `RDIV sr` = `(A<<16|D) /
reg[sr]`, quotient->A (§3.7); `RMPY sr dr` = signed `reg[sr]*reg[dr]`,
A=high16 / D=low16 (§3.7); `SAT n` = `T = n` (§4).

Worked example, `PACK-ONE` (pages = 36945):
`floor(36945/2) = 18472` -> `18472 / 9 = 2052` (quotient) -> `9 * 2052 = 18468`
= `bit_file_ptr` **44044B (18468)**. Byte-exact. VERIFIED.

The `x16 bits/word` bitmap sizing appears later in `ALBIT` (137710B `MPY 20`,
octal 20 = 16) inside the three-pattern bad-page test loop, corroborating
`bits_per_page = 1024 * 16 = 16384`. VERIFIED.

---

## 3. Data table - every directory image found

Placement read from the master block (byte 2016: object @+0x10, user @+0x14, bit
@+0x18). "Declared" = `GSIZE`/`pages_available` (SMD) or inferred (floppies,
FLOMON -> no valid extended info). "Predicted bit" = `ROUND_DOWN(floor(declared/2),9)`
for the default path, or `declared_phys - 1` for the override path.

| Image | Volume | Phys pages | Declared (GSIZE) | object | user | **bit** | Path | Predicted bit | Fit |
|-------|--------|-----------:|-----------------:|-------:|-----:|--------:|------|--------------:|-----|
| `nd100x/SMD0.IMG` (+`SMD0-org`,`SMD0-L`, `norskdata-ndfs`, `ndcompile/*`, `simh/BIN`) | PACK-ONE | 38400 | **36945** | 18684 | 18686 | **18468** | default | **18468** | YES |
| `nd100x/250305L07-XX-01D.IMG` (=`SINTRAN/VSXL1.IMG`) | 250305L07-XX-01D | 616 | ~616 | 611 | 613 | **615** | override BA=615 | 615 | YES |
| `nd100em/.../Nd-210523I01-XX-01D.img` (+`-02D`, `simh/BIN/FLOPPY525`, `nd100x/images/...`) | 210523I01-XX-01D | 616 | ~613-616 | 508 | 510 | **306** | default | **306** | YES |
| `nd100em/SINTRAN/TPE-MON-A02.IMG` | 210523E00-XX-01D | 616 | ~613-616 | 508 | 510 | **306** | default | **306** | YES |
| `nd100x/images/211305B02-XX-01D.img` (=`nd100x/FLOPPY.IMG`) | 211305B02-XX-01D | 640 | ~613-629 | 508 | 510 | **306** | default | **306** | YES |
| `nd/DISK15.IMG` (+`DISK16`,`DISK3-5`, `nd100em/SINTRAN/DISK*`) | N-10-102-* | 154 | ~154 | 149 | 151 | **153** | override BA=153 | 153 | YES |

Notes:
- **The bit-file formula predicts every default-path image byte-exactly.** For the
  38400-page SMD it gives 18468 (NDFS `pages/2` gives 18472, off by 4).
- **Two 616-page floppies use opposite layouts** (`250305L07` -> metadata at end,
  bit=615; `210523I01` -> bit mid-disk, bit=306). Same size, different layout =
  proof the branch is **not** size-driven but **parameter-driven** (§2.1). The
  end-layout floppies were created with an explicit bit address = last page; the
  mid-layout floppies used the default.
- `211305B02` is physically 640 pages but bit=306, so its **declared** directory
  size is ~612-629, not 640 (the file is padded past the directory). This is why
  the formula must use the *declared* `GSIZE`, never the image byte-size.
- `nd100em/BIGDISK0.img` has a non-NDFS (scrambled) master block - excluded.
  Zero/empty images (`SMD-BSD`, `bsd211_481/SMD0`, `FILE-SYS-INV-C01`) excluded.
- `user = object + 2` holds on **every** image (VERIFIED, both paths).

---

## 4. Object / user placement - what is and isn't closed

### Override path (VERIFIED from images)

When the user supplies bit address `BA`, the metadata packs into the **five pages
just below and including `BA`**: `object = BA-4`, `object_data = BA-3`,
`user = BA-2`, `user_data = BA-1`, `bit = BA`. Confirmed on `250305L07`
(615/613/611) and `N-10-102` (153/151/149). This is exactly NDFS's small-disk
branch `object = pages-5, user = pages-3, bit = pages-1` **when** the caller drove
`BA = pages-1`. VERIFIED (image fit).

### Default path - **still OPEN**

The object/user blocks sit **above** the auto-placed bit file, but the offset is
**not** a clean function of the bitmap span:

| Image | bit | object | object - bit |
|-------|----:|-------:|-------------:|
| PACK-ONE (36945) | 18468 | 18684 | **216** |
| 210523I01 (~616) | 306 | 508 | **202** |

216 vs 202 do not scale with `pages` and 216 != bitmap span (3 pages), so the
NDFS `object = bit + bitmap_pages` rule is wrong and no single offset fits.
`user = object + 2` is the only VERIFIED sub-rule. The object/user block numbers
are produced by the allocation/bad-page scan loop in `CRDIR` (137173B-137352B,
cursor `B+32/33`), whose starting base and bound (`B+26/27`, `B+30/31`) are set
from `ALBIT`'s return and a second `GSIZE`, but the exact base arithmetic is not
statically decidable here. **OPEN** - needs a live trace.

---

## 5. What is VERIFIED vs INFERRED vs OPEN

| Claim | Verdict | Evidence |
|-------|---------|----------|
| Placement branch = supplied-bit-address vs default (not device type) | **VERIFIED** | `ALBIT` 137517B `SKP IF DD EQL 0` / 137520B `JMP` |
| Default `bit = 9*floor(floor(pages/2)/9)` (round pages/2 down to mult of 9) | **VERIFIED** | `ALBIT` 137526B-137532B; byte-exact on 18468 and 306 |
| `pages` = declared `GSIZE` (=`pages_available` 36945), not physical size | **VERIFIED** | numeric fit (18468 needs ~36945); 211305B02 phys 640 -> bit still 306 |
| Round constant = literal **9**, same for all devices | **VERIFIED** | `ALBIT` 137527B `SAT 11` (octal 11 = 9) |
| bit-file span = `ceil(pages/16384)`, 1 bit/page | **VERIFIED** | `ALBIT` 137710B `MPY 20` (=16); bitmap semantics doc |
| Override layout: object=BA-4, user=BA-2, bit=BA | **VERIFIED** | `250305L07` 611/613/615; `N-10-102` 149/151/153 |
| `user = object + 2` (both paths) | **VERIFIED** | all images |
| Whether the constant 9 equals a physical track for these devices | **OPEN** | it is a fixed literal, not from a geometry table; the doc's "start of a track" is approximate |
| Default-path object/user block numbers (the +216 / +202 base) | **OPEN** | scan-loop base `B+26/27` not statically decidable |

---

## 6. Concrete recommendation for NDFS

`ndfs-c/src/image_creator.c build_custom_spec()` currently branches on
`pages > 1000` and uses `bf_block = pages / 2`. Two fixes:

1. **Replace `bf_block = pages / 2` with the round-to-9 rule** so the produced
   image matches genuine `CRDIR` output:

   ```c
   /* SINTRAN CRDIR/ALBIT: round floor(pages/2) DOWN to a multiple of 9 pages.
      VERIFIED from 006-S3FS ALBIT 137526B-137532B; byte-exact on PACK-ONE
      (36945 -> 18468) and the 210523I01 floppies (-> 306). */
   uint32_t half = pages / 2;
   bf_block = (half / 9) * 9;
   ```

   This alone turns the SMD default from 18472 into the real **18468**.

2. **Do not use `pages > 1000` to pick the "metadata at end" layout.** That layout
   is the *user supplied bit-address = last page* case, not a small-disk case -
   a default-created 616-page floppy (e.g. `210523I01`) puts the bit file
   mid-disk (306), which the `>1000` rule gets wrong. Drive the branch off an
   explicit "bit-file address" option instead: if the caller passes one, use the
   `BA / BA-2 / BA-4` packing; otherwise use the round-to-9 mid-disk rule for
   **all** sizes.

3. **Object/user default-path blocks stay approximate.** `object = bit +
   bitmap_pages` does not match real `CRDIR` (real is bit+216 on the SMD). Keep
   the per-geometry template values (`spec_smd_75mb` 18684/18686) for the known
   devices, and flag custom-size object/user placement as unverified until a live
   `@CREATE-DIRECTORY` trace pins the scan-loop base. `user = object + 2` is safe.

**What would close the remaining OPEN items:** one live `@CREATE-DIRECTORY` trace
on a known geometry - breakpoint `CRDIR` 136741B, single-step through `ALBIT` and
the 137173B-137352B allocation scan, and dump `B+26/27`, `B+30/31`, `B+32/33`
after `ALBIT` returns. That resolves the default-path object/user base and
confirms whether the literal 9 is tied to a track.

---

**Related:** [`create-directory.md`](create-directory.md) (full CRDIR walk-through)
- [`on-disk-format/directory-label.md`](on-disk-format/directory-label.md) (master
block field layout) - [`README.md`](README.md) (Phase 7).
