# Making TCP/IP product floppies for SINTRAN

How to build floppies that match the **original ND TCP/IP Gateway D02 distribution**
(product 211185), and how to format a blank one on a running SINTRAN under `nd100x`.

Everything in the "verified" tables below is read off primary evidence - the product's own
install log, the `.xat` metadata that came off the pack with the files, real ND product
floppy images, and the emulator source. Anything I could not verify is marked as such and
kept separate. Nothing here is from memory.

---

## 1. What the original product actually was

**Two floppies.** Verified from the installer's own log,
`x/D02-gateway-and-clients/SYSTEM/IN-TCP-IP-XX-D02:LOGG`, which records the real install on
the customer's machine (dated `92.01.21`).

| Floppy | Volume name | User | Mounted as |
|---|---|---|---|
| 1 | `211185D02-XX-01D` | `FLOPPY-USER` | `FLOPPY-DISC-1`, unit 0 |
| 2 | `211185D02-XX-02D` | `FLOPPY-USER` | `FLOPPY-DISC-1`, unit 0 |

The log types the shorter form `211185D-XX-01D`. That is not a different name - SINTRAN
abbreviates **by character prefix per name part**, parts split on `-`, and `211185D` is a
prefix of `211185D02`. The full name is what the file listing prints.

### Files, with their real size on the original floppy

Page counts are from the install log itself, which printed `INDEXED <n> pages` for each
file as it copied. They agree exactly with the `.xat` metadata extracted from the pack, so
two independent sources say the same thing.

**Floppy 2 - `211185D02-XX-02D`** (VERIFIED - file numbers are the floppy's own index)

| # | File | Pages |
|---:|---|---:|
| 1 | `TCP-SER-B0-D02:BPUN` | 65 |
| 2 | `TCP-SER-B1-D02:BPUN` | 65 |
| 3 | `TCP-SER-B2-D02:BPUN` | 65 |
| 4 | `TCP-SER-B3-D02:BPUN` | 65 |
| 5 | `PO-STOP-D02:PROG` | 5 |
| 6 | `PO-PWRFAIL-D02:PROG` | 23 |
| 7 | **unknown - existed, not copied** | ? |
| 8 | `DEFINE-TCPP-D02:MODE` | 1 |
| 9 | `DEFINE-FTPRT-D02:MODE` | 1 |

**Floppy 1 - `211185D02-XX-01D`** (VERIFIED)

| # | File | Pages |
|---:|---|---:|
| 1-6 | **unknown - existed, not copied** | ? |
| 7 | `FTP-SERVER-C07:PROG` | 76 |
| 8 | `TCPP-D02:PROG` | 56 |
| 9 | `FTPRT-D02:PROG` | 26 |

**Why there are gaps.** The installer ran `BACKUP-SYSTEM` `COPY-USERS-FILES` with **manual
SELECT**, so only the files it needed were copied and logged. The numbering is the floppy's
own file index, not a count of what was copied - which is how we know floppy 1 held six more
files and floppy 2 held one more. Their names are not recoverable from the log. Do not
invent them; a reconstruction is a reconstruction.

---

## 2. Floppy geometry - the numbers, and where each comes from

| Source | Says |
|---|---|
| ND-30.003.007 System Supervisor | format `17B` = DS/DD, **612 pages** allocatable; only formats `0B` and `17B` are legal under SINTRAN |
| `nd100x/src/devices/floppy/deviceFloppyDMA.c:56-68` | format 17b = 1024 b/sector, 8 sectors/track, 77 tracks, 2 sides, **616 pages total, 612 allocatable**; notes this is PC "1.2 MB high density" |
| `nd100x/floppy.img` | 1,261,568 bytes = 616 pages |
| ndfs-py `ImageTemplate.Floppy12MB` | 616 pages |
| **Real ND product floppies** (`210580B01-XX-01D`, `210374E04-XX-01D`) | **1,310,720 bytes = 640 pages**, one user `FLOPPY-USER` |
| Software archive, 1066 real floppies | 640 pages on 195 of them; 616 pages on 115 |

**Both 616 and 640 are real.** They are not a contradiction - 616 is the 77-track format the
manual and controller describe, 640 is the 80-track media that ND actually shipped product
on. Use **640 pages / 1,310,720 bytes**, because that is what every real ND product floppy
in this repo is, including two from the same era as this product.

`nd100x` detects the format **from file size alone** (`diskFileSize >= 1261568` -> 1024
bytes/sector, double sided, double density), so a correctly sized file is recognised with no
further setup.

---

## 3. The trap: these `:PROG` files are SPARSE

`FTP-SERVER-C07:PROG` is `bytes_in_file` 299,008 but `pages_in_file` **76**, with 70 holes.
The extracted file on the PC is the full address range; the pages actually allocated are far
fewer. Counting the byte size instead of the allocated pages overstates the whole kit by
almost 70% - it gives 1605 pages against a true 952.

**Copy them hole-for-hole**, or a 76-page file becomes a 146-page one and the floppy fills
up for no reason:

```python
src   = source.read_file(path, parity="none")          # no transform
holes = [i for i, b in enumerate(source.get_file_blocks(path)) if b == 0]
dest.write_file(path, src, "none", holes if holes else None)
```

The `.xat` sidecar beside each extracted file already carries the `holes` list and
`pages_in_file`, so the answer does not have to be recomputed.

Also: **do not re-apply parity** when copying pack-to-pack. A real pack is mixed - on the c3
pack `AIP-CONFIG`, `AIP-HOSTS` and `AIP-SERVICES` carry parity while `AIP-NETWORKS` and
`AIP-PROTOCOL` do not. Byte-exact copying sidesteps the question entirely.

### Whole D02 kit, true cost

**952 pages.** Two 640-page floppies give ~1268 usable, so everything fits with room to
spare - including the D01 clients and the programmer's library, which belong to *different*
products (211154 and the SLIB/NK/SKP set) and were never on the 211185 floppies.

---

## 4. Formatting a blank floppy on the running machine

### 4a. Make the blank

```powershell
$fs = [System.IO.File]::Create('BLANK-640.IMG')
$fs.SetLength(1310720)      # 640 pages x 2048
$fs.Close()
```

Keep an untouched copy. Once SINTRAN has formatted it, back up the **formatted empty** image
too - that becomes the reusable blank and saves doing this dance again.

### 4b. Mount it

In `nd100x.ini`, unit 0 of the floppy controller is peripheral file `FLOPPY-1`; unit 1 is
`FLOPPY-2`, and so on.

```ini
[controller.floppy.0]
enabled = yes
disk0 = BLANK-640.IMG
```

### 4c. On the machine, logged in as SYSTEM

```
@DEVICE-FUNCTION FLOPPY-1
FUNCTION: SET-FLOPPY-FORMAT
FORMAT (OCT): 17

@DEVICE-FUNCTION FLOPPY-1
FUNCTION: FORMAT-FLOPPY

@CREATE-DIRECTORY
DIRECTORY NAME: 211185D02-XX-01D
DEVICE NAME: FLOPPY-DISC-1
DEVICE UNIT: 0
BIT FILE ADDRESS:              <- leave blank on a floppy

@ENTER-DIRECTORY 211185D02-XX-01D,FLOPPY-DISC-1,0
@CREATE-USER FLOPPY-USER
@GIVE-USER-SPACE FLOPPY-USER,600
```

**`FORMAT-FLOPPY` does nothing under `nd100x`.** `deviceFloppyDMA.c:608` prints
`Starting FormatFloppy` and returns success without touching the image. That is harmless -
formatting lays down sector headers, which a flat image file already has by definition - but
it means the step you can *see* is not the step that matters. **`@CREATE-DIRECTORY` is what
actually writes a file system.** On real hardware the format is not optional.

`SET-FLOPPY-FORMAT` is likewise not needed under the emulator, since the format is inferred
from the file size. Both are kept above because they are correct on real hardware.

### 4c-bis. What actually happened when this was run (2026-08-23)

Both floppies were formatted this way on `nd100x 1.0.12` in WSL, booting SINTRAN III
VSX/500 L from a **copy** of the pack. Result, confirmed from both ends:

| Check | Floppy 1 | Floppy 2 |
|---|---|---|
| SINTRAN `LIST-USERS` | `USER 0 : 211185D02-XX-01D:FLOPPY-USER` | `USER 0 : 211185D02-XX-02D:FLOPPY-USER` |
| ndfs-py, host side | vol `211185D02-XX-01D`, 640 pg, 636 free, integrity True | vol `211185D02-XX-02D`, 640 pg, 636 free, integrity True |

`FORMAT-FLOPPY` printed `Starting FormatFloppy on drive position 0` and returned **at
once** - no minutes of formatting, no bad-page report. That is the no-op above, observed
live rather than only read in the source. The image was byte-identical afterwards; the
first change to it came from `@CREATE-DIRECTORY`, which put `211185D02-XX-01D` in the
master block at offset `0x7E0`.

Two syntax notes worth keeping:

- `@CREATE-USER` takes **`<directory>:<user>` with no parentheses**.
  `CREATE-USER (211185D02-XX-01D:)FLOPPY-USER` answers `ILLEGAL CHARACTER IN PARAMETER`.
- The bit-file prompt appears as `OCTAL BIT FILE ADDRESS (-1 END OF DISC):` and takes a
  bare CR on a floppy.

A freshly formatted empty image is kept as `FORMATTED-BLANK-640.IMG`, so this does not
have to be repeated - copy it and it is already a valid empty NDFS floppy. The one thing
it carries that a generic blank should not is the **volume name**, and ndfs-py has no
rename call, so a copy is only reusable as `211185D02-XX-01D` unless it is re-formatted
under SINTRAN.

### 4d. Release it before shutting down

```
@RELEASE-DIRECTORY 211185D02-XX-01D
```

A directory left entered can leave the image inconsistent, and the pack has to be released
before it can be entered again on a later boot.

---

## 4e. UNRESOLVED: SINTRAN cannot see files that ndfs-py writes

**The floppies are built and byte-correct, and the machine still lists no files.** This is
open, not finished.

What was measured, in order:

| Step | Result |
|---|---|
| Write files into the SINTRAN-formatted blank | pages allocated, entries **lost on reopen** |
| Cause of that | master block object-file pointer (+0x10) is **0** - SINTRAN allocates the catalogue lazily, on first file creation |
| Create the first file from SINTRAN instead | `NOT DIRECTORY ACCESS` for user SYSTEM |
| Build with ndfs-py `create_image`, Custom 640 pages | `OUTSIDE DEVICE LIMITS` on `@ENTER-DIRECTORY` |
| Build with `Floppy12MB` (616 pages) | mounts, `LIST-USERS` works, **`LIST-FILES` empty** |
| **Control: genuine ND floppy `210580B01-XX-01D`, 640 pages** | **mounts and lists all 12 files** |
| Clone that real floppy - empty it, rename, write our files | mounts, `LIST-USERS` right, **`LIST-FILES` still empty** |

The control run is the important one: the same emulator, controller and SINTRAN read a
real ND product floppy perfectly. So this is **not** the emulator, not the media size, and
not the 640-page geometry - my earlier conclusion that the device is 616 pages was wrong
and the control disproved it.

It is the object entries themselves. Diffing the entry page (block 509, reached through
the index at 508) between the real floppy and our clone:

```
real  90 00 45 4e 43 4f 53 2d ...   00 08 12 40 ... dates present ...
ours  80 00 54 43 50 2d 53 54 ...   00 08 00 00 ... dates zero ...
```

- **byte 0: `0x90` on every SINTRAN-written entry, `0x80` on every ndfs-py one.** ndfs-py
  documents bit 7 as "in use" and never sets bit 4. Setting `0x10` on all 12 entries and
  re-testing did **not** fix it, so that bit alone is not the answer.
- offset 26-27 access bits `0x07FF` vs `0x03FF`
- offset 30-31 `0x1240` vs `0x0000`
- offsets 36-51 creation/read/write dates: real has them, ours are zero

Any of the last three could be what `LIST-FILES` filters on. That is a hypothesis and has
not been tested.

**Two ways forward, neither attempted yet:**

1. Fix ndfs-py's object-entry writing to match SINTRAN. The evidence needed is in the diff
   above; the test is `LIST-FILES` on the machine, not a round-trip in Python. ndfs-py
   reads and writes its own entries happily, which is exactly why this went unnoticed.
2. Write the floppies from inside SINTRAN with `BACKUP-SYSTEM COPY-USERS-FILES` - the same
   tool the original installer used to read them. That needs the source files on a pack the
   machine can see, and sidesteps the entry-format question entirely because SINTRAN writes
   the entries.

## 5. Open questions

- **Floppy 1 files 1-6 and floppy 2 file 7.** They existed on the originals and their names
  are not in the log. Candidates from the D02 product that are otherwise unplaced:
  `TCPIP-MONITOR:PROG` (84), `TCP-START-D02:MODE`, `TCP-STOP-D02:MODE`,
  `TCP-IP-LO-D02:MODE`, `TCP-IP-LO-D02:LIST`, `TCP-START-D02:LIST`, plus the installer
  itself (`IN-TCP-IP-XX-D02:PROG`, not present in the extracted kit) and its `:INST` answer
  file. That is a plausible fit for seven slots and **it is a guess** - the count matching is
  not evidence.
- **Whether the originals were 616 or 640 pages.** No 211185 floppy survives in the software
  archive to measure. 640 is chosen to match contemporary ND product floppies, not because
  this product is known to have used it.

---

**Parent:** [../README.md](../README.md) ·
**Related:** [../COPYING-FILES-TO-SINTRAN.md](../COPYING-FILES-TO-SINTRAN.md) ·
[../RE/README.md](../RE/README.md)
