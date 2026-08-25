# Copying files onto a SINTRAN III pack

**How to get files from Windows onto a SINTRAN III disk image, without silently corrupting
them.** Written while installing COSMOS TCP/IP, but nothing here is TCP/IP-specific.

Everything below was measured on 2026-08-21 unless marked otherwise. Where a claim is not
verified it says so.

**Tooling:** the `ndfs-py` library and the `ndtool` CLI both come from
**norskdata-ndfs** — <https://github.com/HackerCorpLabs/norskdata-ndfs>.
Take `ndtool` from the **releases** page: <https://github.com/HackerCorpLabs/norskdata-ndfs/releases>

---

## 1. Pick a route

| Route | Use it when | Safe for writes? |
|---|---|---|
| **A — `ndfs-py`** | the normal case: bulk copy into a `.img` | **Yes** (0.0.6) |
| **B — `ndtool.exe`** | listing, extracting, `--fsck` | Yes (0.0.6) — see §5 |
| **C — floppy image** | you want the machine to do the copy itself | Yes |
| **D — `COPY-FILE ,TERM`** | one short text file, no tooling | Yes, but tedious |

Route A is what the rest of this document assumes. It is the only one that restores the
original file metadata (type, access bits, dates, sparse holes) in one step.

**Two conditions apply to A and B both:**

- **Stop the emulator first.** A running RetroCore holds the image. Writing to a pack it
  has open is not safe; if you are lucky the write fails with `PermissionError`, which is
  the good outcome. Stop the machine, back up, write, restart.
- **Work on a copy.** These packs are irreplaceable.

---

## 2. The parity problem — read this before you write anything

SINTRAN stores **text** files with ND-100 **even parity**: bit 7 of each byte is set only
when the low seven bits contain an odd number of ones. Binary files (`:PROG`, `:BPUN`,
`:BRF`, `:DATA`) are raw and must never be touched.

Get this wrong in either direction and the failure is quiet:

| Mistake | What you see |
|---|---|
| Parity missing where it is needed | SINTRAN *displays* the file correctly on a terminal, so it looks fine — but DMAC and any program reading it hit `ILL. CHARACTER` on nearly every byte. A 4,800-line patch mode file once produced 2,812 `ILL. CHARACTER` errors and did nothing. |
| Parity applied to a binary | The program is destroyed. `ndtool -x -p` over a whole pack corrupts every `:PROG` on it. |

### You cannot decide parity from the file type. It is per-file, per-pack.

This is the part that catches people. Measured across the two source packs for this
product:

| File | c3 pack | tingo pack |
|---|---|---|
| `AIP-CONFIG:SYMB` | **parity** (69.2% high-bit) | — |
| `AIP-HOSTS:SYMB` | **parity** (62.4%) | **parity** (65.8%) |
| `AIP-NETWORKS:SYMB` | no parity (0.0%) | no parity (0.0%) |
| `AIP-PROTOCOL:SYMB` | no parity (0.0%) | no parity (0.0%) |
| `AIP-SERVICES:SYMB` | **parity** (72.3%) | **no parity (0.0%)** |
| `SKP-C00:DEFS` | no parity | — |
| `SKP-C00:IMPT` | **parity** (52.7%) | — |
| `SKP-C00:INTL` | no parity | — |
| `SLIB:DEFS` / `SLIB:IMPT` | no parity | — |
| `TCP-START-D02:MODE` | **parity** (76.6%) | — |

Two rows kill every shortcut anyone is tempted to invent:

- **`SKP-C00` is one base name with three types and two different answers.** So "all
  `:SYMB` have parity" or "all `:DEFS` do not" are both wrong.
- **`AIP-SERVICES:SYMB` has parity on one pack and not on the other.** So it is not even a
  property of the filename. It is a property of *that file on that pack*.

A genuine ND text file with parity has roughly **35–75%** of its bytes with bit 7 set. A
7-bit file has **0.0%**. That measurement is the check — not the file type.

### The extracted kit in `x/` is 7-bit

Every text file in `x/` reads **0.0% high-bit**: the extractor stripped parity, and in
doing so it normalised away the distinction in the table above. **The sidecars do not
record parity either.**

So there are two honest ways to restore:

1. **Byte-exact from the source pack** (best — see §3b). Reproduces the original exactly
   and sidesteps the question.
2. **From `x/`, re-applying parity only to the files the table above says had it**
   (§3c). Correct for the files listed; for anything not listed, measure the original.

---

## 3. Route A — `ndfs-py` (recommended)

The `ndfs-py` library from **norskdata-ndfs** — https://github.com/HackerCorpLabs/norskdata-ndfs
Use **0.0.5 or later**; 0.0.6 was what this was verified against (2026-08-21).
No build step: put its `ndfs-py/src` on `PYTHONPATH`.

```powershell
$env:PYTHONPATH = '<ndfs-repo>\ndfs-py\src'
```

### 3a. Look before you write

```python
from ndfs import NdfsFileSystem
fs = NdfsFileSystem(open(r'mypack.img','rb').read(), read_only=True)
print(fs.get_directory_name())
for u in fs.get_users():
    print(u.name, u.reserved_pages)
```

Create the destination user first if it is missing — `fs.add_user(name, reserved_pages)`.
For TCP/IP that is user `TCP-IP` with about a 1000-page quota.

### 3b. Byte-exact copy, pack → pack — the safest thing you can do

No transformation at all, so parity, holes and content are reproduced exactly.

```python
src = NdfsFileSystem(open(SRC,'rb').read(), read_only=True)
dst = NdfsFileSystem(bytearray(open(DST,'rb').read()))

for e in src.get_object_entries():
    if e.user_name != 'TCP-IP':
        continue
    path = f'{e.user_name}/{e.object_name}:{e.type}'
    data, props = src.read_file_with_properties(path)   # raw bytes + XAT metadata
    dst.write_file_with_properties(path, data, props)   # restores holes, access, dates

open(DST,'wb').write(dst.to_buffer())
```

`write_file_with_properties` is the one call that legitimately recreates **sparse holes**,
because the sidecar records where the *original's* holes were. That matters: a hole is a
property of the file, not of the data. Inferring holes from "this page is all zero"
produces a file SINTRAN cannot read — `READ-BI` and `READ-PROGFILE` fail on it with
`NO SUCH PAGE / ERROR IN ACCESSING INPUT FILE`.

It also restores **access bits**, so there is no separate "make it readable" step.

### 3c. Copy from the extracted kit in `x/`

Each file has a `.xat` sidecar carrying its real SINTRAN name, type, access bits, dates and
hole map. Use it — the Windows filename has lost the `:` (see §8).

```python
import json, glob, os
from ndfs import NdfsFileSystem

# the files that carried parity on the c3 pack (measured - see §2)
NEEDS_PARITY = {
    ('SYSTEM','AIP-CONFIG','SYMB'), ('SYSTEM','AIP-HOSTS','SYMB'),
    ('SYSTEM','AIP-SERVICES','SYMB'),
    ('TCP-IP','DEFINE-FTPRT-D02','MODE'), ('TCP-IP','DEFINE-TCPP-D02','MODE'),
    ('TCP-IP','TCP-IP-LO-D02','MODE'),   ('TCP-IP','TCP-IP-LO-D02','LIST'),
    ('TCP-IP','TCP-START-D02','MODE'),   ('TCP-IP','TCP-START-D02','LIST'),
    ('TCP-IP','TCP-STOP-D02','MODE'),
    ('TCP-IP','TCP-IP-LO-C07','LIST'),   ('TCP-IP','TCP-START-C07','LIST'),
    ('TCP-IP','SKP-C00','IMPT'),
}

dst = NdfsFileSystem(bytearray(open(DST,'rb').read()))

for xat_path in glob.glob(os.path.join(KIT, '**', '*.xat'), recursive=True):
    props = json.load(open(xat_path))
    user  = props['ndfs.user_name']
    name  = props['ndfs.object_name']
    typ   = props['ndfs.type']
    data  = open(xat_path[:-4], 'rb').read()          # strip ".xat"

    if (user, name, typ) in NEEDS_PARITY:
        data = bytes((b & 0x7F) | (0x80 if bin(b & 0x7F).count('1') % 2 else 0)
                     for b in data)

    dst.write_file_with_properties(f'{user}/{name}:{typ}', data, props)

open(DST,'wb').write(dst.to_buffer())
```

**`parity` values in this library are `"none"` and `"set"`** — not `"even"`. An older note
claimed `write_file(path, data, "even")` silently failed to set parity; the reason is that
`"even"` was never a valid value. Use `"set"`, or encode it yourself as above.

The encoder is verified: it reproduces `DEFINE-TCPP-D02:MODE`, `XMSG-START:MODE` and
`TCP-START-D02:MODE` byte for byte. `@` = 0x40 → 0xC0, `C` = 0x43 → 0xC3, `-` = 0x2D
unchanged.

**Line endings:** ND text is **CR** only — but not always. `AIP-CONFIG:SYMB` on c3 is
**CRLF**. Copy whatever the original had; do not normalise.

---

## 4. Route C — via a floppy image

Lets the machine do the copying, which means SINTRAN decides the on-disk form and the
parity question does not arise.

1. Build or obtain a floppy `.img` containing the files.
2. Attach it to the emulator's floppy controller.
3. On the machine:

```
@ENTER-DIRECTORY ,,FLOPPY-DISC-1,0
@COPY-FILE "(PACK-ONE:TCP-IP)TCP-START-D02:MODE",(<label>:TCP-IP)TCP-START-D02:MODE
```

Quotes wrap the **entire** destination specification including the `(USER)` prefix —
`"(USER)NAME:TYPE"`, never `(USER)"NAME:TYPE"`. Quotes mean "create this file", so they go
on the destination.

Product installers usually expect this route, and some of them reopen themselves by bare
name against the default directory — if one aborts with `"<name>:PROG" NO SUCH FILE NAME`,
copy the installer onto `SYSTEM` and run it by bare name.

---

## 5. Route B — `ndtool.exe`

**Get the latest release binary from https://github.com/HackerCorpLabs/norskdata-ndfs/releases** —
do not hunt for a build directory on disk. Verified here against 0.0.6 (2026-08-21).

> **Always run `-V` first.** Versions before 0.0.5 **read and write the allocation bitmap
> byte-swapped** and can hand out pages SINTRAN is already using, overwriting live file
> data. Such a build is fine for reading and must never be used for `--put`, `--rm`,
> `--create`, or any user/quota/friend change.
>
> **Do not pick a binary out of a local build directory.** Two builds of different
> versions can sit side by side, and the stale one is easy to grab by accident — that
> is exactly how this document first got the version wrong. Take the release.

0.0.6 is past that bug by version and is verified here for **reading** (users, file listing,
against a real pack). **A write has not been tested with it**, so if you do write with
ndtool, work on a copy and `--fsck` afterwards. Route A remains the recommended write path.

```powershell
$nd = '<path-to>\ndtool.exe'                  # from the releases page

& $nd -V                                      # expect 0.0.6 or later
& $nd -i -v  mypack.img                       # volume info + integrity
& $nd -u     mypack.img                       # users and quotas
& $nd -t -u TCP-IP mypack.img                 # one user's files
& $nd --fsck mypack.img                       # 5-phase check
& $nd --stat 'TCP-IP/TCP-START-D02:MODE' mypack.img

# extract WITH sidecars - do this
& $nd -x --xat -o out\ mypack.img
# binaries: no -p
& $nd -x -d -l -o out\ mypack.img
# text only: -p strips parity
& $nd -x -p -F 'TCP-IP/*:MODE' -o out\ mypack.img
```

If `-V` reports below 0.0.5, **download a current release** (https://github.com/HackerCorpLabs/norskdata-ndfs/releases).
To build it yourself instead, from a checkout of the repo:

```powershell
cmake -S <ndfs-repo>\ndfs-c -B <ndfs-repo>\ndfs-c\build -G "MinGW Makefiles"
cmake --build <ndfs-repo>\ndfs-c\build --target ndtool
& $nd -V     # confirm it moved
```

Note that `ndtool --put` leaves PUBLIC access **NONE**, so it needs a follow-up
`--chmod PUBLIC+R`. Route A with a sidecar does not, because the access bits come from the
sidecar.

---

## 6. Route D — type it in at the terminal

For one short text file with no tooling at all:

```
@COPY-FILE "(TCP-IP)MYFILE:SYMB",TERM
```

…then type the content, terminated with **CTRL-L**. SINTRAN applies its own parity, so the
result is a proper ND text file.

QED is the other option but **is unreliable under RetroCore** [open bug]: creating a *new*
file crashes with `ERROR 24 AT 11452; ILLEGAL INSTRUCTION`, reproducible at the same
address on K and L images, so it is emulator- or binary-side rather than an install fault.
Writing to an **already existing** file with an **unquoted** name works. Workaround:
`CREATE-FILE` first, then edit.

---

## 7. Verify the copy — do not skip this

**1. Bank images carry their own checksum.** `:BPUN` PIOC banks are the one ND file type
where "is this intact?" has a real answer:

```powershell
python verify_pioc_bank.py mypack.img
```

All eight banks in `x/` pass (verified after the copy into this repo, 2026-08-21):

```
TCP-SER-B0-D02  computed=2890 stored=2890  OK    74.4% content
TCP-SER-B1-D02  computed=5421 stored=5421  OK    30.1%
TCP-SER-B2-D02  computed=0000 stored=0000  OK     0.0%   (empty bank)
TCP-SER-B3-D02  computed=99EE stored=99EE  OK     2.3%
TCP-SER-B0-B05  computed=D998 stored=D998  OK    75.0%
TCP-SER-B1-B05  computed=25F3 stored=25F3  OK    12.1%
TCP-SER-B2-B05  computed=471E stored=471E  OK     0.1%
TCP-SER-B3-B05  computed=42CE stored=42CE  OK     9.2%
```

A parity mistake on a `:BPUN` shows up here instantly — which makes this the fastest test
that a bulk copy went in cleanly. Note `TCP-SER-B2-D02` is **100% zero and correct**: the
bank is simply unused. A PASS on a bank with real content is strong; a PASS on an empty one
proves little.

**2. Check the parity of every text file you wrote**, against §2:

```python
d = fs.read_file('TCP-IP/TCP-START-D02:MODE', parity='none')
print(100.0*sum(1 for b in d if b & 0x80)/len(d))   # expect ~76%, not 0.0
```

**3. `--fsck` the pack** and compare free-page counts before and after.

**4. Read a file back and compare it to the source** byte for byte. For a pack-to-pack
copy this should be exact.

Do **not** verify a text file by `COPY-FILE`-ing it to the terminal: SINTRAN ends lines
with CR and no LF, so every line prints at column 0 and overwrites the last. The file is
usually fine even though the screen looks like garbage. Use `FILE-STATISTICS` for the byte
count, or run it.

---

## 8. Traps that have already cost time

- **`:` cannot appear in a Windows filename.** Extracted files use `.` instead —
  `TCP-START-D02:MODE` becomes `TCP-START-D02.MODE`. The `.xat` sidecar carries the real
  name and type back. Always drive a restore from the sidecar, never from the Windows
  filename.
- **Same name, different type, overwritten.** A pack legitimately carries
  `TCP-IP-LO-D02` as both `:MODE` and `:LIST`, and `SKP-C00` as `:DEFS`, `:IMPT` *and*
  `:INTL`. Older library versions resolved a file by **name only**, so writing `AAA:MODE`
  after `AAA:LIST` overwrote the LIST entry's data while keeping its type — the MODE file
  vanished and the LIST file held the wrong bytes. Fixed 2026-08-19 in ndfs-py and ndfs-ts;
  **ndfs-c never had the bug**. On an older checkout the symptom is: the copy reports N
  files written, the pack has fewer, and sizes belong to the wrong type.
- **An all-zero page is not a hole.** See §3b — state holes from the sidecar, never infer
  them from the data.
- **A file extracted from a damaged image looks fine and is not.** Holes read as zeros, so
  the size and header are right while the middle is NUL. Before trusting anything pulled
  off a recovered pack, hash it against every other copy of that pack you have and check
  the image with `img_sector_map.py`.
- **Do not grep an image for a filename.** Names live in object entries with a `0x27`
  terminator and the pack may be sparse or fragmented. A grep that finds nothing proves
  nothing. Open the filesystem and enumerate.
- **A "missing" file may be a second object block.** A user holds 256 files per object
  block; `--objblocks` shows whether more were granted.
- **PowerShell, not Bash.** `cd e:\path` does not change drive on its own — use full paths.

---

## 9. Sources

| | |
|---|---|
| Library / tool | **norskdata-ndfs** — https://github.com/HackerCorpLabs/norskdata-ndfs · releases: https://github.com/HackerCorpLabs/norskdata-ndfs/releases. ndfs-py 0.0.6 and ndtool 0.0.6 verified 2026-08-21 |
| Format spec | `docs/NDFS-FORMAT.md` in the norskdata-ndfs repo |
| Parity map in §2 | measured from the 2024 read of the **c3** Micropolis 1325 pack and the **Tingo** Micropolis 1325 pack, 2026-08-21 |
| Bank verifier | `verify_pioc_bank.py` (bank checksum layout documented in [README.md](README.md) §7) |
| Command syntax | `ND-60.128.5 EN SINTRAN III Reference Manual` |

**Parent:** [README.md](README.md) · **Related:** [../../OS/](../../OS/README.md)
