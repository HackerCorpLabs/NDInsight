# Initial Commands and Automatic Startup in SINTRAN III

**Scope:** SINTRAN III VSX / VSX-500, ND-100/ND-110, versions K03 / L07 / M06
**Purpose:** Explain what SINTRAN runs automatically at start-up, where those
commands live (MEMORY vs DISK), and exactly how to read/find them.

Notation: memory sizes in **WORDS** (1 word = 2 bytes). Octal written `NNNNN₈`.
Every claim is tagged **VERIFIED** (from a manual, the symbol tables, the NPL
source, or the actual system disk) or **UNCERTAIN**.

Cross-references (relative links):
- [22-READING-RT-AND-SEGMENT-TABLES-FROM-MEMORY.md](22-READING-RT-AND-SEGMENT-TABLES-FROM-MEMORY.md) — live memory-read conventions
- [19-MEMORY-MAP-REFERENCE.md](19-MEMORY-MAP-REFERENCE.md) — memory map
- [../../Reference-Manuals/SINTRAN-COMMANDS-REFERENCE.md](../../Reference-Manuals/SINTRAN-COMMANDS-REFERENCE.md) — command reference

---

## 0. Executive summary

There are **two distinct startup mechanisms**, and they work together:

| Mechanism | Lives in | Set/edited by | Read by | Confidence |
|---|---|---|---|---|
| **Initial-command buffer** (a small list of commands, first must be `@ENTER-DIRECTORY`) | **MEMORY** — resident/segment-mapped buffer `INICO`; persisted inside the SINTRAN memory image on `SEGFIL0` | `@INITIAL-COMMAND`, `@NEXT-INITIAL-COMMAND` | `@LIST-INITIAL-COMMANDS` | VERIFIED |
| **Mode / batch start-up files** `HENT-MODE:MODE` (cold start) and `LOAD-MODE:MODE` / `LOAD-MODE:BATC` (warm start) | **DISK** — ordinary 7-bit command files on user `SYSTEM` | any editor (PED etc.) | `@LIST-FILES`, extract & read the file | VERIFIED |

The in-memory initial-command buffer is tiny and runs **first** at restart. Its
job is to enter the main directory and then hand off to the disk mode/batch
files, which do the real work (start XMSG/COSMOS, TAD, spooling, set the system
available, etc.).

**The practical recipe:** to see what runs automatically, look in **both** places —
`@LIST-INITIAL-COMMANDS` (the memory buffer) and the `HENT-MODE`/`LOAD-MODE` files
on disk (§4).

---

## 1. Terminology — what SINTRAN calls it

**VERIFIED.** SINTRAN III uses the term *initial commands* for the in-memory
buffer, and *mode files* / *batch files* for the disk scripts.

Commands (all restricted to user `SYSTEM`):

| Command | Function | Source |
|---|---|---|
| `@INITIAL-COMMAND <command string>` | Define the **first** initial command (must be `@ENTER-DIRECTORY`). Stored as the first entry of the initial-command buffer. | ND-60.128.5 Reference Manual, "@INITIAL-COMMAND" |
| `@NEXT-INITIAL-COMMAND <command string>` | Append a further command to the buffer. | ND-60.128.5, "@NEXT-INITIAL-COMMAND" |
| `@LIST-INITIAL-COMMANDS <output file>` | List the buffer contents (default output = terminal). | ND-60.128.5, "@LIST-INITIAL-COMMANDS" |

> "Specify the first command to be executed at next restart from memory image,
> @RESTART-SYSTEM, or pressing MASTER CLEAR and LOAD buttons. The command must be
> @ENTER-DIRECTORY. It is stored as the first command in the initial command
> buffer."
> — `Reference-Manuals/ND-60.128.5 EN SINTRAN III Reference Manual.md:5340`

> "When @INITIAL-COMMAND has been issued, @RTENTER is automatically executed at
> subsequent restarts."
> — `Reference-Manuals/ND-60.128.5 EN SINTRAN III Reference Manual.md:5355`

So after `@INITIAL-COMMAND` is set once, `@RTENTER` no longer has to be issued
by hand at every restart (also stated in
`Reference-Manuals/ND-60.050.06 SINTRAN III Users Guide.md:4799`).

`HENT` is the Norwegian for "fetch/retrieve" — `HENT-MODE` is the "fetch/cold-start"
mode file, `LOAD-MODE` the warm-start (load-from-image) file. **VERIFIED** by the
consistent §1.6 "CHANGES TO THE HENT-MODE AND LOAD-MODE FILES" sections in the
K/L/N release notes (e.g.
`SINTRAN/Release-Documentation/ND-860230-6-EN Sintran III - Release Information - L-Version.md:499`).

---

## 2. The MEMORY structure — the initial-command buffer (`INICO`)

**VERIFIED.** The initial-command buffer is a named location in the SINTRAN
**file-system** symbol space (`FILSYS-SYMBOLS`), symbol **`INICO`**:

| Version | `INICO` address | Source |
|---|---|---|
| L07 | `134620₈` | `SINTRAN/NPL-SOURCE/SYMBOLS/L07/FILSYS-SYMBOLS.SYMB.TXT:3525` |
| M06 | `137255₈` | `SINTRAN/NPL-SOURCE/SYMBOLS/M06/FILSYS-SYMBOLS.SYMB.TXT:3634` |

Neighboring symbols confirm this is command-line / command-buffer machinery:

- L07: `RCPOO=134516`, **`INICO=134620`**, `CLCOM=134657`, `ECLCO=134670`, `WNKER=134721`
- M06: `CLHOM=137200`, `CLSND=137227`, **`INICO=137255`** (`CL*` = command-line handling)

(`FILSYS-SYMBOLS.SYMB.TXT`, sorted by address.)

### 2.1 Is it resident or segment-mapped?

**VERIFIED (address range) / UNCERTAIN (exact segment):** `INICO` sits at
`134620₈`/`137255₈`, i.e. inside the **`26000₈–177777₈` segment-mapped region**,
not resident low core. In the VSX system-segment table this range is covered by
the file-system and command segments:

| Seg | Name | Range | Description |
|---|---|---|---|
| 6 | `S3FS` | `26000–177777` | File system segment (running) |
| 3 | `S3COM` | `30000–177777` | Command segment (running) |
| 12 | `S3FSV` | `26000–177777` | **Initial** file system segment (save image) |
| 13 | `S3OPCSV` | `26000–177777` | **Initial command segment** (save image) |

— `SINTRAN/Release-Documentation/ND-60230-5-EN SINTRAN III - Release Information - K-version.md:7882-7887`

Because `INICO` is a `FILSYS` symbol, the live buffer is mapped in the
**file-system / command segment** (`S3FS`/`S3COM`); its restart-surviving copy is
held in the **initial** save segments (`S3FSV` / `S3OPCSV`, "Initial command
segment"), which are written into the SINTRAN memory image on `SEGFIL0` at
`@RESTART-SYSTEM` / cold start. That is *why* the initial commands survive a
restart even though they are a memory structure: the memory image is checkpointed
to disk. This matches the manual: the commands run "at next restart **from memory
image**". **VERIFIED** (mechanism), **UNCERTAIN** (whether `INICO`'s home is
segment 6 vs 3 precisely — both span the address).

### 2.2 CORRECTION (2026-07-10): the command text is at `INIBU`, not `INICO`

**`INICO` is not the command-text buffer.** It is a file-system symbol; the assumption
that it held the buffer was an inference, now disproved. The initial-command *text*
lives at the kernel symbol **`INIBU`** ("initial buffer") in the **command segment**:

| Version | `INIBU` |
|---|---|
| K03 | `067172`<sub>8</sub> |
| L07 | `074123`<sub>8</sub> |
| M06 | `102327`<sub>8</sub> |

Verified byte-exact against a live L07 system and two real SMD packs. The buffer can be
read (and written) **offline from a disk image**, with no boot required: it sits inside
the command segment's image in `(SYSTEM)SEGFIL0:DATA`, and its exact byte offset is derived
from `INIBU` plus the segment table read off the same image.

**Full specification, encoding, algorithm and golden test vectors:**
[26-INITIAL-COMMAND-BUFFER-ON-DISK.md](26-INITIAL-COMMAND-BUFFER-ON-DISK.md)

On a *running* system, `@LIST-INITIAL-COMMANDS` (§4.1) remains the easiest way to see them.
The NPL command handler that fills `INICO` is **not present in the NPL source
subset in this repo** (searching `SINTRAN/NPL-SOURCE/NPL/` for `INICO` /
`INITIAL` finds only unrelated matches); the symbol is known only from
`FILSYS-SYMBOLS`. **VERIFIED** (absence in repo).

---

## 3. The DISK files — mode / batch start-up scripts

**VERIFIED.** The bulk of automatic startup is ordinary SINTRAN command files on
user `SYSTEM`:

- **`HENT-MODE:MODE`** — run after a **cold start** (`)HENT`, MACL+LOAD). Does
  one-time setup: `@RTENTER`, `DUMP-REENTRANT`, load ND-500 monitor, initialise
  XMSG, define memory config, etc. Referenced throughout the release notes'
  §1.6 and installation sections, e.g.
  `SINTRAN/Release-Documentation/ND-860230-6-EN ... L-Version.md:503,781`.
- **`LOAD-MODE:MODE`** / **`LOAD-MODE:BATC`** — run after a **warm start**
  (restart from image). Re-applies things that do not survive a warm start
  (start batch processors, XMSG/COSMOS spooling, TAD administrator, set the
  system available). See §1.6 of the same release notes.

### 3.1 Actual `LOAD-MODE:MODE` from the system disk

**VERIFIED** by extraction from the live SINTRAN III VSX/500-L disk image
(`~/repos/nd100x/SMD0.IMG`), file **`(SYSTEM)LOAD-MODE:MODE`** (entry `[0037]`,
410 bytes, dated 1996-04-01). Extracted with
`ndtool -x -F 'SYSTEM/LOAD-MODE:MODE'` and decoded from SINTRAN 7-bit
(high-bit-set) text:

```
@ENTER SYSTEM,xxxxxx,,3200,,,

@CC Start all batch processors
@BATCH
@BATCH

@CC Start XMSG and COSMOS Spooling
@MODE XMSG-START:MODE,,,

@CC Start the TAD ADMinistrator
@START-TADADM

@CC Set the system available
@SET-AVAILABLE

@MAIL
@DIRECT-BROADCAST
SYSTEM IS AVAILABLE
@EXIT

@CC List routing to initialize all systems:
@(UTIL)XMSG-COMMAND
LIST-ROUTING,,,,,,
EXIT
@CC END OF FILE
```

This is the concrete "automatic startup command list" for this machine: it
starts two batch processors, runs the `XMSG-START:MODE` sub-mode file (which
brings up XMSG and COSMOS spooling), starts the TAD administrator, marks the
system available, and broadcasts availability.

Note there is **no plain-text file that stores the initial-command buffer**; the
buffer's persisted form lives *inside* the binary `(SYSTEM)SEGFIL0:DATA` memory
image (entry `[0002]` on the same disk), not as a readable file. The readable
startup scripts are the `HENT-MODE` / `LOAD-MODE` mode files above.

---

## 4. HOW-TO — see what is set up to run automatically

### 4.1 On a running system (the buffer)
From user `SYSTEM`:
```
@LIST-INITIAL-COMMANDS,,
```
Example real output (from ND-60.128.5:5814):
```
ENTER-DIRECTORY,,DISC-2-75MB,0
CONN SYS-OUT-1 105 R
CLOSE 105
BATCH
APPEND-BATCH 1 LOAD-MODE SYS-OUT-1
```
Here the buffer enters the directory and then chains into the disk `LOAD-MODE`
file via `APPEND-BATCH` — showing the memory→disk hand-off directly.

### 4.2 On a running system (the mode files)
```
@LIST-FILES HENT-MODE:MODE,
@LIST-FILES LOAD-MODE,
@PLACE-BINARY ...   (or just open in PED / TYPE the file)
```

### 4.3 Offline, from the disk image (no boot required)
List and extract with the NDFS tool:
```
# list SYSTEM user files
~/repos/norskdata-ndfs/ndfs-c/build/ndtool -t -u SYSTEM ~/repos/nd100x/SMD0.IMG

# extract the warm-start mode file
~/repos/norskdata-ndfs/ndfs-c/build/ndtool -x -F 'SYSTEM/LOAD-MODE:MODE' -o /tmp/out ~/repos/nd100x/SMD0.IMG
# (also try HENT-MODE:MODE if present on the pack)
```
The extracted file is **7-bit with the high bit set** — mask each byte with
`0x7F` to read it as ASCII (see §3.1).

### 4.4 The initial-command buffer from raw memory (rarely needed)
`INICO` = `134620₈` (L07) / `137255₈` (M06), in the file-system/command segment.
Prefer §4.1; only decode `INICO` directly if `@LIST-INITIAL-COMMANDS` is
unavailable and the file-system segment is mapped.

---

## 5. Sources

- `Reference-Manuals/ND-60.128.5 EN SINTRAN III Reference Manual.md` — `@INITIAL-COMMAND` (5337-5360), `@LIST-INITIAL-COMMANDS` (5794-5823), `@NEXT-INITIAL-COMMAND` (6837+), "initial command buffer"
- `Reference-Manuals/ND-60.050.06 SINTRAN III Users Guide.md:4799,5436,5448-5454` — `@INITIAL-COMMAND` and `@RTENTER` behavior
- `Reference-Manuals/SINTRAN-COMMANDS-REFERENCE.md:3212,3568` — INITIAL-COMMAND / LIST-INITIAL-COMMANDS
- `SINTRAN/NPL-SOURCE/SYMBOLS/L07/FILSYS-SYMBOLS.SYMB.TXT:3525` — `INICO=134620`
- `SINTRAN/NPL-SOURCE/SYMBOLS/M06/FILSYS-SYMBOLS.SYMB.TXT:3634` — `INICO=137255`
- `SINTRAN/Release-Documentation/ND-60230-5-EN ... K-version.md:7882-7887` — system-segment table incl. `S3OPCSV` "Initial command segment"
- `SINTRAN/Release-Documentation/ND-860230-6-EN ... L-Version.md:499-513,781` — §1.6 HENT-MODE / LOAD-MODE, cold vs warm
- System disk `~/repos/nd100x/SMD0.IMG`, `(SYSTEM)LOAD-MODE:MODE` extracted via `ndtool` (§3.1)

---

**Last updated:** 2026-07-09
**Status:** VERIFIED where tagged; two UNCERTAIN points noted (exact home segment of `INICO`; NPL handler not in repo subset)
