# Extracting SINTRAN resident code and data

Full path: `tools/sintran-segment-carver/EXTRACTING-RESIDENT-CODE.md`

The single guide for the memory-**resident** parts of SINTRAN that the segment
carver does **not** produce: the resident common code, the resident common data
(the top page with the device registers and login state), the RT-loader area, and
the resident level-14 monitor-call dispatch. For the ordinary paged system segments
see the companion [EXTRACTING-SEGMENTS.md](EXTRACTING-SEGMENTS.md).

---

## 1. Why the carver skips this code

`carve.py` only produces segments that have a distinct `(SYSTEM)SEGFIL0:DATA`
image (`FLAG` SEGFIL# = 0, `SEGLE > 0`, `MADR > 0`). Two things fall outside that:

- **`MADR == 0` segments** (S3IMAGE / S3MPIT / S3RPIT, etc.) — memory-resident,
  no distinct SEGFIL0 image. Carving them from offset 0 yields duplicate/bogus
  content, so they are deliberately skipped.
- **Resident common code / data** — mapped into the top of *every* segment's
  virtual window at runtime (via the Page Index Tables), not stored per-segment.
  It lives in separate NDFS files, described below.

Consequence: routines and variables at fixed resident addresses (e.g. the password
fold, `OPWCH`, `PASST`, the GOTAB dispatch) are **not** in any per-segment `.bin`,
even after the full-length carve fix. They must be pulled from the resident images
or captured live.

---

## 2. NDFS layout of the resident files

From `ndtool --stat -v` (1 block/page = **2048 bytes = 1024 words**):

| NDFS file | Blocks | Pages | Contents |
|-----------|--------|-------|----------|
| `(SYSTEM)SINTRAN:DATA` | 1..63 | 63 | resident common code ("Common Code Restart/Start") |
| `(SYSTEM)MACM-AREA:DATA` | 64..127 | 64 | RT-loader area |
| `(SYSTEM)SEGFIL0:DATA` | 128.. | 10000 | the paged system segments (what the carver reads) |

**`ndtool -x` extracts 0 bytes for the first two** (their directory byte-length is
0), so they must be pulled by **raw block range** instead:

```bash
# 2048 bytes/page; SINTRAN:DATA = blocks 1..63, MACM-AREA:DATA = blocks 64..127.
python3 -c "d=open('SMD0.IMG','rb').read(); open('SINTRAN-DATA_commoncode.bin','wb').write(d[1*2048:64*2048])"
python3 -c "d=open('SMD0.IMG','rb').read(); open('MACM-AREA-DATA_rtloader.bin','wb').write(d[64*2048:128*2048])"
```

Output is big-endian (native ND-100); load in Ghidra as-is, byte-swap only for
`nd100-dis` (same rule as the segment carver).

---

## 3. Disk-layout cross-reference (release manual section 8.1)

Source: `SINTRAN/Release-Documentation/ND-860230-6-EN Sintran III - Release
Information - L-Version.md`, section **8.1 SYSTEM LAYOUT ON DISK** (all octal):

| File | Contents | Disk addr | Size | Seg addr |
|------|----------|-----------|------|----------|
| SINTRAN:DATA | Common Code Restart/Start | 1B | 77B | 0B |
| | Error Messages | 100B | 20B | 30000B |
| MACM-AREA:DATA | RT-Loader | 137B | 41B | 30000B |
| SEGFILE:DATA | Common Code Restart/Start | 200B | 77B | 30000B |
| | **Resident Data** | **300B** | 55B | 40000B |
| | System Segment | 355B | 3B | 144000B |

Section **8.2 PAGE INDEX TABLE LAYOUT**: every user PIT (1-6) carries `Common code`
at pages 2..13; PIT 7 (DPIT) carries `Resident common data` at pages 2..57. The top
resident page (virtual 177000-177777) is resident common data mapped into the top
of every map — which is why its symbols appear "inside" every segment's window.

---

## 4. Extracted artifacts (K05 / L07 / M06)

Extracted for all three versions into `versions/<VER>/resident/` (big-endian,
load as-is in Ghidra; the base is the seg-addr below in hex):

| File | Blocks | Size | Seg addr | Ghidra base (hex) | Contents |
|------|--------|------|----------|-------------------|----------|
| `SINTRAN-DATA_commoncode.bin` | 1..63 | 63 pages / 129024 B (64512 words) | 0B | `0x0000` | resident "Common Code Restart/Start" |
| `MACM-AREA-DATA_rtloader.bin` | 64..127 | 64 pages / 131072 B (65536 words) | 30000B | `0x3000` | RT-loader area |

Verified real ND-100 code — the common-code image disassembles to ~10.2-10.6 k
control-flow instructions per version (IOF/POF/reset vectors at the top). Note: the
144000+ region of the common-code image is zero — segment-specific code such as
S3ISYS `PWLOG` is **not** mirrored here; it lives in the segments.

(M's disk directory has no `SINTRAN:DATA` entry by name, but blocks 1..63 physically
hold the same resident common code — `MACM-AREA:DATA` and `SEGFIL0:DATA` sit at the
standard blocks 64/128, and the bytes match the K05 image.)

---

## 5. Resident monitor-call dispatch (level 14 / GOTAB)

The resident level-14 MON dispatch is captured **inside a carved SEGFIL0 segment**,
not in the resident images above: it is in `segments/116-S3SERWD.bin`, mapped
resident at base `0o3000` (0x600). Key entries (identified by GOTAB content
signature, since S3MPIT / S3RPIT / S3IMAGE do not carve):
- `ENT14 = 072167`, `GOTAB = 071233` (256-word jump table), dispatch `JMP ,X` at
  `072260`.

See `SINTRAN/OS/23-MON-CALL-DISPATCH-DEVELOPER-GUIDE.md`
and `ghidra-tasks/TASK-03-mon-dispatch-handlers.md` for the per-handler walk.

---

## 6. Resident DATA cells are runtime-only (verified)

The resident common-data page (virtual 177600-177777) holds variables, not code:
device registers (`2TREG/2DREG/2XREG`), file-system state (`WCOUN 177651`,
`FSTA1 177652`, `FDRIV 177657`), and the password work cells `OPWCH = 177650`
("Old PassWord CHange") and `PASST = 177606` ("PASsword Temp").

**Verified:** in the full-length carves these cells read `000000`, and the whole
177600-177777 page is byte-identical (all zero) across S3CP and S3FS — i.e. it is
the resident-data page baked as zeros into every segment image and populated only
at runtime. So full-length carving puts the fold's *addresses* in range but **not**
its runtime *values*. To observe the folded password word or the runtime-resolved
fold linkage, capture live via DAP — see
[re/HANDOFF-fold-live-capture.md](versions/L-VSX-500/re/HANDOFF-fold-live-capture.md).

Resident login/user **code** candidates: `USERD = 176254`, `LOGSE = 176146`,
`BCLOG = 176112`, `D8LOG = 170271` / `8LOGL = 170272`. NOTE these sit ABOVE the
common-code image top (0o175777); they are in the top resident data page, not in
`SINTRAN-DATA_commoncode.bin`.

---

## 7. Resolving a resident address: the PIT-overlay model

A resident virtual address is NOT served by one flat image. The resident common
code is base 0, but it has **gaps** (uninitialised / BSS pages), and at runtime the
Page Index Tables (PITs) **overlay other segments into those gaps**. So to read the
code at a resident address you must find *which* image actually backs that page.

### 7.1 The common-code image and its gaps (base 0)

`SINTRAN-DATA_commoncode.bin` (SINTRAN:DATA, the resident common code = the live
`S3IMAGE`/`S3SAVE` segments, which have `madr==0` and so are not carved from
SEGFIL0) loads at **base 0x0**. Per-page non-zero scan (page = 1024 words = 0o2000):

| Virtual (octal) | Pages | Content |
|-----------------|-------|---------|
| 0 .. 0o24000    | 0-10  | dense common code (reset/restart vectors at 0) |
| **0o26000 .. 0o30000** | **11** | **ZERO gap** (overlaid at runtime - see 7.2) |
| 0o30000 .. 0o104000 | 12-33 | dense common code |
| 0o104000 .. 0o170000 | 34-59 | ZERO (BSS / overlaid) |
| 0o170000 .. 0o172000 | 60 | sparse |

So an address like `0o27032` reads as zero in this image - it is in the page-11 gap.

### 7.2 The overlay segments: Common and Extended Common

The segments whose live copy has `madr==0` (memory-resident, no SEGFIL0 image) are
recovered from their carved **Image / Save** copies:

| Live (skipped) | Carved image / save | Ghidra base | nd100-dis -b | Role |
|----------------|---------------------|-------------|--------------|------|
| `S3IMAGE`/`S3SAVE` (common code) | SINTRAN:DATA raw block -> `SINTRAN-DATA_commoncode.bin` | **0x0** | 0 | resident common code (section 4) |
| `S3ECOM` (extended common) | `061-S3IECOM.bin` (madr 1067), `060-S3SECOM.bin` (madr 113) | **0x2C00** | 11264 | overlays 0o26000-0o31777 (pages 11-12) |

Extended Common is 2 pages, loads at octal **26000 = 0x2C00**, symbol file
`SYMBOL-1-LIST`. Its Image and Save copies carve normally from SEGFIL0 (see
EXTRACTING-SEGMENTS.md section 6) and are byte-identical in the code region.

### 7.3 Method - resolve any resident address X

1. Is X in the common-code image and non-zero? -> read it in
   `SINTRAN-DATA_commoncode.bin` at base 0x0.
2. If X is a zero gap there, find the carved segment whose
   `load_address .. load_address + segle*1024` (from `manifest.json`) covers X and
   has non-zero content at X - that segment is the PIT overlay for that page. For
   0o26000-0o31777 that is Extended Common (`061-S3IECOM.bin`).
3. Some resident code also lives inside ordinary carved segments (e.g. the level-14
   MON dispatch is in `116-S3SERWD.bin`, section 5).

### 7.4 Load into Ghidra / disassemble (for other tools and LLMs)

Extended Common (worked example):
- **Ghidra**: import `segments/061-S3IECOM.bin`, Raw Binary, processor
  **ND-100 big-endian 16-bit (word-addressed)**, **Base Address `0x2C00`** (hex),
  do NOT byte-swap. Disassemble from `0x2C00`; apply `SYMBOL-1-LIST` labels.
- **nd100-dis** (reads little-endian only): byte-swap first, then base octal 26000
  (pass decimal `11264` to the `-b` flag):
  ```bash
  python3 -c "import sys;d=bytearray(open(sys.argv[1],'rb').read());d[0::2],d[1::2]=d[1::2],d[0::2];open(sys.argv[2],'wb').write(d)" \
    segments/061-S3IECOM.bin /tmp/s3iecom.le.bin
  nd100-dis -a -S -o -b 11264 /tmp/s3iecom.le.bin
  ```
- Common code image: same rules at **base 0x0** (nd100-dis `-b 0`).

### 7.5 Worked example - the login char-read at 0o27032

The S3CP `LOGIN` password loop reads each char via `JPL I 23` whose pointer word
(060777) holds **0o27032**. That address is the page-11 gap in the common image, so
it is NOT there - it is in **Extended Common** (`061-S3IECOM.bin` at 0x2C00). Read
there, 0o27032 is a low-level device-interaction routine: `TRA PID` / `MST PID`
(inter-level register transfer), `IRW` (inter-register write across PIL levels), and
`IOF`/`ION` - i.e. it brokers a character read from the terminal driver running at
another PIL level. The character's uppercasing (and any 7-bit masking) therefore
happens one layer deeper still, in the terminal driver at the device level, not in
this wrapper.

### 7.6 Disambiguating the swappable window (0o104000-0o170000): multiple candidates

Section 7.3 step 2 ("find the segment covering X with non-zero content") is
**ambiguous** in the high region `0o104000-0o170000`: this is the swappable-segment
window, so MANY carved segments' virtual ranges cover the same address and each has
*some* non-zero bytes there. A load-address scan for e.g. `DVIO=0o141027` returns
S3FS, S3DMAC, S3RFAC, S3SM5, S3IMPIT, ... all at once. "Has non-zero bytes" does NOT
identify the right overlay - you must add a **semantic-coherence** test.

**Method - pick the true overlay by resident-call-target density, then byte anchors:**

1. **Score candidates by call-target hits.** The resident driver you're resolving
   calls a known set of resident routines/fields. Scan each candidate's bytes across
   the routine's window for the *pointer words / symbol addresses* of those targets
   (from the version's SYMBOL files). The correct segment shows a decisive spread.
   Worked measurement for the ND-500 level-12 driver over `0o136000-0o144000`,
   counting hits on `5MBBANK, NXTMSG, EMONICO, XACTRDY, WN5STATUS, ...`:
   - **S3IMPIT / S3SMPIT: 140 hits** (5MBBANK x29, NXTMSG x24, XACTRDY x17 ...)
   - S3SM5 / S3SSM5: 8 (plus 73 invalid ND-100 opcodes + IOX - it is 32-bit ND-500
     code, wrong instruction set)
   - S3FS / S3SFS: 5 (file-system code); S3DMAC: 0; S3RFAC: doesn't span the range.
2. **Confirm with byte anchors** that cannot coincide: (a) a routine that ends by
   indirecting through inline pointer words - e.g. `SWMC` ends `JPL I` / `JMP I`
   through words holding `5ACTSWAPPER` and `NXTMSG`; (b) a known DATA array - e.g.
   the XMSG write-back mask `XMRETMASK` decodes byte-exactly as
   `16,0,4,0,34,0,20,20,0,14,2,0,0,74,34,4,...` immediately below `A5XMSG`. A match
   on real data/pointer values is proof; a coincidental opcode run is not.
3. **Expect an address OFFSET between the NPL source and the running symbols.** The
   NPL is usually a different revision; here the L07 symbol addresses run a uniform
   **+0o200** above the NPL addresses. Match *structure/behaviour*, never raw bytes,
   against the NPL.

### 7.7 Result - the ND-500 level-12 MON handlers live in S3MPIT

The ND-500 driver (level-12) MON handlers are memory-resident (`madr==0`) and are
recovered byte-for-byte from the **S3MPIT** Image/Save copies (identical in the code
region):

| Carved image / save | Load (octal) | nd100-dis `-b` | Ghidra base |
|---------------------|--------------|----------------|-------------|
| `026-S3IMPIT.bin` (Image) / `017-S3SMPIT.bin` (Save) | **0o32000** | `13312` | `0x3400` |

Disassemble: byte-swap the `.bin`, then
`nd100-dis -a -S -o -b 13312 <le.bin>` (or Ghidra Raw Binary, ND-100 big-endian
16-bit word-addressed, base `0x3400`, no byte-swap). L07 handler addresses (all
octal, VERIFIED as coherent code with the NPL-shape prologue):

| Handler | Addr | Opening shape |
|---------|------|---------------|
| STAPROC (500B) | 0o140356 | `LDT 5MBBANK / AAX NPROC / LDDTX`, legal-proc checks |
| NSTOPROC (501B) | 0o140511 | `CALL SLOCK / LDT 5MBBANK / AAX 5MSFL / LDATX`, REP-bit + WN5STATUS |
| DVIO (511B) | 0o141027 | `CALL 5GTDF / GO NORMMC`, TODF/DNOBY, EC174 |
| GERRC (505B) | 0o141633 | `LDT 5MBBANK / LDATX 5RECE`, reads trap ERREG |
| 5SIBMO (506B) | 0o141716 | `LDT 5MBBANK / AAX SIBNO / LDDTX`, SIBAS check -> EMONICO |
| SWMC (510B) | 0o142153 | `SHZ 10 / LDT 5MBBANK / AAX TRAPN / LDATX / ... / 5ACTSWAPPER / NXTMSG` |
| A5XMSG/B5XMSG (512/513B) | 0o142253 | `X=:B / LDT 5MBBANK / AAX N5XFU / LDATX / AND X5MASK`; XMRETMASK array follows |
| 5MTRANS (515B) | 0o143445 | `X=:CMSGA / A:=B / LDT 5MBBANK / AAX 5MNWA / LDDTX` |

Full annotated L disassembly:
`versions/L-VSX-500/re/ND500-HANDLERS-OVERLAY.md`. This resolves the handlers from
disk (the emulator live-memory dump described elsewhere is the fallback, not needed).

---

## 8. Live-DAP capture of the running dispatch table (definitive, 2026-07-10)

The OFFLINE carve route (§7) is preferred, but for anything the carve can't hold
(runtime-populated cells, overlay pages not mapped in a carve) the DEFINITIVE source
is the running system read over DAP. Booting the L image under nd100x and reading
memory proved the following — treat these as ground truth over any carve for the
dispatch region:

### 8.1 `MGOTA` / GOTAB is at virtual `0x729b` (octal 071233) — symbol-confirmed

`MGOTA` (MON GO TAble) is a named symbol at `0x729b` in
`re/116-S3SERWD.ghidra-symbols.txt:3719` and `re/003-S3CP.ghidra-symbols.txt`.
`ENT14 = 0x7477` (octal 072167). Each `GOTAB[MON#]` is one word = the handler
address for that monitor call.

> **Correction to §5:** §5 says the dispatch table is "captured inside carved
> `116-S3SERWD.bin`." That is only true for the *addresses/symbols*. The actual
> `071233` REGION in the carved `.bin` is runtime-populated (reads as data/zero on
> disk, same as the §6 resident-data page). The LIVE values below come from DAP, not
> from `116-S3SERWD.bin`. Do not disassemble `071233` out of the carve — read it
> live.

### 8.2 A `GOTAB[n] == 000000` entry means "second-level fall-through", NOT illegal

Verified live: `GOTAB[14B] = 000000` yet MON 14 = OUTBT is a real, heavily-used
call. A zero slot means MON n is dispatched by the level-14 fall-through
(`MFELL` -> `CALLPROC`, a second-level table in an as-yet-uncarved overlay), not
that MON n is unimplemented. **Never record a zero-GOTAB call as "illegal."**
(This corrected a near-miss on MON 42, `GOTAB[42B]=0`.)

### 8.3 Non-zero handlers point into the PIT-overlay window and read zero at idle

Verified live GOTAB entries: `GOTAB[15B]=120501`, `GOTAB[51B]=121147`,
`GOTAB[1B]=120303`, `GOTAB[13B]=120454`. These targets sit in the swappable
overlay window `0o104000-0o170000`. Reading them at an arbitrary idle pause returns
**all-zero** — the overlay holding the handler body is not mapped in the idle
context. The POINTER is real; the CODE is only present when that MON runs. To
disassemble the body, read it WHILE MAPPED:
  1. translate virtual page -> physical frame via the page table (`ispace:`/`dspace:`
     reads) and read `phys:<frame>`; or
  2. break inside level-14 dispatch while that MON executes (correct overlay swapped
     in), then read the virtual address.
Do NOT just set an instruction breakpoint at the overlay virtual address during boot
— the page isn't the handler page yet, and it hangs.

### 8.4 How to run the capture (raw DAP, one connection)

Operational rules live in the debug skill (`~/.claude/skills/nd100-debug/SKILL.md`,
"Raw-DAP live-memory session"). Summary: start `nd100x --debugger -p <port>
--boot=smd` in the BACKGROUND; do the whole job in ONE connection
`initialize -> attach{pid:1} -> configurationDone -> continue -> sleep ~16s ->
pause -> readMemory`; never reconnect a second client (hangs); use a private port
(`-p 4712`) if another session holds 4711; byte-swap DAP reads before `nd100-dis`
(it is little-endian only), never for Ghidra.

---

## 9. The MON dispatch is THREE layers (2026-07-11) - static carves cannot close it

Verified by 30+15 agent runs and a live trace. The ND-100 MON dispatch has three layers,
and per-call carving only captures the two ends:

```
MON N -> GOTAB[N]                              (level-14; commoncode.bin 071233B+MON#; byte + LIVE proven)
      -> resident CALLPROC / segment-switch    (UNCARVED overlay - the gap)
      -> worker body (RDISK/OSIZE/DEBUGGER/...) (real SINTRAN L bytes, byte-proven)
```

- `GOTAB[N]` is either an **F16xx stub** address (in `025-S3IRPIT.bin`) or **000000**
  (fall-through). Both then pass through the resident `CALLPROC` second-level dispatch,
  which is memory-resident and NOT in any carve.
- Therefore the `MON N -> specific worker` LINK is **not statically provable** for ND-100
  calls. Verdicts recorded per call in `versions/L-VSX-500/re/mon-analysis/*/DISPATCH.md`:
  0 fully proven, 7 partial, **6 misattributed** (worker unreachable from `GOTAB[N]` by any
  resolvable path: 5,45,67,304,313,327), 2 unresolved (13,14). The carved worker BODIES are
  still real bytes and correct-family by symbol; they just sit downstream of the uncarved bridge.
- **Two conflicting symbol tables** (different revisions), do not mix: `SYMBOL-1-LIST` /
  `FILSYS-SYMBOLS` give the worker names (`RDISK=102021`, `OSIZE=044231`, `CIBUF=044120`);
  `SYMBOL-2-LIST` gives the `F16xx` stub names. Always report which table an address came from.
- **Closing the gap needs a LIVE trace** - the static bridge is unfollowable. Use the
  full-speed native breakpoint: `nd100x --breakpoint=<worker addr> --ring-dump=200 --boot=smd`
  (foreground, to a file); if the OS halts on the worker, the ring dump shows the CALLPROC
  path that reached it. Details in `~/.claude/skills/nd100-debug/SKILL.md`.
- Tools: `scripts/prove-mon.py` (derive dispatch from ground truth; ND-500 calls are NOT
  GOTAB-dispatched and are guarded), `scripts/validate-mon-carves.py` (closure integrity).
