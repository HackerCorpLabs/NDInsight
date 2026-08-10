# NC (Norsk Data C, A06) — Usage Specification and MON Call Contract

**Full path of this document:** `/mnt/d/ND/500/FraTor/nc/nc-a06-usage-and-mon-contract.md`
**Binary:** `/mnt/d/ND/500/FraTor/nc/nc-a06.dom` — "Norsk Data C - Version: A06 - 1989-01-10"

Date: 2026-07-10

## Purpose

NC does not currently reach code generation under emulation — every
code-producing phase prints `no rewrite` / ` terminated` and exits. This
document exists so that NC can be **driven correctly**, and so that any
remaining misbehaviour can be attributed to the monitor-call emulation rather
than to the way we invoke the compiler.

It is written as a contract. Section 3 is how to drive NC. Section 5 is the
exact set of 34 monitor calls NC issues, extracted from the binary, with the
argument shapes NC actually passes. Section 6 lists the specific places where a
wrong MON implementation would produce exactly the symptom we see.

Companion documents:

- `/mnt/d/ND/500/nd-linker/nd500-c-compile-and-link.md` — the full C build pipeline
- `/mnt/d/ND/500/nd-linker/linker-b01.dom.moncalls.md` — how ND-500 MON calls work
- `/mnt/d/ND/500/FraTor/nc/NC-INTERFACE.md` — the original probe results
- `/mnt/d/ND/500/FraTor/nc/nc-a06_analysis.md` — deep binary analysis

---

## 1. Binary facts

| Property | Value |
|---|---|
| Format | ND-500 Domain (DOM), SINTRAN III root domain |
| Segments used | 1 (segment 1) |
| Program part | `0x2ECBE` bytes, virtual base `0x08000000` |
| Data part | `0x1E4F1` bytes |
| Entry point | `0x08000004` |
| Linker version | v97.251 |
| Instructions (linear sweep) | 44,450 |
| MON call sites | **50** |
| Distinct MON numbers | **34** |
| MON instruction used | `CALL` (opcode `0xC3`) only — **no `CALLG`** |

That last row matters: because NC issues every monitor call through `CALL` and
never `CALLG`, `nd500-dis` annotates all 50 of them correctly. (The ND LINKER,
by contrast, uses `CALLG` for 27 of its 95 sites, which `nd500-dis` does not yet
annotate.)

The MON wrapper block sits at `0x0802DB00 .. 0x0802E207`, one small
`ents` / marshal / `call $0xF8xxxxxx` / `ret` routine per call, plus a few
scattered elsewhere (`0x08029027`, `0x0802907E`, `0x080290A8`, `0x0802CCF7`,
`0x0802D171`, `0x0802D1A5`, `0x0802D1D8`, `0x0802D447`, `0x0802D8FE`,
`0x0802ECAC`).

---

## 2. How NC talks to the terminal

This is the part most likely to be driven wrongly, so get it exact.

1. On start NC prints its banner via **`MON 504B DVOUTS`** to **device 1**:
   `Norsk Data C - Version: A06 - 1989-01-10`
2. It then reads via **`MON 503B DVINST`**, **one character at a time**
   (`MaxNo = 1`), initially from **device 0** — the SINTRAN command buffer,
   i.e. the initial argument line, not the terminal.
3. **There is no `NC:` prompt until the first CR.** When `DVINST` returns a
   CR (`0x0D`) on device 0, NC emits `0D 0A 4E 43 3A 20` = `\r\nNC: ` and
   **switches its input to device 1** (the interactive terminal). Every command
   after that is read character-by-character from device 1.
4. NC also calls **`MON 12B SETCM`** (SetCommandBuffer) at `0x0802DB6E` with a
   `0x80`-byte buffer. This is the command-buffer path — it is how the initial
   argument line reaches the program.

Two correct ways to drive it:

- **One-liner via device 0.** Queue `COMPILE name,name,name\r` *before* the
  banner. NC consumes it as the initial command line. This is what the ctest
  `dom_nc_compile_*` tests do.
- **Interactive.** Queue `\r` first (that yields the `NC: ` prompt), then the
  command text: `\rcompile B,B,BOUT\r`.

Sending an interactive command without the leading `\r` will be swallowed by the
device-0 path and the prompt will never appear.

---

## 3. Command reference

From NC's own built-in `help`. `<x: >` = a value NC prompts for, `[...]`
optional, `...` repeatable.

| Command | Arguments / prompts |
|---|---|
| `compile` | `<source file: >,<list file: >,<object file: >` |
| `preprocess` | `<source file: >,[<list file: >],[<output file: >]` |
| `check` | `<source file: >,[<list file: >],[<CAT file: >]` |
| `generate-code` | `<CAT file: >,<object file: >` |
| `link` | `<source file: >,<program: >` |
| `cross` | `<source file: >,<cross reference file: >,<lines per page: >` |
| `format` | `<source file: >,<new source file: >` |
| `define` | `[<macro identifier [(identifier,...)]: >],[<value: >]` |
| `undef` | `[<macro identifier: >]` |
| `directory` | `[<include directory/user: >]` |
| `options` | `<option: >...` (repeatable) |
| `library` | `<library file: >...` (repeatable) |
| `value` | `<definitions / options / libraries: >` |
| `page-length` | `[<lines: >]` |
| `initialize-compile-parameters` | `[<initialization file: >]` — reads `NC-A:INIT` |
| `save-compile-parameters` | `[<initialization file: >]` — writes `NC-A:INIT` |
| `clear` | (none) — reset compile parameters |
| `cc` | (none) — silent, returns to `NC:` |
| `help` | `<command: >` (blank = list all) |
| `exit` | (none) |
| `@<cmd>` | pass a command to SINTRAN |

### File-name handling — a real trap

**NC treats a dot as part of the name.** Typing `B.C` makes NC look for host
file `B.C.C`, because it appends the default type itself. Always type the bare
SINTRAN name: `B`.

Default types NC appends: source `:C`, listing `:LIST`, object `:NRF`,
preprocessed `:PP`, intermediate `:CAT`.

### `NC-A:INIT`

`save-compile-parameters` writes it (77 bytes). Contents are the default option
string:

```
options m2  a4  f-  r4  l+  d+  n+  s-  p-  i-  o-  pr- ic+ lm+ t-  a-  lo+
```

Each token is `<flag><+|-|digit>`. On start NC always tries to open `NC-A:INIT`
and normally fails with error `-46` (no such file). **Pre-placing a valid one
makes the open succeed and changes nothing else** — the missing init file is not
the blocker.

---

## 4. Observed behaviour of each phase

All probes used source `GUEST/B.C` containing `#define VALUE 42 ... x = VALUE;`.

| Command | Result |
|---|---|
| `preprocess B,B,BPRE` | writes `BPRE:PP`, 36 bytes = correct macro-expanded source, then `no rewrite` / ` terminated`, then `MON 0B LEAVE` |
| `check B,B,BCHK` | prints `preprocessing`, writes `BCHK:CAT`, 30 bytes = **preprocessed source only, not a parsed intermediate**, then terminates |
| `compile B,B,BOUT` | opens `BOUT:NRF`, `B:LIST`, `B:C`; `BOUT:NRF` = 30 bytes of preprocessed source; terminates |
| `generate-code` | never reached when chained — any preceding phase command exits the whole program |
| `link B,BPROG` | opens `SCRATCH-00001:MODE`, prints `preprocessing`, terminates |
| `cc`, `clear`, `options`, `define`, `undef`, `directory`, `library`, `value`, `page-length` | return to `NC:`, no exit |

Full console transcript for `compile B,B,BOUT`:

```
Norsk Data C - Version: A06 - 1989-01-10
NC:
preprocessing

no rewrite

 terminated
   0:00:01
```

### What is established about the blocker

- **It is not a MON error.** A full MON trace of `check` shows every call
  (`50B OPEN`, `117B RFILE`, `120B WFILE`, `76B SETBS`, `73B SMAX`,
  `62B RMAX`, `43B CLOSE`) returning **success**.
- `no rewrite` is not a literal string in the disassembly; it is assembled at
  run time and emitted via **`MON 32B MSG`** from `PC 0x0802D199`.
  (`0x0802D1CC` is the separate newline-MSG site.)
- Over 1,113,964 traced instructions: all compiler code is loaded and
  exercised, the highest PC reached is `0x0802E506` (the top of the code), so
  **the back end is present and reachable**. 871k instructions (87%) run in the
  `0x0800Dxxxx` lexer/preprocessor region and produce the **correct** expansion
  (`VALUE` → `42`). The front end works.
- The driver at `0x0802Cxxx-0x0802Exxx` then emits `no rewrite` via the print
  routine at `0x0802D171` **without invoking the code generator**.
- **Not** caused by: the missing `NC-A:INIT`; the `o+` option (tested); any MON
  error return.

---

## 5. The MON contract — all 34 calls NC issues

Extracted from `nc-a06.dom` by disassembly. `args` is the argument count encoded
in the `CALL` instruction. Names are from the authoritative registry
(`/home/ronny/repos/nd500x/src/libmon/mon_registry.c`) and the 233 YAML specs,
**not** from `nd500-dis`'s built-in table, which is wrong for several numbers.

| MON | dec | Name | args | sites | wrapper(s) |
|---|---|---|---|---|---|
| `0B` | 0 | LEAVE (ExitFromProgram) | 0 | 5 | `0802D1D8` `0802D447` `0802D8FE` `0802ECAC` + entry |
| `1B` | 1 | INBT (InByte) | 2 | 1 | `0802DB0D` |
| `2B` | 2 | OUTBT (OutByte) | 2 | 1 | `0802DB40` |
| `11B` | 9 | TIME (GetBasicTime) | 0 | 2 | `0802D1D8` `0802D447` |
| `12B` | 10 | SETCM (SetCommandBuffer) | 1 | 1 | `0802DB6E` |
| `30B` | 24 | GETRT (GetOwnRTAddress) | 0 | 1 | `0802DBC7` |
| `32B` | 26 | **MSG (OutMessage)** | 1 | 6 | `0802D171` `0802D1A5` `0802D1D8` `0802D447` `0802D8FE` |
| `41B` | 33 | ROBJE (ReadObjectEntry) | 2 | 1 | `0802DBDC` |
| `43B` | 35 | CLOSE (CloseFile) | 1 | 1 | `0802DC06` |
| `50B` | 40 | **OPEN (OpenFile)** | 4 | 1 | `0802DC2D` |
| `54B` | 44 | MDLFI (DeleteFile) | 1 | 1 | `0802DD13` |
| `62B` | 50 | **RMAX (GetBytesInFile)** | 2 | 1 | `0802DD76` |
| `64B` | 52 | ERMSG (WarningMessage) | 1 | 2 | `0802D447` `0802DDA0` |
| `73B` | 59 | **SMAX (SetMaxBytes)** | 2 | 1 | `0802DDB7` |
| `76B` | 62 | **SETBS (SetBlockSize)** | 2 | 1 | `0802DDE0` |
| `113B` | 75 | CLOCK (GetCurrentTime) | 1 | 1 | `0802DE0D` |
| `114B` | 76 | TUSED (GetTimeUsed) | 0 | 2 | `0802D1D8` `0802D447` |
| `117B` | 79 | **RFILE (ReadFromFile)** | 5 | 1 | `0802DE4B` |
| `120B` | 80 | **WFILE (WriteToFile)** | 5 | 1 | `0802DE93` |
| `122B` | 82 | RESRV (ReserveResource) | 3 | 1 | `0802DEDB` |
| `123B` | 83 | RELES (ReleaseResource) | 2 | 1 | `0802DF03` |
| `142B` | 98 | ERMON (ToErrorDevice) | 2 | 3 | `0802D1D8` `0802D447` |
| `143B` | 99 | RSIO (ExecutionInfo) | 4 | 2 | `0802D447` `0802DF21` |
| `221B` | 145 | CRALF (CreateFile) | 3 | 1 | `0802DFB5` |
| `256B` | 174 | DEABF (FullFileName) | 3 | 1 | `0802E01A` |
| `262B` | 178 | CPUST (GetSystemInfo) | 2 | 1 | `080290A8` |
| `312B` | 202 | MOINF (CheckMonCall) | 2 | 1 | `0802907E` |
| `317B` | 207 | UECOM (ExecuteCommand) | 1 | 1 | `0802E116` |
| `321B` | 209 | UEADM (UEAdministrator) | 3 | 1 | `08029027` |
| `412B` | 266 | FSCNT (FileAsSegment) | 4 | 1 | `0802E169` |
| `413B` | 267 | FSCDNT (FileNotAsSegment) | 2 | 1 | `0802E193` |
| `422B` | 274 | GSWSP (GetScratchSegment) | 3 | 2 | `0802CCF7` `0802D447` |
| `503B` | 323 | **DVINST (InputString)** | 14 | 1 | `0802E1CF` |
| `504B` | 324 | **DVOUTS (OutputString)** | 3 | 1 | `0802E1A6` |

Bold = on the critical path for `compile`, or a call whose semantics are easy to
get wrong.

### Note on `321B UEADM`

The registry marks `321B` as deliberately erroring (deprecated). NC calls it
once, at `0x08029027`. The C emulator formerly returned success here; correct
behaviour is **error 52 with the K flag set**. Verify this is still right — if a
handler returns success where NC expects failure, NC may take a different branch.

---

## 6. Argument-marshalling contracts, read out of the binary

These are the shapes NC actually passes. If the emulator's handler disagrees
with any of them, that is a bug — and several are exactly the sort of bug that
would silently route the driver into `no rewrite`.

### `MON 117B RFILE` — wrapper `0x0802DE4B`

```
0802DE56: w comp2  b.0x20,b.0x28      ; compare two counts
0802DE59: if >= go $0xA
0802DE5C: w1 := b.0x20                ; take the smaller ...
0802DE5E: w1 =: b.0x38
0802DE63: w1 := b.0x28                ; ... of the two
0802DE65: w1 =: b.0x38
0802DE6D: h wconv b.0x16,b.0x2C       ; FileNo    : halfword -> word
0802DE72: h wconv b.0x1A,b.0x30       ; WaitFlag  : halfword -> word
0802DE77: h wconv b.0x26,b.0x34       ; BlockNo   : halfword -> word
0802DE7C: call $0xF800004F,$0x5, b.0x2C, b.0x30, @b.0x1C, b.0x34, b.0x38
                                      ; MON 117B RFILE(FileNo, WaitFlag, @Buff, BlockNo, NoOfBytes)
0802DE88: if -k go $0xA               ; K flag set => error
```

`MON 120B WFILE` at `0x0802DE93` is byte-for-byte the same shape.

Spec (from the YAML):

- `[I] FileNo (INTEGER2)`
- `[I] WaitFlag (INTEGER2)` — 0 = suspend until complete
- `[O] Buff (BYTES)` — must start on an **even byte address**
- `[I] BlockNo (INTEGER2)` — **-1 = next block**
- `[I] NoOfBytes (LONGINT)` — a full 32-bit **byte** count

Three things to check in the emulator:

1. **`NoOfBytes` is a byte count, 32 bits, and is `min(requested, available)`
   as computed by NC itself.** The classic ND-100 `RFILE`/`WFILE` take a *word*
   count. **CHECKED 2026-07-10: the C emulator already treats it as bytes**
   (`fread(buffer, 1, num_bytes, ...)`). Not a bug there. Still worth checking
   in C#.
2. **`BlockNo = -1` means "next block".** A handler that treats `-1` as an
   absolute block index, or as an error, breaks sequential streaming.
   **CONFIRMED BUG, fixed 2026-07-10.** Both C handlers read `BlockNo` into a
   `uint32_t` and computed `block_no * block_size`, so the sign-extended `-1`
   became `0xFFFFFFFF` and every seek went ~2.2 TB past EOF. `-1` now resumes
   from the recorded position. The same bug should be checked for in C#.
3. **The spec declares no `[O]` transferred-byte-count parameter.** If the
   handler writes one anyway, it is clobbering `NoOfBytes` (an `[I]` parameter).

The preprocessing round-trip does `WFILE` then `RFILE` on the intermediate.

**Correction (2026-07-10): the byte-vs-word hypothesis is disproven for the C
emulator.** An earlier draft of this document ranked it first. Reading
`mon_117B_ReadFromFile.c` shows the count was already handled as bytes. The
`BlockNo = -1` defect found alongside it is real and is now fixed, but it
cannot be the cause of `no rewrite` either: the traced `check` run showed every
`RFILE`/`WFILE` returning success, which means NC was not passing `-1` on that
path. **There is currently no leading hypothesis backed by evidence.**

### `MON 50B OPEN` — wrapper `0x0802DC2D`

`OPEN(FileNo, AccessCode, FileName, FileType)`.

- `[IO] FileNo (INTEGER)` — **0 on input means "allocate and return a file
  number"**. This is an in/out parameter; a handler that only reads it will
  leave NC without a file number.
- `[I] AccessCode` — 0 sequential write, 1 sequential read, 2 random read/write,
  3 random read only, 4 sequential read/write, 5 sequential write append,
  6/7/8/9 contiguous and RT variants.
- `[I] FileName (STRING)` up to 64 chars. **If empty, the name is read from the
  terminal** — worth knowing, because an empty name is not an error.
- `[I] FileType (STRING)` up to 4 chars, e.g. `'SYMB'`.

NC builds the string descriptors via the helper at `0x0802E208`, which is called
with `(@descriptor, r1, r2, buffer, $0x0, $0x7F)` — note the `0x7F` length cap.

The C# emulator has been observed to **throw on an empty filename**, where the C
emulator returns error `0x2E` (46). Per the spec, neither is right for the
"prompt the user" case. Both are wrong in different ways; the divergence is a
known open item.

### `MON 503B DVINST` — wrapper `0x0802E1CF`

14 arguments:

```
DVINST(DevNo, MaxNo, @NoOfBytesRet, @Buff,
       BreakStrat, EchoStrat,
       BreakT1..BreakT4, EchoT1..EchoT4)
```

- `[I] DevNo` — **0 = SINTRAN command buffer, 1 = own terminal**. NC starts on
  0 and switches to 1 after the first CR. Getting this wrong is why the `NC:`
  prompt does or does not appear.
- `[I] MaxNo` — max bytes before break. **NC passes 1**: it reads one character
  at a time.
- `[O] NoOfBytesRet`, `[O] Buff` — both outputs.
- `BreakStrat`/`EchoStrat` = 8 means "use the user-defined table" that follows.
- The four `BreakTn` words are a 128-bit break table; a set bit causes a break.
  The four `EchoTn` words are the echo table; a **0** bit causes echo (note the
  inverted sense).

### `MON 504B DVOUTS` — wrapper `0x0802E1A6`

`DVOUTS(DeviceNo, NoOfBytes, Buffer)`. `NoOfBytes` max 2048. Device 1 = own
terminal; otherwise a SINTRAN open file number.

### `MON 32B MSG` — six sites

`MSG(Message)` — a single `[I] STRING`, max 512 chars, to the user's terminal.
This is what emits `no rewrite` (from `0x0802D199`) and the newlines (from
`0x0802D1CC`). NC uses `MSG` for diagnostics and `DVOUTS` for the banner.

### `MON 76B SETBS`, `73B SMAX`, `62B RMAX`

- `SETBS(FileNumber, BlockSize)` — **`BlockSize` must be even**; factors of 2048
  are most efficient. NC sets 2048.
- `SMAX(FileNumber, MaxBytePointer)` — `INTEGER4`, a **byte** pointer.
- `RMAX(FileNumber, @NoOfBytes)` — `[O] INTEGER4`, a **byte** count.

All three are byte-denominated on the ND-500. Same word-vs-byte trap as
`RFILE`/`WFILE`.

### `MON 312B MOINF`

`MOINF(MonCallNumber, @MonCallEntry)`. Returns the address of the monitor-call
entry, **0 means not implemented**. Both emulators correctly return a fake entry
address of `0xF8000000 + n` for implemented calls. A C-side test expecting `0/-1`
is the test being wrong, not the handler.

### `MON 412B FSCNT` / `413B FSCDNT`

`FSCNT(FileNo, LogSegmentNo, AccessType, @SegmentNo)` — map a file as a segment.
`LogSegmentNo = 0` means "pick the first free one" and the choice comes back in
`[O] SegmentNo`.

`FSCDNT(FileNumber, LogSegmentNumber)` — unmap. Note the spelling: the registry
name is `FSCDNT`, and the C# emulator registers it as **`FSDCNT`** while its
code calls it `FSCDNT`. Its 97 lines of real logic are **dead code** because of
the broken registration wiring.

---

## 7. Where to look next

The single highest-value experiment, unchanged from the earlier probe:

> Set an execution breakpoint at `PC 0x0802D199` (the `MON 32B MSG` site that
> emits `no rewrite`) and walk **backwards** through the trace. Find the branch
> that routed the compile into the `no rewrite` path instead of the code
> generator, then find the value it tested and where that value came from.

Ranked hypotheses:

1. ~~**`RFILE`/`WFILE` byte-count semantics.**~~ **RULED OUT 2026-07-10.** The C
   handler already treats `NoOfBytes` as a byte count (`fread(buffer, 1,
   num_bytes, ...)`). A genuine `BlockNo = -1` bug was found next to it and
   fixed, but the traced `check` run showed every `RFILE`/`WFILE` returning
   success, so NC never exercised it either.
2. **`nc-a06.dom` is a front-end-only image** and expects a separate driver or a
   genuine two-step `check` → `generate-code` with a real parsed `:CAT`. Against
   this: the back end is present and the top of the code is reached. For it: the
   `:CAT` this front end writes is just preprocessed source, not a parsed
   intermediate.
3. **`321B UEADM` returning success** where the spec says error 52 + K flag.
4. The option string. **Ruled out** — `o+` was tested and changes nothing.
5. The missing `NC-A:INIT`. **Ruled out** — pre-placing a valid one changes
   nothing.

A golden reference for what a real NRF file looks like:
`/mnt/d/ND/500/FraTor/test-real/test-real.nrf` (genuine ND-produced, 1991).

---

## 8. Provenance

**Established by disassembling `nc-a06.dom` for this document:** the 50 MON call
sites, the 34 distinct numbers, the wrapper addresses, the argument counts, the
fact that NC uses `CALL` exclusively and never `CALLG`, and the
`RFILE`/`WFILE`/`OPEN`/`DVINST`/`SETCM` marshalling sequences quoted verbatim in
section 6.

**Taken from the authoritative MON tables** (`mon_registry.c`, the 233 YAML
specs, `mon-calls.json`, which agree with each other): all parameter names,
types, directions, and the semantics of `AccessCode`, `BlockNo = -1`, the break
and echo table conventions.

**Taken from the earlier emulator probes** (`NC-INTERFACE.md`): the console
dialogue, the per-command outcomes, the instruction-trace figures, and the list
of ruled-out causes. I did not re-run those probes.

**Inference, not fact:** that the `RFILE`/`WFILE` count semantics are the cause
of `no rewrite`. It is the best-supported hypothesis, not a finding.
