# `SGLOA` - what MON 60 subfunction 006 actually does, and why no bulk read happens at PLACE time

**Date:** 2026-08-03
**Subject:** `030-S3SM5` (the ND-500 System Monitor segment, ND-100 code, base `40000B`)
**Routine:** `FUNCS[006] = SGLOA @ 142637`, "load (place) one segment"
**Depends on:** [`nd-500-mon/RECOVER-DOMAIN-WORKER-AND-SEGMENT-LOAD-CARVED-2026-08-02.md`](nd-500-mon/RECOVER-DOMAIN-WORKER-AND-SEGMENT-LOAD-CARVED-2026-08-02.md)

---

## 1. The hard result

**`SGLOA` contains no `RFILE`.** In its 457 words (`142637`-`143550`) there is exactly
**one** `MON` instruction of any kind: `MON 43` (CLOSE) at `143517`.

It does open a file - by calling a shared helper at `100732`, whose `MON 50` (OPEN) is at
`100735` - and it closes one. **Between the open at `143352` and the close at `143517`
there is no file read of any kind.**

So the last hop of the chain does not read segment content either. Every hop is now
accounted for:

| Hop | Reads segment content? |
|---|---|
| `042115` monitor-side loader | No - no `MON`, no `IOX` at all |
| `5IFUNC[006]` ISEGLOAD | No - copies the segment **name** only |
| `5NOPAR` | No - copies the info block to the ND-500 data segment, then `FPT2ENTRY` |
| `FUNCS[006]` **SGLOA** | **No - opens and closes, never reads** |
| return path `FUNCS[006]` = `RET5` | No - does nothing |

---

## 2. The `RFILE` cluster in this segment is CSLOA's, not SGLOA's

`030-S3SM5` does contain real `RFILE` calls - six of them, at `154026`, `154255`, `154334`,
`155120`, `155216`, `155375`. They are **not** reachable from `SGLOA`, and they are not for
segments:

- `FUNCS[037] = CSLOA @ 153441` - **load control store**. `FUNCS[040] = DEFMC @ 155742`.
  So `153441`-`155741` is CSLOA's body, and all six `RFILE` sites fall inside it.
- A whole-image scan for `JPL`/`JMP` (direct and one level of indirection) resolving into
  `153700`-`155600` returns **91 call sites, every one of them inside that same block**.
  Nothing outside calls in.

**Those reads are the microcode file, not a domain segment.** Anyone chasing "S3SM5 reads
files, so it must be reading the domain" lands here and should stop.

---

## 3. What SGLOA does between open and close

`[I]` - read from the instruction stream, not yet proven by execution.

```
143352  JPL -> 100732     open the file (MON 50 lives at 100735)
143355  JPL -> 077236
143361  JPL -> 100740     the same helper's "already open" entry (enters past the MON 50)
143370  JPL -> 077236
143375  JPL -> 077362
143407  JPL -> 100656
143414  JPL -> 072040
...
143446  STA I ,X 71       ->  PSPHS[B-176] := physical start
143461  STZ I ,X 57       ->  PSLLI[B-176] := 0
143462  STA I ,X 57       ->  PSULI[B-176] := size-1
143473  STA I ,X 47       ->  PSMOD[B-176] := mode flag
143517  MON 43            close
```

**CORRECTED 2026-08-03.** This block first read as "indirect stores through the pointer in
`B-176`, writing FIELDS of a structure". That was wrong: `I` plus `X` with `B` clear is
POST-indexed indirect (`EA = M[P+disp] + X`), so the displacement names a pointer word in the
routine's own literal pool and `B-176` is the ARRAY INDEX. The four stores go to four separate
resident tables, all named in `N500-SYMBOLS.SYMB`. Full derivation, the pool dump and the
evidence for the symbol identification:
[PSPHS-PHYSICAL-SEGMENT-TABLES-CARVED-2026-08-03.md](PSPHS-PHYSICAL-SEGMENT-TABLES-CARVED-2026-08-03.md).

The shape is **descriptor setup**: open the file, interrogate it through helper routines,
write a handful of fields into a structure reached indirectly through `B-176`. There is no
loop, no `MOVEW` and no block-transfer primitive in this window.

**Working hypothesis, explicitly NOT established:** `SGLOA` records where the segment lives
(and how big it is) and the content is paged in later, on demand, by the swapper - which is
what the measured MON 377B `LSWPAGE` traffic is. If that is right, then **"the floppy is
never bulk-read at PLACE time" is EXPECTED BEHAVIOUR, not the defect**, and the standing
"placement is requested but segment content is never fetched" line has been chasing a
non-problem.

That would be a significant reframing, so it needs proof before anyone acts on it. What
would settle it: identify the structure at `B-176` and confirm one of the stored fields is
the file connect number or a page/disc address that the swapper later consumes.

**UPDATE 2026-08-03 - half of that is now answered.** The four targets are `PSPHS`, `PSLLI`,
`PSULI` and `PSMOD`, and `PSPHS` is the *PHysical Start* table that the **`RPHS`** instruction
reads - the same `RPHS` the 5SWAP trap fires on. So a stored field IS an address the swapper
later consumes. What the stored VALUE is (it comes from the uncarved open/interrogate chain)
remains open. See
[PSPHS-PHYSICAL-SEGMENT-TABLES-CARVED-2026-08-03.md](PSPHS-PHYSICAL-SEGMENT-TABLES-CARVED-2026-08-03.md).

---

## 3a. The whole FUNCS vocabulary has no file-to-memory operation `[V]`

Rather than keep chasing SGLOA's callees, read the **complete** operation set of the ND-500
system monitor (`FUNCS-dispatch-table.md`, 60 live entries). Grouped:

| Group | Operations |
|---|---|
| Registers | `REGRE REGWR REGSR REGSW` |
| Memory | `PMREA PMWRI DMREA DMWRI AMEMR AMEMW PMEXA PMDEP DMEXA DMDEP RPHSG WPHSG` |
| Control store | `CSREA CSWRI CSLOA MPSTA MPSTO RMVER` |
| Place | `SGLOA SPLAC EPLAC` |
| Swapper | `LDSWA RUNSW TOSWP` |
| Swap file | `SWFDE SWFDL` |
| Process / files / name segment | `PROGS CONFI CLOSF RESR5 REL50 LIOPF SPRTE GPRTE SSGTE GSGTE SPRNM LINKT ...` |
| Memory config | `DEFMC LIMEM MEMSP G5PAG T5PAG` |

**There is no "read a file into ND-500 memory" operation anywhere in the table.** Every
content-moving operation is a memory primitive driven by the *caller* supplying the bytes
(`PMWRI`, `DMWRI`, `AMEMW`, `WPHSG`), or a read back to the caller (`RPHSG`, `AMEMR`).

Two name-checks done rather than assumed:

- **`076 TOSWP` is "message to swapper", not "copy segment to swap".** The name reads like a
  transfer; the table entry (`166733`) and the NPL handler (`ITOSWP:  % FUNCTION=076: MESSAGE
  TO SWAPPER`, `s3vs-4.symb:78328`) both say message. Checked precisely because the name
  suggested what I was looking for.
- **`110 WPHSG` ("write into a physical segment") has exactly ONE call site** in the whole
  monitor image, at `055736`. It is not part of the PLACE bracket.

**Consequence.** The PLACE subfunctions cannot be delivering segment content - no operation
they could call is capable of it. Combined with the measured swapper behaviour (its `LSWPAGE`
names the swap file's logical device number `0o1100` explicitly, already verified), the
question sharpens to:

> **What is supposed to write the domain into the swap file before the swapper reads it?**

The swapper is asking the right file for a page that was never written. That is the gap,
and it is upstream of everything carved so far.

---

## 4. What is NOT established

- The `[I]` above. Four indirect stores are not a proof of a descriptor.
- The identity of the helper routines `077236`, `077362`, `100656`, `072040`.
- Whether the file `SGLOA` opens is the domain `:PSEG`/`:DSEG` at all. It opens *a* file by
  going through the shared open helper; the name it passes was not traced.
- Whether some other subfunction in the PLACE bracket (`055 ISPLACE` / `056 IEPLACE`, or
  `007 IPLSWAPPER`) does the bulk transfer instead. Not checked.

---

## 5. Method notes

- **`030-S3SM5` is ND-100 code.** Disassemble the byte-swapped image with `nd100-dis` at word
  base `40000B`. `re/030-S3SM5-routine-map.md` says `nd500-dis` and calls the code region
  "ND-500 code" - its data findings are fine, its code-region claims are not.
- The entry is `FPT2E = 40000+3`; word `040003` is `JMP I 1` -> pointer `040004` = `142231`.
  The `FUNCS` table is at `142031B`.
- Tools used, in `tools/sintran-segment-carver/`: `swap.py` (big-endian carve ->
  little-endian for nd100-dis), `segcalls.py` (resolve calls inside a based segment),
  `findcallers.py` (who calls into an address range). All three resolve **one extra level**
  through the pointer word, because `JPL I <disp>` names the pointer, not the routine.
