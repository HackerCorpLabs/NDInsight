# Who writes the swap file? Three findings that narrow it

**Date:** 2026-08-03
**Context:** the 5SWAP trap is fully explained (`RPHS @ 1000010525`, physical address
`b.30 x 0o400` with `b.30 = 0`). The zero comes from a swap page that was never written. This
is the search for what should have written it.
**Status:** not answered; three candidates eliminated.

---

## 1. `DEFINE-SWAP-FILE` does not initialise the file `[V]`

`FUNCS[046] = SWFDE @ 156207` in `030-S3SM5`. Its entire `MON` inventory:

```
156241  MON 50   OpenFile
156333  MON 43   CloseFile
```

**It opens the swap file and closes it. No `RFILE`, no `WFILE`, nothing in between.** So
`define-swap-file` validates and registers the file; it does not lay down initial content.

That matters because the operating procedure is `create-file swap-file-0:swap` then
`define-swap-file`, and `create-file` on SINTRAN yields a file whose pages read as zero. Nothing
in either step writes a page.

## 2. The whole ND-500 system monitor has exactly ONE `WFILE`, and it is bidirectional `[V]`

Across all 49152 words of `030-S3SM5` there is a single `MON 120` (WriteToFile), at `113240`.
It sits in a shared transfer primitive whose direction is chosen by a caller-supplied flag:

```
113230  LDA ,B -200        direction flag
113231  JAF 5   -> 113236
113232  RADD CLD SX DA
113233  ADD 21
113234  MON 117            ReadFromFile     <- flag one way
113235  JMP 4   -> 113241
113236  RADD CLD SX DA
113237  ADD 15
113240  MON 120            WriteToFile      <- flag the other way
```

So "the monitor can write a file" is true but says nothing on its own - **every write goes
through one door, and the caller decides.** Finding a write to the *swap* file means finding a
caller that passes both the swap file's connect number and the write flag.

## 3. SINTRAN does not choose the direction either - the swapper does `[V]`

`LSWPAGE` (`MP-P2-N500.NPL:136112`, its own comment: `% Disk I/O`) is the handler that services
the swapper's page requests:

```
136115  11=:L; SWMSG+"SWPINFO"=:D; 5MBBANK; T:="XSDUNIT"; *MOVPA
        % Move parameters from swmsg to par.array for 5swap
136136  IF XABSFUNC/\77=60 THEN XABSFUNC+6=:XABSFUNC FI
```

**`XABSFUNC` - the disc function code - is moved out of the swapper's own message** (`SWMSG` +
`SWPINFO`). SINTRAN adjusts it for disc optimisation and executes it; it never decides read
versus write.

The measured request carried `XABSFUNC = 0o60` (already recorded), i.e. **the swapper explicitly
asked for a READ** of that swap page.

---

## 4. What this leaves

Three plausible writers are now eliminated: `define-swap-file`, any implicit initialisation in
the monitor's file layer, and SINTRAN's own disc path. The read of an unwritten page is
**requested by the swapper itself**, from its own tables.

So the question changes shape. It is no longer "who forgot to write the swap file" but:

> **Why do the swapper's tables say this page lives on the swap file, when nothing has ever put
> it there?**

On a first placement the page should either be absent (fault it in from the domain `:PSEG`) or
be written out before being read back. The tables are set up by the place path, whose descriptor
writes were carved earlier (`SGLOA`, four indirect stores through `B-176`, no bulk copy).

**Next concrete step:** identify the fields `SGLOA` writes and whether one of them is the
"page is on the swap file" indication. That is a bounded read of about twenty instructions, and
it is the same `B-176` structure already flagged as unidentified in
`SGLOA-SEGMENT-PLACE-CARVED-2026-08-03.md` section 3.

**Caution carried forward:** this whole line assumes the emulated first-start sequence matches a
real machine's. The trap is domain-independent and reproduces with no domain installed, so it is
equally possible that a real machine never reaches this state and the harness is starting the
swapper in a way the hardware would not. That has not been tested either way.
