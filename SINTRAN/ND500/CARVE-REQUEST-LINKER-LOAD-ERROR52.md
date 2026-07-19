# CARVE REQUEST - ND Linker LOAD command errors 52 BEFORE opening the NRF

**For:** the sintran/linker byte-carver.
**From:** nd500x linker bring-up (2026-07-17).
**This request:** `/mnt/e/Dev/Ronny/NDInsight/SINTRAN/ND500/CARVE-REQUEST-LINKER-LOAD-ERROR52.md`
**Linker binary + full disassembly:** `/mnt/d/ND/500/nd-linker/linker-b01.dom`
and `/mnt/d/ND/500/nd-linker/linker-b01.dom.asm` (105,189 lines).

## Where we are (byte- and trace-verified)

The ND linker now runs under nd500x. `OPEN-DOMAIN "A-TEST"` **fully succeeds** - it
creates a real 4096-byte `A-TEST.DOM` (MON 120B WFILE fires twice, header written).

The wall is the very next command, `LOAD B:NRF`:

1. LOAD resolves the file name `B:NRF` through the shared resolve helper
   **B0000A3D** (which wraps the single MON 256B DEABF call site at **B0000A6D**).
   Our DEABF returns **SUCCESS** with the resolved name; the helper returns K clear.
2. LOAD then makes a **pure-CPU decision to error (code 52 octal = 42 decimal)**
   with **no further MON call**, and **NEVER issues a 50B OPEN of B.NRF**.
3. The error is rendered on the console as:
   `*** (blank text)  (-677:52)`
   The SSI field `-677` looks like a garbage/uninitialised descriptor; `52` is the
   error number (displayed octal, like every other linker error e.g. `(0055:41)`).

`B.NRF` is a real 513-byte NRF object (NC output: contains `PROG!NAME`, symbol
strings). It exists in the sandbox and DEABF resolves it to `./GUEST/B.NRF`.

## Key finding that redirects the question

DEABF is **NOT** the differentiator. There is exactly **one** DEABF call site in
the whole linker (B0000A6D, wrapped by resolve helper B0000A3D). **Both**
`OPEN-DOMAIN "A-TEST"` and `LOAD B:NRF` call the *same* helper and our handler
returns SUCCESS to both. OPEN-DOMAIN's caller is satisfied; LOAD's caller errors 52.

So error 52 is **LOAD-command-specific logic that runs AFTER name resolution and
BEFORE the OPEN**. That code is what we need carved.

## The questions (byte-level answers wanted, cite segment + octal/hex addresses)

1. **Find the LOAD command handler** in `linker-b01.dom.asm`. Where does the
   command dispatcher route the "LOAD" command? Give its entry address.

2. **Trace LOAD from the resolve call to the error.** After the shared resolve
   helper **B0000A3D** returns success for `B:NRF`, what does LOAD check next?
   Identify the **exact instruction** that decides to raise error **52** (the
   value that reaches the error-reporter path - the reporter formats at
   **B004C9C1**, whose RSIO/CPUST/MOINF error dump we see at **B004CA35**).
   What register/stack slot holds the `52` and where is it set?

3. **What does error 52 MEAN** in the linker's own error vocabulary (and/or the
   SINTRAN error table)? Is it "no domain/segment open", "illegal object /
   wrong file type", "not an NRF", "no current segment", or something else?
   The `(-677:52)` SSI `-677` - is that a real subsystem id or a sign that a
   descriptor was never filled in (i.e. a *secondary* fault masking the real
   cause)?

4. **What state must exist before LOAD will open the NRF?** Specifically: does
   LOAD require a preceding command we are not issuing - e.g. `SET-DOMAIN`,
   `OPEN-SEGMENT`/`NEW-SEGMENT`, or a *current segment* inside the freshly
   OPEN-DOMAIN'd A-TEST? Our script is literally:
   `OPEN-DOMAIN "A-TEST"` ; `LOAD B:NRF` ; `EXIT`.
   If the real batch needs an intervening command, name it (this may make error
   52 correct linker behaviour for our missing step, not an emulator bug).

5. **Does LOAD re-open / stat B:NRF via a path we are getting wrong?** After
   B0000A3D, before the error, does LOAD expect the resolved name to carry a
   field our DEABF does not populate (directory index, user index, version,
   object-file-type flag)? If the post-resolve check reads such a field from the
   descriptor our resolve wrote, tell us which field and its expected value so we
   can populate it. (Our resolve currently writes only the ASCII full name +
   apostrophe terminator to the output descriptor's pointer.)

## Why this matters

This is the last gate before an end-to-end C -> NRF -> :DOM link under nd500x.
OPEN-DOMAIN works and writes a real domain; only LOAD's post-resolve error 52
stands between us and a linked, runnable binary. Q3+Q4 are the highest value:
they tell us whether 52 is a genuine emulator/MON defect or a missing linker
command in our driving script.

## Where nd500x implements the relevant handlers (for reference)

- Name resolve: `/home/ronny/repos/nd500x/src/libmon/handlers/mon_256B_FullFileName.c`
- OPEN: `/home/ronny/repos/nd500x/src/libmon/mon_file_table.c` (`mon_file_open_ex`)
- The driving harness: `/home/ronny/repos/nd500x/test/diag_linkdrive.c`, run
  pinned from `/home/ronny/repos/nd500x/build/link_sandbox/`.
