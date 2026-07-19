# CARVE REQUEST - MON 50B OPEN quoted-vs-unquoted file-name semantics

**For:** the sintran-segment-carver, to byte-verify the exact SINTRAN III OPEN
behaviour so the nd500x/RetroCore emulators implement it correctly.
**From:** nd500x linker bring-up (2026-07-17).
**This request:** `/mnt/e/Dev/Ronny/NDInsight/SINTRAN/ND500/CARVE-REQUEST-OPEN-QUOTED-FILENAME.md`

## What we already know (from the printed manuals - want byte confirmation)

The SINTRAN "create-on-open" convention: a file name **in double quotes** means
"create it if it does not exist" (indexed file); **without quotes** the file
**must already exist**. Manual sources:
- ND-60.050.06 SINTRAN III Users Guide p1422, p977, p851
- ND-60.128.5 SINTRAN III Reference Manual (lines ~2054, ~6978, ~2227)
- ND-860289-2-EN ND Linker manual line 956 (the linker's OPEN-DOMAIN relies on it)

Observed in the ND linker: `OPEN-DOMAIN "A-TEST"` issues `MON 50B OPEN` with the
name **including the quote characters**: `FileName='"A-TEST"'`, AccessCode=2.
`OPEN-DOMAIN A-TEST` (no quotes) issues `FileName='A-TEST'`.

## The questions (byte-level answers wanted)

1. **Where does MON 50B OPEN (or its resident worker) detect the `"` quote
   characters**, and what exactly does it do with them - strip them, set a
   "create" flag, and how? Cite the segment + octal addresses.

2. **Exact create semantics of a QUOTED name:** does it always create, or only
   when the file is absent? If the quoted file already exists, is it truncated,
   overwritten, versioned, or opened as-is? (The linker manual line 956 says an
   unquoted existing domain is "overwritten" - is that OPEN's doing or the
   linker's?)

3. **Exact semantics of an UNQUOTED name that does NOT exist:** which error code
   is returned? We assume "No such file" (056B / 46). Confirm, and confirm OPEN
   does NOT create in this case for ANY access code (0..9). This is the half we
   have NOT yet enforced because it risks the NC compiler.

4. **Access-code interaction:** does the quote/no-quote rule apply uniformly
   across all access codes (0 seq-write, 2 random RW, 3 random read, 5 append,
   etc.), or do some access codes create regardless of quotes? Specifically:
   what does OPEN do for **access 0 (sequential write)** of a missing file with
   and without quotes - does write-open imply create even unquoted?

5. **The NC case (critical for us):** the NC compiler creates its `A:NRF` and
   `A:LIST` output. Does it open them WITH quotes (`"A:NRF"`), or does it
   `@CREATE-FILE` (MON 221B) first and then open unquoted, or does it rely on
   write-open-creates? If you can see NC's actual OPEN sequence in a trace or
   infer it from the SINTRAN OPEN worker's access-code handling, that settles
   whether we can make unquoted-missing-write fail without breaking NC.

6. **Version syntax:** the manuals mention quotes can also wrap a version number
   to create a new version. Is that the same code path as name-quoting, and does
   it matter for a plain create?

## Why this matters

nd500x currently AUTO-CREATES any write-access OPEN of a missing file (non-
standard). We just added quote-stripping so `OPEN-DOMAIN "A-TEST"` creates the
domain (works - A-TEST.DOM now written). But to implement the CORRECT convention
(unquoted must-exist) we must know it will not break NC, whose output-file
creation path we have not been able to pin down. A byte-verified answer to Q3+Q5
lets us finish this correctly.

## Where nd500x implements it (for reference)

`/home/ronny/repos/nd500x/src/libmon/mon_file_table.c` - `mon_file_open_ex()`:
the quote-strip is near the top; the create fallback is `if (!fp && allows_write)
fopen(host_path, "w+b")`.
