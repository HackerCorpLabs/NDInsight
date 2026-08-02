# Handoff — finish the VERIFIED audit (remaining items)

**Date:** 2026-08-02
**Status of the work this continues:** committed and pushed. See
`NDInsight` `6211e39` + `5809d8f`, `norskdata-ndfs` `5208d28` + `f73e1d0`,
`RetroFS` `1c54721`, `RetroCore` `7ee8dd811`.

---

## 1. What this is about — read this first or you will repeat the mistake

A single documentation claim, marked **VERIFIED**, propagated a data-corruption bug into four
independent NDFS implementations and survived months of green test suites.

The claim was that the NDFS allocation bitmap is addressed as `byte N/8, bit N%8`. It is
actually addressed as a **16-bit word**: page `N` is bit `N%16` of word `N/16`. On a big-endian
image those differ by a byte swap inside every word.

**The evidence cited for it was a popcount comparison.** Popcount is invariant under
byte-swapping. It could not have failed whichever convention was right. That is the entire
failure mode this audit is about:

> **A claim with a discrete alternative, marked VERIFIED, whose cited check gives the same
> answer whether the claim is true or false.**

Three compounding traps made it survive:

1. **Round-trip tests cannot detect a symmetric error.** Write with convention X, read with
   convention X passes for any self-consistent X, including a wrong one.
2. **`ndtool` is not an independent cross-reader.** `ndfs-c/CMakeLists.txt`:
   `target_link_libraries(ndtool ndfs)`. It links the very library it was cited as a check on,
   so it cannot disagree. This phrase appeared in four provenance footers and was false in all
   of them.
3. **Agreement between re-implementations is not corroboration when they share an ancestor
   doc.** Four ports agreed with each other and were all wrong; the one hand-written
   implementation (`RetroCore/Emulated.Utilities/ND/FileSystem`) disagreed and was right.
   Weight provenance, not headcount.

**Your job is to audit whether the EVIDENCE SUPPORTS a claim — not whether the claim is true.**
Those are different, narrower questions. Most remaining items are probably-correct claims with
citations that cannot bear them. Say it that way. Do not delete a fact because its citation is
weak; mark the citation.

---

## 2. Ground rules

- **"CANNOT ASSESS" is a valuable answer.** There are ~4400 VERIFIED tags in this repo. An
  agent under pressure to produce findings will invent them. Twenty solid findings beat a
  hundred plausible ones.
- **Believe ND documentation.** `Reference-Manuals/`, `Operations/SINTRAN/`, and the manual
  mirror at `E:\Dev\Ronny\mirror-sintran-com`. Where the manuals are silent, **carve the
  SINTRAN code** (`tools/sintran-segment-carver/versions/*/re/segments-ref/`). Real ND
  microcode listings are at `E:\Dev\Repos\Ronny\ND120CPUEMU\ND120CPU\ROM\`.
- **Never cite our own re-implementation as proof of ND behaviour.** That includes `ndfs-c`,
  `ndfs-py`, `ndfs-ts`, `RetroFS`, RetroCore's emulator, and `ndtool`.
- **When you add a regression test for a decoding bug, prove it works by reverting the fix and
  confirming the test actually fails.** This was done for the fixes already made: the RetroFS
  relocation fixture fails 4 of 7 under the old formula, and 10 of 23 bit-order assertions fail
  under the old convention. A test that passes either way is worth nothing.
- Do not edit files outside the item you are working on. Several repos have unrelated
  in-progress work (BSD/FFS drivers, VolumeInfo, RetroCommander UI, ND500 handoffs).

---

## 3. Unresolved — needs new evidence, do not guess

These three were deliberately left open. Each names the experiment.

### 3.1 `BSET ONE 10 DX` — which bit? [MEDIUM]

`SINTRAN/Filesystem/code-logic/allocation.md` section 2 reads the disassembler's output
`174217 BSET ONE 10 DX` (at `006-S3FS` 50647) as "bit 10".

`SINTRAN/Filesystem/on-disk-format/extended-info-block.md` states the disassembler convention
explicitly: *"the disassembler prints the bit-number field pre-shifted: `170` octal = 120
decimal = `15 << 3`, i.e. bit 15."* Under that same convention `10` octal = 8 decimal =
`1 << 3` = **bit 1**.

Two documents in the same set decode the identical field two ways. Only one can be right.

**To settle:** decode `174217` against the `nd100-as` BSET encoding table and state the
bit-field extraction explicitly, as `extended-info-block.md` does. Then trace the
`JPL I 140 -> 051003` helper to find where the page-number-to-bit rotation actually happens —
a *constant* bit number cannot select the bit for an arbitrary page, so the sentence as written
does not describe a working bitmap primitive.

### 3.2 ACCP register roles at `0x770000` / `0x770004` [HIGH]

`SINTRAN/ND5000/ACCP-COMPLETE-REFERENCE.md` (~line 1509) marks several addresses **CONFIRMED**
on the evidence that an instruction names them, plus a hit count. That establishes the address
is *touched*, not its *role* — which is what the rows assert. A single
`move.w #4,(0x770006)` is equally consistent with a command register, a mode register, a mask,
or a length.

`SINTRAN/ND5000/ACCP-EMULATION-STATUS-AND-HANDOFF.md` (~line 682) describes `0x770004` as
"word data-in", while the reference calls it "a data register" with no direction. They
disagree, which is exactly what an instruction-existence check cannot settle.

This is the source for the emulated ACCP register map, so a wrong role silently mis-emulates
the octobus transport.

**To settle:** for each address, follow the full data flow — what value is written, what is
subsequently read, what branch depends on it — or run the real firmware and observe which write
causes an observable state change. That method already worked at `0x114550` for the signature
matrix (same file, ~line 2331). The genuinely PROVEN rows, e.g. `0x660001` bit 4 tied to the
`"$MF-bus memory timeout$"` string, show what good evidence looks like here.

### 3.3 ND-120 microcode version byte [MEDIUM]

`SINTRAN/OS/25-ND120-MICROCODE-VERSION.md` claims the version is the low 8 bits of the 64-bit
microword at address octal `020`, calibrated against one EPROM dump (version L, byte `0x0c`,
and L is the 12th letter).

A 32 KB microcode image contains thousands of bytes equal to `0x0c`. One point cannot
distinguish "this is the version field" from "this is an unrelated constant that happens to
equal 12", nor a version *field* from a fixed constant. The `0x0b -> K` row is pure
extrapolation; that value was never read.

**To settle:** read microword `020` from a **second** ND-120 EPROM of a different revision and
confirm the low byte tracks that letter's alphabet position. Or find the loader code / ND
documentation that names the version location.

---

## 4. Remaining LOW items — judged not worth churn, listed for completeness

Each is a correct-looking claim with a citation that cannot support it. No failure mode
identified. Fix only if you are already in the file.

| File | Item |
|---|---|
| `SINTRAN/Filesystem/on-disk-format/boot-sector.md` §5.2, and line ~250 | "VERIFIED against `boot_loader.c`" — circular for a claim about the ND format. Mitigating: the summary row at ~306 is honest (SMD path VERIFIED, others INFERRED); only the section heading over-claims. Better source: the ALD preset table in `ND-06.014.2A` p.232. |
| `SINTRAN/Filesystem/NDFS-VALIDATION.md` line ~43 | "Real disk decode + NDFS reader agree" is one party, not two. `directory-label.md` §5 has the real check — a second disk whose pointers were followed to actual content. Cite that. |
| `SINTRAN/Devices/SCSI/scsi-open-last-block-read.md` §4d vs `scsi-disk-format.md` ~244 | Two mutually exclusive field models for the same 12 bytes, both VERIFIED. "Reproduces every non-zero region" is a coverage check, invariant under how the third field is *named*. To settle: dump the datafield cells at `031770+X` for the entries with run = 20 and run = 171. |
| `SINTRAN/Devices/SCSI/scsi-open-last-block-read.md` ~93 | Buffer base "confirmed because the first `LDATX` returns `004000`" — a common value. Match a distinctive word instead (`052331₈` at base+1). |
| `SINTRAN/Devices/SCSI/scsi-disk-format.md` ~64, ~347 | "SMD and floppy have no such table" from an all-zero block — an all-zero region separates nothing, n=1 per device class. Settle from the SMD driver carve (no last-block read on that mount path). |
| `SINTRAN/Devices/HDLC/HDLC-Frame-Format-Reference.md` §5 ~232 | S-frame type field stated as bits 5-4, but the hex bases differ in bits 3-2 and the ASCII diagram places it differently again. RNR/REJ/SREJ were never observed on the wire. Cite ISO 7776 or capture an RR frame. |
| `tools/boot-floppy/SCSI-DEVICE-STRINGS.md` ~136, ~90 | "No device-type field exists" by exhaustive accounting — but every observed code is 0..10 and every trailing count 0 or 2, so a one-byte field in any of those lanes reads as zero padding in every record. Compare high bytes across a Direct/Sequential mix (`NDMICROP` vs `TANDBERG`/`EXABYTE`). |
| `tools/boot-floppy/FIRST-BOOT.md` ~11 | "(VERIFIED)" top-line answer outruns the body's own tags — the two cited facts eliminate MACM and establish `22!` reaches `SINTR`, but the file itself keeps three doors open at ~239, ~261, ~270. Scope the headline. |
| `tools/boot-floppy/versions/L-VSX-500-07/carve-crosscheck.md` ~21 | 30 segments promoted medium→high on "28 exact matches", where the match rule is itself tagged INFERRED one paragraph earlier — circular. Also rows at ~45 have empty page/madr columns inside the table headed "28 exact matches". Quote the floppy's macro legend expansion per parameter, as was done for `PXRO`. |
| `tools/boot-floppy/patches/README.md` ~127-148 | Three claims (number = patch identifier; trailing letter = revision; H/J ascending, K descending) each resting on monotonicity, one worked example, or "by inspection" over 9 files. The genuinely discriminating fact — sparsity, "H-223 has 124 records for numbers up to 184" — appears at ~145 and is not what the VERIFIED tag cites. |
| `tools/boot-floppy/MACM-DIALOGUE.md` ~480 | "Exactly one mark is installed per generation run" from a single install site; the command-table extent is explicitly COULD NOT DETERMINE at ~58, so the binary was not swept. |

---

## 5. Areas already audited — do not redo

- `SINTRAN/Filesystem/**`, `norskdata-ndfs/docs/`, `RetroFS/docs/`, `RetroCore/DOCS/` (NDFS)
- `SINTRAN/Devices/**` (SCSI, HDLC, Octobus)
- `SINTRAN/XMSG/DOC/**`, `SINTRAN/TAD/**`
- `SINTRAN/OS/**`, `SINTRAN/SINTRAN Structures/**`, `SINTRAN/ND500/**`, `SINTRAN/ND5000/**`
- `tools/boot-floppy/**`, `tools/sintran-segment-carver/**`

**Clean, and worth protecting as examples of the right pattern:**

- `tools/sintran-segment-carver/**` and `MON-CALL-INDEX.md` (87 VERIFIED tags, the densest file
  in the repo) — clean, because it *defines its vocabulary*: "worker VERIFIED" is stated to mean
  "this is the carved `MCTAB[N]` word", and the base/stride is pinned by a
  symbol-name-to-semantics correspondence that an off-by-one would destroy.
- `SINTRAN/Filesystem/on-disk-format/extended-info-block.md` — kernel-proven, and explicitly
  considers and rejects the XOR look-alike.
- `SINTRAN/Filesystem/on-disk-format/create-directory-placement.md` — a formula where the
  obvious alternative gives a demonstrably different answer, checked on three differently-sized
  images.
- `SINTRAN/Filesystem/code-logic/allocation.md` §4 — allocation direction resolved from
  `TESTP`'s `AAX -1`, with "never `AAX 1`" stated.
- `SINTRAN/Devices/Octobus/OCTOBUS-PROTOCOL-REFERENCE.md` — documents a claim being disproven
  live and retracted.
- `SINTRAN/XMSG/DOC/XMSG-HEADER-WORD6-IS-A-CHECKSUM-2026-07-31.md` — diagnoses this exact
  failure mode in its own earlier work: *"the 'seed' was the contribution of the fields nobody
  was varying."*

**Useful negative result:** tag density does not predict the failure mode. The two densest
files in the repo came back clean; `page-bitmap.md` had one bad tag and it cost four
implementations a corruption bug. **Precision of definition predicts it; volume does not.**

---

## 6. Verification commands

```
# Python  (norskdata-ndfs/ndfs-py)   472 fast + 3 growth (~35 min)
PYTHONPATH=src python -m pytest tests -q --ignore=tests/test_object_directory_growth.py

# C       (norskdata-ndfs/ndfs-c)    261
cmake --build build_win -j 8 && build_win/ndfs_tests

# TS      (norskdata-ndfs/ndfs-ts)   396
npm test

# RetroFS                            310 NDFS
dotnet test tests/RetroFS.Tests/RetroFS.Tests.csproj --filter "FullyQualifiedName~Ndfs"

# RetroCore                          24 NDFS
dotnet test Emulated.Tests/Emulated.Tests.csproj --filter "FullyQualifiedName~NDFS"
```

Regression fixtures produced by this work, both captured from real SINTRAN:

- `norskdata-ndfs/testdata/BIGDISK0-K-201users.img.gz` (also in `RetroFS.Tests/TestData/`) —
  201 users, one thrice-relocated overflow object block. 2.7 MB gzipped from 78 MB.
- `RetroCore/Emulated.Tests/NDFS/TestData/objblocks-1200p.img.gz` — an **Indexed** object file,
  so the Indexed-to-SubIndexed conversion is actually exercised.

Driving a live SINTRAN headless (used to capture the above) is documented in the
`sintran-console-driver` memory note: read `F:\RC\RonnyTest\<machine>\RetroCore.ini` for the
telnet and DAP ports, send ESC first, answer prompts one field at a time.

---

## 7. Not part of this audit

The XMSG/COSMOS protocol work is the next thread and is **not** yours. Do not touch
`SINTRAN/XMSG/SRC/**` or the `Xmsg.*` C# projects.
