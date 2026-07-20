# DOC-AUDIT — documentation staleness audit after the 2026-07-20 disc-boot findings

**Full path:** `E:\Dev\Ronny\NDInsight\tools\boot-floppy\DOC-AUDIT.md`

Scope: every document tree named in the audit brief — `tools/boot-floppy/`,
`SINTRAN/Filesystem/`, `tools/sintran-segment-carver/`, NDInsight top level,
`E:\Dev\Ronny\RetroFS\`, `E:\Dev\Ronny\norskdata-ndfs\docs\`, plus a sweep for
skills.

**Evidence rule.** Every row is tagged **[VERIFIED]** — I read the quoted text at
the quoted file and line — or **[INFERRED]**. Where I looked and found nothing
the entry says **NOT FOUND**. No document is called wrong without the offending
text quoted.

**Not audited (excluded by the brief):**
`E:\Dev\Ronny\NDInsight\tools\boot-floppy\README.md` (being rewritten by the main
agent) and `E:\Dev\Ronny\NDInsight\tools\boot-floppy\SCSI-DEVICE-STRINGS.md`
(another agent is writing it). Both were read for cross-reference only; findings
about `README.md` are listed in §5 for the main agent to fold in.

---

## 1. Audit table

Severity key: **WRONG** = asserts something the new evidence contradicts ·
**STALE** = was true when written, is now superseded or marked OPEN when it is
closed · **INCOMPLETE** = correct but missing material that now exists ·
**OK** = checked, no change needed.

| Document | What is wrong / stale (quoted) | Severity | Precise correction |
|---|---|---|---|
| `E:\Dev\Ronny\NDInsight\SINTRAN\Filesystem\boot-creation.md` §7.2, l.376-388 | *"The **origin of the code** is therefore a **prebuilt boot program**, not a live SINTRAN routine and not a checksum-wrapped BPUN."* and *"historically by a **stand-alone pack-to-pack / mass-storage copy** program (e.g. `COP-VERIFY`…), which copies page 0 (and the rest) verbatim from a master pack."* **[VERIFIED — read at those lines]** | **WRONG** | Replace with: the hard-disc page-0 program **is** written by a live SINTRAN routine — `PH-P2-OPPSTART.NPL` `FILL2`/`*PL011=*`, ND-100 addresses `045464`–`045626`, which MOVNPs the 192-word LOAD PROGRAM (`RELOA`..`LDEND`) plus a 744-word per-disc swap driver (`SWDSI` = `1350B`) into page 0 and patches ~16 parameter words, at every `@COLD-START` / `@RESTART-SYSTEM`. Cite `DISC-BOOTSTRAP.md` §2. Pack-to-pack copy is a *distribution* mechanism, not the authoring mechanism. |
| same file, §10 status table, l.490 | `| The utility/MODE file that originally *authored* the SMD page-0 program | **OPEN** | not pinned in manuals on hand |` **[VERIFIED]** | **STALE** | Change status **OPEN → VERIFIED**; evidence = `PH-P2-OPPSTART.NPL` `045464`-`045626` + the 176/192-word diff of `loadprogram-VSX-L-RELOA.bin` against a real SMD pack (16 diffs, all patched parameters). |
| same file, §10 status table, l.489 | `| Hard-disk page-0 blob is copied pack-to-pack when the system is installed | **INFERRED** | ND-10022S \`COP-VERIFY\`; no explicit "author" utility found |` **[VERIFIED]** | **STALE** | Delete or demote: the authoring path is now known. Keep pack-to-pack only as "also possible", not as the origin. |
| same file, §5 (Winchester) l.~296 *"**To get real Winchester bytes you need** one artifact: a **Winchester system disk image**…"*; §6 (SCSI) l.~283 *"**We have no real SCSI boot image**"* and l.334 *"**To get real SCSI bytes you need** a **SCSI system disk image**…"* **[VERIFIED]** | **WRONG** | We now have both, byte-verified, in `E:\Dev\Ronny\NDInsight\tools\boot-floppy\boot-sectors\` (`installed-winchester-0ab983b4.bin`, `installed-scsi-d90b55c5.bin`, plus `installed-smd-296ed770.bin`). Promote §5 and §6 from INFERRED to VERIFIED and point at `DISC-BOOT-SECTOR-ANATOMY.md`. |
| same file, §1 table, l.30-36 (page-0 byte map: `0..1999` boot, `2000..2015` ext-info, `2016..2047` label) **[VERIFIED]** | **INCOMPLETE** | Correct as far as it goes, but add the newly-verified structure of the boot half: words `0..0o35` are a **relocator** that runs in place and then copies body A and body B elsewhere; **linear disassembly past word `0o35` is wrong**; and the geometry block / device-class word live inside body A. Cross-reference `DISC-BOOT-SECTOR-ANATOMY.md` §5. |
| same file, §6 SCSI section, *"SCSI systems commonly **boot from a floppy first** (ALD 1560 …) - so a pure page-0 SCSI mass-storage bootstrap may not even be the normal boot path on those systems. This is **OPEN**."* **[VERIFIED]** | **STALE** | Real SCSI packs **do** carry a page-0 mass-storage bootstrap (five distinct SCSI page-0 families catalogued in `DISC-BOOT-SECTOR-ANATOMY.md` §6). Close the OPEN item on "does a page-0 SCSI bootstrap exist"; leave open only "which ALD an operator normally used". |
| `E:\Dev\Ronny\NDInsight\SINTRAN\Filesystem\create-directory-placement.md` l.49, l.104, l.182 | l.49: *"The `x16` split is `MPY 20` (octal 20 = 16) in `ALBIT` at 137710B. VERIFIED."* · l.104: *"The `x16 bits/word` bitmap sizing appears later in `ALBIT` (137710B `MPY 20`, octal 20 = 16)…"* · l.182: `` | bit-file span = `ceil(pages/16384)`, 1 bit/page | **VERIFIED** | `ALBIT` 137710B `MPY 20` (=16) … | `` **[VERIFIED — all three read]** | **WRONG** | `MPY` on the ND-100 has **no immediate form**; the `20` is not an operand value. Both `MPY` sites at/near `137710B` fetch the literal `000011B` (= 9) held at `137730B`, and they belong to **bad-page relocation (±1 track)**, not to bitmap sizing. Strike "MPY 20 = 16" from all three places. The `ceil(pages/16384)` result may still be right, but it must be re-derived from another site or re-tagged **INFERRED**. |
| `E:\Dev\Ronny\NDInsight\SINTRAN\Filesystem\create-directory.md` l.195 | *"…and the bits/word split (`MPY 20` = octal 20 = **16 bits per bitmap word**, VERIFIED opcode at 137710B; `RDIV` for the word/bit index)."* **[VERIFIED]** | **WRONG** | Same correction as above. Note the surrounding three-pattern bad-page loop description (l.196-205) is consistent with the *relocation* reading, which strengthens the correction. |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\CARVED-DISC-SUPPORT.md` §0 summary l.30 | `| \`MSTYP\` numeric value | **NOT FOUND** in any carved symbol table (K03/L07/M06) or NPL listing. The open question is *not* resolved; see §7. |` **[VERIFIED]** | **STALE** | The *kernel-side* NOT FOUND is correct and should stay (the kernel uses `SWTYP`, 7..36B). But the question **is** now resolved on the MACM side: `MACM-DIALOGUE.md` §6.2/§6.6 gives the full menu→MSTYP→record→mark mapping. Reword to: "MSTYP is a **MACM-side generation input only**; it does not exist in the kernel. Full decode in `MACM-DIALOGUE.md` §6." |
| same file, §7 heading l.864 *"## 7. `MSTYP` — the open question is NOT resolved"* **[VERIFIED]** | **STALE** | Retitle to *"§7. `MSTYP` is not a kernel symbol — it is a MACM generation input"* and add a forward link to `MACM-DIALOGUE.md` §6. The body's evidence (no `MSTYP` in K03/L07/M06 symbol tables, only `CMSTY`; `SWTYP` 7..36B is the kernel scheme) is **correct and should be preserved** — it is the proof that MSTYP ≠ SWTYP. |
| same file, §6.2 l.757-759 | *"`DUMP-BOOTSTRAP` — the subcommand that writes the boot block to the disc — is at K `153367B`, L `117361B`, M `125575B`. **[VERIFIED]** This is the mechanism that writes the disc boot area; `@CREATE-DIRECTORY` does not (§5)."* **[VERIFIED]** | **WRONG (over-claim)** | The `[VERIFIED]` tag legitimately covers *the table entry and its handler address*. It does **not** cover "this is the mechanism that writes the disc boot area" — that is an inference, and it is contradicted by `DISC-BOOTSTRAP.md` l.454-456 and by ND-60.128.5 p.97 ("Allowed only on floppy disk"). Split the claim: keep the addresses as VERIFIED; re-tag the mechanism sentence **[INFERRED — disputed, see §2 of DOC-AUDIT]**. The "and `@CREATE-DIRECTORY` does not" half is independently supported and can stay. |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\CARVED-DISC-SUPPORT.md` §1.5 l.170-179 | *"A parallel pass over `006-S3FS` concluded 'no repeating disc-geometry table exists in SINTRAN L07'. That conclusion is **wrong**…"* **[VERIFIED]** | **OK** | Already carries the correction. **The originating document was NOT FOUND** — a repo-wide grep for `repeating disc-geometry` / `no repeating` matched only this file and an unrelated line in `SINTRAN\Devices\SCSI\SCSI-WRITE-LOOP-INVESTIGATION-2026-07-17.md:46`. The erroneous conclusion appears to have lived only in an agent transcript, so there is nothing else to fix. |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\INSTALL-PROCEDURE.md` §0 l.19 and l.644 | *"Extracted read-only with `ndtool.exe -x -p -o <outdir> <image>`"* and the same command in the reproduction recipe **[VERIFIED]** | **WRONG** | `-p` strips bit 7 of every byte and **corrupts binaries** (`MACM-1718L:BPUN` is listed as extracted this way). This document's own sibling states it: `DISC-BOOTSTRAP.md:273` — *"`ndtool -x -p` **destroys** the `:DATA` stream … Extract with **`ndtool -x -o <dir> <image>`** (no `-p`)."* Change both occurrences to `ndtool -x -o <outdir> <image>`, and add a one-line note that `-p` is for **text only** (`.PATC`, `.SYMB`). |
| same file, l.225-238 | *"`MSTYP` and `MSTYQ` are **defined by MACM**… [INFERRED]"* · *"The exact MSTYP → mark mapping (which MSTYP numbers select `BD288` rather than …)"* left open **[VERIFIED]** | **STALE** | Promote **[INFERRED] → [VERIFIED]** and replace the open item with a pointer to `MACM-DIALOGUE.md` §6.3/§6.4/§6.6: pointer table `ram:9715` indexed by MSTYP → 11/12-word record; words 10/11 point at the packed marks at `ram:9807`-`9816` (`DRUM`, —, `REMOV`, `FIXED`, `BD288`, `BDFIX`, `W8INC`, `SCASI`); R/F is asked only for MSTYP 2 and 6. |
| same file, §3.2 l.493-496 | *"That this is the `MSTYP` table (rather than the other one) is **[INFERRED]** from the adjacent column header…"* **[VERIFIED]** | **STALE** | Now **VERIFIED**: `MACM-DIALOGUE.md` §6.6 reaches the same 21 rows independently, from the record pointer table indexed by MSTYP. Change the tag and cite §6.6. |
| same file, §2.1 l.373-374 (`MSTYP` = 23₈, `DISK TYPE` = 24₈ for SCSI) | — | **OK** | **[VERIFIED]** — matches `MACM-DIALOGUE.md` §6.1/§6.2 exactly (menu answer 24₈ → MSTYP 23₈). No contradiction; this doc already keeps the two numbering schemes apart. Worth adding an explicit cross-link so a future reader does not "fix" one of them. |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\device-geometry.md`, the "Distinct (MSTYP, DEVNO) catalogue" table | every MSTYP cell reads `*unresolved* | *unknown*` **[VERIFIED — read at the top of the file]** | **STALE** | Resolvable now. The `)9BYTT` streams carry DEVNO + marks, and `MACM-DIALOGUE.md` §6.6 maps mark+DEVNO back to MSTYP: `1540` + `BD288`/`BDFIX` → MSTYP 3-7 group (BDFIX = the FIXED variant of MSTYP 6); `500` + `W8INC` → MSTYP 10₈; `500` + `REMOV`/`FIXED` → MSTYP 2; `144300` + `SCASI` → MSTYP 23₈. Regenerate `tools/extract_9bytt.py` output with that lookup, or at minimum footnote it. |
| same file, header *"Generated by `tools/extract_9bytt.py` from the `SINTRAN*:DATA` MACM generation streams"* | the generator's own comment (`tools/extract_9bytt.py:89`) documents `ndtool -x -p -o <dir>\<key> <image>` **[VERIFIED]** | **INCOMPLETE** | The values here are octal ASCII so parity-stripping did not change them, but the provenance line should say so explicitly, or the extraction should be redone without `-p`. Add: *"the `:DATA` streams were extracted with `-p`; that is safe for the ASCII `)9BYTT` parameters parsed here but corrupts the embedded `)9READ` binary records — see `DISC-BOOTSTRAP.md` §6."* |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\DISC-BOOTSTRAP.md` §3 l.114-116 | *"word 0200B .. 1747B   swap driver    (SWDSI       = 1350B = 744 words)   per disc type"* — presented as the layout for all disc types **[VERIFIED]** | **INCOMPLETE** | `DISC-BOOT-SECTOR-ANATOMY.md:196` shows page word `0o34` (the body-B word count) is `001350` (744) for SMD and SCSI but **`001360` (752)** for Winchester. Add a note that the count is a *patched* word, not a universal constant; 744 is the L-revision SMD/SCSI value. |
| same file, l.454-456 (`[NOT FOUND]` an operator command that writes a hard-disc bootstrap; `DUMP-BOOTSTRAP` floppy-only) | **[VERIFIED]** | **INCOMPLETE** | Not wrong, but it must now explicitly acknowledge and rebut the opposite claim in `CARVED-DISC-SUPPORT.md` §6.2. See §2 below. |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\versions\L-VSX-500-07\README.md` l.52-53 | ```ndtool -x -p -o D:\ND\extract\VSXL1 D:\ND\S\VSXL1.IMG``` — `VSXL1.IMG` holds `MACM-1718L:BPUN`, a binary **[VERIFIED]** | **WRONG** | Drop `-p` from both lines. |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\patches\inventory.md` l.9 | `ndtool.exe -x -p -o <dir> <image>` **[VERIFIED]** | **INCOMPLETE** | Patch floppies are `.PATC` text so `-p` is correct **here**; add "text only — never for `:BPUN`/`:DATA`" so the recipe is not copied to a binary. |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\patches\README.md` l.37 (*"100.0 % printable ASCII after `ndtool -p` (parity strip)"*) | — | **OK** | **[VERIFIED]** — legitimate text-only use of `-p`. |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\tools\extract_media_install_evidence.py` l.25 and `tools\extract_9bytt.py` l.89 | both document `ndtool -x -p` in their headers **[VERIFIED]** | **INCOMPLETE** | Same footnote as `device-geometry.md`: safe for the ASCII they parse, wrong as a general recipe. |
| `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\segment-facts.json` segnum 61 | `"name": "S3SXROU"` with `"description": "Save of XMSG kernel"` — identical to segnum 60 (`S3SXMK`) **[VERIFIED — read at lines 904-936]** | **WRONG** | **FIXED — see §5.** Set to `"Save of XMSG xrouter segment"`, matching the file's own wording for segnum 63 (`S3XROU`, `"XMSG xrouter segment"`, and `segle` 40 in both, vs `segle` 24 for 60/62). |
| `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\M-VSX-500\segment-facts.json` segnum 61 | identical defect **[VERIFIED]** | **WRONG** | **FIXED — see §5.** |
| `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\K-VSX-500\segment-facts.json` | — | **OK** | **[VERIFIED]** — no segnum 60/61 entries present; nothing to fix. |
| `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\README.md` | grep for `segment 61` / `S3SXROU` / `disc` / `geometry` returned nothing **[VERIFIED]** | **INCOMPLETE** | The carver README says nothing about the DPIT segment holding `DISPE` + the `DTxxx` geometry records, which is now the single most-cited fact from the carve. Add one line pointing at `boot-floppy\CARVED-DISC-SUPPORT.md` §1. |
| `E:\Dev\Ronny\norskdata-ndfs\docs\NDFS-FORMAT.md` "Boot Sector Formats", l.398-400 *"The first 1024 bytes of page 0 may contain boot code"* | contradicted 65 lines later by its own note l.463-465: *"'The first 1024 bytes are the boot sector' is a simplification… bytes **0–1999**"* **[VERIFIED]** | **INCOMPLETE** | Fix the lead sentence to say bytes **0–1999**; keep the note. Minor self-contradiction inside one document. |
| same file, controller-signature table l.440-445 | `| SCSI (NCR-5386) | \`IOXT\` (octal 150415) — device number taken from the **T register** |` **[VERIFIED]** | **OK** | Confirmed by `DISC-BOOT-SECTOR-ANATOMY.md:418-422` (real SCSI page 0s: `IOXT` only, class word 3, geometry all zero). **Do not "correct" this using the MACM finding** — MACM having zero `IOXT` is about the *generation-time assembler*, not the boot page. |
| same file, controller-signature table (no SCSI discriminator beyond `IOXT`) | — | **INCOMPLETE** | Add the two stronger, data-driven discriminators found today: the **device-class word** (SMD = 1, Winchester = 2, SCSI = 3, at page word `0o67`/`0o77`) and the **9-word geometry block anchored by `0o1000`** (SMD 512/18/90/822/821, Winchester 512/9/72/1021/1011, SCSI all zeros). These classify without disassembling. Also add the `PACK-ONE` label false-positive: bytes 2000-2047 are the NDFS volume label and read as "IOX 3154" in a whole-page scan (`DISC-BOOT-SECTOR-ANATOMY.md:91-100`). |
| same file, "Disk Image Templates" table l.477-484 | four templates only; **no SCSI row** **[VERIFIED]** | **INCOMPLETE** | Add a SCSI template. Note the geometry cannot be tabulated (SCSI capacity is interrogated at run time — `INQUIRY` + `READ CAPACITY(10)`, CDB word `022400`, plus the vendor control record on the last block); only the block-size rule is fixed: > 1, an exact power of two, and it must fit in 16 bits (`RSZER`/`ILRCS`). |
| same file, l.479-483 (SMD bit block **18,468**, Winchester device **36,864**) | — | **OK** | **[VERIFIED]** — both already carry the RetroFS-corrected values with the reasoning. |
| `E:\Dev\Ronny\norskdata-ndfs\docs\KERNEL-VERIFIED-CORRECTIONS.md` | contains no boot-loader section; kernel anchors list stops at `ALBIT` **[VERIFIED — read l.1-50, grepped whole file]** | **INCOMPLETE** | Add two corrections: (a) the `try_parse_bpun()` FLOMON-detection bug (below); (b) that `ndtool --create` emits a non-bootable image. Both are kernel/format-level facts that belong in the file that claims *"Where this document and any older doc disagree, this one wins."* |
| `E:\Dev\Ronny\norskdata-ndfs\ndfs-c\src\boot_loader.c` l.87-88, l.104 | `/* Check for FLOMON marker: address=0, count=0, checksum=0 */` then `if (address == 0 && count == 0 && file_checksum == 0)` and later `word_count = data[pos];` **[VERIFIED — read at those lines]** | **WRONG (code + its comment)** | In a **byte-expanded** record those 6 bytes are the first three *expanded* header bytes (`00 00 00` whenever the load address is 0 and the count < 256), so every normal BPUN with load address 0 is misdetected as FLOMON; `data[pos]` is then a pad byte, so the function returns 0 bytes of boot code. The word count is at `data[pos+1]`. Correct fix: **de-expand first, then parse**. Verified today: with that order all 102 boot floppies' checksums verify. The header's doc comment must be corrected with the code. |
| `E:\Dev\Ronny\norskdata-ndfs\ndfs-c\src\image_creator.c` + `cmd_create.c` | `grep -c boot image_creator.c` → **0**; `cmd_create.c` **NOT FOUND** in `ndfs-c/src/` (files present: `backend_stdio.c bit_file.c block_pointer.c boot_loader.c endian_util.c filesystem.c image_creator.c master_block.c nd_time.c ndfs_name.c object_entry.c parity.c sintran.c user_entry.c user_friend.c wildmatch.c xat.c`) **[VERIFIED]** | **INCOMPLETE (undocumented limitation)** | No doc anywhere states that `ndtool --create` produces a **non-bootable** image (page 0 all zeros) and that the template enum has no SCSI entry. `E:\Dev\Ronny\norskdata-ndfs\README.md` l.112 advertises `ndtool --create floppy360 --name MYDISK new.ndfs` with no such caveat. Add the caveat to `README.md` and to `NDFS-FORMAT.md` §"Disk Image Templates". *(The `cmd_create.c` path in the brief could not be located; the create path in this tree is `image_creator.c`.)* |
| `E:\Dev\Ronny\norskdata-ndfs\README.md` l.110-111 | `ndtool -x -p -d -l -o output/ disk.ndfs     # Extract all, strip parity, lowercase` **[VERIFIED]** | **WRONG (as blanket advice)** | This is the top-level "how to extract" example and it will silently corrupt every `:BPUN`, `:PROG` and `:DATA` on the disk. Change the example to `ndtool -x -d -l -o output/ disk.ndfs` and add a second line: `ndtool -x -p … # text files only (.SYMB/.PATC) — -p clears bit 7 and destroys binaries`. |
| `E:\Dev\Ronny\RetroFS\RetroCommander\RetroCommanderUI\CreateNdfsImageDialog.axaml.cs` l.147 | `NdfsImageTemplate.Winchester74MB => (36396u, 36360u, "Winchester"),` **[VERIFIED]** | **WRONG** | Still carries the pre-correction `36360`, i.e. a device smaller than the declared 36,396-page capacity. `RetroFS\src\RetroFS.NDFS\Creation\NdfsTemplateSpec.cs` was fixed; the UI dialog was not. Not a doc — flagged because it silently defeats the documented fix. |
| `E:\Dev\Ronny\RetroFS\ai-docs\NDFS-BLOCK0-HEADER-MAP.md` l.312 (`uint bitFileBlock = ndfsPages / 2;  // 18472 (bitmap at 50%)`), `ai-docs\NDFS-IMAGE-CREATOR-ENHANCED-DESIGN.md` l.191 (`BitFileBlock = 18472`) and l.206 (`FileBlocks = 36360`), `ai-docs\NDFS-IMAGE-CREATOR-DESIGN.md` l.244 (`TotalPages = 36360`), `ai-docs\NDFS-IMAGE-CREATION-…-COMPLETE.md` l.159 (`(36396u, 36360u, …)`) **[VERIFIED — all five read]** | **STALE** | These are superseded design docs still quoting the pre-kernel-correction constants. Cheapest correct fix: add a two-line **superseded-by** banner at the top of each pointing to `RetroFS\ai-docs\NDFS-KERNEL-VERIFIED-FINDINGS.md` (which already has 18468 at l.108) and to `norskdata-ndfs\docs\KERNEL-VERIFIED-CORRECTIONS.md`. Do not silently edit the numbers — they are historical design records. |
| `E:\Dev\Ronny\RetroFS\ai-docs\NDFS-BOOT-SECTOR-ANALYSIS.md` | classifies boot sectors by preamble length and FLOMON word count (*"FloMon Word Count: 0 (no boot code)"*, l.40/76; *"Bootstrap loader in preamble (~72-78 bytes)"*, l.240) **[VERIFIED]** | **STALE** | The "word count 0 = no boot code" conclusion is the reader-side symptom of the `try_parse_bpun()` bug above, not a property of the disks. Add a banner: *"word-count-0 readings in this report were produced by a parser that read the count from the wrong byte; see `norskdata-ndfs\ndfs-c\src\boot_loader.c`."* Do not trust the per-image conclusions until re-run. |
| `E:\Dev\Ronny\RetroFS\CLAUDE.md` | grepped for boot/geometry/disk-type guidance; **NOT FOUND** — no NDFS boot-sector or geometry rules present **[VERIFIED]** | **INCOMPLETE** | Add two one-line rules: (1) NDFS template geometry is **measured, never computed** — bit block = `9*floor(floor(pages/2)/9)`, device pages ≥ declared pages; (2) never extract ND binaries with parity stripping. |
| `E:\Dev\Ronny\NDInsight\CLAUDE.md` | grepped for boot / disc / MACM / geometry; **NOT FOUND** **[VERIFIED]** | **INCOMPLETE** | Add a pointer to `tools\boot-floppy\` as the authority for disc boot sectors / MACM generation, and the one-line rule "`ndtool -p` is for text only". |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\MACM-DIALOGUE.md` | — | **OK** | **[VERIFIED]** — already states `ram:8342` (l.173-174, three independent anchors) and explicitly refutes `834c` at l.505-507. Also correctly separates `)REDEF`/`)HENT` (real command-table entries, `ram:872a`/`ram:8760`) from `10,0$` / `22!`. Nothing to change. |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\DISC-BOOT-SECTOR-ANATOMY.md` | — | **OK** | **[VERIFIED]** — relocator/body-A/body-B model, the `PACK-ONE` false positive (l.91-100), the class word (l.334-342) and the geometry block (l.352-390) are all present and internally consistent. |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\CATALOGUE.md` | — | **OK** | **[VERIFIED]** — pure media inventory; nothing today's findings touch. |
| `E:\Dev\Ronny\NDInsight\SINTRAN\Filesystem\on-disk-format\boot-sector.md` | referenced from `boot-creation.md` §1 as the page-0 map and boot-format detector | **INCOMPLETE** *(not opened line-by-line — flagged from its callers)* | Should receive the same three additions as `NDFS-FORMAT.md`: relocator-not-program, class word, geometry block. **[INFERRED]** from the cross-references at `boot-creation.md` l.9-11 and l.286-288; **not** read directly. |

---

## 2. Contradictions **between today's own boot-floppy documents**

These were written by agents working in parallel who could not see each other's
results.

### 2.1 Who writes the hard-disc boot area — `DUMP-BOOTSTRAP` or the cold-start kernel routine?

* `CARVED-DISC-SUPPORT.md` l.757-759: *"`DUMP-BOOTSTRAP` … **[VERIFIED]** This is
  the mechanism that writes the disc boot area."*
* `DISC-BOOTSTRAP.md` l.454-456: *"**[NOT FOUND]** an `@DEVICE-FUNCTION`-style
  operator command that writes a *hard-disc* bootstrap. `DUMP-BOOTSTRAP` is
  documented as floppy-only (ND-60.128.5 p.97) and that remains the case."*

**`DISC-BOOTSTRAP.md` is the better-supported document.** It has a named kernel
routine with an address range (`PH-P2-OPPSTART.NPL` `045464`-`045626`), a source
listing, and a 176/192-word byte diff against a real pack. `CARVED-DISC-SUPPORT.md`
has only a command-table entry and its handler address — which proves the
subcommand *exists*, not what it writes, and the manual explicitly restricts it to
floppies. **Both documents agree on the negative half** (`@CREATE-DIRECTORY` does
not write the boot area), and that half is safe to state everywhere.

**Proposed resolution:** demote the `CARVED-DISC-SUPPORT.md` sentence to
`[INFERRED]`; keep `DISC-BOOTSTRAP.md` as written; add a cross-link in both. The
two are not necessarily exclusive — `DUMP-BOOTSTRAP` could be an operator-driven
alternate path — but nothing on this machine shows it writing a hard disc.

### 2.2 `-p` on extraction

* `DISC-BOOTSTRAP.md` l.273: *"`ndtool -x -p` **destroys** the `:DATA` stream …
  Extract with `ndtool -x -o <dir> <image>` (no `-p`)."*
* `INSTALL-PROCEDURE.md` l.19 and l.644, `versions\L-VSX-500-07\README.md` l.52-53,
  `patches\inventory.md` l.9, `tools\extract_9bytt.py` l.89,
  `tools\extract_media_install_evidence.py` l.25, `README.md` l.93: all still
  publish `ndtool -x -p`.

**`DISC-BOOTSTRAP.md` is correct** — it is the only one that tested it (all 22
`)9READ` BPUN checksums verify only without `-p`). The `-p` uses on pure-text
`.PATC` files are harmless; the uses on `VSXL1.IMG` (which contains
`MACM-1718L:BPUN`) and on `:DATA` are not.

### 2.3 `MSTYP` storage address — resolved, no live contradiction

The brief flagged `834c` vs `8342`. **A grep of the whole `boot-floppy` tree for
`834c` returns zero hits.** The only surviving statement is
`MACM-DIALOGUE.md` l.505-507, which already records `8342`-`834b` plus `833b` and
explicitly rejects `834c`. **No edit needed** — but no other document should be
allowed to reintroduce `834c`.

### 2.4 `MSTYP` vs `SWTYP` — apparent, not real

`CARVED-DISC-SUPPORT.md` §7 says MSTYP is NOT FOUND; `MACM-DIALOGUE.md` §6 decodes
it fully. **Both are right about different things**: MSTYP exists only in MACM
(generation input, `ram:8342`); the kernel uses `SWTYP` (7..36B) indexing `MDISCS`
and `DISPE`. Neither doc currently says so plainly. Add the sentence *"MSTYP is a
MACM-side generation input; the kernel's equivalent is `SWTYP`. They are different
numbering schemes and must never be equated"* to **both**.

### 2.5 Swap-driver size 744 vs 752 words

`DISC-BOOTSTRAP.md` l.111/115 gives `SWDSI = 1350B = 744 words` as *the* size;
`DISC-BOOT-SECTOR-ANATOMY.md` l.196 shows page word `0o34` = `001360` (**752**) on
Winchester. **The anatomy document is right** — it read the value out of real
packs, three device classes side by side. 744 is the SMD/SCSI value for the L
revision. `DISC-BOOTSTRAP.md` should say "744 for SMD/SCSI in the L build; the
count is itself a patched word".

### 2.6 SCSI and `IOXT` — a trap, not a contradiction

`NDFS-FORMAT.md` and `DISC-BOOT-SECTOR-ANATOMY.md` both say the SCSI boot page uses
`IOXT` (42 occurrences in body B, `DISC-BOOT-SECTOR-ANATOMY.md` l.304-310). Today's
MACM finding says `IOXT` appears **zero** times in MACM. **Both are true and about
different programs** — MACM is the generation-time assembler, the boot page is
kernel-emitted code. Whoever edits these next must not "reconcile" them. Add a
one-line warning in both.

---

## 3. Top 10 edits by value

1. **`SINTRAN\Filesystem\boot-creation.md` §7.2 + §10** — replace the "prebuilt
   blob, copied pack-to-pack, author OPEN" story with the kernel-writes-it fact.
   This is the single largest wrong conclusion in the whole corpus and it is the
   document other trees link to.
2. **`SINTRAN\Filesystem\create-directory-placement.md` (×3) and
   `create-directory.md` (×1)** — remove `MPY 20 = 16 bits/word`. It is an
   impossible reading of the instruction, and it is tagged VERIFIED, which makes it
   maximally dangerous.
3. **`norskdata-ndfs\ndfs-c\src\boot_loader.c` `try_parse_bpun()` + header** — fix
   the FLOMON misdetection (de-expand, then parse; count at `data[pos+1]`). It
   silently returns "no boot code" for the whole boot-floppy corpus.
4. **`norskdata-ndfs\README.md` l.110 and `boot-floppy\INSTALL-PROCEDURE.md`
   l.19/644** — stop recommending `ndtool -x -p` for binaries. Highest
   blast-radius doc bug: it corrupts evidence at the point of collection.
5. **`boot-floppy\CARVED-DISC-SUPPORT.md` §6.2** — demote the `DUMP-BOOTSTRAP`
   mechanism sentence from `[VERIFIED]` to `[INFERRED]`; it is the only
   agent-vs-agent contradiction that is currently mis-tagged as proven.
6. **`boot-floppy\CARVED-DISC-SUPPORT.md` §0 + §7** — reframe MSTYP from
   "unresolved" to "MACM-side only; kernel uses SWTYP", and link `MACM-DIALOGUE.md`
   §6. Closes the loudest open question in the tree.
7. **`boot-floppy\INSTALL-PROCEDURE.md` §3.2 and l.225-238** — promote the MSTYP
   inferences to VERIFIED with the `MACM-DIALOGUE.md` §6.6 citation.
8. **`norskdata-ndfs\docs\NDFS-FORMAT.md` boot section** — add the class word, the
   `0o1000` geometry block, the relocator warning and the `PACK-ONE`
   false-positive; add a SCSI template row with the run-time-capacity caveat.
9. **`boot-floppy\device-geometry.md`** — fill in the four `*unresolved*` MSTYP
   cells from the mark+DEVNO lookup.
10. **`RetroFS\RetroCommander\RetroCommanderUI\CreateNdfsImageDialog.axaml.cs`
    l.147** — `36360` → `36864`. One token; it is the last place the corrected
    Winchester geometry has not landed.

---

## 4. Skills recommendation

**Sweep result [VERIFIED]:** `find` for `SKILL.md` across `NDInsight`, `RetroFS`
and `norskdata-ndfs` returned **none**. The `.claude` directories that exist
(`NDInsight\.claude`, `NDInsight\SINTRAN\.claude`,
`NDInsight\SINTRAN\Devices\SCSI\.claude`, `RetroFS\.claude`,
`RetroFS\src\.claude`, `norskdata-ndfs\.claude`, and four more) contain only
`settings.local.json` / `scheduled_tasks.lock`. **All skills in play are the
user-level ones under `~\.claude\skills\`.** Recommendations follow; **nothing was
created.**

### 4.1 NEW skill — `nd-disc-boot` (recommended)

Nothing in the existing roster covers the hard-disc page-0 boot sector, and it is
now the best-evidenced and most trap-laden subject on the machine. A skill would
carry: the mass-storage load contract (1 KW to address 0, jump 0); that page 0 is
a **relocator** and linear disassembly past word `0o35` is wrong; that the kernel
writes it at every `@COLD-START` from `PH-P2-OPPSTART.NPL` `045464`-`045626`; the
patched-word list (`KLIOX`, `KLHDE`, `YSWTY`, `NOBLK`, `DYBLS`, `LDRAD`, `ADR2B`,
`KLRC1`, `KBLSZ`); the device-class word and the `0o1000`-anchored geometry block
as the correct classifiers; the `PACK-ONE`-label false positive; and the
MACM-`IOXT`-vs-boot-page-`IOXT` trap. Every one of these has already cost an agent
a wrong conclusion in the documents audited above, which is exactly the bar a
skill should clear.

### 4.2 NEW skill — `sintran-generation` (MACM) (recommended, lower priority)

The MACM generation dialogue is a self-contained, byte-verified body of knowledge
with its own traps: MSTYP lives at `ram:8342` (not `834c`), MSTYP is **not** the
kernel's `SWTYP`, the menu answer (0-24₈) is **not** MSTYP, `10,0$` and `22!` are
console commands MACM merely prints while `)REDEF` and `)HENT` are real, and
`F`(ixed) sets bit 15 in **four** words. It is currently spread across
`MACM-DIALOGUE.md`, `INSTALL-PROCEDURE.md` and `device-geometry.md`. Justified,
but only after those three documents are reconciled — a skill built on top of an
unreconciled corpus would encode the disagreement.

### 4.3 AMEND — `sintran-carving` (recommended)

Its description covers carving segments and MON-call analysis and it advertises
"the traps that previously produced months of wrong analysis". Two new traps
belong in it and are not there: (a) the disc-geometry table (`DISPE` + `DTxxx`,
9-word records) lives in the **DPIT** segment (base `4000B`), **not** in
`006-S3FS` — a parallel pass concluded it did not exist at all; (b) `MSTYP` is not
a kernel symbol, so searching the kernel symbol tables for it will always fail and
the answer is `SWTYP` (7..36B). Also worth adding the device-name table shape
(17-word entries, 266 units, word 8:9 = 32-bit page count, word 16 → `DTxxx`,
word 14 = logical device number) and `MDISCS` at `041440B`.

### 4.4 AMEND — `scsi-debug` (recommended)

It covers the SINTRAN SCSI driver and the RetroCore controller but not the
capacity model, which is where the audited docs went wrong. Add: SCSI geometry is
**never tabulated** (`DTSSS`, index 36B, all-zero; all 112 `DISC-n-SCSI-m` name
entries size 0); capacity comes from `INQUIRY` + `READ CAPACITY(10)` (CDB word
`022400`) plus the XOR-checksummed vendor control record on the last block (32-bit
`UHLIM`, requires `2 < NPART <= 10`); the only hard limit is block size — > 1, an
exact power of two, fits in 16 bits (`RSZER`/`ILRCS`); and **no** maximum disc size
is enforced in `ALBIT` (`137500B`) or `CRDIR` (`136741B`).

### 4.5 No change — `nd100-asm`, `nd100-ghidra`, `sintran-print`, `xmsg-decode`

Checked against today's findings; nothing in them is contradicted. One optional
addition to `nd100-asm`: **`MPY` has no immediate form** — the `MPY 20` misreading
in `create-directory*.md` is precisely the error that line would have prevented.

---

## 5. Trivial factual errors fixed directly

Only unambiguous copy-paste defects were touched. Two edits, one field each.

| File | Field | Before | After |
|---|---|---|---|
| `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\segment-facts.json` | `segments[segnum=61 / name="S3SXROU"].description` | `"Save of XMSG kernel"` | `"Save of XMSG xrouter segment"` |
| `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\M-VSX-500\segment-facts.json` | `segments[segnum=61 / name="S3SXROU"].description` | `"Save of XMSG kernel"` | `"Save of XMSG xrouter segment"` |

**Justification [VERIFIED]:** in the same file, segnum 63 is `"name": "S3XROU"`,
`"description": "XMSG xrouter segment"`, `"segle": 40`; segnum 62 is
`"name": "S3XMK"`, `"description": "XMSG kernel"`, `"segle": 24`. Segnum 61
(`S3SXROU`) has `segle` **40** and segnum 60 (`S3SXMK`) has `segle` **24** — the
save segments match their live counterparts on both name and length, so 61's
description was a copy of 60's. The replacement wording is the file's own phrasing
for segment 63, chosen over the brief's "Save of XMSG XROUT" for internal
consistency. `K-VSX-500\segment-facts.json` has no segnum 60/61 and was not
touched. Independently corroborated by
`E:\Dev\Ronny\NDInsight\tools\boot-floppy\versions\L-VSX-500-07\README.md`
("Segment **61**'s description is a copy-paste of segment 60's — the floppy shows
it is XMSG **XROUT**, not the kernel").

Nothing else was changed. All disc images and everything under `D:\ND\` were
opened read-only or not at all; no process was stopped.
