# FIRST-BOOT — how disc page 0 first reaches a VIRGIN, never-booted ND pack

Full path: `E:\Dev\Ronny\NDInsight\tools\boot-floppy\FIRST-BOOT.md`

**The question.** Every prior finding is circular for a blank pack: a *running*
SINTRAN writes its own disc page 0 at cold-start (routine `PL011`/`FILL2`,
`PH-P2-OPPSTART.NPL`), but that requires SINTRAN to already be running from that
disc. So who writes page 0 the **first** time, onto a freshly formatted pack
whose page 0 is all zero?

**The answer, one line (VERIFIED).** The first page-0 write is done by the
**generated SINTRAN kernel running *in core*, started by the operator's `22!`
command** — not by MACM, and not by any disc-resident bootstrap. `22!` is the
MOPC "start at address 22B" command; word 22B of the in-core kernel is
`JMP I` → `SINTR` (the cold-start entry); `SINTR`'s cold path runs the page-0
writer `PL011`. MACM only puts the *system data* (SAVE area) on the pack; it
never writes page 0. The loop is broken because the very first kernel executes
from core, loaded by MACM, not from the pack's (empty) page 0.

Every statement below is tagged **[VERIFIED]** (exact file/address + quoted
bytes/disassembly/source), **[INFERRED]** (reasoning shown, stated as such),
**[MANUAL]** (corroboration from an OCR'd manual, used only to confirm something
already found in code/bytes), or **NOT FOUND / COULD NOT DETERMINE**.

This document builds on and does not restate the four sibling docs in this
folder: `DISC-BOOTSTRAP.md` (who writes page 0 and its byte layout),
`MACM-DIALOGUE.md` (the MACM binary), `INSTALL-PROCEDURE.md` (the media),
`CARVED-DISC-SUPPORT.md` (the kernel disc tables). It closes their shared open
item "how page 0 gets onto a brand-new, never-booted pack".

---

## 1. The virgin-pack sequence, step by step

Target of the walk-through: an SMD or Winchester pack (the disc types MACM can
itself drive). The SCSI caveat is in §6.

| # | actor | action | status |
|---|---|---|---|
| S0 | operator | Format the pack (all of page 0 is zero). Set the ALD thumbwheel to the floppy device; MASTER CLEAR; press LOAD (≡ MOPC `$`/`&`, ALD-driven bootstrap load). Microcode reads 1 K from the floppy = the FLOMON boot record. | [MANUAL] ND-06.015.02 §7.2.5.2/§7.2.5.3; FLOMON itself **out of scope** |
| S1 | operator / FLOMON | `10,0$` — "LOAD SINTRAN FROM DISKETTE". FLOMON pulls **MACM** off the diskette into high core and runs it. MACM reads the `SINTRAN*:DATA` stream as its command input. | crib string [VERIFIED]; `10,0$` semantics see §4 |
| S2 | MACM | MSTYP dialogue: operator answers `DISK TYPE`/`ENTER MSTYP` (+ `R/F` for types 2,6). MACM bakes the device number, the disc-type library mark (`BD288`/`W8INC`/`SCASI`/…) and the geometry words into the SINTRAN image via `)9BYTT`. | [VERIFIED] `MACM-DIALOGUE.md` §5–6 |
| S3 | MACM | `)9READ` loads the 22 checksummed BPUN blocks — the whole SINTRAN system — into core; patch macros apply; **`)9SAVE` writes the SINTRAN SAVE area to the pack** through `IOX 1543/1545` (SMD) or `IOX 500` (Winchester). | [VERIFIED] `MACM-DIALOGUE.md` §7.2; `DISC-BOOTSTRAP.md` §6 |
| S3a | — | **Page 0 is NOT written in S2/S3.** No page-0-shaped image exists anywhere in the stream, and MACM does not assemble or emit one. | [VERIFIED negative] `DISC-BOOTSTRAP.md` §6 |
| S4 | operator | After the stream ends ("THE SINTRAN III SYSTEM MAY NOW BE STARTED BY TYPING: `22!`") the operator types **`22!`**. | banner [VERIFIED] `INSTALL-PROCEDURE.md` §1.1 step E |
| S5 | microcode | `22!` = MOPC "start program in main memory at address 22B". Control jumps to word 22B of the **in-core** generated kernel. | [MANUAL] §4; entry proven in §2 |
| S6 | in-core kernel | Word 22B = `125001 JMP I 1` → indirect via word 23B = `042645` = **`SINTR`**, the cold-start entry. | **[VERIFIED bytes]** §2 |
| S7 | `SINTR` cold path | Copies the segments SAVE→IMAGE on the pack (`CRWDISC` per component), then **`PL011`/`FILL2` reads page 0, `MOVNP`s the 192-word LOAD PROGRAM + the disc's 744-word swap driver in, patches ≈16 parameter words, and writes page 0 back** via `CRDISC` to the system disc = this pack. **This is the first authoring of page 0.** | [VERIFIED] `DISC-BOOTSTRAP.md` §2/§4; `CARVED-DISC-SUPPORT.md` §3.2 |
| S8 | — | The pack now has a valid page 0. Future boots need no floppy: ALD→mass-storage, LOAD, microcode reads page 0 (1 K) to address 0, starts at 0; the LOAD PROGRAM pulls the resident system from SEGFILE 0 and enters `SINTR` (§5). | [VERIFIED] §5 |

**So, to the sub-question "is page 0 written by (a) MACM directly, (b) a one-time
cold-start of the in-core generated kernel, or (c) something else?": the answer
is (b).** Proven by two facts that meet in the middle: MACM emits no page 0
[VERIFIED negative], and `22!` demonstrably enters `SINTR` in the in-core kernel
[VERIFIED bytes, §2], whose cold path is the verified page-0 writer.

Hypothesis scorecard from the brief: **H1 = CONFIRMED** (with the refinement that
the trigger is the operator's `22!`, and the writer is `SINTR`/`PL011`, not
`DUMP-BOOTSTRAP`). **H3** — `22!` is indeed the trigger; `10,0$` is the earlier
MACM-load step, not the page-0 trigger (§4). **H2/H4** — no evidence a
stand-alone loader, microprogram, or `@CREATE-DIRECTORY` writes page 0; the
earlier finding that `@CREATE-DIRECTORY` does *not* touch the boot area stands
(`CARVED-DISC-SUPPORT.md` §5).

---

## 2. `22!` — decoded and traced into the kernel  **[VERIFIED]**

**The command.** [MANUAL] `ND-06.015.02 ND-100 Functional Description.md`
§7.2.1 (line 6977): "**Characters only legal in STOP: `!` = Start program in main
memory command**", with the octal start address typed in front. So `22!` = start
execution at address **22B**.

**What is at 22B in the running image.** Carved resident image
`E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\resident\SINTRAN-DATA_commoncode.bin`
(load base 0). Raw big-endian bytes at word 0o16..0o23 (file offset 0x1C..0x27),
read read-only with `xxd`:

```
offset 0x1C: d1 04    word 0o15 = 150404  POF
offset ...   aa 01    word 0o16 = 125001  JMP I 1   -> indirect via word 0o17
             31 40    word 0o17 = 030500  = MEMTO
             aa 01    word 0o20 = 125001  JMP I 1   -> indirect via word 0o21
             36 3b    word 0o21 = 033073  = RESTA   (RESTART-SYSTEM)
             aa 01    word 0o22 = 125001  JMP I 1   -> indirect via word 0o23
             45 a5    word 0o23 = 042645  = SINTR   (cold start)
```

`0xAA01 = 0o125001 = JMP I 1` (jump indirect through P+1). `0x45A5 = 0o042645`.
The three targets are named exactly by the L07 symbol table
`SINTRAN\NPL-SOURCE\SYMBOLS\L07\SYMBOL-2-LIST.SYMB.TXT`:
`MEMTO=030500` (:2435), `RESTA=033073` (:2454), `SINTR=042645` (:2544); and
`l07-kallsyms.txt:9852` gives `0x45A5 T SINTR` (a *text*/code symbol).

**Therefore starting at 22B executes `JMP I 1`, which fetches word 23B =
`042645` and jumps to `SINTR`** — the SINTRAN cold-start routine. This is a small
start-vector table: `22!` selects `SINTR` (full/cold start), `20!` would select
`RESTA` (restart). **[VERIFIED]**

`SINTR` (`042645B`) is the head of the cold-start flow whose page-0 writer
`PL011`/`FILL2` is byte-verified in `DISC-BOOTSTRAP.md` §2/§4 (176 of 192 words of
the shipped, unpatched LOAD PROGRAM match a real installed disc, the 16 that
differ being exactly the words `PL011` patches). The chain
`22! → 22B → SINTR → …PL011… → write page 0` is thus complete and byte-proven end
to end.

Reproduce with the companion tool:

```
python tools\decode_start_vectors.py ^
  ...\versions\L-VSX-500\resident\SINTRAN-DATA_commoncode.bin ^
  ...\SYMBOLS\L07\SYMBOL-2-LIST.SYMB.TXT
```

which prints the three `JMP I 1` vectors and resolves 22B→SINTR.

**Refinement of an earlier note.** `MACM-DIALOGUE.md` §5 step 2 listed `22!` as
"[INFERRED] starts the loaded program", left ambiguous between MACM and SINTRAN.
The bytes above resolve it: MACM lives high in core (base ~`0o77120`/`0o76203`),
the generated SINTRAN occupies low/mid core, and 22B is SINTRAN's cold-start
vector — so `22!` starts **SINTRAN**, exactly as MACM's own crib says
("`22!  => START SINTRAN`"). MACM is brought up earlier by FLOMON (§4), not by
`22!`.

---

## 3. The other octal `!` numbers in the stream tail: `160616!` / `115123!`  **[VERIFIED, corroborating prior work]**

These are **not** MOPC commands and have nothing to do with `22!`. They are the
terminator of the MAC `)BPUN` **ASCII preamble** that precedes each binary
`)9READ` record. `DISC-BOOTSTRAP.md` §6 already established this from the stream
bytes: each record is `NUL leader → ASCII preamble → '!'-record
(load-addr, word-count, words, checksum)`; the preamble is a 34-word octal
mini-loader containing `164403/164402/164400` (`IOX 403/402/400`, the tape-reader
window), `175235`, `124376` (`JMP *-2`), and it is written as a location-set
`160616/ … 160616!` (record #19 uses `115123`). The number in front of that `!`
is the mini-loader's own load/entry address; MACM's `)9READ` consumes only the
binary record and ignores the ASCII. **[VERIFIED bytes, prior pass; not re-run
here.]** The value `115123B` is additionally the word-count of `)9READ` record
#19 (`DISC-BOOTSTRAP.md` §6 table), i.e. the same octal number reused — a
coincidence of magnitude, not a shared meaning.

Bottom line: `22!` is a live operator command that starts code; `160616!` /
`115123!` are inert preamble text inside the data stream. Do not conflate them.

---

## 4. `10,0$` — what it is, and what `10` and `0` are  **[partly determined]**

**`$` decoded.** [MANUAL] `ND-06.015.02` §7.2.1 (line 6979): "**Characters only
legal in STOP: `$` or `&` = Bootstrap load command.**" §7.2.5.3 (line 7553): a
single `$` or `&` loads using the 16-bit **ALD** thumbwheel value. So `$` is the
console **bootstrap-load** trigger — consistent with the crib "LOAD SINTRAN FROM
DISKETTE".

**`10,0$` is NOT a bare MOPC command.** [MANUAL, VERIFIED negative] The full list
of legal MOPC input characters (§7.2.1, lines 6950-6983) is: `0-7`, `A-Y`, `@`,
`.`/space, `<`, `#`, CR, `/`, `)`, ESC, and the STOP-only set `! Z $ & . ' #`.
**The comma is not in that set** ("All other characters are answered with a ?").
A raw MOPC line therefore cannot contain the `,` in `10,0$`. Combined with the
[VERIFIED] fact from `MACM-DIALOGUE.md` §3 that **MACM contains no parser for
`10,0$`** (no command-table entry; `,` `$` are outside MACM's symbol character
set), `10,0$` is neither a MOPC command nor a MACM command.

**[INFERRED] `10,0$` is a command to the floppy-resident loader/monitor
(FLOMON)** — the program the ALD-`$` bootstrap of step S0 brings into core, and
the only piece running at that instant that could parse it. Its parser lives in
the **FLOMON boot record**, which the brief places explicitly **out of scope**.

**What `10` and `0` select: COULD NOT DETERMINE from in-scope artifacts.** Every
in-scope binary was checked: MOPC (manual) rejects the syntax; MACM has no
handler [VERIFIED]. The comma-separated operand form is FLOMON's, and FLOMON was
not analysed. The most that is defensible without the FLOMON record: `10` and `0`
are two octal operands to a floppy "load" verb whose executor is `$`; by shape
they are plausibly a device/segment selector and a unit/sub-selector, but **this
is not verified and must not be stated as fact**. To close it, disassemble the
**FLOMON floppy boot record** (out of scope here) or consult its documentation
(the ND floppy-load / stand-alone-loader manual). The ND-100 MOPC microcode does
*not* hold the answer, because the comma proves the parse is not MOPC's.

Note the same token family (`10,1$`, `10,0,10$`, `1,0$`) also appears *inside*
the `SINTRAN*:DATA` text (`INSTALL-PROCEDURE.md` §1.1). Whether MACM treats those
in-stream occurrences as no-ops/comments or FLOMON hand-off markers is **NOT
FOUND**; it does not affect the page-0 question and was not pursued.

---

## 5. Closing the loop: what the LOAD PROGRAM does once page 0 exists  **[VERIFIED]**

Once S7 has written page 0, a normal power-on boot needs no floppy. Trace of the
192-word LOAD PROGRAM (`RELOA`), source `PH-P2-OPPSTART.NPL` l.3729-3783,
byte-verified in `DISC-BOOTSTRAP.md` §4:

1. Microcode mass-storage load: 1 K from disc address 0 → core address 0, then
   start at 0 (`RELOA`, word 0 = `PIOF`). [MANUAL] §7.2.5.2.
2. `RELOA` disables interrupts (`PIOF`), sets up page-table/registers, then
   **relocates itself and the swap driver to high core** (`LDX ADR3; LDA I,X
   ADR1; STA I,X ADR2; JNC *-2`, then the driver copy loop with `-SWDSI`).
   [VERIFIED source l.3733-3741]
3. `NALOA`: it calls the **swap driver** (`JPL I LDRAD`) to read the resident
   system image from the disc, starting at block `DYBLS` = `DBLST(0)` = **first
   block of SEGFILE 0**, using the patched device word `KLIOX` (`IOX HDEV+4`) for
   SMD/Winchester or `IOXT` for SCSI. The patch words (`NOBLK, LDRAD, DYBLS,
   XSWTP, YSWTY, KLIOX, KLHDE, ADR2B, KLRC1`) are the ≈16 words `PL011` filled in
   at S7. [VERIFIED source l.3748-3766 + patch cross-check `DISC-BOOTSTRAP.md`
   §4.1]
4. `NALO2, IDENT PL11; JMP I (SINTR` — when the image is in, it **jumps to
   `SINTR`**, the same cold-start entry `22!` reaches. `IDENT PL11` (`143611`) is
   present in every bootable specimen. [VERIFIED source l.3767 + bytes §5 of
   `DISC-BOOTSTRAP.md`]

So the runtime loop is closed: **page 0 exists → microcode loads it → LOAD
PROGRAM pulls the resident system from SEGFILE 0 → enters `SINTR` → system
running** — and on any cold-start, `SINTR`/`PL011` rewrites page 0 again,
which is why an already-installed pack keeps a fresh, correctly-patched boot
sector.

---

## 6. Can a *running* system A bless a *different* fresh pack B?  **[mostly determined]**

This decides whether "generate, then cold-start" is actually necessary.

**The proven page-0 writer only ever writes its own system disc.** [VERIFIED]
`PL011`/`FILL2` issues `CALL FAR CRDISC` with the swap parameters taken from
`MASSNO(0)` — the logical device number of the *main swap device*, set at
`SINTR`/`FILL1` from the running system's own `SWTYP`
(`PH-P2-OPPSTART.NPL` l.741 `A=:MASSNO(0)`; `CARVED-DISC-SUPPORT.md` §3.2). There
is no device-selector argument: `PL011` cannot be pointed at an arbitrary pack.
Consequently a running system A writes **A's** page 0, not B's.

**[INFERRED]** Therefore a virgin pack B is blessed only when a SINTRAN
**cold-starts treating B as its own system disc** — i.e. the generation-onto-B
followed by `22!`/cold-start path of §1. Generating B as a mere *secondary*
spindle under a running A leaves B's page 0 zero until the machine is cold-started
with B as the system disc. Generation-then-cold-start is thus **necessary**, not
optional, for making a pack self-bootable.

**The operator command `DEVICE-FUNCTION → DUMP-BOOTSTRAP` exists** (carved name
table, L `003-S3CP.bin` @ `117361B`; `CARVED-DISC-SUPPORT.md` §6.2) and is the
one command whose *name* implies writing a boot block to a chosen device.
**Whether it can target an arbitrary hard-disc pack B was not established from
carved code in this pass**; the only manual reference frames `DUMP-BOOTSTRAP` as
floppy-oriented (ND-60.128, cited in `DISC-BOOTSTRAP.md` §9). This remains the
one open door by which a running A *might* bless B without a cold-start — see
§7. It does **not** change the virgin-first-boot answer, because the shipped
generation procedure uses `22!`, not `DUMP-BOOTSTRAP`.

---

## 7. Remaining unknowns

1. **`10,0$` operands `10` and `0`.** COULD NOT DETERMINE from MOPC (manual
   rejects the comma) or MACM (no parser). Requires the **FLOMON floppy boot
   record** (out of scope) or the ND floppy-loader documentation. §4.
2. **`DEVICE-FUNCTION → DUMP-BOOTSTRAP` on a hard disc.** Not traced to code
   here; unknown whether it can author page 0 on an arbitrary non-system pack B,
   or only dump/handle a floppy boot. Carve the handler reached from the
   `DUMP-BOOTSTRAP` name-table slot (`003-S3CP` @ `117361B`, base `30000B`) and
   check for a device parameter and a `MOVNP`/`CRDISC`(page 0) write. §6.
3. **The SCSI generation path.** MACM cannot drive SCSI (no `IOXT`; `144300`
   needs 16 bits, `IOX` carries 11 — [VERIFIED] `MACM-DIALOGUE.md` §7). So MACM's
   `)9SAVE` in step S3 cannot write a SCSI pack's SAVE area, yet `SINTR`'s
   SAVE→IMAGE copy in S7 presupposes a SAVE area on the pack. How a **SCSI** pack
   receives its SAVE area, and hence its first page 0, is **NOT FOUND**. The
   in-core kernel started by `22!` *does* speak SCSI, so it is the plausible
   writer for both SAVE and page 0 on SCSI — but the mechanism that gets the
   image onto the SCSI disc before/at that first start is unproven. [INFERRED
   gap]
4. **Cold-vs-restart discrimination inside `SINTR`.** `SINTR` is entered both by
   `22!` and by the disc LOAD PROGRAM's `JMP I (SINTR`. Which flag/register makes
   the first `22!` behave as a *cold* start (SAVE→IMAGE + page-0 write) rather
   than a restart was not carved. It does not affect the result — `DISC-BOOTSTRAP.md`
   §2 verifies page 0 is (re)written on **both** cold-start and restart — but the
   discriminator itself is [NOT FOUND].
5. **`MEMTO` (030500) and the word-3 vector `JMP I 21 → 24B`.** The other
   low-memory vectors were not chased; only `RESTA` (restart) and `SINTR`
   (cold-start) were needed and are verified.

---

## 8. Artifacts

* **Deliverable:** `E:\Dev\Ronny\NDInsight\tools\boot-floppy\FIRST-BOOT.md` (this file).
* **Reusable tool (new):**
  `E:\Dev\Ronny\NDInsight\tools\boot-floppy\tools\decode_start_vectors.py` —
  decodes the ND-100 low-memory `JMP I 1` start-vector table from any carved
  resident image and resolves each target against a SINTRAN symbol list. Opens
  everything read-only. Reproduces the byte-verified 22B→`SINTR` result.

## 9. Evidence index

| Claim | Source (read-only) |
|---|---|
| word 22B = `125001 JMP I 1`, 23B = `042645` = SINTR; 20B→RESTA; 16B→MEMTO | `…\sintran-segment-carver\versions\L-VSX-500\resident\SINTRAN-DATA_commoncode.bin` (bytes 0x1C-0x27) + `.dis` |
| `SINTR=042645`, `RESTA=033073`, `MEMTO=030500`; `0x45A5 T SINTR` | `SINTRAN\NPL-SOURCE\SYMBOLS\L07\SYMBOL-2-LIST.SYMB.TXT`, `l07-kallsyms.txt` |
| `!` = start-in-memory, `$`/`&` = bootstrap-load, comma illegal in MOPC, ALD | [MANUAL] `Reference-Manuals\ND-06.015.02 ND-100 Functional Description.md` §7.2.1, §7.2.5.2-3 |
| MACM writes SAVE area, emits no page 0; `)9SAVE` via `IOX 1543/1545`/`500`; no `IOXT` | `MACM-DIALOGUE.md` §3, §7; `DISC-BOOTSTRAP.md` §6 |
| `PL011`/`FILL2` writes page 0 via `CRDISC`; LOAD PROGRAM byte layout & patch words | `DISC-BOOTSTRAP.md` §2, §4, §4.1; `PH-P2-OPPSTART.NPL` l.3729-3783, 845-876 |
| `SINTR`/`FILL1` sets `MASSNO(0)` from `SWTYP`; SAVE→IMAGE `CRWDISC` | `CARVED-DISC-SUPPORT.md` §3.2; `PH-P2-OPPSTART.NPL` l.739-749 |
| Operator banner "STARTED BY TYPING: 22!"; `10,0$`/`22!` cribs | `INSTALL-PROCEDURE.md` §1.1; `MACM-DIALOGUE.md` §2 |
| `DUMP-BOOTSTRAP` name-table slot `003-S3CP` @ `117361B` | `CARVED-DISC-SUPPORT.md` §6.2 |

No file under `D:\ND\`, no `.img`/`.image`, no carved binary, and no other
agent's document was modified. Nothing was written outside
`E:\Dev\Ronny\NDInsight\tools\boot-floppy\FIRST-BOOT.md` and
`…\tools\decode_start_vectors.py`.
