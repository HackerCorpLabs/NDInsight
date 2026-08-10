# The FA server's own command names, read out of the binary

**Date:** 2026-08-06
**Binary:** `DOC/COSMOS-RE/carve/COS-FA-SERV-E04_PROG.bin` (231424 bytes = 115712 words)
**Goal:** learn what FA operations `0x01`, `0x04` and `0x0D` are, since no capture carries them.
**Outcome:** got their NAMES. Did NOT get their behaviour. Also found a 14th handler with no name.

---

## 1. What was established

The command-name table is plain ASCII, run together with no separators, at file word 100401.
In table order, these are the server's own names:

| Code | Name in the binary | Our C# name |
|---|---|---|
| `0x01` | `File-entry-disconnect` | `FileEntryDisconnect` |
| `0x02` | `Reserve-file-entry` | `ReserveFileEntry` |
| `0x03` | `Release-file-entry` | `ReleaseFileEntry` |
| `0x04` | `Change-file-entry-id` | `ChangeFileEntryId` |
| `0x05` | `Open-file` | `OpenFile` |
| `0x06` | `Close-file` | `CloseFile` |
| `0x07` | `Set-block-size` | `SetBlockSize` |
| `0x08` | `Read-file` | `ReadFile` |
| `0x09` | `Write-file` | `WriteFile` |
| `0x0A` | `Create-file` | `CreateFile` |
| `0x0B` | `Delete-file` | `DeleteFile` |
| `0x0C` | `SIII-special` | `SiiiSpecial` |
| `0x0D` | `Device-function` | `DeviceFunction` |

Every C# name matches. Three of them were chosen before these strings were read, so this is a
real check rather than a restatement.

## 2. How the addresses were anchored

Two independent anchors agree, which is the only reason the base is stated as fact:

1. The `"FA-server"` status text is at file word 99628. The analysis records it at `BANK2::842c`
   (= 33836). So the bank-2 base is `99628 - 33836 = 65792` file words.
2. Applying that base, the name table at word 100401 lands on `BANK2::8731` - exactly where the
   analysis puts it, and a value not used to derive the base.

The handler address table sits immediately after the names, at file word 100493, big-endian, and
matches the transcription already in `FaOperation.cs` word for word:

```
idx   0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
    1fb0  1fb0  1ead  1ecc  1edd  1eee  1eff  1f10  1f21  1f32  1f43  1f6c  1f7d  1f8e  1f9f  1fb0
```

Slots 0, 1 and 15 hold `1fb0`, which is the padding value. That `0x01` shares it is the evidence
for the existing note that `File-entry-disconnect` is not dispatched down the normal path.

## 3z. RESOLVED - read this instead of sections 3, 3a, 3b and 4

Everything below section 2 was written while the `1ead` row was still unread. Ghidra has now
disassembled it (the region needed FORCING - no analysis pass had touched it, which is exactly why
two earlier readings of it were wrong). The answer:

**Every entry is the same 17-word stub, and it is a STRING DESCRIPTOR.**

```
SAA  <n>          ; n = the command name's length MINUS ONE
SWAP CLD SA, DD
COPY SB, DT
AAT  -0x76
AAT  0x3
STF  -0x62,B
SAA  <n>          ; again
SWAP CLD SA, DD
LDT  *<addr>      ; the word at addr holds the ADDRESS of the name's ASCII text
LDX  -0x7e,B
STF  0x6,X
LDF  -0x62,B
STF  0x9,X
LDD  *<addr+1>
JPL / JPL / JMP   ; shared tails
```

So the row is a **command-name table** - length plus pointer for each name, which is what a parser
that matches a typed command looks like. **It is not a wire-handler table.** The real request
handlers are ordinary named functions at `ram:2xxx`..`ram:4xxx`.

### It is also off by one against the names

Entry `i` serves name `i-1`. Two independent fields agree in all six checked cases:

| Stub | `SAA` | Name pointer | Name it resolves to | Length |
|---|---|---|---|---|
| `1ead` | 20 | `8731` | `File-entry-disconnect` | 21 |
| `1edd` | 17 | `8745` | `Release-file-entry` | 18 |
| `1eee` | 19 | - | `Change-file-entry-id` | 20 |
| `1f21` | 13 | `8762` | `Set-block-size` | 14 |
| `1f8e` | 11 | `877f` | `SIII-special` | 12 |
| `1f9f` | 14 | `8785` | `Device-function` | 15 |

`SAA` is length-1 every time - the usual PLANC upper-bound-of-a-0-based-array form.

**Two of this document's own claims die here:**

1. "The operation code is the index into that table" - wrong by one.
2. "Slot 14 has a handler and no name" (section 3) - **dead**. Slot 14 is `Device-function`.
   There is no fourteenth operation. That "finding" was an artifact of the off-by-one.

The operation CODES are untouched: they come from captured traffic, never from this table.

### The lead in section 3b is exhausted

Section 3b proposed following the per-command pointers in the hope they reached parameter
descriptors, which would have given the missing wire layouts. **They reach the command's own name
text.** That is a definite answer, not a stalled one: this route cannot produce a wire layout, so
`0x01`, `0x04` and `0x0D` still need a capture.

Section 4's "the disassembly is nonsense" is also explained - the region simply had not been
disassembled, in the flat carve or in Ghidra. Sections 3a and 3b's "uniform DATA records" reading
was wrong for the same reason; the uniformity is real, but they are code.

---

## 3. NEW: slot 14 has a handler and no name  *(SUPERSEDED - see 3z)*

Slot 14 holds `1f9f`. That is distinct from the `1fb0` padding, so it is a real separate address -
but the name table stops at thirteen entries.

**What this is, is UNKNOWN.** It could be an operation ND did not name in this table, or a
default / error arm that happens to sit past the last command. It is deliberately NOT added to
`FaOperation`: a code with no name and no captured traffic is not an operation we can claim
exists. Recorded so the next person sees it rather than rediscovering it.

## 3a. CORRECTION, same day: those addresses are not code

Section 4 below records a failed attempt to disassemble `1ead`..`1f9f`. **The reason it failed is
that they are not instructions.** With the Ghidra MCP back, `cos-fa-serv-e04.prog` is loaded
properly as an ND-100 `:PROG` with real segments (`BANK1 0000-a895`, `BANK2 0000-c2ea`), and that
settles it:

 - `BANK2::1f21` and `BANK2::1edd` are **zero**. The addresses are not bank-2 at all.
 - `ram:1ead` (BANK1) holds data, and a hexdump shows fixed-size records that repeat: a block
   beginning `f1 14 c8 69 cc 5e f6 8a f6 03 31 9e`, then `1f b0`, then a near-identical block
   beginning `f1 11 c8 69 cc 5e f6 8a f6 03 31 9e`. Uniform records, with `1fb0` sitting inside
   them - not a function prologue.

So the run of `FSB` / `FAD` / `FDV` that nd100-dis produced was correct behaviour on its part: it
was decoding a data table. The stride-17 structure I found is real, and it is a **record stride**,
not a code layout. The description of this table as "a table of handler addresses", carried in
`FaOperation.cs` from the earlier analysis, is **not supported** by what the loaded image shows.
What the table actually is, is unresolved.

### The real handlers are named functions, already carved

The FA operation handlers live at `ram:2xxx`..`ram:4xxx` as proper functions. Relevant here:

| Op | Handler | State |
|---|---|---|
| `0x04` `Change-file-entry-id` | `fa_change_file_entry_id` @ `ram:2e12` | carved and commented |
| `0x01` `File-entry-disconnect` | not found | no function named for it |
| `0x0D` `Device-function` | not found | no function named for it |

**Operation `0x04`, from the comment left by the earlier Ghidra pass** (their words, not a fresh
reading of mine - flagged because I have not re-derived it):

> Finds the entry via `fa_find_global_entry_by_key` (`0x28b2`); requires it to be reserved
> (`entry[+0xa]` reservation bits set, else error `0xd`) and of type 8 (`entry[+1]`). Then, gated
> by a flag: either ACCUMULATES the request's 2-word value into the entry's sub-block
> `entry[+0xb+2]` (ADD), or REPLACES it. Finally writes the new id/attributes into `entry[+0xb]`
> and `entry[+0xb+1]` and processes the `entry[+5]` chain to build the reply.

Note the decompiled body runs from `2e12` all the way to `315b`, where `fa_file_data_transfer`
begins - so the function bounds look over-extended, and the tail of that listing probably belongs
to a neighbour. Treat the comment as the reliable part and the body as suspect until the bounds
are fixed.

Its only caller is `fa_registry_helper_4056`, NOT a dispatch table - which is another reason to
doubt that `1ead` is a handler-address table.

**This still does not give a wire layout.** Knowing that `0x04` renumbers a reserved entry does not
say how the request is encoded, and `0x01` and `0x0D` remain unidentified in the carve. They stay
refused.

## 3b. The record structure, measured

Each command's record is 17 words (Reserve is 31 and Create 41 - those two carry extra). Comparing
records across commands, **only 7 of the 17 words differ**; the other ten are byte-identical
everywhere. Measured on `Open-file` (`1eee`), `Read-file` (`1f21`), `Device-function` (`1f8e`) and
the unnamed slot 14 (`1f9f`):

| Word | Open | Read | Device-fn | slot 14 | |
|---|---|---|---|---|---|
| 0 | `13f1` | `0df1` | `0bf1` | `0ef1` | varies |
| 1-5 | `69c8 5ecc 8af6 03f6 9e31` | same | same | same | **identical** |
| 6 | `13f1` | `0df1` | `0bf1` | `0ef1` | varies, = word 0 |
| 7 | `69c8` | same | same | same | identical |
| 8 | `6250` | `3850` | `5150` | `4350` | varies |
| 9-12 | `8259 0634 9e39 0934` | same | same | same | **identical** |
| 13 | `5e28` | `3428` | `4d28` | `3f28` | varies |
| 14 | `ceba` | `9bba` | `43ba` | `32ba` | varies |
| 15 | `c4ba` | `91ba` | `b7ba` | `a6ba` | varies |
| 16 | `cdaa` | `9aaa` | `12a8` | `01a8` | varies |

The varying words all have the shape of memory-reference instructions with a per-command
displacement in the low byte - i.e. **the records point at something different per command, through
an identical frame.** The per-command values are not the operation codes (Read is op 8 but carries
`0d`), so they are displacements or table indices, not the code itself.

**Whether these records are executed or interpreted is UNRESOLVED.** Against them being code: the
ten identical words decode as a fixed pattern of `FSB` / `FDV` / `FAD` floating-point instructions,
which no compiler emits as the common body of thirteen command routines. For them being code: the
uniform-frame-with-varying-displacement shape is exactly what generated per-command thunks look
like. Not settled, and not guessed.

**Why this matters if anyone picks it up:** if the varying words are pointers to per-command
parameter descriptors, following them for a command whose wire layout we already know
(`Open-file`, `Read-file`) would validate the reading, and following them for `0x01`, `0x04` and
`0x0D` would then give those layouts. That is the one route from here to the missing encodings that
does not need a capture. It is a lead, not a result.

## 4. What FAILED, and why it is written down

**The handler bodies were not read.** The addresses (`1ead` .. `1f9f`) are 16-bit, and the
image's code region does not disassemble into anything sensible at them under either offset tried:

 - `K = 0` (address = file word): produced runs of `FSB` / `FAD` / `FDV` - floating-point
   instructions in a pattern no compiler emits.
 - `K = 256` (the 256-word header the data region demonstrably uses, since the bank-2 base is
   `65536 + 256`): same character of output.

The documented code anchor `fa_dispatch_by_type3bits = 0x08b1` decodes as noise under both.

One structural fact IS solid and survives: searching for words repeating at a stride of 17
finds a run at file word ~8140, and `8140 - 256 = 0x1ecc`, the `Release-file-entry` address.
So the handlers really are a family of 17-word stubs and the `K = 256` mapping really does point
at them - the *addressing* is right and the *decoding* is what fails. That points at the image
being other than a flat big-endian word dump in that region (byte order, or a container the
carve did not flatten), not at the mapping being wrong.

**Next step is fixing the image, not more searching.** Compare this carve against how the
`:PROG` container is laid out, or re-carve from `COS-FA-SERV-E04.PROG` with the loader's own
placement, then disassemble `1ead` and check it against the known-good `Read-file` at `1f21`
before trusting anything read from `1edd`, `1f8e` or `1f9f`.

## 5. What this does not change

Nothing is now served that was not served before. `0x01`, `0x04` and `0x0D` remain refused, which
is still the correct answer: a name is not a wire layout. The value here is that the next attempt
starts from thirteen confirmed names, a verified table position, a proven bank-2 base, and a
written-down record of which mapping already failed.
