# NDIX-C as a second XMSG client - what it confirms, corrects, and cannot tell us

**Date:** 2026-08-05
**Tree examined:** `E:\Dev\Ronny\NDIX-C` (a Unix port for the ND-500 that talks XMSG)

NDIX-C is the only **independent** XMSG client we have besides the wire captures. That makes
it worth reading. It is also a 1987 third-party client, so the rule applied throughout this
document is:

> **The official ND include files beat NDIX every time they disagree.** Those are
> `DOC/COSMOS-RE/ENNS0-Startup-RE-2026-07-23/xmsg-L-binaries/XMSG-PL-VALUES-L.INCL` and
> `XMSG-VALUES-L.SYMB`, plus the M-version data in `xmsg-constants.json`.

Absolute paths into the NDIX tree are used below because it sits outside this repository and
there is no relative way to name it. Links to files inside this repository stay relative.

---

## 0. Where the XMSG material actually is

The two richest files are easy to miss:

 - `E:\Dev\Ronny\NDIX-C\kernel\MASTER\if\xmsg.h` - function codes, option masks, XROUT
   services and error codes, all written as **C octal literals**.
 - `E:\Dev\Ronny\NDIX-C\baseline\bin\cps\` - a complete userland `*XFTRA` file-transfer
   client. This is the closest thing in the tree to what our C# does. `sucps.c` is a second
   copy of the same code.
 - `E:\Dev\Ronny\NDIX-C\kernel\MASTER\if\xg.c` - the driver.
 - `E:\Dev\Ronny\NDIX-C\kernel\MASTER\machine\if.h` - the ND-500 front-end call packets.

**Number-base trap.** Every code in `xmsg.h` is a C octal literal. `060` is **48**, not 60.
Two independent cross-checks confirm the octal reading: `XFRRE 051` = 41 and `XETHER 055` = 45
both line up with our own table.

---

## 1. What NDIX CONFIRMS [VERIFIED - read first-hand]

### 1a. The `*XFTRA` parameter set and wire order

`E:\Dev\Ronny\NDIX-C\baseline\bin\cps\xmsg.c:493-547` appends parameters in this order:

```
1, 2, 12, 13, 8, 9, 10, 11
```

That is **byte-for-byte the order in our own capture** - see
[XMSG-XFTRA-FILE-TRANSFER-REQUEST-CAPTURED-2026-07-28.md](XMSG-XFTRA-FILE-TRANSFER-REQUEST-CAPTURED-2026-07-28.md)
lines 29-36, taken off the wire from real SINTRAN. Two clients written by different people on
different machines emit the same non-obvious order. That is strong agreement.

### 1b. The file type is always sent

`xmsg.c:504` says it outright, in the source's own words:

```
 * i) type must be specified (protocol doc. is WRONG)
```

When the user names no type, `cps` supplies `"symb"` (`xmsg.c:517`). Our capture likewise
carries parameter 9 = `SYMB` even though the name already contains `:BATC`.

### 1c. The password fold

`xmsg.c:324-333` uppercases **letters only** (guarded by `islower`), then shifts a 16-bit
accumulator by 3, then adds the character. That is the same shape as our
`SRC/Xmsg.Api/Model/SintranPassword.cs`. See section 4 for the direction, which is where I
initially got this wrong.

---

## 2. What NDIX CORRECTS in our documentation [VERIFIED - read first-hand]

Our capture doc had marked parameters 10, 11 and 13 `[INFERRED]` and guessed them from the
`DEFINE-TRANSFER-CONDITIONS` command's argument list. One guess was right and two were wrong.

| Param | Our old guess | What NDIX shows | Evidence |
|---|---|---|---|
| 10 | buffer size in bytes | **Correct** - transfer block size, `FSIZ` = 1024 | `sucps.c:56`, `xmsg.c:42`, used at `xmsg.c:544` |
| 11 | a buffer count | **WRONG** - it is the **OPERATION**, `ioflg+1`, so **1 = read, 2 = write** | `xmsg.c:546` |
| 13 | secure-messages flag | **WRONG** - it is the **folded PASSWORD** | `xmsg.c:499`, declared `/* default passwd */` at `xmsg.c:148`, set by the fold at `xmsg.c:333` |

Both corrections are consistent with every byte of the capture:

 - Parameter 11 = 2 in the capture, which is a `COPY-FILE` **writing** to the remote. The
   direction reading is confirmed independently at `xmsg.c:549`, where `!ioflg` is the case
   that checks the **source** file's length - so `ioflg` 0 = read, 1 = write, hence
   `ioflg+1` gives 1 = read and 2 = write.
 - Parameter 13 = 0 in the capture because that user has no password, and the fold of an
   empty password is 0. A placeholder zero is exactly why it looked like a flag.

Parameter 11 also agrees with the later
[XMSG-APPEND-REMOTE-BATCH-CAPTURED-2026-07-31.md](XMSG-APPEND-REMOTE-BATCH-CAPTURED-2026-07-31.md),
where parameter 11 carries the operation.

The capture doc has been updated with these, keeping the old reading quoted so the record
shows what the capture alone could and could not settle.

---

## 3. Where NDIX differs and OUR side stands [VERIFIED]

### 3a. Function 48 is the end marker, not a function - DO NOT ADD IT

`xmsg.h:97` defines `XFRREN 060` = 48, "receive and read message (don't wait)". But the
official ND source declares 48 as the **table terminator**:

```
XMSG-PL-VALUES-L.INCL:61   CONSTANT X5FUN=48  % == END MARKER == LEAVE ME HERE PLEASE
XMSG-VALUES-L.SYMB:64      SYMBOL   X5FUN=48  % == END MARKER == LEAVE ME HERE PLEASE
```

The M-version table in `xmsg-constants.json` likewise stops at `XFGSM=47`. Adding `XFRREN`
to `SRC/Xmsg.Protocol/Enums/XmsgFunction.cs` would have been a **defect**, not a fix. It was
nearly done on NDIX's word alone.

Why NDIX uses a function code ND's own header calls the end of the table is **UNKNOWN**.

### 3b. `XETHER` is not a conflict

`xmsg.h:96` calls 055 (= 45) `XETHER`, "special Ethernet call". Our table calls 45 `XFSFM`,
"Send message via specified link/netserver". Sending via a specified netserver **is** what
NDIX uses it for. Same function, NDIX named it after its own use. Nothing to change.

### 3c. `XFWAK` - ours is right by the authority; NDIX's value is UNEXPLAINED

Official: `XMSG-PL-VALUES-L.INCL` gives `XFWAK=14` as a **bit number**, meaning
"In XFRCV/PST/GST/RRH/RRE: Wake up task on status change". Our
`SRC/Xmsg.Protocol/Enums/XmsgOption.cs` uses bit 14 with that same meaning. **Ours matches
the authority and needs no change.**

NDIX's `xmsg.h:112` gives `XFWAK 0x8000`, the same mask as its own `XFWTF` at `xmsg.h:103`.

**I do not know whether that is an error.** It was called a typo in an earlier pass and that
claim is withdrawn. This header deliberately reuses bit values per function elsewhere -
`0x1000` is `XFBNC`, `XFRES` and `XFRMR` depending on the call (`xmsg.h:107-109`), and
`0x2000` is both `XFRRO` and `XFHIP` (`xmsg.h:110-111`). So a per-function meaning for
`0x8000` may well be intended. Either way it says nothing about our value.

### 3d. Three error names in `xmsg.h` are stale

`xmsg.h` gives -8 = `XEISP`, -15 = `XECRA`, -28 = `XELOK`. The official L include gives
-8 = `XEIRT`, -15 = `XEBNC`, -28 = `XEDRI`, which is what we use. NDIX's **own** recovered
`/usr/include/xmsgerror.h` also agrees with us, not with its kernel header. `xmsg.h` is the
stale copy. [The recovered-image claim is SECOND-HAND - see section 6.]

### 3e. The quote reordering is an NDIX-client workaround, not a protocol rule

`xmsg.c:502-507` warns that for a **quoted new file** the closing `"` goes before the file
type. The code at `xmsg.c:520-536` does exactly that: it sends **parameter 9 first**
(`:531`), then rewrites the quote over the colon and sends **parameter 8** (`:534`).

Our capture from real SINTRAN does the **opposite**: parameter 8 first, with the quotes
wrapping name and type together as `"XMSG-COPY:BATC"`. So this is a workaround inside the
NDIX client. **Our builder follows the capture and should not change.**

---

## 4. The error I made, and why it matters for method

I first reported that NDIX's password fold contradicted ours. It does not.

The fold uses inline assembly, `asm("h shr r3,$3")` (`xmsg.c:324-333`). I read `shr` as
"shift right" - an **ND-100 habit applied to ND-500 code**. On the ND-500, `SHR` means
"shift **R**otationally", and the count is a signed byte where **positive means LEFT**.
From `ND-05.009.4 EN ND-500 Reference Manual` section 10.26:

> "A rotational shift is performed on the byte, halfword or word operand. `<shiftcount>` is
> interpreted as a signed byte. **Positive `<shiftcount>` implies left shift**, negative
> implies right shift."

`H SHR` is the halfword form - 16-bit, matching our 16-bit accumulator. So `h shr r3,$3` is
a **16-bit rotate LEFT by 3**, which is exactly what
`SRC/Xmsg.Api/Model/SintranPassword.cs` does. **NDIX confirms our implementation.**

Our fold also matches the real vector `ORANGE` -> `0x382A`. A literal shift-right-3 would
give `0x004F`.

**A caution about `nd500x`.** That repo's `src/cpu/instructions/SHIFT/Shr.c` implements the
opposite direction. Its own notes
(`~/repos/nd500x/docs/instruction-reference/SHIFT.md:250-260`) record the C file claiming it
was *"empirically verified against nd500-as"* and flag that the difference *"may reflect an
assembler/operand-encoding quirk rather than the CPU's rotate direction"*. **That is an open
question in that repo, not a settled bug, and this analysis does not rest on it.** The
password conclusion rests on the manual and on the real vector, which agree with each other.

**The general lesson:** NDIX C code runs on the **ND-500**. Its inline assembly must be read
with ND-500 instruction semantics, never ND-100 ones.

---

## 5. What NDIX CANNOT tell us

 - **Nothing about the wire.** NDIX sits above XMSG and reaches the network only through the
   ND-100's XMSG. It says nothing about the header word-6 checksum, the fragment split, or
   the FA record layout. Those stay pinned to the captures - see
   [XMSG-PROTOCOL.md](XMSG-PROTOCOL.md).
 - **The contents of `xpara`.** A 512-byte area, 64 bytes (32 ND-100 words) per XMSG
   subdevice, whose address is published exactly once. **Nothing in NDIX ever reads or
   writes it.** What the ND-100 puts there is UNKNOWN. If we ever have to answer an
   `FE_IDEV` for generic device 7, we would be inventing it. **Do not.**
 - **Anything about the ND-500 front-end call path** (`fecall`, `FE_IDEV`, `paralist`). Our
   C# has no notion of it, and that is correct - it matters only if we emulate an ND-500
   running NDIX.

---

## 6. Provenance - what is first-hand and what is not

Read directly while writing this document, and safe to rely on:

 - `E:\Dev\Ronny\NDIX-C\kernel\MASTER\if\xmsg.h:76-124`
 - `E:\Dev\Ronny\NDIX-C\baseline\bin\cps\xmsg.c:489-550`, and the `FSIZ` / `depas`
   definitions in `xmsg.c` and `sucps.c`
 - `XMSG-PL-VALUES-L.INCL` and `XMSG-VALUES-L.SYMB` - the function table, the option bits,
   and the three error bases
 - `ND-05.009.4 EN ND-500 Reference Manual` section 10.26
 - our own `XmsgFunction.cs`, `XmsgError.cs`, `XroutError.cs`, `XmfidoStatus.cs`,
   `SintranPassword.cs`

**SECOND-HAND** - reported by a survey pass over the tree and **not** re-read line by line.
Treat as a lead, not as fact, and verify before acting:

 - the `xg.c` driver call sequence, the ring-buffer layout and fixed addresses
 - the `machine/if.h` fecall packet field offsets
 - the recovered `/usr/include/xmsgerror.h` inside `rootfs_full.img`, including the
   `XMXE = 16896 + |code|` re-basing
 - the `*XFTRA` fragment framing (request codes 65 / 66 / 67, always-1030-byte writes, the
   max byte pointer carried in the close)

---

## 7. Outcome

**No production code was changed.** Checking each claim against the official ND include
files turned three of the four candidate defects into non-defects, and the fourth into a
"do not do this". One documentation file was corrected:
[XMSG-XFTRA-FILE-TRANSFER-REQUEST-CAPTURED-2026-07-28.md](XMSG-XFTRA-FILE-TRANSFER-REQUEST-CAPTURED-2026-07-28.md).

Still open, and worth a look if anyone returns to this tree: the `*XFTRA` fragment framing
in section 6's second-hand list. If it holds up, it describes the read/write ladder in wire
order and could be compared against our FA read and write paths.
