# XMSG crash evidence, D100, 2026-08-21/22

The first XMSG crash dump ever taken off this machine, with the console log that goes with it.

## What is here

| file | what it is |
| --- | --- |
| `XMSGDUMP-T-tables.bin` | the XMSG **table area** at the moment of the crash - 38912 bytes, 19 pages |
| `d100-console-errors.log` | D100's error device, timestamped with the HOST clock, covering four deaths |

`XMSGDUMP-A` (segments 33/34) and `XMSGDUMP-B` (the buffer pool) are still on D100 as
`(PACK-ONE:RT)XMSGDUMP-A:DATA` and `-B`, copied under SYSTEM as `XMSGDMPA1` / `XMSGDMPB1`. Pull
them the same way if the tables are not enough.

## How it was obtained, because none of it was possible before

**1. The console was pointed at a terminal we own.** `@GET-ERROR-DEVICE` answered `1` - the
physical console, Ronny's window - so no agent session could ever see the fatal line. It had been
printing since 2026-08-04 and cost ten controls.

```
@SET-ERROR-DEVICE 39          <- a logged RetroTerm session; @SET-ERROR-DEVICE 1 puts it back
```

**2. The dump files were defined.** `X-C LIST-DUMP-FILES` said *"No dump files defined"*, so every
crash before this discarded the state XMSG was willing to hand over.

```
X-C: DEFINE-DUMP-FILES        -> XMSGDUMP-A / XMSGDUMP-T / XMSGDUMP-B
```

XROUT then writes all three by itself on every crash, announcing it on the console:
`** XROUT: XMSG segments, tables and buffer dumped to files **`.

**3. They were copied before being read.** Each crash OVERWRITES them, so they were copied to
SYSTEM under short names first. Two things bite here:

 - `COPY-FILE` wants the WHOLE destination spec inside the quotes, type included:
   `COPY-FILE "XMSGDMPT1:DATA",(PACK-ONE:RT)XMSGDUMP-T:DATA`
   (`"XMSGDMPT1":DATA` is rejected with `ILLEGAL CHARACTER IN PARAMETER`.)
 - a file transfer allows **13 characters of name**, so `(RT)XMSGDUMP-T` (14) cannot be pulled
   directly and `XMSGDMPT1:DATA` (14) cannot either. Copy again to something short - `XDT1:DATA`.

## The crash this dump belongs to

```
08.15.25  ERROR 46 IN DUMMY AT 34360; XMSG FATAL ERROR - INTERNAL ERROR OR INCONSISTENCY
          XMSG ERROR CODE:      27; PHYSICAL ADDRESS:  141204
08.15.27  ** XROUT: XMSG segments, tables and buffer dumped to files **
```

**Code 27 is `XXNER`, "Network gateway error"** - `SINTRAN/XMSG/XMSG-VALUES-M.SYMB:356`. The full
crash-code list is `SYMBOL XX*` at lines 330-385 of that file. `ERROR 46 IN DUMMY` is the SINTRAN
wrapper and `DUMMY` is the RT program it was charged to; `ERROR 53 IN XROUT SUBERROR 36` further
down is XROUT's own `ERRMON` report AFTER the kernel died, and `COSPO` / `FSART` / `TADAD` /
`XMFIDO` aborting underneath are the cascade, not four separate faults.

**Every death recorded here carries the SAME code at the SAME address**, back to 2026-08-04. It is
deterministic, not a timer.

## What the console log shows across four deaths

| # | death | what preceded it | gap |
|---|---|---|---|
| 1 | 23:38:51 | a 6400-byte push COMPLETED | ~30 s |
| 2 | 23:58:36 | a 133KB push, peer went silent mid-transfer | during |
| 3 | 00:28:47 | a 133KB push COMPLETED cleanly | 4 min 38 s |
| 4 | 00:43:00 | a 224KB listing PULL completed | ~3-6 min |

**The common factor is an FA file transfer - push OR pull - and the delay varies from seconds to
five minutes.** That variation is why short controls kept passing, and why two different "it dies
N minutes after X" theories were published and both had to be retracted. Deaths 1 and 4 kill the
size theory: 6400 bytes is small, and 4 followed a pull.

## DECODED: two tables identified, 2026-08-22

The dump is **19456 words = 19 pages**, and `LIST-UTILIZATION` reports `Size of physical tables:
19 pages`. So this file IS the whole physical table area, one for one.

Only **1163 words are non-zero** (6%), and they fall into two regular structures.

### The MESSAGE TABLE - words 0..~640, stride 17

Entries repeat every **17 words**, and **exactly 37 of them carry data** - which matches
`Message table ... Max used 37` on the same machine. That pins both the base (word 0) and the
entry size.

```
entry  0 @    0: 0000 8660 0064 06F5 0064 06F5 0000 0000 0008 0005 50D1 0004 0004 0000 D6C0 0000 E060
entry  1 @   17: 0000 9610 0064 0569 0064 0490 0000 0000 0108 0005 4C61 0006 0006 0000 D65A D65A E044
```

`0064` is 100 - D100's own system number - and it appears twice in every entry, which is what a
from/to pair would look like on a local message.

### The RECEIVE FRAME TABLE - words 12289.., stride 313, five entries

Five regions exactly **313 words apart**: 12289, 12602, 12915, 13228, 13541. That matches
`Receive Frame table ... 20 limit, 5 max used, 5 in use`, and 313 words = 626 bytes, which is the
LAPB **max info 622** plus a small header.

**And the contents are real XMSG frames, readable against the runner's own wire logs:**

```
frame 0 @ 12289: 013F 4E1F 0003 0064 4E1F 01C3 0001 8EA2 2100 9600 0064 06F5
frame 1 @ 12602: 092A 2113 000E 0064 4E1F 0152 0008 8F01 2100 9600 0064 06F5
frame 2 @ 12915: 0901 4E1F 000E 0064 4E1F 01C5 000A 8E8C 2100 8284 0064 06F5
```

`4E1F` is **19999** - our node. `0064` is **100** - D100. `2113` is the XMSG header word seen at
the front of every frame in the runner logs. `8EA2` / `8F01` / `8E8C` sit where the header
checksum belongs.

**What this already says about the crash:** the receive frame table was **5 of 20 in use** - NOT
exhausted - which agrees with every `LIST-UTILIZATION` sample taken all night. The kernel was not
short of anything. It stopped; it did not fill up.

### A 9-WORD TABLE WITH AN INTACT FREE LIST - pages 4..9, words 4435..9766

Pages 4 to 9 hold one contiguous table with a strikingly even ~113 non-zero words per page. The
stride is **9 words**, and the values are the giveaway:

```
offset 6145 = 180A      6154 = 1813      6163 = 181C      6172 = 1825  ...
       (=6154)               (=6163)           (=6172)          (=6181)
```

**Each entry's first word holds the offset of the NEXT entry.** `0x180A` is 6154, which is exactly
where the next populated word sits. That is a **linked free list** threaded through a table of
9-word entries - 113 unbroken links in page 6 alone, and the same pattern across pages 5 to 8.

**THE FREE LIST IS PERFECTLY INTACT AT THE MOMENT OF THE CRASH.** Every link points where it
should; there is no break, no loop back, no wild value. Whatever `XXNER` was complaining about, it
was **not** a smashed free chain in this table.

(Which table it is, is not yet pinned. The `768`-entry name table is the only one big enough for
5300+ words at 9 words an entry, and 768 minus the 13 in use would leave ~755 free - the right
order of magnitude, but the arithmetic has not been made to come out exactly, so it is NOT claimed.)

### THE SMALL TABLES ARE NOT IN THIS FILE - fetch XMSGDUMP-A next

Page layout of the whole dump, by non-zero words:

```
page  0 : 452     the message table (37 entries x 17 words)
page  1..3 : 0    the rest of the 256-entry message table, unused
page  4..9 : 536  the 9-word table with the intact free list
page 10..11: 0
page 12..13: 175  the five receive frames (313 words each, mostly empty payload)
page 14..18: 0
```

**Every one of page 0's 452 non-zero words lies INSIDE the 37 message entries - not one outside.**
That makes the message-table identification exact rather than approximate.

And it settles where the rest are: the **task (11 in use), port (13), name (13), system (2),
friend (5) and link (1)** tables are all populated on the live machine and **none of them is in
this file**. So they are in **`XMSGDUMP-A`** (segments 33 and 34), which is still on D100 and was
copied to `XMSGDMPA1`.

**That is the next fetch, and it is where the gateway state will be** - which is what the crash
code `XXNER` actually names.

## XMSGDUMP-A - segments 33 and 34, 63 pages / 129024 bytes

Pulled from the SAME crash as the table dump (both copied on D100 at 08.26 machine time before any
later crash could overwrite them), so the two files are a consistent pair.

42.6% non-zero. Three distinct zones:

```
pages  0..9   ~1000 non-zero per page   DENSE - this is CODE
pages 11..29    39-40 per page          a 26-WORD table with an INTACT FREE LIST
pages 34..58  mixed, much of it dense   more code and data
```

### The second free list - pages 11..29, stride 26

Same signature as the 9-word table in `XMSGDUMP-T`, and just as clean:

```
offset 15376 = 3C2A (=15402)    15402 = 3C44 (=15428)    15428 = 3C5E (=15454)  ...
```

Each entry's first word is **exactly the offset of the next entry**, 26 words on. Thirty-eight
consecutive flawless links in page 15 alone, and the chain runs from word 11268 to 29988 - about
**720 entries**.

**Consistent with the 768-entry NAME table** (768 total, 13 in use, so ~755 free), but the count
does not come out exactly, so the identification is **NOT claimed** - only the structure is.

### SO: TWO FREE LISTS, BOTH PERFECTLY INTACT

The 9-word chain in `XMSGDUMP-T` and the 26-word chain here are both unbroken - no wild pointers,
no loops, no truncation. Together with "nothing was exhausted", that is now two independent lines
of evidence saying the same thing:

**XMSG did not run out of anything, and it did not corrupt its own linked structures.** It stopped.

Whatever `XXNER` (network gateway error) is complaining about, it is not a smashed allocator.

### THE TWO ADDRESSES ON THE CONSOLE DO NOT INDEX THESE DUMPS - do not retry this

Tempting shortcut, tried and failed 2026-08-22:

 - **`ERROR 46 IN DUMMY AT 34360`** - `34360` octal is 14576 words, which IS inside
   `XMSGDUMP-A` (64512 words). Word 14576 is **all zeros**, and its neighbourhood is empty. That is
   because `34360` is an address in the RT program **DUMMY's** own address space, not in XMSG's
   segments 33/34. Different address space entirely.
 - **`PHYSICAL ADDRESS 141204`** - octal for 49796, also numerically inside the dump. But it is a
   PHYSICAL address, and these dumps are SEGMENTS. Mapping one to the other needs the segment's
   physical base, which is not recorded anywhere we have.

**So neither number can be used as a direct offset into these files.** To locate the crash site in
the dump, the segment-to-physical mapping has to be established first - or the code found by
matching against the XMSG NPL sources instead.

### THE MACHINE HAS NO NETWORK SERVER AT ALL - and the fatal is a NETWORK GATEWAY error

Asked on the live machine 2026-08-22:

```
X-C LIST-NETWORK-SERVERS,,
Name    Sysid  Link  Network  Xnser-port-Xgate  Rcv-buff-Xmit
                    (empty - not one row)
```

**No network server is configured.** ENNS0 is never started here - `restart-xmsg-cosmos.ps1` only
starts it under `-WithEthernet`, which this lab does not use, and the note there says it killed
XMSG once on its own. All of our traffic is FA over the **HDLC** link 1362.

**And yet every death is `XMSG ERROR CODE 27` = `XXNER`, "Network gateway error"**
(`XMSG-VALUES-M.SYMB:356`). The kernel is built for it - the X-C banner says
`Inter-system: Network gateway/IOC` - but nothing is registered in the network server table.

That is a sharp constraint on where to look, and it cuts two ways:

 - either the **gateway code path also carries HDLC inter-system traffic**, not just Ethernet, and
   the fault is in that shared path;
 - or something is entering the gateway path for a gateway that **is not there**.

The second is worth taking seriously because "not configured" already has its OWN, non-fatal code:
`XRNGA=44  XMSG not congfigurated with gateway code`. So a clean "no gateway" answer exists and is
NOT what is being raised. `XXNER` is a FATAL inconsistency, which means the code got far enough in
to find something it could not reconcile.

**AND THE OBVIOUS NEXT STEP IS NOT AVAILABLE - checked, 2026-08-22.** "Read which routines raise
`XXNER`" cannot be done from this repo: **there is no XMSG NPL source here.** `SINTRAN/NPL-SOURCE/NPL`
is the SINTRAN kernel - disk, SCSI, the HDLC driver, TAD, terminal and X21 drivers, segment admin -
and contains no XMSG, XROUT or gateway module. What we have of XMSG is:

 - `XMSG-VALUES-M.SYMB` / `-L` and the `XMSG-PL-VALUES-*.INCL` files - CONSTANTS only;
 - `SINTRAN/NPL-SOURCE/SYMBOLS/L07/XMSG-SYMBOL-LIST.SYMB.TXT` - a SYMBOL/ADDRESS list
   (it gives `XXNER=000033` octal = 27, which independently confirms the decode);
 - the two dumps in this folder.

**So the routes forward are:**
 1. **The symbol list is an ADDRESS map.** `XMSG-SYMBOL-LIST.SYMB.TXT` names symbols with octal
    addresses. If those addresses are in the same space as the segment dump, the symbol list turns
    `XMSGDUMP-A` from anonymous words into named variables - which is the whole game. Establish the
    base first; do not assume it is zero (see the failed address shortcut above).
 2. Find XMSG's own source or listing in the software archive / floppy images - it is product
    **210373L**, and the X-C banner names that build exactly.
 3. Failing both, disassemble the relevant part of `XMSGDUMP-A` with the ND-100 tooling.

Route 1 is much the cheapest and uses something already on disk.

## THE CRASH SITE IS NAMED: 56 WORDS INSIDE `ZLKIL` - LINK KILL

`SINTRAN/NPL-SOURCE/SYMBOLS/L07/XMSG-SYMBOL-LIST.SYMB.TXT` is a SYMBOL -> OCTAL ADDRESS map with
2136 entries, and the console's `PHYSICAL ADDRESS 141204` lands inside it cleanly:

```
   ZLINI    141046   (-94)    link init
   ZLKIL    141114   (-56)    LINK KILL      <-- the crash is 56 words INTO this routine
>>>          141204            XMSG ERROR CODE 27  (XXNER, network gateway error)
   ZGKIL    141304   (+64)    GATEWAY KILL
   ZLFAL    141547  (+227)    link fail
   ZXDIC    141714  (+328)
   ZXENC    141716  (+330)
```

Neighbours in the same region: `XHLNK` (HDLC link), `ZLINI`, `ZLFAL` - this is the link
management area, and `ZLKIL` is immediately followed by `ZGKIL`.

**`ZLKIL` = LINK KILL. XMSG dies during LINK TEARDOWN.**

**That fits every single observation:**

 - every death follows an FA transfer, and a one-shot transfer ENDS by sending DISC - a link
   teardown. The gap is the teardown path running, not a timer.
 - the console line immediately before a death is repeatedly
   `** XROUT: Link restarted, LUN 1362B **` - the link going down and up.
 - the code raised is a **GATEWAY** error, and the very next routine after `ZLKIL` is `ZGKIL`,
   **gateway kill** - so link teardown calls gateway teardown.
 - **there is no gateway configured on this machine** (`LIST-NETWORK-SERVERS` is empty), so a
   teardown path that walks into gateway cleanup has nothing to find - and `XXNER` is a fatal
   INCONSISTENCY rather than the polite `XRNGA` "not configured".
 - nothing was exhausted and no free list was corrupt, which is exactly what you would expect if
   the fault is a bad pointer/branch on a teardown path rather than resource damage.

**CONFIDENCE, stated honestly:** this assumes the symbol list's address space is the same one the
console reports `PHYSICAL ADDRESS` in. That is NOT proved. What makes it more than a coincidence is
that 2136 symbols bracket the address tightly and the two nearest are `ZLKIL` and `ZGKIL` - kill
routines - on a fault that only ever happens around link teardown. A wrong mapping landing on
exactly those two names by chance is a stretch.

**HOW TO CONFIRM CHEAPLY:** the same symbol list gives `XCRAP` (the saved crash location) and
`XCRAR` (the reason) in the XMSG basefield. Read those out of `XMSGDUMP-A` and check `XCRAP` holds
141204 - that proves the address space and the mapping in one step.

### Still to do

Find the base and stride of the remaining tables (task 80, port 128, name 768, system 512, friend
128, link 4, transmit frame 25) and read the gateway-related ones, since the crash code is
`XXNER` - network gateway. The 6-word island at word 4961 is unexplained. The table layouts are in
the XMSG NPL sources and `XMSG-VALUES-M.SYMB`.

## First look at the table dump (superseded by the section above)

Not yet decoded. What is established:

 - 38912 bytes = 19456 words, **5.1% non-zero** - mostly empty table space, as expected;
 - 658 distinct word values, so it is structured data and not noise;
 - `0064` (100 decimal, **D100's own system number**) appears 84 times;
 - a record shape repeats - the first 32 words recur at the same offsets in the next block.

**To decode it properly**, the table layouts live in the NPL source and the symbol files:
`SINTRAN/XMSG/XMSG-VALUES-M.SYMB` and the XMSG NPL sources. Start from the gateway tables, since
the crash code names the gateway.
