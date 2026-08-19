# Carve answer: when does `*FA-SERVER` give a connection seat back?

**Answers:** `SINTRAN\XMSG\DOC\REQUEST-FA-SEAT-LEAK-INVESTIGATION.md`
**Binary:** `Installation\Communication\COSMOS Basic\x\cos-fa-serv-e04.prog` (231,424 bytes)
**Method:** static read of the Ghidra database for that binary. **No live experiment was run** - the
brief asked for source analysis because three live attempts had already killed the file server twice.
**Date:** 2026-08-18

---

## The answer, in one line

**It never does.** The FA server's only call that changes a free-connection counter is on its
**initialisation** path. No request handler, no `Release` handler and no session-teardown routine in
the program can reach it. **Nothing a client sends makes `*FA-SERVER` call `XMPINFC`.**

That is answer 4 of the four the brief said it would accept.

---

## 1. Is the seat logic in the NPL sources? No - and here is the search

Searched both `SINTRAN\NPL-SOURCE\` and `SINTRAN\NPL-SOURCE-2\`:

| Symbol | Hits in any NPL listing |
|---|---|
| `XMPINFC` | **none anywhere in the repo's NPL trees** |
| `FA-SERVER`, `QFORM`, `XSGSP` | **none** |
| `XSNSP` | **symbol tables only, never a listing** |

The `XSNSP` hits are all definition lines in the tables, not code:

```
NPL-SOURCE/SYMBOLS/J/SYMBOL-1-LIST.SYMB.TXT:2124     XSNSP=000121
NPL-SOURCE/SYMBOLS/K03/SYMBOL-1-LIST.SYMB.TXT:3005   XSNSP=000121
NPL-SOURCE/SYMBOLS/L07/XMSG-SYMBOL-LIST.SYMB.TXT:825 XSNSP=000121
NPL-SOURCE/SYMBOLS/L07/l07-kallsyms.txt:4776         0x51 T XSNSP
```

Zero hits in `S3VS-6.SYMB`, in `s3vs-4.symb`, or in any of the split NPL files. The earlier audit was
right: **the FA server and QFORM are a COSMOS product, not part of the SINTRAN kernel listing.**

Free cross-check from those two lines, per the `nd100-symbols` habit: `000121` octal = `0x51` = 81
decimal. The octal and hex tables agree, so the service number is settled.

## 1b. Correction to the brief's Ghidra plan: segment 73B is the wrong artifact

The brief said to carve `FA-SERVER-TAD`, "segment 73B", out of the carved segment set. That does not
work, and it is worth writing down so nobody spends a day on it:

- In `tools\sintran-segment-carver\versions\L-VSX-500\segments\`, **segment `073` is `S3IDMWD`**, an
  ordinary SINTRAN system segment. The reentrant-subsystem numbers printed by `LIST-REENTRANT` on
  D100 are that machine's, and they do not line up with this carved image.
- The carved set holds 95 segments and stops at `141`. The COSMOS pieces in it are `130-CFT`,
  `132-CCT`, `135-XFTRAD`, `136-FSASG`, `137-COSPOOL`. **There is no FA-SERVER segment.**

**You do not need one.** The file server ships as an ordinary `:PROG` file and it is already in the
repo, already loaded in Ghidra, and already ~190 functions named by an earlier pass:

```
Installation\Communication\COSMOS Basic\x\cos-fa-serv-e04.prog
SINTRAN\XMSG\DOC\COSMOS-RE\Analysis\COS-FA-SERV-E04-Analysis.md
```

## 2. The one place the server touches a seat count

### 2.1 How to find it: the service number is a byte in the letter

`fa_server_main` (0x0500) builds its XROUT name-registration letter like this - straight from the
disassembly at ram:0518-0531:

```
ram:0518: f1 01   SAA 0x1     ->  nd100_sbyt(1,    buf, 0)     byte 0
ram:051a: f1 45   SAA 0x45    ->  nd100_sbyt(0x45, buf, 1)     byte 1 = service
```

`0x45` = `000105` octal = **XSGNI**. So the service number is **byte 1 of the letter**, and it is
loaded with `SAA <service>`, which encodes as the two bytes `f1 <service>`.

**XSNSP would therefore appear as `f1 51`.** That is a distinctive constant, and the hit count is the
smallest it can be - which is the only condition under which a value scan is trustworthy on this
architecture:

| Pattern | Meaning | Occurrences in the whole 231KB binary |
|---|---|---|
| `f1 45` | `SAA 0x45` = XSGNI | **1** (ram:051a, the registration letter - the control) |
| `f1 51` | `SAA 0x51` = XSNSP | **1** (ram:9fac) |

### 2.2 What is at ram:9fac

`ram:9f74`, now named **`xrout_send_xsnsp_letter`**. Real PLANC routine (`RADD SL, DX` prologue).

```
ram:9fa8: f1 07   SAA 0x7          )
ram:9fa9: c8 69   SWAP CLD SA, DD  )  array descriptor (buf, 0, 7) = an 8-byte letter
ram:9faa: 51 8e   LDT -0x72,B      )
ram:9fab: 34 06   STF 0x6,X        )
ram:9fac: f1 51   SAA 0x51            <-- XSNSP
ram:9fad: 0c 09   STA 0x9,X
ram:9fae: 49 89   LDA -0x77,B         <-- 4th argument
ram:9faf: 0c 0a   STA 0xa,X
ram:9fb0: ba 40   JPL 0x9ff0          -> letter_put_byte_pair_at_offset (0xa25a)
```

The callee `0xa25a` (now **`letter_put_byte_pair_at_offset`**) was decoded instruction by
instruction. It rejects an odd byte offset (`RDIV ST` by 2, remainder non-zero -> return -1), then:

```
ram:a26b: 49 89   LDA -0x77,B      ; the 0x51
ram:a26d: f3 01   SAX 0x1
ram:a26e: c5 80   SBYT             ; -> BYTE 1 of the word
ram:a26f: 49 8a   LDA -0x76,B      ; the 4th argument
ram:a271: f3 00   SAX 0x0
ram:a272: c5 80   SBYT             ; -> BYTE 0 of the word
```

So the letter is `byte1 = 0x51 = XSNSP`, `byte0 = <4th argument>`. The COSMOS Programmer Guide
ND-60.164.3 p.91 says of `XMPINFC`:

> `serialNumber` is put into byte 0 of the request sent to XROUT to allow the caller, who may have
> many requests outstanding at the same time, to recognize the reply.

**Two independent sources, same layout.** This is `XMPINFC`, built by hand rather than through the
PLANC library.

The letter is then sent the ordinary way - `XFGET` (pool word 0x9ff1 -> `xmsg_XFGET` @a0e6),
`XFWRI` (0x9ff2 -> `xmsg_XFWRI` @a143), `XFSND`, with `XFREL` (0x9ff3 -> @a0fc) on the error paths.

## 3. Who calls it, and when - the whole chain

Every hop below was checked for a real PLANC prologue (`RADD SL, DX` / `COPY SL, DX`), which is the
fragment test from `COS-FA-SERV-E04-Analysis.md` section 6.4.

```
fa_init_global_registries   0x3fee   [RADD SL, DX]   <-- INITIALISATION
  ram:400c  JPL pool 0x4045 -> 0x7d99               [COPY SL, DX]
    ram:7e03  JPL pool 0x7e27 -> 0x9245             [RADD SL, DX]
       (siblings for rec[2],rec[3],rec[4]: 0x8f7c, 0x90df, 0x91f6 - this is the rec[5] one)
      ram:92af and ram:9346 -> 0x8d74               [RADD SL, DX]
                              fa_xmpinfc_change_free_connections
        ram:8d88 -> 0x9f74    xrout_send_xsnsp_letter
```

**Both call sites pass the identical triple**, read straight off the disassembly:

```
ram:92a9: 4c 14   LDA 0x14,X    ; portNumber   = record[0x14]
ram:92ab: f1 01   SAA 0x1       ; extraConn    = +1
ram:92ad: f1 24   SAA 0x24      ; serialNumber = 0x24 (36)
ram:92af: ba 16   JPL 0x92c5    -> 0x8d74

ram:9340: 4c 14   LDA 0x14,X    ; portNumber   = record[0x14]
ram:9342: f1 01   SAA 0x1       ; extraConn    = +1
ram:9344: f1 24   SAA 0x24      ; serialNumber = 0x24 (36)
ram:9346: ba 07   JPL 0x934d    -> 0x8d74
```

`extraConn` is **+1**, never a bulk figure, at both sites. The first is guarded by `if (arg == 1)`
(ram:92a5-92a8); the second is not.

### 3.0 CORRECTION, same day: how much of section 3 is Ghidra's word, not the bytes'

**Ghidra's function boundaries in the 0x9cxx-0x9fxx region are badly wrong.** Decompiling anything
in that span returns a single merged giant that runs from `0x9c25` through `0x9fxx`, and it prints
the XSNSP letter build at its tail - the SAME site at `ram:9fac`, absorbed by the bad boundary, not
a second call. Anyone reading that decompile will think they have found a second emission site on
the request path. They have not.

So the two claims in this document are not equally strong, and they must not be quoted as if they
were:

| Claim | Rests on | Strength |
|---|---|---|
| **Exactly one XSNSP emission site in the binary** | the `f1 51` byte search, which is independent of function boundaries | **PROVEN** |
| The letter is XMPINFC (byte1 = service, byte0 = serialNumber) | instruction-level read of `0xa25a` + the guide | **PROVEN** |
| **Reachable only from the initialisation path** | Ghidra's xref graph, whose function analysis is demonstrably unreliable HERE | **PROBABLE, NOT PROVEN** |

The pool-word arithmetic for `0x8d88 -> 0x9f74`, `0x9346 -> 0x8d74` and `0x7e03 -> 0x9245` was
re-checked by hand and is right. What is not established is that Ghidra found EVERY caller. Treat
"init only" as the best current reading, not as a byte-proven fact, and re-derive it from the call
sites rather than from the xref window if anything is ever built on it.

### 3.1 The negative, which is the actual answer

`xrefs` gives the complete picture, and it is small enough to state exhaustively:

- `xrout_send_xsnsp_letter` (0x9f74) has **exactly one** caller: ram:8d88.
- `fa_xmpinfc_change_free_connections` (0x8d74) has **exactly two**: ram:92af, ram:9346.
- Both live in 0x9245, whose only caller is ram:7e03, inside the routine at 0x7d99, whose only caller
  is ram:400c, inside **`fa_init_global_registries`**.

So the seat call is reachable **only** from initialisation. None of the named teardown routines
reaches it:

| Routine | Address | Reaches XSNSP? |
|---|---|---|
| `fa_release_all_session_entries` (file-entry-disconnect) | 0x27f4 | no |
| `fa_release_file_entry_op` | 0x34cd | no |
| `fa_close_file_decrement_ref` | 0x2f2d | no |
| `fa_delete_file_entry_op` | 0x34f8 | no |
| the request engine and its QFORM parsers | 0x8c5d family | no |

**There is no conditional to satisfy, because there is no call to reach.** The brief asked "what must
a CLIENT send to make that happen" - nothing can, and that is why every substitution experiment on
the teardown frame failed to move the counter.

## 4. A trap this carve walked into - recorded so the next reader does not

Ghidra's function boundaries in the 0x7dxx region are **phantom splits**, exactly as
`COS-FA-SERV-E04-Analysis.md` section 5 and 6.4 warn.

`FUN_ram_7dcf` decompiles into a plausible, readable routine that "zeroes a 21-word record and calls
four sub-routines". **It is not a function.** ram:7dcf has no prologue - it begins mid-expression:

```
ram:7dcf: 4c 03   LDA 0x3,X
ram:7dd0: a0 51   MPY *0x7e21
ram:7dd1: 61 92   ADD -0x6e,B
```

The real routine begins at **0x7d99** with `COPY SL, DX`. Taking the decompiler's word for it would
have attributed the seat call to the tail of `qform_read_tag_and_value`, which parses request bodies
- and would have produced the exact opposite answer: "the seat is returned while parsing a request".
**Apply the prologue test at every hop, not just the interesting one.**

## 5. What this means for the seat leak - and the one thing still inferred

The leak itself was fixed earlier the same day (commits `a6e8f047`, `ec7f0772`): our client was
sending the server's `Close` (`07C0`) with the two conversation numbers swapped and the wrong frame
flags, instead of the client's `Release` (`0782`). Measured after: 3 pushes + 2 pulls, `*FA-SERVER`
steady at 29 free. See `fa-transfers-leak-connection-seats` and `DOC\CARVE-FA-SEAT-LEAK-2026-08-18.md`.

This carve explains **why that fix worked, and why none of the three earlier attempts could have**:

- **[PROVEN]** The server never asks XROUT for a seat back. `XMPINFC` is used once, at start-up, to
  publish the pool - which is what `START-SERVER 1` reports as "No of FACs attached: 30".
- **[PROVEN]** No `XSNSP` letter crosses the link either: the ND-to-ND capture
  `DOC/captures/ND-TO-ND-WRITE-2026-08-10/nd-to-nd-write.pcapng` carries exactly one XROUT-addressed
  frame in two complete file writes, the client's opening connect letter.
- **[INFERRED - not proven by this carve]** The seat must therefore be credited back by **XMSG/XROUT
  itself when the connection is torn down**, triggered by the client's `Release` and the server's
  answering `Close`. The measured post-fix behaviour requires it, and the FA server demonstrably has
  no other way to do it, but this carve read the FA server - **not** XROUT - so the mechanism inside
  XROUT is not shown here. **The guide never documents an automatic restore**: it only ever describes
  XROUT decrementing on forward and `XMPINFC` incrementing. Answer route: CARVE the XMSG kernel /
  `077-S3XROU` for the writers of the free-connection counter.
- **[PROVEN, and it is what makes the inference above reasonable]** The server DOES close the session
  port on teardown. The loop at `ram:9c5a-9c70` walks the session chain and, for each record, closes
  that record's port with the `XFCLS` wrapper (`0x8da7`, through pool word `0x9c7f`). The port it
  closes is **`rec[0x14]`** - the SAME field the init path hands to XSNSP as `portNumber`. The seat
  and the port are attributes of one record.

### 5.1 The cheap, SAFE measurement that would settle it

Read `X-C` -> `LIST-NAMES` three times: **idle, during a transfer, idle again.**

- `30 -> 29 -> 30` = the seat IS returned per transfer, and the teardown is the trigger.
- `29 -> 29 -> 29` = one seat is held by a persistent conversation and transfers reuse it.

**Read-only** - it sends no frames and cannot kill the file server the way the three earlier
teardown-frame experiments did. Do this before carving XROUT; it may make the carve unnecessary.

**Practical consequence:** there is nothing further to change in our client for this. The seat is not
an FA-level object we can hand back with a message; it is released by ending the conversation
correctly, which we now do on both the success and the refusal paths.

## 6. Ghidra database changes made by this carve

In `cos-fa-serv-e04.prog`:

| Address | Was | Now |
|---|---|---|
| ram:9f74 | `FUN_ram_9f74` | `xrout_send_xsnsp_letter` |
| ram:8d74 | `FUN_ram_8d74` | `fa_xmpinfc_change_free_connections` |
| ram:a25a | `FUN_ram_a25a` | `letter_put_byte_pair_at_offset` |

Both renamed XROUT routines carry a decompiler comment with the evidence above, including the
`f1 51` / `f1 45` search counts and the phantom-split warning.

In `XMSG-KERNEL-L03_flat_be.bin`: a plate comment at `ram:b30c` (see section 7). The region was
undisassembled; `disassemble` was run over `0xb2c6-0xb33e` first.

---

# 7. THE TRIGGER, FOUND. Section 5's open inference is CLOSED

**Added later the same day, after "the guide doesn't document it, so carve it" - which was the right
call.** The mechanism is not in the guide because it is not a programmer-visible API at all. It is an
internal kernel-to-XROUT signal, and it is one bit.

## 7.1 ND says it in their own commented symbol file

`F:\ND\SINTRAN-K05-XMSG-2026\FLOPPY\xmsg\xmsg-poftabs-l03.symb` (ND parity text; decode with
`b & 0x7F`, drop NULs) defines the port block and the bits of its status word:

```
DISP 5PLEN=0                 % DEFINE THE STRUCTURE OF A PORT
       INTEGER XPCHN         % CHAIN FOR FREE OR PORTS FOR ONE PROCESS
       INTEGER XPSTA         % PORT STATUS WORD.
       ...
% BITS IN XPSTA
SYMBOL 5PKOC=0            % KICK XROUT ON CLOSE (SET BY XROUT)
SYMBOL 5PKIK=5            % XROUT/COSROUT has been kicked (only on routing ports)
SYMBOL 5PIMX=16           % IMMEDIATE EXEC OF CODE -> XPIMC ON CHAIN ATTEMPT
SYMBOL 5PACT=17           % PORT ACTIVE
```

**`5PKOC` = bit 0 of the port status word = "KICK XROUT ON CLOSE (SET BY XROUT)".** XROUT marks the
port itself when it spends a seat; the kernel honours the mark when the port closes.

(MAC numbers are octal, so `5PIMX=16` is bit 14 and `5PACT=17` is bit 15. `XPSTA` is word 1 of the
port block, which is why the code below reads `0x1,X`.)

## 7.2 And the kernel does it, in bytes

`YCLOS = 131306` (from `xmsg-kernel-l03-symbols-decoded.txt`) is the kernel's port-close routine.
`131306`octal = `0xb2c6` in the flat image, which is the base the existing Ghidra program uses.
Its tail:

```
ram:b30c: 59 55   LDX 0x55,B        ; X = the port block
ram:b30d: 4c 01   LDA 0x1,X         ; A = XPSTA
ram:b30e: fa 85   BSKP ONE bit 0    ; 5PKOC - "kick XROUT on close"
ram:b30f: a8 02   JMP *0xb311       ; taken when 5PKOC is CLEAR: ordinary port, no kick
ram:b310: ba 17   JPL [pool 0xb327] ; pool word = 0xb330 = 131460 = YKROU   <== KICK XROUT
ram:b311: 59 55   LDX 0x55,B
ram:b312: 04 01   STZ 0x1,X         ; clear XPSTA
```

`131460` is **`YKROU`** in the kernel symbol list - Y-routine, Kick ROUt.

**BSKP decode, cross-checked three times.** The bit field prints as `bit << 3` and the sub-operation
is `ZRO` below `0o200`, `ONE` at or above it:

| bytes | octal | sub | field | bit | lands on |
|---|---|---|---|---|---|
| `fa 85` | `175205` | ONE | `0o5` = 5 | `5>>3` = **0** | **5PKOC** |
| `fa 75` | `175165` | ZRO | `0o165` = 117 | `117>>3` = 14 | 5PIMX (`=16`octal) |
| `fa ad` | `175255` | ONE | `0o55` = 45 | `45>>3` = 5 | 5PKIK |

Three independent decodes, three defined bits of `XPSTA`. A wrong decode would not land on named
bits three times.

## 7.3 The whole flow, end to end

1. Client sends its connect letter to the name `*FA-SERVER`.
2. **XROUT decrements the free-connection counter** and forwards the letter. (Guide, lines 1190 and
   3381.) The seat is spent here.
3. **XROUT sets `5PKOC` on the port** - "kick XROUT on close", its own note to the kernel.
4. The FA server serves the session on that port.
5. Client sends `Release` (`0782`); the server answers `Close` (`07C0`) and **closes the port** -
   the `XFCLS` loop at `ram:9c5a-9c70` in `cos-fa-serv-e04.prog`.
6. **`YCLOS` sees `5PKOC` and calls `YKROU`** - the kernel kicks XROUT.
7. XROUT wakes and does the cleanup, restoring the seat.

Step 7 is the only one not read instruction by instruction, and it is no longer an open question of
mechanism: XROUT is explicitly notified, by a bit XROUT set for exactly this purpose, named after
exactly this purpose.

## 7.4 Everything this explains

- **Why the FA server never calls XMPINFC per session** - it does not have to. The kernel tells
  XROUT.
- **Why nothing shows on the wire** - the kick is local, between the kernel and XROUT.
- **Why the guide is silent** - `5PKOC`/`YKROU` are internal; `XMPINFC` is the programmer-visible
  way to change the maximum, which is a different operation.
- **Why our `Release` fix worked.** The Release is what makes the server close the port. Sending the
  server's own `Close` instead meant the server never concluded the session, never closed the port,
  `5PKOC` never fired, XROUT was never kicked - and the seat stayed spent. **The leak was three hops
  upstream of the counter.**
- **Why "give the seat back yourself" was never going to work.** Every experiment aimed at the
  teardown frame's opcode and flags was aiming at step 5; the fix had to make step 5 actually happen.
