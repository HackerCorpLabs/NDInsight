# Handoff: the COSMOS FA file server, end of 2026-08-04

**Read this first.** It replaces nothing - the detail lives in
[FA-EXCHANGE-MODEL-AND-DIRECTORY-WALK-2026-08-04.md](FA-EXCHANGE-MODEL-AND-DIRECTORY-WALK-2026-08-04.md)
and [NOTES-FOR-RONNY-2026-08-04.md](NOTES-FOR-RONNY-2026-08-04.md) - but this is the shortest path
back into the work.

---

## 1. Where it stands

A real SINTRAN III machine (D100) drives our C# `*FA-SERVER` over HDLC through a complete
conversation: connect, reserve, a two-entry directory walk, clean teardown. No rejections.

**SOLVED 2026-08-05 - the listing prints.** See
[FA-LISTING-RECORD-ACCESS-WORD-2026-08-05.md](FA-LISTING-RECORD-ACCESS-WORD-2026-08-05.md).

```
FILE 0 : D103.(PACK-ONE:SYSTEM)HELLO:TXT;1
FILE 1 : D103.(PACK-ONE:SYSTEM)README:TXT;1
FILE 2 : D103.(PACK-ONE:SYSTEM)THIRD:TXT;1
```

The cause was NOT in the record. **Every request was read as "give me the next file"**, so the two
requests that ask for the DIRECTORY and the USER - the two halves of the `(PACK-ONE:SYSTEM)` header
- were answered with file records, and the client gave up after the second file whatever the folder
held. Four genuine record defects were fixed on the way there and none of them changed what D100
did; only the dispatch did.

Everything below this line is the state BEFORE that, kept for the reasoning.

571 tests pass. Everything below is committed on branch `5000x`.

---

## 2. The one that mattered most - we were crashing the machine

`XmsgFrameBuilder.BuildShort` FABRICATED header word 6 from the superseded channel/counter model.
Word 6 is a **checksum, and D100 validates it**. Our restart announce therefore sent

```
2113 0019 0064 0067 FFFF 0001 DE00        correct: DE07
```

on every link-up, and D100 died:

```
ERROR 46 IN DUMMY; XMSG FATAL ERROR - INTERNAL ERROR OR INCONSISTENCY
XMSG ERROR CODE: 24
```

Most of the day's "the machine keeps crashing / the HDLC line is dead / FILE-ACCESS NOT RUNNING OR
CRASHED" was **self-inflicted**. Fixed in `429d96e`; `BuildShort` now computes it.

**Lesson worth keeping: when the machine starts misbehaving, dump what WE sent and checksum-check
it before touching the machine.** Ronny spotting the console line is what broke this open.

---

## 3. The protocol, as now established

All measured against `DOC\captures\FA-READ-WRITE-2026-08-04\`.

| Rule | Evidence |
| --- | --- |
| A request is answered by a **ShortAck** at its Flags 1; the reply is a NEW exchange one higher | unbroken through a 100-file listing |
| **XMCSM / Flags2 is the frame's own body length** | every Data frame, all four captures |
| **Flags 1 is PER DIRECTION** - each side counts the Data frames IT sends | 222 matches, 0 mismatches |
| An FA body is **word aligned** - pad odd lengths with one zero byte | 480 bodies even, 0 odd |
| The **server** holds the walk position; cursor `FFFF` = restart, `0000` = next | `A2 FFFF` once, `A2 0000` 102 times |
| `0x078x` means "finished" and wants a **Close** | the captured teardown |
| Listing record: walk ordinal at bytes 22-25 and 35; `0x000078DA` at 60-63 | 49 records compared field by field |

The **echo** model for Flags 1 (one shared number, echoed by the answer) reproduces every capture we
hold, but only because the conversation strictly alternates so two counters move in lockstep. It is
wrong, and it is what earns XENSE and the 24B crash.

---

## 4. What is left

**The listing still does not print.** After all of the above, D100 serves both files, finishes with
`0782`, and then retries. A real `LIST-FILES` uses exactly ONE connect; ours provokes a retry.

**UPDATE 2026-08-05 - bytes 26-29 are closed, and one of them was a real defect.** See
[FA-LISTING-RECORD-ACCESS-WORD-2026-08-05.md](FA-LISTING-RECORD-ACCESS-WORD-2026-08-05.md). In
short:

 - Bytes 26-27 are the SINTRAN **file ACCESS word**, and we were sending **zero** - a file nobody
   may read, write or append, not even its owner. Now `0x04F7`, what an ordinary user file carries.
   **That is the leading suspect for the retry, and it is NOT yet live-verified.**
 - Bytes 28-29 are the attribute word and were never wrong. The diff had compared our indexed user
   file against `SINTRAN:DATA`, an allocated system file.
 - Two things below this line are now known to be **wrong**: bytes 60-63 are not constant across the
   49 records (only the low page id is), and bytes 22-25 are the version pointers, not the walk
   ordinal. Both corrected in code.

All of it came from manuals already in the repository. **Grep the manuals first** - this is the
second time that has cost a day.

**A live run on 2026-08-05 07:20 confirmed the access word reaches the wire and did NOT fix the
listing.** The conversation was completely clean - zero XENSE, zero rejections - and D100 still
retried. It also never asked for a third entry, so the end-of-directory reply has still never been
exercised. Read section 8 of the new document before planning the next run.

Also open:

 - **Ethernet is separately broken.** The HDLC A/B test proved it: identical upper stack, HDLC
   reaches the directory walk, Ethernet is torn down (`kind 0x60`) right after the connect confirm.
   D19999-over-Ethernet is blocked in `NdLinkLayer` / `EthernetLink` or the card, NOT in XMSG or FA.
 - `XmsgNode.ResyncAcceptDown` ("step the accept down one per XENSE") is a symptom of the old echo
   model. It should go now the model is right.
 - ~~The packed ND date spans only 1950-2013, so every file we serve shows **no date**.~~ **FIXED
   2026-08-05, and it was not cosmetic:** the creation date is non-zero on **all 49** captured
   records - the "real records carry 0000 too" excuse was only ever true of the last-opened fields.
   `ToListingDate` now folds the year by 64-year cycles into the range the format holds.

---

## 5. Running a live test without wasting an hour

The traps, all paid for today:

1. **Only an EMULATOR restart resets D100's Flags 1.** `STOP-X`/`START-X` does NOT. Both sides must
   start from zero or the conversation dies in silence. Ronny has granted permission to restart
   RetroCore for D100 (`F:\RC\RonnyTest\HDLC1`) and D102 (`F:\RC\RonnyTest\HDLC2`), no arguments,
   working directory must be that folder. Allow a couple of minutes to boot.
2. **Stop the runner BEFORE restarting anything**, and only then delete `xmsg-sequence.state`.
3. **Redirect the runner's stdout** (`-RedirectStandardOutput`) or every log line blocks ~1s on a
   hidden console and D100 times out waiting for the connect confirm.
4. **The runner locks the DLLs** - stop it before building, and remember `dotnet test` does NOT
   rebuild it.
5. **A dead HDLC link usually means XMSG is down**, not that the link needs cycling. `X-C` answering
   `-45` is the tell; `STOP-X` / wait 10s / `START-X` inside `@SIN` fixes it. There is no
   `STOP-LINK` command, and `LIST-LINK` prompts `XROUT system?` rather than taking a number.
6. **`START-NET-SERVER,ENNS0` can take XMSG down with it** (once in eleven restarts). It is the
   ETHERNET server and is not needed for an HDLC-reached system - now behind `-WithEthernet`, off by
   default. When you do want it, `@ABORT ENNS0` first (a SINTRAN command, not an X-C one) then wait
   10 seconds.

The script is `tools\restart-xmsg-cosmos.ps1`; the terminal driver is `tools\ndterm.ps1`. Both are
documented in `tools\README.md`.

Test line:

```powershell
.\ndterm.ps1 -Port 9010 -User SYSTEM -Steps "LIST-FILES","D103(sys).","" -SettleMs 30000
```

---

## 6. Commits, newest first

| | |
| --- | --- |
| `9cd7073` | restart announce OFF - it drops the link and does not resynchronise |
| `f290e6d` | object index is the walk ordinal, not the file number |
| `429d96e` | **short frames COMPUTE word 6 - stops us crashing D100** |
| `0b5dd04` | tests for the listing-record fields |
| `a85ad3e` | walk ordinal at 22-25, `0x78DA` at 60-63 |
| `beec9e3` | FA bodies are word aligned |
| `48bf0a6` | **Flags 1 is per direction** |
| `a2239db` | the second `A2` is the entry ordinal |
| `c22562a` | server holds the walk position; `078x` wants a Close |
| `e764b65` | **ShortAck answers; the reply is the next exchange** |
| `d6ec806` | ndterm login waits for its prompts |
| `e14a257` | ENNS0 opt-in, ABORT + 10s wait |

`beec9e3` is the last commit with a fully verified good live run behind it.
