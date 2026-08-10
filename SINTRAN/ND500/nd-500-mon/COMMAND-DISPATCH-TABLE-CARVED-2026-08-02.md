# ND-500-MON J04: the outer command dispatch table, carved

**Date:** 2026-08-02
**Subject:** `MON-DEBUG:PROG` (ND-500/5000 monitor, version J04)
**Status:** `[V]` - byte-verified, and the dispatch instruction decoded field by field.

This closes **open question 9** of
[`nd-500-mon-j04.prog.md`](nd-500-mon-j04.prog.md) ("Command-name -> handler-address
binding"), which had stood as `PARTIAL` since the original carve: the command-name
strings were extracted but the table binding a name to a handler address had never
been located, so every name -> subfunction claim was name-correspondence only.

It is now located, and the binding is exact.

---

## 1. The answer in one line

Command ordinal *i* -> handler address = **`bank2[0o020671 + i]`**, a plain
one-word-per-command array of bank-1 code addresses, 151 entries long.

---

## 2. The dispatch instruction

Three words in bank 1 at `003260`:

```
003260  146157   RADD CLD SA DX     ; X := A            (A = the command ordinal)
003261  057050   LDX I ,X 50        ; X := bank2[ bank1[003331] + X ]
003262  126000   JMP ,X 0           ; jump to that address
```

`057050` decodes as opcode `054` (LDX) with **I = 1 and X = 1**. On the ND-100 that
combination is **post-indexing**: the effective address is `M[P + disp] + X`, not
`M[P + disp + X]`. So:

- `P + disp` = `003261 + 0o50` = `003331`
- `bank1[003331]` = `0o020671`  <- the table base, held in the pointer pool at
  `003314`-`003333` (the disassembler renders that pool as garbage instructions;
  it is data)
- final operand `bank2[0o020671 + X]` - the indexed load runs with PTM = 1, so it
  reads the **data** bank, exactly the "pointer word from bank 1, final operand
  from bank 2" rule already established in section 2.2 of the main analysis

`JMP ,X 0` then jumps to the word just loaded. One indexed load and one jump - there
is no thunk, no parameter block and no intermediate table on this path.

**Note the sequence is only three words long.** That is why it was never found by
searching for a large dispatch structure: the structure is the array in the *data*
bank, and the code that uses it is three instructions in the middle of the command
loop.

---

## 3. How the ordinal is produced: the descriptor array

The command names live at `bank2` `012454`-`020050` as a packed text region. They are
indexed by a **3-word-per-command descriptor array at `bank2` `011547`**, running
`011547`-`012453` - 151 x 3 = 453 words, ending exactly where the string region
begins:

```
+0   name pointer  (into 012454..020050)
+1   0             (always zero in all 151 entries)
+2   byte length of the entry text
```

The entry text is `NAME` `\` `<parameter descriptor>`, so the command name is
everything up to the first backslash; commands that take no parameters have no
backslash at all. Examples read verbatim from the bytes:

```
RECOVER-DOMAIN\F DSCRATCH'Domain name:
OUTPUT-FILE\F D1'File name:
CONTINUE
```

The descriptor array and the handler table are **parallel and the same length**, so
the ordinal that indexes one indexes the other.

---

## 4. Why the extent of the table is not a guess

Three independent checks, all from bytes:

1. **The descriptor array ends where the string region starts.** 151 entries x 3
   words from `011547` lands on `012454`, which is the first command name (`EX`).
   A 150- or 152-entry reading does not.
2. **The word after the handler table is `0o000010`** - not a plausible bank-1 code
   address, and nothing like the values inside the table.
3. **Two entries collide, and the collision is meaningful.** Entry 0 (`EX`) and
   entry 7 (`EXIT`) both dispatch to `010570`. `EX` is the abbreviation of `EXIT`,
   so the two ordinals sharing one handler is the expected result, not a
   misalignment artefact. Entries 20 (`RESET-TRACE`) and 22 (`RESET-GUARD`) share
   `004207` for the same kind of reason.

---

## 5. RECOVER-DOMAIN, worked end to end

This is the command that motivated the carve, so it is written out in full.

**Ordinal 8** -> `bank2[0o020671 + 8]` = `bank2[020701]` = **`003577`**.

```
003577  034665   LDF ,B -113      ; the parsed parameter (domain name)
003600  054602   LDX ,B -176      ; X := stack top = base of the callee frame
003601  032006   STF ,X 6         ; place it at callee local +6
003602  135151   JPL I 151        ; -> pointer at 003753 = 030302   <- the worker
003603  135145   JPL I 145        ; -> pointer at 003750 = 002673   <- error return
003604  125323   JMP I -55        ; -> 003527, back to the command loop
```

The `LDX ,B -176` / `STx ,X 6` idiom is the documented caller-marshals-into-callee
convention from section 4.4 of the main analysis, and `003527` is the command loop's
re-entry point - it is also the target of `003340 JMP I 167`. The handler is
coherent with everything already known about this program's calling convention.

**So the RECOVER-DOMAIN worker is `bank1` `030302`**, and it begins a normal routine:

```
030302  146547   RADD AD1 CLD SL DX
030303  135077   JPL I 77         ; -> 030402
030304  000004
030305  124020   JMP 20           ; -> 030325
...
```

Carving that routine is the next step (task 1.6) and is what open question 9 was
blocking.

---

## 6. What this does NOT settle

- **The parameter descriptor language is only partly read.** `\F D<default>'<prompt>`
  is clear enough to recover names and prompts, but the meaning of each type letter
  (`F`, `D`, `O`, `M`, `E`, `I`, `W`, `R`, `P`) is `[OPEN]`. Nothing in this document
  depends on it.
- **The handler addresses are verified as table contents, not as routines.** Only
  `003577` (RECOVER-DOMAIN) and its callee `030302` were disassembled and checked for
  coherence. The other 149 are read correctly out of the table but have not each been
  confirmed to start a real routine.
- **The relationship to the MON 60 thunk table is not traced.** The thunks at
  `146310`-`147070` (123 x 3 words, section 8.3 of the main analysis) are reached
  from inside the handlers, not from this table. Which handler calls which thunk is
  still open, and that - not this table - is what finally binds a command name to a
  MON 60 subfunction number.

---

## 7. Method note

The table was **not** found by scanning for it, and two scans that looked reasonable
both failed first:

- Scanning bank 1 for a word holding the string-table base `012456` found nothing
  (the base is held in bank 2, at `011552`, as part of the descriptor array).
- Scanning for a 151-word window of plausible code addresses returned 8 candidate
  windows in bank 1 and 65 in bank 2, **all false** - the bank-2 hits were ASCII
  text whose word values happen to fall in the code-address range, and the bank-1
  hits were ordinary code.

What worked was searching for the **descriptor array base** `0o011547` as a constant.
That value has exactly **one** occurrence in bank 1, at `003321` - inside the pointer
pool of the command loop itself, three words away from the dispatch. A single-value
search on a distinctive constant beat every range scan, which is the same lesson
already recorded in the carving skill: a range scan over addresses that overlap
instruction encodings is noise, and picking the *most distinctive* constant available
is what makes the search decisive.

---

## 8. Reproducing it

```powershell
cd E:\Dev\Ronny\NDInsight\SINTRAN\ND500\nd-500-mon
wsl python3 <path-to>/gen_cmdtable.py nd-500-mon-j04-bank1.bin nd-500-mon-j04-bank2.bin
```

The generator walks the descriptor array from `0o011547` and reads
`bank2[0o020671 + i]` for each entry. It terminates on **two** conditions - the name
pointer leaving the string range, and the middle word being non-zero - so a single
bad word cannot run it off the end of the array.

---

## 9. The full map

151 commands, in table order. Addresses are octal. "Handler" is a **bank 1** code
address; "Descriptor" and "Name ptr" are **bank 2** word addresses.

| # | Command | Descriptor | Name ptr | Handler (bank 1) |
|---|---|---|---|---|
| 0 | `EX` | `011547` | `012454` | `010570` |
| 1 | `GO` | `011552` | `012456` | `003263` |
| 2 | `CONTINUE` | `011555` | `012466` | `003341` |
| 3 | `RUN` | `011560` | `012473` | `003371` |
| 4 | `HELP` | `011563` | `012475` | `003433` |
| 5 | `OUTPUT-FILE` | `011566` | `012507` | `003456` |
| 6 | `CC` | `011571` | `012526` | `003576` |
| 7 | `EXIT` | `011574` | `012530` | `010570` |
| 8 | `RECOVER-DOMAIN` | `011577` | `012533` | `003577` |
| 9 | `LOOK-AT-PROGRAM` | `011602` | `012557` | `004036` |
| 10 | `LOOK-AT-DATA` | `011605` | `012613` | `004076` |
| 11 | `LOOK-AT-STACK` | `011610` | `012641` | `004011` |
| 12 | `LOOK-AT-RELATIVE` | `011613` | `012650` | `003736` |
| 13 | `LOOK-AT-REGISTER` | `011616` | `012672` | `003605` |
| 14 | `LOOK-AT-FILE` | `011621` | `012714` | `003631` |
| 15 | `INSPECT-DUMP` | `011624` | `012742` | `003671` |
| 16 | `RESET-INSPECT-DUMP` | `011627` | `012760` | `003733` |
| 17 | `MAIN-FORMAT` | `011632` | `012772` | `004160` |
| 18 | `EXTRA-FORMAT` | `011635` | `013011` | `004136` |
| 19 | `TRACE` | `011640` | `013026` | `004166` |
| 20 | `RESET-TRACE` | `011643` | `013050` | `004207` |
| 21 | `GUARD` | `011646` | `013056` | `004231` |
| 22 | `RESET-GUARD` | `011651` | `013117` | `004207` |
| 23 | `BRANCH-TRACE` | `011654` | `013125` | `004335` |
| 24 | `RESET-BRANCH-TRACE` | `011657` | `013170` | `004215` |
| 25 | `CALL-TRACE` | `011662` | `013202` | `004260` |
| 26 | `RESET-CALL-TRACE` | `011665` | `013244` | `004223` |
| 27 | `BREAK` | `011670` | `013255` | `004423` |
| 28 | `TEMPORARY-BREAK` | `011673` | `013301` | `004464` |
| 29 | `RESET-LAST-BREAK` | `011676` | `013332` | `004525` |
| 30 | `EXHIBIT-ADDRESS` | `011701` | `013343` | `004530` |
| 31 | `RESET-BREAKS` | `011704` | `013413` | `004565` |
| 32 | `DEBUG-STATUS` | `011707` | `013432` | `004577` |
| 33 | `DEBUGGER` | `011712` | `013441` | `004602` |
| 34 | `SPECIAL-DEBUGGER` | `011715` | `013455` | `004644` |
| 35 | `STEP` | `011720` | `013517` | `010446` |
| 36 | `PLACE-DOMAIN` | `011723` | `013560` | `003376` |
| 37 | `DEBUG-PLACE` | `011726` | `013577` | `003407` |
| 38 | `RESIDENT-PLACE` | `011731` | `013616` | `003421` |
| 39 | `LOCAL-TRAP-ENABLE` | `011734` | `013636` | `010460` |
| 40 | `SYSTEM-TRAP-ENABLE` | `011737` | `013673` | `010504` |
| 41 | `LOCAL-TRAP-DISABLE` | `011742` | `013717` | `010504` |
| 42 | `SYSTEM-TRAP-DISABLE` | `011745` | `013743` | `010504` |
| 43 | `RESET-DEBUG` | `011750` | `013767` | `004212` |
| 44 | `STATUS` | `011753` | `013775` | `010557` |
| 45 | `ENABLED-TRAPS` | `011756` | `014001` | `010562` |
| 46 | `RESTART-PROCESS` | `011761` | `014010` | `010573` |
| 47 | `OPEN-FILE` | `011764` | `014031` | `005061` |
| 48 | `CLOSE-FILE` | `011767` | `014071` | `005113` |
| 49 | `SET-BLOCK-SIZE` | `011772` | `014111` | `005126` |
| 50 | `LIST-OPEN-FILES` | `011775` | `014145` | `005157` |
| 51 | `SET-HISTOGRAM` | `012000` | `014155` | `010370` |
| 52 | `PRINT-HISTOGRAM` | `012003` | `014225` | `010432` |
| 53 | `STOP-HISTOGRAM` | `012006` | `014235` | `010435` |
| 54 | `START-HISTOGRAM` | `012011` | `014245` | `010440` |
| 55 | `RELEASE-HISTOGRAM` | `012014` | `014255` | `010443` |
| 56 | `TIME-USED` | `012017` | `014266` | `005162` |
| 57 | `WHO-IS-ON` | `012022` | `014273` | `005165` |
| 58 | `DEFINE-MACRO` | `012025` | `014300` | `004704` |
| 59 | `EXECUTE-MACRO` | `012030` | `014317` | `004747` |
| 60 | `ERASE-MACRO` | `012033` | `014346` | `004712` |
| 61 | `LIST-MACRO` | `012036` | `014365` | `004724` |
| 62 | `DUMP-MACRO` | `012041` | `014404` | `004736` |
| 63 | `RESUME-MACRO` | `012044` | `014422` | `004744` |
| 64 | `ABORT-BATCH-ON-ERROR` | `012047` | `014431` | `005055` |
| 65 | `AUTOMATIC-ERROR-MESSAGE` | `012052` | `014453` | `005170` |
| 66 | `RESET-AUTOMATIC-ERROR-MESSAGE` | `012055` | `014467` | `005176` |
| 67 | `SET-MEMORY-CONTENTS` | `012060` | `014506` | `004773` |
| 68 | `SET-FLAG` | `012063` | `014562` | `005204` |
| 69 | `GET-FLAG` | `012066` | `014606` | `005226` |
| 70 | `DEFINE-MEMORY-CONFIGURATION` | `012071` | `014625` | `005274` |
| 71 | `MEMORY-CONFIGURATION` | `012074` | `014676` | `005310` |
| 72 | `VERSION` | `012077` | `014711` | `005313` |
| 73 | `MASTER-CLEAR` | `012102` | `014721` | `005673` |
| 74 | `RUN-SELFTEST` | `012105` | `014730` | `005741` |
| 75 | `RESET-CPU` | `012110` | `014737` | `005744` |
| 76 | `INIT-TRACER` | `012113` | `014744` | `005747` |
| 77 | `ARM-TRACER` | `012116` | `015036` | `006024` |
| 78 | `DISARM-TRACER` | `012121` | `015044` | `006027` |
| 79 | `CLEAR-TRACE-ADDRESS` | `012124` | `015053` | `006032` |
| 80 | `CLEAR-TRACE-MEMORY` | `012127` | `015065` | `006035` |
| 81 | `DUMP-TRACE-MEMORY` | `012132` | `015077` | `006040` |
| 82 | `EXAMINE-TRACE` | `012135` | `015110` | `006045` |
| 83 | `READ-TRACE-FILE` | `012140` | `015117` | `006050` |
| 84 | `WRITE-TRACE-FILE` | `012143` | `015137` | `006056` |
| 85 | `LOAD-CONTROL-STORE` | `012146` | `015157` | `006064` |
| 86 | `COMPARE-CONTROL-STORE` | `012151` | `015231` | `006117` |
| 87 | `LOOK-AT-CONTROL-STORE` | `012154` | `015331` | `006220` |
| 88 | `LOOK-AT-RESIDENT-MEMORY` | `012157` | `015354` | `006237` |
| 89 | `LOOK-AT-HARDWARE` | `012162` | `015400` | `006231` |
| 90 | `MICRO-START` | `012165` | `015423` | `006277` |
| 91 | `MICRO-STOP` | `012170` | `015444` | `006312` |
| 92 | `SET-PRIORITY` | `012173` | `015452` | `006325` |
| 93 | `START-PROCESS-LOG-ALL` | `012176` | `015513` | `006573` |
| 94 | `START-PROCESS-LOG-ONE` | `012201` | `015526` | `006573` |
| 95 | `PROCESS-LOG-ALL` | `012204` | `015553` | `006516` |
| 96 | `PROCESS-LOG-ONE` | `012207` | `015606` | `006573` |
| 97 | `PRINT-PROCESS-LOG` | `012212` | `015641` | `006573` |
| 98 | `SWAPPING-LOG` | `012215` | `015665` | `006635` |
| 99 | `RELEASE-LOG-BUFFER` | `012220` | `015704` | `006676` |
| 100 | `ATTACH-PROCESS` | `012223` | `015716` | `006701` |
| 101 | `LOOK-AT-PHYSICAL-SEGMENT` | `012226` | `015741` | `006715` |
| 102 | `LOOK-AT-SRF` | `012231` | `015777` | `006755` |
| 103 | `SET-SEGMENT-LIMITS` | `012234` | `016017` | `007037` |
| 104 | `FIX-SEGMENT-SCATTERED` | `012237` | `016117` | `007112` |
| 105 | `FIX-SEGMENT-CONTIGUOUS` | `012242` | `016201` | `007112` |
| 106 | `FIX-SEGMENT-ABSOLUTE` | `012245` | `016264` | `007112` |
| 107 | `UNFIX-SEGMENT` | `012250` | `016357` | `007222` |
| 108 | `LIST-SYSTEM-PARAMETERS` | `012253` | `016412` | `007252` |
| 109 | `SET-SYSTEM-PARAMETERS` | `012256` | `016426` | `007257` |
| 110 | `VALUE-ENTRIES` | `012261` | `016441` | `007265` |
| 111 | `START-MONCALL-LOG` | `012264` | `016457` | `007277` |
| 112 | `PRINT-MONCALL-LOG` | `012267` | `016501` | `007315` |
| 113 | `STOP-MONCALL-LOG` | `012272` | `016512` | `007320` |
| 114 | `DEFINE-STANDARD-DOMAIN` | `012275` | `016523` | `007323` |
| 115 | `DELETE-STANDARD-DOMAIN` | `012300` | `016564` | `007344` |
| 116 | `LIST-STANDARD-DOMAINS` | `012303` | `016615` | `007352` |
| 117 | `LIST-EXECUTION-QUEUE` | `012306` | `016630` | `007355` |
| 118 | `LIST-TIME-QUEUE` | `012311` | `016654` | `007366` |
| 119 | `DEFINE-SWAP-FILE` | `012314` | `016675` | `007417` |
| 120 | `DELETE-SWAP-FILE` | `012317` | `016715` | `007425` |
| 121 | `SET-ND-500-AVAILABLE` | `012322` | `016735` | `007477` |
| 122 | `SET-ND-500-UNAVAILABLE` | `012325` | `016750` | `007433` |
| 123 | `STOP-ND-500` | `012330` | `016766` | `007510` |
| 124 | `STOP-ND-500` | `012333` | `016774` | `007510` |
| 125 | `LOGOUT-PROCESS` | `012336` | `017002` | `007517` |
| 126 | `ABORT-PROCESS` | `012341` | `017024` | `007517` |
| 127 | `LIST-ACTIVE-PROCESSES` | `012344` | `017045` | `007751` |
| 128 | `LIST-DOMAIN` | `012347` | `017060` | `007765` |
| 129 | `DOMAIN-STATUS` | `012352` | `017077` | `010073` |
| 130 | `LIST-STATUS` | `012355` | `017117` | `010101` |
| 131 | `SET-PROCESS-NAME` | `012360` | `017136` | `010107` |
| 132 | `LIST-PROCESS-TABLE-ENTRY` | `012363` | `017160` | `007563` |
| 133 | `LIST-ACTIVE-SEGMENTS` | `012366` | `017211` | `007563` |
| 134 | `PROCESS-STATUS` | `012371` | `017240` | `007563` |
| 135 | `LIST-SEGMENT-TABLE-ENTRY` | `012374` | `017250` | `007563` |
| 136 | `LIST-SWAP-FILE-INFO` | `012377` | `017303` | `007563` |
| 137 | `DEBUG-SWAPPER` | `012402` | `017330` | `010115` |
| 138 | `DUMP-SWAPPER` | `012405` | `017347` | `010137` |
| 139 | `DUMP-PHYSICAL-SEGMENT` | `012410` | `017365` | `010147` |
| 140 | `LIST-TABLE` | `012413` | `017417` | `010172` |
| 141 | `LOAD-SWAPPER` | `012416` | `017442` | `010211` |
| 142 | `START-SWAPPER` | `012421` | `017464` | `010217` |
| 143 | `GIVE-N500-PAGES` | `012424` | `017473` | `010222` |
| 144 | `TAKE-N500-PAGES` | `012427` | `017516` | `010235` |
| 145 | `SET-CPU-STATUS` | `012432` | `017541` | `010250` |
| 146 | `CHANGE-CPU` | `012435` | `017606` | `010320` |
| 147 | `SET-PHYSICAL-SEGMENT-ADDRESS` | `012440` | `017624` | `010350` |
| 148 | `CACHE-MODE` | `012443` | `017672` | `006315` |
| 149 | `REMOVE-FROM-TIME-SLICE` | `012446` | `017730` | `006411` |
| 150 | `INSERT-IN-TIME-SLICE` | `012451` | `017757` | `006447` |

commands: 151   descriptor array: 0o11547..0o12453   handler table: 0o20671..0o21117
word immediately after the handler table: 0o000010
