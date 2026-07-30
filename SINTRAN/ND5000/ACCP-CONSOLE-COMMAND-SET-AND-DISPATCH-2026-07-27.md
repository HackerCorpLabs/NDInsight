# ACCP console command set and dispatch map

**Date**: 2026-07-27
**Image**: `E:\Dev\Ronny\NDInsight\Installation\Communication\OctobusAccp\eprom\octo.bin`
**Status**: COMPLETE and read directly from the image - all 43 commands, their codes, their
full parameter syntax, and their handler addresses. Nothing here is inferred.

This is the ACCP's entire control surface. It is what a person sees on the RS-232 console,
and it is what a RetroCore ACCP machine has to reproduce.

---

## 1. How a command gets from the terminal to its handler

```
AccpMainInitAndRunConsole @0x205C
  builds PlancArrayDescriptor {origo 0x000130FE, lower 0, upper 0x2A}   <- the command table
  builds PlancArrayDescriptor over RAM 0x00113232                       <- the parse result
  jsr ConsoleCommandLoop @0x21A6
        |
        +-- ConsoleReadCommandLine @0x274E     read a line from the console
        +-- MatchCommandNamePrefix @0x2D36     match it against the 43 names
        |     returns a pointer to the matched TABLE RECORD in A0
        +-- move.w (A2),(0x00113334)           the record's CODE word -> global 0x00113334
        +-- a LINEAR COMPARE CHAIN, 0x227E-0x2746, dispatching on 0x00113334
```

**The dispatch is a chain of `cmpi.w` / `bne.b` / `jsr`, not a jump table.** 43 compares in
sequence. That matters for two reasons: there is no table to type as pointers, and the
command codes are sparse (0x03..0x46 with holes), which a jump table could not have been.

`0x00113334` holds the current command code. Watch that one word and you know what the
console is executing.

`HELP` (code 0x0C) is the only command with **no `jsr`** - its body is inline in the chain at
0x22D2, which is why its `bne` displacement is 34 instead of the usual 18.

---

## 2. The 43 commands

Read from the table at `0x000130FE`: 43 records of 14 bytes,
`{word code, long origo, long lower, long upper}` - a code plus a 12-byte PLANC array
descriptor over the command's help text. Table ends at 0x13358, exactly where the text
begins.

Sorted by command code.

| Code | Command and its parameter syntax | Handler |
|---|---|---|
| 0x03 | `DUMP-LOCAL-MEMORY <Address> <Wordsize /halfword/>` | 0x03A12 |
| 0x06 | `SHOW-REGISTERS` | 0x0A3B2 |
| 0x07 | `LOOK-AT-LOCAL-MEMORY <Address>` | 0x0353E |
| 0x09 | `VALUE <Convert number>` | 0x0400A |
| 0x0A | `MAIN-FORMAT <BASE (HEX,OCT,DEC)>` | 0x04076 |
| 0x0C | `HELP <Command>` | **inline @0x22D2** |
| 0x1F | `READ-ECO-LEVELS` | 0x09F12 |
| 0x20 | `LOOK-AT-CONTROL-CACHE <CC address>` | 0x0ADE0 |
| 0x21 | `LOAD-CONTROL-STORE <CS address> <127-112> <111-096> <095-080> <079-064> <063-048> <047-032> <031-016> <015-000>` | 0x08C44 |
| 0x22 | `LOOK-AT-CONTROL-STORE <CS address>` | 0x0AA5E |
| 0x23 | `START-MICROPROGRAM <CS address>` | 0x09110 |
| 0x24 | `STOP-MICROPROGRAM` | 0x091B8 |
| 0x25 | `CONTINUE-MICROPROGRAM` | 0x09218 |
| 0x26 | `RESTART-MICROPROGRAM <CS address> <Interval>` | 0x09272 |
| 0x27 | `CHECK-ALIVE` | 0x09D9C |
| 0x28 | `LOAD-MAR <CS address>` | 0x08D98 |
| 0x29 | `LOAD-MIR <127-112> <111-096> <095-080> <079-064> <063-048> <047-032> <031-016> <015-000>` | 0x08E04 |
| 0x2A | `READ-MIR` | 0x08F64 |
| 0x2B | `READ-AIB16` | 0x0898A |
| 0x2C | `READ-AIB32` | 0x0885A |
| 0x2D | `LOAD-AOB16 <Data (16)>` | 0x088E2 |
| 0x2E | `LOAD-AOB32 <Data (32)>` | 0x087B8 |
| 0x2F | `RUN-SHORT-SELFTEST <Loop selftest? (y/n)>` | 0x07FBC |
| 0x30 | `READ-ACCP-STATUS` | 0x09686 |
| 0x31 | `LOAD-MODE-REGISTER < Upper byte> < Lower byte>` | 0x0945E |
| 0x32 | `LOAD-CONTROL-DECODER <Data (16)>` | 0x095E4 |
| 0x33 | `LOOK-AT-MEMORY <Address>` | 0x0A556 |
| 0x34 | `SET-INTERRUPT-MASK <Interrupt mask>` | 0x0333A |
| 0x35 | `SET-SERIAL-LINE <Enable ND100-communication via serial line ? (y/n)>` | 0x07F06 |
| 0x36 | `SET-KICK-TIMEOUT <Kick timeout (ms)>` | 0x07EAE |
| 0x37 | `RECEIVE-OCTOBUS` | 0x09748 |
| 0x38 | `SEND-OCTOBUS <Data (16)>` | 0x097CA |
| 0x39 | `RECEIVE-MULTIBYTE-OCTOBUS` | 0x09B98 |
| 0x3A | `SEND-MULTIBYTE-OCTOBUS <Destination><Subprocess><Message>` | 0x0986C |
| 0x3B | `SEND-KICK-OCTOBUS <DESTINATION><Kick value (process)>` | 0x09A4E |
| 0x3C | `TRACE-COMMUNICATION-DATA <Trace Octobus communication to consol? (y/n)>` | 0x09D62 |
| 0x3D | `RUN-LONG-SELFTEST <Loop selftest? (y/n)>` | 0x08072 |
| 0x3E | `TEST-BUFFERS <ASR/AOB>` | 0x0855C |
| 0x3F | `TEST-BUSLOOP <Test-pattern>` | 0x0868A |
| 0x40 | `TEST-MEMORY <From address> <To address>` | 0x08128 |
| 0x41 | `LOOP-ON-NEXT-COMMAND <Supress output text ?>` | 0x07F40 |
| 0x42 | `SET-CLOCK-SPEED <Clock speed (Slow,Normal,Fast)>` | 0x09004 |
| 0x46 | `RESET-CPU` | 0x09708 |

All 43 handlers now exist as named functions in the Ghidra database, as
`Cmd<code>_<Name>`.

### Codes are sparse - a shared enum, not an index

Used: 03, 06, 07, 09, 0A, 0C, 1F, 20-2F, 30-3F, 40, 41, 42, 46.
Absent: 04, 05, 08, 0B, 0D-1E, 43, 44, 45.

The holes are real. This looks like a **global ND command-code enum** that the console
shares with something else - most likely the ACCP-ND100 command set, given the string
`"Illegal ACCP command received from microprogram:"`. **UNVERIFIED** - the holes have not
been traced to a second consumer.

---

## 3. What the command set tells us about the hardware

Reading the commands as a specification of the machine:

- **The microword is 128 bits.** `LOAD-CONTROL-STORE` and `LOAD-MIR` both take eight 16-bit
  fields spelled out from `<127-112>` down to `<015-000>`. The firmware states its own
  microword width; no inference needed. (Settles it for this card - cf. the ND-5800 vs
  classic-500 question.)
- **The ACCP can single-step and restart the ND-5000 microengine**: START / STOP / CONTINUE /
  RESTART-MICROPROGRAM, LOAD-MAR (microaddress register), LOAD-MIR / READ-MIR
  (microinstruction register), LOOK-AT-CONTROL-STORE, LOOK-AT-CONTROL-CACHE.
  `RESTART-MICROPROGRAM <CS address> <Interval>` even takes a repeat interval - the
  `"restarted every N microseconds"` string.
- **AIB / AOB are the ACCP-to-microprogram mailboxes**, in both 16- and 32-bit forms:
  READ-AIB16/32 (in) and LOAD-AOB16/32 (out). The error strings
  `"AOB not read by microprogram within timeout"` and `"AOB full, previous message not read.
  Message lost!"` describe exactly this pair.
- **The octobus has five console entry points**: SEND / RECEIVE-OCTOBUS (16-bit single word),
  SEND / RECEIVE-MULTIBYTE-OCTOBUS (with `<Destination><Subprocess><Message>`), and
  SEND-KICK-OCTOBUS (`<DESTINATION><Kick value (process)>`). **"Subprocess" and "process" in
  those parameter lists are the addressing above station number** - a kick is aimed at a
  process, a multibyte message at a destination plus subprocess.
- **Three commands were not in any earlier list**: `LOAD-CONTROL-DECODER <Data (16)>`,
  `LOAD-MODE-REGISTER <Upper byte> <Lower byte>`, `READ-ECO-LEVELS`, plus
  `SET-CLOCK-SPEED <Slow,Normal,Fast>` and `TEST-BUSLOOP`. SET-CLOCK-SPEED is notable - the
  ACCP can change the ND-5000's clock rate.

---

## 4. Related structures found alongside

| Address | What |
|---|---|
| 0x000130FE | the command table itself, 43 x 14 bytes |
| 0x00013358 | the command name / help text blob |
| 0x00012E9C | `tbl_runtimeErrorMessages` - 12-byte descriptors, indexed `(errcode - 1000) * 12` |
| 0x00012F5C | the error text: `"No such command$Ambiguous command$Illegal format$..."` |
| 0x00113334 | RAM: the current command code |
| 0x00113232 | RAM: the parsed-command array the loop passes to the matcher |
| 0x00113324 | RAM: tested right after the read; non-zero continues, zero exits the loop |

---

## Provenance

The table was decoded by reading the 43 records straight out of the ROM file and resolving
each descriptor to its text. The dispatch map was extracted by walking the compare chain
0x227E-0x2746 in the raw image and reading the `jsr` target after each `cmpi.w` / `bne.b`.
Both were cross-checked against the Ghidra listing. Section 3's readings of what the
commands imply are labelled where they are interpretation rather than transcription; the
sparse-code explanation in section 2 is explicitly UNVERIFIED.
