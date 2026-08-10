# Windows to SINTRAN and BACK - the round trip works

**Date:** 2026-08-09. Our C# node joined the live Ethernet segment and a file authored on
Windows landed on D102, intact.

## What now works

```
Windows folder  ->  our FA server (node 19998)  ->  hub 5010  ->  D102 COPY-FILE  ->  D102 pack
```

Proved by reading the file back on D102:

```
@ENTER SYSTEM,,,1
@TIME
@CC hello from windows
```

Three separate records, exactly as authored. **This solves the line-separator problem** that
made terminal authoring useless - typing over the terminal produced one mangled line whatever
separator was tried, because CR does not start a new record there. Authoring on Windows with
CRLF and transferring gives correct records.

## How to run it

1. Topology `SRC/Xmsg.Live.Runner/topology-d19998-ethernet.json`:
   - `"self": 19998` - **not** 19999, which another agent's HDLC relay uses.
   - `"fileServer": { "root": "served" }` - without this the FA server is OFF and nothing is
     served. The log says so plainly at startup.
   - the peer node carries `"transport": "ethernet"` with
     `"ethernet": { "net": "tcp", "host": "127.0.0.1", "port": 5010 }` - the same segment the
     RetroCore machines dial.
2. Put the files to serve in `served/` next to the exe.
3. `Xmsg.Live.Runner.exe --config topology-d19998-ethernet.json`
4. **On the machine, once:** tell it we exist.
   ```
   @(UTILITY)XMSG-COMMAND
   X-C: DEF-REMOTE,,D19998 19998
   X-C: DEF-NETWORK-CONN D19998 ENNS0,,0,0,0,0
   X-C: EXIT
   ```
5. Pull the file:
   ```
   COPY-FILE "ARBJOB9:SYMB",D19998(SYSTEM).ARBJOB9:SYMB
   ```
   Quotes on the LOCAL name being created; the remote source unquoted.

The node also sniffs the segment - the log shows live D100/D102 traffic - because the hub is a
broadcast hub.

## What does NOT work yet: the batch job still produces no output

With a VALID job file now on D102, `APPEND-REMOTE-BATCH` from D100 still answers `OK` and
`ARBOUT:SYMB` stays EMPTY, with batch processor 1 back at `IDLE`.

So the remaining fault is NOT the job file, which was the earlier theory and is now disproved -
the file is correct and readable on the machine. Something between `*XFTRA` accepting the
letter and the batch processor running the job is missing. Candidates NOT yet tested:

 - whether `*XFTRA` actually appends to the batch queue, or only accepts the letter;
 - whether the output file needs different access than an indexed file created with `,0`;
 - whether remote batch needs something else enabled on the receiving machine.

`OK` from the submission means the letter was accepted. It has now been shown twice to say
nothing about whether the job runs.

## The RETURN leg works too - the round trip is closed

D102 pushes a file to us with the same command shape, and it lands in the served folder:

```
COPY-FILE D19998(SYSTEM)."BATCHLOG:TXT",ARBOUTB:SYMB
```

Quotes on OUR name, because it is being created; the machine's own file unquoted.

So the whole loop runs today, with no C# client needed - the machine does both transfers and
our FA server answers both:

```
author on Windows -> D102 pulls it -> batch runs it -> D102 pushes the output back -> Windows
```

Proved with a real batch job: `ARBJOB9.SYMB` authored here, run by D102's batch processor 1,
and its output file returned as `BATCHLOG.TXT`, 427 bytes.

### The returned file is PARITY-MARKED

229 of its 427 bytes carry bit 7, with 17 even-parity violations - the same MIXTURE measured
in the July captures, which is why no per-extension rule could work. Strip it with
`Xmsg.Sync.SintranParity.Strip` and the listing comes out clean:

```
 --- SINTRAN III BATCH PROCESSOR ---
 USER SYSTEM ENTERED AT 02.52.00      9 AUGUST   1998
MAXIMUM TIME IS      1 MINUTES
@TIME
 ND-100 CPU TIME USED IS    0 SECS
@CC hello from windows
 BATCH USER LOGGED OUT AT 02.52.00      9 AUGUST   1998
```

**So the return leg MUST strip parity** or the daemon hands VS Code a file full of high-bit
bytes. That is what this morning's parity measurement was for, and it was needed the same day.

## One operational trap

The runner defaults to a **3600-second** session and then stops cleanly - the log says
`for 3600s` at startup and `[runner] done.` at the end. A push that answered
`NO ANSWER FROM REMOTE SYSTEM; FILE-ACCESS CONNECTION ABORTED` was simply our node having
timed out, not a protocol fault. Pass `0` as the fourth positional for an unlimited run:

```
Xmsg.Live.Runner.exe --config topology-d19998-ethernet.json 127.0.0.1 5010 19998 0
```

## Still to do

Task #33's daemon is now a WRAPPER around a proven transport: watch a folder, and drive the
two transfers. What it still needs from us is the PUSH direction under our own control - today
both transfers are initiated by a command typed on the machine, which an unattended daemon
cannot do. That is the FA client work (#30/#33).
