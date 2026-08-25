# NUCLEUS, the octobus and the ND-5000 — a complete reference

**Compiled:** 2026-08-24

**Subject:** how an ND-5000 program reaches an I/O device without going through SINTRAN — the
NUCLEUS message system, the DOMINO controllers it talks to, the octobus that carries the
signalling, and the four microcoded instructions in the ND-5800 control store that make it
fast.

---

## How to read this

Every claim carries a grade. **Nothing here is unmarked assertion.**

| Grade | Means |
|---|---|
| **[M]** | Printed in a Norsk Data manual. The manual is named and the wording is verbatim where quoted. |
| **[V]** | Verified by me against a binary artefact — the `MICRO-5800-B30` control-store image, its `.LABE` symbol table, or a disk image listing. |
| **[D]** | Derived. A reasoned inference from [M] or [V] material. The reasoning is always shown. |
| **[OPEN]** | Not known. Listed so it is not mistaken for settled. |

Octal is written either with a trailing `B` (ND's own style, as in `101023B`) or with the `0o`
prefix for control-store addresses. Both appear because both appear in the sources.

---

## Table of contents

1. [Introduction — the problem NUCLEUS solves](#1-introduction)
2. [Executive summary](#2-executive-summary)
3. [The components](#3-the-components)
4. [The NUCLEUS programming interface](#4-the-nucleus-programming-interface)
5. [The fast path — what is microcoded and what is not](#5-the-fast-path)
6. [**The B30 microcode — disassembled**](#6-the-b30-microcode-disassembled)
7. [The octobus](#7-the-octobus)
8. [DOMINO controllers](#8-domino-controllers)
9. [Manuals, products and media](#9-manuals-products-and-media)
10. [What we do not have](#10-what-we-do-not-have)
11. [Open questions](#11-open-questions)

---

## 1. Introduction

An ND-5000 system is two computers that need each other.

The **ND-5000** — code-named *Samson* — is the 32-bit compute engine. It has no I/O hardware of
its own. The **ND-100 / ND-110 / ND-120** is a 16-bit front-end that owns the terminals, the
discs and the file system, and runs the operating system, SINTRAN III. [M, ND-05.020.01]

In the classic arrangement that means every I/O request has to cross:

> "When a monitor call is executed, the ND-500 process is suspended and a twin process in the
> ND-100 is started to execute the call on behalf of the ND-500 process. Some monitor calls may
> allow the ND-500 process to continue while the call is executed if the function code is
> selected accordingly." [M, ND-60.136.04A section 8.7]

Suspend, hand over, wait, resume. Correct, general, and slow — and as the ND-5000 got faster the
front-end became the bottleneck.

**NUCLEUS is the answer to that.** It is a message-passing system, separate from monitor calls,
in which an *unprivileged* user program on the ND-5000 moves data straight into buffers that an
intelligent I/O controller can read — with no operating system involvement in the data movement
at all. On the ND-5000 the hot calls are not subroutines. **They are machine instructions.**

> "The Nucleus system, used for communication, allows data to be moved between an unprivileged
> ND-500 user program and the DIOC I/O buffers by using ND-500 microcode. Hence, user processes
> can move data to/from the I/O handlers **with no operating system overhead as no system calls
> are required**." [M, ND-14001-1 section 1.3]

---

## 2. Executive summary

**Nine things a reader must not get wrong.**

1. **NUCLEUS is not monitor calls.** It is a parallel mechanism with its own library, its own
   error-code space (`101000B`+) and its own transport. Ordinary file I/O still goes the old
   way. [M]

2. **Only four calls are microcoded**, and only on the ND-5000:
   > "For ND-5000, the time-critical NUCLEUS calls `nkMove`, `nkSend`, `nkReceive` and
   > `nkGetInfo` are microcoded to achieve required performance. **All other NUCLEUS calls are
   > executed in ND-100.**" [M, ND-820026]

3. **The classic ND-500 gets no fast path at all.**
   > "For ND-500, the time-critical NUCLEUS calls are not microcoded. These calls are executed
   > in ND-100 (level 12)." [M, ND-820026]

4. **The programmer writes ordinary procedure calls.** Same source, same include file, same
   library for ND-500 and ND-5000. The fast path is something you get, not something you ask
   for. [M]

5. **Those four calls are '87 extension instructions** — `SEND`, `RECVE`, `GETINF` and the hole
   move — living at consecutive entry points `0o1063`–`0o1071` in the ND-5800 control store.
   **[V]**

6. **`SEND` and `RECVE` are single-byte opcodes**, 182 and 183 decimal (`0o266`, `0o267`) —
   the cheapest opcode space in the machine. Everything else in the '87 extension set is a
   two-byte opcode on page `0xFF`. **[D from the dispatch reconstruction, see §6.3]**

7. **They only exist in the work-mode-500 (B) microcode.** In the A image (generation 406)
   `SEND`, `RECVE`, `GETINF` and `WHOLE` vector to the illegal-instruction handler. **[V]** So
   the fast path is a *generation-500* feature, not merely an ND-5000 one.

8. **The octobus is a doorbell, not a pipe.** *"The octobus is normally not used to transport
   data."* [M, ND-05.020.01] Bulk data moves through shared MFbus memory; the octobus carries
   short kicks and idents.

9. **We have the ND-5000 end and nothing else.** The microcode is in hand and disassembles. The
   NUCLEUS libraries — ND-100 side, ND-500/5000 side and DOMINO side — are **not in any archive
   we hold.** See §10.

---

## 3. The components

```
   ND-5000 (Samson)                 ND-100 / ND-110 / ND-120
   32-bit compute                   16-bit front end, runs SINTRAN III
   +----------------------+         +---------------------------+
   | user program         |         | NUCLEUS "slow" services   |
   |   nkSend / nkReceive |         |   create port, open, close|
   |   nkMove / nkGetInfo |         | ND-500/5000 Monitor       |
   +----------+-----------+         | monitor-call shadow tasks |
              |                     +-------------+-------------+
   +----------v-----------+                       |
   | MICROCODE            |                       |
   |  SEND   0o1067       |                       |
   |  RECVE  0o1071       |                       |
   |  GETINF 0o1065       |                       |
   |  WHOLE  0o1063       |                       |
   +----------+-----------+                       |
              |                                   |
   ===========v===================================v===========  MFbus (32-bit)
              |   shared memory: message buffers, holes         18 MB/s
   ===========+===============================================
              |
   -----------+-----------------------------------------------  OCTObus (serial)
              |   short messages only: kicks, idents            signalling
   -----------+-----------------------------------------------
              |
   +----------v-----------+
   | DOMINO controller    |  DIOC: MC68020 + OBCON + MFA + MFP
   |  DOMINOS  (the OS)   |  runs its own operating system
   |  NUCLEUS (kernel)    |
   |  application image   |  SCSI, Ethernet III, terminals, ...
   +----------+-----------+
              |
          the device
```

| Component | What it is |
|---|---|
| **NUCLEUS** | The message system. Ports, messages, send references, holes. Present on all three machine kinds — ND-100, ND-500/5000, DOMINO. [M] |
| **DOMINO** | The intelligent I/O controller architecture. A DIOC is an MC68020 card with its own OS. [M, ND-14001-1] |
| **DOMINOS** | The DIOC's operating system, "an enhanced version of PIOCOS". [M] |
| **OCTObus** | Serial signalling bus. *"optimized for fast handling of short messages … used for interprocessor synchronization"*. [M, ND-14001-1] |
| **MFbus** | The 32-bit multifunction bus. Where the data actually moves. Capacity **18 MB/sec**. [M, ND-5230] |
| **Hole** | A data stream, seen from two sides — see §4.3. |

---

## 4. The NUCLEUS programming interface

### 4.1 The twelve calls [M, ND-820026]

| Call | Purpose | ND-5000 |
|---|---|---|
| `nkCrePort` | create a port | ND-100 |
| `nkCreName` | give a port a name | ND-100 |
| `nkOpenPort` | open a port by name | ND-100 |
| `nkOpenReturnPort` | open the reply port | ND-100 |
| `nkDelName` | delete a port name | ND-100 |
| `nkCreMessage` | allocate a message buffer | ND-100 |
| **`nkMove`** | read/write message data | **microcoded** |
| **`nkSend`** | send to a port | **microcoded** |
| **`nkReceive`** | receive from a port | **microcoded** |
| **`nkGetInfo`** | query size, length, ids, buffer, queue | **microcoded** |
| `nkClose` | close port / message / send reference | ND-100 |
| `nkVersion` | library, kernel or station version | ND-100 |

The split is by cost, not by capability: setup is rare and may be slow; moving bytes and
handing off messages is hot.

### 4.2 Function codes [M, ND-820026 Table 5]

| Call | Code | Name |
|---|---|---|
| `nkMove` | 0 | `nkfRead` |
| | 1 | `nkfWrite` |
| | 2 | `nkfInsert` — like write, but the byte pointer is not set if the message is smaller than the old one |
| `nkGetInfo` | 0 | `nkfSize` |
| | 1 | `nkfLength` |
| | 2 | `nkfHomeid` |
| | 3 | `nkfLastid` |
| | 4 | `nkfBuffer` |
| | 5 | `nkfQueue` |
| `nkClose` | 0 | `nkfRemove` |
| | 1 | `nkfReject` |
| `nkVersion` | 0 | `nkfLibrary` |
| | 1 | `nkfKernel` |
| | 2 | `nkfStation` |
| `nkOpenReturnPort` | 0 | `nkfOpenHomePort` |
| | 1 | `nkfOpenLastPort` |

**Hold on to the `nkMove` and `nkGetInfo` rows.** §6.4 shows both of them as literal jump tables
in the microcode.

### 4.3 Holes — one object, two views [M, ND-14001-1]

> "From the ND-500, the data is represented as **holes**, with a hole number giving access to a
> data stream. From the DIOC, a hole is seen as a **chain of buffers in a linear queue**. For
> optimized speed, the hole operations are microcoded in the ND-500."

A message buffer is *"allocated in a contiguous area of physical memory"* [M] — contiguous
because both the ND-5000's microcode and the controller's DMA engine have to reach it without
help.

### 4.4 Status codes [M, ND-820026 section 7.2.1]

Defined in the include file `NK-ERRCODE:DEFS`.

| Constant | Octal | Meaning |
|---|---|---|
| `nke_ERROR_BASE` | `101000B` | base number for NUCLEUS errors |
| `nke_ILLPAR` | `101001B` | invalid parameter value |
| `nke_ILLTYPE` | `101002B` | wrong type used — port, message or send reference |
| `nke_NOMESS` | `101003B` | both port and message in send reference may not be zero |
| `nke_ILLNO` | `101004B` | port, message or send reference outside range |
| `nke_OUTSIDE` | `101006B` | displacement outside buffer |
| `nke_NOACCESS` | `101014B` | no access to given port, message or send reference |
| `nke_LOCK` | `101023B` | unable to lock port |
| `nke_NOTINITIALISED` | `101025B` | NUCLEUS not started |
| `nke_PORTCLOSED` | `101032B` | receive port is closed |
| `nke_ILLFUNC` | `101033B` | invalid function code |
| `nke_KICKLOCK` | `101042B` | timeout when waiting for lock (kick-queue) |

**§6.5 shows all of these but one emitted directly by the microcode**, as literal constants.

---

## 5. The fast path

### 5.1 Three layers, one changes

| Layer | ND-500 | ND-5000 |
|---|---|---|
| Application source | identical | identical |
| Include file | `NK-LIBRARY-C:IMPT` — *"common to all computers"* [M] | same |
| Library | `NK-5000-C:BRF` | **same file** |
| The four hot calls | ND-100, level 12 | **microcoded** |
| The other eight | ND-100 | ND-100 |

> "The services provided by NUCLEUS are **independent of the CPU and operating system** where
> the process is running." [M]

### 5.2 The one thing the programmer must know [M]

> "The library should be loaded on a **separate segment** if the application is running on a
> ND-500 computer. Performance will decrease if program code and library are loaded on the same
> segment, because **cache is turned off on the segments that libraries are loaded on**."

### 5.3 A transfer, end to end [D — assembled from [M] parts]

1. The program calls `nkMove(nkfWrite, …)`. On an ND-5000 this becomes a **single machine
   instruction** that writes into the shared message buffer. No trap, no SINTRAN, no ND-100.
2. `nkSend` — again one instruction — queues the message on a port.
3. A short **octobus** frame kicks the target station.
4. The DIOC's **MC68020**, under DOMINOS, takes the message off its port and performs the real
   device work, DMA-ing across the MFbus.
5. Completion returns the same way: buffer plus kick.

The ND-100 appears nowhere in steps 1–5. It set the port up beforehand, and it will tear it
down afterwards.

---

## 6. The B30 microcode, disassembled

Everything in this section is **[V]** — read by me out of
`E:\Dev\Ronny\ND5000UC\docs\MC\MICRO-5800-B30.DATA`, its symbol table
`MICRO-5800-B30.LABE`, and the rendered disassembly
`E:\Dev\Ronny\ND5000UC\microcode\MICRO-5800-B30.md` — except where marked otherwise.

The image is 16384 control-store words of 128 bits. `.LABE` header: *"ND-5800 microprogram cross
reference table version 11930 (WM-500)"*.

### 6.1 The '87 extension block

The 1987 extension instructions occupy one contiguous run of the macro-instruction entry region,
as short stubs — mostly two words each. **[V]**

| Entry | Label | Instruction |
|---|---|---|
| `0o1026` | `PHYLADR` | get physical address |
| `0o1030` | `LREGBL` | load register block |
| `0o1033` | `SREGBL` | save register block |
| `0o1036` | `LCNTXT` | load context block |
| `0o1042` | `SCNTXT` | save context block |
| `0o1045` | `JUMPS` | call supervisor |
| `0o1047` | `SCPUNO` | store CPU number |
| `0o1051` | `SVERS` | store microprogram version |
| `0o1053` | `REXT` | read from device external to CPU |
| `0o1055` | `WEXT` | write to device external to CPU |
| `0o1057` | `RPHS` | read from physical segment |
| `0o1061` | `WPHS` | write to physical segment |
| **`0o1063`** | **`WHOLE`** | **write to NUCLEUS hole** |
| **`0o1065`** | **`GETINF`** | **get info** |
| **`0o1067`** | **`SEND`** | **send to port** |
| **`0o1071`** | **`RECVE`** | **receive from port** |
| `0o1073` | `PLCCNBI` | PLANC → ND-500 descriptor |
| `0o1075` | `NCPLC` | ND-500 → PLANC descriptor |

**The four NUCLEUS instructions are adjacent, in the middle of the block.** That is not
accidental — they were designed as a set.

### 6.2 The entry stubs, verbatim

```
0o1063  WHOLE   ALU,A TYP,DD A,ALU,REG37 B,X1 D,SC3 T,JMP COND,MSEXO TBC,NEXT
                READ ADACT ADDR=001064 ORCON=04
0o1064          ALU,A-1 A,BM02 B,X1 D,SC10 T,JMP COND,MSEXO TBC,NEXT AD_ARTI=1
                ADACT ADDR=MHOLE_1 AA=4 AB=1 ORCON=0x04

0o1065  GETINF  ALU,A TYP,DF A,ALU,REG37 B,X1 D,SC3 T,JMP COND,MSEXO TBC,NEXT
                READ ADACT ADDR=001066 ORCON=04
0o1066          ALU,A A,SC3 B,X1 D,SC6 T,JMP COND,MSEXO TBC,NEXT AD_ARTI=1
                ADACT ADDR=GETINF_1 AA=4 AB=1 ORCON=0x04

0o1067  SEND    ALU,A A,ALU,REG37 B,X1 D,SC3 T,JMP COND,MSEXO TBC,NEXT
                READ ADACT ADDR=001070 ORCON=04
0o1070          ALU,A A,X1 B,X1 T,JMP COND,MSEXO TBC,NEXT ADDR=SEND_1

0o1071  RECVE   ALU,A A,ALU,REG37 B,X1 D,SC3 T,JMP COND,MSEXO TBC,NEXT
                READ ADACT ADDR=001072 ORCON=04
0o1072          ALU,XOR A,BM00 B,X1 T,JMP COND,MSEXO TBC,NEXT G,OPS ADDR=RECVE_1
```

All four share a shape: word 1 reads the first operand (`READ`, `ADACT` address arithmetic
active), word 2 fetches or computes the second and jumps to the body. `WHOLE` and `GETINF`
carry a datatype on the first word — `TYP,DD` (double word) and `TYP,DF` (double float)
respectively — which is how the operand size reaches the address adder.

Bodies:

| Instruction | Body | Address |
|---|---|---|
| `WHOLE` | `MHOLE_1` | `0o4635` |
| `GETINF` | `GETINF_1` | `0o5315` |
| `SEND` | `SEND_1` | `0o5057` |
| `RECVE` | `RECVE_1` | `0o5247` |

### 6.3 Opcodes — and the two that are missing

From the reconstructed dispatch map (`dispatch-map-b30.json`, 1183 rows). **Grade [D]**: the map
is a reconstruction from the symbol table, because the opcode→entry map lives in PALs and a gate
array on the IDA card, not in any dumpable PROM. The *entry addresses* are [V]; the *opcodes*
are not.

| Opcode | Octal | Form | Instruction | Entry |
|---:|---|---|---|---|
| **182** | `0o266` | `W1` | **`SEND`** | `0o1067` |
| **183** | `0o267` | `W1` | **`RECVE`** | `0o1071` |
| 185 | `0o271` | | `JUMPS` | `0o1045` |
| **65181** | `0o177235` | `BY` | **`WHOLE`** | `0o1063` |
| 65512–65515 | `0o177750`–`753` | `W1`–`W4` | `REXT` | `0o1053` |
| 65516–65519 | `0o177754`–`757` | `W1`–`W4` | `WEXT` | `0o1055` |
| 65520–65523 | `0o177760`–`763` | `t1`–`t4` | `PHYLADR` | `0o1026` |
| 65524 | `0o177764` | | `WPHS` | `0o1061` |
| 65525 | `0o177765` | | `RPHS` | `0o1057` |

**`SEND` and `RECVE` sit at 182 and 183 — single-byte opcodes.** Every other '87 extension is a
two-byte opcode on page `0xFF`. Single-byte opcode space on this machine is scarce and valuable;
spending two slots of it on the NUCLEUS send and receive is a deliberate performance decision
and the strongest single piece of evidence for how central this path was meant to be. **[D]**

**Two absences, both real:**

- **`GETINF` has an entry but no opcode.** `0o1065` exists, is labelled, and has a working body
  — and nothing in the reconstructed map reaches it. It is one of the 28 orphaned entry points
  the dispatch reconstruction reports. **[V]**
- **`RHOLE` has no label anywhere in the image.** Appendix D of the ND-500 Reference Manual
  lists `RHOLE` "read from NUCLEUS hole" as a distinct instruction, but the B30 symbol table has
  no such symbol. **[V]**

The `RHOLE` absence is explained by §6.4: **`WHOLE`'s body is `MHOLE`, a *move* routine that
dispatches on a function code and handles read, write and insert alike.** The manual names two
instructions; the microcode implements one entry with a three-way table. Whether a separate
`RHOLE` opcode also dispatches to `0o1063`, or whether `RHOLE` simply does not exist on this
CPU, is **[OPEN]** — the map that would say is in the PALs.

### 6.4 Two jump tables that match the manual exactly

This is the clearest evidence that the microcode implements the documented API rather than
something adjacent to it.

**`MHOLE_TAB` at `0o4654` — three entries:** **[V]**

```
0o4654  MHOLE_TAB  ... ADDR=MHOLE_READ ...
0o4655             ... ADDR=MHOLE_WRIT ...
0o4656             ... ADDR=MHOLE_INSE ...
```

Against `nkMove`'s documented function codes: `0 = nkfRead`, `1 = nkfWrite`, `2 = nkfInsert`.
**Three slots, three functions, same order.**

**`GETINF_TAB` at `0o5333` — eight entries:** **[V]**

| Slot | Target | `nkGetInfo` function |
|---|---|---|
| 0 | `NKFSIZE` `0o5344` | `0 = nkfSize` |
| 1 | `NKFLENGTH` `0o5354` | `1 = nkfLength` |
| 2 | `NKFHOMEID` `0o5364` | `2 = nkfHomeid` |
| 3 | `NKFLASTID` `0o5415` | `3 = nkfLastid` |
| 4 | `NKFBUFFER` `0o5426` | `4 = nkfBuffer` |
| 5 | `NKFQUEUE` `0o5434` | `5 = nkfQueue` |
| 6 | `NKGILLEG` | illegal |
| 7 | `NKGILLEG` | illegal |

**Six documented functions, six handlers, in documented order — and the two unused slots of the
eight-way table fall through to the illegal-function path.** The table is sized to a power of
two and the surplus is trapped.

Each handler begins with `RD,POF` — a **read with paging off**, i.e. a physical read. The
NUCLEUS control tables live outside the user's address translation, which is exactly what lets
an *unprivileged* program reach them safely: the microcode does the privileged access on the
program's behalf.

### 6.5 The error returns — the manual's constants, in silicon

The block at `0o5604`–`0o5621` is a fan of one-word routines, each loading a literal into the
status register and jumping to a common exit. **[V]**

| Address | Label | `SARG` constant | Manual constant [M] |
|---|---|---|---|
| `0o5604` | `NKRETSTS` | (no error) | success |
| `0o5606` | `NKSET_IOV` | `101002` | `nke_ILLTYPE` |
| `0o5607` | `NKNOMESS` | `101003` | `nke_NOMESS` |
| `0o5610` | `NKILLNO` | `101004` | `nke_ILLNO` |
| `0o5611` | `NKSOUR_RANGE` / `NKDEST_RANGE` | `101006` | `nke_OUTSIDE` |
| `0o5612` | `NKPROTVIOL` | `101014` | `nke_NOACCESS` |
| `0o5613`–`14` | `NKTIMEOUT` | `101023` | `nke_LOCK` |
| `0o5615` | `NKPORTCLOSED` | `101032` | `nke_PORTCLOSED` |
| `0o5617` | `NKILLEG` | `101033` | `nke_ILLFUNC` |
| `0o5620` | `NKNOTSTART` | `101025` | `nke_NOTINITIALISED` |
| `0o5621` | `NKGETNEXT` | — | common exit → `LOAD_L` |

A representative word, verbatim:

```
0o5607  NKNOMESS  ALU,A TYP,HW A,SARG SARG=101003 B,X1 D,X1 K,ONE
                  T,JMP COND,MSEXO TBC,NEXT ADDR=NKGETNEXT
```

`SARG=101003` is the error code as a literal in the microword; `K,ONE` sets the condition flag
the caller tests; `NKGETNEXT` is the shared return that ends the instruction.

**Ten of the twelve documented status codes are emitted directly by the CPU's own microcode**,
and the microcode's label names track the manual's constant names one for one —
`NKNOMESS`↔`nke_NOMESS`, `NKILLNO`↔`nke_ILLNO`, `NKPORTCLOSED`↔`nke_PORTCLOSED`,
`NKNOTSTART`↔`nke_NOTINITIALISED`, `NKTIMEOUT`↔`nke_LOCK` (unable to lock). Two codes —
`nke_ILLPAR` `101001B` and `nke_KICKLOCK` `101042B` — do not appear in this block; the
kick-queue lock is a DIOC-side and ND-100-side concern. **[V] + [M]**

> **A discrepancy worth recording.** A separate project document lists the 211276C microprogram
> sheet's error codes with different descriptions — `101003` as "illegal message type",
> `101004` as "no message", `101023` as "no access to the message", `101033` as "kicklock
> timeout". Those pairings **disagree with both** ND-820026 and the microcode labels, which
> agree with each other. I have not read the 211276C sheet myself, so the error may be in that
> transcription rather than in the sheet. **Prefer ND-820026 and the microcode. [OPEN]**

### 6.6 The port lock

`LOCK_DH` at `0o5504` is a real spin lock with a bounded retry. **[V]**

```
0o5517          ALU,A A,MARG MARG=045 B,X1 D,LC ...     <- load retry count 045B into LC
0o5520-22       ... test-and-set, physical write ...
0o5523  LOCK_DHTIM  ALU,FZRO A,BM00 B,X1 LCDECR C,SEQ
                    T,JMP INVSEQ COND,LCZ TBC,NEXT ADDR=LOCK_DHTIM   <- spin, decrement LC
0o5524          ... ADDR=LOCK_DH1                                     <- retry the acquire
0o5525  UNLOCK_DH   ...                                               <- release
```

- The retry budget is a literal, `MARG=045` (37 decimal), loaded into the loop counter `LC`.
- `LOCK_DHTIM` is a delay loop that jumps to itself while `LC` is non-zero.
- The lock word is touched with `WR,POF` / `RD,POF` — physical, paging off.
- Exhausting the budget lands on `NKTIMEOUT` → `101023B` = `nke_LOCK`, *"unable to lock port"*.

`LOCK_DH` is referenced from five sites, all inside the NUCLEUS bodies (`0o5130`, `0o5205`,
`0o5253`, `0o5330`, `0o5463`) — that is, `SEND`, `RECVE`, `GETINF` and the trace path each take
the lock. **[V]**

### 6.7 The rest of the layer

The image carries **37 labels beginning `NK`** plus the `MHOLE_*`, `SEND_*` and `RECVE_*`
families. **[V]** Notable:

| Label | Address | Role |
|---|---|---|
| `NKMB_POINT` | `0o24` | NUCLEUS mailbox pointer, in the fixed-vector page |
| `NK_TRACE` | `0o5451` | tracing hook, entered from `0o5127` and `0o5311` |
| `NKACONV` / `NKACHECK` | `0o5535` / `0o5562` | address conversion and checking |
| `NKCHOWN` | `0o5566` | ownership check |
| `NKGETCI4` / `NKGETCI8` | `0o5442` / `0o5444` | 4- and 8-bit field extraction helpers |
| `NKPROTVIOL` | `0o5612` | protection violation → `101014B` |
| `SENKICK` | `0o25142` | send a kick |
| `KICK06` | `0o25563` | kick handler 6 |

`SEND_1` alone runs from `0o5057` through roughly `0o5236` — around 100 words, with sub-labels
`SEND_2` … `SEND_14`, a transfer path (`SEND_TR`), and its own lock acquisition. This is not a
thin wrapper; the whole send protocol is in microcode.

**`NKMB_POINT` at `0o24` is worth noting**: it sits among the fixed service vectors in the first
page of the control store, alongside `VERSION` `0o1`, `CPUMODEL` `0o7` and `SAMSON_CPU` `0o25`.
The NUCLEUS mailbox pointer is CPU-level furniture, not an afterthought.

### 6.8 What the A image does instead

In `MICRO-5800-A30` — the generation-406 build — the same instruction entries exist but the
bodies do not. `SEND`, `RECVE`, `GETINF` and `WHOLE` vector to `ILLEG`; the multiprocessor
nucleus, the spin locks, the trace machinery and octobus kicks 3–6 are absent or stubbed to
"not recognised". The two revision-30 images differ in **11315 of 16384 words**. **[V]**

So on a generation-406 machine an application linked against the same NUCLEUS library hits an
illegal instruction where the fast path would be — and the library must therefore be taking a
different route. Which route is **[OPEN]**; see §11.

---

## 7. The octobus

### 7.1 What it is for [M]

> "The OCTObus is a serial bus optimized for fast handling of short messages. It is used for
> interprocessor synchronization and for passing configuration parameters during
> initialization." [ND-14001-1]

> "The octobus is normally not used to transport data." [ND-05.020.01, stated twice]

Data goes over the MFbus — *"a full 32-bit bus in both the address and data paths"* with
*"semaphore cycles [to] ensure safe access to data structures which are common to two or more
processors"* [M, ND-14001-1], rated at 18 MB/sec [M, ND-5230].

### 7.2 Station assignments [M, ND-05.020.01 Appendix 2 / ND-05.017.01 ch.3]

| Station (octal) | Device |
|---|---|
| 1 | ND-100 / ND-120 CPU — normally bus MASTER |
| 2–7 | MFbus controllers (crate masters) |
| 10–13 | SCSI controllers |
| 14–15 | Matra VME |
| 16–17 | Multifunction communication |
| 20 | Hyperchannel |
| 21–23 | FDDI |
| 24–27 | FPS-5000 |
| 30–33 | Graphic controller |
| 34–67 | free for expansion |
| 70–76 | ND-5000 CPUs (SINTRAN uses 70–73 for up to four) |

### 7.3 The conversation shape [D, from [M] parts]

Every host↔controller exchange has the same rhythm:

1. Producer builds a command or message block **in shared memory**.
2. Producer sends a short **octobus** frame — a kick or ident — to wake the consumer.
3. Consumer processes, DMAs data as needed, writes status back **into shared memory**.
4. Consumer sends a short octobus frame back to signal completion.

NUCLEUS is one instance of this pattern. The ND-100↔ND-5000 mailbox is another.

---

## 8. DOMINO controllers

### 8.1 The standard core [M, ND-14001-1]

Every DIOC is the same card with a different personality bolted on. Figure 22 of ND-14001 marks
each block standard or device-dependent:

| Standard on every DIOC | Device-dependent |
|---|---|
| OCTObus adapter (OBCON gate array) | device logic + request arbiter |
| MFbus adapter (MFA / BADAP gate array) | the device itself |
| MC68020 + DRAM / EPROM / EEROM | |
| MC68901 MFP (timers, console UART), RTC, interrupt system | |
| Console and trace connector | |

The manual is explicit that this is a **design kit**: it includes the circuit diagrams *"for
those wishing to know more about DIOC design or wanting to design new I/O controllers in the
DOMINO range."*

### 8.2 Module types [M, ND-820026 Table 1]

| Module no. (octal) | Hardware id | Type |
|---|---|---|
| 5 | `VMEI` | VME-bus interface |
| 20 | `IPI3` | IPI level III disk |
| 21 | `SCSI` | SCSI disk/tape/optical/streamer |
| 22 | `ETH3` | Ethernet III |
| 23 | `FPS5` | FPS-5000 |
| 24 | `TERM` | terminal controller |
| 25 | `GRAP` | graphic controller |
| 26 | `MFCC` | multifunction comms |
| 27 | `VMEC` | VME-bus controller |
| 30 | `DMAC` | MF-DMA controller |

### 8.3 The software stack [M]

```
DOMINO OPCOM      PROM, common to every DIOC. Octobus and terminal interrupt
                  drivers, hardware tests, DOMINO Monitor command execution.
DOMINOS           The OS. "An enhanced version of PIOCOS." Processes, events,
                  buffers, timers.
NUCLEUS           The message layer — the DIOC end of the same system the
                  ND-5000 microcode implements.
Application       SCSI / Ethernet III / terminal — where controllers differ.
```

**XMSG is not part of this stack on the controller side.** It sits between the DOMINO Monitor
and the BOPCOM server *inside the ND-100*. A DIOC never sees XMSG. [M]

---

## 9. Manuals, products and media

### 9.1 Manuals held [M]

| Manual | What it gives |
|---|---|
| `ND-820026.1` / `ND-820026-1c` **DOMINO and NUCLEUS Software Guide** | The NUCLEUS API, function codes, status codes, the microcoded/not-microcoded split, library file list. **The primary source for this document.** |
| `ND-14001-1` **DOMINO Standard Hardware Description** | The DIOC standard hardware, the hole model, the octobus adapter, the design-kit framing. |
| `ND-05.009.4` **ND-500 Reference Manual** | Appendix D — the 25 '87 extension instructions including `SEND`, `RECVE`, `RHOLE`, `WHOLE`. |
| `ND-05.020.01` **ND-5000 Hardware Description** | Octobus station map, "not used to transport data", ACCP command set. |
| `ND-05.017.01` **ND-5000 Hardware Maintenance** | Octobus chapter, CPU module part lists. |
| `ND-814009-1` **DOMINO SCSI Operator Guide** | Operator-level only — **not** a register spec. |
| `ND-60.136.04A` **ND-500 Loader/Monitor** | The monitor-call path NUCLEUS bypasses; process model. |

All are in `E:\Dev\Ronny\NDInsight\Reference-Manuals\`.

### 9.2 The NUCLEUS products [M, ND product structure sheets]

| Product | ND number | Diskette | Format |
|---|---|---|---|
| **NUCLEUS Library** | **250295 C06** — 38 SW-modules, 87 DOC-modules | `250295C06-XX-01D` | 17 |
| **NUCLEUS Maintenance Kit** | **211321 C03** | `211321C03-XX-01D` | 17 |
| DOMINO Maintenance Kit | 211322 C | — | — |

Product information sheets: `ND-895058.1` (Library), `ND-895059.2` (Maintenance Kit),
`ND-895056.2` (DOMINO Maintenance Kit). Both NUCLEUS sheets are **indexed in the sintran.com
mirror but were never downloaded** — a cheap re-crawl target.

### 9.3 The library files [M, ND-820026 section 6.1]

| File | For |
|---|---|
| `NK-100-1bank-C:BRF` | ND-100, 1-bank program |
| `NK-100-1bank-C:BRF` *(the manual prints the same name twice — an evident typo)* | ND-100, 2-bank program |
| **`NK-5000-C:BRF`** | **ND-500 and ND-5000 — one library for both** |
| `NK-DOM-APPL-C:NRF` | DOMINO controller, application side |
| `NK-DOM-OS-C:NRF` | DOMINO controller, must be loaded in DOMINO |
| `NK-DOM-LINK-C:MODE` | example DOMINO load/link job |
| `NK-ERRCODE-C:DEFS` | error and function code constants |
| `NK-LIBRARY-C:IMPT` | PLANC import declarations — *"common to all computers"* |

> "After loading any NUCLEUS library, a PLANC library (I-version or later) must be loaded." [M]

### 9.4 Microcode media held [V]

`E:\Dev\Ronny\ND5000UC\docs\MC\img\` — identified 2026-08-24 by reading ND filenames out of each
image:

| Image | Product | Model |
|---|---|---|
| `ND-disk-00259.img` | 211272A | ND-5200 |
| `ND-disk-00260.img` | 211274A | ND-5500 |
| `ND-disk-00262.img` | 211275A | ND-5700 |
| `ND-disk-00263.img` | 211276C | ND-5800 rev 29 |
| `ND-disk-00264.img` | 211276A | ND-5800 rev 27 |
| `211276D01-XX-01D.image` | 211276D | ND-5800 rev D |

Plus the 250247A ND-5000 Test Microprograms floppies (2 discs + label photos) fetched from the
Danish Datamuseum bitstore.

---

## 10. What we do not have

Searched: the sintran.com mirror (7.3 GB), the Norsk Data Software Archive (1102 floppy
records), every ND disk image on the E: drive (45, listed with `ndtool`), and NDInsight. **[V]**

| Missing | Status |
|---|---|
| **`NK-5000-C:BRF`** — the ND-500/5000 NUCLEUS library | Not in any archive |
| **`NK-DOM-APPL-C:NRF`** / **`NK-DOM-OS-C:NRF`** — the DIOC side | Not in any archive |
| **`NK-LIBRARY-C:IMPT`**, `NK-ERRCODE-C:DEFS` | Not in any archive |
| Diskettes `250295C06-XX-01D`, `211321C03-XX-01D` | Catalogued as products; no image exists |
| Product sheets `ND-895058-1`, `ND-895059-2` | Indexed in the mirror, never downloaded |
| Any DOMINO controller firmware image | None held |
| ND-5830/5850 (Rallar) microcode, product 211847 | Not in any archive |

**A near-miss worth recording so nobody repeats it.** Four disk images match on the word
"NUCLEUS", including two 78 MB SINTRAN system discs. Listing their directories with `ndtool`
shows **no `NK-*` file in any of them**. The matches are inside `ER-S3WD-DESC-D:EDAT`, the
error-message description file, which carries strings like *"Unable to initialise Sintran Part
of Nucleus"*, *"Unable to connect to Octobus"*, *"Inconsistent Nucleus module versions
installed"*, *"Nucleus buffer full"* and *"No more Nucleus descriptor resources available"*.
**[V]**

Those strings are still informative: they show the ND-100 half of NUCLEUS was a normal part of a
SINTRAN installation, and that **version skew between the ND-100, ND-5000 and DIOC halves was a
recognised failure mode.**

**The net position: we hold the ND-5000 end of a three-ended contract.** The microcode is in
hand and disassembles. The ND-100 library and the DIOC library are both gone.

---

## 11. Open questions

1. **How one library binary serves both ND-500 and ND-5000.** `NK-5000-C:BRF` covers both, yet
   the fast calls are microcoded on one and executed in the ND-100 on the other. Three
   candidate mechanisms — a runtime CPU-type branch, the instructions existing on both with the
   ND-500's microcode routing them to level 12, or load-time entry-point resolution. The second
   is the most economical reading (Appendix D says the '87 extensions also run on ND-500/1 and
   ND-500/2) but it is **not documented**.
2. **What happens on a generation-406 ND-5000.** The instructions trap as illegal there. Does
   the library detect the work mode, or does the trap handler emulate? Unknown.
3. **Does `RHOLE` exist as an opcode?** No label in B30; `WHOLE` → `MHOLE` handles all three
   directions. §6.3.
4. **What opcode reaches `GETINF`?** The entry at `0o1065` is live and orphaned. §6.3.
5. **Whether a SCSI transfer actually uses NUCLEUS holes**, or a different DIOC path. The
   mechanism is generic and confirmed; a SCSI read has not been traced through it.
6. **How a hole is bound to a particular DIOC.** That is one of the ND-100-side "slow services"
   and is not covered here.
7. **The 211276C error-code descriptions** disagree with ND-820026 and with the microcode
   labels. §6.5.
8. **`nke_ILLPAR` `101001B` and `nke_KICKLOCK` `101042B`** are not emitted by the B30 NUCLEUS
   block. Presumably ND-100- and DIOC-side only, but unconfirmed.

---

## Cross-references

- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ND5000-FAMILY-MODELS-REFERENCE.md` — CPU models, work
  modes, microprogram versions, the A/B split.
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\DOMINO-DIOC-GENERIC-CONTROLLER-ARCHITECTURE-2026-07-28.md`
  — the DIOC standard/device-dependent seam.
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\OCTOBUS-DEVICE-CONTROLLERS-ANALYSIS-AND-EMULATION-PLAN-2026-07-19.md`
  — station map, module types, emulation plan.
- `E:\Dev\Ronny\ND5000UC\docs\ND5000-MICROCODE-COMPLETE-REFERENCE-2026-08-24.md` — the microword
  format, the control-store map, the dispatch reconstruction.
- `E:\Dev\Ronny\ND5000UC\docs\ND5000-ND100-MESSAGE-PROCESSING-REFERENCE-2026-08-23.md` — the
  ACCP, octobus frames and the mailbox transport.
