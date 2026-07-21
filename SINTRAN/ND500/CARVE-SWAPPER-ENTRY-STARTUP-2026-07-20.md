# CARVE: SWAPPER-K01 entry / startup - does it build its own MMU state, and is the MMU already on?

**Date:** 2026-07-20
**Track:** SINTRAN carving (software-side cross-check of the ND-5000 microcode swapper-start questions)
**Method:** direct read of the disassembled swapper bytes. Every claim below cites
`SINTRAN/ND500/swapper/swapper-k01-pseg.asm` by line number and shows the instruction. Opcode
semantics are cited to `Reference-Manuals/ND-05.009.4 EN ND-500 Reference Manual.md` (chapter 16,
Special Instructions). Numbers in the listing are OCTAL; hex is prefixed `0x`.

This document answers two questions the microcode track is answering from the other side, so the two
answers can be corroborated:

- **Q1 (cross-checks C1):** does the swapper build its own PCB (capabilities) and PST once running?
- **Q2 (cross-checks C5):** does the swapper assume the MMU is already ON at entry, or does it enable
  translation itself?

Plus a note on what the swapper assumes about its registers on entry.

---

## 0. TL;DR

- **Q1 (build own PCB/PST?): NO - PROVEN by absence + design.** The startup path (entry -> stack
  INIT -> build-tag check -> main loop) writes no PST, no capability table, and never loads a
  PST-base or any capability register. Moreover the swapper reaches physical memory through the
  DMOF/DMON "turn data translation off for one access" escape hatch (66 sites), NOT through mapped
  capabilities - which is exactly what a domain does when it does NOT own capabilities for that
  memory. It assumes its translation state already exists when it starts.
- **Q2 (MMU already on?): YES, ALREADY ON - PROVEN.** The swapper never enables translation. It
  never executes PMON or DMON as an *initialisation*; the only DMON sites are *restores* that follow
  a transient DMOF. Program translation is never touched at all (PMON/PMOF: **zero** occurrences),
  so the swapper's own code (segment 1, VA 0x08000000) runs translated from the first instruction on
  a mapping it did not create. Data translation is ambient-ON: every DMOF is paired with a following
  DMON that puts it back.

---

## 1. The instructions involved, from the manual (semantics, not guessed)

`Reference-Manuals/ND-05.009.4 EN ND-500 Reference Manual.md`:

| Mnemonic | Octal opcode | Listing bytes | Meaning (manual line) |
|---|---|---|---|
| DMON | 177426B | `377 026` | data memory management ON. "Following data accesses will be mapped on a physical segment through the MMS." "If already on, no effect." (line 10486/10495/10497) |
| DMOF | 177430B | `377 030` | data memory management OFF. "Following data accesses will be interpreted directly as physical addresses." (line 10560/10569) |
| PMON | 177427B | `377 027` | program memory management ON. "Following instruction accesses will be mapped..." L->P. (line 10521/10531) |
| PMOF | 177431B | `377 031` | program memory management OFF. (line 10593) |
| PCTSB | 177434B | `377 034` | clear PROGRAM translation speedup buffer (TLB) (line 10868) |
| DCTSB | 177435B | `377 035` | clear DATA translation speedup buffer (TLB) (line 10869) |
| DCC | 177425B | `377 025` | data cache clear (line 10392) |

Note the opcode->bytes match exactly: DMON 177426B == `377 026`, DMOF 177430B == `377 030`. This
is how the mnemonics below are grounded.

---

## 2. Q2 first (it is the cleaner proof): the MMU is already ON at entry

### 2.1 Program translation is never touched - PROVEN by absence

Grep of `swapper-k01-pseg.asm` for the program-MMU instructions PMON (`377 027`) and PMOF
(`377 031`), and for the mnemonics `pmon`/`pmof`:

```
pmon|pmof|377 027|377 031  ->  0 occurrences
```

The swapper contains **zero** program-memory-management instructions. Yet it demonstrably executes
translated code: entry is at PSEG+4 = VA `0x08000004` (segment 1), and the main body it calls is at
`0x080081A5` (see section 3). It never enables program translation, so **program translation must
already be ON at entry**, mapping segment 1's code to the physical code pages. The swapper relies on
a program mapping it did not create.

### 2.2 Data translation is ambient-ON; DMOF/DMON are transient physical windows - PROVEN

There are 66 DMOF/DMON pairs (132 instructions). Every one has the identical shape: set a pointer in
R, DMOF, do exactly ONE load/store against `r.0`, DMON. Two adjacent examples from the first physical
helper routine (`ents $64` at `swapper-k01-pseg.asm:173`):

```
swapper-k01-pseg.asm:193   1000001263: r:=    b.44
swapper-k01-pseg.asm:194   1000001265: dmof            ; data MMU OFF  -> next access is PHYSICAL
swapper-k01-pseg.asm:195   1000001267: h1 :=  r.0      ; read one halfword from physical [r.0]
swapper-k01-pseg.asm:196   1000001271: dmon            ; data MMU ON   -> restore ambient state
...
swapper-k01-pseg.asm:198   1000001275: r:=    b.44
swapper-k01-pseg.asm:199   1000001277: dmof
swapper-k01-pseg.asm:200   1000001301: w1 :=  r.0      ; read one word from physical [r.0]
swapper-k01-pseg.asm:201   1000001303: dmon
```

The controlling fact: **the sequence always ends by executing DMON (turn ON), never DMOF.** If the
ambient state were OFF, the swapper would corrupt every subsequent normal data access by leaving the
MMU ON; instead it *restores* ON. Per the manual "if already on, no effect" - the restoring DMON is a
no-op when the swapper is not inside one of these windows, i.e. the ambient/entry state is ON. The
build-tag self-check at entry proves data translation is ON before the first DMOF is ever reached:

```
swapper-k01-pseg.asm:19    1000000026: w comp2 $1000224030,$12221253056   ; reads DSEG 0x08012818 ('REV.')
swapper-k01-pseg.asm:23    1000000047: w comp2 $1000224034,$5522630061    ; reads DSEG 0x0801281C ('-K01')
```

`$1000224030 = 0x08012818` is a segment-1 DATA address. It is read with NO surrounding DMOF/DMON, so
it is a translated access - data translation is already ON at entry, before any DMOF appears in the
program.

**Verdict Q2: the swapper assumes the MMU (both program and data) is ALREADY ON at entry. It never
enables translation. It only transiently DISABLES data translation (DMOF) for individual
physical-memory accesses - its paging job - and immediately restores it (DMON). This directly
cross-checks C5: at swapper start translation is ON; the swapper does NOT begin untranslated and it
does NOT switch the MMU on itself.**

---

## 3. Q1: the swapper does NOT build its own PCB/PST

### 3.1 The entire startup path, cited

Entry is at PSEG+4:

```
swapper-k01-pseg.asm:16    1000000004: init  $1000441124,$44,$17504   ; stack bottom 0x08024254, size 8004
swapper-k01-pseg.asm:17    1000000021: w move $32,b.24
swapper-k01-pseg.asm:18    1000000024: w stz  b.34
swapper-k01-pseg.asm:19    1000000026: w comp2 $1000224030,...          ; build-tag 'REV.'
swapper-k01-pseg.asm:23    1000000047: w comp2 $1000224034,...          ; build-tag '-K01'
swapper-k01-pseg.asm:26-29 ...                                          ; AND the two match-flags
swapper-k01-pseg.asm:30    1000000076: if >< go $24                     ; tag OK -> jump to the main call (line 38)
swapper-k01-pseg.asm:38    1000000122: call  $1000100645,$0            ; call MAIN body @ 0x080081A5
swapper-k01-pseg.asm:39    1000000130: ifkret
swapper-k01-pseg.asm:40    1000000131: call  $0xF8000000,$0            ; MON 0B LEAVE (only if main returns)
```

On a normal (tag-matching) start the sequence is exactly: **INIT stack -> call main -> (main never
returns).** Between INIT and the main call there is NO instruction that writes a page table, a
capability table, or a PST, and none that loads a PST-base or capability register. `INIT` only
establishes the run-time stack (bottom `0x08024254`, per the operand and the deep-analysis).

The main body:

```
swapper-k01-pseg.asm:10495  1000100645: ents  $210                     ; MAIN (0x080081A5)
swapper-k01-pseg.asm:10496-10498 ...   clear message-control words 0x080240B0/0x080240B4
swapper-k01-pseg.asm:10535  ...        MON 377B sub-fn 1  (ask ND-100 for a message)
swapper-k01-pseg.asm:10577  ...        h riom $1000440264,$1000440274,...  ; DMA-pull the message
swapper-k01-pseg.asm:10600  1000101546: jumpg $1000460630+              ; dispatch on 29-entry table
```

The main body goes straight into the message-receive-and-dispatch loop. **No PST/PCB/capability
construction anywhere on the path from entry to the first loop iteration.**

### 3.2 The design tell: physical access via DMOF, not via capabilities

A domain that owned capabilities for the memory it manages would simply address that memory with
normal (translated) loads/stores through its own segments. The swapper does the opposite: it reaches
page-frame / page-table memory by turning data translation OFF (DMOF), touching the physical address,
and turning it back ON (66 sites, section 2.2). This is only necessary because the swapper does NOT
hold capabilities mapping that physical memory - i.e. it did NOT build a PCB covering it. This is
positive evidence, not just absence: the swapper's whole physical-memory strategy presupposes it has
NO self-built capability set for the pages it moves.

### 3.3 What the swapper DOES do to translation: flush stale entries, never build

The swapper issues the TLB/cache-invalidation primitives, reachable from the main loop (the routine
at `swapper-k01-pseg.asm:60`, `ents $30`, is called from the main loop at
`swapper-k01-pseg.asm:10532` `call $1000000215`):

```
swapper-k01-pseg.asm:88    1000000370: dctsb    ; clear DATA translation speedup buffer (TLB)
swapper-k01-pseg.asm:95    1000000432: pctsb    ; clear PROGRAM translation speedup buffer (TLB)
swapper-k01-pseg.asm:161   1000001070: dcc      ; data cache clear
```

These CLEAR the translation-speedup buffer (TLB) and cache - they are coherence operations performed
AFTER the swapper edits page tables in physical memory (via DMOF writes), so a stale cached
translation is not used. They do not build a PST and cannot: PCTSB/DCTSB only invalidate. The
presence of PCTSB (program-side TLB flush) is itself further proof that program translation is ON and
in use (you do not flush a TLB that is not active).

**Verdict Q1: the swapper does NOT build its own PCB or PST at startup (or at all). Right after entry
it performs a stack INIT and a build-tag self-check, then enters its message loop. It writes no
capability table and no PST, loads no PST-base, and reaches the physical memory it manages by
disabling data translation rather than by mapping it. It therefore ASSUMES the PCB/PST/capabilities
that map its own code and data (segment 1) already exist when it starts. This directly cross-checks
C1: the swapper is NOT the builder of the PCB/PST - that state is set up by something else (microcode
at process start, or SINTRAN) before the first swapper instruction runs.**

---

## 4. What the swapper assumes about its registers / state on entry

From the startup bytes:

- **It relies on B / stack registers being valid enough for `INIT` to build the frame** (line 16).
  `INIT` sets the stack bottom to `0x08024254`; the swapper does not compute or load a stack pointer
  from PS or from any table first.
- **It relies on segment-1 program translation** so that PSEG+4 and the main body `0x080081A5` fetch
  and execute (section 2.1).
- **It relies on segment-1 data translation** so that the absolute DSEG reads at entry
  (`0x08012818` / `0x0801281C`, lines 19/23) resolve, and so that the stack it just INIT'd is usable.
- **It does NOT read PS, and does NOT read or load PSTP, anywhere on the startup path.** There is no
  `PST[PS]`-style lookup in the swapper's own code; it never consults PS to find its tables. It uses
  B-relative (`b.NN`), R-relative (`r.NN`) and absolute segment-1 addressing throughout. (This is a
  useful cross-check for the microcode track's B3/B4: the swapper does not itself use PS or PSTP to
  bootstrap, so whatever the microcode does with PS/PSTP at 3START must be complete BEFORE the
  swapper's first instruction, because the swapper never finishes that job for it.)
- **It requires supervisor/privileged mode**: DMON/DMOF/PCTSB/DCTSB/RIOM are all privileged
  instructions per the manual; the swapper runs them without first raising privilege, so it assumes
  it is entered privileged.

---

## 5. What remains UNKNOWN (with the experiment that would settle each)

1. **WHO built the PCB/PST, and WHERE.** The bytes prove the swapper did not; they do NOT prove
   whether the microcode built it at 3START, or SINTRAN built it in ND-500 local memory, or it lives
   in CPU-internal state. *Experiment:* this is exactly microcode question C1 - answer it from the
   microcode side, or dump ND-500 local memory (not just the shared window) immediately before the
   first swapper instruction and search for a PSTE naming the swapper's code/data page tables.
2. **The exact ambient MMU mode bits at entry** (e.g. is program translation on AND data translation
   on, versus data-on-only with code in a special mode). The bytes prove both program and data
   translation are effectively ON (code runs translated; data reads translated), but the swapper
   never reads the mode register, so the precise status-word contents at entry are not visible here.
   *Experiment:* single-step the CpuND500 to the first swapper instruction and read the MMU
   status/PS registers.
3. **What the `comp2 $1000436534,$0` gate at the physical-helper routines selects** (lines 175, 191,
   234, 291, ...). It switches each physical helper between a halfword (`h1`) and a word (`w1`)
   DMOF/DMON access. This looks like a 16-bit-vs-32-bit page-table-entry-width mode flag, but its
   origin is not established from the startup path. *Experiment:* trace who writes `0x08023D1C`
   (=`$1000436534`) before the swapper reaches these routines.
4. **Whether any swapper HANDLER (function codes 0..28), as opposed to the startup path, ever writes a
   PSTE/capability for a third party.** This carve covered the startup path and the physical-access
   idiom; it did not exhaustively decode all 29 handlers' table writes. Even if some handler writes
   page-table entries for the domains it pages, that is service to OTHER processes, not the swapper
   building its OWN PCB/PST - but the handler-by-handler table-write inventory is not done here.
   *Experiment:* per-handler trace (see `swapper-k01-handlers.md`).

---

## 6. Corroboration summary for the microcode track

| Their question | This carve's answer | Strength |
|---|---|---|
| C1 - does the swapper build the PCB/PST? | **NO.** No PST/PCB/capability write on the startup path; reaches physical memory via DMOF not via owned capabilities. Builder must be microcode or SINTRAN, before first swapper instruction. | PROVEN (absence + design idiom) |
| C5 - is the MMU on at swapper start? | **YES, already ON** (both program and data). Swapper never enables translation; PMON/PMOF absent; every DMON is a restore after a transient DMOF. | PROVEN |
| B3/B4 (side note) - does the swapper use PS/PSTP to bootstrap? | **NO.** Startup never reads PS or loads PSTP; so PS/PSTP handling must be complete before entry. | PROVEN (absence) |

---

## 7. File written

`E:\Dev\Ronny\NDInsight\SINTRAN\ND500\CARVE-SWAPPER-ENTRY-STARTUP-2026-07-20.md` (this document).
Primary evidence: `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\swapper\swapper-k01-pseg.asm`; opcode
semantics: `E:\Dev\Ronny\NDInsight\Reference-Manuals\ND-05.009.4 EN ND-500 Reference Manual.md`
chapter 16.
