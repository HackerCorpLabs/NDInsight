# CARVE: Operator RUN -> swapper gets real work -> domain runs (the road to NLL:) (2026-07-20)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\CARVE-RUN-TO-WORK-POSTING-CHAIN-2026-07-20.md`
**Track:** SINTRAN carving (Phase 4 - the road to the `NLL:` prompt).
**Method:** direct read of SINTRAN-L NPL (`MP-P2-N500.NPL`, `RP-P2-N500.NPL`) for LOGIC, cross-checked
against L07 symbol tables for the field/message-code VALUES (`SYMBOLS\L07\*.TXT`), and against the D4
finding (`ND500-D4-RUN-BLOCKER-FINDING-2026-07-19.md`) + the context-block carve
(`CARVE-SWAPPER-CONTEXT-BLOCK-BUILDER-2026-07-20.md`).

**Grades:** `[V]` byte/symbol-verified from the L07 tables or from the D4 live harness; `[I]` inferred
from NPL logic (NPL is a DIFFERENT revision than the L07 bytes - logic only, never final byte authority);
`[OPEN]` unresolved. Numbers OCTAL unless prefixed `0x`. ASCII only; "section N" not the sign.

---

## BOTTOM LINE (answers the three questions)

1. **What calls `5ACTSWAPPER` / posts real `SWPINFO` work?** A **PAGE FAULT taken by a RUNNING ND-500
   process** (the domain, or any placed process). The fault arrives as a level-12 STOP message; SINTRAN's
   driver decodes it (`DECOMESS` -> `TRAPDECODER`) and, for trap 46 = PAGE FAULT, calls `5ACTSWAPPER`,
   which writes `SWMSG.SWPINFO` (`HSWPI=104`) to point at the faulting message. It is **NOT** an explicit
   activation issued by RUN, and RUN does **NOT** post swapper work directly. `[V]`

2. **What must the EMULATOR do to trigger it?** The placed domain must **actually START executing** (RUN
   issues a `3START` = mailbox 23B), and then **generate a real level-12 STOP message** - either a PAGE
   FAULT (`STOPR=TRAPCODE`, `TRAPN=46`) which routes to the swapper, or a MONITOR CALL (`STOPR=MOCALL`)
   which routes to `MCHANDLE`. There is no path where posting `SWPINFO` by hand substitutes for the
   domain running and faulting. `[V]`

3. **Cross-check with the RUN precondition (task #13):** RUN of a properly-placed domain issues the
   domain `3START`; execution then reaches `5ACTSWAPPER` ONLY through a page fault, and reaches `NLL:`
   output through `MCHANDLE` (monitor calls). **The current D4 stop is UPSTREAM of all of this**: the
   SWAPPER'S OWN cold-start (process 0) has not completed/parked, so RUN still fails "NO WELL DEFINED
   PROGRAM" and never gets to `3START` the domain. `5ACTSWAPPER` is not even reached yet. `[V]`

---

## THE CHAIN (per-hop, graded)

```
operator RUN
  -> [H0] RUNN precondition (well-defined program)         [V, task #13]
  -> [H1] RUNN issues domain 3START (mailbox 23B)           [I]
  -> [H2] domain EXECUTES on the ND-500 CPU                 [I]
        |-- (a) MON call -> STOPR=MOCALL  -> DECOMESS -> MCHANDLE -> output "NLL:"   [V]
        `-- (b) page fault -> STOPR=TRAPCODE, TRAPN=46 -> DECOMESS -> TRAPDECODER    [V]
  -> [H3] TRAPDECODER: trap 46 = PAGE FAULT, not swapper's own -> CALL 5ACTSWAPPER   [V]
  -> [H4] 5ACTSWAPPER: SWMSG.SWPINFO := faulting message; reactivate swapper         [V]
  -> [H5] swapper wakes with REAL work, pages the fault in (5SWRT/MON 131), restarts [V/I]
        domain -> domain continues -> NLL:
```

### H0 - RUN precondition [V, prior carve task #13]
`RUN`/`GO`/`CONTINUE` all issue `RUNN` = MON 60 subfn `12B`; its "well defined program" runnable state is
set only on the all-success path of PLACE's `START-STANDARD-DOMAIN @043011`. Source:
`ND500-D4-RUN-BLOCKER-FINDING-2026-07-19.md` section 8. Not re-derived here.

### H1 - RUN starts the DOMAIN via mailbox `3START` [I]
The domain is activated by a mailbox message carrying `MICFU = 3START`. Symbol `[V]`: `3STAR=000023`
(= mailbox **23B StartProcess**, `SYMBOLS\L07\*`). The N5XXC driver dispatch has `3START` in slot 23
(`STAOPP`, `MP-P2-N500.NPL:398`). The servicer's 23B StartProcess is REAL/wired to `CpuND500`
(D4 finding section 7). The exact `RUNN -> 3START` emission site is on the ND-100 monitor side
(`nd-500-mon` J04) and is `[I]` here, not carved in this pass.

### H2 - the domain executes and STOPS for service [V]
When the running ND-500 process needs the ND-100, the CPU stops and a level-12 message is delivered to
SINTRAN's driver kernel (`N500`, `NXTMSG` ex-queue loop). The **answer decoder** is `DECOMESS`
(`MP-P2-N500.NPL:803-819`, src `135161`):

```
135167  *MICFU@3 LDATX                                      % MIC.FUNC
135170  IF A=3MONCO OR A=3TRACO OR A=3START OR A=3WMONCO THEN
135204     *AAX STOPR; LDATX
135210     IF A=MOCALL     THEN CALL MCHANDLE               % STOP-REASON = MONITOR CALL
135214     ELSE IF A=5FMOCALL THEN CALL MCHANDLE            %              = file-transfer moncall
135221     ELSE IF A=TRAPCODE THEN CALL TRAPDECODER         %              = TRAP  (-> page-fault path)
135226     ELSE CALL 5RRTWT
```
Symbol pins `[V]`: `MOCAL=000001` (`MOCALL`), `TRAPC=000002` (`TRAPCODE`), `3MONC=000024`, `3STAR=000023`.
- **(a) MON-call sub-path** `[V]`: the LINKAGE-LOADER domain prints `NLL:` through terminal-output
  monitor calls; each stops the CPU with `STOPR=MOCALL` and is serviced by `MCHANDEL`
  (`MP-P2-N500.NPL:1267`, src `136764`), which does the I/O and restarts the domain. **This path never
  touches the swapper.**
- **(b) trap sub-path** `[V]`: a non-resident reference stops with `STOPR=TRAPCODE` -> `TRAPDECODER`.

(A second entry to the same code, `DECOERRMESS` `135240`, routes an error-answer with `TRAPN=46` and a
legal micro-func straight to `ITRAPDECODER` - same page-fault handling.)

### H3 - TRAPDECODER: page fault -> activate swapper [V]
`TRAPDECODER` (`MP-P2-N500.NPL:859-895`, src `135314`):

```
135320  *AAX TRAPN-1; LDDTX                 % D = TRAPNO
135324  IF D>53 THEN ... UNKNOWN TRAP
135327  ELSE IF D = 46 THEN                 % PAGE FAULT
135336  ITRAPDECODER: IF X >< SWMSG THEN
135354     *5RECE@3 LDATX                   % RECEIVER
135357     IF A-5SWPROC=0 GO ITRPERR        % page fault IN the swapper -> background monitor
135361     MSWPFAULT SHZ 10 + D             % pack reason
135364     *AAX TRAPN; STATX
135367     CALL 5ACTSWAPPER                 % <=== ACTIVATE THE SWAPPER
```
Symbol pins `[V]`: `TRAPN=000016` (trap-number field in the message), `MSWPF=000012` (`MSWPFAULT`),
`5SWPR=011254` (`5SWPROC` = swapper process descriptor / receiver id). The NPL comment names `D = 46`
as PAGE FAULT (octal-vs-decimal of the literal not resolved here; the comment is authoritative for
meaning). `[V for the branch, I for the literal base]`

`5ACTSWAPPER` is also called from three other sites, all "a process needs the pager":
`SWMC` (swapper monitor call, `141765`), the `LNEWSWAP` chain that serves the next FIFO waiter
(`136037`), and the resident-place path (`134154`). `[V - grep of MP-P2-N500.NPL]`

### H4 - 5ACTSWAPPER posts the work into SWPINFO [V]
`5ACTSWAPPER` (`MP-P2-N500.NPL:2851-2908`, src `144762`); entry `X = message requiring service`:

```
144766  X=:D=:MSGTOSW; A:=5MBBANK
144771  *NNC24,CNVWADR                       % multiport phys addr of the message
144774  AD=:CMSGTOSW
144775  SWPWAIT; CALL WN5STATUS              % mark faulting proc "waiting for swapper"
144777  X:=SWMSG; CALL RN5STATUS
145001  IF A=PSWWAIT THEN                    % swapper FREE?
145006     AD:=CMSGTOSW; *AAX HSWPI; STDTX   %   SWMSG.SWPINFO := &message   <=== REAL WORK POSTED
145011     SWACTIVE; *AAX SWPFU-HSWPI; STATX %   SWMSG.SWPFU := active
145054     X:=SWMSG; *AAX SWPST; STATX       %   SWMSG.SWPST := reason (page fault)
145071     3MONCO; *MICFU@3 STATX            %   MICFU := 3MONCO (restart swapper after moncall)
145073     CALL MCCO ; CALL XACTRDY          %   reactivate the ND-500 (the swapper)
145111  ELSE
145112     % swapper BUSY -> insert message into the Swap-wait FIFO (X5SWF), served later by LNEWSWAP
```
Symbol pins `[V]`: `HSWPI=000104` (`SWMSG.SWPINFO`), `SWPFU=000101`, `SWPST=000103`, `3MONC=000024`.
This is the write that makes `SWPINFO` non-zero. **It runs only because a running process faulted.**

### H5 - swapper services the fault, restarts the domain [V/I]
Parked process 0 (the swapper) wakes, reads `SWMSG.SWPINFO` to find its work, and pages the fault in.
The disk transfer is done by the ND-100 RT-program `5SWRT` (`RP-P2-N500.NPL:16-58`, src `126464`):
`A:=SWMSG+"SWPINFO"=:D:=5MBBANK; AD=:DSWMSG` (phys addr of `SWPINFO`), then `ABSLI; *MON 131` (ABSTR
disk read of the page). On completion `MONICO`/`5RRTWT` restarts the faulted domain, which resumes and
runs on toward `NLL:`. `[V for 5SWRT/MON 131; I for the full loop-back]`

### The cold-start init message is DISTINCT from fault work [V/I]
At swapper START, `SWMESS`/`MSWSTART` (`MP-P2-N500.NPL:428-462`, src `133635`) also writes
`SWMSG.SWPINFO` (`133654`), but to the swapper's own START message (`MSWST=000007`), not a page-fault
work item. So `SWPINFO` is written on two different occasions: cold-start init (SWMESS) and per-fault
work (5ACTSWAPPER). `[V/I]`

---

## WHY the current D4 state has not reached H1 yet [V, from the finding]

Per `ND500-D4-RUN-BLOCKER-FINDING-2026-07-19.md` sections 12i-12l: on this ND-5800 image, PLACE-DOMAIN
drives `500IN` init (LOAD control store + LOAD swapper) and **the SWAPPER (process 0) is 3START'd and
executes**. It runs clean on `CpuND500` up to `PC=0x0800913B`, then dereferences a null taken from an
EMPTY message (SWMSG buffer reads zero). That is the **swapper's COLD-START** path (`SWMESS`/`MSWSTART`),
which should let the swapper initialize and PARK. Because it does not complete, PLACE never finishes, so:
- RUN still prints `NO WELL DEFINED PROGRAM` (precondition H0 never satisfied), and
- the DOMAIN `LINKAGE-LOAD-H02` is **never** `3START`'d, so no domain fault ever occurs, so
- `TRAPDECODER` and `5ACTSWAPPER` are **not reached at all** in the current state.

So the swapper-work-posting chain (H3-H5) is real and carved, but it sits BEHIND two gates that are not
yet open: (1) swapper cold-start must park; (2) RUN must `3START` the domain.

---

## WHAT THE EMULATOR MUST DO TO REACH NLL:

In order, each a distinct requirement:

1. **Make the swapper's cold-start COMPLETE and PARK (current blocker).** The swapper reads its START
   message via `SWMSG.SWPINFO` and must reach its idle/park state, not null-deref. Either the START
   message (`SWMESS`/`MSWSTART`, `SWPINFO=start msg`) must be delivered non-empty, or the swapper's
   "no work -> park" branch must be taken. This is the top item (D4 finding 12l). `[V - this is the wall]`

2. **Let RUN issue a real `3START` (mailbox 23B) for the DOMAIN.** Already wired: servicer 23B
   StartProcess is REAL (D4 finding section 7). Gated only by H0 (needs gate 1 first). `[V]`

3. **Run the DOMAIN on `CpuND500` and deliver its STOP messages faithfully:**
   - **Monitor calls** (how `NLL:` is printed): the domain stops with `STOPR=MOCALL` -> the emulator's
     3022/mailbox must present that stop so `DECOMESS -> MCHANDLE` services the output. This is the
     MINIMAL path to `NLL:` and **needs the swapper only if the domain page-faults**. `[V]`
   - **Page faults** (only if the placed domain is demand-paged): the `CpuND500` page fault
     (`LastPageFault`) must be surfaced to the mailbox as a level-12 STOP with `STOPR=TRAPCODE` and
     `TRAPN=46`, NOT swallowed. Only that runs `TRAPDECODER -> 5ACTSWAPPER` and pages it in via `5SWRT`
     (`MON 131`). `[V]`

4. **Do NOT fake `SWPINFO` to shortcut this.** RUN posts no swapper work; the swapper only ever gets work
   from a RUNNING process faulting (H2b -> H3 -> H4). A hand-written `SWPINFO` is not a substitute for the
   domain executing. `[V]`

**Important honest nuance `[OPEN]`:** whether `LINKAGE-LOAD-H02` demand-faults at all is unresolved. If
PLACE-DOMAIN placed its segments RESIDENT (the observed `14B RESIWR` burst into ND-500 memory) and the
`CpuND500` MMU maps them, the domain can run to `NLL:` with **zero** swapper involvement - purely via the
MON-call path (H2a / `MCHANDLE`), with the swapper idle. In that case the swapper-work chain (H3-H5) is
NOT on the critical path to `NLL:`; the critical path is: swapper parks (gate 1) -> domain `3START`
(gate 2) -> domain MON-call output (`MCHANDLE`). The swapper-work chain becomes load-bearing only for a
domain that actually demand-pages. Settling which case applies needs a live run once gate 1 is open.

---

## Evidence index

| Claim | Source | Grade |
|---|---|---|
| DECOMESS routes STOPR=TRAPCODE -> TRAPDECODER, MOCALL -> MCHANDLE | `MP-P2-N500.NPL:803-819` (135161) | [V] |
| TRAPDECODER: trap 46 = PAGE FAULT -> CALL 5ACTSWAPPER | `MP-P2-N500.NPL:859-895` (135314) | [V] |
| DECOERRMESS: TRAPN=46 + legal micfunc -> ITRAPDECODER | `MP-P2-N500.NPL:836-844` (135240) | [V] |
| 5ACTSWAPPER writes SWMSG.SWPINFO := faulting msg, or FIFO-queues | `MP-P2-N500.NPL:2851-2908` (144762) | [V] |
| 5ACTSWAPPER call sites (TRAPDECODER/SWMC/LNEWSWAP/resident-place) | grep `MP-P2-N500.NPL` (510/879/1052/2050) | [V] |
| SWMESS/MSWSTART sets SWPINFO to the START message (cold-start) | `MP-P2-N500.NPL:428-462` (133635) | [V/I] |
| Domain start = mailbox 3START (=23B); N5XXC slot 23 = STAOPP | `MP-P2-N500.NPL:398`; `3STAR=000023` | [V/I] |
| MCHANDEL services domain monitor calls (NLL: output path) | `MP-P2-N500.NPL:1267` (136764) | [V] |
| 5SWRT pages the fault in via MON 131 ABSTR | `RP-P2-N500.NPL:16-58` (126464) | [V] |
| Field/code values HSWPI/TRAPN/SWPFU/SWPST/3MONCO/3START/MOCALL/TRAPCODE/5SWPROC/MSWPFAULT | `SYMBOLS\L07\*.TXT` | [V] |
| Current D4 stop is swapper cold-start (0x913B null-deref), not the domain | `ND500-D4-RUN-BLOCKER-FINDING-2026-07-19.md` 12i-12l | [V] |
| Whether LINKAGE-LOAD-H02 demand-faults or runs resident | (needs live run) | [OPEN] |
