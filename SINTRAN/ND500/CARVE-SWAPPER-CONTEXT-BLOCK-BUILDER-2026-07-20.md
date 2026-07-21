# CARVE: Does SINTRAN build the swapper's context block + PST + PCB? (2026-07-20)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\CARVE-SWAPPER-CONTEXT-BLOCK-BUILDER-2026-07-20.md`
**Track:** SINTRAN carving (Phase 2 remaining open question; feeds Phase 1 D4 stop + Phase 4).
**Method:** direct read of SINTRAN-L NPL (`MP-P2-N500.NPL`, `RP-P2-N500.NPL`, `5P-P2-MON60.NPL`,
`CC-P2-N500.NPL`) + L07 symbol tables, cross-checked against the B30 microcode answers
(`MICROCODE-ANSWER-C1-PCB-PST-BUILDER-2026-07-20.md`, `MICROCODE-ANSWER-PSTP-AND-SEGMENT-2026-07-20.md`)
and the swapper entry carve (`CARVE-SWAPPER-ENTRY-STARTUP-2026-07-20.md`).

**Grades:** `[V]` byte/symbol-verified from the L07 tables; `[I]` inferred from NPL logic (NPL is a
DIFFERENT revision from the L07 bytes - used for logic, never as final byte authority); `[OPEN]`
unresolved. Numbers are OCTAL unless prefixed `0x`. No section-sign; ASCII only.

---

## BOTTOM LINE

**SINTRAN builds NEITHER the swapper's per-process context/register block, NOR the PST, NOR the PCB.**
In the swapper-start path it constructs ONE thing in ND-500-visible memory: the **mailbox MESSAGE**
(`SWMSG`) in the 5MPM shared window. The per-process context/register block DOES exist and SINTRAN
KNOWS its geometry (symbols `CNTXPAGE`/`ADRZERO`/`REGBSZ`/`ERREG`), but SINTRAN only ever **READS** it
(after a trap) - no SINTRAN routine WRITES `P/PS/DOM`/register words into it to seed a fresh context.
The PST (physical 2) and the PCB (capabilities) appear NOWHERE in the SINTRAN ND-500 source at all.

Consequence: the swapper's `P=4 / PS=1 / DOM=1` at first execution cannot come from a SINTRAN-built
context block, because SINTRAN builds none. It comes from the **microcode** - most consistently the
ACCP cold-start vector `MACRO_STARTL` (which sets those exact values as literals). The
mailbox-23B/`CNTXTLOAD` path stays viable only for a WARM restart, where a prior microcode `CNTXTSAVE`
has already populated the block. See the discriminator (section 5).

---

## 1. What SINTRAN's swapper-START handler actually writes [I from NPL; field offsets V]

`SWMESS` / `MSWSTART`, `MP-P2-N500.NPL:428-462` (octal src addr `133635`-`133752`). This is the body
that runs for START-SWAPPER (MON-60 subfn 54B `STSWP` -> `RUNSW` -> `MSWSTART`). Every store below is
an indexed store into `SWMSG` (base `T:=5MBBANK`), i.e. into the **5MPM shared window** - NOT into
ND-500 local physical memory:

| src | NPL (`MP-P2-N500.NPL`) | Writes into SWMSG | Field offset [V, L07] |
|---|---|---|---|
| `133654` | `*AAX HSWPI; STDTX` (`MMESSAGE=:SWMSG.SWPINFO`) | `SWPINFO` = ptr to activating msg | `HSWPI=000104` |
| `133661` | `3START; *MICFU@3 STATX` | `MICFU` = 3START (=23B) | `MICFU=000006` |
| `133663` | `5SWPROC; *SENDE@3 STATX; 5RECE@3 STATX` | sender/receiver = swapper proc | `SENDE=000003`,`5RECE=000004` |
| `133666` | `SWACTIVE; *AAX SWPFU; STATX` | `SWPFU` = SWACTIVE | `SWPFU=000101`,`SWACT=000000` |
| `133671` | `A:=300; *AAX 5PRIO-SWPFU; STATX` | priority 300 | `5PRIO=177773` (= -5) |
| `133674` | `CPUNO; *AAX 5CPUN-5PRIO; STATX` | target CPU number | - |
| `133742` | `SWME1: CALL XACTRDY; CALL LOWACT500` | activate ND-500 | - |

L07 symbol pins (all `[V]`, `SYMBOL-1/2-LIST.SYMB.TXT`): `SWMSG=110054`, `S500S=115542`,
`5SWPR=011254`, `5MMES=111101`, `55MES=000200` (msg size 128 words), `MSWST=000007`,
`SWPIN=000105`, `SWPST=000103`.

**There is no PST write, no PCB/capability write, no register-image write, and no store to ND-500
low physical memory anywhere in this handler.** The only ND-500-memory writes are the 5MPM mailbox
message fields above. `[I]`

The page-fault activation path is identical in kind - `5ACTSWAPPER`, `MP-P2-N500.NPL:2857-2907`
(`144762`-`145147`): it too only fills `SWMSG` fields (`HSWPI`, `SWPFU`, `SWPST`, `MICFU=3MONCO`,
`NUMPA`, ...) and activates. No context/PST/PCB construction. `[I]`

`P0START` (the 22B handler, `MP-P2-N500.NPL:134500-134521`) inserts a `WATCHDOG` message and
reactivates - it builds no context either (re-confirms the prior carve: 22B is the watchdog, not the
swapper start). `[I]`

---

## 2. The context/register block DOES exist and SINTRAN knows its geometry - but only READS it [V+I]

The per-process context/register block that the microcode `CNTXTLOAD`/`CNTXTSAVE` reads/writes at
physical `0o4000 + index*0o400` is the SAME structure SINTRAN addresses through the ND-500 CPU
datafield `N500DF`. The one and only reference:

`GERRC` ("Get Error Code, used after a programmed trap"), `MP-P2-N500.NPL:1941-1943` (`141435`):

```
141435   A-5SWPROC+1*REGBSZ+"ERREG"=:T                     % offset into the register block
141442   "N500DF".CNTXPAGE+X.ADRZERO=:D:=0                 % base = context-page + ADRZERO
141447   AD SHZ 12; D+T; ... *LDDTX                        % READ the ERREG word
141455   *STZTX; AAX 1; STZTX                              % zero it after reading
```

L07 symbols (all `[V]`, `SYMBOL-1-LIST` / `N500-SYMBOLS`):

| Symbol | Octal | Meaning |
|---|---|---|
| `CNTXP` | `000057` | offset in `N500DF` of the context-page base |
| `ADRZE` | `000060` | offset in `N500DF` of `ADRZERO` (ND-500 zero addr in the window) |
| `REGBS` | `000200` | register-block size = 128 words per process |
| `ERREG` | `000152` | error-register offset inside a block |

**Cross-check that this is the microcode's block:** REGBSZ = `0o200` words = 128 words =
`0o400` **bytes** = the microcode's per-process stride (`CNTXTLOAD` `SC12 := index*0o400`,
`MICROCODE-ANSWER-...-C1...:54-58`). The stride matches exactly. `[V/I]`

**But `CNTXPAGE` appears in EXACTLY ONE place in the entire SINTRAN ND-500 source (this GERRC read).**
No SINTRAN routine WRITES `P`, `PS`, `DOM`, or the register file into `CNTXPAGE+ADRZERO` to SEED a
fresh context. The block is **written by the microcode** (`CNTXTSAVE`, on a trap/context-switch) and
**read back by SINTRAN** (`GERRC`, to recover the trap error code). SINTRAN is a consumer of that
block, not its producer. `[I - single-reference absence]`

---

## 3. PST and PCB: absent from SINTRAN entirely [I - absence]

Grep of all four ND-500 NPL files for PST-root / capability construction (`PSTP`, `PST`, capability
table, `PSTBASE`, PCB): **zero** hits that build such a table. The microcode roots the PST at fixed
physical `PSTP=2` (set once at CPU init, `MICROCODE-ANSWER-...-C1...:83-90`) and never reloads it.
SINTRAN neither sets nor references PSTP. So the PST and PCB are **not** SINTRAN's to build. This
agrees with the microcode side (PSTP is a control-store constant) and the swapper side (the swapper
writes no PST/PCB either, `CARVE-SWAPPER-ENTRY-STARTUP-2026-07-20.md` section 3). `[I]`

---

## 4. What SINTRAN DOES put into ND-500-visible memory for the swapper (the complete list) [V/I]

1. **The swapper PSEG image** - by ordinary disk-controller DMA into ND-100 physical memory
   (`LDSWA`->`PLSWA`-> `MON 50` OPEN + `MON 131` ABSTR, page from `MON 61` FIXC5), landing at MPM
   physical `0x06F800`, byte-identical to `SWAPPER-K01.PSEG`. `[V, prior carve - status doc section 11]`
   (**Known gap:** the DSEG CONTENT never arrives - status doc section 11.)
2. **The mailbox MESSAGE** (`SWMSG` fields, section 1). `[I]`
3. **Message/process-descriptor buffers** at init - `MSINIT`/`XMSINIT` allocate `SWMSG`, `S500S`, and
   the per-process message buffers in the 5MPM window (`ND500-SWAPPER-LOADING-MECHANISM.md` section 1.2).
   These are the MESSAGE plumbing, not the register context block. `[I]`

That is the whole set. **No context/register block, no PST, no PCB.**

---

## 5. The SINTRAN-vs-ACCP discriminator (answers question 2)

The observed `P=4/PS=1/DOM=1` matches BOTH candidate start paths (mailbox-23B `CNTXTLOAD` reading a
context block, vs the ACCP `MACRO_STARTL` cold-start literals). This carve supplies the discriminator:

- The **mailbox-23B / `CNTXTLOAD`** path REQUIRES a pre-populated context block at `CNTXPAGE+ADRZERO`
  (`P`,`L`,`X/A/E`,`PS`,`DOM`,`ADOM`,`MOD` are all READ from it - microcode C1 answer sections 3-4).
- SINTRAN provably does **not** seed that block (section 2: `CNTXPAGE` is read-only in SINTRAN; the
  block is written only by the microcode's `CNTXTSAVE`, which runs only AFTER a process has executed).
- On a **cold** swapper start there has been no prior `CNTXTSAVE`, so the block is unseeded. A
  23B/`CNTXTLOAD` start would therefore load undefined `P/PS/DOM` - it could NOT deterministically
  produce `P=4/PS=1`.
- The ACCP **`MACRO_STARTL @000033`** cold-start vector produces `P=4, PS=1, DOM=ADOM=1, PSTP=0` as
  microcode LITERALS with no memory dependency (`MICROCODE-ANSWER-PSTP-AND-SEGMENT...:145-164`).

**Therefore, for the FIRST (cold) swapper start on this ND-5800 B30 image, the ACCP cold-start vector
is the mechanism that fits the evidence; the mailbox-23B/`CNTXTLOAD` path is a WARM-restart mechanism
(valid once the block has been saved).** This also fits the octobus/ACCP driving model: SINTRAN brings
the 5800 up with octobus ACCP commands (`XRS5CPU` sends `CMCPURES` "Reset CPU", `MP-P2-N500.NPL:3334-3337`;
plus control-store load and micro-clock start) - and the CPU then runs from its reset/cold-start vector.
`[I - strong inference, NOT [V]]`

**Honesty caveat:** this is inference, not a byte-proof that a specific ACCP command is issued for the
swapper. Two facts keep it `[OPEN]`: (a) SINTRAN's `SWMESS` DEMONSTRABLY can and does write
`MICFU=3START` into the mailbox (section 1), so SINTRAN retains a 23B start wire even here; (b) the
`P=4/PS=1` we "observe" is produced by the emulator's hand-built swapper start, so it is not
independent evidence of the real hardware path. What is solid and new here is the negative:
**SINTRAN does not build the context block / PST / PCB.** Who seeds the context block on a cold start
is the microcode/ACCP's job, not SINTRAN's.

**Discriminator to run next (LIVE):** dump ND-500 physical `CNTXPAGE+ADRZERO` (the process-0 block)
IMMEDIATELY before the first swapper instruction. If `P/PS/DOM` words are present there -> a
`CNTXTLOAD`/23B start seeded from a saved block. If the block is empty/zero yet the CPU still runs at
`P=4/PS=1` -> the cold-start vector supplied the literals and no context block was used. In parallel,
trace whether SINTRAN issues mailbox function 23 OR an ACCP execute-microroutine command as the kick.

---

## 6. Bonus (question 3): the swapper's first work-message pointer = `SWMSG.SWPINFO` [V+I]

The swapper's first work item is reached through `SWMSG.SWPINFO` (offset `HSWPI=000104` `[V]`).
Both start paths set it to the CONVERTED (multiport) address of the activating message:

- START: `SWMESS` `133651 *NNC06,CNVWADR` then `133654 *AAX HSWPI; STDTX` = `MMESSAGE =: SWMSG.SWPINFO`.
- Page-fault: `5ACTSWAPPER` `144771 CNVWADR ... 145006 AD:=CMSGTOSW; *AAX HSWPI; STDTX`.

The swapper reads its message via `MON 377B` sub-fn 1 then `RIOM` from an ND-100 physical address
(`CARVE-SWAPPER-ENTRY-STARTUP-2026-07-20.md` section 3.1; D4 stop at `PC=0x0800913B`, `r2=0`).

**Tie to the D4 null-deref:** if the swapper is kicked WITHOUT `SWMESS`/`5ACTSWAPPER` having run for a
real activating process (e.g. a synthetic/cold start with no page-faulting ND-500 process behind it),
`SWMSG.SWPINFO` is never set - it stays zero - and the swapper's `RIOM` pulls an empty message, giving
exactly the `r2=0` null-deref at `0x913B`. So the D4 stop is the SAME gap as the context question:
the swapper is being STARTED, but nothing SINTRAN-side has POSTED it a valid work message
(`SWPINFO`). `[I - consistent, not proven end-to-end]`

*Not carved here:* the exact ND-100 physical source address the `RIOM` computes from `SWPINFO`, and
whether the swapper's very first loop iteration expects a real message or tolerates an empty one. That
needs the swapper `MON 377`/`RIOM` operand math traced (see the swapper handlers doc).

---

## 7. WHAT REMAINS OPEN

1. **[OPEN, LIVE]** Which start path actually kicks the cold swapper on B30 - ACCP `MACRO_STARTL`
   cold-start vector vs mailbox 23B `CNTXTLOAD`. Section 5 argues cold-start vector by elimination
   (SINTRAN seeds no block), but it is inference. Settle by dumping `CNTXPAGE+ADRZERO` before the
   first swapper instruction and tracing the kick command.
2. **[OPEN, CARVE]** Whether the ACCP firmware (outside every carve here) or any un-read part of
   `030-S3SM5` seeds the context block on cold start. This carve covered the swapper-start handlers and
   the single `CNTXPAGE` reference; a full `030-S3SM5` pass would close whether SINTRAN writes the block
   anywhere the NPL revision does not show.
3. **[OPEN]** The exact byte layout inside the `REGBSZ`-sized register block (`P/L/PS/DOM` field
   offsets) vs the microcode `CNTXTLOAD` read order - only `ERREG=0o152` is pinned from SINTRAN here.
4. **[OPEN]** The `RIOM` source-address computation from `SWPINFO` (question 3 residue), needed to
   confirm the D4 `r2=0` is precisely an unset `SWPINFO`.
5. **[OPEN]** Whether, on a WARM restart, the 23B/`CNTXTLOAD` path is genuinely exercised on this
   image (would confirm the block is microcode-seeded and reused).

---

## 8. Evidence index

| Claim | Source | Grade |
|---|---|---|
| SWMESS writes only SWMSG mailbox fields, no context/PST/PCB | `MP-P2-N500.NPL:428-462` | [I] |
| Message field offsets (HSWPI/MICFU/SENDE/5RECE/SWPFU/SWPST) | `SYMBOLS/L07/SYMBOL-1-LIST.SYMB.TXT` | [V] |
| Buffer/proc symbols (SWMSG/S500S/5SWPR/5MMES/55MES) | `SYMBOLS/L07/SYMBOL-2-LIST` + `-1-LIST` | [V] |
| Context block geometry known to SINTRAN (CNTXP/ADRZE/REGBS/ERREG) | `SYMBOLS/L07/*`; `MP-P2-N500.NPL:1941-1943` | [V] |
| CNTXPAGE referenced exactly once (GERRC read after trap) | grep `CNTXPAGE` across `*N500.NPL` -> 1 hit | [I] |
| REGBSZ 0o200 words = 0o400-byte microcode stride | `REGBS=000200` vs C1 answer sections 54-58 | [V/I] |
| No PST/PCB construction in SINTRAN | absence across `*N500.NPL` | [I] |
| PSEG arrives by disk DMA to 0x06F800, DSEG content missing | status doc section 11 (prior carve) | [V] |
| ACCP reset-cpu command wire (CMCPURES) | `MP-P2-N500.NPL:3334-3337`; `CMCPU=000071` | [V] |
| Cold-start literals P=4/PS=1/DOM=1 | `MICROCODE-ANSWER-PSTP-AND-SEGMENT...:145-164` | [V, microcode track] |
