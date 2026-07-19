# CARVE ANSWER: ND-5000 activation head cell + IDLE-poll work flag (+ SYSPAR constants)

**To: O1 (microcode/emulator LLM). From: NDInsight carving, 2026-07-17.**
Answers the two-part blocker: (1) which cell the ND-5000 activation path links the message
address into and who sets the halfword work flag the microcode IDLE poll spins on;
(2) CMSYSPAR / N100IDENT / FN5DEST values + SYSPAR offsets, and GIVEINT's FIFOB.

**Evidence class: [NPL-V]** = verified in the SINTRAN NPL source (`SINTRAN/NPL-SOURCE/NPL/`,
s3vs-4 build) plus the per-version symbol tables. This is NOT byte-verified against the carved
L07/M06 binary; NPL is a different revision. The X5* offsets below are identical in the L07 and
M06 symbol tables, and the ND-5000 symbols are from `SYMBOLS/M06/N5000-SYMBOLS.SYMB.TXT` (the
only version with an N5000 set). [I] = inferred, stated as such.

---

## 1. THE HEADLINE ANSWER

**Your listing analysis is consistent with the NPL: on ND-5000 there is NO per-message "head
cell" / MAR write at all.** The classic-500 directed activation (IOX LMAR5 message-address +
LCON5 5) is bypassed: `XACT500` begins with `GO XACTRDY  % Continue in XACTRDY if nd5000`
(NNJ14 patch, MP-P2-N500.NPL:3059, addr 145551). The message address travels ONLY through:

1. the software **execution queue** (LINK chain, head at the per-CPU MAILINK block; DUMMESS
   head in multi-CPU) - inserted by `ITO500XQ` (CC-P2-N500.NPL:232, addr 022547); and
2. the **X5FIF ring** in shared memory - inserted by `ITOFIFOQ` (XC-P2-N500.NPL:76,
   addr 030366, `%new code` NNJ02 patch: direct exit if old 500).

**The halfword work flag is `X5ACT` = word offset 5 of the per-CPU extension block** (the block
MAILINK points to, size `5EXTD = 200B` words each, in the mailbox bank `5MBBANK`).

- Init: `XMSINIT` writes **-1** to X5ACT (and -1 to X5PRO "ND-500 IDLE") - RP-P2-N500.NPL:754-756
  (addr 131210-131216).
- The activation write: `XACTRDY`/`ACT51`: `T:=5MBBANK; X:=MAILINK; *AAX X5ACT; STZTX` -
  **stores 0** - MP-P2-N500.NPL:3027 (addr 145500).
- **These are the ONLY two ND-100 writes to X5ACT in the entire NPL tree.** SINTRAN never
  writes it back to -1: [I] the ND-5000 side (your microcode) re-arms it when it takes the work.
- So the protocol is: **-1 = nothing pending; 0 = ex-queue/FIFO has work - go look.**

**Why your carve didn't surface it near ITOFIFOQ/XKICK500:** the write is NOT at the insert
site. Insert sites do `ITO500XQ; ITOFIFOQ; SUNLOCK` and only LATER call `XACTRDY` (or `XACT500`
-> `GO XACTRDY`), and the X5ACT store is inside XACTRDY at label ACT51. The decision there
(MP-P2-N500.NPL:3007-3037):

```
GETC5PROC (reads MAILINK+X5PRO, cache-bypassed)     % current process on that CPU
  = -1 (CPU idle)          -> ACT51: X5ACT := 0     % IDLE poll picks it up, no kick needed
  = process, lower prio    -> ACT51: X5ACT := 0     % picked up when CPU next idles
  = process, higher-prio incoming (or A<0 special)
                           -> ACT52: N100KICK; CALL XKICK500   % octobus kick = preempt
```

So the octobus kick is only the PREEMPT path; the idle-wakeup path is purely the X5ACT
halfword your poll spins on. Supporting evidence that these cells are genuinely
microcode-written: `GETC5PROC` reads X5PRO twice with `*BSET BCM 120 DX; LDATX` -
"% Fool the cache" (CC-P2-N500.NPL:658-661) - SINTRAN distrusts its own cache because the
other side writes the cell.

**Cross-check for you:** if your `srf[0o2017]` pointer target = (per-CPU extension block
base) + 5 halfwords, the identification is closed. The block base chain is handed over at
init (section 3); block stride is 200B (128 dec) words.

## 2. THE TWO SHARED STRUCTURES (offsets, all octal words)

**Warning first:** the flat 5-char symbol tables collide here. `X5FYL=4 / X5MXF=5 / X5FIF=6`
are offsets in the GLOBAL mailbox header (X500DF area); `X5CPU=4 / X5ACT=5 / X5PRO=6` are
offsets in the PER-CPU extension block (MAILINK). Same small numbers, different bases. Do not
mix them.

### 2a. Global mailbox header (X500DF area) - the ND-100 -> ND-5000 FIFO [NPL-V]

| Offset | Cell | Meaning |
|---|---|---|
| 0 | X5SEM | test-and-set semaphore guarding the queue ops (SLOCK/SUNLOCK) |
| 3 | X5HEN | ring head index ([I] consumer = microcode) |
| 4 | X5FYL | ring fill index (producer = ND-100; post-increment) |
| 5 | X5MXF | ring size (wrap: `if fill+1 >= X5MXF then 0`) |
| 6-7 | X5FIF | **32-bit ring BASE POINTER** (double), not the ring itself |

`ITOFIFOQ` (XC-P2-N500.NPL:80-93): reads X5MXF, loads X5FYL, computes `slot = oldfill`,
stores `fill+1 mod X5MXF` back, loads the 32-bit base from X5FIF, `CNVBYADR`, target =
base + oldfill*2, then converts the message word-address with `CNVWADR` and `STDTX` -
**each slot = one 32-bit converted message pointer** (2 halfwords, hence the `SH 1`).

### 2b. Per-CPU extension block (MAILINK, stride 5EXTD=200B) [NPL-V]

| Offset | Cell | Init (XMSINIT) | Writers seen in NPL |
|---|---|---|---|
| 0-1 | X5BEX | -1,-1 (double) | init only |
| 2-3 | X5NAC | ND-500 addr of NEXT CPU's block (chain; head stored at X500DF+X5BAC=22B) | init only |
| 4 | X5CPU | - | MPFAIL written on power-fail (XRSTARTALL 145723); read `=MPACTIVE` checks |
| **5** | **X5ACT** | **-1** | **ACT51 := 0 (the work flag - section 1)** |
| 6 | X5PRO | -1 (= ND-500 IDLE) | [I] microcode writes current process; SINTRAN only reads (cache-bypassed) |
| 7 | X5STA | 5STATION | init only |
| 10 | X5CLR | - | SWPCLRMASK / 77B written, then CLRKICK (MP-P2-N500 1230/3768) |
| 11 | X5CCL | - | cache-clear counter (read/compared) |
| 20-21 | X5ACC | ACCP buffer base (double) | init only |
| 22-23 | X5OCT | octobus buffer base (double) | init only |
| 24-25 | X5HWB | HW buffer base (double) | init only |

## 3. How the microcode learns these addresses [NPL-V structure, content OPEN]

- Mailbox bank = `5FPMAILBOX SH 12` -> `5MBBANK`; area cleared; header at the base; per-CPU
  blocks follow at +5EXTDFSIZE steps; block chain linked via X5NAC (RP-P2-N500.NPL:736-771).
- Boot handshake: `CON5IDENT`/`MFPREPARE` send the CMSYSPAR multibyte to the ACCP
  (MP-P2-N500.NPL:3586-3634); the microprogram then fetches **micro-command 1 = get system
  parameters, 3 words via AOB** (manual ch. 5.3.7; your SYS_READ 017111 does 3x ACCP_READ).
- **OPEN: the CONTENT of those 3 words is not proven here.** If your listing shows SYS_READ's
  3 words being used to compute the block/flag addresses into srf (including srf[0o2017]),
  that closes the loop from your side.

## 4. Question 2: the constants [NPL-V from M06 symbol tables]

| Symbol | Value (octal) | Meaning / use |
|---|---|---|
| CMSYSPAR (CMSYS) | **016** | CM* code "system parameters"; sent as `MCOMMAND = CMSYSPAR<<8 \| N100IDENT` = 007001B (= 0x0E01) |
| N100IDENT (N100I) | **1** | ND-100's octobus ident (low byte of MCOMMAND; also `ECONID` connect at LV12B) |
| FN5DEST (FN5DE) | **070** | first ND-5000 destination station; `5STATION = CPUNO + FN5DEST - 1` (RP-P2-N500.NPL:976) |
| LN5DEST (LN5DE) | **073** | last ND-5000 destination station (so up to 4 CPUs, stations 070-073) |
| SYSPAR (SYSPA) | **111** | offset in the N500DF datafield of the 16-word system-parameter block; MON60 fn 103 (IRSYSP) reads it, fn 104 (IWSYSP) writes it (5P-P2-MON60.NPL:1570-1579) |

MFPREPARE message: MMSGLENGTH=3, MDP1 = 5OMDNO<<8 (tells MF-controller which OMD the ND-100
receives on). CON5IDENT message: MMSGLENGTH=7, S5 = 5OMDNO<<8, S6=S7=0, to OMDACCP.
**OPEN:** internal layout of the 16-word SYSPAR block - the NPL only shows the block copy.

## 5. GIVEINT / FIFOB [partial]

- GIVEINT is microcode-side. Catalogued: **GIVEINT1 @025441** emits the single-word
  `(SC10 & 037400 IX/8) | 100001` ("give interrupt", from MSG_QUEUE_END) -
  `ND5800-MICROCODE-ACCP-OCTOBUS-CATALOG.md` section 5.
- On the SINTRAN side that lands as the octobus input-controller ident: **idents 40B/41B on
  level 13** (verified LIVE earlier - see `SINTRAN/ND5000/` docs); the driver branch drains
  the answer FIFO under the X5SEM semaphore.
- **FIFOB itself: I don't know.** The name does not exist anywhere in the NPL tree or the
  SINTRAN-side docs. It is a label in YOUR listing; if it is the queue GIVEINT drains before
  raising the interrupt, the SINTRAN-visible counterpart would be the answer direction of the
  shared-memory FIFO machinery, but I will not assert that without seeing the listing context.

## 6. Evidence index

| Claim | Where |
|---|---|
| ND-5000 skips MAR write, GO XACTRDY | MP-P2-N500.NPL:3059 (145551, NNJ14) |
| ITOFIFOQ ring insert mechanics | XC-P2-N500.NPL:76-93 (030366-030436, NNJ02) |
| ITO500XQ ex-queue insert + 5IEXQUEUE flag | CC-P2-N500.NPL:232-238 (022547+) |
| ACT51 X5ACT:=0 / ACT52 kick decision | MP-P2-N500.NPL:3007-3037 (145413-145524) |
| X5ACT/X5PRO init -1 "ND-500 IDLE" | RP-P2-N500.NPL:752-756 (131200-131216) |
| GETC5PROC cache-bypassed X5PRO read | CC-P2-N500.NPL:658-661 (023630-023642) |
| Insert sites: ITO500XQ+ITOFIFOQ+SUNLOCK, XACTRDY later | MP-P2-N500.NPL:2599-2604, 2774-2779, 2825-2830; callers of XACTRDY at 244, 309, 458, 505, 534, 624, 962, 994 |
| Kick call sites (N100KICK/CLRKICK/IDLEKICK) | MP-P2-N500.NPL:1231, 2950, 3032, 3769 |
| Symbol values | SYMBOLS/M06/N5000-SYMBOLS.SYMB.TXT + SYMBOL-1-LIST.SYMB.TXT (X5* identical in L07) |
| Micro-command 1 = 3 words via AOB | OCTOBUS-ND100-ND5000-REFERENCE.md section 7.2 (manual ch. 5.3.7) |

**OPEN items:** FIFOB identity; byte-verification of the NPL claims against the M06 carve.

**RESOLVED 2026-07-17: X500D=177745 is a NEGATIVE FIELD OFFSET** (-33B) into the N500DF
datafield (N500D=051767B in L07 SYMBOL-2), so the X500DF mailbox-base pointer cell sits at
051767B - 33B = **051734B** absolute (L07; recompute from N500D for other versions). XMSINIT
writes the mailbox base (word address within 5MBBANK) there; ITOFIFOQ's
`X:="N500DF".X500DF` reads it. [NPL-V arithmetic; cell content is runtime-written, so not
statically byte-checkable.]

**RESOLVED 2026-07-17 (see `CARVE-ANSWER-SYSPAR-LSYSPAR-DISAMBIGUATION.md`):** the 3 SYS_READ
words = the CON5IDENT CMSYSPAR payload S5/S6/S7 = (5OMDNO<<8, 0, 0) - NOT the N500DF+111B
block; word 1 reproduces O1's live 100401B with 5OMDNO=10B. The 16-word SYSPAR block is the
ND-500 Monitor SET-SYSTEM-PARAMETERS tunables (ND-60.136), unrelated to the handshake.
