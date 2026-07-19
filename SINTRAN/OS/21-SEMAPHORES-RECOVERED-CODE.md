# 21A - Semaphore Kernel Routines: Recovered Code (SINTRAN L07)

**Companion to**: [21-SEMAPHORES-EXPLAINED.md](21-SEMAPHORES-EXPLAINED.md)

The RT-monitor reservation primitives `BRESERVE`/`BRELEASE` and the `RESRV`/`RELES` monitor-call handlers are missing from the NPL source subset in this repository (call sites only — see the completeness note in chapter 21). This document contains their **actual machine code, recovered from a running SINTRAN III VSX/500 L system** and verified live.

## Provenance

| Item | Value |
|---|---|
| Method | nd100x emulator, SMD boot, DAP debugger (memory read + disassembly + breakpoints + single-step) |
| System | SINTRAN III - VSX/500 **L**, boot banner "GENERATED: 09.34.00 16 DECEMBER 1988", CPU TYPE 9883 |
| Disk image | `SMD0.IMG` (nd100x repository, ND-110 emulation) |
| Symbol tables | [../NPL-SOURCE/SYMBOLS/L07/SYMBOL-1-LIST.SYMB.TXT](../NPL-SOURCE/SYMBOLS/L07/SYMBOL-1-LIST.SYMB.TXT): `BRESE=010563`, `BRELE=010610`, `TOEXQ=010744`, `TOWQU=010760`, `FREXQ=011016`, `PRSRV=037076`, `RESRV=037103`, `PRLS=037147`, `RELES=037156` |
| Recovery date | 2026-07-09 |

**Address-space findings** (verified during the session):

- `BRESE`/`BRELE` (010xxx) are in the **resident kernel**, mapped 1:1 logical = physical (identical bytes read via the live page table and via `phys:`).
- `RESRV`/`RELES` (037xxx) are in a **paged monitor-segment window**: three different code contents were observed at 037103 at different times (mid-boot at level 14, post-boot idle, and at the live RESRV breakpoint hit). The listing below was captured **while the CPU was stopped at PC=037103**, i.e. with the correct segment mapped.
- At level 3 (monitor level), instruction and data page tables differ (PT vs APT): data structures must be read through the APT (`dspace:`), otherwise code is returned.

---

## 1. BRESERVE (BRESE = 010563) — fully verified

**Entry**: B = datafield address, X = RT-description of the requesting program.
**Exit**: A = 0 (reserved, or already owned by the requester), A = -1 (occupied by another program). Plain `EXIT` (return via L).

21 words, verified two ways: (a) disassembled from the running kernel, (b) **single-stepped live** while SINTRAN reserved the console terminal datafield (details in section 3).

```
Addr    Word    Instruction        Annotation
------  ------  -----------------  ------------------------------------------------
010563  044400  LDA ,B 0           A := DF.RESLI  (reservation-chain link, offset 0)
010564  131410  JAF *+10           RESLI <> 0 -> already reserved, go to 010574
                                   --- free path ---
010565  046020  LDA ,X 20          A := RTdesc.BRESLINK  (owner chain head, offset 20B)
010566  004400  STA ,B 0           DF.RESLI := old chain head
010567  146135  COPY SB DA         A := B  (datafield address)
010570  006020  STA ,X 20          RTdesc.BRESLINK := DF  (DF becomes new chain head)
010571  014401  STX ,B 1           DF.RTRES := X  (owner := requester)  ** RESERVED **
010572  146105  COPY 0 DA          A := 0  (success)
010573  124007  JMP *+7            -> exit at 010602
                                   --- occupied path ---
010574  050401  LDT ,B 1           T := DF.RTRES  (current owner)
010575  140067  SKP IF DX EQL ST   requester == owner ?
010576  124003  JMP *+3            no -> 010601
010577  146105  COPY 0 DA          yes: A := 0  ** recursive reserve is a no-op **
010600  124002  JMP *+2            -> exit at 010602
010601  170777  SAA -1             A := -1  (occupied by another program)
010602  146142  EXIT               return via L
010603-010607   (5 data words)     BRELEASE register save area (TAD, L, X)
```

This is the machine-code proof of two facts documented in chapter 21:

1. The `A < 0` = occupied convention seen at every NPL call site.
2. The **SINTRAN deviation from the Dijkstra semaphore**: re-reserving a semaphore you already own returns success (010574-010600) instead of deadlocking.

Note the era-typical trick: BRESERVE contains **no** save/restore — it is a pure leaf routine — while BRELEASE's register save area lives in the words *between* the two routines.

## 2. BRELEASE (BRELE = 010610) — code recovered, key mechanics verified

**Entry**: B = datafield, X = expected owner RT-description (X = 0 observed as kernel force-release path).
**Exit**: return via saved L.

```
Addr    Word    Instruction        Annotation
------  ------  -----------------  ------------------------------------------------
010610  014377  STX *-1            save X -> 010607
010611  030372  STF *-6            save T,A,D -> 010603..010605
010612  146145  COPY SL DA         A := L
010613  004373  STA *-5            save return address -> 010606
010614  051106  LDT I *+106        T := bound (literal)      \
010615  141463  SKP IF DB MGRE ST   |  range check: is B a    |
010616  124005  JMP *+5             |  file-system datafield? | special-release
010617  050104  LDT *+104           |                         | hook for a
010620  143463  SKP IF DB MLST ST   |                         | datafield class
010621  124002  JMP *+2             |                         |
010622  135102  JPL I *+102        in range: call hook        /
010623  044400  LDA ,B 0           A := DF.RESLI
010624  131073  JAZ *+73           RESLI = 0 -> not reserved, restore + exit
010625  044401  LDA ,B 1           A := DF.RTRES (owner)
010626  131402  JAF *+2            owner <> 0 -> ok
010627  135075  JPL I *+75         inconsistent (chained but ownerless): error call
010630  142057  SKP IF DX UEQ SA   X <> owner ?
010631  124003  JMP *+3            X == owner: proceed
010632  133002  JXZ *+2            X == 0 (force release): proceed
010633  135071  JPL I *+71         X is a third party: error call
                                   --- unlink DF from owner's reservation chain ---
010634  146157  COPY SA DX         X := owner RT-description
010635  173420  AAX 20             X := &RTdesc.BRESLINK
010636  135067  JPL I *+67         (helper via literal)
010637  046000  LDA ,X 0           walk:  A := (X)
010640  142035  SKP IF DA UEQ SB     until (X) == this DF
010641  124003  JMP *+3            found predecessor
010642  146157  COPY SA DX         X := next element
010643  124374  JMP *-4            loop
010644  044400  LDA ,B 0           A := DF.RESLI (successor)
010645  006000  STA ,X 0           predecessor.link := successor  (DF unlinked)
010646  000400  STZ ,B 0           DF.RESLI := 0
010647  000401  STZ ,B 1           DF.RTRES := 0        ** FREE **
                                   --- device-type dependent post-release ---
010650  044403  LDA ,B 3           A := DF type/flags word (offset 3)
010651  175375  BSKP ONE 170 DA    bit tests on the type word select
010652  124002  JMP *+2              driver/device hooks, called through
010653  000404  STZ ,B 4             the literal pool (JPL I *+44 / *+41)
010654  175255  BSKP ONE 50 DA       -- partially annotated; device-class
010655  124012  JMP *+12             specific (restart output, etc.)
010656  175025  BSKP ZRO 20 DA
010657  124010  JMP *+10
010660  050046  LDT *+46
010661  146137  COPY SB DX
010662  146105  COPY 0 DA
010663  135044  JPL I *+44
010664  050044  LDT *+44
010665  146105  COPY 0 DA
010666  135041  JPL I *+41
010667  045042  LDA I *+42
010670  131002  JAZ *+2
010671  135041  JPL I *+41
                                   --- wake the first waiter ---
010672  054402  LDX ,B 2           X := DF.BWLIN (first waiter RT-desc)
010673  142037  SKP IF DX UEQ SB   waiting queue empty (BWLIN == DF itself)?
010674  124023  JMP *+23           empty -> restore registers + exit
010675  046003  LDA ,X 3           waiter state word checks
010676  131412  JAF *+12             (filter which waiters are eligible;
010677  050034  LDT *+34              compared against literal-pool
010700  142067  SKP IF DX UEQ ST      constants - partially annotated)
010701  124007  JMP *+7
010702  050032  LDT *+32
010703  142067  SKP IF DX UEQ ST
010704  124004  JMP *+4
010705  050030  LDT *+30
010706  140067  SKP IF DX EQL ST
010707  124010  JMP *+10
010710  046013  LDA ,X 13          A := waiter.WLINK  (queue link, offset 13B)
010711  004402  STA ,B 2           DF.BWLIN := next waiter  (dequeue first)
010712  002013  STZ ,X 13          waiter.WLINK := 0
010713  135023  JPL I *+23         literal 010736 = 010563: ** CALL BRESERVE **
                                     reserve the DF for the waiter (ownership transfer)
010714  135023  JPL I *+23         literal 010737 = 010744: ** CALL TOEXQU **
                                     insert the waiter into the execution queue
010715  170401  SAA 1
010716  005022  STA I *+22         flag via literal (scheduling hint)
                                   --- restore and return ---
010717  054270  LDX *-110          X := saved X (010607)
010720  034263  LDF *-115          T,A,D := saved (010603..010605)
010721  125265  JMP I *-113        return via saved L (010606)
010722-010743   (literal pool)     includes 010736: 010563 (BRESE)
                                            010737: 010744 (TOEXQ)
```

The two `JPL I` targets at 010713/010714 resolve through the literal pool to **010563 (BRESERVE)** and **010744 (TOEXQU)** — the machine-code confirmation of ND-60062's description: *"RELEASE removes the first one from the waiting queue, reserves the resource for that program by calling BRESERVE and inserts the program into the execution queue by calling TOEXQU."*

## 3. Live Verification Log

Captured with an instruction breakpoint at 010563 while pressing ESC on the SINTRAN console (the command processor reserving the console terminal):

**At BRESERVE entry** (stop at PC=010563, level 3 = monitor level):

| Register | Value | Meaning |
|---|---|---|
| B | 042373 | terminal datafield to reserve |
| X | 023337 | requesting RT-description (background program) |
| L | 010460 | caller (resident kernel) |

**Datafield 042373 before** (read via APT/`dspace:`): `RESLI=0, RTRES=0, BWLIN=042373 (self = empty waiting queue), type=2` — the exact `(0, 0, self, 2)` initializer pattern of the semaphore datafield declarations in [../NPL-SOURCE/NPL/DP-P2-VARIABLES.NPL](../NPL-SOURCE/NPL/DP-P2-VARIABLES.NPL) (`INTEGER WEMSE:=(0,0,WEMSE,2)`).
**RT-description 023337 before**: `BRESLINK (offset 20B) = 023337` (self = empty reservation chain).

**Single-stepped through 010563-010571**: the `JAF` at 010564 fell through (free), and after the link sequence:

| Location | After | Meaning |
|---|---|---|
| DF.RESLI (042373+0) | 023337 | chain link -> back to owner (chain was empty) |
| DF.RTRES (042373+1) | 023337 | owner set |
| RTdesc.BRESLINK (023337+20B) | 042373 | chain head -> the datafield |

The circular reservation chain `RTdesc.BRESLINK -> DF.RESLI -> RTdesc` matches the documented walk idiom `DO WHILE X:=X.RESLINK><RTREF` exactly.

On a later (warm-start) session the CPU was caught mid-flight with **L = 010672** — a live return address inside BRELEASE's waiter-wake section — with A = 042373, the same terminal datafield.

## 4. RESRV / RELES Monitor Call Handlers (captured, annotation preliminary)

An instruction breakpoint at 037103 (RESRV) **hit within milliseconds of resuming** the booted system — SINTRAN calls RESRV constantly. At the stop (PC=037103, level 2, L=035537, B=146647, X=146602), the correct monitor segment was mapped and the region was captured. UNVERIFIED annotations below are hypotheses; the raw capture is authoritative.

```
Addr    Word    Instruction        Annotation
------  ------  -----------------  ------------------------------------------------
037076  003776  (word)             PRSRV entry area (UNVERIFIED: the 003776 word
                                   also appears at RELES 037156 - possibly a
                                   descriptor/continuation word, not an instruction)
037077  177400  ...
037100  050132  LDT *+132
037101  021042  STD I *+42
037102  146145  COPY SL DA         A := L
037103  146131  COPY SB DD         D := B          <- RESRV (breakpoint hit here)
037104  170010  SAB 10             B := 10B
037105  135037  JPL I *+37         call via literal 037144 (= 003752: resident
                                   routine - UNVERIFIED: parameter fetch)
037106  054037  LDX *+37           X := literal
037107  135037  JPL I *+37         call via literal 037146 (UNVERIFIED: logical
                                   device number -> datafield resolution)
037110  124031  JMP *+31           (= EXECC/OTRAN symbol address)
037111  004407  STA ,B 7
037112  070035  AND *+35
037113  146156  COPY SA DT
037114  054034  LDX *+34
037115  135034  JPL I *+34
037116  124023  JMP *+23
037117  004406  STA ,B 6
037120  054032  LDX *+32
037121  135032  JPL I *+32
037122  124017  JMP *+17
037123  050406  LDT ,B 6
037124  175306  BSKP ONE 100 DT
037125  124010  JMP *+10
037126  131002  JAZ *+2
037127  135025  JPL I *+25
037130  044407  LDA ,B 7
037131  070024  AND *+24
037132  156570  SHA ZIN SHR 10
037133  146156  COPY SA DT
037134  141216  RMPY SD DT
037135  020402  STD ,B 2
037136  040404  MIN ,B 4
037137  170770  SAA -10
037140  125016  JMP I *+16
037141  004402  STA ,B 2
037142  124375  JMP *-3
037143  147417  (NOOP/data)
037144  003752  (literal)
037145  000002  (literal)
037146  050124  (literal)
037147  000377  (word)             <- PRLS
037150  000013  ...
037151  050223  LDT *-155
037152  000010  ...
037153  050220  LDT *-160
037154  000215  ...
037155  007400  ...
037156  003776  (word)             <- RELES (same 003776 marker as PRSRV)
037157  021075  STD I *+75
037160  146145  COPY SL DA         A := L
037161  146131  COPY SB DD         D := B
037162  170023  SAB 23             B := 23B
037163  135072  JPL I *+72         call via literal (UNVERIFIED: parameter fetch)
037164  146165  COPY ST DA
037165  135071  JPL I *+71         call via literal
037166  146157  COPY SA DX
037167  004407  STA ,B 7
037170  140007  SKP IF DX EQL 0
037171  124007  JMP *+7
037172  140001  SKP IF DD EQL 0
037173  124003  JMP *+3
037174  170532  SAA 132            A := 132B (error code? UNVERIFIED)
037175  124003  JMP *+3
037176  170526  SAA 126            A := 126B (error code? UNVERIFIED)
037177  124053  JMP *+53
037200  046003  LDA ,X 3           datafield status word checks
037201  175165  BSKP ZRO 160 DA
037202  124003  JMP *+3
037203  170533  SAA 133
037204  124046  JMP *+46
037205  175375  BSKP ONE 170 DA
037206  124003  JMP *+3
037207  170526  SAA 126
037210  124042  JMP *+42
037211  175365  BSKP ONE 160 DA
037212  124005  JMP *+5
037213  046007  LDA ,X 7           <- WHERE symbol
037214  175305  BSKP ONE 100 DA
037215  124002  JMP *+2
037216  125041  JMP I *+41
037217  044402  LDA ,B 2
...     (continues; SEMES symbol at 037226)
```

The common prologue shape (`A := L; D := B; SAB n; JPL I <literal>`) and the error-code loads (`SAA 126B` / `SAA 132B` / `SAA 133B`) match monitor-call handler structure; full annotation (literal resolution, moncall parameter conventions) is follow-up work — see chapter 21's recovery-leads note.

## 5. Raw Dumps (authoritative)

Big-endian byte pairs per 16-bit word, as read from the running system.

**Resident kernel, physical = logical, 010563-010745 (BRESE through start of TOEXQ):**

```
phys 010563:  49 00 B3 08 4C 10 09 00 CC 5D 0C 10 19 01 CC 45
phys 010573:  A8 07 51 01 C0 37 A8 03 CC 45 A8 02 F1 FF CC 62
phys 010603:  00 01 3C B1 00 00 11 39 15 57 18 FF 30 FA CC 65   (save area = live values)
phys 010613:  08 FB 52 46 C3 33 A8 05 50 44 C7 33 A8 02 BA 42
phys 010623:  49 00 B2 3B 49 01 B3 02 BA 3D C4 2F A8 03 B6 02
phys 010633:  BA 39 CC 6F F7 10 BA 37 4C 00 C4 1D A8 03 CC 6F
phys 010643:  A8 FC 49 00 0C 00 01 00 01 01 49 03 FA FD A8 02
phys 010653:  01 04 FA AD A8 0A FA 15 A8 08 50 26 CC 5F CC 45
phys 010663:  BA 24 50 24 CC 45 BA 21 4A 22 B2 02 BA 21 59 02
phys 010673:  C4 1F A8 13 4C 03 B3 0A 50 1C C4 37 A8 07 50 1A
phys 010703:  C4 37 A8 04 50 18 C0 37 A8 08 4C 0B 09 02 04 0B
phys 010713:  BA 13 BA 13 F1 01 0A 12 58 B8 38 B3 AA B5 08 10
phys 010723:  32 21 00 8D 12 EE 00 2E 09 4D FF EC 08 F9 08 9D
phys 010733:  14 39 15 F1 14 4F 11 73 11 E4 08 06 15 57 45 FF   (11 73 = 010563 BRESE,
phys 010743:  13 F1 48 22                                        11 E4 = 010744 TOEXQ)
```

**Paged monitor segment, I-space at the RESRV breakpoint hit, 037066-037231:**

```
ispace 037066:  00 FF 00 0B 50 93 3D 49 39 6F 3B 3E B0 60 00 AC
ispace 037076:  07 FE FF 00 50 5A 22 22 CC 65 CC 59 F0 08 BA 1F
ispace 037106:  58 1F BA 1F A8 19 09 07 70 1D CC 6E 58 1C BA 1C
ispace 037116:  A8 13 09 06 58 1A BA 1A A8 0F 51 06 FA C6 A8 08
ispace 037126:  B2 02 BA 15 49 07 70 14 DD 78 CC 6E C2 8E 21 02
ispace 037136:  41 04 F1 F8 AA 0E 09 02 A8 FD CF 0F 07 EA 00 02
ispace 037146:  50 54 00 FF 00 0B 50 93 00 08 50 90 00 8D 0F 00
ispace 037156:  07 FE 22 3D CC 65 CC 59 F0 13 BA 3A CC 75 BA 39
ispace 037166:  CC 6F 09 07 C0 07 A8 07 C0 01 A8 03 F1 5A A8 03
ispace 037176:  F1 56 A8 2B 4C 03 FA 75 A8 03 F1 5B A8 26 FA FD
ispace 037206:  A8 03 F1 56 A8 22 FA F5 A8 05 4C 07 FA C5 A8 02
ispace 037216:  AA 21 49 02 F5 01 B2 09 54 0C F5 FF BA 1C DD 81
ispace 037226:  CC 81 CE 6D F5 FF 24 0F 2C 0F CD 01 CE 6D DD FF
ispace 037236:  DD F6 BA 12
```

Note: 037076 in this raw dump reads `07 FE` (003776) — matching the RELES word at 037156, supporting the descriptor-word hypothesis. The dump and the instruction-by-instruction disassembly in section 4 were taken moments apart at the same stop.

## 6. Session Lessons (for reproducing)

- `nd100x --debugger --boot=smd` from the nd100x repository boots SINTRAN L; connect DAP on port 4711, `debug_launch` with the image path, then let it run (~30 s to `SINTRAN III RUNNING`).
- **No page-table math was needed**: resident 010xxx addresses are identity-mapped (verify with a `phys:` read); for paged 037xxx addresses, set an instruction breakpoint at the symbol address and let the OS land there with the correct segment mapped.
- Read kernel *data* through the APT (`dspace:` prefix) — plain virtual reads at monitor level return instruction-space content.
- A DAP disconnect kills the emulator even with terminate=false; breakpoints could not be cleared via an empty set — plan breakpoint usage accordingly.

## References

- [21-SEMAPHORES-EXPLAINED.md](21-SEMAPHORES-EXPLAINED.md) — semantics, manuals, NPL call sites
- [../NPL-SOURCE/SYMBOLS/L07/SYMBOL-1-LIST.SYMB.TXT](../NPL-SOURCE/SYMBOLS/L07/SYMBOL-1-LIST.SYMB.TXT) — L07 symbol addresses
- [../SINTRAN Structures/SINTRAN-STRUCTURES.md](../SINTRAN%20Structures/SINTRAN-STRUCTURES.md) — RESLI/RTRES/BWLIN, WLINK/BRESLINK offsets (all confirmed live)
- [../../Operations/SINTRAN/ND-60062-01D-EN SINTRAN III System Documentation.md](../../Operations/SINTRAN/ND-60062-01D-EN%20SINTRAN%20III%20System%20Documentation.md) — BRESERVE/BRELEASE/TOEXQU behavior (confirmed by the literal pool at 010736-010737)
