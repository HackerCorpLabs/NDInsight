# CPU TYPE: three different things with the same name

**The question this answers**: when you install SINTRAN and run `NEW-SYSTEM`, it
asks for a **CPU NUMBER** and a **CPU TYPE**. What is the CPU TYPE, and does it
affect anything?

**Short answer**: **we did not find a use for it.** Nothing in the kernel reads
it, and it is not consumed at boot either - on an ND-110/120, boot *writes* it.
It is an identity code from ND's order form, stored so it can be displayed and
exported. Answering it or pressing RETURN makes no difference to how the system
runs.

**It is not "boot information".** The only ways the value leaves the kernel are
the `@LIST-TITLE` banner and MON 262, which copies the whole system-info table to
any program that asks. So if anything uses it at all, that something is **outside
the kernel** - a utility or a layered product. See section 5.

The two *other* things called "CPU type" are both detected automatically and are
heavily used.

Investigated 2026-08-28. Companion to
[03-CPU-DETECTION-AND-INITIALIZATION.md](03-CPU-DETECTION-AND-INITIALIZATION.md),
which covers `SYSEVAL`, `GCPUNR` and the full `HWINFO` layout.

---

## 1. The three meanings, side by side

| | Where it comes from | Values | Read by the OS? |
|---|---|---|---|
| **`HWINFO(0)`** high byte | **Probed at every boot** - `VERSN` plus deliberately executing illegal instructions and catching the level-14 trap | 0-7 (NORD-10 / ND-100 / ND-110 / ND-120, x 48- or 32-bit float) | **Constantly** - 12 branch sites |
| **`HWINFO(2)`** | **Typed at `NEW-SYSTEM`**, or from the backplane PROM on ND-110/120 | 100, 102, 500, 502, 503, 5561 ... | **Never** |
| **`5CPUTYPE`** | **Probed from the ND-100 bus** - which coprocessor interface answers | `OLD500`=1, `SAMSON`=3 | **Yes** - selects the transport |

Only the middle one is ever typed by a human, and it is the only one nothing
depends on.

---

## 2. `HWINFO(2)` - the one `NEW-SYSTEM` asks for

### 2.1 The prompt

From `(SYSTEM)NEW-SYSTEM:PROG`, strings carved from `VSXL3.IMG`
(`tools/boot-floppy/INSTALL-PROCEDURE.md`) **[VERIFIED]**:

```
> Give CPU number (in Decimal):
> Give CPU type (in Decimal):
```

The `NEW-SYSTEM` sub-command that does it is **`CPU-UPDATE`** - "update CPU number
and CPU type" (L-version release information). On older media the job had its own
program, `CPU-TYPE:PROG` (H-version floppies, 1984).

**It is optional.** The System Supervisor manual walks through an installation on
two named example machines - **SAMBA**, an ND-570/CX running VSX, and **MAMBA**, an
ND-100 running VSE (`ND-30.003.007`, line 4827). In the SAMBA walkthrough the
installer presses RETURN at the CPU type prompt, and the manual says:

> "If ND has given you a CPU type (see your SINTRAN order), then you must give
> this as input here! The CR given above means that no CPU type was assigned for
> SAMBA!"

So a machine could be shipped with **no CPU type assigned at all** - which is
itself evidence about how much the field mattered.

The CPU **number** prompt is not optional in the same dialogue; the manual marks
the entered value as "the correct CPU number for SAMBA".

### 2.2 The whole life of the value

```
  operator types a number
        |
        v
  33CPU              generation symbol, beside 33CPN for the number
        |
        v
  HWINF+2/ 33CPU     assignment in START-PATCH-FILE:MODE
        |
        v
  HWINFO(2)          word 3 of the system-info array, address 004054B
        |
        +--> @LIST-TITLE prints "CPU TYPE:  <n>"
        +--> MON 262 GetSystemInfo copies the whole 12-word table out
```

From `NPL-SOURCE-2/BOUT-6.SYMB`, verbatim:

```
"-33CPN; 33CPN:005551    % CPU NUMBER
"-33CPU; 33CPU:000146    % CPU TYPE
```

`000146B` = 102 decimal, an ND-100 dual-CPU system type. The generation program
clears both so the installer is asked - `NPL-SOURCE/NPL/0.SIN-GEN.NPL`:

```
% STANDARD SYSTEM, FORCE NEW-SYSTEM TO ASK FOR CPU-NUMBER
)KILL 33CPU 33CPN
33CPU=0; 33CPN=0
```

### 2.3 On an ND-110 or ND-120, the PROM overwrites it

`SUBR GCPUNR` in `PH-P2-OPPSTART.NPL` - *"ROUTINE TO GET CPU NUMBER FROM BACK
WIRING PROM. CALLED ONLY IF 110/120 CPU"* - reads **three** values from the
backplane PROM, not one:

```npl
IF INF3><52652 THEN EXIT FI            % wrong PROM, give up (52652B = 0x55AA)
IF INF0><-1 THEN
   A=:SYSNO=:FCPUN; 1=:PRFLAG          % CPU NUMBER (PRFLAG is used by NEW-SYSTEM)
FI
IF INF1><-1 THEN A=:HWINFO(2) FI       % CPU TYPE
IF INF2 SHZ -10><377 THEN A=:NLEGU FI  % NUMBER OF LEGAL USERS
```

So on a 110/120 with a programmed PROM, whatever was typed at `NEW-SYSTEM` is
**replaced at every start-up**. Note the third item: the same PROM carries the
number of legal users.

### 2.4 Three separate proofs that nothing uses it

1. **One write, zero reads.** Across roughly 70 NPL source files in two SINTRAN
   versions (`NPL-SOURCE` for L/M, `NPL-SOURCE-2` for J), `HWINFO(2)` appears
   exactly twice: the descriptive comment, and the single store in `GCPUNR`. No
   comparison, no branch, no table indexed by it.

2. **The J-version source says so.** `NPL-SOURCE-2/NPL/23-WINCHESTER-POF.NPL`:

   ```
   %   2    HWINFO(1)       NOT USED
   %   3    HWINFO(2)       NOT USED
   ```

3. ~~**A real machine runs with nonsense in it.**~~ **WITHDRAWN 2026-08-28 - this
   was my error.** `SINTRAN-STRUCTURES.md` records a live image with
   `HWINFO(2) = 023233B` = 9883 decimal, and I called that "not a valid system
   type". It is valid: **9883 = ND-110 Satellite model T9**, per the value table in
   ND-830053.3 (section 2.6 below). The field was correctly set and the machine was
   a Satellite T9. This is not evidence of anything.

   Proofs 1 and 2 stand on their own and are unaffected.

Its only consumers are display and export. The carved analysis of MON 262
(`tools/sintran-segment-carver/.../262B-GetSystemInfo/`) shows the worker `CPUST`
is a range check plus two `MOVEW` block copies - it inspects no individual field.

### 2.6 What the values mean - the authoritative table

**ND-830053.3 EN, "SINTRAN III How to order it", November 1988**, section 1
CUSTOMER INFORMATION, defines the field. Verbatim:

> **- CPU type:**
> This is the CPU type of the system.
> You will find the CPU type stated on your order confirmation.
> [...]
> **The symbol mark generated is 33CPU.**

> **- CPU no.:**
> The system number must be filled in. It will identify the SINTRAN III later on
> when a new SINTRAN III needs to be installed.
> **The symbol mark generated is 33CPN.**

So CPU TYPE is a **hardware model code**, taken from the order confirmation, and
its only stated product is the generation symbol `33CPU`.

The "some common values" table from that manual (digit noise from OCR marked `?`):

| Code | Machine |
|---|---|
| 3080 | ND-110 Compact model A0 |
| 3095 | ND-110/CX Compact model B |
| 3740 | ND-110/CX system |
| 5230 | ND-5200 ES model L |
| 5700 | ND-5700 ES model L |
| 5800 | ND-5800 ES model L |
| 5904 | ND-5900 ES model L4, four CPUs |
| 9883 | ND-110 Satellite model T9 |

*A representative handful. The **full catalog of 98 codes** - with configurations,
where each is documented, and how many machines carry it - is
[../../History/machines/TYPES.md](../../History/machines/TYPES.md). It is kept in
one place on purpose; this document does not duplicate it.*

**These are the same codes as the product numbers** catalogued in
`History/machines/TYPES.md` and the type column of `History/machines/MACHINE-LIST.md`.
The `/SCSI` pairs and the Compact/System split explain several codes that catalog
could not resolve.

**Two things this settles:**

- The machines **COMSON-A** and **COMSON-B** carry type `5795` - so they were
  **ND-5700 Compact model B1** machines.
- `3740`, against NODEA, NODEB and INTERCOM on the machine list, is an **ND-110/CX system**.

**And one negative worth recording**: in all 76 pages of the order manual, CPU type
and CPU number appear **only in the customer-information header**. They are *not*
among the SYSTEM PARAMETERS (RT-PROG, SEGM, SEMAPHORE, INTERNAL DEVICE, DEVICE
BUFFER and so on) that actually size the generated system. Nothing in the manual
says CPU TYPE changes any generated code.

### 2.6b A note on @LIST-TITLE

The output changed between versions. On **SINTRAN III/VSX** it prints a full
banner, verbatim from `Installation/Installation-Description/ND-211297-1-EN.md`:

```
@LIST-TITLE

SINTRAN III - VSX/500 K HANS
STANDARD CONFIGURATION:       E
BETA TEST (WORK MODE NO.):    312B
REVISION (PATCH FILE NO.):    8000B
CPU TYPE:                     100
CPU NUMBER:                   1644
GENERATED:                    13.14.00    15 SEPTEMBER 1987
```

On the older **NORD-10/S** documentation (ND-60.128.01) the whole output is a
single identification string, `NORD-10/S - 781012`, with no separate CPU type
line. So the CPU type became visible to the operator somewhere between the two.

### 2.7 The CPU number itself encodes the type

From the archive's own column legend:

> "CPU no - The CPU number of a given ND computer. Usually **tttt.nnnnn** where
> tttt is the computer type or model, and nnnnn is the actual CPU number."

So a full ND system number carries the model as a prefix - which is why the
SINTRAN order-form archive is filed in `100`, `500`, `900` and `xxx` sets by that
prefix.

### 2.5 Observed real values

| Value | Machine | Source |
|---|---|---|
| 100 | "HANS", VSX/500 K | `Installation/Installation-Description/ND-211297-1-EN.md` |
| 102 | generation symbol default | `BOUT-6.SYMB` |
| 503 | "SNORRE", ND-570/CX | `Reference-Manuals/SINTRAN III Haandbok for driftsansvarlig.md` |

These are **system-type codes**, the same family of numbers as the type column in
`History/machines/MACHINE-LIST.md` and the catalog in `History/machines/TYPES.md`.

---

## 3. `HWINFO(0)` - the CPU type the kernel actually uses

Never asked for. `SUBR SYSEVAL` works it out at every start-up by **probing**:

- `T:=0; A:=1; *NLZ 20` - 32- versus 48-bit floating point
- `CPSTA/\10000 SHZ -13+T SH 10` - NORD-10 versus ND-100, from the status register
- illegal-instruction traps on level 14 (`CLEV14`) around `140130` (BFILL),
  `142700` (GECO), `143500` (SLWCS), `ICLEP`, `WGLOB` - fingerprinting the
  instruction set by seeing which instructions fail
- `*VERSN` - microprogram version into `HWINFO(1)`, and
  `IF T BIT 17 THEN A+1000=:CHWINFO FI  % ND-120?`

This one is load-bearing. `PH-P2-OPPSTART.NPL:314`:

```npl
IF HWINFO(0)/\377 < 2 THEN CALL ERRFATAL FI  % MUST BE ND-100/CX, ND-110/CX OR ND-120/CX
```

plus branches at lines 313, 315, 1197, 1516, 2591, 2613, in `PH-P2-RESTART.NPL`
(631, 647, 653, 1041), `12-EX-MRES-SINA.NPL:1386` and `14-MRES-SEGADM.NPL:926`.
There is also a bit test for a Rask CPU in `CC-P2-N500.NPL:714`:

```npl
1HWINF; *RASK@3 BLDA DA   % K-bit set if Rask-cpu     (RASK=000012)
```

Full value tables are in
[03-CPU-DETECTION-AND-INITIALIZATION.md](03-CPU-DETECTION-AND-INITIALIZATION.md)
section 6.2.

---

## 4. `5CPUTYPE` - detected from which coprocessor interface answers the bus

A third, unrelated field, in the ND-500 CPU datafield. **It is not a processor
identity - it is the identity of the interface found on the ND-100 bus**, from
which the coprocessor generation follows.

`SUBR CH5CPUPRESENT`, `PH-P2-OPPSTART.NPL` lines 3893-3943:

```npl
T:=HDEV+RSTA5; *TRA IIC             % arm IOX error handling
A:=200; *TRR IIE; IOXT; TRA IIC     % read the DMA status register
IF A=0 THEN                         % no IOX error = the 3022 answered
   CPUAVAILABLE/\140000\/OLD500     % set CPU type to OLD500
   A BONE 5ALIVE
   MIN COLD
ELSE
   ...
   T:=100406; *IOXT; TRA IIC        % read Octobus status at 100406
   IF A=0 THEN                      % no IOX error = octobus answered
      ...
      CPUAVAILABLE/\140000\/SAMSON  % set CPU type to SAMSON
```

So:

| Probe | Answers? | Result |
|---|---|---|
| DMA status register at `HDEV+RSTA5` - the **PCB 3022** bus interface | yes | **`OLD500`** |
| Octobus status at IOX **`100406`** | yes | **`SAMSON`** |
| neither | - | not present |

`RSTA5` is offset +2 in the 3022 register map, "Read Status Register" - so the
first probe is literally the 3022 board answering.

**Notes that matter when reading this:**

- It names the **interface**, not the processor. An ND-500/1 and an ND-500/2 both
  read as `OLD500`. Samson and Rallar both sit on octobus, so **Rallar also reads
  as `SAMSON`**.
- The routine loops over up to four CPU datafields (`S5CPUDF` to `E5CPUDF`) with
  two short-circuits: if a SAMSON was already found the DMA probe is skipped, and
  if DMA was found first the octobus probe is skipped. A system is assumed to be
  all one kind.
- Unlike `HWINFO(2)`, this **is** read later - `MP-P2-N500.NPL:265`:

  ```npl
  IF CPUAVAILABLE/\5CPUTYPE><SAMSON THEN   % DMA interface?
  ```

  plus around nine more sites in `MP-P2-N500.NPL`, `RP-P2-N500.NPL`,
  `PH-P2-RESTART.NPL` (100, 112) and `MP-P2-PERF-SAMP.NPL` (370, 488).

The same detect-by-probing philosophy appears three times over: illegal
instructions for the CPU, IOX errors for the coprocessor bus, and a magic word
(`52652B`) for the backplane PROM. **ND's way of identifying hardware is to try it
and see what fails.**

---

## 5. So what IS it for? - the honest position

**No purpose was found.** That is the finding, and it should not be dressed up.

What can be said positively:

| Question | Answer |
|---|---|
| Is it read during boot? | **No.** On ND-110/120 boot *writes* it from the PROM |
| Is it read by the kernel at all? | **No.** Zero read sites in ~70 NPL files, two versions |
| Is it needed for the system to run? | **No.** A live image has an invalid value and runs fine |
| Can anything read it? | **Yes** - `@LIST-TITLE` prints it, MON 262 exports it to any program |
| Do we know of anything that does? | **No** |

Because MON 262 hands the whole table to user programs, **any consumer would be
outside the kernel** - and the kernel is the part we hold as source. That is the
one honest gap.

**What the defining manual says it is for**: identifying the system. The CPU
number entry says it "will identify the SINTRAN III later on when a new SINTRAN III
needs to be installed" - i.e. these fields exist so ND can tell which machine a
given SINTRAN belongs to. That is a **records purpose, not a runtime one**, and it
fits everything observed.

**Candidates for a runtime use, all unverified, listed so nobody has to re-guess them:**

- **Licensing.** The strongest hunch, and still only a hunch. The **number of legal
  users** sits in the same backplane PROM immediately beside the CPU type, and the
  value itself comes off ND's order form - the same paperwork that records what
  was purchased. A licence check asking "which model is this?" via MON 262 would
  fit the shape. **No such check has been found.**
- **Service and inventory.** The value matches the confirmed order and is printed
  by `@LIST-TITLE`, which is what a field engineer would read out. This needs no
  code at all, and would explain why nothing consumes it.
- **A layered product** - SIBAS, COSMOS, NOTIS - gating a feature on system type.

**How to settle it**: search the carved binaries of `NEW-SYSTEM:PROG`,
`S3-CONFIG` and the layered products for a MON 262 call that then tests bytes 6:7
of the returned table. Nothing else will close it.

## 6. Practical answer

- **Answer the CPU TYPE prompt if ND gave you one on the order form; otherwise
  press RETURN.** Nothing in the kernel behaves differently either way.
- **The CPU NUMBER is different** - it is load-bearing. `MP-P2-1.NPL:232` branches
  on it (`IF A=SYSNO THEN AREG; EXITA FI  % LOCAL CONNECTIONS ALWAYS OK`), so it
  matters for COSMOS and XMSG routing.
- On an **ND-110 or ND-120** the backplane PROM supplies the CPU number, the CPU
  type and the number of legal users, and overrides what was typed.

## 7. What is still unverified

- **The kernel is what we hold in NPL.** `NEW-SYSTEM:PROG`, `S3-CONFIG` and
  layered products - SIBAS, COSMOS, licence checks - exist here only as carved
  binaries. A *utility* could still read `HWINFO(2)`; nothing in the kernel does.
- **Licensing is the one plausible remaining consumer.** The number of legal users
  sits beside the CPU type in the same backplane PROM, which makes a licence check
  a reasonable place to look next.
- No document found anywhere states an enumeration of legal CPU TYPE values. The
  list `(100, 102, 500, 502, 5561..)` is a comment in the source, ending in "..".

## 8. Sources

**Primary ND documents (in this repo):**

- `Operations/SINTRAN/ND-30.003.007 EN SINTRAN III System Supervisor.md` - the install dialogue
- `Reference-Manuals/ND-820023-1-EN SINTRAN III-VSX System Documentation.md` - the system-info array
- `Reference-Manuals/ND-860228-2-EN SINTRAN III Monitor Calls.md` - MON 262 GetSystemInfo
- `SINTRAN/Release-Documentation/` - K, L, M and N release information, `CPU-UPDATE`
- `Reference-Manuals/SINTRAN III Haandbok for driftsansvarlig.md` - the Norwegian twin

**Source code:**

- `SINTRAN/NPL-SOURCE/NPL/PH-P2-OPPSTART.NPL` - `SYSEVAL`, `GCPUNR`, `CH5CPUPRESENT`
- `SINTRAN/NPL-SOURCE/NPL/0.SIN-GEN.NPL` - the generation blanking
- `SINTRAN/NPL-SOURCE-2/BOUT-6.SYMB` - `33CPU` and `33CPN`
- `SINTRAN/NPL-SOURCE-2/NPL/23-WINCHESTER-POF.NPL` - the "NOT USED" annotation

**Carved and derived:**

- `tools/boot-floppy/INSTALL-PROCEDURE.md` - prompt strings from the binary
- `tools/boot-floppy/patches/README.md` - `HWINF+2/ 33CPU`
- `SINTRAN/ND500/ND500-ND5000-INTERFACE-COMPREHENSIVE-GUIDE.md` - the detection flow
- `SINTRAN/SINTRAN Structures/SINTRAN-STRUCTURES.md` - the live image with 023233B

---

*See also: [03-CPU-DETECTION-AND-INITIALIZATION.md](03-CPU-DETECTION-AND-INITIALIZATION.md)
for the full HWINFO layout and detection flowcharts, and
[../../History/machines/TYPES.md](../../History/machines/TYPES.md) for what the
system-type numbers mean as product codes.*
