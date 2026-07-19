# ND-500 bus / Octobus hardware interface (from NPL source)

Reverse-engineered from the SINTRAN III NPL driver source for the physical
ND-100 <-> ND-500 interface. Primary evidence: `../NPL-SOURCE/NPL/` files
`5P-P2-MON60`, `MP-P2-N500`, `RP-P2-N500`, `CC-P2-N500`, `XC-P2-N500`,
`MP-P2-PIOC-DRIV`, `RP-P2-PIOC`, and the symbol tables under
`../NPL-SOURCE/SYMBOLS/{K03,L07,M06}/N500-SYMBOLS.SYMB.TXT`. All addresses and
constants are octal (NPL convention). Register-offset symbols are identical
across K03/L07/M06; citations use M06 unless noted.

> Status: primary RE deliverable for the ND-500 bus / Octobus interface
> (RE-MASTER-PLAN Phase 5.1 / hardware-interface task). Facts below are quoted
> from source with file:line evidence; items marked UNCERTAIN need a follow-up
> read or bench confirmation.

There are **two physically distinct ND-100<->ND-500 interfaces** in this source:
- **A. DMA "process interface"** — a bank of IOX registers at base device number
  held in resident variable `HDEV`. Classic ND-500. Sections 1-5.
- **B. Octobus / Samson interface (ND-5000)** — a serial inter-processor ring at
  IOX base `100400`/`100404`. Section 6. The driver selects between them at
  runtime (`*NNJ14`, `SAMSON` CPU-type test).

---

## 1. I/O registers (DMA process interface)

`HDEV` is a resident kernel word holding the ND-500 interface's IOX device
number; every register is addressed as `HDEV+offset` and driven with `*IOXT`
(T = device number, A = data). Even offsets read, odd offsets write (standard
ND-100 I/O pairing).

`HDEV` variable address: `HDEV=177775`, `XHDEV=177774`
(`SYMBOLS/M06/SYMBOL-1-LIST.SYMB.TXT:5000-5001`). Runtime device-number check
`IF X.HDEV=660` at `RP-P2-N500.NPL:989` (660 octal is the observed device number;
UNCERTAIN whether fixed).

Register offsets (`SYMBOLS/M06/N500-SYMBOLS.SYMB.TXT`):

| Off | Read sym (line) | Write sym (line) | Meaning (from usage) |
|-----|-----------------|------------------|----------------------|
| 0 | `RMAR5=000000` (1230) | `LMAR5=000001` (7235) | Memory Address Register (DMA multiport addr) |
| 2 | `RSTA5=000002` (1229) | `LSTA5=000003` (7234) | Status register (read) / load-status (write) |
| 4 | `RCON5=000004` (1228) | `LCON5=000005` (7233), `LEVL5=000005` (7237) | Control register |
| 7 | - | `TERM5=000007` (4661) | Terminate ND-500 |
| 10 | `RUPP5=000010`, `RTAG5=000010` (1226-27) | `LUPP5=000011`, `LTAG5=000011` (7231-32) | Upper / tag word |
| 12 | `RLOW5=000012` (1225) | `LLOW5=000013`, `LDAT5=000013` (7229-30) | Lower / data word |
| 14 | - | `SLOC5=000014` (522) | Set interface LOCK |
| 16 | - | `UNLC5=000016` (3659) | UNLOCK interface |
| 17 | - | `RETG5=000017` (1224) | Re-enable / "re-tag" (tag-in) decoding |

### 1a. Status register bits (read `HDEV+RSTA5`)
Verbatim from `XC-P2-N500.NPL:41-45` (routine `CLE5STATUS`):
```
%              BIT  5PAGF=4 (000020) = Inclusive "or" of errors
%              BIT 5DMAER=6 (000100) = Communication error
%              BIT 5PFAIL=7 (000200) = Power fault executed by Microprog
%              BIT 5POWOF=8 (000400) = Latched power fault
%              BIT 5CLOST=9 (001000) = Microclock stopped
```
Activation-gating bits (`MP-P2-N500.NPL:3065-3066`, `RP-P2-N500.NPL:95`):
```
IF A NBIT 5CLOST THEN     % If nd-500 not stopped (clock stopped)
   IF A BIT 5ILOCK THEN   % If nd-500 not terminated
```
`5ILOC(K)=000005` (bit 5), `SYMBOLS/M06/N500-SYMBOLS.SYMB.TXT:778`. Status word:
errors-OR (b4), interface-lock/"running" (b5), DMA/comm error (b6),
power-fault-by-microprog (b7), latched power-off (b8), microclock-stopped (b9).

### 1b. Master clear / micro-stop sequence
`CC-P2-N500.NPL:212-217` (`5MCST`/`X5MCST`):
```
X5MCST: T:=HDEV
5MCST: T+UNLC5; *IOXT                   % UNLOCK
       A:=40; T+"LCON5-UNLC5"; *IOXT    % DISABLE TAG-IN DECODING
       A:=2;  T+"RETG5-LCON5"; *IOXT
```

### 1c. Activation / interrupt-enable sequence
`MP-P2-N500.NPL:3084-3092` (`ACT50` in `XACT500`):
```
ACT50:  5MBBANK; T:=HDEV+LMAR5; *IOXT        % load message-bank addr into LMAR5
        A:=X; *IOXT
        A:=5; T+"LCON5-LMAR5"; *IOXT
        ...
        A:=10; T:=HDEV+LCON5;   *IOXT         % Enable for interrupt
        A:=0;  T+"LSTA5-LCON5"; *IOXT
        A:=1;  T+"LCON5-LSTA5"; *IOXT
               T+"SLOC5-LCON5"; *IOXT         % SLOC5: set lock -> hands off to ND-500
```
Write the multiport message address into `LMAR5`, control `10` (interrupt-enable)
then `5` to `LCON5`, clear `LSTA5`, then `SLOC5` (set-lock) releases the ND-500 to
run. Power-fault clear uses the same register set (`XC-P2-N500.NPL:50-59`).

---

## 2. MON 60 (`N500M`) subfunction table

Full function-code list, `5P-P2-MON60.NPL:165-287` (octal codes):

| Code | Name | Code | Name |
|------|------|------|------|
| 0 | `RREG` read register | 1 | `WREG` write register |
| 2 | `PMREAD` read program memory | 3 | `D5MREAD` read data memory |
| 4 | `PMWRITE` write program mem | 5 | `DMWRITE` write data mem |
| 6 | `SEGLOAD` load segment | 7 | `PLSWAPPER` place swapper |
| 10 | `RREGS` read registers | 11 | `WREGS` write registers |
| 12 | `PRSTART` start program | 13 | `FILCON` connect file |
| 14 | `FILCLO` close file | 15 | `N5RES` allocate ND-500 process |
| 16 | `N5REL` release process | 17 | `FLIOP` list open files |
| 20 | `TIMEUS` | 21 | `WISON` who-is-on |
| 22 | `ERRFSET` | 23 | `RCNTS` read control store |
| 24 | `WCNTS` write control store | 25 | `MICPSTART` micro-program start |
| 26 | `DMEXA` data-mem examine | 27 | `DMDEP` data-mem deposit |
| 30 | `PMEXA` prog-mem examine | 31 | `PMDEP` prog-mem deposit |
| 32 | `DAMR` absolute data read | 33 | `DAMW` absolute data write |
| 34 | `STOPMIC` stop micro-prog | 35 | `FMCLEAR` **master clear** |
| 36 | `ALLPSEG` alloc prog seg | 37 | `CSLOAD` load control store |
| 40 | `MEMDEF` define memory config | 41 | `RSTATU` **read N500+N100 comm status** |
| 42 | `ABREL` abort process | 43 | `N5SRS` reserve special use |
| 44 | `N5SRL` release special use | 45 | `SCPLOOP` scope-loop |
| 46 | `DEFSWAP` | 47 | `DELSWAP` |
| 50 | `TSTFUNC` test function | 51 | `RIREG` **read interface (IODATUT) reg** |
| 52 | `GIVPAGES` | 53 | `TAKPAGES` |
| 54 | `STSWAPPER` start swapper | 55 | `SPLACE` |
| 56 | `EPLACE` | 57 | `RMVERS` read microprog version |
| 60 | `LIMEM` list memory config | 61 | `MRESSPES` |
| 62-66 | histogram fns (`5DEFHIST`...`RELHIST`) | 67-72 | process/segment name-seg lookups |
| 73 | `RPHSG` read physical seg | 76 | `TOSWP` send message to swapper |
| 77 | `RMESSAGE` read last message | 100 | `5RFLAG` read flag-info from N500 |
| 101 | `5WFLAG` write flag-info | 102 | `FORGET` release N500 from SINTRAN |
| 103/104 | `RSYSP`/`WSYSP` sys params | 106 | `LINKTO` |
| 107 | `MICBRK` micro-program break | 110 | `WPHSG` |
| 111-115 | logging | 116 | `XN5REL` logout |
| 117 | `PRSTOP` abort process | 136 | `PRACTIVATE` activate stopped proc |
| 143 | `MO5RT` activate ND-500 proc or ND-100 prog | 144 | `CHACPU` change CPU |

Max function `FUNCMAX=177`. Dispatch by `PARANT` byte table
(`5P-P2-MON60.NPL:309-319`); bits 0-2 = parameter count, bit `RTPLEGAL=4`,
`BSPRES=5`, `COMPROT=6`, `COMSPEC=7` gate authorization (`:299-305`). Interface
error symbols `5P-P2-MON60.NPL:38-128` (e.g. `EILFUNC=2011` illegal function,
`SPCTRAP=2006` MOR/PF/PN/HE/ME/CP/MSR trap, `ILSTOP=2004` illegal stop reason,
`POWER=2057` power fail in ND-500, `ENOCPU=2052` no ND-500 CPU found).

---

## 3. Message / mailbox mechanism

Messages live in a **multiport message bank** addressed by bank register
`5MBBANK` (`T:=5MBBANK` before every `LDATX`/`STATX`). Per-CPU and per-process
mailboxes are reached through `MAILINK`/`MESSBUF`.

Message/mailbox field offsets (`SYMBOLS/M06/N500-SYMBOLS.SYMB.TXT`):

| Sym | Off | Meaning |
|-----|-----|---------|
| `LINK`/`LINK1` | 0 | forward queue link (5405/7197) |
| `LINK2` | 1 | secondary link (7202) |
| `MICFU`(MICFUNC) | 6 | micro-function code (5333) |
| `N500A` | 7 | ND-500 address in transfer (5826) |
| `MESSB`(MESSBUF) | 7 | message-buffer ptr in mailbox (5496) |
| `STOPR`(STOPREASON) | 11 | stop-reason field |
| `N100A` | 11 | ND-100 address in transfer (5824) |
| `KFLIP` | 11 | error flag (7094) |
| `FUNCV` | 13 | return function value (2183) |
| `MCNO` | 13 | monitor-call number (5794) |
| `NRBYT` | 13 | byte count (6131) |
| `TRAPN` | 16 | trap number (762) |
| `MAILI`(MAILINK) | 22 | mailbox link (5687) |
| `CNTXP`(CNTXPAGE) | 57 | context page (3023) |
| `ABUFA` | 140 | auxiliary buffer addr (2697) |
| `XADPR` | 144 | process-descriptor addr (6813) |
| `SPFLA`(SPFLAG) | 143 | special-flag/continuation addr (4188) |
| `PLINK` | 147 | backward queue link (2586) |
| `5PRIO` | -5 (177773) | message priority (1636) |
| `5CPUN` | -6 (177772) | CPU number (1593) |
| `500TU` | -10 (177770) | CPU time used (519) |
| `5MSFL` | -1 (177777) | **status-flags word** (1503) |

`5MSFL` bits: `5IEXQ(UEUE)=15` in-exec-queue (85), `5SYSR(ES)=14` (160/182),
`5CPUB(OUND)=13` (721); also `5IBRK`, `52ESCSET` (`MP-P2-N500.NPL:64-68`).

### Ex-queue insert/remove (VERIFIED)
- **`ITO500XQ`** (`CC-P2-N500.NPL:232-267`): sets `5IEXQUEUE` in `5MSFL`,
  priority-inserts into the mailbox queue via `LINK`/`PLINK`, increments
  `LEXQUEUE` (`LEXQU=000014`, 5606). *"Insert message in the N500-Execution queue
  ... The N100/N500 general semaphore must be locked."*
- **`IFM500XQ`** (`CC-P2-N500.NPL:286-306`): clears `5IEXQUEUE`, unlinks,
  decrements `LEXQUEUE`. `FR5TMQ` does the same for the time-queue (`5ITMQUEUE`).
- **`ITOFIFOQ`** (`XC-P2-N500.NPL:76-93`): ND-5000/Samson variant — inserts into
  an in-memory FIFO ring (`X5MXF`/`X5FYL`/`X5FIF`); uses `CNVBYADR`/`CNVWADR`.

Message size `55MESSIZE=000200` (128 words); process descriptor `5PRDSIZE=000010`.

---

## 4. DMA / shared memory & address conversion

- The DMA address handed to the interface is a **multiport bank:offset** loaded
  into `LMAR5` (`MP-P2-N500.NPL:3084` writes `5MBBANK` then the offset).
- Transfer descriptor fields: `N500A` (off 7), `N100A` (off 11), `NRBYT` (off 13).
  Example set-up `MP-P2-N500.NPL:1330-1333` copies `PDR1->N500A`, `A:=34` into
  `NRBYT`, `ABUFA->N100A`.
- **`CNVWADR`** ("convert word address") / **`CNVBYADR`** ("convert byte address")
  are ND-500 assembler macros invoked in-line as `*NNCxx, CNVWADR`. Comment
  *"Convert multi-port address"* (`MP-P2-N500.NPL:434`, `RP-P2-N500.NPL:209`).
  They map an ND-100 (bank in A/D, offset in X) reference to the ND-500-visible
  multiport byte/word address. **UNCERTAIN**: the macro body (bit layout) is not
  in these files — it lives in the ND-500 microcode/assembler include.
- CPU datafield: per-CPU descriptors `S5CPUDF`...`E5CPUDF`, size `5CPUDFSZ`
  (`MP-P2-N500.NPL:560-564`). Each holds `C5STAT` (checked for `BHPFAIL` bit 0),
  `MAILINK`, `X5CPU`/`X5CCL`. `CNTXPAGE` (`CNTXP=57`) is the per-message
  context-page field.

---

## 5. Interrupt / activation path & stop-reasons

**Interrupt level: 12.** The driver runs on level 12 (*"DRIVER LEVEL"*,
`MP-P2-N500.NPL:727,765,796,828`). Octobus ident connected on `LV12B`
(`MP-P2-N500.NPL:3619`, `CON5IDENT`). `LV12=010000`, `LV12B=000140`,
`BLV12=000014`. Shadow-program scheduling priority word
`"N5PIT+ADPIT+ALEVB+ERNG2"` (`:76-83`). Comm priority `5COMPRIOR=71`.

> **CLARIFICATION 2026-07-15 (idents CORRECTED 2026-07-16) — two different
> "idents" here, don't conflate them:**
> (a) `CON5IDENT` connects an **octobus-protocol IDENT-message entry** (App. 2
> message stream 1, routed by source station + ident number) whose handler runs
> at driver level 12 — that is what the paragraph above describes. (b) The 3109
> **card's ND-100 IOX IDENT codes** are **40B (input/receive) / 41B
> (output/transmit) on level 13**, LIVE-VERIFIED 2026-07-16 by two independent
> ND diagnostics (TPE OCTOBUS B00 LIST-OCTOBUS-DEVICES prints the full table
> 40/41..46/47; CONFIGURATION D05: "Expected identcodes: 40B and 41B"). The
> earlier byte-derived claim of 37B/40B (from ITB13+37B/+40B = IOCT0/OOCT0)
> misread the ITB13 slot index as the ident code (plausibly ident N sits at
> ITB13+N-1 - UNVERIFIED); the 60B/61B formula claim stays wrong. See
> [OCTOBUS-ND100-ND5000-REFERENCE.md section 6.2](../ND5000/OCTOBUS-ND100-ND5000-REFERENCE.md).

**Answer decoding (VERIFIED)** — `DECOMESS` (`MP-P2-N500.NPL:803-819`):
```
IF A=3MONCO OR A=3TRACO OR A=3START OR A=3WMONCO THEN
   T:=5MBBANK; *AAX STOPR; LDATX
   IF A=MOCALL   THEN CALL MCHANDLE        % STOP-REASON IS MON.CALL
   ELSE IF A=5FMOCALL THEN CALL MCHANDLE   % FILE-TRANSFER MONCALL
   ELSE IF A=TRAPCODE THEN CALL TRAPDECODER% STOP-REASON IS TRAP
   ELSE CALL 5RRTWT                        % RESTART ND-100 PROCESS
```

**Stop-reason codes** (`STOPR` field): `MOCAL(L)=000001`, `TRAPC(ODE)=000002`,
`5FMOC(ALL)=000003`. **Micro-function codes** (`MICFU`): `3MONCO`, `3TRACO`,
`3START`, `3WMONCO`, `3WMED`/`3RMED`. **N5STATUS** values (`WN5STATUS`):
`MSGN500=1`, `WAITING=2`, `SWPPING=6`, `STOPPED=13`, `MPACTIVE=1`, `SUSPC=145`.

`MCHANDEL` (`MP-P2-N500.NPL:1286+`) decodes MON messages; special range
`L12MIN=500`...`L12MAX=523` handled on level 12 via the `GOSW` table (`:1385-1390`);
`CERN=376` and `N5SWAP=377` special (`:1272-1273`). Unfieldable traps raise
`SPCTRAP=2006`/`ILSTOP=2004`. Abort/escape `ESC500`/`SYSABORT` (`:55-91`) sets
`5IBRK`/`52ESCSET`; power-fail restart `XRSTARTALL` (`:3113+`).

---

## 6. Octobus / physical notes (ND-5000 / Samson path)

Selected when CPU type is `SAMSON` (`RP-P2-N500.NPL:85`) or via patch markers
`*NNJ14`, `*NNJ00`/`*NNJ01` (`MP-P2-N500.NPL:3058`, `CC-P2-N500.NPL:183-192`).
Uses the Octobus serial ring instead of the `HDEV` DMA registers:

- **IOX layout** (from `../Devices/Octobus/OCTOBUS-PROTOCOL-REFERENCE.md`,
  cross-checked vs `PH-P2-OPPSTART.NPL`): Input controller base `100400`
  (+0 read-data, +2 read-status, +3 write-control); Output controller base
  `100404` (+5 write-data, +6 read-status, +7 write-control). "Clear interface"
  writes `20` to `+3` and `+7`.
- **Frame format**: 16-bit word, bit 15 `CBIT` = control/data, bit 7 `EBIT` =
  enable; station number shifted into upper bits
  (`A SH 10 BONE CBIT BONE EBIT`).
- **Ring/ident**: `OCTOR(ING)=000000`, `N100I(DENT)=000001`, `OMDAC(CP)=000003`,
  `OMDNO=177777`. `CON5IDENT` (`MP-P2-N500.NPL:3614-3634`) connects the ND-100
  ident on level 12 and sends an "alive" message; `MFPREPARE`/`MBSEND` build/send
  Octobus messages (fields `MOCTSTATION`, `MOCTOMD`, `MBROADCAST`, `MMSGLENGTH`,
  `MCOMMAND`).
- Command codes `CM*`: masterclear `CMMAC=41`, run `CMRUN=33`, stop `CMSTO=34`,
  continue `CMCON=35`, reset `CMRES=36`, memory read/write `CMREA=20`/`CMDWW=24`.

### PIOC (peripheral/interface processor) — related but distinct bus
`MP-P2-PIOC-DRIV.NPL` / `RP-P2-PIOC.NPL` drive a PIOC over `HDEV`/`HDEV+3` with a
control word `PWCR`: `A:=PWCR BONE BNDC; T:=HDEV+3; *IOXT` = "wake up PIOC"
(`MP-P2-PIOC-DRIV.NPL:241,476`); `PWCR BONE BENA` = enable (`:579`); `A:=60;
T:=HDEV+3; *IOXT` = "reset and halt" (`RP-P2-PIOC.NPL:323,346`); `T:=HDEV; *IOXT;
SHA ZIN SHR 10` reads a bank number (`:190,309`). Separate load/interface
processor, not the ND-500 CPU interface.

---

## 7. Open questions / UNCERTAIN

1. **`HDEV` runtime value** — resident variable loaded at boot; only literal seen
   is overlap check `X.HDEV=660` (`RP-P2-N500.NPL:989`). Treat 660 as observed.
2. **`CNVWADR`/`CNVBYADR` algorithm** — body not in these files (invoked via
   `*NNCxx` patch macros); mapping lives in the ND-500 microcode/assembler include.
3. **`RTAG5`/`RUPP5` (off 10) and `LTAG5`/`LUPP5`/`LDAT5`/`LLOW5` (off 11/13)** —
   two symbol aliases per offset; precise DMA-burst use not spelled out.
4. **`RCON5`/`RSTA5` write control-bit meanings** beyond written values
   (`10,5,1,40,2,0`) — `10`=interrupt-enable and `SLOC5`=release/run inferred.
5. **Full trap-code table** — `TRAPDECODER` (`MP-P2-N500.NPL:812`, region 835+)
   not fully expanded; individual ND-500 trap numbers / `SPCTRAP` set
   (MOR/PF/PN/HE/ME/CP/MSR) need a follow-up read.

### Key file:line index for a bench RE session
- Register offsets: `SYMBOLS/M06/N500-SYMBOLS.SYMB.TXT:1224-1230, 7229-7237, 522, 3659, 4661`
- Status bits: `XC-P2-N500.NPL:41-45`
- Master clear: `CC-P2-N500.NPL:212-217`
- Activate + interrupt-enable: `MP-P2-N500.NPL:3084-3092`
- Power-fail clear: `XC-P2-N500.NPL:49-64`
- MON 60 function codes: `5P-P2-MON60.NPL:165-287`
- Ex-queue in/out: `CC-P2-N500.NPL:232-306`
- Answer/stop-reason decode: `MP-P2-N500.NPL:803-819`, `1246-1402`
- Octobus registers/frames: `../Devices/Octobus/OCTOBUS-PROTOCOL-REFERENCE.md`, `MP-P2-N500.NPL:3586-3634`
