# SINTRAN III L07 - SCSI PROTOCOL DRIVER (complete level-11 / interrupt carve)

Byte-verified static reverse-engineering of the SCSI **protocol driver**
(`IP-P2-SCSI-DRIV`) as it runs in the live L07 system (L-VSX-500). This carve
covers **every path** of the driver: the enqueue side (`SCLLD` -> `INITO` ->
`SCWAQ`), the arbitration side (`SELEC`), and the whole interrupt/completion
state machine (`SCINT` + `SCISR` phase demux, `DCTHR`/`TEROP`/`BUSFP`/`SCRST`,
the message handlers).

- **Segment:** `065-S3SIPIT`, load base `32000B` (byte-identical twin
  `066-S3IIPIT`; `cmp -l` = 0 diffs).
- **Binary:** `../../../segments/065-S3SIPIT.bin` (big-endian, as carved).
- **sha256:** `9ccd8f3df7666eb3ac3d89e3edf68a0b1b7688f2c98a4a3b1fb761d58562b144`.
- **Byte offset of octal addr A:** `(A - 32000B) * 2` (both octal, result decimal).

**Ground truth = the carved bytes.** The NPL (`IP-P2-SCSI-DRIV.NPL`) is a
different revision and is used only to *name* the logic. Its column-1 addresses
do **not** equal the L07 addresses, and the per-routine drift is **not** a fixed
`+376B` (SCLLD drifts `+376`, SELEC drifts `+402`). Every carved address here
came from bytes or from an embedded pointer word, and every entry plus the
load-bearing words were reproduced with `dd`.

### Relationship to the older `../SCSI-DRIVER/` folder
This folder **supersedes the driver half** of `../SCSI-DRIVER/` by carving the
paths that older folder left as `...` (the SCINT phase ladder, DCTHR/TEROP,
SCRST, the message handlers). The older folder's function-42/disk-layer section
was already flagged as wrong-address; that half is owned by
`../SCSDISK-TRANSFER/`, `../FUNCTION-42-RETURN/`, and the SCSI-DISKLAYER-COMPLETE
carve. **This carve does not re-do the disk layer** - only the disk-layer ->
driver hand-off (`EXCOM -> GO SCLLD`) is shown.

---

## 1. Verified routine address map

All octal. "How verified" column: `dd` = entry word reproduced from the binary;
`ptr` = value read from an embedded pointer word (also dd'd); `struct` = the
routine's first instructions match its NPL body at that address.

| Routine | Addr | Role | How verified |
|---------|------|------|--------------|
| SCLLD | 067160 | driver entry / classify+enqueue+arbitrate | dd word `146151` |
| SCINT | 067247 | interrupt / completion handler | dd word `050775` |
| SCTIO | 067471 | software timeout handler | struct (NPL SCTIO) |
| SCDIS | 067724 | fatal error -> reset SCSI bus | struct + ptr@067345 |
| BUSFP | 067745 | bus-free phase -> CALL SELEC | ptr@067354/067765 |
| EXDRI | 067746 | exit driver (compute return P) | ptr@067240; struct |
| SELEC | 070165 | arbitration / selection | dd word `030374`; ptr@067242 |
| RFWAQ | 070250 | remove head of SCWAQ | ptr@067352/067461; struct |
| INITO | 070261 | init op + splice onto SCWAQ tail | dd word `046030`; ptr@067241 |
| CNTHR | 070321 | connect physical path | ptr@067463; struct |
| DCTHR | 070341 | disconnect logical thread | ptr@067350; struct |
| RSTMR | 070375 | restart LUN timer | ptr@067351; struct |
| ENTIM | 070421 | enable LUN timer | ptr@067243/067464; struct |
| TEROP | 070500 | terminate operation | dd word `046033`; ptr@067353/070373 |
| DITIM | ~070561 | disable LUN timer (falls out of TEROP) | NPL |
| SCRST | 070635 | start reset sequence on SCSI bus | ptr@067244/067764; struct |
| STFIN | 071031 | terminate reset / selftest done | ptr@067465 |
| MSGOUT/EMSGO family | 071121 | message-out phase | ptr@067246/067770 |
| SCMPE | 071746 | send "message parity error" | ptr@067466 |
| SCIDE | 071754 | send "initiator detected error" | ptr@067346/067467 |
| RINFO | ~071325 | read one info byte from SCSI bus | NPL |

**Driver datafield** (B-relative; B base resolves cells to `0200000+disp`, names
from SYMBOL-1-LIST): `HDEV -3`, `BUSFL -35`, `SCTST -36`, `SCRXR -44`,
`SCRLR -45`, `SCRCO -46`, `NCROK -47`, `SCWTI -51`, `SCTQP -56`, `SCWAQ -57`,
`SCCSU -60`, `CMSGI -61`, `CMSGO -62`, `SCISR -76`, `SCEIM/SCNIH -77`,
`SCCCW -100`, `SCNIS -101`, `SCSSR -102`.

---

## 2. The driver state machine (every arm)

### 2a. Enqueue side - `SCLLD` (067160)
`A = function`, `X = unit`, `L = caller return`. Classify `class = A >> 8`:

- **class 0..2 (DATA TRANSFER, READ/WRITE):** store `A=:SUCON`, `L=:SULRG`,
  `0=:SUTRG`. If `NCROK<0` (interface dead) -> `GO FAR EXDRI` with `NCRST`.
  Else `CALL INITO` (**the enqueue**), and if `BUSFL==0` (bus idle)
  `CALL SELEC` (**start arbitration**). Then `GO SCWTI`.
- **class 3 (start-with-timer control):** store control/return, set
  `SUTHS = 1<<6SFUN`, `CALL ENTIM`, `GO SCWTI`.
- **class 4 (bus reset):** if `NCROK.8SRST` already set -> intermediate return
  (`L+1`). Else save X/L/A into SCRXR/SCRLR/SCRCO and `GO FAR SCRST`.
- **else:** `T := ILDCO` (illegal function), `GO SCWTI`.

### 2b. `INITO` (070261) - the ONLY writer that appends to SCWAQ
Build the thread status from the op class: class 0 = `INOPR` (+`6SRFD` if
`4SRCA`), class 1 = `INABO` (abort), else `INBDR` (bus device reset). Seed
`SUSDP:=SUIDP`, `SUSBC:=SUIBC`, `SUTMR:=-2`. Then walk `SULINK` from the
`SCWAQ` anchor to the tail and set `tail.SULINK = X`. **If SCLLD is not called
for a block, that block is never on SCWAQ and never reaches the wire.**

### 2c. Arbitration side - `SELEC` (070165), both arms carved
Save regs, `SCEIM:=-1` (disable expected interrupt), `X := SCWAQ`:
- **EMPTY arm (070240):** `JXZ` at 070172 taken -> `BUSFL := 0` (**declare bus
  free**) -> common tail re-enables interrupt and returns. This is the leg the
  failing ENTER-DIRECTORY trace takes.
- **NON-EMPTY arm (070173..070236):** clear-to-memory, `BUSFL := 1<<6SARB`,
  write SCSI ident to `WDESI`, load the waiting time (`WATFS`) into the transfer
  counter (`WTCM/WTC2/WTCL`), pick select cmd `010` or `011` (`4SINA`), write it
  to `WNCOM` (**the select goes on the wire here**), then `SCTST:=1`, `TMR:=-5`
  (arm the select timeout). Common tail `WCONT:=5` re-enables the interrupt.

### 2d. Interrupt / completion - `SCINT` (067247) full ladder
Read `RSTAU`. If `64/\status`: bit2 -> controller busy (leave); bit5 -> SCSI bus
reset received -> `GO FAR SCDIS`; bit4 -> `CALL SCIDE`. If not `INTERRUPT FROM
NCR` (bit 11) -> tail. Otherwise clear-to-memory, read `RAUXS` and `RITRG` into
`SCNIS`, `SCCCW:=0`.

- **EXPECTED interrupt** (`(SCNIS/\177500)==SCEIM`, 067310-067314):
  `CALL SCISR` runs the phase machine (NEWPH -> DAOPH/DAIPH/COMPH/STAPH/MSOPH/
  MSIPH). This is the normal data/command/status/message path.
- **UNEXPECTED** -> decode `A = SCNIS>>8`:
  - **A==4 & BUSFL!=0 : DISCONNECT** (067320). If `SCCSU!=0` -> `CALL DCTHR`.
    Else if `BUSFL.6SARB` (arbitration timeout): `MIN SCWAQ.SUTMR` retries;
    when exhausted `SCTST:=0`, `RSTMR`, `RFWAQ`, `TEROP(NESER)`. Always
    `GO FAR BUSFP` (re-arbitrate). **<- the leg the failing trace runs.**
  - **A==1 & 6SARB : ARBITRATION WON** (067356). `RFWAQ`, set `6SCRP` in BUSFL,
    `CALL CNTHR` (connect), `SCTST:=0`, `CALL ENTIM`, `SCNIH:=RBSIR`.
  - **A==020 & !6SARB & !6SRST : RECONNECT** (067376). Set `6SCRP`, `SCNIH:=RBSIR`.
  - **A==1 & NCROK.8SDIA : SELFTEST finished** (067412) -> `GO FAR STFIN`.
  - **A==SCEIM>>8 & PARIT : parity error** (067422). If message phase
    (`017/\BUSFL==7`) -> `CALL SCMPE` else `CALL SCIDE`; then `CALL SCISR`.
  - **else : illegal interrupt** (067451) -> `T:=NCRER; GO FAR SCDIS`.
- **Tail (067453):** `WCONT := 5\/SCCCW` (activate + enable interrupt),
  `GO SCWTI`.

### 2e. Completion helpers
- **DCTHR (070341):** if `SUTRG==0 && BUSFL.6SDIS` save thread status
  (`DCCLR\/CMSGO`) and return (intermediate if `6SRFD`, else no-return);
  otherwise `CALL TEROP`. Always `SCCSU:=0`, return via `HOME4`.
- **TEROP (070500):** derive final status - `SUTRG!=0`: error; `SCCSU==x`: save
  `SCCDP/SCCBC`, then `6SCCO`->`NOSST`/`0`, unexpected disconnect->`UNDIS`,
  message errors->`TRANE`/`MNIBT`; timer op->`0`. Clear `SUTHS` (free the unit),
  fall into `DITIM` (unlink from timer queue, `RSTMR`).
- **BUSFP (067745):** `CALL SELEC` (pull next SCWAQ head or idle the bus), then
  fall into **EXDRI (067746)**: `X==0`->SCWTI; else return `SULRG+2` (T==0, ok),
  `SULRG+1` (T<0, intermediate), or `SULRG` with `MIN ERCNT` (T>0, error).
- **SCRST (070635):** mark active ops for recovery (`8SCLR`), lock the interface
  (`SCEIM:=-1`, `SCTST:=2`), disconnect+clear the NCR, drive RST >=250us (or
  handle an incoming RST), launch selftest/diagnostic, `GO SCTIO`.

---

## 3. How a queued block-0 transfer reaches the wire

1. Disk layer builds the READ(6) CDB and executes it through the vector at
   `063453 = 067160 = SCLLD`. **VERIFIED:** `word@063453 = 067160`
   (dd off 26198 -> bytes `6e70`).
2. `SCLLD` stores `SUCON`, `INITO(x)` links `x` onto the `SCWAQ` tail.
3. `BUSFL==0` -> `SELEC` finds `x` at the SCWAQ head and drives ARBITRATION +
   SELECT (`WNCOM`) onto the bus.
4. Target responds; `SCINT` sees `A==1 & 6SARB` -> `RFWAQ`, `CNTHR` (SCCSU:=x),
   `ENTIM`, `SCNIH:=RBSIR`.
5. Each following bus-service interrupt is EXPECTED (`SCEIM` match) -> `SCISR`
   -> `NEWPH`: **COMPH** DMAs the 6-byte CDB out, **DAIPH** DMAs the 512-byte
   block into ND-100 memory, **STAPH** reads the status byte, **MSIPH** handles
   COMMAND-COMPLETE / DISCONNECT.
6. On COMMAND COMPLETE: `TEROP` derives `T=0`, frees the unit, `EXDRI` returns
   `SULRG+2`, `BUSFP -> SELEC` pulls the next element or idles the bus.

**A block-0 READ only reaches the wire if `SCLLD` is called for it.** On the
failing ENTER-DIRECTORY trace, after the function-42 control-record read the
driver runs only the `A==4` disconnect leg -> `DCTHR` -> `TEROP` -> `BUSFP` ->
`SELEC` finds **SCWAQ EMPTY** -> `BUSFL:=0`, and the bus goes idle. The missing
block-0 enqueue is **upstream** of this driver (the device-agnostic mount path),
consistent with the ground-truth trace silence.

---

## 4. dd verification (reproduced from the binary)

```
addr    off    bytes  word    meaning
067160  29920  cc69   146151  SCLLD entry  (RADD CLD SA DD)
067247  30030  51fd   050775  SCINT entry  (LDT ,B -3 = HDEV)
070165  30954  30fc   030374  SELEC entry  (STF -4 = save SVTAD)
070261  31074  4c18   046030  INITO entry  (LDA ,X 30 = SUCON)
067172  29940  b003   130003  SCLLD NCROK check (JAP 3)          [load-bearing]
070172  30964  b626   133046  SELEC SCWAQ-empty branch (JXZ 46)  [load-bearing]
070373  31222  7140   070500  DCTHR -> TEROP pointer             [load-bearing]
067241  30018  70b1   070261  ptr -> INITO
067242  30020  7075   070165  ptr -> SELEC
067765  30698  7075   070165  ptr -> SELEC (BUSFP)
063453  26198  6e70   067160  disk-layer call vector -> SCLLD
070500  31360  4c1b   046033  TEROP entry  (LDA ,X 33 = SUTRG)
```

---

## 5. VERIFIED / INFERRED / OPEN

| # | Claim | Status |
|---|-------|--------|
| 1 | Driver core in `065-S3SIPIT` (= `066-S3IIPIT`), base `32000B` | VERIFIED (bytes + cmp) |
| 2 | SCLLD/SCINT/SELEC/INITO/TEROP entries at the addresses in section 1 | VERIFIED (dd words) |
| 3 | SCLLD classify `A>>8`: <3 transfer, 3 timer-ctrl, 4 reset, else illegal | VERIFIED (067160-067237) |
| 4 | INITO splices X onto the SCWAQ tail; only writer that appends | VERIFIED (070310-070317) |
| 5 | SELEC EMPTY arm sets `BUSFL:=0`; NON-EMPTY arm writes select to `WNCOM` | VERIFIED (070172/070240 vs 070224) |
| 6 | SCINT `A==4 & BUSFL!=0` disconnect -> DCTHR -> BUSFP; SCCSU==0 arb-TO retry | VERIFIED (067320-067344) |
| 7 | SCINT EXPECTED-int path (`SCNIS/\177500==SCEIM`) -> SCISR phase machine | VERIFIED (067310-067314) |
| 8 | DCTHR calls TEROP and clears SCCSU (the failing-trace disconnect leg) | VERIFIED (070341-070367 + ptr@070373) |
| 9 | BUSFP = `CALL SELEC` then EXDRI; EXDRI returns SULRG+2/+1/+0 by T | VERIFIED (067745-067763) |
| 10 | Disk layer reaches the driver via `063453 -> SCLLD` (067160) | VERIFIED (dd word@063453) |
| 11 | COMPH DMAs the CDB, DAIPH DMAs the block, STAPH/MSIPH complete the op | VERIFIED-logic (NPL 655-918; carved bytes in ../SCSI-DRIVER/_driver.dis) |
| 12 | The block-0 READ only reaches the wire if SCLLD is called for it | VERIFIED (INITO is the sole SCWAQ writer, only reached from SCLLD) |
| 13 | The missing block-0 enqueue is upstream (device-agnostic mount path) | INFERRED (consistent with trace silence + ground truth) |
| 14 | Exact NEWPH byte-transfer/MAR-fixup arithmetic per phase | OPEN (logic from NPL; not re-dd'd word-by-word here) |
| 15 | CACOB/EXCOM/5SCIN gate internals (disk-layer command build) | OPEN here - owned by SCSI-DISKLAYER-COMPLETE (only the SCLLD vector is in scope) |

---

## 6. Cross-links

- `../SCSI-DRIVER/` - prior driver-half carve (superseded for the SCINT/DCTHR/
  SCRST paths); `_driver.dis` there is the full raw disassembly of `067160..072011`.
- `../SCSDISK-TRANSFER/` - disk-layer function-0 dispatch (the caller of SCLLD).
- `../FUNCTION-42-RETURN/` - disk-layer READ FORMAT / control-record path.
- `../ENTER-DIRECTORY/` - the device-agnostic mount path (upstream of this driver).
- NPL logic source: `SINTRAN/NPL-SOURCE/NPL/IP-P2-SCSI-DRIV.NPL`.
- Segment disassembly: `../../segments-ref/065-S3SIPIT/065-S3SIPIT.asm`.

## Files
- `SCSI-DRIVER-COMPLETE.ASM` - annotated, byte-verified disassembly of every
  driver routine (SCLLD, SCINT full ladder, SELEC both arms, INITO, CNTHR,
  DCTHR, RSTMR, ENTIM, TEROP, SCRST, SCDIS/BUSFP/EXDRI) + the disk-layer vector.
- `SCSI-DRIVER-COMPLETE.pseudo.c` - the full driver state machine in pseudo-C,
  every arm, plus the "how a block-0 transfer reaches the wire" walk-through.
- `README.md` - this file.
