# SCSDISK-TRANSFER - does a fn-0 READ of block 0 reach SCLLD, or is it rejected first?

Byte-verified reverse-engineering of the SINTRAN III VSX/500 **L07** SCSI disk
transfer primitive **SCSDISK** (`= SCSDI 057215B`, the routine the
device-agnostic filesystem reaches through the SCSI unit datafield field
`,X 14`) and the SCSI driver-command dispatch **SCSID** (`062217B`), traced
along the **function-0 (READ) path for block 0** all the way to the driver
enqueue **SCLLD** (`067160B`) - or to the pre-transfer reject.

This answers the ONE question the FUNCTION-42-RETURN carve left open from the
disk-layer side: after function 42 succeeds and leaves `5SCIN` SET, when the
filesystem issues its separate "read block 0" (fn 0) request through
`,X 14 -> SCSDISK`, does that request **reach** `SCLLD -> INITO -> SCWAQ`, or is
it **dropped / rejected** in the SCSI disk layer before the transfer (case 2)?

- **Segment:** `065-S3SIPIT` (load base `32000B` = 13312 dec; byte-identical
  twin `066-S3IIPIT`).
- **Source bin:** `../../segments/065-S3SIPIT.bin` (big-endian words).
- **Disasm:** `nd100-dis -a -o -b 13312` over the byte-swapped image.
- **dd offset:** `byte_off = (addr - 32000B) * 2` (decimal).
- Evidence: **VERIFIED** = read from L07 bytes. **INFERRED** = NPL logic
  (`IP-P2-SCSI-DISK.NPL`, a DIFFERENT revision - naming only). **OPEN** =
  uncarved / runtime. All addresses/values **octal**.

---

## 0. The +376B revision offset (re-confirmed)

The entire `IP-P2-SCSI-DISK` compilation unit (SCSDISK, SCSID, CACOB, INQUI,
the fn-42 handling) sits in the L07 image at **NPL-source-label + 376B**:

| Symbol | NPL label | L07 (SYMBOL-2-LIST) | delta |
|--------|-----------|---------------------|-------|
| SWT11  | 056120    | 056516              | +376B |
| CTRSC  | 056223    | 056621              | +376B |
| SCSDI (SCSDISK) | 056617 | **057215**       | +376B |
| SCSID  | 061621    | **062217**          | +376B |

The SCSI **driver** (`IP-P2-SCSI-DRIV`: `SCLLD=067160`, `INITO=070261`,
`SELEC=070165`, `SCINT=067247`) is a separate unit aligned directly to its NPL
labels (no +376B).

---

## 1. The fn-0 read-block-0 dispatch path (VERIFIED, byte-by-byte)

```
",X 14"  (RCBLO device transfer dispatch, JPL I ,B 10)
  -> SCSDISK  = SCSDI 057215B
       057215 RADD CLD SL DA / 006006 STA ,X 6   A:=L=:X.NFUNC   (entry)
       057217 JPL I 51 -> DIALO                  MON PERFO
       057221 LDA ,X 14 / SAX 77 / RAND SA DX     fn := ABFUN & 077
       -- geometry bounds check (whole-disk arm) --
       057352 LDD ,X 17     AD := X.ABPA2  (disk block address, =0 for block 0)
       057353 LDT ,X 22     T  := X.ABP32  (amount)
       057354 RADD ST DD / 057355 RADD ADC CLD SA DA   AD := addr + amount
       057356 LDT 32 / 057357 RADD SB DT             T := UHLIM (+B)
       057360 JPL I 27 -> COMPD                      compare (addr+amount, UHLIM)
       057361 SKP IF 0 GRE SA                        skip if within bounds
       057362 JMP 15 -> 057377 = ERR2               *** past end -> BADPA ***
       -- block 0: addr 0 -> within bounds -> NO branch to ERR2 --
       057367 MIN ,B 10 (SUNOP) ; if SLINK != 0 -> DSORT/SWT11 else NEWOP
  -> NEWOP / RETRY
       5SCIN lazy-init loop (057515..057532): SKIPPED because 5SCIN is SET
       -- real transfer driver call --
       057606 LDA 27 / 072365 AND ,X -13    A := 377 & X.SCOCW
       057610 JPL I 21 -> 057631            CALL SCSID   (ptr 057631 = 062217)
  -> SCSID 062217B
       062217 STA ,X 10                     entry (A=:X.HSTAT)
       062226 SAA 77 / AND ,X 14            fn := ABFUN & 077
       062231 SAT 37 / SKP IF DA EQL ST     fn 37 -> DOEXS  (not us)
       062243 LDA ,B 31 (SUTHS)             busy? -> queue+SCWTI (not us)
  -> REPEAT 062300B  (dispatch by function)
       062302 fn 31 -> BDRST
       062305 fn 74 -> GUSCB
       062312/062315 fn 42/36 -> INQUI
       062320/062323 fn 23/25 -> MODES
       -- fn 0 matches none -> FALL THROUGH --
       062326 JPL I 66 -> 062414            CALL CACOB   (build SCSI command)
       062327 JPL I 66 -> 062415            CALL EXCOM   (execute -> SCLLD)
  -> CACOB 063750B
       063752 LDA ,B 23        A := SUTYP
       063753 BSKP ONE 70 DA   skip JMP if 5SCIN (bit 7) = ONE
       063754 JMP I 163 -> INQUI      5SCIN CLEAR -> divert to INQUI
       -- 5SCIN SET (our case): skip the JMP, build READ(6) CDB (SCSF1[0]=010\340) --
  -> EXCOM 063403B
       063430 JMP I 23 -> 063453         GO SCLLD   (ptr 063453 = 067160)
  -> SCLLD 067160B   (IP-P2-SCSI-DRIV: enqueue -> INITO -> SCWAQ)
```

---

## 2. The exact decision instructions

| What | Address | Word | Meaning |
|------|---------|------|---------|
| **Pre-transfer reject branch** | `057361`/`057362` | `141050` / `124015` | `SKP IF 0 GRE SA` then `JMP -> ERR2`. If `addr+amount > UHLIM` -> ERR2. Block 0 (addr 0) does **not** take this branch. |
| **The reject itself (BADPA)** | `057401` | `171005` | `SAT 5` -> `T := 5 = BADPA`; then `RETOP`/`SWT11`, **no SCLLD**. |
| **fn-0 falls to CACOB** | `062326` | `135066` | `JPL I 66 -> CACOB` (fn 0 matches none of 31/74/42/36/23/25). |
| **5SCIN re-entry gate** | `063752`/`063753`/`063754` | `044423` / `175275` / `125163` | `LDA SUTYP; BSKP ONE 70 DA; JMP I -> INQUI`. 5SCIN **SET** -> skip -> build CDB. |
| **Driver enqueue call** | `062327` -> EXCOM `063430` -> `067160` | `135066` / `125023` / ptr `067160` | `CALL EXCOM`; EXCOM `GO SCLLD`; pointer `063453 = 067160 = SCLLD`. |

**Where block 0 reaches SCLLD or is rejected:** the reject can only happen at the
UHLIM branch `057361/057362 -> ERR2 057401 (BADPA)`. For block 0 the address is
`0`, so `addr + amount <= UHLIM` and the branch is **not taken**; control
proceeds to `CALL SCSID (057610) -> CACOB (062326) -> [5SCIN SET, 063752] ->
EXCOM (062327) -> SCLLD (067160)`. **Block 0 reaches SCLLD.**

---

## 3. dd spot-checks (raw big-endian `065-S3SIPIT.bin`)

| Item | L07 addr | byte off | word | mnemonic |
|------|----------|----------|------|----------|
| SCSDISK entry | `057215` | 21786 | `146145` | `RADD CLD SL DA` (`A:=L=:X.NFUNC`) |
| BADPA reject | `057401` | 22018 | `171005` | `SAT 5` (`T:=BADPA`) |
| ptr -> SCSID | `057631` | 22322 | `062217` | pointer word = SCSID address |
| SCSID entry | `062217` | 24862 | `006010` | `STA ,X 10` (`A=:X.HSTAT`) |
| 5SCIN gate | `063752` | 26580 | `044423` | `LDA ,B 23` (`A:=SUTYP`) |
| ptr -> SCLLD | `063453` | 26198 | `067160` | pointer word = SCLLD address |

Reproduce, e.g.:
`dd if=065-S3SIPIT.bin bs=1 skip=24862 count=2 | od -An -tx1` -> `0c 08` = `006010`.

---

## 4. THE ANSWER

**A function-0 (READ) request for block 0 REACHES `SCLLD` in the static L07
code.** It is **not** dropped or rejected inside the SCSI disk layer.

- The only pre-transfer reject in the disk layer is the UHLIM/geometry bounds
  check (`057356..057362`) that yields **`BADPA` (`T=5`) at `057401`**. That
  branch fires **only** when `disk_address + amount > UHLIM`. Block 0 has
  `address = 0`, so it is trivially within bounds and the branch is **not
  taken** - byte-verified at `057361/057362`.
- The `5SCIN` re-entry gate at `063752` (in `CACOB`), with `5SCIN` **SET** (the
  state function 42 leaves), takes the fall-through and **builds the READ(6)
  CDB**; it does not divert to `INQUI`. It then reaches `EXCOM -> GO SCLLD
  (067160)` - byte-verified pointer at `063453 = 067160`.

Therefore **case 2 (block-0 request issued but rejected before `SCLLD`) is
REFUTED by the bytes** for block 0 specifically: nothing in `SCSDISK` / `SCSID`
/ `CACOB` / `EXCOM` rejects a well-formed fn-0 block-0 read; it enqueues it.

**The remaining explanation is case 1: the device-agnostic caller never issues
the block-0 fn-0 request** (or issues it with an out-of-range address that would
trip BADPA, which for a correct block-0 LBA cannot happen). This is consistent
with:
- FUNCTION-42-RETURN (function 42 returns `T=0`, leaves `5SCIN` SET, and the
  disk layer contains no "issue block-0 vs not" branch), and
- the live ground truth (`SCWAQ` empty, `SCLLD` never called for block 0,
  silence after the control-record `READ(6)`).

### The settling runtime check (unchanged from FUNCTION-42-RETURN section 5)
Break `SCSID 062217B` and the 5SCIN gate `063752B`:
- If a **second** `SCSID` entry with `(ABFUN & 077) == 0` and a block-0 address
  arrives, watch it fall through `062326 -> CACOB -> 063752 (5SCIN SET) -> EXCOM
  -> 063430 -> SCLLD 067160B`. If instead it errors first, dump `ABPA2`/`ABP32`
  against `UHLIM` - a mis-scaled LBA would take `057362 -> ERR2 -> BADPA`.
- If `SCSID` is **never re-entered** for block 0 (the trace's actual behaviour),
  the caller never issued it -> the decision is in the **uncarved
  device-agnostic connect/mount overlay**, not here.

---

## 5. VERIFIED / INFERRED / OPEN

| # | Claim | Verdict |
|---|-------|---------|
| 1 | Whole `IP-P2-SCSI-DISK` unit at L07 = NPL + 376B (SCSDI 057215, SCSID 062217) | VERIFIED (symbols + dd) |
| 2 | SCSDISK entry `057215` `RADD CLD SL DA / STA ,X 6` (`A:=L=:X.NFUNC`) | VERIFIED (dd `146145`) |
| 3 | Whole-disk UHLIM bounds check at `057352..057362`; branch to ERR2 on `>UHLIM` | VERIFIED |
| 4 | Reject sets `T:=BADPA(5)` at `057401` (`SAT 5`), then RETOP/SWT11, no SCLLD | VERIFIED (dd `171005`) |
| 5 | Block 0 (addr 0) is within bounds -> does NOT branch to ERR2 | VERIFIED (branch semantics) |
| 6 | SCSDISK calls SCSID; pointer `057631 = 062217` | VERIFIED (dd `062217`) |
| 7 | SCSID `062217` dispatches by ABFUN; fn 0 falls through to CALL CACOB at `062326` | VERIFIED |
| 8 | 5SCIN gate `063752`: `LDA SUTYP; BSKP ONE 70 DA; JMP I -> INQUI`; SET -> build CDB | VERIFIED (dd `044423`) |
| 9 | CACOB READ CDB via `SCSF1[0]=010\340` = SCSI READ(6) op 0x08 | VERIFIED (SCSF1 table bytes) + INFERRED (opcode meaning) |
| 10 | EXCOM `GO SCLLD`; pointer `063453 = 067160 = SCLLD` | VERIFIED (dd `067160`) |
| 11 | fn-0 block-0 read REACHES SCLLD; case 2 (pre-transfer reject) REFUTED | VERIFIED |
| 12 | Case 1 (caller never issues block 0) is the remaining explanation | INFERRED (this carve + trace) |
| 13 | The deciding "issue block-0 vs not" instruction lives in the uncarved overlay | OPEN |

**Provenance.** Carved `065-S3SIPIT` L07 bytes (base `32000B`); symbols
`SINTRAN/NPL-SOURCE/SYMBOLS/L07/SYMBOL-2-LIST.SYMB.TXT` (SCSDI/SCSID/SCLLD/SWT11)
and FILSYS/SYMBOL-1 (SUTYP=23, 5SCIN=7, UHLIM, SCWAQ). Logic (different
revision, INFERRED): `SINTRAN/NPL-SOURCE/NPL/IP-P2-SCSI-DISK.NPL`
(SCSDISK / SCSID / CACOB / EXCOM / INQUI). Ground truth: INQUIRY + READ CAPACITY
+ one control-record READ(6), then silence; `SCWAQ` empty.

## See also
- [`../FUNCTION-42-RETURN/README.md`](../FUNCTION-42-RETURN/README.md) - function 42 return values + the 5SCIN state machine (companion; found the `063752` gate).
- [`../SCSI-DRIVER/README.md`](../SCSI-DRIVER/README.md) - the driver core `SCLLD -> INITO -> SCWAQ` (the enqueue this path lands in). NOTE: its function-42 section used wrong (no-offset) addresses - superseded by FUNCTION-42-RETURN.
- [`../RCBLO/README.md`](../RCBLO/README.md) - the `,X 14` cache-block transfer dispatcher (the caller of SCSDISK).
- [`../ENTER-DIRECTORY/README.md`](../ENTER-DIRECTORY/README.md) - device-agnostic mount path + `,X 14` hand-off (caller side).
