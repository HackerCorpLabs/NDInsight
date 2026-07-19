# SCSI-DISKLAYER-COMPLETE - the complete SCSI disk-layer transfer state machine

Byte-verified reverse-engineering of the **complete** SINTRAN III VSX/500 **L07**
SCSI disk-layer transfer state machine (`IP-P2-SCSI-DISK`: SCSDISK / SCSID /
INQUI / CACOB / fn-42), with **every path** and **every status/error exit**
disassembled and reconstructed. This consolidates and supersedes the partial
[`../FUNCTION-42-RETURN/`](../FUNCTION-42-RETURN/) and
[`../SCSDISK-TRANSFER/`](../SCSDISK-TRANSFER/) folders.

- **Segment:** `065-S3SIPIT` (load base `32000B`; byte-identical twin `066-S3IIPIT`).
- **Source bin:** `../../../segments/065-S3SIPIT.bin` (52224 big-endian words).
- **Disasm:** `nd100-dis -a -o -b 13312` over the byte-swapped image
  (whole-segment ref: `../../segments-ref/065-S3SIPIT/065-S3SIPIT.asm`).
- **dd offset:** `byte_off = (addr - 32000B) * 2` (decimal). Addresses/values **octal**.
- Evidence tags: **VERIFIED** = read from L07 bytes; **INFERRED** = NPL logic /
  architecture (`IP-P2-SCSI-DISK.NPL`, a DIFFERENT revision - naming only);
  **OPEN** = uncarved / runtime.
- Deliverables here: `SCSI-DISKLAYER-COMPLETE.ASM` (full commented disasm, every
  path) and `SCSI-DISKLAYER-COMPLETE.pseudo.c` (the whole state machine).

---

## 0. The +376B revision offset (re-confirmed)

The whole `IP-P2-SCSI-DISK` unit is at **NPL-source-label + 376B** in L07:

| Symbol | NPL label | L07 addr | delta | dd word |
|--------|-----------|----------|-------|---------|
| SCSDI (SCSDISK) | 056617 | **057215** | +376B | `146145` |
| SCSID  | 061621 | **062217** | +376B | `006010` |
| INQUI  | 062215 | **062613** | +376B | `044423` (also ptr `062412=062613`) |

The SCSI **driver** (`IP-P2-SCSI-DRIV`: `SCLLD=067160`, `INITO=070261`) is a
separate unit aligned to its own NPL labels (no +376B).

---

## 1. Function-code dispatch table (SCSID REPEAT, 062300-062331, VERIFIED)

`fn = ABFUN & 077` (`062300 SAA 77 / AND ,B 14`). Dispatch far-pointer table at
`062406-062425`.

| fn (octal) | meaning | SCSID branch | target (ptr) | onward |
|-----------|---------|--------------|--------------|--------|
| **0** | READ | fall-through `062326` | CACOB `063750` | CACOB->EXCOM->**SCLLD `067160`** (DMA read) |
| **1** | WRITE | fall-through `062326` | CACOB `063750` | build WRITE(6) CDB -> SCLLD |
| **4** | SEEK | fall-through `062326` | CACOB `063750` | -> SCLLD |
| 23 | MODE SELECT | `062320` | MODES `063244` | mode page |
| 25 | MODE SENSE | `062323` | MODES `063244` | mode page |
| 31 | BUS/DEVICE RESET | `062302-304` | BDRST `063522` | reset |
| 36 | READ LAYOUT | `062315` (also pre-handled in SCSDISK `057445`) | INQUI `062613` | geometry |
| 37 | DOEXS (special) | `062231-234` | DOEXS `063460` | execute-special |
| 42 | READ FORMAT | `062312`; SCSDISK clears 5SCIN at `057506` | INQUI `062613` | geometry |
| 74 | GET SCSI BUS | `062305-310` | GUSCB `063431` | then EXCOM |

Also: fn 37 short-circuits at SCSID entry (`062231`); a busy device (`SUTHS !=0`,
`062243`) queues the op and waits on SCWTI before any dispatch.
**fn 75 was NOT found in the SCSID REPEAT dispatch (OPEN).**

---

## 2. The RETRY / init / fall-through flow (VERIFIED)

```
SCSDISK 057215 (,X 14 entry)
  -> fn = ABFUN & 077 ; ILAOP guard (057233, T=4)
  -> geometry bounds check:
        whole-disk arm  057352-362 : end = ABPA2+ABP32 ; if end > UHLIM -> BADPA
        partition arm   057302-351 : several BADPA exits
     block 0 (ABPA2=0) is within bounds -> NOT BADPA
  -> NEWOP 057414 : SCSDISK-level fn pre-decode
        fn 6  -> init-call block ; fn 36 -> layout MOVEW ;
        fn 42 -> clear 5SCIN (057506) ; else fall through
  -> RETRY 057510
        057514/515  if (SUTYP & 5SCIN)  -> TRANSFER          (skip init)
        --- INIT block (5SCIN clear) ---
        057524/530  ABFUN := 42 or 36  (pick init read)
        057531/532  CALL SCSID (init read)                    [3-way return]
        057535-551  status check: D=HSTAT&~bit15 ;
                    D==6 or HSTAT==13 -> TACOU--, GO 057531 (retry in place) ;
                    D!=0 (other)      -> FINEX/terminate 057625 ;
        057552-556  DEVICE-TYPE GATE (see section 3)
        --- TRANSFER (real control-record READ) ---
        057557-604  build READ(6): ABP32:=1, LBA:=last block
        057610      CALL SCSID (real transfer)                [3-way return]
        057613-624  status check: D==6/HSTAT==13 -> TACOU--, JMP 057514
                    (OUTER WHILE back-edge) ; else -> FINEX 057655
  -> FINEX 057655 : XOR checksum + geometry publish + T:=0 (section 5)
```

The **outer WHILE** is the back-edge `057624 -> 057514` (re-reads SUTYP, re-gates
on 5SCIN); the **inner retry** for the init read is `057546 -> 057531`.

---

## 3. THE DECISION REGION 057531-057610 - what routes to error vs block-0 read

Carved instruction-by-instruction in the `.ASM`; the deciding sites:

| Site | Instruction (dd) | Condition | Route |
|------|------------------|-----------|-------|
| `057532` | `JPL I 77 -> 062217` (`135077`) | init CALL SCSID (3-way return) | `057533`=far ERREX, `057534`=alt, `057535`=status |
| `057535-536` | `RADD CLD SA DD` / `BSET ZRO 170 DD` | `D := HSTAT & ~bit15` | compute status D |
| `057537-546` | `SAT 6`/`SKP..`/`SAT 13`/`MIN ,X -15`/`JMP 057531` | `D==6 (UNIT ATTN)` OR `HSTAT==13 (ABORTED)` | **retry in place** (TACOU--) |
| `057547-551` | `SAT 1`/`SKP IF DT GRE SD`/`JMP 057625` | `D != 0` (any other nonzero status) | **FINEX/terminate** (not block-0) |
| `057552` | `LDA ,B 23` (`044423`) | `A := SUTYP` | device-type gate |
| `057553` | `SHA ZIN SHR 10` (`156570`) | `A := SUTYP >> 8` (device type) | " |
| `057554` | `JAZ 3 -> 057557` (`131003`) | **device type == 0 (DISK)** | **-> TRANSFER / block read** |
| `057555` | `SAT 1` (`171001`) | else (non-disk) | `T := 1 = TYPER` |
| `057556` | `JMP I 54 -> 057632` (`125054`) | | **-> ERREX (TYPER error exit)** |
| `057610` | `JPL I 21 -> 062217` (`135021`) | real transfer CALL SCSID | block-0/control READ enqueue |

**EXACT condition that decides block-0-read vs error:** after the INQUIRY init
read, control reaches the read only if **both**:
1. the init read status `D = HSTAT & ~bit15` is **0** (D==6/HSTAT==13 retry;
   any other D!=0 diverts to FINEX/terminate), AND
2. **`SUTYP >> 8 == 0`** at `057554` - i.e. the SCSI **device type byte is 0
   (Direct-Access / disk)**. Any nonzero device type takes `057555 T:=1 (TYPER)`
   and `057556 -> ERREX`.

For our device, INQUIRY byte0 = `00` (device type = disk) => `SUTYP>>8 == 0` =>
the gate **passes** and control proceeds to the control-record `READ(6)` at
`057610`. The gate does **not** divert a disk to an error exit.

---

## 4. INQUI SUTYP-high-byte construction (VERIFIED bytes; mapping INFERRED)

INQUI (`062613`) builds `SUTYP` from the INQUIRY response:

| Site | Instruction (dd) | Effect |
|------|------------------|--------|
| `062613-616` | `LDA ,B 23`/`AND -2`/`ORA 76`/`STA ,B 23` | re-init SUTYP base bits (preset the high byte before masking) |
| `062617-631` | build INQUIRY CDB | 6-byte INQUIRY |
| `062632` | `JPL I 63` (exec) | run INQUIRY -> DMA response into buffer |
| `062636-642` | `LDA ,B 13`/`AND 60`/`SKP..` | command-status gate (bits 4-5) |
| `062645-646` | `LDA I 55`/`EXR SA` | computed copy of INQUIRY bytes |
| `062647-651` | `LDT ,B 42`/`LDX ,B 43`/`AAX 70` | point at device-type word in the response |
| `062652` | `LDATX` (`143300`) | `A := response word` (device type in HIGH byte) |
| `062653` | `ORA 50` (`074050`) | OR low-byte mask (affects low byte only) |
| `062654-655` | `AND ,B 23`/`STA ,B 23` (`004423`) | **`SUTYP := response & SUTYP` -> SUTYP high byte := (preset_high AND device-type byte)** |
| `062656-657` | `SHA ZIN SHR 10`/`JAZ` | test device type; 0 -> disk path |
| `063101-103` | `LDA ,B 23`/`BSET ONE 70 DA`/`STA ,B 23` (`174275`) | **SET 5SCIN ("init done")** on success |

**SUTYP high-byte source (VERIFIED chain):** the INQUIRY-response word loaded at
`062652` (`LDATX`) carries the SCSI **device-type byte in its high byte**; it is
masked into `SUTYP` at `062654-655` (`AND ,B 23 / STA ,B 23`, dd `004423`), so
`SUTYP` bits 8-15 end up equal to the device-type byte. That is exactly the byte
the `057553/057554` gate tests. With INQUIRY byte0 = `00`, `SUTYP>>8 == 0` and
the gate passes.

- **VERIFIED:** the load (`143300`), the mask-into-SUTYP store (`004423`), and
  that SUTYP high byte is what both `062656` and `057553` shift-test.
- **INFERRED:** that the specific word at `(field43 + 70)` is the INQUIRY byte0
  device-type packed high; the exact response-buffer layout / byte packing is
  the SCSI driver's (`IP-P2-SCSI-DRIV`) DMA convention, not carved here. The
  device's INQUIRY `00 00 05 01 34 00 00 00` (byte0=00=disk) is consistent with
  the disk taking the `JAZ`-passes path.

---

## 5. All error / terminate exits (VERIFIED sites)

| Code (T) | Symbol | Exact condition | Byte site |
|----------|--------|-----------------|-----------|
| **0** | OK | control record read, XOR checksum = 0, `2 < NPART <= 010` | `057707-716` -> `057747 T:=0` |
| **1** | TYPER | init read OK but `SUTYP>>8 != 0` (non-disk device type) | `057554-556` (gate) / also INQUI classify `062660-665` |
| **4** | ILAOP | illegal operation (e.g. non-direct-access request) | `057233 SAT 4` |
| **5** | BADPA | `ABPA2+ABP32 > UHLIM` (whole-disk) or partition out of range | `057362 -> 057377 -> 057401 SAT 5`; partition arm `057312/057323/057326/057350` |
| **11** | NOCRC | control-record XOR `!= 0`, or `NPART <= 2`, or `NPART > 010` | `057707-720` |
| (6) | UNIT ATTN | init/transfer status `D == 6` | retried (`057540-546`, `057616-624`) - not returned |
| (13) | ABORTED | init/transfer status `HSTAT == 13` | retried - not returned |
| (50/43/51) | SBRST/LIRST/PFAIL | bus reset / power fail at ERREX | `057636-650` `MIN TACOU; GO 060005` - retried |
| - | RETEX/RETOP | normal terminate (carries T) | `057750-755 JPL 060012` |

Retry paths: **inner** init retry `057546 -> 057531`; **outer WHILE**
`057624 -> 057514`; **hard** bus-reset retry `057650 -> 060005`. All decrement
`TACOU` (`MIN ,X -15`); exhaustion terminates with the pending T code.

---

## 6. Does OUR mount's block-0 attempt read or error?

For our run (`blockSize=1024`; INQUIRY device type = `00` = disk; control record
valid `NPART=8`, XOR=0; `UHLIM=121560`):

1. **fn-42 READ FORMAT** clears 5SCIN (`057506`), runs INQUIRY+READ CAPACITY,
   passes the device-type gate (`057554`, high byte 0 = disk), reads the
   control record, checksum passes, publishes geometry, returns **`T=0`**
   (`057747`), and leaves **5SCIN SET** (`063102`).
2. When the filesystem later issues the separate **fn-0 READ of block 0** via
   `,X 14 -> SCSDISK`, `ABPA2 = 0` -> within `UHLIM` -> **no BADPA**
   (`057361/057362` not taken); SCSID falls through (`062326`) to CACOB; the
   `063752` 5SCIN gate is **SET** -> builds READ(6) -> EXCOM -> **GO SCLLD
   `067160`**. **Block 0 reaches the driver enqueue.**

So nothing in the carved disk layer routes a well-formed block-0 read to an
error exit or a skip. The **only** disk-layer reject for block 0 would be
`BADPA` (`057401`), which requires `address + amount > UHLIM` - impossible for
`address = 0`. The device-type gate (`057554`) only rejects **non-disk** device
types, and our device reports `00` (disk).

**Therefore the block-0 skip is NOT caused by the SCSI disk layer** - it is
upstream, in the **uncarved device-agnostic connect/mount overlay** that
consumes fn-42's output (`UHLIM` / partition table / status `36`) and is
responsible for issuing the block-0 request. This matches the ground truth
(`SCWAQ` empty; `SCLLD` never called for block 0; silence after the
control-record `READ(6)`).

### The ONE value to read live (settling runtime check)
- Break **`SCSID 062217B`** and the **5SCIN gate `063752B`**:
  - If a **second** `SCSID` entry with `(ABFUN & 077) == 0` and a block-0
    address arrives, watch it fall `062326 -> CACOB -> 063752 (5SCIN SET) ->
    EXCOM 063403 -> SCLLD 067160`. If it instead errors, dump `ABPA2`/`ABP32`
    vs `UHLIM` - a mis-scaled LBA takes `057362 -> ERR2 -> BADPA (T=5)`.
  - If `SCSID` is **never re-entered** for block 0, the caller never issued it
    -> carve the connect/mount overlay next, not the disk layer.
- The distinguishing read: at the break, dump the fn-42 output buffer
  (`SLINK.MEMA1/MEMA2` = `UHLIM` + status `36`) to confirm the caller received
  sane geometry it could turn into a valid block-0 LBA.

---

## 7. dd spot-checks (raw big-endian `065-S3SIPIT.bin`)

All re-read with `dd if=065-S3SIPIT.bin bs=1 skip=<off> count=2 | od -An -tx1`
(`off = (addr-32000B)*2`), big-endian word = octal:

| Item | addr | off | hex | oct | mnemonic |
|------|------|-----|-----|-----|----------|
| SCSDISK entry | `057215` | 21786 | `cc65` | `146145` | `RADD CLD SL DA` |
| whole-disk bounds cmp | `057361` | 21986 | `c228` | `141050` | `SKP IF 0 GRE SA` |
| BADPA reject | `057401` | 22018 | `f205` | `171005` | `SAT 5` (T:=BADPA) |
| fn-42 gate | `057472` | 22132 | `f222` | `171042` | `SAT 42` |
| CLEAR 5SCIN | `057506` | 22156 | `f83d` | `174075` | `BSET ZRO 70 DA` |
| gate: LDA SUTYP | `057552` | 22228 | `4913` | `044423` | `LDA ,B 23` |
| gate: SUTYP>>8 | `057553` | 22230 | `dd78` | `156570` | `SHA ZIN SHR 10` |
| gate: JAZ disk | `057554` | 22232 | `b203` | `131003` | `JAZ 3 -> 057557` |
| gate: TYPER | `057555` | 22234 | `f201` | `171001` | `SAT 1` |
| gate: err jmp | `057556` | 22236 | `aa2c` | `125054` | `JMP I 54 -> 057632` |
| transfer CALL | `057610` | 22288 | `ba11` | `135021` | `JPL I 21 -> 062217` |
| fn-42 success | `057747` | 22478 | `cc46` | `146106` | `RADD CLD 0 DT` (T:=0) |
| SCSID entry | `062217` | 24862 | `0c08` | `006010` | `STA ,X 10` |
| fn-0 fall-through | `062326` | 25004 | `ba36` | `135066` | `JPL I 66 -> CACOB` |
| INQUI ptr | `062412` | 25108 | `658b` | `062613` | ptr = INQUI |
| INQUI entry | `062613` | 25366 | `4913` | `044423` | `LDA ,B 23` |
| INQUI resp load | `062652` | 25428 | `c6c0` | `143300` | `LDATX` |
| SUTYP hi store | `062655` | 25434 | `0913` | `004423` | `STA ,B 23` |
| SET 5SCIN | `063102` | 25732 | `f8bd` | `174275` | `BSET ONE 70 DA` |
| CACOB entry | `063750` | 26576 | `cc65` | `146145` | `RADD CLD SL DA` |
| 5SCIN gate | `063752` | 26580 | `4913` | `044423` | `LDA ,B 23` |

---

## 8. VERIFIED / INFERRED / OPEN

| # | Claim | Verdict |
|---|-------|---------|
| 1 | Whole `IP-P2-SCSI-DISK` at L07 = NPL + 376B (SCSDI 057215, SCSID 062217, INQUI 062613) | VERIFIED (symbols + dd) |
| 2 | SCSID dispatch table (fn 0/1/4 -> transfer; 31 BDRST; 36/42 INQUI; 23/25 MODES; 37 DOEXS; 74 GUSCB) | VERIFIED (062300-331 + ptr table 062406-425) |
| 3 | Whole-disk bounds `ABPA2+ABP32 > UHLIM -> BADPA (T=5)` at 057356-362/057401 | VERIFIED (dd) |
| 4 | fn-42 clears 5SCIN (057506); RETRY runs INQUIRY when 5SCIN clear (057515) | VERIFIED (dd) |
| 5 | Init read 3-way CALL SCSID (057532) + status check (D==6/13 retry, D!=0 divert) | VERIFIED |
| 6 | DEVICE-TYPE GATE: `SUTYP>>8 == 0` -> READ (057554); else TYPER (T=1) -> ERREX (057555-556) | VERIFIED (dd every word) |
| 7 | Real transfer CALL SCSID at 057610; outer WHILE back-edge 057624 -> 057514 | VERIFIED (dd) |
| 8 | INQUI builds SUTYP; response word masked into SUTYP high byte at 062652-655 | VERIFIED (dd 143300/004423) |
| 9 | The masked-in high byte == INQUIRY device-type byte0 (buffer packing) | INFERRED (driver DMA layout not carved) |
| 10 | INQUI SETS 5SCIN on success (063102) | VERIFIED (dd 174275) |
| 11 | FINEX success: XOR=0 & 2<NPART<=010 -> publish geometry + T:=0 (057747) | VERIFIED |
| 12 | Error map: OK=0/TYPER=1/ILAOP=4/BADPA=5/NOCRC=11; 6/13 retried; 50/43/51 bus-reset retry | VERIFIED (sites) |
| 13 | For our run: device type 0 (disk) -> gate passes -> block-0 read reaches SCLLD | VERIFIED (bytes) + ground-truth trace |
| 14 | Block-0 skip is NOT decided in the disk layer; it is the uncarved mount overlay | VERIFIED (no such branch) / OPEN (exact upstream instruction) |
| 15 | fn 75 in SCSID dispatch | OPEN (not present in REPEAT) |

**Provenance.** Carved `065-S3SIPIT` L07 bytes (base `32000B`); symbols
`SINTRAN/NPL-SOURCE/SYMBOLS/L07/SYMBOL-2-LIST.SYMB.TXT` (SCSDI/SCSID/SCLLD) and
`FILSYS-SYMBOLS`/`SYMBOL-1-LIST` (SUTYP=23, 5SCIN=7, UHLIM=32, SCWAQ). Logic
(different revision, INFERRED naming only):
`SINTRAN/NPL-SOURCE/NPL/IP-P2-SCSI-DISK.NPL` (SCSDISK / SCSID / INQUI / CACOB /
EXCOM / FINEX / RETEX). Ground truth: INQUIRY + READ CAPACITY + one
control-record READ(6), then silence; `SCWAQ` empty.

## See also
- [`../FUNCTION-42-RETURN/README.md`](../FUNCTION-42-RETURN/README.md) - fn-42 return values + 5SCIN state machine (predecessor; consolidated here).
- [`../SCSDISK-TRANSFER/README.md`](../SCSDISK-TRANSFER/README.md) - fn-0 block-0 path to SCLLD (predecessor; consolidated here).
- [`../SCSI-DRIVER/README.md`](../SCSI-DRIVER/README.md) - driver core `SCLLD -> INITO -> SCWAQ` (the enqueue this path lands in).
- [`../ENTER-DIRECTORY/README.md`](../ENTER-DIRECTORY/README.md) - device-agnostic mount path + the `,X 14` hand-off (the caller side / uncarved decision).
- [`../RCBLO/README.md`](../RCBLO/README.md) - cache-block dispatcher (caller of SCSDISK).
