# SMD / Winchester / ST506 disk driver - L07 baseline carve

Carved analysis of the **working** mass-storage disk driver that
`@ENTER-DIRECTORY` uses successfully on SMD / Winchester / ST506 / cartridge
disks, so it can be compared against the SCSI driver (which fails the same
device-agnostic "read block 0" request).

- Version: SINTRAN III **L-VSX-500** (running system), symbols **L07**.
- Carver base dir:
  `../../../segments/` (relative to this folder).
- Ground truth = carved bytes of `065-S3SIPIT.bin`. NPL
  (`SINTRAN/NPL-SOURCE/NPL/IP-P2-DISK-START.NPL`) is a different revision and
  is used for **naming/logic only**, never as authoritative bytes.

---

## 1. Where the SMD/Winchester driver lives  (VERIFIED)

**Segment `065-S3SIPIT` ("Save of IPIT"), load base `032000B` (13312 dec).**
Byte-identical to `066-S3IIPIT` ("Image of IPIT") - `cmp` = 0 diffs.

Symbols come from **`SYMBOL-2-LIST.SYMB.TXT` (L07)** - the resident
device-driver overlay. This ONE overlay holds BOTH drivers, at
non-overlapping addresses inside the same carved segment:

| Driver            | address span (L07) | key symbols |
|-------------------|--------------------|-------------|
| SMD / ST506 (this)| ~`054000B..057200B`| `CTRDI 054305`, `MDRIV 055015`, `BSEEK 056011`, `WSEEK 056072`, `STRDI 056266`, `SSTDI 056504` |
| SCSI (`../SCSI-DRIVER/`) | ~`057215B..070xxxB` | `SCSDI 057215`, `SCLLD 067160`, `INITO 070261` |

### Coherence evidence (why 065-S3SIPIT, not another overlay)

The big resident segments all overlay the same virtual window, so an address
"disassembling to something" proves nothing. These SMD symbols land on
**exact NPL-matching entries** only in `065-S3SIPIT` at base `032000B`:

| Symbol | Addr | Carved bytes decode to | NPL match |
|--------|------|------------------------|-----------|
| `BSEEK` | `056011B` | `T:=X; X:=X.SLINK(4); A:=X.TYPCO(11); X:=T; IF A NBIT SSEEK EXIT; IF TRNSF><BDISK EXIT` | `BSEEK: T:=X; A:=X.SLINK.TYPCO; X:=T; IF A NBIT SSEEK THEN EXIT; IF "TRNSF"><"BDISK" THEN EXIT` - exact |
| `WSEEK` | `056072B` | `A:=L =: CTRLR(40)`, spin `IOXT` status, `A\/030005; IOXT`, `CALL ID11` | `WSEEK: A:=L=:"CTRLR" ... A \/ 030005; *IOXT; CALL ID11` - exact |
| `SSTDI` | `056504B` | `135413 = JPL I ,B 13 = CALL DODMA` (DODMA = datafield word 13) | `SSTDI: CALL "DODMA"` - exact |
| fn-42 gate | `054464B` | `SAT 42; SKP IF A=42; LDA TRNSF; LDT BDISK; SKP IF =; JMP I 44 (FIN)` | `IF A=42 AND "TRNSF"><"BDISK" GO FAR FIN` - exact |

This is the **same segment** in which the prior SCSI carve verified
`SCLLD@067160B` (bytes `cc69 dcf8 f203 c631 a80e`). Both halves of
`SYMBOL-2-LIST` are coherent here; other overlays that map `056011B` decode to
unrelated loop/data bytes.

### dd byte proof (SMD anchors)

`065-S3SIPIT.bin`, base `032000B`; byte offset = `(addr - 032000B) * 2`:

| addr | byte off | word | bytes (hi lo) |
|------|----------|------|---------------|
| `056011` | 20498 | `146176` (RADD CLD SX DT, `T:=X`) | `cc 7e` |
| `056012` | 20500 | `056004` (LDX ,X 4, `X:=X.SLINK`)  | `5c 04` |
| `056013` | 20502 | `046011` (LDA ,X 11, `A:=X.TYPCO`) | `4c 09` |
| `056072` | 20596 | `146145` (RADD CLD SL DA, `A:=L`)  | `cc 65` |
| `056073` | 20598 | `004440` (STA ,B 40, `=:CTRLR`)    | `09 20` |
| `056504` | 21128 | `135413` (JPL I ,B 13, `CALL DODMA`)| `bb 0b` |
| `054464` | 19048 | `171042` (SAT 42)                  | `f2 22` |
| `054472` | 19060 | `125044` (JMP I 44 -> FIN)         | `aa 24` |

Reproduce, e.g.:
```
dd if=065-S3SIPIT.bin bs=1 skip=20498 count=2 | od -An -tx1     # -> cc 7e
```

---

## 2. How a block-0 read flows on the WORKING (SMD) path

The path ABOVE the driver is device-agnostic and identical for every disk:

```
@ENTER-DIRECTORY -> ENDIR -> CHDSI -> RXDIR(block:=0) -> RCBLO
   RCBLO: LDX ,B 11 (unit df) ; LDA ,X 14 (transfer pointer) ; STA ,B 10
          JPL I ,B 10                         <- runtime dispatch into driver
```

For an SMD/Winchester unit the datafield word `,X 14` points at the SMD
transfer chain. Inside the driver (`STRDISK 056266B`):

1. `CTRDISK` decodes the function code; function 0 (READ) is a normal transfer.
2. Optional `BSEEK` (056011) starts a seek if `SSEEK` set.
3. `WSEEK` (056072) waits for seek-complete via controller status `IOXT` +
   `CALL ID11` on the seek interrupt.
4. `SSTDI` (056504) `CALL DODMA` (`JPL I ,B 13`) programs the DMA controller
   and reads **that one physical block**.

Block 0 is **not special-cased**. The block number (0) is already in the
datafield when `RCBLO` dispatches; no capacity probe, no geometry lookup, no
LBA computation - the driver seeks and DMAs the physical block directly. That
is why the working baseline never stalls on "read block 0".

---

## 3. Function 42 (READ FORMAT) - SMD is a near-no-op   (VERIFIED gate)

`CTRDISK` function-decode at `054463B` (byte-verified):

```
054463 LDX I -162        ; X := &CTRG (function/control record)
054464 SAT 42            ; T := 42B  (READ FORMAT)
054465 SKP IF DA EQL ST  ; IF function == 42 fall through
054466 JMP 054473        ;   else normal transfer decode
054467 LDA ,B -12        ; A := TRNSF (device-type tag)
054470 LDT 45            ; T := "BDISK"
054471 SKP IF DA EQL ST  ; IF TRNSF == BDISK skip the FIN jump
054472 JMP I 44          ;   TRNSF <> BDISK -> GO FAR FIN (read format illegal)
```

- If `TRNSF <> BDISK`: READ FORMAT is not legal in this driver -> `GO FIN`
  (VERIFIED).
- If `TRNSF == BDISK`: return a **format number from an internal table into
  the first location of the DMA buffer, with NO disk I/O**, then terminate
  (INFERRED from NPL; the FIN cleanup is at `054536B`, `STF ,X 2`; the exact
  table-to-buffer[0] store is not pinned to one verified word - see OPEN).

No `INQUIRY`, no `READ CAPACITY`, no control-record read. Function 42 for SMD
does not talk to the disk at all.

---

## 4. Side-by-side: SMD (working) vs SCSI (failing)

| Aspect | SMD / Winchester / ST506 (this carve) | SCSI (`../SCSI-DRIVER/`) |
|--------|----------------------------------------|--------------------------|
| (a) **Function 42 (READ FORMAT)** | Near no-op. `TRNSF==BDISK`: return format number from a table into DMA-buffer word 0, NO disk I/O, terminate. `TRNSF!=BDISK`: `GO FIN` (illegal). VERIFIED gate @054463. | Real device I/O: `INQUIRY` + `READ CAPACITY` + one control-record `READ(6)`; returns `UHLIM` (last LBA) + partitions; issues **no** block-0 read and chains none. VERIFIED in SCSI carve. |
| (b) **How block 0 is read** | `RCBLO` dispatches `,X 14`; `CTRDISK->STRDISK->BSEEK/WSEEK->CALL DODMA` DMAs the single physical block. Block number (0) taken from datafield; no capacity/geometry needed. VERIFIED chain. | `,X 14 -> SCSDISK -> SCLLD -> INITO -> SCWAQ`; LBA derived from the `READ CAPACITY` result / block-size shift. If capacity/geometry mis-scaled, or `SCLLD` is never called, the page-0 read is never issued. |
| (c) **Device-type branch** | Explicit `IF fn==42 AND TRNSF><BDISK GO FIN` at `054464-054472` (VERIFIED). `BSEEK` also gates parallel seek on `TRNSF==BDISK` (056020-056024, VERIFIED). | SCSI is selected by the unit's `,X 14` transfer pointer binding (device config), landing in `SCSDISK` instead of `STRDISK`. |
| **Net divergence** | fn 42 is a table lookup; block 0 is a direct physical DMA. Nothing depends on a prior capacity/geometry probe -> robust. | fn 42 performs the geometry probe (READ CAPACITY -> UHLIM) but does NOT read block 0; a later, separate `SCSID/SCLLD` call must issue block 0, and that is where the SCSI mount path breaks. |

**Concrete answer to the task:** on the SMD path the "read format" (fn 42)
request is answered from a table with zero disk traffic, and the subsequent
"read block 0" (fn 0) is a plain seek+DMA of the physical block using the
block number the filesystem already supplied. The SCSI driver instead folds a
real `READ CAPACITY` geometry probe into fn 42 and makes the block-0 read
depend on that probe's result via `SCLLD -> INITO -> SCWAQ`; when the probe
result is mis-scaled or `SCLLD` is not (re)called, block 0 is never fetched and
`@ENTER-DIRECTORY` fails.

---

## 5. VERIFIED / INFERRED / OPEN

| # | Claim | Status |
|---|-------|--------|
| 1 | SMD driver lives in `065-S3SIPIT` (=`066-S3IIPIT`), base `032000B`; symbols from `SYMBOL-2-LIST` (L07) | VERIFIED (bytes + NPL match at BSEEK/WSEEK/SSTDI/fn-42) |
| 2 | `BSEEK@056011`: `T:=X; X:=X.SLINK; A:=X.TYPCO; X:=T; IF A NBIT SSEEK EXIT; IF TRNSF><BDISK EXIT` | VERIFIED (dd `cc7e 5c04 4c09 cc77`) |
| 3 | `WSEEK@056072`: `A:=L=:CTRLR(40)`; status-poll `IOXT`; `A\/030005; IOXT`; `CALL ID11` | VERIFIED (dd `cc65 0920`) |
| 4 | `SSTDI@056504` = `JPL I ,B 13` = `CALL DODMA` (DODMA = datafield word 13) | VERIFIED (dd `bb0b`) |
| 5 | Function-42 gate `IF fn==42 AND TRNSF><BDISK GO FIN` | VERIFIED (dd @054464 `f222`, @054472 `aa24`) |
| 6 | fn 42 issues NO disk I/O (no seek, no DODMA) on the FIN path | VERIFIED (FIN @054536 has no IOX/DODMA before terminate) |
| 7 | Block 0 read = normal `CTRDISK->STRDISK->DODMA` DMA of the physical block; block number from datafield; no capacity probe | VERIFIED (transfer chain) + INFERRED (NPL for full block set-up) |
| 8 | fn 42 for `TRNSF==BDISK` returns a format number from a table into DMA-buffer word 0 | INFERRED (NPL); exact table->buffer[0] store not pinned to one verified word |
| 9 | `MDRIV@055015` / `CTRDI@054305` are the driver main / controller-init entries | VERIFIED code (coherent), function-by-function mapping partial - OPEN |
| 10| SCSI comparison figures (READ CAPACITY / SCLLD / SCWAQ) | from `../SCSI-DRIVER/` (VERIFIED there) |

**OPEN / next:**
- Pin the exact instruction(s) that copy the format-number table entry into
  DMA-buffer word 0 on the `TRNSF==BDISK` fn-42 path (item 8).
- Full opcode-by-opcode map of `MDRIV`/`CTRDISK`/`DODMA` bodies if a complete
  SMD driver listing is wanted (item 9).

---

## Cross-links

- Device-agnostic caller: `../ENTER-DIRECTORY/`, `../RCBLO/`
  (`,X 14` datafield transfer dispatch, `JPL I ,B 10`).
- SCSI counterpart: `../SCSI-DRIVER/` (`SCLLD 067160B`, `SCSDI 057215B`,
  function-42 = INQUIRY+READ CAPACITY+control-record READ).
- Function-42 return on the filesystem side: `../FUNCTION-42-RETURN/`.
- Source segment: `../../../segments/065-S3SIPIT.bin` (= `066-S3IIPIT.bin`),
  base `032000B`; symbols `SINTRAN/NPL-SOURCE/SYMBOLS/L07/SYMBOL-2-LIST.SYMB.TXT`.
- NPL logic (different revision, naming only):
  `SINTRAN/NPL-SOURCE/NPL/IP-P2-DISK-START.NPL`.
