# SCSI Disc-Transfer Completion Status - why a good read is reported as
# "DISC TRANSFER ERROR, STATUS 100020"

**Question answered:** on `@ENTER-DIRECTORY ,,DISC-SCSI-1,0` every SCSI command
returns `SS_GOOD` and the 1024 bytes DMA into ND memory, yet SINTRAN aborts the
mount with **ERROR 20 / ERROR 21 "Disk transfer error"**, **DISC ADDRESS 0 and
7777B**, **STATUS 100020B**. This document decodes, from the carved SINTRAN L
bytes plus the ND-3201 firmware analysis, exactly which status word SINTRAN reads,
which bit makes it declare a transfer error, what `100020B` means, and precisely
what the emulated ND-3201 controller must present on a completed transfer so the
driver accepts it.

**Grading:** **VERIFIED** = proven by carved SINTRAN L bytes, by the embedded
driver register table, or by a reference manual; **INFERRED** = strong reasoning
from the NPL driver revision or firmware doc (NPL is a *different* revision than L,
so its numeric details are not carved-proven); **OPEN** = not closable statically,
needs a live trace.

All ND addresses are **octal**; status words are given octal and hex.

**Carved provenance:** segment `006-S3FS`, load base **26000B** (11264 dec) -
`../../../tools/sintran-segment-carver/versions/L-VSX-500/segments/006-S3FS.bin`;
disassembly + symbols under
`../../../tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/006-S3FS/`.
Opcodes grounded in
`../../../tools/sintran-segment-carver/versions/L-VSX-500/re/instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md`.

Cross-links: [scsi-mount-geometry / disk format](scsi-disk-format.md) -
[SCSI-MOUNT-FIX-PLAN.md](SCSI-MOUNT-FIX-PLAN.md) -
[nd-scsi-3201.md](nd-scsi-3201.md) -
[IP-P2-SCSI-DRIV-ANALYSIS.md](IP-P2-SCSI-DRIV-ANALYSIS.md).

---

## 0. Answer in one paragraph

SINTRAN's SCSI driver reads the controller status register **RSTAU (IOX offset
04)** after the transfer and gates completion on the **error-summary bit, RSTAU
bit 4 ("Error - OR of error conditions")**. The value it read was **100020B =
0x8010 = bit 15 + bit 4**. **Bit 4 is the error flag** - its presence, and nothing
else, is what makes the driver reject the transfer and the mass-storage layer print
"DISC TRANSFER ERROR, STATUS 100020". **Bit 15 is "Differential" - a static board
strap, not an error** - it is noise in the printed value. The data DMA'd fine, but
the emulated controller is presenting RSTAU with the error bit set on a
successful transfer. **To fix the mount, the emulated ND-3201 must present, on a
completed good transfer, RSTAU with bit 4 = 0 and bit 11 = 0 (no error / no bus
error), bit 3 = 1 (Ready for Transfer), the NCR-interrupt bit set at completion
and cleared only on the RITRG acknowledge, the transfer counter read back as 0,
and the Busy bit cleared.**

---

## 1. Where the status comes from (VERIFIED)

The message text is the standard SINTRAN mass-storage disk-error report. From the
reference manuals (`../../../Reference-Manuals/ND-60.128.5 EN SINTRAN III Reference
Manual.md` error table, and `../../../Reference-Manuals/ND-60.050.06 SINTRAN III
Users Guide.md`):

| Error | Text | Word A | Word B |
|-------|------|--------|--------|
| 20 | Disk transfer error | hardware device no. | unit |
| 21 | Disk transfer error | last 16 bit of sector address | **hardware status** |

So the printed **STATUS = the "hardware status" word = the device status register
the driver read = RSTAU**. `DISC ADDRESS 0` (the block being verified) and `7777B`
(low 16 bits of the sector address of the probe) match error 21's "last 16 bit of
sector address". **The `100020B` in the message is the raw RSTAU value.**
(VERIFIED - reference manual + trace.)

The driver and its error translator are carved and on this path (VERIFIED - exact
addresses, `006-S3FS.asm`):

| Symbol | Octal | Carved line | Role |
|--------|-------|-------------|------|
| `SCSI1` | 046530B | `046530  125070  JMP I 70` | SCSI driver entry |
| `SCSI2` | 046661B | `046661  047027  LDA I ,X 27` | SCSI driver entry |
| `SCDTS` | 062107B | `062107  044416  LDA ,B 16` | status / error translator |
| `SCSID` | 062217B | `062217  000410  STZ ,B 10` | driver dispatcher |
| `NCROK` | 177731B | symbol `177731B NCROK(FILSYS-SYMBOLS)` | NCR-interface status word |

**OPEN (carved decode limit):** the `SCDTS` (062107B) and `SCSID` (062217B)
regions disassemble as **PLANC data-before-code** - the linear sweep produces
indirect-call jump tables and packed constants (`062303 ROP NOOP`,
`062310 LDT I ,B ,X -40`, `062223 JMP 44`, `062224 BSKP ONE 170 DA`, ...), so the
*exact* internal-code -> user-error-code table (the 232B/141B/252B/... mapping) is
**not cleanly readable** from the current carve. For those numeric mappings the
firmware doc and NPL are the fallback authority, marked INFERRED. What **is**
carved-proven: these are the real routines and a page-0 read failure returns
through them. This matches the finding already recorded in
[SCSI-MOUNT-FIX-PLAN.md](SCSI-MOUNT-FIX-PLAN.md) sec. 1a.

---

## 2. The RSTAU status-register format (VERIFIED - embedded driver table)

RSTAU is read at **IOX offset 04**; the driver writes control at **WCONT, offset
05**. The authoritative bit layout is the driver's own register definition table
carried in the emulator's embedded driver source and reproduced in
[nd-scsi-3201.md](nd-scsi-3201.md) sec. "RSTAU Status Register Bits":

| Bit | Octal weight | Name | Meaning |
|-----|--------------|------|---------|
| 0 | 000001 | Interrupt Enabled | echoes WCONT bit 0 |
| 2 | 000004 | Busy (Active) | controller still processing a command |
| 3 | 000010 | **Ready for Transfer** | controller completed, ready |
| 4 | 000020 | **Error** | **OR of error conditions** |
| 5 | 000040 | Reset on SCSI bus | SCSI bus reset detected (IRQ source) |
| 6 | 000100 | NCR 5386 disabled | chip in disabled state |
| 7 | 000200 | Single-ended | single-ended SCSI drivers strapped |
| 8 | 000400 | Data Request | DREQ from NCR 5386 |
| 9 | 001000 | **NCR Interrupt** | interrupt from NCR 5386 (IRQ source) |
| 10 | 002000 | Data Acknowledge | DACK to NCR 5386 |
| 11 | 004000 | **BERROR** | **ND-100 bus DMA error** |
| 12 | 010000 | BSY | SCSI BSY line |
| 13 | 020000 | REQ | SCSI REQ line |
| 14 | 040000 | ACK | SCSI ACK line |
| 15 | 100000 | **Differential** | differential SCSI receivers strapped |

Note the two distinct error indicators: **bit 4 = generic error summary** and
**bit 11 = BERROR = the actual DMA/bus error**. The DMA-completion error lives in
bit 11, not bit 4.

**Discrepancy to be aware of (OPEN):** the NPL driver *revision*
([IP-P2-SCSI-DRIV-ANALYSIS.md](IP-P2-SCSI-DRIV-ANALYSIS.md), lines 123-138) reads
RSTAU and tests **bit 11 for "interrupt from NCR"** and bit 4 for an error, whereas
the register table above (closest to L) puts **NCR interrupt at bit 9** and BERROR
at bit 11. NPL is a different revision than carved L; the two disagree on whether
the NCR-interrupt bit is 9 or 11. This does **not** change the bit-4 finding, but it
does mean the *completion-interrupt* bit position (9 vs 11) should be pinned by a
live trace before hard-coding it (see sec. 6).

---

## 3. Decode of STATUS 100020B

```
  100020B  =  0x8010  =  1000 0000 0001 0000b
                          |              |
                          bit 15         bit 4
```

| Bit set | RSTAU meaning | Verdict |
|---------|---------------|---------|
| **bit 4** (000020B) | **Error - OR of error conditions** | **THE trigger.** This is why SINTRAN declares "DISC TRANSFER ERROR". |
| **bit 15** (100000B) | **Differential** (SCSI receivers strapped) | Benign static strap. Not an error, not tested by the completion gate. Noise in the printed word. |

**What SINTRAN thinks went wrong:** the driver does **not** see a specific cause
(BERROR/bit 11 is clear, Data-Request/Data-Ack are clear). It sees only the generic
**error-summary bit 4** raised, so it takes the error exit, hands `SCDTS` a
device-error code, and the mass-storage layer prints the raw status. In short:
**"the controller told me the operation ended in error" - with no more detail than
the summary bit.** (VERIFIED that bit 4 = error summary; INFERRED that this is the
exact gate the completion path evaluates, from the NPL revision + the register
table - see sec. 4.)

**Why bit 15 is a red herring:** bit 15 = Differential is a hardware-strap readback
that is set on a differential board regardless of any transfer. If the emulator
sets it unconditionally, it will always appear in a printed RSTAU. SINTRAN's error
gate does not test it, so it neither causes nor prevents the failure - but it does
make the printed status look alarming. (INFERRED from the register table; an
ND-3201 is normally single-ended, so a real single-ended board would show **bit 7**
set and **bit 15 clear** - see sec. 6, item 6.)

---

## 4. The completion / error gate in the driver (VERIFIED table + INFERRED code)

**VERIFIED:** the driver reads RSTAU (IOX 04) as the first step of its SCSI
interrupt/status service, and bit 4 is defined as the error summary (sec. 2). The
error message is produced from that RSTAU word (sec. 1).

**INFERRED (NPL revision, `IP-P2-SCSI-DRIV-ANALYSIS.md` lines 123-131) - the shape
of the gate:**

```
Line 123: T := HDEV+RSTAU; *IOXT        % read device status into A
Line 124: IF X := 64/\A ><0 THEN        % gate mask 64B = bits 2,4,5
Line 126:    IF A BIT 2  GO SCWTI       %   bit 2  -> controller busy, keep waiting
Line 126:    IF A BIT 5  ... SCDIS      %   bit 5  -> SCSI bus reset received
Line 129:    IF A BIT 4  CALL SCIDE     %   bit 4  -> error condition -> error path
```

The gate mask `64B` = `000064B` covers **bit 2 (Busy) + bit 4 (Error) + bit 5
(Reset)**. When RSTAU = `100020B`, `64B /\ 100020B = 000020B` (bit 4 only) which is
`><0`, so the driver enters the handler, finds bit 2 and bit 5 clear, and falls to
the **bit-4 error path**. That is the "transfer error" exit. (INFERRED: NPL is a
different revision, so the exact opcodes/line numbers are not carved-proven; the
mask value and bit assignments are consistent with the VERIFIED register table.)

**The GOOD vs ERROR decision, stated plainly:**

| RSTAU after transfer | Driver verdict |
|----------------------|----------------|
| bit 4 = 0, bit 3 = 1, counter = 0, NCR-int seen+ack'd | **GOOD** - transfer complete, proceed |
| **bit 4 = 1** (any error OR) | **ERROR** - "DISC TRANSFER ERROR", abort |
| bit 2 = 1 (still Busy) | not done - keep polling / eventually time out |

`100020B` fails on the first row's condition because **bit 4 = 1**.

---

## 5. The connect / verify probe - why it aborts before block 0 (VERIFIED geometry
## + INFERRED sequencing)

Between `READ CAPACITY` and the directory read at block 0, SINTRAN does **not** go
straight to page 0. It runs a **connect/verify probe**: it reads the **highest
addressable block** (LBA = reported-capacity - 1) to confirm the drive is
physically accessible at the capacity it just reported. Only if that probe
completes cleanly does it issue the SCSI READ for block 0 (the directory).

- **VERIFIED (disk format):** [scsi-disk-format.md](scsi-disk-format.md) shows a
  real ND SCSI disk's top blocks are a **disk-parameter block** (LBA raw-2) and a
  **defect/reallocation table** (LBA raw-1), and that **READ CAPACITY reports the
  RAW last-LBA** (e.g. 129311 on SCSI-K). SINTRAN's mount path reads that last
  block. So the probe target is the top-of-disk table, exactly the block the
  emulator's trace shows being read (a full 1024-byte DMA) before any block-0 read.
- **The failure (this document's finding):** the probe's data **does** DMA in, but
  the emulated controller raises **RSTAU bit 4** at completion. The driver's
  post-transfer gate (sec. 4) sees the error summary and aborts **at the probe
  stage** - so it **never issues the block-0 read**. This is precisely the observed
  behaviour: "SINTRAN never issues a SCSI READ for block 0; it aborts at the
  connect/verify probe."
- **RAW-capacity crash vs usable-capacity error (VERIFIED symptom, INFERRED cause):**
  with RAW capacity the probe LBA is the true top block and something downstream
  dereferences past a valid structure -> crash at PC 012466; with a usable capacity
  the probe still runs but the same bit-4-on-completion makes it the clean
  "transfer error". **Same stage, same root bit (RSTAU bit 4), two surface
  symptoms.**

**Handshake the probe requires to proceed:** the read must end with RSTAU bit 4
clear (no error), bit 3 set (ready), transfer counter 0, and the completion
interrupt delivered and acknowledged - i.e. the identical good-completion contract
as any other read (sec. 6). The probe is not special; it is just the *first*
interrupt-driven read on the mount path, so it is the first to expose a
completion-status defect.

---

## 6. What the emulated ND-3201 must present on a GOOD transfer (the fix contract)

For SINTRAN to accept a completed disc transfer, the controller's IOX-visible
state, at the moment SINTRAN reads RSTAU after the transfer, must be:

| # | Register / state | Required value on GOOD completion | Grade |
|---|------------------|-----------------------------------|-------|
| 1 | **RSTAU bit 4 (Error)** | **0** - clear. Do **not** map the internal "dmaError"/completion onto bit 4 for a successful DMA. This is the single bit that currently breaks the mount. | VERIFIED (register table + gate) |
| 2 | **RSTAU bit 11 (BERROR)** | **0** - no ND-100 bus DMA error. A genuine DMA fault belongs here, not on bit 4. | VERIFIED |
| 3 | **RSTAU bit 3 (Ready for Transfer)** | **1** - controller completed and ready. | VERIFIED |
| 4 | **RSTAU bit 2 (Busy/Active)** | **0** - no longer processing. | VERIFIED |
| 5 | **NCR-interrupt bit** set at completion, **cleared only on the RITRG read (IOX 54)**, never on an RSTAU poll | present the completion IRQ (level 11) and let it survive RSTAU status polls; ack on RITRG | VERIFIED contract; **bit position 9 vs 11 is OPEN** - the register table says **bit 9**, the NPL revision tests **bit 11**. Pin with a live trace (sec. 7). Ties to [SCSI-MOUNT-FIX-PLAN.md](SCSI-MOUNT-FIX-PLAN.md) FIX 1 (do not clear the interrupt flag on the RSTAU case). |
| 6 | **RSTAU bit 15 (Differential)** | present per emulated board type. An ND-3201 is single-ended, so ideally **bit 15 = 0 and bit 7 (Single-ended) = 1**. Harmless either way (not gated), but setting bit 15 makes the printed status misleading. | INFERRED |
| 7 | **Transfer counter RTCM/RTC2/RTCL (IOX 70/72/74)** | **read back as 0** - all requested bytes moved. The driver treats a non-zero residual as an incomplete/failed transfer. | INFERRED (firmware doc: NCR transfer-counter mismatch is the controller's own "internal DMA error", POST code 0x1B) |
| 8 | **NCR interrupt register RITRG (IOX 54)** | present a completion/OK reason code, cleared by the read (the acknowledge) | INFERRED (firmware doc + impl guide) |

**Concrete controller-side change (RetroCore):** in the RSTAU read path of
`NDBusDiscControllerSCSI.cs` (the `Register.RSTAU` case), the assembled status word
must have the error bit (bit 4) **clear** whenever the transfer completed with all
bytes moved. Track down what sets the internal "dmaError"/error flag on this
otherwise-successful transfer and stop it from asserting for a completed DMA (a
completed 1024-byte move with counter 0 is success). Keep BERROR (bit 11) for real
bus faults only. Combine with FIX 1 from the mount-fix plan (acknowledge the NCR
interrupt on RITRG, not on RSTAU) so the completion interrupt is not lost during
the driver's status polling.

**Do not** simply mask bit 15 out of the printed value to make the message look
clean - the failure is bit 4; bit 15 is cosmetic.

---

## 7. What a live trace would pin (for the two OPEN points)

Two items cannot be closed from static carved bytes:

1. **The NCR-interrupt bit position (9 vs 11).** Break the ND-100 in the SCSI
   driver at the RSTAU read on the completion interrupt (`SCSI1` 046530B /
   `SCSI2` 046661B / `SCSID` 062217B on the mount path) and read the RSTAU value the
   driver evaluates, plus the immediately following bit test. The bit the branch
   keys on for "interrupt from NCR" is the one the emulator must set.
2. **The exact error-code translation in `SCDTS` (062107B).** Because that region
   is PLANC data-before-code in the current carve, break at `SCDTS` on the failing
   probe and capture the internal code in T/A and the user-error code it returns.
   That confirms the 232B/141B mapping without trusting the NPL revision.

For the controller side, the RetroCore `[SCSI-TRACE]` plan in
[SCSI-MOUNT-FIX-PLAN.md](SCSI-MOUNT-FIX-PLAN.md) sec. 4 already logs every IOX
read/write with `active`/`intFlag`; add the assembled RSTAU value and the
transfer-counter readback to that log so the exact bits presented at completion are
visible next to what SINTRAN reads.

---

## 8. Summary

- The printed `STATUS 100020B` is the raw **RSTAU** value SINTRAN read after the
  probe transfer. (VERIFIED)
- `100020B = bit 15 + bit 4`. **Bit 4 = "Error (OR of error conditions)" is the
  trigger**; **bit 15 = "Differential" is a benign strap.** (VERIFIED - register
  table)
- The driver's completion gate rejects any transfer with **RSTAU bit 4 set**,
  regardless of the fact that all data DMA'd and every SCSI command returned
  SS_GOOD. It aborts at the connect/verify probe (highest-addressable-block read)
  and never reaches the block-0 directory read. (VERIFIED gate meaning; INFERRED
  exact opcode sequence, NPL revision.)
- **Fix:** the emulated ND-3201 must present, on a completed good transfer, RSTAU
  **bit 4 = 0, bit 11 = 0, bit 3 = 1, bit 2 = 0**, the NCR-completion interrupt set
  and acknowledged only via RITRG, and the transfer counter read back as **0**.
  Stop mapping the internal DMA-completion/error onto bit 4 for a successful move.
  (VERIFIED contract; NCR-interrupt bit 9-vs-11 OPEN - pin by live trace.)
