# ENNS0 LU-2240B INPUT path: where the wake must come from, and the fix point (2026-07-23)

Answers the emulator-vs-SINTRAN question raised by
`ROOT-CAUSE-ENNS0-HANGS-INPUT-WAIT-LU2240B-2026-07-23.md` and
`ENNS0-LU2240B-DEVICE-PIN-2026-07-23.md`. Code+source only, no DAP this session.

`[V]`=VERIFIED (read the bytes / read the source). `[I]`=INFERRED. `[OPEN]`=needs DAP.

Sources read this pass:
- Emulator: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusEthernetII.cs` (4028 lines),
  `...\NDBUS\NDBusDeviceBase.cs`.
- SINTRAN drivers: `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\MP-P2-PIOC-DRIV.NPL` (level-12 PIOC
  driver `PDRIV`), `...\NPL\RP-P2-PIOC.NPL` (MON 255B `PIOCM`/`PISTA`, per the PIOCM-start finding).
- Prior findings: `ENNS0-PIOCM-START-FINDINGS-2026-07-23.md`, `ENNS0-LU2240B-DEVICE-PIN-2026-07-23.md`.

---

## TL;DR - hypothesis CONFIRMED but the fix point is NOT "add an input path to NDBusEthernetII"

1. `[V]` The ETH II controller presents to SINTRAN as exactly THREE things and nothing more:
   a 2-register IOX device (STATUS read / CONTROL write - there is **no data-input register**),
   a 512 KB shared DRAM window, and SCIP -> **INT12 / IDENT 140034B**. It does **not** model any
   SINTRAN character/record input device, and it **should not** - see point 2.
2. `[V]` For this PIOC-class device, a "SINTRAN logical-unit INPUT" is **never produced by the
   controller/emulator**. It is produced entirely by SINTRAN's own **level-12 driver `PDRIV`**
   (`MP-P2-PIOC-DRIV.NPL`) reacting to the **68K firmware** posting an RT-activation into shared
   PIOC DRAM (the "superkick" ring / RTBOX) and firing **SCIP -> INT12**. So the root-cause doc's
   phrase "the emulator never delivers a completed SINTRAN input" is a **category error**: the
   emulator never delivers a device INPUT for *any* device - the SINTRAN driver does, on a HW
   interrupt. The wake path EXISTS in the emulator (SCIP->INT12 is wired and the firmware is real).
3. `[V]+[I]` What is actually missing is upstream: the **68K firmware never completes the shared-DRAM
   handshake** that would (a) let the kernel finish `PISTA` START (readiness key `PRKEY`=052163B at
   PIOC word 1002B - see the PIOCM finding) and (b) post the RT-activation + SCIP that `PDRIV` turns
   into ENNS0's LU-2240B input. Because that never happens, `PDRIV` has nothing to dequeue and
   ENNS0's INPUT wait is never woken.
4. `[V]` **SINTRAN L is NOT missing the driver** (generation option ii is ruled DOWN): both halves
   exist in the L source - `PDRIV` (level-12 interrupt handler, wakes RT via `XRTEN`/`RTACT`) and
   `PISTA`/`PIOCM` (MON 255B). So the fix is **not** a re-generation.
5. `[OPEN]` Whether the firmware fails to post because of an **emulator stimulus/timing gap** in the
   RESET+INITIATE -> firmware -> SCIP handshake, or a **firmware<->SINTRAN-L protocol/version
   mismatch**, cannot be split statically. This is the one thing DAP must decide (section 6).

---

## 1. How the ETH II controller presents to SINTRAN (Task 1) `[V]`

From `NDBusEthernetII.cs`:

- **IOX registers** (enum `Register`, lines 1519-1540; `Read` 1093-1191; `Write` 1192-1373):
  only a STATUS register (read, base+0/+2) and a CONTROL word (write, base+1/+3). Crucially the
  `ReadDataRegister` case **falls through into `ReadStatusRegister`** (lines 1100-1110) and returns
  the status word - there is **no path that returns received bytes/records** to an ND-100 read.
  => the controller is NOT a byte/record input device. `[V]`
- **Interrupt to ND-100**: DEVNO 140360-140363, `InterruptLevel = 12`, `IdentCode = 140034B`
  (ctor, lines 493-498). SCIP from the 68K raises INT12 via `MemoryMap_OnNDInterrupt` (877-903)
  -> `SetInterruptBit(true)`; `IDENT(12)` returns 140034B and clears the latch (1389-1410).
  `IDENT` on `NDBusDeviceBase` just returns `IdentCode` and clears the interrupt bit (223-234) -
  **it carries no input payload**. `[V]`
- **Shared memory**: a 512 KB DRAM `InstrumentedRAM` (ctor 539) mapped into an ND-100 bank; the 68K
  firmware (real `Cpu68K`, line 592) and the ND-100 both read/write it. The ND-100<->firmware
  "mail" is these DRAM cells (mailbox map 1626-1713, tracer fields 1850-1877), not an IOX FIFO.

**Answer to "is there ANY path by which a SINTRAN device INPUT would receive data and complete?"**
`[V]` Not inside the controller. The only channels the controller offers are (i) INT12 (a bare
interrupt + IDENT, no data) and (ii) the shared DRAM window. A SINTRAN INPUT completion on LU 2240B
can therefore only be manufactured by SINTRAN's level-12 driver reading the DRAM window when INT12
fires. That is by design and matches real hardware.

## 2. The SINTRAN side that DOES wake the RT (Task 2) `[V]`

`MP-P2-PIOC-DRIV.NPL` is the level-12 PIOC driver. Verified mechanics:

- The 68K posts activation entries into a **"superkick" ring** in PIOC DRAM at fixed offset
  `SUKOF=1012`, guarded by pattern `HSKPA=52525 / LSKPA=125252` (lines 39-41, 65-72). Each entry
  has a `DLEVL` "level" tag selecting RT vs XMSG vs MTAD (86-89).
- On INT12, `PDRIV` (entry 100765) walks that ring. The **`RTPR`** branch (115-126) takes an entry
  whose payload is an **RT-description address** and does `JPL I (XRTEN` (line 125) = **enter/kick
  the RT program** = the wake. `PIWKF` (228-246) is the "wake up routine, activated from XMSG" that
  sets `NXRTF` bit 2 (RTDONE) and `IOXT`s `HDEV+3` to re-enable the PIOC.
- `NXRTF` semantics (verbatim, lines 22-23): *"BIT 0 SET BY PIOC WHEN RTWAK REQUESTED, BIT 2 SET BY
  ND100 WHEN RTWAK COMPLETED."* i.e. the firmware requests the RT wake; the ND-100 driver performs
  it. This is exactly the "post a record and dequeue the first waiter" that the root-cause doc
  predicted - and it lives in SINTRAN, driven off INT12, **not** in the controller.

So the generated system **does** contain a working level-12 driver that converts a firmware INT12
into an RT wake. Generation option (ii) "missing/unwired driver" is ruled DOWN. `[V]`

## 3. Reconciliation - what wakes ENNS0's LU-2240B read in a WORKING system (Task 3)

`[V]+[I]` The chain in a healthy COSMOS system:

```
68K firmware (has input / start-ack ready)
   -> writes RT-activation entry into PIOC DRAM superkick ring  (+ readiness/mailbox cells)
   -> SCIP write (EF_0180)  ->  MemoryMap_OnNDInterrupt  ->  INT12 to ND-100
ND-100 level 12: PDRIV walks superkick ring -> RTPR -> XRTEN  ->  RT (ENNS0) woken
   -> ENNS0's blocked INPUT read on LU 2240B (external routine 0146547B) returns
```

`[V]` This is the same transport the START handshake uses. The PIOCM-start finding
(`ENNS0-PIOCM-START-FINDINGS-2026-07-23.md`) already pinned the first stall on the SAME firmware
side: `PISTA` (MON 255B, T=6) does RESET (`PWCR=60B`) then INITIATE (`PWCR=0`), then **busy-polls
PIOC word 1002B for `PRKEY=052163B`** with a 3 s timeout; the firmware never wrote `PRKEY`, so the
start timed out. `PRKEY` and the LU-2240B input are two posts on the same never-completing firmware
DRAM/SCIP handshake.

`[I]` So the "INPUT wait" is NOT a MON 255B REC_KIC/INT2GET blocking that the emulator forgot to
kick in isolation. The wrappers (`REC_KIC` T=3->`PIWKI`, `SEND_KI` T=7->`PISTO`, `INT2GET` builds a
block, `START_P` T=6->`PISTA`; table in the PIOCM finding, decoded @032703-033147) all funnel
through the SAME `PIOCM` kernel driver and the SAME shared-DRAM+SCIP transport. The blocked frame
itself (resume-P 030440, PRSRV path, calls external `0146547B`) is a device INPUT via RTCOMMON that
`PDRIV` must wake - and `PDRIV` can only wake it if the firmware posts + SCIPs. `[V]` for the
transport identity; `[OPEN]` for the exact identity of `0146547B` (needs DAP step-in).

## 4. Why the firmware never posts - candidate causes (Task 4) `[OPEN]`, ranked

Because the emulator runs the **real** production firmware (`encos-ser-all-banks-68k.bin`) on a real
`Cpu68K`, "the firmware never posts PRKEY / never SCIPs the input" has three static-plausible causes
that only DAP can separate:

1. `[I, leading]` **RESET/INITIATE -> firmware stimulus gap.** `PISTA` drives the START purely by
   writing `PWCR` (control word): `60B` = halt+reset, then `0` = initiate (PIOCM finding lines
   114715-114721). In the emulator these land in `Write()` and set `reset`/`halt`/`startOpcom`
   bits (1243-1256) and edge-detect reset (1292-1317) and OPCOM (1349-1355). If the firmware's
   post-reset "publish datafield ptr @word1001B + write PRKEY @word1002B" routine is gated on a
   68K interrupt/vector that this `PWCR` sequence does NOT actually raise in the model (e.g. the
   INITIATE doorbell is expected as an OPCOM/GPIP edge the firmware's init waits on), the firmware
   sits before writing PRKEY. The file's own history shows this class of bug is real here (the
   GPIP-I6 "level not one-shot" fix at 1319-1334; the SCIP-pending race fix at 877-903/1258-1288).
2. `[I]` **Firmware<->SINTRAN-L protocol/version mismatch.** The mailbox map has TWO overlays for
   the SAME DRAM cells - a bank-0 DIAGNOSTIC/TPE map and a PRODUCTION ENNS0 map (comments
   1683-1713). If the production firmware image loaded does not implement the exact `PRKEY`@1002B /
   MASTA-mailbox contract that L's `PISTA`/`PDRIV` expects, no amount of emulator wiring will make
   the handshake close. This is a firmware/generation-pairing issue, not an emulator bug.
3. `[ruled-down]` **Emulator must synthesize the input itself.** Rejected: there is no device-input
   surface to add (section 1), and doing so would fake data ENNS0 has no real source for. The wake
   must come from the firmware+PDRIV path, not from NDBusEthernetII fabricating a record.

## 5. CONCLUSION - the precise gap and fix

- **It is an emulator gap, but NOT in the place the leading hypothesis named.** Do **not** add a
  "SINTRAN LU-2240B input completion" or a character/record input buffer to `NDBusEthernetII.cs` -
  that path does not exist on real hardware and is not how any SINTRAN device INPUT completes.
  `[V]`
- **The single fix point is: make the 68K firmware's post-reset shared-DRAM handshake actually
  complete**, i.e. after `PWCR`=60B then `PWCR`=0 the firmware must publish the PIOC datafield
  pointer at PIOC word 1001B and write `PRKEY=052163B` at word 1002B, and thereafter post
  RT-activations into the superkick ring + SCIP. When PRKEY posts, `PISTA` proceeds and writes
  `MPIOC=5`+`TRIG`+doorbell (PIOCM finding 115067-115103); when the firmware later posts an RT
  activation + SCIP, `PDRIV`->`RTPR`->`XRTEN` wakes ENNS0's LU-2240B read. `[V]` for the SINTRAN
  half; `[OPEN]` for which emulator stimulus (or firmware image) currently prevents the firmware
  from getting there.
- **SINTRAN-generation gap: ruled DOWN.** The L system contains both the MON 255B `PIOCM`/`PISTA`
  driver and the level-12 `PDRIV` input/wake driver. `[V]`

The emulator code region to instrument/fix is the **CONTROL-word RESET/INITIATE/OPCOM edge handling
in `NDBusEthernetII.Write()` (lines 1243-1355)** and the **SCIP->INT12 delivery
(`MemoryMap_OnNDInterrupt` 877-903, pending-SCIP 1258-1288)** - specifically whether the `PWCR`=0
INITIATE that `PISTA` issues produces the 68K stimulus the firmware's PRKEY-writing init actually
waits on. That is where a real fix (if it is an emulator gap) will land; but the decision of
emulator-gap vs firmware-image-mismatch is `[OPEN]` and needs the DAP run below.

## 6. Exactly what to breakpoint (DAP, decisive) `[OPEN]`

ND-100 side (RetroCore DAP):
1. BP `PISTA` INITIATE `114721` and the readiness-poll exit `114737` (`IF A >< PRKEY`). Read A after
   the poll: if A stays 0 -> firmware never wrote word 1002B (confirms the PRKEY stall drives
   everything). Watch PIOC DRAM word `1002B+2000B` for any write.
2. BP ENNS0 `030427` (PRSRV) and resume-P `030440`; read the PRSRV param block (DeviceNo=?2240B,
   IOflag, Stat) and step into `0146547B` to name the input primitive and the datafield-103356B
   waiter it queues on.
3. BP the level-12 vector / `PDRIV` entry (100765) - confirm whether INT12 ever fires during the
   ENNS0 wait and, if it does, whether the superkick ring has an `RTPR` entry to dequeue.

68K side (firmware, Cpu68K):
4. BP the firmware post-reset init and the `post_and_signal_nd100_scip` path (the code that writes
   the monitor postbox 0x40A / STARTED_FLAG 0x4C0 and pokes SCIP EF_0180). Determine whether it is
   reached at all after `PWCR`=0, and if not, what event it is waiting on (OPCOM level 6 / GPIP I6 /
   a mailbox cell). That event, cross-referenced to `NDBusEthernetII.Write()` bits 2/3/4, is the
   emulator fix - OR its absence in this firmware image is the version-mismatch proof.

Decision rule: if forcing/correcting that one stimulus makes the firmware write PRKEY and later
SCIP an RT activation, and ENNS0 wakes -> emulator gap (fix in `Write()`/SCIP edge). If the firmware
image simply has no PRKEY/superkick contract matching L -> firmware/generation pairing, not an
emulator bug.

## Honest OPEN items (no guessing)
- Emulator-stimulus-gap vs firmware-image-mismatch for the missing PRKEY/SCIP post. `[OPEN]` - DAP.
- Identity of external routine `0146547B` (RTCOMMON input primitive). `[OPEN]` - DAP step-in.
- Byte-mapping of firmware DRAM 0x406/0x408/0x4C0 to ND-100 `MASTA+NPFUN`/word-1002B. `[OPEN]`.
- Whether the two live symptoms (PIOCM START 3 s timeout vs LU-2240B INPUT RTWT) are sequential
  phases of one run or two different runs. `[OPEN]` - both trace to the same firmware handshake.
```
