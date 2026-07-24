# ENNS0 LU 2240B device pin - what ENNS0 waits on and why it never wakes (2026-07-23)

Follow-up to `ROOT-CAUSE-ENNS0-HANGS-INPUT-WAIT-LU2240B-2026-07-23.md`. Static, read-only
decode of `Installation\Communication\Ethernet\x\encos-err-i-b01.brf` (MAIN=ENNS0 @031655),
cross-checked against `Reference-Manuals\ND-860228-2-EN SINTRAN III Monitor Calls.md`,
`SINTRAN\OS\00-SINTRAN-ARCHITECTURE-OVERVIEW.md`, and RetroCore `NDBusEthernetII.cs`.

`[V]`=VERIFIED (decoded bytes / read manual). `[I]`=INFERRED. `[OPEN]`=undecidable statically.

Tools reused (not rebuilt): `tools\brf_link.py`, `tools\nd100dis.py`, `tools\dumpwords.py`,
`tools\symdump.py`.

---

## TL;DR

- `[V]` MON identities confirmed against the manual: MON 322 = **GSGNO** GetSegmentNo;
  MON 124 = **PRSRV** ForceReserve `(DeviceNo, IOflag, RTProgram, Stat)`; MON 125 = **PRLS**
  ForceRelease. PRSRV works **only on peripheral devices and semaphores** (manual line 8967).
- `[V]` **LU 2240B is not a literal in ENNS0.** The only `002240` word in the whole image is
  the instruction `STZ -96,X` at 030327 (false match); `103356` appears nowhere. Both the LU
  number and the datafield address are runtime values - confirmed, not fabricated.
- `[V]` **Datafield 103356B lands in the POF resident range 100000-110000B**, which
  `00-SINTRAN-ARCHITECTURE-OVERVIEW.md` (sec 5.3) labels *"Error device, line printers,
  **SINTRAN communication**, SIBAS internal devices."* So LU 2240B's datafield is a **SINTRAN
  communication / internal-device** datafield - NOT a terminal, disk, or ordinary peripheral.
  This is the strongest single locator we have.
- `[I, strong]` LU 2240B is the **SINTRAN Ethernet-interface communication device** (the
  "Ethernet Interface datafield" the install prereq ND-210580 p1 requires). ENNS0 force-reserves
  its **input part** and blocks reading it.
- `[V, NEW]` resume-P `030440` sits on the **PRSRV-returned-NONZERO** branch, not the OK branch
  (see section 2). This reframes the hang and is the key new fact this pass adds.
- `[V]` The routine reached from the blocked frame (`JPL I *12` @030440 -> pointer word@030452
  = `146547`) is **outside the ENNS0 image** (image occupies 001756B..073116B; 0146547B is
  above it). It is an external/shared (RTCOMMON-class) routine, not ENNS0 code.
- Fix direction: leading candidate is an **emulator gap** - nothing ever delivers a SINTRAN
  device INPUT on LU 2240B - but a live single-step is required to close it decisively
  (section 5). Do NOT ship a fix on the static read alone.

---

## 1. The verified instruction path (Task 1 + 2)

From `nd100dis.py`, POSU startup body (execution order; all opcodes `[V]`):

```
030375  153322  MON 322      ; [V] GSGNO GetSegmentNo ; A -> STA -118,B
030376  004612  STA -118,B
...
030405  045042  LDA I *42 =030447  ; param staging for the reserve
030406..030426  (compute DeviceNo/IOflag param block; DD MGRE byte-extract idiom)
030426  044607  LDA -121,B          ; A = DeviceNo argument for PRSRV
030427  153124  MON 124      ; [V] PRSRV ForceReserve(DeviceNo, IOflag, RTProgram, Stat)
030430  004616  STA -114,B          ; save returned Stat
030431  044616  LDA -114,B          ; A = Stat
030432  131012  JAZ  030444         ; [V] if Stat==0 (OK) -> jump to OK path @030444
                                    ;     if Stat!=0 (error) -> fall through @030433
--- PRSRV-NONZERO (error) path -------------------------------------------------
030433  044016  LDA *16 =030451     ; A = word@030451 = 030147
030434  054602  LDX -126,B          ; X = POSU context block ptr
030435  006006  STA 6,X             ; block[6] = 030147
030436  170462  SAA 50              ; A = 50B (=40 dec)
030437  006007  STA 7,X             ; block[7] = 50B
030440  135012  JPL I *12 =030452   ; <== RESUME-P. indirect via word@030452 = 0146547
030441  135012  JPL I *11 =030453
030442  170400  SAA 0
030443  135011  JPL I *11 =030454
--- OK path --------------------------------------------------------------------
030444  170401  SAA 1
030445  135007  JPL I *7 =030454
030446  001761              ; data
030447  001756  -> POSUERR   ; [V] POSUERR datafield pointer (001756B)
030450  044511
030451  030147              ; block[6] value used above
030452  146547              ; [V] pointer -> 0146547B (OUTSIDE ENNS0 image)
```

`[V]` `dumpwords`: word@030452 = `146547`; ENNS0's non-zero image span is 001756B..073116B, so
`0146547B` is **not ENNS0 code** - it is an external/shared routine (RTCOMMON-class COSMOS
common, `[I]`; exact identity `[OPEN]` - could be the POSUERR reporter at 001756B reached via a
different word, or a shared input primitive). `030447 = 001756 = POSUERR` sits in this same
data island, so the block is the standard POSU error-report idiom used after every MON in this
module.

PRSRV parameter order (manual 8975-8978, `[V]`): 1=Logical device number (=LU 2240B),
2=IOflag (0=input part / 1=output part), 3=RT-description addr (0=own program), 4=Stat
(0=OK; negative = *device already reserved*). ENNS0 reserves the **input part of LU 2240B for
its own RT**, then reads that input.

Name-registration footprint: unchanged from the annotated disassembly - ENNS0's only XMSG is
`030230 MON 200` T=0 (XFDUM) + `030233 MON 200` T=1 (XFDCT). This hang is a **device INPUT
wait**, a separate mechanism from XMSG naming.

---

## 2. NEW finding: the blocked frame is on the PRSRV-NONZERO branch `[V]`

`JAZ 030444` (@030432) jumps to the OK path **when Stat==0**. resume-P is `030440`, which is
only reachable by **falling through** = **Stat != 0**. Per the manual a negative PRSRV Stat
means *the device was already reserved*. So statically:

- ENNS0's PRSRV @030427 on LU 2240B returned **non-zero**, and the RT is parked in the
  non-zero handler that calls out to `0146547`.

This partially tensions with the live descriptor line `RESERVED DATAFIELDS 103356B` (which says
the device *is* reserved). Two readings, both consistent with the bytes, cannot be separated
without live registers:
- `[I-a]` the device was already reserved (by a prior ENNS0 run / another RT), PRSRV returned
  negative, and ENNS0 fell into a handler that queues it INPUT-waiting on that same device
  (hence `FIRST WAITING`); or
- `[I-b]` a *different/earlier* reserve holds 103356B and this PRSRV concerns another unit.

This is exactly the point where I stop and refuse to guess: **which branch and which LU the
live registers actually carry is `[OPEN]` on static evidence** and must be read live (section 5).

---

## 3. What the input is, and T=2 (Task 2 continued)

- `[V]` ENNS0 does an **INPUT (FIRST WAITING)** on LU 2240B - a SINTRAN device-input wait,
  queued first. The reading routine is the external `0146547` (block[7]=50B/40dec looks like a
  max byte/record length passed to it, `[I]`).
- `[I]` What it is trying to receive: an input record/byte on the Ethernet-interface
  communication device - i.e. data the SINTRAN level-12 Ethernet driver would post after the
  controller signals inbound data. NOT an XMSG message (no XFRCV/XFGET in ENNS0) and not an
  inter-RT signal via XMSG.
- `[OPEN]` **T=2 in the saved register set: I cannot verify a meaning.** T is just the RT's T
  register at suspension; SINTRAN MON params are passed on the RT stack, not in T, so T=2 is
  most likely a leftover. I will not invent a semantic for it.

---

## 4. Identifying LU 2240B / datafield 103356B in SINTRAN terms (Task 4)

- `[V]` `103356B` in 100000-110000B = POF resident *"SINTRAN communication / internal
  devices"* area (`00-SINTRAN-ARCHITECTURE-OVERVIEW.md` sec 5.3). => a communication/internal
  device datafield, not a terminal/disk.
- `[V]` PRSRV only reserves peripheral devices + semaphores => LU 2240B is a peripheral-class
  (communication) device, consistent with the datafield range.
- `[V]` RetroCore Ethernet II (`NDBusEthernetII.cs` header + IdentCode): hardware DEVNO
  140360-140363B, **IDENT 140034B, interrupt LEVEL 12**; the SINTRAN logical device number is
  assigned at SINTRAN-generation time (the "Ethernet Interface datafield").
- `[I, strong]` Net: **LU 2240B = the SINTRAN-generated Ethernet-interface communication
  device** (input part). `2240B`=1184dec is a generated logical number, computed at runtime -
  which is why it is not a literal anywhere in ENNS0.

I did NOT find an Appendix-B logical-number table for THIS generated system in the repo, so the
exact LU->hardware binding is `[I]`, not byte-proven.

---

## 5. Why no input arrives, and the fix (Task 3 + 5)

Evidence-ranked:

1. `[I, leading] Emulator gap on the SINTRAN device-input path.` The RetroCore Ethernet II is
   serviced at IOX / SCIP / INT12 / kernel-PIOCM (MON 255) level, and ENNS0 drives the
   controller only through PIOCM. But the thing that must wake this INPUT wait is a **SINTRAN
   level-12 Ethernet-driver input completion** on LU 2240B (driver posts a record to the
   device's input buffer and dequeues the first waiter). If the emulator never raises the
   specific input-ready interrupt (IDENT 140034B on level 12) that the generated ETH-datafield
   driver turns into LU-2240B input, ENNS0 waits forever. This matches the observed symptom
   (controller reaches READY / LANCE RX+TX ON in the harness, yet ENNS0's SINTRAN-level read
   never completes).
2. `[I] SINTRAN generation.` The device clearly EXISTS (real datafield 103356B, ENNS0 reached
   the reserve), so it was generated. But whether SINTRAN L actually has a level-12 driver that
   converts controller input into LU-2240B records is unproven here. If that driver is absent
   or not wired to IDENT 140034B, the wait never satisfies regardless of the emulator.
3. `[ruled-down] ENNS0 self-trigger.` ENNS0 has no XMSG receive and no prior step it skipped
   that would feed this device; the input is expected to come from the controller/driver side,
   not from another ENNS0 action.

`[OPEN]` I cannot decide (1) vs (2) statically - both produce an identical never-woken INPUT
wait. **Decisive next step (live, DAP):**
- Breakpoint `030427` (PRSRV) and `030440` (resume-P). Read A/Stat and the PRSRV param block to
  get the **actual DeviceNo (confirm =2240B), IOflag, and Stat** - this settles section 2.
- Step into `0146547` to name the input primitive (INBT/WAITF) and read the datafield 103356B
  driver state.
- From SINTRAN's side, check whether an IDENT 140034B on level 12 dequeues this waiter. If the
  controller can be made to post an input interrupt and ENNS0 wakes -> fix is emulator
  input-delivery on the LU-2240B path (option i). If SINTRAN has no driver to service it -> the
  fix is a generation issue (option ii).

---

## Verified evidence index

- `030432 131012 JAZ 030444` => resume-P 030440 is the PRSRV-nonzero branch. `[V]`
- MON 322/124/125 identities + PRSRV param order + "0=OK, negative=already reserved":
  `Reference-Manuals\ND-860228-2-EN SINTRAN III Monitor Calls.md` lines 1442-1445, 8963-8978,
  2537. `[V]`
- LU 2240B / 103356B not literals in ENNS0 (`002240` only @030327; `103356` absent). `[V]`
- word@030452=146547 outside image (image hi=073116B). `[V]`
- Datafield 103356B in POF 100000-110000B "SINTRAN communication / internal devices":
  `SINTRAN\OS\00-SINTRAN-ARCHITECTURE-OVERVIEW.md` sec 5.3. `[V]`
- ETH hardware DEVNO/IDENT/level: RetroCore
  `Emulated.HW\ND\CPU\NDBUS\NDBusEthernetII.cs` lines 17-22, 497. `[V]`

## Honest OPEN items (no guessing)
- Exact identity of routine at 0146547 (RTCOMMON input primitive vs POSUERR reporter). `[OPEN]`
- Which branch/LU the LIVE registers carry at the hang (static shows nonzero-PRSRV path). `[OPEN]`
- Meaning of T=2. `[OPEN]`
- Whether the failure is emulator input-delivery (option i) or a missing/unwired SINTRAN L
  ETH-datafield level-12 driver (option ii) - needs the live DAP session above. `[OPEN]`
