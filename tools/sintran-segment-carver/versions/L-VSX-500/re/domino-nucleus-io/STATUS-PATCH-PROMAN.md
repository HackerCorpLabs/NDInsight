# STATUS PATCH - PROMAN auto-run recon (S0-4 / SIN-F9) - 2026-07-20

Paste block for the status docs (CARVING-HANDOFF.md / ND500-STATUS-AND-INDEX.md /
SCSI-DIOC-OCTOBUS-EMULATION-PLAN). Do not edit those docs from this task.

---

## PROMAN auto-run (SIN-F9) - RESOLVED: does NOT run at boot on this image [V]

- SIN-F9 was [UNVERIFIED] "does this L07 image auto-run PROMAN when a DOMINO
  device is generated?". Answer: **(a) SAFE - PROMAN never runs on this image's
  boot.** An emulated SCSI DIOC station at 10B-13B will receive NO
  EchoTest/IdentY/SetBxP/BxDoLd/RegMod/Go-On boot-protocol traffic.
- Decisive [V] evidence: live `@LIST-RT-PROGRAMS` (inputs/list-rt-programs.txt,
  captured after XMSG start) shows PROMAN 14615B PASSIVE P-REG=0B (never
  executed), likewise NKSERV/NKNAME/EVMESG/BOPCOM/MTSERV; calibration: XROUT
  P=2631B IO-WAIT and XMFIDO P=114427B READY in the same listing.
- [V] Pack BIGDISK0-L.IMG has NO (SYSTEM)PMA-CONFIG, no PMA-ERS-BUFFER, no
  (UTILITY)PMA-* images, no DOMINO kit (full 70-file ndtool listing).
- [V] Segment identity proven by strings: 120-S3SPRMA/121-S3IPRMA = PROMAN
  ("PROMAN started" @byte 26146, "(SYSTEM)PMA-CONFIG" @72682, full boot-error
  ladder, module table incl. "SCSI" @76228); 124-S3SBOPC/125-S3IBOPC = BOPCOM
  ("Bopcom-Server" @20787). Save/image twins byte-identical at these offsets.
- [NPL-V] No kernel auto-start exists in the NPL tree: only the RT-description
  (s3vs-4.symb, PROMA start=SPSRS=030020 seg 5PROM=121), the OPPSTART cold-start
  save->image copy, and CLEALLSEGS. OCSTART/NUCST allocate memory only.
- [V, consumer OPEN] Command-processor segments hold a server-start table:
  003-S3CP @0xbb60 / 013-S3SCP @0xc360 = pairs (RTdesc,flag): NKSERV,2 NKNAME,0
  PROMAN,2 EVMESG,0 BOPCOM,2 MTSERV,2 end 8dce/8002. Whatever consumes it did
  not fire on this boot; consumer + flag semantics + gate condition [OPEN].
- [MAN+I] Even a started PROMAN only talks to controllers discovered via MF-bus
  controller crate interrogation (investigate-bank) or PMA-CONFIG entries - no
  blind station probing (ND-820026.1 sec 2.6.1; consistent with carved truth
  that bring-up sends MFPREPARE to stations 2-6 only).
- Answer flips to (b) only if PROMAN is deliberately started AND an MF-bus
  controller (or PMA-CONFIG) reports the DIOC AND PMA-* images are put on the
  pack. Re-open if the harness ever emulates MF-bus crate interrogation.
- Full finding: re/domino-nucleus-io/PROMAN-AUTORUN-RECON.md (all offsets
  dd-reproduced 2026-07-20).
