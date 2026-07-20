# STATUS PATCH - NKSE server interior carve (2026-07-20)

Paste block for the status-of-record docs. Do NOT edit the status docs from this session;
this is the hand-off patch.

## For CARVING-HANDOFF.md (segment coverage / DOMINO-NUCLEUS section)

Add:

- **105-S3INKSE (NUCLEUS server "C02 Sep 26 1988") interior carved** (task S0-3,
  2026-07-20). New doc:
  `tools/sintran-segment-carver/versions/L-VSX-500/re/domino-nucleus-io/NKSE-SERVER-INTERIOR-CARVE.md`
  + 6 `a-nkse-105-*.txt` annotated listings.
  - Segment is a **PLANC-compiled program** (runtime lib linked in at 112xxx; PLANC
    frame stubs 112541 ENTER / 112576 LEAVE / 112570 ERRETURN). NOT hand-NPL.
  - **`doNuc` server dispatcher @ 037033** [V]: linear `SAT n` ladder, functions 1..14B,
    function-code -> worker table dd-verified. Default case -> `doNuc: unknown func=`
    (string @040101) + `*** Nucleus FATAL Error` (@040062) via byte-string printer 106702.
  - **fn 10B = 047432 = descriptor CREATE/provision** [V]: writes port fields +20
    (KICKDEST/station), +30 (OWNID/port-number block), +2 (OWNER), +4 (FREELINK) via
    field-set helpers 070345/070422/070477/070560. This is the SIN-F5a/c port-number
    writer.
  - Server ACONV (number->ID) = 056332; descriptor validity/free walker = 057631;
    allocator front-end = 063371/063464.
  - NCALL client wrappers (MON 347) at 072263/073207/073266 - confirm request record
    {state, word-count from byte-len, caller-id block, param, >=7-word reply}.
  - **Coherence check vs NUCLEUS-PRIMITIVES-CARVE.md section 4 PASSED** on 7 structural
    items (ID->phys transform, record stride number<<6, port +20/+30, +2/+4, 101004
    ILLNO, PCR-remap window). No divergence.

## For the DOMINO SCSI / SIN-F5 review (SCSI-DIOC-OCTOBUS-EMULATION-PLAN)

- SIN-F5a/c (port provisioning) partially unblocked: the port-descriptor field writer is
  **located and byte-verified** = server fn 10B routine **047432** in segment 105, using
  field-set helpers 0703xx. It writes descriptor offsets +20 (remote station) and +30
  (OWNID/port-number block) which match the kernel-verified port record layout.
  Remaining [OPEN]: exact DRPRT/DLPRT symbol -> sub-offset pin (needs SYMBOL-2-LIST
  NKMBU/port symbols or a live create-port capture); descriptor-freelist head master
  offset (server reads via runtime global pointer, zero on disk); full NCALL mailbox
  per-word map (needs live round-trip).

## Recon target list status (NUCLEUS-SEGMENTS-RECON.md section 4)

- Target 1 (105 pages 0-3 dispatcher / doNuc / fn table): **DONE** [V].
- Target 2 (master-block hash/net/buffer/freelist server-side): **PARTIAL** - freelist
  helpers found; head cell offset [OPEN] (runtime global).
- Target 3 (NCALL mailbox record layout): **PARTIAL** - client wrappers carved, skeleton
  confirmed; full field map [OPEN].
- Target 4 (107 name server): not in scope of S0-3.
- Target 5 (pin SYMBOL-2-LIST NKSER/NKNAM/NKMBU/MSNKS to bytes): [OPEN], named as the
  blocker for the DRPRT/DLPRT sub-offset pin.
