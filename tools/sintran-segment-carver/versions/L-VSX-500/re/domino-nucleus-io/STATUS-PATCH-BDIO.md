# STATUS-PATCH: BDIO / DOMINO nucleus block-I/O carve (2026-07-19)

Paste-ready text blocks for the two status-of-record docs. Written as a
separate patch file to avoid concurrent-edit collisions; apply manually.

---

## Block A - for `E:\Dev\Ronny\NDInsight\SINTRAN\CARVING-HANDOFF.md`
(add under the segment-coverage / kernel-carving section)

```
### BDIO / DOMINO nucleus block-I/O driver (2026-07-19) - BYTE-VERIFIED

Full carve of the in-kernel DOMINO block-I/O path (ABSTRANS -> nucleus
message -> completion), analysis + annotated asm + pseudo-C at
`tools/sintran-segment-carver/versions/L-VSX-500/re/domino-nucleus-io/BDIO-DOMINO-DRIVER-CARVE.md`.

- Overlay: **017-S3SMPIT** (= 026-S3IMPIT, byte-identical), base 032000B.
  Proven by sibling coherence: BDMTR 073454 / BDMFU 073565 / MBUIL 073700 /
  DCNVA 073750 / SSBDI 074000 / BDTRA 074012 / BD12T 074024 /
  STRBD 074072 / REBDI 074246 (SYMBOL-2) all land on NPL-matching bodies,
  40+ literal-pool words resolve to L07 symbols, dd-reproduced.
  065-S3SIPIT decodes the same addresses to garbage (wrong overlay).
- STRBDIO builds the BDIO message in the global **DOMDF=041064** record
  (message body starts at DSVER = DOMDF+20 = 041104B) and sends it via the
  nucleus gates **NKWRI 043411 / NKSEN 042171**, waits on **WT12 033616**,
  completion in REBDIO via **NKREC 043076 / NKREA 043375** (all four gates
  share one prologue, same overlay). Function codes: read=166B(size 74B),
  write=167B(70B), compare=213B(70B); NKREA read-back max 76B.
- **DCNVA 073750**: ND-100 phys word addr -> DOMINO byte addr:
  `((addr - (N500D.ADRZERO << 10dec)) << 1) | bit31`; bias cached by
  SELF-MODIFYING the entry word to 124012 (JMP) after the first call.
  N500D=051767, ADRZERO=+60.
- HSTAT (queue DF +10) result codes: -1 illegal fn, -2 nucleus error/reject
  (SINEC 1661), -3 illegal memory address, -4 device error (SINEC 1662,
  9FLEX-logged, BDTMU timer-retry armed), -5 = statuses
  104031B/104651B/104622B (NPL: blank check / read only). Mirror-pool
  change arms BDTMV.
- NPL reference (different revision): MP-P2-DISK-START.NPL; L07 addresses
  shifted ~ +237B vs the NPL listing; logic matched 1:1.
- Open: DOMDF static header words (DSVER..+21, +52..+67), the WT12/NFUNC
  nucleus wake path, NK* internals, STDTX-displacement drift.
```

---

## Block B - for `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-STATUS-AND-INDEX.md`
(add to the document index + evidence register; this touches the level-12
path and the DOMINO/N500D datafield, so it is cross-relevant)

```
### BDIO / DOMINO nucleus block-I/O (level 12 + monitor level) - 2026-07-19

Doc: `tools/sintran-segment-carver/versions/L-VSX-500/re/domino-nucleus-io/BDIO-DOMINO-DRIVER-CARVE.md`
Status: BYTE-VERIFIED in 017-S3SMPIT (=026-S3IMPIT), base 032000B, L07
symbols (SYMBOL-2 routines, SYMBOL-1 offsets/nucleus).

- The DOMINO disk path does NOT use the 3022/MPM message block: it is
  nucleus messaging (NKWRI/NKSEN/NKREC/NKREA at 043411/042171/043076/
  043375) through the global DOMDF=041064 record, level-12 wait via
  WT12=033616 with DOMDF.NFUNC(+6) = REBDI=074246 as the continuation.
- Level-12 start: BDMTR (monitor level) arms level 12 with
  IRW LV12(140B): B=controller DF, X=que DF, P=controller.STDRIV(-2),
  then MST PID bit 12 (word 010000). Byte-verified at 073515-073526.
- **DCNVA 073750** converts ND-100 phys word addresses to DOMINO byte
  addresses with bit 31 set, bias = N500D.ADRZERO(+60) << 10dec;
  N500D=051767 [SYMBOL-2]. Self-modifying one-shot bias computation
  (entry word becomes 124012 JMP). This is the DOMINO analogue of the
  ND-500 bit-31 MPM addressing convention - same bit-31 flag semantics.
- Evidence: 13 dd-reproduced anchor words + 40 literal-pool symbol hits
  (table in the doc). NPL = MP-P2-DISK-START.NPL, different revision,
  logic-only.
```
