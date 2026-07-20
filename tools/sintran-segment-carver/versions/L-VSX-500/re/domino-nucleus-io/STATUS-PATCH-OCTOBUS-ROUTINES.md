# Status-doc patch blocks - octobus driver routine carve (2026-07-19)

Ready-to-paste text for the status-of-record docs. DO NOT apply blindly - merge into the
existing sections. Source doc:
`E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\domino-nucleus-io\OCTOBUS-DRIVER-ROUTINES-CARVE.md`

---

## Block for `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-STATUS-AND-INDEX.md`
(add under the octobus / ND-5000 evidence register; also add the doc to the master index)

```
### Octobus driver routine bodies - BYTE-CARVED (2026-07-19)

Full annotated disassembly + pseudo-C for the SINTRAN L07 octobus high-level driver set:
XKICK500/LV12KICK (146526/146555), XRS5CPU (146642), RS5CPU (146700), 5OMBREAD/I5OMBR/
GN5CPUDF (146756/147240/147252), CON5OMD (147271), MFPREPARE (147300), CON5IDENT (147334),
5MTRANS (143445), 5MRDTRANS (144740) - all in 026-S3IMPIT (load 32000B, = 017-S3SMPIT) -
plus the primitives SKICK/SIDEN (037254/037256), MBSEND (037425), OMBREAD (037660).
Doc: tools\sintran-segment-carver\versions\L-VSX-500\re\domino-nucleus-io\OCTOBUS-DRIVER-ROUTINES-CARVE.md

Key byte-proven facts [V]:
- Overlay proof: 14 siblings at NPL+200B; every literal-pool pointer resolves to a named
  L07 symbol (SKICK/MBSEND/OMBREAD/ECONID/CONOMD/9FLER/9ERR/SLOCK/SUNLOCK/GETOUT/PTFREE/
  WN5STATUS/CHFIX/LOGPH/XACTRDY/XTER500/XRSTARTALL/NXTMSG/YWAIT/WT12/ID12).
- LMFIELD = 011545, LMDF = 011537 (LMDF[0] = 5OMDNO, runtime-allocated by CON5OMD);
  DPITPHYS = 0, DPITBANK = 1 in this build.
- CPU df offsets: 5STATION=17B, MAILINK=22B, CPUAVAILABLE=27B (5ALIVE = bit 13 dec,
  5CPUTYPE mask 7, SAMSON=3), CPUNO=-14B, stride 5CPUDFSZ=46B; list bounds cells
  S5CPUDF=052222 / E5CPUDF=052404; 5MBBANK cell = 004654.
- SKICK builds C|K|station<<8|kick and IOXes control:=4 / frame / control:=1 on the OUTPUT
  base (df[-3]=HDEV+4) when idle, else queues in a single-frame ring; SIDEN = same body,
  no K bit. Error codes 13B-20B.
- MBSEND has NO IOXT: validates (station 1..76B, OMD <=17B, length 1..255 BYTES ->
  errors 101430/101431/101427), pops a CBPOOL buffer (free head 007341 / count 007342),
  copies payload from LMFIELD+4, queues on the TX df, and if idle FIRES LEVEL 13 with
  P:=SOCTW(036342) to transmit. (Corrects the catalog row that placed "IOXT at 037320+"
  under MBSEND - 037320 is SKICK's direct-TX path.)
- OMBREAD fills the receive record: [0]=src station, [1]=src OMD, [2]=broadcast bit,
  [3]=byte count, +4=payload; empty status = 101410 (constant byte-located at 147171).
- 5OMBREAD: SAMSON source range is 70B..77B in L07 (LN5DEST=77, not M06's 73);
  MPFATAL array @146750 = 1,0,0,1,0,0,0,0,0,1,1,0; ack/nack -> CPUAVAILABLE|=5ALIVE;
  mp hwfault(200B)/trap(201B) with CMICP=1 -> shadow-id patch into S4, record LMFIELD+2
  len+4; else LMFIELD+3 len+2; MF etype -> 9FLER + MFACK reply; SEC codes ORed with
  N5SECCODE=2000B. 9FLER record: SEC code at LMREC+2, source station at LMREC+3
  (M06-symbol claim "offsets 0/1" flagged as divergence).
- MFPREPARE wire body (station 2..6, OMD 4, 3 bytes): 0E 01 <5OMDNO>;
  CON5IDENT (SAMSON, OMD 3, 7 bytes): 0E 01 <5OMDNO> 00 00 00 00.
- 5MTRANS: 5MPM displacements 5MNWA=100B, 5MREQ=105B, 5MEMA=106B, 5MLGN=110B, 5MDIS=111B,
  5DSEC=112B, 5MNOS=115B, PLINK=147B; disk queue element RTRES=1/NLINK=5/ABFUN=14B/
  MEMAD=15B/ABPA2=17B/ABP31=21B/REQID=25B/ADMESS=26B/5MNOWAIT=27B; function codes 60
  (read+clear cache), 61 (write), 66 (read keep cache); M5TRA=000012 = M5TRANS is the
  per-controller entry at disk-controller df offset 12B, called JPL I ,B 12; wait status
  5MWAIT=22B; success -> 5MRDTRANS (pointer cell 144014 = 144740).
Open: OMBREAD entry[-10..-12] status semantics; helper 036765 level-dispatch details;
MBSEND code 101426 site.
```

## One-liner for `E:\Dev\Ronny\NDInsight\SINTRAN\CARVING-HANDOFF.md`

```
- 2026-07-19: Octobus driver send/receive routine set byte-carved (SKICK/MBSEND/OMBREAD +
  XKICK500/5OMBREAD/MFPREPARE/CON5IDENT/5MTRANS/5MRDTRANS in 026-S3IMPIT; MBSEND has no
  IOXT - it fires level 13 -> SOCTW; max multibyte length 255 bytes; L07 SAMSON stations
  70-77B) -> re\domino-nucleus-io\OCTOBUS-DRIVER-ROUTINES-CARVE.md (ND-500 status doc has
  the full fact block).
```

## Catalog correction candidates (for SINTRAN-OCTOBUS-MESSAGE-CATALOG.md, do not auto-apply)

1. Section 1: EOCTSOURCE/ESECCODE - L07 store displacements are +3/+2 relative to LMREC
   (not 1/0); M06 numbers may be record-base-relative. Verify M06 before editing.
2. Section 8 MBSEND row: replace "IOXT sequence at 037320+" with "no IOXT; queues buffer +
   fires level 13 (P:=SOCTW 036342); 037320 is SKICK's direct-TX IOXT block".
3. Section 2: note LN5DEST=73 is M06; L07 = 77.
