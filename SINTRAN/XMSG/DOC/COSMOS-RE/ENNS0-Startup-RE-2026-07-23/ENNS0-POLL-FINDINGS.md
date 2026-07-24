# ENNS0 START-NETWORK-SERVER poll analysis

## Tooling (reconstructed, in scratchpad)
- brf_link.py  - BRF loader/linker per SINTRAN/File-Formats/BRF-FILE-FORMAT.md
- nd100dis.py  - ND-100 disassembler (control flow / MON / IOX focus)

## VERIFIED
- Linker resolves all 174 units to completion; MAIN = ENNS0 @ 031655 (octal);
  215 defined symbols incl. the 10 PIOCM wrappers.
- ENNS0 contains ZERO IOX and ZERO IOXT instructions. Same for encos-mon-i/ii .prog.
  => ENNS0 never reads the Ethernet STATUS/CONTROL register or any DRAM word directly.
- All controller access is via SINTRAN monitor calls:
  - MON 200B x2  = XMSG/XROUT registration (SAT0;MON200;SAT1;MON200) @ 030230/030233
  - MON 255B x10 = PIOCM wrappers: READPIO, SEGLOAD(T=4), UNLOAD(T=5), START_P(T=6),
    STOP_PI(T=7), RES_SLO(T=0), REL_SLO(T=5), SEND_KI(T=7), REC_KIC(T=3), INT2GET
- Server-start sequence region 030200-030710: XROUT reg -> MON 322/124/125 ->
  SEGLOAD (firmware banks) -> START_P -> a retry/timeout poll loop @ 030657-030700
  (backward `JMP *-17`), terminating on `SKP IF DA LST SB` (loop while counter A < limit B).

## INFERRED / NOT verifiable from these bytes
- The exact DRAM word / status bit cannot be pinned to an ENNS0 instruction, because
  ENNS0 issues no direct I/O. The $1001 status-register reads the live debugger saw are
  the SINTRAN kernel PIOCM (MON 255B) driver polling on ENNS0's behalf - that driver is
  NOT in these files.
- $1001 = bank 16 (0x10 in status bits 15:8) | bit0 (interrupt enabled). The bit that
  stays 0 is STATUS bit 2 = "interrupt set for ND-100 on level 12" (per hw manual).
- Firmware posts readiness via monitor postbox: monitor_code (0x40C), monitor_counter
  (0x40A), monitor_request_flag (0x412), then post_and_signal_nd100_scip (0x1A48) does
  SCIP -> INT12. The missing link is INT12 delivery / STATUS bit 2, in the kernel driver
  or emulator status register - NOT in ENNS0.
</content>
