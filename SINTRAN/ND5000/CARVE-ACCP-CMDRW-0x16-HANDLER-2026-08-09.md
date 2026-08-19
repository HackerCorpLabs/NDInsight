# Carve — ACCP command 0x16 = CMDRW (026B) handler (2026-08-09)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\CARVE-ACCP-CMDRW-0x16-HANDLER-2026-08-09.md`
**Scope:** E2-P2. Does NOT touch `ACCP-COMPLETE-REFERENCE.md`.
**Reads with:** `CARVE-ACCP-CMDWW-0x14-HANDLER-2026-08-09.md` (the write twin),
`CARVE-ACCP-E2-P1-COMMAND-BYTE-MAP-2026-08-08.md`.

## Result

**Command byte `0x16` is `CMDRW` (`0o26`)** — a **control-store word READ-BACK**. Bound from
`N500-SYMBOLS.SYMB` (`CMDRW=000026`); behavior matches the name ("Read Word"), so this is a clean
name-lock (contrast 0x15, whose write behavior did not match its `CMRWC`/read alias).

It reads one 128-bit control-store word (8 halfwords) at a caller-supplied CS address and returns it
+ a checksum to the ND-100 — the read counterpart of `CMDWW`(0x14, the CS-staging write).

## Evidence — the handler (octo.bin, Ghidra 2026-08-09)

Arm `0x519C` = `cmpi.b #0x16,D0` (matches the carve map). Body `0x51A4`–`0x52C1`, ends `bra.w 0x6878`:

```
51A4  jsr 0x6FFA; @0x52 = D0        ; read the CS-address parameter from the ND-100 message
51B0  clr.w 0x1144EC; clr.w 0x1144EA
51BC  tst.w 0x1143AC; beq 51E8      ; mode flag; !=0 -> 0xFF/6A64 alt-abort path -> done
51E8  andi.l #0xFFFF,D0
51F6  D1=0x3FFF; cmp.w @0x52,D1; bcc 5222   ; require @0x52 <= 0x3FFF (control store is 0..0x3FFF)
      (addr > 0x3FFF): 0xFF nak; NAK code 3; 6A64 -> done      ; NAK 3 = address out of range
5222  D0=@0x52; jsr 0x741E; @0x6E=D0         ; 741E(cs_addr) = READ the CS word -> fills 0x1144F0
5230  tst.w 0x113138; bne 5222              ; busy-wait until ready (0x113138)
523A  ext.l D0; tst.l D0; beq 5260
      (result != 0): 0xFF nak; NAK code 5; 6A64 -> 526A         ; NAK 5 = read error
5260  D0=0; jsr 0x6986                        ; ACK code 0 = success
526A  i = 0
loop (526E):
526E  D0 = buffer[i]   (A0=0x1144F0)
5286  jsr 0x69D0                              ; SEND the halfword back to the ND-100 (reply-word)
528E  @0x50 += buffer[i]                      ; checksum
52A6  i++; cmpi #7; bne 526E                  ; i = 0..7  (8 halfwords = the 128-bit CS word)
52B6  D0 = @0x50; jsr 0x69D0                   ; send the checksum word
52C2  bra.w 0x6878                            ; done
```

Helper roles pinned by usage (this carve):
- `0x741E(cs_addr)` = **read a CS word into `0x1144F0`** (the 8-halfword staging buffer). Its write
  twin used by `CMDWW` is `0x73B2`.
- `0x69D0(word)` = **reply-WORD sender** (data back to the ND-100), distinct from `0x6986` =
  reply-CODE sender (ack/nak byte). `0x6FFA` = request-word reader.

## The 0x13–0x16 cluster, now coherent

| Cmd | SYMB (0o) | Behavior (carved) | target |
|---|---|---|---|
| 0x13 | CMWWC 023 (LOCSM) | write word to control store | CS |
| 0x14 | CMDWW 024 | checksummed 8-halfword write into `0x1144F0` staging (→ `0x73B2`) | CS |
| 0x15 | CMRWC/CMADR 025 | checksummed multi-word **memory** write-block (→ `0x70AA` MFbus) | MFbus mem |
| **0x16** | **CMDRW 026** | **read CS word at addr, return 8 halfwords + checksum (→ `0x69D0`)** | **CS** |

## Verified vs inferred

- **[V]** `0x16 = CMDRW = 0o26`; arm `0x519C`; reads addr param, validates `<=0x3FFF`, reads a CS
  word (`0x741E`) into `0x1144F0`, returns 8 halfwords + checksum via `0x69D0`; NAK 3 = addr range,
  NAK 5 = read error, ACK 0 = ok; `0x1143AC` mode-flag alt path.
- **[INFERRED]** the exact internals of `0x741E` (named here from its usage as the CS read); the
  precise meaning of the `0x1143AC` alt path (shared across 0x14/0x15/0x16).

## Wrong turns — do not repeat

- Same as 0x14: Ghidra ACTIVE program is a BRF, always pass `program_name: "octo.bin"`.
- Do not confuse `0x69D0` (reply-word/data) with `0x6986` (reply-code/ack-nak) — different senders.

## Open

- 29 unnamed arms remain. Next in chain order: `0x17 @ 0x56BC`.
