# Carve — ACCP command 0x14 = CMDWW (024B) handler (2026-08-09)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\CARVE-ACCP-CMDWW-0x14-HANDLER-2026-08-09.md`
**Scope:** E2-P2, first of the 31 unnamed dispatcher arms. Does NOT touch `ACCP-COMPLETE-REFERENCE.md`.
**Reads with:** `CARVE-ACCP-E2-P1-COMMAND-BYTE-MAP-2026-08-08.md` (the arm map),
`CM-SYMBOLS-ARE-THE-OCTOBUS-ARM-CODES-2026-08-03.md` (CM*-symbol = arm-code binding).

## Result

**Command byte `0x14` is `CMDWW` (`0o24`).** Bound authoritatively from `N500-SYMBOLS.SYMB`
(`CMDWW=000024`) — the same source that named CMRUN/CMMIC/CMSTO. The 68000 handler is a
**checksummed 8-halfword ("word") WRITE into the control-store staging buffer at `0x1144F0`**.

It sits in the coherent `0x13–0x16` word-transfer cluster:
`CMWWC 0o23` (0x13), **`CMDWW 0o24` (0x14)**, `CMRWC`/`CMADR` `0o25` (0x15), `CMDRW 0o26` (0x16).
"DWW" expansion is **[INFERRED]** "Data/Double Write Word" (its read twin `CMDRW`=0x16 supports the
write/read pairing); the letters are not expanded in any source I have, so the NAME is `CMDWW`, not
a guessed long form.

## Evidence — the handler (octo.bin, verified in Ghidra 2026-08-09)

Arm at `0x4EDC` reads exactly `cmpi.b #0x14,D0` (matches the carved arm map — confirms the loaded
`octo.bin` image lines up with the carve). Body `0x4EE4`–`0x4FBF`, ending `bra.w 0x6878` (dispatcher
return):

```
4EE4  jsr 0x6FFA            ; read one 16-bit word from the ND-100 message  [6FFA not yet decompiled]
4EEC  move.w D0,(0x52,A6)   ; save it as the first parameter (local @0x52)
4EF0  clr.l (0x64,A6)       ; i = 0
loop (4EF4):
4EF4  jsr 0x6FFA            ; read next word
4EFC  D1 = i; asl.l #1      ; word index -> byte offset
4F02  lea 0x1144F0,A0
4F08  move.w D0,(A0,D1)     ; buffer[i] = word            <-- the 8-halfword / 128-bit staging buffer
4F12  D3 = @0x50 (running sum); add buffer[i]; store back ; checksum accumulator
4F22  i++
4F26  cmpi.l #7,D4; bne loop ; i = 0..7  (8 data halfwords)
4F2E  jsr 0x6FFA            ; read the 9th word (the checksum word)
4F36  @0x50 = @0x50 + word  ; fold checksum word into the sum
4F3E  clr.w 0x1144EC ; clr.w 0x1144EA
4F4A  tst.w 0x1143AC        ; mode flag
4F50  beq 4F74              ; flag==0 -> normal path
      (flag!=0): D0=0xFF; jsr 0x6986 (x2); jsr 0x6A64; -> done   [alternate/abort path]
4F74  andi.l #0xFFFF,D0; tst.l D0
4F7C  beq 4F9E              ; checksum sum==0 -> success
      (sum!=0): D0=0xFF jsr 0x6986; D0=4 jsr 0x6986; jsr 0x6A64  ; NAK code 4 = checksum error
4F9E  D0 = @0x52 (first param); jsr 0x73B2   ; act on the loaded word  [73B2 not yet decompiled]
4FA8  tst.w 0x113138; bne 4F9E                ; wait/ready spin
4FB2  D0 = 0; jsr 0x6986                       ; ACK code 0 = success
4FBC  bra.w 0x6878         ; back to dispatcher
```

Cross-checks:
- `0x1144F0` is the **same 8-halfword staging buffer** the start/stop-microprogram read-back verify
  walks (`cmpi.w #0x0100,(0x0,A0,D0)` at `0x1144F0`) — so this cluster stages 128-bit words there.
- `0x6986` is the reply-byte sender (0xFF = nak marker, then a code; 0 = ack) — same convention
  `AccpCommandChannelTests` already proved for ALIVE (`FF 07 …`).
- Termination `bra.w 0x6878` is the shared dispatcher exit.

## Verified vs inferred

- **[V]** `0x14 = CMDWW = 0o24` (N500-SYMBOLS.SYMB). Arm at `0x4EDC`. Handler reads 8 halfwords +
  1 checksum word into `0x1144F0`, validates `sum & 0xFFFF == 0`, NAKs code 4 on mismatch, ACKs 0 on
  success, uses the `0x1143AC` mode flag for an alternate path.
- **[INFERRED]** the "DWW" long form; the exact action of `0x73B2` (consumes the first param — likely
  the target CS/mem address or a commit) and `0x6FFA` (the per-word message reader). Decompiling
  `0x6FFA`/`0x73B2` is the obvious next step to upgrade these to [V].

## Wrong turns — do not repeat

- The Ghidra ACTIVE program was `encos-err-ii-b01.brf` (an ND-100 BRF), not octo.bin — a bare
  `get_disassembly` returned garbage undefined bytes. ALWAYS pass `program_name: "octo.bin"`.
- octo.bin is loaded from `C:\Temp\octo\octo.bin` (not the NDInsight eprom path); confirmed
  equivalent by the `cmpi.b #0x14` arm landing exactly at the carved `0x4EDC`.

## Open

- 30 unnamed arms remain (worklist in the E2-P1 map). Next in chain order: `0x15 @ 0x4FC0` (`CMRWC`
  family — note the map already shows 0x15 unnamed; SYMB gives `CMRWC/CMADR 0o25`).
