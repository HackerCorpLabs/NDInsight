# ND Ethernet II 68K: OPCOM SUBFUNCTION=5 exchange - SUCCESS or ERROR?

Date: 2026-07-23. Image (MC68000, BE, base 0x0, 512 KB):
`E:\Dev\Ronny\NDInsight\Installation\Communication\Ethernet\x\stripped\encos-ser-all-banks-68k.bin`
Tooling: scratchpad `m68kdis.py`.
Legend: **[V]=VERIFIED** (decoded bytes) - **[I]=INFERRED**.

---

## ONE-LINE VERDICT

**[V] The SUBFUNCTION=5 / MON_CODE=1 / PARAM=0x1E(30) exchange is a NORMAL, SUCCESSFUL
handshake step - an ACK, not an error.** In this firmware the monitor result code (postbox
word 0x40C) is a **signed** status: **>=0 (here =1) = success/ACK; negative (-2, -4, -5) =
error**. SUBFUNCTION=5 takes the success path and posts MON_CODE=+1. **PARAM=30 is a fixed
constant the OPCOM handler always stamps (= the OPCOM vector number 0x1E), not a computed
result or error code.** The separate `D0=-5` (REQUEST=0) case is the **"no request pending /
stale doorbell"** guard - benign, not a fatal comms failure by itself.

---

## 1. The OPCOM level-6 handler and its dispatcher  [V]

Vector 0x1E lives at 68K addr 0x78; `long[0x78] = 0x00001B00`. GPIP-I6/ND-INT vector 0x4E
(`long[0x138]`) = 0x0000250E (the channel scanner, separate mechanism - see section 4).

Decoded handler at **0x1B00** (misdecoded words corrected: `4880`=`ext.w D0`;
`4880 41F9 0000 040A`+`3140 0002` = `ext.w D0 / lea ($40A),A0 / move.w D0,(2,A0)` i.e.
**write D0 -> 0x40C = MON_CODE**; `4279 0000 04C0` = `clr.w ($4C0)`):

```
1B00: move.l A0,($500).L                 ; save A0
1B06: jsr    $1A30                        ; nd_monitor_set_flag (sets 0x412 reqflag)
1B0A: move.w #0,($EF0020).L               ; ACK / clear the OPCOM doorbell latch
1B12: lea    ($40A).L,A0                  ; A0 = postbox base
1B18: move.w #$1E,(4,A0)                  ; **PARAM (0x40E) = 0x1E = 30  (CONSTANT, always)**
1B1E: move.w #0,(2,A0)                    ; MON_CODE (0x40C) = 0  (default)
1B24: lea    ($406).L,A0                  ; A0 = REQUEST cell
1B2A: cmpi.w #0,(A0)                      ; REQUEST(0x406) == 0 ?
1B2E: bne    $1B42
1B30:   moveq #-5,D0                       ; ** REQUEST==0 -> D0 = -5 **
1B32:   ext.w D0 / lea ($40A),A0 / move.w D0,(2,A0)   ; MON_CODE = -5
1B3E:   bra   $1C0A                        ; -> epilogue (still SCIP-signals ND-100)
1B42: move.w #0,(A0)                      ; consume REQUEST := 0
1B46: cmpi.w #0,(2,A0)                    ; SUBFUNCTION(0x408) == 0 ?
1B4C: bne    $1BAA
        ; --- SUB==0 : first-time START ---
1B4E:   cmpi.w #0,($4C0).L                 ; STARTED flag already set?
1B56:   bne   $1B9A                        ;   yes -> error -4 (already started)
1B58:   ... (start body: 0x500 ctx, jsr, tst 0x650 / jsr $3A58) ...
1B80:   move.w #1,($4C0).L                 ; STARTED := 1
1B88:   moveq #1,D0 ; MON_CODE = +1         ; SUCCESS
1B96:   bra   $1C0A
1B9A:   moveq #-4,D0 ; MON_CODE = -4        ; ERROR: already started
1BAA: cmpi.w #1,(2,A0)                     ; SUBFUNCTION == 1 ?
1BB0:   bne  $1BD4
1BB2:   cmpi.w #0,($4C0).L ; bne $1BD4     ; SUB==1 & not-started -> park:
1BC2:     move.w #$FFFC,(2,A0)              ;   MON_CODE = -4, then
1BC8:     jsr (via 0x500) ; rte             ;   (special early return)
        ; --- general table dispatch ---
1BD4: lea    ($406).L,A0
1BDA: move.w (2,A0),D0                     ; D0 = SUBFUNCTION
1BDE: cmpi.w #0,D0 ; blt $1BFC             ; SUB < 0  -> error -2
1BE4: cmpi.w #5,D0 ; bgt $1BFC             ; SUB > 5  -> error -2  (valid range 0..5)
1BEA: asl.w  #2,D0                          ; index * 4
1BEC: lea    ($512).L,A2                   ; jump table base
1BF2: lea    (0,A2,D0.W),A1
1BF6: move.l (A1),A1
1BF8: jsr    (A1)                           ; ** call table[SUBFUNCTION] **
1BFA: bra    $1C0A
1BFC: moveq #-2,D0 ; MON_CODE = -2          ; ERROR: subfunction out of range
1C0A: addq.l #6,A7                          ; epilogue
1C0C: jsr    $1A48                          ; post_and_signal_nd100_scip (postbox + SCIP INT12)
1C10: move.l A7,($500).L
1C16: rte
```

**Dispatch = a jump table at 0x512, indexed by SUBFUNCTION*4 (long entries)** [V]:

| SUB | table@ | target | role (decoded / inferred) |
|-----|--------|--------|---------------------------|
| 0 | 0x512 | 0x001A10 | (also handled inline: first-time START, sets STARTED) |
| 1 | 0x516 | 0x001C1A | clr STARTED; MON_CODE=1; jsr; bsr $1A12; jmp (A5) |
| 2 | 0x51A | 0x001C38 | MON_CODE=1; rts  (thin ACK stub) |
| 3 | 0x51E | 0x001C38 | same stub |
| 4 | 0x522 | 0x001C38 | same stub |
| 5 | 0x526 | 0x001C48 | **SUB=5 handler (below)** |

Range guard is 0..5, so **SUBFUNCTION=5 is a VALID, in-range request** (not the -2 path).

---

## 2. The SUBFUNCTION=5 path (0x1C48) - what it computes / writes  [V]

```
1C48: moveq #1,D0
1C4A: ext.w D0
1C4C: lea   ($40A).L,A0
1C52: move.w D0,(2,A0)          ; ** MON_CODE (0x40C) = +1 **  (ACK)
1C56: clr.w ($4C0).L            ; ** STARTED flag (0x4C0) = 0 **  (clears "started")
1C5C: jsr   $1A48               ; post_and_signal_nd100_scip: postbox counters++ + SCIP INT12
1C60: move.l ($4B6).L,A1        ; A1 = function pointer held in slot 0x4B6
1C66: jsr   (A1)                ; ** indirect call through 0x4B6 **
1C68: jmp   (A5)                ; return via saved A5
```

- **MON_CODE=1 is written at 0x1C52** (D0=+1). This is the *same* +1 that every success path
  writes; it is the ACK code.
- **PARAM=0x1E(30) is NOT written here.** It was stamped once, unconditionally, at handler
  entry `0x1B18: move.w #$1E,(4,A0)`. It is a hard constant equal to the OPCOM vector number
  (0x1E). [I] It reads as a "message source / OPCOM postbox" tag, not a version, size, or
  error. (Note: routine 0x2562 range-checks a code against `#$1E` (30) as an *upper bound* for
  a 0..30 dispatch table at 0xA8A - so 30 is also the top of a message-type space; the reuse of
  the value is consistent with "type/source tag", but the OPCOM handler simply hardcodes it.)
- post_and_signal_nd100_scip (**0x1A48**) [V]: `addi.w #1,($40A)` (counter++),
  `addi.w #1,(6,$40A)` (counter2++), then `move.b #1,($EF0080)` = **SCIP -> INT12 to ND-100**.
- The indirect callee at **0x4B6** is a function-pointer RAM slot (populated at runtime by init,
  not an immediate in the image - only readers/address-takers found at 0x1C60 and in the
  pointer-table setup 0x1C6A). Its exact target is NOT resolved in this pass. [I] Combined with
  `clr STARTED (0x4C0)`, SUB=5 looks like a **stop / re-arm / teardown** style request (it
  *clears* the started flag that SUB=0 *sets*), but I did not decode the callee, so I do not
  assert the precise semantic name.

---

## 3. The `D0 = -5` / REQUEST=0 path  [V]

`0x1B2A: cmpi.w #0,(0x406)` -> if REQUEST==0, `0x1B30: moveq #-5,D0`, MON_CODE:=-5,
`bra $1C0A` (epilogue still calls post_and_signal -> the ND-100 does get a postbox update with
MON_CODE=-5).

Meaning [V]+[I]:
- The handler **consumes REQUEST by writing 0 to 0x406 after it dispatches** (0x1B42). So a
  *second/stale* doorbell that arrives when the request box is already empty sees REQUEST==0
  and returns -5. [I] -5 is therefore the **"no request pending / stale doorbell"** guard, not a
  protocol/transport failure.
- It is a *signed-negative* status, so a strict ND-100 supervisor that treats "MON_CODE<0" as
  "operation failed" would flag it. **Whether ENNS0 aborts on -5 is an ND-100-side decision and
  is NOT determined from this image** (the ND-100 ENNS0 code is not in this binary). Structurally
  on the 68K side it is benign - the box was simply empty.

---

## 4. The ND-INT (GPIP I6, vector 0x4E) handler 0x250E  [V]

`0x250E` is a **channel scanner**, a different mechanism from the OPCOM request box: it loops
`D1 = 0x0E .. 0` step 2 over the 8 flag words at **0x0B56**, and for each set flag builds an
entry from tables at 0xBE8 / 0xB66 into 0xC0E. It does **not** read REQUEST(0x406); it never
produces the -5. The `-5` is unambiguously the OPCOM/request-dispatcher guard at 0x1B30. (A
sibling routine at 0x2562 does a signed range check `blt` / `cmpi.w #$1E` and indexes table
0xA8A - that is the 0..30 message-type dispatcher, where the 30 = 0x1E bound reappears.)

---

## 5. Status-code convention (the basis of the verdict)  [V]

Every result the handler posts to MON_CODE (0x40C), by branch:

| Condition | MON_CODE | meaning |
|-----------|----------|---------|
| REQUEST==0 (stale) | **-5** | no request pending |
| SUB==0 but already started | **-4** | already started |
| SUB==1 & not started (park) | **-4** (0xFFFC) | not-started park |
| SUB<0 or SUB>5 | **-2** | subfunction out of range |
| SUB==0 first start | **+1** | OK, started |
| SUB==1/2/3/4 | **+1** | OK |
| **SUB==5** | **+1** | **OK (ACK)** |

Positive (=1) = success; every error is a distinct negative. **MON_CODE=1 for SUB=5 is a
success ACK.**

---

## VERDICT

**SUCCESS.** The ND-100 write REQUEST=1 / SUBFUNCTION=5 -> 68K OPCOM handler (0x1B00) ->
in-range table dispatch -> 0x1C48 -> **MON_CODE=+1 (ACK)** + SCIP INT12, with PARAM=0x1E(30) a
fixed source-tag constant (not error/version/size). This exchange does **not** signal a
firmware-side error and would **not, on its own, make ENNS0 abort startup**. The separate
REQUEST=0 -> D0=-5 case is a benign "no request pending / stale doorbell" guard; it is a
negative status but reflects an empty request box, not a transport failure. Whether ENNS0
treats a -5 as fatal is an ND-100-side question outside this firmware image. [V] for all 68K
decoding above; [I] where marked (0x4B6 callee identity, PARAM semantic label, ND-100 reaction
to -5).

## Addresses (evidence anchors)
- OPCOM handler: 0x1B00 (vec 0x1E @ 0x78 -> 0x1B00)
- REQUEST/-5 guard: 0x1B2A cmpi / 0x1B30 moveq #-5
- PARAM=0x1E stamp: 0x1B18
- range guard 0..5 + table: 0x1BDE..0x1BF8, table @0x512
- SUB=5 handler: 0x1C48 (MON_CODE=1 @0x1C52; clr STARTED @0x1C56; SCIP @0x1C5C; ind. call @0x1C60)
- post_and_signal_nd100_scip: 0x1A48 (SCIP write 0x1A5C -> 0xEF0080)
- ND-INT channel scanner: 0x250E ; message-type (0..30) dispatcher: 0x2562 (table 0xA8A)
