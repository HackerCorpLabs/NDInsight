# XMSG magic number (MAGNO) - bit layout CARVED (2026-07-26)

Status: **VERIFIED** by static disassembly of the XMSG kernel. Supersedes the
"UNKNOWN / NOT YET EXTRACTED" entry in
`ENNS0-XROUT-GETMAGIC-FINDINGS-2026-07-07.md` (section "byte layout").

---

## TL;DR - the layout

```
MAGNO (32 bit, carried in the A:D register pair, A = high word, D = low word)

 bit  31              16 15                7 6           0
     +------------------+-------------------+-------------+
     |  system number   |    port number    |   random    |
     +------------------+-------------------+-------------+
       A register (16)     9 bits, 1-based     7 bits

 A = system number
 D = (port number << 7) | random
```

- **System number**: the whole high word, taken from the XMSG base field at `B+60`
  (symbol at that displacement: `XSYID`).
- **Port number**: 9 bits, **1-based**, derived from the port block's index in the
  port table (block size 7 words).
- **Random part**: 7 bits, values `1..126` (0 and 127 are rejected and redrawn),
  held in the port block at displacement 3 (symbol at that displacement: `RNMAG`).
  It is NOT random - it is a linear congruential generator, see section 2 below.

This is what makes a reopened port improbable to collide: only the 7-bit random
part changes, giving 1 in 126 per reuse - but because the generator is an LCG whose
low seven bits have full period 128, the value is fully predictable from any earlier
one.

---

## Why the manual does not tell you this

ND-60.164.3 (COSMOS Programmer Guide) deliberately treats MAGNO as opaque and
exposes conversion FUNCTIONS instead - `XFP2M` (port to magic, appendix A section
4.5) and `XFM2P` (magic to port and system, section 4.4). The layout is an
implementation detail of the kernel, so it had to be carved.

**Consequence for application code: keep using XFP2M / XFM2P.** The layout below is
for tooling, decoders and emulation - not a licence to build magic numbers by hand
in code that has a kernel available.

---

## Evidence

### Artifacts

| Item | Path |
|---|---|
| Kernel image | `DOC/COSMOS-RE/ENNS0-Startup-RE-2026-07-23/xmsg-L-binaries/XMSG-KERNEL-L03.BPUN` |
| Symbol table | `DOC/COSMOS-RE/ENNS0-Startup-RE-2026-07-23/xmsg-L-binaries/XMSG-SYMBOL-L03.SYMB` |
| BPUN loader | `DOC/COSMOS-RE/ENNS0-Startup-RE-2026-07-23/tools/bpun_load.py` |

BPUN load result (checksums OK, 0 bad):
`load_base = 120000, span 120000..175776 (23551 words), start_addr = 160616`.

Instruction field layouts used for decoding are the ND-06.014.02 diagrams quoted
verbatim in the RetroCore ND-100 CPU sources
(`Emulated.HW/ND/CPU/ND100/Instructions.ShiftInstructions.cs`,
`.BitOperationInstructions.cs`, `.Skip.cs`, `.RegisterOperations.cs`). The
register-field numbering is `0=none, 1=D, 2=P, 3=B, 4=L, 5=A, 6=T, 7=X`, confirmed
by decoding the known idiom `146142 = COPY SL DP = EXIT`.

Note: memory-reference displacements are relative to the address of the CURRENT
instruction (`EA = P + disp`), not the next one. Getting this wrong makes the
control flow read as nonsense (jumps landing on pointer words).

### 1. `ZCRMG` - create magic number (kernel 131055)

```
131055  174220  ZCRMG:  BSET ONE 2 D0      ; clear/prepare K flag
131056  146175          COPY SX DA         ; A := X   (address of the port block)
131057  064444          SUB  44,B          ; A -= port-table base
131060  146151          COPY SA DD         ; D := A
131061  146105          COPY S0 DA         ; A := 0        (A:D = 32-bit index)
131062  171007          SAT 7              ; T := 7        (port block size, words)
131063  141660          RDIV ST D0         ; A := AD/7, D := AD mod 7
131064  140001          SKP IF DD EQL S0   ; remainder must be 0
131065  124004          JMP  *4  ; =131071 ;   ...else error exit (cf. XXEIE
131066  050414          LDT  14,B          ;   "Illegal port address in the
131067  141465          .WORD 141465       ;    creation of magic number")
131070  124003          JMP  *3  ; =131073
131071  135012          JPL  I *12 ; =131103
131072  000001          .WORD 000001
131073  172401          AAA 1              ; A := index + 1  -> PORT NUMBER (1-based)
131074  175220          BSKP ONE 2 D0      ; K flag set?
131075  124005          JMP  *5  ; =131102 ;   ...yes: exit early
131076  156407          SHA ZIN 7          ; A := port << 7        <-- SHIFT
131077  076003          ORA  3,X           ; A |= [X+3] = RNMAG    <-- RANDOM
131100  146151          COPY SA DD         ; D := (port << 7) | random   = LOW word
131101  044460          LDA  60,B          ; A := XSYID = system number  = HIGH word
131102  146142          COPY SL DP         ; EXIT   -> A:D = MAGNO
```

`SHA ZIN 7` is decoded from the shift-group fields: bits 10-9 = `10` = ZIN (zero
end input), bits 8-7 = `10` = A register, bits 5-0 = `+7` = shift left 7.

### 2. `ZRAND` - the random part is 7 bits, and is an LCG (kernel 131152)

```
131152  044375  ZRAND:  LDA  *-3           ; seed
131153  131404          JAN  ...           ; linear congruential step
131155  045013          LDA  I *13
131157  120371          MPY  *-7
131160  060371          ADD  *-7
131161  004366          STA  *-12          ; store new seed
131162  171177          SAT 127            ; T := 177 octal                <-- MASK
131163  144465          RAND ST DA         ; A := A AND T  -> 7 bits
131164  131002          JAZ  *2  ; =131166 ; A == 0   -> redraw
131165  142065          SKP IF DA UEQ ST   ; A != 127 -> skip the redraw
131166  125003          JMP  I *3 ; =[131171]=131152 ; back to ZRAND
131167  146142          COPY SL DP         ; EXIT
```

`SAT 127` + `RAND ST DA` is the whole argument for the 7-bit width. The three words
after it are the rejection loop: **0 and 127 are both redrawn**, so the random part
is `1..126`. (`131164` jumps to the redraw when the masked value is zero; `131165`
skips over the redraw only when the value differs from T, which still holds 127.)

**The generator is a linear congruential generator, not a random source.**

```
131152  044375  ZRAND:  LDA  *-3           ; A := seed at 131147
131153  131404          JAF  *4  ; =131157 ; seed non-zero -> skip first-time init
131154  174200          BSET ONE 0 D0
131155  045013          LDA  I *13         ; seed := mem[mem[131170]] = mem[004137]
131156  174000          BSET ZRO 0 D0      ;   (a SINTRAN resident cell, below the
131157  120371          MPY  *-7           ;    XMSG load base - not in this image)
131160  060371          ADD  *-7
131161  004366          STA  *-12          ; seed := seed * 012465 + 033031  (mod 2^16)
```

Multiplier `012465` = 5429, increment `033031` = 13849. The increment is odd and
`a - 1` is divisible by 4, so by Hull-Dobell the generator has full period 2^16 -
and the low seven bits are themselves a full-period generator modulo 128:

```
r' = (53 * r + 25) mod 128        ; cycles through all 128 values
```

So the "random" part is completely deterministic: one observed value predicts every
later one, and a port word's position in that 128-long cycle is the node's
allocation ordinal since XMSG started. This is confirmed against the capture corpus
in `XMSG-WIRE-PORT-IS-MAGIC-LOW-WORD-2026-07-26.md`: 24 distinct low-7 values from
753 wire endpoint fields, all lying on this cycle, including one unbroken run of ten
consecutive generator outputs, and no observed value equal to 0 or 127.

### 3. `MFM2P` - decode magic to port and system (kernel 126774)

Entry test on the port field:

```
126774  026013  MFM2P:  LDD  13,X          ; A := magic high, D := magic low
126775  146116          COPY SD DT         ; T := low word
126776  156171          SHT ZIN SHR 7      ; T := low >> 7  -> PORT NUMBER
126777  140006          SKP IF DT EQL S0   ; port == 0 ?
127000  124007          JMP  *7  ; =127007
```

Result path, which is what the caller sees:

```
127041  026013          LDD  13,X          ; A := high (system), D := low
127042  144015          SWAP SD DA         ; A := low,  D := high (system)
127043  156571          SHA ZIN SHR 7      ; A := low >> 7 -> PORT NUMBER
127044  022013          STD  13,X          ; [X+13] := A = PORTNO, [X+14] := D = SYSNO
```

This matches the manual's documented XFM2P register contract exactly - "A = port
number, D = system number" (appendix A section 4.4) - which is the independent
cross-check that the split is right way round.

---

## Symbol corroboration

| Symbol | Value | Where used | Reading |
|---|---|---|---|
| `RNMAG` | 000003 | `ORA 3,X` in ZCRMG | random-part displacement in the port block |
| `XSYID` | 000060 | `LDA 60,B` in ZCRMG | system id in the XMSG base field |
| `XXEIE` | 1 | - | "Illegal port address in the creation of magic number" - the error the remainder test guards |
| `MFP2M` | 127047 | - | port-to-magic entry (calls the same builder) |
| `ZRAND` | 131152 | - | the random generator |

Symbol values are not unique across the table (several names share a
displacement), so `RNMAG`/`XSYID` are corroboration, not proof. The proof is the
code.

---

## Derived facts

- **Port number range**: 9 bits -> 1..511 usable (0 is the "default port" marker and
  is tested for explicitly at 126777).
- **Port block size**: 7 words; a magic number whose port field does not land on a
  7-word boundary in the port table is rejected at creation.
- **Collision probability on reopen**: 1 in 128 (7 random bits), which is what the
  manual means by "extremely unlikely" and by calling the 16-bit hashed form only
  "almost unique".

## Open / INFERRED

- **VERIFIED (2026-07-26, same day)**: the wire port fields `XMDPT` / `XMSPT` at XMSG
  sub-header offsets 20-21 and 24-25 ARE exactly this low word. See
  `XMSG-WIRE-PORT-IS-MAGIC-LOW-WORD-2026-07-26.md` - the TAD 7CORS port-assign ships a
  whole 32-bit MAGNO (`00 00 <system16> <portword16>`) whose two halves then appear
  verbatim as XMSSY/XMSPT, and the named server `*TADADM` (logical port 2 in the live
  registry) has wire port words that shift down to exactly 2. The paragraph below is
  kept for the record of what was open. `XMSG-PROTOCOL.md`
  section 7.1 describes observed ports as `(logical slot << 7) | low7`, which is
  the same shift and the same 7-bit tail. If that identification holds, the "low7"
  we have been treating as an opaque nuisance IS the magic random part. Proving it
  needs one capture where the same port appears both as a wire port field and in an
  XSGMG/XSGNM reply - the current corpus has no XSGMG traffic.
- **UNVERIFIED**: `B+44` as the port-table base. Read off the code's use, not from a
  unique symbol.
- The `BSET ONE 2 D0` / `BSKP ONE 2 D0` pair around the build is a K-flag protocol
  between ZCRMG and its callers; not decoded, and it does not affect the layout.
- Version: this is the **L03** kernel. The M06 symbol list has the same symbol names
  (`RNMAG=3`, `ZRAND` present), so the layout is expected to be identical in M, but
  the M kernel binary was not on hand to confirm.
