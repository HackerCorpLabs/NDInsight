# CORRECTION: the D4 place-domain hot loop is a DEVICE-I/O POLL (it IS 030-S3SM5; the .dis was corrupt)

## FINAL BYTE-VERIFIED CORRECTION 2026-07-21e (supersedes the "not-S3SM5 / overlay confound" claim below)

I was WRONG that this is a non-S3SM5 overlay. Reading the ground-truth `.bin` directly settles it:
`030-S3SM5.bin` at the runtime PCs matches the RUNTIME TRACE byte-for-byte (base 0x4000, so runtime
word W is at `.bin` byte `(W-0x4000)*2`):

| PC | `.bin` word (ground truth) | runtime trace | agent's `030-S3SM5.dis` (WRONG) |
|---|---|---|---|
| 0xDA50 | `0xD64F` MON 117B | `0xD64F` MON 117B | (not analysed) |
| 0xDAB3 | `0xBA14` JPL I *0x14 | `0xBA14` | `045027` LDA I 27 |
| 0xDAC8 | `0xCC7E` COPY SX DT | `0xCC7E` | `056737` LDX ,X ,B -41 |
| 0xDACA | `0xD10D` IOXT | `0xD10D` | `004004` STA 4 |
| 0xDAAD | `0xD10D` IOXT | `0xD10D` | (n/a) |

So: **the hot loop IS `030-S3SM5` (no overlay confound).** The thing that is wrong is the
agent-generated **`030-S3SM5.dis` FILE** - its words disagree with the `.bin` at the same addresses,
so it is CORRUPT/MISALIGNED and must NOT be used (it caused BOTH the retracted "cell 27B table scan"
AND my mistaken "not-S3SM5" claim). The `.bin` and the live trace are the ground truth and they agree.
The device-poll-with-timeout characterisation below is CORRECT (it is byte-verified from the `.bin`);
only the "not-S3SM5 / different overlay" attribution in the original title/body is wrong.

**Net truth:** the hot loop is `030-S3SM5` code at runtime 0xDA40..0xDAD0 (base 0x4000), a
read-with-timeout loop (MON 117B + dynamic IOXT + retry counter `[B-7A]`).

### .dis REGENERATED + VERIFIED CORRECT 2026-07-21e

Root cause of the corrupt `.dis`: it was disassembled WITHOUT the byte-swap. `030-S3SM5.bin` is
big-endian; `nd100-dis` needs it byte-swapped to LE first. Correct recipe (verified):
```
python3 -c "d=open('030-S3SM5.bin','rb').read(); b=bytearray(len(d)); b[0::2],b[1::2]=d[1::2],d[0::2]; open('le.bin','wb').write(b)"
nd100-dis -a -o -b 040000 le.bin
```
The DIRECT-BE disasm gives garbage (155120 -> `LDA I ,B ,X -52`); the byte-swapped disasm is correct
(155120 -> `MON 117 ; RFILE`). `re/030-S3SM5.dis` has been REGENERATED with the byte-swap and verified
against the `.bin`/runtime at 155120/155205/155255/155263/155310/155312 (all match). Use it now.

**KEY: `MON 117B` = `RFILE` (ReadFromFile)** [V, nd100-dis annotation]. So this loop is a FILE-READ
loop (reading an image/segment) plus IOX, with a retry/timeout counter `[B-7A]` - it is LOADING
something and timing out, NOT a pure hardware status poll. Re-analyse the loop on the corrected `.dis`.

**Downstream cleanup:** the two earlier S3SM5 docs (`CARVE-S3SM5-MSWIN-STAMP-AND-FILL-...`,
`CARVE-S3SM5-CSLOAD-VERIFY-LOOP-...`) were built on the OLD corrupt `.dis`; their address-level claims
(builders @140771/162155, cell 27B) must be RE-DERIVED from this corrected `.dis`.

---

# (original, partially-wrong) CORRECTION: the D4 place-domain hot loop is a DEVICE-I/O POLL, not the S3SM5 table scan

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\CORRECTION-HOT-LOOP-IS-IOX-POLL-NOT-S3SM5-2026-07-21.md`
**Date:** 2026-07-21
**Grade key:** [V] byte-verified from a live trace; [I] inference; [OPEN] not yet established.

## What is retracted

Commits `da22546` and `e830dda` (and the "HISTOGRAM-PINNED LOOP + EXACT CELL" section of
`CARVE-S3SM5-CSLOAD-VERIFY-LOOP-2026-07-21.md`) concluded the non-terminating D4 place-domain hot loop
is an **S3SM5 software table scan** whose gate is **low-core cell 27B + a record `+42B` bit-3**. That
is **RETRACTED**. It rested on the static decode `030-S3SM5.dis`, but a live REGISTER trace of the
running code proves the runtime bytes at those PCs are NOT the S3SM5 segment (overlay confound).

Earlier "no confound" cross-check was insufficient: it checked that the S3SM5 decode *has code* at
those addresses, NOT that the runtime *bytes* match. They do not.

## The byte proof [V]

Live trace (D4 harness, `Nd500_D4_RunDomain_RealCpu_Capture`, ND-100 register trace) vs `030-S3SM5.dis`:

| PC | runtime word (executed) | runtime disasm | S3SM5.dis word at same octal addr | S3SM5.dis disasm |
|---|---|---|---|---|
| 0xDAB3 (155263B) | `0xBA14` (135024B) | `JPL I *0x14` | `045027` | `LDA I 27` |
| 0xDAC8 (155310B) | `0xCC7E` (146176B) | `COPY SX DT` | `056737` | `LDX ,X ,B -41` |
| 0xDACA (155312B) | `0xD10D` (150415B) | `IOXT` (an IOX!) | `004004` | `STA 4` |

Different bytes -> the carved `030-S3SM5` segment is NOT what is mapped/executing at these PCs at
runtime. So the "cell 27B" gate does not describe the running code.

## What the loop ACTUALLY is [V, from the live trace]

A **device-poll-with-timeout** routine (some non-S3SM5 overlay, PC band ~`0xDA40..0xDAD3`). Real
runtime disassembly (addr, executed word, disasm):

```
0xDA40 09EC  STA -0x14,B
0xDA41 4986  LDA -0x7A,B        ; A := retry counter [B-7A]
0xDA42 F201  SAT 0x1
0xDA43 C035  SKP IF DA EQL ST
0xDA44 A803  JMP *0x3 -> 0xDA47
0xDA47 4855  LDA *0x55
0xDA48 0988  STA -0x78,B
0xDA49 4980  LDA -0x80,B
0xDA4A 098B  STA -0x75,B
0xDA4B BA52  JPL I *0x52        ; call (via pointer)
0xDA4D BA51  JPL I *0x51        ; call
0xDA4F 49F6  LDA -0xA,B
0xDA50 D64F  MON 0x4F           ; MON 117B  <-- monitor call
0xDA51 C005  SKP IF DA EQL 0
0xDA53 4984  LDA -0x7C,B
0xDA54 B327  JAF 0x27 -> 0xDA7B ; if A false (fail) -> counter-decrement
0xDA55 F308  SAX 0x8
0xDA56 4E4A  LDA ,X I *0x4A
0xDA57 59D2  LDX -0x2E,B        ; X := device/interface base [B-2E]
0xDA58 0CF9  STA -0x7,X
0xDA59 CC69  COPY SA DD
0xDA5A 0410  STZ 0x10,X
0xDA5B 5046  LDT *0x46
0xDA5C C335  SKP IF DA MGRE ST
0xDA5E 5044  LDT *0x44
0xDA5F C735  SKP IF DA MLST ST
0xDA61 F202  SAT 0x2
0xDA62 A802  JMP *0x2
0xDA64 59D2  LDX -0x2E,B
0xDA65 4C17  LDA 0x17,X
0xDA66 703D  AND *0x3D
0xDA67 CB35  RORA ST DA
0xDA68 0C17  STA 0x17,X
0xDA69 CC45  RCLR DA
0xDA6A F264  SAT 0x64           ; T := 100
0xDA6B C3B0  RDIV ST            ; divide by 100 (timing/scale)
0xDA6C 5038  LDT *0x38
0xDA6D C32E  SKP IF DT MGRE SA
0xDA6E A804  JMP *0x4
0xDA72 CC46  RCLR DT
0xDA73 F886  BSET ONE 0 DT
0xDA74 A802  JMP *0x2
0xDA76 59D2  LDX -0x2E,B
0xDA77 4CF8  LDA -0x8,X
0xDA78 F805  BSET ZRO 0 DA
0xDA79 CB35  RORA ST DA
0xDA7A 0CF8  STA -0x8,X
0xDA7B 4986  LDA -0x7A,B        ; A := retry counter
0xDA7C F5FF  AAA -0x1           ; counter -= 1
0xDA7D 0986  STA -0x7A,B
0xDA7E 5985  LDX -0x7B,B
0xDA7F 4826  LDA *0x26
0xDA80 0989  STA -0x77,B
0xDA81 498B  LDA -0x75,B
0xDA82 BA24  JPL I *0x24
0xDA83 F1F7  SAA -0x9
0xDA84 098A  STA -0x76,B
0xDA85 5188  LDT -0x78,B
0xDA86 C037  SKP IF DX EQL ST
0xDA87 A820  JMP *0x20 -> 0xDAA7
0xDAA7 CC79  COPY SX DD
0xDAA8 4E1E  LDA ,X I *0x1E
0xDAA9 59D2  LDX -0x2E,B
0xDAAA 54FD  LDT -0x3,X         ; T := [X-3]  (device IOX base from table)
0xDAAB CC77  COPY ST DX
0xDAAC F60B  AAT 0xB            ; T += 0xB  -> IOX register
0xDAAD D10D  IOXT               ; IOX[T]    <-- device I/O
0xDAAE F101  SAA 0x1
0xDAAF BA18  JPL I *0x18
0xDAB0 F108  SAA 0x8
0xDAB1 BA16  JPL I *0x16        ; call IOX subroutine (0xDAC8)
0xDAB2 F105  SAA 0x5
0xDAB3 BA14  JPL I *0x14
0xDAB4 4977  LDA -0x77,B
0xDAB5 CC7D  COPY SX DT
0xDAB6 F60B  AAT 0xB
0xDAB7 D10D  IOXT               ; IOX[T]
...
```

Key features [V]:
- **`MON 0x4F` = MON 117B** at 0xDA50 (a monitor call inside the poll; identity [OPEN]).
- **`IOXT`** (IOX with device address in T) at 0xDAAD / 0xDAB7 ..., device number computed dynamically
  from a device/interface table: `X := [B-2E]`, `T := [X-3] + 0xB`. So it polls a HARDWARE device
  register, address taken from a runtime interface descriptor.
- **Retry counter `[B-7A]`** decremented at 0xDA7C; `RDIV` by 100 (0xDA6A/0xDA6B) = a timed/scaled
  wait. `JAF 0x27` @0xDA54 branches to the counter-decrement on a failed poll.
- So it is a **poll-with-timeout**: it repeatedly calls MON 117B + IOX to check device/swapper
  readiness, decrementing a retry counter; when readiness never comes it exhausts the counter and
  place-domain reports "The Swapper stopped".

## Consequence for task 8

The task-8 blocker is a **hardware/monitor readiness poll that never goes ready** (much closer to the
original "CS-load / swapper-ready verify" framing than the retracted "software table scan"). The fix
is still SERVICER-side in spirit (make the ND-500 side actually reach the ready state the poll waits
for), but the EXACT gate is now:

- [OPEN] which overlay/segment this routine belongs to (identify by byte-matching the runtime words
  above against the carved segments - do NOT trust `030-S3SM5.dis` here).
- [OPEN] what MON 117B does and what IOX register `[X-3]+0xB` reads (the readiness bit).
- [OPEN] why the emulator never makes that readiness true, and where to fix it (3022 `NDBusND500IF`
  status vs servicer vs octobus).

## Method lesson (pin this)

A live PC trace shows the ADDRESS executed; the carved segment for that address is only correct if the
runtime BYTES match. ALWAYS compare the executed word against the segment's word before attributing a
running PC to a carved segment. Overlays make the same virtual address decode differently per mapping.
