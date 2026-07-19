# CARVE ANSWER (V4) - the fixed [0,70] is the LINE-LENGTH COUNT: token end = r.0x94 - 1, and r.0x94 = 71 in your runs

Answers [CARVE-REQUEST-LINKER-LOAD-ERROR52-V4.md](CARVE-REQUEST-LINKER-LOAD-ERROR52-V4.md).
Binary: `D:\ND\500\nd-linker\linker-b01.dom` (+ `.dom.asm`).
Addressing: PSEG file = VA - 0xB0000000 + 0x1000; DSEG file = VA - 0xB0000000 + 0x57800.
Tags: [V] byte-cited, [I] inferred with basis.

## TL;DR

- **V3's "delimiter-free 71-char token" explanation is retracted.** I had the
  scanner's inner loop branch backwards. For the file-name parameter kind, the
  scanner **consumes NUL, space, CR and every other char <= 0x20 silently** -
  they are NOT terminators. The only content terminators are `%`, `&`, `,`,
  `=`. Everything else scans on until the COUNT limit. [V]
- **The count limit is `r.0x94 - 1`**, read LIVE from the input context
  0xB0049140 field +0x94 (VA **0xB00491D4**) inside the scanner
  (B0036725-B0036729). Token = [start, r.0x94 - 1] when no `%&,=` occurs.
  Your fixed b.0x4C = 0x46 = 70 therefore means **r.0x94 = 71 at scan time in
  BOTH runs** - input-independent because it is the LINE LENGTH the line
  reader reported, not anything in the text. [V for the limit mechanism;
  r.0x94 = 71 is its arithmetic consequence]
- **r.0x94 is set at B003519B**: `r.0x94 := b.0x64 - b.0x60 + 1` = the length
  of the descriptor returned by line reader **B003F876** (called at B0035151).
  B003F876 is the line-editor/history layer (0xB004D8xx state, 162-byte
  (0xA2) history records); the raw OS read is below it. **Your emulator's
  input path is delivering/claiming 71-byte lines.** [V for the assignment
  chain; the emulator-side attribution is I]
- **Not stale** (your Q3): b.0x44/0x48/0x4C are rebuilt every call - scanner
  token -> local copy via B00401FC -> b.0x44. The cross-run constancy reflects
  a constant r.0x94 = 71, not un-reinitialized frame memory. [V]
- **Decisive probe: write-watch VA 0xB00491D4.** Expect a write of 0x47 (71)
  right after each line read (from the B003515A-B003519B block). Then trace
  where B003F876 got a 71-length descriptor - that is your bug, one level
  above your MON handler. On real HW typing `B:NRF` + CR the reader returns
  length 5, token [0,4], 6 <= 65, check passes.

## 1. Q1: what b.0x34/0x38 and b.0x48/0x4C are

**b.0x30/0x34/0x38** = the caller's destination descriptor. For LOAD:
{&LOAD.b.0x18, 0, 0x40} = the 65-byte name slot, built as constants at
B0016471-B001647B (`stz r.0x4; move $0x40,r.0x8`). Unchanged from V3. [V]

**b.0x44/0x48/0x4C** = the accepted-parameter descriptor. On the path your
runs took (byte-walked):

1. Kind dispatch: `w comp2 b.0x2C,$0xD2 ; if < go` at B0034985-B003498A routes
   kind 0x42 to **B0034A0E**. [V]
2. Scanner **B0036620** called at B0034A2C with the stream descriptor (copy of
   context r.0x0) and the kind-0x42 char bitmap **0xB0048F20** (file 0xA0720:
   bits for NUL, space, `%`, `&`, `,`, `=`); result descriptor r.0x44 ->
   b.0x44, mirrored to b.0x50 (B0034A35-B0034A3A). [V]
3. Alternatives matcher B003C66E (B0034AEB); your runs: no match, r1 = 0
   (required for step 4's path). [V code, I that r1=0 - proven by the pointer]
4. Line-end local-copy path B0034B1E-B0034B50: build desc
   {&b.0x98, 0, 0xFF} in b.0x68, copy the token into local buffer b.0x98 via
   **B00401FC** (B0034B34), store B00401FC's RESULT descriptor back to b.0x68
   (B0034B3D), then **b.0x44 := b.0x68** (B0034B50). Your observed
   b.0x44 = 0xB000230C = B(0xB0002274) + 0x98 proves this exact path ran. [V]
5. Accept: `w move b.0x8C,b.0x3C` (out-info), **clear error cell at B0034C2F**,
   `go $0x60A` -> **B003523F** = the delivery epilogue containing the range
   check. So your watch sequence (0x9011 armed -> cleared @B0034C2F -> 0x9016
   @B0035291) is ONE round, not two. [V]

B00401FC decoded [V] (B00401FC-B004024F): generic bounded copy;
returns, in its arg slot r.0x20, the descriptor
**{dest.ptr, dest.lo, dest.lo + copied_len - 1}** (the `w2 := b.0x1C - b.0x18
+ 1 (+r1)` arithmetic at B0040214-B004021E with r1 = 0 here). So
b.0x4C = 0x46 <=> **the scanner token was 71 chars long**.

## 2. Why the token is 71 chars regardless of content - the scanner loop [V]

B0036620, non-quoted path (first char not 0x27), the token loop:

```
B003671F: r := $0xB0049140
B0036725: w3 := r.0x94 ; - $0x1 ; =: b.0x98    ; LIMIT = count - 1  <- THE 70
B003672B: w2 comp r3 ; if > go $0x3E           ; empty -> out
B003672F: w1 := b.0x50 ; =: b.0x54
B0036733: by2 := @b.0x14+                       ; ch = next char
B0036736: w2 =: b.0x58
B0036738: w2 comp $0x20
B003673B: if > go $0x8                          ; ch >  0x20 -> delimiter tests
B003673D: d loopi b.0x50,b.0x98,-0xE            ; ch <= 0x20 -> KEEP SCANNING
B0036741: go $0x2A                              ; count exhausted -> token ends
B0036743: comp $0x25 / $0x26 / $0x2C / $0x3D    ; % & , = -> token ends
```

Read it carefully: the `if > go` at B003673B branches AWAY to the delimiter
tests only for printable chars; **chars <= 0x20 (NUL, space, CR, controls)
fall into `loopi` and are consumed**. With no `%&,=` in the text the loop
runs to the count limit and the token ends at index r.0x94 - 1. (The quoted
path has the same shape: unclosed quote also ends at `r.0x94 - 1`,
B00366D8.) [V]

Correction to V3 [poisoned prior]: V3 cited the OTHER bitmap (0xB0048F40, the
non-filename kinds) and claimed "space is not a delimiter, your line was 71
delimiter-free chars". Wrong twice: the filename kind uses bitmap 0xB0048F20,
and in this loop the bitmap chars <= 0x20 never terminate anyway - the token
length here is COUNT-driven, not content-driven.

## 3. Q2/Q3: where the 71 comes from, and why it is not stale

Inside B003472C, the "need another line" path:

```
B0035151: call B003F876                          ; line reader (editor/history layer)
B003515A: w bmove r.0x18,b.0x5C,$0x3             ; returned line descriptor
B0035163: (copy whole line into 0xB0048FEC via B00401FC at B0035177)
B003518F: w3 := b.0x64 ; - b.0x60 ; + $0x1       ; line length
B003519B: w3 =: r.0x94                            ; count := LINE LENGTH  <- THE 71
B00351A1: w bmove $0xB0049918,r.0x0,$0x3          ; stream desc reset {0xB0048FEC,0,0xFF}
```

So r.0x94 = the length of whatever descriptor **B003F876** returned. [V]
B003F876 (entry B003F876, frame 0x54) is the command-line EDITOR: it manages
the 0xB004D8xx state cluster and a line-history array with 0xA2 (162)-byte
records (`w3 * $0xA2` at B003F932); the actual character/OS input happens in
its callees. I did not walk it to the MON instruction - your live rig gets
there faster (see probes). [V for what is cited; the routine's full behavior
is not carved]

**Q3 answer: not stale.** Every B003472C call rebuilds b.0x44/0x48/0x4C from a
fresh scan (step chain in section 1); the scan limit is read LIVE from
0xB00491D4 inside the scanner. Constant 70 = constant r.0x94 = 71 = constant
line-length reports from the input side. Your run-1 break-time reading of
0xB00491D4 = 0x0A does not contradict this: the value at the 0x9016 break is
post-scan bookkeeping, not the value the scanner used (watch WRITES to catch
the 0x47). [V mechanism; the 0x0A reconciliation is I]

## 4. Q4: caller path and who supplies [0,70]

Confirmed as in V3: LOAD body B00163FD (twin B00165FC) -> prompt/default/dest
setup B0016458-B0016481 -> call B003472C at B001648A (twin B0016689). The
caller passes ONLY the [0,64] destination; **the [0,70] is not passed by
anyone** - it is manufactured inside the call from the live line-length count
as shown above. [V]

## 5. Probes for nd500x (in order of decisiveness)

1. **Write-watch VA 0xB00491D4** (context 0xB0049140 + 0x94). You should see
   0x47 (71) written by the B003519B block (or by the entry-path variant at
   B003483C which also sets it from a descriptor length). Whichever
   instruction writes 71, its source descriptor tells you which reader lied.
2. **Break after `call B003F876` (at B0035158)** and read the returned
   descriptor in callee slot r.0x18 (three words: ptr, lo, hi). If
   hi - lo + 1 = 71, descend into B003F876's callees to your MON handler.
3. Sanity: after the fix, typing `B:NRF` should give r.0x94 = 5, token [0,4],
   b.0x4C = 4, and the check passes (6 <= 65).

Expected real-HW behavior for reference: line reader returns the typed length
with CR stripped; r.0x94 = 5; LOAD accepts the name, prompts again; empty
answer leaves parked 0x9011 -> collection ends (B00164B0) -> per-file loader
B0019914 runs. Your run-2 side-win (clean MON 0B LEAVE with the empty-line
terminator) is consistent with all of this and worth keeping.

## Evidence register

From `D:\ND\500\nd-linker\linker-b01.dom.asm` (PSEG VAs) and `.dom` DSEG
bytes at +0x57800:

- Kind dispatch B003497B-B003498A (`2E 4B 0C / CA 07 / 2E 4B 0D / CE 11 /
  2E 4B CE 00 D2 / CB 00 84`).
- Scanner call block B0034A0E-B0034A3A; bitmap 0xB0048F20 file 0xA0720 =
  `80 00 00 00 86 08 00 04 ...` = {00, 20, 25, 26, 2C, 3D}.
- Scanner loop B003671D-B0036752; count limit load B0036725-B0036729
  (`0E A5 / 62 01 / 22 66`); ch<=0x20 fallthrough B0036738-B003673D
  (`35 CD 20 / C8 08 / BF 54 66 F2`); unclosed-quote end B00366D8-B00366E2.
- Local-copy path B0034B1E-B0034B50; B00401FC body B00401FC-B004024F (result
  desc write-back `FD 20 4D 48 0C` at B004024A).
- Accept path B0034C2C-B0034C35 (`1A 63 4F / 4A C4 B0 04 8C FC / C1 06 0A`
  -> B003523F).
- Line-accept block B0035148-B00351A9; count store B003519B (`22 A5`);
  B003F876 entry (frame 0x54) and history stride `6E CE 00 A2` at B003F932.
- LOAD call-site constants B0016458-B001648A (unchanged from V3).
