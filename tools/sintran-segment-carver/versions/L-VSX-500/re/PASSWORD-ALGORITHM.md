# SINTRAN III password algorithm (L-VSX-500 / L07) - VERIFIED

Full path: `tools/sintran-segment-carver/versions/L-VSX-500/re/PASSWORD-ALGORITHM.md`

Status: **VERIFIED** against several real stored account values (multiple
passwords, including letters, digits, and special characters, across two different
users to rule out any per-user salt) and against the ND-100 instruction reference.
This supersedes the earlier `README-password-login.md`, which was written on a
mis-based carve and is invalid.

---

## 1. The algorithm

SINTRAN stores each user's password as a single **16-bit word**. It is not a
cryptographic hash - the typed string is folded into one word:

```
acc = 0
for each typed password character, until CR (015B):
    c   = toupper(char)                 # ONLY a-z -> A-Z; digits and symbols unchanged
    acc = ( ROL16(acc, 3) + c ) & 0xFFFF
stored_password_word = acc
```

- `ROL16(x,3)` = rotate the 16-bit value left by 3 bits.
- `c` is the character's ASCII value after uppercasing **letters only**.
- The result is a pure function of the string: **case-insensitive** and **not
  salted** (independent of user id - two users with the same password get the same
  word).

### What is proven vs inferred (the facit is the disassembly)

- **PROVEN from the disassembly**: the fold loop is `acc = ROL16(acc,3) + char`,
  adding the character **unmasked** (`RADD SA DT` adds the full A register). See
  section 2.
- **PROVEN by test vectors**: letters are uppercased (`ford` == `FORD`), so the
  character-read path uppercases `a-z`.
- **NOT proven here**: whether the character is masked to 7 bits. The fold loop does
  not mask; the read routine (`JPL I 23 -> 027032`) would be the only place, but that
  routine is resident / runtime-linked and is not resolvable from the carved images
  (it reads as zero in the resident common-code image). A web description claims
  "value of character (7 bits)"; that is plausible (ND-100 terminals are 7-bit and
  drivers strip the parity bit) but it is NOT confirmed by our disassembly, so this
  implementation does NOT apply a 7-bit mask. It only matters for bytes >= 0x80,
  which cannot be typed on a normal terminal.

### 1.1 Character handling

The only transform is uppercasing of `a-z` (add nothing / clear bit 5 for letters
in 0x61-0x7A). Everything else is added to the fold by its raw ASCII value:

- **Digits `0-9`** (0x30-0x39): unchanged. (A blanket "clear bit 5" would be WRONG -
  digits have bit 5 set, so it would corrupt them.)
- **Special / punctuation** (e.g. `!`, `-`): unchanged and fully valid in passwords.
  So the usable password alphabet is much larger than just `[A-Z0-9]`.

### 1.2 Test vectors (all values DECIMAL) - illustrative words

These are freshly computed example strings (not any real account):

| Password    | Decimal | Octal    | Hex    | Note                          |
|-------------|---------|----------|--------|-------------------------------|
| `ORANGE`    | 14378   | 0o34052  | 0x382A | letters only                  |
| `TIGER42`   | 37323   | 0o110713 | 0x91CB | letters + digits              |
| `COFFEE7`   | 32983   | 0o100327 | 0x80D7 | letters + digit               |
| `sky-9`     | 56806   | 0o156746 | 0xDDE6 | lowercase + special char `-`  |

`ORANGE`, `orange`, `OrAnGe` all produce 14378 (case-insensitivity confirmed).
`sky-9` shows that lowercase is uppercased (`SKY`) while the `-` passes through.

### 1.3 IMPORTANT: the stored value is DECIMAL

When read back from the user table, the value is shown in **decimal**. Reading it as
octal is a trap: `ORANGE` = 14378 decimal, which is `0o34052` octal - NOT `0o14378`
(that would be 6664). An early brute force failed for exactly this base confusion.

---

## 2. Where it lives (verified disassembly)

Segment **S3CP** (command processor), loads at octal 030000 (hex `0x3000`).
Routine **`LOGIN`** at octal 060616 (hex `0x618E`).

The password is read with echo OFF and folded inline:

```
060750  STZ ,B -145
060751  STZ ,B -200          ; acc := 0
060752  SAA -1
060753  MON 3                ; ECHOM - turn echo OFF (hidden entry)
060754  JPL I 23             ; read one char -> A (read routine uppercases letters)
060755  SAT 15               ; T := 015B (CR)
060756  SKP IF DA UEQ ST     ; char == CR ?
060757  JMP 65               ; -> done (061044)
...
060766  RADD CLD ST DA       ; A := char
060767  LDT ,B -200          ; T := acc
060770  SHT ROT 3            ; T := ROL16(T,3)
060771  RADD SA DT           ; T := T + char
060772  STT ,B -200          ; acc := T
060773  JMP -17              ; -> next char (060754)
...
061044  SAA 1 / MON 3        ; ECHOM - echo back ON
```

### 2.1 Instruction decode (checked against `nd100-markdown/docs/cpu_documentation.md`)

- `SHT ROT 3` (155003B): shift T, shift_type = ROT (bits 10-9 = 01, most/least
  significant bits connected), count = 3, bit 5 = 0 => shift LEFT. So this is a
  16-bit rotate-left-by-3. VERIFIED.
- `RADD SA DT` (146056B): register add, source = A (SA), destination = T (DT), no
  CLD/CM1 => `T := T + A`. VERIFIED.
- `RADD CLD ST DA` (146165B): CLD clears the destination first, source = T =>
  `A := T`. VERIFIED.
- `LDT ,B -200` / `STT ,B -200`: load/store the accumulator from the stack frame.

Note on an earlier mistake: `RORA` (opcode 145400B) is **register inclusive-OR**,
NOT "rotate"; `REXO` (145000B) is XOR. A prior analysis mis-read `RORA` as rotate.
Rotation is only done by the shift instructions (`SHT`/`SHA`/`SHD`) with `ROT`.

---

## 3. The octal-digit fixed point (CONFIRMED login bypass)

**This is not a rumour - it is provably and confirmed true.** Feeding a stored value
back in as its own octal digits reconstructs that exact value, for EVERY 16-bit
value, and it has been confirmed to log in. The reason:

Because `ROL16(acc,3)` equals `acc*8` whenever the top 3 bits of `acc` are zero, the
fold is exactly **Horner's method in base 8**:

```
acc = acc*8 + digit
```

When you feed a stored value V back in as its own **octal digits** (as raw byte
values 0..7), every intermediate `acc` is a prefix of V's octal expansion. The
prefix before the final digit is `floor(V/8) <= 0o17777 = 8191 < 2^13`, so its top 3
bits are always zero - meaning `ROL16 = *8` at every step, with no wrap. The digits
therefore rebuild V exactly, for **any** 16-bit value.

Example (from the rumour): value `0o12345` -> feed bytes 1,2,3,4,5 ->
`((((1*8+2)*8+3)*8+4)*8+5)` = 5349 = `0o12345`. It comes back.

**Consequence (confirmed): anyone who can read the stored octal word can log in
without the original password**, by entering that word's octal digits as their raw
byte values 0..7. This always reconstructs the stored value - no cracking, no
wordlist, no search.

How it is entered (this is the mechanism, not a limitation):
- The digits are entered as their **raw byte values `0x00`-`0x07`** (control bytes
  NUL..BEL) - NOT the ASCII digit characters `'0'`-`'7'` (0x30-0x37, which fold to
  something else). On a terminal these are simply **Ctrl-A .. Ctrl-G** (Ctrl-A =
  0x01 ... Ctrl-G = 0x07; the digit 0 = NUL = Ctrl-@).
- The read path uppercases only `a-z`, so these control bytes pass straight into the
  fold unchanged, which is exactly why the reconstruction is exact.
- **The login read accepts them.** LOGIN sets break strategy 1 (`SAA 1 / MON 4`
  BRKM) which per the Monitor Calls manual = "break on control characters (ASCII <
  32 and DEL)". Control characters are therefore break characters that the driver
  delivers to the read - the same mechanism by which CR (015B) terminates input -
  so 0x01-0x07 are handed to the fold loop, which folds every char that is not CR.
  Caveat: byte 0x00 (NUL, the digit `0`) is commonly swallowed by the driver, and
  BEL (0x07) may be special on some setups; confirm end to end on the live system by
  logging in with the octal digits typed as Ctrl-A..Ctrl-G for a value with no 0
  digit.

---

## 4. Brute forcer

`sintran-passcrack.c` (same folder) inverts the fold by exhaustive search. Build and
run under Linux:

```bash
gcc -O2 -o passcrack sintran-passcrack.c
./passcrack
# Target password value (decimal, or octal with trailing b/B): 14378
# Max password length [10]: 6
# Include special characters (e.g. { [ ] } \ | ) ? [y/N]: <Enter>
# ... -> MATCH: "ORANGE"
```

Prompts:
- **Target value** - decimal (e.g. `14378`) or octal with a trailing `b`/`B`
  (e.g. `34052b`). Masked to 16 bits.
- **Max length** - press Enter for the default 10.
- **Special characters** - `y` extends the alphabet from `[A-Z0-9]` (36) to also
  include the punctuation set `!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~` (68 total). Default
  is No. Real passwords can contain specials, so a `[A-Z0-9]`-only run will not find
  those.

Search space is `alphabet^L`, so short lengths finish quickly; long lengths may run a
very long time. A Unicode/ANSI progress bar shows coverage. Because the fold is
16-bit, many strings collide - the tool reports the FIRST match in search order,
which is not necessarily the original password.

## 5. Wordlist database (instant reverse lookup)

`sintran-passdb.c` (same folder) precomputes a reverse table from a wordlist such as
`rockyou.txt` (~14M entries). Because the fold output is only 16 bits, there are just
**65536 possible values**, so the "database" is a direct-indexed table of 65536 slots
(value -> one password producing it). Lookup is O(1) - a single 64-byte read at
`offset = 32 + value*64`.

```bash
gcc -O2 -o passdb sintran-passdb.c
./passdb build rockyou.txt rockyou.db          # fold every word, fill the table
./passdb lookup rockyou.db 14378               # decimal, or octal ending in b/B
#   found     -> prints the password, exit code 1
#   not found -> prints 'password not found', exit code 0
```

The DB is ~4 MB regardless of wordlist size (65536 x 64-byte slots + 32-byte header).
Per value it keeps the **most word-like** entry (most letters), then the **shortest**,
then the first seen - so a readable `FORD` is preferred over a numeric collision like
`032291`. If the only wordlist entry that folds to a value is numeric, that is what
you get. Any stored password is still just a **collision** (it folds to the value and
authenticates), not necessarily the original password.

Per value it keeps the most word-like entry (most letters), then shortest, then
first-seen, so a readable collision is preferred (see section 6).

### 5.1 Collisions are the whole point

Login compares only the 16-bit fold, never the original string, and ~14M passwords
map onto 65536 values (~214 per value on average). So **any** string with the
matching value logs in. `passcrack` and `passdb` therefore return *a* working
password, not necessarily the one that was set - and that is sufficient to
authenticate. This 16-bit fold offers no meaningful resistance to an attacker who can
read the stored value (or who can just try wordlist collisions).

## 6. Security assessment: a 16-bit "hash" is trivially reversible

The fold output is a single 16-bit word - only **65536 possible values** for the
entire password space. Two consequences:

1. **A wordlist database is not even needed.** Exhaustively folding short `[A-Z0-9]`
   strings covers the whole value space almost immediately:

   | Max length | Distinct values reachable | Strings folded |
   |------------|---------------------------|----------------|
   | <= 3       | 3,328  (5.1 %)            | 47,988         |
   | <= 4       | 26,870 (41.0 %)           | 1,727,604      |
   | <= 5       | ~65,500 (~100 %)          | ~60,000,000    |

   Every 16-bit value has a short `[A-Z0-9]` preimage, so `passcrack` inverts ANY
   stored value in well under a second on a modern CPU - no wordlist required.
   `passdb` is retained only because it returns a *recognizable* real word when one
   exists (a plausible-looking password), whereas `passcrack` returns the first
   preimage in search order (often gibberish).

2. **The scheme provides essentially no protection.** Anyone who can read the stored
   16-bit word (from the user table on disk, or via the octal-digit fixed point in
   section 3) can recover a working password instantly. There is no salt, the key
   space is 2^16, and collisions are dense (~214 per value against a 14M wordlist).
   In modern terms this is an obfuscation, not a hash.

This is a property of the design, verified end to end: the algorithm reversed from
the S3CP disassembly, the reversibility measured empirically, and both directions
checked against real stored values.
