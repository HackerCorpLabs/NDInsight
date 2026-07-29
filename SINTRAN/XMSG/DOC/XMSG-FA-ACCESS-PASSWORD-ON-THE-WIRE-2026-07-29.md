# Remote file access working, and the password word on the wire (2026-07-29)

Two firsts in one capture:

1. **A COSMOS file-access request that SUCCEEDS.** Every earlier capture died at the opening
   letter with `XRNRO` because no peer existed. Here node 102 lists a real user's files on node
   100 over HDLC and gets the listing back.
2. **The SINTRAN password fold, confirmed on the wire.** The algorithm was carved from the
   L-VSX-500 disassembly and verified only against *stored* user-table words. This is the first
   time the same 16-bit word has been seen travelling between two machines - and it proves the
   password is never sent as text.

---

## 1. The setup [VERIFIED]

Three RetroCore machines, star topology, node 100 in the middle:

| Node | SINTRAN | Console | Link |
|---|---|---|---|
| 100 | K | 9010 | listens 10362 + 10364; has user `SECRET`, password `secret` |
| 102 | K | 9102 | HDLC to 100 on 10362 - the CLIENT |
| 103 | L | 9003 | HDLC to 100 on 10364 - no COSMOS file access (revision-F gate), TRANSFER-FILE only |

The working direction is **102 -> 100**. On 102:

```
LIST-FILES d100(secret(secret)).,,
FILE 0 : D100.(PACK-ONE:SECRET)TXT1:SYMB;1
```

Syntax note: the remote password goes in **nested parentheses after the user name**,
`d100(user(password))`. Omitting it, or giving the wrong one, answers `WRONG PASSWORD`.

## 2. The controlled experiment [VERIFIED]

Three requests, changing only the password:

| Password given | Result |
|---|---|
| `secret` (correct) | the file listing |
| `orange` (wrong) | `WRONG PASSWORD` |
| none | `WRONG PASSWORD` |

`orange` was chosen deliberately: `ORANGE` is one of the published test vectors in
`PASSWORD-ALGORITHM.md`, so its expected word was already known independently.

Expected words from the carved fold `acc = ROL16(acc,3) + toupper(c)`:

| Password | Decimal | Octal | Hex | Wire bytes |
|---|---|---|---|---|
| `secret` | 27946 | 66452B | **0x6D2A** | `6D 2A` |
| `ORANGE` | 14378 | 34052B | **0x382A** | `38 2A` |

## 3. The result: one byte [VERIFIED]

The correct-password request and the wrong-password request are **byte-identical apart from the
password word**. Full byte diff of the two raw HDLC frames (146 bytes each):

| Raw offset | correct | wrong | What it is |
|---|---|---|---|
| 12 | `29` | `39` | Flags1 - envelope, changes per frame |
| 16 | `7b` | `6b` | Counter - envelope, derived |
| 28 | `15` | `72` | XMSPT low byte - a new source port per attempt |
| 34 | `3f` | `40` | XMLEN low byte - see caveat below |
| **106** | **`6d`** | **`38`** | **the password word, high byte** |
| 143-144 | - | - | FCS |

Offset 107 is `2a` in BOTH, because `secret` = 0x6D**2A** and `ORANGE` = 0x38**2A** happen to
share a low byte. Coincidence, but it makes the diff a single byte.

**So the password word sits at raw offset 106-107** (SINTRAN-header-relative 103, trailer offset
71), immediately after a `B0 10` tag. Nothing else in the request moves.

That is simultaneously:

- **the location of the password field**, pinned by single-variable variation, and
- **live confirmation of the fold algorithm** - `ORANGE` produced exactly the 0x382A that the
  disassembly-derived implementation predicts, on the wire, on a different machine, three years
  of abstraction away from the carve.

**The plaintext never appears.** Neither `secret` nor `orange` is anywhere in the frame. The
client folds locally and sends the word.

## 4. The rest of the request [PARTIALLY DECODED]

The frame carries far more than the opening `XSLET` letter did:

```
XMDSY/XMDPT = 100 : 1453      (port 11, random 45)
XMSSY/XMSPT = 102 : 1557      (port 12, random 21)
XMCSM       = 0x007007F0      Flags2 = 0x0070, a class we had not seen
trailer     = tagged fields, NOT XROUT parameter tagging
```

Readable strings in the trailer: `BAK03  SYSTEM` (the local background process and user) and
`SECRET` (the remote user). The tag vocabulary (`92`, `f2`, `a2`, `8c`, `bd`, `b0`, `e1`) is a
different scheme from XROUT's parameter tags - most likely the QFORM tagging described in the
Ghidra carve of `cos-fa-serv-e04`. Decoding it properly is the next job; the carve
(`COSMOS-RE/Analysis/COS-FA-SERV-E04-Analysis.md`, 13 operations and typed-param protocol) is
the reference to check it against.

## 5. Caveat

`XMLEN` reads `0x3F` on one request and `0x40` on the other while the visible trailer is the same
length. Not explained. Flagged rather than hand-waved.

---

## Provenance

Capture: `E:\Dev\Ronny\X25Emulator\pcap\fa-access-secret-102-to-100-2026-07-29.pcapng`
(128 payload frames, both links). Correct-password request = frame 9; wrong-password requests =
frames 138 and 164.

Fold algorithm:
`E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\PASSWORD-ALGORITHM.md`
(the implementation used here was self-checked against all three of its published vectors -
`ORANGE`, `TIGER42`, `sky-9` - before being applied).
