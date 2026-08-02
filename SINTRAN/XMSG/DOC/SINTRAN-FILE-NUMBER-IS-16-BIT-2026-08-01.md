# The SINTRAN file number is 16-bit, and the object entry splits it into block + slot (2026-08-01)

> ## CORRECTED 2026-08-02 — the file-number formula here is wrong for a relocated block
>
> This document derives the logical block from the **physical** index-block group
> (`block = page / 512`). Measured live on a 201-user pack, SINTRAN **relocates** a user's
> overflow object block when another user needs that group — user 8's overflow was watched
> moving group 2 -> 3 -> 4 as users 136 and 200 created files, with all 300 files surviving
> and staying numbered FILE 0..299.
>
> So the logical block is the **ordinal rank** of the group among those the user occupies,
> not the group number. On that pack the formula below gives one user 0..255 then
> **1024..1067**, where SINTRAN says **256..299**.
>
> The `F0500 = FILE 307` vector below is still correct — but it holds under **both** readings,
> because nothing had displaced that pack's overflow block, so rank happened to equal group
> number. It never could have discriminated them.
>
> Correct rule and evidence: `norskdata-ndfs/docs/NDFS-OBJECT-BLOCKS-SPEC.md` sections 6.1-6.2.



Settled by a controlled experiment on live SINTRAN III K machines. It closes experiment 7.1 in
[PLAN-CSHARP-FILE-SERVER-AND-FOLDER-SYNC-2026-08-01.md](PLAN-CSHARP-FILE-SERVER-AND-FOLDER-SYNC-2026-08-01.md)
and it invalidates any 8-bit assumption about file numbers anywhere in the codebase.

---

## 1. Why this needed testing

A user area holds 256 files by default and up to **4096** on SINTRAN K, structured as **16 object
blocks of 256 objects each** (`ND-30.003.007 EN SINTRAN III System Supervisor` section 4.3.2).
4096 needs 12 bits, so any field recorded as one byte could not be the whole story.

The specific suspect was the `*FA-SERVER` trailer, where earlier work recorded *"only the sixth byte
moves"*. Every capture until now came from user areas with fewer than 256 files, so **no observed
value had ever exceeded 255** - the same blind spot that made the MAC system number look 8-bit when
it is 16.

---

## 2. The experiment [VERIFIED]

On node 100:

```
CREATE-USER BIGMAN
GIVE-USER-SPACE BIGMAN 4000
USER-STATISTICS BIGMAN,TERMINAL     ->  MAXIMUM NUMBER OF FILES : 256
GIVE-OBJECT-BLOCKS BIGMAN,3
USER-STATISTICS BIGMAN,TERMINAL     ->  MAXIMUM NUMBER OF FILES : 1024
```

**The manual's block model is confirmed on a running machine**: 4 object blocks give exactly
4 x 256 = 1024.

Then ~482 one-page files were created under BIGMAN, giving two files whose numbers straddle 255:

| file | number |
|---|---|
| `F0010` | 8 |
| `F0500` | **307** (`0x0133`) |

From node 102, the **same operation** was captured against each:

```
LIST-FILES d100(bigman()).F0010:TXT,,     ->  FILE 8   : D100.(PACK-ONE:BIGMAN)F0010:TXT;1
LIST-FILES d100(bigman()).F0500:TXT,,     ->  FILE 307 : D100.(PACK-ONE:BIGMAN)F0500:TXT;1
```

Both names are the same length, so the two frames differ only in the name, the envelope, and
whatever encodes the number. Capture:
`E:\Dev\Ronny\X25Emulator\pcap\filenum-8-vs-307-102-to-100-2026-08-01.pcapng`

---

## 3. Result A: the request carries NO file number [VERIFIED]

Diffing the two 156-byte request frames, the only differences are:

| offset | change | what it is |
|---|---|---|
| 21 | `79` -> `09` | ND link header sequence - envelope |
| 37 | `93` -> `9b` | SINTRAN Flags 1 - envelope |
| 41 | `1d` -> `15` | word 6 checksum - derived from the above |
| 60, 69 | both **+4** | a counter that advanced between the two operations |
| 94-95, 109-110 | ASCII | `F0010` vs `F0500`, the name, twice |

**A lookup is by NAME.** The client does not send a number, so a server does not need to resolve one
on the request path.

---

## 4. Result B: the reply carries the number as 16 bits [VERIFIED]

In the reply, a QFORM-tagged run of integers:

```
FILE 8    ...  a2 00 08   a2 00 08   a2 00 01  ...
FILE 307  ...  a2 00 08   a2 01 33   a2 00 01  ...
                          ^^^^^^^^
```

The tag `0xA2` is `(class << 4) | length` = class 10, **length 2**. The second one is the file
number: `0x0008` -> `0x0133` = 8 -> **307**, big-endian.

> **The file number is a 16-bit big-endian value on the wire. An 8-bit field cannot carry it, and
> any code that treats it as a byte silently misaddresses every file numbered above 255.**

---

## 5. Result C: the object entry splits it into block + slot [INFERRED, 2 data points]

The same reply carries the 64-byte object entry, and inside it the number appears **split**:

| offset in frame | FILE 8 | FILE 307 | reading |
|---|---|---|---|
| 114 | `00` | `10` | high nibble = object block number (0 -> 1) |
| 109, 111, 121 | `08` | `33` | slot within the block (8 -> 51) |

307 = 1 x 256 + 51, and `0x33` = 51. So the entry encodes the number the way the
16-blocks-of-256 structure implies, while the protocol parameter in section 4 carries it whole.

This also explains three bytes that looked like an 8-bit truncation: they are not truncated, they
are the **slot**, which is 0-255 by construction.

**Marked INFERRED deliberately.** Two data points. `0x00` -> `0x10` is consistent with "block
number in the high nibble", but two samples cannot exclude "bit 4 happens to mean something else".

**The test that settles it:** a file numbered above **511** (block 2), where the byte should read
`0x20`. The highest number reached in this experiment was 307, so it needs more files created -
around 550 successful creations in one user area.

---

## 6. Consequences

- **Any 8-bit file number in our code or in NDFS tooling is a bug** for user areas above 256 files.
  An audit of `E:\Dev\Ronny\norskdata-ndfs` and `RetroFS.NDFS` is running separately.
- The C# file server does **not** need number resolution on the request path (section 3), but it
  **must** emit a correct 16-bit number in listings.
- A sidecar-based design (serving a plain Windows folder) must model the block structure, not a flat
  counter, or it will hand out numbers the real system could never produce.
- The general lesson, now hit twice in one day: **a field's width is unverified until a value has
  exceeded the smaller width.** Both the MAC system number and this were "8-bit" only because
  nothing had exceeded 255. Every byte-width claim derived from two nodes and small directories
  should be re-read with that in mind.

---

## 7. Related

- [PLAN-CSHARP-FILE-SERVER-AND-FOLDER-SYNC-2026-08-01.md](PLAN-CSHARP-FILE-SERVER-AND-FOLDER-SYNC-2026-08-01.md) sections 2, 2.1, 7.1
- [COSMOS-ETHERNET-TRANSPORT-FRAMING-2026-08-01.md](COSMOS-ETHERNET-TRANSPORT-FRAMING-2026-08-01.md) section 3 - the MAC field, the same trap
- [XMSG-RETROFS-MIGRATION-PLAN-2026-07-29.md](XMSG-RETROFS-MIGRATION-PLAN-2026-07-29.md) - the object entry is shipped verbatim
