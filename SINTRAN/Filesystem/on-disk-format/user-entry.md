# User (account) entry - 64 bytes (Phase 3)

Every account on a directory device has one **user entry**: a 64-byte record in
the *user file* holding the user name, password, page quota (reserved / used),
and a friends table. The user file is an **indexed** file: the `user_file_ptr` in
the [master block](directory-label.md) points at an index block whose first
contiguous pointer names the user **data page** (32 user entries per page).

Sources: real disk `~/repos/nd100x/SMD0.IMG` (PACK-ONE); NDFS
`ndfs-c/src/user_entry.c` + `include/ndfs/user_entry.h` and
`user_friend.c`/`.h` (the reader that round-trips these bytes); `ndtool -u` /
`--friends` (NOT an independent cross-reader — it links the same `ndfs-c` library, so it
cannot disagree with it); carved `006-S3FS` `RUSER`/`WUSER`/`GUSEN`
(producing/consuming code). On-disk multi-byte values are **big-endian words**.

---

## 1. Locating a real user entry

`user_file_ptr = 0x400048FE` -> INDEXED, block **44376B (18686)**. The index
block at page 18686 begins:

```
0247f000: 0000 48ff ...   (block 44376B = 18686, byte 0)
```

First index entry `0000 48FF` = CONTIGUOUS block **44377B (18687)** - the user
**data page**. It holds exactly **10 valid users** (flag byte `0x81`), matching
`ndtool -u` -> `Users: 10`. First three entries (192 bytes):

```
0247f800: 8100 5359 5354 454d 2700 0000 0000 0000   <- user 0: SYSTEM
0247f810: 0000 0000 b8af 2bda 9b20 9884 0000 3a98
0247f820: 0000 2cd3 0000 0000 04f7 0000 0000 0000
0247f830: 8705 8108 0000 0000 0000 0000 0000 0000
0247f840: 8100 464c 4f50 5059 2d55 5345 5227 0000   <- user 1: FLOPPY-USER
0247f850: 0000 0000 b8af 2c9a be64 bd9c 0000 0000
0247f860: 0000 0000 0001 0000 04f7 0001 0001 0000
0247f870: 0000 0000 0000 0000 0000 0000 0000 0000
0247f880: 8100 5554 494c 4954 5927 0000 0000 0000   <- user 2: UTILITY
0247f890: 0000 0000 b8af 2c9a ba65 1b77 0000 06ae
0247f8a0: 0000 06ae 0002 0000 04f7 0002 0002 0000
0247f8b0: 0000 0000 0000 0000 0000 0000 0000 0000
```

---

## 2. Field layout (offsets within the 64-byte entry)

| Off | Field | Size | Verdict | Notes |
|-----|-------|------|---------|-------|
| 0 | Flag byte | 1 | **VERIFIED** | `0x81` = valid user (`NDFS_USER_ENTRY_FLAG`) |
| 1 | Enter count | 1 | INFERRED | number of times entered |
| 2-17 | User name | 16 | **VERIFIED** | ASCII, `0x27`-terminated |
| 18-19 | Password | 2 | **VERIFIED** | 16-bit (hashed/packed; 0 = none) |
| 20-23 | Date created | 4 | **VERIFIED** | ND timestamp |
| 24-27 | Last date entered | 4 | **VERIFIED** | ND timestamp |
| 28-31 | Pages reserved (quota) | 4 | **VERIFIED** | 32-bit |
| 32-35 | Pages used | 4 | **VERIFIED** | 32-bit |
| 36 | Directory index | 1 | **VERIFIED** | |
| 37 | User index | 1 | **VERIFIED** | this account's index |
| 38-39 | reserved | 2 | OPEN | unmodelled by NDFS |
| 40-41 | Default file access | 2 | **VERIFIED** | 3 x 5-bit tiers (template for new files) |
| 42-46 | reserved / tracking | 5 | OPEN | 42-43 and 44-45 both hold the user index |
| 47 | MXOBL/ACOBL | 1 | **VERIFIED** | object-block counts, two ZERO-BASED nibbles - see 4.5 |
| 48-63 | Friends table | 16 | **VERIFIED** | 8 x 2-byte packed entries |

Reader offsets: `ndfs_ue_from_bytes()` (`user_entry.c` lines 43-58) reads enter
count at +1, name at +2, password at +18, dates at +20/+24, quotas at +28/+32,
directory index at +36, user index at +37, default access at +40, and 8 friends
starting at +48. **VERIFIED** offsets; NDFS explicitly leaves 38-39 and 42-47 as
verbatim pass-through (`user_entry.h` lines 49-53).

---

## 3. Worked decode - user 0 (`SYSTEM`)

| Off | Bytes | Field | Value |
|-----|-------|-------|-------|
| 0 | `81` | Flag | valid user |
| 1 | `00` | Enter count | 0 |
| 2-17 | `53 59 53 54 45 4D 27 00...` | Name | `SYSTEM` |
| 18-19 | `00 00` | Password | 0 (none) |
| 20-23 | `B8 AF 2B DA` | Date created | ND timestamp |
| 24-27 | `9B 20 98 84` | Last entered | ND timestamp |
| 28-31 | `00 00 3A 98` | Pages reserved | 15000 |
| 32-35 | `00 00 2C D3` | Pages used | 11475 |
| 36 | `00` | Directory index | 0 |
| 37 | `00` | User index | 0 |
| 40-41 | `04 F7` | Default file access | 0x04F7 (see 4.3) |
| 48-49 | `87 05` | Friend 0 | active, user 5, RWA |
| 50-51 | `81 08` | Friend 1 | active, user 8, R |

`ndtool -u` reports `[000] SYSTEM ... Reserved: 15000 Used: 11475 Free: 3525`
(15000 - 11475 = 3525). `ndtool --friends SYSTEM` reports
`[5] RT RWA--` and `[8] COSMOS-BASIC R----`. Both match the byte decode exactly.
**VERIFIED.**

Cross-check on user 2 (`UTILITY`): pages reserved `00 00 06 AE` = 1710, used
`00 00 06 AE` = 1710 -> `ndtool` shows `Reserved: 1710 Used: 1710 Free: 0`.
**VERIFIED.**

---

## 4. Field semantics

### 4.1 Flag byte (byte 0)

`0x81` marks a valid, in-use user slot. NDFS accepts an entry only when
`(byte0 & 0x81) == 0x81` (`user_entry.c` lines 32-34). All 10 real entries have
byte 0 = `0x81`; empty slots are `0x00`. **VERIFIED.** (Bit 7 `0x80` parallels the
object entry's in-use bit; bit 0 `0x01` is the extra "user" marker.)

### 4.2 Password (bytes 18-19)

A single 16-bit word. On PACK-ONE SYSTEM it is `0x0000` (no password). The word is
an obfuscated/packed password, not cleartext; `ndtool --passwd` clears it by
zeroing this field. Exact hashing: **INFERRED** (16-bit field VERIFIED; algorithm
not decoded here). Resolve against `006-S3FS` `DPASS` (43376B) / the enter-user
path.

### 4.3 Quota and default access

- **Pages reserved** (28-31) = the account's quota; **Pages used** (32-35) = pages
  currently consumed. `Free = reserved - used` (`user_entry.h` lines 80-84).
  **VERIFIED** against `ndtool -u` for all 10 users.
- **Default file access** (40-41) is a 3 x 5-bit tier word (same encoding as an
  [object entry's access bits](object-entry.md#42-access-bits-bytes-26-27)) used
  as the template when this user creates a file. On PACK-ONE = `0x04F7` ->
  OWN = `0x17` (R W A - D), FRIEND = `0x07` (R W A), PUBLIC = `0x01` (R). The
  manual's documented system default is "public READ, friend READ WRITE APPEND,
  all access for owner" (`ND-60.128.5` `SET-DEFAULT-FILE-ACCESS`), which matches
  the PUBLIC=R / FRIEND=RWA tiers; the OWN tier on this disk is RWAD (missing the
  Common/execute bit) rather than the doc example's RWACD - a per-disk stored
  value, not the doc's illustration. **VERIFIED** value / **INFERRED** per-bit
  letter mapping. (NDFS `ndfs_ue_init` seeds a fresh default of `0x04FF`,
  `user_entry.c` line 18.)

### 4.4 Friends table (bytes 48-63) - 8 x 2-byte entries

Each friend is a 16-bit packed word (NDFS `user_friend.h`):

```
 bit 15 | 14 13 | 12  11  10  9   8  | 7 .......... 0
  active  (rsvd)  D   C   A   W   R    friend user index (0-255)
```

| Bit | Mask | Meaning |
|-----|------|---------|
| 15 | `0x8000` | Entry active |
| 12 | `0x1000` | Directory access (D) |
| 11 | `0x0800` | Common access (C) |
| 10 | `0x0400` | Append access (A) |
| 9 | `0x0200` | Write access (W) |
| 8 | `0x0100` | Read access (R) |
| 7-0 | `0x00FF` | Friend's user index |

Worked (SYSTEM): friend 0 = `0x8705` -> active, index **5**, perms
`0x0700` = R+W+A -> `RWA--`. Friend 1 = `0x8108` -> active, index **8**, perms
`0x0100` = R -> `R----`.

**VERIFIED against ND documentation (2026-08-02).** `ND-860228-2 EN SINTRAN III Monitor
Calls`, appendix C, states the word bit by bit:

> Bit 15: set if friend exists. Bit 12: set if friend has directory access. Bit 11: set if
> friend has common access. Bit 10: set if friend has append access. Bit 9: set if friend has
> write access. Bit 8: set if friend has read access. Bit 7-0: user index of friend.

corroborated by `ND-30.003.007` ("THE FRIEND TABLE", bits `15 | 12 | 11 | 10 | 9 | 8 | 7 0`).
The layout above matches exactly.

> **The previous evidence did not support this.** It read: *"`ndtool --friends SYSTEM` prints
> exactly `[5] RT RWA--` and `[8] COSMOS-BASIC R----`. **VERIFIED**"*. `ndtool` links the same
> `ndfs-c` library that defines this bit table (`target_link_libraries(ndtool ndfs)`), so it
> cannot disagree with it — it re-prints our own assumption. Under the reversed hypothesis
> (R at bit 12 descending to D at bit 8), `0x0700` decodes to `D+C+A` and `ndtool` would have
> printed *those* letters with equal confidence. The check could not fail. The user-index half
> was weakly self-supporting (index 5 resolves to a real user, "RT"), but nothing in the cited
> evidence tested the permission nibble's direction.

Note the friend-permission bit **positions** (bits 8-12) differ from the object
access-tier layout (bits 0-4 per tier) - friends carry their permission bits in
the high byte alongside the index in the low byte.

---

## 5. Producing / consuming code (`006-S3FS`)

| Addr (octal) | Symbol | Role |
|--------------|--------|------|
| 53174B / 53243B | `TUSEN` / `FUSEB` | Test / find user block |
| 53246B / 53410B | `RUSER` / `WUSER` | Read / write user entry |
| 55111B | `GUSEN` | Get user entry |
| 55206B | `CUSED` | Change used (pages) |
| 62206B / 62314B | `CHNUS` / `INSUS` | Change / insert user |

Directory-datafield accessors that name these fields (43263B-43775B):
`DUSEN` (43334B), `DPASS` (43376B), `DACCE` (43437B), `DFNAM` (43465B),
`DPAGE` (43503B) - confirming the field vocabulary (user, password, access, file
name, pages). The carved `214B-GetUserName` (`GUSNA`, 105301B) reads the 16-char
name field via `LBYT`. See `.../re/mon-analysis/214B-GetUserName/`.

---

## 6. Fields marked OPEN

- Bytes 38-39 and 42-47 - reserved/tracking; byte 47 hinted `mxobl`/`acobl`
  (max/active object-block). NDFS preserves them verbatim (`user_entry.c` lines
  38-41). Resolve against `006-S3FS` `RUSER`/`WUSER`.
- Password (18-19) hashing algorithm - field VERIFIED, algorithm INFERRED.

**Provenance:** real bytes `SMD0.IMG`; reader `user_entry.c`/`.h` +
`user_friend.c`/`.h`; `ndtool -u` / `--friends` (same library, not an independent check);
the FRIEND word bit layout is carried by `ND-860228-2` appendix C; producer `006-S3FS`
`RUSER`/`WUSER`/`GUSEN`.
</content>
