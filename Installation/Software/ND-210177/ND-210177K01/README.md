# ND-210177K01 — COBOL-85 for ND-500/5000, version K01

> Status: IN-PROGRESS — no `:MODE` script on this disk, procedure adapted from J02, NOT verified   ·   Install source: [OBS] + [INF]

| Field | Value |
|-------|-------|
| Part number | `210177K01` |
| Base product | [`ND-210177`](../README.md) |
| Version | K01 |
| Release date | files dated 1987-07 through 1989-02-17 |
| CPU target | ND-500 / ND-5000 |
| OS requirement | unknown |

## Description
Same bundle shape as [ND-210177J02](../ND-210177J02/README.md) — compiler domain, runtime
library, ISAM add-on — but **this disk carries no `:MODE` install script**, and adds a pair of
`:NEW`/`:OLD` files per library that J02 doesn't have.

## Distribution media

| Floppy volume | Contents |
|----------------|----------|
| `210177K01-XX-01D` | `DESCRIPTION-FILE:DESC` (8 pages) · `COBOL-85-K01:LINK`/`:DSEG`/`:PSEG` (9/64/130 pages) — the compiler domain · `COBOL-85-LIB-K01:NRF` (142 pages) + `COBOL-85-LIB-K01:NEW`/`:OLD` (1 page each) · `IS-MULTI-K00:NRF` (1 page) + `IS-MULTI-K00:NEW`/`:OLD` (1 page each) · `ISAM-INTER-K01:PROG` (58 pages), `ISAM-SERVICE-K02:PROG` (83 pages), user `FLOPPY-USER` |

Confirmed by downloading the image (MD5 `6a774ca2eac376a4a060d5a375d6ad2c`) and reading with
`ndtool -t`.

> **The `:NEW`/`:OLD` pair is not understood.** No `:MODE` script references them, and nothing
> read so far explains their purpose. Given the name, a plausible guess is a before/after pair
> for a binary patch or version-migration step — but this is a guess, not a finding, and is not
> used in the procedure below.

## Installation procedure — ADAPTED FROM J02, NOT CONFIRMED FOR K01

No install script exists on this specific disk. The safest starting point is J02's real, decoded
procedure (see [../ND-210177J02/README.md](../ND-210177J02/README.md)), substituting K01's file
names — but note K01 has **no `IS-MULTI-K00:MODE` on this disk either** (only `IS-MULTI-K00:NRF`
+ its `:NEW`/`:OLD` pair), so the shared-segment setup step may need to be carried over from a
J02 install (if the `IS-MULTI-K00` domain is already resident) or reconstructed by hand:

```
@ENTER-DIRECTORY 210177K01-XX-01D,FLOPPY-DISC-1,0,
@ND-500-MONITOR
N500: LINKAGE-LOADER
NLL: SET-DOMAIN "COBOL-85-K01"
NLL: SET-SEGMENT-NUMBER 21D
NLL: OPEN-SEGMENT "COBOL-85-K01",WCP
NLL: TOTAL-SEGMENT-LOAD COBOL-85-LIB-K01
NLL: TOTAL-SEGMENT-LOAD EXCEPT-LIB
NLL: CLOSE-SEGMENT 21D
NLL: END-DOMAIN
```
(hand-derived from J02's `COBOL-LIB-J02:MODE` shape, with the `IS-MULTI` common-segment
attachment omitted since this disk has no script defining it — **not verified**, and likely
incomplete if the multiuser ISAM facility is actually needed.)

**Do not treat the above as verified.**

## Configuration / post-install
Unknown.

## Documentation
- PD-sheet: not located
- PI-sheet: not located for this article number
- Manual(s): `ND-60.144.3` COBOL Reference Manual

## Provenance & open items
- Source: floppy directory listing via `ndtool -t` on the downloaded image.
- **TODO (blocking):** find or reconstruct the real install script for this version — the
  procedure above is adapted from a different version's script, not this one's.
- **TODO:** determine the purpose of the `:NEW`/`:OLD` file pairs.
- **TODO:** this install has not been run live in the emulator.

---
**Parent:** [../README.md](../README.md) (`ND-210177` product overview)
