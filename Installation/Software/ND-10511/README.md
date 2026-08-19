# ND-10511 — Exception Handling System

> Status: IN-PROGRESS — real floppy decoded, file inventory confirms the EXCEPT-LIB identity; no install script found

| Field | Value |
|-------|-------|
| ND article number | `ND-10511` |
| Product name | Exception Handling System |
| Functional category | Language Tools (runtime support library) |
| Related products | **Confirmed** as the source of `EXCEPT-LIB`, a library already referenced (but unsourced until now) in [ND-211224B01](../ND-211224/ND-211224B01/README.md)'s ND LINKER install notes. |

## What is known — real floppy, decoded

Floppy `ND-10511A00` (downloaded via NDwiki, imaged by Torfinn "Tingo" Ingolfsen) mounts cleanly
and contains exactly two files:

```
(FLOPPY-USER)EXCEPTION-LIB-A:NRF   21432 bytes  11 pages   1983-05-03
(FLOPPY-USER)EXCEPTION-TPS-A:NRF   21513 bytes  11 pages   1983-05-03
```

Both are `:NRF` (relocatable object) modules — a runtime library, not an application. No
`DESCRIPTION-FILE`, no `:MODE`/`:BATC`/`:XCOM` install script, no installer `:PROG` — this is a
bare pair of link-time library modules, meant to be `COPY-DOMAIN`'d or linked into another
product's build the same way the ND LINKER install notes assumed. `EXCEPTION-LIB-A` matches the
suspected `EXCEPT-LIB` name closely enough (same "exception" root, same era) to treat the
identification as confirmed rather than merely likely.

**No PD sheet, no PI sheet located** for this article number — this entry rests entirely on the
decoded floppy.

## Provenance
Real floppy image, downloaded via NDwiki and decoded in this session (`ndfs -t`/`ndtool -x`). No
install procedure exists on the floppy to transcribe — two bare `:NRF` files is the entire
contents.

---
**Parent:** [../README.md](../README.md) (Software catalog)
