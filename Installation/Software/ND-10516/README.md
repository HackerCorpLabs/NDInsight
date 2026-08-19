# ND-10516 — FILE-HANDLER

> Status: IN-PROGRESS — real floppy decoded, real installer + main program + sample data found

| Field | Value |
|-------|-------|
| ND article number | `ND-10516` |
| Product name | FILE-HANDLER |
| Functional category | Databases & File Access |
| CPU target | ND-100 |

## What is known — real floppy, decoded

Floppy `10516C01-D` (downloaded via NDwiki, imaged by Torfinn "Tingo" Ingolfsen) mounts cleanly:

```
INSTALL-FH-C01:PROG        real installer program (compiled, not decoded)
FILE-HANDLER-C00:PROG      the main application (206848 bytes — the largest file on the disk)
FILE-HANDLER-C00:HELP      online help text
EMPLOYEES-C00:DATA         sample data file ("Employees")
SALES-DESC-C00:DATA        sample data-description file ("Sales")
SALES-C00:DATA             sample data file ("Sales")
UE-ERMSG-EN-B00:ERR        User Environment error-message file, English
```

**Reading it**: FILE-HANDLER ships with two worked sample datasets (Employees, Sales) — matching
the pattern already seen with UNIQUE's `CUSTOMER-REG`/`PART-REG`/`ORDER-REG` samples (see
[ND-10730](../ND-10730/README.md)) — suggesting this is a similar end-user data-management tool
rather than a programmer's library. The presence of a `UE-ERMSG` (User Environment error message)
file suggests FILE-HANDLER is built on the User Environment framework, same as other menu-driven
ND-100 utilities in this catalog.

**No `:MODE`/`:BATC` install script was found** — `INSTALL-FH-C01:PROG` is a compiled installer,
not decoded here.

## Documentation
- No PD sheet, no PI sheet located.

## Provenance & open items
- Source: one real floppy image, downloaded via NDwiki and decoded in this session
  (`ndfs -t`/`ndtool -x`).
- **TODO:** `FILE-HANDLER-C00:HELP` was not read; the installer's interactive prompts were not
  decoded (compiled binary).

---
**Parent:** [../README.md](../README.md) (Software catalog)
