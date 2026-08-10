# SIBAS Developer Guide

**SIBAS - the Norsk Data database system for SINTRAN III**

**Status:** Reference index (OCR manuals linked below)

---

## What SIBAS Is

SIBAS is Norsk Data's database management system for NORD/SINTRAN systems. It is a
CODASYL / DBTG-style (network model) database: data is organised into **realms**,
described by a **schema**, and manipulated through a **DML** (Data Manipulation Language)
that is called from a **host language** program.

**Verified from the manuals** (ND-60.127.5): the DML is invoked from host-language
programs written in **COBOL** and **FORTRAN**; the manual uses the CODASYL/DBTG
terminology (realm, schema, DML, host language) throughout.

Two generations are documented here:

- **SIBAS I** - the earlier version (see the ND-60.057 appendix below).
- **SIBAS II** - the later version. Per its own printing record, **ND-60.127.5 replaces
  the previous manuals numbered ND-60.057**.

> NOTE: Beyond the data model and host-language bindings stated above, this guide does not
> yet contain worked DML examples. Use the reference manuals for the authoritative call
> syntax. Anything not taken directly from those manuals is marked as UNVERIFIED.

---

## Installing SIBAS-II

- **Older article** — [ND-10166](../../../Installation/Software/ND-10166/README.md), version
  [ND-10166E](../../../Installation/Software/ND-10166/ND-10166E/README.md) — a 9-part floppy
  set; install dialogue adapted from the later F-revision's real installer text (structurally
  identical file set), not independently verified for E.
- **Later article** — [ND-210166](../../../Installation/Software/ND-210166/README.md), version
  [ND-210166F02](../../../Installation/Software/ND-210166/ND-210166F02/README.md) — **fully
  verified from a complete, real 17-page PD sheet**: installer-driven and manual install paths,
  cold-start persistence, and the database-conversion procedure for pre-F databases.

## Programming Against SIBAS

See [SIBAS-DATABASE-PROGRAMMING.md](../../Workflow/SIBAS-DATABASE-PROGRAMMING.md) for the DDL
(realm/set definition, real `NEW REALM`/`CHANGE REALM` syntax from a real PD sheet) and the real
DML call names (`SOPDB`, `SFTCH`, `SGET`, `STORE`, `SMODFY`, `SRASE`, `SUBEG`/`SUENDO`, and more)
— sourced, not invented, with pointers to the full User/Operator manuals for exact parameters.

---

## Reference Manuals

All manuals live under [Reference-Manuals/](../../../Reference-Manuals/):

| Manual | Document # | Covers |
|--------|-----------|--------|
| [THE DATABASE SYSTEM SIBAS II - ND User Manual](../../../Reference-Manuals/ND-60.127.5%20EN%20THE%20DATABASE%20SYSTEM%20SIBAS%20II%20ND%20User%20Manual.md) | ND-60.127.5 EN | Primary SIBAS II user manual - schema, realms, DML, host-language (COBOL/FORTRAN) access (Version 05, 1986; replaces ND-60.057) |
| [SIBAS II Operator Manual](../../../Reference-Manuals/ND-30.009.3%20EN%20SIBAS%20II%20Operator%20Manual.md) | ND-30.009.3 EN | Operating SIBAS II - database administration and operator procedures |
| [SIBAS II for ND-100](../../../Reference-Manuals/210166F%20SIBAS%20II%20for%20ND-100.md) | 210166F | SIBAS II product documentation specific to the ND-100 |
| [The Data Base System SIBAS I - Users Manual, Appendix A](../../../Reference-Manuals/ND-60.057.03%20The%20Data%20Base%20System%20SIBAS%20I%20Users%20Manual%20Appendix%20A.md) | ND-60.057.03 | SIBAS I users manual (Appendix A) - the earlier generation superseded by ND-60.127.5 |

---

## Recommended Reading Order

1. **SIBAS II User Manual** (ND-60.127.5) - start here for the data model and DML.
2. **SIBAS II for ND-100** (210166F) - ND-100-specific product notes.
3. **SIBAS II Operator Manual** (ND-30.009.3) - for running and administering a database.
4. **SIBAS I Appendix A** (ND-60.057.03) - only if working with the older SIBAS I.

---

## See Also

- **Host languages:** [COBOL-DEVELOPER-GUIDE.md](COBOL-DEVELOPER-GUIDE.md), [FORTRAN-DEVELOPER-GUIDE.md](FORTRAN-DEVELOPER-GUIDE.md)
- **All reference manuals:** [../../../Reference-Manuals/](../../../Reference-Manuals/)
- **SINTRAN commands:** [../../../Reference-Manuals/SINTRAN-COMMANDS-REFERENCE.md](../../../Reference-Manuals/SINTRAN-COMMANDS-REFERENCE.md)

---

**Documentation Status:** Reference index (developer walkthrough TODO)
