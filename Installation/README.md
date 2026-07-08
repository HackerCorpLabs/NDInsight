# Installation

**Task-oriented procedures for installing SINTRAN III and application software on Norsk Data ND-100 / ND-500 systems.**

This branch is for *how to install*, as opposed to:
- `../Operations/` — operator/admin reference manuals and analysis
- `../SINTRAN/OS/` — how the kernel works internally
- `../Reference-Manuals/` — verbatim OCR'd ND manuals

---

## Structure

| Area | Contents |
|------|----------|
| [OS/](OS/README.md) | Installing the SINTRAN III operating system on a fresh disk from distribution floppies |
| [Software/](Software/README.md) | Installing application software / subsystems on a running SINTRAN III system *(later phase)* |

---

## Source material

Primary source is the version-specific SINTRAN distribution archives
(`SINTRAN-H`, `SINTRAN-K`, `SINTRAN-K05`, `SINTRAN-L`, `SINTRAN-M`), each containing:

- `Admin/` — install/init/startup command flows (`System initialisation.txt`, `install-log.txt`, `HENT-MODE.txt`, `START-SINTRAN-MULTIUSER.TXT`)
- `FILE-INFO/` — directory dumps of the golden disk (`BIGDISK0-*.txt`) and the distribution floppies (`VSX*.txt`)
- `FLOPPY/` — floppy images and notes

> **Honesty note:** Every command and file fact in this branch must be traceable to a
> source file in the distribution archive or an official ND manual. Anything inferred is marked `ASSUMPTION:`
> or `UNVERIFIED:`. Unknowns are left as explicit `TODO`.

---

**Parent:** [../README.md](../README.md)
