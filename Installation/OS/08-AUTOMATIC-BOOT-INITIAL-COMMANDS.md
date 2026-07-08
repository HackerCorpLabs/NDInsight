# 08 — Automatic Boot: Mode Files & INITIAL-COMMANDS

> Status: SCAFFOLD. **No assumptions.** Only what is observed in source is stated as fact;
> everything else is a TODO to validate via documentation, actual SINTRAN commands, or
> manual exercise.

Goal: document how the system is set up for automatic boot — running a startup mode file
(e.g. `LOAD-MODE:MODE`) and the INITIAL-COMMAND mechanism.

## Observed in source (verified)
- The SINTRAN-M archive `Admin/HENT-MODE.txt` begins with `@CC HENT-MODE:MODE` and is a mode file
  of `@`-commands (RT-LOADER, INIT-BACKGROUND, CHANGE-DATAFIELD, BATCH, MAIL, SET-AVAILABLE).

## To document (validate first)
- `LOAD-MODE:MODE` — the automatic-boot mode file: contents and when it runs. TODO: locate source.
- `INITIAL-COMMAND` / `NEXT-INITIAL-COMMAND` — how startup commands are registered.
- `LIST-INITIAL-COMMANDS` — how to inspect the configured boot command chain.
- Relationship between mode files and the INITIAL-COMMAND chain on automatic boot.

> None of the INITIAL-COMMAND command semantics are documented here yet because they are not
> present in the source files read so far. They must be validated against an ND manual or by
> manual exercise (ask the user) before being written as fact.

## Verified examples now available
`research/HDD-IMAGE-FINDINGS.md` §2 contains **real HENT-MODE files extracted from disk
images** showing the `@INITIAL-COMMAND` / `@NEXT-INITIAL-COMMAND` chain ending in
`APPEND-BATCH 1 LOAD-MODE:MODE` — confirming the automatic-boot mechanism. Promote those
examples here once cross-checked against the System Supervisor manual §3.2.2–3.2.3.

## Cross-links
- [06-STARTUP-AND-TERMINAL-CONFIG.md](06-STARTUP-AND-TERMINAL-CONFIG.md) — the warm-boot/startup scripts.
- [research/HDD-IMAGE-FINDINGS.md](research/HDD-IMAGE-FINDINGS.md) §2–§4 — extracted MODE files & chaining.
