# SINTRAN III password / login - SUPERSEDED (redirect)

The reverse-engineering of the SINTRAN III password/login is **complete**. The
authoritative write-up is:

> **[PASSWORD-ALGORITHM.md](PASSWORD-ALGORITHM.md)** (same folder)

It covers the verified fold (`acc = ROL16(acc,3) + toupper(char)`, a 16-bit word),
the S3CP `LOGIN` disassembly with instruction decode, the octal-digit login
backdoor (and its break-strategy entry mechanism), the 16-bit keyspace security
assessment, and the tools `sintran-passcrack.c` / `sintran-passdb.c`.

This file previously held earlier notes and mermaid diagrams derived from a
**mis-based carve** (the old raw-disk base pointed to the wrong location, so the
disassembled bytes were not the real login code). Those findings were WRONG - e.g.
they placed the fold at "PWLOG / S3ISYS 145734", which on the corrected carve is
not even executable code. They have been removed here to avoid confusion; see the
git history if you need the retired notes.
