# COSMOS Basic — boot wiring (how it hooks into cold/warm start)

The **COSMOS-specific** boot integration for the COSMOS Basic Module (rev E04). The generic boot
mechanism (initial commands, HENT-MODE, LOAD-MODE, XMSG) is in
[../../OS/SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md](../../OS/SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md);
the ready-to-use generic mode files are in
[../../OS/mode-files/](../../OS/mode-files/README.md). Install details are in
[COSMOS-Basic-Install-Guide.md](COSMOS-Basic-Install-Guide.md).

COSMOS ships **two** boot mode files (installed to `PACK-ONE:COSMOS-BASIC`):

| File | Half | When |
|------|------|------|
| `COS-HENT-E04:MODE` | cold — dumps reentrant programs, loads segments (persists in segment file) | run once per cold start |
| `COS-START-E04:MODE` | warm — starts the daemons (XFTRAD, COSPO, file server, TADADM) | every warm start |

## The two wiring lines

**Cold start** — add to `(SYSTEM)HENT-MODE:MODE`, after XMSG is loaded reentrant
(`@MODE (UTILITY)XMSG-LOAD:MODE,,,`):

```
@MODE (PACK-ONE:COSMOS-BASIC)COS-HENT-E04:MODE,,
```

**Warm start** — add to `(SYSTEM)LOAD-MODE:MODE`, after XMSG is started
(`@MODE (SYSTEM)XMSG-START:MODE,,,`):

```
@MODE (PACK-ONE:COSMOS-BASIC)COS-START-E04:MODE,,
```

> The warm-start file is split into a thin `LOAD-MODE:BATC` wrapper (login + terminator) and the
> real config `LOAD-MODE:MODE`. The COSMOS line goes in the **`:MODE`**. See
> [../../OS/mode-files/README.md](../../OS/mode-files/README.md).

Both lines are already present in the templates in
[../../OS/mode-files/](../../OS/mode-files/README.md).

## Prerequisites (must be true before COSMOS starts)

- **XMSG running** — COSMOS depends on it; that is why the warm-start line comes *after*
  `XMSG-START:MODE`.
- **TADADM** — `COS-START-E04:MODE` runs `START-TADADM` itself (via `COS-FA-SERV-E04:MODE`). Only
  add a standalone `@START-TADADM` if you did **not** install the COSMOS file-server part.
- **COSMOS SPOOLING** configured in SINTRAN (peripheral `COSMOS-SPOOLING` at `1731B`).
- **REMOTE FILE ACCESS option** configured (File-User part); **TADs** configured (File-Server part).

## VSX vs VSE

This machine is **VSX**; the installer copied the VSX variants
(`COS-COSP-VSX-E02`, `COS-FAU-VSX-E03`). The generic names inside `COS-HENT-E04:MODE`
(`COS-COSP-VS-E`, `COS-FAU-VS-E`) resolve to them by SINTRAN's component-wise abbreviation match.
Details: [COSMOS-Basic-Install-Guide.md](COSMOS-Basic-Install-Guide.md) (sections 3–4).
