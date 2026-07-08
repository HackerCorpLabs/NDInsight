# 00 — Installation Concepts & Terminology

> Status: SCAFFOLD. Verified facts cited from the SINTRAN-H archive (`Admin/install-log.txt`).

## MACM (the bootstrap loader)

MACM is the standalone loader that runs *before* SINTRAN to load the OS from floppy onto
the disk and to cold-start the system. The banner identifies the build, e.g.
`M A C M - 1 7 1 8 - K` (verified, H install-log).

Verified MACM commands (from H install-log banner):

| Command | Meaning |
|---------|---------|
| `)REDEF` | Redefine disc type |
| `)HENT`  | Get SINTRAN from the save-area |
| `22!`    | Start SINTRAN |
| `10,0$`  | Load SINTRAN from diskette |

## Save-area / )HENT vs )GJEM

- `)GJEM` (save) and `)9BYTT` are executed at the end of the floppy load (verified, H log).
- Later cold starts: load MACM from SINTRAN DISKETTE-I, type `)HENT`, wait for line feed
  (verified, H log).

## The install phases

1. Bootstrap (MACM, floppy → disk) — [02-BOOTSTRAP-MACM.md](02-BOOTSTRAP-MACM.md)
2. File-system init — [03-FILESYSTEM-INIT.md](03-FILESYSTEM-INIT.md)
3. S3 configuration — [04-S3-CONFIGURATION.md](04-S3-CONFIGURATION.md)
4. Patches — [05-PATCHES.md](05-PATCHES.md)
5. Startup & terminal config — [06-STARTUP-AND-TERMINAL-CONFIG.md](06-STARTUP-AND-TERMINAL-CONFIG.md)

## TODO
- CPU type / ID and how MACM build maps to CPU — source not located.
- Exact meaning of `)9SBLO`, `)9BYTT` octal/loader directives.
