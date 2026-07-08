# COSMOS Basic - Extracted Distribution Files

**SINTRAN files from the COSMOS Basic Module distribution (see [../README.md](../README.md); floppy image `../210374E04-XX-01D.img`).**

File revisions on this media are E02-E04 (the PI sheet describes rev G; names map `-G` to `-E`).

---

## Programs (`:PROG`)

| File | Program |
|------|---------|
| `cos-bas-in-e04.prog` | COSMOS Basic installation program |
| `cos-conn-to-e02.prog` | CONNECT-TO client (TAD-over-XMSG terminal access) |
| `cos-fa-serv-e04.prog` | File-Access server |
| `cos-file-tra-e02.prog` | File Transfer |
| `cos-fs-admin-e02.prog` | FS-Administrator |
| `cos-spoo-ser-e02.prog` | Spooling server |
| `cos-xftra-e02.prog` | XMSG transport exerciser |

Four of these (`conn-to`, `fa-serv`, `file-tra`, `xftra`) have been fully reverse engineered;
see [../../../../SINTRAN/XMSG/DOC/COSMOS-RE/README.md](../../../../SINTRAN/XMSG/DOC/COSMOS-RE/README.md).

## Loadable binaries (`:BPUN`)

`cos-cosp-vse-e02.bpun`, `cos-cosp-vsx-e02.bpun`, `cos-fau-vse1-e03.bpun`,
`cos-fau-vse2-e03.bpun`, `cos-fau-vsx-e03.bpun`, `cos-fsart-e02.bpun`
(FAU = COSMOS File-Access User, in VSE/VSX variants; each is loaded by its matching
`.mode` file. What COSP and FSART stand for is not verified here - see the install guide.)

## Mode files (`:MODE`)

`cos-hent-e04.mode`, `cos-start-e04.mode` (boot/start), plus one install/start mode file per
component: `cos-cosp-vse-e02.mode`, `cos-cosp-vsx-e02.mode`, `cos-def-prin-e02.mode`,
`cos-fa-serv-e04.mode`, `cos-fau-vse-e03.mode`, `cos-fau-vsx-e03.mode`, `cos-fsart-e02.mode`,
`cos-xftra-e02.mode`.

How these are wired into the boot sequence is documented in
[../COS-BOOT-WIRING.md](../COS-BOOT-WIRING.md); the install order is in
[../COSMOS-Basic-Install-Guide.md](../COSMOS-Basic-Install-Guide.md).

---

**Parent:** [../README.md](../README.md)
