# COSMOS Ethernet II - Extracted Distribution Files

**Files from the COSMOS Ethernet II Option distribution (floppy image `../210580B01-XX-01D.img`, product 210580 rev B01).**

---

## 68000 controller firmware (`:BPUN`)

| File | Contents |
|------|----------|
| `encos-ser-b0-b01.bpun` .. `encos-ser-b3-b01.bpun` | The four ENCOS server firmware banks for the ND-110063 Ethernet II controller (each 64KW = 128KB, BPUN container). Staged onto ND-100 segments and pushed to the controller's 68000 at `START-NETWORK-SERVER`. |
| `encos-ser-i-b01.dseg` | ENCOS server data segment file (not analyzed) |

The raw 68000 binaries stripped out of these BPUN containers - plus the container-format
analysis, checksum verification and the firmware reverse engineering - are in
[stripped/](stripped/README.md).

## ND-100-side files

| File | Contents |
|------|----------|
| `encos-err-i-b01.brf`, `encos-err-ii-b01.brf` | BRF object files for the ND-100-side supervisor (RT program ENNS0, segment ENCOSE0) - see "Related pieces" in [stripped/README.md](stripped/README.md) |
| `encos-in-b01.prog` | ENCOS installation program (not analyzed) |
| `encos-mon-i-b01.prog`, `encos-mon-ii-b01.prog` | ENCOS monitor programs (not analyzed) |
| `po-pwrfail-a00.prog` | Power-fail related program (from the file name; not analyzed) |
| `ue-ermsg-en-b03.err` | Error-message file, English (from the file name; not analyzed) |

---

**Parent:** [../README.md](../README.md)
