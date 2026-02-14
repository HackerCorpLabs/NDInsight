# SINTRAN III Logical Device Number Reference

All logical device numbers used by SINTRAN III, from 0₈ through 3377₈ (0-1791 decimal).

**Source**: Appendix B of ND-860228-2-EN "SINTRAN III Monitor Calls"

---

## Overview

Logical device numbers are 11-bit values (0-3777₈ = 0-2047 decimal). They are organized into 32 groups of 64 entries each via the LDNT (Logical Device Number Table). The logical device number encodes the group and entry index:

```
Logical device number = group_index * 64 + entry_index
                      = group_index * 100₈ + entry_index
```

### Summary Table

| Octal Range | Decimal Range | LDNT Group | Description |
|-------------|---------------|------------|-------------|
| 0-77 | 0-63 | DV000 (0) | Character devices (terminals 1-32, tape, printers, modems) |
| 100-177 | 64-127 | (reserved) (1) | Open mass storage files |
| 200-277 | 128-191 | DV200 (2) | Internal devices (32 general + 24 SIBAS + special) |
| 300-377 | 192-255 | DV300 (3) | User semaphores 1-64 |
| 400-477 | 256-319 | DV400 (4) | CAMAC, process control, CONNECT, direct task devices |
| 500-577 | 320-383 | DV500 (5) | System semaphores and data fields |
| 600-677 | 384-447 | DV600 (6) | Spooling semaphores 31-60 |
| 700-777 | 448-511 | DV700 (7) | HDLC DMA links 7-32, NORCOM systems 1-3 |
| 1000-1077 | 512-575 | D1000 (8) | Floppy, HASP, line printers, terminals 33-64 |
| 1100-1177 | 576-639 | D1100 (9) | ECC disk controllers, mag tape, floppy, spooling 1-3 |
| 1200-1277 | 640-703 | D1200 (10) | Winchester disks, batch 1-10, internal device DFs |
| 1300-1377 | 704-767 | D1300 (11) | HASP DMA, ECC disk ctrl 2, DMA printers, HDLC 1-6, X.21 |
| 1400-1477 | 768-831 | D1400 (12) | Terminal access devices (TADs) 1-64 |
| 1500-1577 | 832-895 | D1500 (13) | TADs 65-96, Telefix terminals/background 1-16 |
| 1600-1677 | 896-959 | D1600 (14) | DMA device buffer header semaphores (64 headers) |
| 1700-1777 | 960-1023 | D1700 (15) | PIOC 1-16, virtual disk, spooling 13-15, SIBAS 0-23 |
| 2000-2077 | 1024-1087 | D2000 (16) | Terminals 65-128 |
| 2100-2177 | 1088-1151 | D2100 (17) | Universal DMA/Vicom, GPIB, spooling 16-30, COSMOS |
| 2200-2277 | 1152-1215 | D2200 (18) | SCSI, Ethernet, BDIO pools |
| 2300-2377 | 1216-1279 | D2300 (19) | User-defined logical device numbers |
| 2400-2477 | 1280-1343 | D2400 (20) | Octobus units 0-3 (J/K-version compatible) |
| 2500-2577 | 1344-1407 | D2500 (21) | Directory semaphores (entries 0-31) |
| 2600-2677 | 1408-1471 | D2600 (22) | Directory semaphores (entries 32-47), rest unused |
| 2700-2777 | 1472-1535 | D2700 (23) | Terminals 129-192 |
| 3000-3077 | 1536-1599 | D3000 (24) | Terminals 193-256 |
| 3100-3177 | 1600-1663 | D3100 (25) | Batch processes 11-30, rest unused |
| 3200-3277 | 1664-1727 | D3300 (27) | Remote open files |
| 3300-3377 | 1728-1791 | D3400 (28) | Batch extra queue devices 1-30, rest unused |

> **Note**: LDNT group 26₁₀ (RDLNO) is always empty (CNVRT[26]=0).

---

## Detailed Device Listings

### 0-77₈ — Character Devices (LDNT Group 0: DV000)

| Octal | Dec | Description |
|-------|-----|-------------|
| 0 | 0 | INBT; INCH (background): edited input, else: dummy |
| 1 | 1 | Background: "own terminal" or RT: Terminal 1 (console) |
| 2 | 2 | Error device (output) / Paper tape reader 1 (input, on console) |
| 3 | 3 | Paper tape punch 1 |
| 4 | 4 | Card reader 1 |
| 5 | 5 | Line printer 1 |
| 6 | 6 | Synchronous modem 1 |
| 7 | 7 | Terminal 17 |
| 10 | 8 | Plotter 1 |
| 11 | 9 | Terminal 2 |
| 12 | 10 | Paper tape reader 2 / special internal device for mode files |
| 13 | 11 | Paper tape punch 2 / special internal device for mode files |
| 14 | 12 | Bus switch device |
| 15 | 13 | Line printer 2 |
| 16 | 14 | Synchronous modem 2 |
| 17 | 15 | Terminal 18 |
| 20 | 16 | Cassette drive 1 |
| 21 | 17 | Cassette drive 2 |
| 22 | 18 | Versatec printer/plotter 1 on DMA / IBM communication |
| 23 | 19 | Versatec printer/plotter 2 on DMA |
| 24 | 20 | Tektronix display |
| 25 | 21 | Magnetic tape controller 1, unit 2 |
| 26 | 22 | Synchronous modem 5 |
| 27 | 23 | Synchronous modem 6 |
| 30 | 24 | Synchronous modem 3 |
| 31 | 25 | Synchronous modem 4 |
| 32 | 26 | Magnetic tape controller 2, unit 0 |
| 33 | 27 | Magnetic tape controller 1, unit 3 |
| 34 | 28 | Magnetic tape controller 2, unit 1 |
| 35 | 29 | Card punch 3 |
| 36 | 30 | CDC link / TTY link sender |
| 37 | 31 | TTY link receiver |
| 40 | 32 | Magnetic tape controller 1, unit 0 |
| 41 | 33 | Magnetic tape controller 1, unit 1 |
| 42 | 34 | Terminal 3 |
| 43 | 35 | Terminal 4 / barcode reader |
| 44 | 36 | Terminal 5 / barcode reader |
| 45 | 37 | Terminal 6 |
| 46 | 38 | Terminal 7 |
| 47 | 39 | Terminal 8 |
| 50 | 40 | Card punch 1 |
| 51 | 41 | Card punch 2 |
| 52 | 42 | Terminal 19 |
| 53 | 43 | Terminal 20 |
| 54 | 44 | Terminal 21 |
| 55 | 45 | Terminal 22 |
| 56 | 46 | Terminal 23 |
| 57 | 47 | Terminal 24 |
| 60 | 48 | Terminal 9 |
| 61 | 49 | Terminal 10 |
| 62 | 50 | Terminal 11 |
| 63 | 51 | Terminal 12 |
| 64 | 52 | Terminal 13 |
| 65 | 53 | Terminal 14 |
| 66 | 54 | Terminal 15 |
| 67 | 55 | Terminal 16 |
| 70 | 56 | Terminal 25 / special synchronous modem 5 |
| 71 | 57 | Terminal 26 / special synchronous modem 6 |
| 72 | 58 | Terminal 27 / special sync modem 7 / Graf cassette 1 |
| 73 | 59 | Terminal 28 / special sync modem 8 / Graf cassette 2 |
| 74 | 60 | Terminal 29 / special sync modem 9 / photosetter 1 / Harris photosetter 1 |
| 75 | 61 | Terminal 30 / photosetter 2 / Harris photosetter 2 |
| 76 | 62 | Terminal 31 / photosetter 3 |
| 77 | 63 | Terminal 32 |

---

### 100-177₈ — Open Mass Storage Files (LDNT Group 1: reserved)

Logical device numbers 100₈-177₈ (64-127 decimal) are reserved for open mass storage files. These are dynamically assigned when files are opened and do not correspond to physical devices. LDNT group 1 is reserved and not populated via CNVRT.

---

### 200-277₈ — Internal Devices (LDNT Group 2: DV200)

| Octal | Dec | Description |
|-------|-----|-------------|
| 200-237 | 128-159 | Internal devices 1-32 |
| 240-267 | 160-183 | SIBAS internal devices 1-24 |
| 270-275 | 184-189 | Not used |
| 276 | 190 | Internal device for ERS/SINTRAN III Watchdog |
| 277 | 191 | Internal device for FTX error logger |

---

### 300-377₈ — User Semaphores (LDNT Group 3: DV300)

| Octal | Dec | Description |
|-------|-----|-------------|
| 300-372 | 192-250 | User semaphores 1-59 |
| 373-377 | 251-255 | User semaphores 60-64 (also used by the Backup-System) |

---

### 400-477₈ — Process Control and CONNECT Devices (LDNT Group 4: DV400)

| Octal | Dec | Description |
|-------|-----|-------------|
| 400-407 | 256-263 | CAMAC 1-8 / Special DMA data fields 1-6 / Digital I/O units 1-8 / DR11C units 1-8 / Aristogrid digitizer units 1-4 |
| 410-420 | 264-272 | CAMAC 9-17 / Special DMA data fields 7-11 / Digital I/O units 9-17 / DR11C units 9-17 / Norcontrol process I/O units 1-9 / Aristogrid digitizer units 5-9 |
| 421-423 | 273-275 | Digital I/O units 18-20 / DR11C units 18-20 / Norcontrol process I/O units 10-12 / Aristogrid digitizer units 9-10 |
| 424-426 | 276-278 | Norcontrol process I/O units 13-15 |
| 427 | 279 | Not used |
| 430-437 | 280-287 | Analog input units 1-8 |
| 440-443 | 288-291 | Direct task levels 6-9 |
| 444-447 | 292-295 | Not used |
| 450-467 | 296-311 | CONNECT devices 1-16 |
| 470 | 312 | ND 23 — programmed clock |
| 471-477 | 313-319 | Not used |

---

### 500-577₈ — System Devices (LDNT Group 5: DV500)

| Octal | Dec | Description |
|-------|-----|-------------|
| 500 | 320 | Internal device for error message RT-program |
| 501 | 321 | Semaphore for segment transfer |
| 502 | 322 | Not used |
| 503 | 323 | RT-Loader command semaphore |
| 504 | 324 | General semaphore for file system |
| 505 | 325 | User-file-buffer semaphore |
| 506 | 326 | Object-file-buffer semaphore |
| 507 | 327 | RT-open-file-table semaphore |
| 510-514 | 328-332 | Not used |
| 515 | 333 | DF1, file-transfer for RT, semaphore for disk 1-4 |
| 516 | 334 | DF2, open-file monitor call from RT-program data field |
| 517 | 335 | RTFIL semaphore |
| 520 | 336 | NOTIS-IR semaphore 2 |
| 521 | 337 | Device buffer allocation semaphore |
| 522-525 | 338-341 | Not used |
| 526 | 342 | DF3, transfer semaphore for magnetic tape 1 |
| 527 | 343 | Spooling queue semaphore |
| 530 | 344 | Accounting semaphore |
| 531 | 345 | CDC link monitor call data field |
| 532 | 346 | Spooling device 4, queue semaphore |
| 533 | 347 | Spooling device 4, I/O semaphore |
| 534 | 348 | Spooling device 5, queue semaphore |
| 535 | 349 | Spooling device 5, I/O semaphore |
| 536 | 350 | Spooling device 6, queue semaphore |
| 537 | 351 | Spooling device 6, I/O semaphore |
| 540 | 352 | Internal device Remote Batch IBM |
| 541 | 353 | Internal device Remote Batch UNIVAC |
| 542 | 354 | Internal device Remote Batch Honeywell Bull |
| 543 | 355 | Internal device Remote Batch CDC |
| 544-553 | 356-363 | ECC disk controller 3, units 0-3 (directory/bit file semaphores) |
| 554-557 | 364-367 | Not used |
| 560 | 368 | Magnetic tape controller 1, data field |
| 561 | 369 | All magnetic tapes, directory semaphore |
| 562 | 370 | Spooling device 11, queue semaphore |
| 563 | 371 | Magnetic tape controller 2, unit 2, I/O data field |
| 564 | 372 | Magnetic tape controller 2, unit 3, I/O data field |
| 565 | 373 | ECC disk controller 3, data field |
| 566 | 374 | ECC disk controller 4, data field |
| 567 | 375 | CDC link data field |
| 570-573 | 376-379 | Not used |
| 574 | 380 | Monitor call data field for cassette |
| 575 | 381 | Cassette data field |
| 576 | 382 | DF5, monitor call data field for Versatec 1 |
| 577 | 383 | Versatec data field |

---

### 600-677₈ — Spooling Semaphores 31-60 (LDNT Group 6: DV600)

| Octal | Dec | Description |
|-------|-----|-------------|
| 600 | 384 | BADMIN semaphores |
| 601 | 385 | BASEM |
| 602 | 386 | Default subsystem semaphore |
| 603 | 387 | Not used |
| 604-677 | 388-447 | Spooling devices 31-60, queue and I/O semaphores (pairs) |

Spooling device pattern (604₈ onwards): even = queue semaphore, odd = I/O semaphore, for devices 31 through 60.

---

### 700-777₈ — HDLC DMA and NORCOM (LDNT Group 7: DV700)

| Octal | Dec | Description |
|-------|-----|-------------|
| 700-725 | 448-469 | HDLC DMA links 7-17 (input/output pairs) / NORCOM system 1 buffers and selectors |
| 726-751 | 470-489 | HDLC DMA links 18-27 (input/output pairs) / NORCOM system 2 buffers and selectors / ACM 1-5 |
| 752-763 | 490-499 | HDLC DMA links 28-32 (input/output pairs) / NORCOM system 3 semigraphic buffers |
| 764-767 | 500-503 | NORCOM system 3, graphic buffers 1, 3, 5, 7 |
| 770-777 | 504-511 | NORCOM system 3, selector modules 1-8 |

HDLC DMA link pattern: even octal = input, odd octal = output.

---

### 1000-1077₈ — Character Devices (LDNT Group 8: D1000)

| Octal | Dec | Description |
|-------|-----|-------------|
| 1000-1002 | 512-514 | Floppy disk controller 1, units 0-2, I/O data field |
| 1003-1005 | 515-517 | Floppy disk controller 2, units 0-2, I/O data field |
| 1006-1013 | 518-523 | HASP DMA 1-6, I/O data field |
| 1014-1015 | 524-525 | Line printer 3-4, I/O data field |
| 1016-1037 | 526-543 | Not used |
| 1040-1077 | 544-575 | Terminals 33-64 |

---

### 1100-1177₈ — System Devices (LDNT Group 9: D1100)

| Octal | Dec | Description |
|-------|-----|-------------|
| 1100 | 576 | ECC disk controller 1, data field |
| 1101-1102 | 577-578 | ECC disk controller 1, unit 0, directory/bit file semaphores |
| 1103-1110 | 579-584 | Not used |
| 1111 | 585 | Magnetic tape controller 2, data field |
| 1112 | 586 | ECC disk controller 4, unit 0, directory table semaphore |
| 1113 | 587 | Floppy disk controller 1, unit 3, I/O data field |
| 1114 | 588 | ECC disk controller 4, unit 0, bit file buffer semaphore |
| 1115 | 589 | Floppy disk controller 2, unit 3, I/O data field |
| 1116 | 590 | DR 7, transfer semaphore for magnetic tape controller 2 |
| 1117-1124 | 591-596 | ECC disk controller 1, units 1-3, directory/bit file semaphores |
| 1125 | 597 | Versatec controller 2 |
| 1126 | 598 | Monitor call data field for Versatec controller 2 |
| 1127 | 599 | DF 39, magnetic tape controller 3 monitor call data field |
| 1130-1133 | 600-603 | Not used |
| 1134-1135 | 604-605 | Floppy disk controller 1, unit 3, directory/bit file semaphores |
| 1136-1141 | 606-609 | Spooling devices 1-2, queue/I/O semaphores |
| 1142 | 610 | Spooling system general semaphore |
| 1143 | 611 | Spooling system wait for used pages semaphore |
| 1144 | 612 | Spooling system wait for free pages semaphore |
| 1145-1146 | 613-614 | Floppy disk controller 1, data field / monitor call DF |
| 1147 | 615 | Floppy disk controller 2, unit 3, directory table semaphore |
| 1150-1155 | 616-621 | Floppy disk controller 1, units 0-2, directory/bit file semaphores |
| 1156-1157 | 622-623 | Floppy disk controller 2, data field / monitor call DF |
| 1160 | 624 | Floppy disk controller 2, unit 3, bit file buffer semaphore |
| 1161-1166 | 625-630 | Floppy disk controller 2, units 0-2, directory/bit file semaphores |
| 1167 | 631 | DMA line printer 1, data field |
| 1170 | 632 | Monitor call data field for DMA line printer 1 |
| 1171-1172 | 633-634 | ECC disk controller 4, unit 2, directory/bit file semaphores |
| 1173-1174 | 635-636 | Spooling device 3, queue/I/O semaphore |
| 1175 | 637 | DMA line printer 2, data field |
| 1176 | 638 | Monitor call data field for DMA line printer 2 |
| 1177 | 639 | Spooling semaphore for id data buffer |

---

### 1200-1277₈ — System Devices (LDNT Group 10: D1200)

| Octal | Dec | Description |
|-------|-----|-------------|
| 1200 | 640 | NOTPS system semaphore |
| 1201 | 641 | DMAC command semaphore |
| 1202 | 642 | RT-PROGRAM-LOG semaphore |
| 1203 | 643 | Histogram commands semaphore |
| 1204 | 644 | SINTRAN Service Program command semaphore |
| 1205 | 645 | Mail system semaphore |
| 1206 | 646 | Terminal 1, data field |
| 1207 | 647 | ECC disk controller 2, data field |
| 1210-1221 | 648-657 | Internal devices 1-5, data field / monitor call data field (pairs) |
| 1222 | 658 | Accounting semaphore |
| 1223 | 659 | NOTIS-IR semaphore |
| 1224 | 660 | ST-506 (Winchester) disk controller 1, data field / STC mag tape controller 4 |
| 1225-1230 | 661-664 | ST-506 Winchester disk ctrl 1, units 0-1 dir/bit file sema / STC mag tape ctrl 4, units 0-3 I/O DF |
| 1231 | 665 | ST-506 (Winchester) disk controller 2, data field / STC mag tape controller 3 |
| 1232-1235 | 666-669 | ST-506 Winchester disk ctrl 2, units 0-1 dir/bit file sema / STC mag tape ctrl 3, units 0-3 I/O DF |
| 1236-1261 | 670-689 | Batch processes 1-10, data field / internal device (pairs) |
| 1262-1271 | 690-697 | Spooling devices 7-10, queue/I/O semaphores |
| 1272-1276 | 698-702 | Monitor call data fields for internal devices 1-5 |
| 1277 | 703 | DF 40, magnetic tape controller 4, monitor call data field |

---

### 1300-1377₈ — System Devices (LDNT Group 11: D1300)

| Octal | Dec | Description |
|-------|-----|-------------|
| 1300-1301 | 704-705 | ECC disk controller 4, unit 3, directory/bit file semaphores |
| 1302 | 706 | Device buffer semaphore |
| 1303-1332 | 707-730 | HASP DMA 1-6, input/output data fields and monitor call DFs |
| 1333-1342 | 731-738 | ECC disk controller 2, units 0-3, directory/bit file semaphores |
| 1343-1346 | 739-742 | DMA line printers 3-4, data fields / monitor call DFs |
| 1347 | 743 | Spooling device 11, I/O semaphore |
| 1350-1351 | 744-745 | Spooling device 12, queue/I/O semaphore |
| 1352 | 746 | RT-PROGRAM-LOG command semaphore |
| 1353-1357 | 747-751 | Not used |
| 1360-1373 | 752-763 | HDLC DMA links 1-6, input/output (pairs) / sync modems 1-6 for HDLC |
| 1374-1377 | 764-767 | X.21 line numbers 1-4 |

---

### 1400-1477₈ — Terminal Access Devices 1-64 (LDNT Group 12: D1400)

| Octal | Dec | Description |
|-------|-----|-------------|
| 1400-1477 | 768-831 | Terminal access devices (TADs) 1-64 |

TAD N has logical device number 1400₈ + (N-1).

---

### 1500-1577₈ — TADs 65-96 and Telefix (LDNT Group 13: D1500)

| Octal | Dec | Description |
|-------|-----|-------------|
| 1500-1537 | 832-863 | Terminal access devices (TADs) 65-96 |
| 1540-1557 | 864-879 | Telefix terminals 1-16 |
| 1560-1577 | 880-895 | Telefix background terminals 1-16 |

---

### 1600-1677₈ — DMA Buffer Header Semaphores (LDNT Group 14: D1600)

| Octal | Dec | Description |
|-------|-----|-------------|
| 1600-1677 | 896-959 | DMA device buffer header semaphores for headers 0₈-77₈ (64 headers) |

Header number N₈ has logical device number 1600₈ + N.

---

### 1700-1777₈ — System Devices (LDNT Group 15: D1700)

| Octal | Dec | Description |
|-------|-----|-------------|
| 1700-1716 | 960-974 | PIOC numbers 1-15 |
| 1717 | 975 | PIOC number 16 / virtual disk driver 1 |
| 1720-1721 | 976-977 | Virtual disk drivers 3-4 |
| 1722-1727 | 978-983 | Spooling devices 13-15, queue/I/O semaphores |
| 1730 | 984 | COSMOS file access, DF data field |
| 1731 | 985 | COSMOS Spooling, peripheral device |
| 1732-1735 | 986-989 | ST-506 Winchester disk ctrl 1, units 0-1, directory/bit file semaphores |
| 1737-1742 | 991-994 | ST-506 Winchester disk ctrl 2, units 0-1, directory/bit file semaphores |
| 1743-1747 | 995-999 | Not used |
| 1750-1777 | 1000-1023 | SIBAS numbers 0-23 |

---

### 2000-2077₈ — Terminals 65-128 (LDNT Group 16: D2000)

| Octal | Dec | Description |
|-------|-----|-------------|
| 2000-2077 | 1024-1087 | Terminals 65-128 |

Terminal N (65-128) has logical device number 2000₈ + (N-65).

---

### 2100-2177₈ — System Devices (LDNT Group 17: D2100)

| Octal | Dec | Description |
|-------|-----|-------------|
| 2100-2117 | 1088-1103 | Universal DMA / Vicom interfaces 1-16 |
| 2120-2127 | 1104-1111 | GPIB interface numbers 0-7 |
| 2130-2165 | 1112-1141 | Spooling devices 16-30, queue/I/O semaphores (pairs) |
| 2166-2167 | 1142-1143 | COSMOS Spooling, queue/I/O semaphore |
| 2170-2177 | 1144-1151 | Not used |

---

### 2200-2277₈ — SCSI, Ethernet, BDIO (LDNT Group 18: D2200)

| Octal | Dec | Description |
|-------|-----|-------------|
| 2200 | 1152 | Disk access log data field |
| 2201 | 1153 | Disk access log buffer semaphore |
| 2202-2205 | 1154-1157 | SCSI adaptors 1-4 |
| 2206-2207 | 1158-1159 | SCSI streamer tape drives 1-2 |
| 2210-2225 | 1160-1173 | SCSI magnetic disk drives 1-14 |
| 2226-2227 | 1174-1175 | SCSI streamer tape 1-2, I/O data field |
| 2230-2231 | 1176-1177 | SCSI streamer tape 1-2, DF data field |
| 2232-2235 | 1178-1181 | SCSI optical disk drives 1-4 |
| 2236-2237 | 1182-1183 | Not used |
| 2240-2243 | 1184-1187 | Ethernet interfaces 1-4 |
| 2244-2256 | 1188-1198 | Not used |
| 2257 | 1199 | Domino allocation semaphore |
| 2260-2277 | 1200-1215 | BDIO pools 1-16 |

---

### 2300-2377₈ — User-Defined (LDNT Group 19: D2300)

| Octal | Dec | Description |
|-------|-----|-------------|
| 2300-2377 | 1216-1279 | User-defined logical device numbers (64 entries) |

These are available for user/application-specific device assignments.

---

### 2400-2477₈ — Octobus Devices (LDNT Group 20: D2400)

| Octal | Dec | Description |
|-------|-----|-------------|
| 2400-2417 | 1280-1295 | Octobus unit 0 (J/K-version compatible, 16 subdevices) |
| 2420-2437 | 1296-1311 | Octobus unit 1 (J/K-version compatible, 16 subdevices) |
| 2440-2457 | 1312-1327 | Octobus unit 2 (J/K-version compatible, 16 subdevices) |
| 2460-2477 | 1328-1343 | Octobus unit 3 (J/K-version compatible, 16 subdevices) |

---

### 2500-2577₈ — Directory Semaphores, Part 1 (LDNT Group 21: D2500)

| Octal | Dec | Description |
|-------|-----|-------------|
| 2500-2577 | 1344-1407 | Directory entry semaphores for entries 0-31 (pairs: directory semaphore + bit file semaphore) |

Directory entry N (0-31) uses:
- 2500₈ + N*2 = directory semaphore
- 2501₈ + N*2 = bit file semaphore

---

### 2600-2677₈ — Directory Semaphores, Part 2 (LDNT Group 22: D2600)

| Octal | Dec | Description |
|-------|-----|-------------|
| 2600-2637 | 1408-1439 | Directory entry semaphores for entries 32-47 (pairs) |
| 2640-2677 | 1440-1471 | Not used |

---

### 2700-2777₈ — Terminals 129-192 (LDNT Group 23: D2700)

| Octal | Dec | Description |
|-------|-----|-------------|
| 2700-2777 | 1472-1535 | Terminals 129-192 |

Terminal N (129-192) has logical device number 2700₈ + (N-129).

---

### 3000-3077₈ — Terminals 193-256 (LDNT Group 24: D3000)

| Octal | Dec | Description |
|-------|-----|-------------|
| 3000-3077 | 1536-1599 | Terminals 193-256 |

Terminal N (193-256) has logical device number 3000₈ + (N-193).

---

### 3100-3177₈ — Batch Semaphores 11-30 (LDNT Group 25: D3100)

| Octal | Dec | Description |
|-------|-----|-------------|
| 3100-3147 | 1600-1639 | Batch processes 11-30, data field / internal device (pairs) |
| 3150-3177 | 1640-1663 | Not used |

---

### 3200-3277₈ — Remote Open Files (LDNT Group 27: D3300)

| Octal | Dec | Description |
|-------|-----|-------------|
| 3200-3277 | 1664-1727 | Used for remote open files (64 entries) |

---

### 3300-3377₈ — Batch Extra Queue Devices (LDNT Group 28: D3400)

| Octal | Dec | Description |
|-------|-----|-------------|
| 3300-3335 | 1728-1757 | Batch processes 1-30, extra batch queue device |
| 3336-3377 | 1758-1791 | Not used |

---

## Terminal Number Quick Reference

Terminals are scattered across multiple LDNT groups. This table summarizes all terminal ranges:

| Terminal Range | Octal Device | LDNT Group |
|----------------|--------------|------------|
| Terminal 1 (console) | 1 | DV000 |
| Terminals 2-32 | scattered in 0-77₈ | DV000 |
| Terminals 33-64 | 1040-1077 | D1000 |
| Terminals 65-128 | 2000-2077 | D2000 |
| Terminals 129-192 | 2700-2777 | D2700 |
| Terminals 193-256 | 3000-3077 | D3000 |

Terminals 1-32 do not follow a simple sequential pattern in group DV000 because early device numbers were assigned to various peripherals (tape readers, printers, modems, plotters). The terminal assignments within 0-77₈ are:

| Terminal | Octal Device |
|----------|--------------|
| 1 | 1 |
| 2 | 11 |
| 3 | 42 |
| 4 | 43 |
| 5 | 44 |
| 6 | 45 |
| 7 | 46 |
| 8 | 47 |
| 9 | 60 |
| 10 | 61 |
| 11 | 62 |
| 12 | 63 |
| 13 | 64 |
| 14 | 65 |
| 15 | 66 |
| 16 | 67 |
| 17 | 7 |
| 18 | 17 |
| 19 | 52 |
| 20 | 53 |
| 21 | 54 |
| 22 | 55 |
| 23 | 56 |
| 24 | 57 |
| 25 | 70 |
| 26 | 71 |
| 27 | 72 |
| 28 | 73 |
| 29 | 74 |
| 30 | 75 |
| 31 | 76 |
| 32 | 77 |

---

## Device Category Index

Quick lookup by device type:

| Category | Octal Ranges |
|----------|-------------|
| **Terminals** | 1, 7, 11, 17, 42-47, 52-57, 60-77, 1040-1077, 2000-2077, 2700-2777, 3000-3077 |
| **TADs** | 1400-1537 |
| **Telefix** | 1540-1577 |
| **Magnetic tape** | 25, 32-34, 40-41, 526, 560-564, 1111, 1116, 1127, 1277, 2226-2231 |
| **Floppy disk** | 1000-1005, 1113, 1115, 1134-1166 |
| **ECC disk** | 544-553, 565-566, 1100-1124, 1171-1172, 1300-1301, 1333-1342 |
| **Winchester (ST-506)** | 1224-1235, 1732-1742 |
| **SCSI disk** | 2210-2225 |
| **SCSI optical** | 2232-2235 |
| **SCSI tape** | 2206-2207, 2226-2231 |
| **SCSI adaptor** | 2202-2205 |
| **Line printers** | 5, 15, 1014-1015, 1167-1176, 1343-1346 |
| **Synchronous modems** | 6, 16, 26-27, 30-31 |
| **HDLC DMA** | 700-763, 1360-1373 |
| **NORCOM** | 700-777 (shared with HDLC) |
| **Ethernet** | 2240-2243 |
| **CAMAC** | 400-420 |
| **Process I/O** | 400-426, 430-443 |
| **CONNECT devices** | 450-467 |
| **User semaphores** | 300-377 |
| **Directory semaphores** | 2500-2637 |
| **Spooling** | 527, 532-537, 562, 604-677, 1136-1144, 1173-1174, 1262-1271, 1347-1351, 1722-1727, 2130-2167 |
| **Batch processes** | 1236-1261, 3100-3147, 3300-3335 |
| **Internal devices** | 200-277, 1210-1221, 1272-1276 |
| **SIBAS** | 240-267, 1750-1777 |
| **PIOC** | 1700-1717 |
| **BDIO pools** | 2260-2277 |
| **Octobus** | 2400-2477 |
| **GPIB** | 2120-2127 |
| **Versatec** | 22-23, 576-577, 1125-1126 |
| **Open files** | 100-177, 3200-3277 |
| **User-defined** | 2300-2377 |

---

**Source**: ND-860228-2-EN "SINTRAN III Monitor Calls", Appendix B, pages 585-622
