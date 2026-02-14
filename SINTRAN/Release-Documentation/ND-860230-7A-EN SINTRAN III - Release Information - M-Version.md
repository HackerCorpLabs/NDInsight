## Page 1

# SINTRAN III

## Release Information, M-version

ND-860230EN7A

---

![NorskData Logo](ND-Logo.png)

Scanned by Jonny Oddene for Sintran Data © 2021.

---

## Page 2

I'm sorry, but the document appears to be blank. Could you provide a different image or check the file again?

---

## Page 3

# SINTRAN III

## Release Information, M-version

*ND-860230EN7A*

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 4

# Norsk Data A.S Manual

The information in this manual is subject to change without notice.  
Norsk Data A.S assumes no responsibility for any errors that may appear in this manual,  
or for the use or reliability of its software on equipment that is not furnished or supplied by  
Norsk Data A.S.

## Copyright Information

Copyright © 1990 by Norsk Data A.S  

| Version  | Date          |
|----------|---------------|
| Version 1 | January 1985  |
| Version 2 | June 1986     |
| Version 3 | May 1987      |
| Version 4 | November 1987 |
| Version 5 | May 1988      |
| Version 6 | September 1988|
| Version 7 | January 1990  |
| Version 7A| December 1990 |

## Documentation Requests

Send all documentation requests to:

Norsk Data A.S  
P.O. Box 25 - Bogerud  
N-0621 Oslo 6, Norway  

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 5

# Preface

| S I N T R A N I I I / V S X |
|-----------------------------|
| M-version                   |

Keywords for SINTRAN III M-version:

- Support of new hardware
- Increased performance in the file system
- Increased performance in the ND-500/5000 Swapper
- Improved error logging
- Enhanced security functions

This manual describes the changes in the M-version of SINTRAN III/VSX compared to the L-version.

The current revision of the manual is updated to reflect generation 6 of the M-version of SINTRAN III/VSX. Changes from the original version (generation 5) are marked with a change bar.

*Scanned by Jonny Oddene for Sintran Data © 2021*

---

## Page 6

# Table of Contents

## 1. Installation

1.1 Hardware requirements for SINTRAN III/VSX version M ....................... 1  
1.2 Software requirements for SINTRAN III/VSX version M ....................... 1  
1.3 Microprogram versions for ND-500/5000 ......................................... 2  
1.4 New hardware supported .................................................................. 2  
1.5 Changes in configuration limitations ................................................. 2  
1.6 Configuration .................................................................................. 3  
1.7 Changes in installation procedure ....................................................... 4  
1.8 Changes to HENT-MODE / LOAD-MODE - and other mode files .......... 5  
1.9 Changes to the New-System program ............................................... 6  
1.10 Example of installation of SINTRAN III/VSX ....................................... 6  

## 2. SINTRAN III Commands

2.1 Commands removed ....................................................................... 10  
2.1.1 @COPY-DIRECTORY ..................................................................... 10  
2.2 Modified commands ......................................................................... 10  
2.2.1 @APPEND-SPOOLING-FILE ......................................................... 10  
2.2.2 @DEVICE-FUNCTION .................................................................. 10  
2.2.3 @DEFINE-REENTRANT-PROGRAM ........................................... 10  
2.2.4 @DUMP-PROGRAM-REENTRANT ................................................. 10  
2.2.5 @DUMP-REENTRANT .................................................................... 10  
2.2.6 @ENTER ................................................................................... 11  
2.2.7 @LIST-BATCH-PROCESS ............................................................ 11  
2.2.8 @LIST-BATCH-QUEUE .................................................................. 11  
2.2.9 @LIST-DEVICE ........................................................................... 11  
2.2.10 @LIST-EXECUTION-QUEUE ....................................................... 11  
2.2.11 @LIST-REENTRANT ................................................................... 11  
2.2.12 @LIST-REMOTE-QUEUE ............................................................. 11  
2.2.13 @LIST-RT-ACCOUNT .................................................................. 12  
2.2.14 @LIST-RT-DESCRIPTION .......................................................... 12  
2.2.15 @LIST-SEGMENT ....................................................................... 12  
2.2.16 @LIST-SPOOLING-FORM ........................................................... 12  
2.2.17 @LIST-TIME-QUEUE .................................................................. 12  
2.2.18 @MAIL ...................................................................................... 12  
2.2.19 @RECOVER ............................................................................... 13  
2.2.20 @TERMINAL-STATUS ................................................................. 13  
2.2.21 @WHO-IS-ON .............................................................................. 13  
2.3 New commands ............................................................................... 14  
2.3.1 @EXPAND-DIRECTORY ............................................................... 14

---

## Page 7

# 3. Monitor Calls (ND-100)

| Section | Description | Page |
|---------|-------------|------|
| 3.1 | Modified monitor calls | 15 |
| 3.1.1 | SETCM MON 12 | 15 |
| 3.1.2 | N500M MON 60 | 15 |
| 3.1.3 | COMND MON 70 | 15 |
| 3.1.4 | APSPF MON 240 | 15 |
| 3.1.5 | UECOM MON 317 | 15 |
| 3.1.6 | FSMTY MON 327 | 15 |
| 3.1.7 | TERST MON 330 | 17 |
| 3.1.8 | IOMTY MON 336 | 17 |
| 3.2 | New monitor calls | 18 |
| 3.2.1 | IOPEN MON 351 | 18 |
| 3.2.2 | EVENT MON 352 | 20 |

# 4. SINTRAN Service Program

| Section | Description | Page |
|---------|-------------|------|
| 4.1 | Modified commands | 24 |
| 4.1.1 | *BACKGROUND-ALLOCATION-UTILITIES | 24 |
| 4.1.2 | *CHANGE-DATAFIELD | 24 |
| 4.1.3 | *CHANGE-VARIABLE | 24 |
| 4.1.4 | *ST-TIME-SLICED-PROGRAMS | 24 |
| 4.2 | New commands | 25 |
| 4.2.1 | *DUMP-DATAFIELD | 25 |
| 4.2.2 | *FILE-SYSTEM-EVENT-LOG | 25 |

# 5. ND-500/5000 Monitor

| Section | Description | Page |
|---------|-------------|------|
| 5.1 | Installation procedure | 27 |
| 5.2 | Configuration limitations | 27 |
| 5.3 | Modified commands to SINTRAN III affecting the ND-500/5000 | 27 |
| 5.3.1 | @ENTER | 27 |
| 5.3.2 | @RECOVER | 27 |
| 5.3.3 | @TERMINAL-STATUS | 28 |
| 5.4 | Modified commands to the ND-500/5000 background monitor | 28 |
| 5.4.1 | DEFINE-STANDARD-DOMAIN | 28 |
| 5.4.2 | LOOK-AT-HARDWARE | 28 |
| 5.5 | Modified monitor calls to the ND-100 affecting ND-500/5000 | 28 |
| 5.5.1 | N500M MON 60 | 28 |
| 5.6 | Modified monitor calls (ND-500/5000) | 28 |
| 5.6.1 | SETCM MON 12 | 28 |
| 5.6.2 | COMND MON 70 | 28 |
| 5.6.3 | APSPF MON 240 | 28 |
| 5.6.4 | UECOM MON 317 | 28 |
| 5.6.5 | FSMTY MON 327 | 29 |
| 5.6.6 | TERST MON 330 | 30 |
| 5.6.7 | IOMTY MON 336 | 30 |

---

## Page 8

# 5.7 New monitor calls (ND-500/5000)
5.7.1 IOPEN MON 351 ........................................................ 31  
5.7.2 EVENT MON 352 ........................................................ 32  

# 5.8 Modified monitor calls - available only on ND-500/5000
5.8.1 FIXMEM MON 410 ..................................................... 36  
5.8.2 5MTRANS MON 515 .................................................. 37  

# 5.9 New monitor calls - available only on ND-500/5000
5.9.1 PLACE MON 441 ...................................................... 39  

# 6. File System ................................................................. 40
6.1 Changes in configuration limitations .......................... 40  
6.2 Performance ............................................................... 40  
6.3 File system event-log utility ........................................ 40  
6.3.1 Reports from the file system event-log .................. 40  
6.3.2 File system operations available for logging ........ 41  
6.3.3 Operation of the file system event-log .................. 41  

# 7. Spooling ................................................................. 42

# 8. XMSG ........................................................................ 43
8.1 Changes in configuration limitations ......................... 43  
8.2 Changed error handling ............................................ 43  
8.3 Modified function ....................................................... 43  
8.3.1 XFOPN .................................................................... 43  

# 9. The Event System .................................................... 44
9.1 Introduction ................................................................. 44  
9.2 Event buffer ................................................................. 44  
9.3 Event agreement ......................................................... 44  
9.4 The functions of the event system ............................ 44  

# 10. Security Primitives .................................................... 46
10.1 File system event log ................................................. 46  
10.2 Improved remote file server ...................................... 46  

# 11. SINTRAN III Mail System .......................................... 47

# 12. New Error Messages .................................................. 48
12.1 SINTRAN III run-time errors ...................................... 48  
12.2 Error codes returned from monitor calls - numeric list ........ 48  
12.3 Error codes returned from monitor calls - alphabetic list ...... 48  
12.4 Error codes returned from the ND-5850 service partner (James) ... 49  

# 13. Logical Device Numbers ............................................ 50

# 14. Terminal Input/Output ................................................ 55

---

## Page 9

# 15. Nucleus
... 56

# 16. ERS/SINTRAN III Watchdog
... 57

## 16.1 Introduction
... 57

## 16.2 General concepts
... 57

## 16.3 The watchdog program
... 57

## 16.4 The log-list program
... 58

## 16.5 The manager program
... 58

## 16.6 The error message descriptor file
... 59

## 16.7 Expanding the log file
... 59

## 16.8 Increasing the buffer size of the internal device
... 59

## 16.9 Report format
... 60

## 16.10 Suppression
... 60

## 16.11 Messages from the watchdog itself
... 61

## 16.12 Reporters recognised by the SINTRAN III watchdog
... 64

# 17. SINTRAN III M-version, System Layout
... 65

## 17.1 System layout on disk
... 65

## 17.2 Page index table layout
... 66

## 17.3 System included segments
... 68

## 17.4 System included RT-programs
... 70

## 17.5 Changes to the RT-description
... 72

## 17.6 Changed data fields - terminals / TAD / NOTS / MTAD
... 73

### 17.6.1 Terminal data field - DPIT part - SINTRAN memory area
... 73

### 17.6.2 Terminal input data field - non-DPIT part - memory
... 74

### 17.6.3 Terminal output data field - non-DPIT part - memory
... 76

### 17.6.4 TAD input data field - non-DPIT part - memory
... 77

### 17.6.5 TAD output data field - non-DPIT part - memory
... 79

### 17.6.6 NOTS input data field - non-DPIT part - memory
... 80

### 17.6.7 NOTS output data field - non-DPIT part - memory
... 82

### 17.6.8 MTAD input data field - non-DPIT part - memory
... 83

### 17.6.9 MTAD output data field - non-DPIT part - memory
... 85

# 18. Affected Subsystems
... 86

---

## Page 10

## Contents - Release Information M-version

| Description         | Section |
|---------------------|---------|
| Introduction        | 1       |
| New Features        | 2       |
| Improvements        | 3       |
| Bug Fixes           | 4       |
| Known Issues        | 5       |

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 11

# Installation

## 1.1 Hardware requirements for SINTRAN III/VSX version M

ND-100 CPU, one of:
- ND-100/CX CPU with ECO 100-522 (48-bit floating representation)  
  or ECO 100-523 (32-bit floating representation)  
  and Memory management II (16 PITs) with ECO 100-534 (level N)
- ND-110 CPU (CPU and memory management) print no. 3090 (level P)
- ND-110 CPU (CPU and memory management) print no. 3095 (level H)
- ND-120/CX CPU (CPU, memory management and memory on one card) (level K)

If SMD disk controller (10 MHz) is used, the following applies:
- SMD Control (print 3018) ECO level R is required  
- SMD Data (print 3019) ECO level BE is required

If Dual Disk Channel Switch is present, ECO level J is required.

If NUCLEUS is to be run, one of the following is necessary:
- ND-5000
- ND-500/II with ND-100 Octobus Line Driver (ND 324133, level D)  
  (or ND 324118, level G)

If DOMINO controllers are used, the following requirements apply:
- MFB/SCSI (print 5467, level A)
- ND-5000
- either - MF-bus controller (ND 324245, ECO level C)  
  or - Double-bus controller (ND 324244, ECO level E)
- either - PROM for MF-bus controller (47800, ECO level E)  
  or - PROM for Double-bus controller (47500, ECO level D)
- either - MFB port (ND 350161, ECO level F)  
  or - MPM-5 port (ND 324355, ECO level G)
- DOMINO PROM (73100, ECO level C)

## 1.2 Software requirements for SINTRAN III/VSX version M

SINTRAN III/VSX version M, generation 6 requires revision level (patch file level) 4000 or higher.

---

## Page 12

# 1.3 Microprogram versions for ND-500/5000

The following table shows the microprogram versions required to run ND-500 and ND-5000 systems on the M-version of SINTRAN III:

| ND prod.no. | System type              | Microprogram version |
|-------------|--------------------------|----------------------|
| 210332 J    | ND-500 series I, standard| 10512                |
| 210338 I    | ND-500 series I, AX-CPU  | 10412                |
| 210411 G    | ND-500 series I, CX-CPU  | 10312                |
| 210412 G    | ND-500 series I, CXA-CPU | 10612                |
| 210787 F    | ND-530                   | 15313                |
| 210786 F    | ND-550/560/570           | 15213                |
| 210788 F    | ND-550/560/570, > 32 Mb  | 15413                |
| 210701 H    | ND-580                   | 15113                |
| 211272 E    | ND-5200                  | 11531                |
| 211273 E    | ND-5400                  | 11631                |
| 211274 E    | ND-5500                  | 11731                |
| 211275 E    | ND-5700                  | 11831                |
| 211276 E    | ND-5800                  | 11931                |
| 211847 A    | ND-5830/5850             | 12009                |

# 1.4 New hardware supported

- All ND-500/5000 systems, including the new ND-5830 and ND-5850 systems, are supported by the M-version of SINTRAN III.
- SCSI magnetic tape drives with several logical units on each controller are supported by the M-version of SINTRAN III.

# 1.5 Changes in configuration limitations

- The maximum number of device buffers has been increased from 64 to 128.
- The maximum number of entries in the reentrant-subsystem table (ND-100 reentrant subsystems and ND-500/5000 standard domains) has been increased to 400 (maximum 100 ND-500/5000 standard domains). The total length of names of all entries in the table has been increased to 4096 characters (including the apostrophe terminating each name).
- The previous limitation of memory size to 32 megabytes was changed to 128 megabytes on ND-5000 systems in the L-version of SINTRAN III. This new limit (128 megabytes) now applies to all ND-500/5000 systems.
- The length of the command buffer has been changed from 104 characters to 150 characters.
- The maximum number of BDIO pools supported by SINTRAN III has been increased to 64 in generation 6.

---

## Page 13

# 1.6 Configuration

The M-version of SINTRAN III/VSX is delivered as a limited number of standard versions able to support a great variety of configurations. As for the K and L versions, a program for handling reconfiguration is supplied.

A list of options included in the SINTRAN III/VSX version M standard systems A, B and C is given below (standard system C is only available in generation 6):

| | A | B | C |
|---|---|---|---|
| SMD/ECC disk controllers (max. 4 units/each): | 2 | 4 | 2 |
| ST-506 (Winchester) disk (max. 2 units/each): | 1 | 2 | 1 |
| SCSI host adaptor (controller): | 3 | 2 | 1 |
| SCSI disk units (per system): | 8 | 8 | 2 |
| SCSI streamer units (per system): | 2 | 2 | 2 |
| SCSI magnetic tape units (per system): | 3 | 2 | 2 |
| SCSI optical disk units (per system): | 2 | 1 | 2 |
| Bootstrap driver for SMD disk controller: | Yes | Yes | Yes |
| Bootstrap driver for Winchester disk controller: | Yes | Yes | Yes |
| Bootstrap driver for SCSI disk controller: | Yes | Yes | Yes |
| Floppy/streamer controllers (maximum 3 units/each): | 2 | 2 | 2 |
| (both types of floppy drives supported) | | | |
| Magnetic tape controllers (maximum 4 units/each): | 2 | 2 | 2 |
| (Cipher, Pertec, STC) | | | |
| Terminals: | 135 | 125 | 172 |
| Line printers: | | | |
| Parallel or DMA interfaces: | 2 | 2 | 2 |
| Versatec printer/plotter DMA: | 2 | 2 | 2 |
| Versatec printer/plotter I/O: | 2 | 2 | 2 |
| Extra spooling processes: | 16 | 10 | 16 |
| COSMOS spooling: | Yes | Yes | Yes |
| Communication: | | | |
| HDLC + synchronous modem (total): | 6 | 12 | 2 |
| HDLC interfaces (reserved for HDLC): | 0 | 6 | 0 |
| Synchronous modem interface: | 2 | 2 | 2 |
| PIOC interfaces: | 4 | 4 | 2 |
| GPIB interface: | 1 | 1 | 1 |
| MPM IV option: | Yes | Yes | Yes |
| I/O bus extensions: | 2 | 2 | 2 |
| X.21 interfaces: | 2 | 2 | 1 |
| X.25 option: | Yes | Yes | Yes |
| X.29 option: | Yes | Yes | Yes |
| CAMAC: | 0 | 16 | 0 |
| Universal DMA / Vicom interfaces: | 2 | 6 | 2 |
| Fast UDMA on ND-500/5000: | Yes | Yes | Yes |
| Ethernet interfaces: | 3 | 3 | 3 |
| TELEFIX: | 1 | 1 | 1 |
| HASP DMA interface: | 1 | 1 | 1 |
| Net/One controllers: | 3 | 1 | 1 |
| Support for WPX IPS Bridge: | Yes | No | Yes |

---

## Page 14

# Software Options

| | A | B | C |
|---|---|---|---|
| Terminal/TAD background tasks | 159 | 120 | 200 |
| Terminal Access Devices (TADs) | 70 | 50 | 70 |
| Batch processes | 10 | 10 | 10 |
| Segments | 500 | 750 | 500 |
| Free RT-descriptions for users | 180 | 150 | 180 |
| ND-500/5000 processes | 134 | 128 | 190 |
| SIBAS processes (SIBAS F) | 12 | 12 | 12 |
| Semaphores | 50 | 50 | 50 |
| Internal device (byte-oriented) | 30 | 30 | 30 |
| Internal device (block-oriented) | 2 | 2 | 2 |
| CX-CPU | Yes | Yes | Yes |
| ND-500/5000 | Yes | Yes | Yes |
| ND-500 CPUs | 4 | 4 | 4 |
| ND-5000 CPUs | 4 | 4 | 4 |
| XMSG | Yes | Yes | Yes |
| Device buffers | 128 | 125 | 128 |
| Symbolic Debugger tasks | 32 | 8 | 32 |
| Remote file access segments | 50 | 32 | 50 |
| CONNECT-TO | Yes | Yes | Yes |
| RT and I/O accounting | Yes | Yes | Yes |
| Remote Job Entry queues | All | All | All |
| Logging facilities | All | All | All |
| RT-Common | 6 | 6 | 6 |
| TPS | 1 | 1 | 1 |
| LAMU | Yes | Yes | Yes |
| MON ADP | Yes | Yes | Yes |
| MON 5MTRANS | Yes | Yes | Yes |
| Background allocation | Yes | Yes | Yes |
| Read segment | Yes | Yes | Yes |
| Disk optimisation | Yes | Yes | Yes |
| Direct task | No | Yes | No |
| RT-programs from direct task | 0 | 25 | 0 |
| Magnetic Tape from direct task | No | Yes | No |
| Direct transfer on magnetic tape | Yes | Yes | Yes |
| Connect data fields | 2 | 16 | 2 |
| Fault Tolerant eXtension (FTX) | Yes | Yes | Yes |
| Disk Mirroring clusters | 8 | 8 | 1 |
| Paper-tape punch | Yes | Yes | Yes |
| Allocated areas | 112 | 112 | 112 |
| Programmable RT-clock driver | No | Yes | No |
| Standard bootstrap drivers | Yes | Yes | Yes |
| BDIO pool data fields | 16 | 16 | 40 |

## 1.7 Changes in installation procedure

- SINTRAN III/VSX version M will be delivered on 3 double-sided/double-density (8" or 5¼") diskettes.
- Just as in the K and L versions, the ND-500/5000 System Monitor is installed as part of SINTRAN.
- Furthermore, the SINTRAN III Watchdog is also installed as part of SINTRAN.

---

## Page 15

# 1.8 Changes to HENT-MODE / LOAD-MODE - and other mode files

- If you have a mode file to be run after an installation of SINTRAN III from diskettes, remove any commands used to initialise XMSG to your system, especially if you move directly from the K-version to the M-version of SINTRAN III.

- If you run private patches in any of the mode files, these patches must be checked carefully (and possibly modified) to run on the M-version of SINTRAN III.

- The following changes must be made to the mode file to be run after a cold start (usually called HENT-MODE:MODE):
  - Ensure that any abbreviations of the ENTER-DIRECTORY command are non-ambiguous (@EN-D is now the shortest possible abbreviation).
  - If COSMOS Basic Module version G is installed, replace loading of COSMOS Basic Module by new files loading version G.
  - If User Environment version D is installed, replace loading of User Environment by new files loading version D.
  - If WPX IPS Bridge is installed, remove commands used for patching of SINTRAN III (required on the L-version and earlier). The following commands should be removed for the M-version:

    ```
    @MODE (COMMTAD)COMMTAD-PTCH-A:MODE (COMMTAD)COMMTAD-PTCH-A:LIST
    @(COMMTAD)COMMTAD-INST-A CHECK-PATCH
    ```

- The following changes must be made to the batch file to be run after a warm start (usually called LOAD-MODE:MODE):
  - Ensure that any abbreviations of the ENTER-DIRECTORY command are non-ambiguous (@EN-D is now the shortest possible abbreviation).
  - Make certain that you use the correct version of the XMSG-Command program in your XMSG-START file (the N- or O-version). Note that XMSG-Command and the XMSG-STARTEX files are copied to user area SYSTEM during installation.

- If you have a mode file used for stopping the system in a controlled way, you should include commands to close the log file for the SINTRAN III Watchdog:
  - Start the SINTRAN III Watchdog Manager Program.
  - Use the command: SELECT-WRITE-PARAMETERS Yes No
  - Then EXIT from the Manager program.

A further description of closing and (re-)opening the log file is found on page 59.

Furthermore, in the same mode file, you may want to ensure that any contents of the disk cache are written to disk before stopping the system. To force the disk cache to be written, include the command @RELEASE-DIRECTORY for each disk on your system. You can also achieve this by including a @RENAME-USER command (for example @RENAME-USER,FLOPPY-USER,FLOPPY-USER).

---

## Page 16

# 1.9 Changes to the New-System program

- The command LIST-IMPLEMENTED-PATCHES now asks which area to consider.  
  Possible values:  
  M : current running SINTRAN  
  I : image area (to be activated by a warm start)  
  S : save area (to be activated by a cold start)  

- When copying SINTRAN III from floppy disk, NEW-SYSTEM also copies the ND-500/5000 System Monitor. In the M-version, there are different versions for ND-500 and ND-5000 systems, and NEW-SYSTEM will decide which version to copy, based on the type of system.  
  This may cause problems in a few cases, for example, if you install SINTRAN on a removable disk and move the disk to another system, the System Monitor may be wrong for this system.

- When copying SINTRAN III from floppy disk, NEW-SYSTEM also copies the auxiliary files of the SINTRAN III Watchdog (version D). The files copied are:

| File                  | Description                                     |
|-----------------------|-------------------------------------------------|
| ER-S3WD-DESC-D:EDAT   | ERS/SINTRAN Watchdog Descriptor file            |
| ER-S3WD-LOG-D:PROG    | ERS/SINTRAN Watchdog Log-list program           |
| ER-S3WD-MANA-D:PROG   | ERS/SINTRAN Watchdog Manager program            |

All files will be copied to user area SYSTEM. The descriptor file must reside either on SYSTEM or user area ND-OPERATIONS. The two program files can reside on any user area.

If two revisions of the description file are found, one on user area SYSTEM and the other on ND-OPERATIONS, the one on ND-OPERATIONS is used.

Previous versions of the descriptor file (ER-S3WD-DESC-Cxx:EDAT) and the log-list program (ER-S3WD-LOG-Cxx:PROG) are not used, and should be deleted.

# 1.10 Example of installation of SINTRAN III/VSX

This example assumes you are upgrading your system from the L-version.  
For brevity, it is also assumed that you are installing all products from double-density/double-sided diskettes.

A more detailed installation description is given in the product description for:
- SINTRAN III/VSX, version M
- SINTRAN III Configuration program
- ND-500/5000 System Package for SINTRAN III version M
- ND-500/5000 Microprogram

- First, ensure that you have the correct versions of all products you need:
  - SINTRAN III/VSX version M with patch file diskette
  - SINTRAN III Configuration program - version E or later
  - ND-500/5000 System Package (ND-500/5000 systems) version C
  - ND-500/5000 Microprogram (ND-500/5000 systems, only)

- Then give the commands: `@DIRECTORY-STATISTICS,,,`  
  and: `@LIST-TITLE _`  

Write down the following information:
- the device name, unit number and subunit (if any) of the directory marked as "(MAIN AND DEFAULT DIRECTORY)"
- the CPU number and CPU type of your system.

---

## Page 17

# SINTRAN III Release Information, M-version

- Finally, run the old version of S3-CONFIG and select the PRINT option to get a print-out of your previous configuration. This list may be helpful when you set the correct configuration on your new system.

- Stop the system in a controlled way as described in the SINTRAN III System Supervisor manual.

- You may at this point choose to install any new versions of software required and update the files to be run after a cold and warm start, or you may choose to do this at a later stage. In this example, we have chosen to wait.

- Press the STOP and MCL buttons on the front panel.

- Insert SINTRAN III diskette number 1 in FLOPPY-DISC-1 unit 0.

- Give the command: 1560& (without pressing a ⇑).

- You will then get a list of disk types and you are asked to give the disk type of your system disk. Find the disk type corresponding to the device name you noted earlier and give the type as the number of the disk type in the list.

- Wait until you get the message "TYPE ANY MACM COMMAND".

- Type the command: 10,0$ (without pressing a ⇑).

- Wait until you get the message "**** 000000 DIAGNOSTICS ****".

- Remove SINTRAN III diskette number 1 from FLOPPY-DISC-1 unit 0.

- Insert SINTRAN III diskette number 2 in FLOPPY-DISC-1 unit 0.

- Type the command: 10,0$ (without pressing a ⇑).

- Wait until you get the message "**** 000000 DIAGNOSTICS ****".

- Type the command 22! (without pressing a ⇑).

- Wait until you get the message "PAGES FOR SWAPPING (OCT:) xxxxx".

- You must now enter the main directory of your system:
  - Log in without giving a user area name:
    - Press ESC
    - After "ENTER" press ⇑
    - After "PASSWORD" press ⇑

  - Then give the command: @ENTER-DIRECTORY ⇑
  - Answer the questions for device name, unit (and subunit) with the information you noted earlier about your main directory.

  - Log out: @LOGOUT ⇑

  - Log in as user area SYSTEM:
    - Press ESC
    - ENTER SYSTEM ⇑
    - PASSWORD: <your SYSTEM password> ⇑

- Remove SINTRAN III diskette number 2 from FLOPPY-DISC-1 unit 0.

- Insert SINTRAN III diskette number 3 in FLOPPY-DISC-1 unit 0.

- Give the command: @ENTER-DIRECTORY,,FLOPPY-DISC-1,0 ⇑

- Run the NEW-SYSTEM program: @(2:)NEW-SYSTEM ⇑

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 18

# SINTRAN III Release Information, M-version

- Answer the questions for CPU number and CPU type with the information you noted earlier.
- When asked if you want to run the patch file, answer Y(es).
- Remove SINTRAN III diskette number 3 from FLOPPY-DISC-1 unit 0.
- Insert the Patch file diskette in FLOPPY-DISC-1 unit 0.
- Then answer Y(es) for "ready to continue".
- When asked to do a cold start to set the patches into effect, do the following:
  - Remove the Patch file diskette from FLOPPY-DISC-1 unit 0.
  
## Installing SINTRAN III Configuration Program

- Insert the diskette containing the SINTRAN III Configuration program (ND 211024) in FLOPPY-DISC-1 unit 0.
- Give the command: `@ENTER-DIRECTORY,,FLOPPY-DISC-1,0`
- Delete any old version of the configuration program and copy the program to disk:
  - `@DELETE-FILE S3-CONFIG:PROG`
  - `@COPY-FILE "S3-CONFIG-E:PROG" (211024:F-U)S3-CONFIG-E:PROG`
- If your system includes Net/One, you should install the NOTS-Service program delivered on the same diskette:
  - Delete any old version of the NOTS-Service program and copy the program to disk:
    - `@DELETE-FILE NOTS-SERVICE:PROG`
    - `@COPY-FILE "NOTS-SERVICE-C:PROG" (211024:FL)"NOTS-SERV:PROG"`
  - Give the command: `@RELEASE-DIRECTORY 211024`
  - Remove the diskette containing the SINTRAN III Configuration program (ND 211024) from FLOPPY-DISC-1 unit 0.

## Running SINTRAN III Configuration Program

- Run the SINTRAN III Configuration program to update SINTRAN III according to your configuration: `@S3-CONFIG,GENERATE`
- If you want to change the configuration of your system, run the configuration program: `@S3-CONFIG` and change the appropriate parameters.
- Give the command: `@COLD-START`
- Wait until you get the message "PAGES FOR SWAPPING (OCT:) xxxxx".
- You must now (again) enter the main directory of your system:
  - Log in without giving a user area name:
    - Press ESC
    - After "ENTER" press 
    - After "PASSWORD" press 

- Then give the command: `@ENTER-DIRECTORY` and answer the questions for device name, unit (and subunit) with the information you noted about your main directory.
- Log out: `@LOGOUT`

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 19

# SINTRAN III Release Information, M-version

## Instructions

- **Log in as user area SYSTEM:**
  - Press ESC
  - ENTER SYSTEM ↵
  - PASSWORD: `<your SYSTEM password>` ↵

- The following points (until "Run the mode file HENT-MODE:MODE" below) only concern ND-500 and ND-5000 systems, and should be ignored for ND-100 installations.

## Installation of ND-500/5000 System Package (Version M)

- Insert the diskette containing the ND-500/5000 System Package for version M (ND 211305) in FLOPPY-DISC-1 unit 0.

- **Give the command:**
  ```
  @ENTER-DIRECTORY,,FLOPPY-DISC-1,0 ↵
  ```

- **Delete any old version of the ND-500/5000 Background Monitor and copy the new version to disk:**
  ```
  @DELETE-FILE ND-500-MON:PROG ↵
  @COPY-FILE "ND-500-MON-K:PROG" (211305:FL)ND-500-MON-K:PROG ↵
  ```

- **Delete any old version of the ND-500/5000 Swapper and copy the new version to disk:**
  ```
  @DELETE-FILE SWAPPER:PSEG ↵
  @DELETE-FILE SWAPPER:DSEG ↵
  @COPY-FILE "SWAPPER-L:PSEG" (211305:F-U)SWAPPER-L:PSEG ↵
  @COPY-FILE "SWAPPER-L:DSEG" (211305:F-U)SWAPPER-L:DSEG ↵
  ```

- **Give the command:**
  ```
  @RELEASE-DIRECTORY 211305 ↵
  ```

- Remove the diskette containing the ND-500/5000 System Package from FLOPPY-DISC-1 unit 0.

## Installation of Microprogram for ND-500/5000 System

- Insert the diskette containing the ND-500/5000 microprogram for the type of ND-500/5000 system you have in FLOPPY-DISC-1 unit 0.

- **Give the command:**
  ```
  @ENTER-DIRECTORY,,FLOPPY-DISC-1,0 ↵
  ```

- **Copy the new version of the microprogram to disk:**

  - **If you have an ND-500, do as follows:**
    ```
    @COPY-FILE CONTROL-STORE:DATA (21):CONT-STORE:DATA ↵
    ```

  - **If you have an ND-5000, do as follows:**
    ```
    @COPY-FILE CONTROL-STORE:DATA (21):MICRO-5xxx:DATA ↵
    ```
    substituting xxx with 200, 400, 500, 700 or 800 depending on the type of ND-5000 you have (if you have an ND-5900, you use the same microprogram as for ND-5800).

- **Give the command:**
  ```
  @RELEASE-DIRECTORY 21 ↵
  ```

- Remove the diskette containing the ND-500/5000 microprogram from FLOPPY-DISC-1 unit 0.

- **Run the mode file HENT-MODE:MODE (to be run after a cold start):**
  ```
  @MODE HENT-MODE:MODE,, ↵
  ```

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 20

# 2. SINTRAN III Commands

## 2.1 Commands removed

### 2.1.1 @COPY-DIRECTORY

Use the MULTI-USER-COPY function in the Backup System instead.

## 2.2 Modified commands

### 2.2.1 @APPEND-SPOOLING-FILE

The maximum length of parameter number 4 `<text>` has been changed from 128 characters to 80 characters.

### 2.2.2 @DEVICE-FUNCTION

One function, FORMAT-FLOPPY, has been modified and one new function, FORMAT-TRACK, has been introduced.

When FORMAT-FLOPPY is used to format floppy disks after selecting floppy format no. 14₈, the last 6 tracks on the floppy disk are now formatted along with the rest of the floppy disk.

The new function FORMAT-TRACK is introduced to format a single track on a floppy disk. This function has one parameter, `<sector address>`, which must be at the start of a track. FORMAT-TRACK will format the track specified according to the floppy format selected, which must be the same as previously used to format the rest of the floppy disk.

### 2.2.3 @DEFINE-REENTRANT-PROGRAM

The maximum number of entries in the reentrant-subsystem table (ND-100 reentrant subsystems and ND-500/5000 standard domains) has been increased to 400. The total length of names of all entries in the table has been increased to 4096 characters (including the apostrophe terminating each name).

### 2.2.4 @DUMP-PROGRAM-REENTRANT

The maximum number of entries in the reentrant-subsystem table (ND-100 reentrant subsystems and ND-500/5000 standard domains) has been increased to 400. The total length of names of all entries in the table has been increased to 4096 characters (including the apostrophe terminating each name).

### 2.2.5 @DUMP-REENTRANT

The maximum number of entries in the reentrant-subsystem table (ND-100 reentrant subsystems and ND-500/5000 standard domains) has been increased to 400. The total length of names of all entries in the table has been increased to 4096 characters (including the apostrophe terminating each name).

---

## Page 21

# 2.2.6 @ENTER

The fourth (last) parameter, `<maximum time>`, is now a combined limit of ND-100 and ND-500/5000 CPU time. This means that the total CPU time used by a batch job (as the sum of ND-100 and ND-500/5000 CPU time) may not exceed this limit.

# 2.2.7 @LIST-BATCH-PROCESS

One optional parameter, `<output file>`, has been introduced.  
The parameter syntax is thus:

| Parameter       | Description                                       |
|-----------------|---------------------------------------------------|
| `<output file>` | output file to receive the list (default = terminal). |

# 2.2.8 @LIST-BATCH-QUEUE

One new optional parameter, `<output file>`, has been introduced.  
The parameter syntax is thus:

| Parameter          | Description                                       |
|--------------------|----------------------------------------------------|
| `<batch number>`   | batch process number.                             |
| `<output file>`    | output file to receive the list (default = terminal). |

# 2.2.9 @LIST-DEVICE

One new optional parameter, `<output file>`, has been introduced.  
The parameter syntax is thus:

| Parameter                  | Description                                                       |
|----------------------------|-------------------------------------------------------------------|
| `<logical device number>`  | logical device number of the device (decimal value).             |
| `<input/output>`           | 0 = input part 1 = output part                                    |
| `<output file>`            | output file to receive the list (default = terminal).            |

# 2.2.10 @LIST-EXECUTION-QUEUE

One optional parameter, `<output file>`, has been introduced.  
The parameter syntax is thus:

| Parameter       | Description                                       |
|-----------------|---------------------------------------------------|
| `<output file>` | output file to receive the list (default = terminal). |

# 2.2.11 @LIST-REENTRANT

One new parameter, `<output file>`, has been introduced and the parameter `<subsystem>` is now required. The parameter syntax is thus:

| Parameter      | Description                                                                 |
|----------------|-----------------------------------------------------------------------------|
| `<subsystem>`  | abbreviated subsystem name (default = all subsystems).                      |
| `<output file>`| output file to receive the list (default = terminal).                       |

# 2.2.12 @LIST-REMOTE-QUEUE

One new optional parameter, `<output file>`, has been introduced.  
The parameter syntax is thus:

| Parameter          | Description                                                              |
|--------------------|--------------------------------------------------------------------------|
| `<remote computer>`| peripheral file name for remote computer (default type = REM).           |
| `<output file>`    | output file to receive the list (default = terminal).                    |

---

## Page 22

# 2.2.13 @LIST-RT-ACCOUNT

One parameter, `<output file>`, has been introduced.  
The parameter syntax is thus:

| Parameter    | Description                                               |
|--------------|-----------------------------------------------------------|
| `<output file>` | Output file to receive the list (default = terminal). |

# 2.2.14 @LIST-RT-DESCRIPTION

One new optional parameter, `<output file>`, has been introduced.  
The parameter syntax is thus:

| Parameter       | Description                                                   |
|-----------------|---------------------------------------------------------------|
| `<RT-program>`  | Symbolic name of RT-program or RT-description address (octal value) |
| `[<output file>]` | Output file to receive the list (default = terminal).         |

# 2.2.15 @LIST-SEGMENT

One new optional parameter, `<output file>`, has been introduced.  
The parameter syntax is thus:

| Parameter      | Description                                                   |
|----------------|---------------------------------------------------------------|
| `<segment>`    | The name or number (octal value) of the segment.              |
| `[<output file>]` | Output file to receive the list (default = terminal).       |

# 2.2.16 @LIST-SPOOLING-FORM

One new parameter, `<output file>`, has been introduced.  
The parameter syntax is thus:

| Parameter             | Description                              |
|-----------------------|------------------------------------------|
| `<peripheral file name>` | A spooling device.                    |
| `<output file>`       | Output file to receive the list (default = terminal). |

# 2.2.17 @LIST-TIME-QUEUE

One optional parameter, `<output file>`, has been introduced.  
The parameter syntax is thus:

| Parameter      | Description                                               |
|----------------|-----------------------------------------------------------|
| `[<output file>]` | Output file to receive the list (default = terminal). |

# 2.2.18 @MAIL

Subcommand DIRECT-BROADCAST:

The contents of the last direct broadcast message are now also written to internal device number 275₈.

The message is formatted just as for terminals, that is, the character `$` is translated to CR LF before writing to the internal device.

---

## Page 23

# 2.2.19 @RECOVER

On an ND-500/5000 system, the RECOVER-command can now be used to start both ND-100 programs (:PROG-files) and ND-500/5000 domains.

This means that the order of commands/files attempted when a name is given without explicit user area and/or file type, is as follows:

1. SINTRAN III commands (if the command name RECOVER is omitted).
2. ND-100 reentrant subsystem and ND-500/5000 standard domains.
3. `<file>:DOM` (if the ND-500/5000 Monitor is a reentrant subsystem) both the current user area and the user area SYSTEM are searched in the following order: current user area first, then user area SYSTEM.
4. `<file>:PROG` both the current user area and the user area SYSTEM are searched in the following order: current user area first, then user area SYSTEM.

Note that this implies that if you have two programs, one for ND-100 and the other for ND-500/5000, with the same name (:DOM and :PROG files with the same file name) and omit the file type, the ND-500/5000 program is started.

# 2.2.20 @TERMINAL-STATUS

One new parameter, `<output file>`, has been introduced.  
The parameter syntax is thus:

| Parameter | Description |
|-----------|-------------|
| `<terminal number>` | logical device number of a terminal (decimal value, default = all active terminals). |
| `<interval>` | interval in seconds between each sample (decimal value, default = only one sample). |
| `<output file>` | output file to receive the list (default = terminal). |

The CPU-time shown is now the sum of ND-100 CPU time and ND-500/5000 CPU time.

# 2.2.21 @WHO-IS-ON

One optional parameter, `<output file>`, has been introduced.  
The parameter syntax is thus:

| Parameter | Description |
|-----------|-------------|
| `[<output file>]` | output file to receive the list (default = terminal). |

---

## Page 24

# 2.3 New commands

## 2.3.1 @EXPAND-DIRECTORY

This command is used to expand a directory located on a disk connected to a SCSI controller or a DOMINO controller.  
It may also be used to reposition the bit-file to another free area within the directory (applies to all types of hard disks).

**Parameters:**

| Parameter              | Description                                                                                                                                                 |
|------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `<directory name>`     | name of an entered directory                                                                                                                                 |
| `<number of pages>`    | number of pages by which the directory is to be expanded (a value of zero means reposition the bit-file).                                                    |
| `<octal bit-file address>` | start address of the bit-file (octal value, default = the file system will select a medium dependent optimal value). The value -1 may be used to place the bit-file at the highest disk addresses within the directory. |

**Rules:**

1. Available only to users logged in as user area SYSTEM.
2. Available only for directories located on disks connected to a SCSI controller or a DOMINO controller if expansion is specified (`<number of pages>` ≠ 0).
   - Available for directories located on all types of hard disks if repositioning of bit-file is specified (`<number of pages>` = 0).
3. Not available for directories located on floppy disks.
4. On disks connected to a DOMINO controller, the following applies:
   - First, the definition of the disk and directory must be removed from the system tables (use the commands `@RELEASE-DIRECTORY` and `@DELETE-MASS-STORAGE-UNIT`).
   - The BDIO service program can then be used to change the directory size on the disk.
   - Last, use the commands `@ENTER-DIRECTORY` and `@EXPAND-DIRECTORY` to reenter the directory in the system tables with the correct size.

---

## Page 25

# 3. Monitor Calls (ND-100)

## 3.1 Modified monitor calls

### 3.1.1 SETCM MON 12

The maximum length of the command buffer has been increased from 104 characters to 150 characters.

### 3.1.2 N500M MON 60

Function 174₈ LOIMM (Load IMAP via Memory).

This is a new function only supported on ND-5830 and ND-5850 systems. It is used by hardware test programs.

### 3.1.3 COMND MON 70

The maximum length of the command buffer has been increased from 104 characters to 150 characters.

### 3.1.4 APSPF MON 240

The maximum length of parameter number 4 (<optional message>) has been decreased from 128 characters to 80 characters.

### 3.1.5 UECOM MON 317

The maximum length of the command buffer has been increased from 104 characters to 150 characters.

### 3.1.6 FSMTY MON 327

Two new functions (13₈ and 14₈) have been introduced. The monitor call format varies slightly for each function:

#### Function no. 13₈:

**Function:**  
Reset the "file-modified"-bit in object entry.

**Monitor call format:**

| Instruction | Description |
| ----------- | ----------- |
| LDT FUNC    | % T = function (13₈) |
| LDA DUIDX   | % A = directory- and user indexes |
| LDX OINDX   | % X = file object index |
| MON 327     | |
| JMP ERROR   | % error return |
| ..........  | % normal return |

| FUNC | DUIDX | OINDX |
| ---- | ----- | ----- |
| 13   | 1     | 2     |

---

## Page 26

# Input parameters:

- **T-register** = function = 13₈
- **A-register** = directory and user indexes:
  - most significant byte = directory index
  - least significant byte = user index
  - (if bit 17₈ is set, the D-register contains a remote system identification)
- **X-register** = file object index
- **D-register** = address of buffer containing remote system identification
  - (this parameter applies only if bit 17₈ of the A-register is set)

# Output parameters:

- **Return**: Error - A-register contains error code.
- **Skip return**: OK.

# Function no. 14₈:

## Function:
Get page number of next page in a file.  
This is especially useful for files containing holes: if the AD-register contains a page number inside the "hole", the first page number after the "hole" is returned.

## Monitor call format:

| Code  | Description                      |
|-------|----------------------------------|
| LDT   | FUNC                             |
|       | % T = function (14₈)             |
| LDX   | OPNFN                            |
|       | % X = open file number           |
| LDD   | SPGNO                            |
|       | % AD = logical page number to start from |
| MON   | 327                              |
| JMP   | ERROR                            |
|       | % error return                   |
|       | % normal return                  |

```
FUNC,   14
OPNFN,  101
SPGNO,  0;50
```

# Input parameters:

- **T-register** = function = 14₈
- **X-register** = open file number
- **AD-register** = logical page number to start from
  - if the given logical page number refers to a page inside a hole, the logical page number of the first page after the hole is returned.
  - if the given logical page number refers to an existing page, the AD-register is not changed.

# Output parameters:

- **Return**: Error - A-register contains error code, except:
  - A-register = 3 : end of file
- **Skip return**: OK, AD-register = logical page number of next page in file (may be the same as input).

Refer to page 29 for a description of these functions used from ND-500/5000 programs.

---

## Page 27

# 3.1.7 TERST MON 330

The CPU-time returned is now the sum of ND-100 CPU time and ND-500/5000 CPU time.

# 3.1.8 IOMTY MON 336

One new function has been introduced:

function 26₈: get protocol ID and MTAD ID from the MTAD data field

### Function no. 26₈:

**Function:**  
Get protocol ID and MTAD ID from the MTAD data field.

### Monitor call format:

- **LDA** (PARLI) % A = address of parameter list
- **MON** 336 % IOMTY
- **JMP** ERROR % error return
- .......... % normal return

**PARLI:**

| | |
|---|---|
| FUNC | % address of function |
| SIZE | % address of the length of the parameter array |
| ARRAY | % address of the parameter array |

**FUNC,**

- 26 % function (26₈)

**SIZE,**

- 6 % length of parameter array

**ARRAY,**

| | |
|---|---|
| 0 | % function parameter 1 (word 1) |
| 0 | % function parameter 2 (word 2) |
| 0 | % function parameter 3 (word 3) |
| 0 | % function parameter 4 (word 4) |
| 0 | % function parameter 5 (word 5) |
| 0 | % function parameter 6 (word 6) |

**Input parameters:**

Word 1 = Logical device number.

**Output parameters:**

Word 2 = Protocol ID.  
Word 3 = MTAD ID 1.  
Word 4 = MTAD ID 2.  
Word 5 = MTAD ID 3.  
Word 6 = MTAD ID 4.

**Protocol ID = 1 (Telnet) implies the following values:**

- MTAD ID 1 : IP-address
- MTAD ID 2 : IP-address
- MTAD ID 3 : 0
- MTAD ID 4 : port

**Rules:**

1. Available to all users.

Refer to page 30 for a description of this function used from ND-500/5000 programs.

---

## Page 28

# 3.2 New monitor calls

## 3.2.1 IOPEN MON 351

Open a file with specific directory, user and file object indexes or return indexes. The input parameters may specify either the directory, user and file object indexes or the file name and file type. If both file name and file system indexes are specified, the file system will verify that the file indexes match the file name and type. A description of MON IOPEN used from ND-500/5000 are found on pages 31-32.

### Monitor call format:

```
LDA  (INDEX
COPY SA DD      % D = address of index list
LDX  (FNAME     % X = address of file name buffer
LDA  (FTYPE     % A = address of default file type buffer
LDT  ACCES      % T = access mode

MON  351        % IOPEN
JMP  ERROR      % error return
..........      % normal return

INDEX,     1    % most significant byte = directory index
                 % least significant byte = user index
            2    % second word = file object index
FNAME,   'EXAMPL:SYMB' % file name (with or without type)
FTYPE,   'DATA'        % default file type used if not specified in
                       % file name parameter
ACCES,   3             % file access mode (see below)
```

### Input parameters:

- **D-register** = address of double word buffer containing the following:  

  | Word    | Byte                  | Description              |
  |---------|-----------------------|--------------------------|
  | 1st     | Most significant byte | Directory index          |
  | 1st     | Least significant byte| User index               |
  | 2nd     |                       | File object index        |

  - If both words are -1, the file name/file type parameters apply and the indexes will be returned.

- **X-register** = address of buffer containing file name and/or file type. 

  - This parameter does not apply if the double word pointed at by the D-register contains indexes described above.

- **A-register** = address of buffer containing default file type.

  - This parameter does not apply if the double word pointed at by the D-register contains indexes described above.
  - The default file type is used only when the file name pointed at by the X-register does not specify file type.
  - The default file type must not contain the leading colon.

- **T-register** = file access mode

The file access modes used by MON IOPEN may specify three different ranges corresponding to access modes used by:

| MON Call     | Range           |
|--------------|-----------------|
| MON OPEN  (MON 50)  | 0 - 11₈          |
| MON SCROP (MON 235) | 40₈-51₈        |
| MON DOPEN (MON 220) | 100₈-111₈      |

---

## Page 29

# Access Modes

The access modes are:

| Code   | Description                                                               |
|--------|---------------------------------------------------------------------------|
| 0      | sequential write.                                                         |
| 1      | sequential read.                                                          |
| 2      | random read or write.                                                     |
| 3      | random read only.                                                         |
| 4      | sequential read or write.                                                 |
| 5      | sequential write append.                                                  |
| 6      | random read or write common on contiguous files.                          |
| 7      | random read common on contiguous files.                                   |
| 10₈    | random read or write on contiguous files.                                 |

Direct transfer for MON RFILE (MON 117), MON WFILE (MON 120) and MON MAGTP (MON 114) in RT-programs.

| Code   | Description                                                               |
|--------|---------------------------------------------------------------------------|
| 11₈    | random read, write append for MON WFILE (MON 120).                        |
| 40₈    | sequential write.                                                         |
| 41₈    | sequential read.                                                          |
| 42₈    | random read or write.                                                     |
| 43₈    | random read only.                                                         |
| 44₈    | sequential read or write.                                                 |
| 45₈    | sequential write append.                                                  |
| 46₈    | random read or write common on contiguous files.                          |
| 47₈    | random read common on contiguous files.                                   |
| 50₈    | random read or write on contiguous files.                                 |

Direct transfer for MON RFILE (MON 117), MON WFILE (MON 120) and MON MAGTP (MON 144) in RT-programs.

| Code   | Description                                                               |
|--------|---------------------------------------------------------------------------|
| 51₈    | random read, write append for MON RFILE (MON 117) and MON WFILE (MON 120).|
| 100₈   | sequential write.                                                         |
| 101₈   | sequential read.                                                          |
| 102₈   | random read or write.                                                     |
| 103₈   | random read only.                                                         |
| 104₈   | sequential read or write.                                                 |
| 105₈   | sequential write append.                                                  |
| 106₈   | random read or write common on contiguous files.                          |
| 107₈   | random read common on contiguous files.                                   |
| 110₈   | random read or write on contiguous files.                                 |

Direct transfer for MON RFILE (MON 117), MON WFILE (MON 120) and MON MAGTP (MON 114) in RT-programs.

| Code   | Description                                                               |
|--------|---------------------------------------------------------------------------|
| 111₈   | random read, write append for MON WFILE (MON 120).                        |

# Output Parameters

- **A-register** = open file number
- **D-register** = address of double word buffer containing the following:

  - 1st word, most significant byte : directory index
  - 1st word, least significant byte : user index
  - 2nd word : file object index

# Rules

1. Available to all users with sufficient access to the file.
2. If both file name and file system indexes are specified, the file system will verify that the file indexes match the file name.

---

## Page 30

# 3.2.2 EVENT MON 352

## General description:
An event system has now been introduced in SINTRAN III. A description of the event system is found on pages 44-45, and the monitor call used in programs running on the ND-500/5000 part of the system is described on pages 32-36.

This event system can handle sets of 32 events for each process (RT-program). The events are kept in an extended RT-description (further description is found on pages 72-73).

The event system is operated through this new monitor call MON EVENT, which has 6 functions:

- 0 : check if specified function is implemented
- 1 : set event(s) on specified process
- 2 : read events on current process
- 3 : wait for events to occur
- 4 : connect events to SINTRAN functions
- 5 : specify time-related events at regular intervals

## Monitor call format:
| Instruction | Parameter | Description |
|-------------|-----------|-------------|
| LDA         | (PEVNT)   | % Address of parameter list |
| MON         | 352       | % MON EVENT |
| JMP         | ERR       | % A-register contains error code |
| ERR,        |           | % Handle errors |

| Parameter  | Description |
|------------|-------------|
| PEVNT, FUNC | % Address of function code |
| SIZE        | % Address of length of parameter array |
| ARRAY       | % Address of parameter array |
| FUNC, ..    | % Function code |
| SIZE, ..    | % Length of parameter array |
| ARRAY=*     | % Parameter array |

## Parameters
| P0 | 0 |
|----|---|
| P1 | 0 |
| P2 | 0 |
| P3 | 0 |
| P4 | 0 |
| P5 | 0 |
| P6 | 0 |

## Rules:
1. MON EVENT (MON 352) is available to all users in background, and to RT-programs.

---

## Page 31

# Function code 0:

## Function description:
Check if specified function is implemented.

## Input parameters:
| Parameter | Description |
|-----------|-------------|
| FUNC      | = 0         |
| SIZE      | = 2         |
| P0        | = Function code to check. |
| P1        | = Subfunction code (this applies only to functions 3 and 4, it is not used for other functions). |

## Output parameters:
| AD-register | Description |
|-------------|-------------|
| Status      | 0 = Function/subfunction is not implemented.<br>1 = Function/subfunction is implemented (note that this means that the A-register = 0; the D-register = 1). |

# Function code 1:

## Function description:
Set event(s) on specified process. The mask to be set is logically ORed (inclusive OR) with the event buffer of the specified process.

The event buffer is found in location EVSET of the extended RT-description, see page 72.

## Input parameters:
| Parameter | Description |
|-----------|-------------|
| FUNC      | = 1         |
| SIZE      | = 3         |
| P0        | = Process ID (RT-description address). 0 means current process. |
| P1        | = First part of event mask to be set. |
| P2        | = Last part of event mask to be set. |

## Output parameters:
None.

# Function code 2:

## Function description:
Read events for current process. The returned events are cleared in the process' event buffer.

The event buffer is found in location EVSET of the extended RT-description, see page 72.

## Input parameters:
| Parameter | Description |
|-----------|-------------|
| FUNC      | = 2         |
| SIZE      | = 0         |

## Output parameters:
| AD-register | Description |
|-------------|-------------|
| Events read | (from location EVSET). |

---

## Page 32

# Function Code 3

## Function Description
Wait for events to occur.  
The subfunction in P0 specifies different strategies for restarting the process.  
If P3 ≠ 0 then a timeout event will be generated when the specified timeout time is reached.

The event buffer used for this function is found in location EVWAIT of the extended RT-description, see page 72.

If the timeout function is used, the timeout event buffer found in location EVTMOUT of the extended RT-description (see page 72) is used.

If a timeout event is generated, this event mask is logically ORed (inclusive OR) with the event buffer found in location EVSET, and if the result satisfies the expected event mask, the process is restarted.

## Input Parameters
| Parameter | Description |
|-----------|-------------|
| FUNC      | = 3         |
| SIZE      | = 7         |
| P0        | = Subfunction (See below). <br> Bit 0 = Selection <br> Bit 1 = Completion |
| P1        | = First part of event mask to wait for. |
| P2        | = Last part of event mask to wait for.  |
| P3        | = Timeout value |
| P4        | = Time unit (only valid when P3 ≠ 0) <br> 1 = Basic time units (20 ms) <br> 2 = Seconds <br> 3 = Minutes <br> 4 = Hours |
| P5        | = First part of events to be set on timeout. |
| P6        | = Last part of events to be set on timeout. |

## Output Parameters
- **AD-register** = Events returned.

## Subfunction
This may contain any combination of Selection and Completion.

**Selection:**  
If this bit is set, then only those events the process is waiting for are returned and cleared. Otherwise, all events are returned and cleared.

**Completion:**  
If this bit is set, then the process is restarted only when all the events specified in P1, P2 have occurred.

# Function Code 4

## Function Description
Connect events to SINTRAN functions. If some SINTRAN functions are not connected, they will be treated as "other events". If "other events" are not connected, the process may be restarted without any events set.  
To use the SINTRAN functions terminal input or terminal output, the terminal input/output must specify NOWAIT mode.

---

## Page 33

# SINTRAN III Release Information, M-version

The event buffer used for this function depends on the SINTRAN function specified:
- **0 (other)**: location EVOVEV of the extended RT-description
- **1 (terminal input)**: location TRMIEV of the terminal input data field
- **2 (terminal output)**: location TRMOEV of the terminal output data field
- **3 (NUCLEUS event)**: NUCLEUS event mask

When the specified SINTRAN function occurs, the appropriate event mask is ORed (inclusive OR) with the event buffer found in location EVSET.

## Input Parameters:

| Parameter | Description |
|-----------|-------------|
| FUNC      | = 4         |
| SIZE      | = 4         |
| P0        | = SINTRAN function.<br>0 = Other events (not defined or not specified below).<br>1 = Terminal input (break condition encountered).<br>2 = Terminal output (ready for output).<br>3 = NUCLEUS event. |
| P1        | = For SINTRAN function = 1 or 2: The terminal's logical device number, 0 means own terminal.<br>For SINTRAN function = 0 or 3: This parameter must be zero. |
| P2        | = First part of events to be set on specified function. |
| P3        | = Last part of events to be set on specified function. |

## Output Parameters:
None.

# Function Code 5:

## Function Description:
Specify time-related events at regular intervals. Only one definition per process is allowed. Each call will redefine the previous settings. By specifying 0 in P4, the interval will be disabled.

The event buffer used for this function is found in location EVINTV of the extended RT-description, see page 72.

At each interval, the interval event mask is logically ORed (inclusive OR) with the event buffer found in location EVSET.

## Input Parameters:

| Parameter | Description |
|-----------|-------------|
| FUNC      | = 5         |
| SIZE      | = 6         |
| P0        | = Process ID (RT-description address). 0 means current process. |
| P1        | = First part of events to be set at specified intervals. |
| P2        | = Last part of events to be set at specified intervals. |
| P3        | = Delay (first time) |
| P4        | = Interval |
| P5        | = Time unit (valid for both P3 and P4)<br>1 = Basic time units (20 ms)<br>2 = Seconds<br>3 = Minutes<br>4 = Hours |

## Output Parameters:
None.

---

## Page 34

# 4. SINTRAN Service Program

## 4.1 Modified commands

### 4.1.1 *BACKGROUND-ALLOCATION-UTILITIES

One new parameter, `<output file>`, has been introduced in the subcommands DISPLAY, LIST-PARAMETERS and FREE-BACKGROUND-PROGRAMS. The parameter syntax for these subcommands is thus:

`<output file>` output file to receive the list (default = terminal).

The timeout function is changed. Only terminals in "terminal input wait" are now candidates for being logged out.

### 4.1.2 *CHANGE-DATAFIELD

1 old symbolic data field variable has been removed: TSTATE (23₈)

6 new symbolic data field variables may be used:

| Symbolic name | Octal value | Description |
|---------------|-------------|-------------|
| BITFLAG       | -7          | Various flag bits. |
| IN5MSG        | -24         | Address of ND-500/5000 message in fast INSTRING. |
| MNTMFL        | -14         | Timer routine parameter. |
| MTDFTYP       | 4           | MTAD type. |
| MTFLAG        | 5           | MTAD flags. |
| ON5MSG        | 25          | Address of ND-500/5000 message in fast OUTSTRING. |

### 4.1.3 *CHANGE-VARIABLE

One new variable may now be accessed:

| Name  | Description |
|-------|-------------|
| V24FL | Flag to indicate use of V.24 lines. The following flag bits are defined: <br> Bit 0: set to indicate that pin 19 is used for handshake. <br> Bit 1: set to indicate that DTR is to be dropped on logout. |

### 4.1.4 *LIST-TIME-SLICED-PROGRAMS

One parameter, `<output file>`, has been introduced, the parameter syntax is thus:

`<output file>` output file to receive the list (default = terminal).

---

## Page 35

# 4.2 New commands

## 4.2.1 *DUMP-DATAFIELD

Dump the contents of the different locations of a terminal, TAD, MTAD or NOTS data field. Only the non-DPIT part (the part outside resident data) of the data field is shown.

**Parameters:**

| Parameter            | Description                                                                                                |
|----------------------|------------------------------------------------------------------------------------------------------------|
| \<logical device number> | the logical device number of a terminal, TAD, MTAD or NOTS (octal value)                                 |
| \<input or output>       | 1 for input, O for output part of data field                                                             |
| \<output file>           | output file (default = terminal)                                                                         |

## 4.2.2 *FILE-SYSTEM-EVENT-LOG

The file system event-log utility is a system to report occurrences of selected file system events. It is intended for security-violation tracing and debugging purposes. A further description is found on pages 40-41.

All reports on occurrences of the selected events are routed through the ERS/SINTRAN III Watchdog.

The following subcommands are available in the file system event-log:

- **HELP**

  List all subcommands available in the event-log utility.

- **ENABLE-LOG \<memory?>,\<image?>,\<save area?>**

  Enable the file system event-log utility. Bit number 6 in the SINTRAN III system variable EXSECURITY is set to indicate this.

- **DISABLE-LOG \<memory?>,\<image?>,\<save area?>**

  Disable the file system event-log utility.

- **ENABLE-EVENT \<event>,\<memory?>,\<image?>,\<save area?>**

  Enable the logging of the specified event. A list of possible events is found on the next page. The subcommands ENABLE-ERROR and DISABLE-ERROR, described below, are used to further specify if all occurrences of the event or only occurrences in error situations are to be reported.

- **DISABLE-EVENT \<event>,\<memory?>,\<image?>,\<save area?>**

  Disable the logging of the specified event. A list of possible events is found on the next page.

- **LIST-ENABLED-EVENTS \<output file>**

  List the events selected to be logged.

---

## Page 36

# ENABLE-ERROR `<error code>,<memory?>,<image?>,<save area?>`

Enable logging of occurrences of the event selected by the subcommands ENABLE-EVENT and DISABLE-EVENT, described above, only when occurring in the specified error situations.

The codes available are in the ranges 1:255 and 1664:1698.  
The value -1 may be used to select all error codes.  
The value 0 may be used to select all occurrences of the event(s), indicating logging also when no errors occurred.

# DISABLE-ERROR `<error code>,<memory?>,<image?>,<save area?>`

Disable logging occurrences of specified error situations.

The codes available are in the ranges 0:255 and 1664:1698.  
A value of -1 may be used to select all error codes.

# LIST-ENABLED-ERRORS `<output file>`

List the error codes selected to be logged.

# EXIT

Exit from the file system event-log utility.

The following events may be selected by the subcommands ENABLE-EVENT and DISABLE-EVENT:

| Event               | Description |
|---------------------|-------------|
| CHANGE-PASSWORD     | use of the commands @CHANGE-PASSWORD or @CLEAR-PASSWORD. |
| CHANGE-USER-AREA    | use of UE-FUNCTION CHANGE-USER-AREA or MON SUSCN or MON RUSCN. |
| CHANGE-USER-ENTRY   | use of the command @CHANGE-USER-ENTRY. |
| CREATE-FILE         | use of the commands @CREATE-FILE, @ALLOCATE-FILE, @CREATE-NEW-VERSION or @ALLOCATE-NEW-VERSION; or MON CRALF or MON CRALN. |
| CREATE-USER         | use of the command @CREATE-USER. |
| DELETE-FILE         | use of the command @DELETE-FILE or @DELETE-USERS-FILES; or MON MDLFI. |
| DELETE-USER         | use of the command @DELETE-USER. |
| LOGIN               | use of the command @LOGIN or MON MLOGI. |
| LOGOUT              | use of the command @LOGOUT. |
| OPEN-FILE           | use of the commands @OPEN-FILE, @SCRATCH-OPEN, @CONNECT-FILE, @RTOPEN-FILE or RTCONNECT-FILE; or MON OPEN, MON DOPEN, MON SCROP, MON IOPEN. |
| READ-USER-ENTRY     | use of the command @DUMP-USER-ENTRY or MON RUSER. |
| RENAME-FILE         | use of the command @RENAME-FILE or MON MRNFI. |
| RENAME-USER         | use of the command @RENAME-USER. |
| SET-FILE-ACCESS     | use of the command @SET-FILE-ACCESS or MON SFACC. |
| ALL                 | all the events listed above. |
| HELP                | list available events. |

---

## Page 37

# 5. ND-500/5000 Monitor

The ND-500/5000 Background Monitor version K or later and the ND-500/5000 Swapper version L are intended to be used under SINTRAN III version M.

## 5.1 Installation procedure

All software required to run an ND-500/5000 system is delivered as one product: ND-500/5000 System Package for SINTRAN III/VSX, version M (ND 211305). This product is delivered on one diskette to simplify installation.

The products concerned are:
- ND-500/5000 Monitor (background part)
- ND-500/5000 Swapper
- ND-500/5000 Place Library

For a complete installation of these products, see the product description. An example of a complete installation of SINTRAN III (including these products) is given on pages 6-9 in this manual, and in the SINTRAN III/VSX product description.

## 5.2 Configuration limitations

- The previous limitation of memory size to 32 megabytes was changed to 128 megabytes on ND-5000 systems in the L-version of SINTRAN III. This new limit (128 megabytes) now applies to all ND-500/5000 systems.
- The maximum number of standard domains has been changed to 100.

## 5.3 Modified commands to SINTRAN III affecting the ND-500/5000

### 5.3.1 @ENTER

The fourth (last) parameter, `<maximum time>`, is now a combined limit of ND-100 and ND-500/5000 CPU time. This means that the total CPU time used by the batch job (as the sum of ND-100 and ND-500/5000 CPU time) may not exceed this limit.

### 5.3.2 @RECOVER

On an ND-500/5000 system, the RECOVER-command can now be used to start both ND-100 programs (:PROG-files) and ND-500/5000 domains.

This means that the order of commands/files attempted when a name is given without explicit user area and/or file type, is as follows:

1. SINTRAN III commands (if the command name RECOVER is omitted).
2. ND-100 reentrant subsystem and ND-500/5000 standard domains.
3. `<file>::DOM` (if the ND-500/5000 Monitor is a reentrant subsystem) both the current user area and the user area SYSTEM are searched in the following order: current user area first, then user area SYSTEM.
4. `<file>::PROG` both the current user area and the user area SYSTEM are searched in the following order: current user area first, then SYSTEM.

Note that this implies that if you have two programs, one for ND-100 and the other for ND-500/5000, with the same name (:DOM and :PROG files with same file name) and omit the file type, the ND-500/5000 program is started.

---

## Page 38

# 5.3.3 @TERMINAL-STATUS

The CPU-time shown is now the sum of ND-100 CPU time and ND-500/5000 CPU time.

# 5.4 Modified commands to the ND-500/5000 background monitor

## 5.4.1 DEFINE-STANDARD-DOMAIN

The maximum number of ND-500/5000 standard domains has been increased to 100. Further, the maximum number of entries in the reentrant-subsystem table (ND-100 reentrant subsystems and ND-500/5000 standard domains) has been increased to 400. The total length of names of all entries in the table has been increased to 4096 characters (including the apostrophe terminating each name).

## 5.4.2 LOOK-AT-HARDWARE

The output from this command is changed if running on an ND-5830 or an ND-5850 CPU.

# 5.5 Modified monitor calls to the ND-100 affecting ND-500/5000

## 5.5.1 N500M MON 60

Function 174₈ LOIMM (Load IMAP via Memory).

This is a new function only supported on ND-5830 and ND-5850 systems. It is used by hardware test programs.

# 5.6 Modified monitor calls (ND-500/5000)

## 5.6.1 SETCM MON 12

The maximum length of the command buffer has been increased to 150 characters.

## 5.6.2 COMND MON 70

The maximum length of the command buffer has been increased to 150 characters.

## 5.6.3 APSPF MON 240

The maximum length of parameter number 4 (<optional message>) has been decreased from 128 characters to 80 characters.

## 5.6.4 UECOM MON 317

The maximum length of the command buffer has been increased to 150 characters.

---

## Page 39

# 5.6.5 FSMTY MON 327

The two new functions 13₈ and 14₈ are also available on ND-500/5000:

## Function no. 13₈:

**Function:**  
Reset the "file-modified"-bit in object entry.

**Monitor call format:**  
CALLG 37000000327B,4 or 5,<function>,<directory index>,<user index>,<file object index>[,<remote system specification>]

**Input parameters:**

| Parameter              | Description                                                                                       |
|------------------------|---------------------------------------------------------------------------------------------------|
| <function>             | function = 13₈                                                                                    |
| <directory index>      | directory index. If bit number 7 is set, the file is located on a remote system, and the 5th parameter is taken to contain a remote system specification |
| <user index>           | user index.                                                                                       |
| <file object index>    | file object index.                                                                                |
| <remote system spec.>  | remote system specification. This parameter applies only if bit number 7 of the <directory index> is set to indicate that the file is located on a remote system. |

**Output parameters:**  
None.

## Function no. 14₈:

**Function:**  
Get page number of next page in a file.  
This is useful for files containing holes: if a page number inside a "hole" is input, the first page number after the "hole" is returned.

**Monitor call format:**  
CALLG 37000000327B,4,<function>,<file number>,<start logical page number>,<next existing logical page number>

**Input parameters:**

| Parameter                  | Description                                                                                      |
|----------------------------|--------------------------------------------------------------------------------------------------|
| <function>                 | function = 14₈                                                                                    |
| <file number>              | connect file number.                                                                             |
| <start logical page no.>   | logical page number to start from<br>- if the given logical page number refers to a page inside a hole, the logical page number of the first page after the hole is returned.<br>- if the given logical page number refers to an existing page, this page number is returned. |

**Output parameters:**

| Parameter                  | Description                                                                                      |
|----------------------------|--------------------------------------------------------------------------------------------------|
| <next existing page no.>   | logical page number of next page in file (may be the same as input).                             |

Refer to pages 15-16 for a description of these functions used from ND-100 programs.

---

## Page 40

# 5.6.6 TERST MON 330

The CPU-time returned is now the sum of ND-100 CPU time and ND-500/5000 CPU time.

# 5.6.7 IOMTY MON 336

One new function has been introduced:

function 26₈: get protocol ID and MTAD ID from the MTAD data field

**Function no. 26₈:**

**Function:**  
Get protocol ID and MTAD ID from the MTAD data field.

**Monitor call format:**  
CALLG 37000000336B,4,<function>,<size>,<array>,<status>

**Parameters:**  
<function> function = 26₈.  
<size> size of parameter array.  
<array> parameter array.  
<status> returned status.

### Input parameters:
| Parameter | Description |
|-----------|-------------|
| 1 | Logical device number. |

### Output parameters:
| Parameter | Description |
|-----------|-------------|
| 2 | Protocol ID. |
| 3 | MTAD ID 1. |
| 4 | MTAD ID 2. |
| 5 | MTAD ID 3. |
| 6 | MTAD ID 4. |

Protocol ID = 1 (Telnet) implies the following values:  
- MTAD ID 1: IP-address  
- MTAD ID 2: IP-address  
- MTAD ID 3: 0  
- MTAD ID 4: port  

### Rules:
1. Available to all users.

Refer to page 17 for a description of this function used from ND-100 programs.

---

## Page 41

# 5.7 New monitor calls (ND-500/5000)

## 5.7.1 IOPEN MON 351

**Monitor call description.**  
Open a file with specific directory, user and file object indexes or return indexes.  
The input parameters may specify either the directory, user and file object indexes or the file name and file type.  
If both file name and file system indexes are specified, the file system will verify that the file indexes match the file name and type.  
A description of MON IOPEN used from ND-100 programs are found on pages 18-19.

**Monitor call format:**  
CALLG 37000000351B,5,<file number>,<access code>,<file name>,<file type>,<directory/user/object index>

**Parameters:**

| Parameter                  | Description                                                                                                                                      |
|----------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------|
| `<file number>`            | ND-500/5000 connect file number. If 0 is specified, the first free connect number is used. The connect number is then returned.                   |
| `<access code>`            | File access mode (see table below).                                                                                                              |
| `<file name>`              | File name and/or type (this parameter does not apply if the file system indexes parameter is used).                                               |
| `<file type>`              | Default file type (this parameter does not apply if the file system indexes parameter is used).                                                   |
| `<dir-/user/object index>` | File system indexes (32-bit word):<br> first byte: directory index<br> second byte: user index<br> last two bytes: object index<br> The value -1 is used to indicate that file name and type from the parameters above are used instead. The indexes will then be returned. |

The file access modes used by MON IOPEN may specify three different ranges corresponding to access modes used by:

| Mode                  | Range     |
|-----------------------|-----------|
| MON OPEN (MON 50)     | range 0 - 11₈  |
| MON SCROP (MON 235)   | range 40₈ - 51₈ |
| MON DOPEN (MON 220)   | range 100₈ - 111₈ |

---

## Page 42

# The access modes are:

| Code   | Description |
|--------|-------------|
| 0      | sequential write. |
| 1      | sequential read. |
| 2      | random read or write. |
| 3      | random read only. |
| 4      | sequential read or write. |
| 5      | sequential write append. |
| 6      | random read or write common on contiguous files. |
| 7      | random read common on contiguous files. |
| 10₈    | random read or write on contiguous files. Direct transfer for MON RFILE (MON 117), MON WFILE (MON 120) and MON MAGTP (MON 114) in RT-programs. |
| 11₈    | random read, write append for MON WFILE (MON 120). |
| 40₈    | sequential write. |
| 41₈    | sequential read. |
| 42₈    | random read or write. |
| 43₈    | random read only. |
| 44₈    | sequential read or write. |
| 45₈    | sequential write append. |
| 46₈    | random read or write common on contiguous files. |
| 47₈    | random read common on contiguous files. |
| 50₈    | random read or write on contiguous files. Direct transfer for MON RFILE (MON 117), MON WFILE (MON 120) and MON MAGTP (MON 144) in RT-programs. |
| 51₈    | random read, write append for MON RFILE (MON 117) and MON WFILE (MON 120). |
| 100₈   | sequential write. |
| 101₈   | sequential read. |
| 102₈   | random read or write. |
| 103₈   | random read only. |
| 104₈   | sequential read or write. |
| 105₈   | sequential write append. |
| 106₈   | random read or write common on contiguous files. |
| 107₈   | random read common on contiguous files. |
| 110₈   | random read or write on contiguous files. Direct transfer for MON RFILE (MON 117), MON WFILE (MON 120) and MON MAGTP (MON 114) in RT-programs. |
| 111₈   | random read, write append for MON WFILE (MON 120). |

## 5.7.2 EVENT MON 352

MON EVENT, the programming interface to the new event system, is also available from programs running on the ND-500/5000.

A description of the event system is found on pages 44-45, and the monitor call used in programs running on the ND-100 part of the system is described on pages 20-23.

---

## Page 43

# MON EVENT Functions

MON EVENT has 6 functions:

| Code | Description |
|------|-------------|
| 0    | Check if specified function is implemented |
| 1    | Set event(s) on specified process |
| 2    | Read events on current process |
| 3    | Wait for events to occur |
| 4    | Connect events to SINTRAN functions |
| 5    | Specify time-related events at regular intervals |

## Rules

1. MON EVENT (MON 352) is available to all users.
2. All parameters are 32-bit values on the ND-500/5000.
3. Error returns are flagged by the K-register and error codes are returned in the W1-register.
4. As event masks are kept in the (extended) RT-description, the RT-description address is used for references to a process.
   - For ND-500/5000 programs, this means the RT-description address of the shadow process.

## Function Code 0

### Function Description

Check if specified function is implemented.

### Input Parameters

- Number of parameters = 3
  - `<function = 0>`
  - `<function code to check>`
  - `<subfunction to check>` (applies only if `<function to check>` = 3 or 4)

### Output Parameters

- I1 = Status
  - 0 = Function/subfunction is not implemented.
  - 1 = Function/subfunction is implemented

## Function Code 1

### Function Description

Set event(s) on specified process. The mask to be set is logically ORed (inclusive OR) with the event buffer of the specified process.

The event buffer is found in location EVSET of the extended RT-description, see page 72.

### Input Parameters

- Number of parameters = 3
  - `<function = 1>`
  - `<RT-description address>`
  - `<event mask to be set>`

### Output Parameters

- None.

---

## Page 44

# Function code 2:

## Function description:
Read events for current process.  
The returned events are cleared in the process' event buffer.

The event buffer is found in location EVSET of the extended RT-description, see page 72.

## Input parameters:
- number of parameters = 1
- `<function = 2>`

## Output parameters:
- I1-register = Events read (from location EVSET).

# Function code 3:

## Function description:
Wait for events to occur.  
The subfunction specifies different strategies for restarting the process.  
If subfunction ≠ 0 then a timeout event will be generated when the specified timeout time is reached.

The event buffer used for this function is found in location EVWAIT of the extended RT-description, see page 72.

If the timeout function is used, the timeout event buffer found in location EVTMOUT of the extended RT-description (see page 72) is used.

If a timeout event is generated, this event mask is logically ORed (inclusive OR) with the event buffer found in location EVSET, and if the result satisfies the expected event mask, the process is restarted.

## Input parameters:
- number of parameters = 6
- `<function = 3>`
- `<subfunction> (see below)`
  - Bit 0 = Selection
  - Bit 1 = Completion
- `<event mask to wait for>`
- `<timeout value>`
- `<time unit> (only valid when subfunction ≠ 0)`
  - 1 = Basic time units (20 ms)
  - 2 = Seconds
  - 3 = Minutes
  - 4 = Hours
- `<events to be set on timeout>`

## Output parameters:
- I1-register = Events returned.

---

## Page 45

# Subfunction

This may contain any combination of Selection and Completion.

**Selection:** If this bit is set, then only those events the process is waiting for is returned and cleared. Otherwise, all events are returned and cleared.

**Completion:** If this bit is set, then the process is restarted only when all the events specified in \<event mask to wait for\> have occurred.

# Function code 4

## Function description

Connect events to SINTRAN functions. If some SINTRAN functions are not connected, they will be treated as "other events". If "other events" are not connected, the process may be restarted without any events set.  
To use the SINTRAN functions terminal input or terminal output, the terminal input/output must specify NOWAIT mode.

The event buffer used for this function depend on the SINTRAN function specified:  

| Code | Description |
|------|-------------|
| 0 (other) | location EV0EV of the extended RT-description |
| 1 (terminal input) | location TRMIEV of the terminal input data field |
| 2 (terminal output) | location TRMOEV of the terminal output data field |
| 3 (NUCLEUS event) | NUCLEUS event mask |

When the specified SINTRAN function occurs, the appropriate event mask is ORed (inclusive OR) with the event buffer found in location EVSET.

## Input parameters

- **number of parameters** = 4  
- \<function = 4\>  
- \<SINTRAN function\>  
  - 0 = Other events (not defined or not specified below).
  - 1 = Terminal input (break condition encountered).
  - 2 = Terminal output (ready for output).
  - 3 = NUCLEUS event.  
- \<logical device number\> the following values apply:  
  - For SINTRAN function = 1 or 2: The terminal's logical device number, 0 means own terminal.
  - For SINTRAN function = 0 or 3: This parameter must be zero.

\<events to be set on specified function\>

## Output parameters

None.

---

## Page 46

# Function code 5:

## Function description:
Specify time-related events at regular intervals. Only one definition per process is allowed. Each call will redefine the previous settings. By specifying 0 in `<interval>`, the interval will be disabled.

The event buffer used for this function is found in location EVINTV of the extended RT-description, see page 72.

At each interval, the interval event mask is logically ORed (inclusive OR) with the event buffer found in location EVSET.

### Input parameters:
- number of parameters = 6
- `<function = 5>`
- `<process ID (RT-description address). 0 means current process>`
- `<events to be set at specified intervals>`
- `<delay (first time)>`
- `<interval>`
- `<time unit>` (valid for both `<delay>` and `<interval>`)
  - 1 = Basic time units (20 ms)
  - 2 = Seconds
  - 3 = Minutes
  - 4 = Hours

### Output parameters:
None.

## 5.8 Modified monitor calls - available only on ND-500/5000

### 5.8.1 FIXME MON 410
The first parameter, type of fixing, now takes 3 additional values.

The possible values are:

| Value | Description |
|-------|-------------|
| 0     | fix pages scattered |
| 1     | fix pages contiguously (return start address in memory) |
| 2     | fix pages absolutely (contiguously starting at given address) |
| 3     | fix pages scattered, possibly above the old 32 Mbyte limit |
| 4     | fix pages contiguously, possibly above the old 32 Mbyte limit |
| 5     | fix pages absolutely, possibly above the old 32 Mbyte limit |

---

## Page 47

# 5.8.2 5MTRANS MON 515

One new function has been introduced: disk transfer setting event flag. This function makes it possible for a process to start several disk transfer operations and then wait for all of them to be completed.

## Function = DISK TRANSFER SETTING EVENT FLAG

**Monitor call format:**

```
CALLG 37000000515B,10B,<function>,<I/O code>,<request id.>,<memory addr.>,
<disk id.>,<sector>,<number of sectors>,<event mask>
```

### Parameters:

- **<function>**  
  bit number 4 set, disk transfer with event flag.  
  Nowait mode is always used for this function (regardless of the value of bit number 16)

- **<I/O code>**  
  returned HW status (16 least significant bits)

- **<request identifier>**  
  file magic number (16 most significant bits)  
  request id. - user defined value (16 least significant bits)

- **<memory address>**  
  ND-100 physical memory address (must be a contiguously fixed area)

- **<disk identifier>**  
  bits 16-31 : logical device number  
  bits 6-8 : unit number  
  bits 0-5 : function  
  0 = read  
  1 = write  
  6 = read without clearing cache (applicable only if cache-inhibit is set for this area)  
  7 = write without "dump dirty" (write from cache-inhibit area)

- **<sector>**  
  disk sector number

- **<number of sectors>**  
  number of sectors to transfer

- **<event mask>**  
  event mask used (to be logically ORed to event mask of calling process on completion)

### Function values returned (octal value):

- **K-register = 0 :** W1-register = 1 : OK, request received (nowait mode)

- **K-register = 1 :** the W1-register contains function values as follows:

| Value | Description                                           |
|-------|-------------------------------------------------------|
| 6     | No disk optimisation for this controller              |
| 7     | Illegal read/write function                           |
| 10    | Segment is not contiguously fixed                     |
| 11    | Disk transfer error, <I/O code> contains hardware status |
| 12    | Illegal 5MTRANS function, neither disk transfer, check event, start process nor get magic number |
| 14    | Illegal monitor call (not implemented)                |
| 15    | Illegal file magic number                             |
| 17    | Not write access                                      |
| 20    | Attempt to access outside file                        |
| 21    | Illegal unit number                                   |
| 22    | Illegal logical device number                         |
| 23    | No more free disk access queue elements               |

---

## Page 48

# Example

| Item   | Value          | Description                              |
|--------|----------------|------------------------------------------|
| FUNC   | W DATA 20B     | % disk transfer setting event flag       |
| IOCD1  | W DATA 0       | % returned HW status                     |
| IOCD2  | W DATA 0       | % returned HW status                     |
| RQID1  | W DATA 131     | % request ID = 131 (any value)           |
| RQID2  | W DATA 313     | % request ID = 313 (any value)           |
| MEMA1  | W DATA 100000000B | % ND-100 physical memory address     |
| MEMA2  | W DATA 200000000B | % ND-100 physical memory address     |
| DSKID  | W DATA 1100001B   | % write to main disk (1100₈) unit 0  |
| SECT1  | W DATA 123B    | % sector no. 123₈                         |
| SECT2  | W DATA 145B    | % sector no. 145₈                         |
| NOSECT | W DATA 2       | % transfer 2 sectors                     |
| EVFL1  | W DATA 4       | % use bit 2 of event mask                |
| EVFL2  | W DATA 10B     | % use bit 3 of event mask                |

CALLG 37000000515B,10B,FUNC,IOCD1,RQID1,MEMA1,DSKID,SECT1,NOSECT,EVFL1  
% First call on MON 515 with 8 parameters  
% on error return, IOCD1 = error  

IF K GO ERROR  

CALLG 37000000515B,10B,FUNC,IOCD2,RQID2,MEMA2,DSKID,SECT2,NOSECT,EVFL1  
% Second call on MON 515 with 8 parameters  
% on error return, IOCD2 = error  

% Use MON EVENT (MON 352) function 3 (wait for event) waiting for  
% event 14₈ (logical OR of bits 2 and 3) to occur.  
%   
% Then use MON 5MTRANS function Check Event to check status on each disk  
% transfer.  

# Rules

1. Function Disk Transfer Setting Event Flag always runs in nowait mode (immediate return).
2. There is no check on `<request ID>` to see if it already has been used on a pending transfer from the same process. A process may thus have several pending transfers with the same request id.
3. MON 5MTRANS is reserved for internal use by ND.
4. Note that 5MTRANS can only be used if disk sorting is enabled.

---

## Page 49

# 5.9 New monitor calls - available only on ND-500/5000

## 5.9.1 PLACE MON 441

**Monitor call description.**  
Place a program or data segment.  
To disconnect the segment again, use MON ClearCapability (MON 424).  
MON PLACE is equivalent to MON N500M (MON 60) function NEWPLACE (160₈).

**Monitor call format:**  
CALLG 370000000441B,7,<file name>,<file offset>,<size>,<logical segment number>,  
&nbsp;&nbsp;&nbsp;&nbsp;<logical type and attributes>,  
&nbsp;&nbsp;&nbsp;&nbsp;<SINTRAN III shared segment information>,  
&nbsp;&nbsp;&nbsp;&nbsp;<returned segment number>

**Input parameters:**

| Parameter | Description |
|-----------|-------------|
| `<file name>` | ND-500/5000 descriptor addressing, must include file type. |
| `<file offset in bytes>` | |
| `<size in bytes>` | |
| `<logical segment number>` | if logical segment number 0 is specified, the first free segment number is used. |
| `<logical type and attributes>` | |
| `<SINTRAN III shared segment information>` | |

**Output parameters:**

| Parameter | Description |
|-----------|-------------|
| `<returned segment number>` | segment number is returned in the address specified by input parameter 7. |

---

## Page 50

# 6. File System

## 6.1 Changes in configuration limitations

- The maximum number of device buffers has been increased from 64 to 128.
- The maximum number of BDIO pools supported by SINTRAN III has been increased to 64 in generation 6 of SINTRAN III version M.

## 6.2 Performance

For indexed files, the function of allocating a new page is changed to give increased performance. This function now performs approximately three times faster. This means that common functions like reading a file into NOTIS-WP (which implies writing a copy of the file to a scratch file) will run significantly faster.

The maximum size of the disk cache is doubled by increasing the number of device buffers. Note that if you install the M-version of SINTRAN III to match the configuration used for the L-version, the size of the disk cache will also be the same. To increase the disk cache, use the SINTRAN III Configuration Program to change the number of device buffers. A cold start is required for such changes to take effect.

Note that operating the system with a large disk cache implies that the contents of the disk cache may not be written to disk immediately. To force writing the disk cache to disk on a controlled stop of the system, you should ensure that all files are closed properly. For example, use the command @RELEASE-DIRECTORY on all entered directories.

## 6.3 File system event-log utility

The file system event-log utility is a system to report occurrences of selected file system events such as login, logout, open file, etc. It is possible to specify reporting on all occurrences of the events or just unsuccessful attempts of the operations.

It is intended for security-violation tracing and debugging purposes.

The file system event-log utility is operated by the command *FILE-SYSTEM-EVENT-LOG in the SINTRAN Service Program. A detailed description of this command is found on pages 25-26.

## 6.3.1 Reports from the file system event-log

All reports on occurrences of the selected events are routed through the ERS/SINTRAN III Watchdog.

This means that it may be necessary to increase the size of the log file used by the Watchdog. If the number of reported file system events is expected to be high, the file ER-S3WD-LOG:DATA on user area SYSTEM should be expanded (it is a contiguous file and it will probably be necessary to delete it and recreate it with a larger size). The Watchdog must close and re-open the log file to bring this change into effect. A description of the recommended procedure is found on page 59.

---

## Page 51

# 6.3.2 File system operations available for logging

The following file system operations may be logged:

| Operation           | Description                                                                 |
|---------------------|-----------------------------------------------------------------------------|
| change password     | use of the commands @CHANGE-PASSWORD or @CLEAR-PASSWORD.                     |
| change user area    | use of UE-FUNCTION CHANGE-USER-AREA or MON SUSCN or MON RUSCN.               |
| change user entry   | use of the command @CHANGE-USER-ENTRY.                                      |
| create file         | use of the commands @CREATE-FILE, @ALLOCATE-FILE, @CREATE-NEW-VERSION or @ALLOCATE-NEW-VERSION; or MON CRALF or MON CRALN. |
| create user area    | use of the command @CREATE-USER.                                            |
| delete file         | use of the command @DELETE-FILE or @DELETE-USERS-FILES; or MON MDLFI.       |
| delete user area    | use of the command @DELETE-USER.                                            |
| login               | use of the command @LOGIN or MON MLOGI.                                     |
| logout              | use of the command @LOGOUT.                                                 |
| open file           | use of the commands @OPEN-FILE, @SCRATCH-OPEN, @CONNECT-FILE, @RTOPEN-FILE or RTCONNECT-FILE; or MON OPEN, MON DOPEN, MON SCROP, MON IOPEN. |
| read user entry     | use of the command @DUMP-USER-ENTRY or MON RUSER.                           |
| rename file         | use of the command @RENAME-FILE or MON MRNFI.                               |
| rename user area    | use of the command @RENAME-USER.                                            |
| set file access     | use of the command @SET-FILE-ACCESS or MON SFACC.                           |

# 6.3.3 Operation of the file system event-log

The file system event-log utility is operated by the command *FILE-SYSTEM-EVENT-LOG in the SINTRAN Service Program.

This command has subcommands for selecting and listing which file system events (file system operations) to consider and when the selected events are to be reported.

For the events (file system operations selected) it is possible to specify reports on all or only (selected) unsuccessful attempts.

This is done by specifying which error codes will generate reports: error codes in the ranges 1:255 and 1664:1698 indicate errors and 0 means all situations including successful attempts (no error).

---

## Page 52

# 7. Spooling

The maximum length of the user message in the `@APPEND-SPOOLING-FILE` command and `MON APSPF (MON 240)` has been decreased from 128 characters to 80 characters.

---

## Page 53

# 8. XMSG

## 8.1 Changes in configuration limitations

In the O-version of X-message (part of SINTRAN III/VSX, M-version, generation 6 and later), the internal capacity is increased to allow for more buffer and table space.

## 8.2 Changed error handling

The O-version of X-message (part of SINTRAN III/VSX, M-version, generation 6 and later), routes all error messages through the ERS/SINTRAN III Watchdog.

## 8.3 Modified function

### 8.3.1 XFOPN

The XFOPN function of MON XMSG (MON 200) is changed to support the new event system.

If the XFEVE bit (bit number 13\(_8\)) is set in the T-register, the AD-register is assumed to contain an event mask for this port. This means that when this option is used, the RT-program is restarted with the specified event mask when messages are sent to this port.

When this option is used, the WAKE-UP option must NOT be used for further calls to this port.

---

## Page 54

# 9. The Event System

## 9.1 Introduction

The event system is used for synchronisation purposes between processes (two-way synchronisation) and between interrupt handlers and processes (one-way synchronisation). "Process" means RT-program, which implies that for ND-500/5000 programs, the shadow RT-program is used.

The event system is fairly general and may be used for solving a wide range of problems regarding the signalling part of inter-process communication.

## 9.2 Event buffer

Each process has an event buffer containing the current events set for it. The event buffer is represented as a 32-bit integer where each bit corresponds to a discrete event. The event buffer is kept in the extended RT-description (further description is found on pages 72-73).

## 9.3 Event agreement

The communicating processes must agree in advance upon which event bit (or bits) to use and the exact meaning of each bit (sometimes called the semantic of the event bit).

Events are normally used in combination with additional information exchanged between the processes, for example, a message stored in a mailbox.

The event itself can only say 'something occurred' but any additional information must be passed through different channels.

## 9.4 The functions of the event system

The following basic functions are available in the event system:

- **Set event(s) on specified process.**  
  This means that specified events are forced to occur for the specified process. If this process is waiting for events to occur, and the restart condition (expected events) is satisfied, it is restarted.

- **Read events on current process.**  
  Used by a process to read its own event buffer.

- **Connect events to SINTRAN functions.**  
  Used to specify that some SINTRAN III functions (currently, terminal input or output and Nucleus) will cause an event. For terminal input or output, NOWAIT mode must be used.

---

## Page 55

# SINTRAN III Release Information, M-version

* Wait for events to occur (with possibility for timeout).  
  Used by a process which is going to wait for some other process to restart it (or time-related events).  
  A process may wait for several different events.  
  It is also possible to set a time limit and specify an event mask associated with this time limit. If a timeout occurs, the events specified in the timeout event mask are made to occur and the process is restarted if the restart condition (expected events) is satisfied.

* Specify time-related events at regular intervals.  
  Specify that selected events are made to occur at fixed time intervals, possibly causing the process to be restarted.

A detailed description of these functions is found under the description of MON EVENT (MON 352) for use from ND-100 programs (pages 20-23) and from ND-500/5000 programs (pages 32-36).

---

## Page 56

# 10. Security Primitives

## 10.1 File system event log

An event-log utility is introduced. This utility will report occurrences of file system events according to conditions specified by the FILE-SYSTEM-EVENT-LOG command to the SINTRAN Service Program. Refer to pages 25-26 and 40-41 for more information.

Bit number 6 in the system variable EXSECURITY is used to indicate if the file system event-log utility is activated or not.

The system variable EXSECURITY now has the following layout:

| Bit  | Description |
|------|-------------|
| 0    | No listing of command lines in the @TERMINAL-STATUS command except for own user. If the command is performed by user SYSTEM, the command lines for all background programs logged in will be listed. The command lines will also be listed for the background programs running under the same user as the one executing the @TERMINAL-STATUS command. |
| 1    | The background segment, both program and data bank, will be set to zero when logging out. This feature will delay the logout sequence considerably (seconds). If the background program has been terminated abnormally, this zeroing will take place the first time you log in after the abnormal termination. |
| 2    | The scratch file pages written to in the last session, will be set to zero when logging out. This will also slow down the logout sequence. |
| 3    | Zeroing of pages released from a file, normally in the @DELETE-FILE command. |
| 4    | Not allowed to log in if the user has no password. Only one login without a password is allowed after @CREATE-USER. If this bit is set, remote file access to users without a password is also not allowed. |
| 5    | The commands @HELP and @LIST-REENTRANT will only list commands and reentrant subsystems and/or ND-500/5000 standard domains available to the user giving the command. An unprivileged user will thus not "see" commands available only to users SYSTEM or RT. |
| 6    | The file system event-log utility is activated. |

The default value of the variable EXSECURITY is 7 (bits 0, 1 and 2 are set) but this can be changed by the SINTRAN Service Program command *CHANGE-VARIABLE*.

## 10.2 Improved remote file server

The remote file server included in COSMOS Basic Module version G offers protection against users attempting access with the wrong password.

When a user makes a number of unsuccessful attempts in sequence, this is reported to the error device on the target system on every 20th attempt. The message "Remote user attempted access with wrong password via file server" is printed.

Further, the user will be denied further access to the remote file server, and the error message "Remote file server not available" is returned.

---

## Page 57

# 11. SINTRAN III Mail System

The contents of the last direct broadcast message are now also written to internal device number 275₅.

---

## Page 58

# 12. New Error Messages

## 12.1 SINTRAN III run-time errors

| Octal number | Message                                       | Parameters                      |
|--------------|-----------------------------------------------|---------------------------------|
| 1671         | BDIO pool reconnected                         | BDIO pool name (device name)    |
| 1672         | Unsuccessful attempt to reconnect to BDIO pool| BDIO pool name (device name)    |

## 12.2 Error codes returned from monitor calls - numeric list

| Error Octal | Code: Decimal | Message                                         |
|-------------|---------------|-------------------------------------------------|
| 3231        | 1688          | Illegal directory index                         |
| 3232        | 1689          | Illegal user index                              |
| 3233        | 1690          | Illegal object index                            |
| 3234        | 1691          | Directory index does not match directory name   |
| 3235        | 1692          | User index does not match user name             |
| 3236        | 1693          | Object index does not match object name         |
| 3237        | 1694          | Illegal file type                               |
| 3240        | 1695          | Illegal version number                          |
| 3241        | 1696          | Not so much space available                     |
| 3242        | 1697          | The specified bit file pages are not free       |

## 12.3 Error codes returned from monitor calls - alphabetic list

### Directory index does not match directory name (3234₈)

The directory name in a file specification does not match the name of the directory with the directory index specified.

### Illegal directory index (3231₈)

Directory index out of range or no directory with the index specified.

### Illegal file type (3237₈)

The file type in a file specification does not match the file type of the file with the object index specified.

### Illegal object index (3233₈)

Object (file) index out of range or no file with the index specified.

### Illegal user index (3232₈)

User index out of range or no user with the index specified.

---

## Page 59

# Illegal version number (3240₈)

The version number in a file specification does not match the version number of the file with the object index specified.

# Not so much space available (3241₈)

Space not available to expand the directory with the number of pages specified.

# Object index does not match object name (3236₈)

The file name in a file specification does not match the name of the file with the object index specified.

# The specified bit file pages are not free (3242₈)

Attempt to reposition the bit-file to an area which is not free.

# User index does not match user name (3235₈)

The user name in a file specification does not match the name of the user with the user index specified.

## 12.4 Error codes returned from ND-5850 service partner (James)

| Error Octal | Code Decimal | Message                            |
|-------------|--------------|------------------------------------|
| 7000        | 3584         | Microprogram already running       |
| 7001        | 3585         | Microprogram not started           |
| 7002        | 3586         | No parameter pointer given         |
| 7003        | 3587         | Illegal word count                 |
| 7004        | 3588         | Illegal address                    |
| 7005        | 3589         | Wrong checksum                     |
| 7007        | 3591         | No such function defined           |
| 7010        | 3592         | ND-5000 is not alive               |
| 7011        | 3593         | Memory error                       |
| 7015        | 3597         | Illegal CPU configuration          |
| 7016        | 3598         | No system parameters given         |
| 7017        | 3599         | Illegal CPU number                 |
| 7020        | 3600         | Tracer not present                 |
| 7021        | 3601         | Illegal parameter                  |
| 7022        | 3602         | Illegal function for this CPU type |

---

## Page 60

# Logical Device Numbers

The following logical device numbers are used for different purposes than in previous versions:

| Number | Description |
|--------|-------------|
| 275 | last direct broadcast message from SINTRAN III MAIL system |
| 505 | not used; previously: user-file-buffer semaphore |
| 506 | not used; previously: object-file-buffer semaphore |
| 545 | not used; previously: ECC disk controller 3, unit 0, bit-file semaphore |
| 547 | not used; previously: ECC disk controller 3, unit 1, bit-file semaphore |
| 551 | not used; previously: ECC disk controller 3, unit 2, bit-file semaphore |
| 553 | not used; previously: ECC disk controller 3, unit 3, bit-file semaphore |
| 1017 | ND-500/5000 name segment semaphore |
| 1020 | ND-500/5000 standard domain segment semaphore |
| 1021 | ND-500/5000 control store semaphore |
| 1022 | ND-500/5000 place swapper semaphore |
| 1023 | ND-500/5000 fix segment semaphore |
| 1024 | ND-500/5000 semaphore |
| 1025 | ND-500/5000 general semaphore |
| 1026 | ND-500/5000 semaphore |
| 1027 | ND-500/5000 semaphore |
| 1030 | ND-500/5000 CPU number 1 data field |
| 1031 | ND-500/5000 CPU number 2 data field |
| 1032 | ND-500/5000 CPU number 3 data field |
| 1033 | ND-500/5000 CPU number 4 data field |
| 1034 | reserved, but not used |
| 1035 | reserved, but not used |
| 1036 | reserved, but not used |
| 1037 | reserved, but not used |
| 1102 | not used; previously: ECC disk controller 1, unit 0, bit-file semaphore |
| 1114 | not used; previously: ECC disk controller 4, unit 0, bit-file semaphore |
| 1120 | not used; previously: ECC disk controller 1, unit 1, bit-file semaphore |
| 1122 | not used; previously: ECC disk controller 1, unit 2, bit-file semaphore |
| 1124 | not used; previously: ECC disk controller 1, unit 3, bit-file semaphore |
| 1135 | not used; previously: Floppy disk controller 1, unit 3, bit-file semaphore |
| 1151 | not used; previously: Floppy disk controller 1, unit 0, bit-file semaphore |
| 1153 | not used; previously: Floppy disk controller 1, unit 1, bit-file semaphore |
| 1155 | not used; previously: Floppy disk controller 1, unit 2, bit-file semaphore |
| 1160 | not used; previously: Floppy disk controller 2, unit 3, bit-file semaphore |
| 1162 | not used; previously: Floppy disk controller 2, unit 0, bit-file semaphore |
| 1164 | not used; previously: Floppy disk controller 2, unit 1, bit-file semaphore |
| 1166 | not used; previously: Floppy disk controller 2, unit 2, bit-file semaphore |
| 1172 | not used; previously: ECC disk controller 4, unit 2, bit-file semaphore |
| 1226 | STC magnetic tape controller 4, unit 1, I/O data field |
| 1230 | STC magnetic tape controller 4, unit 3, I/O data field |
| 1233 | STC magnetic tape controller 3, unit 1, I/O data field |
| 1235 | STC magnetic tape controller 3, unit 3, I/O data field |
| 1301 | not used; previously: ECC disk controller 4, unit 3, bit-file semaphore |
| 1334 | not used; previously: ECC disk controller 2, unit 0, bit-file semaphore |

---

## Page 61

# SINTRAN III Release Information, M-version

## Disk Controller and Directory Semaphore Information

| Number | Description |
|--------|-------------|
| 1336 | - not used; previously: ECC disk controller 2, unit 1, bit-file semaphore |
| 1340 | - not used; previously: ECC disk controller 2, unit 2, bit-file semaphore |
| 1342 | - not used; previously: ECC disk controller 2, unit 3, bit-file semaphore |
| 1733 | ST-506 (Winchester) disk controller 1, unit 1, directory semaphore |
| 1734 | - not used; previously: ST-506 disk controller 1, unit 1, directory semaphore |
| 1735 | - not used; previously: ST-506 disk controller 1, unit 1, bit-file semaphore |
| 1740 | ST-506 (Winchester) disk controller 2, unit 1, directory semaphore |
| 1741 | - not used; previously: ST-506 disk controller 2, unit 1, directory semaphore |
| 1742 | - not used; previously: ST-506 disk controller 2, unit 1, bit-file semaphore |

## Directory Entries

| Number | Description |
|--------|-------------|
| 2501 | Directory entry number 1, directory semaphore |
| 2502 | Directory entry number 2, directory semaphore |
| 2503 | Directory entry number 3, directory semaphore |
| 2504 | Directory entry number 4, directory semaphore |
| 2505 | Directory entry number 5, directory semaphore |
| 2506 | Directory entry number 6, directory semaphore |
| 2507 | Directory entry number 7, directory semaphore |
| 2510 | Directory entry number 8, directory semaphore |
| 2511 | Directory entry number 9, directory semaphore |
| 2512 | Directory entry number 10, directory semaphore |
| 2513 | Directory entry number 11, directory semaphore |
| 2514 | Directory entry number 12, directory semaphore |
| 2515 | Directory entry number 13, directory semaphore |
| 2516 | Directory entry number 14, directory semaphore |
| 2517 | Directory entry number 15, directory semaphore |
| 2520 | Directory entry number 16, directory semaphore |
| 2521 | Directory entry number 17, directory semaphore |
| 2522 | Directory entry number 18, directory semaphore |
| 2523 | Directory entry number 19, directory semaphore |
| 2524 | Directory entry number 20, directory semaphore |
| 2525 | Directory entry number 21, directory semaphore |
| 2526 | Directory entry number 22, directory semaphore |
| 2527 | Directory entry number 23, directory semaphore |
| 2530 | Directory entry number 24, directory semaphore |
| 2531 | Directory entry number 25, directory semaphore |
| 2532 | Directory entry number 26, directory semaphore |
| 2533 | Directory entry number 27, directory semaphore |
| 2534 | Directory entry number 28, directory semaphore |
| 2535 | Directory entry number 29, directory semaphore |
| 2536 | Directory entry number 30, directory semaphore |
| 2537 | Directory entry number 31, directory semaphore |
| 2540 | Directory entry number 32, directory semaphore |
| 2541 | Directory entry number 33, directory semaphore |
| 2542 | Directory entry number 34, directory semaphore |
| 2543 | Directory entry number 35, directory semaphore |
| 2544 | Directory entry number 36, directory semaphore |
| 2545 | Directory entry number 37, directory semaphore |
| 2546 | Directory entry number 38, directory semaphore |
| 2547 | Directory entry number 39, directory semaphore |
| 2550 | Directory entry number 40, directory semaphore |
| 2551 | Directory entry number 41, directory semaphore |

---

## Page 62

# SINTRAN III Release Information, M-version

| Entry Number | Description |
|--------------|-------------|
| 2552 | Directory entry number 42, directory semaphore |
| 2553 | Directory entry number 43, directory semaphore |
| 2554 | Directory entry number 44, directory semaphore |
| 2555 | Directory entry number 45, directory semaphore |
| 2556 | Directory entry number 46, directory semaphore |
| 2557 | Directory entry number 47, directory semaphore |
| 2560 | - not used; previously: directory entry no. 24, directory semaphore |
| 2561 | - not used; previously: directory entry no. 24, bit-file semaphore |
| 2562 | - not used; previously: directory entry no. 25, directory semaphore |
| 2563 | - not used; previously: directory entry no. 25, bit-file semaphore |
| 2564 | - not used; previously: directory entry no. 26, directory semaphore |
| 2565 | - not used; previously: directory entry no. 26, bit-file semaphore |
| 2566 | - not used; previously: directory entry no. 27, directory semaphore |
| 2567 | - not used; previously: directory entry no. 27, bit-file semaphore |
| 2570 | - not used; previously: directory entry no. 28, directory semaphore |
| 2571 | - not used; previously: directory entry no. 28, bit-file semaphore |
| 2572 | - not used; previously: directory entry no. 29, directory semaphore |
| 2573 | - not used; previously: directory entry no. 29, bit-file semaphore |
| 2574 | - not used; previously: directory entry no. 30, directory semaphore |
| 2575 | - not used; previously: directory entry no. 30, bit-file semaphore |
| 2576 | - not used; previously: directory entry no. 31, directory semaphore |
| 2577 | - not used; previously: directory entry no. 31, bit-file semaphore |

## DMA Device Buffer Header Semaphores

| Header Number | Description |
|---------------|-------------|
| 2600 | DMA device buffer header semaphore for header number 100₈ |
| 2601 | DMA device buffer header semaphore for header number 101₈ |
| 2602 | DMA device buffer header semaphore for header number 102₈ |
| 2603 | DMA device buffer header semaphore for header number 103₈ |
| 2604 | DMA device buffer header semaphore for header number 104₈ |
| 2605 | DMA device buffer header semaphore for header number 105₈ |
| 2606 | DMA device buffer header semaphore for header number 106₈ |
| 2607 | DMA device buffer header semaphore for header number 107₈ |
| 2610 | DMA device buffer header semaphore for header number 110₈ |
| 2611 | DMA device buffer header semaphore for header number 111₈ |
| 2612 | DMA device buffer header semaphore for header number 112₈ |
| 2613 | DMA device buffer header semaphore for header number 113₈ |
| 2614 | DMA device buffer header semaphore for header number 114₈ |
| 2615 | DMA device buffer header semaphore for header number 115₈ |
| 2616 | DMA device buffer header semaphore for header number 116₈ |
| 2617 | DMA device buffer header semaphore for header number 117₈ |
| 2620 | DMA device buffer header semaphore for header number 120₈ |
| 2621 | DMA device buffer header semaphore for header number 121₈ |
| 2622 | DMA device buffer header semaphore for header number 122₈ |
| 2623 | DMA device buffer header semaphore for header number 123₈ |
| 2624 | DMA device buffer header semaphore for header number 124₈ |
| 2625 | DMA device buffer header semaphore for header number 125₈ |
| 2626 | DMA device buffer header semaphore for header number 126₈ |
| 2627 | DMA device buffer header semaphore for header number 127₈ |
| 2630 | DMA device buffer header semaphore for header number 130₈ |
| 2631 | DMA device buffer header semaphore for header number 131₈ |
| 2632 | DMA device buffer header semaphore for header number 132₈ |
| 2633 | DMA device buffer header semaphore for header number 133₈ |
| 2634 | DMA device buffer header semaphore for header number 134₈ |

---

## Page 63

# SINTRAN III Release Information, M-version

| Number | Description |
|--------|-------------|
| 2635 | DMA device buffer header semaphore for header number 135 |
| 2636 | DMA device buffer header semaphore for header number 136₈ |
| 2637 | DMA device buffer header semaphore for header number 137₈ |
| 2640 | DMA device buffer header semaphore for header number 140₈ |
| 2641 | DMA device buffer header semaphore for header number 141₈ |
| 2642 | DMA device buffer header semaphore for header number 142₈ |
| 2643 | DMA device buffer header semaphore for header number 143₈ |
| 2644 | DMA device buffer header semaphore for header number 144₈ |
| 2645 | DMA device buffer header semaphore for header number 145₈ |
| 2646 | DMA device buffer header semaphore for header number 146₈ |
| 2647 | DMA device buffer header semaphore for header number 147₈ |
| 2650 | DMA device buffer header semaphore for header number 150₈ |
| 2651 | DMA device buffer header semaphore for header number 151₈ |
| 2652 | DMA device buffer header semaphore for header number 152₈ |
| 2653 | DMA device buffer header semaphore for header number 153₈ |
| 2654 | DMA device buffer header semaphore for header number 154₈ |
| 2655 | DMA device buffer header semaphore for header number 155₈ |
| 2656 | DMA device buffer header semaphore for header number 156₈ |
| 2657 | DMA device buffer header semaphore for header number 157₈ |
| 2660 | DMA device buffer header semaphore for header number 160₈ |
| 2661 | DMA device buffer header semaphore for header number 161₈ |
| 2662 | DMA device buffer header semaphore for header number 162₈ |
| 2663 | DMA device buffer header semaphore for header number 163₈ |
| 2664 | DMA device buffer header semaphore for header number 164₈ |
| 2665 | DMA device buffer header semaphore for header number 165₈ |
| 2666 | DMA device buffer header semaphore for header number 166₈ |
| 2667 | DMA device buffer header semaphore for header number 167₈ |
| 2670 | DMA device buffer header semaphore for header number 170₈ |
| 2671 | DMA device buffer header semaphore for header number 171₈ |
| 2672 | DMA device buffer header semaphore for header number 172₈ |
| 2673 | DMA device buffer header semaphore for header number 173₈ |
| 2674 | DMA device buffer header semaphore for header number 174₈ |
| 2675 | DMA device buffer header semaphore for header number 175₈ |
| 2676 | DMA device buffer header semaphore for header number 176₈ |
| 2677 | DMA device buffer header semaphore for header number 177₈ |

| Number | Description |
|--------|-------------|
| 3400 | BDIO pool 17 |
| 3401 | BDIO pool 18 |
| 3402 | BDIO pool 19 |
| 3403 | BDIO pool 20 |
| 3404 | BDIO pool 21 |
| 3405 | BDIO pool 22 |
| 3406 | BDIO pool 23 |
| 3407 | BDIO pool 24 |
| 3410 | BDIO pool 25 |
| 3411 | BDIO pool 26 |
| 3412 | BDIO pool 27 |
| 3413 | BDIO pool 28 |
| 3414 | BDIO pool 29 |
| 3415 | BDIO pool 30 |
| 3416 | BDIO pool 31 |
| 3417 | BDIO pool 32 |

---

## Page 64

# SINTRAN III Release Information, M-version

| Number | Description            |
|--------|------------------------|
| 3420   | BDIO pool 33           |
| 3421   | BDIO pool 34           |
| 3422   | BDIO pool 35           |
| 3423   | BDIO pool 36           |
| 3424   | BDIO pool 37           |
| 3425   | BDIO pool 38           |
| 3426   | BDIO pool 39           |
| 3427   | BDIO pool 40           |
| 3430   | BDIO pool 41           |
| 3431   | BDIO pool 42           |
| 3432   | BDIO pool 43           |
| 3433   | BDIO pool 44           |
| 3434   | BDIO pool 45           |
| 3435   | BDIO pool 46           |
| 3436   | BDIO pool 47           |
| 3437   | BDIO pool 48           |
| 3440   | BDIO pool 49           |
| 3441   | BDIO pool 50           |
| 3442   | BDIO pool 51           |
| 3443   | BDIO pool 52           |
| 3444   | BDIO pool 53           |
| 3445   | BDIO pool 54           |
| 3446   | BDIO pool 55           |
| 3447   | BDIO pool 56           |
| 3450   | BDIO pool 57           |
| 3451   | BDIO pool 58           |
| 3452   | BDIO pool 59           |
| 3453   | BDIO pool 60           |
| 3454   | BDIO pool 61           |
| 3455   | BDIO pool 62           |
| 3456   | BDIO pool 63           |
| 3457   | BDIO pool 64           |
| 3460   | Reserved for future use |
| 3461   | Reserved for future use |
| 3462   | Reserved for future use |
| 3463   | Reserved for future use |
| 3464   | Reserved for future use |
| 3465   | Reserved for future use |
| 3466   | Reserved for future use |
| 3467   | Reserved for future use |
| 3470   | Reserved for future use |
| 3471   | Reserved for future use |
| 3472   | Reserved for future use |
| 3473   | Reserved for future use |
| 3474   | Reserved for future use |
| 3475   | Reserved for future use |
| 3476   | Reserved for future use |
| 3477   | Reserved for future use |

---

## Page 65

# SINTRAN III Release Information, M-version

## 14. Terminal Input/Output

The default size of the terminal buffers has been increased:

The default size of the terminal input buffer is now 152 characters and the default size of the terminal output buffer is 256 characters.

The layout of the terminal data fields is also changed slightly. The new data field layout is shown on pages 73-85.

---

## Page 66

# 15. Nucleus

If MON EVENT (MON 352) is used, the event mask in NUCLEUS is now used as an event mask when restarting the RT-program.

---

## Page 67

# 16. ERS/SINTRAN III Watchdog

## 16.1 Introduction

The ERS/SINTRAN III Watchdog is a system for handling and reporting SINTRAN III error messages. The system consists of three programs and an error message descriptor file.

The programs are:

- The RT-program ERS3WD (the watchdog itself).
- The log-list program used to list all the latest messages collected.
- A manager program used to set parameters for the watchdog.

## 16.2 General concepts

The following terms are used in this chapter:

| Term | Description |
|------|-------------|
| SSI  | Subsystem Identification, a number uniquely identifying a product. |
| EC   | Event Code (or Error Code), a number identifying an event in a product. |
| SEC  | Standard Event Code (16 bits), the SSI and the EC combined. The SEC identifies both the (failing) product and the event which has occurred. |

Example:

- `SSI = 1061B 'FTX - Disk Mirroring'`
- `EC  = 15B 'Page outside limits'`
- `SEC = 106115B`

## 16.3 The watchdog program

The RT-program ERS3WD handles error reports sent via or by SINTRAN III. ERS3WD receives these reports from SINTRAN on the internal device with logical device number 2768. The SEC parameter in every report specifies which system module has issued the report and which error has occurred.

A received report is in numeric format. ERS3WD converts the report to text format and writes it to the error device.

To do this conversion ERS3WD needs some formatting information, the so-called "report descriptors". This information is already contained in ERS3WD's data segment at installation. The report descriptors are also supplied as the file `ER-S3WD-DESC-Dxx:EDAT` to make it possible to update the descriptors more frequently than ERS3WD is revised. If this file exists on the user areas SYSTEM or ND-OPERATIONS, ERS3WD will substitute this file's descriptors for its own descriptors at startup.

ERS3WD also writes the numerically formatted report to its own log file. This is the file `(SYSTEM)ER-S3WD-LOG:DATA`, which is created by ERS3WD at startup if it does not already exist. The log file is created as a contiguous 11-page (default size) file and ERS3WD treats it as a ring file. This means that ERS3WD will start writing again at the beginning of the file after reaching the end. The default size of the log file (11 pages) is sufficient to keep 100-800 reports (depending on the size of the reports). The file may be expanded as described on page 59.

---

## Page 68

# 16.4 The log-list program

The Log-List program reads the log file produced by ERS3WD and formats it into text messages. The Log-list program depends on finding and reading the descriptor file ER-S3WD-DESC-Dxx:EDAT, either on the user area ND-OPERATIONS or on SYSTEM.

ERS3WD writes to the file (SYSTEM)ER-S3WD-LOG:DATA, and this is the default input file to the Error Log Lister. Output can be directed to a file or to the terminal, default output is the file ER-S3WD-LOG:OUT. The size of the output file depends on the number of reports and the size of each report. Generally, the output file will require three times the size of the log file.

It is possible to clear the log file when reading it, but this is not necessary as the log file is circular as explained above.

# 16.5 The manager program

The manager program is used to set parameters for the watchdog. It can be run only from user areas RT or SYSTEM.

The following commands are available:

| Command | Description |
| --- | --- |
| REPORT-WRAPAROUND `<Confirmation>` | Report when the log file is nearly full (approaching time when the oldest reports are overwritten). Initial state is off. |
| REQUEST-STATUS | Request the SINTRAN III Watchdog to report its status. |
| SELECT-ERRORDEVICE-PARAMETERS `<Severity level>` `<Write parameter list?>` | Select messages to be written to error device. |
| SELECT-SUPPRESS-PARAMETERS `<Number of reports>` `<Time limit>` | Set parameters for suppressing identical reports. |
| SELECT-WRITE-PARAMETERS `<Write to error device?>` `<Write to log file?>` | Set parameters for writing reports. Turning off writing to the log file implies closing the log file. Turning on writing to the log file implies opening the log file. Note that it is not possible to turn off both reporting to the error device and writing to the log file. In such cases, the SINTRAN III Watchdog should be stopped. |
| START-SINTRAN-WATCHDOG | Start the SINTRAN III Watchdog. This implies opening the log file. |
| STOP-SINTRAN-WATCHDOG `<Confirmation>` | Stop the SINTRAN III Watchdog. This implies closing the log file. |
| EXIT | Exit from the SINTRAN III Watchdog Manager Program. |
| HELP `<Command>` | List all (or matching) commands. |
| CC `<Comment>` | Available for comments used in mode files. |

---

## Page 69

# SINTRAN III Release Information, M-version

## 16.6 The error message descriptor file

The error message descriptor file must reside either on user area ND-OPERATIONS or on SYSTEM. It is called ER-S3WD-DESC-Dxx:EDAT.

If the file is found on both user areas, the one on ND-OPERATIONS is used. The file is installed on user area SYSTEM by the NEW-SYSTEM program during installation of SINTRAN. This means that if you move it to the user area ND-OPERATIONS, you must be careful to copy any new revisions of the descriptor file installed later. Old versions (ER-S3WD-DESC-Cxx:EDAT) are not used and should be deleted.

## 16.7 Expanding the log file

If the number of reported events is expected to be very high, it may be considered necessary to expand the log file.

The following procedure is recommended:

- Start the SINTRAN Watchdog Manager Program and close the log file:
  - SELECT-WRITE-PARAMETERS Yes No

- Then EXIT from the Manager program.

- Expand the file, or, if this is not possible, delete and recreate it. Note that the maximum size of the log file is 20000 pages.
  - `@EXPAND-FILE ER-S3WD-LOG:DATA,<number of pages>`
  - or: `@DELETE-FILE ER-S3WD-LOG:DATA`
  - `@CREATE-FILE ER-S3WD-LOG:DATA,<number of pages>`

- Use the SINTRAN Watchdog Manager program to re-open the log file:
  - SELECT-WRITE-PARAMETERS Y/N Yes

- When ERS3WD takes the new pages in use, all the previous reports in the log file are lost. The number of data pages in use as ring file is one less than the number of pages in the log file.

## 16.8 Increasing the buffer size of the internal device

If the rate of reported events is very high (in a period), reports may be produced faster than they are read from the internal device. SINTRAN III will detect if there is not enough room for a new report and, if so, will generate a report with SEC = 1666₈ instead. If writing to the error device is enabled, this report will be printed as one of the messages "Internal device for error messages is full" or "Temporary overflow in internal device for error messages detected. At least one report was lost".

This situation is not serious, but reports produced when there is not enough space for them in the internal device, are lost. It may thus be necessary to increase the buffer size of the internal device with device number 276₈.

---

## Page 70

# Report Format

The following procedure is recommended:

- Start the SINTRAN III Service Program and use the command:
  
  `*CHANGE-BUFFER-SIZE 276B,I,<new buffer size in words>,Y,Y`

  to change the buffer size. Initial buffer size is 1024 words. Maximum allowed buffer size is 32767 words.

- A warm start is required to bring the change into effect.

## 16.9 Report Format

This is the layout of a report written to the error device:

```
severity * SSI.EC * date time * RT-program.P-register * systemname.systemnumber
  product name
  event text
  description parameter
  description parameter
  ...
  ...
  ...
```

| **Field**       | **Description**                                                                 |
|-----------------|---------------------------------------------------------------------------------|
| severity        | the severity of the reported event (Information/Warning/Error/Fatal)            |
| SSI             | SSI code of the event (octal number)                                            |
| EC              | Event Code of the event (octal number)                                          |
| date            | the date when the event was read by the watchdog (on the form YYYY-MM-DD)       |
| time            | the time when the event was read by the watchdog (on the form HH:MM:SS)         |
| RT-program      | the name (or RT-description address) of the RT-program causing the event        |
| P-register      | the P-register (program counter) of this RT-program                             |
| systemname      | the system name (as defined in XMSG)                                            |
| systemnumber    | the system number (as defined in XMSG)                                          |
| product name    | the name of the system module corresponding to the SSI                          |
| event text      | the event text corresponding to the EC                                          |
| description     | a description of the following parameter value                                  |
| parameter       | parameter value                                                                 |

## 16.10 Suppression

When a process reporting to ERS3WD loops and sends a stream of identical error messages, it is often desirable that "excess" messages are suppressed.

If the same event report appears more than 10 times in succession, the following identical event reports will be suppressed until a different event arrives or the time between events exceeds 15 seconds. These two values may be modified by the `SELECT-SUPPRESS-PARAMETERS` command in the Watchdog Manager program.

When the suppression starts, a message about this will be given. When the logging of event reports is resumed, ERS3WD reports the number of reports that have been suppressed, and then the new report.

---

## Page 71

# 16.11 Messages from the watchdog itself

ERS3WD indicates a few situations by writing messages both to the error device and the log file. The messages are:

### 1170B.01B ERS/SINTRAN III Watchdog has started

### 1170B.02B ERS/SINTRAN III Watchdog has stopped

### 1170B.03B Cannot reserve internal device

The internal device 276₆ could not be reserved.  
This is a fatal error for ERS3WD, and it will subsequently stop.  
Use the command START-SINTRAN-WATCHDOG in the Watchdog Manager program to restart.

### 1170B.04B Descriptor file could not be opened or was not found on ND-OPERATIONS / SYSTEM

The descriptor file could not be found or accessed. The file was searched for on both the user areas ND-OPERATIONS and SYSTEM. ERS3WD continues and will use its initial descriptors.

### 1170B.06B Log file could not be opened/created

The log file (SYSTEM)ER-S3WD-LOG:DATA could not be created or opened.  
By default it is an 11-page contiguous file.

### 1170B.07B Further reports on this event will be suppressed

**Event:**  
If the same event report appears more than 10 times in succession, the following identical event reports will be suppressed until a different event arrives or the time between the last and previous event exceeds 15 seconds. These two values may be modified by the command SELECT-SUPPRESS-PARAMETERS in the Watchdog Manager program.

### 1170B.10B The previous report(s) has been suppressed

**Number of reports suppressed:**  
This message is written at the end of a suppression period.

### 1170B.11B Descriptor file too big

The descriptor file is larger than ERS3WD's data segment. ERS3WD continues and will use its initial descriptors.

### 1170B.12B Descriptor file too small/corrupted

The descriptor file is found to be too small.  
It must at least contain 2049 bytes. ERS3WD continues and will use its initial descriptors.

---

## Page 72

# 1170B.13B Error when reading descriptor file

A file system error occurred while ERS3WD was substituting the descriptor file data for its own initial descriptors.  
This is a fatal error for ERS3WD, and it will subsequently stop.  
Use the command START-SINTRAN-WATCHDOG in the Watchdog Manager program to restart.

# 1170B.14B Protocol error on internal device
## The process ERS3WD may need a restart
## Number of bytes skipped:

The byte string that was received on the internal device was not recognised by ERS3WD as a legal protocol. ERS3WD will continue to read from the internal device and skip over bytes until a byte string is found that seems to be a legal protocol.  
Eventually, message 1170B.15B will be produced, and then a (presumably correct) new report. Obviously this report (and perhaps the ones following) may be nonsense.  
To control the situation, it is recommended that you use the command REQUEST-STATUS in the Watchdog Manager program until you get the correct response from ERS3WD. If ERS3WD does not respond to this treatment, you should restart the process.

To restart ERS3WD use the Watchdog Manager program commands:  
STOP-SINTRAN-WATCHDOG and START-SINTRAN-WATCHDOG

If this does not work, do:  
@ABORT ERS3WD  
@RTCLOSE (on ER-S3WD-LOG:DATA)  
@RT ERS3WD

# 1170B.15B Attempted correction of the protocol error
## Total number of bytes skipped:

See the previously described situation 1170B.14B.

# 1170B.16B Logging reports to the log file is disabled

The following lines of this message will tell you why.  
When the reason for this message is cleared, you may enable logging to the log file again by the command SELECT-WRITE-PARAMETERS in the Watchdog Manager program.

# 1170B.17B Unexpected values returned from Nucleus
## Status:
## Received message :

If this happens it indicates overwriting, and a restart is recommended.  
(See 1170B.14B about how to restart ERS3WD.)

---

## Page 73

# 1170B.20B The log file is almost full

Free space before wraparound (in per cent)...

Number of reports in the log file......

Number of data pages in use as ring file......

This message will be given only if the command REPORT-WRAPAROUND (with parameter YES) has been used in the Watchdog Manager program. Then, when there is less than 20% free space left before ERS3WD will start overwriting the first reports in the log file, this warning will be given. The warning is significant only for those who want to take care of (in some way or the other) all reports in the log file before any of them are overwritten. If you find the log file too small for your intention, you may expand it as described on page 59.

# 1170B.21B The log file could not be used

It must be a contiguous file with at least 2 pages

Self-explanatory.

# 1170B.22B Received command: 

For security reasons some of the commands ERS3WD receives from the Watchdog Manager program are reported.

# 1170B.23B Received command with illegal parameter(s) 

For security reasons this situation is reported.

# 1170B.24B Inconsistence found in the log file

The report pointers are reset

This message will be given if the watchdog detects an inconsistency in the log file (for example due to overwrite by another program or a system crash during update). Logging will be restarted and no user/supervisor action is required.

---

## Page 74

# 16.12 Reporters recognised by the SINTRAN III watchdog

This list may be expanded by the installation of a revision of the descriptor file later than ER-S3WD-DESC-D02:EDAT.

| SEC       | System Module Name                                          |
|-----------|-------------------------------------------------------------|
| 000000B - 000377B | SINTRAN III File System                               |
| 001000B - 001077B | ND-500/5000 System Monitor                            |
| 001400B - 001677B | SINTRAN III Runtime System                            |
| 002000B - 002177B | ND-500/5000 Monitor Internal                          |
| 002200B - 002277B | ACCP/Microprogram                                     |
| 002300B - 002377B | ND-500/5000 Monitor Internal                          |
| 003200B - 003277B | SINTRAN III File System                               |
| 006000B - 006077B | Domino Operating System                               |
| 006200B - 006277B | Domino Services                                       |
| 007000B - 007077B | ACCP process in the ND-5850 Service Partner (James)   |
| 007600B - 007677B | ND-500/5000 Traps                                     |
| 040500B - 040577B | COSMOS Remote file access                             |
| 041000B - 041077B | XMSG                                                 |
| 041200B - 041277B | XMSG Watchdog (XMFIDO)                                |
| 041300B - 041377B | XMSG XROUT                                           |
| 044500B - 044577B | PO-LIB Service Point System                           |
| 044600B - 044677B | Ethernet Media Access                                 |
| 047200B - 047277B | AIP - ARPA Internet Protocol                          |
| 047300B - 047377B | TCP/IP - Transmission Control Protocol / Internet Protocol |
| 047400B - 047477B | SLIB - Socket Library                                 |
| 047600B - 047677B | Telnet Server                                        |
| 101000B - 101077B | Nucleus                                               |
| 101100B - 101177B | Nucleus Operations                                    |
| 101200B - 101277B | MTAD Server                                          |
| 101400B - 101477B | Octobus driver                                       |
| 101500B - 101577B | MF-bus Controller                                    |
| 101600B - 101677B | Power Supply Server                                  |
| 104000B - 104277B | BDIO - Basic Disk I/O                                 |
| 104500B - 104577B | SCSI domino driver                                    |
| 104600B - 104677B | SCSI domino device level                              |
| 104700B - 104777B | SCSI domino tape access                               |
| 105000B - 105077B | PROMAN - Processor Manager                            |
| 105300B - 105377B | SCSI domino device level                              |
| 106100B - 106177B | FTX Disk Mirroring                                    |
| 106200B - 106277B | FTX Disk Mirroring Revive                             |
| 117000B - 117077B | ERS/SINTRAN III Watchdog                              |
| 142000B - 142077B | Superkernel                                           |
| 142200B - 142277B | SIBAS Access Server                                   |

The following reporters were supported by the D00 version of the description file, but are not supported by the D02 version:

|           |       |
|-----------|-------|
| 117100B - 117177B | ERS Library   |
| 117200B - 117277B | ERS Router    |
| 117300B - 117377B | ERS Receiver  |
| 117400B - 117477B | ERS Formatter |
| 117500B - 117577B | ERS Configuration Server |

---

## Page 75

# 17. SINTRAN III M-version, System Layout

## 17.1 System layout on disk

| File             | Contents                        | Disk address | Size  | Segment address |
|------------------|---------------------------------|--------------|-------|-----------------|
| SINTRAN:DATA     | Common Code Restart/Start       | 1B           | 77B   | 0B              |
| MACM-AREA:DATA   | Error Messages RT-Loader        | 100B 137B    | 20B 41B | 30000B 30000B   |
| SEGFILE0:DATA    | Common Code Restart/Start       | 200B         | 77B   | 30000B          |
|                  | Resident Data                   | 300B         | 55B   | 4000B           |
|                  | System Segment                  | 355B         | 3B    | 144000B         |
|                  | Spooling Data Fields            | 360B         | 1B    | 164000B         |
|                  | Extended COMMON                 | 361B         | 2B    | 26000B          |
|                  | RPIT                             | 363B         | 63B   | 32000B          |
|                  | MPIT                             | 446B         | 63B   | 32000B          |
|                  | IPIT                             | 531B         | 63B   | 32000B          |
|                  | 5PIT                             | 614B         | 5B    | 26000B          |
|                  | ND-500/5000 System Monitor      | 621B         | 60B   | 40000B          |
|                  | Segment Table                   | 701B         | 20B   | 0B              |
|                  | ND-110 Micro Program            | 721B         | 40B   | 0B              |
|                  | ND-120 Micro Program            | 761B         | 40B   | 0B              |
|                  | File System                     | 1021B        | 65B   | 26000B          |
|                  | Command Segment                 | 1106B        | 65B   | 26000B          |
|                  | SSP/Mail Segment                | 1173B        | 44B   | 30000B          |
|                  | XMSG Kernel                     | 1237B        | 37B   | 102000B         |
|                  | XMSG XROUT Segment              | 1276B        | 41B   | 0B              |
|                  | XMSG Watchdog                   | 1337B        | 66B   | 0B              |
|                  | Device-name Table               | 1425B        | 6B    | 164000B         |
|                  | Disk Mirroring WD Segment       | 1433B        | 4B    | 2000B           |
|                  | NUCLEUS Server                  | 1437B        | 64B   | 30000B          |
|                  | NUCLEUS Name Server             | 1523B        | 100B  | 0B              |
|                  | ERS Watchdog Program            | 1623B        | 64B   | 0B              |
|                  | ERS Watchdog Data               | 1707B        | 70B   | 0B              |
|                  | Processor Manager Server        | 1777B        | 64B   | 30000B          |
|                  | PFTCON Server                   | 2063B        | 64B   | 30000B          |
|                  | BOPCOM Server                   | 2147B        | 64B   | 30000B          |
|                  | MT Server                       | 2233B        | 4B    | 30000B          |

---

## Page 76

# 17.2 Page index table layout

| PIT 0                           | PIT 1 - UPITN              | PIT 2 - UPITA               | PIT 3 - FUPIT              |
|--------------------------------|----------------------------|----------------------------|---------------------------|
| 0                              | 0                          | 0                          | 0                         |
| Only used during startup       | Users normal PIT           | Users alternate PIT        | Micro-©                   |
|                                |                            |                            | 2                         |
|                                |                            |                            | Common code (©)           |
|                                |                            |                            | 13                        |
|                                |                            |                            | Remote file user PIT      |

| PIT 4 - FPIT                   | PIT 5 - 5PIT               | PIT 6 - XPIT               | PIT 7 - DPIT              |
|--------------------------------|----------------------------|----------------------------|---------------------------|
| 0                              | 0                          | 0                          | 0                         |
| Micro-©                        | Micro-©                    | Micro-©                    | Micro-©                   |
| 2                              | 2                          | 2                          | 2                         |
| Common code (©)                | Common code (©)            | Common code (©)            | Resident common data      |
| 13                             | 13                         | 13                         | 57                        |
| File system segment            | MON 60                     | XMSG                       | Wind.BF                   |
| 20                             |                            |                            | Wind.ND-500               |
|                                | ND-500(0) system segment   |                            | Wind.1/4                  |
|                                |                            |                            | Sys. segm.                |
|                                |                            |                            | Wnd.10/12                 |
|                                |                            |                            | 62                        |
|                                |                            |                            | Data segm.                |
|                                |                            |                            | 72                        |

---

## Page 77

# PIT 10 - RPIT

|   |          |
|---|----------|
| 0 | Micro-©  |
| 2 | Common code (©) |
|13 | Extended common(©) |
|15 | Monitor calls |
|   | B-level (level 4) |

# PIT 11 - SPIT

|   |                  |
|---|------------------|
| 0 | Micro-©          |
| 2 | Common code (©)  |
|13 | Edit segment     |
|14 | Command segment  |
|   | SSP/Mail segment |
|   | RT-Loader        |
|   | DMAC             |
|   | Error prog.      |

# PIT 12 - MPIT

|   |                  |
|---|------------------|
| 0 | Micro-©          |
| 2 | Common code (©)  |
|13 | Extended common(©) |
|15 | Level 2          |
|   | Level 10         |
|   | Level 12         |
|   | Level 13         |
|   | Level 14         |
|   | MPERF            |

# PIT 13 - X5DP1

|             |                   |
|-------------|-------------------|
| 0           | ND-500(0) name-   |
|              | tables segment   |
|             |                   |
|             | Stack wnd.        |

# PIT 14 - X5DP2

|             |                            |
|-------------|----------------------------|
| 0           | ND-500(0) standard domains |
|             | segment                    |
|             |                            |
|             | Stack wnd.                 |

# PIT 15 - IPIT

|   |                  |
|---|------------------|
| 0 | Micro-©          |
| 2 | Common code (©)  |
|13 | Extended common(©) |
|15 | Level 3          |
|   | Level 11         |

# PIT 16

| | |
|-|-|
|0| |

# PIT 17 - DTPIT

|   |                                     |
|---|-------------------------------------|
| 0 | Direct tasks                        |
|   | (Used for mapping DPIT during startup) |

---

## Page 78

# 17.3 System Included Segments

| Segment No. | Name   | Address Range  | PIT | Description                                     |
|-------------|--------|----------------|-----|-------------------------------------------------|
| 2           | S3IMAGE | 0:175777       | 1   | Image of common code, start/restart             |
| 3           | S3CP   | 30000:177777   | 11  | Command segment                                 |
| 4           | S3RTL  | 30000:123777   | 11  | RT-Loader segment                               |
| 5           | S3ERFS | 144000:145777  | 7   | System segment for error program                |
| 6           | S3FS   | 26000:177777   | 4   | File system segment                             |
| 7           | S3DMAC | 64000:153777   | 11  | DMAC segment                                    |
| 10          | S3RTFIL| 0:177777       | 2   | RTFIL segment                                   |
| 11          | S3ERBL | 0:177777       | 1   | Error log segment                               |
| 12          | S3SFS  | 26000:177777   | 1   | Save of file system segment                     |
| 13          | S3SCP  | 26000:177777   | 1   | Save of command segment                         |
| 14          | S3ERRP | 30000:67777    | 11  | Error program segment                           |
| 15          | S3BFLY | 26000:26000    |     | Reserved for Butterfly                          |
| 16          | S3SRPIT| 32000:177777   | 1   | Save of RPIT                                    |
| 17          | S3SMPIT| 32000:177777   | 1   | Save of MPIT                                    |
| 20          | S3SDT5 | 0:175777       | 14  | ND-500/5000 standard domains seg.               |
| 21          | S3NM5  | 0:175777       | 13  | ND-500/5000 name-tables segment                 |
| 22          | S3RFAC | 26000:171777   | 3   | Remote file access segment                      |
| 23          | S3DPIT | 4000:135777    | 7   | DPIT segment                                    |
| 24          | S3SSGT | 0:37777        | 1   | Save of segment table                           |
| 25          | S3IRPIT| 32000:177777   | 1   | Image of RPIT                                   |
| 26          | S3IMPIT| 32000:177777   | 1   | Image of MPIT                                   |
| 27          | S3ISGT | 0:37777        | 1   | Image of segment table                          |
| 30          | S3SM5  | 40000:177777   | 5   | ND-500/5000 System Monitor segm.                |
| 31          | S3SSPD | 164000:165777  | 7   | Save of spooling data fields                    |
| 32          |        |                |     | Reserved, but not used                          |
| 33          |        |                |     | Reserved, but not used                          |
| 34          |        |                |     | Reserved, but not used                          |
| 35          | S3MPIT | 32000:157777   | 12  | MPIT segment                                    |
| 36          | S3TAD  | 110000:133777  | 11  | TADADM segment                                  |
| 37          | S3RTD  | 0:177777       | 1   | RT-Loader data segment                          |
| 40          | S3FUDRT| 164000:173777  | 7   | File user data segment for RT prog.             |
| 41          | S3IMED | 26000:27777    | 1   | Image of edit routines                          |
| 42          | S3ED   | 26000:27777    | 11  | Edit routines                                   |
| 43          | S3PATCH| 174000:177777  | 2   | Used for patching purposes                      |
| 44          | S3IDPIT| 4000:135777    | 1   | Image of DPIT                                   |
| 45          | S3ISYS | 144000:151777  | 1   | Image of system segment                         |
| 46          | S3SPIT | 26000:37777    | 1   | Save of 5PIT segment                            |
| 47          | S3RPIT | 32000:143777   | 10  | RPIT segment                                    |
| 50          | S3ISPIT| 26000:37777    | 1   | Image of 5PIT segment                           |
| 51          | S3SPIT | 26000:37777    | 5   | 5PIT segment                                    |
| 52          | S3SAVE | 0:175777       | 1   | Save of common code & start/restart             |
| 53          | S3SDPIT| 4000:135777    | 1   | Save of DPIT                                    |
| 54          | S3SYS  | 144000:151777  | 1   | Save of system segment                          |
| 55          | S3SERRP| 30000:67777    | 1   | Save of error program                           |
| 56          | S3SRTC | 30000:67777    | 1   | Save of RT-Loader code segment                  |
| 57          | S3SRTD | 0:25777        | 1   | Save of RT-Loader data segment                  |
| 60          | S3SECM | 26000:317777   | 1   | Save of extended common                         |

---

## Page 79

# SINTRAN III Release Information, M-version

## Segment Information

| Segment No. | Name     | Address Range | PIT | Description                           |
|-------------|----------|---------------|-----|---------------------------------------|
| 61          | S3IECOM  | 26000:31777   | 1   | Image of extended common              |
| 62          | S3SSM5   | 40000:177777  | 1   | Save of ND-500/5000 System Monitor    |
| 63          | S3MEMTF  | 172000:172000 |     | MEMTOF segment                        |
| 64          | S3ECOM   | 26000:31777   | 10  | Extended common segment               |
| 65          | S3SIPIT  | 32000:177777  | 1   | Save of IPIT                          |
| 66          | S3IIPIT  | 32000:177777  | 1   | Image of IPIT                         |
| 67          | S3IPIT   | 32000:77777   | 15  | IPIT segment                          |
| 70          | S3SSM    | 30000:137777  | 1   | Save service/mail segment             |
| 71          | S3SM     | 30000:137777  | 11  | Service/mail segment                  |
| 72          | S3SMDWD  | 2000:11777    | 1   | Save of disk mirroring WD segment     |
| 73          | S3IDMWD  | 2000:11777    | 1   | Image of disk mirroring WD segment    |
| 74          | S3SXMK   | 102000:177777 | 1   | Save of XMSG kernel                   |
| 75          | S3SXROU  | 0:101777      | 1   | Save of XMSG XROUT segment            |
| 76          | S3XMK    | 102000:177777 | 2   | XMSG kernel                           |
| 77          | S3XROU   | 0:101777      | 2   | XMSG XROUT segment                    |
| 100         | S3SDNAM  | 164000:177777 | 1   | Save of device-name table             |
| 101         | SDNAM    | 164000:177777 | 7   | Device-name table                     |
| 102         | S3SXMFI  | 0:153777      | 1   | Save of XMSG watchdog (XMFIDO)        |
| 103         | S3XMFI   | 0:153777      | 1   | XMSG watchdog (XMFIDO)                |
| 104         | S3SNKSE  | 30000:177777  | 11  | Save of NUCLEUS server                |
| 105         | S3INKSE  | 30000:177777  | 11  | Image of NUCLEUS server               |
| 106         | S3SNKNA  | 0:177777      | 1   | Save of NUCLEUS name server           |
| 107         | S3INKNA  | 0:177777      | 1   | Image of NUCLEUS name server          |
| 110         | S3SU110  | 0:77777       | 1   | Save of ND-110 Microprogram           |
| 111         | S3IU110  | 0:77777       | 1   | Image of ND-110 Microprogram          |
| 112         | S3SU120  | 0:77777       | 1   | Save of ND-120 Microprogram           |
| 113         | S3IU120  | 0:77777       | 1   | Image of ND-120 Microprogram          |
| 114         | S3SERWC  | 0:147777      | 1   | Save of ERS Watchdog program          |
| 115         | S3IERWC  | 0:147777      | 1   | Image of ERS Watchdog program         |
| 116         | S3SERWD  | 0:157777      | 1   | Save of ERS Watchdog data             |
| 117         | S3IERWD  | 0:157777      | 2   | Image of ERS Watchdog data            |
| 120         | S3SPRMA  | 30000:177777  | 11  | Save of Processor Manager server      |
| 121         | S3IPRMA  | 30000:177777  | 11  | Image of Processor Manager server     |
| 122         | S3SPWRS  | 30000:177777  | 11  | Save of PFTCON server                 |
| 123         | S3IPWRS  | 30000:177777  | 11  | Image of PFTCON server                |
| 124         | S3SBOPC  | 30000:177777  | 11  | Save of BOPCOM Server                 |
| 125         | S3IBOPC  | 30000:177777  | 11  | Image of BOPCOM Server                |
| 126         | S3SMTSE  | 30000:37777   | 11  | Save of MT server                     |
| 127         | S3IMTSE  | 30000:37777   | 11  | Image of MT server                    |

---

## Page 80

# 17.4 System included RT-programs

| Program | Purpose |
|---------|---------|
| 1SWAP | Queueing program requests for swapping |
| 5SWAP | Performs ABSTR in ND-100 for the ND-500/5000 Swapper |
| ACCRT | RT accounting |
| BAKnn | Background process for terminal (BAK01-BAK99) |
| BKnnn | Background process for terminal (BK100-BK128) |
| BCHnn | Batch process |
| BPTMP | Timeout program for background allocation system |
| COSPO | COSMOS-spooling server |
| DUMM2 | Dummy program used by the spooling system |
| DUMMY | Dummy program to prevent empty execution queue |
| FDRT1 | Transfer data between interface buffer and memory. Floppy formatting. (FLOPPY-1) |
| FDRT2 | Transfer data between interface buffer and memory. Floppy formatting. (FLOPPY-2) |
| FIXRT | Monitor call/command FIXC execution |
| RTDIL | Buffer transfer program for DISC-ACCESS-LOG |
| RTER | Output error messages |
| RTRFA | Does remote file access for RT-programs (COSMOS - remote file access) |
| RTSLI | Time slicer. Changes priority on all time sliced processes. |
| RTREC | Process to reconnect SINTRAN file system directory to DOMINO controller (after re-boot of DOMINO or when BDIO switch to mirror pool). |
| RWRT1 | Block data transfer. Activated from RFILE/EWFILE/RPAGE/WPAGE for RT-programs |
| RWRT2 | Open file from RT-programs |
| RWRT3 | Block transfer on MAG-TAPE-1 (MAGTP) |
| RWRT5 | Block transfer on VERSATEC-1 DMA |
| RWRT6 | Block transfer on CDC-DMA LINK |
| RWRT7 | Block transfer on MAG-TAPE-2 |
| RWRT8 | Block transfer on VERSATEC-2 DMA |
| RWRT9 | Block transfer on FLOPPY-DISC 1 |
| RWRT10 | Block transfer on FLOPPY-DISC 2 |
| RWRT11 | Block transfer on LINE-PRINTER/VERSATEC -1 I/O |
| RWRT12 | Block transfer on LINE-PRINTER/VERSATEC -2 I/O |
| RWRT13 | Block-oriented internal device 1 Input |
| RWRT14 | Block-oriented internal device 2 Input |
| RWRT15 | Block-oriented internal device 3 Input |
| RWRT16 | Block-oriented internal device 4 Input |
| RWRT17 | Block-oriented internal device 5 Input |
| RWRT20 | Block-oriented internal device 1 Output |
| RWRT21 | Block-oriented internal device 2 Output |
| RWRT22 | Block-oriented internal device 3 Output |
| RWRT23 | Block-oriented internal device 4 Output |
| RWRT24 | Block-oriented internal device 5 Output |
| RWRT25 | HASP DMA 1 Input |
| RWRT26 | HASP DMA 1 Output |
| RWRT27 | HASP DMA 2 Input |

---

## Page 81

# SINTRAN III Release Information, M-version

| Code   | Description                                                                      |
|--------|----------------------------------------------------------------------------------|
| RWRT28 | HASP DMA 2 Output                                                                |
| RWRT29 | HASP DMA 3 Input                                                                 |
| RWRT30 | HASP DMA 3 Output                                                                |
| RWRT31 | HASP DMA 4 Input                                                                 |
| RWRT32 | HASP DMA 4 Output                                                                |
| RWRT33 | HASP DMA 5 Input                                                                 |
| RWRT34 | HASP DMA 5 Output                                                                |
| RWRT35 | HASP DMA 6 Input                                                                 |
| RWRT36 | HASP DMA 6 Output                                                                |
| RWRT41 | Transfer on SCSI Streamer                                                        |
| RWRT42 | Open/close file on SCSI Streamer                                                 |
| SPRTn  | Spooling programs (1-9)                                                          |
| SPRnn  | Spooling programs (10-30)                                                        |
| STSIN  | Initialize SINTRAN III and start systems RT-programs                             |
| TADnn  | Background process for Terminal Access Device                                    |
| TADAD  | Administers connections to TADs from requesting users.                           |
| TERMP  | Starts the user defined "clean-up" RT-program when RT-programs are aborted (if enabled) |
| TIMRT  | Timer RT-program. Start timeout-routine for all devices in timer-table.          |
| UDRnn  | Performs Fast Universal DMA for user processes.                                  |
| DIMWD  | Used by the disk mirroring facility which is part of the Fault Tolerant eXtension (FTX).  |
| REVIVE | Used by the disk mirroring facility which is part of the Fault Tolerant eXtension (FTX).  |
| XROUT  | XMSG server                                                                      |
| XTRACE | XMSG server                                                                      |
| XMFIDO | XMSG Watchdog                                                                    |
| NKSERV | NUCLEUS server                                                                   |
| NKNAME | NUCLEUS name server                                                              |
| ERS3WD | ERS/SINTRAN III Watchdog                                                         |
| PROMAN | Process Manager Server                                                           |
| PFTCON | Power Supply Controller server                                                   |
| BOPCOM | BOPCOM Server                                                                    |
| MTSERV | NUCLEUS MTAD-server                                                              |

---

## Page 82

# 17.5 Changes to the RT-description

The "old" RT-description found in resident data (DPIT) is unchanged.

Further, the part outside resident data (DPIT) containing the register save area and reentrant segment bit-map is unchanged.

A new extended part of the RT-description is defined, the layout is:

### Displacement (octal)

|       |           |
|-------|-----------|
| 0     | EXSTS     | Extra status|
| 1     | EV1SET    | Events set (EVSET)|
| 2     | EV2SET    | |
| 3     | EV1WAIT   | Events waited for (EVWAIT)|
| 4     | EV2WAIT   | |
| 5     | EV1TMOUT  | Timeout events (EVTMOUT)|
| 6     | EV2TMOUT  | |
| 7     | EV1INTV   | Interval events (EVINTV)|
| 10    | EV2INTV   | |
| 11    | IN1TRV    | Interval (INTRV)|
| 12    | IN2TRV    | |
| 13    | NX1INT    | Next interval (NXINT)|
| 14    | NX2INT    | |
| 15    | EV1OEV    | Other events (EVOEV)|
| 16    | EV2OEV    | |
| 17    |           | Reserved for future use|
| 20    |           | Reserved for future use|
| 21    | XRTLINK   | Link in process start queue|
| 22    | INTQL     | Link in interval event queue|
| 23    | CPU5X     | Accumulated ND-500/5000 CPU time|
| 24    | CPU5Y     | |
| 25    | XCERR     | DMA error code|

This extended RT-description is located in the physical memory.

The physical memory bank is found in the variable XRTBA in DPIT.

The address within this bank, and position of each extended RT-description corresponds to the ordinary RT-description table in DPIT. (The start address of the RT-description table in DPIT, found in the variable RTSTART, is also the start address of the extended RT-descriptions, and the size of the DPIT-part and this extended part of the descriptions are the same.)

---

## Page 83

# Format of EXSTS:

- **5SEL:** Only events in EVWAI are returned and cleared
- **5CMP:** All events in EVWAI must be set before restart
- **5EVWT:** Process is waiting for events
- **5EVRS:** Process is restarted by SETEV
- **5EVNC:** Process is using the event system in NUCLEUS

```
| 17 | 16 | 15 | 14 | 13 | 12 | 11 | 10 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
```

## 17.6 Changed data fields - terminals / TAD / NOTS / MTAD

Changes compared to the L-version are marked with a change bar.

## 17.6.1 Terminal data field - DPIT part - SINTRAN memory area

This layout applies to both the input and output data fields.

### Displacement

| Displacement | Field       | Description                                          |
|--------------|-------------|------------------------------------------------------|
| -4           | TDFPHPAGE   | Physical page of data field                          |
| -3           | TDFLGADDR   | Address within a bank of data fields                 |
| -2           | STDRIV      | Driver start address                                 |
| -1           | DRIVER      | Driver interrupt restart address                     |
| 0            | RESLINK     | Reservation link                                     |
| 1            | RTRES       | Reserving RT-program                                 |
| 2            | BWLINK      | Beginning of waiting queue                           |
| 3            | TYPRING     | Device-type bits and ring                            |
| 4            | ISTATE      | 0 = active, 1 = I/O-wait, 2 = buffer wait, -1 & -2 = nowait |
| 5            | MLINK       | Monitor queue link                                   |
| 6            | MFUNC       | Monitor level function address                       |

---

## Page 84

# 17.6.2 Terminal input data field - non-DPIT part - memory

_Displacement (octal)_

| Displacement | Code    | Description                                                      |
|--------------|---------|------------------------------------------------------------------|
| -45          | TINFO   | Various information bits for terminal                            |
| -44          | PECH7   | Echo table                                                       |
| -34          | PBRK7   | Break table                                                      |
| -24          | IN5MSG  | Address of ND-500/5000 message in fast INSTRING                  |
| -23          | RSISTE  | Echo pointer                                                     |
| -22          | BRECHOFL| Break and echo flag                                              |
| -21          | ROUSPEC | Address of special subroutine                                    |
| -20          | NCBRK   | Number of characters after last break                            |
| -17          | CTTYP   | Terminal type                                                    |
| -16          | CESCP   | Disconnect and escape characters                                 |
| -15          | BRKMAX  | Maximum BHOLD before break                                       |
| -14          | TSPEED  | Terminal speed                                                   |
| -13          | CNTREG  | Control register                                                 |
| -12          | DFLAG   | Device flag bits                                                 |
| -11          | ECHOTAB | Pointer to echo table                                            |
| -10          | BRKTAB  | Pointer to break table                                           |
| -7           | LAST    | Last typed character                                             |
| -6           | TMSUB   | Timeout subroutine                                               |
| -5           | TMR     | Timeout counter                                                  |
| -4           | TTMR    | Start value of TMR                                               |
| -3           | HDEV    | Hardware device number                                           |
| -2           | STDRIV  | Driver start address                                             |
| -1           | DRIVER  | Driver interrupt restart address                                 |
| 0            | TDRADDR | Address of data field in resident                                |
| 1            | XDFOPP  | Address of DFOPP in resident                                     |
| 2            | XOPPDF  | Address of opposite data field (outside resident)                |
| 3            | TYPRING | Device-type bits and ring                                        |
| 4            | XONCR   | XON character, input control                                     |
| 5            | XOFCFR  | XOFF character, input control                                    |
| 6            | PDISPLAY| Pointer to next data field in display table                      |
| 7            | IOTRANS | Called from INBT/OUTBT to transfer                               |
| 10           | STDEV   | Start device routine                                             |

_To be continued_

---

## Page 85

# SINTRAN III Release Information, M-version

| Number | Name      | Description                                                              |
|--------|-----------|--------------------------------------------------------------------------|
| 11     | SETDV     | IOSET routine                                                             |
| 12     | DFOPP     | Pointer to output channel data field                                      |
| 13     | DERROR    | Error code                                                                |
| 14     | BUFST     | Start of ring buffer                                                      |
| 15     | MAX       | Buffer capacity                                                           |
| 16     | BHOLD     | Number of characters in buffer                                            |
| 17     | HENTE     | Fetch pointer                                                             |
| 20     | CFREE     | Free positions                                                            |
| 21     | FYLLE     | Store pointer                                                             |
| 22     | BSTATE    | Background program state                                                  |
| 23     |           | Reserved for future use                                                   |
| 24     | DBPROG    | Background RT-program                                                     |
| 25     | DBADR     | Saved P-register on escape and file system monitor calls                  |
| 26     | RIFIL     | Mode input file number                                                    |
| 27     | BCHISTS   | Mode input status                                                         |
| 30     | DER0      | Error information                                                         |
| 30     | BREGBLOCK | Register save at escape                                                   |
| 32     | DER2      | Error information                                                         |
| 40     | DBPREG    | P-register on page fault on IOBT level                                    |
| 41     | DBACTPRI  | ACTPRI on page fault on IOBT level                                        |
| 42     | FLAGB     | Background flags                                                          |
| 43     | EUSADD    | Address for user-escape handling                                          |
| 44     | LUSADD    | Address for local-function handling                                       |
| 45     | NBREAKS   | Number of break characters in buffer                                      |
| 46     | MWFIELD   | Address of current monitor call working field                             |
| 47     | TRM1IEV   | Event mask for terminal input (TRMIEV)                                    |
| 50     | TRM2IEV   |                                                                           |
| 51     | UACTPRI   | PCR-register when accessing caller’s buffer                               |
| 52     | USADDR    | Address of caller’s buffer                                                |
| 53     | XBUFST    | Logical window address to ring buffer                                     |
| 54     | NCHARS    | Number of characters stored in caller’s buffer                            |
| 55     | CPITENTRY | PIT-entry of terminal data field                                          |
| 57     | BRKCHAR   | Break character                                                           |
| 60     | BRKMODE   | Break mode                                                                |
| 61     |           | Reserved for future use                                                   |
| 62     |           | Reserved for future use                                                   |

---

## Page 86

# 17.6.3 Terminal output data field - non-DPIT part - memory

## Displacement (octal)

| Displacement (octal) | Description |
|----------------------|-------------|
| -10 | SCREEN | Counter for stop on full page |
| -7 | BITFLAG | Various flag bits |
| -6 | TMSUB | Timeout subroutine |
| -5 | TMR | Timeout counter |
| -4 | TTMR | Start value of TMR |
| -3 | HDEV | Hardware device number |
| -2 | STDRIV | Driver start address |
| -1 | DRIVER | Driver interrupt restart address |
| 0 | TDRADDR | Address of data field in resident |
| 1 | XDFOPP | Address of DFOPP in resident |
| 2 | XOPPDF | Value to add to current data field address |
| 3 | TYPRING | Device-type bits and ring |
| 4 | XONCR | XON Character, input control |
| 5 | XOFC | XOFF character, input control |
| 6 | PDISPLAY | Pointer to next terminal in display table |
| 7 | IOTRANS | Called from INBT/OUTBT to transfer |
| 10 | STDEV | Start device |
| 11 | SETDV | IOSET routine |
| 12 | DFOPP | Pointer to output channel data field |
| 13 | DERROR | Error code |
| 14 | BUFST | Start of ring buffer |
| 15 | MAX | Buffer capacity |
| 16 | BHOLD | Number of characters in buffer |
| 17 | HENTE | Fetch pointer |
| 20 | CFREE | Free positions |
| 21 | FYLLE | Store pointer |
| 22 | MINBHOLD | Lower limit for break |
| 23 | ROFIL | For "mode" (output file number) |
| 24 | BCHOST | For "mode" (output status) |
| 25 | ON5MSG | Address for ND-500/5000 message |
| 26 | CBUADR | Current user buffer address (OUTSTRING) |
| 27 | NOCHAR | Number of bytes in OUTSTRING monitor call |
| 30 | CNOCHAR | Number of words left to transfer in OUTSTRING |
| 31 | XNOCHAR | Working location for OUTSTRING |
| 32 | ZOPRG | P, X, T-registers in OUTSTRING |
| 35 | ZOARG | A, D and L-registers in OUTSTRING |
| 40 | ZOSRG | S, B-registers + old page in OUTSTRING |
| 43 | SBHOLD | Saved BHOLD in OUTSTRING |
| 44 | Reserved for future use | |
| 45 | Reserved for future use | |
| 46 | Reserved for future use | |
| 47 | TRM1OEV | Event mask for terminal output (TRMOEV) |
| 50 | TRM2OEV | |

---

## Page 87

# 17.6.4 TAD Input Data Field - Non-DPIT Part - Memory

_Displacement (octal)_

| Displacement | Field    | Description                                            |
|--------------|----------|--------------------------------------------------------|
| -45          | TINFO    | Various information bits for terminal                  |
| -44          | PECH7    | Echo table                                             |
| -34          | PBRK7    | Break table                                            |
| -24          | DFRDATR  | Returned parameter from 7ISRS and 7RESP                |
| -23          | ESCBUF   | Buffer for escape response                             |
| -22          | TMPBUF   | Temporary buffer ID                                    |
| -21          | TADTYP   | TAD type                                               |
| -20          | BRECST   | Break & echo strategy in MON BRKM and MON ECHOM        |
| -17          | CTTYP    | Terminal type                                          |
| -16          | CESCP    | Disconnect and escape characters                       |
| -15          | BRKMAX   | Maximum BHOLD before break                             |
| -14          | NOBUFF   | Number of XMSG buffers to use                          |
| -13          | FBSIZ    | Size of XMSG buffers to use                            |
| -12          | DFLAG    | Device flag bits                                       |
| -11          | ECHOTAB  | Pointer to echo table                                  |
| -10          | BRKTAB   | Pointer to break table                                 |
| -7           | LAST     | Last typed character                                   |
| -6           | TMSUB    | Timeout subroutine                                     |
| -5           | TMR      | Timeout counter                                        |
| -4           | TTMR     | Start value of TMR                                     |
| -3           | PORTNO   | Port number of open port                               |
| -2           | DBCOU    | Data byte counter in input calls                       |
| -1           | DRIVER   | Saved L-register of input driver                       |
| 0            | TDRADDR  | Address of data field in resident                      |
| 1            | XDFOPP   | Address of DFOPP in resident                           |
| 2            | XMXMRET  | Return address of XMSG call                            |
| 3            | TYPRING  | Device-type bits and ring                              |
| 4            | CURMES   | Current message type                                   |
| 5            | OSVTPN   | SINTRAN version and TAD protocol number of partner     |
| 6            | BRCOUNT  | Buffer rotate count                                    |
| 7            | IOTRANS  | Called from INBT/OUTBT to transfer                     |
| 10           | STDEV    | Start device routine                                   |

---

## Page 88

# SINTRAN III Release Information, M-version

| Number | Code     | Description                                           |
|--------|----------|-------------------------------------------------------|
| 11     | SETDV    | IOSET routine                                         |
| 12     | DFOPP    | Pointer to output channel data field                  |
| 13     | DERROR   | Error code                                            |
| 14     | BUFFID   | XMSG buffer identifier                                |
| 15     | TDTAFI   | Buffer address (TDTADD)                               |
| 16     | TDTALA   | Buffer address (TDTADD)                               |
| 17     | TDBTPT   | Byte pointer in XMSG buffer                           |
| 20     | XRSA     | Saved X-register in IOTRANS and INIBDR                |
| 21     | LRSA     | Saved L-register in IOTRANS                           |
| 22     | BSTATE   | Background program state                              |
| 23     | REMBYT   | Remaining bytes in CURMES                             |
| 24     | DBPROG   | Background RT-program                                 |
| 25     | DBADR    | Saved P-register on escape and file system monitor calls |
| 26     | RIFIL    | Mode input file number                                |
| 27     | BCHISTS  | Mode input status                                     |
| 30     | DER0     | Error information                                     |
| 30     | BREGBLOCK| Register save at escape                               |
| 32     | DER2     | Error information                                     |
| 40     | DBPREG   | P-register on page fault on IOBT level                |
| 41     | DBACTPRI | ACTPRI on page fault on IOBT level                    |
| 42     | FLAGB    | Background flags                                      |
| 43     | EUSADD   | Address for user-escape handling                      |
| 44     | LUSADD   | Address for local-function handling                   |
| 45     |          | Reserved for future use                               |
| 46     | REMSIZ   | Remaining bytes in XMSG buffer                        |
| 47     | TRM1IEV  | Event mask for terminal input (TRMIEV)                |
| 50     | TRM2IEV  | Event mask for terminal input (TRMIEV)                |

---

## Page 89

# 17.6.5 TAD output data field - non-DPIT part - memory

## Displacement (octal)

| Displacement | Name   | Description                                               |
|--------------|--------|-----------------------------------------------------------|
| -10          | SCREEN | Counter for stop on full page                             |
| -7           | LAST   | Last character output                                     |
| -6           | TMSUB  | Timeout subroutine                                        |
| -5           | TMR    | Timeout counter                                           |
| -4           | TTMR   | Start value of TMR                                        |
| -3           | RPORT  | Funny number of communication partner                     |
| -2           | PRTN1  | Magic number of communication partner (PARTNER)           |
| -1           | PRTN2  |                                                           |
| 0            | TDRADDR| Address of data field in resident                         |
| 1            | XDFOPP | Address of DFOPP in resident                              |
| 2            | NOBDIS | Message byte counter in XMSG buffer                       |
| 3            | TYPRING| Device-type bits and ring                                 |
| 4            | CURMES | Current message type                                      |
| 5            | POOLL1 | Buffer address of first buffer in pool                    |
| 7            | IOTRANS| Output transfer routine                                   |
| 10           | STDEV  | Start device routine                                      |
| 11           | SETDV  | IOSET routine                                             |
| 12           | DFOPP  | Pointer to output channel data field                      |
| 13           | DERROR | Error code                                                |
| 14           | BUFFID | XMSG buffer identifier                                    |
| 15           | TDTADDR| Buffer address (TDTADD)                                   |
| 16           | TDTALA |                                                           |
| 17           | TDBTPT | Byte pointer in XMSG buffer                               |
| 20           | XRSA   | Saved X-register in IOTRANS                               |
| 21           | LRSA   | Saved L-register in IOTRANS                               |
| 22           | SVOTS  | Saved time slice status if server TAD                     |
| 23           | ROFIL  | For "mode" (output file number)                           |
| 24           | BCHOST | For "mode" (output status)                                |
| 25           | ON5MSG | Address of ND-500/5000 message in fast OUTSTRING          |
| 26           | CBUADR | Current user buffer address (OUTSTRING)                   |
| 27           | NOCHAR | Number of bytes in OUTSTRING monitor call                 |
| 30           | MBFID  | Buffer identifier for mail message                        |
| 31           | RSPNUM | Response number expected on input                         |
| 32           | ZOPRG  | P, X, T-registers in OUTSTRING                            |
| 34           | ZOARG  | A, D and L-registers in OUTSTRING                         |
| 40           | ZOSRG  | S, B-registers + old page in OUTSTRING                    |
| 43           | OSTUAPUT| PCR when accessing user's data in OUTSTRING              |
| 44           | -      | Reserved for future use                                   |
| 45           | CURECST| Current echo strategy on input                            |
| 46           | REMSIZ | Remaining bytes in XMSG buffer                            |
| 47           | TRM1OEV| Event mask for terminal output (TRMOEV)                   |
| 50           | TRM2OEV|                                                           |

---

## Page 90

## 17.6.6 NOTS input data field - non-DPIT part - memory

### Displacement (octal)

|        |        |
|--------|--------|
| -45    | TINFO  | Various information bits |
| -44    | PECH7  | Echo table 7 |
| -34    | PBRK7  | Break table 7 |
| -24    | IN5MSG | Address of ND-500/5000 message in fast INSTRING |
| -23    | RSISTE | Echo pointer |
| -22    | BRECHOFL | Break and echo flag |
| -21    | ROUSPEC | Address of special subroutine |
| -20    | NCBRK  | Number of characters after last break |
| -17    | CTTYP  | Terminal type |
| -16    | CESCP  | Disconnect and escape characters |
| -15    | BRKMAX | Maximum BHOLD before break |
| -14    | MNTMFL | Timer routine parameter |
| -13    | MNGET  | Get pointer |
| -12    | DFLAG  | Device flag bits |
| -11    | ECHOTAB | Pointer to echo table |
| -10    | BRKTAB | Pointer to break table |
| -7     | LAST   | Last typed character |
| -6     | TMSUB  | Timeout subroutine |
| -5     | TMR    | Timeout counter |
| -4     | TTMR   | Start value of TMR |
| -3     | HDEV   | Hardware device number |
| -2     | STDRIV | Driver start address |
| -1     | DRIVER | Driver interrupt restart address |
| 0      | TDADDR | Address of data field in resident |
| 1      | XDFOPP | Address of DFOPP in resident |
| 2      | XOPPDPF | Address of opposite data field (outside resident) |
| 3      | TYPRING | Device-type bits and ring |
| 4      | MNWAD  | Address of window |
| 5      | MNCURB | Current buffer pointer |
| 6      | MNCDF  | Address of controller data field |
| 7      | IOTRANS | Called from INBT/OUTBT to transfer |
| 10     | STDEV  | Start device routine |

---

## Page 91

# SINTRAN III Release Information, M-version

| Number | Name     | Description                                              |
|--------|----------|----------------------------------------------------------|
| 11     | SETDV    | IOSET routine                                            |
| 12     | DFOPP    | Pointer to output channel data field                     |
| 13     | DERROR   | Error code                                               |
| 14     | BUFST    | Start of ring buffer                                     |
| 15     | MAX      | Buffer capacity                                          |
| 16     | BHOLD    | Number of characters in buffer                           |
| 17     | HENTE    | Fetch pointer                                            |
| 20     | CFREE    | Free positions                                           |
| 21     | FYLLE    | Store pointer                                            |
| 22     | BSTATE   | Background program state                                 |
| 23     |          | Reserved for future use                                  |
| 24     | DBPROG   | Background RT-program                                    |
| 25     | DBADR    | Saved P-register on escape and file system monitor calls |
| 26     | RIFIL    | Mode input file number                                   |
| 27     | BCHISTS  | Mode input status                                        |
| 30     | DER0     | Error information                                        |
| 31     | BREGBLOCK| Register save at escape                                  |
| 32     | DER2     | Error information                                        |
| 40     | DBPREG   | P-register on page fault on IOBT level                   |
| 41     | DBACTPRI | ACTPRI on page fault on IOBT level                       |
| 42     | FLAGB    | Background flags                                         |
| 43     | EUSADD   | Address for user-escape handling                         |
| 44     | LUSADD   | Address for user local-function handling                 |
| 45     | NBREAKS  | Number of break characters in buffer                     |
| 46     | MWFIELD  | Address of current monitor call working field            |
| 47     | TRM11EV  | Event mask for terminal input (TRMIEV)                   |
| 50     | TRM2IEV  |                                                          |
| 51     | UACTPRI  | PCR-register when accessing caller's buffer              |
| 52     | USADDR   | Address of caller's buffer                               |
| 53     | XBUFST   | Logical window address to ring buffer                    |
| 54     | NCHARS   | Number of characters stored in caller's buffer           |
| 55     | CPITENTRY| PIT-entry of terminal data field                         |
| 57     | BRKCHAR  | Break character                                          |
| 60     | BRKMODE  | Break mode                                               |
| 61     |          | Reserved for future use                                  |
| 62     |          | Reserved for future use                                  |

---

## Page 92

# 17.6.7 NOTS Output Data Field - Non-DPIT Part - Memory

### Displacement (octal)

| Displacement | Description                                |
|--------------|--------------------------------------------|
| -10          | SCREEN Counter for stop on full page       |
| -7           | BITFLAG Various flag bits                  |
| -6           | TMSUB Timeout subroutine                   |
| -5           | TMR Timeout counter                        |
| -4           | TTMR Start value of TMR                    |
| -3           | HDEV Hardware device number                |
| -2           | STDRIV Driver start address                |
| -1           | DRIVER Driver interrupt restart address    |
| 0            | TDRADDR Address of data field in resident  |
| 1            | XDFOPP Address of DFOPP in resident        |
| 2            | XOPPDF Value to add to current data field address |
| 3            | TYPRING Device-type bits and ring          |
| 4            | MNFILL Fill pointer (absolute address)     |
| 5            | MNCURB Current buffer pointer              |
| 6            | MNWBNK Bank number for window              |
| 7            | IOTRANS Called from INBT/OUTBT to transfer |
| 10           | STDEV Start device                         |
| 11           | SETDV IOSET routine                        |
| 12           | DFOPP Pointer to output channel data field |
| 13           | DFERROR Error code                         |
| 14           | BUFST Start of ring buffer                 |
| 15           | MAX Buffer capacity                        |
| 16           | BHOLD Number of characters in buffer       |
| 17           | HENTE Fetch pointer                        |
| 20           | CFREE Free positions                       |
| 21           | FYLLE Store pointer                        |
| 22           | MINBHOLD Lower limit for break             |
| 23           | ROFIL For "mode" (output file number)      |
| 24           | BCHOST For "mode" (output status)          |
| 25           | ON5MSG Address for ND-500/5000 message     |
| 26           | CBUADR Current user buffer address (OUTSTRING) |
| 27           | NOCHAR Number of bytes in OUTSTRING monitor call |
| 30           | CNOCHAR Number of words left to transfer in OUTSTRING |
| 31           | XNOCHAR Working location for OUTSTRING     |
| 32           | ZOPRG P, X, T-registers in OUTSTRING       |
| 35           | ZOARG A, D and L-registers in OUTSTRING    |
| 40           | ZOSRG S, B-registers + old page in OUTSTRING |
| 43           | SBHOLD Saved BHOLD in OUTSTRING            |
| 44           | Reserved for future use                    |
| 45           | Reserved for future use                    |
| 46           | Reserved for future use                    |
| 47           | TRM1OEV Event mask for terminal output (TRMOEV) |
| 50           | TRM2OEV                                     |

---

## Page 93

# 17.6.8 MTAD input data field - non-DPIT part - memory

## Displacement (octal)

| Displacement | Label   | Description                                          |
|--------------|---------|------------------------------------------------------|
| -45          | TINFO   | Various information bits                             |
| -44          | PECH7   | Echo table 7                                         |
| -34          | PBRK7   | Break table 7                                        |
| -24          | IN5MSG  | Address of ND-500/5000 message in fast INSTRING      |
| -23          | RSISTE  | Echo pointer                                         |
| -22          | BRECHOFL| Break and echo flag                                  |
| -21          | ROUSPEC | Address of special subroutine                        |
| -20          | NCBRK   | Number of characters after last break                |
| -17          | CTYTP   | Terminal type                                        |
| -16          | CESC    | Disconnect and escape characters                     |
| -15          | BRKMAX  | Maximum BHOLD before break                           |
| -14          | MDFLI   | Link to next free data field (-1 = end of list)      |
| -13          | MTRTP   | RT-description address of client                     |
| -12          | DFLAG   | Device flag bits                                     |
| -11          | ECHOTAB | Pointer to echo table                                |
| -10          | BRKTAB  | Pointer to break table                               |
| -7           | LAST    | Last typed character                                 |
| -6           | TMSUB   | Timeout subroutine                                   |
| -5           | TMR     | Timeout counter                                      |
| -4           | TTM     | Start value of TMR                                   |
| -3           | HDEV    | Hardware device number                               |
| -2           | STDRIV  | Driver start address                                 |
| -1           | DRIVER  | Driver interrupt restart address                     |
| 0            | TDRADDR | Address of data field in resident                    |
| 1            | XFOPP   | Address of DFOPP in resident                         |
| 2            | XOPPDF  | Address of opposite data field (outside resident)    |
| 3            | TYPRING | Device-type bits and ring                            |
| 4            | MTMBAD  | Mailbox address                                      |
| 5            | MTGET   | Address of get routine                               |
| 6            | MTFLAG  | Timer flag                                           |
| 7            | IOTRANS | Called from INBT/OUTBT to transfer                   |
| 10           | STDEV   | Start device routine                                 |

*To be continued*

---

## Page 94

# SINTRAN III Release Information, M-version

| Number | Code      | Description                                                    |
|--------|-----------|----------------------------------------------------------------|
| 11     | SETDV     | IOSET routine                                                  |
| 12     | DFOPP     | Pointer to output channel data field                           |
| 13     | DERROR    | Error code                                                     |
| 14     | BUFST     | Start of ring buffer                                           |
| 15     | MAX       | Buffer capacity                                                |
| 16     | BHOLD     | Number of characters in buffer                                 |
| 17     | HENTE     | Fetch pointer                                                  |
| 20     | CFREE     | Free positions                                                 |
| 21     | FYLLE     | Store pointer                                                  |
| 22     | BSTATE    | Background program state                                       |
| 23     |           | Reserved for future use                                        |
| 24     | DBPROG    | Background RT-program                                          |
| 25     | DBADR     | Saved P-register on escape and file system monitor calls       |
| 26     | RIFIL     | Mode input file number                                         |
| 27     | BCHISTS   | Mode input status                                              |
| 30     | DER0      | Error information                                              |
| 31     | BREGBLOCK | Register save at escape                                        |
| 32     | DER2      | Error information                                              |
| 40     | DBPREG    | P-register on page fault on IOBT level                         |
| 41     | DBACTPRI  | ACTPRI on page fault on IOBT level                             |
| 42     | FLAGB     | Background flags                                               |
| 43     | EUSADD    | Address for user-escape handling                               |
| 44     | LUSADD    | Address for user local-function handling                       |
| 45     | NBREAKS   | Number of break characters in buffer                           |
| 46     | MWFIELD   | Address of current monitor call working field                  |
| 47     | TRM1IEV   | Event mask for terminal input (TRMIEV)                         |
| 50     | TRM2IEV   |                                                               |
| 51     | UACTPRI   | PCR-register when accessing caller's buffer                    |
| 52     | USADDR    | Address of caller's buffer                                     |
| 53     | XBUFST    | Logical window address to ring buffer                          |
| 54     | NCHARS    | Number of characters stored in caller's buffer                 |
| 55     | CPITENTRY | PIT-entry of terminal data field                               |
| 57     | BRKCHAR   | Break character                                                |
| 60     | BRKMODE   | Break mode                                                     |
| 61     |           | Reserved for future use                                        |
| 62     |           | Reserved for future use                                        |

---

## Page 95

# 17.6.9 MTAD Output Data Field - Non-DPIT Part - Memory

## Displacement (octal)

| Displacement | Name    | Description                                          |
|--------------|---------|------------------------------------------------------|
| -10          | SCREEN  | Counter for stop on full page                        |
| -7           | BITFLAG | Various flag bits                                    |
| -6           | TMSUB   | Timeout subroutine                                   |
| -5           | TMR     | Timeout counter                                      |
| -4           | TTMR    | Start value of TMR                                   |
| -3           | HDEV    | Hardware device number                               |
| -2           | STDRIV  | Driver start address                                 |
| -1           | DRIVER  | Driver interrupt restart address                     |
| 0            | TDRADDR | Address of data field in resident                    |
| 1            | XDFOPP  | Address of DFOPP in resident                         |
| 2            | XOPPDF  | Value to add to current data field address           |
| 3            | TYPRING | Device-type bits and ring                            |
| 4            | MTMBAD  | Mailbox address                                      |
| 5            | MTPUT   | Address of put routine                               |
| 6            | MTACT   | Address of routine to restart client                 |
| 7            | IOTRANS | Called from INBT/OUTBT to transfer                   |
| 10           | STDEV   | Start device                                         |
| 11           | SETDV   | IOSET routine                                        |
| 12           | DFOPP   | Pointer to output channel data field                 |
| 13           | DERROR  | Error code                                           |
| 14           | BUFST   | Start of ring buffer                                 |
| 15           | MAX     | Buffer capacity                                      |
| 16           | BHOLD   | Number of characters in buffer                       |
| 17           | HENTE   | Fetch pointer                                        |
| 20           | CFREE   | Free positions                                       |
| 21           | FYLLE   | Store pointer                                        |
| 22           | MINBHOLD| Lower limit for break                                |
| 23           | ROFIL   | For "mode" (output file number)                      |
| 24           | BCHOST  | For "mode" (output status)                           |
| 25           | ON5MSG  | Address for ND-500/5000 message                      |
| 26           | CBUADR  | Current user buffer address (OUTSTRING)              |
| 27           | NOCHAR  | Number of bytes in OUTSTRING monitor call            |
| 30           | CNOCHAR | Number of words left to transfer in OUTSTRING        |
| 31           | XNOCHAR | Working location for OUTSTRING                       |
| 32           | ZOPRG   | P, X, T-registers in OUTSTRING                       |
| 35           | ZOARG   | A, D and L-registers in OUTSTRING                    |
| 40           | ZOSRG   | S, B-registers + old page in OUTSTRING               |
| 43           | SBHOLD  | Saved BHOLD in OUTSTRING                             |
| 44-46        |         | Reserved for future use                              |
| 47           | TRM1OEV | Event mask for terminal output (TRMOEV)              |
| 50           | TRM2OEV |                                                     |

---

## Page 96

# 18. Affected Subsystems

## ND-500/5000 System Package

For SINTRAN III/VSX version M, version C of the ND-500/5000 System Package (ND 211305) contains the following products:

- ND-500/5000 Background Monitor (version K)
- ND-500/5000 Swapper (version L)
- ND-500/5000 Place-Library (version C)

For use under version M of SINTRAN III.

## ND-500/5000 Monitor

Only version J or later of ND-500/5000 Background Monitor (ND 210333, part of ND 211305) may be used when running SINTRAN III version M. Version K is required when running ND-5830 or ND-5850 systems.

## ND-500/5000 Swapper

Only version L of ND-500/5000 Swapper (ND 211034, part of ND 211305) may be used when running SINTRAN III version M.

## XMSG

XMSG is part of SINTRAN III in the M-version and must not be installed as a separate product.

## COSMOS Basic Module

Version F of COSMOS Basic Module (ND 210374) is required when running XMSG which is now part of SINTRAN III version M. Version G of COSMOS Basic Module offers better performance when copying files with "holes".

## SINTRAN III Configuration

Version E or later of the SINTRAN III Configuration program (ND 211024) is required when configuring the M-version of SINTRAN III.

## NOTS Service

Version C or later of the NOTS Service program (part of ND 211024) is used to set or change the configuration of Net/One terminals.

## ERS/SINTRAN III Watchdog

The SINTRAN III Watchdog of the Event Report System is part of SINTRAN III in the M-version and must not be installed as a separate product. The D-versions of the error message descriptor file and the log-list program are required to run under the M-version of SINTRAN III.

## Backup Manager

Version A of the Backup Manager (ND 211226) is designed to ease the task of taking backup. Version B offers better performance.

## Backup System

Version H of the Backup System (ND 210337) is required to handle files with file index > 255 (more than 256 files per user). Version I is required to handle SCSI streamer tape drives. Revision I05 of the Backup System and revision I05 of the DMA server are required to handle SCSI optical disks and magnetic tape drives.

---

## Page 97

# SINTRAN III Release Information, M-version

| Component               | Details                                                                                                                                 |
|-------------------------|-----------------------------------------------------------------------------------------------------------------------------------------|
| **Disk Mirroring**      | Only version E of Disk Mirroring (ND 210855) may be used when running SINTRAN III version M.                                            |
| **File Manager**        | Versions A or B of the File Manager (part of ND 210518) will not handle files with file index > 255 (more than 256 files per user). Version C of the File Manager (ND 211075) will handle this problem. |
| **File System Investigator** | Version O of the File System Investigator (part of ND 210628) is required to handle files with file index > 255 (more than 256 files per user). |
| **Linkage Loader**      | Version H of the Linkage Loader (ND 210319) is required to handle communication with RT-programs.                                        |
| **ND Linker**           | The ND-LINKER (ND 211224, version A or later) is required to handle the new domain files (as opposed to the old ND-500/5000 domains built by the Linkage-Loader). |
| **Convert Domain**      | The conversion program, Convert Domain (ND 211229) is used to convert an "old" domain (built by the Linkage-Loader and stored on a triple of files) to a domain file without having to reload the domain. |
| **Performance Monitor** | Only version B of the Performance Monitor (ND 211074) may be used when running SINTRAN III version M.                                   |
| **Symbolic Debugger**   | Version F of the Symbolic Debugger (ND 210336) can be used when running SINTRAN III version M to debug RT-programs. Version H is required to handle ND-500/5000 domains stored on domain files (by the ND-Linker). |
| **LED Debugger**        | Revision B02 of the LED Debugger (ND 211157) is required to handle ND-500/5000 domains stored on domain files (by the ND-Linker).       |
| **Telefix Local**       | Version C01 of Telefix Local (ND 210775) is required.                                                                                   |
| **User Environment**    | Version C or later of User Environment (ND 210518) is required to run under SINTRAN III version M.                                      |

---

## Page 98

I'm sorry. The document you uploaded appears to be blank and doesn't have any text for conversion. Could you please provide another page or check the document for any potential issues?

---

## Page 99

I'm sorry, but the image provided is completely blank except for a small footer. Please provide another image with text content to convert into Markdown.

---

## Page 100

I'm unable to process this image as there isn't any visible text to convert to Markdown. If you have another page with text, please share it, and I'll be happy to help!

---

