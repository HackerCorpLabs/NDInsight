## Page 1

# SINTRAN III

Release Information  
N-version

*Comma*

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 2

# U.se of the Language Translator

The Language translator allows you to convert programs written in SINTRAN III to run under another system or vice versa, with a limited amount of editing effort. 

## Method of Operation

The translator is designed to process SQL, NAP, RPG and PAL source statements and map them into the equivalent target language source statements.

## Target Machine

Generally, programs are written under the assumption of a particular environment. SINTRAN III is no exception. The following considerations must be made when selecting a target machine:

- Word size
- Input/Output
- Peripherals
- Operating System differences

## Restrictions of Use

Certain restrictions may apply depending on the hardware and software configurations:

| Component              | Restriction                |
|------------------------|----------------------------|
| Data Size              | Limited to 64k per module  |
| I/O Device Support     | Limited to SINTRAN devices |
| Language Constructs    | Specific to SINTRAN III    |

## Example Usage

Consider a scenario where a program written in SQL needs to be translated into SINTRAN III. The following example illustrates a basic conversion process:

1. Identify target language equivalents.
2. Perform syntax mapping.
3. Conduct testing on the SINTRAN III environment.

```plaintext
Example Code Pre-Conversion:
SQL SELECT command

Example Code Post-Conversion:
Equivalent SINTRAN III Command
```

## Conclusion

In conclusion, the Language Translator is an essential tool for operating across different systems efficiently, minimizing efforts on manual rewriting and optimizing processes for integration and testing.

---

## Page 3

# SINTRAN III  
## Release Information  
### N-version  

860230 EN 8

---

## Page 4

## Information Notice

The information in this manual is subject to change without notice. Comma Data Service AS assumes no responsibility for any errors that may appear in this manual, or for the use or reliability of its software on equipment that is not furnished or supplied by Comma Data Service AS.

## Copyright

Copyright © 1993 by Comma Data Service AS

| Version  | Date         |
|----------|--------------|
| Version 1| January 1985 |
| Version 2| June 1986    |
| Version 3| May 1987     |
| Version 4| November 1987|
| Version 5| May 1988     |
| Version 6| September 1988|
| Version 7| January 1990 |
| Version 7A| December 1990|
| Version 8| February 1993|

## Contact Information

Send all documentation requests to:

Comma Data Service AS  
P.O. Box 6884 - Etterstad  
N-0605 Oslo, Norway

---

## Page 5

# Preface

| S I N T R A N III / VSX |
|--------------------------|
| N-version                |

Keywords for SINTRAN III N-version:

- Increased performance in the ND-5000 Swapper (Multi-thread)
- Increased performance in the file system
- Support for larger configurations
- Improved error logging
- Enhanced security functions

This manual describes the changes in the N-version of SINTRAN III/VSX compared to the M-version.

---

## Page 6

# Table of Contents

## 1. Preparing for installation
1.1 Hardware requirements for SINTRAN III/VSX version N  
1.2 Software requirements for SINTRAN III/VSX version N  
1.3 Changes in configuration limitations  
1.4 Microprogram versions for ND-500/5000  
1.5 Changes in installation procedure  
1.6 Changes to HENT-MODE / LOAD-MODE - and other mode files  
1.7 Standard configurations  
1.8 Changes to the New-System program  

## 2. Installation of SINTRAN III/VSX

## 3. SINTRAN III Commands
3.1 Modified commands  
3.1.1 @CHANGE-DIRECTORY-ENTRY  
3.1.2 @INITIAL-COMMAND  
3.1.3 @LIST-INITIAL-COMMANDS  
3.1.4 @LOOK-AT  
3.1.5 @NEXT-INITIAL-COMMAND  
3.1.6 @RTCLOSE-FILE  
3.1.7 @SET-ERROR-DEVICE  
3.1.8 @WHO-IS-ON  
3.1.9 @ (RECOVER without giving the command name)  

## 4. Monitor Calls (ND-100)
4.1 Modified monitor calls  
4.1.1 CLOSE MON 43  
4.1.2 IOMTY MON 336  
4.1.3 CONFG MON 343  

## 5. SINTRAN Service Program
5.1 Modified commands  
5.1.1 *CHANGE-VARIABLE  
5.1.2 *FILE-SYSTEM-EVENT-LOG  
5.2 New commands  
5.2.1 *LIST-COLDSTART-MODE-FILE  
5.2.2 *LIST-DEFAULT-ERROR-DEVICE  
5.2.3 *LIST-VARIABLES  
5.2.4 *SET-DEFAULT-ERROR-DEVICE

---

## Page 7

# 6. ND-500/5000 Monitor

## 6.1 Installation procedure
19

## 6.2 Configuration limitations
19

## 6.3 Modified commands to SINTRAN III affecting the ND-500/5000
19

### 6.3.1 @ (RECOVER without giving the command name)
19

## 6.4 Modified commands to the ND-500/5000 Monitor version L
20

### 6.4.1 LIST-TABLE
20

### 6.4.2 LOOK-AT
20

### 6.4.3 LOOK-AT-CONTROL-STORE
20

### 6.4.4 LOOK-AT-HARDWARE
20

### 6.4.5 LOOK-AT-STACK
21

### 6.4.6 SWAPPING-LOG
21

## 6.5 New commands to the ND-500/5000 Monitor version L
22

### 6.5.1 SWAP-TABLE
22

## 6.6 Modified monitor calls to the ND-100 affecting ND-500/5000
23

### 6.6.1 CLOSE MON 43
23

## 6.7 Modified monitor calls (ND-500/5000)
23

### 6.7.1 IOMTY MON 336
23

## 6.8 Modified monitor calls - available only on ND-500/5000
23

### 6.8.1 AT5SGM MON 440 (Attach500Segment)
23

# 7. ND-5000 Swapper version M

## 7.1 Major improvements
24

## 7.2 Possible case were it might be worse
25

## 7.3 Memory usage
25

## 7.4 Postmortem information
26

## 7.5 Changed error handling
26

# 8. File System

## 8.1 Changes in configuration limitations
27

## 8.2 Performance
27

## 8.3 Changes to the file system event-log utility
27

# 9. XMSG

## 9.1 Changed configuration limitation
28

# 10. ND-5000 Microprograms
29

# 11. ERS/SINTRAN III Watchdog

## 11.1 New reporters recognized by the SINTRAN III Watchdog
30

---

## Page 8

# 12. SINTRAN III Configuration program
...31

# 13. Nucleus
...32

# 14. UPS Power Server
...33

# 15. MEMTOF / MEMTOS
...34

| Section | Description | Page |
|---------|-------------|------|
| 15.1    | Introduction | 34   |
| 15.2    | Operating procedure | 34   |
| 15.3    | Error handling | 34   |
| 15.4    | Tape status decoding | 35   |
| 15.5    | Floppy disk status decoding | 35   |

# 16. User Environment
...36

# 17. New Error Messages
...37

| Section | Description | Page |
|---------|-------------|------|
| 17.1    | Error handling during start of SINTRAN | 37   |
| 17.2    | SINTRAN III Run-time errors | 37   |
| 17.3    | Error codes returned from the ND-500/5000 monitor - numeric list | 37   |
| 17.4    | Error codes returned from the ND-5000 Swapper | 37   |
| 17.5    | Fatal errors returned from the ND-5000 Swapper | 38   |
| 17.6    | Non-fatal errors returned from the ND-5000 Swapper | 39   |
| 17.7    | Error codes returned from ND-5850 Service Partner (James) | 40   |

# 18. SINTRAN III N-version, System Layout
...41

| Section | Description | Page |
|---------|-------------|------|
| 18.1    | System layout on disk | 41   |
| 18.2    | Page index table layout | 42   |
| 18.3    | System included segments | 44   |
| 18.4    | System included RT-programs | 46   |

# 19. Affected Subsystems
...48

| Section | Description | Page |
|---------|-------------|------|
| 19.1    | Subsystems delivered together with SINTRAN | 48   |
| 19.2    | Other subsystems | 48   |

---

## Page 9

# 1. Preparing for installation

## 1.1 Hardware requirements for SINTRAN III/VSX version N

### ND-100 CPU, one of:
- ND-100/CX CPU with ECO 100-522 or later (48-bit floating representation)  
  or ECO 100-523 or later (32-bit floating representation)  
  and Memory management II (16 PITs) with ECO 100-534 (level N or later)
- ND-110 CPU (CPU and memory management) print no. 3090 (level P or later)
- ND-110 CPU (CPU and memory management) print no. 3095 (level H or later)
- ND-120/CX CPU (CPU, memory management and memory on one card) (level K)

If SMD disk controller (10 MHz) is used, the following applies:
- SMD Control (print 3018) ECO level R or later is required
- SMD Data (print 3019) ECO level BE or later is required

If Dual Disk Channel Switch is present, ECO level J or later is required.

If NUCLEUS is to be run, one of the following is necessary:
- ND-5000
- ND-500/II with ND-100 Octobus Line Driver (324133) level D or later  
  (or 324118) level G or later

If the system has ND-5850 or ND-5830 CPU(s), the following requirements apply:
- either  
  – ND-5850 CPU (320027) ECO level 13 or later  
  or  
  – ND-5830 CPU (320026) ECO level 13 or later
- either  
  – ND-5000 MF Bus controller "James" (324278) ECO level E or later  
  or  
  – ND-5000 MF Bus controller "James" (324271) ECO level F or later

If DOMINO controllers are used, the following requirements apply:
- MFB/SCSI (print 5467) level A or later
- ND-5000
- either  
  – MF-bus controller (324245) ECO level C or later  
  or  
  – Double-bus controller (324244) ECO level E or later
- either  
  – PROM for MF-bus controller (47800) ECO level E or later  
  or  
  – PROM for Double-bus controller (47500) ECO level D or later
- either  
  – MFB port (350161) ECO level F or later  
  or  
  – MPM-5 port (324355) ECO level G or later
- DOMINO PROM (73100) ECO level C or later

If the M-version of the ND-5000 Swapper is to be used, the system must be an ND-5000 system.

---

## Page 10

# The RAM disk feature is available as an option both on the ND tpServer platform (version C) and on the ND-5000 ES platform (version C).

To use this feature, the following requirements apply to the different components, when present:

- MF-bus controller (324245) ECO level G or later
- Double-bus controller (324244) ECO level L or later
- MFB port (350161) ECO level G or later
- MF bus memory 16 Mbyte (324242) ECO level F or later
- MF bus memory 8 Mbyte (350152) ECO level F or later
- MF bus memory 4 Mbyte (350160) ECO level F or later
- MF bus memory 4 Mbyte (324158) ECO level W or later

## 1.2 Software requirements for SINTRAN III/VSX version N

SINTRAN III/VSX version N, generation 1 requires revision level (patch file level) 1000 or higher.

## 1.3 Changes in configuration limitations

- The size of the buffer containing initial commands (commands to be executed on a warm start of the system) is increased to 508 characters.
- LAMUs to be used from the ND-500/5000 may now be placed above the old 32 Mbyte limit.

## 1.4 Microprogram versions for ND-500/5000

The following table shows the microprogram versions required to run ND-5000 and ND-500 systems on the N-version of SINTRAN III:

| Prod.no   | System type                         | Microprogram version |
|-----------|-------------------------------------|----------------------|
| 210332 J  | ND-500 series I, standard           | 10512                |
| 210338 I  | ND-500 series I, AX-CPU             | 10412                |
| 210411 G  | ND-500 series I, CX-CPU             | 10312                |
| 210412 G  | ND-500 series I, CXA-CPU            | 10612                |
| 210787 F  | ND-530                              | 15313                |
| 210786 F  | ND-550/560/570                      | 15213                |
| 210788 F  | ND-550/560/570, > 32 Mbyte          | 15413                |
| 210701 H  | ND-580                              | 15113                |
| 211272 E  | ND-5200                             | 11533                |
| 211273 E  | ND-5400                             | 11633                |
| 211274 E  | ND-5500                             | 11733                |
| 211275 F  | ND-5700                             | 11833                |
| 211276 F  | ND-5800                             | 11933                |
| 211847 B  | ND-5830/5850                        | 12011                |

---

## Page 11

# 1.5 Changes in installation procedure

- SINTRAN III/VSX version N will be delivered on 3 double-sided/double-density (8" or 5 1/4") diskettes. In addition to the ordinary SINTRAN diskettes, three additional diskettes are delivered with SINTRAN:
  - One diskette containing the SINTRAN III Patch file for the N-version.
  - One diskette containing the ND-500 Swapper, ND-500/5000 Monitor, SINTRAN III Configuration Program and NOTS Service.
  - One diskette containing new microprograms for ND-5850, ND-5830, ND-5800 and ND-5700.

# 1.6 Changes to HENT-MODE / LOAD-MODE - and other mode files

Some of the items listed below only apply when moving directly from the K- or L-versions to the N-version. This is indicated in brackets.

- If you have a mode file to be run after an installation of SINTRAN III from diskettes, remove any commands used to load and initialise XMSG to your system. This is important if you move directly from the K-version to the N-version of SINTRAN III.

- If you run private patches in any of the mode files, these patches must be checked carefully (and possibly modified) to run on the N-version of SINTRAN III.

- The following changes must be made to the mode file to be run after a cold start (usually called HENT-MODE:MODE):
  - Ensure that any abbreviations of the ENTER-DIRECTORY command are non-ambiguous (@EN-D is now the shortest possible abbreviation) [from K or L].
  - If COSMOS Basic Module revision G03 is to be installed, replace loading of COSMOS Basic Module by new files loading revision G03.
  - If User Environment version E is to be installed, replace loading of User Environment by new files loading version E.

- The following changes must be made to the batch file to be run after a warm start (usually called LOAD-MODE:MODE):
  - As the User Environment server (version E) now runs on the ND-500/5000, ensure that the command used to start User Environment (@UE-FUNC,START) is placed after the commands used to start the ND-500/5000 part of the system.
  - Ensure that any abbreviations of the ENTER-DIRECTORY command are non-ambiguous (@EN-D is the shortest possible abbreviation) [from K or L].
  - Make certain that you use the correct version of the XMSG-Command program in your XMSG-START file (the P-version). Note that XMSG-Command and the XMSG-STARTEX files are copied to user area SYSTEM during installation.

- If you have a mode file used for stopping the system in a controlled way, you should include commands to close the log file for the SINTRAN III Watchdog:
  - Start the SINTRAN III Watchdog Manager Program.
  - Use the command: SELECT-WRITE-PARAMETERS Yes No
  - Then EXIT from the Manager program.

---

## Page 12

# 1.7 Standard configurations

The M-version of SINTRAN III/VSX is delivered as a limited number of standard versions able to support a great variety of configurations. As for the K, L and M versions, a program for handling reconfiguration is supplied.

A list of options included in the SINTRAN III/VSX version N standard configurations A, B and C is given below (values from the M-version are indicated, when different):

| | A | B | C |
|---|---|---|---|
| SMD/ECC disk controllers (max. 4 units/each): | 2 | 4 | 2 |
| ST-506 (Winchester) disk (max. 2 units/each): | 1 | 2 | 1 |
| SCSI host adaptor (controller): | 3 | 2 | 2 (1) |
| SCSI disk units (per system): | 8 | 8 | 4 (2) |
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
| Terminals: | 135 | 155 (125) | 175 (172) |
| Line printers: Parallel or DMA interfaces: | 2 | 2 | 2 |
| Versatec printer/plotter DMA: | 2 | 2 | 2 |
| Versatec printer/plotter I/O: | 2 | 2 | 2 |
| Extra spooling processes: | 16 | 10 | 16 |
| COSMOS spooling: | Yes | Yes | Yes |
| Communication: HDLC + synchronous modem (total): | 6 | 12 | 2 |
| HDLC interfaces (reserved for HDLC): | 0 | 6 | 0 |
| Synchronous modem interface: | 2 | 2 | 2 |
| PIOC interfaces: | 4 | 4 | 2 |
| GPIB interface: | 1 | 1 | 1 |
| MPM IV option: | Yes | Yes | Yes |
| I/O bus extensions: | 2 | 2 | 0 (2) |
| X.21 interfaces: | 2 | 2 | 1 |
| X.25 option: | Yes | Yes | Yes |
| X.29 option: | Yes | Yes | Yes |
| CAMAC: | 0 | 16 | 0 |
| Universal DMA / Vicom interfaces: | 2 | 6 | 2 |
| Fast UDMA on ND-500/5000: | Yes | Yes | Yes |
| Ethernet interfaces: | 3 | 3 | 2 (3) |
| TELEFIX: | 1 | 1 | 1 |
| HASP DMA interface: | 1 | 1 | 1 |
| Net/One controllers: | 3 | 3 | 1 |
| Support for WPX IPS Bridge: | Yes | No | Yes |

---

## Page 13

# SINTRAN III Release Information, N-version

## Software Options

| Software Options                            | A           | B          | C          |
|---------------------------------------------|-------------|------------|------------|
| Terminal/TAD background tasks:              | 172 (159)   | 125 (120)  | 205 (200)  |
| Terminal Access Devices (TADs):             | 70          | 50         | 70         |
| Batch processes:                            | 10          | 10         | 10         |
| Segments:                                   | 600 (500)   | 750        | 620 (500)  |
| Free RT-descriptions for users:             | 200 (180)   | 148 (150)  | 230 (180)  |
| ND-500/5000 processes:                      | 150 (134)   | 135 (128)  | 200 (190)  |
| SIBAS processes (SIBAS F):                  | 12          | 12         | 12         |
| Semaphores:                                 | 50          | 50         | 50         |
| Internal device (byte-oriented):            | 22 (30)     | 30         | 30         |
| Internal device (block-oriented):           | 2           | 2          | 2          |
| CX-CPU:                                     | Yes         | Yes        | Yes        |
| ND-500/5000:                                | Yes         | Yes        | Yes        |
| ND-500 CPUs:                                | 4           | 4          | 4          |
| ND-5000 CPUs:                               | 4           | 4          | 4          |
| XMSG:                                       | Yes         | Yes        | Yes        |
| Device buffers:                             | 128         | 125        | 128        |
| Symbolic Debugger tasks:                    | 12 (32)     | 8          | 15 (32)    |
| Remote file access segments:                | 50          | 32         | 50         |
| CONNECT-TO:                                 | Yes         | Yes        | Yes        |
| RT and I/O accounting:                      | Yes         | Yes        | Yes        |
| Remote Job Entry queues:                    | All         | All        | All        |
| Logging facilities:                         | All         | All        | All        |
| RT-Common:                                  | 6           | 6          | 6          |
| TPS:                                        | 0 (1)       | 1          | 1          |
| LAMU:                                       | Yes         | Yes        | Yes        |
| MON ADP:                                    | Yes         | Yes        | Yes        |
| MON 5MTRANS:                                | Yes         | Yes        | Yes        |
| Background allocation:                      | Yes         | Yes        | Yes        |
| Read segment:                               | Yes         | Yes        | Yes        |
| Disk optimisation:                          | Yes         | Yes        | Yes        |
| Direct task:                                | No          | Yes        | No         |
| RT-programs from direct task:               | 0           | 25         | 0          |
| Magnetic Tape from direct task:             | No          | Yes        | No         |
| Direct transfer on magnetic tape:           | Yes         | Yes        | Yes        |
| Connect data fields:                        | 2           | 16         | 2          |
| Fault Tolerant eXtension (FTX):             | Yes         | Yes        | Yes        |
| Disk Mirroring clusters                     | 8           | 8          | 1          |
| Paper-tape punch:                           | Yes         | Yes        | Yes        |
| Allocated areas:                            | 112         | 112        | 112        |
| Programmable RT-clock driver:               | No          | Yes        | No         |
| Standard bootstrap drivers:                 | Yes         | Yes        | Yes        |
| BDIO pool data fields                       | 32 (16)     | 16         | 40         |

---

## Page 14

# 1.8 Changes to the New-System program

- The command LIST-IMPLEMENTED-PATCHES now asks which area to consider.  
  Possible values:  
  M : current running SINTRAN  
  I : image area (to be activated by a warm start)  
  S : save area (to be activated by a cold start)  

- When copying SINTRAN III from floppy disk, NEW-SYSTEM also copies the ND-500/5000 System Monitor. In the N-version, there are different versions for ND-500 and ND-5000 systems, and NEW-SYSTEM will decide which version to copy, based on the type of system.  
  This may cause problems in a few cases, for example, if you install SINTRAN on a removable disk and move the disk to another system, the System Monitor may be wrong for this system.  

- When copying SINTRAN III from floppy disk, NEW-SYSTEM also copies the auxiliary files of the SINTRAN III Watchdog (version D). The files copied are:

| File                         | Description                                |
|------------------------------|--------------------------------------------|
| ER-S3WD-DESC-D:EDAT          | ERS/SINTRAN Watchdog Descriptor file       |
| ER-S3WD-LOG-D:PROG           | ERS/SINTRAN Watchdog Log-list program      |
| ER-S3WD-MANA-D:PROG          | ERS/SINTRAN Watchdog Manager program       |

All files will be copied to user area SYSTEM. The descriptor file must reside either on SYSTEM or user area ND-OPERATIONS. The two program files can reside on any user area.  

If two revisions of the description file are found, one on user area SYSTEM and the other on ND-OPERATIONS, the one on ND-OPERATIONS is used.  

Previous versions of the descriptor file (ER-S3WD-DESC-Cxx:EDAT) and the log-list program (ER-S3WD-LOG-Cxx:PROG) are not used, and should be deleted.

---

## Page 15

# 2. Installing SINTRAN III/VSX

This description assumes you are upgrading your system from the M-version. It is also assumed that you are installing the complete SINTRAN III version N package:

- SINTRAN III/VSX, version N
- SINTRAN III Patch File diskette
- ND-5000 Swapper
- ND-500/5000 Monitor
- SINTRAN III Configuration program
- ND-5000 Microprogram

In addition to these products, we recommend that you upgrade your versions of User Environment to the E-version and COSMOS Basic Module to revision G03.

- First, give the two commands: `@DIRECTORY-STATISTICS,,,`  
  and: `@LIST-TITLE`  
  Write down the following information:
  - the device name, unit number and subunit (if any) of the directory marked as "(MAIN AND DEFAULT DIRECTORY)"
  - the CPU number and CPU type of your system.

- Finally, run the old version of S3-CONFIG and select the PRINT option to get a print-out of your previous configuration. This list may be helpful when you set the correct configuration on your new system.

- You may at this point choose to install any new versions of software required and update the files to be run after a cold and warm start, or you may choose to do this at a later stage. In this description we have chosen to wait.

- Stop the system in a controlled way as described in the SINTRAN III System Supervisor manual.

- If you make any mistakes during the loading of SINTRAN III, you must restart from this point.

- Press the STOP and MCL buttons on the front panel.  
  You should now have a # on your terminal.

- Insert SINTRAN III diskette number 1 in FLOPPY-DISC-1 unit 0.

- Give the command: `1560&` (without pressing a return).

- You will then get a list of disk types and you are asked to give the disk type of your system disk. Find the disk type corresponding to the device name you noted earlier and give the type as the number of the disk type in the list.

- You will then get a message telling you which disk type is selected, and then a summary of further commands.

- Wait until you get the message "TYPE ANY MACM COMMAND".

- Type the command: `10,0$` (without pressing a return).

- Wait until you get the message "***** 000000 DIAGNOSTICS *****".

- Remove SINTRAN III diskette number 1 from FLOPPY-DISC-1 unit 0.

---

## Page 16

# SINTRAN III Release Information, N-version

- Insert SINTRAN III diskette number 2 in FLOPPY-DISC-1 unit 0.
- Type the command: `10,0$` (without pressing a ↵).
- Wait until you get the message "***** 000000 DIAGNOSTICS *****".
- Ensure that you got 000000 diagnostics. If not, restart the installation.
- Type the command `22!` (without pressing a ↵).
- Wait until you get the message "PAGES FOR SWAPPING (OCT:) xxxxx".
- You must now enter the main directory of your system:
  - Log in without giving a user area name:
    - Press ESC
    - After "ENTER" press ↵
    - After "PASSWORD" press ↵
  - The message NO MAIN DIRECTORY will be typed.
  - Then give the command:
    - `@ENTER-DIRECTORY` ↵ 
    - and answer the questions for device name, unit (and subunit) with the information you noted earlier about your main directory.
  - Log out:
    - `@LOGOUT` ↵
  - Log in as user area SYSTEM:
    - Press ESC
    - `ENTER SYSTEM` ↵
    - `PASSWORD: <your SYSTEM password>` ↵
- Remove SINTRAN III diskette number 2 from FLOPPY-DISC-1 unit 0.
- Insert SINTRAN III diskette number 3 in FLOPPY-DISC-1 unit 0.
- Give the command:
  - `@ENTER-DIRECTORY,Y,FLOPPY-DISC-1,0` ↵
- Run the NEW-SYSTEM program:
  - `@(2:SYSTEM)NEW-SYSTEM` ↵
- If you get the question "Give CPU number (in decimal): ", answer it by entering the CPU number you noted above.
- If you get the question "Give CPU type (in decimal): ", answer it by entering the CPU type you noted above.
- When asked if you want to run the patch file, answer Y(es).
- Remove SINTRAN III diskette number 3 from FLOPPY-DISC-1 unit 0.
- Insert the Patch file diskette in FLOPPY-DISC-1 unit 0.
- Then answer Y(es) for "ready to continue".
- When asked to do a cold start to set the patches into effect, do the following:
- Remove the Patch file diskette from FLOPPY-DISC-1 unit 0.

---

## Page 17

# SINTRAN III Release Information, N-version

- If your system needs any special or local patches, run your local (system-specific) patch file at this point.

- You should now install the rest of the SINTRAN III version N package:

  - Insert SINTRAN III additional diskette number 1 in FLOPPY-DISC-1 unit 0.

  - Give the command:  
    `@ENTER-DIRECTORY,,FLOPPY-DISC-1,0`

  - Delete any old version of the SINTRAN III configuration program and copy the new version of the program to disk:  
    `@DELETE-FILE S3-CONFIG:PROG`  
    `@COPY-FILE "S3-CONFIG-F:PROG" (211305D:SYSTEM)S3-CONFIG-F:PROG`

  - If your system includes Net/One, you should also install the NOTS-Service program delivered on the same diskette:  
    Delete any old version of the NOTS-Service program and copy the program to disk:  
    `@DELETE-FILE NOTS-SERVICE:PROG`  
    `@COPY-FILE "NOTS-SERVICE-D:PROG" (211305D:SYSTEM)"NOTS-SERV:PROG"`

  - If your system is an ND-5000 system, you should install the new version of the ND-500/5000 Monitor.  
    **Note:** If you are running an ND-500 system, you should keep the K-version of the ND-500 Monitor. You should not install the ND-500 Monitor version L in this case.  
    First, delete any old version of the ND-500/5000 Background Monitor and copy the new version to disk:  
    `@DELETE-FILE ND-500-MON:PROG`  
    `@COPY-FILE "ND-500-MON-L:PROG" (211305D:SYSTEM)ND-500-MON-L:PROG`

  - Furthermore, if your system is an ND-5000 system, you should install the new version of the ND-5000 Swapper.  
    **Note:** If you are running an ND-500 system, you must keep the L-version of the ND-500/5000 Swapper. You should not install the ND-5000 Swapper version M in this case.  
    First, delete any old version of the ND-500/5000 Swapper and then copy the new version to disk:  
    `@DELETE-FILE SWAPPER:PSEG`  
    `@DELETE-FILE SWAPPER:DSEG`  
    `@COPY-FILE "SWAPPER-M:PSEG" (211305D:SYSTEM)SWAPPER-M:PSEG`  
    `@COPY-FILE "SWAPPER-M:DSEG" (211305D:SYSTEM)SWAPPER-M:DSEG`

  - Give the command:  
    `@RELEASE-DIRECTORY 211305D`

  - Remove the SINTRAN III additional diskette number 1 from FLOPPY-DISC-1 unit 0.

---

## Page 18

# ND-5000 Micro Program Installation

- The following commands to install the a new version of the ND-5000 Micro program assumes that your system is either ND-5700, ND-5800, ND-5830 or ND-5850.
- Insert SINTRAN III additional diskette number 2 in FLOPPY-DISC-1 unit 0.
- Give the command:
  
  ```
  @ENTER-DIRECTORY,,FLOPPY-DISC-1,0
  ```

- Give the following command:

  ```
  @COPY-FILE CONTROL-STORE:DATA (211305D:SYSTEM)MICRO-xxxx:DATA
  ```

  Substituting xxxx with 5850, 5830, 5800 or 5700 depending on the type of ND-5000 you have.

- Give the command:

  ```
  @RELEASE-DIRECTORY 211305D
  ```

- Remove the SINTRAN III additional diskette number 2 from FLOPPY-DISC-1 unit 0.

# Configuration

- If you want to change the configuration of your system, run the configuration program:  

  ```
  @S3-CONFIG
  ```

  And change the appropriate parameters. You should at least adjust the number of background processes, spooling programs and ND-500 processes, set the spooling device numbers, and define the line printer parameters. Remember to answer YES when asked if you want to save the changes, or use the GENERATE option explicitly.

- Even if you do not want to change the configuration, you should run the SINTRAN Configuration program to update SINTRAN III according to your configuration:

  ```
  @S3-CONFIG,GENERATE
  ```

# COSMOS Basic Module Installation

- If you plan to install the new revision of COSMOS Basic Module (revision G03), this may be done at this point:

  - Insert the COSMOS Basic Module diskette in FLOPPY-DISC-1 unit 0.

  - Give the command:

    ```
    @ENTER-DIRECTORY,,FLOPPY-DISC-1,0
    ```

  - Decide which user area to use for the COSMOS Basic Module files, and ensure that this user area has at least 415 pages.

  - Copy the installation program for the COSMOS Basic Module to disk:

    ```
    @DELETE-FILE IN-COS-BAS:PROG
    @COPY-FILE,"IN-COS-BAS-G03:PROG",(2:F-U)IN-COS-BAS-G:PROG
    ```

  - Give the command:

    ```
    @IN-COS-BAS-G
    ```

    And follow the instructions from the program. Note that if you chose to install only parts of the COSMOS Basic Module, the two relevant start-up files (COS-HENT-H:MODE and COS-START-G:MODE) must be updated manually to reflect the actual installation.

- Remember to remove the COSMOS Basic Module diskette from FLOPPY-DISC-1 unit 0.

---

## Page 19

# SINTRAN III Release Information, N-version

- Then give the command:  
  `@COLD-START ⏎`

- Wait until you get the message "PAGES FOR SWAPPING (OCT:) xxxxx".

- You must now (again) enter the main directory of your system:  
  - Log in without giving a user area name:  
    Press ESC  
    After "ENTER" press ⏎  
    After "PASSWORD" press ⏎  

  - The message NO MAIN DIRECTORY will be typed.

  - Then give the command:  
    `@ENTER-DIRECTORY ⏎`  
    and answer the questions for device name, unit (and subunit) with the information you noted about your main directory.

  - Log out:  
    `@LOGOUT ⏎`

  - Log in as user area SYSTEM:  
    Press ESC  
    ENTER SYSTEM ⏎  
    PASSWORD: `<your SYSTEM password> ⏎`

- Run the mode file HENT-MODE:MODE (to be run after a cold start):  
  `@MODE HENT-MODE:MODE,"HENT-MODE:OUT" ⏎`

- If you plan to install the new version of User Environment (version E00), this may be done at this point:
  - Insert the first User Environment diskette in FLOPPY-DISC-1 unit 0.

  - Give the command:  
    `@ENTER-DIRECTORY,,FLOPPY-DISC-1,0 ⏎`

  - Decide which user area to use for the User Environment database files.  
    If you want to use the default user area in version E (UE-DATABASE-USER), you must create this user area, allocate the necessary disk space, and move the files UE-DATABASE::xxxx from user area USER-ENVIRONMENT; Furthermore, ensure that the user area USER-ENVIRONMENT has 500 pages free.

  - Give the command:  
    `@(2:FLOPPY-USER)IN-UE-:.E ⏎`  
    and follow the instructions from the program, including change of diskettes.

  - Remember to remove the last User Environment diskette from FLOPPY-DISC-1 unit 0.

---

## Page 20

# SINTRAN III Commands

## 3.1 Modified commands

### 3.1.1 @CHANGE-DIRECTORY-ENTRY

It is now possible to access the extended part (first 10∝ words - addresses 1750∝-1757∝) of the directory entry.

Use negative displacement to change the extended part:
- -1 Number of pages available, least significant part (word 1757∝)
- -2 Number of pages available, most significant part (word 1756∝)
- -3 System number last entering (word 1755∝ in master block)
- -4 Flag word (word 1754∝ in master block)
- -5 Reserved (word 1753∝ in master block)
- -6 Reserved (word 1752∝ in master block)
- -7 Reserved (word 1751∝ in master block)
- -10 Checksum (word 1750∝ in master block)

### 3.1.2 @INITIAL-COMMAND

The length of the buffer used to keep initial commands is now increased to 508 characters.

### 3.1.3 @LIST-INITIAL-COMMANDS

The length of the buffer used to keep *initial* commands is now increased to 508 characters.

### 3.1.4 @LOOK-AT

The LOOK-AT command now displays both the octal contents of a word as well as the ASCII equivalent. Non-printable characters are shown as space.

### 3.1.5 @NEXT-INITIAL-COMMAND

The length of the buffer used to keep initial commands is now increased to 508 characters.

### 3.1.6 @RTCLOSE-FILE

The command RTCLOSE-FILE now reports the closing of an RT-open file to be logged by the ERS/SINTRAN III Watchdog. This applies unless the file was opened by the command RTOPEN-FILE from the same process.  
See also MON CLOSE (MON 43).

---

## Page 21

# 3.1.7 @SET-ERROR-DEVICE

If a terminal is temporarily assigned as error device (using the command SET-ERROR-DEVICE), and the terminal is logged out, the error device is reset to the device defined as the default error device by the command *SET-DEFAULT-ERROR-DEVICE in the SINTRAN Service Program.

# 3.1.8 @WHO-IS-ON

The WHO-IS-ON command now reports information about the type of connection.

The following types are used: IP address  
TAD  
NOTS  
Batch  
Terminal PIO  
MTAD-100  
MTAD-500(0)

The parameter syntax is unchanged.

Example:

|     | SYSTEM |                |
|-----|--------|----------------|
| 1   | SYSTEM | TERMINAL-PIO   |
| 56  | SYSTEM | NOTS           |
| 1072| SYSTEM | MTAD-500(0)    |
| 1475| SYSTEM | IP-address: 130.067.226.004 |
| 768 | SYSTEM | TAD            |
| 670 | SYSTEM | BATCH          |

# 3.1.9 @ (RECOVER without giving the command name)

The command can now be used to start :MODE files as well as both ND-100 programs (:PROG-files) and ND-500/5000 domains.

That the order of commands/files attempted when a name is given without giving the explicit command name (RECOVER), user area and/or file type, is then as follows:  
1. SINTRAN III commands (if the command name RECOVER is omitted).  
2. ND-100 reentrant subsystem and ND-500/5000 standard domains.  
3. `<file>`:DOM (if the ND-500/5000 Monitor is a reentrant subsystem) both the current user area and the user area SYSTEM are searched in the following order: current user area first, then user area SYSTEM.  
4. `<file>`:PROG both the current user area and the user area SYSTEM are searched in the following order: current user area first, then user area SYSTEM.  
5. `<file>`:MODE both the current user area and the user area SYSTEM are searched in the following order: current user area first, then user area SYSTEM.  

Note that this implies that if you have two programs, one for ND-100 and the other for ND-500/5000 with the same name (:DOM and :PROG files with same file name) and omit the file type, the ND-500/5000 program is started; if you have two programs, one for ND-100 and the other for ND-500/5000 and a mode file with the same name (:DOM, :PROG and :MODE files with same file name) and omit the file type, the ND-500/5000 program is started.

---

## Page 22

# 4. Monitor Calls (ND-100)

## 4.1 Modified monitor calls

### 4.1.1 CLOSE MON 43

MON CLOSE (MON 43) used from an RT-program to close a file not opened by the program itself, now reports the closing of the RT-open file to be logged by the ERS/SINTRAN III Watchdog.

### 4.1.2 IOMTY MON 336

One new function has been introduced:

function 27ø : get IP address and TAD information.

#### Function no. 27ø:

**Function:**  
GetIP address and TAD information.

**Monitor call format:**

|            |                                                                                            |
|------------|--------------------------------------------------------------------------------------------|
| LDA        | (PARLI % A = address of parameter list                                                     |
| MON 336    | % IOMTY                                                                                      |
| JMP ERROR  | % error return                                                                             |
| .......... | % normal return                                                                            |

| PARLI, FUNC | % address of function |
| SIZE        | % address of the length of the parameter array                                            |
| ARRAY       | % address of the parameter array                                                         |

| FUNC,       | 27 % function (27ø)                                                                       |
| SIZE,       | 4 % length of parameter array                                                             |
| ARRAY,      | 0 % function parameter 1 (word 1)                                                         |
|             | 0 % function parameter 2 (word 2)                                                         |
|             | 0 % function parameter 3 (word 3)                                                         |
|             | 0 % function parameter 4 (word 4)                                                         |

**Input parameters:**  
Word 1 = Logical device number.

**Output parameters:**  
Word 2 = Port number  
Word 3 = First part of a magic number  
Word 4 = Second part of a magic number  

**Rules:**  
1. Available to all users.  
2. Available on ND-100 and ND-500/5000 systems.  

Refer to page 23 for a description of this function used from ND-500/5000 programs.

---

## Page 23

# 4.1.2 CONFG MON 343

One configuration parameter has been extended (52a).

## Function

GetIP address and TAD information.

## Monitor call format:

```
LDA    (PARLI  % A = address of parameter list
MON    343     % CONFG
JMP    ERROR   % Error handling
........       % normal return
```

```
PARLI,  (FUNC   % Function code
        (INDEX  % Configuration parameter number
        (SUBIN  % Subindex (only used for some values of INDEX)
        (VALUE  % Input and/or output value (integer or string)
```

The following values of the function code are used, but not all functions apply to all configuration parameters:

| FUNC = | Description |
|--------|-------------|
| 1 : (Save) | Read value from SINTRAN III save area (next value to be used after a cold start) |
| 2 : (Read) | Read current active value |
| 3 : (Write) | Write value to SINTRAN III save area (next value to be used after a cold start) |
| 4 : (Generated) | Read generated value |
| 5 : (Free) | Read currently unused units |
| 6 : (Special) | Parameter dependent |

MON 343 is restricted to user SYSTEM, and is allowed from RT-programs.

Note that MON 343 is generally intended to be used by the reconfiguration program (S3-CONFIG).

Furthermore, note that a cold start is necessary to make changes come into effect (all changes are made as "write new value to save area").

---

## Page 24

# Configuration parameter 52a:

## Parameter name:
Read XMSG parameters used for calculation of space

## Input parameters:
FUNC = Function, see below.  
INDEX = 52  
SUBIN = Subparameter, see table below.  
VALUE = New value of an XMSG parameter, see table below.

## Subparameter:
A new subparameter (number 10₈) is introduced:  
10₈ Number of transmit datagram elements (variable: X4DGB)

The other (old) subparameters are:

| # | Description | Variable |
|---|-------------|----------|
| 1 | Length of one XT-block | 4TLEN |
| 2 | Length of one XP-block | 4PLEN |
| 3 | Length of one XM-block | 4MLEN |
| 4 | Length of one XL-block | 4LLEN |
| 5 | Length of one XD-block and XF-block | 5FLEN |
| 6 | Length of function block | X5FUN |
| 7 | End of segment 76 (S3XMK) | X6TOP |

## Output parameters:
VALUE = Next value (from the SINTRAN III save area) or current active value (depending on function) of an XMSG parameter (see table above).

## Functions allowed for this parameter:
Read current active value

---

## Page 25

# 5. SINTRAN Service Program

## 5.1 Modified commands

### 5.1.1 *CHANGE-VARIABLE

The following new variables can now be accessed:

| Name      | Description                                                                                  |
|-----------|----------------------------------------------------------------------------------------------|
| DELWR     | Enable (1) or disable (0) the use of the file-system cache.                                  |
| MXSUSPEND | Maximum number of basic time units an ND-500/5000 process may use the CPU before it is suspended by the time slicer. |
| SUSPTIME  | Number of basic time units an ND-500/5000 process is suspended when it has used too much CPU time. |

### 5.1.2 *FILE-SYSTEM-EVENT-LOG

The following additional events may be selected by the subcommands ENABLE-EVENT and DISABLE-EVENT:

|            |                                                                                     |
|------------|-------------------------------------------------------------------------------------|
| CREATE-FRIEND    | use of the command @CREATE-FRIEND.                                                  |
| DELETE-FRIEND    | use of the command @DELETE-FRIEND.                                                  |
| SET-FRIEND-ACCESS| use of the command @SET-FRIEND-ACCESS.                                              |

Furthermore, the following events will now always be reported:

|              |                                                                                                  |
|--------------|--------------------------------------------------------------------------------------------------|
| DISABLE-ERROR| use of the command *FILE-SYSTEM-EVENT-LOG subcommand DISABLE-ERROR.                              |
| DISABLE-EVENT| use of the command *FILE-SYSTEM-EVENT-LOG subcommand DISABLE-EVENT.                              |
| DISABLE-LOG  | use of the command *FILE-SYSTEM-EVENT-LOG subcommand DISABLE-LOG.                                |
| ENABLE-ERROR | use of the command *FILE-SYSTEM-EVENT-LOG subcommand ENABLE-ERROR.                               |
| ENABLE-EVENT | use of the command *FILE-SYSTEM-EVENT-LOG subcommand ENABLE-EVENT.                               |
| ENABLE-LOG   | use of the command *FILE-SYSTEM-EVENT-LOG subcommand ENABLE-LOG.                                 |

---

## Page 26

# 5.2 New commands

## 5.2.1 *LIST-COLDSTART-MODE-FILE

List the values for parameter list to @ENTER-DIRECTORY and the mode and output files to be run after a cold start. The command has one parameter.

**Parameters:**

| Parameter   | Description                                                        |
|-------------|--------------------------------------------------------------------|
| `<output file>` | output file to receive the parameter list and file names (default = TERMINAL). |

## 5.2.2 *LIST-DEFAULT-ERROR-DEVICE

List the logical device number of the device to be assigned as the default error device. The command has one optional parameter.

**Parameters:**

| Parameter       | Description                                                        |
|-----------------|--------------------------------------------------------------------|
| `<output file>` | output file to receive the device number (default = TERMINAL).     |

## 5.2.3 *LIST-VARIABLES

List the values of the variables which may be changed by the command CHANGE-VARIABLE. The command has one parameter.

**Parameters:**

| Parameter       | Description                                                        |
|-----------------|--------------------------------------------------------------------|
| `<output file>` | output file to receive the list of values (default = TERMINAL).    |

## 5.2.4 *SET-DEFAULT-ERROR-DEVICE

Set the logical device number of the device assigned as the default error device. The command has one parameter.

**Parameters:**

| Parameter       | Description                                                                                          |
|-----------------|------------------------------------------------------------------------------------------------------|
| `<device number>` | the logical device number of the device (for example a terminal) which will normally be assigned as the error device (octal value). |

The device number assigned as the default error device is used if a temporarily assigned error device is logged out.

---

## Page 27

# 6. ND-500/5000 Monitor

The ND-500/5000 Background Monitor version K or later and the ND-500/5000 Swapper version L or later are intended to be used under SINTRAN III version M and later.

On ND-5000 systems, the M-version of the ND-5000 Swapper is recommended as it offers significantly improved performance on most systems. Furthermore, the L-version of the ND-500/5000 Monitor is recommended as it supports the M-version of the ND-5000 Swapper.

Note that the M-version of the ND-5000 Swapper can **not** run on ND-500 systems.

## 6.1 Installation procedure

All software required to run an ND-500/5000 system is delivered together with SINTRAN III/VSX, version N.

## 6.2 Configuration limitations

- LAMUs to be used from the ND-500/5000 may now be placed above the old 32 Mbyte limit.

## 6.3 Modified commands to SINTRAN III affecting the ND-500/5000

### 6.3.1 @ (RECOVER without giving the command name)

The command can now be used to start :MODE files as well as both ND-100 programs (:PROG-files) and ND-500/5000 domains.

This means that the order of commands/files attempted when a name is given without giving the explicit command name (RECOVER), user area and/or file type, is as follows:

1. SINTRAN III commands (if the command name RECOVER is omitted).
2. ND-100 reentrant subsystem and ND-500/5000 standard domains.
3. `<file>::DOM` (if the ND-500/5000 Monitor is a reentrant subsystem) both the current user area and the user area SYSTEM are searched in the following order: current user area first, then user area SYSTEM.
4. `<file>::PROG` both the current user area and the user area SYSTEM are searched in the following order: current user area first, then user area SYSTEM.
5. `<file>::MODE` both the current user area and the user area SYSTEM are searched in the following order: current user area first, then user area SYSTEM.

Note that this implies that if you have two programs, one for ND-100 and the other for ND-500/5000 with the same name (`::DOM and ::PROG files with same file name) and omit the file type, the ND-500/5000 program is started; if you have two programs, one for ND-100 and the other for ND-500/5000 and a mode file with the same name (`::DOM, ::PROG and ::MODE files with same file name) and omit the file type, the ND-500/5000 program is started.

---

## Page 28

# 6.4 Modified commands to the ND-500/5000 Monitor version L

## 6.4.1 LIST-TABLE

In the output from the tables LAST-N500-MSG and TRACK-SWAPPER-MESSAGES, the layout is changed to increase readability.

The following Swapper message function codes are now used (octal codes):

| Code | Function        | Code | Function     |
|------|-----------------|------|--------------|
| 0    | FixSegment      | 17   | InitSegment  |
| 1    | UnfixSegm       | 20   | RestartSwapper |
| 2    | N100 Overlap    | 21   | Statistics   |
| 3    | IncreaseMem     | 22   | SetSysPars   |
| 4    | DecreaseMem     | 23   | FlushSegm    |
| 5    | InitSwapper     | 24   | RestaAlloc   |
| 6    | ProcOutOfMem    | 25   | SegmLimits   |
| 7    | StartSwapper    | 26   | ForgModifPages |
| 10   | ForgetProc      | 27   | Sw MC-510    |
| 11   | InitProc        | 30   | CreateSegm   |
| 12   | Page Fault      | 31   | Init-S4      |
| 13   | MemoryErr       | 32   | Close-S4     |
| 14   | ND 500 MC       | 33   | Prep I/O     |
| 15   | SetPriority     | 34   | ForgetDom    |
| 16   | ForgSegment     | 35   | InitDomPool  |

The following MON 377 function codes are now used:

| Code | Function       |
|------|----------------|
| 0    | SwappFatal     |
| 1    | NewSwap        |
| 2    | Abstrans       |
| 3    | SuspendProc    |
| 4    | AllocPage      |
| 5    | DecrMemTo100   |
| 6    | ClrTSB etc     |
| 7    | MultiElTrnsf   |

## 6.4.2 LOOK-AT

All the LOOK-AT commands allow you to enter a stack pointer on the STACK subcommand.

Example:

STACK 1'24760

## 6.4.3 LOOK-AT-CONTROL-STORE

This command now supports ND-5830/5850. It no longer supports ND-500 systems.

## 6.4.4 LOOK-AT-HARDWARE

This command now supports ND-5830/5850. It no longer supports ND-500 systems.

---

## Page 29

# 6.4.5 LOOK-AT-STACK

This command allows you to go past the "top of stack". This is useful if you want to view the last stack frame. Note that there is no check for consistency here.

# 6.4.6 SWAPPING-LOG

The layout of the output is changed to increase readability.

Example:

ND-5000: swapping-log 10

```
SWAPPING LOG: 10. 9.35             24 SEPTEMBER 1992
               IN INTERVAL   AVERAGE/INT.   SUM INTERVALS
Free memory          5             5               5
Disk buf hit ratio   68            68              68
Procs forced out     0             0               0
Pages reset pgu      358           358             358
No. of page faults   568           568             568
File ix hit ratio    31            31              31
Pages from absfree   549           549             549
Pages written back   25            25              25
Calls to swapper     535           535             535
ND-100 disk transf.  193           193             193
Domino transfers     51            51              51
Allocate page oper.  0             0               0
Flush pages checked  68            68              68
    -pages written   19            19              19
    -transfers       3             3               3
Clear TSB all        32            32              32
    TSB prog         9             9               9
    -TSB data        23            23              23
    -Cache data      45            45              45
Threads -free        9             9               9
    -S3wait          0             0               0
    -DOMwait         0             0               0

<ESC>
```

---

## Page 30

# 6.5 New commands to the ND-500/5000 Monitor version L

## 6.5.1 SWAP-TABLE

Display one of the tables used internally in the ND-5000 Swapper for statistics.

The commands has two parameters:

| Parameter     | Description                              |
|---------------|------------------------------------------|
| \<table number\> | the number of the table to be displayed.  |
| \<interval\>      | interval in seconds between each time the table is displayed (decimal value). |

The following table numbers (and restrictions) are implemented:
- 0 - Page faults per segment (segment no. > 199 can not be displayed)
- 1 - Page faults per process (process no. > 199 can not be displayed)
- 2 - Swapper message type usage
- 3 - Memory used per process (process no. 0 is assigned shared segments, process no. > 199 can not be displayed)

Examples:

```
ND-5000: SWAP-TABLE,0,10

Page faults per segment:        14.46.28        10 DECEMBER 1992

|     | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B |
|-----|----|----|----|----|----|----|----|----|----|----|
| 0B  | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B |
| 12B | 0B | 0B | 0B | 0B | 10B | 12B | 0B | 0B | 0B | 0B |
| 24B | 0B | 0B | 0B | 170B | 103B | 100B | 200B | 0B | 0B | 0B |
| 36B | 40B | 33B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B |
| 50B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 400B |
| 62B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B |
| 74B | 0B | 0B | 0B | 117B | 0B | 0B | 0B | 0B | 0B | 0B |

ND-5000: SWAP-TABLE,1,10

Page faults per process:        16.53.11        10 DECEMBER 1992

|     | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B |
|-----|----|----|----|----|----|----|----|----|----|----|
| 8B  | 0B | 0B | 12B | 0B | 0B | 0B | 0B | 0B | 0B | 0B |
| 12B | 357B | 13B | 0B | 0B | 61B | 62B | 121B | 0B | 0B | 0B |
| 24B | 0B | 0B | 5B | 0B | 0B | 0B | 0B | 0B | 0B | 0B |
| 36B | 0B | 700B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B |
| 50B | 0B | 0B | 3B | 0B | 0B | 0B | 365B | 0B | 0B | 0B |
| 62B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B |

ND-5000: SWAP-TABLE,2,10

Swapper message usage :        16.53.36        10 DECEMBER 1992

|     | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B |
|-----|----|----|----|----|----|----|----|----|----|----|
| 0B  | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B |
| 12B | 63B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B |
| 24B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B |

(swapper message number 12 is "page fault")

ND-5000: SWAP-TABLE,3,10

Memory used per process:        16.54. 4        10 DECEMBER 1992

|     | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B |
|-----|----|----|----|----|----|----|----|----|----|----|
| 0B  | 3740B | 0B | 421B | 17B | 17B | 414B | 100B | 40B | 24B | 22B |
| 12B | 23B | 13B | 12B | 10B | 11B | 54B | 16B | 216B | 0B | 0B |
| 24B | 0B | 0B | 1157B | 514B | 0B | 0B | 0B | 0B | 0B | 0B |
| 36B | 2152B | 555B | 0B | 0B | 0B | 0B | 471B | 1037B | 501B |
| 50B | 1125B | 0B | 671B | 0B | 0B | 320B | 0B | 0B | 0B | 0B |
| 62B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B |
| 74B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B | 0B |
```

---

## Page 31

# 6.6 Modified monitor calls to the ND-100 affecting ND-500/5000

## 6.6.1 CLOSE MON 43

MON CLOSE (MON 43) used from an RT-program to close a file not opened by the program itself, now reports the closing of the RT-open file to be logged by the ERS/SINTRAN III Watchdog.

# 6.7 Modified monitor calls (ND-500/5000)

## 6.7.1 IOMTY MON 336

One new function has been introduced:

function 27ø : get IP address and TAD information.

**Function no. 27ø:**

**Function:**  
Get IP address and TAD information.

**Monitor call format:**  
CALLG 370000000336B,4,<function>,<size>,<array>,<status>

**Parameters:**  
| Parameter    | Description                 |
|--------------|-----------------------------|
| <function>   | function = 27ø.             |
| <size>       | size of parameter array.    |
| <array>      | parameter array.            |
| <status>     | returned status.            |

**Input parameters:**  
Parameter 1 = Logical device number.

**Output parameters:**  
| Parameter    | Description                     |
|--------------|---------------------------------|
| Parameter 2  | Port number                     |
| Parameter 3  | First part of a magic number    |
| Parameter 4  | Second part of a magic number   |

**Rules:**  
1. Available to all users.  
2. Available on ND-100 and ND-500/5000 systems.

Refer to page 14 for a description of this function used from ND-100 programs.

# 6.8 Modified monitor calls - available only on ND-500/5000

## 6.8.1 AT5SGM MON 440 (Attach500Segment)

Function 3 (MALLOC) may now be used to fix a segment above the old 32 Mbyte limit when specified explicitly (fix codes 3, 4, or 5).

---

## Page 32

# 7. ND-5000 Swapper version M

There are several reasons that make this Swapper better than its predecessors: Parallelism has been introduced, the paging algorithm has been improved, more buffers are used, TSB handling has been improved, a cache for disk read cache has been introduced and other minor changes.

## 7.1 Major improvements

### Multi-threading

Multi-threading is a general term, but in this context it means that the Swapper is able to serve several virtual memory management operations in parallel (an example of such an operation is the PageFault).

To do this, the memory management process (the Swapper) must be able to manage several contexts (threads) at the time. This version of the Swapper has 9 threads available for general use. A 10th thread is dedicated for internal clean-up. Multi-threading in the Swapper alone does not make parallelism, it must be available both on the process communication side and on the I/O communication side as well.

Three calls are made multi-thread: PageFault, Flush (WSEG) and the statistics call.

### Asynchronous communication

Multi-threading is useless without this feature and this feature is useless without multi-threading. This must be available both on the process side and on the I/O side.

On the I/O communication side, the following functions are made asynchronous:
- Allocate page towards the SINTRAN file system.
- Disk transfers against SINTRAN disks
- Disk transfers against Domino disks.

On the process communication side, the following functions are made asynchronous:
- New tasks to the Swapper is given to it by the ND-500 driver using a FIFO queue.
- Restart of processes through the ND-500 driver using Octobus kick.
- Direct restart of processes by manipulating messages (same way as the μ-code).

### Paging algorithm

A few adjustments have been done to the paging algorithm. The basic rules are still:
- Nobody is given memory unless they generate a pagefault for it or require FIX.
- Memory are primarily taken from totally free memory.
- If no memory is free, it is taken from others according to certain strategies.

These strategies are adjusted somewhat:
- Segments are not removed from the SwapOutQ unless they are empty. This ensures that all unused memory are freed, including memory that has been written to.
- Freeing of pages are done in batches of 30. This causes less clearing of the TSB, which in turn gives faster execution.
- When ForceProcessToSwapOutQ is necessary, all processes are forced out. This is done to prevent punishing one process all the time (like the previous versions did).

---

## Page 33

# Clear TSB

Has been optimised. In most cases due to Batch Freeing of memory.

# Disk caching

One ½ Mbyte CopyExcusive cache has been introduced.

# Larger buffers

Flush and FileIndex buffers have been increased.

# Communication

Communication towards processes, SINTRAN and disks are now asynchronous.

## 7.2 Possible case were it might be worse

The thread handling implies a minor overhead, thus one or two processes running single thread with enough memory, might run slightly slower. However, due to other improvements in the Swapper even these situations will normally run as fast or faster.

## 7.3 Memory usage

This version of the Swapper will use considerably more memory than the previous versions. Here is an overview of the number of pages used by this version and the previous version for some specified areas:

| Swapper version          | M00  | L04                |
|--------------------------|------|--------------------|
| Swapper program          | 53   | 43                 |
| Swapper data             | 17   | 100                |
| Copy exclusive cache     | 256  | 0                  |
| Flush buffer             | 41   | 21                 |
| Various buffers          | 75   | 70                 |
| Task data                | 24   | 0                  |
| Software segment table   | 130  | 128                |
| Software memory map max. | 256  | 128 50 pages / 16 Mbyte |

| Total minimum (10 Mbyte) | 620  | 384 |
| Total                    | 852  | 490 |

As the Swapper now allocates memory for many internal tables at start-up, the :DSEG file is smaller.

---

## Page 34

# 7.4 Postmortem information

If the Swapper fails then it is wise to take a dump of it, and send it in for inspection.

The most convenient way to do this, is to dump memory to either a streamer cartridge or to a set of diskettes. This procedure is described on pages 34-35.

If you cannot make a complete dump of memory, you can dump only the Swapper itself. The procedure is then as follows (you will end up with two files of type :DUMP).

```
@ND-500-MON
DUMP-SWAPPER
"sw-<date>-1"
DUMP-PHYSICAL-SEGMENT
"sw-<date>-6"
6
EXIT
```

This information can also be extracted from a SINTRAN dump, so if there are other problems this should be taken instead.

# 7.5 Changed error handling

The Swapper is now more tolerant to error conditions as several errors conditions have been made non-fatal.

A list of new error message (both fatal and non-fatal) is found on pages 37-39.

Errors from the Swapper are now reported through the ERS/SINTRAN Watchdog.

Furthermore, the Swapper now reports status and error messages through DP-100 if available (DP-100 is by default available if the system has Domino disks). However, we recommend that you have DP-100 loaded even if the system does not use Domino disks.

---

## Page 35

# 8. File System

## 8.1 Changes in configuration limitations

- The number of BDIO pools supported in standard configuration A is increased to 32.
- The size of the name table is increased to accommodate the increased number of BDIO pools used in the standard configurations.

## 8.2 Performance

The file system cache is changed. This implies introduction of delayed write operations ("dirty cache") which is flushed to disk when necessary.

Flushing is always done when:

- The least recently used buffer in the file system cache is about to be used again.
- The buffer has not been accessed for 60 seconds.
- One of the commands @RESTART-SYSTEM, @STOP-SYSTEM, or @COLD-START is given (implies flush of the complete file system cache).
- The command @RELEASE-DIRECTORY is given (implies flush of all buffers for a specific directory).
- One of the commands @RENAME-USER, @CREATE-USER, @DELETE-USER is given (implies flush of the complete file system cache).

The delayed write option may be disabled by using the SINTRAN Service Program command *CHANGE-VARIABLE to set the variable DELWR to 0 (setting it to 1 means enable the option).

## 8.3 Changes to the file system event-log utility

The following additional events may be selected by the subcommands ENABLE-EVENT and DISABLE-EVENT:

| Event               | Command                                   |
|---------------------|-------------------------------------------|
| CREATE-FRIEND       | use of the command @CREATE-FRIEND.        |
| DELETE-FRIEND       | use of the command @DELETE-FRIEND.        |
| SET-FRIEND-ACCESS   | use of the command @SET-FRIEND-ACCESS.    |

Furthermore, the following events will now always be reported:

| Event             | Command                                                    |
|-------------------|------------------------------------------------------------|
| DISABLE-ERROR     | use of the command *FILE-SYSTEM-EVENT-LOG subcommand DISABLE-ERROR.     |
| DISABLE-EVENT     | use of the command *FILE-SYSTEM-EVENT-LOG subcommand DISABLE-EVENT.     |
| DISABLE-LOG       | use of the command *FILE-SYSTEM-EVENT-LOG subcommand DISABLE-LOG.       |
| ENABLE-ERROR      | use of the command *FILE-SYSTEM-EVENT-LOG subcommand ENABLE-ERROR.      |
| ENABLE-EVENT      | use of the command *FILE-SYSTEM-EVENT-LOG subcommand ENABLE-EVENT.      |
| ENABLE-LOG        | use of the command *FILE-SYSTEM-EVENT-LOG subcommand ENABLE-LOG.        |

---

## Page 36

# 9. XMSG

## 9.1 Changed configuration limitation

The size of the kernel is reduced to allow slightly larger buffer space (1008 words).

---

## Page 37

# ND-5000 Microprograms

The microprograms for ND-5800 and ND-5700 (version F) contain the following modifications:

- Corrections in the AXI-instruction, which lost floating precision on the operation a\*n (n < 0).
- Corrections in a wait for a semaphore, introducing a 13-second timeout.
- Changes to the Octobus driver to release the execution queue if the microprogram has reserved the queue and the microprogram must wait more than 10 microseconds. This is done to avoid Lock timeout 2132₈ and timeout 2000₈.
- Corrections in CLREAD, NXGET and break.
- Corrected CONVxxx instructions to handle operators (B-operand) with index = 1.
- Changes in MON 515 (5MTRANS); the microprogram will always give an interrupt to the ND-100 and then continue scanning the execution queue without waiting for an answer from the ND-100.
- Changes in MON 504 and MON 511 (Instring, Outstring), affecting SINTRAN version M or later; the microprogram will remove parity when using 8-bit terminals.
- Changed MON 335 (EXABS) to dump dirty.
- Corrected write-back of data buffer in restart after monitor call error in testing of the data buffer (exceeds 2048 bytes).
- A separate Nucleus driver for Octobus is introduced.
- The Nucleus driver in the microprogram for ND-5800 is optimised to start processes running in the same CPU directly instead of using the NK server in the ND-100.

The microprograms for ND-5850 and ND-5830 (version B) contain the following modifications:

- Changes in the algorithm on TSET-cycles in the lock routine. While waiting for a semaphore, TSET was run every 2 microseconds. This is now changed to 50-microsecond intervals to avoid hang situations on the MF bus.
- Changes in timing in the instructions ENTS, ENTM, ENTF, and ENTB to avoid a data cache hit on an instruction channel access when I-error. Previously, this led to inconsistency between cache and memory.
- Corrections in the WEXT instruction adding a new Octobus driver.
- Corrections in a wait for a semaphore, introducing a 13-second timeout.
- Changes to the Octobus driver to release the execution queue if the microprogram has reserved the queue, but must wait more than 10 microseconds.
- Corrections to logical data memory read, function 10. This is now always run with paging on to allow for segment trace and multi-CPU.
- Corrected return after trap which previously failed when using the debugger and line mode.
- Corrections in the CALL instruction, which gave an instruction sequence error on a false cache hit in the instruction cache.
- Corrections in the RETX instruction which failed on a TSB error on the second parameter in the instruction pointed at by the return address.
- The Nucleus driver in the micro program is optimised to start processes running in the same CPU directly instead of using the NK server in the ND-100.
- A separate Nucleus driver for Octobus is introduced.

---

## Page 38

# 11. ERS/SINTRAN III Watchdog

The following versions of the different modules of the ERS/SINTRAN III Watchdog are supplied initially as part of SINTRAN III:

The Watchdog program : version D02  
The Manager program : version D02  
The Log-List program : version D03  
The Description file : version D23  

## 11.1 New reporters recognised by the SINTRAN III Watchdog

The following new reporters are supported by the SINTRAN III Watchdog.

This list may be expanded by the installation of a revision of the descriptor file later than ER-S3WD-DESC-D20:EDAT.

| SEC from     | SEC to     | System Module Name  |
|--------------|------------|---------------------|
| 003600B      | 003677B    | ND-5000 Swapper     |
| 016100B      | 016177B    | User Environment    |
| 050300B      | 050377B    | COSMOS Spooling     |

The layout of a report written to the error device:

severity * SSI.EC * date time * RT-program.P-register * systemname.systemnumber  
product name  
event text  
description parameter  
description parameter  

severity : the severity of the reported event (Information/Warning/Error/Fatal)  
SSI : SSI code of the event (octal number)  
EC : Event Code of the event (octal number)  
date : the date when the event was read by the watchdog (on the form YYYY-MM-DD)  
time : the time when the event was read by the watchdog (on the form HH:MM:SS)

---

## Page 39

# 12. SINTRAN III Configuration Program

The F-version of the SINTRAN III Configuration program is distributed with the N-version of SINTRAN III.

The previous versions of the SINTRAN III configuration program were not able to display or input large values to the following two XMSG parameters correctly.

| Parameter | Range | Error Above |
|-----------|-------|-------------|
| Max. buffer space owned by a task in bytes | 256 - 65334 | 32767 |
| Default maximum SABMs when starting link | 0 - 65535 | 32767 |

This is corrected in the F-version.

Furthermore, the size of the table space in the XMSG kernel is now calculated accurately.

---

## Page 40

# 13. Nucleus

The code of the Nucleus system is optimised. This implies better performance for MTADs using Ethernet III and the M-version of the ND-5000 Swapper.

---

## Page 41

# 14. UPS Power Server

The UPS Server for SINTRAN makes it possible to communicate with a UPS (Uninterrupted Power Supply) unit. The UPS server is started at SINTRAN start-up and communicate with the UPS unit via Octobus.

If a power fail lasting more than 10 seconds occur, the UPS Server will detect this and run the mode file (SYSTEM)SHUT-POWERFAIL:MODE (using the file (SYSTEM)SHUT-POWERFAIL:OUT as output file) to shut down the system. This mode file should be edited to include the necessary commands to stop databases, etc.

**Note!**  
Please note that the two files (SYSTEM)SHUT-POWERFAIL:MODE and (SYSTEM)SHUT-POWERFAIL:OUT must not be deleted or renamed.

When the power-fail mode file is finished (and the activity on the system is stopped), the UPS unit is told to turn off power to preserve the batteries.

On ND-5000 systems, the monitor program PS-MONITOR is available to perform adjustments and status display.

---

## Page 42

# 15. MEMTOF / MEMTOS

## 15.1 Introduction

MEMTOF (MEMory TO Floppy dump) has been part of SINTRAN III (installed as part of SINTRAN) since the K-version.  

Now this program is replaced by a new version called MEMTOS (MEMory TO Streamer dump).  

This new version will dump the contents of memory (or the first 32 Mbyte of memory), either to diskettes (as before), or to a streamer cartridge.  

If a streamer is used, it must be connected as ID no. 1 on SCSI adaptor number 1 on the ND-100 part (hardware device number 144300a).  

## 15.2 Operating procedure

The following simple procedure may then be used to dump memory:

- Stop the system (if it is not stopped already).
- Dump the register block (use the OPCOM command 0<17RD )
- Dump the internal registers (use the OPCOM command IRD )
- Press the MCL (master clear) button on the panel. This is important otherwise the register block for the current active interrupt level will be destroyed.
- Type 15! (just 15 and an exclamation mark - without a return)

MEMTOS will then start, and guide you through the procedure of taking a memory dump.  

When the dump is finished, the following message is shown:

-- Dump finished --

and the system is stopped.

## 15.3 Error handling

If errors are detected during operations on tape or floppy disk, the following error message is displayed:

** ERROR - Status = xxxxxx  
Check or replace the media and retry  

xxxxxx is the octal status from the driver or hardware, the most common values are described below.  

The program asks for new media.

---

## Page 43

# SINTRAN III Release Information, N-version

## 15.4 Tape Status Decoding

The status values are the same as for ordinary operations on SCSI devices in SINTRAN. All status values are described in the manuals SINTRAN III Commands Reference Manual (860128.7), SINTRAN III Monitor Calls (860228.3) and ND-100 SCSI Reference Guide (812048). The most common are:

| Code   | Description   |
|--------|---------------|
| 000002 | Not ready     |
| 000007 | Data protect  |

## 15.5 Floppy Disk Status Decoding

The status values are the same as the status word 1 for ordinary operations on floppy disks in SINTRAN. All status values are described in the manuals SINTRAN III Commands Reference Manual (860128.7), SINTRAN III Monitor Calls (860228.3) and Floppy and Streamer Controller 3106/3112 (811021). The most common are:

| Code   | Description               |
|--------|---------------------------|
| 020030 | Drive not ready           |
| 016030 | Write protected diskette  |

---

## Page 44

# User Environment

The E-version of User Environment is designed to both improve performance and security. User Environment version E may only be run on SINTRAN III version N or later. The most important changes are:

- The User Environment server and the login program now run on the ND-5000 part of the system. This implies that the mode file to be run after a warm start (usually called LOAD-MODE:MODE) must be checked, and possibly updated as follows:
  - Ensure that the line @UE-FUNC,START is placed after starting ND-500/5000 and XMSG.
  - Ensure that the line @WAIT-FOR-UE is still placed after @UE-FUNC,START.

- We recommend that at least one terminal is run without User Environment. This is necessary if the ND-500/5000 part of the system is unavailable.

- The password mechanisms are improved to increase security:
  - The old password must now be entered when changing password.
  - Possibility to prevent changing the password back to the previous password (or some of the later passwords).
  - Possibility for getting system-generated passwords.
  - Possibility to set a minimum time between password changes.

- Logging IP address (on terminal or PC) in the activity log and show it on UE-FUNC,TERMINAL-STATUS.

- Possibility to set access control on IP address (similar to terminal number).

- Log remote system ID (when running on a TAD) in the activity log and show it on UE-FUNC,TERMINAL-STATUS.

- All error messages from User Environment are now sent to the SINTRAN III Watchdog.

---

## Page 45

# 17. New Error Messages

## 17.1 Error handling during start of SINTRAN

If an error occurs during start-up of SINTRAN III, an error message is printed to the console and the system is stopped.

## 17.2 SINTRAN III Run-time errors

| Octal number | Message                                          |
|--------------|--------------------------------------------------|
| 1470         | Impossible to read from or write to segment      |
| 1471         | Closing RT-open file not opened by this process  |

## 17.3 Error codes returned from the ND-5000 monitor - numeric list

| Error code: | Message:                                                    |
|-------------|-------------------------------------------------------------|
| Octal | Decimal |
| 2213  | 1163    | Error in message link (Next = 0)                        |
| 2214  | 1164    | Error in message link (Next > Physical memory)          |
| 2346  | 1254    | SWAPPER: Nucleus MsgID inconsistency                    |
| 2347  | 1255    | SWAPPER: "No such page" on writeback                    |
| 2350  | 1256    | SWAPPER: Trying to read process segment from disk on ND-5000 |
| 2351  | 1257    | SWAPPER: Error in InitPool. Device not in legal DOMINO SCSI range. |

## 17.4 Error codes returned from the ND-5000 Swapper

| Error Code: | Message:                                                      |
|-------------|---------------------------------------------------------------|
| Octal | Decimal |
| 3600  | 1920    | Test message                                              |
| 3601  | 1921    | Swapper has started                                       |
| 3602  | 1922    | Swapper initialized - OK                                  |
| 3603  | 1923    | Swapper fatal error                                       |
| 3604  | 1924    | Function terminated                                       |
| 3605  | 1925    | Function terminated due to ESCape                         |
| 3606  | 1926    | Domino transfer error                                     |
| 3607  | 1927    | Direct Domino interaction disconnected due to errors     |
| 3610  | 1928    | Function timeout                                          |
| 3611  | 1929    | (user-defined error message)                              |
| 3612  | 1930    | I/O timeout                                               |
| 3613  | 1931    | Disk transfer error                                       |
| 3677  | 1983    | Internal error in Swapper                                 |

---

## Page 46

# 17.5 Fatal errors returned from the ND-5000 Swapper

| Error Code | Message |
|------------|---------|
| Octal | Decimal |
| 142 | 98 | Inconsistent increment memory message |
| 143 | 99 | Inconsistent increment memory message |
| 144 | 100 | Inconsistent increment memory message |
| 145 | 101 | PreGetPage called illegally |
| 146 | 102 | Inconsistent increment memory message |
| 147 | 103 | Inconsistent increment memory message |
| 150 | 104 | Inconsistent increment memory message |
| 151 | 105 | Problem when mapping in new logical areas |
| 152 | 106 | Problem when remapping memory map segment |
| 153 | 107 | Task status not UnInitialized upon initialization |
| 154 | 108 | Problem when mapping in new logical areas |
| 155 | 109 | Task status not I/O wait when removed from wait |
| 156 | 110 | Status not LockWait when taken out of wait |
| 160 | 112 | Process wait queue is NIL |
| 161 | 113 | Lock not found in process wait queue |
| 162 | 114 | Domino interchanged nucleus messages |
| 171 | 121 | Pre-initialization problems |
| 172 | 122 | Already in wait queue |
| 173 | 123 | LockOwner is NOT ExeqTread when lock is released |
| 174 | 124 | No free task |
| 175 | 125 | LockOwner is NIL when lock is released |
| 176 | 126 | ND-100 FIFO overflow |
| 177 | 127 | Swapper FIFO overflow |
| 200 | 128 | Number of CPUs found is inconsistent |
| 201 | 129 | No CPU found |
| 202 | 130 | Timeout |
| 207 | 135 | Timeout |
| 210 | 136 | Message not ending on 060B |
| 211 | 137 | Sender not in range 1:255 |
| 212 | 138 | Sender and message address not corresponding |
| 213 | 139 | Message address is NIL |
| 214 | 140 | Lock owner is not EcceqTread upon return from Reserve |
| 215 | 141 | Lock waitQ is not NIL but lock is free |
| 216 | 142 | Already waiting |
| 217 | 143 | Message address is not NIL when task is starting up |
| 222 | 146 | Inconsistency in data field |
| 312 | 202 | MicFunc not 5 (message to Swapper) |
| 313 | 203 | Message not ending on 060B |
| 314 | 204 | Sender not in range 1:255 |
| 315 | 205 | Sender and message address not corresponding |
| 316 | 206 | Task for return message is NIL |
| 411 | 265 | In message address is NILswapper) |
| 412 | 266 | Single threading problems |
| 413 | 267 | Message not ending on 060b |
| 414 | 268 | Sender not in range 1:255 |
| 415 | 269 | Task has entries in queues when detaching |

---

## Page 47

# Error Code

| Octal | Decimal | Message |
|-------|---------|---------|
| 416   | 270     | Task for return message is NIL |
| 417   | 271     | Sender end message address not |
| 420   | 272     | ExeqTread not owner of IndexBufferLock when releasing |
| 500   | 320     | Message not ending on 060B |
| 510   | 328     | Message not ending on 060B |
| 550   | 360     | NoWrite page found in write link. |
| 551   | 361     | No waiting when there should be |
| 552   | 362     | Wait when there should not be. |
| 553   | 363     | Transfer entry not found upon release |
| 554   | 364     | Page being written is already in read transfer |
| 556   | 366     | No free transfer entries |

## 17.6 Non-fatal errors returned from the ND-5000 Swapper

| Error Code: | Message:                       |
|-------------|--------------------------------|
| Octal       | Decimal | Message              |
| 001         | 1       | Pointer (bbp) points to itself |
| 015         | 13      | No pages available from own segment |
| 025         | 21      | Illegal segment number |
| 027         | 23      | Domain number ≠ 0    |
| 035         | 29      | Illegal link pointer |
| 045         | 37      | Segment not expandible |
| 050         | 40      | Number of pages in segment is -1 |
| 055         | 45      | Message type (msgtype) outside range |
| 056         | 46      | Filetype is not contiguous |
| 063         | 51      | Number of references ≠ 1 |
| 064         | 52      | Illegal owner        |
| 066         | 54      | Illegal swap function |
| 100         | 64      | Memory page count is inconsistent |
| 102         | 66      | Illegal parameter    |
| 106         | 70      | Domain number ≠ 0    |
| 112         | 74      | Page number ≠ 0      |
| 120         | 80      | Illegal Device       |
| 211         | 137     | Message address not ending on 060B |
| 222         | 146     | Outside array bounds (TSB statistics) |
| 223         | 147     | Segment not Attach or LAMU |
| 230         | 152     | Internal error (PQp.dBlNo ≠ xferBloc) |
| 231         | 153     | Internal error (No ≠ xferBloc) |
| 232         | 154     | Internal error (PQp.dBlNo ≠ LastBln) |
| 233         | 155     | Pointer error (HeadTrFreeQ = nil) |
| 234         | 156     | Internal error (RetEl ≠ PrevEl) |
| 235         | 157     | Internal error (RetEl ≠ CurEl) |
| 236         | 158     | Internal error (PagCount ≠ ttNoOfPages) |
| 237         | 159     | Pointer error (HeadFreeQ = nil) |
| 208         | 200     | Message status ≠ 6 (owned by Swapper) |
| 321         | 209     | Message status ≠ 6 (owned by Swapper) |
| 322         | 210     | Message status ≠ 6 (owned by Swapper) |

---

## Page 48

# 17.7 Error codes returned from ND-5850 Service Partner (James)

| Error Code: | Message:                   |
|-------------|----------------------------|
| Octal  | Decimal |                        |
| 7006   | 3590    | Selftest PROM not valid |
| 7023   | 3603    | Timeout                 |
| 7077   | 3647    | ND-5000 selftest failed |

---

## Page 49

# 18. SINTRAN III N-version, System Layout

## 18.1 System layout on disk

| File              | Contents                                                                 | Disk Address | Size  | Segment Address |
|-------------------|--------------------------------------------------------------------------|--------------|-------|-----------------|
| SINTRAN:DATA      | Common Code <br> Restart/Start                                           | 1B           | 77B   | 0B              |
| MACM-AREA:DATA    | Error Messages <br> RT-Loader                                            | 100B, 137B   | 20B, 41B | 30000B          |
| SEGFIL0:DATA      | Common Code <br> Restart/Start <br> Resident Data <br> System Segment <br> Spooling Data Fields <br> Extended COMMON <br> RPIT <br> MPIT <br> IPIT <br> 5PIT <br> ND-500/5000 System Monitor <br> Segment Table <br> ND-110 Micro Program <br> ND-120 Micro Program <br> File System <br> Command Segment <br> SSP/Mail Segment <br> XMSG Kernel <br> XMSG XROUT Segment <br> XMSG Watchdog <br> Device-name Table <br> Disk Mirroring WD Segment <br> NUCLEUS Server <br> NUCLEUS Name Server <br> ERS Watchdog Program <br> ERS Watchdog Data <br> Processor Manager Server <br> PFTCON Server <br> BOPCOM Server <br> MT Server | 200B, 300B, 355B, 360B, 361B, 363B, 446B, 531B, 614B, 621B, 701B, 721B, 761B, 1021B, 1106B, 1173B, 1237B, 1276B, 1337B, 1425B, 1433B, 1437B, 1523B, 1623B, 1707B, 1777B, 2063B, 2147B, 2233B | 77B, 55B, 3B, 1B, 2B, 63B, 63B, 63B, 5B, 60B, 20B, 40B, 40B, 65B, 65B, 44B, 37B, 41B, 66B, 6B, 4B, 64B, 100B, 64B, 70B, 64B, 64B, 64B, 4B | 30000B, 4000B, 144000B, 164000B, 26000B, 32000B, 32000B, 32000B, 26000B, 40000B, 0B, 0B, 0B, 26000B, 26000B, 30000B, 102000B, 0B, 0B, 164000B, 2000B, 30000B, 0B, 0B, 30000B, 30000B, 30000B, 30000B |

---

## Page 50

# 18.2 Page Index Table Layout

| PIT 0           | PIT 1 - UPITN    | PIT 2 - UPITA   | PIT 3 - FUPIT   |
|-----------------|------------------|-----------------|-----------------|
| 0 Only used during start-up | 0 Users normal PIT | 0 Users alternate PIT | 0 Micro-©<br>2 Common code (©)<br>13 Remote file user PIT |

| PIT 4 - FPIT    | PIT 5 - 5PIT     | PIT 6 - XPIT    | PIT 7 - DPIT    |
|-----------------|------------------|-----------------|-----------------|
| 0 Micro-©<br>2 Common code (©)<br>13 File system segment | 0 Micro-©<br>2 Common code (©)<br>13 MON 60<br>20 ND-500(0) system segment | 0 Micro-©<br>2 Common code (©)<br>13 XMSG | 0 Micro-©<br>2 Resident common data<br>57 Wind.BF<br>Wind.ND-500<br>Wind.1/4<br>62 Sys. segm. Wnd.10/12<br>72 Data segm. |

---

## Page 51

# SINTRAN III Release Information, N-version

## PIT 10 - RPIT

| 0  | Micro-©                   |
|----|---------------------------|
| 2  | Common code (©)           |
| 13 | Extended common(©)        |
| 15 | Monitor calls             |
|    | B-level (level 4)         |

## PIT 11 - SPIT

| 0  | Micro-©                   |
|----|---------------------------|
| 2  | Common code (©)           |
| 13 | Edit segment              |
| 14 | Command segment           |
|    | SSP/Mail segment          |
|    | RT-Loader                 |
|    | DMAC                      |
|    | Error prog.               |

## PIT 12 - MPIT

| 0  | Micro-©                   |
|----|---------------------------|
| 2  | Common code (©)           |
| 13 | Extended common(©)        |
| 15 | Level 2                   |
|    | Level 10                  |
|    | Level 12                  |
|    | Level 13                  |
|    | Level 14                  |
|    | MPERF                     |

## PIT 13 - X5DP1

| 0  | ND-500(0) name-tables segment |
|----|-------------------------------|
|    | Stack wnd.                    |

## PIT 14 - X5DP2

| 0  | ND-500(0) standard domains segment |
|----|-----------------------------------|
|    | Stack wnd.                        |

## PIT 15 - IPIT

| 0  | Micro-©                   |
|----|---------------------------|
| 2  | Common code (©)           |
| 13 | Extended common(©)        |
| 15 | Level 3                   |
|    | Level 11                  |

## PIT 16

| 0  |
|----|

## PIT 17 - DTPIT

| 0  | Direct tasks                   |
|----|--------------------------------|
|    | (Used for mapping DPIT during startup) |

---

## Page 52

# 18.3 System included segments

| Segment No. | Name     | Address Range | PIT | Description                                      |
|-------------|----------|---------------|-----|--------------------------------------------------|
| 2           | S3IMAGE  | 0:175777      | 1   | Image of common code, start/restart               |
| 3           | S3CP     | 30000:177777  | 11  | Command segment                                   |
| 4           | S3RTL    | 30000:123777  | 11  | RT-Loader segment                                 |
| 5           | S3ERRS   | 144000:145777 | 7   | System segment for error program                  |
| 6           | S3FS     | 26000:177777  | 4   | File system segment                               |
| 7           | S3DMAC   | 64000:153777  | 11  | DMAC segment                                      |
| 10          | S3RTFIL  | 0:177777      | 2   | RTFIL segment                                     |
| 11          | S3ERRL   | 0:17777       | 1   | Error log segment                                 |
| 12          | S3SFS    | 26000:177777  | 1   | Save of file system segment                       |
| 13          | S3SCP    | 26000:177777  | 1   | Save of command segment                           |
| 14          | S3ERRP   | 30000:67777   | 11  | Error program segment                             |
| 15          | S3BFLY   | 26000:26000   |     | Reserved, but not used                            |
| 16          | S3SRPIT  | 32000:177777  | 1   | Save of RPIT                                      |
| 17          | S3SMPIT  | 32000:177777  | 1   | Save of MPIT                                      |
| 20          | S3SDT5   | 0:175777      | 14  | ND-500/5000 standard domains seg.                 |
| 21          | S3NM5    | 0:175777      | 13  | ND-500/5000 name-tables segment                   |
| 22          | S3RFAC   | 26000:171777  | 3   | Remote file access segment                        |
| 23          | S3DPIT   | 4000:135777   | 7   | DPIT segment                                      |
| 24          | S3SGST   | 0:37777       | 1   | Save of segment table                             |
| 25          | S3IRPIT  | 32000:177777  | 1   | Image of RPIT                                     |
| 26          | S3IMPIT  | 32000:177777  | 1   | Image of MPIT                                     |
| 27          | S3ISGT   | 0:37777       | 1   | Image of segment table                            |
| 30          | S3SM5    | 40000:177777  | 5   | ND-500/5000 System Monitor seg.                   |
| 31          | S3SSPD   | 164000:165777 | 7   | Save of spooling data fields                      |
| 32          |          |               |     | Reserved, but not used                            |
| 33          |          |               |     | Reserved, but not used                            |
| 34          |          |               |     | Reserved, but not used                            |
| 35          | S3MPIT   | 32000:155777  | 12  | MPIT segment (standard system A)                  |
| 36          | S3MPIT   | 32000:161777  | 12  | MPIT segment (standard syst. B&C)                 |
| 36          | S3TAD    | 110000:133777 | 11  | TADADM segment                                    |
| 37          | S3RTD    | 0:177777      | 1   | RT-Loader data segment                            |
| 40          | S3FUDRT  | 164000:173777 | 7   | File user data segment for RT prog.               |
| 41          | S3IMED   | 26000:27777   | 1   | Image of edit routines                            |
| 42          | S3ED     | 26000:27777   | 11  | Edit routines                                     |
| 43          | S3PATCH  | 174000:177777 | 2   | Used for patching purposes                        |
| 44          | S3IDPIT  | 4000:135777   | 1   | Image of DPIT                                     |
| 45          | S3ISYS   | 144000:151777 | 1   | Image of system segment                           |
| 46          | S3S5PIT  | 26000:37777   | 1   | Save of 5PIT segment                              |
| 47          | S3RPIT   | 32000:143777  | 10  | RPIT segment                                      |
| 50          | S3IS5PIT | 26000:37777   | 1   | Image of 5PIT segment                             |
| 51          | S3S5PIT  | 26000:37777   | 5   | 5PIT segment                                      |
| 52          | S3SAVE   | 0:175777      | 1   | Save of common code & start/restart               |
| 53          | S3SDPIT  | 4000:135777   | 1   | Save of DPIT                                      |
| 54          | S3SSYS   | 144000:151777 | 1   | Save of system segment                            |
| 55          | S3SERRP  | 30000:67777   | 1   | Save of error program                             |

---

## Page 53

# Segment Information

| No. | Name    | Address Range  | PIT | Description                                     |
|-----|---------|----------------|-----|-------------------------------------------------|
| 56  | S3SRTC  | 30000: 67777  | 1   | Save of RT-Loader code segment                  |
| 57  | S3SRTD  | 0: 25777      | 1   | Save of RT-Loader data segment                  |
| 60  | S3SECOM | 26000: 31777  | 1   | Save of extended common                         |
| 60  | S3SECOM | 26000: 31777  | 1   | Save of extended common                         |
| 62  | S3SSM5  | 40000:177777  | 1   | Save of ND-500/5000 System Monitor              |
| 63  | S3MEMTF | 172000:172000 |     | MEMTOF segment                                  |
| 64  | S3ECOM  | 26000: 31777  | 10  | Extended common segment                         |
| 65  | S3SIPIT | 32000:177777  | 1   | Save of IPIT                                    |
| 66  | S3IIPIT | 32000:177777  | 1   | Image of IPIT                                   |
| 67  | S3IPIT  | 32000: 77777  | 15  | IPIT segment                                    |
| 70  | S3SSM   | 30000:137777  | 1   | Save service/mail segment                       |
| 71  | S3SM    | 30000:137777  | 11  | Service/mail segment                            |
| 72  | S3SDMWD | 2000: 11777   | 1   | Save of disk mirroring WD segment               |
| 73  | S3IDMWD | 2000: 11777   | 1   | Image of disk mirroring WD segment              |
| 74  | S3SXMK  | 102000:177777 | 1   | Save of XMSG kernel                             |
| 75  | S3SXROU | 0:101777      | 1   | Save of XMSG XROUT segment                      |
| 76  | S3XMK   | 102000:177777 | 2   | XMSG kernel                                     |
| 77  | S3XROU  | 0:101777      | 2   | XMSG XROUT segment                              |
| 100 | S3SDNAM | 164000:177777 | 1   | Save of device-name table                       |
| 101 | SDNAM   | 164000:177777 | 7   | Device-name table                               |
| 102 | S3SXMFI | 0:153777      | 1   | Save of XMSG watchdog (XMFIDO)                  |
| 103 | S3XMFI  | 0:153777      | 1   | XMSG watchdog (XMFIDO)                          |
| 104 | S3SNKSE | 30000:177777  | 11  | Save of NUCLEUS server                          |
| 105 | S3INKSE | 30000:177777  | 11  | Image of NUCLEUS server                         |
| 106 | S3SNKNA | 0:177777      | 1   | Save of NUCLEUS name server                     |
| 107 | S3INKNA | 0:177777      | 1   | Image of NUCLEUS name server                    |
| 110 | S3SU110 | 0: 77777      | 1   | Save of ND-110 Microprogram                     |
| 111 | S3IU110 | 0: 77777      | 1   | Image of ND-110 Microprogram                    |
| 112 | S3SU120 | 0: 77777      | 1   | Save of ND-120 Microprogram                     |
| 113 | S3IU120 | 0: 77777      | 1   | Image of ND-120 Microprogram                    |
| 114 | S3SERWC | 0:147777      | 1   | Save of ERS Watchdog program                    |
| 115 | S3IERWC | 0:147777      | 1   | Image of ERS Watchdog program                   |
| 116 | S3SERWD | 0:157777      | 1   | Save of ERS Watchdog data                       |
| 117 | S3IERWD | 0:157777      | 2   | Image of ERS Watchdog data                      |
| 120 | S3SPPRMA| 30000:177777  | 11  | Save of Processor Manager server                |
| 121 | S3IPRMA | 30000:177777  | 11  | Image of Processor Manager server               |
| 122 | S3SPWRS | 30000:177777  | 11  | Save of PFTCON server                           |
| 123 | S3IPWRS | 30000:177777  | 11  | Image of PFTCON server                          |
| 124 | S3SBOPC | 30000:177777  | 11  | Save of BOPCOM Server                           |
| 125 | S3IBOPC | 30000:177777  | 11  | Image of BOPCOM Server                          |
| 126 | S3SMTSE | 30000: 37777  | 11  | Save of MT server                               |
| 127 | S3IMTSE | 30000: 37777  | 11  | Image of MT server                              |

---

## Page 54

# 18.4 System included RT-programs

| Program | Purpose |
|---------|---------|
| 1SWAP | Queueing program requests for swapping |
| 5SWAP | Performs ABSTR in ND-100 for the ND-500/5000 Swapper |
| ACCRT | RT accounting |
| BAKnn | Background process for terminal (BAK01-BAK99) |
| BKnnn | Background process for terminal (BK100-BK128) |
| BCHnn | Batch process |
| BOPCOM | BOPCOM Server |
| BPTMP | Timeout program for background allocation system |
| COSPO | COSMOS-spooling server |
| DIMWD | Used by the disk mirroring facility which is part of the Fault Tolerant eXtension (FTX). |
| DUMM2 | Dummy program used by the spooling system |
| DUMMY | Dummy program to prevent empty execution queue |
| ERS3WD | ERS/SINTRAN III Watchdog |
| FDRT1 | Transfer data between interface buffer and memory. |
| FDRT2 | Floppy formatting. (FLOPPY-2) |
| FIXRT | Monitor call/command FIXC execution |
| FLUSH | Write (flush) file-system cache to disk |
| MTSERV | NUCLEUS MTAD-server |
| NKNAME | NUCLEUS name server |
| NKSERV | NUCLEUS server |
| PROMAN | Process Manager Server |
| PFTCON | Power Supply Controller server |
| REVIVE | Used by the disk mirroring facility which is part of the Fault Tolerant eXtension (FTX). |
| RTDIL | Buffer transfer program for DISC-ACCESS-LOG |
| RTERR | Output error messages |
| RTRFA | Does remote file access for RT-programs (COSMOS - remote file access) |
| RTSLI | Time slicer. Changes priority on all time sliced processes. |
| RTREC | Process to reconnect SINTRAN file system directory to DOMINO controller (after re-boot of DOMINO or when BDIO switch to mirror pool). |
| RWRT1 | Block data transfer. Activated from RFILE/WFILE/RPAGE/WPAGE for RT-programs |
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

---

## Page 55

# SINTRAN III Release Information, N-version

| Program | Purpose |
|---------|---------|
| RWRT13  | Block-oriented internal device 1 Input |
| RWRT14  | Block-oriented internal device 2 Input |
| RWRT15  | Block-oriented internal device 3 Input |
| RWRT16  | Block-oriented internal device 4 Input |
| RWRT17  | Block-oriented internal device 5 Input |
| RWRT20  | Block-oriented internal device 1 Output |
| RWRT21  | Block-oriented internal device 2 Output |
| RWRT22  | Block-oriented internal device 3 Output |
| RWRT23  | Block-oriented internal device 4 Output |
| RWRT24  | Block-oriented internal device 5 Output |
| RWRT25  | HASP DMA 1 Input |
| RWRT26  | HASP DMA 1 Output |
| RWRT27  | HASP DMA 2 Input |
| RWRT28  | HASP DMA 2 Output |
| RWRT29  | HASP DMA 3 Input |
| RWRT30  | HASP DMA 3 Output |
| RWRT31  | HASP DMA 4 Input |
| RWRT32  | HASP DMA 4 Output |
| RWRT33  | HASP DMA 5 Input |
| RWRT34  | HASP DMA 5 Output |
| RWRT35  | HASP DMA 6 Input |
| RWRT36  | HASP DMA 6 Output |
| RWRT41  | Transfer on SCSI Streamer |
| RWRT42  | Open/close file on SCSI Streamer |
| SPRTn   | Spooling programs (1-9) |
| SPRnn   | Spooling programs (10-30) |
| STSIN   | Initialise SINTRAN III and start systems RT-programs |
| TADnn   | Background process for Terminal Access Device |
| TADAD   | Administers connections to TADs from requesting users. |
| TERMP   | Starts the user defined "clean-up" RT-program when RT-programs are aborted (if enabled) |
| TIMRT   | Timer RT-program. Start timeout-routine for all devices in timer-table. |
| UDRnn   | Performs Fast Universal DMA for user processes. |
| XROUT   | XMSG server |
| XTRACE  | XMSG server |
| XMFIDO  | XMSG Watchdog |

---

## Page 56

# 19. Affected Subsystems

## 19.1 Subsystems delivered together with SINTRAN

| Subsystem                     | Description                                                                                                                                                                                               |
|-------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ND-500/5000 Monitor           | only version K or later of ND-500/5000 Background Monitor may be used when running SINTRAN III version N. Version L of the ND-500/5000 Monitor is required when running ND-5850 or ND-5830 systems.        |
| ND-5000 Swapper               | only version L or later of the ND-500/5000 Swapper may be used when running SINTRAN III version N. On ND-5000 systems, the M-version of the ND-5000 Swapper offers significantly improved performance on most systems. |
| XMSG                          | XMSG is part of SINTRAN III in the N-version and must not be installed as a separate product.                                                                                                               |
| SINTRAN III Configuration     | Version E or later of the SINTRAN III Configuration program is required when configuring the N-version of SINTRAN III. The F-version of the Configuration program is recommended as it contains some corrections compared to the E-version. |
| NOTS Service                  | version C or later of the NOTS Service program is used to set or change the configuration of Net/One terminals. The D-version of NOTS Service is recommended as it contains some corrections compared to the C-version. |
| ERS/SINTRAN III Watchdog      | the SINTRAN III Watchdog of the Event Report System is part of SINTRAN III in the N-version and must not be installed as a separate product. The D-version of the error message the log-list program and revision D23 or later of the descriptor file are required to run under the N-version of SINTRAN. |

## 19.2 Other subsystems

| Subsystem                     | Description                                                                                                                                                                                               |
|-------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| User Environment              | version C or later of User Environment (210518) is required to run under SINTRAN III version N. Version E of User Environment offers enhanced security as well as improved performance by moving parts of User Environment to ND-5000. |
| COSMOS Basic Module           | version F of COSMOS Basic Module (210374) is required when running XMSG which is now part of SINTRAN III version N. Version G of COSMOS Basic Module offers better performance when copying files with "holes". |

---

## Page 57

# SINTRAN III Release Information, N-version

## Backup Manager

Version A of the Backup Manager (211226) is designed to ease the task of taking backup. Version B offers better performance.

## Backup System

Version H of the Backup System (210337) is required to handle files with file index > 255 (more than 256 files per user).  
Version I is required to handle SCSI streamer tape drives. Revision I05 of the Backup System and revision I05 of the DMA server are required to handle SCSI optical disks and magnetic tape drives.

## Disk Mirroring

Only version E of Disk Mirroring (210855) may be used when running SINTRAN III version N. Disk mirroring applies to ND-100 connected disks.

## File Manager

Versions A or B of the File Manager (part of 210518) will not handle files with file index > 255 (more than 256 files per user). Version C of the File Manager (211075) will handle this problem.

## File System Investigator

Version O of the File System Investigator (part of 210628) is required to handle files with file index > 255 (more than 256 files per user).

## File System Verification

Version B of the File System Verification (211073) is recommended as it includes a significantly improved user interface and enhanced functions.

## Linkage Loader

Version H of the Linkage Loader (210319) is required to handle communication with RT-programs.

## ND Linker

The ND-LINKER 211224, version A or later, is required to handle the new domain files (as opposed to the old ND-500/5000 domains built by the Linkage-Loader).

## Convert Domain

The conversion program, Convert Domain (211229), is used to convert an "old" domain (built by the Linkage-Loader and stored on a triple of files) to a domain file without having to reload the domain.

## Performance Monitor

Only version B of the Performance Monitor (211074) may be used when running SINTRAN III version N.

## Symbolic Debugger

Version F of the Symbolic Debugger (210336) can be used when running SINTRAN III version N to debug RT-programs.  
Version H is required to handle ND-500/5000 domains stored on domain files (by the ND-Linker).

## LED Debugger

Revision B02 of the LED Debugger (211157) is required to handle ND-500/5000 domains stored on domain files (by the ND-Linker).

## Telefix Local

Version C01 of Telefix Local (210775) is required.

---

## Page 58


---

## Page 59



---

## Page 60



---

