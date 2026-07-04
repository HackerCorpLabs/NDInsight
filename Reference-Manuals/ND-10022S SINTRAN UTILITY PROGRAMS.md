## Page 1

# NORD SOFTWARE LIBRARY DISKETTE

## CONTAINING : SINTRAN UTILITY PROGRAMS

### DIRECTORY NAME : ND-10022S
### USER NAME : FLOPPY-USER

| File | Description |
|------|-------------|
| FILE 1 | (ND-10022S:FLOPPY-USER)FLOPPY-MON-2010F:BFUN;1 |
| FILE 2 | (ND-10022S:FLOPPY-USER)FLSYS-INV-2135H:BFUN;1 |
| FILE 3 | (ND-10022S:FLOPPY-USER)MEMTOF-2326A:BFUN;1 |
| FILE 4 | (ND-10022S:FLOPPY-USER)DUMPFL-2327A:BFUN;1 |
| FILE 5 | (ND-10022S:FLOPPY-USER)DMAC-1915J:BFUN;1 |
| FILE 6 | (ND-10022S:FLOPPY-USER)MCOPY-TAN8-1649J:BFUN;1 |
| FILE 7 | (ND-10022S:FLOPPY-USER)MTCOPY-TANE-1649J:BFUN;1 |
| FILE 8 | (ND-10022S:FLOPPY-USER)DITAP-1380D:BFUN;1 |
| FILE 9 | (ND-10022S:FLOPPY-USER)COP-VERIFY-2035B:BFUN;1 |
| FILE 10 | (ND-10022S:FLOPPY-USER)MCOPY-MP-1650G:BFUN;1 |

16 MARCH 1982

---

## Page 2

# NORSK DATA A/S NORD SOFTWARE LIBRARY 
## REVISION LOG

| PRODUCT   | NAME                                 | ND-NUMBER    |
|-----------|--------------------------------------|--------------|
|           | Subsystem Package                    | 10005/10044  |
|           | SINTRAN III/VS Utility Programs      | 10022        |

| ISSUED    | DATE 82.04.06                        | BY (INITIALS) KN |
|-----------|--------------------------------------|------------------|

| REASON    | ERROR CORRECTION                     | DIFFERENT ENVIRONMENT |
|           | X CHANGE/ADDITION                    |                      |

| FILES     | PROG.NUM. NAME                       |
|-----------|--------------------------------------|
| CHANGED   | ... ...                              |
| OR NEW    | ... ...                              |
| FILES     | ... ...                              |

## CHANGES

The subsystems on the diskettes 10005, 10044 and 10022 are organized slightly different, and one new diskette - 10400 Subsystem Package II - is introduced, to have room for future extensions of the programs on the diskettes. The new diskettes will contain:

10005/10044  
Subsystem Package  
FILE-EXTRACT  
BRF-EDITOR  
LOOK-FILE  
NRL  
PERFORM  
GPM  

10022  
SINTRAN III/VS  
Utility Programs  
FLOPPY-MONITOR  
FILE-SYSTEM-INVESTIGATOR  
MEMTOF  
DUMPFL  
DMAC  
MCOPY (HP and TANDBERG)  
DITAP  
COPY-VERIFY  

10400  
Subsystem Package II  
FMAC (32 and 48 bit)  
MAC (32 and 48 bit)  
QED  
NPL  

The diskette 10124 GPM will be phased out.

The diskette 10400 will not be issued to customers that already have the products on that diskette until a change occur in one of those products.

---

## Page 3

# NORSK DATA A/S  
NORD SOFTWARE LIBRARY  
PROGRAM DESCRIPTION  

## PRODUCT NAME
SINTRAN UTILITY PROGRAMS

| ND-NUMBER/CATEGORY | ND-NUMBER FOR SOURCE |
|--------------------|----------------------|
| 10022Q,R, P        | (LDR-2036F)          |

## ISSUED
| DATE: 25 AUG 1981 | BY (INITIALS) PVDV |

## COMPUTERS 
| X 10 | X 12 | 50 | X 100 | 500 |
|------|------|----|-------|-----|

## INSTR.SET
| 48 BIT FL. | 32 BIT FL. | COMMERCIAL |

## OP.SYSTEM
| SIN III VS | SIN III RT | X ALONE |

## DOCUMENTATION
| NUMBER:   |              |
| TITLE:    |              |

## PURPOSE
Floppy disc bootstrap and monitor, used to load programs from diskettes.

## PROGRAMS
| PROG.NUMB.  | NAME       | TYPE  | CONTAINING         |
|-------------|------------|-------|--------------------|
| LDR-2010F   | FLOPPY-MON | BPUN  | FLOPPY-MON-2010F   |

## LOADING PROCEDURE:
Put the monitor on the discette by doing the SINTRAN command:

$DEV-FUNCTION,<FLOPPY PERIPHERAL NAME>,DUMP-BOOTSTRAP,FLOPPY-MON

## USER INSTRUCTIONS:
Place floppy in Floppy Disc Controller 1, Unit 0.  
Press Master Clear and type 1560&. The floppy monitor should now start up by printing an asterisk and is ready to accept one of the following commands:

- **LIST-FILE <dev>**: Makes a list of the :BPUN files contained on the discette. If <dev>=1 (or CR) output will go to the terminal; if <dev>=5 it will go to the line printer.

- **LOAD-FILE <file>**: Load the file as specified by <file> into memory and start execution of this program.

- **PLACE-FILE <file>**: As LOAD-FILE but does not start execution.

- **HELP**: Lists the monitor-version and the possible commands.

---

## Page 4

# Norsk Data A/S - Nord Software Library

## Product Information

| PRODUCT | NAME                     | ND-NUMBER   |
|---------|--------------------------|-------------|
|         | SINTRAN UTILITY PROGRAMS | 10022Q,R,S  |

If a terminal different from device 300 is to be used as console device, one may set the T-register on level 15 (dec.) to any legal terminal device number (310, 320 etc.) prior to typing 156Q&. The text `IF HERE TYPE ANY CHARACTER` will then be printed on both device 300 and on the 'alternative' console device. The first device that sends a character to the computer will be taken as console device.

If the discette contains only one :BPUN file and the X-register on level 15 (dec.) is different from 0, this file will be automatically loaded without entering the floppy monitor.

The following program stops are defined (last instruction was WAIT)

- WAIT 77 : not possible to read bootstrap from floppy (hardware err)

- WAIT 0 : always when PLACE-FILE command is used (legal wait)

Any other error situation will give an error message. If the error occurs after the loading of a program has started, an error message will be given and the floppy monitor will be reloaded.

---

## Page 5

# NORSK DATA A/S  
NORD SOFTWARE LIBRARY  

**REVISION LOG**  

| PRODUCT   | NAME                     | ND-NUMBER  |
|-----------|--------------------------|------------|
|           | SINTRAN UTILITY PROGRAMS | 10022Q,R,S |

| ISSUED       | DATE 25 AUG 1981   | BY (INITIALS) PVDV   |
|--------------|--------------------|----------------------|

| REASON                 | DIFFERENT ENVIRONMENT         |
|------------------------|-------------------------------|
| X ERROR CORRECTION     |                               |
| X CHANGE/ADDITION      |                               |

| FILES CHANGED OR NEW   | PROG.NUM. NAME                |
|------------------------|-------------------------------|
|                        | LDR-2010F FLOPPY-MON          |

## CHANGES

- If the discette contains only one :BPUN file and the X-register on level 15 (dec.) is different from 0, this file will be automatically loaded without entering the floppy monitor.

- Carriage Return may now be entered as default for console device in the LIST-FILE command.

- The monitor now accepts lower case characters on input.

- Certain error conditions will now give error messages in stead of going into WAIT.

- The PLACE-FILE command will type the message 'PLACED' when the place is finished.

- The floppy monitor will automatically be reloaded if an error occurred during loading of a program.

- The necessary code has been added to be able to support the floppy-disc DMA controller.

## ERRORS CORRECTED

- Corrected abbreviation uplook routine.

- Sometimes if the file name specified would be ambiguous, one of the programs would still be loaded. This is corrected.

---

## Page 6

# NORSK DATA A/S  
## NORD SOFTWARE LIBRARY  
### PROGRAM DESCRIPTION

| PRODUCT   | NAME                     | ND-NUMBER | CATEGORY    |
|-----------|--------------------------|-----------|-------------|
|           | SINTRAN UTILITY PROGRAMS | 100229    | R, P        |
|           |                          | ND-NUMBER FOR SOURCE | SUT-2369D |

| ISSUED                        | DATE AUG 20, 1981 | BY (INITIALS) TP |
|-------------------------------|-------------------|------------------|
| COMPUTERS                     | X 10 | 12 | 50 | X 100 | 500 | ... |
| INSTR.SET                     | 48 BIT FL. | 32 BIT FL. | COMMERCIAL |
| OP.SYSTEM                     | SIN III VS | SIN III RT | X ALONE | ... |
| DOCUMENTATION                 | NUMBER: ......     |                  |
|                               | TITLE: ................................ |
| PURPOSE                       | To check directories |

| PROGRAMS (FILES)              | PROG.NUM3. NAME | TYPE CONTAINING         |
|-------------------------------|-----------------|-------------------------|
|                               | SUT-2135H FILSYS-INV | BPUN File system investigator |

## LOADING/OPERATING PROCEDURE, USE

Place the diskette in the floppy disk drive, unit 0.  
Press MASTER CLEAR. Type 1560&.  
When the terminal responds with an asterisk, type LOAD FIL.

After power fail, the program should be restarted.

SET-PRINTER-DEVICE should not be used with device number 0.

Due to an error in the test program monitor, it is absolutely necessary  
to press MASTER CLEAR before the diskette is loaded. The error will be corrected  
in the next version.

---

## Page 7

# NORSK DATA A/S

## NORD SOFTWARE LIBRARY  
### REVISION LOG

| PRODUCT NAME              | ND-NUMBER     |
|---------------------------|---------------|
| SINTRAN UTILITY PROGRAMS  | 10022Q,R,S    |

| ISSUED         | DATE AUG 20, 1981 | BY (INITIALS) TP |
|----------------|--------------------|-----------------|
|                |                    |                 |

| REASON              | X ERROR CORRECTION       | X DIFFERENT ENVIRONMENT    |
|---------------------|--------------------------|----------------------------|
|                     | X CHANGE/ADDITION        |                            |

| FILES CHANGED OR NEW | PROG.NUMBER NAME             |
|----------------------|------------------------------|
|                      | SUT-2135H FILSYS-INV:BPUN    |

## CHANGES

This version can operate on the 150 Kbyte (DISC-2-75) disc.

Some of the directories have changed size. Therefore, a new disk driver has been included.

Some minor errors are corrected.

---

## Page 8

# NORD SOFTWARE LIBRARY  
## OBJECT DESCRIPTION

| PROGRAM                     | PROGRAM NUMBER              |
|-----------------------------|-----------------------------|
| NAME      | MEMTOF          | SUT-2326 A                  |

| REFERENCE | DATE 09.04.79   | NAME TNY                    | PROGRAM CATEGORY  |
|-----------|-----------------|-----------------------------|-------------------|
| REVISED   | DATE            | NAME                        | REVISION LOG FOR PROGRAM  |
| EXPIRED   | DATE            | NAME                        | NEW PROGRAM NUMBER    |

| COMPUTERS |                 |                             |                     |
|-----------|-----------------|-----------------------------|---------------------|
|           | [ ] 28          | [ ] 5                       | [X] 10              |
|           | [ ] 12          | [ ] 20                      | [ ] 50              |

| OPERATING SYSTEMS |         |                             |                     |
|-------------------|---------|-----------------------------|---------------------|
| [ ] TSS           | [X] SIN III M  | [ ] SIN III C   | [ ] ALONE            |           

| TYPE              | [X] BINARY | [ ] OCTAL | [ ] BRF | [ ] RB8  |           

| DOCUMENTATION   |              |            |                     |    |
|-----------------|--------------|------------|---------------------|----|
| SYSTEM DESCRIPTION | [X] USER DESCRIPTION | [ ] MANUAL | TITLE 

| ADDRESS SPACE | SIZE 1K  | BOUNDARIES | 66000<70000  |
|---------------|----------|------------|--------------|
| START ADDRESS | RESTART ADDRESS |    |

| PURPOSE             |
|---------------------|
| Dump content memory bank 0 plus registerblocks to floppy. |

## LOADING/OPERATING PROCEDURE, USE

1. Copy the file MEMTOF:8PUN to main directory, user system

   ```
   @COPY-F "MEMTOF-2326:8PUN" ( P-10022F:F-U ) MEMTOF:8PUN
   ```

2. Dump MEMTOF as a stand-alone program on diskette by using

   ```
   @DUMPFL ( SUT-2327 )
   ```

   (NB! No directory on the diskette).

3. When you want to do a memory dump:

   Insert diskette with MEMTOF - stand-alone in floppydevice-one, unit 0 and type 1560 & on the consol.

---

## Page 9

# NORD SOFTWARE LIBRARY
## OBJECT DESCRIPTION

| PROGRAM | PROGRAM NUMBER |
|---------|----------------|
| NAME: DUMPFL | SUT-2327 A |

| REFERENCE | NAME | PROGRAM CATEGORY |
|-----------|------|-------------------|
| DATE: 09.04.79 | TNY | |

| REVISED | NAME | REVISION LOG FOR PROGRAM |
|---------|------|--------------------------|
| DATE | | |

| EXPIRED | NAME | NEW PROGRAM NUMBER |
|---------|------|--------------------|
| DATE | | |

| COMPUTERS | | | | | | |
|------------|----|----|----|----|----|----|----|
|            |    | 28 | 5  | 10 | 12 | 20 | 50 |

| OPERATING SYSTEMS | | | | |
|-------------------|----|----|----|----|----|
|                   | TSS | X SIN III M | SIN III C | ALONE |

| TYPE | | | | | |
|------|----|----|----|----|----|----|
|      | X BINARY | OCTAL | BRF | AB |

| DOCUMENTATION |                   |
|---------------|-------------------|
|               | SYSTEM DESCRIPTION| X USER DESCRIPTION| MANUAL |
|               | TITLE:            |                   |        |

| ADDRESS SPACE | SIZE | BOUNDARIES |
|---------------|------|------------|
|               | START ADDRESS | 13665 |
|               | RESTART ADDRESS | 13665 |

## PURPOSE

Dump MEMTOF on floppy as a stand-alone program to be loaded by the hardware loader.

## LOADING/OPERATING PROCEDURE, USE

1. Copy the file DUMPFL:PROG to main directory, user system.

   ```
   @COPY-F "DUMPFL-2327:SPUN" (P-10022F:F-u) DUMPFL:SPUN
   ```

   Dump as "PROC" version:

   ```
   @PLACE-BINARY DUMPFL
   @DUMP "DUMPFL-2327A" 13665 13665
   ```

   or as reentrant subsystem:

   ```
   @DUMP-REENTRANT DUMPFL, 13665, 13665, DUMPFL
   ```

2. To produce a diskette with the MEMTOF stand-alone:

   Insert a formatted diskette in floppy-disk-one unit 0, and start the program DUMPFL.

   ```
   @DUMPFL
   ```

   There will be some output, but the program requires no input.

   **NB! There is no directory on the diskette containing MEMTOF stand-alone.**

---

## Page 10

# NORSK DATA A/S NORD SOFTWARE LIBRARY  
## PROGRAM DESCRIPTION  

| PROGRAM | NAME          | PROGRAM NUMBER |
|---------|---------------|----------------|
|         | DMAC          | MAC-1915 D     |
|         |               | PART OF ND-NUMBER 10022: N,P,Q,R,S |

| COMPUTERS      | X 10  | 12  | 50  | X 100 | 500 |       |
|----------------|-------|-----|-----|-------|-----|-------|
| INSTR.SET      |       | 48 BIT FL. | 32 BIT FL. | COMMERCIAL |
| OP.SYSTEM      |       | X SIN III VS | SIN III RT | ALONE |
| PROG.TYPE      |       | X BINARY | BRF | SYMBOLIC |
| DOCUMENTATION  |       | NUMBER: ND-60.096.01 ND-60.050.09 |
|                |       | TITLE: MAC USER'S GUIDE AND SINTRAN III USER'S GUIDE |

| ADDRESS SPACE (SIN.PROG) | BOUNDRIES: 104700<123777 |
|--------------------------|--------------------------|
|                          | START ADDRESS: 123777    |
|                          | RESTART ADDRESS: 123775  |

| PURPOSE                      |
|------------------------------|
| DEBUGGING RT-PROGRAMS (OPERATE ON SEGMENTS) |

LOADING/OPERATING PROCEDURE, USE  

SEE SINTRAN III SYSTEM SUPERVISOR (ND-60.103.04)  

DOES NOT INCLUDE THE TWO PASS ASSEMBLY OPTION!

---

## Page 11

# NORD SOFTWARE LIBRARY
## REVISION LOG

| PRODUCT | NAME | ND-NUMBER |
|---------|------|-----------|
| SINTRAN UTILITY PROGRAMS | 10022N,P,Q,R,S |

| ISSUED | DATE 81.02.10 | BY (INITIALS) TNY |

| REASON | * ERROR CORRECTION | * DIFFERENT ENVIRONMENT |
|        | X CHANGE/ADDITION |                         |

| FILES CHANGED OR NEW FILES | PROG.NUM. NAME |
|----------------------------|----------------|
|                            | MAC-1915D DMAC |

### CHANGES

Included option for single step breakpoint (partly missing in the C-version) and floating point numbers.

---

## Page 12

# NORSK DATA A/S

## NORD SOFTWARE LIBRARY  
### PROGRAM DESCRIPTION  

| PRODUCT         | NAME                       | ND-NUMBER!CATEGORY   |
|-----------------|----------------------------|----------------------|
| SINTRAN UTILITY PROGRAMS |                        | 10022R,S!       |
| (MCOPY-TANB)   |                            | P                    |
|                 | ND-NUMBER FOR SOURCE      |                       |
|                 | HUT-1648J                 |                       |

| ISSUED          | DATE 81.11.13              | BY (INITIALS) TNY    |
|-----------------|----------------------------|----------------------|
| COMPUTERS       | X 10 ! . 12 ! . 50 ! X 100! | 50O! ...            |
| INSTR.SET       | . 48 BIT FL ! . 32 BIT FL! | COMMERCIAL           |
| OP.SYSTEM       | . SIN III VS!              | . SIN III RT! X ALONE| 

| DOCUMENTATION   |                            |                      |
|-----------------|----------------------------|----------------------|
| NO MANUAL EXISTS AT THE PRESENT TIME         |                      |

| PURPOSE         |                            |                      |
|-----------------|----------------------------|----------------------|
| TO COPY DIRECTORIES ON DISK TO AND FROM TANDBERG, PERTEC AND STC MAG-TAPE DRIVES |                      |

| PROGRAMS (FILES) | PROG.NUMBA NAME           | TYPE CONTAINING      |
|------------------|---------------------------|----------------------|
| HUT-1649J MCOPY-TANB | BPUN                  |                      |

Boundaries: 0 < 30550  
Start address: 0  
Restart address: 20  

### LOADING/OPERATING PROCEDURE, USE

MASTER CLEAR  
1560&  
*LOAD MCOPY-TANB  

When starting the program, users of Nord-10 and 10/S computers may get an error message:

    INTERNAL INTERRUPT. IIC: 0

This message may be disregarded (it is due to an error in the Test-program monitor).

A more complete description of MCOPY and how to use it is given on the next pages. In the examples, user input is *underlined*.

## 1. DESCRIPTION OF MCOPY

MCOPY means Mag-tape - Disk Copy.

---

## Page 13

# NORSK DATA A/S NORD SOFTWARE LIBRARY

| PRODUCT | NAME                          | ND-NUMBER |
|---------|-------------------------------|-----------|
|         | SINTRAN UTILITY PROGRAMS      | 10022R,S  |
|         | (MCOPY-TANB)                  |           |

## 1.1 Purpose of the program

MCOPY is a program for backup of disks to tape. It can be used to copy one or more directories from disk to tape, or from tape to disk. It can also compare one or more directories on disk and tape. MCOPY runs under the Test-program Monitor. (But it is not a test program!)

## 1.2 Necessary equipment

The current version of MCOPY (MCOPY-TANB-1649J) may be used with the following equipment:

- Computers: ND-10, ND-10/S, ND-100
- Disk drives: 10 MB (Hawk), 30/60/90 MB (Phoenix), 33/66/38/75/160/288 MB
- Tape drives: Tandberg, Pertec, STC

## 1.3 Loading the program

*LOAD MCOPY*

MAG-TAPE - DISK COPY, HUT-1649J. ISSUED: 2. SEPTEMBER 1981  
THE COMMAND HELP GIVES YOU A LIST OF THE COMMANDS

## 1.4 Listing of all the commands in the program

This list is also printed on your terminal when you give the command

> LIST-SPECIAL-COMMANDS  
> COPY-TO-MAG-TAPE  
> COPY-FROM-MAG-TAPE  
> COMPARE-DISK-TAPE  
> SINTRAN-BLOCK-SIZE  
> SET-MAG-TAPE-DEVICE-NUMBER  
> 1600-BPI  
> SYSTEM-COPY  
> AUTOMATIC-COMPARE  
> SET-DISK-TYPE

---

## Page 14

# NORSK DATA A/S NORD SOFTWARE LIBRARY PAGE 3 OF 7

| PRODUCT | NAME                       | ND-NUMBER |
|---------|----------------------------|-----------|
|         | SINTRAN UTILITY PROGRAMS   | 10022R,S  |
|         | (MCOPY-TANB)               |           |

## 1.5. General Information

The main commands of the program are:

- **COPY-TO-MAG-TAPE**  
  used to copy from disk to tape  
- **COPY-FROM-MAG-TAPE**  
  used to copy from tape to disk  
- **COMPARE-DISK-TAPE**  
  used to compare disk and tape  

The commands:

- **SINTRAN-BLOCK-SIZE**
- **1600-BPI**
- **SYSTEM-COPY**
- **AUTOMATIC-COMPARE**

are used to turn on/off (set/reset) special features (modes) of the program, i.e. each time one of these commands is given, the relevant feature is either turned on or off, depending on what state it was in previously.

The commands:

- **SET-MAG-TAPE-DEVICE-NUMBER**
- **SET-DISK-TYPE**

are used to change the values of mag-tape device number or disk type.

Several of the MCOPY commands expect numbers as parameters. When the numbers are asked for, the program also indicates the range of legal values, and the radix (decimal or octal), e.g.:

DISK UNIT (DECIMAL, 0-3) : 1

MAG-TAPE DEVICE NUMBER (OCTAL, 520-530) : **530**

## 1.6. How to copy/compare one or more directories

One or more directories may be copied from disk to tape by giving the command:

> **COPY-TO-MAG-TAPE**

One or more directories may be copied from tape to disk by giving the command:

> **COPY-FROM-MAG-TAPE**

---

## Page 15

# NORSK DATA A/S

## NORD SOFTWARE LIBRARY

### PRODUCT

| NAME                          | ND-NUMBER  |
|-------------------------------|------------|
| SINTRAN UTILITY PROGRAMS      | 10022R,S   |
| (MCOPY-TANB)                  |            |

One or more directories may be compared by giving the command:

`>COMPARE-DISK-TAPE`

After each of these commands the user is asked a number of questions, some of them dependent on disk type, whether system copy is turned on etc.

1. If disk type has not been given previously, the user is asked about disk type. The disk type is given by standard Sintran disk names, e.g.

   **DISK NAME**: `DISC-75MB-1`

2. For some of the disk types (38/75/288/30/60/90 MB) the user is then asked about old or new directory size:

   > DIRECTORIES CREATED BY SINTRAN VERSION E OR LATER ARE A FEW PAGES SMALLER THAN THOSE CREATED BY VERSIONS A, B, C, D. DO YOU WANT THE NEW SIZE (VERSION E OR LATER) OR THE OLD SIZE (VERSION D OR EARLIER)? PLEASE ANSWER OLD OR NEW: `NEW`

3. The user is then asked about disk unit number:

   > **DISK UNIT** (DECIMAL, 0-3): `0`

4. If the disk type is one with multiple directories on the same unit (i.e. 10 MB, 30/60/90 MB, 2-75 MB, 3-75 MB) the user is then asked for:

   a) If system copy is turned on: Number of directories to copy, e.g.:

   > **NUMBER OF DIRECTORIES** (DECIMAL, 1-2): `2`

   b) If system copy is not turned on: Which directory to copy, i.e. removable or fixed for 10 MB and 30/60/90 MB, which subunit for disks with more than one subunit (60/90 MB, 2-75 MB, 3-75 MB), e.g.:

   > REMOVABLE OR FIXED: `FIXED`
   >
   > **SUBUNIT** (DECIMAL, 0-2): `1`

---

## Page 16

# NORSK DATA A/S NORD SOFTWARE LIBRARY

| PRODUCT | NAME | ND-NUMBER |
|---------|------|-----------|
| . | SINTRAN UTILITY PROGRAMS | 10022R,S |
| | (MCOPY-TANB) | |

5) The user is then asked about mag-tape unit, e.g.:

MAG-TAPE UNIT (DECIMAL, 0-3): 0

If the disk type is one with multiple directories on the same unit, and system copy is turned off, the program then asks for file number on tape, e.g.:

MAG-TAPE FILE NUMBER (DECIMAL, 0-99): 2

The intention of this is that the user should be able to copy multiple directories to tape, and then be able to select one specific directory to be copied back to disk.

6) Finally, before starting any copy/compare operation, the user is asked:

OK? YES

and if the answer is YES, the copy/compare operation is started.

## 1.7 How to change disk type and mag-tape device number

When one of the copy/compare commands is given, the program checks if disk type has already been given (i.e. in connection with an earlier copy/compare operation). If the disk type has been given, the program will continue to use this disk type until it is explicitly changed by using the command `SET-DISK-TYPE`, e.g.:

>SET-DISK-TYPE  
DISK NAME: DISC-2-75MB-1

When the program is started, it assumes that the mag-tape drive is connected to mag-tape controller 1, device number 520 octal. This may be changed by giving the command `SET-MAG-TAPE-DEVICE-NUMBER`, e.g.:

>SET-MAG-TAPE-DEVICE-NUMBER  
MAG-TAPE DEVICE NUMBER (OCTAL, 520-530): 530

Mag-tape drives connected to mag-tape controller 2 have device number 530 octal.

---

## Page 17

# NORSK DATA A/S NORD SOFTWARE LIBRARY 

| PRODUCT          | NAME                     | ND-NUMBER |
|------------------|--------------------------|-----------|
|                  | SINTRAN UTILITY PROGRAMS | 10022R,S  |
|                  | (MCOPY-TANB)             |           |

## 1.8 How to change various modes of the program

The program has some commands which act like an on/off switch, i.e. each time such a command is given, the relevant feature (mode) of the program is either turned off or on, depending on what state it was in previously. When such a command is given, the command will answer whether the relevant feature is turned off or on, e.g.:

>SYSTEM-COPY  
SYSTEM COPY TURNED ON

>SYSTEM-COPY  
SYSTEM COPY TURNED OFF

### 1.8.1 Automatic compare

When the program is started, automatic compare is turned on, meaning that after each copy operation the program will automatically do a compare. When copying directories occupying multiple reels of tape (e.g. a 75 MB directory on 1600 BPI), the program will compare each reel of tape after it has been copied, thus avoiding the trouble of having to mount each reel of tape twice. The automatic compare may be turned off by giving the command AUTOMATIC-COMPARE, e.g.:

>AUTOMATIC-COMPARE  
AUTOMATIC COMPARE TURNED OFF

### 1.8.2 System copy

With disk types having multiple directories on the same unit, it is possible to copy/compare multiple directories. This feature is turned on by giving the command SYSTEM-COPY, e.g.:

>SYSTEM-COPY  
SYSTEM COPY TURNED ON

System copy is initially turned off.

### 1.8.3 1600 BPI

The STC mag-tape drive may record data on tape in either 1600 BPI or 6250 BPI mode. It is initially set in 6250 BPI mode, but this may be changed by giving the command 1600-BPI, e.g.:

---

## Page 18

# NORSK DATA A/S NORD SOFTWARE LIBRARY

| PRODUCT                     | NAME                         | ND-NUMBER |
|-----------------------------|------------------------------|-----------|
|                            | SINTRAN UTILITY PROGRAMS     | 10022R,S  |
|                            | (MCOPY-TANB)                 |           |

> 1600-BPI  
  1600 BPI TURNED ON

Recording the data in 1600 BPI mode makes it possible to read the tape on the Pertec mag-tape drive.

## 1.8.4 Sintran block size

The record size on tape is usually greater than 1K (1024) 16-bit words (i.e., greater than one page), in order to utilize the tape efficiently. Using the command SINTRAN-BLOCK-SIZE makes it possible to produce tape records which are exactly one page each. The intention of this command is that the user should be able to copy from tape to disk while Sintran is running, using the Sintran command @COPY-DEVICE. Sintran block size is initially turned off.

Note that it is not possible to copy back directories occupying more than one reel of tape with the @COPY-DEVICE command. Note also that using a record size of one page leads to inefficient utilization of the tape, and makes the copy/compare operation slower. Therefore, Sintran block size should only be used if absolutely necessary. Example of use:

>SINTRAN-BLOCK-SIZE  
SINTRAN BLOCK SIZE TURNED ON

---

## Page 19

# NORSK DATA A/S - NORD SOFTWARE LIBRARY

## REVISION LOG

| PRODUCT | NAME | ND-NUMBER |
|---------|------|-----------|
| SINTRAN UTILITY PROGRAMS | | 10022R,S |
| (MCOPY-TANB) | | |

| ISSUED | DATE 81.11.13 | BY (INITIALS) TNY |
|--------|---------------|------------------|

| REASON | X ERROR CORRECTION |   DIFFERENT ENVIRONMENT  |
|--------|-------------------|--------------------------|
|        | X CHANGE/ADDITION |                          |

| FILES CHANGED OR NEW FILES | PROG.NUMBER NAME |
|----------------------------|------------------|
|                            | HUT-1649J MCOPY-TANB |

## Errors corrected:

- The program did not work correctly with certain disk and mag-tape configurations. This is corrected.

- Various minor errors corrected.

## Changes made, new features:

- The program may now be used with the subdivided 150 and 288 Mb disks, i.e. DISC-2-75MB-1, DISC-3-75MB-2, etc.

- The program now runs under the TEST-MONITOR version 2441D.

- Disks are now named by standard Sintran names, e.g.
  DISC-75MB-1  
  DISC-90MB-2  
  With the 60/90 Mb Phoenix disks, and the subdivided 150 and 288 Mb disks, the user must now indicate subunit number (the usual way, used in Sintran), instead of surface number (which was used in previous versions of this program).

- When the program is started, it will ask for disk type. If another disk type is wanted, the command  
  SET-DISK-TYPE  
  may be used. The command SET-DISK-DEVICE-NUMBER no longer exists.

- For some of the disk types, the program will ask for old or new directory size. This is due to the fact that directory sizes in Sintran version E or later are a few pages smaller than earlier versions.

---

## Page 20

# NORSK DATA A/S - NORD SOFTWARE LIBRARY

## PRODUCT NAME

| PRODUCT                         | NAME                  | ND-NUMBER  |
|---------------------------------|-----------------------|------------|
| SINTRAN UTILITY PROGRAMS        | (MCOPY-TANB)          | 10022R,S   |

- Commands requiring numeric parameters will now specify what kind of number (decimal or octal) is expected, and the legal range for this number. User input is checked against these specifications.

- Instead of the program asking whether the user wanted system copy or not, there is now a command called `SYSTEM-COPY` which will turn system copy on or off.

- The first file on tape is now file number 0, instead of number 1. Similarly, the tape numbers now start from 0 instead of from 1.

- Earlier versions of the program always did compare after copy. This is now possible to turn off, by using the command `AUTOMATIC-COMPARE`. Note that it is still recommended to do compare after copy!

- When copying directories occupying multiple reels of tape, each tape reel will be rewound and compared before the next tape reel is mounted (unless compare is turned off).

- The command earlier called `SET-1600-BPI` is now called `1600-BPI`.

- The command earlier called `SET-SINTRAN-BLOCK-SIZE` is now called `SINTRAN-BLOCK-SIZE`.

The reason for the change is that these commands may now turn on or off (set or reset) the relevant mode. In fact, the four commands mentioned below all work like this: The relevant feature or mode is alternately turned on or off (set or reset) each time the command is given:

- `AUTOMATIC-COMPARE`
- `SYSTEM-COPY`
- `1600-BPI`
- `SINTRAN-BLOCK-SIZE`

---

## Page 21

# NORD SOFTWARE LIBRARY
## OBJECT DESCRIPTION

| PROGRAM      | PROGRAM NUMBER |
|--------------|----------------|
| NAME DITAP   | SUT - 1880D    |

| REFERENCE    | DATE 79.03.27  | NAME JFB |
|--------------|----------------|----------|

| REVISED      | DATE           | NAME     | PROGRAM CATEGORY |
|--------------|----------------|----------|------------------|

|              | REVISION LOG FOR PROGRAM SUT - 1880D |

| EXPIRED      | DATE           | NAME     | NEW PROGRAM NUMBER |
|--------------|----------------|----------|--------------------|

| COMPUTERS    |  ☐ 28          | ☐ 5      | ☒ 10 | ☐ 12 | ☐ 20 | ☐ 50 | ☒ 10 ~ 32 |

| OPERATING SYSTEMS | ☐ TSS | ☒ SIN III M | ☐ SIN III C | ☒ ALONE |

| TYPE         | ☑ BINARY  | ☐ OCTAL | ☐ BAF | ☐ ABF |

|              | ☐ SYSTEM DESCRIPTION  | ☐ USER DESCRIPTION | ☐ MANUAL |

| DOCUMENTATION TITLE |

## ADDRESS SPACE

| SIZE | 1300₈ | BOUNDARIES | 0 < 1300₈ |
|------|-------|------------|-----------|
| START ADDRESS | 70 | RESTART ADDRESS | 70 |

## PURPOSE

Produce binary files with bootstrap from a "Recover-file". Specially fit for use in making all your subsystems REENTRANT.

---

**LOADING/OPERATING PROCEDURE, USE**

If you have SINTRAN with REENTRANT-SUBSYSTEMS do:

```
@ COPY "DITAP:BPUN" T-R
@ DUMP-REENTRANT ↰ DITAP ↰ 70 ↰ 70 ↰ DITAP
```

If you have SINTRAN version 7.6.0, 615 or older do:

```
@ PLACE T-R
@ DUMP "DITAP-1880B" 70 70
```

Then for both SINTRAN with reentrant subsystems and the older versions:

```
@ DITAP
Destination file: <filename> default type is BPUN. You can use the signs ( , ), ", " in addition to the file name.
```

```
SOURCE FILE: <filename> default type is PROG. You can use the signs ( , ), in addition to the file name.
```

The "PROG-file" will be converted to a "BPUN-file" with BOOTSTRAP and checksum and placed on the destination file. Then this destination file can be used as a reentrant subsystem or only be saved on ")BPUN" format.

---

## Page 22

# NORD SOFTWARE LIBRARY  
## REVISION LOG

| PROGRAM | NAME(S) | PROGRAM NUMBER(S) |
|---------|---------|-------------------|
| DITAP   |         | SUT-1880D<br>SUT-1879D |

| REFERENCE | DATE | NAME |
|-----------|------|------|
|           | 79.03.27 | JFB |

| REASONS                  |                       |
|--------------------------|-----------------------|
| ☒ ERROR CORRECTION       | CORRECTED SSRs        |
| ☐ DIFFERENT ENVIRONMENT  |                       |
| ☐ CHANGE/ADDITION        |                       |

| COMPUTERS | OPERATING SYSTEMS | LANGUAGE (SOURCE) | TYPE (OBJECT) |
|-----------|-------------------|-------------------|---------------|
| ☐ 1       | ☐ TSS             | ☐ MAC             | ☒ BINARY      |
| ☐ 28      | ☒ SIN III M       | ☒ NPL             | ☐ OCTAL       |
| ☐ 5       | ☐ SIN III C       | ☐ FORTRAN         | ☐ BRF         |
| ☒ 10      | ☐ ALONE           | ☐ BASIC           | ☐ AB          |
| ☐ 12      |                   |                   | ☐             |
| ☐ 20      |                   |                   |               |
| ☐ 60      |                   |                   |               |

## CHANGES

The program can now be used in mode- and batch-jobs.

---

## Page 23

# NORD SOFTWARE LIBRARY

## PROGRAM DESCRIPTION

### PROGRAM

| NAME       | PROGRAM NUMBER | PART OF NO NUMBER  |
|------------|----------------|--------------------|
| COP-VERIFY | HUI-2035D      | 10022P,Q,R,S,T     |

### COMPUTERS

| 10 | 12 | 50 | 100 | 500 |
|----|----|----|-----|-----|
| X  | X  | X  |     |     |

### INSTRUCTION SET

| 48 BIT FL. | 32 BIT FL. | COMMERCIAL |
|------------|------------|------------|
| X          | X          |            |

### OPERATING SYSTEMS

| SIN III VS | SIN III RT | ALONE |
|------------|------------|-------|
|            | X          |       |

### PROGRAM TYPE

| BINARY | BRF | SYMBOLIC |
|--------|-----|----------|
| X      |     |          |

### DOCUMENTATION

| NUMBER      |                  |
|-------------|------------------|
|             |                  |
| TITLE       |                  |

### ADDRESS SPACE (For Binary Programs)

| BOUNDARIES | 0 < 017515 |
|------------|------------|
| START ADDRESS | 0       |
| RESTART ADDRESS | 020   |

### PURPOSE

To copy pages between mass storage devices.

## LOADING/OPERATING PROCEDURE, USE

Master Clear, 1560 &

---

## Page 24

# NORD SOFTWARE LIBRARY
## REVISION LOG

| PROGRAM   | NAME       | PROGRAM NUMBER | PART OF NO NUMBER  |
|-----------|------------|----------------|--------------------|
|           | COP-VERIFY | HUT-2035D      | 10022P, Q, R, S, T |

### ISSUED
| DATE      | BY (INITIALS) |
|-----------|---------------|
| 79.07.23  | TEP           |

### REASON
- [ ] ERROR CORRECTION
- [ ] DIFFERENT ENVIRONMENT
- [x] CHANGE/ADDITION

### CHANGES
Will also run Phoenix disks.

---

## Page 25

# NORD SOFTWARE LIBRARY

## PROGRAM DESCRIPTION

| PROGRAM              | PROGRAM      |
|----------------------|--------------|
| **NAME** MCOPY-HP    | **NUMBER** HUT-1650G |

**PART OF**

ND NUMBER - 19022P, Q, R, S, T

### COMPUTERS

- [X] 10
- [ ] 12
- [ ] 50
- [X] 100
- [ ] 500

### INSTRUCTION SET

- [ ] 48 BIT FL.
- [ ] 32 BIT FL.
- [ ] COMMERCIAL

### OPERATING SYSTEMS

- [ ] SIN III VS
- [ ] SIN III RT
- [X] ALONE

### PROGRAM TYPE

- [X] BINARY
- [ ] BRF
- [ ] SYMBOLIC

### DOCUMENTATION

**NUMBER** ND-62.009.03

**TITLE** TEST PROGRAM DESCRIPTION

### ADDRESS SPACE (For Binary Programs)

|             |                        |
|-------------|------------------------|
| **BOUNDARIES** | 20 < 6023           |
| **START ADDRESS**    | 20             |
| **RESTART ADDRESS**  | 20             |

### PURPOSE

To copy disk or drum to and from H-P magnetic tape

## LOADING/OPERATING PROCEDURE, USE

Floppy disk: Load with 1560X

* LOAD MCOPY-HP

Use: See manual

---

## Page 26

# NORD SOFTWARE LIBRARY

## REVISION LOG

| PROGRAM   | NAME     | PROGRAM NUMBER | HUT-1650G |
|-----------|----------|----------------|-----------|
|           | MCOPY-HP | PART OF NO NUMBER | 10022P,Q,R,S,T |

| ISSUED           | DATE 79.12.17 | BY (INITIALS) | DG |
|------------------|---------------|---------------|----|
| REASON           | [X] ERROR CORRECTION | [ ] DIFFERENT ENVIRONMENT | [X] CHANGE/ADDITION |

## CHANGES

The F version did not copy the last cylinders of the 37 and 75 MB disks.

The G version copies the entire disks. The possibility to do only compare between the mag-tape and disk is added.

The printout at tape shift now prints the tape no. The tape-unit specified is now tested if it is ON-LINE, if not a message is printed and the question is asked again.

---

