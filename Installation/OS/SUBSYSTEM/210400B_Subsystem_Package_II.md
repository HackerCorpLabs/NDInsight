## Page 1

# ND SOFTWARE LIBRARY DISKETTE

---

**PAGE 1**

## Containing:
SUBSUSTEM PACKAGE II

**Directory Name:**
21040B00-XX-01S

---

### User Name: FLOPPY-USER

| No | File Name         | Type  | T | Public | Friend | Own   | Pages | Bytes |
|----|-------------------|-------|---|--------|--------|-------|-------|-------|
| 0  | FMAC-1408D        | BPUN  | I | R      | RWA    | RWACD | 16    | 28844 |
| 1  | MAC-1415C         | BPUN  | I | R      | RWA    | RWACD | 15    | 28389 |
| 2  | QED-1644L         | BPUN  | I | R      | RWA    | RWACD | 10    | 17520 |
| 3  | F32-MAC-1628C     | BPUN  | I | RWACD  | RWACD  |       | 16    | 29001 |
| 4  | F32-FMAC-1920C    | BPUN  | I | R      | RWA    | RWACD | 16    | 28844 |
| 5  | DITAP-1880D       | BPUN  | I | R      | RWA    | RWACD | 2     | 1974  |
| 6  | NPL-1896D         | BPUN  | I | R      | RWA    | RWACD | 32    | 62288 |

7 files using 107 pages. 148 pages reserved out of 148 pages.

---

## Page 2

# ND SOFTWARE LIBRARY DISKETTE

## PAGE 1

Containing:

**SUBSUSTEM PACKAGE II**

Directory Name: 210400B00-XX-01D

---

### User Name: FLOPPY-USER

| No | File name      | Type  | T | Public | Friend | Own   | Pages | Bytes |
|----|----------------|-------|---|--------|--------|-------|-------|-------|
| 0  | FMAC-1408D     | BPUN  | I | R      | RWA    | RWACD | 16    | 28844 |
| 1  | MAC-1415C      | BPUN  | I | R      | RWA    | RWACD | 15    | 28389 |
| 2  | QED-1644L      | BPUN  | I | R      | RWA    | RWACD | 10    | 17520 |
| 3  | F32-MAC-1628C  | BPUN  | I | RWACD  | RWACD  | RWACD | 16    | 29001 |
| 4  | F32-FMAC-1920C | BPUN  | I | R      | RWA    | RWACD | 16    | 28844 |
| 5  | DITAP-1880D    | BPUN  | I | R      | RWA    | RWACD | 2     | 1974  |
| 6  | NPL-1896D      | BPUN  | I | R      | RWA    | RWACD | 32    | 62288 |

7 files using 107 pages. 148 pages reserved out of 610 pages.

---

## Page 3

# Norsk Data A.S

## PROGRAM DESCRIPTION

**Date**: 87.05.20

**Page**: 1 of 4

### Product

| Name                   | Reg. no. | Category |
|------------------------|----------|----------|
| SUBSYSTEM PACKAGE II   | 210400B  | STIN     |

### Reason

- New product
- Error Correction
- Change/Addition
- x Different Environment
- Other: .....................................

### Documentation

| Title                   | Reg. no.   |
|-------------------------|------------|
| MAC Users Guide         | 60.096.01 EN  |
| SINTRAN III Utilities Manual | 60.151.2A EN |
| QED User's Manual       | 60.031.4C EN  |
| NORD PL User's Guide    | 60.047.3A EN  |

### Purpose

Basic Utilities for SINTRAN III

### Prerequisites

| Computer | Type | Floating format | Op. system | Version |
|----------|------|-----------------|------------|---------|
| ND-100   | Any  | All             | SIN III    | >= H    |

Minimum mass storage resources for installation

| User | User space | Number of files |
|------|------------|-----------------|
| <any>| 107 pages  | 7 files         |

Minimum permanent mass storage resources

| User | User space | Number of files |
|------|------------|-----------------|
| <any>| 107 pages  | 7 files         |

### Product consists of

| MODULE-no.  | Name                    | Reg. no. |
|-------------|-------------------------|----------|
| SUT-1880D   | DITAP                   | 210120B  |
| MAC-1920C   | FMAC 32 bit floating-point | 210120B  |
| MAC-1408D   | FMAC 48 bit floating-point | 210120B  |
| MAC-1628C   | MAC 32 bit floating-point | 210120B  |
| MAC-1415C   | MAC 48 bit floating-point | 210120B  |
| NPL-1896D   | NORD-PL                 |          |
| SUT-1644L   | QED                     |          |

### File Information

| File Name      | Type | Containing                 |
|----------------|------|----------------------------|
| DITAP-1880D    | BPUN | Ditap                      |
| F32-FMAC-1920C | BPUN | FMAC 32 BIT                |
| F32-MAC-1628C  | BPUN | MAC 32 BIT                 |
| FMAC-1415C     | BPUN | FMAC 48 BIT                |
| MAC-1415C      | BPUN | MAC 48 BIT                 |
| NFL-1896D      | BPUN | NORD-PL Language           |
| QED-1644L      | BPUN | QED, Quick editor          |

ooo ND Norsk Data ooo

---

## Page 4

# TABLE OF CONTENTS

| Section                      | Page |
|------------------------------|------|
| 1 ERRORS CORRECTED           | 2    |
| 1.1 QED                      | 2    |
| 2 MODIFICATIONS              |      |
| 2.1 CHANGES                  | 2    |
| 2.1.1 MAC and FMAC           | 2    |
| 2.2 CHANGED COMMANDS         | 3    |
| 2.3 NORD-PL                  | 3    |
| 2.4 QED                      | 3    |
| 3 INSTALLATION PROCEDURE     | 4    |
| 3.1 How to dump programs reentrant | 4    |
| 3.2 How to dump programs as :PROG files | 4    |

ooo ND Norsk Data ooo

---

## Page 5

| Date       | Norsk Data A.S              | Page 2 of 4 |
|------------|-----------------------------|------------|
| Product    | Name                        | Reg. no.   | Category |
| SUBSYSTEM PACKAGE II | 21040OB | STIN    |

# 1 ERRORS CORRECTED

## 1.1 QED

- Some errors when reading a specified interval of lines from a file is corrected.

- An error when specifying 12 tabulator stops is corrected.

# 2 MODIFICATIONS

## 2.1 CHANGES

### 2.1.1 MAC and FMAC

The only change is: listing of "empty" library marks is removed.

An empty library mark is a library mark or an expression of library marks that has the value FALSE, thereby skipping the code behind it up to next ". 

ooo ND Norsk Data ooo

---

## Page 6

| Date       | Norsk Data A.S                   | Page 3 of 4         |
|------------|---------------------------------|---------------------|
| Product    | Name                            | Reg. no.  | Category |
|            | SUBSYSTEM PACKAGE II            | 210400B   | STIN     |

## 2.2 CHANGED COMMANDS

## 2.3 NORD-PL

- The statement-  
  IF A NBIT <symbol> GO <integer pointer>;  
  does now generate correct code.

- The incorrect statements-  
  A BZERO; and A BONE;  
  does now generate an error message.

- If a MAC system symbol is used as subroutine entrypoint, a warning will be given by the NIRD-PL compiler about the symbol change.

- The error recovery after a source line > 128 characters is improved.

- The statements-  
  SYMBOL XY=1  
  ...  
  A:=D+XY  
  does now generate correct code.

## 2.4 QED

- The edit character (CTRL)R which skip next "word" is changed to (CTRL)G.

- (CTRL)R now retypes the old and new line and awaits more edit characters (same as in command buffer editing in SINTRAN III).

ooo ND Norsk Data ooo

---

## Page 7

# Program Description

| Date       | Norsk Data A.S | Page 4 of 4 |
|------------|----------------|-------------|
| Product    | Name           | Reg. no.    | Category  |
| SUBSYSTEM PACKAGE II | 210400B  | STIN       |

## 3 Installation Procedure

Delete the old :PROG and :BPUN versions of the products.

Use the BACKUP-SYSTEM to copy all the files to the user where the BPUN-files are kept. Standard user is user BPUN-FILES.

The BPUN files on the diskette may be dumped as :PROG files or dumped REENTRANT. The next sections will describe how this is done.

### 3.1 How to dump programs reentrant

Login as user SYSTEM and dump the files reentrant with following parameters:

| Name            | Start, Restart adr | File name: DITAP-    |
|-----------------|--------------------|----------------------|
| 18B0D 70 70     | (BPUN-FILES)DITAP-18B0D |
| F32-FMAC-1920C  | -1 -3              | (BPUN-FILES)F32-FMAC-1920C |
| MAC-32-1626C    | -1 -3              | (BPUN-FILES)F32-MAC-1628C |
| FMAC-1408D      | -1 -3              | (BPUN-FILES)FMAC-1408D |
| MAC-1415C       | -1 -3              | (BPUN-FILES)MAC-1415C |
| NPL 0 1         | (BPUN-FILES)NPL-1896D |
| QED 0 1         | (BPUN-FILES)QED-1644L |

Example:

    @DUMP-REENTRANT↵
    NAME: QED↵
    START ADDRESS: 0↵
    RESTART ADDRESS: 1↵
    FILE NAME: (BPUN-FILES)QED-1644L↵

### 3.2 How to dump programs as :PROG files

Login as user SYSTEM or user where :PROG files should be dumped and dump with same parameters as in section 3.1.  
Example:

    @PLACE-BINARY,(BPUN-FILES)QED-1644L↵
    @DUMP↵
    FILE NAME: "QED"↵
    START ADDRESS: 0↵
    RESTART ADDRESS: 1↵

ooo ND Norsk Data ooo

---

## Page 8

I'm sorry, but the image you uploaded is blank. Please provide a different image or check if you've uploaded the correct file.

---

