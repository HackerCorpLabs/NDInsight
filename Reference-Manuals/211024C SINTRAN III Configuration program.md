## Page 1

# ND Software Library Diskette

## Containing:
SINTRAN III Configuration Program

### Directory Name:
211024C01-XX-01S

### User Name: FLOPPY-USER

| No | File name     | Type | T | Public | Friend | Own | Pages | Bytes  |
|----|---------------|------|---|--------|--------|-----|-------|--------|
| 0  | S3-CONFIG-C00 | PROG | I | R      | R      | R   | 90    | 184320 |
| 1  | DDBTABLES-E07 | VTM  | I | R      | R      | R   | 17    | 32448  |

2 files using 107 pages. 148 pages reserved out of 148 pages.

---

## Page 2

# ND SOFTWARE LIBRARY DISKETTE

**PAGE 1**

Containing:  
SINTRAN III Configuration Program  

**Directory Name:**  
211024C01-XX-02S

## User Name: FLOPPY-USER

| No | File name        | Type   | T | Public | Friend | Own | Pages | Bytes |
|----|------------------|--------|---|--------|--------|-----|-------|-------|
| 0  | NOTS-SERVICE-C00 | PROG I | R | R      | R      | 52  | 86016 |

1 file using 52 pages. 148 pages reserved out of 148 pages.

---

## Page 3

# ND SOFTWARE LIBRARY DISKETTE

## PAGE 1

Containing:  
SINTRAN III Configuration Program

Directory Name:  
:211024C01-XX-01D

### User Name: FLOPPY-USER

| No | File name         | Type    | T | Public | Friend | Own | Pages | Bytes  |
|----|-------------------|---------|---|--------|--------|-----|-------|--------|
| 0  | S3-CONFIG-C00     | PROG    | I | R      | R      | R   | 90    | 184320 |
| 1  | DDBTABLES-E07     | VTM     | I | R      | R      | R   | 17    | 32448  |
| 2  | NOT"S-SERVICE-C00 | PROG    | I | R      | R      | R   | 52    | 86016  |

3 files using 159 pages.  
610 pages reserved out of 610 pages.

---

## Page 4

# Program Description

**Date**: 87.10.01  
**Norsk Data A.S**  
**Page**: 1 of 2

## Product

| Name                           | Reg. no. | Category |
|--------------------------------|----------|----------|
| SINTRAN III Configuration program | 211024C | STPR     |

### Reason

- New product
- Error Correction
- X Change/Addition
- Different Environment
- Other: ...................................

## Documentation

| Title                                        | ND-no.      |
|----------------------------------------------|-------------|
| SINTRAN III System Supervisor                | ND-30.003.7 |
| SINTRAN III K Release Information            | ND-60.230.4 |

## Purpose

Configuration of SINTRAN III-VSX (version K or later).

## Prerequisites

| Computer | Type | Floating format | Op. system     | Version  |
|----------|------|-----------------|----------------|----------|
| All      | All  | All             | SINTRAN III VSX | >= K     |

### Minimum permanent mass storage resources

| User   | Userspace | Number of files  |
|--------|-----------|------------------|
| SYSTEM | 155 pages | on 3 files       |

ND-no. for Source 250165C

## File Information

| File Name            | Type | Containing                    |
|----------------------|------|-------------------------------|
| S3-CONFIG-C<rev>     | PROG | Configuration program.        |
| DDBTABLES-E07        | VTM  | VTM terminal tables.          |
| NOTS-SERVICE-8<rev>  | PROG | NOTS service program.         |

## Note

`<rev>` is to be replaced by the current revision of the DIRECTORY or FILE. Revision is found on the "ND SOFTWARE LIBRARY DISKETTE" pages.

## Modifications

The NOTS-Service program is now included as a part of this product. It is only needed for customers connecting to ND machines via One terminal servers (NOTS).

### New Commands

SCSI: Is used to initiate SCSI-disks.

## Installation Procedure

ooo ND Norsk Data ooo

---

## Page 5

# Norsk Data A.S - Program Description

| Date       | Page     |
|------------|----------|
| 87.10.01   | 2 of 2   |

| Product Name                         | Reg. no. | Category |
|--------------------------------------|----------|----------|
| SINTRAN III Configuration program    | 211024C  | STPR     |

## 4 Installation Together With SINTRAN

If this program is installed together with SINTRAN III VSX version K, it will be done by the program NEW-SYSTEM, which is run during installation of SINTRAN.

## 5 Separate Installation

Log in as user SYSTEM. Make sure that user SYSTEM has enough files and disk space. Put the diskette into a diskette drive. Enter the directory:

```
@ENTER-DIRECTORY⬇
DIRECTORY NAME: 211024C<rev>-XX-01⬇
DEVICE NAME: FLOPPY-DISC-<drive-no.>,<floppy-unit>⬇
```

Copy the configuration program to disk:

```
@COPY-FILE⬇
DESTINATION FILE: "S3-CONFIG-C<rev>:PROG"⬇
SOURCE FILE: (211024C:FLOPPY-USER)S3-CONFIG-C:PROG⬇
```

Remember to delete the old S3-CONFIG-A/B, because the C-version has a new datastucture in the configuration file.

If your system does not already have a file called DDBTABLES-D:VTM nor DDBTABLES-E:VTM you must also copy it from the diskette:

```
@COPY-FILE⬇
DESTINATION FILE: "DDBTABLES-E07:VTM"⬇
SOURCE FILE: (211024C:FLOPPY-USER)DDBTABLES-E07:VTM⬇
```

The configuration program is now completely installed.

If you have Net/One terminal server controllers in your ND machine, you need to install the NOTS-Service program:

```
@COPY-FILE⬇
DESTINATION FILE: "NOTS-SERVICE-C<rev>:PROG"⬇
SOURCE FILE: (211024C:FLOPPY-USER)NOTS-SERVICE-B:PROG⬇
```

ooo ND Norsk Data ooo

---

## Page 6

I'm sorry. The image appears to be blank or not contain recognizable text. Please try again with a different image.

---

