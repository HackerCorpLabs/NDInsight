## Page 1

# NORD SOFTWARE LIBRARY DISKETTE

**CONTAINING**: COBOL  
48 AND 32 BITS FLOATING FORMAT

## Directory Information

**DIRECTORY NAME**: ND-10020G  
**USER NAME**: FLOPPY-USER

| File Number | Details |
|-------------|---------|
| FILE 0 | (ND-10020G:FLOPPY-USER)COBOL-215OH:BFUN:1 |
| FILE 1 | (ND-10020G:FLOPPY-USER)RUNCOB-2151H:BRF:1 |
| FILE 2 | (ND-10020G:FLOPPY-USER)L-COB-FILE-2332H:BFUN:1 |
| FILE 3 | (ND-10020G:FLOPPY-USER)TPS-RUNCOB-2333H:BRF:1 |
| FILE 4 | (ND-10020G:FLOPPY-USER)ISAMLIB-2306H:BRF:1 |

**22 NOVEMBER 1979**

---

## Page 2

# NORD SOFTWARE LIBRARY

## PROGRAM DESCRIPTION

### Program Details

| Program                               |                               |
|---------------------------------------|-------------------------------|
| **NAME**                              | COBOL                         |
| **PROGRAM NUMBER**                    | COM-2150H                     |
| **PART OF**                           | [illegible]                   |
| **NO NUMBER**                         | 10020G                        |

### Computers

| Options |   |
|---------|---|
| X       | 10|
|         | 12|
|         | 50|
| X       | 100|
|         | 500|
|         | [illegible]|

### Instruction Set

| Options   |                   |
|-----------|-------------------|
| [ ]       | 48 BIT FL.        |
| [ ]       | 32 BIT FL.        |
| X         | COMMERCIAL        |

### Operating Systems

| Options   |                    |
|-----------|--------------------|
| X         | SIN III VS         |
| [ ]       | SIN III RT         |
| [ ]       | ALONE              |

### Program Type

| Options  |          |
|----------|----------|
| X        | BINARY   |
| [ ]      | BRF      |
| [ ]      | SYMBOLIC |

### Documentation

| Field    | Value                                          |
|----------|------------------------------------------------|
| **NUMBER** | 60.089.03/60.090.02                           |
| **TITLE** | NORD-10 COBOL REFERENCE MANUAL                |
|          | NORD-10 COBOL USER'S GUIDE                     |

### Address Space (For Binary Programs)

| Boundaries  | 0        <        65403 |
|-------------|-------------------------|
| **START ADDRESS**   | 0                |
| **RESTART ADDRESS** | 1                |

### Purpose

**NORD 10/100 COBOL COMPILER**

## Loading/Operating Procedure

Number of lines per source listing page can be found in location 5156₈.

```
@DUMP-REENTRANT COBOL 0 1 COBOL-2150H:BPUN
```

---

## Page 3

# NORD Software Library

## Program Description

| PROGRAM      | NAME    | RUNCOS                                                   |
|--------------|---------|----------------------------------------------------------|
| PROGRAM      | NUMBER  | COM-2151H                                                |
| PART OF      | NO NUMBER| 10020G                                                  |
| COMPUTERS    |         | ☐ 10  ☐ 12  ☐ 50  ☑ 100  ☐ 500  ☐ ___  ☐ ___             |
| INSTRUCTION SET |     | ☑ 48 BIT FL.  ☐ 32 BIT FL.  ☐ COMMERCIAL                  |
| OPERATING SYSTEMS |    | ☑ SIN III VS  ☐ SIN III RT  ☐ ALONE                      |
| PROGRAM TYPE |          | ☐ BINARY  ☑ BRF  ☐ SYMBOLIC                             |
| DOCUMENTATION| NUMBER  | 60.089.03 / 60.090.02                                    |
|              | TITLE   | NORD-10 COBOL REFERENCE MANUAL                           |
|              |         | NORD-10 COBOL USERS GUIDE                                |
| ADDRESS SPACE (For Binary Programs) | BOUNDARIES  | <                            |
|              | START ADDRESS     |                                                  |
|              | RESTART ADDRESS   |                                                  |
| PURPOSE      |         | Runtime system for NORD 10/100 COBOL compiler.           |

## Loading/Operating Procedure, Use

[Diagram/Logo Placeholder]

---

## Page 4

# NORD SOFTWARE LIBRARY REVISION LOG

| PROGRAM NAME      | PROGRAM NUMBER | PART OF NO NUMBER |
|-------------------|----------------|-------------------|
| COBOL, RUNCOB     | COM-2150H COM-2151H | 10020G           |

| ISSUED DATE | BY (INITIALS) |
|-------------|---------------|
| 79.11.17    | BSH           |

| REASON                |
|-----------------------|
| ☒ ERROR CORRECTION    |
| ☐ DIFFERENT ENVIRONMENT |
| ☐ CHANGE/ADDITION     |

## CHANGES

1. RUNCOB returns correct **FILE-STATUS** in correct data name if specified in **SELECT** statement.

2. If FILE opened for **OUTPUT** is never used, **CLOSE** now sets **BYTE-POINTER = 0**.

3. MULTIPLE calls to **SORT** in same program now functions correctly.

4. OCCURS clause at **01** level will issue error message.

5. MINOR error in **PACK-DECIMAL** field sign detection corrected.

6. **DECLARATIVE** sections should now function correctly.

---

## Page 5

# NORD SOFTWARE LIBRARY

## PROGRAM DESCRIPTION

| PROGRAM       | NAME                       |
|---------------|----------------------------|
|               | LIST-COBOL-FILE            |
|               | PROGRAM NUMBER             |
|               | SUT-2332 B                 |
|               | PART OF ND NUMBER          |
|               | 10020F,G                   |

| COMPUTERS     | 10  | 12  | 50  | 100 | 500 |
|---------------|-----|-----|-----|-----|-----|
|               | [X] | [X] | [ ] | [X] | [ ] |

| INSTRUCTION SET   | 48 BIT FL. | 32 BIT FL. | COMMERCIAL |
|-------------------|------------|------------|------------|
|                   | [X]        | [ ]        | [ ]        |

| OPERATING SYSTEMS | SIN II/VS  | SIN III RT | ALONE      |
|-------------------|------------|------------|------------|
|                   | [X]        | [ ]        | [ ]        |

| PROGRAM TYPE | BINARY | BRF | SYMBOLIC |
|--------------|--------|-----|----------|
|              | [X]    | [ ] | [ ]      |

| DOCUMENTATION | NUMBER         |
|---------------|----------------|
|               |                |
| TITLE         |                |

| ADDRESS SPACE (For Binary Programs) | BOUNDARIES | 0 < 12000 |
|-------------------------------|--------|-------------|
| START ADDRESS                 | 6652   |             |
| RESTART ADDRESS               | 6652   |             |

### PURPOSE
File listing system part of the debugging system for NORD 10/100 COBOL system.

---

LOADING/OPERATING PROCEDURE, USE

`DUMP-REENTRANT LIST-COB-FILE 6652 6652 L-COB-FILE:BPUN`

---

## Page 6

# Nord Software Library

## Program Description

| PROGRAM                          |                                          |
|----------------------------------|------------------------------------------|
| NAME                             | PROGRAM NUMBER                           |
|                                  | COM-2333H                                |
| TPS-RUNCOB                       | PART OF ND NUMBER                        |
|                                  | 10020G                                   |

| COMPUTERS                  |                         |       |       |
|----------------------------|-------------------------|-------|-------|
| X 10                       | □ 12                    | □ 50  | X 100 |
| □ 500                      | □ __ __  __ __          |       |       |

| INSTRUCTION SET |                                           |
|----------------|-------------------------------------------|
| □ 48 BIT FL    | □ 32 BIT FL.                               |
| X COMMERCIAL   |                                            |

| OPERATING SYSTEMS  |                                  |
|---------------------|----------------------------------|
| X SIN III VS        | □ SIN III RT                     |
| □ ALONE             |                                  |

| PROGRAM TYPE   |                        |
|----------------|------------------------|
| □ BINARY       | X BRF                  |
| □ SYMBOLIC     | □ __ __ __ __ __ __ __ |

| DOCUMENTATION |                    |
|---------------|--------------------|
| NUMBER        |                    |
| TITLE         |                    |

| ADDRESS SPACE                  |                                   |
|--------------------------------|-----------------------------------|
| (For Binary Programs)          | BOUNDARIES <                       |
|                                |                                   |
| START ADDRESS                  | RESTART ADDRESS                   |
|                                |                                   |

| PURPOSE                                     |
|---------------------------------------------|
| Runtime system for TPS-COBOL                |

**LOADING/OPERATING PROCEDURE, USE**

```
._____.     ._____.
|     |_____|     |_____
|_____|     |_____|
```

---

## Page 7

# NORD SOFTWARE LIBRARY

## PROGRAM DESCRIPTION

### PROGRAM

| NAME    | PROGRAM NUMBER |
|---------|----------------|
| ISAMLIB | COM-2306C      |
|         | PART OF        |
|         | ND NUMBER      |
|         | 10020F,G       |

### COMPUTERS

|              | 10 | 12 | 50 | 100 | 500 |     |
|--------------|----|----|----|-----|-----|-----|
| [X]          | [ ]| [ ]| [X]| [ ] | [ ] | [ ] |

### INSTRUCTION SET

| 48 BIT FL. | 32 BIT FL. | COMMERCIAL |
|------------|------------|------------|
| [X]        | [ ]        | [ ]        |

### OPERATING SYSTEMS

| SIN III/VS | SIN III RT | ALONE      |
|------------|------------|------------|
| [X]        | [ ]        | [ ]        |

### PROGRAM TYPE

| BINARY     | BRF        | SYMBOLIC   |
|------------|------------|------------|
| [X]        | [ ]        | [ ]        |

### DOCUMENTATION

|              |                     |
|--------------|---------------------|
| NUMBER       | 60.089.03 / 60.090.02 |
| TITLE        | NORD-10 COBOL REFERENCE MANUAL |
|              | NORD-10 COBOL USER'S GUIDE     |

### ADDRESS SPACE (For Binary Programs)

| BOUNDARIES   |
|--------------|
| START        |
| ADDRESS      |
| RESTART      |
| ADDRESS      |

### PURPOSE

Indexed sequential file system part of the runtime system for NORD 10/100 COBOL compiler.

### LOADING/OPERATING PROCEDURE

USE

---

## Page 8

# NORD SOFTWARE LIBRARY

| PROGRAM   | PAGE 1 OF |
|-----------|-----------|
| NAME      | PROGRAM NUMBER |
| COBOL     | PART OF |
|           | 10020G |
| ISSUED    | ND NUMBER |
| DATE      | 10020G |
| 79.11.17  | BY (INITIALS) |
|           | BSH |

---

## REFERENCE MANUAL CHANGES:

### 10020G COM-2150H COBOL ND-60.089.03

**Page: 3-5 Sect: 3.2.1.3 Par: 4**

Organization is RELATIVE

---

**Page: 5-38 Sect: 5.8.23 Par: 3**

Delete the following:  
"In a SD-ENTRY only the RECORD contains and DATA RECORDS clause may appear".  
These entries are not required with the "new" COBOL SORT (MSD-SORT).

---

**Page: 5-39 SORT USING FILE-NAME-1**

Add new line:  
"FILE-NAME-1 must be fixed length records. QED type files cannot be sorted."

---

**Page: 5-47 Sect: 5.8.29 Par: 5**

The first line should now read:  
"The advancing option is restricted to line-printer files. and files with block contains 0 records and .... etc."

---

## Page 9

# Norsk Data A.S

## COBOL Users Guide - ( ND-60.090.02 )

### Page 2 - 7 Sect. 2.4

#### Add This Paragraph:

**NOTE:** If running a program in batch mode ensure that _only one_ spooling file is opened at a time i.e.: LINE-PRINTER.

This is because COBOL allows only two (2) such files and batch-output is one of these.

You may create more print files however, and `@APPEND-SPOOLING-FILE` at job end.

---

