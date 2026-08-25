## Page 1

# ND SOFTWARE LIBRARY DISKETTE

Containing: SINTRAN III MONITOR CALL Package

## Directory Name: 210913A00-XX-01S

User Name: FLOPPY-USER

### File Access

| Num | File name          | Type | T | Public | Friend | Own   | Pages | Bytes |
|-----|--------------------|------|---|--------|--------|-------|-------|-------|
| 0   | MON-CALL-1B-A00    | BRF  | I | R      | RWACD  | RWACD | 4     | 6120  |
| 1   | MON-CALL-2B-A00    | BRF  | I | R      | RWACD  | RWACD | $     | 6159  |
| 2   | MON-CALL-LIB-A00   | NRF  | I | R      | RWACD  | RWACD | 5     | 7923  |
| 3   | MON-CALL-NAMES-A   | DATA | I | R      | RWACD  | RWACD | 5     | 7379  |

```
=====================================================================
4 files using 19 pages. 148 pages reserved out of 148 pages 
=====================================================================
```

---

## Page 2

# ND SOFTWARE LIBRARY DISKETTE

## PAGE 1

Containing: SINTRAN III MONITOR CALL Package

**Directory Name:** 210913A00-XX-01D  
**User Name:** FLOPPY-USER  

| Num | File name                   | Type | T | Public | Friend | Own   | Pages | Bytes |
|-----|-----------------------------|------|---|--------|--------|-------|-------|-------|
| 0   | MON-CALL-1B-A00.BRF         | I R  | RWACD  | RWACD  |       | 4     | 6120  |
| 1   | MON-CALL-2B-A00.BRF         | I R  | RWACD  | RWACD  |       | 5     | 6159  |
| 2   | MON-CALL-LIB-A00.NRF.1.R    | RWACD| RWACD  | 5     | 7923  |
| 3   | MON-CALL-NAMES-A-DATA-...   | RWACD| RWACD  | 5     | 7379  |

---

4 files using 19 pages. 610 pages reserved out of 610 pages.

---

---

## Page 3

# Program Description

| Date        | Norsk Data A.S       | Page 1 of 2     |
|-------------|----------------------|-----------------|
| **Program Description** |                      |                 |

| Product   | Name                          | Reg. no. | Category |
|-----------|-------------------------------|----------|----------|
|           | SINTRAN III MONITOR CALL Package | 210913A | STPR     |

| Reason         |                                   |
|----------------|-----------------------------------|
| x New product  | Error Correction                  |
| Change/Addition| Different Environment             |

| Documentation | Title                       | Reg. no.  |
|---------------|-----------------------------|-----------|
|               | SINTRAN III Monitor Calls   | 60.228.1 EN |

| Purpose  |
|----------|
| ND100 and ND500 monitor call interface for FORTRAN, COBOL and PLANC. |

| Prerequisites | Computer  | Type | Floating format | Op. system Version |
|---------------|-----------|------|-----------------|---------------------|
|               | NO-10/NO-100 All | All   | SINTRAN III All      |
|               | or ND-500        |       |                     |

| Minimum permanent mass storage resources |
|------------------------------------------|
| User   | Userspace | Number of files      |
| Any user | 11      | pages on 3 files    |
| SYSTEM | 4       | pages on 1 files     |

| Reg. no. for Source |
|---------------------|
| 250104A             |

## Directory Details

| Directory Name: 210913A<rev>-XX-01 | User Name: FLOPPY-USER |
|------------------------------------|------------------------|
| File no. | File Name           | Type     | Containing                     |
|----------|---------------------|----------|--------------------------------|
| 208287A  | MON-CALL-1B-A<rev>  | BRF      | Monitor Call ND100 1bank       |
| 208288A  | MON-CALL-2B-A<rev>  | BRF      | Monitor Call ND100 2bank       |
| 208289A  | MON-CALL-LIB-A<rev> | NRF      | Monitor Call ND500.            |
| 208290A  | MON-CALL-NAMES-A    | DATA     | Monitor Call names.            |

* `<rev>` = Current revision

# Installation Procedure

Enter the directory on the floppy and copy the files. The file MON-CALL-NAMES-A<rev>.DATA must be copied to user SYSTEM, for other files user SYSTEM may be used but are not needed. The files must have public read access.

```
O>ENTER-DIRECTORY 210913A-XX-01_FLOPPY-DISC-<drive-no.>\<floppy-unit>
O>CREATE-FILE MON-CALL-1B-A<rev>.BRF 0
O>CREATE-FILE MON-CALL-2B-A<rev>.BRF 0
O>CREATE-FILE MON-CALL-LIB-A<rev>.NRF 0
O>CREATE-FILE MON-CALL-NAMES-A.DATA 0
O>SET-FILE-ACCESS MON-CALL-1B-A<rev>.BRF,R,R,RWACD
O>SET-FILE-ACCESS MON-CALL-2B-A<rev>.BRF,R,R,RWACD 
O>SET-FILE-ACCESS MON-CALL-LIB-A<rev>.NRF,R,R,RWACD
O>SET-FILE-ACCESS MON-CALL-NAMES-A.DATA,R,R,RWACD
A>COPY-FILE
   MON-CALL-1B-A<rev>.BRF
   [210913A<rev>-XX-01:FLOPPY-USER]MON-CALL-1B-A<rev>.BRF
A>COPY-FILE 
   MON-CALL-2B-A<rev>.BRF
```

000 ND Norsk Data 000

---

## Page 4

# Program Description

| Date       | Not'5K Datä A/S   | Page 2 of 2 |
|------------|-------------------|-------------|
| Product    | Name'             | Reg. no.    | Category |
|            | STNTRAN III MONITOR'CALL Package | 210913A | STPR |

## 210913A(rev):XX-01:FLOPPY-USER)MON-CALL-28-A(rev):BRF

### COPY-FILE

MON-CALL-LIB-A(rev):NRF

210913A(rev):XX-01:FLOPPY-USER)MON-CALL-LIB-A(rev):NRF

### COPY-FILE

MON-CALL-NAMES-A:DATA

210913A(rev):XX-01:FLOPPY-USER)MON-CALL-NAMES-A:DATA

#### RELEASE-DIRECTORY 210913A:XX-01

```plaintext
+-------------------+-------------------+
| Coordinates       | SOME   |  SOME    |
| MONITOR III     | DATA   |  MORE    |
| MONITOR III     | DATA   |  MORE    |
| MONITOR CALL    | DECO   |  MORD    |
+-------------------+-------------------+
```

ooo NO Norsk Datä ooo

---

## Page 5

# PROGRAM DESCRIPTION

| Date       | 86.08.01             | Norsk Data. A.S.          | Page 1 of 2 |
|------------|----------------------|---------------------------|-------------|
| Product    | Name                 | Reg. no.    | Category  |
|            | SINTRAN III MONITOR CALL Package | 210913A | STPR      |

## Reason

- [x] New product
- [ ] Change/Addition
- Error Correction
- Different Environment

## Documentation

| Title                    | Reg. no.   |
|--------------------------|------------|
| SINTRAN III Monitor Calls | 60.228.1 EN |

## Purpose

ND100 and ND500 monitor call interface for FORTRAN, COBOL and PLANC.

## Prerequisites

| Computer       | Type    | Floating format | Op. system | Version   |
|----------------|---------|-----------------|------------|-----------|
| ND-10/ND-100   | All     | All             | SINTRAN III| All       |
| or ND-500      |         |                 |            |           |

### Minimum permanent mass storage resources

| User   | Userspace | Number of files         |
|--------|-----------|-------------------------|
| Any user | 11 pages | on 3 files              |
| SYSTEM  | 4 pages  | on 1 file               |

| Directory Name: 210913A<reV>-XX-01 | User Name: FLOPPY-USER |
|------------------------------------|------------------------|

| File no. | File Name         | Type | Containing                |
|----------|-------------------|------|---------------------------|
| 208287A  | MON-CALL-1B-A<rev> | BRF  | Monitor Call ND100 1bank  |
| 208288A  | MON-CALL-2B-A<rev> | BRF  | Monitor Call ND100 2bank  |
| 208289A  | MON-CALL-LIB-A<rev>| NRF  | Monitor Call ND500.      |
| 208290A  | MON-CALL-NAMES-A   | DATA | Monitor Call names.      |

*<rev> = Current revision  

## 1 INSTALLATION PROCEDURE

Enter the directory on the floppy and copy the files. The file MON-CALL-NAMES-A<rev>:DATA must be copied to user SYSTEM, for the other files user SYSTEM may be used but are not needed. The files must have public read access.

```
ENTER-DIRECTORY 210913A-XX-01,FLOPPY-DISC-<drive-no.>,<floppy-unit>
CREATE-FILE MON-CALL-1B-A<rev>,BRF 0
CREATE-FILE MON-CALL-2B-A<rev>,BRF 0
CREATE-FILE MON-CALL-LIB-A<rev>,NRF 0
CREATE-FILE MON-CALL-NAMES-A,DATA 0
SET-FILE-ACCESS MON-CALL-1B-A<rev>,BRF,R,R,RWACD
SET-FILE-ACCESS MON-CALL-2B-A<rev>,BRF,R,R,RWACD
SET-FILE-ACCESS MON-CALL-LIB-A<rev>,NRF,R,R,RWACD
SET-FILE-ACCESS MON-CALL-NAMES-A:DATA,R,R,RWACD

COPY-FILE
MON-CALL-1B-A<rev>,BRF
(210913A<rev>-XX-01:FLOPPY-USER)MON-CALL-1B-A<rev>,BRF

COPY-FILE
MON-CALL-2B-A<rev>,BRF
```

```
000  ND Norsk Data  000
```

---

## Page 6

# Program Description

| Date     | Norsk Data A.S | Page |
|----------|----------------|------|
| 86.08.01 |                | 2 of 2 |

| Product | Name                             | Reg. no. | Category |
|---------|----------------------------------|----------|----------|
|         | SINTRAN III MONITOR CALL Package | 210913A  | STPR     |

## File Information

- `(210913A<rev>-XX-01:FLOPPY-USER)MON-CALL-ZB-A<rev>:BRF`
  - **COPY-FILE**
  - `MON-CALL-LIB-A<rev>:NRF`
  
- `(210913A<rev>-XX-01:FLOPPY-USER)MON-CALL-LIB-A<rev>:NRF`
  - **COPY-FILE**
  - `MON-CALL-NAMES-A:DATA`
  
- `(210913A<rev>-XX-01:FLOPPY-USER)MON-CALL-NAMES-A:DATA`
  - **RELEASE-DIRECTORY 210913A-XX-01**

[Illegible content]

```
          .       .
                  .
                   .
```

000 ND Norsk Data 000

---

## Page 7

# Source Description

| Date      | Norsk Data A/S | Page 1 of 2        |
|-----------|----------------|--------------------|
| 86.08.01  | SOURCE DESCRIPTION |                |

| Product       | Name                                  | ND-no:    | Category |
|---------------|---------------------------------------|-----------|----------|
|               | SINTRAN III MONITOR CALL Package      | 250104A   | SPEC     |

| Issued by        | Project Leader (initials), ORA          |                        |
|------------------|-----------------------------------------|------------------------|

| Document-        | Title                                   | ND-no.                 |
| tation           | SINTRAN III Monitor Calls               | 60.288.1 EN            |

## Object Products

| ND-no.   | Product Name                                |
|----------|----------------------------------------------|
| 210913A  | MONITOR CALL Package ND100/500               |

## Tools

| Computer Type Floating Format | Op. System | Version |
|-------------------------------|------------|---------|
| ND-500                        | SIN TII VSX| K       |

| ND-no.   | Product Name (SW/HW)                         |
|----------|----------------------------------------------|
| 210309F  | PLANC for ND-100                             |
| 210310F  | PLANC for ND-500                             |
| 210721B  | BRF-Linker for ND-100                        |
| 210319F  | Linkage Loader for ND-500                    |

## Description, Compile Instruction, Use

The monitor call library for ND-100 and ND-500 consists of:

```
(210913A00-XX-01D:FLOPPY-USER)MON-CALL-100-A00:SYMB
(210913A00-XX-01D:FLOPPY-USER)MON-CALL-500-A00:SYMB
(210913A00-XX-01D:FLOPPY-USER)MON-CALL-NAMES-A:DATA
```

The following procedure is used for compiling:

```
Θcreate-file mon-call-lib-a00:nrf,0
Θcreate-file mon-call-1b-a00:brf,0
Θcreate-file mon-call-2b-a00:brf,0
Θcreate-file mon-call-REE:brf,0
θplanc-500
library ON
compile mon-call-500-a00,0,mon-call-lib-a00
Θlinkage-loader
fetch-nrf-module (system)planc-lib-f,mon-call-lib-a00,#demand,#demand
exit
θplanc-100
constant ree_fort=FALSE
library ON
compile mon-call-100-a00,0,mon-call-1b-a00
θplanc-100
constant ree_fort=TRUE
library ON
compile mon-call-100-a00,0,mon-call-REE
@BRF-LINKER
app-brf mon-call-REE,mon-call-1b-a00,$mon_f
exit
Θdelete-file mon-call-REE:brf
Θcreate-file mon-call-REE:brf,0
θplanc-100
constant ree_fort=FALSE
separate
library ON
```

*** SOURCE DESCRIPTION ***

---

## Page 8

# Source Description

| Product | Name                             | ND-no.  | Category |
|---------|----------------------------------|---------|----------|
|         | SINTRAN III MONITOR CALL Package | 250104A | SPEC     |

```
compile mon-call-100-a00,0,mon-call-2b-a00
øplanc-100
constant ree_fort=TRUE
separate
library ON
compile mon-call-100-a00,0,mon-call-REE
øBRF-LINKER
append-brf mon-call-REE,mon-call-2b-a00,5mon_f
exit
ødel-file mon-call-REE:brf
øSET-FILE-ACC MON-CALL-TB-A00:BRF,R,R,RWACD
øSET-FILE-ACC MON-CALL-2B-A00:BRF,R,R,RWACD
øSET-FILE-ACC MON-CALL-LIB-A00:NRF,R,R,RWACD
```

*** SOURCE DESCRIPTION ***

---

