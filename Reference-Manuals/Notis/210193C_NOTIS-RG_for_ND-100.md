## Page 1

# ND Software Library Diskette

### Page 1

Containing: NOTIS-RG for ND-100 (English version)

**Directory Name:** 210193C02-EN-010D

**User Name:** FLOPPY-USER

| Nr | File name         | Type  | File access:         | Pages | Bytes  |
|----|-------------------|-------|----------------------|-------|--------|
|    |                   |       | T   | Public | Friend | Own   |       |        |
| 0  | INST-RG-100-C02   | PROG  | I   | RWACD  | RWACD  | RWACD | 65    | 187292 |
| 1  | RG-SETUP-C02      | XCOM  | I   | RWACD  | RWACD  | RWACD | 5     | 8010   |
| 2  | NOTIS-RG1-EN-C02  | PROG  | I   | RWACD  | RWACD  | RWACD | 73    | 403456 |
| 3  | NOTIS-RP1-EN-C02  | PROG  | I   | RWACD  | RWACD  | RWACD | 72    | 399360 |
| 4  | UE-ERMSG-EN-B03   | ERR   | I   | RWACD  | RWACD  | RWACD | 46    | 90880  |
| 5  | NOTIS-RG0-EN-C02  | PROG  | I   | RWACD  | RWACD  | RWACD | 98    | 401408 |
| 6  | RG-FORMS-EN-C02   | NDPF  | I   | RWACD  | RWACD  | RWACD | 21    | 40960  |
| 7  | NOTIS-RP0-EN-C02  | PROG  | I   | RWACD  | RWACD  | RWACD | 80    | 397312 |
| 8  | DD8TABLES-E-E04   | VTM   | I   | RWACD  | RWACD  | RWACD | 13    | 23274  |
| 9  | RG-EXLIB-EN-C00   | RPRT  | C   | RC     | RWAC   | RWACD | 10    | 20480  |
| 10 | RG-PRIV-EN-C00    | TEXT  | I   | RC     | RWAC   | RWACD | 2     | 699    |
| 11 | RG-PERSON-EN-C00  | TEXT  | I   | RC     | RWAC   | RWACD | 5     | 6221   |

12 files using 490 pages. 610 pages reserved out of 610 pages.

---

## Page 2

# ND SOFTWARE LIBRARY DISKETTE

## Page 1

Containing NOMIS-RG for ND-100 (English version)

Directory Name: 210193C02-EN-04S

User Name: FLOPPY-USER

### File Access:

| Nr | File name         | Type  | T  | Public | Friend | Own  | Pages | Bytes  |
|----|-------------------|-------|----|--------|--------|------|-------|--------|
| 0  | NOTIS-RPD-EN-C02  | PROG  | I  | RWACD  | RWACD  | RWACD| 80    | 397312 |
| 1  | DDBTABLES-E-E04   | VTM   | I  | RWACD  | RWACD  | RWACD| 13    | 23274  |
| 2  | RG-EXLIB-EN-C00   | RPRT  | C  | RC     | RWAC   | RWACD| 10    | 20480  |
| 3  | RG-PRIV-EN-C00    | TEXT  | I  | RC     | RWAC   | RWACD| 2     | 699    |
| 4  | RG-PERSON-EN-C00  | TEXT  | I  | RC     | RWAC   | RWACD| 5     | 6221   |

5 files using 110 pages. 148 pages reserved out of 148 pages.

---

## Page 3

# ND Norsk Data

### Software INFORMATION Report

| Date       | NOTIS-RG for ND-100 | Report No: 210193C |
|------------|---------------------|--------------------|
| 861120     |                     | 4.1                |

## Program: All

### Subject: New revision of NOTIS-RG and NOTIS-RP, revision C02

The C02 revision of NOTIS-RG includes ISAM version J. This gives improved performance for multiuser ISAM operation.

**NOTE!** Because of the new ISAM version, the installation procedure for NOTIS-RG is slightly changed:  
On ND-100 the patch address to use when changing ISAM multiuser device is changed from block 139, word 40 to block 139, word 42. See section 'CHANGE ISAM INTERNAL DEVICE' in Program Description Sheet for NOTIS-RG for ND-100. The patch should be done this way for NOTIS-RP-C02:

- Log in on user NOTIS
- @look-file <CR>
- >>OPEN NOTIS-RPO-EN-C:PROG <CR>
- >> <CR>
- >>PATCH 139,42 <CR>
- 2028 <CR> (or another device number)
- . <CR>
- EXIT <CR>

Errors corrected in revision C02:

- Edit the 'Length' column in SIBAS infile description (required when e.g. redefining from INTEGER to PACKED DECIMAL).
- Defining two outfiles with same number but different names.

---

## Page 4

# ND Software Library Diskette

## NOTIS-RG for ND-100
### (Swedish version)

---

### Directory Name: 210193C02-SW-010

**User Name:** FLOPPY-USER

**File access:**

| Nr | File name         | Type | Public | Friend | Own  | Pages | Bytes  |
|----|-------------------|------|--------|--------|------|-------|--------|
| 0  | INST-RG-100-C02   | PROG | I      | R      | RWACD| 65    | 187232 |
| 1  | RG-SETUP-C02      | PROG | I      | R      | RWACD| 5     | 8010   |
| 2  | NOTIS-RG1-SW-C02  | PROG | I      | R      | RWACD| 73    | 403456 |
| 3  | NOTIS-RP1-SW-C02  | PROG | I      | R      | RWACD| 72    | 393360 |
| 4  | UE-ERMSG-SW-010   | ERRC | C      | I      | RWACD| 49    | 97200  |
| 5  | NOTIS-RG0-SW-C02  | PROG | I      | R      | RWACD| 98    | 401408 |
| 6  | DDBTABLES-E-E04   | VTM  | I      | R      | RWACD| 13    | 23274  |
| 7  | RG-FORM-SW-C02    | PROG | T      | I      | RWACD| 21    | 40950  |
| 8  | NOTIS-RP0-SW-C02  | PROG | I      | R      | RWACD| 80    | 397312 |
| 9  | RG-EXBIB-SW-C00   | PRTC | C      | RC     | RWAC | 10    | 20480  |
| 10 | RG-PRTWS-SW-C00   | TEXT | I      | I      | RWAC | 2     | 956    |
| 11 | RG-PERSON-SW-C03  | TEXT | I      | I      | RWAC | 5     | 6414   |

---

65 pages in 65 files, 1435 pages reserved out of 610 pages.

---

## Page 5

# ND Norsk Data Software INFORMATION Report

**Page:** 2

| NOTIS-RG for ND-100 | 210193C | Report no: 4 |
|---------------------|----------|--------------|

**Date:** 861120

## Install NOTIS-RG when actual users have got more than 255 object entries.

- Run reports with sorting on terminals with terminal number > 399.
- Searching in SIBAS substream on non-unique key if owner field has parity (e.g. from WP file).
- Reflecting the TOTAL or SUBTOTAL fields in printout even if the result is erroneous.
- The function NUMBER-DF-DAYS ((<date1>,<date2>) now gives negative result if <date1> > <date2>.
- MIN or MAX function in Named Expression with condition.

---

## Page 6

# Norsk Data A.S

## PROGRAM DESCRIPTION

### Date: 87.04.02

**Page 1 of 8**

**Product Name**  
NOTIS-RG for ND-100

| Reg. no. | Category |
|----------|----------|
| 210193C  | STPR     |

### Reason
* Change/Addition

### Documentation

| Title                          | Reg. no.  |
|--------------------------------|-----------|
| NOTIS-RG Reference Manual      | 63.013.5 EN |
| NOTIS-RG Reference Card        | 99.003.1 EN |
| NOTIS-RG Eksempler og Øvelser  | 63.040.1 EN |
| NOTIS-RG Referanshåndbok       | 63.014.5 NO |
| NOTIS-RG Oppslagskort          | 99.003.1 NO |
| NOTIS-RG Referenshandbok       | 63.013.5 SW |
| NOTIS-RG Referenzhandbuch      | 63.013.5 GE |
| NOTIS-RG Referenzkarte         | 99.003.1 GE |

### Purpose
General purpose report generator

### Prerequisites

**Computer Type:** ND-100  
**Floating format:** 48 or 32 bit  
**Op. system Version:** SINTRAN III: I,J,K

Minimum mass storage resources for installation:

| User      | Userspace | Number of files |
|-----------|-----------|-----------------|
| SYSTEM    | 213 pages | on 10 files     |
| NOTIS     | 351 pages | on 7 files      |

Minimum permanent mass storage resources:

| User      | Userspace | Number of files |
|-----------|-----------|-----------------|
| SYSTEM    | 150 pages | on 6 files      |
| NOTIS     | 345 pages | on 16 files     |

Max/Min number of RQ descriptions: Max 10 / Min 0  
Max/Min number of segments: Max 4 / Min 4  
Space requirements on segment file(s) is 320 pages  

For Source: Reg no: 210577C

### File Name

| File Name                 | Type   | Containing   |
|---------------------------|--------|--------------|
| INST-RG-100-<rev>/        | PROG   | Installation program |
| RG-SETUP<rev>             | XCOM   | Inst. utilities    |
| NOTIS-RG1-<lang>-<C:rev> | PROG   | Sub segment RG    |
| NOTIS-RP1-<lang>-<C:rev> | PROG   | Sub segment RP    |
| UE-ERMSG-<lang>-B<rev>   | ERR    | Error messages    |
| NOTIS-RG0-<lang>-C<rev>  | PROG   | Main segment RG   |
| RG-FORMS-<lang>-C<rev>   | NDPF   | NDP forms and help |
| NOTIS-RPQ-<lang>-C<rev>  | PROG   | Main segment RP1093X# |
| DDBTABLS-E-E04            | VTM    | Terminal descriptions |
| RG-EXLIB-<lang>-C<rev>   | RPRT   | Example reports  |
| RG-PRIV-<lang>2-C<rev>   | TEXT   | Example data     |
| RG-PERSON-<lang>-C<rev>  | TEXT   | Example data     |

Note: <rev> is to be replaced by the current revision of the DIRECTORY or FILE. The revision level is found on the preceding "ND SOFTWARE LIBRARY DISKETTE" pages.

`<lang>` is to be replaced by the relevant language version.

---

ND Norsk Data

---

## Page 7

Date 87.04.02  
Norsk Data A.S  
Page 2 of 8  

# PROGRAM DESCRIPTION

| Product      |                      | Reg. no. | Category |
|--------------|----------------------|----------|----------|
| Name         | NOTIS-RG for ND-100  | 210193C  | STPR     |

## 1 ERRORS CORRECTED

- Contiguous files are opened for READ-COMMON access, allowing other products to open the file for WRITE COMMON during report production.
- Sorting on substring of alphanumeric field now works also for space-saving sort method.
- Error in positioning of PAGE HEADING/FOOTING in reports with horizontally repeated DETAIL area is corrected.
- Key search on ISAM/SIBAS group keys is properly implemented.
- Edit the Length column in SIBAS infile description (required when e.g. redefining from INTEGER to PACKED DECIMAL).
- Defining two outfiles with same number but different names.
- Install NOTIS-RG when actual users have got more than 255 object entries.
- Run reports with sorting on terminals with terminal number > 999.
- Searching in SIBAS substream on non-unique key if owner field has parity (e.g. from WP file).
- Reflecting the TOTAL or SUBTOTAL fields in printout even if the result is erroneous.
- The function NUMBER-OF-DAYS (<date1>, <date2>) now gives negative result if <date1> > <date2>. MIN or MAX function in Named Expression with condition.

## 2 PREREQUISITES

- To read data from local SIBAS databases, NOTIS-RG version C requires SIBAS version D or later.
- To read from remote SIBAS databases, SIBAS-BACKEND version D must be installed.

000 ND Norsk Data 000

---

## Page 8

# Program Description

| Date         | Norsk Data A.S                             | Page 3 of 8 |
|--------------|--------------------------------------------|-------------|
| Product      | Name                                       | Reg. no.    | Category |
|              | NOTIS-RG for ND-100                        | 210193C     | STPR     |

## 3 Modifications

### 3.1 Changed Commands 

- PAGE command is replaced by the OUTPUT-FILES command.
- GENERAL command is replaced by the INPUT-FILES command.
- NEW-FILE command is replaced by the CREATE-LIBRARY command.
- FILES command is replaced by the LIST-LIBRARIES command.
- ENLARGE-FILE command is replaced by the ENLARGE-LIBRARY command.

### 3.2 Improved Performance

- Infile description modification is done much faster than in earlier versions.
- Reading of sequential input files (made with an editor) is faster than in earlier versions.
- The J version of ISAM is included. This gives improved performance for multiuser ISAM operation.

### 3.3 New Features

The C version of NOTIS-RG offers improved functionality, with a moderately changed user interface. The most important news are:

- Multiline text fields as input, printed as justified text columns in the report.
- Sequential (NOTIS-WP) files used as substreams.
- Several output files per report.
- Key search on ISAM and SIBAS group keys.
- Datatype conversion to any SIBAS storage format.
- Consistency check is done against SIBAS timestamp.

---

## Page 9

# Norsk Data A.S - Program Description

**Date:** 87.04.02

| Product     | Name              | Reg. no.  | Category |
|-------------|-------------------|-----------|----------|
| NOTIS-RG    | for ND-100        | 210193C   | STPR     |

### Features

- Selection, sort and control break on NAMED EXPRESSIONS.
- Calendar functions.
- More space for defining report parameters.
- Command line, message line and status line.

### Additional Information

- RECALCULATION modification is done much faster than in earlier versions.
- Reading of sequential input files (made with an editor) is faster than in earlier versions.
- If relation of TEAM is doubled, the system improves performance for multitask TEAM operation.

### Key Points

- Define text fields as input, printed in a similar way used in the report.
- Not initial (%NOTIS-W) files used as address files.
- Separate output files per report.
- Key search of I and M: Class C; Group keys.
- 6-address conversion to any %SEG% storage format.
- Consistency check is done against DIANA mainstem.

ND Norsk Data

---

## Page 10

# Installation Procedure

In SS/SD, NOTIS-RG for ND-100 is supplied on four diskettes, with directory names 210193C<rev><lang>_01S to 210193C<rev><lang>_04S. If you have ordered DS/DD diskettes, one diskette 210193C<rev><lang>_01D is supplied.

If you already have NOTIS-RG version B on your computer, you should delete this program before installing NOTIS-RG version C. The last section describes how this may be done.

The procedure to load NOTIS-RG version C is as follows:

1. Log in as user SYSTEM.

2. Make sure that the required number of pages and files is free on user SYSTEM (prerequisites, page 1).

3. NOTIS-RG should always be installed on SINTRAN user NOTIS. This user must have some free pages (see prerequisites, page 1). Make sure that the user exists, and has enough free pages! NOTE: If you want to install several language versions of NOTIS-RG, the space requirements on user NOTIS will add up.

4. Insert the diskette 210193C<rev><lang>_01D in your diskette station.

5. Enter this directory by typing:

   ```plaintext
   @ENT-DIR 210193C<rev><lang>_01 FLOPPY-D:<drive><unit>
   ```

6. Start the installation program by typing:

   ```plaintext
   @210193C<rev>FLOPPY-USER)INST-RG
   ```

The installation program will then guide you through the installation. It asks which language version(s) to install. Your answer must correspond to the diskette set(s) you have. If you have ordered both Norwegian and English version, the installation program should be used to install both language versions at the same time.

You are asked to insert the other diskettes when required.

To check that the installation was successful, you may inspect the file (SYSTEM)RG-100-DUMP-C:LOG. This file is output by the installation MODE job. Delete the file when you have looked at it.

The installation leaves a temporary file on user SYSTEM, (SYSTEM) INSTALL-RG-C:TEMP. This file should be deleted.

---

ooo...ND Norsk Data...ooo

---

## Page 11

# Program Description

| Date       | 87.04.02       | Norsk Data A.S                              |
|------------|----------------|---------------------------------------------|
| Product    |                | Page ** of **                               |
| Name       | NOTIS-RG for ND-100 | Reg. no | Category                    |
|            |                | 210193C                                     | STPR |

## 5 Re-Installation From Dump-Reentrant File

The installation program makes a mode file that should later be run to re-install NOTIS-RG. Remember to update the DUMP-REENTRANT (or HENT-MODE) file with this command:

`@MODE (NOTIS)RG-100-DUMP-C:MODE.`

## 6 Example Files

The NOTIS-RG Reference Manual contains an appendix with some examples. One example report file and two example data files are found on the last diskette. These files are useful for anyone who wants to work with the examples. Before you remove the last diskette from the diskette station, you may ENTER it and copy the three example files from this diskette using BACKUP-SYSTEM. Please inform the NOTIS-RG users where these files may be found.

## 7 The Terminal Description File

If you already have a version of the terminal description file `DDBTABLES-E:VTM`, the file `DDBTABLES-E04:VTM` will be copied to a file named: `DDBTABLES-E04:NEW` on user SYSTEM.

If you want to use this new file, please delete the old file `DDBTABLES-E:VTM`, and rename the new file by typing:

`@DELETE-FILE (SYSTEM)DDBTABLES-E:VTM`  
`@RENAME-FILE (SYSTEM)DDBTABLES-E04:NEW VTM`

---

## Page 12

# Norsk Data A.S Program Description

| Date | Page |
|------|------|
|87.04.02| 7 of 8 |

| Product | Name | Reg. no. | Category |
|---------|------|----------|----------|
| NOTIS-RG for ND-100 | 210193C | STPR |

## 8 THE ERROR MESSAGE FILE

If you already have a version of the error message file `UE-ERMSG-<lang>-B:ERR`, the file `UE-ERMSG-<lang>-B<rev>:ERR` will be copied to a file named `UE-ERMSG-<lang>-B<rev>:NEW` (on user SYSTEM).

If you want to use this new file, please delete the old file `UE-ERMSG-<lang>-B:ERR`, and rename the new file by typing:

```
@DELETE-FILE (SYSTEM)UE-ERMSG-<lang>-B:ERR″``-TECH NO)
@RENAME-FILE (SYSTEM)UE-ERMSG-<lang>-B<rev>:NEW :ERR‴
```

## 9 CHANGE ISAM INTERNAL DEVICE

The ISAM version loaded with NOTIS-RP is a multiuser ISAM, allowing ISAM files to be accessed in multiuser mode. The ISAM version loaded with NOTIS-RP will by default use internal devices 206B and 201B.

Internal devices are only used if ISAM multiuser supervisor (ISAMRT) is installed on your computer. If your ISAM multiuser supervisor uses other device numbers than 200B/201B, you must change the device number in NOTIS-RP. After NOTIS-RG/RP version is installed, you may change the device numbers this way:

1) Log in as user NOTIS.

2) Use the program LOOK-FILE to change the device number in the NOTIS-RP program file. Note that ISAM will use the device number entered, and the succeeding device number (as shown below, 202B and 203B).

   ```
   @LOOK-FILE,
   >>OPEN NOTIS:RP0:<lang>CI:PROG
   >>PATCH 139.42
   000052 ( 42)/000200 : 202B      (or another number)
   000053 ( 43)/025432 : _
   >>EXIT
   ```

3) Log in as user SYSTEM.

4) Run the mode file that dumps NOTIS-RG and NOTIS-RP:

   ```
   @MODE(NOTIS)RG-100-DUMP-C:MODE,,
   ```

---

## Page 13

# Norsk Data A.S  
PROGRAM DESCRIPTION

| Date       | Page     |
|------------|----------|
| 87.04.02   | 8 of 8 a |

| Product      | Name                | Reg. no. | Category |
|--------------|---------------------|----------|----------|
| NOTIS-RG for ND-100 |               | 210193C  | STPR     |

## 10 DELETE NOTIS-RG VERSION B

It is possible to have both version B and C of NOTIS-RG installed at the same computer. However, we recommend that you only have the last version installed.

The list below shows how to delete NOTIS-RG version B.

1. Log in as the user where you installed NOTIS-RG version B.  
   The name of this user may be found in your DUMP-REENTRANT  
   (or HENT-MODE) file (see next section).

2. Delete the files NOTIS-RG0-<lang\>-B:PROG and NOTIS-RG1-<lang\>-B:PROG

3. Delete the files NOTIS-RP0-<lang\>-B:PROG and NOTIS-RP1-<lang\>-B:PROG

4. Delete the file DUMP-RG-<lang\>-B-100:MODE

5. Log in as user SYSTEM

6. Give the following SINTRAN COMMANDS to delete NOTIS-RG and NOTIS-RP as reentrant subsystems (if no other versions are present):  
   @DELETE-REENTRANT-NOTIS-RG, <lang\><B>  
   @DELETE-REENTRANT-NOTIS-RP, <lang\><B>

7. To clear the sub-segments used by RG/RP, give the following RT-LOADER commands:  

   ```
   @RT-LOADER  
   CLEAR-SEGMENT RG1<lang\>G1B  
   CLEARING "REENTRANT_SUBSYSTEM", SEGMENT=X  
   DELETE-SEG-NAME RG1<lang\>G1B

   CLEAR-SEGMENT RP1<lang\>G1B  
   CLEARING "REENTRANT_SUBSYSTEM", SEGMENT=Y  
   DELETE-SEG-NAME=RP1<lang\>G1B   
   EXIT
   ```

```
00 SPGOS : 006000  
   00 SEEROS : 648  
   
@NOTIS-RG-NOTIS,
@DUMP-100-OR<NOTIS(R)RG)1<lang\>B.

```

---

```
Norsk Data
```

---

## Page 14

I'm sorry, the image appears to be blank or doesn't contain visible text. Can you provide more details or a different image?

---

