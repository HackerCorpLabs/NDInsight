## Page 1

# NORSK.DATA A/S  
## NORD SOFTWARE LIBRARY  
### PROGRAM DESCRIPTION  

| PRODUCT   | NAME                          | ND-NUMBER | CATEGORY |
|-----------|-------------------------------|-----------|----------|
|           | Fortran for ND-100/NORD-10    | 10191a    | P        |
|           |                               | ND-NUMBER FOR SOURCE | 10351E |

| ISSUED        | DATE 82.06.11          | BY (INITIALS) JKL |
|---------------|------------------------|-------------------|

| COMPUTERS       | X 10  | 12   | 50   | X 100 | 500   | ...   |
|-----------------|-------|------|------|-------|-------|-------|

| INSTR.SET      | 48 BIT FL. | 32 BIT FL. | COMMERCIAL |
|----------------|------------|------------|------------|

| OP.SYSTEM      | X SIN III VS! | SIN III RT! | ALONE | .......... |

| DOCUMEN-       | NUMBER: 60.145.03 |
| TATION         |
|                | TITLE: ND FORTRAN Reference Manual |

| PURPOSE        |
|                | ND-100/NORD-10 ANSI 77 FORTRAN COMPILER AND RUNTIME SYSTEM. |

| PROGRAMS (FILES) | PROG.NUMB. | NAME           | TYPE | CONTAINING                     |
|------------------|------------|----------------|------|--------------------------------|
|                  | 203053A    | FORTRAN-100    | PROG | Fortran compiler               |
|                  | 203531A    | FORTRAN-100    | BPUN | Fortran compiler               |
|                  | 203532A    | FORTRAN-1BANK  | BRF  | 1-bank runtime system          |
|                  | 203533A    | FORTRAN-2BANK  | BRF  | 2-bank runtime system          |
|                  | 203534A    | F32FORT-1BANK  | BRF  | 1-bank runtime system          |
|                  | 203535A    | F32FORT-2BANK  | BRF  | 2-bank runtime system          |

## Procedure for generating a Fortran subsystem:

- **COPY-FILE** `"FORTRAN-100-A:PROG", (ND-10191A-PART1:F-U)FORTRAN-100-A:PROG`
- **COPY-FILE** `"FORTRAN-100-A:BPUN", (ND-10191A-PART1:F-U)FORTRAN-100-A:BPUN`
- **DUMP-REENTRANT FORTRAN-100-A[,11,11], FORTRAN-100-A:BPUN`

for ND-100/NORD-10 with 48-bit floating point hardware:

- **COPY-FILE** `"FORTRAN-1BANK-A:BRF", (ND-10191A-PART2:F-U)FORTRAN-1BANK-A:BRF`
- **COPY-FILE** `"FORTRAN-2BANK-A:BRF", (ND-10191A-PART2:F-U)FORTRAN-2BANK-A:BRF`

For ND-100/NORD-10 with 32-bit floating point hardware:

- **COPY-FILE** `"FORTRAN-1BANK-A:BRF", (ND-10191A-PART3:F-U)F32FORT-1BANK-A:BRF`
- **COPY-FILE** `"FORTRAN-2BANK-A:BRF", (ND-10191A-PART3:F-U)F32FORT-2BANK-A:BRF`

The following Sintran command must be used for all the terminals from which you want to do compilations by using the new FORTRAN compiler:

- **CHANGE-BACKGROUND-SEGMENT-SIZE** `<terminal number>,128`

**NOTE:** THE FILE FORTRAN-100-A:PROG MUST BE COPIED TO THE USER "SYSTEM", AND IT MUST NOT BE DELETED AFTER THE REENTRANT SUBSYSTEM IS GENERATED.

---

## Page 2

# NORD SOFTWARE LIBRARY DISKETTE

CONTAINING: FORTRAN FOR ND-100/NORD-10

## DIRECTORY NAME

- ND-10191A-PART1

## USER NAME

- FLOPPY-USER

## FILES

| File | Path                                                                                   |
|------|----------------------------------------------------------------------------------------|
| 0    | (ND-10191A-PART1:FLOPPY-USER)FORTRAN-100-A:PROG;1                                      |
| 1    | (ND-10191A-PART1:FLOPPY-USER)FORTRAN-100-A:BPRUN;1                                     |

16 JUNE 1982

---

## Page 3

# NORD SOFTWARE LIBRARY DISKETTE

CONTAINING: FORTRAN FOR ND-100/NORD-10

## DIRECTORY NAME
ND-10191A-PART2

## USER NAME
FLOPPY-USER

## Files

| File Number | Directory/Path |
| ----------- | -------------- |
| FILE 0      | (ND-10191A-PART2:FLOPPY-USER)FORTRAN-1BANK-A:BRF;1 |
| FILE 1      | (ND-10191A-PART2:FLOPPY-USER)FORTRAN-2BANK-A:BRF;1 |

16 JUNE 1982

---

## Page 4

# NORD SOFTWARE LIBRARY DISKETTE

## CONTAINING

FORTRAN FOR ND-100/NORD-10

## Directory Information

| DIRECTORY NAME | USER NAME   |
| -------------- | ----------- |
| ND-10191A-PART3| FLOPPY-USER |

## Files

| File   | Path                                                                  |
| ------ | --------------------------------------------------------------------- |
| FILE 0 | (ND-10191A-PART3:FLOPPY-USER)F32FORT-1BANK-A:BRF;1                    |
| FILE 1 | (ND-10191A-PART3:FLOPPY-USER)F32FORT-2BANK-A:BRF;1                    |

16 JUNE 1982

---

