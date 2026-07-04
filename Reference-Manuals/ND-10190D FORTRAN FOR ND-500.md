## Page 1

# NORD Software Library Diskette

**Containing:** FORTRAN for ND-500

## Directory Information

**Directory Name:** ND-1019200-PART1  
**User Name:** FLOPPY-USER

## File List

| File Number | Description |
|-------------|-------------|
| FILE 0 | ND-1019200-PART1:FLOPPY-USER:DESCRIPTION-FILE;DESC;1 |
| FILE 1 | ND-1019200-PART1:FLOPPY-USER:SCRATCH-SEQ-011INTRO;1 |
| FILE 2 | ND-1019200-PART1:FLOPPY-USER:SCRATCH-SEQ-0110SEQ;1 |
| FILE 3 | ND-1019200-PART1:FLOPPY-USER:SCRATCH-SEQ-011PSEQ;1 |
| FILE 4 | ND-1019200-PART1:FLOPPY-USER:FORTRAN-500:INTRO;1 |
| FILE 5 | ND-1019200-PART1:FLOPPY-USER:FORTRAN-500:SEQ;1 |
| FILE 6 | ND-1019200-PART1:FLOPPY-USER:FORTRAN-500:PSEQ;1 |

18 March 1982

---

## Page 2

# NORD SOFTWARE LIBRARY DISKETTE

**CONTAINING**: FORTRAN FOR ND-500

**DIRECTORY NAME**: ND-10190D-PART2  
**USER NAME**: FLOPPY-USER

**FILE 0**: (ND-10190D-PART2:FLOPPY-USER)FORTRAN-LIB-0:NRF:1

18 MARCH 1982

---

## Page 3

# NORSK DATA A/S NORD SOFTWARE LIBRARY

## PROGRAM DESCRIPTION

### PRODUCT

| NAME | ND-NUMBER | CATEGORY |
|------|-----------|----------|
| Fortran for ND-500 | 10190D | P |

| ND-NUMBER FOR SOURCE |
|----------------------|
| 10351D |

### ISSUED

| DATE 82.03.11 | BY (INITIALS) JKL |
|---------------|-------------------|

### COMPUTERS

| 10 | 12 | 50 | X 100 | X 500 | ... |
|----|----|----|-------|-------|-----|

### INSTR.SET

| 48 BIT FL. | 32 BIT FL. | COMMERCIAL |
|------------|------------|------------|

### OP.SYSTEM

| SIN III VS | SIN III RT | ALONE |
|------------|------------|-------|

### DOCUMENTATION

| NUMBER: 60.145.03 |
|-------------------|
| TITLE: ND FORTRAN Reference Manual |

### PURPOSE

ND-500 ANSI 77 FORTRAN COMPILER AND RUNTIME SYSTEM.

### PROGRAMS (FILES)

| PROG.NUMBER | NAME | TYPE CONTAINING |
|-------------|------|-----------------|
| 203054D | FORTRAN-500 | Fortran compiler |
| 203101D | FORTRAN-LIB | NRF Fortran runtime system |

Procedure for generating the Fortran runtime system and compiler:

```
@COPY-FILE "FORTRAN-LIB-D:NRF", (ND-10190D-PART2:FL-U)FORTRAN-LIB-D:NRF
@ND LINKAGE-LOADER
ND-Linkage-Loader - C
N1l: DELETE-AUTO-LOAD-FILE (Deletes all auto-load files)
N1l: DELETE-AUTO-LINK-SEGMENT (Deletes all auto-link segments)
N1l: SET-DOMAIN "FORTRAN-LIB-D"
N1l: SET-SEGMENT-NUMBER 30D
N1l: OPEN-SEGMENT "FORTRAN-LIB-D",
N1l: SET-IO-BUFFERS 20B
N1l: LOCAL-TRAP-DISABLE ALL
N1l: ENTRY-ROUTINES 500B
N1l: TOTAL-SEGMENT-LOAD FORTRAN-LIB-D
N1l: END-DOMAIN
N1l: SET-AUTO-LINK-SEGMENT FORTRAN-LIB-D,FORTRAN
N1l: SET-AUTO-LOAD-FILE FORTRAN-LIB-D,FORTRAN
N1l: EXIT
@ 
@ND LINKAGE-LOADER
ND-Linkage-Loader - C
N1l: ABORT-BATCH-ON-ERROR OFF
N1l: DELETE-DOMAIN FORTRAN-500
N1l: COPY-DOMAIN "FORTRAN-500-D", (ND-10190D-PART1:FL-U)FORTRAN-500-D
N1l: EXIT
@

The Fortran compiler can be started as following:
@ND FORTRAN-500-D
```

---

## Page 4

# NORSK DATA A/S NORD SOFTWARE LIBRARY

## REVISION LOG

| PRODUCT | NAME             | ND-NUMBER |
|---------|------------------|-----------|
|         | Fortran for ND-500 | 10190D   |

| ISSUED  | DATE 82.03.11 | BY (INITIALS) JKL |

| REASON | X ERROR CORRECTION | DIFFERENT ENVIRONMENT |
|--------|----------------------|-----------------------|
|        | X CHANGE/ADDITION    | ...                   |

| FILES   | PROG.NUMB. NAME  |
|---------|------------------|
| CHANGED | 203054D FORTRAN-500 |
| OR NEW  | 203101D FORTRAN-LIB |
| FILES   |                   |

### 1. ERRORS CORRECTED

#### 1.1

Some errors concerning the debug information generated for multi-dimensional arrays and labels have been corrected.

#### 1.2

The LEN function will now return the value zero if the string has null length.

#### 1.3

A character substring expression, where the last index is less than the first index, will now get the length zero.

#### 1.4

The following examples sometimes caused the compiler to terminate the compilation with the error message 'PROTECT VIOLATION'. These errors have been corrected.

a) An array subscript expression of the type INTEGER*2.

b) Syntax errors in EQUIVALENCE, DIMENSION or multiple assignment statements.

c) An array subscript expression, an intrinsic function argument or a logical expression giving the error message 'CANNOT CONVERT'.

d) A unary operation on an invariant variable in a loop.

    DO FOR K=1,100  
    L=-I+K  
    ENDFOR  

e) If an EXTERNAL symbol without any parameters occurred on the left side of an assignment statement.

#### 1.5

An error concerning conversion of the arguments of intrinsic functions has been corrected.

#### 1.6

Incorrect code was sometimes produced for an expression containing variables present in an EQUIVALENCE statement. The following example will now work correctly:

---

## Page 5

# NORSK DATA A/S

## NORD SOFTWARE LIBRARY

**PRODUCT** | **NAME** | **ND-NUMBER**
---|---|---
| Fortran for ND-500 | 10190D |

```
INTEGER ARR(10)
LOGICAL LARR(10)
EQUIVALENCE (LARR,ARR)
ARR(K) = ARR(J)
LARR(K) = LARR(I) .AND. LARR(N)
ARR(N) = ARR(K)
```

### Correction Notes

1.7 The exception handling routines GETMESS, PGETMESS, PRIMESS, EXCEPT, and EXCDEF will now work correctly.

1.8 The monitor call routines INCH, OUTCH, ISIZE and OSIZE have been corrected according to the ND FORTRAN Reference Manual.

1.9 An error concerning the handling of consecutive "**" or '""' (quote or asterisk) characters in a FORMAT has been corrected.

1.10 An error concerning non-default handling of the error codes 401B-457B in the EXCEPTION HANDLING SYSTEM has been corrected.

1.11 An error concerning SEQUENTIAL unformatted Input/Output, using the RECL specifier in the OPEN statement, has been corrected.

1.12 A case when .99 was written as 0.99 or **** has been corrected.

1.13 An error causing the error message PROTECT VIOLATION to occur when using READ/WRITE to an internal file has been corrected.

1.14 An error concerning the handling of the error code 457B has been corrected.

1.15 An error concerning DIRECT files, leading to the destruction of records in the last page of the file has been corrected.

1.16 A case of incorrect skipping of records in FORMATTED READ statements has been corrected.

1.17 An error concerning the use of UNIT=1 has been corrected.

However, it is not recommended to use unit number 1 for files other than the terminal, as this may lead to problems in the handling of error output from the FORTRAN I/O system.

1.18 An error prohibiting any type of I/O in the user EXCEPTION-handlers has been corrected.

**NOTE**: If the error is of I/O-type (i.e., 401B-430B) the handler must not use the FORTRAN I/O statements (READ, WRITE, REWIND, etc.) but may call monitor subroutines directly, for instance to write a message on a terminal. If the error is of non-I/O type, the handler may use the FORTRAN I/O statements, but the utmost care must be taken to avoid new errors while inside the exception handler.

---

## Page 6

# NORSK DATA A/S NORD SOFTWARE LIBRARY

| PRODUCT | NAME              | ND-NUMBER |
|---------|-------------------|-----------|
|         | Fortran for ND-500| 10190D    |

## 1.20
An error resulting in leading zeros and wrong size of the exponent field when writing scaled E-formatted numbers has been corrected.

## 1.21
The monitor call subroutine ERMSG has been corrected, allowing the use of ERRCODE as parameter.

## 1.22
The statement READ (1,*) will now skip one record.

# 2. CHANGES

## 2.1
If a special micro program is present on the computer, the compiler will now generate instructions rather than library function calls for the following intrinsic functions: SIN, ASIN, COS, ACOS, TAN, ATAN, ATAN2, EXP, ALOG, ALOG2 and ALOG10.

## 2.2
The Z-format has been implemented for hexadecimal Input/Output.

## 2.3
List directed formatted output will now signal DOUBLE PRECISION numbers by a "D" in front of the exponent value.

## 2.4
SCRATCH as status to the OPEN statement is now permitted with the constraints mentioned in the ND FORTRAN Reference Manual version 60.145.03.

## 2.5
The traps FLOATING OVERFLOW and PROGRAMMED TRAP are now default enabled when running FORTRAN programs.

---

