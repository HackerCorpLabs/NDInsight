## Page 1

# NORD SOFTWARE LIBRARY DISKETTE

## CONTAINING 

FORTRAN  
32 BITS FLOATING FORMAT.  

## DIRECTORY NAME 

ND-10033K

## USER NAME 

FLOPPY-USER

| FILE | PATH |
|------|------|
| FILE 0 | (ND-10033K:FLOPPY-USER)PRINT-FILE-2324F:BFUN;1 |
| FILE 1 | (ND-10033K:FLOPPY-USER)FTN-20901:BFUN;1 |
| FILE 2 | (ND-10033K:FLOPPY-USER)FTNLIBR-2093F:BRF;1 |
| FILE 3 | (ND-10033K:FLOPPY-USER)FTNLTLIBR-2094F:BRF;1 |

29 SEPTEMBER 1981

---

## Page 2

# NORD SOFTWARE LIBRARY

## PROGRAM DESCRIPTION

| PROGRAM  | NAME          | PROGRAM NUMBER | PART OF           |
|----------|---------------|----------------|-------------------|
|          | PRINT-FILE    | SUT-2324B      | NO NUMBER 10023<br>H, I, J, K 10033<br>H, I, J, K |

| COMPUTERS     | INSTRUCTION SET  | OPERATING SYSTEMS | PROGRAM TYPE | DOCUMENTATION |
|---------------|------------------|-------------------|--------------|---------------|
| X 10          | X 48 BIT FL.     | X SIN III VS      | X BINARY     | NUMBER        |
| □ 12          | X 32 BIT FL.     | □ SIN III RT      | □ BRF        |               |
| □ 50          | □ COMMERCIAL     | □ ALONE           | □ SYMBOLIC   | TITLE         |
| X 100         |                  |                   |              |               |
| □ 500         |                  |                   |              |               |

| ADDRESS SPACE (For Binary Programs) | START ADDRESS | RESTART ADDRESS |
|-------------------------------------|---------------|-----------------|
| BOUNDARIES 13400 < 34040            | 13665         | 13665           |

**PURPOSE**

PRINT A FORMATTED FILE CREATED BY A FTN-PROG.

### LOADING/OPERATING PROCEDURE

@PLACE <input file>  
@DUMP "PRINT-FILE", 13665; 13665

This program prints a formatted file created by a FORTRAN program on a specified list device. The control characters in FORTRAN are expanded (1H1, 1H+, 1H$ etc.)

---

## Page 3

# NORD SOFTWARE LIBRARY  
## REVISION LOG  

| PROGRAM | NAME      | PROGRAM NUMBER |  
|---------|-----------|----------------|  
| PRINT-FILE |           | SUI-2324B      |  

| PART OF NO NUMBER |  
|-------------------|  
| 10023G,H,I,J,K 10033H,I,J |

| ISSUED        | DATE 79.08.17 | BY (INITIALS) |  
|---------------|--------------|---------------|  
|               |              | BR            |  

| REASON                   | |  
|--------------------------|-|  
| [ ] ERROR CORRECTION     | |  
| [ ] DIFFERENT ENVIRONMENT| |  
| [X] CHANGE/ADDITION      | |  

## CHANGES  

The speed of printing is improved by not printing trailing spaces.

---

## Page 4

# NORSK DATA A/S NORD SOFTWARE LIBRARY

## PROGRAM DESCRIPTION

| PROGRAM        | NAME                 | PROGRAM NUMBER |
|----------------|----------------------|----------------|
|                |                      | FTN-20901      |
|                | FTN                  |                |
|                |                      | PART OF ND-NUMBER |
|                |                      | 10023K, 10033K |

| COMPUTERS      | X 10  | . 12  | . 50  | X 100 | . 500!  | ...!   |
|----------------|-------|-------|-------|-------|---------|--------|

| INSTR.SET      | . 48 BIT FL. | . 32 BIT FL. | . COMMERCIAL |
|----------------|--------------|--------------|--------------|

| OP.SYSTEM      | X SIN III VS | . SIN III RT | . ALONE      | ...      |
|----------------|--------------|--------------|--------------|----------|

| PROG.TYPE      | X BINARY     | . BRF        | . SYMBOLIC   | ...      |
|----------------|--------------|--------------|--------------|----------|

| DOCUMEN-       | NUMBER: ND-60.074.03                                 |
| TATION         | TITLE: NORD-10/100 FORTRAN SYSTEM REFERENCE MANUAL   |

| ADDRESS SPACE  | BOUNDARIES: 0<76370+tables!                          |
| (BIN.PROG)     | START ADDRESS: 0   |   RESTART ADDRESS: 1            |

| PURPOSE        | NORD 10/100 FORTRAN COMPILER                         |

---

LOADING/OPERATING PROCEDURE, USE

Procedure for generating a standard subsystem:

    @PLACE-BINARY <input file>
    @DUMP "FTN-20901",0,1

Procedure for generating a reentrant subsystem:

    @DUMP-REENTRANT FTN-20901,0,1,<input file>

The number of lines printed on each page on the line-printer may be changed by the following patch in the compiler:

    333/67 <No. of lines>

or by the compiler command

    PAGE-SIZE <No. of lines>

A run-time message, if floating division by zero, may be obtained by the following patch in the compiler:

    240/0 10000

---

## Page 5

# NORSK DATA A/S NORD SOFTWARE LIBRARY

## REVISION LOG

| PRODUCT   | NAME        | ND-NUMBER  |
|-----------|-------------|------------|
|           | NORD FORTRAN| 10023K, 10033K |

| ISSUED    | DATE 81.09.14 | BY (INITIALS) JKL |
|-----------|--------------|-------------------|

| REASON              | X ERROR CORRECTION | DIFFERENT ENVIRONMENT |
|---------------------|--------------------|-----------------------|
|                     | CHANGE/ADDITION    |                       |

| FILES   | PROG.NUMBER NAME |
|---------|------------------|
| CHANGED | FTN-2090I FTN    |
| OR NEW  |                  |
| FILES   |                  |

### 1. ERRORS CORRECTED

#### 1.1

The compiler command DIRECT-ADDRESSED-CALLS did not work correctly when subprogram calls contained either subscripted parameters or expressions.

#### 1.2

The compiler command PROFILE-MAP destroyed the compiler's memory area. This error has been corrected.

#### 1.3

The logical shift function ISHFT did not work correctly when the parameters were of different types. This error has been corrected.

#### 1.4

In program units greater than 100000B, character strings in DATA statements were sometimes destroyed. This error has been corrected.

---

## Page 6

# NORSK DATA A/S - NORD SOFTWARE LIBRARY

## PROGRAM DESCRIPTION

### PROGRAM
| NAME     | PROGRAM NUMBER |
|----------|----------------|
| FTNLIBR  | FTN-2093F      |

| PART OF ND-NUMBER |
|-------------------|
| 100331,J,K        |
| 10136B            |

### COMPUTERS
| X 10 | 12 | 50 | X 100 | 500 | ... |

### INSTR.SET
| 48 BIT FL. | X 32 BIT FL. | COMMERCIAL |

### OP.SYSTEM
| X SIN III VS | SIN III RT | ALONE |

### PROG.TYPE
| BINARY | X BRF | SYMBOLIC |

### DOCUMENTATION
| NUMBER: ND60.074.03/ ND60.050.08 |
| TITLE: NORD-10/100 FORTRAN SYSTEM Reference Manual and SINTRAN III User's Guide |

### ADDRESS SPACE
| BOUNDARIES: .......<....... |
| START ADDRESS: ....... |
| RESTART ADDRESS: ....... |

### PURPOSE
NORD-10/100 FORTRAN LIBRARY/RUNTIME SYSTEM

---

LOADING/OPERATING PROCEDURE, USE

Procedure for generating a BRF file:

@COPY "FTNLIBR-2093F:BRF",<input file>

---

## Page 7

# NORSK DATA A/S NORD SOFTWARE LIBRARY

## PROGRAM DESCRIPTION

| PROGRAM   | NAME        | PROGRAM NUMBER  |
|-----------|-------------|-----------------|
|           | FTNRTLIBR   | FTN-2094F       |
|           |             | PART OF ND-NUMBER |
|           |             | 100331,J,K      |
|           |             | 10136B          |

| COMPUTERS | X 10 | 12  | 50 | X 100 | 500 | ... |
|-----------|------|-----|----|-------|-----|-----|
| INSTR.SET | 48 BIT FL. | X 32 BIT FL. | - COMMERCIAL |

| OP.SYSTEM | X SIN III VS | - SIN III RT | - ALONE | ... |

| PROG.TYPE | - BINARY     | X BRF       | - SYMBOLIC | ... |

| DOCUMENTATION | NUMBER: ND60.074.03/ ND60.050.08 |
|               | TITLE: NORD-10/100 FORTRAN SYSTEM Reference Manual and |
|               | SINTRAN III User's Guide |

| ADDRESS SPACE (BIN.PROG) | BOUNDARIES: ......<...... |
|                          | START ADDRESS: ......     |
|                          | RESTART ADDRESS: ......  |

| PURPOSE | NORD-10/100 FORTRAN REENTRANT LIBRARY/RUNTIME SYSTEM |

## LOADING/OPERATING PROCEDURE

Procedure for generating a BRF file:

`@COPY "FTNRTLIBR-2094F:BRF",<input file>`

---

## Page 8

# NORSK DATA A/S NORD SOFTWARE LIBRARY

### REVISION LOG

| PROGRAM       | NAME               | PROGRAM NUMBER                |
|---------------|--------------------|-------------------------------|
|               |                    | FTN-2091F, FTN-2092F          |
|               | FTNLIBR/FTNRTLIBR  | FTN-2093F, FTN-2094F          |

| PART OF ND-NUMBER                 |
|-----------------------------------|
| 100231,J,K 100331,J,K             |
| 10136B 100672                    |

| ISSUED       | DATE 80.11.26     | BY (INITIALS) JKL             |

| REASON                |                            |
|-----------------------|----------------------------|
| X ERROR CORRECTION    | DIFFERENT ENVIRONMENT      |
| X CHANGE/ADDITION     |                            |

## CHANGES

### 1. ERRORS CORRECTED

1.1 The routine 8CLSB will not be doubly defined if the OPEN statement is performed only on an overlay.

1.2 An error concerning REWIND for buffered I/O is corrected.

1.3 When reading from a terminal with the READ statement, the control will be transferred to the error label if the terminal is not opened for read.

1.4 Underflow and overflow in the double precision real multiplication routine will be handled correctly in the versions FTN-2093F and FTN-2094F.

1.5 Some minor errors are corrected.

### 2. CHANGES MADE

2.1 The OPEN statement will accept the access codes SEQUENTIAL, DIRECT and SPECIAL.

The access codes SEQUENTIAL and DIRECT should only be used for the I/O which employ READ/WRITE statements. For files, line-printers and magnetic tapes the I/O will be buffered, if a buffer is made available with the loader command SET-IO-BUFFERS. If a buffer is not available, the unit will be opened for sequential access.

The access code SPECIAL should only be used for I/O using the RFILE/WFILE and MAGTP file utility subprograms. This access code will function similarly to the access code D.

2.2 When using buffered I/O the READ/WRITE statements containing the REC- specifier will accept a record number in the range of 0 to 65535.

---

## Page 9

# NORSK DATA A/S NORD SOFTWARE LIBRARY

## PROGRAM DETAILS

| PROGRAM        | NAME              | PROGRAM NUMBER                     |
|----------------|-------------------|------------------------------------|
|                | FTNLIBR/FTNRTLIBR | FTN-2091F, FTN-2092F               |
|                |                   | FTN-2093F, FTN-2094F               |
|                |                   | PART OF ND-NUMBER                  |
|                |                   | 100231J, K100331J, K               |
|                |                   | 10136B 10067D                      |

| ISSUED         | DATE 80.11.26     | BY (INITIALS) JKL                  |

## Updates and Changes

### 2.3
The following new routines for overlay handling are implemented in the versions FTN-2091F and FTN-2093F:

OVLINIT, OVERLAY and OVRECAL.

**NOTE:** The NRL-version LDR-1935G or later must be used.

### 2.4
The following SINTRAN III monitor calls are implemented:

PASET, PAGET, EDTRM and RERRP.

### 2.5
A fourth parameter is implemented in the BRKM routine if the strategy is greater than 2. The parameter is the maximum number of characters input before break is given.

### 2.6
The old OPEN routine will now accept the access code 8.

### 2.7
The DEBUG command processor will accept SINTRAN commands starting with commercial at (@).

### 2.8
Some minor changes.

---

