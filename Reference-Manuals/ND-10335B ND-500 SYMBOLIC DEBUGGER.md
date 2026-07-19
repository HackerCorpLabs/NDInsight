## Page 1

# NORD Software Library Diskette

**Containing:**  
ND-500 Symbolic Debugger

**Directory Name:**  
ND-10335E

**User Name:**  
FLOPPY-USER

**File 0:**  
(ND-10335E:FLOPPY-USER)DEBUGGER-E:NRF$1

22 February 1982

---

## Page 2

# NORD SOFTWARE LIBRARY  
## PROGRAM DESCRIPTION  

| PRODUCT   | NAME                           | ND-NUMBER | CATEGORY |
|-----------|--------------------------------|-----------|----------|
|           | ND-500 Symbolic Debugger       | 10335B    | P        |
|           |                                | ND-NUMBER FOR SOURCE   |
|           |                                | 10348B    |          |

| ISSUED    | DATE 82.02.10 | BY (INITIALS) Jensen |
|-----------|---------------|-----------------------|

| COMPUTERS | 10   | 12   | 50  | 100 | X 500  | ...  |
|-----------|------|------|-----|-----|--------|------|

| TNSTR.SET | 48 BIT FL. | 32 BIT FL. | COMMERCIAL |
|-----------|------------|------------|------------|

| OP.SYSTEM | X SIN III VS | SIN III RT | ALONE     | ... |
|-----------|--------------|------------|-----------|-----|

| DOCUMENTATION | NUMBER: 60.158.01  |
|---------------|---------------------|
|               | TITLE: Symbolic Debugger Reference Manual |

| PURPOSE                                        |
|------------------------------------------------|
| Symbolic Debugger for FORTRAN, PLANC and COBOL.|

| PROGRAMS (FILES) | PROG.NUMBER  | NAME    | TYPE CONTAINING               |
|------------------|--------------|---------|----------------------------------|
|                  | 203318B      | DEBUGGER| NRF ND-500 Symbolic Debugger |

## LOADING/OPERATING PROCEDURE, USE

To copy the debugger from the floppy the following command sequence can be used:

```
@ENTER-DIRECTORY ND-10335 <Floppy disk name and unit>
@COPY-FILE "DEBUGGER-B:NRF" (ND-10335: FLOPPY-USER)DEBUGGER:NRF
@RELEASE-DIRECTORY ND-10335
```

The debugger must be loaded together with the program to be debugged. The user's program may consist of several segments. The debugger may be loaded on any of these segments or on a separate segment. Since the debugger uses routines in the FORTRAN library this must always be loaded (not linked). The debugger must be loaded prior to the FORTRAN library. The LINKAGE-LOADER command TOTAL-SEGMENT-LOAD must be used. 

The following part of the loading procedure is language dependent:

- **FORTRAN**  
  No special restrictions.

- **PLANC**  
  The debugger must be explicitly called from the PLANC program. The entry point is imported as follows:
  ```
  IMPORT (ROUTINE VOID, VOID: DEBUG)
  ```
  The module calling the debugger need not be compiled in DEBUG-MODE.

- **COBOL**  
  The main program must be compiled in DEBUG-MODE.

Modules written in different languages may be mixed as long as the above conditions are satisfied.

---

## Page 3

# NORSK DATA A/S NORD SOFTWARE LIBRARY

## PRODUCT NAME

| PRODUCT  | NAME                     | ND-NUMBER |
|----------|--------------------------|-----------|
|          | ND-500 Symbolic Debugger | 10335B    |

In order to allow write access to the program the special monitor command DEBUG-PLACE should be used instead of the normal PLACE. If PLACE is used some debugger commands will respond with the message: ATTEMPT TO MODIFY READ-ONLY SEGMENT.

When started, the debugger asks for the names of the segments used by the user's program. All segments should be specified whether or not they contain modules compiled in DEBUG-MODE.

### Example

```
@ND-500-MONITOR
N500: DEBUG-PLACE MY-DOMAIN
N500: RUN
SEGMENT NUMBER(S) OF SEGMENT(S) WITH DEBUG-INFORMATION: 8
NAME OF SEGMENT 8D: MY-DOMAIN
ND-500 SYMBOLIC DEBUGGER - 203318B. FEBRUARY 10, 1982.
PLANC PROGRAM. Starting scope
*
```

If several segments with debug-information are used, the current scope may be moved to a specified segment by using the SCOPE command. E.g.: SCOPE .19 XROUT moves the current scope to the routine XROUT on segment 19. Both initially and when a breakpoint is encountered, the scope is set automatically.

---

## Page 4

# NORSK DATA A/S - NORD SOFTWARE LIBRARY

## REVISION LOG

| PRODUCT         | NAME                  | ND-NUMBER |
|-----------------|-----------------------|-----------|
| ND-500 Symbolic Debugger | | 10335B   |

| ISSUED         | DATE 82.02.10         | BY (INITIALS) Jensen |
|----------------|-----------------------|----------------------|

| REASON                      |                           |
|-----------------------------|---------------------------|
| X ERROR CORRECTION          | DIFFERENT ENVIRONMENT     |
| X CHANGE/ADDITION           |                           |

| FILES          | PROG.NUMBER NAME     |
|----------------|----------------------|
| CHANGED        | 203318B DEBUGGER     |
| OR NEW FILES   |                      |

## CHANGES

1. This version of the debugger requires ND-500-MONITOR Version B or later.

2. Real (floating point) constants are implemented. The operators +, -, *, / and ** can be used with real operands. The exponent for ** must be of integer type.

3. Hexadecimal and binary constants are implemented. A hexadecimal constant must start with a decimal digit (0-9) and is followed by the letter H. A binary constant is followed by the letter X. E.g. 0F9EH, 100110X.

4. Printing of arrays is improved. Printing does not stop after one line but after approximately one page (20 lines). Also, two dimensional FORTRAN arrays are not transposed.

5. The INVOKE command is improved. Normal PLANC routines may be invoked (STANDARD not needed). Pointer, Integer, enumeration, boolean and record may be used as actual parameters to normal PLANC routines.

6. In LOOK-AT-STACK, addresses used with the slash command are taken as relative to the B-register in the stack frame currently being examined.

7. The following notation can be for addresses: segment'address. E.g. 16'1233B means address 1233B on segment 16B.

8. Several minor errors corrected.

---

