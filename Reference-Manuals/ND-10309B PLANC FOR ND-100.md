## Page 1

# NORD Software Library Diskette

## Containing: PLANC for ND-100

### Directory Information

- **Directory Name**: ND-10309B
- **User Name**: FLOPPY-USER

### Files

| File Number | Path | Description |
|-------------|------|-------------|
| File 0 | (ND-10309B:FLOPPY-USER)PLANC-100-B:BPLUN:1 |  |
| File 1 | (ND-10309B:FLOPPY-USER)PLANC-1BANK-B:BRF:1 |  |
| File 2 | (ND-10309B:FLOPPY-USER)PLANC-2BANK-B:BRF:1 |  |

### Date

27 June 1982

---

## Page 2

# NORD SOFTWARE LIBRARY PROGRAM DESCRIPTION

| PRODUCT | NAME                | ND-NUMBER | CATEGORY |
|---------|---------------------|-----------|----------|
|         | PLANC for ND-100    | 10309B    | P        |
|         |                     | ND-NUMBER FOR SOURCE |
|         |                     | 10370B    |          |

| ISSUED     | DATE 82.06.15 | BY (INITIALS) GAH |
|------------|---------------|-------------------|

| COMPUTERS  | X 10  | 12 | 50 | X 100 | 500! |
|------------|-------|----|----|-------|------|

| INSTR.SET  | 48 BIT FL. | 32 BIT FL. | COMMERCIAL |
|------------|------------|------------|------------|

| OP.SYSTEM  | X SIN III VS! | SIN III RT! | ALONE |
|------------|---------------|-------------|-------|

| DOCUMENTATION | TITLE: Planc Reference Manual | NUMBER: 60.117.03 |
|---------------|-------------------------------|-------------------|

| PURPOSE |
|---------|
| PLANC-100 Compiler |

| PROGRAMS (FILES) | PROG.NUM. | NAME        | TYPE | CONTAINING              |
|------------------|-----------|-------------|------|-------------------------|
|                  | 203452B   | PLANC-100   | BPUN | PLANC-100 Compiler      |
|                  | 203453B   | PLANC-1BANK | BRF  | 1 BANK RUNTIME SYSTEM   |
|                  | 203454B   | PLANC-2BANK | BRF  | 2 BANK RUNTIME SYSTEM   |

## LOADING/OPERATING PROCEDURE, USE

The PLANC compiler consists of the following files:

```
PLANC-100-B:BPUN  SA=0 , RA=1  ; The compiler
PLANC-1BANK-B:BRF ; Runtime system 1 bank
PLANC-2BANK-B:BRF ; Runtime system 2 bank
```

Enter the directory on the floppy and copy all the files to user SYSTEM.

Dump the compiler as a reentrant subsystem, using the SINTRAN command:

```
@DUMP-REENTRANT PLANC-100,0,1,<input-file>
```

The PLANC-100 compiler must have a terminal background segment of .128k Words. The background segment size can be changed by the SINTRAN command:

```
@CHANGE-BACKGROUND-SEGMENT-SIZE <terminal number> , 128
```

Note:

The PLANC-100 cannot be restarted with @CONTINUE

*** PROGRAM DESCRIPTION ***

---

## Page 3

# NORSK DATA A/S
## NORD SOFTWARE LIBRARY

### PROGRAM DESCRIPTION

| PRODUCT          | NAME              | ND-NUMBER | CATEGORY |
|------------------|-------------------|-----------|----------|
|                  | PLANC for ND-100  | 10309B    | P        |
|                  |                   | ND-NUMBER FOR SOURCE |
|                  |                   | 10370B    |          |

**THE FOLLOWING LIST CONTAINS A SUMMARY OF SYNTACTICAL RESTRICTIONS IN THE PLANC-100-B VERSION.**

1. A statement containing a MACRO call, an INLINE routine call or a $INCLUDE command, must be terminated by carriage return.
2. The IND standard routine can not have as parameter a routine pointer which qualifies a routine with an in-value.
3. The argument to the standard routine ADDR can not be enclosed in parentheses if it is a routine pointer.
4. The standard routine ADDR can not have as argument a routine with an outvalue.
5. The standard routines MININDEX, MAXINDEX and IN can not be applied on the parameters within a routine with the STANDARD modifier. The compiler does not give any error messages.
6. The ON OVERFLOW statement does not handle integer unsigned values.
7. It is impossible to export a family of routines, where the routine-name is equal to the name of a PLANC predefined standard routine or a PLANC operator.
8. The following type declaration is illegal, and the compiler does not give any error message:

```
TYPE A=RECORD ......ENDRECORD
TYPE B=A     % illegal type declaration.
```

9. The access modifiers of a formal parameter type are not relevant when declaring routines with equal names. Example:

```
A. ROUTINE VOID,VOID(INTEGER          ): RUT?
B. ROUTINE VOID,VOID(INTEGER WRITE): RUT?
```

   The compiler can not distinguish between declaration A. and B.

10. The $COMPILE command and the <source file> must be separated with at least one space.

***PROGRAM DESCRIPTION***

---

## Page 4

# NORSK DATA A/S NORD SOFTWARE LIBRARY

## REVISION LOG

| PRODUCT | NAME | ND-NUMBER |
|---------|------|-----------|
| PLANC for ND-100 | | 10309B |

| ISSUED | DATE 82.06.03 | BY (INITIALS) GAH |

| REASON | x. ERROR CORRECTION | . DIFFERENT ENVIRONMENT |
|--------|----------------------|-------------------------|
|        | x CHANGE/ADDITION    |                         |

| FILES | PROG. NUMB. NAME | TYPE |
|-------|------------------|------|
| CHANGED | 203452B PLANC-100 | BPUN |
| OR NEW | 203453B PLANC-1BANK | BRF |
| FILES | 203454B PLANC-2BANK | BRF |

## 1. Improvements

- Exponential REAL8\*\*INTEGER is implemented.

## 2. New Compiler Commands

- `$OPTION ARRAY-INDEX-CHECK <ON/OFF>`  
  If the option is ON, legal array accesses will be checked upon compile-/run-time. The option might be turned ON/OFF anywhere in the source program.

- `$EJECT`  
  A form-feed will be written to the list-device.

*** REVISION LOG ***

---

## Page 5

# NORSK DATA A/S

## NORD SOFTWARE LIBRARY
### REVISION LOG
**PAGE 2 OF 2**

| PRODUCT          | NAME               | ND-NUMBER |
|------------------|--------------------|-----------|
| PLANC for ND-100 |                    | 10309B    |

## 3. Errors Corrected

1. Errors concerning `FOR <xx> IN <element of a set array>` are corrected.

2. Inconsistence between a variable predeclaration and a variable declaration is detected, and a warning message is given.

3. Actual macro parameters can be enclosed in double quote characters according to the definition in the PLANC REFERENCE MANUAL.

4. Errors corrected concerning `NEW <type> IN <integer array>`.

5. Errors corrected concerning initiation of variables using the operators AND, OR, XOR and NOT.

6. Errors corrected concerning INLINE routines.

7. If the standard routine MAXINDEX or MININDEX is used in a PLANC routine with the STANDARD modifier, an error message is given.

8. Errors corrected concerning BYTE stored into a multiple number of BYTES.

9. Errors corrected concerning `Recordarray(index).INTEGER4-INTEGER4=<xx>`.

10. Equivalence with the invalue of a routine is made possible.

11. Errors corrected concerning stack damage if a routine has a composite outvalue and the number of parameters > 2.

12. Errors corrected concerning the standard routine `<unsigned 16 bits value> MOD <xx>`.

13. If the standard routine ADDR is applied on a LOCAL/INLINE routine, an error message is given.

14. Some minor errors are corrected.

***REVISION LOG***

---

