## Page 1

# NORD SOFTWARE LIBRARY DISKETTE

**CONTAINING:** PLANC FOR ND-100

## DIRECTORY NAME: ND-10309A  
## USER NAME: FLOPPY-USER

| File | Directory and File Name |
|------|-------------------------|
| 0    | (ND-10309A:FLOPPY-USER)PLANC-100-A:IFUN#1 |
| 1    | (ND-10309A:FLOPPY-USER)PLANC-1BANK-A:BRF#1 |
| 2    | (ND-10309A:FLOPPY-USER)PLANC-2BANK-A:BRF#1 |

24 FEBRUARY 1982

---

## Page 2

# NORSK DATA A/S - NORD SOFTWARE LIBRARY PROGRAM DESCRIPTION

## PRODUCT NAME
PLANC for ND-100

### ND-NUMBER/CATEGORY
10309A P

### ND-NUMBER FOR SOURCE
10370A

### ISSUED
DATE 82.02.19 | BY (INITIALS) GAH

### COMPUTERS
| X 10 | 12 | 50 | X 100 | 500 |

### INSTR.SET
48 BIT FL. | 32 BIT FL. | COMMERCIAL

### OP.SYSTEM
X SIN III VS | SIN III RT | ALONE

### DOCUMENTATION
TITLE: Planc Reference Manual

| NUMBER: 60.117.03 |

### PURPOSE
PLANC-100 Compiler

### PROGRAMS (FILES)
| PROG.NUMBER | NAME | TYPE CONTAINING |
|-------------|------|-----------------|
| 203452A     | PLANC-100   | BPUN PLANC-100 Compiler |
| 203453A     | PLANC-1BANK | BRF 1 BANK RUNTIME SYSTEM |
| 203454A     | PLANC-2BANK | BRF 2 BANK RUNTIME SYSTEM |

## LOADING/OPERATING PROCEDURE, USE

The PLANC compiler consists of the following files:

PLANC-100-A:BPUN SA=0 , RA=1 ; The compiler  
PLANC-1BANK-A:BRF ; Runtime system 1 bank  
PLANC-2BANK-A:BRF ; Runtime system 2 bank  

Enter the directory on the floppy and copy all the files to user SYSTEM.

Dump the compiler as a reentrant subsystem, using the SINTRAN command:

    @DUMP-REENTRANT PLANC-100,0,1,<input-file>

The PLANC-100 compiler must have a terminal background segment of 126k Words. The background segment size can be changed by the SINTRAN command:

    @CHANGE-BACKGROUND-SEGMENT-SIZE <terminal number>, 128

Note:

The PLANC-100 can not be restarted with @CONTINUE

***

*** PROGRAM DESCRIPTION ***

---

## Page 3

# NORSK DATA A/S — NORD SOFTWARE LIBRARY — PROGRAM DESCRIPTION

**PRODUCT | NAME | ND-NUMBER | CATEGORY**  
**PLANC for ND-100 | 10309A | P**  
**ND-NUMBER FOR SOURCE:**  
**10370A**

## THE FOLLOWING LIST CONTAINS A SUMMARY OF SYNTACTICAL RESTRICTIONS IN THE PLANC-100-A VERSION.

1. The list expression in the `FOR` statement cannot be an element of a `SET ARRAY`.

2. A statement containing a `MACRO` call, an `INLINE` routine call or a `$INCLUDE` command, must be terminated by a carriage return.

3. The `IND` standard routine cannot have as parameter a routine pointer which qualifies a routine with an in-value.

4. The `ON OVERFLOW` statement does not handle integer unsigned values.

5. It is impossible to export a family of routines, where the routine-name is equal to the name of a `PLANC` predefined standard routine or a `PLANC` operator.

6. No error message is given if there is any inconsistence between the routine modifiers in a routine predeclaration and the routine declaration.

7. The actual parameters of a macro cannot be enclosed in the double quote character.

8. The compiler does not check for multiple predeclarations of a variable.

9. No error message is given if the standard routine `ADDR` is used with a local-/inline routine as parameter.

10. The standard routines `MININDEX`, `MAXINDEX` and `IN` cannot be applied on the parameters within a routine with the `STANDARD` modifier. The compiler does not give any error messages.

11. The following type declaration is illegal, and the compiler does not give any error message:
   
   ```
   TYPE A=RECORD ... ENDRECORD
   TYPE B=A    /* illegal type declaration */
   ```

12. The access modifiers of a formal parameter type are not relevant when declaring routines with equal names. Example:

   - A. `ROUTINE VOID, VOID(INTEGER )`: `RUT?`
   - B. `ROUTINE VOID, VOID(INTEGER WRITE)`: `RUT?`
   
   The compiler cannot distinguish between declaration A. and B.

***PROGRAM DESCRIPTION***

---

## Page 4

# NORSK DATA A/S  
## NORD SOFTWARE LIBRARY  
### PROGRAM DESCRIPTION PAGE 3 OF 3

| PRODUCT | NAME             | ND-NUMBER | CATEGORY |
|---------|------------------|-----------|----------|
|         | PLANC for ND-100 | 10309A    | P        |
|         |                  | ND-NUMBER FOR SOURCE |
|         |                  | 10370A             |

13) The lower bound value of the integer array in the `<NEW data-DISPOSE integer array>` standard routine, must be zero if the DISPOSE standard routine is applied on the following types: INTEGER4, REAL, REAL8 and RECORD ARRAY.

14) The `$COMPILE` command and the `<source file>` must be separated with at least one space.

15) If the operators `AND`, `OR`, `XOR` or `NOT` are used in a variable initiation statement, then the entire expression must be enclosed in parentheses.

16) It is illegal to declare a family of routines with the `INLINE` modifier.

17) There is an error in example 1.1 in the PLANC Reference Manual. Substitute the first line, `MODULE mod`, with `MODULE mud`.

***PROGRAM DESCRIPTION***

---

