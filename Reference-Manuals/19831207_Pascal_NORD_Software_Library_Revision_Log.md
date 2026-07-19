## Page 1

# NORSK DATA A/S - NORD SOFTWARE LIBRARY  
## REVISION LOG

| PRODUCT   | NAME   | ND-NUMBER         |
|-----------|--------|-------------------|
|           | PASCAL | 10076J, 10133J, 10187J |

| ISSUED    | DATE 83.12.07 | BY (INITIALS) TN |
|-----------|---------------|------------------|

| REASON                  |   |   |
|-------------------------|---|---|
| x ERROR CORRECTION      | x DIFFERENT ENVIRONMENT |
| x CHANGE/ADDITION       |   |

| FILES CHANGED OR NEW FILES | PROG.NUM. | NAME              |
|----------------------------|-----------|-------------------|
| COM-2349J                  | PASCAL-COD-J:BRF | } 10076J |
| COM-2351J                  | PASCAL-LIB-J:BRF |   |
| 204615J                    | PASCAL-ZLIB-J:BRF |   |
| COM-235OJ                  | PASCAL-ERR-J:SYMB | } 10133J |
| COM-2374J                  | PASCAL-COD-J:BRF |   |
| COM-2376J                  | PASCAL-LIB-J:BRF |   |
| 204616J                    | PASCAL-ZLIB-J:BRF |   |
| COM-235OJ                  | PASCAL-ERR-J:SYMB | } 10187J |
| 203394J                    | PASCAL-COD-J:BRF |   |
| 203395J                    | PASCAL-LIB-J:BRF |   |
| COM-235OJ                  | PASCAL-ERR-J:SYMB |   |

## ND-Pascal. Version J.

With this release of ND-Pascal a new revision of the user manual, ND-60.124.5, has been produced. The manual covers both ND-100 Pascal and ND-500 Pascal.

### Corrected errors

All known errors in version I have been corrected. Some of these were of a minor nature, and are not reported below.

1. **ND-500:** Character assignment failed when option C+ was in effect.

2. **ND-100:** In some cases the compiler generated erroneous code re-using already occupied stack space.

3. The maximum legal length of a source code line is now 96 characters both for the ND-100 and ND-500 compilers.

4. **ND-500:** FILE OF REAL did not work correctly.

5. A program would hang at program end when the same file was CONNECTed several times without intervening DISCONNECTs.

6. The call PAGE; did not work correctly.

---

## Page 2

# Revision Log

| NORSK DATA A/S | NORD SOFTWARE LIBRARY | PAGE 2 OF 3 |
|---------------|------------------------|------------|
| PRODUCT       | NAME                   | ND-NUMBER  |
|               | PASCAL                 | 10076J,10133J,10187J |

### Revision Notes

7. **ND-500:** The compiler in some cases generated erroneous code for Boolean expressions.

8. An error in the CONNECT procedure which occurred when CONNECTing a logical unit has been corrected.

9. **ND-100:** A problem with )LIB marks when generating overlay modules has been corrected.

10. The procedure CONNECT's buffer holding the SINTRAN file name has been extended to 60 characters.

11. **ND-500:** A program initially gave EOLN = TRUE even though the program call had parameters.

12. **ND-500:** Byte (character) comparisons erroneously were signed.

13. An error in the "make set range" run-time routine has been corrected.

14. Both the values 15 octal and 215 octal will give EOLN = TRUE when option C+ is in effect.

15. It was not possible to transfer a conformant array parameter to a non-Pascal routine.

16. **ND-100:** The interface to PLANC-routines with or without INITSTACK now works correctly. The PLANC-routine has to be defined as STANDARD in the Pascal program.

### Extensions and Changes

1. The compilers now accept hexadecimal integer constants. The syntax is:

```
<hex constant> ::= <sign> <digit> ⊗ <hexdigit> å <size> "H"
<sign> ::= <empty> ⊗ "+" ⊗ "-"
<hexdigit> ::= <digit> ⊗ "A" ⊗ "B" ⊗ "C" ⊗ "D" ⊗ "E" ⊗ "F"
<size> ::= <empty> ⊗ "L"
```

---

## Page 3

# NORD SOFTWARE LIBRARY REVISION LOG

| PRODUCT | NAME  | ND-NUMBER            |
|---------|-------|----------------------|
|         | PASCAL| 10076J,10133J,10187J |

## 2. ND-100

The stack format of a Pascal procedure or function object has been changed to conform with the standard for PLANC and other ND-100 languages. An object now has six system locations in the header instead of three, as follows:

- return address
- dynamic link
- not used
- not used
- static link
- not used

Note: This change makes it necessary to modify all assembly routines called from a Pascal program. The function value, or first parameter, will now be at relative location 6 to (A)+(B), instead of previously at relative location 3.

## 3. ND-100

The compiler will now produce two-bank BRF code when option B2 is in effect. The new library, PASCAL-2LIB:BRF, must be loaded to produce an executable two-bank program.

It is still possible to make two-bank programs as it was done previously, i.e. by entering the loader command DEFINE NOBKS 2 and loading PASCAL-LIB, but the new method saves space in the instruction bank.

When loading two-bank code to create a system of overlaid modules, one must enter the NRL command:

```
SET-MODE DATA
```

before the first OVERLAY-GENERATION command.

Refer to the new ND-Pascal Reference Manual for further details on two-bank programs.

---

