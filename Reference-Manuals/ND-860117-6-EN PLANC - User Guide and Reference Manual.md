## Page 1

# PLANC

### User Guide and Reference Manual

**ND-860117.6 EN**

```
   _    ___         \ /
  /_\  / __|  _ __   #   _   _
 / _ \| (_| || '_ \ #  .#.#.#.
 /_/ \_\\___/| .__/  #   #####
             |_|     |   ####
                     |  
```

**Norsk Data**

*Scanned by Jonny Oddene for Sintran Data © 2021*

---

## Page 2

I'm sorry, but the page seems to be unreadable. Let me know if there is anything else I can help with.

---

## Page 3

```
PLANC
User Guide and
Reference Manual
ND-860117.6 EN
```

---

## Page 4

# Note

The numbering system for Norsk Data's documentation changed in September 1988. All numbers now start with an 8. The numbering structure is therefore ND-8xxxxx.xx xx. Example: ND-863018.3A EN. Existing manuals will receive a new number if and when they are updated or revised.

The information in this manual is subject to change without notice. Norsk Data A.S assumes no responsibility for any errors that may appear in this manual, or for the use or reliability of its software on equipment that is not furnished or supported by Norsk Data A.S.

| Copyright 1989 by Norsk Data A.S | Version 6    | June 1989    |
|---------------------------------|-------------|--------------|
|                                 | Version 2    | September 1987 |

Previous version was named PLANC Reference Manual

Send all documentation requests to:

Norsk Data A.S  
Graphic Centre  
P.O. Box 25 - Bogerud

---

## Page 5

# Table of Contents

## PREFACE
1

## About PLANC and this manual
3
- What you need to know first
  4
- Notation
  4
- Terminology
  4

## PLANC User Guide
9
- Getting used to reading PLANC
  10
  - M1 - Your first PLANC program
    10
  - Comments
    12
  - M2 - Presenting PLANC
    13
  - Comments
    17
- Control Structures
  19
  - M3 - Control structures
    20
  - Comments
    24

## Declarations
27
- M4 - Declaring simple variables
  27
  - Comments
    32
- M5 - Type expressions
  34
  - Comments
    35
- M6 - Arrays
  36
- M7 - Sets
  41
- M8 - Records and dynamic allocation of variables
  43
  - Comments
    50

## Expressions
52
- M9 - Expressions
  53

## Using routines
61
- M10 - Using routines
  62
- M11 - Routine modifiers
  68
- M12 - Routine pointers
  73
- M13 - Routines in records and object-oriented programming
  75
- M14 - Co-routines
  81
- M15 - Advanced co-routines
  89

## Modules
92
- M16 - Modules
  92

## The compiler's command processor
96
- M17 - The command processor
  96

## Implementation details
101
- OS-specific implementation details
  101
  - Avoiding massive recompilation under SINTRAN
    101
  - Select demo
    103
- Packing of composite-data types
  106

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 6

# Data/runtime organization on the ND-500(0)

## General Topics
- Types ........................................................................................... 111
- Type checking ........................................................................... 111
  - Initial values of global variables ............................................. 112
  - Routines and operators in executable statements .................. 113
- Implicit type conversion .......................................................... 116
- Operators and standard routines .............................................. 116
- Stacks .......................................................................................... 124
- Parameter transfer .................................................................... 126
- Portable programming in PLANC ........................................... 126

# PLANC Reference
- `(end-of-line - statement delimiter)` .............................................. 131
- `% (% %%) (comment delimiters)` ............................................... 132
- `" (macro parameter delimiter)` ................................................... 132
- `# (get ASCII value of byte)` .......................................................... 133
- `$ (line shift in Output statements)` ............................................ 133
- `& (continuation of statement line)` ............................................ 133
- `' (byte string delimiter)` ............................................................... 133
- `* (multiplication)` - `9` ................................................................. 134
- `S* (inline assembly follows)` ...................................................... 134
- `** (exponentiation)` - `11` ........................................................... 135
- `+ (addition)` - `8` .......................................................................... 136
- `++ (command processor value increment)` ............................... 136
- `++ (increment variable)` - `10` ..................................................... 136
- `, (list item separator)` - `6` ........................................................... 137
- `- (subtraction)` - `8` .................................................................... 137
- `- (change sign)` - `10` ................................................................. 137
- `-- (command processor value decrement)` ............................... 137
- `-- (decrement)` - `10` ................................................................. 138
- `. (record component access)` - `13` .......................................... 138
- `/ (division)` - `9` .......................................................................... 138
- `// (string concatenation)` - `8` .................................................... 139
- `: (data declaration indicator)` ....................................................... 139
- `: (range indicator)` - `7` ............................................................. 140
- `:= (variable initialization)` ........................................................... 141
- `:=: (variable value swap)` - `1 and 12` ....................................... 141
- `; (statement and routine parameter separator)` ...................... 142
- `< (less than)` - `6` ......................................................................... 142
- `<= (less than or equal)` - `6` ....................................................... 143
- `= (equal)` - `6` ................................................................................ 143
- `= (address equivalence)` ............................................................. 144
- `=: (store value in variable)` - `1 and 12` .................................... 144

---

## Page 7

# Technical Reference

| Symbol/Name                         | Description                                                      | Page |
|------------------------------------|------------------------------------------------------------------|------|
| \>                                 | (greater than)                                                   | 145  |
| ><                                 | (unequal)                                                        | 145  |
| \>=                                | (greater than or equal)                                          | 145  |
| ?                                  | (predeclaration)                                                | 146  |
| @                                  | (in-value name in routine)                                       | 146  |
| Abs                                | (absolute value) 11                                              | 146  |
| Ada notation                       |                                                                  | 146  |
| Addr                               | (pointer to) 14                                                  | 147  |
| ALIAS                              | (loader symbol redefinition)                                     | 147  |
| AND                                | (intersection) 3                                                 | 147  |
| Append                             | (append to pointer-implied list) 5                               | 148  |
| ARRAY                              | (composite type constructor)                                     | 148  |
| %ARRAY-INDEX-CHECK                 | (compiler check of array bounds)                                 | 150  |
| ASSERT                             | (test for exception in program)                                  | 150  |
| Bit                                | (read/set bit) 11                                                | 150  |
| BITS                               | (bit array)                                                      | 151  |
| Bit_position                       | (position of record component) 11                                | 151  |
| Bit_size                           | (size of variable/component) 11                                  | 151  |
| Blocksize                          | (blocksize of file) 11                                           | 152  |
| BOOLEAN                            | (simple type)                                                    | 152  |
| BOOLEAN1                           | (Boolean subtype)                                                | 152  |
| BOOLEAN2                           | (Boolean subtype)                                                | 153  |
| %BOOLEAN2-ENUMERATION2             | (make 16-bit variables)                                          | 153  |
| BYTE                               | (simple type)                                                    | 153  |
| BYTES                              | (byte string)                                                    | 153  |
| C                                  | (interface to C code)                                            | 154  |
| $CALL-HIERARCHY                    | (routine call-hierarchy listing)                                 | 155  |
| CASE                               | (branching statement)                                            | 155  |
| Close                              | (closing files) 11                                               | 156  |
| COBOL                              | (interface to COBOL)                                             | 156  |
| COMMON                             | (importing FORTRAN COMMON)                                       | 157  |
| $COMPILE                           | (compile source file)                                            | 157  |
| co_Call                            | (let co-routine proceed) 11                                      | 158  |
| co_Detach                          | (suspend co-routine) 11                                          | 159  |
| co_Resume                          | (suspend one, resume another co-routine) 11                      | 160  |
| CONSTANT                           | (declare a value to the compiler)                                | 160  |
| $CONSTANT                          | (declare constant for subsequent compiles)                       | 161  |
| CONVERT                            | (change type of variable) 11                                     | 161  |
| $CPU-EXTENSION                     | (get CPU version)                                                | 162  |
| $CROSS-REFERENCE                   | (source is cross-referenced)                                     | 163  |
| $DATE                              | (get current date into byte string)                              | 163  |
| $DEBUG-MODE                        | (compiler generates debug information)                           | 164  |
| $DEFINE                            | (for direct load on ND-100)                                      | 164  |
| Dispose                            | (release dynamically allocated memory) 11                        | 164  |
| DO                                 | (loop statement)                                                 | 165  |
| DOMAIN                             | (ND internal routine modifier)                                   | 166  |
| $EJECT                             | (new page in listing)                                            | 166  |
| $ELSE                              | (conditional compilation)                                        | 166  |

---

## Page 8

# Table of Contents

- ELSE (conditional statement clause) ......................................................... 166
- SELSIF (conditional compilation) ............................................................... 166
- ELSIF (conditional statement clause) ......................................................... 167
- ENDCASE (end of branching block) .......................................................... 167
- ENDDO (end of loop) .................................................................................... 167
- ENDFOR (end of loop) ................................................................................. 167
- $ENDIF (end of conditional compilation) ...................................................... 167
- ENDIF (end of conditional statement) ......................................................... 167
- $ENDMACRO (end of compiler macro) ......................................................... 168
- ENDMODULE (end of source module) ......................................................... 168
- ENDON (end of exception-handler) ............................................................. 168
- ENDRECORD (end of record declaration) ................................................... 168
- ENDROUTINE (end of routine declaration) ................................................... 168
- ENDUSING (end of using block) ................................................................... 169
- ENUMERATION (user-defined value range) .............................................. 169
- $EOF (end of source file) ............................................................................. 169
- ERRCODE (for error identification) ............................................................. 170
- ERRETURN (used if error occurred in routine) ......................................... 170
- SEXIT (from compiler) ................................................................................ 170
- EXITFOR (execute before leaving for loop) .............................................. 170
- EXITWHILE (execute before leaving loop) ............................................... 171
- $EXPAND-MACROS (expand on listing) .................................................... 171
- EXPORT (make variable known outside module) .................................... 171
- FALSE (Boolean value) ................................................................................ 172
- Filesize (get or set size of file) .................................................................. 172
- FOR (loop) .................................................................................................. 172
- FORCE (re-interpret type of variable) ........................................................ 174
- FORTRAN (interface to FORTRAN on PCs) .............................................. 175
- $GENERATE-IMPORTS (make IMPORT list from EXPORTs) ................ 175
- SGET-VALUE (of a compiler command) ................................................... 175
- GO (unconditional jump) ........................................................................... 176
- $HELP (compiler command list) ............................................................... 176
- %HELP (compiler option list) ..................................................................... 176
- $HINTS (about avoidable trouble) ............................................................. 176
- SIF (conditional compilation) ..................................................................... 177
- IF (conditional execution) .......................................................................... 178
- IMPORT (get variables/routines from other modules) ............................ 178
- IN (test for membership in set or range) .................................................. 180
- IN (list part of for loop) .............................................................................. 180
- IN (indicate array in dynamic memory allocation) ................................. 181
- INCASE (options in CASE statements) ..................................................... 181
- $INCLUDE (include source file into current compilation) ....................... 181
- $INCLUDE-PLANC (include according to byte string) ............................. 181
- Ind (de-reference, get value of variable pointed to) ............................... 182
- Inistack (make new program stack) ......................................................... 182
- INLINE (routine modifier) ......................................................................... 183
- Input (from file or terminal) ...................................................................... 184
- Insert (variable into list or value into set) ................................................ 185

---

## Page 9

# Technical Documentation

## Index

| Topic                                          | Page |
|------------------------------------------------|------|
| INTEGER (simple type)                          | 186  |
| INTEGER1 (subtype of INTEGER)                  | 186  |
| INTEGER2 (subtype of INTEGER)                  | 187  |
| INTEGER4 (subtype of INTEGER)                  | 187  |
| SKILL (remove from compiler's symbol list)     | 187  |
| LABEL (for GO statements)                      | 187  |
| $LIBRARY-MODE (make library)                   | 188  |
| $LINE-BIAS (adjust line number on listing)     | 188  |
| $LNK-TO (for multisegment load on ND-100)      | 188  |
| $LINKAGE-REFERENCE (IMPORT/EXPORT cross reference) | 189  |
| $LIST (generate listing)                       | 189  |
| $LOAD (direct load on ND-100)                  | 189  |
| $LONG-NAMES (switch between 16- and 10-byte name length) | 190  |
| $MACRO (declare a compiler macro)              | 190  |
| MAINSTART (main routines accessing UNIX/DOS command line) | 192  |
| Maxindex (get highest index in array)          | 192  |
| $MESSAGE-PLANC (output a byte string during compilation) | 193  |
| $MESSAGE-TO-TERMINAL (output message while compiling) | 193  |
| Minindex (get smallest index in array)         | 193  |
| MOD (get modulo of integers)                   | 193  |
| MODULE (start of a PLANC module)               | 194  |
| $MODULE-LIBRARY-MODE (make library from single modules) | 196  |
| Monitor_call (do SINTRAN III monitor call)     | 196  |
| NATIVE (same routine modifier as C)            | 197  |
| SND100-EXTENDED (use extended instruction set) | 197  |
| New (dynamically allocate new variable)        | 197  |
| NIL (special pointer value)                    | 198  |
| NOT (negation)                                 | 198  |
| %OBLIST (output the code generated in disassembly) | 199  |
| SOBLIST (output the code generated in disassembly) | 199  |
| ON (start of exception-handler)                | 199  |
| Open (a file)                                  | 201  |
| $OPTION (to set compiler options)              | 201  |
| OR (union of BOOLEANS or SETs)                 | 202  |
| Output (write to terminal or file)             | 202  |
| OVERFLOW (exception condition)                 | 203  |
| $OVERLAY (making overlays on the ND-100)       | 204  |
| PACK (make composite variables take less space) | 204  |
| PACKED (make composite variables take less space) | 204  |
| PARALLEL (routine modifier for co-routines)    | 205  |
| PASCAL (interface to Pascal routines)          | 206  |
| POINTER (declare pointer to a type)            | 206  |
| POINTERERROR (exception condition)             | 207  |
| PRECISION (of REALs)                           | 208  |
| Pred (get previous enumeration value)          | 208  |
| $PRESENT (symbol in compiler's symbol table)   | 208  |
| PRIORITY (adjust priority of a routine)        | 208  |
| $PROG-FILE (for direct loading on ND-100)      | 209  |

---

## Page 10

# Contents

| Term                           | Description                                                   | Page |
|-------------------------------|---------------------------------------------------------------|------|
| PROGRAM                       | (routine contains main entry point)                           | 209  |
| PUBLIC                        | (make components known outside record)                        | 210  |
| RANGE                         | (set allowed value range for INTEGERS)                        | 210  |
| RANGEERROR                    | (exception if value is not allowed)                           | 211  |
| READ                          | (variable is read-only)                                       | 211  |
| REAL                          | (simple type)                                                 | 212  |
| $REAL-PRECISION               | (set number of valid digits for REALs)                        | 213  |
| REAL8                         | (long real subtype)                                           | 214  |
| RECORD                        | (to make variables of a record type)                          | 214  |
| REFERENCE                     | (routine modifier)                                            | 215  |
| Remove                        | (remove record from list or value from set) - 5               | 216  |
| RETURN                        | (leave routine) - 1                                           | 216  |
| REVERSE                       | (apply to ranges in FOR loops)                                | 217  |
| ROUTINE                       | (declaration of routine)                                      | 217  |
| ROUTINEERROR                  | (exception condition)                                         | 220  |
| $SELECT                       | (for fast reload)                                             | 220  |
| $SEPARATE-DATA                | (two-bank on ND-100)                                          | 221  |
| $SEPARATE-DATA                | (dummy command)                                               | 222  |
| SET                           | (to construct a variable of a SET type)                       | 222  |
| SHIFT                         | (bits in an INTEGER) - 8                                      | 223  |
| Size                          | (of data type in bytes) - 11                                  | 223  |
| SPECIAL                       | (for making dangerous routines)                               | 223  |
| $SPLIT-CODE                   | (80286 segment handling)                                      | 224  |
| %$SQUEEZE                     | (compact ND-1x0, MC680x0 and 80386 code)                      | 224  |
| STACKERROR                    | (exception condition)                                         | 224  |
| STANDARD                      | (interface to COBOL and FORTRAN routines)                     | 224  |
| Succ                          | (get next value in enumeration range) - 11                    | 225  |
| SYSTEM                        | (IMPORT modifier)                                             | 225  |
| $TARGET-MACHINE               | (get CPU type)                                                | 225  |
| $THEN                         | (conditional compilation)                                     | 225  |
| THEN                          | (part of conditional statement)                               | 226  |
| TRUE                          | (Boolean value)                                               | 226  |
| TYPE                          | (to make new variable types from old)                         | 226  |
| Typeof                        | (make new variable of known type) - 11                        | 227  |
| UNSIGNED                      | (integer cannot take negative values)                         | 227  |
| USING                         | (avoid excessive dot notation)                                | 227  |
| $VERSION-INFORMATION          | (get info about PLANC)                                        | 229  |
| VOID                          | (special type)                                                | 230  |
| WHILE                         | (to leave loops)                                              | 230  |
| WRITE                         | (variable can be changed)                                     | 230  |
| XARGS                         | (routine can have variable number of parameters)              | 231  |
| XOR                           | (mutually exclusive OR) - 2                                   | 232  |
| $XREF                         | (make cross-references)                                       | 232  |
| -                             | (underscore)                                                  | 232  |

---

## Page 11

## Appendix A

plc, the compiler frontend for UNIX systems ........... 233

## INDEX

................................................................................................. 239

---

## Page 12

```
Scanned by Jonny Oddene for Sintran Data © 2021
```

---

## Page 13

# PREFACE

## The product

This manual explains the use of Norsk Data's PLANC compilers, which have these ND product numbers:

```
210309  (ND-100 code, executing on ND-100)
211037  (ND-100 code, executing on ND-500(0))
210310  (ND-500(0) code, executing on ND-500(0))
210491  (MC680x0 code, executing on ND-100)
211038  (MC680x0 code, executing on ND-500(0))
250298  (Intel 80286 code, executing on ND-500(0))
```

PLANC is a structured programming language. It is primarily used for development on computers and operating systems that are offered by Norsk Data A.S (ND).

## The reader

This manual is essential reading for all PLANC programmers.

## Prerequisite knowledge

The reader should have previous experience with a structured language, such as Pascal or C, before reading this manual, as the introductory part assumes previous programming experience in such a language.

## This manual

This manual is a completely rewritten version of the previous PLANC manual (ND-60.117.5).

The first chapter contains a user guide, intended to introduce PLANC to programmers with experience in structured programming. Most of the instruction is given as commented code in executable programs. These programs are available on SINTRAN files, so you can compile, load and execute them, and preferably inspect them while executing with a debugger.

The second chapter gives details that are intended for more advanced programmers, explaining some of the internals of the language and giving some CPU- and OS-specific information. The final chapter is the reference part of the manual. It contains descriptions of all keywords and compiler commands/options in alphabetical order. Details about the `plc` compiler frontend for UNIX systems is found in an appendix and in the `man` pages of your computer.

## Related manuals

More information about PLANC is found in the manuals PLANC for Intel Microprocessors (ND-20.012), PLANC Utility Library and PLANC-GEN (ND-60.297) and ND-Specific Programming & Advanced PLANC (ND-20.034).

---

## Page 14

is largely superceded by this manual, but it contains useful information about program optimization on ND computers.)

If you want to use the tutorial examples in this manual to learn PLANC, it is also recommended that you use ND's *Language Editor (LED)* and *Symbolic Debugger*. The related manuals are *LED User Guide, ND-860266* and *Symbolic Debugger User Guide, ND-860158*. These programs are also most useful for general programming!

Those who program for SINTRAN will need the *SINTRAN III Monitor Calls Guide, ND-860228*, the *ND Linker User Guide & Reference Manual, ND-860289* and the *BRF Linker User Guide, ND-860196*.

---

## Page 15

# About PLANC and this Manual

## PLANC is Block-Structured

PLANC is a block-structured programming language in the ALGOL tradition, and thus it is related to languages such as C, Pascal, SIMULA, Ada, and so on. Since no set of I/O primitives would be generally sufficient, it does not have very advanced I/O facilities - your needs will be covered by dedicated libraries like the PLANC utilities, VTM, NDP and FOCUS. It is rich in facilities for structuring the code in a way conducive to a good programming style.

## Where PLANC is Used

PLANC is a high-level systems programming language, developed by Norsk Data A.S. for use in its own development department in the late 1970s. It has been used for diverse programming tasks ranging from office automation projects to operating system development.

## PLANC Availability

PLANC is a living language. For example, it has recently been extended so records may contain subroutines, making it more of an object-oriented language, and other programming features have been added. It has been implemented on a variety of CPUs (ND-100, ND-500(0), INTEL 80x86, Motorola 680x0) and operating systems (SINTRAN, NDIX, XENIX, Sun UNIX, MS/DOS, OS/2).

## Utilities and Interfaces to Other Languages

A set of utility routines called **PLANC Utility Library** comes with the PLANC compiler. The utilities provide solutions to frequently recurring programmer tasks such as converting numbers to strings, doing I/O on screen, generating random numbers, efficient file I/O etc., while PLANC-GEN is a program that uses the utilities to generate simple screen dialogues. Furthermore, PLANC has interfaces to most other languages in the ND environment, so, for example, C and FORTRAN libraries may be used as well.

## Editor

ND's **Language Editor (LED)** is a suitable editor for working with PLANC. The LED can indent your source code, pretty-print it, set up windows where you can run compilers for syntax checks, and more.

## Debugger

The PLANC compilers generate debug information for use with ND's **Source Debugger** or the MS/DOS **SYMDEB** debugger. The Source Debugger can run in a LED window, making the source code very available during debugging. It is a good idea to use one of these debuggers in your work.

---

## Page 16

# What you need to know first

**The reader is assumed to know a block-structured language**

This manual is written for programmers who have learned a block-structured programming language such as C or Pascal already. The introductory part of the manual supplies very little information on fundamental principles. It builds on your previous programming knowledge and shows how conditional statements, loops, routines and so on are programmed in PLANC. However, to prepare you for reading this material, it first shows the features of PLANC that differ from other such languages.

# Notation

| Ordinary, italicized and bold text | The font used here (known as Times Roman) is the font that will be used throughout this manual. The letters will be *italicized* when discussing non-terminals and when emphasis is needed. **Bold** is used in the note margin and in case of warnings. |
|------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|

| Courier fonts for tokens | Varieties of the `Courier` font will be used in programming examples as well as in the text to refer to tokens that occur in the examples. `Standard Courier` will be used for comments and strings, `Courier Italics` for names, operators and so on, and **Bold Courier Italics** for keywords, standard routines and compiler commands. |
|--------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|

# Terminology

**Block**  
A *block* consists of a sequence of *statements* enclosed by appropriate *keywords*. The statements can either be *declaration statements* or *executable statements*. For example, the keywords `ROUTINE` and `ENDROUTINE` enclose a block consisting of the declaration statements and executable statements of a routine. In Pascal, the blocks are enclosed by `begin` and `end`, while in C, the special characters `{` and `}` enclose the blocks.

**Character**  
All printable ASCII characters except `'`, `{`, `|`, `}`, and `~` can be used in PLANC programs. However, particular kinds of *tokens* may be made up of limited subsets of this character set.

---

## Page 17

# Composite Type

Composite types allow the user to define complex data structures from the simple types, which are the basic building blocks. Composite types can be used in new composite declarations after they have been declared or predeclared.

# Constant

Names can be associated with values by constant declarations. When the name is used, the compiler replaces it with the value.

# Declaration Statement

Declaration statements are used to associate constant names with values, to compose types and to introduce into the program variables of the types that are already available.

# Digit

The characters 0 ... 9, plus the letters A ... F and a ... f in hexadecimal numbers.

# Executable Statement

Executable statements change the initial state of the program/routine's memory and registers. The initial state of a program/routine is determined by its declarations of variables, which in the case of global declarations include initial values assigned to the variables.

When no ambiguity can arise, the word statement in this manual means an executable statement.

# Intermediate Result

An intermediate result is an instance of one of the types known to the compiler that is passed on to the rest of the expression where it occurs.

In the statement a + b =: a, there are two intermediate results. The first intermediate result contains the sum of a and b. This intermediate result is passed on as an operand to the store (=: ) operator. The store operator in turn has the value that was stored to a as the intermediate result. This second result could be used if more calculations were included in this expression, but is unused in the example above. (The store operator will transfer the value of its lefthand side, which is an intermediate result in this case, to one of the program's storage locations.)

# Keyword

Keywords are tokens with special meanings to the compiler. Examples are the IF ... ELSIF ... ELSE ... ENDIF keywords that are used in conditional statements. They are always made from letters, digits and underscores (_).

# Letter

Letters mean normal upper- and lowercase characters.

---

## Page 18

# Technical Page

## Literal

A **literal** is an integer (including octal numbers, numbers written in the Ada notation and ASCII numbers such as #A), real, Boolean or string constant, and the special pointer value NIL. They are typically used to assign values to constants and to give values to variables in the declaration/initialization part of programs or during program execution.

## Name

Whenever a **constant**, **type**, or **variable** is declared, it is given a **name** by which it is known in the program.

## Non-terminals

In syntax descriptions of computer languages, **non-terminals** denote symbols that are used in the **productions** that generate syntactically correct programs, but which do not occur in the final source code. (The two synonymous words **terminals** and **tokens** denote the keywords, operators, variable names, etc. that occur in the source code.)

## Production

A **production** describes the relationship between a **non-terminal** symbol and the **tokens** (terminals) that it can be expanded into.

In the PLANC syntax, the non-terminal symbol `return_statement` produces return statements as follows:

```
return_statement ➝ RETURN
                 | anytype_expression RETURN
                 | integer_expression ERRETURN
```

Here, ➝ denotes a production, RETURN and ERRETURN are tokens while **expression** is another non-terminal, and | denotes a choice between alternatives.

## Overload

When tokens that represent operators and routines (including standard routines) are defined more than once, they are said to be **overloaded**. For example, in addition to the compiler's own definition of the token `+`, you can declare more routines and call them `+`. This is useful if you want to define add operations for types you have defined yourself. (The compiler consults the declaration of the overloaded routines to identify the appropriate routine for the current context.)

## Predeclaration

It is possible to make variable names known to the compiler before they have been declared using **predeclarations**. Such predeclarations are sometimes necessary, since the compiler is strictly one-pass; that is, whenever it reads a statement, it has no knowledge of the contents of later statements.

## Predefined routine

See standard routine.

## Semantics

Every token that is passed to the code-generator part of the compiler has an effect on the behavior of the final program. The **semantics** of a token is the description of this effect. For instance, when `ADDR` is passed to the...

---

## Page 19

# Simple Type

The *simple types* are the basic building blocks of declarations. They include integers, real numbers, enumerations, and Booleans.

# Special Character

*Characters* with special meanings to the compiler. Examples are the percent sign (`%`), which is used to denote comments, and the question mark (`?`), which is used to indicate predeclarations.

# Standard Routine

These are routines that are known to the compiler. Examples are `MONITOR_CALL`, which is used to execute monitor calls, `ADDR` which finds the address of variables, and `BIT_SIZE` which gives the size of variables in bits. Also called *predefined* routines.

# Statement

As seen from the parser's point of view, all tokens belong to *statements*, of which there are two kinds: *declaration statements* and *executable statements*. Normally and when no ambiguity can arise, declaration statements are called *declarations* while the word *statement* denotes an executable statement.

# Syntax

The syntax is the rule for forming strings of tokens that are correct PLANC. Syntactic correctness is a prerequisite if the application of the *semantics* of the token string is to yield a meaningful program (but in no way ensures that the program will be meaningful!). A syntactically correct `IF ... THEN ... ENDIF` statement is necessary if the code generated by application of the semantics of the individual tokens in the if-statement are to make a meaningful conditional statement.

# Token

*Tokens* are what programs ultimately consist of. Tokens are built from one or more non-blank, printable ASCII characters. Examples of tokens are keywords, operators, string literals, and names for types, variables, and routines. It is synonymous with the *standard symbols* of previous PLANC manuals.

# Type

All *variables*, literals, and constants have a *type*. PLANC itself recognizes simple types such as `INTEGER`, `BOOLEAN`, and `REAL` and subtypes thereof. Further types are made using the type constructors `POINTER`, `ENUMERATION`, `SET`, `RECORD`, `ARRAY`, and `ROUTINE`. The two first constructors yield new simple variables while the rest yields complex variables.

# Type Checking

To check that a type is appropriate in the context where it is used. For example, only types/subtypes of the `INTEGER` and `ENUMERATION` types can be used as `ARRAY` indexes, only variables of types that agree with the declaration of a `ROUTINE` can be used when the routine is called, and binary operators can only be applied to variables of the same type/subtype.

# Type Constructor

All keywords used in declarations (including *type expressions*) to make

---

## Page 20

# Type Constructors

New types, `POINTER`, `ENUMERATION`, `SET`, `RECORD`, `ARRAY`, and `ROUTINE`, are called type constructors.

## Type Expression

A *type expression* (or *type specification* in previous PLANC manuals) builds a new type from existing ones using existing types, modifiers, and type constructors, and associates a name with the new type. To make a new type for use in later declarations in PLANC, write `TYPE valid_PLANC_name =` before the type expression.

## Value

All *variables* contain a *value*. A value can be given to the variable by initialization if it is a global variable or a local `READ` variable, or stored into it by executable statements. The value of an uninitialized global variable is zero, while the value of a local variable is undefined if it has not had a value stored into it (or is declared as `READ` only in a routine on the outermost module level).

## Variable

An area of one or more bits in the program's data part where *values* can be stored and retrieved, and which has got a *name* in a *declaration statement*.

---

## Page 21

# PLANC User Guide

## About this chapter

This chapter will teach you to read and make PLANC code.

## The first two sections

The first subsection will introduce you to the language features that make PLANC programs look unfamiliar at first sight.

## The rest builds on these basics and your previous knowledge

When you have a grasp of these basics, you will be shown how the common ingredients in block-structured languages have been implemented in PLANC. There will be sections about:

- Control structures
- Declaration of simple and composite variables
- Statements
- Routines
- Pointers and indirect routine call
- Object-oriented programming

## Learn by examples

The manual contains extensive and complete code examples that can be compiled into executable programs. Much information is given as comments inside the examples. There are three reasons for this approach:

- It is desirable that the examples should answer as many questions as possible about how and where to use the PLANC features that are demonstrated.
- Much of your work will consist in reading PLANC code - you may as well get used to it early.
- We hope the manual will promote good and uniform programming habits among PLANC users, thus making it easier for you to read other people's code.

## Use tools while learning PLANC

It is common practice in textbook examples of code to include numerous I/O statements which print results during execution of the programs. These I/O statements do not contribute to the readability of the examples, and can be made superfluous if you use a good debugger (such as the ND Symbolic Debugger) to observe the state of the programs during execution. It is also a good idea to use LED, as it ...

---

## Page 22

# Getting Used to Reading PLANC

For the most part, PLANC is a conventional block-structured language. However, it has a few features that set it apart from other languages such as C and Pascal and which will obscure the more familiar features of the language until you know more about them. This section contains a couple of profusely commented programs that demonstrate those features.

## M1 - Your First PLANC Program

### Introduction

This subsection contains a simple program. It shows how all statements are contained in a block delimited by the keywords `MODULE` and `END-MODULE`. You will also notice PLANC's explicit stack initiation, how to use the `PROGRAM` declaration, how PLANC assignments are written, and a little about type checking.

### Before You Start Reading the Example

What you need to know before you read the following code is that end-of-line also denotes end-of-statement, and that the percent sign, `{%}`, marks the beginning of a comment that ends at the end of the line. To give emphasis to the comments in important module and routine declarations, they have been placed inside LED header frames. When using LED, you make such frames by pressing the F4 key on the TDV keyboard. There are percentage signs at the end of some comments too, but they are there for cosmetic purposes only. The points demonstrated by the example are summarized immediately after it.

---

## Page 23

# M 1

Your first PLANC module.

## MODULE m1

The following INTEGER ARRAY will contain the stack space of the program. It will be initialized as such in the main program. A program may have any number of stacks, although you rarely need more than one.

```
INTEGER ARRAY : StackSpace (0:255)
```

## Main

The keyword PROGRAM tells the compiler that what follows is a routine in which program execution will start.

### PROGRAM : Main

These are declarations of a couple of variables; one is an integer which will occupy two bytes of memory, the next is a real number.

```
INTEGER : i
REAL : r
```

Before the program can run, it needs a stack for storing stackframes, parameters and intermediate results during execution. The StackSpace array is prepared for that use in the next declaration.

```
INITSTACK StackSpace
```

This is the first executable statement in the program.

In PLANC, the operator `=:` is used to store values into data elements during evaluation of an expression. The store operator works from left to right: The result of the expression on the left is stored into the variable on the right.

In the following statement, the value 7 is stored into the variable `i`.

```
7 =: i
```

There is type checking in PLANC. For instance, only values that are INTEGER can be stored into variables declared as INTEGER. If it is necessary to store an INTEGER value into a REAL variable, it must be converted to REAL before it can be stored, as follows:

```
(i * i) CONVERT REAL =: r
```

The keyword ENDROUTINE ends the PROGRAM.

---

## Page 24

# PROGRAM Declaration

- In the comment to the PROGRAM declaration, we said that the keyword PROGRAM designates the start of the routine where program execution will begin. Both PROGRAMs and all other routine declarations end with the ENDROUTINE statement. Just to make it clear which routine ends where, you can write the name of the routine after its ENDROUTINE statement.

```
ENDROUTINE Main
```

- Just like ENDROUTINE ends routines, ENDMODULE ends modules.

```
ENDMODULE
```

- The next line contains a compiler command that signals the end of the current file. This command is not strictly necessary, only useful. If it were not used, you would have to type EXIT to the compiler to leave it, but the next line stops the compiler for you. (If this file were included as part of another file, the compiler would resume compilation of the other file instead of stopping.)

```
$EOF
```

# Comments

Most PLANC code comes in modules. (Only CONSTANT, TYPE and macro declarations plus compiler commands are allowed outside modules, as we shall see later.)

Variables (and routines) are declared by first writing their type followed by a colon (:), followed by the name of the variable(s). In the example, INTEGER ARRAY, INTEGER2 and REAL were used to declare the StackSpace, i and r variables.

All keywords that mark the beginning of a block of code are matched with a similar keyword marking the end of the block. In this example, MODULE was matched with ENDMODULE, while PROGRAM was matched with ENDROUTINE.

A program must contain one, and only one, PROGRAM declaration to indicate where execution will start.

The stack is handled explicitly. The programmer must declare a suitable INTEGER ARRAY for it, and initialize the stack before the first executable statement in the PROGRAM.

---

## Page 25

# M2 - Presenting PLANC

The first example contained only one routine, which happened to be the simplest of routine declarations, the PROGRAM. Now meet some more complex routines, a record with a routine inside it and an exception-handler.

```
%===========================================================%
% M 2                                                       %
%                                                           %
% This module shows more PLANC features, with emphasis      %
% on routines and records this time. The code here          %
% shows how vector arithmetic can be implemented. (Vectors  %
% are records consisting of real numbers that are handy in  %
% three-dimensional mathematics, in case you haven't met    %
% the term before.)                                         %
%===========================================================%
```

**MODULE** m2  
**INTEGER ARRAY** : Stack (0:1027)  

```
%===========================================================%
% s q r t                                                   %
%                                                           %
% PLANC does not have built-in mathematical functions, so   %
% an iterative algorithm for computing square roots of      %
% reals is implemented as a PLANC routine.                  %
%                                                           %
% All routines except the main PROGRAMs are defined using   %
% the keyword ROUTINE. PLANC has a couple of specialties:   %
% the in-value and the out-value. In Sqrt, the in-value is  %
% not used, as indicated with the keyword VOID, while the   %
% routine has an out-value that is REAL. That is the value  %
% that the routine will return to where it was called.      %
% Next comes parentheses, inside which the parameters to    %
% the ROUTINE are declared. There is one parameter here,    %
% namely the real number for which the square root is to    %
% be computed. Following the parameters comes a colon       %
% and the name that the routine will be known by in the     %
% program.                                                  %
%===========================================================%
```

**ROUTINE VOID, REAL (REAL : r) : Sqrt**  
% Declaring a couple of variables to work with. These  
% variables are only known locally, i.e. between  
% the enclosing ROUTINE ... ENDROUTINE statements.  
**REAL** : xn, xp

---

## Page 26

# Square Root Calculation Routine

We cannot compute the square root of a negative number.

```plaintext
IF r >= 0.0 THEN
    % Giving initial values to the local variables.
    r =: xn
    0.0 =: xp
    % The DO ... ENDDO construction makes an endless loop.
    % One or more WHILE statements are needed inside the loop
    % to exit from it.
    DO
        % This is the iterative formula.
        (xn + r/xn)/2.0 =: xn
        % Iterations are to continue until there is no
        % significant improvement relative to the previous
        % value.
    WHILE ABS(xn - xp) > 1.0E-6
        % This statement is executed as long as the above
        % condition holds. If the condition does not hold,
        % execution continues at the first statement after
        % the keyword ENDDO.
        xn =: xp
    ENDDO
    % Now, the value of xn is good enough to be returned
    % as the out-value of the routine.
    xn RETURN
ELSE
    % If r is negative, return to the calling routine
    % with an ERRETURN statement which signals that something
    % is wrong. If a routine exits through ERRETURN, an
    % exception handler in the calling program (see below)
    % can be invoked to take appropriate action.
    0 ERRETURN
ENDIF
ENDROUTINE Sqrt
```

## OldSqrt

In PLANC versions up to H, another ROUTINE declaration layout was used. In it, the parameter types were declared before the `:`, while the parameter names were declared after the routine name. Sqrt uses a new layout which is easier to read, but the old layout can still be used, and this is how an old style Sqrt declaration looks:

```plaintext
ROUTINE VOID, REAL (REAL) : OldSqrt (r)
    REAL : xn, xp
    % ... et cetera.
    xn RETURN
```

---

## Page 27

# Endroutine

```
==============================================================

% V e c t o r

% This is a record describing a vector as a record with
% three components. This vector definition also contains a
% routine that computes a real number that is called the
% length of the vector.

==============================================================
```

## TYPE Vector = RECORD

- The vector's three real components:
  - **REAL**: x, y, z
  - **Length**: returns a real out-value computed by applying the Sqrt routine to the sum of the square of the real components of the vector.

### ROUTINE VOID, REAL: Length
- **ON ... ENDON**: is an exception handler for use if Sqrt exits via its ERRETURN statement. When an exception occurs and handlers for it exist, the closest handler before the offending statement is activated. When the exception handler statements have been executed, the program will continue on the statement immediately after the offending statement - unless the exception handler changes the flow of control.
  
- **ON RUNTIMEERROR DO**
  - `%` is the line continuation sign. It makes the current statement continue on the next line.
  
  ```
  Output(1, 'a', &
  'You cannot get the square root of a negative number!$')
  ```

- **ENDON**

  ```
  Sqrt(x*x + y*y + z*z) RETURN
  ```

### ENDROUTINE Length

## ENDRECORD

```
==============================================================

% You are probably not used to routines having names such
% as the next routine, which is called +. It has an
% in-value, an out-value and a parameter which are all
% Vectors.

% The in-value to a routine represents a data element that
% can be written to, the left of the routine when it is
% called, and used inside the routine together with the 
% parameters.

% Furthermore, if a routine has only one parameter, the
% parameter can be written without an enclosing paren-
```

---

## Page 28

```
% theses when the routine is called.
%
% To uniquely identify a routine, the type of the
% in-value and the number and types of the parameter(s)
% are used together with the name of the routine. So when
% the compiler sees a Vector followed by a + followed by
% another Vector, it knows that + is the name of a routine
% that takes a Vector on each side of it - i.e. the +
% works like a binary operator. This + also has the same
% priority as the operator + when used in expressions.
% ==========================================================
ROUTINE Vector, Vector (Vector : b) : +
% A local Vector dataelement to put the desired
% out-value in.
Vector : Result
% The in-value is accessed using the special character @.
% Here, the x-component of the in-value is added to the
% x-component of the parameter, the result being stored
% into the x-component of the local Vector.
@.x + b.x =: Result.x
@.y + b.y =: Result.y
@.z + b.z =: Result.z
% ... Result's components contain the desired values,
% and Result can be returned as the routine's out-value.
Result RETURN

ENDROUTINE +
% All variables declared on the outermost level of the
% module are called global, while variables declared
% inside routines (including PROGRAMs) are called local.
%
% Global variables (and local variables with READ access
% only, see later) can be given initial values. Each
% variable of type Vector has three real numbers as
% components, and the three initial values for the compo-
% nents must be grouped together as a list of reals
% enclosed by parentheses. Statement continuation
% signs are not needed if you insert a CrLf in the middle
% of a list, like this:
Vector : xAxis := (1.0, 0.0, 0.0),
         yAxis := (0.0, 1.0, 0.0),
         zAxis := (0.0, 0.0, 1.0),
         Composite
% The following global variable is also initialized. Its
% single initial value need not be enclosed in parentheses.
REAL : I := 0.0
PROGRAM : Main
% ... Important: always remember to initialize the stack
% in your PROGRAM!
```

---

## Page 29

# INISTACK Stack

- In the next statement,
  - `zAxis` is a Vector used as in-value,
  - `+` is a routine name,
  - `yAxis` is a Vector used as parameter,
  - `Composite` is a Vector into which the out-value is stored.

```plaintext
zAxis + yAxis =: Composite
```

- All records of the type Vector contain a Length routine which has a real number representing its length as out-value. This routine is accessed via the usual dot notation for record component access. Let us compute the length of the Vector called Composite and store it into l.

```plaintext
Composite.Length =: l
```

```
ENDROUTINE Main
ENDMODULE
$EOF
```

## Comments

This program demonstrated how routines and records are made and used. Routines can be used like operators because of the in-/out-value mechanism and naming convention, while records can have routines as components.

### Routines

The keyword `ROUTINE` is used for defining all routines except the main `PROGRAM`.

`ROUTINES` always have an in-value and an out-value.

The in-value and the out-value can be declared as being of any valid type, or of the special type `VOID` if you do not want to use one or both of them. The keyword `VOID` can only be used in routine declaration.

When routines with non-void in-values are called, the name of the variable/constant/expression you want to use as in-value is written to the left of the routine name, and can subsequently be accessed (but not changed) using the special variable name @ inside the routine.

If the out-value is not declared as `VOID`, the routine returns to the calling statement with a value that can be stored in a variable or used in further calculations. If the out-value is void, then no value is returned.

Routines can have zero or more parameters, and the parameters can be of any type.

---

## Page 30

# Routine Parameters

The parameter names can either be declared together with their type declarations to the left of the `:` (new style), or separately after the routine name (old style).

When the routine is called, the parameters are written as a list enclosed in parentheses to the right of the routine name, with one exception: if the routine has one parameter, the parameter can optionally be written without enclosing parentheses.

To identify the routine declaration used in a routine call when the routine name is _overloaded_, the compiler uses the type of the in-value, the number and the types of the parameters, and the name of the routine.

The out-value is not used for routine identification when overloading. This is because the out-value is not necessarily used. The routine name may be written as a statement all by itself, if necessary preceded by an in-value only. On the other hand, the routine name may be written as in-value to other routines. The latter routines have the same name, but have different types of in-value. The compiler, which uses the routine declarations to identify overloaded routines, will not get enough information about the out-value of the first routine from the context of the call to decide which of the latter is to be used.

# Records

Records can contain routines as components.

Routines that are part of records are called using the customary dot notation for record component access.

Such routines can access the components of the record they are declared in directly (without the use of dot notation).

# Exception-handlers

If specific error conditions occur in the program, execution of exception-handler code tailored to the condition can be invoked. Such specific conditions can be errors in the routines called, attempts to divide by zero, stack overflows, and when evaluation of conditional expressions in `ASSERT` statements yields the value `FALSE`.

Exception-handler code is enclosed between the keywords `ON` and `ENDON`.

When an exception condition arises, the statements in the first relevant exception-handler before the offending statement are executed.

If the exception-handler does not contain statements that affect flow of control, the first statement after the offending statement will be executed after the exception-handler statements.

---

## Page 31

# Control Structures

In the following example, you will meet these control structures:

## IF statement

```
IF condition THEN
    statements
ELSIF condition THEN
    statements
ELSE
    statements
ENDIF
```

## CASE statement

```
CASE single-valued variable
    INCASE value
        statements
    ELSE
        default statements
ENDCASE
```

## ON statement

```
ON exception condition DO
    statements
ENDON
```

## DO loop

```
DO
    statements
    WHILE condition
        statements
    EXITWHILE
        statements
ENDDO
```

## FOR loop

```
FOR control variable IN value list DO
    statements
    WHILE condition
        statements
    EXITWHILE
        statements
    EXITFOR
        statements
ENDFOR
```

---

## Page 32

# M3 - Control Structures

```
%===============================================================%
% M 3                                                           %
%                                                               %
% Control structure examples                                    %
%===============================================================%
```

```
MODULE m3
CONSTANT MaxInteger = 2 ** 31
INTEGER ARRAY : Stack (0:1023)
% Space will be used for dynamic memory allocation.
INTEGER ARRAY : Space (0:1023)
INTEGER ARRAY : TestArray (0:15)                    % For general use.
INTEGER : i := 0, j := 1, k, l
% Some ENUMERATION and SET declarations:
TYPE PrimaryColour = ENUMERATION (Red, Yellow, Blue)
PrimaryColour : SingleColour := Blue
TYPE Colour = PrimaryColour SET
Colour : Green := (Yellow, Blue)
% A RECORD type for use in linked lists:
TYPE ListElement = RECORD
    INTEGER : Key
    ListElement POINTER : next
ENDRECORD
ListElement POINTER : ListHead, ListPointer
PROGRAM : m3Main
    % You c a n  use LABELs and GO. Here, a LABEL is
    % declared for use in this routine.
    LABEL : EndLoop
    INSTACK stack
    % The simplest control structure is the IF statement:
    IF i = 0 THEN
        4 =: i
    % IF is always matched with an
    ENDIF
    % IF statements can have more than one choice.
    IF i = 1 THEN
        3 =: i
    % If the first condition did not hold, try another one.
    % There may be any number of ELSIFs.
    ELSIF i = 0 THEN
        1 =: i
    % If none of the previous conditions held, execute the
    % statements after ELSE.
    ELSE
```

---

## Page 33

# Technical Document

7 =: i

**ENDIF**

- The **INTEGER RANGE** (0:255) types (including **BYTE**) and **ENUMERATION** types can be used in CASE statements. If there are many values to choose from, CASE is usually fastest. ELSE must be present unless all values for SingleColour are used in INCASE statements.

**CASE** SingleColour

- In case the SingleColour is Red, jump to the statement after this INCASE statement.

**INCASE** Red

- Blue =: SingleColour
- ... and so on.

**INCASE** Yellow

- Red =: SingleColour
- If the SingleColour did not match any of the alternatives, execute the statements after ELSE.

**ELSE**

- Yellow =: SingleColour

**ENDCASE**

- Here comes an **exception-handler**. It can be used if some assertion about the program does not hold any more, if an error occurs in a routine that is called by the current routine, if the hardware detects a division by zero or similar.
- The following exception handler is used if a programmer-defined assertion in the endless loop immediately after it does not hold any more.

**ON ASSERTFALSE DO**

- The action taken in this exception handler is to jump to the LABEL with name EndLoop.

**GO** EndLoop

**ENDON**

- The DO ... ENDDO statement is basically an endless loop.

**DO**

- (The unary ++ operator increments i by one.)
  ++ i
- This **ASSERT** must hold, otherwise control is transferred to the ON ASSERTFALSE exception handler that was defined above.
  
  **ASSERT** i < 100

**ENDDO**

- A LABEL that can be jumped to by the GO statement. Remember that LABELs must be declared.

**EndLoop:**

- A DO loop can contain zero or more WHILE statements and an optional EXITWHILE statement. The looping is terminated the first time a WHILE condition does not.

---

## Page 34

```
% hold. If a WHILE terminates the loop and it has an
% EXITWHILE, then, and only then, the statements between
% EXITWHILE and ENDDO are executed.
1 := i =: j

DO
   ++ i
   WHILE i < 10
      i * j =: j
   WHILE j < 1000
EXITWHILE
   ABS j =: j
ENDDO

% The FOR loop is very capable. The statement inside this
% one is executed once for all values between 7 and 11,
% beginning with i = 7, then with i incremented to 8, and
% so on until, and including, i = 11.
FOR i IN 7:11 DO
   i - 1 =: j
ENDFOR

% In the next FOR loop, the declaration of TestArray(0:15)
% is used to provide a range to loop through. The first
% value of i will be 0, the next will be 1, and so on
% until and including the highest number of a TestArray
% element, which is 15, is reached. This provides an easy
% way to access all elements in an array.
1 := j =: k

FOR i IN TestArray DO
   % The loop computes Fibonacci numbers and stores them
   % in the TestArray.
   j + k =: l =: TestArray(i)
   % The WHILE statement can be used to exit from FOR
   % loops too.
   WHILE l <= MaxInteger
      k =: j; l =: k
% As in DO loops, EXITWHILE is optional if WHILE has been
% used in the loop.
EXITWHILE
   o =: k
   % There is an optional EXITFOR. If the FOR loop terminates
   % "normally" and not because a WHILE condition does not
   % hold, then, and only then, the statements after EXITFOR are
   % executed. If both EXITWHILE and EXITFOR are used,
   % then EXITWHILE must come before EXITFOR. After exit via
   % WHILE, the EXITFOR statement is not executed.
EXITFOR
   0 =: l
```

---

## Page 35

# FOR Loop Using Sets

Another version of the FOR loop uses the values held in **SETS** to control the loop. As Green is a Colour, which in turn is a **SET** of the values of the **ENUMERATION** type `PrimaryColours`, the following loop counts the number of `PrimaryColours` that Green consists of.

```
0 := i
FOR SingleColour IN Green DO
   ++ i
END FOR
```

# Linked Lists

We digress a little before the final FOR loop is presented, because it deals with linked lists. The standard routine `New` creates an instance of the type given as parameter and returns a POINTER to the memory location where the new `ListElement` is placed. The IN clause is optional. If it is used, the new `ListElement` is placed in the INTEGER ARRAY that follows, otherwise the new `ListElement` is placed on the stack. Here, the new `ListElement` is placed in the INTEGER ARRAY named `Space`.

New `ListElement IN Space` := `ListHead` := `ListPointer`

This FOR loop has a REVERSE clause. The effect of the REVERSE clause here is that i will take on the values 1, 3, 9, 8, 7 - that is, the range 7:9 is scanned in reverse order.

```
FOR i IN REVERSE 1, 3, 7:9 DO
   % Store the current i into the Key of the ListElement
   % pointed to by ListPointer. If you have a pointer,
   % the standard routine Ind returns the element pointed
   % to when given a pointer as parameter. (We will see
   % below that Ind is not strictly necessary.)
   i := Ind(ListPointer).Key
   
   % Make a new ListElement and let the ListPointer's
   % Next, which is of the appropriate ListElement POINTER
   % type, point to it. Thus, the new ListElement is
   % appended to the list.
   New ListElement IN Space := Ind(ListPointer).Next
   
   % Update ListPointer to point to the new ListElement.
   % Since it is clear that we mean a RECORD and not a
   % pointer when we use the dot notation, the Ind
   % call can be removed.
   ListPointer.Next := ListPointer
EXIT FOR
0 := ListPointer.Key
```

---

## Page 36

```
% The keyword NIL is used to indicate that a POINTER
% variable points to nothing. If the pointer to the
% next ListElement is NIL, the program knows it has
% reached the end of the list.
NIL =: ListPointer.Next

ENDFOR
% The final version of the FOR loop scans through linked
% lists. To do so, it needs a POINTER to the first ListElement
% to be looked at, and to know which one of the ListElement's
% components points to the next element in the list.
% The program will loop until the pointer to the next
% element has the special value NIL, that is, points to
% nothing. (If it does not, we have a bug!)

% Here is how to count the list members.
0 =: i

FOR ListPointer IN ListHead:Next DO
  ++ i
ENDFOR
ENDROUTINE m3Main
ENDMODULE
$EOF
```

# Comments

## The IF statement

The `IF` Boolean expression `THEN ... ENDIF` block contains zero or more `ELSIF` Boolean expression `THEN` statements. The `ELSIF` statements are followed by zero or one `ELSE` statement.

## The CASE statement

The control flow in the `CASE variable... ENDCASE` block depends on simple variable that cannot be a `REAL`, as it must have discrete values in the range 0 to 255.

The `CASE ... ENDCASE` block contains one or more `INCASE` value statements, to which control is transferred if the `CASE` variable has that value, followed by an optional `ELSE` if the `CASE` variable's value does not have a matching `INCASE`.

## Exception-handlers

The blocks enclosed by the keywords `ON ... ENDON` are called exception-handlers.

The statements in the `ON exception condition(s) DO... ENDON` block are executed if specific error conditions occur during program execution. Such conditions can be detected by the hardware (such as division by zero), by the operating system (for instance, when a file does not exist),
```

---

## Page 37

# The DO Loop

The `DO ... ENDDO` block is an endless loop.

To leave a `DO` loop, one or more `WHILE` conditional expression statements can be placed inside the loop. When the Boolean expression no longer holds, the loop is terminated.

If the `DO` loop contains `WHILE` statements, it can contain an optional `EXITWHILE` statement. When and only when the loop terminates via a `WHILE`, the statements between `EXITWHILE` and `ENDDO` are executed.

# The FOR Loop

The `FOR simple variable IN value list DO ... ENDFOR` loop has several ways of controlling looping: By incrementing/decrementing simple types (except `REAL`), by looping through the index range of an array, by looping through the values contained in a `SET`, and by following `POINTERs` through linked lists.

`FOR` loops may contain an `EXITFOR` statement. When and only when such a `FOR` loop terminates after the last value has been looped for, the statements between the `EXITFOR` and `ENDFOR` are executed.

`FOR` loops can contain `WHILE` Boolean condition statements as well. Such loops will terminate before the last value in the `FOR` list if the condition in the `WHILE` statement is no longer met.

If a `FOR` loop contains one or more `WHILE` statements, an optional `EXITWHILE` statement can be inserted _before_ the optional `EXITFOR`, or before the `ENDFOR` if there is no `EXITFOR`. The statements after `EXITWHILE` are executed when, and only when, the loop terminates via a `WHILE`.

If a `FOR` loop contains both `EXITWHILE` and `EXITFOR`, only one of them will be executed when the loop terminates.

The control variable in `FOR` loops can be of type `INTEGER`, `ENUMERATION`, or `BYTE`, or it can be a `POINTER` to a `RECORD` type that can be used in linked lists. It cannot be `REAL`.

---

## Page 38

# Technical Page

Except when using linked lists, the values for the control variable are given as a value list of single values and value ranges. A value range is a variable of type `set` or specified as `lower bound: upper bound`.

Arrays are declared using value-range definitions, and the indexes of the array elements can be used in `for` value lists.

The `REVERSE` clause in `for` loops applies to value ranges specified as `lower bound: upper bound` only.

When the `REVERSE` clause is used in a `for` loop, the order of the members of the value list is kept, while the values in value ranges that are members of the list are used in reverse order.

Linked lists can be used to control `for` loops. In this case, the control variable is a pointer to the `RECORD` type of the list members, while the value list is specified as `first list member: next member component`. The `first list member` is a `POINTER` to the list member record, which in turn must have a `POINTER` to the next member in the list as one of its components.

`for` loops with linked lists are exited when the value of the pointer to the next element in the linked list is `NIL` (i.e. the pointer points to nothing). The loop will not terminate properly if this is not the case.

---

## Page 39

# Declarations

| Simple and composite types | PLANC types can be broadly divided into two different kinds: simple and composite. Types of the former kind are the building blocks for the latter. |
|----------------------------|--------------------------------------------------------------------------------------------------------------------------------------|
| Several examples in this section | In this section, you will find several example programs. M4 demonstrates how to declare simple variables, M5 introduces type expressions using simple types, M6 shows how to declare arrays, M7 deals with sets and M8 shows how to make records. |
| Routines given special treatment | The PLANC routine is a special type, and will be treated in a separate subsection of this chapter. |
| Type modifiers | Type modifiers are keywords used to make new subtypes by changing the characteristics of a type, while retaining the properties of the basic type. Examples are `RANGE`, which limits the set of values of the basic integer simple-type variable, and `PACKED`, which packs data in composite types more densely than the default packing. |
| Type constructors | Type constructors make types that are different from the types they are based on. Examples are `POINTER`, which makes a pointer to a variable of the type or type expression it is applied to, and `RECORD`, which makes a new type from a collection of existing types and type expressions using existing types. |

# M4 - Declaring simple variables

## Purpose

The simple types are the basic building blocks of PLANC's data structures - the data structures used in `ARRAY`, `SET`, `RECORD` and `ROUTINE` declarations either all are or can be reduced into simple types.

The simple types that are available are:

**INTEGER** - you can make one-, two- or four-byte integers thus:
- `INTEGER1`
- `INTEGER2`
- `INTEGER4`
- `BYTE`

**REAL** - you can make a 64-bit real like this:
- `REAL8`

**BOOLEAN** - varieties:

---

## Page 40

# BOOLEANS

- **BOOLEAN1**
- **BOOLEAN2**

**LABEL** will always be big enough to address the complete address space on your CPU. On INTEL CPUs, that means four bytes.

**VOID** is used in routine declarations to indicate non-existent in- or out-values.

**ENUMERATION** allows you to define your own range of up to 256 different values.

**POINTER** contains the address of a variable, which can be of any type.

## Modifiers

In variable declarations, the simple-type specification can be followed by one or more modifiers. Such modifiers are:

- **UNSIGNED** indicates that only positive values can be assigned to this variable.
- **RANGE** defines a set of values that are valid in assignments to the variable (not applicable to reals).
- **PRECISION** is only applicable to reals, and indicates how many valid digits it will have after the decimal point.
- **READ** allows code in the block where the variable is declared to read its value.
- **WRITE** allows code in the block where the variable is declared to change its value.

```
%====================================================================%
%                               M 4                                  %
%                                                                    %
% Simple variable declarations.                                      %
%                                                                    %
% The simple variables and their subtypes are the basis              %
% on which all other data types are built. This example              %
% shows how to declare and use them.                                 %
%====================================================================%
MODULE m4
% Declaring a constant for later use. The constants are
% used by the compiler only, and take no memory locations
% in programs.
CONSTANT Kilo = 1024
% An ordinary declaration of some integers, of which one (1)
```

---

## Page 41

# Technical Page

% is initialized to contain the value associated with the constant Kilo:

## INTEGER

- `i, j, k, l := Kilo, m, n`

% Now, an integer occupying two bytes which will accept only positive values less than 2**16-1. To modify the INTEGER to occupy two bytes, we affix the number 2 to it. To make it accept positive values only, we apply the modifier % UNSIGNED.

### INTEGER2 UNSIGNED

- `i2u`

% The following integer occupies one byte and can only have values between -64 and 63 assigned to it at compile time. There are no runtime checks to verify that values stay in the range they are declared in.

### INTEGER1 RANGE

- `(-64:63) : i1r := -15`

% Here comes a real of 8 bytes which will be accurate to 15 digits due to the PRECISION modifier, and which is initialized.

### REAL8 PRECISION (15)

- `r := 1.8350E13`

% The BYTE is like an INTEGER1 UNSIGNED. This one has access & defined so that it can be both read from and written to by code inside the module. This is the default access mode for variables. It is initialized to contain the ASCII value for the uppercase letter 'B'.

### BYTE READ WRITE

- `b := #B`

% ENUMERATION is a type constructor that makes it possible to define special value sets when needed. This is useful in conjunction with CASE blocks, for example.

### ENUMERATION

- `(Monday, Tuesday, Wednesday, Thursday, Friday) &`
- `Workday := Monday`

% Here comes a boolean variable, which is initialized to the true value FALSE.

### BOOLEAN

- `boo := FALSE`

% Using the type constructor POINTER, you can make simple variables which are pointers to variables of all types. % (A pointer can contain a number which is the memory address of a variable of its base type.) The following pointer points to an eight-byte real, and the keyword Addr is used to indicate which variable's address it is going to contain.

### REAL8 POINTER

- `pr := Addr(r)`

% The parentheses around the "parameter" to Addr is optional.

### INTEGER POINTER

- `pi := Addr i`

% If the variable you create a pointer to has modifiers, the modifiers must be included in the pointer declaration.

### INTEGER1 RANGE POINTER

- `pi1`

% You can always use the standard routines Size and Bit_size to calculate the size in bytes or bits of a type or variable.

---

## Page 42

# Technical Documentation

## Constants

```
CONSTANT Size1 = Size pil, Size2 = Bit_size pil
```

% You can make a new variable of the same type as an existing  
% one with the standard routine Typeof:

```
Typeof pil : SameAspil
```

% A pointer variable can be pointed to by other pointers  
% that are declared like this:

```
INTEGER POINTER POINTER : ppi := Addr pi
INTEGER POINTER POINTER POINTER : pppi := Addr ppi
```

% You can force names of different types to refer to the same  
% location in memory with the `=` sign, which is similar to the  
% EQUIVALENCE statement in FORTRAN. Here, if you look at  
% what piEquiv contains, you will get an integer number which  
% is the address of the integer i.

```
INTEGER : piEquiv = pi
```

---

## ParaTran

% The most important use of the READ and WRITE modifiers  
% is in routines. The parameters named iwRout and irwRout  
% are especially noteworthy: changes done to the variables  
% passed to the routine as these parameters will remain  
% after return from the routine call. This offers an  
% alternative to using global variables or pointer parame-  
% ters to preserve the results of the routine after it has  
% returned.

---

## Routine Modifiers

| Modifier           | Description           |
|--------------------|-----------------------|
| ROUTINE INTEGER,   | && In-value, read only. |
| INTEGER (          | && Out-value.          |
| INTEGER : iRout;   | && Un-qualified, read only. |
| INTEGER READ : iRout; | && Can be read from.   |
| INTEGER WRITE : iwRout; | && Write (and read!) |
| INTEGER READ WRITE : irwRout | && Read and write. |

% ParaTran

% These are local variables - they cannot be used  
% outside this routine.

```
INTEGER : LocalInt, LabInt
```

% Labels can only be declared inside routines.

```
LABEL : Lab
```

% Inside this routine, `@` is the name used to access the  
% in-value. All of the variables `@`, iRout and irRout can  
% only be read from, not stored into.

- `iRout + irRout =: LocalInt`
- `iwRout` can only be stored into. The value assigned to  
  it here will remain after the routine has returned.
- `@ + 1 =: iwRout`
- `irwRout` can be read from and written to, and changes

---

## Page 43

```
% will remain after the routine has returned.
-- lrwRout =: lrwRout
% A label name followed by a colon designates a program
% address. This is how you can get at that address:
Lab: [Lab CONVERT INTEGER] =: LabInt
% The next statement returns to the calling routine,
% passing LocalInt on as the out-value of the routine.
LocalInt RETURN
ENDROUTINE ParaTran
INTEGER ARRAY : m4Stack (0:Kilo-1)
%============================================================%
% m 4 M a i n                                                 %
%                                                            %
% This program shows some of the assignments that can be     %
% made using the variables declared above.                   %
%============================================================%

PROGRAM : m4Main
INITSTACK m4Stack
% The standard routines Succ and Pred access the
% predecessor or successor of an enumeration variable
% according to the order the values were defined in.
Succ Workday =: Workday
% The keyword Addr can be used as a standard routine in
% statements. It takes one parameter, and returns a
% pointer to the variable given as parameter. To be able
% to store this pointer into pi1, pi1 must have been
% declared as a pointer to variables of the relevant
% type, including modifiers.
Addr i1r =: pi1
% The keyword Addr has an "inverse" keyword, Ind. When
% given a pointer as in-value, Ind returns the element
% being pointed to. In the following statement, the
% application of Ind to pi returns the variable pointed
% to by pi, which is the integer i. Thus, the effect of
% the statement is to store 5 into i.
5 =: Ind(pi)
% Since ppi is a pointer to another pointer which can
% point to an integer, and since ppi is initialized to
% point to pi, the result of the following statement is
% to store 6 into i.
6 =: Ind Ind ppi
% The two operators CONVERT and FORCE can be used
% to convert between data types. Both have an in-value
% and a "parameter" which is the type of the value you
% want as an out-value.
%
% This CONVERT will return an integer which is truncated
% if necessary.
```

---

## Page 44

```
r CONVERT INTEGER =: j
% FORCE copies bit patterns. There must be room for all
% the bits of the in-value in variables of the out-value
% type.
pi FORCE INTEGER =: j
% The special value NIL is used to indicate that a
% pointer does not point to anything.
NIL =: pi

% Finally, a call to the ParaTran routine.
2 =: i =: j =: k =: l =: m
m ParaTran (i, j, k, l) =: n

ENDROUTINE m4Main
ENDMODULE
$EOF
```

# Comments

|                                         |                                                               |
|-----------------------------------------|---------------------------------------------------------------|
| The size of simple types                | For most simple types, the type can be suffixed with a digit indicating the number of bytes you want it to occupy. For example, INTEGER1 is an integer occupying one byte of the computer's memory. When a simple type is specified without suffix, its size depends on the word size of the CPU the code is compiled for. |

| Type checking and implicit type conversion | The compiler will check that variables are appropriate in the context of the expressions where they are used. When necessary, it will also convert the sizes of the subtypes so that they match. For details, see the relevant sections of the General Topics chapter. |

| Converting between types               | It is not considered good programming practice to override the type checking. However, some bit-twiddlers have a need to do so from time to time. The standard routines CONVERT and FORCE together with the equivalence sign (=) are available for that purpose. The standard routine CONVERT causes its in-value to be converted into a value of the type of its parameter. If converting from REAL to any other type, the real value will be truncated, not rounded. The standard routine FORCE copies bits from the in-value variable to a value of the type of its parameter, provided the parameter type has the same number of bits as the in-value. The equivalence sign (=) makes the first byte of the variable being declared lie at the same address in memory as the first byte of the variable specified after the equivalence sign. (The "first byte" is the byte closest to ... [illegible]) |

Scanned by Jonny Oddone for Sintran Data © 2021

---

## Page 45

# Pointers

A type specification followed by the keyword `POINTER` is a pointer declaration.

Variables declared as pointers contain the address of variables of the type described by the types and modifiers preceding the `POINTER` keyword.

The special value `NIL` can be assigned to pointers to indicate that they do not point to any specific variable.

The keyword `Addr` can be used in both declarations and statements to get the address of variables. In declarations, `Addr variable name` initializes a pointer with the address of the named variable. In statements, it is a standard routine giving the address of the variable given as a parameter.

The keyword `Ind` is the inverse of `Addr`. It can only be used as a standard routine, and returns the variable pointed to by the pointer given as a parameter to the routine.

# Useful Standard Routines

The standard routines `Size`, `Bit_size`, and `Typeof` are used to get the size of a variable that you have declared, and to make new variables of precisely the same type/subtype as the old ones.

---

## Page 46

# M5 - Type Expressions

## Purpose

`type expressions` are necessary to make more complex data types than the simple types and subtypes of the previous section. They make construction of new types, both subtypes of simple types and composite types, much easier.

## Simple Examples in This Section

The following example shows how type expressions are used with simple variables. The topic of the next sections is construction and declaration of more complex data types and variables, and the corresponding type expressions will be demonstrated in that context.

```
%===============================================================%
%                           M 5                                 %
%                                                               %
%                    Type expressions                           %
%                                                               %
% The purpose of this module is to introduce the type           %
% expressions, which are used to build new types from old.      %
% Type expressions are most useful when making new              %
% composite types, but can also be useful simple expres-        %
% sions, as a lengthy sequence of keywords may be neces-        %
% sary to make the subtype you are interested in, and type      %
% expressions may save some finger energy.                      %
%===============================================================%

MODULE m5
% A type statement begins with the keyword TYPE, followed by
% the name you want to associate with the new type and an 
% equals sign (=). To the right of the equals sign comes the 
% t y p e   e x p r e s s i o n: a sequence of types and 
% modifiers as they would appear in a variable declaration.
% This is how you would make a new type, Nibble, which is
% four bits:                     
TYPE Nibble = INTEGER RANGE(0:15)
% In the type expression above, INTEGER is a simple type
% while the RANGE modifier is used to create the appropriate
% subtype. Whenever you need a new INTEGER RANGE(0:15),
% you can declare it like this:
  Nibble : n1
% ENUMERATION is a type constructor that makes a new simple
% type which can have any number of different tokens as value,
% but can have only up to 256 different values if it is going to
% be used in SETs or as a control variable in CASE statements.
% If you have an enumeration that is going to be frequently
% used, you can use the following type expression:
```

[Scanned by Jonny Oddene for Sintran Data © 2021]

---

## Page 47

# WeekDay Enumeration

```plaintext
TYPE WeekDay = ENUMERATION &
  (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday)
```

* Subsequent declarations may look like this:
  
```plaintext
  Weekday : Workday := Monday
```

* Previously declared types can be used as part of new type expressions:

```plaintext
TYPE r8p10 = REAL8 PRECISION(10)
TYPE r8p10p = r8p10 POINTER
  r8p10p : ResultLocation
```

* The standard routine Typeof can be used to make new variables identical to one already defined:

```plaintext
Typeof ResultLocation : ResLoc2
```

```plaintext
TYPE ir = INTEGER READ
TYPE irw = ir WRITE

INTEGER ARRAY : stack (0:127)
```

* Type expressions are most useful when used to make composite types (ARRAYs, SETs, RECORDs, ROUTINEs).
* The next sections will give further details about this. Here are some examples of how to use simple types and subtypes.

```plaintext
PROGRAM : TypeExprs
  irw : i

  INITSTACK stack
  % Variables made with types that are subtypes of
  % the same simple type can be used in expressions:
  s =: nl =: i
  % If they belong to different types, you must
  % use CONVERT or FORCE:
  Workday FORCE INTEGER =: i
  Workday CONVERT Nibble =: nl
  % Another use of Typeof:
  Workday CONVERT (Typeof nl) =: nl

ENDROUTINE
ENDMODULE
$EOF
```

# Comments

Declarations of even simple types and subtypes can be a little complicated, so type expressions are handy to save finger energy, to make uniform updates in a type/subtype that is used several places in the same program, and to improve legibility.

As a matter of fact, type expressions can be avoided altogether: You can

---

## Page 48

# M6 - Arrays

| PLANC arrays similar to arrays in other languages | You can use arrays to hold tables of information in PLANC, like in most other block-structured languages. Arrays are formed by application of the keyword `ARRAY` as many times as there are dimensions in the array after the base type of the array has been declared. |
|---|---|
| What arrays consist of | In a declaration of an array type, both simple and composite types can be used as a basic building element. The basic elements in an array can be retrieved using the name of the array followed by a list of one or more index values enclosed in parentheses. |
| Access to the elements | The number of elements in the index list must be equal to or less than the number of dimensions in the array. If there is no index list, the operation is on the entire array. If there are fewer values in the index list than there are dimensions in the array, the access is to a subarray of the entire array with as many dimensions as are left unspecified in the accessing statement. |

---

## Page 49

# M 6

## ARRAY Declarations

Arrays are composite types whose components are types which are addressable via one or more indexes. The type of the components may be any valid PLANC type.

---

## MODULE m6

An array declaration begins with the type of the basic element in the array, then comes the type constructor `ARRAY` repeated once for each dimension in the array, then a colon followed by the variable name, and then parentheses containing an upper and a lower bound for each dimension.

The arrays used for the stack always have one dimension. The first element in the stack array must always have index 0.

### INTEGER ARRAY

`Stack (0:1023)`

Composite types can be initialized when declared globally (or declared locally with READ access):

### INTEGER ARRAY

`SmallArray(0:5) := (5, 6, 7, 8, 9, 10)`

This is how a two-dimensional array of integers is declared and initialized. When addressed using one index, it can be seen as an array whose components are arrays themselves, while it is a two-dimensional array whose components are integer when addressed with two indexes.

You need not initialize all components, un-initialized components will be zero.

### INTEGER ARRAY ARRAY

```plaintext
iaa(0:1, 0:2) := ((1, 2, 3), (4, 5, 6))
iaa2(0:1, 0:2) := ((1, 1), (3))
```

Modifiers written before the first ARRAY apply to the single elements that the array consists of, while modifiers written after the last ARRAY apply to the entire array.

In the following example, the elements of the two-dimensional array `iuwaar` are unsigned INTEGERS which can only be written to one at a time, while you can only read from the entire array in one go instead of reading single elements.

### INTEGER UNSIGNED WRITE ARRAY ARRAY READ

`iuwaar(0:1, 0:2)`

### INTEGER UNSIGNED WRITE ARRAY ARRAY

`iuwaar2(0:1, 0:2)`

Arrays can be PACKED, meaning that the compiler will pack the single elements together to take less data space. Note that the packing in PLANC is pragmatic, in that some wastage is allowed in packed data to avoid excessive overheads in access times and code size.

---

## Page 50

# Array Packing

When packing arrays, each element of the base type (be it a simple type or a record) will occupy a minimum of one byte (except BITS and BOOLEAN ARRAY PACKED arrays, see below). Hence, the following array will occupy exactly two 32-bit words or four 16-bit words in the final program. Four bits will be used and four bits will be wasted in each byte.

## INTEGER RANGE (0:15) ARRAY PACKED : Nibbles(0:7)

The BYTES type is similar to a BYTE ARRAY PACKED. (And a BYTE is similar to an INTEGER1 UNSIGNED.) BYTES can be initialized, like s1 and s2 below are. If not all bytes are initialized, the rest will be filled with zeros (ASCII 0). s2 will contain as many bytes as are contained in the initializing string, and the lowest index number will be 0.

```plaintext
BYTES : s1 (0:79) := '-> Screenline <-',
        s2 := 'This is a string'
```

You can declare BITS too. BITS is similar to BOOLEAN ARRAY PACKED without padding to next byte boundary for each element, and is used to access individual bits. This variable occupies one byte:

```plaintext
BITS : Flags(0:7) := s
       (TRUE, FALSE, TRUE, TRUE, 
        FALSE, FALSE, FALSE, TRUE)
```

The following byte will overlap the BITS of the line above, because the equivalence operator (=) causes the variable named after the colon to be located at the same address as the variable named after the equivalence.

```plaintext
BYTE : FlagByte = Flags
```

The following type statement shows how you define new array types to the compiler:

```plaintext
TYPE iaType = INTEGER ARRAY
```

After this, writing iaType is synonymous to writing INTEGER ARRAY in subsequent declarations.

```plaintext
iaType : iaT (5:10)
iaType ARRAY : iaTa(1:3, 5:7)
```

A two-dimensional array type can be made thus:

```plaintext
TYPE iaType2 = iaType ARRAY
```

... or thus:

```plaintext
TYPE SameAsiaType2 = INTEGER ARRAY ARRAY
```

It is simple to make pointers to arrays (or any other type).

```plaintext
INTEGER ARRAY POINTER : iap
INTEGER ARRAY ARRAY POINTER : iaap
```

---

## Page 51

```
% Or alternatively:
iaType POINTER : iapAlt
iaType2 POINTER : iapAlt2
% Or:
TYPE iaPointType = iaType POINTER
iaPointType : iaPT
% etc.
%
% The following code shows how arrays can be used locally
% and in statements.

PROGRAM : ModifyArray
INTEGER : i, j, k, Rows, Columns
BOOLEAN : TestResult

% Local arrays can be initialized when declared as READ:
INTEGER ARRAY READ : LocRead (0:7) := 
  (7, 6, 5, 4, 3, 2, 1, 0)
% But otherwise, local arrays cannot be initialized.
INTEGER ARRAY : LocArr (0:7)
iaType : LociaT(0:2)

INITSTACK Stack

% Here is another way to assign data to a composite
% variable. Note that the type of the variable is
% enclosed in parentheses, to make it clear to the
% compiler that this is a statement and not a declaration.
(INTEGER ARRAY (7, 6, 5, 4, 3, 2, 1, 0)) := LocArr

% The highest and lowest index of an array is returned
% by the standard routines Maxindex and Minindex.
Maxindex(SmallArray) - Minindex(SmallArray) =: i

% If the array has more than one dimension, it is
% necessary to state the number of the dimension you
% want. The first statement below looks at the first
% dimension, the next at the second dimension.
Maxindex(iuwaar, 1) - Minindex(iuwaar, 1) =: Rows
Maxindex(iuwaar, 2) - Minindex(iuwaar, 2) =: Columns

% Now, store the one-dimensional array formed by keeping
% the first index of ia a constant into another one-
% dimensional array, LociaT:
iaa(0) =: LociaT

% We include the following just to remind you how you can
% loop through all elements in an array. In multi-
% dimensional arrays, you will have to tell the compiler
% the number of the dimension to work on (see later).

FOR j IN Flags DO
  % Setting every bit in the FlagByte.
  TRUE =: Flags(j)
ENDFOR
```

---

## Page 52

```
% You can see if i is a valid index to an array like this:
i IN SmallArray =: TestResult
% Since iuwaar can only be written to one element at
% a time, a loop like this may be necessary to fill
% it with values.
-l =: k
FOR i IN Minindex(iuwaar, 1):Maxindex(iuwaar, 1) DO
  FOR j IN Minindex(iuwaar, 2):Maxindex(iuwaar, 2) DO
    SmallArray (++k) =: iuwaar(i, j)
  ENDFOR
% An alternative way to write a FOR loop for a
% multi-dimensional array is as follows, where the
% number of the dimension which you want to loop
% through is indicated inside parentheses:
  FOR j IN iuwaar(2) DO
    % ...
  ENDFOR
ENDFOR
% But the entire iuwaar array can be read in one go.
% Here it is copied into the array iuwaar2.
iuwaar =: iuwaar2
ENDROUTINE ModifyArray
ENDMODULE
$EOF
```

---

## Page 53

# M7 - Sets

## Sets

The composite-data type `SET` corresponds to the mathematical notion of a set, with limitations in the number of elements in the set and on what may be used as set members. The mathematical operations of union, intersection, test on set membership etc. are available.

```
==========================================================
%  M 7
%
%  Declaration of sets
%
%  Sets may hold up to 256 elements that are either
%  integers or enumerations. There are operators available
%  for operations such as unions, intersections, negations
%  and tests for membership.
==========================================================
MODULE M7
% Some more types, some made with enumerations:
TYPE MonthDate = INTEGER1 RANGE  [0:31]
TYPE aDay = ENUMERATION &
  (Monday, Tuesday, Wednesday, Thursday,
   Friday, Saturday, Sunday)
TYPE aMonth = ENUMERATION &
  (January, February, March, April, May, June,
   July, August, September, October, November, December)
% Introducing the type constructor SET. It can contain
% up to 256 values of the INTEGER types (including BYTE)
% and ENUMERATION.
TYPE Days = aDay SET
% This is how SET variables are declared and initialized.
Days : Workdays := (Monday:Friday), Weekend := &
  (Friday, Saturday, Sunday)
TYPE ASCIIChar = BYTE SET
ASCIIChar : Printable := (40B:177B),
  UpperCase := (#A:#Z),
  LowerCase := (#a:#z),
  Digits := (#0:#9) 
INTEGER ARRAY : Stack (0:1023)
========================================================== 
%  S e t U s e  
%
%  Showing how sets are used in programs.
==========================================================
PROGRAM : SetUse
```

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 54

```
aDay : TheDay
Days : DayOff
ASCIIChar : AllowedFirst, AllowedAfterFirst, Especial
BYTES POINTER : StringPointer, NamePointer
INTEGER UNSIGNED : FirstInName, LastInName
BOOLEAN : TestResult
INSTACK Stack

% The set operations for union, intersection and
% negation work with the sets we have declared. We
% can also test for equality, inequality, subset,
% true subset, membership etc.
%
% The following statement finds the days where you
% do not go to work:
Weekend AND NOT WorkDays =: DaysOff

% In PLANC names, the first character must be a letter
% while the other characters can be characters, digits
% and underscores (_). This is how you make the
% corresponding sets.
UpperCase OR LowerCase =: AllowedFirst
AllowedFirst OR Digits OR ASCIIChar(#_) =: & 
AllowedAfterFirst

% Making a set with special characters as members:
ASCIIChar(#&, #&, ##, #, #, #, #, #&, #(, #), #$) =: Especial

% Finding a name in a string:
Addr ' ( This_is_a_name ).' =: StringPointer

FOR FirstInName IN Ind StringPointer DO
  WHILE NOT Ind(StringPointer)(FirstInName) IN AllowedFirst
  EXITWHILE
  FOR LastInName IN FirstInName : &
    Maxindex(Ind(StringPointer)) DO
    WHILE Ind(StringPointer)(LastInName) IN &
      AllowedAfterFirst
    EXITWHILE
    -- LastInName
  ENDFOR
ENDFOR

ENDROUTINE SetUse
ENDMODULE

$EOF
```

---

## Page 55

# M8 - Records and Dynamic Allocation of Variables

A flexible type

The PLANC record is a very flexible composite-data type, in that it can contain elements of all other data types, including other records, and pointers to elements of its own type.

```
=================================================================
% M 8
%
% Records
%
% Records can contain components of any type or subtype
% that are currently known to the compiler, including other
% records, routines, and pointers to all such types.
% Records can also contain pointers to structures that
% have not been declared yet. Therefore, a record type can
% contain pointers to record variables of the same type.
=================================================================
```

**MODULE** m8

The first part of this example will be about cars. The kinds of cars we want to deal with are enumerated in the following type.

**TYPE** KindOfCar = **ENUMERATION** (CarType, BusType, TruckType)

A data structure describing a car will contain information about what kind of car it is, what it weighs, and its measurements. We want to keep firstin, firstout lists of cars (like in queues), so we add a pointer to the next car in the list to the data structure. This a type expression defining a record for such cars.

**TYPE** Car = **RECORD**

- KindOfCar: ThisCar
- **REAL**: Weight, Length, Width, Height
- Car **POINTER**: Next

**ENDRECORD**

Using this data structure, you can define variables thus:

```
Car : PoliceCar := (CarType, 1000.0, 5.3, 2.4, 1.7, NIL)
```

There is an alternative way to declare record variables which you may consider if the type expression is not strictly necessary.

**CONSTANT** MaxCars = 24

**RECORD**

- **INTEGER**: CurCars
- Car **ARRAY**: CarPark(0:MaxCars)

**ENDRECORD** : RecordVariable

A record can be used as a basis that contains character-

---

## Page 56

```
% istics common to several records types. Building on
% the basis, variants can be declared that contain
% additional components. When variant record types are used
% to make variables, the additional components will be
% located after the common components in memory.

% A bus distinguishes itself by its ability to carry passen-
% gers. It is operated by one or more crew members.

TYPE Bus = Car RECORD
  INTEGER : Passengers
  INTEGER RANGE (1:15) : Crew
ENDRECORD

% A truck has a load capacity, and the load may vary
% from trip to trip.
TYPE Truck = Car RECORD
  REAL : LoadCapacity
  BYTES : LoadDescription (0:7)
ENDRECORD

% It is common for records to be allocated and deallocated
% dynamically. Dynamically allocated records are placed in
% integer arrays, or if no such array is specified, in the
% current stack array.

% The space used by a record may vary from CPU to CPU. We
% want to be able to emulate the way cars queue up at a
% traffic light, and to make the implementation of this as
% CPU-independent as possible.

% To do this, we need information about the size of
% integers and integer pointers in addition to the size
% of the records we want to allocate. We also define a
% constant Delta which is zero if there is an integer number
% of words in the record type and one otherwise.
% (The word size on any CPU is equal to the number of bits
% in an integer of the default size.)

CONSTANT &
  BitsPerInteger = Bit_size(INTEGER),
  BitsPerPointer = Bit_size(INTEGER POINTER),
  WordsPerPointer = BitsPerPointer/BitsPerInteger,
  Delta = -1 % not defined.

% Compiler macros will be presented later, but you may want
% to notice that both compiler commands and statements may
% be used between the $MACRO ... $ENDMACRO commands, and
% that macros may have parameters that are expanded
% textually.

$MACRO SetDelta(oFAType)
```

---

## Page 57

# Technical Documentation

% Remove the symbol Delta from the compiler's symbol table:

```
KILL Delta
```

% Does the size of the data type given as parameter contain
% an integer number of words?

```
$IF (Bit_size("OFAtype") MOD BitsPerInteger) = 0 $THEN
```

% If yes, define Delta to have value zero:

```
CONSTANT Delta = 0
```

$ELSE

% If no, define Delta to have value one:

```
CONSTANT Delta = 1
```

$ENDIF

$ENDMACRO

% There will be room for ten records of the basic "car"
% record type in the queue:

```
CONSTANT CarsInQueue = 10
```

% Now, set the Delta according to the size of the "car":

```
SetDelta(Car)
% In an array used for dynamic data allocation, the first
% words are used to keep 18 different integer pointers.
% For each element in the array, there is one integer and
% one integer pointer in addition to the element itself.
% This is the basis for the following formula that gives
% the size, in integers, of an array that will have room
% for the desired number of "cars". If the size of the
% element is not an integer number of words, another
% integer is needed to make room in the array for the
% remainder, and that is where the Delta is used:

CONSTANT QueueSize = 18 * WordsPerPointer &
    + CarsInQueue * (1 + WordsPerPointer + Bit_size(Car) &
    / BitsPerInteger + Delta)
```

% Now, we declare an array that will keep the desired number
% of "cars":

```
INTEGER ARRAY: SpaceForQueue(0:QueueSize)
```

% The queue will be a single-linked list that is either
% empty or contains a certain number of cars, the maximum
% contents being for the basic "car". A pointer to the last
% car in the queue is used to add new cars at the end of
% the queue. Here is a record type containing the relevant
% data elements:

```
TYPE LinkedList = RECORD
    INTEGER: MaxNo, CurNo
    Car POINTER: FirstCar, LastCar
ENDRECORD
```

% We make a queue of cars using this type:

```
LinkedList : CarQueue := (CarsInQueue, 0, NIL, NIL)
```

% We define some integers to be used when the routines that

---

## Page 58

```
% operate on the queue cannot do their job. This may happen
% if the program tries to put more "cars" into the queue
% than there is room for, or tries to remove "cars" when the
% queue is empty, or tries to put a new "car" into the array
% when the room there has been used because some of the
% "cars" take more room than the basic "car", i.e. there
% are "trucks" or "buses" there too.

CONSTANT BadAdd = 200000B, BadRemove = 200001B,
BadNew = 200002B

% The next routine, AddCar, allocates a new car of the type
% specified as in-value in the array where the "car" records
% are kept, and returns a pointer to the new record which
% has been allocated as out-value.

ROUTINE KindOfCar, Car POINTER : AddCar
% We need a pointer to a "car" that can be returned to
% the calling routine.

% Note an important point here: This pointer can point to
% records of the type it is declared with and
% variants of it. Therefore, you do not need to declare
% three different pointers here.

Car POINTER : LocalCar

% We want an error return to the caller if it tries to
% put more "cars" into the queue than it can take under
% any circumstance.

IF CarQueue.CurNo >= CarQueue.MaxNo THEN
    BadAdd ERRETURN
ENDIF

% Furthermore, attempts to allocate data dynamically
% outside the array where it is meant to be will
% give a pointer error, which is caught
% by the following exception handler:

ON POINTERERROR DO
    BadNew ERRETURN
ENDON

% The standard routine New is used to allocate data
% elements dynamically, while the name after the
% optional IN tells in which integer array the data
% will be put. If IN is not used, the data will be put
% in the stack. Data dynamically allocated in the stack
% may be lost or overwritten due to subsequent changes
% in the routine-call hierarchy.

CASE ☐
INCASE CarType
    New Car IN SpaceForQueue =: LocalCar
INCASE BusType
    New Bus IN SpaceForQueue =: LocalCar
INCASE TruckType
```

---

## Page 59

## New Truck IN SpaceForQueue =: LocalCar

### ENDCASE

```
@ =: LocalCar.ThisCar
% Inside the USING block, access to the components of
% the records named in the list of records after USING
% keyword can be done using only the names of the
% components.
```

### USING CarQueue

```
IF firstCar = NIL THEN
  LocalCar =: FirstCar =: LastCar
ELSE
  % The standard routine Append adds a new record
  % at the end of a pointer-implied range. The
  % in-value to Append is a pointer to the record
  % to be added, while the parameter is a pointer-
  % implied range. The queue we are using here has
  % a pointer to the last member of the queue, so
  % we append new elements to the pointer implied
  % range whose first member is the record pointed
  % to by LastCar. Thus, we avoid skipping through
  % a long list of records until we find the last
  % record in the list (i.e. the "car" whose Next
  % is NIL).
  LocalCar Append LastCar:Next
  LastCar.Next =: LastCar
  NIL =: LastCar.Next
  % An alternative would be to replace the three
  % previous statements with these two:
  % LocalCar =: LastCar.Next =: LastCar
  % NIL =: LastCar.Next
  %
  % There are two companion routines to Append:
  % Insert adds a new element at the beginning of
  % a pointer-implied range, while Remove takes
  % the element pointed to by the in-value out
  % of the list.
ENDIF
```

### ENDUSING

```
++ CarQueue.Curno
LocalCar RETURN
```

### ENDROUTINE AddCar

#### ROUTINE VOID, KindOfCar : RemoveCar

```
Car POINTER : LocalCar
KindOfCar : LocalCarType
% You cannot remove an item from an empty list:
```

---

## Page 60

```
IF CarQueue.FirstCar = NIL THEN
    BadRemove ERRETURN
ENDIF

USING CarQueue
    FirstCar =: LocalCar
    LocalCar.ThisCar =: LocalCarType
    IF FirstCar < LastCar THEN
        FirstCar.Next =: FirstCar
    ELSE
        NIL =: FirstCar =: LastCar
    ENDIF
    % The standard routine Dispose removes the item
    % pointed to by the pointer named in its parameter
    % from the integer array in which it located, so
    % that the room can be used for other purposes.
    Dispose LocalCar
    -- CurNo
ENDUSING

LocalCarType RETURN
ENDROUTINE RemoveCar

% A routine that uses the records and routines we have
% declared above.
ROUTINE VOID, VOID : RecordsAndVariants
    Car POINTER : NewCar
    Bus POINTER : NewBus
    Truck POINTER : NewTruck
    KindOfCar : CurrentItem
    INTEGER : LocalInt
    % This exception handler deals with the errors we have
    % defined in the previous routines.
    ON ROUTINEERROR DO
        IF ERRCODE = BadAdd THEN
            % ...
        ELSIF ERRCODE = BadRemove THEN
            % ...
        ELSIF ERRCODE = BadNew THEN
            % ...
        ELSE
            % ...
        ENDIF
ENDON

% Putting a new "car" record into the queue.
CarType AddCar =: NewCar
% The following construction stores to all components
% of the record in one go. It takes the name of a
% known type followed by a list of constants enclosed in
% parentheses. It is stored into the record pointed
% to by the NewCar pointer.
```

---

## Page 61

# Technical Page

```plaintext
Car(CarType, 900.0, 5.5, 2.4, 1.7, NIL) := Ind NewCar
% Putting a "car" of the "bus" variant into the queue:
BusType AddCar := NewBus
% Here, you see how the values of the components added
% in the variant declaration come after the components
% of the basic record type.
Bus(BusType, 13000.0, 14.7, 3.2, 4.0, NIL, &
60, 2) := Ind NewBus

TruckType AddCar := NewTruck
Truck(TruckType, 5000.0, 8.1, 3.0, 3.7, NIL, &
3.5, 'Gravel') := Ind NewTruck
% This DO loop will remove cars from the queue until a
% truck is first.
```

## DO

```plaintext
WHILE CarQueue.CurNo > 0 &
AND CarQueue.FirstCar.ThisCar >< TruckType
RemoveCar
ENDDO
```

## ENDRROUTINE RecordsAndVariants

```plaintext
% Records can be packed, like arrays. To achieve
% reasonably fast access to the components of packed
% records on the ND-500(0), components which do not
% fit into what is left of the current word will
% have their address moved to the next free byte address.
% On ND-1x0 and MC 680x0, no simple-type components will
% straddle 16-bit word boundaries. If they might, their
% first bit will be moved to the next free word. Therefore,
% there may be some unused bits left in packed records.
TYPE ND500Word = RECORD PACKED

INTEGER UNSIGNED RANGE (0:31) : Segment
% In the next line, the standard routine Bit_size is
% used to calculate the size of the address part of
% an ND-500(0) word. If the final -1 were not added to
% the range expression, the Address part would be
% moved to the next byte, and the record would
% occupy more than one 32-bit word.

INTEGER UNSIGNED RANGE 6
(0: (2**(32-(Bit_size(Segment)))-1)) : Offset
ENDRECORD
% Making such a record and initializing it to point to one
% of the bytes on segment 1:

ND500Word n_w := (1B, 10B)
% This pointer will lie at the same address in memory as
% the record above, and point to the integer occupying
% the four bytes on segment 1 that start at the indicated
% address:
```

---

## Page 62

# Technical Page

## Code Definitions

```plaintext
INTEGER POINTER: IntPoi = nw
% How this record definition can be used:
ROUTINE VOID, VOID : UsePacked
    INTEGER: SegNo, LocOffset, WhatIsThere, BitPosOffset
    nw.Segment := SegNo
    nw.Offset := LocOffset
    Ind IntPoi := WhatIsThere
    % If you want to know precisely where a component in a
    % data structure is located, the standard routine
    % Bit_position will give you the number of the bit,
    % counting from the start of the structure.
    Bit_position Offset =: BitPosOffset
ENDROUTINE UsePacked
INTEGER ARRAY : Stack(0:127)
PROGRAM : m8Main
    INITSTACK Stack
    RecordsAndVariants
    UsePacked
ENDROUTINE m8Main
ENDMODULE
$EOF
```

## Comments

The declaration of a PLANC record can contain components that are of any type, excluding only the record type being declared. But it can contain pointers to variables of the type being declared.

Once you have declared a record, you can make variants of it. Variants first contain the components of the root record, then the components of the variant declaration.

Records can be allocated dynamically, like all other types. The standard routine `New` allocates a new variable in the integer array specified in the subsequent IN clause, or on the stack in the absence of such a clause. The out-value of `New` is a pointer to the variable that was created.

Dynamically-created variables are removed by applying the standard routine `Dispose` to pointers to the variables in question. After a variable has been disposed of, the storage it occupied previously is free to be used for other purposes.

It is usually not recommended to create variables dynamically on the stack, as subsequent routine calls may ruin the variables that were created, and result in unpredictable errors.

## Standard Routines for Dynamic Variables

[Standard routine details continue as described...]

[Photo: Copyright notice at the bottom]

---

## Page 63

The example shows how the size of the area that will receive the dynamic variables can be calculated if you are in a pinch for space.

There is an exception condition called `POINTERERROR` that can catch attempts to dynamically assign a variable that is too big to fit into the remainder of the array where it is supposed to go.

Records can be linked together to form lists. There are special standard routines and a form of `FOR` loop for handling such lists.

In lists where each member record has a pointer to the next record, the standard routine `Insert` puts a new record first in the list while `Append` puts it at the end of the list.

---

## Page 64

# Expressions

## What expressions are

An expression consists of a string of operators and operands formed according to common rules, like you will know from other languages. When the program is executed, expressions are evaluated to yield a resulting value which may be stored in a variable. A special PLANC feature is that the store (`=:`) sign and the swap (`::=`) sign are both operators, so that intermediate results in expressions may be stored too, and not just the resulting value.

## Issues related to expressions

Usually, it is very simple to write expression statements, but it may nevertheless pay to study them in some detail. Keywords are type checking, implicit type conversion, implicit dereferencing, operator and routine priorities and associativity.

## PLANC operator priorities

Operators in PLANC follow the customary pattern where priorities, operator precedence and parentheses determine how an expression is evaluated, with the exception of the store operators. Your own routines get the user priority 11 by default, but you can give them other priorities using the PRIORITY clause in the declaration - see example. This enumeration shows the operator, standard routine and user-defined routine priorities:

```plaintext
TYP E PriorType=ENUMERATION(&
& % Enumvalue operators Lowest priority
MinPri, % unused 0
PreStorePri, % := :: =: ERRETURN RETURN 1
OrPri, % OR XOR 2
AndPri, % AND 3
NotPri, % NOT 4
InPri, % Append IN Insert Remove 5
RelPri, % , < <= = >< > => 6
RelColPri, % ; 7
PlusMinusPri, % + - // SHIFT 8
MultDivPri, % * / 9
UnMinusPri, % ++ -- 10
UserPri, % ** Abs Bit Bit_position Bit_size 11
& % Blocksize Close CONVERT Dispose
& % Filesize FORCE Input Maxindex
& % Minindex MOD Monitor_call New Open
& % Output Pred Size Succ
& % your own routines without PRIORITY
& % clause or name matching an operator
& % or std. rout. with another priority
PostStorePri, % ::= =: 12
DotPri, % . 13
MaxPri) % Addr Ind 14
% Highest priority
```

---

## Page 65

# M9 - Expressions

```
%======================================================%
%                      M 9                             %
%                                                      %
%                   Expressions                        %
%======================================================%
```

**MODULE** m9

There is one exception to the common operator usage pattern: The assignment operators, which store values into variables. They are operators in the sense that they result in values after they have been applied to their operands, and they are unusual in that the lefthand side always has higher priority than the righthand side.

**ROUTINE** VOID, VOID : StoreRoutine

**INTEGER** : i, j, k, l

There are two value assignment operators: the store operator `=` and the swap operator `:=`. The store operator moves the value on its left into the storage location of the variable named on its right, and its value is the expression on the lefthand side. The swap operator also stores the value of its left operand into the variable on the right, but its value is the contents of the righthand variable before anything has been stored into it.

For example, after execution of this statement, `i` has the value 3, `j` the value 2 and `k` the value 9: 

```
(3 =: i) * (2 =: j) =: k
```

Giving `k` the value of `i` and `l` the previous value of `k`, so that `k` becomes three and `l` becomes nine:

```
i :=: k =: l
```

**ENDROUTINE** StoreRoutine

Your routines usually have a very high priority, higher than the unary minus but less than the righthand side of the store operator. However, if its name overloads the name of an operator or standard routine that exists already, it will be given the priority of that name. Furthermore, if it does not overload an operator or standard routine, you can give it the priority of any operator using the PRIORITY clause in the routine declaration. For instance, this is how you make a new routine that moves to the next month in the calendar.

---

## Page 66

# Enumeration and Operators

% and which has the same priority as the ++ operator:

**TYPE** Month = **ENUMERATION**(January, February, March, April,  
May, June, July, August, September, October, November, December)

**ROUTINE** Month, Month : NextMonth PRIORITY ++

```plaintext
    IF @ = December THEN January RETURN
    ELSE Succ @ RETURN
    ENDIF
```

**ENDROUTINE** NextMonth

% The unary ++ and -- operators are used to increment  
% or decrement the variable to its right by 1 when  
% applied to operators. (They are similar to C's ++ and  
% --, but can only be used for incrementing variables  
% before evaluation of the rest of the expression.)

% ++ and -- can be applied to integers of all kinds, and  
% to pointers. The next type and routine shows what happens  
% when using these operators with record pointers. First,  
% we make a record type:

**TYPE** Something = **RECORD**

```plaintext
    INTEGER : Int
    Something POINTER : Next
```

**ENDRECORD**

**ROUTINE VOID, VOID** : DoThing

% Next, we make an array of, and a pointer to, such  
% records, and then we make the pointer point to the  
% first of the records in the array:

```plaintext
    SomeThing ARRAY : Things(0:5)
    SomeThing POINTER : stp
    Addr(Things(0)) =: stp
    % To make the pointer point to the next record in the
    % array, do as follows:
    ++ stp
```

**ENDROUTINE** DoThing

% All components of a set, array or record can be specified  
% in one operation. You can omit components in arrays and  
% sets, while you must specify all components in a  
% record operation. This routine shows how.

**ROUTINE VOID, VOID** : CompositeStore

```plaintext
    Month SET : MusselsAreGoodIn
    INTEGER ARRAY : Months(January:December)
    BOOLEAN ARRAY : Bools(0:5)
    BYTES : Byts(0:59)
    Something : OneThing
    Something ARRAY : ManyThings(0:2)
    % You must use a type expression to describe what you
    % want to store to. You tell the compiler that what
```

---

## Page 67

```
% follows is a composite operation and not a declaration
% by enclosing it in parentheses.
(Month SET (May, June, July, August) =: MusselsAreGoodIn)
% This precaution is not necessary in the next statement.
% Also note that not all components are specified. Only
% the first five components have explicit values assigned
% to them here, the remaining ones will be set equal to
% zero.

INTEGER ARRAY (1, 2, 3, 4, 5) =: Months
% You can work on subarrays:
INTEGER ARRAY (8, 9) =: Months(August:September)
% When storing the same value in all components of an 
% array, you can omit the type expression. The following
% statement will set all elements equal to zero:
(0) =: Months
% When storing to a Boolean array, unspecified components
% will become FALSE.

BOOLEAN ARRAY (TRUE, FALSE, TRUE) =: Bools
% Setting all components to TRUE:
(TRUE) =: Bools

% BYTES are somewhat special. A sequence of integer
% values separated by commas can be replaced with a
% string of printable ASCII characters enclosed in
% double quotation marks. This sets the first four
% bytes of the righthand string, while zeroing the
% remaining bytes:
BYTES 'ABCD' =: Byts
% This does the same thing:
'ABCD' =: Byts
% Here is how to fill the Byts with blanks
% (40B = 20H = # = ASCII space).
(# ) =: Byts
% An alternative way to give any value to any substring
% of the string:
BYTES (40B, #&, 16#7F#, 15B) =: Byts(4:7)
% or:
'ABCD' =: Byts(8:11)
% Here is an operator that concatenates two strings of
% bytes:
Byts(8:11) // Byts(0:3) =: Byts(12:19)

% When doing a block store to a record, all components
% must be specified:
Something(5, NIL) =: OneThing
% To round it off, we do a block store on an array of
% records. The third element in the array will get the
% value (0, NIL) since it is not initialized:
```

---

## Page 68

# Something Array

```plaintext
ARRAY ((1, NIL), (2, Addr(OneThing))) & 
=: ManyThings
```

## ENDROUTINE CompositeStore

% Sometimes, it is necessary to work on bits in data
% structures. This can be done with the Bit standard
% routine. (Another way to do bit operations would
% be to equivalence the variable in question with a
% BITS array or to FORCE a BITS POINTER to its address.)

### ROUTINE VOID, VOID : TwiddleTheBits

```plaintext
INTEGER : i, j
BOOLEAN : SignBit
% Setting all bits in i except the sign bit:
2#01111111_11111111_11111111_1111111# =: i
% The Bit routine has two variants, one with a Boolean
% in-value but no out-value that sets a bit to one if
% the in-value is TRUE and to zero if the in-value is
% FALSE, and one with no in-value but a Boolean out-value
% that returns TRUE if the bit is one and FALSE if it is
% ZERO. Here are some bit operations:
IF NOT (Bit(i, Bit_size(i)-1) =: SignBit) THEN
    FOR j IN 0:Bit_size(i)-1 DO
        Bit(j,0) =: Bit(i,j)
    ENDFOR
ELSE
    TRUE =: Bit(i, Bit_size(i)-1)
ENDIF
ENDROUTINE TwiddleTheBits
```

% Arithmetical and logical operators such as
% OR, XOR, AND, NOT, =:, :!=:, <, >, <=, >=, =, ><, +, -,
% * and / all have the same types as left and right
% operands. The out-value from these operations will always
% have the same size as the largest of the two operands,
% and an implicit CONVERT will be applied to the smaller
% of the two operands to the type of the larger operand.
% Thus, if you multiply an INTEGER2 with an INTEGER2, the
% out-value will be an INTEGER2, and the out-value from
% the multiplication of an INTEGER1 with an INTEGER4 will
% be an INTEGER4, with an implicit conversion of the
% INTEGER1 to INTEGER4 before the multiplication.

% In the case of multiplication of two INTEGER2s,
% significant bits in the result may be lost as a conse-
% quence of the rules cited above. The following routine
% illustrates the point.

---

## Page 69

# ImplicitTypeConversion

## Routine Void, Void: ImplicitTypeConversion

**INTEGER2**: i, j, k; **INTEGER**: kk

- Setting i and j to a value that will cause overflow on multiplication.

```
16#01_FF# =: i =: j
```

- Now, i * j should be 16#03_FC_01# if the most significant bits of the product were not discarded in order to make the result fit into an INTEGER2. However, now an negative value will be stored into k.

```
i * j =: k
```

- Since the out-value will be INTEGER2, an implicit conversion to the type of kk, which has more bits in it than the out-value, will be done after the multiplication. However, the most significant bits will still be missing:

```
i * j =: kk
```

- What must be done to get a correct result from the multiplication is to convert the out-value to become the same type as kk:

```
CONVERT Typeof(kk) * i =: kk
```

## Endroutine ImplicitTypeConversion

- Type checking follows the same basic pattern, regardless of whether it is an operator, a standard routine or a declared routine that is applied on the operands/values/parameters.

- The pattern is that the routine first looks up the symbol of the operator/routine. If more than one version exists, it is overloaded. If it is overloaded, it looks for an overloaded routine that fits the context in the source code. In some cases, it will also choose a best match among several alternatives.

- An operator/routine may be overloaded because it is defined as such in the compiler, because you have declared more than one routine with the same name, or a combination, i.e. you have declared a routine with a name known to the compiler already.

- Overloading is used in many computer languages. For example, integers and reals are different types, so different code is used to add two reals and two integers. So the meaning of the + operator is overloaded: It means add two integers in one context, and add two reals in another. And it may be overloaded further, to allow addition of a real with an integer and an integer with a real.

---

## Page 70

# PLANC Overloading

What is special in PLANC, is that you are allowed to extend the set of combinations that may include the `+` operator by declaring new routines called `+`. Likewise, you may overload any other operator or routine. 

Type checking in PLANC is the same as verifying that one combination of operands and operator or a combination of in-value, parameter(s) and routine name exists in the compiler's tables that is compatible with the types used in the expression being read by the compiler.

The following three routines have the same name, thus they are overloaded. Note a point here: The number of parameters in the parameter list must always be the same in overloaded routines, but the types of the in-value, the out-value and the different parameters may vary from declaration to declaration.

## Overloaded Routines

**Overloaded routine no. 1:**

```
ROUTINE INTEGER, VOID (INTEGER : i) : OvlRout
ENDROUTINE OvlRout
```

**Overloaded routine no. 2:**

```
ROUTINE VOID, INTEGER (INTEGER1 : i) : OvlRout
   i+1 RETURN
ENDROUTINE OvlRout
```

**Overloaded routine no. 3:**

```
ROUTINE VOID, INTEGER (INTEGER : i) : OvlRout
   0 RETURN
ENDROUTINE OvlRout
```

## Example Routine

```
ROUTINE VOID, VOID : RoutineOverloads
   INTEGER ARRAY : Arr(0:3)
   Something : Here
   (INTEGER ARRAY (0,1,2,3) =: Arr)
   Something(5, NIL) =: Here
```

In the next statement, the first call to OvlRout has no in-value, so the compiler can identify it as a call to overloaded routine no. 3, with void in-value and integer parameter. This routine has an integer out-value, which is in-value to the second call to OvlRout. Therefore, the compiler identifies it as a call to overloaded routine 1. This routine has no out-value, hence the absence of an assignment.

---

## Page 71

# Overloaded Operator

## OvlRout 2

A trickier version: The parameter is converted to be an `INTEGER1`. This makes a reasonable yet not perfect call to overloaded routine no. 1, but since the parameter in the call is a different integer subtype, the compiler looks further for an overloaded routine that has the appropriate parameter type. It finds this in overloaded routine no. 2, and therefore overloaded routine no. 2 is the one that will be called.

## OvlRout (2 CONVERT INTEGER1) OvlRout 2

In the case of components of composite types being used as values, the type check is done on the components. Here is another call to routines 3 and 1. (Also note that since the dot (.) operator has higher priority than user-defined routines, it is not necessary to enclose the component access expression in parentheses.)

## OvlRout Here.Int OvlRout Arr(3)

## ENDROUTINE RoutineOverloads

An additional point concerns record types. If a routine has a record as in-value/parameter, all variants of that record are acceptable in its place - it has all the components that the routine may require. However, if the record is a variant of a root type, the root type cannot be used. Here is an example:

```plain
TYPE RootType = RECORD
  INTEGER : RootInt
ENDRECORD

TYPE Variant1 = RootType RECORD
  INTEGER : Var1Int
ENDRECORD

TYPE Variant11 = Variant1 RECORD
  INTEGER : Var11Int
ENDRECORD

TYPE Variant2 = RootType RECORD
  INTEGER : Var2Int
ENDRECORD
```

## Overloaded routine no. 4:

```plain
ROUTINE Variant1, INTEGER (INTEGER : i) : OvlRout
  .RootInt * .Var1Int RETURN
ENDROUTINE OvlRout

ROUTINE VOID, VOID : Overloads
  The following variable cannot be used as in-value to overloaded routine 4:
```

[Scanned by Jonny Oddene for Sintran Data © 2021]

---

## Page 72

```
RootType : Root
% ... but this one is of the right type for routine 4:
Variant1 : Var1
% ... this one is a variant of the right type for 
% routine 4:
Variant11 : Var11
(RootType (0) =: Root)
Variant1 (1, 1) =: Var1
Variant11 (1, 11, 20) =: Var11
% This is a call to overloaded routine no. 4:
Var1 OvlRout 5 =: Var11.Var11Int
% ... as is this, since Var11 is a variant of Var1:
Var11 OvlRout 9 =: Var11.Var11Int

ENDROUTINE Overloads

ROUTINE VOID, BYTE (BYTE : ParByte) : InLineAssembly

BYTE : LocalByte
% Here is some inline assembly code. The following conditional 
% compilation commands (about which you will learn more in 
% another example) make the compiler pick inline assembly 
% code that is correct for the CPU your program will run on.

$IF $TARGET-MACHINE = 500 $THEN
$* BY1 := ParByte
$* BY1 AND 177B
$* BY1 =: LocalByte
$ELSIF $TARGET-MACHINE = 68000 $THEN
$* MOVE.B ParByte,D1
$* AND.B #177B,D1
$* MOVE.B D1,LocalByte
$ELSIF $TARGET-MACHINE = 186 OR $TARGET-MACHINE = 386 $THEN
% NOTE: This inline assembly works on Intel-80186, Intel-80286 and
% Intel-80386 only!
$* MOV AL,ParByte
$* AND AL,177B
$* MOV LocalByte,AL
$ELSIF $TARGET-MACHINE = 100 $THEN
$* LDA ParByte
$* SAT 177
$* RAND ST DA
$* STA LocalByte
$ENDIF

LocalByte RETURN

ENDROUTINE InLineAssembly

INTEGER ARRAY : Stack(0:1023)

PROGRAM : Expressions

BYTE : b

Month : aMonth
```

---

## Page 73

# Using Routines

```
INISTACK Stack
StoreRoutine
December NextMonth =: aMonth
DoThing 
CompositeStore
TwiddleTheBits
ImplicitTypeConversion
RoutineOverloads
Overloads 
InLineAssembly 213B =: b

ENDROUTINE 
ENDMODULE 
$EOF
```

## There is more to routines

You will know quite a lot about routines already. Since all expressions must be written inside a routine block, all examples that you have seen so far in the manual have contained routines. But there are still a few things to learn. In this section, you will learn how to nest routines, to make recursive routines, and to predeclare routines and other data structures. A few more topics are discussed in later sections, such as routine modifiers (allowing routine-body substitution into the compiled code, special call sequences and calls to other languages), using routine pointers, and object-oriented programming.

---

## Page 74

# M10 - Using routines

## Ordinary routines

The purpose of the following example is to demonstrate how routines are used in ordinary PLANC programs. The next section will show some special routine types that can be used both inside PLANC programs and in calls to/from code written in other languages.

```
%===============================================================%
% M10                                                          %
%                                                              %
% Routine usage                                                %
%===============================================================%

MODULE m10
% Routine declarations can be nested to any level. Note,
% however, that routines on inner-nesting levels cannot
% be recursive (see below).
%
% This routine contains two nested routines, of which the
% first nested routine contains a nested routine that calls
% the next routine to be declared.

ROUTINE VOID, VOID : Lev0
% The routine Lev1 contains Lev2, which calls a routine
% which has not been declared yet, namely Lev12. Hence,
% Lev12 must be predeclared so that Lev2 knows how to
% call it.

ROUTINE VOID, VOID : Lev12 ?
ROUTINE VOID, VOID : Lev1

  ROUTINE VOID, VOID : Lev2
    Lev12

  ENDROUTINE Lev2
    Lev2

ENDROUTINE Lev1
BOOLEAN : Bool12
  TRUE =: Bool12

ROUTINE VOID, VOID : Lev12
  Lev1
  Lev12

ENDROUTINE Lev12

ENDROUTINE Lev0

% You can easily write recursive routines in PLANC. Here,
% we want to show you how to program recursive routines
% to work with a binary tree. The tree nodes will have
% a single byte as a key, and the tree will be built
% so keys of a lesser ASCII value than the key of the
% current node will be found in the lefthand branch of the
% tree while bigger values are in the righthand branch of
```

---

## Page 75

# Technical Document on Binary Trees

- The tree. In other words, the tree will be sorted.

## TYPE Node = RECORD

- **BYTE**: Key
- **Node POINTER**: Left, Right

## ENDRECORD

- This will be a pointer to the root node of the tree:

```
Node POINTER: Root := NIL
```

- All nodes will be placed in the following array:

```
INTEGER ARRAY: NodeSpace(0:2047)
```

- The following routine adds a new key to the tree, provided the key given as in-value does not duplicate a key that is in the tree already. It is convenient to make this a recursive routine:
  - Inserting a value into the tree whose root is the current node is equivalent to inserting it in either the left or the right subtree, depending on its size.

- Recursion stops on two conditions: If the current node pointer is NIL, a new node must be made to hold the key given as in-value before the routines; if the in-value is equal to the key of the current node, then return without making any new node.

- **Important**: While routines may be nested to any level, only routines on the outermost module level can be recursive. (If you make an inner-level routine recursive, the compiler will not complain, but most likely, the program will fail nevertheless.)

## ROUTINE BYTE, VOID (Node POINTER: ThisNode): AddKey

**USING** ThisNode

- Make the first node in the tree:

```
IF Root = NIL THEN
  New Node IN NodeSpace =: Root
  @ =: Root.Key
  NIL =: Root.Left =: Root.Right
  RETURN
ELSIF @ = Key THEN
  RETURN
ELSIF @ < Key THEN
```

- Always set the Left and Right pointers to NIL before proceeding, otherwise one of the halting conditions for recursion may not hold:

  - **RETURN**: Halt if the current key exists in the tree.

  - If the in-value is less than the current key, go to the left subtree:

```
@ < Key THEN
```

- Is the in-value absent from the tree? If so, insert the new node and return.

---

## Page 76

# Node Insertion Routine

```plaintext
IF Left = NIL THEN
    % We put new nodes in the NodeSpace - the stack
    % is not a safe place right now with new stack
    % frames being allocated and deallocated as
    % routine calls are made and finished.
    New Node IN NodeSpace =: Left
    NIL =: Left.Left =: Left.Right
    Θ =: Left.Key
    % Left was not NIL, so we do a recursive call
    % with the same in-value, but with the left
    % subtree as the root of the tree being
    % inserted into.
ELSE
    Θ AddKey Left
ENDIF
    % Repeat the process for the right subtree.
ELSIF Θ > Key THEN
    IF Right = NIL THEN
        New Node IN NodeSpace =: Right
        NIL =: Right.Left =: Right.Right
        Θ =: Right.Key
    ELSE
        Θ AddKey Right
    ENDIF
ENDIF
ENDUSING
```

## EndRoutine AddKey

% The next couple of arrays hold an unsorted sequence of
% keys and, eventually, a sorted array.

```plaintext
BYTES : Keys(0:79) := (# ), SortedKeys(0:79) := (# )
% The following routine does an "infix traverse" of
% the key tree using recursion: First, write the sorted
% sequence of the left subtree, then write the key of
% the current node, then write the sorted sequence of the
% right subtree. Recursion halts if the pointer to the
% current node is NIL.
```

% By default, variables passed to routines have WRITE
% access inside the routine. The default can be over-
% ridden in the routine declaration - see the sections
% on declaration of simple and composite variables.

% A variable holding the position of the key currently
% written into the SortedKeys is necessary. It might be
% tempting to make it global, but that can be avoided
% by making it a parameter to the routine with read and
% write access.

```plaintext
ROUTINE VOID, VOID
```

---

## Page 77

```
(Node POINTER : ThisNode; &
 INTEGER READ WRITE : Pos ) &
: SortSequence  
IF ThisNode = NIL THEN  
    RETURN  
ELSE  
    USING ThisNode  
        SortSequence(Left, Pos).  
        Key =: SortedKeys(++Pos)  
        SortSequence(Right, Pos)  
    ENDUSING  
ENDIF  
ENDROUTINE SortSequence  
% Predeclaration of routines is necessary in case two or
% more routines make mutual calls to each other. Here 
% come two routines that measure the heights of the left 
% and the right subtree, respectively. Since the height 
% of the left subtree depends on which of the left node's 
% right or left trees is highest, the routine LeftHeight 
% must know how to call the routine RightHeight, so the 
% latter routine must be predeclared. 
%
% A predeclaration is a type expression followed by a 
% question mark.  
ROUTINE Node POINTER, INTEGER : RightHeight ?  
ROUTINE Node POINTER, INTEGER : LeftHeight  
    INTEGER : lh, rh  
    IF @.Left = NIL THEN  
        0 RETURN  
    ELSE  
        @.Left LeftHeight =: lh  
        @.Left RightHeight =: rh  
        IF lh > rh THEN lh+1 RETURN ELSE rh+1 RETURN ENDIF  
    ENDIF  
ENDROUTINE LeftHeight  
ROUTINE Node POINTER, INTEGER : RightHeight  
    INTEGER : lh, rh  
    IF @.Right = NIL THEN  
        0 RETURN  
    ELSE  
        @.Right LeftHeight =: lh  
        @.Right RightHeight =: rh  
        IF lh > rh THEN lh+1 RETURN ELSE rh+1 RETURN ENDIF  
    ENDIF  
```

---

## Page 78

```
ENDIF

ENDROUTINE RightHeight
% It might be tempting to declare the two previous routines
% inside the next routine, which measures the height of any
% binary tree of Nodes. However, since they are recursive,
% this must be avoided.

ROUTINE VOID, INTEGER : TreeHeight
INTEGER : hl, hr
Root LeftHeight =: hl
Root RightHeight =: hr
IF hr > hl THEN hr RETURN ELSE hl RETURN ENDIF

ENDROUTINE TreeHeight
% To round off, let us define a factorial function.
% ASCII characters such as +, -, *, ! etc. can be 
% used as routine names, in addition to names formed
% with characters and numbers, so the routine will
% be given the logical name !. It will be recursive,
% using the property that the factorial of a number
% equals the number multiplied with the factorial of the
% number minus one. Recursion stops when the in-value
% to the routine is less than or equal to 1, in which
% case 1 is returned.

ROUTINE INTEGER, INTEGER : !
IF Q <= 1 THEN
   1 RETURN
ELSE
   % This is the recursive call. Note the spaces on
   % the right side of the routine name. This is neces-
   % sary to avoid making a routine whose name is '! ' 
   (Q-1) ! * Q RETURN 
ENDIF

ENDROUTINE !

INTEGER ARRAY : Stack(0:1023)

PROGRAM : m10
INTEGER : i, SortPos
INITSTACK stack
% Defining a suitable sequence of bytes for a binary
% tree:
'qwertyuiopasdfghjklzxcvbnm' =: Keys
% Build a tree with the Root pointer as the root and
% the Keys as key values:
FOR i IN Keys DO
   Keys(i) AddKey Root
ENDIF

% Write a sorted list of keys into the SortedKeys array:
MinIndex(SortedKeys) - 1 =: SortPos
```

---

## Page 79

```
SortSequence(Root, SortPos)
% Find the height of the tree:
TreeHeight := i
% Finally, calculate the factorial of six:
6! := i
ENDROUTINE m10
ENDMODULE
$EOF
```

---

## Page 80

# M11 - Routine Modifiers

## Making Contact with the Outside World, and More

There are a number of different modifiers to the PLANC routine declaration, catering to the special needs of PLANC programmers and making PLANC code accessible to and from code written in other languages. PLANC routines can be modified to match routines written in other programming languages such as C, COBOL, and FORTRAN. Routines which are going to be used outside the module where they are declared must be exported from the module. This theme will be elaborated further in the section about modules.

```
╔═══════════════════════════════════════════════════════════════════════╗
║                               M11                                    ║
║                                                                       ║
║                         Routine Modifiers                             ║
╚═══════════════════════════════════════════════════════════════════════╝

MODULE m11
% Routines that are going to be accessible to other PLANC
% modules or code in other languages must be made known
% outside the current module with EXPORT statements. If
% variables or routines are going to be known with names
% that are not valid PLANC identifiers (such as library
% routines whose external name begin with a digit, as
% in 5LEAVE) or because the PLANC identifier is not a valid
% identifier in the language where it is going to be used
% (such as the PLANC routine name +++), then it must be
% redefined with an ALIAS clause in the EXPORT statement.

EXPORT Rout ALIAS 'SNOPAR'
% In the case of routines that have C's call sequence, they
% must be known outside the PLANC module with a name that
% begins with an underscore, which is standard for all C
% functions names.

EXPORT RoutC ALIAS '_routc'
% The next routine can be called from COBOL and FORTRAN, see
% the declaration later in this module.

EXPORT RoutStandard

INTEGER ARRAY : Stack(0:1023), RoutineStack(0:127)
% The PLANC routine declaration can employ several modifiers
% to mold the way the routine is called and executed to
% different circumstances and needs. The ordinary,
% unmodified routine call puts a new routine frame on the
% current stack, saves registers from the calling routine
% and puts the parameters on the stack before jumping to
% the called routine. On return, all registers are restored.
```

---

## Page 81

```
% All simple variables used as parameters have default access
% modification READ, while composite parameters are
% transmitted to the called routine as pointers to save
% execution time and stack space. The pointers have READ
% access, while the components of composite-data types will
% be modified by the routine as if they had READ WRITE
% access.
%
% Here is an ordinary PLAN C routine. It removes the parity
% bit from the byte that is given as parameter.
ROUTINE VOID, BYTE (BYTE : Char) : Rout
   Char AND 177B RETURN
ENDROUTINE Rout

% In this routine, the variable is READ modified. (This is
% usually unnecessary, since the default access mode is
% READ.)
ROUTINE VOID, BYTE (BYTE READ : Char) : RoutRead
   Char AND 177B RETURN
ENDROUTINE RoutRead

% If the variable is WRITE modified only, it cannot be read
% from. Hence Char cannot be used as input to the RETURN
% statement.
ROUTINE VOID, BYTE (BYTE WRITE : Char) : RoutWrite
   177B =: Char; 177B RETURN
ENDROUTINE RoutWrite

% Both READ and WRITE access means that changes done to
% the parameter inside the routine will be in effect after
% exit from the routine.
ROUTINE VOID, BYTE (BYTE READ WRITE : Char) : RoutReadWrite
   (Char AND 177B =: Char) RETURN
ENDROUTINE RoutReadWrite

% INLINE routines have no object code generated by the
% compiler. Instead, the entire routine code is inserted
% once every time it is called, giving larger programs but
% faster code.
ROUTINE INLINE VOID, BYTE (BYTE : Char) : RoutInline
   Char AND 177B RETURN
ENDROUTINE RoutInline

% The modifiers C and NATIVE are equivalent, the latter
% being retained for historical reasons. They make
% routines with the same call structure as C routines, so
% that the following routine can be called from C programs,
% see the EXPORT statement at the beginning of this module.
```

---

## Page 82

# Technical Documentation

However, to avoid code from the two languages messing up each other's code, it is wise to let the PLANC routine use its own stack. Furthermore, remember that in C routines there is no in-value!

## ROUTINE C VOID, BYTE (BYTE : Char) : RoutC

```
INISTACK RoutineStack
Char AND 177B RETURN
ENDROUTINE RoutC
```

The STANDARD modifier is for calling from the "standard" languages COBOL and FORTRAN. The same consideration for stacks is valid, and routines in these languages do not have any in-value either. STANDARD routines have the addresses of parameters transferred to them instead of copies of the variables themselves. The dimensions and sizes of arrays are not transmitted, and you cannot use ERRETURN in them.

## ROUTINE STANDARD VOID, BYTE (BYTE : Char) : RoutStandard

```
INISTACK RoutineStack
Char AND 177B RETURN
ENDROUTINE RoutStandard
```

REFERENCE routines are similar to STANDARD routines in that they have parameters transferred by address, but otherwise they are like ordinary PLANC routines with in-value, array dimensions and ERRETURN.

## ROUTINE REFERENCE VOID, BYTE (BYTE : Char) : RoutReference

```
IF Char = 0 THEN 177 ERRETURN ENDIF
Char AND 177B RETURN
ENDROUTINE RoutReference
```

SPECIAL routines assume the programmer supplies the entry and exit sequences. They are the domain of "real programmers"!

## ROUTINE SPECIAL BYTE, BYTE : RoutSpecial

The following conditional compilation adapts the inline assembly of the routine to the CPU being compiled for:

```
$IF $TARGET-MACHINE = 500 $THEN
    $* ENT0
    $* BY1 AND 177B
    $* RET0
$ELSIF $TARGET-MACHINE = 68000 $THEN
    $* AND.B #177B,D0
    $* ADDQ.L #2,(A7)
    $* RTS
$ELSIF $TARGET-MACHINE = 186 $THEN
    $* AND AL,177B
```

---

## Page 83

```plaintext
$* CLC
$* RET L

$ELSIF $TARGET-MACHINE = 386 $THEN
  $* AND AL,177B
  $* XOR ECX,ECX
  $* RET

$ELSIF $TARGET-MACHINE = 100 $THEN
  $* SAT 177
  $* RAND ST DA
  $* EXIT AD1

$ELSIF $TARGET-MACHINE = 88000 $THEN
  $* AND R2,R2,177B
  $* JMP R1
$ENDIF

ENDROUTINE RoutSpecial
% Another dangerous practice. The address of this routine will be
% equivalent to that of the next, so that you can execute the same
% routine code with different values/parameters.
ROUTINE SPECIAL VOID, INTEGER1 (INTEGER1 : param) : RoutVerySpecial
ENDROUTINE RoutVerySpecial
%

ROUTINE VOID, BYTE (BYTE : Char) : RoutHelpVerySpecial
  Char AND 177B RETURN
ENDROUTINE RoutHelpVerySpecial
% The modifier MAINSTART makes a main entry point for
% a program that can access the command line in UNIX
% and MS/DOS.
CONSTANT Unix = FALSE, MsDos = FALSE
$IF Unix $THEN
ROUTINE mainstart VOID, VOID &
  (INTEGER : Argc, BYTE POINTER POINTER : Argv) : mli
$ELSIF MsDos $THEN
ROUTINE mainstart VOID, VOID &
  (INTEGER : Argc, BYTE POINTER POINTER : Argv) : mli
$ELSE
PROGRAM : mli
$ENDIF

IMPLICIT Stack
INTEGER1 : Int1, Int2
BYTE : Ch1, Ch2
BOOLEAN : errors
```

---

## Page 84

```
FALSE =:Errors
-9 =:Int1; 0 =:Int2
367B =:Ch1; 0 =:Ch2
Rout Ch1 =:Ch2
(Ch2><167B) OR Errors =:Errors
367B =:Ch1; 0 =:Ch2
RoutRead Ch1 =:Ch2
(Ch2><167B) OR Errors =:Errors
367B =:Ch1; 0 =:Ch2
RoutWrite Ch1 =:Ch2
(Ch1><177B) OR (Ch2><177B) OR Errors =:Errors
367B =:Ch1; 0 =:Ch2
RoutReadWrite Ch1 =:Ch2
(Ch2><167B) OR Errors =:Errors
367B =:Ch1; 0 =:Ch2
RouteReadWrite Ch1 =:Ch2
(Ch1><177B) OR (Ch2><177B), OR Errors =:Errors
367B =:Ch1; 0 =:Ch2
RouteReference Ch1 =:Ch2
(Ch2><167B) OR Errors =:Errors
367B =:Ch1; 0 =:Ch2
RoutInline Ch1 =:Ch2
(Ch2><167B) OR Errors =:Errors
367B =:Ch1; 0 =:Ch2
Ch1 RoutSpecial =:Ch2
(Ch2><167B) OR Errors =:Errors
367B =:Ch1; 0 =:Ch2
RoutHelpVerySpecial(Ch1) =:Ch2
(Ch2><167B) OR Errors =:Errors
RoutVerySpecial(Int1) =:Int2
(Ch2><167B) OR Errors =:Errors

ENDROUTINE
ENDMODULE
$EOF
```

---

## Page 85

# M12 - Routine Pointers

## The routine is a data structure

The routine is a composite-data type and can be pointed to like other types. Hence, it is possible to invoke as a routine the routine that a pointer currently points to.

The example here simulates the states of a traffic light by making a routine for each state change and a pointer which each such routine sets to point to the next state change.

```
%========================================================%
% M 1 2                                                  %
%                                                        %
% Routine pointers                                       %
%========================================================%

MODULE m12
BYTES: CrLf(0:1) := (15b, 12b)
INTEGER ARRAY: Stack (0:127)
TYPE Colors = ENUMERATION (Red, Yellow, Green)
BOOLEAN ARRAY: Lit(Red:Green) := [FALSE]
BYTES ARRAY: Light(Red:Green,0:6) := &
  ('red  ', 'yellow ', 'green ')
% The next routine writes which colours are currently lit:
ROUTINE VOID, VOID: WriteState
  Colors: c
  FOR c IN Red:Green DO
    IF Lit(c) THEN Output(1, 'a', Light(c)) ENDIF
  ENDFOR
  Output(1, 'a', CrLf)
ENDROUTINE

% Since a routine is a data type, you can use it in type
% statements such as this one:
TYPE vv = ROUTINE VOID, VOID
% Predeclaring all state changes, so that all state change
% routines can set a routine pointer to all other state
% changes:
vv : SwitchToRed ?
vv : SwitchToYellow ?
vv : SwitchToGreen ?
% Now, make a pointer to variables of the type vv:
vv POINTER: Next := Addr SwitchToRed
```

---

## Page 86

% Then follows the state change routines.

### ROUTINE VOID, VOID : SwitchToRed
```
TRUE  =: Lit(Red)
FALSE =: Lit(Yellow)
```

```
%===============================================================%
%                           IMPORTANT:                          %
%                                                               %
% By convention, you must give the routine name inside          %
% parentheses if you want the address of the out-value,         %
% without parentheses if you want the address of the            %
% routine itself.                                               %
%                                                               %
% It is the latter option that is used if you want to           %
% call a routine whose address you have in a pointer.           %
%===============================================================%
```

```
Addr SwitchToYellow =: Next
WriteState
ENDROUTINE
```

### ROUTINE VOID, VOID : SwitchToYellow
```
TRUE  =: Lit(Yellow)
IF Lit(Red) THEN
    Addr SwitchToGreen =: Next
ELSE
    FALSE =: Lit(Green)
    Addr SwitchToRed =: Next
ENDIF
WriteState
ENDROUTINE
```

### ROUTINE VOID, VOID : SwitchToGreen
```
TRUE  =: Lit(Green)
FALSE =: Lit(Yellow) =: Lit(Red)
Addr SwitchToYellow =: Next
WriteState
ENDROUTINE
```

### PROGRAM : ml2
```
INTEGER : i
INITSTACK Stack
0 =: i
DO
    % This is how the routines are invoked via pointers.
    Ind Next
WHILE ++i < 20
ENDDO
ENDROUTINE ml2
ENDMODULE
```

```
$EOF
```

---

## Page 87

# M13 - Routines in Records and Object-Oriented Programming

## Routines in Records Work Fine

PLANC records can contain components that are routine data types. Such routine components can be called using the dot notation. The code inside a routine block executes as if it were surrounded by a `USING record name ... ENDUSING` block.

This module displays how routines as part of records in PLANC can be used together with variant records. The example simulates a carwash, where cars arrive at random and queue up for their turn. It is much inspired by a similar example in SIMULA.

```plaintext
%==============================================================%
% M13                                                          %
%                                                              %
% Object-oriented programming.                                 %
%==============================================================%

CONSTANT Infinity = 1.0E75

MODULE m13

IMPORT ( ROUTINE VOID, VOID : utRandom )

IMPORT ( ROUTINE VOID, REAL : utRnd )

INTEGER ARRAY : Stack(0:5127), Space(0:5127)

% Record components will be inherited by variants, but they   %
% may be redefined by the variants. To find the right         %
% version of a component that has been redefined, we must     %
% keep track of the "genes" of the variants.                  %

TYPE Gene = ENUMERATION (LinkageRec, LinkRec, HeadRec, 
    ProcessRec, CarWashRec, MakeCustRec)

% Linkage is in fact the prototype record for all later      %
% records types. That is, all record types can be inserted   %
% in a double-linked list.                                   %

TYPE Linkage = RECORD
    Gene : RecordType
    Linkage POINTER : Previous, Next
ENDRECORD

% Head is a record type that is used to head all linked       %
% lists. It has three ROUTINE components:                     %
% - One which initializes an empty list (i.e. makes the      %
%   list pointers point to the head itself) and sets the     %
%   "gene" to HeadRec.                                       %
% - A ROUTINE which returns the number of elements in the    %
%   queue.                                                   %
% - Another that tells you if there is anything in the       %
```

[Scanned by Jonny Oddene for Sintran Data © 2021]

---

## Page 88

# Documentation: Queue Management

**TYPE** Head = Linkage **RECORD**

**ROUTINE** VOID, VOID : InitiateHead  
   THISRECORD =: Next =: Previous  
   HeadRec =: RecordType  
**ENDROUTINE**

**ROUTINE** VOID, **INTEGER** : Cardinal  
   **INTEGER** : i  
   Linkage **POINTER** : currentLinkage  
   0 =: i  
   *Next =: CurrentLinkage  

   **DO**  
   **WHILE** CurrentLinkage.RecordType >< HeadRec  
      CurrentLinkage.Next =: CurrentLinkage  
      ++ i  
   **ENDDO**  

   i **RETURN**  
**ENDROUTINE**

**ROUTINE** VOID, **BOOLEAN** : Empty  
   (Next = THISRECORD) **RETURN**  
**ENDROUTINE**

**ENDRECORD**

% Link is the basic type for list members. It contains  
% ROUTINEs for  
% - being removed from list  
% - insertion first or last in list  
% - insertion before or after other list members  

**TYPE** Link = Linkage **RECORD**

**ROUTINE** VOID, VOID : Out  
   **IF** (Next = **NIL**) **KOR** (Previous = **NIL**) **THEN**  
      Output(1,'a','$Pointers screwed up!$')  
   **ELSIF** Next >< **NIL** **THEN**  
      Next =: Previous.Next  
      Previous =: Next.Previous  
      **NIL** =: Previous =: Next  
   **ENDIF**  
**ENDROUTINE**

**ROUTINE** VOID, VOID (Link **POINTER**) : Follow(ThisLink)  
   Out  
   ThisLink.Next =: Next  
   ThisLink =: Previous  
   THISRECORD =: ThisLink.Next =: Next.Previous  
**ENDROUTINE**

---

## Page 89

# Routine Definitions

## Routine VOID, VOID (Link POINTER) : Precede (ThisLink)

```
Out
    ThisLink.Previous =: Previous
    ThisLink =: Next
    THISRECORD =: ThisLink.Previous =: Previous.Next
ENDROUTINE
```

## Routine VOID, VOID (Head) : IntoStart (ThisHead)

```
Out
    ThisHead.Next =: Next
    Addr(ThisHead) =: Previous
    THISRECORD =: Next.Previous =: ThisHead.Next
ENDROUTINE
```

## Routine VOID, VOID (Head) : IntoEnd (ThisHead)

```
Out
    ThisHead.Previous =: Previous
    Addr(ThisHead) =: Next
    THISRECORD =: Previous.Next =: ThisHead.Previous
ENDROUTINE
```

# End Record

```
% A special variant of Head is needed to keep events in.
% The variant contains a variable showing the last time
% an event occurred.
```

## Type TimeHead = Head RECORD

```
REAL : Time
ENDRECORD
```

- The list of events

## TimeHead : EventQueue

```
% Here, a variant of Link is made for events that can
% occur. The variant is called Process, and it has a real
% variable that shows when it is due to be activated.
% Activation is done by the ROUTINE called Activate.
% If there is nothing for the process to do, it can be
% Passivated, i.e. activated at an infinitely later time.
% A test for whether or not the routine is passive is
% included, and the process has a Body telling you when
% activation takes place.
```

## Type Process = Link RECORD

```
REAL : ActivationTime
```

### Routine VOID, VOID (REAL) : Activate (NewTime)

```
    Link POINTER : l
    NewTime =: ActivationTime
    IF ActivationTime >= EventQueue.Time THEN
        IF (EventQueue.Empty) &
           OR (ActivationTime < EventQueue.Next.ActivationTime) THEN
               IntoStart(EventQueue)
    ELSIF (ActivationTime = Infinity) OR &
```

---

## Page 90

```
(ActivationTime >= EventQueue.Previous.ActivationTime) &  
THEN  
    IntoEnd(EventQueue)  
ELSIF THISRECORD = EventQueue.Next THEN  
    % Nothing  
ELSE  
    EventQueue.Next =: l  
    % This DO loop will activate the current  
    % process after other processes with the  
    % same activation time. You may want to change  
    % this strategy.  
    DO  
        WHILE (l.ActivationTime >= ActivationTime) &  
              (l.RecordType >< HeadRec)  
            l.Next =: l  
        EXITWHILE  
            Precede(l)  
    ENDDO  
ENDIF  
ELSE  
    Out  
ENDIF  
ENDROUTINE  

ROUTINE VOID, VOID : Passivate  
    Activate(Infinity)  
ENDROUTINE  

ROUTINE VOID, BOOLEAN : Passive  
    IF ActivationTime = Infinity THEN  
        TRUE RETURN  
    ELSE  
        FALSE RETURN  
    ENDIF  
ENDROUTINE  

ROUTINE VOID, VOID : Body  
    Output(1,'a','$Process activated at ')  
    Output(1,'f10.3',ActivationTime)  
ENDROUTINE  

ENDRECORD  

% Making a queue of cars waiting to be washed.  
Head : WaitingCars  
% Here comes a car definition. The only property the car  
% needs in this context is a variable telling how long it  
% takes to wash it.  
TYPE Car = Link RECORD  
    REAL : TimeForWashingTheCar  
ENDRECORD  

% Now, we define what the carwash is going to do. This is  
```

---

## Page 91

```
% is done in a redefined version of the ROUTINE called
% Body, which passivates the process if the queue of
% cars is empty, or takes the first car out of the queue
% to wash it and reactivates itself after the car has
% been done to look for more cars in the queue.
% By the way, the carwash will only be open between 0800
% and 1600.

TYPE CarWash = Process RECORD
ROUTINE VOID, VOID : Body
    Car POINTER : NextCar
    IF (NOT WaitingCars.Empty) &
       AND (EventQueue.Time < 16.0) THEN
        WaitingCars.Next =: NextCar
        NextCar.Out
        Activate(ActivationTime + & 
            NextCar.TimeForWashingTheCar)
        Output(1,'a','$W')
        Output(1,'i2',WaitingCars.Cardinal)
        Output(1,'F1.3',EventQueue.Time)
    ELSE
        Passivate
    ENDIF
ENDROUTINE
ENDRECORD

CarWash : TheCarWash
REAL : CustomerArrivalTime := 8.00

% This process puts new cars into the queue at random
% intervals. If the carwash is passive, it also wakes it up
% so the car that just arrived can be washed.

TYPE MakeCustomers = Process RECORD
ROUTINE VOID, VOID : Body
    Car POINTER : NewCar
    IF CustomerArrivalTime < 16.00 THEN
        CustomerArrivalTime + 1.0 / (0.1 + utRnd * 6.0) &
            =: CustomerArrivalTime
        NewCar IN Space =: NewCar
        0.4 * utRnd =: Ind(NewCar).TimeForWashingTheCar
        NewCar.IntoEnd(WaitingCars)
        Output(1,'a','$C')
        Output(1,'i2',WaitingCars.Cardinal)
        Output(1,'F1.3',EventQueue.Time)
        Activate(CustomerArrivalTime)
        IF TheCarWash.Passive THEN
            TheCarWash.Activate(CustomerArrivalTime)
        ENDIF
    ELSE
```

---

## Page 92

# Passivate

**ENDIT**  
**ENDROUTINE**  
**ENDRECORD**

MakeCustomers : TodaysCustomers  
Process POINTER : CurrentProcess  

**PROGRAM** : m13  

**INITSTACK** Stack  
% Initiating the random number generator and setting  
% the right genes to the carwash and the customer-  
% making process.  

utRandom  
EventQueue.InitiateHead  
WaitingCars.InitiateHead  
CarWashRec =: TheCarWash.RecordType  
MakeCustRec =: TodaysCustomers.RecordType  

% The carwash begins the day by looking for a customer,  
% but a random time interval will pass before one  
% actually arrives.  

TheCarWash.Activate(CustomerArrivalTime)  
CustomerArrivalTime + 1.0 / (0.1 + utRnd * 6.0) &  
  =: CustomerArrivalTime  
TodaysCustomers.Activate(CustomerArrivalTime)  

% This is where the work actually begins - a loop  
% that will continue until 1600 when the carwash  
% closes, or there are no active processes in the  
% event queue.  

**DO**  

EventQueue.Next =: CurrentProcess  
**WHILE** (CurrentProcess.RecordType >< HeadRec ) &  
  **AND** ( **NOT** CurrentProcess.Passive)  
**USING** Ind(CurrentProcess)  
  
ActivationTime =: EventQueue.Time  
% The "genes" are used to determine which process  
% body is going to be activated.  

**IF** RecordType = CarWashRec **THEN**  
  TheCarWash.Body  
**ELSIF** RecordType = MakeCustRec **THEN**  
  TodaysCustomers.Body  
**ELSIF** RecordType = ProcessRec **THEN**  
  CurrentProcess.Body  
**ENDIF**  
**ENDUSING**  
**EXITWHILE**  
Output(1,'a','$Done for today. ')  
**ENDDO**  

**ENDROUTINE** m13

---

## Page 93

# M14 - Co-routines

*Co-routines* (or quasi-parallel routines) are part of the PLANC language. Such routines can be stopped in "mid-air" and restarted again without loss of internal variable values. Hence, the program can do something else, and then restart the co-routine after a while. There are several beneficial effects of the availability of co-routines, and no associated syntactic changes.

The merit of quasi-parallel programming (and routines as record components, which was introduced in the I version of PLANC) lies in the ways it allows you to think when solving programming tasks.

## Avoid Global Data

First, since none of the intermediate internal-data values of the co-routine are lost, you can avoid storing to and retrieving such data values from global variables.

## Think in New Ways About Programs

Second (but perhaps most importantly), the availability of co-routines can significantly ease the process of creating an algorithm by letting you model your program in a way more similar to real life. Thus, the ways of problem solving that you use outside the EDP domain can be added to and perhaps replace the ones you use when you make programs.

## Use Time Efficiently

Third, the possibility to stow away the status of a routine so that it can be resumed later is useful if the program requests services from the operating system or other processes that will take some time. This is especially so if you use computers with some possibility for parallel processing, such as an ND-5x0(x0) under SINTRAN with its built-in ND-100 and possibly some PIOCs/DOMINOs. Here, some monitor calls have a NOWAIT option, so you can start a read or write operation and do something else instead of waiting for the operation to finish. The time that your process would spend idle can be used for other purposes, and when the operating system or other process(es) signals that the task is finished, execution of the suspended routine can resume. (This can certainly be done in other ways, but co-routines are a particularly neat and logically straight-forward way to do it.)

## Co-routines Always Inside Records

Co-routines *must* be declared/predeclared inside records. To make a co-routine, you must put the modifier *PARALLEL* in the routine header and provide the co-routine with a stack. This is what the co-routine declaration itself looks like:

```plaintext
[code snippet showing co-routine declaration]
```

---

## Page 94

# Routine Parallel in Value Type

## Optional Parameters: Routine Name

Since a program with co-routines in it cannot be expected to use a common stack as orderly as "traditional" programs, each co-routine must contain an `Inistack` statement. You may use an array declared globally or inside a record, or use `Ind` of an integer-array pointer as the stack array. This array must have zero as `MinIndex`, and you cannot use subranges in the `Inistack` statement.

Hence, the `Inistack` statement must have one of the following four forms:

```
inistack ➔ Inistack integer_array_name
           Inistack record.integer_array_name
           Inistack Ind(integer_array_pointer_name)
           Inistack Ind(record.integer_array_pointer_name)
```

(Note that `Inistack rec1.rec2.name` or similar are not allowed.)

In conjunction with `PARALLEL` routines, the new standard routines `co_Call`, `co_Detach`, and `co_Resume` are introduced to stop and start execution of co-routines. Here are some more details about these routines. As a minor point, note that these standard routines are the only ones in PLANC that have routines as parameters.

## How Problem-Solving is Made Easier

We will elaborate somewhat on the second point above in the following program example.

---

## Page 95

# M14

This module shows how quasi-parallel programming is done using a simple system of PLANC utility frames simulating windows. Plus some more.

## Module M14

We are not interested in listing the following files, so listing is switched off.

## $LIST --

A technical note: There may be different name lengths in the libraries that you need to load this program. Make sure that all libraries have 16 byte name lengths before you load. You may also need to change the user where the `$INCLUDE` files are located - it is (libraries) here, but may be something else on your computer.

```plaintext
$INCLUDE (libraries)planc-util:defs
$INCLUDE (libraries)planc-util:incl
```

The circular list part of the Carwash example has been put into a separate file for general use. This example will use it.

```plaintext
$INCLUDE CIRCLIST:PLNC
```

## $LIST ++

Defining the size of the screen.

```plaintext
CONSTANT syMin = 1, sxMin = 1, syMax = 25, sxMax = 80
```

Various commands will be input as bytes. This set contains the legal byte values.

```plaintext
BYTE SET: Options = {#c, #C, #d, #D, #n, #N, #p, #P, #e, #E}
```

Building on the link type in the CIRCLIST:PLNC file, we make a frame that can be linked into a circular list of frames.

```plaintext
TYPE Frame = link RECORD
  The following Mins and Maxes set limits for cursor movements.
  INTEGER: yMin, yMax, xMin, xMax
  
  For use by the PLANC utility called utBytAcc, which reads keystrokes from the terminal.
  BYTES: LeaveKey(0:5), CommandByte(0:0)
  
  We will use co-routines, and each co-routine needs its own stack to preserve its data when it is not active.
  INTEGER ARRAY: CoStack (0:1023)
  
  Each frame record needs to be able to draw itself once it has been created or overlapped by other frames.
  ROUTINE VOID, Void: ReDraw
  utFrame(yMin-1, xMin-1, yMax-yMin+2, xMax-xMin+2, 'SPACE-FILL')
ENDROUTINE ReDraw
```

---

## Page 96

# Navigate Routine

Navigate is a quasi-parallel routine or a co-routine.
It has its own stack, and can save its state by calling the co_Detach standard routine.

## Routine Parallel

**VOID, VOID**: Navigate  
% x and y are the current cursor position. They are local to this routine, and will be remembered each time the routine is restarted after having been "frozen".  
%  
% The fact that the cursor will be placed at the location indicated by those two variables each time this routine is restarted can be seen as a "proof" that the internal state of the quasi-parallel routine is saved by co_Detach and recalled by co_Call.

**INTEGER**: x, y  
% The following four routines update the cursor position, and wrap around the edges of the frames.

### Routine Up

**VOID, VOID**: Up  
```
IF (-- y) <= yMin-1 THEN yMax-1 =: y ENDIF
```
ENDROUTINE Up

### Routine Down

**VOID, VOID**: Down  
```
IF (++ y) >= yMax THEN yMin =: y ENDIF
```
ENDROUTINE Down

### Routine Left

**VOID, VOID**: Left  
```
IF (-- x) <= xMin-1 THEN xMax-1 =: x ENDIF
```
ENDROUTINE Left

### Routine Right

**VOID, VOID**: Right  
```
IF (++ x) >= xMax THEN xMin =: x ENDIF
```
ENDROUTINE Right

% Navigate will use an array component of the current record as stack.

### InitStack

**CoStack**  
% The first time this routine is executed, it places the frame defined by its enclosing record on the screen and places the cursor in the upper left corner.

ReDraw; yMin =: y; xMin =: x  
% Navigate as long as the frame exists!

### Do

% Place the cursor at the current position and wait for input from the user.  
%  
% utBytAcc places the cursor at the current location

---

## Page 97

```
* on the screen and expects you to type a string of
* bytes. The strings used here will contain only one
* byte, which will be placed in the CommandByte.
* For details about utBytAcc, read the PLAN C utilities
* manual, ND-860297. Note that here, you must use
* the left and right TAB keys to move the cursor horizon-
* tally, as the left and right arrow keys are used for
* intra-string editing.
utBytAcc(y, x, l, CommandByte, 'UP,DOWN,LEFT,RIGHT')
= : LeaveKey

IF CommandByte(0) IN Options THEN
    % If the CommandByte is a valid option, then "freeze"
    % the state of this record's Navigate routine and
    % continue execution in the routine that started it.
    co_Detach THISRECORD.Navigate
    % When this routine is restarted, there may be other
    % frames on top of the current one, so redraw it.
    ReDraw
ELSE
    % Move the cursor around inside the frame.
    IF LeaveKey(0:1) = 'UP' THEN Up
    ELSIF LeaveKey(0:3) = 'DOWN' THEN Down
    ELSIF LeaveKey(0:3) = 'LEFT' THEN Left
    ELSIF LeaveKey(0:4) = 'RIGHT' THEN Right
    ENDIF
ENDIF

ENDDO
ENDROUTINE Navigate
ENDRECORD

% In the list of frames, one of them will be the currently
% active frame.
Frame POINTER : CurFrame
% Frames will be created and destroyed dynamically, and as
% each one of them will contain a local stack, they will
% need a lot of room.

INTEGER ARRAY : Space(0:37777B)
% To head the circular list, we create a variant of the
% Head record type in CIRCLIST:PLNC. It will be capable of
% creating new frames, and of redisplaying all frames in
% the list, ending with the CurFrame which will thus be
% shown over of the others.

TYPE Screen = head RECORD
ROUTINE VOID, VOID : ReDisplay
```

---

## Page 98

# Code Implementation

```plaintext
Frame POINTER : DisFrame
utClearScreen
CurFrame =: DisFrame
DO WHILE DisFrame.Next >< CurFrame
    DisFrame.Next =: DisFrame
    IF DisFrame.RecordType = LinkRec THEN
        DisFrame.ReDraw
    ENDIF
ENDDO
CurFrame.ReDraw

ENDROUTINE ReDisplay

ROUTINE VOID, VOID : MakeNewFrame
    Frame POINTER : BeingMade
    New Frame IN Space =: BeingMade
    USING BeingMade
    DO
        LinkRec =: RecordType
        utBytDis(syMax, 2, 6, 'yMin: ', 'LOW-INTENSITY')
        utBytDis(syMax, 12, 6, 'xMin: ', 'LOW-INTENSITY')
        utBytDis(syMax, 22, 6, 'yMax: ', 'LOW-INTENSITY')
        utBytDis(syMax, 32, 6, 'xMax: ', 'LOW-INTENSITY')
        utIntAcc(syMax, 9, 2, yMin, 'NORMAL')
        utIntAcc(syMax, 19, 2, xMin, 'NORMAL')
        utIntAcc(syMax, 29, 2, yMax, 'NORMAL')
        utIntAcc(syMax, 39, 2, xMax, 'NORMAL')

        WHILE NOT &
        (((syMin < yMin) AND (yMin < yMax) AND (yMax < syMax)) &
        AND ((sxMin < xMin) AND (xMin < xMax) AND (xMax < sxMax)))
        utBytDis(syMax, 43, 6, 'Wrong!', 'INVERSE-VIDEO,BEEP')

        EXITWHILE

        IF THISRECORD.Empty THEN
            IntoStart (Ind THISRECORD)
        ELSE
            Follow CurFrame
        ENDIF
        BeingMade =: CurFrame
        THISRECORD.ReDisplay
    ENDDO
ENDUSING
ENDROUTINE MakeNewFrame
ENDRECORD
```

---

## Page 99

```markdown
% A short routine that exits the program in an informative way.
% utDisplay is a simple routine that prints strings on the
% terminal. The code parameter is for use if ERRCODE is
% set.

ROUTINE VOID, VOID (BYTES : String; INTEGER : code) : LeaveProgram
  utClearScreen
  utDisplay (String // ' ')
  IF code >< 0 THEN
    Output(1, '17', Code)
  ENDIF
  utDisplay BYTES(15B, 12B)
  Monitor_call('LEAVE')
ENDROUTINE

INTEGER ARRAY : Stack(0:7777B)

% Making the variable Frames, which will be of type Screen and thus
% head of the frame list.

Screen : Frames

PROGRAM : Main
  INITSTACK Stack
  ON ROUTINEERROR DO
    LeaveProgram ('Routineerror.', ERRCODE)
  ENDON

  Frames.InitiateHead
  utClearScreen
  % Always start by creating one new frame:
  Frames.MakeNewFrame
  % ... and start moving the cursor inside the frame or
  % selecting options.
  CurFrame.Navigate
  % Execution will continue here after the first Navigate
  % has co_Detach'ed. The loop will go on until LeaveProgram
  % or something similar is called.

  DO
    % Next, Previous, Create, Delete, Exit
    % - which one was it?

    CASE CurFrame.CommandByte(0)
      INCASE #e, #E
        LeaveProgram ('Normal exit.', 0)
      
      INCASE #c, #C
        % Create a new frame in the list and start moving
        % around inside it.
        Frames.MakeNewFrame
        CurFrame.Navigate

      INCASE #d, #D
        % If you delete the last frame in the list, then exit,
        % otherwise remove the current frame from the list and
```

---

## Page 100

```
% ReDisplay the remaining frames.
IF Frames.Cardinal > 1 THEN
    CurFrame.Next =: CurFrame
    CurFrame.Previous.Out
    IF CurFrame.RecordType = HeadRec THEN
        CurFrame.Next =: CurFrame
    ENDIF
    Frames.ReDisplay
    % Now, restart execution of the Navigate of the
    % record pointed to by CurFrame.
    co_Call CurFrame.Navigate
ELSE
    LeaveProgram ('Deleted last Frame!', 0)
ENDIF
INCASE #p, #P
    % Restart the preceding Frame in the list. The
    % preceding record in the list may be the head
    % of the list, so therefore a little precaution:
    IF CurFrame.Previous.RecordType = HeadRec THEN
        CurFrame.Previous.Previous =: CurFrame
    ELSE
        CurFrame.Previous =: CurFrame
    ENDIF
    co_Call CurFrame.Navigate
INCASE #n, #N
    % Same procedure when restarting the next frame in
    % the list.
    IF CurFrame.Next.RecordType = HeadRec THEN
        CurFrame.Next.Next =: CurFrame
    ELSE
        CurFrame.Next =: CurFrame
    ENDIF
    co_Call CurFrame.Navigate
ENDCASE
ENDDO
ENDROUTINE Main
ENDMODULE
%EOF

Get closer to real life

When making programs in PLANC version J, it may be useful to consider that the actions of many real-life processes depend on their own states and what they can ascertain about the states of other processes. If you can write down records describing the states of a process and its actions, you are in fact getting closer to the solution of your problem.

If making a card-playing program, we can associate a record with each participant that describes his cards and has a pointer to a history of the game which he can peruse. The cards can be hidden from view by other
```

---

## Page 101

# M15 - Advanced Co-Routines

## Parameters

Co-routines may have parameters in the same way as ordinary routines. But write parameters and out-values are not legal and return no sensible values after `RETURN` from co-routines.

## Terminating Co-Routines

You _may_ use `RETURN` to terminate a co-routine in the same way as an ordinary PLANC routine. But you might as well use `co_Detach`. If you use `RETURN`, take care not to call/resume the co-routine again afterwards!

## Detaching/Calling Other Routines

The possibility the PLANC co-routine facilities give you to detach on other routines than the one currently executing may appear confusing, since it is not so easy to get an intuitive feeling for how this can be used. The following example shows one way of applying detach to other routines.

```
%===========================================================%
% m15                                                      %
%                                                          %
% More complex co-routine calls. The following module      %
% shows how to merge two arrays of integers using          %
% co-routines. It is hardly the most efficient way to      %
% merge, but it highlights some features of PLANCs         %
% co-routines.                                             %
%===========================================================%
MODULE Merging
% The algorithm will make use of one co-routine which
% "manages" co-routines associated with the arrays to be
% merged.
TYPE MergeManager = RECORD
  INTEGER : Value
```

---

## Page 102

# Technical Documentation

## BOOLEAN
- End

## INTEGER ARRAY
- `mmStack(0:255)`

## ROUTINE PARALLEL VOID, VOID
- Manager ?

### RECORD
*There will be one of the following records for each array.*

#### TYPE Merger = RECORD

- **INTEGER**
  - Next

- **INTEGER ARRAY**
  - mStack(0:255)

*Comments*:
- The `in-value` to this routine is a pointer to another Merger record, the parameter an array to be merged.
- Since the routine is going to operate on records of the type being declared, the routine must be predeclared and then declared later, when the compiler knows the size of the record.

#### ROUTINE PARALLEL Merger POINTER, VOID (INTEGER ARRAY) : Merge ?

### CONSTANT
- MaxInt = 32767
- MinInt = -32768

### INTEGER ARRAY
- `Arr1 := (-3, 0, 13, 34, 56, 123, 213, 815, 816, 1000, 1037)`

### INTEGER ARRAY
- `Arr2 := (5, 7, 10, 77, 300, 350, 700, 1010, 1030, 1035)`

### Manager
- MergeManager : MergeManage
- Merger : ArrayOneRecord, ArrayTwoRecord

### ROUTINE PARALLEL VOID, VOID
- MergeManager.Manager

#### INISTACK
- `mmStack`

#### FALSE
- End

*Comments*:
- Call the two Merge routines with the other record as `in-value` and an array as parameter. They will now be initialized.

```plaintext
Addr(ArrayTwoRecord) ArrayOneRecord.Merge(Arr1)
Addr(ArrayOneRecord) ArrayTwoRecord.Merge(Arr2)
```

- Set the ball rolling.

```plaintext
Co_Call(ArrayOneRecord.Merge)
```

#### TRUE
- End

## ENDROUTINE Manager

### ROUTINE PARALLEL Merger POINTER, VOID &

#### (INTEGER ARRAY)
- Merger.Merge(x)

#### INTEGER
- i

#### INISTACK
- `mStack`

#### MinInt
- Next

```plaintext
Co_Datach(THISRECORD.Merge)
```

*Comments*:
- Repeat once for each element in the array

```plaintext
FOR i IN x DO
```

- If current element is greater than the next element in the other array, then let other routine proceed

---

## Page 103

```
% to output its next value.
IF x(i) > @.Next THEN
  x(i) =: Next
  % Detach this routine and make other routine proceed.
  Co_Resume(THISRECORD, Merge, @.Merge)
ENDIF
% Now, the current element can be output.
x(i) =: MergeManage.Value
% Detach this routine, using another routine's stack
% to store the data needed to proceed. Following
% this statement, this routine will
% proceed when MergeManage.Manager is called.
Co_Detach(MergeManage.Manager)

ENDIF
MaxInt =: Next
% Detach this routine and make other routine proceed. If
% this routine is called again, it will RETURN like an
% ordinary routine.
Co_Resume(Merge, @.Merge)

ENDROUTINE Merge

ROUTINE VOID, VOID : MergeThem
  MergeManage.Manager
  DO WHILE NOT MergeManage.End
    Output(1, I8', MergeManage.Value); Output(1, 'A', 'S')
    % After the following statement, the routine which
    % did a detach on the MergeManage.Manager will proceed
    % - and that is not necessarily the Manager itself!
    % In this program, it will be the Merge routine which
    % has installed its state on MergeManage.Manager's stack.
    Co_Call(MergeManage.Manager)
  ENDDO
ENDROUTINE MergeThem

INTEGER ARRAY : Stack(0:511)
PROGRAM : Main
INSTACK Stack
  MergeThem
ENDROUTINE Main
ENDMODULE
$EOF
```

---

## Page 104

# Modules

## M16 - Modules

All PLANC code and data come in blocks delimited by the keywords MODULE and ENDMODULE. (You will have noticed this in the previous examples.)

For a PLANC program to be executable, it must consist of at least one module, and one and only one of the modules in the program must contain a routine of the special types PROGRAM or ROUTINE MAINSTART that defines the main entry point where execution begins. A PLANC module is the smallest unit that can be compiled separately.

The rule about the main entry point is not strictly true in mixed language programming, where the main entry point may reside in code written in another language than PLANC. When mixing PLANC and C, the C language demands that the entry point lies in the C main library routine, for example.

Code, data, constants and types defined inside a PLANC module cannot be used by other modules or code in other languages unless the EXPORT statement is explicitly used to make the associated names known outside the module, and a PLANC module cannot use such entities unless they are made known inside the module with the IMPORT statement.

This provides a compartmentalization of the program that is beneficient to the developer: code in one module cannot influence on other modules unless he/she explicitly makes this possible, and since modules are separately compilable, only parts of the source code need be recompiled before reloading the system. (Using the $SELECT compiler command, only the recompiled module...)

---

## Page 105

```
% needs to be reloaded, so the compartmentalization can
% give significant reductions in the time the recompile/re-
% load cycle takes too.)

%==============================================================
% You can declare new types and constants outside modules.
% Such constants can be used in all modules on the source
% file. It is not necessary to import this type to inner
% module levels.

TYPE SharedType = INTEGER2 UNSIGNED

MODULE M16One

% All declarations of constants, types and variables
% (including routine) inside a module must be exported
% from the module where they are declared to the "outside
% world" if they are going to be used in other modules
% or code in other languages. This is how you export
% some of your variables - you simply list them after
% the keyword EXPORT. Note that the variable LocalVar
% will not be known outside this module.

EXPORT Intl, Int2, Bool

% This shows how you import from the "outside world" into
% a module. You must give a complete description of its
% data structure in addition to its name. This is necessary
% when modules are compiled separately, otherwise the
% compiler would not be able to decide that routine calls
% and variable usage is correct.

IMPORT (ROUTINE SharedType, INTEGER : M16Two)

INTEGER : Intl, Int2, LocalVar

BOOLEAN : Bool

% Making a variable of the type declared outside the
% modules:

SharedType : st := 841177777#

% A complete program has one and only one main entry point.
% The following will serve this purpose in the current
% program:

INTEGER ARRAY : Stack(0:1023)

CONSTANT UnixOrDos = FALSE

$IF UnixOrDos $THEN

ROUTINE MAINSTART VOID, VOID & 
  (INTEGER : ArgC; &
   BYTE POINTER POINTER : ArgV) &
   : RoutMainStart

$ELSE

PROGRAM : M16One

$ENDIF

INITSTACK Stack
```

---

## Page 106

# Technical Document

* Now, use the imported item as if it is declared in
* the current module.
```
st M16Two =: LocalVar
ENDROUTINE M16One
ENDMODULE
```

## MODULE M16Two

```
EXPORT M16Two
IMPORT (INTEGER : Int1, Int2), (BOOLEAN : Bool)
INTEGER : LocalVar := -1
ROUTINE SharedType, INTEGER : M16Two
IF Bool THEN
  (@ + Int1 - Int2 :=: LocalVar) RETURN
ENDIF
```
LocalVar RETURN

```
ENDROUTINE M16Two
ENDMODULE
```

* Although it is not common to do so, it is possible to
* nest modules. The general rule is that either modules
* consist of declarations of data and routines, or they
* consist of one or more inner modules. The inner modules
* must ex-/import routines and variables to communicate,
* and if variables/routines are exported/imported on the 
* outermost level, they must be ex-/imported all the
* way to the module nesting level where they will be used.

* Nesting modules gives the compiler an opportunity to
* type check the ex-/imported types. However, serious
* programs consisting of nested modules are slow to compile
* and heavy to work with, so most developers prefer to
* keep modules separate and to generate correct import
* statements from export lists using for instance the
* $GENERATE-IMPORTS compiler command. For details, see the
* next section in this chapter. But here are some nested
* modules.

## Outer Module, Level Zero:

### MODULE M160

* Making a variable known outside the enclosing module:
```
EXPORT Lev2lVar 
TYPE ModuleType = INTEGER1 UNSIGNED
```

## First Module on Level One:

### MODULE M161

```
IMPORT ModuleType
```

* Exporting the following variable out of this module:
```
EXPORT Lev21Var
```

## A Module on Level Two:

### MODULE M1621

---

## Page 107

```
IMPORT ModuleType
% The following variable, which is declared in this module,
% is made known outside it:
EXPORT Lev2lVar
INTEGER : Lev2lVar
% Declaring a variable of a type that has been imported
% from outside the outermost level:
SharedType : sTyp
% Declaring a variable of a type that has been imported
% from inside the outermost level:
ModuleType : mTyp
ENDMODULE
ENDMODULE
% Second module on level one:
MODULE M1612
IMPORT ModuleType
ENDMODULE
ENDMODULE
$EOF
```

---

## Page 108

# The Compiler's Command Processor

## A Multitude of Commands

The compiler has many more commands and facilities than merely `DEBUG` and `$COMPILE`, which have been the most used commands thus far. For example, the useful `$IF ... $ELSEIF ... $ELSE ... $ENDIF` and `$MACRO ... $ENDMACRO` commands were introduced in example M8. Other commands are used to make libraries, cross references, `$INCLUDE` files according to `EXPORT`-statements in module being compiled, information for query databases for large system development, send messages to the terminal during compilation, and more.

It is not convenient to demonstrate all compiler commands in an example. The example in the next subsection will present some useful features such as macro programming, conditional compilation, file inclusion and automatic include file generation, and to get precise information, you should start at the section about compiler commands in the reference part of the manual, which contains an overview of all compiler commands.

## M17 - The Command Processor

### Numerous Compiler Commands Available

The PLANC compiler has many commands that guide its execution in addition to the ones you have met already. It has commands to make libraries, to list object code as it is ejected from the compiler, to make cross references, to textually include other source files into the current compilation, to define macros, for conditional compilation and more.

### Commands Can Be Given in Two Contexts

Commands can either be typed to the compiler's command processor in response to its prompt, or written in the source file preceded by a `$` (dollar) sign. Not all commands can be used both places, however: It makes no sense to give `$COMPILE`-commands inside a source file, or to use conditional compilation outside source files.

### Command Abbreviation

Compiler commands may be abbreviated according to the SINTRAN III conventions. That is, a command may consist of many parts separated by dashes (-), like in the command `MODULE-LIBRARY-MODE`. Each part may be abbreviated as long as it is unambiguous; thus, `MOD-LIB`, `MOD--MODE` or simply `MO` are sufficient to identify the command.

### No Extra Preprocessor Pass

The PLANC compiler does not need an extra preprocessor pass to execute compiler commands in the source files. On the ND-100, it can even generate program (`:PROG`) files directly, so that the final loader pass is unnecessary.

---

## Page 109

# M17

## The Command Processor

---

**MODULE** m17

One of the most common and useful compiler commands is `INCLUDE`, which textually inserts the contents of another source file into the file which is currently being compiled. The following two lines include the definitions and import statements needed to use the PLANC utilities. It is a good idea to have a look at these include files, as they contain many useful utility routines. They are found on user (libraries) on the computer where this example was made, but may be somewhere else on your computer.

```
$LIST --
$INCLUDE (libraries)planc-util::defs
$INCLUDE (libraries)planc-util::incl
$LIST ++
```

The next command makes type checking of data structures exported/imported between different files easy. It will generate a file that can be `$INCLUDED` in other files with correct `IMPORT` statements for the exported data.

```
$GENERATE-IMPORTS m17::impt
EXPORT Type1, Type2
EXPORT Rout1, Rout2, Rout3, Rout4
EXPORT Var1, Var2, Var3
BYTES : CrLf := (15B, 12B)
ROUTINE BYTE, VOID : Rout1
  % The next routine is an utility that displays a string 
  % on the terminal. It is declared in an IMPORT statement
  % in the PLANC-UTILLIB::INCL file.
  utDisplay ('Rout1' // CrLf)
ENDROUTINE
TYPE Type1 = ENUMERATION (Trit0, Trit1, Trit2)
TYPE Type2 = RECORD
  Type1 : Some
  INTEGER : Thing
ENDRECORD
ROUTINE VOID, INTEGER (INTEGER, BOOLEAN READ WRITE) : Rout2 (w, x)
  utDisplay ('Rout2' // CrLf)
  RETURN
ENDROUTINE
ROUTINE VOID, VOID (BYTES, Type2) : Rout3(y,z)
```

---

## Page 110

# Technical Document

```
utDisplay ('Rout3' // CrLf)
```

## ENDROUTINE

### ROUTINE VOID, VOID (BYTES : a, b, c) : Rout4
```
utDisplay ('Rout4' // CrLf)
```

## ENDROUTINE

### Declarations
- **INTEGER** : Var1
- **BOOLEAN READ** : Var2
- **BYTES** : Var3(-10:10)

### Notes
- If your program is going to be used on several different CPUs and OSs, it is desirable to make it as "portable" as possible, i.e. independent of the special characteristics of the hardware and software on which it will be run. This is conveniently done using the conditional compilation commands, $IF ... $THEN ... $ELSIF ... $ELSE ... $ENDIF.

### Constants
- First, we define a couple of constants:
  ```
  CONSTANT sintran = 313, unix = 317
  ```

- The compiler command $PRESENT makes it possible to check if a symbol has been defined in the compiler's symbol table:
  ```
  $IF NOT $PRESENT OpSys $THEN
    $MESSAGE-TO-TERMINAL OpSys is set to SINTRAN
    Enter a new constant into the table:
    CONSTANT OpSys=sintran
  $ENDIF
  ```

  $IF OpSys=sintran $THEN
  ```
  $IF $PRESENT OpSysSINTRAN $THEN
    $KILL OpSysSINTRAN
  $ENDIF
  $IF $PRESENT OpSysUNIX $THEN
    $KILL OpSysUNIX
  $ENDIF
  ```

- The $CONSTANT command defines a constant that will be in effect during all subsequent $COMPILE commands (or until it is $KILLed).
  ```
  $CONSTANT OpSysSINTRAN=TRUE
  $CONSTANT OpSysUNIX=FALSE
  $ELSIF OpSys=unix $THEN
  $IF $PRESENT OpSysSINTRAN $THEN
    $KILL OpSysSINTRAN
  $ENDIF
  $IF $PRESENT OpSysUNIX $THEN
  ```

---

## Page 111

```
$KILL OpSysUNIX
$ENDIF
$CONSTANT OpSysSINTRAN=FALSE
$CONSTANT OpSysUNIX=TRUE
$ELSE
$MESSAGE-TO-TERMINAL ERROR: OpSys Should be
$MESSAGE-TO-TERMINAL declared as an INTEGER CONSTANT
$EXIT
$ENDIF

% You can define macros in the command processor using the
% $MACRO ... $ENDMACRO commands. When a macro name is read
% in the source file, the compiler replaces the name with
% the macro body and then compiles the resulting source
% code. If the macro has parameters, the names of the
% parameters are literally substituted into the macro
% body as the macro is inserted into the source code.
% Here is a macro that swaps the values of two variables:
$MACRO Swap(x, y)
  "x" :=: "y" =: "x"
$ENDMACRO

% This one calculates the biggest number that can be held in
% an INTEGER on the CPU being compiled for. Note that $ENDMACRO
% is on the same line as the macro body. This is because all
% characters in the macro body from the first character on the
% first line after the $MACRO line and up to but not including
% the dollar sign in $ENDMACRO are part of the macro. If
% $ENDMACRO were on a line of its own, there would be an extra
% CrLf in the macro body.
$MACRO MaxInt
  2**(Bit_size(INTEGER)-2)-1+2**(Bit_size(INTEGER)-2) $ENDMACRO

% We proceed to use these in some code:
INTEGER : i := 2, j := 3
ROUTINE VOID, VOID : UseMac
  % Now,
  Swap(i, j)
  % is the same as writing
  i :=: j =: i
  MaxInt =: i
ENDROUTINE UseMac
ENDMODULE
$EOF
```

---

## Page 112

The page is blank except for a page number and some stains. Therefore, no text or discernible non-text elements can be transcribed or recreated.

---

## Page 113

# Implementation Details

---

## OS-specific Implementation Details

### Avoiding Massive Recompilation Under SINTRAN

#### Be SELECTive

The SELECT command to the PLANC compiler can help you save much compile- and load time during development of large programs. But the compiler generates symbol names for routines that consist of a composite of the module name and the routine name separated by a dot, thus: `<module name>.<routine name>`. The total length of the composite name cannot exceed 16 bytes (including the dot). This limitation is going to be removed in the B version of the new ND-Linker. This version of the linker can reload SELECT routines and has 256 bytes symbol length. But in the meantime, you must keep the 16 byte limitation in mind when using SELECT.

#### What is it

Massive recompilations and loading of source code for a major software system can be very time consuming. An alternative to massive recompilations is to compile only the parts of the source that have changed since the last time it was compiled, and to let the linker "patch" the recompiled code into the existing absolute code. This will make the size of the executable code increase in size, as the "old" code will still be present even if it is never executed while the "new" code will be added at the end of the executable code. But the recompilation and reloading of selected parts of the code will take much less time, which is the main point.

#### Bigger, Not Slower

The reloaded program files may become bigger when they are reloaded, but execution is not necessarily slower. Only the pages that are executed by the program are read into memory, and pages containing new code will be read when needed while pages containing code that is patched out may never be swapped in.

#### Tidy up Once in a While

(After you have reloaded changed code repeatedly for a while, you will need to do a massive recompilation and loading again to clear away dead code. Use the lunch break or a good, long meeting for this. Or do it after working hours.)

#### How to SELECT

The ND-500(0) PLANC compiler has been extended to allow recompila-

---

## Page 114

\### Selection of Routines

Selection of selected routines from within a module. This selection is done with the compiler command:

\```
SELECT <routine>, <routine>
\```

Use of this option may speed up compile time about five to ten times over a complete recompilation, depending on type redefinitions and global data. This also gives faster syntax checks after minor changes in large programs. Furthermore, the SELECTed routine(s) may be reloaded with the Linkage-Loader at a fraction of the CPU cost.

If you want to use the SELECT option, the total system must be compiled with the option:

\```
SELECT *ALL*
\```

which will make all routines known to the loader as composites of module name and routine name. This is necessary to make the loader able to resolve name clashes, both for routine names and global variables within the module.

Routines that will be reloaded must always be predefined and declared as a total set. Inner routines cannot be selected individually, only as part of an enclosing routine on level 1. All routines must be declared with the keyword ROUTINE and not just the type name for its data type.

---

## Page 115

# Select Demo

In this section, you see how the SELECT command is used on a small example. In the example, the originally loaded program undergoes a few small changes, and the changed routines are then patched into the executable ND-500(0) domain.

Suppose you have the following program:

```
%========================================================%
% The $SELECT compiler command                            %
%========================================================%
% This module and its companion files M17:PLNC and M18:MODE
% show how massive recompilation can be avoided on the
% ND-500(0) under SINTRAN.

MODULE m16
$INCLUDE (LIBRARIES)PLANC-UTILLIB:DEFS
$INCLUDE (LIBRARIES)PLANC-UTILLIB:INCL

ROUTINE VOID, VOID : a?
ROUTINE VOID, VOID : b?
ROUTINE VOID, VOID : c?
ROUTINE VOID, VOID : d?

BYTES READ : CrLf := (15B, 12B)

ROUTINE VOID, VOID : a 
    utDisplay(CrLf//'This Is ROUTINE a.1')
    utDisplay('- To Become Smaller')
    b
ENDROUTINE

ROUTINE VOID, VOID : b 
    utDisplay(CrLf//'This Is ROUTINE b.1')
    c
ENDROUTINE

ROUTINE VOID, VOID : c 
    utDisplay(CrLf//'This Is ROUTINE c.1')
    d
ENDROUTINE

ROUTINE VOID, VOID : d 
    utDisplay(CrLf//'This Is ROUTINE d.1')
ENDROUTINE

INTEGER ARRAY : Stack(0:1023)

PROGRAM : DemoSelect
    INITSTACK Stack 
    a
ENDROUTINE
```

---

## Page 116

# Technical Document

```plaintext
%============================================================%
% $SELECT demo                                               %
%============================================================%

MODULE m18
$INCLUDE (LIBRARIES)PLANC-UTILLIB:DEFS
$INCLUDE (LIBRARIES)PLANC-UTILLIB:INCL
ROUTINE VOID,VOID : a?
ROUTINE VOID,VOID : b?
ROUTINE VOID,VOID : c?
ROUTINE VOID,VOID : d?
BYTES READ : CrLf:= (15B,12B)
ROUTINE VOID,VOID : a
    utDisplay(CrLf//'This Is ROUTINE a.2')
    b
ENDROUTINE
ROUTINE VOID,VOID : b
    utDisplay(CrLf//'This Is ROUTINE b.2')
    c
ENDROUTINE
ROUTINE VOID,VOID : c
    utDisplay(CrLf//'This Is ROUTINE c.2')
    utDisplay(' - Is Now Larger')
    d
ENDROUTINE
ROUTINE VOID,VOID : d
    utDisplay(CrLf//'This Is ROUTINE d.2')
ENDROUTINE
INTEGER ARRAY : Stack(0:1023)
PROGRAM : DemoSelect
INISTACK Stack
    a
ENDROUTINE
ENDMODULE
$EOF
```

---

## Page 117

## How to Avoid Massive Recompilations

```
@cc ================================
@cc How to avoid massive recompilations
@cc ================================

@PLAN-500
SELECT *ALL*
COMPILE select-1,,select
@LINKAGE-LOADER
SET-DOMAIN select
OPEN-SEGMENT select,,,
LOAD-SEGMENT select
LOAD-SEGMENT (libraries)planc-utillib
LOAD-SEGMENT (libraries)planc-lib
.EXIT
@ND-500 select
@PLAN-500
SELECT a,c
COMPILE select-2,,select
@LINKAGE-LOADER
SET-DOMAIN select
APPEND-SEGMENT select,,,,
RELOAD-SEGMENT select
LOAD-SEGMENT (libraries)planc-utillib
LOAD-SEGMENT (libraries)planc-lib
EXIT
@ND-500 select
```

---

## Page 118

# Packing of Composite-Data Types

## Purpose of Data Packing

Packing of data gives less wasted data space in the program, but access to packed data usually takes more time and code than for unpacked data.

## Packing is Pragmatic

Packing in PLANC is a compromise between data density on one side and data access time and code size on the other: some data space wastage is allowed in packed composite-data in order to keep access time fast and code size small.

## Finding Size and Position

If you are in doubt about the size of and record component position within a packed record, then use the predefined functions `Bit_Size`, which gives the number of bits in a data structure, and `Bit_Position`, which gives the number of the bit within the record where the record component starts.

## Arrays

A new component of the base type of the array is always located at the next free byte, even if the base type is a record. There is one exception to this rule: `BITS` arrays, which is the same as `BOOLEAN ARRAY PACKED` arrays, are packed bit-by-bit.

## General Advice

It is strongly advised that you group declarations of standard types together and **before** user-defined types. When declaring large arrays in records, the array declaration should be at the end of the record, so that large addressing displacements of single variables can be avoided.

## On the ND-500(0)

### Field Location

Fields within packed records are assigned space from bit 31 down to 0.

### If Word is Too Small

If a record component requires more space than there are free bits in the 32-bit word, the current word is abandoned and a new one is defined at the next **byte** boundary. Thus, a maximum of seven bits may be vacant per record component in a packed record.

## On the ND-100, MC680xx, and INTEL 286/386

### Field Location

Fields within packed records are assigned space from bit 15 down to bit 0.

### If Word is Too Small

If a record component requires more space than there are free bits in the 16-bit word, the current word is abandoned and a new one is defined at the next **word** boundary. Thus, a maximum of 15 bits may be vacant per record component in a packed record.

---

## Page 119

# Data/runtime organization on the ND-500(0)

## Routine entry and exit on the outermost module level

The instructions `CALL` and `CALLG` are used for all calls. Parameters are transferred explicitly with number of arguments equal to zero unless the routine has been declared with the `STANDARD` modifier.

Ordinary (non-`STANDARD`) routine calls are followed by the `IFK RET` instruction. Routines usually begin with `ENTS`, if they contain an Inistack they begin with `ENTM`, while main programs begin with `INIT`.

## Routine entry and exit in nested routines

Nested routines are entered with `ENTD` and save the `L` register in a local temporary variable. Exit is by an indirect jump. Parameters are passed explicitly by the generated code.

Nested routines use the stackframe of the enclosing routine, thus accessing its local-scope variables directly `.B` (= relative to B). The X flag is set explicitly.

## Parameter transfer

See the preceding section for principles.

The `CALL`/`CALLG` instructions with `n` arguments are used to call `STANDARD` routines.

## Out-values from routines

All kinds of `BOOLEAN`, `ENUMERATION` and `INTEGER` variables are returned in the `I1` register. 32-bit reals are returned in the `A1` register, while 64-bit reals are returned in the `D1` register. Pointers to simple variables and records are returned in the `I1` register. Array pointers and sets with more than 32 bits are fetched from the stackframe that is being left. Sets with fewer than 32 elements are returned in `I1`.

## In-values to routines

The same as for out-values, but array pointers and big sets are moved explicitly to next stackframe.

## Routine pointers

A routine pointer is just a pointer to a single location, like a record pointer. But remember, you cannot point to nested local routines, only to routines on the outermost module level.

---

## Page 120

# Representation of Non-Packed Data in Bytes

| Data Type          | Bytes                                    |
|--------------------|------------------------------------------|
| BOOLEAN1           | 1                                        |
| BOOLEAN2           | 2                                        |
| BOOLEAN            | 4                                        |
| BYTE               | 1                                        |
| INTEGER1           | 1                                        |
| INTEGER2           | 2                                        |
| INTEGER4           | 4                                        |
| INTEGER            | 4                                        |
| INTEGER RANGE      | 1, 2, or 4 depending on range            |
| REAL               | 4                                        |
| REAL8              | 8                                        |
| ENUMERATION        | 4                                        |
| XXX ARRAY POINTER  | 12 times dimensionality                  |
| YYY POINTER        | 4                                        |
| ZZZ SET            | (NumberOfElements + 7) / 8               |

# Storage Alignment for Non-Packed Data

Here, `byte` means alignment on the next byte from the current position, `halfword` means alignment on the next half-word in memory, and `word` means alignment on the next word in the memory space.

| Data Type          | Alignment       |
|--------------------|-----------------|
| BOOLEAN1           | byte            |
| BOOLEAN2           | halfword        |
| BOOLEAN            | word            |
| BYTE               | byte            |
| INTEGER1           | byte            |
| INTEGER2           | halfword        |
| INTEGER4           | word            |
| INTEGER            | word            |
| INTEGER RANGE      | byte, halfword, or 4 depending on range |
| REAL               | word            |
| REAL8              | word            |
| ENUMERATION        | word            |
| XXX ARRAY POINTER  | word            |
| YYY POINTER        | word            |
| ZZZ SET            | word            |
| RECORD             | word            |

# Representation of Packed Data

See the keyword `PACKED`.

# Error Return Mechanism

If a routine gets an error return, i.e., exits via the `value ERROR RETURN` statement, the `value` is returned in the I1 register, and X is set to 1.

Normal PLANC routine sequences are followed by a test on X, upon which an `ON ROUTINE ERROR` exception-handler can be invoked, or a direct return to the previous routine level takes place. If an `ON ...` sequence is activated, the I1 is stored into the `ERROR CODE` variable, so it can be examined further.

# Exception Handling

No default exception handling is implemented.

# Traps and Trap Handling

You can handle the traps yourself using the `utDefineTraps` routine. See [illegible].

---

## Page 121

Page 109

the manual *PLANC Utility Library and PLANC-GEN, ND-860297* for details.

---

## Page 122

The page is completely empty except for the page number:

```
110
```

---

## Page 123

# General Topics

## Types

### Purpose of Types

The *type* concept is introduced to avoid programming errors by ensuring that variables, literals and constants are appropriate in the context where they are used. Examples of such errors would be adding an integer to an array, or calling a routine with a real in-value when an integer in-value was specified in the declaration of the routine.

### PLANCs Types

In addition to the types recognized by the compiler when compilation starts, you can define an infinite number of new types building on already existing ones. Before any new types have been defined, PLANC recognizes the simple types `INTEGER`, `REAL`, `BOOLEAN`, `LABEL` and `VOID` or subtypes made by applying modifiers to them. (A `BYTE` is similar to an `INTEGER1 UNSIGNED`. `VOID` is only used in routine declarations.)

### Making New Types

New types are made with the *type constructors* `POINTER`, `ENUMERATION`, `ARRAY`, `SET`, `RECORD` and `ROUTINE` in declarations.

### Construction of Simple Types

The pointers are simple types that contain the address of a variable of any type (in the case of pointers to arrays, they also contain the index information of the arrays), while enumerations are simple types that have a list of valid PLANC name tokens as values.

### Construction of Composite Types: Arrays

Arrays have a base element of any type that occurs as many times in the array as specified by its index set. The index set may be defined over a range of integers or enumerations. The `BYTES` type is similar to the `INTEGER1 UNSIGNED ARRAY PACKED` type (like the `BYTE` is similar to an `INTEGER1 UNSIGNED`), while `BITS` is similar to `BOOLEAN ARRAY PACKED`.

### Sets

Sets may contain up to 255 members which are integers or enumeration values.

### Records

Records may contain any mix of types, including pointers to the record type being declared. It is possible to declare *variants* of records that exist already, where the variants will inherit all the components of the parent type in addition to its own components.

### Routines

Routines are a data type where the in-value and the out-value may be of any type.

---

## Page 124

# Type Checking

## Purpose of Type Checking

Type checking is a common denominator for checks done by the compiler to ensure that the types of variables, literals, constants and intermediate results all kinds of statements match the context they are used in. For instance, *usually* the expression `a + b =: c` makes sense if `a`, `b` and `c` are all integers or all reals, but not if they are all pointers or if two are real and one is integer. (The qualification *usually* must be included because PLANC programs can contain declarations *overloading* the `+` operator that would make "addition" of pointers acceptable to the compiler.)

## Overview of Checking in Statements

Before any overloading of operators and standard routine has taken place, the compiler will only allow reals (i.e. real literals, constants, variables or expressions) to be added to reals yielding a real result which will be passed on as an intermediate result in the expression. Likewise, only integers (i.e. the integer type and subtypes thereof) can be added to integers. Only simple types are allowed as input to standard routines.

# Initial Values of Global Variables

## Overview of Checking in Declarations

When a global simple variable is declared and given an initial value with the `:=` data initialization operator, some simple checks are done to see if the initialization value makes sense as a value of the variable.

## Simple Checks

The compiler will check that the types of literals, constants and expressions used for initialization of global variables are of the appropriate type and, in the case of composite variables, number. The expressions, in turn, can only contain constants and literals plus some operators and standard routines that operate on them.

## Overview

For instance, only integer constants/literals/expressions can initialize an integer, a real constant/literal/expression cannot.

## Expressions in Initialization Statements

When initializing simple variables with expressions, the expressions are formed with literals, constants, some operators (such as `+`, `-`, `*`, and `/`) and standard routines (such as `Addr` and `Bit_size`). The initial values of variables that have been declared previous to the current variable cannot be used. The checks made on operators and operands etc. in such ex...

[Photo: Page end]

---

## Page 125

# Initialization of Variables

pressions used to initialize simple variables are the same as those described for routines and operators below.

## If a value does not fit

Furthermore, the type of the initial value of a simple variable needs only conform to the basic type of the variable - thus an **INTEGER UNSIGNED** may be given an initial value that does not fit into one byte or which is negative. If the basic type is the same, bit-by-bit copying beginning with the *lowest* bits in the initial value is used to make the initial value.

## Records

The number of constants/literals/expressions used to initialize the components of a record must be equal to the number of components in the record.

## Arrays

The number of constants/literals/expressions used to initialize an array must be less than or equal to the number of elements in the array. The elements for which no values have been specified are set to zero.

## Sets

Sets can contain up to 256 elements and, in the case of integer sets, the lowest integer in the value range must be 0. When sets are initialized, there may be fewer elements than are indicated by the type declaration, in which cases the remaining elements are initially absent.

When initializing composite variables, the checks for simple variables are made on each of its constituent simple components. For example, when assigning a value to an element in an integer array, the value must be an integer constant or literal, or an expression with constants/literals that evaluates to an integer.

# Routines and Operators in Executable Statements

## Syntactic similarities

Syntactically, routines with zero or one parameters and unary/binary operators are used in the same way in PLANC. Routines with more than one parameter still retain some similarity to operators, because they have an in-value in front of them, and because they pass a value on to the rest of the expression of which they are a part when declared with out-values (and not placed at the end of a statement).

## Similarities in type checking

The relation between operators and routines extends into the type checking, in that the rules for type checking of in-values and parameters are the same as those for checking which variables are allowed as operands to operators. (Checking of the type of non-void out-values/results from routines/operators is done according to the rules for the rest of the statement: If followed by store or swap operators, the store/swap rules are used; if followed by other operators/routines, the rules for operators/routines are used; if followed by end of statement, no checking is done.)

## Convenient definitions in this section

In the rest of this section, it will be convenient to let *routine* mean both...

---

## Page 126

# Initial Combination Set

Before compilation starts, the compiler has an initial combination set. This initial combination set is extended as the compiler reads declarations of routines with new names or names that overload existing names. Examples of combinations that are known before the compilation of source code starts are `MOD`, which will only accept integers as in-value and parameter and which has integer out-value, and `++`, which has no in-value and takes simple types such as integers, enumerations and pointers as parameter and has the same type as out-value.

(Some types differ a little from the common pattern: `CONVERT` and `FORCE` have a type name as parameter instead of a variable name. This simply means the compiler does not have to look up the type of a variable before checking.)

# Restriction on How Combination Set Can Grow

The combination set grows as the compiler reads new routine declarations in the source code. However, note a restriction on how the set can be extended: A new routine that overloads existing ones is only accepted into the set if the number of variables in the parameter is the same as in the existing routines.

# Use of Combination Set

When the compiler checks if a routine call is correct, it first checks that its name is entered one or more times in the combination set. If so, it looks for combinations that match with the types of the in-value and the parameter in the routine call. If there is more than one match, it checks all of them to see if there is a good match, i.e., one where the types in the call correspond exactly to those in the declaration. If no good match is found, the compiler settles for the first of the reasonable matches that were found in the set.

What constitutes a good and a reasonable match for simple and composite types may vary from situation to situation, and is described in detail in the following subsections. But to get an idea of the practical implications of the preceding paragraphs, consider the routines declared as

```
ROUTINE INTEGER RANGE (0:1000), VOID &
(INTEGER2 : Par) : Mix
```

and

```
ROUTINE INTEGER2, VOID (INTEGER1 : Par) : Mix
```

---

## Page 127

# Simple and Composite Types in Routine Calls

The match in calls to `MIX` will only be good if the in-value and the parameter have been declared with types that exactly match the subtypes of integers used in one of the two declarations of `MIX`. If the call is done with integer types other than those used in either declaration of `MIX`, the match will be reasonable. In the case of reasonable matches only, the first reasonable routine will be invoked. The second routine will not be invoked when the match is reasonable after both routines have been checked.

If the parameter is a list of variable names, the compiler checks if the number of variables conforms to the number assigned in the combination set, and that the types of the variable(s) in the parameter of the routine call conform to the routine combination.

## Simple Types as In-Value and In Parameter

Let us consider for a while routines that have simple types and subtypes of simple types as in-value and in the parameter. When comparing the type of a variable with a type that is used in a valid combination, there are three possible outcomes: 

- If the two are of the same type/subtype, the match is **good**.
- If the two are so closely related that the compiler can make a conversion between them, the match is **reasonable**.
- If they are of different types, the match is **bad**.

For example: If a routine is declared with an `INTEGER4` as in-value and is called with a `REAL` variable or literal as in-value, the match is bad. In this case, the compiler will immediately start looking for another routine in the set known to it that fits the call. If it is called with an `INTEGER2` `UNSIGNED` variable, the match is reasonable because this is another subtype of the type `INTEGER`. If the routine is called with an `INTEGER4` variable, the match is good. In both the latter cases, the compiler will proceed to check the variable to see if the rest of the routine call has good or reasonable matches with the types in the declaration.

## If No Good or Reasonable Matches

If no good or reasonable matches are found among the routines known to the compiler for the in-value variable and all variables in the parameter, an error message is printed. If not all matches are good, the compiler will try to find a routine where all matches are good. If no such all-good match exists, there is an attempt to identify the "best" of the reasonable matches.

## Composite Types as In-Value and In Parameter

When a composite variable or an expression yielding a composite result is used as in-value or parameter, the variable or expression result must be of the same type as those used in the routine declaration or, in the case of records, a variant of the type used in the declaration of the routine.

This can be demonstrated by the following code fragment:

```
TYPE a = RECORD
  % ...
ENDRECORD
ROUTINE a, a (a : par) : r
```

---

## Page 128

```plaintext
par RETURN
ENDROUTINE
TYPE b = a RECORD
% ...
ENDRECORD
b : x, y, z
ROUTINE VOID, VOID : d
   x r y =: z
ENDROUTINE
```

Here, variables (and expressions) of type b can be used in the routine r, since b is a variant of a.

# Implicit type conversion

| Purpose of implicit type conversion | Implicit type conversion means adapting the bit-sizes of literals, constants and variables that are used as operands to variables or as in-values and parameters to routines to make them fit the defined operators and routines. |
|-------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|

| Example                             | For instance, the statement i + j = : i, where i is an INTEGER1 and j is an INTEGER2 implies two type conversions, one for each of the two operators in the statement. First, the variable i is converted to an INTEGER2 so the two operands to + are of equal size. The result of the operation (which will be passed on to the next operator) will be equal to the size of the biggest of the two variables, which is 16 bits in this case. Second, this 16-bit intermediate result is to be stored into an eight-bit variable. PLANC assumes that the lowest (least significant) bits are the most likely to be used, and copies the eight lowest bits into the variable i, while the highest eight bits in the 16-bit intermediate result are lost. |
|-------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|

# Operators and standard routines

The table in this section shows the priorities and operand-, in-value, out-value and parameter types of PLANC's operators and standard routines. It is a summary; detailed explanations are found in sections devoted to the individual operator/routine.
```

---

## Page 129

# The Priorities and In-values/Out-values and Parameters of Operators and Standard Routines

| Pr. | Name | Description | In-value | Parameter | Out-value |
|----|------|-------------|----------|-----------|-----------|
| 9  | \*  | Arithmetic multiplication | Integer, real | Integer, real | Integer, real |
| 11 | \*\* | Arithmetic exponentiation | Integer, real | Integer | Integer, real |
| 8  | +    | Arithmetic addition | Integer, real | Integer, real | Integer, real |
| 10 | ++   | Increment by one | Void | Integer, pointer to any type | Integer, pointer to any type |
| 6  | ,    | List item separator | Any listable | Any listable | Void |
| 8  | -    | Arithmetic subtraction | Integer, real | Integer, real | Integer, real |
| 10 | -    | Unary minus | Void | Integer | Integer |
| 10 | --   | Decrement by one | Void | Integer, pointer to any type | Integer, pointer to any type |
| 13 | .    | Access to components of records | Record variable | The name of a component in the record | The value of that component |
| 9  | /    | Arithmetic division | Integer, real | Integer, real | Integer, real |
| 8  | //   | String concatenation | Bytes | Bytes | Bytes |
| 7  | :    | Range designator | Integer, enumeration | Integer, enumeration | Void |

---

## Page 130

# The Priorities and In-Values/Out-Values and Parameters of Operators and Standard Routines (cont.)

| Pr.  | Name  | Description       | In-value                                          | Parameter                                 | Out-value                                   |
|------|-------|-------------------|--------------------------------------------------|-------------------------------------------|---------------------------------------------|
| 1, 12| =:=   | Swap operator     | Variable/literal/constant of any type            | Variable of same type as the in-value     | The value of the parameter before the swap  |
| 6    | <     | Less than         | Integer, real, set, enumeration, pointer, bytes, record | Integer, real, set, enumeration, pointer, bytes, record | Boolean                                     |
| 6    | <=    | Less than or equal| Integer, real, set, enumeration, pointer, bytes, record | Integer, real, set, enumeration, pointer, bytes, record | Boolean                                     |
| 6    | =     | Equal             | Integer, real, set, enumeration, pointer, bytes, record | Integer, real, set, enumeration, pointer, bytes, record | Boolean                                     |
| 1, 12| =:=   | Store             | Variable/literal/constant of any type            | Variable of same type as the in-value     | The value of the parameter before the swap  |
| 6    | >     | Greater than      | Integer, real, set, enumeration, pointer, bytes, record | Integer, real, set, enumeration, pointer, bytes, record | Boolean                                     |

---

## Page 131

# Priorities and Values of Operators and Standard Routines

| Pr. | Name   | Description                   | In-value                        | Parameter                                               | Out-value                                  |
|-----|--------|-------------------------------|--------------------------------|----------------------------------------------------------|------------------------------------------|
| 6   | `<>`   | Not equal                     | Integer, real, set, enumeration, pointer, bytes, record | Integer, real, set, enumeration, pointer, bytes, record | Boolean                                  |
| 6   | `>=`   | Greater than or equal         | Integer, real, set, enumeration, pointer, bytes, record | Integer, real, set, enumeration, pointer, bytes, record | Boolean                                  |
| 11  | `Abs`  | Absolute value of/cardinal number | Void                           | Integer, real, set                                       | Integer, real                             |
| 14  | `Addr` | Address of                    | Void                           | Any variable, including routines and labels              | Pointer to the location where the variable begins. |
| 3   | `AND`  | Logical and                   | Integer, Boolean, set           | Integer, Boolean, set                                   | Boolean                                  |
| 5   | `Append` | Add record to end of linked list | Record                          | Address implied range specifying a list                  | Void                                     |
| 11  | `Bit`  | Store a Boolean value in a bit | Boolean                        | 1) Any simple variable, 2) Integer number of bit to be set | Void                                     |

---

## Page 132

# Priorities and Parameters of Operators and Standard Routines (cont.)

| Pr. | Name         | Description                                      | In-value                     | Parameter                                                      | Out-value                            |
|----|--------------|--------------------------------------------------|------------------------------|----------------------------------------------------------------|--------------------------------------|
| 11 | Bit          | Get a bit value                                  | Void                         | 1) Any simple variable, 2) Integer number of bit to be set     | Boolean                             |
| 11 | Bit_position | Find position of first bit of a record component | Void                         | Component with optional dot notation                          | Integer                             |
| 11 | Bit_size     | Find size of data type                           | Void                         | Element/component with optional dot notation                   | Integer                             |
| 11 | Blocksize    | Set blocksize of a file                          | Integer                      | Integer                                                        | Void                                 |
| 11 | Close        | Close a file                                     | Void                         | Integer                                                        | Void                                 |
| 11 | CONVERT      | Convert to or from real or integer types/subtypes| Integer or real type/subtype | Type expression giving integer or real type/subtype           | Integer or real type/subtype        |
| 11 | Dispose      | Deallocate dynamically allocated data            | Void                         | Pointer to type to be disposed                                  | Void                                 |
| 1  | ERRETURN     | Return from routine with ERRCODE set             | Integer                      | Void                                                           | Void                                 |

---

## Page 133

## The Priorities and In-values/Out-values and Parameters of Operators and Standard Routines (cont.)

| Pr. | Name   | Description                   | In-value                      | Parameter                                   | Out-value                      |
|-----|--------|-------------------------------|-------------------------------|---------------------------------------------|--------------------------------|
| 11  | Filesize | Read file size                | Void                          | Integer                                     | Integer                        |
| 11  | Filesize | Set file size                 | Integer                       | Integer                                     | Void                           |
| 11  | FORCE   | Interpret data element        | Variable                      | Type expression                             | Variable                       |
|     |        | as another type               |                               |                                             |                                |
| 5   | IN     | Membership                    | Integer, enumeration, pointer | A set, or a range of integer, enumeration, or pointer | Boolean                      |
| 14  | Ind    | Get value of variable pointed to | Void                         | Pointer to a type                           | The value of the variable      |
| 11  | Input  | Read formatted input          | Void                          | 1) Integer, 2) bytes, 3) variable of any type | Integer                        |
| 11  | Input  | Read unformatted input        | Void                          | 1) Integer, 2) integer, 3) bytes            | Integer                        |
| 5   | Insert | Add record to head of list    | Record                        | Address-implied range                       | Void                           |
| 5   | Insert | Add value to set              | Integer, enumeration          | Set                                         | Void                           |

---

## Page 134

# The Priorities and In-values/Out-values and Parameters of Operators and Standard Routines (cont.)

| Pr. | Name         | Description                      | In-value                  | Parameter                                    | Out-value                             |
|-----|--------------|----------------------------------|---------------------------|----------------------------------------------|---------------------------------------|
| 11  | Maxindex     | Get upper bound of an array      | Void                      | 1) Array identifier 2) Integer constant or literal | Integer                               |
| 11  | Minindex     | Get lower bound of an array      | Void                      | 1) Array identifier 2) Integer constant or literal | Integer                               |
| 11  | MOD          | Modulo                           | Integer                   | Integer                                      | Integer                               |
| 11  | Monitor_call | Execute a SINTRAN III monitor call | Void                      | 1) Integer or bytes 2) Parameters            | Void                                  |
| 11  | New          | Dynamically create a new variable | Void                     | Type of new variable                         | Pointer to the new variable           |
| 4   | NOT          | Logical negation                 | Integer, Boolean, set     | Integer, Boolean, set                        | Boolean                               |
| 11  | Open         | Open a file                      | Void                      | 1) Integer 2) bytes 3) bytes 4) bytes        | Void                                  |
| 2   | OR           | Logical inclusive OR             | Integer, Boolean, set     | Integer, Boolean, Boolean, set               | Boolean                               |
| 11  | Output       | Formatted output                 | Void                      | 1) Integer 2) Bytes 3) The type specified    | Integer                               |

---

## Page 135

### The Priorities and In-values/Out-values and Parameters of Operators and Standard Routines (cont.)

| Pr. | Name             | Description                                           | In-value                             | Parameter                                    | Out-value          |
|-----|------------------|-------------------------------------------------------|--------------------------------------|----------------------------------------------|--------------------|
| 11  | Output           | Random unformatted output                             | Void                                 | 1) Integer 2) integer 3) bytes               | Integer            |
| 11  | Pred             | Get enumeration value preceding current               | Void                                 | Enumeration value or variable                | Enumeration value  |
| 5   | Remove           | Remove a record from a linked list                    | Record                               | Address implied range                        | Void               |
| 5   | Remove           | Remove value from set                                 | Integer or enumeration literal/constant/variable | Set                | Void               |
| 1   | RETURN           | Return from called routine to calling routine         | Any value or void                    | Void                                         | Void               |
| 8   | SHIFT            | Shift bits                                            | Integer                              | Integer                                      | Integer            |
| 11  | Size             | Storage size of data type in bytes                    | Void                                 | Identifier or data type                      | Integer            |
| 11  | Succ             | Get enumeration value succeeding current              | Void                                 | Enumeration value or variable                | Enumeration value  |
| 11  | Programmer Defined | The priority of your own routines if PRIORITY is not used | Any                                 | Any                                          | Any                |
| 2   | XOR              | Logical exclusive or                                  | Integer, Boolean, set                | Integer, Boolean, set                        | Boolean            |

---

## Page 136

# Stacks

## A stack is an array

A stack is an array that the program uses to store lists of routine descriptions - also known as **stackframes** - in routine call sequences, plus transient data such as parameters that are transferred to routines and intermediate results of expressions used in the routines. In addition, objects created with the new routine are put on the stack if they are not explicitly put somewhere else.

## Stackframe list

The lists of stackframes are doubly linked. The reason for this linking also explains some of the nature of the stack usage, because the address of the stackframe is kept permanently in one of the CPU's registers during execution of the routine so that addressing into the stack can be done relative to this register. The address of the previous stackframe is kept in the current stackframe, so that the current routine knows where to return after it has returned to its caller. The address of the next free stackframe is also kept in the current stackframe. It is kept ready in case the current routine calls a new routine.

## Inspecting the stack

The stackframe linking makes it possible for you to look at and retrace the routine call sequences in your program. When you use ND's **Symbolic Debugger**, you can do this with the commands **ACTIVE-ROUTINES**, which shows the current call sequence, and **LOOK-AT-STACK**, which gives details of the stackframe and the transient data (parameters and intermediate results).

(If you do a **LOOK-AT-STACK** on the stack of a PLANC program on the ND-500(0), you may be puzzled by the fact that the field called **NUMBER OF PARAMETERS** is always zero even if the routine called has parameters. This is because the number of parameters is not needed by ordinary PLANC routines. However, if you use other routine modifiers such as STANDARD, this field in the stackframe is used.)

Routine calls are important elements in any program, and computers must handle them efficiently and securely. That is, there should be minimal time penalties for ordering code into subroutines, and stackframes or transient data should not overwrite other parts of the program by accident. (Such overwrites will lead to obscure error situations!)

For these reasons, most CPUs have special instructions for putting stack- frames onto the stack ("pushing") and retrieving stackframes from the stack when the subroutine called has terminated ("popping").

## Stack under-/overflow

Additional facilities may exist in the hardware to keep the pushing and popping inside the areas of memory designated for stack usage. Then, if the program tries to push past the end of the stack areas, you get a stack

---

## Page 137

# Stack Initialization

Stacks must be initialized by the program before they can be used. It is evident from explanation above that a stack area is not just like any other part of the memory used by a program, especially if there are trap-handlers in the hardware to prevent stack overflows and underflows. So the hardware must be notified about the first stackframe pointer, that of the main program, and about the limits of the stack so that over/underflows can be trapped. This is what PLANC's `Initstack` statement does for you.

## Initstack

In some CPUs, special instructions for preparing stacks have been implemented as well, most notably on the ND-500(0). But in any case, `Initstack` sets up a new stack for usage.

## Several Separate Stacks

Normally, only one `Initstack` statement is needed in a PLANC program. However, the possibility PLANC gives you to initiate a new stack in the declaration part of outer module-level routines is useful in some situations.

When a routine contains an `Initstack` statement, the program will stop using the previous stack and use the new stack both for that routine and all routines called by it (that is, until yet another `Initstack` declaration is encountered). Upon `RETURN` or `ERRORRETURN` from the `ROUTINE`, the previous stack is used again. Thus, the previous stack is completely untouched by whatever the `ROUTINE` being left did.

## Stacks and Other Languages

This property may be desirable when you call `ROUTINE STANDARDS` for programs written in other languages. Using it, you avoid doing harm to whatever was on the stack of the previous code. So when you make routines and libraries that will be used by code written in other languages, start the routines with an `Initstack` declaration.

Another situation where the flexible-stack designation scheme may be of some value, is when you do not have much dataspace available (such as on the ND-100 and under MS-DOS). Then you may want to use a part of the dataspace as, for example, a heap for dynamic data structures in one part of the program and as a stack in another part of it.

## Trapping Stack Over/Underflow

If you work on an ND-500(0), you can use that CPU's traphandling facilities to prevent stack overflows. The traps can be set using the PLANC utility routine `utDefineTraps`, and the trap you need to set to catch stack overflows is number 27 decimal.

---

## Page 138

# Parameter Transfer

Parameters are normally transferred as values, i.e., copied onto the stack. Two important exceptions are `ARRAYS` and `RECORDS`, which are passed as pointers. The consequence of this is that changes done to array/record components by the called routine will be in effect for the calling routine after `RETURN` from the called subroutine, while changes to simple variables passed as parameters will not be in effect after the `RETURN` statement.

## Default Modifiers

The default access mode for code inside a routine to its parameters and in-value is `READ` only. The access to the in-value cannot be changed, but if you want the routine to change the value of a parameter, you must modify it with `WRITE` or `READ WRITE` in the routine declaration. Changes done to `WRITE` or `READ WRITE` parameters during routine execution will be in effect after return from the routine.

## Arrays as Parameters

`ARRAYS` are passed as pointers, with a pointer to a "virtual origo" (the address of the "zero'th" element of the array which all addresses in the array are relative to), a lower limit and an upper limit. For each dimension greater than one, this descriptor is extended with the number of elements in the previous dimension, along with the new upper and lower limits of the new dimension. Therefore, array parameters take more than one word on the stack, but less space than if copying the whole array. Furthermore, it is convenient to declare `ARRAYS` that are going to be passed to `STANDARD` routines with `lower limit equal to 0`.

## Records as Parameters

`RECORDS` are passed as pointers to their first address. Thus, a record as a parameter occupies one word on the stack, regardless of the size of the record.

# Portable Programming in PLANC

The following constructions may cause problems when a PLANC program is ported to another CPU:

### FORCE

If you `FORCE` a `POINTER` to an `INTEGER` on the ND-500(0) you will get into trouble if you port to the Intel-286 CPU, where the default `INTEGER` size will be 2 bytes and a `POINTER` will have four bytes, causing an error from the PLANC compiler. In addition, you usually force a pointer to an integer to do some arithmetic on.

---

## Page 139

# Equivalence (=)

Equivalencing may cause a lot of problems because it depends on the size, alignment and representation of the variables involved. A common reason for equivalencing is to access the `MinIndex` and `MaxIndex` of a single-dimension `ARRAY POINTER`. A portable way of doing this, if you want new values for the `MinIndex` and `MaxIndex` of `byp`, is:

```
Addr(Ind(byp) (new_min:new_max)) =: byp
```

If you want direct access to the `MinIndex` and `MaxIndex`, you can still do it portable thus:

```
BYTE POINTER : paddr % Leave us
INTEGER : pmin, pmax % together!
BYTES POINTER : byp = paddr
```

# Monitor Calls

`Monitor_calls` and other operating-system dependent constructs should be avoided or collected in a separate module containing general routines making a logical abstract interface to the environment that will be easy to change later.

# $* INLINE ASSEMBLY

The same as `Monitor_calls`.

# Access to "external" pointers

An external pointer is a pointer you get from or give to a part of your system that may run on a different process, CPU or computer. Such pointers often have to be converted some way or other, or the objects they point to need to be converted. Consequently, you should make a few general routines for accessing this kind of pointers.

# Size of Integers

All integer variables that may contain large numbers (abs > 32767) or that may be sensitive to the way they are used in expressions should be declared with an explicit range. You may use `INTEGER RANGE(min:max)` or `BYTE`, `INTEGER1`, ... A new feature in the I version of PLANC is that you are now able to declare unsigned integer variables:

```
INTEGER2 UNSIGNED : u16
```

# RECORD PACKED

Problems may arise if some components of a record must align their first bit on fixed displacements from the start of the record. This is common when porting to new CPUs or when sending records in messages between different CPUs. To be optimal and as a consequence of restrictions on the different CPUs, the different PLANC versions differ much in how record elements are packed, and there is no simple rule as to how this is done.

---

## Page 140

# MOD and Bit Position Constructs

To help this situation, the `MOD` and `Bit_position` constructs can be used. You may declare an element in a record in the following way:

```
INTEGER2 : length MOD 1
```

The `length` variable will be put on the next byte after the previous variable in the record. If you put `MOD 2` after the variable, the variable will be put on the next displacement that is a multiple of 2 relative to the start of the record and so on.

Check that you have got the displacements you want with the construct `Bit_position(record_element)`.

If you want to check that the record element length has displacement 7 relative to the start of the record, you can do it like this:

```
$IF Bit_position(length)/8 >< 7 $THEN
  $MESSAGE Error in RECORD layout !!!
  $EXIT % Terminate compilation
$ENDIF
```

If you declare "sensitive" records in an `$INCLUDE` file with this compile-time check in it, and use it in all dependent systems, you should be fairly certain of getting no problems.

# Equivalence Operator

Another construct that often causes problems inside packed records is the equivalence operator `=`. This is not implemented the same way for packed records as unpacked records. In nonpacked records, the alignment is on the most significant bit in the equivalenced variables, whereas in packed records, it causes the least significant bits in the two variables to be aligned.

There now is a new equivalence operator, `>=`, that aligns on the most significant bit both in packed and unpacked records.

# File Names

File names differ in layout on different operating systems, and therefore should be used carefully. In the `Open` statement, it is useful to split name and extension. Then you do not have to worry about having a `.` or a `:` to separate them. Example:

```
Open(fno, 'ACCESS', 'NAME', 'EXT')
```

Also the access and the contents of the files you operate on may differ on different operating systems. You should be aware of this when you write your program. If you port a program scanning text files from SIN-TRAN-III to Unix and your program scans to `CR` (= 15B) to find the end of a line. Then this will work badly on Unix because there, the lines are

---

## Page 141

# Calls to/from Other Languages

If you want to call or get calls from other languages you have to verify in each case that this is possible and check how the parameters have to look. For example, if you want to call a routine in C with a `BYTES` as parameter, you cannot just put the `BYTES` in the parameter list because C does not have `ARRAY POINTERS` like PLANC. What you have to do is to split the `BYTES` into a `BYTE POINTER` and an `INTEGER` containing the address of the start and the length of the `BYTES`. If the `BYTES` is `b` then you call the C-routine crout like this:

```
crout (Addr (b(Minindex(b))), Size(b))
```

(Note: `Minindex(b)` without the `,1` in one-dimensional arrays is allowed in the I-version of PLANC.)

# Some General Comments

All constructs like those mentioned above that may differ on different computers should be put in a separate module containing general routines doing the operations that are machine-dependent. That way you know in advance where to make the changes when you port the system and you get a minimum of changes that need to be done. Both in the case of machine-dependent constructs and other operations that are tricky, it is useful to make a general routine doing it and use this everywhere. That way you will ease the maintenance of your system.

Declarations like the `RECORD PACKED` described above that are needed by other parts of your system should be put in an `$INCLUDE` file and included in all the parts. That way you only have to make changes one place if something needs to be changed. Making general routines that you use everywhere, keeping declarations needed many places in include files and giving names to all constants in your system will ease both porting and general maintenance of your system.

Making `$INCLUDE` files for the purpose of having declarations needed many places in one place and for importing all routines from a system is useful, but one should not put everything in `$INCLUDE` files. If you have too many of your definitions in `$INCLUDE` files, you will have problems finding them later on. When you compile your PLANC program on a new computer you may get some warnings that you didn’t get before. Do not ignore these. If you try to force a `POINTER` to an `INTEGER` on the ND-500(0) CPU everything will work fine, but if you port this program to the ND-100 CPU you will get the warning:

```
Illegal data-element to be converted
```

If you ignore this warning your program probably will fail and the error will be very difficult to find by debugging. In general you...

---

## Page 142

should never ignore warnings from the PLANC compiler. Often they are fatal and will cause errors that are difficult to find.

---

## Page 143

# PLANC Reference

---

### All keywords and commands

This chapter contains sections describing all PLANC's special symbols, keywords and compiler commands. It is ordered alphabetically (hence, it begins with the end-of-line character) with compiler commands marked with a dollar sign and compiler options with a percent sign.

### Operator and routine priorities

If a token represents an operator or a standard routine, its priority is given in the section heading.

### Limited syntax descriptions

The sections on the individual keywords and commands most often contain a syntax description designed to give you the necessary information to use the keyword correctly. No attempt is made in this chapter to give a complete grammar of PLANC. (PLANC has grown according to the needs of its users, and a side-effect of this is a quite big grammar.)

### Description of operators and standard routines

A somewhat unusual property of the PLANC language is its extendability. PLANC's set of legal operator/operand and routine/in-value/parameter combinations is somewhat limited when compilation starts, but the set may be extended during compilation by overloading existing standard routine and operator names with new versions of said routines and operators.

To make clear what combinations are in the compiler's initial combination set (i.e. allowed by the compiler when compilation starts), the syntax of operators and standard routines has been described in terms of routine declarations that show precisely how they are used. Also note that all binary operators and standard routines are left associative.

---

### (end-of-line - statement delimiter)

| Category | Special character. |
|----------|--------------------|
| Semantics | End-of-line is the most common statement separator in PLANC. If you want more than one statement on a line, use the semicolon (;) as statement separator. |
| NOTE: Line continuation | If you want a statement to take more than one line, an ampersand (&) denotes that the statement continues on the next line. |
| NOTE: Implicit line continuation | A couple of special characters imply line continuation, so that if they are the last character on the line, no ampersand is needed. Hence, end- |

---

## Page 144

# NOTE: String Continuation

of-line is not end of statement if it follows a comma (,) in a list of items or if it follows either a comma or a semicolon (;) in a list of routine parameters.

Furthermore, if a bytes literal is too long to fit conveniently into one line, it may be split into several parts that end with an ampersand. For example, the three byte strings declared and initialized here are equal:

```
BYTES: b1 := 'a b c',
       b2 := 'a &
              b &
              c',
       b3 := 'a ' // &
              'b ' // &
              'c'
```

# % (%%) (Comment Delimiters)

| Category   | Special characters. |
|------------|---------------------|
| Semantics  | There are two kinds of PLANC comments: those which begin with a % (percent sign) and end with an end-of-line, and those which are enclosed between the (% and the %) signs. While the former can only be used before the end-of-lines unless the line contains comments only, the latter may be embedded anywhere in the source code, including inside other comments of the (% ... %) type. |

## Note on the % Comment
A quite destructive compile-time error may occur if you put a single % inside a string of bytes, as the compiler will assume that the string continues on the next line, and proceed to read the rest of the program as a string if it does not find a terminating '. (You will get a warning if this happens.) To include a % in a bytes string, use a double percent sign: %%.

## Note on the (%%) Comment
This kind of comment can be nested, thus:

```
(% Comment (% Comment inside comment %) here %)
```

# " (Macro Parameter Delimiter)

| Category   | Special character.             |
|------------|--------------------------------|
| Semantics  | Double quotes surround formal parameters in macro bodies. See $MACRO for details. |

---

## Page 145

# # (get ASCII value of byte)

| Category   | Special character. |
|------------|--------------------|
| Semantics  | This special character applied to an ASCII character returns the ASCII value of that character. It is also used in the Ada notation for numbers. See Ada notation. |
| Example    | `#& =: i`, where `i` is an integer, will store the ASCII value of `&`, which is 38, into `i`. |

# $ (line shift in Output statements)

| Category   | Special character. |
|------------|--------------------|
| Semantics  | This character is special only inside strings that are output with the standard routine `Output`. |
| Example    | The following statement:<br>`Output(1, 'a', 'Line one$Line two$')`<br>Will result in the following output to screen:<br>`Line one`<br>`Line two` |

# & (continuation of statement line)

| Category   | Special character. |
|------------|--------------------|
| Semantics  | The statement continues on the next line. See "end-of-line". |

# ' (byte string delimiter)

| Category   | Special character. |
|------------|--------------------|
| Semantics  | Byte literals, which are used in initialization of and assignment to byte strings, must be enclosed in `'` (single quote) signs. Special characters that |

---

## Page 146

# Example

To output the string `Smith & Co.: $35,720 or 41 %` to the terminal, write

`Output(1, 'a', 'Smith && Co.: $$35,720 or 41 %%')`

# * (multiplication) - 9

## Category

Binary operator, priority 9.

## Syntax

```
multiplication ➞
    ROUTINE INTEGER, INTEGER (INTEGER) : *
    ROUTINE REAL, REAL (REAL) : *
```

## Semantics

Multiplication of numbers. If the operands are of subtypes of unequal size, the out-value is converted to the bigger of the two subtypes.

# $* (inline assembly follows)

## Category

Compiler command.

## Semantics

Indicates that the rest of the line is instructions in assembly. More than one assembly statement can be written on one line provided the statements are separated by a semicolon.

The inline assemblers are 90-95% similar to the ND-100 assembler (MAC), the ND-500(0), the MC680x0 and the Intel 80x86 assemblers. The differences are:

- Numbers can be given instead of instruction mnemonics.
- The compiler will fill in the appropriate addressing mode when referencing variables declared in the PLANC program. Therefore, PLANC identifiers must be used in the assembler instructions without special addressing mechanisms such as via base registers or indirection.

## Example, ND-100

```
$* LDA 0; X; SAD SHR 20; SAT 4; RDIV ST
```

---

## Page 147

# Examples

## Example, ND-500(0)

```
$* W1 DIV4 B.24B:S,4,W2
```

## Example, MC68000

```
$* MOVE 22B(A6),D0; EXT.L D0; DIVS 4B,D0
```

## Note

Record components cannot be reached through the customary dot notation. Neither will you get the right addressing mode when accessing components inside `USING` blocks for the record. The following example shows what you will have to do on the ND-500(0):

```
TYPE x = RECORD
  INTEGER : y
ENDRECORD

x : xx

USING xx
  $* W1 := y $ Gives 0, which is the displacement within x
  $* R := xx
  $* W1 := R.y $ Gives y
ENDUSING
```

Also take care to avoid name conflicts between your variables and assembler mnemonics, such as the I for indirection on the ND-100.

## Manuals

The syntax of the inline assembler instructions is described in the following manuals:

- ND-100 Reference Manual, ND-806014
- ND-500 Reference Manual, ND-805009
- MC68000 16 BIT MICROPROCESSOR User's Manual (third edition)
- iAPX 286 Programmer's Reference Manual
- 80386 Programmer's Reference Manual

# Exponentiation

```
** (exponentiation)  -  11
```

| Category | Binary operator, priority 11. |
|----------|-------------------------------|

## Syntax

```
exponentiation ➝
  ROUTINE INTEGER, INTEGER (INTEGER) : **
  | ROUTINE REAL, REAL (INTEGER) : **
```

## Semantics

Used to compute integer powers of reals and integers.

---

## Page 148

# + (Addition) - 8

**Category**  
Binary operator, priority 8.

**Syntax**  
addition ➜
```
ROUTINE INTEGER, INTEGER (INTEGER) : +
ROUTINE REAL, REAL (REAL) : +
```

**Semantics**  
Adds numbers. If the operands are of subtypes of unequal size, the outcome value is converted to the bigger of the two subtypes.

# ++ (Command Processor Value Increment)

**Command processor option increment.**

**Semantics**  
This operator and its reverse operator, --, are an alternative to the ON/OFF options in many commands. If listing is initially off when compilation starts, you can switch it on with one or more $LISTING ++ commands. Following this, the same number of $LISTING -- commands must be given to turn listing off again.

# ++ (Increment Variable) - 10

**Category**  
Unary operator, priority 10.

**Syntax**  
increment ➜
```
ROUTINE VOID, INTEGER (INTEGER) : ++
ROUTINE VOID, any_type_expression POINTER (any_type_expression POINTER) : ++
```

**Semantics**  
Increments integer variable to the right by one, or to increase the address pointed to by a pointer by a number of bytes equal to the size of the variable the pointer points to. This is useful when the pointer points to record elements in an array - applying ++ to the pointer makes it point to the next record in the array.

---

## Page 149

# , (list item separator) - 6

**Category**

Binary operator, priority 6.

**Semantics**

The comma is used to build lists of items in declarations and statements. It can be considered to be a binary operator with no out-value which cannot be overloaded. In declaration statements, the items must be of the same type except in the parameter list of routine declarations or of components in initialization statements for records, where the parameters may be of any type. In executable statements, the same format as for initialization of records can be used in stores to records. In for-loop value lists, the items must be single values or ranges (including array and pointer-implied ranges) of the type of the loop control variable.

# - (subtraction) - 8

**Category**

Binary operator, priority 8.

**Syntax**

```
subtraction ➝
  ROUTINE INTEGER, INTEGER (INTEGER) : -
  | ROUTINE REAL, REAL (REAL) : -
```

**Semantics**

Subtracts numbers. If the operands are of subtypes of unequal size, the out-value is converted to the bigger of the two subtypes.

# - (change sign) - 10

**Category**

Unary operator, priority 10.

**Syntax**

```
unary_minus ➝
  ROUTINE VOID, INTEGER (INTEGER) : -
  | ROUTINE VOID, REAL (REAL) : -
```

**Semantics**

To change the sign of the operand (or parameter).

# -- (command processor value decrement)

**Category**

Command processor option decrement.

---

## Page 150

# Semantics

See the entry for its reverse, ++.

## -- (decrement) - 10

| Category | Unary operator, priority 10. |
|----------|------------------------------|

### Syntax

```
increment ->
  ROUTINE VOID, INTEGER (INTEGER) : --
  ROUTINE VOID, any_type_expression POINTER (any_type_expression POINTER) : --
```

### Semantics

Decrements integer variable to the right by one, or to decrease the address pointed to by a pointer by a number of bytes equal to the size of the variable the pointer points to. This is useful when the pointer points to record elements in an array, as applying -- to the pointer makes it point to the previous record in the array.

## . (record component access) - 13

| Category | Binary operator, priority 13. |
|----------|-------------------------------|

### Syntax

```
dot_access ->
  ROUTINE record_variable, component_type (component_name) : .
```

where `record_variable` is a variable of any record type or a pointer to a variable of any record type, and `component_name` is the name of a component of that type.

Spaces are allowed before or after the dot (but probably not very useful).

### Semantics

This operator gets the value of the component given as parameter from the record or record pointer given as in-value.

## / (division) - 9

| Category | Binary operator, priority 9. |
|----------|------------------------------|

### Syntax

```
division ->
  ROUTINE INTEGER, INTEGER (INTEGER) : /
  ROUTINE REAL, REAL (REAL) : /
```

---

## Page 151

# Semantics

To divide an integer by an integer or a real by a real. Integer division may give a remainder, which may be computed with the `MOD` operator and the same operands.

## // (string concatenation) - 8

**Category**  
Binary operator, priority 8.

**Syntax**  
```
concatenation ➝
   ROUTINE BYTES, BYTES (BYTES) : //
```

**Semantics**  
This operator concatenates two strings of bytes.

## : (data declaration indicator)

**Category**  
Special token.

**Syntax**  
```
declaration ➝
   type_expression : variable_list

variable_list ➝
   variable 
   | variable variable_list

variable ➝
   name optional_range optional_initialization

optional_range ➝
   empty 
   | (range_indication)

optional_initialization ➝
   empty 
   | := variable_initialization variable_initialization ➝
   
simple_initialization 
   | composite_initialization

simple_initialization ➝
   constant_expression

composite_initialization ➝
   (variable_initialization_list)

variable_initialization_list ➝
   variable_initialization_item 
   | variable_initialization_item variable_initialization_list

variable_initialization_item ➝
   optional_repeat variable_initialization

optional_repeat ➝
   [illegible]
```

---

## Page 152

# Semantics

A type expression followed by a colon gives the compiler a data structure and instructs it to reserve space in memory for one or more variables according to that definition. If the variables are simple, a valid constant expression (see `CONSTANT`) can be used to initialize them, while composite variables that are being initialized must have the values of their components enclosed in parentheses.

## Declaring ARRAYs

One of the composite type constructors, `ARRAY`, carries with it a need to specify a value range for indexing the elements of the array. This can be done in one or both of two ways: Either by an explicit range indication (see `:` and `ARRAY`), or by being initialized to contain the number of elements that is specified in its initialization. If both methods are used, note that there cannot be more values in the initialization list than there are index values in the array. If the initialization list gives fewer values than the array can hold, the remaining values are initialized to ASCII zero.

Also note that you can assign the same value to more than one element in an array by prefixing the value with a parentheses containing a multiplication sign (`*`) and the number of consecutive elements that are going to receive that value.

## Declaring RECORDs

The `RECORD` composite constructor can also be initialized. In this case, all components must be initialized, and the type and order of the initial values must exactly match the type and order of declaration of the components. This implies that if the record has another record type as a component, its initialization must contain a composite initialization for this other record type, complete with an enclosing parentheses. See also `RECORD`.

| : (range indicator) | - | 7 |
| ------------------- |---|---|

### Category

Binary operator, priority 7.

### Syntax

In the following syntax description, note that the first of the two colons is a data declaration indicator as described in the previous section, while the second is the name given to the range indication operator.

```
range ->
    ROUTINE INTEGER, integer_range (INTEGER) :
    | ROUTINE enumeration_value, enumeration_range & 
      (enumeration_value) :
```

---

## Page 153

# Syntax

```
ROUTINE any_type POINTER, pointer_implied_range ∈ 
(any_type POINTER, ) : :
```

## Semantics

To indicate a range of values in declarations and statements. The operands must be of the same type. The operands may be either integers or enumerations, or pointers to records in `FOR` blocks, `Append`, `Insert` and `Remove` statements. (See `FOR`, `Append`, `Insert` and `Remove`).

# `:=` (variable initialization)

## Category

Declaration clause.

## Semantics

The use of this token in a declaration indicates that the variable which has just been declared is going to contain a value when the execution of the code starts. The number of elements in the initialization may also indicate the number of elements the array being declared is going to have. For a general description of declarations, see the data declaration indicator above.

# `:=:` (variable value swap) - 1 and 12

## Category

Binary operator, priority 1 and 12. The lefthand operand (or in-value) to this operator is always finished before the `swap` operation can take place and the righthand (or parameter) side is executed after the swap has been done.

## Syntax

```
swap ➔
ROUTINE simple_type_value, simple_type_value (simple_type_variable)
: :=:
```

Where `simple_type_value` and `simple_type_variable` all are of the same type.

## Semantics

This operator swaps an intermediate result in an expression with the contents of a variable, so that the storage location of the variable contains the intermediate result while the previous contents of the variable is used in the rest of the expression instead of the intermediate result. The left- and righthand sides must be of the same simple types/subtypes. (Note that only simple types can be used, while the `=` works with both simple and composite types.)

---

## Page 154

# `;` (Statement and Routine Parameter Separator)

## Category
Special character.

## Semantics
This is a statement and routine parameter separator.

If you want to have more than one declaration or executable statement on a line (including assembly statements after the $* compiler command), they must be separated by a semicolon.

When routine parameters are declared with type *and* name inside a parentheses between the out-value and the colon of the declaration, each *type: name* pair must be separated from the next with a semicolon. Lines may be continued on the next line after such semicolons without use of the special character `&`.

## Examples
More than one statement per line:

```
x+y =: z; z/2 =: x; ++x =: y
```

Routine declaration:

```
ROUTINE VOID, VOID (INTEGER : a; BOOLEAN : b;
INTEGER POINTER : ip) : DoSomething
```

# `<` (Less Than) - Priority 6

## Category
Binary operator, priority 6.

## Syntax

```
less_than ➝
ROUTINE INTEGER, BOOLEAN (INTEGER) : <
| ROUTINE REAL, BOOLEAN (REAL) : <
| ROUTINE BYTES, BOOLEAN (BYTES) : <
| ROUTINE enumerated_type, BOOLEAN (enumerated_type) : <
| ROUTINE set_type, BOOLEAN (set_type) : <
| ROUTINE any_type POINTER, BOOLEAN (any_type POINTER) : <
```

## Semantics
Returns true if the size of the lefthand operand is less than the righthand operand, or if the lefthand set is a subset of the righthand set. The operands must be of the same type. They can be integers, reals, strings of bytes, pointers to variables of the same type, enumeration values of the same type, and sets of the same type.

---

## Page 155

# Strings

In the case of comparisons of strings, the test is done as follows:

If the strings are of equal length in bytes, the bytes will be matched pair-wise (one byte from each string) from the lowest index and up, and the out-value is determined by the ASCII values of the first unequal byte pair, if any.

If the strings are of unequal length, the test proceeds as if the shorter string were padded with ASCII nulls to the same length as the longer string.

# Pointers

When comparing pointers, the outcome depends on the memory addresses of pointers.

# Enumerations

The outcome of a comparison of two enumeration values depends on the order in which the values were listed when the enumeration type is declared. If they were declared as, say, (Red, Green, Blue), then Red < Blue is true.

# Sets

The outcome depends on set relationships, such as one set being a subset of another, being equal to or a subset, being disjoint etc.

# <= (less than or equal) - 6

| Category      | Binary operator, priority 6.  |
|---------------|-------------------------------|
| Syntax        | See <.                        |
| Semantics     | Returns true if the size of the lefthand operand is less than or equal the righthand operand, or if the lefthand set is equal to or a subset of the righthand set. For details, see <. |

# = (equal) - 6

| Category      | Binary operator, priority 6.  |
|---------------|-------------------------------|
| Syntax        | equals ->                     |

```
ROUTINE INTEGER, BOOLEAN (INTEGER) :=
ROUTINE REAL, BOOLEAN (REAL) :=
ROUTINE BYTES, BOOLEAN (BYTES) :=
ROUTINE enumerated_type, BOOLEAN (enumerated_type) :=
ROUTINE set_type, BOOLEAN (set_type) :=
ROUTINE record_type, BOOLEAN (record_type) :=
```

---

## Page 156

# Technical Document Page

## Routine Declaration

| Element       | Description                                                                                                                                                                                  |
|---------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Semantics     | Returns true if the size of the lefthand operand is equal to the righthand operand, or if the lefthand set is equal to the righthand set. For details about types that are not records, see <. |
| Records Special | This operator can be applied to record types. The result of the test is true if the two records have the same bit-by-bit content in addition to being of the same type. You may also compare two records where one is a variant of the other. In this case, the bit-by-bit comparison will be done on all common components. |

## Address Equivalence

### Syntax

``` 
type expression : new variable name = existing variable name
```

| Element  | Description                                                                                                                                                        |
|----------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Category | Special character                                                                                                                                                  |
| Semantics | This character is used in declarations to make the variable being declared lie at the same address in memory as a variable that has been declared previously.   |
| Example  | In this example, the eight byte real will start at the first byte of the first four byte integer. Since the second integer immediately follows after the first one in memory, the last four bytes of the real will lie at the same addresses as the second integer. |


```
INTEGER4 : I1, I2
REAL8   : R12 = I1
```

## Store Value in Variable

### Syntax

```
store ➔
ROUTINE any_type_value, any_type_value (any_type_variable) := :=
```

| Element  | Description                                                                                                                                                                                                                            |
|----------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Category | Binary operator, priority 1 and 12. The lefthand operand (or in-value) to this operator is always finished before the store operation can take place, and the righthand (or parameter) side is executed after the store has been done. |

---

## Page 157

# Operators

## > (greater than) - 6

**Category**

Binary operator, priority 6.

**Syntax**

See `<`.

**Semantics**

Returns true if the size of the lefthand operand is greater than the righthand operand, or if the lefthand set is a superset of the righthand set. For details, see `<`.

## >< (unequal) - 6

**Category**

Binary operator, priority 6.

**Syntax**

See `=`.

**Semantics**

Returns true if the size of the lefthand operand is equal to the righthand operand, or if the lefthand set is unequal to the righthand set (i.e. each of the two sets has one or more members which are not member of the other set). This operation is the negation of `=`. For details, see `=`.

## >= (greater than or equal) - 6

**Category**

Binary operator, priority 6.

**Syntax**

See `<`.

**Semantics**

Returns true if the size of the lefthand operand is greater than or equal to the righthand operand, or if the lefthand set is a superset of the righthand set. For details, see `<`.

---

## Page 158

# ? (predeclaration)

| Category  | Special character. |
|-----------|--------------------|
| **Syntax** | `predeclaration ➝ type_expression : variable_name?` |
| **Semantics** | Predeclaration. The variables (data or routines) named in front of the question mark are "announced" to the compiler, which will expect them to be declared later. After a variable has been predeclared, it can be used just as if it had been declared. Predeclarations are most often used when one variable/routine depends on the contents of another, such as in mutually recursive routines. |

# @ (in-value name in routine)

| Category  | Special character. |
|-----------|--------------------|
| **Semantics** | This character represents the in-value to a routine inside the routine block. It can be used in expressions just like any other variable, with the exception that it cannot be stored to. |

# Abs (absolute value) - 11

| Category  | Standard routine, priority 11. |
|-----------|--------------------------------|
| **Semantics** | Returns the absolute value of the integer or real given as parameter or the cardinal number of the set given as parameter. |

# Ada notation

| Category  | Notation. |
|-----------|-----------|
| **Semantics** | Integer literals can be written with the Ada notation, which makes it possible to write numbers with any radix between 2 and 32. (Also remember that the _ (underscore) character can be used to group digits together.) |

---

## Page 159

# Example

**CONSTANT** NoParity = 16#7F#, Nonsense = 2#1101_1001_000#

# Addr (pointer to) - 14

| Category | Standard routine, priority 14. |
|----------|--------------------------------|
| Semantics | Returns a pointer to the memory location of the parameter, which may be of any type including routine and label. If the parameter is an array, the pointer will contain the Maxindex and Minindex of each of the dimensions in the array, so you can, for instance, use pointer in implicit ranges. |

# ALIAS (loader symbol redefinition)

| Category | Special token. |
|----------|----------------|
| Syntax   | alias_option ➞<br>empty<br>| **ALIAS** 'alias_name' |

The alias_name is any sequence of ASCII characters.

| Semantics | Redefines a name to the loader. Sometimes, names must be given to routines and variables that are not valid PLANC names. For instance, the names may begin with digits or special symbols (typical for library routines), specifically with an underscore (_) in routines imported from or exported to C code. See also **EXPORT**, $LIBRARY-MODE and $MODULE-LIBRARY-MODE. |

# AND (intersection) - 3

| Category | Binary operator, priority 3. |
|----------|------------------------------|
| Syntax   | and ➞<br><br>ROUTINE INTEGER, INTEGER (INTEGER) : AND<br>ROUTINE BOOLEAN, BOOLEAN (BOOLEAN) : AND<br>ROUTINE set_type, set_type (set_type) : AND |

where set_type are sets of the same basic type.

| Semantics | The AND operator uses operands of the same type. If the operands are inte- |

---

## Page 160

# Append (append to pointer-implied list)

**Category:**  
Standard routine, priority 5.

**Syntax**
```
append ➝
  ROUTINE record_type POINTER, VOID (pointer_implied_range) : Append

record_type ➝
  RECORD
    optional_declarations
    record_type POINTER : name
    optional_declarations
  ENDRECORD

pointer_implied_range ➝ record_type : name
```

**Semantics**  
Adds a record of type `record_type` which is pointed to by the in-value at the *end* of a pointer-implied list, provided that the last link pointer in the list points to `NIL`.

# ARRAY (composite type constructor)

**Category:**  
Composite type constructor.

**Semantics**  
If you apply the keyword `ARRAY` to a type expression that is not an array, you construct a one-dimensional array whose *base type* is of the initial type expression and whose index value range is specified inside parentheses following the name of the array. Repeated application of the `ARRAY` keyword gives the array a new dimension each time the keyword is repeated. The new index ranges must be added at the end of the list of ranges inside the parentheses to the right of the variable name. The type of the index range delimiters may be either integer or enumeration. You find more details about declaring and initializing arrays under the section for the special token `:`.

## BITS and BYTES
There are two predefined array types in PLANC, the `BITS` and the `BYTES`. When they are used to declare a variable, PLANC expects you to supply

---

## Page 161

# Subarrays

You may use parts of arrays specified by indexes which are subsets of the indexes in the array declarations as a single entity. The index of a subarray may be subsets in any or both of two ways: either because they use a subset of the number of dimensions in the original array, or because they use index ranges of one or more dimensions which are subranges of the original array ranges.

# Examples

```
REAL ARRAY : r1(1:10), r2(5:40)
INTEGER ARRAY : ia(1:100)
INTEGER ARRAY ARRAY : ia1(0:10,1:5), ia2(1:11,-2:2)
...
r1(4:8) =: r2(24:28)
ia1(0:10,i:k-2) =: ia2(2:3,0:j-1)
ia1(10) =: ia
```

# NOTE concerning ND-100

On the ND-100, a `PACKED` array of two-byte integer subtypes must not have a negative lower bound in any of its index ranges. Also note that the scheme for computing the memory address of array elements on this CPU demands that the declared lower index bounds must result in the first element of a packed integer array being placed on an odd byte.

You achieve this on the ND-100 for arrays of more than one dimension by letting the lower bound of the last dimension and the number of values in the index set be a multiple of the number of elements per word.

---

## Page 162

# %ARRAY-INDEX-CHECK (compiler check of array bounds)

**Category**  
Compiler option

**Syntax**  
```
%ARRAY-INDEX-CHECK ON | OFF | ++ | --
```

**Semantics**  
To check that access to array elements uses valid index values, this compiler option can be used. After an incorrect access has occurred, an `ON RANGEERROR` exception-handler can be activated.

If a subarray whose bounds lie outside those of the original array is used, this check will fail, and after such a subarray has occurred, the check will be incorrect.

# ASSERT (test for exception in program)

**Category**  
Directing program flow

**Syntax**  
```
ASSERT Boolean_expression
```

**Semantics**  
This keyword is used in conjunction with PLANC's exception handling block (`ON ASSERTFALSE DO ... ENDON`) to handle exceptional or unexpected situations in the program. When the Boolean expression in the `ASSERT` statement is true, execution will continue at the next executable statement, otherwise control will pass to the nearest preceding exception-handler in the source code that handles the `ASSERTFALSE` condition. If the exception-handler does not contain any statements that transfer control to other locations in the code, execution will continue at the statement following the `ASSERT` statement after the exception-handler has been executed.

# Bit (read/set bit) - 11

**Category**  
Standard routine, priority 11, left or right associative

**Syntax**  
```
bit ➜  
  ROUTINE BOOLEAN, VOID (any_simple_variable; INTEGER : integer_bit_number) : Bit
  [ROUTINE VOID, BOOLEAN (any_simple_variable; INTEGER : integer_bit_number) : Bit
```

---

## Page 163

# BITS (bit array)

## Category
Type constructor.

## Semantics
The `BITS` type constructor is similar to the declaration `BOOLEAN ARRAY PACKED`. One particular use is to equivalence it with variables of other types to be able to check the individual bits of the variable. If you use this particular trick, you must remember that the 80x86 CPUs address memory differently from the other CPUs used by ND/Dolphin. For example, an Intel CPU places the least significant byte of an `INTEGER4` at the first byte address of the word in memory, while the other CPUs place the most significant first. For details about the byte ordering differences, see the manual *PLANC for Intel Microprocessors, ND-820012*, page 42.

# Bit_position (position of record component) - 11

## Category
Standard routine, priority 11.

## Syntax
```
bit_position ➔
  ROUTINE VOID, INTEGER (component_type) : Bit_position
  ROUTINE VOID, INTEGER (record_type.component_type) : Bit_position
```

## Semantics
The purpose of this routine is to find the bit number of the first bit of a component in a composite variable.

# Bit_size (size of variable/component) - 11

## Category
Standard routine, priority 11.

## Syntax
```
bit_size ➔
  ROUTINE VOID, INTEGER (component_name) : Bit_size
  ROUTINE VOID, INTEGER (simple_type) : Bit_size
```

## Semantics
Finds the number of bits that a record component or simple variable contains. Record component names are unique within a module, so...

---

## Page 164

# Blocksize (blocksize of file) - 11

**Category**

Standard routine, priority 11.

**Syntax**

```
block_size ➔
ROUTINE INTEGER, VOID (INTEGER) : Blocksize
```

**Semantics**

The in-value is the number of bytes that will be read from/written to a file in one operation. The parameter is a file number. The file must be opened before the blocksize is set, and the blocksize must be greater than 1. I/O operations are of critical importance to the performance of many programs, and it is a good idea to use big blocksizes that are in accordance with the page size of the OS you are working with. By accordance, we mean that the blocksize should preferably be an integer number of pages.

# BOOLEAN (simple type)

| **Category** | Simple type |
|--------------|-------------|

**Semantics**

Boolean variables can have the two values TRUE and FALSE. If you need to make one- or two-byte Booleans, declare the Boolean subtypes BOOLEAN1 or BOOLEAN2. You can also force the compiler to make two-byte Booleans on four-byte CPUs by giving the following compiler command: `$OPTION BOOLEAN2-ENUMERATION2`. The option thus set will remain in effect for all Booleans declared until you leave the compiler, or the state of the option is changed by ++, --, ON or OFF.

# BOOLEAN1 (Boolean subtype)

| **Category** | Subtype of BOOLEAN. |
|--------------|----------------------|

**Semantics**

Same values as BOOLEAN, but occupies one byte only.

---

## Page 165

# BOOLEAN2 (Boolean subtype)

**Category**  
Subtype of BOOLEAN.

**Semantics**  
Same values as BOOLEAN, but occupies two bytes.

---

# %BOOLEAN2-ENUMERATION2 (make 16-bit variables)

**Category**  
Compiler option.

**Syntax**  
`$OPTION BOOLEAN2-ENUMERATION2 ON | OFF | ++ | --`

**Semantics**  
This option forces the compilers for ND-500(0) and MC680x0 to allocate two bytes for Boolean and enumeration variables.

---

# BYTE (simple type)

**Category**  
Predefined integer subtype which occupies one byte in memory, i.e. INTEGER1 UNSIGNED (or INTEGER RANGE (0:255)).

**Semantics**  
This type can contain all eight-bit ASCII characters. Being an integer subtype, it can also be used everywhere an integer can be used, such as in expressions and routine calls. Values can be assigned to BYTEs as integer values, or alternatively as the ASCII character prefixed by a `#`, thus:

```
#A =: ByteVar.
```

---

# BYTES (byte string)

**Category**  
Predefined type for an array of BYTEs, i.e. BYTE ARRAY PACKED.

**Syntax**  

```
bytes_declaration ➝
  BYTES : declaration_list

declaration_list ➝
  declaration_list single_declaration
  | single_declaration
```

---

## Page 166

# Single Declaration

```
single_declaration ➞
    string_name (range) optional_initialization
```

The index range for this array type can either be specified as an ordinary range, thus:

```
BYTES : string_name (range) optional_initialization
```

Alternatively, you can let an initialization string decide the index range.

```
BYTES : string_name initialization
```

```
initialization ➞
    := bytes_literal

optional_initialization ➞
    initialization
    | empty
```

## Semantics

This is PLANC's type for holding strings of ASCII characters. The default initial value for global strings and local read-modified strings is that all bytes are initialized to ASCII null. If a string with a range is initialized, the initialization bytes will begin to be filled in at the lowest index in the string. If the string is longer than the initialization literal, the remaining bytes will be ASCII null. If the literal is too long, the compiler gives an error message.

If the range is specified by the initialization literal, the bytes in the string are addressed with indexes ranging from zero up to the number of bytes in the literal minus one.

# C (interface to C code)

| Category | Routine modifier |
|----------|------------------|
| Semantics | This modifier is the same as the modifier NATIVE, which lets you make your PLANC routines callable as if they were C routines and lets you import and use C routines in your PLANC program. |

---

## Page 167

# $CALL-HIERARCHY (routine call-hierarchy listing)

| Category | Compiler command |
|----------|-------------------|

| Syntax | CALL-HIERARCHY ON | OFF | ++ | -- |

| Semantics | This command makes the compiler generate a list of the routine call hierarchy of the program being compiled. The listing follows the source listing and precedes the cross-reference listing if present. The default setting for this command is off. |

You may prefer to use the $QUERY command to make output that can be read into a database for the QUERY program as an alternative to this and other listing commands such as $CROSS-REFERENCE.

# CASE (branching statement)

| Category | Directing program flow. |
|----------|-------------------------|

| Syntax | 
```
case_block ➞
    CASE expression
    incase_statements
    optional_else_clause
    ENDCASE

where

incase_statements ➞
    incase_statement
    | incase_statement incase_statements

incase_statement ➞
    INCASE value_list
    statements

value_list ➞
    value_or_range
    | value_list, value_or_range

value_or_range ➞
    value
    | (value:value)

optional_else_statement ➞
    ELSE
```

---

## Page 168

# Semantics

The `CASE` statement is a fast way to select one of many alternatives, depending on the value of the expression after the `CASE` keyword. The value of the expression must be either an integer in the range 0 : 255 or an enumeration value. Each of the `INCASE` alternatives may be associated with a list of more than one value, meaning that several values of the expression will lead to execution of the statements after the `INCASE` statement.

The optional `ELSE` statement *must* be used if the total set of values in the `INCASE` statements is less than the possible outcomes of evaluation of the `CASE` expression, otherwise you get an error message. For example, if the expression yields the value 10 while the `INCASE` statements only cater to the values 0 to 9, then control will pass to the statements following the `ELSE` statement.

# Close (closing files) - 11

| **Category** | Standard routine, priority 11. |
|--------------|--------------------------------|

| **Syntax**   |                               |
|--------------|-------------------------------|
| `close ➞`    |                               |
| `ROUTINE VOID, VOID (INTEGER) : Close` |     |

| **Semantics** | Closes a file which is open and whose file number is given as parameter to `close`. If the file has been used as segment on the ND-500(0) under SINTRAN, you must remember to set the file size of the routine before closing it. |

# COBOL (interface to COBOL)

| **Category** | Routine modifier. |
|--------------|--------------------|

| **Semantics** | Instructs the compiler for 80x86 CPUs that the following routine can be called from MicroFocus COBOL, or to import a routine written in MicroFocus COBOL. To call COBOL on the ND-100/500(0) CPUs, use the routine modifier `STANDARD` instead. |

---

## Page 169

# COMMON (importing FORTRAN COMMON)

**Category**

Import statement modifier.

**Semantics**

See IMPORT.

# $COMPILE (compile source file)

**Category**

Compiler command.

**Syntax**

The command and the parameters may be separated by either one or more spaces or one comma.

```
COMPILE source_file list_file object_file
```

**Semantics**

Instructs the compiler to start compilation of a source file. If you do not give all three parameters to this command, the compiler will prompt you for the ones missing, but you may just press ENTER or enter one comma to skip them. The following rules concern the three parameters:

The `source_file` must always be specified. It must be either the name of an existing file of PLANC code or the number 1, meaning the compiler will take its input directly from the terminal.

The default file type of the source file is `:SYMB` or `:PLNC` under SINTRAN, `.plnc` under UNIX, and `.plc` under MS/DOS.

There are three different possibilities for the nature of the `list_file`. First, it may be either the name of an existing file or the name of a file to be created by the compiler. (To make the compiler create a file, type the name of the new file between double quotes, for example thus: `"/muser/myfile.list".`) Second, if you use a 1 as list file, the listing goes to the standard output (which is always the terminal under SINTRAN). Third, the `list_file` may be absent or 0 meaning the listing should be omitted.

If you give the command `COMPILE 1,1,`, then the compiler will take input from the terminal after it has typed the number of the new line number. If you give the compiler command `QBLIST` before the `COMPILE` command, you will get a disassembled listing of the object code as it is emitted from the compiler, which may be useful if you want to check precisely what the compiler generates.

---

## Page 170

# Default File Types and Object File Parameter

The default file type for list files under SINTRAN is `:LIST`, and under UNIX it is `.list`.

There are two possibilities for the `object_file` parameter:

1. It can be either the name of an existing file or the name of a new file enclosed in double quotes that the compiler will create.
2. It can be `0` or nonexistent, meaning the object code will be discarded.

The second possibility is useful for test compilations to locate compile-time errors, especially in conjunction with ND's LED editor.

The listing will consist of source line numbers followed by the code on the source line. Error messages are printed under the line that generated the error and on the terminal if different from the source file. Line numbers are incremented through `$IF ... $ENDIF$` and `$INCLUDES`.

The default file type for the object file is `:BRF` when generating code for the ND-100; `:NRF` for ND-500(0) and MC680x0 under SINTRAN; `.o` on all types of UNIX and OS2/MSDOS.

# co_Call (Let Co-Routine Proceed) - 11

| Category | Standard routine, priority 11. |
|----------|--------------------------------|

## Syntax

```
co_call ➞
    ROUTINE VOID, VOID &
    (record_name.passive_routine_name)
    : co_Call
```

## Semantics

This routine **activates** a routine that has executed a `co_Detach` or a `co_Resume`. `record_name` is the name of the record in which the parallel routine resides and `passive_routine_name` is the name of the routine to be restarted. Specifying the `record_name` may be unnecessary inside `USING` blocks.

The data needed to restart the current routine is saved and the named routine will start executing. When the called routine does a `co_Detach` or terminates through `RETURN`, `ERRORRETURN` or `ENDROUTINE`, execution will continue at the next statement after the `co_Call` statement.

Note that a routine which executes a `co_Call` does not become passive. Rather, it remains an active routine in the call hierarchy of the `co_Called` routine. As an example, consider a case where the main-program `co_Calls` and thus activates a co-routine, which in turn activates a second co-routine, which in turn activates a third co-routine. The call hierarchy remains active.

---

## Page 171

# co_Detach (suspend co-routine) - 11

## Category
Standard routine, priority 11.

## Syntax
```
co_detach ➞
ROUTINE VOID, VOID 6
(record_name.active_parallel_routine_name)
: co_Detach
```

## Semantics
This routine stops execution of the current co-routine in the state it has when the `co_Detach` statement is reached. Put in another way, the routine passes from an active to a passive state. After `co_Detach` has been called, execution will continue at the location where the co-routine was last called or `co_Called`. Execution of the co-routine will continue from the location it was detached if it is `co_Called` or `co_Resumed` from another part of the program.

`record_name` is the name of the record in which the parallel routine resides and `active_parallel_routine_name` is the name of the currently active parallel routine to be detached. Specifying the `record_name` may be unnecessary inside `USING` blocks.

The `parallel_routine_name` is either the name of the current routine or an active co-routine in its call hierarchy. The most important reason for introducing this parameter to the `co_Detach` routine is that if the current routine is not a co-routine itself, but an ordinary routine which has been called by a co-routine, it is the co-routine which must be restarted; therefore the name of a co-routine in the call-hierarchy of the current routine must be given as parameter to `co_Detach`.

Advanced users may want to make use of the fact that if there are more than one active co-routine in the call hierarchy leading to a `co_Detach`, any of those co-routines may be given as parameter to the detach, and everything below the detached routine will be "frozen" after execution of the detach. When a routine thus passivated is reactivated, execution will proceed according to the "frozen" state.

See also **PARALLEL**.

---

## Page 172

# co_Resume (suspend one, resume another co-routine) - 11

**Category**

Standard routine, priority 11.

**Syntax**

```
co_resume ➔
    ROUTINE VOID, VOID ᵀ 
    (record_name1.active_routine_name, ε
    record_name2.passive_routine_name)
    : co_Resume
```

This standard routine can be seen as a combination of `co_Call` and `co_Detach`. `record_name1` is the name of the record in which the co-routine resides, `active_routine_name` is the name of a co-routine in the call hierarchy of the current routine and `record_name2.passive_routine_name` is the record and name of the routine to be restarted. Specifying the `record_names` may be unnecessary inside `USING` blocks.

`co_Resume` is a combination of `co_Detach` and `co_Call`. The reason for introducing it is that a routine that detaches itself may want to specify which routine is going to be resumed next.

See also `PARALLEL`.

# CONSTANT (declare a value to the compiler)

**Category**

Declaration keyword.

**Syntax**

```
constant_declaration ➔
    CONSTANT constant_list

constant_list ➔
    constant_list , name optional_value
    | name optional_value

optional_value ➔
    empty | = constant_expression

constant_expression ➔
    (constant_expression)
    | constant_expression operator constant_expression
    | constant

constant ➔
    constant_name
```

---

## Page 173

# `literal`

## Semantics

Associates the values of constant expressions of simple types/subtypes and byte strings with names. This association does not result in any code or data; it is used by the compiler during the compilation of the source file named in the `COMPILE` command and its `INCLUDE` files. If you want to use the constant in more than one compilation during the same compiler session, you must use the compiler command `$CONSTANT`.

If no value is assigned to a constant name, it will get a value according to a default rule. The rule is that if no value is associated with the constant being declared, the compiler will increment the previous integer value assigned to a constant and assign it to the new constant. If the first constant has no value associated with it, it gets the value 0.

### Example

```
CONSTANT Zero, One, Two, Five = 5, Six, Seven,
         Eight = Size(Real8)
```

These constants will get values from the compiler so that `Zero` gets the value `0`, `One` becomes `1`, `Two` becomes `2`, `Five` becomes `5`, `Six` and `Seven` become `6` and `7`, and `Eight` gets the number of bytes in an eight-byte real, which is eight of course.

## $CONSTANT (declare constant for subsequent compiles)

| Category | Syntax | Semantics |
|----------|--------|-----------|
| Compiler command. | See `CONSTANT`. | If you want to set a constant to be used during compilation, or you want the constant you define to be used in more than one `COMPILE` command, you give this command to the command processor or you use it as `$CONSTANT` in a source file.|

# `CONVERT` (change type of variable)

| Category | Syntax |
|----------|--------|
| Operator, priority 11. | conversion ➜ `simple_type_expression CONVERT simple_type`<br>`| record_type_expression CONVERT BYTES` |

Where the `simple_type_expression` yields a value of a simple type/subtype.

---

## Page 174

# Semantics

Change the type of the expression on the lefthand side to be of the type on the righthand side.

In the case of integer types/subtypes (including `BYTES`), we can distinguish between two cases: When there are as many or more bits in the lefthand expression as there are in the righthand expression, and when there are fewer bits in the lefthand side than in the righthand side. In the first case, `CONVERT` does a bit-by-bit copying with truncation of the most significant bits if necessary. In the second case, the extra bits on the lefthand type are filled with zeros except when the lefthand expression is negative and the righthand type is signed, in which case the extra bits are filled with ones.

When `REALS` are involved, decimal digits are truncated, not rounded, when high-precision reals go into lower-precision reals or integers.

In the variant of `CONVERT` where records are converted into strings of bytes, the `MinIndex` of the resulting byte string will be zero.

# Example

To convert the value of an integer expression to be of the byte pointer type, you can write:

```
((16325 =: base) + (15 =: displacement)) &
    CONVERT BYTE POINTER =: BytePointer
```

# $CPU-EXTENSION (get CPU version)

| Category | Compiler command. |
|----------|-------------------|

## Syntax

```
$CPU-EXTENSION number
```

## Semantics

There are differences in the instruction set of the 68000 and the 68020 CPUs, and likewise, there are differences between the 80186, the 80286 and the 80386 CPUs. This command makes it possible to choose which CPU you want to generate code for. Typing `$CPU-EXTENTION` without any parameter gives you a list of available CPUs in the compiler you are using.

---

## Page 175

# $CROSS-REFERENCE (source is cross-referenced)

## Category

Syntax

## Syntax

`$CROSS-REFERENCE file_name`

## Semantics

Generates an identifier cross-reference listing on the source file listing, following the source file statements.

The `file_name` is the name of a mass storage file that will be used as a temporary work area. Its default file type on SINTRAN III is `:XREF`, otherwise it is `.xref`.

If you use a compiler that executes on an ND-100, only eight characters are significant in the `CROSS-REFERENCE` information.

This command has a couple of related commands, `CALL-HIERARCHY` which gives routine call hierarchies, and `LINKAGE-REFERENCE` which lists all imported/exported items at the outermost module level. `CROSS-REFERENCE` and `LINKAGE-REFERENCE` cannot both be in effect when a `COMPILE` command is given.

# $DATE (get current date into byte string)

## Category

Compiler command.

## Semantics

This compiler command puts the date of compilation into a string of bytes. The format of the string is as follows:

```
month dd, 19yy
```

where `month` is in letters, `dd` is the number of the day and `19yy` is the year.

## Example

After compilation of this declaration:

```
BYTES READ : date := $DATE & A blank must precede the $
```

the variable `date` could contain the following string:

```
December 25, 1988
```

---

## Page 176

# $DEBUG-MODE (compiler generates debug information)

**Category**  
Compiler command.

**Syntax**  
```
DEBUG-MODE ON | OFF | ++ | --
```

**Semantics**  
Make the compiler generate debug information. The command must be given outside the outermost module level. Under UNIX, the `-g` option to the `plc` frontend replaces this option. For details about the ND debugger, see the manual *Symbolic Debugger User Guide, ND-860158*. Under UNIX, both the `dbx` debugger, which is a standard UNIX debugger, and ND's `ndb` can be used. The latter is better adapted to PLANC; for instance, it handles PLANC's variant records in a better way than `dbx`.

# $DEFINE (for direct load on ND-100)

**Category**  
Compiler command when generating code for the ND-100.

**Syntax**  
```
$DEFINE name octal_value mode

mode -> P | D
```

**Semantics**  
This command generates entry points in the compiler's symbol table when `LOADING` after the `$PROG-FILE` command has been given. `name` is the name of the entry point. If an asterisk (*) is used, the current address will be used as the next load address. If a question mark (?) is used, a map of undefined entries will be output. If this parameter is blank, a map of undefined entries will be output.

`octal_value` is the load address in octal.

`mode` can be `P` (program area) or `D` (data area).

# Dispose (release dynamically allocated memory) - 11

**Category**  
Standard routine, priority 11.

**Syntax**  
```
dispose_statement -> D1spose pointer_expression
```

**Semantics**  
Deallocate dynamically allocated memory. The `pointer_expression` must yield a pointer to a variable that has been allocated dynamically.

---

## Page 177

# DO (loop statement)

## Category
Directing program flow.

## Syntax
We can distinguish between simple and composite DO blocks:

```
do_block ➞ simple_do_block | composite_do_block

simple_do_block ➞
  DO
  statements
  ENDDO

composite_do_block ➞
  DO
  optional_statements
  whiles
  exit_while
  ENDDO

whiles ➞ while_statement whiles | while_statement

while_statement ➞
  WHILE condition
  optional_statements

exit_while ➞
  EXITWHILE
  statements
```

The line shift after DO is not mandatory.

## Semantics
This is PLANC's do loop. The basic loop is an eternal loop unless it contains statements that break it, such as RETURN, ERROR RETURN, ASSERT, GO or calls to exit routines such as Monitor_call Leave on SINTRAN.

However, it is most common to include one or more while_statements in the loop. Following the keyword WHILE is a Boolean expression. If the value of the expression is TRUE, execution will continue at the next statement after the while_statement. If the value of the expression is FALSE, execution will proceed with the statements after the keyword EXITWHILE, or with the first statements after ENDDO in the absence of an EXITWHILE.

---

## Page 178

# DOMAIN (ND internal routine modifier)

| Category | Routine modifier |
|----------|------------------|
| Semantics| For ND internal use.|

# $EJECT (new page in listing)

| Category | Compiler command.                      |
|----------|----------------------------------------|
| Semantics| Insert a page shift in a printed listing.|

# $ELSE (conditional compilation)

| Category | Compiler command.                                                    |
|----------|----------------------------------------------------------------------|
| Semantics| Part of the $IF statement in the command processor. See $IF for more information.|

# ELSE (conditional statement clause)

| Category | Structuring program flow.                                      |
|----------|----------------------------------------------------------------|
| Semantics| This keyword is used in IF ... ENDIF and CASE ... ENDCASE blocks. See IF and CASE for more information.|

# $ELSIF (conditional compilation)

| Category | Compiler command.                                                    |
|----------|----------------------------------------------------------------------|
| Semantics| Part of the $IF statement in the command processor. See $IF.|

---

## Page 179

# ELSIF (conditional statement clause)

| Category      | Directing program flow.                                                |
|---------------|------------------------------------------------------------------------|
| Semantics     | This keyword is used in `IF` ... `ENDIF` block. See `IF` for more information. |

# ENDCASE (end of branching block)

| Category      | Directing program flow.               |
|---------------|---------------------------------------|
| Semantics     | End of case blocks. See `CASE`.       |

# ENDDO (end of loop)

| Category      | Directing program flow.             |
|---------------|-------------------------------------|
| Semantics     | End of do loops. See `DO`.          |

# ENDFOR (end of loop)

| Category      | Directing program flow.             |
|---------------|-------------------------------------|
| Semantics     | End of for loops. See `FOR`.        |

# $ENDIF (end of conditional compilation)

| Category      | Compiler command.                                               |
|---------------|------------------------------------------------------------------|
| Semantics     | Part of the `$IF` statement in the command processor. See `$IF`. |

# ENDIF (end of conditional statement)

| Category      | Directing program flow.                                               |
|---------------|-----------------------------------------------------------------------|
| Semantics     | This keyword is used in `IF` ... `ENDIF` block. See `IF` for more information. |

---

## Page 180

# $ENDMACRO (end of compiler macro)

| Category | Compiler command. |
|----------|-------------------|
| Semantics | Part of the $MACRO ... $ENDMACRO command in the command processor. See $MACRO. |

# ENDMODULE (end of source module)

| Category | Structuring program flow. |
|----------|---------------------------|
| Semantics | Ends all PLANC modules. See MODULE. |

# ENDON (end of exception-handler)

| Category | Directing program flow. |
|----------|-------------------------|
| Semantics | This keyword is the end of the ON ... ENDON exception-handler blocks. See ON for more information. |

# ENDRECORD (end of record declaration)

| Category | Used in declarations. |
|----------|------------------------|
| Semantics | End of RECORD ... ENDRECORD type declaration. See RECORD. |

# ENDROUTINE (end of routine declaration)

| Category | Structuring program flow. |
|----------|---------------------------|
| Semantics | This keyword always ends ROUTINE declarations. It causes return to the calling routine if the routine has void out-value and contains no RETURN statements. See ROUTINE. |

---

## Page 181

# ENDUSING (end of using block)

| Category | Used in an alternative to the dot (.) operator. |
|----------|-----------------------------------------------|
| Semantics | Denotes end of `USING` block. See `USING`.    |

# ENUMERATION (user-defined value range)

| Category | Type constructor. |
|----------|-------------------|

### Syntax

```
enumeration_constructor ➔ ENUMERATION (name_list)
```

The `enumeration_construct` can be used in type expressions or to make a new variable:

```
TYPE name = ENUMERATION (name_list)
ENUMERATION (name_list) : name_list
```

`name_list` is a list of two or more valid PLANC names separated by commas.

| Semantics | Type constructor for simple enumerated types. The `name_list` in the parentheses enumerates the values which variables or expressions of the new type can have. Enumeration value ranges can be used as indexes in arrays, just like integers, and are handy in `FOR` loops. They can also be used as base type in sets and as alternatives in case blocks. See the example programs on control structures and array declarations. |

# $EOF (end of source file)

| Category | Compiler command. |
|----------|-------------------|

| Semantics | When the compiler reads this command while compiling a source file, compilation of the current file stops and continues in the nearest enclosing `$INCLUDE` file, if any. In the absence of an include file, `EOF` signals termination of compilation and exit from the compiler. |

---

## Page 182

# ERRCODE (for error identification)

| Category | Semantics |
|----------|-----------|
| Standard variable. | This standard variable contains the value returned by the last `ERRETURN` statement that was executed. See `ERRETURN`. |

# ERRETURN (used if error occurred in routine)

| Category | Syntax | 
|----------|--------|
| Operator, structuring program flow, priority 1. | `error_return ➔ integer_expression ERRETURN` |

## Semantics

If something goes wrong in a routine, application of this operator to the result of an integer expression causes return to the calling routine or exit from program. In case of a return to a calling routine, the nearest `ON ROUTINE-ERROR` exception-handler preceding the routine call will be invoked. If the calling routine has no exception-handler, there will be an `ERRETURN` from it too, and so on until a routine with an exception-handler is found or the program is terminated.

The value of the integer expression is found in the standard variable `ERRCODE`, and can be used in further tests. In the absence of a suitable exception-handler in the calling routine, control will be transferred to the next higher level in the routine call hierarchy until either an `ON ROUTINE-ERROR` exception-handler is found or the program terminates.

# $EXIT (from compiler)

| Category         | Semantics                               |
|------------------|-----------------------------------------|
| Compiler command. | Causes unconditional exit from the compiler. |

# EXITFOR (execute before leaving for loop)

| Category             | Semantics                                                |
|----------------------|----------------------------------------------------------|
| Structuring program flow. | Start of code which is only executed after the loop terminates through exhaustion of iteration values. See `FOR`. |

---

## Page 183

# EXITWHILE (execute before leaving loop)

**Category**  
Structuring program flow.

**Semantics**  
Start of code which is only executed after a DO or FOR loop terminates because a WHILE condition inside the loop block no longer holds. See DO and FOR.

---

# $EXPAND-MACROS (expand on listing)

**Category**  
Compiler command.

**Syntax**  
`$EXPAND-MACROS ON | OFF | ++ | --`

**Semantics**  
Expands macros on listing only.

---

# EXPORT (make variable known outside module)

**Category**  
Declaration statement.

**Syntax**  

    export_statement ➝
    EXPORT name_list
    
    name_list ➝
      single_item
      | single_item name_list
    
    single_item ➝
      name
      | name alias_list
    
    alias_list ➝
      ALIAS 'loader_symbol' optional_or_list
    
    optional_or_list ➝
      empty
      | OR 'loader_symbol' optional_or_list

**Semantics**  
Indicates list of names to be known outside the current module. The ALIAS may be on either the EXPORT statement or in the declaration of the variable; however, the optional_or_list can only be used in the former.

---

## Page 184

# FALSE (Boolean value)

**Category**  
Boolean value

**Semantics**  
This is one of the two values that a `BOOLEAN` variable may take, the other one being `TRUE`.

# Filesize (get or set size of file) - 11

**Category**  
Standard routine, priority 11.

**Syntax**  
This routine has two different forms, one with in-value but no out-value, and one with out-value but no in-value, according to the following routine declarations:

```
filesize_routine ➜
    ROUTINE INTEGER, VOID (INTEGER) : Filesize
  | ROUTINE VOID, INTEGER (INTEGER) : Filesize
```

**Semantics**  
This routine deals with the size of files that are currently open. When given an in-value, `Filesize` sets the byte size of the file to that value. When the integer out-value is used, it tells you how many bytes the file contains.

# FOR (loop)

**Category**  
Structuring program flow.

**Syntax**  
```
for_loop ➜
    simple_for_loop
  | composite_for_loop
```

---

## Page 185

# Loop Structures

## simple_for_loop
```
FOR loop_control_variable IN value_list DO
    optional_statements
ENDFOR
```

## composite_for_loop
```
FOR loop_control_variable IN optional_reverse value_list DO
    optional_statements
    whiles
    exit_while
    exit_for
ENDFOR
```

## loop_control_variable
```
    integer_variable
  | enumeration_variable
  | pointer_variable
```

## optional_reverse
```
    empty
  | REVERSE
```

## value_list
```
    integer_value_list
  | enumeration_value_list
  | array_implied_list
  | pointer_implied_list
```

## optional_statements
```
    executable_statements
  | empty
```

## whiles
```
    while_statement whiles
  | while_statement
```

## while_statement
```
WHILE condition
    optional_statements
```

## exit_while
```
EXITWHILE
    optional_statements
```

## exit_for
```
EXITFOR
    optional_statements
```

---

## Page 186

# Semantics

The contents of a `for` loop will be executed, at most, once for each time in the value list. The phrase "at most" is deliberate: you can also leave the loop in any way that you can leave the `do` loop. You can leave via `RETURN, ERRETURN, ASSERT, GO, WHILE condition` or calls to exit routines such as `Monitor_call`. Leave on SINTRAN.

If the `for` loop is left because a while condition no longer holds, the execution can continue after the optional `EXITWHILE` statement. If it terminates through exhaustion of the `value_list`, execution can continue after the optional `EXITFOR` statement.

The effect of the optional `REVERSE` clause is to reverse the sequence in which a value range is used as loop control value. Usually, the first value in the range is used first, but when the sequence is reversed, the last value in the range is used. _Note that this_ applies to implicit ranges, not to the items in the list of single values and ranges.

## Example

```
FOR i IN 1, 2, 3, 4:6, 7 DO
```

is executed once for each of the values 1 through 7 in ascending order, while:

```
FOR i IN REVERSE 1, 2, 3, 4:6, 7 DO
```

will be executed with the `i` value sequence 1, 2, 3, 6, 5, 4, 7.

# FORCE (re-interpret type of variable)

## Category

| Operator | Priority |
|----------|----------|
|    -     |    11    |

## Syntax

```
type_reinterpretation ➝ simple_type_expression FORCE simple_type
```

where the `simple_type_expression` yields a value of a simple type/subtype and `simple_type` describes a type. Both can be of the types `INTEGER, REAL, BOOLEAN`, an `ENUMERATION` type or a `POINTER` to any type except array pointers.

## Semantics

Makes a value of the righthand type from the lefthand side. The size of the type of the new value must be the same as the type of the lefthand value, as what `FORCE` does is a simple bit-by-bit copying.

---

## Page 187

# FORTRAN (interface to FORTRAN on PCs)

| Category | Routine modifier |
|----------|------------------|
| Semantics | This modifier makes calls to/from FORTRAN code on PCs under MS/DOS possible. (This is what the STANDARD modifier does on all other OSs/CPUs.) |

# $GENERATE-IMPORTS (make IMPORT list from EXPORTs)

| Category | Compiler command. |
|----------|-------------------|
| Syntax   | `$GENERATE-IMPORTS file_name` |
| Semantics | When this command is given, an $INCLUDE file is generated on the file `file_name` from the subsequent $COMPILE command containing IMPORT statements that match the types of the variables/routines that are EXPORTED from the compiled file(s). |

# $GET-VALUE (of a compiler command)

| Category | Compiler command. |
|----------|-------------------|
| Syntax   | `$GET-VALUE command_name` |
| Semantics | This command returns TRUE if the command is on, FALSE otherwise. |
| Examples  | To find out if the present code will be listed, write<br><br>`$IF $GET-VALUE LIST $THEN ...`<br><br>To set a constant according to what CPU you are compiling for, write<br><br>`CONSTANT CPUtype = $GET-VALUE CPU` |

---

## Page 188

# GO (unconditional jump)

| Category | Special token |
|----------|---------------|
| Semantics | Execution of a `GO` statement causes an unconditional jump to another instruction in the program. The instruction is identified by a `LABEL`, which must be declared before it can be used. See `LABEL`. |

# $HELP (compiler command list)

| Category | Compiler command. |
|----------|--------------------|
| Syntax | `$HELP optional_command_abbreviation` |
| Semantics | When a command abbreviation (in the SINTRAN sense) is given, you get a list of the command(s) that match the abbreviation, their parameter types and values. If no abbreviation is given, all commands are listed. |

# %HELP (compiler option list)

| Category | Compiler option. |
|----------|-------------------|
| Syntax | `$OPTION HELP optional_option_abbreviation` |
| Semantics | Like the compiler command `$HELP` gives a list of commands, the `OPTION HELP` gives a list of options with possible parameter values. |

# $HINTS (about avoidable trouble)

| Category | Compiler command. |
|----------|--------------------|
| Syntax | `$HINTS ON | OFF | ++ | --` |
| Semantics | This command will tell you about unused variables, and give hints instead of warnings. |

---

## Page 189

# $IF (Conditional Compilation)

## Category
Compiler command.

## Syntax

```plaintext
compiler_conditional ➝
    $IF Boolean_expression $THEN
        optional_statements_and_commands
        optional_elsifs
        optional_else
    $ENDIF
```

- **optional_statements_and_commands ➝**
  - empty
  - \| statements_and_commands

- **optional_elsifs ➝**
  - empty
  - \| optional_elsif
  - \| optional_elsif optional_elsifs

- **optional_elsif ➝**
  - `$ELSIF Boolean_expression $THEN`
  - statements_and_commands

- **optional_else ➝ `$ELSE`**
  - statements_and_commands

## Semantics

You can make compilation depend on the state of the compiler and the value of its constants. For example, you can make compilation depend on whether $DEBUG is ON or whether you are generating code for SINTRAN, NDIX, XENIX, DOS etc.

If the first Boolean expression is true, compilation continues with the source lines immediately following the $IF, otherwise possible $ELSIFs are checked in turn and the corresponding lines compiled if the condition holds. When one of the conditions in the $IF ... $ENDIF command holds, compilation continues after the $ENDIF once the source lines corresponding to that condition have been compiled. If no conditions hold and there is an $ELSE clause, the statements after the $ELSE are compiled before exit from the block.

$IF commands can be nested to up to 11 levels.

---

## Page 190

# IF (conditional execution)

---

**Category**  
Structuring program flow.

**Syntax**  

```
if_block ➔  
    IF Boolean_expression THEN
        optional_statements
        optional_elsifs
        optional_else
    ENDIF

optional_statements ➔  
    empty
    | statements

optional_elsifs ➔  
    empty
    | optional_elsif
    | optional_elsif optional_elsifs

optional_elsif ➔  
    ELSIF Boolean_expression THEN
        optional_statements

optional_else ➔  
    ELSE
        optional_statements
```

**Semantics**  
The if statement is the basic construct for making branching statements. If the `Boolean_expression` is true, then the statements immediately after the `THEN` are executed, whereupon execution continues after `ENDIF`. If the first `Boolean_expression` is false, the program will look for the first `ELSIF` whose condition holds and execute the following statements, whereupon execution continues after `ENDIF`. If no condition holds and there is an optional `ELSE`, the statements following the `ELSE` are executed before leaving the block. If no condition holds and there is no `ELSE`, execution continues after `ENDIF`.

---

# IMPORT (get variables/routines from other modules)

---

**Category**  
Special token.

**Syntax**  

```
import_statement ➔  
    IMPORT import_option import_units
```

---

## Page 191

# Import Options

```plaintext
import_option ➔
    (SYSTEM) | (COMMON) | empty

import_units ➔
    variable_declaration
    | import_list

import_list ➔
    import_list , import_unit
    | import_unit

import_unit ➔
    variable_declaration
    | routine_import

routine_import ➔
    (ROUTINE type_expression, type_expression &
    optional_parameter_types : name optional_alias)
   
optional_parameter_types ➔
    empty
    | (type_expression_list)

type_expression_list ➔
    type_expression
    | type_expression, type_expression_list

optional_alias ➔
    empty
    | ALIAS 'literal'
```

## Semantics

The import statement makes the nature of the variables you want to import from "the outside world" into your module known to the compiler, by announcing to it the name and type of imported variables and routines. The type expressions in the import statement should completely match the type expressions used in the declarations of the variables/routines in the module they are EXPORTED from. The simplest way to achieve this is to use the `$GENERATE-IMPORTS` compiler command to make an `$INCLUDE`-file that will contain correct `IMPORT`-statements.

The `ALIAS` clause has the same function as in declarations: it makes special symbols for use with the loader or to suit special circumstances. For example, all loader symbols for C functions begin with an underscore, `_`, which is not a legal start of a PLANC name, so alias clauses must be used when you import C functions. It is also customary to give library routines `ALIAS` names that are not allowed identifiers in your program, so that redefinition of names in loader tables can be avoided.

The `COMMON` clause indicates that the identifiers are the names of FOR-

---

## Page 192

# TRAN COMMON Blocks

For example, suppose you have the following FORTRAN block:

```
BLOCK DATA
COMMON /COMBLOCK/INT1, INT2, INT3
DATA INT1/10/, INT2/101/, INT3/58/
END
```

Then this PLANC code shows how this block can be used:

```
...
TYPE ComRec = RECORD
  INTEGER: i1, i2, i3
ENDRECORD
IMPORT (COMMON) ComRec:COMBLOK
...
COMBLOC.i2 =: int
```

## IN (Test for Membership in Set or Range) - 5

| Category | Logical operator, priority 5. |
| --- | --- |

### Syntax

```
inclusion_test ->
  integer_expression IN integer_range
  | enumeration_expression IN enumeration_range
  | integer_expression IN integer_set
  | enumeration_expression IN enumeration_set
```

### Semantics

This operator allows you to check whether or not an integer or enumeration expression yields a value that is present in a value range or set. Note that integer sets have INTEGER1 UNSIGNED as base type, so integers outside the range 0:255 can never be set members.

## IN (List Part of for Loop)

| Category | Special token. |
| --- | --- |

### Semantics

Used to denote values-list part of for loops, see FOR.

---

## Page 193

# IN (indicate array in dynamic memory allocation)

| Category | Special token. |
|----------|----------------|
| Semantics | Used to indicate which integer array dynamically-allocated variables will be made in, see **NEW**. |

# INCASE (options in CASE statements)

| Category | Directing program flow. |
|----------|-------------------------|
| Semantics | Alternative in case blocks. See **CASE**. |

# $INCLUDE (include source file into current compilation)

| Category | Compiler command. |
|----------|-------------------|
| Syntax | `$INCLUDE source_file` |
| Semantics | When the compiler reads this command from the source code, it will open the `source_file` and read subsequent statements and commands from it. The inclusion will terminate at the end of the include file or if the `$EOF` compiler command is encountered. |

# $INCLUDE-PLANC (include according to byte string)

| Category | Compiler command. |
|----------|-------------------|
| Syntax | `$INCLUDE-PLANC bytes_expression` |
| Semantics | The `bytes_expression` can be built from bytes constants to provide more flexible ways of including files than the bare `$INCLUDE` command would allow. This is useful if you want to compile your system on different operating systems. |

**Example**

Suppose you enter the compiler and give the following commands:

```
@planc
*constant UserPath = '/usr/me/source/'
```

---

## Page 194

# Code Implementation

```plaintext
*constant FileName = 'includefile.incl'
*compile MainFile, ListFile, OutFile

Then you could have the following in your main file:

$IF $PRESENT UserPath AND $PRESENT FileName $THEN
    $INCLUDE-PLANC UserPath//FileName
$ELSE
    $INCLUDE (SintranUser)include-file:incl
$ENDIF
```

# Ind (de-reference, get value of variable pointed to)

## Category

Standard routine, priority 14.

## Syntax

```plaintext
ind ➞

ROUTINE VOID, any_type (any_type POINTER) : ind
```

## Semantics

This routine de-references the parameter, i.e. it has the object pointed to by the parameter as out-value.

In expressions where access to a component of a record being pointed to is required, the dot notation for component access causes an implicit de-referencing to take place. For example, if you have the following declarations:

```plaintext
TYPE r = RECORD
    INTEGER: i, j
ENDRECORD
INTEGER: k
r : rec := (3, 4)
r POINTER : rp := addr(rec)
```

Then the following expressions both yield k = 3:

```plaintext
ind rp.i =: k
rp.i =: k
```

# Inistack (make new program stack)

## Category

Special token

## Syntax

```plaintext
inistack ➞
```

---

## Page 195

# Initstack `integer_array_name`

## Semantics

Initializes array for use as program stack. The array must be declared to be of type `INTEGER` (and _not_ a subtype of integer), and the `MinIndex` of that array must be zero. An executable PLANC program may contain any number of `Initstack` statements on different stack arrays, but there must always be at least one in the `PROGRAM/ROUTINE MAINSTART`. When PLANC routines are called from code written in other languages, it may be a good idea to have an `Initstack` in the PLANC routines to keep the PLANC stack isolated from the doings of other language code.

The size of the stack must be sufficient to keep all intermediate results that occur during the execution of the program plus all stackframes of the routine call hierarchies that may occur and any variables that are dynamically allocated to lie in the stack (i.e., all dynamic variables allocated using the `New` standard routine _without_ an `IN array_name` clause).

For more details on stack handling, see the special section on stacks.

# INLINE (routine modifier)

| Category | Routine modifier. |
|----------|-------------------|

## Semantics

This routine modifier causes the code generated for the routine block to be textually inserted at the location where the routine is called. Thus, overheads for pushing registers onto a stackframe and for parameter passing is avoided and stack space saved while the size of the rest of the code will increase.

Routine error conditions will not be properly handled if they occur within inline routines.

### Note

Inline routines are an alternative to macros.

### Example

Suppose you have the following code:

```
ROUTINE INLINE VOID, VOID : c
    0 ERRRETURN
ENDROUTINE

ROUTINE VOID, VOID : b
    c
ENDROUTINE

ROUTINE VOID, VOID : a
    b
ENDROUTINE
```

---

## Page 196

# Input (from file or terminal) - 11

## Category

Standard routine, priority 11.

## Syntax

```
input ➔
  formatted_input
  | unformatted_input

formatted_input ➔
  ROUTINE VOID, INTEGER (INTEGER : file_number;
  BYTES : descriptor;
  simple_type_expression : variable): Input

unformatted_input ➔
  ROUTINE VOID, INTEGER (INTEGER : file_number;
  INTEGER : record_number; BYTES : string_name) : Input
```

## Semantics

This routine provides simple input of variables to your program. It is not meant to be used in products, as it is relatively slow and cumbersome. Alternatives are found in various subroutine libraries, such as the "accept" routines in the PLANC utilities, and in the system calls/monitor calls of the different OSs.

The out-value from Input is the number of bytes read. The first parameter is the number of an open file.

## Formatted Input

The second parameter when doing formatted input is a BYTES string that describes how the last parameter is going to be read. The following string formats can be used:

- `'Iw'`  
  Integer of width w.

- `'Ow'`  
  Octal integer of width w.

- `'Fw.d'`  
  Floating-point real with width w. This width includes a decimal point and optional minus sign, which will be followed by d decimals.

- `'Ew.d'`  
  Floating-point real with width w. This width includes a decimal point and an exponent. The decimal point will be followed by d decimals. The exponent consists of the letter E, an optional minus sign and two digits.

- `'Aw'`  
  Bytes string. The width w is the maximum number of bytes to be read.

---

## Page 197

# Unformatted Input

The second parameter, `record_number`, in unformatted input statements is the block number within a file opened for random access that you want to read. The size of a file block is set by the standard routine `Blocksize`. The third parameter, `string_name`, is a bytes array to be filled with data. If there is not enough data in a block to fill the array, the surplus bytes will be unchanged. If the array is not big enough, data for which there is no room will be lost.

# Insert (Variable Into List or Value Into Set) - 5

## Category

Standard routine, priority 5.

## Syntax

```
insert ➝
  ROUTINE record_type POINTER, VOID ←
    (pointer_implied_range) : Insert
  ROUTINE INTEGER, VOID (INTEGER SET : set_name) : Insert
  ROUTINE enumeration_type, VOID ←
    (enumeration_type SET : name) : Insert

record_type ➝
  RECORD
    optional_declarations
  record_type POINTER : name
    optional_declarations
  ENDRECORD

pointer_implied_range ➝ record_type : name
```

## Semantics

This routine either inserts a record data element into the front of a pointer-implied linked list, or adds another value to a set of integers or enumerated type values provided the value is not a member of the set already. Remember that the values of integer set members must be in the range zero to 255.

---

## Page 198

# INTEGER (simple type)

## Category
Simple type.

## Syntax

```
integer_dec ➔ integer optional_unsigned optional_range

integer ➔
    INTEGER | INTEGER1 | INTEGER2 | INTEGER4

optional_unsigned ➔
    empty | UNSIGNED

optional_range ➔
    empty 
    | RANGE (lower_limit:upper_limit)
lower_limit ➔ integer_constant
upper_limit ➔ integer_constant
```

## Semantics
Integers can occupy one to four bytes of memory, depending on which subtype you declare.

The integers can be signed, in which case both negative and positive values are allowed, or unsigned, in which case the number is always interpreted as positive. An ordinary INTEGER1 can have values ranging from -128 to 127, while an INTEGER1 UNSIGNED has values ranging from zero to 255. Yet it is possible to store negative numbers into unsigned integers; for example, the expression `-1 =: IntUn` will fill the `IntUn` with binary ones, and consequently it contains the largest possible value in its range afterwards.

Integers can also have value ranges assigned to them via the RANGE clause. However, note that this gives compile time checks only, and the checks are done not on the range in the declaration but on the smallest enclosing range whose upper and lowest limits are powers of two.

# INTEGER1 (subtype of INTEGER)

## Category
Subtype of integer.

## Semantics
This integer subtype has eight bits, range -128:127. See INTEGER.

---

## Page 199

# INTEGER2 (subtype of INTEGER)

| Category | Subtype of integer. |
|----------|---------------------|
| Semantics| This integer subtype has 16 bits, range -32768:32767. See INTEGER. |

# INTEGER4 (subtype of INTEGER)

| Category | Subtype of integer. |
|----------|---------------------|
| Semantics| This integer subtype has 32 bits, range -2147483648:2147483647. See INTEGER. |

# $KILL (remove from compiler's symbol list)

| Category | Compiler command. |
|----------|--------------------|

**Syntax**

```
$KILL constant_identifier_list
constant_identifier_list ->
   constant_identifier, constant_identifier_list
   | constant_identifier
```

| Semantics | Sometimes, it is convenient to set a parameter value to be tested by $IF outside the source files using the $CONSTANT command. The $KILL command makes it possible to remove or change such definitions. |
|-----------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|

# LABEL (for GO statements)

| Category | Simple type. |
|----------|--------------|

**Syntax**

```
label_declaration ->
   LABEL : name

label ->
   name: optional_statements

label_usage ->
   GO name
```

---

## Page 200

# Semantics

Labels are addresses in the program code that can be used for unconditional jumps with the `GO` statement. Labels can only be declared locally (i.e. inside routines).

# $LIBRARY-MODE (make library)

| Category | Compiler command. |
|----------|--------------------|
| Syntax   | `$LIBRARY-MODE ON | OFF | ++ | --` |

## Semantics

This command is used to make libraries. If the mode is `ON`, a library mark will be generated for each outer-level module in the compiled file. The loaders will not load any outer-level module from files compiled in library mode unless there is an unresolved reference to an identifier that is exported from that module. The default value is `OFF`.

If the exported identifier has alias names, an alias clause must be present in the export statements or in the individual declaration. See also `EXPORT`, `ALIAS` and `$MODULE-LIBRARY-MODE`.

# $LINE-BIAS (adjust line number on listing)

| Category | Compiler command. |
|----------|--------------------|
| Syntax   | `$LINE-BIAS line-number` |

## Semantics

This command affects the line number of the next line on the list file or in the debug information. Using it, you can force listing/debug info for a file, a module, a routine and so on to always start at the same line number. 

This command is mostly useful if you do not have access to a source debugger.

# $LINK-TO (for multisegment load on ND-100)

| Category | Compiler command. |
|----------|--------------------|
| Syntax   | `$LINK-TO link_file_name` |

## Semantics

This command is used in the same way as the `LINK-TO` command in the BRF-Linker. For details about multisegment programs, see the **BRF-Linker User Manual**, ND-860196. Only available on the ND-100.

---

## Page 201

# $LINKAGE-REFERENCE (IMPORT/EXPORT cross reference)

**Category**  
Compiler command.

**Syntax**  
`$LINKAGE-REFERENCE work_file_name`

**Semantics**  
This command provides an overview of exported/imported items on the outermost module level. The `work_file_name` is the name of an intermediate file. The output will be printed on the list file after the end of the compilation. Items that are exported from a module will be marked with an asterisk in front of the name of that module, and alias information will be given. If you give this command prior to one or more compile commands, the compiler will enter command mode after each compile—use `$EXIT` to leave the compiler.

The `$LINKAGE-REFERENCE` command and the `$CROSS-REFERENCE` command must not be used together in one compile.

# $LIST (generate listing)

**Category**  
Compiler command.

**Syntax**  
`$LIST ON | OFF | ++ | --`

**Semantics**  
To list selected parts of the output from a file on the list file. The default value is `ON`.

# $LOAD (direct load on ND-100)

**Category**  
Compiler command, ND-100 only.

**Syntax**  
`$LOAD file_name`

**Semantics**  
You can compile and load using the compiler on the ND-100. To make a `::PROG` file, you use the `$PROG-FILE` command to compile and load the main program and this command to load libraries etc. To define entries in the loader table, use the `$DEFINE` command. See also `$PROG-FILE` and `$DEFINE`.

---

## Page 202

# $LONG-NAMES (switch between 16- and 10-byte name length)

**Category**

Compiler command.

**Syntax**

```
$LONG-NAMES ON | OFF | ++ | --
```

**Semantics**

PLANC identifiers may be as long as you like, but only the first ten or 16 bytes are used to determine uniqueness. In PLANC versions prior to J, the default name length is ten bytes; in succeeding versions, the default name length is 16 bytes. You can change the name length with this command, which is available in PLANC versions I and J.

# $MACRO (declare a compiler macro)

**Category**

Compiler command.

**Syntax**

```
$MACRO macro_name optional_parameters
  macro_body
$ENDMACRO

optional_parameters -> 
  empty
  | (parameter_list)

parameter_list ->
  parameter
  | parameter, parameter_list
```

The macro name and the parameters in the list must be valid PLANC names. The parameters may be used anywhere inside the `macro_body`. To signal to the compiler that what follows is a macro parameter, enclose the name of the parameter in double quotes, thus: `"parameter"`. The double quote may not be used for any other purpose than this inside the `macro_body`.

The actual parameters used in macro calls can contain any text string of bytes **except** comma, right parentheses or double quotes. If a comma or a right parentheses is required within an actual parameter, the *entire* actual parameter must be enclosed in double quotes.

**Note:**

A mistake which causes errors that are difficult to find is to forget the double quotes and to give the parameters names that are known to the compiler, as when rewriting routines as macros to speed up the code. Also note that the `macro_body` includes every byte from the first byte.

---

## Page 203

# Semantics

Macros are used as an alternative to conditional compilation and to facilitate production of frequently recurring code. You may define and use them anywhere in your source code, and they may contain both compiler commands (such as `$IF ... $ENDIF`) and other macros.

When the compiler reads a macro name (which may occur anywhere in the source code, such as nested inside other macro calls and outside the outermost module level), it replaces it with the code in the `macro_body`, and replaces the quoted parameter names with the parameter strings given in the macro call.

# Macros and Good Programming Style

Experience indicates that extensive macro usage is *not* conducive to good programming style. They tend to make the code hard to read, and statement lines within the macro bodies cannot be traced by debuggers so you must resort to singlestepping and disassembly when debugging them.

# INLINE is a Good Substitute

It is recommended that you use `INLINE` routines instead of macros when you want to speed up executable statements.

# Example

Consider the following macro:

```
$MACRO exmac (param1, param2)
    "param1""param2"
$ENDMACRO
```

This macro may be expanded as follows:

```
exmac(INTEGER, 2) : i, j
exmac(REAL, 8) : r, s
```

to yield the following declarations in the source code:

```
INTEGER2 : i, j
REAL8 : r, s
```

---

## Page 204

# MAINSTART (main routines accessing UNIX/DOS command line)

## Category
Routine modifier.

## Syntax
```
main_routine ->
ROUTINE MAINSTART VOID, VOID (INTEGER : argc; ε
BYTE POINTER POINTER : argv, envp) : main_routine_name
   declarations
   INISTACK stack_array_name
   statements
ENDROUTINE optional_name

optional_name -> empty | main_routine_name
```

## Semantics
This routine modifier is an alternative to the common PROGRAM declaration for routines that contain a main entry point for a program. When it is used, you can pick up the contents of the command line and the environment in DOS or UNIX programs. (In SINTRAN programs, you pick up the command line by reading from file number 0 instead of from device 1, thus: `Input(0, format, variable)`.)

---

# Maxindex (get highest index in array) - 11

## Category
Standard routine, priority 11.

## Syntax
```
maxindex ->

ROUTINE VOID, INTEGER (array_name, dimension_number)
| ROUTINE VOID, INTEGER (array_name)
| ROUTINE VOID, INTEGER ε
| (Ind(array_pointer_name), dimension_number)
| ROUTINE VOID, INTEGER (Ind(array_pointer_name))
```

## Semantics
This standard routine returns the highest number of an array element in one of the array's dimensions. If the array called `array_name` has one dimension, it is not necessary to specify which dimension you want. If it has more than one dimension, the optional integer `dimension_number` tells the compiler which one you want.

---

## Page 205

# $MESSAGE-PLANC (output a byte string during compilation)

|                  |                              |
|------------------|------------------------------|
| **Category**     | Compiler command.            |
| **Syntax**       | `$MESSAGE-PLANC constant_string_expression` |
| **Semantics**    | Using this command, you can build a string from declared string constants and output it to the terminal during a compile session. |
| **Example**      | `$MESSAGE-PLANC 'Abc' // CrLf` |

# $MESSAGE-TO-TERMINAL (output message while compiling)

|                  |                              |
|------------------|------------------------------|
| **Category**     | Compiler command.            |
| **Syntax**       | `$MESSAGE-TO-TERMINAL message` |
| **Semantics**    | When the compiler reads a line containing this command, the message is printed on the terminal followed by a carriage return. `message` is printed as is and followed by a line feed; it is not necessary to enclose it in parentheses. |

# Minindex (get smallest index in array) - 11

|                  |                                      |
|------------------|--------------------------------------|
| **Category**     | Standard routine, priority 11.       |
| **Semantics**    | This routine returns the minimum index of a dimension in an array. Apart from this, it has precisely the same syntax and semantics as Maxindex. |

# MOD (get modulo of integers) - 11

|                  |                                      |
|------------------|--------------------------------------|
| **Category**     | Binary operator, priority 11.        |
| **Syntax**       | `mod →`<br>`ROUTINE INTEGER, INTEGER (INTEGER) : MOD` |
| **Semantics**    | This operator returns the remainder of the left operand divided by the right operand. |

---

## Page 206

# MODULE (start of a PLANC module)

**Category**

Special token, start of module block.

**Syntax**

```
module ➙
    basic_module
  | composite_module

basic_module ➙
    MODULE name
        header_statements
        basic_module_body
    ENDMODULE

header_statements ➙
    header_statements header_statement
  | header_statement

header_statement ➙
    import_statements
  | export_statements
  | type_declarations
  | constant_declarations
  | comments
  | compiler_commands
  | empty

basic_module_body ➙
    basic_module_body declaration_unit
  | declaration_unit
  | empty

declaration_unit ➙
    data_declaration_statements
  | main_program
  | routine_declarations

composite_module ➙
    MODULE name
        header_statements
        composite_module_body
    ENDMODULE

composite_module_body ➙
    composite_module_body composite_module
  | composite_module_body basic_module
  | composite_module
  | basic_module
```

Header statements may occur outside the outermost module on a PLANC source file:

```
source_file ➙
```

---

## Page 207

# Compilation Units

```
compilation_units
    compilation_units →
    | compilation_units compilation_unit
    compilation_unit →
    global_header_statements module
global_header_statements →
    | type_declarations
    | constant_declarations
    | comments
    | compiler_commands
    | empty
```

## Semantics

All PLANC statements, except those that do not result in storage locations (i.e. the *header_statements*: constant and type declarations, import/export statements, comments and compiler commands), must be enclosed in the `MODULE ... ENDMODULE` block that forms a *basic_module*. Modules may alternatively be composed by inner (nested) modules and header statements, in which case they are called *composite_modules*.

When object files resulting from compilation of different source files are loaded together to form an executable program, one and only one of the modules that were compiled must contain a `PROGRAM` or a `ROUTINE MAINSTART`.

(There is an important exception to this rule: when loading programs from source code in several languages, the main program may reside in the code of the other language(s). When mixing PLANC and C, there must be a `C main()` function in the finished program due to C's idiosyncrasies. See the C section.)

A correctly implemented module is the software equivalent of a hardware chip: signals are passed into it, transformed by it and sent out of it. Its internal structure is hidden from view (as long as the module/chip works according to specifications, its inner workings are of no special interest), the only "handles" accessible to the outside world are quantities imported into/exported from it via pins on the package in the case of chips or via import/export statements in the case of PLANC modules.

If several routines that are overloaded are exported from a module, they must be given individual names via `ALIAS` clauses in their declarations so the loader can distinguish them from each other.

Modules may be nested within modules up to a maximum nesting level of 16. For two modules on the same nesting level to be able to communicate, the data/routines in each sending module must be exported from it, imported into and exported from the enclosing module and imported into the receiving module. If data/routines are going to be used on different nesting levels, they must be exported to each intervening level out to a

---

## Page 208

# Module Importing and Exporting

Level common to the exporting and importing module, and then imported via each intervening level into the module where they are going to be used. (Types and constants defined outside the outermost module level need not be imported in this way.)

Types and constants that are used inside an inner module need not be exported from the modules where they are declared and inwards, but there must be import statements for each intervening module level.

When the rules for importing/exporting names are followed through the nesting levels of composite modules, the compiler type checks the imported/exported items.

**Note:**

Composite modules superficially appear to give some advantages in terms of type checking of data/routines that are exported/imported between nested modules. However, in practice, this seems to make code maintenance more cumbersome than the alternative, which is to use basic modules only and to generate correct import statements on an $INCLUDE-file using the $GENERATE-IMPORTS compiler command. Organizing source code according to the latter principle is facilitated further if you can use facilities like `make`, `AUTOMAKE`, and `sccs` or similar.

# $MODULE-LIBRARY-MODE (Make Library from Single Modules)

| Category  | Compiler command.                       |
|-----------|-----------------------------------------|
| Syntax    | `$MODULE-LIBRARY-MODE ON | OFF | ++ | --` |
| Semantics | When this flag is set, every routine on the outer-level in a module becomes a separate library module and is exported in library mode. See also `$LIBRARY-MODE`. Global variables are collected into a separate module from which they are exported. |

# Monitor_call (Do SINTRAN III Monitor Call)

| Category  | Standard routine, priority 11.                         |
|-----------|--------------------------------------------------------|
| Syntax    | `monitor_call ➡`                                       |
|           | `ROUTINEVOID, VOID (INTEGER parameter_list) : Monitor_call` |
|           | `ROUTINEVOID, VOID (BYTES parameter_list) : Monitor_call`   |
|           | `parameter_list ➡`                                     |
|           | `empty`                                                |

---

## Page 209

# Semantics

This standard routine makes the SINTRAN III monitor calls available. The first parameter is either the number of the monitor call or a byte string containing its name. The name may be in one of two forms: Either it is the "short name", which is the traditional name of a monitor call under SINTRAN, or it is a long and descriptive name. Following the first parameter is a list of parameters corresponding to the needs of the monitor call.

For details and examples, see the manual *SINTRAN III Monitor Calls, ND-860228*.

# NATIVE (same routine modifier as C)

| Category | Semantic |
|----------|----------|
| Routine modifier. | This modifier is the same as the modifier C on 80x86 and MC680x0 CPUs. See C. |

# $ND100-EXTENDED (use extended instruction set)

| Category        | Compiler command. |
|-----------------|--------------------|
| **Syntax**      | $ND100-EXTENDED ON \| OFF \| ++ \| -- |
| **Semantics**   | This command is available on the ND-100 only. It makes the compiler generate code for the extended instruction set of the ND-100/CXE CPUs. It is synonymous with the command $CPU-EXTENSION 2. |

# New (dynamically allocate new variable) - 11

| Category | Standard routine, priority 11. |
|----------|--------------------------------|
| **Syntax** | `new_definition ➔ ROUTINE VOID, any_type POINTER (any_type) : New` |
|            | `new_usage ➔ New any_type optional_in_clause =: any_type_pointer`  |
|            | `optional_in_clause ➔ empty`                                       |

---

## Page 210

# `IN` integer_array

### Semantics

This routine creates a new variable of `any_type` and returns a pointer to it. If the optional `in_clause` is used, the new variable will be placed in the `integer_array`, otherwise it will be allocated on the stack. Variables located on the stack are subject to changes due to other dynamic changes in the stack due to routine calls etc., so it is probably a good idea to keep dynamic structures on separate arrays.

To get rid of dynamic variables when they are no longer needed, use the standard routine `Dispose`, but remember to unlink the variables from any data structures of which they may be a part before you do that! Also note that since there is no dynamic reorganization of the contents of the `integer_array` (or stack) to collect unused space, the array may become fragmented after a while and its usage non-optimal.

The compiler will not complain if you try to create routines dynamically, but it is fair to say that the resulting program is unlikely to work. Use records containing routines as components instead.

---

# NIL (special pointer value)

| Category | Special token. |
|----------|----------------|

### Semantics

`NIL` is the only value all pointers can have, irrespective of which type they have been declared to point to. When a pointer has the value `NIL`, this means that it points to nothing.

---

# NOT (negation) - 4

| Category | Unary operator, priority 4. |
|----------|-----------------------------|

### Syntax
```
negation ->
ROUTINE VOID, BOOLEAN (BOOLEAN) : NOT
```

### Semantics

This operator returns the negative value of the Boolean given as operand.

---

## Page 211

# %OBLIST (output the code generated in disassembly)

**Category**  
Compiler option.

**Syntax**  
```
$OPTION OBLIST ON | OFF | ++ | --
```

**Semantics**  
Using this option has the same effect as the compiler command `$OBLIST`, see below.

# $OBLIST (output the code generated in disassembly)

**Category**  
Compiler command.

**Syntax**  
```
$OBLIST ON | OFF | ++ | --
```

**Semantics**  
When this command is on, the code and data emitted by the compiler are displayed in disassembly.

# ON (start of exception-handler)

**Category**  
Special token.

**Syntax**
```
exception_handler →
    ON exception_condition_list DO
        executable_statements
    ENDON

exception_condition_list →
    exception_condition, exception_condition_list
    | exception_condition

exception_condition →
    ASSERTFALSE
    OVERFLOW
    POINTERERROR
    ROUTINEERROR
    STACKERROR
```

**Semantics**  
The special token `ON` signals the start of an `exception_handler` block. The purpose of exception-handlers is to ease detection and handling of error situations, such as division by zero (OVERFLOW).

---

## Page 212

# Exception Handling in Programming

If you try to allocate data with `New` in an array which is already full or fail to find an object in an implied range, such as in `el Remove List: Next` when `el` is not in the implied range (`POINTERERROR`).

### Errors From Routines

- **RETURN** from routines (`ROUTINEERROR`)
- Stack overflows/underflows (`STACKERROR`)

You can also define your own exception conditions with an `ASSERT` statement, and invoke `ON ASSERTFALSE`-handlers when the assert condition does not hold.

A further exception condition, `RANGEERROR` is allowed but not implemented yet. If it is used, a warning will be given during compilation to that effect.

### Handling Specific Exceptions

The `ASSERTFALSE`, `OVERFLOW`, and `POINTERERROR` exception conditions can only be handled during execution of a program if a corresponding exception-handler is in the routine where the condition is raised and textually precedes the statement which raises it.

The `ROUTINEERROR` and `STACKERROR` exception conditions can only be handled during execution of a program if a corresponding exception-handler is in the routine where the condition is raised or in its call hierarchy, and textually precedes the statement which raises it. If more than one possible exception-handler for one particular exception exists, the last handler in the source before the triggering statement is used.

When an exception condition occurs, the statements in the exception handling block are executed. If the exception-handler does not contain statements that alter program flow (such as `GO`, routine calls that abort execution, `RETURN/ERRORRETURN` statements), execution continues at the location immediately following the instruction that triggered the condition.

### Notes

#### ROUTINEERRORs
A `ROUTINEERROR`-handler cannot repair the out-value of the offending routine, or output parameters that would have been passed back by successful completion of the routine that triggered the condition. You can use the standard variable `Errcode` to identify the error.

#### OVERFLOW
This condition is triggered by an attempted division by zero. Only hardware can raise this condition.

#### POINTERERROR
This condition is raised by pointers pointing outside legal address ranges ??? or having the value `NIL`.

#### RANGEERRORs
This exception is not implemented yet. When implemented, this ex[illegible].

---

## Page 213

# Open (a file)

## Category
Standard routine, priority 11.

## Syntax
```
open ➝
    ROUTINE VOID,
    VOID &
    (INTEGER READ WRITE : file_number; &
    BYTES : file_access, file_name, file_type) &
    : Open
```

## Semantics
This standard routine opens files. After a file has been successfully opened, it can be accessed using the `file_number`. The inverse operation of Open is Close. The file access can be one of the following:

| Access Code | Description                       |
|-------------|-----------------------------------|
| R           | Sequential read                   |
| W           | Sequential write                  |
| RW          | Sequential read and write         |
| WA          | Sequential write append           |
| RX          | Random read                       |
| WX          | Random read and write             |
| RC          | Random read common                |
| WC          | Random read and write common      |
| D           | Direct transfer                   |
| DC          | Direct transfer with file closed  |

# $OPTION (to set compiler options)

## Category
Standard routine

## Syntax
```
$OPTION options

options ➝
    option
    | option, options

option ➝
```

---

## Page 214

# Compiler Options

```
ARRAY-INDEX-CHECK flag_value
BOOLEAN2-ENUMERATION2 flag_value
HELP | OBLIST flag_value
SQUEEZE flag_value
```

`flag_value` ➔ ON | OFF | ++ | --

### Semantics

This command lets you set compiler options. The details of the individual options are described in dedicated sections of this chapter.

# OR (union of BOOLEANS or SETs) - 2

| Category | Binary operator, priority 2. |
|----------|------------------------------|

### Syntax

```
or ➔ 

ROUTINE INTEGER, INTEGER (INTEGER) : OR
ROUTINE BOOLEAN, BOOLEAN (BOOLEAN) : OR
ROUTINE set_type, set_type (set_type) : OR
```

where `set_type` are sets of the same base type.

### Semantics

The OR operator uses operands of the same type. If the operands are integers, it returns the arithmetic union of the two numbers. If the operands are Booleans, it returns true if one of the operands are true and false otherwise. If the operands are sets, it returns the union of the two sets, which must have the same base type.

# Output (write to terminal or file) - 11

| Category | Standard routine, priority 11. |
|----------|--------------------------------|

### Syntax

```
output ➔ 

ROUTINE VOID, INTEGER (INTEGER : file_number;
BYTES : descriptor; simple_type_expression : variable) &
: Output
```

### Semantics

This routine provides simple output of variables to your program. It is not meant to be used in products, as it is relatively slow and cumbersome. Alternatives are found in various subroutine libraries, such as the "Display" routines in the PLANC utilities, various libraries and in the system calls/monitor calls of the different OSs.

The out-value from Output is the number of bytes written. The first para[graph]...

---

## Page 215

# Technical Documentation

meter is the number of a file which can be written to.

The second parameter is a **BYTES** string that describes how the last parameter is going to be written. The following string formats can be used:

| Format   | Description |
|----------|-------------|
| `'Iw'`   | Integer of width w. |
| `'Ow'`   | Octal integer of width w. |
| `'Zw'`   | Octal integer of width w with leading zeros. |
| `'Fw.d'` | Floating-point real with width w. This width includes a decimal point and optional minus sign, which will be followed by d decimals. |
| `'Ew.d'` | Floating-point real with width w. This width includes a decimal point and an exponent. The decimal point will be followed by d decimals. The exponent consists of the letter E, an optional minus sign and two digits. |
| `'Dw.d'` | Floating-point real, same as `'Ew.d'`. |
| `'Aw'`   | Bytes string. The width w is the maximum number of bytes to be written. If more bytes are written than there is room for in the field, the surplus bytes are ignored. If there are too few, the field will be right justified and leading blanks will be inserted. |
| `'ALw'`  | Bytes string. The width w is the maximum number of bytes to be written. If more bytes are written than there is room for in the field, the surplus bytes are ignored. If there are too few, the field will be left justified and trailing blanks will be inserted. |
| `'Lw'`   | Boolean value. Will output T or F to a field of width w. |

The w or the w.d in the above descriptions may be omitted, in which case the compiler will use default values according to the size of the different types on the target CPU. In this case, use the out-value to see how many bytes were actually output.

In the case of output of numerical values, if the value output is smaller than the width descriptor, then leading blanks will be inserted. If the value is too large, the entire field is filled with asterisks (\*).

## OVERFLOW (exception condition)

**Category**
Exception condition.

**Semantics**
This exception condition is raised by the hardware when division by zero.

---

## Page 216

# $OVERLAY (making overlays on the ND-100)

**Category**  
Compiler command.

**Syntax**  
`$OVERLAY number, routine_names`

**Semantics**  
This command is given when making overlay code for the ND-100. It is similar to the `OVERLAY` command in the `BRF-Linker`, see the *BRF-Linker User Manual, ND-860196*.

---

# PACK (make composite variables take less space)

**Category**  
Special token.

**Semantics**  
This keyword may be removed from PLANC, and should not be used.

---

# PACKED (make composite variables take less space)

**Category**  
Type modifier.

**Semantics**  
This type modifier packs the components of arrays and records more densely than would otherwise be the case. It is important to be aware that the packing strategies implemented are a compromise between speed and data density, and that packed structures may differ from one CPU to another. Details are given below, but you should also remember the standard routines `Bit_size`, which gives the size in bits of packed structures, and `Bit_position`, which tells you which bit within the structure is the first bit of the component given as parameter.

**On the ND-500(0)**  
Fields within packed records are assigned space from bit 31 down to bit zero.

If a record component requires more space than there are free bits in the current 32-bit word, the current word is abandoned and a new one is defined at the next byte boundary.

Thus, a maximum of seven bits may be vacant per record component in a packed record.

---

## Page 217

# PARALLEL (routine modifier for co-routines)

## Category
Routine modifier.

## Syntax

```
co-routine_declaration ➝
    TYPE name = RECORD
        optional_declarations
        co-routine
        optional_declarations
    ENDRECORD

co-routine ➝
    ROUTINE parallel in_value_type, out_value_type & (optional_parameters) : routine_name
        declarations
        inistack_statement
        statements
    ENDROUTINE routine_name

inistack ➝ Inistack integer_array_name
            | Inistack record.integer_array_name
            | Inistack Ind(integer_array_pointer_name)
            | Inistack Ind(record.integer_array_pointer_name)
```

(Note that `Inistack rec1.rec2.name` or similar are not allowed.)

## Semantics

This keyword makes a **co-routine** (or **quasi-parallel** routine). Such a routine can suspend execution of itself, to be restarted by another routine from where it stopped without changes to its internal state.

Since a program with co-routines in it cannot be expected to use a common stack as orderly as a "traditional" program, each co-routine must contain an `Inistack` statement to set up a stack for its exclusive use. You can use an array declared globally or inside a record, or use `Ind` of an integer array pointer as the stack array. This array must have zero as `Minindex`, and you cannot use subranges in the `Inistack` statement.

---

## Page 218

# Parallel Routines

In conjunction with **parallel** routines, the new standard routines `co_Call`, `co_Detach` and `co_Resume` are introduced to stop and start execution of co-routines. As a minor point, note that these standard routines are the only ones in PLANC that have routines as parameters.

See also `co_Detach`, `co_Call` and `co_Resume`.

# PASCAL (interface to Pascal routines)

| Category | Routine modifier. |
|----------|-------------------|
| Semantics | This routine modifier tells the compiler that the routine being **IMPORTED** is written in **PASCAL**. |

# POINTER (declare pointer to a type)

| Category | Type constructor. |
|----------|-------------------|

## Syntax

```
pointer_type_expression → 
    type_expression POINTER optional_array_and_modifiers

type_expression →
    non_pointer_type_expression
  | pointer_type_expression optional_array_and_modifiers
```

The `optional_array_and_modifiers` signifies that there may be **ARRAYS** of pointers, that pointers may be **READ/WRITE** modified, and that they may be **PACKED**.

## Semantics

This type constructor creates a variable type that can contain either the memory address of (or point to) variables of the type described by the preceding `type_expression` or the special value **NIL**.

To give a value to a pointer, you must either initialize it using the keyword **Addr** in a global/local **READ** declaration, store the contents of another pointer to the appropriate type to it, or apply the standard routine **Addr** to a variable of the appropriate type.

To retrieve the value of the object pointed to or its components, apply the standard routine **Ind** to the pointer. If the data structure to be pointed to is a routine, you must use **Addr** without parentheses around the pointer expression.

---

## Page 219

# Note on Array Pointers

Pointers to arrays (including `BYTES`) contain the index range(s) of the array in addition to the address of the array; hence, statements like

```
FOR i IN Ind ArrayPointer DO
```

are just fine.

# Note on Record Pointers

A pointer that has been declared to point to one particular record type can also point to variants of that record type. Hence, the following constructs are legal:

```
TYPE Original = RECORD
   INTEGER : i
ENDRECORD

TYPE Variant = Original RECORD
   INTEGER : j
ENDRECORD

Variant: VariantRecord := (1, 2)
Original POINTER: OriginalPointer := Addr VariantRecord
...
OriginalPointer.j ** 2 =: OriginalPointer.i
```

# Examples

For examples of how to use pointers, see examples M4 (data) and M12 (routines).

# POINTERERROR (Exception Condition)

## Category

Exception condition.

## Semantics

This condition allows special handling of some types of pointer errors. For example, when you try to create a dynamic variable and there is not sufficient room left in the array where you want to keep it, the error can be detected because this exception condition is raised by the `New` routine. See `ON` for information about exception handling.

---

## Page 220

# PRECISION (of REALs)

| Category | Variable modifier. |
| -------- | ------------------ |
| Syntax   | `precision_modified_real` ➔ `REAL PRECISION (integer_value) : name` |
| Semantics| This modifier changes the precision of real variables from the maximum number of places allowed by the number of bits in the real variable to the number of places indicated by the `integer_value`. Note that reals are never rounded but truncated. |

# Pred (get previous enumeration value) - 11

| Category | Standard routine, priority 11. |
| -------- | ------------------------------ |
| Syntax   | `pred` ➔ `ROUTINE VOID, enumerated_type (enumerated_type) : Pred` |
| Semantics| This standard routine can only be applied to enumerated types. It will return the previous enumeration value, within the declared list of enumeration values, to that contained in the variable which is the parameter for the routine call. The out-value is unpredictable if you try to reach it before the first value in the enumeration. |

# $PRESENT (symbol in compiler's symbol table)

| Category | Compiler command. |
| -------- | ----------------- |
| Syntax   | `$PRESENT name` |
| Semantics| If the `name` is a symbol present in the compiler's tables, this command returns the value `TRUE`, otherwise it returns `FALSE`. |

# PRIORITY (adjust priority of a routine)

| Category | Routine clause. |
| -------- | --------------- |
| Syntax   | `priority_routine` ➔ |

---

## Page 221

# ROUTINE

```
ROUTINE type, type(parameter_list) : name &
PRIORITY known_routine_or_operator
```

The `known_routine_or_operator` signifies that the names of user-defined routines cannot be used.

## Semantics

You can use this clause to assign a priority to the routine being declared different from what it would get by default. (The default rules are that if the routine overloads a known routine/operator, it gets the priority of that operator, otherwise it gets priority 11.)

## Example

The following routine will get the same priority as the `+` operator:

```
ROUTINE BYTE POINTER, BYTE POINTER &
(BYTE POINTER : BytePointer) : Add PRIORITY +
```

# $PROG-FILE (for direct loading on ND-100)

| Category | Compiler command. |
|----------|-------------------|

| Syntax   | `$PROG-FILE prog_file_name` |

## Semantics

On the ND-100, you can compile and load in one go from the compiler instead of leaving the compiler and using the BRF-Linker. This command names the file which will contain the executable program. Subsequent `$COMPILE` commands will put code on this file, and also on a relocatable `:BRF` file if one has been specified in the `$COMPILE` command. Use the `$SEPARATE-DATA` command to make one- or two-bank mode programs, and `$LOAD` to load libraries, etc. The loader table can be manipulated with the `$DEFINE` command. Error messages are the same as for the ND Relocating Loader (see the manual ND-860066).

# PROGRAM (routine contains main entry point)

| Category | Routine type.            |
|----------|--------------------------|

| Syntax   | `program ➞`               |
|          | `PROGRAM : program_name`  |
|          | `declaration_statements`  |
|          | `INSTACK stack_array_name`|
|          | `executable_statements`   |

---

## Page 222

# ENDROUTINE

### Syntax
```
ENDROUTINE optional_name  
optional_name ➞ program_name | empty
```

### Semantics
This keyword starts the main-program entry point of a program. It can be regarded as a special subtype of the common `ROUTINE` type, and the program block also ends with the keyword `ENDROUTINE`. `PROGRAM` routines can be declared or predeclared anywhere inside a module.

An alternative to the `PROGRAM` declaration is to use the routine modifier `MAINSTART`. This modifier lets you pick up the contents of the command line as well as pointers to the environment in UNIX environments. See also `ROUTINE`.

# PUBLIC (Make Components Known Outside Record)

| Category  | Syntax |
|-----------|--------|
| Special token. | `record_with_hidden_components ➞`<br>`RECORD`<br>`PUBLIC public_components`<br>`component_declarations`<br>`ENDRECORD` |

```
public_components ➞  
    name  
    | name, public_components
```

### Semantics
If you want only a subset of the components of a record to be known outside the record declaration, you can list those components after the special token `PUBLIC`. The components that are not in the public list will be hidden from view outside the record.

This option is particularly useful in records that contain routines as components: Only some routines, and possibly some status variables, need be known outside the record; while all the components that are of interest to the component routines only are safe from being tampered with from the outside.

# RANGE (Set Allowed Value Range for INTEGERS)

| Category   | Syntax |
|------------|--------|
| Type modifier. | `range_integer ➞` |

---

## Page 223

# RANGEERROR (exception if value is not allowed)

| Category | Exception condition. |
|----------|----------------------|

## Semantics

This exception condition is not implemented yet.

# READ (variable is read-only)

| Category | Type modifier. |
|----------|----------------|

## Semantics

This modifier inhibits runtime modifications of the value of the variables it is applied to. It can be used both in variable declarations and in the parameter lists of routine declarations.

A simple READ modified variable cannot be stored in the scope where it is defined. Hence, it is necessary to initialize such variables if they are global or local to a routine. Formal routine parameters cannot be changed inside the routine block.

## READ and arrays

Both the basic elements of and the entire array can be READ modified, but not subarrays of fewer dimensions than the entire array. It is, however, possible to READ modify the basic elements of BYTES/BITS ARRAYS by applying the modification before the ARRAY constructor. When the basic elements are read modified, you cannot store to one element at a time, while you can store to the entire array in one operation. When the entire array is modified, storing to the entire array is impossible while you can still store to individual elements. And if both basic elements and the entire array are READ modified, you cannot change it in any way.

## Example

```
INTEGER READ ARRAY ARRAY READ
```

is a legal type expression where both.

---

## Page 224

# READ and Records

Like arrays, you can choose between modifying the entire record or its individual components. To modify the entire record, put the `READ` modifier after the keyword `RECORD` in the type expression. To modify individual components, include the modifier in the modifier list. Also note that use of both `READ` and `WRITE` modifications after the `RECORD` constructor is of no consequence.

# READ and Routine Parameters

The formal parameters to routines are by default `READ` modified. This means that when you refer to a parameter by the name it is given in the `ROUTINE` declaration, you cannot store to it. Thus, this modifier is redundant in routine parameter descriptions.

# REAL (Simple Type)

| Category | Simple type.       |
|----------|--------------------|

## Syntax

```
real_type ➔ 
    REAL | REAL8 precision_option

precision_option ➔ 
    empty | PRECISION (integer_value)
```

## Semantics

This type contains floating-point numbers. The subtype `REAL8` contains eight bytes or 64 bits, while the `REAL` defaults to a computer-dependent size: On MC680x0, Intel 80x86 and ND-500(0) it is four bytes or 32 bits, while the ND-1x0 CPUs may have either four or six byte (48-bit) reals. The number of digits after the decimal point is modified by the `precision_option`. See `PRECISION`.

## Four-byte Reals

In a four-byte real, bit 31 indicates the sign of the number: If it is one, then the number is negative, otherwise it is zero or positive.

Bits 30 through 22 are the exponent of the number, and it is stored with a bias of 256. This means that if the exponent is 256, then the mantissa is the value of the number. If the exponent is greater than 256, then the resulting exponent is positive, else it is negative. If the exponent is zero, then the entire number is zero.

Bits 21 through 0 hold the mantissa, giving a 22-bit or seven-digit precision. The mantissa is normalized without the 0.5 (0.1 binary) excess, unless the value is zero. The binary point is one place to the left of the mantissa. The mantissa is normalized so `0.5 < = mantissa < 1.0`.

---

## Page 225

# Six-byte reals

This type of real is available on the ND-1x0 CPUs only. Bit 47 holds the sign: If it is one, then the number is negative, otherwise it is positive or zero.

Bits 46 through 32 are the exponent of the number, and it is stored with a bias of 16384. This means that if the exponent is 16384 then the mantissa is the value of the number. If the exponent is greater than 16384 then the resulting exponent is positive else it is negative. If the exponent is zero, then the entire number is zero.

Bits 31 through 0 hold the mantissa, giving a 32-bit or nine-digit precision. The mantissa is normalized without the 0.5 (0.1 binary) excess, unless the value is zero. The binary point is one place to the left of the mantissa. The mantissa is normalized so 0.5 <= mantissa < 1.0.

# Eight-byte reals

Bit 63 holds the sign: If it is one then the number is negative, otherwise it is positive or zero.

Bits 62 through 54 are the exponent of the number, and it is stored with a bias of 256. This means that if the exponent is 256, then the mantissa is the value of the number; if the exponent is greater than 256, then the resulting exponent is positive, else it is negative. If the exponent is zero, then the entire number is zero.

Bits 53 through 0 hold the mantissa, giving a 54-bit or 15-digit precision. The mantissa is normalized without the 0.5 (0.1 binary) excess, unless the value is zero. The binary point is one place to the left of the mantissa. The mantissa is normalized so that 0.5 <= mantissa < 1.0.

The range of values of a `REAL8` is

```
10**-76 <= value <= 10**76
```

with a precision of 15 valid digits.

# $REAL-PRECISION (set number of valid digits for REALs)

| Category           | Compiler command.            |
|--------------------|-------------------------------|
| Syntax             | `$REAL-PRECISION integer_number`<br>`integer-number ➜ 7 | 10` |
| Semantics          | The purpose of this command is to allow a compiler that executes on |

---

## Page 226

# REAL8 (long real subtype)

| Category |                                                                 |
|----------|-----------------------------------------------------------------|
| Category | Real subtype                                                    |
| Semantics| See REAL.                                                       |

# RECORD (to make variables of a record type)

| Category |                                                                 |
|----------|-----------------------------------------------------------------|
| Category | Type constructor                                                |

## Syntax

```
record_type_expression ➔
  base_record_type_expression
  | variant_record_type_expression

base_record_type_expression ➔
  RECORD modifiers optional_public_list
  declarations
  ENDRECORD

variant_record_type_expression ➔
  record_type_name base_record_type_expression

record_type_name_declaration ➔
  TYPE record_type_name = record_type_expression

modifiers is zero or more of READ, WRITE and PACKED.

optional_public_list ➔
  empty
  | PUBLIC public_list
```

## Semantics

The RECORD is PLANC's most versatile type constructor, because it can contain components of every kind that can be declared with type expressions, except recursive instances of itself but including pointers to variables of the same record type and variants of that record type. The latter property is used to make looping through linked lists of records easy with a special kind of for loop. See FOR.

---

## Page 227

# Variant Records

An important property of records is that you can make *variant records* by letting new record types inherit components from existing record types. Variant records will contain all the components of both the base record type and the variant record type. If you examine the memory locations of a variant record, you will find that the components of the base record type come first, followed by the components of the variant type.

It is perfectly acceptable to make variants based on existing variants, and to make more than one variant of an existing record. If you apply the `PACK` modifier to a variant declaration, the components of the variant type and not those of the base type will be packed. Variants of a packed record will not have the variant components packed unless the `PACKED` modifier is used in the variant declaration too.

Pointers to a base record type can also point to records of a variant type, and the components of the variant type can be accessed if you use the dot operator on such pointers. See `POINTER` and example M8.

**Note**

The fact that you can access the components of a variant record via a pointer to the base record type may lead to programming errors that may be difficult to detect. The reason is that if you have a pointer to a variable of the base type and you access components of the variant via this pointer, the compiler will accept this, but memory locations not belonging to the record pointed to may be unintentionally changed.

(Some use this property as a way of equivalencing memory locations: If you have two or more variants of a base record type, you can access the memory locations that follow those of the base record components according to the layout of either of the two variants. This practice must be mentioned, but it must also be pointed out that it is not considered to be good programming style to use this trick.)

# REFERENCE (Routine Modifier)

| Category    | Routine modifier.                                                                            |
|-------------|----------------------------------------------------------------------------------------------|
| Semantics   | Routines with this modifier are similar to `STANDARD` routines in that they have parameters transferred by address (instead of copying the value of the parameter onto the stack, its address is copied), but otherwise they are like ordinary PLANC routines in that they can have in-values, dimensions on parameter arrays and use `ERR-RETURN`. Also note that inner reference-routines are not executed correctly - they can only be used on the outermost level. |

---

## Page 228

# Remove (remove record from list or value from set) - 5

**Category**  
Standard routine, priority 5.

**Syntax**  
```plaintext
remove ➝
  ROUTINE record_type POINTER, VOID ✶
    (pointer_implied_range) : Remove
  | ROUTINE integer_or_enumeration_variable, VOID ✶
    (integer_or_enumeration_set) : Remove

pointer_implied_range ➝
  record_type_pointer : record_type_pointer
```

**Semantics**  
This standard routine takes records out of linked lists and removes set members from sets.

A premise for Remove to work on the linked list is that the `record_type_pointers` can point to elements of the `record_type`, that the `record_type` likewise contains a pointer to `record_type` so linked lists can be formed, that a pointer-implied range exists whose head is pointed to by the first `record_type_pointer` in the range and whose continuation goes via the `record_type` component until it is terminated by a `NIL` pointer, and that the in-value variable is a member of the list thus constructed. If it is not a member of the list, Remove will raise the `POINTERERROR` exception so an exception handler can be activated.

# RETURN (leave routine) - 1

**Category**  
Standard operator, priority 1.

**Syntax**  
```plaintext
return ➝
  ROUTINE type_of_out_value, VOID : RETURN
```

where `type_of_out_value` is the type of the out-value of the routine that encloses the RETURN statement.

**Semantics**  
Execution of a RETURN operator causes immediate exit from the routine that encloses it. If that routine is declared with an out-value, the in-value to the RETURN operator must be of the same type. If the enclosing routine has VOID out-value, RETURN has no (VOID) in-value.

All routines with an out-value must have one or more RETURN state-

---

## Page 229

# REVERSE (apply to ranges in FOR loops)

| Category | Special token. |
|----------|----------------|

| Semantics | This token causes reverse iteration through ranges in for loops. See `FOR`. |
|-----------|--------------------------------------------------------------------------------|

# ROUTINE (declaration of routine)

| Category | Type constructor. |
|----------|-------------------|

## Syntax

```
routine ➞ 
  routine_header optional_alias optional_priority
  optional_declarations
  optional_initstack
  statements
  ENDROUTINE optional_routine_name

routine_header ➞ 
  routine_header_1
  | routine_header_2

optional_priority ➞ 
  empty
  | PRIORITY known_routine_or_operator

optional_alias ➞ 
  empty
  | ALIAS 'routine_or_operator_export_name'

routine_header_1 ➞ 
  ROUTINE optional_modifier in_value_type, out_value_type
  optional_parameters : routine_name
```

---

## Page 230

# Routine Header

```
routine_header_2 ➝ 
  ROUTINE optional_modifier in_value_type, out_value_type
  optional_parameter_types : routine_name optional_parameter_names
```

## Optional Modifier

```
optional_modifier ➝ 
  empty | C | COBOL | DOMAIN | FORTRAN
  | INLINE | MAINSTART | NATIVE | PASCAL
  | parallel | REFERENCE | SPECIAL
  | STANDARD | XARGS
```

## Optional Parameters

```
optional_parameters ➝ 
  empty
  | (parameter_list)
```

### Parameter List

```
parameter_list ➝ 
  parameter_specification
  | parameter_specification; parameter_list
```

### Parameter Specification

```
parameter_specification ➝ 
  parameter_type : name_list
```

### Name List

```
name_list ➝ 
  name
  | name, name_list
```

## Optional Parameter Types

```
optional_parameter_types ➝ 
  empty
  | (parameter_type_list)
```

## Optional Parameter Names

```
optional_parameter_names ➝ 
  empty
  | (parameter_name_list)
```

### Parameter Type List

```
parameter_type_list ➝ 
  parameter_type
  | parameter_type, parameter_type_list
```

### Parameter Name List

```
parameter_name_list ➝ 
  parameter_name
  | parameter_name, parameter_name_list
```

## Optional Inistack

```
optional_inistack ➝ 
  empty
  | Initstack integer_array
```

## Optional Routine Name

```
optional_routine_name ➝ 
  empty
```

---

## Page 231

# Syntax

```
| routine_name

optional_routine_name ➝
    empty
    | routine_name
```

In `routine_header_2`, there must be as many names in the `parameter_name_list` as there are types in the `parameter_type_list`, and the names in the name list will correspond to a formal parameter of the type described in the type list.

## Semantics

The routines of your code, including the special routine type called `PROGRAM`, contain the program's executable statements. The routine header itself contains optional modifiers, type expressions describing the in-value and the out-value of the routine. If there are any parameters then there will be parameter descriptions in one of two formats: The name of the routine, and optional alias and priority clauses. The routine block ends with the special token `ENDROUTINE`, which may be followed by the routine name so you can see which routine declaration ends where.

## Priority

User-defined routines have a standard priority of 11 unless they have a name that overloads an existing standard routine or operator with a different priority or the `optional_priority` clause is used. See `PRIORITY`.

## Routine Modifiers

The available modifiers are `C, COBOL, DOMAIN, FORTRAN, INLINE, MAINSTART, NATIVE, PASCAL, PARALLEL, REFERENCE, SPECIAL, STANDARD` and `XARGS`. The routine modifiers serve various purposes, such as making the routines compatible with routines and routine calls written in other languages (`C, COBOL, FORTRAN, NATIVE, PASCAL, STANDARD` and `XARGS`), making inline replacements of routine bodies wherever the routine is called (`INLINE`), or routines with user-written enter/leave sequences (`SPECIAL`), making special types of routine parameter transfers possible (`REFERENCE`), and indicating that routines are co-routines to be executed in quasi-parallel (`PARALLEL`). The modifier `MAINSTART` can be used as an alternative to the `PROGRAM` declaration. `MAINSTART` routines have been introduced to facilitate retrieval of the contents of the UNIX/DOS command line. (Under SINTRAN, this is achieved by reading from device 0.)

It is important to refer to the individual modifier sections for details about special considerations for the various modifiers.

---

## Page 232

# ROUTINEERROR (exception condition)

| Category | Exception condition. |
|----------|----------------------|
| Semantics | This exception condition is raised when a routine terminates through an ERRRETURN statement. See ERRRETURN and ON. |

# $SELECT (for fast reload)

| Category | Compiler command. |
|----------|--------------------|
| Syntax   | $SELECT `routine_list` \| *ALL* |

## Semantics

The `$SELECT` compiler command helps you save much compile and load time during development of large programs, because it lets you recompile and reload only the routines that have been changed. But the compiler generates symbol names for routines that consist of a composite of the module name and the routine name separated by a dot, thus: `module_name.routine_name`. The total length of the composite name cannot exceed 16 bytes (including the dot). This limitation is going to be removed in the B version of the new ND-Linker. This version of the linker can reload SELECT routines and has a 256 byte symbol length. But in the meantime, you must keep the 16-byte limitation in mind when using SELECT.

Massive recompilations and loading of source code for a major software system can be very time-consuming. An alternative to massive recompilations is to compile only the parts of the source that have changed since the last time it was compiled, and to let the linker "patch" the recompiled code into the existing absolute code. This will make the size of the executable code increase in size, as the "old" code will still be present even if it is never executed, while the "new" code will be added at the end of the executable code. But the recompilation and reloading of selected parts of the code will take much less time, which is the main point.

The reloaded program files may become bigger when they are reloaded, but execution is not necessarily slower. Only the pages that are executed by the program are read into memory, and pages containing new code will be read when needed while pages containing code that is patched out may never be swapped in.

(After you have reloaded changed code repeatedly for a while, you will need to do a massive recompilation and loading again to clear [illegible].)

---

## Page 233

# Compiler Recompilation Options

The ND-500(0) PLANC compiler has been extended to allow recompilation of selected routines from within a module. This selection is done with the compiler command:

```
$SELECT routine_list
```

Use of this option may speed up compile time about five to ten times over a complete recompilation, depending on type redefinitions and global data. This also gives faster syntax checks after minor changes in large programs. Furthermore, the selected routine(s) may be reloaded with the Linkage-Loader at a fraction of the CPU cost.

If you want to use the SELECT option, the total system must be compiled with the option:

```
SELECT *ALL*
```

which will make all routines known to the loader as composites of module name and routine name. This is necessary to make the loader able to resolve name clashes, both for routine names and global variables within the module.

*All* routines to be reloaded must *always* be predefined. Inner routines cannot be selected individually, only as part of an enclosing routine on level one. All routines must be declared with the keyword ROUTINE and not just the type name for its data type. This means that:

```
TYPE rvv = ROUTINE VOID, VOID
rvv : rout ?
```

is *not* allowed; you must declare each and every routine like this:

```
ROUTINE VOID, VOID : rout ?
```

# $SEPARATE-DATA (two-bank on ND-100)

| Category | Compiler command, ND-1x0 only. |
|----------|--------------------------------|
| Syntax   | `$SEPARATE-DATA on | off | ++ | --` |
| Semantics| This command lets you decide whether you want to make *one-bank* or *two-bank* programs on the ND-100 CPU. |

---

## Page 234

# $SEPARATE-DATA (dummy command)

**Category**  
Compiler command, not ND-1x0.

**Syntax**  
```
$SEPARATE-DATA on | off | ++ | --
```

**Semantics**  
This is a dummy command to avoid errors when compiling for CPUs other than the ND-100.

---

# SET (to construct a variable of a SET type)

**Category**  
Type constructor.

**Syntax**  
```
set ->
enumerated_type SET
| integer_type SET
```
where the `enumerated_type` is built with the `ENUMERATION` type constructor and `integer_type` is an integer with values in the range 0:255, i.e. a BYTE, INTEGER1 UNSIGNED or INTEGER RANGE (`lower_limit`: `upper_limit`) where 0 <= `lower_limit` <= `upper_limit` <= 255.

**Semantics**  
Variables of `set` types may contain up to 255 different elements of the types described above. The operators `AND`, `OR` and `NOT` operate on the contents of sets of the same type, while tests are provided by the Boolean operators `<`, `>`, `>=`, `<=`, `<>`, `<=` and `IN`.

**Example**  
```
BYTE SET : Caps := (#A:#Z, #_), &
  IntVar := (#I, #J, #K), Alpha
Byte : Char
% ...
% Note how to inform the compiler that a list of values
Caps AND NOT (IntVar OR [illegible] [SET: ]
```

---

## Page 235

# SHIFT (bits in an INTEGER) - 8

**Category**  
Binary operator, priority 8.

**Syntax**  
```
shift ➔
ROUTINE integer, integer (integer) : SHIFT
```
where *integer* is any integer or integer subtype.

**Semantics**  
This operator shifts the contents of the left operand to the left if the right operand is positive and to the right if it is negative, filling vacant bits with zeros. (Most CPUs have a variety of shift instructions which may be of use; use inline assembly to get at those.)

# Size (of data type in bytes) - 11

**Category**  
Standard routine, priority 11.

**Syntax**  
```
size ➔
ROUTINE VOID, INTEGER (any_type) : Size
```

**Semantics**  
This routine returns the number of bytes occupied by the data type or variable given as parameter. This is useful when programs are implemented on different CPUs, as the space occupied by a composite-data structure may vary (especially if it is a PACKED structure).

# SPECIAL (for making dangerous routines)

**Category**  
Routine modifier.

**Semantics**  
Calling a SPECIAL routine can be faster than for a normal routine, as the usual register storage and stack initialization are not done. Consequently, you get faster code and a decrease in security when using special routines. Hence, this is for experienced users who know what they are doing, and who need to use/reuse inline assembly!

---

## Page 236

# $SPLIT-CODE (80286 segment handling)

**Category:**  
Compiler command.

**Syntax:**  
`$SPLIT-CODE maximum_code_size`

**Semantics:**  
This command, which is valid on 80286 CPUs only, splits code on segments, with maximum code size on each segment. It sets `$MODULE-LIBRARY-MODE ON` and `$LIBRARY-MODE OFF`.

# %SQUEEZE (compact ND-1x0, MC680x0 and 80386 code)

**Category:**  
Compiler option.

**Syntax:**  
`$OPTION SQUEEZE ON | OFF | ++ | --`

**Semantics:**  
Make the ND-1x0, MC680x0 and 80386 CPU code more compact.

# STACKERROR (exception condition)

**Category:**  
Exception condition.

**Semantics:**  
This exception condition is allowed but not implemented yet. Instead of this exception-handler on ND-500(0) CPUs, the `utDefineTrap` utility routine can be combined with the `ON ROUTINEERROR` exception-handler to catch stack over- and underflow. See `ON` for details about exception handling, and **ND-Specific Programming and Advanced PLAN**, ND-820034 for details about catching exceptional stack conditions on the ND-500(0).

# STANDARD (interface to COBOL and FORTRAN routines)

**Category:**  
Routine modifier.

**Semantics:**  
This routine modifier provides a call interface to FORTRAN and COBOL routines. That is, the call sequence and parameter transfer follow the standard used by ND's FORTRAN and COBOL compilers. Since these languages do not have in-values, in-values to `STANDARD` routines are not allowed.

---

## Page 237

# Succ (get next value in enumeration range)

**Category**  
Standard routine, priority 11.

**Syntax**  
```
succ ->
  ROUTINE VOID, enumerated_type (enumerated_type) : Succ
```

**Semantics**  
This standard routine can only be applied to enumerated types. It will return the succeeding enumeration value, within the declared list of enumeration values, to that contained in the variable which is the parameter for the routine call. The out-value is unpredictable if you try to go beyond the last value in the enumeration.

---

# SYSTEM (IMPORT modifier)

| Category | Import statement modifier. |
|----------|-----------------------------|
| Semantics | See IMPORT.               |

---

# $TARGET-MACHINE (get CPU type)

| Category | Compiler command.                                                        |
|----------|--------------------------------------------------------------------------|
| Semantics | This command returns an integer constant which indicates which CPU the compiler is generating code for, such as 1x0, 500(x), x86, and 680x0. |

---

# $THEN (conditional compilation)

| Category | Compiler command. |
|----------|--------------------|
| Semantics | See the $IF command. |

---

## Page 238

# THEN (part of conditional statement)

| Category | Special token. |
|----------|----------------|
| Semantics| Part of the `if` statement. See `IF`. |

---

# TRUE (Boolean value)

| Category  | Boolean value |
|-----------|---------------|
| Semantics | This is one of the two values that a `BOOLEAN` variable may take, the other one being `FALSE`. |

---

# TYPE (to make new variable types from old)

| Category     | Declaration statement.              |
|--------------|-------------------------------------|
| Syntax       | `type ➔`<br>`TYPE name = type_expression` |
| Semantics    | This token tells the compiler that you want to associate a name with a type expression, such as when generating a subtype of a simple variable or when making new array, enumeration, set or record types. This is useful because many type expressions are very big and often consist of previously used type expressions, so substituting a name for a complex expression saves time and ensures coherence between declarations.<br><br>Since `TYPE` declarations do not result in memory space being reserved by the compiler, they may occur outside the outermost module(s) in a file. In the case of nested modules, type declarations may also be _IMPORTED_ from outer modules to inner modules. But the most common practice is to write type declarations to a separate file, and then to `$INCLUDE` them into the modules where they are needed. |
| Examples     | The use of the `TYPE` keyword is demonstrated in several examples in this manual. Its use with simple types is shown in example M5, and all the subsequent examples contain `TYPE` statements for the composite types they deal with. |

---

## Page 239

# Typeof (make new variable of known type)

## Category
Standard routine, priority 11.

## Syntax
```
typeof ➞ 
ROUTINE VOID, VOID (any_type) : Typeof
```

## Semantics
This standard routine can be used both in declaration statements and in executable statements. It causes the compiler to produce variables or intermediate results in expressions that are copies of variables that exist already.

## Example
```
INTEGER2 UNSIGNED : i2u
Typeof i2u array : i2ua(0:3)
RECORD
  INTEGER : di, dj
ENDRECORD : DoublInt
Typeof DoublInt : AnotherDoublInt
...
((300B * 377B) CONVERT Typeof i2u) + 70B =: i2ua(1)
```

# UNSIGNED (integer cannot take negative values)

## Category
Type modifier.

## Syntax
```
unsigned_integer ➞ 
INTEGER optional_modifiers UNSIGNED optional_modifiers
```
where the `optional_modifiers` are any modifiers except UNSIGNED or empty.

## Semantics
This modifier will force integers to be interpreted without sign. For example, the maximum value of an INTEGER1 UNSIGNED is 377B, while its minimum value is zero. For an ordinary INTEGER1, the value range is -200B:177B.

# USING (avoid excessive dot notation)

## Category
Special token.

## Syntax
```
using_block ➞ 
USING using_list
```

---

## Page 240

# Semantics

The purpose of the `USING` block is to avoid repetitious use of the dot notation to get at the components of the record variables in the `using_list` by letting you access the components of the records in the list without any preceding record qualifier and dot.

# Example

If you have the following declarations:

```
TYPE PartRec = RECORD
    BYTES : PartName (1:20)
    INTEGER : PartNumber, CurrentlyInStock
    REAL : PartCost
ENDRECORD
PartRec ARRAY : Parts (1:100)
REAL : Sum
INTEGER : Order
```

Following those declarations, you may write statements like

```
USING Parts(19)
    IF CurrentlyInStock >= Order THEN
        Order * PartCost + Sum =: Sum
        CurrentlyInStock - Order =: CurrentlyInStock
    ENDIF
ENDUSING
```

instead of the more tedious

```
Parts(19).CurrentlyInStock - Order =: Parts(19).CurrentlyInStock
```

# The USING List

If you use a `USING` list, this corresponds to a nested set of `USING` blocks with the last item in the list on the deepest level of nesting. For example,

```
USING v1, v2, ... vn
    % statements
ENDUSING
```

is equivalent to

---

## Page 241

# Using Scopes

```
USING v1
USING v2
...
USING vn
    S statements
ENDUSING
...
ENDUSING
ENDUSING
```

That is, the scopes are opened, and therefore nested, in the order in which they are listed in the `USING` statement.

Thus, if the records `v1` and `v2` both have a component named `F`, then a simple occurrence of `F` inside `s` denotes the corresponding component of `v2`, not of `v1`, by the rules of nested scopes. The component `F` of `v1` can be reached only by dot notation inside `s`, that is, by writing `v1.F`.

**NOTE**

All elements in a `USING` list are stored in temporary pointers to their respective records. These pointers are used when accessing record components in the scope of the current `USING` block (i.e., between `USING` and `ENDUSING`). Thus, if a pointer that is referred to in the `USING` list changes value inside the `USING` block, this has no effect on access to components by component name only.

Also note that this implicit access by temporary pointers does not extend to inline assembly statements - in such statements, record components must be accessed in the ordinary way.

# \$VERSION-INFORMATION (get info about PLANC)

| Category  | Compiler command.                                                                                                          |
|-----------|----------------------------------------------------------------------------------------------------------------------------|
| Semantics | This command, which is available from PLANC version J and on, gives you information about the version of the PLANC compiler that you are currently using. It will also give you the name and whereabouts of a file with information about new features, modifications and errors corrected in your current compiler. |

---

## Page 242

# VOID (special type)

| Category | Simple type. |
|----------|--------------|

| Semantics | The appearance of this special simple type in a routine declaration instead of an ordinary type expression, in the position of either the in-value or the out-value, or in both positions, signifies that either in-value or out-value or both will not be used in the routine being declared. |
|------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|

# WHILE (to leave loops)

| Category | Special token. |
|----------|----------------|

| Semantics | This special token precedes exit conditions in both `DO` and `FOR` loops. The loop is left when the condition, which may occur anywhere inside the loop block, no longer holds. See `DO` and `FOR` for details. |
|------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|

# WRITE (variable can be changed)

| Category | Type modifier. |
|----------|----------------|

| Semantics | This modifier tells the compiler that the variable being declared can be changed. It complements the modifier `READ`, which allows you to access the contents of a variable. If a variable is both `READ` and `WRITE` modified, it can be stored to and read from, if it is `WRITE` only it can be stored to, while `READ` will make storing to the variable impossible. The default access mode for all variables is `READ WRITE`, except for routine parameters where the default mode is `READ` and where you must add a `WRITE` modifier to make a new value last after return from the routine. |

## WRITE and arrays

Both the basic elements of and the entire array can be `WRITE` modified, *but not subarrays of fewer dimensions than the entire array.* (It is, however, possible to `WRITE` modify the basic elements of `BYTES`/`BITS` arrays by applying the modification before the `ARRAY` constructor.) When the basic elements are modified, you can store to but not read from one element at a time, while you can store to and read the entire array in one operation. When the entire array is modified, storing to the entire array is possible while you can still store to and read from individual elements. And if both basic elements and the entire array are `WRITE` modified, you cannot read it in any way.

| Example | `INTEGER WRITE ARRAY ARRAY WRITE` is a legal type expression. |
|---------|---------------------------------------------------------------|

---

## Page 243

# WRITE and Records

Like arrays, you can choose between modifying the entire record or its individual components. To modify the entire record, put the `WRITE` modifier after the keyword `RECORD` in the type expression. To modify individual components, include the modifier in the modifier list of the component. Also note that use of both `READ` and `WRITE` modifications after the `RECORD` constructor have no effect.

# WRITE and Routine Parameters

The formal parameters to routines are by default `READ` modified. If you want to change this so the values assigned to a parameter are in effect after return from the routine, the parameter must be `WRITE` or `READ WRITE` modified.

---

# XARGS (routine can have variable number of parameters)

| Category | Routine modifier. |
|----------|-------------------|

## Semantics

When you use this routine modifier, the number of parameters in calls to the routine may vary. The parameter description indicates how parameters are placed on the stack, starting with the first parameter and increasing the stack offset as new parameters are declared. When using more parameters than are described in the routine declaration, it is the responsibility of the calling and the called routine to make something meaningful out of it.

## Examples

Here are a couple of `XARGS` routines:

```
ROUTINE XARGS VOID, INTEGER (INTEGER : i) : rx
    INTEGER POINTER : x
    Addr :=: x
    Ind (++x) RETURN
ENDROUTINE rx

ROUTINE XARGS VOID, VOID (INTEGER : a, b, c, d) : abcd
    ...
ENDROUTINE abcd
```

Following those declarations, the following two statements will be OK:

```
rx(2, 3 CONVERT INTEGER) :=: int
abcd(1)
```

Following the first statement, `int` will have the value 3. (If the number 3 had not been converted into an integer, the compiler would put it into

---

## Page 244

# XOR (mutually exclusive OR)

## Category
Binary operator, priority 2.

## Syntax
```
xor ➝
ROUTINE INTEGER, INTEGER (INTEGER) : XOR
ROUTINE BOOLEAN, BOOLEAN (BOOLEAN) : XOR
ROUTINE set_type, set_type (set_type) : XOR
```

where `set_type` are sets of the same base type.

## Semantics
The `XOR` operator uses operands of the same type. If the operands are integers, it returns the arithmetic mutually exclusive union of the two numbers. If the operands are Booleans, it returns true if one and only one of the operands are true and false otherwise. If the operands are sets, it returns the mutually exclusive union of the two sets, which must have the same base type.

# $XREF (make cross-references)

## Category
Compiler command.

## Syntax
```
$XREF auxiliary_file
```

## Semantics
The same as `$CROSS-REFERENCE`.

# _ (underscore)

## Category
Special character.

## Semantics
This character may be used anywhere in names except as the first character, and to group digits in numbers.

## Examples
`Under_score` and `Under_` are valid names while `_under` is not. `2#1011_1100#` is the same as `2#10111100#`, `16#B_C#` and `188`.

---

## Page 245

# Appendix A

## plc, the compiler frontend for UNIX systems

---

## Page 246

The image provided is mostly blank with visible discoloration and stains, making it unreadable. There is no discernible text or non-text elements to convert to Markdown.

---

## Page 247

# PLC(1) NDX V (2) PLC(1)

## NAME

`plc` - PLANC compiler frontend

## SYNOPSIS

`plc [ option ] ... file ...`

## DESCRIPTION

PLANC is the NDIX PLANC compiler. `plc` accepts several types of arguments:

Arguments whose names end with `.plnc` are taken to be PLANC source programs; they are compiled, and each object program is left on the file whose name is that of the source with `.o` substituted for `.plnc`. Arguments whose names end with `.Plnc` are processed by the C preprocessor to yield a `.plnc` file, which is subsequently compiled unless the `-P` option has been set. The `.o` file(s) are normally deleted.

In the same way, arguments whose names end with `.s` are taken to be assembly source programs and are assembled, producing a `.o` file.

## FILE EXTENSIONS HANDLED

`plc` can compile and load files with the following extensions:

| Extension | Description                                                     |
|-----------|-----------------------------------------------------------------|
| `.plnc`   | Ordinary PLANC files                                           |
| `.Plnc`   | PLANC files that the C preprocessor will expand before compilation |
| `.c`      | C source files                                                 |
| `.o`      | Object files                                                   |
| `.s`      | Assembly files from C compilations etc.                        |
| `.f`      | FORTRAN files                                                  |
| `.F`      | FORTRAN files that the C preprocessor will expand before compilation |
| `.r`      | RATFOR files                                                   |
| `.e`      | Extended FORTRAN language (EFL) files                           |
| `.cbl`    | COBOL85 files                                                  |
| `.Cbl`    | COBOL85 files that the C preprocessor will expand before compilation |

## OPTIONS

`plc` starts the frontend, `pp` or alternatively `plc -NDc` starts the PLANC compiler itself, so that communication with its command processor can take place in the same way as

---

Page 1

April 28, 1989

---

## Page 248

# PLC(1) - NDX V (2) - PLC(1)

Under SINTRAN III. The following options are interpreted by both the `plc` frontend and `pp` unless otherwise noted. See `ld(1)` for load-time options.

## Options

- `-c`
  Suppress the loading phase of the compilation, and force all object files to be produced. Only meaningful to the `plc` frontend.

- `-g`
  Have the compiler produce additional symbol table information for debuggers.

- `-NDc`
  Start the PLANC command processor. Upon exit from the command processor, any object code generated will be linked to form an executable program, unless you also used the `-c` option.

- `-p`
  Arrange for the compiler to produce code which counts the number of times each routine is called. When the resulting program starts, it calls `monitor(3)` and writes out a `mon.out` file at normal termination of execution of the program. An execution profile can then be generated by use of `prof(1)`.

- `-S`
  Compile the named C (not PLANC!) programs, and leave the assembler-language output on corresponding files suffixed `s`.

- `-P`
  Run only the C macro preprocessor on the named PLANC programs to generate a source file where `#include` files and `#define` macros will be expanded. This is a handy alternative to the PLANC compiler itself, which expands macros, inserts text from `$INCLUDE` files and compiles in the same pass. But remember not to have any PLANC statements beginning with a `#` in column one.

### -NDparameters

This is how to pass the same parameters in the same format as in the SINTRAN PLANCs to the command processor. As the parameters may contain characters that are special to the shell, you may need to quote or escape them, like in:

```
plc -ND' $CONSTANT name=TRUE; $LIST ..'
```

- `-NDl`
  Make a listing of the compiled file with extension `.list`.

- `-NDg filename`
  Write IMPORT statements generated from the EXPORTED symbols on the source file on the named file. Default extension will be `.impt`.

April 28, 1989 (Page 2)

---

## Page 249

# PLC(1) - NDX V (2) - PLC(1)

### Options

- **-NDm**  
  Same result as if the PLANC command `SLIBRARY-MODE ++` had been given.

- **-NDo**  
  Same result as if the PLANC command `SOBLIST ++` had been given.

- **-NDq _filename_**  
  Same result as if the PLANC command `SQUERY-INFORMATION _filename_` had been given. The query information is used in a database where SQL-like queries for relationships between names can be done. The resulting file has the default extension `.qinf`.

- **-C**  
  Prevent the macro preprocessor from editing comments.

- **-o _output_**  
  Name the final output file _output_. If this option is unused, the executable code is written to the file `a.out`. If it is used, any previous `a.out` is left undisturbed. This option is not meaningful in `pp`, since PLANC proper does not do any automatic load after termination of the compile session.

- **-Dname=def**  
  Define the _name_ to the preprocessor, as if by `#define`. If no definition is given, the name is defined as "1".

- **-Uname**  
  Remove any initial definition of _name_.

- **-Idir**  
  `#include` files whose names do not begin with '/' are always sought first in the directory of the _file_ argument, then in directories named in `-I` options, then in directories on a standard list. (This is used by the C preprocessor, and is not a part of PLANC.)

- **-V**  
  Give more detailed information about the processes of compilation.

- **-L**  
  Make one `.o` file from each file specified, and make a library from the resulting files using `ar(1)` and `ranlib(1)`. When loading from a library, only code from the load unit corresponding to that `.o` file will be loaded if it contains an undefined entry.

- **-LS**  
  Like `-L`, but an `.o` file containing code from a source that has many outer level modules will be partitioned into separate load units, one for each module.

Other arguments are taken to be either loader option arguments, or object programs, typically produced by an earlier `plc` run, or perhaps libraries of routines. These programs, together with the results of any compilations specified, are loaded (in the order given) to complete the process.

---

**Page 3**

April 28, 1989

---

## Page 250

# PLC(1) NDX V (2) PLC(1)

produce an executable program with the name a.out.

## FILES USED BY PLANC

In this list, only files used by PLANC are included.

| File | Description |
|------|-------------|
| file.plnc | input file |
| file.o | object file |
| a.out | loaded output |
| /bin/plc | PLANC frontend |
| /bin/pp | PLANC compiler and code generator |
| /lib/crt0.o | runtime startoff |
| /lib/mcrt0.o | startoff for profiling |
| /lib/libplanc.a | standard library, see intro(3) |
| /usr/lib/osplit | for use with -LS option |
| /usr/lib/libplanc_p.a | profiling library, see intro(3) |
| /usr/lib/libcV.a | System V library, see intro(3) |
| mon.out | file produced for analysis by prof(1) |

## SEE ALSO

_PLANC User Guide and Reference Manual_

ND-860117

monitor(3), prof(1), adb(1), ld(1), dbx(1), ndb(1), as(1), cc(1), ftn(1), cbl(1).

## DIAGNOSTICS

The diagnostics produced by PLANC itself are intended to be self-explanatory. Occasional messages may be produced by the assembler or loader.

April 28, 1989 Page 4

---

## Page 251

# Index

## "
- " and macro parameters .................................................. 99

## $
- $ in Output statements ..................................................... 133
- $ sign and compiler commands in source file ................. 96
- $* inline assembly .............................................................. 127, 134

## &
- & and continuation of statement line ............................... 133

## *
- * multiplication operator ................................................... 134
- ** exponentiation operator ................................................. 135

## +
- + addition operator ............................................................. 136
- ++ and pointers .................................................................. 54
- ++ command processor value increment ......................... 136
- ++ increment operator ........................................................ 136

## ,
- , list item separator ............................................................. 137

## -
- - change sign operator ........................................................ 137
- - subtraction operator ........................................................ 137
- -- and pointers ...................................................................... 54
- -- command processor value decrement ........................... 137
- -- decrement operator ........................................................ 138

## .
- . dot access to routine components of records ................. 75
- . dot access, how to avoid with USING block ................... 47
- . dot notation and Ind ......................................................... 23
- . record component access operator ................................ 138

## /
- / division operator ............................................................... 138
- // concatenation operator .................................................... 55
- // string concatenation operator .......................................... 139

[Scanned by Jonny Oddene for Sintran Data © 2021]

---

## Page 252

# Technical Reference

## Boolean and Enumeration Variables

| Description                                    | Page |
|------------------------------------------------|------|
| 16-bit Boolean or enumeration variables        | 153  |
| 16-byte name length                            | 190  |

## Name Length

| Description                                   | Page |
|-----------------------------------------------|------|
| 32-byte name length                           | 190  |

## Code Compaction

| Description                      | Page |
|----------------------------------|------|
| 68000 code compaction            | 224  |
| 80286 segments                   | 224  |
| 80386 code compaction            | 224  |

## Declaration and Assignment

| Description                               | Page |
|-------------------------------------------|------|
| `:` data declaration indicator            | 139  |
| `:` range indicator                       | 140  |
| `:=` variable initialization              | 141  |
| `:=:` swap operator                       | 53   |
| `=:` value assignment operator            | 53   |
| `:=:` variable value swap operator        | 141  |

## Separators

| Description                          | Page |
|--------------------------------------|------|
| `;` routine parameter separator      | 142  |
| `;` statement separator              | 142  |

## Operators

### Less Than

| Description                         | Page |
|-------------------------------------|------|
| `<` less than operator              | 142  |
| `<=` less than or equal operator    | 143  |

### Equal and Assignment

| Description                                | Page       |
|--------------------------------------------|------------|
| `=`                                        | 127        |
| `=` address equivalence                    | 30, 32, 144|
| `=` equal operator                         | 143        |
| `=:` and BYTES                             | 55         |
| `=:` and composite variables               | 39         |
| `=:` and records/arrays/sets               | 54         |
| `=:` store operator                        | 144        |
| `=:` value assignment operator             | 53         |

### Greater Than

| Description                               | Page |
|-------------------------------------------|------|
| `>` greater than operator                 | 145  |
| `>=` greater than or equal operator       | 145  |

---

## Page 253

# Index

## Symbols

- >< unequal operator ............................................................. 145

- `?`
  - ? and predeclaration of routine ....................................... 65
  - ? predeclaration sign ......................................................... 146

- `@`
  - @ is in-value inside routine .................................................... 16

- `_`
  - _ special character .............................................................. 232

## A

- Abs absolute value operator ................................................... 146
- absolute value ............................................................................. 146
- access modification of types .................................................. 230
- access modification on parameters to routines ................ 30
- access to in-value ...................................................................... 30
- Ada ....................................................................................................... 3
- Ada notation .................................................................................. 146
- addition ............................................................................................ 136
- Addr address of variable ............................................................. 147
- Addr standard routine ................................................................. 31, 33
- address equivalence ................................................................. 30, 32, 38, 144
- address equivalence of routines .............................................. 71
- address of variable .................................................................... 147
- addressing array elements ...................................................... 149
- ALGOL .................................................................................................. 3
- ALIAS clause in EXPORT statements ....................................... 68
- ALIAS loader symbol redefinition ............................................. 147
- allocation of memory, dynamic ................................................ 197
- AND Boolean operator ................................................................. 147
- AND set operator .......................................................................... 147
- Append standard routine .......................................................... 47
- Append to pointer-implied list .................................................. 148
- argc and argv .................................................................................. 71
- ARRAY declaration ................................................................. 37, 111, 140
- ARRAY declaration and use ....................................................... 148
- array element addressing ............................................................ 149
- ARRAY elements, access to .......................................................... 36
- ARRAY index ranges and FOR loop .......................................... 26
- ARRAY indexes and index ranges ............................................ 140
- array initialization ............................................................................ 37
- ARRAY initialization ........................................................................ 140
- array packing on ND-100 ............................................................ 149
- array transferred as parameter to routine ...................... 126
- ARRAY type construction ............................................................... 38

---

## Page 254

# Technical Index

## A

| Topic                                                | Pages        |
|------------------------------------------------------|--------------|
| ARRAY type constructor                               | 36, 148      |
| ARRAY, basic elements of                             | 36           |
| array, multi-dimensional                             | 37           |
| array, one-dimensional                               | 37           |
| ARRAY-INDEX-CHECK compiler option                    | 150          |
| arrays and FOR loop                                  | 39           |
| arrays and stacks                                    | 124          |
| arrays and the PACKED modifier                       | 38           |
| arrays, access modification of                       | 37           |
| arrays, initialization of                            | 113          |
| arrays, maximum index in                             | 192          |
| arrays, minimum index in                             | 193          |
| arrays, packed                                       | 106          |
| arrays, store to all elements in                     | 54           |
| arrays, store to subarrays elements in               | 54           |
| ASCII characters used in PLANC                       | 4            |
| ASCII value, how to get                              | 133          |
| assembly, inline                                     | 60, 70, 134  |
| ASSERT statement                                     | 18           |
| ASSERT test for exception condition in program       | 150          |
| ASSERTFALSE exception-handler                        | 21, 150      |
| assignment of values to composite variables          | 39           |
| assignment of values to variables                    | 144          |
| assignment operators                                 | 53           |

## B

| Topic                                                | Pages        |
|------------------------------------------------------|--------------|
| bit operations                                       | 56, 150      |
| bit shifting                                         | 223          |
| Bit standard routine                                 | 56, 150      |
| Bit_position of record component                     | 151          |
| Bit_position standard routine                        | 106          |
| Bit_size of variable or record component             | 151          |
| Bit_size standard routine                            | 33, 44, 56, 106 |
| BITS and BOOLEAN, relationship of                    | 148          |
| BITS array                                           | 151          |
| BITS predefined array type                           | 38, 148      |
| block                                                | 4            |
| block store to records/arrays/sets                   | 54           |
| Blocksize for file I/O                               | 152          |
| BOOLEAN                                              | 152          |
| BOOLEAN and BITS, relationship of                    | 148          |
| BOOLEAN declaration                                  | 29           |
| Boolean expression in WHILE statement                | 25           |
| Boolean value TRUE                                   | 226          |
| BOOLEAN1                                             | 152          |
| BOOLEAN2                                             | 153          |
| BOOLEAN2-ENUMERATION2 compiler option                | 153          |
| Booleans, ORing of                                   | 202          |

---

## Page 255

# Index

## B

- Booleans, XORing of .............................................................................. 232
- branching with CASE ........................................................................... 155

## BYTE

- BYTE ........................................................................................................ 153
- BYTE declaration ................................................................................... 29
- BYTE set ................................................................................................. 41
- byte string delimiter .............................................................................. 133
- BYTE value, how to get ......................................................................... 133
- BYTES ...................................................................................................... 153
- BYTES concatenation ............................................................................ 139
- BYTES continuation to next line ......................................................... 132
- BYTES predefined array type .............................................................. 38, 148
- BYTES, initialization of ......................................................................... 38
- BYTES, storing to .................................................................................... 55

## C

- C ................................................................................................................. 3, 68
- C code interface ...................................................................................... 154
- C routine modifier ................................................................................. 70, 154
- C routines and ALIAS in EXPORT statements .................................. 68
- c_Resume standard routine .................................................................. 91
- CALL-HIERARCHY compiler command ........................................... 155
- calling co-routines .................................................................................. 88
- carwash example ..................................................................................... 75
- CASE flow control statement ................................................................ 155
- CASE statement ...................................................................................... 19, 21, 24
- CASE statement options ........................................................................ 181
- change sign .............................................................................................. 137
- character .................................................................................................. 4
- circular list ............................................................................................... 83
- circular lists .............................................................................................. 75
- Close standard routine .......................................................................... 156
- closing files ............................................................................................... 156
- co-routine resumption ........................................................................... 158
- co-routine suspension ............................................................................. 159
- co-routine suspension and resumption combined ........................... 160
- co-routines ............................................................................................... 81
- co-routines and global data ................................................................... 81
- co-routines and Initstack ....................................................................... 82
- co-routines and parameters .................................................................. 89
- co-routines and routines as record components ............................... 81
- co-routines and stacks ........................................................................... 82
- co-routines, detaching ............................................................................ 84, 85
- co-routines, detaching other than the current one .............................. 89
- co-routines, how they make programming easier ............................ 81, 83
- co-routines, how to declare ................................................................... 205
- co-routines, restarting ............................................................................ 88
- co-routines, terminating ........................................................................ 89
- co_Call standard routine ....................................................................... 88, 158

---

## Page 256

# Index

| Topic                                                                                             | Page(s)            |
|---------------------------------------------------------------------------------------------------|--------------------|
| co_Detach standard routine                                                                        | 84, 85, 159        |
| co_Resume standard routine                                                                        | 160                |
| COBOL                                                                                             | 68                 |
| COBOL and PLANC on non-PC computers                                                               | 224                |
| COBOL routine modifier for PCs                                                                    | 156                |
| COBOL, routine calls from and EXPORT                                                              | 68                 |
| COBOL, routine calls from and STANDARD modifier                                                   | 70                 |
| code disassembly listing by compiler                                                              | 157                |
| code, textual insertion of                                                                        | 183                |
| combination set of routines/operators                                                             | 114                |
| combination set, growth of                                                                        | 114                |
| combination set, use in type checking                                                             | 114                |
| command abbreviation                                                                              | 96                 |
| command line in UNIX/DOS                                                                          | 192                |
| command processor                                                                                 | 96                 |
| command processor value decrement                                                                 | 137                |
| command processor value increment                                                                 | 136                |
| command state detection                                                                           | 175                |
| comment nesting                                                                                   | 132                |
| comments and comment delimiters                                                                   | 132                |
| COMMON, importing from FORTRAN                                                                    | 157                |
| compact ND-1x0, MC680x0 and 80386 code                                                            | 224                |
| compilation of modules, separate                                                                  | 93                 |
| compilation, conditional                                                                          | 45, 98, 177        |
| compilation, how to avoid unnecessary                                                             | 101                |
| COMPILE a source file                                                                             | 157                |
| compile time output of byte strings                                                               | 193                |
| compile time output of messages                                                                   | 193                |
| compiler command abbreviation                                                                     | 96                 |
| compiler command list                                                                             | 176                |
| compiler command processor                                                                        | 96                 |
| compiler command state detection                                                                  | 175                |
| compiler commands from command processor                                                          | 96                 |
| compiler commands in source file                                                                  | 96                 |
| compiler constant declaration by command processor                                                | 161                |
| compiler macros                                                                                   | 44, 99, 190        |
| compiler option list                                                                              | 176                |
| compiler options                                                                                  | 201                |
| compiler symbol, presence of                                                                      | 208                |
| composite type                                                                                    | 5                  |
| composite types and type checking                                                                 | 115                |
| composite variables                                                                               | 214                |
| composite variables, initialization of                                                            | 113                |
| composite variables, packing                                                                      | 204                |
| composite-data types, packing of                                                                  | 106                |
| concatenating strings                                                                             | 55                 |
| conditional compilation                                                                           | 45, 98, 177        |
| conditional execution of statements                                                               | 178                |

---

## Page 257

# C

- conditional expression in WHILE statement .................................... 25
- conditional statement ................................................................. 19
- constant ....................................................................................... 5
- CONSTANT declaration ............................................................ 28, 160
- constant declaration by command processor ............................... 161
- constant declarations outside module .......................................... 93
- constants ....................................................................................... 160
- constants, global .......................................................................... 93
- constants, removal from compiler ............................................... 187
- continuation of statement line ..................................................... 133
- control structures ......................................................................... 19, 20
- conversion of types ...................................................................... 32
- CONVERT operator ................................................................. 31, 32, 35, 161
- CPU version, how to get ............................................................ 162
- CPU-EXTENSION compiler command .................................... 162
- CPU-type, how to get ................................................................. 225
- cross reference listing .................................................................. 163
- cross references ........................................................................... 232
- cross referencing imports/exports ................................................ 189
- CROSS-REFERENCE listing .................................................... 163

# D

- data declaration indicator ........................................................... 139
- data declaration, description of .................................................... 139
- data initialization and type checking .......................................... 112
- data initialization with expressions .....................................................
  - ................................................................................... 112, 113
- data sizes in bytes ........................................................................ 108
- data type size in bytes ................................................................. 223
- data/runtime organization on the ND-500(x) ............................... 107
- date, how to get ........................................................................... 163
- de-referencing pointers ............................................................... 182
- debug information ........................................................................3
- debug information, generation of ................................................. 164
- DEBUG-MODE command .......................................................... 164
- debuggers ....................................................................................... 3
- declaration and initialization of variables .................................... 141
- declaration of compiler macros ...................................................... 190
- declaration of record type ............................................................. 43
- declaration of record without existing type .................................... 43
- declaration of simple types ........................................................... 27, 35
- declaration statement ..................................................................... 5
- declaration, nested routine ........................................................... 62
- declaration, semantics of ............................................................... 140
- declarations .................................................................................... 27
- declarations and type checking ..................................................... 112
- declarations with implicit type conversion ..................................... 113
- decrement ..................................................................................... 138
- default routine parameter access .................................................. 126

---

## Page 258

# Technical Reference Index

| Topic                                                                 | Page(s)      |
|----------------------------------------------------------------------|--------------|
| DEFINE command                                                        | 164          |
| dereferencing pointers                                                | 33           |
| detaching co-routines                                                 | 84, 85       |
| detaching other co-routine than the current                           | 89           |
| digit                                                                 | 5            |
| disassembly listing by compiler                                       | 157          |
| disassembly of the generated object code                              | 199          |
| Dispose standard routine                                              | 48, 50, 164  |
| divide by zero                                                        | 18           |
| division                                                              | 138          |
| division by zero                                                      | 24           |
| DO loop                                                               | 19, 21, 25, 165 |
| DO loop, end of                                                       | 167          |
| DO loop, EXITWHILE statement                                          | 171          |
| DO loops                                                              | 230          |
| DOMAIN routine modifier                                               | 166          |
| DOS command line                                                      | 71           |
| dot access                                                            | 138          |
| dot access to routine components of records                           | 75           |
| dot access, how to avoid with USING block                             | 47           |
| dot notation and Ind                                                  | 23           |
| dot notation and routines                                             | 18           |
| dot notation, how to avoid                                            | 227          |
| dynamic allocation of records                                         | 50           |
| dynamic memory allocation                                             | 23, 181, 197 |
| dynamic memory allocation of records                                  | 44           |
| dynamic memory allocation on stack                                    | 50           |
| dynamic memory allocation, reclaiming allocated memory                | 48           |
| dynamically allocated memory, disposing of                            | 164          |

## E

| Topic                                                                 | Page(s)      |
|----------------------------------------------------------------------|--------------|
| EJECT command to shift to new page in listing                         | 166          |
| ELSE conditional compilation clause                                   | 166          |
| ELSE conditional statement clause                                     | 166          |
| ELSIF conditional compilation clause                                  | 166          |
| ELSIF conditional statement clause                                    | 167          |
| end of source file                                                    | 169          |
| end-of-line                                                           | 131          |
| ENDCASE of CASE block                                                 | 167          |
| ENDDO statement                                                       | 167          |
| ENDIF conditional compilation ends                                    | 167          |
| ENDIF, end of IF statement                                            | 167          |
| ENDMACRO                                                              | 168          |
| ENDMODULE                                                             | 12, 168      |
| ENDON end of exception-handler                                        | 168          |
| ENDRECORD                                                             | 168          |
| ENDROUTINE                                                            | 12, 168      |
| ENDUSING                                                              | 169          |

---

## Page 259

# Technical Index

| Entry                                                    | Page Numbers       |
|----------------------------------------------------------|--------------------|
| entry point, main                                        | 93                 |
| entry sequence to routine                                | 70                 |
| ENUMERATION declaration                                  | 29                 |
| ENUMERATION type construction                            | 35                 |
| ENUMERATION type constructor                             | 28, 29, 169        |
| enumeration value, predecessor of                        | 208                |
| enumeration value, successor of                          | 225                |
| enumeration values in sets                               | 41                 |
| EOF                                                      | 12, 169            |
| equal                                                    | 143                |
| EQUIVALENCE                                              | 127                |
| equivalent addresses                                     | 30, 38, 144        |
| ERRCODE standard variable                                | 170                |
| ERRETURN and REFERENCE routine modifier                  | 70                 |
| ERRETURN and STANDARD routine modifier                   | 70                 |
| ERRETURN operator                                        | 170                |
| error code identification                                | 170                |
| error condition and exception-handlers                   | 18                 |
| error conditions                                         | 24                 |
| error return mechanism                                   | 108                |
| errors in the routines called                            | 18                 |
| exception condition                                      | 24                 |
| exception condition in program                           | 150                |
| exception condition OVERFLOW                             | 203                |
| exception condition POINTERERROR                         | 207                |
| exception condition RANGEERROR                           | 211                |
| exception condition ROUTINEERROR                         | 220                |
| exception condition STACKERROR                           | 224                |
| exception-handler                                        | 18                 |
| exception-handlers                                       | 24                 |
| exception-handler                                        | 15, 21             |
| exception-handlers                                       | 108, 199           |
| exception-handlers, rules for activation of              | 25                 |
| exclusive union of Booleans or sets                      | 232                |
| executable statement                                     | 5                  |
| EXIT from compiler                                       | 170                |
| exit sequence to routine                                 | 70                 |
| EXITFOR                                                  | 170                |
| EXITFOR statement                                        | 19, 22, 25         |
| EXITWHILE                                                | 171                |
| EXITWHILE statement                                      | 19, 22, 25         |
| EXPAND-MACROS                                            | 171                |
| exponentiation                                           | 135                |
| EXPORT and nested modules                                | 94                 |
| EXPORT and separate compilation of modules               | 93                 |
| EXPORT declaration                                       | 171                |
| EXPORT list and GENERATE-IMPORTS command                 | 175                |
| EXPORT of variables from modules                         | 171                |

---

## Page 260

# Table of Contents

- [EXPORT statement](#export-statement)
- [EXPORT statement with ALIAS clause](#export-statement-with-alias-clause)
- [Exported variable cross referencing](#exported-variable-cross-referencing)
- [Expressions](#expressions)
- [Expressions and data initialization](#expressions-and-data-initialization)
- [Expressions, implicit type conversion and data initialization](#expressions-implicit-type-conversion-and-data-initialization)
- [Extended instruction set on the ND-100](#extended-instruction-set-on-the-nd-100)
- [F](#f)
  - [Factorial function](#factorial-function)
  - [FALSE](#false)
  - [Fast reload on ND-500(0)/SINTRAN III](#fast-reload-on-nd-5000sintran-iii)
  - [File I/O blocksize](#file-io-blocksize)
  - [File input](#file-input)
  - [File names](#file-names)
  - [File size, getting and setting](#file-size-getting-and-setting)
  - [File, include](#file-include)
  - [File, output to](#file-output-to)
  - [Files, opening](#files-opening)
  - [Filesize standard routine](#filesize-standard-routine)
  - [Flow control with CASE](#flow-control-with-case)
  - [FOR loop](#for-loop)
  - [FOR loop and ARRAY index ranges](#for-loop-and-array-index-ranges)
  - [FOR loop and arrays](#for-loop-and-arrays)
  - [FOR loop and linked lists](#for-loop-and-linked-lists)
  - [FOR loop and REVERSE](#for-loop-and-reverse)
  - [FOR loop and sets](#for-loop-and-sets)
  - [FOR loop control variable, type of](#for-loop-control-variable-type-of)
  - [FOR loop control variables](#for-loop-control-variables)
  - [FOR loop range reversal](#for-loop-range-reversal)
  - [FOR loop value list](#for-loop-value-list)
  - [FOR loop value range reversal](#for-loop-value-range-reversal)
  - [FOR loop with both EXITWHILE and EXITFOR](#for-loop-with-both-exitwhile-and-exitfor)
  - [FOR loop, end of](#for-loop-end-of)
  - [FOR loop, EXITFOR statement](#for-loop-exitfor-statement)
  - [FOR loop, EXITWHILE statement](#for-loop-exitwhile-statement)
  - [FOR loop, list part of](#for-loop-list-part-of)
  - [FOR loop, WHILE statement](#for-loop-while-statement)
  - [FOR loops](#for-loops)
  - [FORCE](#force)
  - [FORCE operator](#force-operator)
  - [FORTRAN](#fortran)
  - [FORTRAN and PLANC on non-PC computers](#fortran-and-planc-on-non-pc-computers)
  - [FORTRAN COMMON, importing](#fortran-common-importing)
  - [FORTRAN routine modifier on PCs](#fortran-routine-modifier-on-pcs)
  - [FORTRAN, routine calls from and EXPORT](#fortran-routine-calls-from-and-export)
  - [FORTRAN, routine calls from and STANDARD modifier](#fortran-routine-calls-from-and-standard-modifier)

# EXPORT statement
.......................................................................... 68, 93

# EXPORT statement with ALIAS clause
.......................................................................... 68

# Exported variable cross referencing
.......................................................................... 189

# Expressions
.......................................................................... 52

# Expressions and data initialization
.......................................................................... 112

# Expressions, implicit type conversion and data initialization
.......................................................................... 113

# Extended instruction set on the ND-100
.......................................................................... 197

# F

## Factorial function
.......................................................................... 66

## FALSE
.......................................................................... 172

## Fast reload on ND-500(0)/SINTRAN III
.......................................................................... 220

## File I/O blocksize
.......................................................................... 152

## File input
.......................................................................... 184

## File names
.......................................................................... 128

## File size, getting and setting
.......................................................................... 172

## File, include
.......................................................................... 97

## File, output to
.......................................................................... 202

## Files, opening
.......................................................................... 201

## Filesize standard routine
.......................................................................... 172

## Flow control with CASE
.......................................................................... 155

## FOR loop
.......................................................................... 19, 22, 25, 172

## FOR loop and ARRAY index ranges
.......................................................................... 26

## FOR loop and arrays
.......................................................................... 39

## FOR loop and linked lists
.......................................................................... 26

## FOR loop and REVERSE
.......................................................................... 23, 26

## FOR loop and sets
.......................................................................... 23

## FOR loop control variable, type of
.......................................................................... 25

## FOR loop control variables
.......................................................................... 25

## FOR loop range reversal
.......................................................................... 217

## FOR loop value list
.......................................................................... 25

## FOR loop value range reversal
.......................................................................... 23

## FOR loop with both EXITWHILE and EXITFOR
.......................................................................... 25

## FOR loop, end of
.......................................................................... 167

## FOR loop, EXITFOR statement
.......................................................................... 25, 170

## FOR loop, EXITWHILE statement
.......................................................................... 171

## FOR loop, list part of
.......................................................................... 180

## FOR loop, WHILE statement
.......................................................................... 25

## FOR loops
.......................................................................... 230

## FORCE
.......................................................................... 126

## FORCE operator
.......................................................................... 31, 32, 35, 174

## FORTRAN
.......................................................................... 68

## FORTRAN and PLANC on non-PC computers
.......................................................................... 224

## FORTRAN COMMON, importing
.......................................................................... 157

## FORTRAN routine modifier on PCs
.......................................................................... 175

## FORTRAN, routine calls from and EXPORT
.......................................................................... 68

## FORTRAN, routine calls from and STANDARD modifier
.......................................................................... 70

---

## Page 261

# G

| Topic                                                                     | Pages         |
|--------------------------------------------------------------------------|--------------|
| GENERATE-IMPORTS command                                                 | 97, 175      |
| GENERATE-IMPORTS command vs. module nesting                              | 94           |
| GET-VALUE command                                                        | 175          |
| global constants                                                         | 93           |
| global data, how to avoid with co-routines                               | 81           |
| global types                                                             | 93           |
| global variable initialization                                           | 16           |
| GO and LABEL                                                             | 187          |
| GO statement                                                             | 21, 176      |
| greater than                                                             | 145          |
| greater than or equal                                                    | 145          |

# H

| Topic                                                                     | Pages        |
|--------------------------------------------------------------------------|-------------|
| HELP command                                                             | 176         |
| HELP compiler option                                                     | 176         |
| hidden record components                                                 | 210         |
| HINTS about avoidable trouble, how to get                                | 176         |

# I

| Topic                                                                     | Pages                  |
|--------------------------------------------------------------------------|-----------------------|
| IF command                                                               | 98                    |
| IF conditional compilation command                                       | 177                   |
| IF statement                                                             | 19, 20, 24, 178       |
| implicit dereferencing of pointers                                       | 23                    |
| implicit type conversion                                                 | 56, 116               |
| IMPORT                                                                   | 178                   |
| IMPORT and nested modules                                                | 94                    |
| IMPORT and separate compilation of modules                               | 93                    |
| IMPORT declarations, automatic generation of                             | 175                   |
| IMPORT statement                                                         | 93, 178               |
| import statement modifier COMMON                                         | 157                   |
| import statement modifier SYSTEM                                         | 225                   |
| IMPORT statements and GENERATE-IMPORTS command                           | 97                    |
| imported variable cross referencing                                      | 189                   |
| IN array indicator in dynamic memory allocation                          | 181                   |
| IN list part of FOR loop                                                 | 180                   |
| IN operator to test set/range membership                                 | 180                   |
| in-value                                                                 | 16                    |
| in-value name inside routines                                            | 146                   |
| in-value placement in routine call                                       | 17                    |
| in-value, read only access to                                            | 30                    |
| in-values                                                                | 17, 146               |
| INCASE                                                                   | 181                   |
| INCASE statement                                                         | 19, 21, 24            |
| INCLUDE command                                                          | 97, 181               |

---

## Page 262

# Technical Index

| Topic                                                          | Page Numbers  |
|----------------------------------------------------------------|---------------|
| include file                                                   | 97            |
| INCLUDE-PLANC command                                          | 181           |
| increment                                                      | 136           |
| Ind standard routine                                           | 23, 31, 33, 182|
| Ind standard routine, omit when using dot notation             | 23            |
| indentation                                                    | 3             |
| index checking of array bounds                                 | 150           |
| index value range of arrays                                    | 39            |
| indexing first element                                         | 193           |
| indexing last element                                          | 192           |
| Inistack and co-routines                                       | 82            |
| Inistack statement                                             | 125, 182      |
| initial combination set of routines/operators                  | 114           |
| initialization of arrays                                       | 37            |
| initialization of BYTES variables                              | 38            |
| initialization of composite variables                          | 113           |
| initialization of data and type checking                       | 112           |
| initialization of local variables                              | 39            |
| initialization of variables                                    | 141           |
| initializing data with expressions                             | 112, 113      |
| inline assembly                                                | 60, 70, 127, 134|
| inline assembly statement separator                            | 142           |
| INLNE routine modifier                                         | 69, 183       |
| Input standard routine                                         | 184           |
| Insert standard routine                                        | 47, 185       |
| inserting variable in linked list                              | 47            |
| instruction set version, how to get                            | 162           |
| INTEGER                                                        | 186           |
| INTEGER declaration                                            | 29            |
| integer declaration with valuerange limitation                 | 210           |
| integer value bit shift                                        | 223           |
| integer values, Ada notation for representing                  | 146           |
| INTEGER1                                                       | 186           |
| INTEGER2                                                       | 187           |
| INTEGER4                                                       | 187           |
| integers, modulo of                                            | 193           |
| integers, unsigned                                             | 227           |
| interface to COBOL and FORTRAN                                 | 224           |
| interface to COBOL on PCs                                      | 156           |
| interface to FORTRAN on PCs                                    | 175           |
| interface to PASCAL routines                                   | 206           |
| intermediate result                                            | 5             |
| intersection of sets                                           | 147           |

---

## Page 263

# J

jump, unconditional .................................................................................. 21

# K

keyword .................................................................................................... 5  
KILL command ................................................................................. 45, 98, 187

# L

LABEL .................................................................................................. 28  
LABEL declaration ............................................................................ 20  
LABEL for GO statements ................................................................ 187  
LABEL use ............................................................................................ 21  
language editor (LED) .......................................................................... 3  
leaving routines ................................................................................. 216  
LED (language editor) ......................................................................... 3  
less than ............................................................................................ 142  
less than or equal ................................................................................ 143  
letter ...................................................................................................... 5  
libraries, how to make ....................................................................... 188  
library from a single module ............................................................. 196  
LIBRARY-MODE command ............................................................ 188  
line continuation ............................................................................... 131  
line numbering on listing ................................................................. 188  
line shift in Output statements ......................................................... 133  
LINE-BIAS command ....................................................................... 188  
LINK-TO command ........................................................................ 188  
LINKAGE-REFERENCE command .............................................. 189  
linked list and FOR loop .................................................................... 24  
linked list, adding variable to .................................................. 47, 185  
linked list, creating ............................................................................ 45  
linked list, inserting variable in ...................................................... 47  
linked list, removing variable from ................................. 47, 216  
linked lists and FOR loop ................................................................. 26  
LIST command .................................................................................. 83  
list command ..................................................................................... 189  
list file extention, default ................................................................ 158  
list file generation with COMPILE command ........................... 157  
list item separator ............................................................................ 137  
list, adding variable to .................................................................... 185  
list, removing variable from .......................................................... 216  
listing of compiler session ............................................................. 189  
lists, circular ........................................................................................ 75  
literal ..................................................................................................... 6  
LOAD command .............................................................................. 189  
loader symbol redefinition ............................................................... 147

---

## Page 264

# Technical Index Page

## Loading
- loading directly from compiler on ND-100 .................................................. 96
- loading directly on the ND-100 ..................................................................... 164
- loading fast on ND-500(0)/SINTRAN III ..................................................... 220
- loading on ND-100 ....................................................................................... 189
- loading overlays on ND-100 ........................................................................ 204
- loading two-bank programs on the ND-100 ................................................ 221
- loading, make a program file on the ND-100 ............................................. 209

## Local Variables
- local READ variable initialization .............................................................. 16
- local variables ............................................................................................ 30
- local variables, initialization of ................................................................... 39

## Logic and Commands
- logical AND ................................................................................................. 147
- LONG-NAMES command ........................................................................... 190

## Loops
- loops, how to leave ................................................................................... 230

# M

## Machine Type
- machine type, how to get ........................................................................... 225

## MACRO
- MACRO command ...................................................................................... 99, 190
- macro expansion on listing ........................................................................ 171
- macro parameter delimiter ......................................................................... 132
- macro parameters ...................................................................................... 99
- macros ......................................................................................................... 44

## Main Entry
- main entry point ......................................................................................... 93, 209
- main-program ............................................................................................. 209

## Routine Modifiers
- MAINSTART routine modifier ...................................................................... 71, 93, 192
- Maxindex of an array .................................................................................. 192
- Maxindex standard routine ........................................................................ 39
- memory allocation, dynamic ..................................................................... 23, 197
- memory allocation, reclaiming allocated memory .................................... 48

## Commands
- MESSAGE-PLANC command ..................................................................... 193
- MESSAGE-TO-TERMINAL command .......................................................... 98, 193
- Minindex of an array ................................................................................... 193
- Minindex standard routine ......................................................................... 39
- MOD operator ............................................................................................. 45, 193

## Modifiers
- modifiers for simple types ......................................................................... 28
- modifiers, routine ....................................................................................... 68

## MODULE
- MODULE ...................................................................................................... 11, 12, 92, 194
- module and type declaration ...................................................................... 12
- module nesting ............................................................................................ 94
- module nesting and IMPORT/EXPORT ...................................................... 94
- module nesting and type checking ............................................................. 94
- module nesting vs. GENERATE-IMPORTS command ............................... 94
- module, end of ............................................................................................ 168
- MODULE-LIBRARY-MODE command ........................................................ 196

## Modules
- modules, max nesting level ........................................................................ 195
- modules, type checking and nested .......................................................... 94

## Miscellaneous
- modulo of integers ...................................................................................... 193
- monitor calls ............................................................................................... 127
- Monitor_call standard routine .................................................................... 196

---

## Page 265

# Technical Index

## MS-DOS
- MS-DOS command line ......................................................... 71
- MS/DOS command line .......................................................... 192

## Multi-dimensional Array
- multi-dimensional array ....................................................... 37

## Multiplication
- multiplication ................................................................. 134

## Multisegment Load
- multisegment load on ND-100 ............................................ 188

## Booleans
- mutually exclusive union of Booleans or sets ....................... 232

## N
- name ...................................................................................... 6
- name length in bytes ............................................................. 190
- NATIVE routine modifier ....................................................... 70
- NATIVE routine modifier, same as C ........................................ 197
- ND-100 code compaction ...................................................... 224
- ND-100 direct load .............................................................. 164, 189
- ND-100 direct loading .......................................................... 96
- ND-100 extended instruction set ........................................... 197
- ND-100 multisegment load .................................................... 188
- ND-100 overlays ................................................................. 204
- ND-100 packed arrays ........................................................ 149
- ND-100 program file ........................................................... 209
- ND-100 two-bank load ........................................................ 221
- ND-100-EXTENDED command ............................................. 197
- ND-500(0) data/runtime organization ............................ 107
- ND-500(0) fast load ....................................................... 220
- negation of Boolean value ...................................................... 198
- nested modules ................................................................. 94
  - nested modules and IMPORT/EXPORT .................................... 94
  - nested modules and type checking ...................................... 94
  - nested modules vs. GENERATE-IMPORTS command ............... 94
- nested routine declaration ................................................... 62
- nested routine declaration and recursion ............................ 62
- New standard routine .......................................................... 50
- New standard routine and IN clause ..................................... 50
- New standard routine for dynamic memory allocation .......... 23, 197
- NIL pointer value ............................................................... 24, 32, 33, 198
- non-terminal ......................................................................... 6
- NOT Boolean operator .......................................................... 198
- notation ................................................................................. 4

## O
- object code disassembly by compiler ...................................... 157
- object code, disassembly of .................................................. 199
- object file generation with COMPILE command ...................... 157
- object-oriented programming in PLANC ................................. 75
- OBLIST command ............................................................... 157
- OBLIST compiler command ................................................... 199
- OBLIST compiler option ......................................................... 199

---

## Page 266

# Table of Contents

- [ON ASSERTFALSE Exception-Handler](#on-assertfalse-exception-handler)
- [ON Exception-Handler](#on-exception-handler)
- [ON ROUTINEERROR](#on-routineerror)
- [ON ROUTINEERROR Exception-Handlers](#on-routineerror-exception-handlers)
- [One-Dimensional Array](#one-dimensional-array)
- [Open Standard Routine](#open-standard-routine)
- [Operator Priority](#operator-priority)
- [Operator Priority, Table of](#operator-priority-table-of)
- [Operators and Implicit Type Conversion](#operators-and-implicit-type-conversion)
- [Operators and Type Checking](#operators-and-type-checking)
- [Operators, Priorities of](#operators-priorities-of)
- [OPTION Compiler Command](#option-compiler-command)
- [OR Operator](#or-operator)
- [Out-Value](#out-value)
- [Out-Values](#out-values)
- [Output Standard Routine](#output-standard-routine)
- [OVERFLOW Exception Condition](#overflow-exception-condition)
- [OVERLAY ND-100 Command](#overlay-nd-100-command)
- [Overload](#overload)
- [Overloaded Routines and Type Checking](#overloaded-routines-and-type-checking)
- [Overloaded Routines, Priority of User-Defined](#overloaded-routines-priority-of-user-defined)
- [Pack Modifier](#pack-modifier)
- [PACKED Arrays](#packed-arrays)
- [Packed Arrays](#packed-arrays-1)
- [PACKED Modifier](#packed-modifier)
- [Packed Record](#packed-record)
- [Packed Records](#packed-records)
- [Packing Composite Commands](#packing-composite-commands)
- [Packing of Composite-Data Types](#packing-of-composite-data-types)
- [Page Shift in Listing](#page-shift-in-listing)
- [PARALLEL Routine Modifier](#parallel-routine-modifier)
- [Parallel Routine Resumption](#parallel-routine-resumption)
- [Parallel Routine Suspension](#parallel-routine-suspension)
- [Parallel Routine Suspension and Resumption Combined](#parallel-routine-suspension-and-resumption-combined)
- [Parallel Routines](#parallel-routines)
- [Parallel Routines and Routines as Record Components](#parallel-routines-and-routines-as-record-components)
- [Parallel Routines, Detaching](#parallel-routines-detaching)
- [Parallel Routines, How They Make Programming Easier](#parallel-routines-how-they-make-programming-easier)
- [Parallel Routines, Restarting](#parallel-routines-restarting)
- [Parameter Access in Routines, Default](#parameter-access-in-routines-default)
- [Parameter List](#parameter-list)
- [Parameter Transfer](#parameter-transfer)
- [Parameter Transfer for Arrays](#parameter-transfer-for-arrays)
- [Parameter Transfer for Records](#parameter-transfer-for-records)
- [Parameter Without Parentheses](#parameter-without-parentheses)
- [Parameters and Overloading Routines](#parameters-and-overloading-routines)

## ON ASSERTFALSE Exception-Handler
Page 21

## ON Exception-Handler
Pages 19, 199

## ON ROUTINEERROR
Page 15

## ON ROUTINEERROR Exception-Handlers
Page 108

## One-Dimensional Array
Page 37

## Open Standard Routine
Page 201

## Operator Priority
Page 52

## Operator Priority, Table of
Page 52

## Operators and Implicit Type Conversion
Page 56

## Operators and Type Checking
Pages 57, 113

## Operators, Priorities of
Page 116

## OPTION Compiler Command
Page 201

## OR Operator
Page 202

## Out-Value
Page 16

## Out-Values
Page 17

## Output Standard Routine
Page 202

## OVERFLOW Exception Condition
Page 203

## OVERLAY ND-100 Command
Page 204

## Overload
Page 6

## Overloaded Routines and Type Checking
Page 57

## Overloaded Routines, Priority of User-Defined
Page 53

## Pack Modifier
Page 204

## PACKED Arrays
Page 38

## Packed Arrays
Page 106

## PACKED Modifier
Pages 49, 106, 204

## Packed Record
Pages 49, 127

## Packed Records
Page 106

## Packing Composite Commands
Page 204

## Packing of Composite-Data Types
Page 106

## Page Shift in Listing
Page 166

## PARALLEL Routine Modifier
Pages 82, 205

## Parallel Routine Resumption
Page 158

## Parallel Routine Suspension
Page 159

## Parallel Routine Suspension and Resumption Combined
Page 160

## Parallel Routines
Page 81

## Parallel Routines and Routines as Record Components
Page 81

## Parallel Routines, Detaching
Pages 84, 85

## Parallel Routines, How They Make Programming Easier
Pages 81, 83

## Parallel Routines, Restarting
Page 88

## Parameter Access in Routines, Default
Page 126

## Parameter List
Page 18

## Parameter Transfer
Pages 69, 126

## Parameter Transfer for Arrays
Page 126

## Parameter Transfer for Records
Page 126

## Parameter Without Parentheses
Page 18

## Parameters and Overloading Routines
Page 114

---

## Page 267

# Table of Contents

| Topic                                                             | Page |
|-------------------------------------------------------------------|------|
| parameters and type checking                                      | 57   |
| parameters to co-routines                                         | 89   |
| parameters to macros                                              | 99   |
| parameters, READ access type modifier for routine                 | 69   |
| parameters, variable number of                                    | 231  |
| parity bit                                                        | 69   |
| Pascal                                                            | 3    |
| PASCAL routine modifier                                           | 206  |
| PLANC availability                                                | 3    |
| PLANC command processor                                           | 96   |
| PLANC debugger                                                    | 3    |
| PLANC editor                                                      | 3    |
| PLANC indentation                                                 | 3    |
| PLANC macros                                                      | 99   |
| PLANC modules                                                     | 194  |
| PLANC pretty-print                                                | 3    |
| PLANC utilities                                                   | 3    |
| PLANC version information from compiler                           | 229  |
| PLANC, origins of                                                 | 3    |
| POINTER declaration                                               | 29   |
| pointer dereferencing                                             | 33   |
| pointer to variable, how to make one                              | 147  |
| POINTER type constructor                                          | 28, 29, 33, 206 |
| pointer value NIL                                                 | 32, 198 |
| pointer, value assignment to                                      | 31   |
| pointer, value assignment to variable pointed to                  | 31   |
| pointer-implied list, appending to                                | 148  |
| POINTERERROR exception condition                                  | 51, 207 |
| pointers and Addr                                                 | 33   |
| pointers and Ind                                                  | 33   |
| pointers and routines                                             | 73   |
| pointers and the ++/-- operators                                  | 54   |
| pointers, de-referencing                                          | 182  |
| pointers, implicit dereferencing of                               | 23   |
| portable programming in PLANC                                     | 126  |
| PRECISION modified real                                           | 29   |
| PRECISION modifier for REAL numbers                               | 208  |
| precision of reals                                                | 213  |
| PRECISION real modifier                                           | 28   |
| Pred standard routine                                             | 31, 208 |
| predecessor of enumeration value                                  | 208  |
| predeclaration                                                    | 6, 146 |
| predeclaration of routine                                         | 62, 65 |
| predefined routine                                                | 6    |
| preprocessor pass, absence of                                     | 96   |
| PRESENT command                                                   | 208  |
| PRESENT test for presence of constant during compilation          | 98   |
| pretty-print                                                      | 3    |

---

## Page 268

# Page 256

## Priorities

- priorities of operators/standard routines ................................................. 116
- priority adjustment of routines ................................................................... 208
- priority of operators ....................................................................................... 52
- priority of operators, table of ....................................................................... 52
- priority of user-defined routines, default ................................................. 53
- PRIORITY routine declaration clause ....................................................... 53, 208

## Production

- production ........................................................................................................ 6
- PROG-FILE ND-100 command ................................................................... 209
- PROGRAM routine type ............................................................................... 11, 71, 209

## Program Stack

- program stack .................................................................................................. 182
- program stack initialization ....................................................................... 182
- PUBLIC record components ....................................................................... 210

## Q

- quasi-parallel routines ................................................................................... 81
- quasi-parallel routines and routines as record components .................. 81
- quasi-parallel routines, detaching .............................................................. 84, 85
- quasi-parallel routines, how they make programming easier ................. 81,
- ............................................................................................................................. 83
- quasi-parallel routines, how to declare ..................................................... 205
- quasi-parallel routines, restarting .............................................................. 88

## R

- range indicator ................................................................................................. 140 
- RANGE integer modifier ............................................................................. 28, 210
- range limitations on integer ........................................................................ 210
- RANGEERROR exception condition ......................................................... 211
- RANGEERROR exception-handler ............................................................ 150
- READ access modification on routine parameters ................................. 30
- READ access type modifier ......................................................................... 28, 29, 211
- READ access type modifier for routine parameters .............................. 69
- READ and arrays ............................................................................................. 37
- REAL ................................................................................................................... 212
- REAL declaration ........................................................................................... 29
- real numbers, PRECISION of ......................................................................... 208
- REAL PRECISION declaration .................................................................... 29
- REAL variables, precision of ...................................................................... 213
- REAL-PRECISION command ..................................................................... 213
- REAL8 ............................................................................................................... 214
- recompilation, how to avoid unnecessary ................................................. 101
- record and dynamic memory allocation ................................................... 44
- record component access ............................................................................ 18, 138
- record component access notation and Ind .............................................. 23
- record component inheritance .................................................................... 43
- record component inheritance in variant records ................................... 50
- record component position, how to find ................................................... 151
- record components, valid .............................................................................. 50

---

## Page 269

# Table of Contents

| Topic | Page(s) |
|-------|---------|
| record containing routines | 75 |
| RECORD declaration | 15, 111 |
| record declaration | 50 |
| record declaration without existing type | 43 |
| record declaration, end of | 168 |
| RECORD declaration | 140 |
| record initialization | 16 |
| record packed | 127 |
| RECORD type constructor | 43, 214 |
| record type declaration | 43 |
| record variants | 43, 50, 75 |
| record, packed | 49 |
| record, public components of | 210 |
| record-with-routines and USING | 75 |
| records and type checking | 115 |
| records transferred as parameter to routine | 126 |
| records, dynamic allocation of | 50 |
| records, initialization of | 113 |
| records, packed | 106 |
| records, store to all components of | 54, 55 |
| recursion | 62 |
| recursion and nested routine declaration | 62 |
| recursive routines | 62 |
| REFERENCE routine modifier | 70, 215 |
| REFERENCE routine modifier and ERRETURN | 70 |
| relocatable code, disassembly of | 199 |
| Remove standard routine | 47, 216 |
| removing variable from linked list | 47 |
| representation of non-packed data in bytes | 108 |
| restarting co-routines | 88 |
| RETURN operator | 216 |
| REVERSE clause in FOR loop | 23, 26 |
| REVERSE loop range modifier | 217 |
| reversing ranges in for loops | 217 |
| routine address equivalencing | 71 |
| routine call overhead, how to avoid | 183 |
| routine call-hierarchy listing | 155 |
| ROUTINE declaration | 13, 111, 217 |
| ROUTINE declaration in RECORD | 15 |
| routine declaration inside records | 75 |
| routine declaration, end of | 168 |
| routine declaration, nested | 62 |
| ROUTINE declarations, new syntax | 13 |
| ROUTINE declarations, old syntax | 14 |
| routine entry/exit sequence | 70 |
| routine error return | 170 |
| routine in-value, read only access to | 30 |
| routine interface to PASCAL | 206 |

---

## Page 270

# Routine Modifiers

- routine modifier MAINSTART ...................................................... 71
- routine modifier PARALLEL ........................................................ 82
- routine modifier SPECIAL .......................................................... 70
- routine modifiers ............................................................................. 68
- routine names that are special characters ............................ 66

# Routine Overloading

- routine overloading ........................................................................ 15, 18
- routine parameter list .................................................................... 18
- routine parameter separator ...................................................... 142
- routine parameter without parentheses ................................. 18

# Routine Parameters

- routine parameters and overloading ...................................... 114
- routine parameters, READ access type modifier for ......... 69
- routine parameters, variable number of ................................ 231

# Routine Pointers

- routine pointers .............................................................................. 73
- routine predeclaration ................................................................... 62, 65

# Routine Type

- ROUTINE type constructor ......................................................... 217
- routine type declaration .............................................................. 73
- routine with access modification on parameters ............... 30
- routine-call type checking ......................................................... 114

# Error Handling

- ROUTINEERROR exception condition .................................. 220

# Routines and Type Checking

- routines and type checking ........................................................ 57
- routines callable from C, COBOL and FORTRAN .......... 68
- routines callable from COBOL and FORTRAN .................. 70
- routines in records and USING ................................................ 75
- routines in records, how to call .................................................. 18
- routines with in-values ................................................................. 146
- routines, adjusting priority of .................................................... 208
- routines, C modifier ....................................................................... 70
- routines, exporting out of modules ......................................... 171
- routines, importing from other modules ................................ 178

# Routine Modifiers Continued

- routines, INLINE modifier ............................................................ 69
- routines, NATIVE modifier ........................................................... 70
- routines, parallel ............................................................................. 81
- routines, priority of user-defined ............................................ 53
- routines, REFERENCE modifier ................................................ 70
- routines, returning from .............................................................. 216
- routines, STANDARD modifier ................................................... 70

# Data Organization

- routines and type checking ........................................................ 113
- runtime/data organization on the ND-500(x0) ........................ 107

# S

- screen I/O ........................................................................................... 3
- segment handling on INTEL 80286 .......................................... 224
- SELECT command, limitations of .............................................. 101
- SELECT compiler command ...................................................... 101, 220
- semantics ............................................................................................ 6
- separate compilation of modules and IMPORT/EXPORT ... 93
- SEPARATE-DATA as dummy command ................................... 222
- SEPARATE-DATA command ..................................................... 221

---

## Page 271

# SET Declarations

- SET declaration ............................................................ 41, 111
- set membership, test for ................................................ 180
- set of BYTE ................................................................. 41
- set of enumeration values .............................................. 41
- set operations ............................................................... 42
- SET type constructor ..................................................... 41, 222

# Set Operations

- set unions ................................................................. 42, 202
- set unions, mutually exclusive .................................... 232
- set variables ............................................................. 222
- set, adding value to ................................................... 185
- set, removing value from .......................................... 216
- sets, store to all elements in ..................................... 54
- sets, initialization of .................................................. 113

# Operations and Operators

- SHIFT operator ............................................................ 223

# Simple Types

- simple type ............................................................... 7, 111
- simple type declaration ............................................ 27, 28
- simple type modifiers ................................................ 28 
- simple types and type checking ................................ 115
- simple types, byte size of .......................................... 32
- simple types, list of ................................................... 27
   
# Programming Languages

- SIMULA .............................................................. 3, 75

# Compiler and System

- SINTRAN III command abbreviation in compiler ......... 96
- SINTRAN III monitor calls .......................................... 196

# Component Size

- size of data types in bytes .......................................... 223
- size of variable or record component in bits ............... 151

# Miscellaneous

- Size standard routine .................................................. 33, 223
- source code listing ....................................................... 189
- source debugger .......................................................... 3
- source file compilation ................................................. 157
- source file extension, default ...................................... 157
- source file inclusion in compilation ............................ 181
- source file with compiler commands ......................... 96
- source file, including ................................................... 97
- source line numbering ................................................ 158
- special characters ........................................................ 7
- special characters as routine names ............................ 66
- SPECIAL routine modifier ........................................... 70, 223
- SPLIT-CODE command for 80286 segment handling .. 224
- SQUEEZE compiler option .......................................... 224

# Stack Management

- stack ........................................................................... 124, 182
- stack frames ............................................................... 124
- stack initialization ........................................................ 11, 182
- stack overflow ............................................................. 125
- stack overflows ........................................................... 18
- stack pushing overhead, how to avoid ..................... 183
- stack underflow ........................................................... 125
- stack, declaration of ..................................................... 11
- STACKERROR exception condition .............................. 224

---

## Page 272

# Technical Index

| Topic                                                                   | Page |
|------------------------------------------------------------------------|------|
| stacks and co-routines                                                 | 82   |
| standard routine                                                       | 7    |
| STANDARD routine modifier                                              | 70   |
| STANDARD routine modifier and ERRETURN                                 | 70   |
| STANDARD routine modifier, interface to COBOL and FORTRAN              | 224  |
| standard routines and type checking                                    | 57   |
| standard routines, priorities of                                       | 116  |
| statement                                                              | 5, 7 |
| statement continuation to next line                                    | 131  |
| statement delimiter                                                    | 131  |
| statement separator                                                    | 142  |
| storage alignment for non-packed data                                  | 108  |
| store operator                                                         | 144  |
| store operators                                                        | 53   |
| store value in variable                                                | 144  |
| storing values to composite variables                                  | 39   |
| string concatenation                                                   | 55, 139 |
| string continuation to next line                                       | 132  |
| string delimiter                                                       | 133  |
| strings of bytes                                                       | 153  |
| subarrays                                                              | 149  |
| subtraction                                                            | 137  |
| Succ standard routine                                                  | 31, 225 |
| successor of enumeration value                                         | 225  |
| swap operator                                                          | 53   |
| switches                                                               | 19   |
| switching statement                                                    | 155  |
| SYMDEB                                                                 | 3    |
| syntax                                                                 | 7    |
| syntax check in LED                                                    | 3    |
| SYSTEM import statement modifier                                       | 225  |
| system type, how to get                                                | 225  |

# T

| Topic                                                                   | Page |
|------------------------------------------------------------------------|------|
| target computer type for code being compiled                           | 225  |
| TARGET-MACHINE command                                                 | 225  |
| terminal output                                                        | 3    |
| terminal, output to                                                    | 202  |
| terminal, reading from                                                 | 184  |
| terminology                                                            | 4    |
| test for exception condition in program                                | 150  |
| THEN in conditional compilation                                        | 225  |
| THEN in conditional statements                                         | 226  |
| token                                                                  | 7    |
| tokens, fonts for                                                      | 4    |
| TRUE Boolean value                                                     | 226  |
| type                                                                   | 7, 12, 111 |
| type access modification                                               | 230  |

---

## Page 273

# Technical Index

| Topic                                            | Page       |
|--------------------------------------------------|------------|
| type access modifier READ                        | 211        |
| TYPE and ARRAY                                   | 38         |
| TYPE and record declaration                      | 43         |
| TYPE and ROUTINE                                 | 73         |
| type casting, the PLANC approach                 | 32         |
| type checking                                    | 7, 57, 58, 111, 112, 114 |
| type checking and nested modules                 | 94         |
| type checking in declarations                    | 112        |
| type checking of composite types                 | 115        |
| type checking of record variants                 | 115        |
| type checking of routines/operators              | 113        |
| type checking of simple types                    | 115        |
| type construction                                | 34         |
| type constructor                                 | 7, 27      |
| type constructors                                | 111        |
| type conversion                                  | 32, 56, 161|
| type conversion, implicit                        | 116        |
| type declaration                                 | 226        |
| type declarations outside module                 | 93         |
| type expression                                  | 8, 34      |
| type modifiers                                   | 27         |
| type replication                                 | 227        |
| type size in bytes                               | 223        |
| type, simple                                     | 111        |
| TYPE, the type construction keyword              | 34, 226    |
| Typeof standard routine                          | 33, 35, 227|
| Typeof standard routine in declarations          | 30         |
| types and pointers                               | 206        |
| types, global                                    | 93         |
| types, packed                                    | 106        |

## U

| Topic                                            | Page       |
|--------------------------------------------------|------------|
| unconditional jumps                              | 21, 176    |
| underscore character                             | 232        |
| unequal                                          | 145        |
| union of Booleans or sets                        | 202        |
| UNIX command line                                | 71, 192    |
| UNSIGNED integer modifier                        | 28, 227    |
| user routine priority                            | 52         |
| user-defined value ranges                        | 169        |
| USING block                                      | 47         |
| USING blocks                                     | 65, 227    |
| USING vs. record-with-routines                   | 75         |
| utilities see PLANC utilities                    | 3          |

---

## Page 274

# V

| Topic                                            | Page                    |
|--------------------------------------------------|-------------------------|
| valid digits for reals                           | 213                     |
| value                                            | 8                       |
| value assignment                                 | 144                     |
| value assignment operators                       | 53                      |
| value of variables pointed to                    | 182                     |
| value range limitations on integer               | 210                     |
| variable                                         | 8                       |
| variable access modification                     | 211, 230                |
| variable initialization                          | 16, 141                 |
| variable type conversion                         | 161                     |
| variable type re-interpretation                  | 174                     |
| variable types, how to make                      | 226                     |
| variable value swap                              | 141                     |
| variables and pointers                           | 206                     |
| variables, exporting out of modules              | 171                     |
| variables, importing from other modules          | 178                     |
| variables, local                                 | 30                      |
| variables, replicating the type of               | 227                     |
| variant records                                  | 43, 50, 75              |
| variant records and type checking                | 115                     |
| variants of record                               | 50                      |
| VERSION-INFORMATION compiler command             | 229                     |
| VOID special type                                | 17, 28, 230             |

# W

| Topic                                            | Page                    |
|--------------------------------------------------|-------------------------|
| WHILE statement                                  | 19, 22, 25, 230         |
| WHILE statement in FOR loop                      | 25                      |
| WRITE access modification on routine parameters  | 30                      |
| WRITE access type modifier                       | 28, 29                  |
| WRITE and arrays                                 | 37                      |
| WRITE type modifier                              | 230                     |

# X

| Topic                                            | Page                    |
|--------------------------------------------------|-------------------------|
| XARGS routine modifier                           | 231                     |
| XOR operator                                     | 232                     |
| XREF command                                     | 232                     |

---

## Page 275

```
[Image not readable due to heavy staining]
```

---

## Page 276

I'm unable to provide any transcription or diagram conversion from this image, as it does not contain text or identifiable technical diagrams. It appears to be an image with some colored rectangles on a textured background.

---

