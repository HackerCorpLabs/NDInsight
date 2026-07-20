## Page 1

# NORD-PL

## INTRODUCTION

The NORD-PL is a powerful medium level language designed as replacement for assembly language to achieve faster program development and to improve maintainability of system software.

The syntax resembles that of ALGOL but all facilities of the machine language are available from the NORD-PL. The NORD-PL compiler produces MAC Assembler source code.

## FEATURES

- Object code with efficiency equal to or better than assembly code due to:
  - easy program checking
  - high readability of programs
  - easy maintenance of programs
- Assembly coded object code for on-line debugging
- In-line assembly statements
- Conditional compilation
- Data type declaration and checking

## PRODUCT DESCRIPTION

### Basic Elements

A NORD-PL program consists of the following basic elements: identifiers, numbers, character and string constants, operators, delimiters and reserved symbols.

### Data Structure

- Integers (16 bit)
- Double (32 bits)
- Real (48 bits)

These types may be used either as single variables or arrays. Pointer to variables and arrays may also be declared.

### Data Expressions

A data expression is evaluated at compile-time. The operators are:
- `+` Add
- `-` Subtract
- `*` Multiply
- `\` Byte separation

## Executable Expressions

In general, an expression specifies a series of operations between the primary operand, which is a register, and one or more secondary operands, which can be registers, variables or constants.

### Statements

Statements are the basic functional units of the NORD-PL. There are two classes of statements:

- Declaration statements (non-executable statements) which describe the characteristics and arrangements of data, and the classification of program units.

  These are:
  - Data declaration
  - Symbolic constants
  - Pointers
  - Addressing modes
  - Labels
  - Subroutines

- Executable statements which specify actions. These statements can be divided into two classes:

#### Arithmetical statements

Using variables and arithmetical operations, which are:
- Arithmetical:
  - `:=` Load
  - `=:` Store
  - `:=:` Swap
  - `-` Subtract
  - `+` Add
  - `*` Multiply
  - `/` Divide (reals only)

- Shift:
  - `SHZ` Shift with zero end input
  - `SH` Arithmetical shift
  - `SHR` Rotational shift
  - `SHL` Shift with link end input (bits shifted into the register are taken from the M bit in the status register, bits shifted out are fed to M. This corresponds to an extended 17 bit rotational shift).

- Logical:
  - `/\` AND
  - `\/` OR
  - `XOR` Exclusive OR

```
[Illustration: NORD COMPUTER SYSTEMS Logo]
```

```
NPL—A3—2500—0779
```

---

## Page 2

# Control Statements

Control Statements control the logical flow in the program. The Control Statements are:

- `GO`
- `IF THEN ELSE`
- `FOR STEP TO`
- `DO WHILE`
- `CALL`
- `EXIT`

# Commands

A command may be used to give control information to the compiler. All commands start with a circled alpha (@) followed by the command name. The command names are not reserved symbols so that the same symbol can be used for a command name as well as for a user variable.

The available commands are:

- `@ICR`
  - **"Ignore Carriage Return" mode.**
  - This command is used if a statement should need several lines (especially declaration statements). The carriage return is treated as if it were a space.

- `@CR`
  - **"Carriage Return" mode.**
  - After this command carriage return will have the same effect as the semicolon (;), so that it will terminate the current statement.

- `@EOF`
  - **"End of File".**
  - This command is used for exit from the compiler to the Operating System. The MAC command JLINE is output on the object device. The command will list the number of errors detected during the compilation on the communication device.

- `@CLEAR`
  - Clear the symbol table of the compiler.

- `@OCT`
  - All integer numbers will be treated as octal.

- `@DEC`
  - Integer numbers will be treated as decimal, except for those preceded by the '&' sign.

- `@DEV <input device>, <list device>, <object output device>`
  - This command is used for setting device numbers for the compiler. If the list device equals 0, the error messages will be printed on the output communication device, otherwise also on the list device.

- `@MODE <input communication device>, <output communication device>`

There are two ways of including assembly coding:

1. If the statement starts with an asterisk (*), the rest of the line will be taken as assembly code, being copied to the object output stream.

2. The command `@MAC` switches the compiler to assembly mode. The source code will pass unchanged to the output stream until an `@` is found.

- `@LIB`
  - Followed by a logical expression starts a mode that may be included depending on the value of the expression.

- `@ELIB`
  - Terminates the LIB mode.

- `@STLIB`
  - Followed by a symbol sets the library include flag for this symbol.

- `@NSLIB`
  - Followed by a symbol resets the library include flag for this symbol.

# REQUIREMENTS

NORD-PL may be run under the SINTRAN III/VS Operating System or as a stand-alone program.

NORD-PL requires approximately 10 Kwords plus main symbol table (5 locations per symbol) of memory.

# REFERENCES

ND—60.046 NORD-PL User’s Guide.

---

```plaintext
Norway:                            Sweden:                            Sweden:
NORSK DATA A.S                     ND NORSK DATA AB                    ND NORSK DATA AB
Jernkroken P.O. Box 4 Lindeberg gård Kanalvägen 3, Box 2031           Klangfärgsgatan 11, Box 9052
OSLO 10                            194 02  UPPLANDS VÄSBY              421 09  VÄSTRA FRÖLUNDA
Tel. 02-9916001, Tlx. 18661 nd n   Tel. 0760-86050, Tlx. 13528 nordata s Tel. 031-299590

Denmark:                           France:                            France:
NORSK DATA ApS                     NORSK DATA FRANCE                  NORSK DATA FRANCE
Overødvej 5                        "Le Brevent", Avenue du Jura       120, Bureaux de la Colline
2840  HOLTE                        01170 FERNEY-VOLTAIRE              92213  SAINT-CLOUD-CEDEX  
Tel. 02-425055, Tlx. 37725 nd dk   Tel. 050-408576, Tlx. 38563 nordata ferrv Tel. 01-6032366, Tlx. 291108 nd paris

West Germany:                      U.S.A.:                            England:
NORSK DATA-DEUTSCHLAND             NORSK DATA N.A., Inc.              RICHARD NORTON (NORD) Ltd.
Abraham-Lincoln-Str. 30            65, William Street                 NORD HOUSE, 17 Balfle Street, King's Cross
6200  WIESBADEN                    Wellesley, MASS. 02181             LONDON N19BE
Tel. 061-764220, Tlx. 4186370 noda d Tel. 061-237-7945, Tlx. 921740 norsk well Tel. 01-2785501, Tlx. 299537 norton gld

Note: NORSK DATA reserves the right to change specifications at any time without given notice.
```

---

