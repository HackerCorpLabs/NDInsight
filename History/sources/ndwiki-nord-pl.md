# Source: ndwiki article "NORD PL"

- **Live page**: <https://www.ndwiki.org/wiki/NORD_PL>
- **Copy used here**: Wayback Machine snapshot, fetched 2026-08-27 by Ronny's
  request. The live ndwiki is behind an Anubis proof-of-work gate.

**Status: SECONDARY.** NORD PL is the language SINTRAN III's core and file system
were written in - "a programming language developed by ND, halfway between a
third-generation language and an assembler", per Norwegian Wikipedia.

Relevant to the history because it is the answer to "what was the operating system
written in", and because this repo has a large body of NPL source under
`SINTRAN/NPL-SOURCE/` and a developer guide in
`Developer/Languages/System/NPL-DEVELOPER-GUIDE.md`. Those outrank this page.

---

## Verbatim extract

NORD PL (NORD Programming Language, or NPL ) is a programming language from Norsk Data. It shipped as a standard component of the operating system SINTRAN III. It could also run freestanding, or under NORD-TSS.

The language was also used to implement SINTRAN III. The NORD PL compiler was also written in NORD PL and some core applications was early on written in NORD PL until PLANC came and linker and other software was rewritten in PLANC.

A quote from the NORD PL Users Guide, July 77, "important advice to the NORD PL programmer":

NORD PL is not a problem oriented high level language like FORTRAN or COBOL. It is a machine oriented medium level language introduced to simplify the assembly coding, i.e., in any statement written the programmer should call attention to the influence oon the register contents!

The NORD PL compiler produced MAC assembly code which then had to be assembled using the MAC assembler. The syntax resembles ALGOL, but it was used like an assembler because all instructions, addressing modes, registers, and memory could be accessed from the language. Compared to writing assembly (MAC) directly, the intention was to make it easier to write code and to improve error checking and readability. At the same time, using NORD PL produces code about as if assembly was used directly, unlike if a more traditional systems language (e.g. C) is used.

### Contents

- 1 Properties

- 2 Example code

- 3 Product number

- 4 Sources

### Properties

- Conditional compiling

- Declaration statements

- Arithmetical statements

- Constant expressions, evaluated at compile time

- Control statments, including:
GO unconditional branching
IF conditional branching
FOR loop control
WHILE conditional loop control
CALL subroutine call

- One pass compiler

- The compiler needs about 6.5Kwords of memory, plus symbol table (5 words per symbol)

I/O must be performed via monitor calls as when writing MAC directly.

### Example code

The registers of the CPU were available in NORD PL as predefined variables. Thus you could write:

X + T =: A

and the compiler would generate:

COPY SX DA
RADD ST DA

Functions could be declared with multiple entry points (the stand-alone colons represent 'your code goes here'):

SUBR ENT1, ENT2
ENT1:
T?:= 1
:
:
ENT2:
: 
EXIT
RBUS

(RBUS is SUBR backwards, and marks the end of the subroutine/function)

Here you could either call ENT1 which set T to 1 before falling into ENT2 or you could set T to something else and call ENT2. If the T register specified which file handle to write to you could then either call ENT1 to always output to terminal or you could specify a file handle yourself in T and call ENT2 to output to that file.

### Product number

NPL is part of ND-210400 Subsystem package II.

### Sources

- This article was originally a copy of the English Wikipedia article NORD Programming Language in 18 December 2008., but has been updated, extended, and corrected since then.

- Norsk Data Document ND-60.047.01 NORD PL USER'S GUIDE , first printing, printed August 1973

- Norsk Data Document ND-60.047.03 NORD PL USER'S GUIDE

- Structured design benefits to a process control software project - usage of NORD PL
