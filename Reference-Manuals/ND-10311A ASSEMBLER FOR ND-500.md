## Page 1

# NORD Software Library Diskette

## Containing

- Assembler for ND-500

## Directory Information

| Field         | Value        |
|---------------|--------------|
| Directory Name| ND-10311A    |
| User Name     | FLOPPY-USER  |

## File Information

- File 0: `(ND-10311A:FLOPPY-USER)ASSEMBLER-500-A:SPUN#1`

## Date

3 June 1981

---

## Page 2

# NORD SOFTWARE LIBRARY

## PROGRAM DESCRIPTION

| PRODUCT    | NAME               | ND-NUMBER | CATEGORY |
|------------|--------------------|-----------|----------|
| Assembler for ND-500 |            | 10311A    | P        |

| ISSUED    | DATE 81.05.04    | BY (INITIALS) | Jensen |
|-----------|------------------|---------------|--------|

| COMPUTERS | .10 | .12 | .50 | X 100 | .500! | ....! |

| INSTR.SET | .48 BIT FL. | .32 BIT FL. | .COMMERCIAL |

| OP.SYSTEM | X SIN III VS! | .SIN III RT! | .ALONE | .... | ....... |

| DOCUMENTATION | NUMBER: 60.113.02 |

| TITLE     | NORD-500 Assembler Reference Manual |

| PURPOSE   | ND-500 Cross-assembler running on ND-100 |

| PROGRAMS (FILES) | PROG.NUMBER | NAME           | TYPE | CONTAINING |
|------------------|-------------|----------------|------|------------|
|                  | 203132A     | ASSEMBLER-500  | BPUN | ND-500 Assembler |

## LOADING/OPERATING PROCEDURE, USE

### Non-reentrant load:

```
@ENTER-DIRECTORY ND-10311 <floppy disk name and unit>
@PLACE-BINARY (ND-10311:FLOPPY-USER)ASSEMBLER-500
@DUMP "ASSEMBLER-500-A",0,1
@RELEASE-DIRECTORY ND-10311
```

### Reentrant load:

```
@ENTER-DIRECTORY ND-10311 <Floppy disk name and unit>
@COPY-FILE "ASSEMBLER-500-A:BPUN" (ND-10311:FLOPPY-USER)ASSEMBLER-500:BPUN
@RELEASE-DIRECTORY ND-10311
@DUMP-REENTRANT ASSEMBLER-500 0 1 ASSEMBLER-500
```

The assembler is dumped with address space 0<45777 on the BPUN file.

---

