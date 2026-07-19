## Page 1

# INSTALLATION OF ND-100 PASCAL, VERSION J.

Date: 18 January 1984

## 1 FILES

To install the Nord Pascal system, the following files are needed:

1. **PASCAL-COD**  
   The Pascal compiler in BRF format.

2. **PASCAL-LIB or PASCAL-2LIB**  
   The Pascal run-time library in BRF format. PASCAL-LIB is used with one-bank code, while PASCAL-2LIB is used with two-bank code.

3. **PASCAL-ERR**  
   The compile-time error messages in symbolic form.

## 2 INSTALLATION PROCEDURE

1. Copy PASCAL-LIB to a system file named PASCAL-LIB:BRF. Copy PASCAL-2LIB to a system file named PASCAL-2LIB:BRF.

2. Copy PASCAL-ERR to a system file named PASCAL-ERR:SYMB.

3. The Pascal compiler runs as a two-bank program. Be aware that a terminal running the compiler or two-bank Pascal programs must have 128k user segments.

   To dump the PASCAL compiler as a re-entrant subsystem, enter the following commands. The commands marked (o) are optional: When the compiler prints a source program, it will by default print 60 (decimal) lines per page. To change this number, enter the `*DEFINE LINPP n` command (remember that n must be octal). The `*DEFINE NOBUF 4` command will set the number of files being buffered to 4. This will give better performance when programs with $INCLUDE commands are compiled.

---

## Page 2

# SINTRAN Version H

```
$NRL
*IMAGE-FILE 100
*SIZE 1500
(o) *DEFINE L1NPP n
(o) *DEFINE NOBUF 4
*LOAD PASCAL-COD PASCAL-2LIB
*VALUE PASCAL
xxxxxx                 11
*VALUE CONTINU
yyyyyy                 12
*DUMP "PASCAL:PROG",xxxxxx,yyyyyy
*EXIT

$DITAP "PASCAL" PASCAL

$DUMP-REENTRANT PASCAL,xxxxxx,yyyyyy,PASCAL
```

# SINTRAN Version I or Later

```
$NRL
*IMAGE-FILE 100
*SIZE 1500
(o) *DEFINE L1NPP n
(o) *DEFINE NOBUF 4
*LOAD PASCAL-COD PASCAL-2LIB
*VALUE PASCAL
xxxxxx
*VALUE CONTINU
yyyyyy
*DUMP "PASCAL:PROG",xxxxxx,yyyyyy
*EXIT

$DUMP-PROGRAM-REENTRANT PASCAL,PASCAL
```

The installation is now complete, and the system can be used according to the user manual.

---

