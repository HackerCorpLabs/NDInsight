# ND-10732 — ND Telex

> Status: IN-PROGRESS — real floppy decoded, including a complete real SINTRAN monitor-call installation (DMAC assembly)

| Field | Value |
|-------|-------|
| ND article number | `ND-10732` |
| Product name | ND Telex |
| Functional category | Networking & Communications |
| CPU target | ND-100 |

## What is known — real floppy, decoded

Floppy `10732B01-EN-01D` (downloaded via NDwiki, imaged by Torfinn "Tingo" Ingolfsen; reported
"errors" by the imager, but it mounted and read cleanly in this session) contains:

```
INSTALL-TLX-B01:PROG       real installer (compiled, not decoded)
TLX-INSTALL-B00:XCOM       installer command file
FTNLIBR-2091F:BRF          a FORTRAN library dependency
TLX-LIB-B01:BRF            the Telex call library
TLX-MAIN-B01:BRF           main program runtime
TLX-UTIL-B01:BRF           utility runtime
TLX-MN177-B00:MODE         a real, decoded SINTRAN monitor-call installation script — see below
```

## Installing monitor call 177(octal) — real, decoded `TLX-MN177-B00:MODE`, in full

Source: byte-for-byte decode (`byte & 0x7F`). [MODE] This is a genuine, low-level SINTRAN kernel
patch — Telex needs to write to a terminal device **without reserving it** (so other programs can
still use the terminal concurrently), and SINTRAN has no built-in monitor call for that, so the
product defines a new one via a direct `@DMAC` (SINTRAN's resident assembler) patch.

```
@CC **********************************************************************
@CC * MONITORCALL FOR WRITING TO A TERMINAL WITHOUT RESERVING THE DEVICE *
@CC **********************************************************************
@CC ALWAYS DEFINE OUTPUT FILE FOR MODEFILE TO A PRINTER OR TO A FILE
@CC SO IT IS EASY TO CHECK FOR ERRORS.
@CC
@CC BEFORE RUNNING THIS MONITORCALL PATCH, CHECK THAT THE ADDRESS
@CC IN POSITION 141 ON IMAGE SEGMENT(2) IS LESSER THAN 107704. IF NOT
@CC THIS THE CASE, IT IS NOT POSSIBLE TO INSTALL MONITORCALL.
@CC
@CC LOG IN AS USER SYSTEM.
@CC @LOOK IMAGE
@CC 141/ ??????.   % NOTE THIS ADDRESS
@CC
@CC ALWAYS DUMP THE RESULT TO A LISTFILE SO IT LATER WILL BE
@CC POSSIBLE TO SEE WHAT POSITION 141 CONTAINED BEFORE PATCH.
@CC
@CC A WARM-START HAS TO BE PERFORMED AFTER THE PATCH.
@CC
@CC THE MONITORCALL MUST BE DEFINED IN SINTRAN:
@CC @SINTRAN-SERVICE
@CC *DEFINE-USER-MON-CALL
@CC 177     % MONITOR CALL NUMBER
@CC ADDRESS % POS.141 BEFORE PATCH (USE ADDRESS FROM 141/??????)
@CC 16      % TYPE. 16 MEANS: CAN BE CALLED FROM RT AND BACKGROUND
@CC Y       % RESIDENT
@CC Y       % IMAGE
@CC N       % SAVE AREA
@CC EXIT
@CC
@CC EACH TIME A COLD-START IS PERFORMED, THIS MODE-JOB MUST BE
@CC EXECUTED AND MONITORCALL DEFINED IN SINTRAN SERVICE.
@CC
@CC DO NOT RUN THIS JOB MORE THAN 1 TIME. THE ADDRESS IN LOCATION
@CC 141 WILL BE SET TO NEW END RESIDENT AREA EACH TIME.

@SCHEDULE 1201
@DMAC
)CLEAR
)CLOAD 2
)RESSM
141/7ENDC=^
7ENDC:
7ENDC/LOGPH
      GET5
      D0
      D1
      D2
      D3
      D4
      ZAREG
      ZBREG
      ZTREG
      ZDREG
      ZPREG
      ZXREG
      5TERM
      5BAD
      5NORE
      TYPRI
      SETPA
      XERDE
      RET
)SYSDF
)9ASSM (SYS)SYMB-1-LIST
)SYSDF
)9ASSM (SYS)SYMB-2-LIST
7ENDC/   JPL I (GET5             % Get parameters in D0,D1,D2,D3,D4
         LDA   ,B D0             % Terminal number
         JPL I (LOGPH            % Find datafield addresses
         COPY  SA DT             % Input datafield in T-register
         STT   ,B ZTREG          % Input datafield in T-register
         COPY  SD DT             % Output datafield addresses in D-register
         STT   ,B ZDREG          % Output datafield addresses in D-register
         LDA   ,B D1             % Load function code
         JAN   OUT               % If negative, no function
         JAZ   OUT               % If ZERO, no function
         LDX   ,B ZDREG          % Load output datafield
         SAA   33                % No such device number
         JXZ   ERR               % No output datafield, return
         LDA   ,X TYPRI          % Load TYPRING word from datafield
         BSKP  IF 170 ONE DA     % Test for IO allowed
         JMP   IOERR
         BSKP  IF 50 ONE DA      % Test for term bit
         JMP   TRBAD             % So test if BAD bit is set
         JMP   CNTUE             % Do the actual job
TRBAD,   BSKP  IF 40 ZRO DA      % Test for BAD bit
         JMP   CNTUE             % We can write on this device
IOERR,   SAA   33                % Error
         JMP   ERR               % Exit
CNTUE,   LDA   ,X TYPRI          % Load TYPRING word
         STA   SAVTY             % Save TYPRING word
         LDA   ,B D1             % Load function code
         AAA   -3                % Test for function code <3
         JAZ   FNC3              % Function code 3 reset NORES flag
         LDA   SAVTY             % Load saved TYPRING
         BSET  ONE 30 DA         % Set 5NORES bit to write on this device
         STA   ,X TYPRI          % Set new word into datafield
         LDA   ,B D1             % Load function code
         AAA   -2                % Test for function code 2
         JAZ   OUT               % Function code 2 set NORES flag
         LDT   ,B D0             % Load device number
         LDA   ,B D2             % Load character from parameter 2
         JPL   I (SETPA          % Set parity
         MON   2                 % Output
         JMP   ERR               % Error
         LDA   SAVTY             % Load saved TYPRING
         STA   ,X TYPRI          % Save TYPRING word back
         JMP   OUT
FNC3,    LDA   ,B D0             % Load device
         AAA   -1                % If device 1
         JAZ   OUT               % go out
         LDA I (XERDE            % Load error_device terminal
         SUB   ,B D0             % Is error_device this terminal?
         JAZ   OUT               % Go out
         LDA   SAVTY
         BSET  ZRO 30 DA         % Reset 5NORES bit to write on this device
         STA   ,X TYPRI
OUT,     SAA   0                 % No error
ERR,     STA   ,B ZAREG          % Store A-reg
         JMP I (RET              % Return to monitor
SAVTY,   0
)FILL
)KILL 7ENDC
7ENDC=*
7ENDC:
141/7ENDC
)LINE
)9EXIT
```

**Reading it**: `@DMAC` is SINTRAN's resident kernel assembler/patcher (distinct from `@MAC`,
`@QED`, and `@RT-LOADER` seen elsewhere in this catalog). The script assembles a small routine
directly into the resident kernel area, then patches location `141` (octal, on `IMAGE` segment 2)
to point at it — this is exactly the low-level `SINTRAN-SERVICE`/`DEFINE-USER-MON-CALL` mechanism
that registers a genuinely new monitor call (number `177` octal) system-wide. The routine itself
(`MON 2` = the real terminal-output monitor call) checks a device's `TYPRING` control word for the
"IO allowed"/"term"/"BAD" bits before writing, and can toggle the `5NORES` (no-reserve) bit on
that word via function codes 2/3 — this is the actual mechanism that lets Telex write output to a
terminal a *different* program currently has reserved, without disturbing that reservation.
**This monitor call must be re-installed after every cold start** (the comment says so explicitly)
— it does not survive a cold boot, only a warm start.

## Documentation
- No PD sheet, no PI sheet located.

## Provenance & open items
- Source: one real floppy image, downloaded via NDwiki and decoded in this session
  (`ndfs -t`/`ndtool -x` for listing/extraction, `byte & 0x7F` for the `:MODE` script, in full).
- **TODO:** `TLX-INSTALL-B00:XCOM` (the actual product installer, as opposed to this monitor-call
  patch) was not decoded.

---
**Parent:** [../README.md](../README.md) (Software catalog)
