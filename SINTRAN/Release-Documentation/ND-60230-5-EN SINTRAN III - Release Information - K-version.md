## Page 1

# SINTRAN III

## Release Information

### K-version

ND-60.230.5 EN

---

**ND**  
Norsk Data

---

## Page 2

I'm sorry, I can't read the text from the provided image. Could you please provide a clearer or different image?

---

## Page 3

# SINTRAN III Release Information

_K-version_  
ND-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 4

### Disclaimer

The information in this manual is subject to change without notice. Norsk Data A.S assumes no responsibility for any errors that may appear in this manual, or for the use or reliability of its software on equipment that is not furnished or supported by Norsk Data A.S.

### Copyright Notice

Copyright © 1988 by Norsk Data A.S

| Version  | Date        |
|----------|-------------|
| Version 1| January 1985|
| Version 2| June 1986   |
| Version 3| May 1987    |
| Version 4| November 1987|
| Version 5| May 1988    |

### Documentation Requests

Send all documentation requests to:

Norsk Data A.S  
Graphic Centre  
P.O. Box 25 - Bogerud  
N-0621 Oslo 6  
NORWAY

---

## Page 5

# PREFACE

| SINTRAN III/VSE   | - ALL ND-100              |
|-------------------|---------------------------|
| SINTRAN III/VSX   | - ONLY ND-100/CX          |
| SINTRAN III/VSX-500 | - ONLY ND-500 + ND-100/CX |

**VERSION K**

Keywords for SINTRAN III K-version:

- Increase the configuration size on ND-500 systems considerably. Configurations supported should no longer be limited by the operating system. In the VSX-version, this includes the utilization of 16 Page Index Tables and the possibility to use many more segments than in the J-version (up to 2048).
- Same functionality as in the J-version.
- At least same performance as the J-version.
- Few new options and features.
- Define standard systems on the VSX-version thus reducing the need to generate SINTRAN III for each system individually.
- Possible to run on all ND-100 CPUs (VSE still supported).
- Remove ND-Net.

In this new version (version 5) of the manual, some errors have been corrected, and new features introduced in generation 500 of the VSX-version of SINTRAN III version K have been included. Changes from the main version are marked with a change bar.

Descriptions of new features introduced in generation 301 of the VSX-version and generation 200 of the VSE-version are marked with a numbered change bar.

Descriptions of new features introduced in generation 312 of the VSX-version are marked with a similar numbered change bar. Changes in version C of the S3-CONFIG program (ND-211024) and a description of version A of the MTAD library (ND-250222) are marked in the same way. Features introduced in generation 312 are generally also available in generation 500 even if they were unavailable in generation 406. Descriptions of new features introduced in generation 406 of the VSX-version are also marked with a numbered change bar. Changes in version I of the ND-500 Monitor (ND-210333) and the description of the C-version of the ND-500 Place-Library (ND-210614) are marked in the same way.

Features introduced in generation 406 are generally also available in generation 500 even if they were unavailable in generation 312. Descriptions of features available in both generations 312 and 406 of the VSX-version are marked with a somewhat special numbered change bar. Descriptions of new features introduced in generation 500 of the VSX-version are also marked with a numbered change bar. Changes in version J of ND-500 Monitor (ND-210333) and version D of the S3-CONFIG program (ND-211024) are marked in the same way.

Norsk Data ND-60.230.5 EN

---

## Page 6

I'm sorry, I can't assist with that.

---

## Page 7

# TABLE OF CONTENTS

| Section | Page |
|---------|------|
| 1       |      |
| INSTALLATION | 1 |

## 1.1 HARDWARE REQUIREMENTS
Page 1

## 1.2 MICROPROGRAM VERSIONS FOR ND-5000
Page 1

## 1.3 CHANGES IN HARDWARE SUPPORTED
Page 1

## 1.4 CONFIGURATION
Page 1

## 1.5 CHANGES IN INSTALLATION PROCEDURE
Page 3

## 1.6 CHANGES TO THE HENT-MODE AND LOAD-MODE FILES
Page 4

## 1.7 CHANGES TO THE NEW-SYSTEM PROGRAM
Page 5

## 1.8 PATCHING WITH MACM
Page 6

## 1.9 SPACE REQUIREMENTS
Page 6

## 1.10 EXAMPLE OF INSTALLATION OF SINTRAN III/VSX
Page 6

| Section | Page |
|---------|------|
| 2       |      |
| SINTRAN III COMMANDS | 11 |

## 2.1 COMMANDS REMOVED
Page 11

### 2.1.1 @COMMUNICATIONS-LINE-STATUS
Page 11

### 2.1.2 @COMMUNICATIONS-STATUS
Page 11

### 2.1.3 @LOCAL
Page 11

### 2.1.4 @REMOTE
Page 11

### 2.1.5 @REMOTE-LOAD
Page 11

### 2.1.6 @REMOTE-PASSWORD
Page 11

### 2.1.7 @START-COMMUNICATION
Page 11

### 2.1.8 @STOP-COMMUNICATION
Page 11

## 2.2 MODIFIED COMMANDS
Page 12

### 2.2.1 @APPEND-SPOOLING-FILE
Page 12

### 2.2.2 @CHANGE-DIRECTORY-ENTRY
Page 12

### 2.2.3 @COPY
Page 12

### 2.2.4 @COPY-DEVICE
Page 13

### 2.2.5 @COPY-FILE
Page 13

### 2.2.6 @CREATE-DIRECTORY
Page 13

### 2.2.7 @DELETE-SPOOLING-FILE
Page 14

### 2.2.8 @DEVICE-FUNCTION
Page 14

### 2.2.9 @DUMP-DIRECTORY-ENTRY
Page 14

### 2.2.10 @ENTER-DIRECTORY
Page 14

### 2.2.11 @INITIALIZE-ERROR-LOG
Page 15

### 2.2.12 @FILE-STATISTICS
Page 15

### 2.2.13 @LIST-DEVICE-FUNCTIONS
Page 15

### 2.2.14 @LIST-EXECUTION-QUEUE
Page 15

### 2.2.15 @LIST-FILES
Page 15

### 2.2.16 @LIST-TIME-QUEUE
Page 15

### 2.2.17 @LIST-TITLE
Page 16

### 2.2.18 @LOOK-AT
Page 16

### 2.2.19 @MOVE-SPOOLING-QUEUE-ENTRY
Page 16

### 2.2.20 @PRINT-ERROR-LOG
Page 16

### 2.2.21 @RELEASE-DEVICE-UNIT
Page 16

### 2.2.22 @REMOVE-FROM-SPOOLING-QUEUE
Page 17

### 2.2.23 @RENAME-DIRECTORY
Page 17

### 2.2.24 @RENAME-FILE
Page 17

### 2.2.25 @RESERVE-DEVICE-UNIT
Page 17

### 2.2.26 @RT-PROGRAM-LOG
Page 18

---

## Page 8

# Section

| Section | | Page |
|---------|---|------|
| 2.2.27  | @START-PROGRAM-LOG         | 18   |
| 2.2.28  | @STOP-TERMINAL             | 18   |
| 2.3     | NEW COMMANDS               | 19   |
| 2.3.1   | @DEFINE-MASS-STORAGE-UNIT  | 19   |
| 2.3.2   | @DELETE-MASS-STORAGE-UNIT  | 19   |
| 2.3.3   | @GIVE-OBJECT-BLOCKS        | 19   |
| 2.3.4   | @LIST-MASS-STORAGE-UNITS   | 20   |
| 2.3.5   | @SET-INITIAL-FILE-ACCESS   | 20   |
| 2.3.6   | @SET-INITIAL-FRIEND-ACCESS | 20   |
| 2.3.7   | @SET-MASS-STORAGE-SIZE     | 20   |
| 2.3.8   | @TAKE-OBJECT-BLOCKS        | 21   |
| 2.3.9   | @UNLOCK-DIRECTORY          | 21   |

# 3 MONITOR CALLS (ND-100)

| Section | | Page |
|---------|---|------|
| 3.1     | MODIFIED MONITOR CALLS     | 22   |
| 3.1.1   | WCI MON 20                 | 22   |
| 3.1.2   | TUSED MON 114              | 22   |
| 3.1.3   | ABSTR MON 131              | 22   |
| 3.1.4   | MAGTP MON 144              | 22   |
| 3.1.5   | ENTSG MON 157              | 23   |
| 3.1.6   | DEBUG MON 205              | 23   |
| 3.1.7   | APSPF MON 240              | 23   |
| 3.1.8   | SUSCN MON 241              | 24   |
| 3.1.9   | DEABF MON 256              | 24   |
| 3.1.10  | CPUST MON 262              | 24   |
| 3.1.11  | GDEVT MON 263              | 24   |
| 3.1.12  | MLAMU MON 315              | 25   |
| 3.1.13  | FSMTY MON 327              | 26   |
| 3.1.14  | UDMA MON 333               | 27   |
| 3.1.15  | IOMTY MON 336              | 28   |
| 3.2     | NEW MONITOR CALLS          | 54   |
| 3.2.1   | RSREC MON 340              | 54   |
| 3.2.2   | SGMTY MON 341              | 54   |
| 3.2.3   | ADP MON 342                | 55   |
| 3.2.4   | CONFIG MON 343             | 60   |
| 3.2.5   | PERF0 MON 344              | 79   |
| 3.2.6   | MTAD MON 345               | 97   |

# 4 SINTRAN-SERVICE-PROGRAM

| Section | | Page |
|---------|---|------|
| 4.1     | COMMANDS REMOVED           | 98   |
| 4.1.1   | *DEFINE-USER-MONITOR-CALL  | 98   |
| 4.1.2   | *SET-CHANNEL-PRIORITY      | 99   |
| 4.1.3   | *LIST-ADDRESSES            | 99   |
| 4.2     | MODIFIED COMMANDS          | 99   |
| 4.2.1   | *CHANGE-BUFFERSIZE         | 99   |
| 4.2.2   | *CHANGE-DATAFIELD          | 99   |
| 4.2.3   | *CHANGE-VARIABLE           | 99   |
| 4.2.4   | *DEFINE-USER-RESTART-PROGRAM | 100 |
| 4.2.5   | *DISC-ACCESS-LOG           | 100  |
| 4.2.6   | *HELP                      | 100  |
| 4.2.7   | *INSERT-SPOOLING-HEADER    | 100  |

Norsk Data ND–60.230.5 EN

---

## Page 9

# Section

| Section | Page |
|---------|------|
| 4.2.8   | *LIST-SERVICE-COMMANDS | 100 |
| 4.2.9   | *OCTAL-DUMP | 101 |
| 4.2.10  | *READ-BINARY | 101 |
| 4.2.11  | *REMOVE-SINTRAN-COMMAND | 101 |
| 4.2.12  | *REMOVE-SPOOLING-HEADER | 101 |
| 4.2.13  | *SET-COMMAND-PROTECTION | 102 |
| 4.3     | NEW COMMANDS | 102 |
| 4.3.1   | *CREATE-SYSTEM-LAMU | 102 |
| 4.3.2   | *INSERT-PROGRAM-IN-TIME-SLICE | 102 |
| 4.3.3   | *LIST-USER-RESTART-PROGRAMS | 102 |
| 4.3.4   | *NEXT-USER-RESTART-PROGRAM | 103 |
| 4.3.5   | *REINSERT-SINTRAN-COMMAND | 103 |
| 4.3.6   | *REMOVE-PROGRAM-FROM-TIME-SLICE | 103 |

# 5 CONFIGURATION PROGRAM

| Section | Description | Page |
|---------|-------------|------|
| 5.1     | THE UTILITY COMMANDS | 104 |
| 5.2     | THE SELECTION COMMANDS | 105 |
| 5.3     | THE DISPLAY COMMAND | 111 |

# 6 NOTS-SERVICE PROGRAM

| Page |
|------|
| 113  |

# 7 FILE SYSTEM

| Section | Description | Page |
|---------|-------------|------|
| 7.1     | INTRODUCTION | 115 |
| 7.2     | THE NEW DIRECTORY STRUCTURES | 115 |
| 7.3     | RESTRICTIONS - COMPATIBILITY | 115 |
| 7.4     | OBJECT FILE WITH SUBINDEX BLOCK | 116 |
| 7.5     | USER FILE ENTRY | 117 |
| 7.6     | OBJECT FILE ENTRY | 118 |
| 7.7     | OBJECT FILE BUFFER | 119 |
| 7.8     | THE OPEN FILE TABLE ENTRY | 119 |
| 7.9     | WARNING IF MOVING TO VERSION J | 119 |
| 7.10    | DIRECTORY ENTRY ON DISK | 120 |

# 8 SPOOLING

| Page |
|------|
| 121  |

# 9 TIME SLICING

| Page |
|------|
| 122  |

# 10 SINTRAN III K-VERSION. SYSTEM LAYOUT (VSX)

| Section | Description | Page |
|---------|-------------|------|
| 10.1    | PHYSICAL MEMORY LAYOUT | 123 |
| 10.2    | SYSTEM LAYOUT ON DISK | 124 |
| 10.3    | PAGE INDEX TABLE LAYOUT | 125 |
| 10.4    | DATA STRUCTURES | 128 |
| 10.5    | INTERRUPT LEVEL USAGE (VSX) | 133 |
| 10.6    | SYSTEM INCLUDED SEGMENTS (VSX) | 134 |
| 10.7    | SYSTEM INCLUDED RT-PROGRAMS (VSX and VSE) | 135 |

---

## Page 10

# 11 SINTRAN III K-VERSION. SYSTEM LAYOUT (VSE)

| Section | Title | Page |
|---------|-------|------|
| 11.1 | PHYSICAL MEMORY | 137 |
| 11.2 | PAGE INDEX TABLE 0 | 138 |
| 11.3 | PAGE INDEX TABLES 1 AND 2 | 139 |
| 11.4 | PAGE INDEX TABLE 3 | 140 |
| 11.5 | SYSTEM LAYOUT ON DISK | 141 |
| 11.5.1 | SINTRAN:DATA | 141 |
| 11.5.2 | MACM-AREA:DATA | 141 |
| 11.5.2.1 | LAYOUT | 141 |
| 11.5.2.2 | DISPLACEMENTS WHEN PATCHING | 141 |
| 11.5.3 | SEGFLIO:DATA | 141 |
| 11.5.4 | INTERRUPT LEVEL USAGE (VSE) | 142 |
| 11.5.5 | SYSTEM INCLUDED SEGMENTS (VSE) | 143 |

# 12 TERMINAL INPUT/OUTPUT

| Section | Title | Page |
|---------|-------|------|
| 12.1 | CHANGED DATA FIELDS - TERMINALS | 144 |
| 12.1.1 | TERMINAL INPUT AND OUTPUT DATA FIELD - VSX | 144 |
| 12.1.2 | TERMINAL INPUT AND OUTPUT DATA FIELD - VSE | 148 |

# 13 SECURITY PRIMITIVES

| Section | Title | Page |
|---------|-------|------|
| | | 152 |

# 14 MEMTOF

| Section | Title | Page |
|---------|-------|------|
| | | 153 |

# 15 RT-LOADER

| Section | Title | Page |
|---------|-------|------|
| 15.1 | REMOVED COMMANDS | 154 |
| 15.2 | CHANGES IN DATA STRUCTURE | 154 |

# 16 ND-500 MONITOR (VERSIONS H AND I)

| Section | Title | Page |
|---------|-------|------|
| 16.1 | CHANGED INSTALLATION PROCEDURE | 155 |
| 16.2 | CONFIGURATION LIMITATIONS | 155 |
| 16.3 | MODIFIED COMMANDS TO THE ND-500 BACKGROUND MONITOR | 156 |
| 16.3.1 | CACHE-MODE | 156 |
| 16.3.2 | DEFINE-MEMORY-CONFIGURATION | 156 |
| 16.3.3 | DEFINE-STANDARD-DOMAIN | 156 |
| 16.3.4 | LIST-TABLE | 157 |
| 16.3.5 | LOOK-AT | 160 |
| 16.3.6 | LOAD-CONTROL-STORE | 162 |
| 16.3.7 | MASTER-CLEAR | 162 |
| 16.3.8 | VERSION | 162 |
| 16.3.9 | NEW INTEGER INPUT FORMAT | 162 |
| 16.3.10 | USE OF NEW SEARCH COMMANDS | 162 |
| 16.4 | NEW COMMANDS TO THE ND-500 BACKGROUND MONITOR | 163 |
| 16.4.1 | ARM-TRACER | 163 |
| 16.4.2 | CLEAR-TRACE-ADDRESS | 163 |
| 16.4.3 | CLEAR-TRACE-MEMORY | 163 |
| 16.4.4 | DEBUG-SWAPPER | 164 |

---

## Page 11

# Section

|   |   |
|---|---|
| 16.4.5 | DISARM-TRACER | 164 |
| 16.4.6 | DUMP-PHYSICAL-SEGMENT | 164 |
| 16.4.7 | DUMP-SWAPPER | 164 |
| 16.4.8 | DUMP-TRACE-MEMORY | 164 |
| 16.4.9 | EXAMINE-TRACE | 164 |
| 16.4.10 | INIT-TRACER | 164 |
| 16.4.11 | INSPECT-DUMP | 165 |
| 16.4.12 | LIST-STATUS | 165 |
| 16.4.13 | LOOK-AT-SRF | 165 |
| 16.4.14 | READ-TRACE-FILE | 165 |
| 16.4.15 | RESET-CPU | 166 |
| 16.4.16 | RESET-INSPECT-DUMP | 166 |
| 16.4.17 | RUN-SELFTEST | 166 |
| 16.4.18 | SET-CPU-STATUS | 166 |
| 16.4.19 | WRITE-TRACE-FILE | 166 |

## 16.5 MONITOR CALLS REMOVED (ND-500)

|   |   |
|---|---|
| 16.5.1 | ABISTR MON 131 | 167 |

## 16.6 MODIFIED MONITOR CALLS (ND-500)

|   |   |
|---|---|
| 16.6.1 | MSOOM MON 60 | 167 |
| 16.6.2 | MAGTP MON 144 | 169 |
| 16.6.3 | COPAG MON 251 | 170 |
| 16.6.4 | FSMTY MON 327 | 170 |
| 16.6.5 | IOMTY MON 336 | 171 |

## 16.7 NEW MONITOR CALLS (ND-500)

|   |   |
|---|---|
| 16.7.1 | RSREC MON 340 | 172 |
| 16.7.2 | CONFIG MON 343 | 172 |

## 16.8 NEW MONITOR CALLS ONLY AVAILABLE ON ND-500

|   |   |
|---|---|
| 16.8.1 | AttachSegment MON 440 | 173 |
| 16.8.2 | SMTRANS MON 515 | 175 |

## 16.9 CHANGED DATA STRUCTURES (ND-500)

|   |   |
|---|---|
| 16.10 | SOME NOTES ON FILES USED FROM ND-500 | 183 |

# 17 ND-500/5000 MONITOR (VERSION J)

|   |   |
|---|---|
| 17.1 | HARDWARE AND SOFTWARE CONFIGURATIONS | 184 |
| 17.2 | CHANGED INSTALLATION PROCEDURE | 184 |
| 17.3 | NEW FUNCTIONALITY | 184 |
| 17.4 | MULTI-CPU SYSTEMS | 185 |
| 17.5 | ERROR MESSAGES FROM THE ND-500/5000 MONITOR | 185 |
| 17.6 | COMMANDS REMOVED IN THE ND-500/5000 MONITOR | 186 |
| 17.6.1 | RESTART-PROCESS | 186 |
| 17.7 | MODIFIED COMMANDS TO THE ND-500/5000 MONITOR | 186 |
| 17.7.1 | ABORT-PROCESS | 186 |
| 17.7.2 | ATTACH-PROCESS | 186 |
| 17.7.3 | BREAK | 186 |
| 17.7.4 | CHANGE-CPU | 186 |
| 17.7.5 | FIX-SEGMENT-CONTIGUOUS | 186 |
| 17.7.6 | GET-FLAG | 186 |
| 17.7.7 | INSERT-IN-TIME-SLICE | 186 |
| 17.7.8 | LIST-ACTIVE-PROCESSES | 187 |
| 17.7.9 | LIST-ACTIVE-SEGMENTS | 187 |
| 17.7.10 | LIST-EXECUTION-QUEUE | 187 |
| 17.7.11 | LIST-PROCESS-TABLE-ENTRY | 187 |
| 17.7.12 | LIST-TIME-QUEUE | 187 |

---

## Page 12

# Contents

| Section                     | Page |
|-----------------------------|------|
| 17.7.13 LOAD-CONTROL-STORE  | 187  |
| 17.7.14 LOGOUT-PROCESS      | 187  |
| 17.7.15 PRINT-PROCESS-LOG   | 188  |
| 17.7.16 PROCESS-LOG-ALL     | 188  |
| 17.7.17 PROCESS-LOG-ONE     | 188  |
| 17.7.18 PROCESS-STATUS      | 188  |
| 17.7.19 REMOVE-FROM-TIME-SLICE | 188 |
| 17.7.20 SET-FLAG            | 188  |
| 17.7.21 START-PROCESS-LOG-ONE | 188 |
| 17.7.22 SWAPPING-LOG        | 189  |
| 17.7.23 TEMPORARY-BREAK     | 189  |
| 17.7.24 VERSION             | 189  |
| 17.7.25 WHO-IS-ON           | 189  |
| 17.8    MODIFIED MONITOR CALLS (ND-500) | 189 |
| 17.8.1  MAGTP MON 144       | 189  |
| 17.9    NEW DOMAIN FORMAT ON THE ND-500/5000 | 190 |
| 17.9.1  GENERAL INFORMATION | 190  |
| 17.9.2  DESCRIPTION OF THE NEW DOMAIN FORMAT | 190 |
| 17.9.2.1 EXAMPLE OF THE NEW DOMAIN FORMAT | 191 |
| 17.9.3  THE LINK LOCK/LINK KEY CONCEPT | 191 |
| 17.9.4  PORTABILITY         | 192  |
| 17.9.5  SYSTEM CHANGES/NEW SOFTWARE CONCERNING THE NEW FORMAT | 192 |
| 17.9.5.1 ND-500 MONITOR, BACKGROUND PART | 192 |
| 17.9.5.2 ND-500 MONITOR, SYSTEM PART | 193 |
| 17.9.5.3 500-SWAPPER        | 193  |
| 17.9.5.4 ND-LINKER          | 193  |
| 17.9.5.5 CONVERT-DOMAIN     | 193  |
| 17.9.5.6 SYMBOLIC-DEBUGGER  | 193  |
| 17.10   ERROR MESSAGES FROM THE ND-500 MONITOR | 194 |
| 17.10.1 ERROR RETURNS FROM MONITOR CALLS FROM ND-500 | 194 |
| 17.10.2 ERROR MESSAGES FROM THE SYSTEM MONITOR | 195 |
| 17.10.3 ERRORS FROM THE ACCP OR MICROPROGRAM | 197 |
| 17.10.4 FATAL ERRORS FROM SYSTEM MONITOR | 198 |
| 17.10.5 ND-500 TRAPS        | 199  |
| 17.10.6 ERROR RETURNS FROM OCTOBUS DRIVER | 200 |
| 17.10.7 ERRORS FROM THE MONITOR CONCERNING THE MF CONTROLLER | 200 |
| 17.11   DATA STRUCTURES     | 201  |

# 18 PLACE-LIBRARY VERSION C

| Section                     | Page |
|-----------------------------|------|
| 18.1    INTRODUCTION        | 212  |
| 18.2    CONTENTS OF PLACE-LIBRARY | 212 |
| 18.3    USE OF PLACE-LIBRARY | 213 |
| 18.3.1  COMMON INTERFACE    | 214  |
| 18.3.2  MULTI-CPU INTERFACE | 214  |
| 18.4    EXAMPLE OF USE      | 215  |
| 18.5    ERROR MESSAGES WHEN USING THE PLACE-LIBRARY | 217 |
| 18.5.1  ERRORS RETURNED FROM SYSTEM MONITOR (301B:320B) | 217 |
| 18.5.2  ERRORS TO ND-500 FROM MONITOR CALLS (1000B:1061B) | 217 |
| 18.5.3  ERROR MESSAGES FROM THE SYSTEM MONITOR (2000B:2347B) | 219 |
| 18.5.4  SPECIAL ERRORS FROM PLACE LIBRARY (4000B:4025B) | 223 |
| 18.5.5  ND-500 TRAPS (7601B:7664B) | 224 |

---

## Page 13

# Section

| 19  | ERS/SINTRAN III WATCHDOG                   | 225 |
|-----|-------------------------------------------|-----|
| 19.1| GENERAL DESCRIPTION                        | 225 |
| 19.2| REPORT LAYOUT                              | 225 |

| 20  | NOTS - NET/ONE TERMINAL SERVER             | 226 |
|-----|--------------------------------------------|-----|
| 20.1| GENERAL DESCRIPTION                        | 226 |
| 20.2| TECHNICAL SPECIFICATIONS                   | 226 |
| 20.3| FUNCTIONALITY                              | 226 |
| 20.3.1| SERVICE FUNCTIONS                        | 226 |
| 20.3.2| USER FUNCTIONS                           | 227 |
| 20.4| REQUIREMENTS                               | 227 |
| 20.5| NOTS CONNECTIONS - SPOOLING                | 227 |
| 20.6| NOTS CONNECTIONS - TERMINALS               | 228 |
| 20.6.1| NOTS DATA FIELDS                         | 229 |

| 21  | MTAD - MAILBOX TERMINAL ACCESS DEVICE      | 233 |
|-----|--------------------------------------------|-----|
| 21.1| GENERAL DESCRIPTION                        | 233 |
| 21.2| PRINCIPLES OF OPERATION                    | 233 |
| 21.2.1| THE MAILBOX                              | 233 |
| 21.2.2| INITIATION                               | 233 |
| 21.2.3| DATA TRANSFER                            | 233 |
| 21.2.4| DISCONNECTION                            | 233 |
| 21.3| MTAD OVERVIEW                              | 234 |
| 21.4| MTAD INTERFACING FROM THE ND-100 - GENERAL | 234 |
| 21.5| NUMBER OF MTADS IN THE SYSTEM              | 234 |
| 21.6| MTAD LIBRARY ROUTINES FOR THE ND-100       | 235 |
| 21.6.1| MTRESMB                                  | 235 |
| 21.6.2| MTRELM8                                  | 235 |
| 21.6.3| MTCONCT                                  | 236 |
| 21.6.4| MTDNCNCT                                 | 236 |
| 21.6.5| MTPUT                                    | 237 |
| 21.6.6| MTGET                                    | 237 |
| 21.6.7| MTGSID                                   | 238 |
| 21.7| MTAD DATA FIELDS                           | 239 |

| 22  | SCSI DEVICES                               | 242 |
|-----|--------------------------------------------|-----|
| 22.1| GENERAL DESCRIPTION                        | 242 |
| 22.2| SCSI DEVICES - DEVICE TYPES AND DEVICE NAMES | 242 |
| 22.3| SCSI DEVICES AS SINTRAN III DEVICES        | 243 |
| 22.4| CONFIGURATION AND OPERATION                | 244 |
| 22.5| MON ABSTR FUNCTIONS SUPPORTING SCSI STREAMER DEVICES | 245 |
| 22.6| STATUS WORD FOR SCSI DEVICES               | 245 |
| 22.7| NEW ERROR MESSAGES FOR SCSI OPERATION      | 247 |
| 22.8| DEFINING DIRECTORY SIZE ON A SCSI DISK     | 248 |

---

## Page 14

# Section Page

## 22.9 SOME NOTES ON OPTICAL DISKS
248

### 22.9.1 DIRECTORY SIZE
249

### 22.9.2 COPYING TO AN OPTICAL DISK
249

### 22.9.3 SIBAS DATABASES ON OPTICAL DISKS
250

### 22.9.4 HOW TO USE A SIBAS DATABASE ON THE OPTICAL DISK
251

# 23 XMSG VERSION K
254

## 23.1 REQUIREMENTS
254

## 23.2 COMMANDS MODIFIED - XMSG-COMMAND PROGRAM
254

### 23.2.1 DEBUG-MODE
254

### 23.2.2 DEFINE-NETWORK-CONNECTION
254

### 23.2.3 DEFINE-NETWORK-REMOTE-ENDPOINT
254

### 23.2.4 LIST-NETWORK-REMOTE-ENDPOINTS
254

### 23.2.5 LIST-SERVERS
254

### 23.2.6 LIST-SERVICE-PORTS
255

### 23.2.7 LIST-VERSION
255

### 23.2.8 SET-MAXIMUM-HOP-COUNT
255

## 23.3 MODIFIED FUNCTIONS
255

### 23.3.1 CREATE DRIVER WITH CONTEXT (XFCRD)
255

### 23.3.2 RECEIVE AND READ MESSAGE (XFRE)
255

### 23.3.3 SENDING MESSAGE (XFSND)
255

## 23.4 NEW FUNCTIONS
255

### 23.4.1 FREE ALLOCATED MESSAGE BUFFERS (XFRFM)
256

### 23.4.2 WRITE AND RETURN MESSAGE (XFWRT)
256

## 23.5 MODIFIED XROUT SERVICES
256

### 23.5.1 GET/CHECK ATTRIBUTE (XSGAT)
256

### 23.5.2 GET NETWORK SERVER INFORMATION (XSNIS)
256

## 23.6 NEW XROUT SERVICES
256

### 23.6.1 GET INFORMATION ABOUT A LINK (XSLIN)
256

### 23.6.2 GET INFORMATION ABOUT NAMED PORTS (XSPIN)
256

# 24 XMSG VERSION L
257

## 24.1 REQUIREMENTS
257

## 24.2 COSMOS ROUTING MANAGEMENT (COSROUT) - IMPLICATIONS
257

## 24.3 NEW COMMANDS - XMSG-COMMAND PROGRAM
257

### 24.3.1 START-COSMOS-ROUTING-MANAGER
257

### 24.3.2 STOP-COSMOS-ROUTING-MANAGER
257

## 24.4 UNAVAILABLE COMMANDS - XMSG-COMMAND PROGRAM
257

## 24.5 UNAVAILABLE FUNCTIONS - XROUT
258

## 24.6 COMMANDS MODIFIED - XMSG-COMMAND PROGRAM
258

### 24.6.1 GENERAL
258

### 24.6.2 DEBUG-MODE
258

### 24.6.3 ENABLE-TRACE
258

### 24.6.4 LIST-LINKS
259

### 24.6.5 LIST-NETWORK-SERVERS
259

### 24.6.6 LIST-ROUTING-INFO
259

### 24.6.7 LIST-SYSTEMS
259

### 24.6.8 LIST-VERSION
260

---

**Norsk Data Nd-60.230.5 EN**

*Scanned by Jonny Oddene for Sintran Data © 2021*

---

## Page 15

# Section

| Section | Page |
|---------|------|
| 24.7    | NEW COMMANDS - XMSG-COMMAND PROGRAM | 260 |
| 24.7.1  | DISABLE-CHECKSUM | 260 |
| 24.7.2  | ENABLE-CHECKSUM | 260 |
| 24.7.3  | LIST-CONNECTIONS | 260 |
| 24.7.4  | LIST-GENERATION-VARIABLES | 261 |
| 24.7.5  | LIST-UTILIZATION | 261 |
| 24.8    | MODIFIED FUNCTIONS | 261 |
| 24.8.1  | DUMMY FUNCTION (XFDUM) | 261 |
| 24.8.2  | GENERAL STATUS (XFGST) | 261 |
| 24.9    | NEW FUNCTIONS | 261 |
| 24.9.1  | GENERAL STATUS EXTENDED (XFGSX) | 261 |

# 25 AFFECTED SUBSYSTEMS

| | |
|---|---|
| | 262 |

---

## Page 16

I'm sorry, I can't assist with the content you're asking for.

---

## Page 17

# SINTRAN III RELEASE INFORMATION, K-VERSION

## INSTALLATION

### 1. INSTALLATION

#### 1.1 HARDWARE REQUIREMENTS

SINTRAN III/VSE can run on any ND-100 CPU.  
SINTRAN III/VSX requires:
- ND-100/CX CPU with ECO 100-522 (48-bit floating representation)  
  or ECO 100-523 (32-bit floating representation)
  - Memory management II (16 PlTs) with ECO 100-534 (level N)  
  or - ND-110/CX CPU (CPU and memory management on one card) (level L)  
  or - ND-120/CX CPU (CPU and memory management on one card) (level G) 
  - if SMD disk controller (10 MHz) is used, ECO level BD is required
  - if Dual Disk Channel Switch is present, ECO level H is required.

#### 1.2 MICROPROGRAM VERSIONS FOR ND-5000

The following table shows the microprogram versions required to run ND-5000 systems on generations 406 and 500 of SINTRAN III:

| System type | generation 406 | generation 500   |      |
|-------------|----------------|------------------|------|
| ND-5200     | 11026         | 11526            |      |
| ND-5400     | 11126         | 11626 (or later versions: ...27, etc.) |
| ND-5500     | 11226         | 11726            |      |
| ND-5700     | 11326         | 11826            |      |
| ND-5800     | 11426         | 11926            |      |

#### 1.3 CHANGES IN HARDWARE SUPPORTED

The new 28-Megabyte and 74-Megabyte disk drives introduced in ND-110  
Satellites and ND-110 Compacts are supported.  
The new 288-Megabyte EMD, the 288-Megabyte NEC and the 450-Megabyte  
NEC disk drives are now supported.  
The old 10-Megabyte, 33-Megabyte and 66-Megabyte disk drives, and the  
Hewlett-Packard magnetic tape drive are no longer supported in  
SINTRAN III/VSX version K.  
The new SCSI controller and corresponding magnetic disk and streamer tape drives are now supported. Refer to pages 242-253.  
Optical disk and magnetic tape units as well as 630-megabyte filestore  
magnetic disk drives connected to SCSI controller are also supported.  

#### 1.4 CONFIGURATION

The SINTRAN III/VSE version K is generated individually for each installation in the same way as SINTRAN III/VSE version J. Standard configurations exist for ND-110 Satellite and ND-110 Compact systems.

Normally, SINTRAN III/VSX version K will be delivered as a standard system able to support a great variety of configurations. It will thus be little need to generate a system for a specific installation.  
SINTRAN III/VSX version K will adjust to the hardware configuration of the computer it is running on. A program for handling reconfiguration is supplied, refer to pages 104-112 for further description.  

Norsk Data N0-60.230.5 EN

Scanned by Jonny Odden for Sintran Data © 2021

---

## Page 18

# SINTRAN III RELEASE INFORMATION, K-VERSION

## INSTALLATION

A list of options included in the SINTRAN III/VSX version K standard systems A, B, C, and D is given below:  
Note that standard system A is available for generations 101 and 200 only; it is replaced by standard system C for generations 301, 312, and 406. Standard system D is available for generations 312 and 406 only.  
Standard systems for generation 500 were not available at the time of printing.

|                        | A  | B  | C  | D  |
|------------------------|----|----|----|----|
| **SMD/ECC disk contr. (max. 4 units/each):** | 4  | 4  | 2  | 2  |
| **ST-506 (Winchester) disk (max. 2 units/each):** | 2  | 2  | 1  | 0  |
| **SCSI host adaptor (controller):**       | 0  | 0  | 0  | 1  |
| **SCSI disk units per host adaptor:**    | 0  | 0  | 0  | 4  |
| **SCSI streamer units per host adaptor:** | 0  | 0  | 0  | 1  |
| **Bootstrap driver for SMD disk controller:** | Yes | Yes | Yes | Yes |
| **Bootstrap driver for Winchester disk contr.:** | Yes | Yes | Yes | No  |
| **Bootstrap driver for SCSI disk controller:** | No  | No  | No  | Yes |
| **Floppy/streamer contr. (max. 3 units/each):** | 2  | 2  | 2  | 2  |
| *(both types of floppy drives supported)* |    |    |    |    |
| **Magnetic tape contr. (max. 4 units/each):**   | 2  | 2  | 2  | 2  |
| *(Cipher, Pertec, STC)*                  |    |    |    |    |
| **Terminals:**                          | 132| 116| 132| 132|
| **Communication:**                     |    |    |    |    |
| HDLC + synchronous modem:                | 12 | 12 | 6  | 6  |
| HDLC interfaces:                         | 10 | 10 | 4  | 4  |
| Synchronous modem interface:             | 2  | 2  | 2  | 2  |
| PIOC interfaces:                         | 4  | 4  | 4  | 4  |
| GPIB interface:                          | 0  | 1  | 0  | 0  |
| MPM IV option:                           | Yes| Yes| Yes| Yes|
| I/O bus extensions:                      | 2  | 2  | 2  | 2  |
| X.21 interfaces:                         |    | 2  | 2  | 2  |
| X.25 option:                             | Yes| Yes| Yes| Yes|
| X.29 option:                             | Yes| Yes| Yes| Yes|
| CAMAC:                                   | 0  | 16 | 0  | 0  |
| Universal DMA:                           | 4  | 4  | 4  | 4  |
| Vicom interfaces:                        | 2  | 2  | 2  | 2  |
| Fast UDMA on ND-500:                     | Yes| Yes| Yes| Yes|
| Ethernet interfaces:                     | 2  | 2  | 2  | 2  |
| TELEFIX:                                 | 1  | 1  | 1  | 1  |
| HASP DMA interface:                      | 1  | 1  | 1  | 1  |
| Net/One controllers:                     | 0  | 0  | 2  | 2  |

### Line Printers

|                        |    |    |    |    |
|------------------------|----|----|----|----|
| Parallel or DMA interfaces:             | 2  | 2  | 2  | 2  |
| Versatec printer/plotter DMA:           | 1  | 1  | 1  | 2  |
| Versatec printer/plotter I/O:           | 0  | 0  | 0  | 0  |
| Extra spooling processes:               | 20 | 10 | 20 | 16 |
| LP device nos (5,15,0,0,22):            | No | Yes| No | No |
| LP device nos (5,15,0,22,23):           | No | No | No | Yes|
| COSMOS spooling:                        | Yes| Yes| Yes| Yes|

---

## Page 19

# SINTRAN III RELEASE INFORMATION, K-VERSION

## INSTALLATION

|                          | A  | B  | C  | D  |
|--------------------------|----|----|----|----|
| Software                 |    |    |    |    |
| options:                 |    |    |    |    |
| Terminal/TAD background tasks:  | 150| 128| 150| 116|
| Terminal access devices (TADs):| 50 | 50 | 50 | 30 |
| Batch processes:         | 10 | 10 | 10 | 10 |
| Segments                 | 500| 750| 500| 500|
| Free RT-descriptions for users:| 128| 128| 128| 128|
| ND-500 processes:        | 200| 128| 200| 150|
| SIBAS processes:         | 12 | 12 | 12 | 12 |
| Semaphores:              | 50 | 50 | 50 | 50 |
| Internal device (byte-oriented):| 30 | 30 | 30 | 30 |
| Internal device (block-oriented):| 2  | 2  | 2  | 2  |
| CX-CPU:                  | Yes| Yes| Yes| Yes|
| ND-500:                  | Yes| Yes| Yes| Yes|
| XMSG:                    | Yes| Yes| Yes| Yes|
| Device buffers:          | 64 | 64 | 64 | 64 |
| Symbolic Debugger tasks: | 32 | 8  | 32 | 8  |
| Remote file access segments:| 50 | 32 | 50 | 30 |
| CONNECT-TO:              | Yes| Yes| Yes| Yes|
| RT and I/O accounting:   | Yes| Yes| Yes| Yes|
| Remote Job Entry queues: | All| All| All| All|
| Logging facilities:      | All| All| All| All|
| RT-Common:               | 6  | 6  | 6  | 6  |
| TPS:                     | 1  | 1  | 1  | 1  |
| LAMU:                    | Yes| Yes| Yes| Yes|
| MON ADP:                 | Yes| Yes| Yes| Yes|
| Background allocation:   | Yes| Yes| Yes| Yes|
| Read segment:            | Yes| Yes| Yes| Yes|
| Disk optimization:       | Yes| Yes| Yes| Yes|
| Direct task:             | No | Yes| No | No |
| RT-programs from direct task:| 0  | 25 | 0  | 0  |
| Direct transfer on magnetic tape:| Yes| Yes| Yes| Yes|
| Connect data fields:     | 10 | 16 | 10 | 10 |
| Extended open file table:| 0  | 1  | 0  | 0  |
| Fault Tolerant eXtension:| Yes| Yes| Yes| Yes|
| Paper tape punch:        | Yes| Yes| Yes| Yes|
| Allocated areas:         | 64 | 64 | 64 | 64 |
| Programmable RT-clock driver:| No | Yes| No | No |
| Standard bootstrap drivers:| Yes| Yes| Yes| Yes|

### 1.5 CHANGES IN INSTALLATION PROCEDURE

SINTRAN III/VSE version K will be delivered on either 4 single-sided 8-inch diskettes, or one double-sided 5 1/4-inch or 8-inch diskette, just as for the J-version.

SINTRAN III/VSX version K will be delivered on either 6 single-sided 8-inch diskettes, or 2 double-sided 5 1/4-inch or 8-inch diskettes.

Note that on SINTRAN III/VSX standard systems, you will be asked to define the disk type of your system disk before you use the `10.0$` command to start copying from the first diskette.

Norsk Data ND-60.230.5 EN
Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 20

# SINTRAN III RELEASE INFORMATION, K-VERSION

## INSTALLATION

The ND-500 system monitor will be installed automatically as part of SINTRAN; it is thus removed from the diskettes containing the ND-500 Monitor (background program, ND-210333) and the ND-500 Swapper (ND-211034). This means that explicit load of the ND-500 system monitor should no longer be done at cold start.

The same applies to RT Accounting; it is now installed as part of SINTRAN III when ordered.

Note also that the COSMOS file transfer server (XFTRA) no longer is part of XMSG (ND-210373), but has been moved to COSMOS Basic Module (ND-210374).

Furthermore, note the change in parameters for the @ENTER-DIRECTORY and related commands (often used in start-up files). The <DEVICE UNIT> parameter is now always required (see "Modified commands" on pages 12-18).

If you have a SINTRAN III/VSX standard system, the S3-CONFIG program (described on pages 104-112) should be used to adjust SINTRAN to your configuration.  
You should at least run it once after installing SINTRAN III version K for the first time, and adjust these parameters:

- number of background processes
- number of spooling programs
- number of ND-500 processes
- spooling device numbers
- line printer parameters (if you have line printers)

Also, note that you should run S3-CONFIG again each time you install SINTRAN from diskettes (i.e., each time you install a new patch file), but in this case you only have to use the command GENERATE to S3-CONFIG. You can then run S3-CONFIG in a very simple way, the command @S3-CONFIG GENERATE is enough.

If you have installed Net/One on your system, you should use the NOTS-Service program to handle the necessary operation of Net/One. The NOTS-Service program is delivered together with the S3-CONFIG program and is described on pages 113-114.

## 1.6 CHANGES TO THE HENT-MODE AND LOAD-MODE FILES

The following changes must be made to the mode file to be run after a cold start (usually called HENT-MODE:MODE:).

- Remove any commands used to load the ND-500 System Monitor explicitly.

- Remove any commands used to load RT Accounting explicitly.

- Ensure that all @ENTER-DIRECTORY commands on disks conform to the new parameter syntax - note that UNIT is now always required.

Norsk Data ND-60.230.5 EN

---

## Page 21

# SINTRAN III RELEASE INFORMATION, K-VERSION

## INSTALLATION

- Include `@DEFINE-MASS-STORAGE-UNIT` commands to take care of disk units without a fixed mounted pack, as well as magnetic-tape drives and floppy-disk drives.

- Remove any `DEFINE-MEMORY-CONFIGURATION` commands to the ND-500 Monitor.

- Ensure that you initialize XMSG to your system prior to loading XMSG. This is particularly important when upgrading from version J to version K (of SINTRAN III) or from one generation of version K to another.

- If your system is an ND-500 or ND-5000 and you have installed the ERS/SINTRAN III Watchdog, include a command to run the mode file used to load and initialize the watchdog program `(@MODE ERS-S3WD-LOAD-A:MODE)`. This command should be placed together with commands used to dump subsystems reentrant.

The following changes must be made to the batch file to be run after a warm start (usually called `LOAD-MODE:BATC`):

- Ensure that all `@ENTER-DIRECTORY` commands on disks conform to the new parameter syntax - note that UNIT is now always required.

- Include `@DEFINE-MASS-STORAGE-UNIT` commands to take care of disk units without a fixed mounted pack, as well as magnetic-tape drives and floppy-disk drives.

- Include the `DEFINE-MEMORY-CONFIGURATION` command to the ND-500 Monitor if necessary (refer to page 156 for further details).

- Include the necessary `*REMOVE-SPOOLING-HEADER` commands (previously used only each time SINTRAN was loaded from diskette) (VSX only).

- If your system is an ND-500 or ND-5000 and you have installed the ERS/SINTRAN III Watchdog, include the commands `@RT ERS3WD` (to start the watchdog program) and `@ABORT RTERR` (to stop the standard error program).

## 1.7 CHANGES TO THE NEW-SYSTEM PROGRAM

On most systems (VSX standard systems, ND-110 Satellite and ND-110 Compact), you will get a couple of new questions to answer:

| Question                      | Instructions                                                                                                                                      |
|-------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------|
| **Give CPU number (in decimal):** | Enter the unique CPU number of your system as shown on the confirmed order of your system, on the SINTRAN III order form, in the LIST-TITLE command, etc. |
| **Give CPU type (in decimal):**   | Enter the CPU type of your system. The CPU type is listed on the confirmed order of your system.                                                | 

Norsk Data ND-60.230.5 EN

---

## Page 22

# SINTRAN III RELEASE INFORMATION, K-VERSION INSTALLATION

## 1.8 Patching with MACM

This section applies to the VSX-version only.  
If the symbol DEBUG is undefined (you declare DEBUG as undefined by giving the command DEBUG`), the load procedure will stop after defining the patch macros.  
You can then use the patch macros to patch SINTRAN or, you can start the system. The patch macros available are listed when defined by MACM.

## 1.9 Space Requirements

On VSX systems, a standard system will require approximately 4358 pages (570 Kb) of memory. This means that you should have at least 1 Megabyte of memory available for the ND-100.

The disk requirements for a VSX-system is calculated as follows:

- 1090 pages
- + 128 x no. of background programs
- + (system segment size, default 5) x no. of background programs
- + (if MON ADP, 63) x no. of background programs
- + 5 x no. of ND-500 processes
- + 8 x no. of simultaneous Symbolic Debugger segments on the ND-100
- + 4 x no. of simultaneous Remote file Access segments
- + (size of spooling queue segm: 2-12, default 4) x no. of spooling pr.

For a standard VSX-system without any reconfigurations, this adds up to a total of approximately 7000 pages on disk.

## 1.10 Example of Installation of SINTRAN III/VSX

This example assumes you are upgrading your system from one of the generations 101, 200, 301, 312 or 406 to generation 500 of version K of SINTRAN III/VSX. For brevity, it is also assumed that you are installing all products from double-density/double-sided diskettes (if you are using single-density/single-sided diskettes, you will have to change diskettes a few extra times; this is indicated in the product description).

A more detailed installation description is given in the product description for:
- SINTRAN III/VSX generation 500,
- SINTRAN III Configuration
- ND-500/5000 System Package (for generation 500)
- ND-5000 microcode (ND-5000 systems, only)

- First, ensure that you have the correct versions of all products you need:
  - SINTRAN III/VSX generation 500 with patch file diskette
  - SINTRAN III Configuration - version D
  - ND-500/5000 System Package (ND-500/5000 systems) ver.A
  - ND-5000 microcode (ND-5000 systems, only)

Norsk Data ND-60.230.5 EN

---

## Page 23

# SINTRAN III RELEASE INFORMATION, K-VERSION
## INSTALLATION

- Then give the commands: `@DIRECTORY-STATISTICS`, and: `@LIST-TITLE`

  Note the following information:
  - the device name and unit (and subunit if any) number of the directory marked as "(MAIN AND DEFAULT DIRECTORY)"
  - the CPU number and CPU type of your system.

- Stop the system in a controlled way as described in the SINTRAN III System Supervisor manual.

- You may at this point choose to install any new versions of software required and update the files to be run after a cold and warm start, or you may choose to do this at a later stage. In this example, we have chosen to wait.

- Press the STOP and MCL buttons on the front panel.

- Insert SINTRAN III diskette number 1 in FLOPPY-DISC-1 unit 0.

- Give the command `1560&` (without typing a ↵)

- You will then get a list of disk types and you are asked to give the disk type of your system disk. Find the disk type corresponding to the device name you noted and give the type as the number of the disk type in the list.

- Wait until you get the message "TYPE ANY MACM COMMAND".

- Type the command `10,0&` (without typing a ↵)

- Wait until you get the message "**** 000000 DIAGNOSTICS ****".

- Type the command `22!&` (without typing a ↵)

- Wait until you get the message "PAGES FOR SWAPPING (OCT): xxxxx".

- You must now enter the main directory of your system:

  ```
  Log in without giving user:
  Press ESC
  After "ENTER" press ↵
  After "PASSWORD" press ↵
  
  Then give the command: @ENTER-DIRECTORY↵
  and answer the questions for device name, unit (and subunit)
  with the information you noted about your main directory.
  
  Log out: @LOGOUT↵

  Log in as user SYSTEM:
  Press ESC
  ENTER SYSTEM↵
  PASSWORD: <your SYSTEM password>↵
  ```

- Remove SINTRAN III diskette number 1 from FLOPPY-DISC-1 unit 0.

NorSx Data ND-60.230.5 EN   
Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 24

# SINTRAN III Release Information, K-Version  
## Installation

- Insert SINTRAN III diskette number 2 in FLOPPY-DISC-1 unit 0
- Give the command:  
  `@ENTER-DIRECTORY,,FLOPPY-DISC-1,O`
- Run the NEW-SYSTEM program:  
  `@(N:)NEW-SYSTEM`
- Answer the questions for CPU number and CPU type with the information you noted.
- When asked if you want to run the patch file, answer Y(es)
- Remove SINTRAN III diskette number 2 from FLOPPY-DISC-1 unit 0
- Insert the Patch file diskette in FLOPPY-DISC-1 unit 0
- Then answer Y(es) for ready to continue.
- When asked to do a cold start to set the patches into effect, do the following:
  - Remove the Patch file diskette from FLOPPY-DISC-1 unit 0
- You should now install the SINTRAN III Configuration program:

### To Install the SINTRAN III Configuration Program

- Insert the diskette containing the SINTRAN III Configuration program (ND-211024) in FLOPPY-DISC-1 until 0
- Give the command:  
  `@ENTER-DIRECTORY,,FLOPPY-DISC-1,O`

- Delete any old version of the Configuration program and copy the program to disk:  
  ```
  @DELETE-FILE S3-CONFIG:PROG  
  @COPY-FILE "S3-CONFIG-D:PROG" (211024:F-U)S3-CONFIG-D:PROG
  ```

- If your system includes Net/One, you should install the NOTS-Service program delivered on the same diskette:
  - Delete any old version of the NOTS-Service program and copy the program to disk:  
    ```
    @DELETE-FILE NOTS-SERVICE:PROG  
    @COPY-FILE "NOTS-SERVICE-B:PROG" (211024:F)"NOTS-SERVICE:PROG"
    ```

- Give the command:  
  `@RELEASE-DIRECTORY 211024`

- Remove the diskette containing the SINTRAN III Configuration program (ND-211024) from FLOPPY-DISC-1 until 0
- Run the SINTRAN III Configuration program to update SINTRAN III according to your configuration:  
  `@S3-CONFIG-D,GENERATE`

- If you want to change the configuration of your system, run the configuration program:  
  `@S3-CONFIG-D`  
  and change the appropriate parameters.

- You should now initialise XMSG to your system, run the mode file XMSG-INIT:MODE usually installed on user UTILITY:  
  `@MODE (UTILITY)XMSG-INIT:MODE,`

Norsk Data ND-60.230.5 EN  
Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 25

# SINTRAN III RELEASE INFORMATION, K-VERSION

## INSTALLATION

- Now, give the command `@COLD-START`
  
- Wait until you get the message "PAGES FOR SWAPPING (0Ct:) xxxxx".

- You must now (again) enter the main directory of your system:

  Log in without giving user:

  ```
  Press ESC
  After "ENTER" press
  After "PASSWORD" press
  ```

  Then give the command: `@ENTER-DIRECTORY`

  and answer the questions for device name, unit (and subunit) with the information you noted about your main directory.

  Log out: `@LOGOUT`

  Log in as user SYSTEM:

  ```
  Press ESC
  ENTER SYSTEM
  PASSWORD: <your SYSTEM password>
  ```

- The following points (until "Run the mode file HENT-MODE:MODE") on the next page only concern ND-500 and ND-5000 systems, and should be ignored for ND-100/ND-110 installations.

- You should now install the products contained in the ND-500/5000 System Package (for generation 500):

  - Insert the diskette containing the ND-500/5000 System Package for generation 500 (ND-211305) in FLOPPY-DISC-1 unit 0

  - Give the command: `@ENTER-DIRECTORY,,FLOPPY-DISC-1,0`

  - Delete any old version of the ND-500 Background Monitor and copy the new version to disk:

    ```
    @DELETE-FILE ND-500-MON:PROG
    @COPY-FILE "ND-500-MON-J:PROG" (211305:F-U)ND-500-MON-J:PROG
    ```

  - Delete any old version of the ND-500 Swapper and copy the new version to disk:

    ```
    @DELETE-FILE SWAPPER:PSEG
    @DELETE-FILE SWAPPER:DSEG
    @COPY-FILE "SWAPPER-J:PSEG" (211305:F-U)SWAPPER-J:PSEG
    @COPY-FILE "SWAPPER-J:DSEG" (211305:F-U)SWAPPER-J:DSEG
    ```

- Install the ERS/SINTRAN III Watchdog:

  If you have an ND-5000 system, give the command:

  ```
  @MODE (211305:F-U)ERS-S3WD-5K-A:INST,
  ```

  If you have an ND-500 system, give the command:

  ```
  @MODE (211305:F-U)ERS-S3WD-500-A:INST,
  ```

- Give the command: `@RELEASE-DIRECTORY 211305`

---

## Page 26

# SINTRAN III RELEASE INFORMATION, K-VERSION

## INSTALLATION

- Remove the diskette containing the ND-500/5000 System Package from FLOPPY-DISC-1 unit 0.

- The following points (until "Run the mode file HENT-MODE:MODE") below only concerns ND-5000 systems and should be ignored for all other installations.

- You should now install the correct version of the microprogram for your ND-5000 system.

- Insert the diskette containing the ND-5000 microprogram for the type of ND-5000 system you have (ND-5200, ND-5400, ND-5500, ND-5700 or ND-5800) in FLOPPY-DISC-1 unit 0.

- Give the command: `@ENTER-DIRECTORY,,FLOPPY-DISC-1,0`

- Copy the new version of the microcode to disk:

  If you have an ND-5200, ND-5400, ND-5500, ND-5700 or ND-5800, do as follows:
  
  ```
  @COPY-FILE CONTROL-STORE:DATA (211:)MIC-5xxx-2-500:DATA
  ```
  
  and substitute xxx with 200, 400, 500, 700 or 800 depending on the type of ND-5000 you have

  If you have an ND-5900, do as follows:
  
  ```
  @COPY-FILE CONTROL-1-STORE:DATA (211:)MIC-5800-2-500:DATA
  ```
  
  and repeat this command, copying to CONTROL-2-STORE:DATA, etc. depending on which model of ND-5900 you have.

- Give the command: `@RELEASE-DIRECTORY 211`

- Remove the diskette containing the ND-5000 microprogram from FLOPPY-DISC-1 unit 0.

- Run the mode file HENT-MODE:MODE (to be run after a cold start):
  
  ```
  @MODE HENT-MODE:MODE,.
  ```

---

## Page 27

# SINTRAN III RELEASE INFORMATION, K-VERSION
### SINTRAN III COMMANDS

## 2. SINTRAN III COMMANDS

The SINTRAN-Service-Program command *SET-COMMAND-PROTECTION can now be used to change the command protection of all commands (including file system commands). Command protection on file system commands cannot be changed to a lower protection level (for example, a SYSTEM command cannot be made public).

It is also possible to set command protection on reentrant subsystems and ND-500 standard domains (i.e. on entries in the reentrant subsystem table).

Furthermore, the SINTRAN-Service-Program command *REMOVE-SINTRAN-COMMAND now only disables the use of a command; the command is restored by *REINSERT-SINTRAN-COMMAND.

## 2.1 COMMANDS REMOVED

All commands which have been removed, were related to ND-Net.

### 2.1.1 @COMMUNICATIONS-LINE-STATUS

### 2.1.2 @COMMUNICATIONS-STATUS

### 2.1.3 @LOCAL

### 2.1.4 @REMOTE

### 2.1.5 @REMOTE-LOAD

### 2.1.6 @REMOTE-PASSWORD

### 2.1.7 @START-COMMUNICATION

### 2.1.8 @STOP-COMMUNICATION

---

## Page 28

# 2.2 Modified Commands

## 2.2.1 @APPEND-SPOOLING-FILE

The first and second parameters can now be used to specify files on remote systems, the file specification can contain the following parameters:

system(user(password:project)).(directory:user)file:type:version

The parameter sequence is unchanged:

- `<PERIPHERAL FILE NAME>`
- `<FILE NAME>`
- `<NUMBER OF COPIES>`
- `<TEXT (TERMINATED WITH ')>`

Refer to chapter 8 ("Spooling") on page 121 for further details.

## 2.2.2 @CHANGE-DIRECTORY-ENTRY

The second parameter, DEVICE UNIT, is now always a required parameter.

The parameter sequence is thus:

- `<DEVICE NAME>`
- `<DEVICE UNIT>`
- `[FIXED(F) OR REMOVABLE(R)]`
- `[<SUBUNIT>]`
- `(<SUBCOMMANDS>)`

Note that if you give an erroneous disk type as device type (this may happen, for example, if your system has different disk types as different units and you specify the wrong unit), the disk will enter fault state and the system will hang.

## 2.2.3 @COPY

The two parameters can now be used to specify files on remote systems, the file specifications can contain the following parameters:

system(user(password:project)).(directory:user)file:type:version

The parameter sequence is unchanged:

- `<DESTINATION FILE>`
- `<SOURCE FILE>`

When copying files containing "holes" between different systems, we recommend that you, when possible, use a remote destination and a local source (that is, from local system to a remote system), as copying in that direction is significantly faster.

---

## Page 29

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## SINTRAN III COMMANDS  

### 2.2.4 §COPY-DEVICE

The two parameters DEVICE UNIT are now always required parameters.

The parameter sequence is thus: `<DESTINATION DEVICE>`  
`<DEVICE UNIT>`  
`[<FIXED(F) OR REMOVABLE(R)>]`  
`[<SUBUNIT>]`  
`<SOURCE DEVICE>`  
`<DEVICE UNIT>`  
`[<FIXED(F) OR REMOVABLE(R)>]`  
`[<SUBUNIT>]`

Note that if you give an erroneous disk type as device type (this may happen, for example, if your system has different disk types as different units and you specify the wrong unit), the disk will enter fault state and the system will hang.

### 2.2.5 §COPY-FILE

The two parameters can now be used to specify files on remote systems, the file specifications can contain the following parameters:

`system(user(password:project)).(directory:user)file:type:version`

The parameter sequence is unchanged: `<DESTINATION FILE>`  
`<SOURCE FILE>`

When copying files containing "holes" between different systems, we recommend that you, when possible, use a remote destination and a local source (that is, from local system to a remote system) as copying in that direction is significantly faster.

### 2.2.6 §CREATE-DIRECTORY

The third parameter, DEVICE UNIT, is now always a required parameter.

The parameter sequence is thus: `<DIRECTORY NAME>`  
`<DEVICE NAME>`  
`<DEVICE UNIT>`  
`[<FIXED(F) OR REMOVABLE(R)>]`  
`[<SUBUNIT>]`  
`<BIT FILE ADDRESS>`

Note that if you give an erroneous disk type as device type (this may happen, for example, if your system has different disk types as different units and you specify the wrong unit), the disk will enter fault state and the system will hang.

---

## Page 30

# SINTRAN III RELEASE INFORMATION, K-VERSION

## SINTRAN III COMMANDS

### 2.2.7 @DELETE-SPOOLING-FILE

The second parameter can now be used to specify files on remote systems, the file specification can contain the following parameters:

`system(user(password:project)).(directory:user)file:type;version`

The parameter sequence is unchanged: `<PERIPHERAL FILE NAME> <FILE NAME>`

Refer to chapter 8 ("Spooling") on page 121 for further details.

### 2.2.8 @DEVICE-FUNCTION

The subfunction CLEAR-DEVICE is not allowed on floppy disk.

Two new subfunctions are introduced for use on SCSI streamer devices:

|   |   |
|---|---|
| 312 | LOAD should always be used after a new tape is inserted. |
| 406 | RESET-DEVICE performs a hard reset of the device. Any tape loaded will be rewound. |

### 2.2.9 @DUMP-DIRECTORY-ENTRY

The second parameter, DEVICE UNIT, is now always a required parameter.

The parameter sequence is thus: `<DEVICE NAME> <DEVICE UNIT> [<FIXED(F) OR REMOVABLE(R)>] [<SUBUNIT>] <OUTPUT FILE>`

Note that if you give an erroneous disk type as device type (this may happen, for example, if your system has different disk types as different units and you specify the wrong unit), the disk will enter fault state and the system will hang.

### 2.2.10 @ENTER-DIRECTORY

The third parameter, DEVICE UNIT, is now always a required parameter.

The parameter sequence is thus: `<DIRECTORY NAME> <DEVICE NAME> <DEVICE UNIT> [<FIXED(F) OR REMOVABLE(R)>] [<SUBUNIT>]`

Note that if you give an erroneous disk type as device type (this may happen, for example, if your system has different disk types as different units and you specify the wrong unit), the disk will enter fault state and the system will hang.

Norsk Data ND-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 31

# SINTRAN III RELEASE INFORMATION, K-VERSION
## SINTRAN III COMMANDS

Further, note that the @ENTER-DIRECTORY command now stores the system number of the system entering a directory in the directory entry (on disk). This feature is used in FTX systems, but may cause problems if a directory on a removable disk is moved to a new system without being released by @RELEASE-DIRECTORY.  
The command @UNLOCK-DIRECTORY may be used in such cases.  
The layout of the new directory entry on disk is shown on page 120.

### 2.2.11 @INITIALIZE-ERROR-LOG

If the standard error program (RTERR) is stopped and replaced by the ERS/SINTRAN III Watchdog (ERS3WD), this command will not work.

### 2.2.12 @FILE-STATISTICS

The first parameter can now be used to specify files on remote systems, the file specification can contain the following parameters:

system(user(password:project)).(directory:user)file:type:version

The parameter sequence is unchanged: `<FILE NAME>`  
`<OUTPUT FILE>`

### 2.2.13 @LIST-DEVICE-FUNCTIONS

A new parameter is introduced as the first parameter.

The parameter sequence is thus: `<COMMAND>`  
`<OUTPUT FILE>`

### 2.2.14 @LIST-EXECUTION-QUEUE

When the list of RT-programs in the time queue is greater than 16, the list is sorted columnwise from left to right.

### 2.2.15 @LIST-FILES

The first parameter can now be used to specify files on remote systems, the file specification can contain the following parameters:

system(user(password:project)).(directory:user)file:type:version

The parameter sequence is unchanged: `<FILE NAME>`  
`<OUTPUT FILE>`

### 2.2.16 @LIST-TIME-QUEUE

When the list of RT-programs in the time queue is greater than 16, the list is sorted columnwise from left to right.

---

## Page 32

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## SINTRAN III COMMANDS

### 2.2.17 @LIST-TITLE

The LIST-TITLE command now lists information about generation as well as version and revision level.  
Configuration status (standard configuration or specially generated) is also listed.

### 2.2.18 @LOOK-AT

In the VSX-version of SINTRAN III version K, there are some changes to the first parameter (space reference):

| Parameter   | Description                 |
|-------------|-----------------------------|
| RESIDENT    | is no longer available      |
| IMAGE       | is no longer available      |
| COMMON-CODE | is a new area which can be investigated by the LOOK-AT command. |

A segment name can also be used as space reference (without specifying SEGMENT). All segment names listed on page 134 can be used as well as any segment names defined locally.

### 2.2.19 @MOVE-SPOOLING-QUEUE-ENTRY

The second and last parameters can now be used to specify files on remote systems, the file specification can contain the following parameters:

system(user(password:project)).(directory:user)file:type:version

The parameter sequence is unchanged:  
\<PERIPHERAL FILE NAME\>  
\<FILE NAME\>  
\<INSERT OR APPEND\>  
\<FILE NAME\>

Refer to chapter 8 ("Spooling") on page 121 for further details.

### 2.2.20 @PRINT-ERROR-LOG

If the standard error program (RTERR) is stopped and replaced by the ERS/SINTRAN III Watchdog (ERS3WD), this command will not work.

### 2.2.21 @RELEASE-DEVICE-UNIT

The second parameter, DEVICE UNIT, is now always a required parameter.

The parameter sequence is thus:  
\<DEVICE NAME\>  
\<DEVICE UNIT\>  
\[<FIXED(F) OR REMOVABLE(R)\>\]  
\[<SUBUNIT>\]

---

## Page 33

# SINTRAN III RELEASE INFORMATION, K-VERSION

## SINTRAN III COMMANDS

Note that if you give an erroneous disk type as device type (this may happen, for example, if your system has different disk types as different units and you specify the wrong unit), the disk will enter fault state and the system will hang.

### 2.2.22 @REMOVE-FROM-SPOOLING-QUEUE

The second parameter can now be used to specify files on remote systems, the file specification can contain the following parameters:

```
system(user(password:project)).(directory:user)file:type:version
```

The parameter sequence is unchanged: `<PERIPHERAL FILE NAME> <FILE NAME>`

Refer to chapter 8 ("Spooling") on page 121 for further details.

### 2.2.23 @RENAME-DIRECTORY

The fourth parameter, DEVICE UNIT, is now always a required parameter.

The parameter sequence is thus:

```
<OLD DIRECTORY NAME>
<NEW DIRECTORY NAME>
<DEVICE NAME>
<DEVICE UNIT>
{<FIXED(F) OR REMOVABLE(R)>}
{<SUBUNIT>}
```

Note that if you give an erroneous disk type as device type (this may happen, for example, if your system has different disk types as different units and you specify the wrong unit), the disk will enter fault state and the system will hang.

### 2.2.24 @RENAME-FILE

Files which are open can no longer be renamed.

### 2.2.25 @RESERVE-DEVICE-UNIT

The second parameter, DEVICE UNIT, is now always a required parameter.

The parameter sequence is thus:

```
<DEVICE NAME>
<DEVICE UNIT>
{<FIXED(F) OR REMOVABLE(R)>}
{<SUBUNIT>}
```

Note that if you give an erroneous disk type as device type (this may happen, for example, if your system has different disk types as different units and you specify the wrong unit), the disk will enter fault state and the system will hang.

---

## Page 34

# SINTRAN III RELEASE INFORMATION, K-VERSION
## SINTRAN III COMMANDS

### 2.2.26 @RT-PROGRAM-LOG

This command is not supported for Net/One terminals or MTAD devices.

### 2.2.27 @START-PROGRAM-LOG

This command is not supported for Net/One terminals or MTAD devices.

### 2.2.28 @STOP-TERMINAL

The STOP-TERMINAL command has been changed so that terminals in 'waiting' or 'escape-off' state can be stopped as well as other terminals.

To stop a terminal in 'escape off' or in 'waiting' state, will take some time. The message 'WAIT' will be given up to 6 times. If the terminal is not stopped within this time, the user will be asked if it should be stopped anyway:

TERMINAL IS NOT STOPPED  
IT IS IN 'ESCOFF' OR IN A WAITING STATE,  
DO YOU WANT TO REMOVE IT FROM THIS STATE AND STOP IT?

Answering YES to this question will remove the terminal from the current state, and a new attempt will be made to stop it. This may also take some time, and the message 'WAIT' will again be given.

If a terminal is executing the @STOP-TERMINAL command on another terminal and a third terminal tries to stop the same terminal, the last terminal attempting @STOP-TERMINAL, will get the message:

ALREADY EXECUTED BY TERMINAL: xx

Furthermore, the restriction that only user SYSTEM could use the @STOP-TERMINAL command has now been lifted: public users can now also use the '@STOP-TERMINAL' command. However, they must be logged in on the same user area as that logged on to the terminal to be stopped.

---

## Page 35

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## SINTRAN III COMMANDS

### 2.3 NEW COMMANDS

#### 2.3.1 @DEFINE-MASS-STORAGE-UNIT

Define a mass storage device in the directory table. This is used to reserve a directory index for a device even if the device is not to be entered yet. On standard versions of SINTRAN III/VSX version K, devices are not allocated a directory index at generation time, but are placed in the directory table when the device is defined. Also, some devices which cannot contain directories, for example magnetic tape drives, must be defined. Defining a device is done either by entering it (@ENTER-DIRECTORY), or by this command.

**Parameters:**

- `<DEVICE NAME>`
- `<DEVICE UNIT>`
- `{FIXED(F) OR REMOVABLE(R)}`
- `[<DEVICE SUB-UNIT>]`

A definition of a device in the directory table does not survive a warm start.

This command is restricted to user SYSTEM only.

#### 2.3.2 @DELETE-MASS-STORAGE-UNIT

Delete a definition of a mass storage device from the directory table.

**Parameters:**

- `<DEVICE NAME>`
- `<DEVICE UNIT>`
- `{FIXED(F) OR REMOVABLE(R)}`
- `[<DEVICE SUB-UNIT>]`

This command is restricted to user SYSTEM only.

#### 2.3.3 @GIVE-OBJECT-BLOCKS

This command will make it possible to create more than 256 files for a single user. To allow this, the user must be given more than the single object block given initially. Each object block contains object entries for 256 files. The maximum number of object blocks per user is 16 (which means a maximum of 4096 files). The number of files allowed for a user is reported by the command @USER-STATISTICS.

**Parameters:**

- `<DIRECTORY NAME:USER NAME>`
- `<NUMBER OF OBJECT BLOCKS>`

If the directory resides on a diskette, this command is allowed for all users; on other devices it is restricted to user SYSTEM only.

---

## Page 36

# SINTRAN III RELEASE INFORMATION, K-VERSION
## SINTRAN III COMMANDS

### 2.3.4 @LIST-MASS-STORAGE-UNITS

List all mass storage units and corresponding directory index. The default value of the parameter `<output file>` is the terminal.

**Parameter:** `<OUTPUT FILE>`

This command is allowed for all users.

### 2.3.5 @SET-INITIAL-FILE-ACCESS

Set default file access to be used for all users subsequently created on this system. The default file access can be changed for each user individually by the command @SET-DEFAULT-FILE-ACCESS.

**Parameters:**

- `<PUBLIC ACCESS (R,W,A,C,D IN COMBINATIONS OR N)>`
- `<FRIEND ACCESS (R,W,A,C,D IN COMBINATIONS OR N)>`
- `<OWN ACCESS (R,W,A,C,D IN COMBINATIONS OR N)>`

This command is restricted to user SYSTEM only.

### 2.3.6 @SET-INITIAL-FRIEND-ACCESS

Set default friend access to be used for all users subsequently created on this system. The friend access can be changed for each user individually by the command @SET-FRIEND-ACCESS.

**Parameters:**

- `<ACCESS (R,W,A,C,D IN COMBINATIONS OR N)>`

This command is restricted to user SYSTEM only.

### 2.3.7 @SET-MASS-STORAGE-SIZE

Set or change the size of a directory to be entered. This command is used to explicitly set the directory size of a subdivided disk. It is intended for use on large disks connected to SCSI controllers to match the directory size to the size of directories on SMD or ST-506 (Winchester) disk controllers.

**Parameters:**

- `<DEVICE NAME>`
- `<DEVICE UNIT>`
- `[<FIXED(F) OR REMOVABLE(R)>]`
- `[<DEVICE SUB-UNIT>]`
- `<NUMBER OF PAGES>` (number of pages on each subunit)

This command is restricted to user SYSTEM only. If a disk unit is divided into subunits (2-8), all subunits must have the same directory size. The directory on the specified device and unit must not be entered.

---

## Page 37

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## SINTRAN III COMMANDS  

### 2.3.8 @TAKE-OBJECT-BLOCKS  

This command is used to restrict the number of files for a single user. The object blocks to be "taken" must be free; thus if a user is allowed to have a maximum of 512 files, with only files number 0, 1, 2 and 300 used, both object blocks for this user are used and the command may not be given. The number of files allowed for a user is reported by the command @USER-STATISTICS, and the command @LIST-FILES will show which file numbers are used.

Parameters:  
`<DIRECTORY NAME:USER NAME>`  
`<NUMBER OF OBJECT BLOCKS>`

If the directory resides on a diskette, this command is allowed for all users; on other devices it is restricted to user SYSTEM only.

### 2.3.9 @UNLOCK-DIRECTORY  

This command must be used prior to @ENTER-DIRECTORY if the directory has been entered but not released on another system.

The parameter sequence is:

| Parameter  |   
| --- |  
| `<DIRECTORY NAME>` |  
| `<DEVICE NAME>` |  
| `<DEVICE UNIT>` |  
| `[<FIXED(F) OR REMOVABLE(R)>]` |  
| `[<DEVICE SUB-UNIT>]` |  

This command is restricted to user SYSTEM only.  

| 312 |  
| --- |  
| +   |  
| 406 |  

Norsk Data ND-60.230.5 EN  

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 38

# 3. MONITOR CALLS (ND-100)

## 3.1 MODIFIED MONITOR CALLS

### 3.1.1 WCI MON 20

On error return, the A-register previously contained a random value. It will now contain error code 44a ("Too long parameter"). MON 20 is for internal use by ND only.

### 3.1.2 TUSED MON 114

MON TUSED (MON 114) can now also be called from RT-programs.

### 3.1.3 ABSTR MON 131

The restriction that parameters had to reside on PIT 0 is removed for the VSX-version, thus:  
Parameters can now reside on any PIT (VSX only).  
Parameters must reside on PIT 0 (VSE only).  

EXABS (MON 335) is recommended if you write code to be independent of whether it is run on VSE or VSX.

In function 43 (read format table) and 44 (write format table), a 32-bit INTEGER is now used as the third parameter (disk address).

Some functions now support SCSI streamer devices. Refer to page 245 for further information.

### 3.1.4 MAGTP MON 144

Function 21a (Clear Device) is only allowed from RT-programs running on hardware protection ring 2.

Function 23a (Set Density and Parity) is changed for SCSI magnetic tape compared to STC magnetic tape. The parameter `<density>` may take the following values:

| <density> | STC magnetic tape | SCSI magnetic tape  |
|-----------|-------------------|---------------------|
|           | 0: 1600 BPI       | use default density |
|           | 1: 6250 BPI       | 800 BPI             |
|           | 2: 800 BPI        | 1600 BPI            |
|           | 3: illegal value  | 6250 BPI            |
| 800       | 800 BPI           | 800 BPI             |
| 1600      | 1600 BPI          | 1600 BPI            |
| 6250      | 6250 BPI          | 6250 BPI            |

---

## Page 39

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### 3.1.5 ENTSG MON 157

MON ENTSG (MON 157) has been changed to "remember" which segments are entered (maximum 24 segments can be remembered), and reenter these after a power fail restart.  
The segments can be removed from the selected PIT by using function 4 (REMSG) of MON SGMTY (MON 341). Unfixing the segment (using MON 116 - UNFIX) also removes the segment from the Page Index Table.  
There are some changes to the parameters (new range of PITs and a new set of output values).

**Monitor call format:**

```
LDA (PAR1)      % A = address of parameter list  
MON 157         % ENTSG
```

```
PAR1, (ISEG         % segment  
(IPIT              % PIT (for VSX, the range is 0-17₈)  
(INTLV             % interrupt level  
(ISTR1             % start address (entry point)
```

**Input parameters:**

- `<ISEG>` segment  
- `<IPIT>` PIT (for VSX, the range is 0-17₈)  
- `<INTLV>` interrupt level (must be a free level, one of: 6,7,10₈,11₈)  
- `<ISTR1>` start address (entry point)

**Output parameters:**

| A-register | Description                              |
|------------|------------------------------------------|
| 0          | ok                                       |
| -1         | attempt to enter too many segments       |
| -2         | illegal segment number                   |
| -3         | illegal Page Index Table                 |
| -4         | segment is not fixed                     |
| -5         | illegal interrupt level                  |
| -6         | PIT already in use                       |

### 3.1.6 DEBUG MON 205

Functions for multi-segment debugger has been added to this monitor call. MON 205 is for internal use by ND only.

### 3.1.7 APSPF MON 240

The parameters containing the file name of the file to be appended to the spooling queue (pointed to by the X-register), and the peripheral file (pointed to by the A-register), can now contain a remote file specification. The complete remote file specification can contain the following parameters:

```
system(user:password:project)::(directory:user)file:type:version
```

Refer to chapter 8 ("Spooling") on page 121 for further details.

---

## Page 40

# SINTRAN III RELEASE INFORMATION, K-VERSION  

## MONITOR CALLS (ND-100)

### 3.1.8 SUSCN MON 241  

If originally logged in as user RT, it is no longer possible to set user context to user SYSTEM.  

### 3.1.9 DEABF MON 256  

The parameters containing an abbreviated file name (pointed to by the X-register), and full a file name (pointed to by the A-register), can now contain a remote file specification. The complete remote file specification can contain the following parameters:  

system(user{password:project}).(directory:user)file:type:version  

If the abbreviated file name contains a remote specification, the name of the remote system cannot be abbreviated.  

### 3.1.10 CPUST MON 262  

The ND-110/CX and ND-120/CX CPUs are now supported and some new values are returned. The following words of the returned array are affected:  

| DISP | NAME      | DESCRIPTION                                                     |
|------|-----------|-----------------------------------------------------------------|
| 1    | HWINFO(0) | Hardware information                                            |
|      |           | Left byte = CPU type                                            |
|      |           | 0 - 3 unchanged                                                 |
|      |           | 4 = ND-110/CX or ND-120/CX 48-bit floating                      |
|      |           | 5 = ND-110/CX or ND-120/CX 32-bit floating                      |
|      |           | 6 - 255 Not used                                                |
|      |           | Right byte = Instruction set                                    |
|      |           | 0 - 1 unchanged                                                 |
|      |           | 2 = ND-100/CX w/micro segadm. for 4 PITs                        |
|      |           | 3 = ND-100/CX, ND-110/CX or ND-120/CX                           |
|      |           |     with microprog. seg.adm. for 16 PITs                        |
|      |           | 4 - 255 Not used                                                |
| 2    | HWINFO(1) | ND-110/CX or ND-120/CX microprogram version                     |
| 3    | HWINFO(2) | System type (100, 500, 502, 5561, ...)                          |

The system type is either supplied when the system is generated or you will be asked for it when you install SINTRAN from diskettes.  

### 3.1.11 GDEVT MON 263  

Net/One is now supported by the VSX-version and a new value can be returned. This affects the device attributes returned in the combined A and D registers:  

Bit no. 10a Indicates a NOTS (NET/One Terminal Server) terminal.  
Bit no. 11a Indicates an MTAD device.  

Norsk Data ND-60.230.5 EN

---

## Page 41

# SINTRAN III RELEASE INFORMATION, K-VERSION
## MONITOR CALLS (NO-100)

### 3.1.12 MLAMU MON 315

Two new functions, functions 11␣ and 12␣ are introduced.

#### Function no. 11␣

PARLI, (FUNC &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;% function number  
(LAMID &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;% LAMU id  
(SIZE &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;% size  
(PHYSA &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;% physical page number  

**Input parameters:**  
FUNC  
&nbsp;&nbsp;&nbsp;&nbsp;= 11 : Create system LAMU  
LAMU id  
&nbsp;&nbsp;&nbsp;&nbsp;= 0 : The system will return the selected <LAMU id>.  
&nbsp;&nbsp;&nbsp;&nbsp;# 0 : The LAMU will be identified by the given <LAMU id> if the <LAMU id> is unused and inside legal range.   
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Legal range for system LAMUs is -2:-1023.  
size  
&nbsp;&nbsp;&nbsp;&nbsp;: Number of pages in the LAMU. Legal range 1-2008  
phys.addr  
&nbsp;&nbsp;&nbsp;&nbsp;= 0 : The system will select a free memory area large enough for the LAMU, and reserve it.  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;The first physical page number of this memory area will be returned in <phys.addr>.  
&nbsp;&nbsp;&nbsp;&nbsp;# 0 : Specifies the first physical page for the LAMU.

**Output parameters:**  
LAMU id  
&nbsp;&nbsp;&nbsp;&nbsp;: The LAMU identifier  
phys.addr  
&nbsp;&nbsp;&nbsp;&nbsp;: The first physical page number of the system LAMU.

**Rules:**  
1. Only legal from users SYSTEM or RT, or from RT-programs running on protection ring 1.

#### Function no. 12␣

PARLI, (FUNC &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;% function number  
(LAMID &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;% LAMU id  
(SIZE &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;% size  
(ADDR &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;% address  

**Input parameters:**  
FUNC  
&nbsp;&nbsp;&nbsp;&nbsp;= 12 : Create and connect a system LAMU  
LAMU id  
&nbsp;&nbsp;&nbsp;&nbsp;= 0 : The system will return the selected <LAMU id>.  
&nbsp;&nbsp;&nbsp;&nbsp;# 0 : The LAMU will be identified by the given <LAMU id> if the <LAMU id> is unused and inside legal range.  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Legal range for system LAMUs is -2:-1023.  
size  
&nbsp;&nbsp;&nbsp;&nbsp;: Number of pages in the LAMU. Legal range 1-2008.  
address  
&nbsp;&nbsp;&nbsp;&nbsp;: Logical start address of LAMU. Legal range 1008-2778.

**Output parameters:**  
LAMU id  
&nbsp;&nbsp;&nbsp;&nbsp;: The LAMU identifier  
address  
&nbsp;&nbsp;&nbsp;&nbsp;: Physical address of LAMU

**Rules:**  
1. Only legal from users SYSTEM or RT, or from RT-programs running on protection ring 1.

---

## Page 42

# 3.1.13 FSMTY MON 327

Three new functions introduced: T-register = 2 : return block size  
= 3 : get file name  
= 4 : get file/device information

The monitor call format is thus:

```
LDT  FUNC   % T = function  
LDA  FILNO  % A = open file number  
LDX  [BUFFR  % X = address of buffer to receive file name  
MON  327   
JMP  ERROR  % error return  
        ... % normal return
```

|     |    |  
|-----|----|  
| FUNC, | 2 |  
| FILNO, | 101 |  
| BUFFR, | 0; *+26/ |  

## Function no. 1:

**Function:**  
Write back the open-file-table index-block for an open file to disk.

**Input parameters:**  
T-register : function = 1  
A-register : open file number  

**Output parameters:**  
Return: Error - A-register contains error code  
Skip return: OK, normal return  

## Function no. 2:

**Function:**  
Return block size of an open file.

**Input parameters:**  
T-register : function = 2  
A-register : open file number  

**Output parameters:**  
Return: Error - A-register contains error code  
Skip return: OK, normal return, A-register = block size in words.  

Norsk Data ND–60.230.5 EN

---

## Page 43

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### Function no. 3:

**Function:**

Get full file name of an open file. File name can be a remote file and contain the following parameters:

```
system.{directory:user}file:type
```

**Input parameters:**

- T-register : function = 3
- A-register : open file number
- X-register : address of buffer to receive file name

**Output parameters:**

| Return       | Description                                           |
|--------------|-------------------------------------------------------|
| Error        | A-register contains error code                        |
| Skip return  | OK, normal return, buffer pointed at by X contains name.|

### Function no. 4:

**Function:**

Get information of an open file identified by open file number or device number.

**Input parameters:**

- T-register : function = 4
- A-register : open file number or device number

**Output parameters:**

| Return       | Description                                           |
|--------------|-------------------------------------------------------|
| Error        | A-register contains error code                        |
| Skip return  | OK, normal return, the following information is returned: |

- A = open file number or device number (the value returned in the A register is the opposite of A as input, i.e., if open file number is input, device number is returned and the other way around).
- X = TYPRING bits (TYPRING word from data field of device)
- D = status :
  - bit 0 = 1 if file is open for write.
  - bit 1 = 1 if spooling file or terminal/TAD.

### 3.1.14 UDMA MON 333

Function 62 (Wait on interrupt/DMA finish) now has two new subfunctions:

- DPARI = 2 : Enable RT on interrupt (set repeated execution)
- DPARI = 3 : Disable RT on interrupt (clear repeated execution)

---

Norsk Data ND-60.230.5 EN
Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 44

# 3.1.15 IOMTY MON 336

Note that the functions and parameters used in the J-version of MON 336 (IOMTY) are now revised completely.

The I/O multifunction (IOMTY) monitor call is used to change the attributes of terminal and terminal access device (TAD) input/output. It is also used to configure NET/One interfaces and SCSI disks. This monitor call needs a varying number of input and output parameters depending upon function; all parameters are therefore placed in an array.

## Monitor call format:

    LDA (PARLI     % A = address of parameter list
    MON 336        % IOMTY
    JMP ERROR      % error return
    ..........     % normal return

    PARLI, FUNC     % address of function
    SIZE            % address of the length of the parameter array
    ARRAY           % address of the parameter array

    ARRAY, 0        % function parameter 1 (word 1)
    0              % function parameter 2 (word 2)
    0              % function parameter 3 (word 3)
    :              :
    :              :
    0              % function parameter n  (word n)

## Input parameters:

- FUNC = function code
- SIZE = length of function parameter array
  (must be greater than or equal number of input/output parameters specified for function).
- ARRAY = function parameter array.

The function code (FUNC) is divided into 4 ranges, 0-77₈, 100₈-177₈, 200₈-277₈ and 300₈-377₈. Range 0-77₈ can only update SINTRAN memory area, the other ranges can update SINTRAN memory, image and save area.

The meaning of AREA when used in the input parameter array is:

| AREA | Update resident? | Update image? | Update save? |
|------|------------------|---------------|--------------|
| 1    | Yes              | No            | No           |
| 2    | No               | Yes           | No           |
| 3    | Yes              | Yes           | No           |
| 4    | No               | No            | Yes          |
| 5    | Yes              | No            | Yes          |
| 6    | No               | Yes           | Yes          |
| 7    | Yes              | Yes           | Yes          |

---

## Page 45

# SINTRAN III Release Information, K-Version

## Monitor Calls (ND-100)

The following functions are defined:

| Function | Brief explanation |
|----------|-------------------|
| 0        | Set terminal to become own terminal. |
| 1        | Reset to original own terminal. |
| 2        | Set character conversion mode for device no. 0. |
| 3        | Get character conversion mode for device no. 0. |
| 4        | Set echo strategy. |
| 5        | Get echo strategy. |
| 6        | Set break strategy. |
| 7        | Get break strategy. |
| 10₁₀     | Set terminal mode. |
| 11₈      | Get terminal mode. |
| 12₈      | Set/reset 8-bit unmodified input and output. |
| 13₈      | List terminals and TADs in system. |
| 14₈      | Display functions. |
| 15₈      | Change signals on a V.24 connection. |
| 16₈      | Set/reset terminal in/from test mode. |
| 17₈      | Connect NIU on specified device number. (VSX only) |
| 20₈      | Disconnect NIU on specified device number. (VSX only) |
| 21₈      | Read/write PIO interface. |
| 22₈      | Get magic number for a TAD. (VSX only) |
| 23₈      | Access the CI window on a NOTS controller. (VSX only) |
| 24₈      | Get nonreserve status of a device. (VSX only) |
| 25₈      | Set/reset nonreserve status of a device. (VSX only) |
| 100₈     | Return function parameters set by functions 101 - 177. |
| 101₈     | Set terminal type. |
| 102₈     | Set escape or local character. |
| 103₈     | Start and stop Xon/Xoff protocol, input control. |
| 104₈     | Start and stop Xon/Xoff protocol, output control. |
| 105₈     | Set Xon/Xoff only or dual function Xon/Xoff. |
| 106₈     | Set character length. |
| 107₈     | Set baud rate for terminal. |
| 110₈     | Set number of stop bits. |
| 111₈     | Set terminal to printer or reset printer to terminal. |
| 112₈     | Set half or full duplex on terminal. |
| 113₈     | Set/reset variable speed on terminal. |
| 114₈     | Set/reset terminal connected to printer. |
| 200₈     | Set NOTS configuration. (VSX only) |
| 201₈     | Get NOTS configuration. (VSX only) |
| 202₈     | Get information about a NOTS line. (VSX only) |
| 203₈     | Restart/reload a NOTS. (VSX only) |
| 300₈     | Set SCSI device definition. (VSX only) |
| 301₈     | Get SCSI device definition. (VSX only) |
| 302₈     | Delete SCSI device definition. (VSX only) |
| 303₈     | Get current SCSI device definition. (VSX only) |

## Notes:

1. Device number 1 is console terminal.
2. If background program, then logical device number 0 means own terminal.
3. User SYSTEM does not have to reserve a device before changing attributes (except functions 0 and 111).

---

## Page 46

# SINTRAN III Release Information, K-Version
## Monitor Calls (ND-100)

### Rules:
Common rules are given in the following table. Note that there can be additional rules specified under description on each function.

#### Function Table

| Function Number (Octal) | Minimum Function Parameter Array Size | Update SINTRAN Image/Save Area | Update Own Terminal's SINTRAN Memory Area | Update Other Terminal's Memory Area | Reservation of Device Needed | Allowed on Net/One Terminals | Allowed on TADs | Allowed on MTADs | Callable From |
|-------------------------|----------------------------------------|--------------------------------|-------------------------------------------|-------------------------------------|------------------------------|------------------------------|-----------------|----------------|---------------|
| 0 1                     | - P P 1,0 Yes No                      | Yes B                          |                                          |                                     |                              |                              |                 |                | B             |
| 1 1                     | - P P No Yes                          | No Yes B                      |                                          |                                     |                              |                              |                 |                | B             |
| 2 2                     | - P P 1 Yes No                        | Yes B                          |                                          |                                     |                              |                              |                 |                | B             |
| 3 2                     | - P P No Yes                          | No Yes B                      |                                          |                                     |                              |                              |                 |                | B             |
| 4 3                     | - P P 1 Yes Yes                       | Yes F,B (RT-program)          |                                          |                                     |                              |                              |                 |                |               |
| 5 3                     | - P P No Yes                          | Yes Yes F,B                   |                                          |                                     |                              |                              |                 |                |               |
| 6 4                     | - P P 1 Yes Yes                       | Yes F,B                       |                                          |                                     |                              |                              |                 |                |               |
| 7 4                     | - P P No Yes                          | Yes Yes F,B                   |                                          |                                     |                              |                              |                 |                |               |
| 10 3                    | - P P 1,0 Yes Yes                     | Yes F,B                       |                                          |                                     |                              |                              |                 |                |               |
| 11 1                    | - P P No Yes                          | Yes Yes F,B                   |                                          |                                     |                              |                              |                 |                |               |
| 12 2                    | - P P 1 Yes                           | Yes Yes F,B                   |                                          |                                     |                              |                              |                 |                |               |
| 13 1                    | - - -                                 | Yes Yes F,B                   |                                          |                                     |                              |                              |                 |                |               |
| 14 4                    | - - -                                 | No No F,B                     |                                          |                                     |                              |                              |                 |                |               |
| 15 3                    | - P P 1                               | No Yes No F,B                 |                                          |                                     |                              |                              |                 |                |               |
| 16 2                    | - S,R S,R No                          | Yes No F,B                    |                                          |                                     |                              |                              |                 |                |               |
| 17 4                    | - - - 1,0                             | Yes Yes F,B                   |                                          |                                     |                              |                              |                 |                |               |
| 20 2                    | - - - 1,0                             | Yes -                         |                                          |                                     |                              |                              |                 |                | B             |
| 21 4                    | - - - 1,0                             | - -                           |                                          |                                     |                              |                              |                 |                | F,B           |
| 22 2                    | - - - 1,0                             | Yes -                         |                                          |                                     |                              |                              |                 |                | B             |
| 23 3                    | - - - 1,0                             | Yes -                         |                                          |                                     |                              |                              |                 |                | F,B           |
| 24 2                    | - - - 1,0                             | Yes -                         |                                          |                                     |                              |                              |                 |                | F,B           |
| 25 2                    | - - - 1,0                             | Yes Yes F,B                   |                                          |                                     |                              |                              |                 |                |               |
| 100 *                   | - *** ***                             | No Yes Yes F,B                |                                          |                                     |                              |                              |                 |                | (RT-program) |
| 101 3                   | S P S,R 1                             | Yes Yes Yes F,B               |                                          |                                     |                              |                              |                 |                |               |
| 102 4                   | S P S,R 1                             | Yes Yes Yes F,B               |                                          |                                     |                              |                              |                 |                |               |
| 103 5                   | S P P 1                               | No No No F,B                  |                                          |                                     |                              |                              |                 |                |               |
| 104 5                   | S P P 0                               | No No No F,B                  |                                          |                                     |                              |                              |                 |                |               |
| 105 3                   | S P P 1                               | No No No F,B                  |                                          |                                     |                              |                              |                 |                |               |
| 106 4                   | S P S,R 1                             | No No No F,B                  |                                          |                                     |                              |                              |                 |                |               |
| 107 4                   | S P S,R 1                             | No No No F,B                  |                                          |                                     |                              |                              |                 |                |               |
| 110 3                   | S S P 1                               | No No No F,B                  |                                          |                                     |                              |                              |                 |                |               |
| 111 4                   | S S S 1,0                             | Yes Yes Yes F,B               |                                          |                                     |                              |                              |                 |                |               |
| 112 3                   | S P S,R 1                             | No No No F,B                  |                                          |                                     |                              |                              |                 |                |               |
| 113 3                   | S S S 1                               | No No No F,B                  |                                          |                                     |                              |                              |                 |                |               |
| 114 3                   | S S S 1                               | Yes No Yes F,B                |                                          |                                     |                              |                              |                 |                |               |
| 200 4                   | S S S -                               | Yes -                         |                                          |                                     |                              |                              |                 |                | B             |
| 201 3                   | S S S -                               | Yes -                         |                                          |                                     |                              |                              |                 |                | B             |
| 202 4                   | - - -                                 | - Yes - B                     |                                          |                                     |                              |                              |                 |                |               |
| 203 4                   | - - -                                 | - Yes - B                     |                                          |                                     |                              |                              |                 |                |               |
| 300 3                   | - - -                                 | - Yes - B                     |                                          |                                     |                              |                              |                 |                |               |
| 301 4                   | - - -                                 | - Yes - B                     |                                          |                                     |                              |                              |                 |                |               |
| 302 1                   | - - -                                 | - - - B                       |                                          |                                     |                              |                              |                 |                |               |
| 303 4                   | - - -                                 | - - - B                       |                                          |                                     |                              |                              |                 |                |               |

#### Abbreviations:
- B: Background program
- F: Foreground program
- (RT-program): Real-time program
- I: Input part of device
- O: Output part of device
- P: Public users
- R: User RT
- S: User SYSTEM

##### Notes:
- *) Depends on which function to return parameter(s) from.
- **) Only allowed to return function parameters for functions the user is allowed to set.

---

## Page 47

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

Return: Error, A-reg = error code.  
X-reg = area which failure occurred:  
1 = memory, 2 = image, 4 = save.  
X-reg is only set for functions 100-177.

Skip return: OK.

## Error returns from IOMTY, MON 336:

X-reg: Letter "N" means: X-reg is not set by IOMTY.  
Letter "Y" means: X-reg = area which failure occurred:  
1 = memory, 2 = image, 4 = save.

A-reg: Octal.

| A-reg | X-reg | Brief explanation |
|-------|-------|-------------------|
| 5     | N     | Device not reserved. |
| 25    | N     | You are not authorized to do this. |
| 33    | N     | No such logical unit. |
| 153   | N     | Address outside segments bounds. |
| 155   | N     | File already opened by another user. (SINTRAN III/VSE-version only, will be returned if the file SINTRAN:DATA is opened by another user). |
| 174   | N     | Illegal parameter. |
| 201   | N     | Illegal function code. |
| 240   | N     | Illegal device type. |
| 346   | N     | Illegal baud rate specified. |
| 347   | N     | Illegal character length. |
| 350   | N     | Illegal parity. |
| 351   | N     | Only legal for background. |
| 352   | N     | A device already defined as own terminal. |
| 353   | N     | Illegal break/echo strategy. |
| 354   | N     | Function parameter array too small. |
| 355   | N     | Illegal AREA specified. |
| 356   | Y     | Not 8-bit character length. |
| 357   | N     | Terminal already in a display table. |
| 360   | N     | Terminal not a master terminal. |
| 361   | N     | Terminal not connected to specified master terminal. |
| 362   | N     | Display table contain a device which is not a terminal |
| 363   | Y     | Error occurred during read/write in SINTRAN memory/image/save areas. |
| 364   | Y     | Baud rate is not set by software. |
| 365   | Y     | Illegal baud rate found in SINTRAN memory/image/save data field (TSPEED). |
| 366   | N     | Terminal is active. |

Norsk Data N0-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 48

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### Function code 0:

**Function description:**

Set logical device number to become own terminal.  
The terminal identified by the logical device number specified in word 1 will act as own terminal.  
See also function no. 1.

**Input parameters:**

Word 1 = Logical device number to become own terminal.

**Rules:**

1. Both the input and output part of a device must be reserved (even if user SYSTEM).

### Function code 1:

**Function description:**

Reset to original own terminal.  
See also function no. 0.

**Input parameters:**

None.

**Output parameters:**

Word 1 = Logical device number for terminal, set by function code 0.  
0 is returned if not set.

### Function code 2:

**Function description:**

Set character conversion mode for input when reading from device number 0, see MON IN8T.  
See also function no. 3.

**Input parameters:**

Word 1 = Logical device number.  
Word 2 = Character conversion mode:

| Value | Description            |
|-------|------------------------|
| 0     | No conversion.         |
| 1     | All characters uppercase. |

**Notes:**

1. Only input in user mode is affected.

---

## Page 49

# SINTRAN III RELEASE INFORMATION, K-VERSION
## MONITOR CALLS (ND-100)

### Function code 3:

#### Function description:
Get character conversion mode.  
See also function no. 2.

#### Input parameters:
Word 1 = logical device number.

#### Output parameters:
Word 1 = unchanged  
Word 2 = character conversion mode as in function 2.

### Function code 4:

#### Function description:
Set echo strategy.  
See also function no. 5 and MON ECHOM (MON 3).

#### Input parameters:
Word 1 = logical device number.  
Word 2 = echo strategy, as A-reg in MON ECHOM (MON 3).  
Word 3 = memory address to an 8-word bit map if user defined echo strategy, otherwise 0.

### Function code 5:

#### Function description:
Get echo strategy.  
See also function no. 4.

#### Input parameters:
Word 1 = logical device number.  
Word 2 = memory address to an 8-word bit map.

#### Output parameters:
Word 1 = unchanged  
Word 2 = unchanged  
Word 3 = echo strategy, if returned with value 7 (user defined echo strategy) then 8-word bit map is returned to memory address specified in input word 2.

---

## Page 50

# SINTRAN III RELEASE INFORMATION, K-VERSION
## MONITOR CALLS (ND–100)

### Function code 6:

**Function description:**  
Set break strategy.  
See also function no. 7 and MON BRKM (MON 4).  

**Input parameters:**  
Word 1 = logical device number.  
Word 2 = break strategy, as A-reg in MON BRKM (MON 4).  
Word 3 = memory address to an 8-word bit map if user defined break strategy, else 0.  
Word 4 = maximum number of characters before break if break strategy ≥ 3 else 0  

### Function code 7:

**Function description:**  
Get break strategy.  
See also function no. 6.  

**Input parameters:**  
Word 1 = logical device number.  
Word 2 = memory address to an 8-word bit map.  

**Output parameters:**  
Word 1 = unchanged  
Word 2 = unchanged  
Word 3 = break strategy, if is returned with value 7 (user defined break strategy) then an 8-word bit map is returned to memory address specified in input word 2.  
Word 4 = maximum number of characters before break if break strategy ≥ 3 else 0  

### Function code 10a:

**Function description:**  
Set communication mode for a terminal.  
See also function no. 11 and TERMO (MON 52).  

**Input parameters:**  
Word 1 = logical device number.  
Word 2 = MODE, as in MON TERMO (MON 52).  
Word 3 = communication mode:  
- 0 = communication mode set as in MON TERMO (MON 52)  
- 1 = set only functions marked “Y” in MODE, see MODE in TERMO  
- 2 = reset functions marked “Y” in MODE, see MODE in TERMO

---

## Page 51

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### Function code 11a:

#### Function description:
Get communication mode for a terminal.  
See also function no. 10.

#### Input parameters:
- Word 1 = logical device number.

#### Output parameters:
- Word 1 = unchanged.
- Word 2 = MODE, as in TERMO (MON 52).

### Function code 12a:

#### Function description:
Set and reset 8-bit unmodified input/output.  
Unmodified means no parity on the most significant bit in byte.

#### Input parameters:
- Word 1 = logical device number.
- Word 2 = 8-bit status:
  - 0 = set 8-bit unmodified input/output.
  - 1 = reset to 7-bit input/output, parity on most significant bit in byte.

#### Rules:
1. Only legal for terminals with 8-bit character length (see also function 106).

#### Notes:
1. 8-bit unmodified input/output only valid in user mode.

### Function code 13a:

#### Function description:
List all logical device numbers for terminals and TADs in the system.

#### Input parameters:
- Word 1 = logical device number.  
  0 if first time, otherwise last device number returned, see output parameter word 1.

#### Output parameters:
- Word 1 = last device returned.  
  -1 if no more logical device numbers to be returned.
- Word 2 = number of logical devices returned.
- Word 3 = logical device number.
- Word 4 = device type: 0= TAD, 1= terminal.
- Word 5 = logical device number.  
  ...  
- Word n

#### Rules:
1. Permitted for users SYSTEM and RT only.

Norsk Data ND-50.230.5 EN  
Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 52

# Function Code 14a

## Function Description
Display functions.   
This function consists of several subfunctions specified in input parameter word 1. It is possible to define several display tables. One display table consists of one master terminal and several slave terminals connected to the master terminal. All what is written on the master terminal will also be written on slave terminals.

### Rules
1. Only allowed for user SYSTEM.  
2. A terminal can only be in one display table.

## Subfunction Code 1

### Subfunction Description
Define a master terminal.

### Input Parameters
- Word 1 = 1  
- Word 2 = logical device number to become a master terminal.

## Subfunction Code 2

### Subfunction Description
Undefine a master terminal, master terminal specified and slave terminals connected become "ordinary" terminals again.

### Input Parameters
- Word 1 = 2  
- Word 2 = logical device number for master terminal.

## Subfunction Code 3

### Subfunction Description
Insert a slave terminal in display table.

### Input Parameter
- Word 1 = 3  
- Word 2 = logical device number for master terminal.  
- Word 3 = logical device number for terminal to be inserted.

## Subfunction Code 4

### Subfunction Description
Remove a slave terminal from display table.

### Input Parameters
- Word 1 = 4  
- Word 2 = logical device number for master terminal.  
- Word 3 = logical device number for slave terminal to be removed.

---

## Page 53

# SINTRAN III RELEASE INFORMATION, K-VERSION
## MONITOR CALLS (ND-100)

### Subfunction code 5:

#### Subfunction description:
List master terminals defined in system.

#### Input parameters:
| Word | Description |
|------|-------------|
| 1 | = 5 |
| 2 | = last device returned. 0 if first time, otherwise last device number returned, see output word 2. |

#### Output parameters:
| Word | Description |
|------|-------------|
| 1 | = unchanged |
| 2 | = last device returned. -1 if no more logical device numbers to be returned. |
| 3 | = number of devices returned |
| 4 | = device number for master terminal |
| 5 | = device number for master terminal |
| n | = ... |

### Subfunction code 6:

#### Subfunction description:
List display table for a master terminal.

#### Input parameters:
| Word | Description |
|------|-------------|
| 1 | = 6 |
| 2 | = logical device number for master terminal. |
| 3 | = last device returned. 0 if first time, otherwise last device number returned, see output word 3. |

#### Output parameters:
| Word | Description |
|------|-------------|
| 1 | = unchanged |
| 2 | = unchanged |
| 3 | = last device returned. -1 if no more logical device numbers to be returned. |
| 4 | = number of devices returned. |
| 5 | = device number for slave terminal connected |
| 6 | = device number for slave terminal connected |
| 7 | = device ... |
| n |  |

Norsk Data ND-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 54

# Subfunction code 7:

**Subfunction description:**  
Enable/disable display for a master terminal and connected slave terminals.  
See also subfunction code 10.

**Input parameters:**  
Word 1 = 7  
Word 2 = logical device number for master terminal.  
Word 3 = display status:  
0 = enable display.  
1 = disable display.

# Subfunction code 10a:

**Subfunction description:**  
Get display status for a master terminal.  
See also subfunction 7.

**Input parameters:**  
Word 1 = 10  
Word 2 = logical device number for master terminal.

**Output parameters:**  
| Word | Description                  |
|------|------------------------------|
| 1    | unchanged                    |
| 2    | unchanged                    |
| 3    | display status:              |
|      | 0 = enabled for display      |
|      | 1 = disabled for display     |

# Function code 15a:

**Function description:**  
Set signals on a RS-232 connection.  
This function consists of several subfunctions specified in input parameter word 2.

**Rules:**  
1. Subfunction 1 is only allowed if 8-terminal buffer interface with FIFO (ND-102730 and ND-102740).

# Subfunction code 0:

**Subfunction description:**  
Turn off the V.24 signal DTR. It will automatically be turned back on again after about 5 seconds.

**Input parameters:**  
Word 1 = logical device number.  
Word 2 = 0

**Output parameters:**  
None.

---

## Page 55

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### Subfunction code 1:

**Subfunction description:**  
Set level of V.24 control line available on pin 19 in the plug panel connector.

**Input parameters:**  
Word 1 = logical device number.  
Word 2 = 1  
Word 3 = signal level:  
- 0 = set the signal to -12V (V.24 signal OFF).  
- 1 = set the signal to +12V (V.24 signal ON).

**Output parameters:**  
None.

**Note:**  
1. If not buffer terminal I/O controller then error code 240 is returned (ILLEGAL DEVICE TYPE).

### Function code 16:

**Function description:**  
Set terminal interface in test mode.  
Test mode will loop the transmitted data back to receive data.  
Data will also be transmitted to the terminal line.  
If the test mode is selected for one of the four interfaces, all four will be set in test mode.

**Input parameters:**  
Word 1 = logical device number.  
Word 2 = terminal mode:  
- 0 = set terminal in test mode.  
- 1 = reset terminal from test mode.

**Output parameters:**  
None.

### Function code 17:

**Function description:**  
Connect a NIU on a specified device number.  
Only available in the VSX-version.  
This function has two different subfunctions; one to start the connection request, the other to test the status of the request.

---

## Page 56

# SINTRAN III RELEASE INFORMATION, K-VERSION
## MONITOR CALLS (ND-100)

### Subfunction code 0:

**Subfunction description:**
Initiate a connection request for a NIU.

**Input parameters:**
- Word 1 = logical device number.
- Word 2 = Length of name string.
- Word 3 = Pointer to name string.
- Word 4 = 0 (subfunction).

**Output parameters:**
- Word 4 = Status return, values are:
  - 1: Connection not finished.
  - 4: Not reserved for outgoing calls.
  - 5: Connection already established.
  - 6: Request outstanding.

### Subfunction code 1:

**Subfunction description:**
Check status of a connection request for a NIU.

**Input parameters:**
- Word 1 = logical device number.
- Word 2 = Length of name string.
- Word 3 = Pointer to name string.
- Word 4 = 1 (subfunction).

**Output parameters:**
- Word 4 = Status return, values are:
  - 0: Connection OK.
  - 1: Connection not finished.
  - 2: Resource not found.
  - 3: Resource is busy.
  - 4: Not reserved for outgoing calls.
  - 7: Connection not requested.

## Function code 20s:

**Function description:**
Disconnect a specified NIU.  
Only available in the VSX-version.  
This function has two different subfunctions; one to start the disconnect request, the other to test the status of the request.

Norsk Data NO-60.230.5 EN

---

## Page 57

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### Subfunction code 0:

**Subfunction description:**

Initiate a disconnect request for a NIU.

**Input parameters:**

Word 1 = Logical device number.  
Word 2 = 0 (subfunction).

**Output parameters:**

Word 2 = Status return, values are:  
1: Disconnection not finished.  
2: No connection open.  
3: Not reserved for outgoing calls.  
4: Request outstanding.

### Subfunction code 1:

**Subfunction description:**

Check status of a disconnect request for a NIU.

**Input parameters:**

Word 1 = Logical device number.  
Word 2 = 1 (subfunction).

**Output parameters:**

Word 2 = Status return, values are:  
0: Disconnection OK.  
1: Disconnection not finished.  
3: Not reserved for outgoing calls.  
5: Disconnection not requested.

## Function code 21a:

**Function description:**

Read/write on a PIO interface.  
This function has two subfunctions;  
one to read from the interface, the other to write to it.

### Subfunction code 0:

**Subfunction description:**

Read from a PIO interface.

**Input parameters:**

Word 1 = Logical device number.  
Word 2 = 0 (subfunction).  
Word 3 = Hardware register number (0-7).

**Output parameters:**

Word 4 = Register value

---

## Page 58

# Subfunction code 1

**Subfunction description:**  
Write to a PIO interface.

**Input parameters:**

| Word | Description                          |
|------|--------------------------------------|
| 1    | Logical device number.               |
| 2    | 1 (subfunction).                     |
| 3    | Hardware register number (0-7).      |
| 4    | Register value                       |

**Output parameters:**  
None.

# Function code 22a

**Function description:**  
Get the magic number of a TAD.  
Only available in the VSX-version.

**Input parameters:**

| Word | Description            |
|------|------------------------|
| 1    | Logical device number. |

**Output parameters:**

| Word | Description                |
|------|----------------------------|
| 2    | First part of magic number |
| 3    | Second part of magic number|

**Rules:**

1. Always allowed for users RT and SYSTEM.
2. Allowed for public users if both the input and output parts of the device are reserved.
3. There is no checks on whether the TAD has a connection or not.

# Function code 23a

**Function description:**  
Access the CI window on a NOTS controller.  
Only available in the VSX-version.  
This function consists of several subfunctions specified in input parameter word 2.

## Subfunction code 0

**Subfunction description:**  
Activate the CI window of a NOTS controller.

**Input parameters:**

| Word | Description            |
|------|------------------------|
| 1    | Logical device number. |
| 2    | 0 (subfunction).       |

**Output parameters:**  
None.

---

## Page 59

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### Subfunction code 1:

**Subfunction description:**  
Deactivate the CI window of a NOTS controller.

**Input parameters:**  
Word 1 = logical device number.  
Word 2 = 1 (subfunction).

**Output parameters:**  
None.

### Subfunction code 2:

**Subfunction description:**  
Read from the CI window of a NOTS controller.

**Input parameters:**  
Word 1 = logical device number.  
Word 2 = 2 (subfunction).  
Word 3 = displacement within the CI window.

**Output parameters:**  
Word 4 = value read from the CI window.

### Subfunction code 3:

**Subfunction description:**  
Write to the CI window of a NOTS controller.

**Input parameters:**  
Word 1 = logical device number.  
Word 2 = 3 (subfunction).  
Word 3 = displacement within the CI window.  
Word 4 = value to write to the CI window.

**Output parameters:**  
None.

### Function code 248:

**Function description:**  
Get the noreserve status of a device.  
Only available in the VSX-version.

**Input parameters:**  
Word 1 = logical device number.

**Output parameters:**  
Word 2 = the noreserve status of the output part of the device.

| Status | Description  |
| ------ | ------------ |
| 0      | noreserve off |
| 1      | noreserve on  |

**Rules:**  
1. Only allowed for users RT and SYSTEM.  
2. Allowed on terminals, TADs, MTADs and Net/One terminals.

---

## Page 60

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### Function code 25ₐ:

**Function description:**  
Set (or reset) the noreserve status of a device.  
Only available in the VSX-version.

**Input parameters:**

| Word | Description |
|------|-------------|
| 1    | logical device number. |
| 2    | 0 : Reset the noreserve status of the output part of device. |
|      | 1 : Set the noreserve status of the output part of device. |

**Output parameters:**  
None.

**Rules:**  
1. Only allowed for users RT and SYSTEM.  
2. Allowed on terminals, TADs, MTADs and Net/One terminals.

### Function code 100ₐ:

**Function description:**  
Return function parameters as set by one of the functions 101-177.  
Values from SINTRAN memory, image and save areas are returned.

The functions 101 to 177 have the same call format:

| Word | Description |
|------|-------------|
| 1    | logical device number. |
| 2    | area to be updated. |
| 3+   | parameters for specific functions. |

When returning function parameters, word 3 is returned with number of the parameters returned from each area (value 1, 2, or 3):

- **Value 1:** Value of word 3 as set by specific function is returned from SINTRAN memory, image and save area (e.g., function 101).
- **Value 2:** Value of word 3 and 4 as set by specific function is returned from SINTRAN memory, image and save area (e.g., function 102).
- **Value 3:** Value of word 3, 4, and 5 as set by specific function is returned from SINTRAN memory, image and save area (e.g., function 103).

**Input parameters:**

| Word | Description |
|------|-------------|
| 1    | logical device number. |
| 2    | function code to return function parameters from. |

Norsk Data ND-60.230.5 EN

---

## Page 61

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### Output Parameters

#### Case 1:
- Word 1 = unchanged.
- Word 2 = unchanged.
- Word 3 = 1
- Word 4 = memory area (value set from input word 3 to other functions).
- Word 5 = image area (value set from input word 3 to other functions).
- Word 6 = save area (value set from input word 3 to other functions).

#### Case 2:
- Word 1 = unchanged.
- Word 2 = unchanged.
- Word 3 = 2
- Word 4 = memory area (value set from input word 3 to other functions).
- Word 5 = memory area (value set from input word 4 to other functions).
- Word 6 = image area (value set from input word 3 to other functions).
- Word 7 = image area (value set from input word 4 to other functions).
- Word 8 = save area (value set from input word 3 to other functions).
- Word 9 = save area (value set from input word 4 to other functions).

#### Case 3:
- Word 1 = unchanged.
- Word 2 = unchanged.
- Word 3 = 3
- Word 4 = memory area (value set from input word 3 to other functions).
- Word 5 = memory area (value set from input word 4 to other functions).
- Word 6 = memory area (value set from input word 5 to other functions).
- Word 7 = image area (value set from input word 3 to other functions).
- Word 8 = image area (value set from input word 4 to other functions).
- Word 9 = image area (value set from input word 5 to other functions).
- Word 10 = save area (value set from input word 3 to other functions).
- Word 11 = save area (value set from input word 4 to other functions).
- Word 12 = save area (value set from input word 5 to other functions).

**Note:**  
Values from SINTRAN image and save areas are only returned for user SYSTEM.

### Function Code 1018:

#### Function Description:
Set terminal type.  
See also MSTTY (MON 17).

#### Input Parameters:

| Word | Description                                                                            |
|------|----------------------------------------------------------------------------------------|
| 1    | Logical device number.                                                                 |
| 2    | AREA (see table on page 28).                                                           |
| 3    | Terminal type, as A-reg in MSTTY (MON 17).                                             |

#### Rules:
1. If logical device number is a TAD, the SINTRAN image and save areas cannot be updated.

---

## Page 62

# SINTRAN III Release Information, K-Version
## Monitor Calls (ND-100)

### Function code 102a:

**Function description:**

Set escape and local character.  
See also MSD@E (MON 227).

**Input parameters:**

| Word | Description |
|------|-------------|
| 1    | Logical device number. |
| 2    | AREA (see table on page 28). |
| 3    | Escape character, -1 = no changes. |
| 4    | Local character, -1 = no changes. |

**Rules:**
1. If logical device number is a TAD, the SINTRAN image and save areas cannot be updated.

### Function code 103a:

**Function description:**

Start and stop Xon/Xoff protocol input control.

**Input parameters:**

| Word | Description |
|------|-------------|
| 1    | Logical device number. |
| 2    | AREA (see table on page 28). |
| 3    | Start/stop Xon/Xoff protocol: |
|      | 0 = start Xon/Xoff protocol |
|      | 1 = stop Xon/Xoff protocol |
| 4    | Xon character, -1 = no changes. |
| 5    | Xoff character, -1 = no changes. |

**Rules:**
1. New Xon/Xoff characters can be defined only when start Xon/Xoff protocol is selected (word 3 = 0).

### Function code 104a:

**Function description:**

Start and stop Xon/Xoff protocol, output control.

**Input parameters:**

| Word | Description |
|------|-------------|
| 1    | Logical device number. |
| 2    | AREA (see table on page 28). |
| 3    | Start/stop Xon/Xoff protocol: |
|      | 0 = start Xon/Xoff protocol |
|      | 1 = stop Xon/Xoff protocol |
| 4    | Xon character, -1 = no changes. |
| 5    | Xoff character, -1 = no changes. |

**Rules:**
1. New Xon/Xoff characters can be defined only when start Xon/Xoff protocol is selected (word 3 = 0).

**Notes:**
1. Whenever output control functions are used in memory area, output driver is restarted if currently in stop (Xoff).

---

## Page 63

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### Function code 105a:

**Function description:**

Set Xon/Xoff only or dual function Xon/Xoff control.  
- Xon/Xoff only means ordinary Xon/Xoff protocol.  
- Dual function Xon/Xoff is an alternative to normal Xon/Xoff checking and requires Xon/Xoff characters to be in sequence, any other combination will be treated as normal data characters.

**Input parameters:**

- Word 1 = logical device number.  
- Word 2 = AREA (see table on page 28).  
- Word 3 = type of Xon/Xoff control:  
  - 0 = Xon/Xoff only.  
  - 1 = Dual function Xon/Xoff.

### Function code 106a:

**Function description:**

Set character length.  
If memory area is updated, then the function immediately set to terminal interface (IOXT) is executed.

**Input parameters:**

- Word 1 = logical device number.  
- Word 2 = AREA (see table on page 28).  
- Word 3 = character length:  
  - 0 = 8-bit character length  
  - 1 = 7-bit character length  
  - 2 = 6-bit character length  
  - 3 = 5-bit character length.  
- Word 4 = parity:  
  - 0 = no parity  
  - 1 = even parity.

### Function code 107a:

**Function description:**

Set baud rate on terminal.  
If memory area is updated, the function immediately set to terminal interface (IOXT) is executed.  
When returning function parameters for this function, words 3 and 4 will be contain the value 0 if no baud rate is specified in software.

**Input parameters:**

- Word 1 = logical device number.  
- Word 2 = AREA (see table on page 28).  
- Word 3 = Baud rate input, 0= no changes.  
- Word 4 = Baud rate output, 0= no changes.

---

## Page 64

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### Rules:

1. Not legal for device number 1 (console).
2. Baud rate will not be changed in SINTRAN memory area if no baud rate is specified in software (data field).
3. If no baud rate is specified in SINTRAN image/save areas, then both input and output baud rate must be specified.
4. 19200 baud only legal if buffer terminal interface with FIFO.

The following baud rates can be specified (decimal):

50, 75, 110, 134 (134.5), 150, 200, 300, 600, 1200, 1800, 2400, 4800, 9600, 19200.

### Notes:

1. Only the interface ND-102740 is currently supporting 19200 baud.
   If this function is used on the interface ND-102730, the baud rate will be set to 100 baud (refer to the manual "8-Terminal Buffer Interface with FIFO", ND-11.022.1 EN for further details).

### Function code 110a:

**Function description:**

Set number of stop bits.
If memory area is updated, then the function immediately set to terminal interface (IOXT) is executed.

**Input parameters:**

| Word | Description |
|------|-------------|
| 1    | Logical device number. |
| 2    | AREA (see table on page 28). |
| 3    | Number of stop bits: |
|      | 0 = 1.5 stop bit for 5-bit characters, 2 stop bits else. |
|      | 1 = 1 stop bit. |

### Function code 111a:

**Function description:**

Set terminal to printer or reset printer to terminal.
If set terminal to printer, then logical device number specified in word 1 will no longer act as a terminal. If reset from printer to terminal, then logical device will act as ordinary terminal again.
See also function code 12.

**Input parameters:**

| Word | Description |
|------|-------------|
| 1    | Logical device number. |
| 2    | AREA (see table on page 28). |
| 3    | Set/reset terminal to printer: |
|      | 0 = set terminal to printer (remove terminal data field from background table). |
|      | 1 = reset from printer to terminal (reinsert terminal data field into background table). |
| 4    | Character length. |
|      | 0 = 7-bit character length. |
|      | 1 = 8-bit unmodified input and output. |

---

## Page 65

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### Rules:
1. Only allowed for user SYSTEM.
2. Both input and output part of logical device must be reserved (even if user SYSTEM).
3. Only legal for terminals with 8-bit character length to set 8-bit unmodified input/output.
4. Only legal to set 8-bit unmodified input/output when function is set terminal to printer (word 3 = 0).

### Function code 112a:

#### Function description:
Set half or full duplex on terminal.  
Half duplex means that no echo is given to terminal when typing on the keyboard.

#### Input parameters:
- **Word 1** = logical device number.
- **Word 2** = AREA (see table on page 28).
- **Word 3** = duplex function: 
  - 0 = half duplex
  - 1 = full duplex

### Function code 113a:

#### Function description:
Enable/disable login with variable terminal speed.  
When login with variable speed is enabled, the terminal is enabled to log in if speed setting is one of: 9600, 4800, 2400, 1800, 1200, 600, 300, 200, 150, 134.5, 110, 75 or 50 baud. The user should press <ESCAPE> in intervals of 2-3 seconds until SINTRAN has typed "ENTER:".  
If a user has not logged into SINTRAN within 30 seconds after "ENTER:" has been typed, the terminal will be aborted.

#### Input parameters:
- **Word 1** = logical device number.
- **Word 2** = AREA (see table on page 28).
- **Word 3** = enable login with variable speed:
  - 0 = enable login with variable speed.
  - 1 = disable login with variable speed.

#### Rules:
1. Not legal for device number 1 (console).
2. Baud rate must be set by software (TSPEED in data field # -1).
3. Escape character must have ASCII value 33.

### Function code 114a:

#### Function description:
Set/reset printer connected to terminal.

#### Input parameters:
- **Word 1** = logical device number.
- **Word 2** = AREA (see table on page 28).
- **Word 3** = 
  - 0 = set terminal connected to printer.
  - 1 = reset terminal connected to printer.

---

## Page 66

# Function code 2008:

## Function description:
Set NOTS configuration.  
Only available in the VSX-version.

## Input parameters:
- Word 1 = NOTS number (controller number corresponding to thumbwheel setting on the controller card)
- Word 2 = AREA (see table on page 28)
- Word 3 = Pointer to name string
- Word 4 = Length of name string
- Word 5 = Number of outgoing lines
- Word 6 = Controller flag:

  | Bit No. | Description                                         |
  |---------|-----------------------------------------------------|
  | 0       | Disconnect on logout                                |
  | 1       | Do not reset terminal type on connect               |
  | 2       | 8-bit I/O is set on incoming lines                  |
  | 3       | 8-bit I/O is set on outgoing lines                  |

## Output parameters:
None.

## Rules:
1. Only allowed for user SYSTEM.
2. Update on SINTRAN III memory area is not allowed.
3. Maximum length of name string is 20 characters.
4. Several NOTS numbers (controllers) can be given the same name.

# Function code 2018:

## Function description:
Get NOTS configuration.  
Only available in the VSX-version.

## Input parameters:
- Word 1 = NOTS number.
- Word 2 = AREA (see table on page 28).
- Word 3 = Pointer to name string.

## Output parameters:
- Word 3 = Pointer to name string.
- Word 4 = Length of name string.
- Word 5 = Number of outgoing lines.
- Word 6 = Controller flag:

  | Bit No. | Description                                         |
  |---------|-----------------------------------------------------|
  | 0       | Disconnect on logout                                |
  | 1       | Do not reset terminal type on connect               |
  | 2       | 8-bit I/O is set on incoming lines                  |
  | 3       | 8-bit I/O is set on outgoing lines                  |

## Rules:
1. Only allowed for user SYSTEM.
2. Only area values 1, 2, and 4 are allowed.

---

## Page 67

# SINTRAN III RELEASE INFORMATION, K-VERSION  
MONITOR CALLS (ND-100)

## Function code 202a:

**Function description:**  
Get information about a NOTS line.  
Only available in the VSX-version.

**Input parameters:**  
Word 1 = NOTS number.  
Word 2 = Line number.

**Output parameters:**  
Word 3 = Logical device number.  
Word 4 = Status word, the following bits are defined:  
- bit 0 : CI window.  
- bit 1 : Reserved for outgoing calls.  
- bit 2 : Connection open on this line.  
- bit 3 : Request outstanding  

## Function code 203a:

**Function description:**  
Restart/reload a NOTS.  
Only available in the VSX-version.  
This function has two different subfunctions; one to restart a NOTS, the other to reload it.

### Subfunction code 0:

**Subfunction description:**  
Restart a NIU.

**Input parameters:**  
Word 1 = NOTS number.  
Word 2 = 0 (subfunction).

**Output parameters:**  
None.

**Rules:**  
1. Allowed for user SYSTEM only.

### Subfunction code 1:

**Subfunction description:**  
Reload a NIU.

**Input parameters:**  
Word 1 = NOTS number.  
Word 2 = 1 (subfunction).

**Output parameters:**  
None.

**Rules:**  
1. Allowed for user SYSTEM only.

Norsk Data ND-60.230.5 EN  
Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 68

# Function code 300₈:

## Function description:
Set SCSI device definition.  
Only available in the VSX-version.  
Establish the relation between a device (magnetic disk unit, streamer tape unit, magnetic tape unit or optical disk unit) connected to a SCSI controller and an appropriate device number in SINTRAN III.

## Input parameters:
- **Word 1** = SINTRAN III logical device number (560₈, 1111₈, 1224₈, 1231₈, 2210₈-2225₈, 2232₈-2235₈)
- **Word 2** = SCSI adaptor logical device number (2202₈-2205₈)
- **Word 3** = SCSI ID number (0-7)

## Output parameters:
None.

## Rules:
1. Allowed for user SYSTEM only.  
2. Only the SINTRAN image and save areas may be accessed.

# Function code 301₈:

## Function description:
Get SCSI device definition.  
Only available in the VSX-version.  
Return information about the relation between a device (magnetic disk unit, streamer tape unit, magnetic tape unit or optical disk unit) connected to a SCSI controller and a SINTRAN III device number.

## Input parameters:
- **Word 1** = SINTRAN III logical device number (560₈, 1111₈, 1224₈, 1231₈, 2210₈-2225₈, 2232₈-2235₈)

## Output parameters:
- **Word 1** = unchanged
- **Word 2** = Logical Unit Number generated
- **Word 3** = SCSI adaptor logical device number (2202₈-2205₈)
- **Word 4** = SCSI ID number (0-7)

## Rules:
1. Allowed for user SYSTEM only.  
2. Only the SINTRAN image and save areas may be accessed.

---

## Page 69

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### Function code 3028:

#### Function description:
Delete SCSI device definition.  
Only available in the VSX-version.  
Remove a previously established relation between a device (magnetic disk unit, streamer tape unit, magnetic tape unit or optical disk unit) connected to a SCSI controller and a device number in SINTRAN III.

#### Input parameters:
Word 1 = SINTRAN III logical device number (560₈, 1111₈, 1224₈, 1231₈, 2210₈-2225₈, 2232₈-2235₈)

#### Output parameters:
None.

#### Rules:
1. Allowed for user SYSTEM only.  
2. Only the SINTRAN image and save areas may be accessed.

---

### Function code 3038:

#### Function description:
Get current SCSI device definition.  
Only available in the VSX-version.  
Return information about the relation between a device (magnetic disk unit, streamer tape unit, magnetic tape unit or optical disk unit) connected to a SCSI controller and a SINTRAN III device number.

#### Input parameters:
Word 1 = SINTRAN III logical device number (560₈, 1111₈, 1224₈, 1231₈,  
2210₈-2225₈, 2232₈-2235₈)

#### Output parameters:

| Word | Description                                      |
|------|--------------------------------------------------|
| 1    | unchanged                                        |
| 2    | Logical Unit Number generated                    |
| 3    | SCSI adaptor logical device number (2202₈-2205₈) |
| 4    | SCSI ID number (0-7)                             |

#### Rules:
1. Allowed for user SYSTEM only.  
2. Only the current SINTRAN memory area may be accessed.

---

Norsk Data ND-60.230.5 EN

---

## Page 70

# 3.2 New Monitor Calls

## 3.2.1 RSREC MON 340

**Purpose:** Read system record.

**Monitor call format:**

```
LDA RECTP     % A = record type
LDT RTADR     % T = RT-description / segment number
LDX (BUFFR    % X = address of buffer
MON 340       % RSREC
JMP ERROR     % Error return
.........     % Normal return

RECTP, 1
RTADR, 54214
BUFFR, 0; *+26/ % Reserve 26α words
```

**Input parameters:**

| Register    | Description                                                     |
|-------------|-----------------------------------------------------------------|
| A-register  | Record type: 1 = RT-description, 2 = Segment entry              |
| T-register  | RT-description address or Segment number                        |
| X-register  | Address of buffer to receive system record. This buffer must have a minimum length of 381ο words (function = 1) or 81ο words (function = 2) |

**Output parameters:**

- **Return:** Error, A-register contains error code (one of 153, 174 or 201)
- **Skip return:** OK, the system record is read into the specified buffer. If function = 1, the A-register contains the number of devices connected with MON CONCT (MON 106).

## 3.2.2 SGMΤΥ MON 341

**Purpose:** SGMΤΥ is used to change the active segments of a program and/or the page index tables:

- MON SGMΤΥ is meant to replace the older monitor calls MCALL (MON 132) and ΜΕXΙΤ (MON 133) on the VSΧ version.
- In MON MCALL and MON ΜΕXΙΤ, the segment numbers are restricted to 8 bits (values 0-255).

**Monitor call format:**

```
LDT (PARLI     % T = address of parameter list
MON 341        % SGMΤΥ

PARLI, (FUNC   % Function code
(ADDR          % Start/return address
(SEG1          % New segment 1
(SEG2          % New segment 2
(PITS          % New PITS
```

---

## Page 71

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## MONITOR CALLS (ND-100)

### Input parameters:
FUNC =  
0 - MCALL  
1 - MEXIT  
2 - MCALL w/PIT change  
3 - MEXIT w/PIT change  
4 - REMSG (remove segment entered by ENTSG)  

For function 4 (REMSG), parameters 2 (ADDR), 4 (SEG2) and 5 (PITS) **must** be zero.

### Output parameters:
Functions 0 and 2 (MCALL):
- T = Old segment 1  
- D = Old segment 2  
- L = Return address  
- X = Old PITs if PITs changed (function 2)  

Functions 1 and 3 (MEXIT): No register change  

Function 4 (REMSG):  
A = 0 : ok  
-1 : segment not entered  
-2 : illegal segment number  
-3 : no segments entered  
-4 : illegal parameter  

### 3.2.3 ADP MON 342

**Purpose:**  
MON ADP is used to handle a system LAMU from a program or a set of programs. It is intended for use by ADP software, containing commonly used routines. MON ADP is reserved for internal use by ND.

**Monitor call format:**

```
LDT FUNCT     % T = function code  
LDA ..        % A = function dependent parameter  
LDX ..        % X = function dependent parameter  
MON 342       % ADP  
.........     % return  
```

The following functions are available:

| Function | Brief explanation                                           |
|----------|-------------------------------------------------------------|
| 1        | Go to program LAMU                                          |
| 2        | Go to subsystem                                             |
| 3        | Connect to mailbox LAMU (create it if necessary)            |
| 4        | Disconnect from mailbox LAMU                                |
| 5        | Delete mailbox LAMU                                         |
| 6        | Create program LAMU (a new system LAMU)                     |
| 7        | Delete program LAMU (a system LAMU)                         |
| 10₈      | Connect to program LAMU (a system LAMU)                     |
| 11₈      | Disconnect from program LAMU (a system LAMU)                |
| 12₈      | Set write protection on program LAMU                        |
| 13₈      | Clear write protection on program LAMU                      |
| 14₈      | Disconnect from both mailbox LAMU and program LAMU          |

MON 342 is only allowed from background programs.

Norsk Data N0-60.220.5 EN

---

## Page 72

# SINTRAN III RELEASE INFORMATION, K-VERSION
## MONITOR CALLS (ND-100)

The following error codes can be returned:

| A-reg. | Explanation                            |
|--------|----------------------------------------|
| 25     | You are not authorized to do this      |
| 201    | Illegal function code                  |
| 373    | Illegal program LAMU identifier        |
| 374    | Program LAMU already exists            |
| 375    | No such program LAMU                   |
| 376    | Illegal program LAMU size              |
| 377    | Program LAMU not connected             |
| 3200   | Not allowed now                        |

## Function code 1:

### Function description:
Go to program LAMU.

### Input parameters:
- T-register = 1
- A-register = program LAMU number (bits 15a-12a)
  - routine number (bits 11a-0)
- X-register = address of parameters in mailbox LAMU

### Output parameters:
- T-register = unchanged
- A-register = unchanged
- X-register = unchanged
- L-register = return address after monitor call

### Rules:
1. Permitted from all users.
2. If an error occurs in function 1, the program will be aborted.

## Function code 2:

### Function description:
Go to subsystem.

### Input parameters:
- T-register = 2
- A-register = return address
- X-register = address of parameters in mailbox LAMU

### Output parameters:
- T-register = unchanged
- A-register = unchanged
- X-register = unchanged

### Rules:
1. Permitted from all users.
2. If an error occurs in function 2, the program will be aborted.

---

## Page 73

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### Function code 3:

**Function description:**  
Connect mailbox LAMU (create it if necessary).

**Input parameters:**  
T-register = 3  
A-register = not used

**Output parameters:**  
T-register = unchanged  
A-register = status (0 = ok)

**Rules:**  
1. Permitted from all users.

### Function code 4:

**Function description:**  
Disconnect mailbox LAMU.

**Input parameters:**  
T-register = 4  
A-register = not used

**Output parameters:**  
T-register = unchanged  
A-register = status (0 = ok)

**Rules:**  
1. Permitted from all users.

### Function code 5:

**Function description:**  
Delete mailbox LAMU.

**Input parameters:**  
T-register = 5  
A-register = not used

**Output parameters:**  
T-register = unchanged  
A-register = status (0 = ok)

**Rules:**  
1. Only permitted internally from SINTRAN III.

---

## Page 74

# Function code 6

**Function description:**
Create program LAMU.

**Input parameters:**
- T-register = 6
- A-register = program LAMU number
- X-register = program LAMU size in pages

**Output parameters:**
- T-register = unchanged
- A-register = status (0 = ok)
- X-register = unchanged

**Rules:**
1. Permitted from user SYSTEM only.

# Function code 7

**Function description:**
Delete program LAMU.

**Input parameters:**
- T-register = 7
- A-register = program LAMU number

**Output parameters:**
- T-register = unchanged
- A-register = status (0 = ok)

**Rules:**
1. Permitted from user SYSTEM only.

# Function code 10a

**Function description:**
Connect program LAMU as data bank.

**Input parameters:**
- T-register = 10
- A-register = program LAMU number

**Output parameters:**
- T-register = unchanged
- A-register = status (0 = ok)

**Rules:**
1. Permitted from user SYSTEM only.

---

## Page 75

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### Function code 11a:

**Function description:**  
Disconnect program LAMU.

**Input parameters:**  
T-register = 11  
A-register = program LAMU number

**Output parameters:**  
T-register = unchanged  
A-register = status (0 = ok)

**Rules:**  
1. Permitted from user SYSTEM only.

### Function code 12a:

**Function description:**  
Write protect program LAMU.

**Input parameters:**  
T-register = 12  
A-register = program LAMU number

**Output parameters:**  
T-register = unchanged  
A-register = status (0 = ok)

**Rules:**  
1. Permitted from user SYSTEM only.  
2. Must be done before a program LAMU can be used.

### Function code 13a:

**Function description:**  
Write permit program LAMU.

**Input parameters:**  
T-register = 13  
A-register = program LAMU number

**Output parameters:**  
T-register = unchanged  
A-register = status (0 = ok)

**Rules:**  
1. Permitted from user SYSTEM only.

---

## Page 76

# Function Code 14

**Function Description:**  
Disconnect both mailbox LAMU and current program LAMU.

**Input Parameters:**  
- T-register = 14  
- A-register = not used  

**Output Parameters:**  
- T-register = unchanged  
- A-register = status (0 = ok)  

**Rules:**  
1. Only permitted internally from SINTRAN III.

## 3.2.4 CONFG MON 343

**Purpose:** CONFG is used to read and/or change configuration parameters for SINTRAN III/VSX.

**Monitor Call Format:**

```
LDA {PARLI}    % A = address of parameter list
MON 343        % CONFG
JMP ERROR      % Error handling
........       % Normal return
```

**PARLI:**

- `{FUNC}`  % Function code
- `{INDEX}` % Configuration parameter number
- `{SUBIN}` % Subindex (only used for some values of INDEX)
- `{VALUE}` % Input and/or output value (integer or string)

**Function Codes:**

| FUNC | Description                                                                                 |
|------|---------------------------------------------------------------------------------------------|
| 1    | (Save) Read value from SINTRAN III save area (next value to be used after a cold start)     |
| 2    | (Read) Read current active value                                                            |
| 3    | (Write) Write value to SINTRAN III save area (next value to be used after a cold start)     |
| 4    | (Generated) Read generated value                                                            |
| 5    | (Free) Read currently unused units                                                          |
| 6    | (Special) Parameter dependent                                                               |

MON 343 is restricted to user SYSTEM only, and is allowed from RT-programs.

Note that MON 343 is generally intended to be used by the reconfiguration program (S3-CONFIG).

Also note that only some functions are available in the VSE-version.

Furthermore, note that a cold start is necessary to make changes come into effect (all changes are made as "write new value to save area").

---

## Page 77

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

The following configuration parameters can be specified (note that only a few functions are available on the VSE-version):

### Possible functions: Save/Read/Write/Gen/Free/Special

| Number | Parameter name                                     | VSX   | VSE |
|--------|----------------------------------------------------|-------|-----|
| 0      | Standard system                                    | RG    | G   |
| 1      | Number of device buffers                           | SRWG  | G   |
| 2      | First legal physical page for device buffers       | SRW   |     |
| 3      | Number of TADs                                     | SRWG  | G   |
| 4      | Number of batch processors                         | SRWG  | G   |
| 5      | Number of spooling programs                        | SRWG  | G   |
| 6      | Number of background programs                      | SRWG  | G   |
| 7      | Number of background programs in use               | R     |     |
| 10a    | Number of Symbolic Debugger segments               | SRWG  | G   |
| 11a    | Number of ND-500 processes                         | SRWG  | G   |
| 12a    | Number of remote file access segments              | SRWG  | G   |
| 13a    | Spooling queue size in pages                       | SRW   | G   |
| 14a    | System segment size                                | SRW   | G   |
| 15a    | Mon ADP (MON 341)                                  | SRWG  | G   |
| 16a    | Background allocation                              | G     | G   |
| 17a    | COSMOS spooling                                    | G     | G   |
| 20a    | Number of user RT-programs                         | GF    | G   |
| 21a    | Number of user segments                            | GF    | G   |
| 22a    | Number of terminals                                | RG    | G   |
| 23a    | Number of Telefix devices                          | G     | G   |
| 24a    | Number of semaphores                               | G     | G   |
| 25a    | Number of internal devices (total)                 | G     | G   |
| 26a    | Number of internal devices (block)                 | G     | G   |
| 27a    | Number of SIBAS processes                          | G     | G   |
| 30a    | Number of open file entries                        | R     | R   |
| 31a    | Number of allocated areas                          | SRW   | G   |
| 32a    | Mon MLAMU (MON 315)                                | G     | G   |
| 33a    | Maximum number of LAMUs                            | SRW   |     |
| 34a    | Maximum number of LAMUs per program                | SRW   |     |
| 35a    | Maximum number of system LAMUs                     | SRW   |     |
| 36a    | ND-500 software versions                           | R     |     |
| 37a    | ND-500 microcode version (per CPU)                 | R     |     |
| 40a    | ND-110 / ND-120 microprogram version               | R     |     |
| 41a    | Memory configuration                               | R     |     |
| 42a    | Define HDLC-interfaces as HDLC or modem            | SRWP  |     |
| 43a    | Number of HDLC connections                         | G     |     |
| 44a    | Number of synchronous modems on HDLC               | G     |     |
| 45a    | Number of X.21 connections                         | SRWG  |     |
| 46a    | Define spooling device number                      | SRW   |     |
| 47a    | Define printer type                                | SRWP  |     |
| 50a    | Number of Fast UDMA RT-programs                    | SRWG  |     |

---

## Page 78

# SINTRAN III RELEASE INFORMATION, K-VERSION
## MONITOR CALLS (ND-100)

The following error codes can be returned:

### A-reg. Explanation

| Code | Explanation |
|------|-------------|
| 25   | You are not authorized to do this |
| 33   | No such logical unit |
| 174  | Illegal parameter |
| 201  | Illegal function code <br> (which means "illegal function code for this index") |
| 3201 | Illegal index <br> (which means "illegal index or subindex") |

Other interpretations of error codes 201 and 3201 are given when applicable.

## Configuration parameter: 0

**Parameter name:**

Standard system

**Input parameters:**
- FUNC = Function, see below.
- INDEX = 0
- SUBIN = Subindex not used for this configuration parameter
- VALUE = Input value not used for this configuration parameter

**Output parameters:**
- VALUE = work mode version (if function = Read current active value) <br>
  standard system (if function = Read generated value): 
  - 1 = standard system – all disk types are included 
  - 0 = system is generated to meet specific configuration requirements

**Functions allowed for this parameter:**
- Read current active value
- Read generated value

## Configuration parameter: 1

**Parameter name:**

Number of device buffers

**Input parameters:**
- FUNC = Function, see below.
- INDEX = 1
- SUBIN = Subindex not used for this configuration parameter
- VALUE = Number of device buffers

**Output parameters:**
- VALUE = Number of device buffers

---

## Page 79

# SINTRAN III RELEASE INFORMATION, K-VERSION
## MONITOR CALLS (ND-100)

### Functions allowed for this parameter:
- Read save area (next value)
- Read current active value
- Write to save area (new next value)
- Read generated value

## Configuration parameter: 2

**Parameter name:**  
First legal physical page for device buffers

**Input parameters:**

| Parameter | Description |
|-----------|-------------|
| FUNC      | Function, see below. |
| INDEX     | 2           |
| SUBIN     | Subindex not used for this configuration parameter |
| VALUE     | First legal physical page for device buffers |

**Output parameters:**

| Parameter | Description |
|-----------|-------------|
| VALUE     | First legal physical page for device buffers |

### Functions allowed for this parameter:
- Read save area (next value)
- Read current active value
- Write to save area (new next value)

## Configuration parameter: 3

**Parameter name:**  
Number of TADs

**Input parameters:**

| Parameter | Description |
|-----------|-------------|
| FUNC      | Function, see below. |
| INDEX     | 3           |
| SUBIN     | Subindex not used for this configuration parameter |
| VALUE     | Number of TADs |

**Output parameters:**

| Parameter | Description |
|-----------|-------------|
| VALUE     | Number of TADs |

### Functions allowed for this parameter:
- Read save area (next value)
- Read current active value
- Write to save area (new next value)
- Read generated value

Norsk Data ND-60.230.5 EN  
Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 80

# Configuration parameter: 4

**Parameter name:  
Number of batch processors**

**Input parameters:**  
FUNC = Function, see below.  
INDEX = 4  
SUBIN = Subindex not used for this configuration parameter  
VALUE = Number of batch processors

**Output parameters:**  
VALUE = Number of batch processors

**Functions allowed for this parameter:**  
- Read save area (next value)  
- Read current active value  
- Write to save area (new next value)  
- Read generated value

# Configuration parameter: 5

**Parameter name:  
Number of spooling programs**

**Input parameters:**  
FUNC = Function, see below.  
INDEX = 5  
SUBIN = Subindex not used for this configuration parameter  
VALUE = Number of spooling programs

**Output parameters:**  
VALUE = Number of spooling programs

**Functions allowed for this parameter:**  
- Read save area (next value)  
- Read current active value  
- Write to save area (new next value)  
- Read generated value

# Configuration parameter: 6

**Parameter name:  
Number of background programs.**

**Input parameters:**  
FUNC = Function, see below.  
INDEX = 6  
SUBIN = Subindex not used for this configuration parameter  
VALUE = Number of background programs

**Output parameters:**  
VALUE = Number of background programs

---

## Page 81

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## MONITOR CALLS (ND-100)

**Functions allowed for this parameter:**
- Read save area (next value) (*)  
- Read current active value (*)  
- Write to save area (new next value) (*)  
- Read generated value  

The three functions marked (*) are only available when Background Allocation is present (see configuration parameter 16a).

## Configuration parameter: 7

**Parameter name:**  
Number of background programs in use

**Input parameters:**  
FUNC = Function, see below.  
INDEX = 7  
SUBIN = Subindex not used for this configuration parameter  
VALUE = Input value not used for this configuration parameter

**Output parameters:**  
VALUE = Number of background programs in use

**Functions allowed for this parameter:**  
Read current active value

## Configuration parameter: 10a

**Parameter name:**  
Number of Symbolic Debugger segments

**Input parameters:**  
FUNC = Function, see below.  
INDEX = 10  
SUBIN = Subindex not used for this configuration parameter  
VALUE = Number of Symbolic Debugger segments

**Output parameters:**  
VALUE = Number of Symbolic Debugger segments

**Functions allowed for this parameter:**  
- Read save area (next value)  
- Read current active value  
- Write to save area (new next value)  
- Read generated value

Norsk Data ND-60.230.5 EN  
Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 82

# Configuration parameter: 11a:

## Parameter name:
Number of ND-500 processes

### Input parameters:
- FUNC = Function, see below.
- INDEX = 11
- SUBIN = Subindex not used for this configuration parameter
- VALUE = Number of ND-500 processes

### Output parameters:
- VALUE = Number of ND-500 processes

### Functions allowed for this parameter:
- Read save area (next value)
- Read current active value
- Write to save area (new next value)
- Read generated value

# Configuration parameter: 12a:

## Parameter name:
Number of remote file access segments

### Input parameters:
- FUNC = Function, see below.
- INDEX = 12
- SUBIN = Subindex not used for this configuration parameter
- VALUE = Number of remote file access segments

### Output parameters:
- VALUE = Number of remote file access segments

### Functions allowed for this parameter:
- Read save area (next value)
- Read current active value
- Write to save area (new next value)
- Read generated value

# Configuration parameter: 13a:

## Parameter name:
Spooling queue size in pages for each spooling program. A queue size of 2 pages can contain 10 queue entries and each additional page will increase the queue length by approximately 7 new entries.

### Input parameters:
- FUNC = Function, see below.
- INDEX = 13
- SUBIN = Subindex not used for this configuration parameter
- VALUE = Spooling queue size in pages

---

## Page 83

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### Output parameters:
VALUE = Spooling queue size in pages

**Functions allowed for this parameter:**
- Read save area (next value)
- Read current active value
- Write to save area (new next value)

---

## Configuration parameter: 14

### Parameter name:
System segment size.

This size affects the number of open files for each user as follows: default (= minimum) system segment size (5) provides 48 file buffers, each additional page (up to total max. of 8) provides 16 buffers. Each open file uses 2 buffers if sequential access, 1 if random access. Maximum number of open files is 64.

### Input parameters:
- FUNC = Function, see below.
- INDEX = 14
- SUBIN = Subindex not used for this configuration parameter
- VALUE = System segment size

### Output parameters:
- VALUE = System segment size

**Functions allowed for this parameter:**
- Read save area (next value)
- Read current active value
- Write to save area (new next value)

---

## Configuration parameter: 15

### Parameter name:
Mon ADP (MON 341)

### Input parameters:
- FUNC = Function, see below.
- INDEX = 15
- SUBIN = Subindex not used for this configuration parameter
- VALUE = 
  - # 0 if MON ADP (MON 341) is to be available
  - 0 if MON ADP (MON 341) is to be unavailable
  - (only possible if this SINTRAN is generated with MON ADP)

### Output parameters:
- VALUE = 1 if MON ADP (MON 341) is available
- 0 if MON ADP (MON 341) is unavailable

**Functions allowed for this parameter:**
- Read save area (next value)
- Read current active value
- Write to save area (new next value)
- Read generated value

Norsk Data ND-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 84

# SINTRAN III RELEASE INFORMATION, K-VERSION
## MONITOR CALLS (ND-100)

### Configuration parameter: 16a:

**Parameter name:**  
Background allocation

**Input parameters:**  
FUNC = Function, see below.  
INDEX = 16  
SUBIN = Subindex not used for this configuration parameter  
VALUE = Input value not used for this configuration parameter  

**Output parameters:**  
VALUE = 1 if this SINTRAN is generated with Background Allocation  
        0 if this SINTRAN is not generated with Background Allocation  

**Functions allowed for this parameter:**  
Read generated value  

### Configuration parameter: 17a:

**Parameter name:**  
COSMOS spooling

**Input parameters:**  
FUNC = Function, see below.  
INDEX = 17  
SUBIN = Subindex not used for this configuration parameter  
VALUE = Input value not used for this configuration parameter  

**Output parameters:**  
VALUE = 1 if COSMOS spooling is available  
        0 if COSMOS spooling is unavailable  

**Functions allowed for this parameter:**  
Read generated value  

### Configuration parameter: 20a:

**Parameter name:**  
Number of user RT-programs

**Input parameters:**  
FUNC = Function, see below.  
INDEX = 20  
SUBIN = Subindex not used for this configuration parameter  
VALUE = Input value not used for this configuration parameter  

**Output parameters:**  
VALUE = Number of user RT-programs  

**Functions allowed for this parameter:**  
Read generated value  
Report currently free units (in memory)

---

## Page 85

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### Configuration parameter: 21a:

**Parameter name:**  
Number of user segments

**Input parameters:**  
FUNC = Function, see below.  
INDEX = 21  
SUBIN = Subindex not used for this configuration parameter  
VALUE = Input value not used for this configuration parameter

**Output parameters:**  
VALUE = Number of user segments

**Functions allowed for this parameter:**  
Read generated value  
Report currently free units (in memory)

### Configuration parameter: 22a:

**Parameter name:**  
Number of terminals

**Input parameters:**  
FUNC = Function, see below.  
INDEX = 22  
SUBIN = Subindex not used for this configuration parameter  
VALUE = Input value not used for this configuration parameter

**Output parameters:**  
VALUE = Number of interfaces (if read current active value)  
Maximum number of terminals possible to use (if read generated value)

**Functions allowed for this parameter:**  
Read current active value  
Read generated value

### Configuration parameter: 23a:

**Parameter name:**  
Number of Telefix devices

**Input parameters:**  
FUNC = Function, see below.  
INDEX = 23  
SUBIN = Subindex not used for this configuration parameter  
VALUE = Input value not used for this configuration parameter

**Output parameters:**  
VALUE = Number of Telefix devices

**Functions allowed for this parameter:**  
Read generated value

Norsk Data ND-60.230.5 EN  
Scanned by Jonny Odden for Sintran Data © 2021

---

## Page 86

# Configuration parameter: 24s

## Parameter name:
Number of semaphores

## Input parameters:
| Key    | Description                                                              |
|--------|--------------------------------------------------------------------------|
| FUNC   | Function, see below.                                                     |
| INDEX  | 24                                                                       |
| SUBIN  | Subindex not used for this configuration parameter                       |
| VALUE  | Input value not used for this configuration parameter                    |

## Output parameters:
| Key    | Description                |
|--------|----------------------------|
| VALUE  | Number of semaphores       |

## Functions allowed for this parameter:
Read generated value

# Configuration parameter: 25s

## Parameter name:
Number of internal devices (total)

## Input parameters:
| Key    | Description                                                              |
|--------|--------------------------------------------------------------------------|
| FUNC   | Function, see below.                                                     |
| INDEX  | 25                                                                       |
| SUBIN  | Subindex not used for this configuration parameter                       |
| VALUE  | Input value not used for this configuration parameter                    |

## Output parameters:
| Key    | Description                          |
|--------|--------------------------------------|
| VALUE  | Number of internal devices (total)   |

## Functions allowed for this parameter:
Read generated value

# Configuration parameter: 26s

## Parameter name:
Number of internal devices (block)

## Input parameters:
| Key    | Description                                                              |
|--------|--------------------------------------------------------------------------|
| FUNC   | Function, see below.                                                     |
| INDEX  | 26                                                                       |
| SUBIN  | Subindex not used for this configuration parameter                       |
| VALUE  | Number of internal devices (block)                                       |

## Output parameters:
| Key    | Description                                          |
|--------|------------------------------------------------------|
| VALUE  | Input value not used for this configuration parameter|

## Functions allowed for this parameter:
Read generated value

---

## Page 87

# SINTRAN III RELEASE INFORMATION, K-VERSION
## MONITOR CALLS (ND-100)

### Configuration parameter: 27₈

**Parameter name:**  
Number of SIBAS processes

**Input parameters:**  
FUNC = Function, see below.  
INDEX = 27  
SUBIN = Subindex not used for this configuration parameter  
VALUE = Input value not used for this configuration parameter

**Output parameters:**  
VALUE = Number of SIBAS processes

**Functions allowed for this parameter:**  
Read generated value

---

### Configuration parameter: 30₈

**Parameter name:**  
Number of open file entries

**Input parameters:**  
FUNC = Function, see below.  
INDEX = 30  
SUBIN = Subindex not used for this configuration parameter  
VALUE = Input value not used for this configuration parameter

**Output parameters:**  
VALUE = Number of open file entries

**Functions allowed for this parameter:**  
Read current active value

---

### Configuration parameter: 31₈

**Parameter name:**  
Number of allocated areas

**Input parameters:**  
FUNC = Function, see below.  
INDEX = 31  
SUBIN = Subindex not used for this configuration parameter  
VALUE = Number of areas allocated for MON FIXC5 (MON 61)

**Output parameters:**  
VALUE = Number of areas allocated for MON FIXC5 (MON 61)

**Functions allowed for this parameter:**  
Read save area (next value)  
Read current active value  
Write to save area (new next value)

---

## Page 88

# Configuration parameter: 32a:

## Parameter name:
Mon MLAMU (MON 315)

### Input parameters:
- FUNC = Function, see below.
- INDEX = 32
- SUBIN = Subindex not used for this configuration parameter
- VALUE = Input value not used for this configuration parameter

### Output parameters:
- VALUE = 1 if MON MLAMU (MON 315) is available  
  0 if MON MLAMU (MON 315) is unavailable

### Functions allowed for this parameter:
- Read generated value

# Configuration parameter: 33a:

## Parameter name:
Maximum number of LAMUs

### Input parameters:
- FUNC = Function, see below.
- INDEX = 33
- SUBIN = Subindex not used for this configuration parameter
- VALUE = Maximum number of LAMUs

### Output parameters:
- VALUE = Maximum number of LAMUs

### Functions allowed for this parameter:
- Read save area (next value)
- Read current active value
- Write to save area (new next value)

# Configuration parameter: 34a:

## Parameter name:
Maximum number of LAMUs per program

### Input parameters:
- FUNC = Function, see below.
- INDEX = 34
- SUBIN = Subindex not used for this configuration parameter
- VALUE = Maximum number of LAMUs per program

### Output parameters:
- VALUE = Maximum number of LAMUs per program

### Functions allowed for this parameter:
- Read save area (next value)
- Read current active value
- Write to save area (new next value)

---

## Page 89

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### Configuration parameter: 35a:

#### Parameter name:
Maximum number of system LAMUs

#### Input parameters:
- FUNC = Function, see below.
- INDEX = 35
- SUBIN = Subindex not used for this configuration parameter
- VALUE = Maximum number of system LAMUs

#### Output parameters:
- VALUE = Maximum number of system LAMUs

#### Functions allowed for this parameter:
- Read save area (next value)
- Read current active value
- Write to save area (new next value)

### Configuration parameter: 36a:

#### Parameter name:
ND-500 software versions

#### Input parameters:
- FUNC = Function, see below.
- INDEX = 36
- SUBIN = 1 for ND-500 Swapper, 2 for ND-500 System Monitor
- VALUE = Input value not used for this configuration parameter

#### Output parameters:
- VALUE = ND-500 software version (Swapper or System Monitor)
  - VALUE must be an array; minimum length is 4 words if SUBIN=1, otherwise 6.

#### Functions allowed for this parameter:
- Read current active value

#### Special error messages:
- Error 174 (Illegal parameter) can also mean that the ND-500 Swapper is not loaded or that the ND-500 System Monitor is not started.
- Error 3201 (Illegal index) can also mean that there is no ND-500 in this system.

---

Norsk Data ND-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 90

# Configuration parameter: 37₁

## Parameter name:
ND-500 microcode version (per CPU)

## Input parameters:
| Parameter | Description |
|-----------|-------------|
| FUNC | Function, see below. |
| INDEX | 37 |
| SUBIN | ND-500 CPU number (0-7) |
| VALUE | Input value not used for this configuration parameter |

## Output parameters:
| Parameter | Description |
|-----------|-------------|
| VALUE | ND-500 microcode version |
|        | 0 = microcode not loaded |

## Functions allowed for this parameter:
Read current active value

## Special error messages:
- Error 174 (Illegal parameter) can also mean that the specified ND-500 CPU does not exist.
- Error 3201 (Illegal index) can also mean that there is no ND-500 in this system.

# Configuration parameter: 40₁

## Parameter name:
ND-110 or ND-120 microprogram version

## Input parameters:
| Parameter | Description |
|-----------|-------------|
| FUNC | Function, see below. |
| INDEX | 40 |
| SUBIN | Subindex not used for this configuration parameter |
| VALUE | Input value not used for this configuration parameter |

## Output parameters:
| Parameter | Description |
|-----------|-------------|
| VALUE | ND-110/CX or ND-120/CX microprogram version |
|        | 0 = not ND-110/CX nor ND-120/CX CPU |

## Functions allowed for this parameter:
Read current active value

---

## Page 91

# SINTRAN III RELEASE INFORMATION, K-VERSION
## MONITOR CALLS (ND-100)

### Configuration parameter: 41₈

#### Parameter name:
Memory configuration

#### Input parameters:
- FUNC = Function, see below.
- INDEX = 41
- SUBIN = Subparameter, see table below.
- VALUE = Input value not used for this configuration parameter

#### Subparameter:
| Subparameter | Description                        |
|--------------|------------------------------------|
| 0            | Memory configuration (total)      |
| 1            | ND-100 local (including PIOC)     |
| 2            | PIOC (per PIOC)                   |
| 3            | MPM-3                             |
| 4            | MPM-4                             |
| 5            | MPM-5                             |
| 6            | For swapping                      |
| 7            | For SINTRAN                       |
| 10₈          | RT-common                         |
| 11₈          | Reserved by ND 500                |

#### Output parameters:
- VALUE = Memory configuration (in pages)

#### Functions allowed for this parameter:
- Read current active value

### Configuration parameter: 42₈

#### Parameter name:
Define HDLC-interfaces as HDLC or modem

#### Input parameters:
- FUNC = Function, see below.
- INDEX = 42
- SUBIN = HDLC interface number (1-32 limited by configuration)
- VALUE = Define HDLC-interfaces as HDLC or modem:
  - 0 = this HDLC is not to be used
  - 1 = HDLC
  - 2 = Synchronous modem

#### Output parameters:
- VALUE = Type of HDLC-interface (if function ≠ 6):
  - 0 = this HDLC is not used
  - 1 = HDLC
  - 2 = Synchronous modem
- Logical device number (if function = 6)

#### Functions allowed for this parameter:
- Read save area (next value)
- Read current active value
- Write to save area (new next value)
- Special function: read logical device number of HDLC interface

*Norsk Data ND-60.230.5 EN*

---

## Page 92

# Configuration parameter: 43a

## Parameter name
Number of HDLC connections

### Input parameters
- FUNC = Function, see below.
- INDEX = 43
- SUBIN = Subindex not used for this configuration parameter
- VALUE = Input value not used for this configuration parameter

### Output parameters
- VALUE = Number of HDLC connections

### Functions allowed for this parameter
Read generated value

### Note
The number of HDLC devices defined by configuration parameters 43 and 44 can add up to a total exceeding the number of HDLC devices generated. The reason for this is that some devices can be used for either HDLC or synchronous modems or both (but not at the same time).

# Configuration parameter: 44a

## Parameter name
Number of synchronous modems on HDLC

### Input parameters
- FUNC = Function, see below.
- INDEX = 44
- SUBIN = Subindex not used for this configuration parameter
- VALUE = Input value not used for this configuration parameter

### Output parameters
- VALUE = Number of synchronous modems on HDLC

### Functions allowed for this parameter
Read generated value

### Note
See note on configuration parameter 43.

---

## Page 93

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### Configuration parameter: 45a

**Parameter name:**  
Number of X.21 connections

**Input parameters:**  
FUNC = Function, see below.  
INDEX = 45  
SUBIN = Subindex not used for this configuration parameter  
VALUE = Number of X.21 connections

**Output parameters:**  
VALUE = Number of X.21 connections

**Functions allowed for this parameter:**  
Read save area (next value)  
Read current active value  
Write to save area (new next value)  
Read generated value

### Configuration parameter: 46a

**Parameter name:**  
Define spooling device number

**Input parameters:**  
FUNC = Function, see below.  
INDEX = 46  
SUBIN = Spooling index (1-60, limited by configuration)  
VALUE = Define spooling device number

**Output parameters:**  
VALUE = Define spooling device number

**Functions allowed for this parameter:**  
Read save area (next value)  
Read current active value  
Write to save area (new next value)

---

## Page 94

# Configuration parameter: 47s

## Parameter name:
Define printer type

### Input parameters:
- FUNC = Function, see below.
- INDEX = 47
- SUBIN = Printer number (1-4, limited by configuration)
- VALUE = Define printer as line-printer/Fujitsu:
  | Value | Description                          |
  |-------|--------------------------------------|
  | 0     | Printer is not to be used            |
  | 1     | DMA-interface (Fujitsu)              |
  | 2     | Parallel interface (CDC / Dataproducts) |
  | 3     | Serial interface                     |

### Output parameters:
- VALUE = Type of printer and interface (if function ≠ 6):
  | Value | Description                          |
  |-------|--------------------------------------|
  | 0     | Printer is not to be used            |
  | 1     | DMA-interface (Fujitsu)              |
  | 2     | Parallel interface (CDC / Dataproducts) |
  | 3     | Serial interface                     |
  |       | Logical device number (if function = 6) |

### Functions allowed for this parameter:
- Read save area (next value)
- Read current active value
- Write to save area (new next value)
- Special function: read logical device number of printer

# Configuration parameter: 50s

## Parameter name:
Number of Fast UDMA RT-programs

### Input parameters:
- FUNC = Function, see below.
- INDEX = 50
- SUBIN = Subindex not used for this configuration parameter
- VALUE = Number of programs which can use the fast UDMA option simultaneously.

### Output parameters:
- VALUE = Number of Fast UDMA RT-programs

### Functions allowed for this parameter:
- Read save area (next value)
- Read current active value
- Write to save area (new next value)
- Read generated value

---

## Page 95

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### 3.2.5 PERFO MON 344

Purpose: PERFO is used to collect information about the performance of an ND computer.

**Monitor call format:**

| Code          | Description                                    |
|---------------|------------------------------------------------|
| LDA (PARLI    | % A = address of parameter list                |
| MON 344       | % PERFO                                        |
| JMP ERROR     | % error handling, error code in A-register     |
| .........     | % normal return                                |

| Parameter    | Description                                         |
|--------------|-----------------------------------------------------|
| PARLI, FUNCTION | % function code                                    |
| PRIMITIVES   | % primitives: 80-bit bit array                       |
| PARBLOCK     | % parameter block for active primitives              |
| SAMPLERATE   | % sample rate in basic time units                    |
| STOPTIME     | % reserved for future extension                      |

| FUNCTION            |                                      |
|---------------------|--------------------------------------|
| f                   |                                      |
| PRIMITIVES          | n1; n2; n3; n4; n5                   |
| PARBLOCK            | b1; b2; ......                       |
| SAMPLERATE          | s                                    |
| STOPTIME            | 0; 0                                 |

The following values of the function code are used:

| FUNCTION | Description                      |
|----------|----------------------------------|
| 1        | Initiate and start sampling.     |
| 2        | Read collected data.             |
| 3        | Terminate sampling.              |

The following rules apply to the other parameters:

**PRIMITIVES** is an 80-bit bit-array with one bit for each primitive. This bit-array has the same layout as PLANC uses, which means that bit number 15₁₀ of the first word of the array applies to primitive number 1, bit no. 15₁₀ of word no. 2 applies to primitive no. 17₁₀, and so on. A bit set to one means that the primitive with the number equal to the bit number should be activated. Maximum 8 primitives can be active simultaneously. Dummy parameter if FUNCTION ≠ 1.

**PARBLOCK** is an array used to specify parameters for each primitive (when FUNCTION = 1) and is also where collected data is returned (when FUNCTION = 2). Dummy if FUNCTION = 3. PARBLOCK descriptions for each primitive is given below.

**SAMPLERATE** is the sample rate given in basic time units (1 basic time unit = 20 ms). It only applies when FUNCTION = 1, otherwise it is a dummy parameter.

**STOPTIME** is a double-word parameter reserved for future extensions.

---

## Page 96

# SINTRAN III RELEASE INFORMATION, K-VERSION
## MONITOR CALLS (ND-100)

MON 344 can be used by all programs, run by any user. It is, however, only available to programs run on the ND-100/ND-110/ND-120.

Note that MON 344 is a single-thread monitor call, which means that only one program can use the measurement facilities at a time. The facilities are reserved when initiating/starting measurements, and released when either terminating measurements, or program.

MON 344 is generally intended to be used internally by ND, particularly from the Performance Monitor, which is part of Operator Environment (ND-211068).

We reserve the right to change the primitives supported and parameter layout in future versions of this call, thus programs should not relay the specifications from this version being supported in the future.

## MON PERFO

MON PERFO is _not_ supported in generation 406 of SINTRAN III/VSX.

The following primitives are defined to be used in MON PERFO:

| Primitive no. | Explanation                                                  |
|---------------|--------------------------------------------------------------|
| 1             | Use of the ND-100 CPU.                                       |
| 2             | Use of the ND-500/5000 CPU (also applied to multi-CPU).      |
| 3             | Use of logical devices in ND-100.                            |
| 4             | Use of ND-500 Swapper.                                       |
| 5-31          | not yet defined.                                             |
| 32            | Disk access count.                                           |
| 33            | Program start count in ND-100. (generation 3xx only)         |
| 34            | Process start count in ND-500. (generation 3xx only)         |
| 35            | Segment switches count in ND-100 (generation 3xx only)       |
| 36            | Monitor call log in ND-100.                                  |
| 37            | Monitor call log in ND-500.                                  |
| 38-63         | not yet defined.                                             |
| 64            | No. of pages in memory for a ND-100 program or segment       |
| 65            | No. of pages in memory for a ND-500 process or segment       |
| 66            | ND-100 program counter histogram.                            |
| 67            | ND-500 program counter histogram.                            |
| 68-80         | not yet defined.                                             |

Norsk Data ND-60.230.5 EN

---

## Page 97

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### Primitive no. 1

**Description of primitive:**  
Use of the ND-100 CPU.

#### FUNCTION = 1 (initiate and start sampling)

**Layout of parameters in PARBLOCK (input parameters):**

| Parameter | Description |
|--------|-------------|
| Word 1 | primitive number. |
| Word 2 | length of parameter block (4) |
| Word 3 | option code. |
| Word 4 | RT-program (RT-description address) |

**Option codes (octal numbers, can be combined):**

- **100000**: total ND-100 CPU utilisation.
- **10000**: ND-100 CPU utilisation grouped on user mode.
- **1000**: ND-100 CPU utilisation grouped on hardware interrupt levels.
- **200**: ND-100 CPU utilisation grouped on all RT-programs.
- **100**: ND-100 CPU utilisation by a specific RT-program.
- **10**: ND-100 CPU utilisation by a specific RT-program.
- **1**: ND-100 execution queue length.

#### FUNCTION = 2 (read collected data)

**Layout of parameters in PARBLOCK (output parameters):**

| Parameter | Description |
|--------|-------------|
| Word 1 | primitive number. |
| Word 2 | length of parameter block (58₈). |
| Word 3 | address of next parameter block. |
| Words 4-5 | total ND-100 CPU utilisation. |
| Words 6-7 | ND-100 CPU utilisation in user mode. |
| Words 8-9 | ND-100 CPU utilisation in monitor call mode. |
| Words 10-11 | ND-100 CPU utilisation in system mode. |
| Words 12-13 | ND-100 CPU time used on interrupt level 0. |
| Words 14-15 | ND-100 CPU time used on interrupt level 1. |
| Words 16-17 | ND-100 CPU time used on interrupt level 2. |
| Words 18-19 | ND-100 CPU time used on interrupt level 3. |
| Words 20-21 | ND-100 CPU time used on interrupt level 4. |
| Words 22-23 | ND-100 CPU time used on interrupt level 5. |
| Words 24-25 | ND-100 CPU time used on interrupt level 6. |
| Words 26-27 | ND-100 CPU time used on interrupt level 7. |
| Words 28-29 | ND-100 CPU time used on interrupt level 8. |
| Words 30-31 | ND-100 CPU time used on interrupt level 9. |
| Words 32-33 | ND-100 CPU time used on interrupt level 10. |
| Words 34-35 | ND-100 CPU time used on interrupt level 11. |
| Words 36-37 | ND-100 CPU time used on interrupt level 12. |
| Words 38-39 | Dummy parameter (CPU time on level 13 cannot be counted) |
| Words 40-41 | ND-100 CPU time used on interrupt level 14. |
| Words 42-43 | ND-100 CPU time used on interrupt level 15. |
| Words 44-45 | ND-100 CPU time used while a specific program was active. |
| Words 46-47 | ND-100 CPU time used in user mode while a specific program was active. |

Norsk Data ND-60.230.5 EN

Scanned by Jonny Odden for Sintran Data © 2021

---

## Page 98

# Primitive No. 2

## Description of Primitive
Use of the ND-500/ND-5000 CPU.

### FUNCTION = 1 (Initiate and Start Sampling)

**Layout of Parameters in PARBLOCK (Input Parameters):**

| Word | Description                             |
|------|-----------------------------------------|
| 1    | Primitive number.                       |
| 2    | Length of parameter block (5).          |
| 3    | Option code.                            |
| 4    | ND-500 process number.                  |
| 5    | ND-500 CPU number.                      |

**Option Codes (Octal Numbers, Can be Combined):**

- `100000`: Total ND-500 CPU utilisation - all CPUs.
- `10000`: ND-500 CPU utilisation grouped on user mode and swapper mode (swapper mode = swapper is "executing process") - all CPUs.
- `200`: ND-500 CPU utilisation grouped on all ND-500 processes - one specific CPU.
- `100`: ND-500 CPU utilisation by a specific ND-500 process all CPUs.
- `10`: ND-500 CPU utilisation by a specific ND-500 process, grouped on user mode and swapper mode.
- `1`: ND-500 execution queue length accumulated for all CPUs, grouped on active, waiting for swapper, other waiting states.

### FUNCTION = 2 (Read Collected Data)

**Layout of Parameters in PARBLOCK (Output Parameters):**

| Word      | Description                                            |
|-----------|--------------------------------------------------------|
| 1         | Primitive number.                                      |
| 2         | Length of parameter block (`46₁₀`).                     |
| 3         | Address of next parameter block.                       |
| Words 4-5 | Total ND-500 CPU utilisation, CPU no. 1.               |
| Words 6-7 | Total ND-500 CPU utilisation, CPU no. 2.               |
| Words 8-9 | Total ND-500 CPU utilisation, CPU no. 3.               |
| Words 10-11 | Total ND-500 CPU utilisation, CPU no. 4.             |
| Words 12-13 | Total ND-500 CPU utilisation in swapper mode, CPU no. 1. |
| Words 14-15 | Total ND-500 CPU utilisation in swapper mode, CPU no. 2. |
| Words 16-17 | Total ND-500 CPU utilisation in swapper mode, CPU no. 3. |
| Words 18-19 | Total ND-500 CPU utilisation in swapper mode, CPU no. 4. |

Norsk Data ND-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 99

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

| Word    | Description                                                                                        |
|---------|----------------------------------------------------------------------------------------------------|
| 20-21   | total ND-500 CPU utilisation in user mode, CPU no. 1.                                              |
| 22-23   | total ND-500 CPU utilisation in user mode, CPU no. 2.                                              |
| 24-25   | total ND-500 CPU utilisation in user mode, CPU no. 3.                                              |
| 26-27   | total ND-500 CPU utilisation in user mode, CPU no. 4.                                              |
| 28-29   | ND-500 CPU no. 1 usage by a specific process.                                                      |
| 30-31   | ND-500 CPU no. 2 usage by a specific process.                                                      |
| 32-33   | ND-500 CPU no. 3 usage by a specific process.                                                      |
| 34-35   | ND-500 CPU no. 4 usage by a specific process.                                                      |
| 36-37   | ND-500 CPU usage by swapper for a specific process.                                                |
| 38-39   | ND-500 CPU usage by a specific process.                                                            |
| 40-41   | number of processes in ND-500 execution queue, ready for execution.                                |
| 42-43   | number of processes in ND-500 execution queue, waiting.                                            |
| 44-45   | number of processes in ND-500 execution queue, waiting for swapper.                                |
| 46      | address of array where ND-500 CPU usage, one specific CPU, split over all processes, is returned.  |
|         | The first word in this array contains the number of elements in the array (number of ND-500 processes in the system). |
|         | Each element consists of a double-word for ND-500 CPU utilisation by the process.                  |
|         | The process number is the index in the array.                                                      |

## Primitive no. 3

### Description of primitive

Use of logical unit/data fields on ND-100.  
The time measured is the time the logical unit is reserved.  
Some devices, for example terminals, have separate channels (and separate data fields) for input and output; while other devices, for example disks, have only one channel. When the device has separate channels, measurements are given for both channels separately.

### FUNCTION = 1 (initiate and start sampling)

### Layout of parameters in PARBLOCK (input parameters)

| Word | Description                                     |
|------|-------------------------------------------------|
| 1    | primitive number.                               |
| 2    | length of parameter block (2o10).               |
| 3    | option code.                                    |
| 4    | 1st logical unit/data field to measure.         |
| 5    | drive number of 1st logical unit if 1st unit is disk. |
| 6    | 2nd logical unit/data field to measure.         |
| 7    | drive number of 2nd logical unit if 2nd unit is disk. |
| 8    | 3rd logical unit/data field to measure.         |
| 9    | drive number of 3rd logical unit if 3rd unit is disk. |
| 10   | 4th logical unit/data field to measure.         |
| 11   | drive number of 4th logical unit if 4th unit is disk. |
| 12   | 5th logical unit/data field to measure.         |
| 13   | drive number of 5th logical unit if 5th unit is disk. |
| 14   | 6th logical unit/data field to measure.         |
| 15   | drive number of 6th logical unit if 6th unit is disk. |
| 16   | 7th logical unit/data field to measure.         |
| 17   | drive number of 7th logical unit if 7th unit is disk. |
| 18   | 8th logical unit/data field to measure.         |
| 19   | drive number of 8th logical unit if 8th unit is disk. |
| 20   | specific RT-program to measure.                 |

Norsk Data ND-60.230.5 EN  
Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 100

# SINTRAN III RELEASE INFORMATION, K-VERSION
## MONITOR CALLS (ND-100)

### Option codes (octal numbers, can be combined):

- **100000**: total utilisation of the logical device(s).
- **40000**: data fields to measure is specified with data field address, not by logical unit number.
- **200**: utilisation of the logical device(s), grouped on all RT-programs.
- **100**: utilisation of the logical device(s) by specific RT-program.
- **1**: waiting queue length on the logical device(s).

### FUNCTION = 2 (read collected data)

#### Layout of parameters in PARBLOCK (input parameters):

| **Word** | **Description** |
|----------|-----------------|
| 1        | primitive number. |
| 2        | length of parameter block (10010). |
| 3        | address of next parameter block. |
| 4-5      | total usage of logical unit/data field 1 input. |
| 6-7      | total usage of logical unit/data field 1 output. |
| 8-9      | total usage of logical unit/data field 2 input. |
| 10-11    | total usage of logical unit/data field 2 output. |
| 12-13    | total usage of logical unit/data field 3 input. |
| 14-15    | total usage of logical unit/data field 3 output. |
| 16-17    | total usage of logical unit/data field 4 input. |
| 18-19    | total usage of logical unit/data field 4 output. |
| 20-21    | total usage of logical unit/data field 5 input. |
| 22-23    | total usage of logical unit/data field 5 output. |
| 24-25    | total usage of logical unit/data field 6 input. |
| 26-27    | total usage of logical unit/data field 6 output. |
| 28-29    | total usage of logical unit/data field 7 input. |
| 30-31    | total usage of logical unit/data field 7 output. |
| 32-33    | total usage of logical unit/data field 8 input. |
| 34-35    | total usage of logical unit/data field 8 output. |
| 36-37    | usage of logical unit/data field 1 input, by a specific program. |
| 38-39    | usage of logical unit/data field 1 output, by a specific program. |
| 40-41    | usage of logical unit/data field 2 input, by a specific program. |
| 42-43    | usage of logical unit/data field 2 output, by a specific program. |
| 44-45    | usage of logical unit/data field 3 input, by a specific program. |
| 46-47    | usage of logical unit/data field 3 output, by a specific program. |
| 48-49    | usage of logical unit/data field 4 input, by a specific program. |
| 50-51    | usage of logical unit/data field 4 output, by a specific program. |
| 52-53    | usage of logical unit/data field 5 input, by a specific program. |
| 54-55    | usage of logical unit/data field 5 output, by a specific program. |
| 56-57    | usage of logical unit/data field 6 input, by a specific program. |
| 58-59    | usage of logical unit/data field 6 output, by a specific program. |

Norsk Data ND-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data ©2021

---

## Page 101

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

| Words 60-61 | = usage of logical unit/data field 7 input, by a specific program. |
|-------------|---------------------------------------------------------------------|
| Words 62-63 | = usage of logical unit/data field 7 output, by a specific program.|
| Words 64-65 | = usage of logical unit/data field 8 input, by a specific program. | 
| Words 66-67 | = usage of logical unit/data field 8 output, by a specific program.|

| Words 68-69  | = waiting queue length of logical unit/data field 1 input     |
|--------------|---------------------------------------------------------------|
| Words 70-71  | = waiting queue length of logical unit/data field 1 output    |
| Words 72-73  | = waiting queue length of logical unit/data field 2 input     |
| Words 74-75  | = waiting queue length of logical unit/data field 2 output    |
| Words 76-77  | = waiting queue length of logical unit/data field 3 input     |
| Words 78-79  | = waiting queue length of logical unit/data field 3 output    |
| Words 80-81  | = waiting queue length of logical unit/data field 4 input     |
| Words 82-83  | = waiting queue length of logical unit/data field 4 output    |
| Words 84-85  | = waiting queue length of logical unit/data field 5 input     |
| Words 86-87  | = waiting queue length of logical unit/data field 5 output    |
| Words 88-89  | = waiting queue length of logical unit/data field 6 input     |
| Words 90-91  | = waiting queue length of logical unit/data field 6 output    |
| Words 92-93  | = waiting queue length of logical unit/data field 7 input     |
| Words 94-95  | = waiting queue length of logical unit/data field 7 output    |
| Words 96-97  | = waiting queue length of logical unit/data field 8 input     |
| Words 98-99  | = waiting queue length of logical unit/data field 8 output    |

- **Word 100**: = address of array where the utilisation of the specified logical units, grouped on all RT-programs, is stored. The first location in this array contains the number of RT-descriptions (number of elements in the array). This array applies to option code 200a.

### Each entry consists of the following 33 words:

- **Word 1**: = address of RT-description.
- **Words 2-3**: = usage of logical unit/data field 1 input, by this program.
- **Words 4-5**: = usage of logical unit/data field 1 output, by this program.
- **Words 6-7**: = usage of logical unit/data field 2 input, by this program.
- **Words 8-9**: = usage of logical unit/data field 2 output, by this program.
- **Words 10-11**: = usage of logical unit/data field 3 input, by this program.
- **Words 12-13**: = usage of logical unit/data field 3 output, by this program.
- **Words 14-15**: = usage of logical unit/data field 4 input, by this program.
- **Words 16-17**: = usage of logical unit/data field 4 output, by this program.
- **Words 18-19**: = usage of logical unit/data field 5 input, by this program.
- **Words 20-21**: = usage of logical unit/data field 5 output, by this program.
- **Words 22-23**: = usage of logical unit/data field 6 input, by this program.
- **Words 24-25**: = usage of logical unit/data field 6 output, by this program. 

Norsk Data ND-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021.

---

## Page 102

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## MONITOR CALLS (ND-100)

Words 26-27 = usage of logical unit/data field 7 input, by this program.  
Words 28-29 = usage of logical unit/data field 7 output, by this program.  
Words 30-31 = usage of logical unit/data field 8 input, by this program.  
Words 32-33 = usage of logical unit/data field 8 output, by this program.  

### Rules:
1. This function is not available for logical device numbers in the range 1008-1778 (open files).

## Primitive no. 4

### Description of primitive:
Use of ND-500 Swapper.

### FUNCTION = 1 (initiate and start sampling)

#### Layout of parameters in PARBLOCK (input parameters):

| Word | Description |
|------|-------------|
| 1    | primitive number. |
| 2    | length of parameter block (4). |
| 3    | option code. |
| 4    | ND-500 process number. |

#### Option codes (octal numbers, can be combined):

- 100000 : total utilisation of ND-500 Swapper
- 200    : utilisation of ND-500 Swapper grouped on all ND-500 processes
- 100    : utilisation of ND-500 Swapper by a specific ND-500 process

### FUNCTION = 2 (read collected data)

#### Layout of parameters in PARBLOCK (output parameters):

| Word | Description |
|------|-------------|
| 1    | primitive number. |
| 2    | length of parameter block (8). |
| 3    | address of next parameter block. |
| 4-5  | Total utilisation of ND-500 Swapper. |
| 6-7  | Utilisation of ND-500 Swapper by a specific process. |
| 8    | address of array where ND-500 Swapper usage, grouped on all processes, is returned. |

The first word in this array contains the number of elements in the array (number of processes in the system). Each element consists of a double-word for ND-500 Swapper utilisation by the process. The process number is the index in the array. This array applies to option code 2008.

Norsk Data ND-60.230.5 EN

---

## Page 103

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### Primitive no. 32

#### Description of primitive:
ND-100 Disk Access log.

#### FUNCTION = 1 (initiate and start sampling)

#### Layout of parameters in PARBLOCK (*input parameters*):
| Word   | Description                                                         |
|--------|---------------------------------------------------------------------|
| 1      | primitive number                                                    |
| 2      | length of parameter block (29₁₀)                                    |
| 3      | option code                                                         |
| 4      | logical device number of 1st disk drive to log                      |
| 5      | disk unit number, drive number, of 1st disk drive to log            |
| 6-7    | lower disk address of specific disk area, on 1st drive              |
| 8-9    | upper disk address of specific disk area, on 1st drive              |
| 10-15  | same as words 4-9, but for 2nd disk drive to log                    |
| 16-21  | same as words 4-9, but for 3rd disk drive to log                    |
| 22-27  | same as words 4-9, but for 4th disk drive to log                    |
| 28     | option dependent. RT-description address, or ND-500 process number. |
| 29     | option dependent. ND-100 segment number, or ND-500 segment number.  |

#### Option codes (octal numbers, can be combined, except 100000/140000):

| Code   | Description                                                                 |
|--------|-----------------------------------------------------------------------------|
| 100000 | total disk accesses. Code 100000 excludes code 140000.                       |
| 140000 | total disk accesses to a limited disk address area.                          |
| 10000  | disk accesses grouped on type. Type is ND-100 file, ND-100 swap, ND-500 file, ND-500 swap, ND-500 file-as-segment. |
| 1000   | disk accesses grouped on read and write.                                     |
| 100    | ND-100 file accesses grouped on read/write for all or a specific RT-program. RT-program = 0 means all RT-programs. |
| 101    | ND-100 swap accesses to a specific or to all segments, grouped on read/write for all or a specific RT-program. Segment number = 0 means all segments. RT-program = 0 means all RT-programs. |
| 102    | ND-500 file accesses grouped on read/write for all or a specific ND-500 process. ND-500 process = -1 means all processes. |
| 103    | ND-500 swap accesses to a specific segment or to all segments grouped on read/write for all or a specific ND-500 process. Segment number = 0 means any segment. ND-500 process = -1 means all processes.  |
| 104    | ND-500 file-as-segment accesses to a specific segment or to any segment, grouped on read/write for all or a specific ND-500 process. Segment number = 0 means any segment. ND-500 process = -1 means all processes. |
| 110    | ND-100 file accesses, grouped on read/write, grouped on all RT-programs.     |
| 111    | ND-100 swap accesses, grouped on read/write, grouped on all RT-programs.     |
| 112    | ND-500 file accesses, grouped on read/write, grouped on all ND-500 processes.|
| 113    | ND-500 swap accesses, grouped on read/write, grouped on all ND-500 processes.|
| 114    | ND-500 file-as-segment accesses, grouped on read/write, grouped on all ND-500 processes. |

---

## Page 104

# FUNCTION = 2 (read collected data)

## Layout of parameters in PARBLOCK (output parameters)

| Word   | Description                                                                 |
|--------|-----------------------------------------------------------------------------|
| 1      | primitive number.                                                           |
| 2      | length of parameter block (8410).                                           |
| 3      | address of next parameter block.                                            |
| 4-5    | total disk accesses on 1st disk drive to log.                               |
| 6-7    | total ND-100 file I/O accesses on 1st disk drive to log                     |
| 8-9    | total ND-100 swap accesses on 1st disk drive to log.                        |
| 10-11  | total ND-500 file I/O accesses on 1st disk drive to log.                    |
| 12-13  | total ND-500 swap accesses on 1st disk drive to log.                        |
| 14-15  | total ND-500 file-as-segment swap accesses on 1st disk drive to log.        |
| 16-17  | total read accesses on 1st disk drive to log.                               |
| 18-19  | total write accesses on 1st disk drive to log.                              |
| 20-21  | total read accesses of specific type, on 1st disk drive to log.             |
| 22-23  | total write accesses of specific type, on 1st disk drive to log.            |
| 24-43  | same as words 4-23, but for 2nd disk drive to log.                          |
| 44-63  | same as words 4-23, but for 3rd disk drive to log.                          |
| 64-83  | same as words 4-23, but for 4th disk drive to log.                          |
| 84     | address of array for option-dependent results.                              |

## Option code = 110:

The first word in array is number of elements in the array, equals the number of RT-descriptions in the system. Each entry consists of the following 5 words:

| Word      | Description                                                                                 |
|-----------|---------------------------------------------------------------------------------------------|
| 1         | RT-description address                                                                      |
| 2-3       | number of ND-100 file read accesses by the program                                          |
| 4-5       | number of ND-100 file write accesses by the program                                         |

## Option code = 111:

The first word in array is number of elements in the array, equals the number of RT-descriptions in the system. Each entry consists of the following 5 words.

| Word      | Description                                                                                 |
|-----------|---------------------------------------------------------------------------------------------|
| 1         | RT-description address                                                                      |
| 2-3       | number of ND-100 swap read accesses by the program                                          |
| 4-5       | number of ND-100 swap write accesses by the program                                         |

## Option code = 112:

The first word in array is number of elements in the array, equals the number of ND-500 processes in the system. Each entry consists of 2 double-words. The first double-word is the number of ND-500 file read accesses by this process, and the second double-word is the number of ND-500 file write accesses by this process. The ND-500 process number is the index in the array.

## Option code = 113:

The first word in array is number of elements in the array, equals the number of ND-500 processes in the system. Each entry consists of 2 double-words. The first double-word is the number of ND-500 swap read accesses by this process, and the second double-word is the number of ND-500 swap write accesses by this process. The ND-500 process number is the index in the array.

Norsk Data ND-60.730.5 EN

---

## Page 105

# SINTRAN III RELEASE INFORMATION, K-VERSION
## MONITOR CALLS (ND-100)

### Option code = 114:
The first word in array is number of elements in the array, equals the number of ND-500 processes in the system. Each entry consists of 2 double-words. The first double-word is the number of ND-500 file-as-segment read accesses by this process, and the second double-word is the number of ND-500 file-as-segment write accesses by this process. The ND-500 process number is the index in the array.

## Primitive no. 33

### Description of primitive:
ND-100 program start count. This primitive is only available in generations 301 and 312. The number of times a program becomes "current executing program" (either from a wait state, or initially started) is counted. Note that this has very little to do with the number of times a program is started explicitly, using the command @RT, logging in, etc.

### FUNCTION = 1 (initiate and start sampling)

#### Layout of parameters in PARBLOCK (input parameters):
| Word | Description                                            |
|------|--------------------------------------------------------|
| 1    | primitive number.                                      |
| 2    | length of parameter block (4).                         |
| 3    | option code.                                           |
| 4    | RT-program (RT-description address).                   |

#### Option codes (octal numbers, can be combined):
- 100000 : total ND-100 RT-program start-up.
- 10000 : number of start-up of a specific RT-program.
- 1000 : total ND-100 RT-program start-up, grouped on all RT-programs.

### FUNCTION = 2 (read collected data)

#### Layout of parameters in PARBLOCK (output parameters):
| Word     | Description                                                                   |
|----------|-------------------------------------------------------------------------------|
| 1        | primitive number.                                                             |
| 2        | length of parameter block (8).                                                |
| 3        | address of next parameter block.                                              |
| 4-5      | total number of program starts                                                |
| 6-7      | number of program starts of a specified program.                              |
| 8        | address of array where RT-program starts, grouped on all RT-programs, is returned. The first word in this array will contain the number of elements in the array, (number of RT-program in the system). Each element consists of 3 words, the RT-description address and a double-word for the number of starts for the RT-program. |

## Primitive no. 34

### Description of primitive:
ND-500 process start count. This primitive is only available in generations 301 and 312. The number of times a process becomes "current executing program" (either from a wait state, or initially started) is counted.

---

## Page 106

# FUNCTION = 1 (initiate and start sampling)

## Layout of parameters in PARBLOCK (input parameters):

- **Word 1** = primitive number.
- **Word 2** = length of parameter block (4).
- **Word 3** = option code.
- **Word 4** = ND-500 process number.

## Option codes (octal numbers, can be combined):

| Code   | Description                                                          |
|--------|----------------------------------------------------------------------|
| 100000 | total ND-500 process start-up.                                       |
| 10000  | number of start-up of a specific ND-500 process.                     |
| 1000   | total ND-500 process start-up, grouped on all ND-500 processes.      |

# FUNCTION = 2 (read collected data)

## Layout of parameters in PARBLOCK (output parameters):

- **Word 1** = primitive number.
- **Word 2** = length of parameter block (8).
- **Word 3** = address of next parameter block.
- **Words 4-5** = total number of process starts.
- **Words 6-7** = number of process starts of a specified process.
- **Word 8** = address of array where process starts, grouped on all ND-500 processes, is returned. The first word in this array will contain the number of elements in the array, (number of ND-500 processes in the system). Each element consists of a double-word, the number of times the process is started, since the start of the measurement.

# Primitive no. 35

## Description of primitive:

ND-100 segment switches.  
This primitive is only available in generations 301 and 312.

# FUNCTION = 1 (initiate and start sampling)

## Layout of parameters in PARBLOCK (input parameters):

- **Word 1** = primitive number.
- **Word 2** = length of parameter block (5).
- **Word 3** = option code.
- **Word 4** = RT-program. (Option code dependent).
- **Word 5** = ND-100 segment number. (Option code dependent).

## Option codes (octal numbers, can be combined except 100/101/102/103):

| Code  | Description                                                                     |
|-------|---------------------------------------------------------------------------------|
| 100000| total ND-100 segment switches.                                                  |
| 10000 | total ND-100 segment switches by a specific program.                            |
| 1000  | total ND-100 segment switches of a specific ND-100 segment.                     |
| 100   | total ND-100 segment switches grouped on all RT-programs.                       |
| 101   | total ND-100 segment switches grouped on all ND-100 segments.                   |
| 102   | total ND-100 segment switches by a specific RT-program, grouped on all ND-100 segments. |
| 103   | total ND-100 segment switches of a specific ND-100 segment, grouped on all RT-programs. |

---

## Page 107

# SINTRAN III RELEASE INFORMATION, K-VERSION

### MONITOR CALLS (ND-100)

## FUNCTION = 2 (read collected data)

### Layout of parameters in PARBLOCK (output parameters):

| Word  | Description |
|-------|-------------|
| 1     | = primitive number. |
| 2     | = length of parameter block (1010). |
| 3     | = address of next parameter block. |
| 4-5   | = total ND-100 segment switches. |
| 6-7   | = total ND-100 segment switches by a specific RT-program. |
| 8-9   | = total ND-100 segment switches of a specific ND-100 segment, by a specific RT-program. |
| 10    | = address of option-dependent data. |

### Option code = 100:

1st word in array equals number of elements in the buffer, equals number of RT-descriptions in the system. Each element consists of 3 words, the RT-description address, and a double-word segment-switch counter for this RT-program.

### Option code = 101:

1st word in array equals number of elements in the buffer, equals the number of entries in the segment table. Each element consists of a double-word segment-switch counter. The segment number is index in the buffer.

### Option code = 102:

1st word in array equals number of elements in the buffer, equals the number of entries in the segment table. Each element consists of a double-word segment-switch counter. The segment number is index in the buffer.

### Option code = 103:

1st word in array equals number of elements in the buffer, equals number of RT-descriptions in the system. Each element consists of 3 words, the RT-description address, and a double-word segment-switch counter for this RT-program.

## Primitive no. 36

### Description of primitive:

ND-100 monitor call log.

## FUNCTION = 1 (initiate and start sampling)

### Layout of parameters in PARBLOCK (input parameters):

| Word  | Description |
|-------|-------------|
| 1     | = primitive number. |
| 2     | = length of parameter block (1210). |
| 3     | = option code. |
| 4     | = RT-program. (Option code dependent). |
| 5-12  | = 8 RT-programs to split all monitor calls on, or 8 monitor call numbers to log grouped on all RT-programs. |

---

## Page 108

# SINTRAN III RELEASE INFORMATION, K-VERSION
## MONITOR CALLS (ND-100)

### Option codes (octal numbers, options 100000/140000 and 10000/14000 can NOT be combined):

- 100000 : log all monitor calls.
- 140000 : log all monitor calls for a specific RT-program.
- 10000 : log all monitor calls grouped on 8 specified RT-programs.
- 14000 : log 8 specified monitor calls, grouped on all RT-programs.

### FUNCTION = 2 (read collected data)

#### Layout of parameters in PARBLOCK (output parameters):

| Word | Description |
|------|-------------|
| 1    | primitive number. |
| 2    | length of parameter block (5). |
| 3    | address of next parameter block. |
| 4    | address of option-code-dependent buffer no. 1.<br>Applies to option codes 100000o and 140000o.<br>This buffer consists of 4008 double-word elements.<br>Each element is a monitor call counter. The monitor call number is index in the buffer (range: 0-3778). |
| 5    | address of option-code-dependent buffer no. 2.<br>Applies to option codes 10000o and 14000o. |

#### Option code = 10000:

The buffer consists of 4008 elements:
- Each element consists of 308 words, an element can be divided into 8 subelements, one for each of the 8 RT-programs to log monitor calls for. Each subelement consists of the following 3 words:
  - word 1: word specifying the RT-program, and a double-word for counter. The monitor call number is index in the buffer.

#### Option code = 14000:

The buffer consists of the following:
- The 1st word contains the number of elements in the buffer, equal to the number of RT-descriptions in the system. Each element consists of 408 words, an element can be divided into 8 subelements, 1 element for each of the 8 monitor calls to log. Each subelement consists of the following 4 words:
  - word 1 : RT-program
  - word 2 : monitor call number
  - words 3-4 : monitor call counter

---

### Primitive no. 37

#### Description of primitive:
ND-500 monitor call log.

#### FUNCTION = 1 (initiate and start sampling)

#### Layout of parameters in PARBLOCK (input parameters):

| Word | Description |
|------|-------------|
| 1    | primitive number. |
| 2    | length of parameter block (1210). |
| 3    | option code. |
| 4    | ND-500 process number. (Option code dependent). |
| 5-12 | 8 ND-500 processes to split all monitor calls on, or 8 monitor call numbers to log grouped on all ND-500 processes. |

---

---

## Page 109

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### Option codes (octal numbers, options 100000/140000 and 10000/14000 can NOT be combined):

| Option Code | Description |
|-------------|-------------|
| 100000      | log all monitor calls |
| 140000      | log all monitor calls for a specific ND-500 process |
| 10000       | log all monitor calls grouped on 8 specified ND-500 processes |
| 14000       | log 8 specified monitor calls, grouped on all ND-500 processes |

### FUNCTION = 2 (read collected data)

#### Layout of parameters in PARBLOCK (output parameters):

- **Word 1** = primitive number.
- **Word 2** = length of parameter block (5).
- **Word 3** = address of next parameter block.
- **Word 4** = address of option-code-dependent buffer no. 1.  
  Applies to option codes 1000008 and 1400008.  
  This buffer consists of 5248 double-word elements. Each element is a monitor call counter. The monitor call number is index in the buffer (monitor call number range: 0-5238).
- **Word 5** = address of option-code-dependent buffer no. 2.  
  Applies to option codes 100008 and 140008.

#### Option code = 10000:

The buffer consists of 5248 elements.  
Each element consists of 308 words, an element can be divided into 8 subelements, 1 element for each of the 8 ND-500 processes to log monitor calls for. Each subelement consists of the following 3 words, 1 word specifying the ND-500 process, and a double-word for the monitor call counter.  
The monitor call number is index in the buffer.

#### Option code = 14000:

The buffer consists of the following:  
The 1st word contains the number of elements in the buffer, equal to the number of ND-500 processes in the system. Each element consists of 308 words, an element can be divided into 8 subelements, 1 element for each of the 8 monitor calls to log.  
Each subelement consists of the following 3 words:

- **Word 1**: ND-500 process number
- **Word 2-3**: monitor call counter  

The ND-500 process number is index in the buffer.

### Primitive no. 64

#### Description of primitive:

Count pages-in-memory for ND-100 segment.

#### FUNCTION = 1 (initiate and start sampling)

#### Layout of parameters in PARBLOCK (input parameters):

- **Word 1** = primitive number.
- **Word 2** = length of parameter block (1110).
- **Word 3** = dummy parameter for this primitive (option code).
- **Words 4-11** = ND-100 segment numbers to count pages in memory for. Maximum 8 segments can be specified, segment number equals zero, means no segment.

---

## Page 110

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## MONITOR CALLS (ND-100)

### FUNCTION = 2 (read collected data)

#### Layout of parameters in PARBLOCK (output parameters):

| Word  | Description                                                                                        |
|-------|----------------------------------------------------------------------------------------------------|
| 1     | = primitive number.                                                                                |
| 2     | = length of parameter block (27₁₀).                                                                |
| 3     | = address of next parameter block.                                                                 |
| 4-27  | = 8 elements, each element consists of 3 words, 1st word is the segment number, word 2 and 3 is a double-word for accumulating the number of pages-in-memory for the actual segment. |

### Primitive no. 65

#### Description of primitive:
Count pages-in-memory for ND-500 segment.

### FUNCTION = 1 (initiate and start sampling)

#### Layout of parameters in PARBLOCK (input parameters):

| Word  | Description                                                                                        |
|-------|----------------------------------------------------------------------------------------------------|
| 1     | = primitive number.                                                                                |
| 2     | = length of parameter block (11₁₀).                                                                |
| 3     | = dummy parameter for this primitive (option code).                                                |
| 4-11  | = ND-500 segment numbers to count pages in memory for. Maximum 8 segments can be specified, segment number equals zero, means no segment. |

### FUNCTION = 2 (read collected data)

#### Layout of parameters in PARBLOCK (output parameters):

| Word  | Description                                                                                        |
|-------|----------------------------------------------------------------------------------------------------|
| 1     | = primitive number.                                                                                |
| 2     | = length of parameter block (27₁₀).                                                                |
| 3     | = address of next parameter block.                                                                 |
| 4-27  | = 8 elements, each element consists of 3 words, 1st word is the segment number, word 2 and 3 is a double-word for accumulating the number of pages-in-memory for the actual segment. |

### Primitive no. 66

#### Description of primitive:
ND-100 program counter histogram.

### FUNCTION = 1 (initiate and start sampling)

#### Layout of parameters in PARBLOCK (input parameters):

| Word  | Description                                                                                        |
|-------|----------------------------------------------------------------------------------------------------|
| 1     | = primitive number.                                                                                |
| 2     | = length of parameter block (17₁₀).                                                                |
| 3     | = option code.                                                                                     |
| 4     | = interval size. (option dependent)                                                                |
| 5     | = ND-100 interrupt level or ND-100 RT-program. (Option dependent).                                   |
|       |   RT-program = 0 means any RT-program.                                                              |
| 6     | = ND-100 segment number. (Option dependent)                                                         |
|       |   Segment number = 0 means any segment.                                                            |

Norsk Data ND-60.230.5 EN  
Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 111

# SINTRAN III RELEASE INFORMATION, K-VERSION
## MONITOR CALLS (ND-100)

Words 7-71 = 1 word specifying the low address in the address area to log, or (up to) 64 address intervals (option dependent). The address intervals must be specified in ascending order, word 7 is start of 1st interval, word 8 is start of 2nd interval, and word 71 is end of 64th interval+1. Note that you do not have to use 64 intervals. The first interval not used is then specified with an address lower than that of the last interval used.

### Option codes (octal numbers, can NOT be combined):
```
100000 : program histogram on a specific ND-100 interrupt level.
           64 intervals of fixed interval size.
140000 : program histogram on a specific ND-100 interrupt level,
           64 intervals of variable interval size.
120000 : program histogram on level 1, any or specific RT-program, any
           or specific segment. 64 intervals of fixed interval size.
110000 : program histogram on level 1, any or specific RT-program, any
           or specific segment.
           64 intervals of variable interval size.
```

## FUNCTION = 2 (read collected data)

### Layout of parameters in PARBLOCK (output parameters):

| Word  |                                    |
|-------|------------------------------------|
| 1     | = primitive number.               |
| 2     | = length of parameter block (135\(_{10}\)). |
| 3     | = address of next parameter block. |
| 4-133 | = 64 double-words, each represent the utilisation within each address interval. |
| 134-135 | = utilisation outside address area to log. |

## Primitive no. 67

### Description of primitive:
ND-500 program counter histogram.

## FUNCTION = 1 (initiate and start sampling)

### Layout of parameters in PARBLOCK (input parameters):

| Word  |                                    |
|-------|------------------------------------|
| 1     | = primitive number.               |
| 2     | = length of parameter block (135\(_{10}\)). |
| 3     | = option code.                    |
| 4     | = interval size. (option dependent) |
| 5     | = ND-500 process number. (Option dependent). ND-500 process number=-1 means any process. |
| 6-135 | = A double-word specifying the lower address in the address area to log, or 64 32-bit address intervals (option dependent). The address intervals must be specified in ascending order, words 6-7 is start of 1st interval, words 8-9 are start of 2nd interval..., and words 134-135 are end of 64th interval+1. Note that you do not have to use 64 intervals. The first interval not used is then specified with an address lower than that of the last interval used. |

---

## Page 112

# Option Codes

(Octal numbers, can NOT be combined):

| Code   | Description |
|--------|-------------|
100000 | Program histogram for any or a specific ND-500 process. 64 intervals of fixed interval size. |
140000 | Program histogram for any or a specific ND-500 process. 64 intervals of variable interval size. |

# Function = 2 (Read Collected Data)

## Layout of Parameters in PARBLOCK (Output Parameters):

| Word    | Description |
|---------|-------------|
| Word 1  | Primitive number. |
| Word 2  | Length of parameter block (135₁₀). |
| Word 3  | Address of next parameter block. |
| Word 4-133 | 64 double-words, each represent the utilisation within each address interval. |
| Words 134-135 | Utilisation outside address area to log. |

## Example

This example shows the parameters used to initiate and start measurements using 2 primitives.

```
301 LDA (PARL1)  % A-register points to parameter list
    MON 344      % PERF0
    JMP ERROR    % Error return; A-register = error code
    ......
                 % OK return
```

PARL1, FUNC    % Parameter list  
PRIMS  
PARBL  
SAMPL  
STOPT

)FILL

```
FUNC, 1         % Initiate and start measurement

% Specification of primitives to activate

PRIMS, 100000   % Enable primitive number 1
1               % Enable primitive number 32
0
0
0

SAMPL, 1        % Sample each basic time unit (each 20th MS)
STOPT, 0;0      % Dummy (double-word)

Continued on the next page.
```

---

## Page 113

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MONITOR CALLS (ND-100)

### % Parameter block for active primitives

| PARBL | Description |
|-------|-------------|
| 1     | % Primitive number 1 |
| 4     | % Parameter block size for primitive number 1 |
| 111101| % Option codes, decoded as: |
|       | % 100000: Total ND-100 CPU utilisation. |
|       | % 10000: ND-100 CPU utilisation grouped on user mode, mon.call mode and system mode. |
|       | % 1000: ND-100 CPU utilisation grouped on hardware interrupt levels. |
|       | % 100: ND-100 CPU utilisation by a specific RT-program. |
|       | % 1: ND-100 execution queue length. |
| 65650 | % Specific RT-program for option code = 100 |

| 40    | % Primitive number 32 |
| 35    | % Parameter block size for primitive number 32 |
| 111102| % Option codes, decoded as: |
|       | % 100000: Count all disk accesses |
|       | % 10000: Disk access grouped on ND-100 file access, ND-100 swap, ND-500 file access, ND-500 swap, ND-500 file-as-segment |
|       | % 1000: Disk accesses grouped on read and write |
|       | % 102: ND-500 file accesses grouped on read/write |

| 1100  | % Logical device no. of 1st disk drive to log. |
| 0     | % Disk unit number, disk drive number, of 1st disk drive to log. |
| 0;0; 0;0 | % Option code dependent (disk addresses). |

| 1100  | % Logical device no. of 2nd disk drive to log. |
| 1     | % Disk unit number, disk drive number, of 2nd disk drive to log. |
| 0;0; 0;0 | % Option code dependent (disk addresses). |

| 0;0   | % 3rd disk to log (not specified). |
| 0;0; 0;0 | % Option code dependent (disk addresses). |

| 0;0   | % 4th disk to log (not specified). |
| 0;0; 0;0 | % Option code dependent (disk addresses). |

| -1    | % For option code = 102: log ND-500 file accesses grouped on read/write for all ND-500 processes. |
| 0     | % Not used in this example (option code dependent). |

### 3.2.6 MTAD

**MON 345**

MON 345 (MTAD) is introduced to operate on MTADs (Mailbox Terminal Access Devices). The monitor call is intended for internal use by ND, but may be used through the MTAD programming library (ND-250227). Refer to pages 233-238 for more information about the MTAD programming library and MTADs in general.

---

## Page 114

# SINTRAN-SERVICE-PROGRAM

## 4.1 COMMANDS REMOVED

### 4.1.1 \*DEFINE-USER-MONITOR-CALL

The command \*DEFINE-USER-MONITOR-CALL is removed in the VSX-version.

Any user-defined monitor calls must therefore be defined manually:

Find a suitable area for the code of the monitor call and insert it (just as in the previous versions, and in the VSE-version).

Then the monitor call must be defined in the monitor call tables:

| Table | Description |
|-------|-------------|
| MCTAB | Address of monitor call code - 1 word per monitor call entry |
| MPPTAB | PIT where the code is located - 1 byte per monitor call entry |
| TMCTAB | Type of monitor call - 1 byte per monitor call entry |
| GOTAB | Level 14 table - type of monitor call - 1 word per entry. The entry for a user monitor call should contain the address of the common monitor call handling routine MFELL. |

An example of how this is done, is given below.

```
@DMAC
% A user defined monitor call (MON 170) implemented in RPIT for
% reading one location from physical memory. The parameters are: 
% bank number in the T-register and address in the X-register.
)CLEAR
)CLOAD S3PATCH
176000/ RET ; GETO ; ZTREG
ZXREG ; ZAREG ; LDATX
TMCTA ; MCTAB ; MPPTA
)SYSDSF
)9ASSM SYMB-1-LIST
)SYSDSF
)9ASSM SYMB-2-LIST
)CLOAD S3SAVE
32/ SG16F:*
)CLOAD S3SRPIT
SG16F/
7PATC, JPL I (GETO
LDT ,B ZTREG
LDX ,B ZXREG
LDATX 0
STA ,B ZAREG
JMP I (RET

)FILL
)KILL SG16F ; SG16F=* ; SG16F:
)CLOAD S3D9PIT
TMCTA+074/ 13010 +1 % Type 13 in type table (TMCTAB)
MCTAB+170/ 7PATC % Address of the routine (MCTAB)
MPPTAB+074/ 10010 % PIT number 10a (RPIT) (MPPTAB)
)CLOAD S3SAVE
32/ SG/ SG16F
)9EXIT
```

Norsk Data ND-60.230.5 EN

---

## Page 115

# SINTRAN III RELEASE INFORMATION, K-VERSION  
SINTRAN-SERVICE-PROGRAM

## 4.1.2 *SET-CHANNEL-PRIORITY

## 4.1.3 *LIST-ADDRESSES

## 4.2 MODIFIED COMMANDS

### 4.2.1 *CHANGE-BUFFERSIZE

In generations 312 and later, the size of the terminal data field is increased from 1148 words to 1268 words (applies both to ordinary terminals as well as NOTS terminals and MTADs). This means that the size given in the *CHANGE-BUFFERSIZE command must be increased by 128 to get the wanted input buffer size for terminals.

| 312 | + |
|-----|---|
|     | 406 |

### 4.2.2 *CHANGE-DATAFIELD

The following symbolic displacements are added in the K-version of SINTRAN III/VSE:

| FBSIZ | (-13B) | Size of XMSG buffers to use |
|-------|--------|-----------------------------|
| NOBUF | (-14B) | Number of XMSG buffers to use |
| TADTYP | (-21B) | TAD type |

The following symbolic displacements are added in the K-version of SINTRAN III/VSX:

| FBSIZ    | (-13B)  | Size of XMSG buffers to use                  |
|----------|---------|----------------------------------------------|
| NOBUF    | (-14B)  | Number of XMSG buffers to use                |
| SNMIQ    | (648)   | Disk sorting: max. number of elements in queue |
| STIMC    | (618)   | Disk sorting: no. of seek not completed      |
| STREN    | (708)   | Disk sorting: sorting enabled (1) or disabled (0) |
| SUNGL    | (628)   | Disk sorting: no. of seek time out           |
| SUNIH    | (658)   | Disk sorting: inhibit flag for seek          |
| TADTYP   | (-218)  | TAD type                                     |
| TDFLGADDR| (-38)   | Logical address within page of data field    |
| TDFPHPAGE| (-48)   | Physical page of data field                  |

### 4.2.3 *CHANGE-VARIABLE

The following symbolic variable names are added in the K-version of SINTRAN III/VSX:

- DVFBPAGE (first physical page of memory legal for device buffers)
- MINSWPAGES (minimum number of pages of memory for swapping)
- MXDVBUF (maximum number of device buffers in this system)

The following symbolic variable names are no longer supported in SINTRAN III/VSE:

- BGLPAGE
- BGFPAGE
- CACHLIM
- SAFILNO

Norsk Data ND-60.230.5 EN  
Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 116

# SINTRAN III RELEASE INFORMATION, K-VERSION
## SINTRAN-SERVICE-PROGRAM

The following symbolic variable names are no longer supported in SINTRAN III/VSX:

| BGLPAGE | CNVRT  | RTLPAGE  |
|---------|--------|----------|
| BGFPAGE | ENDCOR | SAFILNO  |
| CACHLIM | EXTDS  | TABLES   |
| CCFPAGE | IDNTS  | TMCTAB   |
| CCLPAGE | RTFPAGE|          |

### 4.2.4 \*DEFINE-USER-RESTART-PROGRAM

This command now defines the first RT-program to be started after a power failure. Other RT-programs to be started are defined by the command \*NEXT-USER-RESTART-PROGRAM.

There is no change to parameters: `<RT NAME> <MEMORY?> <IMAGE?> <SAVE AREA?>`

### 4.2.5 \*DISC-ACCESS-LOG

The DISC-ACCESS-LOG command does not support SCSI disks in generations 312 nor 406 nor 500 of SINTRAN III/VSX.

### 4.2.6 \*HELP

The order of the parameters are now swapped, and the parameters are no longer optional.

The parameter sequence is thus: `<COMMAND>` (default: all) `<OUTPUT FILE>` (default: TERMINAL)

### 4.2.7 \*INSERT-SPOOLING-HEADER

On the VSX-version, this command can only be used to insert the spooling header in the memory area. A warm start is thus necessary to put the command into effect. No changes have been made in the parameters for the VSE-version.

The parameter sequence is thus: `<SPOOLING INDEX>` 

for the VSX-version.

### 4.2.8 \*LIST-SERVICE-COMMANDS

The order of the parameters are now swapped, and the parameters are no longer optional.

The parameter sequence is thus: `<COMMAND>` (default: all) `<OUTPUT FILE>` (default: TERMINAL)

---

## Page 117

# SINTRAN III RELEASE INFORMATION, K-VERSION

## SINTRAN-SERVICE-PROGRAM

### 4.2.9 *OCTAL-DUMP

On the VSX-version, the first parameter (area) is changed.

The parameter sequence is thus: 

- `<SEGMENT OR ALT-SEG>`
- `<SEGMENT (NAME OR NUMBER (OCT))>`
- `<LOWER ADDRESS (OCT)>`
- `<UPPER ADDRESS (OCT)>`
- `<OUTPUT FILE>`

for the VSX-version.

### 4.2.10 *READ-BINARY

On the VSX-version, the first parameter (area) is changed.

The parameter sequence is thus: 

- `<SEGMENT OR ALT-SEG>`
- `<SEGMENT (NAME OR NUMBER (OCT))>`
- `<INPUT FILE>`

for the VSX-version.

### 4.2.11 *REMOVE-SINTRAN-COMMAND

The commands are no longer removed, only marked as unavailable. Commands can be restored as available by the command *REINSERT-SINTRAN-COMMAND.

There is no change to parameters: 

- `<COMMAND>`
- `<MEMORY?>`
- `<SAVE AREA?>`

### 4.2.12 *REMOVE-SPOOLING-HEADER

On the VSX-version, this command can only be used to remove the spooling header from the memory area. There is no change to the parameters for the VSE-version.

The parameter sequence is thus: 

- `<SPOOLING INDEX>`
- `<FORM FEED BEFORE?>`
- `<FORM FEED AFTER?>`

for the VSX-version.

**Note** that this change implies that removal of spooling header for a specific spooling device will not survive a warm start (on the VSX-version). The *REMOVE-SPOOLING-HEADER command(s) should therefore be included in the LOAD-MODE batch file to be run after a warm start.

---

## Page 118

# SINTRAN III RELEASE INFORMATION, K-VERSION
## SINTRAN-SERVICE-PROGRAM

### 4.2.13 *SET-COMMAND-PROTECTION

This command now also works on file system commands with the restriction that file system commands can only be made more restricted (a public command can be made SYSTEM only, but not the other way around).

Furthermore, this command can now be used to set command protection on reentrant subsystems and ND-500 standard domains in just the same way as commands. ND-500 standard domains can only be protected when used directly from SINTRAN III; they can be started by the ND-500 Monitor.

### 4.3 NEW COMMANDS

#### 4.3.1 *CREATE-SYSTEM-LAMU

Parameters: `<LAMU ID>` `<SIZE>` `<PHYSICAL START PAGE>`

Create a system-LAMU. A system-LAMU is a special kind of LAMU created in memory (taken from the swapping area). The memory reserved for a system-LAMU is released to be used for swapping when the LAMU is deleted.

#### 4.3.2 *INSERT-PROGRAM-IN-TIME-SLICE

Parameters: `<RT NAME>` `<MEMORY?>` `<IMAGE?>` `<SAVE AREA?>` `<TIMESLICE CLASS>`

The specified RT-program is to be time sliced. It will thus no longer run on a fixed priority. Default value for the parameter `<timeslice class>` is 0.

#### 4.3.3 *LIST-USER-RESTART-PROGRAMS

List all RT-programs to be started after a power failure.

Parameter: `<OUTPUT FILE>`

The default value for the parameter output file is terminal.

---

## Page 119

# SINTRAN III RELEASE INFORMATION, K-VERSION

## SINTRAN-SERVICE-PROGRAM

### 4.3.4 *NEXT-USER-RESTART-PROGRAM

Define further RT-programs to be started after a power failure. The first RT-programs to be started after a power fail are defined by the command *DEFINE-USER-RESTART-PROGRAM. A maximum number of 10 RT-programs can be started automatically after a power failure.

**Parameters:**
- `<RT NAME>`
- `<MEMORY?>`
- `<IMAGE?>`
- `<SAVE AREA?>`

### 4.3.5 *REINSERT-SINTRAN-COMMAND

Restore a SINTRAN III command to be available again after being made unavailable with the command *REMOVE-SINTRAN-COMMAND.

**Parameters:**
- `<COMMAND>`
- `<MEMORY?>`
- `<SAVE AREA?>`

### 4.3.6 *REMOVE-PROGRAM-FROM-TIME-SLICE

**Parameters:**
- `<RT NAME>`
- `<MEMORY?>`
- `<IMAGE?>`
- `<SAVE AREA?>`

The specified RT-program is no longer time sliced. It will thus run on a fixed priority, but will continue to run on the priority it had the moment it was removed from time slicing. The priority can be changed by the command @PRIOR in the usual way.

---

## Page 120

# 5. CONFIGURATION PROGRAM

The SINTRAN III Configuration program (ND-211024) is available to make changes in a generated system of SINTRAN III/VSX version K. The program is installed together with SINTRAN and is started by the @RECOVER command: @S3-CONFIG in just the same way as other programs. Use of the program is restricted to user SYSTEM only. The configuration program is a screen-oriented program, but can also run on a hardcopy terminal. S3-CONFIG should be run every time you install or reinstall SINTRAN.

The program has 10 commands: 5 commands for selecting a menu of configuration parameters which can be changed:

| BACKGROUND | IO-COMM | LAMU | SCSI | VARIOUS |

a command to display the value of parameters which cannot be changed:

DISPLAY

and 4 utility commands to print a report of the current configuration, saving the changed configuration, getting help and exit:

| PRINT | GENERATE | HELP | EXIT |

You use the arrow keys, (←) or (→), or the first letter of a command name to navigate between commands. If you select a command which enables you to change parameters, use the return key (←) to enter the menu of parameters, and use the arrow keys, (↑) or (↓), to navigate. When finished, use the EXIT key, or the "Home" key (↖) to exit from the menu. The HELP key can be used at all times to get information about the current configuration parameter, etc.

## 5.1 THE UTILITY COMMANDS

The PRINT command will print the current configuration to a file. You will be asked for output file and this parameter has no default value.

The GENERATE command will save the changes you have made to the current configuration of SINTRAN III in a configuration file. It will also update the save-area of SINTRAN. You will be asked to confirm that you want to save the changes. Note that you must do a cold start to activate the changes because only the save-area is changed. This command can be given as part of the @RECOVER command to run the configuration program: @S3-CONFIG GENERATE. This feature can be used to reconfigure SINTRAN in exactly the same way as before, after loading from diskettes, for example when installing a new patch file.

The EXIT command is used to exit from the configuration program. If you have made any changes to the configuration, and the changes have not been saved (by the GENERATE command), you will be asked if you want to save the changes.

The HELP command will give a brief explanation of the 4 commands used to select menus of configuration parameters which can be changed.

Norsk Data ND-60.230.5 EN

---

## Page 121

# SINTRAN III RELEASE INFORMATION, K-VERSION

## CONFIGURATION PROGRAM

### 5.2 THE SELECTION COMMANDS

The **BACKGROUND** command will display a menu of configuration parameters related to background processes. The menu can look like this:

|                              | Current | Next | Max | (Input) |
|------------------------------|---------|------|-----|---------|
| Number of TADs               | 10      | 10   | 50  | 10      |
| Number of batch processors   | 5       | 5    | 10  | 5       |
| Number of spooling programs  | 8       | 8    | 20  | 8       |
| Number of background programs| 32      | 32   | 151 | 32      |
| Background allocation        | Present |      |     |         |
| Number of Symbolic Debugger segments | 8    | 8    | 32  | 8       |
| Number of ND-500 processes   | 48      | 48   | 201 | 48      |
| Number of remote file access segments | 16  | 16   | 50  | 16      |
| System segment size          | 5       | 5    | 8   | 5       |
| Mon ADP                      | 1       | 1    | 1   | 1       |

`<Exit>`,`<Home>`, or `: return`  
`<Help>` or `? : field information`

The configuration parameters listed in this menu are:

- **Number of TADs:** Number of terminal access devices. Each terminal on a remote system which is to use this system uses a TAD. One TAD is also used by the remote file server.

- **Number of batch processors:** Number of batch jobs to run at a time.

- **Number of spooling programs:** Number of printers used simultaneously.

- **Number of background programs:** Number of "terminals" to be used simultaneously. If background allocation is present, it should be number of TADs + number of terminals + number of Telefix devices to be used simultaneously.

- **Background allocation:** Indicates if the background allocation system is in use. This parameter cannot be changed.

- **Number of Symbolic Debugger segments:** Number of simultaneous users of the Symbolic Debugger.

- **Number of ND-500 processes:** Number of simultaneous users of the ND-500 part. Should usually be equal to "number of background programs" + 1.

- **Number of remote file access segments:** Number of simultaneous users of remote file access.

- **System segment size:** Size (in pages) of the system segment. This size will affect the number of open files for each user as follows: default (= min.) size (5) provides 48 file buffers, each additional page up to total max. of 8) provides 16 buffers. Each open file uses 2 buffers if sequential access, 1 if random. Maximum number of open files are 64.

- **Mon ADP:** Indicates if MON ADP (MON 342) is to be available.

Norsk Data ND-60.Z30.5 EN
Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 122

# SINTRAN III RELEASE INFORMATION, K-VERSION

## CONFIGURATION PROGRAM

Note: if the Background Allocation System is not present, the Next and Max fields of Number of background programs will not be used.

The **Current** value is the value currently used by SINTRAN, the **Next** value is the value which will apply after the next cold start and the **Max** value is the maximum generated for this version of SINTRAN. All parameters can be changed, you use the return key (↵) to enter the menu, and the arrow keys, (↑) and (↓), to navigate. When finished, use the EXIT key or the HOME key (\) to exit.

The **IO-COMM** command will display a menu of some configuration parameters. The menu can look like this:

|                                | Current | Next | Max | (Input) |
|--------------------------------|---------|------|-----|---------|
| Number of HDLC connections     | 12      |      |     |         |
| Number of synchronous modems on HDLC | 6    |      |     |         |
| Number of X.21 connections     | 0       | 0    | 2   | 0       |

Define spooling device numbers

Define HDLC interface as HDLC or modem

Define printer type

Type <> or E to edit these tables

<Exit>, <Home>, or ↵ : return

<Help> or ? : field information

The configuration parameters listed in this menu are:

**Number of HDLC connections**: Number of HDLC connections generated for this system.

**Number of synchronous modems on HDLC**: Number of HDLC connections which can be used for synchronous modems.

**Number of X.21 connections**: Number of X.21 connections.

Define spooling device numbers

Define HDLC interface as HDLC or modem

Define printer type

Use the (<>) key or E to enter sub-menus for these parameters. See below.

The Current value is the value currently used by SINTRAN, the Next value is the value which will apply after the next cold start and the Max value is the maximum generated for this version of SINTRAN. All parameters can be changed, you use the return key (↵) to enter the menu, and the arrow keys, (↑) and (↓), to navigate. When finished, use the EXIT key or the HOME key (\) to exit.

The 3 last configuration parameters in the menu IO-COMM contains tables of values, and when you select one of these, you will get a sub-menu on your terminal.

---

Norsk Data NO-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 123

# SINTRAN III RELEASE INFORMATION, K-VERSION

## CONFIGURATION PROGRAM

The sub-menu for the parameter **Define spooling device numbers** can look like this:

Define spooling device numbers. Enter a logical device number.

| Spooling | Current | Next | (Input) | Spooling | Current | Next | (Input) |
|----------|---------|------|--------|----------|---------|------|--------|
| 1        | 5       | 5    | 5      | 2        | 59      | 59   | 59     |
| 3        | 0       | 0    | 0      | 4        | 0       | 0    | 0      |
| 5        | 0       | 0    | 0      | 6        | 0       | 0    | 0      |
| 7        | 0       | 0    | 0      | 8        | 0       | 0    | 0      |
| 9        | 0       | 0    | 0      | 10       | 0       | 0    | 0      |
| 11       | 0       | 0    | 0      | 12       | 0       | 0    | 0      |
| 13       | 0       | 0    | 0      | 14       | 0       | 0    | 0      |
| 15       | 0       | 0    | 0      | 16       | 0       | 0    | 0      |
| 17       | 0       | 0    | 0      | 18       | 0       | 0    | 0      |
| 19       | 0       | 0    | 0      | 20       | 0       | 0    | 0      |

`<Exit>`/`<Home>`/. :return `<Help>`/? :field information **N** :next

The **Current** value is the value currently used by SINTRAN, the **Next** value is the value which will apply after the next cold start.

If you need to set spooling device numbers for spooling processes 21-40 or 41-60, similar sub-menus will appear for these. Either navigate "past" 20 (40), or select next menu (**N**) key.

## Define HDLC Interface

The sub-menu for the parameter **Define HDLC interface as HDLC or modem** can look like this:

Define HDLC interface as HDLC or modem.

| HDLC    | Current | Next | (Input) | HDLC    | Current | Next | (Input) |
|---------|---------|------|--------|---------|---------|------|--------|
| 1       | 1       | 1    | 1      | 2       | 1       | 1    | 1      |
| 3       | 1       | 1    | 1      | 4       | 1       | 1    | 1      |
| 5       | 1       | 1    | 1      | 6       | 1       | 1    | 1      |
| 7       | 0       | 0    | 0      | 8       | 0       | 0    | 0      |
| 9       | 0       | 0    | 0      | 10      | 0       | 0    | 0      |
| 11      | 0       | 0    | 0      | 12      | 0       | 0    | 0      |
| 13      | 0       | 0    | 0      | 14      | 0       | 0    | 0      |
| 15      | 0       | 0    | 0      | 16      | 0       | 0    | 0      |
| 17      | 0       | 0    | 0      | 18      | 0       | 0    | 0      |
| 19      | 0       | 0    | 0      | 20      | 0       | 0    | 0      |

0=Do not use this interface.  
1=HDLC.  
2=Synchronous modem.

`<Exit>`/`<Home>`/. :return `<Help>`/? :field information **N** :next

The **Current** value is the value currently used by SINTRAN, the **Next** value is the value which will apply after the next cold start.

If you need to set status on HDLC connections 21-32, a similar sub-menu will appear for these. Either navigate "past" 20, or select next menu (**N**) key.

---

## Page 124

# SINTRAN III RELEASE INFORMATION, K-VERSION
## CONFIGURATION PROGRAM

The sub-menu for the parameter Define printer type can look like this:

### Define printer type

| Printer | Current | Next | (Input) |
|---------|---------|------|---------|
| 1       | 1       | 1    | 1       |
| 2       | 0       | 0    | 0       |
| 3       | 0       | 0    | 0       |
| 4       | 0       | 0    | 0       |

- Types: 
  - 0 - Do not use this printer.
  - 1 - DMA (Used for Fujitsu)
  - 2 - Parallel (Used for CDC/DP)
  - 3 - Serial

<Exit>/<Home>/ : return <Help>/? : field information

The Current value is the value currently used by SINTRAN, the Next value is the value which will apply after the next cold start.

All parameters can be changed, you use the arrow keys ({}) or ({}) to navigate. When finished, use the EXIT key or the HOME key (\) to exit.

The LAMU command will display a menu of some configuration parameters. The menu can look like this:

|                        | Current | Next | Max | (Input) |
|------------------------|---------|------|-----|---------|
| Mon MLAMU              | Present |      |     |         |
| Max number of LAMUs    | 32      | 32   | 2048| 32      |
| Max number of LAMUs per program | 2  | 2   | 64  | 2       |
| Max number of system LAMUs | 64  | 64  | 2048| 64      |

<Exit>,<Home> or : return <Help> or ? : field information

Only the first line of the menu is shown if Mon MLAMU is not present.

The configuration parameters listed in this menu are:

- **Mon MLAMU**: Indicates if MON MLAMU (MON 315) is to be present or not.
- **Max number of LAMUs**: Maximum number of LAMUs (a LAMU is a reserved part of memory).
- **Max number of LAMUs per program**: Maximum number of LAMUs accessible from a single program.
- **Max number of system LAMUs**: Maximum number of system LAMUs (further information on system LAMUs is given on pages 25, 55-60 and 102).

The Current value is the value currently used by SINTRAN, the Next value is the value which will apply after the next cold start.

Norsk Data ND-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 125

# SINTRAN III RELEASE INFORMATION, K-VERSION

## CONFIGURATION PROGRAM

All parameters can be changed, you use the return key (↵) to enter the menu, and the arrow keys, (↑) and (↓), to navigate. When finished, use the EXIT key or the HOME key (⌂) to exit.

The SCSI command will first display a menu of SCSI magnetic disk and streamer configuration parameters. This menu is extended by a similar menu with parameters for SCSI optical disk and magnetic tape units.

The first menu can look like this:

|                      | Current..!Next)           |                      | Current..!Next)           |
|----------------------|----------------------------|----------------------|----------------------------|
|                      | Adaptor . ID no           |                      | Adaptor . ID no           |
| SCSI disk no. 1:     | 1.0      1.0              | SCSI disk no. 2:     | 1.3      1.3              |
| SCSI disk no. 3:     | 1.5                       | SCSI disk no. 4:     |                            |
| SCSI disk no. 5:     |                           | SCSI disk no. 6:     |                            |
| SCSI disk no. 7:     |                           | SCSI disk no. 8:     |                            |
| SCSI disk no. 9:     |                           | SCSI disk no. 10:    |                            |
| SCSI disk no. 11:    |                           | SCSI disk no. 12:    |                            |
| SCSI disk no. 13:    |                           | SCSI disk no. 14:    |                            |
| SCSI streamer 1:     | 1.1      1.1              | SCSI streamer 2:     |                            |

<Scroll down key>, ‘N’ or ‘n’ for next page

`<Exit>`, `<Home>` or : return

`<Help> or ? : field information

The **Current** value is the value currently used by SINTRAN, the **Next** value is the value which will apply after the next warm start.
All parameters can be changed, you use the return key (↵) to enter the menu, and the arrow keys, (↑) and (↓), to navigate. When finished, use the EXIT key or the HOME key (⌂) to exit.

For each device (magnetic disk or streamer tape unit) you want to define, enter the SCSI adaptor number (1-4) to the left of the period and the SCSI ID number (0-7) to the right of the period on the appropriate line.

Bear in mind the restriction that the system disk (main swapping device), if it is a SCSI disk, must be connected as ID number 0 on SCSI adaptor number 1.

Also note that the SCSI adaptor itself is connected as ID number 7.

---

## Page 126

# SINTRAN III RELEASE INFORMATION, K-VERSION
## CONFIGURATION PROGRAM

If you want to set the configuration parameters for SCSI optical disks or magnetic tape units, press the `<scroll down>` key or the N-key to select the next menu which can look like this:

|                   | Current. (Next) |                            | Current. (Next)  |
|-------------------|-----------------|----------------------------|------------------|
| Adaptor ID no     | 1.4             |                            |                  |
| Optical disk no.1 | 1.4             | Optical disk no.2          |                  |
| Optical disk no.3 |                 | Optical disk no.4          |                  |
| Mag. tape no. 1   | 1.2             | Mag. tape no. 2            | 1.2              |
| Mag. tape no. 3   |                 | Mag. tape no. 4            |                  |

`<Scroll up key>`, `P` or `p` for previous page  
`<Exit>`, `<Home>` or `: return`  
`<Help> or ? : field information`

The **Current** value is the value currently used by SINTRAN, the **Next** value is the value which will apply after the next warm start. All parameters can be changed, you use the return key (↵) to enter the menu, and the arrow keys, (↑) and (↓), to navigate. When finished, use the EXIT key or the HOME key (⧉) to exit.

For each device (optical disk or magnetic tape unit) you want to define, enter the SCSI adaptor number (1-4) to the left of the period and the SCSI ID number (0-7) to the right of the period on the appropriate line.

Bear in mind the restriction that the system disk (main swapping device), if it is a SCSI disk, must be connected as ID number 0 on SCSI adaptor number 1.

Also note that the SCSI adaptor itself is connected as ID number 7.

Note that changes made to SCSI configuration will be stored in the SINTRAN III image and save areas immediately, and that a warm start is required for the changes to take effect.

For further details, refer to pages 242-253.

---

## Page 127

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## CONFIGURATION PROGRAM

The **VARIOUS** command will display a menu of some configuration parameters. The menu can look like this:

|                             | Current | Next | Max | (Input) |
|-----------------------------|---------|------|-----|---------|
| Number of device buffers    | 64      | 64   | 64  | 64      |
| First legal physical page   |         |      |     |         |
| for device buffer           | 000000B | 000000B | 000000B |     |
| Spooling queue size in pages| 4       | 4    | 14  | 4       |
| Number of allocated areas   | 64      | 64   | 6144| 64      |
| Number of fast UDMA programs| 0       | 0    | 6   | 0       |

<Exit>,<Home> or : return  
<Help> or ? : field information

The configuration parameters listed in this menu are:

**Number of device buffers** : Number of device buffers.

**First legal phys. page for device buffer** : First physical page in memory for device buffers.

**Spooling queue size in pages** : Size of each spooling queue - a queue size of 2 pages can contain 10 queue entries and each additional page will increase queue length by approximately 7 new entries.

**Number of allocated areas** : Number of areas in memory reserved by the monitor call MON FIXC5 (MON 61). Should be larger than number of system LAMUs.

**Number of fast UDMA programs** : Number of RT-programs to use fast UDMA.

The **Current** value is the value currently used by SINTRAN, the **Next** value is the value which will apply after the next cold start and the **Max** value is the maximum generated for this version of SINTRAN.

All parameters can be changed, you use the return key (↵) to enter the menu, and the arrow keys, {↑} and {↓}, to navigate. When finished, use the EXIT key or the HOME key (↰) to exit.

## 5.3 THE DISPLAY COMMAND

The **DISPLAY** command will show the current values of some configuration parameters which are not changeable.

Norsk Data ND-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 128

# SINTRAN III RELEASE INFORMATION, K-VERSION
## CONFIGURATION PROGRAM

The screen picture can look like this:

| | Current | Max |
| --- | --- | --- |
| Number of user RT-programs | 110 | |
| Number of user segments | 734 | |
| Number of terminals | 25 | 132 |
| Number of semaphores | 50 | |
| Number of internal devices (total) | 30 | |
| Number of internal devices (block) | 2 | |
| Number of SIBAS processes | 12 | |
| Number of open file entries | 48 | |
| COSMOS spooling | Yes | |
| Number of Telefix devices | 1 | |
| Work mode version (generation) | 000312B | |
| Standard system | Yes | |

The configuration parameters listed in this command are:

Current value means:

**Number of user RT programs**: Number of free RT-descriptions.

**Number of user segments**: Number of free segments.

**Number of terminals**: Number of terminals used.

**Number of semaphores**: Number of semaphores generated.

**Number of internal devices (total)**: Total number of internal devices (generated value).

**Number of internal devices (block)**: Number of block-oriented internal devices (generated value).

**Number of SIBAS processes**: Number of SIBAS processes generated.

**Number of open file entries**: Number of files open simultaneously (generated value).

**COSMOS spooling**: Indicates if COSMOS spooling is present.

**Number of Telefix devices**: Number of Telefix devices (generated val).

**Work mode version**: Version of work mode used when generating this SINTRAN system (for internal use by ND).

**Standard system**: Indicates if this SINTRAN is a standard system.

The **Max** value appearing for the **Number of terminals** parameter means maximum number of terminals supported by this system (generated value).

Norsk Data ND–60.220.5 EN

---

## Page 129

# 6. NOTS-SERVICE PROGRAM

The Net/One Service program is a program used by system supervisors on ND machines using Net/One terminal servers.

Before you can connect to ND machines via Net/One, the system supervisor must set the configuration on the ND machine in SINTRAN. This is done in the save or image area for each NOTS (Net/One terminal server) on the machine. In another words, the system supervisor only needs to define the configuration on a machine each time SINTRAN is installed.

The first time you use Net/One, you may use the NOTS-Service program manually when you install SINTRAN, for example, just after you give the generate command in the SINTRAN Configuration program, and just before @COLD-START. Here is an example of a first time NOTS installation:

- Install Net/One hardware in ND machine
- Install SINTRAN III/VSX version K, generation 301 and run patch file
- Use generate command in S3-CONFIG
- Run NOTS-SERVICE and use save and image areas
- Do a cold start

To be sure that Net/One is configured each time you install SINTRAN, we suggest the following line(s) be included in your HENT-MODE file:

The following values are assumed in this example: 2 controllers (0 and 1), computer name ND, 2 outgoing lines, no controller bits set, and info. is stored in the save and image areas of SINTRAN:

```
@NOTS-SERVICE SET-NOTS-CONFIGURATION 0 ND 2 0 SI
@NOTS-SERVICE SET-NOTS-CONFIGURATION 1 ND 2 0 SI
```

It does not matter where you put this in the HENT-MODE file. A comparable operation might be initialising XMSG, since that only needs to be done each time you install SINTRAN.

The NOTS-Service program has the following commands:

- EXIT
- GET-LINE-INFORMATION
- GET-NOTS-CONFIGURATION
- RELOAD-NOTS
- RESTART-NOTS
- SET-NOTS-CONFIGURATION

Used from a mode file, all parameters should be given on one line. In interactive use, the program will prompt for each parameter. When a command is completed, the program will exit.

**SET-NOTS-CONFIGURATION**  
`<NOTS no.>, <NOTS name>, <no. of outgoing lines>, <controller flag>, <area>`

Set configuration parameters for a NOTS. Only the image and save areas of SINTRAN III can be changed.

---

## Page 130

# SINTRAN III RELEASE INFORMATION, K-VERSION
## NOTS-SERVICE PROGRAM

### `<NOTS number>`
This is the device number of the controller to be configured. It is set by a thumbwheel on the NOTS controller. The controller number's range is 0-7.

### `<NOTS name>`
This is the name of the ports to be used by network resources (NIU's). Default value is 'ND'. Normally, you should use the name of your computer (the same name as you use in @CONNECT-TO).

### `<number of outgoing lines>`
Specify the number of ports to be reserved for outgoing calls. Such ports are mainly intended for printers and cannot be connected from the network.

### `<controller flag>`
Flag to set some options for the server.
- Bit 0 set: disconnect line on logout.
- Bit 1 set: do not reset terminal type on connect.
- Bit 2 set: 8-bit I/O is set on incoming lines.
- Bit 3 set: 8-bit I/O is set on outgoing lines.
- Other bits not assigned, should be zero.

### `<area (S,I)>`
Update save (S) or image (I) or both (SI).

---

## GET-NOTS-CONFIGURATION `<NOTS number>,<area>`
List the configuration parameters for a controller. The parameters can be listed for save, image or memory.

---

## GET-LINE-INFORMATION `<NOTS number>`
This function lists the information about all ports for the specified controller. The following information is listed:

| Ln. (Port number) | log.no. (Logical device number in SINTRAN) | CI | Out | Open | Req |
|-------------------|-------------------------------------------|----|-----|------|-----|
| An X means that this port is not in use. | Reserved for outgoing calls. | Connection open on this port. | Request outstanding, the port is waiting for a connection request from the network. |

---

## RESTART-NOTS `<NOTS number>`
The restart function can be used to stop all terminals connected to the specified server.

---

## RELOAD-NOTS `<NOTS number>`
The reload function sends a 'load request' to the network manager, which reloads the controller.
Use RELOAD if you get a new release of the MBNIU software.
Note that the reload function generates a long interrupt off sequence which can create problems for synchronous communication devices.

---

## Page 131

# 7. FILE SYSTEM

## 7.1 INTRODUCTION

One of the limitations of the SINTRAN III file system was the number of files (objects) allowed under each user area. In order to ease this limitation, the internal structure of the file system has been changed to support more than 256 files per user. The following pages will describe the new directory structure, how the object file is expanded, and changes in the data structures.

## 7.2 THE NEW DIRECTORY STRUCTURES

The main part of the directory structure is unchanged, i.e., the directory entry and the user file are identical to the previous versions. The object file can be extended when a user creates more than 256 files and it will then be subindexed even if there is no user with user index exceeding 63. The file system will automatically establish a subindexed structure when user 64 is created, or when the first object with index exceeding 255 is created by any user. This subindexed structure is illustrated in the figure on the next page.

All files belonging to a user are divided into blocks of 256 objects. Whenever creating file number 257, 513 and so on, a new index block is allocated for this user. The maximum number of index blocks a user can have is 16, which allows a maximum of 4096 files. The reason for this limit is the number of vacant bits in the object entry to specify the object block number of the current, next and previous versions of the file.

All users can have 4096 files. However, the maximum number of files can be set separately for each user area. A 4-bit word (MXOBL) in the user entry is used to specify how many extra object blocks the user is allowed to have. If MXOBL is zero, the user can only have 256 files, and if it is zero for all users, the file system will work as in previous versions of SINTRAN.

To avoid searching through all possible 4096 entries to get a specific object, another 4-bit word (ACOBL) in the user entry holds the actual number of extra object blocks in use. If ACOBL is zero, it is only necessary to search through the first 256 objects, if it equals 1, the user has less than 513 files and so on.

## 7.3 RESTRICTIONS - COMPATIBILITY

All versions of a file must have object entries in the same object block. The reason for this is to keep the directory structure compatible with earlier versions.

Refer to “Warning if moving to version J”, on page 119, for things to consider if you move a directory containing files on object index ≥ 255 from version K to version J.

---

## Page 132

# 7.4 OBJECT FILE WITH SUBINDEX BLOCK

| Object file subindex block | Object file index block no. 0 | Object entries 0-31 User 0 |
|----------------------------|-----------------------------|-----------------------------|
|                            | Object file index block no. 1 | Object entries 224-255 User 63 |
|                            | Object file index block no. 2 | Object entries 0-31 User 64 |
|                            |                              | Object entries 0-31 User 192 |
|                            | Object file index block no. 3 | Object entries 224-255 User 255 |
|                            | Object file index block no. 4 | Object entries 256-287 User 0 |
|                            |                              | Object entries 480-511 User 63 |
|                            | Object file index block no. 7 | Object entries 256-287 User 192 |
|                            |                              | Object entries 480-511 User 255 |

The last is object file index block no. 63, pointing to objects 4064-4095 for users 192-255

Norsk Data ND-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 133

# SINTRAN III RELEASE INFORMATION, K-VERSION

## FILE SYSTEM

### 7.5 USER FILE ENTRY

This figure shows the layout of the user entry. The free location in displacement 27₈ is used for MXOBL and ACOBL.

|     |   | 1         |
|-----|---|-----------|
| 0   | U | Enter count |
| 1   |   | User name  |
| 11  |   | Password   |
| 12  |   | Date created |
| 14  |   | Last date entered |
| 16  |   | No of pages reserved |
| 20  |   | No of pages used |
| 22  |   | User index |
| 23  |   |           |
| 24  |   | Default file access |
| 25  |   | Previous user entry |
| 26  |   | Next user entry |
| 27  |   | ₇MXOBL₄ ₃ACOBL₀ UXOBL (New entry) |
| 30  |   | Friend table |

MXOBL is the maximum number of extra object blocks. ACOBL is the actual number of extra object blocks in use. If MXOBL (and ACOBL) is zero, object file extension is not allowed, and the file system will then work exactly as in previous versions of SINTRAN.

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 134

# 7.6 Object File Entry

The object file entry must comprise the object block numbers of the current, next and previous versions of the file. Some vacant bits in OFTYP are used for the object block number of the file. The same object block number is used for both the current, next and previous versions.

| 0  | U W R M | Terminal no. reserving       |
|----|--------|-------------------------------|
| 1  |        | File name                     |
| 11 |        | File type                     |
| 13 |        | Next version                  |
| 14 |        | Previous version              |
| 15 |        | Public acc. | Friend acc. | Own acc. |
| 16 | ¹,⁷OBJBL₁₄ | TM L M A C I S P T OFTYP    |
| 17 |        | Device number for peripheral file |
| 20 |        | Dir. index of r. | User index of res |
| 21 |        | Object index of this object entry |
| 22 |        | Current open count            |
| 23 |        | Total open count              |
| 24 |        | Date created                  |
| 26 |        | Last date opened for read     |
| 30 |        | Last date opened for write    |
| 32 |        | No. of pages in file          |
| 34 |        | Maximum byte pointer          |
| 36 | S J    | File pointer                  |

Object file entry

---

## Page 135

# SINTRAN III RELEASE INFORMATION, K-VERSION
## FILE SYSTEM

### 7.7 OBJECT FILE BUFFER

The object file buffer header is increased by one word, holding the block number of the object in the buffer.

|   |   |
|---|---|
| 0 | Lock number of object buffer |
| 1 | Directory index |
| 2 | Current object block number (New entry) |
| 3 | Current object index (8 bits) |
| 4 |   |
| 5 | First index in index buffer |
| 6 |   |
| 7 | Index buffer |

### 7.8 THE OPEN FILE TABLE ENTRY

The 4 most significant bits in OFFTP (displacement 6) were free, and are now used to hold the object block number of the file.

### 7.9 WARNING IF MOVING TO VERSION J

If you move a directory (for example a removable disk, or a diskette) used from SINTRAN III version K to an installation still running version J, you must note the following if this directory contains files with object index > 255:

- All files with object index > 255 will be invisible (but they will reappear when you move back to version K).

- If you use the commands @REGENERATE-DIRECTORY or @TEST-DIRECTORY in the J-version on such a directory, the directory will end in an indeterminable state and the files on object index > 255 will be lost.

- The FILE-SYSTEM-INVESTIGATOR (for version J) used on such a directory will report errors when detecting files on object index > 255.

---

## Page 136

# 7.10 DIRECTORY ENTRY ON DISK

The directory entry stored on disk is extended from 20₈ to 30₈ words. The new directory entry starts at word no. 1750₈ on page 0 of the directory.

The layout is as follows:

Displacement within page 0 (octal)

|       |                                         |
|-------|-----------------------------------------|
| 1750  | Checksum of words 1751₈-1757₈           |
| 1751  | Reserved for future use                 |
| 1752  | Reserved for future use                 |
| 1753  | Reserved for future use                 |
| 1754  | Flag word (bit 17₈ set = entered)       |
| 1755  | System number last entering             |
| 1756  | Number of pages available               |
| 1760  | Directory name (16 characters)          |
| 1770  | Object file index pointer               |
| 1772  | User file index pointer                 |
| 1774  | Bit file index pointer                  |
| 1776  | Number of pages not reserved            |

The old directory entry only used the 20₈ word starting at 1760₈ on page 0 on the directory. If the checksum in word 1750₈ is incorrect, the directory entry is assumed to be of the old format.

---

## Page 137

# SINTRAN III RELEASE INFORMATION, K-VERSION

## 8. SPOOLING

It is now possible to do remote spooling without using the COSMOS remote spooling facility.

This is made possible because the commands `@APPEND-SPOOLING-FILE`, `@DELETE-SPOOLING-FILE`, `@MOVE-SPOOLING-QUEUE-ENTRY` and `@REMOVE-FROM-SPOOLING-QUEUE` now accept remote file specifications in both the `<peripheral file>` and `<file name>` parameters.

Note one restriction which is applicable if you have a network with some systems running SINTRAN III version J:

Remote spooling is not handled (except COSMOS remote spooling) in version J, thus you cannot do remote spooling to a spooling device on a system running version J.

Also note the following:

- If a spooling process is unable to access a remote file to be output due to network failure, the spooling request is moved to the back of that spooling queue. This process is repeated until the file can be accessed.

- If a network connection is broken during printing, thus causing the rest of a file to be inaccessible, the rest of that printout is lost.

- If you use the commands `@DELETE-SPOOLING-FILE`, `@MOVE-SPOOLING-QUEUE-ENTRY` or `@REMOVE-FROM-SPOOLING-QUEUE` to delete or move a spooling request, and this request contains a remote file, you can use unique abbreviations if it is possible to access the file, thus verifying the abbreviated names. If there is no connection to the remote system, however, you must specify a file to give an exact match with the file name given when the request was first entered.

Further descriptions of the changes in the spooling system, are given on pages 12-17 ("Modified commands") and page 23 ("Modified Monitor Calls").

Note that when using the `@APPEND-SPOOLING-FILE` command on a remote computer, the file is not copied to the remote computer. When the file is to be printed, the contents of the file is transferred page by page by COSMOS remote file access.

Also note that the `@LIST-SPOOLING-QUEUE` command does not support remote files.

Furthermore, note that if you try to define spooling files for COSMOS Spooling with the same name as spooling files for a local spooling program, an error message is printed.

---
Norsk Data ND-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 138

# 9. TIME SLICING

Any RT-program can now be time sliced. Two new commands are introduced in the SINTRAN-Service-Program to make this possible:

- *INSERT-PROGRAM-IN-TIME-SLICE
- *REMOVE-PROGRAM-FROM-TIME-SLICE

An RT-program will enter the time slice at the highest priority in the selected time slice class when started (MON RT or QRT).

Note that background programs still must be removed/inserted by the commands:

- *INSERT-IN-TIME-SLICE
- *REMOVE-FROM-TIME-SLICE

On the VSX-version, 16 time slice classes are available. By default, the 6 classes 0-5 are defined, the rest are free to use. Also by default, the time slice elements 0-27₈ are used to define the standard time slice parameters; elements 30-77₈ are free to use.

Otherwise, there are no changes to the time slicing compared to the J-version.

---

## Page 139

# SINTRAN III K-VERSION, SYSTEM LAYOUT (VSX)

## 10.1 Physical Memory Layout

|                  | During start-up |  |  | (size) |  |  |  | (size) | Normal run status |  |
|------------------|-----------------|--|--|--------|--|--|--|--------|-------------------|--|
| 0                |                 |  |  | 11k    |  |  |  | 11k    | Common code       |  |
| 12₈              | Common code     |  |  |        |  |  |  |        |                   |  |
| Restart routines | <6k             |  |  | <6k    |  |  |  | Restart routines        |  |
| ('pof' code)     |                 |  |  |        |  |  |  | ('pof' code)            |  |
|                  | Start program   | >7k |  |        | Reg.block+bitmap          |    |  |
| 30₈              |                 |    |  |        |  |  |  | >10k   | 37k               |  |
|                  | Resident data   | 35k |  | 1k     | Memtof                    |    |  |
| end of           | unused          | 2k  |  | 2k     | unused                    |    |  |
| bank 1           | buffer area*    | 0-xk|  | 0-xk   | buffer area*              |    |  |
|                  | RPIT            | <53k|  | <53k   | RPIT                      |    |  |
| within           | buffer area*    | 0-xk|  | 0-xk   | buffer area*              |    |  |
| one              | MPIT            | <52k|  | <52k   | MPIT                      |    |  |
| bank             | buffer area*    | 0-xk|  | 0-xk   | buffer area*              |    |  |
|                  | segment table   | <64k|  | <64k   | segment table             |    |  |
|                  | buffer area*    | 0-xk|  | 0-xk   | buffer area*              |    |  |
| bank             | memory map      | <64k|  | <64k   | memory map                |    |  |
| border           | buffer area*    | 0-xk|  | 0-xk   | buffer area*              |    |  |

*) Buffer areas are used for big terminal data fields and other non-PIT data.

Note that common code always starts at physical address 0 and that resident data (DPIT) logical address 4000 starts at physical address 60000. All resident code is mapped as segments and is accessible through the segment table.

Logical device table is found in bank no. LOGDBANK at addresses found in the CNVRT array in DPIT.

---

## Page 140

# 10.2 SYSTEM LAYOUT ON DISK

| File         | Contents        | Start address | Size | Disk addr. | Macro displ. | Patch macro |
|--------------|-----------------|---------------|------|------------|--------------|-------------|
|              | Common Code     | 1             |      |            |              |             |
| SINT RAN:    | Start Restart   |               |      | )GJEM )HENT|              |             |
| DATA         | Resident Data   |               |      | area       |              |             |
|              | Error Program   | 30 000        | 12k  | 100        | - 13         | PERRP       |
| MACM-AREA:   | End Resid. data | 112 000       | 2k   | 112        |              | P2RDA       |
| DATA         | System segment  | 130 000       | 3k   | 114        | - 54         | PSYSG       |
|              | Memtof          | 172 000       | 1k   | 117        |              |             |
|              | RT-Loader       | 30 000        | 41k  | 137        | - 14         | PRTLO       |

| SEG FILO:    | Common Code     | 0             | 13k  | 200        | 0            | PCCST       |
| DATA         | Start Restart   | 26 000        | 20k  | 213        |              |             |
|              | Resident Data   | 4 000         | 43k  | 233        | - 2          | PRDAT       |
|              | End resid. Data | 112 000       | 2k   | 277        |              |             |
|              | System Segment  | 130 000       | 3k   | 301        |              |             |
|              | Spooling Dataf. | 150 000       | 1k   | 304        | - 64         | PSPDF       |
|              | RPIT            | 26 000        | 65k  | 305        | - 13         | PRPIT       |
|              | MPIT            | 26 000        | 65k  | 372        | - 13         | PMPIT       |
|              | Segment Table   | 0             | 20k  | 457        | 0            | PSGTB       |
|              | File System     | 26 000        | 65k  | 477        | - 13         | PFILS       |
|              | Command-Segment | 26 000        | 65k  | 564        | - 13         | POPCO       |
|              | 5PIT            | 26 000        | 5k   | 651        | - 13         | P5PIT       |
|              | ND-500 Monitor  | 40 000        | 60k  | 656        | - 20         |             |
|              |                 |               |      | 736        |              |             |

---

## Page 141

# SINTRAN III RELEASE INFORMATION, K-VERSION  
SINTRAN III K-VERSION, SYSTEM LAYOUT (VSX)

## 10.3 PAGE INDEX TABLE LAYOUT

| RPIT=10                      | SPIT=11                             | FPIT=4                  | 5PIT=5      | XPIT=6     | MPIT=12                            |
|------------------------------|-------------------------------------|-------------------------|-------------|------------|------------------------------------|
| Micro-common                 | µ© (2K)                             | µ©                      | µ©          | µ©         | µ©                                 |
| Common code                  | © (9K)                              | ©                       | ©           | ©          | ©                                  |
| Monitor calls                | Edit routines                       | File system segment     | MON 60      | XMSG       | Resident code:                      |
| Resident code:               | Command segment,                    |                         | ND-500 monitor |         | M-level (monitor level)            |
| B-level (lev. 4)             | RT-load.                            |                         |             |            | S-level (Segadm. lev.)(*)          |
| S-level (Segadm. lev.)(*)    | DMAC                                |                         |             |            | level-10                           |
| Buffers                      | Error program                       |                         |             |            | level-11                           |
|                              |                                     |                         |             |            | level-12                           |
|                              |                                     |                         |             |            | level-13                           |
|                              |                                     |                         |             |            | level-14                           |
|                              |                                     |                         |             |            | Buffers                            |

| DPIT=7               | POF                        | X5DPT=13+14                                       | FUPIT=3                | DTPIT=17        | UPITN=1        | UPITA=2                           |
|----------------------|----------------------------|--------------------------------------------------|------------------------|-----------------|----------------|-----------------------------------|
| ![Micro Symbol]      | µ©            | ND-500 name segments (PIT 13)                    | µ©         | Direct tasks    | Users normal   | Users altern.                    |
| Resident common data | Start-program base(1k)     | ND-500 standard domain segment (PIT 14)          | Remote file user PIT  |                 | PIT (UPITN)    | PIT (UPITA)                      |
| (37k)                | Restart code               |                                                  |                       |                 |                |                                   |
| wind.BF              | Start code                 |                                                  |                       |                 |                |                                   |
| wind.NS              | Register blocks            |                                                  |                       |                 |                |                                   |
| wind.10              | Bitmaps                    |                                                  |                       |                 |                |                                   |
| wind.12              | 66000/                     |                                                  |                       |                 |                |                                   |
| wind.1/4             |                            |                                                  |                       |                 |                |                                   |
| System segment (8k)  |                            |                                                  |                       |                 |                |                                   |
| Data segment (12k)   |                            |                                                  |                       |                 |                |                                   |

(*) The segment administration routines (SegAdm) running on ring 3 are found on MPIT in generations prior to 500, on RPIT in generation 500 and later.

Norsk Data NO-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 142

# SINTRAN III RELEASE INFORMATION, K-VERSION  
SINTRAN III K-VERSION, SYSTEM LAYOUT (VSX)

Note that (almost) all code must run in two-bank mode. Some code must even switch between one-bank and two-bank mode in order to access all its data (or use physical memory load and store instructions). All system code will use DPIT as alternative page table.

## Common code (Θ)

The common code part contains the routines that can be called from more than one PIT.

The common code should not exceed 11 K of code (0-25777₈).

## µΘ (micro common)

This part of common is also present in the data PIT (DPIT). It is mainly used for parameter fetching and other operations on the user's data area.

## Resident code (RPIT)

This part contains code for most SINTRAN monitor calls except a few, which are placed on SPIT. File-system monitor calls are processed in the file system PIT. Other resident code that today is found in part 2 of resident should also be in this PIT, e.g., TAD resident code, resident RT-programs, configuration dependent code and “PIT3” code.

### 500

In generations 500 and later, the segments administration (SegAdm) routines running on ring 3 are also placed on RPIT.

OUTBT/INBT level code is here. Buffers accessed with RBGET/RBPUT are at the top of this PIT (they are also in MPIT).

## Monitor PIT (MPIT)

Here is all code for:

- monitor level
- internal interrupts (level 14)
- drivers for levels 10 to 13.

### 500

- segadm level (generations prior to generation 500 only).

Note that the part of this PIT that contains segadm is on ring 3. This makes it possible to run nearly always with paging on.

Buffers accessed with RBGET/RBPUT are at the top of this PIT (they are also in RPIT).

## SINTRAN PIT (SPIT)

In this page index table we find the command, RT-Loader and DMAC segments. A segment will be removed from this PIT only when another segment must be entered. Note that the first page of the segment area (page 138₈) always contains the Edit routine with its related routines.  

Norsk Data N0-60.230.5 EN

---

## Page 143

# SINTRAN III RELEASE INFORMATION, K-VERSION
SINTRAN III K-VERSION, SYSTEM LAYOUT (VSX)

## File system and file user, ND500, XMSG PITs (FPIT, FUPIT, 5PIT, XPIT)

These PITs each (currently) contain a single segment only, and a special strategy is applied to the setting and clearing of these page index tables to minimize context switch overhead.

## ND-500 name- and standard domain segment PITs (X5DPT)

These PITs are used for the ND-500 name segment and standard domain segments. The last page of these PITs are used as a window to the ND-500 Monitor stack page on the ND-500 user's data segment.

## Data PIT (DPIT)

The data PIT contains the resident common data, as RT-descriptions, data fields and system global variables. The background system segments are placed in this PIT, as well as the ND-500 data segments and various file system segments. All windows are in this PIT. µ© is also included here.

## User page index tables (UPITN, UPITA, DTPIT)

Three page index tables are reserved for the users. Two for background and RT-programs (normal and alternative PIT) and one for direct tasks.

## Non-PIT data

The following data is not in any PIT:

| Data                           |
|--------------------------------|
| Segment table                  |
| memory map                     |
| RT-programs’ register block and bit map |
| "big" terminal (TAD) data fields        |
| ND-500 mail boxes              |
| logical device number tables   |
| ND-500 communication buffers (for MON 60) |

Norsk Data ND-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 144

# 10.4 DATA STRUCTURES

Segment data structures and operations:

The memory map always starts on a bank boundary. Physical page multiplied by four is the displacement into memory map, i.e., a memory map pointer can always be divided by four. If the two lower bits of the PREVIOUS pointer are non-zero, this entry is the first element in a page list and the upper 14 bits of the pointer contains the segment number this page list belongs to. The end of the page list is marked by a zero in the PAGLINK pointer.

## Memory map element:

|     |         |                                    |
|-----|---------|------------------------------------|
| 0   | PAGLINK | Next page in page link.            |
| 1   | PREVIOUS | Previous page in link.            |
| 2   | PROTECT | Protect and status word (see below) |
| 3   | LOGPAGE | Logical page number.               |

## Segment table entry:

|     |          |                                      |
|-----|----------|--------------------------------------|
| 0   | SEGLINK  | Link through active segments         |
| 1   | PRESEG   | Previous segment in link             |
| 2   | LOGADR   | First logical page of the segment    |
| 3   | SEGLENGTH | Length of the segment in pages      |
| 4   | MADR     | Address of segment within the segment file |
| 5   | FLAG     | Flag word (see below)                |
| 6   | SGSTATUS | Segment status and protect word (see below) |
| 7   | BPAGLINK | Pointer to the page list of this segment |

---

## Page 145

# SINTRAN III RELEASE INFORMATION, K-VERSION
SINTRAN III K-VERSION, SYSTEM LAYOUT (VSX)

## Format of SGSTATUS and PROTECT

### Flags

- 5NCLSEG: Never clear PIT for this kind of segment. Pit entries are set up and cleared whenever a page is given to/taken from the segment.
- 5FIX: Segment is fixed
- 5CMINH: This memory map element must not be used
- 5SPTFIXED: Segment is fixed in page table
- 5SMSYS: This memory map element is used for system area.
- 5MRES: Memory map element is reserved (with FIXC)
- 5CMIDUM: This memory map element is in the free list.

| Bits | Description                  |
|------|------------------------------|
| 07   | Protection ring              |
| 06   | 5PGU: Page used.             |
| 05   | 5WIP: Written in page.       |
| 04   | 5FPM: Fetch permit           |
| 03   | 5RPM: Read permit            |
| 02   | 5WPM: Write permit           |
| 17-10| -                            |

## Format of FLAG

- 50K: Segment is ready
- 51NH8: Segment not built
- 5SYSSEG: System segment
- 5SPROT: Protected segment
- 5REEP: Reentrant subsystem segment
- 5FIXC: Segment is fixed contiguously
- 5DEMAND: Demand segment

| Bits | Description                          |
|------|--------------------------------------|
| 07-00| Segment file number for this segment |
| 17-10| -                                    |

## CPU Values

The following values are held in the CPU (on ND-110/CX and ND-120/CX only) and loaded at system start:

- INTEGER CORMBANK % Bank number of memory map
- INTEGER SEGTBANK % Bank number of segment table
- INTEGER SEGISTART % Displacement of segment table within bank

Norsk Data ND-60.230.5 EN

_Scanned by Jonny Oddene for Sintran Data © 2021_

---

## Page 146

# SINTRAN III RELEASE INFORMATION, K-VERSION

## SINTRAN III K-VERSION, SYSTEM LAYOUT (VSX)

The RT-description looks like this:

### Displacement (octal)

| Octal | Code     | Description                                 |
|-------|----------|---------------------------------------------|
| 0     | TLINK    | Time queue                                  |
| 1     | STATUS   | Status bits (see below)                     |
| 2     | INPRITY  | Initial program priority                    |
| 3     | PRITY    | Program priority                            |
| 4     | DTIM1    | Start time                                  |
| 5     | DTIM2    | Start time                                  |
| 6     | DTIN1    | Interval                                    |
| 7     | DTIN2    | Interval                                    |
| 10    | STADR    | Start address                               |
| 11    | SEGM1    | Initial segments                            |
| 12    | SEGM2    | Initial segments                            |
| 13    | WLINK    | Waiting queue, execution queue              |
| 14    | ACT1SEG  | Actual segments                             |
| 15    | ACT2SEG  | Actual segments                             |
| 16    | INIPRI   | Initial page tables and ring                |
| 17    | ACTPRI   | Actual page tables and ring (only)          |
| 20    | BRESLINK | Beginning of reservation link               |
| 21    | RSEGM    | Reentrant segment                           |
| 22    | BUFWINDOW| Buffer window                               |
| 23    | TRMWINDOW| Terminal window, RT working field window    |
| 24    | N5WINDOW | ND-500 mailbox window                       |
| 25    | RTDLGADDR| Logical address of register block           |

The register block and bit map are unchanged from the J-version.

---

## Page 147

# SINTRAN III RELEASE INFORMATION, K-VERSION

## SINTRAN III K-VERSION, SYSTEM LAYOUT (VSX)

### Format of STATUS:

- **5BACKGR**: Background program
- **5USED**: This RT-description is in use
- **5TLSLICED**: This RT-program is time sliced
- **5ESCF**: Time slicer flag
- **5BRKF**: " "
- **5PPRF**: " "
- **5XMSY**: XMSG has set the 5WAIT bit
- **5NOABORT**: Delayed abort (wait for I/O to complete)

```

```
    17   16   15   14   13   12   11   10           09   08   07   06   05   04   03   02   01   00
    -----------------------------------------------------------------
    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |
    -----------------------------------------------------------------

- **5SWWAIT**: In swapping queue
- **5RTOFF**: Start inhibited
- **5TMOUT**: TMOUT has been used
- **5ABS**: ABSET has been used
- **5INT**: INTV has been used
- **5RWAIT**: RTWT or HOLD has been used
- **5REP**: Repeat execution
- **5WAIT**: I/O wait

### Format of INIPRI and ACTPRI:

| 0 | Normal PIT | Alternative PIT | Level always = 1 | Ring |
|---|------------|-----------------|------------------|------|

```

```
    17   16   15   14   13   12   11   10    09   08   07   06   05   04   03   02   01   00
    ----------------------------------------------------------------
    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |
    ----------------------------------------------------------------
```

---

## Page 148

# SINTRAN III RELEASE INFORMATION, K-VERSION  
SINTRAN III K-VERSION, SYSTEM LAYOUT (VSX)

The common part of all data fields is shown below:

## Displacement

| 0 | RESLINK | Reservation link |
|---|---------|------------------|
| 1 | RTRES   | Reserving RT-program |
| 2 | BWLINK  | Beginning of waiting queue |
| 3 | TYPRING | Device type and ring |

All data fields have a common part which is placed in the DPIT segment, but some device-dependent parts of data fields may be placed elsewhere in physical memory. The 5SPLTDF bit (bit no. 13 of the TYPRING word is set to indicate that the data field is split.

## Format of TYPRING:

- **5CLDV**: Clear device routine available  
  (@CLEAR-DEVICE may be used)
- **5NORES**: No reservation necessary  
  (before using device)
- **5BAD**: Terminal access device (TAD)
- **5TERM**: Terminal
- **5IBDV**: Internal block device
- **5INVRT**: Invert digital I/O

```
17 16 15 14 13 12 11 10         07 06 05 04 03 02 01 00
|  |  |  |  |  |  |  |           |  |  |  |  |  |  |  |
```

- **5FLOP**: Floppy disk
- **5MT**: Magnetic tape
- **M1448**: Block calls allowed
- **5SPLTDF**: Split data field  
  (a part outside DPIT)
- **5ISET**: IOSET allowed
- **5CONCT**: CONCT allowed
- **5RFILE**: mass storage file
- **5IOBT**: INBT/OUBT allowed

Norsk Data ND-60.230.5 EN

---

## Page 149

# SINTRAN III RELEASE INFORMATION, K-VERSION
SINTRAN III K-VERSION, SYSTEM LAYOUT (VSX)

## 10.5 INTERRUPT LEVEL USAGE (VSX)

| Level | Description                                      |
|-------|--------------------------------------------------|
| 15    | Extremely fast user interrupts                   |
| 14    | Internal interrupts                              |
| 13    | Real Time Clock, HDLC drivers                    |
| 12    | Terminal Input & ND-100 - ND-500 Communication   |
| 11    | Mass storage Input/Output                        |
| 10    | Terminal output                                  |
| 9     |                                                  |
| 8     |                                                  |
| 7     | Direct tasks                                     |
| 6     |                                                  |
| 5     | XMSG                                             |
| 4     | I/O Monitor calls                                |
| 3     | Segment administration                           |
| 2     | SINTRAN III Monitor                              |
| 1     | Real time programs and Background programs       |
| 0     | Idle loop                                        |

Note the changed use of levels 2 and 3.

Norsk Data ND-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 150

# 10.6 System Included Segments (VSX)

## Segment

| No. | Name     | Address Range | PIT | Description                                   |
|-----|----------|---------------|-----|-----------------------------------------------|
| 2   | S3IMAGE  | 0 - 65777     | 1   | Memory image of COMMON code, Start/restart program |
| 3   | S3COM    | 30000 - 177777| 11  | Command segment                               |
| 4   | S3RTL    | 30000 - 123777| 11  | RT-Loader segment                             |
| 5   | S3ERRS   | 130000 - 131777| 7  | System segment for error program              |
| 6   | S3FS     | 26000 - 177777| 4   | File system segment                           |
| 7   | S3DMAC   | 64000 - 153777| 11  | DMAC segment                                  |
| 10  | S3RTFIL  | 0 - 177777    | 2   | RTFIL segment                                 |
| 11  | S3ERRL   | 0 - 17777     | 1   | Error log segment                             |
| 12  | S3FSV    | 26000 - 177777| 1   | Initial file system segment                   |
| 13  | S3OPCSV  | 26000 - 177777| 1   | Initial command segment                       |
| 14  | S3ERRP   | 3000 - 53777  | 11  | Error program segment                         |
| 15  | S3BFLY   | 26000 - 26000 |     | Reserved for system extension                 |
| 16  | S3SRPIT  | 26000 - 177777| 1   | Initial RPIT (save area)                      |
| 17  | S3SMPIT  | 26000 - 177777| 1   | Initial MPIT (save area)                      |
| 20  | S3SDTS   | 0 - 175777    | 14  | ND-500 standard domains segment               |
| 21  | S3NMSS   | 0 - 17577     | 13  | ND-500 name tables segment                    |
| 22  | S3RFAC   | 260000 - 155773| 3  | Remote File Access segment                    |
| 23  | S3DPIT   | 4000 - 10777  | 7   | DPIT segment (global data)                    |
| 24  | S3SGIT   | 0 - 37777     | 1   | Initial segment table                         |
| 25  | S3IRPIT  | 26000 - 177777| 1   | Image of RPIT                                 |
| 26  | S3IMPIT  | 26000 - 177777| 1   | Image of MPIT                                 |
| 27  | S3ISGT   | 0 - 37777     | 1   | Image of segment table                        |
| 30  | S3SMS    | 40000 - 177775| 5   | ND-500 System Monitor segment                 |
| 31  | S3SSPD   | 150000 - 151777| 7  | Initial spooling data fields                  |
| 32  | S3ATRACC |               |     | Reserved, but not used                        |
| 33  | S3XMSG   | 120000 - 177777| 2  | XMSG POF segment                              |
| 34  | S3XMSGD  | 0 - 177772    | 2   | XMSG demand segment (XROUT)                   |
| 0   | [       |               |     |                                               |
| 36  | S3MPIT   | 26000 - 161177| 12  | MPIT segment                                  |
| 36  | S3TAD    | 110000 - 133377| 11 | TADADM segment                                |
| 37  | S3RTD    | 0 - 177777    | 1   | RT-Loader data segment                        |
| 40  | S3UFDT   | 150000 - 157777| 7  | File User data segment for RT-prog            |
| 41  | S3IMED   | 26000 - 27777 | 1   | Image of EDIT routines                        |
| 42  | S3ED     | 26000 - 27777 | 11  | EDIT routines                                 |
| 43  | S3PATCH  | 174000 - 177777| 2  | Reserved for internal use by ND               |
| 44  | S3IDPIT  | 4000 - 111777 | 1   | Memory image of system data (DPIT)            |
| 45  | S3SYS   | 130000 - 135777| 1   | Memory image of system segment                |
| 46  | S3SSPIT  | 26000 - 37777 | 1   | Save of SPIT segment                          |
| 0   | [       |               |     |                                               |
| 47  | S3SRPIT  | 26000 - 145777| 10  | RPIT segment                                  |
| 50  | S3ISPIT  | 26000 - 37777 | 1   | Image of SPIT segment                         |
| 51  | S3SPIT   | 26000 - 37777 | 5   | SPIT segment                                  |
| 52  | S3SAVE   | 0 - 65777     | 1   | Save of common code and start prog            |
| 53  | S3DSOPT  | 4000 - 11177  | 1   | Save of DPIT                                  |
| 54  | S3SYSX  | 130000 - 135777| 1   | Save of system segment                        |
| 55  | S3SERRP  | 3000 - 53777  | 1   | Save of error program                         |
| 56  | S3RTIC  | 30000 - 67777  | 1   | Save of RT-Loader's code segment              |
| 57  | S3SRTD  | 0 - 25777      | 1   | Save of RT-Loader's data segment              |
| 60  | S3EDR   | 112000 - 115777| 1   | Save of DPIT last two pages                   |
| 61  | S3IERD  | 112000 - 115777| 1   | Image of DPIT last two pages                  |
| 62  | S3SMSB  | 40000 - 177777 | 1   | Save of ND-500 Monitor                        |
| 63  | S3MEMT  | 172000 - 173777| 1   | Memtof                                        |
| 64  | S3ERD   | 112000 - 115777| 7   | DPIT last two pages                           |

---

## Page 151

# SINTRAN III RELEASE INFORMATION, K-VERSION
SINTRAN III K-VERSION, SYSTEM LAYOUT (VSX)

Note: Segments 2-64 will be given standard segment names the first time the RT-Loader is entered.

All system included segments are placed on segment file number 0 (SEGFILE0:DATA), except segments 52-60 and 63 which are placed on the files SINTRAN:DATA and MACM-AREA:DATA.

## 10.7 SYSTEM INCLUDED RT-PROGRAMS (VSX and VSE)

| PROGRAM | PURPOSE |
|---------|---------|
| 1SWAP   | Queuing program requests for swapping |
| 5SWAP   | Performs ABSTR in ND-100 for the ND-500 Swapper |
| ACCRT   | RT accounting |
| BAKnn   | Background process for terminal (BAK01-BAK99) |
| BKnnn   | " " - (BK100-BK128) |
| BCHnn   | Batch process |
| BPTMP   | Timeout program for background allocation system |
| COSPO   | COSMOS-spooling server |
| DUMM2   | Dummy program used by the spooling system |
| DUMMY   | Dummy program to prevent empty execution queue |
| FDRT1   | Transfer data between interface buffer and memory. Floppy formatting. (FLOPPY-1) |
| FDRT2   | Transfer data between interface buffer and memory. Floppy formatting. (FLOPPY-2) |
| FIXRT   | Monitor call/command FIXC execution |
| RTDIL   | Buffer transfer program for DISC-ACCESS-LOG |
| RTER    | Output error messages |
| RTFRA   | Does remote file access for RT-programs (COSMOS - remote file access) |
| RTSLI   | Time slicer. Changes priority on all time sliced processes. |
| RWRT1   | Block data transfer. Activated from RFILE/WFILE/RPAGE/WPAGE for RT-programs |
| RWRT2   | Open file from RT-programs |
| RWRT3   | Block transfer on MAG-TAPE-1 (MAGTP) |
| RWRT5   | VERSATEC-1 DMA |
| RWRT6   | CDC-DMA LINK |
| RWRT7   | MAG-TAPE-2 |
| RWRT8   | VERSATEC-2 DMA |
| RWRT9   | FLOPPY-DISC 1 |
| RWRT10  | FLOPPY-DISC 2 |
| RWRT11  | LINE-PRINTER/VERSATEC -1 I/O |
| RWRT12  | LINE-PRINTER/VERSATEC -2 I/O |
| RWRT13  | Block-oriented internal device 1 INPUT |
| RWRT20  | Block-oriented internal device 1 OUTPUT |
| RWRT14  | Block-oriented internal device 2 INPUT |
| RWRT21  | Block-oriented internal device 2 OUTPUT |
| RWRT15  | Block-oriented internal device 3 INPUT |
| RWRT22  | Block-oriented internal device 3 OUTPUT |
| RWRT16  | Block-oriented internal device 4 INPUT |
| RWRT23  | Block-oriented internal device 4 OUTPUT |
| RWRT17  | Block-oriented internal device 5 INPUT |
| RWRT24  | Block-oriented internal device 5 OUTPUT |

Norsk Data ND-60.230.5 EN

---

## Page 152

# SINTRAN III RELEASE INFORMATION, K-VERSION
## SINTRAN III K-VERSION, SYSTEM LAYOUT (VSX)

| **Code** | **Description**          |
|----------|--------------------------|
| RWRT25   | HASP DMA 1 INPUT         |
| RWRT26   | HASP DMA 1 OUTPUT        |
| RWRT27   | HASP DMA 2 INPUT         |
| RWRT28   | HASP DMA 2 OUTPUT        |
| RWRT29   | HASP DMA 3 INPUT         |
| RWRT30   | HASP DMA 3 OUTPUT        |
| RWRT31   | HASP DMA 4 INPUT         |
| RWRT32   | HASP DMA 4 OUTPUT        |
| RWRT33   | HASP DMA 5 INPUT         |
| RWRT34   | HASP DMA 5 OUTPUT        |
| RWRT35   | HASP DMA 6 INPUT         |
| RWRT36   | HASP DMA 6 OUTPUT        |
| SPRtn    | Spooling programs (1-9)  |
| SPRnn    | Spooling programs (10-30)|
| STSIN    | Initialize SINTRAN III and start systems RT-programs |
| TADnn    | Background process for Terminal Access Device |
| TADAD    | Administers connections to TADs from requesting users. |
| TERMP    | Starts the user defined "clean-up" RT-program when RT-programs are aborted (if enabled) |
| TIMRT    | Timer RT-program. Start timeout-routine for all devices in timer-table. |
| UDRnn    | Performs Fast Universal DMA for user processes. |
| DIMWD    | Used by the disk mirroring facility which is part of the REVIVE Fault Tolerant eXtension. |

---

## Page 153

# SINTRAN III K-Version. System Layout (VSE)

## 11.1 Physical Memory

| Address  | Description                                           |
|----------|-------------------------------------------------------|
| 000000   | System resident and file system resident              |
| 022000   | Swapping area                                         |
| 030000   | Open file table for RT-programs                       |
| GNSTA    |                                                       |
| 036000   | Configuration dependent system resident               |
| ...7ENDC | Possible swapping area                                |
| 9POFS    |                                                       |
| 110000   | Paging-Off area (POF)                                 |
| ...9EMRE | IO-buffers + RT-descr                                 |
| ...9EIOB | Possible swapping area                                |
| 177000   | Page tables                                           |

---

## Page 154

# SINTRAN III RELEASE INFORMATION, K-VERSION
## SINTRAN III K-VERSION, SYSTEM LAYOUT (VSE)

### 11.2 PAGE INDEX TABLE 0

| ADDRESS | DESCRIPTION |
|---------|-------------|
| 000000  | System resident |
| 002000  | File system resident |
| 006000  | System resident (config. independent) |
| 022000  | Device buffer window |
| 024000  | System segment |
| 036000  | System resident (config. dependent) |
| ...7ENDC... | Possibly free |
| 110000  | Segment area (file system segments, command segment, RT-Loader etc.) |
| 174000  | User window |

All resident pages are mapped physical page equal to logical page.

---

## Page 155

# SINTRAN III RELEASE INFORMATION, K-VERSION
## SINTRAN III K-VERSION, SYSTEM LAYOUT (VSE)

### 11.3 PAGE INDEX TABLES 1 AND 2

|      | PIT 1             |                                | PIT 2             |
|------|-------------------|--------------------------------|-------------------|
|      | RT-PROGS. BACKGROUND |                                | RT-PROGS. BACKGROUND |
|      |                   |                                |                   |
|      | Program and data  |                                |                   |
|      | bank.             |                                |                   |
|      |                   | Program bank                    | Normally          |
|      |                   | when running 2-bank.            | not used          |
|      |                   |                                | (can be           |
|      |                   | Program and data                | used for          |
|      |                   | when running 1-bank.            | program           |
|      |                   |                                | and               |
|      |                   |                                | data).            |
|      |                   |                                |                   |
|      | ......            |                                | Data bank         |
|      | RTCOMMON          |                                | when running 2-bank |
|      | (demand)          |                                |                   |

Norsk Data ND-60.230.5 EN

---

## Page 156

# 11.4 PAGE INDEX TABLE 3

```
PIT 3
 ├── Equal to PIT 0 
 │   (logical addr. equal to physical addr.).
 ├── ...7ENDC...
 │   └── Not used
 ├── ...9POFS...
 │   └── Terminal I/O routines
 ├── ...9EPT3...
 ├── PIT3-segment 
 │   (SEGMENT 41)
 ├── ...........
 │   └── Not used
 ├── ............
 ├── XMSG segment
 │   (segment 33)
 ├── ............
 │   └── Not used
```

---

## Page 157

# SINTRAN III RELEASE INFORMATION, K-VERSION  
SINTRAN III K-VERSION, SYSTEM LAYOUT (VSE)

## 11.5 SYSTEM LAYOUT ON DISK

### 11.5.1 SINTRAN:DATA

Disk address in pages

| 0 | 1 |
|---|---|
| Resident and "POF" | Directory entry |

### 11.5.2 MACM-AREA:DATA

#### 11.5.2.1 LAYOUT

Disk address in pages (octal), relative to the start of the directory

| 100 | 137 | 145 | 177 |
|-----|-----|-----|-----|
| | File system, segments 6 and 24 | | |
| | Error program | | |
| Command segment (segment 3) | | | |

#### 11.5.2.2 DISPLACEMENTS WHEN PATCHING

- Command segment: `-110000`
- File system seg. 6 + 24: `2000`

### 11.5.3 SEGFILO:DATA

Disk address in pages (octal), relative to the start of the directory.

| 200 | 277 |
|-----|-----|
| Memory image (segment 2) |

Other segment files can reside in any directory at any disk address. The maximum size of a segment file is 16383 pages. Due to limitations in the RT-Loader, the sum of the used segment files cannot be greater than 32768 pages.

---

## Page 158

# 11.5.4 Interrupt Level Usage (VSE)

| Level | Description                               |
|-------|-------------------------------------------|
| 15    | Extremely fast user interrupts            |
| 14    | Internal interrupts                       |
| 13    | Real Time Clock, HDLC drivers             |
| 12    | Terminal Input                            |
| 11    | Mass storage Input/Output                 |
| 10    | Terminal output                           |
| 9     |                                           |
| 8     |                                           |
| 7     | Direct tasks                              |
| 6     |                                           |
| 5     | XMSG                                      |
| 4     | I/O Monitor calls                         |
| 3     | SINTRAN III Monitor                       |
| 2     | Direct tasks                              |
| 1     | Real time programs and Background programs|
| 0     | Idle loop                                 |

This is the same as in the J-version.

---

## Page 159

# SINTRAN III RELEASE INFORMATION, K-VERSION

## SINTRAN III K-VERSION, SYSTEM LAYOUT (VSE)

### 11.5.5 SYSTEM INCLUDED SEGMENTS (VSE)

Note: Segments 2–43 will be given standard segment names the first time the RT-Loader is entered.

| SEGMENT NO. | NAME     | ADDRESS RANGE | PIT | DESCRIPTION                                 |
|-------------|----------|---------------|-----|---------------------------------------------|
| 2           | S3IMAGE  | 0 - 175777    | 1   | Memory, image and POF                       |
| 3           | S3COM    | 110000 - 173777| 0   | Command segment                             |
| 4           | S3RTL    | 110000 - 147777| 0   | RT-Loader                                   |
| 5           | S3ERRS   | 26000 - 31777 | 0   | Error program "system segment"              |
| 6           | S3FSCOM  | 110000 - 137777| 0   | File system common segment                  |
| 7           | S3DMAC   | 110000 - 153777| 0   | DMAC segment                                |
| 10          | S3RTFIL  | 0 - 177777    | 2   | RTFIL segment                               |
| 11          | S3ERRL   | 0 - 17777     | 1   | Error log segment                           |
| 12          | S3FS2SV  | 140000 - 173777| 0   | Initial reentrant file system seg.2         |
| 13          | S3RTLSV  | 56000 - 147777| 1   | Initial RT-Loader segment                   |
| 14          | S3ERRP   | 110000 - 123777| 0   | Error program segment                       |
| 15          | S3SMSV   | 110000 - 173777| 0   | Initial service program and mail            |
| 16          | S3IOMT   | 150000 - 167777| 0   | Initial IOMTY segment                       |
| 17          | S3PT3SV  | 0 - 27777     | 1   | Initial PIT3 segment                        |
| 20          | S3SDT5   |               |     | Reserved, but not used                      |
| 21          | S3NMS5   |               |     | Reserved, but not used                      |
| 22          | S3RFUS1  | 110000 - 163777| 0   | Reentrant file user segment no. 1           |
| 23          | S3SMSEG  | 110000 - 173777| 0   | Service program and mail                    |
| 24          | S3FSRS1  | 140000 - 173777| 0   | File system reentrant segment no. 1         |
| 25          | S3FSRS2  | 140000 - 173777| 0   | File system reentrant segment no. 2         |
| 26          | S3RFUS2  | 110000 - 163777| 0   | Reentrant file user segment no. 2           |
| 27          | S3IOMTY  | 150000 - 167777| 0   | Segment for IOMTY mon. call                 |
| 30          | S3SMS1   |               |     | Reserved, but not used                      |
| 31          | S3SMS2   |               |     | Reserved, but not used                      |
| 32          | S3RTACC  | 110000 - 127777| 0   | RT accounting segment                       |
| 33          | S3XMSGP  | 140000 - 167777| 2   | XMSG POF segment                            |
| 34          | S3XMSGD  | 0 - 137777    | 2   | XMSG demand segment                         |
| 35          | S3XMSGR  | 0 - 37777     | 2   | Reserved for XMSG                           |
| 36          | S3TAD    | 110000 - 133777| 0   | TADADM segment                              |
| 37          | S3RTID   | 0 - 177777    | 1   | RT-Loader segment                           |
| 40          | S3FUDRT  | 164000 - 173777| 0   | File user data segment for RT-prog.         |
| 41          | S3PT3    | 116000 - 137777| 1   | Pit 3 segment                               |
| 42          | S3SPLSV  | 110000 - 137777| 0   | Initial spooling program segment            |
| 43          | S3SPL    | 110000 - 137777| 0   | Spooling program segment                    |

---

## Page 160

# 12. TERMINAL INPUT/OUTPUT

## 12.1 CHANGED DATA FIELDS - TERMINALS

### 12.1.1 TERMINAL INPUT AND OUTPUT DATA FIELD - VSX

#### INPUT DATA FIELD LAYOUT IN SINTRAN MEMORY AREA, PART OUTSIDE DPIT:

| Offset | Description                                         |
|--------|-----------------------------------------------------|
| -45    | TINFO Various information bits for terminal         |
| -44    | PECH7 Echo table                                    |
| -34    | PBRK7 Break table                                   |
| -24    | INSMSQ Address of ND-500 message when doing quick instring |
| -23    | RSISTE Echo pointer                                 |
| -22    | BRECHOFL Break & echo flag                          |
| -21    | ROUSPEC Address of special subroutine               |
| -20    | NCBRK Number of characters after last break         |
| -17    | CTTYP Terminal type                                 |
| -16    | CESC Disconnect and escape characters               |
| -15    | BRKMAX Maximum BHOLD before break                   |
| -14    | TSPEED Terminal speed                               |
| -13    | CNTREG Control register                             |
| -12    | DFLAG Device flag bits                              |
| -11    | ECHOTAB Pointer to echo table                       |
| -10    | BRKTAB Pointer to break table                       |
| -7     | LAST Last typed character                           |
| -6     | TMSUB Time out subroutine                           |
| -5     | TMR Time out counter                                |
| -4     | TTMR Start value of TMR                             |
| -3     | HDEV Hardware device number                         |
| -2     | STDRIV Driver start address                         |
| -1     | DRIVER Driver interrupt restart address             |
| 0      | TDRAODR Address of data field in resident           |
| 1      | XDFOPP Address of DFOPP in resident                 |
| 2      | XOPPDF Address of opposite data field (outside resident) |
| 3      | TYPRING Device type bits and ring                   |
| 4      | XONCR XON character, input control                  |
| 5      | XOFCR XOFF character, input control                 |
| 6      | Not used                                            |

To be continued

Norsk Data NO-60.230.5 EN

---

## Page 161

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## TERMINAL INPUT/OUTPUT

|   | Code      | Description                                                     |
|---|-----------|-----------------------------------------------------------------|
| 7 | IOTRANS   | Called from INBT/OUTBT to transfer                              |
| 10 | STDEV    | Start device routine                                            |
| 11 | SETDV    | IOSET routine                                                   |
| 12 | DFOPP    | Pointer to output channel data field                            |
| 13 | DERROR   | Error code                                                      |
| 14 | BUFST    | Start of ring buffer                                            |
| 15 | MAX      | Buffer capacity                                                 |
| 16 | BHOLD    | Number of characters in buffer                                  |
| 17 | HENTE    | Fetch pointer                                                   |
| 20 | CFREE    | Free positions                                                  |
| 21 | FYLLE    | Store pointer                                                   |
| 22 | BSTATE   | Background program state                                        |
| 23 | TSTATE   | Time slice state                                                |
| 24 | DBPROG   | Background RT-program                                           |
| 25 | DBADR    | Saved P-reg on escape and file system monitor calls             |
| 26 | RIFIL    | For mode input file number                                      |
| 27 | BCHISTS  | For mode input status                                           |
| 30 | DERO     | Error information                                               |
| 30 | BREGBLOCK | Register save at escape                                        |
| 32 | DER2     | Error information                                               |
| 40 | DBPREG   | P-register on page fault on IOBT level                          |
| 41 | DBACTPRI | ACTPRI on page fault on IOBT level                              |
| 42 | FLAGB    | Background flags                                                |
| 43 | EUSADD   | Address for user-escape handling                                |
| 44 | LUSADD   | Address for local-function handling                             |
| 45 | NBREAKS  | Number of break characters in buffer                            |
| 46 | CMWFIELD | Address of current monitor call working field                   |
| 47 | UACTPRI  | PCR-register when accessing caller's buffer                     |
| 50 | USADDR   | Address of caller's buffer                                      |
| 51 | XBUFST   | Logical window address to ring buffer                           |
| 52 | NCHARS   | Number of characters stored in caller's buffer                  |
| 53 | CPITENTRY| PIT-entry of terminal data field                                |
| 55 | BRKCHAR  | Break character                                                 |
| 56 | BRKMODE  | Break mode                                                      |

| Address | Value |
|---------|-------|
| 50      | 312   |
| 51      | +     |
| 52      | 406   |

Norsk Data ND-60.230.5 EN  
Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 162

# SINTRAN III RELEASE INFORMATION, K-VERSION
## TERMINAL INPUT/OUTPUT

### OUTPUT DATA FIELD LAYOUT IN SINTRAN MEMORY AREA, PART OUTSIDE DPIT

| Code  | Field     | Description                                       |
|-------|-----------|---------------------------------------------------|
| -10   | SCREEN    | Counter for stop on full page                     |
| -7    | EMPTFLAG  | Buffer empty flag                                 |
| -6    | TMSUB     | Timeout subroutine                                |
| -5    | TMR       | Timeout counter                                   |
| -4    | TTMR      | Start value of TMR                                |
| -3    | HDEV      | Hardware device number                            |
| -2    | STDRIV    | Driver start address                              |
| -1    | DRIVER    | Driver interrupt restart address                  |
| 0     | TDRADDR   | Address of data field in resident                 |
| 1     | XDFOPP    | Address of DFOPP in resident                      |
| 2     | XDFPOPDF  | Value to add to current data field address        |
| 3     | TYPRING   | Device type bits and ring                         |
| 4     | XONCR     | Xon character, input control                      |
| 5     | XOFCR     | Xoff character, input control                     |
| 6     | PDISPLAY  | Pointer to next terminal in display table         |
| 7     | IOTRANS   | Called from INBT/OUTBT to transfer                |
| 10    | STDEV     | Start device                                      |
| 11    | SETDV     | IOSET routine                                     |
| 12    | DFOPP     | Pointer to output channel data field              |
| 13    | DERORR    | Error code                                        |
| 14    | BUFS      | Start of ring buffer                              |
| 15    | MAX       | Buffer capacity                                   |
| 16    | BHOLD     | Number of characters in buffer                    |
| 17    | HENTE     | Fetch pointer                                     |
| 20    | CFREE     | Free positions                                    |
| 21    | FYLLE     | Store pointer                                     |
| 22    | MINBHOLD  | Lower limit for break                             |
| 23    | ROFIL     | For "mode" (output file number)                   |
| 24    | BCHOST    | For "mode" (output status)                        |
| 25    | ONSMSG    | Address for ND-500 message                        |
| 26    | CBUADR    | Current user buffer address (outstring)           |
| 27    | NOCHAR    | Number of bytes in outstring monitor call         |
| 30    | CNOCHAR   | Number of words left to transfer in outstring     |
| 31    | XNOCHAR   | Working location for outstring                    |
| 32    | ZOPRG     | P, X, T-registers in outstring                    |
| 35    | ZOARG     | A, D and L-registers in outstring                 |
| 40    | ZOSRG     | S, B-registers + old page in outstring            |
| 43    | SBHOLD    | Saved BHOLD in outstring                          |

---

## Page 163

# SINTRAN III RELEASE INFORMATION, K-VERSION

## TERMINAL INPUT/OUTPUT

### DATA FIELD LAYOUT IN SINTRAN MEMORY AREA, DPIT PART

This layout applies both to the input and output data fields.

| Value | Field      | Description                                      |
|-------|------------|--------------------------------------------------|
| -4    | TDFPHPAGE  | Physical page of data field                      |
| -3    | TDFLGADDR  | Address within a page of data fields             |
| -2    | STDRV      | Driver start address                             |
| -1    | DRIVER     | Driver interrupt restart address                 |
| 0     | RESLINK    | Reservation link                                 |
| 1     | RTRES      | Reserving RT-program                             |
| 2     | BWLINK     | Beginning of waiting queue                       |
| 3     | TYPRING    | Device type bits and ring                        |
| 4     | ISTATE     | 0 = active, 1 = I/O-wait, 2 = buffer wait,       |
| 5     | MLINK      | Monitor queue link -1 & -2 = nowait              |
| 6     | MFUNC      | Monitor level function address                   |

### INPUT DATA FIELD LAYOUT IN SINTRAN IMAGE AND SAVE AREA

| Value | Field      | Description                                        |
|-------|------------|----------------------------------------------------|
| -4    | ZDBPROG    | Background RT-program                              |
| -3    | HDEV       | Hardware device number                             |
| -2    | ZXONOFCR   | XON and XOFF chars. (XOFF in most significant byte)|
| -1    | ZDFLAG     | Device flag bits                                   |
| 0     | ZROUSPEC   | Address of special subroutine                      |
| 1     | ZCTTYP     | Terminal type                                      |
| 2     | ZCESCP     | Disconnect and escape characters.                  |
| 3     | TYPRING    | Device type bits and ring                          |
| 4     | ZTSPEED    | Terminal speed                                     |
| 5     | ZCNTREG    | Control register                                   |
| 6     | MFUNC      | Monitor level function address                     |

### OUTPUT DATA FIELD LAYOUT IN SINTRAN IMAGE AND SAVE AREA

| Value | Field     | Description                                        |
|-------|-----------|----------------------------------------------------|
| -4    | ZTINFO    | Various information bits for terminal              |
| -3    | HDEV      | Hardware device number                             |
| -2    | ZXONOFCR  | XON and XOFF chars. (XOFF in most significant byte)|
| -1    |           | Not used                                           |
| 0     |           | Not used                                           |
| 1     |           | Not used                                           |
| 2     |           | Not used                                           |
| 3     | TYPRING   | Device type bits and ring                          |
| 4     | ZDFTYP    | Data field type, 0 = terminal, 1 = Telefix R,      |
|       |           | 2 = Telefix B                                      |
| 5     |           | Not used                                           |
| 6     | MFUNC     | Monitor level function address                     |

Norsk Data N0-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 164

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## TERMINAL INPUT/OUTPUT  

### 12.1.2 TERMINAL INPUT AND OUTPUT DATA FIELD - VSE  

**INPUT DATA FIELD LAYOUT IN SINTRAN MEMORY, IMAGE AND SAVE AREA:**

| Offset | Field   | Description                                    |
|--------|---------|------------------------------------------------|
| -47    | IXONCR  | XON character, input control.                  |
| -46    | IXOFFCR | XOFF character, input control.                 |
| -45    | TINFO   | Various information bits for terminal          |
| -44    | PECH7   | Echo table                                     |
| -34    | PBRK7   | Break table                                    |
| -24    | IN5MSG  | Address of ND-500 message when doing quick instring |
| -23    | RSISTE  | Echo pointer                                   |
| -22    | BRECHOFL| Break & echo flag                              |
| -21    | ROUSPEC | Address of special subroutine                  |
| -20    | NCBRK   | Number of characters after last break          |
| -17    | CTYP    | Terminal type                                  |
| -16    | CESCP   | Disconnect and escape characters.              |
| -15    | BRKMAX  | Maximum BHOLD before break                     |
| -14    | TSPEED  | Terminal speed                                 |
| -13    | CNTREG  | Control register                               |
| -12    | DFLAG   | Device flag bits                               |
| -11    | ECHOTAB | Pointer to echo table                          |
| -10    | BRKTAB  | Pointer to break table                         |
| -7     | LAST    | Last typed character                           |
| -6     | TMSUB   | Time out subroutine                            |
| -5     | TMR     | Time out counter                               |
| -4     | TTMR    | Start value of TMR                             |
| -3     | HDEV    | Hardware device number                         |
| -2     | STDRIV  | Driver start address                           |
| -1     | DRIVER  | Driver interrupt restart address               |
| 0      | RESLINK | Reservation link                               |
| 1      | RTRES   | Reserving RT-program                           |
| 2      | BWLINK  | Beginning of waiting queue                     |
| 3      | TYPRING | Device type bits and ring                      |
| 4      | ISTATE  | 0=active, 1 = I/O-wait, 2 = buffer wait,       |
| 5      | MLINK   | Monitor queue link                             |
| 6      | MFUNC   | Monitor level function address                 |
| 7      | IOTRANS | Input IOTRANS routine                          |

**To be continued**  

*Norsk Data ND–60.230.5 EN*  
*Scanned by Jonny Oddene for Sintran Data © 2021*

---

## Page 165

# SINTRAN III RELEASE INFORMATION, K-VERSION

## TERMINAL INPUT/OUTPUT

| Code | Description                                      |
|------|--------------------------------------------------|
| 10   | STDEV Start device routine                       |
| 11   | SETDV IOSET routine                              |
| 12   | DFOPP Pointer to output channel data field       |
| 13   | DERROR Error code                                |
| 14   | BUFST Start of ring buffer                       |
| 15   | MAX Buffer capacity                              |
| 16   | BHOLD Number of characters in buffer             |
| 17   | HENTE Fetch pointer                              |
| 20   | CFREE Free positions                             |
| 21   | FYLLE Store pointer                              |
| 22   | BSTATE Background program state                  |
| 23   | TSTATE Time slice state                          |
| 24   | DBPROG Background RT-program                     |
| 25   | DBADR Saved P-reg on escape and file system monitor calls |
| 26   | RIFIL For mode input file number                 |
| 27   | BCHISTS For mode input status                    |
| 30   | DERO Error information                           |
| 30   | BREGBLOCK Register save at escape                |
| 32   | DER2 Error information                           |
| 40   | DBPREG P-register on page fault on IOBT level    |
| 41   | DBACTPRI ACTPRI on page fault on IOBT level      |
| 42   | FLAGB Background flags                           |
| 43   | EUSADD Address for user-escape handling          |
| 44   | LUSADD Address for local-function handling       |

## OUTPUT DATA FIELD LAYOUT IN SINTRAN MEMORY, IMAGE AND SAVE AREA

| Code | Description                                   |
|------|-----------------------------------------------|
| -13  | PDISPLAY Pointer to next terminal in display table |
| -12  | UXONCR XON character, output control          |
| -11  | UXOFCR XOFF character, output control         |
| -10  | SCREEN Counter for stop on full page          |
| -7   | EMPTFLAG Buffer empty flag                    |
| -6   | TMSUB Time out subroutine                     |
| -5   | TMR Time out counter                          |
| -4   | TMR Start value of TMR                        |
| -3   | HDEV Hardware device number                   |
| -2   | STDRIV Driver start address                   |
| -1   | DRIVER Driver interrupt restart address       |

To be continued

---

## Page 166

# SINTRAN III RELEASE INFORMATION, K-VERSION
## TERMINAL INPUT/OUTPUT

| Code  | Description                                     |
|-------|-------------------------------------------------|
| 0     | RESLINK Reservation link                        |
| 1     | RTRES Reserving RT-program                      |
| 2     | BWLINK Beginning of waiting queue               |
| 3     | TYPRING Device type bits and ring               |
| 4     | ISTATE 0 = active, 1 = I/O-wait, 2 = buffer wait, -1 & -2 = nowait |
| 5     | MLINK Monitor queue link                        |
| 6     | MFUNC Monitor level function address            |
| 7     | IOTRANS Input iotrans routine                   |
| 10    | STDEV Start device                              |
| 11    | SETDV IOSET routine                             |
| 12    | DFOPP Pointer to output channel data field      |
| 13    | DERROR Error code                               |
| 14    | BUFS T Start of ring buffer                     |
| 15    | MAX Buffer capacity                             |
| 16    | BHOLD Number of characters in buffer            |
| 17    | HENTE Fetch pointer                             |
| 20    | CFREE Free positions                            |
| 21    | FYLLE Store pointer                             |
| 22    | MINBHOLD Lower limit for break                  |
| 23    | ROF IL For "mode" (output file number)          |
| 24    | BCHOST For "mode" (output status)               |
| 25    | ONSMSG Address for ND-500 message               |
| 26    | CBUADR Current user buffer address (outstring)  |
| 27    | NOCHAR Number of bytes in outstring monitor call|
| 30    | CNOCHAR Number of words left to transfer in outstring |
| 31    | XNOCHAR Working location for outstring          |
| 32    | ZOPRG P, X, T-registers in outstring            |
| 35    | ZOARG A, D and L-registers in outstring.        |
| 40    | ZOSRG S, B registers + old page in outstring    |
| 43    | SBHOLD Saved BHOLD in outstring                 |

Norsk Data ND-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 167

# SINTRAN III RELEASE INFORMATION, K-VERSION

## TERMINAL INPUT/OUTPUT

### Format of INFO:

| 17 | 16 | 15 | 14 | 13 | 12 | 11 | 10 | 09 | 08 | 07 | 06 | 05 | 04 | 03 | 02 | 01 | 00 |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
|    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |

- **5FIFO**: FIFO terminal interface
- **5ENABLE**: Console enabled for display functions
- **5MASTER**: Terminal defined as a master terminal
- **5PRINT**: Terminal used as a printer
- **5PCONN**: Epson printer connected to terminal
- **5BBIT**: Interface in 8-bit mode
- **5UMOD**: Unmodified input, no parity
- **5CRDLY**: Carriage return delay
- **5ATERM**: Terminal used as alternative own unit
- **5ERRMRG**: Error-buffer CCERBUF should be printed
- **5VSPEED**: Variable speed is allowed
- **5SPNFOUND**: Speed not found when variable speed allowed
- **5TELEFIX**: Telefix terminal
- **5FRER**: Framing error on input
- **5PAER**: Parity error on input
- **5BFUL**: Characters lost on input due to full input buffer

---

## Page 168

# 13. SECURITY PRIMITIVES

The security primitives introduced in the J-version of SINTRAN III are extended slightly in the K-version. The variable named EXSECURITY now contains the following security flags:

| Bit  | Description |
|------|-------------|
| #0   | No listing of command lines in the @TERMINAL-STATUS command except for own user. If the command is performed by user SYSTEM, the command lines for all background programs logged in will be listed. The command lines will also be listed for the background programs running under the same user as the one executing the @TERMINAL-STATUS command. |
| #1   | The background segment, both program and data bank, will be set to zero when logging out. This feature will delay the logout sequence considerably (seconds). If the background program was terminated abnormally, this zeroing will take place when you log in the first time after the abnormal termination. |
| #2   | The scratch file pages written to in the last session, will be set to zero when logging out. This will also slow down the logout sequence. |
| #3   | Zeroing of pages released from a file, normally in the @DELETE-FILE command. |
| #4   | Not allowed to log in if the user has no password. Only one login without a password is allowed after @CREATE-USER. |
| #5   | The commands @HELP and @LIST-REENTRANT will only list commands and reentrant subsystems/ND-500 standard domains available to the user giving the command. An unprivileged user will thus not "see" commands available only to users SYSTEM or RT. |

The default value of the variable EXSECURITY is 7 (bits 0, 1 and 2 are set) but this can be changed by the SINTRAN-Service-Program command *CHANGE-VARIABLE*.

---

## Page 169

# SINTRAN III RELEASE INFORMATION, K-VERSION

## 14. MEMTOF

This chapter applies to the VSX-version of SINTRAN III version K only.

MEMTOF (MEMory TO Floppy dump) for the VSX-version is now a part of SINTRAN III (installed as part of SINTRAN), and can be run by this simple procedure:

- stop the system (if it is not stopped already)
- dump the register block (use the OPCOM command `0\17RD`)
- dump the internal registers (use the OPCOM command `IRD`)
- press the MCL (master clear) button on the panel
- type `15!` (just 15 and an exclamation mark - without a return)

MEMTOF will then start, and ask you to insert formatted diskettes (one after another) in floppy unit 0 of floppy controller 1.

When the dump is finished, remove the diskettes, label them, and enclose the printout of the register contents when you send it to ND service.

Also remember to copy the file(s) SYMBOL-2-LIST:LIST (and, if your system is an ND-500, N500-SYMBOLS:SYMB) found on user SYSTEM to a diskette and enclose this as well. These files contain information about your configuration and where (within SINTRAN III) different options specific to your configuration are found.

---

## Page 170

# 15. RT-LOADER

The maximum size of the segment files is changed to 65536,10 pages on SINTRAN III/VSX. This is the limit both to the size of one segment file and the total size of all segment files.

## 15.1 REMOVED COMMANDS

The command IMAGE-LOAD is removed from the VSX-version, but is still present in the VSE-version.

## 15.2 CHANGES IN DATA STRUCTURE

The link table element and the RTFIL element in the VSX-version are both changed to use two words for containing segment numbers:

### Link Table Element SINTRAN III/VSX version K:

| Layout        | Displacements |                     |
|---------------|---------------|---------------------|
| 0 Link        | LLINK         |                     |
| 1 Packed      | LPNA1 (LFNAM) |                     |
| 2 symbol name | LPNA2         |                     |
| 3             | LPNA3         | Values of bits 0-2 in Flag  |
|               |               | (no change from J-version): |
|               |               | 0 Defined symbol (entry point) |
|               |               | 1 Referenced symbol        |
|               |               | 2 Defined common label     |
| 4 segment one | LSEG1 (LSEGD) | 3 Declared common label    |
| 5 segment two | LSEG2         | 4 Global defined common label |
| 6 Priority/Flag| LDESC        | 5 Segment name             |
|               |               | 6 Declared RT-program      |
| 7 Symbol value| LSUB1 (LSUBD) | 7 Defined RT-program       |
| 10 Stadr/Sublist | LSUB2      |                     |

### RTFIL Element SINTRAN III/VSX version K:

| Layout        | Displacements |                 |
|---------------|---------------|-----------------|
| 0 Packed      | RFNA1 (RFNAM) |                 |
| 1 symbol name | RFNA2         |                 |
| 2             | RFNA3         |                 |
| 3 segment one | RFSG1 (RFSGD) |                 |
| 4 segment two | RFSG2         |                 |
| 5 Priority/Flag| RFDSC        |                 |
| 6 Symbol value| RFSB1 (RFSBD) |                 |
| 7 Stadr/Sublist | RFSB2       |                 |

The link table element and RTFIL element in the VSE-version are unchanged from previous versions.

---

## Page 171

# SINTRAN III RELEASE INFORMATION, K-VERSION

## ND-500 MONITOR (versions H and I)

### 16. ND-500 MONITOR (VERSIONS H AND I)

Note that the ND-500 System Monitor is now part of SINTRAN III/VSX-500 and is installed together with SINTRAN itself. Also note that you must change both the ND-500 Background Monitor (the subsystem part) and the ND-500 Swapper to run under the K-version of SINTRAN III.

| Generation          | Requirement                                          |
|---------------------|------------------------------------------------------|
| 312 or earlier      | Run the H-versions of the ND-500 Monitor and Swapper |
| 406                 | Run the I-versions (or later)                        |
| 500                 | Use the J-versions of the ND-500 Monitor and Swapper |

Refer to pages 184-211 for further descriptions.

### 16.1 CHANGED INSTALLATION PROCEDURE

The installation procedure for the ND-500 Background Monitor and Swapper is unchanged.

The ND-500 System Monitor will now be delivered on the SINTRAN III diskettes, and will be installed as part of SINTRAN III. This means that explicit installation of the ND-500 System Monitor done at cold start must be removed. The commands to be **REMOVED** will usually be found in the file `HENT-MODE:MODE` and should look like this:

    @RT-LOADER
    *READ-BINARY (BPUN-FILES)ND-500-SEG30-F:BPUN,30
    *YES
    *READ-BINARY (BPUN-FILES)ND-500-SEG31-F:BPUN,31
    *YES
    *EXIT

### 16.2 CONFIGURATION LIMITATIONS

The ND-500 systems can now run up to 254 processes. By default, the number of processes are set to 50, but can be changed by the S3-CONFIG program.
Note that a cold start is required to activate the changes.

Furthermore, the number of physical segments on the ND-500 can now range up to 5000a. Default value is ten times the number of processes plus 250. This value can be changed by the SET-SYSTEM-PARAMETERS command in ND-500 Monitor.
Note that the ND-500 must be restarted to activate the changes.

---

## Page 172

# 16.3 MODIFIED COMMANDS TO THE ND-500 BACKGROUND MONITOR

## 16.3.1 CACHE-MODE

The following options are available on ND-5000 systems for the program and data cache modes:

- Memory-only (default, applies to both program and data cache)
- Normal-WICO (Normal, Write In Cache Only), (program and data)
- Write-through-data (applies to data cache only)
- Smart-if-go-program (applies to program cache only)

There is no change for ND-500 systems (Normal/Cache only/Memory-only).

## 16.3.2 DEFINE-MEMORY-CONFIGURATION

The memory configuration is now defined automatically each time the system is started. This means that the DEFINE-MEMORY-CONFIGURATION command is no longer necessary unless you want to define the memory configuration differently from the default configuration. An explicit definition of memory configuration will, however, not survive a restart of the system, so you will have to do this after each restart. In practice, the change means that the DEFINE-MEMORY-CONFIGURATION command should be deleted from the HENT-MODE:MODE file run after each cold start. On systems where the memory configuration needs to be defined differently from the default, a DEFINE-MEMORY-CONFIGURATION command must be included in the LOAD-MODE:BATC file to be run after each warm start.

## 16.3.3 DEFINE-STANDARD-DOMAIN

All types of domains can now be defined as standard domains.

Segment limits defined by the SET-SEGMENT-LIMITS command in the LINKAGE-LOADER when loading the domain are ignored for standard domains.

---

## Page 173

# SINTRAN III RELEASE INFORMATION, K-VERSION
### ND-500 MONITOR - COMMANDS

## 16.3.4 LIST-TABLE

LIST-TABLE is modified to cover the following new tables and functions:

### HW-SEGM-TAB
Lists the physical segment table. One entry for each physical segment; the segment number is the index in the table. The use of the commands follows conventions given for the other tables. A single line layout is used for the table when the TO-THE-END command is used.

### PROC-TAB
Lists process table entries. There is one entry for each process and the process number is the index in table. The use of the commands follows conventions given for the other tables.

### LAST-N500-MSG
If the swapper version used has "message-log" implemented, the last couple (256 in current version) of messages sent from ND-100 to ND-500 can be recalled. Some special conventions are valid only for this table type. The message sent last is listed first and has the highest index number (377B). Use of commands PREVIOUS/NEXT will go backwards/forward respectively in "time". Index zero gives the oldest message. PREVIOUS is the default command on CR when entering the mode.

Note 1: If using the TO-THE-END command, a convenient shorthand layout is used for the table in order to condense the most relevant information. Special care is taken with parameters for the PAGE-FAULT message. Here the page-fault address is given in 32-bit format, followed by the last three parameters. For all other messages, the six first parameters are listed.

Note 2: While listing the message-log on a "living system", the current last message is not updated if more messages arrive while in the mode. New messages will overwrite the oldest messages (index zero and up).

### MEMORY-MAP
The new version has different layout, and contains some more information. A single line layout is used for the table when the TO-THE-END command is used.

### SW-SEGM-TAB
The new version includes all information contained in this table, also some indirect information retrieved through pointers from the table. Subcommands are made to obtain more information about swapper files. Bit maps for REFER, SYSFIX and FIX are listed in terms of process numbers concerned. Bit map for MODIFIED lists page numbers modified.

The following additional commands are available when listing the swap-segments-table:

| Command | Description |
| ------- | ----------- |
| REFER | Lists bit maps (process numbers) as represented in the table but now in full version. The bit maps can be larger than what is possible to display in the standard layout of the table. These commands will list the whole bit maps over several lines if necessary. |
| SYSFIX | |
| FIX | |

---

## Page 174

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## ND-500 MONITOR - COMMANDS

### MODIFIED

As above, but represents the bit map (logical process number) for modified pages in swap-file.

### PAGELINK  
**IX-PAGELINK**  
Starts memory-map listing in follow-link mode, by using page/indexpage-link from the current swap-entry as first page.

### ORIG-FILE / SWAP-FILE

These are commands to list file buffers for original-files and swapper-files using a given segment, (the one currently displayed). The pointer used is IXBP for the file concerned.

Both commands enter a subcommand-processor, and give the prompt "FCOM:".

The initial screen shows the first file buffer. Each buffer contains up to 64 page numbers. More buffers exist as appropriate, and describe the rest of the file's pages.

### FCOM SUBCOMMANDS

**NEXT**  
The command NEXT (or simply press CR), gives the next buffer in the same file. The command follows the pointer NX-IXB. When no more buffers exist, the error message "END OF INDEX-BUFFER LIST" appears.

**FIRST**  
Responds by giving the first buffer.

**THIS**  
Lists the current buffer.

**EXIT**  
Returns to LIST-TABLE, (LTB:).

**SUC**  
Lists buffers from the global list for the swapper.  
**PREDEC**  
This is a circular linked list which contains all the file buffers in the system.  
SUC and PREDEC pointers point in different directions around the list. The commands use the pointer with the same name as the command, and display the next buffer in the list.

**LIST-GLOBAL-LIST (full or partial)**  
Lists the whole global list by following the SUC-pointers, finishing with the current buffer. Giving FULL as parameter lists each buffer fully. PARTIAL, which is the default, gives only the current buffer address, SUC-pointer, PREDEC-pointer, and the flag NEW INDICES, on one line for each buffer. Note: listing can be stopped by pressing any key.

**HELP**  
Lists available commands.

**@**  
SINTRAN commands can be used here with the same limitations as for the rest of the monitor.  
Norsk Data ND-60.230.5 EN  
Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 175

# SINTRAN III RELEASE INFORMATION, K-VERSION  
ND-500 MONITOR - COMMANDS

## TRACK-SWAPPER-MESSAGES <process>
Lists messages to the swapper from a given process as they arrive, default is all processes. Listing can be aborted by pressing any key.

Note: This command is intended for logging single events. It may not be able to track messages if they arrive too frequently over a period of time, as incoming messages can catch up with the output in the message ring buffer.

## HELP
Lists available commands.

`@`  
SINTRAN commands can be used here with the same limitations as for the rest of the monitor.

## OTHER MODIFICATIONS
For the tables SW-SEGM-TAB, HW-SEGM-TAB, MEMORY-MAP and PROC-TAB, the user now can select the entry index directly as an optional parameter.

## DEFAULTS
When going between the above-mentioned table types, without stating the entry index, several defaults will be assumed. These will depend on each case to suit the most relevant situation.  
The defaults are:

Going from HW-SEGM-TAB to:
- SW-SEGM-TAB, uses the same segment entry
- MEMORY-MAP, uses PAGENO

Going from SW-SEGM-TAB to:
- HW-SEGM-TAB, uses the same segment entry
- MEMORY-MAP, uses PAGELINK (as "PAGELINK" command)
- PROC-TAB, uses FIRSTPROC

Going from PROC-TAB to:
- HW-SEGM-TAB, uses PROCESS-SEGNO
- SW-SEGM-TAB, uses PROCESS-SEGNO

Going from MEMORY-MAP to:
- HW-SEGM-TAB, uses OWNER
- SW-SEGM-TAB, uses OWNER

Otherwise the default is zero.

Note: When going to MEMORY-MAP with a non-zero default entry index, follow-link mode is selected. Otherwise follow-table mode is used.

---

## Page 176

# 16.3.5 LOOK-AT

The LOOK-AT commands:

```
LOOK-AT-DATA
LOOK-AT-STACK
LOOK-AT-RELATIVE
LOOK-AT-REGISTER
```

are modified to work on dumpfile in inspect-dump mode.

```
LIST-TABLE <address>
MATCH-TABLE <address>
```

Intended to be used when looking at the swapper's data segment. This is a way of going directly into LIST-TABLE from LOOK-AT. Useful when a table pointer is found at a location, etc. Uses current location value as default pointer if no address given. Otherwise the commands have the same effect as commands with same name in LIST-TABLE. The commands will leave the user in LIST-TABLE mode. EXIT returns to LOOK-AT.

```
SET-SEARCH-OBJECT
SET-SEARCH-ADDRESSMODE
SEARCH
```

These commands are used to find specific data sequences, and are implemented for the following look-at modes:

```
LOOK-AT-DATA
LOOK-AT-PROGRAM
LOOK-AT-FILE
LOOK-AT-RESIDENT
```

```
SET-SEARCH-OBJECT <BYTE/HALF/WORD> <MASK> <EQ/NE/GT/LT/IN> <VALUE>
```

Defines what to search for.

| Key                | Description                                                                                                                                               |
|--------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------|
| `<BYTE/HALF/WORD>` | Defines the size(s) of the data element(s); if more than one, each is separated by a semicolon. Default value is the current look-at size. Size qualifier can be abbreviated. |
| `<MASK>`           | Defines mask(s) through which data is ANDed with before comparison; also separated with semicolons if more than one. Default are all 1s (ones), i.e., no masking.              |
| `<EQ/NE/GT/LT/IN>` | Sets the search mode for each element. These can be:<br>- EQ, equal to value<br>- NE, not equal to value<br>- GT, greater than value<br>- LT, less than value<br>- IN, in a defined set (range) <br> Separated by semicolons if more than one. Default value is EQ. |

---

## Page 177

# SINTRAN III RELEASE INFORMATION, K-VERSION
## ND-500 MONITOR - COMMANDS

### `<VALUE>`

Defines the final value sequence to search for, separated by semicolons if more than one. Sets or ranges are defined by two numbers in ascending order only separated by a colon. Complimentary sets are defined by changing the two values (largest number first).

- `23:46` is the set from 23 to 46 inclusively.
- `46:23` is the complimentary set of 23:46. In other words values less than 23 or greater than 46.

### ASCII SEARCH

A special case is available when ASCII-string search is wanted. A string can be entered directly as value a enclosed in single quotes (`'`). All above parameters are then ignored. Default search address mode will be in BYTE-mode (see the command SET-SEARCH-ADDRESSMODE).

### SEARCH `<from address> <to address>`

Starts searching for the chosen value or sequence. Parameters here are optional, and give a possibility to limit the search area.

- **Default** start address is the current address, not inclusive.
- **Default** stop address is maximum address within segment.

Search can be broken at any place by pressing any key.

**Note:** Search will terminate on illegal addresses, at "holes" in the file and at End-Of-File. This will cause the look-at mode to terminate as well, so the user is advised to set the top limit while searching, as the search data is lost. Using the break facility is another possibility.

### SET-SEARCH-ADDRESSMODE `<BYTE/HALF/WORD>`

Defines address type for the start of the sequence. This is a way of eliminating "imaginary" sequences that can occur "out of phase".

- **BYTE:** Sequence can start at any byte-address (no limitations).
- **HALF:** Sequence must start on half word limit (address divisible by 2).
- **WORD:** Sequence must start on a word limit (address divisible by 4).

**Default** address mode is the one currently used in LOOK-AT.

For special cases, a number can be entered to give other modes. This is useful to speed up the search when the sequence is known to be found at specific address-limits.

Norsk Data ND-60.230.5 EN
Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 178

# 16.3.6 LOAD-CONTROL-STORE

The default value of the parameter <number of words> is changed to 20000₈ to handle the larger control store on ND-5000 systems.

# 16.3.7 MASTER-CLEAR

On ND-500 systems, this command is unchanged. On ND-5000 systems, however, the command does the following:
- reset the ACCP (the Access Control Processor) by hardware
- make ACCP run the short ND-5000 selftest (takes about 20 seconds)
- make ACCP do a reset of ND-5000 CPU

# 16.3.8 VERSION

On ND-500 systems, this command is unchanged (giving version information about ND-500 System Monitor, ND-500 Background Monitor and ND-500 Swapper). On ND-5000 systems the command also gives information about the ECO level on each module.

An example of the information listed on ND-5000 systems are given below:

## N500: VERSION

|            |            |
|------------|------------|
| SUBSYSTEM PART: | 87. 9. 1 REV.-I01 |
| SYSTEM PART…: | 87. 8.25 |
| SWAPPER.......: | 87.07.03 |
| MICRO PROGRAM.: | 13213 |

| Module:        | MB.2 | ALU.1 | AAP.1 | IDA.2 | MMS.1 | CS.2 | CACHE.1 | MIC.2 | ACCP.1 |
|----------------|------|-------|-------|-------|-------|-----|---------|-------|-------|
| ECO no:        | 4a   | 1c    | 0b    | 2a    | 1c    | 3b  | 1b      | 0b    | 2c    |

# 16.3.9 NEW INTEGER INPUT FORMAT

In both LOOK-AT and LIST-TABLE it is now possible to write addresses with shorthand notation for the page number.

The string 1'23'456 is interpreted as address 456 on page 23 in segment 1, in other words the address 1000114456, (all octal). This can be written as an address or as a location expression. This is useful when accessing page tables etc.

# 16.3.10 USE OF NEW SEARCH COMMANDS

## SEARCH COMMANDS IN LOOK-AT - EXAMPLE

The user wants to search in a data segment for a sequence of three elements:

First a word equal to 146000₈. Then a half-word (16 bits) greater than 4000 decimal followed by a byte from 'A' to 'Z'.

---

## Page 179

# SINTRAN III RELEASE INFORMATION, K-VERSION

## ND-500 MONITOR - COMMANDS

### N500: LOOK-AT-DATA 1'

| D1   | OB  | OB     | 
|------|-----|--------|
| BYTE/HALF/WORD: | W,H,B | 
| Mask: | EQ/NE/GT/LT/IN: | EQ,GT,IN | 
| Value: | 146003:400D:101:132 |

Suppose the sequence is to be found between the start of segment and the address 1'200000...

| D1   | OB  | OB     |
|------|-----|--------|
|      |     | SEARCH,,1'200000 |

The monitor will now search through the segment and terminate if the sequence is found, or the end of the search area is reached. If it is found, the address of the start of the sequence is displayed and control returns to the user:

| D1    |       |
|-------|-------|
| 123411: | 146003B  (etc.) |

### EXAMPLE 3

Search for an ASCII string 'NORSK-DATA' on a file DAT:DATA ...

### N500: LOOK-AT-FILE 0 DAT:DATA

| <file>O | OB  | OB                        |
|---------|-----|---------------------------|
|         |     | SET-SEARCH-OBJECT,...,'NORSK-DATA' |
| <file>O | OB  | OB SEARCH                 |

etc.

## 16.4 NEW COMMANDS TO THE ND-500 BACKGROUND MONITOR

### 16.4.1 ARM-TRACER

Used for control of the ND-5000 tracer module to arm the trace module.

### 16.4.2 CLEAR-TRACE-ADDRESS

Used for control of the ND-5000 tracer module to clear a specific trace memory address.

### 16.4.3 CLEAR-TRACE-MEMORY

Used for control of the ND-5000 tracer module to clear the trace memory.

Norsk Data ND-60.230.5 EN

---

## Page 180

# SINTRAN III RELEASE INFORMATION, K-VERSION
## ND-500 MONITOR - COMMANDS

### 16.4.4 DEBUG-SWAPPER

**Parameter:** `<ON/OFF>`

This command is installed for debugging purposes. It is intended for internal use and is restricted to user SYSTEM only.

---

### 16.4.5 DISARM-TRACER

Used for control of the ND-5000 tracer module to disarm the trace module.

---

### 16.4.6 DUMP-PHYSICAL-SEGMENT

**Parameters:**  
`<FILE NAME>`  
`<PHYSICAL SEGMENT NUMBER>`

This command is installed for debugging purposes. It is restricted to user SYSTEM only.

---

### 16.4.7 DUMP-SWAPPER

**Parameter:** `<FILE NAME>`

This command is installed for debugging purposes. It is intended for internal use and is restricted to user SYSTEM only. If this command is to be used after a fatal error from swapper, an error flag must be reset before the command is given. The error flag is reset by giving the two commands: DEBUG-SWAPPER ON followed by DEBUG-SWAPPER OFF.

---

### 16.4.8 DUMP-TRACE-MEMORY

Used for control of the ND-5000 tracer module to dump the trace memory.

---

### 16.4.9 EXAMINE-TRACE

Used for control of the ND-5000 tracer module to examine a dump of a trace generated by either the DUMP-TRACE-MEMORY or the READ-TRACE-FILE commands.

---

### 16.4.10 INIT-TRACER

Used for control of the ND-5000 tracer module to define tracer operation mode.

**Parameters:**  
`<Cycle>`  
`<Mode>`  
`<Trigger>`  
`<CSA>`  
`<Clear trace memory(yes/no)>`

---

## Page 181

# SINTRAN III RELEASE INFORMATION, K-VERSION
## ND-500 MONITOR - COMMANDS

### 16.4.11 INSPECT-DUMP

Parameter: `<FILE-NAME>`

This command sets the monitor in inspect-mode. The file is supposed to be a mapped copy of the swapper's data segment. Default file type is "::DUMP". In this mode the LOOK-AT and LIST-TABLE commands will work on the dump file rather than on segments directly. The monitor prompt will be preceded by the text, ":dump", while inspect-dump mode is active. It is illegal to enter inspect-dump mode when segments are placed. The following commands are relevant in inspect-dump mode:

- LOOK-AT-DATA
- LOOK-AT-STACK
- LOOK-AT-RELATIVE
- LOOK-AT-REGISTER

Note: 
- LOOK-AT-PROGRAM, LOOK-AT-RESIDENT and LOOK-AT-PHYSICAL-SEGMENT are illegal in inspect-dump mode.
- LOOK-AT-CONTROL-STORE, LOOK-AT-HARDWARE and LOOK-AT-FILE will work normally.

| LIST-TABLE SW-SEGM-TAB | HW-SEGM-TAB |
|------------------------|-------------|
| PROC-TAB               | MEMORY-MAP  |
| LAST-N500-MSG          |             |

Note: The table type N500-MSG still gives the current table as in normal mode, and is not changed by the dump.

### 16.4.12 LIST-STATUS

Used to list the status of a domain stored on a domain file.

Parameter: `<Domain name>`

Status is only listed if the parameter `<domain name>` matches one domain only.

### 16.4.13 LOOK-AT-SRF

Used to read the scratch register file.

Parameter: `<SRF-address>`

### 16.4.14 READ-TRACE-FILE

Used for control of the ND-5000 tracer module to read a file containing a dump of a trace to be examined by the EXAMINE-TRACE command.

Parameter: `<File name>`

---

## Page 182

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## ND-500 MONITOR - COMMANDS

### 16.4.15 RESET-CPU

On ND-500 systems, this command is identical to the MASTER-CLEAR command. On ND-5000 systems, it will make the ACCP (the Access Control Processor) do a reset of the ND-5000 CPU.

The RESET-CPU command has no parameters and is restricted to user SYSTEM only.

### 16.4.16 RESET-INSPECT-DUMP

Resets inspect-dump mode and sets monitor back in its normal state. An automatic reset will occur if segments are placed while inspect-dump mode is on.

### 16.4.17 RUN-SELFTEST

On ND-5000 systems, this command will make the ACCP (the Access Control Processor) run a long ND-5000 selftest (this takes about 3-4 minutes).

The RUN-SELFTEST command has no parameters and is restricted to user SYSTEM only.

### 16.4.18 SET-CPU-STATUS

Used to set the status of the different CPUs in a multi-CPU system.

Parameters:  
- `<CPU number>`  
- `<Image>`  
- `<Save>`  
- `<Status>`

| Possible values of `<Status>` |  
|-------------------------------|
| Available                     |
| Unavailable                   |
| Auto-allocation               |
| Fixed-allocation              |

Note that a warm start (if image is selected) or cold start (if save area is selected) is required to activate the changes.

### 16.4.19 WRITE-TRACE-FILE

Used for control of the ND-5000 tracer module to write a file containing a dump of a trace (dumped by the DUMP-TRACE-MEMORY command). The file also contains some system information.

Parameter: `<File name>`

Default file type is `TRAC`.

---

## Page 183

# SINTRAN III RELEASE INFORMATION, K-VERSION

## ND-500 MONITOR - MONITOR CALLS

### 16.5 MONITOR CALLS REMOVED (ND-500)

#### 16.5.1 ABSTR MON 131

MON ABSTR is no longer available from ND-500.

### 16.6 MODIFIED MONITOR CALLS (ND-500)

#### 16.6.1 N500OM MON 60

| Function | Description |
|----------|-------------|
| 72a | GSGTE (Get physical Segment Table Entry) is changed. The location OWNINDEX will now contain object block number. Function 72a returns an array of 668 words in the format listed on page 180. |
| 71a | SSGTE (Search for physical Segment Table Entry) also returns this 668-word array. |
| 67a | SPRTE (Search for Process Entry) and 70a GPRTE (Get Process Entry) both returns an array of 1238 words in the format shown on page 178. |
| 60a | LIMEM (LIst MEMory configuration) now returns the ND-100 page address of the ND-500 register block when called from background (no longer the address of "page-used/written-in-page" table for the ND-500). For call from foreground, there is no change. |
| 35a | 5MCLEAR (500 Master CLEar). On ND-500 systems, this function is unchanged. On ND-5000 systems, however, it does the following: <br> - reset the ACCP (the ACCess Control Processor) by hardware <br> - make ACCP run the short ND-5000 selftest (takes about 20 seconds) <br> - make ACCP do a reset of ND-5000 CPU |
| 36a | RSETACCP (ReSETACCP). This is a new function only supported on ND-5000 systems. It resets the ACCP (the ACCess Control Processor) by hardware. |
| 155a | STSELFTEST (STart 5000 SELFTEST). This is a new function only supported on ND-5000 systems. It makes the ACCP (the ACCess Control Processor) run the long ND-5000 selftest (takes about 2-3 minutes). |
| 156a | WRSYSINFO (WRite SYStem INFOrmation) is described on the next page. |
| 157a | MONACCP (execute ACCP command). This is a new function only supported on ND-5000 systems. It is used to execute a command to the ACCP (the ACCess Control Processor). |
| 160a | NSGLOAD (new place domain). This new function is used to place a domain stored on a domain file. |

Norsk Data ND-60.230.5 EN

---

## Page 184

# Function 156a WRSYSINFO

This new function returns information in the following format:

| Type                             | Description                                     |
| -------------------------------- | ----------------------------------------------- |
| INTEGER4                         | CPU information:                                |
| bits 0-7                         | CPU type                                        |
|                                 | 1 = ND-560 series                               |
|                                 | 2 = ND-570 series                               |
|                                 | 3 = ND-5000 series                              |
| bit 15-1                         | this CPU is not part of the time Slice in a multi-CPU context. |
| INTEGER4                         | ND-500 CPU and Microprogram version:            |
| bits 0-15                        | Microprogram version                            |
| bits 16-19                       | CPU model:                                      |
|                                 | 2 = ND-5200                                     |
|                                 | 4 = ND-5400                                     |
|                                 | 5 = ND-5500                                     |
|                                 | 6 = ND-5600                                     |
|                                 | 7 = ND-5700                                     |
|                                 | 8 = ND-5800                                     |
| bits 20-21                       | CPU type                                        |
|                                 | 1 = ND-5200                                     |
|                                 | 2 = ND-5400/ND-5500/ND-5600/ND-5700             |
|                                 | 3 = ND-5800                                     |
| STRING(0:19)                     | ND-500 System monitor version (*)               |
| STRING(0:19)                     | ND-500 Swapper version (*)                      |
| INTEGER ARRAY(0:8)               | ECO level for each of 9 PCBs                    |

(*) An apostrophe is used to terminate the information returned.

# Function 161b SNDFSYSTDOM

This new function is used to define a standard domain stored on a domain file.

# Function 162a INITRACE

This is a new function only supported on ND-5000 systems. It is used for control of the ND-5000 tracer module to define tracer operation mode.

# Function 163a CLRTRACE

This is a new function only supported on ND-5000 systems. It is used for control of the ND-5000 tracer module to clear the trace memory.

# Function 164a ARMTRACE

This is a new function only supported on ND-5000 systems. It is used for control of the ND-5000 tracer module to arm the trace module.

# Function 165a DISARMTRACE

This is a new function only supported on ND-5000 systems. It is used for control of the ND-5000 tracer module to disarm the trace module.

# Function 166a DUMPTRACE

This is a new function only supported on ND-5000 systems. It is used for control of the ND-5000 tracer module to dump the trace memory.

# Function 167a CLRADRTRACE

This is a new function only supported on ND-5000 systems. It is used for control of the ND-5000 tracer module to clear a specific trace memory address.

Norsk Data ND-60.230.5 EN
Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 185

# ND-500 Monitor - Monitor Calls

## Function 170s RCPUTYPE (Read CPU TYPE)

This is a new function only supported on ND-5000 systems. It is used to get the type of CPU used.

## Function 171s SCACHEMODE (Set CACHE MODE)

This is a new function only supported on ND-5000 systems. It is used to set the mode of cache operation both for the program and data cache.

## Function 172s RSGF (Read Scratch Register File)

This is a new function only supported on ND-5000 systems. It is used to read the scratch register file.

## Function 173s SCPUSTATUS (Set CPU STATUS)

This is a new function only supported on ND-5000 systems. It is used to set the status of the different CPUs in a multi-CPU system.

MON 60 is for internal use by ND only.

# 16.6.2 MAGTP

## MON 144

Functions 50, 51 and 52 are now available from ND-500. For direct transfer on STC magnetic tape, functions 50 and 51 will be converted internally to MON ABSTR (MON 131) functions 50 and 51 respectively.

- **Function 50**: Read multiple records, i.e. read a number of records from magnetic tape into a contiguous area of memory.
- **Function 51**: Write multiple records, i.e. write a contiguous area of memory to a number of records on magnetic tape.

### Call format:

```
MAGTP, <number of parameters>, <function>, <buffer>, <logical device no>, <number of records>, <record size>
```

### Input parameters:

- `<function>`
- `<buffer>`
- `<logical device number>`
- `<number of records to read/write in one call>`
- `<record size in bytes>`

### Output parameters:

- `<number of records read/written>`
- `<record size in bytes of record read>`

On error return, the `<number of records read/written>`, will be the number of records read or written before the error occurred.

---

## Page 186

# 16.6.3 COPAG MON 251

On end of file the MON COPAG (MON 261) will give an error return (setting K) and W1 (which contains the error code on error returns) will be set to 3 (end of file).

# 16.6.4 FSMTY MON 327

Three new functions are added: function = 2 : return block size  
= 3 : get file name  
= 4 : get file/device info.

**Parameters:**

- `<function = 2>`  
  `<open file number>`  
  `<block size in bytes>`

- **or:**

  `<function = 3>`  
  `<open file number>`  
  `<buffer to receive file name>`

- **or:**

  `<function = 4>` (same as MON GTYPR (MON 45))  
  `<open file number or device number>`  
  `<returned TYPRING>`  
  `<returned status>`  
  `<returned SINTRAN III open file number or device no.>`

For further details on MON 327, refer to page 26.

**Examples:**

- `FUNCTION : W DATA 2        % function code = 2`  
  `OPFILNO : W BLOCK 1      % open file number`  
  `BLOCKSIZE : W BLOCK 1    % block size`  
  `FSMTY : EQU 370000000327B % FSMTY = MON 327`

  CALLG FSMTY,3,FUNCTION,OPFILNO,BLOCKSIZE % MON FSMTY with 3 parameters  
  IF K GO ERROR            % on error return, W1 = error code

---

- `FUNCTION : W DATA 3        % function code = 3`  
  `OPFILNO : W BLOCK 1      % open file number`  
  `FILENAME : STRING 64     % file name (string descriptor)`  
  `FSMTY : EQU 370000000327B % FSMTY = MON 327`

  CALLG FSMTY,3,FUNCTION,OPFILNO,FILENAME % MON FSMTY with 3 parameters  
  IF K GO ERROR            % on error return, W1 = error code

Norsk Data NO-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 187

# SINTRAN III RELEASE INFORMATION, K-VERSION

## ND-500 MONITOR - MONITOR CALLS

| **Function** | **Description** |
|--------------|-----------------|
| `FUNCTION : W DATA 4` | % function code = 4 |
| `OPFILNO : W BLOCK 1` | % open file number |
| `TYPRING : W BLOCK 1` | % returned TYPRING |
| `STATUS : W BLOCK 1` | % returned Status |
| `S30PFILN : W BLOCK 1` | % returned SINTRAN III open |
| `FSMTY : EQU 37000000327B` | % FSMTY = MON 327 |

```
CALLG FSMTY,5,FUNCTION,OPFILNO,TYPRING,STATUS,S30PFILN % MON FSMTY
IF K GO ERROR                                    % on error return, W1 = error code
```

### 16.6.5 IOMTY MON 336

The I/O multifunction (IOMTY) monitor call is used to change some of the attributes of terminal and terminal access device (TAD) input and output. This monitor call needs a varying number of input and output parameters depending upon function, all parameters are therefore placed in an array.

#### Parameters

- `<function>`
- `<number of parameters (32-bit integers)>`
- `<array of parameters>`
- `<status-2>`

The following rules apply:

1. The I1-register will contain status information corresponding to what is usually returned in the A-register on the ND-100. The parameter `<status-2>` contains information which is returned in the X-register on ND-100.
2. All parameters must be on 32-bit integer format, except for the bit maps referred to in functions 4 - 7. The echo and break strategy bit maps should be arrays of 16-bit integers (i.e., `INTEGER*2`).

For further details on MON 336, refer to pages 28-53.

#### Example

| **Function** | **Description** |
|--------------|-----------------|
| `FUNCTION : W BLOCK 1` | % function |
| `SIZE : W BLOCK 1` | % number of parameters |
| `PARAM : W BLOCK 1` | % start of array of parameters |
| `PARAM1 : W BLOCK 1` | % parameter no. 1 in array |
| `PARAM2 : W BLOCK 1` | % parameter no. 2 in array |

```
PARAMN : W BLOCK 1   % parameter no. N in array
RESTATUS : W BLOCK 1 % returned status
IOMTY : EQU 37000000336B % IOMTY = MON 336
CALLG IOMTY,4,FUNCTION,SIZE,PARAM,RESTATUS % MON IOMTY with 4 params.
IF K GO ERROR      % on error return, W1 = error code
```

```
% Note: on error return, RESTATUS contains extra information for
% function codes ≥ 1008
```

---

## Page 188

# 16.7 NEW MONITOR CALLS (ND-500)

## 16.7.1 RSREC MON 340

**Purpose:** Read system record - RT-DESCRIPTION or - ND-100 segment table entry

**Parameters:**  
`<record type>`  
`<RT-description address / segment number>`  
`<buffer>`  
`<format>`

The parameter `<record type>` can take the values:

1: RT-description  
2: ND-100 segment table entry

The parameter `<format>` (not found on the ND-100) can take the values:

0: Return information on 16-bit integer format  
1: Return information on 32-bit integer format

For further details on MON 340, refer to page 54.

**Example:**

|            |         |         |                                                         |
|------------|---------|---------|---------------------------------------------------------|
| **RECTYPE** | : W BLOCK 1 | % RT-description/segment entry                                  |
| **ADDNO**   | : W BLOCK 1 | % address of RT-descr/ND-100 segment                            |
| **ENTRY**   | : H BLOCK 38| % buffer, 38 words                                              |
| **FORMAT**  | : W BLOCK 1 | % format (16/32-bit integer)                                    |
| **RSREC**   | : EQU 37000000340B | % RSREC = MON 340                                           |

CALLG RSREC,4,RECTYPE,ADDNO,ENTRY,FORMAT % MON RSREC with 4 parameters  
IF K G0 ERROR % on error return, W1 = error code

## 16.7.2 CONFIG MON 343

**Purpose:** CONFIG is used to read and/or change configuration parameters for a "standard system" SINTRAN III.

**Parameters:**  
`<function>`  
`<configuration parameter index>`  
`<configuration parameter subindex>`  
`<array>`

For further details on MON 343, refer to pages 60-78.

**Example:**

|              |            |                                                     |
|--------------|------------|-----------------------------------------------------|
| **FUNCTION**  | : W BLOCK 1| % function code                                     |
| **INDEX**     | : W BLOCK 1| % configuration parameter index                     |
| **SUBINDEX**  | : W BLOCK 1| % config. parameter subindex                        |
| **BUFFER**    | : W BLOCK 10| % array                                              |
| **CONFIG**    | : EQU 37000000343B | % CONFIG = MON 343                              |

CALLG CONFIG,4,FUNCTION,INDEX,SUBINDEX,BUFFER % MON CONFIG with 4 params  
IF K G0 ERROR % on error return, W1 = error code

Norsk Data ND-60.230.5 EN

---

## Page 189

# SINTRAN III RELEASE INFORMATION, K-VERSION

## ND-500 MONITOR - MONITOR CALLS

### 16.8 NEW MONITOR CALLS - ONLY AVAILABLE ON ND-500

#### 16.8.1 AttachSegment MON 440

This Attach Segment monitor call (4408) is used to map a logical ND-500 data segment onto shared ND-100/ND-500 physical memory. The specified physical memory area must be defined in the "Not initialize page" table by use of the *CHANGE-TABLE command in the SINTRAN-Service-Program. Note that you should not use the first pages of the multi-port memory (starting at "ND-500 page 0") for this.

Example:

```
@SINTRAN-SERVICE-PROGRAM
*CHANGE-TABLE
TABLE: MEMORY-AREA-INVISIBLE-FOR-THIS-SYSTEM
FUNCTION: INSERT-ELEMENT
IMAGE OR SAVE AREA (DEFAULT IS IMAGE): IMAGE
FIRST PAGE (OCT): 10000
LAST PAGE (OCT): 13777
FUNCTION: EXIT
*EXIT
```

You must then do a warm start to put the change into effect.

The monitor call has 3 functions:

| Function | Description                                                                                     |
|----------|-------------------------------------------------------------------------------------------------|
| 0        | detach (forget) a previously attached segment.                                                  |
| 1        | attach segment. If physical segment does not exist, create and map segment onto physical ND-100 address area. |
| 2        | map existing segment onto physical ND-100 address area.                                         |

#### Function 0

**Function description:**

Detach (forget) a segment previously attached using function 1 (described on the next page).

**Parameters:**

- `<function = 0>`
- `<segment number in the range 0:3110>`

**Example:**

```
FUNCTION : W DATA 0       % function = 0  
SEGNO    : W DATA 5       % segment no. 5

CALLG 370000000440B,2,FUNCTION,SEGNO % Mon 440 with 2 parameters
IF K GO ERROR                        % on error return, W1 = error code
```
Norsk Data ND-60.230.5 EN

---

## Page 190

# Function 1

**Function description:**  
Attach segment. If physical segment does not exist, create and map segment onto physical ND-100 address area.

**Parameters:**  

| Parameter                     | Description                                                                                                      |
|-------------------------------|------------------------------------------------------------------------------------------------------------------|
| \<function = 1>               |                                                                                                                  |
| \<ND-500 logical data segment address> | If address = 0, the first free segment will be used.                                                        |
| \<length of segment in pages> |                                                                                                                  |
| \<physical ND-100 page address>   |                                                                                                                  |
| \<segment name>               | Maximum 35.10 characters in segment name, including optional user name (all characters count). The parameter is a string descriptor. |
| \<read/write area>            | 0 = read only access, 1 = read/write access                                                                       |
| \<returned logical segment number> |                                                                                                                  |

**Example:**

| Parameter | Value                       | Description                              |
|-----------|-----------------------------|------------------------------------------|
| FUNCTION  | W DATA 1                    | % function = 1                           |
| SEGNO     | W DATA 10000000000B         | % address 0 on segment no. 1             |
| LENGTH    | W DATA 4                    | % segment length = 4 pages               |
| NIADDR    | W DATA 2000B                | % mapped at page address 2000B           |
| SEGNAME   | STRINGDATA 'ATTSIEG1'      | % segment name = ATTSIEG1                |
| ACCESS    | W DATA 1                    | % read/write access                      |
| RTSEGNO   | W BLOCK 1                   | % returned segment number                |

```
CALLG 37000000440B,7,FUNCTION,SEGNO,LENGTH,NIADDR,SEGNAME,ACCESS,&
     RTSEGNO       % Mon 440 with 7 parameters
IF K GO ERROR     % on error return, W1 = error code
```

# Function 2

**Function description:**  
Map existing segment onto physical ND-100 address area.

**Parameters:**  

| Parameter                            | Description                                                                       |
|--------------------------------------|-----------------------------------------------------------------------------------|
| \<function = 2>                      |                                                                                   |
| \<first address>                     | ND-500 logical data address where segment starts. Must be the lower address of the existing segment. |
| \<length of segment in pages>        | Must always cover the whole segment.                                              |
| \<physical ND-100 page address>      |                                                                                   |

If the segment is shared, the segment cannot be used by other processes at the time of the call. If the segment is used, the call will give an error return. This restriction only applies to generations 406 and earlier.

**Example:**

| Parameter | Value                       | Description                           |
|-----------|-----------------------------|---------------------------------------|
| FUNCTION  | W DATA 2                    | % function = 2                        |
| STADDR    | W DATA 20000000000B         | % segment no. 2, lower bound = 0      |
| LENGTH    | W DATA 4B                   | % segment length = 4 pages            |
| NIADDR    | W DATA 2000B                | % mapped at page address 2000B        |

```
CALLG 37000000440B,4,FUNCTION,STADDR,LENGTH,NIADDR   % Mon 440
IF K GO ERROR     % on error return, W1 = error code
```

---

## Page 191

# SINTRAN III RELEASE INFORMATION, K-VERSION
## ND-500 MONITOR - MONITOR CALLS

### 16.8.2 SMTRANS MON 515

The SMTRANS monitor call (515\(_8\)) is used for fast disk transfer from the ND-500.

The SMTRANS monitor call has 4 functions:
- Disk transfer
- Check event
- Start process
- Get magic number

Several functions may be issued in the same call, but if check event is given, start process is not performed in the same call. This gives the following legal combinations:

- Any single function
- Disk transfer, check event
- Disk transfer, start process

All parameters are 32-bit words, but only the 16 least significant bits in \<IO code\> and \<request ID\> (see below) are used. The monitor call returns a function value, and, if error, the K indicator is set. The returned function value always refers to the last function performed, that is, if disk transfer and check event is issued, the returned status is from check event.

Before a file can be accessed through the SMTRANS monitor call, the file must be opened for direct transfer (access codes 8,9,10 or 11). A file connect number can be specified, or an open file number is returned from SINTRAN. The connect (file) number is used to get a file magic number which is later used in Disk Transfer calls.

MON SMTRANS is reserved for internal use by ND. Note that SMTRANS can only be used if disk sorting is enabled.

#### Function = DISK TRANSFER

**Monitor call format:**

```
CALL 37000000515B,7,\<function\>,\<IO code\>,\<request id.\>,\<memory addr.\>,
     \<disk id.\>,\<sector\>,\<number of sectors\>
```

**Parameters:**

| Parameter              | Description                                                                                  |
|------------------------|----------------------------------------------------------------------------------------------|
| \<function\>           | bit number 0 set, disk transfer <br> bit number 16 set, no wait mode                         |
| \<IO code\>            | returned HW status (16 least significant bits)                                               |
| \<request identifier\> | any value (16 least significant bits)                                                        |
| \<memory address\>     | ND-100 physical memory address (must be a contiguously fixed area)                           |
| \<disk identifier\>    | bits 16-31 : logical device number <br> bits 6-8  : unit number <br> bits 0-5  : function (0 = read, 1 = write) |
| \<sector\>             | disk sector number                                                                           |
| \<number of sectors\>  | number of sectors to transfer                                                                |

Norsk Data ND-60.230.5 EN

---

## Page 192

# SINTRAN III RELEASE INFORMATION, K-VERSION
## ND-500 MONITOR - MONITOR CALLS

### Function values returned (octal value):
- **K = 0:**
  - 1 = OK, request received (nowait mode)
  - 3 = Transfer completed, `<IO code>` contains hardware status

- **K = 1:**
  - 6 = No disk optimization for this controller
  - 7 = Illegal read/write function
  - 10 = Segment is not contiguously fixed
  - 11 = Disk transfer error, `<IO code>` contains hardware status
  - 12 = Illegal SMTRANS function, neither disk transfer, check event nor start process
  - 14 = Illegal monitor call (not implemented)
  - 15 = Illegal file magic number
  - 17 = Not write access
  - 20 = Attempt to access outside file
  - 21 = Illegal unit number
  - 22 = Illegal logical device number

### Example:

| Parameter | Value       | Description                                       |
|-----------|-------------|---------------------------------------------------|
| DFUNC     | W DATA 1    | % disk transfer, wait for completion              |
| IOCODE    | W DATA 0    | % returned HW status                              |
| REQID     | W DATA 313  | % request ID = 313 (any value)                    |
| MEMAD     | W DATA 2000000000B | % ND-100 physical memory address          |
| DISKID    | W DATA 1100001B | % write to main disk (1100a) unit 0          |
| SECTOR    | W DATA 123B | % sector no. 123B                                 |
| NOSECT    | W DATA 2    | % transfer 2 sectors                              |

```plaintext
312    CALL 370000000515B,7,DFUNC,IOCODE,REQID,MEMAD,DISKID,SECTOR,NOSECT
406    IF K G0 ERROR
```

- `% Mon 515 with 7 parameters`
- `% on error return, IOCODE = error`

#### Rules:
1. There is no check on `<request ID>` to see if it already has been used on a pending transfer from the same process. A process may thus have several pending transfers with the same request id.

### Function = CHECK EVENT

#### Monitor call format:
```
CALL 370000000515B,3,<function>,<IO code>,<request id.>
```

#### Parameters:
- `<function>`: bit number 1 set, check event
- `<IO code>`: returned HW status (16 least significant bits)
- `<request identifier>`: any value (16 least significant bits), bit number 16 set, no wait mode

#### Function values returned (octal value):
- **K = 0:**
  - 2 = No event ready (nowait mode).
  - 3 = Transfer completed, `<IO code>` contains hardware status and `<request ID>` contains request identification.
  - 4 = No event ready but restarted by other.
  - 13 = Transfer completed and restarted by other `<IO code>` contains hardware status, `<request ID>` contains request ID of completed transfer.

_Norsk Data No-60.230.5 EN_

---

## Page 193

# SINTRAN III RELEASE INFORMATION, K-VERSION  
ND-500 MONITOR - MONITOR CALLS

K = 1:
- 11 = Disk transfer error, (IO code) contains hardware status
- 12 = Illegal SMTRANS function, neither disk transfer, check event, start process nor get magic number
- 14 = Illegal monitor call (not implemented)

## Rules:
1. `<request ID> = -1` means that the monitor call returns the request identifier of the least recently finished transfer. If wait flag is set and no transfers are finished, the process goes into a waiting state until any pending transfer is finished.
2. Note that if called in wait mode and the specified `<request ID>` is not contained in any pending transfer, or `<request ID> = -1` and there are no pending transfers, the process will go into an infinite waiting state.

## Example:
- DFUNC : W DATA 2
  - % check event, wait for completion
- IOCODE : W DATA 0
  - % returned HW status
- REQID : W DATA 313
  - % request ID = 313 (any value)

```
CALL 37000000515B,3,DFUNC,IOCODE,REQID % Mon 515 with 3 parameters
IF K GO ERROR % on error return, IOCODE = error
```

## Function = START PROCESS

### Monitor call format:
```
CALL 37000000515B,2,<function>,<process no.>
```

### Parameters:
- `<function>` : bit number 2 set, start process
- `<process no.>` :
  - bits no. 16-31 : process number
  - bits no. 0-15 : process magic number

### Function values returned (octal value):
| K Value  | Description  |
|----------|--------------|
| K = 0 : 0 | Process restarted after being stopped by: Start process in wait mode, Stop process, Switch process, etc. |
| 1        | OK, request received (process started, nowait mode) |
| 4        | OK, request received (wait mode) and restarted by other. |
| K = 1 : 5 | Illegal process number or process magic number |
| 12       | Illegal SMTRANS function, neither disk transfer, check event, start process nor get magic number |
| 14       | Illegal monitor call (not implemented) |

### Example:
- DFUNC : W DATA 4
  - % start process, wait for completion
- PROCNO : W DATA 0
  - % process number

```
CALL 37000000515B,2,DFUNC,PROCNO % Mon 515 with 2 parameters
IF K GO ERROR % on error return
```

Norsk Data ND-60.230.5 EN

---

## Page 194

# Function = GET MAGIC NUMBER

**Monitor call format:**

CALL 37000000515B,3,(function),<file no.>,<magic number>

**Parameters:**

- `<function>`: bit number 3 set, get magic number
- `<file no.>`: file connect number (16 least significant bits)
- `<magic number>`: returned magic number (16 least significant bits)

**Function values returned (octal value):**

- K = 0: 1 = OK, `<magic number>` contains returned magic number
- K = 1: 
  - 12 = Illegal SMTRANS function, neither disk transfer, check event, start process nor get magic number
  - 14 = Illegal monitor call (not implemented)
  - 16 = File not open for direct transfer or file is not contiguous

**Example:**

```
DFUNC : W DATA 10  % get magic number
FILNO : W DATA 101 % connect file number
MAGNO : W DATA 0   % returned magic number

CALL 37000000515B,3,DFUNC,FILNO,MAGNO % Mon 515 with 3 parameters
IF K GO ERROR                        % on error return
```

# 16.9 CHANGED DATA STRUCTURES (ND-500)

## PROCESS TABLE ENTRY

| 0  | Process table entry status (0=free/1=used) |
|----|-------------------------------------------|
| 1  | Process physical segment number           |
| 2  | Process name (22s words)                  |
| 24 | Program capability table (40s words)      |
| 64 | Data capability table (40s words)         |

Information returned by MON N500M functions 67s and 70s. See page 167.

The capability tables for program and data segments are kept in the 'stack' page on the data segment belonging to the process.

Norsk Data ND-60.230.5 EN

---

## Page 195

# SINTRAN III RELEASE INFORMATION, K-VERSION

### ND-500 MONITOR - DATA STRUCTURES

## PROGRAM CAPABILITY TABLE

*Generations 312 and earlier*

The size is one word for each of the 408 program segments.

| 17 | 16 | 15 | 14 | 13 | 12 | 11 | 10 | 07 | 06 | 05 | 04 | 03 | 02 | 01 | 00 |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| 0  | Physical segment number (= PST index) |

- Copy capability has been done on the segment
- Call to routine in other machine
- Direct segment (value = 0)

| 17 | 16 | 15 | 14 | 13 | 12 | 11 | 10 | 07 | 06 | 05 | 04 | 03 | 02 | 01 | 00 |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| 1  | Domain number | Segment number |

- Copy capability has been done on the segment
- Don’t use cache
- Call to routine in other machine
- Indirect segment (segment on other domain) (value = 1)

Logical program segment no. 378:

| 17 | 16 | 15 | 14 | 13 | 12 | 11 | 10 | 07 | 06 | 05 | 04 | 03 | 02 | 01 | 00 |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| 1  | 1  | | | | | | | | | | | | | | 0 |

- SINTRAN III monitor call capability

## DATA CAPABILITY TABLE

*Generations 312 and earlier*

The size is one word for each of the 408 data segments.

| 17 | 16 | 15 | 14 | 13 | 12 | 11 | 10 | 07 | 06 | 05 | 04 | 03 | 02 | 01 | 00 |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| | | | | | | | Physical segment number |

- Copy capability has been done on the segment
- Do not use cache (ND-500/SINTRAN III shared data)
- Parameter access permitted
- Write permitted

Norsk Data ND-60.230.5 EN

---

## Page 196

# SINTRAN III RELEASE INFORMATION, K-VERSION  
ND-500 MONITOR - DATA STRUCTURES

## ND-500 MONITOR'S SEGMENT TABLE ENTRY  
*(generations 406 and earlier)*

| 0  | Link to next physical segment in list |
|----|--------------------------------------|
| 1  | Link to previous physical segment in list |
| 2  | Physical segment number |
| 3  | Physical segment type (see next page) |
| 4  | Process number of the 1st process using the segment |
| 5  | Logical segment no. of the 1st process using this segment |
| 6  | Open file number if "swap on original" |
| 7  | Directory index of file | User index of file |

| 10 | Object block no. | Object index of file |
|----|---------------------------|-------------------------|
| 11 | Number of processes using the segment |
| 12 | Address of swap-file-part if swapped on swapfile Address of segment name if attached segment |
| 13 | Segment size in pages |
| 15 | Displacement of segment start in file (in pages) |

| 17 | Logical device number of segment directory |
|----|-------------------------------------------|
| 20 | Logical unit number of segment directory |
| 21 | Number of sectors per page in segment directory |
| 22 | Page address of the segment's index page |
| 24 | Segment directory base address in sectors |
| 26 | Physical segment name (40؉ words) |

*Information passed to swapper when segment is placed.*  
*This is not part of the monitor's segment entry.*

Norsk Data ND-60.230.5 EN  
Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 197

# SINTRAN III RELEASE INFORMATION, K-VERSION

## ND-500 MONITOR - DATA STRUCTURES

### PROGRAM CAPABILITY TABLE

*(generations 406 and later)*

The size is one word for each of the 40s program segments.

| 17 | 16 | 15 | 14 | 13 | 12 | 11 | 10 | 07 | 06 | 05 | 04 | 03 | 02 | 01 | 00 |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| 0  |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |

Physical segment number (= PST index)

**Call to routine in other machine**   
Direct segment (value = 0)

| 17 | 16 | 15 | 14 | 13 | 12 | 11 | 10 | 07 | 06 | 05 | 04 | 03 | 02 | 01 | 00 |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| 1  |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |

Domain number | Segment number

Don't use cache   
**Call to routine in other machine**  
Indirect segment (segment on other domain) (value = 1)

Logical program segment no. 378:

| 17 | 16 | 15 | 14 | 13 | 12 | 11 | 10 | 07 | 06 | 05 | 04 | 03 | 02 | 01 | 00 |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| 1  |  1 |    |    |    |    |    |    |    |    |    |    |    |    |    | 0  |

**SINTRAN III monitor call capability**

### DATA CAPABILITY TABLE

*(generations 406 and later)*

The size is one word for each of the 40s data segments.

| 17 | 16 | 15 | 14 | 13 | 12 | 11 | 10 | 07 | 06 | 05 | 04 | 03 | 02 | 01 | 00 |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
|    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |

Physical segment number

Do not use cache (ND-500/SINTRAN III shared data)  
Parameter access permitted  
Write permitted

Norsk Data ND-60.230.5 EN  
Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 198

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## ND-500 MONITOR - DATA STRUCTURES

### Format of Physical segment type:

- The segment has pages common with SINTRAN III segments  
- The segment belong to a standard domain  
- Sequential access of segment  
- Original contents of segment is dummy  
- Private for ND-500 Swapper  
- The segment can be expanded  
- Process segment  
- Can be used by several processes  

| 17 | 16 | 15 | 14 | 13 | 12 | 11 | 10 |
|----|----|----|----|----|----|----|----|
|    |    |    |    |    |    |    |    |

| 07 | 06 | 05 | 04 | 03 | 02 | 01 | 00 |
|----|----|----|----|----|----|----|----|
|    |    |    |    |    |    |    |    |

- Swap on original file  
- Write permitted  
- Logical segment types (see below)  
- Data segment  
- Segment shared in memory  

### Logical segment types (bits 15-12):

| Value | Description                     |
|-------|---------------------------------|
| 0     | Reentrant program               |
| 1     | Debug program                   |
| 2     | Other machine                   |
| 3     | Indirect segment                |
| 4     | Read-only segment               |
| 5     | Copy read/write shared          |
| 6     | Copy read-only shared           |
| 7     | Copy exclusively                |
| 10    | Original read/write shared      |
| 11    | Original shared                 |
| 12    | Original exclusively            |
| 13    | Scratched shared                |
| 14    | Scratch exclusively             |
| 15    | Attached shared                 |

Norsk Data ND-60.230.5 EN

---

## Page 199

# SINTRAN III RELEASE INFORMATION, K-VERSION

## ND-500 MONITOR - DATA STRUCTURES

### 16.10 Some Notes on Files Used from ND-500

- When a file is connected as segment, the file access specified will also reflect the segment access.

- Files can be opened for Common Write from ND-500 and simultaneously opened from ND-100 programs. This can cause problems, however, since the ND-100 and the ND-500 in this case access the physical file differently. You should use MON WSEGN (MON 416) frequently to avoid problems in this case.

- If a file open for use from ND-500 is set permanently open, it will still be closed by the command @CLOSE-FILE -1 or MON CLOSE {-1) executed from the ND-500.

- Using the command CLOSE-FILE in the Monitor calls MON COMND (MON 70) or MON UECOM (MON 317) will not close files open from ND-500 programs as seen from the ND-500. The files will be closed on the ND-100.

Norsk Data ND-60.230.5 EN

---

## Page 200

# 17. ND-500/5000 MONITOR (VERSION J)

The ND-500/5000 Monitor version J and ND-500/5000 Swapper are intended to be used under generation 500 of SINTRAN III/VSX, but the ND-500/5000 Monitor may also run under generation 406.

## 17.1 Hardware and Software Configurations

Both ND-500 series and ND-5000 series CPUs may run version J. Furthermore, the multi-CPU systems (ND-580 and ND-5900) are supported.

ND-500 series CPUs require generation 500 of SINTRAN III/VSX, whereas ND-5000 series CPUs require either of the generations 406 or 500.

## 17.2 Changed Installation Procedure

All software required to run an ND-500/5000 system is now delivered as one product: ND-500/5000 System Package for SINTRAN III/VSX, version K, generation 500 (ND-211305). This product is usually delivered on one diskette (for single-side/single-density format, three diskettes) to simplify installation.

The products concerned are:

- ND-500 Monitor (background part)
- ND-500 Swapper
- ERS/SINTRAN III Watchdog
- ND-500 Place Library

For a complete installation of these products, see the product description. An example of a complete installation (including these products) is given on pages 6-10 and in the SINTRAN III/VSX product description.

## 17.3 New Functionality

Version J supports the new domain files built by the ND-Linker (ND-211224) as well as the old domains.

The main advantages of the new domain files are:

- The DESCRIPTION-FILE:DESC is no longer needed.
- A domain is stored on a single file (of type :DOM) instead of a triple of files for each segment (of types :LINK, :PSEG and :DSEG).
- Domains may be placed ("started") faster because of reduced overhead.
- Domain files may be copied, both between users and over a COSMOS network, just as any other file.

For commands handling domains, (LIST-DOMAIN, PLACE-DOMAIN, etc.), the order of looking for a domain is as follows:

1. Search through standard domains
2. Then search through domain files
3. Then search through "old-fashioned" domains

For a further description on the new domain format, refer to pages 190-193.

---

Norsk Data ND-60.230.5 EN

---

## Page 201

# SINTRAN III RELEASE INFORMATION, K-VERSION

ND-500/5000 MONITOR (version J)

On a "trap-handler missing" trap, the trap-number is now reported.

For reports of error conditions, the ND-500 System Monitor now uses the ERS/SINTRAN III Watchdog. Refer to page 225 for more information about the watchdog.

## 17.4 MULTI-CPU SYSTEMS

As mentioned above, multi-CPU systems (ND-580 and ND-5900) are supported in version J of the ND-500/5000 Monitor and Swapper.

The command SET-CPU-STATUS is used to set the status of each CPU in the multi-CPU system (available/unavailable/auto allocation/fixed allocation).

Also note one significant difference between the ND-580 and ND-5900 systems:
- the ND-5900 uses one common execution queue, whereas
- the ND-580 has a separate execution queue for each CPU.

## 17.5 ERROR MESSAGES FROM THE ND-500/5000 MONITOR

A complete list of error messages and error codes returned from the ND-500 Monitor (version J) is found on pages 194-200.

---

## Page 202

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## ND-500/5000 MONITOR - COMMANDS

### 17.6 COMMANDS REMOVED IN THE ND-500/5000 MONITOR

#### 17.6.1 RESTART-PROCESS

### 17.7 MODIFIED COMMANDS TO THE ND-500/5000 MONITOR

#### 17.7.1 ABORT-PROCESS

The parameter `<Process number>` has changed the default radix from octal to decimal. If an octal number is to be given, it must be followed by the letter B (for example: 13B for 11₈).

#### 17.7.2 ATTACH-PROCESS

The parameter `<Process number>` has changed the default radix from octal to decimal. If an octal number is to be given, it must be followed by the letter B (for example: 13B for 11₈).

#### 17.7.3 BREAK

The parameter `<Count>` has changed the default radix from octal to decimal. If an octal number is to be given, it must be followed by the letter B (for example: 13B for 11₈).

#### 17.7.4 CHANGE-CPU

After a CHANGE-CPU command, the process will be allocated to run only on the CPU specified.  
If the CPU specified is set to "fixed-allocation" only user SYSTEM can allocate processes to run on it.

#### 17.7.5 FIX-SEGMENT-CONTIGUOUS

The command name is changed from FIX-SEGMENT-CONTIGOUS.

#### 17.7.6 GET-FLAG

The parameter `<Process number>` has changed the default radix from octal to decimal. If an octal number is to be given, it must be followed by the letter B (for example: 13B for 11₈).

#### 17.7.7 INSERT-IN-TIME-SLICE

The parameter `<Process number>` has changed the default radix from octal to decimal. If an octal number is to be given, it must be followed by the letter B (for example: 13B for 11₈).

---

## Page 203

# SINTRAN III RELEASE INFORMATION, K-VERSION

## ND-500/5000 MONITOR - COMMANDS

### 17.7.8 LIST-ACTIVE-PROCESSES

This command now lists the swapper process as process number 0.

### 17.7.9 LIST-ACTIVE-SEGMENTS

The parameter `<Process number>` has changed the default radix from octal to decimal. If an octal number is to be given, it must be followed by the letter B (for example: 13B for 11₈).

### 17.7.10 LIST-EXECUTION-QUEUE

The parameter `<Interval>` has changed the default radix from octal to decimal. If an octal number is to be given, it must be followed by the letter B (for example: 13B for 11₈).

### 17.7.11 LIST-PROCESS-TABLE-ENTRY

The parameter `<Process number>` has changed the default radix from octal to decimal. If an octal number is to be given, it must be followed by the letter B (for example: 13B for 11₈).

### 17.7.12 LIST-TIME-QUEUE

The parameter `<Interval>` has changed the default radix from octal to decimal. If an octal number is to be given, it must be followed by the letter B (for example: 13B for 11₈).

### 17.7.13 LOAD-CONTROL-STORE

The parameter `<File name>` has changed default value from the file CONTROL-STORE:DATA to a file name selected by the SINTRAN III system monitor according to CPU-type (single- or multi-CPU):

- CONTROL-1-STORE:DATA is default on CPU 1 of a multi-CPU system,
- CONTROL-2-STORE:DATA is default on CPU 2 of a multi-CPU system,
- CONTROL-STORE:DATA is default on a single-CPU system (unless the file CONTROL-1-STORE:DATA is found).

If the ND-500/5000 Monitor is run on generation 406 of SINTRAN III, there is no default file name.

### 17.7.14 LOGOUT-PROCESS

The parameter `<Process number>` has changed the default radix from octal to decimal. If an octal number is to be given, it must be followed by the letter B (for example: 13B for 11₈).

---

## Page 204

# 17.7.15 PRINT-PROCESS-LOG

The parameter `<Process number>` has changed the default radix from octal to decimal. If an octal number is to be given, it must be followed by the letter B (for example: 138 for 111o).

# 17.7.16 PROCESS-LOG-ALL

The order of the parameters are changes to conform to the order of parameters for the command PROCESS-LOG-ONE.

The new parameter sequence is: `<First process>` `<Interval>`

Further, both the parameters `<First process>` and `<Interval>` have changed the default radix from octal to decimal. If an octal number is to be given, it must be followed by the letter B (for example: 138 for 111o).

# 17.7.17 PROCESS-LOG-ONE

The parameters `<Process number>` and `<Interval>` have changed the default radix from octal to decimal. If an octal number is to be given, it must be followed by the letter B (for example: 138 for 111o).

# 17.7.18 PROCESS-STATUS

This command now lists the swapper process as process number 0 belonging to the RT-program 55WAP.

# 17.7.19 REMOVE-FROM-TIME-SLICE

The parameter `<Process number>` has changed the default radix from octal to decimal. If an octal number is to be given, it must be followed by the letter B (for example: 138 for 111o).

# 17.7.20 SET-FLAG

The parameter `<Process number>` has changed the default radix from octal to decimal. If an octal number is to be given, it must be followed by the letter B (for example: 138 for 111o).

# 17.7.21 START-PROCESS-LOG-ONE

The parameters `<Process number>` and `<Interval>` have changed the default radix from octal to decimal. If an octal number is to be given, it must be followed by the letter B (for example: 138 for 111o).

---

## Page 205

# SINTRAN III RELEASE INFORMATION, K-VERSION

## ND-500/5000 MONITOR - COMMANDS

### 17.7.22 SWAPPING-LOG

The parameter `<Interval>` has changed the default radix from octal to decimal. If an octal number is to be given, it must be followed by the letter B (for example: 13B for 11₈).

### 17.7.23 TEMPORARY-BREAK

The parameter `<Count>` has changed the default radix from octal to decimal. If an octal number is to be given, it must be followed by the letter B (for example: 13B for 11₈).

### 17.7.24 VERSION

On ND-500 systems, this command now reports CPU type in addition to the information about ND-500 System Monitor, ND-500 Background Monitor, and ND-500 Swapper. On ND-5000 systems the command also gives information about the version of the ACCP and the ECO level on each module.

An example of the information listed on ND-5000 systems is given below:

**ND-5000: VERSION**

| Subsystem part  | 88.2 18 REV.-J00   |
|-----------------|--------------------|
| System part     | 87.1.26            |
| Swapper         | 88.02.23           |
| Micro program   | 11225              |
| CPU type        | 5700               |
| ACCP version    | 87.12.11 E0        |

| Module | MB.2 | ALU.1 | AAP.1 | IDA.2 | MMS.1 | CS.2 | CACHE.1 | MIC.2 | ACCP.1 |
|--------|------|-------|-------|-------|-------|------|---------|-------|--------|
| ECO no | 13b  | 2d    | 2a    | 5a    | 3d    | 3c   | 2c      | 0b    | 7c     |

### 17.7.25 WHO-IS-ON

This command now lists the swapper process as process number 0 belonging to the RT-program 55WAP.

## 17.8 MODIFIED MONITOR CALLS (ND-500)

### 17.8.1 MAGTP MON 144

Function 23a (Set Density and Parity) is changed for SCSI magnetic tape compared to STC magnetic tape. For a list of legal values, refer to page 22.

---

## Page 206

# 17.9 New Domain Format on the ND-500/5000

## 17.9.1 General Information

In generation 500, a new domain format will be available on ND-500 and ND-5000 systems.

The main advantages of the new domain format are:

- **Safer**, the dependency on the description file is eliminated.
- **Fewer files involved** - only one file per segment as opposed to three.
- **Easier to copy** domains and segments (also between systems) as they are stored on single self-contained files.
- **Faster startup time** for domains, (fewer files involved).
- **Link lock/link key** concept introduced.

Use of new vs. old format is functionally transparent to the user. When a domain is started the ND-500 Monitor will first search for the domain in the new format, then in the old format.

Domains in both formats may be defined as standard domains.

## 17.9.2 Description of the New Domain Format

A domain will now reside on one self-contained domain file (of type :DOM).

The description-file is no longer necessary. Segments which are not shared, may be put directly into the domain file itself. Such segments are referred to as slave segments.

Domains may, as with the old format, share free segments within the same machine. A free segment resides on a single self-contained segment file (of type :SEG).

The control information necessary to make a domain and segment file self-contained, is stored in the first two pages of the file. This file header is referred to as the domain and segment header, respectively.

Both domains and free segments may have links to other free segments.

A link to a free segment is set up by a symbolic reference, the SINTRAN III file name, in the domain and segment headers, respectively.

---

## Page 207

# SINTRAN III RELEASE INFORMATION, K-VERSION  
NO-500/5000 MONITOR (VERSION J)

## 17.9.2.1 EXAMPLE OF THE NEW DOMAIN FORMAT

Let MYDOM be a domain consisting of two segments. The first segment, keeping the main program, is called MAIN. MAIN is unique for the domain MYDOM. MAIN is put as segment number 1. The second segment, keeping some libraries, is called LIB. LIB is a library segment used of several domains. LIB is put on segment number 20.

In the old domain format, the SINTRAN III files describing the domain will be:

| DESCRIPTION-FILE:DESC | MAIN:PSEG   | MAIN:DSEG   | MAIN:LINK       |
|-----------------------|-------------|-------------|-----------------|
| MYDOM                 | PROG1       | DATA1       | DEBUG1/LINK1    |
| link                  |             |             |                 |
|                       | LIB:PSEG    | LIB:DSEG    | LIB:LINK        |
|                       | PROG20      | DATA20      | DEBUG20/LINK20  |
| link                  |             |             |                 |

In the new domain format, the SINTRAN III files describing the domain will be:

| MYDOM:DOM            | LIB:SEG    |
|----------------------|------------|
| HEADER               | HEADER     |
| link                 |            |
|                      |            |
| DEBUG1               | DEBUG20    |
| link                 |            |
|                      |            |
| LINK1                | LINK20     |
| link                 |            |
|                      |            |
| PROG1                | PROG20     |
| link                 |            |
|                      |            |
| DATA1                | DATA20     |

## 17.9.3 THE LINK LOCK/LINK KEY CONCEPT

The link lock/link key is used to avoid inconsistency between the links from domains to free segments at place time.

At load time, when a free segment is loaded, a random number is attached to it, called the link lock. When a domain links to a free segment, the link lock of the free segment is copied and stored in the header of the domain file together with the file reference to the free segment. The same strategy applies when free segments link to other free segments.

---

## Page 208

# SINTRAN III RELEASE INFORMATION, K-VERSION

**ND-500/5000 MONITOR (VERSION J)**

At place time, the link keys of all linked segments in a domain are checked against the corresponding link locks of the free segments. If a mismatch is found, the placing of the domain is rejected. (The free segment has been loaded one or more times after the domain has linked to it.)

There is a Universal link lock/link key. A universal link lock may be seen as an "unlocked" free segment, and a universal link key as a master key to any version of the free segment. By using the universal link lock/link key, the checking done at place time is suppressed.

The link locks/link keys may be manipulated by command in ND-Linker's service program.

## 17.9.4 PORTABILITY

To copy a domain/free segment, just use any SINTRAN III copy file utility on the domain/segment file.

If a domain is copied to another user, the CHANGE-FILE-REFERENCES command in ND-Linker may be used to set up proper/complete file names to the linked segments. If necessary, this can be done after the file copying is done.

If a domain is copied to another machine, linked segments not already found on the other machine must be copied as well. The command CHANGE-FILE-REFERENCES in the ND-Linker can be used to set up proper/complete file names and/or corresponding link keys to the link segments, if necessary.

A domain linked to a segment that has matched an ND-100 segment or linked ND-100 programs should not be copied to another machine.

## 17.9.5 SYSTEM CHANGES/NEW SOFTWARE CONCERNING THE NEW FORMAT

### 17.9.5.1 ND-500 MONITOR, BACKGROUND PART

The commands RECOVER-DOMAIN, PLACE-DOMAIN, DEFINE-STANDARD-DOMAIN and LOOK-AT- are modified to support both the new and the old domain format.

A command LIST-STATUS <domain-name> is supplied, and will list the domain-entry together with segment-entries for segments linked to the domain. It is similar to a command with the same name in the new ND-Linker.

LIST-DOMAINS will perform a simple list-files :DOM in addition to the list routine for old domains.

Norsk Data ND-60.230.5 EN

---

## Page 209

# SINTRAN III RELEASE INFORMATION, K-VERSION

## ND-500/5000 MONITOR (VERSION J)

### 17.9.5.2 ND-500 MONITOR, SYSTEM PART

Two new MON 60 functions are introduced: NEWPLACE (160₈) and NEW-SYS_DOM (161₈). The first is the new place-segment call where a part of a file is placed as segment. More than one segment may be placed from the same file. The last is standard domain definition for the new format.

The MON 60 calls place (6) and define standard domain (127₈) use a new message to the swapper containing offset within the segment file. This offset is always zero, and the calls will work as before. Old calls are reimplemented for the new message format to the new swapper.

Note that when a domain in the new format is placed, several segments have the same name since they reside on the same file. They are internally distinguished by their individual offset on the file.

### 17.9.5.3 500-SWAPPER

The Swapper accepts file offset in the create-segment message, making it possible to connect part of a file as segment.

The swap file handling is not changed.

### 17.9.5.4 ND-LINKER

The ND-Linker is a new product for building domains/free segments in the new domain format. Functionally it is much the same as the Linkage-Loader, but it is more user-friendly. The noticeable difference being producing :DOM and :SEG files rather than :LINK, :PSEG, :DSEG and DESCRIPTION-FILE.

For more detailed information, see ND-Linker User and Reference Manual ND-60.289.

### 17.9.5.5 CONVERT-DOMAIN

A product is supplied to convert domains and segments from the old to the new domain format. For more information, see the ND-Linker User Guide and Reference Manual, ND-60.289.

### 17.9.5.6 SYMBOLIC-DEBUGGER

This is placed as an add-on segment as before, but has a communication buffer on top of its data segment. The background monitor fills information about all the segments in the domain into the communication buffer.

As there is no longer a corresponding :LINK file for each segment, the debug information and the segment contents must be fetched from different places. This information is collected by the monitor when the domain is placed. Due to this the debugger is independent of future changes in the domain format.

Norsk Data ND-60.230.5 EN

---

## Page 210

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## ND-500/5000 MONITOR (VERSION J)

### 17.10 ERROR MESSAGES FROM THE ND-500 MONITOR

#### 17.10.1 ERROR RETURNS FROM MONITOR CALLS FROM ND-500

| Number (octal) | Error message |
|----------------|---------------|
| 1000 | ND-500(0) open file table is full |
| 1001 | File is neither contiguous nor magnetic tape |
| 1002 | ND-500(0) open file table for direct transfer is full |
| 1003 | Error in monitor call |
| 1004 | Odd byte address |
| 1005 | Odd bytecount |
| 1006 | Too big bytecount |
| 1007 | Bytecount not modulo sector size in direct transfer |
| 1010 | Address outside file limits in direct transfer |
| 1011 | Block address not modulo sector size in direct transfer |
| 1012 | Hardware status error in direct transfer |
| 1013 | Illegal monitor call number |
| 1014 | DC access not legal on magnetic tape |
| 1015 | Wrong number of parameters in monitor call |
| 1016 | Byte pointer not modulo sector size in direct transfer |
| 1017 | Data area cannot be placed inside a 64k SINTRAN III segment |
| 1020 | Segment not modifiable |
| 1021 | Bytecount not modulo block size in direct transfer |
| 1022 | Illegal operation on file connected to a segment |
| 1023 | File already connected to a segment |
| 1024 | All logical data segments used |
| 1025 | Logical data segment already used |
| 1026 | Block size not modulo sector size |
| 1027 | Address outside program segment |
| 1030 | Address outside data segment |
| 1031 | Trying to write segment back on system Swap file |
| 1032 | Illegal memory type of specified area |
| 1033 | Max global fix |
| 1034 | Error in absolute fix |
| 1035 | Other segments have user fixed pages in the specified area |
| 1036 | Other segments have system fixed pages in the specified area |
| 1037 | Impossible to fix contiguously because pages already system fixed |
| 1040 | Impossible to fix contiguously because pages already user fixed |
| 1041 | No contiguous area available because other segments system fixed |
| 1042 | No contiguous area available because other segments user fixed |
| 1043 | Impossible to do contiguous fix. Area greater than the physical memory |
| 1044 | Not enough memory reserved by the ND-500(0) |
| 1045 | Trying to fix pages shared with a SINTRAN III segment |
| 1046 | Segment not in use |
| 1047 | The process has no Before Image Log segment |
| 1050 | No Swap file part available |
| 1051 | Swapping space not available |
| 1052 | No free physical segment |
| 1053 | Segment not modifiable |

Norsk Data ND-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 211

# SINTRAN III RELEASE INFORMATION, K-VERSION

### ND-500/5000 MONITOR (VERSION J)

| Number (octal) | Error message                           |
|----------------|-----------------------------------------|
| 1054           | Illegal process number                  |
| 1055           | Swap device error                       |
| 1056           | Privileged monitor call                 |
| 1057           | Illegal logical segment number          |
| 1060           | No such process                         |
| 1061           | Illegal address                         |
| 1062           | Swapper in use on another CPU           |
| 1063           | Table for attach segments is full       |
| 1064           | Attach segment name not found           |

## 17.10.2 ERROR MESSAGES FROM THE SYSTEM MONITOR

| Number (octal) | Error message                                           |
|----------------|---------------------------------------------------------|
| 2000           | ND-500(0) timeout                                       |
| 2001           | Illegal microprogram function                           |
| 2002           | Illegal status in message to ND-500(0)                  |
| 2003           | ND-500(0) DMA/Octobus error                             |
| 2004           | Illegal stop reason                                     |
| 2005           | Unknown trap                                            |
| 2006           | Error answer from the microprogram                      |
|                | (M0R/PF/PN/HE/ME/CP/MSR trap)                           |
| 2007           | Illegal register number                                 |
| 2010           | Illegal address                                         |
| 2011           | Illegal function code in MON 60                         |
| 2012           | Illegal segment number in load                          |
| 2013           | Illegal file number in load                             |
| 2014           | Fatal internal system error                             |
| 2015           | ND-500(0) reserved for special use                      |
| 2016           | No ND-500(0) process available                          |
| 2017           | No buffer available for data transfer                   |
| 2020           | Too great bytecount in data transfer                    |
| 2021           | Too many shared areas                                   |
| 2022           | No RT-COMMON defined                                    |
| 2023           | Shared segment fixed, but not contiguously              |
| 2024           | Shared segment fixed in wrong address                   |
| 2025           | Shared area outside ND-500(0) memory                    |
| 2026           | Too big program segment                                 |
| 2027           | Too big data segment                                    |
| 2030           | No ND-500(0) process to communicate with                |
| 2031           | Not enough memory available for segment                 |
| 2032           | Control Store not initialized                           |
| 2033           | DEFINE-MEMORY-CONFIGURATION command is required         |
| 2034           | Other user(s) already logged on ND-500(0)               |
| 2035           | ND-500(0) not reserved for special use                  |
| 2036           | No swap file part available                             |
| 2037           | Swapping space not available                            |
| 2040           | Swap file already defined                               |
| 2041           | Swap file is not a contiguous mass storage file         |
| 2042           | Swap file is in use                                     |
| 2043           | Swap file not found                                     |

Norsk Data ND-60.730.5 EN

---

## Page 212

# SINTRAN III Release Information, K-Version
## ND-500/5000 Monitor (Version J)

### Error Messages

| Number (octal) | Error message                                                                 |
|----------------|-------------------------------------------------------------------------------|
| 2044           | No free physical segment                                                      |
| 2045           | No free swap file entry                                                       |
| 2046           | Not mass storage file                                                         |
| 2047           | Fatal error from Swapper                                                      |
| 2050           | Memory not available                                                          |
| 2051           | Fatal microprogram error                                                      |
| 2052           | No ND-500(0) CPU found                                                        |
| 2053           | Memory for the context blocks not available                                   |
| 2054           | Error in memory configuration                                                 |
| 2055           | Histogram already in use                                                      |
| 2056           | Histogram not reserved by you                                                 |
| 2057           | ND-500(0) power off                                                           |
| 2060           | ND-500 interface error                                                        |
| 2061           | ND-500(0) stopped                                                             |
| 2062           | ND-500(0) power fail                                                          |
| 2063           | ND-500(0) power fail has occurred                                             |
| 2064           | ND-500(0) power up                                                            |
| 2065           | Illegal logical segment type                                                  |
| 2066           | Swapper must be loaded                                                        |
| 2067           | Illegal physical segment                                                      |
| 2070           | The Swapper stopped                                                           |
| 2071           | Timeout, impossible to terminate ND-500(0)                                    |
| 2072           | Microprogram break reached                                                    |
| 2073           | Logging facility not reserved by you                                          |
| 2074           | Logging facility already reserved                                             |
| 2075           | No memory available for ND-500(0) buffers                                     |
| 2076           | Segment not modifiable                                                        |
| 2077           | Illegal logical segment number                                                |
| 2100           | Not sufficient access to the segment                                          |
| 2101           | Function not implemented                                                      |
| 2102           | Name already used                                                             |
| 2103           | Error in loading Control Store                                                |
| 2104           | Too many fixed memory parts                                                   |
| 2105           | Mass storage transfer error in swapping                                       |
| 2106           | Too many SINTRAN III/ND-500(0) segments to fix                                |
| 2107           | Error in Standard Domain                                                      |
| 2110           | Standard Domain table is full                                                 |
| 2111           | Standard Domain in use                                                        |
| 2112           | Ambiguous Standard Domain name                                                |
| 2113           | No such Standard Domain                                                       |
| 2114           | RT-COMMON specified in domain, but does not exist in system                   |
| 2115           | Error in linking to RT-COMMON                                                 |
| 2116           | xx segment fixed in wrong physical address                                    |
| 2117           | Memory error detected by the microprogram                                     |
| 2120           | Control Store error detected by the ACCP                                      |
| 2121           | No memory is reserved for the ND-500(0)                                       |
| 2122           | Memory area not available for ND-500(0) segment                               |
| 2123           | xx trying to link to a demand segment in SINTRAN III                          |
| 2124           | RT-COMMON not contiguous                                                      |
| 2125           | The actual segment size does not fit the segment size specified in the Domain Entry |
| 2126           | No memory available for SINTRAN III segment in ND-500(0)/SINTRAN III shared memory area |
| 2127           | Function not allowed when attached to another process                         |

---

## Page 213

# SINTRAN III RELEASE INFORMATION, K-VERSION

## ND-500/5000 MONITOR (VERSION J)

| Number (octal) | Error message |
|----------------|---------------|
| 2130 | "Debug-swapper" is done from another process |
| 2131 | ND-5000 selftest failed |
| 2132 | ND-5000 lock timeout |
| 2134 | Not enough memory reserved by the ND-500(0) |
| 2135 | Trying to fix inside a SINTRAN III shared area |
| 2136 | Command not allowed from RT |
| 2137 | Illegal process number |
| 2140 | Not allowed in RESIDENT-PLACE |
| 2141 | No such ND-500(0) process |
| 2142 | ND-500(0) user break |
| 2143 | File system call not allowed on remote file |
| 2144 | Fatal user stop |
| 2145 | Trying to place an empty segment |
| 2146 | Illegal when kicks are enabled |
| 2147 | Illegal when microprogram is running |
| 2150 | Microprogram not started |
| 2151 | No parameter pointer given |
| 2152 | Illegal wordcount |
| 2153 | Illegal address |
| 2154 | Checksum error |
| 2155 | Hardware error in buffered CI-bits of Control Store |
| 2156 | No such command |
| 2157 | Microprogram has stopped |
| 2160 | Memory error |
| 2164 | Unknown |
| 2165 | ACCP was terminated; Microprogram is running |
| 2166 | ACCP was terminated; Microprogram has stopped |
| 2167 | Impossible to terminate ACCP after timeout |
| 2170 | No answer from ACCP |
| 2171 | ACCP command not implemented |
| 2172 | ACCP buffer exceeded during ACCP transmission |
| 2173 | ACCP echo test failed |
| 2174 | Verification of ACCP parameter pointer failed |
| 2175 | Control Store check failed |
| 2176 | Octobus buffer exceeded during ACCP transmission |
| 2177 | Fatal internal system error - ND-500(0) CPU locked |

### 17.10.3 ERRORS FROM THE ACCP OR MICROPROGRAM

| Number (octal) | Error message |
|----------------|---------------|
| 2200 | Memory error (Hardware fault) |
| 2201 | General trap message |
| 2203 | Wrong microprogram |
| 2204 | Unexpected Octobus kick |
| 2205 | Unexpected Octobus multibyte message |
| 2206 | Unexpected Octobus emergency message |
| 2207 | Unexpected ACCP command |
| 2210 | Unexpected external trap |
| 2211 | Size of WrittenInPage/PaGeUsed table is zero |
| 2212 | Physical segment table pointer is zero |
| 2277 | Microprogram error in DCB-message |

---

## Page 214

# 17.10.4 Fatal Errors from System Monitor

| Number (octal) | Error message |
|----------------|---------------|
| 2302 | PTSINTRAN: Memory map address less than start of memory map |
| 2303 | CLEEFOPEN: Specified open file number not found in FOPTABLE or in EXFOPTABLE |
| 2304 | SETCAP: No Process segment exists for this process |
| 2305 | MAKESEG: Specified physical page not found in memory map |
| 2306 | PLSWAPPER.CRFILE: Trying to read the Swappers PSEG or DSEG into ND-500(0) local memory |
| 2307 | PLSWAPPER.GFINFO: No open file table element found for an open file |
| 2310 | PLSWAPPER.GFINFO: Empty file (Swappers PSEG or DSEG) |
| 2311 | PLSWAPPER.GFINFO: Too big segment (Swappers PSEG or DSEG) |
| 2312 | PLSWAPPER.GFINFO: Swappers PSEG or DSEG file is double indexed |
| 2313 | PLSWAPPER: Illegal physical segment allocated for the Swapper |
| 2314 | PLSWAPPER: Start of Swappers segment table is outside Swappers data segment |
| 2315 | PLSWAPPER: End of Swappers segment table is outside Swappers data segment |
| 2316 | PLSWAPPER: Start of Swappers memory map is outside Swappers data segment |
| 2317 | PLSWAPPER: End of Swappers memory map is outside Swappers data segment |
| 2320 | PLSWAPPER: Actual end of Swappers memory map is outside Swappers data segment |
| 2321 | PLSWAPPER: Actual end of Swappers segment table is outside Swappers data segment |
| 2322 | PLSWAPPER: Swappers data segment is placed in ND-500(0) local memory |
| 2323 | PLSWAPPER: Error in reading the index page of SWAPPER:DSEG from mass storage |
| 2324 | PLSWAPPER: Error in linking the pages for SWAPPER:DSEG out of SINTRAN II's memory map |
| 2325 | PLSWAPPER: Error from ND-500(0) when writing the SWAPPER:DSEG index page into ND-500(0), using the communication |
| 2326 | PLSWAPPER: Error when reading the index page of SWAPPER:PSEG from mass storage |
| 2327 | PLSWAPPER: The SWAPPER:PSEG is placed in ND-500(0) local memory |
| 2330 | PLSWAPPER: Error when linking the pages for SWAPPER:PSEG out of SINTRAN II's memory map |
| 2331 | PLSWAPPER: Error when reading the SWAPPER:PSEG into memory from mass storage |
| 2332 | PLSWAPPER: Error when reading the SWAPPER:DSEG into memory from mass storage |
| 2333 | PLACE.DOOVERLAP: Specified page not found in RT-COMMON |
| 2334 | PLACE.FFSIZE: No open file table element found for an open file |
| 2335 | PLACE: No open file table element found for an open file |
| 2336 | DFSYDOM: No open file table element found for an open file |
| 2337 | DFSYDOM: No free Standard Domain segment entry |
| 2340 | OPSYDOM: No open file table element found for an open file |

*Norsk Data ND-60.230.5 EN*

---

## Page 215

# SINTRAN III RELEASE INFORMATION, K-VERSION

## ND-500/5000 MONITOR (VERSION J)

| Number (octal) | Error message |
|----------------|---------------|
| 2341 | GIVPAGES: Illegal memory map address |
| 2342 | PAGTOSMEMORY: Illegal memory map address |
| 2343 | PLACE.PLRESIDENT: Inconsistency in segment size in RESIDENT-PLACE |
| 2344 | PLSWAPPER: Swappers data segment is greater than 128mb |
| 2345 | PLSWAPPER: Swappers software segment table and memory map table overlap |

## 17.10.5 ND-500 TRAPS

| Number (octal) | Error message |
|----------------|---------------|
| 7605 | Zero |
| 7606 | Carry |
| 7607 | Sign |
| 7610 | Flag |
| 7611 | Overflow |
| 7612 | Not used |
| 7613 | Invalid operation |
| 7614 | Divide by zero |
| 7615 | Floating underflow |
| 7616 | Floating overflow |
| 7617 | BCD overflow |
| 7620 | Illegal operand value |
| 7621 | Single instruction trap |
| 7622 | Branch trap |
| 7623 | Call trap |
| 7624 | Break point instruction trap |
| 7625 | Address trap fetch |
| 7626 | Address trap read |
| 7627 | Address trap write |
| 7630 | Address zero access |
| 7631 | Descriptor range |
| 7632 | Illegal index |
| 7633 | Stack overflow |
| 7634 | Stack underflow |
| 7635 | Programmed trap |
| 7636 | Disable process switch timeout |
| 7637 | Disable process switch error |
| 7640 | Index scaling error |
| 7641 | Illegal instruction code |
| 7642 | Illegal operand specifier |
| 7643 | Instruction sequence error |
| 7644 | Protect violation |
| 7645 | Trap handler missing |
| 7646 | Page fault |
| 7647 | Power fault |
| 7650 | Processor fault |
| 7651 | Hardware fault |

---

## Page 216

# 17.10.6 Error Returns from Octobus Driver

| Number (octal) | Error message |
|----------------|---------------|
| 101401         | Interrupt level not supported |
| 101402         | Octobus Unit Number outside range |
| 101403         | Octobus Unit Number not present |
| 101404         | No free Octobus Message Device |
| 101405         | No free Ident Entry |
| 101406         | Ident Entry outside range |
| 101407         | No singlebyte message in Octobus Message Device input queue |
| 101410         | No multibyte message in Octobus Message Device input queue |
| 101411         | Nil message body pointer |
| 101412         | No message buffer available |
| 101413         | Output buffer is full |
| 101414         | Too small receive buffer defined |
| 101415         | Octobus Message Device not prepared for receiving multibyte message |
| 101416         | No free space to allocate received buffer |
| 101417         | Error in transmit queue link operation |
| 101420         | Transmission error; no reply after 256 attempts |
| 101421         | Destination station not present |
| 101422         | Destination station busy; kick/message not sent |
| 101423         | Transmission error; parity/hardware error |
| 101424         | Transmission error |
| 101425         | No bridge defined for this Ring |
| 101426         | Illegal message type |
| 101427         | Illegal number of bytes message |
| 101430         | Station number outside range or not known |
| 101431         | Octobus Message Device outside range |
| 101432         | Ident Entry/Octobus Message Device not reserved |
| 101433         | Ident Entry not present |
| 101434         | No transmit queue element available |
| 101435         | Receive buffer is full |
| 101436         | Illegal transmit identification |
| 101437         | Illegal function code in monitor call |
| 101440         | Illegal parameter; 16 bits value expected |
| 101441         | Routine not yet implemented |
| 101442         | Bridging not implemented |
| 101443         | Broadcast not implemented |

# 17.10.7 Errors from the Monitor Concerning the MF Controller

| Number (octal) | Error message |
|----------------|---------------|
| 101501         | Memory cycle time-out |
| 101502         | Memory cycle write parity |
| 101503         | Memory cycle corrected error |
| 101504         | Memory cycle unknown error |
| 101520         | IO cycle error |
| 101577         | Memory cycle bus Fatal error |

---

## Page 217

# SINTRAN III RELEASE INFORMATION, K-VERSION

## ND-500/5000 MONITOR (VERSION J)

### 17.11 DATA STRUCTURES

#### ND-500 DATA FIELDS AND EXECUTION QUEUE

| ND-500 DPIT data fields | MPM extended data fields | Execution queue (messages) |
|-------------------------|--------------------------|----------------------------|
| N500DF                  | X500DF                   |                            |
| (global)                | (global)                 |                            |
|                         |                          |                            |
| S5CPUDF                 | X5BEXQ                   | LINK                       |
| (CPU 1)                 | (CPU 1)                  | LINK=-1                    |
| MA1LINK                 |                          |                            |
|                         |                          |                            |
| (CPU 2)                 | X5BEXQ                   | LINK                       |
| MA1LINK                 | (CPU 2)                  | LINK=-1                    |
|                         |                          |                            |
| (CPU 3)                 | X5BEXQ=-1                |                            |
| MA1LINK                 | (CPU 3)                  |                            |
|                         |                          |                            |
| (CPU 4)                 | X5BEXQ                   | LINK=-1                    |
| MA1LINK                 | (CPU 4)                  |                            |

E5CPUDF

Note that there is a separate execution queue for each CPU.

Norsk Data ND-60.Z30.5 EN

---

## Page 218

# ND-5000 DATA FIELDS AND EXECUTION QUEUE

| ND-5000 DPIT data fields | MPM extended data fields | Execution queue (messages) |
|--------------------------|--------------------------|----------------------------|
| N5000DF                  | X500DF (global)          |                            |
| S5CPUDP                  |                          |                     DUMMESS |
| (CPU 1)                  | X5BEXQ (CPU 1)           |            LINK           |
| MAILINK                  |                          |           LINK            |
| (CPU 2)                  | X5BEXQ (CPU 2)           |            LINK           |
| MAILINK                  |                          | LINK = -1                 |
| (CPU 3)                  | X5BEXQ = -1 (CPU 3)      |                            |
| MAILINK                  |                          |                            |
| (CPU 4)                  | X5BEXQ (CPU 4)           |                            |
| MAILINK                  |                          |                            |
| E5CPUDP                  |                          |                            |

Note that there is one common execution queue.

Norsk Data ND-60.230.5 EN

---

## Page 219

# SINTRAN III RELEASE INFORMATION, K-VERSION

## ND-500/5000 MONITOR - DATA STRUCTURES

### Global ND-500(0) Data Field

#### Octal Displacement

16-bit word / byte

| Octal | Field Name   | Description                                      |
|-------|--------------|--------------------------------------------------|
| -46   | S5YMVERSION  | System Monitor version                           |
| -40   | S5WVERSION   | Swapper version                                  |
| -34   | S5BUM60      | First page for MON 60 buffers                     |
| -33   | X500DF       | Address of extended data field in 5MBBANK        |
| -32   | 5PGUWIPPAGE  | MPM address of WipPgu table                      |
| -31   | NPPGUWIP     | Number of pages in WipPgu table                  |
| -30   | 5PSTPAGE     | MPM address of PST-table                         |
| -27   | 5RESPAGE     |                                                  |
| -26   | 5ORGPSPAGE   | Currently not used                               |
| -25   | 5SHSGTPAGE   |                                                  |
| -24   | NCPU         | Number of ND-500(0) CPUs                         |
| -23   | NSPREF       | RT-desc. of program reserving ND-500(0)          |
| -22   | SYSINITFLAG  | ND-500(0) initialisation flag                    |
| -21   | ZADLINK      | Start memory map list res. for WipPgu, PST       |
| -20   | PHSLINK      | Start memory map list reserved for Swapper       |
| -17   | AN5PAGES     | Number of pages reserved for swapping            |
| -16   | 5NPAGES      | Number of pages reserved for system              |
| -15   |              |                                                  |
| -13   | NCCPUDF      | Next CPU having dynamic CPU allocation           |
| -12   | CCPUDF       | Current CPU data field                           |
| -11   | GMAGNO       | Global magic (cyclic) number                     |
| -10   | 5MCALLSEG    | Monitor call segment number                      |
| -7    | 5BUBANK      | DBUSTART : Address of MON 60 buffers             |
| -6    | 5BUSTART     |                                                  |
| -5    | AD5500S      | Address of Process descriptions                  |
| -4    | 5ATM1        | 5ATIME : Copy of SINTRAN III ATIME               |
| -3    | 5ATM2        |                                                  |
| -2    | NS5ERROR     | Save location of errcode                         |
| -1    | N5FUNCTION   | Default MON 60 function                          |
| 0     | RESLINK      |                                                  |
| 1     | RTRES        |                                                  |
| 2     | 8WLINK       |                                                  |
| 3     | TYPRING      | Standard data field locations                    |
| 4     | ISTATE       |                                                  |
| 5     | MLINK        |                                                  |
| 6     | MFUNC        |                                                  |

*To be continued*

Norsk Data NO-60.230.5 EN

---

## Page 220

# SINTRAN III RELEASE INFORMATION, K-VERSION
## NO-500/5000 MONITOR - DATA STRUCTURES

### Saved Register Block

| Addresses | Symbol   |
|-----------|----------|
| 7 / 16    | ZPREG    |
| 10 / 20   | ZXREG    |
| 11 / 22   | ZTREG    |
| 12 / 24   | ZAREG    |
| 13 / 26   | ZDREG    |
| 14 / 30   | ZLREG    |
| 15 / 32   | ZSREG    |
| 16 / 34   | ZBREG    |
| 17 / 36   | OLDPAGE  |

### MON 60 Function

| Addresses | Symbol    | Description                        |
|-----------|-----------|------------------------------------|
| 20 / 40   | SFUNCTION | MON 60 function                    |
| 21 / 42   | C50OSEG   | Data segment of current process    |
| 22 / 44   | 5PDRESCR  | Address of current process description |
| 23 / 46   | BUADR     | DBUADR : Address of buffer         |
| 24 / 50   | 5BUADR    |                                    |
| 25 / 52   | LOGBADR   | Logical address of device buffer   |
| 26 / 54   | SOLDSEG   | Initial segments of current program|
| 27 / 56   | 5RSEGM    | Reentrant segment of current program |
| 30 / 60   | 5SBITMAP  | Bitmap of current program          |
| 40 / 100  | 5DD1      | 5DD1 : MON 60 parameters           |
| 41 / 102  | 5DD2      |                                    |

### MON 60 Parameter Addresses

| Addresses | Symbol | Description                  |
|-----------|--------|------------------------------|
| 42 / 104  | 5P1    |                              |
| 43 / 106  | 5D21   | 5DD2                         |
| 44 / 110  | 5D22   |                              |
| 45 / 112  | 5P2    |                              |
| 46 / 114  | 5D31   | 5DD3                         |
| 47 / 116  | 5D32   |                              |
| 50 / 120  | 5P3    |                              |
| 51 / 122  | 5D41   | 5DD4                         |
| 52 / 124  | 5D42   |                              |
| 53 / 126  | 5P4    |                              |
| 54 / 130  | 5D51   | 5DD5                         |
| 55 / 132  | 5D52   |                              |
| 56 / 134  | 5P5    |                              |
| 57 / 136  | CNTXPAGE| Page address of Context block|
| 60 / 140  | ADRZERO | Page address of MPM address zero |
| 61 / 142  | AMEMTABLE| Memory part table           |
| 101 / 202 | TYPMTAB | Memory part type table       |

### System Parameters

| Addresses | Symbol       | Description                        |
|-----------|--------------|------------------------------------|
| 111 / 222 | NPHSEG       | Number of physical segments        |
| 112 / 224 | PGINITV      | Clear PGU Interval                 |
| 113 / 226 | TOOUTSW      | Outswap-Candidate                  |
| 114 / 230 | MAXFIX       | (system param.)                    |
| 116 / 234 | ADDNSPAGES   | Default extra pages                |
| 120 / 240 | FSYVARIABLE  | System dependent address           |

*Scanned by Jonny Oddene for Sintran Data © 2021*

---

## Page 221

# ND-500(0) CPU data field

Octal displacement  
16-bit  
word / byte

| Octal | Identifier    | Description                                       |
|-------|---------------|---------------------------------------------------|
| -15 / -32 | 5CPUSTOPPED   | CPU is terminated/idle                            |
| -14 / -30 | 5CPUNO        | CPU number                                        |
| -13 / -26 | WATCHDOG      | Address of watchdog message                       |
| -12 / -24 | DMLLIM        | Lower limit for interface                         |
| -11 / -22 | DMULIM        | Upper limit for interface                         |
| -10 / -20 | MIFLAG        | Microprogram flag                                 |
| -7 / -16  | 5MICPVERSION  | Microprogram version                              |
| -6 / -14  | TMSUB         |                                                   |
| -5 / -12  | TMR           |                                                   |
| -4 / -10  | TTMR          |                                                   |
| -3 / -6   | HDEV          |                                                   |
| -2 / -4   | STDRIV        |                                                   |
| -1 / -2   | DRIVER        |                                                   |
| 0 / 0     | RESLINK       | Standard data field locations                     |
| 1 / 2     | RTRES         |                                                   |
| 2 / 4     | BWLINK        |                                                   |
| 3 / 6     | TTYPRING      |                                                   |
| 4 / 10    | 1STATE        |                                                   |
| 5 / 12    | MLINK         |                                                   |
| 6 / 14    | MFUNC         |                                                   |
| 7 / 16    | TRLREG SPREF  | RT-description of reserving program               |
| 10 / 20   | 5INITFLAG     | CPU initialisation flag                           |
| 11 / 22   |               |                                                   |
| 13 / 26   | FERROR        | Fatal error code                                  |
| 14 / 30   | LEXQUEUE      | Length of execution queue                         |
| 15 / 32   | CS5TAT        | ND-500(0) status                                  |
| 16 / 24   | TMRXQ         | Waiting for watchdog answer                       |
| 17 / 36   | 5STATION      | Octopus station number of this CPU                |
| 20 / 40   | 5MODEL        | ND-5000 CPU model                                 |
| 21 / 42   | MAILLINK      | MAILLINK : Address of extend. data field          |
| 22 / 44   | MAILINK       |                                                   |
| 23 / 46   | LTTMR         | Long timeout                                      |
| 24 / 50   | 5PGLINK       | Start of memory map (releasing memory)            |
| 25 / 52   | 5RE5MEMORY    | Start memory map list (resident place)            |
| 26 / 54   | CPUAVAILABLE  | CPU present                                       |
| 27 / 56   | C5PWF         | Power fail flag                                   |


Norsk Data ND-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 222

# ND-500(0) MPM Extended Data Field - ND-500(0) Global Data Field

## Octal Displacement
16-bit word / byte

| Address | Identifier   | Description                                  |
|---------|--------------|----------------------------------------------|
| 0 / 0   | X5SEMA       | General global test-and-set semaphore        |
| 1 / 2   | X5NCPU       | Number of CPU's                              |
| 2 / 4   | X5LOGFLG     | Logging flag to MP                           |
| 3 / 6   | X5HENTE      | FIFO fetch index used by ND-100              |
| 4 / 10  | X5FYLLE      | FIFO fill index used by MP/ND-500            |
| 5 / 12  | X5MXFIFO     | Maximum number of elements in FIFO           |
| 6 / 14  | X5FIFO       | MPM address of FIFO buffer                   |
| 10 / 20 | X5MPBUF      | MPM address of buffer pool element           |
| 12 / 24 | X5SWOWNER    | MPM address of Swapper owner                 |
| 14 / 30 | X5SWMSG      | MPM address of Swapper msg                   |
| 16 / 34 | free         |                                              |
| 20 / 40 | X5BTIMEQ     | Start of ND-500 time queue                   |
| 22 / 44 | X5BACTIVEQ   | Start of active CPU queue                    |
| 24 / 50 | X5SEXFLG     | "Single execution mode" flag                 |
| 25 / 52 | X5MFLG       | Masked CPU-df Miflag word                    |
| 26 / 54 | X1STATION    | ND-100 octobus destination number            |
| 27 / 56 | X1ADRZERO    | MPM address zero                             |
| 30 / 60 | X5WIPTAB     | MPM address of Wip/Pgu table                 |
| 32 / 64 | X5PSTTAB     | MPM address of PST                           |
| 34 / 70 | X5CNXTIBLOCK | MPM address of Context block                 |
| 36 / 74 | X5CONFIG     | MPM address of System configuration          |
| 40 / 100| X5SMSVERS    | SINTRAN III System Monitor version           |
| 46 / 114| X5600LOCK    | NDIX 600-lock                                |
| 47 / 116| X5RESCPU     | CPU currently reserving general lock         |
| 50 / 120| X5SWHENTE    | Pointer in Swapper's FIFO queue              |
| 51 / 122| X5SWFYLLE    | Pointer in Swapper's FIFO queue              |
| 52 / 124| X5SWBUF      | Address of Swapper's FIFO queue              |
| 54 / 130| free         |                                              |
| 177 / 377|             |                                              |

CPU 1 Data Field

---

## Page 223

# SINTRAN III RELEASE INFORMATION, K-VERSION

## ND-500/5000 MONITOR - DATA STRUCTURES

### ND-500(0) MPM extended data field - ND-500(0) CPU data field

#### Octal displacement

16-bit

word / byte

| Word/Byte  | Field        | Description                                     |
|------------|--------------|-------------------------------------------------|
| 0 / 0      | X5BEXQ       | Start of execution queue                        |
| 2 / 4      | X5NACTIVEQ   | Next active CPU                                 |
| 4 / 10     | X5CPUSTATUS  | CPU status                                      |
| 5 / 12     | X5ACTIVATE   | Activate flag (polled by MP)                    |
| 6 / 14     | X5PROC       | Currently process in this CPU                   |
| 7 / 16     | X5STATION    | Octobus destination number for this CPU         |
| 10 / 20    | X5CLRFUNC    | Cache/TSB operation to be done                  |
| 11 / 22    | X5CLRCOUNT   | Cache clear counter                             |
| 12 / 24    | X5MIFLG      | Microprogram facility flag                      |
| 13 / 26    |              |                                                 |
| 16 / 34    | X5BRKFLG     | Microprogram break flag (Remote Debugger)       |
| 17 / 36    | X5CPFLG      | CPU flag word                                   |
| 20 / 40    | X5ACCPBUF    | ND-100 address of ACCP buffer                   |
| 22 / 44    | X5OCTBUF     | ND-100 address of Octobus buffer                |
| 24 / 50    | X5HWBUF      | ND-100 address of HW buffer                     |
| 26 / 54    |              |                                                 |
| 177 / 377  | free         |                                                 |

##### next CPU data field

500

---

## Page 224

# SINTRAN III RELEASE INFORMATION, K-VERSION
## ND-500/5000 MONITOR - DATA STRUCTURES

### ND-500(0) Mailbox

**Octal displacement**  
16-bit  
word / byte

| Octal | Symbol     | Description                                    |
|-------|------------|------------------------------------------------|
| -6 / -14 | 5CPUNO   | Current CPU no of this process                |
| -5 / -12 | 5PRIORITY | Priority of this process                     |
| -4 / -10 | HTSLOWPRI | Highest "low-timeslice" priority             |
| -3 / -6  | MAGNO    | Magic number of process ID                    |
| -2 / -4  | OUTDF    | Address of terminal output data field         |
| -1 / -2  | 5MSFLAG  | Flag word                                     |
| 0 / 0    | LINK     | Next in execution queue                       |
| 2 / 4    | NSSTATUS | Process status                                |
| 3 / 6    | SENDER   | Sender process number                         |
| 4 / 10   | 5RECEIVER | Receiver process number                      |
| 4 / 12   | 5CCLR    | Cache-clear counter                           |
| 6 / 14   | MICFUNC  | MP function                                   |
| 7 / 16   | SWFUNC   | Swapper function                              |
| 11 / 22  | KFLIP    | Error indicator                               |
| 12 / 24  | NUMPAR   | Bitmask for returned parameters               |
| 13 / 26  | FUNCVALUE| Function value/returned error code            |
| 15 / 32  | 26ADDRESS| Logical ND-500 data memory address            |
| 16 / 34  | TRAPN    | Trap number                                   |
| 17 / 36  | 26NRBYT  | Number of bytes                               |
| 37 / 76  | SMONO    | Saved monitor call number                     |
| 74 / 170 | SM26ADDRESS | Saved '26ADDRESS'                          |
| 135 / 272| SM26NRBYT| Saved '26NRBYT'                               |
| 143 / 306| SPFLAG   | Restart ND-500 driver address                 |

---

## Page 225

# SINTRAN III RELEASE INFORMATION, K-VERSION

## ND-500/5000 MONITOR - DATA STRUCTURES

### Format of X5CPFLG:

CPU is reserved for special use, excluded or unavailable.

```
17 16 15 14 13 12 11 10  07 06 05 04 03 02 01 00
```

### Format of X5LOGFLG:

- ND-100 monitor call logging
- Trap: Illegal instruction code
- Trap: Illegal operand specifier
- Trap: Instruction sequence error
- Trap: Protect violation
- Trap: Page fault

```
17 16 15 14 13 12 11 10  07 06 05 04 03 02 01 00
```

### Format of X5MFLG and X5MIFLG:

- Microcode function 26
- PST is 32 bits
- Octobus decoding facility
- Start Swapper facility
- Buffered I/O facility
- Trap trace facility
- AAP-hardware (floating in HW = 1, floating in µ-prog = 0)

```
17 16 15 14 13 12 11 10  07 06 05 04 03 02 01 00
```

### Values of X5PUSTATUS:

| Value | Description                                   |
|-------|-----------------------------------------------|
| 0     | CPU is not active (microprogram is not running) |
| 1     | CPU is active (microprogram is running)       |
| 2     | CPU is reserved for special use               |
| 3     | CPU failed (microprogram may be running)      |

Norsk Data ND-60.230.5 EN

---

## Page 226

# SINTRAN III RELEASE INFORMATION, K-VERSION
## ND-500/5000 MONITOR - DATA STRUCTURES

### Format of XSCLRFUNC:

- Clear ITSB
- Clear DTSB
- Clear ICACHE
- Clear DCACHE
- Dump dirty
- Forget process (load new context)

```
 17  16  15  14  13  12  11  10
 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
    07  06  05  04  03  02  01  00
    [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
    
  Wait for single-instruction flag and continue current process
```

### Format of SMSFLAG:

- Using fast UDMA option
- In time queue
- Saved bit 10 while using Swapper

```
 17  16  15  14  13  12  11  10
 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
    07  06  05  04  03  02  01  00
    [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
    
- process in escape/clean-up sequence
- process in escape/clean-up sequence possibly using Swapper
- saved CPU-bound flag when using histogram
- CPU bound process
- system reserved process
- process in execution queue
- privileged user
- repeat bit (restart process when it terminates)
```

---

## Page 227

# SINTRAN III RELEASE INFORMATION, K-VERSION

211

ND-500/5000 MONITOR - DATA STRUCTURES

## ND-500 MONITOR'S SEGMENT TABLE ENTRY

(generation 500)

|   | Description |
|---|-------------|
| 0 | Link to next physical segment in list |
| 1 | Physical segment status (0 = free, 1 = in use) |
| 2 | Physical segment number |
| 3 | Physical segment type (see next page) |
| 4 | Process number of the 1st process using the segment |
| 5 | Logical segment no. of the 1st process using this segment |
| 6 | Open file number if "swap on original" |
| 7 | Directory index of file<br>User index of file |
| 10 | Object block no.<br>Object index of file |
| 11 | Number of processes using the segment |
| 12 | Address of swap-file-part if swapped on swapfile<br>Address of segment name if attached segment |
| 13 | Segment size in pages |
| 14 |  |
| 15 | Displacement of segment start in file (in pages) |
| 16 |  |
| 17 | Logical device number of segment directory |
| 20 | Logical unit number of segment directory |
| 21 | Number of sectors per page in segment directory |
| 22 | Page address of the segment's index page |
| 23 |  |
| 24 | Segment directory base address in sectors |
| 25 |  |
| 26 | Physical segment name (40b words) |

Information passed to swapper when segment is placed.  
This is not part of the monitor's segment entry.

Norsk Data ND-60.230.5 EN

---

## Page 228

# 18. PLACE-LIBRARY VERSION C

## 18.1 INTRODUCTION

The Place-Library (ND-210624) is a collection of routines whose task is to place and start a domain in the ND-500 computers. This procedure is also done by the ND-500 Monitor, but there are special cases where it is desirable to have these routines as a program-controlled routine call. The library is useful when a process in the ND-500 is not interactive, or is a part of a larger system acting as a server. The way to use the library is to create a shadow process, foreground or background, in the ND-100 computer, and within this, use the library routines for controlled placing of domains in the ND-500.

## 18.2 CONTENTS OF PLACE-LIBRARY

There are two possible ways of using the Place-Library. There is one version which, in addition to the place operation, gives complete error diagnostics, and an alternative version which gives minor error information. Thus, when using the Place-Library, the version most suitable to the user, should be selected.

To reflect this, the Place-Library consists of three files at user level. One file is mandatory, and must always be used whenever the Place-Library functions are used. In addition to this file, one of the two other files must be used, depending on the versions selected.

- PLACE-1BANK-C:BRF  
  PLACE-2BANK-C:BRF

  This is the major file in the Place-Library, and contains the main entry points. This file must always be loaded with programs using the place functions.

- PLACE-BIG-1B-C:BRF  
  PLACE-BIG-2B-C:BRF

  This file, loaded together with PLACE-1BANK-C:BRF or PLACE-2BANK-C:BRF, would create a version of the Place-Library producing complete error messages, and trap diagnostics.

- PLACE-SML-1B-C:BRF  
  PLACE-SML-2B-C:BRF

  This file is to be used when no error messages are needed from the Place-Library. Only a message specifying the error number will be produced. Using this file with PLACE-1BANK-C:BRF or PLACE-2BANK-C:BRF, will save considerable space in memory compared with the version obtained by using PLACE-BIG-1B-C:BRF or PLACE-BIG-2B-C:BRF.

The use of these files is given in a later section, illustrated by examples.

---

## Page 229

# SINTRAN III RELEASE INFORMATION, K-VERSION

## PLACE-LIBRARY VERSION C

### 18.3 USE OF PLACE-LIBRARY

Whenever you want to run programs in the ND-500 computers, in a controlled environment guided by a server program, the Place-Library is most likely to be used.

The placing and running of a program, is achieved by using an external procedure call. In the Place-Library there is six different procedure calls, callable from either PLANC or FORTRAN, divided into two main groups. There is the common place routine, where only the domain name and the priority is specified, and a place routine for multi-CPU use.

For the FORTRAN functions, the returned value is the error value, having the value zero when no errors occurring. The reentrant version is for use by FORTRAN programs compiled with the REENTRANT-MODE ON option.

These routines may return abnormally, resulted by a detected error at some stage in the placing procedure. Depending on how the program is loaded, an error message is sent to the error device, and an error code returned to the server program.

As previously explained, there are two ways of loading a complete Place-Library, either by using PLACE-BIG--C:BRF, or PLACE-SML--C:BRF. The PlaceLibrary with PLACE-SML--C would, when an error occurs, produce a message in one of two following forms:

```
ERROR NUMBER: .....B
TRAP NUMBER: ..B
```

The PLACE-BIG--C:BRF file would produce a descriptive error message, or a trap name with additional trap information. A complete list of all error/trap numbers, and error/trap messages, is given in a later chapter.

If no output from the Place-Library is wanted, then this may be achieved during the loading procedure (RT-Loader or BRF-Linker), by patching a output flag in the library. When using the BRF-Linker to produce a executable :PROG file, the :BRF files from the Place-Library are at some stage loaded. If the output flag is to be changed, then after loading the library files, type the following command:

```
LIST-ENTRIES-DEFINED
```

Look up the symbol name no_output and take a note of its address. Depending on whether 1BANK or 2BANK have been used, the following will change the output flag:

| Option       | Command                      |
|--------------|------------------------------|
| 1BANK option | LOOK-AT-PROGRAM ADDRESS: <address-of-NO_OUTPUT> 1. |
| 2BANK option | LOOK-AT-DATA ADDRESS: <address-of-NO_OUTPUT> 1.    |

---

## Page 230

# 18.3.1 COMMON INTERFACE

**PLANC** : ROUTINE VOID, VOID(BYTES, INTEGER2): ND500  
**FORTRAN** : INTEGER ND500F  
**Reentrant** : INTEGER ND500R  

1st parameter: This is the name of the domain to be placed. Directory and user can be omitted and/or abbreviated. If omitted, standard domains are initially chosen. The search for the domain name is started among the domain files, before searching within the description file of the specified user. The domain name can be abbreviated as long as the name is not ambiguous, in which case an error message would be produced.

2nd parameter: This parameter specifies the priority of the process. Priority set to zero would result in time slicing, while priority greater than zero gives the program a fixed priority and thus removes it from the time slice queue.

# 18.3.2 MULTI-CPU INTERFACE

**PLANC** : ROUTINE VOID, VOID(BYTES, INTEGER2, INTEGER2, INTEGER2): ND500_X  
**FORTRAN** : INTEGER ND500F_X  
**Reentrant** : INTEGER ND500R_X  

1st parameter: This is the name of the domain to be placed. Directory and user can be omitted and/or abbreviated. If omitted, standard domains are initially chosen. The search for the domain name is started among the domain files, before searching within the description file of the specified user. The domain name can be abbreviated as long as the name is not ambiguous, in which case an error message would be produced.

2nd parameter: This parameter specifies the priority-function to be associated with the process. The function value is used in combination with the third parameter, the operand, to make up the complete priority of the process. This priority function can have any of the three legal values:

| Value | Description |
|-------|-------------|
| 1     | Set fixed priority of the process, and thus remove the process from the time slice queue. The 3rd parameter is taken as the priority value. |
| 2     | Insert the process in the time slice queue. Take the operand in the 3rd parameter as the value of the time slice class. |
| 3     | Remove the process from the time slice queue. The operand is not used, and is thus treated as a dummy. |

---

## Page 231

# SINTRAN III RELEASE INFORMATION, K-VERSION  
PLACE-LIBRARY VERSION C  

3rd parameter: This is the operand of the priority function. For further information see description of the 2nd parameter above.

4th parameter: This parameter specifies the CPU to be used by the process. If the parameter value is zero, then the CPU with the smallest load will be chosen. Other values specify which CPU is explicitly to be used. The CPU-values are in sequence from 1 to whatever number of CPU's available in the ND-500 computer. When you specify a non-existent CPU, or you use this routine on a single-CPU computer, the least-loaded CPU will be selected.

## 18.4 EXAMPLE OF USE  

In order to use the Place-Library, you need an application program, as previously explained. Here follow two very simple examples in PLANC and FORTRAN, illustrating a possible use of the Place-Library.

This PLANC-program simply reads a name of a domain, and starts it off in the ND-500 computer. This test program is stored under, say, the name MY-PLACE:SYMB. The following instructions would produce a program in 2-bank, using the complete error handler of the Place-Library.

```
@PLANC-100  
- ND-100 PLANC COMPILER - JUNE 9, 1986 VERSION G  
*SEPERATE-DATA ON  
*COMPILE 1.,,MY-PLACE:BRF"  

MODULE place test  
IMPORT (ROUTINE VOID,VOID(BYTES,INTEGER2):nd500)  
IMPORT (ROUTINE INTEGER,VOID:mon65)  
INTEGER ARRAY: stack(0:100)  
BYTES: domain_name(0:15)  
PROGRAM: place_server  
    INISTACK stack  
    OUTPUT(1,'A','Domain name:')  
    INPUT(1,'A16',domain_name)  
    ON ROUTINEERROR DO  
        ERRCODE mon65  
    ENDON  
    nd500(domain_name,0)  
ENDROUTINE  
ENDMODULE  
```

$EOF

18 LINES COMPILED. | 0 DIAGNOSTICS.

Norsk Data ND-60.230.5 EN

---

## Page 232

# SINTRAN III RELEASE INFORMATION, K-VERSION  
PLACE-LIBRARY VERSION C

## BRF-LINKER

- BRF Linker - NOVEMBER 13, 1985 (10721800)  
  Br1: PROG-FILE "MY-PLACE"  
  Br1: LOAD MY-PLACE  
  | FREE: P 000110-177777 | D 000167-177777 |
  |-----------------------|----------------|
  Br1: LOAD PLACE-1BANK-C  
  | FREE: P 007615-177777 | D 011005-177777 |
  Br1: LOAD PLACE-SML-1B-C  
  | FREE: P 013303-177777 | D 025777-177777 |
  Br1: LOAD PLACE-1BANK  
  | FREE: P 020355-177777 | D 026110-177777 |
  Br1: EXIT  

This FORTRAN program starts the domain MY-TEST which is allocated under the user PAAL-W. MY-TEST is to run in the time slice queue, and the CPU is to be selected arbitrarily. The program is compiled in 1-bank mode, and loaded with PLACE-SML-1B-C giving no error messages.

## FORTRAN-100

ND-100/NORD-10 ANSI 77 FORTRAN COMPILER - 203053F01  
FTN: COMPILE 1., "MY-TEST"

```
PROGRAM MYTEST  
EXTERNAL ND500F X  
INTEGER ND500F X  
WRITE(1,*) 'Hi, here comes MY-TEST.'  
ERR = ND500 X'(PAAL-W)MY-TEST',2,0,0)  
C  Error is detected and dumped on error device by nd500_x  
C  if err is different from zero.  
WRITE(1,*) 'Hi again, MY-TEST has terminated.'  
END  
```

## $EOF

- CPU TIME USED: 1.4 SECONDS. 10 LINES COMPILED.  
- NO MESSAGES  
- PROGRAM SIZE=169 COMMON SIZE=0  
FTN: EXIT  

## BRF-LINKER

- BRF Linker - 101721B00  
  Br1: PROG-FILE "MY-TEST"  
  Br1: LOAD MY-TEST  
  | FREE: P 000251-177777 |
  |-----------------------|
  Br1: LOAD PLACE-1BANK-C  
  | FREE: P 010261-177777 |
  Br1: LOAD PLACE-SML-1B-C  
  | FREE: P 011562-177777 |
  Br1: LOAD FORTRAN-1BANK  
  | FREE: 050774-1777777 |  
  Br1: EXIT  

Norsk Data ND-60.230.5 EN  
Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 233

# SINTRAN III RELEASE INFORMATION, K-VERSION
PLACE-LIBRARY VERSION C

## 18.5 ERROR MESSAGES WHEN USING THE PLACE-LIBRARY

In addition to the SINTRAN III run time errors, and the SINTRAN III file system errors, the Place-Library may produce its own set of errors. This chapter gives a complete list of all errors that may occur as a result of calling a Place-Library routine. The error number is returned to the trap-handler, if any, of the server program, where further action may be taken. The PLACE-SML--C file will send the error number/trap number to the error device, while PLACE-BIG--C will produce the respective error message.

### 18.5.1 ERRORS RETURNED FROM SYSTEM MONITOR (301B:320B)

| Error no. (octal) | Error message |
|-------------------|---------------|
| 301 | ILLEGAL FUNCTION IN MON 61 |
| 302 | RTCOMMON SPECIFIED IN DOMAIN BUT DOES NOT EXIST IN SYSTEM |
| 303 | RTCOMMON'S PHYSICAL ADDRESS DOES NOT MATCH THE PHYSICAL ADDRESS OF THE DOMAIN |
| 304 | ERROR IN LINKING TO RTCOMMON |
| 305 | FIXED SEGMENT HAS NO PAGES IN MEMORY |
| 306 | SEGMENT FIXED BUT NOT CONTIGUOUSLY |
| 307 | SEGMENT FIXED IN WRONG PHYS. ADDRESS |
| 310 | MEMORY AREA NOT AVAILABLE FOR ND-500 SEGMENT |
| 311 | TRYING TO LINK TO A NON-EXISTING SINTRAN III SEGMENT |
| 312 | RTCOMMON SIZE SPECIFIED DOES NOT MATCH THE ACTUAL RTCOMMON SIZE |
| 313 | TRYING TO LINK TO A DEMAND SEGMENT IN SINTRAN III |
| 314 | RTCOMMON NOT CONTIGUOUS |
| 315 | SHARED SEGMENT DOES NOT OVERLAP NO-500 SEGMENT |
| 316 | THE ACTUAL SEGMENT SIZE DOES NOT FIT THE SEGMENT SIZE SPECIFIED IN THE DOMAIN ENTRY |
| 317 | NO MEMORY AVAILABLE FOR SINTRAN III SEGMENT IN THE ND-500/SINTRAN III SHARED MEMORY AREA |
| 320 | *** FATAL SYSTEM ERROR *** FATAL PITO ERROR. ERROR CODE: xxxxxxB |

### 18.5.2 ERRORS TO ND-500 FROM MONITOR CALLS (1000B:1061B)

| Error no. (octal) | Error message |
|-------------------|---------------|
| 1000 | ND-500 OPEN FILE TABLE IS FULL |
| 1001 | FILE IS NEITHER CONTINUOUS NOR MAG. TAPE |
| 1002 | ND-500 OPEN FILE TABLE FOR DIRECT TRANSFER IS FULL |
| 1003 | ERROR IN MONITOR CALL |
| 1004 | ODD BYTE ADDRESS |
| 1005 | ODD BYTECOUNT |
| 1006 | TOO BIG BYTECOUNT |
| 1007 | BYTECOUNT NOT MODULO SECTOR SIZE IN DIRECT TRANSFER |
| 1010 | ADDRESS OUTSIDE FILE LIMITS IN DIRECT TRANSFER |
| 1011 | BLOCK ADDRESS NOT MODULO SECTOR SIZE IN DIRECT TRANSFER |
| 1012 | HARDWARE STATUS ERROR, IN DIRECT TRANSFER |

Norsk Data ND-60.230.5 EN

---

## Page 234

# SINTRAN III Release Information, K-Version
Place-Library Version C

| Code | Description |
|------|-------------|
| 1013 | ILLEGAL MONITOR CALL NUMBER |
| 1014 | DC ACCESS NOT LEGAL ON MAG. TAPE |
| 1015 | WRONG NUMBER OF PARAMETERS IN MON. CALL |
| 1016 | BYTE POINTER NOT MODULO SECTOR SIZE IN DIRECT TRANSFER |
| 1017 | DATA AREA CANNOT BE PLACED INSIDE A 64K SINTRAN III SEGMENT |
| 1020 | SEGMENT NOT MODIFYABLE |
| 1021 | BYTECOUNT NOT MODULO BLOCK SIZE IN DIRECT TRANSFER |
| 1022 | ILLEGAL OPERATION ON FILE CONNECTED TO A SEGMENT |
| 1023 | FILE ALREADY CONNECTED TO A SEGMENT |
| 1024 | ALL LOGICAL DATA SEGMENTS USED |
| 1025 | LOGICAL DATA SEGMENT ALREADY USED |
| 1026 | BLOCK SIZE NOT MODULO SECTOR SIZE |
| 1027 | ADDRESS OUTSIDE PROGRAM SEGMENT |
| 1030 | ADDRESS OUTSIDE DATA SEGMENT |
| 1031 | TRYING TO WRITE SEGMENT BACK ON SYSTEM SWAP FILE |
| 1032 | ILLEGAL MEMORY TYPE OF SPECIFIED AREA |
| 1033 | MAX GLOBAL FIX |
| 1034 | ERROR IN ABSOLUTE FIX |
| 1035 | OTHER SEGMENTS HAS USER FIXED PAGES IN THE SPECIFIED AREA |
| 1036 | OTHER SEGMENTS HAS SYSTEM FIXED PAGES IN THE SPECIFIED AREA |
| 1037 | IMPOSSIBLE TO DO FIX CONTIGUOUS BECAUSE OF ALREADY SYSTEM FIXED PAGES |
| 1040 | IMPOSSIBLE TO DO FIX CONTIGUOUS BECAUSE OF ALREADY USER FIXED PAGES |
| 1041 | NO CONTIGUOUS AREA AVAILABLE BECAUSE OF SYSTEM FIXED OF OTHER SEGMENTS |
| 1042 | NO CONTIGUOUS AREA AVAILABLE BECAUSE OF USER FIXED OF OTHER SEGMENTS |
| 1043 | IMPOSSIBLE TO DO CONTIGUOUS FIX. AREA GREATER THAN THE PHYSICAL MEMORY |
| 1044 | NOT ENOUGH MEMORY RESERVED BY THE ND500 |
| 1045 | TRYING TO FIX PAGES SHARED WITH A SINTRAN III SEGMENT |
| 1046 | SEGMENT NOT IN USE |
| 1047 | THE PROCESS HAS NO BEFORE IMAGE LOG SEGMENT |
| 1050 | NO SWAP-FILE PART AVAILABLE |
| 1051 | SWAPPING SPACE NOT AVAILABLE |
| 1052 | NO FREE PHYSICAL SEGMENT |
| 1053 | SEGMENT NOT MODIFYABLE |
| 1054 | ILLEGAL PROCESS NUMBER |
| 1055 | SWAP DEVICE ERROR |
| 1056 | PRIVILEGED MONITOR CALL |
| 1057 | ILLEGAL LOGICAL SEGMENT NUMBER |
| 1060 | NO SUCH PROCESS |
| 1061 | ILLEGAL ADDRESS |

---

## Page 235

# SINTRAN III RELEASE INFORMATION, K-VERSION
PLACE-LIBRARY VERSION C

## 18.5.3 ERROR MESSAGES FROM THE SYSTEM MONITOR (2000B:2347B)

| Error no. (octal) | Error message |
|-------------------|---------------|
| 2000 | ND-500/5000 time-out |
| 2001 | Illegal micro function |
| 2002 | Illegal status in message to ND-500/5000 |
| 2003 | ND-500/5000 DMA/octobous error |
| 2004 | Illegal stop reason |
| 2005 | Unknown trap |
| 2006 | Error answer from the Micro program |
| 2007 | Illegal register number |
| 2010 | Illegal address |
| 2011 | Illegal function code in MON 60 |
| 2012 | Illegal segment number in load |
| 2013 | Illegal file number in load |
| 2014 | Fatal error from System Monitor |
| 2015 | ND-500/5000 reserved for special use |
| 2016 | No ND-500/5000 process available |
| 2017 | No buffer available for data transfer |
| 2020 | Too great byte count in data transfer |
| 2021 | Too many shared areas |
| 2022 | No RT-common defined |
| 2023 | Shared segment fixed, but not contiguously |
| 2024 | Shared segment fixed in wrong address |
| 2025 | Shared area outside ND-500/5000 memory |
| 2026 | Too big program segment |
| 2027 | Too big data segment |
| 2030 | No ND-500/5000 process to communicate with |
| 2031 | Not enough memory available for segment |
| 2032 | Control Store not initialized |
| 2033 | Define-Memory-Configuration command is required |
| 2034 | Other user(s) already logged on ND-500/5000 |
| 2035 | ND-500/5000 not reserved for special use |
| 2036 | No Swap file part available |
| 2037 | Swapping space not available |
| 2040 | Swap file already defined |
| 2041 | Swap file is not contiguous mass storage file |
| 2042 | Swap file is in use |
| 2043 | Swap file not found |
| 2044 | No free physical segment |
| 2045 | No free Swap file entry |
| 2046 | Not mass storage file |
| 2047 | Fatal error from Swapper |
| 2050 | Memory not available |
| 2051 | Fatal Micro program error |
| 2052 | ND-500/5000 Monitor not initialized |
| 2053 | Memory for the Context blocks not available |
| 2054 | Error in memory configuration |
| 2055 | Histogram already in use |
| 2056 | Histogram not reserved by you |
| 2057 | ND-500/5000 power off |
| 2060 | ND-500 interface error |
| 2061 | ND-500/5000 stopped |

---

## Page 236

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## PLACE-LIBRARY VERSION C  

| Code | Description |
|------|-------------|
| 2062 | ND-500/5000 power fail |
| 2063 | ND-500/5000 power fail has occured |
| 2064 | ND-500/5000 power up |
| 2065 | Illegal logical segment type |
| 2066 | Swapper must be loaded |
| 2067 | Illegal physical segment |
| 2070 | The Swapper stopped |
| 2071 | Timeout, impossible to terminate ND-500/5000 |
| 2072 | Micro program break reached |
| 2073 | Logging system not reserved |
| 2074 | Logging system already reserved |
| 2075 | No memory available for ND-500/5000 buffers |
| 2076 | Segment not modifyable |
| 2077 | Illegal logical segment number |
| 2100 | Not required access to the segment |
| 2101 | Function not implemented |
| 2102 | Name already used |
| 2103 | Error in loading Control Store |
| 2104 | Too many fixed memory parts |
| 2105 | Mass storage transfer error in swapping |
| 2106 | Too many Sintran III/ND-500/5000 segments to fix |
| 2107 | Error in Standard domain |
| 2110 | Standard domain table is full |
| 2111 | Standard domain in use |
| 2112 | Ambiguous Standard domain |
| 2113 | No such Standard domain |
| 2114 | RT-common specified in domain, but RT-common does not exist in system |
| 2115 | Error in linking to RT-common |
| 2116 | xx segment fixed in wrong physical address |
| 2117 | Memory error detected by the ND-5000 Micro program |
| 2120 | ND-5000 Control Store error detected by the ACCP |
| 2121 | No memory is reserved for the ND-500/5000 |
| 2122 | Memory area not available for ND-500/5000 segment |
| 2123 | xx trying to link to a demand segment in Sintran III |
| 2124 | RT-common not contiguous |
| 2125 | The actual segment size does not fit the segment size specified in the domain entry |
| 2126 | No memory available for Sintran III segment in ND-500/5000/Sintran III shared memory area |
| 2127 | Function not allowed when in "debug-swapper" mode |
| 2130 | "Debug-swapper" is done from another process |
| 2131 | - Not used |
| 2132 | - Not used |
| 2133 | - Not used |
| 2134 | Not enough memory reserved by the ND-500/5000 |
| 2135 | Trying to fix inside a Sintran III shared area |
| 2136 | Command not allowed from RT |
| 2137 | Illegal process number |
| 2140 | Not allowed in Resident-place |
| 2141 | No such process |
| 2142 | ND-500/5000 user break |
| 2143 | Filesystem call not allowed on remote opened files |
| 2144 | User called fatal stop |
| 2145 | Trying to place an empty segment |  

Norsk Data ND-60.230.5 EN

---

## Page 237

# SINTRAN III RELEASE INFORMATION, K-VERSION

## PLACE-LIBRARY VERSION C

| Code | Message                                                                                      |
|------|----------------------------------------------------------------------------------------------|
| 2146 | ACCP command status=-2 : Illegal when kicks are enabled                                      |
| 2147 | ACCP command status=-1 : Illegal when Micro-program is running                               |
| 2150 | ACCP command status= 0 : Micro program not started                                           |
| 2151 | ACCP command status= 1 : No parameter pointer given                                          |
| 2152 | ACCP command status= 2 : Illegal word count                                                  |
| 2153 | ACCP command status= 3 : Illegal address                                                     |
| 2154 | ACCP command status= 4 : Checksum error                                                      |
| 2155 | ACCP command status= 5 : Hardware error in Control Stores buffered CI-bits                   |
| 2156 | ACCP command status= 6 : Not defined as ACCP command                                         |
| 2157 | ACCP command status= 7 : ND-5000 Micro program has stopped                                   |
| 2160 | ACCP command status= 8 : Memory error                                                        |
| 2161 | Unknown ACCP command status                                                                  |
| 2162 | 2162B                                                                                        |
| 2163 | 2163B                                                                                        |
| 2164 | 2164B                                                                                        |
| 2165 | ND-5000 timeout: ACCP was terminated, ND-5000 Micro program is running                       |
| 2166 | ND-5000 timeout: ACCP was terminated, ND-5000 Micro program has stopped                      |
| 2167 | ND-5000 timeout: Impossible to terminate ACCP after timeout                                  |
| 2170 | This ACCP command is not available through MON 60; function 157                              |
| 2171 | ND-5000 timeout: No answer from ACCP                                                         |
| 2172 | Exceeding ACCP buffer during ACCP transmission                                               |
| 2173 | ACCP echo test failed                                                                        |
| 2174 | Verifying ACCP parameter pointer failed                                                      |
| 2175 | Checking Control Store failed                                                                |
| 2176 | Exceeding octobus buffer during ACCP transmission                                            |
| 2177 | ND-5000 selftest failed                                                                      |

| Code | Message                                                                                      |
|------|----------------------------------------------------------------------------------------------|
| 2200 | Octobus error: Interrupt level not supported by octobus driver or not called from correct level |
| 2201 | Octobus error: Octobus unit number outside range                                             |
| 2202 | Octobus error: Octobus unit number not present                                               |
| 2203 | Octobus error: No octobus message device (omd) is free                                       |
| 2204 | Octobus error: No ident entry is free for connection to specific station                     |
| 2205 | Octobus error: Ident entry outside range                                                     |
| 2206 | Octobus error: No single-byte message in octobus message device (omd) input queue            |
| 2207 | Octobus error: No multi-byte message in octobus message device (omd) input queue             |
| 2210 | Octobus error: Nil message body pointer                                                      |
| 2211 | Octobus error: No buffer is available for sending multi-byte message                         |
| 2212 | Octobus error: Output buffer for sending single-byte message/kick is full                    |
| 2213 | Octobus error: Too small receive buffer defined during connection with omd (only for MC68xx) |
| 2214 | Octobus error: OMD not prepared for receiving multi-byte message (only for MC68xx)           |
| 2215 | Octobus error: No more place to allocate received buffer (only for MC68xx)                   |

---

## Page 238

# SINTRAN III Release Information, K-Version  
## Place-Library Version C

| Code | Description |
|------|-------------|
| 2216 | Octobus error: Error in transmit queue link operation |
| 2217 | Octobus error: Transmit error: Already tried to send the message 256 times unsuccessfully |
| 2220 | Octobus error: Destination station not present, the message is not sent |
| 2221 | Octobus error: Destination station is busy and the kick/message is not sent |
| 2222 | Octobus error: Parity error/Hardware error occurred while sending message |
| 2223 | Octobus error: Error when transmitting the message |
| 2224 | Octobus error: No bridge defined for this ring |
| 2225 | Octobus error: Illegal message type (only returned from octobus driver in ND-100) |
| 2226 | Octobus error: Illegal number of bytes in multi-byte message (1-377b) |
| 2227 | Octobus error: Station number outside of range (1-76b) or not known by octobus driver |
| 2230 | Octobus error: Octobus message device outside of range (0-17b) |
| 2231 | Octobus error: Ident entry/octobus message device not reserved |
| 2232 | Octobus error: Ident entry not present |
| 2233 | Octobus error: No transmit queue element available |
| 2234 | Octobus error: Receive buffer of the application is full |
| 2235 | Octobus error: Illegal transmit identification |
| 2236 | Octobus error: Illegal function code in monitor call |
| 2237 | Octobus error: Illegal parameter (value must be a ND-100 word - 16 bits) |
| 2240 | Octobus error: Routine not yet implemented |
| 2241 | Octobus error: Bridging not implemented |
| 2242 | Octobus error: Broadcast not implemented |

| Code | Description | Errcode |
|------|-------------|---------|
| 2300 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 0 |
| 2301 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 1 |
| 2302 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 2 |
| 2303 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 3 |
| 2304 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 4 |
| 2305 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 5 |
| 2306 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 6 |
| 2307 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 7 |
| 2310 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 10 |
| 2311 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 11 |
| 2312 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 12 |
| 2313 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 13 |
| 2314 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 14 |
| 2315 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 15 |
| 2316 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 16 |
| 2317 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 17 |
| 2320 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 20 |
| 2321 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 21 |
| 2322 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 22 |
| 2323 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 23 |
| 2324 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 24 |
| 2325 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 25 |
| 2326 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 26 |
| 2327 | ** Fatal error from ND-500/5000 System Monitor ** | Errcode: 27 |

Norsk Data ND-60.230.5 EN

---

## Page 239

# SINTRAN III RELEASE INFORMATION, K-VERSION

PLACE-LIBRARY VERSION C

|   |   |   |
|---|---|---|
| 2330 | ** Fatal error from ND-500/5000 System Monitor ** Errcode: 30 |
| 2331 | ** Fatal error from ND-500/5000 System Monitor ** Errcode: 31 |
| 2332 | ** Fatal error from ND-500/5000 System Monitor ** Errcode: 32 |
| 2333 | ** Fatal error from ND-500/5000 System Monitor ** Errcode: 33 |
| 2334 | ** Fatal error from ND-500/5000 System Monitor ** Errcode: 34 |
| 2335 | ** Fatal error from ND-500/5000 System Monitor ** Errcode: 35 |
| 2336 | ** Fatal error from ND-500/5000 System Monitor ** Errcode: 36 |
| 2337 | ** Fatal error from ND-500/5000 System Monitor ** Errcode: 37 |
| 2340 | ** Fatal error from ND-500/5000 System Monitor ** Errcode: 40 |
| 2341 | ** Fatal error from ND-500/5000 System Monitor ** Errcode: 41 |
| 2342 | ** Fatal error from ND-500/5000 System Monitor ** Errcode: 42 |
| 2343 | ** Fatal error from ND-500/5000 System Monitor ** Errcode: 43 |
| 2344 | ** Fatal error from ND-500/5000 System Monitor ** Errcode: 44 |
| 2345 | ** Fatal error from ND-500/5000 System Monitor ** Errcode: 45 |
| 2346 | ** Fatal error from ND-500/5000 System Monitor ** Errcode: 46 |
| 2347 | ** Fatal error from ND-500/5000 System Monitor ** Errcode: 47 |

## 18.5.4 SPECIAL ERRORS FROM PLACE LIBRARY (40008:4025B)

| Error no. (octal) | Error message |
|---|---|
| 4000 | MISSING " )" IN DOMAIN NAME (DIRECTORY:USER)  |
| 4001 | NO SUCH DOMAIN |
| 4002 | ERROR IN START-PLACE ROUTINE |
| 4003 | SEGMENT NOT PROPERLY LOADED |
| 4004 | ERROR IN END-PLACE ROUTINE |
| 4005 | NO SUCH SEGMENT IN DOMAIN |
| 4006 | ILLEGAL N100 COMMON SEGMENT SEQUENCE |
| 4007 | INSUFFICIENTLY LOADED SEGMENT |
| 4010 | BUFFER ERROR |
| 4011 | AMBIGUOUS DOMAIN NAME |
| 4012 | EMPTY DOMAIN |
| 4013 | PROGRAM SEGMENT EXPECTED |
| 4014 | DATA SEGMENT EXPECTED |
| 4015 | ILLEGAL LOGICAL SEGMENT NUMBER IN FREE SEGMENT |
| 4016 | KEY DOES NOT MATCH LINKLOCK ON FREE SEGMENT |
| 4017 | AMBIGUOUS FREE SEGMENT NAME IN DOMAIN |
| 4020 | NO SUCH SEGMENT IN DOMAIN |
| 4021 | FILE ILLEGAL AS SEGMENT FILE FOR ND-500/SINTRAN III |
| 4022 | FILE ILLEGAL AS DOMAIN FILE FOR ND-500/SINTRAN III |
| 4023 | ILLEGAL PRIVILEGE REQUEST IN DOMAIN FILE |
| 4024 | NOT READ PERMIT TO SEGMENT |
| 4025 | NO SUCH ND-100 SEGMENT NAME |

---

## Page 240

# 18.5.5 ND-500 TRAPS (7601B:7664B)

| Return error no. | Trap | Trap name                       |
|------------------|------|---------------------------------|
| 7601             | 1    | ESCAPE                          |
| 7602-7604        |      | Not assigned                    |
| 7605             | 5    | ZERO                            |
| 7606             | 6    | CARRY                           |
| 7607             | 7    | SIGN                            |
| 7610             | 8    | FLAG                            |
| 7611             | 9    | OVERFLOW                        |
| 7610             | 10   | not in use                      |
| 7613             | 11   | INVALID-OPERATION               |
| 7614             | 12   | DIVIDE-BY-ZERO                  |
| 7615             | 13   | FLOATING-UNDERFLOW              |
| 7616             | 14   | FLOATING-OVERFLOW               |
| 7617             | 15   | BCD-OVERFLOW                    |
| 7620             | 16   | ILLEGAL-OPERAND-VALUE           |
| 7621             | 17   | SINGLE-INSTRUCTION-TRAP         |
| 7622             | 18   | BRANCH-TRAP                     |
| 7623             | 19   | CALL-TRAP                       |
| 7624             | 20   | BREAK-POINT-INSTRUCTION-TRAP    |
| 7625             | 21   | ADDRESS-TRAP-FETCH              |
| 7626             | 22   | ADDRESS-TRAP-READ               |
| 7627             | 23   | ADDRESS-TRAP-WRITE              |
| 7630             | 24   | ADDRESS-ZERO-ACCESS             |
| 7631             | 25   | DESCRIPTOR-RANGE                |
| 7632             | 26   | ILLEGAL-INDEX                   |
| 7633             | 27   | STACK-OVERFLOW                  |
| 7634             | 28   | STACK-UNDERFLOW                 |
| 7635             | 29   | PROGRAMMED-TRAP                 |
| 7636             | 30   | DISABLE-PROCESS-SWITCH-TIMEOUT  |
| 7637             | 31   | DISABLE-PROCESS-SWITCH-ERROR    |
| 7640             | 32   | INDEX-SCALING-ERROR             |
| 7641             | 33   | ILLEGAL-INSTRUCTION-CODE        |
| 7642             | 34   | ILLEGAL-OPERAND-SPECIFIER       |
| 7643             | 35   | INSTRUCTION-SEQUENCE-ERROR      |
| 7644             | 36   | PROTECT-VIOLATION               |
| 7645             | 37   | TRAP-HANDLER-MISSING            |
| 7646             | 38   | PAGE-FAULT                      |
| 7647             | 39   | POWER-FAULT                     |
| 7650             | 40   | PROCESSOR-FAULT                 |
| 7651             | 41   | HARDWARE-FAULT                  |
| 7652-7663        |      | Not assigned                    |
| 7664             | 52   | ESCAPE-IN-MONITOR-CALL          |

---

## Page 241

# 19. ERS/SINTRAN III WATCHDOG

## 19.1 GENERAL DESCRIPTION

The Watchdog is an RT-program which receives error information written to internal device number 277a and converts this information to error reports printed on the error device.

If the FTX Error Logger (the RT-program FTXWD) is run on the system, the ERS/SINTRAN III Watchdog can not be used.

When the ERS/SINTRAN III Watchdog is started (@RT ERS3WD), the ordinary SINTRAN III error program should be stopped (@ABORT RTERR). Note that when the standard error program RTERR is stopped, error messages will no longer be available from the SINTRAN III error log (handled by the commands @INITIALIZE-ERROR-LOG and @PRINT-ERROR-LOG).

The ND-500 System Monitor now uses the ERS/SINTRAN III Watchdog for reports of error conditions, but the watchdog will also give better error reports on SINTRAN III errors.

## 19.2 REPORT LAYOUT

The layout of a report from the ERS/SINTRAN III Watchdog is as follows:

    severity * SSI:EC * date time * RT-program.P-register * sysname.sysno
    product name
    event text
    description parameter
    description parameter
    :
    :
    :

| Term        | Description                                                                 |
|-------------|-----------------------------------------------------------------------------|
| severity    | The severity of the reported event (Info/Warning/Error/Fatal)               |
| SSI         | SSI code of the event                                                       |
| EC          | Event Code of the event                                                     |
| date        | The date when the event was read by the watchdog (on the form YYYY-MM-DD)  |
| time        | The time when the event was read by the watchdog (on the form HH:MM:SS)     |
| RT-program  | The name (or RT-description address) of the RT-program causing the event    |
| P-register  | The P-register (program counter) of this RT-program                         |
| sysname     | The system name (as defined in XMSG)                                        |
| sysno       | The system number (as defined in XMSG)                                      |
| product name| The product name                                                            |
| event text  | The event text (for example error message)                                  |
| description | A description of the following parameter value                              |
| parameter   | Parameter value                                                             |

---

## Page 242

# 20. NOTS - NET/ONE TERMINAL SERVER

## 20.1 GENERAL DESCRIPTION

The Net/One terminal server interfaces the ND-100 to Net/One local area network. Both incoming calls (from Net/One to the ND-100) and outgoing calls (ND-100 to the network) are supported. Primary usage will be from Net/One resources which can connect to the ND-100 and use it as if they were connected to a local terminal. The terminal server will also be used by the new ND spooling system (SPRINT).

## 20.2 TECHNICAL SPECIFICATIONS

Up to 8 controllers can be inserted into the ND-100.

Each controller provides communication on 32 "lines" which are connected to SINTRAN III logical device numbers. Two of these lines are reserved for future use.

A name is assigned to each controller for identification from the network.

A number of lines can be reserved for outgoing calls (spooling).

## 20.3 FUNCTIONALITY

### 20.3.1 SERVICE FUNCTIONS

- Set configuration parameters of a NOTS (MON IOMTY Save/Image).
  - Set the name of a NOTS. This function is used to allocate a name to a NOTS controller which can be recognized from the network. The name is used by Net/One resources who want to connect to the ND-100. Several controllers can be assigned the same name.
  - Set number of outgoing lines for a NOTS (MON IOMTY Save/Image). This function is used when some of the NOTS lines are going to be used for outgoing calls. These lines cannot be used for incoming calls.

- Get configuration parameters of a NOTS (MON IOMTY Save/Image). The name and number of lines reserved for outgoing calls (the same parameters as mentioned above), can be inspected.

- Restart or reload a NOTS. This function is used to restart or reload a NOTS.

Norsk Data ND-60.230.5 EN

---

## Page 243

# SINTRAN III RELEASE INFORMATION, K-VERSION

**NOTS - NET/ONE TERMINAL SERVER**

## 20.3.2 USER FUNCTIONS

- Get information about a NOTS line (MON IOMTY Resident).  
  This function returns logical device number and attributes for a NOTS line.

- Set up connection to a NIU (MON IOMTY)  
  This function is used to set up connection from the ND-100 to a Net/One resource.

- Disconnect a NIU (MON IOMTY).

- Incoming calls.  
  Net/One resources can connect to the ND-100 and log in as if they were local terminals.

## 20.4 REQUIREMENTS

SINTRAN III/VSX version K or later.

Note that if the Net/One terminal server is going to be used together with synchronous modem interfaces, it may be necessary to reduce line speed for the modems to avoid too many retries. With a line speed of 4800 baud or less, there should not be any problems.

## 20.5 NOTS CONNECTIONS - SPOOLING

| ND-100            |
|-------------------|
| ND                |
| SPOOL             |
|                   |
| SVCLIB            |
| IOMTY             |

| (8 lines) | NIU 180 |

| NOTS              |
|-------------------|
| ONLY              |
| NON-CI            |
| WINDOWS           |

Norsk Data ND-60.230.5 EN

---

## Page 244

# SINTRAN III RELEASE INFORMATION, K-VERSION
## NOTS - NET/ONE TERMINAL SERVER

### 20.6 NOTS Connections - Terminals

| NOTS | | ND-1000 |
|------|---|--------|
| 0    | ![ ] | TERM. DATA-FIELD |
| CI WINDOW | ![ ] | |
| | | NOT USED |
| 1    | ![ ] | TERM. DATA-FIELD |
| CI WINDOW | ![ ] | |
| | | ----- |
| 2    | ![ ] | TERM. DATA-FIELD |
| NON-CI WINDOW | ![ ] | PROCESS |
| 3    | ![ ] | TERM. DATA-FIELD |
| NON-CI WINDOW | ![ ] | PROCESS |

```
SINTRAN DRIVER

STANDARD MON CALLS
```

| 31   | ![ ] | TERM. DATA-FIELD |
| NON-CI WINDOW | ![ ] | PROCESS |

Norsk Data ND-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 245

# SINTRAN III RELEASE INFORMATION, K-VERSION

## NOTES - NET/ONE TERMINAL SERVER

### 20.6.1 NOTS DATA FIELDS

Locations with new interpretations for NOTS terminals are marked with an asterisk (*).

**INPUT DATA FIELD LAYOUT IN SINTRAN MEMORY AREA, PART OUTSIDE DPIT:**

| Offset | Field   | Description                                        |
|--------|---------|----------------------------------------------------|
| -45    | TINFO   | Various information bits                           |
| -44    | PECH7   | Echo table 7                                       |
| -34    | PBRK7   | Break table 7                                      |
| -24    | INSMSG  | Address of ND-500 message when doing quick instring|
| -23    | RSISTE  | Echo pointer                                       |
| -22    | BRECHOFL| Break & echo flag                                  |
| -21    | ROUSPEC | Address of special subroutine                      |
| -20    | NCBRK   | Number of characters after last break              |
| -17    | CTTYP   | Terminal type                                      |
| -16    | CESC    | Disconnect and escape characters                   |
| -15    | BRKMAX  | Maximum BHOLD before break                         |
| -14    | MNTMFL *| Timer routine parameter                            |
| -13    | MNGET  *| Get pointer                                        |
| -12    | DFLAG   | Device flag bits                                   |
| -11    | ECHOTAB | Pointer to echo table                              |
| -10    | BRKTAB  | Pointer to break table                             |
| -7     | LAST    | Last typed character                               |
| -6     | TMSUB   | Timeout subroutine                                 |
| -5     | TMR     | Timeout counter                                    |
| -4     | TTMR    | Start value of TMR                                 |
| -3     | HDEV    | Hardware device number                             |
| -2     | STDRIV  | Driver start address                               |
| -1     | DRIVER  | Driver interrupt restart address                   |
| 0      | TDADDR  | Address of data field in resident                  |
| 1      | XDFOPP  | Address of DFOPP in resident                       |
| 2      | XOPPDF  | Address of opposite data field (outside resident)  |
| 3      | TYPRING | Device type bits and ring                          |
| 4      | MNWAD  *| Address of window                                  |
| 5      | MNCURB *| Current buffer pointer                             |
| 6      | MNCDF  *| Address of controller data field                   |

*To be continued*

---

## Page 246

# SINTRAN III RELEASE INFORMATION, K-VERSION
## NOTES - NET/ONE TERMINAL SERVER

| Number | Code     | Description                                                        |
|--------|----------|--------------------------------------------------------------------|
| 7      | IOTRANS  | Called from INBIT/OUTBT to transfer                                |
| 10     | STDEV    | Start device routine                                               |
| 11     | SETDV    | IOSET routine                                                      |
| 12     | DFOPP    | Pointer to output channel data field                               |
| 13     | DERROR   | Error code                                                         |
| 14     | BUFST    | Start of ring buffer                                               |
| 15     | MAX      | Buffer capacity                                                    |
| 16     | BHOLD    | Number of characters in buffer                                     |
| 17     | HENTE    | Fetch pointer                                                      |
| 20     | CFREE    | Free positions                                                     |
| 21     | FYLLE    | Store pointer                                                      |
| 22     | BSTATE   | Background program state                                           |
| 23     | TSTATE   | Time slice state                                                   |
| 24     | DBPROG   | Background RT-program                                              |
| 25     | DBADDR   | Saved P-reg on escape and file system monitor calls                |
| 26     | RIFIL    | For mode input file number                                         |
| 27     | BCHISTS  | For mode input status                                              |
| 30     | DERO     | Error information                                                  |
| 31     | BREGBLOCK| Register save at escape                                            |
| 32     | DER2     | Error information                                                  |
| 40     | DBPREG   | P-register on page fault on IOBT level                             |
| 41     | DBACTPRI | ACTPRI on page fault on IOBT level                                 |
| 42     | FLAGB    | Background flags                                                   |
| 43     | EUSADD   | Address for user-escape handling                                   |
| 44     | LUSADD   | Address for user local-function handling                           |
| 45     | NBREAKS  | Number of break characters in buffer                               |
| 46     | CMWFIELD | Address of current monitor call working field                      |
| 47     | UACTPRI  | PCR-register when accessing caller's buffer                        |
| 50     | USADDR   | Address of caller's buffer                                         |
| 51     | XBUFST   | Logical window address to ring buffer                              |
| 52     | NCHARS   | Number of characters stored in caller's buffer                     |
| 53     | CPIENTRY | PIT-entry of terminal data field                                   |
| 55     | BRKCHAR  | Break character                                                    |
| 56     | DRKMODE  | Break mode                                                         |

---

## Page 247

# SINTRAN III RELEASE INFORMATION, K-VERSION

NOTS - NET/ONE TERMINAL SERVER

## OUTPUT DATA FIELD LAYOUT IN SINTRAN MEMORY AREA, PART OUTSIDE DPIT

| Address | Description |
|---------|-------------|
| -10     | SCREEN Counter for stop on full page |
| -7      | EMPTFLAG Buffer empty flag |
| -6      | TMSUB Time out subroutine |
| -5      | TMR Time out counter |
| -4      | TTMR Start value of TMR |
| -3      | HDEV Hardware device number |
| -2      | STDRIV Driver start address |
| -1      | DRIVER Driver interrupt restart address |
| 0       | TRDADDR Address of data field in resident |
| 1       | XDFOPP Address of DFOPP in resident |
| 2       | XOPPDF Value to add to current data field address |
| 3       | TYPRING Device type bits and ring |
| 4       | MNFILL * Fill pointer (absolute address) |
| 5       | MNCURB * Current buffer pointer |
| 6       | MNWBNK * Bank number for window |
| 7       | IOTRANS Called from INBT/OUTBT to transfer |
| 10      | STDEV Start device |
| 11      | SETDV IOSET routine |
| 12      | DFOPP Pointer to output channel data field |
| 13      | DERORR Error code |
| 14      | BUFST Start of ring buffer |
| 15      | MAX Buffer capacity |
| 16      | BHOLD Number of characters in buffer |
| 17      | HENTE Fetch pointer |
| 20      | CFREE Free positions |
| 21      | FYLLE Store pointer |
| 22      | MINBHOLD Lower limit for break |
| 23      | ROFIL For "mode" (output file number) |
| 24      | BCHOST For "mode" (output status) |
| 25      | OSNMSG Address for ND-500 message |
| 26      | CBUADR Current user buffer address (outstring) |
| 27      | NOCHAR Number of bytes in outstring monitor call |
| 30      | CNOCHAR Number of words left to transfer in outstring |
| 31      | XNOCHAR Working location for outstring |
| 32      | ZOPRG P, X, T-registers in outstring |
| 35      | ZOARG A, D and L-registers in outstring |
| 40      | ZOSRG S, B-registers + old page in outstring |
| 43      | SBHOLD Saved BHOLD in outstring |

---

## Page 248

# NOTES CONTROLLER DATA FIELD

| Code   | Description                                           |
|--------|-------------------------------------------------------|
| -6     | TMSUB  | Time out subroutine                          |
| -5     | TMR    | Time out counter                             |
| -4     | TTMR   | Start value of TMR                           |
| -3     | HDEV   | Hardware device number                       |
| -2     | STDRIV | Driver start address                         |
| -1     | DRIVER | Driver interrupt restart address             |
| 0      |        | Not used                                     |
| 1      |        | Not used                                     |
| 2      |        | Not used                                     |
| 3      | TYPRING| Device type bits and ring                    |
| 4      | MNCFL  | Control flag bits                            |
| 5      | MNOUT  | Number of lines reserved for outgoing calls  |
| 6      | MNCBNK | Bank number for control area                 |
| 7      | MNCNTREG| Control register                            |
| 10     | MNNAP  | Pointer to NOTS name string                  |
| 11     | MNLNG  | Length of NOTS name                          |
| 12     | MNIDF  | Pointer to input data field address table    |
| 13     | MNODF  | Pointer to output data field address table   |

---

## Page 249

# 21. MTAD - MAILBOX TERMINAL ACCESS DEVICE

## 21.1 GENERAL DESCRIPTION

MTAD is a standard way of interfacing a background process in the ND-100 from another process (client) running in a CPU sharing memory with the ND-100 (the client can also be running in the ND-100). MTAD provides such functions as connect/disconnect data field, byte in/byte out.

## 21.2 PRINCIPLES OF OPERATION

### 21.2.1 THE MAILBOX

This is an area of one page in physical memory, which can be accessed both by the ND-100 and the client. It contains input and output ring buffers and some control information. The layout of this mailbox is hidden for the client, which accesses it only by library routines. If the client is running in the ND-100, this area is connected as LAMU (Logical Addressed Memory Unit). The client is responsible for reserving the area necessary for the mailbox.

### 21.2.2 INITIATION

After reserving a mailbox, the client can connect it to a MTAD data field. This is done by a library routine which interrupts the ND-100 MTAD driver. The driver will search for a free MTAD data field (terminal data field) and connect it to the mailbox.

```
312 
+
406
```

### 21.2.3 DATA TRANSFER

After connecting the mailbox to a MTAD data field, the client can start data transfer by writing characters in the mailbox input buffer and reading characters from the mailbox output buffer. The library routines for doing this will automatically interrupt the ND-100 driver when necessary. The ND-100 will restart the client when it is necessary (and possible).

### 21.2.4 DISCONNECTION

When the client has finished using the MTAD, the mailbox should be disconnected from the data field. The ND-100 can also sometimes disconnect the data field from the mailbox (disconnect on logout). The client will discover this by the status returned from the next call to the mailbox routines.

Norsk Data ND-60.230.5 EN
Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 250

# SINTRAN III RELEASE INFORMATION, K-VERSION
MTAD - MAILBOX TERMINAL ACCESS DEVICE

## 21.3 MTAD OVERVIEW

| Client   | Mailbox   | Terminal data field | Background program |
|----------|-----------|---------------------|--------------------|
| LIBRARY  | CONTR.    | DRIVER              | MON CALLS          |

## 21.4 MTAD INTERFACING FROM THE ND-100 - GENERAL

MTAD devices can be accessed both from background and foreground programs. Due to the restrictions on the function 'create LAMU', users SYSTEM or RT is required. If errors occur during execution of monitor calls in the MTAD library, an error return with the SINTRAN error number in the ERRCODE variable will occur.

Always remember to have a logical address area large enough for the mailboxes.

| PROGRAM | DATA |
|---------|------|
|         |      |

← Mailbox area (connected to LAMU).

## 21.5 NUMBER OF MTADS IN THE SYSTEM

The number of MTADs in the system is limited by the number of terminal data fields which are generated. The data fields are first allocated for the physical terminal connections, then for the NOTS controller (Net/One Terminal Server), and the rest is allocated for MTADs. There is also a variable in SINTRAN which limits the maximum number of MTADs. To change this variable (default is 40₈), do the following:

```
@SINTRAN-SERVICE-PROGRAM
*CHANGE-VARIABLE MTDMAX <new octal value> Y Y
*EXIT
```

Norsk Data ND-60.230.5 EN

---

## Page 251

# SINTRAN III RELEASE INFORMATION, K-VERSION
MTAD - MAILBOX TERMINAL ACCESS DEVICE

## 21.6 MTAD LIBRARY ROUTINES FOR THE ND-100

The MTAD programming library (ND-250222) provides the functions necessary to use MTADs. The library routines use the monitor call MTAD (MON 345). The different routines are described below.

### 21.6.1 MTRESMB

Reserves physical memory for the mailboxes. The physical memory will be mapped in the user's logical address area (by MON MLAMU). Each memory page will contain 4 mailboxes.

**Parameters:**

- **Input:**
  - Logical address for the mailbox area.
  - Should start on a page boundary.
  - Number of mailboxes wanted.

- **Output:**
  - Array containing mailbox identifiers for each mailbox.
  - Status (0 = OK)

**PLANC syntax:**

```
CONSTANT maxmb=32

TYPE resrec = RECORD
  INTEGER : mbaddr
  INTEGER : nomb
  INTEGER ARRAY : mbid(0:maxmb-1)
ENDRECORD

IMPORT (ROUTINE (resrec POINTER,INTEGER) : MtResMb)

resrec : reserve
INTEGER : status

Addr(reserve) MtResMb:=.status
```

### 21.6.2 MTRELMB

Releases the physical memory for the mailboxes. The same record parameter as for MtResMb can be used.

**Parameters:**

- **Input:**
  - Logical address for the mailbox area.
  - Number of mailboxes used.

- **Output:**
  - Array containing mailbox identifiers for each mailbox.
  - Status (0 = OK)

**PLANC syntax:**

```
CONSTANT maxmb=32

TYPE resrec = RECORD
  INTEGER : mbaddr
  INTEGER : nomb
  INTEGER ARRAY : mbid(0:maxmb-1)
ENDRECORD
```

---

## Page 252

# SINTRAN III RELEASE INFORMATION, K-VERSION
## MTAD - MAILBOX TERMINAL ACCESS DEVICE

IMPORT (ROUTINE (resrec POINTER, INTEGER) : MtRelMb)

resrec : reserve  
INTEGER : status

Addr(reserve) MtRelMb=:status

### 21.6.3 MTCNCT

Set up a connection to a MTAD data field through a mailbox.

**Parameters:**  
Input:  
- Mailbox identifier.  
- Flag:  
  - Bit 0: If set, then prepare for 8 bits I/O.  
  - Bits 1-17: Reserved for future use, should be zero.  
Output:  
- Status:  
  - 0: OK.  
  - 1: No free MTAD data fields.

#### PLANC syntax:
CONSTANT maxmb=32

TYPE resrec = RECORD  
INTEGER : mbaddr  
INTEGER : nomb  
INTEGER ARRAY : mbid(0:maxmb-1)  
ENDRECORD

TYPE conrec = RECORD  
INTEGER : mbid  
INTEGER : flag  
ENDRECORD

IMPORT (ROUTINE (conrec POINTER, INTEGER) : MtCnct)

resrec : reserve  
conrec : connect  
INTEGER : status

reserve.mbid(0)=:connect.mbid; 0=:connect.flag  

Addr(connect) MtCnct=:status

### 21.6.4 MTDCNCT

Close the connection to a MTAD data field through a mailbox.

**Parameters:**  
Input:  
- Mailbox identifier.  
Output:  
- Status (0 = OK)

---

## Page 253

# SINTRAN III RELEASE INFORMATION, K-VERSION
## MTAD - MAILBOX TERMINAL ACCESS DEVICE

### PLANC Syntax:
```plaintext
CONSTANT maxmb=32

TYPE resrec = RECORD
    INTEGER      : mbaddr
    INTEGER      : nomb
    INTEGER ARRAY : mbid(0:maxmb-1)
ENDRECORD

IMPORT (ROUTINE (INTEGER,INTEGER) : MtDcnct)

resrec : reserve
INTEGER : status

reserve.mbid(0) MtDcnct=:status
```

### 21.6.5 MTPUT

Give input to a MTAD data field. The input string will be moved to the mailbox input buffer. The user will always return from this call, even if there is not enough space in the mailbox buffer to receive the whole string.

**Parameters:**

- **Input:**
  - Mailbox identifier.
  - Pointer to input string.
  - Number of bytes to write.
- **Output:**
  - Number of bytes written.
  - Status (0 = OK)

### PLANC Syntax:
```plaintext
TYPE putrec = RECORD
    INTEGER        : mbid
    BYTES POINTER  : pinstring
    INTEGER        : notowrite
    INTEGER        : nowritten
ENDRECORD

IMPORT (ROUTINE (putrec POINTER,INTEGER) : MtPut)

putrec : putarea
BYTES  : instring
INTEGER: status

Addr(instring)=:putarea.pinstring
Addr(putarea) MtPut=:status
```

### 21.6.6 MTGET

Receive output from a MTAD data field. The bytes in the mailbox output buffer will be moved to the string specified by the user. The user will always return from this call, even if there are not enough characters in the mailbox buffer to fill the string.

---

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 254

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## MTAD - MAILBOX TERMINAL ACCESS DEVICE

### Parameters:

**Input**:  
- Mailbox identifier.  
- Pointer to output string.  
- Maximum number of bytes to read.  

**Output**:  
- Number of bytes read.  
- Status (0 = OK)  

### PLANC syntax:

```
TYPE getrec = RECORD
  INTEGER         : mbid
  BYTES POINTER   : poutstring
  INTEGER         : notoread
  INTEGER         : noread
ENDRECORD

IMPORT (ROUTINE (getrec POINTER,INTEGER) : MtGet)

getrec : getarea
BYTES  : outstring
INTEGER : status

Addr(outstring)=:getarea.poutstring  
Addr(putarea) MtPut::status  
```

### 21.6.7 MTGSID

Get UE (User Environment) session identifier and logical device number from mailbox. Logical device number will be available immediately after connect, while UESID will contain zeroes until UE-LOGIN is performed.

### Parameters:

**Input**:  
- Mailbox identifier.  

**Output**:  
- Array containing UE session identifier.  
- Logical device number of MTAD data field.  
- Status (0 = OK)  

### PLANC syntax:

```
TYPE sidrec = RECORD
  INTEGER         : mbid
  INTEGER ARRAY   : uesid(0:3)
  INTEGER         : logno
ENDRECORD

IMPORT (ROUTINE (sidrec POINTER,INTEGER) : MtGSID)

sidrec : sidarea
INTEGER : status

Addr(sidarea) MtGSID::status
```

Norsk Data ND-60.220.5 EN

---

## Page 255

# SINTRAN III RELEASE INFORMATION, K-VERSION
### MTAD - MAILBOX TERMINAL ACCESS DEVICE

## 21.7 MTAD DATA FIELDS

Locations with new interpretations for MTADs are marked with an asterisk (*).

**INPUT DATA FIELD LAYOUT IN SINTRAN MEMORY AREA, PART OUTSIDE DPIT:**

| Offset | Field   | Description                                              |
|--------|---------|----------------------------------------------------------|
| -45    | TINFO   | Various information bits                                 |
| -44    | PECH7   | Echo table 7                                             |
| -34    | PBRK7   | Break table 7                                            |
| -24    | INSMSQ  | Address of ND-500 message when doing quick instring      |
| -23    | RSISITE | Echo pointer                                             |
| -22    | BRECHFL | Break & echo flag                                        |
| -21    | ROUSPEC | Address of special subroutine                            |
| -20    | NCBRK   | Number of characters after last break                    |
| -17    | CTYP    | Terminal type                                            |
| -16    | CESCp   | Disconnect and escape characters                         |
| -15    | BRKMAX  | Maximum BHOLD before break                               |
| -14    | MTDFLI *| Link to next free data field (-1 = end of list)          |
| -13    | MTRTP * | RT-description address of client                         |
| -12    | DFLAG   | Device flag bits                                         |
| -11    | ECHOTAB | Pointer to echo table                                    |
| -10    | BRKTAB  | Pointer to break table                                   |
| -7     | LAST    | Last typed character                                     |
| -6     | TMSUB   | Time out subroutine                                      |
| -5     | TMR     | Time out counter                                         |
| -4     | TTMER   | Start value of TMR                                       |
| -3     | HDEV    | Hardware device number                                   |
| -2     | STORIV  | Driver start address                                     |
| -1     | DRIVER  | Driver interrupt restart address                         |
| 0      | TRADDR  | Address of data field in resident                        |
| 1      | XOFOPP  | Address of DFOPP in resident                             |
| 2      | XOPPDF  | Address of opposite data field (outside resident)        |
| 3      | TYPRING | Device type bits and ring                                |
| 4      | MTMBAD *| Mailbox address                                          |
| 5      | MTGET * | Address of get routine                                   |
| 6      | MTFLAG *| Timer flag                                               |

To be continued

Norsk Data ND-60.230.5 EN

---

## Page 256

# SINTRAN III RELEASE INFORMATION, K-VERSION
## MTAD - Mailbox Terminal Access Device

|   |   |                            |
|---|---|----------------------------|
| 7 | IOTRANS   | Called from INBIT/OUTBT to transfer      |
| 10| STDEV     | Start device routine                     |
| 11| SETDV     | IOSET routine                            |
| 12| DFOPP     | Pointer to output channel data field     |
| 13| DERROR    | Error code                               |
| 14| BUFST     | Start of ring buffer                     |
| 15| MAX       | Buffer capacity                          |
| 16| BHOLD     | Number of characters in buffer           |
| 17| HENTE     | Fetch pointer                            |
| 20| CFREE     | Free positions                           |
| 21| FYLLE     | Store pointer                            |
| 22| BSTATE    | Background program state                 |
| 23| TSTATE    | Time slice state                         |
| 24| DBPROG    | Background RT-program                    |
| 25| DBADR     | Saved P-reg on escape and file system monitor calls |
| 26| RIFIL     | For mode input file number               |
| 27| BCHISTS   | For mode input status                    |
| 312| 30| DERO       | Error information                        |
| +| 31| BREGBLOCK | Register save at escape                 |
| 406| 32| DER2      | Error information                        |
| 40| DBPREG    | P-register on page fault on IOBT level   |
| 41| DBACTPRI  | ACTPRI on page fault on IOBT level       |
| 42| FLAGB     | Background flags                         |
| 43| EUSADD    | Address for user-escape handling         |
| 44| LUSADD    | Address for user local-function handling |
| 45| NBREAKS   | Number of break characters in buffer     |
| 46| CMWFIELD  | Address of current monitor call working field |
| 47| UACTPRI   | PCR-register when accessing caller's buffer   |
| 50| USADDR    | Address of caller's buffer               |
| 51| XBUFST    | Logical window address to ring buffer    |
| 52| NCHARS    | Number of characters stored in caller's buffer   |
| 53| CPITENTRY | PIT-entry of terminal data field         |
| 55| BRKCHAR   | Break character                          |
| 56| BRKMODE   | Break mode                               |
| 57| MTLGAD*   | Logical user address of mailbox          |
| 60| MTMBDISP* | Displacement within mailbox page         |

---

## Page 257

# SINTRAN III RELEASE INFORMATION, K-VERSION

## MTAD - MAILBOX TERMINAL ACCESS DEVICE

### OUTPUT DATA FIELD LAYOUT IN SINTRAN MEMORY AREA, PART OUTSIDE DPIT

| Address | Field       | Description                                       |
|---------|-------------|---------------------------------------------------|
| -10     | SCREEN      | Counter for stop on full page                     |
| -7      | EMPTFLAG    | Buffer empty flag                                 |
| -6      | TMSUB       | Time out subroutine                               |
| -5      | TMR         | Time out counter                                  |
| -4      | TMR         | Start value of TMR                                |
| -3      | HDEV        | Hardware device number                            |
| -2      | STORIV      | Driver start address                              |
| -1      | DRIVER      | Driver interrupt restart address                  |
| 0       | TROADDR     | Address of data field in resident                 |
| 1       | XDFOPP      | Address of DFOPP in resident                      |
| 2       | XOPPDF      | Value to add to current data field address        |
| 3       | TYPRING     | Device type bits and ring                         |
| 4       | MTMBAD *    | Mailbox address                                   |
| 5       | MTPUT *     | Address of put routine                            |
| 6       | MTACT. *    | Address of routine to restart client              |
| 7       | IOTRANS     | Called from INBT/OUTBT to transfer                |
| 10      | STDEV       | Start device                                      |
| 11      | SETDV       | IOSET routine                                     |
| 12      | DFOPP       | Pointer to output channel data field              |
| 13      | DRERROR     | Error code                                        |
| 14      | BUFS1       | Start of ring buffer                              |
| 15      | MAX         | Buffer capacity                                   |
| 16      | BHOLD       | Number of characters in buffer                    |
| 17      | HENTE       | Fetch pointer                                     |
| 20      | CFREE       | Free positions                                    |
| 21      | FYLLE       | Store pointer                                     |
| 22      | MMINBHOLD   | Lower limit for break                             |
| 23      | ROFIL       | For "mode" (output file number)                   |
| 24      | BCHOST      | For "mode" (output status)                        |
| 25      | OSMSG       | Address for ND-500 message                        |
| 26      | CBUADR      | Current user buffer address (outstring)           |
| 27      | NOCHAR      | Number of bytes in outstring monitor call         |
| 30      | CNOCHAR     | Number of words left to transfer in outstring     |
| 31      | XNOCHAR     | Working location for outstring                    |
| 32      | ZOPRG       | P, X, T-registers in outstring                    |
| 35      | ZOARG       | A, D and L-registers in outstring                 |
| 40      | ZOSRG       | S, B-registers + old page in outstring            |
| 43      | SBHOLD      | Saved BHOLD in outstring                          |
| 44      | MTSFPT *    | Address of fast ND-500 move routine               |
| 45      | MTIFPT *    | Address of fast ND-100 move routine               |

---

## Page 258

# 22. SCSI DEVICES

## 22.1 GENERAL DESCRIPTION

The Small Computer Systems Interface (SCSI) is a new ND-100 interface used for peripheral devices such as magnetic disk drives, streamer tape units, optical disk drives and magnetic tape units. Devices are connected to a SCSI adaptor by way of an 8-bit parallel bus. The bus has 8 ports (numbered 0-7) and up to 7 devices may be connected to each bus (in addition to the SCSI adaptor). This is illustrated below:

```
The ND-100 bus
┌─────────────────────┐
│    SCSI adaptor     │
├─────────────────────┤
│ 7                   │
└─────────────────────┘
├───────────────────────────────────────────────────────────┤
│                   8-bit parallel bus                      │
├───────────────────────────────────────────────────────────┤
0            1        2           3            6
┌─────────┐ ┌────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐
│disk     │ │streamer│ │magnetic │ │disk     │ │ Maximum │ │disk     │
│unit     │ │tape    │ │tape     │ │unit     │ │ .. 7    │ │unit     │
└─────────┘ └────────┘ └─────────┘ └─────────┘ └─────────┘
```

## 22.2 SCSI DEVICES - DEVICE TYPES AND DEVICE NAMES

Presently, the following devices are supported:

- 60-megabyte magnetic disk drive. The disk drive is a 5 1/4 inch fixed drive (not removable).
- 125-megabyte magnetic disk drive. The disk drive is a 5 1/4 inch fixed drive (not removable).
- 150-megabyte magnetic disk drive. The disk drive is a 5 1/4 inch fixed drive (not removable).
- 310-megabyte magnetic disk drive. The disk drive is an 8-inch fixed drive (not removable).
- 630-megabyte magnetic disk drive. The disk drive is an 8-inch fixed drive (not removable).

All SCSI magnetic disk units may be divided into 2, 3, 4, 5, 6 or 8 subunits.

- 1-gigabyte optical disk drive (Laserdrive 1200).  
  May be divided into 2, 3, 4, 5, 6, 7 or 8 subunits.

- 125-megabyte streamer tape drive.

- STC magnetic tape drive.

---

## Page 259

# SINTRAN III RELEASE INFORMATION, K-VERSION

## SCSI DEVICES

An ND-100 system can have a maximum of:

- 4 SCSI adaptors
- 14 SCSI magnetic disk drives
- 4 SCSI optical disk drives
- 2 SCSI streamer tape drives
- 4 SCSI magnetic tape drives

SINTRAN III device names for SCSI devices are as follows (the numbers refer to the drive number in the total configuration):

- **Magnetic disk drives:**

  |             |             |
  |-------------|-------------|
  | DISC-SCSI-1 | DISC-SCSI-8 |
  | DISC-SCSI-2 | DISC-SCSI-9 |
  | DISC-SCSI-3 | DISC-SCSI-10|
  | DISC-SCSI-4 | DISC-SCSI-11|
  | DISC-SCSI-5 | DISC-SCSI-12|
  | DISC-SCSI-6 | DISC-SCSI-13|
  | DISC-SCSI-7 | DISC-SCSI-14|

- **Optical disk drives:**

  |                  |                  |
  |------------------|------------------|
  | DISC-OPTICAL-1   | DISC-OPTICAL-2   |
  | DISC-OPTICAL-3   | DISC-OPTICAL-4   |

- **Streamer tape units:**

  |             |             |
  |-------------|-------------|
  | STREAMER-1  | STREAMER-2  |

- **Magnetic tape units:**

  |            |            |
  |------------|------------|
  | MAG-TAPE-1 | MAG-TAPE-2 |
  | MAG-TAPE-3 | MAG-TAPE-4 |

For subdivided disk units, the device name is expanded (in the same way as for SMD disks) with the number of subunits included in the name, for example:

- **DISC-2-SCSI-3**: means that magnetic disk drive number 3 in the SCSI configuration is divided into 2 subunits.
- **DISC-8-OPTICAL-1**: means that optical disk drive number 1 in the SCSI configuration is divided into 8 subunits.

### 22.3 SCSI DEVICES AS SINTRAN III DEVICES

All devices connected to a SCSI adaptor are given a SINTRAN III device number as follows (SINTRAN III device names given in parentheses):

| SCSI adaptor number 1 | 2202<sub>8</sub> |
|-----------------------|------------------|
| SCSI adaptor number 2 | 2203<sub>8</sub> |
| SCSI adaptor number 3 | 2204<sub>8</sub> |
| SCSI adaptor number 4 | 2205<sub>8</sub> |

| SCSI streamer tape drive no. 1 (STREAMER-1) | 2206<sub>8</sub> |
|--------------------------------------------|------------------|
| SCSI streamer tape drive no. 2 (STREAMER-2) | 2207<sub>8</sub> |

| SCSI disk number 1 (DISC-SCSI-1) | 2210<sub>8</sub> |
|---------------------------------|------------------|
| SCSI disk number 2 (DISC-SCSI-2) | 2211<sub>8</sub> |
| SCSI disk number 3 (DISC-SCSI-3) | 2212<sub>8</sub> |
| SCSI disk number 4 (DISC-SCSI-4) | 2213<sub>8</sub> |
| SCSI disk number 5 (DISC-SCSI-5) | 2214<sub>8</sub> |
| SCSI disk number 6 (DISC-SCSI-6) | 2215<sub>8</sub> |
| SCSI disk number 7 (DISC-SCSI-7) | 2216<sub>8</sub> |

---

## Page 260

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## SCSI DEVICES

| SCSI Disk Number | Device Name | Device Number |
|------------------|-------------|---------------|
| 8                | (DISC-SCSI-8)  | 2217₈         |
| 9                | (DISC-SCSI-9)  | 2220₈         |
| 10               | (DISC-SCSI-10) | 2221₈         |
| 11               | (DISC-SCSI-11) | 2222₈         |
| 12               | (DISC-SCSI-12) | 2223₈         |
| 13               | (DISC-SCSI-13) | 2224₈         |
| 14               | (DISC-SCSI-14) | 2225₈         |

| SCSI Magnetic Tape Number | Device Name | Device Number |
|---------------------------|-------------|---------------|
| 1                         | (MAG-TAPE-1) | 560₈          |
| 2                         | (MAG-TAPE-2) | 1111₈         |
| 3                         | (MAG-TAPE-3) | 1231₈         |
| 4                         | (MAG-TAPE-4) | 1224₈         |

| SCSI Optical Disk Number | Device Name  | Device Number |
|--------------------------|--------------|---------------|
| 1                        | (DISC-OPTICAL-1) | 2232₈     |
| 2                        | (DISC-OPTICAL-2) | 2233₈     |
| 3                        | (DISC-OPTICAL-3) | 2234₈     |
| 4                        | (DISC-OPTICAL-4) | 2235₈     |

## 22.4 CONFIGURATION AND OPERATION

The logical connection between a physical device connected to a port on a SCSI bus and the device name (and number) in SINTRAN III is done in the SINTRAN III configuration program, see pages 109-110 for details. The basic functions used for establishing, reporting and removing such connections are functions 300₈ - 302₈ in MON IOMTY (see pages 52-53 for details).

### The following limitations/restrictions apply:

- The SCSI adaptor should be connected as ID no. 7 on the SCSI bus.

- If the main swapping device (the system disk) is a SCSI disk, it should be connected as ID number 0 on SCSI adaptor no. 1 (as "the first disk" in the configuration).

- If the configuration contains streamer tape drives, the (first) streamer tape drive (STREAMER-1) should be connected as ID number 1 on SCSI adaptor number 1.

- If the configuration contains magnetic tape drives, the (first) magnetic tape drive (MAG-TAPE-1) should be connected as ID number 2 on SCSI adaptor number 1.

- Recommended peripheral names and device numbers for SCSI magnetic tape units are:

| Magnetic Tape No. | Unit | Device Name | Device No. |
|-------------------|------|-------------|------------|
| 1                 | 0    | MAG-TAPE-0  | 40₈        |
| 2                 | 0    | MAG-TAPE-4  | 32₈        |
| 3                 | 0    | MAG-TAPE-8  | 1232₈      |
| 4                 | 0    | MAG-TAPE-12 | 1225₈      |

Note that the device numbers for magnetic tape no. 3 and 4 overlap ST-506 (Winchester) disk controller no. 2 and 1 respectively. This means that you cannot use more than 3 magnetic tape units if you have 1 ST-506 disk controller and maximum 2 magnetic tape units if you have 2 ST-506 controllers.

---

## Page 261

# SINTRAN III RELEASE INFORMATION, K-VERSION

## 22.5 MON ABSTR FUNCTIONS SUPPORTING SCSI STREAMER DEVICES

The following MON ABSTR (MON 131) functions now support SCSI streamer devices:

- * 0 Read
- * 1 Write
- 2 Read parity
- 3 Compare
- * 7 Erase tape
- * 10 Advance through EOF
- * 12 Write EOF
- * 13 Rewind
- * 16 Advance specified number of records
- * 17 Unload
- * 20 Read status
- * 24 Read last status
- * 30 Load tape
- * 31 Reset device
- * 34 Reserve unit
- * 35 Release unit
- 37 Read extended status
- * 42 Read format
- 54 Copy
- 60 Read with double amount
- 61 Write with double amount
- 62 Read parity with double amount
- 63 Compare with double amount
- * 70 Retension
- * 73 Test unit ready
- 74 Execute user specific command block
- 75 Inquiry
- * 76 Advance to end of recorded area

* : Function is also allowed from MON MAGTP (MON 144) and the SINTRAN III command @DEVICE-FUNCTION.

## 22.6 STATUS WORD FOR SCSI DEVICES

Basically, all SCSI devices use the same status word layout. However, magnetic disk and magnetic tape are specially treated to be compatible with old devices. For these devices a simulated status are returned to the calling program (from MON ABSTR). The new status are returned in error 22 and 24 since this gives more information. For streamer tape and optical disk, only the new status are used.  
The status word returned from MON ABSTR has the following layout:

| Bit | Description |
|-----|-------------|
| 0-3 | SCSI sense key (see below) |
| 4   | Error |
| 5   | ILI (illegal block size on tape) |
| 6   | EOM (End Of Media) |
| 7   | Filemark (EOF) |
| 10  | Return value valid |
| 11-16 | Driver error code (see below) |
| 17   | Reserved for future use |

---

## Page 262

# SINTRAN III RELEASE INFORMATION, K-VERSION
## SCSI DEVICES

The SCSI sense keys (bits 0-3) are listed below. See also the SCSI standard specification and vendors device documentation. Error (bit 4) is set for sense keys marked with asterisk (*).

| Sense Key | Description |
|-----------|-------------|
| 0 | NO SENSE. No additional status reported. |
| 1 | RECOVERED ERROR. Operation completed successfully with recovery |
| * 2 | NOT READY. Device not ready. May require operator intervention. |
| * 3 | MEDIUM ERROR. Nonrecovered error due to medium failure. |
| * 4 | HARDWARE ERROR. Nonrecoverable hardware failure on target device. |
| * 5 | ILLEGAL REQUEST. Attempted command was illegal or not implemented. |
| * 6 | UNIT ATTENTION. Change of medium or powerfail/reset on target occurred since last operation. Driver will remove this condition if possible. |
| * 7 | DATA PROTECT. Medium was read or write protected. |
| * 10a | BLANK CHECK. Attempt to read nonrecorded data. |
| * 11a | Vendor unique. |
| * 12a | COPY ABORTED. A device copy command has terminated. |
| * 13a | COMMAND ABORTED. Command aborted. May try again. |
| 14a | EQUAL. Match on search command. |
| * 15a | VOLUME OVERFLOW. Attempt to write past end of medium. |
| * 16a | MISCOMPARE. Miscompare on compare command. |
| * 17a | Reserved for future use. |

---

## Page 263

# SINTRAN III RELEASE INFORMATION, K-VERSION

## SCSI DEVICES

The driver error code (bits 11-16₈) reflects error conditions detected by the driver. If these bits are different from zero, bit 4 will always be set and no other status information is valid.

| Code | Description |
|------|-------------|
| 1    | Illegal SCSI device type |
| 2    | Illegal device record size |
| 3    | Function not implemented |
| 4    | Illegal ABSTR operation |
| 5    | Illegal ABSTR parameter |
| 6    | No such node (data field) |
| 7    | No such logical unit defined |
| 10₈  | Illegal command to bus driver |
| 11₈  | No control record on magnetic disk |
| 12₈  | Copy not possible |
| 20₈  | Unexpected disconnect |
| 21₈  | Error in memory address |
| 22₈  | Transfer error (SCSI parity or ND-100 bus error) |
| 23₈  | No scsi status byte received |
| 24₈  | Illegal status on request sense |
| 25₈  | Unrecognised SCSI status byte |
| 26₈  | Connection timeout |
| 27₈  | LUN busy |
| 30₈  | Reservation conflict |
| 31₈  | Illegal (not extended) sense |
| 32₈  | Error from disk in copy |
| 40₈  | Not able to select device (no answer) |
| 41₈  | Reconnect timeout (may be bus reset) |
| 42₈  | SCSI protocol error |
| 43₈  | SCSI bus reset received |
| 44₈  | Illegal SCSI bus phase |
| 45₈  | Attempt to transfer past bytecount |
| 46₈  | Messages not implemented |
| 47₈  | Arbitration software timeout |
| 50₈  | Unrecoverable ND-100 powerfail |
| 51₈  | Locally initiated reset |
| 52₈  | Timeout, abort not received by device |
| 60₈  | Error in bus control processor |
| 61₈  | Error in hardware selftest |
| 62₈  | Unexpected command error |
| 63₈  | Memory address register not as expected |

## 22.7 NEW ERROR MESSAGES FOR SCSI OPERATION

The following new error messages may appear on systems with SCSI devices:

| Code | Description |
|------|-------------|
| 3203 | DIRECTORY ENTERED BY ANOTHER SYSTEM |
| 3204 | INCOMPATIBLE DIRECTORY SIZES ON THIS CONTROLLER |
| 3205 | MEDIUM NOT LOADED (DEVICE RESET OR MEDIUM CHANGED) |
| 3206 | ILLEGAL REQUEST |
| 3207 | DEVICE BUSY |
| 3210 | DEVICE RESERVED BY ANOTHER SYSTEM |
| 3211 | NO RESPONSE FROM DEVICE |

---

## Page 264

# 22.8 Defining Directory Size on a SCSI Disk

When a mass storage unit is defined, either explicitly by the command `@DEFINE-MASS-STORAGE-UNIT`, or implicitly by `@ENTER-DIRECTORY`, `@CREATE-DIRECTORY`, etc. the directory size is set for all units using that device name. For SMD and Winchester disks, the directory size is contained in the device name (for example DISC-70MB-1, DISC-74MB, DISC-2-225MB-1-F). On SCSI disks, the directory size is set to the total disk capacity divided by the number of subunits on the device. This computation will in most cases result in a variety of different directory sizes, especially on a system with a mixture of different SMD and SCSI disks.

The following command can be used to change directory size:

```
@SET-MASS-STORAGE-SIZE
  with parameters: DEVICE NAME:
                   DEVICE UNIT:
                   DEVICE SUB-UNIT:   (optional)
                   NUMBER OF PAGES:   (directory size, decimal)
```

It is not possible to change directory size if a directory is entered on the specified device. The specified directory size will be altered if the first directory entered on the device has another size. The parameters `<device unit>` and `<subunit>` are dummy, but must be within legal ranges.

For example, match a SCSI directory to SMD 450 MB:

```
@SET-MASS-STORAGE-SIZE DISC-2-OPTICAL-1,,,220584
```

# 22.9 Some Notes on Optical Disks

Software implementation of the optical disk drive allow applications to read SINTRAN files from a read-only directory on an optical disk. It is not possible to modify any information on such a read-only directory. Any attempt to write to the directory results in an error status from the device driver. The file system ignores all errors (saying "sector already written") when trying to update directory-, user- or object entries, but it returns an error message to the application if modification of file data is attempted. Creation of new files or versions and any modification of directory-, user- or object entry (rename directory, rename user, rename file, set file access etc.) will also result in an error message.

The whole directory structure must be written to the optical disk in one operation. This is done by copying from one device to another, from page 0 to the last page on the directory. It is not possible to copy single files or use the Backup-System command `MULTI-USER-COPY`.

An installation may have up to 4 optical disk drives, giving an online capacity of 4 Gigabytes. The media on each drive may be divided into logical subunits, maximum 8 on each device. All subunits on one drive must have equal size, but two drives on a system may have different directory sizes.

---

## Page 265

# SINTRAN III RELEASE INFORMATION, K-VERSION

## SCSI DEVICES

### 22.9.1 DIRECTORY SIZE

To utilise the total disk capacity, the directory size on a subunit should not be less than 125 Mb. It is, however, possible to have subunits with less capacity, but this will then make it impossible to utilise the total physical capacity of the disk.

### 22.9.2 COPYING TO AN OPTICAL DISK

All files containing permanent information should first be organised on a directory on a magnetic disk. Optical disks are slow (long access times) compared to magnetic disks. When reading a file from an optical disk, best performance is obtained if the file is contiguous. The files on the source directory should therefore be contiguous, if possible, but that is not absolutely necessary.

When the source directory is full, copying can be done. Before you start copying, however, be careful to note the directory name the source. If you end up with two directories with the same name on optical disk, you will not be able to rename either of the directories later.

The copying is done by means of the DEVICE-COPY command in the Backup System, but first the destination directory size must be set to match the size of the source directory:

@SET-MASS-STORAGE-SIZE DISC-2-OPTICAL-1,,,220584

Use the command @DIRECTORY-STATISTICS <source directory name> to get the total number of pages on the source directory.

If the directory size of the source directory (on magnetic disk) does not match the directory size of the destination directory (on optical disk, the Backup-System will assume the destination to be a tape device and include a volume header. This makes it impossible to access the files on optical disk later (and you cannot rewrite the files).

If the DMA-server finds a page already written on the optical disk, it will compare source with destination. If they are equal, copying (comparing) will continue until the whole directory is copied (compared), or copying will end with an error message if the source does not match the destination. In the event of power failure or system crash during copying, it is therefore possible to resume copying to the same media subunit after system restart. This must however be done before making any modifications to the source directory.

---

## Page 266

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## SCSI DEVICES

Use the Backup-System to copy:

```
@BACKUP-SYSTEM  
BACKUP-SYSTEM / I03

Ba-sy: SERVICE-PROGRAM-CUF  
Cuf-serv: TOTAL-RESERVATION  
Reservation? 'YES' : NO  
Cuf-serv: EXIT

Ba-sy: DEVICE-COPY  
Destination device name: DISC-2-OPTICAL-1  
Destination device unit and optional subunit: 0-0  
Source device name: DISC-450MB-1-F  
Source device unit and optional subunit: 0  
Function: COPY  
Blocksize in Pages: 36

Operation starts at : 87.10.07 – 13:41:36  
Pages to copy: 220584  

| Operation      | Pages   | Time       |
|----------------|---------|------------|
| Copying        | 220584  | 14:46:26   |
| Comparing      | 220584  | 14:58:41   |

Operation completed : 87.10.07 – 14:58:41  
- OK

Ba-sy: EXIT
```

When the copy operation is finished, release the source directory if it was entered. Then enter the directory on the optical disk:

```
@ENTER-DIRECTORY,,DISC-2-OPTICAL-1,0,0
```

The directory is now available for read-only use.

### 22.9.3 SIBAS DATABASES ON OPTICAL DISKS

SIBAS databases that have been copied to optical disks may be read directly from the optical disks if they are opened for read-only.

SIBAS, however, updates some control information on the disk even if it is opened for read-only. Since the optical disk cannot be updated, this causes an error.

Therefore, in order to read SIBAS databases directly from the optical disk, the following rules must be followed:

1. Each realm must consist of at least two SINTRAN files, a small one (1 page) at the beginning, followed by the rest of the realm in another file or files.

2. When the database is to be accessed as read-only from the optical disk, the small file and the schema file must be copied to a magnetic disk before the database is opened. When SIBAS updates the control information, this will then be done on the magnetic disk, not the optical disk.

Norsk Data ND-60.230.5 EN

---

## Page 267

# SINTRAN III RELEASE INFORMATION, K-VERSION

## SCSI DEVICES

After that, the database can be read from the optical disk.

Note, however, that if an attempt is made to write on the database, no error message will be given, even though the write operation will not be done.

There can be a maximum of 24 files in a SIBAS database. With the optical disk, these will be as follows:

- 1 containing the schema ("databasename:DATA")
- 1 reserved as a scratch file for redefining the database
- 1 containing the small files at the beginning of each realm
- 1 containing the system realm (indexes, etc.)
- 20 available for user realms

| File 1      | File 2       | File 3      | File 24              |
|-------------|--------------|-------------|----------------------|
| "databasename" | "smallfile" |             | Small realms         |
| Scratch     | Schema       |             | Magnetic disk        |

| File 4      | File 5       | File 6       | File 7       |
|-------------|--------------|--------------|--------------|
| "sysrealm"  | "bigrealm1"  | "bigrealm2"  | "bigrealm3"  |

Both the "smallfile" and the schema file will be updated when accessing the optical disk and must therefore be on a magnetic disk.

If the database has more than 20 realms, one solution may be to place the smallest realms in a separate OS file and copy this file also to a magnetic disk before using.

The splitting of a realm into several SINTRAN files means that the operator is responsible for making sure that the files are on the correct directories and telling SIBAS where they are.

## 22.9.4 HOW TO USE A SIBAS DATABASE ON THE OPTICAL DISK

An example using the system realm and two user realms.

Steps 1 - 3 only need to be done once.

### Step 1. Unload onto SINTRAN Files

```
@SIBAS-DBM
:
UNLOAD REALM realm1 ON tempfile1
UNLOAD REALM realm2 ON tempfile2
:
```

---

## Page 268

# Step 2. Edit the schema

@SIBAS-DRL

Input file: Source-schema-file

```
START INITIATION DATABASE databasename.....
NEWOS-FILE smallfile (PAGESIZE 1024)
NEWOS-FILE sysrealm (PAGESIZE nn)
NEWOS-FILE bigrealm1 (PAGESIZE nn)
NEWOS-FILE bigrealm2 (PAGESIZE nn)
NEW-SYSTEM-REALM systemrealm
  OS-FILE smallfile
  REALMSIZE 1
  ADDITIONAL OS-FILE sysrealm
NEW-SYSTEM-REALM realm1
  OS-FILE smallfile
  REALMSIZE 1
  ADDITIONAL OS-FILE bigrealm1
NEW-SYSTEM-REALM realm2
  OS-FILE smallfile
  REALMSIZE 1
  ADDITIONAL OS-FILE bigrealm2
```

# Step 3. Load the database

@SIBAS-DBM

```
LOAD REALM realm1 FROM tempfile1
LOAD REALM realm2 FROM tempfile2
```

The database can now be used on the magnetic disk until it is ready for storing on the optical disk.

Step 4 must be done each time the database is to be stored on the optical disk.

# Step 4. Copy to the optical disk

@SET-MASS-STORAGE-SIZE

@BACKUP-SYSTEM

Ba-sy: DEVICE-COPY

Steps 5 - 7 must be carried out when the database is to be accessed from the optical disk.

---

## Page 269

# SINTRAN III RELEASE INFORMATION, K-VERSION

## SCSI DEVICES

### Step 5. Copy the small files back to a magnetic disk

Make sure that the magnetic disk directory has a different name than the optical disk directory.

Enter both directories and copy the small files.

```
@ENTER-DIRECTORY opt-disk-dir..........
@ENTER-DIRECTORY mag-disk-dir..........

@COPY-FILE (mag-disk-dir)databasename,(opt-disk-dir)databasename
@COPY-FILE (mag-disk-dir)smallfile,(opt-disk-dir)smallfile
```

### Step 6. Tell SIBAS where the files are

```
@SIBAS-DBM

START databasename
CHANGE-OS-FILE databasename DIRECTORY mag-disk-dir
CHANGE-OS-FILE smallfile DIRECTORY mag-disk-dir
CHANGE-OS-FILE sysrealm DIRECTORY opt-disk-dir
CHANGE-OS-FILE bigrealm1 DIRECTORY opt-disk-dir
CHANGE-OS-FILE bigrealm2 DIRECTORY opt-disk-dir
```

### Step 7. Start SIBAS and open the database for read-only

Norsk Data ND-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 270

# 23. XMSG VERSION K

## 23.1 REQUIREMENTS

XMSG version K can only run under SINTRAN III version K. It is also the first version of XMSG that can run under SINTRAN III version K. For a description of an even more recent version of XMSG, refer to the next chapter on pages 257-261 for a summary of news in the L-version of XMSG.

XMSG version K is tailored to SINTRAN at installation time, the product contains code for two different versions of XMSG:

- one to run under SINTRAN III/VSE - this version will be entered on PIT 3 in much the same way as previous versions.

- the other to run under SINTRAN III/VSX - this version will be entered on PIT 6 - XPIT.

Also note that the COSMOS file transfer server (XFTRA) is no longer part of XMSG (ND-210373), but has been moved to COSMOS Basic Module (ND-210374).

## 23.2 COMMANDS MODIFIED - XMSG-COMMAND PROGRAM

### 23.2.1 DEBUG-MODE

To use this command to debug a dumped XMSG system, both the system to be debugged, and the running system must be version K.

### 23.2.2 DEFINE-NETWORK-CONNECTION

This command now accepts a string of 90 characters for the parameter \<Remote DTE address>. The name given as the parameter \<Port or System name?>, cannot start with a digit (0-9).

### 23.2.3 DEFINE-NETWORK-REMOTE-ENDPOINT

Two of the parameter prompts have been changed:

| Old Prompt            | New Prompt         |
|-----------------------|--------------------|
| 'Dial-up cost?'       | 'Connect charge?'  |
| 'Data cost/ksegment'  | 'Connect time?'    |

### 23.2.4 LIST-NETWORK-REMOTE-ENDPOINTS

When accessing a COSMOS X.25 server which is able to handle this command and the response from the X.25 server indicates the speed to be 0 (zero), the field 'Speed' will be filled with spaces.

Norsk Data NO-60.230.5 EN

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 271

# SINTRAN III RELEASE INFORMATION, K-VERSION

## XMSG VERSION K

### 23.2.5 LIST-SERVERS

This command replaces the old command LIST-SERVICE-PORTS. The command asks an XROUT to dump out, from its name table, all named ports.

### 23.2.6 LIST-SERVICE-PORTS

This command has been removed. It is replaced by the command LIST-SERVERS.

### 23.2.7 LIST-VERSION

The information listed by this command has been extended to include system name, version/revision, patch level and product number/name. This information is also listed for all network servers used by XMSG.

### 23.2.8 SET-MAXIMUM-HOP-COUNT

This command is now only available in advanced mode.

## 23.3 MODIFIED FUNCTIONS

### 23.3.1 CREATE DRIVER WITH CONTEXT (XFCRD)

If run under SINTRAN III/VSX, this function now requires that the XFPON option is set - because a driver must always run with paging on.

### 23.3.2 RECEIVE AND READ MESSAGE (XFRRE)

A new option bit (XFRMR) has been implemented in XFRRE which allows a task to receive a message, read the data, and, if the last byte in the message was read, release the message.

### 23.3.3 SENDING MESSAGE (XFSND)

A new option bit (XFTCM) in XFSND provides a way of sending the 'task current' message (without having to set the message as the 'port current' message using the Change Current Message (XFSCM) function).

## 23.4 NEW FUNCTIONS

### 23.4.1 FREE ALLOCATED MESSAGE BUFFERS (XFFRM)

The XFFRM function is used to free message buffers previously allocated by the XFALM (allocate message buffers) function.

---

## Page 272

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## XMSG VERSION K

### 23.4.2 WRITE AND RETURN MESSAGE (XFWRT)

As an alternative to the 'Write Message' (XFWRI) and 'Send Message' (XFSND) functions, the task can execute the XFWRT function which will transfer the user data into a message and then send the message back to the port from which it came.

### 23.5 MODIFIED XROUT SERVICES

#### 23.5.1 GET/CHECK ATTRIBUTE (XSGAT)

This service is divided into two sub-services:  
XSGXV - get XMSG version.  
XSGLO - get local system number and name.

#### 23.5.2 GET NETWORK SERVER INFORMATION (XSNSI)

It is now possible to call this privileged service from an unprivileged caller.

### 23.6 NEW XROUT SERVICES

#### 23.6.1 GET INFORMATION ABOUT A LINK (XSLIN)

This privileged service will return information about an active link (i.e., it will return information for an active link which has been started using the Start-Link and/or the Start-Network-Server command of the background command program).

#### 23.6.2 GET INFORMATION ABOUT NAMED PORTS (XSPIN)

This service allows any user to check and obtain information about named ports by sending a message containing a name as input parameter.

---

## Page 273

# SINTRAN III RELEASE INFORMATION, K-VERSION

XMSG VERSION L

## 24. XMSG VERSION L

### 24.1 REQUIREMENTS

XMSG version L requires SINTRAN III version K or later. It cannot run and installation is not possible under SINTRAN III version J or older.

If MON 300 (EUSEL) or MON 276 (ELOFU) has been called without a following MON 303 (ELOFF) before MON 200 (XMSG) is called, all escape/local characters will be discarded while the task is in XMSG. ELOFF will only delay the handling of an escape/local character until ELON is called.

The recommended sequence is:  
MON 300 (EUSEL) or MON 276 (ELOFU)  
MON 303 (ELOFF)  
MON 200 (XMSG)  
MON 302 (ELON)

### 24.2 COSMOS ROUTING MANAGEMENT (COSROUT) - IMPLICATIONS

XMSG has been modified to operate under control of the COSMOS Routing Management System (COSROUT).  
This implies the following changes/additions:

### 24.3 NEW COMMANDS - XMSG-COMMAND PROGRAM

#### 24.3.1 START-COSMOS-ROUTING-MANAGER

Start the COSMOS Routing Manager (COSROUT). The command is privileged.

#### 24.3.2 STOP-COSMOS-ROUTING-MANAGER

Stop the COSMOS Routing Manager (COSROUT). The command is privileged.

### 24.4 UNAVAILABLE COMMANDS - XMSG-COMMAND PROGRAM

When COSMOS Routing Management system is running, none of the following commands are accepted by the XMSG-Command program:

| Define-Friend-System            | Get-Link-State                       |
|--------------------------------|--------------------------------------|
| Define-Local-System             | Remove-Friend-System                 |
| Define-Network-Connection       | Remove-Network-Local-Endpoint        |
| Define-Network-Direct-Connection| Remove-Network-Remote-Endpoint       |
| Define-Network-Local-Endpoint   | Remove-Network-Remote-Groupnumber    |
| Define-Network-Remote-Endpoint  | Remove-Network-Remote-System         |
| Define-Network-Remote-Groupnumber| Remove-System                       |
| Define-Remote-Name              | Start-Link                           |
| Define-System-Route             | Start-Network-Server                 |
| Disable-Checksum                | Stop-Link                            |
| Enable-Checksum                 | Stop-Network-Server                  |

---

## Page 274

# 24.5 Unavailable Functions - XROUT

When COSMOS Routing Management is running, none of the following XROUT services are available to tasks other than the local COSMOS Routing Manager (COSROUT).

- Define Friend System (XSDFR)
- Define Local System Number (XSDLO)
- Define Remote Name (XSDRN)
- Define System Routing (XSDSY).
- Disable Checksum (XSDCS)
- Enable Checksum (XSECS)
- Remove Friend System (XSFRR)
- Send Letter and Kick (XSLXEK)
- Starting up/Stopping a Network Server (XSNET)
- Starting up/Stopping an Inter-System Link (XSLKI)

If one of the above services is requested by another task, the error XRPRV (caller was not privileged) or the error XRNXM (invalid service request - not available to current caller) is returned.

# 24.6 Commands Modified - XMSG-Command Program

When started, the XMSG-Command program will print additional information in the 'Options' field. If XMSG watchdog program (XMFI00) is included in XMSG, 'Watchdog' is printed. If XMSG has IOC gateway code included, 'Network gateway/IOC' is printed. If COSMOS routing Management (COSROUT) is running, 'Cosrout' is printed.

## 24.6.1 General

If a virtual system number is specified as reply to an 'XROUT system?' or a 'Remote system?' prompt, the command is not accepted and the error message `*- Virtual system number not allowed -*' is printed.

## 24.6.2 Debug-Mode

In debug-mode, the 'RT/DR' field of the List-Tasks command and the 'Owner-task' field of the List-Ports and the List-Messages commands have been changed (and corrected). The 'RT/DR' and the 'Owner-task' fields will now display the task names as BAKxx etc.

## 24.6.3 Enable-Trace

Changed trace 16 (frame received); if checksum is enabled in the remote system, this is traced as 'CE' when the 'start-of-datagram' fragment is received.

Changed trace 17 (frame sent); if checksum is provided on the datagram this is traced as 'CP' when the 'end-of-datagram' fragment is sent.

Norsk Data ND-60.230.5 EN

---

## Page 275

# SINTRAN III RELEASE INFORMATION, K-VERSION

## XMSG VERSION L

Changed trace 14 (message received); if a message is received with checksum provided, this is traced as 'message xxx received with checksum'.

Changed trace 21 (message being sent); when a message is being forced onto a specified link/server by Cosrout, this is traced as 'Forced xmit on link: xx' (where xx is the link index).

### 24.6.4 LIST-LINKS

The command now allows us to list information from the tables held by any XROUT system (which must be XMSG version ≥ K), i.e. the command first prompts for `XROUT system?`. If nothing or 0 (i.e. local XROUT) is specified as response to the first prompt, the command prompts for `Record address?`. If a system (name or number) is specified as response to the `XROUT system` prompt, the command prompts for `Link number?`; depending on the reply to the `Link number` prompt, all links or only the specified link number (decimal) in the remote XROUT system will be listed.

If an XROUT system is specified (as response to the first prompt), the information is obtained using the XROUT service XSLIN. This means that if the accessed XROUT system is running XMSG version K, our system must have been defined as a friend in the executing XROUT system, and, in addition, if the accessed system is XMSG K, the link table status information is not printed (because this information is not returned from the service XSLIN in an XMSG K system).

### 24.6.5 LIST-NETWORK-SERVERS

The command now allows us to list network servers running in any XROUT system (which must be XMSG version ≥ K), i.e. the command now prompts for `XROUT system?`. To obtain information from a remote system running XMSG version K, our system must have been defined as a friend in the executing XROUT system.

### 24.6.6 LIST-ROUTING-INFO

Changed the List-Routing-Info command. Routing information for virtual system numbers (i.e. 9800–9999) is not listed. The command will now discriminate between LAN and WAN. The command program will append a `?` after WAN if a route has not previously been used. This denotes that it is unknown whether it is LAN or WAN.

### 24.6.7 LIST-SYSTEMS

The command now allows us to list information from the tables held by any XROUT system (which must be XMSG version ≥ L), i.e. the command now prompts for `XROUT system?` and `System?`.

When executed, the utilisation of the routing table is listed as `System table status: xxx entries. xx in use. Max xx used`

Norsk Data ND-60.230.5 EN

---

## Page 276

# SINTRAN III RELEASE INFORMATION, K-VERSION  
## XMSG VERSION L  

The information listed in the 'Access' field is:

```
*----x implies no checksum on datagrams is enabled.
*--->x implies checksum is enabled in own system.
*<---x implies checksum is enabled in the listed system.
*<-->x implies checksum is enabled in both systems, i.e. a checksum is provided on each datagram transmitted.
```

"-": here (local system).  
"x": indicates the access rights of the listed system.  
O - Own system, F - Friend system, P - Public system.  

The information listed will also contain the network transmit and receive timeout values.

## 24.6.8 LIST-VERSION

The information listed also includes information about the COSMOS Routing Manager (COSROUT).

## 24.7 NEW COMMANDS - XMSG-COMMAND PROGRAM

### 24.7.1 DISABLE-CHECKSUM

This privileged command prompt for 'Xrout system' and 'System'.

If sent to a remote XROUT system running XMSG version L, the specified 'System' must be the system name/number of the requesting XMSG-Command program. If remote XROUT system is older than XMSG version L, an error message will be printed.

### 24.7.2 ENABLE-CHECKSUM

This privileged command prompt for 'Xrout system' and 'System'. If sent to a remote XROUT system running XMSG version L, the specified 'System' must be the system name/number of the requesting XMSG-Command program. If the remote XROUT system is older than XMSG version L, an error message will be printed.

If you have a network connection to a remote system which is able to handle checksum on datagrams, a checksum is provided on each datagram.

### 24.7.3 LIST-CONNECTIONS

The command "List-Connections" will give a list of ports currently being checked by XMID (watchdog) and the corresponding requesting tasks. It will be prompted for 'XROUT system' and 'Connection System'.

Norsk Data ND-60.230.5 EN

---

## Page 277

# SINTRAN III RELEASE INFORMATION, K-VERSION

## XMSG VERSION L

### 24.7.4 LIST-GENERATION-VARIABLES

This privileged command asks XROUT to dump, from its and XMSG's tables, information about the system-generation variables (as defined by the XMSG installation program). The command prompts for 'XROUT system?', the system number or name of the system where the routing program is to be found (default is the local system).

### 24.7.5 LIST-UTILIZATION

This privileged command asks XROUT to dump, from its and XMSG's tables, statistics about the system. The command prompts for 'XROUT system?', the system number or name of the system where the routing program is to be found (default is the local system).

### 24.8 MODIFIED FUNCTIONS

#### 24.8.1 DUMMY FUNCTION (XFDUM)

On return, the configuration mask being returned in the D-register has been extended with three new bits:

| Bit 11a: | set if COSMOS Routing Management (COSROUT) is running. |
| Bit 12a: | set if XMSG watchdog program (XMFID0) is included.     |
| Bit 13a: | set if XMSG is generated with gateway software for IOC network servers. |

#### 24.8.2 GENERAL STATUS (XFGST)

On return, the X-register will now contain the number of messages queued to the port specified by the A-register, and the D-register will now contain the message type.

### 24.9 NEW FUNCTIONS

#### 24.9.1 GENERAL STATUS EXTENDED (XFGSX)

This is a new function for a 'snapshot' situation overview on messages queued to the first 16 ports opened.

---

## Page 278

# 25. Affected Subsystems

## ND-500/5000 System Package

- For SINTRAN III/VSX ver. K, generation 500.
- Version A of the ND-500/5000 System Package (ND-211305) contains the following products:
  - ND-500 Background Monitor
  - ND-500 Swapper
  - ERS/SINTRAN III Watchdog
  - ND-500 Place-Library

For use under generation 500 of SINTRAN III.

## ND-500 Monitor

- Only version H of ND-500 Background Monitor (ND-210333) may be used when running SINTRAN III generation 312 or earlier (on ND-500 systems); and version I must be used when running generation 406 (on ND-5000 systems).
- Version J should be used for running generation 500 (both on ND-500 and ND-5000 systems).

## ND-500 Swapper

- Only version H of ND-500 Swapper (ND-211034) may be used when running SINTRAN III generation 312 or earlier (on ND-500 systems); and version I must be used when running generation 406 (on ND-5000 systems).
- Version J should be used for running generation 500 (both on ND-500 and ND-5000 systems).

## XMSG

- Only version K or later of XMSG (ND-210373) can be used.

## COSMOS Basic Module

- Version D of COSMOS Basic Module (ND-210374) is required when running XMSG version K or later.
- To be able to use 8-bit I/O on TADs, version E of COSMOS BASIC MODULE is needed.
- Note that documentation on the COSMOS Basic Module is now found in the SINTRAN III System Supervisor manual (ND-30.003).

## SINTRAN III Configuration

- Version C of the SINTRAN III Configuration program (ND-211024) is required to set or change the configuration of SCSI magnetic disks and streamer tape drives.
- To handle SCSI optical disks and magnetic tape drives, version D is required.

## NOTS Service

- Version B of the NOTS Service program (part of ND-211024) is used to set or change the configuration of Net/One terminals.

## ERS/SINTRAN III Watchdog

- The SINTRAN III Watchdog of the Event Report System (ERS), (ND-211072, version A) may be used to get a better report of error conditions on ND-500/5000 systems.

Norsk Data ND-60.220.5 EN
Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 279

# SINTRAN III RELEASE INFORMATION, K-VERSION

## Affected Subsystems

### BACKUP-SYSTEM

Version H of the Backup system (ND-210337) is required to handle files with file index > 255 (more than 256 files per user).  
Version I is required to handle SCSI streamer tape drives.  
Revision 103 of the Backup system and revision 102 of the DMA server are required to handle SCSI optical disks and magnetic tape drives.

| 312 |
| --- |
| 406 |
| 500 |

### FILE-MANAGER

Versions A or B of the File Manager (part of ND-210518) will not handle files with file index > 255 (more than 256 files per user).  
Version C of the File Manager (ND-210705) will handle this problem.

### FILE-SYSTEM-INVESTIGATOR

Version O of the File System Investigator (part of ND-210628) is required to handle files with file index > 255 (more than 256 files per user).

### LINKAGE-LOADER

Version H of the Linkage Loader (ND-210319) is required to handle communication with RT-programs due to the changed RTFIL format.

### ND-LINKER

The ND-LINKER (ND-211224, version A) is required to handle the new domain files (as opposed to the old ND-500 domains built by the Linkage-Loader).

### CONVERT-DOMAIN

The conversion program, CONVERT-DOMAIN (ND-211229) is used to convert an "old" domain (built by the Linkage-Loader and stored on a triple of files) to a domain file without having to reload the domain.

| 500 |
| --- |

### SYMBOLIC-DEBUGGER

Version F of the Symbolic-Debugger (ND-210336) can be used when running SINTRAN III version K to debug RT-programs.  
Version H is required to handle ND-500 domains stored on domain files (by the ND-Linker).

### LED-DEBUGGER

Revision B02 of the LED-Debugger (ND-211157) is required to handle ND-500 domains stored on domain files (by the ND-Linker).

| 500 |
| --- |

### TELEFIX-LOCAL

Version C01 of Telefix-Local (ND-210775) is required.

### USER-ENVIRONMENT

Version B of User Environment (ND-210518) must be changed slightly to run under the VSX-version of SINTRAN III version K.  
Version C of User Environment offers a highly improved performance when used under the K-version of SINTRAN.

---

## Page 280

I'm sorry, the image appears to be blank. Could you please provide another image?

---

## Page 281

# SEND US YOUR COMMENTS!

Are you frustrated because of unclear information in our manuals? Do you have trouble finding things?

Please let us know if you:
- find errors
- cannot understand information
- cannot find information
- find needless information.

Do you think we could improve our manuals by rearranging the contents? You could also tell us if you like the manual.

Send to:  
Norsk Data A.S  
Documentation Department  
P.O. Box 25 BOGERUD  
N - 0621 OSLO 6 - Norway

# NOTE!

This form is primarily for documentation errors. Software and system errors should be reported on Customer System Reports.

| Manual Name: |  | Manual number: |  |
| --- | --- | --- | --- |
| Which version of the product are you using? |  |
| What problems do you have? (use extra pages if needed) |  |
| |  |
| |  |
| |  |
| |  |
| |  |
| Do you have suggestions for improving this manual? |  |
| |  |
| |  |
| |  |
| |  |
| Your name: |  | Date: |  |
| Company: |  | Position: |  |
| Address: |  |
| What are you using this manual for? |  |
| |  |

---

## Page 282


---

Scanned by Jonny Oddene for Sintran Data © 2021

---

## Page 283

I'm sorry, but I can't convert what's on the document as it's just a blank page. If you have another page you'd like me to convert, please let me know.

---

## Page 284

I can't convert this image to Markdown as it appears to be a cover without any text content. If there's another page you'd like me to convert, feel free to share it!

---

