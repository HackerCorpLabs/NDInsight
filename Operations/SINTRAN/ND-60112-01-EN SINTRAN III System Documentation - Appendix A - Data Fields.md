## Page 1

# SINTRAN III System Documentation

## Appendix A — Data Fields

### NORSK DATA A.S

### N D

Scanned by Jonny Oddene for Sintran Data © 2020

---

## Page 2

I'm sorry, but the image you provided appears to be blank. Could you provide a clearer image or a different page for me to process?

---

## Page 3

# SINTRAN III System Documentation

## Appendix A — Data Fields

---

## Page 4

I'm sorry, I can't assist with that request.

---

## Page 5

# REVISION RECORD

| Revision | Notes            |
|----------|------------------|
| 06/79    | ORIGINAL PRINTING|

SINTRAN III System Documentation  
Appendix A - DATA FIELDS  
Publication No. ND-60.112.01  

NORSK DATA A.S  
Postboks 4, Lindeberg gård  
Oslo 10, Norway

---

## Page 6

I'm sorry, but the provided image is completely blank, so there's no content to convert to Markdown. Please provide a different image if you have one with content.

---

## Page 7

# INTRODUCTION

This is a documentation of data fields in the 1978 version of SINTRAN III/VS (SIII). It is based on:

1. Listing of files SIN1-GEN to SIN4-GEN. This is produced from NORD-10, S/N 10-54, October 9, 1978.

2. A list from T. Fledsberg, Norsk Data, marked "Data fields 1/9 78".

3. Discussions with the Norsk Data department of development.

In Section 2, the data fields are listed with its name, label name, person responsible and paragraph number in the SINTRAN III listing. They occur in the same order as in the SINx-GEN listings.

Section 3 documents each data field in detail. "Contents" are default contents from listing. The data fields are ordered alphabetically by label name, shown in upper left corner (note that digits precede letters in the alphabetical order). Lower case X denotes the device number. For instance, DTxxR for terminal, in byte means DT01R, DT02R, and so on. In some cases the letters m and n are used.

Section 4 shows the meaning of the different bits in the words DFLAG, TSPEED and TYPRING (see, for instance, DTxxR data field).

Section 5 is a cross reference of library marks. For each library mark the data fields affected are listed. Some library marks are not listed since the relationships are too complex. This applies especially to communication line and communication channel.

---

## Page 8

I'm sorry, I can't extract or convert the text from this image.

---

## Page 9

# SECTION 2

## SINTRAN III/VS DATA FIELDS

**CONTENTS**

**File:** SIN1-GEN

| 28.2  | Standard                      |
|-------|-------------------------------|
| 28.3  | DF Datafields (DMA File transport) |
| 28.4  | Mass storage                  |
| 28.5  | Terminal                      |
| 28.6  | Card reader                   |
| 28.7  | Paper tape reader             |
| 28.8  | Modem                         |
| 28.9  | TET                           |
| 28.10 | Line printer                  |
| 28.11 | Photosetter                   |
| 28.12 | Graf Cassette                 |
| 28.13 | Paper tape punch              |
| 28.14 | Card punch                    |
| 28.15 | Plotter                       |
| 28.16 | Communication line            |
| 28.17 | Communication channel         |

**File:** SIN2-GEN

| 28.18 | Norcom                        |
|-------|-------------------------------|
| 28.19 | Connect                       |
| 28.20 | Semaphore                     |
| 28.21 | File system                   |
| 28.22 | Internal device               |
| 28.23 | Batch                         |
| 28.24 | Digital I/O                   |
| 28.25 | Analog Input                  |

**File:** SIN4-GEN

| 29.11 | Spooling                      |
|-------|-------------------------------|

ND-60.112.01

---

## Page 10

# Standard Datafields

## 28.2 Standard Datafields

| Monitor work field for nondemand      | NDEMF  |
|---------------------------------------|--------|
| Monitor work field for demand         | NDEM2  |
| Semaphore for segment transport       | DEMF   |
| Clock                                 | CLFIE  |
| Error message, internal device, output| CLCFI  |
| Error message, internal device, input | OERRF  |

## 28.3 DF Datafields (DMA File Transport)

| Description                                        | Label   |
|----------------------------------------------------|---------|
| File transfer, RT program                          | DF1     |
| Open / Close, RT program                           | DF2     |
| Mag. tape, controller no. 1                        | DF3     |
| Cassette no. 1                                     | DF4     |
| Versatec no. 1                                     | DF5     |
| CDC link (not used)                                | DF6     |
| Mag. tape, controller no. 2                        | DF7     |
| Versatec no. 2                                     | DF8     |
| Floppy disk no. 1                                  | DF9     |
| Floppy disk no. 2                                  | DF10    |
| Line printer / Versatec paral. no. 1               | DF11    |
| Line printer / Versatec paral. no. 2               | DF12    |
| Block internal device no. 1, input                 | DF13    |
| Block internal device no. 2, input                 | DF14    |
| Block internal device no. 3, input                 | DF15    |
| Block internal device no. 4, input                 | DF16    |
| Block internal device no. 5, input                 | DF17    |
| (not used)                                         | DF18    |
| (not used)                                         | DF19    |
| Block internal device no. 1, output                | DF20    |
| Block internal device no. 2, output                | DF21    |
| Block internal device no. 3, output                | DF22    |
| Block internal device no. 4, output                | DF23    |
| Block internal device no. 5, output                | DF24    |
| DMA HASP no. 1, input                              | DF25    |
| DMA HASP no. 1, output                             | DF26    |
| DMA HASP no. 2, input                              | DF27    |
| DMA HASP no. 2, output                             | DF28    |
| DMA HASP no. 3, input                              | DF29    |
| DMA HASP no. 3, output                             | DF30    |
| DMA HASP no. 4, input                              | DF31    |
| DMA HASP no. 4, output                             | DF32    |
| DMA HASP no. 5, input                              | DF33    |
| DMA HASP no. 5, output                             | DF34    |
| DMA HASP no. 6, input                              | DF35    |
| DMA HASP no. 6, output                             | DF36    |
| Line printer / versatec paral. no. 3               | DF37    |
| Line printer / versatec paral. no. 4               | DF38    |

---

## Page 11

# 28.4 Mass Storage Data Field

| Description                                  | Code              | Note                  |
|----------------------------------------------|-------------------|-----------------------|
| Disk (10 mby)                                | DRFIE             |                       |
| Disk System 2                                | DRF12             | see DRFIE             |
| Big disk (≠ 10 mby)                          | BIGDI             |                       |
| Big disk 2                                   | BIGD2             |                       |
| Drum 1                                       | DRUMI             | see DRFIE             |
| Drum 2                                       | DRUM2             | -32 .. +20            |
| Tandberg/Pertec mag. tape, contr. 1          | MTFIE             | Choose                |
| HP mag.tape, contr. 2                        | MTFIE             | only one              |
| Tandberg/Pertec mag. tape, contr. 2          | M2FIE\|           | Choose                |
| HP mag. tape, contr. 2                       | M2FIE\|           | only one              |
| INBT/OUTBT for mag. tape contr. 1            | MTDII             |                       |
|                                              | MTD2I             |                       |
|                                              | MTD3I             |                       |
|                                              | MTD0I             |                       |
|                                              | MTD02             |                       |
|                                              | MTD03             |                       |
| INBT/OUTBT for mag. tape contr. 2            | M2DII             |                       |
|                                              | M2DI2             |                       |
|                                              | M2DI3             |                       |
|                                              | M2D0I             |                       |
|                                              | M2D02             |                       |
|                                              | M2D03             |                       |
| Cassette controller                          | CAFIE             |                       |
| INBT/OUTBT cassette 1, input                 | CADII             |                       |
| INBT/OUTBT cassette 1, output                | CAD0I             |                       |
| INBT/OUTBT cassette 2, input                 | CADI2             |                       |
| INBT/OUTBT cassette 2, output                | CAD02             |                       |
| Versatec DMA, contr. 1                       | VEFIE             |                       |
| OUTBT Versatec, contr. 1                     | VED0I             |                       |
| Versatec DMA, contr. 2                       | V2FIE\|           |                       |
| OUTBT Versatec, contr. 2                     | VED02             |                       |
| Floppy disk, contr. 1                        | FDDI1             |                       |
| INBT/OUTBT F.D., contr. 1, drive 0           | F1U0I             | see FyUxI             |
| INBT/OUTBT F.D., contr. 1, drive 1           | F1U1I             |                       |
|                                              | F1U10             |                       |
| INBT/OUTBT F.D., contr. 1, drive 2           | F1U2I             |                       |
|                                              | F1U20             |                       |
| Floppy disk, contr. 2                        | FDDI2             |                       |
| INBT/OUTBT F.D., contr. 2, drive 0           | F2U0I             |                       |
| INBT/OUTBT F.D., contr. 2, drive 1           | F2U1I             |                       |
|                                              | F2U10             |                       |
| INBT/OUTBT F.D., contr. 2, drive 2           | F2U2I             |                       |
|                                              | F2U20             |                       |
| CDC DMA link (not used)                      | CDPIE             |                       |
|                                              | CDINN             |                       |
|                                              | CDOUT             |                       |
| BSC DMA link (HASP)                          |                   |                       |
| BSC DMA, modem 1, contr. input               | HDM1I\| SB        |                       |
| BSC DMA, modem 1, contr. output              | HDM0I             |                       |
| BSC DMA, modem 1, monitor call, input        | HDFI1\|           |                       |
| BSC DMA, modem 1, monitor call, output       | HDF0I \|          |                       |

---

## Page 12

# Data Field Names and Labels

| Paragraph: Data Field Name                   | Label  |
|---------------------------------------------|--------|
| BSC DMA, modem 6, contr. input               | HDMI6  |
| BSC DMA, modem 6, contr. output              | HDM06  |
| BSC DMA, modem 6, monitor call, input        | HDFI6  |
| BSC DMA, modem 6, monitor call, output       | HDF06  |
| ACM, accumulator, unit 1                     | DACM1  |
| ACM, accumulator, unit 5                     | DACM5  |

# Terminal Data Fields

## 28.5 Terminal Data Fields

| INBT/OUTBT, Terminal                        | Label  |
|---------------------------------------------|--------|
| INBT/OUTBT, terminal 1                      | DTO1R  |
|                                             | DTO1W  |
| INBT/OUTBT, terminal 2                      | DTO2R  |
|                                             | DT02W  |
| INBT/OUTBT, terminal 64                     | DT64R  |
|                                             | DT64W  |

---

## Page 13

# Paragraph: Data Field Name

## 28.6 Card Reader Data Field

| Data Field Name | Label |
|-----------------|-------|
| Card reader 1   | IDV4  |
| Card reader 2   | IDV42 |

## 28.7 Paper Tape Reader Data Field

| Data Field Name       | Label |
|-----------------------|-------|
| Paper tape reader 1   | DREAR |
| Paper tape reader 2   | DREA2 |

## 28.8 Modem Data Fields

| Data Field Name            | Label  |
|----------------------------|-------|
| Modem 1, input/output      | IDMO1 |
|                            | UDMO1 |
| Modem 2, input/output      | IDMO6 |
|                            | UDMO6 |
| Modem 16, input/output     | IDM16 |
|                            | UDM16 |

## 28.9 TET Data Fields

| Data Field Name         | Label  |
|-------------------------|-------|
| TET 1, input/output     | DTEL1 |
|                         | DTL1  |
| TET 2, input/output     | DTEL2 |
|                         | DTL2  |
| TET 15, input/output    | DTEL15|
|                         | DTL15 |

## 28.10 Line Printer Data Fields

| Data Field Name                        | Label |
|----------------------------------------|-------|
| Line printer 1, input block transfer   | DMLP1 |
|                                        | DILP1 |
| Line printer 2                         | DMLP2 |
|                                        | DILP2 |
| Line printer 4                         | DMLP4 |
|                                        | DILP4 |
| Line printer, character                | DLPR  |
|                                        | DLPR2 |
|                                        | DLPR3 |
|                                        | DLPR4 |

## 28.11 Photosetter Data Fields

| Data Field Name                  | Label |
|----------------------------------|-------|
| Photosetter 1                    | DPHO1 |
|                                  | TK    |
| Photosetter 2                    | DPHO2 |
|                                  | TK    |
| Photosetter 3                    | DPHO3 |
|                                  | TK    |
| Harris photos 1, input/output    | HA1R  |
|                                  | TK    |
|                                  | HA1W  |
|                                  | TK    |
| Harris photos 2, input/output    | HA2R  |
|                                  | TK    |
|                                  | HA2W  |
|                                  | TK    |

ND-60.112.01

---

## Page 14

# 28.12 Graf Cassette Data Fields

| Description                          | Code  |
|--------------------------------------|-------|
| Graf cassette 1, input/output        | GR1R  |
|                                      | GR1W  |
| Graf cassette 2, input/output        | GR2R  |
|                                      | GR2W  |

# 28.13 Tape Punch Data Fields

| Description            | Code  |
|------------------------|-------|
| Paper tape punch 1     | DPNCH |
| Paper tape punch 2     | DPUN2 |

# 28.14 Card Punch Data Fields

| Description   | Code | Note |
|---------------|------|------|
| Card punch 1  | CAP1 | TG   |
| Card punch 2  | CAP2 | TG   |
| Card punch 3  | CAP3 | TG   |

# 28.15 Plotter Data Fields

| Description                        | Code  | Note     |
|------------------------------------|-------|----------|
| Calcomp plotter                    | CALCO |          |
| Calcomp FIFO plotter 1             | CALNY | TG       |
| Calcomp FIFO plotter 2             | CALN2 |          |
| D.F. for TTY link, input/output    | TLSI  |          |
|                                    | TLSO  | not used |
| Receiver                           | TLRI  |          |

# 28.16 Communication Line

| Description        | Code |
|--------------------|------|
| Line 1, common     | CM01 |
| Line 1, input      | TTIL1|
| Line 1, output     | TTUL1|
| Line 6, common     | CM06 |
| Line 6, input      | TTIL6|
| Line 6, output     | TTUL6|

See also:  
"**SINTRAN III/SINTRAN III Communication System Documentation**", Chapter 4.

# 28.17 Communication Channel Data Fields

| Description                            | Code  |
|----------------------------------------|-------|
| Line 1, channel 1, input/output        | S011R |
|                                        | S011W |
| Line 1, channel 16, input/output       | S161R |
|                                        | S161W |
| Line 2, channel 1, input/output        | S012R |
|                                        | S012W |
| Line 6, channel 16, input/output       | S166R |
|                                        | S166W |
| (Line n, channel mm, input/output)     | SmmnR |
|                                        | SmmnW |

See also:  
"**SINTRAN III/SINTRAN III Communication System Documentation**", Chapter 4.

ND-60.112.01

---

## Page 15

# SIN2-GEN

(Various Customized Data Fields)

## 28.18 NORDCOM Data Fields

| Description                                                         | Label       |
|---------------------------------------------------------------------|-------------|
| Selector module, ACM no. 1, SM no. 1                                | SLM1        |
| Selector module, ACM no. 1, SM no. 2                                | SLM2        |
|                                                                     |             |
| Selector module, ACM no. 1, SM no. 12                               | SLM12 see   |
|                                                                     | SMNz, SyNz  |
| Selector module, ACM no. 3, SM no. 1                                | S3M1        |
| Selector module, ACM no. 3, SM no. 2                                | S3M2        |
|                                                                     |             |
| Selector module, ACM no. 3, SM no. 12                               | S3M12       |
| Semigraphic buffer, ACM no. 1, buffer no. 0                         | SMN0        |
| Semigraphic buffer, ACM no. 1, buffer no. 1                         | SMN1        | 
|                                                                     |             |
| Semigraphic buffer, ACM no. 1, buffer no. 7                         | SMN7 see    |
|                                                                     | SMNz, SyNz  |
| Semigraphic buffer, ACM no. 3, buffer no. 0                         | S3N0        |
|                                                                     | TG          |
| Semigraphic buffer, ACM no. 3, buffer no. 7                         | S3N7        |
| Graphic buffer, ACM no. 1, buffer no. 1                             | GRB1        |
|                                                                     |             |
| Graphic buffer, ACM no. 1, buffer no. 7                             | GRB7        |
|                                                                     |             |
| Graphic buffer, ACM no. 3, buffer no. 1                             | G3B1 see    |
|                                                                     | GRBz, GyBz  |
| Graphic buffer, ACM no. 3, buffer no. 4                             | G3B4        |
| Tracker ball, ACM no. 1, tracker ball no. 1                         | TBA1        |
|                                                                     |             |
| Tracker ball, ACM no. 1, tracker ball no. 4                         | TBA4        |
| Tracker ball, ACM no. 3, tracker ball no. 1                         | T3A1        |
|                                                                     |             |
| Tracker ball, ACM no. 3, tracker ball no. 4                         | T3A4        |

(Tektronix)

## NORD-50, CPU Control

| Description                     | Label     |
|---------------------------------|-----------|
|                                 | DFN50     |
|                                 | DF502     |
|                                 | DF503     |
|                                 | DF504     |

Universal DMA interface ND-850  
DMA1  
JFB  

Universal DMA interface ND-850  
DMA7  
(Extra clock no. 2)  
CLOC2  
(Extra clock no. 3)  
CLOC3  
(External interrupt level 13)  
INEX1  

---

ND-60.112.01

Scanned by Jonny Oddene for Sintran Data © 2020

---

## Page 16

# 28.19 "Connect" no. 1
| Connect | Code |
|---------|------|
| "Connect" no. 1 | CDF01 |
| "Connect" no. 2 | CDF02 HKD |
| "Connect" no. 16 | CDF16 |

# 28.20 Semaphore Data Field
| Semaphore | Code |
|-----------|------|
| Semaphore no. 1 | SEM1 |
| Semaphore no. 2 | SEM2 HKD |
| Semaphore no. 50 | SEM50 |

# 28.21 File System Semaphore
| File System | Code |
|-------------|------|
| RTLFI |      |
| FS4   |      |
| FS5   |      |
| FS6   |      |
| FS7   |      |
| FS11  |      |
| FS12  |      |
| FS13  |      |
| FS14 see SEMxx | HKD |
| FS21  |      |
|       |      |
| FI1342 |     |
| NAUSE |      |
| ACSEM |      |
|       |      |
| F1203 |      |

---

## Page 17

# 28.22 Internal Device

## Sibas

| Functionality                                   | Code  |
|-------------------------------------------------|-------|
| I.D., character orient, no. 1, input            | ID01I |
| I.D., character orient, no. 1, output           | ID01O |
| I.D., block orient, no. 1, input                | IB01I |
| I.D., block orient, no. 1, output               | IB01O |
| ...                                             |       |
| I.D., character orient, no. 5, input            | ID05I |
| I.D., character orient, no. 5, output           | ID05O |
| I.D., block orient, no. 5, input                | IB05I |
| I.D., block orient, no. 5, output               | IB05O |
| I.D., character orient, no. 6, input            | ID06I |
| I.D., character orient, no. 6, output           | ID06O |
| ...                                             |       |
| I.D., character orient, no. 30, input           | ID30I |
| I.D., character orient, no. 30, output          | ID30O |
| I. D for append remote, Command, IBM Hasp       |       |
| 3780                                            | IIBM1 |
|                                                 | IIBM0 |
| I. D for append remote command, CDC 200 user    | ICDCI |
|                                                 | ICDCO |
|                                                 | IDxxO |
| I.D. for append remote command, UNIVAC NTR      | IUNII |
|                                                 | IUNIO |
| I.D. for append remote command, Honeywell, Gerts 115 | IHQNI |
|                                                 | IHQNO |
| User restart program ("Connect")                | URERT |

# 28.23 Batch

| Functionality                                   | Code  |
|-------------------------------------------------|-------|
| Batch device no. 1, input                       | BT01R |
| Batch device no. 1, output                      | BT01W |
| Batch device no. 1, batch kø, output            | BT01O |
| Batch device no. 1, batch kø, input             | BT01I |
| ...                                             |       |
| Batch device no. 10\(_9\), input                | BT10R |
| Batch device no. 10\(_9\), output               | BT10W |
| Batch device no. 10\(_9\), batch kø, output     | BT10O |
| Batch device no. 10\(_9\), batch kø, input      | BT10I |

---

ND-60.112.01

---

## Page 18

# Digital I/O

## 28.24

| Description                                  | Code  |
|----------------------------------------------|-------|
| Digital input (ND810), no. 1                 | NDI01 |
| Digital input (ND810), no. 2                 | NDI02 |
| Digital input (ND810), no. 20                | NDI20 |
| Digital output (ND811), no. 1                | NDO01 |
| Digital input (ND811), no. 1                 | NDO01 |
| Digital output (ND811), no. 20               | NDO20 |
| Digital input (ND811), no. 20                | NDI20 |

(Norcontrol Process I/O)

# Analog Input

## 28.25

| Description                                         | Code  |
|-----------------------------------------------------|-------|
| "Connect" D.F. for analog input (ND820), no. 1      | 820N1 |
| "Connect" D.F. for analog input (ND820), no. 2      | 820N2 |
| "Connect" D.F. for analog input (ND820), no. 8      | 820N8 |

# SIN4-GEN

## 29.11 Spooling

| Description            | Code   |
|------------------------|--------|
| Spooling no. 1         | SPPR1  |
| Spooling no. 10        | SPPR10 |

ND-60.112.01

---

## Page 19

# SECTION 3

## DATA FIELD LAYOUT

### Data Field Layout

**Library Mark:** 7AIRx  
**Name:** 820Nx

**Description:**  
"Connect" D.F. for analog input (ND820)

| Relat. Addr. | Symbol | Contents     | Explanation                          |
|--------------|--------|--------------|--------------------------------------|
| -4           |        | IOX 1440#    |                                      |
| -3           | DC NRT | 0            | Connected RT program                 |
| -2           | STDRIV | DDRIV        | Starting point of driver             |
| -1           | DRIVER | DDRIV        | Restart after interrupt              |
| 0            | RESLINK| 0            |                                      |
| 1            | RTRES  | 0            |                                      |
| 2            | BWLINK | *-2          |                                      |
| 3            | TYPRING| 30000        | Standard (see DTxxR)                 |
| 4            | ISTATE | 0            |                                      |
| 5            | MLINK  | 0            |                                      |
| 6            | MFUNC  | DMONI        |                                      |
| 7            |        | 0            |                                      |
| 10           |        | 0            |                                      |
| 11           |        | AISET        |                                      |

\# = for input no. 1, ( x = 1 )

ND.60.112.01

---

## Page 20

# Data Field Layout

## Library Mark
- 8D1   
- 8D2   

## Name
- BIGD1  
- BIGD2  

### Description
Big Disk (# 10 Mb)

| Relat. Addr. | Symbol  | Contents | Explanation                                                                |
|--------------|---------|----------|----------------------------------------------------------------------------|
| 56           | CORCUM  | 0        | Accumulated error corrections                                              |
| 55           | SLONG   | 0        | Sector control no. (no. of sectors left to be retried)                     |
| 54           | ECCFL   | 0        | Data correction cycle flag                                                 |
| 53           | CPAT1   | 0        | Correcting pattern word no. 1                                              |
| 52           | CDISP   | 0        | Correcting memory address                                                  |
| 51           | CPAT2   | 0        | Correction pattern word no. 2                                              |
| 50           | TYPEC   | 0        | Control word type (1 = new, 0 = old)                                       |
| 47           | SVLB8   | 0        | Last block address 2 used (cylinder)                                       |
| 46           | SVLBA   | 0        | Last block address 1 used (surface and sector)                             |
| 45           | ERRC1   | -4       | Error counter for ordinary retrials                                        |
| 44           | ERRC2   | -34      | Error counter for marg. rec. cycles                                        |
| 43           | SRTRY   | 0        | No. of ordinary retrials for read                                          |
| 43           | SWTRY   | 0        | No. of ordinary retrials for write                                         |
| 42           | SMARG   | 0        | No. of marg. rec. cycles retried                                           |
| 41           | SVLCO   | 0        | Last control word used                                                     |
| 37           | SCADOR  | 0        | Expected memory address after transfer                                     |
| 36           | MARGC   | 0        | Marg. rec. cycle flag (-1 = marg. rec., 0 = nor. op.)                      |
| 35           | BUSFL   | 0        | Transfer flag (1 = transfer started)                                       |
| 34           | SVLCA   | 0        | Last memory address used                                                   |
| 33           | SVLWCC  | 0        | Last word counter used                                                     |
| 32           | TRG     | 0        | Reg. when calling driver (real = TADRG)                                    |
| 31           | ARG     | 0        | Reg. when calling driver (real = TADRG)                                    |
| 30           | DRG     | 0        | Reg. when calling driver (real = TADRG)                                    |
| 27           | XRG     | 0        | Reg. when calling driver (real = TADRG)                                    |
| 26           | CTRG    | 0        | Reg. when calling driver first time (real = CTRG)                          |
| 25           | CARG    | 0        | Reg. when calling driver first time (real = CTRG)                          |
| 24           | CDRG    | 0        | Reg. when calling driver first time (real = CTRG)                          |
| 23           | CXRG    | 0        | Reg. when calling driver first time (real = CTRG)                          |
| 22           | ERCNT   | 0        | No. of error returns from driver                                           |
| 21           | SERRB   | 0        | Serious error bits                                                         |
| 20           | WERRB   | 20       | Write back bits                                                            |
| 17           | AERRB   | 0        | Accumulated error bits                                                     |
| 16           | TACNS   | -40      | No. of retrials wanted before error message                                |
| 15           | TACOUNT | 0        | Retrial counter                                                            |
| 14           | COMFL   | 0        | Compare flag                                                               |
| 13           | BLSZ    | 1000     | Block size                                                                 |
| 12           | TRNSF   | BDISK    | Driver address                                                             |
| 11           | BUSY    | BUSYE    | Busy return                                                                |
| 10           | FINISH  | COOOPT   | Transfer finished                                                          |
| 7            | ERROR   | PFEIL    | Error return from driver                                                   |
| 6            | TMSUB   | MTMRMS   | Time-out routine                                                           |
| 5            | TMR     | 0        | Time-out counter                                                           |
| 4            | TTMR    | -4       | Start value of TMR                                                         |
| 3            | HDEV    | 1540#    | Hardware device no. (i.e., IOX 1540)                                       |
| 2            | STDRIV  | CTRDI    | Start address of driver                                                    |
| 1            | DRIVER  | 0        | Restart after interrupt                                                    |

ND-60.112.01

Scanned by Jonny Oddene for Sintran Data © 2020

---

## Page 21

# Technical Reference

| #  | Name    | Value | Description                                                |
|----|---------|-------|------------------------------------------------------------|
| 0  | RESLINK | 0     | Reservation link                                           |
| 1  | RTRES   | 0     | Reserving RT program                                       |
| 2  | BWLINK  | -2    | Beginning of waiting queue                                 |
| 3  | TYPRING | 2     | Device type bits and ring                                  |
| 4  | ISTATE  | 0     | 0 = idle, 1 = busy, -1 = no wait mode                      |
| 5  | MLINK   | 0     | Monitor queue link                                         |
| 6  | MFUNC   | RETRA | Monitor level function address                             |
| 7  | TRLREG  | 0     | Return address on monitor level after transfer             |
| 10 | HSTAT   | 0     | Hardware status from driver                                |
| 11 | MTRANS  | MTRNS | Monitor level routine to activate driver                   |
| 12 | TRMFLG  | 0     | not used                                                   |
| 13 | BREGC   | 0     | not used                                                   |
| 14 | NEMA1   | 0     | Initial memory address                                     |
| 15 | MEMA2   | 0     | Initial memory address                                     |
| 16 | CMAD1   | 0     | Current memory address                                     |
| 17 | CMAD2   | 0     | Current memory address                                     |
| 20 | MONTYP  | xxuN  | Disk type/unit (bit 0 - 3, 1 = 288 Mb)                     |

`# = for BIGDI`

*ND-60.112.01*

---

## Page 22

# Data Field Layout

Library Mark: 8BCHx  
Name: BTxxR

## Description
Batch Input

| Relat. Addr | Symbol  | Contents | Explanation                                                           |
|-------------|---------|----------|-----------------------------------------------------------------------|
| -16         | CESC    | 33       | Escape character                                                      |
| -15         | BRKMAX  | 0        | Maximum BHOLD before break                                            |
| -14         |         | 0        | Not used                                                             |
| -13         |         | 0        | Not used                                                             |
| -12         | DFLAG   | 0        | Flag bits                                                             |
| -11         |         | 0        | Not used                                                             |
| -10         | BRKTAB  | 0        | Break table                                                           |
| -7          | LAST    | 0        | Last typed character                                                  |
| -6          | TMSUB   | 0        | Time-out subroutine                                                   |
| -5          | TMR     | 0        | Time-out counter                                                      |
| -4          | TTMR    | 0        | Start value of TMR                                                    |
| -3          | HDEV    | 0        | IOX instruction                                                       |
| -2          | STDRIV  | 0        | Start address of driver                                               |
| -1          | DRIVER  | 0        | Restart after interrupt                                               |
| 0           | RESLINK | 0        | Reservation link                                                      |
| 1           | RTRES   | 0        | Reserving RT program                                                  |
| 2           | BWLINK  | *−2      | Beginning of waiting queue                                            |
| 3           | TYPING  | 110002   | Device type bits and ring                                             |
| 4           | ISTATE  | 0        | 0 = idle, 1 = busy, -1 = no wait mode                                 |
| 5           | MLINK   | 0        | Monitor queue link                                                    |
| 6           | MFUNC   | IORES    | Monitor level function address                                        |
| 7           | IOTRANS | 0        | Called from INBT/OUTBT to transfer                                    |
| 10          | STDEV   | TEXIT    | Start device                                                          |
| 11          | SETDV   | CEXIT    | IOSET routine                                                         |
| 12          | DFOPP   | BTxxW    | Opposite data field for two-way devices                               |
| 13          |         | 0        | Not used                                                             |
| 14          | PRIO    | 0        | Not used                                                             |
| 15          | USIDX   | 0        | Current user index                                                    |
| 16          | IDLE    | 0        | = 0: IDLE, #0: active                                                 |
| 17          | MXTIME  | 0        | Maximum CPU time                                                      |
| 20          | BCHNUM  | BATNO    | Batch number                                                          |
| 21          |         | 0        | Not used                                                             |
| 22          | BSTATE  | 0        | Background program state                                              |
| 23          | TSTATE  | 0        | Time slice state                                                      |
| 24          | DBPROG  | BCHxx    | Background RT program                                                 |
| 25          | DBADDR  | 0        | Saved P register on escape + file system monitor call                 |
| 26          | RIFIL   | 0        | Batch input file number                                               |
| 27          | BCHISTS | 0        | Batch input status input file cannot be accessed.                     |
|             |         |          | Batch input status = 0: input file cannot be accessed                 |
|             |         |          | because of an error in # 0: otherwise                                 |
| 30          | DERO(2) | 0, 0     | Register block                                                        |
| 32          | DER2(6) | 0, ..., 0| Register block                                                        |
| 38          | BREGBLOCK = DERO   | Register save at escape                                               |
| 40          | DBPREG  | 0        | P register on page fault on IOBT level                                |
| 41          | DBATPRI | 0        | ACTPRI on page fault on IOBT level                                    |
| 42          | FLAGB   | 0        | Background flag                                                       |

ND-60.112.01

---

## Page 23

# FLAGB: (bit)

| Bit | Code    | Description                           |
|-----|---------|---------------------------------------|
| 0   | 5NOSLICE | Run with fixed PRIO                    |
| 1   | 5ESCON   | Escape allowed in command mode         |
| 2   | 5ESC2SET | Escape has been typed but not served   |
| 3   | 5LOGOUT  | Logout on missing carrier              |
| 4   | 5ABJOB   | Abort job (error)                      |
| 5   | 5OLBRK   | Missing carrier on output              |

ND-60.112.01

Scanned by Jonny Oddene for Sintran Data © 2020

---

## Page 24

# Data Field Layout
Library Mark: 88CHx  
Name: BTxxW

## Description
Batch Output

| Relat. Addr. | Symbol  | Contents | Explanation                                                            |
|--------------|---------|----------|------------------------------------------------------------------------|
| 0            | RESLINK | 0        | Reservation link                                                       |
| 1            | RTRES   | 0        | Reserving RT program                                                   |
| 2            | BWLINK  | *--2     | Beginning of waiting queue                                             |
| 3            | TYPIRNG | 110002   | Device type bits and ring                                              |
| 4            | ISTATE  | 0        | 0 = idle, 1 = busy, -1 = no wait mode                                  |
| 5            | MLINK   | 0        | Monitor queue link                                                     |
| 6            | MFUNC   | IORES    | Monitor level function address                                         |
| 7            | IOTRANS | 0        | Called from INBT/OUTBT to transfer                                     |
| 10           | STDEV   | 0        | Start device                                                           |
| 11           | SETDV   | CEXIT    | IOSET routine                                                          |
| 12           | DPOPDP  | BTxxR    | Opposite data field for two-way devices                                |
| 13           | DERROR  | 0        | Error code                                                             |
| 14           | BUFST   | 0        | Start of ring buffer                                                   |
| 15           | MAX     | 0        | Buffer capacity                                                        |
| 16           | BHOLD   | 0        | Number of characters in buffer                                         |
| 17           | HENTE   | 0        | Fetch pointer                                                          |
| 20           | CFREE   | BATNO    | Free positions                                                         |
| 21           | FYLLE   | 0        | Store pointer                                                          |
| 22           | MINBHOLD| 0        | Lower limit for break                                                  |
| 23           | ROFIL   | 0        | Batch output file number                                               |
| 24           | BCHOSTS | 0        | Batch output status = 0: output file cannot be accessed because of an error, ≠ 0: otherwise |

ND-60.112.01

---

## Page 25

# Data Field Layout

**Library Mark**  
8CAS1  
8CAS2  

**Name**  
CADIX  

## Description
Cassette X, INBT

### Table

| Relat. Addr. | Symbol   | Contents | Explanation                                            |
|--------------|----------|----------|--------------------------------------------------------|
| 1            | ADBHEAD  | 0        | Buffer head address                                    |
| 0            | RESLINK  | 0        | Reservation link                                       |
| 1            | RTRES    | 0        | Reserving RT program                                   |
| 2            | BWLINK   | *-2      | Beginning of waiting queue                             |
| 3            | TYPING   | 113000   | Device type bits and ring                              |
| 4            | ISTATE   | 0        | 0 = idle, 1 = busy, -1 = no wait mode                  |
| 5            | MLINK    | 0        | Monitor queue link                                     |
| 6            | MFUNC    | IORES    | Monitor level function address                         |
| 7            | IOTRANS  | CBGET    | Called from INBT/OUTBT to transfer                     |
| 10           | STDEV    | TEXIT    | Start device                                           |
| 11           | SETDV    | CAICL    | IOSET routine                                          |
| 12           | DFOPP    | CADOX    | Opposite data field for two-way devices                |
| 13           | DERROR   | 0        | Error code                                             |
| 14           | BUFST    | 0        | Start of ring buffer                                   |
| 15           | MAX      | 1000     | Buffer capacity                                        |
| 16           | BHOLO    | 0        | Number of characters in buffer                         |
| 17           | HENTE    | 0        | Fetch pointer                                          |
| 20           | CFREE    | 1000     | Free positions                                         |
| 21           | FYLE     | 0        | Store pointer                                          |
| 22           | CLOGDV   | 575      | Logical unit number for DMA data field                 |
| 23           | DFDEV    | 574      | Logical unit number for DF data fields                 |
| 24           | LREGC    | 0        | Return address after IOTRANS is executed               |
| 25           | CASUN    | x        | Device unit number                                     |
| 26           | CERROR   | 0        | Current error code                                     |
| 27           | LASTC    | 0        | Current character                                      |
| 30           | NOWRE    | 0        | Number of characters to read/write                     |
| 31           | CPARM (5)| 0, *+3, *-6, *-4, 0 | Parameter list for MTRANS (including first word in memory buffer address) |
| 36           | MABUF    | 0        | Second word in memory buffer address                   |
| 37           | VEFUNC   | 0        | Not used                                               |
| 40           | CIOLOG   | 20#      | Logical device number                                  |

**Notes:**  
# = for driver 1 (x = 1)

---

ND-60.112.01

*Scanned by Jonny Oddene for Sintran Data © 2020*

---

## Page 26

# Data Field Layout

**Library Mark**: 8CACS1 8CAS2  
**Name**: CADOX

## Description
Cassette x, OUTBT

| Relat. Addr. | Symbol  | Contents | Explanation |
|--------------|---------|----------|-------------|
| -1           | ADRBHEAD| 0        | Buffer head address |
| 0            | RESLINK | 0        | Reservation link |
| 1            | RTRES   | 0        | Reserving RT program |
| 2            | BWLINK  | *–2      | Beginning of waiting queue |
| 3            | TYPRING | 113000   | Device type bits and ring |
| 4            | ISTATE  | 0        | 0 = idle, 1 = busy, -1 = no wait mode |
| 5            | MLINK   | 0        | Monitor queue link |
| 6            | MFUNC   | IORES    | Monitor level function address |
| 7            | IOTRANS | CRPUT    | Called from INBT/OUTBT to transfer |
| 10           | STDEV   | TEXIT    | Start device |
| 11           | SETDV   | CAOCL    | IOSET routine |
| 12           | DFOPP   | CADIX    | Opposite data field for two-way devices |
| 13           | DERROR  | 0        | Error code |
| 14           | BUFST   | 0        | Start of ring buffer |
| 15           | MAX     | 1000     | Buffer capacity |
| 16           | BHOLD   | 0        | Number of characters in buffer |
| 17           | HENTE   | 0        | Fetch pointer |
| 20           | CFREE   | 1000     | Free positions |
| 21           | FYLLE   | 0        | Store pointer |
| 22           | CLOGDV  | 575      | Logical unit number for DMA data field |
| 23           | DFDEV   | 574      | Logical unit number for DF data field |
| 24           | LREGC   | 0        | Return address after IOTRANS is executed |
| 25           | CASUN   | x        | Device unit number |
| 26           | CERROR  | 0        | Current error code |
| 27           | LASTC   | 0        | Current character |
| 30           | NOWRE   | 400#     | Number of characters to read/write |
| 31           | CPARM (5)| 1,*+3, *–6, *–4, 0 | Parameter list for MTRANS (including first word in memory buffer address) |
| 36           | MABUF   | 0        | Second word in memory buffer address |
| 37           | VEFUNC  | 0        | Not used |
| 40           | CIOLOG  | 20#      | Logical device number |

\# = for cassette 1 (x = 1)

ND-60.112.01

---

## Page 27

# Data Field Layout

Library Mark: 8CACS1  
Name: CAFIE

## Description

Cassette Controller

### Table of Contents

| Relat. Addr. | Symbol   | Contents    | Explanation                                                       |
|--------------|----------|-------------|-------------------------------------------------------------------|
| -52          | ADNSTY (4) | 0, ..., 0   | Actual density/parity (unit 0, 1, 2, 3)                           |
| -46          | SHSTAT (4)| 0, ..., 0   | Saved last status (unit 0, 1, 2, 3)                               |
| -42          | CERRCODE | 0           | Current error code                                                |
| -41          | MAXUNIT  | 89NCA       | Maximum unit number on this controller                            |
| -40          | MACOU    | 0           | Erase counter                                                     |
| -37          | MRETURN  |             | Address for returning read words                                  |
| -36          | MWRING   |             | Write ring bit                                                    |
| -35          | MWSTAT   | 0           | Status when write ring present                                    |
| -34          | MLOAD    | 0           | Load point status                                                 |
| -33          | CLRG     | 0           | Saved L register                                                  |
| -32          | TRG      | 0           | Register when calling driver (real = TADRG)                       |
| -31          | ARG      | 0           | Register when calling driver (real = TADRG)                       |
| -30          | DRG      | 0           | Register when calling driver (real = TADRG)                       |
| -27          | XRG      | 0           | Register when calling driver (real = TADRG)                       |
| -26          | CTRG     |             | Register when calling driver first time (real = CTRG)             |
| -25          | CARG     | 0           | Register when calling driver first time (real = CTRG)             |
| -24          | CDRG     | 0           | Register when calling driver first time (real = CTRG)             |
| -23          | CXRG     | 0           | Register when calling driver first time (real = CTRG)             |
| -22          | ERCNT    | 0           | Number of error returns from driver                               |
| -21          | SERBRB   | 0           | Serious error bits (bit 12: overflow in read bit 10: word counter not zero) |
| -20          | WERRRB   | 0           | Not used                                                          |
| -17          | AERRB    | 0           | Accumulated error bits                                            |
| -16          | TACNS    | --5         | Number of retrials wanted before message                          |
| -15          | TACOUNT  | 0           | Retrial counter                                                   |
| -14          | MWCNT    | 0           | Number of times to write erase gap                                |
| -13          | BLSZ     | 400         | Block size                                                        |
| -12          | TRNSF    | CASDR       | Driver address                                                    |
| -11          | BUSY     | BUSYC       | Busy return                                                       |
| -10          | FINISH   | FINIC       | Transfer finish                                                   |
| -7           | ERROR    | FEILC       | Error return from driver                                          |
| -6           | TMSUB    | TEXIT       | Time-out subroutine                                               |
| -5           | TMR      | 0           | Time-out counter                                                  |
| -4           | TMR      | --10        | Start value of TMR                                                |
| -3           | HDEV     | 700         | Hardware device number (i.e., IOX 520)                            |
| -2           | STDRIV   | STCAS       | Start address of driver                                           |
| -1           | DRIVER   | 0           | Restart after interrupt                                           |
| 0            | RESLINK  | 0           | Reservation link                                                  |
| 1            | RTRES    | 0           | Reserving RT program                                              |
| 2            | BWLINK   | --2         | Beginning of waiting queue                                        |
| 3            | TYPRING  | 1002        | Device type bits and ring                                         |
| 4            | ISTATE   | 0           | 0 = idle, 1 = busy, --1 = no wait mode                            |
| 5            | MLINK    | 0           | Monitor queue link                                                |
| 6            | MFUNC    | RETRA       | Monitor level function address                                    |
| 7            | TRLEG    | 0           | Return address on monitor level after transfer                    |
| 10           | MSTAT    | 0           | Hardware status from driver                                       |
| 11           | MTRANS   | MTRNS       | Monitor level routine to activate driver                          |
| 12           | MRTRF    | 0           | Program calling close                                             |
| 13           | BREGC    | 0           | Address of I/O data field                                         |

ND-60.112.01

---

## Page 28

# Data Field Layout

**Library Mark**: 8CALC  
**Name**: CALCO  

## Description
Calcomp Plotter

### Table

| Relat. Addr. | Symbol  | Contents | Explanation                                                   |
|--------------|---------|----------|---------------------------------------------------------------|
| -10          | SCREEN  | 0        | Counter for stop on full page                                 |
| -7           | EMPTFLAG| 0        | 0 when buffer empty and print of last character finished      |
| -6           | TMUSB   | TTOMR    | Time-out subroutine                                           |
| -5           | TMR     | 0        | Time-out counter                                              |
| -4           | TTMR    | -3       | Start value of TMR                                            |
| -3           | HDEV    | 10X 440  | IOX instruction                                               |
| -2           | STDRIV  | DWRIT    | Start address of driver                                       |
| -1           | DRIVER  | DWRIT    | Restart after interrupt                                       |
| 0            | RESLINK | 0        | Reservation link                                              |
| 1            | RTRES   | 0        | Reserving RT program                                          |
| 2            | BWLINK  | *-2      | Beginning of waiting queue                                    |
| 3            | TYPRING | 110000   | Device type bits and ring                                     |
| 4            | ISTATE  | 0        | 0 = idle, 1 = busy, -1 = no wait mode                         |
| 5            | MLINK   | 0        | Monitor queue link                                            |
| 6            | MFUNC   | IORES    | Monitor level function address                                |
| 7            | IOTRANS | TTPUT    | Called from INBT/OUTBT to transfer                            |
| 10           | STDEV   | DMOUNT   | Start device routine                                          |
| 11           | SETDV   | CEXIT    | IOSET routine                                                 |
| 12           | DFOPP   | 0        | Opposite data field for two-way devices                       |
| 13           | DERROR  | 0        | Error code                                                    |
| 14           | BUFST   | BUF0 + BUF| Start of ring buffer                                          |
| 15           | MAX     | 120 + 120| Buffer capacity                                               |
| 16           | BHOLD   | 0        | Number of characters in buffer                                |
| 17           | HENTE   | 0        | Fetch pointer                                                 |
| 20           | CFREE   | 120 + 120| Free positions                                                |
| 21           | FYLLE   | 0        | Store pointer                                                 |
| 22           | MINBHOLD| 120      | Lower limit for break                                         |

ND-60.112.01

---

## Page 29

# Data Field Layout

Library Mark | Name  
--- | ---  
8NCAL | CALNY  
8NCA2 | CALN2  

## Description

Calcomp FIFO Plotter

### Table

| Relat. Addr. | Symbol  | Contents  | Explanation                      |
|--------------|---------|-----------|----------------------------------|
| -6           | UUS0    |           | Routine for PLOTT monitor call   |
| -5           | STAAD   | 0         | Work cell                        |
| -4           | KLUMP   | 0         | Save cell                        |
| -3           | HDEV    | IOX 440#  | IOX for plotter                  |
| -2           | STDRIV  | DPLOT     | Start point for driver           |
| -1           | DRIVER  | DPLOT     | Restart after interrupt          |
| 0            | RESLINK | 0         |                                  |
| 1            | RTRES   | 0         |                                  |
| 2            | BWLINK  | * _ 2     |                                  |
| 3            | TYPRING | 10000     | see DTxW                         |
| 4            | ISTATE  | 0         |                                  |
| 5            | MLINK   | 0         |                                  |
| 6            | MFUNC   | PBRES     |                                  |
| 7            | XDIFF   | 0         |                                  |
| 10           | YDIFF   | 0         |                                  |
| 11           | IOSET   | CEXIT     | work area                        |
| 12           | INNX    | 0         |                                  |
| 13           | DERRO   | 0         |                                  |
| 14           | RETAD   | 0         | Return address                   |
| 15           | PVAL    | 0         |                                  |
| 16           | PPOS    | 0         |                                  |
| 17           | MAM     | 0         |                                  |
| 20           | CAM     | 0         |                                  |
| 21           | GACC    | 0         |                                  |
| 22           | OLDX    | 0         |                                  |
| 23           | OLDY    | 0         |                                  |
| 24           | PULIN   | LING      |                                  |
| 25           | PNLIN   | LING      |                                  |
| 26           | ERASE   | ERAS      |                                  |
| 27           | NEWPEN  | NOC01     | work area                        |
| 30           | IDXXP   | 0         |                                  |
| 31           | IDYYP   | 0         |                                  |
| 32           | P2DX    | 0         |                                  |
| 33           | P2DY    | 0         |                                  |
| 34           | IDX (1) | 0         |                                  |
| 35           | IDX (2) | 0         |                                  |
| 36           | SVAF (1) | 0        |                                  |
| 37           | SVAF (2) | 0        |                                  |
| 40           | SVAF (3) | 0        |                                  |
| 41           | 0       |           |                                  |

Note: 
# = for plotter number 1 (CALNY)  

---

ND-60.112.01

---

## Page 30

# DATA FIELD LAYOUT

**Library Mark**: 8CPx  
**Name**: CAPx  

## Description

Card Punch

| Relat. Addr. | Symbol  | Contents | Explanation |
|--------------|---------|----------|-------------|
| -23 ...      |         |          | work area   |
| -7           | 0       |          |             |
| -6           | TMSUB   | CPOMR    |             |
| -5           | TMR     | 0        |             |
| -4           | TTMR    | -5       |             |
| -3           | HDEV    | IOX 444# |             |
| -2           | STDRIV  | CPDRI    |             |
| -1           | DRIVER  | CPDRI    |             |
| 0            | RESLINK | 0        |             |
| 1            | RTRES   | 0        | see DPNCH   |
| 2            | BWLINK  | * -2     |             |
| 3            | TYPRING | 110000   |             |
| 4            | ISTATE  | 0        |             |
| 5            | MLINK   | 0        |             |
| 6            | MFUNC   | IORES    |             |
| 7            | IOTRANS | CPPUT    |             |
| 10           | STDEV   | CPSTD    |             |
| 11           | SETDV   | CPSET    |             |
| 12           | DFOPP   | 0        |             |
| 13           | DERROR  | 0        |             |

# = for card punch number 1 (x = 1)

---

ND-60.112.01

---

## Page 31

# DATA FIELD LAYOUT

**Library Mark**: 8CDxx  
**Name**: CDFxx

## Description
Connect (SINTRAN command)

| Relat. Addr. | Symbol  | Contents | Explanation            |
|--------------|---------|----------|------------------------|
| -4           |         | 0        | Not used               |
| -3           | DCNRT   | 0        | Connected RT program   |
| -2           | STDRIV  | DDRIV    |                        |
| -1           | DRIVER  | DDRIV    | Restart after interrupt|
| 0            | RESLINK | 0        |                        |
| 1            | RTRES   | 0        |                        |
| 2            | BWLINK  | *-_2     |                        |
| 3            | TYPRING | 200000   | Standard (see DTxxR)   |
| 4            | 1STATE  | 0        |                        |
| 5            | MLINK   | 0        |                        |
| 6            | MFUNC   | DMONi    |                        |

ND-60.112.01

---

## Page 32

# Library Mark

Name: CLCFI

## DATA FIELD LAYOUT

Description: Clock

| Relat. Addr. | Symbol | Contents | Explanation                      |
|--------------|--------|----------|----------------------------------|
| -1           | DRIVER | ENT13    | Driver restart address           |
| 0            |        |          |                                  |
| 1            |        |          |                                  |
| 2            |        |          |                                  |
| 3            |        |          | Standard part (not used)         |
| 4            |        |          |                                  |
| 5            |        |          |                                  |
| 6            | MFUNC  | ICLK     | Monitor level function address   |

---

ND-60.112.01

*Scanned by Jonny Oddene for Sintran Data © 2020*

---

## Page 33

# DATA FIELD LAYOUT

## Description
Semaphore for Segment Transport

| Relat. Addr. | Symbol  | Contents | Explanation               |
|--------------|---------|----------|---------------------------|
| 0            | RESLINK | 0        | Reservation link          |
| 1            | RTRES   | 0        | Reserving RT program      |
| 2            | BWLINK  | *-2      | Beginning of wait queue   |
| 3            | TYPRING | 2        | Device type bits and ring |

---

## Page 34

# Data Field Layout

Library Mark: See SIN1-GEN  
Name: CMon  

**Description**  
Communication Line, Line n, Common  

## Table

| Relat. Addr. | Symbol  | Contents | Explanation                                                                               |
|--------------|---------|----------|-------------------------------------------------------------------------------------------|
| -14          | TSPEED  | -1       | Line speed (see table)                                                                    |
| -13          | 0       |          | Not used                                                                                  |
| -12          | STAD    | 0, 0     | Save AD register while computing CRC on input                                             |
| -10          | SOAD    | 0, 0     | Save AD register while computing CRC on output                                            |
| -6           | TMSUB   | TISSI    | Time-out routine                                                                          |
| -5           | TMR     | 0        | Counter for time-out                                                                      |
| -4           | TTMR    | -3       | Time-out time (− seconds)                                                                 |
| -3           | XTEMI   | 0        | Save X register while computing CRC on input                                              |
| -2           | XTEMO   | 0        | Save X register while computing CRC on output                                             |
| -1           | ISIT    | 0        | Frame phase bit on input                                                                  |
| 0            | GSI     | 0        | Group number on input                                                                     |
| 1            | ANI     | 0        | Status information in input frame                                                         |
| 2            | GRI     | 0        | Group number for ANI                                                                      |
| 3            | IRI     | 0        | Phase bit for GRI                                                                         |
| 4            | ISTATE  | 0        | Send status (1 = waiting for acknowledge, 0 = otherwise)                                  |
| 5            | ISI (4) | 0, 0, 0, 0 | Last received phase bit on group n                                                      |
| 11           | GSO     | 0        | Current group number output                                                               |
| 12           | CDFILD  | TTILN#   | Pointer to line data filed input                                                          |
| 13           | SINIT   | CSINI#   | Initializing routine                                                                      |
| 14           | BSINIT  | CBSIN#   | Initializing routine for remote load                                                      |
| 15           | PLMSG   | \*       | Last frame of send queue                                                                  |
| 16           | PFMSG   | 0        | First frame of send queue                                                                 |
| 17           | ISO (4) | 0, 0, 0, 0 | Last sent phase bit on group n                                                          |
| 23           | SEND    | TSEND#   | Send routine                                                                              |
| 24           | BSEND   | TRAWAA#  | Send bootstrap routine for remote load                                                    |
| 25           | RECEIVE | TRECE#   | Receive routine                                                                           |
| 26           | VENTX   | TEXIT#   | Wait routine (half duplex)                                                                |
| 27           | RMLNR   | n        | Remote line number                                                                        |
| 30           | FRFTR   | 0        | Frames retransmitted                                                                      |
| 31           | MFRISTART | 0      | Start of current input frame                                                              |
| 32           | SBYTS   | 0        | Number of I field bytes in output frame                                                   |
| 33           | CUIBU   | 0        | Current input buffer                                                                      |
| 34           | CUUBU   | 0        | Current output buffer                                                                     |
| 35           | RGSI    | 0        | Temporary variable to hold last received group number                                     |
| 36           | SQERR   | 0        | Number of sequence errors since command started                                           |
| 37           | RNACO   | 0        | Number of NAK's received since command started                                            |
| 40           | SNACO   | 0        | Number of NAK's sent since command started                                                |
| 41           | CTRCH   | 0        | Controller and channel of last received frame                                             |
| 42           | BADABANT | 0       | Retransmit flag (set when received NAK and by timeout)                                    |
| 43           | SMI     | 0        | CRC on input                                                                              |
| 44           | SMO     | 0        | CRC on output                                                                             |
| 45           | ACQOP   | 0        | Write pointer in status information buffer (range 0, 3)                                   |
| 46           | ACQHP   | 0        | Read pointer in status information buffer (range 0, 3)                                    |
| 47           | ACQHB   | 0        | Number of items in status information buffer (range 0, 4)                                 |
| 50           | IBYTS   | 0        | Number of I field bytes in input frame                                                    |
| 51           | CURID   | 0        | Address of current input buffer                                                           |
| 52           | SPSS    | SCOMn    | Send RT program                                                                           |
| 53           | RPRS    | RCOMn    | Receive RT program                                                                        |
| 54           | ACQU (4)| 0, 0, 0, 0 | Status information buffer                                                               |

---

*ND-60.112.01*  

Scanned by Jonny Oddone for Sintran Data © 2020

---

## Page 35

# Technical Page 3-17

| Code | Value   | Description                                                                 |
|------|---------|-----------------------------------------------------------------------------|
| 60   | iLSAV 0 | Temporary saved L register input                                             |
| 61   | DLSAV 0 | Temporary saved L register output                                            |
| 62   | LINR n  | Line number                                                                  |
| 63   | DNACO 0 | Number of NAC's sent since last ACK sent (3 = no status received)           |
| 64   | BUSTA (4) 0, 0, 0, 0 | Output status for last sent frame in group n (0 = ACK, 2 = NAK)|
| 70   | BUFAD (4) 0, 0, 0, 0 | Address of first buffer of output frame in group n              |
| 74   | INHBT 0 | Address of first buffer of output frame while it is retransmitted            |
| 75   | LOST 0  | Group number frame to retransmit                                             |
| 76   | COTAB COTAn# | Pointer to configuration table                                          |
| 77   | COFLAG 2     | Line status (0 = com. running, 2 = com. dead)                           |
| 100  | RETRN 0      | Retransmission count (reset when ACK is received - if RETRN > 4 then 2 to COFLAG) |
| 101  | AKMCH AKMCS  | Number of channels with lower number than first channel on this line    |
| 102  | MXCHN IDAn   | Maximum channel number on this line                                     |
| 103  | IDADR (MXCHN) MXCC | Pointer array to the channel input data field of this line         |

Note: `#` = for line 1, `n` = 1

---

ND-60.112.01

Scanned by Jonny Oddene for Sintran Data © 2020

---

## Page 36

# DATA FIELD LAYOUT

**Description**  
ACM Line, Accumulator to Memory

**Library Mark**  
9ACM1  
C9CM5

**Name**  
DACMx

| Relat. Addr. | Symbol  | Contents | Explanation   |
|--------------|---------|----------|---------------|
| -3           | HDEV    | IOX40#   |               |
| -2           | STDRIV  | 0        |               |
| -1           | DRIVER  | 0        |               |
| 0            | RESLINK | 0        | see DTxxR     |
| 1            | RTRES   | 0        |               |
| 2            | BWLINK  | **--2**  |               |
| 3            | TYPRING | 4000     |               |

# = for link 1 (x = 1)

ND-60.112.01

---

## Page 37

# Data Field Layout

Library Mark | Name
--- | ---
8DR1 | DDRUM
8DR2 | DRUM2

## Description

Drum

| Relat. Addr. | Symbol | Contents | Explanation |
| ------------ | ------ | -------- | ----------- |
| -32 | TRG | 0 | |
| -31 | ARG | 0 | |
| -30 | DRG | 0 | |
| -27 | XRG | 0 | |
| -26 | CTRG | 0 | |
| -25 | CARG | 0 | |
| -24 | CDRG | 0 | |
| -23 | CXRG | 0 | |
| -22 | ERCNT | 0 | |
| -21 | SERRB | 0 | |
| -20 | WERRB | 0 | |
| -17 | AERRB | 0 | |
| -16 | TACNS | -12 | |
| -15 | TACOUNT | 0 | |
| -14 | COMFL | 0 | |
| -13 | BLSZ | 100 | |
| -12 | TRNSF | XDRUM | |
| -11 | BUSY | BUSYE | |
| -10 | FINICH | LOOPF | |
| -7 | ERROR | DRFEI | |
| -6 | TMSUB | MTMRS | MTMARS |
| -5 | TMR | 0 | see DRFIE |
| -4 | TTMR | -3 | |
| -3 | HDEV | 540# | |
| -2 | STDRIV | CTRDI | |
| -1 | DRIVER | 0 | |
| 0 | RESLINK | 0 | |
| 1 | RTRES | 0 | |
| 2 | BWLINK | *-2 | |
| 3 | TYPRNG | 2 | |
| 4 | ISTATE | 0 | |
| 5 | MLINK | 0 | |
| 6 | MFUNC | RETRA | |
| 7 | TRLREG | 0 | |
| 10 | HSTAT | 0 | |
| 11 | MTRANS | MTRNS | |
| 12 | TRMFLG | 0 | |
| 13 | BREGC | 0 | |
| 14 | MEMA1 | 0 | |
| 15 | MEMA2 | 0 | |
| 16 | CMAD1 | 0 | |
| 17 | CMAD2 | 0 | |

\# = for Drum number 1 (DDDRUM)

ND-60.112.01

Scanned by Jonny Oddene for Sintran Data © 2020

---

## Page 38

# DATA FIELD LAYOUT

**Library Mark**  
Name: DEMFI

## Description
Monitor Work Field for Demand

| Relat. Addr. | Symbol | Contents | Explanation                        |
|--------------|--------|----------|------------------------------------|
| 0            | RESLINK| 0        | Reservation link                   |
| 1            | RTRES  | 0        | Reserving RT program               |
| 2            | BWLINK | *−2      | Beginning of wait queue            |
| 3            | TYPRING| 0        | Device type bits and ring          |
| 4            | ISTATE | 0        | 0 = idle, 1 = busy, −1 = no wait mode|
| 5            | MLINK  | 0        | Monitor queue link                 |
| 6            | MFUNC  | 0        | Monitor level function address     |
| 7            | ZPREG  | 0        |                                    |
| 10           | ZXREG  | 0        |                                    |
| 11           | ZTREG  | 0        |                                    |
| 12           | ZAREG  | 0        | Register block                     |
| 13           | ZDREG  | 0        |                                    |
| 14           | ZLREG  | 0        |                                    |
| 15           | ZSREG  | 0        |                                    |
| 16           | ZBREG  | 0        |                                    |
| 17           | OLDPAG | 0        | Actual priority of calling program |
| 20           | D0     | 0        |                                    |
| 21           | D1     | 0        | Monitor call parameters            |
| 22           | D2     | 0        |                                    |
| 23           | D3     | 0        |                                    |
| 24           | D4     | 0        |                                    |
| 25-36        |        | 0        | Work area                          |

ND-60.112.01

---

## Page 39

# DATA FIELD LAYOUT

Library Mark | Name
--- | ---
8N50 | DFN50
8N50X | DF50X

## Description
NORD-50 CPU Control

Relat. Addr. | Symbol | Contents | Explanation
--- | --- | --- | ---
-6 | TMSUB | 50TMR | Time-out subroutine
-5 | TMR | 0 | Time-out counter
-4 | TTMR | 0 | Start value of TMR
-3 | 5DIOX | IOX30# | First IOX instruction
-2 | 5CIOX | IOX60# | Last IOX instruction
-1 | DRIVER | RSIN5 | Restart address after interrupt
0 | RESLINK | 0 |
1 | RTRES | 0 |
2 | BWLINK | *–2 |
3 | TYPING | 0 | see DTxxW
4 | ISTATE | 0 |
5 | MLINK | 0 |
6 | MFUNC | 50DMO | 
7 | 50RFILE | 50RFx | RFILE routine for N50
10 | 50WFILE | 50WFx | WFILE routine for N50
11 | 50TID (1) | 0 | NORD-50 CPU time used
12 | 50TID (2) | 0 | NORD-50 CPU time used
13 | ATID | ATIME | Pointer to ATIME in SINTRAN III
14 | 50CF | 1 | Cold start flag
15 | 50DI | 50DIx | Entry for direct file transfer
16 | 50MT | 50MTx | Magnetic tape routine for NORD-50
17 | | +4 |
20 | | +4 |
21 | | +4 |
22 | | +5 | ABSTR parameter list
23 | 50FNC | 0 |
24 | 50PHAD (1) | 0 |
25 | 50PHAD (2) | 0 |
26 | 50MASAD | 0 |
27 | 50NBLOCK | 0 |
30 | 50LLGPH | LOGPH | SINTRAN III logic number conversion routine
31 | 50VERSION | 5 | NORD-50 routine version
32 | 50LPPIOF | LDPIO | SINTRAN III word fetch routine
33 | N50NO | 0 or 1# | NORD-50 number
34 | 50PBP (1) | 0 | NORD-50 BP register (power fail)
35 | 50PBP (2) | 0 | NORD-50 BP register (power fail)
36 | 50PBQ (1) | 0 | NORD-50 BQ register (power fail)
37 | 50PBQ (2) | 0 | NORD-50 BQ register (power fail)
40 | 50PBC | 0 | NORD-50 break condition (power fail)
41 | 50FLA | 0 | NORD-50 flag
42 | 50HOU | 0 | NORD-50 head of task queue
43 | 50SWI (1) | 0 |
44 | 50SWI (2) | 0 |
45 | 50PWF (1) | 0 | Task entry points
46 | 50PWF (2) | 0 |
47 | 50PC (1) | 0 |
50 | 50PC (2) | 0 |

ND-60.112.01

---

## Page 40

# Task Functions Overview

| Line | Code   | Value | Description                           |
|------|--------|-------|---------------------------------------|
| 51   | 50RUN  | 0     | Running task                          |
| 52   | 55TABU | 50TSB#| Task descriptor buffer                |
| 53   | 55FUN  | 0     | Parameters to level 12                |
| 54   | 50TSK  | 0     | Parameters to level 12                |
| 55   | LL12C  | L50CH#| Level 12 entry points for task function |
| 56   | LL50SL | SLV12#| SINTRAN III address of SLV12          |
| 57-70|        | 0     | Not used                              |
| 71   | 50TSB  | 0     | Start of task descriptor (if used)    |

`# = for NORD-50 no. 1 (DFN50)`

---

**ND-60.112.01**

Scanned by Jonny Oddene for Sintran Data © 2020

---

## Page 41

# Data Field Layout

Library Mark: see SIN1-GEN  
Name: DFx

## Description
**DF Data Fields (DMA File Transport)**

### Table of Contents

| Relat. Addr. | Symbol  | Contents | Explanation                                      |
|--------------|---------|----------|--------------------------------------------------|
| 0            | RESLINK | 0        | Reservation link                                 |
| 1            | RTRES   | 0        | Reserving RT program                             |
| 2            | BWLINK  | *−2      | Beginning of wait queue                          |
| 3            | TYPRING | 0        | Device type bits and ring                        |
| 4            | ISTATE  | 0        | 0 = idle, 1 = busy, −1 = no wait mode            |
| 5            | MLINK   | 0        | Monitor queue link                               |
| 6            | MFUNC   | 0        | Monitor level function address                   |
| 7            | ZPREG   | 0        |                                                  |
| 10           | ZXREG   | 0        |                                                  |
| 11           | ZTREG   | 0        |                                                  |
| 12           | ZAREG   | 0        |                                                  |
| 13           | ZDREG   | 0        | Register block                                   |
| 14           | ZLREG   | 0        |                                                  |
| 15           | ZSREG   | 0        |                                                  |
| 16           | ZBREG   | 0        |                                                  |
| 17           | OLDPAG  | 0        | Actual priority of calling program               |
| 20           | IOLOG   | 0        | Logical number                                   |
| 21           | WFLAG   | 0        | Original waiting flag                            |
| 22           | ICORAD  | 0        | Memory address                                   |
| 23           | IBLOAD  | 0        | Block address                                    |
| 24           | IMAXW   | 0        | Number of words                                  |
| 25           | IFUNC   | 0        | Function code                                    |
| 26           | IRETW   | 0        | Returned record length (magnetic tape) pointer   |
| 27           | MTFLG   | 0        | MON 144 flag                                     |
| 30           | MRSTA   |          |                                                  |
| 31           | SMRSTA  | 0        | Start address of routine                         |
| 31           | SSREG   | 0        | Originator                                       |
| 32           | STRSEG  | 0        | Segments of originator                           |
| 33           | DRT     | RWRTrx   | Processing RT program                            |
| 34           | MCLRG   | 0        | Return address for MC144                         |
| 35           | 0       |          |                                                  |
| 36           | 0       |          |                                                  |
| 37           | 0       |          |                                                  |
| 40           | 0       |          |                                                  |
| 41           | 0       |          |                                                  |
| 42           | 0       |          |                                                  |
| 43           | 0       |          |                                                  |
| 44           | TRTm    | LDA * 3  |                                                  |
| 45           | JMP|1   |          |                                                  |
| 46           | COMMO   |          |                                                  |
| 47           | DFx     |          |                                                  |

ND-60.112.01

*Scanned by Jonny Oddene for Sintran Data © 2020*

---

## Page 42

# DATA FIELD LAYOUT

Library Mark:  
8DLP4  
8DVE4  

Name:  
DILPx  

## Description

Line Printer n, Input

| Relat. Addr. | Symbol  | Contents | Explanation                                                 |
|--------------|---------|----------|-------------------------------------------------------------|
| -1           | ADBHEAD | 0        | Buffer head address                                         |
| 0            | RESLINK | 0        | Reservation link                                            |
| 1            | RTRES   | 0        | Reserving RT program                                        |
| 2            | BWLINK  | *—2      | Beginning of waiting queue                                  |
| 3            | TYPRING | 112000   | Device type bits and ring                                   |
| 4            | ISTATE  | 0        | 0 = idle, 1 = busy, -1 = no wait mode                       |
| 5            | MLINK   | 0        | Monitor queue link                                          |
| 6            | MFUNC   | IORES    | Monitor level function address                              |
| 7            | IOTRANS | CLPUT    | Called from INBT/OUTBT to transfer                          |
| 10           | STDEV   | TEXIT    | Start device                                                |
| 11           | SETDV   | CLCL0    | IOSET routine                                               |
| 12           | DFOPP   | 0        | Opposite data field for two-way devices                     |
| 13           | DERROR  | 0        | Error code                                                  |
| 14           | BUFST   | 0        | Start of ring buffer                                        |
| 15           | MAX     | 1000     | Buffer capacity                                             |
| 16           | BHOLD   | 0        | Number of characters in buffer                              |
| 17           | HENTE   | 0        | Fetch pointer                                               |
| 20           | CFREE   | 1000     | Free positions                                              |
| 21           | FYLLE   | 0        | Store pointer                                               |
| 22           | CLOGDV  | 1167#    | Logical unit number for DMA data field                      |
| 23           | DFDEV   | 1170#    | Logical unit number for DF data field                       |
| 24           | LREGC   |          | Return address after IOTRANS is executed                    |
| 25           | CASUN   | 0        | Device unit number                                          |
| 26           | CERROR  | 0        | Current error code                                          |
| 27           | LASTC   | 0        | Current character                                           |
| 30           | NOWRE   | 0        | Number of characters to read/write                          |
| 31           | CPARM (5) | 1, *—3, *—6, *—4, 0 | Parameter list for MTRANS (including first word in memory buffer address)  |
| 36           | MABUF   | 0        | Second word in memory buffer address                        |
| 37           | VEFUNC  | 11       | Modus if versacte (10 = plot, 11 = print)                   |
| 40           | CIOLOG  | 5        | Logical device number of I/O data field                     |
| 41           | MDATAF  | DMLPX    | Address of DMA data field                                   |

# = for line printer 1 (x = 1)

---

## Page 43

# Data Field Layout

**Library Mark:** 8LPx  
**Name:** DLPR / DLPRX

## Description

Line Printer, Character Orientation

| Relat. Addr. | Symbol   | Contents   | Explanation                           |
|--------------|----------|------------|---------------------------------------|
| -10          | LAST     | 0          |                                       |
| -7           | TMSUB    | TTOMR      |                                       |
| -5           | TMR      | 0          |                                       |
| -4           | TMR      | -3         |                                       |
| -3           | HDEV     | IOX 430#   |                                       |
| -2           | STDRIV   | DWRIT      |                                       |
| -1           | DRIVER   | DWRIT      |                                       |
| 0            | RESLINK  | 0          |                                       |
| 1            | RTRES    | 0          |                                       |
| 2            | BWLINK   | *-2        |                                       |
| 3            | TYPRING  | 110000     |                                       |
| 4            | ISTATE   | 0          |                                       |
| 5            | MLINK    | 0          |                                       |
| 6            | MFUNC    | IORES      | see DPNCH, paper tape punch           |
| 7            | IOTRANS  | TTPUT#     |                                       |
| 10           | STDEV    | DMOUT      |                                       |
| 11           | SETDV    | CEXIT      |                                       |
| 12           | DFOPP    | 0          |                                       |
| 13           | DERROR   | 0          |                                       |
| 14           | BUFS     | BUFO + BUF |                                       |
| 15           | MAX      | 120 + 120  |                                       |
| 16           | BHOLD    | 0          |                                       |
| 17           | HENTE    | 0          |                                       |
| 20           | CFREE    | 120 + 120  |                                       |
| 21           | FYLLE    | 0          |                                       |
| 22           | MINBHOLD | 120        |                                       |

**Note:** `#` = for line printer 1 (DLPR)

ND-60.112.01

---

## Page 44

# DATA FIELD LAYOUT

**Library Mark**: 8FI6  
**Name**: DMAx  

## Description
Universal DMA Interface

| Relat. Addr. | Symbol  | Contents | Explanation                      |
|--------------|---------|----------|----------------------------------|
| -36          | DERRC   | -4       |                                  |
| -35          | BUSFL   | 0        |                                  |
| -34          | SVLCA   | 0        |                                  |
| -33          | SVLWC   | 0        |                                  |
| -32          | TRG     | 0        |                                  |
| -31          | ARG     | 0        |                                  |
| -30          | DRG     | 0        |                                  |
| -27          | XRG     | 0        |                                  |
| -26          | CTRG    | 0        |                                  |
| -25          | CARG    | 0        |                                  |
| -24          | CDRG    | 0        |                                  |
| -23          | CXRG    | 0        |                                  |
| -22          | ERCNT   | 0        |                                  |
| -21          | SERRB   | 0        |                                  |
| -20          | WERRB   | 0        |                                  |
| -17          | AERRB   | 0        |                                  |
| -16          | TACNS   | -2       |                                  |
| -15          | TACOUNT | 0        |                                  |
| -14          | COMFL   | 0        |                                  |
| -13          | BLSZ    | 0        |                                  |
| -12          | TRNSF   | DMATE    | see DRFIE                        |
| -11          | BUSY    | DMABU    |                                  |
| -10          | FINICH  | DMAFI    |                                  |
| -7           | ERROR   | DMAFE    |                                  |
| -6           | EMSUB   | 0        |                                  |
| -5           | TMR     | 0        |                                  |
| -4           | TTMR    | -3       |                                  |
| -3           | HDLIV   | 1400     |                                  |
| -2           | STDRIV  | DMAUI    |                                  |
| -1           | DRIVER  | 0        |                                  |
| 0            | RESLINK | 0        |                                  |
| 1            | RTRES   | 0        |                                  |
| 2            | BWLINK  | -2       |                                  |
| 3            | TTYRING | 0        |                                  |
| 4            | ISTATE  | 0        |                                  |
| 5            | MLINK   | 0        |                                  |
| 6            | MFUNC   | RETRA    |                                  |
| 7            | TRLREG  | 0        |                                  |
| 10           | HSTAT   | 0        |                                  |
| 11           | MTRANS  | DMATR    |                                  |
| 12           |         | 0        | Not used                         |
| 13           | DMAPR   | DMPR1    | RT program for memory transfer   |
| 14           | MEMA1   | 0        |                                  |
| 15           | MEMA2   | 0        |                                  |
| 16           | CMAD1   | 0        | see DRFILE                       |
| 17           | CMAD2   | 0        |                                  |
| 20           | 0       |          | Not used                         |

ND-60.112.01

*Scanned by Jonny Oddene for Sintran Data © 2020*

---

## Page 45

# Data Field Layout

Library Mark: 8DLPx  
Name: 8DVEx  
DMLPx  

## Description

Line Printer DMA/Versatec Parallel Int.

### Relat.

| Addr. | Symbol  | Contents  | Explanation                                            |
|-------|---------|-----------|--------------------------------------------------------|
| –42   | CERRCODE| 0         | Current error code                                     |
| –41   | VEFLG   | nDVen     | 1 = Versatec, 0 = line printer                         |
| –40   | VEMOD   | 0         | Function code if not write on Versatec                 |
| –37   | CHCONV  | COCHB     | Address of character converting table                  |
| –36 – |         |           | Not used                                               |
| –25   |         |           |                                                        |
| –24   | CXMAX   | 400       | Maximum number of bytes in extra buffer                |
| –23   | CXBHOLD | 0         | Actual number of bytes in extra buffer                 |
| –22   | CXBUFST | BUF0 + BUF| Address of extra buffer                                |
| –21   | CXHENTE | (2), 0, DMLPH | HENTE pointer in extra buffer                      |
| –20   | CIOXC   | 4         | Code to set out in IOX write control                   |
| –17   | IOXWC   | IOX433#   | Code for IOX write control                             |
| –16   | IOXRS   | IOX432#   | Code for IOX read status                               |
| –15   | IOXWD (2)| IOX431#  | Code for IOX write data                                |
| –13   | BLSZ    | 0         | Not used                                               |
| –12   | LP5MF   | THDLF     | Address of monitor function routine                    |
| –11   | CBHOLD  | 0         | Current number of bytes in device buffer               |
| –10   | CHENTE  | 0         | HENTE pointer in device buffer                         |
|  –7   | ERR0R   | 0         | Error return from driver                               |
|  –6   | TMSUB   | DLPTM     | Time-out subroutine                                    |
|  –5   | TMR     | 0         | Time-out counter                                       |
|  –4   | TMR     | –3        | Start value of TMR                                     |
|  –3   | HDEV    | IOX430#   | IOX instruction (i.e., IOX 430)                        |
|  –2   | STDRIV  | TLPRI     | Start address of driver                                |
|  –1   | DRIVER  | CLP10     | Restart address after interrupt                        |
|   0   | RESLINK | 0         | Reservation link                                       |
|   1   | RTRES   | 0         | Reserving RT program                                   |
|   2   | BWLINK  | *_2       | Beginning of waiting queue                             |
|   3   | TYPRING | 2006      | Device type bits and ring                              |
|   4   | ISTATE  | 0         | 0 = idle, 1 = busy, –1 = no wait mode                  |
|   5   | MLINK   | 0         | Monitor queue link                                     |
|   6   | MFUNC   | RETRA     | Monitor level function address                         |
|   7   | TRLREG  | 0         | Return address on monitor level after transfer         |
|  10   | HSTAT   | 0         | Hardware status from driver                            |
|  11   | MTRANS  | MTRNS     | Monitor level routine to activate driver               |
|  12   | MRTRF   | 0         | Not used                                               |
|  13   | BREGC   | DILP1     | Address of I/O data field                              |
|  14   | MEMA1   | 0         | Not used                                               |
|  15   | MEMA2   | 0         | Not used                                               |
|  16   | CMAD1   | 0         | Not used                                               |
|  17   | CMAD2   | 0         | Not used                                               |
|  20   | CLEDV   | DMLPC     | Address of clear device routine                        |
|  21   | VEFUNC  | DILPX + 37| Address of VEFUNC in I/O data field                    |
|  22   | COCHB   |           | Start of converting table                              |

\# = line printer 1 (x = 1)

ND-60.112.01

---

## Page 46

# DATA FIELD LAYOUT

**Library Mark:** 7DPHx  
**Name:** DPHOT, DPHOx

## Description  
**Photosetter**

| Relat. Addr. | Symbol  | Contents       | Explanation       |
|--------------|---------|----------------|-------------------|
| -7           | EMPTFLAG| 0              |                   |
| -6           | TMSUB   | PSTOU          |                   |
| -5           | TMR     | 0              |                   |
| -4           | TMMR    | -60            |                   |
| -3           | HDEV    | IOX 1240#      |                   |
| -2           | STDRIV  | PSDRI          |                   |
| -1           | DRIVER  | PSDRI          |                   |
| 0            | RESLINK | 0              |                   |
| 1            | RTRES   | 0              |                   |
| 2            | BWLINK  | *–2            |                   |
| 3            | TYPRING | 110000         |                   |
| 4            | ISTATE  | 0              |                   |
| 5            | MLINK   | 0              |                   |
| 6            | MFUNC   | IORES          | see DTxW          |
| 7            | IOTRANS | TTPUT          |                   |
| 10           | STDEV   | DMOUT          |                   |
| 11           | SETDV   | PSSET          |                   |
| 12           | DFOPP   | 0              |                   |
| 13           | DERROR  | 0              |                   |
| 14           | BUFST   | BUFO + BUF     |                   |
| 15           | MAX     | 120 + 120      |                   |
| 16           | BHOLD   | 0              |                   |
| 17           | HENTE   | 0              |                   |
| 20           | CFREE   | 120 + 120      |                   |
| 21           | FULLE   | 0              |                   |
| 22           | MINBHOLD| 120            |                   |

---

# = for photosetter 1

ND-60.112.01

---

## Page 47

# DATA FIELD LAYOUT

Library Mark | Name
--- | ---
8PUN1 | DPUNCH
 | DPUN2

## Description
Paper Tape Punch

| Relat. Addr. | Symbol | Contents | Explanation |
|---|---|---|---|
| — 10 | | 0 | |
| — 7 | LAST | 0 | Not used |
| — 6 | TMSUB | TTOMR | Time-out subroutine |
| — 5 | TMR | 0 | Time-out counter |
| — 4 | TTM | −3 | Start value of TMR |
| — 3 | HDEV | IOX 410# | IOX instruction (i.e., IOX 410) |
| — 2 | STDRIV | DWRIT | Start address of driver |
| — 1 | DRIVER | DWRIT | Restart address after interrupt |
| 0 | RESLINK | 0 | Reservation link |
| 1 | RTRES | 0 | Reserving RT program |
| 2 | BWLINK | *−2 | Beginning of waiting queue |
| 3 | TYPRING | 110000 | Device type bits and ring |
| 4 | ISTATE | 0 | 0 = idle, 1 = busy, −1 = no wait queue |
| 5 | MLINK | 0 | Monitor queue link |
| 6 | MFUNC | IORES | Monitor level function address |
| 7 | IOTRANS | TTPUT | Called from INBT/OUTBT to transfer |
| 10 | STDDEV | DMOUT | Start device |
| 11 | SETDV | CEXIT | IOSET routine |
| 12 | DFOPP | 0 | Not used |
| 13 | DERROR | 0 | Error code |
| 14 | BUFST | BUFO + BUF | Start of ring buffer |
| 15 | MAX | 100 + 100 | Buffer capacity |
| 16 | BHOLD | 0 | Number of characters in buffer |
| 17 | HENTE | 0 | Fetch pointer |
| 20 | CFREE | 100 + 100 | Free positions |
| 21 | FYLLE | 100 | Store pointer |
| 22 | MINBHOLD | 8PUNn | Lower limit for break |

\# = for punch / (DPNCH)

---

ND-60.112.01

---

## Page 48

# DATA FIELD LAYOUT

Library Mark | Name
--- | ---
8REA1 | DREA1
8REA2 | DREA2

Description  
Paper Tape Reader

| Relat. Addr. | Symbol  | Contents             | Explanation                               |
|-------------|---------|----------------------|-------------------------------------------|
| -7          | LAST    |                      | Not used                                  |
| -6          | TMSUB   | DTAPT                | Time-out subroutine                       |
| -5          | TMR     |                      | Time-out counter                          |
| -4          | TTMR    | -2                   | Start value of TMR                        |
| -3          | HDEV    |                      | IOX instruction (i.e., IOX 400)           |
| -2          | STDRIV  | DTAPR                | Start address of driver                   |
| -1          | DRIVER  | WT12                 | Restart address after interrupt           |
| 0           | RESLINK | 0                    | Reservation link                          |
| 1           | RTRES   | 0                    | Reserving RT program                      |
| 2           | BWLINK  | *--2                | Beginning of wait queue                   |
| 3           | TYPRING | 110000               | Device type bits and ring                 |
| 4           | ISTATE  | 0                    | 0 = idle, 1 = busy, -1 = no wait mode     |
| 5           | MLINK   | 0                    | Monitor queue link                        |
| 6           | MFUNC   | IORES                | Monitor level function address            |
| 7           | IOTRANS | TRGET                | Called from INBT/OUTBT to transfer        |
| 10          | STDEV   | RSTDE                | Start device                              |
| 11          | SETDV   | CLBUF                | IOSET routine                             |
| 12          | DFOPP   | 0                    | Not used                                  |
| 13          | DERROR  | 0                    | Error code                                |
| 14          | BUFST   | BUF0 + BUF           | Start of ring buffer                      |
| 15          | MAX     | 100 + 100            | Buffer capacity                           |
| 16          | BHOLD   | 0                    | Number of characters in buffer            |
| 17          | HENTE   | 0                    | Fetch pointer                             |
| 20          | CFREE   | 100 + 100            | Free positions                            |
| 21          | FYLLE   | 0                    | Store pointer                             |
| 22          | MINBHOLD| 40                   | Lower limit for break                     |
| 23          | MAXBHOLD| 100                  | Upper limit for break                     |

ND-60.112.01

---

## Page 49

# DATA FIELD LAYOUT

**Library Mark**  
8D1  
8D2  

**Name**  
DRF1E  
DRF1E  

**Description**  
Disk (10 Mb)

## Relat.

| Addr. | Symbol | Contents | Explanation |
|-------|--------|----------|-------------|
| -36   | DERC   | -4       | Number of consecutive errors before clear device |
| -35   | BUSFL  | 0        | Transfer flag (1 = transfer started) |
| -34   | SVLCA  | 0        | Last memory address used |
| -33   | SVLWVC | 0        | Last word counter used |
| -32   | TRG    | 0        | Register when calling driver (real = TADRG) |
| -31   | ARG    | 0        | Register when calling driver (real = TADRG) |
| -30   | DRG    | 0        | Register when calling driver (real = TADRG) |
| -27   | XRG    | 0        | Register when calling driver (real = TADRG) |
| -26   | CTRG   | 0        | Register when calling driver first time (real = CTRG) |
| -25   | CARG   | 0        | Register when calling driver first time (real = CTRG) |
| -24   | CDRG   | 0        | Register when calling driver first time (real = CTRG) |
| -23   | CXRG   | 0        | Register when calling driver first time (real = CTRG) |
| -22   | ERCNT  | 0        | Number of error returns from driver |
| -21   | SERRB  | 0        | Serious error bits |
| -20   | WERRB  | 20       | Write back bits |
| -17   | AERRB  | 0        | Accumulated error bits |
| -16   | TACNS  | -40      | Number of retrials wanted for before error messages |
| -15   | TACOUNT| 0        | Retrial counter |
| -14   | COMFL  | 0        | Compare flag |
| -13   | BLSZ   | 200      | Block size |
| -12   | TRNSF  | CDISK    | Driver address |
| -11   | BUSY   | BUSYE    | Busy return |
| -10   | FINISH | COOPT    | Transfer finished |
| -7    | ERROR  | PFEIL    | Error return from driver |
| -6    | TMSUB  | MTMRS    | Time-out subroutine |
| -5    | TMR    | 0        | Time-out counter |
| -4    | + TMR  | ---4     | Start value of TMR |
| -3    | HDEV   | 500#     | Hardware device number (i.e., IOX 1540) |
| -2    | STDRIV | CTRDI    | Start address of driver |
| -1    | DRIVER | 0        | Restart address after interrupt |
| 0     | RESILNK| 0        | Reservation link |
| 1     | RTRES  | 0        | Reserving RT program |
| 2     | BWLINK | *-2      | Beginning of waiting queue |
| 3     | TYPING | 2        | Device type bits and ring |
| 4     | ISTATE | 0        | 0 = idle, 1 = busy, -1 = no wait mode |
| 5     | MLINK  | 0        | Monitor queue link |
| 6     | MFUNC  | RETRA    | Monitor level function address |
| 7     | TRLREG | 0        | Return address on monitor level after transfer |
| 10    | HSTAT  | 0        | Hardware status from driver |
| 11    | MTRANS | MTRNS    | Monitor level routine to activate driver |
| 12    | TMFRDG | 0        | Not used |
| 13    | BREGC  | 0        | Not used |
| 14    | MEMA1  | 0        | Initial memory address |
| 15    | MEMA2  | 0        | Initial memory address |
| 16    | CMADI  | 0        | Current memory address |
| 17    | CMAD2  | 0        | Current memory address |

\# = for disk number 1 (DRF1E)

---

## Page 50

# DATA FIELD LAYOUT

Library Mark: 7TEx  
Name: DTELx  

## Description

### TET Input

| Relat. Addr. | Symbol  | Contents  | Explanation          |
|--------------|---------|-----------|----------------------|
| -15          | BRKMAX  | 0         |                      |
| -14          | TSPEED  | -1        |                      |
| -13          | CNTEG   | 5         |                      |
| -12          | DFLAG   | 0         |                      |
| -11          | ECHOTAB | 0         |                      |
| -10          | BRKTAB  | 0         |                      |
| -7           | LAST    | 0         |                      |
| -6           | TMSUB   | TTIMR     |                      |
| -5           | TMR     | -3        |                      |
| -4           | TTMR    | -3        |                      |
| -3           | HDEV    | IOX 1370# |                      |
| -2           | STDRIV  | TELIN     |                      |
| -1           | DRIVER  | TELEN     |                      |
| 0            | RESLINK | 0         |  |                      |
| 1            | RTRES   | 0         | see DTxxR            |
| 2            | BWLINK  | -*2       |                      |
| 3            | TYPRING | 110000    |                      |
| 4            | ISTATE  | 0         |                      |
| 5            | MLINK   | 0         |                      |
| 6            | MFUNC   | IORES     |                      |
| 7            | IOTRANS | TRGET     |                      |
| 10           | STDEV   | TEXIT     |                      |
| 11           | SETDV   | CLBUF     |                      |
| 12           | DFPOP   | 0         |                      |
| 13           | DERROR  | 0         |                      |
| 14           | BUFST   | BUF0 + BUF|                      |
| 15           | MAX     | 40 + 40   |                      |
| 16           | BHOLD   | 0         |                      |
| 17           | HENTE   | 0         |                      |
| 20           | CFREE   | 40 + 40   |                      |
| 21           | FYLLE   | 0         |                      |

\# = for TET input number 1 (x = 1)

ND-60.112.01

---

## Page 51

# DATA FIELD LAYOUT

**Library Mark**  
TTEx  

**Name**  
DTLxW  

## Description  
TET Output  

| Relat. Addr. | Symbol  | Contents    | Explanation                |
|--------------|---------|-------------|----------------------------|
| -10          | SCREEN  | 0           |                            |
| -7           | EMPTFLAG| 1           |                            |
| -6           | TMSUB   | TTOMR       |                            |
| -5           | TMR     | 0           |                            |
| -4           | TTM7    | -10         |                            |
| 3            | HDEV    | IOX 1374#   |                            |
| -2           | STDRIV  | DWRIT       |                            |
| -1           | DRIVER  | DWRIT       |                            |
| 0            | RESLINK | 0           |                            |
| 1            | RTRES   | 0           |                            |
| 2            | BVWLINK | *–2         |                            |
| 3            | TYPRING | 110000      | see DTxxW                  |
| 4            | ISTATE  | 0           |                            |
| 5            | MLINK   | 0           |                            |
| 6            | MFUNC   | IORES       |                            |
| 7            | IOTRANS | TTPUT       |                            |
| 10           | STDEV   | DMOUT       |                            |
| 11           | SETDV   | CLBUF       |                            |
| 12           | DFOPP   | DTLxR       |                            |
| 13           | DERROR  | 0           |                            |
| 14           | BUFS    | BUF0 + BUF  |                            |
| 15           | MAX     | 40 + 40     |                            |
| 16           | BHOLD   | 0           |                            |
| 17           | HENTE   | 0           |                            |
| 20           | CFREE   | 40 + 40     |                            |
| 21           | FYLLE   | 0           |                            |
| 22           | MINBHOLD| 40          |                            |
| 23           | ROFIL   | 0           |                            |
| 24           | BCHOSTS | 0           |                            |

\# = for TET number 1 (x = 1)  

**ND-60.112.01**

---

## Page 52

# DATA FIELD LAYOUT

Library Mark: see SiN1-GEN  
Name: DTxxR  

**Description**  
Terminal, inbyte

| Relat. Addr. | Symbol     | Contents | Explanation                                 |
|--------------|------------|----------|---------------------------------------------|
| —41          | PECH7 (10) |          | Echo table 7                                |
| —31          | PBRK7 (10) |          | Break table 7                               |
| —21          | ROUSPEC    |          | Address of special subroutine               |
| —20          | NCBRK      |          | Number of characters after last break       |
| —17          | CTTYP      |          | Terminal type                               |
| —16          | CESC       | 33       | Escape character                            |
| —15          | BRKMAX     |          | Maximum BHOLD before break                  |
| —14          | TSPEED     | see table| Terminal speed                              |
| —13          | CNTREG     | 44005    | Control register                            |
| —12          | DFLAG      |          | Flag bits                                   |
| —11          | ECHOTAB    | ECH0     | Echo table                                  |
| —10          | BRKTAB     | BRK0     | Break table                                 |
| — 7          | LAST       |          | Last typed character                        |
| — 6          | MSUB       | TTIMR    | Time-out subroutine                         |
| — 5          | TMR        |          | Time-out counter                            |
| — 4          | TFM        | —3       | Start value of TMR                          |
| — 3          | HDEV       |          | IOX instruction                             |
| 2            | STDRIV     | STTIN    | Start address of driver                     |
| 1            | DRIVER     | TYENT    | Restart address after interrupt             |
| 0            | RESLINK    | 0        | Reservation link                            |
| 1            | RTRES      | 0        | Reserving RT program                        |
| 2            | BWLINK     | *— 2     | Beginning of waiting queue                  |
| 3            | TYPRING    | 110004   | Device type bits and ring                   |
| 4            | ISTATE     | 0        | 0 = idle, 1 = busy, — 1 = no wait mode      |
| 5            | MLINK      | 0        | Monitor queue link                          |
| 6            | MFUNC      | IORES    | Monitor level function address              |
| 7            | TOTRANS    | TTGET    | Called from INBT/OUTBT to transfer          |
| 10           | STDEV      | TEXIT    | Start device                                |
| 11           | SETDV      | CLBUF    | IOSET routine                               |
| 12           | DFOPP      | DTxxW    | Opposite data field for two-way devices     |
| 13           | DERROR     | 0        | Error code                                  |
| 14           | BUFST      | BUF0 + BUF | Start of ring buffer                      |
| 15           | MAX        | 40 + 40  | Buffer capacity                             |
| 16           | BHOLD      | 0        | Number of characters in buffer              |
| 17           | HENT0      |          | Fetch pointer                               |
| 20           | CFREE      | 40 + 40  | Free positions                              |
| 21           | FYLLE      | 0        | Store pointer                               |
| 22           | BSTATE     | see below| Background program state                    |
| 23           | TSTATE     | see below| Time slice state                            |
| 24           | DBPROG     | BAKxx    | Background RT program                       |
| 25           | DBADDR     | 0        | Saved P register on escape and file system monitor call |
| 26           | RIFIL      | 0        | For MODE (input file no.)                   |
| 27           | BCHISTS    | 0        | For MODE (input status = 0: output file cannot be accessed) |
| 30           | DERO (2)   | 0        | Register block                              |
| 32           | DER2 (6)   | 0        | Register block                              |
| 40           | DBREGBLOCK | = DERO   | Register save on escape                     |
| 40           | DBPPREG    | 0        | P register on page fault on IOBT level      |
| 41           | DBCTPRI    | 0        | CCTPRI on page fault on IOBT level          |
| 42           | FLAGB      | see below| Background flag                             |

**ND-60.112.01**  

Scanned by Jonny Oddene for Sintran Data © 2020

---

## Page 53

# Values of BSTATE:

0: not logged in [5BPASSIVE]  
1: command mode (5BCOMM)  
2: user mode (5BUSER)  
3: escape is punched (5BESC)  
4: error printout (5ERROR)  
5: remote processing (5REMOT) (REMOTE)  
6: rubout is punched (5RERUB) (REMOTE)  
7: remote file transfer (5CFILTRA)  

# Value of TSTATE:

0: outside time slice (5TDUM) PRIO. 20  
1: inside time slice (5TLOW) PRIO. 40  
2: after break character (5THIGH) PRIO. 60  
3: after break character (PRIO. not yet raised) (5TREACT)  
4: medium priority (5CONT) PRIO. 50  
5: waiting for medium priority (5WCONT) PRIO. 50  

# FLAGB (bit):

| Bit | Description                                |
|-----|--------------------------------------------|
| 0   | 5NOSLICE - run with fixed prio.            |
| 1   | 5ESC2ON - escape allowed in command mode   |
| 2   | 5ESC2SET - escape has been typed but no served  |
| 3   | 5LOGOUT - log out on missing carrier       |
| 4   | 5ABJOB - abort job (ERROR)                 |
| 5   | 5OLBRK - missing carrier on output         | 

ND.60.112.01

---

## Page 54

# Data Field Layout
Library Mark: see SIN1-GEN  
Name: DTxxW  

## Description
Terminal, Out Byte

### Relation Table

| Relat. Addr. | Symbol   | Contents      | Explanation                                               |
|--------------|----------|---------------|-----------------------------------------------------------|
| -10          | SCREEN   | 0             | Counter for stop on full page                             |
| -7           | EMPTFLAG | 1             | = 0 when buffer empty and print of last character finished |
| -6           | TMSUB    | TTOMR         | Time-out subroutine                                       |
| -5           | TMR      | 0             | Time-out counter                                          |
| -4           | TTM      | -10           | Start value of TMR                                        |
| -3           | HDEV     | IOX 310 + 4#  | IOX instruction                                           |
| -2           | STDRIV   | DWRIT         | Start address of driver                                   |
| -1           | DRIVER   | DWRIT         | Restart address after interrupt                           |
| 0            | RESLINK  | 0             | Reservation link                                          |
| 1            | RTRES    | 0             | Reserving RT program                                      |
| 2            | BWLINK   | *—2           | Beginning of wait queue                                   |
| 3            | TYPRING  | 110000        | Device type bits and ring                                 |
| 4            | ISTATE   | 0             | 0 = idle, 1 = busy, — 1 = no wait mode                    |
| 5            | MLINK    | 0             | Monitor queue link                                        |
| 6            | MFUNC    | IORES         | Monitor level function address                            |
| 7            | IOTRANS  | TTPUT         | Called from INBT/OUTBT to transfer                        |
| 10           | STDEV    | DMOUT         | Start device                                              |
| 11           | SETDV    | CLBUF         | IOSET routine                                             |
| 12           | OFPOP    | DTxxR         | Opposite data field for two-way devices                   |
| 13           | DERROR   | 0             | Error code                                                |
| 14           | BUFST    | BUFO + BUF    | Start of ring buffer                                      |
| 15           | MAX      | 40 + 40       | Buffer capacity                                           |
| 16           | BHOLD    | 0             | Number of characters in buffer                            |
| 17           | HENTE    | 0             | Fetch pointer                                             |
| 20           | CFREE    | 40 + 40       | Free positions                                            |
| 21           | FYLLE    | 0             | Store pointer                                             |
| 22           | MINBHOLD | 40            | Lower limit for break                                     |
| 23           | ROFIL    | 0             | For MODE (output file number)                             |
| 24           | BCHOSTS  | 0             | For MODE (output status = 0, output file cannot be accessed, > < 0: otherwise) |
| 26           | CBUADR   | 0             | Current user buffer address (OUTSTRING)                   |
| 27           | NOCHAR   | 0             | Number of bytes in OUTSTRING                              |
| 30           | CNOCHAR  | 0             | Number of words left to transfer (OUTSTRING)              |
| 31           | XNOCHAR  | 0             | Working location (OUTSTRING)                              |
| 32           | ZOAPRG   | 0             | P, X, and T registers in OUTSTRING                        |
| 35           | ZOARG    | 0             | A, D and L registers in OUTSTRING                         |
| 40           | ZOSRG    | 0             | S and B registers and OLD PAGE in OUTSTRING               |
| 43           | SBHOLD   | 0             | Saved BHOLD in OUTSTRING                                  |

*26-43 only if background  
# = For terminal 1 (xx = 01)  

ND-60.112.01

Scanned by Jonny Oddene for Sintran Data © 2020

---

## Page 55

# DATA FIELD LAYOUT

**Library Mark:** 8FDIx  
**Name:** FDIDx

**Description:** DMA Floppy Disk

| Relat. Addr. | Symbol | Contents | Explanation |
|--------------|--------|----------|-------------|
| –123 | SPTB | Saved page table content (page 33) |  |
| –122 | SPTB | Saved page table content (page 34) |  |
| –121 | FCNT3 | Used by RT program (word count) |  |
| –120 | FCNT4 | Word counter used by RT program (–WDCNT) |  |
| –117 | FDIS | Temporary hardware status |  |
| –116 | MRETURN | Address of the 3rd parameter (format number) |  |
| –115 | FTRY | Retrail counter for address mismatch |  |
| –114 | DINCR | Number of sectors transferred |  |
| –113 |  | Link for monitor queue (MLINK) |  |
| –112 | MFDIS | Address of monitor function routine (MFUNC) |  |
| –111 | FRTPR | RT program to fill or empty the buffer |  |
| –110 | WDCNT | Word/sector used by the RT program (FDIRT) |  |
| –107 | FDIMOD | Disk address modifier (–1 = rev. to EOF, 1 = addr. to EOF) |  |
| –106 | FCNT1 | Maximum number of sectors in buffer |  |
| –105 | FCNT2 | Counter for sectors in buffer |  |
| –104 | DFPLG | Routine switch in driver |  |
| –103 | DATAF | Current format number |  |
| –102 | SECT | Sector address |  |
| –101 | SCTIB | Number of sectors in buffer |  |
| –100 | FDIFORM (3) | Hardware format (format 0, 1, 2) |  |
| –75 | FLADDR (3) | Last legal disk address (format 0, 1, 2) |  |
| –72 | OLDTR (3) | Old track number (unit 0, 1, 2) |  |
| –67 | NEWTR (3) | New track number (unit 0, 1, 2) |  |
| –64 | WDSCTR (3) | Word/sector (format 0, 1, 2) |  |
| –61 | MSCIB (3) | Maximum number of sectors allowed in buffer (format 0, 1, 2) |  |
| –56 | SCTTR (3) | Sectors/track (format 0, 1, 2) |  |
| –53 | CALIB (3) | Calibration flag (unit 0, 1, 2) |  |
| –50 | NFIDADR (3) | Next disk address (unit 0, 1, 2) |  |
| –45 | SHSTAT (3) | Saved last status (unit 0, 1, 2) |  |
| –42 | CERCODE | Error code |  |
| –41 | MAXUNIT | Maximum unit number on this controller |  |
| –40 | FDRIVE | Current drive number |  |
| –37 |  | Not used |  |
| –36 |  | Not used |  |
| –35 | BUSFL | Transfer flag (1 = transfer started) |  |
| –34 | SVLCA | Not used |  |
| –33 | CFRLG | Return address inside TFDISK |  |
| –32 | TRG | Register when calling driver (real = TADRG) |  |
| –31 | ARG | Register when calling driver (real = TADRG) |  |
| –30 | DRG | Register when calling driver (real = TADRG) |  |
| –27 | XRG | Register when calling driver (real = TADRG) |  |
| –26 | CTRG | Register when calling driver first time (real = CTRG) |  |
| –25 | CARG | Register when calling driver first time (real = CTRG) |  |
| –24 | CDRG | Register when calling driver first time (real = CTRG) |  |
| –23 | CXRG | Register when calling driver first time (real = CTRG) |  |
| –22 | ERCNT | Number of error returns from driver |  |
| –21 | SERRB | Serious error bits |  |
| –20 |  | Not used |  |

**ND-60.112.01**

---

## Page 56

# Technical Details

| Code | Value  | Description                                  |
|------|--------|----------------------------------------------|
| 17   | AERRB  | Accumulated error bits                       |
| 16   | TACNS  | --12  | Number of retries wanted before error message |
| 15   | TACOUNT| Retrial counter                               |
| 14   | COMPL  | Not used                                     |
| 13   | BLSZ   | Not used                                     |
| 12   | TRNSF  | FDISK | Driver address                      |
| 11   | BUSY   | FDIBU | Busy return                         |
| 10   | FINISH | FDIFI | Transfer finished                   |
| 7    | ERROR  | FDIFE | Error return from driver            |
| 6    | TMSUB  | MTMRS | Time-out subroutine                 |
| 5    | TMR    | Time-out counter                            |
| 4    | TTMR   | --10  | Start value of TMR                  |
| 3    | HDEV   | IOX instruction (i.e., IOX 1560)            |
| 2    | STDRIV | TFDISK| Start address of driver             |
| 1    | DRIVER | Restart address after interrupt             |
| 0    | RESLINK| Reservation link                            |
| 1    | RTRES  | Reserving RT program                        |
| 2    | BWLINK | Beginning of waiting queue                  |
| 3    | TYPRING| 402   | Device type bits and ring           |
| 4    | ISTATE |        | 0 = idle, 1 = busy, -1 = no wait mode |
| 5    | MLINK  | Monitor queue link                          |
| 6    | MFUNC  | RETRA | Monitor level function address      |
| 7    | TRLREG | Return address on monitor level after transfer |
| 10   | HSTAT  | Hardware status from driver                 |
| 11   | MTRANS | MTRNS | Monitor level routine to activate driver |
| 12   | MRTREF | Program calling close                       |
| 13   | BREGC  | Address of I/O data field                   |

ND-60.112.01

---

## Page 57

# Data Field Layout

Library Mark | Name
--- | ---
8FyLLx | FyUxI

## Description

Floppy Disk Input  
(y = controller, x = driver)

### Relation

| Addr. | Symbol   | Contents | Explanation                                               |
|-------|----------|----------|-----------------------------------------------------------|
| -1    | ADRBHEAD |          | Buffer head address                                       |
| 0     | RESLINK  | 0        | Reservation link                                          |
| 1     | RTRES    | 0        | Reserving RT program                                      |
| 2     | BWLINK   | *–2      | Beginning of waiting queue                                |
| 3     | TYPRING  | 112000   | Device type bits and ring                                 |
| 4     | ISTATE   | 0        | 0 = idle, 1 = busy, -1 = no wait mode                     |
| 5     | MLINK    | 0        | Monitor queue link                                        |
| 6     | MFUNC    | IORES    | Monitor level function address                            |
| 7     | IOTRANS  | CBGET    | Called from INBT/OUTBT to transfer                        |
| 10    | STDEV    | TEXIT    | Start device                                              |
| 11    | SETDV    | CALCL    | IOSET routine                                             |
| 12    | DFOPP    | FyUxO    | Opposite data field for two-way devices                   |
| 13    | DERROR   | 0        | Error code                                                |
| 14    | BUFSRT   | 0        | Start of ring buffer                                      |
| 15    | MAX      | 0        | Buffer capacity                                           |
| 16    | BHOLD    | 0        | Number of characters in buffer                            |
| 17    | HENTR    | 0        | Fetch pointer                                             |
| 20    | CFREE    | 0        | Free positions                                            |
| 21    | FYLLE    | 0        | Store pointer                                             |
| 22    | CLOGDV   | 1145#    | Logical unit number for DMA data field                    |
| 23    | DFDEV    | 1146#    | Logical unit number for DF data field                     |
| 24    | LREGC    | 0        | Return address after IOTRANS is executed                  |
| 25    | CASUN    | x        | Device unit number                                        |
| 26    | CERROR   | 0        | Current error code                                        |
| 27    | LASTC    | 0        | Current character                                         |
| 30    | NOWRE    | 0        | Number of characters to read/write                        |
| 31    | CPARM (5)| 0, *+3, *+6, *–4, 0 | Parameter list for MTRANS (including first word in memory buffer address)/# |
| 36    | MABUF    | 0        | Second word in memory buffer address                      |
| 37    | VFUNC    | 0        | Not used                                                  |
| 40    | CIOLOG   | 1000#    | Logical device number                                     |
| 41    | FDIADR   | 0        | Floppy disk address                                       |

\# = for controller 1, unit 0 (y = 1, x = 0)

---

## Page 58

# DATA FIELD LAYOUT

**Library Mark**: 8FyUx  
**Name**: FyUxO

## Description
Floppy Disk Output

| Relat. Addr. | Symbol  | Contents | Explanation                                      |
|--------------|---------|----------|--------------------------------------------------|
| \-1          | ADBRHEAD| 0        | Buffer head address                              |
| 0            | RESLINK | 0        | Reservation link                                 |
| 1            | RTRES   | 0        | Reserving RT program                             |
| 2            | BWLINK  | \*\-2    | Beginning of waiting queue                       |
| 3            | TYPRING | 112000   | Device type bits and ring                        |
| 4            | ISTATE  | 0        | 0 = idle, 1 = busy, \-1 = no wait mode           |
| 5            | MLINK   | 0        | Monitor queue link                               |
| 6            | MFUNC   | IORES    | Monitor level function address                   |
| 7            | IOTRANS | CBPUT    | Called from INBT/OUTBT to transfer               |
| 10           | STDEV   | TEXIT    | Start device                                     |
| 11           | SETDV   | CAOCL    | IOSET routine                                    |
| 12           | DFPOP   | FyUxl    | Opposite data field for two-way devices          |
| 13           | DERROR  | 0        | Error code                                       |
| 14           | BUFST   | 0        | Start of ring buffer                             |
| 15           | MAX     | 0        | Buffer capacity                                  |
| 16           | BHOLD   | 0        | Number of characters in buffer                   |
| 17           | HENTE   | 0        | Fetch pointer                                    |
| 20           | CFREE   | 0        | Free positions                                   |
| 21           | FYLIE   | 0        | Store pointer                                    |
| 22           | CLOGDV  | 1145#    | Logical unit number for DMA data field           |
| 23           | DFDEV   | 1146#    | Logical unit number for DF data field            |
| 24           | LREGC   | 0        | Return address after IOTRANS is executed         |
| 25           | CASUN   | x        | Device unit number                               |
| 26           | CERROR  | 0        | Current error code                               |
| 27           | LASTC   | 0        | Current character                                |
| 30           | NOWRE   | 0        | Number of characters to read/write               |
| 31           | CPARM (5)| 1, \*+3, \*+6, \*-4, 0 | Parameter list for MTRANS (including first word in memory buffer address)/ \# |
| 36           | MABUF   | 0        | Second word in memory buffer address             |
| 37           | VEFUNC  | 0        | Not used                                         |
| 40           | CIOLOG  | 1000#    | Logical device number                            |
| 41           | FDIADR  | 0        | Floppy disk address                              |

\# = For controller 1, drive 0 (y = 1, x = 0)

---

*ND-60.112.01*

---

## Page 59

# DATA FIELD LAYOUT

**Library Mark**  
yGBz  

**Name**  
GRBz  
GyBz  

## Description

NORCOM Graphic Buffer (y = ACM no.z = bank no.)

### Relat.

| Addr. | Symbol  | Contents | Explanation                         |
|-------|---------|----------|-------------------------------------|
| -6    | UUS0    |          | Routine for PLOTT monitor call      |
| -5    | STAAD   | 0        | Work cell                           |
| -4    | KLUMP   | 0        | Save cell                           |
| -3    | HDEV    | IOX 40#  | IOX for ACM                         |
| -2    | BCOVN   | 0        | Work cell                           |
| -1    | DRIVER  | 0        | Driver address                      |
| 0     | RESLINK | 0        |                                     |
| 1     | RTRES   | 0        |                                     |
| 2     | BWLINK  | *-2      |                                     |
| 3     | TYPRING | 10000    | see DTxxW                           |
| 4     | ISTATE  | 0        |                                     |
| 5     | MLINK   | 0        |                                     |
| 6     | MFUNC   | IORES    |                                     |
| 7     | XDIFF   | 0        | Increment difference, x coordinate  |
| 10    | YDIFF   | 0        | Increment difference, y coordinate  |
| 11    | SETDV   | SETFA    | IOSET routine                       |
| 12    | INNX    | 0        | Save X register                     |
| 13    | DERRO   | 0        | Error code                          |
| 14    | RETAD   | 0        | Return address                      |
| 15    | CURAD   | 0        | Current ACM address                 |
| 16    | CURDA   | 0        | ACM data                            |
| 17    | BATEM   | 0        | Bank number                         |
| 20    | BANKB   | 71#      | Blank position                      |
| 21    | COLOR   | 0        | Current colour                      |
| 22    | OLDX    | 0        | Previous X coordinate               |
| 23    | OLDY    | 0        | Previous Y coordinate               |
| 24    | TEXIT   | ATEXI    | Exit address                        |
| 25    | LINEG   |          | Vector generating routine           |
| 26    | ERESE   |          | Erase line routine                  |
| 27    | NOCUL   |          | Select colour routine               |
| 30    | VAR     | 0        |                                     |
| 31    | TELL    | 0        |                                     |
| 32    | REFAD   | 0        |                                     |
| 33    | TEMP    | 0        |                                     |
| 34    | REST    | 0        |                                     |
| 35    | ADRE    | 0        |                                     |
| 36    |         |          |                                     |
| 37    | XREG    | 0        |                                     |
| 40    | RESU    | 0        |                                     |
| 41    | YPAR    | 0        |                                     |
| 42    | COPO    | 0        | work area                           |
| 43    | SAVA    | 0        |                                     |
| 44    | LREG    | 0        |                                     |
| 45    | LMER    | 0        |                                     |
| 46    | INC1    | 0        |                                     |
| 47    | INC2    | 0        |                                     |
| 50    | SW1     | 0        |                                     |
| 51    | SW2     | 0        |                                     |
| 52    | INST    | 0        |                                     |
| 53    | X2      | 0        |                                     |
| 54    | Y2      | 0        |                                     |
| 55    | YSTA    | 0        |                                     |

\# = ACM no. 1, bank no. 1, (y,z) = (1,1)  
ND-60.112.01

---

## Page 60

# DATA FIELD LAYOUT

**Library Mark:** 7GRCx  
**Name:** GRxR  

## Description
Graf Cassette, Input

### Relat. Addr.
| Addr. | Symbol  | Contents    | Explanation        |
|-------|---------|-------------|--------------------|
| -13   | CNTREG  | 40005       |                    |
| -12   | DFLAG   | 20000       |                    |
| -11   | ECHOTAB | 0           |                    |
| -10   | BRKTAB  | 0           |                    |
| -7    | LAST    | 0           |                    |
| -6    | TMSUB   | GROUT       |                    |
| -5    | TMR     | 0           |                    |
| -4    | TTMR    | -100        |                    |
| -3    | HDEV    | 10X 1220#   |                    |
| -2    | STDRIV  | GRSTA       |                    |
| -1    | DRIVER  | GRCAS       |                    |
| 0     | RESLINK | 0           |                    |
| 1     | RTRES   | 0           |                    |
| 2     | BWLINK  | *−2         |                    |
| 3     | TYPRING | 110000      |                    |
| 4     | ISTATE  | 0           |                    |
| 5     | MLINK   | 0           |                    |
| 6     | MFUNC   | IOREs       | see DTxR          |
| 7     | IOTRANS | GRGET       |                    |
| 10    | STDEV   | GRGET       |                    |
| 11    | SETDV   | CLBUF       |                    |
| 12    | DFOPP   | GRxW        |                    |
| 13    | DERROR  | 0           |                    |
| 14    | BUFST   | BUFO + BUF  |                    |
| 15    | MAX     | 40 + 40     |                    |
| 16    | BHOLD   | 0           |                    |
| 17    | HENTE   | 0           |                    |
| 20    | CFREE   | 40 + 40     |                    |
| 21    | FYLLE   | 0           |                    |
| 22    | BSTATE  | 0           |                    |
| 23    | TSTATE  | 0           |                    |
| 24    | DBFROG  | 0           |                    |
| 25    | DBADDR  | 0           |                    |
| 26    | RIFIL   | 0           |                    |
| 27    | BCHISTS | 0           |                    |

\# = for Graf cassette, input no. 1 (x = 1)

**ND-60.112.01**

---

## Page 61

# DATA FIELD LAYOUT

**Description**  
Graf Cassette, Output

**Library Mark**  
7GRCx

**Name**  
GRxW

| Relat. Addr. | Symbol   | Contents    | Explanation      |
|--------------|----------|-------------|------------------|
| -10          | SCREEN   | 0           |                  |
| -7           | EMPTFLAG | 1           |                  |
| -6           | TMSUB    | TTOMR       |                  |
| -5           | TMR      | 0           |                  |
| -4           | TTMR     | -10         |                  |
| 3            | HDEV     | IOX 1224#   |                  |
| -2           | STDRIV   | DWRIT       |                  |
| -1           | DRIVER   | DWRIT       |                  |
| 0            | RESLINK  | 0           |                  |
| 1            | RTRES    | 0           |                  |
| 2            | BWLINK   | * -2        |                  |
| 3            | TYPRING  | 110000      |                  |
| 4            | ISTATE   | 0           |                  |
| 5            | MLINK    | 0           |                  |
| 6            | MFUNC    | IORES       | see DTxxW        |
| 7            | IOTRANS  | TTPUT       |                  |
| 10           | STDEV    | DMOUT       |                  |
| 11           | SETDV    | CLBUF       |                  |
| 12           | DFOPP    | GRxR        |                  |
| 13           | DERROR   | 0           |                  |
| 14           | BUFST    | BUF0 + BUF  |                  |
| 15           | MAX      | 40 + 40     |                  |
| 16           | BHOLD    | 0           |                  |
| 17           | HENTE    | 0           |                  |
| 20           | CFREE    | 40 + 40     |                  |
| 21           | FYLLE    | 0           |                  |
| 22           | MINBHOLD | 40          |                  |
| 23           | ROFIL    | 0           |                  |
| 24           | BCHOSTS  | 0           |                  |

# Note

\# = for Graf cassette, output no. 1 (x = 1)

**ND-60.112.01**

---

## Page 62

# DATA FIELD LAYOUT  

**Library Mark:** 7HARx  
**Name:** HAxR  

## Description  
Harris Photosetter, Input  

### Table

| Relat. Addr. | Symbol  | Contents   | Explanation  |
|--------------|---------|------------|--------------|
| -13          | CNTREG  | 40005      |              |
| -12          | DFLAG   | 0          |              |
| -11          | ECHOTAB | 0          |              |
| -10          | BRKTAB  | 0          |              |
| -7           | LAST    | 0          |              |
| -6           | TMSUB   | HOUT       |              |
| -5           | TMR     | 0          |              |
| -4           | TTMR    | - 100      |              |
| -3           | HDEV    | IOX 1240#  |              |
| -2           | STDRIV  | HSTDE      |              |
| -1           | DRIVER  | HARRI      |              |
| 0            | RESLINK | 0          |              |
| 1            | RTRES   | 0          |              |
| 2            | BWLINK  | * -2       |              |
| 3            | TYPRING | 110000     |              |
| 4            | ISTATE  | 0          |              |
| 5            | MLINK   | 0          |              |
| 6            | MFUNC   | IORES      | see DTxxR    |
| 7            | IOTRANS | TRGET      |              |
| 10           | STDEV   | DEVST      |              |
| 11           | SETDV   | CLBUF      |              |
| 12           | DFOPP   | 0          |              |
| 13           | DERROR  | 0          |              |
| 14           | BUFST   | BUFO + BUF |              |
| 15           | MAX     | 40 + 40    |              |
| 16           | BHOLD   | 0          |              |
| 17           | HENTE   | 0          |              |
| 20           | CFREE   | 40 + 40    |              |
| 21           | FYLLE   | 0          |              |
| 22           | BSTATE  | 40         |              |
| 23           | TSTATE  | 0          |              |
| 24           | DBPROG  | 0          |              |
| 25           | DBADDR  | 0          |              |
| 26           | RIFIL   | 0          |              |
| 27           | BCHISTS | 0          |              |

\# = Harris photosetter, input no. 1 (x = 1)

ND-60.112.01

---

## Page 63

# DATA FIELD LAYOUT

Library Mark: 7HARx  
Name: HAxW

## Description
Harris Photosetter, output

| Relat. Addr. | Symbol  | Contents | Explanation |
|--------------|---------|----------|-------------|
| -10          | SCREEN  | 0        |             |
| -7           | EMPTFLAG| 1        |             |
| -6           | TMSUB   | 0        |             |
| -5           | TMR     | 0        |             |
| -4           | TTMR    | -10      |             |
| -3           | HDEV    | IOX 1244#|             |
| -2           | STDRIV  | DWRIT    |             |
| -1           | DRIVER  | DWRIT    |             |
| 0            | RESLINK | 0        |             |
| 1            | RTRES   | 0        |             |
| 2            | BWLINK  | * -2     |             |
| 3            | TYPRING | 110000   |             |
| 4            | ISTATE  | 0        |             |
| 5            | MLINK   | 0        |             |
| 6            | MFUNC   | IORES    | see DTxW    |
| 7            | IOTRANS | TTPUT    |             |
| 10           | STDEV   | DMOUT    |             |
| 11           | SETDV   | CLBUF    |             |
| 12           | DFOPP   | HAxR     |             |
| 13           | DERROR  | 0        |             |
| 14           | BUFST   | BUF0 + BUF|            |
| 15           | MAX     | 40 + 40  |             |
| 16           | BHOLD   | 0        |             |
| 17           | HENTE   | 0        |             |
| 20           | CFREE   | 40 + 40  |             |
| 21           | FYYLE   | 0        |             |
| 22           | BSTATE  | 40       |             |
| 23           | TSTATE  | 0        |             |
| 24           | DBPROG  | 0        |             |

\# = Harris photosetter no. 1 (x = 1)

ND-60.112.01

---

## Page 64

# DATA FIELD LAYOUT

**Library Mark:** 8HDMx  
**Name:** HDFIx  

## Description
BSC-DMA Link, Monitor Call Input

| Relat. Addr. | Symbol   | Contents | Explanation  |
|--------------|----------|----------|--------------|
| \-1          | ADRBHEAD | 0        |              |
| 0            | RESLINK  | 0        |              |
| 1            | RTRES    | 0        |              |
| 2            | BWLINK   | \*2      |              |
| 3            | TYPRING  | 12200    |              |
| 4            | ISTATE   | 0        |              |
| 5            | MLINK    | 0        |              |
| 6            | MFUNC    | IORES    |              |
| 7            | IOTRANS  | TEXIT    |              |
| 10           | STDEV    | TEXIT    |              |
| 11           | SETDV    | TEXIT    |              |
| 12           | DFOPP    | HDFOx    | see MTDIx    |
| 13           | DERROR   | 0        |              |
| 14           | BUFS      | 0       |              |
| 15           | MAX      | 0        |              |
| 16           | BHOLD    | 0        |              |
| 17           | HENTE    | 0        |              |
| 20           | CFREE    | 0        |              |
| 21           | FYLLE    | 0        |              |
| 22           | CLOG     | 1303#    |              |
| 23           | DFOOP    | 1305#    |              |
| 24           | LREGC    | 0        |              |

\# = for modem 1 (x = 1)  

ND-60.112.01

---

## Page 65

# DATA FIELD LAYOUT

**Library Mark**: 8HDMx  
**Name**: HDFOx  

## Description
BSC-DMA Link Monitor Call, Output

## Table

| Relat. Addr. | Symbol  | Contents | Explanation    |
|--------------|---------|----------|----------------|
| -1           | ADRBHEAD| 0        |                |
| 0            | RESLINK | 0        |                |
| 1            | RTRES   | 0        |                |
| 2            | BWLINK  | * -2     |                |
| 3            | TYPRING | 12200    |                |
| 4            | ISTATE  | 0        |                |
| 5            | MLINK   | 0        |                |
| 6            | MFUNC   | IORES    |                |
| 7            | IOTRANS | TEXIT    |                |
| 10           | STDEV   | TEXIT    |                |
| 11           | SETDV   | TEXIT    | see MTD0x      |
| 12           | DFOPP   | HDFIx    |                |
| 13           | DERROR  | 0        |                |
| 14           | BUFST   | 0        |                |
| 15           | MAX     | 0        |                |
| 16           | BHOLD   | 0        |                |
| 17           | HENTE   | 0        |                |
| 20           | CFREE   | 0        |                |
| 21           | FYLLE   | 0        |                |
| 22           | CLOGDV  | 1304#    |                |
| 23           | DFDEV   | 1306#    |                |
| 24           | LREGC   | 0        |                |

## Notes
`#` = for modem no. 1 (x = 1)

---

ND-60.112.01

---

## Page 66

# Data Field Layout

Library Mark: 8HDMx  
Name: HDMIx  

## Description
BSC-DMA Link Controller, Input

### Table

| Relat. Addr. | Symbol  | Contents | Explanation                          |
|--------------|---------|----------|--------------------------------------|
| -44          | HCTRL   | 100      | Input control word                   |
| -43          | HMOD    |          | Not used                             |
| -42          | CERRCODE| 0        | Error code                           |
| -41          |         |          |                                      |
| -37          |         | 0        | Not used                             |
| -36          | MRETURN | 0        | Address for returning read words     |
| -35          |         | 0        | Not used                             |
| -34          |         | 0        | Not used                             |
| -33          | CLREG   | 0        |                                      |
| -32          | TRG     | 0        |                                      |
| -31          | ARG     | 0        |                                      |
| -30          | DRG     | 0        |                                      |
| -27          | XRG     | 0        | See MTFIE                            |
| -26          | CTRG    | 0        |                                      |
| -25          | CARG    | 0        |                                      |
| -24          | DDRG    | 0        |                                      |
| -23          | CXRG    | 0        |                                      |
| -22          |         |          |                                      |
| -13          |         | 0        | Not used                             |
| -12          | TRNSF   | HDMID    |                                      |
| -11          | BUSY    | HDIBU    |                                      |
| -10          | FINISH  | HDIFI    |                                      |
| -7           | ERROR   | HDIFE    |                                      |
| -6           | TMSUB   | HITMR    |                                      |
| -5           | TMR     | 0        |                                      |
| -4           | TTMR    | -6       |                                      |
| -3           | HDEV    | IOX 560# |                                      |
| -2           | STDRIV  | CTRID    |                                      |
| -1           | DRIVER  | 0        |                                      |
| 0            | RESLINK | 0        | See MTFIE                            |
| 1            | RTRES   | 0        |                                      |
| 2            | BWLINK  | * -2     |                                      |
| 3            | TYPRNG  | 202      |                                      |
| 4            | ISTATE  | 0        |                                      |
| 5            | MLINK   | 0        |                                      |
| 6            | MFUNC   | RETRA    |                                      |
| 7            | TRLREG  | 0        |                                      |
| 10           | HSTAT   | 0        |                                      |
| 11           | MTRANS  | HITRN    |                                      |
| 12           | MRTREF  | 0        |                                      |
| 13           | BREGC   | 0        |                                      |

**Note:**  
# = for modem 1 (x = 1)  

ND-60.112.01

---

## Page 67

# DATA FIELD LAYOUT

**Library Mark:** 8HDMx  
**Name:** HDMOx  

## Description
BSC-DMA Link Controller, Output

### Table

| Relat. Addr. | Symbol | Contents  | Explanation            |
|--------------|--------|-----------|------------------------|
| \-44         | HCTRL  | 100       | Output control word    |
| \-43         | HMOD   |           |                        |
| \-42         | GERRCODE | 0       |                        |
| \-41         |        |           |                        |
| \-37         |        | 0         |                        |
| \-36         | MRETURN | 0        |                        |
| \-35         |        | 0         |                        |
| \-34         |        | 0         |                        |
| \-33         | CLREG  | 0         |                        |
| \-32         | TRG    | 0         |                        |
| \-31         | ARG    | 0         |                        |
| \-30         | DRG    | 0         |                        |
| \-27         | XRG    | 0         |                        |
| \-26         | CTRG   | 0         |                        |
| \-25         | CARG   | 0         |                        |
| \-24         | CDRG   | 0         |                        |
| \-23         | CXRG   | 0         |                        |
| \-22         |        |           |                        |
| \-13         |        | 0         |                        |
| \-12         | TRNSF  | HDMOD     |                        |
| \-11         | BUSY   | HDOBU     |                        |
| \-10         | FINISH | HDOFI     |                        |
| \-7          | ERROR  | HDOFE     |                        |
| \-6          | TMSUB  | HOTMR     |                        |
| \-5          | TMR    | 0         |                        |
| \-4          | TTMR   | \-12      |                        |
| \-3          | HDEV   | IOX 560#  |                        |
| \-2          | STDRIV | CTROD     |                        |
| \-1          | DRIVER | 0         |                        |
| 0            | RESLINK| 0         |                        |
| 1            | RTRES  | 0         | see HDMlx              |
| 2            | BVWLINK| *\-2      |                        |
| 3            | TYPRING| 202       |                        |
| 4            | ISTATE | 0         |                        |
| 5            | MLINK  | 0         |                        |
| 6            | MFUNC  | RETRA     |                        |
| 7            | TRLEG  | 0         |                        |
| 10           | HSTAT  | 0         |                        |
| 11           | MTRANS | HOTRN     |                        |
| 12           | MRTREF | 0         |                        |
| 13           | BREGC  | 0         |                        |

\# = for modem no. 1 (x = 1)

ND-60.112.01

---

## Page 68

# Data Field Layout

Library Mark: IBLxx  
Name: IBxxI

## Description

Internal Device, Block Oriented Output

| Relat. Addr. | Symbol   | Contents | Explanation                       |
|--------------|----------|----------|-----------------------------------|
| -3           | CIDEVBU  | 0        | Address to buffer                 |
| -2           | INWORDS  | 0        | Number of words in buffer         |
| -1           | IDEOPP   | IDxxO    | Pointer to opposite data field    |
| 0            | RESLINK  | 0        |                                   |
| 1            | RTRES    | 0        |                                   |
| 2            | BWLINK   | *-2      |                                   |
| 3            | TYPRING  | 102      |                                   |
| 4            | ISTATE   | 0        |                                   |
| 5            | MLINK    | 0        |                                   |
| 6            | MFUNC    | 0        |                                   |
| 7            | TRLREG   | 0        |                                   |
| 10           | HSTAT    | 0        |                                   |
| 11           | MTRANS   | IMTRI    |                                   |
| 12           | TRMFLG   | 0        |                                   |
| 13           | BREGC    | 0        |                                   |

See DRFIE

ND-60.112.01

---

## Page 69

# DATA FIELD LAYOUT

Library Mark | IBLxx  
Name | IBxxO  

## Description
Internal Device, Block Oriented Output

| Relat. Addr. | Symbol | Contents | Explanation |
|--------------|--------|----------|-------------|
| -1 | IDEPOPP | IDxxI | Pointer to opposite data |
| 0 | RESLINK | 0 | |
| 1 | RTRES | 0 | |
| 2 | BWLINK | *-2 | |
| 3 | TYPRING | 102 | |
| 4 | ISTATE | 0 | |
| 5 | MLINK | 0 | see DRFIE |
| 6 | MFUNC | 0 | |
| 7 | TRLREG | 0 | |
| 10 | HSTAT | 0 | |
| 11 | MTRANS | IMTRO | |
| 12 | TRMFLG | 0 | |
| 13 | BREGC | 0 | |

ND-60.112.01

---

## Page 70

# DATA FIELD LAYOUT

Library Mark | Name
--- | ---
8SM0x | IDM0x
9SM0x | IDMxx

## Description
Synchronous Modem Input

| Relat. Addr. | Symbol | Contents | Explanation |
|--------------|--------|----------|-------------|
| -12 | NOWSTAT | 0 | Switch for modem output |
| -11 | TERSW | 0 | Terminate condition switch |
| -10 | MOSW | 0 | Block switch |
| -7 | LAST | 0 | Last character |
| -6 | TMSUB | MOTMI | Timer subroutine |
| -5 | TMR | 0 | Time-out counter |
| -4 | TTMR | -16 | Not used |
| -3 | HDEV | IOX 100# | IOX instruction (i.e., IOX 100) |
| -2 | STDRIV | MODIN | Start address of driver |
| -1 | DRIVER | MODI1 | Restart address after interrupt |
| 0 | RESLINK | 0 | Reservation link |
| 1 | RTRES | 0 | Reserving RT program |
| 2 | BWLINK | *-2 | Beginning of waiting queue |
| 3 | TYPRNG | 110000 | Device type bits and ring |
| 4 | ISTATE | 0 | 0 = idle, 1 = busy, -1 = no wait mode |
| 5 | MLINK | 0 | Monitor queue link |
| 6 | MFUNC | IOREM | Monitor level function address |
| 7 | IOTRANS | MOTRI | Called from INBT/OUTBT to transfer |
| 10 | STDEV | TEXIT | Start device |
| 11 | SETDV | MOSTI | IOSET routine |
| 12 | DFOPP | 0 | Not used |
| 13 | DERROR | 0 | Error code |
| 14 | BUFST | BUF0 + BUF | Start of ring buffer |
| 15 | MAX | MIBUx + MIBUx | Buffer capacity |
| 16 | BHOLD | 0 | Number of characters in buffer |
| 17 | HENTE | 0 | Fetch pointer |
| 20 | CFREE | MIBUx + MIBUx | Free positions |
| 21 | FYLLE | 0 | Store pointer |
| 22 | MINBHOLD | 0 | Lower limit for break |
| 23 | MAXBHOLD | MIBUx + MIBUx | Upper limit for break |

\# = for modem 1 (x = 1)

ND-60.112.01

*Scanned by Jonny Oddene for Sintran Data © 2020*

---

## Page 71

# DATA FIELD LAYOUT

Library Mark | Name  
--- | ---  
8CRD | IDV4  
8CRD2 | IDV42  

## Description
Card Reader

| Relat. Addr. | Symbol | Contents | Explanation |
|--------------|--------|----------|-------------|
| \-12         | BINCARD| 0        | > < 0: binary read (no conversion), = 0: convert to ASCII |
| \-11         | CSC    | 0        | Column counter for IOTRANS routine |
| \-10         | CNC    | 0        | Column counter for interrupt routine |
| \-7          | CASW   | 0        | Information exchange between driver and IOTRANS routine |
| \-6          | TMSUB  | CATIM    | Time-out routine |
| \-5          | TMR    | 0        | Time-out counter |
| \-4          | TTMR   | \-6      | Start value of TMR |
| \-3          | HDEV   | IOX 420# | IOX instruction (i.e., IOX 420) |
| \-2          | STDRIV | IDR4     | Start address of driver |
| \-1          | DRIVER | IDR41    | Restart address after interrupt |
| 0            | RESLINK| 0        | Reservation link |
| 1            | RTRES  | 0        | Reserving RT program |
| 2            | BWLINK | * \-2    | Beginning of waiting queue |
| 3            | TYPRING| 110000   | Device type bits and ring |
| 4            | ISTATE | 0        | 0 = idle, 1 = busy, \-1 = no wait mode |
| 5            | MLINK  | 0        | Monitor queue link |
| 6            | MFUNC  | IORES    | Monitor level function address |
| 7            | IOTRANS| CATRA    | Called from INBT/OUTBT to transfer |
| 10           | STDEV  | TEXIT    | Start device |
| 11           | SETDV  | CARST    | IOSET routine |
| 12           | DFOPP  | 0        | Not used |
| 13           | DERROR | 0        | Error code |
| 14           | BUFST  | BUF + BUF0 | Start of ring buffer |

*# = for IDB4 only.*

ND-60.112.01

---

## Page 72

# DATA FIELD LAYOUT

**Library Mark**  
INDxx  
IBLxx

**Name**  
IDxxI

## Description

Internal Device, Character Oriented Input

| Relat. Addr. | Symbol   | Contents            | Explanation                                |
|--------------|----------|---------------------|---------------------------------------------|
| 0            | RESLINK  | 0                   | Reservation link                            |
| 1            | RTRES    | 0                   | Reserving RT program                        |
| 2            | BWLINK   | *–2                 | Beginning of waiting queue                  |
| 3            | TYPRING  | 112100              | Device type bits and ring                   |
| 4            | ISTATE   | 0                   | 0 = idle, 1 = busy, –1 = no wait mode       |
| 5            | MLINK    | 0                   | Monitor queue link                          |
| 6            | MFUNC    | IORES               | Monitor level function address              |
| 7            | IOTRANS  | IGTCIH              | Called from INBT/OUTBT to transfer          |
| 10           | STDEV    | ISTDV               | Start device                                |
| 11           | SETDV    | CLBUF               | IOSET routine                               |
| 12           | DFOPP    | IDxxO               | Opposite data field for two-way devices     |
| 13           |          | 0                   | Not used                                    |
| 14           | BUFST    | BUF0 + BUF          | Buffer start                                |
| 15           | MAX      | IDBUS + IDBUS       | Buffer capacity                             |
| 16           | BHOLD    | 0                   | Number of characters in buffer              |
| 17           | HENTE    | 0                   | Fetch pointer                               |
| 20           | CFREE    | IDBUS +             | Free positions                              |
| 21           | FYLLE    | 0                   | Store pointer                               |
| 22           | CLOGDV   | 1210 + XIBNX        | Logical unit number for DMA data field      |
| 23           | DFDEV    | 1211 + XIBNX        | Logical unit number for DF data fields      |
| 24           | IMAXBHOLD| D (2) 1, 0 MAXBHOLD |                                             |
| 26           | TERM     | TERMC               | Terminator character                        |
| 27           | CHAR\!   | 0                   | Current character                           |

ND-60.112.01

Scanned by Jonny Oddene for Sintran Data © 2020

---

## Page 73

# DATA FIELD LAYOUT

Library Mark | Name  
-------------|-------
INDxx        | IDxx0  
IBLxx        |   

**Description**  
Internal device, character arr. output  

| Relat. Addr. | Symbol  | Contents   | Explanation                                      |
|--------------|---------|------------|--------------------------------------------------|
| 0            | RESLINK | 0          | Reservation link                                 |
| 1            | RTRES   | 0          | Reserving RT program                             |
| 2            | BWLINK  | *−2        | Beginning of waiting queue                       |
| 3            | TYPIRNG | 112100     | Device type bits and ring                        |
| 4            | ISTATE  | 0          | 0 = idle, 1 = busy, −1 = no wait mode            |
| 5            | MLINK   | 0          | Monitor queue link                               |
| 6            | MFUNC   | IORES      | Monitor level function address                   |
| 7            | IOTRANS | IPTCH      | Called from INBT/OUTBT to transfer               |
| 10           | STDEV   | OSTDV      | Start device                                     |
| 11           | SETDV   | INIOS      | IOSET routine                                    |
| 12           | DFOPP   | IDxxI      | Opposite data field for two-way devices          |
| 13           | DERROR  | 0          | Error code                                       |
| 14           | 0       |            | not used                                         |
| 15           | 0       |            |                                                  |
| 16           | 0       |            |                                                  |
| 17           | 0       |            |                                                  |
| 20           | 0       |            |                                                  |
| 21           | 0       |            |                                                  |
| 22           | CLOGDV  | 1210 + XIBNX| Logical unit number for DMA data field           |
| 23           | DFDV    | 1272 + YIBNY| Logical unit number for DF data field            |
| 24           | LREGC   | 0          | Return address after IOTRANS is executed         |
| 25           | CASUN   | 0          | Device unit number                               |

ND-60.112.01

---

## Page 74

# DATA FIELD LAYOUT

Library Mark | Name
------------ | ----
             | IERRF

**Description**  
Error message, internal device input

| Relat. Addr. | Symbol   | Contents | Explanation |
|--------------|----------|----------|-------------|
| 0            | RESLINK  | 0        |             |
| 1            | RTRES    | 0        |             |
| 2            | BWLINK   | *-2      |             |
| 3            | TYPRINǤ  | 110002   | see IDxxl   |
| 4            | ISTATE   | 0        |             |
| 5            | MLINK    | 0        |             |
| 6            | MFUNC    | IORES    |             |
| 7            | IOTRANS  | GETW     |             |
| 10           | STDEV    | ISTDV    |             |
| 11           | SETDV    | CLBUF    |             |
| 12           | DFOPP    | OERRF    |             |
| 13           |          | 0        |             |

ND-60.112.01

---

## Page 75

# DATA FIELD LAYOUT

**Library Mark**  
8M1Ua  
8M2Ua  

**Name**  
M1DIx  
M2DIx  

_where a = x − 1_

## Description

Magnetic Tape Input

| Relat. Addr. | Symbol    | Contents | Explanation                                      |
|--------------|-----------|----------|--------------------------------------------------|
| −1           | ADRBHEAD  | 0        | Buffer head address                              |
| 0            | RESLINK   | 0        | Reservation link                                 |
| 1            | RTRES     | 0        | Reserving RT program                             |
| 2            | BWLINK    | *−2      | Beginning of waiting queue                       |
| 3            | TYPRING   | 112000   | Device type bits and ring                        |
| 4            | ISTATE    | 0        | 0 = idle, 1 = busy, −1 = no wait mode            |
| 5            | MLINK     | 0        | Monitor queue link                               |
| 6            | MFUNC     | I0RES    | Monitor level function address                   |
| 7            | IOTRANS   | CBGET    | Called from INBT/OUTBT to transfer               |
| 10           | STDEV     | TEXIT    | Start device                                     |
| 11           | STEDV     | CAICL    | IOSET routine                                    |
| 12           | DFOPP     | MTD0x    | Opposite data field for two-way devices          |
| 13           | DERROR    | 0        | Error code                                       |
| 14           | BUFST     | 0        | Start of ring buffer                             |
| 15           | MAX       | 4000     | Buffer capacity                                  |
| 16           | BHOLD     | 0        | Number of characters in buffer                   |
| 17           | HENTE     | 0        | Fetch pointer                                    |
| 18           | CFREE     | 4000     | Free pointer                                     |
| 21           | FYLLE     | 0        | Store pointer                                    |
| 22           | CLODG     | 560#     | Logical unit number for DMA data field           |
| 23           | DFDEV     | 5266, 526# | Logical unit number for DF data fields            |
| 24           | LREGC     | 0        | Return address after IORANS is executed          |
| 25           | CASUN     | x−1      | Device unit number                               |
| 26           | CERROR    | 0        | Current error code                               |
| 27           | LASTC     | 0        | Current character                                |
| 30           | NOWRE     | 0        | Number of characters to read/write               |
| 31           | CPARM (5) | 0, *, +, 3, *, −6, *−4, 0 | Parameter list for MTRANS (including first word in memory buffer address) |
| 36           | MABUF     | 0        | Second word in memory buffer address             |
| 37           | VEFUNC    | 0        | Not used                                         |
| 40           | CIOLOG    | 40#      | Logical device number                            |

# Notes

## ND-60.112.01

#  
∗# = For magnetic tape controller 1, drive 1 (y = 1, x = 1)

---

## Page 76

# DATA FIELD LAYOUT

**Library Mark**
- 8M1Ua
- 8M2Ua

**Name**
- MTDOx
- M2DOx

*where a = x - 1*

## Description
### Magnetic Tape Output

| Relat. Addr. | Symbol | Contents | Explanation |
|--------------|--------|----------|-------------|
| -1 | ADRBHEAD | 0 | Buffer head address |
| 0  | RESLINK  | 0 | Reservation link |
| 1  | RTRES    | 0 | Reserving RT program |
| 2  | BWLINK   | *−2 | Beginning of waiting queue |
| 3  | TYPRING  | 112000 | Device type bits and ring |
| 4  | ISTATE   | 0 | 0 = idle, 1 = busy, −1 = no wait mode |
| 5  | MLINK    | 0 | Monitor queue link |
| 6  | MFUNC    | IORES | Monitor level function address |
| 7  | IOTRANS  | CBPUT | Called from INBT/OUTBT to transfer |
| 10 | STDEV    | TEXIT | Start device |
| 11 | SETDV    | CADCL | IOSET routine |
| 12 | DFOPP    | MTD T1 | Opposite data field for two-way device |
| 13 | DERROR   | 0 | Error code |
| 14 | BUFST    | 0 | Start of ring buffer |
| 15 | MAX      | 4000 | Buffer capacity |
| 16 | BHOLD    | 0 | Number of characters in buffer |
| 17 | HENTC    | 0 | Fetch pointer |
| 20 | CFREE    | 4000 | Free positioning |
| 21 | FBYTE    | 0 | Store pointer |
| 22 | CLOGDV   | 560# | Logical unit number for DMA data field |
| 23 | DFDEV    | 526# | Logical unit number for DF data field |
| 24 | LREGC    | 0 | Return address after IORANS is executed |
| 25 | CASUN    | x−1 | Device unit number |
| 26 | CERROR   | 0 | Current error code |
| 27 | LASTC    | 0 | Current character |
| 30 | NOWRE    | 2000 | Number of characters to read/write |
| 31 | CPARM (5) | 1, *, +3, *−6, *−4, 0 | Parameter list for MTRANS (including first word in memory buffer address) |
| 36 | MABUF    | 0 | Second word in memory buffer address |
| 37 | VEFUNC   | 0 | Not used |
| 40 | CIOLOG   | 40# | Logical device number |

***# = for controller 1, drive 1 (y = 1, x = 1)***

**ND-60.112.01**

---

## Page 77

# DATA FIELD LAYOUT

**Library Mark**  
99TMT  
99HMT/8MT2  

**Name**  
MTFIE  
M2FIE  

---

## Description  
Tandberg/Pertec or H.P. (Mag. tape DMA)

| Relat. Addr. | Symbol     | Contents | Explanation                                                                 |
|--------------|------------|----------|------------------------------------------------------------------------------|
| -52          | ADNSTY (4) | 0, ., 0  | Actual density/parity (unit 0, 1, 2, 3)                                      |
| -46          | SHSTAT (4) | 0, ., 0  | Saved last status (unit 0, 1, 2, 3)                                          |
| -42          | CERRCODE   | 0        | Current error code                                                           |
| -41          | MAXUNIT    | 89NMT    | Maximum unit number on this controller                                       |
| -40          | MACOU      | 0        | Erase counter                                                                |
| -37          | MRETURN    | 0        | Address for returning read words                                             |
| -36          | MWRING     | 2#       | Write ring bit                                                               |
| -35          | MWSTAT     | 0        | Status when write ring present                                               |
| -34          | MLOAD      | 0#       | Load point status                                                            |
| -33          | CLRG       | 0        | Saved L register                                                             |
| -32          | TRG        | 0        | Register when calling driver (REAL = TADRG)                                  |
| -31          | ARG        | 0        | Register when calling driver (REAL = TADRG)                                  |
| -30          | DRG        | 0        | Register when calling driver (REAL = TADRG)                                  |
| -27          | XRG        | 0        | Register when calling driver (REAL = TADRG)                                  |
| -26          | CTRG       | 0        | Register when calling driver first time (REAL = CTRG)                        |
| -25          | CARG       | 0        | Register when calling driver first time (REAL = CTRG)                        |
| -24          | CDRG       | 0        | Register when calling driver first time (REAL = CTRG)                        |
| -23          | CXRG       | 0        | Register when calling driver first time (REAL = CTRG)                        |
| -22          | ERCNT      | 0        | Number of error returns from driver                                          |
| -21          | SERRB      | 1200     | Serious error bits (bit 12: overflow in read, bit 10: data field for channel input SII/SIII communication) |
| -20          | WERRB      | 0        | Not used                                                                     |
| -17          | AERRB      | 0        | Accumulated error bits                                                       |
| -16          | TACNS      | -4       | Number of retrials wanted before message                                     |
| -15          | TACOUNT    | 0        | Retrial counter                                                              |
| -14          | MWCNT      | -2       | Number of times to write erase gap                                           |
| -13          | BLSZ       | 2000     | Block size                                                                   |
| -12          | TRNSF      | TMAGT#   | Driver address                                                               |
| -11          | BUSY       | MBUSY    | Busy return                                                                  |
| -10          | FINISH     | MFINI    | Transfer finished                                                            |
| -7           | ERROR      | MFEIL    | Error return from driver                                                     |
| -6           | MGTMR      | MGTMIR   | Time-out subroutine                                                          |
| -5           | TMR        | 0        | Time-out counter                                                             |
| -4           | TMR        | -10      | Start value of TMR                                                           |
| -3           | HDEV       | 520#*    | Hardware device number (i.e., IOX 520)                                       |
| -2           | STDRIV     | CTRMA    | Start address of driver                                                      |
| -1           | DRIVER     | 0        | Restart address after interrupt                                              |
| 0            | RESLINK    | 0        | Reservation link                                                             |
| 1            | RTRES      | 0        | Reserving RT program                                                         |
| 2            | BWLINK     | *.2       | Beginning of waiting queue                                                   |
| 3            | TYPRING    | 1006     | Device type bits and ring                                                    |
| 4            | ISTATE     | 0        | 0 = idle, 1 = busy, -1 = no wait mode                                        |
| 5            | MLINK      | 0        | Monitor queue link                                                           |
| 6            | MFUNC      | RETRA    | Monitor level function address                                               |
| 7            | TRLREG     | 0        | Return address on monitor level after transfer                               |
| 10           | MSTR       | 0        | Hardware status from driver                                                  |
| 11           | MTRANS     | MTRNS    | Monitor level routine to activate driver                                     |
| 12           | MTRREF     | 0        | Program calling CLOSE                                                        |
| 13           | BREGC      | 0        | Address of I/O data field                                                    |
| 14           | MEMA1      | 0        | Initial memory address                                                       |
| 15           | MEMA2      | 0        | Initial memory address                                                       |

*Scanned by Jonny Oddene for Sintran Data © 2020*  

ND-60.112.01

---

## Page 78

# Technical Specifications

| Code | Value | Description |
|------|-------|-------------|
| 16   | CMAD1 0 | Current memory address |
| 17   | CMAD2 0 | Current memory address |
| 20   | CLDEV MTCLOD | Address of clear device routine |
| 21   | BADTAPE 100# | Status bit for bad tape |
| 22   | RHSTAT (4) 0, .., 0 | Error status in read (unit 0, 1, 2, 3) |
| 26   | RERRCOUNT (4) 0, .., 0 | Error counter in read (unit 0, 1, 2, 3) |
| 32   | WHSTAT (4) 0, .., 0 | Error status in write (unit 0, 1, 2, 3) |
| 36   | WERRCOUNT (4) 0, .., 0 | Error counter in write (unit 0, 1, 2, 3) |

## FLAGS = BIT

| Bit | Description |
|-----|-------------|
| 0   | 5NOSLICE Run with fixed priority |
| 1   | 5ESCON Escape allowed in command mode |
| 2   | 5ESC2SET Escape has been typed but not served |
| 3   | 5LOGOUT Logout on missing carrier |
| 4   | 5ABJOB Abort job (error) |
| 5   | 5OLBRK Missing carrier on output |

\# = Tandberg/Pertec only  
\#\# = Controller 1 only (MTFIE)

ND-60.112.01

---

## Page 79

# Data Field Layout

**Library Mark:** 8S3C  
**Name:** NDEMF, NDEM2

## Description
Monitor Work Field for Non-Demand

| Relat. Addr. | Symbol | Contents | Explanation |
|--------------|--------|----------|-------------|
| 0            | ZPREG  | 0        |             |
| 1            | ZXREG  | 0        |             |
| 2            | ZTREG  | 0        |             |
| 3            | ZAREG  | 0        | Register block |
| 4            | ZDREG  | 0        |             |
| 5            | ZLREG  | 0        |             |
| 6            | ZSREG  | 0        |             |
| 7            | ZBREG  | 0        |             |
| 10           | OLDPAG | 0        | Saved priority and page table numbers of calling program |
| 11           | D0     | 0        |             |
| 12           | D1     | 0        | Monitor call parameters |
| 13           | D2     | 0        |             |
| 14           | D3     | 0        |             |
| 15           | D4     | 0        |             |
| 16-27        |        | 0        | Work area   | 

**ND-60.112.01**

---

## Page 80

# Data Field Layout

**Library Mark**: 7DIxx  
**Name**: NDIxx  

## Description
**Digital Input (ND810)**

| Relat. Addr. | Symbol  | Contents | Explanation                      |
|--------------|---------|----------|----------------------------------|
| -3           | HDEV    | IOX 3000#|                                  |
| -2           | STDRIV  | DIGIN    |                                  |
| -1           | DRIVER  | DIGIF    |                                  |
| 0            | RESLINK | 0        |                                  |
| 1            | HTRES   | 0        |                                  |
| 2            | BWLINK  | *-2      |                                  |
| 3            | TYPRING | 110000   |                                  |
| 4            | ISTATE  | 0        |                                  |
| 5            | MLINK   | 0        |                                  |
| 6            | MFUNC   | JORES    |                                  |
| 7            | IOTRANS | DIGIT    | see DTxxR                        |
| 10           | STOEV   | DIGIS    |                                  |
| 11           | SETDV   | DIOSE    |                                  |
| 12           | DFOPP   | 0        |                                  |
| 13           | DERROR  | 0        |                                  |
| 14           | BUFFST  | BUF0 + BUF |                                |
| 15           | MAX     | 20 + 20  |                                  |
| 16           | BHOLD   | 0        |                                  |
| 17           | HENTE   | 0        |                                  |
| 20           | CFREE   | 20 + 20  |                                  |
| 21           | FYLLE   | 0        |                                  |
| 22           | BMASK   | 0        | Mask for significant bits        |
| 23           | DITYPE  | 0        | Interrupt flag                   |
| 24           | CURRDAT | 0        | Last significant input data      |
| 25           | BITTE   | 0        | Work cell                        |
| 26           | MATCH   | 0        | Last match register              |

\# = for input number 1 (xx = 01)  

**ND-60.112.01**

---

## Page 81

# DATA FIELD LAYOUT

Library Mark: 7DOxx  
Name: NDOxx

## Description
Digital Output (ND811)

### Relat. Addr. | Symbol | Contents | Explanation
--- | --- | --- | ---
-3 | HDEV | IOX3004# | 
-2 | STDRIV | 0 | 
-1 | DRIVER | 0 | 
0 | RESLINK | 0 | 
1 | RTRES | 0 | 
2 | BWLINK | *-_2 | 
3 | TYPRING | 110200 | See DTxxW
4 | ISTATE | 0 | 
5 | MLINK | 0 | 
6 | MFUNC | 0 | 
7 | IOTRANS | DOITR | 
10 | STDEV | TEXIT | 
11 | SETDV | DOIOS | 
12 | DFOPP | 0 | Not used
13 | DERRO | 0 | Error code
14 | OLDREG | 0 | Last status for output

# = for digital output number 1 (xx = 01)

ND-60.112.01

---

## Page 82

# DATA FIELD LAYOUT

**Description**  
Error message internal device output

| Relat. Addr. | Symbol  | Contents | Explanation  |
|--------------|---------|----------|--------------|
| 0            | RESLINK | 0        |              |
| 1            | RTRES   | 0        |              |
| 2            | BWLINK  | 0        |              |
| 3            | TYPRING | 110002   |              |
| 4            | ISTATE  | 0        |              |
| 5            | MLINK   | 0        | see IDxxO    |
| 6            | MFUNC   | IORES    |              |
| 7            | IOTRANS | PUTW     |              |
| 10           | STDEV   | OSTDV    |              |
| 11           | SETDV   | CLBUF    |              |
| 12           | DFOPP   | IERRF    |              |
| 13           | DERROR  | 0        |              |

**Library Mark:** \-  
**Name:** OERRF

# DATA FIELD LAYOUT

**Description**  
Semaphore

| Relat. Addr. | Symbol  | Contents | Explanation  |
|--------------|---------|----------|--------------|
| 0            | RESLINK | 0        |              |
| 1            | RTRES   | 0        |              |
| 2            | BWLINK  | *-2      |              |
| 3            | TYPRING | 0        | see DTxxW    |

**Library Mark:**  
8SM10  
8SM50  

**Name:**  
SEMlx  
SEMxx  

ND-60.112.01

---

## Page 83

# DATA FIELD LAYOUT

Library Mark: ySLx  
Name: SLMx  
SyMx

## Description
### NORDCOM Selector Module

| Relat. Addr. | Symbol  | Contents | Explanation                                     |
|--------------|---------|----------|-------------------------------------------------|
| -5           |         | 0        | n/a                                             |
| -4           | KLUMP   | 0        | Save T register                                 |
| -3           | HDEV    | IOX40#   | IOX for ACM                                     |
| -2           |         | 0        | n/a                                             |
| -1           |         | 0        | n/a                                             |
| 0            | RESLINK | 0        |                                                 |
| 1            | RTRES   | 0        |                                                 |
| 2            | BWLINK  | * -2     |                                                 |
| 3            | TYPRING | 110000   | > see DTxxR                                     |
| 4            | ISTATE  | 0        |                                                 |
| 5            | MLINK   | 0        |                                                 |
| 6            | MFUNC   | IORES    |                                                 |
| 7            | IOTRANS | NORSE    | Called from INBT/OUTBT to transfer              |
| 10           | STDEV   | TEXIT    | Start device                                    |
| 11           | SETDV   | SMSTA    | IOSET routine                                   |
| 12           |         | 0        | n/a                                             |
| 13           | DERRO   | 0        | Error code                                      |
| 14           | RETAD   | . 0      | Return address                                  |
| 15           | REGAD   | 142000#  | Selector module                                 |
| 16           | SCRAT   | 0        | Work cell                                       |
| 17           | ENSTA   | . 0      | Data                                            |
| 20           | INSTA   | 0        | Bank number                                     |

\# = ACM number 1, selector module number 1 (x,y) = (1,1)

ND-60.112.01

---

## Page 84

# DATA FIELD LAYOUT

**Library Mark:** CmmLn  
**Name:** SmmnR

**Description:**  
Channel Input SIII/SII Communication

| Relat. Addr. | Symbol    | Contents    | Explanation                               |
|--------------|-----------|-------------|-------------------------------------------|
| -17          | CTTPY     | 0           | Terminal type                             |
| -16          | CESC      | 33          | Escape character                          |
| -15          | BRKMAX    | 0           | Maximum BHOLD before break                |
| -14          | ANTORD    | 0           | Number of words to read (for MAGTP)       |
| -13          | XSAC      | 0           | Save X register in IOTRANS                |
| -12          | DFLAG     | 4           | Flag bits                                 |
| -11          | ECHOTAB   | ECHO        | Echo table                                |
| -10          | BRKTAB    | 0           | Break table                               |
| -7           | LAST      | 0           | Last typed character                      |
| -6           | CMDAT     | COM0n       | Pointer to communication data field       |
| -5           | IXSAC     | 0           | Save data field for magnetic tape         |
| -4           | ANTMEL    | 0           | Number of frames in input queue           |
| -3           | BYTES     | 0           | Byte counter in frame                     |
| -2           | INCR      | 0           | Byte counter in buffer                    |
| -1           | CHAN      | mm—1        | Channel number (0 - 37 octal)             |
| 0            | RESLINK   | 0           | Reservation link                          |
| 1            | RTRES     | 0           | Reserving RT program                      |
| 2            | BWLINK    | *—2         | Beginning of waiting queue                |
| 3            | TYPING    | 116000      | Device type bits and ring                 |
| 4            | ISTATE    | 0           | 0 = idle, 1 = busy, —1 = no wait mode     |
| 5            | MLINK     | 0           | Monitor queue link                        |
| 6            | MFUNC     | C10RE       | Monitor level function address            |
| 7            | IOTRANS   | SST1        | Called from INBST/OUTBT to transfer       |
| 10           | STDEV     | TEX1T       | Start device                              |
| 11           | SETDV     | CLSB1       | IOSET routine                             |
| 12           | DFOPP     | SmmnWV      | Opposite data field for two-way devices   |
| 13           | DERROR    | 0           | Error code                                |
| 14           | MSSTART   | 0           | Pointer to first buffer of frame          |
| 15           | PLMSG     | •           | Last frame of input queue                 |
| 16           | PFMSG     | 0           | First frame of input queue                |
| 17           | CURBUF    | 0           | Current input buffer                      |
| 20           | CHNST     | 0           | Channel input status                      |
| 21           | LRSA      | 0           | Save L register in IOTRANS                |
| 22           | BSTATE    | 0           | Background program state                  |
| 23           | TSTATE    | 0           | Time slice state                          |
| 24           | DBPROG    | BCmmn       | Background RT program                     |
| 25           | DBADDR    | 0           | Saved P register on escape + file system monitor call |
| 26           | RI/FIL    | 0           | For MODE (input file number)              |
| 27           | BCHISTS   | 0           | For MODE (input status)                   |
| 30           | DER0(2)   | 0, 0        | Register block                            |
| 32           | DER2(6)   | 0, --, 0    | Register block                            |
|              | BREG-BLOCK= |           | Register save at escape                   |
| 40           | DBPRFG    | 0           | P register on page fault on IOBT level    |
| 41           | DBAFTPRI  | 0           | ACTPRI on page fault on IOBT level        |
| 42           | FLABG     | 0           | Background flag                           |
| 43           | RTTUT     | RUmnn       | RT description of file transfer program   |

ND-60.112.01

---

## Page 85

# DATA FIELD LAYOUT

**Library Mark:** CmmLn  
**Name:** SmmnW  

## Description
Channel Output

| Relat. Addr. | Symbol | Contents | Explanation |
|--------------|--------|----------|-------------|
| \-15 |  | 0 |  |
| \-14 | ANTORD | 0 | Number of words to write (from MAGTP) |
| \-13 | XSAC | 0 | Save X register in IOTRANS |
| \-12 | DFLAG | 0 | Flag word (described in manual) |
| \-11 | ECHOTAB | ECH0 | Pointer to echo table |
| \-10 | BRKTAB | 0 | Pointer to break table |
| \-7 | LAST | 0 | Save last byte |
| \-6 | CMDAT | CMO n | Pointer to communication data field |
| \-5 | MSIZE | BUSIZ + BUSIZ | Maximum number of bytes in a frame (406 octal) |
| \-4 | SCPRI | 0 | Channel priority |
| \-3 | BYTS | 0 | Byte counter in frame |
| \-2 | INCR | 0 | Byte counter in buffer |
| \-1 | CHAN | mm—1 | Channel number (0-37 octal) |
| 0 | RESLINK | 0 | Reservation link |
| 1 | RTRES | 0 | Reserving RT program |
| 2 | BWLINK | *—2 | Beginning of waiting queue |
| 3 | TYPIRNG | 116000 | Device type bits and ring |
| 4 | ISTATE | 0 | 0 = idle, 1 = busy, -1 = no wait mode |
| 5 | MLINK | 0 | Monitor queue link |
| 6 | MFUNC | CIORE | Monitor level function address |
| 7 | IOTRANS | SSTO | Called from INBT/OUTBT to transfer |
| 10 | STEDV | TEXIT | Start device |
| 11 | SETDV | CLSBO | IOSET routine |
| 12 | DFOPP | SmmnR | Opposite data field for two-way devices |
| 13 | DERROR | 0 | Error code |
| 14 | MSSTART | 0 | Pointer to first buffer of frame |
| 15 | UANTMEL | 0 | Number of frames in send queue for this channel |
| 16 |  | Not used |  |
| 17 | CURBU | 0 | Current output buffer |
| 20 | CHNST | 0 | Channel output status |
| 21 | LRSA | 0 | Save L register in IOTRANS |
| 22 | RTIN | RUmmn | RT description of file transfer program |
| 23 | ROFIL | 0 | For mode (IOUTPUT file number) |
| 24 | BCHOSTS | 0 | For mode (output status) |

ND-60.112.01

*Scanned by Jonny Oddene for Sintran Data © 2020*

---

## Page 86

# Data Field Layout

**Library Mark:** ySMz  
**Name:** SMNz, SyNz  

## Description

**NORCOM Semigraphic Buffer**  
(y = ACM no.)  
(z = sem.bank no.)

| Relat. Addr. | Symbol | Contents | Explanation |
|--------------|--------|----------|-------------|
| -5           |        | 0        |             |
| -4           | KLUMP  | 0        |             |
| -3           | HDEV   | IOX 40#  |             |
| -2           |        | 0        |             |
| -1           |        | 0        |             |
| 0            | RESLINK| 0        |             |
| 1            | RTRES  | 0        |             |
| 2            | BWLINK | *-2      |             |
| 3            | TYPING | 110000   | see SLMz    |
| 4            | ISTATE | 0        |             |
| 5            | MLINK  | 0        |             |
| 6            | MFUNC  | IORES    |             |
| 7            | IOTRANS| NORDC    |             |
| 10           | STDEV  | TEXIT    |             |
| 11           | SETDV  | NORST    |             |
| 12           |        | 0        |             |
| 13           | DERRO  | 0        |             |
| 14           | RETAD  | 0        |             |
| 15           | CURAD  | 0        | Current ACM address |
| 16           | CHSTA  | 0        | ACM data   |
| 17           | BANKN  | z        | Bank number |
| 20           | RUNMO  | 1        | Program mode |
| 21           | MAXAD  | 5277#    | Maximum address semigraphic |
| 22           | SAVE   | 0        |             |
| 23           | INKRE  | 1        | Screen direction increment |
| 24           | NOSTA  | 0        | Screen addressing modus |
| 25           | CURIN  | 0        |             |
| 26           | SAML1  | 0        | Parameters for ACM address |
| 27           | SAML2  | 0        |             |
| 30           | SAML3  | 0        |             |
| 31           | NOVI   | 0        | Address for RTCOMMON semigraphic buffer |
| 32           | NOVO   | 0        | Number of words for RTCOMMON semigraphic buffer |

# = ACM no. = 1, bank no = 1, (y,z) = (1,1)

ND-60.112.01

---

## Page 87

# Data Field Layout

Library Mark: SLPx  
Name: SPPRx

## Description
### Spooling

| Relat. Addr. | Symbol        | Contents        | Explanation                                                                 |
|--------------|---------------|-----------------|-----------------------------------------------------------------------------|
| –1           | JPLI*1        |                 | Pointer to spooling program                                                 |
| 0            | SPOLPROGRAM   | SPOOL           |                                                                             |
| 1            | RTDESRIPTION  | SPRTx           | Address to RT description                                                   |
| 2            | DEVNO         | SNLPx           | Peripheral logical device number                                            |
| 3            | QSEGMENTS     | SPSGx           | Queue segment ( = queue no.)                                                |
| 4            | QSEMAPHORE    | 1136#           | Queue semaphore                                                             |
| 5            | QIOSEMA-HORE  | 1137#           | Queue I/O semaphore                                                         |
| 6            | STOPF         | 0               | Stop command flag                                                           |
| 7            | ABORF         | 0               | Abort command flag:<br>1 = abort current print; 2 = restart current print; 3 = stop print;<br>bit 8 = 1 = forward space print; bit 9 =1 = backspace bit;<br>bit 10=1 = new file is set first in queue |
| 10           | HEADER        | HEAPR           | Device dependent routine                                                    |
| 11           | TRAILER       | TRAPR           | Device dependent routine                                                    |
| 12           | PRINTBUFFER   | SPRIN           | Device dependent routines                                                   |
| 13           | FILENUMBER    | 0               | File number                                                                 |
| 14           | LASTPAGE      | 0               | Last page number, if any                                                    |
| 15           | PAGENUMBER    | 0               | Current page                                                                |
| 16           | REMAINING     | 0               | Byte number on last page                                                    |
| 17           | BYTENUNBER    | 0               | Current byte number                                                         |
| 20           | FPALIST       | 0               |                                                                             |
| 21           | FPARDLIST     | 0               |                                                                             |
| 22           | BUFFAD        | 74000           | Device buffer                                                               |
| 23           | FPAR11LIST    | 0               |                                                                             |
| 24           | FPAR21LIST    | 0               |                                                                             |
| 25           | PRINPAR       | ENEN            | ABSTRANS parameter list                                                     |
| 26           | BUFFPEK       | +3              | ABSTRANS parameter list                                                     |
| 27           | DUMPAR        | $000            | ABSTRANS parameter list                                                     |
| 30           | BYTNPEK       | *–11            |                                                                             |
| 31           | PCORAD (2)    | 0               |                                                                             |
| 33           | PRSEGM        | 177400 +        |                                                                             |
| 34           | CMAP1         | 0               | Memory map element                                                          |
| 35           | CMAP2         | 36              | Memory map element                                                          |
| 36           | CMAP3         | 162000          | Memory map element                                                          |
| 37           | SNPAGE        | 0               | Number of pages to backspace/forward space                                  |
| 40           | SNLINE        | 0               | Number of lines to backspace/forward space                                  |
| 41           | SCONDITION    | 0               | Spooling conditions                                                         |
| 42           | SNLPAGE       | 104             | Number of lines on one page                                                 |
| 43           | SPMODE        | 0               | 0 = print, 1 = plot                                                         |
| 44           | SPAGENUM-     |                 |                                                                             |
| BER          |               | 0               | Saved page number                                                           |
| 45           | WSNLINE       | 0               | Temporary SNLINE                                                            |
| 46           | WSNPAGE       | 0               | Temporary SNPAGE                                                            |
| 47           | LFCOUNT       | 0               | LF counter                                                                  |

--- 

Scanned by Jonny Oddene for Sintran Data © 2020

ND–60.112.01

---

## Page 88

# Technical Details

| Code | Name | Value | Description |
|------|------|-------|-------------|
| 50   | SPFNAM (11) | 0 | Peripheral file name |
| 61   | QELLEM (0)  | 0 | Buffer for one queue element |
| 61   | NOCOPYS     | 0 | First word = number of copies |
| 62   | FSPMESS     | 0 | Flag for spooling file message |
| 63   | FNAME (57)  | 0 | Position of spooling file name |
| 142  | BSMESS (117)| 0 | Users message buffer |

Note: Relative address 0 corresponds to label SPPRx -1 (B register points to SPPRx -1 in driver)

# = for spooling number 1

---

ND-60.112.01

Scanned by Jonny Oddene for Sintran Data © 2020

---

## Page 89

# DATA FIELD LAYOUT

Library Mark: yTBq  
Name: T8Aq, TByAq

---

### Description
NORCOM Trackerball  
*(y = ACM no.; q = trackerball no.)*

| Relat. Addr. | Symbol | Contents | Explanation |
|--------------|--------|----------|-------------|
| -5           | KDEV   | IOX 1040#| Digital I/O IOX |
| -4           | KLUMP  | 0        | Save T register |
| -3           | HDEV   | IOX 40#  | IOX for ACM |
| -2           | FADR   | 172000#  | ACM foreground address |
| -1           | DRIVER | TBAEN    |             |
| 0            |        | 0        |             |
| 1            |        | 0        |             |
| 2            |        | *–2      | see DTxxR   |
| 3            |        | 0        |             |
| 4            |        | 0        |             |
| 5            |        | 0        |             |
| 6            | MFUNC  | TBRES    | Monitor level function address |
| 7            | TBNO   | q        | Trackerball number |
| 10           | OLDIO  | 0        | Last information from digital I/O |
| 11           | FLAGG  | 0        | Read/no read flag |
| 12           | SCNO   | 0        | Screen number |
| 13           | DERRO  | 0        | Error code |
| 14           | RETAD  | OUT1Y    | Return address |
| 15           | NOADR  | 0        | Current ACM address |
| 16           | NODAT  | 0        | ACM data |
| 17           | XVSO   | 0        | X address screen 0 |
| 18           | YVSO   | 0        | Y address screen 0 |
| 21           | XVS1   | 0        | X address screen 1 |
| 22           | YVS1   | 0        | Y address screen 1 |
| 23           | XVS2   | 0        | X address screen 2 |
| 24           | YVS2   | 0        | Y address screen 2 |
| 25           | XVS3   | 0        | X address screen 3 |
| 26           | YVS3   | 0        | Y address screen 3 |
| 27           | SMTO   | 0        |             |
| 30           | SMT1   | 0        | Pointer to current selector module |
| 31           | SMT2   | 0        |             |
| 32           | SMT3   | 0        |             |
| 33           | RTR0   | 0        | Wait flag for screen 0-3 |
| 34           | RTR1   | 0        |             |
| 35           | RTR2   | 0        |             |
| 36           | RTR3   | 0        |             |
| 37           | CRTR   | 0        | Current RT ref. |
| 40           | SVNOA  | 0        | Save ACM address |

\# = ACM number = 1, trackerball number = 1 (y,q) = (1,1) 

---

ND-60.112.01

---

## Page 90

# DATA FIELD LAYOUT

**Library Mark:** Se file  
**Name:** SIN1-GEN TTILn

## Description

Communication Line Input (Not HDLC DMA)

| Relat. Addr. | Symbol  | Contents | Explanation                               |
|--------------|---------|----------|-------------------------------------------|
| -7           | CCTRL   | 20005#   | Hardware control word                     |
| -6           | TMSUB   | TAIMR#   | Time-out subroutine                       |
| -5           | TMR     | 0        | Time-out counter                          |
| -4           | TTMR    | —3       | Start value of TMR                        |
| -3           | HDEV    | IOX 1374#| IOX instructions                          |
| -2           | STDRIV  | CTDIR#   | Start address of driver                   |
| -1           | DRIVER  | CTDR#    | Restart address after interrupt           |
| 0            | RESLINK | 0        | Reservation link                          |
| 1            | RTRES   | 0        | Reserving RT program                      |
| 2            | BWLINK  | *—2      | Beginning of waiting queue                |
| 3            | TYPRING | 2        | Device type bits and ring                 |
| 4            | ISTATE  | 0        | 0 = idle, 1 = busy, —1 = no wait mode     |
| 5            | MLINK   | 0        | Monitor queue link                        |
| 6            | MFUNC   | IORES    | Monitor level function address            |
| 7            | CHKO    | 0        | Character pointer in input buffer         |
| 10           | STDEV   | CRWAI#   | Start device                              |
| 11           | ANCHA   | 0        | Character counter for input driver        |
| 12           | DFOPP   | TTULn    | Opposite data field for two-way devices   |
| 13           | IIN     | TEXIT#   | I/O initiating routine                    |
| 14           | NPBUP   | 0        | Current buffer chain used by input driver |
| 15           | SWICH   | 0        | Go switch for input driver                |
| 16           | PHEBUF  | 0        | Current buffer chain read by receive RT program |
| 17           | CUDBU   | 0        | Current buffer filled by driver           |
| 20           | IDBST(4)| 0,0,0,4  | Status of buffer chain n (0 = empty, 6 = filled) |
| 27           | IDBAD(4)| CH2,     | Address of first buffer in chain n        |
|              |         | CH430,0,0|                                           |

\# = for line 1 (n = 1)

---

**ND-60.112.01**

---

*Scanned by Jonny Oddene for Sintran Data © 2020*

---

## Page 91

# DATA FIELD LAYOUT

Library Mark | Name  
--- | ---  
Se file | SIN1-GEN TTILn  

## Description

**Communication Line Input (HDLC DMA)**

### Relat.

| Addr. | Symbol   | Contents | Explanation                                      |
|-------|----------|----------|--------------------------------------------------|
| -7    | CCTRL    | 20005#   | Hardware control word                            |
| -6    | TMSUB    | TAIMR#   | Time-out subroutine                              |
| -5    | TMR      | 0        | Time-out counter                                 |
| -4    | TTMR     | -3       | Start value of TMR                               |
| -3    | HDEV     | IOX 1374#| IOX instruction                                  |
| -2    | STDRIV   | CTDR#    | Start address of driver                          |
| -1    | DRIVER   | CTDR#    | Restart address after interrupt                  |
| 0     | RESLNK   | 0        | Reservation link                                 |
| 1     | RTRES    | 0        | Reserving RT program                             |
| 2     | BWLINK   | *-2      | Beginning of waiting queue                       |
| 3     | TYPRING  | 2        | Device type bits and ring                        |
| 4     | ISTATE   | 0        | 0 = idle, 1 = busy, -1 = no wait mode            |
| 5     | MLINK    | 0        | Monitor queue link                               |
| 6     | MFUNC    | IORES    | Monitor level function address                   |
| 7     | CHKO     | 0        | Character pointer in input buffer                |
| 10    | STDEV    | CRWAI#   | Start device                                     |
| 11    | ANCHA    | 0        | Character counter for input driver               |
| 12    | DFOPP    | TTULn    | Opposite data field for two-way devices          |
| 13    | HIN      | TEXIT#   | I/O initiating routine                           |
| 14    | LIBEG    | 0        | Start of buffer descriptor list                  |
| 15    | SWICH    | 0        | Go switch for input driver                       |
| 16    | HPCEK    | 0        | Current buffer chain read by receive RT program  |
| 17    | CUDBU    | 0        | Current buffer filled by driver                  |
| 20    | INIAD(7) |          | Initializing parameters for interface            |
| 27    | LIPOI(124) |        | Buffer descriptor list                           |

*# = for line 1 (n = 1)*

---

*Scanned by Jonny Oddene for Sintran Data © 2020*  
*ND-60.112.01*

---

## Page 92

# DATA FIELD LAYOUT

Library Mark | Name
--- | ---
Se file | TTULn
SIN1-GEN | 

Description  
Communication Line, Line n, Output

| Relat. Addr. | Symbol | Contents | Explanation |
| --- | --- | --- | --- |
| -4 | OCTRL | 5# | Output control word |
| -3 | HDEV | IOX 1374# | IOX instruction |
| -2 | STDRIV | CTODR# | Start address of driver |
| -1 | DRIVER | CTODR# | Restart address after interrupt |
| 0 | RESLINK | 0 | Reservation link |
| 1 | RTRES | 0 | Reserving RT program |
| 2 | BWLINK | *—2 | Beginning of waiting queue |
| 3 | TYPRING | 2 | Device type bits and ring |
| 4 | ISTATE | 0 | 0 = idle, 1 = busy, -1 = no wait mode |
| 5 | MLINK | 0 | Monitor queue link |
| 6 | MFUNC | IORES | Monitor level function address |
| 7 | CHKO | 0 | Character pointer in output buffer |
| 10 | STDEV | CSWAI | Start device |
| 11 | ANCHA | 0 | Character counter for output driver |
| 12 | DFOPP | TTILn | Opposite data field for two-way devices |
| 13 | RESICH | CRSVI# | Reset device routine |
| 14 | BUFST | 0 | Pointer to first buffer of current output buffer chain |
| 15 | SWICH | 0 | Go switch for output driver |
| 16 | LMDATA | CMOn | Pointer to communication data field |
| 17 | CUDBU | 0 | Current output buffer |
| 20 | SLIPO (25) | | Buffer descriptor for HDLC |

\# = for line 1 (n = 1)

ND-60.112.01

---

## Page 93

# Data Field Layout

Library Mark | Name
---|---
8SM0x | UDM0x
9SM0x | UDMxx

## Description

Synchronous Modem Output

### Relat.

| Addr. | Symbol | Contents | Explanation |
|-------|--------|----------|-------------|
| -12   | MOWSTAT | 0        | Switch for modem output |
| -11   | TERSW   | 0        | Terminate condition switch |
| -10   | MOSW    | 0        | Block switch |
| -7    | LAST    | 0        | Last character |
| -6    | TMSUB   | MOTMO    | Time-out subroutine |
| -5    | TMR     | 0        | Time-out counter |
| -4    | TMR     | -3       | Start value of TMR |
| -3    | HDEV    | IOX 104# | !OX instruction (i.e., IOX 104) |
| -2    | STDRIV  | MODUT    | Start address of driver |
| -1    | DRIVER  | MODUT    | Restart address after interrupt |
| 0     | RESLINK | 0        | Reservation link |
| 1     | RTRES   | 0        | Reserving RT program |
| 2     | BWLINK  | *_2      | Beginning of waiting queue |
| 3     | TYPRING | 110000   | Device type bits and ring |
| 4     | ISTATE  | 0        | 0 = idle, 1 = busy, -1 = no wait mode |
| 5     | MLINK   | 0        | Monitor queue link |
| 6     | MFUNC   | IORES    | Monitor level function address |
| 7     | IOTRANS | MOTRQ    | Called from INBT/OUTBT to transfer |
| 10    | STDEV   | TEXIT    | Start device |
| 11    | SETDV   | MOST0    | IOSET routine |
| 12    | DFOPP   | 0        | Not used |
| 13    | DERROR  | 0        | Error code |
| 14    | BUFST   | BUFO + BUF | Start of ring buffer |
| 15    | MAX     | MOBUX + MOBUX | Buffer capacity |
| 16    | BHOLD   | 0        | Number of characters in buffer |
| 17    | HENTE   | 0        | Fetch pointer |
| 20    | CFREE   | MOBUX + MOBUX 0 | Free positions |

# Note

`# = modem number 1 (x = 1)`

ND-60.112.01

---

## Page 94

# DATA FIELD LAYOUT

**Library Mark**
- 8DMVC
- 8DMV2

**Name**
- VEDOx

### Description
Versatec OUTBT

| Relat. Addr. | Symbol   | Contents | Explanation                                    |
|--------------|----------|----------|------------------------------------------------|
| —1           | ADRBHEAD | 0        | Buffer head address                            |
| 0            | RESLINK  | 0        | Reservation link                               |
| 1            | RTRES    | 0        | Reserving RT program                           |
| 2            | BWLINK   | *–2      | Beginning of waiting queue                     |
| 3            | TYPRING  | 112000   | Device type bits and ring                      |
| 4            | ISTATE   | 0        | 0 = idle, 1 = busy, –1 = no wait mode          |
| 5            | MLINK    | 0        | Monitor queue link                             |
| 6            | MFUNC    | IORES    | Monitor level function address                 |
| 7            | IOTRANS  | CBPUT    | Called from INBT/OUTBT to transfer             |
| 10           | STDEV    | TEXIT    | Start device                                   |
| 11           | SETOV    | VEICL    | IOSET routine                                  |
| 12           | DFOPP    | 0        | Opposite data field for two-way devices        |
| 13           | DERROR   | 0        | Error code                                     |
| 14           | BUFST    | 0        | Start of ring buffer                           |
| 15           | MAX      | DVEBz +  | Buffer capacity                                |
|              |          | DVEBz    |                                                |
| 16           | BHOLD    | 0        | Number of characters in buffer                 |
| 17           | HENTE    | 0        | Fetch pointer                                  |
| 20           | CFREE    | DVEBz+   | Free positions                                 |
|              |          | DVEBz    |                                                |
| 21           | FYLLE    | 0        | Store pointer                                  |
| 22           | CLOGDV   | 577      | Logical unit number for DMA data field         |
| 23           | DFDEV    | 576      | Logical unit number for DF data field          |
| 24           | LREGC    | 0        | Return address after IOTRANS is executed       |
| 25           | CASUN    | 0        | Device unit number                             |
| 26           | CEROR    | 0        | Current error code                             |
| 27           | LASTC    | 0        | Current character                              |
| 30           | NOWRE    | DVEBz    | Number of characters to read/write             |
| 31           | CPARM (5)| 1, * +, 3, *–6, *–4, 0 | Parameter list for MTRANS (including first word in memory buffer address) |
| 36           | MABUF    | 0        | Second word in memory buffer address           |
| 37           | VEFUNC   | 11       | Not used                                       |
| 40           | CIOLOG   | 22       | Logical device number                          |
| 41           |          |          |                                                |

ND-60.112.01

---

## Page 95

# DATA FIELD LAYOUT

**Library Mark:**  
8DMVC VEFIE  
8DMV2 VE2F1  

**Description:**  
Versatec DMA Controller  

## Relat. Addr, Symbol, Contents, Explanation

| Addr | Symbol   | Contents     | Explanation                                          |
|------|----------|--------------|------------------------------------------------------|
| -52  | ADNSTY(4)| 0,...,0      | Not used                                             |
| -46  | SHSTAT(4)| 0,...,0      | Saved last status (unit 0, 1, 2, 3)                  |
| -42  | CERRCODE | 0            | Current error code                                   |
| -41  | MAXUNIT  | 89NDV#       | Maximum unit number on this controller               |
| -40  | MACOU    | 0            | Erase counter                                        |
| -37  | MRETURN  | 0            | Not used                                             |
| -36  | MWRING   | 0            | Not used                                             |
| -35  | MWSTAT   | 0            | Not used                                             |
| -34  | MLOAD    | 0            | Load point status                                    |
| -33  | CLRG     | 0            | Saved L register                                     |
| -32  | TRG      | 0            | Register when calling driver (REAL = TADRG)          |
| -31  | ARG      | 0            | Register when calling driver (REAL = TADRG)          |
| -30  | DRG      | 0            | Register when calling driver (REAL = TADRG)          |
| -27  | XRG      | 0            | Register when calling driver (REAL = TADRG)          |
| -26  | CTRG     | 0            | Register when calling driver first time (REAL = CTRG)|
| -25  | CARG     | 0            | Register when calling driver first time (REAL = CTRG)|
| -24  | CDRG     | 0            | Register when calling driver first time (REAL = CTRG)|
| -23  | CXRG     | 0            | Register when calling driver first time (REAL = CTRG)|
| -22  | ERCNT    | 0            | Number of error returns from driver                  |
| -21  | SERB     | 0            | Not used                                             |
| -20  | VWRING   | 0            | Not used                                             |
| -17  | AERRB    | 0            | Accumulated error bits                               |
| -16  | TACNS    | -5           | Number of retrials wanted before message             |
| -15  | TACOUNT  | 0            | Retrial counter                                      |
| -14  | MVCOUNT  | 0            | Not used                                             |
| -13  | BLSZ     | DVEBZ#       | Block size                                           |
| -12  | TRNSF    | VEDR         | Driver address                                       |
| -11  | BUSY     | BUSYV        | Busy return                                          |
| -10  | FINISH   | FINIV        | Transfer finish                                      |
| -7   | ERROR    | FEILV        | Error return from driver                             |
| -6   | TMSUB    | VETMR        | Time-out subroutine                                  |
| -5   | TMR      | 0            | Time-out counter                                     |
| -4   | HTMR     | --200        | Start value TMR                                      |
| --3  | HDEV     | 600#         | Hardware device number (i.e., IOX 520)               |
| --2  | STDRIV   | CTRVE        | Start address of driver                              |
| --1  | DRIVER   | 0            | Restart address after interrupt                      |
| 0    | RESLINK  | 0            | Reservation link                                     |
| 1    | RTRES    | 0            | Reserving RT program                                 |
| 2    | BWLINK   | *--2         | Beginning of waiting queue                           |
| 3    | TYPERING | 6            | Device type bits and ring                            |
| 4    | ISTATE   | 0            | 0 = idle, 1 = busy, -1 = no wait mode                |
| 5    | MLINK    | 0            | Monitor queue link                                   |
| 6    | MFUNC    | RETRA        | Monitor level function address                       |
| 7    | TRLREG   | 0            | Return address on monitor level after transfer       |
| 10   | HSTAT    | 0            | Hardware status from driver                          |
| 11   | MTRANS   | MTRNS        | Monitor level routine to activate driver             |
| 12   | MRTRF    | 0            | Program calling CLOSE                                |
| 13   | BREGC    | 0            | Address of I/0 data field                            |
| 14   | MEMA1    | 0            | Initial memory address                               |
| 15   | MEMA2    | 0            | Initial memory address                               |
| 16   | CMAD1    | 0            | Current memory address                               |
| 17   | CMAD2    | 0            | Current memory address                               |
| 20   | CLEVEL   | VDMAC        | Address of clear device routine                      |
| 21   | PVEFUNC  | VED01 + 37#  | Address of VEFUNC in I/O data field                  |

\# = for controller number 1 (VEFIE)  
ND-60.112.01

---

## Page 96

I'm sorry, I cannot perform OCR on the image you provided.

---

## Page 97

# SECTION 4

## DEFINITION OF SOME WORDS

DFLAG Bits:  
(Relative address = -12)

| Bit | Name   | Explanation                                                          |
|-----|--------|----------------------------------------------------------------------|
| 0   | 5ECHO  | The driver may give echo                                             |
| 1   | 5BREAK | Break flag                                                           |
| 2   | 5SPEC  | Special break character. No echo on next character.                  |
| 3   | 5MDUP  | Half duplex                                                          |
| 4   | 5FMO   | Fixed line modem interface                                           |
| 5   | 5RQI   | SINTRAN III communication                                            |
| 6   | 5WRQI  | SINTRAN III communication                                            |
| 7   | 5XON   | Print XON as next output character (XON = 211)                       |
| 8   | 5XOFF  | Print XOFF as next output character (XOFF = 223)                     |
| 9   | 5XDEVICE | Used by TLIN driver                                                |
| 10  | 5OXON  |                                                                     |
| 11  | 5CAPITAL | Convert to capital letters                                         |
| 12  | 6XOFF  | Buffer almost full. XOFF is or should be printed                     |
| 13  | 5IESC  | Inhibit escape                                                       |
| 14  | 5LBRK  | Missing carrier                                                      |
| 15  | 5LBLOG | Logout on missing carrier                                            |

## TSPEED Table

4 Asynchronous Current Loop (1122 Card) and Dual Asynchronous V24 (1147 Card)

| Baud Rate | T Speed |
|-----------|---------|
| 50        | 42      |
| 75        | 63      |
| 110       | 377     |
| 134.5     | 104     |
| 150       | 356     |
| 200       | 125     |
| 300       | 335     |
| 600       | 146     |
| 1200      | 273     |
| 1800      | 252     |
| 2400      | 167     |
| 4800      | 231     |
| 9600      | 210     |

ND-60.112.01

---

## Page 98

# Terminal Buffer (1095 Card)

## Baud Rate and T Speed

| Baud Rate | T Speed |
|-----------|---------|
| 50        | 377     |
| 75        | 273     |
| 100       | 356     |
| 110       | 314     |
| 150       | 252     |
| 200       | 335     |
| 300       | 231     |
| 600       | 210     |
| 1200      | 63      |
| 2400      | 42      |
| 4800      | 21      |
| 9600      | 0       |

## Bits in TYPRING

| Bit | Name   | Explanation                           |
|-----|--------|---------------------------------------|
| 0   |        | Ring number                           |
| 1   |        |                                       |
| 2   | 5CLDV  | Clear device routine available        |
| 3   | 5NORES | No reservation necessary (terminal output) |
| 4   | 5REMPO | Communication channel used for remote file access |
| 5   | 5TERM  | Terminal                              |
| 6   | 5IBDV  | Internal block devices                |
| 7   | 5INVRT | Invert digital I/O                    |
| 8   | 5FLOP  | Floppy disk                           |
| 9   | 5MT    | Magnetic tape                         |
| 10  | M14AB  | Block monitor calls allowed           |
| 11  | 5COM   | Communication channel                 |
| 12  | 5ISET  | IOSET allowed                         |
| 13  | 5CONCT | CONCT allowed                         |
| 14  | 5RFILE | File                                  |
| 15  | 5IOBT  | INBT/OUTBT allowed                    |
| 9   | 5CRDLY | Carriage return delay in software     |

TYPRING bits 9-6 are used to delay carriage return in driver CRDLY.

ND-60.112.01

---

## Page 99

# Section 5

## Cross Reference of Library Marks

| Library Mark | Label Name(s)           |
|--------------|-------------------------|
| 7AIRx        | 82ONx                   |
| 7DIxx        | NDIxx                   |
| 7DOxx        | NDOxx                   |
| 7DPHx        | DPHOT, DPHOx            |
| 7GRCx        | GRxR, GRxW              |
| 7HARx        | HAxR, HAxW              |
| 7TEx         | DTELx, DTLxW            |
| 8BCHx        | BTxxR, BTxxW            |
| 8BDx         | BIGDI, BIGD2            |
| 8CACS1       | CADIx, CADOx, CAFIE     |
| 8CALC        | CALCO                   |
| 8CAS2        | CADIx, CADOx            |
| 8CDxx        | CDFxx                   |
| 8CPx         | CAPx                    |
| 8CRD         | IDV4                    |
| 8CRD2        | IDV42                   |
| 8DRIX        | DRFIE, DRFI2            |
| 8DLPx        | DMLPx, DILPx            |
| 8DMVC        | VEDOx, VEFIE            |
| 8DMV2        | VEDOx, VE2FI            |
| 8DRx         | DDRUM, DRUM2            |
| 8DVEx        | DMLPx, DILPx            |
| 8F16         | DMAX                    |
| 8FDIx        | FDIDx                   |
| 8FyUx        | FyUxI, FyUxO            |
| 8HDMx        | HDLFx, HDFOx, HDMIx, HDMOx |
| 8LPx         | DLPR, DLRPx             |
| 8M1U(x-1)    | MTDIx, MTDOx            |
| 8M2U(x-1)    | M2DIx, M2DOx            |
| 8MT2         | MTFIE, M2FIE            |
| 8N50         | DFN50                   |
| 8N50x        | DF50x                   |
| 8NCA2        | CALN2                   |
| 8NCAL        | CALNY                   |
| 8PUN1        | DPUNCH                  |
| 8PUN2        | DPUN2                   |
| 8REA1        | DREAR                   |
| 8REA2        | DREA2                   |
| 8S3C         | NDEMF, NDEM2            |
| 8SM0x        | IDM0x, IDMxx, UDM0x, UDMxx |
| 8SMxx        | SEMIx, SEMxx            |
| 99HMT        | MTFIE, M2FI2            |
| 99TMT        | MTFIE, MF2FI2           |
| 9ACMx        | DACMx                   |
| 9SM0x        | IDM0x, IDMxx, UDM0x, UDMxx |

ND-60.112.01

---

## Page 100

# List of Technical Codes

| Code 1 | Code 2 |
|--------|--------|
| C9CMx  | DACMx  |
| CmnLn  | SmnnM, SmnnW |
| IBLxx  | IBxxI, IBxxO, IDxxI, IDxxO |
| INDxx  | IDxxI, IDxxO |
| SLPx   |        |
| yGBz   | GRBz, GyBz |
| ySLx   | SLMx, SyMx |
| ySLPx  | SPPRx  |
| ySMz   | SMNz, SyNz |
| yTBq   | TBAq, TByAq |

ND-60.112.01

---

## Page 101

# ND

NORSK DATA A.S  
Postboks 4, Lindeberg gård  
Oslo 10, Norway  

## COMMENT AND EVALUATION SHEET

### SINTRAN III SYSTEM DOCUMENTATION

Appendix A – Data Fields

ND-60.112.01  
JUNE 1979  

In order for this manual to develop to the point where it best suits your needs, we must have your comments, corrections, suggestions for additions, etc. Please write down your comments on this pre-addressed form and post it. Please be specific wherever possible.

### FROM

---  
---  
---

---

## Page 102

I'm sorry, but it seems there is no content on the provided page. Please provide another image or document for conversion.

---

## Page 103

I'm sorry, but the image is too faint for me to extract any text.

---

## Page 104

# Norsk Data A.S

— we make bits for the future

| Address                          | Contact Details           |
|----------------------------------|---------------------------|
| Box 4 Lindeberg Gård, Oslo 10    | Phone: 30 90 30           |
| Norway                           | Telex: 18661              |

*Scanned by Jonny Oddene for Sintran Data © 2020*

---

