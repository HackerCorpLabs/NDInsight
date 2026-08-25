## Page 1

# ND Competition Newsletter

## CPU Speed Comparisons - Mini and Upwards

**December 1983**  
  
CORPORATE MARKETING

[Corporate Marketing logo or artwork]

---

## Page 2

# CPU-speed comparisons

Several ways of stating the CPU-speed of computers are used today. The most common ones are:

## MIPS (Million Instructions Per Second)

Often used for IBM and IBM-compatible systems. Can be interpreted in at least two ways:

1. The basic cycle time of a CPU is 200 ns, so it is a 1000 / 200 = 5 MIPS machine. (Some instructions take only one cycle to finish).
2. The basic cycle time of a CPU is 200 ns, and an average instruction takes 2.5 cycles to finish, so it is a 1000 / 200 / 2.5 = 2 MIPS machine. This way of calculating is normally used, but the problem is to define an average instruction, or a typical mix of instructions. Some instructions take 10 cycles or more to finish.

## Whetstone MIPS

A computer program (the Whetstone benchmark) has been defined, and is supposed to reflect an average program (mix of statements) in a university environment. The program itself calculates and outputs the Whetstone rating when it is run. A single and a double precision variant exists.

## KOPS (Kilo Operations Per Second)

This is often seen as a measurement unit for COBOL performance, but is not necessarily connected only to COBOL. The problem is to identify the operations whose speed is to be measured or calculated. A typical operation is a string compare or move, which can be considered sort of a macro-instruction.

## FLOPS (Floating Operations Per Second)

Normally used for array processors. If a processor executes a floating addition in 200 ns, and 6 such operations can be executed simultaneously, the speed is said to be 1000 / 200 x 6 = 30 MegaFLOPS.

It is easy to understand that comparisons based on these different measurement methods cannot be as accurate as we would like. However, if we disregard FLOPS, the other ratings (MIPS / Whetstone MIPS / KOPS) seem to be comparable with a maximum deviation of about plus/minus 15%.

IBM 4341-2 is a good example. One source says 4341-2's expected MIPS rating is 1.3, other sources say 1.2 and 1.1, and the Whetstone rating is claimed to be approximately 1.5 MIPS.

---

## Page 3

# Whetstone Performance Ratings

1.3 MIPS - 15% = 1.1 MIPS  
1.3 MIPS + 15% = 1.5 MIPS  

Be aware that the Whetstone rating is not always the highest number.

The following list includes the available MIPS, Whetstone MIPS (single precision) or KOPS (MOPS) rating for a large number of vendors and models. Globally optimized Whetstone-figures have not been used, as they tell more about the FORTRAN-compiler than the hardware speed. Low and high estimates are not given when only a single rating is known.

If you have comments, corrections or additions to the list, please let me know.

| CPU Model      | Low Estimate | Expected Value | High Estimate |
|----------------|--------------|----------------|---------------|
| **AMDAHL**     |              |                |               |
| 470 V/5        |              | 2.49           |               |
| 470 V/5-II     |              | 2.85           |               |
| 470 V/6        |              | 3.45           | 4.6           |
| 470 V/6-II     |              | 3.75           | 5.0           |
| 470 V/7 C      |              | 2.7            |               |
| 470 V/7 B      | 3.5          | 3.83           |               |
| 470 V/7 A      |              | 4.25           | 4.5           |
| 470 V/7        | 5.5          | 5.95           | 7.0           |
| 470 V/8        | 6.5          | 6.38           | 7.5           |
| 5840          |              | 7.5            |               |
| 5850          |              | 10.0           |               |
| 5860          |              | 13.0           |               |
| 5870          |              | 22.8           |               |
| 5880          |              | 23.4           |               |
| **APOLLO COMPUTER**  |       | 0.72           |               |
| DN 400, DN 420|              |                |               |
| **BTI COMPUTER SYSTEMS** |  |                |               |
| BTI 5000      |              | 0.3            |               |
| BTI 8000      |              | 0.59           |               |
| **BURROUGHS** |              |                |               |
| 1705          |              | 0.05           |               |
| 1707          |              | 0.06           |               |
| 1709          |              | 0.06           |               |
| 1713          |              | 0.07           |               |
| 1715          |              | 0.07           |               |
| 1717          |              | 0.08           |               |
| 1720          |              | 0.18           |               |
| 1726          |              | 0.18           |               |
| 1728          |              | 0.20           |               |
| 1776          |              | 0.10           |               |
| 1830          |              | 0.07           |               |
| 1860          |              | 0.18           |               |
| 1870          |              | 0.20           |               |
| 2700          |              | 0.10           |               |
| 2830          |              | 0.20           |               |
| 3700          |              | 0.17           |               |

[Scanned by Jonny Oddene for Sintran Data © 2023]

---

## Page 4

# Technical Data

| Model         | Value |
|---------------|-------|
| 3870          | 0.34  |
| 4700          | 0.34  |
| 4790 (2 CPU)  | 0.96  |
| 4840          | 0.72  |
| 6700          | 0.43  |
| 6750 (2 CPU)  | 0.68  |
| 6760 (3 CPU)  | 0.96  |
| 6803          | 0.38  |
| 6805          | 0.46  |
| 6806          | 0.45  |
| 6807          | 0.54  |
| 6808          | 0.55  |
| 6810          | 0.77  |
| 6811          | 0.77  |
| 6812          | 0.77  |
| 6817          | 1.15  |
| 6818          | 1.15  |
| 6821          | 1.26  |
| 6822          | 1.26  |
| 7550          | 0.85  |
| 7755          | 1.20  |
| 7750          | 1.30  |
| 7760          | 1.53  |
| 7760 (2 CPU)  | 2.04  |
| 7765          | 2.35  |
| 7770          | 1.95  |
| 7770 (3 CPU)  | 2.88  |
| 7775          | 3.00  |
| 7780          | 2.54  |
| 7785          | 3.90  |
| 7803          | 0.85  |
| 7805          | 0.90  |
| 7811          | 2.10  |
| 7821          | 4.00  |

## CAMBEX

| Model   | Value |
|---------|-------|
| B1955   | 0.27  |
| B2925   | 0.55  |
| B3920   | 0.64  |
| B6925   | 1.20  |
| B4955   | 2.10  |
| B7900-F | 5.70  |

### Model Numbers

| Model     | Value |
|-----------|-------|
| 1636-1    | 0.39  |
| 1636-10   | 0.60  |
| 1641-1    | 0.72  |
| 1641-11   | 0.90  |
| 1651-1    | 1.00  |

## CHARLES RIVER DATA SYSTEMS

| System          | Value |
|-----------------|-------|
| UNIVERSE 68/05  | 0.45  |
| UNIVERSE 68/37  | 0.45  |

## COMPUTER DESIGNED SYSTEMS

| System        | Value |
|---------------|-------|
| ADVISOR 32/60 | 4.20  |
| ADVISOR 32/80 | 14.60 |

## CONTROL DATA CORPORATION

| System     | Value |
|------------|-------|
| CDC 6200   | 0.35  |
| CDC 6400   | 0.47  |
|            | 0.56  |

---

## Page 5

# Technical Specifications

## CDC

| Model    | Value 1 | Value 2 |
|----------|---------|---------|
| CDC 6500 | 0.84    |         |
| CDC 6600 | 2.01    | 2.5     |
| CDC 6700 | 3.7     |         |
| CDC 7600 | 10.0    | 10.2    |

## CYBER

| Model                        | Value |
|------------------------------|-------|
| CYBER 170/815                | 0.9   |
| CYBER 170/825                | 1.5   |
| CYBER 170/835                | 3.5   |
| CYBER 170/845                | 5.4   |
| CYBER 170/855                | 8.0   |
| CYBER 170/865                | 11.0  |
| CYBER 170/875                | 19.0  |
| CYBER 170/865D               | 20.0  |
| CYBER 170/875D               | 32.0  |
| CYBER 170/720                | 1.25  |
| CYBER 170/720 BACK TO BACK   | 2.0   |
| CYBER 170/730                | 2.2   |
| CYBER 170/730 BACK TO BACK   | 4.0   |
| CYBER 170/750                | 7.5   |
| CYBER 170/760                | 10.3  |
| CYBER 170/4XX                | 15.0  |

## OMEGA

| Model       | Value |
|-------------|-------|
| OMEGA 480-I | 0.32  |
| OMEGA 480-II| 0.55  |
| OMEGA 480-III| 0.95 |

## 70 Series

| Model | Value 1 | Value 2 |
|-------|---------|---------|
| 71    | 1.2     |         |
| 72    | 1.00    | 2.0     |
| 73    | 1.2     | 1.3     |
| 74    | 2.0     | 2.5     |
| 76    | 3.12    | 5.12    |

## 170 Series

| Model                     | Value 1 | Value 2 |
|---------------------------|---------|---------|
| 171                       | 0.52    | 0.8     |
| 171 BACK TO BACK          | 1.2     |         |
| 172                       | 1.23    | 1.4     |
| 172 BACK TO BACK          | 2.00    |         |
| 173                       | 1.87    | 2.0     |
| 174                       | 2.81    | 3.0     |
| 175                       | 5.06    | 8.0     |
| 176                       | 9.36    | 15.0    |

## DATA GENERAL

| Model           | Value 1 | Value 2 |
|-----------------|---------|---------|
| NOVA 840        | 0.07    |         |
| ECLIPSE S-140   | 0.45    |         |
| ECLIPSE S-230   | 0.50    |         |
| ECLIPSE S-250   | 0.62    |         |
| ECLIPSE S-280   | 0.92    |         |
| MV 4000         | 0.43    | 0.6     |
| MV 4000 fpp     | 0.60    |         |
| MV 6000         | 0.60    |         |
| MV 8000 fpp     | 1.15    | 1.2     |
| MV 10000        | 2.5     |         |

## DIGITAL EQUIPMENT

| Model         | Value |
|---------------|-------|
| PDP 11/34     | 0.18  |
| PDP 11/44     | 0.31  |
| PDP 11/45     | 0.20  |
| PDP 11/55     | 0.71  |

---

## Page 6

# Technical Page

## VAX

| Model      | Value 1 | Value 2 | Value 3 |
|------------|---------|---------|---------|
| VAX 11/730 | 0.20    |         |         |
| VAX 11/730 fpa | 0.30 | 0.26    |         |
| VAX 11/750 | 0.40    |         |         |
| VAX 11/750 fpa | 0.70 | 0.72    |         |
| VAX 11/780 | 0.83    |         |         |
| VAX 11/780 fpa | 1.06 | 1.14    | 1.2     |
| VAX 11/782 | 1.82    | 1.94    | 2.0     |

## DEC

| Model       | Value 1 | Value 2 | Value 3 |
|-------------|---------|---------|---------|
| DEC 10 1040 | 0.16    |         | 0.17    |
| DEC 10 1050 KA | 0.17  |         |         |
| DEC 10 1060 | 0.5     |         |         |
| DEC 10 1070 KI | 0.5   |         |         |
| DEC 10 1080 | 0.83    |         |         |
| DEC 10 1090 KL | 0.83  |         |         |
| DEC 10 1091 | 1.3     |         |         |
| DEC 20 2040 | 0.46    | 0.5     |         |
| DEC 20 2050 | 0.83    | 1.14    |         |
| DEC 20 2060 | 1.3     |         |         |

## FORMATION

| Model                | Value |
|----------------------|-------|
| F1000 MODEL 100      | 0.20  |
| F1000 MODEL 200      | 0.32  |
| F1000 MODEL 200AP    | 0.36  |
| F1000 MODEL 300      | 0.24  |
| F1000 MODEL 300AP    | 0.38  |

## COULD SPL

| Model   | Value 1 | Value 2 | Value 3 |
|---------|---------|---------|---------|
| 32/67   | 0.29    |         |         |
| 32/75   | 0.60    |         |         |
| 32/77   | 0.59    | 0.60    |         |
| 32/7780 | 0.65    |         |         |
| 32/87   | 3.60    | 3.76    |         |
| 32/9705 | 2.6     |         |         |
| 32/8750 | 2.6     |         |         |
| 32/8780 | 5.4     | 6.60    | 6.66    |

## HARRIS

| Model   | Value |
|---------|-------|
| 80/100  | 0.56  |
| 600     | 0.76  |
| 700     | 0.89  |
| 800/850 | 1.46  |
| 1000    | 3.91  |

## HEWLETT PACKARD

| Model     | Value |
|-----------|-------|
| HP 2100S  | 0.80  |
| HP 3000 II | 0.19  |
| HP 3000 39 | 0.56  |
| HP 3000 42 | 0.56  |
| HP 3000 48 | 0.56  |
| HP 3000 68 | 1.1   |

## HONEYWELL

| Model   | Value |
|---------|-------|
| LEVEL 62| 0.08  |
| 62/100R | 0.10  |
| 62/60D  | 0.16  |

---

## Page 7

# Technical Specification

| Model          | Rating 1 | Rating 2 | Rating 3 |
|----------------|----------|----------|----------|
| 64/20          | 0.10     |          |          |
| 64/200         | 0.13     |          |          |
| 64/30          | 0.11     |          |          |
| 64/40          | 0.16     |          |          |
| 64/60          | [illegible] |         |          |
| 64 pps 320     | 0.21     |          |          |
| 64/50          | 0.27     |          |          |
| 64/05          | 0.27     |          |          |
| 66/05 (2 CPU)  | 0.41     |          |          |
| 66/07 (Time Shared) | 0.54 |        |          |
| 66/10          | 0.35     |          |          |
| 66/10 (2 CPU)  | 0.53     |          |          |
| 66/17 (Time Shared) | 0.70 |        |          |
| 66/20          | 0.56     |          |          |
| 66/20 (2 CPU)  | 1.01     |          |          |
| 66/27 (Time Shared) | 1.12 |        |          |
| 66/40          | 0.90     |          |          |
| 66/140 (2 CPU) | 1.62     |          |          |
| 66/60          | 1.27     |          |          |
| 66/60 (2 CPU)  | 2.28     |          |          |
| 66/80          | 1.30     |          |          |
| 66/80 (2 CPU)  | 2.34     |          |          |

| DPS Model      | Rating 1 | Rating 2 |
|----------------|----------|----------|
| DPS 6/92       | 0.50     |          |
| DPS 8/20       | [illegible] |        |
| DPS 8/44       | 0.71     |          |
| DPS 8/17       | 0.73     |          |
| DPS 8/49       | 1.1      |          |
| DPS 8/52       | 1.1      | 1.20     |
| DPS 8/62       | 1.2      |          |
| DPS 8/70       | 1.8      | 1.99     |
| DPS 8/70 (2 CPU) | [illegible] |      |
| DPS 8/70 (3 CPU) | 5.01    |         |
| DPS 8/70 (4 CPU) | 6.51    | 7.2     |
| DPS 88/91      | 7.2      |          |
| DPS 88/??      | 13.0     |          |

# IBM Models

| Model          | Rating 1 | Rating 2 | Rating 3 |
|----------------|----------|----------|----------|
| 20             | 0.01     |          |          |
| 22             | 0.04     |          |          |
| 25             | 0.04     |          |          |
| 30             | 0.04     | 0.06     |          |
| 40             | 0.07     |          |          |
| 41             | 0.08     |          |          |
| 50             | 0.10     |          |          |
| 65             | 0.57     |          |          |
| 67             | 0.24     |          |          |
| 75             | 0.70     |          |          |
| 85             | 2.10     |          |          |
| 91             | 2.20     |          |          |
| 115-0          | 0.06     | 0.08     |          |
| 115-2          | 0.08     | 0.09     |          |
| 125            | 0.07     | 0.08     | 0.08     |
| 125-2          | 0.09     | 0.11     | 0.11     |
| 135-0          | 0.11     | 0.15     | 0.20     |
| 135-3          | 0.20     |          |          |
| 13             | 0.14     | 0.20     | 0.25     |

---

## Page 8

# Technical Data

| Item      | Value 1 | Value 2 | Value 3 |
|-----------|--------|--------|--------|
| 145       | 0.26   | 0.31   | 0.39   |
| 145-3     |        | 0.43   |        |
| 148       | 0.38   | 0.44   | 0.50   |
| 155       |        | 0.50   | 0.55   |
| 155-2     | 0.44   | 0.50   | 0.56   |
| 158-1     | 0.81   | 0.94   | 1.00   |
| 158-3     | 0.90   | 1.00   | 1.10   |
| 158-1 AP/MP| 1.50  | 1.56   | 1.75   |
| 158-3 AP/MP| 1.60  | 1.80   | 2.00   |
| 165       | 1.90   | 2.06   |        |
| 165-2     | 1.81   | 2.06   | 2.31   |
| 168-1     | 2.13   | 2.44   | 2.50   |
| 168-3     | 2.50   | 2.70   | 3.00   |
| 168 AP/MP | 3.75   | 4.13   | 4.31   |
| 195       | 4.75   | 5.00   |        |

## Subsection 3031

| Item      | Value 1 | Value 2 | Value 3 |
|-----------|--------|--------|--------|
| 3031      | 1.05   | 1.20   | 1.30   |
| 3031 A+APU| 1.90   | 2.10   | 2.30   |

## Subsection 3032 and 3033

| Item      | Value 1 | Value 2 | Value 3 |
|-----------|--------|--------|--------|
| 3032      | 2.50   | 2.70   | 3.00   |
| 3033 S    | 2.60   | 2.90   | 3.00   |
| 3033 N    | 3.50   | 3.90   | 4.30   |
| 3033 U    | 4.50   | 5.00   | 5.90   |
| 3033 A+APU/MP| 8.40 | 9.00   | 9.70   |

## Other Data

| Item      | Value 1 | Value 2 | Value 3 |
|-----------|--------|--------|--------|
| 4321-11   | 0.20   | 0.20   | 0.30   |
| 4331-1    | 0.19   | 0.25   | 0.25   |
| 4331-11   | 0.26   | 0.26   | 0.40   |
| 4331-2    | 0.38   | 0.38   | 0.50   |
| 4341-9    |        | 0.40   | 0.60   |
| 4341-10   |        | 0.58   | 0.70   |
| 4341-1    | 0.72   | 0.72   | 0.90   |
| 4341-11   | 0.88   | 0.88   | 1.10   |
| 4341-2    | 1.10   | 1.30   | 1.50   |
| 4341-12   |        | 1.30   | 1.50   |
| 4361-4    | 0.66   | 0.70   | 1.00   |
| 4361-5    |        | 1.14   | 1.20   |
| 4381-1    | 2.0    | 2.1    | 2.1    |
| 4381-2    | 2.5    | 2.7    | 3.0    |

## System Data

| Item      | Value  |
|-----------|--------|
| 8130 A    | 0.20   |
| 8130 B    | 0.03   |
| 8140      | 0.36   |

### System Values

| System    | Value  |
|-----------|------- |
| Sys/3     | 0.06   |
| Sys/32    | 0.02   |
| Sys/34    | 0.11   |
| Sys/36    | 0.13   |
| Sys/38-4  | 0.20   |
| Sys/38-5  | 0.24   |
| Sys/38-7  | 0.52   |
| Sys/38-8  | 0.52   |

## Miscellaneous Data

| Item      | Value 1 | Value 2 | Value 3  |
|-----------|--------|--------|---------|
| 3083 E    | 3.1    | 3.1    | 4.0     |
| 3083 B    | 5.2    | 5.5    | 6.0     |
| 3083 J    | 7.2    | 7.5    | 3.3     |

---

## Page 9

# Technical Specifications

## DEC

| Model       | Specification 1 | Specification 2 | Specification 3 |
|-------------|----------------|----------------|----------------|
| 3061 G      | 10.2           | 16.7           | 11.4           |
| 3061 K      | 13.0           | 11.0           | 8.8            |
| 3061 Q (4 CPU) | 26.0         | 27.0           | 33.0           |

## ICL

| Model     | Value |
|-----------|-------|
| 1902A     | 0.03  |
| 1902T     | 0.13  |
| 1903S     | 0.10  |
| 1904S     | 0.30  |
| 2903      | 0.05  |
| 2904      | 0.13  |
| 2950      | 0.18  |
| 2950      | 0.32  |
| 2970      | 0.55  |
| 2976      | 0.70  |
| 2970 (2 CPU) | 0.99 |
| 2980      | 2.00  |
| 2980 (2 CPU) | 3.80 |

## INTERDATA

| Model | Value |
|-------|-------|
| 8/32  | 0.58  |

## IPL SYSTEMS

| Model   | Value |
|---------|-------|
| 41136   | 0.43  |
| 41139   | 0.79  |
| 41145   | 0.94  |
| 41146   | 1.2   |
| 41460   | 1.3   |
| 41480   | 2.2   |

## ITEL (NATIONAL ADVANCED SYSTEMS)

| Model      | Value |
|------------|-------|
| AS/3I      | 0.50  |
| AS/3I MP   | 1.00  |
| AS/5-1     | 0.83  |
| AS/5-1 MP  | 1.41  |
| AS/5-3     | 0.90  |
| AS/5-3 MP  | 1.53  |
| AS/6       | 3.00  |
| AS/6-2     | 3.00  |
| AS/6620    | 1.6   |
| AS/6630    | 2.0   |
| AS/6650    | 2.4   |
| AS/7010N   | 1.8   |
| AS/7000    | 2.7   |
| AS/7000 PPC| 5.4   |
| AS/8040    | 4.9   |
| AS/8050    | 6.1   |
| AS/8060    | 7.8   |
| AS/9940    | 7.2   |
| AS/9050    | 0.0   |
| AS/9060    | 11.2  |
| AS/9070    | 16.2  |
| AS/9080    | 20.0  |

## MAGNUSON

[No details provided]

---

## Page 10

# Microdata

## SEQUEL MS/3200
- 0.54

# Modular Computer Systems

| System        | Value |
|---------------|-------|
| MODCOMP 7860  | 0.78  |
| MODCOMP 7870  | 0.78  |

# Nanodata

| System    | Value |
|-----------|-------|
| VMX 200   | 0.35  |
| VMX 400   | 0.65  |
| QMX 6333  | 0.38  |
| QMX 6336  | 0.55  |
| QMX 6343  | 0.88  |

# NCR

| System           | Value |
|------------------|-------|
| V-8455           | 0.18  |
| V-8545-II        | 0.2   |
| V-8555 M         | 0.28  |
| V-8555 MP        | 0.28  |
| V-8555-II        | 0.25  |
| V-8560           | 0.28  |
| V-8565 M         | 0.42  |
| V-8565 MP        | 0.66  |
| V-8565           | 0.66  |
| V-8565-II        | 0.37  |
| V-8565-II E      | 0.42  |
| V-8570           | 0.38  |
| V-8570 MP        | 0.38  |
| V-8575 M         | 0.58  |
| V-8575 MP        | 0.88  |
| V-8575-II (2 CPU)| 0.61  |
| V-8585 M         | 0.78  |
| V-8585 MP        | 1.34  |
| V-8585-II (2 CPU)| 0.75  |
| V-8595-II (2 CPU)| 0.77  |
| V-8635           | 1.0   |
| V-8645 (2 CPU)   | 1.9   |
| V-8650           | 2.65  |
| V-8655 (2 CPU)   | 2.0   |
| V-8665 (3 CPU)   | 2.8   |
| V-8670           | 4.29  |
| V-8675 (4 CPU)   | 3.7   |
| V-8685 (6 CPU)   | 5.5   |
| V-8695 (8 CPU)   | 7.3   |

# Nixdorf

| Model        | Value |
|--------------|-------|
| 8890 MODEL 30| 0.25  |
| 8890 MODEL 50| 0.5   |
| 8890 MODEL 70| 0.7   |

# Norsk Data

- [No data visible on the page]

---

## Page 11

# Hardware Specifications

## Satellite

| Model                          | Value |
|-------------------------------|-------|
| ND-100/Compact                | 0.22  |
| ND-100 Compact T, II, III, IV | 0.22  |
| ND-100 Compact/CX I, II, III, IV | 0.22  |
| ND-100                        | 0.22  |
| ND-100/CX                     | 0.32  |

## ND Series

| Model  | Value |
|--------|-------|
| ND-520 | 0.96  |
| ND-540 | 1.80  |
| ND-550/CX | 0.96  |
| ND-560 | 1.80  |
| ND-560/CXA | 1.80 |
| ND-570/CXA | 3.40 |

## Perkin Elmer

| Model  | Value |
|--------|-------|
| 3205   | 0.17  |
| 3210   | 0.34  |
| 3230   | 0.60  |
| 3240 FPA | 1.10 |
| 3250 FPA | 1.10 1.20 |
| 3200 MPS |  |  

## Prime

| Model      | Value  |
|------------|--------|
| 150/250    | 0.28   |
| 250-II     | 0.50   |
| 2250       | 0.47   |
| 450/550    | 0.47   |
| 500        | 0.52   |
| 550-II     | 0.64 0.67 |
| 650        | 0.61   |
| 750        | 1.0    |
| 850 (? CPU)| 1.65 1.90 |
| 9950       | 2.5    |

## SEMS

| Model         | Value |
|---------------|-------|
| MITRA 125     | 0.12  |
| SOLAR 16/65   | 0.110 |
| SOLAR 16/75   | 0.63  |

## Sperry (UNIVAC)

| Model         | Value |
|---------------|-------|
| 1100/11       | 0.39  |
| 1100/11 OVERLAP | 0.61 0.62 |
| 1100/12       | 1.04  |
| 1100/20       | 0.65  |
| 1100/21       | 1.2   |
| 1100/41       | 1.07  |
| 1100/42       | 1.92  |
| 1100/43       | 2.8   |
| 1100/44       | 3.62  |
| 1100/60 C1    | 0.54  |
| 1100/60 C2    | 0.68  |
| 1100/60 H1    | 1.16  |
| 1100/60 H2    | 1.50  |
| 1100/61 C1    | 0.56  |
| 1100/61 C2    | 0.67  |
| 1100/61 H1    | 1.12 1.20 |
| 1100/61 H2    | 1.31  |

---

## Page 12

```
# Technical Data

| Code       | Value 1 | Value 2 | Value 3 |
|------------|---------|---------|---------|
| 1100/62 E1 | 1.50    |         |         |
| 1100/62 E2 | 1.70    |         |         |
| 1100/62 H1 | 2.24    |         |         |
| 1100/62 H2 | 2.80    |         |         |
| 1100/71 H1 | 1.2     |         |         |
| 1100/80    | 1.25    |         |         |
| 1100/81    | 1.80    | 1.83    | 2.0     |
| 1100/82    | 3.36    |         | 4.5     |
| 1100/83    | 5.04    |         | 5.9     |
| 1100/84    | 6.40    |         | 8.4     |
| 1100/91    | 7.5     |         |         |
| 1100/92    | 14.0    |         |         |
| 1100/93    | 20.0    |         |         |
| 1100/94    | 25.0    |         |         |
| 1106       | 0.4     | 0.42    |         |
| 1106 II    | 0.57    |         |         |
| 1108       | 0.76    |         |         |
| 1108 MP    | 1.29    |         |         |
| 1110 (1 BY 1) | 1.14 |         |         |
| 1110 (2 BY 2) | 1.94 |         |         |
| 1110 (4 BY 4) | 3.30 |         |         |
| 90/25      | 0.10    |         |         |
| 90/30 B    | 0.14    |         |         |
| 90/30      | 0.14    |         |         |
| 90/40      | 0.21    |         |         |
| 90/60      | 0.24    | 0.28    |         |
| 90/60 E    | 0.31    |         |         |
| 90/70      | 0.30    | 0.33    |         |
| 90/80      | 0.83    |         |         |
| 90/80-1    | 0.80    |         |         |
| 90/80-2    | 0.60    | 0.80    |         |
| 90/80-3    | 0.78    | 0.80    | 0.82    |
| 90/80-4    | 1.10    |         |         |
| 9200       | 0.02    |         |         |
| 9300       | 0.04    |         |         |
| 9400       | 0.11    |         |         |
| 9480       | 0.11    |         |         |
| 9700       | 0.34    |         |         |
| 70/2 (RCA) | 0.11    |         |         |
| 70/3 (RCA) | 0.08    | 0.11    |         |
| 70/6 (RCA) | 0.30    | 0.49    |         |
| 70/7 (RCA) | 0.29    | 0.49    |         |
| 70/35 (RCA)| 0.05    |         |         |
| 70/45 (RCA)| 0.11    |         |         |
| 70/46 (RCA)| 0.08    | 0.11    |         |
| 70/55 (RCA)| 0.24    | 0.38    |         |
| 70/60 (RCA)| 0.30    | 0.49    |         |
| 70/61 (RCA)| 0.29    | 0.49    |         |
| 70/71 (RCA)| 0.49    |         |         |
```

---

