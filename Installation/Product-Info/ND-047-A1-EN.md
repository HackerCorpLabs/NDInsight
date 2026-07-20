## Page 1

# ND 047 WRITABLE CONTROL STORE (WCS)

## INTRODUCTION

The ND 047 — Writable Control Store (WCS) enables the user to write his own microprogram. This microprogram can define special functions or instructions which enhance the performance when the computer is used for a special application.

## FEATURES

- Enables the user to enhance the CPU performance by defining up to 12 functions/instructions
- The instructions/functions can individually be defined as privileged instructions/functions
- Easy load procedure
- Microprogram selective microcycle execution speed (150/180 ns)

## PRODUCT DESCRIPTION

The Writable Control Store (WCS) is located on the NORD-100 CPU board. The size of the WCS is 1/4K by 64 bits. The WCS can be regarded as an optional addition to the PROM. The address range for the WCS is from 4000₈ to 4377₈.

The WCS can be loaded by executing the privileged instruction Load Writable Control Store (LDWCS, 143500). The microprogram to be loaded into the WCS must reside in main memory in the physical page number 15 (36000₈ — 37777₈). The correspondence between main memory and WCS is given in the illustration above.

## SPECIFICATIONS

| Word length                        | 64 bits          |
|------------------------------------|------------------|
| Size                               | 1/4K             |
| Load time (executing LDWCS, 143500)| 635 μs (using local memory)   |
| Number of instructions/routines located in WCS | <12          |

```mermaid
flowchart TB
    subgraph WCS ["40004000" --- "11114377"]
        direction TB
        line1["C03 | C02 | C01 | C00"]
        line2["C13 | C12 | C11 | C10"]
        line3["etc. | | | C20"]
    end
    subgraph PROM["2K PROM"]
    end
    subgraph MAINMEM["Main Memory"]
        direction TB
        range1["C00"]
        range2["C01"]
        range3["C02"]
        range4["C03"]
        range5["C10"]
        range6["C11"]
        range7["etc."]
    end
    WCS --> PROM
    MAINMEM --> WCS
    style WCS fill:none,stroke:#333,stroke-width:2px
    style PROM fill:none,stroke:#333,stroke-width:2px
    style MAINMEM fill:none,stroke:#333,stroke-width:2px
```

[Logo: Nord Computer Systems]

---

## Page 2

# Contact Information

## Norway
**NORSK DATA A.S**  
Jerikovien 20, Box 4 Linderberg gård  
OSLO 10  
Tel. 02-391601, Tlx. 18661 nd n  

## Denmark
**NORSK DATA ApS**  
Øverødvej 5  
2840 HOLTE  
Tel. 02-425055, Tlx. 37725 nd dk  

## West Germany
**NORSK DATA DEUTSCHLAND**  
Abraham-Lincoln-Str. 30  
6200 WIESBADEN  
Tel. 06121-764220, Tlx. 4186370 noda d  

## Sweden
**ND NORSK DATA AB**  
Kanalvägen 3, Box 231  
194 02 UPPLANDS VÄSBY  
Tel. 0760-86500, Tlx. 13528 nordatas s  

## France
**NORSK DATA FRANCE**  
"Le Brévent", Avenue du Jura  
01210 FERNEY-VOLTAIRE  
Tel. 050-408576, Tlx. 385653 nordata fernv  

## U.S.A.
**NORSK DATA N.A., Inc.**  
65, William Street  
Wellesley, MASS. 02181  
Tel. 0617-237.7945, Tlx. 921740 norsk well  

## Sweden
**ND NORSK DATA AB**  
Klangfå...

421 09 VÄSTRA FRÖLUNDA  
Tel. 031-299350  

## France
**NORSK DATA FRANCE**  
120, Bureaux de la Colline  
92113 SAINT-CLOUD-CEDEX  
Tel. 01-6023266, Tlx. 201108 nd paris  

## England
**RICHARD NORTON (NORD) Ltd.**  
NORD House, 17 Balfe Street, King’s Cross  
LONDON N1 9EB  
Tel. 01-2785501, Tlx. 299537 norton g  

---

**Note:** NORSK DATA reserves the right to change specifications at any time without given notice.

---

