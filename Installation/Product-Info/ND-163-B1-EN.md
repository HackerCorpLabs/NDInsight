## Page 1

# ND163 BUS RECEIVER

## Introduction

The ND163 – BUS RECEIVER – controls the use of the standard I/O Bus for CPU controlled interfaces and DMA controllers. The ND163 also serves as a branch point and a buffer/driver for the interfaces/controllers in the BUS RECEIVERS' associated LOCAL I/O BUS.

ND174 – 16 MEMORY ADDRESS REGISTERS – contain 16 16 bit Memory Address Registers which are used by the DMA devices communicating on the LOCAL I/O BUS.

## Features

- Simple interfacing to LOCAL I/O BUS
- 3-state LOCAL I/O BUS for increased speed
- 4 layer printed circuit back-plane for increased noise immunity
- LOCAL I/O BUS provides separate bidirectional 16 bit data and 18 bit address lines for increased performance
- 4 classes of vectored I/O interrupts and one level of DMA interrupt

## Diagram

```mermaid
graph TB
    A(MAIN I/O BUS from CPU/previous ND163)
    classDef default className fill:#fff,stroke:#333,stroke-width:1px;
    
    subgraph "BUFFER/DRIVERS"
      direction LR
      B[Data 16]
      C[Addr 18]
      D[Contr. 13]
    end
    
    E[16 LOCAL I/O BUS]
    F[18]
    G[16]
    
    A --> B --> E
    A --> C --> F
    A --> D --> G
    
    class B,C,D fill:#eee,stroke:#333;
    class E,F,G fill:#eee,stroke:#333;
    
    H(ND 163 - BUS Receiver)
    
    E --> H
    F --> H
    G --> H
    
    subgraph "LOGICAL CONFIGURATION"
      direction TB
      I(MAIN I/O BUS to next ND163)
    end
```

---

163–B1–1800–0179

Scanned by Jonny Oddene for Sintran Data © 2010

---

## Page 2

# Product Description

Physically, the ND163 — BUS RECEIVER — consists of a 19" rack with a printed circuit back-plane to accommodate the BUS RECEIVER, the ND174 – 16 Memory Address Registers, the ND164 – Bus Brancher and 8 I/O slots. By adding a ND169 — Bus Extender, 8 additional I/O slots are available.

The BUS RECEIVER may drive up to 16 I/O interfaces/ DMA controllers and 8 positions are always reserved for a DMA controller. The BUS RECEIVER can control both CPU controlled and DMA transfers.

# Specifications

| Specification                        | Details                                     |
|--------------------------------------|---------------------------------------------|
| Maximum number of I/O slots          | 16 (requires an additional ND169)           |
| Main I/O Bus                         | Differential lines                          |
| Local I/O Bus                        | - 3-state <br> - 18 address lines <br> - 16 data lines <br> - 16 control lines (includes 5 interrupt lines) |

```plaintext
                 Main I/O Bus to/from CPU or Bus Receiver (Differential lines)
                                    | 
                                    | 
         +--------------------------|---------------------------------------------------
         |                          | 
         |                          |
         ⇓                          |                     Local I/O Bus
Add     Data       Contr.           | 
+---+---+---+---+---+---+-----------|----+---+---+---+---+---+---+---+---+---+---+---+---
| 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |   | 9 |10 |11 |12 |13 |14 |15 |16 |17 |18 |19 |20 |21 |22 |23 |24 |
+---+---+---+---+---+---+---+---+---|----+---+---+---+---+---+---+---+---+---+---+---+---+
 |    |   |   |   |   |   |         | 
 |    |   |   |   |   |   *---------|----------------------------------------------+
 |    |   |   |   |   *-------------|------------------------------------------+   | 
 |    |   |   |   *-----------------|-------------------------------------+    |   | 
 |    |   |   *---------------------|------------------------------+      |    |   |
 |    |   *-------------------------|-------------------------+    |      |    |   |
 |    *-----------------------------|--------------------+    |    |      |    |   |
 *---------------------------------|-------------+      |    |    |      |    |   |
                                      DMA          Data  |     |    |      |    |
                                      Addr.         ⇓   |     |    |      |    |
                                      **ND174:   ┌----┐ |    |    |      |    |
                                       M. Addr** |    | |    |    |      |    |
                                             Reg |    | |    ⇓    |      |    |
                                                 |    | +----^.   |      |    |
                                                 |    |     | |   |      |    |
                                                 └----┘     | |   |      |    |
                                                          *--*|   |      |    |
                                                               |   |      |    |
                                                               +---+------|----+---+---+
       
                                                    ND164: Bus Brancher Data - 1093
                                                    ND174: 16 Memory Address Registers  1096

          8 I/O Slots
          ND169 Bus Extender

- Reserved for any Data
  Channel Controller

```

## Contact Information

| Company                    | Address/Contact Info                                    |
|----------------------------|----------------------------------------------------------|
| NORSK DATA A.S             | Lindebergsvn. nord 20, Box 4 - Lindeberg gård Oslo 10, NORWAY <br> Tel. 02-391601, Tlx. 18661 nd n |
| NORSK DATA A/S             | Øverødvej 5 2840 Holte, DENMARK <br> Tel. 02-425055 |
| NORSK DATA DEUTSCHLAND     | Abraham-Lincoln-Str. 30 6200 Wiesbaden, WEST GERMANY <br> Tel. 06121-764220, Tlx. 4186370 noda |
| ND NORSK DATA AB           | Kanalvagen 3, Box 2031 194 02 Upplands Väsby, SWEDEN <br> Tel. 076-86500, Tlx. 13528 nordata s |
| NORSK DATA FRANCE          | "Le Brevent", Avenue du Jura 1210 Ferney-Voltaire, FRANCE <br> Tel. 050-408576, Tlx. 385653 nordata fernv |
| ND NORSK DATA AB           | Klangfärgsgatan 11, Box 9052 421 09 Västra Frölunda, SWEDEN <br> Tel. 031-299350 |
| NORSK DATA FRANCE          | 120 Bureau de la Colline 92213 Saint Cloud, FRANCE <br> Tel. 01-6023367, Tlx. 210108 nd paris |
| NORSK DATA N.A., Inc.      | 65, William Street Wellesley, Mass. 02181, USA <br> Tel. 0617-237-7945 |
| RICHARD NORTON (NORD) Ltd. | NORD House, 17 Balfre Street, King's Cross London N1 9EB, ENGLAND <br> Tel. 01-2785501, Tlx. 299537 |

**NOTE:** Norsk Data reserves the right to change specifications at any time without given notice!

---

