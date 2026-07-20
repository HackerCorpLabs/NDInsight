## Page 1

# ND Computer Systems

## Multiplexer Diagram

```mermaid
flowchart LR
    A[22 bit ADDRESS] --> B[ADDRESS A]
    A --> C[ADDRESS B]
    A --> D[ADDRESS C]
    A --> E[ADDRESS D]

    B --> F
    C --> F
    D --> F
    E --> F

    F[CONTROL Multiplex Addr MUX Address 2/4 to 1 MUX] --> G[DATA A]
    F --> H[DATA B]
    G --> I[DATA A]
    H --> J[DATA B]
    I --> K[DATA C]
    J --> L[DATA D]

    G --> M[D Multiplexed Multiplexer Channel 22 bit ADDRESS 16 or 32 bit DATA]
    M --> N[Most significant 16 bits (ND 159 only)]
    K --> N
    L --> N
    N --> O[DATA A]
    N --> P[DATA B]
    N --> Q[DATA C]
    N --> R[DATA D]
```

## ND 150 16 BIT MEMORY MULTIPLEXER
## ND 159 32 BIT MEMORY MULTIPLEXER

### Introduction

The ND 150 and ND 159 16 or 32 bit Multiplexers are used to expand the number of channels into a port in a Multiport Memory System from one to a maximum of 4.

Normally the Multiplexers are used in connection with Shared Memory. If all 4 ports on a Shared Memory has a Multiplexer as input, a maximum of 16 channels may be connected.

### Features

- Up to 4 Memory Channels may be multiplexed onto one.
- Accepts 16 or 32 bit wide channel with 22 bit address (ND 150 and ND 159 respectively).
- Low MUX overhead.

150/159-A2-1500-1178

Scanned by Jonny Oddene for Sintran Data © 2010

---

## Page 2

# Product Description

The ND 150 and ND 159 Multiplexers come in a 19" rack complete with all electronics and wiring for 4 Memory Channels. Each memory channel has Lower and Upper Limit Switches such that each channel's address space may be specified to be anywhere within the total address space of 4 Mwords (22 bits).

There is a fixed and linear priority between the 4 channels, with channel A with the highest and channel D with the lowest priority.

# Specifications

| Description                                                       | Value              |
|-------------------------------------------------------------------|--------------------|
| Worst case MUX delay on a memory cycle for a port                 | 230 ns             |
| Additional latency: Previous port is finished and is different from current port | 90 ns (worst case) |

# Contact Information

| Country       | Contact Information                                                                                   |
|---------------|-------------------------------------------------------------------------------------------------------|
| **NORWAY:**   | Norsk Data A.S<br>Lindebergvn. nord 20, Box 4 - Lindeberg gård<br>OSLO 10<br>Tel. 02/391601, Tlx. 18661 nd n |
| **DENMARK:**  | Norsk Data ApS<br>Øverødvej 5<br>2840 HOLTE<br>Tel. 02-425055                                         |
| **WEST GERMANY:** | Norsk Data Deutschland GmbH<br>Abraham-Lincoln-Strasse 30<br>6200 WIESBADEN<br>Tel. 06121-764220, Tlx. 4186370 noda |
| **SWEDEN:**   | ND Norsk Data AB<br>Kanalvägen 3, Box 2031<br>194 02 UPPLANDS VÄSBY<br>Tel. 0760-86500, Tlx. 13528 nordata s |
| **FRANCE:**   | Norsk Data France<br>"Le Brevent", Avenue du Jura<br>01210 FERNEY-VOLTAIRE<br>Tel. 050-405876, Tlx. 38563 nordata fernv |
| **U.S.A.:**   | Norsk Data N.A., Inc.<br>65, William Street<br>Wellesley, MASS. 02181<br>Tel. 0617-237.7945           |
| **SWEDEN:**   | ND Norsk Data AB<br>Käringfästagatan 11, Box 9052<br>421 09 VÄSTRA FRÖLUNDA<br>Tel. 031-299350        |
| **FRANCE:**   | Norsk Data France<br>120 Bureau de la Colline<br>92213 SAINT CLOUD<br>Tel. 01-6032367, Tlx. 201100 nd paris |
| **ENGLAND:**  | Richard Norton (NORD) Ltd.<br>NORD House, 17 Balfe Street, King's Cross<br>LONDON N1 9EB<br>Tel. 01-2785501, Tlx. 299537 |

**NOTE:** Norsk Data reserves the right to change specifications at any time. It is our policy to improve products as new techniques and components become available.

---

