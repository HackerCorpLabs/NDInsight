## Page 1

# ND 271 Terminal Interface, 4 lines, ND-100
# ND 272 Terminal Interface, 8 lines, ND-100

## Product Description

Each channel is a complete full duplex asynchronous interface to the ND-100 machine, capable of acting as either a 20 mA current loop or an RS-232-C. All common baud rates from 50 to 9600 baud are available.

20 mA current loop or RS-232-C is switch selectable for each channel. Baud rate is either thumbwheel selectable for each group of four channels or programmable for each channel.

Parity, character length and stop-bits are programmable for each channel.

In current loop mode each channel is electrically isolated from each other and from the rest of the system.

The interface is the active current-supplying part in current loop.

In RS-232-C mode signal ground is connected to system ground.

Without modification there may be a maximum of 64 asynchronous channels in an ND-100 system.

## Diagram

```mermaid
flowchart TD
    A[ND-100 bus] --> B[Bus control and interrupt logic]
    B --> C[Internal bus]
    C --> D[Channel 0 UART]
    C --> E[Channel 1 UART]
    C --> F[Channel 2 UART]
    C --> G[Channel 3 UART]
    
    H[Switches] --> I[0]
    I --> J[Current loop]
    J --> K[RS-232-C]
    H --> L[1]
    L --> J

    D -->|0| J
    D -->|1| K
    
    E -->|0| J
    E -->|1| K
    
    F -->|0| J
    F -->|1| K
    
    G -->|0| J
    G -->|1| K
    
    B <-- M[Device group]
    M <-- N[Thumbwheel]

    O[Crystal oscillator] --> P[Baud rate generator]
    
    P --> D
    P --> E
    P --> F
    P --> G
    
    Q[Thumbwheel] --> P
```

*ONE GROUP OF FOUR CHANNELS*

---

## Page 2

# Specifications

Available baud rates:

```
9600    200
4800    150
2400    134.5
1800    110
1200    75
600     50
300
```

| Character length | Parity | Stop bits |
|------------------|--------|-----------|
|                  | Even   | No        | 1 | 1 1/2 | 2 |
| 5 bits           | x      | x         | x |       | x |
| 6 bits           | x      | x         | x |       | x |
| 7 bits           | x      | x         | x |       | x |
| 8 bits           | x      | x         | x |       | x |

No cables are included. Cables must be specified in case of connection to modem.

```
 ____________________________________________________________
| Norsk Data      |                                          |
| Jernkveien 20   |                                          |
| Boks 4 Linderberg gård                                      |
| Oslo 10         |                                          |
| Tel.: 02-909030 |                                          |
| Tlx.: 18664 nd n|                                          |
|_________________|__________________________________________|

```

NOTE: NORSK DATA reserves the right to change specifications without given notice!

---

