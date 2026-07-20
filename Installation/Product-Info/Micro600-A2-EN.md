## Page 1

# TERMINAL SWITCH - Micro600

## INTRODUCTION

The Micro600 is a device which allows many terminals to be independently switched between a large number of terminal ports (interfaces). A large number of terminals (for example, larger than the total number of input ports on the computer(s)) may thus be connected to a computer system consisting of one or more computers.

This also provides a means of obtaining an even distribution of load on the different machines.

Rapid switching of terminals from a main system to existing back-up machines is an additional task for which the Micro600 is suitable.

- The Micro600 can be used to restrict access to certain computer ports by simple keyboard commands.

- The Micro600 maintains usage statistics, thus allowing the computer manager to monitor the usage of each port class, and reallocate ports between classes to ensure an optimum level of service to all terminal users.

## PRODUCT DESCRIPTION

- Each terminal connected to the Micro600 may have direct access to one of the individual computers in the system at any one time.

```
mermaid
graph TB
    A1(1) --> B(Terminal switch)
    A2(2) --> B
    AN(N) --> B
    A3(N + 1) --> B
    B --> C1(ND-100)
    B --> C2(ND-100)
    B --> C3(ND-500)
    C1 -->|ND-net| D(Computers)
    C2 -->|ND-net| D
    C3 --> D
    C3 -->|ND-net| D
    subgraph Terminals
        A1
        A2
        AN
        A3
    end
```

A2-6000-0582

---

## Page 2

# Connection

Terminals are connected to the «line» interfaces, RS-232-C, of the Micro60. They may be directly cabled or connected by line drivers or modems, dial-up or leased lines.

The terminal operator requests a connection by depressing any key on the keyboard. The Micro60 responds with the prompt message «CLASS=». The terminal operator must then key the desired class number (1 through 64). If the Micro60 can make a connection to a port of the desired class, it will do so, transmitting «GO» to the terminal. If unsuccessful, it will transmit «BUSY», «UNAVAILABLE», «UNASSIGNED» or «UNAUTHORIZED» as appropriate.

Each CLASS may correspond to one computer in the system. In this case, the CLASS number will simply be the number of the required computer. It is also possible to assign more than one CLASS to the same computer, for example if one general CLASS was required, add in addition one or more restricted classes. It is the supervisors task to assign such CLASSES.

## Disconnection

Terminals are disconnected either when the «port» interface sees that the computer has dropped Data Terminal Ready (i.e. when the computer considers that the terminal no longer requires its services), on detection of a «break» character from the terminal or after a period of inactivity of supervisor determined length.

# Technical Specifications

## Port Selection

Up to 64 port classes; any terminal may select any class of port for which it is authorized.

## Access Control

Any terminal may be prevented from gaining access to certain defined port class(es).

## System Response Messages

System response messages are in 8-level ASCII code for all asynchronous terminals. Other codes subject to special quotation.

## Operator's Console

Interface provided for teletype or teletype-compatible terminal, 110 to 9600 bps, 8-level ASCII code.

## Line/Port Capacity

- Model 1: Up to 60 lines/ports.
- Model 2: Up to 992 lines/ports. (Maximum of 496 simultaneous connections).

## Line/Port Speeds

The maximum line or port speed is dependent on the type of Line/Port module.

| Type | Line/Port Module | Any speed |
|------|------------------|-----------|
| Type I | Line/Port Module | 2400 bps |
| Type II | Line/Port Module: 9600, 4800, or any speed to 2400 bps |

## Autobaud

Carriage Return is the sign-on character in the speed range 110 to 4800 bps. Other sign-on characters may be supported, subject to special quotation.

## Connect Sequences

Data Activity, Break, Ring Indicator.

## Disconnect Sequences

Data Terminal Ready dropped, Break, Timeout.

## Channel Interfaces

EIA RS-232-C (CCITT V.24/V.28) serial, asynchronous. Integral line driver compatible with the Micro 400 Asynchronous Line Driver in 4-wire mode.

## Physical Dimensions

Two models, one with desk-top enclosure, one floor-standing:

| Model | Description | Dimensions |
|-------|-------------|------------|
| Model 1 | 15 slots for Quad Line/Port Modules | 52.1 cm wide, 26.0 cm high, 31.1 cm deep |
| Model 2 | 30 slots for Quad Line/Port Modules with expansion chassis available for maximum configuration | 56.6 cm wide, 198.1 cm high, 83.8 cm deep |

## Operating Environment

0-38°C, 0-95% relative humidity

## Power

115 vac ± 10%, 230 vac ± 10%, 50/60 Hz  
5 amps maximum

This product is delivered and supported by the vendor.

# Vendor

**Heath & Co.**  
Box 7072  
172 07 Sundbyberg  
Sweden

## Contact Information

```
Norsk Data

Olav Helsets vei 5
Oslo Ø
Boks 25 Bogerud
Tel.: 02-99500
Telex: 18294 nd no
Telefax: 02-99517
```

```
COMTEC
DIVISION OF NORSK DATA

Trondheim, tel. 075-16520, txx 55580 comtr n
Stockholm, tel. 0708-99000 ttx. 15255 nordata s
Odense, tel. 69-15740, ttx. 69680 comtec dk
Düsseldorf, tel. 0211-666388, ttx. 8587277 comt d
```

---

