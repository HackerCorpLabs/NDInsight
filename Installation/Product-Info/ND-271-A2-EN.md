## Page 1

# Hardware Interface Module

## ND 271 Terminal Interface, 4 lines, ND-100
## ND 272 Terminal Interface, 8 lines, ND-100

- Programmable setting of character length (5, 6, 7 or 8 bits).
- Programmable setting of stop bits (1, 1 1/2 or 2).
- Full duplex operation.
- The current loop line is electrically isolated with a galvanic isolator.
- Flow control: XON/XOFF protocol in the software for both current loop and RS 232, and in addition DTR protocol (Hardware BUSY) for the RS 232 interface.

In addition to programmable setting of the speed, a thumbwheel selects the baud rate for each group of four lines.

## Product Description

### Current Loop:

|                |                                                            |
|----------------|------------------------------------------------------------|
| Data transmission: | Serial transmission. Two lines forming a transmit loop and two lines forming a receive loop. |
| Information coding: | Data is coded as current (20mA) / not current (0mA) Idle line is 20ma (logical 1). |
| Max. cable length:  | 9600 bps ...................... 50 m 4800 bps ...................... 100 m 2400 bps ...................... 200 m 1200 bps ...................... 400 m 600 bps ....................... 800 m 300 bps .......................1600 m |

In the CURRENT LOOP mode, the controller is the active, current-supplying part.

### V.24(RS232C) Interface Standard:

|                    |                                                            |
|--------------------|------------------------------------------------------------|
| Data transmission: | Serial transmission, i.e., one Txd line and one Rxd line.  |
| Information coding: | Data and control information is coded as a positive or negative voltage relative to a common ground reference. |
| Max. cable length:  | 15 meters (50 feet).                                      |
| Max. transm. speed: | 20kbs (baud).                                           |

## Introduction

The ND 271/272 asynchronous serial communication controller occupies one slot in the ND-100 bus system, and accommodate 4 (ND 271) or 8 (ND 272) communication lines. The controller is used to establish communication with: terminals, printers, ND-PCs or other serial devices. The choice of the Current loop or V.24 line interface gives maximum flexibility. The current loop interface is typically used for communication with local devices while the V.24 interface is used for local devices requiring V.24 or remote devices via modem.

## Features

For each of the 4 or 8 lines, the following is valid:

- Device connection could either be in accordance to 20 mA Current loop or CCITT V.24 (EIA RS-232C). Switch selectable individually per channel.
- Programmable speed from 50 to 9600 bits per second.
- Split-speed capability for send and receive.
- Programmable setting of parity (Even or no parity).

```
          +------------------------+
          |                        |
          |      TERMINAL          |
          |     CONTROLLER         |
          |                        |
          +-------------+---+------+ 
                        |   |
                        |   |
        +---------------+   +-------------------+
        |                                       |
        |                                       |
      +-|------+                                |
      | V.24    |                               |
      |   OR    |                               |
      | CURRENT |                               |
      |  LOOP   |                               |
      +---+-----+                               |
          |                                     |
        +-|-+                                   |
        |   |                                   |
        | SYSTEM                                |
      +-||-|----------+     |                   |
      | 4 ITEM - ND 271 |   |    TERMINAL       |
      | 8 ITEM - ND 272 |   |                   |
      +----------------+---+                   _|
          ONE CHANNEL  |                    _ /
                      |                   /
        +-------------|-+               |
        | SPEED CHARACTER |           |
        |   LENGTH    PARITY |         |
        +-----------------+ |         
                          | |          
                          | |           
                          | |              
             CURRENT -----| |- GALVANIC
               LOOP        |     ISOLATION
```

[Photo: Terminal and Printer connected via Modem]

---

## Page 2

# Norsk Data

## Corporate Headquarters
Alf Bjerckes vei 5  
P.O. Box 25, Økern  
0501 Oslo 5  
Norway  
Tel.: 02-29500  
Telex: 18 6861 nd n  
Telefax: 02-295617  

## Regional Offices

| Location       | Telephone      | Telex            |
|----------------|----------------|------------------|
| Oslo           | 02-309030      | tix. 18661 nd n  |
| Bergen         | tel. 05-20390  |                  |
| Sandnes        | tel. 04-97680  |                  |
| Tromsø         | tel. 083-61762 |                  |
| Trondheim      | tel. 07-921222 | tix. 55580 nd trd|
| Stockholm      | tel. 08-90200  | tix. 15255 nordata s |
| Gothenburg     | tel. 031-49670 |                  |
| Malmö          | tel. 040-35505 | tix. 37275 nd dk |
| Copenhagen     | tel. 02-295056 | tix. 38653 nordata ferv |
| Aarhus         | tel. 06-1205   |                  |

## Subsidiaries

| Location               | Telephone          | Telex                   |
|------------------------|--------------------|-------------------------|
| London                 | tel. 081-566 9999  |                         |
| Munich                 | tel. 089-48640441  | tix. 856770 nd d        |
| Spencer, WV            | tel. 304-927-5545  |                         |
| Düsseldorf             | tel. 021-6604868   | tix. 885277 comt d      |
| Newbury                | tel. 0635-35544    | tix. 848919 norsk g     |
| Berlin                 | tel. 30-8010533    |                         |
| Quickborn, Germany     | tel. 04106-48500   |                         |
| New Delhi              | tel. 9912-398258   |                         |

## Comtec
Jerikoveien 20  
P.O. Box 43, Lindeberg gård  
1007 Oslo 10, Norway  
Tel.: 02-309030  
Tel.: 18661 nd n  
Telefax: 02-309247

## Silvidata
S-851 83 Sundsvall, Sweden  
Tel.: 46-60-151150

## Represented by Agent in:
- **India**: Indchem Electronics Ltd.
- **Finland**: Oy Pargas Ab
- **France**: Matra Datasysteme S.A.

```
ASCII Art Placeholder for Logos

  ___       ___
 |   \     /   |
 | ND  \ /  ND |
 |_____/ \_____|

  COMTEC      Silvidata
```

---

