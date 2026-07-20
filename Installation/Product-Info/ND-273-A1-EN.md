## Page 1

# Hardware Interface Module

## ND 273 8 Channel Buffered Terminal Interface (9.6 Kbaud)
## ND 274 8 Channel Buffered Terminal Interface (19.2 Kbaud)

### INTRODUCTION

The ND 273/274 buffered asynchronous serial communication controller is used to establish communication with terminals, printers, ND-PC's and other serial devices. There is a buffer on both input and output, minimizing the risk of lost data. The 273/274 occupies one slot in the ND-100 bus system, and 8 communication lines. One can choose current loop or V.24 line interface, allowing maximum flexibility. The current loop is typically used for communication with local devices; the V.24 interface for local devices requiring V.24 or remote devices via modem.

### FEATURES

For each line, the following is valid:

- 16 bytes input buffer, 64 bytes output buffer
- Device connection can be either 20mA current loop or CCITT V.24 (EIA RS-232c). Switch selectable individually per channel.
- Programmable speed from 50 to 9600 bits per second for the ND 273, from 50 to 9600 bits for the ND 274
- Split-speed capability for send and receive (not available on the ND 274)
- Programmable setting of parity (even or no parity)
- Programmable setting of character length (5, 6, 7 or 8 bits)
- Programmable setting of stop bits (1, 1 1/2 or 2)
- Full duplex operation
- The current loop line is electrically isolated with a galvanic isolator.
- Flow control: XON/XOFF protocol in software for both current loop and RS 232, and in addition DTR protocol (Hardware BUSY) for the RS 232 interface.

In addition to programmable setting of the speed, a thumbwheel selects the baud rate for each group of four lines.

### PRODUCT DESCRIPTION

#### CURRENT LOOP:

| Data transmission | Serial transmission. Two lines form a transmit loop and two lines form a receive loop. |
| ----------------- | ------------------------------------------------------------------------------------- |
| Information coding | Data is coded as current (20mA)/ not current (0mA), Idle line is 20 mA (logical 1) |

| Max cable length  |                                                                                       |
| ----------------- | ------------------------------------------------------------------------------------- |
| 9600 bps          | 50 m                                                                                 |
| 4800 bps          | 100 m                                                                                |
| 2400 bps          | 200 m                                                                                |
| 1200 bps          | 400 m                                                                                |
| 600 bps           | 800 m                                                                                |
| 300 bps           | 1600 m                                                                               |

- Max trans. speed: 9600 bps

In CURRENT LOOP mode, the controller is the active, current-supplying part.

#### V.24 (RS232c) Interface Standard:

| Data transmission | Serial transmission, i.e., one Txd line and one Rxd line                              |
| ----------------- | ------------------------------------------------------------------------------------- |
| Information coding| Data and control information is coded as a positive or negative voltage relative to a common ground reference |

- Max cable length: 15 meters (50 feet)
- Max trans. speed: ND 273 = 9600 bps, ND 274 = 19200 bps

### REQUIREMENTS

- SINTRAN III Version J or later

### DOCUMENTATION

- Buffer Terminal Interface .......................... ND 11.022

---

## Page 2

# Contact Information

## Norske Data

| Location               | Phone                   | Telex               |
|------------------------|-------------------------|---------------------|
| Oslo                   | 02-390300, tix. 18661 nd n |
| Bergen                 | 05-202230               |                     |
| Sandsvaer              | 06-78580                |                     |
| Tromsø                 | 083-71692               |                     |
| Trondheim              | 07-921222, tix. 55580 nd trd |
| Stockholm              | 08-7039200, tix. 15255 nordata s |
| Gothenburg             | 031-690160              |                     |

## ND Comtec

| Office        | Address                     |
|---------------|-----------------------------|
| Trondheim     | 07-921222, tix. 55580 nd trd |
| Stockholm     | 08-70284100, tsk 15255 nordata s |
| Odense        | tel. 09-157140, tbox 5600 comtec dk |
| Düsseldorf    | tel. 0211-688938, tsk 682727 com t d |
| Newbury       | tel. 0635-35554, tsk 849819 norsk g |

Ojeriveien 20, P.O. Box 4, Lindeberg gård, 1007 Oslo 10, Norway  
Tele: 02-390300, Tlx: 18661 nd n, Telefaks: 02-302947

## ND Silvidata

| Location              | Address         |
|-----------------------|-----------------|
| Växjö                 | 46-470-46200    |
| Sundsvall, SWEDEN     | 851 83          |
| Tel                   | 46-60-151510    |

## International Locations

| City         | Phone                        | Telex              |
|--------------|------------------------------|--------------------|
| Paris        | tel. 464 37165, tk. 471620   |
| Dresden      | tel. 8491436, tk. 2628078    |
| Toulouse     | tel. 891266, tk. 350809     |

(Note: Norske Data reserves the right to change specifications without notice.)

---

