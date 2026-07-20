## Page 1

# ND 750 LIMITED DISTANCE MODEM

## INTRODUCTION

The ND 750 is a low cost, high-performance modem intended for use over distances up to 10 km (6 miles). It meets EIA RS-232 and CCITT V24 specifications. The unit has full duplex capability, and accepts any transmission rate up to 9600 bps.

The transmission is performed by a three-state balanced current loop. This technique gives the ND 750 the capability to transmit one additional signal in each direction, so that the status of the lines and the terminals can be monitored.

The ND 750 can be used in local communication links between computers, CRT displays, typewriters, printers etc. It is an easy-to-use, small size and low cost unit.

```plaintext
  ____________      0-10 km      __________
 |            | <--------------> |          |
 |  ND117 or  | Rs 232 15 m     | ND 750   |
 |  ND252 or  |                 |          |
 |  ND271 or  |                 |          |
 |  ND281 or  |                 |          |
 |  ND272     |                 |          |
 |____________|                 |__________|
                                 |  Rs 232 1.2 m
                                 |/            
                              Printer        
                              ND 418 or
                              ND 427 or 
                              ND 232
```

## FEATURES

- Asynchronous transmission
- Full duplex
- 0-9600 bps (any rate)
- EIA RS-232/CCITT V24
- 2- or 4-wire
- Opto-coupler isolation
- Local links up to 10 km
- Line and terminal monitoring
- Easy to use
- Easy to install
- Small size
- Low cost

## SPECIFICATIONS

| Feature       | Description                        |
|---------------|------------------------------------|
| Interface     | According to EIA RS-232/CCITT V24  |
| Transmission  | Asynchronous, full duplex          |
| Speed         | Any rate from 0 to 9600 bps        |
| Line          | Simplex: One twisted pair          |
|               | Full duplex: Two twisted pairs     |

```
  750—A1—4000—1280
```

---

## Page 2

# Technical Specifications

## General Specifications

- **Max. distance:**
  - 9600 bps ............ 0.9 km
  - 4800 bps ............ 1.5 km
  - 2400 bps ............ 3 km
  - 1200 bps ............ 5 km
  - 600 bps ............... 10 km

- **Transmission technique:**  
  +/- 10 mA three-state balanced current loop

- **Isolation:**  
  Opto-coupler

- **Isolation voltage:**  
  1500 V

- **Power:**  
  220 VAC +/- 10%, 40–60 Hz  
  Other supplies on request

- **Dimensions:**  
  80 x 150 x 50 mm

- **Weight:**  
  0.5 kg

- **Temperature range:**  
  5–50°C

- **Humidity:**  
  0–95% RH, non-condensing

## DTE Connection (To DB25S Connector)

### RS-232

| Pin No. | Designation | Signal Name                |
|---------|-------------|----------------------------|
| 1       | AA          | Protective ground          |
| 2       | BA          | Transmitted data           |
| 3       | BB          | Received data              |
| 4       | CA          | Request to send            |
| 5       | CB          | Clear to send              |
| 6       | CC          | Data set ready             |
| 7       | AB          | Signal ground              |
| 8       | CF          | Received line signal detector |
| 20      | CD          | Data terminal ready        |
| 18      | —           | Data terminal busy         |

## Transmission Line Connection (To ND 750 Screw Terminals)

```mermaid
flowchart LR
    A[ND 750] -->|1| B[ND 750]
    A -->|2| C[Transmitter]
    A -->|4| D[Shield (optional)]
    B -->|6| E[Transmitter]
    B -->|2| F[Receiver]
    B -->|4| G[Shield (optional)]
```

**NOTE:** If shield cable is used, just connect the shield at one end of the transmission line.

## Indicators

- **Power On** (red LED)
- **Carrier On** (green LED)
- **On-board indicator:** Transmitted data

## Contact Information

**Norway:**  
NORSK DATA A.S  
Jerikoveien 20, Box 4 Lindeberg gård  
OSLO 10  
Tel. 02-90300, tik. 18661 nd no  
Bergen: tel. 05-29560  
Sandnes: tel. 045-66662

**Denmark:**  
NORSK DATA ApS  
Øverødvej 5  
2840 HOLTE  
Tel. 02-425055, Tik. 37725 nd dk

**Sweden:**  
ND NORSK DATA AB  
Kanalvägen 3, Box 2031  
194 02 UPPLANDS VÄSBY  
Tel. 0760-8050, tlx. 13528 nordata s  
Gothenburg: tel. 031-299350  
Malmö: tel. 040-70510

**West Germany:**  
NORSK DATA DEUTSCHLAND GmbH  
Abraham-Lincoln-Str. 30  
6200 WIESBADEN  
Tel. 06121-764220, Tik. 4186370 noda d

**U.S.A.:**  
NORSK DATA N.A., Inc.  
65, William Street  
Wellesley, MASS. 02181  
Tel. 0617-237.7945, Tik. 921740 norsk well

**France:**  
NORSK DATA FRANCE  
"Le Brevent", Avenue du Jura  
01210 FERNEY-VOLTAIRE  
Tel. 050-408576, Tik. 385653 nordata fernv  
Paris: tel. 01-6023366, Tik. 201108 nd paris

**England:**  
NORSK DATA Ltd.  
NORB House, Pelican Lane, Newbury  
BERKSHIRE RG13 1NU  
Tel. 0663-31465, Tik. 849819 norsk g

**Note:** NORSK DATA reserves the right to change specifications at any time without given notice.

---

