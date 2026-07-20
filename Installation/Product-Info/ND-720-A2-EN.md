## Page 1

# ND720 HDLC INTERFACE

## INTRODUCTION

The need for standardization for communication equipment and procedures has resulted in electrical, mechanical and frame format standards. The HDLC (High level Data Link Control) is a frame format standard which is defined in the ISO IS 3309 standard.

The frame level of the HDLC format is fully compatible with Synchronous Data Link Control (SDLC) and the Advanced Data Communication Control Procedure (ADCCP).

The ND720 HDLC Interface is designed according to the frame level HDLC format which will be used in the X-25 communication procedure.

The HDLC hardware is based upon microprocessor and LSI technology for maximum flexibility such that it may be used in CPU or DMA controlled communication. The CPU controlled hardware offers full HDLC frame format compatibility up to 19200 baud and is intended for low cost medium performance applications requiring the HDLC frame format.

The HDLC DMA hardware offers up to 307.2 Kbaud high performance, low processing overhead in communication systems.

The HDLC hardware may be operated in half of full duplex and on point to point or multidrop lines.

## FEATURES

- Design according to ISO IS 3309 standard
- Fully compatible frame level with SDLC and ADCCP
- Modem connections may be CCITT V.24, V.35, X-21 Bis or X-21 (X.27)
- Half or full duplex operation
- Data rate up to 307.2 Kbits full duplex (1 Mbit - half duplex) using Direct Memory Access (DMA)
- Automatic buffer chaining for maximum flexibility and minimum overhead

## PRODUCT DESCRIPTION

### General

The HDLC DMA module is operated by placing control information in a buffer in memory, and then writing the address of this buffer — the LIST POINTER — to the HDLC module together with one of eight commands. The modules will then perform the proper action.

A command must be completed before a new command is issued. The only exception is the Device Clear Sequence which may be initiated any time.

### Commands

The 8 commands are as follows:

0. Device Clear
1. Initialize
2. Receiver Start
3. Receiver Continue
4. Transmitter Start
5. Dump Data Module
6. Dump Register(s)
7. Load Register(s)

## HDLC Frame Format

The HDLC frame format is as follows:

| FRAME    | Information Bytes      |
|----------|------------------------|
| 01111110 | A C I FCS 01111110     |

The FLAG marks the beginning and the end of a frame. The FLAG sequence consists of one zero bit followed by 6 one bits and one zero bit.

The A-field (8 bits) is meant as a station address, but its contents are not described in the frame standard.

The C-field (8 bits) is a control byte intended for link control.

The I-field is the information field and may be any length. The I-field may also be absent.

---

## Page 2

# HDLC Hardware

The HDLC hardware is designed around a 16 bit microprocessor and an LSI chip which takes care of the parallel to serial and serial to parallel conversion and the control of the frame format.

# HDLC DMA Data Structure

The HDLC DMA Data Structure allows data transfer from a predefined list without program intervention.

This list structure gives a non-critical system response time in case of heavy I/O load on the system.

# Contact Information

| Company                  | Address                                         | Telephone                | Telex                     |
|--------------------------|-------------------------------------------------|--------------------------|---------------------------|
| NORSK DATA A.S           | Linderudveien nord 20, Box 4 - Lindeberg gård  | Oslo 10, NORWAY          | Tel. 02396160, Tlx. 18661 |
| NORSK DATA ApS           | Øverødvej 5                                     | 2840 Holte, DENMARK      | Tel. 02-245055            |
| NORSK DATA DEUTSCHLAND   | Abraham-Lincoln-Str. 30                         | 6200 Wiesbaden, WEST GERMANY | Tel. 06121-762420, Tlx. 4186370 noda |
| ND NORSK DATA AB         | Kanalvägen 3, Box 2031                          | 194 02 Upplands Väsby, SWEDEN | Tel. 076-86050, Tlx. 13528 nordata s |
| NORSK DATA FRANCE        | "Le Brevent", Avenue du Jura                    | 01210 Ferney-Voltaire, FRANCE | Tel. 050-408576, Tlx. 385653 nordata fernv |
| NORSK DATA N.A., Inc.    | 65, William Street                              | Wellesly, Mass. 02181, USA | Tel. 0617-237.7945        |
| ND NORSK DATA AB         | K kangfarsgatan 11, Box 905 2                   | 421 09 Västra Frölunda, SWEDEN | Tel. 031-299530  |
| NORSK DATA FRANCE        | 120 Bureau de la Colline                        | 92213 Saint Cloud, FRANCE | Tel. 01-6022367, Tlx. 201108 nd paris |
| RICHARD NORTON (NORD) Ltd. | NORD House, 17 Balfe Street, King's Cross      | London N1 9EB, ENGLAND   | Tel. 01-2785501, Tlx. 299537 |

Note: Norsk Data reserves the right to change specifications at any time without given notice!

---

