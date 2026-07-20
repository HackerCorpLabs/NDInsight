## Page 1

# ND 734 Megalink Interface for ND-100

## INTRODUCTION

The need for standardization of communication equipment and procedures has resulted in electrical, mechanical and frame format standards. The HDLC (High Level Data Link Control) is a frame format standard which is defined in the ISO IS 3309 standard. X.25 communication procedure uses this format standard.

The frame level of the HDLC format is fully compatible with Synchronous Data Link Control (SDLC) and the Advanced Data Communication Control Procedure (ADCCP).

ND 734 Megalink Interface is designed according to the frame level HDLC format.

ND 734 Megalink may operate up to 983 Kbaud in a high throughput mode where retransmissions are avoided. This is achieved by using a busy line which necessitates an extra pair of wires in each direction in addition to the X-21 defined lines. The Megalink can therefore be fully exploited in computer links between two Megalink modules. Operation without busy lines is possible below 307 Kbaud.

The Megalink is based upon microprocessor and LSI technology. It may be operated in half or full duplex mode on point to point line. Multidrop requires external equipment.

## FEATURES

- Design according to ISO IS 3309 standard
- Fully compatible frame level with SDLC and ADCCP
- Half or full duplex operation
- Data rate up to 983 Kbits full duplex using Direct Memory Access (DMA).
- Automatic buffer chaining for maximum flexibility and minimum overhead.
- A special busy-line stops the transmitter clock before an overflow or underrun situation occurs. This makes retransmission unnecessary and improves transmission output.
- V 11 (= X 27 = RS 422) Electrical levels for modem connection.

## PRODUCT DESCRIPTION

### General

The Megalink DMA is operated by placing control information in a buffer memory, and then writing the address of this buffer – the LIST POINTER – to the Megalink module together with one of eight commands. The module will then perform the proper action.

A command must be completed before a new command is issued. The only exception is the Device Clear Sequences which may be initiated any time.

### Commands

The 8 commands are:
0. Device Clear
1. Initialize
2. Receiver Start
3. Receiver Continue
4. Transmitter Start
5. Dump Data Module
6. Dump Register(s)
7. Load Register(s)

### Frame Format

The frame format is as follows:

| FRAME      | Information Bytes |
|------------|-------------------|
| 01111110   | A  | C | I | FCS | 01111110 |

The FLAG marks the beginning and the end of a frame. The FLAG sequence consists of one zero bit followed by six one bits and one zero bit.

The A-field (8 bits) is meant as a station address, but its contents are not described in the frame standard.

The C-field (8 bits) is a control byte intended for link control.

The I-field is the information field and may be any length. The I-field may also be absent.

---

## Page 2

# FCS and FRAME

The FCS is a 16 bit frame check sequence and contains the 16 bit CRC number computed over the bits between the last bit of the opening flag and the first bit of FCS.

The information part of the FRAME may consist of a number of data blocks (buffers).

## Hardware

The Megalink is designed around a 16 bit microprocessor and an LSI chip which takes care of the parallel to serial and serial to parallel conversion and the control of the frame format.

The microprocessor and its associated PROM give great flexibility in the design and make it possible to use identical hardware for both HDLC formats and BSC compatible communication on DMA just by using another PROM. (Note that each procedure is registered as a separate ND product).

## Megalink DMA Data Structure

The Megalink DMA Data Structure allows data transfer from a predefined list without program intervention.

The list structure gives a non-critical system response time in case of heavy I/O load on the system.

```
             ND                      ND
 Norsk Data                  ~COMTEC~
Jerikoveien 20              Jerikoveien 20
Boks 4 Linderberg gård      Boks 4 Linderberg gård
Oslo 10                     Oslo 10
Tel.: 02-909030             Tel.: 02-909030
Tlx.: 18661 nd n            Tlx.: 18661 nd n
```

```
Bergen, tel. 05-292090       Trondheim, tel. 075-16520, tlx. 55580 comte n
Sandnes, tel. 063-46544      Stockholm (Upplands Väsby), tel. 070-341010, tlx. 15255 nordata s
Tromsø, tel. 083-77755       Stockholm (Solna), tel. 082-75755, tlx. 13706 swecom s
Stockholm, tel. 086-465640, tlx. 15255 nordata s
Gothenburg, tel. 031-795670  Odense, tel. 09-197440, tlx. 50589 comtec
Malmo, tel. 040-96350        Ballerup/Copenhagen, tel. 02-675000
Copenhagen, tel. 02-453505, tlx. 37275 nd dk
Wiesbaden, tel. 0611-73641, tlx. 487030 ndna n
Plein-Valoiser, tel. 805-455786, tlx. 33653 nordata fenv
Paris, tel. 01643626, tlx 201105 nd parts
Van, tel. 07-863777
Newbury (Berkshire), tel. 0635-31445, tlx. 849819 norsk g
Boston, tel. 617-237-7643, tlx. 921470 norsk well
```

```
                                 ND
                          ~COMTEC~
                         Jerikoveien 20
                         Boks 4 Linderberg gård
                         Oslo 10
                         Tel.: 02-909030
                         Tlx.: 18661 nd n
```

```
                                            Jerkoveien 20
                                            Düsseldorf, tel. 0211-663808, tlx. 8587277 comt d
```

**NOTE: NORSK DATA reserves the right to change specifications without given notice!**

[Scanned by Jonny Oddene for Sintran Data © 2010]

---

