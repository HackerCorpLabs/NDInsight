## Page 1

# ND COMPUTER SYSTEMS

```
 1   2   3   4   5   6   7   8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32
┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
│            BANK X                                                                 BANK Y            │
│        Maximum 256 Kw/21                                                      Maximum 128 Kw/21     │
├──────────────────────────────────────────────────┬──────────────────────────────────────────────────┤
│               X BANK                             │                           Y BANK                 │
│                                                  │                                                  │
│ ┌────────────┐ ┌─────────────┐ ┌─────────────┐   │   ┌─────────────┐ ┌────────────┐ ┌─────────────┐ │
│ │ Control 1  │ │ Port A      │ │ Port B      │   │   │ Port A      │ │ Port B     │ │ Control 1   │ │
│ │ Bank X     │ │ for Bank X  │ │ for Bank X  │   │   │ for Bank Y  │ │ for Bank Y │ │ Bank Y      │ │
│ └────────────┘ └─────────────┘ └─────────────┘   │   └─────────────┘ └────────────┘ └─────────────┘ │
│ ┌─────────────┐ ┌────────────┐ ┌──────────────┐  │  ┌────────────┐  ┌──────────────┐               │
│ │ Port C      │ │ Port D     │ │  ERROR LOG   │  │  │ Port C     │  │ Port D       │               │
│ │ for Bank X  │ │ for Bank X │ │ for Bank X   │  │  │ for Bank Y │  │ for Bank Y   │               │
│ └─────────────┘ └────────────┘ └──────────────┘  │  └────────────┘  └──────────────┘               │
└──────────────────────────────────────────────────┴──────────────────────────────────────────────────┘
```

## System Specifications

| Model | Description                                 |
|-------|---------------------------------------------|
| ND 143 | One Bank 128 Kw Memory System               |
| ND 144 | One Bank 256 Kw Memory System               |
| ND 146 | Dual Bank Multiport Memory System           |
| ND 145 | Port For One Bank Access                    |
| ND 147 | Port For Dual Bank Access                   |
| ND 158 | 32 Bit Four Port Memory System              |
| ND 190 | 32 Bit Memory Port                          |

## Introduction

The ND 146 constitutes a high speed, flexible and modular Multiport Memory System (MPM), designed for multiprocessor/multichannel applications requiring high bandwidth with internal independent banks and independent memory ports. The ND 143/144 are one bank systems built with the same modules as ND 146. Upgrading is therefore possible and does not degrade performance.

The ND 144, ND 146 and ND 158 together with the ND 156 Memory Modules are capable of single bit error correction and multiple error detection.

## Features

- Up to 4 independent high speed memory channels
- Up to 2 independent memory banks
- Separate refresh for each bank
- Standard Error Checking and Correcting — ECC — memory system with built-in error logging
- 32 Kw/16 bits
- Each memory port’s address space is switch controlled for maximum flexibility
- Low overhead in access priority logic
- Printed circuit backplane for increased reliability

## Product Description

The basic Multiport Memory unit ND 143 consists of a 19” rack with printed circuit backwiring plane for 4 ports, one memory bank with associated memory controller and refresh logic. The memory bank has a maximum of 128 Kwords, using the ND 156 Memory Module.

---

## Page 2

# Keyboard

The stand-alone keyboard has a full ASCII (lower and upper case) character set plus 5 keys for cursor control, numeric-key-pad and 25 function keys. For detailed information, refer to ND 677 Keyboard Specifications.

# Trackerball

An optional trackerball offers a simple and easy way of pointing and addressing variables in the picture. The trackerball unit controls a blinking cross on the screen, which follows the ball's direction of movement. Thus, the cursor can be moved across the screen at any angle and not in the x and y directions only. The cursor is assigned the foreground colour of the highest priority (yellow) ensuring high contrast and easy handling. The trackerball has a resolution of 128 steps per revolution.

# NCT Software

The NCT Service Program is available for SINTRAN III/VS systems. The program may be used to define symbols and colours, and to interactively generate and display pictures. A set of library functions is provided for the user program.

# Specifications

## Colours

| Specification                             | Value |
| ----------------------------------------- | ----- |
| Total number of colour shades             | 4096  |
| Foreground colours                        | 16    |
| Background colours                        | 8     |
| Cursor colours                            | 8     |
| Colour combinations per symbol            | 128   |
| Trackerball marker                        | yellow|

## Symbols

| Specification                             | Value |
| ----------------------------------------- | ----- |
| Single size character set                 | 256   |
| Double size character set                 | 128   |

## Display Format

| Specification                             | Value |
| ----------------------------------------- | ----- |
| Number of lines                           | 18–48 |
| Symbols per line                          | 64    |

## Display

Standard CCIR 625 lines 50 frames per second without interlace. The system is intended for use with any standard RGB monitor.

## Computer Interface

The NCT system has 3 standard interface plugs conforming to EIA RS-232 C/CCITT V.24, EIA RS-422 and Teletype standard respectively. The EIA RS-422 is a high-speed full duplex asynchronous DMA interface providing a range of transmission rates, from 19200 to 76800 bauds.

## Transmission Rates

Full duplex transmission. Asynchronous, 110-150-300-600-1200-2400-4800-9600 baud, split speed 1200/75 and 2400/150 baud. DMA (RS-422) Input: 19200-38400 and 76800 baud. Output: 110-150-300-600-1200-2400-4800-9600 baud.

# Physical and Electric Specification

| Specification                 | Terminal Controller      | Monitor (CONRAC)  | Keyboard | Trackerball |
| ----------------------------- | -------------------------| ----------------- | -------- | ----------- |
| Voltage                       | 220 V/50 Hz <br> 115 V/60 Hz | 220 V/50 Hz <br> 115 V/60 Hz | —        | —         |
| Voltage stability             | ± 10%                    | ± 10%             | —        | —         |
| Frequency stability           | ± 3%                     | ± 3%              | —        | —         |
| Power                         | 160 W                    | 125 W             | —        | —         |
| Operating temperature ambient | 0—50°C                   | 10—50°C           | 0—50°C   | 0—50°C    |
| Operating humidity            | 10—90% rel.              | 10—90% rel.       | 10—90% rel. | 10—90% rel. |
| Weight                        | 15 kg                    | 20 kg             | 2 kg     | 2.5 kg    |
| Dimensions in mm:             |                          |                   |          |           |
| Width                         | 450                      | 480               | 455      | —         |
| Height                        | 125                      | 450               | 85       | —         |
| Depth                         | 420                      | 560               | 190      | —         |

```plaintext
[Logo: Norsk Data]
```

NOTE: NORSK DATA reserves the right to change specifications without given notice!

---

