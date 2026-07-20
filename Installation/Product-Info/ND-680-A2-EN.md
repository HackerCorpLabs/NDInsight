## Page 1

# ND 680 ND COLOUR TERMINAL CONTROLLER

## INTRODUCTION

The NORD Colour Terminal — NCT — is a semi-graphic display system designed for colour presentation of alphanumeric text and graphic symbols. The design is taking full advantage of the LSI and MOS technology.

Writable memories are used for symbols and colour definition. This feature enables a dynamic selection of colours and symbols.

The ND Colour Terminal consists of the modules:
- ND 680 NCT Controller
- ND 681 NCT Trackerball Controller
- ND 682 NCT Trackerball
- ND 673 RGB Monitor
- ND 677 Keyboard

## FEATURES

- 4096 colours
- Characters and symbols user defined
- Blinking of symbols
- Stand-alone keyboard
- Trackerball option for direct pointing in the picture
- Standard RS-232, RS-422 and Current Loop interface
- Standard RGB monitor interface
- Hardcopy output

## PRODUCT DESCRIPTION

### Controller

#### MATRIX SIZE

The symbol matrix is 8 horizontal dots, giving 64 symbols per line. Symbol height is program selectable from 1 to 16 dots. 1—8 x 8 matrix is called single size, 9—16 x 16 matrix is called double size symbols.

#### SYMBOL SPECIFICATION

The symbol generator is programmable. The writable symbol memory stores 256 single size of 128 double size symbols.

#### COLOUR SPECIFICATION

From a total range of 4096 colour shades, 16 may be selected to be used as symbol foreground colours, 8 colour shades may be selected as symbol background colour and 8 colours may be used for the cursor.

#### BLINKING

When blinking is specified, the foreground symbol will alter between the specified colour and colour 0, which is normally black. The background of the symbol will alter in the same manner, but with double frequency.

#### CURSOR CONTROL

The normal addressing mode for alphanumeric text is to step the character address (cursor), such that characters are sequentially positioned to the right on the line. For semigraphic applications, however, the addressing method can be adjusted.

---

```
[Photo: ND 680 Colour Terminal Controller]
```

---

680-A2–1000–0681

---

## Page 2

# Specifications

## Colours

| Description                    | Value  |
|-------------------------------|--------|
| Total number of colour shades | 4096   |
| Foreground colours            | 16     |
| Background colours            | 8      |
| Cursor colours                | 8      |
| Colour combinations per symbol| 128    |
| Trackball marker              | yellow |

## Symbols

| Description              | Value |
|-------------------------|-------|
| Single size character set| 256   |
| Double size character set| 128   |

## Display Format

| Description  | Value       |
|-------------|-------------|
| Number of lines | 18–48    |
| Symbols per line| 64       |

## Display

Standard CCIR 625 lines 50 frames per second without interlace. The system is intended for use with any standard RGB monitor.

## Computer Interface

The NCT system has 3 standard interface plugs conforming to EIA RS-232 C/CCITT V.24, EIA RS-422 and Teletype standard respectively. The EIA RS-422 is a high-speed full duplex asynchronous DMA interface providing a range of transmission rates, from 19200 to 76800 bauds.

## Transmission Rates

Full duplex transmission.

- Asynchronous, 110-150-300-600-1200-2400-4800-9600 baud, split speed 1200/75 and 2400/1.500 baud.
- DMA (RS-422) Input: 19200-38400 and 76800 baud.
- Output: 110-150-300-600-1200-2400-4800-9600 baud.

# Physical and Electric Specification

| Description                 | Terminal Controller | Monitor (CONRAC) | Keyboard | Trackball |
|----------------------------|---------------------|------------------|----------|-----------|
| Voltage                    | 220 V/50 Hz         | 220 V/50 Hz      | —        | —         |
|                            | 115 V/60 Hz         | 115 V/60 Hz      | —        | —         |
| Voltage stability          | ± 10%               | ± 10%            | —        | —         |
| Frequency stability        | ± 3%                | ± 3%             | —        | —         |
| Power                      | 160 W               | 125 W            | —        | —         |
| Operating temperature ambient | 0—50°C             | 10—50°C          | 0—50°C   | 0—50°C    |
| Operating humidity         | 10—90% rel.         | 10—90% rel.      | 10—90% rel.| 10—90% rel. |
| Weight                     | 15 kg               | 20 kg            | 2 kg     | 2.5 kg    |
| Width                      | 450 mm              | 480 mm           | 455 mm   | —         |
| Height                     | 125 mm              | 450 mm           | 85 mm    | —         |
| Depth                      | 420 mm              | 560 mm           | 190 mm   | —         |

# Keyboard

The stand-alone keyboard has a full ASCII (lower and upper case) character set plus 5 keys for cursor control, numeric-key-pad and 25 function keys. For detailed information, refer to ND 677 Keyboard Specifications.

# Trackball

An optional trackball offers a simple and easy way of pointing and addressing variables in the picture. The trackball unit controls a blinking cross on the screen, which follows the ball’s direction of movement. Thus, the cursor can be moved across the screen at any angle and not in the x and y directions only. The cursor is assigned the foreground colour of the highest priority (yellow) ensuring high contrast and easy handling. The trackball has a resolution of 128 steps per revolution.

# NCT Software

The NCT SERVICE PROGRAM is available for SINTRAN III/VS systems. The program may be used to define symbols and colours, and to interactively generate and display pictures. A set of library functions is provided for the user program.

---

