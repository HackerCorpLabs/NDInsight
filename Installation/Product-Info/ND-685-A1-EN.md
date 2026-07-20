## Page 1

# ND 685 COLOUR TERMINAL INTERFACE

## INTRODUCTION

The ND 685 Colour Terminal Interface - CTI - is a semigraphic display system designed for colour presentation of alphanumeric text and graphic symbols. The CTI is designed as a standard ND-100 interface board and will occupy one slot in the ND-100 bus. Maximum distance between the monitor, keyboard, trackball and the ND-100 computer is 100 meters.

Writable memories are used for symbols and colour definition. This feature enables a dynamic selection of colours and symbols.

The CTI system consists of the following modules:

- ND 685 CTI board
- ND 687 Keyboard
- ND 686 Trackball adapter
- ND 682 Trackball
- (+ a cable set)

## FEATURES

- 4096 colours
- User-Defined characters and symbols
- Blinking symbols
- Stand-alone keyboard
- Trackball option for direct pointing in the picture
- Standard RGB monitor interface
- Hardcopy output

## PRODUCT DESCRIPTION

### CONTROLLER MATRIX SIZE

The symbol matrix is 8 horizontal dots, giving 64 symbols per line. Symbol height is program selectable from 1 to 16 dots. The matrix from one through eight has single size symbols; from nine through sixteen has double size symbols. There are eight single size matrices; sixteen double size.

### SYMBOL SPECIFICATION

The symbol generator is programmable. The writable symbol memory stores 256 single size symbols and 128 double size symbols.

## COLOUR SPECIFICATION

There are 4096 colour shades. 16 may be used as symbol foreground colours; 8 for symbol background colours and 8 for the cursor.

### BLINKING

When blinking is specified, the foreground symbol will change between the specified colour and colour 0, which is normally black. The background of the symbol will change in the same manner, but with double frequency.

### CURSOR CONTROL

The normal addressing mode for alphanumeric text is to step the character address (cursor) such that characters are sequentially positioned to the right on the line. For semigraphic applications however, the addressing mode may be in any direction: right, left, up or down. Special control characters or function keys determine the addressing mode.

The position of the cursor is held in a cursor register. The advantage of this design is that the cursor may be used exclusively for interaction with the operator. A special «Cursor Follow» mode locks the cursor on the character address.

### KEYBOARD

The stand-alone keyboard has a full ASCII (lower and upper case) character set plus 5 keys for cursor control, a numeric keypad and 12 function keys.

### TRACKERBALL

An optional trackball offers a simple and easy way of pointing and addressing variables in the picture. The trackball unit controls a blinking cross on the screen that follows the ball’s direction of movement. Thus, the cursor can be moved across the screen at any angle, not only in the x and y directions. The cursor is assigned the foreground colour of the highest priority (yellow) ensuring high contrast and easy handling. The trackball has a resolution of 128 steps per revolution.

### NCT SOFTWARE

The NCT SERVICE PROGRAM is available for SINTRAN III/VS systems. The program may be used to define symbols and colours, and to interactively generate and display pictures. A set of library functions is provided for the user program.

---

## Page 2

# SPECIFICATIONS

## COLOURS

| Description                           | Value  |
|---------------------------------------|--------|
| Total number of colour shades         | 4096   |
| Foreground colours                    | 16     |
| Background colours                    | 8      |
| Cursor colours                        | 8      |
| Colour combinations per symbol        | 128    |
| Trackeball marker                     | yellow |

## SYMBOLS

| Description                 | Value |
|-----------------------------|-------|
| Single size character set   | 256   |
| Double size character set   | 128   |

## DISPLAY FORMAT

| Description     | Value  |
|-----------------|--------|
| Number of lines | 18 - 48|
| Symbols per line| 64     |

## DISPLAY

Standard CCIR 625 lines, 50 frames per second without interlace. The system is intended for use with a standard RGB monitor.

# PROGRAMMING SPECIFICATION

7-bit coded characters are used to control the CTI.

A standard TTY driver may be used for both input and output, but automatic echo on input must be omitted.

When the CTI is locally mounted, (in the ND-100 rack), a reduction in CPU loading may be achieved by using a special driver with simple skip-testing instead of the standard interrupt based driver.

There are four main groups of characters. These are:
- CONTROL CHARACTERS
- CONTROL-MODE CHARACTERS
- CHARACTER-MODE CHARACTERS
- ADDRESS-MODE CHARACTERS

# REQUIREMENTS

- The ND-100 COMPACT, the ND-100/CX or the 500/CX systems

# DOCUMENTATION

- CTI Color Terminal Interface User Guide ..... ND-60.223
- CTI Color Terminal Interface Hardware Manual ... ND-12.032

# CONTACT INFORMATION

```
CORPORATE HEADQUARTERS
O. H. Reals vei 5
P.O. Box 25, S
Bogorav 62/1 Oslo 6
NORWAY
Tel.: 02-253600

Oslo, tel. 02-390030, tix. 18661 nd n
Bergen, tel. 05-220290
Sandnes, tel. 04-657650
Tromso, tel. 083-79186
Trondheim, tel. 07-921222, tix. 55580 nd trd
Stockholm, tel. 08-6219020, tix. 15255 nordata s
Gotenborg, tel. 031-692670
Malmo, tel. 040-70150
Copenhagen, tel. 05-42-0556, tix. 37725 nd dk

Jerikoveien 20
P.O. Box 4
Linderberg Gard
1007 Oslo 10
NORWAY
Tel.: 02-390030
Tix: 18661 nd n
Telefax: 02-392647

S-851 88 Sundsvall
SWEDEN
Tel.: 46-60-145150

Vaxjo 46-470-46200
Stockholm, tel. 46-8-7503070
```

[Photo: Company logos and additional contact details]

---

