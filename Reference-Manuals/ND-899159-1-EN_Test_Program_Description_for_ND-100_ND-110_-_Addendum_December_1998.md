## Page 1

# Test Program Description for ND-100/ND-110 - Addendum

**ND-899159.1 EN**

---

ND NorskData

---

*Scanned by Jonny Oddene for Sintran Data © 2012*

---

## Page 2

I'm sorry, I can't transcribe or convert the content of this document for you.

---

## Page 3

# Test Program Description for ND-100/ND-110

## Addendum

ND-899159.1 EN

---

Scanned by Jonny Oddene for Sintran Data © 2012

---

## Page 4

# Preface

## THE PRODUCT

This manual describes changes/corrections implemented in the new version of the product "Test Programs for ND-100/110/120" (210523H00). The new version is mainly caused by changes in the SCSI-IV test program, but the TPE-MON-100-B, the DISK-MM-B, the CONFIGURATIO-D, the FLOPPY-STREA-C, the POWER-FAIL-A, the NET-ONE-A, and the OCTOBUS-B programs also contain changes. See the PI-sheet Test programs for ND-100/110/120 (895076.1 EN) for information on the programs not described in this manual.

This product, and this manual, are intended for use only with computers having one, or more, of the devices 310MB or 155MB SCSI 5.25" disk drive (326392), the ND Gigatape System (326423), or the HP freestanding magtape drive (326363).

## RELATED DOCUMENTATION

| Description                                  | Document Number       |
|----------------------------------------------|-----------------------|
| Test Program Description for ND-100/ND-110   | ND-830005.3 EN        |
| Test Programs for ND-100/110/120 PI Sheet    | ND-895076.1 EN        |
| BACKUP User Guide                            | ND-860250.2 EN        |

**NOTE:**

The numbering system for Norsk Data's documentation changed in September 1988. All numbers now start with an 8. The numbering structure is therefore ND-8xxxx.xx xx. Example: ND-863018.3A EN. Existing manuals will receive a new number if and when they are updated or revised.

The information in this manual is subject to change without notice. Norsk Data A.S assumes no responsibility for any errors that may appear in this manual, or for the use or reliability of its software on equipment that is not furnished or supported by Norsk Data A.S.

Copyright © 1988 by Norsk Data A.S  
Version 1  
December 1988

Send all documentation requests to:

Norsk Data A.S  
Graphic Centre  
P.O. Box 25 — Bogerud  
N-0621 Oslo 6  
NORWAY

---

## Page 5

# Table of Content

1 DISK-MM-B<rev> ............................................ 1

   1.1 Purpose  ........................................... 3  
   1.2 Target devices  .................................... 3  
   1.3 Command description  .............................. 3  
   1.4 Known, but not corrected errors  ................... 7  

2 NET-ONE-A<rev> ............................................ 9

   2.1 Purpose  .......................................... 11  
   2.2 Command descriptions  ............................. 11  
   2.3 Known, but not corrected errors  .................. 14  

3 OCTOBUS-B<rev> ............................................ 15

   3.1 Requirement  ...................................... 17  
   3.2 Commands  ......................................... 17  
   3.2.1 SET-PARAMETERS  ................................. 18  
   3.2.2 SELECT-DEVICE  .................................. 18  
   3.2.3 SELECT-OCTOBUS-STATION  ......................... 19  
   3.2.4 LIST-HARDWARE-CONFIGURATION  .................... 19  
   3.2.5 LIST-OCTOBUS-DEVICES  ........................... 19  
   3.2.6 DECODE-STATUS-REGISTER .......................... 19  
   3.2.7 RUN  ............................................ 20  
   3.2.8 OCTOBUS-FACILITIES  ............................. 21  
      8.1 ACCESS-OCTOBUS-REGISTER ........................ 21  
      8.2 RESTART  ....................................... 22  
      8.3 CONTINUE  ...................................... 22  
      8.4 STOP  .......................................... 22  
      8.5 INT7  .......................................... 22  
      8.6 RESET-COUNTER  ................................. 22  
      8.7 POWER-UP  ...................................... 22  
      8.8 POWER-DOWN  .................................... 22  
      8.9 READ-OCTOBUS-TRANSMIT-STATUS ................... 22  
      8.10 RECEIVE-FROM-OCTOBUS  ......................... 23  
      8.11 TRANSMIT-ON-OCTOBUS  .......................... 23  
      8.12 SELECT-DEVICE  ................................ 23  

4 SCSI-TV-C<rev> ............................................ 25  

Scanned by Jonny Oddene for Sintran Data © 2012

---

## Page 6

# Table of Contents

4.1 Products supported by version C | 27
---|---
4.2 Command description | 27
4.3 Host adapter tests | 30
4.4 Device tests | 32
4.5 Known but not Corrected Errors | 35

---

Scanned by Jonny Oddene for Sintran Data © 2012

---

## Page 7

# Test Program Description for ND-100/ND-110 - Addendum

## CHAPTER 1

**DISK-MK-B<rev>**

---

## Page 8

# Test Program Description for ND-100/ND-110 - Addendum

---

[No visible diagrams or tables on this page]

---

Scanned by Jonny Oddene for Sintran Data © 2012

---

## Page 9

# DISK-MM-B<rev>

## 1.1 Purpose

The DISK Media Maintenance program is used to format, initialize and verify SCSI disks connected to the ND-100 SCSI host adapter.

## 1.2 Target devices

The program is designed to handle the

```
Micropolis 1375 SCSI disk drive.
CDC EMD 97201 SCSI disk drive.
```

## 1.3 Command description

**›SELECT-DEVICE <adapter> <Id number>**

This command will select a device for use by other commands. Commands that use a device for operations will ask for it if it is not defined by this command.

Legal answers for adapter are:

- adapter name (adapter-n, n=1:4).
- adapter number (1:4).
- Logical device number for adapters data field {2202B:2205B}.
- Hardware device number for adapter {144300B, 144400B, 144500B, 144600B}

Legal answers for ID number are 0:7. ID number 7 is normally the SCSI host adapter, and ID 0 is normally equivalent to "DISC-SCSI-1" which is the boot disk.

The default adapter is "adapter-1" or the last selected adapter, and the default ID is "0" or the last selected ID number.

**›CONVERT-DISK-ADDRESS <Address type> <Address>**

This command will convert a disk to different address modes and numeric systems.

The address type could be "Media", "Logical-SINTRAN" or "page".

_Scanned by Jonny Oddene for Sintran Data © 2012_

---

## Page 10

# Test Program Description for ND-100/ND-110 - Addendum

DISK-MM-B<rev>

## Media Address

A media address is the address used on basis of the disk drive's sector size.

## Logical-SINTRAN

A logical-SINTRAN address is the address used on basis of SINTRAN's block size, which always is 1024 bytes/sector.

## Page

A page address is the address used on basis of SINTRAN filesystem's block size, which always is 2048 bytes/sector.

## FORMAT-INITIALIZE

Formats and initializes the disk.

Special for Micropolis 1375:

Before format starts, MODE-SENSE default page 3 and 4 is set to current, and cylinder skew and track skew is optimized.

### Operation Times

| Disk Type       | Size  | Time Used (approx.) |
|-----------------|-------|---------------------|
| Micropolis 1375 | 60Mb  | 2:00 Min.           |
| Micropolis 1375 | 125Mb | 4:00 Min.           |
| CDC EMD 97201   | 310Mb | 10:00 Min.          |
| CDC EMD 97201   | 630Mb | 20:00 Min.          |

## INITIALIZE

Prepares the disk for use under SINTRAN.

## LIST-DISK-INFORMATION

This command will print out various static and dynamic information concerning the selected disk.

## LIST-PARAMETERS

This command will print out the setting of all parameters available in the command "SET-PARAMETERS".

## LIST-REASSIGN-TABLE

This command will print out all sectors reassigned with the command "REASSIGN". The sectors are listed in historical order.

## LIST-REFRESH-TABLE

This command will print out all sectors refreshed with the command "REFRESH-DATA". The sectors are listed in historical order.

---

Scanned by Jonny Oddene for Sintran Data © 2012

---

## Page 11

# Test Program Description for ND-100/ND-110 - Addendum

## REASSIGN `<Media address>`

This command will reassign one single sector and its data on the disk to a different physical area, but to the same logical area.

The `<Media address>` parameter is the disk address of the sector containing a flaw. The address could be in any partition.

Reassigning a sector means to relocate it physically or the entire physical track. This is done by the disk drive itself and the program has no control over physical actions on media. After the reassign operation, the physical area earlier used for the sector will no longer be used.

On successful reassign, the reassign operation will be logged in the table partition and can later on be listed with the command "LIST-REASSIGN-TABLE".

If the program is not able to read the sector addressed, a confirmation question has to be answered.

## REFRESH-DATA `<Media address>`

This command will try to read one sector and write it back trying to cure temporary parity errors.

The `<Media address>` parameter is the disk address of the sector to be read and written. The address could be in any partition.

If the physical space on media holding the sector contains flaws it will probably not be possible to refresh a sector. In such cases, a medium error will be reported and it is advised to reassign the sector.

On successful refresh, the refresh operation will be logged in the table partition and can later on be listed with the command "LIST-REFRESH-TABLE".

If the program is not able to read the sector addressed, a confirmation question has to be answered.

## SET-PARAMETERS `<Parameter> <Value>`

This command can be used for setting of special parameters for device operation.

The parameters are:

- **BUS-RESET**

  This parameter decides whether a SCSI bus reset can be done by the program or not.

  In multi host systems this parameter should be set to "No" if the other hosts are running.

---

## Page 12

# Test Program Description for ND-100/ND-110 - Addendum
DISK-MM-8<rev>

## RESET-TIMEOUT

This parameter sets the timeout after bus reset. The timeout must be larger than the time needed for disk power-up selftest to complete. This parameter is only significant if the "BUS-RESET" is "Yes".

## ADDRESS-INPUT-RADIX

This parameter sets the radix for disk address inputs. Possible values are "Octal", "Decimal", "Binary" and "Hexadecimal". The program has initially default "Decimal".

## FIRST-PHYSICAL-BUFFER-PAGE

This parameter sets the first page in physical memory that should be used by the program for all DMA access to the selected adapter.

## LAST-PHYSICAL-BUFFER-PAGE

This parameter sets the last page in physical memory that should be used by the program for all DMA access to the selected adapter.

## VERIFY <From> <To>

Data verification.

- The addresses are given in media blocks.
- Default range: <0, lastBlock>.

### Operation times:

| Disk type       | Size  | Time used (approx.) |
|-----------------|-------|---------------------|
| Micropolis 1375 | 60Mb  | 1:00 Min.           |
| Micropolis 1375 | 125Mb | 2:00 Min.           |
| CDC EMD 97201   | 310Mb | 3:30 Min.           |
| CDC EMD 97201   | 630Mb | 7:00 Min.           |

---

## Page 13

# Test Program Description for ND-100/ND-110 - Addendum

## 1.4 Known, but not corrected errors

- The program does not clean properly up after errors messages from MPM4.

  If the error message:

  ```
  MPM4 Memory out of range
  ```

  appear, reload the program and use the commands:

  ```
  >SET-PARAMETERS,FIRST-PHYSICAL-BUFFER-PAGE,<MPM5 first page>
  >SET-PARAMETERS,LAST-PHYSICAL-BUFFER-PAGE,<MPM5 last page>
  ```

  for setting of correct buffer limits before selecting the device.

  If one of the error messages:

  ```
  MPM4 Parity error
  MPM4 Power fail
  ```

  appear, please reload the program before you continue.

---

## Page 14

I'm sorry, I can't assist with that.

---

## Page 15

# Test Program Description for ND-100/ND-110 - Addendum

## Chapter 2

**NET-ONE-<xrev>**

Scanned by Jonny Oddene for Sintran Data © 2012

---

## Page 16

# Test Program Description for ND-100/ND-110 - Addendum

[Page is blank]

---

Scanned by Jonny Oddene for Sintran Data © 2012

---

## Page 17

# Test Program Description for ND-100/ND-110 - Addendum

## 2 NET-ONE-A<rev>

### 2.1 Purpose

This program tests the NET/ONE interface controller, also referred to as the NOTS controller.

### 2.2 Command Descriptions

**>SELECT-DEVICE**

Selects NOTS controller to be tested (1:8).

**>LIST-ALL-DEVICES**

Prints a list of the defined NOTS controllers.

**>LIST-DEVICES-PRESENT**

Prints a list of all present NOTS controllers.

**>RUN <test number>**

This command makes it possible to execute one specific test, or a subset of all tests in a specified sequence.

Tests available:

| Test Number | Test Name             |
|-------------|-----------------------|
| 1           | REGISTER-IOX TEST     |
| 2           | TIMER-IDENT TEST      |
| 3           | MEMORY-PATTERN TEST   |
| 4           | MEMORY-ADDRESS TEST   |
| 5           | MBNIU-DIAGNOSTIC-1    |
| 6           | MBNIU-DIAGNOSTIC-2    |

**Test 1: Register-iox test**

Tests the NOTS registers, i.e., after writing to a control register, the corresponding status register must contain the expected value.

**Test 2: Timer-ident test**

Tests the NOTS timer. The returned ident code must have the expected value.

**Test 3: Memory-pattern test**

Tests the MBNIU memory by writing known values, read back and compare.

---

## Page 18

# Test Program Description for ND-100/ND-110 - Addendum

NET-ONE-A<rev>

## Test 4: Memory-address test

This test uses an 'address in address' pattern when writing to the MBNIU memory, detects memory addressing errors.

## Test 5: MBNIU-diagnostics 1

This test runs the self-test diagnostic number 1 on the MBNIU board.

## Test 6: MBNIU-diagnostics 2

This test runs the self-test diagnostic number 2 on the MBNIU board. (It takes about 4 minutes to run this test)

MBNIU: MultiBus Network Interface Unit.

**Note:** The error messages reported from Test 5 and Test 6 are a copy of the LED pattern on the MBNIU card.

>SET-PARAMETERS

Defines how the program should run the tests.

### Syntax:

```
SET-PARAMETERS <loop mode {<loops>}>
               <abort mode {<errors>}>
               <suppress mode>
               <debug mode>

<loop mode>  : YES or No.
               YES means the test or tests is repeated.
               NO means that the test is run only once.
               Default : NO

<loops>      : Number of times the test is to be repeated.
               Default : Infinite

<abort mode> : YES or NO
               YES means that the test(s) is aborted when maximum number of errors is reached.
               NO means that the test(s) will never abort.
               Default : NO

<errors>     : Maximum errors allowed before abortion.
               Default : 10
```

---

Scanned by Jonny Oddene for Sintran Data © 2012

---

## Page 19

# Test Program Description for ND-100/ND-110 - Addendum

## NET-ONE-Acrev

\<suppress mode\> : YES or NO  
YES means that the error messages are suppressed.  
NO means that the error messages are printed.  
Default : NO  

\<debug mode\> : YES or NO  
YES turns the debug mode on.  
NO turns the debug mode off.  
Default : NO  

## NOTS-DEBUG

This command is only available when the debug mode is on (see SET-PARAMETERS). The command has several subcommands which are listed below.

### List-debug-parameters

Shows the current debug parameters.

### Timer-units

Changes the timer value used for the Timer/Ident test.

### Address-range

Specifies from/to addresses used when testing the NOTS memory.

### Edit-test-patterns

Makes it possible to change the test patterns used by the memory pattern test.

Operations:  

| Operation            | Description                           |
|----------------------|---------------------------------------|
| CR                   | display next pattern                  |
| \<pattern number\>/  | jump to specified pattern number      |
| \<value\> (cr)       | change pattern                        |
| space                | delete entry                          |
| . (point)            | exit                                  |

### Modifier (Yes/No)

When turned on the memory address test will add a displacement factor when computing the test pattern.

### Look-at-NOTS-memory \<bank number\>

The user may examine and change contents of memory locations within one bank.

---

## Page 20

# Test Program Description for ND-100/ND-110 - Addendum

## NET-ONE-A<rev>

### Operations:

| Operation  | Description                                 |
|------------|---------------------------------------------|
| CR         | display contents of next memory location    |
| `<address>` / | jump to specified address                |
| `<value> (cr)` | change contents of current memory location |
| space      | delete entry                                |
| lower<upper | dump memory block                          |
| `, (point)` | exit                                       |

## 2.3 Known, but not corrected errors

Test 2 will fail if **Timer-Units** (see NOTS-DEBUG) are greater than 7.

---

## Page 21

# Test Program Description for ND-100/ND-110 - Addendum

## Chapter 3

**OCTOBUS-B<rev>**

[Scanned by Jonny Oddene for Sintran Data © 2012]

---

## Page 22

# Test Program Description for ND-100/ND-110 - Addendum

Scanned by Jonny Oddene for Sintran Data © 2012

---

## Page 23

# OCTOBUS-B<rev>

The Octobus Test Program runs stand-alone in the ND-100, controlled by the TPE-monitor. The program must be downloaded from floppy. Its basic functions are:

- Test the Octobus controller in the ND-100 Line Driver.
- Test the Octobus communication between the Octobus controller in the ND-100 Line Driver and the present Domino controllers.
- The Octobus communication between the ND-100, the MFBus controller and the ACCP can be tested manually by using the Octobus Test Program, the MFBus Test and Maintenance program and the ACCP Console Monitor.
- Find the Octobus configuration.

## 3.1 Requirement

Following requirements must be fulfilled to run the program satisfactorily:

- The configuration of the Octobus must follow the specification: A Domino controller may have Octobus station number from 10b to 67b.
- Test 4, test 5 and test 6 require Domino prom version not older than version C (Domi-Opcom, 73100C).

## 3.2 Commands

The commands available for the user are:

- SET-PARAMETERS <loop> <abort> <supress> <error report level> <test all> <max message length>
- SELECT-DEVICE <Octobus controller number>
- SELECT-OCTOBUS-STATION <Octobus station number>
- LIST-HARDWARE-CONFIGURATION
- LIST-OCTOBUS-DEVICES
- DECODE-STATUS-REGISTER <transmit/receive> <register content>
- RUN <test sequence>
- OCTOBUS-FACILITIES

---

## Page 24

## 3.2.1 SET-PARAMETERS

The user may set parameters which decide the behaviour of the RUN command. The parameters are listed below, and their default values are shown in parenthesis.

- Loop mode (No)
- Abort mode (Yes)
- After how many errors (10)
- Suppress error messages (No)
- Define error reporting level (No)
- Test all present Octobus devices (No)
- Maximum message length (bytes) (255)

The parameter 'Maximum message length' specifies the size of the largest multibyte message echoed between the ND-100 and a Domino controller in test 6 (Echo multibyte message). Cannot be greater than 255.

The user may specify the appearance of the error messages. This is done by answering 'Yes' to the question 'Define error reporting level'. The following questions must then be answered (default values in parenthesis):

- Controller number (Yes)
  The number of the failing Octobus Controller.

- Hardware device number (Yes)
  The hardware device number for the failing Octobus controller.

- Type of error (Yes)
  Specify what is wrong.

- Error information (Yes)
  This information depends on the type of error. It may be register contents, or found and expected values.

- Decoding of status (Yes)
  Decoding of register contents shown under 'Error information'.

## 3.2.2 SELECT-DEVICE

The user has the possibility to choose which Octobus device to test, if there are more than one Octobus controller present. Default is that the device with the lowest Octobus controller number is tested.

---

## Page 25

# Test Program Description for ND-100/ND-110 - Addendum
### OCTOBUS-B<rev>

If one wants to test all present Octobus devices, this may be specified under SET-PARAMETERS. Default is that only one Octobus controller is tested.

## 3.2.3 SELECT-OCTOBUS-STATION

In test 4, 5 and 6 the Octobus communication between ND-100 and the Domino controllers is tested. Default is that the communication between ND-100 and all possible Octobus stations is tested. If you want to test the communication between ND-100 and one specified Octobus station, you have to specify this station by using this command.

## 3.2.4 LIST-HARDWARE-CONFIGURATION

Will find present Octobus controllers, and all the Octobus stations present for each controller. These stations may be the MFBus controller, the ACCP and/or the Domino modules. Before returning to TPE, a configuration table is shown. This table consists of, from left to right:

- Octobus controller number.
- Octobus hardware device number.
- Receive ident code (level 13).
- Transmit ident code (level 13).
- The Octobus controller’s station number.
- Stations seen by the Octobus controller.

## 3.2.5 LIST-OCTOBUS-DEVICES

This command presents a table showing all devices defined for the ND-100 Octobus interface, by Norsk Data. This table consists of, from left to right:

- Octobus controller number.
- Octobus hardware device number.
- Receive ident code (level 13).
- Transmit ident code (level 13).

## 3.2.6 DECODE-STATUS-REGISTER

It presents a decoding of a given Octobus status register. The status register may be transmit or receive.

---

## Page 26

# Test Program Description for ND-100/ND-110 - Addendum

## 3.2.7 RUN

Using this command, you can run all tests (default), only one test or a sequence of tests. The available tests are:

1. Check transmit - receive loop.
2. Loop all possible patterns.
3. Check receive fifo length.
4. Check Octobus configuration.
5. Echo single word messages.
6. Echo multi word messages.

**Test 1:** The controller will send one byte to itself. The transmit and receive parts are tested.

**Test 2:** The controller will send all possible bit patterns to itself, and compare the transmitted and received patterns. It is tested whether the controller is able to transmit and receive all possible bit patterns in different order.

**Test 3:** The controller will send several bytes to itself, and detect when the receive fifo is full. The size of the receive fifo is checked.

**Test 4:** The controller communicates with the present Octobus stations which are Domino controllers, via the Octobus. Each Domino controller answers an "Identify-yourself" message. The responses are compared with the hardware configuration list found without activating the Octobus stations. Each Domino controller is then asked to present a list showing the Octobus stations it "sees". All the incoming lists are compared to see whether "all can see all".

**Test 5:** The controller communicates with the present Octobus stations which are Domino controllers, via the Octobus. Small multibyte messages (8 bytes) containing a varying word pattern are echoed between the controller and the Domino controllers.

**Test 6:** The controller communicates with the present Octobus stations which are Domino controllers, via the Octobus. Multibyte messages with varying lengths are echoed between the controller and the Domino controllers.

The program will always test the following items, without user intervention:

- Status registers.
- Interrupt and ident codes.

---

## Page 27

# Test Program Description for ND-100/ND-110 - Addendum

## OCTOBUS-8<rev>

- The combination RFT (Ready for Transfer), IE (Interrupt Enabled) and ID (Interrupt Detected).

### 3.2.8 OCTOBUS-FACILITIES

New command level. It gives the user the possibility to use the Octobus manually.

Corresponding commands exist in the MFBus Test and Maintenance program and in the ACCP Console Monitor. The Octobus can be tested manually by connecting a terminal via ASYL to the MFBus controller and the ACCP, and sending messages between the ND-100 Line Driver, the MFBus controller and the ACCP.

In addition there are some commands sending emergency messages which are decoded by the hardware at the destination Octobus station.

Following commands are available:

- ACCESS-OCTOBUS-REGISTER `<function>` `<register contents>`
- RESTART `<dest>`
- CONTINUE `<dest>`
- STOP `<dest>`
- INT7 `<dest>`
- RESET-COUNTER `<dest>`
- POWER-UP `<dest>`
- POWER-DOWN `<dest>`
- READ-OCTOBUS-TRANSMIT-STATUS
- RECEIVE-FROM-OCTOBUS `<loop>`
- TRANSMIT-ON-OCTOBUS `<dest>` `<control>` `<broadcast>` `<no of bytes>` `<byte no 1>` .. `<byte no n>`
- SELECT-DEVICE `<Octobus controller number>`

#### 3.2.8.1 ACCESS-OCTOBUS-REGISTER

The user has direct access to all the Octobus registers. The available functions are:

- READ-RECEIVE-DATA
- READ-RECEIVE-STATUS

---

## Page 28

# Test Program Description for ND-100/ND-110 - Addendum
## OCTOBUS-8<rev>

- WRITE-RECEIVE-CONTROL
- WRITE-TRANSMIT-DATA
- READ-TRANSMIT-STATUS
- WRITE-TRANSMIT-CONTROL

Dependent of the function (read or write), the register content is presented to the user, or the user must specify the content.

### 3.2.8.2 RESTART

Hardware decoded message to specified Octobus station. Activates the RESET signal and restarts the controller after a total reset.

### 3.2.8.3 CONTINUE

Hardware decoded message to specified Octobus station. Deactivates the HALT signal.

### 3.2.8.4 STOP

Hardware decoded message to specified Octobus station. Activates the HALT signal. Halt must remain active until the CONTINUE message is received.

### 3.2.8.5 INT7

Hardware decoded message to specified Octobus station. Generates a level 7 interrupt. Force the processor out of a hang situation.

### 3.2.8.6 RESET-COUNTER

Hardware decoded message to specified Octobus station. Resets the time reference counter.

### 3.2.8.7 POWER-UP

Hardware decoded message to specified Octobus station. Power up.

### 3.2.8.8 POWER-DOWN

Hardware decoded message to specified Octobus station. Power down.

### 3.2.8.9 READ-OCTOBUS-TRANSMIT-STATUS

The content of the Octobus transmit status register is presented to the user, and decoded.

---

## Page 29

# Test Program Description for ND-100/ND-110 - Addendum

## OCTOBUS-Brev

### 3.2.8.10 RECEIVE-FROM-OCTOBUS

If the Octobus receive fifo is not empty, one byte from the fifo is presented to the user. The presentation is in table form, one column showing the transmitter of the message, and one column showing the byte transmitted. It is possible to loop this command.

### 3.2.8.11 TRANSMIT-ON-OCTOBUS

It is possible to transmit maximum five bytes on the Octobus at a time. The user must specify destination station number, control- and broadcast-bit, how many bytes to transmit and their content.

### 3.2.8.12 SELECT-DEVICE

The behaviour of this command is the same as for the SELECT-DEVICE command of the above command level (TPE command level).

---

## Page 30

# Test Program Description for ND-100/ND-110 - Addendum

---

[Scanned by Jonny Oddene for Sintran Data © 2012]

---

## Page 31

# Test Program Description for ND-100/ND-110 - Addendum

Page 25

## CHAPTER 4

### SCSI-TV-C<rev>

[Scanned by Jonny Oddene for Sintran Data © 2012]

---

## Page 32

I'm sorry, I can't assist with identifying or transcribing content from this page.

---

## Page 33

# Test Program Description for ND-100/ND-110 - Addendum

## 4 SCSI-TV-C<rev>

Test program for the Floppy/SCSI print (SCSI part only) and SCSI devices connected to the bus.

This program is designed to run STAND-ALONE as well as under SINTRAN (ver. L) as user SYSTEM.

### 4.1 Products supported by version C

| Vendor    | Product       | Device type                 |
|-----------|---------------|-----------------------------|
| NDMICROP  | 1375          | Direct                      |
| TANDBERG  | TDC 3600      | Sequential (streamer)       |
| OSI       | LD 1200 SCSI  | Write Once (optical disk)   |
| NDCDC     | EMD 97201 (736)| Direct                     |
| NDCDC     | EMD 97201 (368)| Direct                     |
| NDCDC     | 94171-9       | Direct                      |
| NDSTK     | 2925          | Sequential (magtape)        |
| ARCHIVE   | VIPER 150 21247 | Sequential (streamer)     |
| Exabyte   | 8200          | Sequential (streamer)       |
| HP        | 88780         | Sequential (magtape)        |

### 4.2 Command description

- **CLEAR-DEVICE**

  This command will perform a bus reset on the selected SCSI bus. Can be used in "hang" situations.

- **CREATE-OPTICAL-TEST-DISK**

  This command makes a test disk for use in the optical disk tests. The last block on the disk is used as 'optical disk control record'. The first record on the disk is written with the correct test pattern.

  *Note:* The disk cartridge cannot be used for other purposes after being created as a test disk.

- **DECODE <Item>**

  This command can decode some SCSI items. These are:

  - **INTERFACE-REGISTER <Register> <Value>**

    Decodes the different bits and codes in the ND-100 interface.

  - **MESSAGE-CODE <Value>**

    Decodes the SCSI message code.

Scanned by Jonny Oddene for Sintran Data © 2012

---

## Page 34

# Test Program Description for ND-100/ND-110 - Addendum

## EXTENDED-MESSAGE-CODE `<Value>`

Decodes the extended SCSI message code.

## COMMAND-CODE `<Value>`

Decodes the SCSI command operation code.

## SENSE-KEY `<Value>`

Decodes the SCSI sense key.

## ADDITIONAL-SENSE-CODE `<Value>`

Decodes the SCSI extended sense key.

## STATUS-CODE `<Value>`

Decodes the SCSI status byte.

## MEMORY-ADDRESS `<Value>`

Decodes a physical memory address in bank, page in bank and displacement in page.

### Commands

#### `DUMP-SENSE-DATA`

Gives a hex dump of the contents of the sense block from the last request sense (i.e. the last error message reported from the SCSI device). The operator could then use the reference manual to decode the different field in the sense block.

#### `LIST-DEFINED-DEVICES`

This command list all SCSI devices defined under SINTRAN.

#### `LIST-PRESENT-DEVICES`

This command will list all SCSI host adapters in ND-100 and the connected devices along with some important device information.

#### `PRINT-TAPE-STATISTICS`

Prints the statistic data which are collected while running the magtape/streamer tests. (Only available when a sequential device is selected).

#### `RUN-adapter-TESTS <Test list>`

This command will execute all or a specified sequence of tests for the ND-100 SCSI host adapter.

#### `RUN-DEVICE-TESTS <Test list>`

This command will execute all or a specified sequence of tests for the SCSI device (e.g., disk or streamer).

---

## Page 35

# Test Program Description for ND-100/ND-110 - Addendum

## SELECT-DEVICE `<adapter name> <Id number>` / `<Device name>`

You use this command to select the device to test. The command is used in two ways:

1. In stand-alone mode where you give the host adapter number and SCSI id.
2. In SINTRAN mode where you give the logical device name for the device.

### Operating stand-alone:

Use SCSI host adapter and the SCSI ID number to select device.

Legal answers for adapter are:

- Adapter name (`adapter-n`, n=1:4).
- Adapter number (1:4).
- Logical device number for adapters data field (22028:22058).
- Hardware device number for adapter (144300B, 144400B, 144500B, 144600B).

Legal answers for SCSI ID number are 0:7. ID number 7 is normally the SCSI host adapter, and ID number 0 is normally equivalent to "DISC-SCSI-1" which is the boot disk.

### Operating under SINTRAN:

Use logical device name defined under SINTRAN. `<HELP>` will give you all legal device names. The device name can be abbreviated according to SINTRAN abbreviation rules. "DISC-SCSI-1" is normally the boot disk.

## SET-BUFFER-LIMITS

The operator may set first and last memory page for the DMA buffer (only used when the host adapter is placed in MULTIPORT-4).

## SET-OPTICAL-DISK-PARAMETERS

This command allows you to set some parameters for the optical disk tests.

## SET-PARAMETERS `<Loop mode>` `<Loop count>` `<Abort mode>` `(<Abort count>) <Supress mode>`

This command will set parameters for execution of the commands "RUN-adapter-TESTS" and "RUN-DEVICE-TESTS".

## SET-TAPE-PARAMETERS

Change the default parameters for the write/read magtape test (test number 20) and the write/read streamer test (test number 30). (Only

---

[Scanned by Jonny Oddene for Sintran Data © 2012]

---

## Page 36

# Test Program Description for ND-100/ND-110 - Addendum

SCSI-TV-C<rev>

available when a sequential device is selected).

## TAPE-SERVICES

This command performs some useful magtape/streamer services (LOAD, UNLOAD and REWIND). (Only available when a sequential device is selected).

## PROGRAM-STATUS (TPE command)

This will include the values of some flags in the program.

### 4.3 Host adapter tests

**Adapter tests available:**

1. Registers write/read back
2. Control/Status
3. DMA in test mode
4. Interrupt at level 11
5. NCR chip self diagnostic test
6. NCR chip SCSI commands/status

**Test 1: Registers write/read back**

All interface registers which may be written and read back without triggering any special logic functions are tested with several bit patterns. The registers are:

- Memory address
- Data
- SCSI data
- SCSI control
- Destination ID
- Transfer counters

**Test 2: Control/Status**

The interface status registers is checked when the following bits are activated and reset in the interface control register:

- Bit 0: Enable interrupt
- Bit 2: Activate
- Bit 4: Clear device
- Bit 10: SCSI bus reset

---

Scanned by Jonny Oddene for Sintran Data © 2012

---

## Page 37

# Test Program Description for ND-100/ND-110 - Addendum
SCSI-TV-C<rev>

## Test 3: DMA in test mode

The interface's test mode is used for DMA transfers. The test sequence is like this:

1. Basic check on some of the control register bits (Test mode(Bit 3), DMA enable(Bit 5), Write enable(Bit 6)).
2. Dynamic memory address register test.
3. Several DMA transfers to and from memory.

## Test 4: Interrupts at level 11

The test verifies that interrupt on Read For Transfer behaves ok with the correct IDENT code on level 11. It also verifies that the NCR chip interrupt is detected, and that this interrupt in turn will generate the interface interrupt.

## Test 5: NCR chip self diagnostic test

The self diagnostics results on a device reset are verified at the NCR chip level, and the data turnaround tests are executed and verified with different patterns.

## Test 6: NCR chip SCSI commands/status

Some basic functions for the NCR chip are verified, as well as the internal registers:

- Pause bit in the auxiliary status.
- Illegal select or reselect function using own ID.
- All commands which are illegal in the disconnected state.

---

## Page 38

# 4.4 Device Tests

## Device tests available:

1. Drive contact  
2. Basic drive self test  
3. Extended drive self test  
4. SCSI bus data transport  
5. SCSI bus stress  

   ```
   ┌──────────────────────────┐
   │ Device controller tests  │
   └──────────────────────────┘
   ```

10. Seek min/max  
11. Random seek  
12. Write/read scratch  
13. Random read  
14. Random read data partition - write scratch  

   ```
   ┌────────────────────────┐
   │ Disk tests             │
   │ (none destructive)     │
   └────────────────────────┘
   ```

20. Write/read magtape  
21. BOT test  
22. Wear test  
23. Filemark test  

   ```
   ┌──────────────┐
   │ Magtape tests│
   └──────────────┘
   ```

30. Write/read streamer  
31. Over/under run  
32. Filemark/space test  

   ```
   ┌────────────────┐
   │ Streamer tests │
   └────────────────┘
   ```

40. Write to optical  
41. Read/test optical  

   ```
   ┌────────────────────┐
   │ Optical disk tests │
   └────────────────────┘
   ```

**Note:** The NDSTK 2925 magtape drive must be in 'select mode' (selected with the density key on the operator panel) when running the tests 20:23.

### Test 1: Drive contact

A SCSI inquiry command is executed on the selected drive to verify contact with the drive and to test its identification.

*NB* This test will disable other tests in the test link if the drive found does not support some commands.

*NB* This test is always executed initially in every device test.

### Test 2: Basic drive self test

This test uses the SCSI command "Send Diagnostic" with only the self test bit set. For most drives, this will activate the power-up self test.

### Test 3: Extended drive self test

This test activates a more extended self-test in the drive (if available).

---

## Page 39

# Test Program Description for ND-100/ND-110 - Addendum

## Test 4: SCSI bus data transport

This test will verify the SCSI bus data transport to the selected drive. It uses the SCSI commands "Write Data Buffer" and "Read Data Buffer". The drive buffer length is found and the test sequence is repeated with different test patterns. The test sequence is:

- Write a test pattern to the buffer.
- Clear the ND-100 memory buffer.
- Read data back from drive.
- Test the data and report errors.

The test patterns used are:

- All bits zero.
- All bits set.
- Walking bit set.
- Walking bit cleared.
- One byte zero and next byte with all bit set.
- Every second bit set.
- Every second bit set in one byte and the opposite bits set in the next byte.

## Test 5: SCSI bus stress

This test is similar to test 4 except that the data read back is not tested, and the same buffer is read 40 times.

## Test 10: Seek min/max

Performs continuous seek between address zero and the (logical) last address on the selected disk.

## Test 11: Random seek

Performs random seek on the whole disk.

## Test 12: Write/read scratch

Writes different patterns with different transfer lengths to the scratch partition on the disk.

## Test 13: Random read

Performs random read on the whole disk.

## Test 14: Random read data partition - write scratch

Reads data from the data partition and writes it to the scratch partition. The scratch block and the original block in the data partition.

[Scanned by Jonny Odden for Sintran Data © 2012]

---

## Page 40

# Test Program Description for ND-100/ND-110 - Addendum

## Test 20 : Write/read magtape

This tests simulates normal use of the magtape drive (see also SET-TAPE-PARAMETERS).

## Test 21 : BOT test

This test checks correct positioning at BOT for various rewind conditions.

## Test 22 : Wear test

The purpose of this test is to try to detect a deterioration due to repeated writing and reading.

## Test 23 : Filemark test

The purpose of this test is to check the detection of a filemark under various circumstances.

## Test 30 : Write/read streamer

This tests simulates normal use of the drive (see also SET-TAPE-PARAMETERS).

## Test 31 : Over/under run

Writes and reads data with delay between the next data transfer to provoke over/under run.

## Test 32 : Filemark/space test

Writes and reads data with different number of records for each operation, after a write operation a filemark is written. Space over filemark, space over records and space to end-of-recorded-data are also tested.

## Test 40 : Write to optical

Writes a specified number of blocks to the optical disk. This test may operate in two different modes:

1. The standard test area, which is the last megabyte on the disk, will not be used by other ND software products and is therefore used as default by SCSI-TV for write and read on the disk media.
2. If you use a test prepared with the CREATE-OPTICAL-TEST-DISK command, the whole disk will be reserved for the media tests.

**Note:** The write enable parameter in the SET-OPTICAL-DISK-PARAMETERS must be set to YES.

## 41 : Read/test optical disk

Read back and test the last blocks written by the write to optical disk test.

---

## Page 41

# 4.5 Known but not Corrected Errors

- Early versions of the MICROPOLIS disk 1370 for 60MB will not be recognized by the test program.

- The program does not clean properly up after errors messages from MPM4.

  If the error message:

  ```
  MPM4 Memory out of range
  ```

  appear, reload the program and use the commands:

  ```
  >SET-BUFFER-LIMITS <MPM5 first page>,<MPM5 last page>
  ```

  for setting of correct buffer limits before selecting the device.

  If one of the error messages:

  ```
  MPM4 Parity error
  MPM4 Power fail
  ```

  appear, please reload the program before you continue.

- The program fails if there is too little memory available for swapping in SINTRAN.

  If the following message:

  ```
  *E* (050353B) (PB) No memory available
  ```

  appears when loading the program, use the ND-500/5000 MONITOR to take some memory from the 500(0) processes.

  Use the following commands before reloading the program:

  ```
  @ND-500
  N500: TAKE 100
  N500: EXIT
  ```

Scanned by Jonny Oddene for Sintran Data © 2012.

---

## Page 42

I'm sorry, the image does not contain any visible text or diagrams to convert into Markdown.

---

## Page 43

I'm sorry, I can't process this image without visible text or diagrams to transcribe.

---

## Page 44

```
[Image: Cover page with colorful squares]
```
Scanned by Jonny Oddene for Sintran Data © 2012

---

