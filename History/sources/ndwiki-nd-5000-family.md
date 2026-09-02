# Source: ndwiki article "ND-5000 family"

- **Live page**: <https://www.ndwiki.org/wiki/ND-5000_family>
- **Copy used here**: Wayback Machine snapshot of 4 October 2022
  http://web.archive.org/web/20221004165948/https://www.ndwiki.org/wiki/ND-5000_family
- **Fetched**: 2026-08-27, by Ronny's request.

**Status: SECONDARY, and largely redundant here.** This repo already has
`SINTRAN/ND5000/ND5000-FAMILY-MODELS-REFERENCE.md`, built from the primary manuals
in `Reference-Manuals/500/` - ND-05.020.01 Hardware Description, ND-05.017.01
Hardware Maintenance, ND-05.022.1 Microprogram Guide and SAMSON Expected Behaviour.
Every distinctive term on this wiki page - KUSK, GAMP, AAP, WICO, the '87
extensions - already appears in that file or in those manuals. **Prefer them.**

Keep this page for two things: it is a compact summary of the model structure, and
it confirms from outside that our own reading of those manuals is right.

## What it says

- The ND-5000 family is the **third generation** on the ND-500 architecture, a new
  construction in hardware and physical layout. Same instructions as the ND-500 CPU
  plus **25 new instructions, the "'87 extensions"**, also found in the ND-500/1
  and ND-500/2 CPUs.
- The new CPU **replaces 24 cards** of the old ND-500. A single-processor
  **ND-5700 has the same computational power as an ND-570**.
- It runs **SINTRAN III and NDIX**, ND's Unix - corroborating the Unix material in
  the English Wikipedia Norsk Data article.
- **The family uses an ND-100/ND-110/ND-120 CPU as I/O processor**, which is the
  host-and-slave pattern again, and matches English Wikipedia saying ND-500 systems
  were upgraded to the ND-110CX in an I/O role.

### Samson and Rallar

- **Samson**: ND-5200, ND-5400, ND-5500, ND-5700, ND-5800. The ND-120 CPU line
  forming the ND-100 side of most ND-5000 machines was named **Delilah**. And the
  page states the problem plainly: as the 5000 line got faster, **the dual
  ND-100/500 architecture became bottlenecked because all I/O had to pass through
  the ND-100.**
- **Rallar**: ND-5830 and ND-5850. The Rallar processor was two main VLSI gate
  arrays, **KUSK and GAMP** - "Jockey" and "Horse".

### The models are mostly the same hardware

This is the useful part. Three CPU types - type 1 has two layers of cards, type 2
three, type 3 four. Type 1 lacks the layer holding the instruction and data cache
and the **AAP (Additional Arithmetic Processor)**, so on type 1 floating point is
done by the microprogram.

Model differences then come from just three things: which CPU type, whether the
master clock is jumpered **Slow (156 ns) or Normal (70 ns)**, and which caching
features are enabled.

| System | CPU type | I-cache | D-cache | Address cache | Smart IfGo | WICO | Clock |
|--------|----------|---------|---------|---------------|------------|------|-------|
| ND-5200 | 1 | - | - | - | - | - | 70 ns |
| ND-5400 | 2 | yes | - | - | yes | - | 156 ns |
| ND-5500 | 2 | yes | yes | - | yes | - | 156 ns |
| ND-5700 | 2 | yes | yes | yes | yes | - | 70 ns |
| ND-5800 | 3 | yes | yes | yes | yes | yes | 70 ns |

**ND-5900** is an ND-5800 with 2, 3 or 4 CPUs. Compact models came later -
ND-5200 Compact and ND-5700 Compact are named in the text, with ND-5400 and
ND-5500 Compact also in the comparison table. The page carries upgrade procedures
for 5200 to 5400 to 5500 to 5700 to 5800 to 5900, including "Setting of the
ND-5000 CPU model" - so the model was, at least partly, a setting.

---

## Verbatim extract

The ND-5000 family is the third generations system based on the ND-500 architecture. It is a totally new construction both in hardware and physical layout. The ND-5000 CPU uses the same instructions as the ND-500 CPU but adds 25 new instructions[1] called '87 extensions', also found in the ND-500/1 and ND-500/2 CPUs.

The new ND-5000 CPU replaces 24 cards in the old ND-500 arcitecture. The single processor ND-5700 system have the same computational power as a ND-570 system.

The ND-5000 series of computers is able to run both SINTRAN III and NDIX, Norsk Data's version of UNIX.

### Contents

- 1 Hardware implementation

- 1.1 Samson

- 1.2 Rallar

- 2 CPU types

- 3 System performance

- 4 Models

- 4.1 The ND-5000 Compact series

- 5 Hardware Upgrades

- 5.1 ND-5200 to ND-5400 system

- 5.2 ND-5400 to ND-5500 system

- 5.3 ND-5500 to ND-5700 system

- 5.4 ND-5700 to ND-5800 system

- 5.5 ND-5800 to ND-5900 system

- 5.6 Setting of the ND-5000 CPU model

- 5.7 Updating tool for ND-5000 Compact Series

- 5.8 Updating tool for ND-5000 Large Cabinet version

- 6 Remaining systems

- 7 Reference

### Hardware implementation

The ND-5000 family systems uses a ND-100/ND-110/ND-120 CPU as I/O processor.

### Samson

Sold as the ND-5200, ND-5400, ND-5500, ND-5700, and ND-5800. The ND-120 CPU line, which constituted the ND-100 side of most ND-5000 computers, was named Delilah. As the 5000 line progressed in speed, the dual-arch ND-100/500 configuration increasingly became bottlenecked by all I/O having to go through the ND-100.

### Rallar

Sold as the ND-5830 and ND-5850. The Rallar processor consisted of two main VLSI gate arrays, KUSK and GAMP - meaning "Jockey" and "Horse", respectively.

### CPU types

The ND-5000 CPUs came in 3 types. Type 1 had two layers of cards, type 2 had three layers of cards, and type 3 had four layers of cards. Type 1 and type 2 used similar mother boards/baby modules, the difference was that a layer with instruction/data cache and the AAP (Additional Arithmetic Processor) was missing in the type 1 CPUs. The missing AAP meant that on type 1 CPUs floating point operations were performed by the microprogram[2].

### System performance

The difference in performance between models was mainly done by 3 methods:

- Select a CPU (Type 1 / Type 2 / Type 3).

- Jumper the master clock speed to Slow (156 ns) or Normal (70 ns).

- Disable or enable a number or performance enhancing caching features.

Different combinations of the above resulted in the different ND-5000 models. The table below illustrates this[3].

ND-5000 model differences and list of enabled functions

ROW| 
System | 
CPU type | 
Instruction cache
 | 
Data cache | 
Address cache | 
Smart IfGo cache
 | 
WICO (Write in Cache only)
 | 
Master clock speed
 | 

ROW| 
ND-5200 | 
1 | 
- | 
- | 
- | 
- | 
- | 
70 ns
 | 

ROW| 
ND-5400 | 
2 | 
Yes | 
- | 
- | 
Yes | 
- | 
156 ns
 | 

ROW| 
ND-5500 | 
2 | 
Yes | 
Yes | 
- | 
Yes | 
- | 
156 ns
 | 

ROW| 
ND-5700 | 
2 | 
Yes | 
Yes | 
Yes | 
Yes | 
- | 
70 ns
 | 

ROW| 
ND-5800 | 
3 | 
Yes | 
Yes | 
Yes | 
Yes | 
Yes | 
70 ns
 | 

### Models

The original series of ND-5000 computers came in three different models, ND-5700, ND-5800 and ND-5900. The ND-5900 models are the same as the ND-5800, but with multiple CPUs (2, 3 or 4). Later additions to the family were Compact models, ND-5200 Compact and ND-5700 Compact[4].

ND-5000 system comparision chart

ROW| 
System | 
Relative CPU performance | 
Memory shared (MB) | 
Memory local (MB)
 | 
Data cache (KB) | 
Instruction cache (KB) | 
Max disk capacity (GB)
 | 

ROW| 
ND-5200 Compact | 
 | 
 | 
 | 
 | 
 | 

 | 

ROW| 
ND-5400 Compact | 
 | 
 | 
 | 
 | 
 | 

 | 

ROW| 
ND-5500 Compact | 
 | 
 | 
 | 
 | 
 | 

 | 

ROW| 
ND-5700 | 
 | 
8 - 512 | 
2 | 
64 | 
320 | 
29
 | 

ROW| 
ND-5700 Compact | 
 | 
 | 
 | 
 | 
 | 

 | 

ROW| 
ND-5800 | 
2 | 
16 - 512 | 
4 | 
64 | 
320 | 
29
 | 

ROW| 
ND-5900 model 2 | 
4 | 
16 - 512 | 
4 | 
2x64 | 
2x320 | 
29
 | 

ROW| 
ND-5900 model 3 | 
6 | 
16 - 512 | 
4 | 
3x64 | 
3x320 | 
29
 | 

ROW| 
ND-5900 model 4 | 
8 | 
16 - 512 | 
4 | 
4x64 | 
4x320 | 
29
 | 

### The ND-5000 Compact series

The ND-5000 Compact series is equipped with:

- ND-110/CX I/O Processor (ND-110 in ND-5200 Compact system)

- Internal disks or a controller for external disks

- One Streamer, 125 MB (Option on systems with external disks)

- One floppy-disk drive (1.2 MB capacity)

- 4 to 6 MB memory

- SINTRAN and utilities

All ND-5000 Compact systems are available in two models: A model with internal disks and B model with external disk option. A models include from one to four internal disks of 125 MB capacity each (called models A1 to A4). ND-5200
Compact system includes an extra model with one 60 MB internal disk (called model A0). Model B versions are delivered with a controller for external disks and can be configured with external disks and magtape.

### Hardware Upgrades

The following upgrades are possible

### ND-5200 to ND-5400 system

- Exchange the ND-5000 CPU from CPU type 1 to CPU type 2.

- Exchange the ND-110 CPU with ND-120 CPU with 4Mb memory.

- Use the updating tool to set the CPU model to 4.

- Exchange the ND-5000 microprogram with version 144xx. (Remember to change switch settings for MPM port and local 100 memory)

### ND-5400 to ND-5500 system

- Use the updating tool to set the CPU model to 5.

- Exchange the ND-5000 microprogram with version 145xx.

### ND-5500 to ND-5700 system

- Use the updating tool to set the CPU model to 7.

- Exchange the ND-5000 microprogram with version 147xx.

- Exchange the ND-110/CX CPU with ND-120/CX-4MB. (Remember to change switch settings for memory 1imits for MPM port and loca1 100 memory).

### ND-5700 to ND-5800 system

- Exchange the ND-5000 CPU from CPU type 2 to CPU type 3.

- Exchange the ND-120/CX-2MB CPU with ND-120/CX-4MB CPU. (Remember to change switch settings for memory 1imits for MPM port and 1oca1 100 memory)

- Use the updating tool to set the CPU model to 8.

- Exchange the ND-5000 microprogram with version 148xx.

### ND-5800 to ND-5900 system

- Insert extra ND-5000 CPU type 3 [1,2 or 3 extra CPU's).

- Insert "Samson console print" behind each extra ND-5000 CPU.

- Use the updating tool (*) to configurate and set the CPU model to 8 for the extra ND-5000 CPU's:

- ND-5000 CPU 1, octobus station no. 708

- ND-5000 CPU 2, octobus station no. 718

- ND-5000 CPU 3, octobus station no. 728

- ND-5000 CPU 4, octobus station no. 738

Updating tool to be used on ND-5000 Compact systems: Part no: 350156 Double Bus Contr. updating tool.

Updating tool to be used on ND-5000, large cabinet version: Part no: 350157 MF Bus Controller updating tool.

Note: The updating tool will be available in limited volume and will normally only be needed when upgrading the ND-5000 CPU.

### Setting of the ND-5000 CPU model

Setting of the CPU model has to be done when the ND-5000 CPU has to be upgraded or when the content of the EEPROM in the MF backplane is cleared or lost.

### Updating tool for ND-5000 Compact Series

Part no: 350156 Double Bus Contr. updating tool.

In this kit only the special PROM are available.

To be able to set the CPU model, exchange these PROMs with the one on the Double bus controller.

PROM version 27/11 -87

 pos 16J, 18J, 16K and 18K

Comment: The MF bus will not be available when these PROMs are used. These PROMs are only to be used during initializion of the MF bus or setting the CPU model on ND-5000 CPU's.
To set the CPU model, the command shown below must be used.

Example:

=========================================================
= MF bus - TEST AND MAINTENANCE PROGRAM - =
= INTERNAL VERSION for 5465 (5454) =
= November 11, 1987 =
=========================================================

 - * INITIALIZING MF-BUS MEMORY * -
BANK NOT PROPERLY INITIALIZED - NOT AVAILABLE

>SET-CPU-MODEL

DANGER! YOU CAN DAMAGE YOUR SYSTEM

PASSWORD:

SLOTNO:6 ?% Slot position of the ND-5000 CPU
CPU:7 ?% ND-SOOO CPU model ref. list abowe.
 ?% Values 2,4,5,7 or 8.

 - WRITING TO NONVOLATILE MEMORY, PLEASE WAIT -
NEW-PASSWORD (Y/N):N

To verify that the CPU model is correct the following command can be used:

>LIST-CONFIGURATION
Slotno:6
SLOT 06 ?: ND 5000 MODEL: 00B
STATION NO: 0000708
POWER FAIL DESTINATION: 000001B
BROADCAST TYPE: OOOOOOB
SPEED: OOOOO1B
CPU MODEL: 000007B
MASTER CONTROL REG?: 000201B

LIMITS THAT DEFINE ACCESS-AREAS FOR THIS SLOT

When the correct ND-5000 CPU mode] setting has been set, the normal PROMS has to be inserted again on the MF bus controller to be able to run the system.

Note:

- The MF-bus will not be set available when using the upgrading tool. When setting of the CPU model is finished, exchange from the upgrading tool, back to the old MF controller with correct version of the MF PROM's.

- Check what kind of AAP module (Checkpoint 3) is installed and use the correct microprogram according to CPU model. Ref. overview of the ND-5000 microprograms.

Warning:

- If the updating tool is not available, the following commands in the MF maintenance program must be avoided:

>INITIATE-EEPROM with slot number equal to the MF controller.
>CONFIGURATE-SLOT with slot number equaI to the ND-5000 CPU and the configuration is saved.

These two commands will destroy the CPU model setting for the ND-5000 CPU.

### Updating tool for ND-5000 Large Cabinet version

Part no: 350157 MF Bus Controller updating tool.

In this kit onIy the special PROM are available. To be able
to set the CPU model, exchange these PROMs with the one on
the MF bus controller.

PROM version 11/11 -87

 pos 18C, 20C, 22C and 23C

Use the command SET-CPU-MODEL to set the correct model.

### Remaining systems

This is a list of some of the known surviving machines. Know about a machine not in this list? Let us know!

- There are several machines in the Telemuseums storage in Fetsund.

- There are several machines in the collection of Gandalf.

- One ND-5200 Compact with cards.

- One ND-5xxx Compact empty cabinet.

- ND-5700 Serial unknown 1 large cabinet with cards.

- ND-5700 Serial unknown 2 large cabinet without cards.

- Two ND-5000 full size racks named Piff and Puff with Several CPU:s included. Previously with the computer club LUDD in Luleaa.

- A ND-5800 named Batman and ND-5830 named Robin together with a filestore. Previously part of the collection of Dansk Datahistorisk Forening.

### Reference

- ^ Norsk Data Document ND-05.009.3 NORD-500 REFERENCE MANUAL 

- ^ ND-5000 Hardware Description ND-05.020.1

- ^ ND-5000 Hardware Description ND-05.020.1

- ^ PRODUCT NEWS November 1987

- Norsk Data Document ND-05.017.01 ND-5000 Hardware Maintenance

- NEWS March 1987, pg. 52-53, "The ND-5000 Series: Removing Hardware Limitations".

- Norsk Data Document ND-20.060.1 [[Documentation list#Unknown|]]

ROW| 

 | 
 ?This article is a stub. You can improve NDWiki by expanding it. |
