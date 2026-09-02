# Source: ndwiki article "NORD-1"

- **Live page**: https://www.ndwiki.org/wiki/NORD-1
- **Copy used here**: Wayback Machine snapshot of 28 August 2025
  http://web.archive.org/web/20250828003143/https://www.ndwiki.org/wiki/NORD-1
- **Why the archive**: as of 2026-08-27 the live ndwiki is behind an Anubis
  proof-of-work gate. Both the article URL and the MediaWiki `api.php` return
  "Making sure you're not a bot" to any script, so the page cannot be fetched
  directly. The archived copy is what this file was made from.
- **Fetched**: 2026-08-27, by Ronny's request.

**Status: SECONDARY SOURCE (a community wiki).** ndwiki is written by ND
enthusiasts and ex-employees and is generally careful - it cites ND documents by
number. It is still a wiki, and it marks its own gaps ("citation needed",
"Unknown", "stub"). Anything used in a History chapter needs the underlying ND
document, not this page.

The text below is the article body, converted from the archived HTML. Tables are
flattened to `ROW| a | b | c |`. Wiki markup, images and navigation are dropped;
wording and every number are unchanged.

---

The NORD-1
The NORD-1 was Norsk Data's first minicomputer. It was the first commercially available computer made in Norway.

It was a 16-bit system, developed in 1967, based on the newly released 74 series TTL logic IC:s[1] from Texas Instruments[2]. It could have 4K - 64K words of core memory[3]. The first NORD-1 (serial number 2) installed was an anti-collision system used with the NORCONTROL process control system aboard the ship M/S Taimyr, where it proved extremely reliable for its time.[4]

It was probably the first minicomputer to feature floating-point equipment as standard, and had an unusually rich complement of registers for its time. It also featured relative addressing, and a fully automatic context switched interrupt system.[citation needed]

It was also the first minicomputer to offer virtual memory, offered as an option by 1969.[5]

It was succeeded by the NORD-10.

### Contents

- 1 Hardware

- 1.1 Core Memory

- 1.2 Memory Control

- 1.3 Central Processing Unit

- 1.3.1 Register block

- 1.3.2 Control flip-flops

- 1.3.3 Arithmetic and control units

- 1.4 Control Panel

- 1.5 Backplane

- 2 Remaining machines

- 2.1 Unknown status

- 3 Sources

### Hardware

### Core Memory

Main storage is a ferrite core memory device. It can be from 4096 to 65536 words in size. Each word is 16 bits.[3]

The CPU operates asynchronous to the memory timing control, and the computer can use memories of different speeds. The fastest memory cycle time which the CPU can efficiently use is 1 microsecond.[3]

### Memory Control

Each memory block has its own memory control. The memory control permits access from two different devices to the memory block as standard - additional channels are optional. The priority between the devices is fixed (wired in). One of the devices is the CPU, usually at the lowest priority. The data channels are usually connected to devices such as disc storage, magnetic tape storage, line printers or other input / output devices with hight data transfer rate. When data channels are operating, memory cycles are stolen from the program running - for each data channel transfer of a 16 bit word one memory cycle is stolen. With a 1 microsecond cycle time the maximum total data channel transfer rate is 16.000.000 bits per second. Two CPUs may be connected to one memory block.[3]

### Central Processing Unit

(Main article Detailed description of the NORD-1 CPU)

The central processing unit, CPU, controls the execution of the instructions and the input / output system. The CPU consists of a register block, control flip-flops and an arithmetic and control unit.[3]

### Register block

The register block consists of 8 general registers, 4 bus memory registers and 2 priority interrupt control registers. The CPU registers are 16 bit high speed, integrated circuit registers.

The 8 general registers are

- R-register: Address register. This register is not accessible by program.

- A-register: This is the main register for arithmetic and logical operations directly to the memory. This register is also used for input / output communication.

- D-register: This register is an extension to the A-register in double precision or floating point operations. It may be connected to the A-register during double length shifts.

- T-register: Temporary register. In floating point instructions it is used to hold the exponent part.

- L-register: Link register. The return address after a subroutine jump is contained in this register.

- X-register: Index register. In connection with indirect addressing it causes post-indexing.

- B-register: Base register or second index register. In connection with indirect addressing it causes pre-indexing.

- P-register: Program counter, address of current instruction. This register is controlled automatically in the normal sequencing or brancjhing mode. But it is also fully program controlled and its contents may be transferred to or from other registers.

Besides the R- and P-register all registers are fully program-controlled and may be used for other purposes than those described here.

Two instructions, ROP and SKP, may specify a register whose contents is always zero.

### Control flip-flops

Six control flip-flops are accessible by program. They are

- C: Carry flip-flop.

- Q: Dynamic overflow flip-flop.

- O: Static overflow flip-flop. Remains set after and overflow condition until it is reset by program.

- Z: Floating point overflow flip-flop. This flip-flop is static and remains set untiil it is reset by program.

- K: One bit accumulator.

- M: Multi shift link flip-flop.

These flip-flops are fully program controlled either by means of the BOP instruction or by the TRA or TRR subinstructions where all flip-flops must be transferred to and from the A-register.

### Arithmetic and control units

The address and index computations are performed in a special address arithmetic unit. All programmed arithmetic and logical operations are performed in a 16 bit high-speed arithmetic unit. Therefore, all such operations may be performed on any of the registers.

The control unit contains the necessary logic circuitry to access data and instruction words, to modify instruction addresses, to perform arithmetic and logical operations and to control the interrupt system.

### Control Panel

The control panel consists of the following main parts

- Power push button. to turn power on or off, push the button. The button is lighted red when power is on.

- push buttons

- STOP - when pushed, it will cause the computer to stop after finishing the current operation. The STOP button is lighted when the computer is in STOP mode.

- CONT. - when pushed, the computer will start normal operation, without changing instruction address.

- SINGLE INSTR. - (only when the INTERRUPT button is not lighted) each time it is pushed, the computer executes the next instruction, then goes back into STOP mode.

- SET ADDRESS - operates as a memory examine button. Use the contents of the switch register OPR as the address to examine.

- DEPOSIT - when pushed, it will store the content specified in switch register OPR in location specified by the R-register. A complete memory deposit function requires that first the address is set up in OPR, then SET ADDRESS is pushed, the the content is set in OPR and DEPOSIT is pushed.

- LOAD - will start hardware assembly from the paper tape reader. This function is identical to typing $ on the Teletype typewriter. The LOAD button is lighted when hardware assembly is from the paper tape reader. To change to Teletype hardware assembly, push MASTER CLEAR.

- PROTECT - will turn on the memory protection system when pushed. If the button is pushed again, the protection system is turned off. The button is lighted when the memory protection system is on.

- INTERRUPT - the INTERRUPT button is lighted when the interrupt system is on.

- MASTER CLEAR - sends out the master clear pulse. The MASTER CLEAR button should be pushed after power is switched on. The MASTER CLEAR button should only be pushed when the computer is in STOP mode. MASTER CLEAR also resets the light in the LOAD button to have hardware assembly from the Teletype. Lighted MASTER CLEAR indicates memory inoperative (memory retention option).

- one switch register OPR (16 switches). This register is used for normal DEPOSIT and SET ADDRESS functions. It may also be read with the TRA OPR instruction.

- indicator lamp register (16 lamps) - display the content of one of the main registers in NORD-1, based on the position of the selector switch.

- a selector switch - the position of this switch determines the register to be displayed on the indicator lamp register.

- IR-register

- L-register

- T-register

- D-register

- A-register

- P-register

- R-register

- H-register

- X-register

- B-register

- F-register - consists of

- bit 0 - not used

- bit 1 - not used

- bit 2 - K one bit accumulator

- bit 3 - Z floating point overflow

- bit 4 - Q Dynamic overflow

- bit 5 - O Static overflow

- bit 6 - C Carry

- bit 7 - M Multi shift link flip-flop

- bit 8-11 - PIL, interrupt level operating

- bit 12 - not used

- bit 13 - Paging on (dynamic core allocation)

- bit 14 - Reject interrupt (Save/unsave programs)

- bit 15 - not used

The Teletype typewriter and the paper tape reader may also be considered part of the control panel.

### Backplane

The back plane of NORD 1 #47.

The backplane consists of a sea of wires. There are a number of different versions known. A list of NORD-1 machines gives the versions 1-2, 1-prod, 1-8, 1-14, 1-18, 1-30, 1-36, 1-51, 1-71A of the backplane.

### Remaining machines

The NORD-1 has been unusually well-preserved. At least 142 machines were produced[6], and at the very least 11 machines have been preserved, including serial numbers 2, 4 and 5. This may owe to the fact that the company Norsk Data was already a very large and exceedingly rapidly growing corporation by the time many of these machines were decommissioned.

- NORD-1 snr 2 is the first machine sold. It was installed on M/S Taimyr. Now in the collection of Norsk Maritimt Museum.[7]

- NORD-1 snr 6 is at IDI, NTNU in Gløshaugen, Trondheim.

- NORD-1 snr 19 is in the collection of Trøim, in Telemuseums storage in Fetsund (2016 inventory).

- NORD-1 snr 20 is in the possession of Bo Gøran Kvamme. (Ref : Facebook post in 2020)

- NORD-1 snr 28 was spotted in Tokke power station in 2010 and verified still there in 2016. It is kept for a future planned museum with the preserved old control rooom.

- NORD-1 (probably snr 31) was purchased in bits and built by IDI is still with NTNU in Gløshaugen, Trondheim.

- NORD-1 snr 37 is at the Teknisk Museums exhibition.[8]

- NORD-1 snr 39 is in the collection of Trøim, in Telemuseums storage in Fetsund (2016 inventory).

- NORD-1 snr 47 is preserved in the Umeå collection of Gandalf.

- NORD-1 snr 50 is in the collection of Trøim, in Telemuseums storage in Fetsund (2016 inventory).

- NORD-1 snr 60 is in the collection of Trøim, in Telemuseums storage in Fetsund (2016 inventory).

### Unknown status

- A NORD-1 (probably snr 4 and from Christian Mikkelsens Institutt) in a dual cabinet was saved by NODAF from a barn in Gjerdrum, current status is unknown.

- In a list over the computer collection of NTNU a NORD-1 is listed without any details of the serial number. It probably one of the machines in Trondheim.[9]

### Sources

- ↑ Wikipedia article on 7400 series integrated circuits

- ↑ [bitsavers.trailing-edge.com/pdf/ti/_dataBooks/1971_TI_IntegratedCircuits/ Texas Instruments databooks from 1972]

- ↑ 3.0 3.1 3.2 3.3 3.4 Norsk Data Library, Hardware - NORD-1 Reference Manual, Complete Instruction Repertoire

- ↑ The Norwegian Museum of Science and Technology: Det norske datafyrtårnet (In norwegian)

- ↑ Norsk Data Annual Report 1982, ND Publications, April 6th 1983

- ↑ Computers and Automation August 1974, Monthly computer census

- ↑ Radar equipment with computer in the collection of Norsk Maritimt Museum

- ↑ NODAF homepage

- ↑ List of machines in the NTNU collection, made by Ola Nordal in 2006.

- Wikipedia list of the 7400 serie

- This article was originally a copy of the English Wikipedia article NORD-1 in 19 October 2008.

ROW| 

 | 
  This article is a stub. You can improve NDWiki by expanding it. |

