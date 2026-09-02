# Source: ndwiki article "Detailed description of the NORD-1 CPU"

- **Live page**: https://www.ndwiki.org/wiki/Detailed_description_of_the_NORD-1_CPU
- **Copy used here**: Wayback Machine snapshot of 9 October 2024
  http://web.archive.org/web/20241009135244/https://www.ndwiki.org/wiki/Detailed_description_of_the_NORD-1_CPU
- **Why the archive**: as of 2026-08-27 the live ndwiki is behind an Anubis
  proof-of-work gate; the article and the MediaWiki api.php both refuse scripts.
- **Fetched**: 2026-08-27, by Ronny's request.

**Status: SECONDARY SOURCE (a community wiki), and it labels itself a stub.**
Its real value is the card-level mapping - which register lives on which board,
and which cards read which instruction-register bits. That mapping is not in the
prose manuals; it comes from someone reading the schematics of a real machine.
Check it against ND-01.004.01 HARDWARE MANUAL NORD 1 before treating it as fact.

The text below is the article body converted from the archived HTML. Tables are
flattened to `ROW| a | b |`. Wording and numbers unchanged.

---

The NORD-1
The NORD-1 CPU consists of two racks with 64 cards and over 1300 standard 7400 series TTL IC:s[1]. It is a 16 bit word length design and can address 64 kWord of memory.

ROW| 

 | 
  This article is a stub. You can improve NDWiki by expanding it. | 

It has floating-point instructions as standard.

The CPU operates asynchronous to the memory timing control, and the computer can use memories of different speeds. The fastest memory cycle time which the CPU can efficiently use is 1 microsecond.

### Contents

- 1 Central Processing Unit

- 1.1 Register block

- 1.2 Instruction register

- 1.3 Bus memory

- 1.4 Control flip-flops

- 1.5 Arithmetic and control units

- 2 Control Panel

- 3 Sources

### Central Processing Unit

The central processing unit, CPU, controls the execution of the instructions and the input / output system. The CPU consists of a register block, control flip-flops and an arithmetic and control unit.

### Register block

The register block consists of 8 general registers, 4 bus memory registers and 2 priority interrupt control registers. The CPU registers are 16 bit high speed, integrated circuit registers.

The 8 general registers are

- R-register: Address register. This register is not accessible by program. It is located on the four 108 "Register II" cards.

- A-register: This is the main register for arithmetic and logical operations directly to the memory. This register is also used for input / output communication. It is located on the four 102 "Register I" cards.

- D-register: This register is an extension to the A-register in double precision or floating point operations. It may be connected to the A-register during double length shifts. It is located on the four 102 "Register I" cards.

- T-register: Temporary register. In floating point instructions it is used to hold the exponent part. It is located on the four 102 "Register I" cards.

- L-register: Link register. The return address after a subroutine jump is contained in this register. It is located on the four 102 "Register I" cards.

- X-register: Index register. In connection with indirect addressing it causes post-indexing. It is located on the four 108 "Register II" cards.

- B-register: Base register or second index register. In connection with indirect addressing it causes pre-indexing. It is located on the four 108 "Register II" cards.

- P-register: Program counter, address of current instruction. This register is controlled automatically in the normal sequencing or branching mode. But it is also fully program controlled and its contents may be transferred to or from other registers. It is located on the four 108 "Register II" cards.

Besides the R- and P-register all registers are fully program-controlled and may be used for other purposes than those described here.

### Instruction register

There is an IR register on 150 "Register". This holds the current instruction during execution. Some of the bits are used on other cards, for example IR8, IR9 and IR10 is used on 130 "Addressing control" to decide the mode (,X ,I ,B).

Cards directly accessing the IR

- 109 "Register control" Bit 15, 14, 13, 12, 11, 10, 9, 8, 5, 4, 3, 2, 1, 0

- 115 "Arithmetic control" Bit 15, 14, 13, 12, 11, 10, 9, 8

- 120 "I/O control" Bit 15, 14, 13, 12, 11

- 123 "Cycle counter" Bit 15, 14, 13, 12, 11, 9

- 124 "Counter control" Bit 10, 9, 8

- 125 "Shift counter" Bit 15, 14, 13, 12, 11

- 126 "Shift control" Bit 10, 9, 8, 7, 5

- 127 "Control flip-flops" Bit 11, 10

- 128 "Protection check" Bit 15, 14, 13, 12, 11, 10, 9, 8, 7, 6

- 130 "Addressing control" Bit 15, 14, 10, 9, 8

- 131 "Addressing control" Bit 15, 14, 13, 12, 11

- 132 "Interrupt control" Bit 6

- 133 "Bit instruction" Bit 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0

- 134 "Floating control" Bit 12, 11

- 135 "Floating control" Bit 11

- 136 "Floating control" Bit 11

- 137 "Floating control" Bit 11

- 140 "Lamp register" Bit 15-0

- 146 "Register transfer" Bit 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0

- 157 "Teletype control" Bit 0

- 163 "I/O output and clock" Bit 10, 9, 8, 5, 4, 3, 2, 1, 0

### Bus memory

There is a bus memory register on * 101 "Bus memory".

### Control flip-flops

Six control flip-flops are accessible by program. They are

- C: Carry flip-flop.

- Q: Dynamic overflow flip-flop.

- O: Static overflow flip-flop. Remains set after and overflow condition until it is reset by program.

- Z: Floating point overflow flip-flop. This flip-flop is static and remains set until it is reset by program.

- K: One bit accumulator.

- M: Multi shift link flip-flop.

These flip-flops are fully program controlled either by means of the BOP instruction or by the TRA or TRR sub-instructions where all flip-flops must be transferred to and from the A-register.

C, Q, O and Z is located on 127 "Control flip-flops". M is located on 126 "Shift control".

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

### Sources

- ↑ Wikipedia article on 7400 series integrated circuits

- NORD-1 HARDWARE MANUAL

- NORD-1 REFERENCE MANUAL

