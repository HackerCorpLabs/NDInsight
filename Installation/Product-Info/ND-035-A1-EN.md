## Page 1

# ND 035 Extended Instruction Set, 48 bit
# ND 039 Extended Instruction Set, 32 bit

## COMMERCIAL INSTRUCTION SET ON THE ND-100/CE

In order to increase the speed of COBOL- and PLANC-programs the following new machine instructions are implemented in the micro-program of the ND-100/CE computer:

### Packed Decimal (BCD) Instructions

| Instruction  | Code     | Description                                      |
|--------------|----------|--------------------------------------------------|
| ADDD         | 140120B  | Add packed decimal numbers.                      |
| SUBD         | 140121B  | Subtract packed decimal numbers.                 |
| COMD         | 140122B  | Compare packed decimal numbers.                  |
| PACK         | 140124B  | Convert from ASCII unpacked to packed decimal number. |
| UNPACK       | 140125B  | Convert from packed to ASCII unpacked decimal number. |
| SHDE         | 140126B  | Shift packed decimal number, with rounding if wanted. |

### Byte Instructions

| Instruction | Code     | Description                                           |
|-------------|----------|-------------------------------------------------------|
| BFILL       | 140130B  | Fill one specified byte into a string of bytes.       |
| MOVEB       | 140131B  | Move one string of bytes to another one, overlap not allowed. |
| MOVBF       | 140132B  | Move one string of bytes to another one, overlap allowed. |

### Call and Return Instructions

| Instruction | Code     | Description                                          |
|-------------|----------|------------------------------------------------------|
| INIT        | 140134B  | Initialize the stack.                                |
| ENTR        | 140135B  | Save the link address and establish stack entry according to the stack demand. Update the stack pointer and maximum stack address. |
| LEAV        | 140136B  | Return to the link address. Reset the stack pointer and maximum stack address. |
| LEAVE       | 140137B  | Same as LEAV, but can handle error-return.           |

### What is a Packed Decimal Number (BCD-numbers)

Each decimal digit is represented by four bits. Therefore two digits can be placed adjacent in a byte. In the right-most byte of the string of bytes representing your number the last four bits are used to keep the sign. Each decimal digit is encoded as follows:

| Digit  | Bit Pattern | Hexa-decimal   |
|--------|-------------|----------------|
| 0      | 0000        | 0              |
| 1      | 0001        | 1              |
| 2      | 0010        | 2              |
| 3      | 0011        | 3              |
| 4      | 0100        | 4              |
| 5      | 0101        | 5              |
| 6      | 0110        | 6              |
| 7      | 0111        | 7              |
| 8      | 1000        | 8              |
| 9      | 1001        | 9              |
| +      | 1010        | A (accepted)   |
| +      | 1011        | B (accepted)   |
| −      | 1100        | C              |
| −      | 1101        | D              |
| +      | 1110        | E (accepted)   |
| unsigned | 1111      | F              |

Every number will fill an integral number of bytes right justified. If the leftmost 4 bits are not needed, they will always contain a zero.

### What is an ASCII coded Decimal Number

In the ASCII format each decimal digit occupies one byte (8 bits). The four high-order bits of a byte are called the zone. The four low-order bits are encoded in the same way as for the packed decimal digits. The most significant bit is the parity bit. This bit is neither tested nor set in the instructions.

---

## Page 2

# Sign Representations in Decimal Numbers

A decimal number in this format may have four different sign representations:

1. **Embedded trailing sign.**  
   The right-most byte occupies both the least significant digit and the sign.

2. **Separate trailing sign.**  
   The right-most byte occupies the ASCII-coded sign only.

3. **Embedded leading sign.**  
   The left-most byte contains both the most significant digit and the sign.

4. **Separate leading sign.**  
   The left-most byte occupies the ASCII-coded sign only.

When a sign is embedded the following codes are used:

|        |         |             |
|--------|---------|-------------|
| **Positive** | 0       | = 173B      |
|        | 1-9     | = 101B-111B |
| **Negative** | 0       | = 175B      |
|        | 1-9     | = 112B-122B |

## Contact Information

```
     Bergen, tel. 05-220299                Trondheim, tel. 075-16520, tix. 55550 comte n
     Sandnes, tel. 044-52554               Stockholm (Upplands Väsby), tel. 08-590950, tix. 13528 nordata s
     Tromsø, tel. 083-511460               Stockholm (Solna), tel. 08-252525, tix. 13750 wexcom s
     Gothenburg, tel. 031-870860           Odense, tel. 09-17547, tix. 58520 comtec dk
     Gotha, tel. 036-2015950               Ballerup (Copenhagen), tel. 02-456700
     Malmö, tel. 040-255055                Düsseldorf, tel. 0211-0638838, tix. 858727 comtd d
     Copenhagen, tel. 02-475851, tix. 37725 nd dk
     Wiesbaden, tel. 0611-7451, tix. 417370 noda
     Ferryl-Yorkdale, tel. 0450-458125, tix. 38553 nordata ferry
     Paris, tel. 01-492357, tix. 30110 nd paris     
     Lyon, tel. 07-873417        
     Newbury (Berkshire?), tel. 0635-31465, tix. 849819 norskg d
     Boston, tel. 631-227-7945, tix. 921750 norsk vel
```

## Company Information

```
        Jérikveien 20 
        Boks 4 Linderberg gård 
        Oslo 10 
        Tel.: 02-390030 
        Tlx.: 18664 nd n
```

```
        Jørikveien 20
        Boks 4 Linderberg gård
        Oslo 10
        Tel.: 02-390030
        Tlx.: 18664 nd n
```

**NOTE: NORSK DATA reserves the right to change specifications without given notice!**

---

