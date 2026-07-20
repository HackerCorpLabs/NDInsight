## Page 1

# ND 073 BCD Arithmetic for ND-500

**A special Hardware Module for Use in Commercial Data Processing on the ND-500/CE Series of Computers.**

In order to increase the speed of COBOL programs on the ND-500, a special BCD-processor is provided as an extension of the ND-500 CPU.

The processor performs arithmetic on Binary Coded Decimal (BCD) numbers, and converts data fields between BCD-format and ASCII-format, and between BCD – and binary (integer) format.

This new processor can handle automatic scaling on all operands and multiplication.

The following instructions are implemented:

## BCD Instructions

- **PADD**: Add packed decimal numbers with automatic scaling.
- **PADDR**: Same as PADD, rounding the result.
- **PSUB**: Subtract Packed decimal numbers with automatic scaling.
- **PSUBR**: Same as PSUB, rounding the result.
- **PCOMP**: Compare packed decimal numbers.
- **PMPY**: Multiply packed decimal numbers with automatic scaling.
- **PMPYR**: Same as PMPY, rounding the result.
- **PSHIFT**: Shift packed decimal number with automatic scaling.
- **PSHIFTR**: Same as PSHIFT with rounding.
- **PPACK**: Convert from ASCII coded decimal number to packed decimal number with automatic scaling.
- **PPACKR**: Same as PPACK, rounding the result.
- **PUPACK**: Convert from packed decimal number to ASCII coded decimal number with automatic scaling.
- **PUPACKR**: Same as PUPACK, rounding the result.
- **Wn WPCONV**: Convert from ordinary binary to packed decimal numbers.
- **Wn PWCONV**: Convert from packed decimal numbers to ordinary binary.

The instructions operate on packed decimal numbers with from 1 to 31 decimal digits. The decimal point ranges from -32 to 31 counted from the least significant digit.

## What is a Packed Decimal Number (BCD-number)

Each decimal digit is represented by four bits. Therefore two digits can be placed adjacent in a byte. In the right-most byte of the the string of bytes representing your number, the last four bits are used to keep the sign. Each decimal digit is encoded as follows:

| Digit | Bit Pattern | Hexa-decimal |
|-------|-------------|--------------|
| 0     | 0000        | 0            |
| 1     | 0001        | 1            |
| 2     | 0010        | 2            |
| 3     | 0011        | 3            |
| 4     | 0100        | 4            |
| 5     | 0101        | 5            |
| 6     | 0110        | 6            |
| 7     | 0111        | 7            |
| 8     | 1000        | 8            |
| 9     | 1001        | 9            |
| +     | 1010        | A            |
| -     | 1011        | B            |
| +     | 1100        | C            |
| -     | 1101        | D            |
| +     | 1110        | E            |
| unsigned | 1111    | F            |

Every number will fill an integral number of bytes right justified. If the leftmost 4 bits are not needed, they will always contain a zero.

---

## Page 2

# What is an ASCII Coded Decimal Number

In the ASCII format each decimal digit occupies one byte (8 bits). The four high-order bits of a byte are called the zone. The four low-order bits are encoded in the same way for the packed decimal digits.

A decimal number in this format may have four different sign representations:

1. Embedded trailing sign.  
   The right-most byte occupies both the least significant digit and the sign.

2. Separate trailing sign.  
   The right-most byte occupies the ASCII-coded sign only.

3. Embedded leading sign.  
   The left-most byte contains both the most significant digit and the sign.

4. Separate leading sign.  
   The left-most byte occupies the ASCII-coded sign only.

When a sign is embedded the following codes are used:

|        | 0          | 1-9         |
|--------|------------|-------------|
| positive | 173B      | 101B-111B   |
| negative | 175B      | 112B-122B   |

```
    _     _                 
  _| |_ _| |_               
 |_   _|_   _|              
   |_|   |_|                
```

```
   _   _    _                    
  | | | |  (_)                   
  | |_| | ___  ___ ___  ___  ___ 
  |  _  |/ _ \/ __/ __|/ _ \/ __|
  | | | |  __/\__ \__ \  __/\__ \
  \_| |_/\___||___/___/\___||___/
                                
```

```
      ____  ____                 
     / ___||  _ \                
    | |    | |_) |               
    | |___ |  __/                
     \____||_|                   
```

NOTE: NORSK DATA reserves the right to change specifications without notice.

---

