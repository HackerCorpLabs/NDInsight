## Page 1

# ND-500 Single Prec. Array Proc. Func.

ND-805013.3 EN

ND  
Norsk Data  

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 2

I'm unable to extract any content from this page as it appears to be mostly blank or very faint.

---

## Page 3

# ND-500 Single Prec.
## Array Proc. Func.

*ND-805013.3 EN*

--- 

*Scanned by Jonny Oddene for Sintran Data © 2011*

---

## Page 4

NOTE:  
The numbering system for Norsk Data's documentation changed in September 1988. All numbers now start with an 8. The numbering structure is therefore ND-8xxxxx.xx xx. Example: ND-863018.3A EN. Existing manuals will receive a new number if and when they are updated or revised.  

The information in this manual is subject to change without notice.  
Norsk Data A.S assumes no responsibility for any errors that may appear in this manual, or for the use or reliability of its software on equipment that is not furnished or supported by Norsk Data A.S.  

Copyright 1989 by Norsk Data A.S  

Send all documentation requests to:  
Norsk Data A.S  
Publication Department  
P.O. Box 25 - Bogerud  
N-0621 Oslo 6  
NORWAY

---

## Page 5

# Preface

## The Product

This manual describes the mathematical functions in the ND-500 APF library.

The APF library routines may be called from FORTRAN. The routines are designed to speed up the execution of operations on single precision real arrays.

The APF library utilizes a special microprogram for the ND-500 CPU. This microprogram is an extension of the standard microprogram.

The ND-numbers for this product on the different ND-500 models are:

| Computer      | ND-number | Microprogram version |
|---------------|-----------|----------------------|
| ND-500/1 series | ND-10338  | 104xx                |
| ------ " ----- | ND-10412  | 106xx                |
| ND-500/2 series | ND-10701  | 152xx                |
| ------ " ----- | ND-10786  | 152xx                |
| ND-570/2 serie | ND-10700  | 150xx                |

**NOTE** The ND-530/2 can not use this product.

## The Manual

This manual provides a functional description of the APF library, and thereby the special array processing functions. A listing of each routine is used to describe the routines. Listing and parameter description is mainly in FORTRAN. Two of the routines are described in ND-500 assembler, although they are callable from FORTRAN.

## Changes from Previous Version

The description of the routines is revised to make this manual consistent with the related manual ND-500/2 Double Precision Array Processing Functions, ND-05.018. Small changes is done in the structure. Some documentation errors are corrected.

## The Reader

This manual is written for people creating and running programs using array processing functions.

---

ND-05.013.03

---

## Page 6

# Prerequisite Knowledge

The reader is assumed to be familiar with FORTRAN and ought to have some knowledge of the ND-500 assembler. It is also assumed that the reader is familiar with using ND-500 computers, and knows how to generate, load and run programs on such systems.

# Related Manuals

Documentation further describing the use of ND-500 computers is found in these manuals:

| Manual                                              | Code     |
|-----------------------------------------------------|----------|
| ND-500 Loader/Monitor                                | ND.60.136 |
| ND FORTRAN Reference Manual                          | ND.60.145 |
| ND-500 Reference manual                              | ND.05.009 |
| ND-500/2 Double Precision Array Processing Functions | ND.05.018 |

ND-05.013.03

---

## Page 7

# TABLE OF CONTENTS

| Section | Page |
|---------|------|
| 1 BASIC CONCEPTS | 1 |
| 1.1 Prerequisite Software for Using the APF Library | 1 |
| 1.2 The Single Precision Floating Point Format | 1 |
| 1.3 Hardware Concepts | 1 |
| 1.4 Array Processing Definitions | 2 |

| 2 USING THE ARRAY PROCESSING FUNCTIONS | 5 |

| 3 ND-500 ARRAY PROCESSING FUNCTIONS PERFORMANCE | 11 |
| 3.1 ND-560/1 Processing Performance | 11 |
| 3.2 ND-570/2 Processing Performance | 13 |

| 4 ARRAY PROCESSING FUNCTIONS | 15 |
| 4.1 Introduction | 15 |
| 4.2 Vector Add (VADDXXX) | 16 |
| 4.3 Vector Subtract (VSUBXXX) | 17 |
| 4.4 Vector Multiply (VMULXXX) | 18 |
| 4.5 Vector Divide (VDIVXXX) | 19 |
| 4.6 Vector Maximum (VMAXXXX) | 20 |
| 4.7 Vector Minimum (VMINXXX) | 21 |
| 4.8 Vector Maximum Magnitude (VMAXMGX) | 22 |
| 4.9 Vector Minimum Magnitude (VMINMGX) | 23 |
| 4.10 Vector Square (VSQXXXX) | 24 |
| 4.11 Vector Signed Square (VSSQXXX) | 25 |
| 4.12 Vector Absolute Value (VABSXXX) | 26 |
| 4.13 Vector Square Root (VSQRTXX) | 27 |
| 4.14 Vector Sine (VSINXXX) | 28 |
| 4.15 Vector Cosine (VCOSXXX) | 29 |
| 4.16 Vector Move (VMOVXXX) | 30 |
| 4.17 Vector Swap (VSWAPXX) | 31 |
| 4.18 Vector Negative (VNEGXXX) | 32 |
| 4.19 ND to IBM Floating Point Convert (NDFPCV) | 33 |
| 4.20 IBM to ND Floating Point Convert (IBMFPCV) | 35 |
| 4.21 Sum of Vector Elements (SVEXXXX) | 37 |
| 4.22 Sum of Vector Elements Magnitude (SVEMGXX) | 38 |
| 4.23 Sum of Vector Elements Square (SVESQXX) | 39 |
| 4.24 Sum of Vector Elements Signed Square (SVSXXXX) | 40 |
| 4.25 Vector Average Absolute Value (VAVGABS) | 41 |
| 4.26 Maximum Value in Vector (MAXVXXX) | 42 |
| 4.27 Minimum Value in Vector (MINVXXX) | 43 |

---

## Page 8

# Table of Contents

| Section | Page |
| ------- | ---- |
| 4.28 | Maximum Magnitude Element in Vector (MAXMGVX) | 44 |
| 4.29 | Minimum Magnitude Element in Vector (MINMGVX) | 45 |
| 4.30 | Maximum and Minimum Value in Vector (MAXMINX) | 46 |
| 4.31 | Maximum and Minimum Magnitude Element in Vector (MXMNMGX) | 47 |
| 4.32 | Vector Scalar Add (VSADDXX) | 48 |
| 4.33 | Vector Scalar Multiply (VSMULXX) | 49 |
| 4.34 | Vector Scalar Divide (VDIVSXX) | 50 |
| 4.35 | Dot Product (DOTPRXX) | 51 |
| 4.36 | Vector Clear (VCLRXXX) | 52 |
| 4.37 | Convolution (CONVXXX) | 53 |
| 4.38 | Complex Vector Multiply (CVMULXX) | 55 |
| 4.39 | Complex Fast Fourier Transform (CFFTXXX) | 56 |
| 4.40 | Real Fast Fourier Transform (RFFTXXX) | 61 |
| 4.41 | Vector Taper (VTAPERX) | 65 |
| 4.42 | Wiener Filter (WIENERX) | 66 |
| 4.43 | Vector Generate (VGENXXX) | 68 |
| 4.44 | Vector Linear Interpolation (VNMOXXX) | 69 |
| 4.45 | Vector Sinc Interpolation (VNMOSXX) | 71 |
| 4.46 | Vector Expand (VXPNDXX) | 73 |
| 4.47 | Vector First and Last Non-Zero Value (VFLNZXX) | 74 |
| 4.48 | Vector Scalar Multiply and Add (VSMADDX) | 75 |
| 4.49 | Predict (PREDICT) | 76 |
| 4.50 | Display Processor Function (XBTMUX) | 77 |
| 4.51 | Image Build (IMGBLD) | 78 |
| 4.52 | Convert and Move (APMOVE) | 80 |
| 4.53 | Demultiplex (DMUXB) | 82 |
| 5 | FAST BYTE MOVE | 85 |
| | Index | 87 |

---

Scanned by Jonny Oddene for Sintran Data © 2011

ND-05.013.03

---

## Page 9

# ND-500 Single Precision Array Processing Functions

## 1 Basic Concepts

### 1.1 Prerequisite Software for Using the APF Library

Together with the microprogram, the APF library must be installed. The actual file name is: `ND-500-APF-LIB:NRF`.

These are the only modifications to be performed on the system.

The APF library utilizes a special microprogram for the ND-500 CPU. This microprogram contains the array processing functions. It is an extension of the standard ND-500 instruction set.

The array processing functions are floating point operations performed in 32 bits floating point format by the ND-500 floating point arithmetic.

### 1.2 The Single Precision Floating Point Format

The number range for the single precision floating point format is:

\[ 8.6 \times 10^{-78} \leq |N| \leq 5.8 \times 10^{76} \]

The accuracy corresponds approximately to 7 decimal digits.

### 1.3 Hardware Concepts

The ND-500 CPU gives the possibility of parallel processing. This means that indexing, memory access, floating point arithmetic, integer arithmetic and loop control may be run in parallel. This is done as far as possible to obtain high speed operations. Temporary results to be used in later calculations, are kept in registers in the ND-500 CPU, accessed directly by both the floating point arithmetic and integer arithmetic.

Arrays involved in an operation are accessed through the ND-500 memory management system. This system will cause automatic allocation of memory, and reservation of continuous memory space for array processing is not required. The result of an array processing function is present in the output array when returning from the function.

The ND-500 array processing functions are fully interruptable to maintain the ND-500 CPU resources being shared by the different processes currently running on the system.

---

## Page 10

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## BASIC CONCEPTS

### 1.4 ARRAY PROCESSING DEFINITIONS

An array contains a group of numbers that are related to each other in some way, and the array may be multi-dimensional. An array is termed a matrix in mathematical terminology.

An array is used to represent equations of different kinds, for example, linear equations. Each row in the array represents one particular equation, thus the array represents a system of equations of the same kind.

The entries in the array are the coefficients of the equations. They are called elements in this manual.

Each row of elements is named a vector in this manual, regardless of what kind of equation it mathematically represents. In those array processing functions which are closely related to mathematical vectors, the row of elements is referred to as a complex vector.

Most of the array processing functions are performed on one-dimensional vectors.

Three parameters are necessary to specify a vector:

| Parameter | Description |
|-----------|-------------|
| V   | The logical name of the vector. A single precision real. |
| INC | The step value (increment) for the index in the array. An integer. |
| NN  | Element count. Number of elements in the array. An integer. |

**NOTE:** It is assumed that the lower limit for the array index is 1.

---

*ND-05.013.03*

_Scanned by Jonny Oddene for Sintran Data © 2011_

---

## Page 11

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## BASIC CONCEPTS

### Example of Use of a Function

    PROGRAM CLEAR

    REAL VA(2000)
    INTEGER INCA,NN;

    <Other program statements>

    INCA = 2
    NN = 1000

    CALL VCLRXXX(VA,INCA,NN)

    <Other program statements>

    END

This function causes each second element of the vector VA to be set to zero (increment is 2).

For vectors where the elements are stored in consecutive locations, the index increment is equal to 1. The flexibility to specify index increments is present to most of the functions.

For complex vectors, each complex equation is represented by two consecutive elements. This corresponds to the real and the imaginary part of the complex vector. The real element is immediately followed by the imaginary element, as the complex vector is represented in the rectangular coordinate system.

This means that for each index in the array there are one real and one imaginary element.

---

## Page 12

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS  
## BASIC CONCEPTS  

### Example of Use of a Complex Function:

```
PROGRAM CVMULTIPLY   

COMPLEX VA(1:100)  
COMPLEX VB(1:100)  
COMPLEX VC(1:100)  

<Other program statements>  

CALL CVMULXX(VA,2,VB,2,VC,1,50,1)  

<Other program statements>  

END  
```

This function causes each second complex vector in array VA to be multiplied with each second vector from VB. The results are stored in consecutive locations in VC. This function corresponds to mathematical multiplication of complex numbers.  

ND-05.013.03  

*Scanned by Jonny Oddene for Sintran Data © 2011*

---

## Page 13

# ND-500 Single Precision Array Processing Functions
## Using the Array Processing Functions

The array processing functions can be called from FORTRAN. The array processing library is used to transfer the parameters from the call to the array processing instructions. Thus the array processing functions are linked to the main program at load time as a part of the program.

Writing a FORTRAN program for array processing with ND-500 array processing functions is much the same as using the FORTRAN equivalent for the array processing function. Before starting an array processing function, input and output arrays for the operations must be defined. Initialization of input arrays is also required. This means that data for processing must be placed in the input arrays for the actual array processing function. Then the array processing function may be called. The result of the operation is present in the output array when returning from an array processing function.

Calling the function APMOVE is a bit different from the other array processing functions, because a function, named LOCARG, must be called before APMOVE. See also page 80.

An example of calling array processing functions is given below. Detailed layout of the different array processing functions is given in chapter 5.

## Example of Creating a FORTRAN Program Using Array Processing Functions

### Source Program in FORTRAN:

| PROGRAM DOKKT                                              |
|:----------------------------------------------------------|
| C  Set name and size of arrays to be used.               |
| DIMENSION VA(100), VB(100), VC(100)                      |
| INTEGER*2 I2(100)                                        |
| INTEGER*4 I4(100)                                        |
| C  Initiate source array.                                |
| I4(1) = 1                                                |
| I4(2) = 10                                               |
| I4(3) = 100                                              |
| I4(4) = 100                                              |
| C  Get pointer to array I4.                              |
| IADDA = LOCARG(I4)                                       |
| C  Get pointer to array VC.                              |
| IADDD = LOCARG(VC)                                       |
| C  Set type of conversion with APMOVE and element count. |
| IFTM = 0                                                 |
| NN = 4                                                   |
| C  Convert 32 bit integer to single floating point and move.|
| CALL APMOVE(IADDA, IADDD, IFTM, NN, *100)                 |
| 100 CONTINUE                                             |
| \[The program continues on next page...\]                |

---

## Page 14

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS
## USING THE ARRAY PROCESSING FUNCTIONS

...Continued from previous page

```
C   Set index increment and element count.
    INCC = 1
    NN = 100
C   Clear the result array to be used in the next operation.
    CALL VCLRXXX(VA,INCC,NN,*101)
101 CONTINUE
    NN = 4
C   Expand array VC with result in array VA. NC is returned.
    CALL VXPNDXX(VC,VA,NN,NC,*102)
102 CONTINUE
C   Set scalar value, index increments and element count.
    B = 200.0
    INCA = 1
    INCC = 1
    NN = NC
C   200.0 divided with each element of VA. Result in VB.
    CALL VDIVSXX(VA,INCA,B,VB,INCC,NN,*103)
103 CONTINUE
C   Set index increments and element count.
    INCA = 2
    INCB = 4
    INCC = 1
    NN = 25
C   Add each second of VA to each fourth of VB. Result in VC.
    CALL VADXXX(VA,INCA,VB,INCB,VC,INCC,NN,*104)
104 CONTINUE
    IADDD = LOCARG(I2)
    IADDA = LOCARG(VC)
    IFTM = 4
C   Convert array VC to 16 bit integer. Result in I2.
    CALL AMPOVE(IADDA,IADDD,IFTM,NN,*105)
105 CONTINUE
    IADDD = LOCARG(I4)
    IFTM = 3
C   Convert array VC to 32 bit integer. Result in I4.
    CALL AMPOVE(IADDA,IADDD,IFTM,NN,*106)
106 CONTINUE
C   Result of the operations in arrays VC, I4 and I2.
    WRITE (1,1000)
    DO FOR I=1,25
        WRITE (1,1001)VC(I),I4(I),I2(I)
    ENDDO
1000 FORMAT (' RESULT OF OPERATION IS :',/, 
             ' ......... REAL ........ INTEGER*4 INTEGER*2 .')
1001 FORMAT (3X,G12.7,5X,I11,5X,I6)
    END
```

---

**ND-05.013.03**

_Scanned by Jonny Oddene for Sintran Data © 2011_

---

## Page 15

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS
## USING THE ARRAY PROCESSING FUNCTIONS

### Compiling the Program:

```
@FORTRAN-500

ND-500 ANSI 77, FORTRAN COMPILER - 203054H  
FTN: COMPILE

SOURCE-FILE DOKKT

LIST-FILE

OBJECT-FILE 'DOKKT'

- CPU TIME USED: 1.0 SECONDS. 67 LINES COMPILED.  
- NO MESSAGES  
- PROGRAM SIZE=401 DATA SIZE=2188 COMMON SIZE=0

FTN: EXIT
```

---

## Page 16

# Loading the Program

| Command                          | Details                   |
|----------------------------------|---------------------------|
| @LINKAGE-LOADER ↵                |                           |
| ND-Linkage-Loader - C            | 22. January 1982 Time: 15:5 |
| N1l: SET-DOMAIN DOKKT ↵         |                           |
| N1l: LOAD DOKKT, ND-500-APF-LIB ↵ |                           |
| PROGRAM: .....555 P              | DATA: ..........4144 D    |
| ND-500-APF-LIB                   |                           |
| PROGRAM: .....1004 P             | DATA: ..........4500 D    |
| N1L: CLOSE Y ↵                  |                           |
| Segment no. .....30              | is linked                 |

22. January 1982 Time: 15:5

Unsatisfied references:

None!

---

Defined symbols:

| Symbol     | Code  |
|------------|-------|
| DOKKT      | ...4 P01  |
| VADDXXX    | ...555 P01|
| VDIVSXX    | ...616 P01|
| VCLRKXX    | ...653 P01|
| VXPNDXX    | ...704 P01|
| APMOVE     | ...732 P01|
| LOCARG     | ...774 P01|

Program: ......1004 P  
Data: ..........6500 D  

N1l: EXIT ↵

---

ND-05.013.03

*Scanned by Jonny Oddene for Sintran Data © 2011*

---

## Page 17

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## USING THE ARRAY PROCESSING FUNCTIONS

### Executing the Program:

**N500: DOKKT** 

RESULT OF OPERATION IS:

| REAL     | INTEGER*4 | INTEGER*2 |
|----------|-----------|-----------|
| 30.00000 | 30        | 30        |
| 26.48485 | 26        | 26        |
| 25.21531 | 25        | 25        |
| 25.01976 | 25        | 25        |
| 25.42088 | 25        | 25        |
| 26.18768 | 26        | 26        |
| 27.19480 | 27        | 27        |
| 28.36829 | 28        | 28        |
| 29.66173 | 29        | 29        |
| 31.04448 | 31        | 31        |
| 32.49554 | 32        | 32        |
| 34.00000 | 34        | 34        |
| 35.54700 | 35        | 35        |
| 37.12843 | 37        | 37        |
| 38.73813 | 38        | 38        |
| 40.37133 | 40        | 40        |
| 42.02425 | 42        | 42        |
| 43.69392 | 43        | 43        |
| 45.37788 | 45        | 45        |
| 47.07420 | 47        | 47        |
| 48.78122 | 48        | 48        |
| 50.49760 | 50        | 50        |
| 52.22221 | 52        | 52        |
| 53.95409 | 53        | 53        |
| 55.69241 | 55        | 55        |

**N500: EXIT** 

---

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 18

# ND-500 Single Precision Array Processing Functions

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 19

# ND-500 Single Precision Array Processing Functions

## ND-500 Array Processing Functions Performance

### 3 ND-500 Array Processing Functions Performance

#### 3.1 ND-560/1 Processing Performance

| NAME   | OPERATION                                                           | Number of elements | Typical execution time pr. loop (microsec.) | Improvement ratio to FTN function¹ |
|--------|---------------------------------------------------------------------|-------------------|---------------------------------------------|------------------------------------|
| CFFTXXX  | COMPLEX FAST FOURIER TRANSFORM                                     | 1024              | 3.80                                        | 62.25                              |
| CONVXXX  | CONVOLUTION (CORRELATION)                                          | 150000            | 8.84                                        | 1.29                               |
| CVMULXX  | COMPLEX VECTOR MULTIPLY                                            | 1500              | 3.20                                        | 8.55                               |
| DOTPRXX  | DOT PRODUCT                                                        | 1500              | 3.73                                        | 2.57                               |
| IBMFPCV  | IBM TO ND FLOATING POINT CONVERT                                   | 1500              | 8.00                                        | 4.86                               |
| IMGBLD   | IMAGE BUILD                                                        | 1500              | 4.53                                        | 2.25                               |
| MAXMGVV  | MAX. MAGNITUDE ELEMENT IN VECTOR                                   | 1500              | 5.43                                        | 1.54                               |
| MAXMINX  | MAX. AND MIN. VALUE IN VECTOR                                      | 1500              | 5.50                                        | 2.38                               |
| MAXVXXX  | MAX. VALUE IN VECTOR                                               | 1500              | 5.50                                        | 1.34                               |
| MINMGVX  | MIN. MAGNITUDE ELEMENT IN VECTOR                                   | 1500              | 5.47                                        | 1.61                               |
| MINVXXX  | MIN. VALUE IN VECTOR                                               | 1500              | 5.41                                        | 1.56                               |
| MKMMGVX  | MAX. AND MIN. MAG. ELEMENT IN VECTOR                               | 1500              | 4.54                                        | 2.62                               |
| ND2FPCV  | ND TO IBM FLOATING POINT CONVERT                                   | 1500              | 10.04                                       | 2.00                               |
| PREDICT  | PREDICT                                                            | 1500              | 4.82                                        | 3.85                               |
| RFFTXXX  | REAL FAST FOURIER TRANSFORM                                        | 1024              | 2.40                                        | 58.60                              |
| SVEGMGX  | SUM OF VECTOR ELEMENTS MAGNITUDE                                   | 1500              | 5.70                                        | 1.24                               |
| SVESQGX  | SUM OF VECTOR ELEMENTS SQUARE                                      | 1500              | 4.20                                        | 1.93                               |
| SVEXXXX  | SUM OF VECTOR ELEMENTS                                             | 1500              | 5.40                                        | 1.24                               |
| SVSSXXX  | SUM OF VECTOR ELEMENTS SIGNED SQUARE                               | 1500              | 5.65                                        | 1.93                               |
| VABSXXX  | VECTOR ABSOLUTE VALUE                                              | 1500              | 6.48                                        | 1.27                               |
| VADDXXX  | VECTOR ADD                                                         | 1500              | 6.00                                        | 1.39                               |
| VAVGABS  | VECTOR AVERAGE ABSOLUTE VALUE                                      | 1500              | 4.00                                        | 1.65                               |
| VCLRXXX  | VECTOR CLEAR                                                       | 1500              | 6.34                                        | 0.80                               |
| VCOSXXX  | VECTOR COSINE                                                      | 1500              | 1.40                                        | 15.80                              |
| VDIVSSX  | VECTOR SCALAR DIVIDE                                               | 1500              | 3.77                                        | 2.90                               |
| VDIVXXX  | VECTOR DIVIDE                                                      | 1500              | 4.09                                        | 3.19                               |
| VFLNXXX  | VECTOR FIRST AND LAST NON-ZERO VALUE                               | 1500              | 8.47                                        | 0.61                               |
| VGENXXX  | VECTOR GENERATE                                                    | 1500              | 7.44                                        | 1.29                               |
| VMAXMGX  | VECTOR MAXIMUM MAGNITUDE                                           | 1500              | 6.00                                        | 2.28                               |
| VMAXXXX  | VECTOR MAXIMUM                                                     | 1500              | 6.32                                        | 2.07                               |
| VMINMGX  | VECTOR MINIMUM MAGNITUDE                                           | 1500              | 6.00                                        | 2.28                               |
| VMINXXX  | VECTOR MINIMUM                                                     | 1500              | 6.32                                        | 2.07                               |
| VMOVXXX  | VECTOR MOVE                                                        | 1500              | 6.39                                        | 1.61                               |
| VMULXXX  | VECTOR MULTIPLY                                                    | 1500              | 5.93                                        | 1.92                               |
| VNEGXXX  | VECTOR NEGATIVE                                                    | 1500              | 7.40                                        | 1.16                               |
| VNMOSXX  | VECTOR SINC INTERPOLATION                                          | 1500              | 3.88                                        | 21.68                              |

¹ (Time used by machine code) / (Time used by microcode).  
² ASM - ND-500 assembler.  
ND-05.013.03  
Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 20

# ND-500 Single Precision Array Processing Functions

## ND-500 Array Processing Functions Performance

### Typical execution time pr. loop (microsec.)

#### Improvement ratio to FTN function

| NAME   | OPERATION                          | Number of elements |   |   |
|--------|------------------------------------|--------------------|---|---|
|        |                                    |                    |   |   |
|        |                                    | 1500               |   |   |
| VNM0XXX | VECTOR LINEAR INTERPOLATION        | 1500               | 3.12 | 8.16 |
| VSADDXX | VECTOR SCALAR ADD                  | 1500               | 5.67 | 1.64 |
| VSINXXX | VECTOR SINE                        | 1500               | 1.40 | 15.09 |
| VSMADDX | VECTOR SCALAR MULTIPLY AND ADD     | 1500               | 5.30 | 2.21 |
| VSMULXX | VECTOR SCALAR MULTIPLY             | 1500               | 5.68 | 1.64 |
| VSQRTXX | VECTOR SQUARE ROOT                 | 1500               | 1.78 | 8.85 |
| VSQXXXX | VECTOR SQUARE                      | 1500               | 5.10 | 1.93 |
| VSSQXXX | VECTOR SIGNED SQUARE               | 1500               | 6.20 | 1.93 |
| VSUBXXX | VECTOR SUBTRACT                    | 1500               | 6.00 | 1.92 |
| VSWAPXX | VECTOR SWAP                        | 1500               | 5.20 | 2.00 |
| VTAPERX | VECTOR TAPER                       | 1500               | 3.00 | 2.07 |
| VXPNDXX | VECTOR EXPAND                      | 1500               | 4.07 | 1.77 |
| WIENERX | WIENER FILTER                      | 81                 | 3.03 | 365.40 |
| XBTMUX  | DISPLAY PROCESSOR FUNCTION         | 1500               | 5.45 | 50.00 |

### Typical execution time pr. loop (microsec.)

#### Improvement ratio to ASM function

| NAME   | OPERATION                          | Number of elements |   |   |
|--------|------------------------------------|--------------------|---|---|
| APMOVE | CONVERT AND MOVE FORMAT 0          | 1500               | 2.68 | 1.38 |
| APMOVE | CONVERT AND MOVE FORMAT 1          | 1500               | 2.38 | 1.53 |
| APMOVE | CONVERT AND MOVE FORMAT 2 & 5      | 1500               | 1.17 | 0.89 |
| APMOVE | CONVERT AND MOVE FORMAT 3          | 1500               | 2.57 | 1.38 |
| APMOVE | CONVERT AND MOVE FORMAT 4          | 1500               | 2.86 | 1.53 |
| DMXB   | DEMULTIPLEX SEGMENT B              | 1500               | 2.66 | 3.01 |

The figures have been taken from a ND-560/1 with 128 k byte cache memory.

1) (Time used by machine code) / (Time used by microcode).

2) ASM - ND-500 assembler.

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 21

# ND-500 Single Precision Array Processing Functions  

## ND-500 Array Processing Functions Performance  

### 3.2 ND-570/2 Processing Performance  

| NAME   | OPERATION | Number of elements | Typical execution time pr. loop (microsec.) | Improvement ratio to FTN function¹ |
|--------|-----------|--------------------|--------------------------------------------|-----------------------------------|
| CFFTXXX | COMPLEX FAST FOURIER TRANSFORM | 1024 | 3.35 | 41.50 |
| CONVXXX | CONVOLUTION (CORRELATION) | 15000 | 6.50 | 0.93 |
| CVMULXX | COMPLEX VECTOR MULTIPLY | 1500 | 2.60 | 6.40 |
| DOTPRXX | DOT PRODUCT | 1500 | 3.40 | 1.72 |
| IBMFPCV | IBM TO ND FLOATING POINT CONVERT | 1500 | 7.15 | 3.29 |
| IMGBLD | IMAGE BUILD | 1500 | 3.50 | 1.85 |
| MAXMGVX | MAX. MAGNITUDE ELEMENT IN VECTOR | 1500 | 5.56 | 1.00 |
| MAXMNX | MAX. AND MIN. VALUE IN VECTOR | 1500 | 3.98 | 1.61 |
| MAXVXXX | MAX. VALUE IN VECTOR | 1500 | 4.64 | 0.90 |
| MINMGVX | MIN. MAGNITUDE ELEMENT IN VECTOR | 1500 | 5.62 | 1.05 |
| MINMVXX | MIN. VALUE IN VECTOR | 1500 | 4.15 | 1.01 |
| MXMNGMX | MAX. AND MIN. MAG. ELEMENT IN VECTOR | 1500 | 4.31 | 1.76 |
| NDFPCV | ND TO IBM FLOATING POINT CONVERT | 1500 | 9.95 | 2.50 |
| PREDICT | PREDICT | 1500 | 4.78 | 1.61 |
| RFFTXXX | REAL FAST FOURIER TRANSFORM | 1024 | 2.08 | 37.50 |
| SVEGMGX | SUM OF VECTOR ELEMENTS MAGNITUDE | 1500 | 5.25 | 0.74 |
| SVEQSQX | SUM OF VECTOR ELEMENTS SQUARE | 1500 | 3.70 | 1.20 |
| SVEXXXX | SUM OF VECTOR ELEMENTS | 1500 | 4.85 | 0.74 |
| SVSQXXX | SUM OF VECTOR ELEMENTS SIGNED SQUARE | 1500 | 5.25 | 0.82 |
| VABSXXX | VECTOR ABSOLUTE VALUE | 1500 | 5.35 | 0.83 |
| VADDXXX | VECTOR ADD | 1500 | 3.70 | 1.60 |
| VAVGABS | VECTOR AVERAGE ABSOLUTE VALUE | 1500 | 4.40 | 0.89 |
| VCLRXXX | VECTOR CLEAR | 1500 | 5.60 | 0.48 |
| VCOSXXX | VECTOR COSINE | 1500 | 1.47 | 10.67 |
| VDIVSXX | VECTOR SCALAR DIVIDE | 1500 | 2.45 | 2.40 |
| VDIVXXX | VECTOR DIVIDE | 1500 | 2.82 | 2.52 |
| VFLNZXX | VECTOR FIRST AND LAST NON-ZERO VALUE | 1500 | 7.62 | 0.39 |
| VGENXXX | VECTOR GENERATE | 1500 | 5.25 | 0.92 |
| VMAXMGX | VECTOR MAXIMUM MAGNITUDE | 1500 | 3.68 | 1.97 |
| VMAXXXX | VECTOR MAXIMUM | 1500 | 3.75 | 1.85 |
| VMINMGX | VECTOR MINIMUM MAGNITUDE | 1500 | 3.68 | 1.97 |
| VMINXXX | VECTOR MINIMUM | 1500 | 3.75 | 1.85 |
| VMOVXXX | VECTOR MOVE | 1500 | 5.50 | 0.80 |
| VMULXXX | VECTOR MULTIPLY | 1500 | 3.70 | 1.60 |
| VNEGXXX | VECTOR NEGATIVE | 1500 | 5.65 | 0.82 |
| VMNOSXX | VECTOR SINC INTERPOLATION | 150 | 3.00 | 14.90 |

¹ (Time used by machine code) / (Time used by microcode).  
² ASM - ND-500 assembler.

---

## Page 22

# ND-500 Single Precision Array Processing Functions
## ND-500 Array Processing Functions Performance

### Typical execution time pr. loop (microsec.)

#### Improvement ratio to FTN function

| NAME    | OPERATION                        | Number of elements | Ratio | Time  |
|---------|----------------------------------|--------------------|-------|-------|
| VNM0XXX | VECTOR LINEAR INTERPOLATION      | 1500               | 2.38  | 5.52  |
| VSADDXX | VECTOR SCALAR ADD                | 1500               | 3.25  | 1.53  |
| VSINXXX | VECTOR SINE                      | 1500               | 1.47  | 10.60 |
| VSMADDX | VECTOR SCALAR MULTIPLY AND ADD   | 1500               | 3.40  | 1.90  |
| VSMULXX | VECTOR SCALAR MULTIPLY           | 1500               | 3.27  | 1.50  |
| VSQRTXX | VECTOR SQUARE ROOT               | 1500               | 1.55  | 6.02  |
| VSQXXX  | VECTOR SQUARE                    | 1500               | 4.20  | 1.18  |
| VSSQXXX | VECTOR SIGNED SQUARE             | 1500               | 5.70  | 1.18  |
| VSUBXXX | VECTOR SUBTRACT                  | 1500               | 3.70  | 1.60  |
| VSWAPXX | VECTOR SWAP                      | 1500               | 2.80  | 2.10  |
| VTAPERX | VECTOR TAPER                     | 1500               | 2.53  | 1.37  |
| VXPNDXX | VECTOR EXPAND                    | 1500               | 3.30  | 1.13  |
| WIENERX | WIENER FILTER                    | 81                 | 1.90  | 251.85|
| XBTMUX  | DISPLAY PROCESSOR FUNCTION       | 1500               | 4.45  | 32.10 |

#### Typical execution time pr. loop (microsec.)

#### Improvement ratio to ASM function

| NAME    | OPERATION                        | Number of elements | Ratio | Time  |
|---------|----------------------------------|--------------------|-------|-------|
| APMOVE  | CONVERT AND MOVE FORMAT 0        | 1500               | 1.83  | 1.24  |
| APMOVE  | CONVERT AND MOVE FORMAT 1        | 1500               | 2.40  | 1.00  |
| APMOVE  | CONVERT AND MOVE FORMAT 2 & 5    | 1500               | 1.73  | 0.49  |
| APMOVE  | CONVERT AND MOVE FORMAT 3        | 1500               | 1.90  | 1.25  |
| APMOVE  | CONVERT AND MOVE FORMAT 4        | 1500               | 3.68  | 0.75  |
| DMX8    | DEMULTIPLEX SEGMENT B            | 1500               | 2.60  | 1.94  |
| VTAPERX | VECTOR TAPER                     | 1500               | 2.53  | 1.37  |

The figures have been taken from a ND-570/2 with 32k bytes of cache memory.

1. (Time used by machine code) / (Time used by microcode).
2. ASM - ND-500 assembler.

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 23

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS  
ARRAY PROCESSING FUNCTIONS

## 4 ARRAY PROCESSING FUNCTIONS

### 4.1 INTRODUCTION

This chapter contains a listing of each array processing function. For each routine, the required parameter list for calling the array processing function is included together with definitions.

The ND-500 array processing functions are implemented as one machine instruction, except the functions DMXB, WIENERX, CFFTTXX, and RFFTTXX, which are partly microcoded. These are implemented as different instructions to be executed consecutively.

The function DMXB uses two instruction codes. Functions XBTMUX and IMGBLD use a third instruction code. All of the other functions use a fourth instruction code. The contents of the record register are the only difference between the functions and are used to distinguish between them.

For each routine, an identification number is given as a cross reference between the object code and the array processing function. This identification number is given as two octal numbers: 'Ident (R:I) : xxx:nnnnnB'. 'xxx' are the contents of the record register. 'nnnnn' is the instruction code used for the processing function.

The library for the ND-500 array processing functions consists of one routine for each of the array processing functions. Each routine builds a data stack used by the array processing function to find addresses of input and output arrays, scalar values or addresses, index increments and element counts.

An address is a pointer to the logical memory for both input and output arrays.

Scalars to be used in an operation are located in the data stack as 32 bit floating point numbers.

Scalars to be returned from an operation are returned to the address given in the data stack.

Index increments and element counts are given in the data stack as 32 bit integers.

It is not necessary to use the alternative return argument when calling a routine.

ND-05.013.03  
Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 24

# 4.2 VECTOR ADD (VADXXXX)

## Format

VADXXXX(VA,INCA,VB,INCB,VC,INCC,NN,*)  
Ident (R:I) : 001:177517B

## Explanation

Add the corresponding elements of two vectors. VCn = VAn + VBn, 'n' is the element index.

## Parameters

| Parameter | Description                   |
|-----------|-------------------------------|
| VA        | Name of input vector VA.      |
| INCA      | VA index increment.           |
| VB        | Name of input vector VB.      |
| INCB      | VB index increment.           |
| VC        | Name of output vector VC.     |
| INCC      | VC index increment.           |
| NN        | Element count.                |

## Listing

```
SUBROUTINE VADXXXX(VA,INCA,VB,INCB,VC,INCC,NN,*)
DIMENSION VA(1),VB(1),VC(1)
IA = 1
IB = 1
IC = 1
DO FOR M = 1,NN
  VC(IC) = VB(IB) + VA(IA)
  IA = IA + INCA
  IB = IB + INCB
  IC = IC + INCC
ENDDO
RETURN 1
END
```

---

## Page 25

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS
## ARRAY PROCESSING FUNCTIONS

### 4.3 VECTOR SUBTRACT (VSUBXXX)

#### Format

VSUBXXX(VA,INCA,VB,INCB,VC,INCC,NN,*) Ident (R:I) : 002:177517B

#### Explanation

Subtract the corresponding elements of two vectors. VCn = VBn - VAn, 'n' is the element index.

#### Parameters

| Parameter | Description                 |
|-----------|-----------------------------|
| VA        | Name of input vector VA.    |
| INCA      | VA index increment.         |
| VB        | Name of input vector VB.    |
| INCB      | VB index increment.         |
| VC        | Name of output vector VC.   |
| INCC      | VC index increment.         |
| NN        | Element count.              |

#### Listing

```
SUBROUTINE VSUBXXX(VA,INCA,VB,INCB,VC,INCC,NN,*)
DIMENSION VA(1),VB(1),VC(1)
IA = 1
IB = 1
IC = 1
DO FOR M = 1,NN
  VC(IC) = VB(IB) - VA(IA)
  IA = IA + INCA
  IB = IB + INCB
  IC = IC + INCC
ENDDO
RETURN 1
END
```

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 26

# 4.4 VECTOR MULTIPLY (VMULXXX)

## Format

VMULXXX(VA,INCA,VB,INCB,VC,INCC,NN,*) &nbsp;&nbsp;&nbsp; Ident (R:I) : 003:177517B

## Explanation

Multiply the corresponding elements of two vectors. VCn = VBn * VAn, 'n' is the element index.

## Parameters

| Parameter | Description                     |
|-----------|---------------------------------|
| VA        | Name of input vector VA.        |
| INCA      | VA index increment.             |
| VB        | Name of input vector VB.        |
| INCB      | VB index increment.             |
| VC        | Name of output vector VC.       |
| INCC      | VC index increment.             |
| NN        | Element count.                  |

## Listing

SUBROUTINE VMULXXX(VA,INCA,VB,INCB,VC,INCC,NN,*)

DIMENSION VA(1),VB(1),VC(1)

IA = 1

IB = 1

IC = 1

DO FOR M = 1,NN

&nbsp;&nbsp;&nbsp;VC(IC) = VB(IB) * VA(IA)

&nbsp;&nbsp;&nbsp;IA = IA + INCA

&nbsp;&nbsp;&nbsp;IB = IB + INCB

&nbsp;&nbsp;&nbsp;IC = IC + INCC

ENDDO

RETURN 1

END

ND-05.013.03

---

## Page 27

# ND-500 Single Precision Array Processing Functions

## 4.5 Vector Divide (VDIVXXX)

### Format

VDIVXXX(VA,INCA,VB,INCB,VC,INCC,NN,*) Ident (R:I) : 004:177517B

### Explanation

Divide the corresponding elements of two vectors. VCn = VBn/VAn, 'n' is the element index.

### Parameters

| Parameter | Description                            |
|-----------|----------------------------------------|
| VA        | Name of input vector VA.               |
| INCA      | VA index increment.                    |
| VB        | Name of input vector VB.               |
| INCB      | VB index increment.                    |
| VC        | Name of output vector VC.              |
| INCC      | VC index increment.                    |
| NN        | Element count.                         |

### Listing

SUBROUTINE VDIVXXX(VA,INCA,VB,INCB,VC,INCC,NN,*)
DIMENSION VA(1),VB(1),VC(1)
IA = 1
IB = 1
IC = 1
DO FOR M = 1,NN
  VC(IC) = VB(IB) / VA(IA)
  IA = IA + INCA
  IB = IB + INCB
  IC = IC + INCC
ENDDO
RETURN 1
END

ND-05.013.03

---

## Page 28

# 4.6 VECTOR MAXIMUM (VMAXXX)

## Format

VMAXXX(VA,INCA,VB,INCB,VC,INCC,NN,*) Ident (R:I) : 005:177517B

## Explanation

Form a vector from the maximum value of each corresponding pair of elements of two vectors. VCn = VAn if VAn > VBn, else VCn = VBn. 'n' is the element index.

## Parameters

| Parameter | Description                  |
|-----------|------------------------------|
| VA        | Name of input vector VA.     |
| INCA      | VA index increment.          |
| VB        | Name of input vector VB.     |
| INCB      | VB index increment.          |
| VC        | Name of output vector VC.    |
| INCC      | VC index increment.          |
| NN        | Element count.               |

## Listing

```
SUBROUTINE VMAXXX(VA,INCA,VB,INCB,VC,INCC,NN,*)
DIMENSION VA(1),VB(1),VC(1)
IA = 1
IB = 1
IC = 1
DO FOR M = 1,NN
  VC(IC) = AMAX1(VA(IA),VB(IB))
  IA = IA + INCA
  IB = IB + INCB
  IC = IC + INCC
ENDDO
RETURN 1
END
```

---

## Page 29

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## ARRAY PROCESSING FUNCTIONS

### 4.7 VECTOR MINIMUM (VMINXXX)

**Format**

VMINXXX(VA,INCA,VB,INCB,VC,INCC,NN,*) Ident (R:I) : 006:177517B

**Explanation**

Form a vector from the minimum value of each corresponding pair of elements of two vectors. VCn = VAn if VAn < VBn, else VCn = VBn. 'n' is the element index.

**Parameters**

| Parameter | Description                    |
|-----------|--------------------------------|
| VA        | Name of input vector VA.       |
| INCA      | VA index increment.            |
| VB        | Name of input vector VB.       |
| INCB      | VB index increment.            |
| VC        | Name of output vector VC.      |
| INCC      | VC index increment.            |
| NN        | Element count.                 |

**Listing**

SUBROUTINE VMINXXX(VA,INCA,VB,INCB,VC,INCC,NN,*)

DIMENSION VA(1),VB(1),VC(1)

IA = 1  
IB = 1  
IC = 1  

DO FOR M = 1,NN  
VC(IC) = AMIN1(VA(IA),VB(IB))  
IA = IA + INCA  
IB = IB + INCB  
IC = IC + INCC  
ENDDO  

RETURN 1  
END

---

## Page 30

# 4.8 VECTOR MAXIMUM MAGNITUDE (VMAXMGX)

## Format

VMAXMGX(VA,INCA,VB,INCB,VC,INCC,NN,*)   Ident (R:I) : 007:177517B

## Explanation

Form a vector from the maximum absolute value of each corresponding pair of elements of two vectors. VCn = |VAn| if |VAn| > |VBn|, else VCn = |VBn|. 'n' is the element index.

## Parameters

| Parameter | Description              |
|-----------|--------------------------|
| VA        | Name of input vector VA. |
| INCA      | VA index increment.      |
| VB        | Name of input vector VB. |
| INCB      | VB index increment.      |
| VC        | Name of output vector VC.|
| INCC      | VC index increment.      |
| NN        | Element count.           |

## Listing

```
SUBROUTINE VMAXMGX(VA,INCA,VB,INCB,VC,INCC,NN,*)
DIMENSION VA(1),VB(1),VC(1)
IA = 1
IB = 1
IC = 1
DO FOR M = 1,NN
    VC(IC) = AMAX1(ABS(VA(IA)),ABS(VB(IB)))
    IA = IA + INCA
    IB = IB + INCB
    IC = IC + INCC
ENDDO
RETURN 1
END
```

---

## Page 31

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS  
## ARRAY PROCESSING FUNCTIONS

### 4.9 VECTOR MINIMUM MAGNITUDE (VMINMGX)

#### Format

VMINMGX(VA,INCA,VB,INCB,VC,INCC,NN,*) &nbsp; &nbsp; Ident (R:I) : 010:177517B

#### Explanation

Form a vector from the minimum absolute value of each corresponding pair of elements of two vectors. VCn = |VAn| if |VAn| < |VBn|, else VCn = |VBn|. 'n' is the element index.

#### Parameters

| Parameter | Description                  |
|-----------|------------------------------|
| VA        | Name of input vector VA.     |
| INCA      | VA index increment.          |
| VB        | Name of input vector VB.     |
| INCB      | VB index increment.          |
| VC        | Name of output vector VC.    |
| INCC      | VC index increment.          |
| NN        | Element count.               |

#### Listing

```
SUBROUTINE VMINMGX(VA,INCA,VB,INCB,VC,INCC,NN,*)
DIMENSION VA(1),VB(1),VC(1)
IA = 1
IB = 1
IC = 1
DO FOR M = 1,NN
   VC(IC) = AMIN1(ABS(VA(IA)),ABS(VB(IB)))
   IA = IA + INCA
   IB = IB + INCB
   IC = IC + INCC
ENDDO
RETURN 1
END
```

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 32

# 4.10 VECTOR SQUARE (VSQXXXX)

## Format

VSQXXXX(VA,INCA,VC,INCC,NN,*) Ident (R:I) : 042:177517B

## Explanation

Square the elements of a vector. VCn = (VAn)². 'n' is the element index.

## Parameters

| Parameter | Description                |
|-----------|----------------------------|
| VA        | Name of input vector VA.   |
| INCA      | VA index increment.        |
| VC        | Name of output vector VC.  |
| INCC      | VC index increment.        |
| NN        | Element count.             |

## Listing

    SUBROUTINE VSQXXXX(VA,INCA,VC,INCC,NN,*)
    DIMENSION VA(1),VC(1)
    IA = 1
    IC = 1
    DO FOR M = 1,NN
        VC(IC) = VA(IA)**2
        IA = IA + INCA
        IC = IC + INCC
    ENDDO
    RETURN 1
    END

ND-05.013.03

---

## Page 33

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## ARRAY PROCESSING FUNCTIONS

### 4.11 VECTOR SIGNED SQUARE (VSSQXXX)

**Format**

VSSQXXX(VA,INCA,VC,INCC,NN,\*) &nbsp; &nbsp; &nbsp; &nbsp; Ident (R:I) : 011:177517B

**Explanation**

Multiply each element of a vector with the absolute value of itself. VCn = VAn \* |VAn|. 'n' is the element index.

**Parameters**

| Parameter | Description                  |
|-----------|------------------------------|
| VA        | Name of input vector VA.     |
| INCA      | VA index increment.          |
| VC        | Name of output vector VC.    |
| INCC      | VC index increment.          |
| NN        | Element count.               |

**Listing**

```
SUBROUTINE VSSQXXX(VA,INCA,VC,INCC,NN,*)
DIMENSION VA(1),VC(1)
IA = 1
IC = 1
DO FOR M = 1,NN
  VC(IC) = SIGN(VA(IA)**2,VA(IA))
  IA = IA + INCA
  IC = IC + INCC
ENDDO
RETURN 1
END
```

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 34

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS
## ARRAY PROCESSING FUNCTIONS

### 4.12 VECTOR ABSOLUTE VALUE (VABSXXX)

#### Format

VABSXXX(VA,INCA,VC,INCC,NN,*)  
Ident (R:I) : 012:177517B

#### Explanation

Form a vector from the absolute values of the elements in a vector.  
VCn = |VAn|. 'n' is the element index.

#### Parameters

| Parameter | Description                  |
|-----------|------------------------------|
| VA        | Name of input vector VA.     |
| INCA      | VA index increment.          |
| VC        | Name of output vector VC.    |
| INCC      | VC index increment.          |
| NN        | Element count.               |

#### Listing

```
SUBROUTINE VABSXXX(VA,INCA,VC,INCC,NN,*)
DIMENSION VA(1),VC(1)
IA = 1
IC = 1
DO FOR M = 1,NN
  VC(IC) = ABS(VA(IA))
  IA = IA + INCA
  IC = IC + INCC
ENDDO
RETURN 1
END
```

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 35

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS  
ARRAY PROCESSING FUNCTIONS

## 4.13 VECTOR SQUARE ROOT (VSQRTXX)

### Format

VSQRTXX(VA,INCA,VC,INCC,NN,*)

Ident (R:I) : 013:177517B

### Explanation

Take the square roots of the elements in a vector. VCn = √VAn. 'n' is the element index.

### Parameters

| Parameter | Description                   |
|-----------|-------------------------------|
| VA        | Name of input vector VA.      |
| INCA      | VA index increment.           |
| VC        | Name of output vector VC.     |
| INCC      | VC index increment.           |
| NN        | Element count.                |

### Listing

    SUBROUTINE VSQRTXX(VA,INCA,VC,INCC,NN,*)
    DIMENSION VA(1),VC(1)
    IA = 1
    IC = 1
    DO FOR M = 1,NN
        VC(IC) = SQRT(VA(IA))
        IA = IA + INCA
        IC = IC + INCC
    ENDDO
    RETURN 1
    END

ND-05.013.03

---

## Page 36

# 4.14 VECTOR SINE (VSINXXX)

**Format**

VSINXXX(VA,INCA,VC,INCC,NN,*) Ident (R:I) : 014:177517B

**Explanation**

Compute the sine of the elements of a vector. VCn = sin(VAn). 'n' is the element index. The arguments in VA must be in radians.

**Parameters**

| Parameter | Description                  |
|-----------|------------------------------|
| VA        | Name of input vector VA.     |
| INCA      | VA index increment.          |
| VC        | Name of output vector VC.    |
| INCC      | VC index increment.          |
| NN        | Element count.               |

**Listing**

    SUBROUTINE VSINXXX(VA,INCA,VC,INCC,NN,*)
    DIMENSION VA(1),VC(1)
    IA = 1
    IC = 1
    DO FOR M = 1,NN
        VC(IC) = SIN(VA(IA))
        IA = IA + INCA
        IC = IC + INCC
    ENDDO
    RETURN 1
    END

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 37

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## ARRAY PROCESSING FUNCTIONS

### 4.15 VECTOR COSINE (VCOSXXX)

**Format**

VCOSXXX(VA,INCA,VC,INCC,NN,*)  
Ident (R:I) : 015:177517B

**Explanation**

Compute the cosine of the elements of a vector. VCn = cos(VAn). 'n' is the element index. The arguments in VA must be in radians.

**Parameters**

| Parameter | Description               |
|-----------|---------------------------|
| VA        | Name of input vector VA.  |
| INCA      | VA index increment.       |
| VC        | Name of output vector VC. |
| INCC      | VC index increment.       |
| NN        | Element count.            |

**Listing**

```
SUBROUTINE VCOSXXX(VA,INCA,VC,INCC,NN,*)
DIMENSION VA(1),VC(1)
IA = 1
IC = 1
DO FOR M = 1,NN
  VC(IC) = COS(VA(IA))
  IA = IA + INCA
  IC = IC + INCC
ENDDO
RETURN 1
END
```

---

## Page 38

# 4.16 Vector Move (VMOVXXX)

## Format

VMOVXXX(VA, INCA, VC, INCC, NN, \*) &nbsp;&nbsp; Ident (R:I) : 016:177517B

## Explanation

Move the elements from one vector into another. VCn = VAn, 'n' is the element index.

## Parameters

| Parameter | Description                    |
|-----------|--------------------------------|
| VA        | Name of input vector VA.       |
| INCA      | VA index increment.            |
| VC        | Name of output vector VC.      |
| INCC      | VC index increment.            |
| NN        | Element count.                 |

## Listing

```
SUBROUTINE VMOVXXX(VA, INCA, VC, INCC, NN, \*)
DIMENSION VA(1), VC(1)
IA = 1
IC = 1
DO FOR M = 1, NN
    VC(IC) = VA(IA)
    IA = IA + INCA
    IC = IC + INCC
ENDDO
RETURN 1
END
```

---

## Page 39

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## ARRAY PROCESSING FUNCTIONS

### 4.17 VECTOR SWAP (VSWAPXX)

**Format**

VSWAPXX(VA, INCA, VC, INCC, NN, *)

Ident (R:I) : 063:177517B

**Explanation**

Swap the elements between two vectors. VAn ↔ VBn and VBn ↔ VAn, 'n' is the element index.

**Parameters**

| Parameter | Description                               |
|-----------|-------------------------------------------|
| VA        | Name of input and output vector VA.       |
| INCA      | VA index increment.                       |
| VC        | Name of input and output vector VC.       |
| INCC      | VC index increment.                       |
| NN        | Element count.                            |

**Listing**

```
SUBROUTINE VSWAPXX(VA,INCA,VC,INCC,NN,*)
DIMENSION VA(1),VC(1)
REAL HOLD
IA = 1
IC = 1
DO FOR M = 1,NN
  HOLD = VC(IC)
  VC(IC) = VA(IA)
  VA(IA) = HOLD
  IA = IA + INCA
  IC = IC + INCC
ENDDO
RETURN 1
END
```

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 40

# 4.18 VECTOR NEGATIVE (VNEGXXX)

## Format

VNEGXXX(VA,INCA,VC,INCC,NN,*)  
Ident (R:I) : 064:177517B

## Explanation

Form a vector from the elements of another vector multiplied with -1.  
VCn = -VAn, 'n' is the element index.

## Parameters

| Parameter | Description                  |
|-----------|------------------------------|
| VA        | Name of input vector VA.     |
| INCA      | VA index increment.          |
| VC        | Name of output vector VC.    |
| INCC      | VC index increment.          |
| NN        | Element count.               |

## Listing

    SUBROUTINE VNEGXXX(VA,INCA,VC,INCC,NN,*)
    DIMENSION VA(1),VC(1)
    IA = 1
    IC = 1
    DO FOR M = 1,NN
    VC(IC) = - VA(IA)
    IA = IA + INCA
    IC = IC + INCC
    ENDDO
    RETURN 1
    END

ND-05.013.03  

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 41

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## ARRAY PROCESSING FUNCTIONS

### 4.19 ND TO IBM FLOATING POINT CONVERT (NDFPCV)

**Format**

NDFPCV(VA,INCA,VC,INCC,NN,*) Ident (R:I) : 017:177517B

**Explanation**

Convert the elements of a vector into ND floating point format to IBM floating point format. Symbolically this can be represented by the formula: VCn = IBMFP(VAn), 'n' denotes the element index.

**Parameters**

| Parameter | Description                        |
|-----------|------------------------------------|
| VA        | Name of input vector VA.           |
| INCA      | VA index increment.                |
| VC        | Name of output vector VC.          |
| INCC      | VC index increment.                |
| NN        | Element count.                     |

**Listing**

```
SUBROUTINE NDFPCV(VA,INCA,VC,INCC,NN,*)
REAL VA(1),VC(1),TEMP
INTEGER MANT,CHAR,HIDBIT,HEXCHR,RSHFT,ITEMP
INTEGER ROUND(3)
EQUIVALENCE (TEMP,ITEMP)
DATA MASK1 /000177777777B/
DATA HIDBIT /000200000000B/
DATA MASK2 /177777777777B/
DATA MASK3 /200000000000B/
DATA ROUND /1,2,4/
IA = 1
IC = 1
DO FOR M = 1,NN
  TEMP = VA(IA)
  IF (ITEMP .EQ. 0) THEN
    VC(IC) = 0.0
    GO TO 100
  ENDIF
  MANT = (IAND(ITEMP,MASK1) + HIDBIT)*2
  CHAR = IAND(ITEMP,MASK2)
  CHAR = ISHFT(CHAR,-22)
  HEXCHR = CHAR/4
  RSHFT = 4 - MOD(CHAR,4)
  IF (RSHFT .NE. 4) THEN
    MANT = MANT + ROUND(RSHFT)
    MANT = ISHFT(MANT,-RSHFT)
    HEXCHR = HEXCHR + 1
  ENDIF
  IF (HEXCHR .GT. 127) THEN
```

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 42

# ND-500 Single Precision Array Processing Functions

## Array Processing Functions

```
HEXCHR = 127
MANT = 00077777777B
ENDIF
ITEMP = ISHFT(HEXCHR,24) + MANT
IF (VA(IA) .LT. 0.) ITEMP = IOR(ITEMP,MASK3)
VC(IC) = TEMP
100 CONTINUE
IA = IA + INCA
IC = IC + INCC
ENDDO
RETURN 1
END
```

---

ND-05.013.03

*Scanned by Jonny Oddene for Sintran Data © 2011*

---

## Page 43

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## ARRAY PROCESSING FUNCTIONS

### 4.20 IBM TO ND FLOATING POINT CONVERT (IBM#FPCV)

#### Format

```
IBM#FPCV(VA,INCA,VC,INCC,NN,*)                Ident (R:I) : 020:177517B
```

#### Explanation

To convert the elements of a vector in IBM floating point format into ND floating point format. Symbolically this can be represented by the formula: VCn = NDFP(VAn), 'n' denotes the element index.

#### Parameters

| Parameter | Description                  |
|-----------|------------------------------|
| VA        | Name of input vector VA.     |
| INCA      | VA index increment.          |
| VC        | Name of output vector VC.    |
| INCC      | VC index increment.          |
| NN        | Element count.               |

#### Listing

```
    SUBROUTINE IBM#FPCV(VA,INCA,VC,INCC,NN,*)
    REAL VA(1),VC(1),TEMP
    INTEGER MANTSFT,CHAR,RSHFT,ITEMP,SHFTCNT
    EQUIVALENCE (TEMP,ITEMP)
    DATA MASK1 /177000000000B/
    DATA MASK2 /000077777777B/
    DATA MASK3 /200000000000B/
    DATA MASK4 /000074000000B/
    IA = 1
    IC = 1
    DO FOR M = 1,NN
    SHFTCNT = -1
    TEMP = VA(IA)
    IF (ITEMP .EQ. 0) THEN
        VC(IC) = 0.0
        GO TO 100
    ENDIF
    CHAR = IAND(ITEMP,MASK1)
    CHAR = ISHFT(CHAR,-24)*4
    MANTSFT = IAND(ITEMP,MASK4)
    MANTSFT = ISHFT(MANTSFT,-20)
    IF (MANTSFT.GT.0) SHFTCNT=4
    IF (MANTSFT.GT.1) SHFTCNT=3
    IF (MANTSFT.GT.3) SHFTCNT=2
    IF (MANTSFT.GT.7) SHFTCNT=1
    CHAR = CHAR-SHFTCNT+1
    ITEMP = ISHFT(ITEMP,SHFTCNT)
    ITEMP = IAND (ITEMP,MASK2)
    ITEMP = ISHFT(ITEMP,-2) + ISHFT(CHAR,22)
```

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 44

# ND-500 Single Precision Array Processing Functions

## Array Processing Functions

```
IF ( VA(IA) .LT. 0. ) ITEMP = IOR (ITEMP,MASK3)
VC(IC) = TEMP
IF(SHFTCNT.EQ.-1)VC(IC)=177777777777B
100 CONTINUE
IA = IA + INCA
IC = IC + INCC
ENDDO
RETURN 1
END
```

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 45

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS
## ARRAY PROCESSING FUNCTIONS

### 4.21 SUM OF VECTOR ELEMENTS (SVEXXXX)

#### Format

SVEXXXX(VA,INCA,VC,NN,*) Ident (R:I) : 021:177517B

#### Explanation

Add the elements of a vector. VC = VA₁ + VA₂ + ... + VAnn, 'nn' is the element count.

#### Parameters

| Name | Description |
|------|-------------|
| VA   | Name of input vector VA. |
| INCA | VA index increment. |
| VC   | Name of output scalar VC. |
| NN   | Element count. |

#### Listing

```
SUBROUTINE SVEXXXX(VA,INCA,VC,NN,*)
DIMENSION VA(1)
IA = 1
SUM = 0.0
DO FOR M = 1,NN
  SUM = SUM + VA(IA)
  IA = IA + INCA
ENDDO
VC = SUM
RETURN 1
END
```

ND-05.013.03

--- 

*Scanned by Jonny Oddene for Sintran Data © 2011*

---

## Page 46

# 4.22 Sum of Vector Elements Magnitude (SVEMGXX)

## Format

SVEMGXX(VA,INCA,VC,NN,*)  
Ident (R:I) : 065:177517B

## Explanation

Form the sum of the absolute values of the elements of a vector.  
VC = \|VA₁\| + \|VA₂\| + ... + \|VAnn\|, 'nn' is the element count.

## Parameters

| Name | Description |
|------|-------------|
| VA   | Name of input vector VA. |
| INCA | VA index increment. |
| VC   | Name of output scalar VC. |
| NN   | Element count. |

## Listing

```
SUBROUTINE SVEMGXX(VA,INCA,VC,NN,*)
DIMENSION VA(1)
IA = 1
SUM = 0.0
DO FOR M = 1,NN
    SUM = SUM + ABS(VA(IA))
    IA = IA + INCA
ENDDO
VC = SUM
RETURN 1
END
```

---

## Page 47

# ND-500 Single Precision Array Processing Functions

## 4.23 Sum of Vector Elements Square (SVESQXX)

### Format

SVESQXX(VA,INCA,VC,NN,\*) &nbsp;&nbsp;&nbsp;&nbsp; Ident (R:I) : 066:177517B

### Explanation

Form the sum of the squared elements of a vector.  
VC = (VA₁)² + (VA₂)² + ....+ (VAnn)², 'nn' is the element count.

### Parameters

| Parameter | Description                   |
|-----------|-------------------------------|
| VA        | Name of input vector VA.      |
| INCA      | VA index increment.           |
| VC        | Name of output scalar VC.     |
| NN        | Element count.                |

### Listing

```
SUBROUTINE SVESQXX(VA,INCA,VC,NN,*)  
DIMENSION VA(1)  
IA = 1  
SUM= 0.0  
DO FOR M = 1,NN  
    SUM=SUM + VA(IA)**2  
    IA = IA + INCA  
ENDDO  
VC=SUM  
RETURN 1  
END  
```

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 48

# 4.24 Sum of Vector Elements Signed Square (SVSXXXX)

## Format

SVSXXXX(VA,INCA,VC,NN,*)  
Ident (R:I) : 022:177517B

## Explanation

Form the sum of the elements of a vector, where each element at first is multiplied with the absolute value of itself.  
VC = VA₁ * |VA₁| + VA₂ * |VA₂| + ... + VAnn * |VAnn|, 'nn' is the element count.

## Parameters

| Parameter | Description                   |
|-----------|-------------------------------|
| VA        | Name of input vector VA.      |
| INCA      | VA index increment.           |
| VC        | Name of output scalar VC.     |
| NN        | Element count.                |

## Listing

    SUBROUTINE SVSXXXX(VA,INCA,VC,NN,*)
    DIMENSION VA(1)
    IA = 1
    SUM= 0.0
    DO  FOR M = 1,NN
        SUM=SUM + SIGN(VA(IA)*VA(IA),VA(IA))
        IA = IA + INCA
    ENDDO
    VC=SUM
    RETURN 1
    END

ND-05.013.03
  
Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 49

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS  
## ARRAY PROCESSING FUNCTIONS

### 4.25 VECTOR AVERAGE ABSOLUTE VALUE (VAVGABS)

**Format**

VAVGABS(VA,INCA,VC,NN,*)  
Ident (R:I) : 023:177517B

**Explanation**

Form the mean value of the absolute values of the elements of a vector.  
vc = (|VA₁| + |VA₂| + ....+ |VAnn|) / nn , 'nn' is the element count.

**Parameters**

| Parameter | Description                          |
|-----------|--------------------------------------|
| VA        | Name of input vector VA.             |
| INCA      | VA index increment.                  |
| VC        | Name of output scalar VC.            |
| NN        | Element count.                       |

**Listing**

```
SUBROUTINE VAVGABS(VA,INCA,VC,NN,*)  
DIMENSION VA(1)  
IA = 1  
SUMABS = 0.0  
DO FOR I = 1,NN  
    SUMABS = SUMABS + ABS(VA(IA))  
    IA = IA + INCA  
ENDDO  
VC = SUMABS/NN  
RETURN 1  
END  
```

ND-05.013.03

---

## Page 50

# 4.26 Maximum Value in Vector (MAXVXXX)

## Format

MAXVXXX(VA,INCA,VC,NN,*) Ident (R:I) : 024:177517B

## Explanation

Scan a vector for its element with maximum value and return this (VC₁) together with the corresponding index (VC₂).

## Parameters

| Parameter | Description                      |
|-----------|----------------------------------|
| VA        | Name of input vector VA.         |
| INCA      | VA index increment.              |
| VC        | Name of output vector VC.        |
| NN        | Element count.                   |

## Listing

    SUBROUTINE MAXVXXX(VA,INCA,VC,NN,*)
    DIMENSION VA(1),VC(1)
    IA = 1
    VC(1) = VA(IA)
    VC(2) = IA
    DO FOR M = 2,NN
      IA = IA + INCA
      IF (VA(IA) .GT. VC(1)) THEN
        VC(1) = VA(IA)
        VC(2) = IA
      ENDIF
    ENDDO
    RETURN 1
    END

---

ND-05.013.03

---

## Page 51

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## ARRAY PROCESSING FUNCTIONS

### 4.27 MINIMUM VALUE IN VECTOR (MINVXXX)

#### Format

MINVXXX(VA,INCA,VC,NN,\*)    Ident (R:I) : 025:177517B

#### Explanation

Scan a vector for its element with minimum value and return this (VC<sub>1</sub>) together with the corresponding index (VC<sub>2</sub>).

#### Parameters

| Parameter | Description                     |
|-----------|---------------------------------|
| VA        | Name of input vector VA.        |
| INCA      | VA index increment.             |
| VC        | Name of output vector VC.       |
| NN        | Element count.                  |

#### Listing

    SUBROUTINE MINVXXX(VA,INCA,VC,NN,\*) 
    DIMENSION VA(1),VC(1)
    IA = 1
    VC(1) = VA(IA)
    VC(2) = IA
    DO FOR M = 2,NN
        IA = IA + INCA
        IF (VA(IA) .LT. VC(1)) THEN
            VC(1) = VA(IA)
            VC(2) = IA
        ENDIF
    ENDDO
    RETURN 1
    END

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 52

# 4.28 Maximum Magnitude Element in Vector (MAXMGVX)

## Format

MAXMGVX(VA,INCA,VC,NN,*) &nbsp;&nbsp;&nbsp; Ident (R:I) : 026:177517B

## Explanation

Scan a vector for its element with maximum absolute value, and return this (VC\_1) together with the corresponding index (VC\_2).

## Parameters

| Parameter | Description                  |
|-----------|------------------------------|
| VA        | Name of input vector VA.     |
| INCA      | VA index increment.          |
| VC        | Name of output vector VC.    |
| NN        | Element count.               |

## Listing

```
SUBROUTINE MAXMGVX(VA,INCA,VC,NN,*)
DIMENSION VA(1),VC(1)
IA = 1
VC(1) = ABS(VA(IA))
VC(2) = IA
DO FOR M = 2,NN
    IA = IA + INCA
    VAABS = ABS(VA(IA))
    IF (VAABS .GT. VC(1)) THEN
        VC(1) = VAABS
        VC(2) = IA
    ENDIF
ENDDO
RETURN 1
END
```

ND-05.013.03

---

## Page 53

# ND-500 Single Precision Array Processing Functions

## Array Processing Functions

### 4.29 Minimum Magnitude Element in Vector (MINMGVX)

**Format**

MINMGVX(VA,INCA,VC,NN,*) Ident (R:I): 027:177517B

**Explanation**

Scan a vector for its element with minimum absolute value, and return this (VC₁) together with the corresponding index (VC₂).

**Parameters**

| Parameter | Description                     |
|-----------|---------------------------------|
| VA        | Name of input vector VA.        |
| INCA      | VA index increment.             |
| VC        | Name of output vector VC.       |
| NN        | Element count.                  |

**Listing**

    SUBROUTINE MINMGVX(VA,INCA,VC,NN,*)
    DIMENSION VA(1),VC(1)
    IA = 1
    VC(1) = ABS(VA(IA))
    VC(2) = IA
    DO FOR M = 2,NN
      IA = IA + INCA
      VAABS = ABS(VA(IA))
      IF (VAABS .LT. VC(1)) THEN
        VC(1) = VAABS
        VC(2) = IA
      ENDIF
    ENDDO
    RETURN 1
    END

---

ND-05.013.03

---

## Page 54

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS  
ARRAY PROCESSING FUNCTIONS

## 4.30 Maximum and Minimum Value in Vector (MAXMINX)

### Format

`MAXMINX(VA, INCA, VC, NN, *)` Ident (R:I) : 030:177517B

### Explanation

Scan a vector for its element with maximum value and its element with minimum value. The maximum value is returned in VC₁ and with index for VA in VC₃. The minimum value is returned in VC₂ and with index for VA in VC₄.

### Parameters

| Parameter | Description                  |
|-----------|------------------------------|
| VA        | Name of input vector VA.     |
| INCA      | VA index increment.          |
| VC        | Name of output vector VC.    |
| NN        | Element count.               |

### Listing

```
SUBROUTINE MAXMINX(VA, INCA, VC, NN, *)
DIMENSION VA(1), VC(1)
IA = 1
VC(1) = VA(IA)
VC(2) = VA(IA)
VC(3) = IA
VC(4) = IA
DO FOR M = 2, NN
    IA = IA + INCA
    IF (VA(IA) .GT. VC(1)) THEN
        VC(1) = VA(IA)
        VC(3) = IA
    ELSEIF (VA(IA) .LT. VC(3)) THEN
        VC(2) = VA(IA)
        VC(4) = IA
    ENDIF
ENDDO
RETURN 1
END
```

ND-05.013.03

---

## Page 55

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## ARRAY PROCESSING FUNCTIONS

### 4.31 Maximum and Minimum Magnitude Element in Vector (MXMNMGX)

**Format**

MXMNMGX(VA,INCA,VC,NN,*) &nbsp;&nbsp;&nbsp;&nbsp; Ident (R:I) : 031:177517B

**Explanation**

Scan a vector for its element with absolute maximum value and its element with minimum absolute value. The maximum value is returned in VC and with index for VA in VC. The minimum value is returned in VC and with index for VA in VC.

**Parameters**

| Parameter | Description               |
|-----------|---------------------------|
| VA        | Name of input vector VA.  |
| INCA      | VA index increment.       |
| VC        | Name of output vector VC. |
| NN        | Element count.            |

**Listing**

```
SUBROUTINE MXMNMGX(VA,INCA,VC,NN,*)
DIMENSION VA(1),VC(1)
IA = 1
VC(1) = ABS(VA(IA))
VC(2) = ABS(VA(IA))
VC(3) = IA
VC(4) = IA
DO FOR M = 2,NN
  IA = IA + INCA
  VAABS = ABS(VA(IA))
  IF (VAABS .GT. VC(1)) THEN
    VC(1) = VAABS
    VC(3) = IA
  ELSEIF (VAABS .LT. VC(3)) THEN
    VC(2) = VAABS
    VC(4) = IA
  ENDIF
ENDDO
RETURN 1
END
```

ND-05.013.03

---

## Page 56

# 4.32 Vector Scalar Add (VSADDXX)

## Format

VSADDXX(VA, INCA, B, VC, INCC, NN, \*)  
Ident (R:I) : 032:177517B

## Explanation

Add the elements of a vector together with a scalar value.  
VCn = VAn + b, where 'b' denotes the scalar, and 'n' denotes the element index.

## Parameters

| Parameter | Description                  |
|-----------|------------------------------|
| VA        | Name of input vector VA.     |
| INCA      | VA index increment.          |
| B         | Scalar B.                    |
| VC        | Name of output vector VC.    |
| INCC      | VC index increment.          |
| NN        | Element count.               |

## Listing

```
SUBROUTINE VSADDXX(VA,INCA,B,VC,INCC,NN,*)
DIMENSION VA(1),VC(1)
IA = 1
IC = 1
DO FOR M = 1,NN
  VC(IC) = B + VA(IA)
  IA = IA + INCA
  IC = IC + INCC
ENDDO
RETURN 1
END
```

---

## Page 57

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## 4.33 VECTOR SCALAR MULTIPLY (VSMULXX)

### Format

VSMULXX(VA,INCA,B,VC,INCC,NN,*)
Ident (R:I) : 033:177517B

### Explanation

Multiply the elements of a vector with a scalar value.
VCn = VAn * b, where 'b' denotes the scalar, and 'n' denotes the element index.

### Parameters

| Parameter | Description                     |
|-----------|---------------------------------|
| VA        | Name of input vector VA.        |
| INCA      | VA index increment.             |
| B         | Scalar B.                       |
| VC        | Name of output vector VC.       |
| INCC      | VC index increment.             |
| NN        | Element count.                  |

### Listing

```
SUBROUTINE VSMULXX(VA,INCA,B,VC,INCC,NN,*)
DIMENSION VA(1),VC(1)
IA = 1
IC = 1
DO FOR M = 1,NN
    VC(IC) = B * VA(IA)
    IA = IA + INCA
    IC = IC + INCC
ENDDO
RETURN 1
END
```

ND-05.013.03

---

## Page 58

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

### 4.34 VECTOR SCALAR DIVIDE (VDIVSXX)

**Format**

VDIVSXX(VA,INCA,B,VC,INCC,NN,*) Ident (R:I) : 034:177517B

**Explanation**

Form a vector from a scalar value divided with the elements of another vector. VCₙ = B/VAₙ, 'b' denotes the scalar and 'n' denotes the element index.

**Parameters**

| Parameter | Description                  |
|-----------|------------------------------|
| VA        | Name of input vector VA.     |
| INCA      | VA index increment.          |
| B         | Scalar B.                    |
| VC        | Name of output vector VC.    |
| INCC      | VC index increment.          |
| NN        | Element count.               |

**Listing**

```
SUBROUTINE VDIVSXX(VA,INCA,B,VC,INCC,NN,*)
DIMENSION VA(1),VC(1)
IA = 1
IC = 1
DO FOR M = 1,NN
  VC(IC) = B / VA(IA)
  IA = IA + INCA
  IC = IC + INCC
ENDDO
RETURN 1
END
```

---

ND-05.013.03

*Scanned by Jonny Oddene for Sintran Data © 2011*

---

## Page 59

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## 4.35 DOT PRODUCT (DOTPRXX)

### Format

DOTPRXX(VA,INCA,VB,INCB,VC,NN,*)

Ident (R:I) : 035:177517B

### Explanation

Add the product of the corresponding elements of two vectors. This function corresponds to the mathematical dot product, also called scalar product, of two vectors.

VC = VA₁ * VB₁ + VA₂ * VB₂ + .... + VAnn * VBnn

'nn' is the element count.

### Parameters

| Parameter | Description                  |
|-----------|------------------------------|
| VA        | Name of input vector VA.     |
| INCA      | VA index increment.          |
| VB        | Name of input vector VB.     |
| INCB      | VB index increment.          |
| VC        | Name of output scalar VC.    |
| NN        | Element count.               |

### Listing

```
SUBROUTINE DOTPRXX(VA,INCA,VB,INCB,VC,NN,*) 
DIMENSION VA(1),VB(1) 
IA = 1 
IB = 1 
SUM = 0.0 
DO FOR M = 1,NN 
   SUM = SUM + VA(IA) * VB(IB) 
   IA = IA + INCA 
   IB = IB + INCB 
ENDDO 
VC=SUM 
RETURN 1 
END
```

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 60

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## 4.36 VECTOR CLEAR (VCLRXXX)

### Format

VCLRXXX(VC, INCC, NN, \*)  
Ident (R:I) : 036:177517B

### Explanation

Set the elements of a vector to all zeros.

### Parameters

| Parameter | Description              |
|-----------|--------------------------|
| VC        | Name of output vector VC.|
| INCC      | VC index increment.      |
| NN        | Element count.           |

### Listing

    SUBROUTINE VCLRXXX(VC, INCC, NN, \*)
    DIMENSION VC(1)
    IC = 1
    DO FOR M = 1, NN
        VC(IC) = 0.0
        IC = IC + INCC
    ENDDO
    RETURN 1
    END

ND-05.013.03

---

## Page 61

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS  
ARRAY PROCESSING FUNCTIONS

## 4.37 CONVOLUTION (CONVXXX)

### Format

CONVXXX(VA,INCA,VB,INCB,VC,INCC,NC,NB,*) | Ident (R:I) : 037:177517B  
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;Ident (R:I) : 067:177517B

### Explanation

Perform a convolution or correlation operation on two vectors. The general equation for the output coefficients in vector VC is:

    VC_ic = Σ (VA_ic+ib * VB_ib)
    ib=0 to NB-1

`ib` and `ic` denote indices for VB and VC.

### Parameters

| Parameter | Description                                   |
|-----------|-----------------------------------------------|
| VA        | Name of input vector VA, operand.             |
| INCA      | VA index increment.                           |
| VB        | Name of input vector VB, operator.            |
| INCB      | VB index increment.                           |
| VC        | Name of output vector VC.                     |
| INCC      | VC index increment.                           |
| NC        | Element count for vector VC.                  |
| NB        | Element count for vector VB.                  |

> **NOTE:** The element count for vector VA must be: NB+NC-1.

### Listing

    SUBROUTINE CONVXXX(VA,INCA,VB,INCB,VC,INCC,NC,NB,*)
    DIMENSION VA(1),VB(1),VC(1)
    DO FOR N = 0,NC-1
        IC = N*INCC+1
        SUM = 0.0
        DO FOR M = 0,NB-1
            IA = (N+M) * INCA + 1
            IB = M * INCB + 1
            SUM = SUM + VA(IA)*VB(IB)
        ENDDO
        VC(IC) = SUM
    ENDDO
    RETURN 1
    END

&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;ND-05.013.03

--- 

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 62

# Performance

The execution time for the function depends on the element counts of NB and NC. Approximate execution time formulas (ftime) for the function are as follows.

## ND-560/1:

ftime \[microsec.\] = NC \* (1.35 \* (NB-2) + 5.7) , NB ≤ 1000.

ftime \[microsec.\] = NC \* (2.30 \* NB + 4.0) , NB > 1000.

## ND-570/2:

ftime \[microsec.\] = NC \* (0.88 \* (NB-2) + 3.0) , NB ≤ 1000.

ftime \[microsec.\] = NC \* (1.26 \* NB + 2.0) , NB > 1000.

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 63

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS  
## ARRAY PROCESSING FUNCTIONS

### 4.38 COMPLEX VECTOR MULTIPLY (CVMULXX)

#### Format

CVMULXX(VA,INCA,VB,INCB,VC,INCC,NN,NF,\*) Ident (R:I) : 040:177517B

#### Explanation

Multiply two complex vectors. This function corresponds to mathematical multiplication of complex numbers. An own flag selects whether the result should be conjugated or not. VA = VAr + VAi, VB = VBr + VBi.

If the conjugate flag ≥ 0 then:

VC = (VAr \* VBr - VAi\*VBi)r + (VAr\*VBi + VAi\*VBr)i, else:

VC = (VAr \* VBr - VAi\*VBi)r - (VAr\*VBi + VAi\*VBr)i.

'r' and 'i' denotes real and imaginary elements.

#### Parameters

| Parameter | Description                      |
|-----------|----------------------------------|
| VA        | Name of input vector VA.         |
| INCA      | VA index increment.              |
| VB        | Name of input vector VB.         |
| INCB      | VB index increment.              |
| VC        | Name of output vector VC.        |
| INCC      | VC index increment.              |
| NN        | Element count.                   |
| NF        | Conjugate flag.                  |

NF = +1 : Normal complex multiply.  
NF = -1 : Multiply with conjugate of VA.

#### Listing

    SUBROUTINE CVMULXX(VA,INCA,VB,INCB,VC,INCC,NN,NF,*)  
    DIMENSION VA(1),VB(1),VC(1)  
    IA = 1  
    IB = 1  
    IC = 1  
    DO FOR M = 1,NN  
    VC(IC) = VA(IA)\*VB(IB)-VA(IA+1)\*VB(IB+1)\*NF  
    VC(IC+1) = VA(IA)\*VB(IB+1)+VA(IA+1)\*VB(IB)\*NF  
    IA = IA + INCA  
    IB = IB + INCB  
    IC = IC + INCC  
    ENDDO  
    RETURN 1  
    END  

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 64

# 4.39 Complex Fast Fourier Transform (CFFTXXX)

## Format

CFFTXXX(C,N,LF,*) 

| Ident (R:I) | 054:177517B |
|-------------|-------------|
| Ident (R:I) | 055:177517B |
| Ident (R:I) | 056:177517B |
| Ident (R:I) | 057:177517B |
| Ident (R:I) | xxx:177516B |

## Parameters

- **C** : Name of complex input and output vector VC.
- **N** : Complex element count, *in power of 2*.
- **LF** : Direction flag.

LF = +1 : Forward FFT of vector VC.  
LF = -1 : Reverse FFT of vector VC.

## Explanation

To perform an in-place **complex** forward, or an inverse Fast Fourier Transform (FFT).

Symbolically the forward FFT can be represented by the block diagram:

```
    X₁         N₁
    X₂         NX₂
    .    FFT   .
    .  ---->   .
    Xn        NXn
```

The 'X' denotes the complex input coefficients to the vector VC, and 'Nx' denotes the complex output coefficients in vector VC. 'n' is the element count.

---

**NOTE:** The output coefficients should be multiplied with 1/N for properly scaling.

---

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 65

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## ARRAY PROCESSING FUNCTIONS

Symbolically the inverse FFT can be represented by the block diagram:

```
x₁       |          |     X₁
x₂ ----> |  FFT⁻¹   | ---> X₂
.        |          |     .
xn                      Xn
```

The 'x' denotes the complex input coefficients to the vector VC, and 'X' denotes the complex output coefficients in vector VC. 'n' is the element count. The output coefficients are properly scaled.

A series of radix 2 passes is used to obtain the coefficients.

A sine table is used to find sine and cosine, instead of calculating them. In this way, the routine is improved with respect to execution time. This is done for element count up to 65536 (2¹⁶). The sine table covers angles from 0 to π/2.

---

## Page 66

# ND-500 Single Precision Array Processing Functions

## Array Processing Functions

### Listing

```
SUBROUTINE CFFTXXX(C,N,LF,*)
DIMENSION M(20)
COMPLEX C(N)
COMPLEX WK,HOLD,Q,CFN
FN = FLOAT(N)
F = FLOAT(LF)
X = ALOG2(FN)
N2 = NINT(X)

C MAX ELEMENT COUNT CFFT.

IF (N2 .GT. 20) STOP
DO 10 I = 1,N2
10 M(I) = 2**(N2-I)
FPX = F * 6.283185308 / FN
DO 40 L = 1,N2
   NBLOCK = 2**(L-1)
   LBLOCK = N / NBLOCK
   LBHALF = LBLOCK / 2
   K = 0
   DO 40 IBLOCK = 1,NBLOCK
      FK = K
      V = FPX * FK
      COSV = COS(V)
      SINV = SIN(V)
      WK = CMPLX (COSV,SINV)
      ISTART = LBLOCK * (IBLOCK-1)
      DO 20 I = 1,LBHALF
         J = ISTART + I
         JH = J + LBHALF
         Q = C(JH) * WK
         C(JH) = C(J) - Q
         C(J) = C(J) + Q
20    CONTINUE
   DO 30 I = 2,N2
      II = I
      IF (K .LT. M(I)) GO TO 40
30    K = K - M(I)
40 K = K + M(II)
```

ND-05.013.03

*Scanned by Jonny Oddene for Sintran Data © 2011*

---

## Page 67

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## ARRAY PROCESSING FUNCTIONS

#### C REORDERING THE TRANSFORM:

```  
K = 0  
DO 70 J = 1,N  
   IF (K .LT. J) GO TO 50  
   HOLD = C(J)  
   C(J) = C(K+1)  
   C(K+1) = HOLD  
50 DO 60 I = 1,N2  
   II = I  
   IF (K .LT. M(II)) GO TO 70  
60 K = K - M(I)  
70 K = K + M(II)  
   IF (F .LT. 0.0) RETURN 1  
C INVERSE TRANSFORM  

   CFN = CMPLX (FN,0.0)  
   DO 80 I = 1,N  
80 C(I) = C(I) / CFN  
   RETURN 1  
   END  
```

## Performance

This table for CFFT function provides version C or newer of the APF library.

| Number of elements | Improvement ratio to FORTRAN | Typical execution time pr. loop (millisec.) |
|--------------------|------------------------------|---------------------------------------------|
|                    | ND-560/1 | ND-570/2 | ND-560/1 | ND-570/2 |
| 32                 | 4.35     | 3.66     | 1.26     | 0.83     |
| 64                 | 4.25     | 3.57     | 2.80     | 1.84     |
| 128                | 4.15     | 3.55     | 6.10     | 4.00     |
| 256                | 4.00     | 3.50     | 13.55    | 8.77     |
| 512                | 3.95     | 3.38     | 29.25    | 19.25    |
| 1024               | 3.80     | 3.30     | 63.75    | 41.88    |
| 2048               | 3.75     | 2.80     | 138.00   | 91.00    |
| 4096               | 3.65     | 2.73     | 297.50   | 240.00   |
| 8192               | 3.55     | 2.70     | 640.00   | 520.00   |
| 16384              | 3.35     | 2.70     | 1460.00  | 1100.00  |
| 32768              | 3.30     | 2.68     | 3140.00  | 2360.00  |
| 65536              | 3.25     | 2.68     | 6700.00  | 4960.00  |

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 68

# References

Among many references on FFT are:

G. D. Bergland: "A Guided Tour of the Fast Fourier Transform"  
IEEE Spectrum, July 1969.

E. O. Brigham: "The Fast Fourier Transform"  
Prentice-Hall, Englewood Cliffs, 1974.

---

ND-05.013.03

*Scanned by Jonny Oddene for Sintran Data © 2011*

---

## Page 69

# 4.40 Real Fast Fourier Transform (RFFTXXX)

## Format

```
RFFTXXX(C,N,LF,*)                  Ident (R:I) : 061:177517B
                                   Ident (R:I) : 062:177517B
```

## Parameters

| Parameter | Description                                         |
|-----------|-----------------------------------------------------|
| C         | Name of complex input and output vector VC.         |
| N         | Complex element count, in power of 2.               |
| LF        | Direction flag.                                     |

### LF Values

| LF  | Description                        |
|-----|------------------------------------|
| +1  | Forward FFT of vector VC.          |
| -1  | Reverse FFT of vector VC.          |

## Explanation

To perform an in-place real to complex forward, or complex to real inverse Fast Fourier Transform (FFT).

Symbolically the forward FFT can be represented by the block diagram:

```
   X1            2NX1
   X2            2NX2
   .      FFT    .
   .   ----->    .
   Xn            2NXn
```

The 'X' denotes the real input coefficients to the vector VC, and '2NX' denotes the complex output coefficients in vector VC. 'n' is the element count.

> **NOTE:** The output coefficients should be multiplied with 1/2N for properly scaling.

ND-05.013.03

---

## Page 70

# ND-500 Single Precision Array Processing Functions

## Array Processing Functions

Symbolically the inverse FFT can be represented by the block diagram:

```
 x₁           x₁
 x₂           x₂
 .      FFT⁻¹  .
 .   ———>  ———> .
 xn           Xn
```

The 'x' denotes the complex input coefficients to the vector VC, and 'X' denotes the real output coefficients in vector VC. 'n' is the complex element count. The output coefficients are properly scaled.

The RFFTKXX routine utilizes the CFFTXXX routine, refer to section 4.39.

---

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 71

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS
## ARRAY PROCESSING FUNCTIONS

### Listing

    SUBROUTINE RFFTXXX(C,N,LF,*)
    COMPLEX C(1)
    COMPLEX UC,VC,UC2,VC2,WK,WK2,CFN
    F = FLOAT(LF)
    NHALF = N / 2
    NFOUR = N / 4
    VK = F*3.141592654 / NHALF
C Using the original CFFT routine.
    CFFTXXX(C,NHALF,LF,*910)
910 CONTINUE

    IF (F .LT. 0.0) THEN
        FDIV1 = 1.0
        FDIV2 = 2.0
    ELSE
        FDIV1 = 2.0
        FDIV2 = 4.0
    ENDIF
  
    URE = REAL(C(1))
    VRE = AIMAG(C(1))
    CRE = (URE+VRE) / FDIV1
    CIM = (URE-VRE) / FDIV1
    C(1) = CMPLX (CRE,CIM)
    URE = (REAL(C(NFOUR+1))) / FDIV1
    VRE = (F*AIMAG(C(NFOUR+1))) / FDIV1
    C(NFOUR+1) = CMPLX (URE,VRE)
  
    IRX = NHALF + 2
    DO 200 IR = 2 , NFOUR
        IR2 = IRX - IR
        FIR = IR - 1
      
        URE = (REAL(C(IR)) + REAL(C(IR2))) / FDIV2
        UIM = (AIMAG(C(IR)) - AIMAG(C(IR2))) / FDIV2
        VRE = (AIMAG(C(IR)) + AIMAG(C(IR2))) / FDIV2
        VIM = (REAL(C(IR2)) - REAL(C(IR))) / FDIV2
      
        UC = CMPLX (URE,UIM)
        VC = CMPLX (VRE,VIM)
        UC2 = CONJG (UC)
        VC2 = CONJG (VC)
      
        V = VK * FIR
        COSV = COS(V)
        SINV = SIN(V)
        WK = CMPLX (COSV,SINV)
        WK2 = CMPLX (-COSV,SINV)
      
        C(IR) = UC + VC * WK
        C(IR2) = UC2 + VC2 * WK2
200 CONTINUE
    RETURN 1
    END

---

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 72

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS
## ARRAY PROCESSING FUNCTIONS

### Performance

This table for RFFT function provides version C or newer of the APF library.

| Number of elements | Improvement ratio to FORTRAN | | | Typical execution time pr. loop (millisec.) | |
|--------------------|-------------------------------|-----|-----|--------------------------------------------|-----|
|                    | ND-560/1                      | ND-570/2 | | ND-560/1                                   | ND-570/2 |
| 32                 | 2.20                          | 2.10 | | 1.40                                       | 0.82 |
| 64                 | 2.20                          | 2.10 | | 3.00                                       | 1.77 |
| 128                | 2.25                          | 2.10 | | 6.30                                       | 3.76 |
| 256                | 2.30                          | 2.10 | | 13.20                                      | 8.05 |
| 512                | 2.30                          | 2.10 | | 27.80                                      | 17.00 |
| 1024               | 2.30                          | 2.15 | | 58.00                                      | 35.80 |
| 2048               | 2.30                          | 2.15 | | 121.00                                     | 74.50 |
| 4096               | 2.30                          | 2.15 | | 252.50                                     | 156.25 |
| 8192               | 2.30                          | 2.05 | | 530.00                                     | 375.00 |
| 16384              | 2.35                          | 2.05 | | 1090.00                                    | 780.00 |
| 32768              | 2.30                          | 2.05 | | 2360.00                                    | 1640.00 |
| 65536              | 2.30                          | 2.05 | | 4940.00                                    | 3420.00 |

For references on FFT, see page 60.

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 73

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## ARRAY PROCESSING FUNCTIONS

### 4.41 VECTOR TAPER (VTAPERX)

#### Format

VTAPERX(VA,VC,NN,IFLAG,*)   Ident (R:I) : 041:177517B

#### Explanation

Multiply each element of a vector with an increasing or decreasing factor. An own flag selects either the decreasing or the increasing factor. The factor is a function of the element count.

If flag > 0 then:

VC<sub>1</sub> = VA<sub>1</sub> * (1/nn), VC<sub>2</sub> = VA<sub>2</sub> * (2/nn) .. VCnn = VAnn * (1).

So the general element equation is: VCn = VAn * (n/nn).

If flag ≤ 0 then:

VC<sub>1</sub> = VA<sub>1</sub> * (1 - 1/nn), VC<sub>2</sub> = VA<sub>2</sub> * (1 - 2/nn) .. VCnn = VAnn * (0).

So the general element equation is: VCn = VAn * (1 - n/nn).

'nn' denotes the element count and 'n' the element index.

#### Parameters

| Parameter | Description                  |
| --------- | ---------------------------- |
| VA        | Name of input vector VA.     |
| VC        | Name of output vector VC.    |
| NN        | Element count.               |
| IFLAG     | Flag.                        |

#### Listing

```
SUBROUTINE VTAPERX(VA,VC,NN,IFLAG,*)
DIMENSION VA(1),VC(1)
REAL MULT,MINC
IF (IFLAG .LE. 0) GO TO 10
MULT = 1.0/NN
MINC = MULT
GO TO 20
10 CONTINUE
MULT = (NN-1)*1.0/NN
MINC = -1.0/NN
20 CONTINUE
DO 30 I = 1,NN
   WC(I) = VA(I) * MULT
   MULT = MULT + MINC
30 CONTINUE
RETURN 1
END
```

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 74

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS  
ARRAY PROCESSING FUNCTIONS  

## 4.42 WIENER FILTER (WIENERX)

### Format

```
WIENERX(LR,R,G,F,A,ISW, WNF, LEC, *)   Ident (R:I) : 047:177517B
                                      Ident (R:I) : 050:177517B
```

### Explanation

To find the so-called single solution channel normal equations, by using the Toeplitz recursive algorithm.

Further description of the algorithm is given in Silva & Robinson: Deconvolution of geophysical time series in the exploration for oil and natural gas. 1979.

### Parameters

| Parameter | Description |
|-----------|-------------|
| LR        | Length of filter. Element count. |
| .R        | Auto correlation coefficients: R(1), R(2), ..., R(LR). |
| G         | Right-hand side coefficients: G(1), G(2), ..., G(LR). |
| F         | Filter coefficients: F(1), F(2), ..., F(LR). |
| A         | Prediction error operators: A(1), A(2), ..., A(LR). |
| ISW       | Flag. |
| WNF       | White noise factor (not used). |
| LEC       | Loop on error count. Output parameter. |

ISW = +1: General algorithm.  
ISW = 0: Only prediction error operators as results.

### Listing

```
SUBROUTINE WIENERX(LR,R,G,F,A,ISW,WNF,LEC,*)
DIMENSION R(LR),G(LR),F(LR),A(LR)
IFLAG = 0
V = R(1)
D = R(2)
A(1) = 1.0
F(1) = G(1)/V
Q = F(1)*R(2)
DO 600 L = 2,LR
  A(L) = -D/V
  AL = A(L)
  IF (V .LE. 0.0) THEN
    LEC = L
    RETURN 1
  ENDIF
  IF (ISW .EQ. 0) F(L)=V
  V = V + AL * D
  D = R(L+1) + AL * R(2)
  L2 = L/2
  IF (L .LE. 3) GO TO 150
```

ND-05.013.03

---

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 75

# ND-500 Single Precision Array Processing Functions

## Array Processing Functions

```
DO 100 J = 2,L2
   K = L - J + 1
   HOLD = A(J)
   A(J) = A(J) + AL * A(K)
   D = D + A(J) * R(K+1)
   A(K) = A(K) + AL * HOLD
100 D = D + A(K) * R(J+1)
150 IF (2*L2 .EQ. L) GO TO 200
   LH = L2 + 1
   A(LH) = A(LH) + AL * A(LH)
   D = D + A(LH) * R(LH+1)
200 IF (ISW .EQ. 0) GO TO 600
   F(L) = (G(L) - Q)/V
   FL = F(L)
   L1 = L - 1
   Q = FL * R(2)
DO 300 J = 1,L1
   K = L - J + 1
   F(J) = F(J) + FL * A(K)
300 Q = Q + F(J) * R(K+1)
600 CONTINUE
   RETURN 1
   END
```

---

ND-05.013.03

*Scanned by Jonny Oddene for Sintran Data © 2011*

---

## Page 76

# 4.43 VECTOR GENERATE (VGENXXX)

## Format

VGENXXX(SCALAR, SCINC, VC, INCC, NN, \*)

Ident (R:I) : 043:177517B

## Explanation

Form a vector as a ramp function with a start value and a slope as input parameters.

VC₁ = sc + scinc, VC₂ = sc + 2 * scinc .. VCnn = sc + nn * scinc.

So the general element expression is: VCn = sc + n * scinc.

'nn' denotes the element count, 'n' the element index, 'sc' start value, and 'scinc' slope.

## Parameters

| Parameter | Description                      |
|-----------|----------------------------------|
| SCALAR    | Scalar for start value.          |
| SCINC     | Scalar for increment.            |
| VC        | Name of output vector VC.        |
| INCC      | VC index increment.              |
| NN        | Element count.                   |

## Listing

```
SUBROUTINE VGENXXX(SCALAR, SCINC, VC, INCC, NN, \*)
DIMENSION V(1)
IC = 1
SC = 0.0
DO FOR I = 1, NN
  VC(IC) = SCALAR + SC
  IC = IC + INCC
  SC = I * SCINC
ENDDO
RETURN 1
END
```

---

## Page 77

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## ARRAY PROCESSING FUNCTIONS

### 4.44 VECTOR LINEAR INTERPOLATION (VMNOXXX)

#### Format

VMNOXXX(VA,VB,VR,LA,LB,*)  
Ident (R:I) : 044:177517B

#### Explanation

Perform linear interpolation between samples.

#### Parameters

| Parameter | Description                  |
|-----------|------------------------------|
| VA        | Name of input vector VA.     |
| VB        | Name of input vector VB.     |
| VR        | Name of output vector VR.    |
| LA        | Element count vector VA.     |
| LB        | Element count vector VB.     |

#### Listing

```
SUBROUTINE VMNOXXX(VA,VB,VR,LA,LB,*)
DIMENSION VA(1),VB(1),VR(1)
IFLAG = 0
LA1 = LA + 1
DO 20 M = 1,LB
  IF (VB(M) .GT. 0.0) GO TO 21
  VR(M) = 0.0
21 CONTINUE:
  IF (M .GT. 1) GO TO 40
  DO 30 M = 1,LB
    IF ((VB(M+1)-VB(M)) .NE. 0.0) GO TO 40
30  VR(M) = 0.0
40 CONTINUE:
  L = M
  DO 100 M = L,LB
    IF(VB(M).GT.LA1)THEN
      VR(M) = 0.0
      GO TO 100
    ENDIF
    IF (IFLAG .EQ. 1) GO TO 70
    IF (M  .EQ. LB) GO TO 70
    IF (VB(M+1) .LT. VB(M)) THEN
      DO 60 I = 1,M
60      VR(I) = 0.0
      IFLAG = 1
      GO TO 100
    ENDIF
```

---

## Page 78

# ND-500 Single Precision Array Processing Functions

## Array Processing Functions

    70    CONTINUE
          IB = INT(VB(M))
          FB = VB(M) - IB
          IF (IB .LE. 0) THEN
            VR(M) = 0.0
          ELSE
            VR(M) = (VA(IB+1) - VA(IB))*FB + VA(IB)
          ENDIF
    100   CONTINUE
          RETURN 1
          END

---

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 79

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## ARRAY PROCESSING FUNCTIONS

### 4.45 VECTOR SINC INTERPOLATION (VNVMOSXX)

**Format**

`VNVMOSXX(VA,VB,VR,LA,LB,*)` Ident `(R:I) : 045:177517B`

**Explanation**

Perform sinc interpolation between samples.

**Parameters**

| Parameter | Description                 |
|-----------|-----------------------------|
| VA        | Name of input vector VA.    |
| VB        | Name of input vector VB.    |
| VR        | Name of vector VR.          |
| LA        | Element count vector VA.    |
| LB        | Element count vector VB.    |

**Listing**

```
SUBROUTINE VNVMOSXX(VA,VB,VR,LA,LB,*)
DIMENSION VA(1),VB(1),VR(1)
DIMENSION FILTER(8,7)
DATA FILTER/
```

```
C    -0.00442400 ,  0.02585229 , -0.08848375 ,  0.97214937 ,
+     0.12341321 , -0.03566697 ,  0.00774525 , -0.00007569 ,
C
+    -0.00583580 ,  0.0402596  , -0.14005053 ,  0.89166689 ,
+     0.27481657 , -0.07685405 ,  0.01818759 , -0.00057665 ,
C
+    -0.00514438 ,  0.04393956 , -0.15734833 ,  0.76733017 ,
+     0.44274765 , -0.11675274 ,  0.02960985 , -0.00174663 ,
C
+    -0.00346141 ,  0.03929961 , -0.14670730 ,  0.61239052 ,
+     0.61239052 , -0.14670724 ,  0.03929964 , -0.00346142 ,
C
+    -0.001746630,  0.02960985 , -0.11675292 ,  0.44274879 ,
+     0.76732922 , -0.15734845 ,  0.04393965 , -0.00514441 ,
C
+    -0.00057664 ,  0.01818759 , -0.07685423 ,  0.27481735 ,
+     0.89166594 , -0.14005071 ,  0.04025969 , -0.00583582 ,
C
+    -0.00007568 ,  0.00774526 , -0.03566715 ,  0.12341386 ,
+     0.97214890 , -0.08848411 ,  0.02585241 , -0.00442402 /
```

```
DATA NFPTS /8/
IFLAG = 0
LA1 = LA + 1
DO 20 M = 1, LB
```

```
ND-05.013.03
Scanned by Jonny Oddene for Sintran Data © 2011
```

---

## Page 80

# ND-500 Single Precision Array Processing Functions

## Array Processing Functions

```
IF (VB(M) .GT. 0.0) GO TO 21
VR(M) = 0.0
```

### 21 Continue

```
IF (M .GT. 1) GO TO 40
DO 30 M = 1,LB
IF ((VB(M+1)-VB(M)) .NE. 0.0) GO TO 40
VR(M) = 0.0
```

### 40 Continue

```
L = M
DO 100 M = L,LB 
IF(VB(M) .GT. LA1) THEN 
  VR(M) = 0.0
  GO TO 100
ENDIF
IF (IFLAG .EQ. 1) GO TO 70
IF (M .EQ. LB) GO TO 70
IF (VB(M+1) .LT. VB(M)) THEN
  DO 60 I = 1,M
    VR(I) = 0.0
  IFLAG = 1
  GO TO 100
ENDIF
```

### 70 Continue

```
IB = INT(VB(M))
FB = VB(M) - IB
IF((IB-3) .GT. 0 .AND. (IB+4) .LE. LA) THEN
  NRFILT= .5 + FB/.125
  GO TO (1,2,2,2,2,2,2,3),NRFILT+1
1 VR(M) = VA(IB)
  GO TO 4
3 VR(M) = VA(IB+1)
  GO TO 4
2 VR(M) = FDOTPR(VA(IB-3),FILTER(1,NRFILT),NFPTS)
4 CONTINUE
ELSEIF (IB .LE. 0) THEN
  VR(M) = 0.0
ELSEIF (M .EQ. LB) THEN
  VR(M) = VA(IB)
ELSE
  VR(M) = (VA(IB+1) - VA(IB))*FB + VA(IB)
ENDIF
```

### 100 Continue

```
RETURN 1
END
```

## Function FDOTPR(VA,VB,N)

```
DIMENSION VA(1),VB(1)
FDOTPR = 0.0
DO FOR I = 1,N
  FDOTPR = FDOTPR + VA(I)*VB(I)
ENDDO
RETURN
END
```

---

*ND-05.013.03*

*Scanned by Jonny Oddene for Sintran Data © 2011*

---

## Page 81

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS
## ARRAY PROCESSING FUNCTIONS

### 4.46 VECTOR EXPAND (VXPNDXX)

**Format**

VXPNDXX(VA,VC,NN,NC,*) Ident (R:I) : 052:177517B

**Explanation**

Expand input vector VA into output vector VC.

Vector VA must be organized in this way:

VA₀, VA₁, VA₂, ..., VAn contain the arguments for the function in increasing order. VA₀, VA₂, VA₄, ..., VAn contain the corresponding function values. The argument in VA₁ must be greater or equal to 1.0.

This is also expressed as: VA₂i = f(VA₂i-1), for i ≤ nn/2.

'nn' denotes the element count.

After execution, VC contains the new function values approximated for the arguments 1,2,3,4, ..., VC. The approximation method is based on linear interpolation. Remember that VC must be large enough to contain the number of elements specified by VCnn-1.

**Parameters**

| Parameter | Description                     |
|-----------|---------------------------------|
| VA        | Name of input vector.           |
| VC        | Name of result vector.          |
| NN        | Element count in input vector.  |
| NC        | Element count in result vector. |

**Listing**

```
SUBROUTINE VXPNDXX(VA,VC,NN,NC,*)
DIMENSION VA(1),VC(1)
IFIRST = 1
LOOP = NN - 3
DO 100 I = 1,LOOP,2
  SLOPE = (VA(I+3)-VA(I+1))/((VA(I+2)-VA(I))
  ILAST = VA(I+2)
  RINC = SLOPE*(IFIRST-VA(I))
  DO 50 J = IFIRST,ILAST
    VC(J) = VA(I+1)+RINC
    RINC = RINC+SLOPE
50 CONTINUE
  IFIRST = ILAST+1
100 CONTINUE
NC = VA(NN-1)
RETURN
END
```

---

## Page 82

# 4.47 VECTOR FIRST AND LAST NON-ZERO VALUE (VFLNZXX)

## Format

VFLNZXX(VA,XINDF,XINDL,NN,*)  
Ident (R:I) : 051:177517B

## Explanation

Find the indices of the first and last non-zero elements in a vector.

## Parameters

| Parameter | Description                      |
|-----------|----------------------------------|
| VA        | Name of input vector VA.         |
| XINDF     | Name for first non-zero index.   |
| XINDL     | Name for last non-zero index.    |
| NN        | Element count.                   |

## Listing

```
SUBROUTINE VFLNZXX(VA,XINDF,XINDL,NN,*)
DIMENSION VA(1)
DO 100 I = 1,NN
  IF (VA(I) .NE. 0.0) THEN
    XINDF = I
    GO TO 105
  ENDIF
100 CONTINUE
XINDF = NN
XINDL = 1
GO TO 205
105 CONTINUE
DO 200 I = NN,1,-1
  IF (VA(I) .NE. 0.0) THEN
    XINDL = I
    GO TO 205
  ENDIF
200 CONTINUE
205 RETURN 1
END
```

---

## Page 83

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS  
## ARRAY PROCESSING FUNCTIONS

### 4.48 VECTOR SCALAR MULTIPLY AND ADD (VSMADDX)

#### Format

VSMADDX(VA,INCA,SC,VB,INCB,VC,INCC,NN,*)  
Ident (R:I) : 053:177517B

#### Explanation

Add the corresponding elements from two vectors, where the elements of one of the vectors are multiplied with a scalar value.  
VCn = VAn * sc + VBn, where 'sc' denotes the scalar, and 'n' denotes the element index.

#### Parameters

| Parameter | Description                  |
|-----------|------------------------------|
| VA        | Name of input vector VA.     |
| INCA      | VA index increment.          |
| SC        | Scalar value.                |
| VB        | Name of input vector VB.     |
| INCB      | VB index increment.          |
| VC        | Name of output vector VC.    |
| INCC      | VC index increment.          |
| NN        | Element count.               |

#### Listing

```
SUBROUTINE VSMADDX(VA,INCA,SC,VB,INCB,VC,INCC,NN,*)
DIMENSION VA(1),VB(1),VC(1)
IA = 1
IB = 1
IC = 1
DO FOR I = 1,NN
   VC(IC)= VA(IA) * SC + VB(IB)
   IA = IA + INCA
   IB = IB + INCB
   IC = IC + INCC
ENDDO
RETURN 1
END
```

ND-05.013.03

---

## Page 84

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## 4.49 PREDICT (PREDICT)

### Format

PREDICT(VA,VB,N,L,*)  
Ident (R:I) : 046:177517B

### Explanation

Form the vector VB with VB₁ equal to 1.0, the next elements VB₂, VB₃, ..., VBₙ equal to 0.0, and the elements VBₗ₊₁, VBₗ₊₂, ..., VBₗ₊ₙ elements equal to -VA₁, -VA₂, ..., -VAₙ.

Note that the elements in VA also are multiplied with -1.

### Parameters

| Parameter | Description                                  |
|-----------|----------------------------------------------|
| VA        | Name of input vector VA.                     |
| VB        | Name of output vector VB.                    |
| N         | Element count for number of negative elements. |
| L         | Element count for number of zero elements. Element count of VB must be at least: N + L. |

### Listing

    SUBROUTINE PREDICT(VA,VB,N,L,*)
    DIMENSION VA(1),VB(1)
    DO 100 I = 1,L
    100 VA(I) = (-1.0)*VA(I)
    VB(1) = 1.0
    DO 200 I = 2,N
    200 VB(I) = 0.0
    DO 300 I = 1,L
    300 VB(I+N) = VA(I)
    RETURN 1
    END

ND-05.013.03

*Scanned by Jonny Oddene for Sintran Data © 2011*

---

## Page 85

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## ARRAY PROCESSING FUNCTIONS

### 4.50 DISPLAY PROCESSOR FUNCTION (XBTMUX)

#### Format

```
XBTMUX(IBSWTH, ISCANS, NDOTS,*)          Ident (R:I) : 001:177507B
```

#### Explanation

Put on proper raster bits for 32 scan lines for a display processor. A swath is converted to a scan.

#### Parameters

| **Parameter** | **Description**                     |
|---------------|-------------------------------------|
| IBSWTH        | Name of input vector IBSWTH.        |
| ISCANS        | Name of output vector ISCANS.       |
| NDOTS         | Element count.                      |

#### Listing

```
SUBROUTINE XBTMUX (IBSWTH, ISCANS, NDOTS, *)
INTEGER*4 IBSWTH(1)
INTEGER*4 ISCANS(1)
INTEGER*4 NDOTS
INTEGER*4 IMASK(32)
DATA IMASK/
  2000000000B, 1000000000B, 0400000000B, 0200000000B,
  0100000000B, 0040000000B, 0020000000B, 0010000000B,
  0004000000B, 0002000000B, 0001000000B, 0000400000B,
  0000200000B, 0000100000B, 0000040000B, 0000020000B,
  0000010000B, 0000004000B, 0000002000B, 0000001000B,
  0000000400B, 0000000200B, 0000000100B, 0000000040B,
  0000000020B, 0000000010B, 0000000004B, 0000000002B,
  0000000001B/

ISCAN = 1
IND = 1
10 CONTINUE
  MASK = IMASK(ISCAN)
  NBIT = 1
  ISWD = 0
  DO 100 N=1, NDOTS
    IBIT = IAND(IBSWTH(N), MASK)
    IF (IBIT.NE.0) ISWD=IOR(ISWD, IMASK(NBIT))
    NBIT = NBIT + 1
    IF (NBIT.LE.32) GOTO 100
    ISCANS(IND) = ISWD
    IND = IND + 1
    NBIT = 1
    ISWD = 0
100 CONTINUE
  ISCAN = ISCAN + 1
  IF (ISCAN.LE.32) GOTO 10
RETURN 1
END
```

ND-05.013.03

*Scanned by Jonny Oddene for Sintran Data © 2011*

---

## Page 86

# 4.51 IMAGE BUILD (IMGBLD)

## Format

IMGBLD(IBPOS, IBSET, NN, ISWTH, *) 
Ident (R:I) : 002:177507B

## Explanation

Raster 32 scans input.

## Parameters

| Name  | Description                                |
|-------|--------------------------------------------|
| IBPOS | Name of input/output vector IBPOS.         |
| IBSET | Name of input/output vector IBSET.         |
| NN    | Element count.                             |
| ISWTH | Name of output vector ISWTH.               |

## Listing

```
SUBROUTINE IMGBLD(IBPOS, IBSET, NN, ISWTH, *)
INTEGER*4 IBPOS(1)
INTEGER*4 IBSET(1)
INTEGER*4 NWDS
INTEGER*4 ISWTH(1)
INTEGER*4 IBIT(33)
DATA IBIT/ 
- 00000000000B, 
  30000000000B, 
  34000000000B, 
  36000000000B, 
  37000000000B, 
  37400000000B, 
  37600000000B, 
  37700000000B, 
  37740000000B, 
  37760000000B, 
  37770000000B, 
  37774000000B, 
  37776000000B, 
  37777000000B, 
  37777400000B, 
  37777600000B, 
  37777700000B, 
  37777740000B, 
  37777760000B, 
  37777770000B, 
  37777774000B, 
  37777776000B, 
  37777777000B, 
  37777777400B, 
  37777777600B, 
  37777777700B, 
  37777777740B, 
  37777777760B, 
  37777777770B, 
  37777777774B, 
  37777777776B, 
  37777777777B/

DO 100 N=1,NN
    NBITS = IBSET(N)
    IF (NBITS.LE.0) GOTO 100
    NBEG = IBPOS(N)
    IF (NBEG.LT.32) GOTO 50
    IBPOS(N) = NBEG - 32
    GOTO 100
50  CONTINUE
    NEND = NBEG+NBITS
    NSET = NBITS
    IF (NEND.LE.32) GOTO 60
    NSET = 32 - NBEG
    NEND = 32
```

ND-05.013.03

---

## Page 87

# ND-500 Single Precision Array Processing Functions

## Array Processing Functions

```
60 CONTINUE
   NP = NBEG + 1
   NS = NEND+1
   IPAT = IEOR(IBIT(NP),IBIT(NS))
   ISWTH(N) = IOR(IPAT,ISWTH(N))
   IBSET(N) = NBITS-NSET
   IBPOS(N) = 0
100 CONTINUE
   RETURN 1
END
```

ND-05.013.03

*Scanned by Jonny Oddene for Sintran Data © 2011*

---

## Page 88

# 4.52 CONVERT AND MOVE (APMOVE)

## Format

APMOVE(ADDR1,ADDR2,IFTM,NN,*)  
Ident (R:I) : 060:177517B

## Explanation

Convert input vector (pointed to by ADDR1) according to the format descriptor IFTM, and move the result to output vector (pointed to by ADDR2).

To get the pointer to a vector, the routine LOCARG must be called. It has the call format:

```
ADDR = LOCARG(V)
```

which means integer ADDR points at vector V after execution of the call. See also page 5, and the listing in the end of this section.

## Format Conversion

| IFTM | Description |
|------|-------------|
| 0 | Integer*4 (32 bits) into single floating point. |
| 1 | Integer*2 (16 bits) into single floating point. |
| 2 | 32 bits move operation. |
| 3 | Single floating point into integer*4 (32 bits). |
| 4 | Single floating point into integer*2 (16 bits). |
| ≥ 5 | 32 bits move operation. |

## Parameters

- **ADDR1** : Pointer to address of input vector.
- **ADDR2** : Pointer to address of output vector.
- **IFTM** : Format descriptor.
- **NN** : Element count.

## Listing

```
ROUTINE APMOVE  
DSTR: STACK FIXED  
APAR1: W BLOCK 1  
APAR2: W BLOCK 1  
APAR3: W BLOCK 1  
APAR4: W BLOCK 1  
COUNT: W BLOCK 1  
ENDSTACK

APMOVE: ENTR DSTR  
  W1 := IND(B.APAR1) %.. Get input address.  
  W2 := IND(B.APAR2) %.. Get output address.  
  W4 := IND(B.APAR4) %.. Get element count.  
  W3 := IND(B.APAR3) %.. Get format description.  
  IF -Z GO FMT1 %.. If IFTM = 0 : next.
```

ND-05.013.03

---

## Page 89

# ND-500 Single Precision Array Processing Functions

## Integer*4 to Single Float Convert and Move

```
%  Integer*4 to single float convert and move.
  W SET1 W3
LOOP0: W FCONV W1.0,W2.0; W1+4; W2+4; W LOOP1 W3,W4,LOOP0
  GO END
```

## IFTM Decrement

```
FMT1:  W DECR W3
  IF -Z GO FMT2
%.. IFTM = IFTM - 1.
%.. If IFTM = 0 : next.
```

## Integer*2 to Single Float Convert and Move

```
% Integer*2 to single float convert and move.
  W SET1 W3
LOOP1: H FCONV W1.0,W2.0; W1+2; W2+4; W LOOP1 W3,W4,LOOP1
  GO END
```

## Single Float Move

```
FMT2:  W DECR W3
  IF -Z GO FMT3
%.. IFTM = IFTM - 1.
%.. If IFTM = 0 : next.
% Single float move .
FMT5:  W BMOVE W1.0,W2.0,W4
  GO END
```

## Single Float to Integer*4 Convert and Move

```
FMT3:  W DECR W3
  IF -Z GO FMT4
%.. IFTM = IFTM - 1.
%.. If IFTM = 0 : next.
% Single float to integer*4 convert and move.
  W SET1 W3
LOOP3: F WCONV W1.0,W2.0; W1+4; W2+4; W LOOP1 W3,W4,LOOP3
  GO END
```

## End Routine

```
FMT4:  W DECR W3
  IF -Z GO FMT5
%.. IFTM = IFTM - 1.
%.. If IFTM = 0 : next, else
%.. IFTM = 5 is assumed.
% Single float to integer*2 convert and move.
  W SET1 W3
LOOP4: F HCONV W1.0,W2.0; W1+4; W2+2; W LOOP1 W3,W4,LOOP4
  GO END
END:  R := B.PREVB
  W MOVE 1,R.AUX
  RET
  ENDROUTINE
```

## Routine LOCARG

```
ROUTINE LOCARG
DSTK:  STACK FIXED
INARG: W BLOCK 1
  ENDSTACK

LOCARG: ENTF DSTACK
  W1 := B.INARG
  RET
  ENDROUTINE
```

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 90

# 4.53 DEMULTIPLEX (DMXB)

## Format

DMXB(INDAT, INGAIN, OUTPUT, IFIXGN, GNLOC, NBYSCN, NSAMP)

| Ident (R:I) | xxx:177505B |
|-------------|-------------|
| Ident (R:I) | xxx:177506B |

## Explanation

Demultiplex of field tape.

## Parameters

- **INDAT**: Name of input vector.
- **INGAIN**: Gain code buffer address.
- **OUTPUT**: Name of output vector.
- **IFIXGN**: Initial / early gain.
- **GNLOC**: Gain location 4 bit index.
- **NBYSCN**: Number of bytes in each scan.
- **NSAMP**: Element count.

GNLOC is an odd number: High order 4 bit group byte.  
GNLOC is an even number: Low order 4 bit group byte.

## Listing

```
ROUTINE DMXB
PARMS: STACK FIXED
INDAT: W BLOCK 1
INGAIN: W BLOCK 1
OUTPUT: W BLOCK 1
IFIXGN: W BLOCK 1
GNLOC: W BLOCK 1
NBYSCN: W BLOCK 1
NSAMP: W BLOCK 1

% Scaler for any given gain is 2**(GAIN*(-1)).

% Note that the least significant bit is not used
% and bit 1 corresponds to 0.5 millivolt.

GNTAB:
W DATA 077600000000B % Scaler for gain = 0
W DATA 077400000000B % Scaler for gain = 1
W DATA 077200000000B % Scaler for gain = 2
W DATA 077000000000B % Scaler for gain = 3
W DATA 076600000000B % Scaler for gain = 4
W DATA 076400000000B % Scaler for gain = 5
W DATA 076200000000B % Scaler for gain = 6
W DATA 076000000000B % Scaler for gain = 7
W DATA 075600000000B % Scaler for gain = 8
```

ND-05.013.03  
Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 91

# ND-500 Single Precision Array Processing Functions

## Array Processing Functions

| W DATA          | Description          |
|-----------------|----------------------|
| 07540000000B | % Scaler for gain = 9  |
| 07520000000B | % Scaler for gain = 10 |
| 07500000000B | % Scaler for gain = 11 |
| 07460000000B | % Scaler for gain = 12 |
| 07440000000B | % Scaler for gain = 13 |
| 07420000000B | % Scaler for gain = 14 |
| 07400000000B | % Scaler for gain = 15 |
| 07360000000B | % Scaler for gain = 16 |
| 07340000000B | % Scaler for gain = 17 |
| 07320000000B | % Scaler for gain = 18 |
| 07300000000B | % Scaler for gain = 19 |
| 07260000000B | % Scaler for gain = 20 |
| 07240000000B | % Scaler for gain = 21 |
| 07220000000B | % Scaler for gain = 22 |
| 07200000000B | % Scaler for gain = 23 |
| 07160000000B | % Scaler for gain = 24 |
| 07140000000B | % Scaler for gain = 25 |
| 07120000000B | % Scaler for gain = 26 |
| 07100000000B | % Scaler for gain = 27 |
| 07060000000B | % Scaler for gain = 28 |
| 07040000000B | % Scaler for gain = 29 |
| 07020000000B | % Scaler for gain = 30 |
| 07000000000B | % Scaler for gain = 31 |

### Limit and Block Configuration

- **LIMIT:** W BLOCK 1
- **NUMSAM:** W BLOCK 1
- **IGAIN:** W BLOCK 1
- **MASKI:** W BLOCK 1
- **ENDSTACK**

### Gain Type 2 - SEG B Compatible Gain Configuration

**DMXB:** ENTF PARMS

**SEGB:**
- W2 := B.INGAIN % Address of gain values.
- W3 := B.INDAT % Address of input.
- R := B.OUTPUT % Address of output.
- W1 := IND(B.NSAMP) % Element count.
- W1 =: B.NUMSAM % Save as loop counter.
- W SET1 B.LIMIT % Set lower limit for loop.
- W1 := IND(B.IFIXGN) % Fixed gain.
- W1 LADDR B.GNTAB(W1) % Address of gain table.
- W1 =: B.IGAIN
- W4 := IND(B.GNLOC) % 4 bit gain index.
- BIT1 := B14 % High or low order 4 bit.
- W4 - 1 % Adjust for 0 based indexing.
- W SHL W4, -1 % Divide by 2 to get byte index.
- W2 + W4 % Point to proper gain byte.
- W4 := IND(B.NBYSCN) % Number of bytes in each scan.
- W1 COMP 1

**IF = GO LOOPH** % Odd : Gain is in high order 4 bits.

ND-05.013.03

---

## Page 92

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS
## ARRAY PROCESSING FUNCTIONS

% Gain is in low order 4 bits of gain byte.

### LOOPL:
| Instruction                     | Comment                           |
|---------------------------------|-----------------------------------|
| BY1 := W2.0                     | % Gain byte.                      |
| BY1 AND 15                      | % With proper gain bits.          |
| H FCONV W3.0,F2                 | % Convert I2 to R4.               |
| W2 + W4 : W3 + W4               | % Next gain byte and input data.  |
| F MUL3 IND(B.IGAIN)(W1),F2,R.0  |                                   |
| W RLADDR R.4                    | % Next output position.           |
| W LOOPD B.NUMSAM,B.LIMIT,LOOPL  | ; RET                             |

% Gain is in high order 4 bits of gain byte.

### LOOPH:
| Instruction                     | Comment                           |
|---------------------------------|-----------------------------------|
| BY1 := W2.0                     | % Gain byte.                      |
| BY SHL BY1,-4                   | % With proper gain bits.          |
| H FCONV W3.0,F2                 | % Convert I2 to R4.               |
| W2 + W4 : W3 + W4               | % Next gain byte and input data.  |
| F MUL3 IND(B.IGAIN)(W1),F2,R.0  |                                   |
| W RLADDR R.4                    | % Next output position.           |
| W LOOPD B.NUMSAM,B.LIMIT,LOOPH  | ; RET                             |

### ENDROUTINE

ND-05.013.03
Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 93

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## FAST BYTE MOVE

### 5 FAST BYTE MOVE

Please refer to the ND-500 Reference Manual, ND-05.009, for a description of the notation etc.

An instruction for fast move of data is implemented. The speed for this instruction depends on the size of the data cache system. The data cache system is either a 128 Kbyte data cache (dc = 4), a 64 Kbyte data cache (dc = 2), a 32 Kbyte data cache (dc = 1), or no data cache at all (dc = 0).

Format : BY SSMOV <source/r/BY>,<destination/w/BY>,<m/r/W>

| Assembly notation | Name     | Hex code | Octal code |
|-------------------|----------|----------|------------|
| BY SSMOV          | byte move| 0FE77H   | 177167B    |

### Operation

```
i -> 0
while i < m do
  S(i..i+dc*4) -> D(i..i+dc*4) ; i + dc*4 -> i
enddo
```

### Description

<m> bytes are moved from <source> to <destination> operand. The source and destination operands are pointers to the start of the data blocks. Overlap is not taken care of. Constants and registers are illegal as source or destination operands and will cause an illegal operand specifier (IOS) trap condition. The source and destination addresses and number of bytes also have to be a multiplum of dc*4 (either 16, 8 or 4), else an IOS trap condition will occur.

Trap conditions : illegal operand specifier, addressing traps.

Termination conditions : Data status bits are reset. K = 0.

### Example

Copy a number of bytes from one location to another on a system with 128 Kbyte data cache (dc = 4):

```
BY SSMOV IND(B.24B),R.0,B.30B
```

The move will only be done as long as the following is true :

```
contents of B.24B  : pppppppppx0B
contents of R register  : pppppppppy0B
contents of B.30B  : ppppppppppz0B
```

where 'p' means any value, and the legal values for x, y and z are 0, 2, 4 and 6.

---

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 94

# ND-500 Single Precision Array Processing Functions

## Fast Byte Move

The speed of the instruction depends on the system configuration. The maximum speed of the instruction is obtained on a system containing 128 Kbyte data cache. The speed is approximately four times the speed of the instruction BMOVE. With dc=1, the instruction runs at approximately the same speed as the instruction BMOVE.

Microseconds per byte moved for the instruction by SSMOV:

| Number of bytes | ND-560/1 | ND-570/2 |
| --------------- | -------- | -------- |
|                 | Data cache system: | Data cache system: |
| Octal | Decimal | dc = 4 | dc = 2 | dc = 1 | dc = 2 | dc = 1 |
| 100B  | 64      | 0.127 | 0.192 | 0.290 | 0.187 | 0.265 |
| 200B  | 128     | 0.096 | 0.160 | 0.258 | 0.140 | 0.210 |
| 400B  | 256     | 0.082 | 0.145 | 0.243 | 0.113 | 0.187 |
| 1000B | 512     | 0.073 | 0.137 | 0.234 | 0.105 | 0.175 |
| 2000B | 1024    | 0.069 | 0.132 | 0.230 | 0.095 | 0.166 |
| 4000B | 2048    | 0.068 | 0.130 | 0.228 | 0.092 | 0.163 |
| 10000B | 4096   | 0.067 | 0.130 | 0.228 | 0.090 | 0.162 |
| 20000B | 8192   | 0.067 | 0.129 | 0.226 | 0.089 | 0.161 |
| 40000B | 16384  | 0.066 | 0.129 | 0.225 | 0.089 | 0.160 |
| 100000B | 32768 | 0.066 | 0.129 | 0.225 | 0.089 | 0.160 |

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 95

# ND-500 SINGLE PRECISION ARRAY PROCESSING FUNCTIONS

## Index

| Topic | Page |
|-------|------|
| APF library | 1 |
| APMOVE | 80 |
| array processing, definition | 1, 2 |
| basic concepts | 1 |
| CFFTXXX | 56 |
| complex vector, definition | 2 |
| complex vector multiply | 55 |
| convert and move | 80 |
| convolution | 53 |
| CONVXXX | 53 |
| CVMULXX | 55 |
| demultiplex | 82 |
| display processor function | 77 |
| DMXB | 82 |
| dot product | 51 |
| DOTPRXX | 51 |
| elements, definition | 2 |
| element count, definition | 2 |
| fast byte move | 85 |
| fast fourier transform, complex | 56 |
| fast fourier transform, real | 61 |
| floating point accuracy | 1 |
| floating point format | 1 |
| hardware concepts | 1 |
| IBMFPCV | 35 |
| IBM to ND floating point convert | 35 |
| image build | 78 |
| IMGBLD | 78 |
| increment, definition | 2 |
| installation of software | 1 |
| maximum and minimum magnitude element in vector | 47 |
| maximum and minimum value in vector | 46 |
| maximum magnitude element in vector | 44 |
| maximum value in vector | 42 |
| MAXMGVX | 44 |
| MAXMINX | 46 |
| MAXVXXX | 42 |
| memory management system | 1 |
| microprogram | 1 |
| minimum magnitude element in vector | 45 |
| minimum value in vector | 43 |
| MINMGX | 45 |
| MINVXXX | 43 |
| MXMMMGX | 47 |
| NDFPCVG | 33 |
| ND to IBM floating point convert | 33 |
| parallel processing | 1 |
| parameters, specification | 2 |
| performance, ND-560/1 | 11 |
| performance, ND-570/2 | 13 |
| PREDICT | 76 |

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 96

# ND-500 Single Precision Array Processing Functions

## Index

| Function                                        | Page |
|-------------------------------------------------|------|
| RFTFXXX                                         | 61   |
| sum of vector elements                          | 37   |
| sum of vector elements magnitude                | 38   |
| sum of vector elements signed square            | 40   |
| sum of vector elements square                   | 39   |
| SVEMGXX                                         | 38   |
| SVESQXX                                         | 39   |
| SVEXXXX                                         | 37   |
| SVSXXXX                                         | 40   |
| use of processing functions                     | 5    |
| VABSXXX                                         | 26   |
| VADDXXX                                         | 16   |
| VAVGABS                                         | 41   |
| VCLRXXX                                         | 52   |
| VCOSXXX                                         | 29   |
| VDIVSXX                                         | 50   |
| VDIVXXX                                         | 19   |

### Vector

| Operation                                       | Page |
|-------------------------------------------------|------|
| absolute value                                  | 26   |
| add                                             | 16   |
| average absolute value                          | 41   |
| clear                                           | 52   |
| cosine                                          | 29   |
| divide                                          | 19   |
| expand                                          | 73   |
| first and last non-zero value                   | 74   |
| generate                                        | 68   |
| linear interpolation                            | 69   |
| maximum                                         | 20   |
| maximum magnitude                               | 22   |
| minimum                                         | 21   |
| minimum magnitude                               | 23   |
| move                                            | 30   |
| multiply                                        | 18   |
| negative                                        | 32   |
| scalar add                                      | 48   |
| scalar divide                                   | 50   |
| scalar multiply                                 | 49   |
| scalar multiply and add                         | 75   |
| signed square                                   | 25   |
| sinc interpolation                              | 71   |
| sine                                            | 28   |
| square                                          | 24   |
| square root                                     | 27   |
| subtract                                        | 17   |
| swap                                            | 31   |
| taper                                           | 65   |

### Vector, Definition

| Term                                            | Page |
|-------------------------------------------------|------|
| definition                                      | 2    |
| name                                            | 2    |

| Function                                        | Page |
|-------------------------------------------------|------|
| VFLNXXX                                         | 74   |
| VGENXXX                                         | 68   |
| VMAXMGX                                         | 22   |
| VMAXXXX                                         | 20   |

ND-05.013.03

Scanned by Jonny Oddene for Sintran Data © 2011

---

## Page 97

# ND-500 Single Precision Array Processing Functions

## Index

| Function   | Page |
|------------|------|
| VMINMGX    | 23   |
| VMINXXX    | 21   |
| VMOVXXX    | 30   |
| VMULXXX    | 18   |
| VNEGXXX    | 32   |
| VNMGSXX    | 71   |
| VNM0XXX    | 69   |
| VSADDXX    | 48   |
| VSINXXX    | 28   |
| VSMADDX    | 75   |
| VSMULXX    | 49   |
| VSQRTXX    | 27   |
| VSQXXXX    | 24   |
| VSSQXXX    | 25   |
| VSUBXXX    | 17   |
| VSWAPXX    | 31   |
| VTAPERX    | 65   |
| VKPNDXX    | 73   |
| WIENERX    | 66   |
| wiener filter | 66 |
| XBTMUX     | 77   |

---

## Page 98

I'm unable to extract text from this specific page for you. Please provide a clearer text or image file.

---

## Page 99

I can't convert the content of this page as it is missing visible content or text. Please provide a clearer image or additional information.

---

## Page 100

I'm unable to convert the image to Markdown as it primarily contains a design with colored blocks and no visible text or technical content. Let me know if there's anything else you need!

---

