# 4.2.2.4 IF Statements

The IF statement has the general form:

```
IF <conditions> THEN <statements> ELSE <statements> FI
```

ELSE can be omitted.  

Between IF and THEN there can be several conditions, delimited by the OR or AND symbols. The conditions are evaluated from left to right. If a condition followed by AND or THEN is not true, the statements after THEN are bypassed. If a condition followed by OR or THEN is true, the statements after THEN are executed. Otherwise more conditions will be tested.

There are two types of conditions: relations and bit tests.  

A relation consists of two executable expressions with a relational operator between them:

| Operator | Meaning           |
|----------|-------------------|
| `>`      | Greater           |
| `<`      | Less              |
| `=`      | Equal             |
| `>=`     | Greater or equal  |
| `<=`     | Less or equal     |
| `><`     | Not equal         |

If the first element of the first expression is a variable or a constant, the A or TAD register is considered as the primary register.

---

If the first element of the second expression is a variable or a constant, the T register is considered as the primary register. This means that the relation:

```
IF VAR1 VAR2 THEN...
```

is equivalent to:

```
IF A := VAR1 T := VAR2 THEN...
```

An expression can be empty. Then the present value of the A or T register will be used.

```
IF > THEN...
```

is equivalent to:

```
IF A > T THEN...
```

This means that a construction like this is possible:

```
IF VAR1 = 4 OR = 6 OR = 7 THEN...
```

If the first expression is of type REAL, the second must be equal to zero.  

It is also possible to compare absolute values, where the register contents are considered as positive numbers from 0 to 64K. Then the two relational operators can be used:

| Operator | Meaning          |
|----------|------------------|
| `<<`     | Less             |
| `>>=`    | Greater or equal |

In the NORD-1 version the second expression must be equal to zero.  

### Examples

```
IF VAR-D<VAR2 THEN...
```
**MAC equivalents:**
```
LDA VAR
RSUB SD DA
LDT VAR2
SKP IF DA LST ST
JMP BYPAS
```

```
IF A + 10 = 0 THEN...
```
**MAC equivalents:**
```
AAA 10
JAF BYPAS
```

```
IF A - LLIM >= 0 THEN
```
**MAC equivalents:**
```
SUB LLIM
BSKP ONE SSC
JMP BYPAS
(NORD-1 version)
```

---

## Bit Tests

A condition can be a bit test.  

The bit to be tested can be one of the single bit registers. The condition can be inverted by placing the symbol `NBIT` after the register specification.

### Examples

```
IF K THEN...
```
**MAC equivalents:**
```
BSKP ONE SSK
JMP BYPAS
```

```
IF M NBIT THEN...
```
**MAC equivalents:**
```
BSKP ZRO SSM
JMP BYPAS
```

A bit in the general registers can also be specified. An expression determines the register. Then one of the symbols BIT or NBIT selects 1 or 0 as true. At last a constant determines the bit number.

```
IF T BIT 7 THEN...
```
**MAC equivalents:**
```
BSKP ONE 70 DT
JMP BYPAS
```

```
IF NBIT 1 THEN
```
**MAC equivalents:**
```
BSKP ZRO 10 DA
JMP BYPAS
```

---

## Abbreviation

The construction:

```
THEN GO <label> FI
```

can be abbreviated to:

```
GO <label>
```

For instance:

```
IF A < 0 GO ERR
```

is equivalent to:

```
IF A < 0 THEN GO ERR FI
```

**MAC equivalent:**
```
JAN ERR
```
