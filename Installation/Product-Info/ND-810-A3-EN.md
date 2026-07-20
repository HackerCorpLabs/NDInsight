## Page 1

# ND Computer Systems

## ND810 Input Configuration

```
                     External
                    Equipment
                       |       
                       |       
                       |          
                       V         
                     +--+   
          Vi         |Dx|        
 ---------------->---|  |--------+              
             RE      |  |        |       
             Limiting+--+        |        
             Resistor            |        
               |                 |        
               |                 |        
               V                 V        
             +--+     VARISTOR  +-+       
             |Dx|<--------------|Z|       
             |  |     3.3V      +-+       
             |  |                                  
             +--+                  +----------+---+  
               |     Opto         |Opto Coupler|   
               +----|De|---------| HP 5082-4360|   
                       |          +----------+|     
                       |                       +---      
                       |                           |   
                       |------Dix0                 |   
                      5V                           | 
                                                   V     
                                          Computer Ground
```

## ND811 Output Configuration

```
               5V                          
               |    
               +-----+-->
               |     |  From output latch      
           +---+----+---+   
           | Photo-    |   
           | Darlington| 
           | Relay     |   
         +-+  MCA2-55  +--+         I0    
         | +-----------+   |      +-----+    
         |                 |      |     |   
         V                 \      /     |
      +--+                  \680k /     |
      |D1|                   \   /      |
      +--+                    \_/-------+    Vc
         |         
         |                    RE
         |                 External
      +--+---+            Equipment         
      | D0   |
      +------+
```

## ND810 Process Digital Input

## ND811 Process Digital Output

### Introduction

The ND810 and ND811 optically coupled digital input/output modules are designed for industrial and marine applications for interfacing ND computers to relays, lamps, and on/off contacts. Each module occupies one standard I/O slot.

### Features

- 12 bits in — 16 bits out
- All lines optically coupled
- Internal programmable Compare Register
- Contact debounce logic
- Input signal conditioning

### Product Description

The ND810 Input Module contains a programmable 12-bit Compare Register. If the input data pattern and the Compare Register differ, an interrupt will be generated.

The ND811 Output Module does not generate any interrupts. 16 bits of data are simply loaded into a holding register and will immediately be available at the output terminals.

```
810/811–A3–1500–0681
Scanned by Jonny Oddene for Sintran Data © 2010
```

---

## Page 2

# SPECIFICATIONS

## ND810 Input Module

- **Number of digital inputs**: 12
- **Operating temperature**: 0 - 55°C
- **Operating relative humidity**: 0 - 90% no condensation

### Electrical Characteristics

|                         | Min. | Typ. | Max.  |
|-------------------------|------|------|-------|
| V<sub>IT</sub> = Transition Voltage    |      | 5.5V |       |
| V<sub>IH</sub> = High level input voltage | 7.8V |      |       |
| V<sub>IC</sub> = Low level input voltage |      |      | 1.7V  |
| I<sub>IH</sub> = High level input current, @V<sub>I</sub> = 12V |      |      | 9.3mA |
| I<sub>IL</sub> = Low level input current, @V<sub>I</sub> = 1.5V |      |      | 0.25mA |
|                                @V<sub>I</sub> = -15V            |      |      | -10mA |

For V<sub>I</sub> > 12V, a limiting resistor — RE — must be included in external equipment.

### Absolute Maximum Ratings

Voltage between corresponding input terminals: -15V, +13.5V.

**NOTE**: DX<sub>o</sub>, DX<sub>1</sub>  
- D = Data  
- X = Data bit #  
- Index 0 = -terminal  
- Index 1 = +terminal  

Continuous Common Mode voltage between computer ground and input terminals: ± 30V.

## Switching Characteristics

Delay from any change in input to interrupt: 4 ms ± 20%. A change in any input line must exceed the 4 ms in order to generate the interrupt. This constitutes the contact debounce logic.

## ND811 Output Module

- **Number of digital outputs**: 16
- **Operating temperature**: 0 - 55°C
- **Operating relative humidity**: 0 - 90% no condensation

### Electrical Characteristics

|                         | Min. | Typ. | Max.  |
|-------------------------|------|------|-------|
| V<sub>OL</sub> = Low level output voltage, @I<sub>OL</sub> = 30mA |      | 0.9V | 1V    |
| I<sub>OL</sub> = Low level output current | 30mA | 80mA |       |
| I<sub>OH</sub> = High level output current, @V<sub>OH</sub> = 30V |      |      | 1µA  |

### Absolute Maximum Ratings

- Voltage at terminal DX<sub>i</sub> relative DX<sub>o</sub>: -5V, +30V.
- Continuous common mode voltage between computer ground and output terminals DX<sub>o</sub> or DX<sub>i</sub>: ± 30V.
- Maximum continuous output current: 100 mA. Inductive load should be properly bypassed by a diode.

## Switching Characteristics

Switching time depends upon load resistance — RE — and pullup voltage V<sub>C</sub>. Typical ON and OFF switching times for RE: 1kΩ and V<sub>C</sub>: 30V are less than 30 µs.

The following table defines plug connection/Data line definition for both ND810 and ND811:

| Data bit DX<sub>i</sub>/DX<sub>o</sub> | Burndy plug pins |
|---------------------------------------|------------------|
| D0/D0<sub>o</sub>                      | A/C              |
| D1/D1<sub>o</sub>                      | B/D              |
| D2/D2<sub>o</sub>                      | E/H              |
| D3/D3<sub>o</sub>                      | F/J              |
| D4/D4<sub>o</sub>                      | K/M ND810        |
| D5/D5<sub>o</sub>                      | L/N and          |
| D6/D6<sub>o</sub>                      | P/S ND811        |
| D7/D7<sub>o</sub>                      | R/T              |
| D8/D8<sub>o</sub>                      | U/W              |
| D9/D9<sub>o</sub>                      | V/X              |
| D10/D10<sub>o</sub>                    | Y/AA             |
| D11/D11<sub>o</sub>                    | Z/BB             |
| D12/D12<sub>o</sub>                    | ZZ/EE            |
| D13/D13<sub>o</sub>                    | DD/FF ND811 only |
| D14/D14<sub>o</sub>                    | HH/KK            |
| D15/D15<sub>o</sub>                    | JJ/LL            |

```
      +---------------------------------+
      |                                 |
  ND  |     Bergen, tel. 05-292290      |    ND 
      |     Sandnes, tel. 06-541564     |    COMTEC
  Da  |     Tromso, tel. 083-71716      |
      |     Stockholm, tel. 08-235065   |    Jerikoveien 20    
      |       tix. 13528 nordata s      |    Boks 4 Lindeberg gard
  Tele|     Goteborg, tel. 031-259554   |    Oslo 10    
      |     Malmo, tel. 040-191250      |    Tel: 02-39030     
  kern|     Copenhagen, tel. 01-259055  |    Tlx: 18664 nd n   
      |       tix. 37725 nd dk          |
      |     Wedschweid, tel. 06151-7541 |    Trondheim, tel. 075-16250, tix. 55580 comtec n
      |       tix 415570 nd ga          |    Stockholm (Upplands Vasby), tel. 08-25030, tix. 13528 nordata s
      |     Paris, tel. 01-234566       |    Stockholm (Solna), tel. 08-275855, tix. 13700 swecom s
      |     Lyon, tel. 78-632347        |    Odense, tel. 09-157044, tix. 65868 comtec dk
      |     Newberry (Norfolk), tel. 0635-31465 |    Ballerup (Copenhaven), tel. 02-678100
  Bosto|     Boston, tel. 061-2377945,  |    Dusseldorf, tel. 0211-663638, tix. 8557277 comt d
      |       tix 921750 norsk weil     |
      |                                 |
      +---------------------------------+
```

---

*Note*: NORSK DATA reserves the right to change specifications without given notice!

---

