## Page 1

# ND Computer Systems

```plaintext
          BEX NO. 0 (MASTER BEX)
          ______________
         |              |  
CPU ---->|   PIO, DMA   |  
MMS ---->|   MEMORY     |  
         |______________|
         |  CRATE A     |

           ______________
 BEX NO.1 |              |
         |   PIO, DMA   |  
         |   MEMORY     |
         |______________|
         |  CRATE B     |

                     ______________
 BEX NO.7           |              |
                   |   PIO, DMA   |
                   |   MEMORY     |
                   |______________|
                   |  CRATE H     |

           CRATE INTERCONNECTION CABLES

           ND-100 Bus Expander System
```

## Product Information

| Product Code | Product Description                |
|--------------|------------------------------------|
| ND 109       | Bus Expander for ND-100            |
| ND 111       | Bus Expander for ND-100            |
| ND 3302      | ND-100 Expansion System            |
| ND 3304      | ND-100/CE Expansion System         |

## ND 109 Bus Expander for ND-100

### Introduction

The ND-100 Bus Expander System (BEX) makes possible an extension of the ND-100 Bus Structure. It consists of two boards, one in each of two racks. The system is controlled by one CPU. Two crates may be physically connected via two cables between one BEX module in each. They occupy one slot position in each of the crates to be linked. To the BEX in the CPU crate, the MASTER BEX, up to seven other crates may be connected, each with one BEX in position one.

### Features

- The total memory capacity in a system may be divided in different crates.
- The ND-100 BEX system ensures that each slot position in any crate has equal properties. Thus, it is possible to mix PIO controllers, DMA controllers and memory modules in all crates.
- The actual placement of modules in a bus expanded system follows the same rules as for a single crate system.
- In addition to being transparent, BEX modules provide several features controllable by program or switches.

```
109/111/3302/3304-A1-6000-0681
```

---

## Page 2

# Product Operation

In order to route memory addresses to crates where they are represented by physical memory each BEX module has a Lower Limit (LL) address register and an Upper Limit (UL) address register.

On each BEX connected to a crate with memory, these registers have to be given a value corresponding to the memory area covered by the crate.

During a memory reference, either initiated by the CPU or a DMA controller, all crates will be presented the memory address simultaneously. Thus, all BEX modules, in parallel, will "look" at the address to see if it is between the LL and UL values set for the crate.

Most of the parameters to be set on the BEX modules, may be set either by program or by switches.

Each BEX unit is programmable by means of IOXT instructions. Each BEX number (device No.) is assigned 4 IOXT device register addresses according to:

| DVVN + 0 | Read Data       |
|----------|-----------------|
| DVVN + 1 | Write Data      |
| DVVN + 2 | Read Status     |
| DVVN + 3 | Write Control Word |

# ND 111 Bus Expander for ND-100

ND 111 consists of one board and is equal to 1/2 x ND 109.

# ND 3302 ND-100 Expansion System

ND 3302 consists of ND 049 Expansion Rack and Power Supply, ND-100. In addition is required: ND 109 Bus Expander and ND 178 ND-100 Cabinet, 6 modules.

# ND 3304 ND-100/CE Expansion System

ND 3304 consists of ND 056, ND-100 Expansion Rack with 20 positions. In addition is required: ND 109 Bus Expander.

```
+---------------------------------+
|                                 |
|           [Photo: ND]           |
|                                 |
+---------------------------------+
```

```
+---------------------------------+
|                                 |
|       [Photo: ND Comtec]        |
|                                 |
+---------------------------------+
```

### Contact Information

| Location      | Phone       | Address                 |
|---------------|-------------|-------------------------|
| Bergen        | 05-20290    |                         |
| Sandnes       | 04-245544   |                         |
| Tromsø        | 083-71765   |                         |
| Stockholm     | 087-86500   | tix. 13528 nordata s    |
| Göteborg      | 031-293900  |                         |
| Malmö         | 040-79515   |                         |
| Copenhagen    | 01-25-5055  | tix. 37775 nord dk      |
| Wiesbaden     | 0611-74641  | tix. 438703 norda       |
| Ferrys-Volteran | 050-4875878 | tix. 385653 nordata ferrv |
| Paris         | tel. 0211-663388 | tix. 858727 comt d  |
| Ivry          | 01-8743747  |                         |
| Newbury       | 0635-31465  | tix. 848919 norskd      |
| Boston        | 001-237-7965 | tix. 921750 norsk well |

_NOTE: NORSK DATA reserves the right to change specifications without given notice!_

---

