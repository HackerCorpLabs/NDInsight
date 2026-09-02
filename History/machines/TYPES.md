# Catalog of ND type and model numbers

*Norsk Data used several different numbering schemes at once. This page says what
each one is, and decodes the ones that turn up in the
[list of surviving machines](MACHINE-LIST.md).*

---

## 1. The problem, stated plainly

A single ND machine carries **five different numbers**, and they mean different
things. This is the main reason the model names are confusing:

| Scheme | Looks like | What it identifies | Example |
|--------|-----------|--------------------|---------|
| **Machine name** | ND-100, ND-5800 | The product family and rough position in the range | ND-5800 |
| **System product number** | 3391, 5772, 5904 | **One specific configuration** you could order - cabinet, CPU, memory, disks | 5772 = ND-570/CX modell 22 |
| **CPU type** | 1, 2, 3, 4 | ND-5000 only. How many layers of cards, and which modules are fitted | Type 3 = ND-5800 |
| **CPU model** | 2, 4, 5, 7, 8, 9 | ND-5000 only. The number the microcode and ACCP report | Model 8 = ND-5800 |
| **Part / PCB number** | 320003, PCB 6201 | A single board | 350403 = Rallar CPU |

The **type** column in the machine list is the *system product number*. That is why it
shows values like 3391 and 5772 that appear in no manual - they are order codes,
each with its own one-page product sheet.

---

## 2. Decoding the system product numbers

The numbering is not one formula, but two clear patterns emerge.

**ND-500/CX systems** are `5<model><variant>`, where the last digit is the
"modell" number:

| Number | System | Modell |
|--------|--------|--------|
| 5171 | ND-510/CX | - |
| 5371 | ND-530/CX | modell 21 |
| 5372 | ND-530/CX | modell 22 |
| 5571 | ND-550/CX | modell 21 |
| 5572 | ND-550/CX | modell 22 |
| 5671 | ND-560/CX | modell 21 |
| 5672 | ND-560/CX | modell 22 |
| 5771 | ND-570/CX | modell 21 |
| 5772 | ND-570/CX | modell 22 |

**ND-5900 systems** end in the **number of processors**:

| Number | System |
|--------|--------|
| 5900 | ND-5900 |
| 5902 | ND-5900 ES model **L2** - two CPUs |
| 5903 | ND-5900 ES model **L3** - three CPUs |
| 5904 | ND-5900 ES model **L4** - four CPUs |

"**ES**" is the ES platform that came out of the Server 88 project; "model L" is
the large-cabinet build.

---

## 2b. The authoritative definition - ND's own order manual

**ND-830053.3 EN, "SINTRAN III How to order it", November 1988**, section 1
CUSTOMER INFORMATION, is where these numbers are actually defined. Verbatim:

> **- CPU type:** This is the CPU type of the system. You will find the CPU type
> stated on your order confirmation. [...] **The symbol mark generated is 33CPU.**

> **- CPU no.:** The system number must be filled in. It will identify the SINTRAN
> III later on when a new SINTRAN III needs to be installed. **The symbol mark
> generated is 33CPN.**

So the type number is the **hardware model code from the order confirmation**, and
it is what SINTRAN's `NEW-SYSTEM` asks for at install time. Its "some common
values" table (OCR digit noise marked `?`):

**Every code known, from all three sources, in one place.** "Known from" says
where each is documented: the **order manual** (`ND-830053.3`), ND's **product
library** of per-product sheets, or a **real machine** preserved and catalogued.
"On the machine list" counts machines carrying that code in the
[266-machine register](MACHINE-LIST.md).

| Code | Machine | Details | Known from | On the machine list |
|---|---|---|---|---|
| 10 | *unidentified* | Appears against 1 machine on the machine list, with no product sheet or manual entry. | machine list only | 1 |
| 100 | *unidentified* | Appears against 43 machines on the machine list, with no product sheet or manual entry. | machine list only | 43 |
| 102 | *unidentified* | Appears against 4 machines on the machine list, with no product sheet or manual entry. | machine list only | 4 |
| 103 | *unidentified* | Appears against 2 machines on the machine list, with no product sheet or manual entry. | machine list only | 2 |
| 500 | *unidentified* | Appears against 10 machines on the machine list, with no product sheet or manual entry. | machine list only | 10 |
| 502 | *unidentified* | Appears against 22 machines on the machine list, with no product sheet or manual entry. | machine list only | 22 |
| 503 | *unidentified* | Appears against 2 machines on the machine list, with no product sheet or manual entry. | machine list only | 2 |
| 563 | *unidentified* | Appears against 1 machine on the machine list, with no product sheet or manual entry. | machine list only | 1 |
| 570 | *unidentified* | Appears against 1 machine on the machine list, with no product sheet or manual entry. | machine list only | 1 |
| 572 | *unidentified* | Appears against 1 machine on the machine list, with no product sheet or manual entry. | machine list only | 1 |
| 573 | *unidentified* | Appears against 1 machine on the machine list, with no product sheet or manual entry. | machine list only | 1 |
| **900** | NORD-100 Satellite (family code) | One of the four short codes SINGEN offered by name. **188 order forms** carry it. ndwiki labels 900.x machines "NORD-100 Satellite 9", but the product library gives **950 = ND-Satellite/5** and **951 = ND-Satellite/9** - so 900 is a broader or older code and the namespaces do not line up. **[?]** | real machine, order archive |  - |
| **950** | ND-Satellite/5 | - | product library | - |
| **951** | ND-Satellite/9 | - | product library | - |
| 3031 | *unidentified* | Appears against 1 machine on the machine list, with no product sheet or manual entry. | machine list only | 1 |
| 3035 | *unidentified* | Appears against 1 machine on the machine list, with no product sheet or manual entry. | machine list only | 1 |
| **3051** | ND-110/CX Compact | One preserved machine. Sits below the manual's 3080-3095 band. | real machine | - |
| **3080** | ND-110 Compact model A0 | ND-110 CPU in a Compact cabinet. Base model. No product sheet found. | order manual | - |
| **3081** | ND-110 Compact model A1 | Next step up from A0. Order manual OCR reads the code as `308?`. | order manual | - |
| 3082 | *unidentified* | Appears against 1 machine on the machine list, with no product sheet or manual entry. | machine list only | 1 |
| **3085** | ND-110 Compact model B | ND-110 Compact, B grade. | order manual | - |
| **3090** | ND-110/CX Compact model A0 | As 3080 but with the faster ND-110/CX CPU board. | order manual | - |
| **3091** | ND-110/CX Compact model A1 | Next step up from 3090. | order manual | - |
| 3092 | *unidentified* | Appears against 3 machines on the machine list, with no product sheet or manual entry. | machine list only | 3 |
| **3095** | ND-110/CX Compact model B | ND-110/CX Compact, B grade. **Also the part number of the ND-110/CX CPU card** - see the cautions. | order manual | - |
| 3209 | *unidentified* | Appears against 2 machines on the machine list, with no product sheet or manual entry. | machine list only | 2 |
| **3260** | ND-100 Compact, Model I | From the Illustrated Parts Catalogue ND-13.018.01, Oct 1984, "Specify this when ordering / Article number". | product library | - |
| **3261** | ND-100 Compact, Model II | Same catalogue. | product library | - |
| **3262** | ND-100 Compact, Model III | Same catalogue. | product library | - |
| **3263** | ND-100 Compact, Model IV | Same catalogue. | product library | - |
| **3270** | ND-110 Compact/FTX | A complete fault-tolerant Compact system. | product library | - |
| **3291** | ND-100 Compact, Model 11 | From the Illustrated Parts Catalogue ND-13.024.1, Jan 1987, which specifies "ND-100 STANDARD CPU 48-BIT CX" for this block. | product library, machine list | 4 |
| **3292** | ND-100 Compact, Model 12 | From the Illustrated Parts Catalogue ND-13.024.1, Jan 1987, which specifies "ND-100 STANDARD CPU 48-BIT CX" for this block. | product library, machine list | 2 |
| **3293** | ND-100 Compact, Model 13 | From the Illustrated Parts Catalogue ND-13.024.1, Jan 1987, which specifies "ND-100 STANDARD CPU 48-BIT CX" for this block. | product library | - |
| **3294** | ND-100 Compact, Model 14 | From the Illustrated Parts Catalogue ND-13.024.1, Jan 1987, which specifies "ND-100 STANDARD CPU 48-BIT CX" for this block. | product library | - |
| **3391** | ND-110 Compact Series, ND-110, model 11 | ND-110 CPU. **1 MB** memory. **28 MB** Winchester disk. No streamer. 1.2 MB floppy. 8 free rack positions. | product library | 1 |
| **3392** | ND-110 Compact Series, ND-110, model 12 | ND-110 CPU. 1 MB memory. **74 MB** Winchester disk. **45 or 60 MB streaming tape**. 1.2 MB floppy. 8 free rack positions. Also seen as the type code of a preserved ND-100/CX Compact. | product library, real machine | 1 |
| **3393** | ND-110 Compact Series, ND-110, model 13 | ND-110 CPU. 1 MB memory. **Two 74 MB** Winchester disks. 45 or 60 MB streamer. 8 free rack positions. | product library | - |
| **3394** | ND-110 Compact Series, ND-110, model 14 | ND-110 CPU. 1 MB memory. **No internal disk** - an **SMD controller for external disks** instead, allowing four extra drives. 7 free rack positions. | product library | - |
| **3395** | ND-110 Compact Series, ND-110/CX, model 11 | As 3391 but with the **ND-110/CX** CPU, which the sheet says has *"approximately twice the performance of the ND-110"*. | product library | - |
| **3396** | ND-110 Compact Series, ND-110/CX, model 12 | As 3392 with the ND-110/CX CPU. | product library | - |
| **3397** | ND-110 Compact Series, ND-110/CX, model 13 | As 3393 with the ND-110/CX CPU. | product library | - |
| **3398** | ND-110 Compact Series, ND-110/CX, model 14 | As 3394 with the ND-110/CX CPU. | product library | - |
| **3740** | ND-110/CX system | A full large-cabinet system rather than a Compact. Three machines on the list - NODEA, NODEB and INTERCOM. | order manual | 3 |
| **3750** | ND-110/FTX | Fault-tolerant full-cabinet ND-110. | product library | - |
| 5025 | *unidentified* | Appears against 1 machine on the machine list, with no product sheet or manual entry. | machine list only | 1 |
| **5151** | ND-510/FTX | Fault-tolerant ND-510. | product library | - |
| 5161 | *unidentified* | Appears against 1 machine on the machine list, with no product sheet or manual entry. | machine list only | 1 |
| **5171** | ND-510/CX Computer System | A preserved machine carrying this code is an **ND-530/CX** - probably as-built versus as-upgraded. The classic ND-500 family uses a 51xx band the order manual never mentions. | product library, real machine | - |
| 5221 | *unidentified* | Appears against 2 machines on the machine list, with no product sheet or manual entry. | machine list only | 2 |
| **5230** | ND-5200 ES model L | Memory **6-544 MB**. Disk max **35.2 GB**. Relative CPU performance **1**, the family baseline. Front-end **ND-120**. Single CPU. | order manual, product library | - |
| **5231** | ND-5200 ES model L, SCSI | As 5230 with an SCSI disk interface instead of SMD. | order manual | - |
| **5282** | ND-5200 Compact | One preserved machine, in a large collection at Umea. Sits just below the manual's 5290-5295 Compact band. | real machine | 1 |
| **5290** | ND-5200 Compact model A10 | ND-5200 in the 5000 Compact cabinet - two backplanes. **Note**: the ES sheet uses 5290 as the *upgrade* product from ND-5200 to ND-5400, so this number has two meanings. | order manual | - |
| **5291** | ND-5200 Compact model A11 | Next step up. | order manual | - |
| **5295** | ND-5200 Compact model B1 | ND-5200 Compact, B grade. | order manual | - |
| **5351** | ND-530/FTX Model 11 | From the FTX product sheet. | product library | - |
| **5352** | ND-530/FTX Model 12 | From the FTX product sheet. | product library | - |
| 5362 | *unidentified* | Appears against 1 machine on the machine list, with no product sheet or manual entry. | machine list only | 1 |
| **5371** | ND-530/CX, modell 21 | - | product library | - |
| **5372** | ND-530/CX, modell 22 | - | product library | - |
| 5400 | *unidentified* | Appears against 1 machine on the machine list, with no product sheet or manual entry. | machine list only | 1 |
| 5408 | *unidentified* | Appears against 2 machines on the machine list, with no product sheet or manual entry. | machine list only | 2 |
| 5421 | *unidentified* | Appears against 1 machine on the machine list, with no product sheet or manual entry. | machine list only | 1 |
| **5430** | ND-5400 ES model L | Memory **8-544 MB**. Disk max 35.2 GB. Relative performance **2.2**. Front-end **ND-120**. CPU type 2, model 4; slow clock 156 ns; instruction cache and Smart IfGo. | order manual, product library, real machine | 5 |
| **5431** | ND-5400 ES model L, SCSI | As 5430 with an SCSI disk interface. | order manual | - |
| 5481 | *unidentified* | Appears against 1 machine on the machine list, with no product sheet or manual entry. | machine list only | 1 |
| **5491** | ND-5400 Compact model A11 | ND-5400 in the 5000 Compact cabinet. | order manual | - |
| **5495** | ND-5400 Compact model B1 | ND-5400 Compact, B grade. | order manual | - |
| **5500** | ND-5500 ES model L | Memory **12-544 MB**. Disk max 35.2 GB. Relative performance **4.3**. Front-end **ND-120**. CPU type 2, model 5; adds the data cache. | order manual, product library | 4 |
| **5503** | ND-5500 ES model L, SCSI | As 5500 with an SCSI disk interface. | order manual | - |
| **5571** | ND-550/CX, modell 21 | - | product library | 1 |
| **5572** | ND-550/CX, modell 22 | - | product library | - |
| **5591** | ND-5500 Compact model A11 | ND-5500 in the 5000 Compact cabinet. | order manual | - |
| **5595** | ND-5500 Compact model B1 | ND-5500 Compact, B grade. | order manual | - |
| **5671** | ND-560/CX, modell 21 | - | product library | 1 |
| **5672** | ND-560/CX, modell 22 | - | product library | - |
| **5700** | ND-5700 ES model L | Memory **18-544 MB**. Disk max 35.2 GB. Relative performance **6.3**. Front-end **ND-120/CX** - first in the range to get the /CX. CPU type 2, model 7; adds the address cache. **3-3.5 Whetstone MIPS**; built to replace the ND-570/CX. | order manual, product library | 10 |
| **5703** | ND-5700 ES model L, SCSI | As 5700 with SCSI. **Code disputed**: order manual says 5703, the ES product sheet says **5711**. | order manual | - |
| 5715 | *unidentified* | Appears against 1 machine on the machine list, with no product sheet or manual entry. | machine list only | 1 |
| 5762 | *unidentified* | Appears against 1 machine on the machine list, with no product sheet or manual entry. | machine list only | 1 |
| **5771** | ND-570/CX, modell 21 | - | product library | - |
| **5772** | ND-570/CX, modell 22 | - | product library | 1 |
| 5782 | *unidentified* | Appears against 1 machine on the machine list, with no product sheet or manual entry. | machine list only | 1 |
| **5791** | ND-5700 Compact model A11 | ND-5700 in the 5000 Compact cabinet. | order manual | - |
| 5792 | *unidentified* | Appears against 1 machine on the machine list, with no product sheet or manual entry. | machine list only | 1 |
| **5795** | ND-5700 Compact model B1 | ND-5700 Compact, B grade. **ND's own test machines COMSON-A and COMSON-B were this configuration.** | order manual | 2 |
| **5800** | ND-5800 ES model L | Memory **26-544 MB**. Disk max 35.2 GB. Relative performance **11.3**. Front-end **ND-120/CX**. CPU type 3, model 8; all caches plus WICO, unique mother board, IDAC booster. **6-7 Whetstone MIPS**. | order manual, product library, real machine | 7 |
| **5803** | ND-5800 ES model L, SCSI | As 5800 with an SCSI disk interface. | order manual | - |
| 5850 | *unidentified* | Appears against 7 machines on the machine list, with no product sheet or manual entry. | machine list only | 7 |
| **5900** | ND-5900 System | Multi-CPU. See models L2, L3, L4 below. | product library | 1 |
| **5902** | ND-5900 ES model L2 | **Two** ND-5000 CPUs. Memory **30-544 MB**. Relative performance **22.6**. **12-14 Whetstone MIPS**. | order manual, product library | 1 |
| **5903** | ND-5900 ES model L3 | **Three** CPUs. Relative performance **33.9**. **18-21 Whetstone MIPS**. | order manual, product library | - |
| **5904** | ND-5900 ES model L4 | **Four** CPUs, top of the range. Relative performance **45.2**. **24-28 Whetstone MIPS**, about 2.3x a VAX-8800. | order manual, product library | - |
| **5907** | ND-5900 ES model L2, SCSI | As 5902 with SCSI. | order manual | - |
| **5908** | ND-5900 ES model L3, SCSI | As 5903 with SCSI. | order manual | - |
| **5909** | ND-5900 ES model L4, SCSI | As 5904 with SCSI. | order manual | - |
| 9724 | *unidentified* | Appears against 1 machine on the machine list, with no product sheet or manual entry. | machine list only | 1 |
| 9801 | *unidentified* | Appears against 1 machine on the machine list, with no product sheet or manual entry. | machine list only | 1 |
| 9804 | *unidentified* | Appears against 1 machine on the machine list, with no product sheet or manual entry. | machine list only | 1 |
| **9871** | ND-110 Satellite Series model 11 | - | product library | - |
| **9872** | ND-110 Satellite Series model 12 | - | product library | - |
| **9873** | ND-110 Satellite Series model 13 | - | product library | - |
| **9874** | ND-110 Satellite Series model 14 | - | product library | - |
| **9875** | ND-110 Satellite Series model 15 | - | product library | 1 |
| **9881** | ND-110 Satellite model S5 | 2 MB memory. **60 MB** ST-506 (Winchester/MFM) disk. **5** terminal/printer interfaces. 3 card slots free. | order manual, product library | 1 |
| **9882** | ND-110 Satellite model S9 | 2 MB memory. **60 MB** disk. **9** interfaces. 3 slots free. Identical to S5 but for the interface count. | order manual, product library | - |
| **9883** | ND-110 Satellite model T9 | 2 MB memory. **125 MB** disk - the T grade buys the bigger disk. **9** interfaces. 3 slots free. **Thirteen preserved machines carry this code**, and a live SINTRAN image in this repo has it. | order manual, product library, real machine | - |
| **9884** | ND-110 Satellite model T17 | 2 MB memory. **125 MB** disk. **17** interfaces, top of the range. Only **2** slots free. Order manual OCR reads the model as `T1?`. | order manual, product library | - |


### Codes seen on real machines

ndwiki's [Category:Individual systems] holds 41 preserved machines, and the
collector names each page **`CPUTYPE.CPUNUMBER`** - so the titles are themselves a
dataset. Evidence gathered 2026-08-28 from Wayback snapshots.

**The convention is confirmed on physical hardware.** A handwritten label inside
the front door of an ND-110 Satellite reads:

```
ORD: 20.6578 E
CPU: 9883.21005
```

Cabinet plates carry the same pairing, one per crate, on an ND-5800:

```
ND-5000 crate / Cabinet assy 1.2 / Function : MPM 5/5800 CX / System no. : 5800.21284
ND-100 crate  / Cabinet assy 1.1 / Function : ND-120 CX     / System no. : 5800.21284
```

Note the two crates of one machine - the ND-5000 side and the ND-100 side - share
one system number, and each plate names the function of its own crate. And a
paper order book for a Satellite writes the pair with no separator at all:
**"CPU-NO.: 900135"**, meaning type 900, number 135.

**Codes confirmed by surviving machines**: 9883 (thirteen separate ND-110
Satellites), 5800, 5430.

**Five codes turned up that are not in the order manual's list**, which the manual itself describes as only "some common values": **900**, **3051**, **3392**, **5171** and **5282**. All five are in the master table above.


### What every ND-110 Compact shares

From the ND-110 Compact Series product sheet. The sheet carries **no document
number and no date** - the back page, where ND normally prints them, was not
scanned.

| Item | Value |
|---|---|
| CPU and MMU | On one card, "which results in one extra free position for memory expansion". Both CPUs "totally compatible with the ND-100 CPU" |
| ND-110/CX gain | "approximately **twice the performance** of the ND-110 CPU" |
| Memory | 1 MB fitted, ceiling **16 MB**. Boards in steps of 1/2, 1 and 2 MB. 22 bits per 16-bit word, single-bit correcting |
| Cache | 8 KB |
| MMU | 16-bit virtual to **24-bit physical**, "extending the latter from 128 KB to 32 MB". 16 page index tables of 64 words. **Page size 2048 bytes** |
| Interrupts | 16 priority levels, context switch **3.4 us** |
| Floating point | 48-bit standard (32-bit mantissa, 16-bit exponent); 32-bit optional |
| Rack | **12 positions**, 8 free (7 on model 14) |
| Terminals | Up to **48**, "limited only by space in the 12 position rack". No per-model interface count is given |
| Disk ceiling | Two 74 MB internal, up to **1800 MB** external |
| Cabinet | 69 cm high, 76 cm deep, 54 cm wide, about 78 kg, forced air |
| Power | Max 800 W, 230 V 50 Hz. Start peak 60 A for 30 ms, 6 A slow main fuse |
| Built in | Telefix adaptor, with a printer outlet for hardcopy of Telefix communication |

**The model digit is the disk configuration and nothing else** - the sheet's own
words: "four models with internal disk storage ranging from 28 MB in Model 11 to
two 74 MB disks in Model 13, and an SMD controller for external disks in Model
14." CPU, memory and floppy are identical across all four.

**On the disk interface**: the sheet says only **"Winchester"** for the internal
disks and never names a controller standard. **ST-506 and SCSI are never
mentioned**; SMD appears only for model 14's *external* disks. So this sheet does
not settle the internal interface on its own - see the ST-506 discussion above.

**An artefact worth knowing**: the software section opens "The following software
is delivered with the **ND-100** COMPACT system", and its whole documentation list
is ND-100 Compact material. The sheet was clearly set from the earlier ND-100
Compact sheet and the references were not updated.

**Open question**: ND named these Compact models **11, 12, 13, 14** here, but the
order manual names Compact models **A0, A1, B** with a different code block
(3080/3081/3085 and 3090/3091/3095). Whether those are two generations of the
range or two naming conventions for the same machines is **not established**. **[?]**


### The 3xxx blocks, grouped by second digit

The 3xxx space is not one sequence. Grouping by the **second digit** matches what
the documents say:

| Block | What is documented in it |
|---|---|
| **308x / 309x** | ND-110 Compact A0/A1/B and ND-110/CX Compact A0/A1/B (order manual, 1988) |
| **320x / 327x** | FTX for Compact - 3201/3202 packages, 3270 a complete ND-110 Compact/FTX |
| **326x** | ND-100 Compact models I-IV (parts catalogue, 1984) |
| **329x** | ND-100 Compact models 11-14 (parts catalogue, 1987) - **ND-100/CX** machines |
| **330x** | 3302 ND-100 Expansion System, 3304 ND-100/CE Expansion System |
| **339x** | ND-110 Compact models 11-14 - 3391-3394 ND-110, 3395-3398 ND-110/CX |
| **370x / 374x / 375x** | Full-cabinet ND-110 - 3701/3702 FTX packages, 3740 ND-110/CX system, 3750 ND-110/FTX |

**A documented suffix rule**: within a family, **x1 marks FTX** and **x7 marks
/CX**. It is clean on the 5-series - ND-530 is `5351/5352` FTX against `5371/5372`
/CX - and carries into the 3-series, ND-110 being `3750` FTX against `3740` /CX.
Read off the product sheets, not inferred.

**The Compact naming resolves into a sequence, not a contradiction**: ND-100
Compact models **I-IV** (326x, 1984), then ND-100 Compact models **11-14** (329x,
1987), then ND-110 Compact models **11-14** (339x). The order manual's **A0/A1/B**
names in 308x/309x remain unreconciled with these. **[?]**

### The older three-digit scheme, proven from SINGEN itself

A scanned SINGEN screen in the order-form archive
(`libsingen/pdf100/CPU-102-2954-0000.pdf`) shows ND's generation program offering
the short codes by name:

```
    S i n g e n  f o r  S i n t r a n   V e r s i o n   J.
 Give CPU type: 100,500,502,900.    : 102
 Give CPU number:                   : 2954
 Give system name:                  : MOSS DAGBLAD A/S
```

So **100, 500, 502 and 900 were the canonical short type codes**, printed in the
prompt itself, and 102 was a real value operators typed.

**The two schemes are separated by serial number**: three-digit types appear with
four-digit serials, four-digit types only ever with serials **16000 and above**.
Prefixes present in the 2711-record order archive: 100 (862), 102 (2), 500 (252),
502 (236), 503 (34), 504 (4), 900 (188), 950 (1).

### Codes that remain unidentified

**3031, 3035, 3082, 3092, 3209** - and **102**, **103** beyond the fact that
they are short-scheme codes. No document in either the mirror or this repo names
them. This was searched properly: the whole sintran.com library index, the machine
register, the mirror's 6052-entry `inventory.json`, and a pdftotext sweep over all
1855 non-order-form PDFs. **The only four-digit `ND-3xxx-*.pdf` product sheet on
the entire site is `ND-3391-B1-EN.pdf`.**

Shape arguments exist and are **not** being promoted to findings: 3082 and 3092
sit beside 3081 and 3091 in both number and delivery date, and 3209 sits in the
320x FTX-for-Compact block with both its machines being TVO's mirrored pair
TVO1/TVO2 at a Finnish nuclear plant. Suggestive only. **[?]**

**What would settle them**: `ND-830053.1` (June 1985) and `ND-830053.2` (February
1987) - the two earlier editions of *SINTRAN III How to order it*. Their value
tables would be contemporaneous with the 30xx and 32xx codes, where the November
1988 edition we have is not. **Neither is downloaded in the mirror.**

### Two OCR readings in doubt

Re-reading the order manual page directly gave two disagreements with the earlier
pass:

- The line for the ND-5200 Compact A10 reads **`5206`** on one reading and
  **`5290`** on another. The A10/A11/B1 sequence argues for 5290, but this is not
  settled. **[?]**
- The last SCSI line reads **`5209`** where **5909** is plainly meant.

### Disk interface on the Satellites

All four Satellite models take a **"Fixed Winchester disk drive"**, and in ND's
vocabulary Winchester means **ST-506**, not SCSI - `ND-11.015.01` is titled
*"5 1/4 inch (ST506) & 8 inch Winchester Disk Controller"* and the ND-5000 guide
writes **"ST-506 (Winchester) disk"** as one term. So the S and T grades differ in
**capacity only**; both are ST-506/MFM. ND used **card 3041** for Winchester/ST-506
and **card 3201** for SCSI plus floppy.

### Three cautions when reading a code off a machine

1. **A plate records the machine as built, not as it stands.** `ND-5830 Serial 129`
   is titled an ND-5830, but both its cabinet plates read `5430.19481` and
   `MPM 5/5400 CX`; the page notes it "has been upgraded to the more modern CPU
   Rallar". The plate is build-time evidence. The same probably explains the
   ND-530/CX carrying a 5171 code.

2. **Card part numbers overlap these codes and are a different namespace.** The
   same machine pages list slot cards as `3095 ND110 CPU&MM 48B`,
   `3094 ETHERNET IF. II`, `3096 OCTOBUS/MPM`, `3033 CPU ND-110 CX`,
   `3002E N100 CPU`, `3202 ND-120CX`. **`3095` is both a CPU-type code in the order
   manual and the ND-110/CX CPU card part number.** Whether that is deliberate
   reuse or coincidence is **not established** - do not merge the two lists. **[?]**

3. **`@LIST-TITLE` output changed between versions.** On SINTRAN III/VSX it prints
   the full banner including `CPU TYPE:` and `CPU NUMBER:` lines. On the older
   NORD-10/S documentation (ND-60.128.01) the whole output is a single
   identification string such as `NORD-10/S - 781012`, with no separate CPU type
   line.

---

## 3. The other numbering schemes

### ND-5000 CPU type and CPU model

Two different numbers, both real, and easy to confuse with the product code:

| System | CPU type | CPU model | I/O processor |
|--------|----------|-----------|---------------|
| [ND-5200](systems/ND-5200.md) | 1 | 2 | ND-110 **[?]** |
| [ND-5400](systems/ND-5400.md) | 2 | 4 | ND-110/CX **[?]** |
| [ND-5500](systems/ND-5500.md) | 2 | 5 | ND-110/CX **[?]** |
| [ND-5700](systems/ND-5700.md) | 2 | 7 | ND-120/CX |
| [ND-5800](systems/ND-5800.md) | 3 | 8 | ND-120/CX |

**CPU type** is physical - how many layers of cards, and whether the cache and AAP
modules are fitted. **CPU model** is what the microcode and the ACCP report back.
Rallar is CPU **type 4**.

The **[?]** marks a conflict: `ND-05.020.01` gives ND-110 and ND-110/CX as the I/O
processor for the three cheaper models, while the 1988 ES product sheet gives
**ND-120** for all three. See the conflicts table in section 2b.

### ND document numbers

Not machine numbers at all, but they look similar: `ND-06.014.02` is the ND-100
Reference Manual, `ND-05.020.01` the ND-5000 Hardware Description. After September
1988 the internal dot was dropped and an 8 prefixed, so `ND-60.230` became
`ND-860230`.

### Part and PCB numbers

Six-digit part numbers identify boards: `320003` is a Samson type III CPU,
`350403` a Rallar CPU module, `324715` the Samson AAP. PCB numbers are separate
again - PCB 3022 is the ND-100-to-ND-500 bus interface, PCB 6201 the Rallar
motherboard. And as caution 2 above says, **four-digit card numbers overlap the
system product codes**.

---

## 4. Sources

- **The definition and the value list**: `ND-830053.3 EN, SINTRAN III How to order
  it`, November 1988, section 1 - in the sintran.com mirror, OCR'd 2026-08-28
- **Configurations**: the ND-5000 ES model L product sheet (November 1988) and the
  ND-110 Satellite Series sheet (June 1987), both OCR'd and visually verified
- **Product numbers and titles**: `mirror/library/libpdpi/`, ND's own
  product-information sheets
- **Machine-list type codes**: `mirror/hardware/hw-system/hw-system.js`
- **CPU type, model and I/O processor**:
  `Reference-Manuals/500/ND-05.020.01 EN ND-5000 Hardware Description.md` **[P]**
- **Real machines**: ndwiki `Category:Individual systems`, 41 preserved machines,
  via Wayback snapshots
- **Where the number goes in the running system**:
  [../../SINTRAN/OS/27-CPU-TYPE-THE-THREE-MEANINGS.md](../../SINTRAN/OS/27-CPU-TYPE-THE-THREE-MEANINGS.md)

---

*Index: [README.md](README.md). Machine list: [MACHINE-LIST.md](MACHINE-LIST.md).*
