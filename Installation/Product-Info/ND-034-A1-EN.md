## Page 1

# ND 034 Display Panel for ND-100

## INTRODUCTION

The ND-100 computer has an optional display panel. It is controlled by an independent microprocessor which is located on the memory management module. The microprocessor receives data to be displayed from the CPU microprogram and from a digital clock driven by the system RT-clock or the CPU board. Cache hit ratio and degree of utilization of the CPU are also monitored by the display microprocessor.

The display is operated from the console keyboard in OPCOM mode. Hence the display shows the result of the last OPCOM command. The information will be updated at a rate of about 1 kHz, while the keyboard only gives the information once; namely at the time the command is given. So the display will continuously be updated even after leaving the OPCOM mode.

The display offers ease of debugging and service to maintenance personnel. However, it has also a great value to the operator running a SINTRAN system. A trained eye can see by a glance if the system works properly. This «all-is-well» indication is impossible without the display.

## PRODUCT DESCRIPTION

The ND 034 is possible to use if the machine has the memory management module installed. This contains, in addition to the memory management system and cache memory, a display processor. The display processor controls the activity on the display. The display panel may be placed outside the cabinet (in another room, etc.). It therefore has an «OPCOM»-button which has the same function as the corresponding button on the operator's panel, namely that of setting the CPU in Operator's Communication Mode.

In the basic state, the following information is displayed:

- Utilization (= not idle on level 0)
- Hit rate in cache
- Program rings entered (with afterglow)
- Interrupt and paging status indications (with afterglow)
- Active levels (with afterglow)
- Copy of system calendar/clock

```
 _______________________________________
|               ND                      |
|             COMPUTER SYSTEMS          |
|_______________________________________|

```

```
 _________________________________ 
|                                 |
|                                 |  
|                                 |
|  _____________________________  |
| |                             | |
| |          OPCOM              | |
| |_____________________________| |
|                                 |
|_________________________________|
```

```
 _________________________________ 
|                                 |
|     FUNCTION     |     DATA     |
|__________________|______________|
|                  |              |
|     ADDRESS      |      ?       |
|__________________|______________|
```

034-A1-6000-0881

---

## Page 2

# Display Functions

The DATA field displays information in binary or octal format. The possible contents are:

### Active Levels (Only binary)

The active levels in the computer will be shown. There are 16 positions, one for each level. The display is provided with afterglow such that it is possible to observe a single instruction on a program level.

### Register Contents

If a register examine is done, the contents of the register is shown.

### Memory Contents

When a memory examine is done, the contents of the examined cell will be shown.

### Bus Information

If the BUS command is given to display memory access on the ND-100 bus, the data present on the bus will be shown and updated continually.

# The ADDRESS Field

### Calendar Clock

A clock that tracks the operating system clock is shown here displaying day, hour, minute and second. This clock is adjusted by the »UPDATE» command under SINTRAN III. Under the load procedure this clock will be read by the operating system and taken as system clock. The clock is also connected to the stand-by power and will stay correct even in case of a power failure.

### Current Program Counter

During a register examine, the current program counter is shown.

### Memory Address

If a memory examine is done, the address of the memory location examined is shown.

# The FUNCTION Field

The FUNCTION display shows which operator's command is actually displayed in the ADDRESS and DATA fields.

After initialization (Master Clear), if no specific command has been given, Utilization, Hit Rate, Ring and Status information are presented.

# Contact Information

| Location       | Contact Information                      |
|----------------|------------------------------------------|
| Bergen         | tel. 05-229200                           |
| Sundsv.        | tel. 064-653544                          |
| Tromsø         | tel. 067-78754                           |
| Stockholm      | tel. 087-6950, tix. 15255 nordata s      |
| Gothenburg     | tel. 031-295950                          |
| Malmo          | tel. 040-130255                          |
| Copenhagen     | tel. 02-26455, tix. 37775 nd dk          |
| Wiezbaden      | tel. 061-241764, tix. 418760 nds d n     |
| Ferrys-Valdatuz| tel. 02-5804878, tix. 386753 nordata ferr|
| Paris          | tel. 01-22301, tix. 2015 ind paris       |
| Avon           | tel. 077-4541147                         |
| Newbury (Berkshire)| tel. 0635-31465, tix. 849919 norsk d g|
| Boston         | tel. 617-237-7945, tix. 921570 norsk well|

# COMTEC Contact Information

| Location   | Contact Information                      |
|------------|------------------------------------------|
| Trondheim  | tel. 075-16230, tix. 55580 comtec n      |
| Stockholm (Upplands Vasby) | tel. 087-6950, tix. 15255 nordata s|
| Stockholm (Solna) | tel. 087-58758, tix. 13754 vdssen s|
| Odense     | tel. 99-17340, tix. 59850 comtec dk      |
| Ballerup (Copenhagen) | tel. 02-675800                 |
| Düsseldorf | tel. 021-606858, tix. 8887277 comtd d n  |

NOTE: NORSK DATA reserves the right to change specifications without given notice!

---

