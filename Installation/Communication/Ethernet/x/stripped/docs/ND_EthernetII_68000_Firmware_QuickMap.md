# ND Ethernet II (PCB 3094) 68000 Firmware - Quick Map

> Full reference: [`ND_EthernetII_68000_Firmware_COMPLETE.md`](ND_EthernetII_68000_Firmware_COMPLETE.md).
> Updates since the first pass: VR=0x40 confirmed (init_mfp_registers 0x396A); full
> RX+TX paths reversed; RTC ISR 0x3A68; XROUT table 0x1D170; ND-100 8-channel doorbell
> at nd_host_interrupt_handler 0x250E (flags at nd_channel_flags 0x0B56); all 116
> functions and meaningful data globals named.

Companion to `ND_EthernetII_68000_Firmware_ReverseEngineering.md`.
Image: `encos-ser-all-banks-68k.bin` (68000 BE, base 0x0, 512 KB, all banks loaded).
Confidence: [C]=confirmed, [H]=hypothesis, [U]=unconfirmed/from-brief.

## Important addresses

| Addr | What | Conf |
|------|------|------|
| 0x000000 | vector 0 = initial SSP (0x000005C8) | C |
| 0x000004 | vector 1 = initial PC (0x00001CFE) | C |
| 0x000078 | vector 0x1E = OPCOM level-6 handler pointer | C |
| 0x0040A | monitor/console postbox block | C |
| 0x00454 | CPU register dump frame (D0-D7/A0-A6/PC/USP/SR) | C |
| 0x004BA | warm-boot magic 0x55555555 | C |
| 0x004BE | warm-boot restart counter | C |
| 0x18810 | LANCE Am7990 init block base | C |
| 0x1E38 | PLANC exception/trap BRA stub table | C |
| 0x66E00 | PLANC routine/symbol table (name->addr) | C |
| 0x400/0x440/0x880/0x948 | diagnostic mailbox map (NOT this image) | U |

## Important functions

| Addr | Name | Conf |
|------|------|------|
| 0x1CFE | reset_entry | C |
| 0x1A30 | nd_monitor_set_flag | C |
| 0x1A48 | post_and_signal_nd100_scip (writes SCIP 0xEF0080) | C |
| 0x1A66 | save_cpu_context_to_0x454 | C |
| 0x48EA | INITLANCE | C(name)/H(body) |
| 0x4C26 | FATALERROR | C |
| 0x5B60 | RCVRINGAPPEND | C |
| 0x6DA8 | LNMAEVENTS | H |
| 0xBED8 | XMRECEIVER | H |
| 0xEACC | maybe_xmsg_postbox_send_ring (writes SCIP 0xEF0180) | H |
| 0xE73C | PORTCREATE | H |
| 0x106F0 | XMPSEND | H |
| 0x10880 | XMPFREL | H |
| 0x10936 | XMPFREA | H |
| 0x11732 | POSIINITIALIZE | C |
| 0x1179C | POSISTART | C |
| 0x11DC4 | POSIAPPEND | C |
| 0x1192A | POSPGETALL | H |
| 0x1A268 | LNCNSPCOMMAND | H |
| 0x2D350 | POCONFIGURE | H |

## Important data structures

| Name | Addr/type | Fields | Conf |
|------|-----------|--------|------|
| Monitor postbox | 0x40A | +0 counter, +2 code, +4 param, +6 counter2, +8 req flag | C |
| Register dump | 0x454 | 15 longs + PC/USP/SR | C |
| XmsgPostboxSlot | 8 bytes | +0 owner, +2/+4/+6 payload | H |
| LANCE init block | 0x18810 | MODE/PADR/LADRF/RDRA/TDRA (TODO) | C ptr / U fields |

## Command table

No numeric command dispatch table confirmed in the server firmware. Command surface
= named PLANC/XMSG routines above. Diagnostic numeric commands belong to the bank-0
diagnostic firmware [U].

## Interrupt table

| 68000 lvl | Source | Vector | Conf |
|-----------|--------|--------|------|
| 2 | LANCE | autovector | C |
| 3 | MFP (vectored) | 105/107/111-114/116/117 | C(host) |
| 4 | PTC console | autovector | C(host) |
| 5 | MERR parity | autovector | C |
| 6 | ND-100 OPCOM | 0x1E (addr 0x78) | C |
| 7 | ND-100 power low | NMI | C(host) |

MFP vectors: 116=ND request (GPIP I6), 117=write violation (I7), 107=LANCE mem error
(I5), 114/113/112/111=USART RX/RXerr/TX/TXerr, 105=RTC (Timer C).

## Buffer ownership summary

| Buffer | Producer | Consumer | Ownership primitive | Doorbell |
|--------|----------|----------|---------------------|----------|
| Monitor postbox 0x40A | 68000 | ND-100 | request flag / counters | SCIP 0xEF0080 |
| XMSG postbox ring (8 slots) | 68000 | ND-100 | owner word (0=free) | SCIP 0xEF0180 |
| ND-100 -> 68000 msg | ND-100 | 68000 | owner word | MFP GPIP I6 (vec 116) |
| LANCE RX ring | LANCE | 68000 | descriptor OWN bit | LANCE level-2 RINT |
| LANCE TX ring | 68000 | LANCE | descriptor OWN bit | CSR0 TDMD / level-2 TINT |

## I/O quick list

| Addr | Reg | Note |
|------|-----|------|
| 0xEF0040 | MERRSTAT | cleared at boot |
| 0xEF0080 / 0xEF0180 | SCIP / mirror | write = INT12 to ND-100 |
| 0xEF00A0 | LANCE RDP | 0x0048 write = INEA|TDMD TX kick |
| 0xEF00A2 | LANCE RAP | CSR select (0,1,2,3) |
| 0xEF00A8 | XCVPW | transceiver 12V power |
| 0xEF00C0-FF | MFP | not located in this image |

## Things still unknown

- MFP register block location (0xEF00C1+ odd) / VR=0x40 unverified.
- LANCE init-block field values at 0x18810; MAC source.
- RX/TX ISR bodies; memory-probe loop location.
- PO100ports vs PO100messages: two rings or two views.
- No confirmed numeric command dispatch table.
