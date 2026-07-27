# How Norsk Data actually shipped TCP/IP on SINTRAN - documentary evidence (2026-07-26)

Date: 2026-07-26
Scope: what ND's own product sheets, manuals and user guides say about running TCP/IP on ND-100 /
ND-500 / ND-5000 systems, and specifically over the ND Ethernet II controller (ND 110063 / PCB 3094).

Companion to [ETHERNET-II-FEASIBILITY-AND-MODE-WORD-RE-2026-07-25.md](ETHERNET-II-FEASIBILITY-AND-MODE-WORD-RE-2026-07-25.md),
which is the firmware reverse engineering. **This document is documentary evidence only** - it is what
ND wrote, not what we decoded. Where the two agree it is noted.

Convention: **[V]** quoted from an ND document (file cited). **[I]** inferred. **[OPEN]** not stated in
any document present in this repo.

---

## TL;DR

1. **ND had TWO different TCP/IP products, on TWO different controllers.** [V]
   - **COSMOS TCP/IP Gateway for Ethernet (211185)** - runs on the **Ethernet II** card, ND 110063,
     the same board whose COSMOS firmware we carved.
   - **TCP/IP Basic Module/III (211327)** - runs on the **Ethernet III** card, ND 110513, MF-bus,
     **ND-5000 only**. A completely different Domino-based software stack.
2. **The layer split is TCP on the ND-100, IP on the controller.** [V] Not "the whole BSD stack on
   the card".
3. **ND's own Ethernet II hardware manual documents the framing split we found in the firmware**:
   the COSMOS stack is LLC over 802.3 MAC, the ARPA stack is over **DIX**. [V]
4. **Protocol selection is by which image you download into the controller.** Proven for Ethernet III
   (`PMA-ETH3-TCPI` vs `PMA-ETH3-COSM` vs `PMA-ETH3-SIBR`) [V]; strongly implied for Ethernet II [I].
5. **Whether one Ethernet II card can carry COSMOS and TCP/IP at the same time is nowhere stated.**
   [OPEN] Everything that touches the question points at one protocol per card.
6. **No distribution media for any TCP/IP product exists in this repo** - paper only. [V]

---

## 1. The product family (as documented here)

| Product | Name | Versions documented | Sheet |
|---|---|---|---|
| **211185** | COSMOS TCP/IP Gateway for Ethernet | C07 (90.02.20), D02 (92.01.23); a version B existed | `ND-895070-1A-EN.md`, `ND-895070-2-EN.md` |
| **211327** | TCP/IP Basic Module/III | A (1988), B05 (90.03.29), D00 (91.11.21) | `ND-895061-1A-EN.md`, `ND-895061-2-EN.md` |
| **211154** | COSMOS TELNET/FTP Client | B05 (88.07.06), C, D01 (90.06.19), E02 | `ND-895071-2-EN.md`, `ND-895071-3-EN.md`, `ND0106.PDF` |
| **211566** | SINTRAN Socket Library (SLlib) | A00 | `ND-895175-S1-EN.md` (summary only) |
| **211324** | OpenLAN TCP/IP Access Module/III (bundle of 211325 + 211327 + 211154) | A00 | `ND-895087-S1-EN.md` |
| **211325** | OWS / CMS Access Server for ND-500/5000 | A, B | `ND-895060-2-EN.md` |
| **211299** | SINTRAN NFS Support (NFS *server*) | A (90.03.29) | `ND-895520-1-EN.md` |
| **211998** | X Window System for ND-500/5000 (X11R3) | A | `ND-895566-1-EN.md` |

All sheets are under `..\..\..\..\Installation\Installation-Description\` unless noted; `ND0106.PDF`
and the product-info sheet `ND-211154-A1-EN.md` are under `..\..\..\..\Installation\Communication\TCP\`
and `..\..\..\..\Installation\Product-Info\` respectively.

---

## 2. The critical distinction: two IP products, two boards

### 211185 - Gateway, on OUR board [V]

> "Norsk Data has implemented the IP protocol of the COSMOS TCP/IP Gateway as a separate controller
> with its own processor (Motorola 68000) and its own memory (1/2 Mbyte). This relieves the main CPU
> of much of the overhead from the communication protocol handling. **The controller is the same as
> for COSMOS over Ethernet (ND-110063 Ethernet II Controller).**"
> - `Installation\Product-Info\ND-211154-A1-EN.md` p.2

Its documented protocol set, same sheet p.3:

| Layer | Standard |
|---|---|
| Application | Telnet RFC 854; FTP RFC 959, RFC 765 |
| Transport | TCP RFC 793 |
| Network | IP RFC 791; ICMP RFC 792; **ARP RFC 826**; IP Reassembly RFC 815 |
| **Data Link** | **Baseband Ethernet: DIX 2.0** |
| Physical | Ethernet Accessories: IS 802.3 |

> "ND's TCP/IP implementation for SINTRAN is based on the **UNIX 4.2 BBN implementation**."

Prerequisites [V], `ND-895070-1A-EN.md` p.2 (C07) / `ND-895070-2-EN.md` p.2 (D02): CPU 100 CX / 110 CX
/ 120 CX / 500 / 5000; "Ethernet II controller | ND 110063"; SINTRAN III L06 PatchFile >= 2000B;
BACKUP-SYSTEM >= H (210337); D02 adds USER ENVIRONMENT >= D03 (210518). Cost scales per controller:
"Number of segments (ND-100) | 3+4*NbOfControllers", "Space required on segment files |
120+256*NbOfControllers pages", "you will have to assign an Internet address **for each controller**".

**[OPEN]** Neither 211185 sheet names the on-card image, its banks, or a firmware version - unlike
210580, whose Program Description lists `ENCOS-SER-B0..B3-B<rev>:BPUN` explicitly.

### 211327 - Basic Module, on a DIFFERENT board [V]

> hardware prerequisites: "CPU type (any of the following): **5000**"; "Other hardware: |
> **Ethernet III controller | ND 110513**"
> - `ND-895061-1A-EN.md` p.2, identical in `ND-895061-2-EN.md`

It is a Domino controller product: "To reboot the domino controller(s), run the mode job:
`@MODE (TCP-IP)TCP-BA-REBO:M,,,`" (p.7); "Improved performance in the **Tcp-Ip domino processes**"
(D00 reasons); "the **Eth-III controller** ... was **active on a wrong level**" (p.10).

**Neither 211327 sheet mentions Ethernet II (110063) or PCB 3094 at all.** [V negative]

Board-number note: the product sheets consistently say **ND 110513** for Ethernet III. The number
**324232** appears only in this repo's own schematic index
(`..\..\..\..\Installation\Communication\Ethernet\Schema\README.md`) and is the PCB number.
`ND-895566-1-EN.md` p.2 gives the bus: "Ethernet III controller **(MF-BUS)** (ND-5000 series only) |
110513".

And ND had a preference between the two:

> "In order to run the X Window System with maximum performance it is necessary to have a **TCP/IP
> Basic Module/III running in an Ethernet III controller**. Although it is possible to use an
> Ethernet II controller with the COSMOS TCP/IP Gateway, **this is not recommended**."
> - `ND-895566-1-EN.md` p.2

---

## 3. Where each layer runs [V]

> "**The TCP software is implemented in ND-100 and the IP software is implemented in a separate
> controller.** The COSMOS TCP/IP software runs under SINTRAN III/VSX."
> - `..\..\..\..\Operations\Cosmos\ND-860284-1-EN COSMOS TELNET-FTP Client User Guide.md` sec 1.3

The SINTRAN error-code ranges make the stack explicit [V]
(`ND-895230-1A-EN.md` line 880, `ND-895230-1G-EN.md` line 725):

```
006000B - 006077B   Domino Operating System
044600B - 044677B   Ethernet Media Access
047200B - 047277B   AIP - ARPA Internet Protocol
047300B - 047377B   TCP/IP - Transmission Control Protocol / Internet Protocol
047400B - 047477B   SLIB - Socket Library
047600B - 047677B   Telnet Server
101500B - 101577B   MF-bus Controller
```

**AIP = ARPA Internet Protocol** [V], confirmed independently by the range table above and by
`ND-860284-1` Appendix D ("This section gives an explanation of **AIP (ARPA Internet Protocol)** error
messages"). Its error texts say where it sits: `AIPpiocError : PIOCOS error`, `AIPportError : fatal in
IOC port message system`, `AIPxmsgError : XMSG error`, `AIPBADmaBuffer : BAD address of MA(Media
Access) buffer`, `AIPdeadMA : medium access dead, reload system` - i.e. the IP layer runs on the
controller, over **PIOCOS** ports (`AIPportError : fatal in IOC port message system`) and XMSG, above
the MA (media access) layer. Same transport ENNS0 uses.

> This `AIPpiocError : PIOCOS error` text is also the documentary confirmation that the Ethernet II
> **controller OS is PIOCOS** - see the RTOS reverse-engineering hub
> [../../../../Installation/Communication/Ethernet/RE/PIOCOS/README.md](../../../../Installation/Communication/Ethernet/RE/PIOCOS/README.md).

The `AIP-*:SYMB` files are ND's UNIX config equivalents [V] (`ND0106.PDF` p.2): `AIP-HOSTS` =
Internet hosts file, `AIP-SERVICES` = services, `AIP-NETWORKS` = networks, `AIP-PROTOCOL` = protocols.
`AIP-CONFIG:SYMB` "contains information on all the local Ethernet-controllers running TCP/IP"
(`ND-895628-1-EN.md`).

Applications reach the stack either through the bundled Telnet/FTP servers or through the separate
**SINTRAN Socket Library (211566 / SLlib)**. `ND-20034-1 EN` describes "**S-LIB** ... Super Kernel
Library - common interface to XMSG, **IP** and Nucleus", and gives max message sizes: Ethernet 1500,
**TCP/IP 1024**, XMSG 2500.

---

## 4. ND's own hardware manual documents the framing split

`..\..\..\..\Reference-Manuals\Devices\ND-12.055.1 EN Ethernet II Controller.md` - the manual for our
exact card - Appendix C, "OSI Model and ND's Implementation" [V]:

| Layer | COSMOS stack | ISO stack | ARPA stack |
|---|---|---|---|
| 7 | COSMOS | ISO Applications | FTP, Telnet |
| 5 | XMSG | | nil |
| 4 | ISO TC.4 | | ARPA TCP* |
| 3 | LNCN | ISO CNLSNL | ARPA IP |
| 2 | **LLC1 / MAC** | | **LLC1 / DIX** |

Footnote: "**\* to be implemented late 1987 by Ethernet II**". Glossary: "**DIX**: Digital-Intel-Xerox
specified protocol".

Also in that manual:
- Appendix B prints the 802.3 and Ethernet frame formats side by side: 802.3 has `SFD 10101011` +
  **length** + pad, Ethernet has `sync 11` + **type**. "An IEEE 802.3 and Ethernet frame should be
  identical. Each protocol divides the frame into fields with different names."
- Section 2/1 conformance: "IEEE 802.3, ECMA 80/81/82, ISO/DIS 8802/3" - **DIX is deliberately not in
  that list**, consistent with the shipped COSMOS firmware.
- Figure 1 shows the shipped layer 2 as `IEEE 802.2 LLC` over `IEEE 802.3 MAC`.

**This is direct documentary corroboration of the firmware carve**: the COSMOS path we decoded (LLC
DSAP/SSAP 0xA8, control 0x03, 802.3 length field) is the "COSMOS stack" column; TCP/IP needs the
"ARPA stack" column, which is DIX - exactly what `g_mode8023LengthField = 0` produces.

Note the ARPA layer-2 cell reads "LLC1 / DIX", so ND may have supported either encapsulation there;
the 211185 sheet's Data Link line says DIX 2.0. **[OPEN]** whether LLC/SNAP was also offered.

---

## 5. Protocol selection = which image you download

**Proven for Ethernet III** [V], `..\..\..\..\Reference-Manuals\500\ND-820026.1 EN DOMINO and NUCLEUS Software Guide.md`
(module 22B = ETH3), where PROMAN downloads one of:

```
PMA-ETH3-TCPI:IMAG   % for communication TCP/IP
PMA-ETH3-COSM:IMAG   % for communication COSMOS
PMA-ETH3-SIBR:IMAG   % for SIBAS
```

described as "Necessary when more than one type of product runs on processors with the same module
number. Examples are TCP/IP, COSMOS and SIBAS-communication, all running on Ethernet-III." A live
`LIST-CONFIGURATION` sample shows a slot booted with `(UTILITY)PMA-ETH3-TCPI`.

Corroborating file evidence [V]: `ND-895061-2-EN.md` p.7 tells the installer to save
`'(UTILITY)PMA-ETH3-TCP*:IMAG'` before upgrading; `ND-896058-2-EN.md` lists
`PMA-ETH3-TCIPNFS:IMAG` at **164 pages** (sibling `PMA-SCSI-B010:IMAG` at 249 pages - so `PMA-*:IMAG`
is ND's Domino controller-image family).

**[I] for Ethernet II.** No document in this repo names an Ethernet II TCP/IP image file. But 211185
must load *something*: its cold-start includes `@MODE (TCP-IP)TCP-IP-LO:MODE,,,` (C07) /
`TCP-IP-LOAD:MODE` (D02), diskette 2 carries a **PIOC-MONITOR** "meant for use by ND service personnel
only", and the product's data link is DIX 2.0 with ARP - all impossible under the ENCOS server we
carved. The words "firmware", "PROM" and "BPUN" appear nowhere in the 211185/211327/211154 sheets.
[V negative]

**The only Ethernet II downloadables present in this repo remain the COSMOS ENCOS set**
(`ENCOS-SER-B0..B3-B01:BPUN`). [V]

---

## 6. Coexistence: what is and is not stated

**[OPEN] - no document in this repo says whether one ND 110063 can carry COSMOS and TCP/IP at the
same time.** Both sweeps searched for "dedicated", "separate controller", "may not be shared",
"simultaneously", "at the same time" across `Installation\`, `Operations\` and `Reference-Manuals\`.

What exists, all pointing the same way but none decisive:

- [V] "**COSMOS and TCP/IP protocols on the same physical Ethernet.**" (`ND-211154-A1-EN.md`,
  Benefits). That is a statement about the **cable**, not the card.
- [V] The requirements lists attach the qualifier to the board: "One ND 110063 Ethernet II Controller
  **for TCP/IP**" (`ND-211154-A1-EN.md`); "**Ethernet II controller for TCP/IP** ... | 110063"
  (`ND-895566-1-EN.md`).
- [V] Multi-controller language is always about multiple *TCP/IP* controllers, never coexistence:
  "Possibility of running TCP/IP in **several Ethernet III controllers** simultaneously"
  (`ND-895061-1A-EN.md`); "**All controllers with TCP-device number >= 4 are running the TCP/IP Basic
  Module/III**" (`ND-895628-1-EN.md`).
- [V] The COSMOS Ethernet II Option installation description
  (`..\..\..\..\Installation\Communication\Ethernet\ND-210580-02-EN.md`) contains **zero occurrences
  of "TCP"**.
- [V] `..\..\..\..\Installation\Product-Info\ND-110587-A1-EN.md` lists "incompatible high-level
  protocols such as XNS, **TCP/IP**, DECnet, OSI, and **COSMOS**" - ND treated them as distinct,
  mutually incompatible link-borne protocol families.
- [V] Ethernet III proves one image per protocol per slot (section 5).

**[I] Conclusion: one protocol per controller. Dual stack needs two cards.** Well supported, not
quoted.

---

## 7. The clients are pure XMSG clients - a practical shortcut

[V] `ND0106.PDF` p.6-7: "The presence of the COSMOS TCP/IP software is necessary to run the
FTP-CLIENT and TELNET-CLIENT. Check it by doing the **LIST-NAMES command in the XMSG-COMMAND
processor**. If installed, the TCP/IP port names are present." The two cases are literally:

- local stack -> XMSG name **`*TCP.`** ("=> TCP port name")
- remote stack -> XMSG name **`*TCPGATE`** ("=> TCP port name for remote clients")

[V] `ND-895071-2-EN.md`: "If TCP/IP is on a remote computer ... then define `*TCPGATE` in `XMSG` where
the servers are installed. (Note that TCP/IP can only be on a remote computer if you are using the
COSMOS TCP/IP Gateway product)."

[V] The clients are ordinary dumped-reentrant user programs (`FTP-CLIENT-D:PROG`,
`TELNET-CLIENT-D:PROG`, `RSH-CLIENT-D:PROG`), requiring **0 segments and 0 RT descriptions**.

**Consequence for emulation [I]:** a high-level-emulated controller does not have to satisfy the ND
Telnet/FTP clients at the MAC level at all. It has to register and serve the right **XMSG port names**.
That is a far smaller target than a faithful DIX MAC path.

[V] One hard ND limitation, `ND-860284-1` p.14: "an ND host **cannot be an IP (Internet) gateway**, a
feature that is standard in the BSD 4.2 version." No IP routing. Real BSD sockets and routing existed
only under **NDIX** on ND-500.

---

## 8. What this means for the Ethernet II TCP/IP question

- The firmware finding is **confirmed, not contradicted**. ND's answer to "we need DIX on the wire"
  was a different software load (211185 on Ethernet II) or a different board entirely (211327 on
  Ethernet III). The ENCOS server speaking only 802.3+LLC A8/A8/03 is by design, not a defect.
- `g_mode8023LengthField = 0` is very likely the exact configuration the 211185 on-card image uses.
  [I] - the documents state the DIX 2.0 result, never the mechanism.
- The ND-100 never sees a raw MAC frame in any shipped ND product. Every path crosses to the
  controller as XMSG / PIOCOS port messages.
- Dual-stack on one card is unsupported as far as any ND document here shows.

---

## 9. Artifacts worth hunting, in priority order

| Want | Why |
|---|---|
| **211185 distribution media** (`211185C-XX-01D` / `211185D-XX-01D`, diskette 2 has the PIOC-MONITOR) | A working DIX-2.0 image for OUR card. Would show the mode word being set and the whole raw-frame path. **Highest value.** |
| **ND-60.197 EN Ethernet Basic Software Programmer Guide** (product 210582A, Feb 85, ~100pp) | Catalogued in `ND-40.004.7 EN`; its entry says the reader "should be familiar with ... **DIX 2.0 and IEEE 802.3**". The programmer-level spec for exactly this seam. |
| **ND-830107.01 / .03 EN OpenLAN Network Supervisor Guide** | Shipped free with 211185 and 211327; the authoritative controller-configuration guide. Likely settles coexistence. |
| **ND-860284.02 EN** COSMOS TELNET/FTP User Guide 2nd ed. | Repo has only the 1st ed. |
| **ND-895175-1 EN** PI for SINTRAN Socket Library 211566 | The socket API surface; repo has only the summary sheet |
| 211327 media (`211327B-XX-1D`, `211327D-XX-01D`...) | `PMA-ETH3-TCP*:IMAG` - a complete ND IP stack image, even if for the other board |
| **ND-250253B** | Named as "ND-no. for Source" for 211154 - client source listing |

[V] No distribution media for **any** TCP/IP product exists anywhere in this repo - verified by
independent sweeps for `*211327*`, `*211154*`, `*211185*`, `*211299*`, `*.img`, `*.IMAG`, `*BPUN*`,
`*AIP-*`. Only COSMOS media is present.

---

## Sources

Product sheets under `..\..\..\..\Installation\Installation-Description\`: `ND-895070-1A-EN.md`,
`ND-895070-2-EN.md`, `ND-895061-1A-EN.md`, `ND-895061-2-EN.md`, `ND-895071-2-EN.md`,
`ND-895071-3-EN.md`, `ND-895060-2-EN.md`, `ND-895087-S1-EN.md`, `ND-895520-1-EN.md`,
`ND-895566-1-EN.md`, `ND-895628-1-EN.md`, `ND-895230-1A-EN.md`, `ND-895230-1G-EN.md`,
`ND-896058-2-EN.md`, `ND-210580-02-EN.md`.
Product info: `..\..\..\..\Installation\Product-Info\ND-211154-A1-EN.md`, `ND-110587-A1-EN.md`.
Media listing: `..\..\..\..\Installation\Communication\TCP\ND0106.PDF`.
Manuals: `..\..\..\..\Reference-Manuals\Devices\ND-12.055.1 EN Ethernet II Controller.md`,
`..\..\..\..\Reference-Manuals\500\ND-820026.1 EN DOMINO and NUCLEUS Software Guide.md`,
`..\..\..\..\Reference-Manuals\ND-40.004.7 EN Documentation Catalogue.md`,
`..\..\..\..\Reference-Manuals\ND-20034-1-EN ND-Specific Programming & Advanced PLANC.md`,
`..\..\..\..\SINTRAN\Release-Documentation\ND-860230-7A-EN SINTRAN III - Release Information - M-Version.md`.
User guide: `..\..\..\..\Operations\Cosmos\ND-860284-1-EN COSMOS TELNET-FTP Client User Guide.md`.

**Hard negative worth recording:** `..\..\..\..\Reference-Manuals\ND-60.134.2 EN SINTRAN III Communication Guide.md`
(ND-60.134.02) is dated **November 1981**, predates ND Ethernet entirely, and greps to **zero hits**
for tcp / telnet / ftp / ethernet / ENNS / 802 across all 5740 lines. It is not a source for any of
this, despite the name.
