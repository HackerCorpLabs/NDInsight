# Getting an OWS running, from bare metal to WinLink

A working order for setting up a Norsk Data Office Work Station: boot from floppy, partition and
format the hard disk, put MS-DOS on it, add the ND-OWS layer, then MS-Windows, then WinLink and
the other Windows programs.

Everything below is either quoted from an ND Installation Description in this repository, or read
directly off the diskettes preserved in the
[software archive](https://github.com/HackerCorpLabs/norskdata-software-archive) - the exact file
is named each time. Where no source states a step, it says so rather than inventing one.

**The one rule to remember:** every OWS product has a PC half and a SINTRAN half, and installing
the PC half alone gets you a program that starts and then cannot reach anything. See
[README.md](README.md) §2 for what the host needs.

---

## 0. What you need in front of you

| | Diskette / product | Label on the disk | Held in the archive |
|---|---|---|---|
| 1 | MS-DOS boot + utilities | `DOS5`, or an Ericsson/IBM DOS disk | yes |
| 2 | ND-OWS system diskette (drivers, setup) | `30002EN1A00` | yes, 3 copies |
| 3 | ND PC Starter Kit | `PCSTART-NO2` | yes |
| 4 | ND Connect Module (only for Ethernet/TCP-IP) | `CONNECTB00` | yes |
| 5 | ND keyboard drivers | `380952XXA0` | yes |
| 6 | MS-Windows 2.10 with ND drivers | `30021SV1B01`..`SV4B01`, `INSTALL1`..`INSTALL4`, `ND-DRIVERE` | yes, 10 disks |
| 7 | WinSMX (starter) | `30212NO1A00` | yes |
| 8 | WinLink | `30210NO1A00`..`NO5A00` | yes, 5 disks + an install set |
| 9 | WinPrint | `30211NO1A00` | yes |
| 10 | Desk Top Manager (the MS-DOS alternative to the Windows set) | `30025*` | yes, 10 disks |

## 1. Boot from floppy

Put the MS-DOS boot diskette in drive A: and power on. The OWS is a PC/AT compatible, so this is
ordinary PC behaviour - the ND-specific parts only appear from step 3 onwards.

The archive's `DOS5` disk is MS-DOS 5.00 (`FDISK.EXE` reports *"MS DOS Version 5.00 (C)Copyright
1981-1991 Microsoft Corp"*). The older Ericsson-built machines came with their own system disk
(`#1310C61208`, `#2310C61208`, GW-BASIC 3.11A *"COPYRIGHT Ericsson Information Systems AB 1984"*).

> **Version floor:** `NDBIOS.SYS` from the PC Starter Kit refuses to load on anything older -
> *"Incompatible DOS version (use ver. > 3.0)"*. ND LAN Connect asks for MS-DOS >= 3.1
> ([ND-895556-1-EN](../Installation-Description/ND-895556-1-EN.md)) and the Connect Module for
> >= 3.2 ([ND-895499-1A-EN](../Installation-Description/ND-895499-1A-EN.md)).

## 2. Partition and format the hard disk, install DOS

Standard MS-DOS procedure, with the DOS diskette still in A:

```
A> FDISK                 create a primary DOS partition, make it active, reboot
A> FORMAT C: /S          format and put the system on it
A> MD C:\DOS
A> COPY A:*.* C:\DOS     the DOS utilities
```

Nothing ND-specific happens here. The layout the ND software expects afterwards is `C:\DOS`,
`C:\WINDOWS`, `C:\ND-OWS` and `C:\TEMP` - that is what the ND-OWS `AUTOEXEC.BAT` on the
`30002EN1A00` diskette sets up (§3).

## 3. The ND-OWS layer

Insert the ND-OWS system diskette (`30002EN1A00`). It carries `SETUP.EXE`, an `ND-OWS` directory,
`EIS` and `MAINT`, plus the keyboard tables.

The `AUTOEXEC.BAT` on that diskette shows exactly what an installed OWS looks like:

```bat
SET ND-OWS=c:\nd-ows
%nd-ows%\comms\connect
echo connect has passed
%nd-ows%\drivers\keyboard sw
%nd-ows%\drivers\mouse
path=c:\windows;c:\nd-ows;c:\dos;c:\
set temp=c:\temp
cd \user
prompt $p$g
```

and its `CONFIG.SYS`:

```
country = 46
files=25
buffers=10
device=ansi.sys
device=clock.sys
```

Read that top down and it is the whole ND-OWS boot: the `ND-OWS` environment variable is what
every later installer looks for, `comms\connect` loads the TCP/IP stack, then the ND keyboard
driver with a language argument (`sw` here), then the mouse.

### 3.1 ND PC Starter Kit

The Starter Kit (`PCSTART-NO2`) is the base every other ND product requires - it creates
`C:\ND-OWS` and installs the drivers found on that diskette under `ND-OWS\DRIVERS`:
`NDBIOS.SYS`, `BIODRIV.EXE`, `CSSDRIV.EXE`, `CSTDRIV.EXE`, `EGADRIV.EXE`, `VGADRIV.EXE`,
`W7HDRIV.EXE`, `ZENDRIV.EXE`, `TOSDRIV.EXE`, and a `MAINT` directory with `CHKPORT.EXE`,
`CLSPORT.EXE` and their `.PIF` files for Windows.

> "Before you start the installation of ND LAN Connect, please make sure you have installed the ND
> PC Starter Kit, and rebooted the workstation." -
> [ND-895556-1-EN](../Installation-Description/ND-895556-1-EN.md)
>
> "To install ND LAN Connect manually you must first locate your ND-OWS directory. If this does not
> exist, the ND PC Starter Kit might not be installed." - same sheet

**Reboot after this step.** Both sheets say the parameters it sets only take effect after a
reboot.

### 3.2 ND keyboard drivers

Diskette `380952XXA0` holds `ND-OWS\DRIVERS\KEYBOARD.COM`, `NDKEYB.COM`, `NTXKEYB.COM` and a
keyboard table per language (`KEYBTABL.EN`, `.DA`, `.FR`, `.GE`, `.NO`, `.SW`, `.US`). Its
`INSTALL.TXT` is the forms file the ND installer reads - *"install.txt - Forms file for ND Keyboard
Installation"*, *"(C) Copyright Norsk Data A.S, 1989"*, revision A04 of 19.10.90.

In the running system the driver is invoked from `AUTOEXEC.BAT` with the language as an argument,
as above: `%nd-ows%\drivers\keyboard sw`.

Product sheets: [ND-895487-1A-EN](../Installation-Description/ND-895487-1A-EN.md) (NOTIS PC
keyboard, [380697A](../Installation-Description/ND-895487-1A-EN.md)) and [ND-895558-1-EN](../Installation-Description/ND-895558-1-EN.md) (NORTEXT
Enhanced keyboard, [230189A](../Installation-Description/ND-895558-1-EN.md)).

### 3.3 Connecting to the host

**Serial line:** nothing more to install; the terminal programs use the COM port directly, and the
host side is the OWS Terminal Line Server ([ND-211295](../Installation-Description/ND-895017-S1-EN.md)).

**Ethernet / TCP-IP:** install the ND Connect Module. Its diskette identifies itself in `DISK.ID`:

```
ND Connect Module
B00
DISK1
```

and the Installation Description gives the procedure verbatim:

> "Type `A:INSTALL`... The installation program will enter a menu of different network adaptors.
> If you are installing on a client running 3+Open client software, choose 4 NDIS from the menu,
> otherwise choose the entry that corresponds to your hardware... All the files will be copied to
> the `\ND-OWS\COMMS` directory. CONFIG.SYS will be updated with necessary changes."
> - [ND-895499-1A-EN](../Installation-Description/ND-895499-1A-EN.md)

The installer script on the diskette (`INSTALL.DAT`) confirms where it puts things and which
interrupts it takes:

```
@DefineProject
    @Name     = "ND Connect Module"
    @Version  = "B00"
    @OutDrive = C
    @Subdir   = @Getenv "ND-OWS"
@EndProject
@DefineVars
    @Qstring @SlibInt = "6dh"
    @Qstring @ConnInt = "69h"
```

Interrupt 66 is taken by `PKTDRVND.SYS` / `DISPKTPM.DOS` on NDIS adaptors (same sheet); the
packet drivers on the diskette cover 3Com EtherLink II (`NDIS\ELNKII.DOS`), Interlan, Ungermann-
Bass and 3+Open, with `INT14.COM` for INT 14h redirection.

After this the `%nd-ows%\comms\connect` line in `AUTOEXEC.BAT` loads the stack at boot.

## 4. MS-Windows

The archive holds **MS-Windows 2.10 in ND's own packaging**: `INSTALL1`..`INSTALL4` (Norwegian
"Oppsett 1-4") and `ND-DRIVERE`, plus the Swedish set labelled `30021SV1B01`..`SV4B01`.

Install Windows from disk 1 in the ordinary way (`A:SETUP`), then take the ND display and printer
drivers from the ND driver disk: `NDEUS.DRV`, `NDEEN.DRV`, `NDENO.DRV`, `NDESW.DRV`, `NDEDE.DRV`,
`NDEGE.DRV` and `FONTDPUS.FON` - one per keyboard/language variant.

The Swedish set also carries Intel's `EMM.SYS` (*"Expanded Memory Manager Version 4.0 Revision A,
Copyright 1985, 1987, 1988 Intel Corporation"*) and printer notes that tell you the driver is
installed but not yet configured - `INFOEPSO.TXT` (Epson LQ), `INFOHP.TXT` (PCL/LaserJet),
`INFOPLOT.TXT` (HP plotters), `INFOPROP.TXT` (IBM Proprinter). Configure those from the Windows
Control Panel afterwards.

> WinLink is "compatible with Microsoft Windows/286 and Microsoft Windows/386 version 2"
> - [ND-610-1-EN](../Product-Info/ND-610-1-EN.md)

## 5. WinSMX, then WinLink, then WinPrint

Order matters: WinLink is started *by* WinSMX (or WinStart), so install the starter first.

> "Either WinSMX (ND 230119) or WinStart (ND 230118) to start WinLink with"
> - [ND-610-1-EN](../Product-Info/ND-610-1-EN.md)

### 5.1 WinSMX

Diskette `30212NO1A00`: `INSTALL.EXE`, and under `BIN` the files `SMXINIT.EXE`, `SMXLIB.EXE`,
`SMXEDIT.EXE`, `WINSMX.HLP`. Run `A:INSTALL` from Windows or DOS as the diskette directs.

What its `INFOPRIV.TXT` tells the installer to do says what the result looks like:

```
d <BACKUP><WINDOWS>\WIN.INI
c (WINSMX)CALL-NAME <WINSMX>\SMXINIT.EXE
w windows load <WINSMX>\SMXINIT
w SMX SMXPATH <WINSMX>
```

- back up `WIN.INI` first
- create a program item calling `SMXINIT.EXE`
- add `SMXINIT` to the `load=` line of `WIN.INI`, so the starter comes up with Windows
- record the SMX path in `WIN.INI`

### 5.2 WinLink

Five diskettes, `30210NO1A00`..`NO5A00`. Disk 1 holds the programs, disks 2-5 the screen fonts per
language (`TD2NO*.FON`, `TD2EN*`, `TD2DA*`, `TD2SW*`, `TD2US*`, `TD2NL*`, `TD2FR*` in 15/25/48/70/84
point). `BIN\INFO.TXT` on disk 1 lists exactly what belongs to the product and which font disk is
wanted:

```
NDTERM.EXE -r        EASYLINK.EXE -r       NDTERM-S.EXE -r
NDETHSEL.EXE -r      NDSERSET.EXE -r       SETUP.BIN -r
no\ -r
en\ -r 203776 "SKRIFTFILER 1"
sw\ -r 203776 "SKRIFTFILER 1"
fr\ -r 203776 "SKRIFTFILER 2"
```

and `BIN\INFOPRIV.TXT` shows the installer backing up the ND configuration and `WIN.INI`, then
creating the program items:

```
d <BACKUPW2NDCONFIG>
d <BACKUP><ND-OWS>\NDCONFIG.
d <BACKUP><WINDOWS>\WIN.INI
c (EMUL)CALL-NAME <WIN3TERM>\NDTERM.EXE
c (EASYLINK)CALL-NAME <WIN3LINK>\EASYLINK.EXE
c (NDTERM-S)CALL-NAME <WIN3LINK>\NDTERM-S.EXE
```

So after installation you have three program items: the terminal emulator (`NDTERM`), the file
transfer (`EASYLINK`), and the serial terminal (`NDTERM-S`).

**Then choose the transport, once:**

- Ethernet: run `NDETHSEL.EXE` to pick which ND server to link to.
- Serial: run `NDSERSET.EXE` to set the line parameters.

The archive also holds a later, compressed WinLink install set (`winlink/3.img`: `INSTALL.EXE`,
`WINLINK.INF`, `WINLINK.INI`, `EASYLINK.~XE`, `WSOCKETS.~LL`, `LESMEG.TXT`) - a self-contained
`INSTALL` that expands the `~`-suffixed files itself.

### 5.2.1 What ND's own WinLink manuals say

Two ND manuals for WinLink itself are held in this repository, and they settle several points the
diskettes alone do not:

- [ND-891092EN1 "How to install WinLink"](../../Reference-Manuals/ND-891092EN1%20How%20to%20install%20WinLink.md)
  - the four-page installation folder.
- [ND-860452EN1 "WinLink User Guide"](../../Reference-Manuals/ND-860452EN1%20WinLink%20User%20Guide.md)
  - "Copyright © 1991 by Norsk Data a.s   Version 1 February 1991" (page 3).

**The product number is ND 230210.** Both manuals state it: *"To install WinLink (ND 230210) you
need"* (ND-891092EN1, page 2, "Read this first!") and *"WinLink has the product number ND 230210."*
(ND-860452EN1, page 4, "What is WinLink?"). That answers the question left open in §8 below and in
[README.md](README.md) §10. The diskettes still carry the media number `30210`; no document here
explains the relationship between the two.

**Prerequisites**, verbatim from ND-891092EN1 page 2:

> To install WinLink (ND 230210) you need:
> - An IBM-compatible PC running Microsoft Windows 3.
> - PC Starter Kit, [ND 230123](../Installation-Description/ND-895499-1A-EN.md) (version A04 or newer).
> - For Ethernet connection: ND Connect Module, ND 230125.
> - SINTRAN software required for PC Starter Kit and ND Connect Module:
>   **Ethernet:** SINTRAN 100: OWS Access Server, [ND 211297](../Installation-Description/ND-211297-1-EN.md) / SINTRAN 500/5000: OWS Access Server,
>   ND 211325. **Serial:** OWS Terminal Server, [ND 211295](../Installation-Description/ND-895017-S1-EN.md)

and, on the same page: *"OWS Terminal Server is a part of PC Starter Kit."*

Four things to note about that list:

- **It says Windows 3.** The PI sheet says the opposite generation: WinLink is *"compatible with
  Microsoft Windows/286 and Microsoft Windows/386 version 2"*
  ([ND-610-1-EN](../Product-Info/ND-610-1-EN.md)). Both statements are kept here with their
  sources; nothing in this repository reconciles them, and the manuals carry no version number for
  WinLink itself that would tell them apart.
- **Neither manual names WinSMX or WinStart as a prerequisite**, although ND-610-1-EN requires one
  of them to start WinLink with. ND-860452EN1 starts every program from the **ND Applications**
  group in Program Manager instead, and mentions WinSMX only as an optional convenience (page 17).
- **[ND 211325](../Installation-Description/ND-895060-2-EN.md) is called "OWS Access Server" here**, while [README.md](README.md) §2 tables that
  number as "CMS Access Server for ND-500/5000" after
  [ND-895060-2-EN](../Installation-Description/ND-895060-2-EN.md). Same number, two names in ND's
  own paperwork.
- **No memory size, no disk size, no MS-DOS version** is stated in either manual. The DOS version
  floor in §1 above still comes only from the Starter Kit and Connect Module sheets.

**Installation order**, ND-891092EN1 page 3, under "Important!":

> The programs must be installed in this order:
> 1. Windows 3
> 2. PC Starter Kit
> 3. ND Connect Module, if Ethernet communication
> 4. WinLink

That puts Windows **before** the PC Starter Kit and the Connect Module. §3 and §7 of this document
install the Starter Kit at step 3, the Connect Module at step 5 and Windows at step 6 - the reverse
relative order. ND-891092EN1 is the only source here that states an order explicitly; the order in
§7 was assembled from the individual product sheets, none of which mentions Windows.

**The procedure itself** (ND-891092EN1 page 3, "From Windows"):

1. Put the diskette into the diskette drive; *"If you have more than one drive, use drive A."*
2. Get **Program Manager** on the screen.
3. From the **File** menu, choose **Run**.
4. Type `A:\INSTALL` and press return.
5. *"The installation is automatic."*

Two side effects the folder names:

- *"If you select a font for the terminal emulator which is not on diskette no. 1, the installation
  program will tell you to change the diskette during the installation."* That matches the five
  archive diskettes - disk 1 programs, disks 2-5 fonts (§5.2 above).
- *"The file NDCONFIG is updated. A copy of the old file will be renamed to NDCONFIG.WI2. It will
  be in the same directory as NDCONFIG."* The installer's `INFOPRIV.TXT` quoted in §5.2 backs up
  `NDCONFIG` and `WIN.INI`; the folder names the backup file for `NDCONFIG` explicitly.

The folder mentions no `CONFIG.SYS` or `AUTOEXEC.BAT` change for WinLink. It only says `NDCONFIG`
and, indirectly through the error messages (§5.2.5), that `CONNECT` belongs in `AUTOEXEC.BAT` and
that a nationality setting lives in `CONFIG.SYS`.

**Multi-user (network) installation**, ND-891092EN1 page 4: install WinLink on the server first,
by the same procedure. *"The last message from the installation program gives you the path for the
program SETUP.EXE. You will use this path later, so write it down."* Every client that is to use
WinLink then runs `SETUP.EXE` from that path on the server.

### 5.2.2 Configuring WinLink after installation

ND-860452EN1 works entirely from the **ND Applications** group in Program Manager. The icons it
names are **NOTIS Terminal** (page 4), **EasyLink** (page 10), **Select Host** (page 20),
**Settings** (page 21) and **Serial** (page 23).

**Terminal type.** ND-860452EN1 page 5, in a box of its own:

> Important!
> When asked for terminal type, answer 93!

**Choosing the host (Ethernet only).** *"If your PC is connected to an OpenLAN network (Ethernet),
you can choose which host computer you want to connect to (if there is more than one host on the
network, and if you are registered as a user on the host computer)"* - page 20. Double-clicking a
host in **Select Host** starts NOTIS Terminal on it; clicking **Select** only records the choice
for the next start. Two notes on the same page: if NOTIS Terminal is running, even as an icon, log
out and log in again after selecting a new host; and connecting to a new host disconnects you from
the current one. The dialog box carries *"Copyright © 1988/89 Norsk Data a.s"*.

**Serial parameters.** The **Serial** icon opens "ND Serial Communication" (page 23) with: port
COM1/COM2, speed 2400/4800/9600, data length 7 or 8, parity even or none, stop bits 1 or 2,
handshake XON/XOFF or hardware, a separate **Host Link** port selection COM1/COM2, and a **Document
Transfer** delay on/off. The manual does not explain the fields - *"See the Microsoft Windows
User's Guide for an explanation of the various fields."* The one hard limit it does give, page 23:

> If you have serial communication and have a 286- or 386SX-computer, you should not use a higher
> transfer speed than 4800 baud. At higher speed you might lose both characters in NOTIS Terminal
> (the terminal emulator) and during file transfer.

**Colours, fonts, cursor.** The **Settings** icon (page 21) sets the colour of normal, inverse and
dimmed text and the background, the screen font, the cursor type (block or line) and how many lines
the screen scrolls per step. Recommended fonts, page 22:

| Screen | Font |
|---|---|
| EGA | EMUL-8X12 FONT |
| VGA | EMUL-8X14 FONT |
| Cornerstone 19" Publisher | EMUL-16X32 FONT |
| Wyse 700 | EMUL-8X19 FONT |

The font list drawn in the dialog on page 21 reads EMUL-16X32, 8X12, 8X10, 8X8, 8X19, which does
not include the 8X14 the table recommends for VGA; the two pages of the manual disagree and there
is no third source here. The error-message section (page 27) says only that *"Normally, there
should be five different fonts listed here"*, and that fewer than five means WinLink was not
installed correctly.

**Clipboard.** Page 7: choose **Edit/Copy** in the terminal window, drag over the area, and confirm
in the "Copy to clipboard?" dialog, then paste into any Windows program. The limit:

> You can only copy ANSI characters to the clipboard. Graphics, special characters, tab settings,
> etc, will either not be copied, or will be translated to ANSI characters and distorted.

**Disconnecting.** Page 8: *"The safest way to disconnect from the host is to press EXIT or give
the SINTRAN command LOG."* ALT+F4 ends the terminal session after a confirmation, but *"you will
still be logged in on the host"*. The same page notes you must stay logged in for WinPrint and
EasyLink to work, so if you only want those, close the terminal window rather than logging out.

### 5.2.3 File transfer - EasyLink

The file-transfer program in the WinLink package is **EasyLink** (`EASYLINK.EXE` on disk 1).
ND-860452EN1 pages 9-19 describe it; no protocol is named anywhere.

- Start it only after logging in through the terminal, then close the terminal window (page 9).
  For users of the older DOS EasyLink: *"Do not press the SYS key to return to the PC."*
- The dialog lists three file systems on each side: **MS-DOS**, **NOTIS-DS**, **SINTRAN** (page 10).
  Transfers always run left to right; to go the other way choose **Reverse Direction** from the
  **Session** menu (pages 14 and 19).
- Names are truncated to the PC's 8+3: *"The SINTRAN file name NEWSLETTER:TEXT becomes NEWSLETT.TXT
  in MS-DOS"* (page 13). Text files convert correctly; other extensions may not.
- Extension mapping is configurable in `WIN.INI` (page 18). Under the heading `[ND EASYLINK]` the
  installed entries are

  ```
  TEXT=TXT
  TXT=TEXT
  ```

  and you add pairs in the same syntax, the manual's example being `SYMB=SMB` / `SMB=SYMB`. The
  constraint: *"The SINTRAN and NOTIS-DS file-type extensions must have four characters. The PC
  file-type extensions cannot have more than three characters."*
- Deleting is off by default: **Delete ON** from the **Options** menu enables it, **Warnings ON/OFF**
  controls the confirmation (page 15).
- Advanced use (page 19): wildcards by leading letters or extension in the **File:** box
  (`89:XLS`); another SINTRAN user with `(NEW-USER)d`; a remote SINTRAN system with the same syntax
  as within SINTRAN, though *"The files cannot be listed in the dialog box"*; the destination list
  is deliberately not refreshed after a transfer; and *"If the PC is rebooted, EasyLink will assume
  that you are logged off."*
- The character set used for national characters in file names *"will depend on the UE language on
  the host computer"* (page 13).
- To hang a fixed transfer off the WinSMX menu (page 17): **Change System Menu**, name the item,
  pick `EASYLINK.EXE` under **ND-OWS** / **ND-UTIL**, and put `NOTIS-DS MS-DOS` under **Startup
  Parameters**.

On **backup**: [README.md](README.md) §3 lists "back up PC files to the server" among WinLink's
functions, after ND-610-1-EN. ND-860452EN1 page 9 describes that as an ordinary EasyLink copy -
*"You can copy PC files to the host to make them available for other users, or as a backup"* - and
names no separate backup program.

### 5.2.4 How WinLink reaches the host - what these manuals do and do not say

**Neither manual names a network protocol.** There is no mention of TCP/IP, Telnet, FTP, sockets or
any packet format in ND-891092EN1 or ND-860452EN1. The statement in [README.md](README.md) §2 that
the Ethernet terminal session is Telnet against a SINTRAN Telnet server therefore stands on its
existing sources ([ND-211297-1-EN](../Installation-Description/ND-211297-1-EN.md) and
[ND-895061-2-EN](../Installation-Description/ND-895061-2-EN.md)) - these two manuals neither
confirm nor contradict it.

What they do state about the path to the host:

- The Ethernet case is *"connected to an OpenLAN network (Ethernet)"* (ND-860452EN1 page 20); the
  serial case is *"connected to the host via a serial cable"* (page 23).
- Ethernet runs through the Connect Module's `CONNECT`, loaded at boot: the error *"No contact with
  communication medium"* is answered with *"CONNECT may not be loaded in AUTOEXEC.BAT. It could
  also be a hardware error."* (page 27). That is the same `%nd-ows%\comms\connect` line quoted in §3.
- The PC-side terminal path goes through a VKM library installed by the Starter Kit: the error
  *"No communication with the DOS VKM library"* is answered with *"Install PC Starter Kit again."*
  (page 26).
- The terminal emulation is ND terminal type 93 (page 5).
- File transfer is EasyLink, with the mechanism unstated.
- The two transports differ in one visible way, page 26 under "No access to host computer":
  *"Check that you are not in a program on the host. This only applies if you are connected via a
  serial line."*

### 5.2.5 Error messages, limits and gotchas

From ND-891092EN1 page 2:

| Message | What the manual says to do |
|---|---|
| "Not enough disk space to install product" | The installation is interrupted. Delete files to make room, then try again. |

From ND-860452EN1 pages 24-27:

| Message | What the manual says to do |
|---|---|
| "There is not enough memory for communication" | Too many programs running at once; close some. |
| "Not enough memory to run the program" / "Low on memory" | Same cause and same answer. |
| "This program has not been installed correctly" | *"You must use the original diskette when you install the program."* |
| "Cannot find the font file: Path/filename" | The nationality in `CONFIG.SYS` no longer matches the nationality of the font files. Either put `CONFIG.SYS` back, or install WinLink again in the matching language version. |
| "The connection has been terminated" | Log in again; if that fails, reboot with CTRL+ALT+DEL and log in again. |
| "No access to host computer" | Check you are logged in; check you are not inside a program on the host (serial lines only); ask the supervisor to check the communication servers. |
| "No more ports available" | Wait a few seconds and retry, or reboot and log in again. |
| "No connection to the host" / "No contact with host computer" | Contact the system supervisor. |
| "No communication with the DOS VKM library" | Install PC Starter Kit again. |
| "No contact with communication medium" | `CONNECT` may not be loaded in `AUTOEXEC.BAT`; could also be a hardware fault. |
| "Configuration file missing" | PC Starter Kit is not installed correctly; install again. |
| "No font entry in NDCONFIG for this screen resolution" | Check the **Settings** font list: fewer than five fonts means WinLink is not correctly installed; five fonts means the screen has no matching font type. |
| "Invalid font entry in NDCONFIG for this screen resolution." | Error in `NDCONFIG`; install WinLink again. |
| "Unknown host" | Choose another host computer with **Select Host**. |

Three behaviours that are not error messages but are worth knowing (pages 23-25):

- **The PC slows down** when WinLink runs alongside several other programs; close some.
- **The cursor disappears** while the terminal screen is being repainted, and does not come back
  until the repaint finishes - which can take a while on a graphics-heavy screen.
- **Graphics come out the wrong size** in host graphics programs (the manual's example is "Nortext
  with graphics option"). The fix edits `NDCONFIG`: `ATTRIB -R NDCONFIG` to make it writable, open
  it in NOTEPAD, find the section `SYSTEM-NAME : OWS.` and the line `OWS-SIZE-MONITOR= .`, note
  what is there, and set

  ```
  OWS-SIZE-MONITOR=NON-STANDARD;273;163
  ```

  where 273 is the width and 163 the height, then save and restart the PC. The manual recommends
  doing this only for Cornerstone 19" Publisher screens, where the useful range is roughly 270-280
  by 160-170; for EGA and VGA screens it gives 250-260 by 140-150 as the normal values.

### 5.3 WinPrint

Diskette `30211NO1A00`: `INSTALL.EXE` and `BIN` with `SPRINT.SYS`, `LPT2SPR.SYS`, `SPRINT.EXE`,
`NDSELSPR.EXE`. `INFOPRIV.TXT` shows what the installation does:

```
w SMX UTIL ND &WinPrint;<WINPRINT>\NDSELSPR.EXE
d <COPYFILE><NETINST><ND-OWS>\DRIVERS\SPRINT.SYS
d <COPYFILE><NETINST><ND-OWS>\SPRINT.EXE
d <UPDAUTOEXECBAT>
```

- adds WinPrint to the SMX utility menu
- copies `SPRINT.SYS` into `ND-OWS\DRIVERS` and `SPRINT.EXE` into `ND-OWS`
- updates `AUTOEXEC.BAT`

Run `NDSELSPR.EXE` to choose which printer in the host's SPRINT system this PC prints to. The host
must therefore have SPRINT ([ND-211056](../Installation-Description/ND-895191-04-NO.md), version >= A02) and, for PC access, the SPRINT Server half
of the OWS Access Server - see [README.md](README.md) §5.

## 6. The MS-DOS alternative: Desk Top Manager

If the machine is not to run Windows, ND's DOS-side environment is **Desk Top Manager (ND-230025)**
- terminal emulation, file transfer and a menu, described in
[ND-250-3-EN](../Product-Info/ND-250-3-EN.md) as the program the OWS-85 was sold with. The archive
holds 10 DTM diskettes (`30025NO1C02`, `30025NO2C02`, `30025SW1C00`, `30025SW2C00`, ...), and one
of them carries the configuration template that names the version:

```
* 230025-C00-EN; NDCONFIG C0C : template file for DTM C
* DTM Section
SYSTEM-NAME : DTM
```

The same disk installs the "STREAM" program for the ND Butterfly tape streamer.

## 7. Order in one page

```
1  boot MS-DOS from floppy                     DOS5 / Ericsson system disk
2  FDISK, FORMAT C: /S, copy DOS to C:\DOS
3  ND PC Starter Kit          -> C:\ND-OWS     PCSTART-NO2      reboot afterwards
4  ND keyboard drivers        -> ND-OWS\DRIVERS 380952XXA0
5  ND Connect Module (Ethernet only) A:INSTALL CONNECTB00       -> \ND-OWS\COMMS
6  MS-Windows 2.10 + ND drivers                INSTALL1..4, ND-DRIVERE
7  WinSMX (starter)                            30212NO1A00
8  WinLink (+ font disks)                      30210NO1A00..NO5A00
9  NDETHSEL (Ethernet) or NDSERSET (serial)
10 WinPrint, then NDSELSPR to pick the SPRINT printer   30211NO1A00
```

Host side, in parallel: SINTRAN III with TCP/IP + Telnet server (or the OWS Terminal Line Server
for serial), plus SPRINT for printing and the OWS Access Server for DS/UE/SIBAS.

> **One source disagrees with this order.**
> [ND-891092EN1 "How to install WinLink"](../../Reference-Manuals/ND-891092EN1%20How%20to%20install%20WinLink.md),
> page 3, states: *"The programs must be installed in this order: 1. Windows 3   2. PC Starter Kit
> 3. ND Connect Module, if Ethernet communication   4. WinLink"* - Windows first, then the ND PC
> software. The list above puts the Starter Kit and Connect Module before Windows, because it was
> assembled from the individual product sheets, none of which mentions Windows at all. See §5.2.1.

## 8. What is not documented here

- **The exact SETUP.EXE dialogue** on the ND-OWS system diskette. No sheet in this repository walks
  through its screens, and the program has not been run.
- **Which DOS version ND shipped with which OWS model.** The archive holds MS-DOS 5.00, Ericsson
  system disks from 1984, and IBM-DOS disks, but no sheet ties a version to a model.
- **Hard disk sizes and partitioning advice.** Nothing in these sheets specifies them - and neither
  WinLink manual gives a memory or disk size either (§5.2.1).
- ~~**WinLink's own ND product number**~~ - **answered.** It is **ND 230210**, stated in both
  [ND-891092EN1](../../Reference-Manuals/ND-891092EN1%20How%20to%20install%20WinLink.md) page 2 and
  [ND-860452EN1](../../Reference-Manuals/ND-860452EN1%20WinLink%20User%20Guide.md) page 4. What is
  still unexplained is how it relates to the media number `30210` on the diskettes.
- **Which Windows version WinLink actually needs.** ND-891092EN1 page 2 says Microsoft Windows 3;
  [ND-610-1-EN](../Product-Info/ND-610-1-EN.md) says Microsoft Windows/286 and /386 version 2. No
  document here says whether these are two releases of WinLink or one statement is wrong (§5.2.1).

## See also

- [README.md](README.md) - what OWS is, all the products, the host side, printing, SIBAS
- [ND-891092EN1 How to install WinLink](../../Reference-Manuals/ND-891092EN1%20How%20to%20install%20WinLink.md)
  - ND's installation folder for WinLink: prerequisites, the install order, `A:\INSTALL`, the
  multi-user variant
- [ND-860452EN1 WinLink User Guide](../../Reference-Manuals/ND-860452EN1%20WinLink%20User%20Guide.md)
  - NOTIS Terminal, EasyLink, Select Host, Settings, Serial, and the error messages
- [ND-895499-1A-EN](../Installation-Description/ND-895499-1A-EN.md) - ND Connect Module, the only
  sheet here with a step-by-step PC installation
- [ND-895556-1-EN](../Installation-Description/ND-895556-1-EN.md) - ND LAN Connect, including a
  manual installation section
- [ND-230115-3-EN](../Installation-Description/ND-230115-3-EN.md) - NSD, for keeping a fleet of
  OWS PCs updated from the host afterwards
