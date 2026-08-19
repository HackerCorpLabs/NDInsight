## Page 1

# WinLink

[Illustration: People working at a desk, one highlighted in red]

**NorskData**

---

## Page 2

# WinLink User Guide

ND-860452EN1

Is WinLink installed on your PC? If not, read the folder "How to install WinLink".

---

## Page 3

# Contents

## What is WinLink?
........................................................................... 3

## Using the PC as a terminal
........................................................................... 4
- Logging in ................................................................................ 4
- Resize and iconize windows ..................................................... 5
- Copy to the clipboard ............................................................. 6
- Scrolling the screen picture ..................................................... 7
- Resize the terminal window .................................................... 7
- Disconnect from the host computer ....................................... 7

## Transferring files from the host computer to your PC and back
............................................................................................. 8
1. Start EasyLink ......................................................................... 8
2. Choose which host files to transfer ............................................... 10
3. Choose the destination .......................................................... 11
4. Copy the files ......................................................................... 12
5. Transfer the same files back to the host computer ................. 13
- Deleting files .............................................................................. 14
- Protection against deleting ....................................................... 14
- Shortcuts ................................................................................. 15
- Get the correct file-type extension! ............................................ 17
- Useful information for advanced users ....................................... 18

## Select which host computer to connect to
............................................. 19

## Set screen colours and fonts
.................................................................. 20
- About screen fonts .................................................................. 21

## Defining communication settings
................................................................ 22

## Errors that may occur
................................................................................ 23
- Your PC slows down ................................................................ 23
- The cursor disappears .............................................................. 23
- Graphics are the wrong size ..................................................... 23
- Error messages ........................................................................ 24

---

MS-DOS® is a registered trademark and Windows™ is a trademark of Microsoft Corporation.

For more information about ND software products, contact your local ND representative or  
Norsk Data a.s, P.O Box 25 Bogerud, N-0621 Oslo 6  
Telephone: (02) 62 68 70   Telefax: (02) 62 68 71

The information in this manual is subject to change without notice. Norsk Data a.s assumes no responsibility for any errors that may appear in this manual.

Copyright © 1991 by Norsk Data a.s    Version 1 February 1991

---

## Page 4

# What is WinLink?

WinLink is the tool that lets you communicate with an ND SINTRAN host computer from your PC.

WinLink gives you the best of two worlds. On the one hand, you have your PC with your integrated and user-friendly Windows applications. On the other hand, you have the host computer with its routines for central backup, shared resources, and an advanced electronic mail system. WinLink is the "link" that makes the communication between the PC and host simple and efficient.

WinLink consists of several programs. With these programs you can:

- connect to a SINTRAN host and use the PC as a terminal
- copy files from the PC to the host and vice versa
- select the host computer you want to connect to (for Ethernet communication)
- select screen colours and fonts when you use the PC as a terminal
- define communication parameters (for serial connection)

WinLink consists of fully integrated Windows programs that you can activate at any time. Like all Windows programs, you can run WinLink programs in separate windows, iconize and resize windows, etc.

WinLink has the product number ND 230210.

---

## Page 5

# Using the PC as a Terminal

One of the most important functions of WinLink is that you can use your PC as a terminal against a SINTRAN host computer.

## Logging in

![Notis Terminal](image.jpg)

1. In **Program Manager**, under **ND Applications**, double-click on the icon for **NOTIS Terminal**.
2. You will see a window something like this:

   ```
   +-----------------------------------------+
   | NO NOTIS Terminal                       |
   | File Edit Settings Options              |
   |                                         |
   | 11:39:39 26. March 1990 HORØ-DONALD     |
   |                                         |
   | Welcome to COMMUNICATION                |
   | (Product Innovation)                    |
   |                                         |
   | Please enter user name :................|
   | & corresponding password:               |
   +-----------------------------------------+
   ```

3. Log in as usual.

---

| Important!                                      |
|-------------------------------------------------|
| When asked for terminal type, answer 93!        |

---

## Page 6

# Resize and iconize windows

The terminal connection to the host computer is running in a separate window, the **terminal window**. You can change the size and iconize this window.

Point at the **top right corner** of the terminal window, and drag the pointer downwards until the window has the size you want. In this way you can have the terminal connection running in one window, and a PC program in another, and quickly jump back and forth between the two just by clicking on the window you want to work in.

```
+-------------------------------------------------+
| ND NOTIS Terminal                               |
|-------------------------------------------------|
| File  Edit  Settings  Options        | Help | * |
|-------------------------------------------------|
| ---> S I N T R A N - I I I                      |
| O B E L I X >                                   |
|                                                 |
+-------------------------------------------------+
```

You can also iconize the terminal window by clicking on the **minimize box** in the top right corner of the window.

```
+-------------------------------------------------+
| ND NOTIS Terminal                               |
|-------------------------------------------------|
| File  Edit  Settings  Options        | Help | * |
+-------------------------------------------------+

 
        ←

     NOTIS Terminal
```

---

## Page 7

# Copy to the Clipboard

You can copy text from the screen picture in the terminal window to the clipboard. This is useful in many situations. Let us say you are working with a document in PageMaker. A colleague has written a contribution, and mailed it to you over NOTIS-ID. You can then copy the text directly from ID in the terminal window to the clipboard, and from there paste it into your PageMaker document.

1. Select **Edit/Copy** from the menu in the terminal window. The screen changes colour, and the pointer changes its shape to a +.

2. Mark the text you want to copy by placing the pointer, pressing the mouse button, and dragging the pointer across the text area you want to copy. The area is highlighted.

```
  ┌───────────────────────────────────────────────────────────────┐
  │ Nb NOTIS Terminal - Copy                                      │
  │ WP: Documents Regions Services NOTIS Functions Menus UDX      │
  │ Fetch Update Store Append List Inspect Delete                 │
  │ (.......T1........T2........T3........T4........T5........T6..│
  │                                                               │
  │ ┌───────────────────────────────────────────────────────────┐ │
  │ │         ND NOTIS Terminal - Copy                           │ │
  │ │ Copy to clipboard?                                         │ │
  │ │                                                           ▼│ │
  │ │      ┌───┐   ┌──┐   ┌─────┐                               │ │
  │ │      │Yes│   │No│   │Cancel│                               │ │
  │ │      └───┘   └──┘   └─────┘                               │ │
  │ └───────────────────────────────────────────────────────────┘ │
  └───────────────────────────────────────────────────────────────┘
```

3. When you release of the mouse button, you are asked whether you want to copy the marked area to the clipboard. Answer 'Yes' if you are happy with the area you have marked. Answer 'No' if you want to mark a different area; answer 'Cancel' to interrupt the operation.

You then activate the window containing the program in which you want to include the text, and paste it the normal way.

> **Note:**  
> You can only copy ANSI characters to the clipboard. Graphics, special characters, tab settings, etc, will either not be copied, or will be translated to ANSI characters and distorted. This means the function is best suited for copying text.

---

## Page 8

# Scrolling the screen picture

If the terminal window is too small to show the entire screen picture of the application you are working with, you can scroll the screen by using the scroll bars. If you select **Settings/Auto-scroll**, the screen will automatically scroll to show the part of the screen that contains the cursor.

> **Note**  
> If you cannot see the entire screen picture, even if you have maximized the terminal window, you may use a screen font that does not correspond to the screen you have. See page 20.

# Resize the terminal window

If you have changed the size of the terminal window, you can set it back to its original size by selecting **Resize** from the **Options** menu.

# Disconnect from the host computer

The safest way to disconnect from the host is to press **EXIT** or give the SINTRAN command **LOG**.

You can also break the connection to the host by pressing **ALT + F4**. The system responds by displaying "This will end your terminal session", and you have to confirm by clicking on **OK**. However, you will still be logged in on the host.

> **Note**  
> Remember that you have to be logged in to be able to use certain programs on the PC, for instance WinPrint and EasyLink. If you only want to use these programs, and do not want to use any of the applications on the host, we recommend that you close the terminal window.
> 
> Don't forget the security! Never leave your PC with an open connection to the host!

---

## Page 9

# Transferring files from the host computer to your PC and back

Another important function in WinLink is the possibility to copy files between the host and the PC. You can copy PC files to the host to make them available for other users, or as a backup. You can also copy large files that occupy a lot of space on your hard disk, and keep them on the host until you need them.

It is also useful to be able to fetch information from the host and process it on your PC, for instance information from your intray in NOTIS-ID.

For these tasks, you use the program in the WinLink package called EasyLink.

## 1. Start EasyLink

If you have not already done so, log in on the host computer as described on page 4.

Close the terminal window.

### Note

For those who have used EasyLink before:

Do not press the **SYS** key to return to the PC.

---

## Page 10

# EasyLink Start Guide

This is how you start EasyLink:

In **Program Manager**, under **ND Applications**, double-click on the icon for **EasyLink**.

You see the dialog box for EasyLink.

```
 _________________  ______________________________  ______________________________
|                 ||  ND EasyLink                ||                              |
|  EasyLink Icon  ||  File  Options  Session     ||  CHOOSE DESTINATION FILE     |
|_________________||_____________________________|   SYSTEM.                     |
                                              ___________________________________|
 _______________________________   __________   |          Transfer to:          |
| ND EasyLink                    | |  System   | |                               |
| File  Options  Session         | |___________| | List systems:                 |
| _____________________________  | |           | | MSDOS                         |
|| CHOOSE SOURCE FILE SYSTEM.  | | |  OK       | | NOTIS-DS                      |
||                             | | |___________| | SINTRAN                       |
||                             | |             | |                               |
|| Transfer from:              | |             | | ____________________________  |
||                             | |             | ||  System                   |  |
|| List systems:               | |             | ||__________________________/  |
||  MSDOS                      | |             | |                               |
||  NOTIS-DS                   | |             | |                               |
||  SINTRAN                    | |             | |  OK                           |
||                             | |             | |_______________________________|
||_____________________________| |             | |      Logged-on to host.       |
| |System                       ||             | |        ________________       |
| |_____________________________| |____________| |=======|     Transfer    |  ___|
|                                               |=====|          EXIT          |
|______________________________________________|                            |
|______________________________________________|____________________________|
```

---

## Page 11

## 2. Choose which host files to transfer

> ### Note
> When you start choosing which host files to transfer, the system will need a few seconds to display the file names. This is indicated by an hourglass on the screen. If you do not wish to list all files, you can interrupt the process by pressing the x key or clicking on the Stop Listing button.

Look at the **System** button on the left. The text in the button should be grey. If the text is not grey, click on the button.

```
  List systems:               System                 List systems:               System
  ┌───────────────┐          ┌───────┐              ┌───────────────┐          ┌───────┐
  │ MS-DOS        │          │       │              │ MS-DOS        │          │       │
  │ NOTIS-DS      │          │  OK   │              │ NOTIS-DS      │          │  OK   │
  │ SINTRAN       │          │       │              │ SINTRAN       │          │       │
  └───────────────┘          └───────┘              └───────────────┘          └───────┘

                     Logged-on to host.                 Transfer                EXIT
```

Under **List systems** on the left, double-click on the file system you want to transfer files from (NOTIS-DS or SINTRAN).

*If you choose SINTRAN:*

- Click once on the file you want

*If you choose NOTIS-DS:*

- Double-click on the drawer you want
- Double-click on the folder you want
- Click once on the file you want

You can also transfer more than one file at a time. Choose several files by holding down the **SHIFT** key while clicking on the files you want to transfer.

---

## Page 12

# Choose the destination

Look at the **System** button to the right. The text in the button should be grey. If the text is not grey, click on the button.

```
+------------------------------------------------+
| ND                                  ND EasyLink |
| File   Options   Session                        |
+------------------------------------------------+
| (ROBERT EUMSEN)         NOTIS-DS                |
| Drawer:                                        |
| Folder:                                       |
| Document:                                     |
|                                                |
| List drawers: System   List systems: System    |
| +--------------+    +---------------------+    |
| | [COSMOS SAKERI]   | MS-DOS               |    |
| | [DIVERSE]         | NOTIS-DS             | OK |
| | [LOCAL AREA NETWORKS] | SINTRAN          |    |
| | [MAILBOX]                                   |
| | [MARKETING NEWS]                            |
| | [NOD]                                    CR |
| | [NOTIS-PM]                                  |
| |                |                            |
| +--------------+    +---------------------+    |
| List complete.      Transfer   EXIT            |
+------------------------------------------------+
```

Under List systems to the right:

- Double-click on **MS-DOS**.
- Find the directory that you want to place files in, and double-click on it.

---

## Page 13

# 4. Copy the files

Click on the **Transfer** button

The files are now copied from the host computer to your PC. The file names are retained but sometimes abbreviated (see the box below).

Click on the **Exit** button

Go to the directory where you stored the files, and start working.

```
┌─────────────────────────────────────────────────────────────────────────┐
│ Note                                                                    │
│                                                                         │
│ On your PC, file names can have up to eight characters plus a three     │
│ character extension (for example BULLETIN.TXT).                         │
│                                                                         │
│ If a SINTRAN or NOTIS-DS file name is too long, the PC removes the      │
│ surplus characters. The SINTRAN file name                               │
│ NEWSLETTER:TEXT becomes NEWSLETT.TXT in MS-DOS. The extension :TEXT     │
│ is automatically abbreviated to the correct PC extension .TXT.          │
│                                                                         │
│ For the file types other than text files, the abbreviation of the       │
│ extension will not necessarily be correct and you may have to rename    │
│ the transferred files to get the correct extension. However, if you     │
│ know how to edit the WIN.INI file, you can insert an instruction for    │
│ the correct abbreviations in this file (see page 17).                   │
│                                                                         │
│ The character set used to display national characters in a file name    │
│ will depend on the UE language on the host computer.                    │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Page 14

# 5. Transfer the same files back to the host computer

When you have finished working on the files, you can transfer them back to the host as follows:

Start EasyLink (see page 8).

From the **Options** menu, choose **Full Menus**.

```
+---------------------------------------------------------+
| ND EasyLink                                             |
| File | Options                                           |
|---------------------------------------------------------|
|  Full Menus                                              |
|-------------------------------------------------------------------------|
|       CHOOSE DESTINATION FILE SYSTEM.                                   |
+---------------------------------------------------------+
```

From the **Session** menu, choose **Reverse Direction**.

```
+---------------------------------------------------------+
| ND EasyLink                                             |
| File | Options | Session                                |
|---------------------------------------------------------|
|  Reverse Direction                                      |
|---------------------------------------------------------|
| Prior session                                           |
| Reset                                                   |
| Reverse Direction                                       |
|                      To MS-DOS                          |
+---------------------------------------------------------+
```

Under **List files** on the left, click on the files you want transferred back to the host.

Click on **Transfer**.

> **Note**  
> ND EasyLink always transfers files from left to right in the dialog box.

---

## Page 15

# Deleting Files

From the **Options** menu in EasyLink, choose **Delete On**.

Under **List files**, click on the files you want to delete.

Click on **Delete**.

If you have chosen Warnings ON (see below), you will see a dialog box asking you to confirm the delete operation.

When you have finished deleting, choose **Delete OFF** from the File menu.

# Protection Against Deleting

From the **Options** menu in EasyLink, you can choose whether or not you want to receive a warning message when deleting files.

**Warnings ON**: If you delete or transfer a file, you are asked to confirm the action. This prevents you from deleting or overwriting files you want to keep.

**Warnings OFF**: If you delete a file, you get no warning first, but you see that the file has been deleted. If you transfer several files, you will see how many of the files are transferred. You can interrupt the process at any time by pressing the space bar.

```
+-------------------+
|  ND EasyLink      |
+-------------------+
| File Options      |
| Session           |
|  Warnings ON      |
| Automatic Settin\ |
| Delete ON         |
| Short Menus       |
|                   |
| To MS-DOS         |
+-------------------+
```

---

## Page 16

# Shortcuts

**ND EasyLink remembers**

When you start EasyLink from a Windows program, the list box to the left will contain the same files as the last time you worked in that program.

Perhaps you keep all the files for a certain task in a separate NOTIS-DS folder. If you transferred this folder the last time, EasyLink will remember and will list the same files again. To transfer these files, simply click on **Transfer**.

When you have finished working with the files (or want a break), choose **Reverse Direction** from the **Session** menu and transfer the updated files back to the host computer.

To remove the file listing, choose **Reset** from the **Session** menu. To regain the file listing, choose **Prior Session**.

To switch off the "remember" function altogether, choose **Automatic Setting OFF** from the **Options** menu. To switch on again, choose **Automatic Setting ON**.

*Transferring or deleting large groups of files*

You can choose to transfer large groups of files that start with certain letters or have a certain file-type extension.

Perhaps you do your accounting in Excel and use linked spreadsheets. You can then let all your spreadsheets start with the year and end with :XLS (e.g. 89TOTAL:XLS). If you want to start an update for all spreadsheets from 1989, simply type 89:XLS in the **File:** edit box.

This technique can also be used to delete large groups of files.

---

## Page 17

# Placing a File-Transfer Operation in the WinSMX Menu

If you have WinSMX installed on your PC, you can set up a specific file-transfer operation as an item in the SMX menu. For example, if you often copy files from NOTIS-DS to MS-DOS, you can place this action in the menu. This is how (see the WinSMX User Guide for more information):

1. In the **SMX** menu, click on **Change System Menu**.
2. Under **Menu Item Name**, type a suitable name, for example **Transfer DS to DOS**.
3. Under **Applications in**, double-click on **ND-OWS** and **ND-UTIL**, then click on **EASYLINK.EXE**.
4. Click on the **Options>>** button.
5. Under **Startup Parameters**, type **NOTIS-DS MS-DOS**.
6. Click on the **Save** button.

The next time you want to transfer files from NOTIS-DS to MS-DOS, you can simply choose **Transfer DS to DOS** from the SMX menu.

---

## Page 18

# Get the correct file-type extension!

On your PC, a file-type extension has up to three characters (for example .TXT or .SMB). Extensions that have more than three characters will be abbreviated when transferred from the host.

Text files are always abbreviated correctly (:TEXT automatically becomes .TXT). For other file-type extensions, the abbreviation will not necessarily be correct.

You can, however, insert instructions to ensure correct abbreviations in the WIN.INI file, provided you know how to edit this file.

1. Open the WIN.INI file and look under the heading [ND EASYLINK]. You see the entries

   ```
   TEXT=TXT
   TXT=TEXT
   ```

2. Type in the abbreviation instructions you want, using the same syntax as for TEXT files, for example:

   ```
   SYMB=SMB
   SMB=SYMB
   ```

> **Note**
> 
> The SINTRAN and NOTIS-DS file-type extensions must have four characters. The PC file-type extensions cannot have more than three characters.

---

## Page 19

# Useful Information for Advanced Users

- EasyLink automatically uses the same MS-DOS work directory as the program it is started from. If you have SMX, you can define another work directory for startup.

- If you wish, you can choose to list only those files that start with certain letters or have certain file-type extensions. Just specify the letters and/or file-type extension you want in the File: or Document: edit box.

- Note that some letter/extension combinations may not work with SINTRAN and NOTIS-DS. Try removing some characters from the specifications.

- You can transfer files to or from a different SINTRAN user by specifying the user name in the File: edit box.
  Example: If you type (NEW-USER)d in the box, EasyLink will list all files that start with the letter “d” on that user.

- You can transfer files to or from a remote SINTRAN system. Use the same syntax as for transferring files within SINTRAN. The files cannot be listed in the dialog box.

- When a file is transferred, the destination list in EasyLink is not updated. This is done to save time.

- EasyLink always transfers from left to right. To transfer from the current right-hand side of the dialog box, you have to choose Reverse Direction from the Session menu.

- If you double-click on a file in the list box on the left side, EasyLink will suggest a name on the other side.

- If the PC is rebooted, EasyLink will assume that you are logged off.

---

## Page 20

# Select which host computer to connect to

If your PC is connected to an OpenLAN network (Ethernet), you can choose which host computer you want to connect to (if there is more than one host on the network, and if you are registered as a user on the host computer).

1. In **Program Manager**, under **ND Applications**, double-click on the icon for **Select Host**.

2. You see a dialog box:

```
  ___________________________________________
 |                                           |
 |  ND Select Host                           |
 |  _______________________________________  |
 | | Host computer name:                   | |
 | |_______________________________________| |
 | | BRUNGE                                | |
 | | DONG                                  | |
 | | HOPS-OLEFIX                           | |
 | | HOPS-SPLENDID                         | |
 | | HITS-NENIS                            | |
 | | HITS-MENIX                            | |
 | | NISSE                                 | |
 | | STYX                                  | |
 | | SUPER                                 | |
 | |                                       | |
 | |                                       | |
 | |                                       | |
 | |_______________________________________| |
 |  Select host and start NOTIS terminal:   |
 |  _____________________________________   |
 | | Link                                  | |
 | |_______________________________________| |
 |  Select host:                            |
 |  _____________________________________   |
 | | Select                               | |
 | |_______________________________________| |
 |  No change:                              |
 |  _____________________________________   |
 | | Cancel                               | |
 | |_______________________________________| |
 |                                           |
 | Copyright © 1988/89 Norsk Data a.s        |
 |___________________________________________|

```

3. Double-click on the computer you want to connect to, and **NOTIS Terminal** will start. If you do not want to start NOTIS Terminal, click once on the host you want to select, and then click on the **Select** button. Next time you start NOTIS Terminal, you will be connected to the host that you selected.

## Note

If you are running NOTIS Terminal (even as an icon) when you select a new host, you should go to NOTIS Terminal, log out and then log in again.

## Note

If you connect to a new host, you will be disconnected from your current host.

---

## Page 21

# Set screen colours and fonts

You can specify several settings for the terminal window.

1. In **Program Manager**, under **ND Applications**, double-click on the icon for **Settings**.
2. You will see this dialog box:

```
 ______________________________________________
| ND Notis Terminal Settings                   |
|----------------------------------------------|
| Sample screen      |  Font:                  |
|----------------------------------------------|
| Normal Text        |  [EMUL-16X32-FONT v]    |
| Inverse Text       |  [EMUL-8X12-FONT   ]    |  
| Dimmed Text        |  [EMUL-8X10-FONT   ]    |
| Inverse-Dimmed Text|  [EMUL-8X8-FONT    ]    |
|                    |  [EMUL-8X19-FONT   ]    |
|----------------------------------------------|
| Text               | [Save] [Default]        |
| Background         | [Reset] [Cancel]        |
| Dimmed Text        |                         |
|----------------------------------------------|
| Lines to scroll:   | [ 3 ▲]                  |
|----------------------------------------------|
| Cursor type        |                         |
| O Block            |                         |
| O Line             |                         |
|                    |                         |
| Copyright © 1986/90 Norsk Data a.s           |
|______________________________________________|
```

You can set the colour of ordinary text, background colour, and inverted text. You can also choose which font you want on the screen, what cursor type, and how many lines you want the screen to scroll in each step. An example window shows how it will look.

---

## Page 22

# About screen fonts

You should choose a font which is suitable for the type of screen you have. If you have chosen an unsuitable font, you will not be able to see all of the contents of the terminal window. You will then get scroll bars on the side of the terminal window.

The recommended fonts are:

| Screen                        | Font            |
|-------------------------------|-----------------|
| EGA screen                    | EMUL-8X12 FONT  |
| VGA screen                    | EMUL-8X14 FONT  |
| Cornerstone 19" Publisher screen | EMUL-16X32 FONT |
| Wyse 700 screen               | EMUL-8X19 FONT  |

---

## Page 23

# Defining Communication Settings

If your PC is connected to the host via a serial cable, you can define several settings for communication between the PC and the host.

1. In Program Manager, under ND Applications, double-click on the icon for Serial.
2. You see a dialog box similar to the one below:

```
+-------------------------------------+
|  ND Serial Communication            |
|  Communication Settings             |
|  PORT     @COM1  OCOM2              |
|  Speed    2400 [ ]                  |
|           4800                       |
|           9600                       |
|  Data length   O7  @8               |
|  Parity       @Even  ONone          |
|  Stop bits     O1  O2               |
|  Handshake   @XON/XOFF  OHardware   |
|                                     |
|  Port Selection                     |
|  Host Link  @COM1  OCOM2             |
|  Document Transfer                  |
|  Delay       OOn  @Off              |
+-------------------------------------+
|  Copyright © 1986/90 Norsk Data a.s |
+-------------------------------------+
| [Save]   [Cancel]  [Default]       |
+-------------------------------------+
```

See the Microsoft Windows User's Guide for an explanation of the various fields.

---

### Note

If you have serial communication and have a 286- or 386SX-computer, you should not use a higher transfer speed than 4800 baud. At higher speed you might lose both characters in NOTIS Terminal (the terminal emulator) and during file transfer.

---

## Page 24

# Errors that may occur

## Your PC slows down

This may happen if you run WinLink and several other programs at the same time. In this case, close one or more of the other programs.

## The cursor disappears

When the PC is used as a terminal against a host, the cursor will disappear while the screen is being updated. If the screen contains a lot of graphics, the update may take some time. If, in the mean time, you go to another program, the cursor will not reappear until the update of the first screen is finished.

## Graphics are the wrong size

When the PC is used as a terminal against a host, and you use graphic programs (for instance Nortext with graphics option), the graphic pictures will be displayed in the wrong format on the screen. Correct this in the following way:

1. Give the file NDCONFIG write access, by typing the MS-DOS command `C:\>ATTRIB -R NDCONFIG`.

2. Start NOTEPAD and open NDCONFIG.

3. Find the section `SYSTEM-NAME : OWS.` and the line `OWS-SIZE-MONITOR= .`

4. Write down what is written on the line, so that you can retype it later if necessary (or you may make a copy of the line and precede it by an asterisk (*) to make it a comment).

---

## Page 25

### Instructions

5. Enter the following on the line:

   ```
   OWS-SIZE-MONITOR=NON-STANDARD;273;163
   ```

   where 273 specifies the width and 163 the height.

6. Save the file, and restart the PC.

7. If the graphic pictures still don’t have the correct width and height, repeat the procedure with other values for width and height (the values are normally in the range between 270 - 280 and 160 - 170).

> **Note**  
> We recommend that you do this only for Cornerstone 19" Publisher screens. For EGA and VGA screens, the values for width and height are normally between 250-260 and 140-150.

### Error Messages

**"There is not enough memory for communication"**

This may happen if you run WinLink and several other programs at the same time. In this case, close one or more of the other programs.

**"This program has not been installed correctly"**

You must use the original diskette when you install the program.

**"Cannot find the font file: Path/filename"**

The reason may be that the nationality setting in CONFIG.SYS no longer corresponds to the nationality of the font files. You can:

a. Change the nationality in CONFIG.SYS back to its original value.

b. Install WinLink again in a language version that corresponds to the nationality setting in CONFIG.SYS.

---

## Page 26

# Error Messages and Solutions

## "The connection has been terminated"

Log in again. If this does not work, reboot the PC (by pressing `CTRL+ALT+DEL`), and log in again.

## "No access to host computer"

If you get this error message:

- Check that you are logged in on the host (see page 4).
- Check that you are not in a program on the host. This only applies if you are connected via a serial line. If you are working in another program on the host, exit this, and go back to the main menu in User Environment or to the SINTRAN prompt.
- Ask your system supervisor to check that there are no problems with the communication or communication servers.

## "Not enough memory to run the program"

This may happen if you run WinLink and several other programs at the same time. In this case, close one or more of the other programs.

## "No more ports available"

- Wait a few seconds and try again.
- Reboot the PC (by pressing `CTRL+ALT+DEL`) and log in again.

## "No connection to the host"

Contact your system supervisor.

## "No communication with the DOS VKM library"

Install PC Starter Kit again.

---

## Page 27

# Troubleshooting

## "No contact with communication medium"

Contact your system supervisor. CONNECT may not be loaded in AUTOEXEC.BAT. It could also be a hardware error.

## "No contact with host computer"

Contact your system supervisor.

## "No font entry in NDCONFIG for this screen resolution"

Open the dialog box **Settings** (see page 20), and check the list box **Font**: Normally, there should be five different fonts listed here.

*If there are not five fonts in the list box*, WinLink is not correctly installed. Install again.

*If there are five fonts in the list box*, WinLink is correctly installed. The problem is that you do not have a screen that corresponds to the available font types (for instance, a very small screen).

## "Configuration file missing"

PC Starter Kit is not installed correctly. Install again.

## "Low on memory"

This may happen if you run WinLink and several other programs at the same time. In this case, close one or more of the other programs.

## "Invalid font entry in NDCONFIG for this screen resolution."

Error in NDCONFIG. Install WinLink again.

## "Unknown host"

Choose another host computer (see page 19).

---

## Page 28

# Other programs from Norsk Data that make your work in Windows™ go faster and easier

## WinSMX

WinSMX is a pop-up menu for starting any Windows™ or MS-DOS® application. It is always there when you need it. From any Windows program you can either click on the WinSMX icon, or press *Alt+Space* and your WinSMX menu appears. Add any application to the menu quickly and easily. WinSMX is a powerful tool for both novices and professionals.

## WinPrint

WinPrint is a program that lets you print directly from Windows™ to a network printer on a SINTRAN machine. With WinPrint you work just like you would if you had your very own private printer. Whether you are using such popular programs as Excel®, AMi®, or PageMaker®, you do not have to exit the program to print. Just use the normal print commands in these programs and your print-job is automatically sent directly to the network printer. You can continue to work even while the file is being sent.

---

**These programs can be ordered from:** Your local ND representative or Norsk Data a.s, P.O Box 25 Bogerud, N-0621 Oslo 6. Telephone: (02) 62 68 70. Telefax: (02) 62 68 71

---

## Page 29

I'm unable to transcribe or recreate text from images containing cursive, elaborate, or low-contrast text. However, if you have any other pages or need assistance with something else, feel free to let me know!

---

