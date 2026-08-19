## Page 1

# NorskData

---

## How to install

WinLink

---

ND-891092EN1

---

## Page 2

# Read this first!

To install WinLink (ND 230210) you need:

- An IBM-compatible PC running Microsoft Windows 3.
- PC Starter Kit, ND 230123 (version A04 or newer).
- For Ethernet connection: ND Connect Module, ND 230125.
- SINTRAN software required for PC Starter Kit and ND Connect Module:

  **Ethernet:**  
  SINTRAN 100: OWS Access Server, ND 211297  
  SINTRAN 500/5000: OWS Access Server, ND 211325  

  **Serial:**  
  OWS Terminal Server, ND 211295  

OWS Terminal Server is a part of PC Starter Kit.

---

# Note!

If you get the error message "Not enough disk space to install product", the installation will be interrupted. You have to delete files to make room before you try again.

---

## Page 3

# Installing WinLink

**Important!**

The programs must be installed in this order:

1. Windows 3
2. PC Starter Kit
3. ND Connect Module, if Ethernet communication
4. WinLink

## From Windows:

1. Put the diskette into the diskette drive. If you have more than one drive, use drive A.

2. Get **Program Manager** on the screen.

3. From the **File** menu, choose **Run**.

4. Type `A:\INSTALL` and press ↵.

5. The installation is automatic.

If you select a font for the terminal emulator which is not on diskette no. 1, the installation program will tell you to change the diskette during the installation.

The file NDCONFIG is updated. A copy of the old file will be renamed to NDCONFIG.WI2. It will be in the same directory as NDCONFIG.

---

## Page 4

# Multi-user installation

## From Windows:

When installing a multi-user version (on a network), WinLink must first be installed on the server. The installation procedure is the same as under **Installing WinLink**.

The last message from the installation program gives you the path for the program SETUP.EXE. You will use this path later, so write it down.

When the installation on the server is finished, each client that is going to use WinLink will have to run the program SETUP.EXE. You got the path to SETUP.EXE on the server from the installation program at the end of the server-installation.

---

