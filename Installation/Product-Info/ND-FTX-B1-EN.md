## Page 1

# FAULT TOLERANT SYSTEM (FTX)
## UPGRADING PACKAGE

[Photo: Computer System]

```
          _____ _____ 
         |_____|_____| 
   _____|_____|     |  
  |_____|_____|     |  
```

```
    ND
Norsk Data
```

---

## Page 2

# Fault Tolerant System (FTX) Upgrading Package

The ND numbers for each package are:

- **ND 3201/3202 FTX package for Compact systems**
- **ND 3701/3702 FTX package for ND-110/CX systems**
- **ND 5021/5022 FTX package for ND-500 systems**

## Introduction

ND's Fault Tolerant Systems, FTX, integrated into the ND-SAFE concept, offer high system availability by providing resilience against both hardware and software failures. By duplicating the critical components of the system and fully utilizing the stand-by components, ND’s implementation of the fault tolerant systems offers high system availability without degrading the overall system performance.

## Features

- Based on existing hardware and software
- Resilience can be added as and where needed
- Both software and hardware fault tolerance
- Very little impact on applications
- Automatic reconfiguration of hardware and software occurs after a failure (not yet available)
- Online maintenance and upgrading of the system

## Product Description

ND’s implementation of fault tolerant systems is based on the concept of “cooperating minicomputers.” ND’s FTX Upgrading Package is a software and hardware package designed to transform existing ND-500/CX or ND-100/CX systems to include hardware and software fault tolerance. At least two ND-100/CX or ND-500/CX systems are involved in the configuration.

The machines are “loosely coupled,” have their own private memory, and run different copies of the operating system. All data can be stored simultaneously on mirrored (parallel) disks. Peripherals and communication lines can be switched from the failing machine to the active one. The work load can be distributed between the two machines. The database can run on one machine, for example, and the application on the other.

An FTX software package is installed in each system. It consists of a Disk Mirroring Module and a Central Error Logging Module. The mirroring module secures all data stored on the mass storage devices by ensuring that all data modifications be written to at least two different disks. If one disk fails to operate, the data can be accessed from the mirrored unit. This is transparent to all applications and to most of the operating system.

The error logging module collects all errors reported from system modules and user applications, and saves these on a set of log files. These log files may be analyzed for periodical or transient errors that may otherwise be difficult to trace.

---

## Page 3

# FAULT TOLERANT SYSTEM (FTX)  
## UPGRADING PACKAGE

### System Configuration

In the FTX system, all critical components are duplicated. In case one component fails, the other component takes over. A standard FTX system consists of two CPUs which are interconnected via a communication line, and two or more mirrored disks which can be switched from one system to another. The terminals and the I/O lines can be connected through a switching device.

### System Description

Each FTX Upgrading Package is comprised of:

- 2 Disk controllers
- 2 Megalink communication interfaces
- ND-Cosmos basic system
- Disk mirroring and error logging module
- Model I: One Filestore system including the disk switch
- Model II: Two Filestore systems including the disk switch

The ND numbers for each package are:

- ND 3201/3202 FTX package for Compact systems
- ND 3701/3702 FTX package for ND-110/CX systems
- ND 5021/5022 FTX package for ND-500 systems

### REQUIREMENTS

#### Hardware

- Two coordinated systems: Any combination of the ND-500/CX, the ND-110/CX and the ND-110 Compact models with SMD disk drives can be used.
- One communication line: This means one HDLC or Megalink per system.
- Disks and/or magtape: At least two disks (mirrored disks) are required to store critical data. At least one system disk per machine is required, and magtapes can be used as backup media. The magtapes/disks are delivered in a FILESTORE cabinet.
- Two Dual-Port Disk Switches
- Software-controlled I/O line switch (optional)
- Power distribution unit (optional, but strongly recommended)

#### Software

Standard system software (operating system, database management, programming languages, screen-handling utilities and communication procedures), plus a fault tolerant system software package for each module. The FTX software package includes ND 210855, Disk Mirroring and Error Logging modules.

### OPTIONS

#### Software-Controlled I/O Line Switch

This switch is delivered in one or two system cabinets and permits switching of several I/O lines. The I/O lines can be of the following types:

- Current-loop, to connect terminals. A maximum of 128 current-loop lines can be accommodated.
- RS-232, to connect terminals, printers, plotters, personal computers, etc. The maximum number of lines supported is 64 per cabinet.
- Parallel, to connect high-speed printers. A maximum of 32 lines per cabinet can be supported.
- HDLC, for HDLC or Megalink communication lines. Up to 32 lines per cabinet are supported.

The DMA lines are not supported with this switch. Switching of the I/O devices can be either manual or automatic.

#### Power Distribution System

To improve the quality of the voltage and reduce noise levels, ND offers three types of power distribution systems. The systems provide automatic reconnection after a power disruption. A special isolation transformer combined with a metal oxide varistor is used, reducing even extreme overvoltage to non-dangerous levels.

### DOCUMENTATION

FTX Operator Manual .................................ND-30.051 EN

[Scanned by Jonny Oddene for Sintran Data © 2021]

---

