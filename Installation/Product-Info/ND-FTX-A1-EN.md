## Page 1

# Fault Tolerant System (FTX)

[Photo: Image of a fault-tolerant system unit]

```
  ______________________   _______   _______
 |______________________| |       | |       |
 |______________________| |_______| |_______|
```

ND  
Norsk Data

---

## Page 2

# FAULT TOLERANT SYSTEM (FTX)

| System Number | Model                      |
|---------------|----------------------------|
| ND 3270       | ND-110 Compact/FTX         |
| ND 3750       | ND-110/FTX                 |
| ND 5151       | ND-510/FTX                 |
| ND 5351       | ND-530/FTX Model 11        |
| ND 5352       | ND-530/FTX Model 12        |
| ND 5551       | ND-550/FTX Model 11        |
| ND 5552       | ND-550/FTX Model 12        |
| ND 5651       | ND-560/FTX Model 11        |
| ND 5652       | ND-560/FTX Model 12        |
| ND 5751       | ND-570/FTX Model 11        |
| ND 5752       | ND-570/FTX Model 12        |

## INTRODUCTION

ND's Fault Tolerant System, FTX, integrated into the ND-SAFE concept, offers high system availability by providing resilience against both hardware and software failures. By duplicating the critical components of the system and fully utilizing the stand-by components, ND's implementation of the fault tolerant systems offers high system availability without degrading the overall system performance.

## FEATURES

- Based on existing hardware and software
- Resilience can be added as and when needed
- Existing systems can be upgraded
- Both software and hardware fault tolerance
- Very little impact on applications
- Automatic reconfiguration of hardware and software occurs after a failure (not yet available)
- Online maintenance and upgrading of the system

## PRODUCT DESCRIPTION

ND's implementation of fault tolerant systems is based on the concept of "cooperating minicomputers." At least two ND-100 or ND-500 systems are involved in the configuration.

The machines are "loosely coupled," have their own private memory, and run different copies of the operating system. All data can be stored simultaneously on mirrored (parallel) disks.

Peripherals and communication lines can be switched from the failing machine to the active one. The workload can be distributed between the two machines. The database can run on one machine, for example, and the application on the other.

An FTX software package is installed in each system. It consists of a Disk Mirroring Module and a Central Error Logging Module. The mirroring module secures all data stored on the mass storage devices by ensuring that all data modifications be written to at least two different disks. If one disk fails to operate, the data can be accessed from the mirrored unit. This is transparent to all applications and to most of the operating system (for example, the file system and swapper).

The error logging module collects all errors reported from the FTX modules and user applications, and saves these on a set of log files. These log files may be analyzed for periodical or transient errors that may otherwise be difficult to trace.

---

## Page 3

# FAULT TOLERANT SYSTEM (FTX)

## System Configuration

In the FTX system, all critical components are duplicated. In case one component fails, the other component takes over. A standard FTX system consists of two CPUs which are interconnected via a communication line, and two or more mirrored disks which can be switched from one system to another. The terminals and the I/O lines can be connected through a switching device.

## System Description

Each FTX system is comprised of two coordinated ND-500/CX models, the ND-110/CX or the ND-110/CX Compact models with SMD type disk drives. In addition, each FTX system includes:

- Two systems (ND-500, ND-110, or Compact) including:
  - 2 Disk controllers
  - 1 Megalink communication interface
  - ND-Cosmos basic system
  - Disk mirroring and error logging module
- A Filestore cabinet containing dual-port disk switch and the mirrored disks.

The ND numbers for all of the systems are:

| ND Number | System Model             |
|-----------|--------------------------|
| ND 3270   | ND-110 Compact/FTX       |
| ND 3750   | ND-110/FTX               |
| ND 5151   | ND-510/FTX               |
| ND 5351   | ND-530/FTX Model 11      |
| ND 5352   | ND-530/FTX Model 12      |
| ND 5551   | ND-550/FTX Model 11      |
| ND 5552   | ND-550/FTX Model 12      |
| ND 5651   | ND-560/FTX Model 11      |
| ND 5652   | ND-560/FTX Model 12      |
| ND 5751   | ND-570/FTX Model 11      |
| ND 5752   | ND-570/FTX Model 12      |

## REQUIREMENTS

### Hardware

- Two systems: All ND-500/CX models, the ND-110/CX and the ND-110 Compact models with SMD disk drives can be used.
- One communication line: This means one HDLC or Megalink per system.
- Disks and/or magtape: At least two disks (mirrored disks) are required to store critical data. At least one system disk per machine is required, and magtapes or disks can be used as backup media. The magtapes/disks are delivered in a FILESTORE cabinet.
- Dual-Port Disk Switches
- Software-controlled I/O line switch (optional)
- Power distribution unit

### Software

- Standard system software (operating system, database management, programming languages, screen handling utilities and communication procedures), plus a fault-tolerant system software package for each module. The FTX software package includes ND 210855, Disk Mirroring and Error Logging modules.

## OPTIONS

### Software Controlled I/O Line Switch

This switch is delivered in one or two system cabinets and permits switching of several I/O lines. The I/O lines can be of the following types:

- Current-loop, to connect terminals. A maximum of 128 current-loop lines can be accommodated.
- RS-233, to connect terminals, printers, plotters, personal computers, etc. The maximum number of lines supported is 64 per cabinet.
- Parallel, to connect high-speed printers. A maximum of 32 lines per cabinet can be supported.
- HDLC, for HDLC or Megalink communication lines. Up to 32 lines per cabinet are supported.

The DMA lines are not supported with this switch. The switching of the I/O devices can be either manual or automatic.

### Power Distribution System

To improve the quality of the voltage and reduce noise levels, ND offers three types of power distribution systems. The systems provide automatic reconnection after a power disruption. A special isolation transformer combined with a metal oxide varistor is used, reducing even extreme overvoltage to non-dangerous levels.

## DOCUMENTATION

FTX Operator Manual ........................... ND 30.051 EN

---

