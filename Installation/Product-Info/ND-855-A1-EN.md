## Page 1

# ND Computer Systems

```mermaid
flowchart LR
    A[ND-100] --> B[ND-100 Bus]
    B --> C[GPIB CARD]
    C --> |REMOTE| D[GPIB REMOTE CARD]
    C --> E[IEC-625]
    C --> F[IEEE-488]
    E --> G[DEVICE]
    E --> H[DEVICE]
    F --> I[DEVICE]
    F --> J[DEVICE]
    D --> |<100 meters 20 twisted pairs| K[IEC-625]
    D --> L[IEEE-488]
    K --> M[DEVICE]
    K --> N[DEVICE]
    L --> O[DEVICE]
    L --> P[DEVICE]
    M -.-> "Maximum no. of devices: 13." 
    M -.-> "Maximum cable length: 20 m or no. of devices x 2 meters."
    N -.-> "Maximum no. of devices: 14."
    N -.-> "Maximum cable length: 20 m or no. of devices x 2 meters."
```

ND-100 with GPIB-system.

## ND 855 General Purpose Interface Bus Controller (GPIB)

## ND 856 GPIB Remote Option

### Introduction

The GPIB-system is an IEEE -488 and IEC -625 standard multi-user interface system for programmable instrumentation. It is intended for applications like laboratory instrumentation in hospitals and research institutions, and for automatic test systems. However, it can also be used to interface peripherals like plotters or printers, if these are IEEE- or IEC-compatible.

The interface system can be used for:
- Measurements requiring high reproducibility and accuracy
- Measurements requiring simultaneous testing of input and output characteristics
- Measurements requiring immediate further processing of the measured data to allow decision-making
- Measurement procedures which are extremely diversified or constantly recurring
- Procedures measuring many parameters
- Measurements with only a few interesting results

855/856-A1-6000-0681

---

## Page 2

# Product Description

The GPIB system on ND-100 is a multi-user system with a maximum of 16 users. The microprocessor on the GPIB-module takes care of creating new users, deleting old users, reserving devices for a user, enabling for a new device that has been connected to the bus, deleting old devices, etc.

The standard version (ND 855) allows for:

- Maximum 15 devices on one bus
- Maximum 20 meters total cable length
- Maximum 500 000 bytes/s data rate

The remote option (ND 856) avoids these limitations in the IEEE-488 or IEC-625 standards. On the ND-100 GPIB-card, the 16 signals in the GPIB are translated into 20 pairs of signals in a differential bus. In a memory box containing one card, these 20 pairs of signals are translated back to the 16 GPIB signals.

This differential bus can be a maximum of 100 meters long. In this way, 13 devices may be connected to the ND-100 and another 14 devices connected to the remote box. This makes 28 devices connected to each other, using only one interface card.

## Software

The GPIB-module is an input/output unit with or without direct memory access (DMA).

The contact between the programmer and the GPIB-module connected to the devices are through:

- Sending device dependent data with or without DMA
- Receiving device dependent data with or without DMA
- Receiving interface status like parallel poll, serial poll and error status through the microprocessor
- Sending special messages to the module like word counter, memory address and control word
- Sending commands to the microprocessor on the module
- Receiving status from the module like interrupt status, state status, memory address

The GPIB-system is delivered with a GPIB-software packet based on FORTRAN callable routines for the different functions of the module.

There are routines for:

- Reading data from device
- Writing data to device(s)
- Transfer data between devices
- Triggering and clearing device(s)
- Setting device(s) in local mode
- Parallel configuration of device(s)
- Pass control to device
- Configuration of the parallel or serial poll status for GPIB-module
- Reserve device(s) for a user
- Release device(s) for a user
- Listing system device(s)
- Listing device(s) reserved for a user

```
        _______
       |       |
       | ND    |          ________
       |_______|        / ______  \
                      / /      \  \
                     / /        \  \
                    /_/__________\__\
```

```
[Photo: ND logos and contact info]
```

**NOTE:** NORSK DATA reserves the right to change specifications without given notice!

---

