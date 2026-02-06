---

# APPENDIX 2: OCTOBUS PROTOCOL VERSION 5

## 2.1 OCTOBUS OVERVIEW

The octobus is a fast serial bus optimized for handling short messages. A maximum of 64 stations (processors) can be connected to one bus. The octobus is used in the low-level operating system to provide interprocess synchronization and exchange of configuration parameters during initialization. The octobus is also used as the communication medium between system components for debugging and maintenance.

The octobus is not visible above the low-level operating system. Communication between processes and synchronization within the operating system, as well as at the application level, is provided by NUCLEUS.

## 2.2 INTRODUCTION TO OCTOBUS HARDWARE

The octobus can be divided into a global and a local octobus. Only a device connected to the global octobus can be MASTER of the octobus chain. All devices connected to the octobus chain are given a unique station number.

### Definitions of octobus station numbers:

| Station No | Octobus Device                          |
|------------|-----------------------------------------|
|     1      | ND-120 CPU                              |
|   2 - 7    | MFbus controllers                       |
|  10 - 13   | SCSI controllers (disk)                 |
|  14 - 15   | Matra VME                               |
|  16 - 17   | Multifunction communication             |
|    20      | Hyperchannel                            |
|  21 - 23   | FDDI (Fibernet)                         |
|  24 - 27   | FPS-5000                                |
|  30 - 33   | Graphic controller                      |
|  34 - 67   | Free for expansion                      |
|  70 - 76   | ND-5000 CPU                             |

## 2.3 OCTOBUS HARDWARE SIGNALS

The global octobus consists of four differential signals, which are converted to TTL signals on the local octobus. The local octobus carries the following signals:

• **XREQ** – request  
• **XCLK** – clock  
• **XDAT** – transmit  
• **XRF0** – refresh signal

With the bus in a quiescent state, the three first lines are off, while, if MASTER is selected, the XRF0 line is pulsing with a 15µs period. If XRF0 is not pulsing, indicating that no MASTER is selected, the stations connected to the octobus automatically start to assign a MASTER. The one with the lowest station number ends up as the MASTER and starts transmitting the refresh signal (XRF0).

When a MASTER is selected, the octobus is ready to transfer messages between any of the stations connected to the bus. A transfer is initiated by a station when it activates the XREQ line. When the MASTER receives this request, it automatically starts to transmit clock pulses (XCLK) with the frequency specified for the octobus (1 or 4 MHz).

[Documentation continues - requesting stations transmit in order...]

## IMPLEMENTATION NOTES FOR EMULATOR

Test 4 "Check Octobus configuration" sends query messages to discover stations on the bus. When sending to station X (e.g., station 10 = SCSI controller), the system expects:

1. **If station X exists**: Station X responds with "Identify yourself" response containing its station number. The response source station should match the queried destination.

2. **If station X does not exist**: No response is received (timeout).

In the current emulator:
- Loopback mode echoes our own transmissions back (for basic interface testing)
- For proper station discovery, we need to either:
  a) Attach actual device emulators at specific station numbers
  b) Simulate station responses based on configured station map

The test failure "Station number - response message: 11, receive data register: 10" indicates that when querying station 10, we're not properly simulating station 10's response. The loopback is echoing data but not formatting it as a proper station 10 response.

---

# Octobus Protocol

## 2.3 Octobus Frame Format

The signals transmitted on the **octobus** during one frame consist of a **start bit**, a **stop bit**, and **30 data bits**.

```
|30 ...... 27| 26 ...... 21  |20 | 19| 18 .. 13  | 12 ...... 5  | 4 3  | 2 1 |
+------------+---------------+---+---+-----------+--------------+------+-----+
|  Priority  | Destination   | C | B |   Source  | Information  |Parity| Ack |
+------------+---------------+---+---+-----------+--------------+------+-----+

<----------------------- Direction of transmission

```

### Field Definitions

**Priority**  
Content of the *Lost Access Counter*.

**Destination**  
- When **B = 0** (normal transmission), this field contains one of **62 station numbers (1–76B)**.  
- When **B = 1** (broadcast), this field contains one of **six station types**.

**C (Control bit)**  
- **C = 1**: The attached information is a **control byte**.  
- **C = 0**: The information field contains **pure data**.

**B (Broadcast bit)**  
- **B = 1**: All stations of the specified type accept this message (broadcast).  
- **B = 0**: Only the station matching the destination number accepts this message.

**Source**  
Station number of the transmitting device.

**Information**  
One byte of data.

**Parity**  
The number of logical ‘1’ bits in the frame is counted, and the **two least significant bits** of this count are appended to the end.

**Ack (Acknowledge)**  
Acknowledgement of the frame returned from the destination device.

Ack codes:

| Ack | Meaning |
|-----|---------|
| 00  | Timeout – 15 retries |
| 01  | Successfully received |
| 10  | Destination busy – 255 retries |
| 11  | If **B = 0**: Parity error – 15 retries  
|     | If **B = 1**: Ambiguous response |

---

## 2.4 Introduction to the Protocol

There are **four separate message streams** on the octobus:

### 1. IDENT Messages
Routed to **IDENT ENTRIES**. These messages immediately activate a process in the destination station with the correct working set.

### 2. KICK Messages
Routed to **HANDLER ENTRIES**. These messages immediately activate a process in the destination station. The destination process receives KICK messages from all stations and maintains its own data structure to determine the reason for activation.

### 3. MULTIBYTE Messages
Routed to **Octobus Message Devices (OMD)**. These messages immediately activate a process in the destination station. The destination process receives multibyte messages from all stations. Primarily used for **initialization, debugging, and maintenance**.

### 4. EMERGENCY Messages
Decoded directly by **hardware** or by the **octobus driver**.

---

The four message streams are kept **completely separate**, as illustrated in Figure 63.


---

## 2.5 Message Format

The basic frame sent and received on the octobus is:

```
15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
+--+--+--------------------+------------------------+
| C| B| DEST(TYPE)/SOURCE  | Frame type dependent   |
+--+--+--------------------+------------------------+
```

### Common Bit Usage (Bits 8–14)

Bits **8–14** are used in the same way for all frames on the octobus:

**B = 1**  
Broadcast octobus frame to all stations within this ring with the specified **TYPE**.

**Source/Destination field**  
- When **sending** a frame to octobus, this field contains the **destination station number** (or **TYPE** if `B = 1`).  
- When **receiving** a frame from octobus, this field contains the **source station number**.

The remainder of the frame is decoded as shown in Figures 64 and 65.

### Figure 64: Octobus Frame Decoding - 1

#### Frame Format
```
|15|14| 13 ..... 08 |07 |06 |05 |04 | 03  02  01  00|
+--+--+-------------+---+---+---+---+---+---+---+---+
| C| B| SOU/DEST    | E | K | M | S |               |
+--+--+-------------+---+---+---+---+---+---+---+---+

```

#### Message Type Decoding

| Type | C | B | E | K | M | S | Data Field | Description |
|------|---|---|---|---|---|---|------------|-------------|
| 1    | X |   | X |   |   |   | Emergency code | Emergency msg. (EMESS) |
| 2    | X |   |   | X |   |   | Kick number | Kick msg. (KICK) |
| 3    | X |   |   |   |   |   | Ident number | Ident msg. (IDF) |
| 4    | X |   |   |   | X | X | OMD number | Start of multibyte msg. (SOMB) |
| 5    | X |   |   |   | X |   | OMD number | End of multibyte msg. (EOMB) |
| 6    |   |   |   |   |   |   | Data byte | Part of multibyte msg. (DATA) |

*Note: X indicates the bit is set (1), blank indicates the bit is clear (0)*

---

### Frame Decoding Overview (Figure 64)

```
15 14 13 ..... 08  07 06 05 04 03 02 01 00
+--+--+-------------+--+--+--+--+--+--+--+
| C| B| SOU/DEST    | E | K | M | S |    |
+--+--+-------------+--+--+--+--+--+--+--+
```

#### Message Types

| Type | Description |
|------|-------------|
| 1 | Emergency message (**EMESS**) – Emergency code |
| 2 | Kick message (**KICK**) – Kick number |
| 3 | Ident message (**IDF**) – Ident number |
| 4 | Start of multibyte message (**SOMB**) – OMD number |
| 5 | End of multibyte message (**EOMB**) – OMD number |
| 6 | Data byte – Part of multibyte message (**DATA**) |

---

### Control Bits

**E – Emergency**  
Indicates that the message is decoded by hardware or by the octobus driver.

**K – Kick**  
Kick to a handler.

**M – Multibyte message**  
Indicates start or stop of a multibyte message.

**S – Start/Stop indicator**  
- **S = 1**: Start of message  
- **S = 0**: End of message

---

### Detailed Frame Decoding (Figure 65)

The decoding logic is hierarchical and driven by the control bits `C`, `E`, `K`, `M`, and `S`:

- **Emergency message (1)**  
  `C=1, E=1` → Emergency code

- **Kick message (2)**  
  `C=1, K=1` → Kick number

- **Ident message (3)**  
  `C=1, S=1` → Source + Ident number

- **Start of multibyte message (4)**  
  `C=1, M=1, S=1` → OMD number

- **End of multibyte message (5)**  
  `C=1, M=1, S=0` → OMD number

- **Data message (6)**  
  `C=0` → Data byte

---

*Figures 64 and 65 illustrate the exact bit-level decoding paths used by octobus hardware and drivers.*

---

## 2.6 Emergency Message

An octobus message with the **C** and **E** bits set to `1` is defined as an **emergency message**. Emergency messages are always sent with the **highest possible priority**.

### Emergency Message Frame Format

```
15 14 13 ..... 08  07 06 05 ..... 00
+--+--+-----------+--+--+--------------+
| C| B| DEST/TYPE | E | R | H | OP. CODE |
+--+--+-----------+--+--+--------------+

C = 1
E = 1
```

**B – Broadcast**  
Set to `1` for **broadcast emergency messages**.

---

### Emergency Message Interpretation

The interpretation of an emergency message depends on the **R** and **H** bits:

| R-bit | H-bit | Interpretation |
|-------|-------|----------------|
| 0 | 0 | Emergency message handled in **software** by the specified destination/type |
| 0 | 1 | Emergency message decoded in **hardware** by the specified destination/type |
| 1 | 0 | Not used |
| 1 | 1 | Not used |

*Figure 66. Emergency Message Interpretation*

---

### Example Uses

Typical uses of emergency messages include:

1. **Reset (master clear)** the ACCP
2. **Continue, stop, or terminate** the ACCP

---

---

## 2.7 Kick Message

The octobus driver uses the **KICK NO** field to obtain the corresponding **HANDLER entry**.

### Kick Message Frame Format

```
15 14 13 ..... 08  07 06 05 ..... 00
+--+--+-----------+--+--+--------------+
| C| B| DEST/TYPE | E | K |  KICK NO    |
+--+--+-----------+--+--+--------------+

C = 1
K = 1
```

**Broadcast is not allowed** for KICK messages. The **B bit must be 0**.

### Example Uses

| Kick No | Meaning |
|---------|---------|
| 1–2 | Activate ND‑5000 process |
| 3 | Clear flag – continue process |
| 4 | Update internal clock |
| 5 | NUCLEUS kick |
| 6 | Save context – go IDLE |

---

## 2.8 Ident Message

The octobus driver uses both the **IDENT** and **SOURCE** fields to obtain the corresponding **IDENT entry**.

### Ident Message Frame Format

```
15 14 13 ..... 08  07 06 05 04 ..... 00
+--+--+--------------+--+--+--+-----------+
| C| B| DEST/SOURCE  | E | K | M | IDENT NO |
+--+--+--------------+--+--+--+-----------+

C = 1
```

**Broadcast is not allowed** for IDENT messages. The **B bit must be 0**.

### Example Uses

1. ND‑5000 / ND‑110 communication
2. Activation of drivers in the ND‑110

---

---

## 2.9 Multibyte Message

A multibyte message consists of **4 + x frames** on the octobus, where **x** is the number of bytes in the message body.

### Multibyte Message Frame Sequence

#### 1) Start of Message (SOMB)

```
15 14 13 ..... 08  07 06 05 04 ..... 00
+--+--+--------------+--+--+--+-----------+
| C| B| DEST/SOURCE  | E | K | M | OMD NO   |
+--+--+--------------+--+--+--+-----------+

C = 1
M = 1
S = 1
```

This control frame starts the multibyte message and specifies which **Octobus Message Device (OMD)** in the receiving station is to be activated.

---

#### 2) Source OMD Frame

```
15 14 13 ..... 08  07 ..... 00
+--+--+--------------+-----------+
| C| B| DEST/SOURCE  | OMD NO    |
+--+--+--------------+-----------+

C = 0
```

This frame contains the **source OMD number**.

---

#### 3) Length Frame

```
15 14 13 ..... 08  07 ..... 00
+--+--+--------------+-----------+
| C| B| DEST/SOURCE  | NO. BYTES |
+--+--+--------------+-----------+

C = 0
```

This frame specifies the **number of bytes** in the message body.

---

#### 4) Message Body Frames (DATA)

```
15 14 13 ..... 08  07 ..... 00
+--+--+--------------+-----------+
| C| B| DEST/SOURCE  | MSG PART |
+--+--+--------------+-----------+

C = 0
```

Each frame carries one byte of the message body. There are **x** such frames.

---

#### 5) End of Message (EOMB)

```
15 14 13 ..... 08  07 06 05 04 ..... 00
+--+--+--------------+--+--+--+-----------+
| C| B| DEST/SOURCE  | E | K | M | OMD NO   |
+--+--+--------------+--+--+--+-----------+

C = 1
M = 1
S = 0
```

This control frame terminates the multibyte message and **repeats the OMD number** to be activated.

---

### Operational Notes

- A multibyte message is always started with a **SOMB** control frame.
- The second frame identifies the **source OMD**.
- The third frame gives the **length** of the message body in bytes.
- The message body follows as a sequence of DATA frames.
- The message is terminated by an **EOMB** control frame.

---

### Example Uses

1. ND-110 / ACCP communication during startup
2. Communication with the **OPCOM** module in **DOMINO** during startup and maintenance

---
