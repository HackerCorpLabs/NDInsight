# XMSG-COMMAND Reference Guide

## Overview

**Program:** XMSG command program (210373M)
**Version:** 1988.04.12 (Release M)
**Purpose:** COSMOS/XMSG network management and debugging utility
**Access:** `@XMSG-COMMAND` or `X-C:`

---

## Prerequisites

### Starting XMSG Subsystem

Before using XMSG-COMMAND, ensure XMSG is started:

```
@SINTRAN-SERVICE-PROGRAM
*START-XMSG
*EXIT
```

**Error Message if Not Started:**
```
XMSG Kernel error: XMSG is either not generated, not loaded or not started
```

---

## Command Categories

### 1. Monitoring & Status Commands

#### Port & Task Management
```
List-Ports                    - Display all XMSG communication ports
List-Tasks                    - Show all active XMSG tasks
List-Messages                 - Display messages in queues
List-Names                    - Show named systems per port
List-Connections              - Show active network connections
List-Utilization              - Display XMSG resource usage statistics
```

#### Network Status
```
List-Links                    - Show network link status (up/down)
List-Frames                   - Display frame statistics (tx/rx/errors)
List-Systems                  - Show all connected systems
List-Friend-Systems           - Display trusted systems list
List-Servers                  - Show active servers
List-Network-Servers          - Display all network servers
```

#### Routing & Configuration
```
List-Routing-Info             - Display routing table entries
List-Generation-Variables     - Show XMSG configuration parameters
List-Network-Remote-Systems   - Display remote system configurations
List-Network-Remote-Endpoints - Show remote endpoint definitions
List-Network-Local-Endpoints  - Display local endpoint configurations
List-Version                  - Show XMSG version information
```

---

### 2. Debug & Diagnostics Commands

#### Enable Debug Mode
```
Debug-Mode-On                 - Enable XMSG debug logging/tracing
```

**Purpose:** Activates detailed logging of XMSG operations including:
- Message passing events
- Frame transmission/reception
- Port operations
- Routing decisions
- Error conditions

#### Dump & Error Analysis
```
Dump-XMSG                     - Dump current XMSG state to file
Define-Dump-Files             - Configure dump output file
Remove-Dump-Files             - Clear dump file configuration
List-Dump-Files               - Display current dump file settings
Get-Error-Message             - Retrieve detailed error information
```

**Dump File Usage:**
```
X-C: Define-Dump-Files XMSG-DEBUG:TXT
X-C: Dump-XMSG
X-C: List-Dump-Files
```

---

### 3. System Configuration Commands

#### Local System Setup
```
Define-Local-System           - Configure local system parameters
Define-Remote-Name            - Add remote system name mapping
Get-System-Name-or-Number     - Lookup system by name or number
Deabbreviate-System-Name      - Expand abbreviated system name
```

#### Routing Configuration
```
Define-System-Route           - Add routing table entry
Remove-System                 - Remove system from routing table
Enable-Route-Through          - Allow routing through this node
Disable-Route-Through         - Block routing through this node
```

#### Friend Systems (Trusted Nodes)
```
Define-Friend-System          - Add system to trusted list
Remove-Friend-System          - Remove system from trusted list
```

---

### 4. Link Management Commands

#### Link Control
```
Start-Link                    - Activate a network link
Stop-Link                     - Deactivate a network link
Define-Alternative-Link       - Configure backup link
Remove-Alternative-Link       - Remove backup link definition
```

#### Link Options
```
Enable-Checksum               - Enable frame checksum validation
Disable-Checksum              - Disable frame checksum validation
```

**Use Cases:**
- **Start-Link:** Bring up HDLC/X.25 connection
- **Stop-Link:** Gracefully shutdown link for maintenance
- **Alternative-Link:** Configure redundant paths for failover

---

### 5. Network Server Commands

#### Server Control
```
Start-Network-Server          - Start network service daemon
Stop-Network-Server           - Stop network service daemon
List-Network-Servers          - Display all network servers
```

#### Connection Management
```
Define-Network-Connection           - Add network connection definition
Define-Network-Direct-Connection    - Configure direct link connection
```

#### Endpoint Configuration
```
Define-Network-Local-Endpoint       - Add local endpoint
Remove-Network-Local-Endpoint       - Remove local endpoint
Define-Network-Remote-Endpoint      - Add remote endpoint
Remove-Network-Remote-Endpoint      - Remove remote endpoint
Remove-Network-Remote-System        - Remove entire remote system
```

#### Group Numbers
```
Define-Network-Remote-Groupnumber   - Assign group number to remote
Remove-Network-Remote-Groupnumber   - Remove group number assignment
```

---

### 6. Auto-Restart & Recovery

#### Restart Configuration
```
Auto-Restart-On               - Enable automatic restart on failure
Auto-Restart-Off              - Disable automatic restart
Define-Restart-Files          - Configure restart file list
Remove-Restart-Files          - Clear restart file configuration
List-Restart-Files            - Display restart file settings
```

**Purpose:** Configure automatic recovery after XMSG crashes or system restart.

---

### 7. Utility Commands

```
Set-Advanced-Mode             - Enable advanced command mode
Help                          - Display command list
Exit                          - Exit XMSG-COMMAND program
?                             - Show help (alias for Help)
```

---

## Debugging Workflow

### Step-by-Step: Troubleshooting XMSG Issues

#### 1. Initial Setup
```bash
@SINTRAN-SERVICE-PROGRAM
*START-XMSG
*EXIT

@XMSG-COMMAND
```

#### 2. Check System Status
```
X-C: List-Version
X-C: List-Generation-Variables
X-C: List-Ports
X-C: List-Tasks
X-C: List-Links
```

#### 3. Enable Debug Logging
```
X-C: Define-Dump-Files XMSG-TRACE:TXT
X-C: Debug-Mode-On
```

#### 4. Monitor Activity
```
X-C: List-Messages
X-C: List-Connections
X-C: List-Frames
X-C: List-Utilization
```

#### 5. Diagnose Decoding Failures

**Check Link Health:**
```
X-C: List-Links
```
Look for: Link state (up/down), error counters

**Check Frame Statistics:**
```
X-C: List-Frames
```
Look for: CRC errors, frame rejects, timeouts

**Check Routing:**
```
X-C: List-Routing-Info
X-C: List-Systems
```
Look for: Missing routes, incorrect paths

**Check Message Queues:**
```
X-C: List-Messages
```
Look for: Stuck messages, queue overflow

#### 6. Capture State on Failure
```
X-C: Dump-XMSG
X-C: Get-Error-Message
```

#### 7. Review Dump File
```
@TYPE XMSG-TRACE:TXT
```

---

## Common Troubleshooting Scenarios

### Scenario 1: Messages Not Being Delivered

**Commands to Use:**
```
X-C: List-Ports               # Check port status
X-C: List-Routing-Info        # Verify routes exist
X-C: List-Links               # Check link state
X-C: List-Messages            # Look for queued messages
X-C: Debug-Mode-On            # Enable tracing
X-C: Dump-XMSG                # Capture state
```

### Scenario 2: Link Down/Connection Failures

**Commands to Use:**
```
X-C: List-Links               # Check link status
X-C: Start-Link <link-id>     # Attempt to restart
X-C: List-Network-Servers     # Verify servers running
X-C: List-Frames              # Check for frame errors
X-C: Get-Error-Message        # Get detailed error
```

### Scenario 3: Routing Issues

**Commands to Use:**
```
X-C: List-Routing-Info        # Check routes
X-C: List-Systems             # Verify system reachable
X-C: Get-System-Name-or-Number <system>
X-C: List-Network-Remote-Systems
```

### Scenario 4: Decoding/Protocol Errors

**Commands to Use:**
```
X-C: Debug-Mode-On            # Enable detailed logging
X-C: List-Frames              # Check frame errors
X-C: List-Messages            # Check message format
X-C: Dump-XMSG                # Full state dump
X-C: List-Generation-Variables # Check buffer sizes
```

**Look for in Generation Variables:**
- **Trace Buffers (X5TRB):** Should be > 0 for logging
- **Frame Size:** Input/output frame size settings
- **Timeout Values:** HDLC, RX, TX timeouts
- **Buffer Pages:** Message buffer allocation

### Scenario 5: Performance Issues

**Commands to Use:**
```
X-C: List-Utilization         # Check resource usage
X-C: List-Messages            # Check queue depths
X-C: List-Frames              # Check retransmission rate
X-C: List-Generation-Variables # Check buffer allocation
```

---

## Integration with SINTRAN Commands

### Related SINTRAN Commands

**Service Program:**
```
@SINTRAN-SERVICE-PROGRAM
*START-XMSG              # Initialize XMSG subsystem
*STOP-XMSG               # Shutdown XMSG subsystem
*DEFINE-HDLC-BUFFER      # Allocate HDLC buffers
*LIST-HDLC-BUFFER        # Show HDLC buffer status
*EXIT
```

**User Commands:**
```
@SET-REMOTE-MODE         # Enable COSMOS networking
@SET-LOCAL-MODE          # Disable COSMOS networking
@SET-DEFAULT-REMOTE-SYSTEM <name>
@LIST-OPEN-FILES         # Show XMSG connections
```

**TAD Management:**
```
@START-TADADM            # Start TAD subsystem
@STOP-TADADM             # Stop TAD subsystem
```

---

## Generation Variables Reference

Use `List-Generation-Variables` to view these parameters:

| Index | Parameter | Description |
|-------|-----------|-------------|
| 1 | Tasks | Number of task descriptors (3-764) |
| 2 | Ports | Number of ports (3-777) |
| 3 | Names | Named systems per port (1-1500) |
| 4 | Name Length | Name length in words (4-24) |
| 5 | Messages | Message elements (2-7640) |
| 6 | Message Size | Max message size in bytes (400-77776) |
| 7 | Task Buffer | Buffer space per task (400-177466 bytes) |
| 10 | Buffer Pages | Message buffer pages (1-400) |
| 11 | Multicall | Max multicall functions (0-144) |
| 12 | Systems | Accessible systems (1-1500) |
| 13 | Links | Network links (0-310) |
| 14 | HDLC Timeout | HDLC timeout in XTU's (0-77777) |
| 15 | RX Timeout | Receive timeout in XTU's (0-77777) |
| 16 | TX Timeout | Transmit timeout in XTU's (0-77777) |
| 17 | Frame Input | Input frame size in words (0-37776) |
| 20 | Frame Output | Output frame size in words (0-37776) |
| 21 | ACK Frames | Acknowledgement frames (0-764) |
| 22 | RX Buffers | Receive buffers per link (0-10) |
| 23 | TX Buffers | Transmit buffers per server (1-144) |
| 24 | SABM Tries | Max SABM attempts (0-177777) |
| 25 | Repeats | Repeats before link stop (0-77777) |
| 26 | Max Hops | Max routing hops (0-377) |
| 27 | Gateway TO | Gateway timeout in XTU's (0-77777) |
| 30 | **Trace Buffers** | **Number of trace buffers (0-177)** |

**Key Parameter for Debugging:**
- **Index 30 (Trace Buffers):** Must be > 0 to enable XMSG tracing

---

## Output Files

### Debug Output Locations

1. **Configured Dump File:**
   - Set via `Define-Dump-Files`
   - Contains state dumps from `Dump-XMSG`

2. **Trace Log (if X5TRB > 0):**
   - System may create `xmsg-log.txt`
   - Contains XMSG function call traces
   - Shows XFWRI, XFSND, XFRCV, etc.

3. **System Error Log:**
   - Check SINTRAN error device
   - May contain XMSG trap 46/47 errors

---

## Error Messages

### Common Errors

**"XMSG Kernel error: XMSG is either not generated, not loaded or not started"**
- **Cause:** XMSG subsystem not initialized
- **Fix:** Run `*START-XMSG` in SINTRAN-SERVICE-PROGRAM

**"XMSG fatal error, internal error or inconsistency" (Trap 46)**
- **Cause:** Internal XMSG error
- **Fix:** Run `Get-Error-Message` for details, check dump

**"XMSG user error" (Trap 47)**
- **Cause:** Invalid XMSG function parameters
- **Fix:** Check calling program parameters

---

## Advanced Mode Commands

### Enabling Advanced Mode

```
X-C: Set-Advanced-Mode
```

**Prompt Changes to:** `X-C(Adv):`

---

### 8. Additional Commands Available in Advanced Mode

The following commands become available after issuing `Set-Advanced-Mode`:

#### Link State
```
Get-Link-State
```

#### Trace Commands
```
Open-Trace
Close-Trace
Enable-Trace
Disable-Trace
Dump-Trace-Open
Dump-Trace-Close
Previous-Trace-Element
Next-Trace-Element
Set-Port
```

#### Memory and Variable Commands
```
Look-At-Basefield-Variables
Dump-All-Basefield-Variables
Dump-Memory
List-Command-Prog-Variables
Read-Direct
Write-Direct
```

#### Message and Buffer Commands
```
Get-Message-Space
Release-Message-Space
Set-Current-Message
Clear-Buffer
Fill-Output-Buffer
Append-Integer
Append-String
Buffer-Ready
Decode-Buffer
List-Buffer
```

#### Port and Message Operations
```
Open-Port
Close-Port
Send-Message
Route-Message
Receive-Message
Message-Status
Wait-General
Ask-Route
```

#### Format Commands
```
QFORM-Format
Edit-Format
Fetch-Format
Save-Format
Delete-Format
List-Format-or-Variable
Dump-Formats-To-File
```

#### Driver Commands
```
Create-Driver
Start-Driver
Mode
Dummy-Loop
```

#### Privilege and Mode Commands
```
Set-Privileged
Clear-Privileged
Debugprint-On
Debugprint-Off
Systemmode-On
Systemmode-Off
Monitorcall-On
Monitorcall-Off
```

#### System Commands
```
Disconnect
Set-Maximum-Hop-Count
Lock-POF
Unlock-POF
XMSG-Call
Clear-Advanced-Mode
```

---

## Complete Command List

### Standard Mode (54 commands)

```
List-Ports
List-Tasks
List-Messages
List-Names
List-Routing-Info
List-Links
List-Frames
List-Systems
List-Friend-Systems
List-Connections
List-Utilization
List-Generation-Variables
Define-System-Route
Remove-System
Define-Local-System
Define-Remote-Name
Enable-Route-Through
Disable-Route-Through
Enable-Checksum
Disable-Checksum
Define-Friend-System
Remove-Friend-System
List-Servers
Get-System-Name-or-Number
Deabbreviate-System-Name
Auto-Restart-On
Auto-Restart-Off
Define-Restart-Files
Remove-Restart-Files
List-Restart-Files
Dump-XMSG
Define-Dump-Files
Remove-Dump-Files
List-Dump-Files
Debug-Mode-On
Start-Link
Stop-Link
Define-Alternative-Link
Remove-Alternative-Link
Start-Network-Server
Stop-Network-Server
Define-Network-Connection
Define-Network-Local-Endpoint
Define-Network-Direct-Connection
Remove-Network-Local-Endpoint
Define-Network-Remote-Endpoint
Remove-Network-Remote-Endpoint
Remove-Network-Remote-System
Define-Network-Remote-Groupnumber
Remove-Network-Remote-Groupnumber
List-Network-Servers
List-Network-Remote-Systems
List-Network-Remote-Endpoints
List-Network-Local-Endpoints
Get-Error-Message
List-Version
Set-Advanced-Mode
Help
Exit
?
```

### Advanced Mode (Additional 37 commands)

```
Get-Link-State
Open-Trace
Close-Trace
Enable-Trace
Disable-Trace
Dump-Trace-Open
Dump-Trace-Close
Previous-Trace-Element
Next-Trace-Element
Set-Port
Look-At-Basefield-Variables
Dump-All-Basefield-Variables
Dump-Memory
List-Command-Prog-Variables
Disconnect
Get-Message-Space
Release-Message-Space
Read-Direct
Write-Direct
Fill-Output-Buffer
Clear-Buffer
Append-Integer
Append-String
Buffer-Ready
Decode-Buffer
Ask-Route
List-Buffer
Open-Port
Close-Port
Send-Message
Route-Message
Receive-Message
Message-Status
Set-Current-Message
Wait-General
QFORM-Format
Edit-Format
Fetch-Format
Save-Format
Delete-Format
List-Format-or-Variable
Dump-Formats-To-File
Mode
Dummy-Loop
Create-Driver
Start-Driver
Set-Privileged
Clear-Privileged
Debugprint-On
Debugprint-Off
Systemmode-On
Systemmode-Off
Monitorcall-On
Monitorcall-Off
Lock-POF
Unlock-POF
XMSG-Call
Clear-Advanced-Mode
```

**Total Commands:** 91 (54 standard + 37 advanced)

---

## Quick Reference Card

### Essential Commands for Debugging

```
# Basic Status
X-C: List-Version
X-C: List-Ports
X-C: List-Links
X-C: List-Messages
X-C: List-Utilization

# Enable Debug Mode
X-C: Debug-Mode-On
X-C: Define-Dump-Files <filename>

# Capture State
X-C: Dump-XMSG
X-C: Get-Error-Message

# Advanced Tracing (requires Set-Advanced-Mode)
X-C(Adv): Open-Trace
X-C(Adv): Enable-Trace
X-C(Adv): Dump-Trace-Open <filename>
X-C(Adv): Close-Trace

# Frame Analysis
X-C: List-Frames
X-C: List-Connections

# Configuration Check
X-C: List-Generation-Variables
X-C: List-Routing-Info
```

---

## References

- **COSMOS Programmer Guide:** ND-860164
- **SINTRAN III Communication Guide:** ND-60.134
- **COSMOS System Supervisor:** ND-30.025 / VD-30.025
- **Monitor Call 200B:** XMSGFunction reference

---

## Notes

- All commands support abbreviation (e.g., `L-P` for `List-Ports`)
- Commands are case-insensitive
- Use `Help` or `?` to see command list
- Use `More?` to page through long help output
- Debug mode increases system overhead - disable when not needed
- Dump files can grow large - monitor disk space

---

**Last Updated:** 2025-11-06
**Version:** Based on XMSG command program 210373M (1988.04.12 Release M)
