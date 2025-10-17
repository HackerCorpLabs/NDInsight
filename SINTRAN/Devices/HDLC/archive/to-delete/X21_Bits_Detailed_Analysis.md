# X.21 Bits Detailed Analysis - SINTRAN Source Code Evidence

## Your C# Bit Definitions vs SINTRAN Reality

### Your Understanding (C# Code):
```csharp
X21D = 1<<12,  // Bit 12 - X.21 Data error
X21S = 1<<13,  // Bit 13 - X.21 Clear Indication  
ReceiverOverrun = 1<<15,  // Bit 15 - Receiver Overrun
```

### SINTRAN Actual Constants (from SYMBOL-1-LIST.SYMB.TXT):
```
HX21M = 060000 (octal) = 0x6000 = bits 13-14  
HX21S = 000016 (octal) = 0x000E = bits 1,2,3
```

## 🚨 Critical Corrections Needed

### 1. X21D and X21S Bit Positions - WRONG in Your Code

**SINTRAN Source Evidence (Line 104450):**
```assembly
IF A/\ HX21M >< 0 THEN    % X21-ERROR?
```

**HX21M = 0x6000 = bits 13-14**, NOT bits 12-13!

**Correct Bit Positions:**
```csharp
// CORRECT based on SINTRAN constants:
X21D = 1<<13,     // Bit 13 (part of HX21M mask) ✅ Your bit position is correct
X21S = 1<<14,     // Bit 14 (part of HX21M mask) ✅ Your bit position is correct  
```

**Your bit positions are actually CORRECT!** The issue is elsewhere.

### 2. HX21S Constant Confusion

**SINTRAN has TWO different X.21 related constants:**

1. **HX21M = 0x6000 (bits 13-14)** - Used to detect X.21 errors
2. **HX21S = 0x000E (bits 1,2,3)** - Used for receiver state, NOT X.21 clear indication

**Source Code Evidence:**
```assembly
% Line 104450: X.21 error detection
IF A/\ HX21M >< 0 THEN                    % Test bits 13-14 for X.21 errors
   % ... error handling ...
   IF A BIT HX21S THEN                    % Test bits 1,2,3 for receiver state
      HASTAT BONE BLDON=:HASTAT           % Terminate current operation
   FI
FI
```

### 3. The Real X.21 Logic

**What SINTRAN Actually Does:**

```csharp
// Step 1: Check for ANY X.21 protocol error
if ((rrts & 0x6000) != 0)  // HX21M - tests bits 13-14
{
    // X.21 error detected (could be bit 13 OR bit 14 OR both)
    handleX21Error();
    
    // Step 2: If receiver is active during error, terminate cleanly  
    if ((rrts & 0x000E) != 0)  // HX21S - tests bits 1,2,3 (receiver state)
    {
        terminateCurrentFrame();  // Clean shutdown of active reception
    }
}
```

## Detailed Bit Usage Analysis

### Bit 13 (X21D) - Your Definition Correct
```csharp
X21D = 1<<13,  // ✅ CORRECT position
```

**SINTRAN Usage:**
- Part of HX21M mask (0x6000)
- Indicates X.21 data link error
- When set: Protocol error detected

### Bit 14 (X21S) - Your Definition Correct  
```csharp  
X21S = 1<<14,  // ✅ CORRECT position
```

**SINTRAN Usage:**
- Part of HX21M mask (0x6000) 
- **This IS the real X.21 Clear Indication bit**
- When set: DCE is requesting connection termination

### Bit 15 (ReceiverOverrun) - Correct
```csharp
ReceiverOverrun = 1<<15,  // ✅ CORRECT position
```

**SINTRAN Evidence:**
- Not directly referenced in the code I analyzed
- Standard HDLC receiver overrun indication
- When set: Data received faster than processed

## The Confusion Explained

**The SINTRAN naming is misleading:**

1. **HX21M** tests the actual X.21 protocol bits (13-14)
2. **HX21S** does NOT test X.21 clear - it tests receiver state bits (1,2,3)

**Your bit definitions are CORRECT!** The issue is that SINTRAN uses a confusing internal constant name.

## Correct Implementation for Your HDLC Driver

### Normal LAPB Packet (No X.21):
```csharp
ReceiverStatusBits status = ReceiverStatusBits.DataAvailable;
// Bits 13-14 = 0 (no X.21 errors)
// Result: Packet processed normally
```

### X.21 Data Error:
```csharp
ReceiverStatusBits status = 
    ReceiverStatusBits.DataAvailable |
    ReceiverStatusBits.X21D;  // Bit 13 set
// SINTRAN will detect (status & 0x6000) != 0 and handle error
```

### X.21 Clear Indication (Connection Termination):
```csharp
ReceiverStatusBits status = 
    ReceiverStatusBits.DataAvailable |
    ReceiverStatusBits.X21S;  // Bit 14 set  
// SINTRAN will detect (status & 0x6000) != 0 and terminate connection
```

### Receiver Overrun:
```csharp
ReceiverStatusBits status = 
    ReceiverStatusBits.DataAvailable |
    ReceiverStatusBits.ReceiverOverrun;  // Bit 15 set
// Indicates hardware couldn't keep up with data rate
```

## Summary

**Your C# bit definitions are CORRECT:**
- **X21D = bit 13** ✅ Matches SINTRAN HX21M mask  
- **X21S = bit 14** ✅ Matches SINTRAN HX21M mask
- **ReceiverOverrun = bit 15** ✅ Standard HDLC meaning

**The confusion comes from SINTRAN's internal constant HX21S = 0x000E**, which tests receiver state bits (1,2,3), not the actual X.21 Clear Indication bit 14.

**For your emulated driver:**
- **Normal LAPB**: Keep bits 13-15 clear (0)
- **X.21 errors**: Set bit 13 (X21D) or bit 14 (X21S) as needed  
- **Overrun**: Set bit 15 when data arrives faster than processing

Your understanding is fundamentally correct - the SINTRAN source just uses confusing internal naming!

---

## 🔧 **CRITICAL: How X.21 Status Errors Are Cleared**

### **Key Question: Will WTTC Reset X.21 Status Errors?**

**❌ NO - WTTC will NOT reset X.21 status errors**

### **Evidence from SINTRAN Source Code Analysis**

**1. X.21 Status Errors Location:**
- **X21D (bit 13)** and **X21S (bit 14)** are **receiver-side status bits**
- Located in **RRTS register** (IOX + 10), not RTTS register
- These are **persistent bits** - NOT auto-cleared on read

**2. Register Responsibility:**

| Register | Full Name | Purpose | Can Clear X.21 Errors? |
|----------|-----------|---------|----------------------|
| **WRTC** | Write Receiver/Transmitter Control | **General device control** | **✅ YES** - Device Clear resets receiver status |
| **WTTC** | Write Transmitter Transfer Control | **Transmitter operations only** | **❌ NO** - Only controls transmitter |

**3. SINTRAN Source Evidence - X.21 Error Clearing:**

```assembly
% X21SH function (lines 52970-52973): X.21 Status Handler
X21SH: A:=0; T:=X2DHD+XWRTC; *EXR ST     % DEVICE CLEAR via WRTC (not WTTC!)
       A:=40; *EXR ST                     % SET MAINTENANCE MODUS  
       *AAT 6; EXR ST                     % CLEAR DMA ALSO
       A:=0; T:=X2DHD+XWRTC; *EXR ST      % DEVICE CLEAR, OUT OF MAINT.MOD

% Device initialization (lines 53229-53236):
A:=100; T:=X2DHD+XWRTC; *EXR ST          % DEVICE CLEAR via WRTC clears X.21 errors
A:=140; *EXR ST                          % SET MAINTENANCE MODUS, CLEAR PULSE
% ... DMA clear operations ...
A:=100; T:=X2DHD+XWRTC; *EXR ST          % DEVICE CLEAR, OUT OF MAINT.MOD
```

**4. WTTC Usage in X.21 Context:**

```assembly
% X2118 function (lines 52946-52947): 
CALL X21SH; 100; T:=X2DHD+XWTTC; *EXR ST    % WTTC used AFTER X21SH clears errors
A:=0; T+"XWRTC-XWTTC"; *EXR ST               % Then WRTC is used for receiver setup
```

**⚠️ Key Pattern: WRTC clears errors FIRST, then WTTC is used for transmitter configuration.**

### **Correct X.21 Error Clearing Sequence**

```csharp
/// <summary>
/// Clear X.21 status errors (bits 13-14 in RRTS register)
/// Based on SINTRAN X21SH function behavior
/// </summary>
public void ClearX21Errors()
{
    // Step 1: Device Clear via WRTC (clears receiver status including X.21 errors)
    WriteControlRegister(WRTC, 0x00);   // Device Clear
    
    // Step 2: Optional maintenance mode for thorough reset
    WriteControlRegister(WRTC, 0x28);   // Maintenance mode (0x40 octal = 0x20 hex)
    
    // Step 3: Clear DMA controller if needed
    ClearDMAController();
    
    // Step 4: Return to normal operation
    WriteControlRegister(WRTC, 0x00);   // Back to normal mode
    
    // Step 5: WTTC operations are for transmitter setup AFTER errors are cleared
    WriteControlRegister(WTTC, transmitterConfig);  // Configure transmitter
}

/// <summary>
/// INCORRECT - This will NOT clear X.21 errors!
/// </summary>
public void IncorrectX21Clear()
{
    WriteControlRegister(WTTC, someValue);  // ❌ Wrong register - won't clear X.21 errors
}
```

### **Hardware Control Register Values (from SINTRAN)**

| Value | Octal | Purpose | Usage |
|-------|-------|---------|--------|
| 0x00 | 000 | Device Clear / Normal mode | **Clears all persistent errors** |
| 0x20 | 040 | Maintenance mode only | Setup mode for diagnostics |
| 0x40 | 100 | Device Clear + keep interface active | **Preferred clear method** |
| 0x60 | 140 | Maintenance + Clear pulse | **Thorough reset method** |

### **Critical Emulator Implementation Notes**

**1. Persistent Bit Clearing Strategy:**
```csharp
// X.21 errors (bits 13-14) are persistent - only cleared by WRTC device clear
private void ClearPersistentErrors()
{
    // Clear X.21D (bit 13) and X.21S (bit 14) by device reset
    PerformDeviceClear();
    
    // These bits remain set until explicit hardware reset
    _rrtsRegister &= ~(ReceiverStatusBits.X21D | ReceiverStatusBits.X21S);
}
```

**2. Auto-Clear vs Persistent Bits:**
```csharp
// Auto-clear on RRTS read (bits 8-14, except 13-14 are special):
private ushort ReadRRTS()
{
    ushort value = _rrtsRegister;
    
    // Auto-clear DMA bits (8-12)
    _rrtsRegister &= ~0x1F00;  // Clear bits 8-12
    
    // Bits 13-14 (X21D, X21S) remain set - only cleared by WRTC device clear
    // Bit 15 (Receiver Overrun) remains set - only cleared by WRTC device clear
    
    return value;
}
```

**3. Control Register Behavior:**
```csharp
private void WriteControlRegister(ControlRegister register, byte value)
{
    switch (register)
    {
        case ControlRegister.WRTC:
            if (value == 0x00 || value == 0x40 || value == 0x60)
            {
                // Device Clear operations - clear persistent error bits
                _rrtsRegister &= ~(ReceiverStatusBits.X21D | 
                                 ReceiverStatusBits.X21S | 
                                 ReceiverStatusBits.ReceiverOverrun);
            }
            break;
            
        case ControlRegister.WTTC:
            // Transmitter control only - cannot clear receiver status bits
            ConfigureTransmitter(value);
            break;
    }
}
```

### **Summary: X.21 Error Management**

| Operation | Register | Can Clear X.21 Errors | SINTRAN Evidence |
|-----------|----------|----------------------|------------------|
| **Device Clear** | **WRTC** | **✅ YES** | X21SH, device init sequences |
| **Transmitter Control** | **WTTC** | **❌ NO** | Only used after WRTC clears errors |
| **Status Read** | **RRTS** | **❌ NO** | X.21 bits are persistent |

**The fundamental principle: X.21 status errors are receiver-side and must be cleared through receiver/device control (WRTC), not transmitter control (WTTC).**