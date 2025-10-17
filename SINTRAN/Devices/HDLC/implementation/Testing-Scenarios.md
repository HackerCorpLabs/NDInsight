# HDLC Testing Scenarios

**Comprehensive test cases for SINTRAN HDLC implementation**

## Normal Operations

### Test 1: Single-Frame Transmission
**Setup**: Send small packet (<1 buffer)  
**Expected RTTS**: 0x0000-0x0200 (success, no errors)  
**Expected**: (RTTS & 0x8002) == 0

### Test 2: Single-Frame Reception
**Setup**: Receive small packet
**Expected RRTS**: 0x0001 (DataAvailable set, no errors)
**Expected**: Bit 0=1, bits 11,13-14 all clear

## Error Conditions

### Test 3: Transmitter Underrun
**Setup**: Simulate slow memory access
**Expected RTTS**: 0x0002 (TXUND set)
**Expected**: Retry mechanism activated

### Test 4: Buffer Exhaustion
**Setup**: Exhaust receive buffers
**Expected RRTS**: 0x0801 (ListEmpty set)
**Expected**: ACTSW forced to 0, receiver stopped

## See Also

**Implementation**: [Emulator Guide](Emulator-Implementation-Guide.md)  
**Debugging**: [Debugging Guide](Debugging-Guide.md)
