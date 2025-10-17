# HDLC Debugging Guide

**Troubleshooting and debugging strategies**

## Common Issues

### Packets Not Transmitting

**Symptoms**: No data being sent  
**Check**:
1. ACTSW = 1?
2. WTTC configured (1134₈)?
3. DMA descriptor correct?
4. RTTS showing errors?

**Solution**: Verify each component systematically

### Packets Not Receiving

**Symptoms**: No data received  
**Check**:
1. ACTSW = 1?
2. WRTC configured (1734₈)?
3. RRTS bit 0 set?
4. RRTS bit 11 clear (buffers available)?

**Solution**: Check interrupt enable and buffer management

## Variable Monitoring

Monitor these key variables:
- **HASTAT**: Current hardware status
- **ACTSW**: Device activity state
- **XRETRY**: Retry counter

## See Also

**Reference**: [Quick Reference Card](../Quick-Reference-Card.md)  
**Testing**: [Testing Scenarios](Testing-Scenarios.md)
