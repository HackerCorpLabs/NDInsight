# MON 60B 004B/005B/033B - IPMWRITE / IDMWRITE / IDAMW (memory write)

Three subfunctions share one body that writes a data block into ND-500 memory:
- `004B` IPMWRITE - logical program-memory write
- `005B` IDMWRITE - logical data-memory write
- `033B` IDAMW - physical data-memory write

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body byte-location pending (bank-2 5IFUNC).

## Handler (verbatim in `.npl`)
1. `AD:=5DD1` - load the byte count.
2. `IF A><0 OR D>>4000` -> `EBIGBUF; GO FAR ERET`: reject if count > 4000B bytes (buffer size limit).
3. `T:=5D12; A:=5P3; CALL FRUSMOVE` - copy 5D12 bytes of data from user (param3) into MON60 buffer.
4. `GO FAR 5NOPAR` - common path performs the actual write.

## Contract
- byte count 5DD1 <= 4000B (2048) else error EBIGBUF; 5D12 = length copied; params[3]=source data.

## Byte status
VERIFIED: dispatch + 5IFUNC[004/005/033]. From NPL: body. PENDING: L07 body address.
