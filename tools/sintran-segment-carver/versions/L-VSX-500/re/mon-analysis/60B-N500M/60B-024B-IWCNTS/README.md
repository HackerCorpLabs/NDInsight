# MON 60B 024B/157B - IWCNTS (WRITE CONTROL STORE)

Writes microcode words directly into the ND-500 writable control store (WCS). `024B` (WRICS) and `157B` share this body. Compare `037B ICSLOAD` (load CS from a FILE); this one takes CS words inline from the caller.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
1. `IF 5D22>>2000 GO FAR ERET` - reject if CS-word count > 2000B (1024).
2. `T:=5D22 SH 1; A:=5P3; CALL FRUSMOVE` - copy 5D22*2 bytes (CS words are 2 bytes) from user (param3).
3. `GO FAR 5NOPAR` - common path writes the WCS.

## Emulator relevance
Direct WCS write path (vs the file-based `037B ICSLOAD`). Part of the control-store bring-up surface.

## Byte status
VERIFIED: dispatch + 5IFUNC[024/157]. From NPL: body. PENDING: L07 body address.
