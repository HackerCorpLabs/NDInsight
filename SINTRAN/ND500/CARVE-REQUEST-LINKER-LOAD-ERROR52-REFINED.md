# CARVE REQUEST (REFINED) - ND Linker LOAD "52": B004AFBE is the WRONG site

**For:** the sintran/linker byte-carver.
**From:** nd500x linker bring-up (2026-07-18).
**This request:** `/mnt/e/Dev/Ronny/NDInsight/SINTRAN/ND500/CARVE-REQUEST-LINKER-LOAD-ERROR52-REFINED.md`
**Supersedes/extends:** `CARVE-ANSWER-LINKER-LOAD-ERROR52.md`
**Disassembly:** `/mnt/d/ND/500/nd-linker/linker-b01.dom.asm` (105,189 lines).

## Thank you - and one empirical correction

Your answer pointed at **B004AFBE** (`r:=0xB00530BC; w comp2 r.0x0,$0x1`) ->
**B004AFC3** `w1:=$0x106A; setk` as the raise site for the "52".

We instrumented the running emulator (single-step, break on PC, dump on hit,
K-flag 0->1 watch). Result **disproves that as the LOAD raise site**:

- **B004AFBE is reached exactly ONCE, at instr 5448, during STARTUP** (before any
  command is typed). At that moment `word[0xB00530BC] = 0x00000001`, so the
  compare **passes** - no error. Dump: `B00530BC: 00 00 00 01 00 00 00 13 0D 0A
  54 68 69 73 ...` (the DDBTABLES header + "This ...").
- During the **LOAD B:NRF** command (instr ~162502..223670) **B004AFBE is never
  reached at all**. So B004AFC3 does not fire for LOAD.
- 0x106A is loaded at only two sites in the whole binary: B004AFC3 (setk, startup
  only) and B00150DB (`w1 comp $0x106A`, the command-loop catcher). Neither is the
  LOAD raise.

So B004AFBE/0x106A is the **DDBTABLES-table validity check at startup**, which
succeeds. The LOAD-command "52" is raised somewhere else.

## What we DID pin dynamically (please trace from here)

The error text is rendered by this chain (verified by MON trace + disasm):
`B0035EAA / B0035EC3  ->  call B004C95B  ->  call B004C9C1  (-> 143B RSIO @B004CA35)`.

- **B004C95B** is the SSI:code formatter. It takes the SSI and error-code as INPUT
  in caller b-slots (b.0x14..b.0x20), i.e. the "52" is passed in, not computed
  there.
- Its only two callers are **B0035EAA** and **B0035EC3** (a shared "print error"
  helper in B0035Exx).
- In the LOAD round the K flag is set (0->1) at many sites, but NONE carry an
  error code with `code & 0x3F == 0x2A` (=052) in I1. Representative LOAD-round
  K-sets (PC, raised-at, I1): B0048855/B0048853 I1=0x1010; B0016490/B0035311
  I1=0x9016; B0014DD1/B0016446 I1=0x9016; B004CB2B/B004CB26 I1=0x100. So the "52"
  reaches the display via a memory slot, not I1.

## The questions (byte-level, cite addresses)

1. **Who calls B0035EAA / B0035EC3 during the LOAD command path**, and what SSI +
   error-code do they pass? Trace back from B0035Exx to the LOAD-command handler
   to find the instruction that decides "52" and sets it into the slot the printer
   reads. That is the real raise site (the analogue of B004AFC3, but for LOAD).

2. **What does that "52" actually mean** for LOAD (octal 052 = 0x2A low bits; full
   code unknown - could be 0x102A, 0x2A, 0x_2A). Is it "illegal object / not an
   NRF", "no current segment / no domain selected", "segment table full", "wrong
   object version", or a file-state code? The NRF `B.NRF` is a real 513-byte NC
   object (starts `0A 00 01 70 ...`, contains PROG!NAME + symbol strings).

3. **Confirm the LOAD-command control flow.** LOAD resolves `B:NRF` via the shared
   resolve helper B0000A3D (success), then errors BEFORE any 50B OPEN of B.NRF and
   before B004AFBE. Which routine is the LOAD-command body (dispatcher B0035972 ->
   ? ), and what state does it check between name-resolve and open that fails?
   Prime candidates we could not confirm: a "current segment"/"domain selected"
   flag, or a header field of the freshly OPEN-DOMAIN'd A-TEST domain that our
   emulator leaves zero.

4. **Separately, the EXIT path page-faults** at PC=B001F66D (`w stz @b.0x2C`
   stepping to limit b.0x20, bad base) - a zero-fill loop finalising an
   incompletely-loaded domain. Likely a downstream symptom of LOAD not completing;
   note if the same missing state explains it.

## Ground truth we verified this session (so you can rely on it)

- DDBTABLES-G06.VTM word0 = 0x00000001 (the startup check's expected value). OK.
- MON 144B MAGTP read-record writes to buffer offset 0 (NOT file-position-mirrored)
  - we tried mirroring and it regressed startup; buffer[0] is correct.
- OPEN-DOMAIN "A-TEST" fully succeeds (4096-byte domain, 120B WFILE x2). The blocker
  is purely the LOAD-command post-resolve error "52".

## nd500x references

- LOAD driver: `/home/ronny/repos/nd500x/test/diag_linkdrive.c` (now has
  ND500X_WATCH_VA, ND500X_KWATCH, ND500X_BREAK_PC/BREAK_DUMP), run pinned from
  `/home/ronny/repos/nd500x/build/link_sandbox/`.
- MAGTP handler: `/home/ronny/repos/nd500x/src/libmon/handlers/mon_144B_DeviceFunction.c`
