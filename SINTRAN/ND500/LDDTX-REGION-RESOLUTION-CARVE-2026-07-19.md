# LDDTX region-address resolution carve (2026-07-19)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\LDDTX-REGION-RESOLUTION-CARVE-2026-07-19.md`

Question: how do ABSWR/ABSST/ABSRE/ABSLD in segment `030-S3SM5` resolve a "region"
address (region-25 observed = MPM word-address `0x0045D800`), and can an emulator's ACCP
model compute that address from data it can read out of shared MPM, without guessing?

Grade: **[V]** = byte-cited in the disassembly / C# source · **[I]** = inferred from code
logic · **[UNVERIFIED]** = could not confirm from the material in hand.

Primary sources:
- ASM: `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\segments-ref\030-S3SM5\030-S3SM5.asm`
  (ABSWR@044505, ABSST@044551, ABSRE@044613, ABSLD@044656).
- LDDTX/LDATX/STATX implementation (ground truth for the instruction):
  `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\ND100\Instructions.MemoryReference.cs`
  (calcEL @284, LDATX @317, LDDTX @353, STATX @396).

---

## 1. LDDTX — exact semantics (this is NOT a descriptor-table lookup)

**One line: `LDDTX` (opcode 143302) is "Load Double word, T/X-indexed" — a *privileged
extended-address physical* double-word read. It reads two consecutive physical-memory words
at the 24-bit address `(T<<16)|(X+disp)` into A and D. There is no segment:offset decoding,
no base+bounds, no page/segment descriptor interpretation. The mnemonic in the task
("Load-Descriptor-Table-indexed") is a misnomer.** [V]

Cited C# ground truth (`Instructions.MemoryReference.cs`):
```csharp
private uint calcEL(sbyte displacement) {                       // line 284
    int EL = (regs.currentRegisters.X + displacement) & 0xFFFF; // low 16 bits: X + 3-bit disp, carry dropped
    EL = (regs.currentRegisters.T & 0xFF) << 16 | EL;           // high 8 bits: T selects the 64K bank
    EL = EL & 0xFF_FFFF;                                        // 24-bit physical address
    return (uint)EL;
}
public void LDDTX() {                                            // line 353
    uint EL = calcEL(regs.fetched.mriDisplacement);
    regs.currentRegisters.A = (ushort)ReadEL(EL);               // A := physical[EL]
    regs.currentRegisters.D = (ushort)ReadEL(EL + 1);           // D := physical[EL+1]
}
private uint ReadEL(uint el) => (uint)cpu.ReadPhysicalMemory(el, true); // PHYSICAL, paging-independent
```
Header comment block (lines 232-282) confirms: these 7 SEX-mode instructions
(LDATX/LDXTX/LDDTX/LDBTX/STATX/STZTX/STDTX) "read/write physical memory locations
independent of whether paging is ON or OFF." Address = `(X+disp)&0xFFFF | (T&0xFF)<<16`;
the carry out of `X+disp` is dropped and **not** propagated into T, so T alone picks the
64K bank. For LDDTX (143302) the displacement field n = 0.

Companion ops used by the sibling routines are the same address model, different width:
- `LDATX` (143300, ABSLD@044701): `A := physical[EL]` — single-word physical load. [V]
- `STATX` (143304, ABSST@044575): `physical[EL] := A` — single-word physical store. [V]

**So "resolving region-25" = a plain physical double-word read. The `(A,D)` it returns IS
region-25's 32-bit physical base address, because that address was previously *stored* into
the descriptor table as data.** The "resolution" is one indirection, not a descriptor decode.

---

## 2. The descriptor table and `control_block[21]`

ABSRE (region-25), byte-cited @044625-044632:
```
044625  LDX ,B -56     ; X := mem[B-56]        = pointer to the per-process control block (virtual)
044626  LDD ,X 21      ; A := mem[X+21], D := mem[X+22]   -> (A:D) = P, a 32-bit PHYSICAL pointer
044627  LDX 25         ; X := 25               = the region index (word offset into table at P)
044630  RADD SD DX     ; X := X + D            = 25 + P_low        (146017: src=D, dst=X, no clear) [V]
044631  RADD CLD SA DT ; T := A                = P_high            (146156: src=A, dst=T, CLD=clear) [V]
044632  LDDTX          ; A := phys[P+25], D := phys[P+25 +1]  -> (A:D) = region-25 base = 0x0045D800
```
RADD field decode (verified against the ND-100 register-op encoding; reg codes
1=D,3=B,4=L,5=A,6=T,7=X; bits0-2=dst, bits3-5=src, bit6=CLD):
- `146017` = 0xCC0F: dst=7(X), src=1(D), CLD=0 → `X := X + D`. [V]
- `146156` = 0xCC6E: dst=6(T), src=5(A), CLD=1 → `T := A`. [V]

Therefore the effective physical address handed to LDDTX is
`EL = (P_high & 0xFF)<<16 | ((P_low + index) & 0xFFFF)` = **P + index** (24-bit), where:
- `P = control_block[21]` is a **32-bit physical pointer** stored as the double word at
  virtual `control_block[21..22]` — the base of a table of region base-addresses. [V]
- `index` ∈ {23 (ABSLD), 24 (ABSST), 25 (ABSRE), 26 (ABSWR)} is a **word offset** into that
  table, hard-coded per primitive (`LDX 26/24/25/23` @044521/044574/044627/044672). [V]

**Descriptor format:** each table slot at `phys[P+index]` is a **32-bit (double-word)
physical byte/word address** — the runtime base of that region — read big-word-first into
`(A,D)` (A = high 16, D = low 16). It is *not* base+bounds and carries no flags in the words
LDDTX consumes; the bound/length comes separately from the loop counter (`mem[B+100]`,
`mem[B-100]`) that the routine adds after LDDTX. [V for "32-bit address", I for "no flags in
those 2 words"]

All four primitives are identical except (index, direction):
| primitive | entry | index | phys op after LDDTX | direction |
|---|---|---|---|---|
| ABSLD | 044656 | 23 | `LDATX` @044701 | single-word load  ← region-23 |
| ABSST | 044551 | 24 | `STATX` @044575 | single-word store → region-24 |
| ABSRE | 044613 | 25 | `MOVEW` @044641 | block move        ← region-25 |
| ABSWR | 044505 | 26 | `MOVEW` @044534 | block write       → region-26 |

For ABSRE/ABSWR the `(A,D)` region base from LDDTX is combined with the running word offset
(`mem[B+100]`) via the RADD chain @044633-044640 (resp. @044525-044533) to form the T/A/D/L
physical operands of `MOVEW` (a physical block move). Same region base, per-word cursor. [V]

Note the index values (23-26) are word offsets into the P-table and are a *different*
numbering from the ACCP command codes (021B/022B/023B…) — see the framing correction in
`DUCS-READBACK-REGION-OWNERSHIP-CARVE-2026-07-19.md` §0.

---

## 3. Can the ACCP compute region-25 = 0x0045D800 from shared-MPM-readable data?

**No — not from shared MPM alone. The resolution is gated on the ND-100 `B` register.** [V]

The exact chain to reach `0x0045D800` is:
1. `X = mem[B-56]` — needs the **ND-100 B register** (the PLANC local-frame pointer). B is
   ND-100 CPU state; it is **not** in shared MPM. [V]
2. `P = mem[X+21 .. X+22]` — the control block is in **ND-100 virtual memory** (LDD @044626
   is an ordinary paged virtual read, not a physical one). Reaching it needs step 1 plus the
   ND-100 page tables. [V]
3. `region25 = phys[P+25 .. P+26]` — a physical double-word read. Only *this* last step is a
   physical access the ACCP could in principle perform, **and only if it already knew P and P
   pointed into a window the ACCP can see.** [V]

Steps 1-2 are ND-100-private. The ACCP has no handle to the `B` register, to the control
block's virtual address, or to `P`, from anything in shared MPM. So the ACCP **cannot
replicate `LDDTX(control_block[21]+25)`**. [V]

Cross-check against the conveyed data the ACCP *does* have:
- The LPARP (021B) pointer handed on the wire = `0x00018000` (X5OCT octobus-buffer base).
- The command param block at MPM `0x00420800` = `{N, csWord, 0x00400000, 0x00018000,
  0x00000000, 0x00800000, 0x00400000, 0x00018000, …}`.
- **None of these equals `0x0045D800`, nor is `0x3D800` (= 0x45D800 − 0x00420000) derivable
  from them by any add/shift I can justify.** The region base is simply not present in the
  shared-MPM data the ACCP receives; it lives only in the ND-100-private descriptor table at
  `P`. [V that the value is absent / [UNVERIFIED] that no encoding exists]

**Definitive statement:** region-25's (and region-23/24/26's) runtime address is produced by
an ND-100-side physical indirection rooted in the `B` register and ND-100 virtual memory. It
**cannot be computed ACCP-side from shared MPM**. The emulator must therefore either
(a) observe the address when the ND-100 actually performs the `MOVEW`/`LDATX`/`STATX`
physical access (i.e. let the guest do the resolution and watch the physical bus), or
(b) sidestep the absolute address entirely — for the DUCS/CMRWC checksum path, compute the
16-bit sum self-consistently over whatever words the ACCP itself deposits, exactly as
recommended in `DUCS-READBACK-REGION-OWNERSHIP-CARVE-2026-07-19.md` §"Emulator recipe". The
checksum passes regardless of the absolute region address, so (b) needs no knowledge of
`0x0045D800` at all. [V for the mechanism; I for "recipe (b) works" — logic-forced]

---

## 4. UNVERIFIED / open

- **[UNVERIFIED]** The physical location of the descriptor table `P = control_block[21]`:
  whether it sits in ND-100 local RAM or inside a shared MPM window. If it were provably in a
  window the ACCP can read AND the ACCP were independently told `P`, step-3 alone would let it
  read `0x0045D800`; neither precondition is met by any conveyed value observed. Needs a live
  dump of `control_block[21]` and the segment's frame to settle.
- **[UNVERIFIED]** Whether the two words at `phys[P+index]` carry any flag/length bits beyond
  the raw 32-bit address. The routines take length from a separate counter, so the descriptor
  *words LDDTX reads* appear to be a bare address, but the full slot layout of the P-table
  (and why regions are spaced one word apart at 23/24/25/26) was not carved — the table
  builder (SINTRAN's ND-500 memory allocator) lives elsewhere, not in `030-S3SM5`.
- **[V-adjacent]** `control_block[21]` here is reached only via `LDX ,B -56` / `LDD ,X 21`,
  a pattern repeated at 043702, 044225, 044266, 044404, 044517, 044563, 045077, 045163, …
  (all in `030-S3SM5.asm`) — i.e. `[B-56]→control block, [+21]→physical region-table base` is
  the standard idiom throughout this segment, not a one-off. [V]
