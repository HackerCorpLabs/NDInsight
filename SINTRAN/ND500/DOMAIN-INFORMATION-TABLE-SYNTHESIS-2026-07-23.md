# ND-5000 Domain Information Table (DIT) — what it is, where PIA lives, how to build it

**Date:** 2026-07-23
**Author:** octobus/context lane session (3-agent research synthesis)
**Context:** follow-up to `SWAPPER-MICROWORD-PIA-RESOLVED-2026-07-23.md` — the swapper's PIA
comes from a DIT byte the harness doesn't populate. This doc defines the DIT from the manuals,
the B30 microcode, and the carved SINTRAN, and flags the discrepancies to resolve before we
emulate it properly.

Grading: **[MANUAL]** = quoted from ND docs. **[MICROCODE]** = decoded/measured from the B30
microcode. **[CARVE]** = from carved SINTRAN. **[OPEN]** = unresolved / needs verification.

---

## 1. What the DIT is  [MANUAL]

Sources: `ND-05.009.4 ND-500 Reference Manual` (byte layout), `ND-05.020.01 ND-5000 Hardware
Description` (defers MMS details to the ND-500 manual).

- One DIT = **256 bytes per domain**; up to 256 DITs per process, laid out on the process segment.
  Each keeps the structure/properties of one domain.
- Selected by the **CED** register (Current Executing Domain, 8-bit domain number). The process
  segment is reached via **PSTP → PS** (physical segment table walk); there is **no single "DIT
  base register"** in the manuals.
- Inside a DIT: program capability table @0B (32×2B), data capability table @100B (32×2B),
  domain-call save @200B, trap-handling save @213B, domain characteristics @226B..310B.
- **PIA = bit 0 of the 1-byte "Domain status" field at DIT offset `310B` (octal) = 0xC8.**
  (ND-500-REF Table 6.) "Privileged Instructions Allowed … defined in the domain information
  table … may not be changed by instructions." At runtime it is *copied* into macrostatus
  (dest 84H) bit 1, which the microcode checks on every privileged instruction.

## 2. How the B30 microcode actually reads it  [MICROCODE]

Path: `CNTXTLOAD` → `TRAPSET` (0o15057) → **`CED_TO_DIT`** (0o12035) → TRAPSET3 block
(0o15072..0o15102).

- `CNTXTLOAD` @0o14747 sets `DPA = PCB base` (used to read P/L/B/R/macrostatus from the context
  block).
- **`CED_TO_DIT` @0o12035 re-points DPA at the DIT:** `Q = CED`, a doubling loop scales it, then
  `0o12043: DPA = BM07 + Q` where **BM07 = 0x80**. So `DPA = 0x80 + domain*stride`.
- TRAPSET3 block issues `RD,PHYS` reads at `AA=2(DPA) + AB=1(MARG)`; the PIA byte lands in **SC7**
  (read issued 0o15075), and 0o15100–0o15102 test **SC7 bit 0** and OR/ANDCB `MIC,STS` bit 1.
- **Measured:** harness domain ⇒ DPA=0x80, PIA byte at **DPA+0x48 = 0xC8**, bit 0. Seeding it
  advances the swapper 34→46 instrs. This is the rock-solid fact.

## 3. Who builds it  [CARVE]

- **No carved SINTRAN ND-100 routine builds the DIT** (nor the context block, PST, or PCB).
  SINTRAN only *reads* the per-process register block; it knows a per-process DIT *number*
  (`5DITN`/`5DITNO`) and has a commented-out `SETDIT` monitor function, but nothing writes the
  physical DIT.
- On cold start the swapper's domain regs (`P=4, PS=1, DOM=ADOM=1, PSTP=0`) come from **microcode/
  ACCP literals** (`MACRO_STARTL`), not SINTRAN.
- Symbol-table `AUPIA`/`NUPIA` are **ND-100-side** paging/XMSG masks, NOT the ND-500 domain PIA.
- Existing prior decode: `CNTXT-BLOCK-DECODE-2026-07-17.md` already had `DIT = 0x80 + CED*256`
  and "byte [DIT+0x48] gates MIC,STS bits."

## 3b. NDIX cross-check — DIT == PCB, and everything agrees  [CODE, authoritative]

NDIX (real ND-500 Unix) defines the DIT as `struct pcb` — `E:\Dev\Ronny\NDIX-C\kernel\MASTER\
machine\pcb.h` (3.7, 88/11/29): *"pcb.h - Process Control Block (DIT) for ND-500"*. 256 bytes,
byte-packed, up to 256 per process (the process segment), indexed by the logical segment / domain.
`pcbtab[]` @ KVA 0xe0000000. **This is the authoritative real-world layout** and it matches the
ND-500 manual Table 6 exactly. Computed byte offsets (byte-packed: char=1, short=2, long=4, ptr=4):

| offset | field | | offset | field |
|--------|-------|-|--------|-------|
| 0x00 | pcb_pc[32] (prog caps) | | 0xBA | pcb_md |
| 0x40 | pcb_dc[32] (data caps) | | 0xBB | pcb_ith (inside trap handler) |
| 0x80 | pcb_call (call_ce/ca/x, call_p, call_b) | | 0xBC | pcb_tos |
| 0x8B | pcb_trap (trap_ce/ca/x, trap_st1/2) | | 0xC0 | pcb_ll |
| 0x96 | pcb_ote1/2 (own trap enable) | | 0xC4 | pcb_hl |
| 0xA6 | pcb_cte1/2 (child trap enable) | | **0xC8** | **pcb_pia (PIA = bit 0)** |
| 0xAE / 0xB6 | pcb_mte, pcb_temm | | 0xC9 | pcb_xxx[3] |
| 0xB6 | pcb_tha (trap handler addr) | | 0xCC | pcb_cad, pcb_ced |

**Every microcode read maps onto a real NDIX field** (microcode DPA = pcb_base + CED*256 + **0x80**,
i.e. it points at `pcb_call`; reads are DPA+offset):

| microcode | PCB offset | NDIX field |
|-----------|-----------|------------|
| DPA+0x16 | 0x96 | pcb_ote1 (own trap enable) |
| DPA+0x26 | 0xA6 | pcb_cte1 (child trap enable) |
| DPA+0x3B | 0xBB | pcb_ith |
| DPA+0x3C | 0xBC | pcb_tos (→SRF12) |
| DPA+0x40 / +0x44 | 0xC0 / 0xC4 | pcb_ll / pcb_hl |
| **DPA+0x48** | **0xC8** | **pcb_pia** |

**CORRECTION of §4 below:** PIA is at PCB struct offset **0xC8** — the manual's 310B and NDIX agree,
and there is NO "compact vs manual layout" conflict. The microcode's "+0x48" is simply measured from
its within-entry base of PCB+0x80 (pcb_call): 0x80+0x48 = 0xC8. NDIX also confirms **KDOM = kernel =
domain 0** (so a privileged cold-start domain 0 is exactly right).

## 3c. NDIX resolves "who sets PIA", + the Context Block (CXB) layout  [CODE, authoritative]

From `E:\Dev\Ronny\NDIX-C\verified-docs\VERIFIED_SOURCE_CODE_REFERENCE.md` (quotes real kernel C):

**Who sets PIA — RESOLVED (was OPEN in §5).** `kpcbinit()` (vm_machdep.c:120-168) initialises the
kernel PCB and does `pcp->pcb_pia = 1;`, sets `pcb_call` (call_ce=1 domain 1, call_p=4, call_b=
USRSTACK), then `asm("dctsb");` to load the new data. **The OS kernel writes pcb_pia into the PCB/DIT
— exactly what our harness `SetDomainPia` does.** So on a real SINTRAN/NDIX boot PIA is OS-seeded, not
microcode/ACCP-seeded; the privileged-instruction hang only appears because our bring-up harness had
no OS to run kpcbinit. `dctsb` itself is the kernel's "load new data / commit PCB" instruction.

**Context Block (CXB) — `struct cxb`, CXBSIZ = 256** (cxb.h). This is the per-process register
save/restore block that CNTXTLOAD reads (base 0o4000 in the harness). Byte-packed `long` fields (all
offsets ×4) — **matches our reverse-engineered ctx-block layout exactly**:

| off | field | off | field | off | field |
|-----|-------|-----|-------|-----|-------|
| 0x00 | cx_p (P) | 0x30-0x3C | cx_e1-e4 (E1-4) | 0x64/0x68 | cx_nu1/nu2 |
| 0x04 | cx_l (L) | 0x40 | cx_st1 (ST1) | 0x6C/0x70 | cx_am11/al11 |
| 0x08 | cx_b (B) | 0x44 | cx_st2 (ST2) | 0x74.. | cx_ote/cte/mte/temm |
| 0x0C | cx_r (R) | 0x48 | cx_ps (PS) | 0x94/0x98 | cx_mic1/mic2 |
| 0x10-0x1C | cx_i1-i4 (X1-4) | 0x4C | cx_tos | 0x9C.. | cx_pmem[10] |
| 0x20-0x2C | cx_a1-a4 (A1-4) | 0x50/0x54 | cx_ll/cx_hl | 0xC4.. | cx_trapnum/bsp/trap_p/info/vaddr |
| | | 0x58 | cx_tha | 0xD8.. | cx_sftwbuffer[10] |
| | | 0x5C/0x60 | cx_ced/cx_cad | | |

NB the CXB HAS slots for tos/ll/hl/tha/ote/... (0x4C-0x90), but CNTXTLOAD sources those from the DIT,
not the ctx block (they are the trap-save copies) — consistent with the carve.

**Also verified in NDIX** (for the MON-600 / ACCP lane, not the DIT): the fecall interface
(ND-500→ND-100 call = MON 600): `FE_INIT/IDEV/OPEN/CLOS/READ/WRIT/DCTL/EXIT/ERRM` (if.h), packet
structs `init_pkt`/`exit_pkt`, generic devices (DISK/TAPE/TERM/CLOCK/XMSG...), and address macros
`htob(x)=x<<1` / `btoh(x)=x>>1` (ND-100 word ↔ ND-500 byte), NBPG=2048, NBSG=128MB. NOTE: 3022/5015
bus-interface details are NOT in NDIX source (only its .md docs) — that stays with the SINTRAN carve.

## 4. Microcode addressing — VERIFIED by raw control-store decode  [MICROCODE, verified]

Pinned by `MicrowordDecodeTests.Dit_AddressingPath_RawDecodeDump` (reads the raw B30 words via
`new Microword(cs.Hi[a],cs.Lo[a])`, bypassing the `.md` ORCON rendering bug). Raw `MARG` values on
the TRAPSET3 `RD,PHYS` rows (AA=2=DPA, AB=1=MARG):

| microword | raw MARG | DPA-relative read | role |
|-----------|----------|-------------------|------|
| 0o15072 | 0x16 | DPA+0x16 | address primer (no RD tag) |
| 0o15073 | 0x3B | DPA+0x3B | trap/limit byte |
| 0o15074 | 0x26 | DPA+0x26 | trap/limit byte |
| **0o15075** | **0x48** | **DPA+0x48** | **→ SC7; bit 0 = PIA** |

1. **PIA read = DPA+`0x48` (DEFINITIVE).** Raw `MARG=0x48` at 0o15075. The `.md` rendered this as
   "ORCON=0x08 IX*2" — `0x48 = 0x40|0x08` is exactly the rendering-bug split. Since DPA = pcb_base +
   CED*256 + 0x80 (§3b), the PIA byte is at **PCB struct offset 0xC8 = pcb_pia** — matching the
   manual's 310B and NDIX. (Earlier "compact layout" framing was wrong; see §3b correction.)
2. **DIT base = BM07 = 0x80 (verified).** `CED_TO_DIT` 0o12043: `DPA := A,BM07 + Q` (AOp=7 = BM07 =
   1<<7 = 0x80).
3. **Stride = 256 bytes/domain (VERIFIED).** `Probe_DitStride_PerDomain` sweeps CED (context block
   +0x5C) and reads the DIT-read address: `DPA = 0x80 + CED*0x100`. So `stride = 0x100 = 256` —
   matching the ND-500 manual's 256-byte DIT and CNTXT-BLOCK-DECODE. (The `CED_TO_DIT` EXUC doubling
   loop 0o12037–0o12042 shifts CED left by 8; agent-2's static "×16" read was wrong.) PIA byte for
   domain d = `0x80 + d*0x100 + 0x48`.

## 5. How to build it

1. **Verify (raw-decode, not the .md)** — DONE (§4): base=0x80, PIA at +0x48, stride=CED<<n.
2. **Model a DIT (harness level)** — DONE. `SwapperStartDiagnosticTests.SeedDomainInfoTablePia`
   writes a structured DIT entry (`DitBase=0x80` + `DitPiaOffset=0x48`, bit 0) instead of the raw
   `mem.Bytes[0xC8] |= 1` stopgap. Suite 192/192; swapper still 34→46.
3. **[TODO] Deeper: model the DIT inside the emulator** (CpuND5000/MmsUnit) with the full field set
   the microcode reads (PIA @+0x48; trap/limit bytes @+0x16/+0x26/+0x3B; plus the +0x3C/+0x40/+0x44
   limits per CNTXT-BLOCK-DECODE), and the real stride (pin the CED<<n loop count) so domain ≠ 0
   works. Belongs with the deferred MMU/RD,ADOM domain-select work.
4. **[TODO] Real boot** — determine who sets PIA on a genuine octobus/SINTRAN boot (microcode/ACCP
   literal `MACRO_STARTL`, or code outside the carved NPL) so the emulated DIT matches reality.

## Source files
- Manuals: `E:\Dev\Ronny\ND5000UC\manual\ND-05.020.01 EN ...`, `E:\Dev\Ronny\NDInsight\Reference-Manuals\ND-05.009.4 EN ND-500 Reference Manual.md` (Table 6).
- Microcode: `E:\Dev\Ronny\ND5000UC\microcode\MICRO-5800-B30.md` (:6717, :6728-6737, :5163-5169), `manual\MICROCODE-FIELDS.md`.
- Carve: `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\CNTXT-BLOCK-DECODE-2026-07-17.md`, `CARVE-SWAPPER-CONTEXT-BLOCK-BUILDER-2026-07-20.md`.
