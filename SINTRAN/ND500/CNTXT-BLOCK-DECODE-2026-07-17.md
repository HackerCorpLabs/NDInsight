# ND-5800 (SAMSON) CONTEXT BLOCK — what NEWCNTXT / CNTXTLOAD0 / GET_CNTXT actually read

Decode date: 2026-07-17. Sources: `E:\Dev\Ronny\ND5000UC\microcode\MICRO-5800-B30.md`
(lossless listing, commit a91dff4, primary), `E:\Dev\Ronny\ND5000UC\microcode\MICRO-5800-A30.md`
(cross-check), decode model per `E:\Dev\Ronny\ND5000UC\microcode\MAILBOX-MICROCODE-PSEUDOCODE.md`
§3.10 (memory op on word N uses the ADACT address of word N−1; EA = AA-base + sign-extended
MARG in BYTES; on AB=1 words the printed `IX*n`/`ORCON=` tokens ARE the MARG bits:
IX*2=0x40, IX*8=0xC0), field mnemonics per `E:\Dev\Ronny\ND5000UC\manual\MICROCODE-FIELDS.md`.

Evidence grades: **[V]** = read directly from the decoded fields; **[D]** = derived/inferred
(reason given); **[?]** = undecoded, raw words shown. All μ-addresses are octal; all context
offsets are **BYTES (hex)** unless marked HW.

A30/B30: the whole routine cluster exists identically in A30 at shifted addresses
(GET_CNTXT A30=012404 vs B30=013370; CNTXTSAVE A30=013702 vs B30=014666; CNTXTLOAD
A30=013756 vs B30=014742) — word content of GET_CNTXT verified identical [V]; the rest
spot-checked at the labels only [D]. All addresses below are B30.

---

## 1. GET_CNTXT (`013370`) — the context-address formula

```
| `013370` | GET_CNTXT | ALU,A+B,*2 EXUC A,SC12 B,SC12 D,SC12 COND,MSEXO TBC,NEXT [ADDR=GET_CNTXT] |
| `013371` |           | ALU,A+B,*2 EXUC A,SC12 B,SC12 D,SC12 COND,MSEXO TBC,NEXT [ADDR=GET_CNTXT] |
| `013372` |           | ALU,FZRO EXUC A,BM00 B,X1 T,RETURN COND,MSEXO TBC,NEXT [ADDR=OFFSET] |
| `000020` | OFFSET    | ALU,A A,LARG LARG=00000004000 B,X1 D,SC13 T,RETURN COND,MSEXO TBC,NEXT |
```

Semantics of the pieces:

- `ALU,A+B,*2` with A=B=SC12 ⇒ `SC12 := (SC12+SC12)*2 = SC12*4` per execution
  [V — MICROCODE-FIELDS.md line 120: "FBUS = ALU.output*2, FBUS(0)=0"].
- `OFFSET` (000020) loads `SC13 := 0o4000` (LARG constant) [V]. It is reached via the
  `EXUC + ADDR=OFFSET` mechanism ("execute microinstructions in the pipeline even when the
  pipeline is broken — to prevent code duplication", MICROCODE-FIELDS.md line 863): the
  shared word at ADDR executes in the branch/return shadow without a control transfer
  [D — mechanism inferred; required by dataflow, see below].
- Callers all follow the identical pattern (e.g. TRAP_FIND):

```
| `013152` | TRAP_FIND | ALU,A A,SRF11 B,X1 D,SC12 T,JMP, T,PUSH ... [ADDR=GET_CNTXT] |
| `013153` |           | ALU,A+B A,SC12 B,SC13 D,DAC,DPA T,JMP, T,LOAD ... [ADDR=DUMMY_2] |
```
  i.e. `SC12 := index; call GET_CNTXT; DPA := SC12 + SC13` [V].

### The formula

```
ctx(idx) = 0o4000 + idx * 256          (byte address, "physical with MMS" space — RD,POF)
```

- Base `0o4000` (=0x800 bytes) [V — the OFFSET LARG constant].
- **Stride 256 bytes** [D — see the scale argument]. The two `A+B,*2` words print only ×16,
  but each carries `EXUC` with `ADDR=GET_CNTXT` (self); under the EXUC sneak mechanism each
  executes twice ⇒ ×256. This is *proven consistent* three independent ways:
  1. CNTXTSAVE (`014666-014671`) and CNTXTLOAD (`014742-014745`) compute the SAME block
     address with **four plain `A+B,*2` words** (no EXUC, `T,JMP ADDR=next` each ⇒ each
     executes exactly once) = ×256 [V], then the same `OFFSET` + `DPA := SC12+SC13`.
  2. The block spans at least 0xBC bytes (trap park area ends at ctx+0xBA, §3 below), so a
     16-byte stride is impossible for the same index domain.
  3. The same fields written by CNTXTSAVE (×256 path) are read back by the trap machinery
     that gets its address from GET_CNTXT (TRAP_SAVE/TRAP_LOAD/TRAP_GEN), so both must map
     to the same block.
- The index is used as a halfword (callers mask with `TYP,HW`); SRF11 bit 31 (`BM37`) is a
  flag ("no context loaded / context already saved"), stripped before use [V — 014662
  `TYP,HW`, set at 014737-014741 and in MSG_KILL_P 013723].

### Where the index comes from

`SRF17 := (message HW@4) + 1` — MSG_LINK7 word `015203` reads msg+0x08 (HW 4, the field the
message catalog names **X5CPU**) into SC4 [V], and `015211` stores `SC4+1` (`CRY,ONE`) into
SRF17 [V]. NEWCNTXT then copies SRF17 → SRF11 as the *current* process index. So:

```
context address = 0o4000 + 0o400 * (msg.HW[4] + 1)
```

GET_CNTXT callers (B30): 011000 (index from SC11), 011220, 012012, **013152 TRAP_FIND**,
**013262 TRAP_LOAD**, **013774 TRAP_PGFENA**, 016007 (MSG_PRT, index SRF17), 016024
(MSG_UNIX5RE, SRF11), 016077 (MSG_UNIX5REL, SRF17).

### Same mechanism, second instance: CED_TO_DIT (`012035`)

```
| `012035` | CED_TO_DIT | ALU,FZRO Q,F ...                ADDR=012036 |   Q := 0
| `012036` |            | ALU,XOR Q,F A,SRF14 B,Q ...     [ADDR=NEW_TO_DIT] |  Q := SRF14 (CED)
| `012037-012042` | NEW_TO_DIT | 4x: ALU,A+B EXUC Q,F A,Q B,Q [ADDR=NEW_TO_DIT] |  Q := Q*2 (EXUC-self)
| `012043` |            | ALU,A+B A,BM07 B,Q D,DAC,DPA ... |   DPA := 0x80 + Q
```
Under the same double-execution rule: **DIT(domain) = 0x80 + CED*256** (physical space,
`RD,PHYS`) [D — same caveat as the GET_CNTXT stride]. The DIT (Domain Information Table) is
what TRAPSET reads the trap enables and limits from (§3, step 8).

---

## 2. The context-block field table

Direction: L = loaded by CNTXTLOAD0, S = stored by CNTXTSAVE, T = trap park/read
(TRAP_SAVE writes / TRAP_LOAD+TRAP_GEN read). Width: w = 32-bit word, hw = halfword.
"Save μ / Load μ" = the microword carrying the WR/RD (its address comes from ADACT on the
preceding word, per the §3.10 pipeline rule).

| Byte off | HW off (oct) | Width | Content / destination on load | Dir | Grade | Save μ | Load μ |
|---|---|---|---|---|---|---|---|
| 0x00 | 0 | w | **P** (`D,IAC,P` via SC3) | L/S | [V] | 014702 | 014751→014757 |
| 0x04 | 2 | w | **L** link reg (`D,IAC,L` via SC4) | L/S | [V] | 014703 | 014752→014760 |
| 0x08 | 4 | w | **B** base reg (save src `A,DAC,B`; load dest printed `D,DAC,REG04`) | L/S | [V] save / [D] load dest (by symmetry; `REG04` is the regenerator's raw-code name) | 014704 | 014753→014755 |
| 0x0C | 6 | w | **R** record reg (save src `A,DAC,XFER`; load dest printed `D,DAC,LDRES`) | L/S | [D] (XFER/LDRES read/write ports of R by symmetry with A,DAC,R=353) | 014705 | 014754→014756 |
| 0x10 | 010 | w | **X1** (=I1) | L/S | [V] | 014706 | 014762 |
| 0x14 | 012 | w | **X2** (=I2) | L/S | [V] | 014707 | 014763 |
| 0x18 | 014 | w | **X3** (=I3) | L/S | [V] | 014710 | 014764 |
| 0x1C | 016 | w | **X4** (=I4) | L/S | [V] | 014711 | 014765 |
| 0x20 | 020 | w | **A1** | L/S | [V] | 014712 | 014766 |
| 0x24 | 022 | w | **A2** | L/S | [V] | 014713 | 014767 |
| 0x28 | 024 | w | **A3** | L/S | [V] | 014714 | 014770 |
| 0x2C | 026 | w | **A4** | L/S | [V] | 014715 | 014771 |
| 0x30 | 030 | w | **E1** | L/S | [V] | 014716 | 014772 |
| 0x34 | 032 | w | **E2** | L/S | [V] | 014717 | 014773 |
| 0x38 | 034 | w | **E3** | L/S | [V] | 014720 | 014774 |
| 0x3C | 036 | w | **E4** | L/S | [V] | 014721 | 014775 |
| 0x40 | 040 | w | **Status composite** = ALU,STS&0o00200177740 \| MIC,STS&0o37404200037 \| IDU,STS&0o00173400000 (READST1 014722/015017-015025). On load WRITEST1 (015030-015035) redistributes: IDU,STS := w&0o00173400000; MIC,STS := w; ALU flags via ST,LOAD. MSG_PRT (3PRT) ORs bit BM35 into this word (016011-016013). | L/S | [V] masks/dests | 014726 | 014776→015001 |
| 0x44 | 042 | w | **SRF10 & 0o1777** (trap/status bits; READST2 014724/015026-27, WRITEST2 015036-015040: SRF10 := w, ALU,STS \|= (w & mask)) | L/S | [V] src/dest, [D] meaning | 014727 | 014777→015002 |
| 0x48 | 044 | w | **SRF13**; low halfword also → **MM,PS and MM,PHS** (MMU process-segment registers, NEW_PS_1 015043-44) | L/S | [V] dests | 014732 | 015000→015004 |
| 0x4C | 046 | w | **TOS** (top of stack) — DOMAIN reg, DIT-sourced (manual A.1); NEWCNTXT does not touch | (DIT) | [DOC-manual] | — | — |
| 0x50 | 050 | w | **LL** (lower limit) — DOMAIN reg; loaded from DIT+0x40 by TRAPSET (§3 step 8), NOT ctx | (DIT) | [DOC+V] | — | (DIT) 015061 |
| 0x54 | 052 | w | **HL** (higher limit) — DOMAIN reg; loaded from DIT+0x44 by TRAPSET, NOT ctx | (DIT) | [DOC+V] | — | (DIT) 015064 |
| 0x58 | 054 | w | **THA** (trap handler address; base of 64×32-bit trap array, manual §7.13/A.1) — DOMAIN reg, DIT-sourced | (DIT) | [DOC-manual] | — | — |
| 0x5C | 056 | w | **CED** (current executing domain): save src SRF14; load: byte → SRF14 **and MM,DOM** (NEW_CED 015053-54) | L/S | [V] | 014733 | 015007→015011 |
| 0x60 | 060 | w | **CAD** (current alternative domain): save src SRF15; load: byte → SRF15 **and MM,ADOM** (NEW_CAD 015055-56) | L/S | [V] | 014734 | 015010→015012 |
| 0x64 | 062 | w | **CES** (current executing segment) — DOMAIN reg, DIT-sourced (manual A.1); NEWCNTXT does not touch | (DIT) | [DOC-manual] | — | — |
| 0x68 | 064 | w | **CAS** (current alternative segment) — DOMAIN reg, DIT-sourced (manual A.1) | (DIT) | [DOC-manual] | — | — |
| 0x6C | 066 | w | **SC1** scratch (observed use: the 3MONCO NUMPA write-back bit mask — MSG_CONMC 015726 loads SC1 from msg HW@0o12 area, MSG_CONMC_33 015734 consumes it) | L/S | [V] cell, [D] meaning | 014735 | 015014 |
| 0x70 | 070 | w | **SC2** scratch | L/S | [V] cell, [?] meaning | 014736 | 015015 |
| 0x74 | 072 | w | **OTE1** (Own Trap Enable 1) — DOMAIN reg; TRAPSET loads ALU/IDU/MIC trap-enable from DIT (+0x16/+0x26), NOT ctx | (DIT) | [DOC+V] | — | (DIT) 015073 |
| 0x78 | 074 | w | **OTE2** (Own Trap Enable 2) — DOMAIN reg, DIT-sourced | (DIT) | [DOC-manual] | — | — |
| 0x7C | 076 | w | **CTE1** (Child Trap Enable 1) — DOMAIN reg, DIT-sourced | (DIT) | [DOC-manual] | — | — |
| 0x80 | 100 | w | **CTE2** (Child Trap Enable 2) — DOMAIN reg, DIT-sourced | (DIT) | [DOC-manual] | — | — |
| 0x84 | 102 | w | **MTE1** (Mother Trap Enable 1) — DOMAIN reg, DIT-sourced | (DIT) | [DOC-manual] | — | — |
| 0x88 | 104 | w | **MTE2** (Mother Trap Enable 2) — DOMAIN reg, DIT-sourced | (DIT) | [DOC-manual] | — | — |
| 0x8C | 106 | w | **TEM1** (Trap Enable Modification mask 1) — DOMAIN reg, DIT-sourced | (DIT) | [DOC-manual] | — | — |
| 0x90 | 110 | w | **TEM2** (Trap Enable Modification mask 2) — DOMAIN reg, DIT-sourced | (DIT) | [DOC-manual] | — | — |
| 0x94 | 112 | w | trap park: SC10 (in TRAP_PGFENA path = saved P copy, 014003) | T | [V] cells, [D] meaning | 013237 | 013266 (TRAP_LOAD) |
| 0x98 | 114 | w | **local-trap-handler enable word**: read+tested for zero by TRAP_FIND (013155-013160: 0 ⇒ no context ⇒ flag route) and TRAP_PGFENA (013777-014001); written by TRAP_SAVE (SC7) and cleared to 0 by MSG_UNIX5RE (016044-45) | T | [V] read/test, [D] meaning | 013241 | 013156 / 014000 |
| 0x9C | 116 | w | trap park: SC12 (TRAP_PGFENA writes its enable copy here, 014007) | T | [V] | 014007 | 013270 |
| 0xA0 | 120 | w | trap park: SC11 | T | [V] | 013240 | 013267 |
| 0xA4-0xB4 | 122-132 | 5×w | **SRF trap record** (SRF area RFA1=0o40, read descending RF1D): {SC14,SC13,SC12,SC11,SC10} = {SC14,SC13,SC5\|SC7,SC6,SC4} record of TRAP_TO_SRF | T | [V] transfers | 013250-013254 | 013275-013301 |
| 0xB8 | 134 | hw | ASTBAD cell, low hw | T | [V] | 013260 | 013303 |
| 0xBA | 135 | hw | ASTBAD cell, high hw | T | [V] | 013257 | 013302 |

**Offset-frame note (differs from the prompt's anchors):** the trap routines address the
block through **EA2 := ctx+0x40** (TRAP_FIND 013154, TRAP_LOAD 013264, TRAP_PGFENA 013776:
`EA2SAVE AA=2 AB=1 IX*2` ⇒ MARG=0x40 [V by the §3.10 model — the same AB=1/EAnSAVE
combination is offset-verified at MSG_CONMC_33 015740, which lands exactly on the proven
msg+0o40 5PPA1 slot]). The pseudocode doc's anchors "ctx+0x54 … +0x74" are EA2-relative;
in true ctx-relative bytes they are **0x94 … 0xB4** — precisely the trap-park rows above.
TRAP_GEN1/3 copy these cells into the stop message (record ↔ message mapping unchanged from
the §3.10 table).

Memory space: all save/load/trap accesses are `RD,POF`/`WR,POF` = "read/write **physical
with MMS**" [V mnemonic; MICROCODE-FIELDS.md lines 1129/1136]. The DIT accesses in TRAPSET
use `RD,PHYS` ("physical segment") instead [V]. Interpretation of the POF/PHYS distinction: [?].

### 2b. Manual Appendix A reconciliation (2026-07-18) — the "untouched" slots are DOMAIN registers

Cross-checked against the authoritative **ND-5000 HW Maintenance manual §A.1 "Context block
(Register block)"** (`ND-05.017.01`, analysis in `ND5000-HW-MAINTENANCE-MANUAL-ANALYSIS-2026-07-18.md`).
The manual gives the FULL register-block layout (byte offset = context-disp × 4, verified
against our anchors: CED disp 27B→0x5C ✓, CAD disp 30B→0x60 ✓). It reconciles CLEANLY with
this microcode decode and RESOLVES old unknown #5:

- The manual's rule (§A.1): "registers … not saved in or loaded from the context block when
  changing to a new process … are loaded from the **domain information table (DIT)** before
  execution." (Ignore the OCR's uniform parentheses; the MICROCODE is authority for *which*.)
- The exact slots this microcode decode found NEWCNTXT NEVER touches — `0x4C–0x58`,
  `0x64–0x68`, `0x74–0x90` — are precisely the **domain registers**: TOS, LL, HL, THA, CES,
  CAS, OTE1/2, CTE1/2, MTE1/2, TEM1/2. That is exactly why NEWCNTXT skips them: their live
  values come from the **DIT** (TRAPSET already loads LL/HL from DIT+0x40/+0x44 and the trap
  enables from DIT+0x16/+0x26 — §3 step 8). NOT reserved/mystery — DOMAIN state, DIT-sourced.
- **Independent confirmation from the manual's SRF map (§A.2):** `SRF10` = "Status 2
  surrogate" → confirms 0x44 = ST2; `SRF11` = "Current/previous process + 1" → confirms the
  index (msg.HW[4]+1); `SRF13` = "PS register" → confirms 0x48 = PS; `SRF14`/`SRF15` = CED/CAD
  → confirms 0x5C/0x60. The manual independently validates this decode's SRF cell IDs.
- **Names for the loaded-from-ctx status slots:** 0x40 = **ST1** (Status register 1), 0x44 =
  **ST2** (Status register 2, via SRF10), 0x48 = **PS** (Process Segment register, via SRF13)
  — these ARE context-loaded [V microcode], so the emulator SHOULD model them (§5).
- **Trap-save record names (§A.1 "Information Saved at Trap" + §A.2 SRF36-42):** the microcode
  "SRF trap record {SC14..SC10}" at ctx+0xA4-0xB4 corresponds to the MMS registers +
  restart/trapping P — `SRF36`=MMS.PHYS, `SRF37`=MMS.LA, `SRF40`=MMS.STS, `SRF41`=Restart P,
  `SRF42`=Trapping P [DOC-manual]. Authority for AnswerTrapStop / TRAP_GEN record mapping.

---

## 3. NEWCNTXT (`014660`) — the exact sequence

```
| `014660` | NEWCNTXT  | ALU,A A,SRF11 B,X1 D,SC12 T,JMP ... ADDR=014661 |
| `014661` |           | ALU,A-B CRY,ONE TYP,HW A,SRF17 B,SC12 C,SEQ T,JMP COND,MSGN ... [ADDR=NEWCNTXT1] |
| `014662` |           | ALU,A TYP,HW A,SC12 B,X1 D,SC12 C,SEQ T,JMP COND,MZRO ... [ADDR=NEWCNTXT2] |
| `014663` |           | ALU,FZRO A,BM00 B,X1 T,JMP, T,PUSH ... [ADDR=CNTXTSAVE] |
| `014664` | NEWCNTXT1 | ALU,XOR TYP,HW A,SRF17 B,SC14 D,SC12 T,JMP, T,PUSH ... [ADDR=CNTXTLOAD] |
| `014665` | NEWCNTXT2 | ALU,XOR TYP,HW A,SRF17 B,SC14 D,SRF11 T,JMP ... [ADDR=DUMMY_2] |
```

Pseudocode (condition at word N tests the ALU result of word N−1 — MICROCODE-FIELDS.md
line 860):

```c
void NEWCNTXT(void) {                     // SRF17 = new index (msg.HW[4]+1), SRF11 = current
    SC12 = srf[SRF11];                    // 014660
    if ((int32)srf[SRF11] >= 0) {        // 014661 tests MSGN of 014660's ALU (=SRF11):
                                          //   negative (BM37 flag) => no context loaded => skip save
        if ((uint16)SRF17 == (uint16)SC12)// 014662 tests MZRO of 014661's SRF17-SRF11 compare
            { srf[SRF11] = (uint16)SRF17; return; }   // NEWCNTXT2: same process — nothing to do
        CNTXTSAVE();                      // 014663: save outgoing process's context
    }
    SC12 = (uint16)SRF17;                 // NEWCNTXT1
    CNTXTLOAD();                          // full load of the new process
    srf[SRF11] = (uint16)SRF17;           // NEWCNTXT2: current := new; return
}
```
All control decisions [V]; the MSGN/MZRO "previous-result" pairing [D] (matches the
CNTXTSAVE call-site idiom `COND,MSGN INVSEQ` used everywhere, e.g. 015661, 016006).

### CNTXTLOAD (`014742`) = address preamble + CNTXTLOAD0 (`014750`)

Order of operations after `DPA := 0o4000 + 256*idx` (014747):

1. **P** := [ctx+0x00], **L** := [ctx+0x04], **B** := [ctx+0x08], **R** := [ctx+0x0C]
   (014750-014760).
2. **X1-X4, A1-A4, E1-E4** := [ctx+0x10 … 0x3C] (014761-014775).
3. Status composite [ctx+0x40] → **MIC,STS / IDU,STS / ALU flags** (WRITEST1);
   [ctx+0x44] → **SRF10** (+ALU,STS bits, WRITEST2); [ctx+0x48] → **SRF13** and low hw →
   **MM,PS, MM,PHS** (NEW_PS_1) (014776-015004).
4. `SPEC,MOD := SC12` (015005) — SC12 still holds the scaled ctx offset here; intent [?]
   (MOD is properly rebuilt by TRAPSET at 015107-015113 afterwards).
5. CED: [ctx+0x5C] byte → **SRF14 + MM,DOM**; CAD: [ctx+0x60] byte → **SRF15 + MM,ADOM**
   (015006-015012).
6. **SC1** := [ctx+0x6C], **SC2** := [ctx+0x70] (015013-015015).
7. Jump **TRAPSET** (015016).
8. TRAPSET (015057): `CED_TO_DIT` ⇒ DPA := DIT(SRF14) = 0x80 + CED*256 (physical); then
   from the **DIT** (all `RD,PHYS`): [DIT+0x40] → **IDU,LL**, [DIT+0x44] → **IDU,HL**
   (lower/upper limits), [DIT+0x3C] → **SRF12** (015061-015066); limit-compare sets an
   **IDU,LIMC** bit (015067-071). TRAPSET3 (015072): [DIT+0x16] and [DIT+0x26] OR-combined
   → **ALU trap-enable (`TE,ALU,LOAD`)**, then the same value → **IDU,TE** and **MIC,TE**
   (015073-015104); byte [DIT+0x3B] and byte [DIT+0x48] gate MIC,STS bits (015074-015102);
   finally MOD-register bits rebuilt from the ADR_MOD/ADR_MODINIT SRF cells (015105-015114).
   [V transfers; DIT-relative because CED_TO_DIT re-loads DPA — the DIT cell semantics
   beyond LL/HL/TE are [D]].

After NEWCNTXT, MSG_START (`015674-015675`) jumps **EXECUTE** (`014636`): clears AAP1/AAP2
and IXC, arms traps (TRAP_ARM1), sets the run flag (SET_RUNNING), merges SRF10 bits into
IDU,STS, `LOADLA` from **IAC,P** and `G,TOOPS`/`G,OOPS` — macro execution resumes at the P
loaded from ctx+0x00 [V flow].

So for **3START (MSG_START @015671)**: `CPU_AVAIL?` (srf cell 0o2016) → NEWCNTXT (save old
if any, load all of the above for process msg.HW[4]+1) → EXECUTE at ctx.P.

### CNTXTSAVE (`014666`) — the mirror

Preamble ×256 + OFFSET + `DPA := ctx` (014666-014673); CNTXTSAV00 (014674) captures
SC5:=B, SC6:=R, SC3:=P, SC4:=L (all XOR SC14, SC14=0 in these flows ⇒ identity [V]);
CNTXTSAVE1 (014701) `EA3 := DPA`; then the WR sequence of §2's Save-μ column; then
READST1/READST2 collect the status words; then SRF13/SRF14/SRF15/SC1/SC2 stores; finally
`SRF11 |= BM37` (014737-014741) — marks "context saved / none loaded" [V].

---

## 4. NEWCNTXT (mailbox path) vs CNTXTLOAD0 / the UNIX5 family

- **NEWCNTXT** = compare-indices + CNTXTSAVE + **CNTXTLOAD** (which is CNTXTLOAD0 plus the
  `ctx = 0o4000 + 256*idx` preamble). Used by MSG_START/MSG_CONMC/MSG_CONWR/MSG_DMEMRD/WR/
  MSG_IMEMRD/WR/MSG_UNIX5RE/MSG_UNIX5CM [V callers].
- **CNTXTLOAD0** (`014750`) is the same loader entered with **DPA already set by the
  caller** — MSG_UNIX5REL (016067-016075) points DPA at a context image whose **address is
  read from the message** (msg+0x10 word → DPA, 016073-016074) instead of the 0o4000 table
  [V]. Same field map.
- **CNTXTSAV00** (`014674`) likewise: MSG_UNIX5RE saves the current context **into a
  message-supplied pointer** (msg+0x10 → DPA at 016021, then CNTXTSAV00 at 016022), copies
  the five trap-park words ctx+0x94..0xA4 into the same external image (016030-016043),
  zeroes the local-enable word ctx+0x98 (016044-45), then installs **new CED := msg hw@0x18**
  (NEW_CED), **CAD** (same value, or SRF15 if msg hw@0x18 tests zero — 016050-016054),
  **P := msg word@0x14 via WRITE_P** (016055), rebuilds status/trap enables
  (READST1/WRITEST1/TRAPSET), and continues [V flow, offsets per model].
- **WRITE_P** (`012674`): `ALU,A A,SC11 B,X1 D,IAC,P` — P := SC11, one word [V].

Net difference: the mailbox path (NEWCNTXT) always uses the **fixed per-process table at
0o4000 with stride 0o400**, keyed by msg.HW[4]+1; the UNIX5 family uses the same
save/load engines against **caller-supplied context images** and takes P/CED/CAD from the
message instead of the block.

---

## 5. Emulator mapping (CpuND500)

| ctx offset | CpuND500 target | Confidence |
|---|---|---|
| 0x00 | P (program counter) | [V] |
| 0x04 | L (link register) | [V] |
| 0x08 | B (base register) | [V] save-side; load dest [D] |
| 0x0C | R (record register) | [D] |
| 0x10-0x1C | I1-I4 (microcode X1-X4; X=I already proven) | [V] |
| 0x20-0x2C | A1-A4 (address registers, if CpuND500 models them as A1-A4) | [D — name match only] |
| 0x30-0x3C | E1-E4 | [?] which architectural registers E1-E4 are |
| 0x40 | **ST1** (Status register 1) — composite of ALU/MIC/IDU status per the three masks in §2; the ND-500 PSW/status image. CONTEXT-LOADED → emulator SHOULD model | [V]+[DOC name] |
| 0x44 | **ST2** (Status register 2, via SRF10 = "Status 2 surrogate" per manual A.2) — trap/pending status. CONTEXT-LOADED | [V]+[DOC name] |
| 0x48 | **PS** (Process Segment register, via SRF13) — low hw → MMU PS + PHS. CONTEXT-LOADED | [V] dest, [DOC] name |
| 0x4C-0x58, 0x64-0x68, 0x74-0x90 | DOMAIN registers (TOS/LL/HL/THA/CES/CAS/OTE/CTE/MTE/TEM) — NOT context-loaded; DIT-sourced. Emulator loads these via the DIT path (step 5), NOT the ctx block — current StartProcessFromContextBlock correctly omits them | [V] (ctx omits) / [DOC] names |
| 0x5C | CED — current executing domain (byte) → MMU DOM | [V] |
| 0x60 | CAD — current alternative domain (byte) → MMU ADOM | [V] |
| 0x6C | SC1 scratch (observed: MON write-back mask) | [?] |
| 0x70 | SC2 scratch | [?] |
| 0x94-0xBA | trap park/record area (only needed if emulating microcode-level traps) | [D] |
| 0x98 | local trap-handler enable word (0 = no local handler ⇒ stop to ND-100) | [V] test semantics |

Process start (3START = MSG_START), emulator recipe:
1. Check CPU-available cell (srf 0o2016 equivalent); if unavailable → OCB 0o203 path.
2. idx := message HW@4 (X5CPU) + 1; ctx := 0o4000 + 0o400*idx (ND-500 physical bytes).
3. If a process is loaded and different: write its state back per §2 (save column).
4. Load P,L,B,R, I1-I4, A1-A4, E1-E4, status, PS→MMU, CED/CAD→MMU, per §2.
5. Trap enables from DIT(CED) = 0x80 + 0o400*CED (if trap emulation is wanted).
6. Resume macro execution at P.

---

## 6. UNKNOWNS (explicit)

1. **EXUC-self double execution** — the ×256 stride for GET_CNTXT/NEW_TO_DIT is [D]:
   forced by consistency with the inline ×256 preambles and the ≥0xBC block span, but the
   sequencer-level mechanism (each `EXUC ADDR=self` word executing twice in the call
   shadow) is inferred, not documented. A single runtime trace of DPA after 013153 would
   close it.
2. `D,DAC,REG04` / `D,DAC,LDRES` exact register decode (taken as B and R by save/load
   symmetry).
3. `SPEC,MOD := SC12` at 015005 (SC12 = scaled ctx offset at that point) — intent unknown.
4. Meaning of SC2 (ctx+0x70) and the upper halfword of ctx+0x48.
5. ~~Untouched ranges 0x4C-0x58, 0x64-0x68, 0x74-0x90~~ **RESOLVED 2026-07-18 (§2b):** these
   are the DOMAIN registers (TOS/LL/HL/THA/CES/CAS/OTE1-2/CTE1-2/MTE1-2/TEM1-2), DIT-sourced —
   NEWCNTXT correctly ignores the ctx slots because the live values come from the DIT (manual
   §A.1). LL/HL/trap-enables loaded from DIT by TRAPSET (§3 step 8), consistent.
6. POF vs PHYS memory-space semantics ("physical with MMS" vs "physical segment").
7. DIT layout beyond LL(+0x40)/HL(+0x44)/SRF12(+0x3C)/TE cells(+0x16,+0x26,+0x3B,+0x48);
   DIT base constant BM07=0x80 and stride share unknown #1's caveat.
8. Whether the SINTRAN side ever creates context blocks anywhere other than the 0o4000
   table (the UNIX5 message-pointer path proves the engines are position-independent).
