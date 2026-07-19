/* ============================================================================
 * MON 152B  GetRTName (GRTNA)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Gets the name of an RT program (7 bytes) given the RT-description address (0 = the
 * calling program). The name is returned with a terminating apostrophe if shorter than
 * 7 characters. Background programs (SINTRAN III VSX).
 *
 * Derived from the real disassembly (see 152B-GetRTName.ASM). GRTNA=041745B is a
 * SYMBOL-1-LIST symbol in resident commoncode, but it is an INTERIOR label (aliasing
 * the cell WMSBA) inside a larger resident routine, NOT a routine entry. The bytes are
 * real code; the excerpt models only the two-word name copy visible in the window - the
 * routine's prologue, return and register contract are outside the carved excerpt.
 * Control flow of the copy is BYTE-VERIFIED; the RT-name field meaning is INFERRED from
 * the manual. Addresses are octal.
 *
 * Dispatch reality:
 *   GOTAB[152B] = 000000 -> FALL-THROUGH (no per-call stub). Dispatch drops into the
 *   resident MFELL/CALLPROC second-level path (uncarved) which reaches GRTNA. So the
 *   MON 152 -> GRTNA link is NOT byte-followable statically; identity rests on the
 *   symbol NAME (GRTNA = Get RT NAme) and GRTNA is an interior label - see README.
 *
 * Every instruction below is grounded in the canonical ND-100 instruction semantics
 * reference (../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md):
 *   SKP IF DD LST 0 = skip if (signed) D < 0;  LDX ,B n = mem[B+n];  LDA/LDT ,X n =
 *   mem[X+n];  STA/STT ,B n = store to frame;  RADD CLD 0 DA = A := 0;  JMP d = branch.
 * ============================================================================ */

/* Manual register contract (from the MON 152 description, INFERRED - not byte-proven):
 *   GetRTName(RTProgram, RTProgramName):
 *     RTProgram     : address of the RT description (0 = calling program)
 *     RTProgramName : 7-byte name returned (apostrophe-terminated if short). */

/* Interior excerpt: the two-word RT-name copy at and around GRTNA (041730-041751). */
void grtna_name_copy(mon_regs *r)
{
    word *pb = (word *)r->B[-2];                    /* 041732/041736/041743: LDX ,B -2   */
    if ((int16)r->D < 0) {                           /* 041730: SKP IF DD LST 0           */
        r->A = pb[1];                               /* 041733: LDA ,X 1                 */
        r->T = pb[0];                               /* 041734: LDT ,X 0                 */
    } else {
        r->A = pb[0];                               /* 041737: LDA ,X 0                 */
        r->T = pb[1];                               /* 041740: LDT ,X 1                 */
        r->B[0162] = r->T;                          /* 041741: STT ,B 162 (first word)   */
        r->A = pb[0];                               /* 041744: LDA ,X 0                 */
    }
    r->B[0162] = r->A;                              /* 041745 GRTNA: STA ,B 162          */
    r->A = pb[1];                                   /* 041746: LDA ,X 1                 */
    r->B[0163] = r->A;                              /* 041747: STA ,B 163 (second word)  */
    r->A = 0;                                        /* 041750: RADD CLD 0 DA            */
    r->B[-033] = 0;                                 /* 041751: STA ,B -33               */
}

/* Byte-verified anchors:
 *   the name-order selector (SKP IF DD LST 0 at 041730), the two-word copy out of the
 *   parameter block [B-2] into B+162/B+163 (041732-041747), and the A:=0 / STA ,B -33
 *   at 041750-041751. GRTNA (041745) is the store point, an interior label.
 * NOT proven: the fall-through MON 152 -> GRTNA bridge (uncarved MFELL/CALLPROC); the
 *   enclosing routine's prologue/return and the full register contract (outside the
 *   excerpt); the RT-name field layout (INFERRED from the manual). */
