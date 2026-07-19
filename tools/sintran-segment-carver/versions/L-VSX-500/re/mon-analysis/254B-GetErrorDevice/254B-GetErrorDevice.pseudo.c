/* ============================================================================
 * MON 254B  GetErrorDevice (GERDV)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Returns the logical device number of the error device (the terminal that
 * outputs system errors and RT program messages, normally the console). If the
 * error device is reserved by an RT program, the RT description address is also
 * returned (0 = unreserved).
 *
 * Dispatch (CORRECTED 2026-07-13): MON calls dispatch through MCTAB @ 005620B
 * (segment 044-S3IDPIT, load 4000B), NOT the fictional commoncode "GOTAB". The
 * old model (GOTAB[254]=066246B -> F1734 stub -> uncarved CALLPROC, worker
 * attached "by name only") was an artefact of the wrong table. MCTAB[254B]
 * @006074B = 102525B = GERDV, proven directly from the table slot:
 *   MON 254B -> ENT14 072167B -> GOTAB[254B]=MFELL 072114B -> CALLP 032201B ->
 *   MCTAB[254B]=102525B=GERDV @102525B (025-S3IRPIT, bounded by HIL14=102555B).
 *
 * The GERDV worker body is BYTE-VERIFIED (see 254B-GetErrorDevice.ASM); the
 * parameter mapping (which B-frame word is the ErrorDevice / RTProgram output)
 * is INFERRED from the manual. Addresses in comments are octal.
 *
 * Every instruction below is grounded in the canonical ND-100 instruction
 * semantics reference (../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md):
 *   RADD CLD Ss Dd = register copy;  MST PIE = masked-set PIE from A;
 *   SAA/SAT n = A/T := signext8(n);  SWAP SD DA = full D<->A exchange (emulator);
 *   LDA/LDX I disp = one-level indirect load;  LDA ,X disp = mem[X+disp];
 *   STZ ,X disp = mem[X+disp]=0;  JPL I = indirect call via link cell.
 * ============================================================================ */

/* Manual register contract (from the MAC example, INFERRED - not byte-proven):
 *   MON 254 GetErrorDevice - MAC:
 *     MON 254 / STA ERDEV / COPY SD DA / STA RTPRO
 *   A   = logical device number of the error device        (ErrorDevice, out)
 *   D   = RT description address of the reserving RT program (RTProgram, out;
 *         0 means the error device is not reserved). */

/* GERDV worker @102525B (025-S3IRPIT), reached directly via MCTAB[254B]. REAL
 * executable code: it reads the
 * error-device descriptor through an indirect pointer, calls shared resident
 * workers, and stores the logical device number and the RT description address
 * back into the caller's B-frame. Control flow is BYTE-VERIFIED; the field
 * semantics below are INFERRED from the manual.                                 */
int gerdv_get_error_device(mon_regs *r)
{
    resident_prologue_worker();                /* 102525: JPL I 32 -> [102557]     */
    r->A = 4;                                  /* 102526: SAA 4                    */
    set_interrupt_enable(r->A);                /* 102527: MST PIE - PIE |= (A&mask)*/

    word *desc = (word *)mem[ind(0102560)];    /* 102530: LDX I 30 -> descriptor ptr*/
    r->A = desc[015];                          /* 102531: LDA ,X 15                */
    r->D = r->A;                               /* 102532: RADD CLD SA DD (copy)    */
    desc[015] = 0;                             /* 102533: STZ ,X 15                */
    r->A = desc[021];                          /* 102534: LDA ,X 21                */
    r->B[021] = r->A;                          /* 102535: STA ,B 21                */
    desc[021] = 0;                             /* 102536: STZ ,X 21                */
    r->T = 5;                                  /* 102537: SAT 5                    */
    resident_worker_at_link_102561();          /* 102540: JPL I 21 -> [102561]     */
    r->A = mem[ind(0102524)];                  /* 102541: LDA I -15 (indirect)     */
    { word t = r->D; r->D = r->A; r->A = t; }  /* 102542: SWAP SD DA (full exchange)*/
    desc[015] = r->A;                          /* 102543: STA ,X 15                */
    resident_worker_at_link_102561();          /* 102544: JPL I 15 -> [102561]     */

    r->A = r->D;                               /* 102545: RADD CLD SD DA (copy)    */
    r->B[012] = r->A;                          /* 102546: STA ,B 12 - ErrorDevice  */
    resident_worker_at_link_102562();          /* 102547: JPL I 13 -> [102562]     */
    r->X = r->D;                               /* 102550: RADD CLD SD DX (copy)    */
    r->A = ((word *)r->X)[1];                  /* 102551: LDA ,X 1                 */
    r->B[013] = r->A;                          /* 102552: STA ,B 13 - RTProgram    */
    r->A = r->B[021];                          /* 102553: LDA ,B 21                */
    r->X = mem[ind(0102560)];                  /* 102554: LDX I 4 -> [102560]      */
    return 0;                                  /* + standard error code            */
}

/* Byte-verified anchors:
 *   MCTAB[254B] slot @006074B = 102525B = GERDV (044-S3IDPIT), GERDV entry 102525
 *   (025-S3IRPIT), MST PIE 102527, the indirect descriptor load LDX I 30 (102530),
 *   the two output stores STA ,B 12 (102546, ErrorDevice) and STA ,B 13 (102552,
 *   RTProgram), and the SWAP SD DA full exchange 102542.
 * NOT proven: the
 *   semantic label of each B-frame word (ErrorDevice / RTProgram assignment is
 *   INFERRED from the manual MAC example, not byte-isolated from this carve);
 *   the JPL I link cells (102557/102561/102562) lie past the carved window in
 *   the following HIL14 region and are not resolved here. */
