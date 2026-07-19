/* ============================================================================
 * MON 155B  GraphicFunction (GRAPH)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Graphic dispatcher cluster 025-S3IRPIT
 * @122434B..122557B ; GOTAB entry F1675 = 122461B.
 *
 * Derived from the real disassembly (see 155B-GraphicFunction.ASM). The Func
 * validation, the Code computed-jump (LDA ,B -2 / RADD SA DP) into an 8-entry
 * table, and each arm's IRW/IRR graphic-device-register access are BYTE-VERIFIED.
 * The mapping of Func 0/1/2 to PLOT/PLOTS/NEWP and the meaning of the Code sub-
 * operations come from the GraphicFunction manual (ND-860228.2 EN) - INFERRED.
 *
 * NOTE ON DISPATCH: GOTAB[155] = 122461B (label F1675) is inside the graphic
 * dispatcher cluster F1674..F1677. Entry re-joins the setup path (JMP -13 ->
 * 122446), then the Code parameter selects a graphic operation. The graphic
 * device workers are reached through JPL-I pointer words [122616]/[122620]/
 * [122621] whose callees are past this carve (uncarved graphic driver).
 * Register role B = per-call graphic datafield (inferred). Addresses octal.
 * ============================================================================ */

/* 122434-122557: graphic-function dispatcher (GOTAB enters at F1675=122461). */
int mon_graphic_function(mon_regs *r, graf_field *B)
{
    int func, code, val;

    /* 122442-122460: validate the Func code. Func 1 (PLOTS) takes the setup
     * path via [122620]; other values fall to the Code dispatch. */
    func = B->func;                    /* 122442 LDA ,B 4  Func code             */
    if (func == 1) {                   /* 122443-122445 SAT 1 ; SKP IF DA UEQ ST */
        /* GOTAB entry F1675 (122461) also lands here via JMP -13 -> 122446. */
        graphic_setup(mem[0122620], B);/* 122446 JPL I 152 -> [122620]           */
        return graphic_common_tail(B); /* 122447 JMP -> 122563                   */
    }

    /* 122462-122473: Code parameter selects the graphic operation via a
     * computed jump (P = P + Code) into an 8-entry JMP table. */
    code = B->code;                    /* 122462 LDA ,B -2  Code parameter        */
    switch (code) {                    /* 122463 RADD SA DP ; 122464-122473 table */
        case 0: graphic_worker(mem[0122621], B);           /* 122474 JPL I 125   */
                break;
        case 1: val = graphic_worker(mem[0122621], B);     /* 122502 JPL I 117   */
                B->tmp = val;                              /* 122504 STA ,B -7   */
                iow_graphic_reg(REG_A, val << 8);          /* 122505 IRR/IRW A   */
                break;
        case 2: graphic_worker(mem[0122621], B);           /* 122511 JPL I 110   */
                iow_graphic_reg(REG_D, B->tmp << 8);       /* 122515 IRW 10 DD   */
                break;
        /* codes 3..7: same pattern against device registers L, X (JPL-I worker,
         * then SHA ZIN 8 / IRW or IRR|ORA / IRW to the graphic device register).
         * The concrete graphic effect of each Code is INFERRED (manual). */
        default: graphic_worker(mem[0122621], B); break;
    }
    B->code = 0;                       /* 122557 STZ ,B -2  common tail merge     */
    return graphic_common_tail(B);
}

/* Caller (from the manual, ND-860228.2 EN):
 *   MON 155B GraphicFunction: parameter list (Ycoor, Xcoor, Code, DeviceNo, Func,
 *   ReturnValue). Func 0 = PLOT, 1 = PLOTS (establish reference / clear NORDCOM
 *   screen), 2 = NEWP (select pen/screen). Executes functions on a graphic
 *   peripheral (NORDCOM terminal, pen plotter, Tektronix display). The
 *   graphic-device I/O uses IRW/IRR to device registers; the leaf workers behind
 *   the JPL-I pointer words are in the uncarved graphic driver.
 */
