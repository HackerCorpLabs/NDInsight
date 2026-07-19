/* ============================================================================
 * MON 336B  Terminal (IOMTY)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * The I/O multifunction monitor call: changes the attributes of terminal and
 * terminal-access-device (TAD) I/O, and configures NET/One interfaces and SCSI
 * disks. It takes a varying number of input/output parameters depending on the
 * function code; all parameters are passed in an array.
 *
 * Derived from the real disassembly (see 336B-Terminal.ASM), the IOMTY worker at
 * 51745B in resident SINTRAN-DATA_commoncode. IOMTY is a SYMBOL-1-LIST symbol.
 * Control flow (the alternating resident-worker calls and parameter-descriptor
 * loads) is BYTE-VERIFIED. The register/field meanings (which B-frame word is the
 * function code, the array length, each parameter) are INFERRED from the SINTRAN
 * III Monitor Calls manual MAC example and the code shape - treat as a model, not
 * gospel. Addresses in comments are octal.
 *
 * Dispatch reality:
 *   GOTAB[336B] = 000000 -> FALL-THROUGH (no per-call stub). Dispatch drops into
 *   the resident MFELL/CALLPROC second-level path (uncarved) which reaches IOMTY.
 *   So the MON 336 -> IOMTY link is NOT byte-followable statically; identity
 *   rests on the symbol NAME (IOMTY = terminal I/O multifunction) - see README.
 *
 * Every instruction below is grounded in the canonical ND-100 instruction
 * semantics reference (../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md):
 *   LDD ,B disp = A = mem[B+disp]; D = mem[B+disp+1]  (load double, A first);
 *   JPL I disp  = L = return addr; PC = mem[P+disp]  (indirect call via link cell).
 * ============================================================================ */

/* Manual register contract (from the MAC example, INFERRED - not byte-proven):
 *   MON 336 Terminal - MAC:
 *     LDT NTERM / SAA 0 / COPY SA DL / LDA 0 / MON 336 / STA ERROR
 *   T = logical device number of the terminal
 *   L = function code (0 in the example)
 *   A = translation flag (0 = no translation to uppercase)
 *   The FunctionCode, ArrayLength and ParameterArray (per the .yaml) select which
 *   of the many I/O sub-functions runs; error number is returned in A. */

int mon_336B_Terminal(mon_regs *r)             /* in: FunctionCode, ArrayLength, ParameterArray */
{
    /* 51745-51762: an alternating sequence of resident-worker calls (JPL I) and
     * parameter-descriptor loads (LDD ,B ...). Each JPL I dispatches into the
     * resident I/O multifunction machinery through a link cell (052021/052023,
     * past the carved window); each LDD fetches a double-word descriptor from the
     * caller's B-frame parameter array. The concrete sub-function selected by the
     * function code lives past the uncarved CALLPROC.                            */

    resident_io_worker_052021();               /* 51745: JPL I 54 -> [052021]     */
    load_param_double(r, r->B - 0115);         /* 51746: LDD ,B -115              */
    resident_io_worker_052021();               /* 51747: JPL I 52 -> [052021]     */
    load_param_double(r, r->B - 0125);         /* 51750: LDD ,B -125              */
    resident_io_worker_052023();               /* 51751: JPL I 52 -> [052023]     */
    load_param_double(r, r->B - 0133);         /* 51752: LDD ,B -133              */
    resident_io_worker_052023();               /* 51753: JPL I 50 -> [052023]     */
    load_param_double(r, r->B - 0103);         /* 51754: LDD ,B -103              */
    resident_io_worker_052023();               /* 51755: JPL I 46 -> [052023]     */
    load_param_double(r, r->B - 0143);         /* 51756: LDD ,B -143              */
    resident_io_worker_052023();               /* 51757: JPL I 44 -> [052023]     */
    load_param_double(r, r->B - 0117);         /* 51760: LDD ,B -117              */
    resident_io_worker_052023();               /* 51761: JPL I 42 -> [052023]     */
    load_param_double(r, r->B - 0127);         /* 51762: LDD ,B -127              */

    return 0;                                  /* + standard error code in A       */
}

/* Byte-verified anchors:
 *   IOMTY entry 51745 (resident commoncode), and the seven JPL I resident-worker
 *   calls interleaved with seven LDD ,B parameter-descriptor loads, bounded by the
 *   next symbol MBFDI=51763B (14 words).
 * NOT proven: the fall-through MON 336 -> IOMTY bridge (uncarved MFELL/CALLPROC);
 *   the semantic label of each B-frame descriptor and which I/O sub-function the
 *   function code selects (INFERRED from the manual); the JPL I link cells
 *   (052021/052023) lie past the carved window and their targets are not resolved
 *   here. This window is the named dispatch entry into a larger resident I/O
 *   module, not a fully self-contained subroutine. */
