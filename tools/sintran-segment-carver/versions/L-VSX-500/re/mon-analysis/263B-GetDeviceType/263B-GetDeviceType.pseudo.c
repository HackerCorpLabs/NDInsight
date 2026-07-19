/* ============================================================================
 * MON 263B  GetDeviceType (GDEVT)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Returns the device type of a logical device (terminal, floppy, mass-storage
 * file, ...) plus a device-attribute bitmask describing how to handle it.
 *
 * Derived from the real disassembly (see 263B-GetDeviceType.ASM), the GDEVT
 * worker at 107104B in segment 025-S3IRPIT. The control flow (descriptor fetch,
 * the equality test, the two-way attribute selection) is BYTE-VERIFIED. The
 * register/field meanings are INFERRED from the SINTRAN III Monitor Calls manual
 * MAC example and the code shape - treat as a model, not gospel. Several branches
 * exit into the ADJACENT X21CL routine (shared tail); those are modelled as
 * returns/exits, not carved. Addresses in comments are octal.
 *
 * Every instruction below is grounded in the canonical ND-100 instruction
 * semantics reference (../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md):
 *   MST PIE = masked-set PIE from A;  LDA I n = A = mem[mem[P+n]];  SAT n = T := signext8(n);
 *   SKP IF DA EQL ST = if (A==T) skip;  JAZ n = if (A==0) jump;  JAF n = if (A!=0) jump;
 *   LDA ,X n / LDX ,X n = X-indexed load;  JPL I = indirect call via link cell.
 * ============================================================================ */

/* Manual register contract (from the MAC example, INFERRED - not byte-proven):
 *   MON 263 GetDeviceType - MAC:  LDT DEVNO / LDA IOF / MON 263 / JMP ERROR
 *   in:   T = DeviceNo (logical device number; 1 = own terminal, appendix B)
 *         A = IOFlag   (0 = input part, 1 = output part)
 *   out:  T = DevType  (0 unspecified,1 terminal,2 TAD,3 comm-channel,
 *                       4 internal block,5 floppy,6 mag-tape,7 mass-storage file)
 *         A:D = DevAttr (32-bit attribute bitmask; bit0 InByte/OutByte allowed,
 *                        bit1 StartOnInterrupt, bit2 DeviceControl, bit3 Block calls,
 *                        bit4 ClearDevice, bit5 Reservation-not-needed, bit6 COSMOS
 *                        remote open, bit10g NOTS terminal, bit11g MTAD device)
 *   Error return: A = error number (K flag set on error). */

int mon_263B_GetDeviceType(mon_regs *r)
{
    resident_worker_at_link_107265();      /* 107104: JPL I 161 -> link @107265       */
    r->A = 4;                              /* 107105: SAA 4                            */
    set_interrupt_enable(r->A);            /* 107106: MST PIE - PIE |= (A & mask)      */

    r->A = device_descriptor();            /* 107107: LDA I 157 - device descriptor    */
    if (r->A == 0)                         /* 107110: JAZ 21 -> 107131 (X21CL tail)    */
        goto shared_tail;                  /*   exit into adjacent X21CL routine        */

    r->A = r->B[011];                      /* 107111: LDA ,B 11 - device flag field    */
    r->T = 1;                              /* 107112: SAT 1                            */
    if (r->A == r->T)                      /* 107113: SKP IF DA EQL ST                 */
        ;                                  /*   present/matched -> fall through         */
    else
        goto shared_tail;                  /* 107114: JMP 15 -> 107131 (X21CL tail)    */

    r->A = descriptor_word_m13();          /* 107115: LDA I -13                        */
    if (r->A != 0) {                       /* 107116: JAF 3 -> 107121                  */
        r->X = descriptor_word_m16();      /* 107121: LDX I -16                        */
        r->A = r->B[012];                  /* 107122: LDA ,B 12                        */
        if (r->A != 0)                     /* 107123: JAF 3 -> 107126                  */
            { r->X = mem[r->X + 012];      /* 107126: LDX ,X 12 - follow chain         */
              r->A = mem[r->X + 023]; }    /* 107127: LDA ,X 23 - device-type attr     */
        else
            r->A = mem[r->X + 026];        /* 107124: LDA ,X 26 - device-type attr     */
    } else {
        r->A = descriptor_word_m16();      /* 107117: LDA I -16                        */
    }                                      /* 107120/107125: JMP -> 107130             */

    /* 107130: JMP 2 -> 107132 : store DevType/DevAttr and return through the shared
     * X21CL tail (adjacent routine, NOT carved here). Modelled as the normal exit. */
shared_tail:
    return device_type_store_tail(r);      /* shared level-14 tail (X21CL, uncarved)   */
}

/* Byte-verified anchors:
 *   GDEVT entry 107104 (025-S3IRPIT), MST PIE 107106, descriptor fetch LDA I 157
 *   (107107), equality test SKP IF DA EQL ST (107113), attribute selects LDA ,X 26
 *   / LDA ,X 23 (107124/107127).
 * NOT proven: the fall-through MON 263 -> GDEVT bridge (uncarved MFELL/CALLPROC);
 *   the DevType/DevAttr store, which happens in the shared X21CL tail past the
 *   window (uncarved); the semantic label of each descriptor field (INFERRED from
 *   the manual). Branches to 107131/107132 leave into the adjacent X21CL routine. */
