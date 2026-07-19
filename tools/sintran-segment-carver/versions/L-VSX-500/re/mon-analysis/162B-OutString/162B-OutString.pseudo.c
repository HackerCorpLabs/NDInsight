/* ============================================================================
 * MON 162B  OutString (OUTST)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Derived from the real disassembly (see 162B-OutString.ASM). Control flow and
 * the GETCH (fetch character) -> SOUTB (output byte) output loop are
 * BYTE-VERIFIED; the semantic labels (which field is device / string / count)
 * are INFERRED from the SINTRAN III Monitor Calls manual and the field copies
 * seen - treat as a model, not gospel. Addresses in comments are octal.
 * ============================================================================ */

/* Manual parameter list (from the MAC example, inferred - not byte-proven):
 *   A = address of a parameter list: { DeviceNo, TextWrite, NoOfBytes }
 *   DeviceNo   = logical device number (peripheral file; not mass storage)
 *   TextWrite  = character string to output (max 2048 bytes)
 *   NoOfBytes  = number of characters to write
 * Status is returned to the caller (STA ,B 2 slot). If the device output buffer
 * is full the call waits until buffer space becomes available. */

int mon_outstring(mon_regs *r)
{
    save_params(r);                        /* 41013: STD I 47 - stash caller A,D via ptr cell */
    resident_prologue();                   /* 41017: JPL I 44 -> 003752           */
    a = load_via_link(041064);             /* 41020: LDA I 44 - A = mem[mem[P+44]] */
    r->B[6] = a;                           /* 41030: STA ,B 6                     */
    d = 0;                                 /* 41031: RADD CLD 0 DD - D = 0 (output index) */

    /* --- output loop (41032-41057) ---------------------------------------- */
    for (;;) {
        t = r->B[1] - 1;                   /* 41032-41033: T = mem[B+1] - 1 (count - 1) */
        /* 41034 SKP IF DT GRE SD: skip (continue) while (int16)T >= (int16)D;
         * 41035 JMP -> 41060 taken when T < D, i.e. index D has reached count. */
        if (!((short)t >= (short)d)) break;

        ch = GETCH();                      /* 41037: JPL I 26 -> GETCH 030062     */
        /* 41040 SAT 47; 41041 SKP IF DA UEQ ST: skip JMP while A != 47(octal);
         * so when A == 47 (terminator) the JMP to 41060 is taken.              */
        if (ch == 047)          break;     /* 41040-41042: terminator -> 41060     */

        /* 41043 SAT 44; 41044 SKP IF DA EQL ST: skip 41045 JMP when A == 44(octal). */
        if (ch == 044) {                   /* 41043-41045: special char path       */
            SOUTB(ch);                     /* 41046-41050: LDT ,B 6 / JPL I 17 -> SOUTB 031030 */
            SOUTB(012);                    /* 41051-41052: SAA 12 (A=012) then JPL I 15 -> SOUTB */
        } else {
            SOUTB(ch);                     /* 41054-41055: LDT ,B 6 / JPL I 12 -> SOUTB 031030 */
        }
        d = d + 1;                         /* 41056: RADD AD1 0 DD - RINC D (index++) */
        /* 41057 JMP -25 -> 41032 (loop back to next character) */
    }

    unwind_frame(7);                       /* 41060: SAA -7 (matches SAB 7)       */
    resident_return();                     /* 41061: JMP I 7 -> 003776             */
    return r->status;                      /* status returned to caller (B+2 slot) */
}

/* Byte-verified anchors:
 *   OUTST entry 41013, frame SAB 7, prologue JPL I 44 -> 003752,
 *   GETCH call JPL I 26 -> 030062, SOUTB calls JPL I -> 031030 (41050/41052/41055),
 *   frame unwind SAA -7 (41060) -> resident return 003776 (link cell 41070).
 *
 * The MON 162B -> OUTST link is NOT byte-proven: GOTAB[162] = 000000 (a
 * fall-through with no per-call vector); dispatch drops into the uncarved
 * resident MFELL/CALLPROC path. Attribution rests on the OUTST symbol name and
 * its GETCH -> SOUTB (fetch-char / output-byte) loop over a string. */
