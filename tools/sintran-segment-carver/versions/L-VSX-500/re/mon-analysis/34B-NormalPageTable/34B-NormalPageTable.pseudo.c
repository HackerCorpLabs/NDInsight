/* ============================================================================
 * MON 034B  NormalPageTable (ALTOF / documented ALTOFF)  -  pseudo-C model
 * SINTRAN III VSX/500 L.  Sets the alternative page table equal to the normal
 * page table, so all data addresses map through the normal page table again.
 *
 * Control flow is BYTE-VERIFIED for the ALTOF worker 004116B..004133B in resident
 * SINTRAN-DATA_commoncode; every instruction is translated per the canonical
 * ND100-INSTRUCTION-SEMANTICS.md. Addresses in comments are octal.
 *
 * DISPATCH CAVEAT: GOTAB[034] = 000000 = fall-through. This body is reached only
 * after the uncarved resident MFELL/CALLPROC second-level dispatch; the
 * MON 034B -> ALTOF edge is UNVERIFIED (no static pointer connects them). The
 * attribution rests on the ALTOFF short-name and ALTOF sitting right after ALTON
 * (MON 033B), sharing the same PCR-image pointer 004007B.
 * ============================================================================ */

/* Paging-control register access is privileged (TRA PGC / TRR PCR). */
void mon_normal_page_table(mon_regs *r)
{
    interrupts_off();                      /* 004116: IOF                          */
    int saved_A = r->A;                    /* 004117: STA -24                       */
    int saved_X = r->X;                    /* 004120: STX -23                       */

    r->A = 010;                            /* 004121: SAA 10 : level selector       */
    int pcr = read_PGC(r->A);              /* 004122: TRA PGC : read paging-control  */
    pcr = pcr & 074000;                    /* 004123: AND 14 -> mask 074000B         */
    pcr = pcr | 001616;                    /* 004124: ORA 14 -> const 001616B        */
    write_PCR(pcr);                        /* 004125: TRR PCR : alt-PT := normal-PT   */

    int pcr_img_ptr = mem[004007];         /* 004126: LDX I 7 -> X = mem[004007]     */
    mem[pcr_img_ptr + 017] = pcr;          /* 004127: STA ,X 17 : update PCR image    */

    r->A = saved_A;                        /* 004130: LDA -35 : restore A            */
    r->X = saved_X;                        /* 004131: LDX -34 : restore X            */
    interrupts_on();                       /* 004132: ION                             */
    /* 004133: EXIT -> P = L (return) */
}

/* No input parameters (the manual lists none for NormalPageTable). The call
 * simply resets the alternative page table to equal the normal page table by
 * rewriting the paging-control register. Error return: standard error code in A
 * (inferred from the manual; not byte-proven from this body). */
