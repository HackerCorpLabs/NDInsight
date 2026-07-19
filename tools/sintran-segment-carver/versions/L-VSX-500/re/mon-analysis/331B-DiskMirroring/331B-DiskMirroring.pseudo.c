/* ============================================================================
 * MON 331B  DiskMirroring (MSYSU)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  DIA10 entry stub 025-S3IRPIT @112532B ;
 * MSYSU worker body 025-S3IRPIT @132567B..132733B.
 *
 * Derived from the real disassembly (see 331B-DiskMirroring.ASM). The DIA10
 * register-block save + TRR PCR context switch, and the MSYSU 16-way
 * sub-function jump table + status store, are BYTE-VERIFIED. The meaning of
 * each of the 16 DIMIR sub-functions and the caller-side parameter convention
 * are INFERRED from the DiskMirroring manual (section 2.14) - treat as a model.
 *
 * NOTE ON DISPATCH: GOTAB[331] = 112532B routes to the DIA10 stub. The stub
 * saves the caller register block, then MSYSU runs after a page-context switch
 * (TRR PCR) whose second-level bridge (resident CALLPROC) is in an uncarved
 * overlay. So "DIA10 reaches MSYSU" is INFERRED from the symbol + the 16-way
 * dispatch that matches the DiskMirroring contract, not a followed pointer.
 * Register roles X (saved register block) and B (per-call disk datafield) are
 * inferred from the access pattern. Addresses in comments are octal.
 * ============================================================================ */

/* 112532-112560 (DIA10): disk-device level-14 entry stub. */
void mon_diskmirror_entry(mon_regs *r, reg_block *X, disk_field *B)
{
    X->slot17 = r->A;                  /* 112532 STA ,X 17  save caller A       */
    set_page_control_register(r->A);   /* 112533 TRR PCR    context switch      */
    X->reg_x = r->L;                   /* 112534 RADD CLD SL DX : X = L (COPY)   */
    X->slot0 = r->T;                   /* 112535 STT ,X 0   save MON number      */
    interrupts_off();                  /* 112537 IOF        critical section     */
    X = (reg_block *)mem[0112661];     /* 112541 LDX I 120  worker block pointer  */
    X->slot17 = r->A;                  /* 112542 STA ,X 17                       */
    interrupts_on();                   /* 112543 ION                             */
    B->status = B->slot22;             /* 112544-112545 LDA ,B 22 ; STA ,B 12    */
    /* 112546 JMP -> 112630 shared disk-stub tail, then MSYSU runs after the
     * page-context switch (bridge in uncarved resident code). */
    mon_diskmirror_worker(X, B);
}

/* 132567-132733 (MSYSU): 16-way DIMIR sub-function dispatcher. */
int mon_diskmirror_worker(reg_block *X, disk_field *B)
{
    int idx, status;

    setup_mirror_state(X, B);          /* 132567-132601 JPL I 145/142/137 setup  */
    if (mem[deref(0132737)] != 0) {    /* 132602 LDA I 135 ; 132603 JAZ          */
        /* 132604-132617: mirror-flag path; on failure store error code and
         * return via the caller link word [132742]. */
        if (mirror_check_failed(X, B)) {
            B->status = mem[0132741];  /* 132607 LDA 132 (P-rel) ; 132610 STA ,B 12 */
            goto ret;                  /* 132611 JMP I 131 -> [132742]           */
        }
    }

    /* 132620-132644: bound the sub-function index to 0..17 and computed-jump.
     * 132621 SAT 17 ; 132622 SKP IF DT MGRE SA -> skip when 17 >= idx ;
     * 132624 RADD SA DP  -> P = P + idx : indexes the 16-entry JMP table. */
    idx = B->slot20;                   /* 132620 LDA ,B 20  sub-function index    */
    if ((unsigned)idx > 017)           /* 132621-132623 out of range              */
        goto deflt;
    switch (idx) {                     /* 132624 computed jump; 132626-132644 tbl */
        case 001: status = dimir_fn(X, B, mem[0132747]); break; /* 132647 JPL I 100 */
        case 002: status = dimir_fn(X, B, mem[0132750]); break; /* 132652 JPL I 76  */
        case 003: status = dimir_fn(X, B, mem[0132751]); break; /* 132655 JPL I 74  */
        case 004: status = dimir_fn(X, B, mem[0132752]); break; /* 132660 JPL I 72  */
        case 005: status = dimir_fn(X, B, mem[0132753]); break; /* 132663 JPL I 70  */
        /* ... indices 6..17 each JPL I through pointer words [132754]..[132765];
         * the concrete DIMIR tag / header / layout / lock operation each performs
         * is INFERRED (worker bodies are past the pointer cells, outside carve). */
        default: deflt: status = mem[0132747]; break;  /* 132645 LDA 101 default  */
    }

    B->status = status;                /* 132726 STA ,B 12  status word to caller */
ret:
    return B->status;                  /* 132733 JMP I 7 -> [132742] return       */
}

/* Caller (INFERRED, manual section 2.14):
 *   MON 331B DiskMirroring: internal-use call; a sub-function index (0..17 octal)
 *   selects one of 16 DIMIR operations (mirror tags / headers / layout / locks).
 *   Exact register/CALLG mapping is NOT confirmed from these bytes.
 */
