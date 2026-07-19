/* ============================================================================
 * MON 120B  WriteToFile (WFILE)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Shares one body with MON 117B ReadFile (RFILE).
 *
 * Derived from the real disassembly (see 120B-WriteToFile.ASM). Control flow
 * and the read/write (SSK) fork are BYTE-VERIFIED; the semantic labels (which
 * file-system worker does what, error-number meanings) are INFERRED from the
 * call structure and the FILSYS symbol table - treat as a model, not gospel.
 * Addresses in comments are octal.
 * ============================================================================ */

/* Entry 102132 = WRITE (ssk=1); entry 102130 (RFILE) = READ (ssk=0).
 * One shared body forks on the SSK skip flag. */
int mon_file_xfer(mon_regs *r, int ssk /* 0=read, 1=write */)
{
    frame_setup();                         /* 102132-102136: SAB 17, build B frame */
    prologue_worker();                     /* 102137 JPL I -> 003752 (resident)     */

    rec = r->B[5];                         /* 102140 LDX ,B 5 = ptr to param RECORD */
    fno   = rec[23];  local_fno  = fno;    /* 102141-102142                          */
    bufp  = rec[22];  local_buf  = bufp;   /* 102143-102144                          */
    nbyte = rec[24];  local_nb   = nbyte;  /* 102145-102146                          */
    blk   = rec[20];                       /* 102147-102150                          */
    setup_worker(blk);                     /* 102151 JPL I -> 010376 (resident)     */

    mode = (ssk ? SD : SA);                /* 102152-102157: direction word -> B+10 */

    /* --- argument validation (102160-102207); each failure loads an error   */
    /*     number (SAA, a literal) and JMP I -> 102476 (common "store+return")  */
    /* 102160-102172: the error is chosen by X==0 then the A,D values (JAF /    */
    /* SKP IF DD EQL 0), NOT by ssk:                                            */
    /*   X==0 & A!=0 -> 0125; X==0 & A==0 & D==0 -> 0132; X==0 & A==0 & D!=0 -> 0126 */
    if (bad_count(nbyte))     return fail(/* per X,A,D: 0132 / 0126 / 0125 */ 0132);
    if (bad_flag(rec[3]))     return fail(0133);          /* 102176 SAA 133      */
    /* 102200-102206: THIS one is ssk-selected (102202 BSKP ONE SSK):           */
    /*   write (SSK=1) -> 0125 (102204); read (SSK=0) -> 0126 (102206).         */
    if (bad_mode(rec[3]))     return fail(ssk ? 0125 : 0126);

    /* --- block / byte-offset arithmetic (102210-102305) --------------------*/
    /* 102210 LDA ,B 13 / 102211 SAT -1 / 102212 SKP IF DA UEQ ST.             */
    /* SKP UEQ skips 102213 JMP when B[13] != -1, so B[13] != -1 FALLS INTO    */
    /* the ATMUL branch; B[13] == -1 TAKES 102213 JMP 11 -> 102224 (RDIV path).*/
    if (blk != -1) {
        /* 102214-102223: scale block by block-size via ATMUL                 */
        blk = ATMUL(rec[14] /*block-size*/); /* 102215 JPL I -> 033740 (ATMUL)      */
    } else {
        /* 102224-102242: block -1 ("next"): normalise via RDIV by block-size; */
        /* a nonzero remainder loads a P-relative constant and fails.          */
        /* 102240 LDA 57 = A = mem[P+57] (const 0252 at 102317), then exit.    */
        if (rdiv_remainder(blk, rec[14]))  return fail(0252 /* 102240 LDA 57 */);
    }
    pos = blk * rec[14];                   /* 102254-102255 MPY ,X 14 (block-size)  */
    /* 102256-102304: 32-bit (INTEGER4) byte-count + double-word offset build  */
    CLRDB();                               /* 102305 JPL I -> 035250 (clear buffer) */

    /* --- core transfer, forked on SSK (102321-102432) ----------------------*/
    if (ssk == 1) {                        /* 102321 BSKP ONE SSK: WRITE half       */
        if (segment_changed())
            CHSGM(local_fno);              /* 102342 JPL I -> 101373 (CHSGM)        */
        if (data_portion)
            rc = FDWRT(local_fno, blk, local_buf); /* 102351 JPL I -> 100570        */
        else
            rc = FWRT (local_fno, blk, local_buf); /* 102361 JPL I -> 100130        */
    } else {                               /* 102364: READ half (mirror bounds chk) */
        if (out_of_bounds())               return fail(3);   /* 102403                */
        if (data_portion)
            rc = FDREA(local_fno, blk, local_buf); /* 102423 JPL I -> 100566        */
        else
            rc = FREA (local_fno, blk, local_buf); /* 102431 JPL I -> 077542        */
    }

    /* --- residual/status compute + exit (102433-102500) --------------------*/
    /* 102437-102460: recompute remaining byte count into B+11 (SWAP/SUB/RSUB, */
    /* 102450 JAN skips the underflow store). On underflow 102457 LDA 31 loads  */
    /* a P-relative constant (A = mem[P+31] = 0234 at 102510) into B+11.        */
    /* 102462/102471 SVCAL/RSCAL save-restore around the update.               */
    status = r->B[11];                     /* 102473 LDA ,B 11                       */
    r->B[2] = status;                      /* 102476 STA ,B 2 (caller status slot)  */
    return status;                         /* 102500 JMP I -> 003776 (resident ret) */
}

/* Callers:
 *   MON 117B ReadFile:   mon_file_xfer(r, 0);   (entry 102130 RFILE)
 *   MON 120B WriteToFile:mon_file_xfer(r, 1);   (entry 102132 WFILE)
 *
 * Seek trick: NoOfBytes = 0 positions the file to the given block boundary
 * (offset = block * block-size); arbitrary byte offsets need MON 74B.
 */
