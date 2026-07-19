/* ==========================================================================
 * PRSRV-124B.pseudo.c
 * Readable pseudo-C for MON 124B PRSRV (ForceReserve) and the block-0
 * read-modify-write "directory reserve" flow of @ENTER-DIRECTORY.
 * SINTRAN III L07 (L-VSX-500). All addresses octal. Grades in comments:
 *   VERIFIED = re-read from carved bytes with dd/nd100-dis.
 *   INFERRED = reasoned from bytes + manual/NPL.
 *   OPEN     = crosses into an uncarved low-resident overlay / runtime state.
 * ========================================================================== */

/* --------------------------------------------------------------------------
 * TWO DISTINCT "reserves" happen in @ENTER-DIRECTORY. Do not conflate them:
 *
 *   (1) MON 124B PRSRV   - reserve the physical DEVICE UNIT (drive/LDN).
 *                          Called once at 0140252B in ENDIR. Device-level.
 *                          Works on SCSI (no 147B error is printed).
 *
 *   (2) CHDSI block-0    - stamp the DIRECTORY as "entered/owned" by reading
 *                          block 0, writing owner+flag, writing block 0 back.
 *                          Directory-level. THIS is the user's "read-modify-
 *                          write of block 0". It never completes on SCSI.
 * -------------------------------------------------------------------------- */


/* ==========================================================================
 * (1) MON 124B PRSRV  -  worker @037076B, resident in System Monitor 071-S3SM
 * ==========================================================================
 * Dispatch: MON 124 -> MCTAB[124B]=037076B  (VERIFIED dd: byteoff 1992 = 3e3e).
 *
 * PRSRV proper is a 2-word TRAMPOLINE (VERIFIED bytes):
 *   A  := MEM[037020] = 3              // selector/function constant  (VERIFIED)
 *   goto *MEM[037107];                 // = 027417B                   (VERIFIED)
 * The reserve body at 027417B is BELOW this segment's 30000B load base, so it
 * is not in the carve window -> OPEN. From the manual, the effect is:        */

int PRSRV(int *param /* A = &{DevNo, IOflag, RTProgram, Status} */)   /* manual */
{
    int selector = 3;                 /* VERIFIED: MEM[037020] loaded into A  */
    /* jump to the low-resident reserve primitive at 027417B (OPEN).
     * Per ND-860228 the primitive:
     *   - force-reserves logical device DevNo (input or output part = IOflag)
     *     for RT program RTProgram (0 = caller);
     *   - returns Status: 0 = OK, negative = already reserved.               */
    return force_reserve_primitive(selector, param);   /* OPEN body @027417B  */
}

/* Sibling reserve/release family, co-located, share a command executor.
 * RESRV(122B)@037103, RELES(123B)@037156, PRLS(125B)@037147 (VERIFIED MCTAB).
 * They funnel into EXECC@037110, which BUILDS a command line and runs it via
 * MON 70B = COMSB (the @-command interpreter). VERIFIED: two MON 70 sites at
 * 037124 and 037154; MCTAB[070]=050673=COMSB. */

void EXECC_execute_reserve_command(void)   /* @037110B, == OTRAN */
{
    if (MEM_ind(053) == 0) return;          /* 037111 JAF: gate clear -> EXIT */
    int link = L;                           /* 037113 T := L                  */
    build_command_string(MEM[046]);         /* 037116-037120 JPL I 45 helper  */
                                            /*   -> 004177B (OPEN, low res)   */
    L = link;
    MON_70_CallCommand(MEM[044]);           /* 037124 run RESERVE cmd -> COMSB */

    for (int x = 0; scan_ok(x); x++) {      /* 037125-037137 byte-scan loop   */
        int b = command_string[x];          /* 037132 LBYT                    */
        if (b == 047) {                     /* 037133 delimiter 47B           */
            MON_70_CallCommand(field);      /* 037154 run per-field command   */
        }
    }
    /* exact per-field semantics INFERRED; the two MON 70B and the loop are
     * byte-proven. */
}


/* ==========================================================================
 * (2) THE BLOCK-0 READ-MODIFY-WRITE "directory reserve"
 * ==========================================================================
 * Bodies in 006-S3FS (load base 26000B). Fully carved in ../ENTER-DIRECTORY/
 * and ../RCBLO/. Reproduced here as the reserve flow the user asked about.
 * All addresses/values VERIFIED in those carves; key ones re-checked here. */

/* CHDSI @037763B: the check/enter-directory driver. */
int CHDSI(directory *dir, int entering_system)
{
    ext_info blk0;

    /* --- READ block 0 (the directory master block) ------------------------ */
    RXDIR(dir, &blk0);          /* 040000 JPL I 143 -> [040143]=037643 RXDIR  */
                                /* RXDIR sets block:=0 (037651 RADD CLD 0 DD) */
                                /* then RCBLO reads block 0 via the device.   */
                                /* *** On SCSI the device transfer for block 0
                                 *     is never issued (SCWAQ empty): the read
                                 *     -modify-write stalls HERE.  VERIFIED
                                 *     (ground truth) / see README (d).  ***  */

    int capacity = GSIZE(dir);  /* 040023 JPL I 121 -> [040144]=037101 GSIZE  */
                                /* in-core configured size, no device I/O.    */

    /* --- checksum + self-heal (VERIFIED, ../ENTER-DIRECTORY sec 4) -------- */
    int sum = 0;
    for (int i = 1; i <= 7; i++) sum += blk0.word[i];   /* additive, not XOR  */
    if (sum != blk0.checksum) {           /* bad/zero checksum does NOT reject */
        for (int i = 0; i < 8; i++) blk0.word[i] = 0;   /* 040063-040071 zero  */
        blk0.capacity = capacity;                       /* 040077 STD ,X 6     */
        /* fall through to stamp + write-back */
    }

    /* --- OWNER INTERLOCK (VERIFIED control flow, ../ENTER-DIRECTORY sec 4) - */
    if ((blk0.flag & BIT15) &&            /* 040110-040117: reject only when   */
        blk0.owner != 0 &&                /*   flag bit15 set AND owner set    */
        blk0.owner != entering_system) {  /*   AND owner != this system        */
        return ERR_ALREADY_ENTERED;       /*   (32B / 34B; code INFERRED)      */
    }

    /* --- STAMP owner + flag  (this IS the directory "reserve") ------------ */
    blk0.owner = entering_system;         /* 040121-040125: word 5 := system   */
    blk0.flag |= BIT15;                   /* 040127:        word 4 bit15 := 1   */

    /* --- WRITE block 0 back ----------------------------------------------- */
    return WXDIR(dir, &blk0);   /* 040127 JPL I 30 -> [040157]=037702 WXDIR    */
}

/* WXDIR @037702B: recompute checksum, write block 0 back to the device. */
int WXDIR(directory *dir, ext_info *blk0)
{
    int sum = 0;
    for (int i = 1; i <= 7; i++) sum += blk0->word[i]; /* 037714-037721 loop   */
    blk0->checksum = sum;                              /* 037722-037723 store  */

    if (WCBLO_write_block0(dir, blk0) != OK)   /* 037727/037730 JPL I 25 ->    */
        return ERR_35B;                        /*   [037754] WCBLO (device WR) */
                                               /* 037747 SAA 35 = "Master block
                                                *   transfer error" (page-0 WR)*/
    return OK;
}

/* --------------------------------------------------------------------------
 * WHY IT FAILS ON SCSI  (VERIFIED / OPEN - see README section (d))
 *
 * The write-back (WXDIR) is never reached because the READ side (RXDIR->RCBLO)
 * never issues the block-0 device transfer on SCSI: RCBLO's
 *   036135 JPL I ,B 10   (dispatch through datafield word ,X 14 = SCSDISK)
 * does not execute for block 0, so SCWAQ stays empty and no page-0 READ and no
 * page-0 WRITE ever appear on the wire. MON 124 PRSRV is NOT the divergence
 * (no 147B error is raised). The abort is in the block-0 READ dispatch or the
 * upstream connect/init overlay - see ../RCBLO/README.md section 4 and the
 * settling DAP check.
 * -------------------------------------------------------------------------- */
