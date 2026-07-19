/* ============================================================================
 * MON 201B  HDLCfunction (HDLC)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Performs HDLC line-driver functions: SEND a Driver
 * Control Block (DCB) to the driver (Func=0) or RECEIVE a DCB from the driver
 * (Func=1).  Also used by X.21 to move DCBs between user programs and the X.21
 * driver; the Logical Device Number selects HDLC vs X.21.  On error the HDLC
 * status code is returned in W1 (1..11 octal).
 *
 * Dispatch reality:
 *   GOTAB[201B] = 000000 -> FALL-THROUGH (BYTE-VERIFIED): there is no per-call
 *   level-14 stub; dispatch drops into the resident MFELL/CALLPROC second-level
 *   path, which is UNCARVED and NOT byte-followable statically.
 *   HDLC @103112B (segment 025-S3IRPIT, the SYMBOL-2-LIST code overlay) is the
 *   NAMED worker and IS real executable code - a function dispatcher inside the
 *   PIO / X.21 line-driver module.  The fall-through -> HDLC link crosses the
 *   uncarved CALLPROC, so identity rests on the symbol NAME (HDLC) + the
 *   send/receive DCB behaviour that matches the manual - see README caveats.
 *
 * The HDLC dispatcher body is BYTE-VERIFIED (see 201B-HDLCfunction.ASM); the
 * DCB field layout and the exact status-code meanings are INFERRED from the
 * manual (201B_HDLCfunction.yaml).  Addresses in comments are octal.  Every
 * instruction is translated per ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md.
 * ============================================================================ */

/* HDLC worker @103112B (025-S3IRPIT code overlay).  Validates the function,
 * then transfers the DCB across the MMU-bypassing physical path (STATX / LDATX,
 * with T supplying the bank/high byte).  Returns an HDLC status code. */
int mon_201B_HDLCfunction(mon_regs *r)   /* in: A = addr of parameter list */
{
    /* The parameter list holds Func, DevNo, Buffer, USize, MSize; the worker
     * stages them into its local frame (cells [B+33]..[B+41]).  Modelled here
     * as fields; the exact frame offsets are byte-visible but their names are
     * INFERRED from the manual parameter order.                                */
    hdlc_params *p = (hdlc_params *)r->A_paramlist;

    /* 103114-103136 RADD/SAT/STATX-free init: first use of this LDN initialises
     * the driver state cell [B+40] (== -1 sentinel) via a driver-init worker.   */
    if (frame[B+40] == -1) driver_init(p->DevNo);   /* BYTE: SAT -1 / SKP UEQ */

    /* 103137-103142: Func selector.  Func==0 branch -> RECEIVE; else SEND.
     * (Manual: 0 = send DCB to driver, 1 = receive DCB from driver.  The carved
     *  test compares the selector cell [B+33] against 0.)                       */
    if (p->Func == RECEIVE) goto receive;           /* 103141 SKP EQL / JMP 103224 */

    /* ---- SEND-DCB path (103143-103223) ---- */
    /* 103143-103146: used size must be positive, else status 5 (illegal size). */
    if (p->USize <= 0)  return 5;                   /* BYTE: SAT -5 / JMP I ,B57 */
    /* 103147-103153: max size must be >= used size, else status 7.             */
    if (p->MSize < p->USize) return 7;              /* BYTE: SKP LST / SAT -7    */
    /* 103154-103164: obtain a driver buffer, else status 4 (no buffer).        */
    void *dcb = alloc_driver_buffer(p->MSize);      /* BYTE: JPL I ->[103306/7] */
    if (dcb == NULL)   return 4;                    /* BYTE: SAT -4 / JMP I ,B57 */
    /* 103165-103213: write the DCB header words into the physical buffer.
     * STATX is a 24-bit PHYSICAL store, phys[EL] := A with
     * EL = ((T & 0377) << 16) | ((X + disp) & 0177777), T = the DevNo/bank cell
     * [B+41].  The MMU is bypassed - the driver and the user share this buffer. */
    phys_store_dcb(dcb, p, /*bank=*/frame[B+41]);   /* BYTE: 4x STATX */
    /* 103214-103221: queue the DCB to the driver under an IOF/ION guard.        */
    disable_io_interrupts();                        /* 103215 IOF */
    queue_dcb_to_driver(p->DevNo, dcb);             /* JPL I ->[103311/103312]  */
    enable_io_interrupts();                         /* 103220 ION */
    return 0;                                       /* 103222 SAT 0 / 103223 return: status OK */

receive:
    /* ---- RECEIVE-DCB path (103224-103240..) ---- */
    /* 103224-103226: dequeue a DCB from the driver under IOF; if none queued,
     * fall to the status path (manual status 3 = no DCB in queue - INFERRED).   */
    disable_io_interrupts();                        /* 103224 IOF */
    void *rdcb = dequeue_dcb_from_driver(p->DevNo); /* JPL I ->[103313] */
    if (rdcb == NULL) { /* 103226 JMP 103320 */ return 3; /* INFERRED: no DCB */ }
    enable_io_interrupts();                         /* 103227 ION */
    /* 103231-103240..: read the DCB header words back with LDATX (A := phys[EL],
     * same physical/bank addressing as the send path) and copy them to the
     * caller's fields; the tail continues in the adjacent PIO04 driver label.   */
    phys_load_dcb(rdcb, p, /*bank=*/frame[B+41]);   /* BYTE: 2x LDATX + tail (UNVERIFIED tail) */
    return 0;                                        /* status OK (INFERRED) */
}

/* Caveats for the emulator author:
 *   - GOTAB[201B]=0 is a BYTE-VERIFIED fall-through; there is no stub to follow.
 *     The MON 201 -> worker transfer is via the UNCARVED resident MFELL/CALLPROC,
 *     so the fall-through -> HDLC hop is NOT byte-followable here.
 *   - HDLC=103112B IS real executable code in the 025-S3IRPIT overlay; its
 *     attribution to MON 201 rests on the symbol NAME (HDLC) + the send/receive
 *     DCB shape (STATX/LDATX physical transfers, status codes 4/5/7) that match
 *     the manual - not a followed pointer.  Status: partial (fall-through).
 *   - The DCB field layout, the frame-cell -> parameter mapping, and the exact
 *     status numbers (1,2,3,6,10,11) not seen on the SEND path are manual-derived
 *     (201B_HDLCfunction.yaml), not byte-isolated here.
 *   - The manual short name is MHDLC; MHDLC=120573B is the ND-500 companion in
 *     the N500 symbol set - a different entry from the ND-100 HDLC worker.
 *   - A live PC trace (issue a real MON 201, single-step the level-14
 *     fall-through into the resident CALLPROC) is needed to confirm P lands on
 *     HDLC=103112.
 */
