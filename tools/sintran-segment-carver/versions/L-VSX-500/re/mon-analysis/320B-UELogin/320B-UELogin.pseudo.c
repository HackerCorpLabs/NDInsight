/* ===========================================================================
 * MON 320B UELogin - pseudo-C model for an emulator author.
 *
 * Models the UELOG worker body at 050726B in resident commoncode (region B of
 * 320B-UELogin.ASM). This is the routine named UELOG in the version-matched L07
 * SYMBOL-1-LIST, inside the User-Environment cluster (UECHE, UECOM, UELOG).
 *
 * HONESTY: the control flow (the two loops and the register moves) is
 * byte-verified from the real SINTRAN L bytes. The *meaning* of the scanned
 * records and status bits is INFERRED from the symbol name and neighbourhood -
 * the manual (section 2.14) lists MON 320B name-only, so field semantics are
 * not byte-proven. Fields marked "inferred" are not confirmed.
 *
 * All literals are octal to match the disassembly. This is a model, not a
 * transliteration - some resident helper calls (JPL I / JMP -40 into UECOM at
 * 050707) cross into code outside region B and are shown as opaque calls.
 * =========================================================================== */

/* An 8-word User-Environment entry (stride = AAX 10 = advance X by 8 words). */
typedef struct ue_entry {         /* layout inferred */
    word w[010];                  /* 8 words per entry; BSET ONE 10 DA sets bit 1
                                     (mask 02; the printed 10 is bn<<3, so bit no. 1) */
} ue_entry;

/* Region B, first stretch (050726-050747): scan/flag pass over UE entries.
 * Two AAX 10 at entry step the cursor by two entries before the loop body.
 * Inside: load a pair of words (LDATX), set bit 010 in each (BSET ONE 10 DA),
 * store back (STATX), advance one entry (AAX 10); a counter word is bumped
 * (MIN ,B 4) twice; the pass repeats via JMP -40 back into UECOM (050707). */
static void uelog_flag_pass(ue_entry *cursor, word *counter)
{
    cursor += 2;                          /* 050726-050727: AAX 10, AAX 10 (X += 8 words each) */
    for (;;) {
        (*counter)++;                     /* 050730: MIN ,B -21 - increment counter word;
                                             MIN skips the exit-JMP when it wraps to 0 (inferred) */
        if (/* limit reached */ 0)        /* 050731/050733: JMP/JAF -> 050745 exit */
            break;

        word a = cursor->w[0];            /* 050735: LDATX  (A = phys[EL]) */
        a |= 02;                          /* 050736: BSET ONE 10 DA - set bit 1 (mask 02) */
        cursor->w[0] = a;                 /* 050737: STATX  (phys[EL] = A) */
        cursor += 1;                      /* 050740: AAX 10 */

        word b = cursor->w[0];            /* 050741: LDATX */
        b |= 02;                          /* 050742: BSET ONE 10 DA - set bit 1 (mask 02) */
        cursor->w[0] = b;                 /* 050743: STATX */
        cursor += 1;                      /* 050744: AAX 10 */
    }
    (*counter) += 2;                      /* 050745-050746: MIN ,B 4 x2 - increment a separate
                                             counter word at ,B 4 twice (inferred) */
    /* 050747: JMP -40 -> 050707 (UECOM shared tail, outside region B) */
    uecom_tail(cursor, counter);
}

/* 050751-050776 is an embedded pointer/data block (constants + zero words),
 * NOT executable code - the disassembler renders it as instructions but the
 * bytes are the routine's data table. Do not execute. */

/* Region B, second stretch (050777-051025): record-copy / apply loop.
 * Copies a caller word (STD), loads a base descriptor, then loops applying an
 * update to successive records until a magnitude test (SKP IF DA MLST ST)
 * terminates it. Semantics of each record field are inferred. */
static void uelog_apply_pass(word *frame)
{
    word base = frame[-2];                /* 050777: STD -2 stores the A:D pair into local
                                             cells (P-2/P-1); base tracks that saved value */
    word ptr  = load137();               /* 051002: LDA 137 (resident pointer) */
    word limit = ldt135();               /* 051005: LDT 135 */

    while (base < limit) {                /* 051006: SKP IF DA MLST ST / 051007 JMP 051024 */
        word *rec = record_at(ptr);      /* 051010: LDX -16 */
        word f = rec[1];                 /* 051011: LDA ,X 1 */
        if (f == 0)                       /* 051013: JAZ 5 -> 051020 (skip inactive) */
            goto next;

        /* active record: stage login fields (inferred) */
        word d = rec_field17();          /* 051014: LDA -17 */
        frame[-0104] = d;                /* 051015: STD ,B -104 */
        d += 010;                        /* 051016: SAA 10 (A = 010); 051017: RADD SA DD
                                            (no CLD) -> D = D + A, i.e. D += 010 */

    next:
        base += 2;                       /* 051020-051021: LDA -26 / AAA 2 */
        store_base(base);                /* 051022: STA -30 */
        /* 051023: JMP -17 -> 051004 (loop) */
    }
    /* 051024-051025: LDD -27 / LDT 116 - load result for the return path */
}

/* Dispatch reality (see README): MON 320B -> GOTAB[320]=112616B lands on DSI6,
 * a device/interrupt routine (MISATTRIBUTED), NOT on UELOG. The real path from
 * the MON entry to UELOG@050726B crosses the uncarved resident CALLPROC bridge
 * and is therefore UNVERIFIED - this model documents the named worker, not a
 * byte-followed dispatch. */
