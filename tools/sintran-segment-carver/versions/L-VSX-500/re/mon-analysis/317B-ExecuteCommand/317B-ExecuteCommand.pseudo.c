/* ===========================================================================
 * MON 317B - UECOM / ExecuteCommand - pseudo-C model for an emulator author.
 *
 * SINTRAN III VSX/500 L07.  Derived from the carved bytes listed in
 * 317B-ExecuteCommand.ASM.  Control flow and the mode-code fork are VERIFIED from
 * bytes; the command-decoder internals are inferred (the decoder itself is a
 * separate body reached by JPL I 41 at 050746B and is not carved out here).
 *
 * All constants are octal, written here as C octal literals.
 * ===========================================================================
 */

/* ---------- Layer 1: the level-14 monitor entry (ENT14 @ 072167B) ---------- */

#define MGOTA   0071233     /* GOTAB base, 256 words, indexed by MON number   */
#define MFELL   0072114     /* the fall-through handler (224 of 256 slots)    */
#define CALLP   0032201     /* monitor-level entry, reached by a LEVEL switch */
#define MCTAB   0005620     /* monitor-call table, 256 words, indexed by N    */

void mon_entry_level14(int mon_number)
{
    /* 072266B holds the mask 000377B: the MON number is 8 bits, so GOTAB has
     * exactly 256 slots. */
    int n = mon_number & 0377;

    /* 072253B-072260B.  The fetch is bracketed by BSET ZRO/ONE SSPTM: the
     * table word is read through a different page-table mapping than the code
     * that runs afterwards.  Then the dispatch is a direct jump -- there is no
     * subroutine call anywhere on this path. */
    unsigned handler = GOTAB[n];        /* MEM[MGOTA + n]   */
    goto *handler;                      /* JMP ,X at 072260B */
}

/* Only 32 of the 256 GOTAB slots are real resident handlers (MON 1B read,
 * 2B write, 21B-24B, 63B, 163B, 200B XMSG, 310B, 346B-377B).  Those arm the
 * B-level via IOB14 = 071660B.  Every other slot -- including MON 317B --
 * holds MFELL. */

/* ---------- Layer 2: MFELL, the level switch (072114B) ---------- */

void mfell(int mon_number)
{
    /* 072114B-072122B.  MFELL is NOT "illegal monitor call".  It hands the
     * call to the monitor program level: it writes the MON number into that
     * level's X register and CALLP into that level's P register, then
     * activates the level (MST PID / MST PIE).
     *
     * THIS is the hop that earlier analysis called the "uncarved CALLPROC
     * bridge".  It is carved, and it is a program-LEVEL switch, not a call. */
    level_write_X(MONITOR_LEVEL, mon_number);   /* IRW 20 DX  @072115B */
    level_write_P(MONITOR_LEVEL, CALLP);        /* IRW 20 DP  @072117B */
    activate_level(MONITOR_LEVEL);              /* MST PID/PIE         */
}

/* ---------- Layer 3: the monitor level dispatches through MCTAB ---------- */

void callp(int mon_number)
{
    /* MCTAB = 005620B lives in 044-S3IDPIT.  216 of its 256 slots are
     * populated and every populated slot lands exactly on a named L07 symbol
     * (RDISK, WDISK, CIBUF, OPFIL, MAGTP, DEBUG, CPUST, MOINF, UECOM, ...).
     * MCTAB[317B] @ 006137B = 050701B = UECOM. */
    unsigned worker = MCTAB_word[mon_number];   /* MEM[MCTAB + n] */
    call(worker);
}

/* ---------- Layer 4: the UECOM worker (050701B, segment 003-S3CP) ---------- */

/* Frame slots used by the shared body (B-relative, from the carved bytes): */
#define F_STRPTR   (-0177)    /* ,B -177 : caller's command-string pointer */
#define F_MODE     (-0175)    /* ,B -175 : which of the three calls we are  */
#define F_TSAVE    (-0200)    /* ,B -200 : caller's T                       */

/* COMSB (MON 070B), UECOM (MON 317B) and UELOG (MON 320B) are THREE ENTRIES
 * INTO ONE BODY.  They differ only by the mode code they store at F_MODE.
 * That is exactly why the manual says COMND terminates the caller on error and
 * UECOM does not: same code, different mode. */
#define MODE_COMSB   1
#define MODE_UECOM_A 2        /* 050720B */
#define MODE_UECOM_B 4        /* 050716B */
#define MODE_UELOG   3

void UECOM(unsigned A, unsigned T)          /* entry 050701B */
{
    frame[F_STRPTR] = A;                    /* 050705B: A = address of the command string */
    frame[F_TSAVE]  = T;                    /* 050706B */

    /* 050707B-050721B: T is range-checked against two constants (@051003B and
     * @051004B) and selects mode 4 or mode 2.  What the two variants mean is
     * NOT byte-proven -- do not guess it. */
    frame[F_MODE] = (in_range(T)) ? MODE_UECOM_B : MODE_UECOM_A;

    goto shared_command_body;               /* 050725B -> 050741B */
}

void shared_command_body(void)              /* 050740B / 050741B */
{
    char *cmd = (char *)frame[F_STRPTR];    /* 050745B: LDX ,B -177 */

    /* 050750B-050771B: walk the command text and upper-case it in place.
     * LBYT/SBYT with the classic 'a'(141B) .. 'z' test and AAA -40 (subtract
     * 40B) -- SINTRAN command matching is case-insensitive. */
    for (int i = 0; cmd[i]; i++) {
        if (cmd[i] >= 0141 && cmd[i] <= 0172)
            cmd[i] -= 040;
    }

    /* 050746B: JPL I 41 -> the standard SINTRAN command decoder (the same one
     * the terminal command processor uses: shortest-unique-prefix match, then
     * dispatch).  The decoder body is NOT carved in this folder.
     *
     * Guaranteed behaviour (from the official manual, ND-860228-2 p.180-181):
     *   - runs the command synchronously; control returns when it completes
     *   - an error prints a message and RETURNS -- it does NOT terminate the
     *     calling program (this is the documented difference from MON 70B)
     *   - command output goes to the terminal
     *   - missing parameters are prompted for on the terminal
     */
    decode_and_run_sintran_command(cmd, frame[F_MODE]);
}
