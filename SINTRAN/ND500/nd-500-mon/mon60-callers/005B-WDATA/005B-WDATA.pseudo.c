/* ===========================================================================
 *  005B-WDATA   ->  MON 60 subfunction WDATA = 5B (0x05 = 5 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Purpose (NPL, authoritative): LOGICAL DATA MEMORY WRITE.  Handler: IDMWRITE.
 *  Four call sites: 002326, 002515 (rtn 002222); 055140 (rtn 055113);
 *                   056023 (rtn 055255).
 *  Return (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_WDATA(void);   /* thunk 146327 (SAA 5) -> gateway 146244        */
extern word *p;                 /* p[6],p[7],p[10] = MON 60 params 1..3          */

/* --- 002326 (routine 002222) --------------------------------------------- */
void write_data_mem_A(void)
{
    dword addr;   /* B-104 := const LDD 162 (logical DM addr, INFERRED)         */
    p[6]  = (word)&addr;         /* 002306..002313 */
    p[7]  = (word)&local_B102;   /* 002314..002321 (B-102 := B-167 SAD 33)       */
    p[10] = f_register;          /* 002322..002325 (3-word)                      */
    if (MON60_WDATA() == ERROR) frame_dispatch(/*->002144*/);  /* 002327 */
    /* 002330 callsite+2 = SUCCESS */
}

/* --- 002515 (routine 002222, entered via JMP from 002466) ---------------- */
void write_data_mem_B(void)
{
    dword addr;   /* B-104 := const LDD 43                                       */
    p[6]  = (word)&addr;         /* 002450..002455 */
    p[7]  = (word)&local_B110;   /* 002456..002460 */
    p[10] = f_register;          /* 002461..002465 (3-word)                      */
    if (MON60_WDATA() == ERROR) frame_dispatch(/*->002333*/);  /* 002516 */
    /* 002517 callsite+2 = SUCCESS */
}

/* --- 055140 (routine 055113) --------------------------------------------- */
void write_data_mem_C(void)
{
    p[6]  = (word)&local_B165;   /* 055123..055132 (B-165 := SAD SHR 20)         */
    p[7]  = (word)&local_B172;   /* 055133..055135 */
    p[10] = f_register_B170;     /* 055136..055137 (3-word)                      */
    if (MON60_WDATA() == ERROR) return LEAVE_value();  /* 055141 ptr=177327 */
    return LEAVE_skip();                               /* 055142 ptr=177335 */
}

/* --- 056023 (routine 055255) --------------------------------------------- */
void write_data_mem_D(void)
{
    p[6]  = (word)&local_B170;   /* 056012..056015 */
    p[7]  = (word)&local_B166;   /* 056016..056020 */
    p[10] = f_register_B155;     /* 056021..056022 (3-word)                      */
    if (MON60_WDATA() == ERROR) return LEAVE_value();  /* 056024 ptr=177327 */
    goto ok_056041;                                    /* 056025 */
}
