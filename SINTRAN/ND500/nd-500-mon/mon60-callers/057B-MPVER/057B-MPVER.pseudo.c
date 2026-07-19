/* 057B-MPVER -> MON 60 MPVER = 57B (0x2F = 47 dec). Purpose: READ MICRO PROGRAM VERSION, handler 5NOPAR.
 * Two call sites: 005577 (interpreter 002662) and 132132 (small routine 132124). OCTAL, BANK 1, base 0. */
extern int  MON60_MPVER(void);        /* JPL I -> thunk 146536; SAA 57 */
extern void cmd_error_002673(void);
extern word *p;
void cmd_version_site1(void)          /* 005573..005601 */
{
    p[6] = &frame_B127;               /* 005573..005576 STA ,X 6 = &(B-127) */
    if (MON60_MPVER() == ERROR)       /* 005577 JPL I 112 -> thunk 146536 (57B) */
        cmd_error_002673();           /* 005600 callsite+1 = ERROR */
    /* 005601 callsite+2 = SUCCESS -> 005623 */
}
int read_micro_version_site2(void)    /* 132127..132134, routine 132124 framesize 0 */
{
    p[6] = const_LDA24;               /* 132127 LDA 24; 132131 STA ,X 6 */
    if (MON60_MPVER() == ERROR)       /* 132132 JPL I 22 -> thunk 146536 (57B) */
        return LEAVE_error(A);        /* 132133 callsite+1 -> 177327 LEAVE-with-value */
    /* 132134 callsite+2 = SUCCESS */
}
