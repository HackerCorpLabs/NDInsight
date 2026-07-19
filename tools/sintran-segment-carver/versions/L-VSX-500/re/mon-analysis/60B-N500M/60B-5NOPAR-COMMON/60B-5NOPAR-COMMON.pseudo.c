/* MON 60B 5NOPAR - common hand-off: package the moncall and enter the ND-500 SYSTEM MONITOR.
 * From 5P-P2-MON60.NPL (.npl). This is the boundary to the "more than MON 60" system-monitor code. */
int mon60_common_path(CPU *cpu, uint16_t function, Mon60Buffer *buf)
{
    int procno = caller_procno(cpu);                 /* RDIV(5PRDESCR-S500S, 5PRDSIZE) */
    cpu->saved_rsegm = rt_of(cpu, RTREF)->rsegm; rt_of(cpu, RTREF)->rsegm = 0;
    map_nd500_dataseg(cpu, procno);                  /* M1MEXY */
    if (function == N5RES || function == MRESSPES || function == SSTDOM)
        clear_nd500_dataseg(cpu);                    /* + set up IOF exit */
    if (function == WISON || function == TSTFUNC)
        copy_all_rtres_to_sbuffr(cpu);               /* proc census onto ND-500 data seg */
    /* build the moncall-info block (the 5MPM message the system monitor reads) */
    save_moncall_info(cpu, function, buf);           /* MOVAA of ZPREG..5DFSIZE */
    irq_off(cpu);
    brelease(cpu, RTREF);                            /* release N500DF datafield */
    irq_on(cpu);
    if ((function & 0377) == FORGET) cforget(cpu);   /* STOP-ND-500 */
    if (cpu->background) escon(cpu);
    /* ENTER THE ND-500 SYSTEM MONITOR (NOT part of N500M - the "more than MON 60" code). */
    return fpt2entry(cpu);                           /* -> 5PT2RET (ok/err) or SYMNLOAD (not loaded) */
}
