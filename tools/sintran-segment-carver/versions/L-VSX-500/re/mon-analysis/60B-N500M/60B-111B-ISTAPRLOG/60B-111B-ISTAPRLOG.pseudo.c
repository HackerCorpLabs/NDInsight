/* MON 60B 111B ISTAPRLOG - start process-log-one. From 5P-P2-MON60.NPL (.npl). */
int i_staprlog(CPU *cpu, Mon60Params *p)
{
    if (cpu->hist_rtp != 0 && p->a != RTREF) return mon60_error(cpu, ELOGINUSE);
    if (cpu->hist_flag != 0 && p->a != 2) return mon60_error(cpu, ELOGINUSE);
    if (p->d1_procno > MX5PROCS) return mon60_error(cpu, EILPAR);
    cpu->logproc = p->d1_procno; cpu->hist_rtp = RTREF;
    cpu->hist_flag = 2;                                    /* logging started */
    memset(cpu->hist_data, 0, HIST_DATA_WORDS * 2);
    return mon60_ok(cpu);
}
