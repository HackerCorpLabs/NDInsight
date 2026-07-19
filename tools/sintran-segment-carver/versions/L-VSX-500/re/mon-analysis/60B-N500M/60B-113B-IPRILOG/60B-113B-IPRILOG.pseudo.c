/* MON 60B 113B IPRILOG - read process-log data to user. From 5P-P2-MON60.NPL (.npl). */
int i_prilog(CPU *cpu, Mon60Params *p)
{
    if (cpu->hist_rtp != RTREF) return mon60_error(cpu, ELOGNRESERVED);
    int words = (cpu->hist_flag == 2) ? 16 : (MX5PROCS + 1 + 3);   /* log-one vs log-all */
    movus(cpu, p->p2_ptr, cpu->hist_data, words * 2);
    if (p->d1 == 0) memset(cpu->hist_data, 0, HIST_DATA_WORDS * 2); /* clear if requested */
    return mon60_ok(cpu);
}
