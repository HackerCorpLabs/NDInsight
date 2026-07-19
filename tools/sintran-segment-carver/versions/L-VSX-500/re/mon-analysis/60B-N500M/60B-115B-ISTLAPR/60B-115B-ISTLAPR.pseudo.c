/* MON 60B 115B ISTLAPR - start process-log-all. From 5P-P2-MON60.NPL (.npl). */
int i_stlapr(CPU *cpu, Mon60Params *p)
{
    if (cpu->hist_rtp != 0 && p->a != RTREF) return mon60_error(cpu, ELOGINUSE);
    if (cpu->hist_flag != 0 && p->a != 3) return mon60_error(cpu, EILFUNC);
    cpu->hist_rtp = RTREF;
    memset(cpu->hist_data, 0, HIST_DATA_WORDS * 2);
    cpu->hist_flag = 3;                                    /* log-all started */
    return mon60_ok(cpu);
}
