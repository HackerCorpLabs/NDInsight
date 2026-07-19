/* MON 60B 114B IRELLOG - stop logging and release the facility. From 5P-P2-MON60.NPL (.npl). */
int i_rellog(CPU *cpu, Mon60Params *p)
{
    if (cpu->hist_rtp != RTREF) return mon60_error(cpu, ELOGNRESERVED);
    cpu->hist_flag = 0; cpu->hist_rtp = 0;
    return mon60_ok(cpu);
}
