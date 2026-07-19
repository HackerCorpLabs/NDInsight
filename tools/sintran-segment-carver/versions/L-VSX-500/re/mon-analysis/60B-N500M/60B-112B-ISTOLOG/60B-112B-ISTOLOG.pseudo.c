/* MON 60B 112B ISTOLOG - stop logging. From 5P-P2-MON60.NPL (.npl). */
int i_stolog(CPU *cpu, Mon60Params *p)
{
    if (cpu->hist_rtp != RTREF) return mon60_error(cpu, ELOGNRESERVED);
    cpu->hist_flag = 0;
    return mon60_ok(cpu);
}
