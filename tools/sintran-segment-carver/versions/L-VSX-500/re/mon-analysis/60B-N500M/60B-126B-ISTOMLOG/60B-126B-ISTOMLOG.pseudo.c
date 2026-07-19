/* MON 60B 126B ISTOMLOG - stop and release moncall log. From 5P-P2-MON60.NPL (.npl). */
int i_stomlog(CPU *cpu, Mon60Params *p)
{
    if (cpu->mlog != RTREF) return mon60_error(cpu, ELOGNRESERVED);
    cpu->mlog = 0; cpu->mlogproc = 0;
    return mon60_ok(cpu);
}
