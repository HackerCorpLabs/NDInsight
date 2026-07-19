/* MON 60B 044B ISREL - release ND-500 CPU/system from special use. From 5P-P2-MON60.NPL (.npl). */
int i_srel(CPU *cpu, Mon60Params *p)
{
    if (ccpudf(cpu)->spref == RTREF) relcpu(cpu);   /* release CPU if caller reserved it */
    if (cpu->nspref == RTREF) cpu->nspref = 0;      /* release system if caller reserved it */
    return mon60_ok(cpu);
}
