/* MON 60B 103B IRSYSP - read 16-word ND-500 system-parameter block to user. From 5P-P2-MON60.NPL (.npl). */
int i_rsysp(CPU *cpu, Mon60Params *p)
{
    movus(cpu, p->p1_ptr, &cpu->n500df.syspar, 16 * 2);   /* 16 words */
    return mon60_ok(cpu);
}
