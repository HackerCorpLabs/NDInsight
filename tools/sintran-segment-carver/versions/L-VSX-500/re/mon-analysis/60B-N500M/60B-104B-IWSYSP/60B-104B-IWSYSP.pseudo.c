/* MON 60B 104B IWSYSP - write 16-word system-parameter block from user. From 5P-P2-MON60.NPL (.npl). */
int i_wsysp(CPU *cpu, Mon60Params *p)
{
    movus_in(cpu, &cpu->n500df.syspar, p->p1_ptr, 16 * 2);  /* K=0: user -> N500DF */
    return mon60_common_path(cpu, 104, mon60_buffer(cpu));
}
