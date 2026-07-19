/* MON 60B 145B ISSTDOM - start system domain from a SINTRAN command. From 5P-P2-MON60.NPL (.npl). */
int i_sstdom(CPU *cpu, Mon60Params *p)
{
    p->p1 = 0;                                     /* mark: return to SINTRAN OPCOM on escape */
    return mon60_common_path(cpu, 0145, mon60_buffer(cpu));
}
