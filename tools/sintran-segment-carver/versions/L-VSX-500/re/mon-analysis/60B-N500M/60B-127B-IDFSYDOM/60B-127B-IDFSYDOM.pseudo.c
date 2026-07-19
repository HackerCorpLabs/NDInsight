/* MON 60B 127B/161B IDFSYDOM/INDFSYDOM - define standard domain (copy info). From 5P-P2-MON60.NPL (.npl). */
int i_dfsydom(CPU *cpu, Mon60Params *p)
{
    frusmove(cpu, mon60_buffer(cpu)->data, p->p1_ptr, 04000);  /* domain info, <=4000B */
    return mon60_common_path(cpu, p->function, mon60_buffer(cpu));
}
