/* MON 60B 160B IN5SEGLOAD - place one segment (new domain format). From 5P-P2-MON60.NPL (.npl). */
int i_n5segload(CPU *cpu, Mon60Params *p)
{
    Mon60Buffer *buf = mon60_buffer(cpu);
    frusmove(cpu, buf->name_buf, p->p1_ptr, 12);            /* 12-byte segment name */
    if (p->d41_shared_flag != 0)
        xfrusmove(cpu, buf->shared_info, p->p4_ptr, 0300, 5); /* shared-info block, 300B */
    return mon60_common_path(cpu, 0160, buf);
}
