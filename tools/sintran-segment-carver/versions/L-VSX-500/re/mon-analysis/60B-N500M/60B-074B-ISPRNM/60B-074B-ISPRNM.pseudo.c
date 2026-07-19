/* MON 60B 074B/136B ISPRNM/IPRACTIVE - copy process name from user. From 5P-P2-MON60.NPL (.npl). */
int i_sprnm(CPU *cpu, Mon60Params *p)
{
    Mon60Buffer *buf = mon60_buffer(cpu);
    frusmove(cpu, buf->name_buf, p->p1_ptr, 050);   /* process name, <=50B bytes */
    return mon60_common_path(cpu, p->function, buf);
}
