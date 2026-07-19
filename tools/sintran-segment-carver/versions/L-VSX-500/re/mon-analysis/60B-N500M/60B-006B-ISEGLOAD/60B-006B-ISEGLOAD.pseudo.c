/* MON 60B 006B ISEGLOAD - place one ND-500 segment. Logic from 5P-P2-MON60.NPL (.npl).
 * Dispatch byte-verified; body byte-location pending (bank-2 5IFUNC). */
int isegload(CPU *cpu, Mon60Params *p)
{
    Mon60Buffer *buf = mon60_buffer(cpu);
    /* copy segment name (param1, <=200B bytes) */
    frusmove(cpu, buf->name_buf, p->p1_name_ptr, 0200);
    /* if the segment has ND-100/ND-500 shared parts, copy the shared-info block too */
    if (p->d51_shared_flag != 0)
        xfrusmove(cpu, buf->shared_info, p->p5_shared_ptr, /*bytes*/ 040, /*page*/ 0100);
    /* common path performs the place */
    return mon60_common_path(cpu, 006, buf);
}
