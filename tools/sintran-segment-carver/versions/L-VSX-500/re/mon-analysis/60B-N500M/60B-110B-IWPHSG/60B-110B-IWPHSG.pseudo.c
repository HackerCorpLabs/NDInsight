/* MON 60B 110B IWPHSG - write into a physical segment (copy from user). From 5P-P2-MON60.NPL (.npl). */
int i_wphsg(CPU *cpu, Mon60Params *p)
{
    if (p->d3_bytecount > 04000) return mon60_error(cpu, EBIGBUF);  /* 4000B max */
    frusmove(cpu, mon60_buffer(cpu)->data, p->p4_ptr, p->d32_len);
    return mon60_common_path(cpu, 0110, mon60_buffer(cpu));
}
