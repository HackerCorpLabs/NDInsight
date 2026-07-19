/* MON 60B 134B IPLDEB - place debugger (copy name). From 5P-P2-MON60.NPL (.npl). */
int i_pldeb(CPU *cpu, Mon60Params *p)
{
    frusmove(cpu, mon60_buffer(cpu)->name_buf, p->p2_ptr, 0200);  /* debugger name, <=200B */
    return mon60_common_path(cpu, 0134, mon60_buffer(cpu));
}
