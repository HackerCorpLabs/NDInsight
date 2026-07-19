/* MON 60B 013B ICONNFI - connect a file for the ND-500. Logic from 5P-P2-MON60.NPL (.npl).
 * Dispatch byte-verified; body byte-location pending (bank-2 5IFUNC). */
int iconnfi(CPU *cpu, Mon60Params *p)
{
    Mon60Buffer *buf = mon60_buffer(cpu);
    frusmove(cpu, buf->name_buf, p->p1_name_ptr, 0200);   /* file name, <=200B bytes */
    xfrusmove(cpu, buf->file_type, p->p3_type_ptr, 4, 0100); /* file type, 4 bytes */
    return mon60_common_path(cpu, 013, buf);
}
