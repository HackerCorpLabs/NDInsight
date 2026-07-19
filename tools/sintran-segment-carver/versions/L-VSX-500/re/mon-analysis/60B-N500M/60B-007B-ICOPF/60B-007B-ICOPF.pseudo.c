/* MON 60B ICOPF - shared file-name-copy handler for codes
 * 007B,046B,047B,067B,071B,130B,131B. Logic from 5P-P2-MON60.NPL (see .npl).
 * Dispatch byte-verified; body byte-location pending (bank-2 5IFUNC). */
int icopf(CPU *cpu, Mon60Params *p)
{
    Mon60Buffer *buf = mon60_buffer(cpu);
    /* A:=5P1; T:=200; CALL FRUSMOVE : copy file/segment name user -> MON60 buffer, <=200B(128) bytes */
    frusmove(cpu, buf->name_buf, p->p1_name_ptr, 0200);
    /* GO FAR 5NOPAR : common path -> system monitor performs the actual operation
     * (place swapper / define-delete swap file / read name-segment entry / std-domain). */
    return mon60_common_path(cpu, p->function, buf);
}
