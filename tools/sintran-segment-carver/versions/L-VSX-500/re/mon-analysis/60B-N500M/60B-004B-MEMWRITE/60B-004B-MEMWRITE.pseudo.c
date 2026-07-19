/* MON 60B 004B/005B/033B IPMWRITE/IDMWRITE/IDAMW - write data block into ND-500 memory.
 * Logic from 5P-P2-MON60.NPL (.npl). Dispatch byte-verified; body loc pending (bank-2 5IFUNC). */
int i_memwrite(CPU *cpu, Mon60Params *p)
{
    Mon60Buffer *buf = mon60_buffer(cpu);
    uint32_t count = p->d1_bytecount;          /* AD:=5DD1 */
    if (count > 04000)                         /* 4000B bytes max (buffer size) */
        return mon60_error(cpu, EBIGBUF);
    frusmove(cpu, buf->data, p->p3_data_ptr, p->d12_len); /* T:=5D12; A:=5P3; FRUSMOVE */
    return mon60_common_path(cpu, p->function, buf);      /* GO FAR 5NOPAR -> perform write */
}
