/* MON 60B 011B/056B IWRGS/IEPLACE - copy ND-500 register block from user. From 5P-P2-MON60.NPL (.npl). */
int i_wrgs(CPU *cpu, Mon60Params *p)
{
    Mon60Buffer *buf = mon60_buffer(cpu);
    frusmove(cpu, buf->regs, p->p1_ptr, NREGS << 2);  /* T:=NREGS SH 2; A:=5P1; FRUSMOVE */
    return mon60_common_path(cpu, p->function, buf);
}
