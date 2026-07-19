/* MON 60B 075B ITSTUSER - check if caller is user SYSTEM. From 5P-P2-MON60.NPL (.npl). */
int i_tstuser(CPU *cpu, Mon60Params *p)
{
    if (cpu->passtype == 2) return mon60_ok(cpu);          /* 2 = user SYSTEM */
    return mon60_error(cpu, ENAUTHORISED);
}
