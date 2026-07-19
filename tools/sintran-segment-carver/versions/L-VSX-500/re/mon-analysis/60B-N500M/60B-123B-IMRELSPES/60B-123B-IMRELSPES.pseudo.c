/* MON 60B 123B IMRELSPES - release ND-500 + memory from the test-monitor. From 5P-P2-MON60.NPL (.npl). */
int i_mrelspes(CPU *cpu, Mon60Params *p)
{
    if (ccpudf(cpu)->spref != RTREF) return mon60_ok(cpu);   /* not caller's -> nothing to release */
    return mon60_common_path(cpu, 0123, mon60_buffer(cpu));  /* release memory */
}
