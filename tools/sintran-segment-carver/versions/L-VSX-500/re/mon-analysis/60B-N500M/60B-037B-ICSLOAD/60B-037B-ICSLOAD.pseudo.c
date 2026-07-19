/* =============================================================================
 * MON 60B subfunction 037B - ICSLOAD (LOAD-CONTROL-STORE)
 * Emulator model. Dispatch byte-verified; handler logic from 5P-P2-MON60.NPL
 * (the MON 60 worker source). See README.md "Byte status".
 *
 * Operator command: LOAD-CONTROL-STORE <file>,<start>,<words>
 * Purpose: load a file into the ND-500 writable control store (microcode/WCS).
 * =============================================================================
 */

/* MON 60B parameter list (Fortran-style: A -> array of param addresses).
 * Only the fields ICSLOAD touches are modelled here. */
typedef struct {
    uint16_t function;      /* params[0] = 037B for ICSLOAD                     */
    /* params[1..2] : start address / word count (used by the driver, not here)*/
    uint16_t p3_name_ptr;   /* params[3] = 5P3 : pointer to CS file-name string */
} Mon60Params;

/* MON60 per-call scratch buffer in the worker (5PIT context). */
typedef struct {
    char     name_buf[128]; /* T:=200B bytes max = 128 bytes                    */
    uint16_t function;      /* saved subfunction code                          */
} Mon60Buffer;

/* --- N500M dispatcher (byte-verified at 030416B, 050-S3I5PIT) --------------- */
int n500m_dispatch(CPU *cpu, uint16_t a_reg /* -> param list */)
{
    Mon60Params *p = (Mon60Params *) resolve_param_list(cpu, a_reg);
    uint16_t fn = p->function;               /* LDA I ,X 132 (through SSPTM)     */

    if (fn > 0177)                            /* SAT 177 ; SKP IF DT MGRE SX      */
        return mon60_error(cpu, EILFUNC);     /* illegal function                */

    /* A := 5IFUNC[fn] ; A =: P  -- jump to the param-prep handler.
     * 5IFUNC[037B] = ICSLOAD (3-way verified). */
    return icsload(cpu, p);
}

/* --- 037B ICSLOAD handler (logic from 5P-P2-MON60.NPL:ICSLOAD) -------------- */
int icsload(CPU *cpu, Mon60Params *p)
{
    Mon60Buffer *buf = mon60_buffer(cpu);

    /* A:=5P3; T:=200; CALL FRUSMOVE
     * Copy the control-store file name from user space into the MON60 buffer,
     * up to 200B (128) bytes. FRUSMOVE = "from user, move" (user -> monitor). */
    frusmove(cpu,
             /*dst*/ buf->name_buf,
             /*src user ptr*/ p->p3_name_ptr,
             /*max bytes*/ 0200);

    /* GO FAR 5NOPAR : hand off to the common system-monitor path.
     * The common path builds the 5MPM request and the RESIDENT ND-500 DRIVER
     * performs the actual control-store (WCS) write from the named file.
     * That driver code is NOT part of N500M and is not yet carved -- modelled
     * here as an opaque call. */
    return mon60_common_path(cpu, /*function*/ 037, buf);
    /* returns: skip (success) / direct + error code (per MON 60B convention). */
}

/* NOTE for the emulator control-store gate:
 * On real hardware this ends with microcode in the WCS and the ECSLOAD
 * ("control store must be loaded") condition cleared. With no real microcode
 * image (CONTROL-STORE:DATA absent, Q7), the emulator instead needs to satisfy
 * the "control store loaded" status the monitor reads -- see
 * SINTRAN/ND500/ND500-STATUS-AND-INDEX.md, control-store section. */
