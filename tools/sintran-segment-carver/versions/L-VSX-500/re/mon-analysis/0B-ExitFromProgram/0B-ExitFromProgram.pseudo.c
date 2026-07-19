/* ============================================================================
 * MON 0B  ExitFromProgram (LEAVE)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Terminates the calling program and returns to
 * SINTRAN III.  A batch job continues with its next command.
 *
 * Dispatch reality:
 *   GOTAB[0B] = 000000 -> FALL-THROUGH (BYTE-VERIFIED).  There is no direct
 *   GOTAB handler word, so the level-14 handler is reached through the resident
 *   MFELL/CALLPROC path - NOT present in any carved segment (uncarved bridge).
 *   LEAVE @144142B (resident commoncode) is the NAMED region for this call, but
 *   in this real SINTRAN L image its word is ZERO: LEAVE lies inside the large
 *   uncarved zero block 103031B..170177B.  So the termination worker body cannot
 *   be read from these bytes; the model below is of the DOCUMENTED behaviour only
 *   (from the manual), NOT of carved code.  Code-vs-data for LEAVE is
 *   NOT-RECOVERABLE - see README caveats.
 * Addresses in comments are octal.
 * ============================================================================ */

/* Documented model (NOT carved: LEAVE region is zero-filled in this L image).
 * MON 0 takes no parameters.  It terminates the current program and returns
 * control to SINTRAN III; the real worker body is not byte-recoverable here. */
void mon_0B_ExitFromProgram(mon_regs *r)   /* no parameters */
{
    /* Documented behaviour (SINTRAN III Monitor Calls, ND-860228.2 EN, p.185):
     *
     *   - If this is a BACKGROUND program: close every file that is not set
     *     permanently open.
     *   - If this is an RT program: close no files, but release all reserved
     *     devices.
     *   - Return control to SINTRAN III.  A batch job continues with its next
     *     command; an interactive background program returns to the command
     *     processor (the terminal command prompt). */

    if (is_background_program(r)) {
        close_non_permanent_files(r);        /* background: close scratch/open files */
    } else {  /* RT program */
        release_all_reserved_devices(r);     /* RT: release reserved devices          */
    }

    return_to_sintran_command_processor(r);  /* never returns to the caller           */
    /* (does not fall through; control passes to SINTRAN / next batch command) */
}

/* Caveats for the emulator author:
 *   - GOTAB[0B]=000000 (fall-through) is BYTE-VERIFIED; there is no entry stub to
 *     model.  Dispatch enters the resident MFELL/CALLPROC (UNCARVED).
 *   - LEAVE=144142B is a single zero word in this carved L image (it sits inside
 *     the uncarved zero block 103031B..170177B), so NONE of the body above is
 *     byte-derived - it is the manual's documented behaviour only.  The worker is
 *     named-only / NOT-RECOVERABLE from these bytes.
 *   - A live PC trace (break on a real MON 0, single-step the fall-through and
 *     CALLPROC) is needed to locate and confirm the real termination worker. */
