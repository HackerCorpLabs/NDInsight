//Applies the __planc calling convention to every PLANC-compiled routine.
//
//WHY A SCRIPT
//  Ghidra's C signature parser does not accept a custom convention name inline - writing
//  "int __planc foo(char *p)" fails with "Can't resolve return type: int __planc". The
//  convention has to be set through Function.setCallingConvention(), which is what this does.
//
//PREREQUISITE
//  The __planc prototype must exist in the 68000 compiler spec, and Ghidra must have been
//  RESTARTED since it was added (a cspec is read only at language-load time). The script checks
//  and refuses to run otherwise.
//  Snippet + instructions: E:\Dev\Ronny\NDInsight\tools\ghidra-planc\
//
//WHAT IT SELECTS
//  A function is treated as PLANC-compiled if EITHER
//    (a) it carries the PLANC_ROUTINE tag that PlancAnnotate.java applies, OR
//    (b) its entry bytes are the standard prologue 2F 0E 2C 56 (move.l A6,-(SP); movea.l (A6),A6)
//  Hand-written leaf runtime routines (#IMU, #IDV, #APPD, #REMV) have neither, take register
//  arguments and end in a plain RTS - they are correctly left on the default convention.
//
//WHAT IT DOES NOT DO
//  It sets the CONVENTION only, not signatures. Parameter counts and types stay as they are.
//  The convention resolves the leading argument (D0 for scalars <= 32 bits, A0 as a pointer for
//  anything else) and the return value (D0 / A0). Second and later arguments live in the callee's
//  frame at 0x14 upward and cannot be expressed in a Ghidra prototype model - those stay manual.
//
//REVERSIBLE
//  Run again choosing "unknown" at the prompt to put everything back to the default convention.
//
//@author NDInsight
//@category ND.PLANC
//@keybinding
//@menupath Tools.ND PLANC.Apply __planc calling convention
//@toolbar

import ghidra.app.script.GhidraScript;
import ghidra.program.model.address.Address;
import ghidra.program.model.lang.PrototypeModel;
import ghidra.program.model.listing.Function;
import ghidra.program.model.listing.FunctionIterator;
import ghidra.program.model.mem.MemoryAccessException;

public class PlancApplyConvention extends GhidraScript {

    private static final String CONV = "__planc";

    /** move.l A6,-(SP) ; movea.l (A6),A6 - the PLANC-MC routine prologue. */
    private static final byte[] PROLOGUE = { 0x2F, 0x0E, 0x2C, 0x56 };

    @Override
    protected void run() throws Exception {

        // ---- prerequisite check -------------------------------------------------------------
        boolean present = false;
        StringBuilder available = new StringBuilder();
        for (PrototypeModel m : currentProgram.getCompilerSpec().getCallingConventions()) {
            available.append(m.getName()).append(' ');
            if (CONV.equals(m.getName())) {
                present = true;
            }
        }
        if (!present) {
            popup("The " + CONV + " calling convention is not available in this program.\n\n"
                    + "Available: " + available.toString().trim() + "\n\n"
                    + "Add the <prototype name=\"__planc\"> block to 68000.cspec in your Ghidra\n"
                    + "install, then RESTART Ghidra. A cspec is only read at language-load time.\n\n"
                    + "See tools\\ghidra-planc\\planc-68000.cspec-snippet.xml");
            println("ABORTED - " + CONV + " not available. Available: " + available);
            return;
        }
        println("=== Apply " + CONV + " ===");
        println("Available conventions: " + available.toString().trim());

        boolean revert = !askYesNo("Apply " + CONV,
                "Set the calling convention on all PLANC-compiled routines?\n\n"
                        + "Yes = set " + CONV + "\n"
                        + "No  = REVERT them to the default convention\n\n"
                        + "Signatures are not touched either way.");
        String target = revert ? "unknown" : CONV;
        println(revert ? "REVERTING to default convention." : "Applying " + CONV + ".");

        int applied = 0, already = 0, skippedLeaf = 0, failed = 0;

        FunctionIterator it = currentProgram.getListing().getFunctions(true);
        while (it.hasNext() && !monitor.isCancelled()) {
            Function f = it.next();

            if (!isPlancRoutine(f)) {
                skippedLeaf++;
                continue;
            }

            String current = f.getCallingConventionName();
            if (target.equals(current)) {
                already++;
                continue;
            }
            try {
                f.setCallingConvention(target);
                applied++;
            } catch (Exception e) {
                failed++;
                println("  failed on " + f.getName() + " @ " + f.getEntryPoint()
                        + " : " + e.getMessage());
            }
        }

        println("");
        println("=== Done ===");
        println("  convention set          : " + applied);
        println("  already correct         : " + already);
        println("  not PLANC-compiled      : " + skippedLeaf
                + "   (leaf runtime helpers, thunks, data-region artefacts)");
        println("  failed                  : " + failed);
        println("");
        println("Re-decompile a routine. in_A0 / unaff_A6 should be gone from the leading");
        println("argument and the return value should resolve. Locals will still appear as");
        println("piVar1[n] - that is the A6 frame, offset n*4, and is not fixable by a");
        println("calling convention.");
    }

    /** PLANC_ROUTINE tag, or the standard prologue at the entry point. */
    private boolean isPlancRoutine(Function f) {
        try {
            for (ghidra.program.model.listing.FunctionTag t : f.getTags()) {
                if ("PLANC_ROUTINE".equals(t.getName())) {
                    return true;
                }
            }
        } catch (Throwable ignored) {
            // Function tags unavailable on this Ghidra version - fall back to the byte test.
        }
        return hasPrologue(f.getEntryPoint());
    }

    private boolean hasPrologue(Address a) {
        try {
            byte[] buf = new byte[PROLOGUE.length];
            if (currentProgram.getMemory().getBytes(a, buf) != PROLOGUE.length) {
                return false;
            }
            for (int i = 0; i < PROLOGUE.length; i++) {
                if (buf[i] != PROLOGUE[i]) {
                    return false;
                }
            }
            return true;
        } catch (MemoryAccessException e) {
            return false;
        }
    }
}
