//Retypes each PLANC routine's frame pointer to PlancFrame*, so the decompiler shows named frame
//fields instead of anonymous piVar1[n] subscripts.
//
//This automates, across every routine, exactly what you would do by hand in the Decompiler:
//click the variable on the "piVar1 = (int *)*unaff_A6;" line, press Ctrl+L, type "PlancFrame *".
//
//THE PROBLEM
//  PLANC-MC frames are bump-allocated in a separate arena, not on A7. The prologue does
//      move.l  A6,-(SP)        save the caller's frame pointer
//      movea.l (A6),A6         A6 := *A6   - follow the next-free cursor to MY frame
//  Ghidra cannot model that as a stack frame, so every local becomes a subscript on an untyped
//  int*:
//      piVar1 = (int *)*unaff_A6;
//      piVar1[6] = (int)dstMac;          <- (0x18,A6)
//  Correct, but the offsets have to be decoded by hand every time.
//
//AFTER
//      frame->slot_18 = (int)dstMac;
//  and when you work out what a slot holds, rename the FIELD once in the Data Type Manager and
//  every routine using that offset improves at the same time.
//
//HOW IT FINDS THE VARIABLE
//  It decompiles the function and looks in the generated C for the assignment from *unaff_A6 -
//  the same line you would click on - then resolves that name to a HighSymbol and retypes it.
//  An earlier version tried to find the pcode LOAD from register A6 instead; that failed on all
//  274 routines, so the pcode route is kept only as a fallback.
//
//@author NDInsight
//@category ND.PLANC
//@keybinding
//@menupath Tools.ND PLANC.Type frame pointers
//@toolbar

import ghidra.app.decompiler.DecompInterface;
import ghidra.app.decompiler.DecompileResults;
import ghidra.app.script.GhidraScript;
import ghidra.program.model.address.Address;
import ghidra.program.model.data.CategoryPath;
import ghidra.program.model.data.DataType;
import ghidra.program.model.data.DataTypeManager;
import ghidra.program.model.data.IntegerDataType;
import ghidra.program.model.data.PointerDataType;
import ghidra.program.model.data.StructureDataType;
import ghidra.program.model.listing.Function;
import ghidra.program.model.listing.FunctionIterator;
import ghidra.program.model.pcode.HighFunction;
import ghidra.program.model.pcode.HighFunctionDBUtil;
import ghidra.program.model.pcode.HighSymbol;
import ghidra.program.model.pcode.LocalSymbolMap;
import ghidra.program.model.symbol.SourceType;

import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class PlancFrameTypes extends GhidraScript {

    private static final String STRUCT_NAME = "PlancFrame";
    private static final int SLOT_COUNT = 24;   // header + locals, 4 bytes each = 96 byte frame

    /** move.l A6,-(SP) ; movea.l (A6),A6 */
    private static final byte[] PROLOGUE = { 0x2F, 0x0E, 0x2C, 0x56 };

    /**
     * The line the decompiler emits for "A6 := *A6". Tolerates an optional cast and any
     * decompiler-invented variable name:
     *     piVar1 = (int *)*unaff_A6;
     *     puVar3 = *unaff_A6;
     */
    private static final Pattern FRAME_ASSIGN =
            Pattern.compile("(\\w+)\\s*=\\s*(?:\\([^)]*\\)\\s*)?\\*\\s*unaff_A6\\s*;");

    private DecompInterface decomp;

    @Override
    protected void run() throws Exception {

        String APPLY = "Apply   - retype frame pointers to " + STRUCT_NAME + "*";
        String REBUILD = "Rebuild - REPLACE " + STRUCT_NAME + " with this script's layout, then retype";
        String PREVIEW = "Preview - report only, change nothing";
        String REVERT = "Revert  - set frame pointers back to int*";
        java.util.List<String> choices = new java.util.ArrayList<>();
        // REBUILD is deliberately FIRST and the default. This script's definition is the source of
        // truth for the layout; keeping a stale struct in the database is never what you want, and
        // a rebuild is harmless when nothing changed. Defaulting to APPLY meant the corrected
        // layout silently never landed - the dropdown keeps its first entry unless you change it.
        choices.add(REBUILD);
        choices.add(APPLY);
        choices.add(PREVIEW);
        choices.add(REVERT);

        String choice = askChoice("PLANC frame pointer typing",
                "What should this do to " + currentProgram.getName() + " ?\n\n"
                        + "REBUILD (default) makes " + STRUCT_NAME + " match this script exactly.\n"
                        + "Apply keeps whatever struct is already in the database.",
                choices, REBUILD);

        boolean dryRun = PREVIEW.equals(choice);
        boolean revert = REVERT.equals(choice);
        boolean rebuild = REBUILD.equals(choice);

        DataType frameType = revert
                ? new PointerDataType(IntegerDataType.dataType)
                : new PointerDataType(ensureFrameStruct(dryRun, rebuild));

        println("=== PLANC frame pointer typing ===");
        println(dryRun ? "*** PREVIEW - no changes ***"
                : (revert ? "REVERTING to int*" : "Applying " + STRUCT_NAME + "*"));

        decomp = new DecompInterface();
        try {
            if (!decomp.openProgram(currentProgram)) {
                println("ABORT: could not open the decompiler: " + decomp.getLastMessage());
                return;
            }

            int done = 0, notPlanc = 0, noFramePtr = 0, failed = 0;
            int viaText = 0, viaPcode = 0;

            FunctionIterator it = currentProgram.getListing().getFunctions(true);
            while (it.hasNext() && !monitor.isCancelled()) {
                Function f = it.next();
                if (!hasPrologue(f.getEntryPoint())) {
                    notPlanc++;
                    continue;
                }
                monitor.setMessage("frame type: " + f.getName());

                DecompileResults res = decomp.decompileFunction(f, 45, monitor);
                if (res == null || !res.decompileCompleted()) {
                    noFramePtr++;
                    continue;
                }

                // --- find the variable, the same way you would by eye -----------------------
                String varName = null;
                if (res.getDecompiledFunction() != null) {
                    Matcher m = FRAME_ASSIGN.matcher(res.getDecompiledFunction().getC());
                    if (m.find()) {
                        varName = m.group(1);
                    }
                }
                HighFunction hf = res.getHighFunction();
                if (hf == null) {
                    noFramePtr++;
                    continue;
                }

                HighSymbol sym = null;
                if (varName != null) {
                    sym = findLocalByName(hf, varName);
                    if (sym != null) {
                        viaText++;
                    }
                }
                if (sym == null) {
                    sym = findViaPcode(hf);
                    if (sym != null) {
                        viaPcode++;
                    }
                }
                if (sym == null) {
                    noFramePtr++;
                    continue;
                }

                if (dryRun) {
                    println("  " + f.getName() + " @ " + f.getEntryPoint()
                            + "  ->  would retype '" + sym.getName() + "'");
                    done++;
                    continue;
                }

                try {
                    HighFunctionDBUtil.updateDBVariable(sym,
                            revert ? sym.getName() : chooseName(hf, sym),
                            frameType, SourceType.USER_DEFINED);
                    done++;
                } catch (Exception e) {
                    failed++;
                    println("  failed on " + f.getName() + " @ " + f.getEntryPoint()
                            + " : " + e.getMessage());
                }
            }

            println("");
            println("=== Done ===");
            println("  frame pointers " + (dryRun ? "found  " : "typed  ") + ": " + done
                    + "   (" + viaText + " matched in the C text, " + viaPcode + " via pcode)");
            println("  not PLANC-compiled   : " + notPlanc);
            println("  no frame ptr found   : " + noFramePtr
                    + "   (routine never dereferences its frame, or decompilation failed)");
            println("  failed               : " + failed);
            if (!dryRun && !revert && done > 0) {
                println("");
                println("Re-decompile a routine: piVar1[6] should now read frame->slot_18.");
                println("As you learn what a slot holds, rename the FIELD once in");
                println("Data Type Manager > /PLANC > " + STRUCT_NAME + " and EVERY routine");
                println("using that offset improves at the same time.");
            }
        } finally {
            decomp.dispose();
        }
    }

    /**
     * Pick a name that will not collide.
     *
     * RE-RUN DEFECT, found 2026-07-26: a second run failed on 11 functions with
     * "A Local Var symbol with name frame already exists in namespace X". The first run had named
     * one variable "frame"; on the second run the decompiler had settled on a DIFFERENT variable
     * as the frame pointer, and renaming that one to "frame" collided with the survivor from run 1.
     *
     * So: if this symbol is already called "frame", keep it (a pure retype, no rename). If some
     * OTHER local owns the name, leave this one's name alone and only change its type - a correct
     * type with an ugly name beats an exception that types nothing.
     */
    private String chooseName(HighFunction hf, HighSymbol sym) {
        if ("frame".equals(sym.getName())) {
            return "frame";
        }
        HighSymbol holder = findLocalByName(hf, "frame");
        if (holder != null && holder != sym) {
            return sym.getName();
        }
        return "frame";
    }

    /** Look up a local by the name the decompiler printed. */
    private HighSymbol findLocalByName(HighFunction hf, String name) {
        LocalSymbolMap lsm = hf.getLocalSymbolMap();
        Iterator<HighSymbol> syms = lsm.getSymbols();
        while (syms.hasNext()) {
            HighSymbol s = syms.next();
            if (name.equals(s.getName())) {
                return s;
            }
        }
        return null;
    }

    /** Fallback: the output of a LOAD whose address operand mentions A6. */
    private HighSymbol findViaPcode(HighFunction hf) {
        Iterator<ghidra.program.model.pcode.PcodeOpAST> ops = hf.getPcodeOps();
        while (ops.hasNext()) {
            ghidra.program.model.pcode.PcodeOpAST op = ops.next();
            if (op.getOpcode() != ghidra.program.model.pcode.PcodeOp.LOAD) {
                continue;
            }
            ghidra.program.model.pcode.Varnode addr = op.getInput(1);
            ghidra.program.model.pcode.Varnode out = op.getOutput();
            if (addr == null || out == null) {
                continue;
            }
            ghidra.program.model.pcode.HighVariable ahv = addr.getHigh();
            if (ahv == null || ahv.getName() == null || !ahv.getName().contains("A6")) {
                continue;
            }
            ghidra.program.model.pcode.HighVariable ohv = out.getHigh();
            if (ohv != null && ohv.getSymbol() != null) {
                return ohv.getSymbol();
            }
        }
        return null;
    }

    private DataType ensureFrameStruct(boolean dryRun, boolean rebuild) {
        DataTypeManager dtm = currentProgram.getDataTypeManager();
        CategoryPath cat = new CategoryPath("/PLANC");
        DataType existing = dtm.getDataType(cat, STRUCT_NAME);
        if (existing != null && !rebuild) {
            println("  using existing " + STRUCT_NAME + " (" + existing.getLength() + " bytes)");
            println("  NOTE: field names/offsets come from that EXISTING type, not from this");
            println("        script. If it predates 2026-07-26 it still has +0x04 as 'frameLimit'");
            println("        and a 32-bit errcode, both of which are wrong. Choose Rebuild.");
            return existing;
        }
        StructureDataType s = new StructureDataType(cat, STRUCT_NAME, 0, dtm);
        s.add(new PointerDataType(), 4, "stp_nextFree",
                "+0x00 STP - next-free cursor; the prologue follows this to reach this frame");
        // VERIFIED 2026-07-26 and it is NOT what either prior source said. ND-820026.1 Figure 8
        // calls +4 "Unused - reserved for future extension"; this script previously called it
        // "frameLimit / overflow guard". Both wrong. It points at the frame the NEXT call will
        // run in: a caller stages arguments through it and the callee reads them back at the
        // same offsets off its own A6. Proven at two levels - 0x31D4 writes (0x12,A1) with
        // A1 = (0x4,A6) and callee 0x28E6 reads (0x12,A6); 0x28E6 in turn writes (0x12,A2) and
        // (0x1A,A2) and its callee 0x40BE reads exactly those two offsets.
        s.add(new PointerDataType(), 4, "outgoingFrame",
                "+0x04 pointer to the CALLEE's frame - the caller stages arguments through this");
        s.add(new PointerDataType(), 4, "smax_stackTop",
                "+0x08 SMAX - top of the free stack. Duplicates A7 for the CURRENT stack; it is "
                        + "a separate field because several stacks are in use (per-process)");
        s.add(IntegerDataType.dataType, 4, "syst", "+0x0C PLANC runtime system use");
        // ERRCODE is a WORD, not a longword: every ON ROUTINEERROR handler stores it with
        // "move.w D0w,(0x10,A6)" (e.g. 0x2EAE). It has to be 2 bytes for the first parameter to
        // land at +0x12, which is where callers demonstrably write it.
        s.add(ghidra.program.model.data.ShortDataType.dataType, 2, "errcode",
                "+0x10 ERRCODE (16-bit) - ON ROUTINEERROR handlers store D0w here");
        // Parameters start at +0x12, NOT the manual's 24B octal (+0x14) - consistent across every
        // call site examined. +0x12 is 2-byte aligned only, hence the word-sized first slot.
        s.add(ghidra.program.model.data.ShortDataType.dataType, 2, "slot_12",
                "+0x12 FIRST PARAMETER - callers write here via the caller's outgoingFrame");
        for (int off = 0x14; off < SLOT_COUNT * 4; off += 4) {
            s.add(IntegerDataType.dataType, 4, String.format("slot_%02x", off),
                    "+0x" + Integer.toHexString(off)
                            + " parameter or local - rename once its role is known");
        }
        s.setDescription("PLANC-MC routine frame, A6-relative. Bump-allocated in a separate arena, "
                + "never popped - the manual's 'the stack grows both upwards and downwards'. "
                + "Parameters from +0x12 (NOT the manual's +0x14). Header per ND-820026.1 Figure 8 "
                + "(compiler version H+), EXCEPT +0x04, which the manual calls unused but which "
                + "this image uses as the outgoing-argument frame pointer - verified from the "
                + "caller/callee offset match at 0x31D4/0x28E6 and 0x28E6/0x40BE.");
        if (dryRun) {
            println("  would " + (existing == null ? "create " : "REBUILD ") + STRUCT_NAME);
            return s;
        }
        if (existing != null) {
            // Replace in place so every variable already typed PlancFrame* follows the new layout.
            // replaceDataType throws a CHECKED DataTypeDependencyException if the outgoing type
            // participates in a dependency cycle.
            try {
                DataType rebuilt = dtm.replaceDataType(existing, s, true);
                println("  REBUILT " + STRUCT_NAME + " (" + rebuilt.getLength() + " bytes)"
                        + " - every variable already typed with it now uses the corrected layout");
                return rebuilt;
            } catch (ghidra.program.model.data.DataTypeDependencyException e) {
                println("  CANNOT rebuild " + STRUCT_NAME + ": " + e.getMessage());
                println("    Falling back to the existing type - the layout is still the OLD one.");
                return existing;
            }
        }
        DataType added = dtm.addDataType(s, null);
        println("  created " + STRUCT_NAME + " (" + added.getLength() + " bytes)");
        return added;
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
        } catch (Exception e) {
            return false;
        }
    }
}
