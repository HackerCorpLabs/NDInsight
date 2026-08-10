//Types the 68000 exception vector table at 0x000-0x3FF as 256 pointers, labels every entry with its
//architectural name, and comments each one.
//
//WHY
//  Ghidra leaves the vector table as raw bytes or as arbitrary data, so:
//    - the reset PC and every interrupt handler are invisible
//    - no references exist from the vectors to their handlers, so the handlers look unreferenced
//      and may never be disassembled at all
//  On the ENCOS image this mattered: the PIOC-OS kernel at 0x3498 is reachable only via TRAP #2,
//  and a 13KB code bank had never been disassembled because nothing pointed into it.
//
//  Making the vectors POINTERS creates the references, which makes the handlers visible in the
//  symbol tree and in xrefs.
//
//AFTER RUNNING
//  Look at 0x004 (reset PC) first - that is the firmware entry point. Then the autovectors
//  (0x064-0x07C), the TRAPs (0x080-0x0BC) and the user vectors (0x100-0x3FC).
//  Vectors that share a target, or point into a dense run of 2-byte stubs, are unused - each stub
//  exists only so the stub ADDRESS encodes which vector fired.
//
//SAFETY
//  Clears and retypes only 0x000-0x3FF. Reversible: choose Revert to clear the range back to
//  undefined bytes. Ctrl+Z undoes the whole run.
//  If the table region is genuinely something else on your target, PREVIEW first - the report shows
//  every value before anything is written.
//
//@author NDInsight
//@category ND.PLANC
//@keybinding
//@menupath Tools.ND PLANC.Type 68000 vector table
//@toolbar

import ghidra.app.script.GhidraScript;
import ghidra.program.model.address.Address;
import ghidra.program.model.data.PointerDataType;
import ghidra.program.model.listing.CodeUnit;
import ghidra.program.model.listing.Listing;
import ghidra.program.model.mem.Memory;
import ghidra.program.model.symbol.SourceType;

public class M68kVectorTable extends GhidraScript {

    private static final int TABLE_START = 0x000;
    private static final int VECTOR_COUNT = 256;

    @Override
    protected void run() throws Exception {
        Listing listing = currentProgram.getListing();
        Memory mem = currentProgram.getMemory();

        String APPLY = "Apply   - type all 256 vectors as pointers, label and comment them";
        String PREVIEW = "Preview - report the decoded table, change nothing";
        String REVERT = "Revert  - clear 0x000-0x3FF back to undefined bytes";
        java.util.List<String> choices = new java.util.ArrayList<>();
        choices.add(APPLY);
        choices.add(PREVIEW);
        choices.add(REVERT);
        String choice = askChoice("68000 exception vector table",
                "Type the vector table at 0x000-0x3FF in " + currentProgram.getName() + " ?",
                choices, APPLY);

        boolean preview = PREVIEW.equals(choice);
        boolean revert = REVERT.equals(choice);

        Address start = toAddr(TABLE_START);
        Address end = toAddr(TABLE_START + VECTOR_COUNT * 4 - 1);

        if (revert) {
            // Read the pointers BEFORE clearing, so we can also strip the handler labels this
            // script scattered across the image - otherwise Revert would leave them orphaned.
            int purged = 0;
            for (int v = 0; v < VECTOR_COUNT; v++) {
                Address at = toAddr(TABLE_START + v * 4);
                purged += purgeOurLabels(at);
                try {
                    long tv = ((long) mem.getInt(at)) & 0xFFFFFFFFL;
                    if (tv != 0 && tv < 0x80000L && (tv & 1L) == 0) {
                        purged += purgeOurLabels(toAddr(tv));
                    }
                } catch (Exception e) {
                    // unreadable slot - nothing to purge at a target we cannot find
                }
            }
            listing.clearCodeUnits(start, end, false);
            println("Cleared " + start + " - " + end + " back to undefined bytes.");
            println("Removed " + purged + " labels written by this script (slots and handlers).");
            return;
        }

        println("=== 68000 exception vector table ===");
        if (preview) {
            println("*** PREVIEW - nothing will be written ***");
        }

        if (!preview) {
            // Must clear first - anything previously laid down here (strings, wrong-sized data,
            // stray instructions) will block createData.
            listing.clearCodeUnits(start, end, false);

            // Overview at the head of the table, so the map is visible without reading 256 EOLs.
            listing.setComment(start, CodeUnit.PLATE_COMMENT,
                    "68000 EXCEPTION VECTOR TABLE  -  0x000-0x3FF, 256 longword entries\n"
                    + "\n"
                    + "  vec   0        0x000   initial supervisor stack pointer (NOT a handler)\n"
                    + "  vec   1        0x004   RESET PC - the firmware entry point\n"
                    + "  vec   2-11     0x008   processor faults: bus/address error, illegal instr,\n"
                    + "                         divide by zero, CHK, TRAPV, privilege, trace, A/F line\n"
                    + "  vec  12-23     0x030   reserved by Motorola\n"
                    + "  vec  15        0x03C   uninitialised interrupt - device vector never set\n"
                    + "  vec  24        0x060   spurious interrupt - no device claimed the IRQ\n"
                    + "  vec  25-31     0x064   autovectors, IRQ level 1..7 (7 = non-maskable)\n"
                    + "  vec  32-47     0x080   TRAP #0 .. TRAP #15 software calls\n"
                    + "  vec  48-63     0x0C0   reserved (coprocessor/FP on later parts)\n"
                    + "  vec  64-255    0x100   device-supplied interrupt vectors\n"
                    + "\n"
                    + "Read the reset PC at 0x004 first - it is the entry point of the whole image.\n"
                    + "Handlers that are still undefined bytes after this script runs are routines\n"
                    + "nothing else references, which is exactly where dark code hides.\n"
                    + "\n"
                    + "This table can ALSO be patched at run time, so a live card may differ from\n"
                    + "what is in the image.");
        }

        int typed = 0, failed = 0, nonZero = 0;
        java.util.Map<Long, Integer> targetCount = new java.util.HashMap<>();
        long[] vectorValue = new long[VECTOR_COUNT];

        for (int v = 0; v < VECTOR_COUNT; v++) {
            if (monitor.isCancelled()) {
                break;
            }
            Address at = toAddr(TABLE_START + v * 4);
            long value;
            try {
                value = ((long) (mem.getInt(at))) & 0xFFFFFFFFL;
            } catch (Exception e) {
                failed++;
                continue;
            }
            vectorValue[v] = value;
            if (value != 0) {
                nonZero++;
                Integer c = targetCount.get(Long.valueOf(value));
                targetCount.put(Long.valueOf(value), Integer.valueOf(c == null ? 1 : c.intValue() + 1));
            }

            String name = vectorName(v);
            String label = slotLabel(v, value);

            if (preview) {
                if (v < 32 || value != 0) {
                    println(String.format("  %s  %-38s = 0x%08X", at, label, value));
                }
                continue;
            }

            try {
                listing.createData(at, new PointerDataType());
                setSoleLabel(at, label);
                listing.setComment(at, CodeUnit.EOL_COMMENT,
                        String.format("vector %d - %s", v, describe(v, name)));
                typed++;
            } catch (Exception e) {
                failed++;
                println("  failed at " + at + " (vector " + v + "): " + e.getMessage());
            }
        }

        // ------------------------------------------------------------------ name the HANDLERS
        // Each vector's target gets a name derived from the vector that reaches it, but ONLY if it
        // currently carries a default label (LAB_/FUN_/SUB_/DAT_) or none at all. Anything already
        // meaningfully named - PiocOsTrap2Dispatch, nd_host_interrupt_handler, the RTC ISR - is left
        // alone and reported, because a name earned by analysis beats one derived from a table slot.
        int named = 0, keptExisting = 0, disasm = 0, sharedSkipped = 0;
        java.util.Set<Long> alreadyHandled = new java.util.HashSet<>();

        for (int v = 0; v < VECTOR_COUNT && !monitor.isCancelled(); v++) {
            long value = vectorValue[v];
            if (v == 0 || value == 0) {
                continue;   // vector 0 is the stack pointer, not a handler
            }
            if (value >= 0x80000L || (value & 1L) != 0) {
                continue;   // outside the image, or odd - not a code address
            }
            if (!alreadyHandled.add(Long.valueOf(value))) {
                sharedSkipped++;
                continue;   // a lower-numbered vector already named this target
            }

            Address target = toAddr(value);
            ghidra.program.model.symbol.Symbol sym =
                    currentProgram.getSymbolTable().getPrimarySymbol(target);
            String existing = (sym == null) ? null : sym.getName();

            if (!isReplaceable(existing)) {
                keptExisting++;
                if (!preview) {
                    println("  vector " + v + " -> " + target + " keeps its existing name '"
                            + existing + "'");
                }
                continue;
            }

            // Is this target inside a run of 2-byte stubs? If the neighbouring even addresses are
            // also vector targets, it is one of the per-vector stubs whose ADDRESS encodes which
            // vector fired - not a real routine.
            boolean inStubRun = targetCount.containsKey(Long.valueOf(value - 2))
                    || targetCount.containsKey(Long.valueOf(value + 2));

            String hname = handlerName(v, inStubRun);
            if (preview) {
                println(String.format("    would name 0x%08X -> %s%s", value, hname,
                        inStubRun ? "   (2-byte stub run)" : ""));
                named++;
                continue;
            }

            try {
                setSoleLabel(target, hname);
                listing.setComment(target, CodeUnit.EOL_COMMENT,
                        "Reached from " + describe(v, vectorName(v))
                        + (inStubRun
                           ? "  This sits in a run of 2-byte per-vector stubs; the stub ADDRESS is"
                             + " what identifies the vector to the shared handler."
                           : ""));
                named++;
                if (listing.getInstructionAt(target) == null
                        && listing.getDefinedDataAt(target) == null) {
                    ghidra.app.cmd.disassemble.DisassembleCommand dc =
                            new ghidra.app.cmd.disassemble.DisassembleCommand(target, null, true);
                    dc.applyTo(currentProgram, monitor);
                    disasm++;
                }
            } catch (Exception e) {
                println("  could not name " + target + ": " + e.getMessage());
            }
        }

        println("");
        println("=== Done ===");
        println("  vectors " + (preview ? "decoded" : "typed as pointers") + " : "
                + (preview ? VECTOR_COUNT : typed));
        println("  non-zero                     : " + nonZero);
        println("  failed                       : " + failed);
        println("  handlers " + (preview ? "to name" : "named") + "        : " + named);
        println("  handlers keeping their name  : " + keptExisting
                + "   (already analysed - not overwritten)");
        println("  duplicate targets skipped    : " + sharedSkipped);
        println("  handler sites disassembled   : " + disasm);

        // Shared targets are the tell for unused vectors pointing at a common stub run.
        println("");
        println("Targets referenced by more than one vector (usually a shared default handler):");
        int shown = 0;
        for (java.util.Map.Entry<Long, Integer> e : targetCount.entrySet()) {
            if (e.getValue().intValue() > 1) {
                println(String.format("    0x%08X  <- %d vectors", e.getKey(), e.getValue()));
                shown++;
            }
        }
        if (shown == 0) {
            println("    (none - every vector has a distinct target)");
        }

        if (!preview) {
            println("");
            println("The vectors are now POINTERS, so Ghidra has created references to every");
            println("handler. Check the reset PC at 0x004 - that is the firmware entry point -");
            println("and look for handlers that are still undefined bytes: those are routines");
            println("nothing else reaches, which is exactly where dark code hides.");
        }
    }

    /**
     * Give an address EXACTLY ONE label: ours.
     *
     * createLabel() ADDS a label, it does not replace one. Ghidra happily keeps every label ever
     * put on an address and just marks one primary, so re-running this script used to leave the
     * previous run's names stacked underneath the new one - VEC_004_IllegalInstruction and
     * Vec004_IllegalInstruction and VecStub_004 all sitting on the same address, with no way to
     * tell which one is current. So: delete every label we are entitled to delete, then create.
     *
     * "Entitled to delete" is deliberately narrow - see isReplaceable(). A name a human typed, or
     * that another analysis script earned, is never touched; if one is present we leave it alone
     * AND add ours as a secondary label rather than fighting over the address.
     */
    private void setSoleLabel(Address a, String name) throws Exception {
        ghidra.program.model.symbol.SymbolTable st = currentProgram.getSymbolTable();
        boolean foreignNamePresent = false;

        ghidra.program.model.symbol.Symbol[] syms = st.getSymbols(a);
        for (int i = 0; i < syms.length; i++) {
            ghidra.program.model.symbol.Symbol s = syms[i];
            // Never delete a function's own symbol or anything imported/analysed into place -
            // deleting those can remove the function, not just its name.
            if (s.getSymbolType() != ghidra.program.model.symbol.SymbolType.LABEL) {
                continue;
            }
            if (s.getSource() == SourceType.DEFAULT) {
                continue;   // LAB_xxxx is generated on demand; it disappears by itself
            }
            String n = s.getName();
            if (name.equals(n)) {
                s.delete();          // our own current name - recreated below, keeps it primary
            } else if (isOurOldOutput(n)) {
                s.delete();          // a previous run of THIS script - the duplicate being complained about
                println("    removed stale label '" + n + "' at " + a);
            } else {
                foreignNamePresent = true;
            }
        }
        createLabel(a, name, !foreignNamePresent, SourceType.USER_DEFINED);
    }

    /** Delete every label at this address that an earlier run of this script wrote. */
    private int purgeOurLabels(Address a) {
        int n = 0;
        ghidra.program.model.symbol.Symbol[] syms = currentProgram.getSymbolTable().getSymbols(a);
        for (int i = 0; i < syms.length; i++) {
            ghidra.program.model.symbol.Symbol s = syms[i];
            if (s.getSymbolType() == ghidra.program.model.symbol.SymbolType.LABEL
                    && s.getSource() != SourceType.DEFAULT
                    && isOurOldOutput(s.getName())) {
                s.delete();
                n++;
            }
        }
        return n;
    }

    /**
     * Was this name written by an earlier run of this script? Those are ours to overwrite; they
     * carry no information we are not about to write again, more accurately.
     *
     * Covers every naming scheme this script has ever used, including the ones that were wrong:
     *     VEC_004_IllegalInstruction   original slot label
     *     Vec004_IllegalInstruction    current slot label
     *     VecStub_004                  the old, useless handler name
     *     UserVec100_Handler           the old device-vector handler name
     *     IllegalInstruction_Stub      current handler names
     */
    private boolean isOurOldOutput(String n) {
        if (n.startsWith("VEC_") || n.startsWith("VecStub_") || n.startsWith("UserVec")) {
            return true;
        }
        if (n.matches("Vec\\d{3}_.*")) {
            return true;
        }
        // A _Handler/_Stub suffix alone is NOT enough - somebody's hand-written "Timer_Handler"
        // must survive. Only names whose base this script can actually generate are ours.
        if (n.endsWith("_Handler") || n.endsWith("_Stub")) {
            String base = n.substring(0, n.lastIndexOf('_'));
            return ourBaseNames().contains(base);
        }
        return false;
    }

    /** Every base name baseHandlerName() can produce, for all 256 vectors. */
    private java.util.Set<String> ourBaseNames() {
        if (ourBases == null) {
            ourBases = new java.util.HashSet<>();
            for (int v = 0; v < VECTOR_COUNT; v++) {
                ourBases.add(baseHandlerName(v));
            }
        }
        return ourBases;
    }

    private java.util.Set<String> ourBases;

    /**
     * May this script name the target? Yes if it is unnamed, if it carries a Ghidra default name
     * (LAB_/FUN_/SUB_/DAT_/UNK_ - the address spelled differently), or if it carries THIS script's
     * own previous output. No if a human or another analysis pass named it, because a name earned
     * by analysis beats one derived from a table slot.
     */
    private boolean isReplaceable(String n) {
        if (n == null) {
            return true;
        }
        return n.startsWith("LAB_") || n.startsWith("FUN_") || n.startsWith("SUB_")
                || n.startsWith("DAT_") || n.startsWith("UNK_") || isOurOldOutput(n);
    }

    /**
     * Label for the TABLE SLOT itself (the four bytes at 0x000 + 4*v).
     *
     * A slot label has one job: tell you, without following the pointer, what fires this entry and
     * where it goes. So it carries three things -
     *     Vec034_TRAP2_to_PiocOsTrap2Dispatch
     *     |      |         |
     *     |      |         +-- where it points, but ONLY when that target already has a name
     *     |      |             earned by analysis. A LAB_/FUN_/DAT_ target adds nothing, so it
     *     |      |             is omitted rather than pasted in as noise.
     *     |      +------------ what raises it, in the same vocabulary as the handler name, so the
     *     |                    slot and its target read as an obvious pair
     *     +------------------- the vector NUMBER, which is what a device actually puts on the bus
     *                          during interrupt acknowledge and what you match against a datasheet
     *
     * Sorting by name therefore also sorts by vector number, which is why the number is
     * zero-padded and comes first.
     */
    private String slotLabel(int v, long value) {
        if (v == 0) {
            return "Vec000_InitialSupervisorStackPointer";   // a value, not a pointer
        }
        String base = baseHandlerName(v);
        String target = meaningfulNameAt(value);
        return (target == null)
                ? String.format("Vec%03d_%s", v, base)
                : String.format("Vec%03d_%s_to_%s", v, base, target);
    }

    /**
     * The symbol at an address IF somebody has actually named it. Ghidra's auto-generated
     * LAB_/FUN_/SUB_/DAT_/UNK_ names are just the address spelled differently, so they count as
     * "unnamed" here - pasting LAB_00007c16 into a vector label would make it longer without
     * making it say anything.
     */
    private String meaningfulNameAt(long value) {
        if (value == 0 || value >= 0x80000L || (value & 1L) != 0) {
            return null;   // zero, outside the image, or odd: not a code address
        }
        try {
            ghidra.program.model.symbol.Symbol s =
                    currentProgram.getSymbolTable().getPrimarySymbol(toAddr(value));
            if (s == null) {
                return null;
            }
            // A default name, or one of ours from a previous run, adds nothing to a slot label.
            return isReplaceable(s.getName()) ? null : s.getName();
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Name for the ROUTINE a vector points at.
     *
     * ALWAYS says which exception reaches it - that is the useful fact. Whether the target happens
     * to be a shared default stub is a secondary detail and only changes the SUFFIX, never the
     * name. "IllegalInstruction_Stub" tells you something; "VecStub_004" tells you nothing.
     */
    private String handlerName(int v, boolean inStubRun) {
        return baseHandlerName(v) + (inStubRun ? "_Stub" : "_Handler");
    }

    /** What this vector IS, independent of what its target looks like. */
    private String baseHandlerName(int v) {
        switch (v) {
            case 1:  return "ResetFirmwareEntry";   // not "Reset" - this IS the image entry point
            case 2:  return "BusError";
            case 3:  return "AddressError";
            case 4:  return "IllegalInstruction";
            case 5:  return "DivideByZero";
            case 6:  return "Chk";
            case 7:  return "Trapv";
            case 8:  return "PrivilegeViolation";
            case 9:  return "Trace";
            case 10: return "Line1010Emulator";
            case 11: return "Line1111Emulator";
            case 14: return "FormatError";
            case 15: return "UninitialisedInterrupt";
            case 24: return "SpuriousInterrupt";
            default: break;
        }
        if (v >= 25 && v <= 31) {
            int level = v - 24;
            return (level == 7) ? "IRQ7_NMI" : ("IRQ" + level);
        }
        if (v >= 32 && v <= 47) {
            return "TRAP" + (v - 32);
        }
        if ((v >= 12 && v <= 23) || (v >= 48 && v <= 63)) {
            return String.format("ReservedVec%03d", v);
        }
        // Device-supplied vectors. Where this project has identified the device, say so; otherwise
        // say the one thing that IS known - a device supplied this number during interrupt
        // acknowledge - and keep the number, since that is what you look up in the datasheet.
        switch (v) {
            case 69: return "RtcMfpTimerTick";
            case 78: return "Nd100HostDoorbell";
            default: return String.format("DeviceIrq%03d", v);
        }
    }

    /** Architectural name for a 68000 exception vector. */
    private String vectorName(int v) {
        switch (v) {
            case 0:  return "InitialSSP";
            case 1:  return "ResetPC";
            case 2:  return "BusError";
            case 3:  return "AddressError";
            case 4:  return "IllegalInstruction";
            case 5:  return "DivideByZero";
            case 6:  return "CHK";
            case 7:  return "TRAPV";
            case 8:  return "PrivilegeViolation";
            case 9:  return "Trace";
            case 10: return "Line1010Emulator";
            case 11: return "Line1111Emulator";
            case 14: return "FormatError";
            case 15: return "UninitialisedInterrupt";
            case 24: return "SpuriousInterrupt";
            default:
                if (v >= 25 && v <= 31) {
                    return "Autovector_IRQ" + (v - 24);
                }
                if (v >= 32 && v <= 47) {
                    return "TRAP_" + (v - 32);
                }
                if (v >= 12 && v <= 23) {
                    return "Reserved";
                }
                if (v >= 48 && v <= 63) {
                    return "Reserved";
                }
                return "UserInterrupt";
        }
    }

    /**
     * What actually causes this exception, plus anything specific to the ENCOS Ethernet II image.
     * The architectural text is from the MC68000 User's Manual exception model; the ENCOS notes are
     * from this project's reverse engineering and are marked as such.
     */
    private String describe(int v, String name) {
        switch (v) {
            case 0:
                return "INITIAL SSP - not a handler. The supervisor stack pointer loaded at reset. "
                        + "The stack grows DOWNWARD from this address.";
            case 1:
                return "RESET PC - *** THE FIRMWARE ENTRY POINT ***. Execution begins here after "
                        + "reset, in supervisor mode with interrupts masked at level 7.";
            case 2:
                return "BUS ERROR - the bus signalled a fault: no DTACK within the timeout, or an "
                        + "access to unmapped/unpopulated memory. On this board a likely cause is "
                        + "touching absent DRAM or a missing peripheral.";
            case 3:
                return "ADDRESS ERROR - a word or long access at an ODD address, or an instruction "
                        + "fetch from an odd address. The classic symptom of a corrupted pointer.";
            case 4:
                return "ILLEGAL INSTRUCTION - the opcode is not a valid 68000 instruction. Usually "
                        + "means execution wandered into data.";
            case 5:
                return "DIVIDE BY ZERO - DIVU or DIVS with a zero divisor. Note the PLANC runtime "
                        + "divide helper #IDV does its own checking, so reaching this is a bug.";
            case 6:
                return "CHK - the CHK instruction found its operand outside the given bounds. "
                        + "Compilers use CHK for array subscript range checks.";
            case 7:
                return "TRAPV - the TRAPV instruction executed with the overflow flag set, i.e. a "
                        + "signed arithmetic overflow the code chose to trap on.";
            case 8:
                return "PRIVILEGE VIOLATION - a supervisor-only instruction attempted in user mode "
                        + "(e.g. writing SR, RESET, STOP, RTE, or a MOVE USP).";
            case 9:
                return "TRACE - single-step. Taken after each instruction while the T bit in SR is "
                        + "set. Used by a debug monitor, not in normal operation.";
            case 10:
                return "LINE 1010 EMULATOR - opcode beginning 1010 (0xA000-0xAFFF). Unimplemented "
                        + "on the 68000 and reserved for software emulation traps.";
            case 11:
                return "LINE 1111 EMULATOR - opcode beginning 1111 (0xF000-0xFFFF). Reserved for "
                        + "coprocessor instructions; unimplemented on a plain 68000.";
            case 14:
                return "FORMAT ERROR - RTE found an invalid stack frame format. 68010 and later; "
                        + "should not occur on a 68000.";
            case 15:
                return "UNINITIALISED INTERRUPT - a peripheral was interrupt-acknowledged but "
                        + "supplied vector 15, meaning its vector register was never programmed. "
                        + "Seeing this at runtime points at a device that was not initialised.";
            case 24:
                return "SPURIOUS INTERRUPT - a bus error occurred during the interrupt acknowledge "
                        + "cycle, i.e. an interrupt was asserted but no device claimed it.";
            default:
                break;
        }
        if (v >= 12 && v <= 23) {
            return "RESERVED by Motorola - should never be taken. If this fires, something is very "
                    + "wrong.";
        }
        if (v >= 25 && v <= 31) {
            int level = v - 24;
            String extra = (level == 7)
                    ? " Level 7 is NON-MASKABLE and edge-triggered."
                    : " Taken when a device asserts IPL" + level + " and does NOT supply its own "
                      + "vector (autovectored).";
            return "AUTOVECTOR IRQ level " + level + "." + extra;
        }
        if (v >= 32 && v <= 47) {
            int t = v - 32;
            String extra = "";
            if (t == 2) {
                extra = " [ENCOS] This is the PIOC-OS KERNEL ENTRY - every kernel service is a "
                        + "'trap #2' with the function code in D0 and an argument block in A0. "
                        + "The dispatcher bounds-checks D0 to 0..0x1A and calls through "
                        + "tbl_piocOsTrap2FunctionDispatch.";
            } else if (t == 0) {
                extra = " [ENCOS] Points at the same handler as TRAP #2.";
            }
            return "TRAP #" + t + " - taken by the 'trap #" + t + "' instruction. Software "
                    + "supervisor call." + extra;
        }
        if (v >= 48 && v <= 63) {
            return "RESERVED - coprocessor and floating-point vectors on later 680x0 parts; "
                    + "unused on a 68000.";
        }
        // User / device-supplied vectors 64..255
        String extra = "";
        if (v == 69) {
            extra = " [ENCOS] The RTC / MFP timer tick. Drives preemptive scheduling and bumps the "
                    + "tick counters at 0x0FC2 / 0x0FCA.";
        } else if (v == 78) {
            extra = " [ENCOS] The ND-100 host doorbell. Statically points at the default handler, "
                    + "but POMNPROCES saves that to 0x199E8 at runtime and installs its own handler "
                    + "in front, which chains back with 'move.l (0x199E8).l,-(SP) ; rts'.";
        }
        return "USER VECTOR " + v + " - supplied by the interrupting device during the interrupt "
                + "acknowledge cycle. Table offset 0x" + Integer.toHexString(v * 4) + "." + extra;
    }
}
