//Fixes PLANC-MC (MC68000) skip-return control flow so Ghidra's decompiler produces sane output.
//
//BACKGROUND
//  ND's PLANC-MC compiler emits a "skip return": a routine that terminates normally returns to
//  RETLINK+2, skipping the two bytes immediately after the call. Those two bytes are the ERROR
//  path. In this firmware the slot contains, variously:
//      4E D5   jmp (A5)        - jump to the runtime unwind routine #XRET (the classic form)
//      60 xx   bra.s <handler> - branch to a local ON ROUTINEERROR handler
//      61 xx   bsr.s <handler> - call a local handler
//      4E 71   nop             - no handler at all
//  The defining property is that the slot is EXACTLY TWO BYTES. That is what is skipped.
//
//    ND-820026.1 EN DOMINO and NUCLEUS Software Guide, line 3432:
//      "If the routine terminates normally (not ERRETURN), this address is incremented by two
//       (bytes) when returning (also called skip return)."
//    ND-60.117.5 EN PLANC Reference Manual, line 12390:
//      "normal return jumps back to LINK + 2"
//
//  The compiled epilogue is:  movea.l (SP)+,A6 ; movea.l (SP)+,A2 ; jmp (0x2,A2)
//  bytes                      2C 5F              24 5F              4E EA 00 02
//
//WHAT GHIDRA GETS WRONG WITHOUT THIS SCRIPT
//  1. It falls through from a call into the 2-byte error slot. When that slot is a BRA it follows
//     the branch instead, so the real normal-path code at RETLINK+2 is NEVER DISASSEMBLED and is
//     left as raw bytes.
//  2. It treats "jmp (0x2,A2)" as an indirect CALL, producing
//     "(**(code **)(in_stack_00000000 + 2))()" and "Could not recover jumptable" warnings
//     instead of a return.
//
//WHAT THIS SCRIPT DOES (flow only - it does not rename, retype, or set prototypes)
//  Repeats these passes until nothing changes (newly disassembled code contains new calls, which
//  have their own error slots - one pass is not enough):
//    Pass 1  Find every skip-return epilogue in the whole program.
//    Pass 2  For every call that skip-returns, override the fallthrough to skip the 2-byte slot.
//    Pass 3  Mark every epilogue as a RETURN via FlowOverride.
//    Pass 4  Disassemble the newly reachable normal-path code, clearing bad instructions first.
//
//SAFETY
//  - Conservative. A fallthrough is only overridden when EITHER the slot is literally jmp (A5)
//    (unambiguous), OR the callee is proven to use the +2 epilogue and the slot is a 2-byte
//    instruction. Anything else is reported, not changed. Leaf runtime routines that end in a
//    plain RTS (#IMU, #IDV, #APPD, #REMV) are correctly left alone.
//  - Reversible. Run PlancUndoFixFlow.java, or Ctrl+Z (Ghidra wraps a script run in one
//    transaction).
//  - Idempotent. Re-running makes no further changes.
//  - Asks before applying. Answer No for a dry run that reports without touching the program.
//
//@author NDInsight
//@category ND.PLANC
//@keybinding
//@menupath Tools.ND PLANC.Fix skip-return flow
//@toolbar

import ghidra.app.script.GhidraScript;
import ghidra.app.cmd.disassemble.DisassembleCommand;
import ghidra.program.model.address.Address;
import ghidra.program.model.address.AddressSet;
import ghidra.program.model.listing.CodeUnit;
import ghidra.program.model.listing.Function;
import ghidra.program.model.listing.Instruction;
import ghidra.program.model.listing.InstructionIterator;
import ghidra.program.model.listing.FlowOverride;
import ghidra.program.model.listing.Listing;
import ghidra.program.model.mem.Memory;
import ghidra.program.model.mem.MemoryAccessException;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public class PlancFixFlow extends GhidraScript {

    /** Report only, changing nothing. Asked at run time. */
    private boolean DRY_RUN = true;

    /**
     * Maximum fix/disassemble rounds. Newly found code exposes new call sites, whose error slots
     * hide more code, and so on. On the ENCOS image the cascade ran well past 8 rounds - it starts
     * at ~131 epilogues and converges towards the ~495 that a raw byte scan finds. Set high enough
     * that the loop terminates on the "stable" test rather than on this cap; if the run ends
     * without printing "stable", raise it again and re-run (the script is idempotent).
     */
    private static final int MAX_ROUNDS = 60;

    /**
     * jmp (N,A2) - the PLANC-MC skip-return epilogue. N is the SKIP DISTANCE and is NOT always 2.
     * Byte census of this image:
     *     N = 2   495 sites   ordinary routine. The 2 skipped bytes are the ERROR SLOT.
     *     N = 8     3 sites   the frame allocator. The 8 skipped bytes are an INLINE DESCRIPTOR,
     *                         i.e. DATA the caller planted after the call - not code, not an
     *                         error slot. Disassembling them produces garbage.
     * Reading N from the epilogue instead of assuming 2 handles both, and any further variant.
     */
    private static final byte[] EPILOGUE_HEAD = { (byte) 0x4E, (byte) 0xEA };

    /** jmp (A5) - the classic contents of the 2-byte error slot. */
    private static final byte[] JMP_A5 = { (byte) 0x4E, (byte) 0xD5 };

    /** movea.l (xx,A6),A0 - first half of the ON ROUTINEERROR handler tail. */
    private static final byte[] HANDLER_TAIL_HEAD = { 0x20, 0x6E };

    /** jmp (A0) - second half of the ON ROUTINEERROR handler tail. */
    private static final byte[] JMP_A0 = { (byte) 0x4E, (byte) 0xD0 };

    private Listing listing;
    private Memory mem;

    private int totalCallsFixed = 0;
    private int totalReturnsMarked = 0;
    private int totalDisassembled = 0;
    private int totalCleared = 0;
    private int totalDescriptors = 0;

    /**
     * Addresses we have already TRIED to disassemble. Some bytes simply will not disassemble -
     * invalid opcodes, or a target that is really data. Without this, such a site is re-queued
     * every round, the round never reports zero work, and the loop spins until the cap.
     */
    private final Set<Address> disasmAttempted = new HashSet<>();

    /** Call sites whose fallthrough we have already corrected, so we do not re-count them. */
    private final Set<Address> callsDone = new HashSet<>();

    /** Sites that were queued for disassembly but produced no instruction. */
    private final Set<Address> disasmFailed = new HashSet<>();
    private final List<String> warnings = new ArrayList<>();

    @Override
    protected void run() throws Exception {
        listing = currentProgram.getListing();
        mem = currentProgram.getMemory();

        DRY_RUN = !askYesNo("PLANC-MC skip-return flow fix",
                "APPLY changes to " + currentProgram.getName() + " ?\n\n"
                        + "Yes = apply the fix (reversible: PlancUndoFixFlow, or Ctrl+Z)\n"
                        + "No  = DRY RUN, report only, change nothing\n\n"
                        + "Recommended: run No first and read the report.");

        println("=== PLANC-MC skip-return flow fix ===");
        println("Program: " + currentProgram.getName());
        if (DRY_RUN) {
            println("*** DRY RUN - no changes will be made ***");
            println("    (a dry run reports ROUND 1 only - it cannot see the code that");
            println("     disassembling would reveal, so the real totals will be higher)");
        }
        println("");

        int round = 0;
        while (round < MAX_ROUNDS && !monitor.isCancelled()) {
            round++;
            int fixed = 0, marked = 0, disasm = 0, cleared = 0, handlerTails = 0, descriptors = 0;

            // ------------------------------------------------------------ Pass 1
            // funcEntry -> skip distance N from its jmp (N,A2) epilogue
            java.util.Map<Address, Integer> skipReturnFuncs = new java.util.HashMap<>();
            List<Instruction> epilogues = new ArrayList<>();
            int outsideFuncs = 0;
            int nonStandardSkip = 0;

            InstructionIterator scan = listing.getInstructions(true);
            while (scan.hasNext() && !monitor.isCancelled()) {
                Instruction ins = scan.next();
                int skip = skipDistanceAt(ins.getMinAddress());
                if (skip < 0) {
                    continue;
                }
                epilogues.add(ins);
                if (skip != 2) {
                    nonStandardSkip++;
                }
                Function f = listing.getFunctionContaining(ins.getMinAddress());
                if (f != null) {
                    skipReturnFuncs.put(f.getEntryPoint(), skip);
                } else {
                    outsideFuncs++;
                }
            }

            // ------------------------------------------------------------ Pass 2
            Set<Address> newTargets = new LinkedHashSet<>();
            // {startAddress, byteCount} for inline descriptors that must be DATA, not code.
            List<Object[]> inlineDescriptors = new ArrayList<>();
            InstructionIterator all = listing.getInstructions(true);
            while (all.hasNext() && !monitor.isCancelled()) {
                Instruction ins = all.next();
                if (!ins.getFlowType().isCall()) {
                    continue;
                }
                // getFlowType().isCall() is NOT sufficient. Ghidra reports some BRA.B instructions
                // as calls here (branch straight into a function entry). PLANC compiles
                //     ON ROUTINEERROR DO ... ENDON
                // as a BRA.B over an inline handler placed between the prologue and the body:
                //     60 14         bra.b body            <- looks like a "call" to Ghidra
                //     2d 5f 00 NN   move.l (SP)+,(NN,A6)  <- handler: pop the error link
                //     2d 40 00 10   move.l D0,(0x10,A6)   <- save ERRCODE
                //     ...
                //     20 6e 00 NN   movea.l (NN,A6),A0    <- resume via the saved link
                //     4e d0         jmp (A0)
                //   body:
                // Treating that BRA as a call and "skipping" 2 bytes would land inside the
                // handler. Only a real BSR/JSR performs a skip return.
                String mnem = ins.getMnemonicString().toLowerCase();
                if (!mnem.startsWith("bsr") && !mnem.startsWith("jsr")) {
                    continue;
                }

                Address natural = ins.getMaxAddress().add(1);

                Address[] flows = ins.getFlows();
                Integer calleeSkip = (flows != null && flows.length == 1)
                        ? skipReturnFuncs.get(flows[0]) : null;
                boolean isJmpA5 = matches(natural, JMP_A5);

                // Skip distance: from the proven callee if we know it, else assume the ordinary 2
                // (justified only when the slot is literally jmp (A5), which is unambiguous).
                int skip = (calleeSkip != null) ? calleeSkip.intValue() : 2;
                Address wanted = natural.add(skip);

                Address current = ins.getFallThrough();
                if (current != null && current.equals(wanted)) {
                    if (skip == 2 && needsDisassembly(wanted)) {
                        newTargets.add(wanted);
                    }
                    continue;
                }

                Instruction slot = listing.getInstructionAt(natural);
                boolean slotIsTwoBytes = (slot != null) && (slot.getLength() == 2);

                boolean accept;
                if (calleeSkip != null && calleeSkip.intValue() != 2) {
                    // Inline-descriptor convention (the frame allocator). The skipped bytes are
                    // DATA planted by the caller - accept on callee identity alone, there is no
                    // "slot instruction" to validate.
                    accept = true;
                    inlineDescriptors.add(new Object[] { natural, Integer.valueOf(skip) });
                } else {
                    accept = isJmpA5 || (calleeSkip != null && slotIsTwoBytes);
                }

                if (!accept) {
                    if (calleeSkip != null && round == 1) {
                        warnings.add(String.format(
                                "call at %s -> %s : callee skip-returns (N=%d) but the instruction"
                                        + " at %s is %d bytes (%s). LEFT ALONE - inspect by hand.",
                                ins.getMinAddress(), flows[0], calleeSkip.intValue(), natural,
                                (slot == null ? -1 : slot.getLength()), hex(natural, 2)));
                    }
                    continue;
                }

                if (!DRY_RUN) {
                    ins.setFallThrough(wanted);
                }
                newTargets.add(wanted);
                // Count each call site once, even if setFallThrough does not read back the way we
                // expect on this Ghidra version - otherwise the round never reports zero work.
                if (callsDone.add(ins.getMinAddress())) {
                    fixed++;
                }
            }

            // ------------------------------------------------------------ Pass 3
            for (Instruction ins : epilogues) {
                if (monitor.isCancelled()) {
                    break;
                }
                if (ins.getFlowOverride() == FlowOverride.RETURN) {
                    continue;
                }
                if (!DRY_RUN) {
                    ins.setFlowOverride(FlowOverride.RETURN);
                }
                marked++;
            }

            // ------------------------------------------------------------ Pass 3b
            // Recover the tail of every inline ON ROUTINEERROR handler. It is a fixed 6-byte
            // sequence that resumes the interrupted routine through the link the handler stashed:
            //     20 6E xx xx   movea.l (xx,A6),A0
            //     4E D0         jmp (A0)
            // Nothing flows into it (the handler above it ends in jmp (A5) or a +2 epilogue), so
            // Ghidra leaves it as raw bytes. Scanning for the literal pattern recovers it.
            Address probe = mem.getMinAddress();
            Address memEnd = mem.getMaxAddress();
            while (probe != null && probe.compareTo(memEnd) < 0 && !monitor.isCancelled()) {
                if (needsDisassembly(probe)
                        && matches(probe, HANDLER_TAIL_HEAD)
                        && matches(probe.add(4), JMP_A0)) {
                    newTargets.add(probe);
                    handlerTails++;
                }
                try {
                    probe = probe.add(2); // instructions are 2-byte aligned on 68000
                } catch (Exception e) {
                    break;
                }
            }

            // ------------------------------------------------------------ Pass 4
            // Disassemble the normal-path code that was unreachable until now. This is the pass
            // that matters when the error slot is a BRA: Ghidra followed the branch and never
            // touched RETLINK+2.
            AddressSet toDisassemble = new AddressSet();
            for (Address a : newTargets) {
                if (!needsDisassembly(a)) {
                    continue;
                }
                // Never attempt the same address twice. Some targets genuinely cannot be
                // disassembled (invalid opcodes, or the target is really data); retrying them
                // every round is what made this loop spin to the cap.
                if (!disasmAttempted.add(a)) {
                    continue;
                }
                // Two ways a bad instruction can block the target, and BOTH have to be handled:
                //
                //  (a) the target lands INSIDE an instruction that starts earlier
                //  (b) an instruction starts a few bytes AFTER the target, squatting on bytes the
                //      real instruction at the target needs. Seen at 0x736E: a valid 6-byte
                //      move.l D0,(0x19BBC).l starts there, but Ghidra had already laid a bogus
                //      2-byte ori.b at 0x7370, so bytes 0x7370-0x7373 were taken and disassembly
                //      at 0x736E had nowhere to go. Only looking "at" or "containing" misses this
                //      completely, which is why that one site span forever.
                //
                // A 68000 instruction is at most 10 bytes, so anything starting inside
                // (a, a+10) can be in the way.
                Instruction containing = listing.getInstructionContaining(a);
                if (containing != null && !containing.getMinAddress().equals(a)) {
                    if (!DRY_RUN) {
                        listing.clearCodeUnits(containing.getMinAddress(),
                                containing.getMaxAddress(), false);
                    }
                    cleared++;
                } else if (containing == null) {
                    Address probeEnd = a.add(10);
                    Address p = a.add(1);
                    while (p.compareTo(probeEnd) < 0) {
                        Instruction blocker = listing.getInstructionAt(p);
                        if (blocker != null) {
                            if (!DRY_RUN) {
                                listing.clearCodeUnits(blocker.getMinAddress(),
                                        blocker.getMaxAddress(), false);
                            }
                            cleared++;
                            break; // clear one blocker; a re-run picks up any further ones
                        }
                        p = p.add(1);
                    }
                }
                toDisassemble.add(a);
                disasm++;
            }
            if (!DRY_RUN && !toDisassemble.isEmpty()) {
                DisassembleCommand cmd = new DisassembleCommand(toDisassemble, null, true);
                cmd.applyTo(currentProgram, monitor);
                // Record anything that still has no instruction, so it is visible rather than
                // silently retried.
                for (Address a : newTargets) {
                    if (disasmAttempted.contains(a) && needsDisassembly(a)) {
                        disasmFailed.add(a);
                    }
                }
            }

            // ------------------------------------------------------------ Pass 4b
            // Inline descriptors are DATA the caller planted after the call for the callee to
            // read. Ghidra will have disassembled them as garbage instructions. Clear them and
            // lay down a byte array so the listing tells the truth.
            for (Object[] rec : inlineDescriptors) {
                if (monitor.isCancelled()) {
                    break;
                }
                Address start = (Address) rec[0];
                int len = ((Integer) rec[1]).intValue();
                Address end = start.add(len - 1);
                if (listing.getDefinedDataAt(start) != null) {
                    continue; // already done
                }
                descriptors++;
                if (DRY_RUN) {
                    continue;
                }
                try {
                    listing.clearCodeUnits(start, end, false);
                    listing.createData(start,
                            new ghidra.program.model.data.ArrayDataType(
                                    ghidra.program.model.data.ByteDataType.dataType, len, 1));
                    listing.setComment(start, CodeUnit.EOL_COMMENT,
                            "PLANC INLINE DESCRIPTOR (" + len + " bytes) - DATA, not code. "
                                    + "Planted by the caller immediately after the call; the callee "
                                    + "reads it through the return-link register and resumes at "
                                    + start.add(len) + " via jmp (" + len + ",A2).");
                } catch (Exception e) {
                    warnings.add("could not lay down inline descriptor at " + start + ": "
                            + e.getMessage());
                }
            }

            // ------------------------------------------------------------ Round report
            println(String.format(
                    "Round %d: %d epilogues (%d outside functions, %d with skip != 2)"
                            + " | %d calls fixed | %d returns marked | %d handler tails"
                            + " | %d inline descriptors | %d to disassemble (%d cleared)",
                    round, epilogues.size(), outsideFuncs, nonStandardSkip, fixed, marked,
                    handlerTails, descriptors, disasm, cleared));

            totalCallsFixed += fixed;
            totalReturnsMarked += marked;
            totalDisassembled += disasm;
            totalCleared += cleared;
            totalDescriptors += descriptors;

            if (fixed == 0 && marked == 0 && disasm == 0 && descriptors == 0) {
                println("         (stable - nothing left to do)");
                break;
            }
            if (DRY_RUN) {
                println("         (dry run stops after one round)");
                break;
            }
        }

        // ---------------------------------------------------------------- Report
        if (!warnings.isEmpty()) {
            println("");
            println("--- WARNINGS (" + warnings.size() + ") - nothing was changed at these sites ---");
            for (String w : warnings) {
                println("  " + w);
            }
        }

        println("");
        println("=== Totals ===");
        println("  call fallthroughs corrected : " + totalCallsFixed);
        println("  epilogues marked as RETURN  : " + totalReturnsMarked);
        println("  sites disassembled          : " + totalDisassembled
                + "  (" + totalCleared + " needed a bad instruction cleared first)");
        println("  inline descriptors -> data  : " + totalDescriptors);
        if (!disasmFailed.isEmpty()) {
            println("");
            println("--- " + disasmFailed.size() + " site(s) would not disassemble ---");
            println("    Queued once, attempted, produced no instruction, and NOT retried.");
            println("    Usually means the target is really data, or the bytes are not valid 68000.");
            int shown = 0;
            for (Address a : disasmFailed) {
                println("    " + a);
                if (++shown >= 20) {
                    println("    ... and " + (disasmFailed.size() - shown) + " more");
                    break;
                }
            }
        }
        if (!DRY_RUN && round >= MAX_ROUNDS) {
            println("");
            println("*** HIT THE ROUND CAP (" + MAX_ROUNDS + ") - THE RUN IS NOT FINISHED. ***");
            println("    The cascade was still producing work. Just run the script again; it is");
            println("    idempotent and will pick up where this left off. If it keeps hitting the");
            println("    cap, raise MAX_ROUNDS at the top of the file.");
        }
        println("");
        println("Re-decompile a function to see the effect. Remaining decompiler artefacts are NOT");
        println("flow problems and are out of scope for this script:");
        println("  - locals shown as piVar1[n]  -> PLANC frame slot at offset n*4 from A6.");
        println("                                  piVar1[n] is (n*4,A6); the FIRST PARAMETER is at");
        println("                                  (0x12,A6), not (0x14,A6) - see PlancFrameTypes.");
        reportCallingConvention();
    }

    /**
     * Is the __planc calling convention actually available in this program's compiler spec?
     * Report accordingly instead of printing a fixed "you need to install it" footer that stays
     * wrong after it HAS been installed.
     */
    private void reportCallingConvention() {
        boolean present = false;
        try {
            for (ghidra.program.model.lang.PrototypeModel m
                    : currentProgram.getCompilerSpec().getCallingConventions()) {
                if ("__planc".equals(m.getName())) {
                    present = true;
                    break;
                }
            }
        } catch (Throwable t) {
            println("  - could not query calling conventions: " + t.getMessage());
            return;
        }

        if (present) {
            println("  - __planc calling convention IS INSTALLED in this compiler spec.");
            println("    Apply it per function: right-click > Edit Function > Calling Convention");
            println("    > __planc, or in bulk with the set_function_prototype MCP tool.");
            println("    It resolves the LEADING argument (D0 scalar / A0 pointer) and the return");
            println("    value. Second and later arguments live in the callee frame at 0x14+ and");
            println("    cannot be modelled - recover those by hand.");
        } else {
            println("  - __planc calling convention is NOT available in this program.");
            println("    Add the <prototype name=\"__planc\"> block from");
            println("    planc-68000.cspec-snippet.xml to 68000.cspec in your Ghidra install, then");
            println("    RESTART Ghidra. A cspec is only read at language-load time, so editing it");
            println("    while Ghidra is running has no effect until you restart.");
            println("    If you just edited it: restart, and make sure you restart the SAME install");
            println("    you patched.");
        }
    }

    /**
     * If the bytes at addr are a PLANC skip-return epilogue "jmp (N,A2)" (4E EA nn nn), return N.
     * Otherwise return -1. N is the number of bytes the normal return skips over.
     */
    private int skipDistanceAt(Address addr) {
        if (!matches(addr, EPILOGUE_HEAD)) {
            return -1;
        }
        try {
            int n = ((mem.getByte(addr.add(2)) & 0xFF) << 8) | (mem.getByte(addr.add(3)) & 0xFF);
            // Sanity: a displacement of 0 is a plain jmp (A2), not a skip return, and anything
            // large is not a skip either.
            if (n < 2 || n > 64) {
                return -1;
            }
            return n;
        } catch (MemoryAccessException e) {
            return -1;
        }
    }

    /** True if this address has no instruction starting on it and is not defined data. */
    private boolean needsDisassembly(Address a) {
        if (a == null) {
            return false;
        }
        if (!mem.contains(a)) {
            return false;
        }
        if (listing.getInstructionAt(a) != null) {
            return false;
        }
        if (listing.getDefinedDataAt(a) != null) {
            return false;
        }
        return true;
    }

    private boolean matches(Address addr, byte[] pattern) {
        try {
            byte[] buf = new byte[pattern.length];
            if (mem.getBytes(addr, buf) != pattern.length) {
                return false;
            }
            for (int i = 0; i < pattern.length; i++) {
                if (buf[i] != pattern[i]) {
                    return false;
                }
            }
            return true;
        } catch (MemoryAccessException e) {
            return false;
        }
    }

    private String hex(Address addr, int n) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < n; i++) {
            try {
                sb.append(String.format("%02x ", mem.getByte(addr.add(i))));
            } catch (MemoryAccessException e) {
                sb.append("?? ");
            }
        }
        return sb.toString().trim();
    }
}
