//Reverts everything PlancFixFlow.java did: clears fallthrough overrides on calls and resets
//FlowOverride to NONE on the PLANC-MC skip-return epilogues.
//
//Use this if the flow fix made things worse, or before re-running with different settings.
//It does NOT undo the disassembly that PlancFixFlow triggered - that is harmless to leave, but
//if you want it gone, select the range in the GUI and press C (Clear Code Bytes).
//
//@author NDInsight
//@category ND.PLANC
//@keybinding
//@menupath Tools.ND PLANC.Undo skip-return flow fix
//@toolbar

import ghidra.app.script.GhidraScript;
import ghidra.program.model.address.Address;
import ghidra.program.model.listing.FlowOverride;
import ghidra.program.model.listing.Instruction;
import ghidra.program.model.listing.InstructionIterator;
import ghidra.program.model.listing.Listing;
import ghidra.program.model.mem.Memory;
import ghidra.program.model.mem.MemoryAccessException;

public class PlancUndoFixFlow extends GhidraScript {

    private static final byte[] EPILOGUE = { (byte) 0x4E, (byte) 0xEA, 0x00, 0x02 };

    @Override
    protected void run() throws Exception {
        Listing listing = currentProgram.getListing();
        Memory mem = currentProgram.getMemory();

        int fallthroughsCleared = 0;
        int overridesCleared = 0;

        InstructionIterator all = listing.getInstructions(true);
        while (all.hasNext() && !monitor.isCancelled()) {
            Instruction ins = all.next();

            // Restore natural fallthrough on any call we overrode.
            if (ins.getFlowType().isCall() && ins.isFallThroughOverridden()) {
                ins.clearFallThroughOverride();
                fallthroughsCleared++;
            }

            // Restore natural flow on the epilogues.
            if (ins.getFlowOverride() != FlowOverride.NONE
                    && matches(mem, ins.getMinAddress(), EPILOGUE)) {
                ins.setFlowOverride(FlowOverride.NONE);
                overridesCleared++;
            }
        }

        println("=== PLANC-MC flow fix reverted ===");
        println("  fallthrough overrides cleared : " + fallthroughsCleared);
        println("  epilogue flow overrides reset : " + overridesCleared);
        println("Disassembly created by the fix was left in place (harmless).");
    }

    private boolean matches(Memory mem, Address addr, byte[] pattern) {
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
}
