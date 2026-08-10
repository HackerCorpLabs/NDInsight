//Annotates PLANC-MC (MC68000) compiled constructs in the listing so a developer - or an LLM
//reading the disassembly through an MCP - can see the source-level shape without knowing PLANC.
//
//Run PlancFixFlow.java FIRST. This script only comments; it changes no flow, no names, no types.
//
//WHAT IT ANNOTATES
//  1. ON ROUTINEERROR DO ... ENDON   - the inline error handler between prologue and body
//  2. error slots                    - the 2 bytes after a call, executed only on ERRETURN
//  3. jmp (A5)                       - the error unwind, NOT a coroutine yield
//  4. the +2 skip-return epilogue
//  5. handler resume tails
//  6. routine prologues              - frame size and where locals start
//  7. function plate comments        - for functions that have none
//  8. function tags + bookmarks      - so you can filter and navigate
//
//IT WILL NOT OVERWRITE AN EXISTING COMMENT. Everything in this database that was written by hand
//is preserved; this only fills empty slots. Set APPEND_TO_EXISTING = true to append instead.
//
//@author NDInsight
//@category ND.PLANC
//@keybinding
//@menupath Tools.ND PLANC.Annotate PLANC constructs
//@toolbar

import ghidra.app.script.GhidraScript;
import ghidra.program.model.address.Address;
import ghidra.program.model.listing.CodeUnit;
import ghidra.program.model.listing.Function;
import ghidra.program.model.listing.FunctionIterator;
import ghidra.program.model.listing.Instruction;
import ghidra.program.model.listing.InstructionIterator;
import ghidra.program.model.listing.Listing;
import ghidra.program.model.mem.Memory;
import ghidra.program.model.mem.MemoryAccessException;

public class PlancAnnotate extends GhidraScript {

    /** If true, append to an existing comment instead of leaving it alone. */
    private static final boolean APPEND_TO_EXISTING = false;

    /**
     * Marker embedded in every comment this script writes.
     *
     * Without it there is no way to tell OUR comment from a hand-written one, so a corrected
     * comment can never be pushed out - the script sees "already has a comment" and leaves the
     * stale text in place forever. That is the same defend-my-own-stale-output trap that hit the
     * label naming and the data types. The marker makes refresh safe: we only ever overwrite text
     * carrying it, and never touch anything a human wrote.
     *
     * Deliberately terse and unlikely to be typed by hand. Comments written BEFORE this marker
     * existed are recognised by the legacy signatures in isOurs().
     */
    private static final String MARK = "[planc-auto]";

    /** Set at run time: replace this script's own previous comments with current text. */
    private boolean refresh = false;

    /** Add an EOL comment to all ~500 epilogues. Noisy - the RETURN override already shows it. */
    private static final boolean COMMENT_EVERY_EPILOGUE = false;

    private static final byte[] EPILOGUE   = { (byte) 0x4E, (byte) 0xEA, 0x00, 0x02 }; // jmp (2,A2)
    private static final byte[] JMP_A5     = { (byte) 0x4E, (byte) 0xD5 };             // jmp (A5)
    private static final byte[] JMP_A0     = { (byte) 0x4E, (byte) 0xD0 };             // jmp (A0)
    private static final byte[] TAIL_HEAD  = { 0x20, 0x6E };                           // movea.l (d,A6),A0
    private static final byte[] POP_LINK   = { 0x2D, 0x5F };                     // move.l (SP)+,(d,A6)
    private static final byte[] SAVE_ERRC  = { 0x2D, 0x40, 0x00, 0x10 };         // move.l D0,(0x10,A6)
    private static final byte[] PROLOGUE   = { 0x2F, 0x0E, 0x2C, 0x56 };  // move.l A6,-(SP); movea.l (A6),A6

    private Listing listing;
    private Memory mem;

    private int nHandlers = 0, nSlots = 0, nUnwinds = 0, nEpilogues = 0;
    private int nTails = 0, nPrologues = 0, nPlates = 0, nTags = 0, nSkipped = 0, nRefreshed = 0;

    @Override
    protected void run() throws Exception {
        listing = currentProgram.getListing();
        mem = currentProgram.getMemory();

        // REFRESH first and default - this script's text is the source of truth for its own
        // comments, and the prologue wording changed on 2026-07-26 (parameters are at +0x12,
        // not +0x14). Hand-written comments are never touched in either mode.
        String REFRESH = "Refresh - fill empty slots AND update this script's own older comments";
        String FILL = "Fill    - only fill empty slots, leave every existing comment alone";
        java.util.List<String> choices = new java.util.ArrayList<>();
        choices.add(REFRESH);
        choices.add(FILL);
        String choice = askChoice("Annotate PLANC constructs",
                "Add PLANC explanatory comments to " + currentProgram.getName() + " ?\n\n"
                        + "Comments YOU wrote are preserved in both modes.\n"
                        + "Run PlancFixFlow.java first if you have not already.",
                choices, REFRESH);
        refresh = REFRESH.equals(choice);

        InstructionIterator it = listing.getInstructions(true);
        while (it.hasNext() && !monitor.isCancelled()) {
            Instruction ins = it.next();
            Address a = ins.getMinAddress();

            // ---------------------------------------------------------- jmp (A5): error unwind
            if (matches(a, JMP_A5)) {
                eol(a, "PLANC ERROR UNWIND. A5 holds the runtime error vector: #XRET (0x135A8) "
                        + "propagates the error up one frame, #ERET (0x13596) dispatches to an "
                        + "ON ROUTINEERROR handler. This is NOT a coroutine yield and NOT a jump "
                        + "table. It executes only when the callee took its ERRETURN exit.");
                nUnwinds++;
            }

            // ---------------------------------------------------------- the +2 skip return
            if (matches(a, EPILOGUE)) {
                if (COMMENT_EVERY_EPILOGUE) {
                    eol(a, "PLANC NORMAL RETURN. Pops the return address into A2 and jumps to "
                            + "RETLINK+2, skipping the caller's 2-byte error slot. "
                            + "(ND-820026.1:3432 \"skip return\".)");
                    nEpilogues++;
                }
            }

            // ---------------------------------------------------------- handler resume tail
            if (matches(a, TAIL_HEAD) && matches(a.add(4), JMP_A0)) {
                eol(a, "PLANC ON ROUTINEERROR resume: reload the error link the handler stashed in "
                        + "the frame and jump back through it.");
                nTails++;
            }

            // ---------------------------------------------------------- call + error slot
            String mnem = ins.getMnemonicString().toLowerCase();
            if (mnem.startsWith("bsr") || mnem.startsWith("jsr")) {
                Address slot = ins.getMaxAddress().add(1);
                Instruction slotIns = listing.getInstructionAt(slot);
                if (slotIns != null && slotIns.getLength() == 2) {
                    String what = matches(slot, JMP_A5) ? "unwinds to #XRET"
                            : "branches to a local ON ROUTINEERROR handler";
                    eol(slot, "PLANC ERROR SLOT (2 bytes) - " + what + ". NOT executed on success: "
                            + "the callee's normal return skips it and lands at "
                            + slot.add(2) + ".");
                    nSlots++;
                }
            }

            // ---------------------------------------------------------- ON ROUTINEERROR handler
            // Shape:  <prologue> ; bra.b body ; 2D 5F 00 NN (pop error link) ; 2D 40 00 10 (ERRCODE)
            if (mnem.startsWith("bra")) {
                Address handler = ins.getMaxAddress().add(1);
                if (matches(handler, POP_LINK) && matches(handler.add(4), SAVE_ERRC)) {
                    Address[] flows = ins.getFlows();
                    String body = (flows != null && flows.length == 1)
                            ? flows[0].toString() : "the routine body";
                    int linkSlot = readWord(handler.add(2));

                    pre(a, "PLANC SOURCE CONSTRUCT:  ON ROUTINEERROR DO ... ENDON\n"
                            + "This BRA is NOT a call. The compiler places the error handler inline\n"
                            + "between the prologue and the routine body, so the entry path has to\n"
                            + "jump over it. Body continues at " + body + ".\n"
                            + "The handler below runs only when a callee inside this routine takes\n"
                            + "its ERRETURN exit and the unwind (#XRET via A5) reaches this frame.");

                    eol(handler, String.format(
                            "PLANC ON ROUTINEERROR handler entry: pop the error link into "
                                    + "(0x%x,A6), then save ERRCODE from D0 into (0x10,A6). "
                                    + "ND-820026.1:3436 - \"The D0-register keeps the ERRCODE value.\"",
                            linkSlot));

                    createBookmark(a, "PLANC", "ON ROUTINEERROR handler (body at " + body + ")");
                    nHandlers++;

                    Function f = listing.getFunctionContaining(a);
                    if (f != null) {
                        tag(f, "PLANC_HAS_ERROR_HANDLER");
                    }
                }
            }

            // ---------------------------------------------------------- routine prologue
            if (matches(a, PROLOGUE)) {
                Function f = listing.getFunctionContaining(a);
                int frameSize = findFrameSize(a);
                String frameTxt = (frameSize < 0) ? "unknown"
                        : String.format("0x%x bytes", frameSize);

                pre(a, "PLANC ROUTINE ENTRY. Frame size " + frameTxt + ".\n"
                        + "  move.l A6,-(SP)      save the caller's frame pointer\n"
                        + "  movea.l (A6),A6      follow the next-free cursor to THIS frame\n"
                        + "  move.l SP,(0x8,A6)   park SP so #XRET can unwind\n"
                        + "  lea (N,A6),A2 / move.l A2,(A6)   publish my own next-free cursor\n"
                        + "Frames are bump-allocated in a separate arena and are never popped - "
                        + "only the (saved A6, return address) pair lives on SP. The manual calls "
                        + "this 'the stack grows both upwards and downwards'.\n"
                        + "PARAMETERS START AT (0x12,A6), not the manual's 24B/(0x14,A6) - verified "
                        + "2026-07-26 from the caller/callee offset match at 0x31D4/0x28E6 and "
                        + "0x28E6/0x40BE. (0x10,A6) ERRCODE is a WORD, which is what leaves +0x12 "
                        + "free. Untyped, the decompiler shows frame cells as piVar1[n] with "
                        + "n = offset/4, so (0x14,A6) is piVar1[5]; run PlancFrameTypes to get "
                        + "named fields instead.");
                nPrologues++;

                if (f != null) {
                    tag(f, "PLANC_ROUTINE");
                    // Same rule as setCmt: fill when empty, and in refresh mode also replace one
                    // of OUR OWN older comments - but never a comment somebody wrote by hand.
                    String fc = f.getComment();
                    boolean writable = (fc == null) || fc.trim().isEmpty()
                            || (refresh && isOurs(fc));
                    if (writable) {
                        f.setComment("PLANC-MC compiled routine. Frame " + frameTxt + ".\n"
                                + "Calling convention: leading argument in D0 (scalars <= 32 bits) "
                                + "or A0 (pointer to anything else); further arguments written by "
                                + "the caller into this frame from (0x12,A6) upward, reached "
                                + "through the caller's (0x4,A6). Integer result "
                                + "in D0, pointer/composite result in A0. A5 and A6 are preserved; "
                                + "D0-D7 and A0-A4 are volatile.\n"
                                + "Returns via the +2 skip return - see the epilogue.");
                        nPlates++;
                    }
                }
            }
        }

        println("=== PLANC annotation complete ===");
        println("  ON ROUTINEERROR handlers  : " + nHandlers);
        println("  error slots               : " + nSlots);
        println("  jmp (A5) unwinds          : " + nUnwinds);
        println("  handler resume tails      : " + nTails);
        println("  routine prologues         : " + nPrologues);
        println("  epilogues                 : " + nEpilogues
                + (COMMENT_EVERY_EPILOGUE ? "" : "  (disabled - set COMMENT_EVERY_EPILOGUE=true)"));
        println("  function plate comments   : " + nPlates);
        println("  function tags applied     : " + nTags);
        println("  refreshed (our own, stale): " + nRefreshed);
        println("  left alone (had a comment): " + nSkipped);
        println("");
        println("Navigate with Bookmarks (category PLANC) or filter the Functions window by tag");
        println("PLANC_ROUTINE / PLANC_HAS_ERROR_HANDLER.");
    }

    /** Frame size from the "lea (N,A6),A2" that follows the prologue, or -1. */
    private int findFrameSize(Address prologue) {
        try {
            // 2F0E 2C56 2D4F 0008 4?EE 00NN
            Address lea = prologue.add(8);
            int b0 = mem.getByte(lea) & 0xFF;
            int b1 = mem.getByte(lea.add(1)) & 0xFF;
            if ((b0 & 0xF0) == 0x40 && b1 == 0xEE) {
                return readWord(lea.add(2));
            }
        } catch (Exception e) {
            // fall through
        }
        return -1;
    }

    private int readWord(Address a) {
        try {
            return ((mem.getByte(a) & 0xFF) << 8) | (mem.getByte(a.add(1)) & 0xFF);
        } catch (MemoryAccessException e) {
            return -1;
        }
    }

    private void eol(Address a, String text) {
        setCmt(a, CodeUnit.EOL_COMMENT, text);
    }

    private void pre(Address a, String text) {
        setCmt(a, CodeUnit.PRE_COMMENT, text);
    }

    /**
     * Never clobbers a hand-written comment - but in refresh mode DOES replace one of our own,
     * so corrected wording actually reaches the database.
     */
    private void setCmt(Address a, int type, String text) {
        String stamped = text + "  " + MARK;
        String existing = listing.getComment(type, a);
        if (existing != null && !existing.trim().isEmpty()) {
            if (refresh && isOurs(existing)) {
                if (!existing.equals(stamped)) {
                    listing.setComment(a, type, stamped);
                    nRefreshed++;
                }
                return;
            }
            if (APPEND_TO_EXISTING && !isOurs(existing)) {
                listing.setComment(a, type, existing + "\n---\n" + stamped);
            } else {
                nSkipped++;
            }
            return;
        }
        listing.setComment(a, type, stamped);
    }

    /**
     * Did THIS script write that comment? Current output carries MARK. Anything written before the
     * marker existed is matched by its opening phrase - those are fixed strings this script emits,
     * specific enough that a human comment will not collide with them.
     */
    private boolean isOurs(String s) {
        if (s.contains(MARK)) {
            return true;
        }
        return s.startsWith("PLANC ROUTINE ENTRY.")
                || s.startsWith("PLANC ERROR SLOT")
                || s.startsWith("PLANC EPILOGUE")
                || s.startsWith("PLANC ON ROUTINEERROR")
                || s.startsWith("PLANC-MC compiled routine.");
    }

    private void tag(Function f, String name) {
        try {
            f.addTag(name);
            nTags++;
        } catch (Throwable t) {
            // Function tags are not available on very old Ghidra versions - not fatal.
        }
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
}
