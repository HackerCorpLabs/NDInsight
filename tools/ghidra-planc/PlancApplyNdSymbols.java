//Applies Norsk Data's OWN embedded symbol table to a PLANC-MC 68000 firmware image.
//
//BACKGROUND
//  ND's PLANC-MC firmware images (ENCOS, the 211185 TCP/IP gateway, ...) carry a vendor symbol
//  table near the top of the image: 32-byte records giving ND's own name and address for every
//  CODE and DRAM symbol. Mining it first is what makes the rest of the disassembly readable.
//
//  Record layout (VERIFIED by hexdump of tcp-ser-all-banks-b05-68k.bin at 0x7C3A4):
//      +0x00  4  self/next pointer, increments by 0x20
//      +0x04  1  name length (1..12)
//      +0x06  1  0x02 = defined, 0xFF = undefined / marker
//      +0x07  1  segment: 0x10 = CODE, 0x16 = DRAM, 0x11 = other
//      +0x08  4  address, big-endian
//      +0x10 12  name (10 characters in practice)
//
//  NOTE this sits 4 bytes later than the layout recorded for the ENCOS table. If a table does not
//  parse, try shifting the record base by -4.
//
//WHY THIS SCRIPT EXISTS - the stale-body trap
//  Ghidra does NOT recompute an existing function's body when control flow changes underneath it.
//  After PlancFixFlow repairs the skip-return flow, functions created BEFORE that run keep their
//  old - often 1-byte - bodies, and createFunction() on an existing entry point simply returns
//  "already exists" without recomputing anything. Renaming such a function pins the broken body.
//
//  The correct order is therefore, per symbol:
//      1. disassemble the entry point if it is not already code
//      2. REMOVE any existing function at that entry point
//      3. create the function afresh, so Ghidra recomputes the body from current flow
//      4. name it
//
//  Run PlancFixFlow FIRST. Running this before the flow is fixed will produce correct-looking
//  functions with truncated bodies.
//
//USAGE
//  GUI      : run it, answer the prompt.
//  Headless : -postScript PlancApplyNdSymbols.java          -> DRY RUN (safe default)
//             -postScript PlancApplyNdSymbols.java apply    -> apply
//
//  Optional second argument overrides the table extent, e.g. "apply 0x7C3A4 0x7FD88".
//
//SAFETY
//  - Idempotent: re-running removes and recreates the same functions, so it does not accumulate.
//  - It uses setName on the function rather than createLabel, so it does NOT stack duplicate
//    labels the way createLabel does on re-runs.
//  - DRAM symbols are labelled, not turned into functions.
//
//@category ND.PLANC
//@author NDInsight

import ghidra.app.script.GhidraScript;
import ghidra.program.model.address.Address;
import ghidra.program.model.listing.Function;
import ghidra.program.model.listing.Instruction;
import ghidra.program.model.listing.Listing;
import ghidra.program.model.mem.Memory;
import ghidra.program.model.symbol.SourceType;
import ghidra.program.model.symbol.Symbol;
import ghidra.program.model.symbol.SymbolTable;

import java.util.ArrayList;
import java.util.List;

public class PlancApplyNdSymbols extends GhidraScript {

    /** Default extent of the embedded table in tcp-ser-all-banks-b05-68k.bin. */
    private long tableStart = 0x7C3A4L;
    private long tableEnd   = 0x7FD88L;

    private static final int REC = 0x20;

    private static final int SEG_CODE = 0x10;
    private static final int SEG_DRAM = 0x16;

    private static final int KIND_DEFINED = 0x02;

    private boolean dryRun = true;

    private Listing listing;
    private Memory mem;
    private SymbolTable symtab;

    /** One parsed symbol-table record. */
    private static final class Rec {
        String name;
        int seg;
        long addr;
        long at;
    }

    @Override
    protected void run() throws Exception {
        listing = currentProgram.getListing();
        mem = currentProgram.getMemory();
        symtab = currentProgram.getSymbolTable();

        String[] args = getScriptArgs();
        if (isRunningHeadless()) {
            dryRun = !(args.length > 0 && "apply".equalsIgnoreCase(args[0]));
        }
        else {
            dryRun = !askYesNo("Apply ND embedded symbols",
                    "APPLY ND's embedded symbol table to " + currentProgram.getName() + " ?\n\n"
                            + "This REMOVES and RECREATES every function at a CODE symbol so that\n"
                            + "Ghidra recomputes bodies from current flow. Run PlancFixFlow first.\n\n"
                            + "No = dry run, report only.");
        }
        if (args.length >= 3) {
            tableStart = Long.decode(args[1]);
            tableEnd = Long.decode(args[2]);
        }

        println("=== Apply ND embedded symbol table ===");
        println("Program: " + currentProgram.getName());
        println(String.format("Table  : 0x%X - 0x%X", tableStart, tableEnd));
        if (dryRun) {
            println("*** DRY RUN - nothing will be changed ***");
        }

        List<Rec> recs = parseTable();
        println("");
        println("Records parsed        : " + recs.size());

        int code = 0, dram = 0;
        for (int i = 0; i < recs.size(); i++) {
            if (recs.get(i).seg == SEG_CODE) code++;
            else if (recs.get(i).seg == SEG_DRAM) dram++;
        }
        println("  CODE defined        : " + code);
        println("  DRAM defined        : " + dram);
        println("");

        int disassembled = 0, removed = 0, created = 0, renamed = 0, labelled = 0;
        List<String> failures = new ArrayList<>();

        for (int i = 0; i < recs.size(); i++) {
            Rec r = recs.get(i);
            Address a;
            try {
                a = toAddr(r.addr);
            }
            catch (Exception e) {
                failures.add(String.format("%-12s bad address 0x%X", r.name, r.addr));
                continue;
            }
            if (!mem.contains(a)) {
                failures.add(String.format("%-12s 0x%08X not in memory", r.name, r.addr));
                continue;
            }

            if (r.seg == SEG_DRAM) {
                // Data: name it, do not create a function. Renaming an existing symbol avoids
                // the createLabel() duplicate-stacking trap.
                if (!dryRun) {
                    Symbol existing = symtab.getPrimarySymbol(a);
                    if (existing != null && !existing.isDynamic()) {
                        existing.setName(r.name, SourceType.USER_DEFINED);
                    }
                    else {
                        createLabel(a, r.name, true);
                    }
                }
                labelled++;
                continue;
            }
            if (r.seg != SEG_CODE) {
                continue;
            }

            // --- CODE: disassemble, remove, recreate, name. Order matters (see header). ---
            Instruction insn = listing.getInstructionAt(a);
            if (insn == null) {
                if (!dryRun) {
                    if (listing.getDefinedDataAt(a) != null) {
                        clearListing(a);
                    }
                    disassemble(a);
                }
                disassembled++;
            }

            Function f = getFunctionAt(a);
            if (f != null) {
                if (!dryRun) {
                    removeFunction(f);
                }
                removed++;
            }

            if (!dryRun) {
                Function nf = createFunction(a, r.name);
                if (nf == null) {
                    // Entry may lie inside another function's body; report rather than guess.
                    Function containing = getFunctionContaining(a);
                    failures.add(String.format("%-12s 0x%08X createFunction failed%s",
                            r.name, r.addr,
                            containing == null ? "" : " (inside " + containing.getName() + ")"));
                    continue;
                }
                created++;
                if (!r.name.equals(nf.getName())) {
                    nf.setName(r.name, SourceType.USER_DEFINED);
                    renamed++;
                }
            }
            else {
                created++;
            }
        }

        println("=== Totals ===");
        println("  entry points disassembled : " + disassembled);
        println("  stale functions removed   : " + removed);
        println("  functions created + named : " + created);
        println("  DRAM symbols labelled     : " + labelled);
        println("  failures                  : " + failures.size());
        if (!failures.isEmpty()) {
            println("");
            println("--- FAILURES ---");
            for (int i = 0; i < failures.size(); i++) {
                println("  " + failures.get(i));
            }
        }
        println("");
        println("Function count now: " + currentProgram.getFunctionManager().getFunctionCount());
        if (dryRun) {
            println("");
            println("DRY RUN - re-run with the 'apply' argument to make these changes.");
        }
    }

    /** Walk the 32-byte records and return every DEFINED one that has a sane name. */
    private List<Rec> parseTable() throws Exception {
        List<Rec> out = new ArrayList<>();
        for (long off = tableStart; off + REC <= tableEnd; off += REC) {
            Address base;
            try {
                base = toAddr(off);
            }
            catch (Exception e) {
                continue;
            }
            if (!mem.contains(base) || !mem.contains(base.add(REC - 1))) {
                continue;
            }
            int nameLen = mem.getByte(base.add(0x04)) & 0xFF;
            int kind = mem.getByte(base.add(0x06)) & 0xFF;
            int seg = mem.getByte(base.add(0x07)) & 0xFF;
            if (nameLen < 1 || nameLen > 12) {
                continue;
            }
            if (kind != KIND_DEFINED) {
                continue;             // 0xFF = undefined / marker - nothing to place
            }
            if (seg != SEG_CODE && seg != SEG_DRAM) {
                continue;
            }
            long addr = 0;
            for (int i = 0; i < 4; i++) {
                addr = (addr << 8) | (mem.getByte(base.add(0x08 + i)) & 0xFFL);
            }
            StringBuilder sb = new StringBuilder(nameLen);
            boolean ok = true;
            for (int i = 0; i < nameLen; i++) {
                int c = mem.getByte(base.add(0x10 + i)) & 0x7F;
                if (c < 32 || c >= 127) {
                    ok = false;
                    break;
                }
                sb.append((char) c);
            }
            if (!ok) {
                continue;
            }
            String name = sb.toString().trim();
            if (name.isEmpty()) {
                continue;
            }
            // Ghidra rejects some characters in symbol names; ND uses '#' for runtime routines.
            name = name.replace(' ', '_');
            Rec r = new Rec();
            r.name = name;
            r.seg = seg;
            r.addr = addr;
            r.at = off;
            out.add(r);
        }
        return out;
    }
}
