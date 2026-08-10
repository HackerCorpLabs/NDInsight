//Sets up the PLANC-MC type and symbol environment: data types from the ND manuals, namespaces for
//the vendor symbol set, and prototypes for the PLANC runtime routines whose signatures are known.
//
//Run after PlancFixFlow.java. Independent of PlancAnnotate.java - either order.
//
//WHAT IT DOES
//  1. Creates data types in category /PLANC:
//       PlancArrayDescriptor  - how ARRAY parameters are passed (ND-20034 section 4.5)
//       PlancListNode         - the node shape used by the #APPD / #REMV list primitives
//       PlancFrameHeader      - the A6-relative routine frame (documentation; cannot be applied
//                               to a register-relative frame, but it belongs in the type manager)
//  2. Sorts the vendor symbols into namespaces so the symbol tree is navigable:
//       PLANC::   the #-prefixed runtime library      (#APPD, #IMU, #XRET, ...)
//       XMSG::    the XMP* / XMF* message primitives
//       PIOCOS::  PO* / POS* operating-system routines
//       LNMA::    LN* Ethernet media-access routines
//       MON::     MON0..MON65 monitor-call stubs
//  3. Applies prototypes ONLY to runtime routines whose signature is actually established.
//     Everything else gets a plate comment and is left alone - a wrong prototype corrupts
//     decompiler output, which is worse than no prototype.
//
//SAFETY
//  - Never renames anything. Never overwrites an existing plate comment.
//  - Namespace moves are reversible (set the namespace back to Global).
//  - Set DRY_RUN via the prompt to preview.
//
//@author NDInsight
//@category ND.PLANC
//@keybinding
//@menupath Tools.ND PLANC.Set up types and namespaces
//@toolbar

import ghidra.app.script.GhidraScript;
import ghidra.program.model.address.Address;
import ghidra.program.model.data.ArrayDataType;
import ghidra.program.model.data.ByteDataType;
import ghidra.program.model.data.CategoryPath;
import ghidra.program.model.data.DataTypeManager;
import ghidra.program.model.data.PointerDataType;
import ghidra.program.model.data.StructureDataType;
import ghidra.program.model.data.UnsignedIntegerDataType;
import ghidra.program.model.data.UnsignedShortDataType;
import ghidra.program.model.listing.Function;
import ghidra.program.model.listing.FunctionIterator;
import ghidra.program.model.symbol.Namespace;
import ghidra.program.model.symbol.SourceType;
import ghidra.program.model.symbol.Symbol;

public class PlancSetupTypes extends GhidraScript {

    private boolean dryRun = true;
    /**
     * When true, a PLANC type that already exists is DELETED and rebuilt from this script's
     * current definition. Needed whenever the definition here has been corrected - otherwise the
     * script keeps the stale layout forever and silently reports "already exists - left alone".
     * That is the same defend-my-own-stale-output trap that bit the vector-table script.
     */
    private boolean replaceTypes = false;
    private int typesMade = 0, moved = 0, plated = 0, protod = 0, skipped = 0, replaced = 0;

    @Override
    protected void run() throws Exception {
        String APPLY = "Apply   - create anything missing, leave existing types alone";
        String REPLACE = "Replace - REBUILD the /PLANC types from this script's current definitions";
        String PREVIEW = "Preview - dry run, change nothing";
        java.util.List<String> choices = new java.util.ArrayList<>();
        // REPLACE is deliberately FIRST and the default - see the note in PlancFrameTypes. The
        // script's definitions are the source of truth; a stale type in the database is never
        // wanted, and replacing with an identical definition is a no-op.
        choices.add(REPLACE);
        choices.add(APPLY);
        choices.add(PREVIEW);
        String choice = askChoice("PLANC type / namespace setup",
                "What should this do to " + currentProgram.getName() + " ?\n\n"
                        + "REPLACE (default) makes the /PLANC types match this script exactly.\n"
                        + "Apply keeps whatever types are already in the database.\n\n"
                        + "Nothing is renamed. Existing plate comments are preserved.",
                choices, REPLACE);
        dryRun = PREVIEW.equals(choice);
        replaceTypes = REPLACE.equals(choice);

        println("=== PLANC type / namespace setup ===");
        if (dryRun) {
            println("*** DRY RUN ***");
        }

        createTypes();
        organiseNamespaces();
        documentRuntime();

        println("");
        println("=== Totals ===");
        println("  data types created     : " + typesMade);
        println("  data types REPLACED    : " + replaced);
        println("  symbols namespaced     : " + moved);
        println("  plate comments added   : " + plated);
        println("  prototypes applied     : " + protod);
        println("  left alone             : " + skipped);
    }

    // ------------------------------------------------------------------ data types

    private void createTypes() throws Exception {
        DataTypeManager dtm = currentProgram.getDataTypeManager();
        CategoryPath cat = new CategoryPath("/PLANC");

        // ARRAY parameters. ND-20034-1-EN section 4.5:
        //   "ARRAYs are passed as pointers, with a pointer to a virtual origo (the address of the
        //    zeroth element of the array which all addresses in the array are relative to), a
        //    lower limit and an upper limit. For each dimension greater than one, this descriptor
        //    is extended with the number of elements in the previous dimension, along with the new
        //    upper and lower limits of the new dimension."
        // NOTE: the 12-bytes-per-dimension figure in that manual is stated for the ND-500. Whether
        // the MC68000 descriptor is the same size is NOT documented - treat this as 1-dimensional
        // and verify before relying on multi-dimensional layouts.
        // MEASURED 2026-07-26: the ENCOS sites build an 8-byte descriptor, not 12. The caller at
        // 0x31D4 writes a LONG origo at +0x00, then WORDS at +0x04 and +0x06:
        //     move.l A2,(0x12,A1) ; clr.w (0x16,A1) ; move.w #3,(0x18,A1)
        // and the callees (0x28E6, 0x40BE) copy exactly 8 bytes back out with two move.l. So the
        // limits are 16-bit here. The 12-byte all-longword form may still exist for wider index
        // types - check the width at the site before applying this type.
        StructureDataType arr = new StructureDataType(cat, "PlancArrayDescriptor", 0, dtm);
        arr.add(new PointerDataType(), 4, "virtualOrigo",
                "address of element zero; all element addresses are relative to this");
        arr.add(new UnsignedShortDataType(), 2, "lowerLimit", "lowest valid index (16-bit here)");
        arr.add(new UnsignedShortDataType(), 2, "upperLimit", "highest valid index (16-bit here)");
        arr.setDescription("PLANC ARRAY parameter descriptor (ND-20034-1-EN section 4.5). "
                + "8 bytes as used in ENCOS - {long origo, word lower, word upper} - measured from "
                + "the 0x31D4 -> 0x28E6 -> 0x40BE call chain, NOT the 12-byte all-longword form. "
                + "The 12-bytes-per-dimension figure in ND-20034 is stated for the ND-500. "
                + "One of these per dimension; multi-dimensional layout on MC68000 is UNVERIFIED.");
        addType(dtm, arr);

        // The list primitives #APPD (append) and #REMV (remove) take the link-field offset in D0,
        // so the 'next' pointer is not always at offset 0 - but every software list observed in
        // this firmware passes D0 = 0.
        StructureDataType node = new StructureDataType(cat, "PlancListNode", 0, dtm);
        node.add(new PointerDataType(), 4, "next",
                "link field. #APPD/#REMV take its offset in D0; every list in this image uses 0.");
        node.setDescription("Singly-linked list node as handled by the PLANC runtime primitives "
                + "#APPD (0x134E6) and #REMV (0x13500).");
        addType(dtm, node);

        // Frame header. Documentation only - Ghidra cannot apply a struct to an A6-relative frame.
        StructureDataType frm = new StructureDataType(cat, "PlancFrameHeader", 0, dtm);
        frm.add(new PointerDataType(), 4, "STP_nextFree",
                "+0x00 next-free cursor. The prologue's movea.l (A6),A6 follows this.");
        // NOT "unused" (ND-820026.1 Fig 8) and NOT an overflow guard (this script's old claim).
        // Verified 2026-07-26: it points at the CALLEE's frame; callers stage arguments through it.
        frm.add(new PointerDataType(), 4, "outgoingFrame",
                "+0x04 pointer to the CALLEE's frame - callers stage arguments through this");
        frm.add(new PointerDataType(), 4, "SMAX_savedSP",
                "+0x08 parked SP; #XRET unwinds through it");
        frm.add(new UnsignedIntegerDataType(), 4, "SYST",
                "+0x0C PLANC runtime system use");
        frm.add(new UnsignedIntegerDataType(), 4, "reserved_10",
                "+0x10 ERRCODE is written here by observed ON ROUTINEERROR handlers");
        frm.add(new UnsignedIntegerDataType(), 4, "paramsAndLocals",
                "+0x12 FIRST PARAMETER, then locals - NOT the manual's 24B/+0x14. Verified from "
                        + "the 0x31D4/0x28E6 and 0x28E6/0x40BE caller/callee offset match.");
        frm.setDescription("PLANC-MC routine frame, A6-relative (ND-820026.1 Figure 8, valid from "
                + "compiler version H). DOCUMENTATION ONLY - frames are bump-allocated in a "
                + "separate arena and are not addressable as a Ghidra stack frame. "
                + "CAVEAT: ND-60.117.5 (version G era) puts PREVB at +0 and STP at +4 instead; "
                + "slots 0x08/0x0C/0x10/0x14 agree in both manuals. This image matches ND-820026.");
        addType(dtm, frm);
    }

    private void addType(DataTypeManager dtm, StructureDataType s) {
        ghidra.program.model.data.DataType existing =
                dtm.getDataType(s.getCategoryPath(), s.getName());
        if (existing != null) {
            if (!replaceTypes) {
                skipped++;
                println("  type " + s.getName() + " already exists - left alone"
                        + "   (choose Replace to rebuild it from this script)");
                return;
            }
            println("  type " + s.getName() + " exists (" + existing.getLength()
                    + " bytes) - REPLACING with this script's definition ("
                    + s.getLength() + " bytes)");
            if (!dryRun) {
                try {
                    // Anything already typed with it is re-pointed at the replacement by Ghidra.
                    // replaceDataType throws if the old type is part of a dependency cycle.
                    dtm.replaceDataType(existing, s, true);
                } catch (ghidra.program.model.data.DataTypeDependencyException e) {
                    println("  CANNOT replace " + s.getName() + ": " + e.getMessage());
                    println("    (it is referenced by another type; delete that one first)");
                    skipped++;
                    return;
                }
            }
            replaced++;
            return;
        }
        if (!dryRun) {
            dtm.addDataType(s, null);
        }
        typesMade++;
        println("  type " + s.getName() + " created");
    }

    // ------------------------------------------------------------------ namespaces

    private void organiseNamespaces() throws Exception {
        FunctionIterator it = currentProgram.getListing().getFunctions(true);
        while (it.hasNext() && !monitor.isCancelled()) {
            Function f = it.next();
            String n = f.getName();
            String ns = namespaceFor(n);
            if (ns == null) {
                continue;
            }
            Symbol sym = f.getSymbol();
            if (sym == null || sym.getParentNamespace() == null) {
                continue;
            }
            if (ns.equals(sym.getParentNamespace().getName())) {
                continue; // already there
            }
            if (!dryRun) {
                Namespace target = getNamespace(ns);
                sym.setNamespace(target);
            }
            moved++;
        }
        println("  symbols moved into namespaces: " + moved);
    }

    /** Which namespace a vendor symbol belongs in, or null to leave it in Global. */
    private String namespaceFor(String n) {
        if (n.startsWith("#")) {
            return "PLANC";                       // the runtime library
        }
        if (n.startsWith("XMP") || n.startsWith("XMF")) {
            return "XMSG";                        // message primitives
        }
        if (n.startsWith("POS") || n.startsWith("PO")) {
            return "PIOCOS";                      // the card's operating system
        }
        if (n.startsWith("LN")) {
            return "LNMA";                        // Ethernet media access / link layer
        }
        if (n.matches("MON\\d+")) {
            return "MON";                         // monitor-call stubs
        }
        return null;
    }

    private Namespace getNamespace(String name) throws Exception {
        Namespace ns = currentProgram.getSymbolTable()
                .getNamespace(name, currentProgram.getGlobalNamespace());
        if (ns == null) {
            ns = currentProgram.getSymbolTable().createNameSpace(
                    currentProgram.getGlobalNamespace(), name, SourceType.USER_DEFINED);
        }
        return ns;
    }

    // ------------------------------------------------------------------ runtime documentation

    /**
     * Plate comments for the PLANC runtime. Prototypes are applied ONLY where the signature is
     * established from decoded call sites - a guessed prototype makes the decompiler worse.
     */
    private void documentRuntime() throws Exception {
        // { address, prototype-or-null, description }
        String[][] rt = {
            { "0x134e6", "void __APPD(void * headCell, void * node, int linkOffset)",
              "#APPD - append NODE to the tail of the singly-linked list whose HEAD-POINTER CELL "
              + "is in A0. A1 = node, D0 = byte offset of the link field within the node. Walks to "
              + "the tail (there is no tail pointer). Every caller in this image passes D0 = 0, so "
              + "next is at node+0. Pure pointer surgery - raises no interrupt and touches no "
              + "counter. VERIFIED from call sites." },
            { "0x13500", null,
              "#REMV - remove a node from a singly-linked list. The inverse of #APPD; same "
              + "head-cell / node / link-offset argument shape. Signature not fully decoded." },
            { "0x133e6", "int __IMU(int a, int b)",
              "#IMU - 32x32 integer multiply. D0, D1 in; D0 out. VERIFIED: used as a general "
              + "multiply helper, e.g. computing f(2,i) = 2i for the LANCE PADR byte index." },
            { "0x1342c", "int __IDV(int a, int b)",
              "#IDV - integer divide. D0, D1 in; D0 out." },
            { "0x1310c", null, "#IMOD - integer modulo." },
            { "0x13286", null, "#MOVE - block move / copy helper." },
            { "0x135a8", null,
              "#XRET - THE ERROR UNWIND. A5 holds this address in normal operation. Pops one PLANC "
              + "frame, restores SP from (0x8,A6), re-arms A5 to itself, then returns to "
              + "RETLINK+0 - which lands on the caller's 2-byte error slot, continuing the unwind "
              + "one frame at a time. It is the ONLY routine in the image that returns to +0. "
              + "ND-820026.1:3436 - 'The address of #XRET is always in the A5-register ... The "
              + "D0-register keeps the ERRCODE value.'" },
            { "0x13596", null,
              "#ERET - the other error vector. Set into A5 at the outermost frame and re-arms "
              + "itself. Where #XRET propagates, #ERET dispatches to a routine-error handler read "
              + "from (savedSP - 4). The manuals document only #XRET; the split is undocumented." },
            { "0x135b6", null,
              "#PRERR - runtime panic printer. Selects one of three messages (0x66370 / 0x66392 / "
              + "0x663B6) by D0 and prints the failing address in OCTAL (repeated asr.l #8, "
              + "andi.l #3, addi.w #0x30 - three-bit digits). Reached from the frame allocator on "
              + "stack overflow." },
            { "0x12fa6", null, "#ERROR - raise a PLANC runtime error. Takes a code in D0." },
            { "0x12ed8", null,
              "#SPASI - numeric field formatter (NOT the stack allocator, despite the name "
              + "suggesting 'space'). Width at (0x14,A6), fill character 0x20 at (0x2c,A6), output "
              + "through #OUTBYT, calls #ERROR on field overflow." },
            { "0x1302a", null, "#GETNO - parse a number from input." },
            { "0x1309e", null, "#OUTBYT - write one byte to the current output." },
            { "0x1249a", null, "#INBY - read one byte." },
            { "0x12644", null, "#UTBY - write one byte." },
        };

        for (String[] r : rt) {
            Address a = toAddr(r[0]);
            Function f = getFunctionAt(a);
            if (f == null) {
                println("  no function at " + r[0] + " - skipped");
                skipped++;
                continue;
            }
            String existing = f.getComment();
            if (existing == null || existing.trim().isEmpty()) {
                if (!dryRun) {
                    f.setComment("PLANC RUNTIME LIBRARY\n" + r[2]);
                }
                plated++;
            } else {
                skipped++;
            }
            // Signatures are applied with CUSTOM STORAGE, not by parsing a C string.
            // These three are hand-written LEAF routines that take their arguments in REGISTERS
            // and end in a plain RTS - they do not use the __planc frame convention. Applying a
            // normal signature would make Ghidra place the parameters on the stack, which is
            // wrong and would make the decompiler output worse than leaving it alone. Naming the
            // exact register for each argument is the only correct way to do it.
            if (r[1] != null) {
                if (dryRun) {
                    println("  would apply: " + r[1]);
                    protod++;
                } else {
                    try {
                        applyRegisterSignature(f);
                        println("  applied: " + r[1]);
                        protod++;
                    } catch (Exception e) {
                        println("  signature FAILED on " + f.getName() + ": " + e.getMessage());
                    }
                }
            }
        }
    }

    /**
     * Apply the register-storage signature for one of the PLANC leaf primitives.
     * Storage established by decoding call sites, not guessed:
     *   #APPD  A0 = address of the head-pointer CELL, A1 = node, D0 = link-field byte offset
     *   #IMU   D0, D1 -> D0
     *   #IDV   D0, D1 -> D0
     */
    private void applyRegisterSignature(Function f) throws Exception {
        String n = f.getName();
        java.util.List<ghidra.program.model.listing.Variable> params = new java.util.ArrayList<>();
        ghidra.program.model.listing.Variable ret;

        if (n.equals("#APPD")) {
            params.add(reg("headCell", new PointerDataType(), "A0"));
            params.add(reg("node", new PointerDataType(), "A1"));
            params.add(reg("linkOffset", ghidra.program.model.data.IntegerDataType.dataType, "D0"));
            ret = new ghidra.program.model.listing.ReturnParameterImpl(
                    ghidra.program.model.data.VoidDataType.dataType, currentProgram);
        } else if (n.equals("#IMU") || n.equals("#IDV")) {
            params.add(reg("a", ghidra.program.model.data.IntegerDataType.dataType, "D0"));
            params.add(reg("b", ghidra.program.model.data.IntegerDataType.dataType, "D1"));
            ret = new ghidra.program.model.listing.ReturnParameterImpl(
                    ghidra.program.model.data.IntegerDataType.dataType,
                    new ghidra.program.model.listing.VariableStorage(
                            currentProgram, currentProgram.getRegister("D0")),
                    currentProgram);
        } else {
            return;
        }

        f.updateFunction(null, ret, params,
                Function.FunctionUpdateType.CUSTOM_STORAGE, true, SourceType.USER_DEFINED);
    }

    private ghidra.program.model.listing.Variable reg(String name, ghidra.program.model.data.DataType dt,
            String regName) throws Exception {
        ghidra.program.model.lang.Register r = currentProgram.getRegister(regName);
        if (r == null) {
            throw new Exception("no register " + regName + " in this language");
        }
        return new ghidra.program.model.listing.ParameterImpl(name, dt,
                new ghidra.program.model.listing.VariableStorage(currentProgram, r), currentProgram);
    }
}
