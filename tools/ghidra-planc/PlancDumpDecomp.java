//Dumps the decompiled C of every function in the program to one text file, for offline reading.
//
//Written for the 211185 TCP/IP firmware RE (tcp-ser-all-banks-b05-68k.bin) so analysis can
//continue while the Ghidra GUI is closed. Read-only: changes nothing in the database.
//
//USAGE (headless):
//  -postScript PlancDumpDecomp.java <outputFile>
//  Argument 1 (required): full path of the output text file.
//  Argument 2 (optional): decompiler timeout per function in seconds (default 30).
//
//@category ND.PLANC
//@author NDInsight

import ghidra.app.decompiler.DecompInterface;
import ghidra.app.decompiler.DecompileResults;
import ghidra.app.script.GhidraScript;
import ghidra.program.model.listing.Function;
import ghidra.program.model.listing.FunctionIterator;

import java.io.PrintWriter;

public class PlancDumpDecomp extends GhidraScript {

    @Override
    protected void run() throws Exception {
        String[] args = getScriptArgs();
        if (args.length < 1) {
            printerr("Usage: PlancDumpDecomp.java <outputFile> [timeoutSeconds]");
            return;
        }
        String outPath = args[0];
        int timeout = args.length >= 2 ? Integer.parseInt(args[1]) : 30;

        DecompInterface decomp = new DecompInterface();
        decomp.openProgram(currentProgram);

        int ok = 0, failed = 0;
        try (PrintWriter out = new PrintWriter(outPath, "UTF-8")) {
            out.println("// Decompiled dump of " + currentProgram.getName());
            out.println("// Functions: " + currentProgram.getFunctionManager().getFunctionCount());
            out.println();

            FunctionIterator it = currentProgram.getFunctionManager().getFunctions(true);
            while (it.hasNext() && !monitor.isCancelled()) {
                Function f = it.next();
                out.println("// ============================================================");
                out.println("// FUNCTION " + f.getName() + " @ " + f.getEntryPoint());
                out.println("// ============================================================");
                DecompileResults res = decomp.decompileFunction(f, timeout, monitor);
                if (res != null && res.decompileCompleted() && res.getDecompiledFunction() != null) {
                    out.println(res.getDecompiledFunction().getC());
                    ok++;
                }
                else {
                    out.println("// DECOMPILE FAILED: "
                            + (res == null ? "null result" : res.getErrorMessage()));
                    failed++;
                }
                out.println();
            }
        }
        finally {
            decomp.dispose();
        }
        println("PlancDumpDecomp: " + ok + " functions dumped, " + failed + " failed -> " + outPath);
    }
}
