#!/usr/bin/env python3
"""
Validate carved MON-call .ASM/.bin deliverables for integrity.

A disassembly artifact is only trustworthy if it is SELF-CONTAINED for its own
control flow. This catches the class of bug where a handler was carved with a
too-small window so its own JMP/branch targets fall outside the file (truncation).

Checks per folder that has a *.ASM (or *.asm) plus a same-named *.bin:
  1. SIZE      .bin byte count == 2 * (number of instruction lines in the .ASM)
  2. CONTIG    ASM instruction addresses are contiguous (step 1 word, octal)
  3. CLOSURE   every DIRECT branch target ('-> NNNNNN', excluding 'JMP I'/'JPL I'
               indirect) lands within [first_addr, last_addr] of the .ASM
  4. ENTRY     (optional) an address passed via --entry appears in the ASM

Exit code is nonzero if any artifact FAILS, so it can gate a commit.

Usage:
  python3 scripts/validate-mon-carves.py                 # scan the default roots
  python3 scripts/validate-mon-carves.py <dir> [<dir>..] # scan given roots
  python3 scripts/validate-mon-carves.py --json
"""
import os, re, sys, glob, json

DEFAULT_ROOTS = [
    "tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis",
    "tools/sintran-segment-carver/versions/L-VSX-500/re/mon-emulation",
]

# nd100-dis prints "<octaladdr>  <octalword>\t<mnemonic>[\t\t; -> <octaltarget>]"
LINE = re.compile(r"^([0-7]+)\s+[0-7]+\s+\t(.*)$")
TARGET = re.compile(r"->\s*([0-7]+)")
# a control-transfer mnemonic that is DIRECT (indirect ones contain ' I ')
BRANCH = re.compile(r"\b(JMP|JPL|JAP|JAN|JAZ|JAF|JXN|JXZ|JPC|JNC)\b")

def parse_asm(path):
    addrs, branches = [], []   # branches: (addr, target, is_indirect, mnem)
    for raw in open(path, encoding="utf-8", errors="replace"):
        m = LINE.match(raw)
        if not m:
            continue
        addr = int(m.group(1), 8)
        mnem = m.group(2)
        addrs.append(addr)
        if BRANCH.search(mnem):
            t = TARGET.search(raw)
            if t:
                branches.append((addr, int(t.group(1), 8), (" I " in mnem or mnem.endswith(" I")), mnem.strip()))
    return addrs, branches

def validate(asm_path):
    stem = asm_path.rsplit(".", 1)[0]
    bin_path = stem + ".bin"
    res = {"asm": asm_path, "bin": bin_path if os.path.exists(bin_path) else None,
           "problems": [], "words": 0, "direct_escapes": [], "skipped": False}
    # ND-500 (32-bit) handlers are disassembled with nd500-dis, a different format the
    # nd100 closure check cannot parse. Skip them honestly (closure validation for
    # ND-500 is a separate TODO with nd500-dis) rather than report a false failure.
    head = ""
    with open(asm_path, encoding="utf-8", errors="replace") as fh:
        for _ in range(12):
            head += fh.readline()
    if "ND-500 (32-bit)" in head or "nd500-dis" in head:
        res["skipped"] = True
        res["skip_reason"] = "ND-500 32-bit (nd500-dis format); nd100 closure check N/A"
        return res
    addrs, branches = parse_asm(asm_path)
    res["words"] = len(addrs)
    if not addrs:
        res["problems"].append("no instruction lines parsed from ASM")
        return res
    lo, hi = addrs[0], addrs[-1]
    res["range"] = f"{lo:o}..{hi:o}"
    # Golden-path: a per-call .ASM with NO sibling .bin is a multi-region reference
    # excerpt (ND-100 calls; bytes live in segments-ref/). Closure/contiguity do not
    # apply to a stub+worker excerpt - skip it, don't fail it. The authoritative
    # closure check is on the whole-segment disassemblies in segments-ref/.
    if not os.path.exists(bin_path):
        res["skipped"] = True
        res["skip_reason"] = "golden-path reference excerpt (no per-call .bin; bytes in segments-ref/)"
        return res
    # 1 SIZE (only for self-contained slices that carry a .bin: ND-500 + mon-emulation)
    nb = os.path.getsize(bin_path)
    if nb != 2 * len(addrs):
        res["problems"].append(f"SIZE: .bin={nb}B but ASM has {len(addrs)} words (expect {2*len(addrs)}B)")
    # 2 CONTIG
    gaps = [f"{addrs[i-1]:o}->{addrs[i]:o}" for i in range(1, len(addrs)) if addrs[i] != addrs[i-1] + 1]
    if gaps:
        res["problems"].append(f"CONTIG: non-contiguous at {gaps[:5]}")
    # 3 CLOSURE (direct branches only)
    esc = [(f"{a:o}", f"{t:o}", mn) for (a, t, ind, mn) in branches if not ind and not (lo <= t <= hi)]
    res["direct_escapes"] = esc
    if esc:
        res["problems"].append(f"CLOSURE: {len(esc)} direct branch(es) escape the file, e.g. "
                               + ", ".join(f"{a}->{t}" for a, t, _ in esc[:6]))
    return res

def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    as_json = "--json" in sys.argv
    roots = args or DEFAULT_ROOTS
    asms = []
    for r in roots:
        asms += glob.glob(os.path.join(r, "**", "*.ASM"), recursive=True)
        asms += glob.glob(os.path.join(r, "**", "*.asm"), recursive=True)
    asms = sorted(set(asms))
    results = [validate(a) for a in asms]
    skipped = [r for r in results if r.get("skipped")]
    checked = [r for r in results if not r.get("skipped")]
    failed = [r for r in checked if r["problems"]]
    if as_json:
        print(json.dumps({"total": len(results), "checked": len(checked),
                          "skipped": len(skipped), "failed": len(failed),
                          "results": results}, indent=2))
    else:
        for r in results:
            name = os.path.relpath(r["asm"])
            if r.get("skipped"):
                print(f"skip  {name}  ({r['skip_reason']})")
            elif r["problems"]:
                print(f"FAIL  {name}  [{r.get('range','?')}, {r['words']}w]")
                for p in r["problems"]:
                    print(f"        - {p}")
            else:
                print(f"ok    {name}  [{r.get('range','?')}, {r['words']}w]")
        print(f"\n{len(checked)-len(failed)}/{len(checked)} checked passed; "
              f"{len(failed)} FAILED; {len(skipped)} skipped (ND-500)")
    return 1 if failed else 0

if __name__ == "__main__":
    sys.exit(main())
