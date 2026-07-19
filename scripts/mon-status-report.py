#!/usr/bin/env python3
"""
mon-status-report.py - Status report for the SINTRAN MON-call RE deliverable.

Extracts, from the sintran-segment-carver version folders, the status of every
per-call MON reverse-engineering deliverable AND every MON call on the friend's
canonical 31-call master list - including calls whose folder does not yet exist
(reported as "not-started" / not yet started).

For each call it reports: call number (octal), ND-100 vs ND-500, the deliverable
folder (or MISSING), presence + filenames of README.md / .ASM / .bin, whether the
README has a "How this was carved" section, a derived provenance, a STATUS
classification, and a list of concrete PROBLEMS.

It also summarises the carved-segment set from segments/manifest.json.

Output:
    (default)   human-readable Markdown to stdout
    --json      machine-readable JSON to stdout
    --version   which version folder to scan (default L-VSX-500)
    --all       scan every version folder present under versions/

Stdlib only. Plain Python (no list comprehensions used as control flow, plain
for-loops). Missing files are handled gracefully.
"""

import argparse
import glob
import json
import os
import re
import sys

# --------------------------------------------------------------------------
# Configuration (absolute, configured once at the top).
# --------------------------------------------------------------------------
REPO_ROOT = "/mnt/e/Dev/Ronny/NDInsight"
VERSIONS_ROOT = os.path.join(
    REPO_ROOT, "tools", "sintran-segment-carver", "versions"
)
YAML_DIR = os.path.join(REPO_ROOT, "Developer", "MON", "calls")

# Sub-paths inside a version folder.
MON_ANALYSIS_SUBDIR = os.path.join("re", "mon-analysis")
MANIFEST_SUBPATH = os.path.join("segments", "manifest.json")
SEGMENTS_SUBDIR = "segments"

# ND-500 calls are > 0377 octal (256 decimal); GOTAB only indexes 0..255.
ND500_THRESHOLD = 0o400

# The friend's canonical master list (octal, exactly as given). These are the
# 31 "friend-requested" calls.
FRIEND_ND100 = [0o5, 0o6, 0o13, 0o14, 0o15, 0o42, 0o45, 0o51,
                0o67, 0o74, 0o75, 0o120, 0o144, 0o304, 0o313, 0o327]
FRIEND_ND500 = [0o410, 0o411, 0o416, 0o417, 0o425, 0o426, 0o427,
                0o500, 0o501, 0o505, 0o510, 0o511, 0o512, 0o513, 0o515]
FRIEND_ALL = FRIEND_ND100 + FRIEND_ND500
SHOWSTOPPERS = [0o511, 0o512, 0o513]

# Folder name convention: <octal-digits>B-<Name>, e.g. 005B-ReadScratchFile.
FOLDER_RE = re.compile(r"^([0-7]+)B-(.*)$")

# Required README section marker.
HOWCARVED_MARKER = "how this was carved"

# Status constants.
ST_NOT_STARTED = "not-started"
ST_STUB = "stub"
ST_PARTIAL = "partial"
ST_COMPLETE = "complete"
STATUS_ORDER = [ST_COMPLETE, ST_PARTIAL, ST_STUB, ST_NOT_STARTED]


def octal_label(num):
    """Zero-padded 3-digit octal + 'B', e.g. 5 -> '005B', 500 -> '500B'."""
    return "%03oB" % num


def octal_bare(num):
    """Octal without leading zeros + 'B', matching YAML naming (e.g. '5B')."""
    return "%oB" % num


def is_nd500(num):
    return num >= ND500_THRESHOLD


def find_yaml(num):
    """Return the YAML filename for this call, or None. YAML uses bare octal."""
    pattern = os.path.join(YAML_DIR, octal_bare(num) + "_*.yaml")
    matches = sorted(glob.glob(pattern))
    if matches:
        return os.path.basename(matches[0])
    return None


def list_version_dirs():
    """Return sorted list of version folder names that contain a mon-analysis dir
    or a segments manifest (i.e. look like real version folders)."""
    result = []
    if not os.path.isdir(VERSIONS_ROOT):
        return result
    entries = sorted(os.listdir(VERSIONS_ROOT))
    for name in entries:
        vpath = os.path.join(VERSIONS_ROOT, name)
        if not os.path.isdir(vpath):
            continue
        mon_dir = os.path.join(vpath, MON_ANALYSIS_SUBDIR)
        manifest = os.path.join(vpath, MANIFEST_SUBPATH)
        if os.path.isdir(mon_dir) or os.path.isfile(manifest):
            result.append(name)
    return result


def classify_files(folder):
    """Given a folder path, return dict with readme/asm/bin filenames (or None),
    a flag for empty, and the raw file list."""
    info = {
        "readme": None,
        "asm": None,
        "bin": None,
        "pseudoc": None,
        "other": [],
        "empty": True,
        "all_files": [],
    }
    if not folder or not os.path.isdir(folder):
        return info
    files = sorted(os.listdir(folder))
    info["all_files"] = files
    if files:
        info["empty"] = False
    for fname in files:
        low = fname.lower()
        if low == "readme.md":
            info["readme"] = fname
        elif low.endswith(".pseudo.c"):
            info["pseudoc"] = fname
        elif low.endswith(".asm"):
            # First .asm wins; there is normally exactly one.
            if info["asm"] is None:
                info["asm"] = fname
        elif low.endswith(".bin"):
            if info["bin"] is None:
                info["bin"] = fname
        else:
            info["other"].append(fname)
    return info


def read_text(path):
    try:
        f = open(path, "r", encoding="utf-8", errors="replace")
        try:
            return f.read()
        finally:
            f.close()
    except (OSError, IOError):
        return ""


def extract_status_line(readme_text):
    """Pull the '**Status:** ...' fragment out of the README header, if present."""
    m = re.search(r"\*\*Status:\*\*\s*(.+)", readme_text)
    if not m:
        return ""
    line = m.group(1).strip()
    # Trim at a trailing markdown bold boundary or newline noise.
    return line.strip()


def derive_provenance(status_line, howcarved_text):
    """Derive a provenance token. The README **Status:** line is the primary,
    authoritative signal for the artifact's provenance; the how-carved prose is
    only a fallback (it often narrates negatives like 'reads as zero in the flat
    image' even for a successful overlay carve)."""
    st = status_line.lower()

    # 1) Trust the status line first.
    if ("real l" in st or "carved" in st or "disassembled from" in st or
            "handler is code in" in st or "code in resident" in st or
            "code in `" in st or "verified" in st):
        # A VERIFIED / REAL-L status line always means a real carve, even if the
        # how-carved prose also explains what was absent from the flat image.
        if ("not resolved" in st or "not present" in st or "not found" in st):
            return "documented-stub-absent"
        return "real-L-carve"
    if ("not resolved" in st or "not present" in st or "not found" in st or
            "not carved" in st or "no distinct handler" in st or
            "no handler" in st or "empty" in st):
        return "documented-stub-absent"

    # 2) Fall back to how-carved prose.
    blob = (status_line + " " + howcarved_text).lower()
    if ("live dap" in blob or "dap recovery" in blob or
            "live trace" in blob or "breakpoint it live" in blob or
            "captured live" in blob):
        return "live-DAP-capture"
    if ("honest negative" in blob or "no l bytes recovered" in blob or
            "not carved" in blob):
        return "documented-stub-absent"
    if ("real l" in blob or "carved" in blob or "disassembled from" in blob or
            "handler is code in" in blob or "overlay" in blob):
        return "real-L-carve"
    return "unknown"


def extract_howcarved_section(readme_text):
    """Return the text of the 'How this was carved' section (or '')."""
    lines = readme_text.splitlines()
    out = []
    capturing = False
    for line in lines:
        if line.lstrip().startswith("#") and HOWCARVED_MARKER in line.lower():
            capturing = True
            continue
        if capturing:
            # Stop at the next heading of same/greater level.
            if line.lstrip().startswith("#"):
                break
            out.append(line)
    return "\n".join(out)


def check_broken_links(readme_text, folder):
    """Cheap relative-link check: for [text](path) where path is a local file
    reference (no scheme, not an anchor), flag if the target does not exist."""
    problems = []
    if not folder:
        return problems
    for m in re.finditer(r"\]\(([^)]+)\)", readme_text):
        target = m.group(1).strip()
        if not target:
            continue
        # Skip URLs, anchors, mailto.
        if "://" in target or target.startswith("#") or target.startswith("mailto:"):
            continue
        # Strip any anchor fragment.
        path_part = target.split("#", 1)[0]
        if not path_part:
            continue
        # Only check things that look like file paths.
        low = path_part.lower()
        checkable = (
            low.endswith(".md") or low.endswith(".bin") or
            low.endswith(".asm") or low.endswith(".json")
        )
        if not checkable:
            continue
        resolved = os.path.normpath(os.path.join(folder, path_part))
        if not os.path.exists(resolved):
            problems.append("broken link: %s" % target)
    return problems


def looks_like_npl_source(asm_path, readme_text):
    """Flag only if the carved ARTIFACT itself looks like NPL source rather than
    a disassembly of carved L bytes. The repo notes NPL is a DIFFERENT version,
    so a .ASM that is actually NPL text (not nd100-dis output) is a real problem.

    A README merely *citing* NPL addresses as a cross-reference caveat (e.g.
    'the NPL source is a different revision') is NOT flagged - that is the
    correct, documented caveat, not a contamination."""
    hits = []
    if asm_path and os.path.isfile(asm_path):
        text = read_text(asm_path)
        low = text.lower()
        # nd100-dis output has no NPL declarations; NPL source uses these tokens.
        npl_markers = ("npl source", "%npl", "\tinteger array", "base register",
                       "9nrl", "9dis", "9end", "subr ")
        marker_hits = 0
        for mk in npl_markers:
            if mk in low:
                marker_hits += 1
        if "npl source" in low or marker_hits >= 2:
            hits.append(".ASM may be NPL source, not a carved-L disassembly "
                        "(repo notes NPL is a different version)")
    return hits


def analyze_call(num, folder_map, version):
    """Build the status record for one call number in one version."""
    label = octal_label(num)
    bare = octal_bare(num)
    kind = "ND-500" if is_nd500(num) else "ND-100"
    friend = num in FRIEND_ALL
    showstopper = num in SHOWSTOPPERS

    folder = folder_map.get(num)
    folder_name = os.path.basename(folder) if folder else None
    fileinfo = classify_files(folder)

    readme_path = None
    readme_text = ""
    if fileinfo["readme"]:
        readme_path = os.path.join(folder, fileinfo["readme"])
        readme_text = read_text(readme_path)

    status_line = extract_status_line(readme_text) if readme_text else ""
    howcarved_text = extract_howcarved_section(readme_text) if readme_text else ""
    has_howcarved = bool(howcarved_text.strip()) or (
        readme_text and HOWCARVED_MARKER in readme_text.lower()
    )

    asm_path = os.path.join(folder, fileinfo["asm"]) if fileinfo["asm"] else None
    bin_path = os.path.join(folder, fileinfo["bin"]) if fileinfo["bin"] else None

    bin_size = None
    if bin_path and os.path.isfile(bin_path):
        try:
            bin_size = os.path.getsize(bin_path)
        except OSError:
            bin_size = None

    provenance = derive_provenance(status_line, howcarved_text)

    # ---- STATUS classification (GOLDEN PATH, see re/mon-analysis/GOLDEN-PATH.md) --
    # A call folder is: README.md + <call>.ASM + <call>.pseudo.c  (code call),
    # OR README-only  (documented fall-through / empty-slot stub). Per-call .bin is
    # ND-500-only (a generated slice); ND-100 calls reference segments-ref instead.
    has_readme = fileinfo["readme"] is not None
    has_asm = fileinfo["asm"] is not None
    has_pseudoc = fileinfo["pseudoc"] is not None

    if folder is None or fileinfo["empty"]:
        status = ST_NOT_STARTED
    elif has_readme and not has_asm and not has_pseudoc:
        status = ST_STUB                      # documented README-only negative
    elif has_readme and has_asm and has_pseudoc:
        status = ST_COMPLETE                  # golden-path complete
    else:
        status = ST_PARTIAL

    # ---- PROBLEMS ----------------------------------------------------------
    problems = []
    if status == ST_NOT_STARTED:
        if folder is None:
            problems.append("not yet started (no folder)")
        else:
            problems.append("not yet started (empty folder)")
    else:
        if not has_readme:
            problems.append("missing README.md")
        # A stub is an intentional documented negative; do not nag about
        # missing asm/pseudo-c there.
        if status != ST_STUB:
            if not has_asm:
                problems.append("missing .ASM")
            if not has_pseudoc:
                problems.append("missing .pseudo.c")
        if fileinfo["bin"] is not None and bin_size == 0:
            problems.append(".bin is empty (0 bytes)")
        if has_readme and "## Code location" not in (readme_text or ""):
            problems.append("README missing '## Code location' section (golden path)")
        # NPL-source contamination check.
        npl_hits = looks_like_npl_source(asm_path, readme_text)
        for h in npl_hits:
            problems.append(h)
        # Broken relative links.
        for p in check_broken_links(readme_text, folder):
            problems.append(p)

    return {
        "call_octal": label,
        "call_octal_bare": bare,
        "call_decimal": num,
        "kind": kind,
        "friend_requested": friend,
        "showstopper": showstopper,
        "version": version,
        "folder": folder_name if folder_name else "MISSING",
        "folder_path": folder if folder else None,
        "status": status,
        "provenance": provenance,
        "status_line": status_line,
        "readme": fileinfo["readme"],
        "asm": fileinfo["asm"],
        "bin": fileinfo["bin"],
        "bin_size": bin_size,
        "has_howcarved": has_howcarved,
        "yaml": find_yaml(num),
        "problems": problems,
    }


def build_folder_map(version):
    """Map call-number -> folder path for every deliverable folder in a version.
    Also return the list of extra (non-friend) call numbers found."""
    folder_map = {}
    extras = []
    mon_dir = os.path.join(VERSIONS_ROOT, version, MON_ANALYSIS_SUBDIR)
    if not os.path.isdir(mon_dir):
        return folder_map, extras
    for name in sorted(os.listdir(mon_dir)):
        full = os.path.join(mon_dir, name)
        if not os.path.isdir(full):
            continue
        m = FOLDER_RE.match(name)
        if not m:
            continue
        try:
            num = int(m.group(1), 8)
        except ValueError:
            continue
        folder_map[num] = full
        if num not in FRIEND_ALL:
            extras.append(num)
    return folder_map, sorted(extras)


def analyze_manifest(version):
    """Summarise the carved-segment set from segments/manifest.json."""
    result = {
        "manifest_path": None,
        "present": False,
        "endianness": None,
        "segment_count": 0,
        "segments_with_file": 0,
        "total_bytes": 0,
        "missing_files": [],
    }
    manifest_path = os.path.join(VERSIONS_ROOT, version, MANIFEST_SUBPATH)
    result["manifest_path"] = manifest_path
    if not os.path.isfile(manifest_path):
        return result
    result["present"] = True
    try:
        data = json.load(open(manifest_path, "r", encoding="utf-8", errors="replace"))
    except (OSError, IOError, ValueError):
        result["present"] = False
        return result
    result["endianness"] = data.get("endianness")
    segments = data.get("segments", [])
    result["segment_count"] = len(segments)
    seg_dir = os.path.join(VERSIONS_ROOT, version, SEGMENTS_SUBDIR)
    for seg in segments:
        fname = seg.get("file")
        if not fname:
            continue
        result["segments_with_file"] += 1
        disk_path = os.path.join(seg_dir, fname)
        if os.path.isfile(disk_path):
            try:
                result["total_bytes"] += os.path.getsize(disk_path)
            except OSError:
                pass
        else:
            result["missing_files"].append(fname)
    return result


def analyze_version(version):
    """Produce the full record for one version folder."""
    folder_map, extras = build_folder_map(version)

    # Master checklist = friend calls + any extra folders present.
    all_nums = []
    for n in FRIEND_ALL:
        all_nums.append(n)
    for n in extras:
        if n not in all_nums:
            all_nums.append(n)
    all_nums.sort()

    calls = []
    for num in all_nums:
        calls.append(analyze_call(num, folder_map, version))

    # Summary counts.
    counts = {ST_COMPLETE: 0, ST_PARTIAL: 0, ST_STUB: 0, ST_NOT_STARTED: 0}
    friend_counts = {ST_COMPLETE: 0, ST_PARTIAL: 0, ST_STUB: 0, ST_NOT_STARTED: 0}
    for c in calls:
        counts[c["status"]] += 1
        if c["friend_requested"]:
            friend_counts[c["status"]] += 1

    manifest = analyze_manifest(version)

    return {
        "version": version,
        "counts": counts,
        "friend_counts": friend_counts,
        "friend_total": len(FRIEND_ALL),
        "extra_calls": [octal_label(n) for n in extras],
        "calls": calls,
        "manifest": manifest,
    }


# --------------------------------------------------------------------------
# Markdown rendering
# --------------------------------------------------------------------------
def md_escape(text):
    if text is None:
        return ""
    return text.replace("|", "\\|")


def render_call_row(c):
    flags = []
    if c["friend_requested"]:
        flags.append("friend")
    else:
        flags.append("extra")
    if c["showstopper"]:
        flags.append("SHOWSTOPPER")
    flagstr = ", ".join(flags)

    readme = c["readme"] or "-"
    asm = c["asm"] or "-"
    binf = c["bin"] or "-"
    if c["bin"] and c["bin_size"] is not None:
        binf = "%s (%d B)" % (c["bin"], c["bin_size"])
    how = "yes" if c["has_howcarved"] else "no"
    yaml = c["yaml"] or "-"
    probs = "; ".join(c["problems"]) if c["problems"] else "-"

    return "| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |" % (
        c["call_octal"],
        flagstr,
        c["status"],
        c["provenance"],
        md_escape(c["folder"]),
        md_escape(readme),
        md_escape(asm),
        md_escape(binf),
        how,
        md_escape(probs),
    )


def render_group(title, calls):
    lines = []
    lines.append("### %s" % title)
    lines.append("")
    lines.append("| Call | Flags | Status | Provenance | Folder | README | .ASM | .bin | How-carved | Problems |")
    lines.append("|------|-------|--------|------------|--------|--------|------|------|-----------|----------|")
    for c in calls:
        lines.append(render_call_row(c))
    lines.append("")
    return lines


def render_version_md(rec):
    lines = []
    v = rec["version"]
    lines.append("## Version %s" % v)
    lines.append("")

    counts = rec["counts"]
    fc = rec["friend_counts"]
    total = sum(counts.values())
    lines.append(
        "**Summary (all %d tracked calls):** complete=%d, partial=%d, stub=%d, not-started=%d"
        % (total, counts[ST_COMPLETE], counts[ST_PARTIAL],
           counts[ST_STUB], counts[ST_NOT_STARTED])
    )
    lines.append("")
    lines.append(
        "**Friend master list (%d calls):** complete=%d, partial=%d, stub=%d, not-started=%d"
        % (rec["friend_total"], fc[ST_COMPLETE], fc[ST_PARTIAL],
           fc[ST_STUB], fc[ST_NOT_STARTED])
    )
    lines.append("")
    if rec["extra_calls"]:
        lines.append("**Extra call folders beyond the 31:** %s" % ", ".join(rec["extra_calls"]))
        lines.append("")

    nd100 = []
    nd500 = []
    for c in rec["calls"]:
        if c["kind"] == "ND-500":
            nd500.append(c)
        else:
            nd100.append(c)

    lines.extend(render_group("ND-100 calls (< 0400)", nd100))
    lines.extend(render_group("ND-500 calls (> 0377)", nd500))

    # Carved-data section.
    man = rec["manifest"]
    lines.append("### Carved segment data (manifest.json)")
    lines.append("")
    if not man["present"]:
        lines.append("_manifest.json not found at_ `%s`" % man["manifest_path"])
        lines.append("")
    else:
        mb = man["total_bytes"] / (1024.0 * 1024.0)
        lines.append("- Segments in manifest: **%d**" % man["segment_count"])
        lines.append("- Segments with a carved .bin: **%d**" % man["segments_with_file"])
        lines.append("- Total carved bytes on disk: **%d** (%.2f MB)" % (man["total_bytes"], mb))
        lines.append("- Endianness: **%s**" % (man["endianness"] or "unknown"))
        if man["missing_files"]:
            lines.append("- **MISSING .bin files (in manifest, absent on disk):** %s"
                         % ", ".join(man["missing_files"]))
        else:
            lines.append("- Missing .bin files: none")
        lines.append("")

    return lines


def render_markdown(records):
    lines = []
    lines.append("# SINTRAN MON-call deliverable status report")
    lines.append("")
    lines.append("_Master checklist: the friend's 31 canonical MON calls "
                 "(ND-100 and ND-500), plus any extra call folders present. "
                 "Calls with no folder or an empty folder are reported as "
                 "not-started (not yet started)._")
    lines.append("")
    for rec in records:
        lines.extend(render_version_md(rec))
        lines.append("---")
        lines.append("")
    return "\n".join(lines)


# --------------------------------------------------------------------------
# Main
# --------------------------------------------------------------------------
def main(argv):
    parser = argparse.ArgumentParser(
        description="Status report for the SINTRAN MON-call RE deliverable."
    )
    parser.add_argument("--version", default="L-VSX-500",
                        help="Version folder to scan (default: L-VSX-500)")
    parser.add_argument("--all", action="store_true",
                        help="Scan every version folder present under versions/")
    parser.add_argument("--json", action="store_true",
                        help="Emit machine-readable JSON instead of Markdown")
    args = parser.parse_args(argv)

    if args.all:
        versions = list_version_dirs()
        if not versions:
            sys.stderr.write("No version folders found under %s\n" % VERSIONS_ROOT)
            return 1
    else:
        vpath = os.path.join(VERSIONS_ROOT, args.version)
        if not os.path.isdir(vpath):
            sys.stderr.write("Version folder not found: %s\n" % vpath)
            return 1
        versions = [args.version]

    records = []
    for v in versions:
        records.append(analyze_version(v))

    if args.json:
        out = {
            "versions_root": VERSIONS_ROOT,
            "friend_master_list": {
                "nd100_octal": [octal_label(n) for n in FRIEND_ND100],
                "nd500_octal": [octal_label(n) for n in FRIEND_ND500],
                "total": len(FRIEND_ALL),
                "showstoppers": [octal_label(n) for n in SHOWSTOPPERS],
            },
            "reports": records,
        }
        sys.stdout.write(json.dumps(out, indent=2))
        sys.stdout.write("\n")
    else:
        sys.stdout.write(render_markdown(records))
        sys.stdout.write("\n")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
