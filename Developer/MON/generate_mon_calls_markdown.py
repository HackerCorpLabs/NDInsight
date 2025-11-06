#!/usr/bin/env python3
"""
Generate comprehensive markdown documentation for all 230 SINTRAN III Monitor Calls.

Creates "ND MON Calls.md" with:
- Multiple indexes (by octal, by name, by short name)
- Full documentation for each call
- Cross-referenced links throughout
- Professional formatting
"""
import yaml
from pathlib import Path
from collections import defaultdict
import re

calls_dir = Path('Developer/MON/calls')
yaml_files = list(calls_dir.glob('*.yaml'))

print(f"Loading {len(yaml_files)} monitor calls...")

# Load all monitor calls
all_calls = []
for yaml_file in yaml_files:
    with open(yaml_file, 'r', encoding='utf-8') as f:
        data = yaml.safe_load(f)
        if data:
            all_calls.append(data)

print(f"Loaded {len(all_calls)} calls")

# Sort calls by octal number
def octal_to_int(octal_str):
    """Convert '131B' to integer for sorting"""
    return int(octal_str.rstrip('B'), 8)

all_calls.sort(key=lambda x: octal_to_int(x['octal']))

# Generate anchor-safe IDs
def make_anchor(octal, name, short_names=None):
    """Create anchor ID like '131b---datatransfer-abstr'"""
    anchor = f"{octal.lower()}---{name.lower()}"
    if short_names and len(short_names) > 0:
        anchor += f"-{short_names[0].lower()}"
    # Replace spaces and special chars with hyphens
    anchor = re.sub(r'[^a-z0-9-]', '-', anchor)
    anchor = re.sub(r'-+', '-', anchor)  # Remove consecutive hyphens
    return anchor.strip('-')

# Build reference map for linking
anchor_map = {}
for call in all_calls:
    octal = call['octal']
    name = call['name']
    short_names = call.get('short_names', [])
    anchor = make_anchor(octal, name, short_names)

    # Map by name, octal, and short name
    anchor_map[name] = anchor
    anchor_map[octal] = anchor
    for sn in short_names:
        anchor_map[sn] = anchor

print(f"Built anchor map with {len(anchor_map)} entries")

# Start generating markdown
output = []

# Header
output.append("# SINTRAN III Monitor Calls Reference")
output.append("")
output.append("**Complete documentation for all 230 monitor calls**")
output.append("")
output.append("*Extracted from: SINTRAN III Monitor Calls (ND-860228.2 EN)*")
output.append("")
output.append("---")
output.append("")

# Table of Contents
output.append("## Table of Contents")
output.append("")
output.append("- [Index by Octal Number](#index-by-octal-number)")
output.append("- [Index by Call Name](#index-by-call-name)")
output.append("- [Index by Short Name](#index-by-short-name)")
output.append("- [Monitor Call Documentation](#monitor-call-documentation)")
output.append("")
output.append("---")
output.append("")

# Index 1: By Octal Number
output.append("## Index by Octal Number")
output.append("")
output.append("| Octal | Name | Short Name | Description |")
output.append("|-------|------|------------|-------------|")

for call in all_calls:
    octal = call['octal']
    name = call['name']
    short_names = call.get('short_names', [])
    short_name = short_names[0] if short_names else ''
    desc = call.get('description', '').split('\n')[0][:60]  # First line, truncated
    anchor = make_anchor(octal, name, short_names)

    output.append(f"| [{octal}](#{anchor}) | [{name}](#{anchor}) | {short_name} | {desc}... |")

output.append("")
output.append("---")
output.append("")

# Index 2: By Call Name (alphabetical)
output.append("## Index by Call Name")
output.append("")
output.append("| Name | Octal | Short Name | Description |")
output.append("|------|-------|------------|-------------|")

sorted_by_name = sorted(all_calls, key=lambda x: x['name'].lower())
for call in sorted_by_name:
    octal = call['octal']
    name = call['name']
    short_names = call.get('short_names', [])
    short_name = short_names[0] if short_names else ''
    desc = call.get('description', '').split('\n')[0][:60]
    anchor = make_anchor(octal, name, short_names)

    output.append(f"| [{name}](#{anchor}) | [{octal}](#{anchor}) | {short_name} | {desc}... |")

output.append("")
output.append("---")
output.append("")

# Index 3: By Short Name (alphabetical)
output.append("## Index by Short Name")
output.append("")
output.append("| Short Name | Name | Octal | Description |")
output.append("|------------|------|-------|-------------|")

# Filter calls with short names and sort
calls_with_short = [c for c in all_calls if c.get('short_names') and len(c['short_names']) > 0]
sorted_by_short = sorted(calls_with_short, key=lambda x: x['short_names'][0].lower())

for call in sorted_by_short:
    octal = call['octal']
    name = call['name']
    short_names = call.get('short_names', [])
    short_name = short_names[0]
    desc = call.get('description', '').split('\n')[0][:60]
    anchor = make_anchor(octal, name, short_names)

    output.append(f"| **{short_name}** | [{name}](#{anchor}) | [{octal}](#{anchor}) | {desc}... |")

output.append("")
output.append("---")
output.append("")

# Main Documentation
output.append("## Monitor Call Documentation")
output.append("")

for call in all_calls:
    octal = call['octal']
    name = call['name']
    short_names = call.get('short_names', [])
    short_name = f" ({short_names[0]})" if short_names else ""
    anchor = make_anchor(octal, name, short_names)

    # Section header
    output.append(f"### {octal} - {name}{short_name}")
    output.append("")

    # Compatibility badges
    compat = call.get('compatibility', {})
    badges = []
    if compat.get('nd100'):
        badges.append("![ND-100](https://img.shields.io/badge/ND--100-Yes-green)")
    if compat.get('nd500'):
        badges.append("![ND-500](https://img.shields.io/badge/ND--500-Yes-green)")
    if compat.get('user_programs'):
        badges.append("![User](https://img.shields.io/badge/User-Yes-blue)")
    if compat.get('rt_programs'):
        badges.append("![RT](https://img.shields.io/badge/RT-Yes-blue)")
    if compat.get('system_programs'):
        badges.append("![System](https://img.shields.io/badge/System-Yes-blue)")

    if badges:
        output.append(" ".join(badges))
        output.append("")

    # Description
    description = call.get('description', 'No description available.')
    output.append(description)
    output.append("")

    # Parameters
    params = call.get('parameters', [])
    if params:
        output.append("#### Parameters")
        output.append("")
        output.append("| Name | Type | I/O | Description |")
        output.append("|------|------|-----|-------------|")

        for param in params:
            p_name = param.get('name', '')
            p_type = param.get('type', 'UNKNOWN')
            p_io = param.get('io', '')
            p_desc = param.get('description', '').replace('\n', ' ')[:80]

            output.append(f"| `{p_name}` | {p_type} | {p_io} | {p_desc} |")

        output.append("")

    # See Also (with links)
    see_also = call.get('see_also', [])
    if see_also:
        output.append("#### See Also")
        output.append("")
        linked_refs = []
        for ref in see_also:
            if ref in anchor_map:
                linked_refs.append(f"[{ref}](#{anchor_map[ref]})")
            else:
                linked_refs.append(ref)
        output.append(", ".join(linked_refs))
        output.append("")

    # Examples
    examples = call.get('examples', {})
    if examples:
        output.append("#### Examples")
        output.append("")

        lang_names = {
            'pascal': 'Pascal',
            'cobol': 'COBOL',
            'fortran': 'Fortran',
            'planc': 'PLANC',
            'assembly_500': 'ASSEMBLY-500',
            'mac': 'MAC'
        }

        for lang_key, lang_name in lang_names.items():
            example = examples.get(lang_key, {})
            if example and example.get('available'):
                code = example.get('code', '')
                if code:
                    output.append(f"<details>")
                    output.append(f"<summary><strong>{lang_name}</strong></summary>")
                    output.append("")
                    output.append(f"```{lang_key if lang_key != 'assembly_500' else 'asm'}")
                    output.append(code.strip())
                    output.append("```")
                    output.append("")
                    output.append("</details>")
                    output.append("")

    # Source information
    source = call.get('source', {})
    if source:
        doc = source.get('document', 'Unknown')
        page = source.get('page', 'N/A')
        output.append(f"*Source: {doc}, Page {page}*")
        output.append("")

    # Back to top link
    output.append("[↑ Back to Top](#table-of-contents)")
    output.append("")
    output.append("---")
    output.append("")

# Footer
output.append("## About This Document")
output.append("")
output.append("This documentation was automatically generated from YAML files extracted from the")
output.append("SINTRAN III Monitor Calls reference manual (ND-860228.2 EN).")
output.append("")
output.append(f"- **Total Monitor Calls:** {len(all_calls)}")
output.append(f"- **Calls with Short Names:** {len(calls_with_short)}")
output.append(f"- **Extraction Date:** 2025-01-06")
output.append("")
output.append("For technical details about the extraction process, see:")
output.append("- `FINAL-EXTRACTION-REPORT.md`")
output.append("- `SCHEMA-CLEANUP-ANALYSIS.md`")
output.append("")

# Write output
output_file = Path('Developer/MON/ND MON Calls.md')
with open(output_file, 'w', encoding='utf-8') as f:
    f.write('\n'.join(output))

print()
print("=" * 80)
print("✅ MARKDOWN GENERATION COMPLETE")
print("=" * 80)
print()
print(f"📄 Output file: {output_file}")
print(f"📊 Total calls documented: {len(all_calls)}")
print(f"📝 Lines generated: {len(output):,}")
print(f"📦 File size: {output_file.stat().st_size / 1024:.1f} KB")
print()
print("✓ Three indexes created (octal, name, short name)")
print("✓ All references linked")
print("✓ Compatibility badges added")
print("✓ Code examples in collapsible sections")
print("✓ Navigation links throughout")
print()
