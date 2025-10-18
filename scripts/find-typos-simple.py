#!/usr/bin/env python3
"""
Find unlinked entries in Section 2 and check if they exist in Section 3 with different spellings.
"""

import sys
import io
import re

# Ensure UTF-8 output on Windows
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')

def main(filepath):
    with open(filepath, 'r', encoding='utf-8') as f:
        lines = f.readlines()
    
    # Section 2: lines 996-2700 approx
    # Section 3: starts around 2703
    section2_lines = lines[996:2700]
    section3_lines = lines[2700:]
    
    # Find unlinked entries in Section 2
    unlinked = []
    for i, line in enumerate(section2_lines, start=996):
        if line.strip().startswith('||') and '[' not in line:
            # Extract name from first column
            match = re.search(r'\|\|\s*([A-Za-z0-9]+(?:[A-Z][a-z]+)*)\s*\|', line)
            if match:
                name = match.group(1).strip()
                # Skip headers
                if name not in ['Command', 'Function', 'Operation', 'Name', 'Code', 'Description', 'Instruction']:
                    unlinked.append((name, i))
    
    # Get unique names
    unique_names = {}
    for name, line_no in unlinked:
        if name not in unique_names:
            unique_names[name] = []
        unique_names[name].append(line_no)
    
    print(f"Found {len(unique_names)} unique unlinked entries in Section 2\n")
    
    # Check each unlinked name in Section 3
    section3_text = ''.join(section3_lines)
    
    found_with_typo = []
    not_found = []
    
    for name in sorted(unique_names.keys()):
        line_nums = unique_names[name]
        
        # Look for exact match in headings
        pattern1 = rf'^#\s+\d+B\s+{re.escape(name)}\s+'
        if re.search(pattern1, section3_text, re.M):
            # Found exact match but not linked - shouldn't happen
            continue
        
        # Look for case-insensitive or similar match
        pattern2 = rf'^#[^#]*\b{re.escape(name)}\b'
        match = re.search(pattern2, section3_text, re.M | re.I)
        if match:
            found_with_typo.append((name, match.group().strip(), line_nums))
        else:
            not_found.append((name, line_nums))
    
    if found_with_typo:
        print(f"✅ Found {len(found_with_typo)} entries in Section 3 (POSSIBLE TYPO/MISMATCH):\n")
        for name, heading, line_nums in found_with_typo:
            print(f"  • Table entry: '{name}' (lines: {', '.join(map(str, line_nums))})")
            print(f"    Section 3: {heading}")
            print()
    
    if not_found:
        print(f"❌ Not documented in Section 3 ({len(not_found)} entries):\n")
        for name, line_nums in not_found:
            print(f"  • {name} (lines: {', '.join(map(str, line_nums))})")

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: python find-typos-simple.py <file>")
        sys.exit(1)
    
    main(sys.argv[1])

