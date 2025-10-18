#!/usr/bin/env python3
"""
Find all unlinked entries in Section 2 tables and check if they exist in Section 3.
"""

import sys
import io
import re

# Ensure UTF-8 output on Windows
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')

def find_unlinked_entries(filepath):
    """Find all unlinked table entries in Section 2."""
    with open(filepath, 'r', encoding='utf-8') as f:
        lines = f.readlines()
    
    # Find Section 2 and Section 3
    section2_start = None
    section2_end = None
    section3_start = None
    
    for i, line in enumerate(lines):
        if '2 Overview of the Monitor Calls' in line:
            section2_start = i
        elif section2_start and 'MONITOR CALL REFERENCE' in line:
            section2_end = i
            section3_start = i
            break
    
    if not section2_start or not section2_end:
        print("Error: Could not find Section 2")
        return
    
    # Find all unlinked entries in Section 2
    unlinked = []
    for i in range(section2_start, section2_end):
        line = lines[i]
        # Look for table rows without links: || Something | CODE | ...
        if line.strip().startswith('||') and '[' not in line:
            # Extract the name from the first column
            match = re.search(r'\|\|\s*([A-Za-z0-9]+(?:[A-Z][a-z]+)*)\s*\|', line)
            if match:
                name = match.group(1).strip()
                # Skip header rows
                if name not in ['Command', 'Function', 'Operation', 'Name', 'Code', 'Description']:
                    unlinked.append(name)
    
    unique_unlinked = sorted(set(unlinked))
    print(f"\nFound {len(unique_unlinked)} unique unlinked entries in Section 2:\n")
    
    # Now check if these exist in Section 3
    section3_text = ''.join(lines[section3_start:])
    
    found_in_section3 = []
    not_found = []
    
    for name in unique_unlinked:
        # Search for this name in Section 3 headings
        # Try exact match first
        pattern = rf'^#\s+\d+B\s+{re.escape(name)}\s+'
        if re.search(pattern, section3_text, re.M | re.I):
            found_in_section3.append((name, 'exact'))
        else:
            # Try fuzzy match (case insensitive, might have slight differences)
            pattern = rf'^#.*{re.escape(name)}'
            match = re.search(pattern, section3_text, re.M | re.I)
            if match:
                found_in_section3.append((name, match.group().strip()))
            else:
                not_found.append(name)
    
    if found_in_section3:
        print(f"\n✅ Found {len(found_in_section3)} entries in Section 3 (possible typo/mismatch):")
        for name, match_type in found_in_section3:
            if match_type == 'exact':
                print(f"  • {name} (exact match)")
            else:
                print(f"  • {name} → {match_type}")
    
    if not_found:
        print(f"\n❌ Not found in Section 3 ({len(not_found)} entries, no documentation):")
        for name in not_found:
            print(f"  • {name}")

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: python find-unlinked-entries.py <file>")
        sys.exit(1)
    
    find_unlinked_entries(sys.argv[1])

