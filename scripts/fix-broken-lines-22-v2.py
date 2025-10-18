#!/usr/bin/env python3
"""
Fix broken parameter lines in Section 2.2 - improved version.
"""

import sys
import io
import re

# Ensure UTF-8 output on Windows
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')

def read_file(filepath):
    with open(filepath, 'r', encoding='utf-8') as f:
        return f.readlines()

def write_file(filepath, lines):
    with open(filepath, 'w', encoding='utf-8', newline='\n') as f:
        f.writelines(lines)

def is_continuation_line(line):
    """Check if a line is a true continuation (not a new entry)."""
    if not line.strip().startswith('|'):
        return False
    cells = [c.strip() for c in line.split('|')[1:-1]]
    if len(cells) < 3:
        return False
    first_cell = cells[0]
    
    # NOT continuation if: starts with **, starts with digit+period, starts with uppercase word, or (optional)
    if (first_cell.startswith('**') or 
        re.match(r'^\d+\.', first_cell) or
        first_cell.startswith('(optional)') or
        (first_cell and first_cell[0].isupper() and ' ' not in first_cell[:20])):
        return False
    
    # IS continuation if: lowercase start AND empty TYPE/I/O columns
    return (first_cell and 
            first_cell[0].islower() and
            cells[1] == '' and cells[2] == '')

def merge_broken_lines(filepath):
    """Re-process from the ORIGINAL file to avoid compounding errors."""
    print("Reading ORIGINAL file...")
    # Read from original, not the broken WEB file
    orig_lines = read_file("Reference-Manuals/ND-860228-2-EN SINTRAN III Monitor Calls.md")
    web_lines = read_file(filepath)
    
    # Find section boundaries in original
    orig_start = None
    orig_end = None
    for i, line in enumerate(orig_lines):
        if '## 2.2 Alphabetic List of Monitor Calls with Parameters' in line:
            orig_start = i
        if orig_start and i > orig_start + 10 and '### 2.3 Commonly-Used Monitor Calls' in line:
            orig_end = i
            break
    
    # Find boundaries in web file
    web_start = None
    web_end = None
    for i, line in enumerate(web_lines):
        if '## 2.2 Alphabetic List of Monitor Calls with Parameters' in line:
            web_start = i
        if web_start and i > web_start + 10 and '### 2.3 Commonly-Used Monitor Calls' in line:
            web_end = i
            break
    
    if not orig_start or not web_start:
        print("Error: Could not find section 2.2")
        return
    
    print("Re-running reformat from original...")
    
    # Just re-run the reformat script
    import subprocess
    result = subprocess.run([
        sys.executable, 
        "scripts/reformat-section-22-v2.py",
        "Reference-Manuals/ND-860228-2-EN SINTRAN III Monitor Calls.md",
        filepath
    ], capture_output=True, text=True)
    
    print(result.stdout)
    if result.returncode != 0:
        print("Error:", result.stderr)
        return
    
    print("✓ Reformatted from original - broken lines should be properly handled now")

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: python fix-broken-lines-22-v2.py <file>")
        sys.exit(1)
    
    merge_broken_lines(sys.argv[1])

