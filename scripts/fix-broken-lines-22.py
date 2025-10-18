#!/usr/bin/env python3
"""
Fix broken parameter lines in Section 2.2 where text was incorrectly split.
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
    """Check if a line is a continuation (starts with lowercase, no parameter number)."""
    if not line.strip().startswith('|'):
        return False
    cells = [c.strip() for c in line.split('|')[1:-1]]
    if len(cells) < 1:
        return False
    first_cell = cells[0]
    # Continuation lines: lowercase start, no digits, empty TYPE and I/O columns
    return (first_cell and 
            not first_cell[0].isupper() and 
            not first_cell[0].isdigit() and
            not first_cell.startswith('(optional)') and
            len(cells) >= 3 and
            cells[1] == '' and cells[2] == '')

def merge_broken_lines(filepath):
    """Merge lines that were incorrectly split."""
    print("Reading file...")
    lines = read_file(filepath)
    
    # Find section 2.2 boundaries
    start_idx = None
    end_idx = None
    
    for i, line in enumerate(lines):
        if '## 2.2 Alphabetic List of Monitor Calls with Parameters' in line:
            start_idx = i
        if start_idx and i > start_idx + 10 and '### 2.3 Commonly-Used Monitor Calls' in line:
            end_idx = i
            break
    
    if not start_idx or not end_idx:
        print("Error: Could not find section 2.2")
        return
    
    print(f"Found section 2.2: lines {start_idx} to {end_idx}")
    
    # Process section
    before = lines[:start_idx]
    after = lines[end_idx:]
    section = lines[start_idx:end_idx]
    
    merged = []
    i = 0
    merge_count = 0
    
    while i < len(section):
        line = section[i]
        
        # Check if next line is a continuation
        if line.strip().startswith('|') and i + 1 < len(section):
            next_line = section[i + 1]
            
            if is_continuation_line(next_line):
                # Merge next line into current
                cells = [c.strip() for c in line.split('|')[1:-1]]
                next_cells = [c.strip() for c in next_line.split('|')[1:-1]]
                
                # Append continuation text to first cell
                if cells and next_cells:
                    cells[0] = cells[0] + ' ' + next_cells[0]
                    
                    # Rebuild line with proper formatting
                    col_widths = [162, 23, 11, 5, 5, 5]
                    formatted = '| ' + ' | '.join([cells[j].ljust(col_widths[j]) if j < len(cells) else ''.ljust(col_widths[j]) for j in range(6)]) + ' |\n'
                    merged.append(formatted)
                    merge_count += 1
                    i += 2  # Skip both lines
                    continue
        
        merged.append(line)
        i += 1
    
    # Rebuild file
    print("Writing file...")
    result = before + merged + after
    write_file(filepath, result)
    
    print(f"✓ Merged {merge_count} broken lines")

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: python fix-broken-lines-22.py <file>")
        sys.exit(1)
    
    merge_broken_lines(sys.argv[1])

