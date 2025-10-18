#!/usr/bin/env python3
"""
Fix bold formatting in Section 2.2 - make all monitor call names consistently bold.
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

def is_monitor_call_entry(cell):
    """Check if a cell is a monitor call name (not a parameter)."""
    cell = cell.strip()
    if not cell:
        return False
    
    # Not a parameter line (doesn't start with digit.)
    if re.match(r'^\d+\.', cell):
        return False
    
    # Not an (optional) parameter
    if cell.startswith('(optional)'):
        return False
    
    # Contains parentheses with the short name (like "LAMUfunction (MLAMU 315B)")
    if '(' in cell and ')' in cell:
        return True
    
    return False

def make_bold(cell):
    """Add bold formatting if not already present."""
    cell = cell.strip()
    if cell.startswith('**') and cell.endswith('**'):
        return cell  # Already bold
    if '**' in cell:
        return cell  # Partially bold, leave as is
    return f'**{cell}**'

def fix_bold_formatting(filepath):
    """Fix bold formatting in section 2.2."""
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
    
    # Process lines
    fixed_count = 0
    
    for i in range(start_idx, end_idx):
        line = lines[i]
        
        # Skip non-table lines
        if not line.strip().startswith('|'):
            continue
        
        cells = [c.strip() for c in line.split('|')[1:-1]]
        if len(cells) < 6:
            continue
        
        # Skip header and separator lines
        if 'NAME, SHORT NAME' in cells[0] or all(set(c.replace('-', '').replace(' ', '')) == set() for c in cells):
            continue
        
        first_cell = cells[0]
        
        # Check if this is a monitor call entry
        if is_monitor_call_entry(first_cell):
            # Make it bold if not already
            if not first_cell.startswith('**'):
                cells[0] = make_bold(first_cell)
                
                # Rebuild line with proper column widths
                col_widths = [162, 23, 11, 5, 5, 5]
                formatted = '| ' + ' | '.join([cells[j].ljust(col_widths[j]) if j < len(cells) else ''.ljust(col_widths[j]) for j in range(6)]) + ' |\n'
                lines[i] = formatted
                fixed_count += 1
    
    # Write file
    print("Writing file...")
    write_file(filepath, lines)
    
    print(f"✓ Fixed bold formatting for {fixed_count} monitor call names")

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: python fix-bold-section-22.py <file>")
        sys.exit(1)
    
    fix_bold_formatting(sys.argv[1])

