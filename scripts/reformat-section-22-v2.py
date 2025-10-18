#!/usr/bin/env python3
"""
Properly reformat Section 2.2 table - handles <br> tags correctly.
"""

import sys
import io
import re

# Ensure UTF-8 output on Windows
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')

def read_file(filepath):
    """Read the file from the original (not the modified WEB version)."""
    with open(filepath, 'r', encoding='utf-8') as f:
        return f.readlines()

def write_file(filepath, lines):
    """Write lines back to file."""
    with open(filepath, 'w', encoding='utf-8', newline='\n') as f:
        f.writelines(lines)

def parse_br_cell(cell):
    """Parse a cell that may contain <br> tags into a list of values."""
    if '<br>' in cell:
        parts = [p.strip() for p in cell.split('<br>')]
        return parts
    else:
        return [cell]

def expand_br_row(cells):
    """Expand a row with <br> tags into multiple rows."""
    # Parse each cell
    cell_parts = [parse_br_cell(c) for c in cells]
    
    # Find max number of parts
    max_parts = max(len(parts) for parts in cell_parts)
    
    # Build rows
    rows = []
    for i in range(max_parts):
        row = []
        for parts in cell_parts:
            if i < len(parts):
                row.append(parts[i])
            else:
                row.append('')  # Pad with empty if this column has fewer parts
        rows.append(row)
    
    return rows

def clean_section_22(orig_file, web_file):
    """Clean and reformat section 2.2 from the original file."""
    print("Reading original file...")
    orig_lines = read_file(orig_file)
    
    print("Reading WEB file...")
    web_lines = read_file(web_file)
    
    # Find section 2.2 boundaries in original
    start_idx = None
    end_idx = None
    
    for i, line in enumerate(orig_lines):
        if '## 2.2 Alphabetic List of Monitor Calls with Parameters' in line:
            start_idx = i
        if start_idx and i > start_idx + 10 and '### 2.3 Commonly-Used Monitor Calls' in line:
            end_idx = i
            break
    
    if not start_idx or not end_idx:
        print("Error: Could not find section 2.2 in original")
        return
    
    print(f"Found section 2.2 in original: lines {start_idx} to {end_idx}")
    
    # Find same in WEB file
    web_start = None
    web_end = None
    for i, line in enumerate(web_lines):
        if '## 2.2 Alphabetic List of Monitor Calls with Parameters' in line:
            web_start = i
        if web_start and i > web_start + 10 and '### 2.3 Commonly-Used Monitor Calls' in line:
            web_end = i
            break
    
    # Extract sections
    before = web_lines[:web_start]
    after = web_lines[web_end:]
    
    # Process original section 2.2
    cleaned = []
    intro_lines = []
    table_rows = []
    in_table = False
    
    for line in orig_lines[start_idx:end_idx]:
        # Skip page markers and separators
        if '## Page' in line or line.strip() == '---' or '# SINTRAN III Monitor Calls' in line or \
           '## Overview of the Monitor Calls' in line or '*Scanned by Jonny Oddene*' in line:
            continue
        
        # Table line
        if line.strip().startswith('|'):
            cells = [c.strip() for c in line.split('|')[1:-1]]
            if not cells or len(cells) < 6:
                continue
            
            # Skip duplicate headers
            if 'NAME, SHORT NAME' in cells[0] or 'TYPE' in line and 'I/O' in line and 'ERR' in line:
                if not in_table:
                    in_table = True
                continue
            
            # Skip separator lines
            if all(set(c.replace('-', '').replace(' ', '')) == set() for c in cells):
                continue
            
            # Expand rows with <br> tags
            if '<br>' in line:
                expanded = expand_br_row(cells)
                table_rows.extend(expanded)
            else:
                table_rows.append(cells)
        elif not in_table:
            intro_lines.append(line)
    
    # Calculate column widths
    col_widths = [0] * 6
    for row in table_rows:
        for i, cell in enumerate(row[:6]):
            col_widths[i] = max(col_widths[i], len(cell))
    
    # Set reasonable widths
    col_widths[0] = max(col_widths[0], 70)  # NAME column
    col_widths[1] = max(col_widths[1], 6)   # TYPE
    col_widths[2] = max(col_widths[2], 5)   # I/O
    col_widths[3] = max(col_widths[3], 5)   # ERR
    col_widths[4] = max(col_widths[4], 5)   # 100
    col_widths[5] = max(col_widths[5], 5)   # 500
    
    print(f"Column widths: {col_widths}")
    print(f"Table rows: {len(table_rows)}")
    
    # Build cleaned section
    cleaned = intro_lines
    
    # Add table header
    header = ['NAME, SHORT NAME, NUMBER, AND PARAMETERS', 'TYPE', 'I/O', 'ERR', '100', '500']
    cleaned.append('| ' + ' | '.join([h.ljust(w) for h, w in zip(header, col_widths)]) + ' |\n')
    cleaned.append('|' + '|'.join(['-' * (w + 2) for w in col_widths]) + '|\n')
    
    # Add all rows
    for row in table_rows:
        while len(row) < 6:
            row.append('')
        formatted = '| ' + ' | '.join([row[i].ljust(col_widths[i]) for i in range(6)]) + ' |\n'
        cleaned.append(formatted)
    
    # Rebuild file
    print("Writing reformatted file...")
    result = before + cleaned + after
    write_file(web_file, result)
    
    print(f"✓ Reformatted section 2.2")
    print(f"  Cleaned section: {len(cleaned)} lines")

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print("Usage: python reformat-section-22-v2.py <original-file> <web-file>")
        sys.exit(1)
    
    clean_section_22(sys.argv[1], sys.argv[2])

