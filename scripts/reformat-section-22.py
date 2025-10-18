#!/usr/bin/env python3
"""
Properly reformat Section 2.2 table with clean, aligned columns.
"""

import sys
import io
import re

# Ensure UTF-8 output on Windows
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')

def read_file(filepath):
    """Read the file and return lines."""
    with open(filepath, 'r', encoding='utf-8') as f:
        return f.readlines()

def write_file(filepath, lines):
    """Write lines back to file."""
    with open(filepath, 'w', encoding='utf-8', newline='\n') as f:
        f.writelines(lines)

def parse_table_row(line):
    """Parse a table row into cells."""
    if not line.strip().startswith('|'):
        return None
    cells = [c.strip() for c in line.split('|')[1:-1]]
    return cells if cells else None

def format_table_row(cells, widths):
    """Format cells into a properly aligned table row."""
    formatted = []
    for i, cell in enumerate(cells):
        if i < len(widths):
            formatted.append(cell.ljust(widths[i]))
        else:
            formatted.append(cell)
    return '| ' + ' | '.join(formatted) + ' |\n'

def format_separator(widths):
    """Create a separator row."""
    return '|' + '|'.join(['-' * (w + 2) for w in widths]) + '|\n'

def clean_section_22(filepath):
    """Clean and reformat section 2.2 table."""
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
        print("Error: Could not find section 2.2 boundaries")
        return
    
    print(f"Found section 2.2: lines {start_idx} to {end_idx}")
    
    # Extract sections
    before = lines[:start_idx]
    section = lines[start_idx:end_idx]
    after = lines[end_idx:]
    
    # Parse all table data
    cleaned = []
    table_rows = []
    in_table = False
    intro_lines = []
    
    for line in section:
        # Skip separators and page markers
        if line.strip() == '---' or '*Scanned by Jonny Oddene*' in line:
            continue
        
        # Check if it's a table line
        if line.strip().startswith('|'):
            in_table = True
            cells = parse_table_row(line)
            if cells and len(cells) >= 6:
                # Skip duplicate headers
                if 'NAME, SHORT NAME' in cells[0] or 'TYPE' in line:
                    if not table_rows:  # Only keep first header
                        continue
                    else:
                        continue
                # Skip separator lines
                if all(set(c.replace('-', '').replace(' ', '')) == set() for c in cells):
                    continue
                # Skip empty rows
                if all(c == '' or c == '●' for c in cells):
                    continue
                
                # Handle rows with <br> tags
                if '<br>' in line:
                    # Split on <br> and create multiple rows
                    for cell_idx, cell in enumerate(cells):
                        if '<br>' in cell:
                            parts = [p.strip() for p in cell.split('<br>')]
                            for j, part in enumerate(parts):
                                if j == 0:
                                    table_rows.append(cells[:cell_idx] + [part] + cells[cell_idx+1:])
                                else:
                                    # Parameter rows
                                    new_row = [''] * len(cells)
                                    new_row[0] = part
                                    # Try to distribute other data
                                    for k in range(1, len(cells)):
                                        if k < len(cells) and '<br>' in cells[k]:
                                            sub_parts = cells[k].split('<br>')
                                            if j < len(sub_parts):
                                                new_row[k] = sub_parts[j].strip()
                                    table_rows.append(new_row)
                            break
                else:
                    table_rows.append(cells)
        elif not in_table:
            intro_lines.append(line)
    
    # Calculate column widths
    col_widths = [0] * 6
    for row in table_rows:
        for i, cell in enumerate(row):
            if i < 6:
                col_widths[i] = max(col_widths[i], len(cell))
    
    # Set minimum widths for readability
    col_widths[0] = max(col_widths[0], 60)  # NAME column
    col_widths[1] = max(col_widths[1], 6)   # TYPE
    col_widths[2] = max(col_widths[2], 5)   # I/O
    col_widths[3] = max(col_widths[3], 5)   # ERR
    col_widths[4] = max(col_widths[4], 5)   # 100
    col_widths[5] = max(col_widths[5], 5)   # 500
    
    print(f"Column widths: {col_widths}")
    
    # Build cleaned section
    cleaned = intro_lines
    
    # Add table header
    header = ['NAME, SHORT NAME, NUMBER, AND PARAMETERS', 'TYPE', 'I/O', 'ERR', '100', '500']
    cleaned.append(format_table_row(header, col_widths))
    cleaned.append(format_separator(col_widths))
    
    # Add all rows
    for row in table_rows:
        while len(row) < 6:
            row.append('')
        cleaned.append(format_table_row(row[:6], col_widths))
    
    # Rebuild file
    print("Writing reformatted file...")
    result = before + cleaned + after
    write_file(filepath, result)
    
    print(f"✓ Reformatted section 2.2")
    print(f"  Original: {len(section)} lines")
    print(f"  Cleaned: {len(cleaned)} lines")
    print(f"  Table rows: {len(table_rows)}")

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: python reformat-section-22.py <file>")
        sys.exit(1)
    
    clean_section_22(sys.argv[1])

