#!/usr/bin/env python3
"""
Clean and reformat Section 2.2 of the Monitor Calls manual.
Removes page breaks, duplicate headers, fixes formatting, and aligns columns.
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

def clean_section_22(filepath):
    """Clean section 2.2 table."""
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
    print(f"Processing {end_idx - start_idx} lines...")
    
    # Extract the section
    before = lines[:start_idx]
    section = lines[start_idx:end_idx]
    after = lines[end_idx:]
    
    # Clean the section
    cleaned = []
    header_found = False
    header_line = None
    separator_line = None
    
    for i, line in enumerate(section):
        # Skip --- separators
        if line.strip() == '---':
            continue
        
        # Handle table header
        if '| NAME, SHORT NAME, NUMBER, AND PARAMETERS' in line or '| | TYPE' in line:
            if not header_found:
                cleaned.append(line)
                header_found = True
                header_line = line
            continue
        
        # Handle separator line
        if line.strip().startswith('|') and set(line.strip().replace('|', '').replace('-', '').replace(' ', '')) == set():
            if not separator_line and header_found:
                cleaned.append(line)
                separator_line = line
            continue
        
        # Convert <br> format to proper rows
        if '<br>' in line and line.strip().startswith('|'):
            # Extract cell contents
            cells = [c.strip() for c in line.split('|')[1:-1]]
            if len(cells) >= 6:
                # Parse the complex cell with <br> tags
                for cell_idx, cell in enumerate(cells):
                    if '<br>' in cell:
                        parts = cell.split('<br>')
                        # Create a row for each main entry
                        for j, part in enumerate(parts):
                            if j == 0:
                                # First line is the monitor call name
                                new_cells = [''] * 6
                                new_cells[cell_idx] = part
                                cleaned.append('| ' + ' | '.join(new_cells) + ' |\n')
                            else:
                                # Subsequent lines are parameters
                                new_cells = [''] * 6
                                new_cells[cell_idx] = part
                                cleaned.append('| ' + ' | '.join(new_cells) + ' |\n')
                        break
                continue
        
        # Skip empty spacer rows
        if line.strip().startswith('|'):
            cells = [c.strip() for c in line.split('|')[1:-1]]
            if all(c == '' for c in cells):
                continue
        
        # Keep regular table rows and other lines
        cleaned.append(line)
    
    # Rebuild file
    print("Writing cleaned file...")
    result = before + cleaned + after
    write_file(filepath, result)
    
    print(f"✓ Cleaned section 2.2")
    print(f"  Original: {len(section)} lines")
    print(f"  Cleaned: {len(cleaned)} lines")
    print(f"  Removed: {len(section) - len(cleaned)} lines")

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: python clean-section-22.py <file>")
        sys.exit(1)
    
    clean_section_22(sys.argv[1])

