#!/usr/bin/env python3
"""
Clean and link section 2.1 - Monitor Calls in Numeric Order.
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

def build_call_map(lines):
    """Build a map of monitor call names to anchors from Section 3."""
    call_map = {}
    in_section3 = False
    pattern = re.compile(r'^# (.+)$')
    
    for i, line in enumerate(lines):
        if '## Page 72' in line or (i > 2700 and '# AccessRTCommon' in line):
            in_section3 = True
        
        if in_section3 and 'APPENDIX' in line:
            break
        
        if not in_section3:
            continue
        
        match = pattern.match(line.strip())
        if match:
            heading = match.group(1).strip()
            
            # Skip generic headers
            if any(heading.strip() == skip or heading.strip().endswith(skip) for skip in [
                'SINTRAN III Monitor Calls', 'PARAMETERS', 'PASCAL', 'COBOL', 
                'FORTRAN', 'PLANC', 'ASSEMBLY', 'MAC'
            ]) or heading.startswith('Page '):
                continue
            
            # Create anchor
            anchor = '#' + heading.lower().replace(' ', '-').replace('_', '-')
            anchor = re.sub(r'[^a-z0-9-#]', '', anchor)
            anchor = re.sub(r'-+', '-', anchor).rstrip('-')
            
            # Extract name parts
            parts = heading.split()
            if not parts:
                continue
            
            # Handle different formats
            if len(parts) == 1:
                call_map[parts[0].upper()] = anchor
            elif parts[0].endswith('B') or parts[0].isdigit():
                # Format: "112B AdjustClock CLADJ"
                if len(parts) >= 2:
                    call_map[parts[1].upper()] = anchor
                if len(parts) >= 3:
                    call_map[parts[2].upper()] = anchor
            else:
                # Format: "AccessRTCommon" or "AccessRTCommon RWRTC"
                call_map[parts[0].upper()] = anchor
                if len(parts) > 1 and parts[1].isupper():
                    call_map[parts[1].upper()] = anchor
    
    return call_map

def link_name(name, call_map):
    """Add link to a monitor call name if found."""
    name_clean = name.strip()
    name_upper = name_clean.upper()
    
    if name_upper in call_map:
        anchor = call_map[name_upper]
        return f'[{name_clean}]({anchor})'
    return name_clean

def fix_section_21(filepath):
    """Fix section 2.1."""
    print("Reading file...")
    lines = read_file(filepath)
    
    # Build call map from Section 3
    print("Building call map from Section 3...")
    call_map = build_call_map(lines)
    print(f"  Found {len(call_map)} monitor calls")
    
    # Find section 2.1
    start_idx = None
    end_idx = None
    
    for i, line in enumerate(lines):
        if '## 2.1 Monitor Calls in Numeric Order' in line:
            start_idx = i
        if start_idx and i > start_idx + 100 and '## 2.2 Alphabetic List' in line:
            end_idx = i
            break
    
    if not start_idx or not end_idx:
        print("Error: Could not find section 2.1")
        return
    
    print(f"Found section 2.1: lines {start_idx} to {end_idx}")
    
    # Extract all table rows, skipping page breaks
    intro_lines = []
    table_rows = []
    in_table = False
    
    for i in range(start_idx, end_idx):
        line = lines[i]
        
        # Skip page markers and headers
        if ('## Page' in line or 
            '# SINTRAN III Monitor Calls' in line or
            '## Overview' in line or
            line.strip() == '---'):
            continue
        
        # Check if it's a table line
        if line.strip().startswith('|'):
            in_table = True
            # Skip separator lines
            if all(c in '|-: \n' for c in line):
                continue
            table_rows.append(line)
        elif not in_table:
            intro_lines.append(line)
    
    print(f"  Extracted {len(table_rows)} table rows")
    
    # Convert dual-column format to single-column and link names
    # First collect left and right columns separately
    left_column = []
    right_column = []
    link_count = 0
    
    for row in table_rows:
        cells = [c.strip() for c in row.split('|')]
        if len(cells) < 7:
            continue
        
        # Skip header rows (contain "Call" or "Name" as column headers)
        if len(cells) > 1 and ('Call' in cells[1] or 'Code' in cells[1]):
            continue
        
        # Actual format: | 0B | ExitFromProgram | LEAVE  | 61B | MemoryAllocation | FIXCS |
        # cells would be: ['', '0B', 'ExitFromProgram', 'LEAVE', '61B', 'MemoryAllocation', 'FIXCS', '']
        
        # Extract first monitor call (columns 1-3) - goes to left column
        code1 = cells[1].strip() if len(cells) > 1 else ''
        name1 = cells[2].strip() if len(cells) > 2 else ''
        short1 = cells[3].strip() if len(cells) > 3 else ''
        
        # Skip if code doesn't look like a valid code (should end with B and not be a header)
        if code1 and code1.endswith('B') and name1 and name1 != 'Name':
            # Link the name
            if '[' not in name1:
                linked = link_name(name1, call_map)
                if '[' in linked:
                    name1 = linked
                    link_count += 1
            
            left_column.append((code1, name1, short1))
        
        # Extract second monitor call (columns 4-6) - goes to right column
        code2 = cells[4].strip() if len(cells) > 4 else ''
        name2 = cells[5].strip() if len(cells) > 5 else ''
        short2 = cells[6].strip() if len(cells) > 6 else ''
        
        # Skip if code doesn't look like a valid code
        if code2 and code2.endswith('B') and name2 and name2 != 'Name':
            # Link the name
            if '[' not in name2:
                linked = link_name(name2, call_map)
                if '[' in linked:
                    name2 = linked
                    link_count += 1
            
            right_column.append((code2, name2, short2))
    
    # Combine: all left column entries first, then all right column entries
    single_rows = left_column + right_column
    
    # Sort by octal code value
    def octal_sort_key(row):
        code = row[0]
        # Remove the 'B' and convert from octal to int for sorting
        if code.endswith('B'):
            try:
                return int(code[:-1], 8)  # Parse as octal
            except ValueError:
                return 0
        return 0
    
    single_rows.sort(key=octal_sort_key)
    
    print(f"  Converted to {len(single_rows)} single rows")
    print(f"  Added {link_count} links")
    
    # Rebuild section with single-column format
    new_section = intro_lines
    
    # Add header for single-column format
    new_section.append('| Code | Monitor Call Name | Short Name |\n')
    new_section.append('|------|-------------------|------------|\n')
    
    # Add all rows
    for code, name, short in single_rows:
        new_section.append(f'| {code} | {name} | {short} |\n')
    
    new_section.append('\n')
    
    # Rebuild file
    result = lines[:start_idx] + new_section + lines[end_idx:]
    
    print("Writing file...")
    write_file(filepath, result)
    print("✓ Section 2.1 cleaned and linked")

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: python fix-section-21.py <file>")
        sys.exit(1)
    
    fix_section_21(sys.argv[1])

