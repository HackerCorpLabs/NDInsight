#!/usr/bin/env python3
"""
Fix OCR-damaged headings in Section 3 by matching with table entries.
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

def build_reference_map(lines):
    """Build a map of monitor calls from all section 2 tables."""
    ref_map = {}
    
    # Parse all tables in section 2
    in_section_2 = False
    for i, line in enumerate(lines):
        if '# 2 Overview of the Monitor Calls' in line or '## 2.' in line:
            in_section_2 = True
        if in_section_2 and '# 3 MONITOR CALL REFERENCE' in line:
            break
        
        if in_section_2 and line.strip().startswith('|'):
            parts = [p.strip() for p in line.split('|')]
            
            # Try different table formats
            for part in parts:
                if not part or part.startswith('-'):
                    continue
                
                # Look for patterns like "Name (SHORT 123B)" or "Name | SHORT | 123B"
                # Pattern 1: **Name (SHORT 123B)**
                match1 = re.search(r'\*\*([A-Za-z0-9]+)\*\*\s*\(([A-Z0-9]+)\s+(\d+B)\)', part)
                if match1:
                    name, short, code = match1.groups()
                    key = name.replace(' ', '').upper()
                    if key not in ref_map:
                        ref_map[key] = (code, name, short)
                    continue
                
                # Pattern 2: Name|CODE|123B format (section 2.1)
                if part.endswith('B') and len(part) <= 5:
                    # This is a code, look at adjacent parts
                    idx = parts.index(part)
                    if idx > 1 and idx < len(parts) - 1:
                        name_part = parts[idx - 1] if idx > 1 else parts[idx + 1]
                        short_part = parts[idx + 1] if idx < len(parts) - 1 else ''
                        
                        # Extract name from link if present
                        link_match = re.search(r'\[([^\]]+)\]', name_part)
                        if link_match:
                            name = link_match.group(1)
                            key = name.replace(' ', '').upper()
                            if key and key not in ref_map:
                                ref_map[key] = (part, name, short_part)
    
    return ref_map

def fix_ocr_headings(original_file, web_file):
    """Fix OCR-damaged headings in both files."""
    print("Reading files...")
    orig_lines = read_file(original_file)
    web_lines = read_file(web_file)
    
    print("Building reference map from section 2.1...")
    ref_map = build_reference_map(web_lines)
    print(f"  Found {len(ref_map)} monitor calls in reference")
    
    # Find damaged headings in Section 3
    print("\nSearching for OCR-damaged headings...")
    fixes = []
    
    for i, line in enumerate(orig_lines):
        # Look for all-caps headings with no spaces (OCR errors)
        match = re.match(r'^# ([A-Z]{8,})$', line.strip())
        if match and i > 2700:  # Section 3 starts around line 2700
            damaged_name = match.group(1)
            
            # Try to find in reference map
            if damaged_name in ref_map:
                code, correct_name, short = ref_map[damaged_name]
                correct_heading = f"# {code} {correct_name}"
                if short:
                    correct_heading += f" {short}"
                correct_heading += "\n"
                
                fixes.append({
                    'line': i,
                    'old': line,
                    'new': correct_heading,
                    'name': correct_name
                })
                
                print(f"  Line {i}: {damaged_name} -> {code} {correct_name} {short}")
    
    print(f"\nFound {len(fixes)} headings to fix")
    
    if not fixes:
        print("No fixes needed!")
        return
    
    # Apply fixes to both files
    print("\nApplying fixes...")
    
    # Fix original file
    for fix in fixes:
        orig_lines[fix['line']] = fix['new']
    
    # Fix web file  
    for fix in fixes:
        # Find the same heading in web file
        for i, line in enumerate(web_lines):
            if i > 2600 and fix['old'].strip() == line.strip():
                web_lines[i] = fix['new']
                break
    
    # Write files
    print(f"Writing {original_file}...")
    write_file(original_file, orig_lines)
    
    print(f"Writing {web_file}...")
    write_file(web_file, web_lines)
    
    print(f"\n✓ Fixed {len(fixes)} OCR-damaged headings")
    
    # Show sample fixes
    print("\nSample fixes:")
    for fix in fixes[:10]:
        print(f"  {fix['old'].strip()} -> {fix['new'].strip()}")

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print("Usage: python fix-ocr-headings.py <original-file> <web-file>")
        sys.exit(1)
    
    fix_ocr_headings(sys.argv[1], sys.argv[2])

