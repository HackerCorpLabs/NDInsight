#!/usr/bin/env python3
"""
Apply manual OCR fixes to damaged headings.
"""

import sys
import io

# Ensure UTF-8 output on Windows
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')

def read_file(filepath):
    with open(filepath, 'r', encoding='utf-8') as f:
        return f.readlines()

def write_file(filepath, lines):
    with open(filepath, 'w', encoding='utf-8', newline='\n') as f:
        f.writelines(lines)

def load_fixes(fixes_file):
    """Load manual fixes from file."""
    fixes = {}
    with open(fixes_file, 'r', encoding='utf-8') as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('#'):
                continue
            parts = line.split('|')
            if len(parts) >= 4:
                damaged, code, correct, short = parts
                fixes[damaged] = (code, correct, short)
    return fixes

def apply_fixes(original_file, web_file, fixes_file):
    """Apply manual OCR fixes to both files."""
    print(f"Loading fixes from {fixes_file}...")
    fixes = load_fixes(fixes_file)
    print(f"  Loaded {len(fixes)} fixes")
    
    print(f"\nReading {original_file}...")
    orig_lines = read_file(original_file)
    
    print(f"Reading {web_file}...")
    web_lines = read_file(web_file)
    
    print("\nApplying fixes...")
    orig_count = 0
    web_count = 0
    
    # Fix original file
    for i, line in enumerate(orig_lines):
        stripped = line.strip()
        if stripped.startswith('# ') and stripped[2:] in fixes:
            damaged = stripped[2:]
            code, correct, short = fixes[damaged]
            new_heading = f"# {code} {correct} {short}\n"
            orig_lines[i] = new_heading
            orig_count += 1
            print(f"  {original_file}:{i+1}: {stripped} -> {new_heading.strip()}")
    
    # Fix web file
    for i, line in enumerate(web_lines):
        stripped = line.strip()
        if stripped.startswith('# ') and stripped[2:] in fixes:
            damaged = stripped[2:]
            code, correct, short = fixes[damaged]
            new_heading = f"# {code} {correct} {short}\n"
            web_lines[i] = new_heading
            web_count += 1
    
    print(f"\nWriting {original_file}...")
    write_file(original_file, orig_lines)
    
    print(f"Writing {web_file}...")
    write_file(web_file, web_lines)
    
    print(f"\n✓ Fixed {orig_count} headings in original file")
    print(f"✓ Fixed {web_count} headings in web file")

if __name__ == '__main__':
    if len(sys.argv) != 4:
        print("Usage: python apply-ocr-fixes.py <original-file> <web-file> <fixes-file>")
        sys.exit(1)
    
    apply_fixes(sys.argv[1], sys.argv[2], sys.argv[3])

