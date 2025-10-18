#!/usr/bin/env python3
"""
Process SINTRAN III Monitor Calls manual to add hyperlinks and merge multi-page tables.
"""

import re
import sys
import io
from pathlib import Path
from typing import Dict, List, Tuple, Optional

# Fix Unicode output on Windows
if sys.platform == 'win32':
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')

class MonitorCallsProcessor:
    def __init__(self, input_file: str, output_file: str):
        self.input_file = Path(input_file)
        self.output_file = Path(output_file)
        self.lines: List[str] = []
        self.call_map: Dict[str, Tuple[str, str]] = {}  # short_name -> (long_name, anchor)
        self.long_name_to_anchor: Dict[str, str] = {}  # long_name -> anchor
        
    def slugify(self, text: str) -> str:
        """Convert heading text to markdown anchor."""
        # Convert to lowercase, replace spaces/special chars with hyphens
        slug = text.lower()
        slug = re.sub(r'[^\w\s-]', '', slug)
        slug = re.sub(r'[-\s]+', '-', slug)
        slug = slug.strip('-')
        return f"#{slug}"
    
    def read_file(self):
        """Read the input file."""
        with open(self.input_file, 'r', encoding='utf-8') as f:
            self.lines = f.readlines()
        print(f"✓ Read {len(self.lines)} lines from {self.input_file}")
    
    def parse_section3(self):
        """Parse section 3 to extract all monitor call definitions."""
        print("\n📊 Parsing Section 3 for monitor calls...")
        
        # Pattern to match monitor call headings in section 3
        # Examples: "# OB EXITFROMPROGRAM LEAVE", "# 112B AdjustClock", "# AccessRTCommon"
        heading_pattern = re.compile(r'^# (.+)$')
        
        in_section3 = False
        count = 0
        
        for i, line in enumerate(self.lines):
            # Detect start of section 3
            if '## Page 72' in line or (i > 2800 and '# AccessRTCommon' in line):
                in_section3 = True
            
            # Stop at appendices
            if '## Page' in line and 'APPENDIX' in self.lines[i+2] if i+2 < len(self.lines) else False:
                break
            
            if not in_section3:
                continue
            
            match = heading_pattern.match(line.strip())
            if match:
                heading_text = match.group(1).strip()
                
                # Skip generic headers (be very specific to avoid skipping real monitor calls)
                if any(heading_text.strip() == skip or heading_text.strip().endswith(skip) for skip in [
                    'SINTRAN III Monitor Calls',
                    'Monitor Call Reference',
                    'PARAMETERS',
                    'PASCAL',
                    'COBOL', 
                    'FORTRAN',
                    'PLANC',
                    'ASSEMBLY',
                    'MAC'
                ]):
                    continue
                if heading_text.startswith('Page '):
                    continue
                
                # Parse the heading
                parts = heading_text.split()
                if not parts:
                    continue
                
                # Create anchor from full heading
                anchor = self.slugify(heading_text)
                
                # Try to extract components
                long_name = None
                short_name = None
                
                # Format: "OB EXITFROMPROGRAM LEAVE" or "112B AdjustClock CLADJ" or just "AccessRTCommon"
                if len(parts) >= 2:
                    # First part might be octal number
                    if parts[0].endswith('B') or parts[0].isdigit():
                        if len(parts) >= 3:
                            long_name = parts[1]
                            short_name = parts[2] if len(parts) > 2 else None
                        else:
                            long_name = parts[1]
                    else:
                        # Just name, possibly with short name
                        long_name = parts[0]
                        short_name = parts[1] if len(parts) > 1 and parts[1].isupper() else None
                elif len(parts) == 1:
                    # Single name (e.g., "AccessRTCommon", "ASSIGNCAMACLAM")
                    long_name = parts[0]
                    short_name = None
                
                if long_name:
                    self.long_name_to_anchor[long_name.upper()] = anchor
                    self.long_name_to_anchor[long_name] = anchor
                    
                    if short_name:
                        self.call_map[short_name.upper()] = (long_name, anchor)
                        self.call_map[short_name] = (long_name, anchor)
                    
                    count += 1
                else:
                    # Debug: show headings without long_name
                    if count < 5:  # Only show first few
                        print(f"  DEBUG: Skipped '{heading_text}' - no long_name extracted")
        
        print(f"✓ Found {count} monitor calls in section 3")
        print(f"✓ Built {len(self.call_map)} short name mappings")
        print(f"✓ Built {len(self.long_name_to_anchor)} long name mappings")
        
        # Debug: print first few mappings
        print("\n📝 Sample mappings:")
        for i, (key, value) in enumerate(list(self.call_map.items())[:5]):
            print(f"  {key} → {value}")
    
    def link_call_name(self, name: str, preserve_formatting: bool = False) -> str:
        """Convert a monitor call name to a markdown link if found."""
        name_clean = name.strip()
        
        # Check short name map first
        if name_clean.upper() in self.call_map:
            long_name, anchor = self.call_map[name_clean.upper()]
            if preserve_formatting:
                return f"[{name_clean}]({anchor})"
            return f"[{name_clean}]({anchor})"
        
        # Check long name map
        if name_clean.upper() in self.long_name_to_anchor:
            anchor = self.long_name_to_anchor[name_clean.upper()]
            return f"[{name_clean}]({anchor})"
        
        # Not found, return as-is
        return name_clean
    
    def process_section_21(self):
        """Process section 2.1 - Monitor Calls in Numeric Order (dual-column table)."""
        print("\n🔗 Processing Section 2.1 (Numeric Order)...")
        
        # Find the table (lines 1004-1046)
        start_line = None
        end_line = None
        
        for i, line in enumerate(self.lines):
            if '2.1 Monitor Calls in Numeric Order' in line:
                start_line = i
            if start_line and i > start_line and '---' in line and i < start_line + 100:
                end_line = i
                break
        
        if not start_line:
            print("  ⚠ Section 2.1 not found")
            return
        
        print(f"  Section 2.1 range: lines {start_line} to {end_line}")
        
        count = 0
        for i in range(start_line, end_line if end_line else start_line + 100):
            line = self.lines[i]
            
            # Match table rows with two monitor call entries
            # Format: | 0B | ExitFromProgram | LEAVE  | 61B | MemoryAllocation | FIXCS |
            if line.strip().startswith('|') and '|' in line[1:]:
                parts = [p.strip() for p in line.split('|')]
                
                # Skip header and separator rows
                if len(parts) < 7 or all('-' in p for p in parts[1:4] if p):
                    continue
                
                # Process both name columns (index 3 and 6)
                if len(parts) > 3 and parts[3] and not parts[3].startswith('-'):
                    # Already a link? Skip it
                    if '[' not in parts[3]:
                        parts[3] = self.link_call_name(parts[3])
                        count += 1
                
                if len(parts) > 6 and parts[6] and not parts[6].startswith('-'):
                    if '[' not in parts[6]:
                        parts[6] = self.link_call_name(parts[6])
                        count += 1
                
                # Reconstruct the line
                self.lines[i] = '|' + '|'.join(parts[1:-1]) + '|\n'
        
        print(f"✓ Linked {count} monitor call names in section 2.1")
    
    def remove_page_breaks(self, start_line: int, end_line: int) -> int:
        """Remove page break markers within a table range."""
        # Page break pattern: ## Page N, followed by headers
        removed = 0
        i = start_line
        
        while i < end_line:
            line = self.lines[i]
            
            # Detect page break
            if line.strip().startswith('## Page '):
                # Look ahead for the typical header pattern
                look_ahead = min(6, end_line - i)
                is_page_break = False
                
                for j in range(1, look_ahead):
                    if i+j < len(self.lines):
                        next_line = self.lines[i+j].strip()
                        if 'SINTRAN III Monitor Calls' in next_line or 'Overview' in next_line:
                            is_page_break = True
                            break
                
                if is_page_break:
                    # Remove the page marker and following headers (up to 5 lines)
                    remove_count = 0
                    for j in range(look_ahead):
                        if i < len(self.lines):
                            # Stop if we hit a table row
                            if self.lines[i].strip().startswith('|'):
                                break
                            del self.lines[i]
                            remove_count += 1
                            removed += 1
                            end_line -= 1
                    i -= 1  # Adjust since we deleted lines
            
            i += 1
        
        return removed
    
    def process_section_22(self):
        """Process section 2.2 - Alphabetic List (multi-page, needs merging)."""
        print("\n🔗 Processing Section 2.2 (Alphabetic List with Parameters)...")
        
        # Find section 2.2 (starts around line 1177, ends around line 2086)
        start_line = None
        end_line = None
        
        for i, line in enumerate(self.lines):
            if '## 2.2 Alphabetic List of Monitor Calls with Parameters' in line:
                start_line = i
            if start_line and i > start_line + 100 and '### 2.3 Commonly-Used' in line:
                end_line = i
                break
        
        if not start_line:
            print("  ⚠ Section 2.2 not found")
            return
        
        print(f"  Section 2.2 range: lines {start_line} to {end_line}")
        
        # First, merge pages
        removed = self.remove_page_breaks(start_line, end_line)
        print(f"✓ Removed {removed} lines from page breaks")
        end_line -= removed  # Adjust end_line after deletions
        
        # Now link monitor call names
        # Format: | **AccessRTCommon** (RWRTC 406B) | | | | | |
        count = 0
        i = start_line
        
        while i < end_line:
            line = self.lines[i]
            
            # Match lines with monitor call names (bold, followed by parentheses)
            match = re.search(r'\*\*([A-Z][A-Za-z0-9]+)\*\*\s*\(([A-Z0-9]+)\s+(\d+B)\)', line)
            if match:
                call_name = match.group(1)
                short_name = match.group(2)
                
                # Check if already linked
                if f'[{call_name}]' not in line:
                    # Create link
                    linked_name = self.link_call_name(call_name)
                    if '[' in linked_name:  # Successfully linked
                        # Replace in line, preserving the bold markers
                        old_text = f'**{call_name}**'
                        new_text = f'**{linked_name}**'
                        self.lines[i] = line.replace(old_text, new_text)
                        count += 1
            
            i += 1
        
        print(f"✓ Linked {count} monitor call names in section 2.2")
    
    def process_simple_table(self, section_name: str, column_index: int, section_marker: str):
        """Process a simple single-page table section."""
        print(f"\n🔗 Processing {section_name}...")
        
        # Find the section
        start_line = None
        end_line = None
        
        for i, line in enumerate(self.lines):
            if section_marker in line:
                start_line = i
            if start_line and i > start_line + 5:
                # End when we hit next section or page marker
                if line.strip().startswith('##') and ('2.' in line or 'Page' in line or 'APPENDIX' in line):
                    end_line = i
                    break
        
        if not start_line:
            print(f"  ⚠ {section_name} not found")
            return
        
        if not end_line:
            end_line = start_line + 200  # Safety limit
        
        print(f"  Range: lines {start_line} to {end_line}")
        
        count = 0
        for i in range(start_line, min(end_line, len(self.lines))):
            line = self.lines[i]
            
            # Process table rows
            if line.strip().startswith('|') and '|' in line[1:]:
                parts = [p.strip() for p in line.split('|')]
                
                # Skip header and separator rows
                if len(parts) < 3 or all('-' in p or not p for p in parts[1:-1]):
                    continue
                
                # Link the specified column
                if len(parts) > column_index and parts[column_index]:
                    if '[' not in parts[column_index]:  # Not already linked
                        # Extract just the name (before any codes or descriptions)
                        name = parts[column_index].strip()
                        # Remove trailing codes like (RWRTC 406B)
                        name = re.sub(r'\s*\([A-Z0-9\s]+\)\s*$', '', name)
                        
                        linked = self.link_call_name(name)
                        if '[' in linked:  # Successfully linked
                            parts[column_index] = linked
                            self.lines[i] = '|' + '|'.join(parts[1:-1]) + '|\n'
                            count += 1
        
        print(f"✓ Linked {count} monitor call names in {section_name}")
    
    def process_all_single_page_tables(self):
        """Process all single-page table sections."""
        # Section 2.3 - Commonly-Used Monitor Calls (Name column is index 1)
        self.process_simple_table("Section 2.3 (Commonly-Used)", 1, "2.3 Commonly-Used Monitor Calls")
        
        # Section 2.4 - File Operations (Operation column is index 1)
        self.process_simple_table("Section 2.4 (File Operations)", 1, "2.4 File Operations")
        
        # Section 2.6 - Terminal Handling (Name column is index 1)
        self.process_simple_table("Section 2.6 (Terminal Handling)", 1, "2.6 Monitor Calls for Terminal Handling")
        
        # Section 2.7 - Printer Handling (Function column is index 1)
        self.process_simple_table("Section 2.7 (Printer Handling)", 1, "2.7 Monitor Calls for Printer Handling")
        
        # Section 2.8 - Error Handling (Command column is index 1)
        self.process_simple_table("Section 2.8 (Error Handling)", 1, "2.8 Monitor Calls for Error Handling")
        
        # Section 2.9 - File System Operations (Function column is index 1)
        self.process_simple_table("Section 2.9 (File System)", 1, "2.9 FILE SYSTEM OPERATIONS")
        
        # Section 2.10 - RT Program Execution (Command column is index 1)
        self.process_simple_table("Section 2.10 (RT Program Execution)", 1, "2.10 RT Program Execution")
        
        # Section 2.12 - Segment Administration (Function column is index 1)
        self.process_simple_table("Section 2.12 (Segment Admin)", 1, "2.12 Segment Administration")
        
        # Section 2.13 - Data Communication (Function column is index 1)
        self.process_simple_table("Section 2.13 (Data Comm)", 1, "2.13 Data Communication")
        
        # Section 2.14 - Internal Use (Call column is index 1)
        self.process_simple_table("Section 2.14 (Internal Use)", 1, "2.14 Monitor Calls for Internal Use")
    
    def process_multipage_simple_table(self, section_name: str, column_index: int, section_marker: str, end_marker: str):
        """Process a multi-page table that needs merging."""
        print(f"\n🔗 Processing {section_name} (multi-page)...")
        
        # Find the section
        start_line = None
        end_line = None
        
        for i, line in enumerate(self.lines):
            if section_marker in line:
                start_line = i
            if start_line and i > start_line + 10 and end_marker in line:
                end_line = i
                break
        
        if not start_line:
            print(f"  ⚠ {section_name} not found")
            return
        
        if not end_line:
            end_line = start_line + 300  # Safety limit
        
        print(f"  Range: lines {start_line} to {end_line}")
        
        # First, merge pages
        removed = self.remove_page_breaks(start_line, end_line)
        print(f"✓ Removed {removed} lines from page breaks")
        end_line -= removed  # Adjust after deletions
        
        # Now link
        count = 0
        for i in range(start_line, min(end_line, len(self.lines))):
            line = self.lines[i]
            
            if line.strip().startswith('|') and '|' in line[1:]:
                parts = [p.strip() for p in line.split('|')]
                
                if len(parts) < 3 or all('-' in p or not p for p in parts[1:-1]):
                    continue
                
                if len(parts) > column_index and parts[column_index]:
                    if '[' not in parts[column_index]:
                        name = parts[column_index].strip()
                        name = re.sub(r'\s*\([A-Z0-9\s]+\)\s*$', '', name)
                        
                        linked = self.link_call_name(name)
                        if '[' in linked:
                            parts[column_index] = linked
                            self.lines[i] = '|' + '|'.join(parts[1:-1]) + '|\n'
                            count += 1
        
        print(f"✓ Linked {count} monitor call names in {section_name}")
    
    def process_multipage_tables(self):
        """Process multi-page tables that need merging."""
        # Section 2.5 - I/O Monitor Calls (Command/Function column is index 1)
        self.process_multipage_simple_table(
            "Section 2.5 (I/O Calls)", 
            1, 
            "2.5 Input and Output Monitor Calls",
            "2.6 Monitor Calls for Terminal"
        )
        
        # Section 2.11 - Device Handling (Command/Function column is index 1)
        self.process_multipage_simple_table(
            "Section 2.11 (Device Handling)", 
            1, 
            "2.11 Device Handling",
            "2.12 Segment Administration"
        )
        
        # Section 2.15 - Short Names (Description column is index 3)
        print("\n🔗 Processing Section 2.15 (Short Names) (multi-page)...")
        
        start_line = None
        end_line = None
        
        for i, line in enumerate(self.lines):
            if "2.15 Monitor Calls Sorted on Short Names" in line:
                start_line = i
            if start_line and i > start_line + 10 and "2.16 Monitor Call Numbers" in line:
                end_line = i
                break
        
        if start_line:
            print(f"  Range: lines {start_line} to {end_line}")
            
            # Merge pages
            removed = self.remove_page_breaks(start_line, end_line)
            print(f"✓ Removed {removed} lines from page breaks")
            end_line -= removed
            
            # Link both description columns (index 3 and 6 for dual-column table)
            count = 0
            for i in range(start_line, min(end_line, len(self.lines))):
                line = self.lines[i]
                
                if line.strip().startswith('|') and '|' in line[1:]:
                    parts = [p.strip() for p in line.split('|')]
                    
                    if len(parts) < 3 or all('-' in p or not p for p in parts[1:4] if p):
                        continue
                    
                    # Link Description columns (index 3 and 6)
                    for col_idx in [3, 6]:
                        if len(parts) > col_idx and parts[col_idx]:
                            if '[' not in parts[col_idx]:
                                name = parts[col_idx].strip()
                                linked = self.link_call_name(name)
                                if '[' in linked:
                                    parts[col_idx] = linked
                                    count += 1
                    
                    self.lines[i] = '|' + '|'.join(parts[1:-1]) + '|\n'
            
            print(f"✓ Linked {count} monitor call names in Section 2.15")
        else:
            print("  ⚠ Section 2.15 not found")
    
    def process_toc(self):
        """Add links to the Table of Contents."""
        print("\n🔗 Processing Table of Contents...")
        
        # Find the TOC (around line 110-154)
        start_line = None
        end_line = None
        
        for i, line in enumerate(self.lines):
            if i > 100 and i < 200 and '# CONTENTS' in line:
                start_line = i
            if start_line and i > start_line and 'Index' in line:
                end_line = i + 5
                break
        
        if not start_line:
            print("  ⚠ TOC not found")
            return
        
        print(f"  Range: lines {start_line} to {end_line}")
        
        count = 0
        for i in range(start_line, min(end_line, len(self.lines))):
            line = self.lines[i]
            
            # Match TOC entries like: | 2.1 Monitor calls in numeric order | 19 |
            match = re.search(r'\|\s*(\d+\.?\d*\s+[A-Z][^|]+?)\s*\|', line)
            if match:
                section_text = match.group(1).strip()
                
                # Skip if already linked
                if '[' in line:
                    continue
                
                # Create anchor
                anchor = self.slugify(section_text)
                
                # Create link
                linked_text = f'[{section_text}]({anchor})'
                
                # Replace in line
                self.lines[i] = line.replace(section_text, linked_text)
                count += 1
        
        print(f"✓ Linked {count} TOC entries")
    
    def write_output(self):
        """Write the processed content to output file."""
        with open(self.output_file, 'w', encoding='utf-8') as f:
            f.writelines(self.lines)
        print(f"\n✅ Wrote {len(self.lines)} lines to {self.output_file}")
    
    def run(self):
        """Main processing pipeline."""
        print("="*60)
        print("SINTRAN III Monitor Calls - Web Version Processor")
        print("="*60)
        
        self.read_file()
        self.parse_section3()
        self.process_section_21()
        self.process_section_22()
        self.process_all_single_page_tables()
        # TODO: Add section 2.5, 2.11, 2.15 (multi-page tables)
        # TODO: Add TOC linking
        self.write_output()
        
        print("\n" + "="*60)
        print("✅ Processing complete!")
        print("="*60)


def main():
    if len(sys.argv) != 3:
        print("Usage: python process-monitor-calls.py <input_file> <output_file>")
        sys.exit(1)
    
    input_file = sys.argv[1]
    output_file = sys.argv[2]
    
    processor = MonitorCallsProcessor(input_file, output_file)
    processor.run()


if __name__ == "__main__":
    main()

