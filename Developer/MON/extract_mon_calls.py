#!/usr/bin/env python3
"""
SINTRAN III Monitor Calls YAML Extractor

Extracts monitor calls from Monitor Calls.md to individual YAML files.
Uses ASSEMBLY-500 sections as ground truth for octal numbers and names.

Author: Claude
Date: 2025-01-06
"""

import re
import yaml
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass, field, asdict
import sys


@dataclass
class Parameter:
    """Represents a monitor call parameter"""
    name: str
    type: str
    io: str  # I, O, or IO
    description: str
    size: Optional[int] = None
    implicit: bool = False


@dataclass
class LanguageExample:
    """Represents a code example in a specific language"""
    available: bool
    code: str
    notes: str = ""


@dataclass
class MonitorCall:
    """Complete monitor call data structure"""
    octal: str
    name: str
    short_names: List[str] = field(default_factory=list)
    description: str = ""
    notes: List[str] = field(default_factory=list)
    see_also: List[str] = field(default_factory=list)
    references: List[str] = field(default_factory=list)
    parameters: List[Dict] = field(default_factory=list)
    examples: Dict = field(default_factory=dict)
    compatibility: Dict = field(default_factory=dict)
    errors: Dict = field(default_factory=dict)
    source: Dict = field(default_factory=dict)
    extraction: Dict = field(default_factory=dict)


class MonCallExtractor:
    """Main extractor class for monitor calls"""

    def __init__(self, source_file: str, output_dir: str):
        self.source_file = Path(source_file)
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(exist_ok=True)

        # Load document
        with open(self.source_file, 'r', encoding='utf-8') as f:
            self.lines = f.readlines()

        # Load ground truth from ASSEMBLY sections
        self.ground_truth = self.load_ground_truth()
        print(f"✓ Loaded {len(self.ground_truth)} verified monitor calls from ASSEMBLY sections")

        # Statistics
        self.stats = {
            'total': 0,
            'extracted': 0,
            'failed': 0,
            'ocr_corrected': 0
        }

    def load_ground_truth(self) -> Dict[str, Dict]:
        """Load octal->name mappings from ASSEMBLY-500 sections"""
        ground_truth = {}

        for i, line in enumerate(self.lines):
            # Pattern: MonCallName : EQU 37B9 + OctalB
            match = re.search(r'(\w+)\s*:\s*(?:W\s+)?EQU\s+(?:37B9|3789)\s*\+\s*(\d+B)', line)
            if match:
                name = match.group(1)
                octal = match.group(2)

                # Skip variable names that aren't mon calls
                if name in ['ErrCode', 'Error', 'W1', 'P1', 'TimeUnits', 'UnitType',
                           'Func', 'DeviceNo', 'NoOfPages', 'FileName', 'StartAddress']:
                    continue

                if octal not in ground_truth:
                    ground_truth[octal] = {
                        'name': name,
                        'line': i + 1
                    }

        return ground_truth

    def find_mon_call_boundaries(self) -> List[Dict]:
        """
        Find start and end line for each monitor call section.
        Returns list of {octal, name, start_line, end_line}
        """
        boundaries = []
        current_section = None

        for i, line in enumerate(self.lines):
            if not line.startswith('# '):
                continue

            header = line[2:].strip()

            # Skip non-mon-call headers
            skip_patterns = [
                'SINTRAN', 'Information', 'PREFACE', 'CONTENTS', 'Copyright',
                'Contact', 'THE ', 'RELATED', 'EXAMPLES', 'Overview',
                'PARAMETERS', 'PASCAL', 'COBOL', 'FORTRAN', 'PLANC',
                'ASSEMBLY', 'MAC', 'Page ', 'Monitor Calls', 'ND Terminal',
                'FUNCTION CODE', 'LOG IN', 'Logical Device', 'Error Returns',
                'APPENDIX', 'SEND US', 'NOTE!', 'Components', 'Transducers',
                'CALL FORMATS', 'Function Codes', 'Call formats', 'Rules',
                'Subfunction', 'Output Parameters'
            ]

            if any(pattern in header for pattern in skip_patterns) or header.endswith(':'):
                continue

            # Check if this is a monitor call header
            match_octal = re.match(r'^(\d+B)\s+(.+)$', header)
            match_name_only = re.match(r'^([A-Z][A-Za-z0-9\.]+)$', header)

            if match_octal or match_name_only:
                # Close previous section if exists
                if current_section:
                    current_section['end_line'] = i
                    boundaries.append(current_section)

                # Start new section
                if match_octal:
                    octal = match_octal.group(1)
                    name = match_octal.group(2).split()[0]  # First word
                else:
                    name = match_name_only.group(1)
                    # Find octal from ground truth
                    octal = None
                    for gt_octal, gt_data in self.ground_truth.items():
                        if gt_data['name'] == name or name in gt_data['name']:
                            octal = gt_octal
                            break

                if octal:  # Only track if we have octal number
                    current_section = {
                        'octal': octal,
                        'name': name,
                        'start_line': i + 1,
                        'end_line': None,
                        'header_had_octal': bool(match_octal)
                    }

        # Close last section
        if current_section:
            current_section['end_line'] = len(self.lines)
            boundaries.append(current_section)

        return boundaries

    def extract_section_text(self, start_line: int, end_line: int) -> List[str]:
        """Extract lines for a section, 1-indexed"""
        return self.lines[start_line-1:end_line-1]

    def extract_description(self, section_lines: List[str]) -> Tuple[str, List[str], List[str], List[str]]:
        """
        Extract description, notes, see_also, and references.
        Returns (description, notes, see_also, references)
        """
        description_lines = []
        notes = []
        see_also = []
        references = []

        in_description = True

        for line in section_lines:
            stripped = line.strip()

            # Stop at first ## section
            if line.startswith('## '):
                break

            # Skip header line
            if line.startswith('# '):
                continue

            # Skip page markers
            if stripped.startswith('## Page') or stripped == '---':
                continue

            # Empty line
            if not stripped:
                if description_lines:
                    description_lines.append('')
                continue

            # See also
            if stripped.startswith('See also'):
                # Extract references after "See also"
                refs_text = stripped.replace('See also', '').strip()
                # Split by commas and "and"
                refs = re.split(r',\s*|\s+and\s+', refs_text)
                see_also.extend([r.strip('.') for r in refs if r.strip()])
                in_description = False
                continue

            # Bullet point (note)
            if stripped.startswith('- ') or stripped.startswith('* '):
                notes.append(stripped[2:])
                in_description = False
                continue

            # Regular description text
            if in_description:
                description_lines.append(stripped)

        description = '\n'.join(description_lines).strip()
        return description, notes, see_also, references

    def extract_parameters(self, section_lines: List[str]) -> List[Dict]:
        """Extract parameters from ## PARAMETERS section"""
        parameters = []
        in_params = False

        for line in section_lines:
            stripped = line.strip()

            if line.startswith('## PARAMETERS') or line.startswith('## Parameters'):
                in_params = True
                continue

            if in_params:
                # Stop at next ## section
                if line.startswith('##') and 'PARAMETERS' not in line and 'Parameters' not in line:
                    break

                # Skip table headers and dividers
                if '---' in stripped or stripped.startswith('|') and ('Description' in stripped or 'TYPE' in stripped):
                    continue

                # Bullet list parameter
                if stripped.startswith('- ') or stripped.startswith('➙'):
                    param_text = stripped.lstrip('- ➙').strip()
                    # Simple extraction - could be enhanced
                    parameters.append({
                        'name': f'Param{len(parameters)+1}',
                        'type': 'UNKNOWN',
                        'io': 'I',
                        'description': param_text
                    })

                # Table format parameter
                elif stripped.startswith('|') and stripped.count('|') >= 2:
                    # Parse table row
                    parts = [p.strip() for p in stripped.split('|')[1:-1]]
                    if len(parts) >= 1 and parts[0]:
                        parameters.append({
                            'name': parts[0] if len(parts) > 0 else 'Unknown',
                            'type': parts[1] if len(parts) > 1 else 'UNKNOWN',
                            'io': parts[2] if len(parts) > 2 else 'I',
                            'description': parts[3] if len(parts) > 3 else ''
                        })

        return parameters

    def extract_language_example(self, section_lines: List[str], language: str,
                                 header_patterns: List[str]) -> Optional[LanguageExample]:
        """Extract code example for a specific language"""
        in_section = False
        code_lines = []

        for line in section_lines:
            # Check if entering this language's section
            if line.startswith('##') and any(p in line for p in header_patterns):
                in_section = True
                continue

            # Stop at next ## section
            if in_section and line.startswith('##'):
                break

            if in_section:
                # Skip page markers
                if line.strip().startswith('## Page') or line.strip() == '---':
                    continue
                if '# SINTRAN III Monitor Calls' in line:
                    continue

                # Check for "Not available"
                if 'Not available' in line:
                    return LanguageExample(available=False, code="", notes="Not available")

                code_lines.append(line.rstrip())

        if code_lines:
            # Remove leading/trailing empty lines
            while code_lines and not code_lines[0].strip():
                code_lines.pop(0)
            while code_lines and not code_lines[-1].strip():
                code_lines.pop()

            return LanguageExample(
                available=True,
                code='\n'.join(code_lines),
                notes=""
            )

        return None

    def extract_compatibility(self, section_lines: List[str]) -> Dict:
        """Extract compatibility table (platforms, users, programs)"""
        compatibility = {
            'platforms': {'nd100': False, 'nd500': False},
            'users': [],
            'programs': [],
            'notes': ''
        }

        # Look for compatibility table near end
        for line in reversed(section_lines[-50:]):
            stripped = line.strip()

            # Pattern: | ND-100 and ND-500 | All users | All programs |
            if stripped.startswith('|') and 'ND-' in stripped:
                parts = [p.strip() for p in stripped.split('|')[1:-1]]

                if len(parts) >= 1:
                    platform_text = parts[0]
                    if 'ND-100' in platform_text or 'ND 100' in platform_text:
                        compatibility['platforms']['nd100'] = True
                    if 'ND-500' in platform_text or 'ND 500' in platform_text:
                        compatibility['platforms']['nd500'] = True

                if len(parts) >= 2:
                    compatibility['users'] = [parts[1]]

                if len(parts) >= 3:
                    compatibility['programs'] = [parts[2]]

                break

        return compatibility

    def extract_mon_call(self, boundary: Dict) -> Optional[MonitorCall]:
        """Extract complete monitor call from document section"""
        try:
            section_lines = self.extract_section_text(boundary['start_line'], boundary['end_line'])

            # Extract all components
            description, notes, see_also, references = self.extract_description(section_lines)
            parameters = self.extract_parameters(section_lines)
            compatibility = self.extract_compatibility(section_lines)

            # Extract language examples
            examples = {}

            pascal_ex = self.extract_language_example(section_lines, 'pascal', ['PASCAL', 'Pascal'])
            if pascal_ex:
                examples['pascal'] = {
                    'available': pascal_ex.available,
                    'code': pascal_ex.code,
                    'notes': pascal_ex.notes
                }

            cobol_ex = self.extract_language_example(section_lines, 'cobol', ['COBOL', 'Cobol'])
            if cobol_ex:
                examples['cobol'] = {
                    'available': cobol_ex.available,
                    'code': cobol_ex.code,
                    'notes': cobol_ex.notes
                }

            fortran_ex = self.extract_language_example(section_lines, 'fortran', ['FORTRAN', 'Fortran'])
            if fortran_ex:
                examples['fortran'] = {
                    'available': fortran_ex.available,
                    'code': fortran_ex.code,
                    'notes': fortran_ex.notes
                }

            planc_ex = self.extract_language_example(section_lines, 'planc', ['PLANC', 'Planc'])
            if planc_ex:
                examples['planc'] = {
                    'available': planc_ex.available,
                    'code': planc_ex.code,
                    'notes': planc_ex.notes
                }

            asm_ex = self.extract_language_example(section_lines, 'assembly', ['ASSEMBLY-500', 'ASSEMBLY'])
            if asm_ex:
                # Extract octal definition
                octal_def_match = re.search(rf'EQU\s+(?:37B9|3789)\s*\+\s*({boundary["octal"]})', asm_ex.code)
                examples['assembly_500'] = {
                    'available': asm_ex.available,
                    'code': asm_ex.code,
                    'octal_definition': f'37B9 + {boundary["octal"]}' if octal_def_match else '',
                    'notes': asm_ex.notes
                }

            mac_ex = self.extract_language_example(section_lines, 'mac', ['MAC', 'Mac'])
            if mac_ex:
                # Extract MON number
                mon_match = re.search(r'MON\s+(\d+)', mac_ex.code)
                mon_num = int(mon_match.group(1)) if mon_match else None
                examples['mac'] = {
                    'available': mac_ex.available,
                    'code': mac_ex.code,
                    'mon_number': mon_num,
                    'notes': mac_ex.notes
                }

            # Find page number
            page_num = None
            for line in section_lines:
                match = re.search(r'## Page (\d+)', line)
                if match:
                    page_num = int(match.group(1))
                    break

            # Create MonitorCall object
            mon_call = MonitorCall(
                octal=boundary['octal'],
                name=boundary['name'],
                short_names=[],
                description=description,
                notes=notes,
                see_also=see_also,
                references=references,
                parameters=parameters,
                examples=examples,
                compatibility=compatibility,
                errors={'returns_error': False, 'error_codes': [], 'error_reference': ''},
                source={
                    'document': 'SINTRAN III Monitor Calls (ND-860228.2 EN)',
                    'page': page_num or 0,
                    'line': boundary['start_line'],
                    'scan_date': '2020',
                    'scanned_by': 'Jonny Oddene for Sintran Data'
                },
                extraction={
                    'date': '2025-01-06',
                    'version': '1.0',
                    'validated': False,
                    'ocr_corrected': not boundary['header_had_octal'],
                    'ocr_notes': 'Header missing octal - reconstructed' if not boundary['header_had_octal'] else ''
                }
            )

            return mon_call

        except Exception as e:
            print(f"  ✗ Error extracting {boundary['octal']} {boundary['name']}: {e}")
            return None

    def generate_filename(self, mon_call: MonitorCall) -> str:
        """Generate filename for YAML file"""
        # Clean name
        clean_name = re.sub(r'[^A-Za-z0-9]', '', mon_call.name)
        return f"{mon_call.octal}_{clean_name}.yaml"

    def write_yaml(self, mon_call: MonitorCall) -> Path:
        """Write monitor call to YAML file"""
        filename = self.generate_filename(mon_call)
        filepath = self.output_dir / filename

        # Convert to dict and write
        data = asdict(mon_call)

        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(f"# SINTRAN III Monitor Call: {mon_call.name}\n")
            f.write(f"# Octal: {mon_call.octal}\n")
            f.write(f"# Extracted from: Monitor Calls.md (ND-860228.2 EN)\n\n")
            yaml.dump(data, f, default_flow_style=False, allow_unicode=True, sort_keys=False)

        return filepath

    def extract_all(self) -> Dict:
        """Main extraction loop"""
        print("\n🔍 Finding monitor call boundaries...")
        boundaries = self.find_mon_call_boundaries()
        print(f"✓ Found {len(boundaries)} monitor call sections\n")

        print("📝 Extracting monitor calls to YAML...\n")

        extracted_files = []
        failed = []

        for boundary in boundaries:
            print(f"  Processing {boundary['octal']:6s} {boundary['name']:30s} ... ", end='', flush=True)

            mon_call = self.extract_mon_call(boundary)
            if mon_call:
                filepath = self.write_yaml(mon_call)
                extracted_files.append(filepath)

                if mon_call.extraction['ocr_corrected']:
                    self.stats['ocr_corrected'] += 1
                    print("✓ (OCR corrected)")
                else:
                    print("✓")

                self.stats['extracted'] += 1
            else:
                failed.append(boundary)
                self.stats['failed'] += 1
                print("✗ FAILED")

        self.stats['total'] = len(boundaries)

        return {
            'extracted_files': extracted_files,
            'failed': failed,
            'stats': self.stats
        }

    def generate_report(self, results: Dict) -> str:
        """Generate extraction report"""
        report_lines = [
            "# Monitor Calls Extraction Report",
            f"**Date:** {self.stats.get('date', '2025-01-06')}",
            "",
            "## Summary",
            f"- **Total sections found:** {self.stats['total']}",
            f"- **Successfully extracted:** {self.stats['extracted']}",
            f"- **Failed:** {self.stats['failed']}",
            f"- **OCR corrections applied:** {self.stats['ocr_corrected']}",
            "",
            f"**Success rate:** {self.stats['extracted']/self.stats['total']*100:.1f}%",
            ""
        ]

        if results['failed']:
            report_lines.extend([
                "## Failed Extractions",
                ""
            ])
            for fail in results['failed']:
                report_lines.append(f"- {fail['octal']} {fail['name']} (line {fail['start_line']})")
            report_lines.append("")

        report_lines.extend([
            "## Output",
            f"- **Output directory:** `{self.output_dir}`",
            f"- **Files created:** {len(results['extracted_files'])}",
            ""
        ])

        report = '\n'.join(report_lines)

        # Write report
        report_path = self.output_dir.parent / 'extraction-report.md'
        with open(report_path, 'w') as f:
            f.write(report)

        return report


def main():
    """Main entry point"""
    print("="*80)
    print("SINTRAN III Monitor Calls YAML Extractor")
    print("="*80)
    print()

    source_file = "Developer/MON/Monitor Calls - CORRECTED.md"
    output_dir = "Developer/MON/calls"

    extractor = MonCallExtractor(source_file, output_dir)
    results = extractor.extract_all()

    print("\n" + "="*80)
    print("📊 Extraction Complete!")
    print("="*80)
    print(f"✓ Extracted: {results['stats']['extracted']}/{results['stats']['total']}")
    print(f"✓ OCR corrected: {results['stats']['ocr_corrected']}")
    print(f"✗ Failed: {results['stats']['failed']}")
    print()

    report = extractor.generate_report(results)
    print(f"📝 Report saved to: {extractor.output_dir.parent}/extraction-report.md")
    print()


if __name__ == "__main__":
    main()
