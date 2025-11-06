# SINTRAN III Monitor Calls - YAML Extraction Plan

**Date:** 2025-01-06
**Document:** Monitor Calls.md (ND-860228.2 EN)
**Target Format:** YAML files with JSON Schema validation

---

## 📊 ANALYSIS SUMMARY

### Data Sources Analyzed:

| Source | Count | Status | Reliability |
|--------|-------|--------|-------------|
| **Master Index** (Overview pages) | 200 | ⚠️ Partially corrupted | Medium |
| **ASSEMBLY-500 Sections** (source code) | 179 | ✅ Verified | **HIGH** |
| **Document Headers** (`#` level 1) | 214 | ⚠️ Some missing/corrupted | Medium |

### Key Findings:

1. **54 Monitor Calls** have missing or corrupted headers in the document
2. **26 Orphaned PARAMETERS sections** without proper headers
3. **179 Verified Monitor Calls** extracted from ASSEMBLY-500 sections (most reliable)
4. **Master Index corruption:** Some entries have octal numbers as names (e.g., `47B | 201B`)

### OCR Corruption Examples:

- `# DATA TRANSFER` → **Should be:** `# 131B DataTransfer`
- `# FORCE TRAP` → **Should be:** `# 435B ForceTrap`
- Missing headers before PARAMETERS sections

---

## 🎯 EXTRACTION STRATEGY

### Phase 1: Document Analysis & Header Reconstruction

**Objective:** Fix source document by adding missing/corrupted headers

#### Step 1.1: Build Ground Truth Table
✅ **COMPLETED** - Extracted 179 verified mon calls from ASSEMBLY-500 sections

**Source File:** `/tmp/assembly_truth.txt`

**Method:**
- Parse all ASSEMBLY-500 sections
- Extract patterns: `<MonCallName> : EQU 37B9 + <OctalB>`
- Extract MAC patterns: `MON <decimal>`
- Cross-reference with nearby headers

#### Step 1.2: Identify Missing Headers

For each of the 179 mon calls:
1. Check if proper `# <OctalB> <Name>` header exists before its content
2. Identify content start (PARAMETERS, PASCAL, COBOL sections)
3. Mark header as:
   - ✅ **PRESENT** - Header exists with correct octal
   - ⚠️ **PARTIAL** - Header exists but missing octal number
   - ❌ **MISSING** - No header, content orphaned

#### Step 1.3: Generate Corrected Document

Create: `Monitor Calls - CORRECTED.md`

**Corrections to apply:**
- Insert missing headers: `# <OctalB> <MonCallName>`
- Add octal numbers to partial headers
- Preserve all existing content
- Mark corrections with comments: `<!-- OCR CORRECTED: Added header -->`

**Example corrections:**
```markdown
<!-- OCR CORRECTED: Header was "# DATA TRANSFER", corrected to: -->
# 131B DataTransfer

<!-- OCR CORRECTED: Added missing header -->
# 201B HDLCFunction
```

#### Step 1.4: Validation

- ✅ All 179 ASSEMBLY-verified mon calls have proper headers
- ✅ No duplicate octal numbers (document known edge cases)
- ✅ Spot-check 10 random corrections manually

---

### Phase 2: YAML Extraction

**Objective:** Extract each monitor call to individual YAML file

#### Step 2.1: Parse Structure

For each monitor call, extract:

1. **Metadata:**
   - Octal number
   - Primary name
   - Short names (from header)

2. **Description:**
   - All paragraphs before first `##` section
   - Bullet points (`-` or `*` lists)
   - "See also" references

3. **Parameters:**
   - Extract from `## PARAMETERS` section
   - Handle both bullet lists and table formats
   - Parse parameter name, type, I/O direction, description

4. **Language Examples:**
   - `## PASCAL` section → `examples.pascal.code`
   - `## COBOL` section → `examples.cobol.code`
   - `## FORTRAN` section → `examples.fortran.code`
   - `## PLANC` section → `examples.planc.code`
   - `## ASSEMBLY-500` section → `examples.assembly_500.code`
     - Extract octal definition: `EQU 37B9 + <OctalB>`
   - `## MAC` section → `examples.mac.code`
     - Extract mon number: `MON <decimal>`
     - Mark as unavailable if "Not available"

5. **Compatibility:**
   - Parse table: `| ND-100 and ND-500 | All users | All programs |`
   - Extract platform, user, and program compatibility

6. **Source Metadata:**
   - Document name
   - Page number (from `## Page XX` markers)
   - Line number in source file

#### Step 2.2: Content Boundaries

**Start:** Line with `# <OctalB> <Name>` header
**End:** Next `# <title>` that is NOT a subsection

**Subsections to include (## level 2):**
- `## PARAMETERS`
- `## PASCAL`
- `## COBOL`
- `## FORTRAN`
- `## PLANC`
- `## ASSEMBLY-500`
- `## MAC`

**Markers to strip:**
- `## Page XX`
- `# SINTRAN III Monitor Calls`
- `---` dividers between subsections

#### Step 2.3: Filename Format

**Pattern:** `<OCTAL>_<CleanName>.yaml`

**Octal Formatting:**
- Zero-pad to 3 digits: `000B`, `112B`, `231B`, `514B`
- Example: `131B` → `131B_DataTransfer.yaml`

**Name Cleaning:**
- Use PascalCase
- Remove spaces
- Remove special characters
- Example: `DATA TRANSFER` → `DataTransfer`

**Examples:**
```
000B_ExitFromProgram.yaml
001B_InByte.yaml
002B_OutByte.yaml
112B_AdjustClock.yaml
131B_DataTransfer.yaml
231B_ExpandFile.yaml
406B_AccessRTCommon.yaml
514B_ND500TimeOut.yaml
```

#### Step 2.4: YAML Generation

Use schema defined in: `mon-call-schema.yaml`

**Key points:**
- Preserve code formatting (use `|` literal block scalar)
- Strip leading/trailing whitespace from descriptions
- Parse bullet points into arrays
- Extract "See also" references
- Mark `ocr_corrected: true` if header was reconstructed

---

### Phase 3: Validation & Quality Assurance

#### Step 3.1: Automated Validation

**For each YAML file:**

1. **Schema Validation:**
   ```bash
   yamllint <file>.yaml
   check-jsonschema --schemafile mon-call.schema.json <file>.yaml
   ```

2. **Content Checks:**
   - ✅ Filename octal matches `octal` field in YAML
   - ✅ File size > 500 bytes (completeness)
   - ✅ Contains `description` field (not empty)
   - ✅ Has at least one language example with `available: true`
   - ✅ No `## Page` markers in code examples
   - ✅ Octal number format: `^\d+B$`

3. **Cross-Reference:**
   - Compare extracted octal with ASSEMBLY truth table
   - Verify mon call name matches

#### Step 3.2: Completeness Check

```python
# Expected results:
✓ Total files: 179 (matching ASSEMBLY truth table)
✓ All octals from 0B to 514B accounted for
✓ No missing files from ground truth
✓ All files pass schema validation
```

#### Step 3.3: Validation Report

Generate: `extraction-validation-report.md`

**Contents:**
```markdown
# Extraction Validation Report

## Summary
- Total files created: 179
- Schema validation: 179/179 passed (100%)
- Missing headers corrected: 54
- Files with OCR corrections: 54

## Statistics
- Average file size: 2.5 KB
- Total lines extracted: ~45,000
- Extraction time: <5 minutes

## Issues Found
- [ ] None (or list any problems)

## Spot Check Results
Manually verified 10 random files:
- ✅ 001B_InByte.yaml
- ✅ 112B_AdjustClock.yaml
- ✅ 131B_DataTransfer.yaml
- ... (7 more)

## Validation Matrix
| Octal | Name | Filename | Header Corrected | Schema Valid |
|-------|------|----------|------------------|--------------|
| 000B | ExitFromProgram | 000B_ExitFromProgram.yaml | ✅ | ✅ |
| 001B | InByte | 001B_InByte.yaml | - | ✅ |
| ... | ... | ... | ... | ... |
```

#### Step 3.4: Manual Spot Check

**Sample 10 random files:**
1. Open YAML file
2. Check structure matches schema
3. Verify code examples are properly formatted
4. Compare against original document section
5. Confirm no data loss

---

## 📁 OUTPUT STRUCTURE

```
Developer/MON/
├── Monitor Calls.md                       # Original document (preserved)
│
├── mon-call-schema.yaml                   # ✅ CREATED - Schema documentation
├── mon-call.schema.json                   # ✅ CREATED - JSON Schema validator
├── EXTRACTION_PLAN.md                     # ✅ THIS FILE
│
├── Monitor Calls - CORRECTED.md           # 🔄 Phase 1 output
├── extraction-validation-report.md        # 🔄 Phase 3 output
├── mon-calls-inventory.md                 # 🔄 Master index of all calls
│
└── calls/
    ├── README.md                          # Index of all YAML files
    ├── 000B_ExitFromProgram.yaml
    ├── 001B_InByte.yaml
    ├── 002B_OutByte.yaml
    ├── 003B_SetEcho.yaml
    ├── 004B_SetBreak.yaml
    ├── ...
    ├── 112B_AdjustClock.yaml
    ├── 131B_DataTransfer.yaml
    ├── 231B_ExpandFile.yaml
    ├── ...
    ├── 406B_AccessRTCommon.yaml
    ├── 514B_ND500TimeOut.yaml
    └── VALIDATION_MATRIX.csv              # Octal -> Filename mapping
```

---

## 🔧 IMPLEMENTATION

### Script Architecture

**Main Script:** `extract_mon_calls.py`

```python
#!/usr/bin/env python3
"""
SINTRAN III Monitor Calls YAML Extractor

Extracts monitor calls from Monitor Calls.md to individual YAML files.
"""

import re
import yaml
from typing import Dict, List, Optional
from dataclasses import dataclass

@dataclass
class MonitorCall:
    octal: str
    name: str
    short_names: List[str]
    description: str
    notes: List[str]
    see_also: List[str]
    references: List[str]
    parameters: List[Dict]
    examples: Dict
    compatibility: Dict
    errors: Optional[Dict]
    source: Dict
    extraction: Dict

class MonCallExtractor:
    def __init__(self, source_file: str, output_dir: str):
        self.source_file = source_file
        self.output_dir = output_dir
        self.ground_truth = self.load_ground_truth()

    def load_ground_truth(self) -> Dict[str, str]:
        """Load ASSEMBLY-500 verified octal->name mappings"""
        pass

    def identify_boundaries(self) -> List[Dict]:
        """Find start/end of each monitor call section"""
        pass

    def extract_metadata(self, section: List[str]) -> Dict:
        """Extract octal, name, short_names from header"""
        pass

    def extract_description(self, section: List[str]) -> str:
        """Extract description paragraphs"""
        pass

    def extract_parameters(self, section: List[str]) -> List[Dict]:
        """Parse PARAMETERS section"""
        pass

    def extract_examples(self, section: List[str]) -> Dict:
        """Extract all language examples"""
        pass

    def extract_compatibility(self, section: List[str]) -> Dict:
        """Parse compatibility table"""
        pass

    def generate_yaml(self, mon_call: MonitorCall) -> str:
        """Convert MonitorCall to YAML string"""
        pass

    def validate_yaml(self, yaml_file: str) -> bool:
        """Validate against JSON schema"""
        pass

    def extract_all(self) -> None:
        """Main extraction loop"""
        boundaries = self.identify_boundaries()
        for boundary in boundaries:
            mon_call = self.extract_monitor_call(boundary)
            yaml_content = self.generate_yaml(mon_call)
            filename = f"{mon_call.octal}_{mon_call.name}.yaml"
            self.write_file(filename, yaml_content)
            self.validate_yaml(filename)

def main():
    extractor = MonCallExtractor(
        source_file="Monitor Calls.md",
        output_dir="calls/"
    )
    extractor.extract_all()
    extractor.generate_report()

if __name__ == "__main__":
    main()
```

### Validation Script

**Script:** `validate_mon_calls.py`

```python
#!/usr/bin/env python3
"""
Validate extracted YAML files against schema and ground truth.
"""

import yaml
import jsonschema
from pathlib import Path

def validate_all():
    schema = load_schema("mon-call.schema.json")
    ground_truth = load_ground_truth()

    results = []
    for yaml_file in Path("calls/").glob("*.yaml"):
        result = validate_file(yaml_file, schema, ground_truth)
        results.append(result)

    generate_report(results)

def validate_file(yaml_file, schema, ground_truth):
    # Load YAML
    # Validate against JSON schema
    # Check filename matches octal
    # Cross-reference with ground truth
    # Return validation result
    pass
```

---

## ⏱️ ESTIMATED EFFORT

| Phase | Task | Estimated Time |
|-------|------|----------------|
| **Phase 1** | Build ground truth | ✅ Complete |
| | Identify missing headers | 30 min |
| | Generate corrected document | 15 min |
| | Validation | 15 min |
| **Phase 2** | Develop extraction script | 2-3 hours |
| | Extract all 179 mon calls | 5 minutes (automated) |
| **Phase 3** | Automated validation | 5 minutes |
| | Manual spot-check | 30 min |
| | Generate reports | 10 min |
| **TOTAL** | **~4 hours** |

---

## ✅ SUCCESS CRITERIA

1. ✅ All 179 ASSEMBLY-verified mon calls extracted to YAML files
2. ✅ Each file validates against JSON Schema
3. ✅ Filenames follow format: `<OCTAL>_<Name>.yaml`
4. ✅ No content loss from original document
5. ✅ Code examples properly formatted (preserves indentation)
6. ✅ All missing headers documented and corrected
7. ✅ Validation report shows 100% success rate
8. ✅ Manual spot-check confirms quality
9. ✅ YAML files are human-readable and machine-parseable
10. ✅ Schema allows for easy querying and processing

---

## 🚀 NEXT STEPS

### Ready to execute:

**Option A: Fully Automated**
```bash
./extract_mon_calls.py --source "Monitor Calls.md" --output "calls/" --validate
```

**Option B: Step-by-Step**
```bash
# Phase 1: Correct document
./correct_headers.py --input "Monitor Calls.md" --output "Monitor Calls - CORRECTED.md"

# Phase 2: Extract to YAML
./extract_mon_calls.py --source "Monitor Calls - CORRECTED.md" --output "calls/"

# Phase 3: Validate
./validate_mon_calls.py --input "calls/" --schema "mon-call.schema.json"
```

---

## 📝 NOTES

- **Backup:** Original `Monitor Calls.md` will NOT be modified
- **Safety:** Dry-run mode available: `--dry-run` flag
- **Logging:** Detailed logs written to `extraction.log`
- **Rollback:** Keep `Monitor Calls - CORRECTED.md` for reference
- **Extensibility:** Schema can be extended for future metadata

---

**AWAITING APPROVAL TO PROCEED** 🎯
