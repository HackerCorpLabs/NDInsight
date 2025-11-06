# SINTRAN III Monitor Calls Documentation

This directory contains YAML files for all 230 SINTRAN III Monitor Calls and tools to generate documentation.

## Directory Structure

```
Developer/MON/
├── calls/                           # 230 YAML files (one per monitor call)
├── mon-call.schema.json             # JSON Schema for YAML validation
├── mon-call-schema.yaml             # YAML Schema for reference
├── Monitor Calls.md                 # Original OCR source material (931K)
├── ND MON Calls.md                  # Generated comprehensive documentation
└── generate_mon_calls_markdown.py   # Script to regenerate markdown docs
```

## Regenerating Documentation

If you update any YAML files in `calls/`, regenerate the markdown documentation by running:

```bash
cd Developer/MON
python3 generate_mon_calls_markdown.py
```

This will regenerate `ND MON Calls.md` with:
- Three indexes (by octal number, call name, and short name)
- Full documentation for all 230 monitor calls
- Cross-referenced links throughout
- Code examples in 6 programming languages
- Compatibility badges

**Note:** Run this script from the repository root or the `Developer/MON` directory.

## YAML File Format

Each monitor call is documented in a YAML file with this structure:

```yaml
octal: 131B
name: DataTransfer
short_names:
  - ABSTR
description: |
  Full description with bullet points...
see_also:
  - TransferData
  - ReadFromFile
parameters:
  - name: Param1
    type: UNKNOWN
    io: I
    description: Parameter description
examples:
  pascal:
    available: true
    code: |
      DataTransfer(DeviceNo, Func, ...);
  cobol:
    available: true
    code: |
      MONITOR-CALL "DataTransfer" USING ...
  # ... other languages
compatibility:
  nd100: true
  nd500: true
  user_programs: true
  rt_programs: true
  system_programs: false
source:
  document: SINTRAN III Monitor Calls (ND-860228.2 EN)
  page: 127
extraction:
  date: '2025-01-06'
  ocr_corrected: false
```

## Validation

Validate YAML files against the schema:

```bash
# Install validation tool if needed
pip install jsonschema pyyaml

# Validate a single file
python3 -c "
import yaml, jsonschema
schema = yaml.safe_load(open('mon-call.schema.json'))
data = yaml.safe_load(open('calls/131B_DataTransfer.yaml'))
jsonschema.validate(data, schema)
print('✓ Valid')
"
```

## Statistics

- **Total Monitor Calls:** 230
- **Monitor Call Range:** 0B - 514B (octal)
- **Calls with Short Names:** 230 (100%)
- **Programming Languages:** 6 (Pascal, COBOL, Fortran, PLANC, ASSEMBLY-500, MAC)
- **Extraction Date:** 2025-01-06
- **Source Document:** SINTRAN III Monitor Calls (ND-860228.2 EN)

## History

1. **Initial Extraction** - 195/230 calls extracted with OCR reconstruction
2. **Description Enhancement** - Full descriptions extracted from source sections
3. **Schema Cleanup** - Removed unused fields, simplified structure
4. **Short Names Population** - Added assembly mnemonics to all 230 calls
5. **Quote Fixing** - Fixed apostrophe escaping in code examples
6. **Markdown Generation** - Created comprehensive documentation with cross-references
7. **Directory Cleanup (Nov 2025)** - Removed backup files, historical extraction scripts, and redundant documentation
