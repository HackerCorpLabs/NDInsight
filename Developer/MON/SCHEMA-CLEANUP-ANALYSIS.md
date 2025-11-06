# YAML Schema Cleanup Analysis
**Date:** 2025-11-06
**Files Analyzed:** 230 monitor call YAML files

## Executive Summary

Analysis reveals significant opportunities for schema simplification:
- **2 fields can be REMOVED** (0% usage)
- **3 fields can be made OPTIONAL** (low usage)
- **1 major data quality issue** identified (parameter types)
- **Compatibility structure** can be simplified

## Detailed Field Usage Analysis

### ✅ Well-Used Fields (Keep As-Is)

| Field | Usage | Notes |
|-------|-------|-------|
| `octal` | 100% | ✓ Required, always populated |
| `name` | 100% | ✓ Required, always populated |
| `description` | 100% | ✓ Required, now fully populated with rich descriptions |
| `see_also` | 70.0% | ✓ Good usage, cross-references to related calls |
| `parameters` | 65.2% | ✓ Most calls have parameters documented |
| `examples` | 76.1% | ✓ Excellent code example coverage |
| `extraction` | 100% | ✓ Metadata about extraction process |

### ❌ Fields to REMOVE

#### 1. `references` - 0% usage
**Current:** Array of external documentation references
**Reality:** Not a single file has this populated (0/230)
**Recommendation:** **DELETE from schema**
**Reason:** The `source` field already captures document references. External references weren't in the source manual.

#### 2. `errors` - 0% usage
**Current:** Complex object with `returns_error`, `error_codes`, `error_reference`
**Reality:** No files have error information (0/230)
**Recommendation:** **DELETE from schema**
**Reason:**
- Error handling isn't consistently documented in source manual
- Most calls return errors in a standard parameter (last param)
- This structured error data doesn't exist in our source
- Can be added later if we find a different source document

### ⚠️  Fields to Make OPTIONAL

#### 3. `short_names` - 15.2% usage
**Current:** Required in original design
**Reality:** Only 35/230 files have short names
**Recommendation:** **Make OPTIONAL** in schema
**Reason:** Many monitor calls don't have documented short names

#### 4. `notes` - 15.7% usage
**Current:** Array for additional notes
**Reality:** Only 36/230 files have notes; most info is in description
**Recommendation:** **Make OPTIONAL** and consider merging into description
**Reason:** Descriptions now contain full details including bullet points

### 🔧 Fields to SIMPLIFY

#### 5. `compatibility` - Overly Complex Structure

**Current Structure:**
```yaml
compatibility:
  platforms:
    nd100: false
    nd500: false
  users: []           # 83/230 files have empty arrays
  programs: []        # 85/230 files have empty arrays
  notes: ''
```

**Usage:**
- ND-100 support: 120/230 (52%)
- ND-500 support: 130/230 (57%)
- Users field: Empty in 83/230 (36% empty)
- Programs field: Empty in 85/230 (37% empty)

**Recommendation:** **SIMPLIFY** to:
```yaml
compatibility:
  nd100: true
  nd500: true
  user_programs: true
  rt_programs: true
  system_programs: true
```

**Reason:**
- The current arrays aren't being used effectively
- A simple boolean structure is clearer
- Matches actual usage patterns better

## 🚨 Data Quality Issues

### Issue 1: Parameter Type 'UNKNOWN'

**Problem:** 420/529 parameters (79%) have type `UNKNOWN`

**Root Cause:** Parameters were extracted but types weren't parsed from the source document format.

**Examples of garbage in type field:**
- "See appendix A."
- "Yes" / "No"
- "Standard Error Code. See appendix A."
- "User defined."
- Full descriptions instead of types

**Recommendation:**
1. **Accept current state** - Type extraction is complex, would require re-parsing all parameter sections
2. **OR** Add a cleanup phase to:
   - Map common patterns to proper types (INT, INT2, STRING, LONGINT, etc.)
   - Default unknown types to "ANY" or "UNKNOWN"
   - Note: This is a significant effort

## Proposed New Schema

### Simplified Fields

```yaml
# REMOVED FIELDS:
# - references (0% usage)
# - errors (0% usage)

# CHANGED TO OPTIONAL:
# - short_names (15% usage)
# - notes (16% usage)

# SIMPLIFIED:
compatibility:
  nd100: boolean
  nd500: boolean
  user_programs: boolean
  rt_programs: boolean
  system_programs: boolean
```

### Full Proposed Schema

```yaml
octal: "131B"                    # Required
name: "DataTransfer"             # Required
short_names: ["DXABS"]          # Optional (was required)
description: |                   # Required
  Full description with paragraphs and bullet points...
notes: []                        # Optional (was required, rarely used)
see_also: []                     # Optional (well-used: 70%)
parameters:                      # Optional (well-used: 65%)
  - name: "DeviceNo"
    type: "INT"                  # Note: 79% are UNKNOWN
    io: "I"
    description: "..."
examples:                        # Required (well-used: 76%)
  pascal:
    available: true
    code: |
      ...
compatibility:                   # Required (simplified structure)
  nd100: true
  nd500: true
  user_programs: true
  rt_programs: true
  system_programs: true
source:                          # Required
  document: "ND-860228.2 EN"
  page_references: []
extraction:                      # Required
  ocr_corrected: true
  ocr_notes: "..."
```

## Impact Analysis

### Files Requiring Updates: 230/230

**Changes needed:**
1. Remove `references` field → All 230 files
2. Remove `errors` field → All 230 files
3. Simplify `compatibility` structure → All 230 files
4. Schema: Make `short_names` and `notes` optional

### Benefits

✅ **Simpler schema** - Remove 2 unused top-level fields
✅ **Smaller files** - ~20-30 lines removed per file
✅ **Clearer structure** - Compatibility is now boolean flags
✅ **Honest schema** - Reflects actual data, not aspirational structure
✅ **Easier validation** - Fewer required fields that are empty

### Risks

⚠️  **Breaking change** - Existing code reading these files needs updates
⚠️  **Future-proofing** - If we find error documentation later, need to re-add

## Recommendations

### Option A: Full Cleanup (Recommended)

**Do:**
1. ✅ Remove `references` field (0% usage)
2. ✅ Remove `errors` field (0% usage)
3. ✅ Simplify `compatibility` structure
4. ✅ Make `short_names` and `notes` optional in schema
5. ✅ Update all 230 YAML files
6. ✅ Update schema JSON

**Time:** ~30 minutes for script + validation

### Option B: Conservative Cleanup

**Do:**
1. ✅ Make `short_names`, `notes`, `references`, `errors` optional (don't remove)
2. ✅ Simplify `compatibility` structure only
3. ⚠️  Keep empty fields in files for future use

**Time:** ~15 minutes

### Option C: Schema-Only Changes

**Do:**
1. ✅ Update schema to make unused fields optional
2. ⚠️  Leave YAML files unchanged

**Time:** ~5 minutes

## Data Quality Improvements (Future)

If we want to improve the 79% UNKNOWN parameters:

1. **Type Inference:** Parse Pascal/COBOL examples to infer types
2. **Pattern Matching:** Map common descriptions to types
   - "Logical device number" → INT
   - "File name" → STRING (64 chars)
   - "Flag" → BOOLEAN
   - "Error code" → INT
3. **Manual Review:** For the remaining unclear cases

**Estimated effort:** 2-4 hours

## Example Before/After

### Before (Current)
```yaml
short_names: []
notes: []
references: []
compatibility:
  platforms:
    nd100: false
    nd500: false
  users: []
  programs: []
  notes: ''
errors:
  returns_error: false
  error_codes: []
  error_reference: ''
```

### After (Proposed)
```yaml
short_names: ["DXABS"]    # Only if exists
compatibility:
  nd100: true
  nd500: false
  user_programs: true
  rt_programs: true
  system_programs: false
```

**Savings:** ~15 lines per file × 230 files = **~3,450 lines removed**

## Next Steps

1. **Decision:** Choose Option A, B, or C
2. **Implementation:** Create cleanup script
3. **Validation:** Run schema validation on updated files
4. **Commit:** Document changes and push

---

**Recommendation: Option A (Full Cleanup)**

The schema should reflect reality, not wishful thinking. Remove what we don't have, simplify what we do have, and make the structure clearer and more maintainable.
