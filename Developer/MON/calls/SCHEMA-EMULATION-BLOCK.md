# MON call YAML - the `emulation:` block

**This document:** `/mnt/e/Dev/Ronny/NDInsight/Developer/MON/calls/SCHEMA-EMULATION-BLOCK.md`

The MON call YAMLs were extracted from the scanned manual
(`SINTRAN III Monitor Calls`, ND-860228.2 EN). They carry what the manual says.
They do **not** carry what we have since learned by actually implementing MON
emulation and watching real ND programs (the NC compiler, CAT-500, the ND linker)
issue these calls.

That knowledge is expensive - some of it took days of byte-level tracing - and it
currently lives scattered across emulator source comments, git commit messages
and handoff docs. This block brings it back into the YAML, which is the source of
truth both emulators work from.

## Rules (non-negotiable)

1. **The manual is the truth.** The `emulation:` block never contradicts the
   manual's own fields; it *adds* what the manual omits or what we proved.
2. **Never invent.** Every claim is either read from a primary source (cite doc +
   page + table), read from a carve (cite the segment + octal address), or
   observed in a real trace (cite the program + PC). If it is none of those, it
   goes under `unverified:` - or is left out.
3. **Label inference as inference.** "The linker's usage demands it" is not the
   same as "the manual says it". Say which.
4. **Do not touch existing fields.** Only append the `emulation:` block. If an
   existing manual field looks wrong, record the disagreement under
   `emulation.discrepancies:` - do not edit the manual field.
5. **Absent knowledge is fine.** A call we never implemented gets no block, or a
   minimal one with `status: not_implemented`. An empty block beats a fabricated
   one.

## Schema

```yaml
emulation:
  status: verified | implemented | partial | stub | not_implemented
  # verified        = implemented AND proven against a real program trace
  # implemented     = real logic, not yet proven end to end
  # partial         = some functions/paths real, others stubbed
  # stub            = returns success/error without doing the work (SAY SO)
  # not_implemented = dispatches to nothing

  nd500x_handler: /home/ronny/repos/nd500x/src/libmon/handlers/mon_<N>B_<Name>.c
  last_updated: 'YYYY-MM-DD'

  # OPTIONAL - only for multi-function calls (144B, 336B, 60B ...)
  function_codes:
    radix: octal | decimal          # SAY WHICH. Getting this wrong is a real bug.
    source:
      doc: 'ND-60.050.06 SINTRAN III Users Guide'
      page: 232
      table: '9.1'
    codes:
      - code: 0
        name: Read-Record
        description: ...
        param1: ...
        param2: ...
        status: implemented | stub | not_implemented

  # OPTIONAL - per-parameter facts the manual does not give
  parameter_notes:
    - name: Buffer
      note: ...
      verified: true | false

  # OPTIONAL but VALUABLE - real observed traffic. This is what lets an
  # implementer reproduce a case without re-deriving it.
  observed_calls:
    - caller: 'ND linker (linker-b01.dom) @0xB004E9BC'
      params:
        FunctionCode: 0
        DeviceNo: 65
      expectation: 'what the caller does with the result, and what breaks if wrong'
      note: ...

  return_contract:
    success: ...
    errors:
      - code: 56              # decimal as the emulator returns it
        octal: 056B
        meaning: 'No such file'

  # What is PROVEN, and by what evidence. Keep the evidence, not just the claim.
  verified:
    - claim: ...
      evidence: 'byte cite / trace cite / manual cite'

  # Honest gaps. These are as valuable as the proven facts - they stop the next
  # person presenting a guess as fact.
  unverified:
    - ...

  # Where the manual and reality disagree, or where an earlier belief was WRONG.
  discrepancies:
    - was: 'ambiguous file name is 0111'
      is: 'ambiguous is 057, no-such is 056'
      evidence: 'carve GOBJI @056326, terminal codes 056576-056607'

  sources:
    - doc: ...
      page: ...
      note: ...
```

## Reference example

See `144B_DeviceFunction.yaml` - it is the fullest worked example (function-code
table from primary source, observed linker traffic, an explicitly inferred
parameter-size decision, and honest unverified gaps).

## Where the knowledge lives (for backfilling)

- nd500x handlers: `/home/ronny/repos/nd500x/src/libmon/handlers/mon_*.c` - the
  comments carry carve citations and manual references.
- Handoff docs: `/home/ronny/repos/nd500x/docs/HANDOFF_*.md`,
  `/home/ronny/repos/nd500x/docs/MON_CSHARP_SYNC_HANDOFF.md`.
- Commit messages: `git -C /home/ronny/repos/nd500x log --oneline -- src/libmon/`
  - these are often the richest source; each fix records why.
- Carves: `/mnt/e/Dev/Ronny/NDInsight/tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/`
