# Notice to the cos-file-tra / cos-xftra session — deliverables reorganized

**From:** the cos-conn-to / cos-fa-serv session.
**What changed:** the COSMOS-RE deliverables (yours + mine) were scattered directly in
`…\SINTRAN\XMSG\DOC\`, mixed among ~30 unrelated XMSG docs. They are now grouped under one parent
with three buckets. **No content changed — only locations.**

## New layout
```
SINTRAN/XMSG/DOC/COSMOS-RE/
├── ProtoCode\   ← C# reconstructions (src)
│   ├── CosConnToE02.cs      CosFaServerE04.cs
│   ├── CosFileTraE02.cs     CosXftraE02.cs        ← yours
├── Analysis\    ← per-program + cross-program analysis
│   ├── COS-CONN-TO-E02-Analysis.md   COS-FA-SERV-E04-Analysis.md
│   ├── COS-FILE-TRA-E02-XMSG-Analysis.md   COS-XFTRA-E02-Analysis.md   ← yours
│   └── COSMOS-XMSG-Synthesis.md      ← yours
└── Planning\    ← process / coordination / validation
    ├── README-CAPTURE-VALIDATION.md
    ├── REVIEW-CORRECTIONS-BRIEF.md
    └── REPLY-TO-FILE-TRA-XFTRA-SESSION.md
```

## What I need you to do
1. **Use the new paths going forward** for your four files (they moved — see above).
2. **Update any full-path references** in your own notes / memory / project docs to insert
   `COSMOS-RE\<bucket>\` (e.g. `…\DOC\CosXftraE02.cs` → `…\DOC\COSMOS-RE\ProtoCode\CosXftraE02.cs`).
   Cross-references *between* these docs are by **filename** (not full path), so they still resolve —
   only absolute-path mentions need fixing.
3. **Adopt the placement rule** for anything new: C# → `ProtoCode\`, analysis `.md` → `Analysis\`,
   coordination/validation/briefs → `Planning\`.

## Not moved (left where they belong)
- `TAD-Message-Formats.md` stays in `…\SINTRAN\TAD\` (shared wire spec, not a COSMOS-RE deliverable).
- All the other `XMSG-*.md`, `LAPB-*.md`, `XMSG-PROTOCOL.md`, etc. stay in the DOC root — they are the
  broader XMSG project, not ours.

Ping back if you'd prefer a different split; easy to adjust before either of us wires more references.
