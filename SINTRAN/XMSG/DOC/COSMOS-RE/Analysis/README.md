# COSMOS-RE Analysis Documents

**Per-program reverse-engineering analyses plus the cross-program synthesis. These are the authoritative write-ups of the [COSMOS-RE](../README.md) work.**

Start with the synthesis for the big picture, then drill into the per-program docs.

---

## Files

| File | Scope |
|------|-------|
| [COSMOS-XMSG-Synthesis.md](COSMOS-XMSG-Synthesis.md) | **Start here** - cross-program synthesis: the shared `MON 200B` XMSG transport usage, per-program wire proofs, and the FA layer |
| [COS-CONN-TO-E02-Analysis.md](COS-CONN-TO-E02-Analysis.md) | `cos-conn-to-e02.prog` (CONNECT-TO TAD client): transport, XMSG envelope, TAD opcode table, startup, receive dispatch, disconnect |
| [COS-FA-SERV-E04-Analysis.md](COS-FA-SERV-E04-Analysis.md) | `cos-fa-serv-e04.prog` (File-Access server): 13 ops, QFORM tags, entry descriptor, data-driven typed-param model |
| [COS-FILE-TRA-E02-XMSG-Analysis.md](COS-FILE-TRA-E02-XMSG-Analysis.md) | `cos-file-tra-e02.prog` (File Transfer): XMSG layer, QFORM formatter, page unit |
| [COS-XFTRA-E02-Analysis.md](COS-XFTRA-E02-Analysis.md) | `cos-xftra-e02.prog` (transport exerciser): XMSG wrapper library, letter-indexed param types |

Confidence tags (`[BIN-VERIFIED]`, `[SYM]`, `[INFERRED]`, ...) are defined in
[../README.md](../README.md). The analysed binaries live in
`Installation/Communication/COSMOS Basic/x/`
([../../../../../Installation/Communication/COSMOS%20Basic/x/README.md](../../../../../Installation/Communication/COSMOS%20Basic/x/README.md)).

---

**Parent:** [../README.md](../README.md)
