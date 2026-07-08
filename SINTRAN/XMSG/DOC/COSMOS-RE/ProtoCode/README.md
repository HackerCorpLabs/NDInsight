# COSMOS-RE ProtoCode - C# Behavioral Reconstructions

**Readable C# reconstructions of what the four COSMOS binaries do. "What the code does" documentation - NOT runnable ND-100 emulators, and NEVER a transport build-spec (the kernel transport below `MON 200B` is invisible in these binaries).**

The files compile clean on net8.0. Confidence tags in the comments follow the scheme in
[../README.md](../README.md).

---

## Files

| File | Program reconstructed |
|------|-----------------------|
| [CosConnToE02.cs](CosConnToE02.cs) | `cos-conn-to-e02.prog` - CONNECT-TO client: TAD-over-XMSG, receive dispatch table, startup, disconnect |
| [CosFaServerE04.cs](CosFaServerE04.cs) | `cos-fa-serv-e04.prog` - File-Access server: data-driven typed-param protocol, file-entry state machine |
| [CosFileTraE02.cs](CosFileTraE02.cs) | `cos-file-tra-e02.prog` - File Transfer: XMSG transport + QFORM formatting |
| [CosXftraE02.cs](CosXftraE02.cs) | `cos-xftra-e02.prog` - XMSG transport exerciser: client/server loopback tester |

The corresponding analysis documents are in [../Analysis/](../Analysis/README.md).

---

**Parent:** [../README.md](../README.md)
