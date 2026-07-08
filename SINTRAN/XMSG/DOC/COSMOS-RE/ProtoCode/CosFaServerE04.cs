// CosFaServerE04.cs
// ---------------------------------------------------------------------------
// Behavioural C# reconstruction of cos-fa-serv-e04.prog (COSMOS File-Access
// Server) from the ND-100 disassembly. This is NOT a runnable ND-100 emulator;
// it shows *what the code does* — the protocol, data model, and control flow —
// in readable C#. Method fidelity is tagged:
//    [FAITHFUL] = matches decoded logic closely
//    [APPROX]   = intent captured; exact bit-twiddling simplified
// Source of truth: COS-FA-SERV-E04-Analysis.md + the annotated Ghidra DB.
// ---------------------------------------------------------------------------
using System;
using System.Collections.Generic;

namespace Cosmos.FaServer
{
    // ---- XMSG transport (MON 200B) -----------------------------------------
    // T = function code | option bits; A/D = params; X = port/pointer.
    public enum XfFunction
    {
        XFDCT = 1, XFGET = 2, XFREL = 3, XFRHD = 4, XFWHD = 5, XFREA = 6,
        XFWRI = 7, XFSCM = 8, XFMST = 9, XFOPN = 10, XFCLS = 11, XFSND = 12,
        XFRCV = 13, XFPST = 14, XFGST = 15
    }

    [Flags]
    public enum XfOption
    {
        None = 0,
        XFTCM = 1 << 8, XFSEC = 1 << 9, XFROU = 1 << 10, XFFWD = 1 << 11,
        XFBNC = 1 << 12, XFHIP = 1 << 13, XFWAK = 1 << 14, XFWTF = 1 << 15
    }

    // The 6-word XMSG header (XM-block) written by XFWHD (see XMSG-POFTABS).
    public struct XmsgHeader
    {
        public ushort DestSystem, DestPort;   // XMDSY, XMDPT
        public ushort SrcSystem, SrcPort;     // XMSSY, XMSPT
        public ushort Checksum;               // XMCSM
    }

    /// <summary>The MON 200B XMSG kernel interface, as the wrappers @a0bc use it.</summary>
    public interface IXmsg
    {
        int  Open(ushort magic, XfOption opt);                 // XFOPN  -> port
        void Close(int port);                                  // XFCLS
        int  GetBuffer(int sizeBytes, XfOption opt);           // XFGET
        void Release(int msg);                                 // XFREL
        void WriteHeader(int msg, in XmsgHeader h);            // XFWHD
        void WriteBytes(int msg, byte[] data);                 // XFWRI
        int  Send(int msg, int destPort, XfOption opt);        // XFSND -> status
        int  Receive(int port, XfOption opt, out int msg);     // XFRCV -> status
        int  ReadBytes(int msg, byte[] into, int count);       // XFREA
    }

    // ---- File-access request protocol --------------------------------------
    // [BIN-VERIFIED] There is NO numeric FA opcode on the wire. Reading the engine
    // (fa_request_engine_process 0x8c99): it consumes a typed-param stream and has no
    // switch(opcode) — the operation is resolved data-drivenly from which params are
    // present + the target file-entry's type/state (see ClassifyOperation). This enum
    // is only a *derived* label matching the operator-display command names at
    // BANK2::8731; do NOT read it from a wire byte.
    public enum FaRequestOp
    {
        FileEntryDisconnect, ReserveFileEntry, ReleaseFileEntry,
        ChangeFileEntryId, OpenFile, CloseFile, SetBlockSize, ReadFile,
        WriteFile, CreateFile, DeleteFile, SiiiSpecial, DeviceFunction, Unknown
    }

    public enum FaStatus
    {
        Ok = 0, BadType = 3, TableFull = 5, AlreadyFree = 8, NotReserved = 0x0D,
        Reserve = 0x28, Release = 0x29
    }

    // QFORM typed parameter (tag + value); the wire body is a list of these.
    // [BIN-VERIFIED] wire tag bytes, read directly from each emitter's tag word:
    //   msg_put_param_word    (0x7a55, tag word @0x7a8c) -> 0x92  (int, 2 bytes)
    //   msg_put_param_dword   (0x7a91, tag word @0x7ac8) -> 0x94  (int, 4 bytes)
    //   msg_put_param_typed_b (0x7acd, tag word @0x7b04) -> 0xA2  (class A, 2 bytes)
    //   msg_put_param_typed_c (0x7b45, tag word @0x7b7c) -> 0xF2  (class F, 2 bytes)
    // Encoding appears to be (type_class << 4) | length_in_bytes  [INFERRED].
    // NOTE: the request-PARSE side (fa_parse_request_params 0x29c0) compares against
    // 0x01/0x10/0x80 — those are the decoded internal type indices, NOT confirmed to be
    // the raw wire tag byte; treat as [UNVERIFIED] until the parser tag load is traced.
    public enum ParamTag : byte { Int16 = 0x92, Int32 = 0x94, ClassA = 0xA2, String = 0xF2 }
    public readonly struct FaParam
    {
        public readonly ParamTag Tag; public readonly long Value; public readonly string Text;
        public FaParam(ParamTag t, long v, string s = null) { Tag = t; Value = v; Text = s; }
    }

    /// <summary>The ~0x2e-word file-entry descriptor (fields named from the disasm).</summary>
    public sealed class FileEntry
    {
        public int  Type;              // entry[+1] top-2-bits: 1 del,2 res,8 chg,0x10 data,0x80 named
        public int  ReservationBits;   // entry[+0xa] owner/lock sub-field
        public int  IdHandle;          // entry[+8]
        public long Attributes;        // entry[+0xb..] id/attr/size
        public int  State;             // entry[+0x11]: 2 ok / 3,4 err
        public bool ValidLock;         // far status word ~[+0x7bf] bit15
        public byte[] DataBuffer = new byte[0x800];   // ~[+0x7ba] embedded page
        public int  Position, Remaining;
        public FileEntry Session5Chain, Session6Chain; // entry[+5]/[+6]
        public int  SlotIndex;
    }

    /// <summary>
    /// The COSMOS file-access server. One instance owns the *FA-SERVER/*FA-FSA
    /// port and services file-entry requests over XMSG.
    /// </summary>
    public sealed class FaServer
    {
        private readonly IXmsg _xmsg;
        private int _port;
        private ushort _blockSize = 100;   // g_fa_blocksize_9020, seeded in init

        // Global registries (g_fa_reg_904c/904d/904e/904f) + the entry table.
        private readonly List<FileEntry> _entries = new();
        private readonly bool[] _slotBitmap = new bool[0x20];   // 32 slots

        public FaServer(IXmsg xmsg) { _xmsg = xmsg; }

        // fa_init_global_registries (0x3fee) + fa_init_server_data_structures (0x2d68)
        // [FAITHFUL] intent
        private void InitDataStructures()
        {
            _entries.Clear();
            Array.Clear(_slotBitmap, 0, _slotBitmap.Length);
            _blockSize = 100;
        }

        // fa_server_main (0x0500): register the two names with XROUT, open port. [FAITHFUL]
        public void Startup()
        {
            InitDataStructures();
            RegisterServiceName("*FA-SERVER", logicalPort: 11);
            RegisterServiceName("*FA-FSA",    logicalPort: 7);
            _port = _xmsg.Open(magic: 0, opt: XfOption.None);
            RunRequestLoop();
        }

        // Build the XROUT letter (byte[1]=0x45 service request) and send it
        // routed + waited (XFROU|XFWTF) — this is how a name is registered. [FAITHFUL]
        private void RegisterServiceName(string name, int logicalPort)
        {
            var letter = new List<byte> { 0x01, 0x45 };  // marker, service byte (bit6 set)
            letter.AddRange(System.Text.Encoding.ASCII.GetBytes(name));
            int msg = _xmsg.GetBuffer(letter.Count, XfOption.None);
            _xmsg.WriteBytes(msg, letter.ToArray());
            _xmsg.Send(msg, destPort: 0 /*XROUT*/, opt: XfOption.XFROU | XfOption.XFWTF);
        }

        // cos_fa_serv_e04 (0x23c3) + fa_recv_request_wait (0x8c5d) +
        // fa_request_engine_process (0x8c99).
        // [BIN-VERIFIED shape] The engine reads a typed-param stream (NO opcode), then
        // resolves the operation data-drivenly (params + target entry type/state) and acts.
        private void RunRequestLoop()
        {
            while (true)
            {
                if (_xmsg.Receive(_port, XfOption.XFWTF, out int msg) != 0) continue;
                List<FaParam> prms = ParseRequest(msg);        // fa_parse_request_params (0x29c0)
                FaRequestOp op = ClassifyOperation(prms);      // data-driven — NOT a wire opcode
                FaParam[] reply = Dispatch(op, prms);
                SendReply(msg, reply);
                _xmsg.Release(msg);
            }
        }

        // fa_parse_request_params (0x29c0): the request body is a typed-param stream —
        // there is NO leading opcode byte. Each param's first byte has bit 7 (0x80) set
        // as the "typed-param present" marker (fa_read_typed_param 0xa004 tests bit 7);
        // the low 7 bits are the type/class. fa-serv tags = 0x92/0x94/0xA2/0xF2 (all
        // bit7-set). 0xFF terminates the list. [BIN-VERIFIED marker; exact class map [INF]]
        private List<FaParam> ParseRequest(int msg)
        {
            var buf = new byte[256];
            _xmsg.ReadBytes(msg, buf, buf.Length);
            var prms = new List<FaParam>();
            int i = 0;
            while (i < buf.Length && buf[i] != 0xFF)      // 0xFF = end marker
            {
                byte b = buf[i++];
                if ((b & 0x80) == 0) continue;            // bit7 = typed-param marker; skip untyped bytes
                var tag = (ParamTag)b;
                switch (tag)
                {
                    case ParamTag.Int16:  prms.Add(new FaParam(tag, BitConverter.ToInt16(buf, i))); i += 2; break;
                    case ParamTag.Int32:  prms.Add(new FaParam(tag, BitConverter.ToInt32(buf, i))); i += 4; break;
                    case ParamTag.String: int n = buf[i++]; prms.Add(new FaParam(tag, n, System.Text.Encoding.ASCII.GetString(buf, i, n))); i += n; break;
                    default:              prms.Add(new FaParam(tag, buf[i++])); break;
                }
            }
            return prms;
        }

        // [BIN-VERIFIED that this is how it works; the exact param->op mapping is [APPROX]]
        // The operation is NOT carried as an opcode. fa_request_engine_process (0x8c99)
        // reads the params, accumulates flags/values, then acts based on the target
        // file-entry's type entry[+1] {1 del, 2 reserved, 8 change, 0x10 data, 0x80 named}
        // + reservation state. This method models that resolution.
        private FaRequestOp ClassifyOperation(List<FaParam> p)
        {
            if (p.Count == 0) return FaRequestOp.Unknown;
            FileEntry target = FindEntry((int)p[0].Value);
            int type = target?.Type ?? 0;
            return type switch
            {
                0x10 => p.Count >= 2 ? FaRequestOp.ReadFile : FaRequestOp.OpenFile, // entry-type-0x10 data path
                8    => FaRequestOp.ChangeFileEntryId,
                2    => FaRequestOp.ReleaseFileEntry,
                1    => FaRequestOp.DeleteFile,
                0x80 => FaRequestOp.CreateFile,
                _    => target == null ? FaRequestOp.ReserveFileEntry : FaRequestOp.SetBlockSize
            };
        }

        // Acts on the classified operation (each case = a named handler in the disasm).
        private FaParam[] Dispatch(FaRequestOp op, List<FaParam> p) => op switch
        {
            FaRequestOp.ReserveFileEntry    => ReserveFileEntry(p),      // 0x2ca5
            FaRequestOp.ReleaseFileEntry    => ReleaseFileEntry(p),      // 0x34cd
            FaRequestOp.ChangeFileEntryId   => ChangeFileEntryId(p),     // 0x2e12
            FaRequestOp.OpenFile            => OpenFile(p),              // 0x2eae
            FaRequestOp.CloseFile           => CloseFile(p),             // 0x2f2d
            FaRequestOp.ReadFile or
            FaRequestOp.WriteFile           => FileDataTransfer(op, p),  // 0x315b
            FaRequestOp.CreateFile          => CreateFileEntry(p),       // 0x3294/0x3332
            FaRequestOp.DeleteFile          => DeleteFileEntry(p),       // 0x34f8
            FaRequestOp.SetBlockSize        => SetBlockSize(p),          // 0x33d6
            FaRequestOp.FileEntryDisconnect => ReleaseAllSessionEntries(p), // 0x27f4
            _                               => StatusReply(FaStatus.BadType)
        };

        // ---- request op handlers (all [APPROX]; logic mirrors the disasm) --

        private FaParam[] ReserveFileEntry(List<FaParam> p)
        {
            var e = FindOrAllocEntry((int)p[0].Value);
            if (e == null) return StatusReply(FaStatus.TableFull);
            e.ReservationBits |= 1;                         // set owner bit
            e.Type = 2;
            return StatusReply(FaStatus.Reserve);
        }

        private FaParam[] ReleaseFileEntry(List<FaParam> p)
        {
            var e = FindEntry((int)p[0].Value);
            if (e == null || e.Type != 2) return StatusReply(FaStatus.BadType);
            e.ReservationBits = 0;
            return StatusReply(FaStatus.Release);
        }

        private FaParam[] ChangeFileEntryId(List<FaParam> p)
        {
            var e = FindEntry((int)p[0].Value);
            if (e == null || e.ReservationBits == 0 || e.Type != 8) return StatusReply(FaStatus.NotReserved);
            bool accumulate = p.Count > 2 && p[2].Value != 0;
            if (accumulate) e.Attributes += p[1].Value; else e.Attributes = p[1].Value;
            return BuildEntryReply(e);
        }

        private FaParam[] OpenFile(List<FaParam> p)
        {
            var e = FindEntry((int)p[0].Value);
            if (e == null || !e.ValidLock) return StatusReply(FaStatus.NotReserved);
            if (e.Remaining <= 0) return StatusReply(FaStatus.BadType);
            return BuildEntryReply(e);
        }

        private FaParam[] CloseFile(List<FaParam> p)
        {
            var e = FindEntry((int)p[0].Value);
            if (e == null) return StatusReply(FaStatus.BadType);
            if (--e.Remaining <= 0) { e.ValidLock = false; FreeEntry(e); }
            return StatusReply(FaStatus.Ok);
        }

        // Read/Write share the data-transfer path (fa_file_data_transfer 0x315b).
        private FaParam[] FileDataTransfer(FaRequestOp op, List<FaParam> p)
        {
            var e = FindEntry((int)p[0].Value);
            if (e == null || e.Type != 0x10 || !e.ValidLock) return StatusReply(FaStatus.BadType);
            int count = (int)p[1].Value;
            // position/remaining bookkeeping in the entry's ~0x800-byte page
            e.Position += count; e.Remaining -= count;
            return new[] { new FaParam(ParamTag.Int16, e.Position), new FaParam(ParamTag.Int16, e.Remaining) };
        }

        private FaParam[] CreateFileEntry(List<FaParam> p)
        {
            var e = AllocEntry();
            if (e == null) return StatusReply(FaStatus.TableFull);
            e.IdHandle = (int)p[0].Value;
            e.ReservationBits |= 1; e.State = 2;
            if (p.Count > 1 && p[1].Tag == ParamTag.String) e.Type = 0x80;  // named
            return BuildEntryReply(e);
        }

        private FaParam[] DeleteFileEntry(List<FaParam> p)
        {
            var e = FindEntry((int)p[0].Value);
            if (e == null || e.Type != 1) return StatusReply(FaStatus.BadType);
            FreeEntry(e);
            return StatusReply(FaStatus.Ok);
        }

        // fa_blocksize_config_op (0x33d6): return/negotiate the transfer block size.
        private FaParam[] SetBlockSize(List<FaParam> p)
        {
            if (p.Count > 0) _blockSize = (ushort)p[0].Value;
            return new[] { new FaParam(ParamTag.Int16, _blockSize) };
        }

        private FaParam[] ReleaseAllSessionEntries(List<FaParam> p)
        {
            foreach (var e in _entries.ToArray())
                if (e.ReservationBits != 0) FreeEntry(e);
            return StatusReply(FaStatus.Ok);
        }

        // ---- entry table / bitmap allocator --------------------------------
        // fa_bitmap_find_free_slot (0x26d9) / fa_bitmap_free_slot (0x271a). [FAITHFUL]
        private FileEntry AllocEntry()
        {
            for (int i = 0; i < _slotBitmap.Length; i++)
                if (!_slotBitmap[i])
                {
                    _slotBitmap[i] = true;
                    var e = new FileEntry { SlotIndex = i };
                    _entries.Add(e);
                    return e;
                }
            return null;   // error 5 = table full
        }
        private void FreeEntry(FileEntry e)
        {
            if (e.SlotIndex >= 0) _slotBitmap[e.SlotIndex] = false;
            _entries.Remove(e);
        }
        private FileEntry FindEntry(int key) => _entries.Find(e => e.IdHandle == key);          // fa_find_global_entry_by_key (0x28b2)
        private FileEntry FindOrAllocEntry(int key) => FindEntry(key) ?? AllocEntry();

        // ---- reply serialization (msg_put_param_* emitters) ----------------
        private static FaParam[] StatusReply(FaStatus s) => new[] { new FaParam(ParamTag.Int16, (int)s) };

        // fa_build_full_entry_reply (0x393a) etc.: serialize the entry as typed params.
        private static FaParam[] BuildEntryReply(FileEntry e) => new[]
        {
            new FaParam(ParamTag.Int16, e.IdHandle),
            new FaParam(ParamTag.Int16, e.State),
            new FaParam(ParamTag.Int32, e.Attributes),
        };

        // fa_format_server_status_msg (0x2645): the operator console message.
        public string FormatStatusMessage(int serverNo, string state) => $"FA-server {serverNo:00} {state}";

        private void SendReply(int reqMsg, FaParam[] reply)
        {
            var body = new List<byte>();
            foreach (var pr in reply)
            {
                body.Add((byte)pr.Tag);
                switch (pr.Tag)
                {
                    case ParamTag.Int16:  body.AddRange(BitConverter.GetBytes((short)pr.Value)); break;
                    case ParamTag.Int32:  body.AddRange(BitConverter.GetBytes((int)pr.Value)); break;
                    case ParamTag.String: body.Add((byte)pr.Value); body.AddRange(System.Text.Encoding.ASCII.GetBytes(pr.Text ?? "")); break;
                    default:              body.Add((byte)pr.Value); break;
                }
            }
            body.Add(0xFF);  // end marker
            int msg = _xmsg.GetBuffer(body.Count, XfOption.None);
            _xmsg.WriteBytes(msg, body.ToArray());
            _xmsg.Send(msg, _port, XfOption.None);
        }
    }
}
