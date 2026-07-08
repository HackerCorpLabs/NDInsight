// CosConnToE02.cs
// ---------------------------------------------------------------------------
// Behavioural C# reconstruction of cos-conn-to-e02.prog (COSMOS CONNECT-TO
// client — the TAD "asker"/RP role) from the ND-100 disassembly. NOT a runnable
// ND-100 emulator; it shows *what the code does*: the TAD-over-XMSG protocol,
// the receive dispatch table, startup handshake, and disconnect.
//    [FAITHFUL] = matches decoded logic · [APPROX] = intent captured
// Source of truth: COS-CONN-TO-E02-Analysis.md + the annotated Ghidra DB.
// ---------------------------------------------------------------------------
using System;
using System.Collections.Generic;

namespace Cosmos.ConnectTo
{
    // ---- XMSG transport (MON 200B) — same kernel API as the server ----------
    public enum XfFunction { XFDCT=1, XFGET=2, XFREL=3, XFRHD=4, XFWHD=5, XFREA=6,
        XFWRI=7, XFSCM=8, XFMST=9, XFOPN=10, XFCLS=11, XFSND=12, XFRCV=13, XFPST=14, XFGST=15 }

    [Flags] public enum XfOption { None=0, XFSEC=1<<9, XFROU=1<<10, XFHIP=1<<13, XFWAK=1<<14, XFWTF=1<<15 }

    public interface IXmsg
    {
        int  Open(ushort magic, XfOption opt);          // XFOPN -> port
        void Close(int port);                           // XFCLS
        void Disconnect();                              // XFDCT (whole task off the msg system)
        int  GetBuffer(int sizeBytes, XfOption opt);    // XFGET
        void Release(int msg);                          // XFREL
        void WriteBytes(int msg, byte[] data);          // XFWRI
        int  Send(int msg, int destPort, XfOption opt); // XFSND
        int  Receive(int port, XfOption opt, out int msg); // XFRCV
        int  MsgType(int msg);                          // XFRCV-returned msg-type; ==3 = XMTHI high-pri (NOT "Data"=Subtype 0x0E)
        int  ReadBytes(int msg, byte[] into, int count);// XFREA
    }

    // ---- TAD application protocol ------------------------------------------
    // TAD message on the wire = [type:1][count:1][data:count], carried as the
    // payload of an XMSG data frame. Opcodes verified vs TAD-Message-Formats.md.
    // NOTE on confidence: names marked * are CANDIDATE (per TAD-Message-Formats.md §2.1),
    // seen on the wire but not in the NPL symbol tables: CORS(0x07)*, LUN(0x0B) STRONG,
    // FBSI(0x15)* WEAK, ESRS(0x20) STRONG. The rest are NPL-symbol VERIFIED.
    public enum TadOpcode : byte
    {
        BDAT=0x01, RFI=0x02, ECKM=0x03, BMMX=0x04, CORS=0x07 /*CANDIDATE*/, ESCA=0x08, DCON=0x09,
        LUN=0x0B, TMOD=0x0C, TTYP=0x0D, CESC=0x0E, DESC=0x0F, SYCN=0x13, USCN=0x14,
        FBSI=0x15, RESE=0x16, RECO=0x17, DUMM=0x18, OPSV=0x1F, ESRS=0x20, CERS=0x21,
        ISRQ=0x22, ISRS=0x23, NOWT=0x24, TNOW=0x25, NWRE=0x26, RLOC=0x27, TREP=0x2A,
        UMOD=0x2B, MOD8=0x2C, CPCO=0xFA, ERRS=0xFB, POLL=0xFD, REJE=0xFE, EOP=0xFF
    }

    public readonly struct TadMessage
    {
        public readonly TadOpcode Type; public readonly byte[] Data;
        public TadMessage(TadOpcode t, byte[] d = null) { Type = t; Data = d ?? Array.Empty<byte>(); }
        // Wire form: [type][count][data], padded to even offset when required.
        public byte[] ToWire()
        {
            var w = new byte[2 + Data.Length];
            w[0] = (byte)Type; w[1] = (byte)Data.Length; Data.CopyTo(w, 2);
            return w;
        }
    }

    /// <summary>Per-session state (the "session struct" at -0x7a,B in the disasm).</summary>
    public sealed class TadSession
    {
        public int  AskerPort;        // session[+0x13] = the XFOPN'd port
        public int  RxBufferPos;      // session[+0x297] byte position in the TAD chain
        public byte[] RxBuffer = new byte[0x100]; // session[+0x299]
        public int  TxSeqLow, TxSeqHigh;  // g_tx_seq_pair: datagram sequence, +1 per msg
        public bool Connected;        // g_state_0af8
        public byte OsVersion, OsSubVersion, ProtocolVersion; // from OPSV
    }

    /// <summary>
    /// The COSMOS CONNECT-TO client. Opens a port, connects to a remote system
    /// (login handshake), relays terminal traffic, and tears the session down.
    /// </summary>
    public sealed class ConnectToClient
    {
        private readonly IXmsg _xmsg;
        private readonly TadSession _s = new();
        private int _hostPort;   // the remote terminal port learned from 7CORS
        private int _systemPort; // the host's system port (342/358), used for session setup

        public ConnectToClient(IXmsg xmsg) { _xmsg = xmsg; }

        // ---- startup: connect_to_session_setup (0x710d) --------------------
        // [FAITHFUL] shape: open port -> send OPSV handshake -> receive/ack loop.
        public void ConnectTo(ushort remoteSystem, string user, string password)
        {
            _s.AskerPort = _xmsg.Open(magic: 0, opt: XfOption.None);   // XFOPN, stored at session[+0x13]
            _s.Connected = false;

            // The connect request is delivered to the *TADADM server via an XSLET
            // XMSG letter; the host answers with the assigned terminal port (7CORS)
            // and TAD logical unit (7LUN). [APPROX — the letter build is in fa/xrout land]

            // [DOC §22.4] CORRECTION: the asker's setup opcodes travel as ONE CHAINED
            // message to the SYSTEM port (342/358) — not one datagram each, and not to the
            // terminal port (which is unknown until 7CORS arrives). The builder appends
            // [type][count][data] for each opcode into one scratch buffer (session+0x31D)
            // then flushes once via XFWRI+XFSND.
            var setup = new List<TadMessage>
            {
                // 7OPSV [os][sub][protocol]; the version word is octal-formatted before send.
                new TadMessage(TadOpcode.OPSV, new byte[]{ _s.OsVersion, _s.OsSubVersion, _s.ProtocolVersion }),
                // 7TMOD flags / 7TTYP 16-bit type: the builders were NOT byte-traced, so the
                // payloads are UNKNOWN — left empty rather than inventing values.
                // (Earlier draft's TTYP="01 08" was WRONG: 0x0108 is the FBSI terminal class,
                //  not a TTYP payload; TTYP carries the decimal terminal-type, e.g. 104=00 68.)
                new TadMessage(TadOpcode.TMOD, /*flags UNKNOWN*/ Array.Empty<byte>()),
                new TadMessage(TadOpcode.TTYP, /*16-bit type UNKNOWN*/ Array.Empty<byte>()),
            };
            SendChainedToSystemPort(setup);

            RunSessionLoop();   // steady state: relay + dispatch received TAD messages
        }

        // The asker chains its setup opcodes into a single XMSG message to the system port.
        // [DOC-derived; the per-opcode append+flush is the builder model found in the binary.]
        private void SendChainedToSystemPort(IEnumerable<TadMessage> chain)
        {
            var buf = new List<byte>();
            foreach (var m in chain) buf.AddRange(m.ToWire());   // append [type][count][data]
            int msg = _xmsg.GetBuffer(buf.Count, XfOption.None); // XFGET
            _xmsg.WriteBytes(msg, buf.ToArray());                // XFWRI
            _xmsg.Send(msg, _systemPort, XfOption.None);         // XFSND once
            _s.TxSeqLow = (_s.TxSeqLow + 1) & 0xFF;
        }

        // ---- send path: the TAD builders (XFGET -> XFWRI -> XFSND) ----------
        // Each builder writes [type][count][data] into an XMSG buffer, then ships
        // it. The 15 builder sites in the binary funnel through xmsg_XFWRI/XFSND.
        // [FAITHFUL] sequence.
        private void SendTad(TadMessage m, XfOption extra = XfOption.None)
        {
            byte[] wire = m.ToWire();
            int msg = _xmsg.GetBuffer(wire.Length, XfOption.None);   // XFGET
            _xmsg.WriteBytes(msg, wire);                              // XFWRI
            _xmsg.Send(msg, _hostPort, extra);                        // XFSND (options OR'd from g_xfsnd_options)
            _s.TxSeqLow = (_s.TxSeqLow + 1) & 0xFF;                   // g_tx_seq_pair += 1 per message
        }

        // ---- receive path: tad_receive_and_dispatch (0x46db) ----------------
        // XFRCV -> require msg-type==3 -> XFREA the TAD chain -> walk it,
        // dispatching each opcode through the jump table (tad_rx_dispatch_table).
        // [FAITHFUL] shape.
        private void RunSessionLoop()
        {
            while (true)   // exits via `break` below when 7DCON clears _s.Connected
            {
                if (_xmsg.Receive(_s.AskerPort, XfOption.XFWTF, out int msg) != 0) continue;
                // [BIN] require the XFRCV-returned msg-type == 3 = XMTHI (high-priority) per the
                // symbol file — NOT "Data" (0x0E is the SINTRAN-header Subtype, a different field).
                // Implies TAD terminal traffic is sent XFHIP; kernel-local, invisible in pcap.
                if (_xmsg.MsgType(msg) != 3) { _xmsg.Release(msg); continue; }

                int n = _xmsg.ReadBytes(msg, _s.RxBuffer, _s.RxBuffer.Length);   // XFREA
                _xmsg.Release(msg);

                int pos = 0;
                while (pos < n)
                {
                    if (_s.RxBuffer[pos] == 0) { pos++; continue; }   // skip 0x00 pads/16-bit-op prefixes
                    var op = (TadOpcode)_s.RxBuffer[pos];
                    int count = pos + 1 < n ? _s.RxBuffer[pos + 1] : 0;
                    var data = new byte[count];
                    Array.Copy(_s.RxBuffer, pos + 2, data, 0, Math.Min(count, n - pos - 2));
                    DispatchRx(op, data);                              // the ...RUT handlers
                    pos += 2 + count;
                }
                if (!_s.Connected) break;
            }
            Teardown();
        }

        // The opcode-indexed dispatch (tad_rx_dispatch_table @0x330e). Each case is
        // a named tad_rx_<op> handler in the binary. [FAITHFUL] mapping.
        private void DispatchRx(TadOpcode op, byte[] data)
        {
            switch (op)
            {
                case TadOpcode.BDAT: OnTerminalData(data); break;                    // tad_rx_BDAT_01
                case TadOpcode.RFI:  OnReadyForInput(); break;                       // tad_rx_RFI_02
                case TadOpcode.ECKM: OnEchoStrategy(data); break;                    // tad_rx_ECKM_03
                case TadOpcode.BMMX: OnBreakStrategy(data); break;                   // tad_rx_BMMX_04
                case TadOpcode.CORS: OnConnectResponse(data); break;                 // tad_rx_CORS_07: assigned port
                case TadOpcode.DCON: OnDisconnect(); break;                          // tad_rx_DCON_09
                case TadOpcode.LUN:  OnLogicalUnit(data); break;                     // tad_rx_LUN_0b (LU = 768+val)
                case TadOpcode.TMOD: /* terminal mode */ break;                      // tad_rx_TMOD_0c
                case TadOpcode.TTYP: /* terminal type */ break;                      // tad_rx_TTYP_0d
                case TadOpcode.CESC: OnEscapeControl(data); break;                   // tad_rx_CESC_0e
                case TadOpcode.DESC: OnDefineEscape(data); break;                    // tad_rx_DESC_0f
                case TadOpcode.SYCN: OnSystemControl(data); break;                   // tad_rx_SYCN_13 (login ladder)
                case TadOpcode.USCN: OnUserControl(data); break;                     // tad_rx_USCN_14
                case TadOpcode.RESE: OnReset(); break;                               // tad_rx_RESE_16
                case TadOpcode.DUMM: break;                                          // tad_rx_DUMM_18 (skip)
                case TadOpcode.OPSV: OnOsProtocolVersion(data); break;               // tad_rx_OPSV_1f
                case TadOpcode.ESRS: OnEscapeResponse(); break;                      // tad_rx_ESRS_20/29
                case TadOpcode.ISRQ: OnInputSizeRequest(); break;                    // tad_rx_ISRQ_22
                case TadOpcode.NOWT:
                case TadOpcode.TNOW: OnNowaitStatus(data); break;                    // tad_rx_NOWT_24/25
                case TadOpcode.NWRE: OnNowaitRestart(); break;                       // tad_rx_NWRE_26
                case TadOpcode.TREP: OnTerminalStatusReport(data); break;            // tad_rx_TREP_2a
                case TadOpcode.UMOD: OnUmodStrategy(data); break;                    // tad_rx_UMOD_2b
                default:             OnUnhandled(op); break;                          // tad_rx_unhandled_default
            }
        }

        // ---- selected handlers (intent) ------------------------------------
        private void OnConnectResponse(byte[] d)
        {
            // 7CORS [CANDIDATE name] payload, count 5: 00 00 <node> <port_hi> <port_lo>.
            // [DOC-VERIFIED vs pcap + COS-CONN-TO-E02-Analysis.md] port = bytes 3..4.
            // (Earlier draft used d[2]/d[3] — an off-by-one that contradicted its own source doc.)
            if (d.Length >= 5) _hostPort = (d[3] << 8) | d[4];
            _s.Connected = true;
        }
        private void OnLogicalUnit(byte[] d) { /* TAD logical unit = 768 + d[1] */ }
        private void OnOsProtocolVersion(byte[] d)
        {
            if (d.Length >= 3) { _s.OsVersion = d[0]; _s.OsSubVersion = d[1]; _s.ProtocolVersion = d[2]; }
        }
        private void OnSystemControl(byte[] d) { /* 0002 WaitUser / 0006 PassOK / 000A LoggedIn / 000C Err */ }
        private void OnDisconnect() { _s.Connected = false; }          // host asked us to close
        private void OnTerminalData(byte[] d) { /* deliver to local terminal; strip bit7 in 7-bit mode */ }
        private void OnReadyForInput() { /* input credit granted */ }
        private void OnEchoStrategy(byte[] d) { } private void OnBreakStrategy(byte[] d) { }
        private void OnEscapeControl(byte[] d) { } private void OnDefineEscape(byte[] d) { }
        private void OnUserControl(byte[] d) { } private void OnReset() { SendTad(new TadMessage(TadOpcode.RECO)); }
        private void OnEscapeResponse() { } private void OnInputSizeRequest() { }
        private void OnNowaitStatus(byte[] d) { } private void OnNowaitRestart() { }
        private void OnTerminalStatusReport(byte[] d) { } private void OnUmodStrategy(byte[] d) { }
        private void OnUnhandled(TadOpcode op) { /* ignored / logged */ }

        // ---- disconnect: disconnect_teardown (0x44a0) ----------------------
        // Send 7DCON, then close the port and detach the task from XMSG. [FAITHFUL] order.
        public void Disconnect() { _s.Connected = false; }
        private void Teardown()
        {
            SendTad(new TadMessage(TadOpcode.DCON));   // 09 00 — instant close from the terminal port
            _xmsg.Close(_s.AskerPort);                 // XFCLS
            _xmsg.Disconnect();                        // XFDCT — detach from the message system
        }
    }
}
