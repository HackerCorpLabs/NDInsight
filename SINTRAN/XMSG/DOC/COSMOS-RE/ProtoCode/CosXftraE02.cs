// =============================================================================
//  CosXftraE02.cs
//  ---------------------------------------------------------------------------
//  A C# rendering of the DECODED LOGIC of the ND-100 SINTRAN-III program
//  "cos-xftra-e02.prog" — the COSMOS XMSG TRANSPORT EXERCISER (a Client/Server
//  loopback tester for the *ae-transport / XMSG layer).
//
//  NOT a port and NOT runnable against hardware. It is a faithful, heavily-
//  commented transcription of what the reverse-engineered PLANC/assembly does,
//  so the XMSG message flow can be read in C#. Each method notes its Ghidra
//  address (ram:XXXX) and is tagged [VERIFIED] (straight from disassembly /
//  the XMSG symbol files), [INFERRED] (deduced from pattern) or [CANDIDATE].
//
//  *** LAYER BOUNDARY CAVEAT (read first) ***
//  This binary is APPLICATION-LEVEL, running ABOVE the MON 200B (XMSG) kernel call.
//  The transport envelope (seed / Counter / channel derivation), the stateless
//  secure-ACK closed form, the odd-length LAPB address rule, and the <=2-datagram
//  flow-control window are all KERNEL-INVISIBLE here and CANNOT be recovered from
//  this .cs. This file explains the app's INTENT; it is NOT a wire build-spec. A
//  node built from this alone will crash the real machine (as the early probes did)
//  unless the kernel-level envelope from XMSG-PROTOCOL.md is layered underneath.
//
//  Re-verified 2026-07-07 against the binary (Corrections Brief): fixed the opcode
//  mask (0xFF00, not 0xFFFF), the param-type encoding (letter-indexed, not numeric
//  0x92 tags), and the receive-loop SAT 3/4 (XFREL/XFRHD calls, not msg-type ==3/4).
//
//  Sources:
//    - Ghidra program cos-xftra-e02.prog (44 functions, all renamed)
//    - E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\COSMOS-RE\Analysis\COS-XFTRA-E02-Analysis.md
//    - F:\ND\SINTRAN-K05-XMSG-2026\XMSG-Symb\XMSG-PL-VALUES-L.INCL
//
//  Style: no LINQ, no foreach, explicit loops, dense comments (project rules).
// =============================================================================

using System;

namespace Ndinsight.Cosmos.Xftra
{
    // -------------------------------------------------------------------------
    //  XMSG function codes (T low byte, decimal). The exerciser's inline wrapper
    //  library at ram:2fa1-30b3 implements exactly these. [VERIFIED from the
    //  SAA immediate in each thunk.]
    // -------------------------------------------------------------------------
    internal enum XmsgFunc
    {
        XFDCT = 1,   // disconnect from message system
        XFGET = 2,   // get message space
        XFREL = 3,   // release message space
        XFRHD = 4,   // read 6-byte header
        XFREA = 6,   // read message -> user buffer
        XFWRI = 7,   // write user -> message
        XFSCM = 8,   // set current message
        XFMST = 9,   // get message status
        XFOPN = 10,  // open port
        XFCLS = 11,  // close port
        XFSND = 12,  // send message
        XFRCV = 13,  // receive message
        XFSIN = 16   // service initialisation (privileged) — server registers its name
    }

    // Option bits OR'd into T (bit numbers -> (1<<bit)). [VERIFIED.]
    [Flags]
    internal enum XmsgOpt
    {
        None  = 0,
        XFSEC = 1 << 9,   // secure — returned if not delivered
        XFROU = 1 << 10,  // route via XROUT
        XFFWD = 1 << 11,  // forward
        XFWAK = 1 << 14,  // wake task on status change (XFRCV)
        XFWTF = 1 << 15   // wait until terminated
    }

    // Message types returned as positive XFRCV status. [VERIFIED.]
    internal enum XmsgMsgType { XMTNO = 1, XMROU = 2, XMTHI = 3, XMTRE = 4 }

    // Errors this binary explicitly checks. [VERIFIED.]
    internal enum XmsgError { XEILM = -21, XEIMA = -19 }

    // Param type system. [BIN-VERIFIED 2026-07-07] decode_param_value (ram:640a) masks the
    // type byte with 0x7F (strip parity), subtracts 0x41 ('A'), and does a 6-way computed
    // jump — so the wire type codes are LETTER-INDEXED in the range 0x41..0x46 ('A'..'F')
    // (optionally with bit7 set), NOT the numeric 0x92/0x94/0xA2/0xF2 tags that cos-fa-serv
    // uses (no `SAA 0x92/0x94` exists anywhere in this binary — searched).
    //   - INT16 / INT32 / STRING are 3 of the 6 DISPLAY names (strings @163b).
    //   - The exact letter->kind mapping is [CANDIDATE]: decode the 6 jump targets @6418
    //     to confirm which letter is which type. Do NOT treat the numeric values below as
    //     wire bytes — they are placeholder ordinals until the jump table is decoded.
    internal enum ParamType { LetterA = 0x41, LetterB, LetterC, LetterD, LetterE, LetterF }

    // -------------------------------------------------------------------------
    //  One test-message parameter. decode_param_value (ram:640a) walks an array
    //  of these in 6-word entries. Layout [INFERRED] from stride + display cols
    //  ("Param Type Data(Dec/Hex) Length").
    // -------------------------------------------------------------------------
    internal struct TestParam
    {
        public ParamType Type;
        public int Length;
        public long Data;  // holds INT16/INT32 value; STRING via a separate buffer
    }

    // -------------------------------------------------------------------------
    //  Outgoing message descriptor handed to XFWRI. Same [op][len][body] shape
    //  as cos-file-tra. [VERIFIED in xmsg_build_and_send_message @ram:5e89.]
    //     word0 = opcode = (opcodeSource & msg_opcode_mask) rotated right
    //     word1 = subLength ; byte count = subLength + 4
    // -------------------------------------------------------------------------
    internal sealed class MessageDescriptor
    {
        public ushort Opcode;
        public ushort SubLength;
        public byte[] Body = Array.Empty<byte>();
        public int ByteCount => SubLength + 4;
    }

    // The XMSG kernel via MON 200B (Ghidra: the 11 wrappers all end in MON 0x80).
    internal interface IXmsgKernel
    {
        int Call(XmsgFunc fn, XmsgOpt opt, ref int a, ref int d, ref int x);
    }

    // SINTRAN monitor calls other than XMSG that the program uses. [VERIFIED
    // from the MON opcodes: DOPEN 220B, HOLD 104B, OUTBT 2B, QERMS 65B,
    // CLOSE 43B, LEAVE 0B.]
    internal interface ISintran
    {
        int DirectOpen(string deviceName);  // MON 220B DOPEN  (*ae-transport)
        void Hold();                         // MON 104B HOLD   (sleep in recv loop)
        void OutByte(byte b);                // MON 2B  OUTBT
        void CloseFile(int fileNo);          // MON 43B CLOSE
        void Leave();                        // MON 0B  LEAVE   (exit)
    }

    internal enum Role { Client, Server, DummyClient, DummyServer }

    // -------------------------------------------------------------------------
    //  Run configuration collected by the interactive menu (main body).
    //  Field meanings are VERIFIED from the prompt strings; this is just the bag.
    // -------------------------------------------------------------------------
    internal sealed class RunConfig
    {
        public Role Role;
        public int MessageCount;       // "No. of messages to transmit"
        public int MessageLength;      // "Message length in bytes"
        public bool AutoPattern;       // "Automatic generated pattern (y/n)"
        public int StartPattern;       // "Start pattern (octal)"
        public int Increment;          // "Increment (octal)"
        public bool EchoMode;          // "Echo mode (y/n)"
        public bool DisplayInfo;       // "Display of transfer info (y/n)"
        public string ServerSystem = "";  // "Server system name?"
        public string ServerPort = "";     // "Server port name:"
    }

    // =========================================================================
    //  The exerciser.
    // =========================================================================
    internal sealed class CosXftraE02
    {
        private readonly IXmsgKernel _xmsg;
        private readonly ISintran _os;

        private int _port = 0;             // -0x77,B  local XMSG port
        private int _message = 0;          // current message handle

        // ram:5ec6 msg_opcode_mask. [BIN-VERIFIED 2026-07-07: word at ram:5ec6 = 0xFF00]
        // (An earlier version wrongly claimed 0xFFFF "as read" — it was a placeholder, not
        //  a decoded value. Corrected.) Used as: opcode = RORA(-0x64,B & 0xFF00), i.e. the
        //  HIGH byte of -0x64,B, at ram:5e8d-5e8e.
        private const ushort MsgOpcodeMask = 0xFF00;

        public CosXftraE02(IXmsgKernel xmsg, ISintran os) { _xmsg = xmsg; _os = os; }

        // ---------------------------------------------------------------------
        //  ENTRY. Ghidra: cos_xftra_e02 @ram:0000 (JMP to init). [VERIFIED role
        //  from strings; sequencing INFERRED.]
        // ---------------------------------------------------------------------
        public void Main(RunConfig cfg)
        {
            _os.DirectOpen("*ae-transport");     // device_direct_open @62b6 (MON 220B)
            DisconnectAndReopenPort();           // 5dee

            if (cfg.Role == Role.Server || cfg.Role == Role.DummyServer)
                ServiceInit();                   // 633a (XFSIN) register named service

            if (cfg.Role == Role.Client || cfg.Role == Role.DummyClient)
            {
                PrepareTransferBuffers(cfg);     // 6266 / 6076 / 61bd
                ValidateTransferParams(cfg);     // 64a8
                RunClient(cfg);
            }
            else
            {
                RunServer(cfg);
            }

            Teardown();                          // XFCLS/XFDCT
            _os.Leave();                         // terminate_program @3c31 (MON 0B)
        }

        // ---------------------------------------------------------------------
        //  Transport reset: XFDCT then XFOPN. Ghidra: xmsg_disconnect_and_reopen_port
        //  @ram:5dee. [VERIFIED.]
        // ---------------------------------------------------------------------
        public void DisconnectAndReopenPort()
        {
            int a = 0, d = 0, x = 0;
            _xmsg.Call(XmsgFunc.XFDCT, XmsgOpt.None, ref a, ref d, ref x); // 5df1
            // (zero-out the port table loop @5df4 omitted)
            a = 0; d = 0; x = 0;
            _xmsg.Call(XmsgFunc.XFOPN, XmsgOpt.None, ref a, ref d, ref x); // 5df9 via wrapper
            _port = a;                                                     // -> -0x68,B
        }

        // ---------------------------------------------------------------------
        //  Server: register the service name. Ghidra: xmsg_service_init @ram:633a
        //  (SAA 0x10 = XFSIN). [VERIFIED opcode; registration role INFERRED.]
        // ---------------------------------------------------------------------
        public void ServiceInit()
        {
            int a = 0, d = 0, x = 0;
            int st = _xmsg.Call(XmsgFunc.XFSIN, XmsgOpt.None, ref a, ref d, ref x);
            if (st < 0) HandleError(0x25); // 6343 SAA 0x25 on failure
        }

        // ---------------------------------------------------------------------
        //  CLIENT loop — transmit N messages. Composed from:
        //    xmsg_alloc_and_write_message (5e0a, XFGET+XFWRI)
        //    xmsg_build_and_send_message  (5e89, XFWRI + XFSND|XFFWD)
        //    send_secure_test_message     (62bf, XFWRI 70B + XFSND|XFSEC)
        //  [VERIFIED opcodes; loop assembly INFERRED from the config prompts.]
        // ---------------------------------------------------------------------
        public void RunClient(RunConfig cfg)
        {
            int pattern = cfg.StartPattern;
            for (int i = 0; i < cfg.MessageCount; i++)
            {
                MessageDescriptor desc = new MessageDescriptor
                {
                    Opcode = (ushort)(RotateRight((ushort)(i & MsgOpcodeMask))),
                    SubLength = (ushort)cfg.MessageLength,
                    Body = MakePattern(cfg, ref pattern)
                };

                AllocAndWriteMessage(desc);                 // 5e0a
                if (cfg.EchoMode) SendSecureTestMessage(desc);   // 62bf (secure)
                else              BuildAndSendMessage(desc);     // 5e89 (forwarded)

                if (cfg.DisplayInfo) DisplayTransferInfo(i);
            }
        }

        // ---------------------------------------------------------------------
        //  SERVER loop — receive and (optionally) echo. Ghidra:
        //  receive_message_wait_loop @ram:63a7. [BIN-VERIFIED 2026-07-07]
        //  CORRECTION: the SAT 3 / SAT 4 in the disasm are XMSG FUNCTION CODES
        //  (XFREL / XFRHD) passed to the gateway, NOT message-type == 3/4 compares.
        //  The only status test is ==0 (no message) vs >0; there is NO XMTHI (==3)
        //  filter and the echo send uses XFSND|XFSEC (not XFHIP).
        // ---------------------------------------------------------------------
        public void RunServer(RunConfig cfg)
        {
            byte[] buf = new byte[128];
            for (;;)
            {
                _os.Hold();                                  // MON 104B HOLD
                int a = 0, d = 0, x = _port;
                int st = _xmsg.Call(XmsgFunc.XFRCV, XmsgOpt.XFWAK, ref a, ref d, ref x);
                if (st == 0) continue;                       // no message -> retry
                if (st < 0) break;                           // error

                // st>0 : a message arrived. Read its 6-byte header (XFRHD) then the
                // payload (XFREA); on release path use XFREL. [VERIFIED @63bf/63c3]
                int ha = 0, hd = 0, hx = 0;
                _xmsg.Call(XmsgFunc.XFRHD, XmsgOpt.None, ref ha, ref hd, ref hx); // SAT 4
                ReadMessageBytes(buf);                       // XFREA
                DecodeParamValue(buf);                       // 640a -> "Param Type Data Length"
                if (cfg.EchoMode)
                {
                    // Echo the message back SECURE. [VERIFIED @63d3: XFSND | XFSEC]
                    int ea = -1, ed = 0, ex = _port;
                    _xmsg.Call(XmsgFunc.XFSND, XmsgOpt.XFSEC, ref ea, ref ed, ref ex);
                }
                else
                {
                    int ra = _message, rd = 0, rx = 0;
                    _xmsg.Call(XmsgFunc.XFREL, XmsgOpt.None, ref ra, ref rd, ref rx); // SAT 3
                }
            }
        }

        // ---- transport primitives (each is a MON 200B via the wrapper lib) ----

        // Ghidra: xmsg_alloc_and_write_message @ram:5e0a (XFGET then XFWRI).
        public void AllocAndWriteMessage(MessageDescriptor desc)
        {
            int a = desc.ByteCount, d = 0, x = 0;
            int st = _xmsg.Call(XmsgFunc.XFGET, XmsgOpt.None, ref a, ref d, ref x); // 5e11
            if (st < 0) { HandleError(st); return; }
            _message = a;
            a = 0; d = desc.ByteCount; x = _message;
            _xmsg.Call(XmsgFunc.XFWRI, XmsgOpt.None, ref a, ref d, ref x);          // 5e15
        }

        // Ghidra: xmsg_build_and_send_message @ram:5e89 (XFWRI + XFSND|XFFWD).
        public void BuildAndSendMessage(MessageDescriptor? desc)
        {
            int a = 0, d = 0, x = _message;
            int st = _xmsg.Call(XmsgFunc.XFWRI, XmsgOpt.None, ref a, ref d, ref x); // 5e97
            if (st < 0 && st == (int)XmsgError.XEILM) { HandleError(st); return; }
            a = -1;
            _xmsg.Call(XmsgFunc.XFSND, XmsgOpt.XFFWD, ref a, ref d, ref x);         // 5ea4
        }

        // Ghidra: send_secure_test_message @ram:62bf (XFWRI 70B + XFSND|XFSEC).
        public void SendSecureTestMessage(MessageDescriptor desc)
        {
            int a = 0, d = 0x46 /* 70 bytes */, x = _message;
            int st = _xmsg.Call(XmsgFunc.XFWRI, XmsgOpt.None, ref a, ref d, ref x); // 62cb
            if (st < 0) { HandleError((int)XmsgError.XEILM); return; }
            a = -1; x = _port;
            int s2 = _xmsg.Call(XmsgFunc.XFSND, XmsgOpt.XFSEC, ref a, ref d, ref x);// 62d9
            if (s2 == (int)XmsgError.XEIMA) HandleInvalidMagic();                  // 62da
        }

        // Ghidra: xmsg_XFREA @ram:2ff2. Read received payload (<=128 bytes).
        public int ReadMessageBytes(byte[] dest)
        {
            int a = 0, d = dest.Length, x = 0;
            return _xmsg.Call(XmsgFunc.XFREA, XmsgOpt.None, ref a, ref d, ref x);
        }

        // Ghidra: xmsg_disconnect via XFCLS/XFDCT teardown.
        public void Teardown()
        {
            int a = _port, d = 0, x = 0;
            if (_message != 0) { a = _message; _xmsg.Call(XmsgFunc.XFREL, XmsgOpt.None, ref a, ref d, ref x); _message = 0; }
            if (_port != 0)    { a = _port;    _xmsg.Call(XmsgFunc.XFCLS, XmsgOpt.None, ref a, ref d, ref x); _port = 0; }
        }

        // ---- non-transport helpers (stubs mirroring the named routines) ----

        // clear_transfer_state @6076 + prepare_transfer_buffers @6266 + prepare_message_header @61bd
        private void PrepareTransferBuffers(RunConfig cfg) { }
        // validate_transfer_params @64a8
        private void ValidateTransferParams(RunConfig cfg) { }
        // decode_param_value @640a (INT16/INT32/STRING typed-param decode for display)
        private void DecodeParamValue(byte[] buf) { }
        // print_line_sequence @5f2d / print_qstr_to_terminal @3c10
        private void DisplayTransferInfo(int index) { }

        private void HandleError(int code) { }
        private void HandleInvalidMagic() { }

        // Build the message payload for this iteration from the pattern generator.
        private static byte[] MakePattern(RunConfig cfg, ref int pattern)
        {
            byte[] b = new byte[cfg.MessageLength];
            for (int i = 0; i < b.Length; i++)
            {
                b[i] = (byte)(cfg.AutoPattern ? (pattern & 0xFF) : cfg.StartPattern);
                pattern += cfg.Increment;
            }
            return b;
        }

        // ND-100 RORA-style rotate used at ram:5e8e when forming the opcode word.
        private static ushort RotateRight(ushort v) => (ushort)((v >> 1) | (v << 15));
    }
}
