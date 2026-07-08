// =============================================================================
//  CosFileTraE02.cs
//  ---------------------------------------------------------------------------
//  A C# rendering of the DECODED LOGIC of the ND-100 SINTRAN-III program
//  "cos-file-tra-e02.prog" (COSMOS File Transfer, Version E02).
//
//  This is NOT a port and NOT runnable against real hardware. It is a faithful,
//  heavily-commented transcription of what the reverse-engineered PLANC/assembly
//  actually does, so a C# reader can follow the XMSG message flow without reading
//  ND-100 disassembly. Every method notes its Ghidra address (ram:XXXX) and is
//  tagged [VERIFIED] (read straight from disassembly / the XMSG symbol files),
//  [INFERRED] (deduced from pattern, not single-stepped) or [CANDIDATE].
//
//  *** LAYER-BOUNDARY CAVEAT (read first) ***
//  This binary is APPLICATION-LEVEL, ABOVE the MON 200B (XMSG) kernel call. The
//  transport envelope (seed/Counter/channel), the stateless secure-ACK closed form,
//  the odd-length LAPB address rule, and the <=2-datagram flow-control window are all
//  KERNEL-INVISIBLE here and cannot be recovered from this .cs. This file explains the
//  app's INTENT, not the wire build-spec; a node built from it alone crashes the real
//  machine unless XMSG-PROTOCOL.md's envelope is layered underneath.
//  Re-verified 2026-07-07: the ONLY on-wire send is the XROUT XSGNI query (opcode 0x0845,
//  byte1 0x45) [VERIFIED]; its reply payload is UNKNOWN (not invented); the file-transfer
//  data commands have NO traced send edge and no wire format is claimed for them.
//
//  Source of truth:
//    - Disassembly in Ghidra (program cos-file-tra-e02.prog)
//    - E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\COSMOS-RE\Analysis\COS-FILE-TRA-E02-XMSG-Analysis.md
//    - F:\ND\SINTRAN-K05-XMSG-2026\XMSG-Symb\XMSG-PL-VALUES-L.INCL  (XF/XS/XE codes)
//    - F:\ND\SINTRAN-K05-XMSG-2026\XMSG-Symb\XMSG-POFTABS-L03.SYMB  (XM/XD block layout)
//
//  Coding style follows the project rule set: no LINQ, no foreach, explicit loops,
//  comments kept dense on purpose.
// =============================================================================

using System;

namespace Ndinsight.Cosmos.FileTransfer
{
    // -------------------------------------------------------------------------
    //  XMSG function codes (T-register value, low byte). DECIMAL, from
    //  XMSG-PL-VALUES-L.INCL. [VERIFIED against the symbol file.]
    // -------------------------------------------------------------------------
    internal enum XmsgFunc
    {
        XFDCT = 1,   // Disconnect from message system
        XFGET = 2,   // Get message space
        XFREL = 3,   // Release message space
        XFRHD = 4,   // Read 6-byte header
        XFREA = 6,   // Read from message into user buffer
        XFWRI = 7,   // Write from user buffer into message
        XFOPN = 10,  // Open port
        XFCLS = 11,  // Close port
        XFSND = 12,  // Send message to a remote port
        XFRCV = 13   // Receive a message on a port
    }

    // -------------------------------------------------------------------------
    //  Option bits OR'd into the high part of the T-register. Values are BIT
    //  NUMBERS (per the symbol file), applied as (1 << bit). [VERIFIED.]
    // -------------------------------------------------------------------------
    [Flags]
    internal enum XmsgOpt
    {
        None  = 0,
        XFTCM = 1 << 8,   // send task current message
        XFSEC = 1 << 9,   // secure — returned if not delivered
        XFROU = 1 << 10,  // route via XROUT (routing port 0)
        XFFWD = 1 << 11,  // forward
        XFBNC = 1 << 12,  // bounce
        XFRRO = 1 << 13,  // (with XFROU) non-local XROUT, system number in A-reg
        XFWAK = 1 << 14,  // wake task on status change (used with XFRCV)
        XFWTF = 1 << 15   // wait if operation not terminated
    }

    // -------------------------------------------------------------------------
    //  XROUT service codes — value carried in BYTE 1 of the letter, bit 6 set =>
    //  service request. [VERIFIED: symbol-file header says "Values in byte 1 of
    //  message. Bit 6 is set => service request".]
    // -------------------------------------------------------------------------
    internal enum XroutService
    {
        XSLET = 65,  // 0x41 send a letter
        XSGNI = 69,  // 0x45 get name (param: magic-no / port-no)  <= the query this binary sends
        XSGSY = 75,  // 0x4B get routing info for system N
        XSGIN = 82,  // get information about name
        XSLSY = 92   // get information about a system
    }

    // -------------------------------------------------------------------------
    //  XMSG user error codes returned in T (negative). Subset actually checked
    //  by this program. [VERIFIED against XMSG-PL-VALUES-L.INCL.]
    // -------------------------------------------------------------------------
    internal enum XmsgError
    {
        XEMFL = -20,  // message space full / buffer not available
        XEILM = -21,  // illegal message size or not enough space left
        XENRU = -37   // XMSG not running
    }

    // -------------------------------------------------------------------------
    //  The XM-block transport header — the words sent verbatim on the link.
    //  Layout from XMSG-POFTABS-L03.SYMB (5MESS). Big-endian 16-bit words on the
    //  ND-100 wire. [VERIFIED from the symbol file.]
    // -------------------------------------------------------------------------
    internal struct XmTransportHeader
    {
        public ushort XMTHD;   // transport header = 0o20400 (version 2, protocol 1)
        public ushort XMSTA;   // status word (5M* bits: XFSEC->5MSEC, XFROU->5MROU, ...)
        public ushort XMDSY;   // destination system number
        public ushort XMDPT;   // destination port/random  (XMDST = XMDSY:XMDPT = "magic no")
        public ushort XMSSY;   // source system number
        public ushort XMSPT;   // source port/random       (XMSRC = XMSSY:XMSPT)
        public ushort XMCSM;   // checksum, else message size

        public const ushort X5THD = 0x2100; // 0o20400 = version 2, protocol 1
    }

    // -------------------------------------------------------------------------
    //  The message DESCRIPTOR this program hands to XFWRI. Local buffer at
    //  ram:-0x7c,B. Only the first two words are structural; the rest is body.
    //  [VERIFIED in xmsg_send_then_receive_timeout @ram:6b3a.]
    //
    //    word0 = request/opcode word (its low byte is the XROUT service code)
    //    word1 = sub-length
    //    bytes written by XFWRI = word1 + 4
    // -------------------------------------------------------------------------
    internal sealed class MessageDescriptor
    {
        public ushort Opcode;     // descriptor[0]
        public ushort SubLength;  // descriptor[1]
        public byte[] Body = Array.Empty<byte>(); // trailing bytes (server name / params)

        public int ByteCount => SubLength + 4; // XFWRI transfer length
    }

    // -------------------------------------------------------------------------
    //  The XMSG kernel as seen through MON 200B. In the real system this is a
    //  single monitor call; here it is an interface so the flow reads naturally.
    //  Ghidra: xmsg_mon_call @ram:7b8f (the MON 0x80 at ram:7bb2). [VERIFIED.]
    //
    //  Convention:  T = fncode | options ; A/D = params ; X = buffer/handle.
    //  Returns status in T (0 ok/pending, <0 XE* error, >0 message type on RCV).
    // -------------------------------------------------------------------------
    internal interface IXmsgKernel
    {
        // status = MON200B(T, A, D, X). Returns the T (status) result; 'a'/'x' are ref
        // because XMSG hands back a result value in A and a pointer in X.
        int Call(XmsgFunc fn, XmsgOpt opt, ref int a, ref int d, ref int x);
    }

    // =========================================================================
    //  The program itself.
    // =========================================================================
    internal sealed class CosFileTraE02
    {
        private readonly IXmsgKernel _xmsg;

        // Persistent handles that the ND-100 code keeps in frame locals.
        private int _portHandle = 0;     // -0x77,B : local XMSG port (0 = not open)
        private int _messageHandle = 0;  // -0x78,B : current message buffer (0 = none)

        // The one baked request opcode this binary transmits.
        // ram:68ee = 0x0845  ->  wire bytes 08 45 ; byte1 0x45 = XSGNI. [VERIFIED.]
        private const ushort XroutQueryOpcode0845 = 0x0845;

        public CosFileTraE02(IXmsgKernel xmsg) => _xmsg = xmsg;

        // ---------------------------------------------------------------------
        //  STARTUP — open the local XMSG port.
        //  Ghidra: xmsg_open_port @ram:70d6 (SAA 0xa = XFOPN). [VERIFIED.]
        //  If a port is already open it is a no-op (the ND-100 code EXITs early).
        // ---------------------------------------------------------------------
        public void OpenPort()
        {
            if (_portHandle >= 1) return;            // already open -> EXIT (ram:70d9)

            int a = 0, d = 0, x = 0;
            int st = _xmsg.Call(XmsgFunc.XFOPN, XmsgOpt.None, ref a, ref d, ref x);
            if (st < 0) throw new XmsgException(st); // no port available
            _portHandle = a;                         // returned magic number -> -0x77,B
        }

        // ---------------------------------------------------------------------
        //  Allocate a message buffer.
        //  Ghidra: xmsg_get_message_space @ram:622b (SAA 2 = XFGET). [VERIFIED.]
        //  On failure the ND-100 code forces A = -0x14 (XEMFL, space full).
        // ---------------------------------------------------------------------
        public int GetMessageSpace(int sizeBytes)
        {
            int a = sizeBytes, d = 0, x = 0;
            int st = _xmsg.Call(XmsgFunc.XFGET, XmsgOpt.None, ref a, ref d, ref x);
            if (st < 0) throw new XmsgException((int)XmsgError.XEMFL); // ram:6235 SAA -0x14
            _messageHandle = a;                       // -> -0x78,B
            return a;
        }

        // ---------------------------------------------------------------------
        //  Send a secure, routed letter to XROUT.
        //  Ghidra: xmsg_send_secure_routed_letter @ram:7b89. Sets
        //     T = XFSND | XFSEC | XFROU | XFRRO
        //  and falls into the MON path. Destination SYSTEM NUMBER is in A (because
        //  XFRRO means non-local XROUT). [VERIFIED from the SAA/BSET bits.]
        // ---------------------------------------------------------------------
        private int SendSecureRoutedLetter(int destSystemNumber)
        {
            int a = destSystemNumber, d = 0, x = _messageHandle;
            XmsgOpt opt = XmsgOpt.XFSEC | XmsgOpt.XFROU | XmsgOpt.XFRRO;
            return _xmsg.Call(XmsgFunc.XFSND, opt, ref a, ref d, ref x);
        }

        // ---------------------------------------------------------------------
        //  Request/reply primitive: write the descriptor, send it, then poll for
        //  the reply with a timeout.
        //  Ghidra: xmsg_send_then_receive_timeout @ram:6b3a. [VERIFIED.]
        //
        //  Steps (exactly as the disassembly):
        //    1. descriptor[0] = opcode (A on entry)     ram:6b3d
        //    2. bytecount     = descriptor[1] + 4        ram:6b3e-6b40
        //    3. XFWRI bytecount bytes from descriptor     ram:6b41-6b46
        //    4. XFSND (secure/routed) to destSystem       ram:6b47-6b49
        //    5. loop: XFRCV | XFWAK ; if status==0 sleep   ram:6b4d..
        //         via MON 267B (TMOUT) and decrement a
        //         retry counter (ram:6b39). status>0 => got
        //         reply; status<0 => error.
        // ---------------------------------------------------------------------
        public ReplyResult SendThenReceive(MessageDescriptor desc, int destSystem, int retryLimit)
        {
            // (1)(2)(3) — write the descriptor into the current message.
            int a = 0, d = desc.ByteCount, x = _messageHandle;
            a = 0; // descriptor pointer stands in for A in the real code
            int wr = _xmsg.Call(XmsgFunc.XFWRI, XmsgOpt.None, ref a, ref d, ref x);
            if (wr < 0) return ReplyResult.Error(wr);

            // (4) — transmit.
            int sndStatus = SendSecureRoutedLetter(destSystem);
            if (sndStatus < 0) return ReplyResult.Error(sndStatus);

            // (5) — poll for the reply with a bounded retry/timeout loop.
            for (int tries = 0; tries < retryLimit; tries++)
            {
                int ra = 0, rd = 0, rx = _portHandle;
                int st = _xmsg.Call(XmsgFunc.XFRCV, XmsgOpt.XFWAK, ref ra, ref rd, ref rx);
                if (st > 0) return ReplyResult.Message(rx, st); // message ready (type in st)
                if (st < 0) return ReplyResult.Error(st);       // hard error
                MonTimeout();                                    // MON 267B: sleep one tick
            }
            return ReplyResult.Timeout();
        }

        // ---------------------------------------------------------------------
        //  Read payload bytes out of the received message.
        //  Ghidra: xmsg_read_message_bytes @ram:6b6d (SAA 6 = XFREA). Reads up to
        //  0x80 (128) bytes, appends a 0x27 (') terminator, tests word0 & 0x00FF
        //  (the returned service/status byte). [VERIFIED.]
        // ---------------------------------------------------------------------
        public int ReadMessageBytes(byte[] dest)
        {
            int a = 0 /* dest ptr */, d = 0x80 /* max 128 bytes */, x = 0;
            int st = _xmsg.Call(XmsgFunc.XFREA, XmsgOpt.None, ref a, ref d, ref x);
            // The ND-100 code then SBYTs a 0x27 terminator and inspects (dest[0] & 0xFF).
            return st;
        }

        // ---------------------------------------------------------------------
        //  XROUT request/reply helper — the ONLY message this binary transmits.
        //  Ghidra: xmsg_xrout_request_reply @ram:6905. [VERIFIED.]
        //
        //  It XFGETs a 128-byte message, opens the port, fills the letter, and
        //  sends+receives via SendThenReceive with the BAKED opcode 0x0845 (XSGNI
        //  "get name"). This resolves a system/server name to a magic number.
        // ---------------------------------------------------------------------
        public ReplyResult XroutNameQuery(string serverOrSystemName, int destSystem)
        {
            OpenPort();                               // ram:6948
            GetMessageSpace(0x80);                    // ram:6907-6909 (size 0x80)

            // Build the letter body. descriptor[0] = 0x0845 (byte1 = 0x45 = XSGNI),
            // followed by the name as a QSTRING built one byte at a time.
            MessageDescriptor desc = new MessageDescriptor
            {
                Opcode = XroutQueryOpcode0845,        // ram:68ee -> passed as A into 6b3a
                SubLength = (ushort)serverOrSystemName.Length,
                Body = BuildNameQString(serverOrSystemName)
            };

            return SendThenReceive(desc, destSystem, retryLimit: 20);
        }

        // ---------------------------------------------------------------------
        //  Build a QSTRING = [count][bytes...][0x27].  The ND-100 code assembles
        //  it byte-by-byte with qstr_put_byte (ram:70e1) into the static buffer
        //  at ram:59d4 (builder at ram:691e). [VERIFIED mechanism.]
        // ---------------------------------------------------------------------
        private static byte[] BuildNameQString(string name)
        {
            // +1 for the trailing 0x27 (') sentinel used by the QSTRING helpers.
            byte[] q = new byte[name.Length + 1];
            for (int i = 0; i < name.Length; i++)
                q[i] = (byte)name[i];
            q[name.Length] = 0x27; // ' terminator
            return q;
        }

        // ---------------------------------------------------------------------
        //  TEARDOWN, per-transfer.
        //  Ghidra: xmsg_release_msg_and_close_port @ram:70c7. Releases the live
        //  message (XFREL) and/or closes the port (XFCLS). [VERIFIED.]
        // ---------------------------------------------------------------------
        public void ReleaseMessageAndClosePort()
        {
            int a = 0, d = 0, x = 0;
            if (_messageHandle != 0)                  // -0x78,B live?
            {
                a = _messageHandle;
                _xmsg.Call(XmsgFunc.XFREL, XmsgOpt.None, ref a, ref d, ref x);
                _messageHandle = 0;
            }
            if (_portHandle != 0)                     // -0x77,B live?
            {
                a = _portHandle;
                _xmsg.Call(XmsgFunc.XFCLS, XmsgOpt.None, ref a, ref d, ref x);
            }
        }

        // ---------------------------------------------------------------------
        //  TEARDOWN, top-level: leave the message system entirely.
        //  Ghidra: xmsg_disconnect @ram:7c4f (SAA 1 = XFDCT). Drops all ports and
        //  allocated messages. [VERIFIED — this corrects the first-pass claim that
        //  XFDCT was unused.]
        // ---------------------------------------------------------------------
        public void Disconnect()
        {
            _messageHandle = 0;                       // ram:7c51 STZ -0x78
            if (_portHandle != 0)
            {
                int a = _portHandle, d = 0, x = 0;
                _xmsg.Call(XmsgFunc.XFDCT, XmsgOpt.None, ref a, ref d, ref x); // ram:7c56
                _portHandle = 0;
            }
        }

        // ---------------------------------------------------------------------
        //  The file-transfer engine.
        //  Ghidra: file_transfer_loop_driver @ram:758e. Reads the source file
        //  object's byte-pointer/size, divides by the PAGE SIZE 0x800 (2048 bytes
        //  = 1 ND page) via RDIV to get the number of page transfers, starts a
        //  timer (MON 11B GetBasicTime), and loops per page dispatching to the
        //  per-page send/receive helpers. [VERIFIED structure; per-page opcode is
        //  formatter-driven, see note.]
        // ---------------------------------------------------------------------
        public void FileTransferLoop(FileObject src)
        {
            const int PageSizeBytes = 0x800;          // ram:7602 constant

            long totalBytes = src.MaxBytePointer;     // [X+0x20]/[X+0x22]
            long pages = totalBytes / PageSizeBytes;  // RDIV @ram:759c

            long startTicks = MonGetBasicTime();      // MON 11B @ram:75aa

            for (long p = 0; p < pages; p++)
            {
                // Each page is written into a message (XFWRI) and shipped/acked via
                // the request/reply engine. The exact per-page application opcode is
                // emitted by the QFORM formatter (qform_format_message @ram:727d)
                // from a template, not a single inline constant. [INFERRED for the
                // opcode value; the XFWRI->XFSND->XFRCV cycle is VERIFIED.]
                TransferOnePage(src, p);
            }

            long elapsed = MonGetBasicTime() - startTicks;
            ReportTransferRate(totalBytes, elapsed);  // "Completed. Transfer rate: ..."
            FinalizeAndClose(src);                    // SMAX + CLOSE @ram:799d
        }

        // ---- helpers that stand in for MON calls / not-yet-decoded detail ----

        private void TransferOnePage(FileObject src, long page)
        {
            // Placeholder for the per-page message build (formatter-driven body).
            // The verified transport cycle it performs is: XFGET -> XFWRI(page)
            // -> XFSND -> XFRCV(ack). [INFERRED assembly of the body bytes.]
        }

        // Ghidra: finalize_and_close_file @ram:799d — MON 73B (SMAX, set max byte
        // pointer) then MON 43B (CLOSE). [VERIFIED.]
        private void FinalizeAndClose(FileObject src) { /* MON 73B SMAX; MON 43B CLOSE */ }

        // Ghidra: calc_transfer_rate @ram:5f6e — derives Kbytes/sec or Bytes/sec.
        private void ReportTransferRate(long bytes, long elapsedTicks) { }

        private static long MonGetBasicTime() => 0; // MON 11B (TIME)
        private static void MonTimeout() { }        // MON 267B (TMOUT) — sleep one tick
    }

    // -------------------------------------------------------------------------
    //  Supporting types (thin — they only exist so the flow above compiles/reads).
    // -------------------------------------------------------------------------
    internal sealed class FileObject
    {
        public long MaxBytePointer; // file size in bytes (from the SINTRAN file object)
        public int FileNumber;      // SINTRAN open file number
    }

    internal readonly struct ReplyResult
    {
        public readonly int Kind;        // 0 = message, 1 = timeout, 2 = error
        public readonly int Pointer;     // message pointer (on message)
        public readonly int StatusOrType;// message type (>0) or error code (<0)

        private ReplyResult(int kind, int ptr, int st) { Kind = kind; Pointer = ptr; StatusOrType = st; }
        public static ReplyResult Message(int ptr, int type) => new ReplyResult(0, ptr, type);
        public static ReplyResult Timeout() => new ReplyResult(1, 0, 0);
        public static ReplyResult Error(int code) => new ReplyResult(2, 0, code);
    }

    internal sealed class XmsgException : Exception
    {
        public int Status { get; }
        public XmsgException(int status) : base($"XMSG error {status}") => Status = status;
    }
}
