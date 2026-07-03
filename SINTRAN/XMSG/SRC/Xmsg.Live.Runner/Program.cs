// Live runner: connects the tested Xmsg.Live LiveNode to a real nd100x --hdlc TCP
// bridge as an XMSG node, brings up the LAPB link, answers reachability, and prints
// every decoded SINTRAN frame using the Xmsg.Protocol decoder. This is the runnable
// entry point for the live node (not test code) — the user asked to exercise the
// C# stack against the live emulator.
//
// Usage:  Xmsg.Live.Runner [host] [port] [nodeDecimal] [seconds]
//   defaults: 127.0.0.1 10364 103 120

using System;
using System.Threading;
using System.Threading.Tasks;

using System.Collections.Generic;

using NDInsight.Sintran.Xmsg;
using NDInsight.Sintran.Xmsg.Hdlc;
using NDInsight.Sintran.Xmsg.ListRouting;
using NDInsight.Sintran.Xmsg.Live;

internal static class Program
{
    private static async Task<int> Main(string[] args)
    {
        string host = args.Length > 0 ? args[0] : "127.0.0.1";
        int port = args.Length > 1 ? int.Parse(args[1]) : 10364;
        ushort node = (ushort)(args.Length > 2 ? int.Parse(args[2]) : 103);
        int seconds = args.Length > 3 ? int.Parse(args[3]) : 120;

        Console.WriteLine($"[runner] connecting to {host}:{port} as node {node} for {seconds}s");

        using CancellationTokenSource cts = new CancellationTokenSource(TimeSpan.FromSeconds(seconds));

        TcpBridgeTransport transport;
        try
        {
            transport = await TcpBridgeTransport.ConnectAsync(host, port, cts.Token);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"[runner] connect failed: {ex.Message}");
            return 1;
        }

        LapbLink link = new LapbLink(node);
        // ackCounter 0 — only relevant to the secure-ACK path; reachability does not use it.
        XmsgNode xnode = new XmsgNode(node, 0x00);
        // Do not inject a 0x03 ACK to arbitrary data frames (that crashed the kernel), but DO
        // answer list-route (XSGSY) requests from the routing table with the byte-validated
        // ListRoutingServer reply — the structurally correct response.
        xnode.AcknowledgeData = false;

        // From node 103's perspective. Values mirror the captured reply pattern (a queried
        // system that is ourselves = Local, 0 hops, ExtraInfo = the system number).
        // INFERRED entries for 100/102 (neighbour / via) in case they are queried.
        List<RoutingTableEntry> entries = new List<RoutingTableEntry>
        {
            new RoutingTableEntry(100, XroutConnectionType.Neighbour, 1, 1, 0),
            new RoutingTableEntry(102, XroutConnectionType.Via, 100, 2, 0),
            new RoutingTableEntry(node, XroutConnectionType.Local, node, 0, 0),
        };
        xnode.RoutingTable = new InMemoryRoutingTable(entries);

        // Simulated remote machine: answer connect-to with the MOTD + menu terminal (1 Time,
        // 2 Date, 3 Echo, 4 Disconnect). First live cut — the connect-accept sequencing is
        // INFERRED (echo pattern) and will be tuned against what machine 100 accepts.
        xnode.TadResponder = new NDInsight.Sintran.Xmsg.Live.Tad.TadTerminalResponder(
            node, () => DateTime.Now);

        // Secure-ACK the TAD connect + session frames on the session-constant ACK channel
        // (connect-channel + 4, VERIFIED D9->DD / DA->DE). Without these ACKs 100 retransmits the
        // connect/setup and stalls; the previous crash was from ACKing on the wrong (+0) channel,
        // which is now fixed. This is the identified next step to get 100 to drive the session.
        xnode.AcknowledgeTadFrames = true;

        // Log every LAPB body we transmit so the handshake is visible.
        link.OnTransmit += body =>
        {
            string kind;
            byte ctrl = body.Length > 1 ? body[1] : (byte)0;
            if ((ctrl & 1) == 0) kind = $"I ns={(ctrl >> 1) & 7} nr={(ctrl >> 5) & 7}";
            else if ((ctrl & 3) == 1) kind = $"RR nr={(ctrl >> 5) & 7}";
            else kind = ctrl switch { 0x3F => "SABM", 0x73 => "UA", _ => $"U 0x{ctrl:X2}" };
            // Log the FULL body hex for I-frames so our exact transmitted bytes can be byte-diffed
            // against the captured responder frames (essential for diagnosing the connect crash).
            string extra = (ctrl & 1) == 0 ? $" body={Convert.ToHexString(body)}" : string.Empty;
            Console.WriteLine($"[TX] a=0x{(body.Length > 0 ? body[0] : 0):X2} {kind} state={link.State}{extra}");
        };

        // Log every delivered SINTRAN information field, decoded by the real Xmsg.Protocol
        // decoder. The LiveNode also subscribes to OnInformation (to send responses); events
        // are multicast so both fire.
        link.OnInformation += info =>
        {
            ReadOnlySpan<byte> span = info.Span;
            if (span.Length < 13 || span[0] != 0x21)
            {
                return;
            }

            XmsgFrame f = XmsgFrame.Parse(span);
            string hex = Convert.ToHexString(span);
            Console.WriteLine(
                $"[RX] {f.Header.SourceNode}->{f.Header.DestinationNode} " +
                $"sub={f.Header.Subtype} proto={f.Header.ProtocolId} " +
                $"f1=0x{f.Header.Flags1:X4} info={hex}");

            // For connect-to / TAD analysis: print the FULL decoded frame (XMSG sub-header,
            // magic-number ports, and the TAD opcode chain) so live connect-to traffic can be
            // read the same way as the captured decode report. Only for data frames that carry
            // a sub-header (short ACK/reachability frames have none).
            if (f.Header.Subtype == SintranPacketSubtype.Data && f.SubHeader != null)
            {
                // Decode ports via the magic-number model: wire port = (logical<<7)|low7.
                ushort dp = f.SubHeader.DestinationPort;
                ushort sp = f.SubHeader.SourcePort;
                Console.WriteLine(
                    $"      ports: src {f.SubHeader.SourceSystem}:{sp} (log {sp >> 7}/low {sp & 0x7F})" +
                    $" -> dst {f.SubHeader.DestinationSystem}:{dp} (log {dp >> 7}/low {dp & 0x7F})" +
                    $"  XMCSM=0x{f.SubHeader.ControlService:X8} role=0x{f.SubHeader.Role:X2}");
                // XmsgDump renders the TAD chain (TMOD/TTYP/OPSV/BDAT/…) when present.
                Console.Write(NDInsight.Sintran.Xmsg.Diagnostics.XmsgDump.ToText(f));
            }
        };

        // Compose the tested LiveNode (transport + link + node) and initiate the link.
        LiveNode live = new LiveNode(transport, link, xnode);

        // Log every raw LAPB frame 100 sends us (RR keepalives, SABM, reachability, data).
        live.OnRawFrameReceived += body =>
        {
            byte ctrl = body.Length > 1 ? body[1] : (byte)0;
            string kind;
            if ((ctrl & 1) == 0) kind = $"I ns={(ctrl >> 1) & 7} nr={(ctrl >> 5) & 7}";
            else if ((ctrl & 3) == 1) kind = $"RR nr={(ctrl >> 5) & 7}";
            else kind = ctrl switch { 0x3F => "SABM", 0x73 => "UA", _ => $"U 0x{ctrl:X2}" };
            Console.WriteLine($"[rx-raw] a=0x{(body.Length > 0 ? body[0] : 0):X2} {kind} [{Convert.ToHexString(body)}]");
        };

        link.Connect(0);
        Console.WriteLine("[runner] SABM sent; pumping live link (LAPB + reachability + decode)...");

        try
        {
            // No periodic keepalive: after SABM a healthy link stays RUN silently (no 4-byte
            // frames until hangup), so we only send RR reactively when 100 sends an I-frame.
            await live.RunAsync(cts.Token);
        }
        catch (OperationCanceledException)
        {
            Console.WriteLine("[runner] time window elapsed; stopping.");
        }
        catch (Exception ex)
        {
            Console.WriteLine($"[runner] pump error: {ex.Message}");
        }
        finally
        {
            transport.Dispose();
        }

        Console.WriteLine("[runner] done.");
        return 0;
    }
}
