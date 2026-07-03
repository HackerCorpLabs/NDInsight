// Live runner: connects the C# XMSG stack to a real nd100x --hdlc TCP bridge, brings up the
// LAPB link, answers reachability / list-route, and (with a TAD responder) accepts connect-to.
//
// Two composition paths:
//   * SEAM (default): TcpBridgeTransport -> LapbLinkAdapter(ILink) -> ProtocolDetector ->
//     XmsgCodec -> XmsgLayer. This is the restructured stack (XMSG-TRANSPORT-SEAM-PLAN.md).
//   * LEGACY (first arg == "legacy"): the original LiveNode + XmsgNode wiring, kept until the
//     seam path is proven live against machine 100 (Phase 5 gate).
//
// Usage:  Xmsg.Live.Runner [legacy] [host] [port] [nodeDecimal] [seconds]
//   defaults: seam  127.0.0.1 10364 103 120

using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;

using NDInsight.Sintran.Xmsg;
using NDInsight.Sintran.Xmsg.Codec;
using NDInsight.Sintran.Xmsg.ListRouting;
using NDInsight.Sintran.Xmsg.Live;
using NDInsight.Sintran.Xmsg.Live.Seam;
using NDInsight.Sintran.Xmsg.Packet;

internal static class Program
{
    private static async Task<int> Main(string[] args)
    {
        // Optional leading "legacy" keyword selects the old LiveNode + XmsgNode path.
        bool legacy = args.Length > 0 && string.Equals(args[0], "legacy", StringComparison.OrdinalIgnoreCase);
        int argOffset = legacy ? 1 : 0;

        string host = args.Length > argOffset ? args[argOffset] : "127.0.0.1";
        int port = args.Length > argOffset + 1 ? int.Parse(args[argOffset + 1]) : 10364;
        ushort node = (ushort)(args.Length > argOffset + 2 ? int.Parse(args[argOffset + 2]) : 103);
        int seconds = args.Length > argOffset + 3 ? int.Parse(args[argOffset + 3]) : 120;

        Console.WriteLine($"[runner] path={(legacy ? "legacy" : "seam")} connecting to {host}:{port} as node {node} for {seconds}s");

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

        try
        {
            if (legacy)
            {
                await RunLegacyAsync(transport, node, cts.Token);
            }
            else
            {
                await RunSeamAsync(transport, host, port, node, cts.Token);
            }
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

    /// <summary>
    /// The restructured seam composition: TcpBridgeTransport -> LapbLinkAdapter(ILink) ->
    /// BoundProtocolDetector -> XmsgCodec -> XmsgLayer. Routing/TAD services are configured on the
    /// layer; the link is bound to XMSG, and the detector (a per-link-binding stub) confirms it.
    /// </summary>
    private static async Task RunSeamAsync(
        TcpBridgeTransport transport, string host, int port, ushort node, CancellationToken token)
    {
        string linkId = $"hdlc:{host}:{port}";

        LapbLink link = new LapbLink(node);
        LapbLinkAdapter adapter = new LapbLinkAdapter(linkId, transport, link, LinkBinding.Xmsg);

        // The per-link-binding "detector" (seam stub): this link carries XMSG, decided by config
        // (the ND machine's installed SW), NOT by sniffing bytes. See XMSG-TRANSPORT-SEAM-PLAN.md §5.
        BoundProtocolDetector detector = new BoundProtocolDetector(LinkBinding.Xmsg);

        // Codec sends down through the link; the layer sits above the codec.
        LinkXmsgTransport codecTransport = new LinkXmsgTransport(adapter);
        XmsgCodec codec = new XmsgCodec(linkId, codecTransport);
        XmsgLayer layer = new XmsgLayer(codec, node, 0x00);

        // Same service configuration as the proven legacy node.
        layer.AcknowledgeData = false;
        List<RoutingTableEntry> entries = new List<RoutingTableEntry>
        {
            new RoutingTableEntry(100, XroutConnectionType.Neighbour, 1, 1, 0),
            new RoutingTableEntry(102, XroutConnectionType.Via, 100, 2, 0),
            new RoutingTableEntry(node, XroutConnectionType.Local, node, 0, 0),
        };
        layer.RoutingTable = new InMemoryRoutingTable(entries);
        NDInsight.Sintran.Xmsg.Live.Tad.TadTerminalResponder tad =
            new NDInsight.Sintran.Xmsg.Live.Tad.TadTerminalResponder(node, () => DateTime.Now);
        // Terminal bring-up (DUMM/MOTD) DISABLED. The connect handshake (accept + port-assign, echoed)
        // is stable and 100 ACKs it. The MOTD is BLOCKED on a genuine unknown: terminal-data must ride
        // channel DB (Base 0x02xx), but 100 forces our datagram sequence LOW (it XENSE-rejects a high
        // accept) and a byte counter cannot lift a low Flags1 to Base 0x02xx. Sending on DC/DD instead
        // crashes 100 (XXPER). Resolving this needs the XMSG channel-allocation rule (kernel source) or
        // a capture of a FRESH-sequence responder, not blind live probing. See XMSG-TRANSPORT-SEAM-PLAN.md.
        tad.SendTerminalBringup = false;
        layer.TadResponder = tad;
        layer.AcknowledgeTadFrames = true;

        // UP wiring: a delivered link payload is classified, then (for XMSG) parsed by the codec,
        // which raises PacketReceived to the layer. An X.25-bound link would route elsewhere here.
        adapter.PayloadReceived += delegate (string id, ReadOnlyMemory<byte> payload, int length)
        {
            ReadOnlySpan<byte> span = payload.Span.Slice(0, length);
            LinkBinding binding = detector.Classify(id, span);
            if (binding == LinkBinding.Xmsg)
            {
                codec.ProcessBytes(span);
            }
        };

        // SABM-storm suppression. A peer whose XMSG has crashed re-SABMs forever (bare link, no XMSG
        // behind it). Collapse that churn — the peer SABMs and our UA/RR answers — into ONE warning
        // instead of hundreds of identical lines. Shared by the TX and rx-raw handlers below.
        bool stormActive = false;
        int peerSabmStreak = 0;

        // Diagnostics: log every LAPB body we transmit (link.OnTransmit is multicast, so this fires
        // alongside the adapter's own encode-and-queue handler).
        link.OnTransmit += body =>
        {
            byte ctrl = body.Length > 1 ? body[1] : (byte)0;
            bool isData = (ctrl & 1) == 0;
            // During a peer SABM storm our UA/RR answers are pure churn — suppress them; data frames
            // (I-frames) are always logged.
            if (stormActive && !isData)
            {
                return;
            }

            string kind;
            if (isData) kind = $"I ns={(ctrl >> 1) & 7} nr={(ctrl >> 5) & 7}";
            else if ((ctrl & 3) == 1) kind = $"RR nr={(ctrl >> 5) & 7}";
            else kind = ctrl switch { 0x3F => "SABM", 0x73 => "UA", _ => $"U 0x{ctrl:X2}" };
            string extra = isData ? $" body={Convert.ToHexString(body)}" : string.Empty;
            Console.WriteLine($"[TX] a=0x{(body.Length > 0 ? body[0] : 0):X2} {kind} state={link.State}{extra}");
        };

        // Diagnostics: log every decoded SINTRAN frame the layer receives, the same way the legacy
        // runner did (full XMSG sub-header + TAD chain for data frames).
        layer.MessageReceived += delegate (string id, XmsgPacketInfo packet)
        {
            XmsgFrame f = packet.Frame;
            Console.WriteLine(
                $"[RX] {f.Header.SourceNode}->{f.Header.DestinationNode} " +
                $"sub={f.Header.Subtype} proto={f.Header.ProtocolId} f1=0x{f.Header.Flags1:X4} " +
                $"info={Convert.ToHexString(packet.RawBytes)}");
            if (f.Header.Subtype == SintranPacketSubtype.Data && f.SubHeader != null)
            {
                ushort dp = f.SubHeader.DestinationPort;
                ushort sp = f.SubHeader.SourcePort;
                Console.WriteLine(
                    $"      ports: src {f.SubHeader.SourceSystem}:{sp} (log {sp >> 7}/low {sp & 0x7F})" +
                    $" -> dst {f.SubHeader.DestinationSystem}:{dp} (log {dp >> 7}/low {dp & 0x7F})" +
                    $"  XMCSM=0x{f.SubHeader.ControlService:X8} role=0x{f.SubHeader.Role:X2}");
                Console.Write(NDInsight.Sintran.Xmsg.Diagnostics.XmsgDump.ToText(f));
            }
        };

        layer.SessionOpened += delegate (string id, ushort clientSystem, ushort clientPort)
        {
            Console.WriteLine($"[session] opened by system {clientSystem} port 0x{clientPort:X4}");
        };
        adapter.StatusChanged += delegate (string id, LinkStatus status)
        {
            Console.WriteLine($"[link] {id} status -> {status}");
        };

        // Diagnostic: log every raw LAPB frame 100 sends us (SABM/UA/RR/I). Detects and COLLAPSES a
        // bare-link SABM storm (peer XMSG crashed) into a single actionable warning.
        adapter.RawFrameReceived += delegate (string id, byte[] body)
        {
            byte ctrl = body.Length > 1 ? body[1] : (byte)0;

            if (ctrl == 0x3F)   // SABM from the peer
            {
                peerSabmStreak++;
                if (peerSabmStreak == 3)
                {
                    // Three back-to-back SABMs with no data = the peer's XMSG is not taking the link
                    // to the data phase. Say so plainly and stop echoing the churn.
                    stormActive = true;
                    Console.WriteLine(
                        "[!] Peer is re-sending SABM with no XMSG data. Machine 100's XMSG is almost " +
                        "certainly DOWN (crashed / not started). Restart XMSG on 100, then reconnect. " +
                        "Suppressing link-establishment churn until real traffic resumes...");
                }

                if (stormActive)
                {
                    return;   // suppress the repeated SABM lines
                }
            }
            else
            {
                if (stormActive)
                {
                    Console.WriteLine($"[!] SABM storm ended after {peerSabmStreak} SABMs; traffic resuming.");
                    stormActive = false;
                }

                peerSabmStreak = 0;
            }

            string kind;
            if ((ctrl & 1) == 0) kind = $"I ns={(ctrl >> 1) & 7} nr={(ctrl >> 5) & 7}";
            else if ((ctrl & 3) == 1) kind = $"RR nr={(ctrl >> 5) & 7}";
            else kind = ctrl switch { 0x3F => "SABM", 0x73 => "UA", _ => $"U 0x{ctrl:X2}" };
            Console.WriteLine($"[rx-raw] a=0x{(body.Length > 0 ? body[0] : 0):X2} {kind} [{Convert.ToHexString(body)}]");
        };

        adapter.Initiate();
        Console.WriteLine("[runner] SABM sent; pumping seam link (LAPB + codec + XmsgLayer)...");
        // NO periodic keepalive — matches the VALIDATED legacy runner: after SABM a healthy link
        // stays RUN silently (no 4-byte frames until hangup); we send RR only reactively when 100
        // sends an I-frame (the LAPB link emits the RR itself on receive). Passing a keepalive here
        // floods the link with one RR per idle second, which the proven path never did.
        await adapter.RunAsync(token, keepaliveInterval: null);
    }

    /// <summary>
    /// The original LiveNode + XmsgNode composition, retained until the seam path is validated live.
    /// </summary>
    private static async Task RunLegacyAsync(TcpBridgeTransport transport, ushort node, CancellationToken token)
    {
        LapbLink link = new LapbLink(node);
        XmsgNode xnode = new XmsgNode(node, 0x00);
        xnode.AcknowledgeData = false;

        List<RoutingTableEntry> entries = new List<RoutingTableEntry>
        {
            new RoutingTableEntry(100, XroutConnectionType.Neighbour, 1, 1, 0),
            new RoutingTableEntry(102, XroutConnectionType.Via, 100, 2, 0),
            new RoutingTableEntry(node, XroutConnectionType.Local, node, 0, 0),
        };
        xnode.RoutingTable = new InMemoryRoutingTable(entries);
        xnode.TadResponder = new NDInsight.Sintran.Xmsg.Live.Tad.TadTerminalResponder(node, () => DateTime.Now);
        xnode.AcknowledgeTadFrames = true;

        link.OnTransmit += body =>
        {
            byte ctrl = body.Length > 1 ? body[1] : (byte)0;
            string kind;
            if ((ctrl & 1) == 0) kind = $"I ns={(ctrl >> 1) & 7} nr={(ctrl >> 5) & 7}";
            else if ((ctrl & 3) == 1) kind = $"RR nr={(ctrl >> 5) & 7}";
            else kind = ctrl switch { 0x3F => "SABM", 0x73 => "UA", _ => $"U 0x{ctrl:X2}" };
            string extra = (ctrl & 1) == 0 ? $" body={Convert.ToHexString(body)}" : string.Empty;
            Console.WriteLine($"[TX] a=0x{(body.Length > 0 ? body[0] : 0):X2} {kind} state={link.State}{extra}");
        };

        LiveNode live = new LiveNode(transport, link, xnode);
        link.Connect(0);
        Console.WriteLine("[runner] SABM sent; pumping legacy link...");
        // No periodic keepalive (matches the original validated runner) — reactive RR only.
        await live.RunAsync(token);
    }
}
