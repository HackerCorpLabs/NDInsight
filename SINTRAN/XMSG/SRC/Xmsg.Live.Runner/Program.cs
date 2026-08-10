// Live runner: connects the C# XMSG stack to a real nd100x --hdlc TCP bridge, brings up the
// LAPB link, answers reachability / list-route, and (with a TAD responder) accepts connect-to.
//
// Two composition paths:
//   * SEAM (default): TcpBridgeTransport -> LapbLayerAdapter(ILink) -> ProtocolDetector ->
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
using NDInsight.Sintran.Xmsg.Live;          // LapbLayer, TcpBridgeTransport, LiveNode (replaceable half)
using NDInsight.Sintran.Xmsg.Live.Logging;  // RotatingFileWriter, TeeTextWriter, LogLevel
using NDInsight.Sintran.Xmsg.Live.Runner;   // TopologyConfig / TopologyNode / LogConfig (this project)
using NDInsight.Sintran.Xmsg.Live.Seam;     // LapbLayerAdapter (HDLC) + EthernetLink (COSMOS segment)
using NDInsight.Sintran.Xmsg.Ethernet;      // IEthernetBackend, EthernetBackendFactory, NdMacAddress
using NDInsight.Sintran.Xmsg.Ndfs;          // FolderFileStore (a Windows folder as an ND file store)
using NDInsight.Sintran.Xmsg.Servers.Fa;    // FaServer (*FA-SERVER, the COSMOS file server)
using NDInsight.Sintran.Xmsg.Servers.Tad;   // TadServer, TadUser, TadUserDirectory
using NDInsight.Sintran.Xmsg.Node;          // XmsgNode, FileResponderSequenceStore (portable half)
using NDInsight.Sintran.Xmsg.Node.Services; // IXmsgServer (the registered-server contract)
using NDInsight.Sintran.Xmsg.Node.Seam;     // ILink, XmsgLayer, LinkXmsgTransport, BoundProtocolDetector
using NDInsight.Sintran.Xmsg.Packet;

// Wraps a TextWriter so every emitted line begins with a full wall-clock timestamp
// (yyyy-MM-dd HH:mm:ss.fff) FOLLOWED BY " | " — the pipe is an explicit, machine-parseable
// delimiter so an LLM (or a script) can split "timestamp | message" on the first " | " with
// zero ambiguity. The date+seconds+ms let the frame log show exact send/receive ordering,
// essential for diagnosing LAPB N(S)/N(R) / retransmit timing.
internal sealed class TimestampWriter : System.IO.TextWriter
{
    private readonly System.IO.TextWriter _inner;
    private bool _atLineStart = true;

    // The delimiter that separates the timestamp from the message. First occurrence per line
    // marks where the date ends — split on " | " (with the surrounding spaces) to be safe even
    // if a message body itself contains a bare '|'.
    private const string Delimiter = " | ";

    public TimestampWriter(System.IO.TextWriter inner) { _inner = inner; }

    public override System.Text.Encoding Encoding { get { return _inner.Encoding; } }

    public override void Write(char value)
    {
        if (_atLineStart)
        {
            // Full date + time down to milliseconds, then the explicit delimiter.
            _inner.Write(DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss.fff"));
            _inner.Write(Delimiter);
            _atLineStart = false;
        }

        _inner.Write(value);
        if (value == '\n')
        {
            _atLineStart = true;
        }
    }

    public override void Write(string? value)
    {
        if (value == null)
        {
            return;
        }

        for (int i = 0; i < value.Length; i++)
        {
            Write(value[i]);
        }
    }
}

internal static class Program
{
    private static async Task<int> Main(string[] args)
    {
        // Pull the named options (--config, --self) out of the vector before positional parsing so the
        // existing "[legacy] host port node seconds" positions are unchanged.
        List<string> argList = new List<string>(args);
        string? configPath = TakeOption(argList, "--config");
        string? selfArg = TakeOption(argList, "--self");

        // The relay options come out HERE too, with the others. Taking them later left them in the
        // vector during positional parsing, and "--relay-inbound-node" was read as the node number.
        string? relayListen = TakeOption(argList, "--relay-listen");
        string? relayInboundPeer = TakeOption(argList, "--relay-inbound-node");

        // PUSH: send a local file to a remote *FA-SERVER once the link is up. This is the first
        // thing the runner ORIGINATES rather than answers, so it is off unless asked for, and it
        // comes out of the vector here with the other options - leaving one behind meant
        // "--relay-inbound-node" was once read as the node number.
        // PER-FRAME TRACING IS OFF BY DEFAULT, and that is a correctness matter rather than a
        // tidiness one.
        //
        // MEASURED 2026-08-10. The segment sniffer and the transmit logger both sit on the hot
        // frame path and, for every frame addressed to us, wrote three lines including TWO full
        // hex dumps of the whole frame - synchronously, timestamped, tee'd to a rotating file.
        // Our acknowledgements then took a median of 1.77 s (p90 7.6 s, worst 8.6 s) while D100's
        // retransmit timer is 0.19 s. It resent every unacknowledged frame roughly nine times over
        // before our answer appeared, gave up, and dropped the LINK - which is the link flap that
        // was mistaken for a protocol fault for several rounds.
        //
        // Turn it on with --trace-frames when a capture is not available. Prefer a real capture.
        bool traceFrames = TakeFlag(argList, "--trace-frames");

        string? pushFile = TakeOption(argList, "--push");
        string? pushAs = TakeOption(argList, "--push-as");
        string? pushToNode = TakeOption(argList, "--push-to");

        // The other direction. --pull names the file ON THE MACHINE and --pull-to where to put it
        // here; --pull-from picks the node, the same way --push-to does.
        string? pullSpec = TakeOption(argList, "--pull");
        string? pullTo = TakeOption(argList, "--pull-to");
        string? pullFromNode = TakeOption(argList, "--pull-from");

        // Resolve and load the topology: --config wins, else the topology.json shipped next to the exe.
        System.IO.TextWriter realConsole = Console.Out;
        string resolvedConfigPath = configPath
            ?? System.IO.Path.Combine(AppContext.BaseDirectory, "topology.json");
        TopologyConfig? topology = TryLoadTopology(resolvedConfigPath, realConsole);

        // Set up logging: the console, optionally tee'd to a rotating file, all timestamped. The file is
        // rotated once here (fresh empty log per run) and again by size while running.
        System.IO.TextWriter sink = realConsole;
        RotatingFileWriter? fileWriter = TryCreateLogWriter(topology, realConsole);
        if (fileWriter != null)
        {
            sink = new TeeTextWriter(realConsole, fileWriter);
        }

        // Timestamp every line so the frame log shows exact ordering (LAPB timing). Both the console and
        // the file receive the same timestamped stream.
        Console.SetOut(new TimestampWriter(sink));

        // Startup banner: version + build time, and the full folders the config and log live in. This
        // lands in the log file too (it is printed after logging is wired), so a captured log records
        // exactly which build produced it and where its inputs/outputs are.
        PrintStartupBanner(resolvedConfigPath, fileWriter);

        // Leading "client" keyword runs the CONNECT-TO CLIENT (asker) instead of the responder:
        //   Xmsg.Live.Runner client [host] [port] [ownNode] [targetName] [hostNode]
        // It brings up LAPB, sends a connect-to letter naming <targetName>, drives the TAD asker
        // state machine, renders the host's terminal output and sends what you type on stdin.
        if (argList.Count > 0 && string.Equals(argList[0], "client", StringComparison.OrdinalIgnoreCase))
        {
            return await RunClientModeAsync(argList, topology);
        }

        // Optional leading "legacy" keyword selects the old LiveNode + XmsgNode path.
        bool legacy = argList.Count > 0 && string.Equals(argList[0], "legacy", StringComparison.OrdinalIgnoreCase);
        int argOffset = legacy ? 1 : 0;

        // Effective self node: --self (id or alias) > positional node arg > topology.self > 103.
        ushort node = ResolveSelf(topology, selfArg, argList, argOffset);

        // Endpoint: positional host/port override the neighbour's TCP endpoint from the topology.
        TopologyEndpoint? endpoint = topology?.PrimaryEndpoint();
        string host = argList.Count > argOffset ? argList[argOffset] : (endpoint?.Host ?? "127.0.0.1");
        int port = argList.Count > argOffset + 1 ? int.Parse(argList[argOffset + 1]) : (endpoint?.Port ?? 10364);
        // Session duration in seconds. 0 (or negative) = run until the process is stopped (Ctrl-C),
        // so an interactive terminal session is not cut off by a timeout. Default 3600 (1 hour).
        int seconds = argList.Count > argOffset + 3 ? int.Parse(argList[argOffset + 3]) : 3600;

        // The routing table comes from the topology (self-consistent for whichever node we run as); the
        // built-in fallback keeps the old behaviour when no topology file is present.
        List<RoutingTableEntry> routingEntries = topology != null
            ? topology.BuildRoutingEntries(node)
            : BuildRoutingEntries(node);

        Console.WriteLine($"[runner] path={(legacy ? "legacy" : "seam")} connecting to {host}:{port} as node {node} for {(seconds > 0 ? seconds + "s" : "unlimited (Ctrl-C to stop)")}");

        // seconds <= 0 -> no timeout (run until the process is stopped); otherwise cancel after the window.
        using CancellationTokenSource cts = seconds > 0
            ? new CancellationTokenSource(TimeSpan.FromSeconds(seconds))
            : new CancellationTokenSource();

        // The COSMOS file server, when a folder is configured. Built ONCE here and handed to
        // whichever transport path runs, so the two paths cannot drift into serving different things.
        FaServer? fileServer = TryCreateFileServer(topology);

        // The file push, when one was asked for. Built here - before any transport is chosen - so a
        // bad filespec or a missing file is reported immediately instead of after a link comes up.
        FaPushRun? pushRun = TryCreatePush(topology, node, pushFile, pushAs, pushToNode);
        if (pushFile != null && pushRun == null)
        {
            return 1;
        }

        // The pull, same reasoning: a bad filespec is reported now, not after a link comes up.
        FaPullRun? pullRun = TryCreatePull(topology, node, pullSpec, pullTo, pullFromNode);
        if (pullSpec != null && pullRun == null)
        {
            return 1;
        }

        // ONE TRANSFER AT A TIME. Both drivers originate their own conversation and both would
        // pump on the same tick, and nothing has ever been captured with two client conversations
        // running at once against one server. Refused rather than tried - a half-understood
        // interleaving that mostly works is the worst outcome, because it fails rarely and looks
        // like a protocol problem when it does.
        if (pushRun != null && pullRun != null)
        {
            Console.WriteLine(
                "[runner] --push and --pull cannot run together. Run one, then the other.");
            return 1;
        }

        // RELAY PATH: two HDLC links, so this node sits BETWEEN two machines instead of hanging off
        // one. Selected by --relay-listen; nothing else reaches it, so every existing path is
        // untouched.
        if (relayListen != null)
        {
            int listenPort;
            if (!int.TryParse(relayListen, out listenPort))
            {
                Console.WriteLine($"[runner] --relay-listen needs a port number, got '{relayListen}'");
                return 1;
            }

            // Which node is on each link cannot be inferred: the inbound peer DIALS us, so nothing
            // in our own configuration names it until it speaks. Stated explicitly rather than
            // guessed, because getting it wrong silently misroutes rather than failing.
            ushort inboundPeer;
            if (relayInboundPeer == null || !ushort.TryParse(relayInboundPeer, out inboundPeer))
            {
                Console.WriteLine("[runner] --relay-listen also needs --relay-inbound-node <nodeNumber>");
                return 1;
            }

            ushort outboundPeer = topology?.NeighbourId() ?? 100;

            // Not wired on the relay path either - a relay has two links and nothing here says
            // which one the file server is on. Say so instead of pushing down the wrong one.
            if (pushRun != null)
            {
                Console.WriteLine(
                    "[push] --push is not wired to the relay path yet (two links, and nothing "
                    + "names which one the file server is on). Nothing will be pushed on this run.");
            }

            if (pullRun != null)
            {
                Console.WriteLine(
                    "[pull] --pull is not wired to the relay path either, and for the same reason. "
                    + "Nothing will be read on this run.");
            }


            try
            {
                await RunRelayAsync(
                    topology, listenPort, host, port, node, inboundPeer, outboundPeer, routingEntries,
                    topology?.Motd, BuildTadUsers(topology), fileServer, cts.Token);
            }
            catch (OperationCanceledException)
            {
                Console.WriteLine("[runner] time window elapsed; stopping.");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[runner] relay error: {ex.Message}");
            }

            Console.WriteLine("[runner] done.");
            return 0;
        }

        // ETHERNET PATH: when the topology says our neighbour is on a COSMOS segment there is no
        // HDLC bridge to dial at all - we join the segment instead. Everything above the link is
        // the same XmsgNodeHost the HDLC path uses.
        TopologyNode? ethernetPeer = topology?.PrimaryEthernetPeer();
        if (ethernetPeer != null && !legacy)
        {
            // The Ethernet link cannot address a peer out of nothing: a data frame carries the
            // peer's link id in its RECEIVER field, and that id is learned from the peer, never
            // derived. So the push waits for the far machine to send us something first - one
            // command typed at its console is enough - and only then starts the ladder.
            if (pushRun != null)
            {
                Console.WriteLine(
                    "[push] waiting for node " + ethernetPeer.Id + " to address us first. We cannot "
                    + "derive its link id; make it talk to us once (any command on its console that "
                    + "reaches this node) and the push starts by itself.");
            }

            if (pullRun != null)
            {
                Console.WriteLine(
                    "[pull] waiting for node " + ethernetPeer.Id + " to address us first. We cannot "
                    + "derive its link id; make it talk to us once (any command on its console that "
                    + "reaches this node) and the pull starts by itself.");
            }

            try
            {
                await RunEthernetSeamAsync(
                    topology!, ethernetPeer, node, routingEntries,
                    topology!.Motd, BuildTadUsers(topology), fileServer, pushRun, pullRun, traceFrames,
                    cts.Token);
            }
            catch (OperationCanceledException)
            {
                Console.WriteLine("[runner] time window elapsed; stopping.");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[runner] pump error: {ex.Message}");
            }

            Console.WriteLine("[runner] done.");
            return 0;
        }

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
                await RunLegacyAsync(transport, node, routingEntries, cts.Token);
            }
            else
            {
                await RunSeamAsync(transport, host, port, node, routingEntries, topology?.Motd, BuildTadUsers(topology), fileServer, pushRun, pullRun, cts.Token);
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
    /// Builds the file push when <c>--push</c> was given, or reports why it cannot be built.
    /// </summary>
    /// <param name="topology">
    /// The loaded topology, used for our own system name and to default the server node.
    /// </param>
    /// <param name="node">
    /// The system number this runner presents on the wire.
    /// </param>
    /// <param name="pushFile">
    /// The local file to send, or null when no push was asked for.
    /// </param>
    /// <param name="pushAs">
    /// The name the file takes on the machine, for example <c>PUSHED:DATA</c>. Defaults to the
    /// local file's own name converted to SINTRAN form.
    /// </param>
    /// <param name="pushToNode">
    /// The node running <c>*FA-SERVER</c>, as a number. Defaults to the topology's neighbour.
    /// </param>
    /// <returns>
    /// The push, or null when none was asked for OR when it could not be built.
    /// </returns>
    /// <remarks>
    /// <para>
    /// The filespec is LOCAL to the server: <c>PUSHED:DATA</c>, never
    /// <c>D102(SYSTEM)."PUSHED:DATA"</c>. The conversation is already addressed to that machine and
    /// the user travels separately in the reserve request, so a machine-qualified name is
    /// command-line syntax that has no place on the FA wire. It is also too long for the field.
    /// </para>
    /// <para>
    /// Everything is checked HERE, before a link is brought up, because the alternative is a
    /// twenty-second wait followed by an argument error.
    /// </para>
    /// </remarks>
    private static FaPushRun? TryCreatePush(
        TopologyConfig? topology, ushort node, string? pushFile, string? pushAs, string? pushToNode)
    {
        if (pushFile == null)
        {
            return null;
        }

        if (!System.IO.File.Exists(pushFile))
        {
            Console.WriteLine($"[push] there is no file at '{pushFile}'");
            return null;
        }

        // Which machine runs the file server. The topology's neighbour is the sensible default -
        // it is the only node we have a link to - but it is stated in the log either way, because
        // pushing to the wrong machine writes a real file in a real user's directory.
        ushort serverNode;
        if (pushToNode != null)
        {
            if (!ushort.TryParse(pushToNode, out serverNode))
            {
                Console.WriteLine($"[push] --push-to needs a node number, got '{pushToNode}'");
                return null;
            }
        }
        else
        {
            serverNode = topology?.NeighbourId() ?? 100;
        }

        // The connect letter names the machine we are asking, NOT ourselves - every capture we
        // hold agrees, and naming ourselves is what D100 answered with a network error. Taken
        // from the topology's alias for that node when it has one.
        TopologyNode? server = topology?.FindById(serverNode);
        string serverName = (server != null && !string.IsNullOrEmpty(server.Alias))
            ? server.Alias!.ToUpperInvariant()
            : "D" + serverNode;

        // Our own name is still worth printing: the remote must have been told about it with
        // DEF-REMOTE or it cannot answer us at all, whatever the letter says.
        TopologyNode? self = topology?.FindById(node);
        string ourName = (self != null && !string.IsNullOrEmpty(self.Alias))
            ? self.Alias!.ToUpperInvariant()
            : "D" + node;

        // The name on the machine. Given explicitly, or taken from the local file with the type
        // separator swapped from a dot to a colon.
        string fileSpec;
        if (pushAs != null)
        {
            fileSpec = pushAs.ToUpperInvariant();
        }
        else
        {
            string leaf = System.IO.Path.GetFileName(pushFile);
            string name;
            string type;
            string problem;
            if (!NDInsight.Sintran.Xmsg.Sync.SintranFileName.TryConvert(
                    leaf, out name, out type, out problem))
            {
                Console.WriteLine($"[push] '{leaf}' cannot be a SINTRAN name: {problem}");
                Console.WriteLine("[push] give one with --push-as, for example --push-as PUSHED:DATA");
                return null;
            }

            fileSpec = NDInsight.Sintran.Xmsg.Sync.SintranFileName.ToFileSpec(name, type);
        }

        byte[] content;
        try
        {
            content = System.IO.File.ReadAllBytes(pushFile);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"[push] cannot read '{pushFile}': {ex.Message}");
            return null;
        }

        // The file is being CREATED, so the name is quoted. Whether a quoted name is what makes
        // the FA server create a file is UNVERIFIED on the wire - it is the command-line rule - and
        // this run is partly there to find out.
        string quoted = "\"" + fileSpec + "\"";

        try
        {
            FaWriteTarget target = new FaWriteTarget(serverNode, serverName, quoted);
            Console.WriteLine(
                $"[push] {pushFile} -> node {serverNode} ({serverName}) user {target.User} " +
                $"as {quoted} (we are {ourName})");
            return new FaPushRun(pushFile, content, target);
        }
        catch (ArgumentException ex)
        {
            Console.WriteLine($"[push] {ex.Message}");
            return null;
        }
    }

    /// <summary>
    /// Builds the file pull when <c>--pull</c> was given, or reports why it cannot be built.
    /// </summary>
    /// <param name="topology">
    /// The loaded topology, or null when there is none.
    /// </param>
    /// <param name="node">
    /// Our own node number, for the log.
    /// </param>
    /// <param name="pullSpec">
    /// The file to read AS THE REMOTE MACHINE NAMES IT, or null when no pull was asked for.
    /// </param>
    /// <param name="pullTo">
    /// Where to write it locally, or null to use the filespec with the colon turned into a dot.
    /// </param>
    /// <param name="pullFromNode">
    /// The node to read from, or null to use the topology's neighbour.
    /// </param>
    /// <returns>
    /// The pull, or null when none was asked for OR when it could not be built.
    /// </returns>
    /// <remarks>
    /// <para><b>The name is NOT quoted, unlike a push</b></para>
    /// <para>
    /// A push creates a file, and SINTRAN's rule is that quotes go around a name being created. A
    /// pull opens one that exists, and the captured reader sends the name bare. That also buys two
    /// characters: a read tolerates thirteen where a write allows eleven.
    /// </para>
    /// </remarks>
    private static FaPullRun? TryCreatePull(
        TopologyConfig? topology, ushort node, string? pullSpec, string? pullTo, string? pullFromNode)
    {
        if (pullSpec == null)
        {
            return null;
        }

        // Which machine to read from. Same default and the same reason as the push: the topology's
        // neighbour is the only node we have a link to, and it is stated in the log either way.
        ushort serverNode;
        if (pullFromNode != null)
        {
            if (!ushort.TryParse(pullFromNode, out serverNode))
            {
                Console.WriteLine($"[pull] --pull-from needs a node number, got '{pullFromNode}'");
                return null;
            }
        }
        else
        {
            serverNode = topology?.NeighbourId() ?? 100;
        }

        // The connect letter names the machine we are asking, NOT ourselves.
        TopologyNode? server = topology?.FindById(serverNode);
        string serverName = (server != null && !string.IsNullOrEmpty(server.Alias))
            ? server.Alias!.ToUpperInvariant()
            : "D" + serverNode;

        TopologyNode? self = topology?.FindById(node);
        string ourName = (self != null && !string.IsNullOrEmpty(self.Alias))
            ? self.Alias!.ToUpperInvariant()
            : "D" + node;

        string fileSpec = pullSpec.ToUpperInvariant();

        // Where it lands here. The filespec with the type separator swapped back from a colon to a
        // dot, which is the inverse of what the push does to a local name.
        string localPath = pullTo ?? fileSpec.Replace(':', '.');

        // REFUSED RATHER THAN OVERWRITTEN. A pull that quietly replaced a local file would be a
        // destructive default, and the whole point of pulling is usually to compare the result
        // against what is already there.
        if (System.IO.File.Exists(localPath))
        {
            Console.WriteLine($"[pull] '{localPath}' already exists; move it or give --pull-to");
            return null;
        }

        try
        {
            FaReadSource source = new FaReadSource(serverNode, serverName, fileSpec);
            Console.WriteLine(
                $"[pull] {fileSpec} on node {serverNode} ({serverName}) user {source.User} " +
                $"-> {localPath} (we are {ourName})");
            return new FaPullRun(localPath, source);
        }
        catch (ArgumentException ex)
        {
            Console.WriteLine($"[pull] {ex.Message}");
            return null;
        }
    }

    /// <summary>
    /// Removes a named <c>--option value</c> pair from the argument list and returns its value.
    /// </summary>
    /// <param name="args">
    /// The mutable argument list; the option name and its value are removed in place when found.
    /// </param>
    /// <param name="name">
    /// The option name to look for (for example <c>--config</c>), matched case-insensitively.
    /// </param>
    /// <returns>
    /// The value following the option, or <c>null</c> when the option is absent.
    /// </returns>
    /// <summary>
    /// Takes a valueless flag out of the argument vector.
    /// </summary>
    /// <param name="args">
    /// The remaining arguments; the flag is removed when found.
    /// </param>
    /// <param name="name">
    /// The flag, for example <c>--trace-frames</c>.
    /// </param>
    /// <returns>
    /// <see langword="true"/> when the flag was present.
    /// </returns>
    private static bool TakeFlag(List<string> args, string name)
    {
        for (int i = 0; i < args.Count; i++)
        {
            if (string.Equals(args[i], name, StringComparison.OrdinalIgnoreCase))
            {
                args.RemoveAt(i);
                return true;
            }
        }

        return false;
    }

    private static string? TakeOption(List<string> args, string name)
    {
        for (int i = 0; i < args.Count; i++)
        {
            if (string.Equals(args[i], name, StringComparison.OrdinalIgnoreCase))
            {
                string? value = (i + 1 < args.Count) ? args[i + 1] : null;
                args.RemoveAt(i);              // remove the option name
                if (value != null)
                {
                    args.RemoveAt(i);          // remove the value that followed it
                }

                return value;
            }
        }

        return null;
    }

    /// <summary>
    /// Prints the startup banner: the runner version and build time, plus the full folders (and paths)
    /// of the config file and the log file.
    /// </summary>
    /// <param name="configPath">
    /// The resolved topology config file path (its folder is reported).
    /// </param>
    /// <param name="fileWriter">
    /// The active log writer, or <c>null</c> when file logging is off (then the log folder is reported as off).
    /// </param>
    /// <summary>
    /// How many FA connection numbers each run reserves up front.
    /// </summary>
    /// <remarks>
    /// Reserved in BLOCKS rather than saved per connection, so a runner that is killed - which is
    /// how it usually ends - still never hands out a number twice.
    /// </remarks>
    private const ushort ConnectionNumberBlock = 64;

    /// <summary>
    /// The lowest connection number handed out, and where the counter wraps back to.
    /// </summary>
    /// <remarks>
    /// Mirrors <c>FaServer</c>'s own range. Kept here too because this method decides what the
    /// NEXT process starts from, and a stale file written before the wrap existed can still hold a
    /// number from the climbing days.
    /// </remarks>
    private const ushort FirstConnectionNumber = 0x0042;

    /// <summary>
    /// The highest connection number handed out before wrapping.
    /// </summary>
    /// <remarks>
    /// Every connection number a real machine has sent us is small - 0x0004 through 0x0046 - and a
    /// climbed 0x0E02 was silently ignored by a live D100. Where the true limit sits is NOT known;
    /// this keeps us in the captures' order of magnitude. See <c>FaServer.LastConnectionNumber</c>.
    /// </remarks>
    private const ushort LastConnectionNumber = 0x00FF;

    /// <summary>
    /// Takes the next block of FA connection numbers, so a restart never reuses one.
    /// </summary>
    /// <returns>
    /// The number the file server should answer its next connect letter with.
    /// </returns>
    /// <remarks>
    /// <para><b>Why this exists</b></para>
    /// The connection number counts up per CONNECTION and carries across conversations - its scope
    /// is the server's lifetime, unlike the two per-conversation counters, which reset with the
    /// conversation. A real server stays up; ours restarts constantly and began at <c>0x0042</c>
    /// every time, handing back numbers the peer had already seen. Persisting it is what the
    /// observed semantics ask for. See <c>FaServer.NextConnectionNumber</c>.
    /// <para><b>It does NOT fix the first-connect stall - that theory was DISPROVED</b></para>
    /// The stall was the reason this was written: after a runner restart the first connect letter
    /// gets its confirmation and then nothing follows, and a retry minutes later works. Reusing a
    /// connection number looked like the cause, because the retry that worked had used the NEXT
    /// number and because restarting XMSG on D100 also cured it.
    /// <para>
    /// Tested 2026-08-06: with the state file in place a restart answered the first connect with a
    /// fresh <c>0x0082</c> and it stalled anyway. So the cause is something else that resets when
    /// this process does - the session wire port, which also restarts at <c>0x0211</c> every run, is
    /// the next candidate and is UNTESTED. Do not repeat the connection-number theory.
    /// </para>
    /// <para><b>Block reservation, not save-on-use</b></para>
    /// The file records where the NEXT run must start. Reserving
    /// <see cref="ConnectionNumberBlock"/> up front lets a run hand out that many without touching
    /// the file again, and being killed loses nothing - the worst case is skipping some numbers,
    /// which costs nothing.
    /// </remarks>
    private static ushort ReserveConnectionNumbers()
    {
        string path = System.IO.Path.Combine(AppContext.BaseDirectory, "fa-connection.state");

        ushort start = FirstConnectionNumber;
        try
        {
            if (System.IO.File.Exists(path))
            {
                string text = System.IO.File.ReadAllText(path).Trim();
                ushort saved;
                if (ushort.TryParse(text, out saved) && saved != 0)
                {
                    start = saved;
                }
            }

            // WRAP, do not climb. This used to be a plain add, so every process start pushed the
            // number 64 higher for ever and it reached 0x0E02 - far outside anything a real machine
            // sends. A live D100 then ignored our connect confirmation entirely: no error, no
            // reject, it just re-sent its letter until the terminal timed out. Measured 2026-08-09.
            ushort next = (ushort)(start + ConnectionNumberBlock);
            if (start < FirstConnectionNumber || start > LastConnectionNumber
                || next > LastConnectionNumber)
            {
                // Either the file is stale from the climbing days, or this block runs off the end.
                // Either way the next run starts over rather than leaving the range.
                if (start < FirstConnectionNumber || start > LastConnectionNumber)
                {
                    start = FirstConnectionNumber;
                }

                next = FirstConnectionNumber;
            }

            System.IO.File.WriteAllText(path, next.ToString());
        }
        catch (System.IO.IOException error)
        {
            // A state file we cannot read or write is not worth failing the run for - the cost is
            // one stalled connect, which the client retries past.
            Console.WriteLine($"[fa] connection-number state file unusable ({error.Message}); starting at 0x{start:X4}");
        }

        Console.WriteLine(
            $"[fa] connection numbers 0x{start:X4}..0x{(ushort)(start + ConnectionNumberBlock - 1):X4} (state: {path})");
        return start;
    }

    private static void PrintStartupBanner(string configPath, RotatingFileWriter? fileWriter)
    {
        System.Reflection.Assembly asm = System.Reflection.Assembly.GetExecutingAssembly();
        Version? version = asm.GetName().Version;

        // Build time = the assembly file's last-write time; empty Location (single-file publish) -> unknown.
        string location = asm.Location;
        string built = !string.IsNullOrEmpty(location) && System.IO.File.Exists(location)
            ? System.IO.File.GetLastWriteTime(location).ToString("yyyy-MM-dd HH:mm:ss")
            : "unknown";

        string configFolder = System.IO.Path.GetDirectoryName(configPath) ?? configPath;
        string logFolder = fileWriter != null
            ? (System.IO.Path.GetDirectoryName(fileWriter.FilePath) ?? fileWriter.FilePath)
            : "(file logging off)";
        string logFile = fileWriter != null ? fileWriter.FilePath : "(file logging off)";

        Console.WriteLine($"[runner] Xmsg.Live.Runner v{version} built {built}");
        Console.WriteLine($"[runner] config folder: {configFolder}");
        Console.WriteLine($"[runner] config file:   {configPath}");
        Console.WriteLine($"[runner] log folder:    {logFolder}");
        Console.WriteLine($"[runner] log file:      {logFile}");
    }

    /// <summary>
    /// Loads the topology file when present, returning <c>null</c> (with a console note) on any problem so
    /// the runner falls back to built-in defaults rather than failing to start.
    /// </summary>
    /// <param name="path">
    /// The resolved topology file path.
    /// </param>
    /// <param name="console">
    /// The console writer used for the load note (logging is not set up yet at this point).
    /// </param>
    /// <returns>
    /// The parsed topology, or <c>null</c> when the file is missing or invalid.
    /// </returns>
    private static TopologyConfig? TryLoadTopology(string path, System.IO.TextWriter console)
    {
        try
        {
            if (!System.IO.File.Exists(path))
            {
                console.WriteLine($"[runner] no topology file at {path}; using built-in defaults.");
                return null;
            }

            TopologyConfig config = TopologyConfig.Load(path);
            console.WriteLine($"[runner] topology loaded: self={config.Self}, {config.Nodes.Count} node(s), file={path}");
            return config;
        }
        catch (Exception ex)
        {
            console.WriteLine($"[runner] topology load failed ({ex.Message}); using built-in defaults.");
            return null;
        }
    }

    /// <summary>
    /// Builds the TAD login directory from the topology's <c>tadUsers</c> section.
    /// </summary>
    /// <remarks>
    /// A user whose password is empty or missing is passwordless (login skips the password prompt). When
    /// the section is absent or empty the directory falls back to a single <c>SYSTEM</c>/<c>SYSTEM</c>
    /// account so the server always has a login. Entries with a blank username are skipped.
    /// </remarks>
    /// <param name="topology">
    /// The loaded topology (may be <c>null</c>).
    /// </param>
    /// <returns>
    /// The TAD user directory.
    /// </returns>
    private static TadUserDirectory BuildTadUsers(TopologyConfig? topology)
    {
        if (topology == null || topology.TadUsers == null || topology.TadUsers.Count == 0)
        {
            return new TadUserDirectory();   // default SYSTEM/SYSTEM
        }

        List<TadUser> users = new List<TadUser>(topology.TadUsers.Count);
        for (int i = 0; i < topology.TadUsers.Count; i++)
        {
            TadUserConfig config = topology.TadUsers[i];
            if (string.IsNullOrWhiteSpace(config.Username))
            {
                continue;
            }

            users.Add(new TadUser(config.Username.Trim(), config.Password));
        }

        return users.Count != 0 ? new TadUserDirectory(users) : new TadUserDirectory();
    }

    /// <summary>
    /// Creates the COSMOS file server (<c>*FA-SERVER</c>) over the configured Windows folder.
    /// </summary>
    /// <param name="topology">
    /// The loaded topology (may be <c>null</c>).
    /// </param>
    /// <returns>
    /// The file server, or <c>null</c> when no folder is configured, the block is disabled, or the
    /// folder cannot be opened.
    /// </returns>
    /// <remarks>
    /// <b>No folder configured means NO file server</b>, and a line in the log saying so. Falling
    /// back to a folder next to the executable would share the runner's own logs, state files and
    /// DLLs with any machine that asked, which nobody chose. Sharing files is an explicit act.
    /// </remarks>
    private static FaServer? TryCreateFileServer(TopologyConfig? topology)
    {
        FileServerConfig? config = topology?.FileServer;
        if (config == null || string.IsNullOrWhiteSpace(config.Root))
        {
            Console.WriteLine(
                "[fa] file server OFF: no \"fileServer\": { \"root\": \"...\" } in the topology file. "
                + "Nothing is served. Set a folder to turn it on.");
            return null;
        }

        if (!config.Enabled)
        {
            Console.WriteLine($"[fa] file server OFF: \"enabled\" is false (the configured folder was {config.Root}).");
            return null;
        }

        // A relative folder lands next to the executable; an absolute path is honoured as-is - the
        // same rule the log file uses, so one convention covers both.
        string root = System.IO.Path.IsPathRooted(config.Root)
            ? config.Root
            : System.IO.Path.Combine(AppContext.BaseDirectory, config.Root);

        try
        {
            FolderFileStore store = new FolderFileStore(root);
            FaServer server = new FaServer(store, config.UserIndex);
            server.NextConnectionNumber = ReserveConnectionNumbers();
            server.Log += line => Console.WriteLine(line);
            Console.WriteLine($"[fa] file server ON as {FaServer.ServerName} (logical port {FaServer.ServerLogicalPort}, wire port 0x{FaServer.FaServerWirePort:X4})");
            Console.WriteLine($"[fa] serving folder: {store.RootFolder}");
            Console.WriteLine($"[fa] files visible now: {store.ListFiles().Count}");
            return server;
        }
        catch (Exception ex)
        {
            Console.WriteLine($"[fa] file server OFF: folder '{root}' could not be opened ({ex.Message}).");
            return null;
        }
    }

    /// <summary>
    /// Builds the server list handed to the node host: the TAD server always, plus the file server
    /// when one is configured.
    /// </summary>
    /// <param name="tadServer">
    /// The TAD terminal server.
    /// </param>
    /// <param name="fileServer">
    /// The COSMOS file server, or <c>null</c> when it is off.
    /// </param>
    /// <returns>
    /// The servers to register, in order.
    /// </returns>
    private static IXmsgServer[] BuildServerList(TadServer tadServer, FaServer? fileServer)
    {
        if (fileServer == null)
        {
            return new IXmsgServer[] { tadServer };
        }

        return new IXmsgServer[] { tadServer, fileServer };
    }

    /// <summary>
    /// Builds the two node hosts a relay runs on - one per link - with the SAME servers registered
    /// on both.
    /// </summary>
    /// <param name="inbound">
    /// The link the relay listens on.
    /// </param>
    /// <param name="outbound">
    /// The link the relay dials out on.
    /// </param>
    /// <param name="node">
    /// Our own node number.
    /// </param>
    /// <param name="routingEntries">
    /// The routing table both hosts answer reachability questions from.
    /// </param>
    /// <param name="sequenceStore">
    /// The persisted responder sequence, shared by both hosts as it always has been.
    /// </param>
    /// <param name="servers">
    /// The servers to register on both hosts.
    /// </param>
    /// <returns>
    /// Two hosts: the inbound one at index 0, the outbound one at index 1.
    /// </returns>
    /// <remarks>
    /// <para><b>Why this is a method and not two lines at the call site</b></para>
    /// <para>
    /// Because the two lines disagreed, and nothing noticed for as long as the relay was only ever
    /// asked to FORWARD. The outbound host was built with no server list at all, so a datagram a
    /// peer on that link addressed TO US - as opposed to asking us to pass it on - reached
    /// <c>XmsgServerHost.Route</c>, found no server to hand it to, and produced nothing.
    /// </para>
    /// <para>
    /// What that looked like from the outside, on 2026-08-08: typing
    /// <c>LI-FI D19999(SYSTEM).,,</c> on D100 hung the terminal until SINTRAN gave up with
    /// "NO ANSWER FROM REMOTE SYSTEM; FILE-ACCESS CONNECTION ABORTED". The capture
    /// (<c>DOC/captures/FA-OPERATIONS-2026-08-08</c>) shows the reachability request answered
    /// normally, then the *FA-SERVER connect letter - body <c>1B41 0014 FF0A</c> "*FA-SERVER"
    /// <c>FE06</c> "D19999" - answered with a BARE ACK and nothing else. The relay counters never
    /// move during any of it, because a datagram addressed to our own node is not transit, so the
    /// log was silent on both sides.
    /// </para>
    /// <para>
    /// ONE set of server instances, not one per link. A session is keyed by its wire port and
    /// records the node and system it belongs to, and every reply is built through whichever host
    /// routed the request - so answers still leave by the link they arrived on, and a client that
    /// reaches us over either link talks to the same served folder.
    /// </para>
    /// </remarks>
    internal static XmsgNodeHost[] BuildRelayHosts(
        ILink inbound,
        ILink outbound,
        ushort node,
        IReadOnlyList<RoutingTableEntry> routingEntries,
        IResponderSequenceStore sequenceStore,
        IXmsgServer[] servers)
    {
        return new XmsgNodeHost[]
        {
            new XmsgNodeHost(inbound, node, routingEntries, sequenceStore, servers),
            new XmsgNodeHost(outbound, node, routingEntries, sequenceStore, servers),
        };
    }

    /// <summary>
    /// Creates the rotating log-file writer from the topology's log section, or <c>null</c> when logging is
    /// disabled or the file cannot be opened.
    /// </summary>
    /// <param name="topology">
    /// The loaded topology (may be <c>null</c>, in which case the default log config is used).
    /// </param>
    /// <param name="console">
    /// The console writer used to report a log-open failure.
    /// </param>
    /// <returns>
    /// A rotating writer, or <c>null</c> when file logging is off.
    /// </returns>
    private static RotatingFileWriter? TryCreateLogWriter(TopologyConfig? topology, System.IO.TextWriter console)
    {
        LogConfig log = topology?.Log ?? new LogConfig();
        if (!log.Enabled || log.LevelKind() == LogLevel.Off)
        {
            return null;
        }

        try
        {
            // A relative file name lands next to the executable; an absolute path is honoured as-is.
            string path = System.IO.Path.IsPathRooted(log.File)
                ? log.File
                : System.IO.Path.Combine(AppContext.BaseDirectory, log.File);
            RotatingFileWriter writer = new RotatingFileWriter(path, log.MaxBytes, log.Keep);
            console.WriteLine($"[runner] logging to {path} (level {log.LevelKind()}, max {log.MaxSizeMb} MB, keep {log.Keep}).");
            return writer;
        }
        catch (Exception ex)
        {
            console.WriteLine($"[runner] file logging disabled ({ex.Message}).");
            return null;
        }
    }

    /// <summary>
    /// Resolves the effective self node: <c>--self</c> (id or alias) wins, then the positional node arg,
    /// then the topology's <c>self</c>, then the historical default of 103.
    /// </summary>
    /// <param name="topology">
    /// The loaded topology, used to resolve a <c>--self</c> alias and to supply the default self.
    /// </param>
    /// <param name="selfArg">
    /// The raw <c>--self</c> value (id or alias), or <c>null</c> when not given.
    /// </param>
    /// <param name="argList">
    /// The positional argument list (after option extraction).
    /// </param>
    /// <param name="argOffset">
    /// The index offset for the positional args (1 when the "legacy" keyword is present, else 0).
    /// </param>
    /// <returns>
    /// The node id the runner should present on the wire.
    /// </returns>
    /// <exception cref="FormatException">
    /// Thrown when <paramref name="selfArg"/> is neither a known alias/id nor a parseable number.
    /// </exception>
    private static ushort ResolveSelf(TopologyConfig? topology, string? selfArg, List<string> argList, int argOffset)
    {
        if (!string.IsNullOrWhiteSpace(selfArg))
        {
            TopologyNode? found = topology?.Find(selfArg);
            if (found != null)
            {
                return found.Id;
            }

            if (ushort.TryParse(selfArg, out ushort selfId))
            {
                return selfId;
            }

            throw new FormatException($"--self '{selfArg}' is not a known node id or alias.");
        }

        if (argList.Count > argOffset + 2)
        {
            return (ushort)int.Parse(argList[argOffset + 2]);
        }

        if (topology != null)
        {
            return topology.Self;
        }

        return 103;
    }

    /// <summary>
    /// Parses the client-mode arguments, connects the bridge, and runs the connect-to asker.
    /// </summary>
    /// <param name="args">
    /// The argument list (args[0] == "client"), after option extraction.
    /// </param>
    /// <param name="topology">
    /// The loaded topology (may be <c>null</c>), used to default the host/port and node numbers.
    /// </param>
    /// <returns>
    /// The process exit code.
    /// </returns>
    private static async Task<int> RunClientModeAsync(List<string> args, TopologyConfig? topology)
    {
        // Topology-derived defaults: dial the neighbour's bridge, present our own self node, connect to
        // the neighbour host node. Positional args still override every one of these.
        TopologyEndpoint? endpoint = topology?.PrimaryEndpoint();
        ushort defaultOwn = topology?.Self ?? 102;
        ushort defaultHostNode = topology?.NeighbourId() ?? 100;

        string host = args.Count > 1 ? args[1] : (endpoint?.Host ?? "127.0.0.1");
        int port = args.Count > 2 ? int.Parse(args[2]) : (endpoint?.Port ?? 10362);
        ushort ownNode = (ushort)(args.Count > 3 ? int.Parse(args[3]) : defaultOwn);
        string targetName = args.Count > 4 ? args[4] : "D100";
        ushort hostNode = (ushort)(args.Count > 5 ? int.Parse(args[5]) : defaultHostNode);
        // Link seed (hex). Default from the observed node pairs: 100<->102 = 0x14, 100<->103 = 0x13.
        byte seed = args.Count > 6 ? Convert.ToByte(args[6], 16) : (ownNode == 103 ? (byte)0x13 : (byte)0x14);

        Console.WriteLine($"[client] connecting bridge {host}:{port} as node {ownNode}; connect-to '{targetName}' on host node {hostNode}; seed 0x{seed:X2}");

        using CancellationTokenSource cts = new CancellationTokenSource();
        TcpBridgeTransport transport;
        try
        {
            transport = await TcpBridgeTransport.ConnectAsync(host, port, cts.Token);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"[client] connect failed: {ex.Message}");
            return 1;
        }

        try
        {
            await RunClientAsync(transport, host, port, ownNode, hostNode, targetName, seed, cts.Token);
        }
        catch (OperationCanceledException)
        {
            Console.WriteLine("[client] stopped.");
        }
        catch (Exception ex)
        {
            Console.WriteLine($"[client] error: {ex.Message}");
        }
        finally
        {
            transport.Dispose();
        }

        return 0;
    }

    /// <summary>
    /// Runs the connect-to CLIENT (asker): TcpBridgeTransport -> LapbLayerAdapter -> XmsgCodec ->
    /// TadAskerSession. Sends the connect letter when the link comes up, drives the TAD handshake,
    /// renders host terminal text, and forwards stdin lines as keystroke frames.
    /// </summary>
    /// <param name="transport">
    /// The connected bridge transport.
    /// </param>
    /// <param name="host">
    /// The bridge host (for the link id).
    /// </param>
    /// <param name="port">
    /// The bridge port (for the link id).
    /// </param>
    /// <param name="ownNode">
    /// This client's node number.
    /// </param>
    /// <param name="hostNode">
    /// The host node we connect to.
    /// </param>
    /// <param name="targetName">
    /// The remote name carried in the connect letter.
    /// </param>
    /// <param name="token">
    /// A token that stops the session.
    /// </param>
    /// <returns>
    /// A task that completes when the pump stops.
    /// </returns>
    private static async Task RunClientAsync(
        TcpBridgeTransport transport, string host, int port, ushort ownNode, ushort hostNode, string targetName, byte seed, CancellationToken token)
    {
        string linkId = $"hdlc:{host}:{port}";

        LapbLayer link = new LapbLayer(ownNode);
        LapbLayerAdapter adapter = new LapbLayerAdapter(linkId, transport, link);
        LinkXmsgTransport codecTransport = new LinkXmsgTransport(adapter);
        XmsgCodec codec = new XmsgCodec(linkId, codecTransport);

        // Persist the client's outgoing datagram sequence per host node so a restart resumes in step
        // with the host's expected-from-us (a fresh Flags1=0 is silently dropped as behind-sequence).
        string statePath = System.IO.Path.Combine(AppContext.BaseDirectory, "xmsg-client-sequence.state");
        FileResponderSequenceStore sequenceStore = new FileResponderSequenceStore(statePath);
        Console.WriteLine($"[client] datagram-sequence state file: {statePath}");

        // The asker: the shared link seed and our chosen client port, resuming its sequence from the store.
        NDInsight.Sintran.Xmsg.Node.Tad.TadAskerSession asker =
            new NDInsight.Sintran.Xmsg.Node.Tad.TadAskerSession(ownNode, hostNode, clientPort: 0x0283, seed, targetName, sequenceStore);
        asker.Log += line => Console.WriteLine(line);
        asker.TerminalText += text => Console.Write(text);   // render host output inline

        // Send a batch of frames down through the codec.
        void SendAll(System.Collections.Generic.IReadOnlyList<XmsgFrame> frames)
        {
            for (int i = 0; i < frames.Count; i++)
            {
                codec.SendPacket(new XmsgPacket(frames[i]));
            }
        }

        // Host frame -> asker reacts -> send its response frames.
        codec.PacketReceived += delegate (string id, XmsgPacketInfo packet)
        {
            SendAll(asker.OnReceive(packet.Frame));
        };

        // Deliver link payloads up into the codec.
        adapter.PayloadReceived += delegate (ILink deliveringLink, byte[] payload, int length)
        {
            codec.ProcessBytes(payload.AsSpan(0, length));
        };

        // Fire the connect letter the first time the LAPB link becomes Active.
        bool connectSent = false;
        adapter.StatusChanged += delegate (ILink changedLink, LinkStatus oldStatus, LinkStatus newStatus, string reason)
        {
            Console.WriteLine($"[link] {changedLink.Name} {oldStatus} -> {newStatus} ({reason})");
            if (newStatus == LinkStatus.Active && !connectSent)
            {
                connectSent = true;
                SendAll(asker.Start());
            }
        };

        // Read stdin lines and send them as keystroke frames (best-effort; RFI gating is not enforced).
        Task inputTask = Task.Run(() =>
        {
            while (!token.IsCancellationRequested)
            {
                string? line = Console.ReadLine();
                if (line == null)
                {
                    break;
                }

                SendAll(asker.SendLine(line));
            }
        }, token);

        Console.WriteLine("[client] bringing up LAPB; type a line + Enter once the prompt appears.");
        adapter.Start();
        await adapter.Completion!;
        _ = inputTask;
    }

    /// <summary>
    /// The restructured seam composition: TcpBridgeTransport -> LapbLayerAdapter(ILink) ->
    /// BoundProtocolDetector -> XmsgCodec -> XmsgLayer. Routing/TAD services are configured on the
    /// layer; the link is bound to XMSG, and the detector (a per-link-binding stub) confirms it.
    /// </summary>
    /// <summary>
    /// Runs this node as a RELAY between two HDLC links: one the peer dials into, one we dial out.
    /// </summary>
    /// <param name="topology">
    /// The loaded topology, for the route table.
    /// </param>
    /// <param name="listenPort">
    /// The TCP port the inbound peer's bridge connects to.
    /// </param>
    /// <param name="dialHost">
    /// The host of the outbound bridge.
    /// </param>
    /// <param name="dialPort">
    /// The port of the outbound bridge.
    /// </param>
    /// <param name="node">
    /// The system number this runner presents on both links.
    /// </param>
    /// <param name="routingEntries">
    /// The routes this node advertises.
    /// </param>
    /// <param name="motdLine">
    /// The TAD banner middle line, or null for the built-in one.
    /// </param>
    /// <param name="users">
    /// The TAD login accounts.
    /// </param>
    /// <param name="fileServer">
    /// The COSMOS file server, or null when no folder is configured.
    /// </param>
    /// <param name="token">
    /// Stops the pump.
    /// </param>
    /// <returns>
    /// A task that completes when the pump stops.
    /// </returns>
    /// <remarks>
    /// <para><b>Why a node must LISTEN to be a relay</b></para>
    /// Every other path here dials out, which makes this node a leaf - an endpoint of somebody
    /// else's link. Traffic only crosses a relay when something is reachable ONLY through it, so at
    /// least one link has to be one the peer dials INTO.
    /// <para>
    /// Live shape: D103's <c>RetroCore.ini</c> points its HDLC at <paramref name="listenPort"/>
    /// instead of at D100, and we dial D100 ourselves. D103 traffic for D100 then has to cross this
    /// node, which is the only arrangement in which the relay carries anything.
    /// </para>
    /// <para><b>One host per link, deliberately</b></para>
    /// <see cref="XmsgNodeHost"/> is the whole per-link stack - codec, layer, responder sequence and
    /// learned link id all belong to ONE peer - so two links means two hosts, composed by
    /// <see cref="XmsgRelayNode"/>. That also sets each host's transit filter, which is what stops
    /// both a host and the relay acting on the same datagram.
    /// <para><b>Ordering matters</b></para>
    /// The listener comes up FIRST, so a peer already retrying its connect is accepted immediately.
    /// The outbound dial follows.
    /// </remarks>
    private static async Task RunRelayAsync(
        TopologyConfig? topology, int listenPort, string dialHost, int dialPort, ushort node,
        ushort inboundPeer, ushort outboundPeer,
        IReadOnlyList<RoutingTableEntry> routingEntries, string? motdLine, TadUserDirectory users,
        FaServer? fileServer, CancellationToken token)
    {
        Console.WriteLine($"[relay] listening on {listenPort} for the inbound bridge...");

        // Listener first: the peer may already be retrying, and this accepts it at once.
        TcpBridgeTransport inboundTransport = await TcpBridgeTransport.ListenAsync(listenPort, token);
        Console.WriteLine($"[relay] inbound bridge connected on {listenPort}");

        TcpBridgeTransport outboundTransport = await TcpBridgeTransport.ConnectAsync(dialHost, dialPort, token);
        Console.WriteLine($"[relay] outbound bridge connected to {dialHost}:{dialPort}");

        LapbLayer inboundLink = new LapbLayer(node);
        LapbLayerAdapter inbound = new LapbLayerAdapter($"hdlc-in:{listenPort}", inboundTransport, inboundLink);

        LapbLayer outboundLink = new LapbLayer(node);
        LapbLayerAdapter outbound = new LapbLayerAdapter($"hdlc-out:{dialHost}:{dialPort}", outboundTransport, outboundLink);

        string statePath = System.IO.Path.Combine(AppContext.BaseDirectory, "xmsg-sequence.state");
        FileResponderSequenceStore sequenceStore = new FileResponderSequenceStore(statePath);

        NDInsight.Sintran.Xmsg.Servers.Tad.TadServer tadServer =
            new NDInsight.Sintran.Xmsg.Servers.Tad.TadServer(() => DateTime.Now, users: users, motdLine: motdLine);

        // BOTH links serve. See BuildRelayHosts - giving the servers to only one of them is a
        // measured defect, not a theoretical one.
        XmsgNodeHost[] hosts = BuildRelayHosts(
            inbound, outbound, node, routingEntries, sequenceStore,
            BuildServerList(tadServer, fileServer));
        XmsgNodeHost inboundHost = hosts[0];
        XmsgNodeHost outboundHost = hosts[1];

        // The host's own log channel was never connected to anything, so every line it writes has
        // been going nowhere - including its "frame implies seed X but link seed is Y" WARNING,
        // which is meant to flag exactly the out-of-model frames worth investigating. Found while
        // instrumenting the Flags 1 sequence for task #28: the new [seq] lines printed nothing
        // until this was wired.
        inboundHost.ServerHost.Log = line => Console.WriteLine("[in  " + line);
        outboundHost.ServerHost.Log = line => Console.WriteLine("[out " + line);

        tadServer.ServerDirectory = inboundHost.ServerHost.DescribeServers;
        tadServer.RouteReport = inboundHost.FormatRouteReport;

        // Routes read off the topology. BOTH links need them: registering only the outbound side
        // would leave traffic coming the other way - from the dialled peer towards the inbound one -
        // with no route, and the relay would drop it as unroutable rather than forward it.
        //
        // A link reaches its own peer, plus every node whose "via" chain STARTS at that peer. Only
        // the first hop of a chain decides which of OUR links to use; the rest is the next relay's
        // problem, which is how the captured route-through behaves.
        XmsgRelayNode relay = new XmsgRelayNode();

        // A DIRECT peer of one link is never reachable "through" the other, whatever the topology
        // file says. Caught on the first live run: topology-d19999.json still declares D103 as
        // "via": [100] from when D103 hung off D100, so 103 landed on BOTH links - and because
        // AddLink re-points a duplicate route at whichever link registered last, every datagram for
        // D103 would have gone out of the D100 link instead of down the link D103 is actually on.
        // A silent misroute, and exactly the failure the topology tests were written for.
        ushort[] throughInbound = RoutesThrough(topology, inboundPeer, outboundPeer);
        ushort[] throughOutbound = RoutesThrough(topology, outboundPeer, inboundPeer);

        relay.AddHost(inboundHost, throughInbound);
        relay.AddHost(outboundHost, throughOutbound);

        Console.WriteLine(
            $"[relay] node {node}: inbound link on {listenPort} reaches [{string.Join(",", throughInbound)}], "
            + $"outbound to {dialHost}:{dialPort} reaches [{string.Join(",", throughOutbound)}]");

        // Link state on BOTH links, and the reachability ANNOUNCE when each comes up.
        //
        // Without the announce a peer never registers us at the XMSG level: the LAPB link goes
        // Active, its routing table still says it can reach us, and yet every request answers
        // "Remote system is not accessible". Measured on D103 - link Active, route "*->19999",
        // still unreachable - because nothing had ever told its XROUT we were here. The
        // single-link path announces on link-up; this one did not, which is why no traffic could
        // ever cross the relay.
        //
        // A relay must announce on BOTH links. Announcing only outbound leaves the inbound peer
        // unable to reach anything at all, including us.
        // EVERY time a link reaches Active, not just the first. A once-only guard looks tidy and is
        // wrong: when this process restarts, both peers re-SABM and the links bounce
        // Active -> Starting -> Active. With a one-shot flag the re-establish gets no announce, so a
        // peer that dropped us stays unable to reach us and nothing ever tells it otherwise -
        // exactly the "no access to system 19999" state, and invisible because the link looks fine.
        //
        // Re-announcing is safe: the announce resets our own sequence for that peer to match, which
        // is what it is for.
        // ARM here, SEND on the next loop tick - never from inside the status callback.
        //
        // Announcing directly from StatusChanged is what made this relay unusable, and it was
        // captured on 2026-08-08 (DOC/captures/FRMR-ON-INNAK-2026-08-08). The callback fires while
        // the layer is still working through a BATCH of received frames, and D100's queued SABM
        // retries arrive as SIX SABMs in a single TCP segment. Answer the first, announce from the
        // callback so V(S) becomes 1, then answer the remaining five - and spec 3.2 hard-zeroes
        // V(S) on every one of them. D100, which never reset its view of our announce, then
        // acknowledges it with N(R)=1, that no longer fits the window from V(A) to V(S), and spec
        // 4.3 obliges us to answer FRMR reason Z. The peer retransmits forever and nothing crosses
        // the relay.
        //
        // Both rules are MUST and both are correct; the defect was transmitting BETWEEN two resets.
        // Deferring to the loop tick means the whole burst is drained first. The single-link path
        // above already did exactly this ("sent here because sending from inside that callback
        // re-enters the adapter and drops the link") - the relay path did not, and that was the
        // entire difference. Pinned by LapbAnnounceOrderingTests, which replays the captured bytes.
        //
        // The flags are re-armed on EVERY Active, not once: a restart bounces both links, and a
        // one-shot guard would leave a peer that dropped us permanently unable to reach us.
        // onceOnly: false - a relay MUST re-announce on every Active. See LinkAnnouncer, which owns
        // the arm-then-send-on-tick mechanism for both this path and the single-link one.
        LinkAnnouncer inboundAnnouncer = new LinkAnnouncer(
            () => inbound.Status,
            () =>
            {
                Console.WriteLine($"[relay] announcing to inbound peer {inboundPeer}");
                inboundHost.AnnounceRestart(inboundPeer);
            },
            enabled: true,
            onceOnly: false);

        LinkAnnouncer outboundAnnouncer = new LinkAnnouncer(
            () => outbound.Status,
            () =>
            {
                Console.WriteLine($"[relay] announcing to outbound peer {outboundPeer}");
                outboundHost.AnnounceRestart(outboundPeer);
            },
            enabled: true,
            onceOnly: false);

        inbound.StatusChanged += (link, previous, current, reason) =>
        {
            Console.WriteLine($"[relay] inbound link {previous} -> {current} ({reason})");
            inboundAnnouncer.OnStatusChanged(current);
        };

        outbound.StatusChanged += (link, previous, current, reason) =>
        {
            Console.WriteLine($"[relay] outbound link {previous} -> {current} ({reason})");
            outboundAnnouncer.OnStatusChanged(current);
        };

        // What each peer SENDS US, and whether we answered it.
        //
        // The relay path had neither, which made a hang invisible: a request addressed to our own
        // node is not transit, so the relay counters stay still and the log says nothing while the
        // caller waits out its timeout. The single-link path carries this and the ANSWERED-OR-NOT
        // check below for exactly that reason - an inbound datagram that produces no reply hangs
        // the calling SINTRAN terminal, and ESC will not abort it.
        LogReceivedOn("in ", inboundHost);
        LogReceivedOn("out", outboundHost);

        relay.Relay.Relayed += (fromLink, toLink, destination) =>
            Console.WriteLine($"[relay] {fromLink.Name} -> {toLink.Name} for node {destination}");
        relay.Relay.NotRelayed += (fromLink, destination, reason) =>
            Console.WriteLine($"[relay] DROPPED from {fromLink.Name} for node {destination}: {reason}");

        // Each adapter runs its own read/timer loop and raises LoopTick per iteration, so the relay
        // is pumped from whichever link is active rather than from a third loop of our own.
        long lastRelayed = 0;
        LapbLayerAdapter.LinkLoopTick pump = delegate
        {
            relay.Pump();

            if (relay.Relay.DatagramsRelayed != lastRelayed)
            {
                lastRelayed = relay.Relay.DatagramsRelayed;
                Console.WriteLine(
                    $"[relay] relayed={lastRelayed} forUs={relay.Relay.DatagramsForUs} "
                    + $"dropped={relay.Relay.DatagramsDropped}");
            }
        };

        // The deferred announces, one handler per link so each fires on its OWN loop thread and
        // checks its OWN status. By the time a loop tick runs, every frame that arrived in the last
        // batch has been processed - including a whole burst of SABMs - so V(S) is settled and the
        // announce cannot land between two resets. See the StatusChanged comment above.
        inbound.LoopTick += delegate { inboundAnnouncer.OnLoopTick(); };
        outbound.LoopTick += delegate { outboundAnnouncer.OnLoopTick(); };

        inbound.LoopTick += pump;
        outbound.LoopTick += pump;

        inbound.Initiate();
        outbound.Initiate();
        Console.WriteLine("[relay] SABM sent on both links; pumping...");

        // BOTH links need a keepalive interval, and running without one was a second defect found on
        // 2026-08-08 while verifying the announce fix. With keepaliveInterval null the adapter
        // documents itself as "the pump simply blocks on reads (in-memory tests)": LoopTick then
        // fires ONLY when a frame arrives, so the deferred announce could sit unsent forever, and -
        // worse - the LAPB T1 retransmit and T3 keepalive timers never tick at all on a LIVE link.
        // The single-link path has always passed 20 ms; the relay path never did.
        await Task.WhenAll(
            inbound.RunAsync(token, TimeSpan.FromMilliseconds(20)),
            outbound.RunAsync(token, TimeSpan.FromMilliseconds(20)));
    }

    /// <summary>
    /// Logs what a host receives, and whether it managed to answer.
    /// </summary>
    /// <param name="side">
    /// A short label naming which link this host sits on, for the log line.
    /// </param>
    /// <param name="host">
    /// The host to watch.
    /// </param>
    /// <remarks>
    /// The second handler is the one that matters. A request that produces NO reply leaves the
    /// calling SINTRAN waiting out its timeout with ESC unable to abort it, and until this was
    /// logged the two cases looked identical from here - silence either way.
    /// </remarks>
    private static void LogReceivedOn(string side, XmsgNodeHost host)
    {
        host.Layer.MessageReceived += delegate (string id, XmsgPacketInfo packet)
        {
            XmsgFrame f = packet.Frame;
            Console.WriteLine(
                $"[{side} RX] {f.Header.SourceNode}->{f.Header.DestinationNode} "
                + $"sub={f.Header.Subtype} f1=0x{f.Header.Flags1:X4} "
                + $"info={Convert.ToHexString(packet.RawBytes)}");
        };

        host.Layer.DispatchCompleted += delegate (string id, XmsgPacketInfo packet, int produced)
        {
            XmsgFrame f = packet.Frame;

            if (produced == 0)
            {
                Console.WriteLine(
                    $"[{side} RX] {f.Header.SourceNode}->{f.Header.DestinationNode} "
                    + $"sub={f.Header.Subtype} *** NO REPLY BUILT *** (this hangs the caller)");
                return;
            }

            Console.WriteLine(
                $"[{side} RX] {f.Header.SourceNode}->{f.Header.DestinationNode} "
                + $"sub={f.Header.Subtype} answered with {produced} frame(s)");
        };
    }

    /// <summary>
    /// Gets the node numbers a link reaches: its own peer, plus every node routed through that peer.
    /// </summary>
    /// <param name="topology">
    /// The loaded topology, or null when none was supplied.
    /// </param>
    /// <param name="peer">
    /// The node on the far end of the link.
    /// </param>
    /// <param name="otherLinkPeer">
    /// The node on the far end of this relay's OTHER link, which is excluded.
    /// </param>
    /// <returns>
    /// The reachable node numbers, always including <paramref name="peer"/> itself.
    /// </returns>
    /// <remarks>
    /// <para>
    /// The peer is included because a relay must be able to deliver TO the machine on the other end
    /// of a link, not only through it. Leaving it out is a silent way to make a directly connected
    /// node unreachable.
    /// </para>
    /// <para><b>Why the other link's peer is excluded</b></para>
    /// A topology file describes how nodes were reachable when it was written. Moving a machine onto
    /// a relay's own link does not update it, so a node that is now a DIRECT neighbour can still be
    /// declared "via" somewhere else - and it would then be registered on both links, with the
    /// second registration winning. Excluding the other link's peer makes the direct connection
    /// authoritative over a stale route, which is the safe direction to be wrong in.
    /// </remarks>
    private static ushort[] RoutesThrough(TopologyConfig? topology, ushort peer, ushort otherLinkPeer)
    {
        List<ushort> routes = new List<ushort>();
        routes.Add(peer);

        if (topology != null)
        {
            List<ushort> via = topology.NodesReachableThrough(peer);
            for (int i = 0; i < via.Count; i++)
            {
                if (via[i] != otherLinkPeer && !routes.Contains(via[i]))
                {
                    routes.Add(via[i]);
                }
            }
        }

        return routes.ToArray();
    }

    private static async Task RunSeamAsync(
        TcpBridgeTransport transport, string host, int port, ushort node,
        IReadOnlyList<RoutingTableEntry> routingEntries, string? motdLine, TadUserDirectory users,
        FaServer? fileServer, FaPushRun? pushRun, FaPullRun? pullRun, CancellationToken token)
    {
        string linkId = $"hdlc:{host}:{port}";

        LapbLayer link = new LapbLayer(node);
        LapbLayerAdapter adapter = new LapbLayerAdapter(linkId, transport, link);

        // Persist our outgoing datagram sequence per remote node across restarts (a state file next
        // to the runner), so we continue in step with 100's persistent expected-from-us instead of
        // resetting to 0x0000 and being silently dropped. See XMSG-SEQUENCE-RESTART-ANSWER doc.
        string statePath = System.IO.Path.Combine(AppContext.BaseDirectory, "xmsg-sequence.state");
        FileResponderSequenceStore sequenceStore = new FileResponderSequenceStore(statePath);
        Console.WriteLine($"[runner] datagram-sequence state file: {statePath}");

        // The MOTD banner middle line comes from the topology file when set; otherwise the server uses its
        // built-in "Emulated TAD server version vN.N.N". The host-id line is generated from our own node.
        NDInsight.Sintran.Xmsg.Servers.Tad.TadServer tadServer =
            new NDInsight.Sintran.Xmsg.Servers.Tad.TadServer(() => DateTime.Now, users: users, motdLine: motdLine);
        Console.WriteLine($"[tad] {users.Count} login account(s) loaded");
        tadServer.SessionOpened += (tadNumber, clientSystem) =>
            Console.WriteLine($"[tad] session opened: tty{tadNumber} from node {clientSystem}");
        tadServer.SessionClosed += (tadNumber, clientSystem) =>
            Console.WriteLine($"[tad] session closed: tty{tadNumber} from node {clientSystem}");

        // Everything above the link - detector, codec, XmsgLayer, routing table, XmsgServerHost and
        // the registered servers - is transport-agnostic and lives in XmsgNodeHost, so the Ethernet
        // node reuses this composition instead of copying it. Only the LAPB diagnostics below are
        // HDLC-specific.
        XmsgNodeHost nodeHost = new XmsgNodeHost(
            adapter, node, routingEntries, sequenceStore, BuildServerList(tadServer, fileServer));
        XmsgLayer layer = nodeHost.Layer;

        // Wire the introspection commands: "list servers" reads the host's registered servers; "list route"
        // formats the routing table this node advertises.
        tadServer.ServerDirectory = nodeHost.ServerHost.DescribeServers;
        tadServer.RouteReport = nodeHost.FormatRouteReport;

        // SABM-storm suppression. A short burst of peer SABMs during link establishment is NORMAL (the
        // balanced SABM/UA handshake) - it is NOT "machine down". We collapse the repeated SABM churn
        // (and our UA/RR echoes) quietly, and ONLY escalate to a "peer XMSG is down" warning if the
        // SABMs PERSIST with no data phase for several seconds (a crashed peer re-SABMs forever).
        bool stormActive = false;
        bool peerDownWarned = false;
        int peerSabmStreak = 0;
        DateTime sabmStreakStart = default;
        TimeSpan sabmDownThreshold = TimeSpan.FromSeconds(5);

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
                    $"  XMCSM=0x{f.ControlService:X8} role=0x{f.SubHeader.Role:X2}");
                Console.Write(NDInsight.Sintran.Xmsg.Diagnostics.XmsgDump.ToText(f));
            }

            // A push is a conversation we started, so its replies arrive here like anything else.
            // The driver decides which frames are its own - a TAD session on the same link must not
            // be able to fail a file transfer.
            if (pushRun != null)
            {
                pushRun.OnFrame(f);
            }

            // A pull is the same kind of conversation, and its content arrives the same way. The
            // frames have already been through the fragment reassembler by this point, so a
            // 1032-byte data message arrives whole rather than as its 0x0A and 0x0C halves.
            if (pullRun != null)
            {
                pullRun.OnFrame(f);
            }
        };

        layer.SessionOpened += delegate (string id, ushort clientSystem, ushort clientPort)
        {
            Console.WriteLine($"[session] opened by system {clientSystem} port 0x{clientPort:X4}");
        };
        // Announce ourselves ONCE, the first time the link comes up: our XMSG has just started, so
        // the peer must reset the datagram sequence it expects from us. Without this the peer keeps
        // counting from wherever it was while we start at zero, and the conversation dies silently
        // just after the connect confirmation. See XmsgNodeHost.AnnounceRestart.
        // OFF by default - it does net HARM as it stands. MEASURED 2026-08-04, with the header
        // checksum already fixed and the send already deferred to the loop tick:
        //
        //     20:40:19.625  link Starting -> Active
        //     20:40:19.667  announce sent
        //     20:40:20.027  link Active -> Starting        <- the link goes down anyway
        //     20:40:34.724  link back to Active
        //
        // and afterwards the two sides were still out of step - D100 opened its conversation at
        // Flags 1 0x000C while we were at 0x0000 - so the announce did not achieve the one thing it
        // exists for. Either D100 re-establishes LAPB when told a peer restarted (arguably correct)
        // and the request is lost in the reset, or it never processed it at all. UNKNOWN which.
        //
        // Without it, both sides simply start from zero after a machine restart, which is what the
        // runs that DID work today all had. Flip this to true only to investigate further.
        const bool AnnounceRestartOnLinkUp = false;

        // Arm on status, send on the next loop tick - the mechanism lives in LinkAnnouncer, shared
        // with the relay path. Sending from inside the status callback re-enters the LAPB adapter
        // while it is still completing the transition and drops the link (measured 260 ms after the
        // announce, 2026-08-04); on the relay it produced an FRMR storm instead. One copy of that
        // rule, two policies.
        //
        // onceOnly: true here - unlike the relay, this path announced at most once when it was
        // enabled at all.
        LinkAnnouncer announcer = new LinkAnnouncer(
            () => adapter.Status,
            () =>
            {
                for (int i = 0; i < routingEntries.Count; i++)
                {
                    RoutingTableEntry entry = routingEntries[i];
                    // Direct neighbours only (Hops == 1). Ourselves, and anything reached THROUGH
                    // another node, are not ours to announce to.
                    if (entry.System == node || entry.Hops != 1)
                    {
                        continue;
                    }

                    Console.WriteLine(
                        $"[link] announcing our XMSG restart to node {entry.System} " +
                        "(ReachabilityRequest; resets both sides' Flags 1 to 0x0000)");
                    nodeHost.AnnounceRestart(entry.System);
                }
            },
            enabled: AnnounceRestartOnLinkUp,
            onceOnly: true);

        adapter.StatusChanged += delegate (ILink changedLink, LinkStatus oldStatus, LinkStatus newStatus, string reason)
        {
            Console.WriteLine($"[link] {changedLink.Name} status {oldStatus} -> {newStatus} ({reason})");
            announcer.OnStatusChanged(newStatus);
        };

        // Diagnostic: log every raw LAPB frame 100 sends us (SABM/UA/RR/I). Detects and COLLAPSES a
        // bare-link SABM storm (peer XMSG crashed) into a single actionable warning.
        adapter.RawFrameReceived += delegate (string id, byte[] body)
        {
            byte ctrl = body.Length > 1 ? body[1] : (byte)0;

            if (ctrl == 0x3F)   // SABM from the peer
            {
                peerSabmStreak++;
                if (peerSabmStreak == 1)
                {
                    sabmStreakStart = DateTime.UtcNow;
                }

                // Collapse the repeated SABM churn after a few. This is NORMAL link-establishment
                // handshaking - do NOT claim the peer is down for it.
                if (peerSabmStreak >= 3 && !stormActive)
                {
                    stormActive = true;
                    Console.WriteLine("[link] collapsing repeated peer SABMs (link establishing)...");
                }

                // Only if the SABMs PERSIST with no data phase for several seconds is the peer's XMSG
                // actually down (a crashed peer re-SABMs a bare link forever). Say so once, accurately.
                if (stormActive && !peerDownWarned
                    && DateTime.UtcNow - sabmStreakStart > sabmDownThreshold)
                {
                    peerDownWarned = true;
                    Console.WriteLine(
                        "[!] Peer has re-sent SABM for over 5s with no XMSG data - machine 100's XMSG is " +
                        "likely DOWN (crashed / not started). Restart XMSG on 100, then reconnect.");
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
                    // Only note "resumed" if we had actually warned the peer was down; a normal
                    // establishment burst resolves silently.
                    if (peerDownWarned)
                    {
                        Console.WriteLine("[link] peer traffic resumed.");
                    }

                    stormActive = false;
                    peerDownWarned = false;
                }

                peerSabmStreak = 0;
            }

            string kind;
            if ((ctrl & 1) == 0)
            {
                kind = $"I ns={(ctrl >> 1) & 7} nr={(ctrl >> 5) & 7}";
            }
            else if ((ctrl & 3) == 1)
            {
                // S-frame: the supervisory subtype is bits 2-3 (0=RR, 1=RNR, 2=REJ). Do NOT label every
                // S-frame "RR" — that hid 100's REJ (ctrl 0xC9) as an "RR" in the raw log.
                string s = ((ctrl >> 2) & 3) switch { 0 => "RR", 1 => "RNR", 2 => "REJ", _ => "S?" };
                kind = $"{s} nr={(ctrl >> 5) & 7}";
            }
            else
            {
                kind = ctrl switch { 0x3F => "SABM", 0x73 => "UA", 0x53 => "DISC", 0x1F => "DM", 0x87 => "FRMR", _ => $"U 0x{ctrl:X2}" };
            }
            Console.WriteLine($"[rx-raw] a=0x{(body.Length > 0 ? body[0] : 0):X2} {kind} [{Convert.ToHexString(body)}]");
        };

        // Time-based output pump (runs on the adapter loop thread, no locking needed). A multi-chunk
        // terminal reply is streamed as 255-byte continuation PAIRS spaced ~46 ms apart (22.16): the SECOND
        // chunk of each pair has no inbound trigger, so DrainPending must be re-driven on a timer to release
        // it once the gap elapses. serverHost.DrainPending walks each active session's burst and returns the
        // now-permitted frames; we send them straight down the codec, same as the inbound-driven path.
        adapter.LoopTick += delegate ()
        {
            // Announce our XMSG restart once the link has settled, so the peer resets the datagram
            // sequence it expects from us. Armed by StatusChanged; sent here because sending from
            // inside that callback re-enters the adapter and drops the link.
            announcer.OnLoopTick();
            nodeHost.Pump();

            // The push rides the same tick. It sends nothing until LAPB is Connected, because a
            // frame we originate before the link is up is simply lost, and nothing after it makes
            // sense. The driver returns nothing while it waits for the server, so the ladder paces
            // itself instead of being paced by this timer.
            if (pushRun != null)
            {
                pushRun.Pump(nodeHost, link.State == LapbLayerState.Connected);
            }

            // The pull rides the same tick, gated the same way. Only one of the two can be set -
            // the runner refuses --push and --pull together.
            if (pullRun != null)
            {
                pullRun.Pump(nodeHost, link.State == LapbLayerState.Connected);
            }
        };

        adapter.Initiate();
        Console.WriteLine("[runner] SABM sent; pumping seam link (LAPB + codec + XmsgLayer)...");
        // Tick every 20 ms so (a) the LAPB T1/T3/N2 timers run and (b) the output pump above can release a
        // continuation pair's second chunk within the ~46 ms intra-pair window. The idle branch only TICKS
        // + pumps; it does NOT flood the link with RRs (the T3 keepalive poll is emitted from inside Tick),
        // so a healthy idle link stays near-silent and DrainPending returns nothing when no burst is active.
        // (Was 1 s: too coarse to reproduce the measured 45-47 ms intra-pair spacing.)
        await adapter.RunAsync(token, keepaliveInterval: TimeSpan.FromMilliseconds(20));
    }

    /// <summary>
    /// Builds this runner's XSGSY routing table relative to the node number we are impersonating.
    /// </summary>
    /// <param name="node">
    /// The system number this runner presents on the wire (the runner's <c>ownNode</c> argument).
    /// </param>
    /// <returns>
    /// The routing-table entries, self-consistent for whichever node we run as.
    /// </returns>
    /// <remarks>
    /// The table MUST be built relative to <paramref name="node"/>, otherwise the XSGSY server hands
    /// node 100 a route that loops back through itself. Concretely, the old fixed table always carried
    /// a "102 reachable Via 100" entry. When the runner is started AS node 102 that entry shadows the
    /// correct "102 is Local (this is me)" self-entry — <see cref="InMemoryRoutingTable.TryLookup"/>
    /// returns the first <c>System >= query</c>, which is the Via-100 entry — so 100's <c>li-rout</c>
    /// reports <c>*->102->100->102...  *Loop suspected*</c> and connect-to routing breaks.
    /// Rules:
    ///  - Our own <paramref name="node"/> is ALWAYS Local (0 hops); never advertise a route to ourselves via anyone.
    ///  - 100 is the direct HDLC link peer (Neighbour), unless we ourselves are 100.
    ///  - 102 is the TAD responder reached through 100 (Via, 2 hops), unless we ourselves are 102.
    /// </remarks>
    /// <summary>
    /// Runs the node over a COSMOS Ethernet segment instead of an HDLC bridge:
    /// EthernetBackend -> EthernetLink(ILink) -> XmsgNodeHost.
    /// </summary>
    /// <param name="topology">
    /// The loaded topology, for the segment settings.
    /// </param>
    /// <param name="peer">
    /// The Ethernet neighbour this node speaks to.
    /// </param>
    /// <param name="node">
    /// The system number this runner presents on the wire.
    /// </param>
    /// <param name="routingEntries">
    /// The routes this node advertises.
    /// </param>
    /// <param name="motdLine">
    /// The TAD banner middle line, or null for the built-in one.
    /// </param>
    /// <param name="users">
    /// The TAD login accounts.
    /// </param>
    /// <param name="pushRun">
    /// The file push, or null when none was asked for. It sends nothing until the peer has
    /// addressed us, because the peer's link id is learned and cannot be derived.
    /// </param>
    /// <param name="token">
    /// Stops the pump.
    /// </param>
    /// <returns>
    /// A task that completes when the pump stops.
    /// </returns>
    /// <remarks>
    /// The composition above the link is IDENTICAL to the HDLC path - the same
    /// <see cref="XmsgNodeHost"/> - because the SINTRAN header is transport-independent. What is
    /// missing here compared to the HDLC path is the LAPB diagnostics, which have no Ethernet
    /// equivalent: the ND link layer has no SABM, no window and no retransmission.
    /// </remarks>
    private static async Task RunEthernetSeamAsync(
        TopologyConfig topology, TopologyNode peer, ushort node,
        IReadOnlyList<RoutingTableEntry> routingEntries, string? motdLine,
        TadUserDirectory users, FaServer? fileServer, FaPushRun? pushRun, FaPullRun? pullRun,
        bool traceFrames,
        CancellationToken token)
    {
        // The segment settings come from OUR entry when we have one, otherwise from the peer's -
        // both machines must be pointed at the same group for anything to arrive.
        TopologyNode? self = topology.FindById(topology.Self);
        string spec = TopologyConfig.EthernetSpec(self?.Ethernet ?? peer.Ethernet);

        IEthernetBackend? backend = EthernetBackendFactory.FromSpec(spec);
        if (backend == null)
        {
            Console.WriteLine($"[runner] ethernet backend spec not supported: {spec}");
            return;
        }

        string linkId = $"eth:{peer.Alias ?? peer.Id.ToString()}";
        EthernetLink link = new EthernetLink(linkId, node, peer.Id, backend);

        // SEGMENT SNIFFER. The link itself only accepts frames from its own peer, which is correct
        // but leaves us blind to everything else on a shared segment - and "nothing happened" is
        // exactly the report that needs evidence behind it. This logs EVERY frame that reaches us,
        // whoever sent it, so we can see who talks, to whom, and when. It observes only; the link's
        // own handler does the work.
        backend.OnPacketReceived += delegate (byte[] data, int length)
        {
            // OFF unless asked for: this runs on the hot receive path and its hex dumps once cost
            // us a 1.77 s median acknowledgement against a 0.19 s peer retransmit timer.
            if (!traceFrames)
            {
                return;
            }

            if (!Ieee8023Frame.TryParse(
                    new ReadOnlySpan<byte>(data, 0, length),
                    out NdMacAddress destination, out NdMacAddress source,
                    out int payloadOffset, out int payloadLength))
            {
                Console.WriteLine($"[sniff] {length}B frame that is not ND/COSMOS 802.3+LLC1");
                return;
            }

            // Frames we sent ourselves come back on a multicast segment; skip our own.
            if (source.Equals(link.LocalMac))
            {
                return;
            }

            string from = DescribeStation(source);
            string to = DescribeStation(destination);

            if (payloadLength >= NdLinkHeader.Length
                && NdLinkHeader.TryParse(
                    new ReadOnlySpan<byte>(data, payloadOffset, payloadLength), out NdLinkHeader ndHeader))
            {
                string kind = ndHeader.IsData ? "data" : ndHeader.IsAcknowledge ? "ack" : $"kind0x{ndHeader.Kind:X2}";
                Console.WriteLine(
                    $"[sniff] {from} -> {to}  {kind} seq={ndHeader.Sequence} " +
                    $"snd={ndHeader.SenderLinkId} rcv={ndHeader.ReceiverLinkId} plen={ndHeader.PayloadLength}" +
                    (destination.Equals(link.LocalMac) ? "   <== ADDRESSED TO US" : string.Empty));

                // Full hex of anything that is NOT the two kinds we already understand, and of
                // anything aimed at us. A frame kind nobody has captured cannot be decoded from a
                // field summary - the bytes are the evidence, and guessing at the layout instead is
                // exactly what this project has been burned by.
                if (!ndHeader.IsData && !ndHeader.IsAcknowledge || destination.Equals(link.LocalMac))
                {
                    Console.WriteLine($"[sniff]   llc+nd : {Convert.ToHexString(data, payloadOffset, payloadLength)}");
                    Console.WriteLine($"[sniff]   frame  : {Convert.ToHexString(data, 0, length)}");
                }
            }
            else
            {
                Console.WriteLine($"[sniff] {from} -> {to}  {payloadLength}B (no ND link header)");
            }
        };

        Console.WriteLine(
            $"[runner] joining COSMOS segment {spec} as node {node} " +
            $"(MAC {Describe(link.LocalMac)}), peer {peer.Id} (MAC {Describe(link.PeerMac)})");

        string statePath = System.IO.Path.Combine(AppContext.BaseDirectory, "xmsg-sequence.state");
        FileResponderSequenceStore sequenceStore = new FileResponderSequenceStore(statePath);
        Console.WriteLine($"[runner] datagram-sequence state file: {statePath}");

        NDInsight.Sintran.Xmsg.Servers.Tad.TadServer tadServer =
            new NDInsight.Sintran.Xmsg.Servers.Tad.TadServer(() => DateTime.Now, users: users, motdLine: motdLine);
        Console.WriteLine($"[tad] {users.Count} login account(s) loaded");
        tadServer.SessionOpened += (tadNumber, clientSystem) =>
            Console.WriteLine($"[tad] session opened: tty{tadNumber} from node {clientSystem}");
        tadServer.SessionClosed += (tadNumber, clientSystem) =>
            Console.WriteLine($"[tad] session closed: tty{tadNumber} from node {clientSystem}");

        XmsgNodeHost nodeHost = new XmsgNodeHost(
            link, node, routingEntries, sequenceStore, BuildServerList(tadServer, fileServer));
        tadServer.ServerDirectory = nodeHost.ServerHost.DescribeServers;
        tadServer.RouteReport = nodeHost.FormatRouteReport;

        nodeHost.Layer.MessageReceived += delegate (string id, XmsgPacketInfo packet)
        {
            XmsgFrame f = packet.Frame;

            // The full hex of every inbound message is trace-only: it runs before the reply is
            // built, so it lands directly on the acknowledgement latency the peer is timing.
            if (traceFrames)
            {
                Console.WriteLine(
                    $"[RX] {f.Header.SourceNode}->{f.Header.DestinationNode} " +
                    $"sub={f.Header.Subtype} f1=0x{f.Header.Flags1:X4} " +
                    $"info={Convert.ToHexString(packet.RawBytes)}");
            }

            // A push is a conversation we started, so its replies arrive here like anything else.
            // The driver decides which frames are its own.
            if (pushRun != null)
            {
                pushRun.OnFrame(f);
            }

            // A pull is the same kind of conversation, and its content arrives the same way,
            // already reassembled from its fragment pair.
            if (pullRun != null)
            {
                pullRun.OnFrame(f);
            }
        };

        // ANSWERED-OR-NOT. An inbound datagram that produces no reply hangs the calling SINTRAN
        // terminal (ESC will not abort it), and until now that looked identical in the log to a
        // datagram that WAS answered. Log the answer count against every request so
        // "received but unanswered" can never be silent again.
        nodeHost.Layer.DispatchCompleted += delegate (string id, XmsgPacketInfo packet, int produced)
        {
            XmsgFrame f = packet.Frame;
            if (produced == 0)
            {
                Console.WriteLine(
                    $"[RX] {f.Header.SourceNode}->{f.Header.DestinationNode} " +
                    $"sub={f.Header.Subtype} *** NO REPLY BUILT *** (this hangs the caller)");
                return;
            }

            Console.WriteLine(
                $"[RX] {f.Header.SourceNode}->{f.Header.DestinationNode} " +
                $"sub={f.Header.Subtype} answered with {produced} frame(s)");
        };

        // A reply the stack built but the link refused never reaches the wire. That is a DIFFERENT
        // fault from building no reply at all, and it lives in different code, so it gets its own
        // line rather than being folded into the one above.
        nodeHost.Transport.SendRefused += delegate (ILink refusing, int byteCount)
        {
            Console.WriteLine(
                $"[TX] *** DROPPED *** {refusing.Name} refused a {byteCount}B reply " +
                $"(status {refusing.Status}); total refused {nodeHost.Transport.RefusedFrames}");
        };

        link.StatusChanged += delegate (ILink changed, LinkStatus older, LinkStatus newer, string reason)
        {
            Console.WriteLine($"[link] {changed.Name} status {older} -> {newer} ({reason})");
        };

        // Everything WE put on the wire, decoded the same way the sniffer decodes inbound frames, so
        // the two sides of a conversation can be read against each other in one log.
        link.OnFrameTransmitted += delegate (byte[] frame, int length)
        {
            // OFF unless asked for - same hot-path cost as the sniffer above.
            if (!traceFrames)
            {
                return;
            }

            if (!Ieee8023Frame.TryParse(
                    new ReadOnlySpan<byte>(frame, 0, length),
                    out NdMacAddress destination, out NdMacAddress source,
                    out int payloadOffset, out int payloadLength))
            {
                Console.WriteLine($"[tx] {length}B frame that is not ND/COSMOS 802.3+LLC1");
                return;
            }

            if (payloadLength >= NdLinkHeader.Length
                && NdLinkHeader.TryParse(
                    new ReadOnlySpan<byte>(frame, payloadOffset, payloadLength), out NdLinkHeader txHeader))
            {
                string kind = txHeader.IsData ? "data" : txHeader.IsAcknowledge ? "ack" : $"kind0x{txHeader.Kind:X2}";
                Console.WriteLine(
                    $"[tx] {DescribeStation(source)} -> {DescribeStation(destination)}  {kind} " +
                    $"seq={txHeader.Sequence} snd={txHeader.SenderLinkId} rcv={txHeader.ReceiverLinkId} " +
                    $"plen={txHeader.PayloadLength}");
                Console.WriteLine($"[tx]   llc+nd : {Convert.ToHexString(frame, payloadOffset, payloadLength)}");
            }
            else
            {
                Console.WriteLine($"[tx]   frame  : {Convert.ToHexString(frame, 0, length)}");
            }
        };

        link.OnUnknownFrameKindReceived += kind =>
            Console.WriteLine(
                $"[link] unknown ND frame kind 0x{kind:X2} (captured so far: 0x0F CR, 0x20 DT, 0x3F AK, 0x60/0x6F DR)");

        // High nibble 6 is the disconnect-request family. D100 sends 0x60 repeatedly once it gives
        // up on a conversation; before 2026-08-04 that fell through to UnknownFrameKindReceived and
        // filled the log. What the LOW nibble means is UNVERIFIED - see NdLinkFrameKind.
        link.OnDisconnectRequested += kind =>
            Console.WriteLine($"[link] disconnect request from the peer, ND frame kind 0x{kind:X2}");

        link.Start();

        // The link CANNOT open the conversation: a data frame carries the peer's link id in its
        // receiver field, and that id is learned from the peer, never derived. So we sit here until
        // the ND machine addresses us. If it never does, that is a real finding, not a hang - say so
        // rather than looking dead.
        Console.WriteLine(
            $"[runner] waiting for node {peer.Id} to send us a frame (we cannot address it until it does)...");

        bool announced = false;
        DateTime started = DateTime.UtcNow;

        // Heartbeat while we wait. Silence is ambiguous - it could mean "the segment is dead" or
        // "the segment is busy but nobody is addressing us", and those need completely different
        // fixes. Reporting what we HEAR tells the two apart without a packet capture (which would
        // not work here anyway: host-local multicast never reaches npcap).
        DateTime lastHeartbeat = DateTime.UtcNow;
        long lastOtherStations = 0;

        // 20 ms so the TAD continuation-pair pump can release a second chunk inside the measured
        // ~46 ms intra-pair window (TAD-Message-Formats.md 22.16).
        while (!token.IsCancellationRequested)
        {
            nodeHost.Pump();

            if (!announced && link.HasLearnedPeer)
            {
                announced = true;
                Console.WriteLine($"[runner] peer {peer.Id} heard after {(DateTime.UtcNow - started).TotalSeconds:F1}s; link is up");
            }

            // HasLearnedPeer is exactly the right gate: it is true once the peer's link id has
            // arrived in a frame from the peer, which is the one thing a frame WE originate cannot
            // do without. Before that, anything we sent would carry a receiver id we invented.
            if (pushRun != null)
            {
                pushRun.Pump(nodeHost, link.HasLearnedPeer);
            }

            // The pull rides the same tick, gated on the same learned peer id.
            if (pullRun != null)
            {
                pullRun.Pump(nodeHost, link.HasLearnedPeer);
            }

            if (DateTime.UtcNow - lastHeartbeat >= TimeSpan.FromSeconds(5))
            {
                lastHeartbeat = DateTime.UtcNow;
                long other = link.FramesFromOtherStations;
                long delta = other - lastOtherStations;
                lastOtherStations = other;

                if (!announced)
                {
                    Console.WriteLine(
                        $"[segment] still waiting for {peer.Id}. Frames from other stations: {other} " +
                        $"(+{delta} in the last 5s). " +
                        (other == 0
                            ? "NOTHING at all is arriving - either no ND machine is on this group, or they have not been restarted since the ini change."
                            : "The segment IS carrying traffic, so the group is right - it is just not coming from our peer."));
                }
                else
                {
                    Console.WriteLine(
                        $"[segment] data={link.DataFramesReceived} acks={link.AcknowledgementsReceived} " +
                        $"other-stations={other}");
                }
            }

            try
            {
                await Task.Delay(20, token);
            }
            catch (OperationCanceledException)
            {
                break;
            }
        }

        if (!announced)
        {
            Console.WriteLine(
                $"[runner] node {peer.Id} never sent us a frame. The link stayed in {link.Status}, so nothing " +
                $"could be sent. Frames seen from other stations: {link.FramesFromOtherStations}.");
        }

        link.Dispose();
    }

    /// <summary>
    /// Formats a station address as the usual colon-separated hex, for logs.
    /// </summary>
    /// <param name="mac">
    /// The address to format.
    /// </param>
    /// <returns>
    /// The formatted address.
    /// </returns>
    /// <summary>
    /// Names a station by its ND system number when the address carries the ND vendor prefix,
    /// falling back to the raw address.
    /// </summary>
    /// <param name="mac">
    /// The station address.
    /// </param>
    /// <returns>
    /// Something readable for a log line.
    /// </returns>
    private static string DescribeStation(NdMacAddress mac)
    {
        // The system number is stored byte-REVERSED in the MAC; TryGetSystemNumber undoes that.
        if (mac.TryGetSystemNumber(out ushort system))
        {
            return $"node{system}";
        }

        return Describe(mac);
    }

    private static string Describe(NdMacAddress mac)
    {
        Span<byte> bytes = stackalloc byte[NdMacAddress.Length];
        mac.Write(bytes);

        System.Text.StringBuilder sb = new System.Text.StringBuilder(17);
        for (int i = 0; i < bytes.Length; i++)
        {
            if (i > 0)
            {
                sb.Append(':');
            }

            sb.Append(bytes[i].ToString("X2"));
        }

        return sb.ToString();
    }

    // The route report now lives on XmsgNodeHost, so every transport formats it identically.

    private static List<RoutingTableEntry> BuildRoutingEntries(ushort node)
    {
        List<RoutingTableEntry> entries = new List<RoutingTableEntry>(3);

        // Self is always directly local — this is the entry that keeps 100 from routing to us via 100.
        entries.Add(new RoutingTableEntry(node, XroutConnectionType.Local, node, 0, 0));

        // 100 is the machine on the other end of the HDLC bridge: a direct neighbour (1 hop).
        if (node != 100)
        {
            entries.Add(new RoutingTableEntry(100, XroutConnectionType.Neighbour, 1, 1, 0));
        }

        // 102 (the TAD terminal server) is reached through 100 — but only when WE are not 102 ourselves,
        // otherwise we would advertise a Via-100 loop back to our own node number.
        if (node != 102)
        {
            entries.Add(new RoutingTableEntry(102, XroutConnectionType.Via, 100, 2, 0));
        }

        return entries;
    }

    /// <summary>
    /// The original LiveNode + XmsgNode composition, retained until the seam path is validated live.
    /// </summary>
    private static async Task RunLegacyAsync(
        TcpBridgeTransport transport, ushort node,
        IReadOnlyList<RoutingTableEntry> routingEntries, CancellationToken token)
    {
        LapbLayer link = new LapbLayer(node);
        XmsgNode xnode = new XmsgNode(node, 0x00);
        xnode.AcknowledgeData = false;

        xnode.RoutingTable = new InMemoryRoutingTable(routingEntries);
        xnode.TadResponder = new NDInsight.Sintran.Xmsg.Node.Tad.TadTerminalResponder(node, () => DateTime.Now);
        xnode.AcknowledgeTadFrames = true;

        link.OnTransmit += body =>
        {
            byte ctrl = body.Length > 1 ? body[1] : (byte)0;
            string kind;
            if ((ctrl & 1) == 0)
            {
                kind = $"I ns={(ctrl >> 1) & 7} nr={(ctrl >> 5) & 7}";
            }
            else if ((ctrl & 3) == 1)
            {
                // S-frame: the supervisory subtype is bits 2-3 (0=RR, 1=RNR, 2=REJ). Do NOT label every
                // S-frame "RR" — that hid 100's REJ (ctrl 0xC9) as an "RR" in the raw log.
                string s = ((ctrl >> 2) & 3) switch { 0 => "RR", 1 => "RNR", 2 => "REJ", _ => "S?" };
                kind = $"{s} nr={(ctrl >> 5) & 7}";
            }
            else
            {
                kind = ctrl switch { 0x3F => "SABM", 0x73 => "UA", 0x53 => "DISC", 0x1F => "DM", 0x87 => "FRMR", _ => $"U 0x{ctrl:X2}" };
            }
            string extra = (ctrl & 1) == 0 ? $" body={Convert.ToHexString(body)}" : string.Empty;
            Console.WriteLine($"[TX] a=0x{(body.Length > 0 ? body[0] : 0):X2} {kind} state={link.State}{extra}");
        };

        LiveNode live = new LiveNode(transport, link, xnode);
        link.Connect(0);
        Console.WriteLine("[runner] SABM sent; pumping legacy link...");
        // No periodic keepalive (matches the original validated runner) — reactive RR only.
        // No timers ON PURPOSE, and now said out loud: the original validated legacy runner emitted
        // no periodic keepalive and answered RR only when spoken to. Reproducing that exactly is
        // the point of this path, so it is the one caller that genuinely wants the no-timer pump.
        await live.RunWithoutTimersAsync(token);
    }
}
