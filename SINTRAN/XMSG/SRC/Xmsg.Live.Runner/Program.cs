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
using NDInsight.Sintran.Xmsg.Live.Seam;     // LapbLayerAdapter (the concrete ILink over HDLC/LAPB)
using NDInsight.Sintran.Xmsg.Servers.Tad;   // TadServer, TadUser, TadUserDirectory
using NDInsight.Sintran.Xmsg.Node;          // XmsgNode, FileResponderSequenceStore (portable half)
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
                await RunSeamAsync(transport, host, port, node, routingEntries, topology?.Motd, BuildTadUsers(topology), cts.Token);
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
    private static async Task RunSeamAsync(
        TcpBridgeTransport transport, string host, int port, ushort node,
        IReadOnlyList<RoutingTableEntry> routingEntries, string? motdLine, TadUserDirectory users, CancellationToken token)
    {
        string linkId = $"hdlc:{host}:{port}";

        LapbLayer link = new LapbLayer(node);
        LapbLayerAdapter adapter = new LapbLayerAdapter(linkId, transport, link);

        // The per-link-binding "detector" (seam stub): this link carries XMSG, decided by config
        // (the ND machine's installed SW), NOT by sniffing bytes. See XMSG-TRANSPORT-SEAM-PLAN.md §5.
        BoundProtocolDetector detector = new BoundProtocolDetector(LinkBinding.Xmsg);

        // Codec sends down through the link; the layer sits above the codec.
        LinkXmsgTransport codecTransport = new LinkXmsgTransport(adapter);
        XmsgCodec codec = new XmsgCodec(linkId, codecTransport);
        XmsgLayer layer = new XmsgLayer(codec, node, 0x00);

        // Same service configuration as the proven legacy node.
        layer.AcknowledgeData = false;
        // Secure-ACK (subtype 0x03) each incoming session data frame. REQUIRED for the multi-chunk
        // terminal-output handshake (TAD-Message-Formats.md 22.6): the host streams <=2 output BDATs then
        // waits for 100's ACKs, and MUST secure-ACK the 7DUMM/data frames 100 sends between output pairs.
        // Without this the framework dispatch never built an ACK (the gate was off), so long output never
        // completed the handshake and 100 discarded the continuation chunk.
        layer.AcknowledgeTadFrames = true;
        layer.RoutingTable = new InMemoryRoutingTable(routingEntries);
        // Persist our outgoing datagram sequence per remote node across restarts (a state file next
        // to the runner), so we continue in step with 100's persistent expected-from-us instead of
        // resetting to 0x0000 and being silently dropped. See XMSG-SEQUENCE-RESTART-ANSWER doc.
        string statePath = System.IO.Path.Combine(AppContext.BaseDirectory, "xmsg-sequence.state");
        FileResponderSequenceStore sequenceStore = new FileResponderSequenceStore(statePath);
        Console.WriteLine($"[runner] datagram-sequence state file: {statePath}");
        // FRAMEWORK PATH (Phase 1): an XmsgServerHost owns the per-link sequencing (seed + continuous
        // Flags1, from the persistent store) and dispatches server traffic to the registered servers;
        // the *TADADM TAD server answers connect-to. This replaces the single-session TadTerminalResponder
        // - the node secure-ACKs each server frame via the closed-form model.
        NDInsight.Sintran.Xmsg.Node.Services.XmsgServerHost serverHost =
            new NDInsight.Sintran.Xmsg.Node.Services.XmsgServerHost(node, sequenceStore);
        // The MOTD banner middle line comes from the topology file when set; otherwise the server uses its
        // built-in "Emulated TAD server version vN.N.N". The host-id line is generated from our own node.
        NDInsight.Sintran.Xmsg.Servers.Tad.TadServer tadServer =
            new NDInsight.Sintran.Xmsg.Servers.Tad.TadServer(() => DateTime.Now, users: users, motdLine: motdLine);
        Console.WriteLine($"[tad] {users.Count} login account(s) loaded");
        tadServer.SessionOpened += (tadNumber, clientSystem) =>
            Console.WriteLine($"[tad] session opened: tty{tadNumber} from node {clientSystem}");
        tadServer.SessionClosed += (tadNumber, clientSystem) =>
            Console.WriteLine($"[tad] session closed: tty{tadNumber} from node {clientSystem}");
        // Wire the introspection commands: "list servers" reads the host's registered servers; "list route"
        // formats the routing table this node advertises.
        tadServer.ServerDirectory = serverHost.DescribeServers;
        tadServer.RouteReport = () => FormatRouteReport(routingEntries);
        serverHost.Register(tadServer);
        layer.ServerHost = serverHost;

        // UP wiring: a delivered link payload is classified, then (for XMSG) parsed by the codec,
        // which raises PacketReceived to the layer. An X.25-bound link would route elsewhere here.
        adapter.PayloadReceived += delegate (ILink deliveringLink, byte[] payload, int length)
        {
            ReadOnlySpan<byte> span = payload.AsSpan(0, length);
            LinkBinding binding = detector.Classify(deliveringLink.Name, span);
            if (binding == LinkBinding.Xmsg)
            {
                codec.ProcessBytes(span);
            }
        };

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
                    $"  XMCSM=0x{f.SubHeader.ControlService:X8} role=0x{f.SubHeader.Role:X2}");
                Console.Write(NDInsight.Sintran.Xmsg.Diagnostics.XmsgDump.ToText(f));
            }
        };

        layer.SessionOpened += delegate (string id, ushort clientSystem, ushort clientPort)
        {
            Console.WriteLine($"[session] opened by system {clientSystem} port 0x{clientPort:X4}");
        };
        adapter.StatusChanged += delegate (ILink changedLink, LinkStatus oldStatus, LinkStatus newStatus, string reason)
        {
            Console.WriteLine($"[link] {changedLink.Name} status {oldStatus} -> {newStatus} ({reason})");
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
            System.Collections.Generic.IReadOnlyList<XmsgFrame> pumped = serverHost.DrainPending();
            for (int i = 0; i < pumped.Count; i++)
            {
                codec.SendPacket(new XmsgPacket(pumped[i]));
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
    /// returns the first <c>System &gt;= query</c>, which is the Via-100 entry — so 100's <c>li-rout</c>
    /// reports <c>*-&gt;102-&gt;100-&gt;102...  *Loop suspected*</c> and connect-to routing breaks.
    /// Rules:
    ///  - Our own <paramref name="node"/> is ALWAYS Local (0 hops); never advertise a route to ourselves via anyone.
    ///  - 100 is the direct HDLC link peer (Neighbour), unless we ourselves are 100.
    ///  - 102 is the TAD responder reached through 100 (Via, 2 hops), unless we ourselves are 102.
    /// </remarks>
    /// <summary>
    /// Formats the routing table for the TAD "list route" command: one line per system with its
    /// connection type and hop count.
    /// </summary>
    /// <param name="entries">
    /// The routing-table entries this node advertises.
    /// </param>
    /// <returns>
    /// The route report text (one line per entry).
    /// </returns>
    private static string FormatRouteReport(IReadOnlyList<RoutingTableEntry> entries)
    {
        System.Text.StringBuilder sb = new System.Text.StringBuilder(256);
        sb.Append("  System  Route       Hops\r\n");
        for (int i = 0; i < entries.Count; i++)
        {
            RoutingTableEntry entry = entries[i];
            sb.Append("  ").Append(entry.System.ToString().PadRight(8))
              .Append(entry.ConnectionType.ToString().PadRight(12))
              .Append(entry.Hops).Append("\r\n");
        }

        return sb.ToString();
    }

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
        await live.RunAsync(token);
    }
}
