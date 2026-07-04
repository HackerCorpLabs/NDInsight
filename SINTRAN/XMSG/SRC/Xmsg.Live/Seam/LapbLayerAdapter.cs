using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;

using NDInsight.Sintran.Xmsg.Hdlc;
using NDInsight.Sintran.Xmsg.Node.Seam;   // ILink + seam types now live in the portable Xmsg.Node half

namespace NDInsight.Sintran.Xmsg.Live.Seam
{
    /// <summary>
    /// An <see cref="ILink"/> over the proven HDLC/LAPB stack: wraps an <see cref="IByteDuplex"/>
    /// transport and a <see cref="LapbLayer"/>, runs the receive→deframe→FCS→LAPB pump, delivers each
    /// in-order information field UP as <see cref="PayloadReceived"/>, and turns
    /// <see cref="SendData"/> into a LAPB I-frame DOWN.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This is the seam-shaped sibling of the proven <see cref="LiveNode"/>: it reuses the identical,
    /// live-tested framing (incremental <c>0x7E</c> splitting, <see cref="HdlcDeframer"/>,
    /// <see cref="Fcs16"/> validation, <see cref="HdlcEncoder"/>) but is DECOUPLED from any
    /// application node — the codec/layer above it does the responding via the up-event. The old
    /// <see cref="LiveNode"/> path stays intact until the new path proves live parity (Phase 5).
    /// </para>
    /// <para>
    /// The HDLC/LAPB transport is common to X.25 and XMSG; this adapter carries the payload opaquely
    /// via <see cref="SendData"/> and never classifies it. Which L3 protocol a link carries is the
    /// composition root's concern (see <c>IProtocolDetector</c>), not the link's.
    /// </para>
    /// <para>
    /// <b>Buffer ownership:</b> the <see cref="PayloadReceived"/> payload MAY be a buffer the adapter
    /// reuses; a consumer that retains the bytes beyond the callback MUST copy them within the
    /// callback. (Today the underlying frame array is fresh per receive, so no copy is strictly
    /// required — but consumers must not rely on that.)
    /// </para>
    /// </remarks>
    public sealed class LapbLayerAdapter : ILink
    {
        /// <summary>
        /// The HDLC flag / frame-delimiter byte.
        /// </summary>
        private const byte Flag = 0x7E;

        private readonly string _linkId;
        private readonly IByteDuplex _transport;
        private readonly LapbLayer _link;

        private readonly List<byte> _frameAccumulator;
        private readonly Queue<byte[]> _pendingWrites;

        // Monotonic millisecond clock feeding the LAPB timers (T1/T3/N2). The LAPB layer never reads a
        // wall clock itself (deterministic by design); this adapter, in the replaceable live half,
        // injects real elapsed milliseconds so the spec timers fire on real time. Stopwatch is
        // monotonic and immune to wall-clock adjustments. Frame bytes never depend on this value
        // (only timer deadlines do), so the in-memory parity tests stay deterministic.
        private readonly System.Diagnostics.Stopwatch _clock = System.Diagnostics.Stopwatch.StartNew();
        private LinkStatus _status;
        // True once Stop()/Dispose() has been called. Used to stop the pump's residual LAPB state from
        // resurrecting the link to Active/Starting. We CANNOT use the status enum for this, because the
        // INITIAL status is also Stopped (before Start) — and from there the link legitimately advances.
        private bool _stopRequested;
        private CancellationTokenSource? _cts;
        private Task? _pump;

        /// <summary>
        /// Gets the current monotonic clock value in milliseconds, injected into the LAPB timers.
        /// </summary>
        private long CurrentMillis
        {
            get { return _clock.ElapsedMilliseconds; }
        }

        /// <inheritdoc />
        public event LinkPayloadReceived? PayloadReceived;

        /// <inheritdoc />
        public event LinkStatusChanged? StatusChanged;

        /// <summary>
        /// Diagnostic up-event: every FCS-valid LAPB frame received from the wire (raw body,
        /// before the LAPB state machine processes it). Lets the runner log SABM/UA/RR/I traffic
        /// from the peer — including a bare-link SABM storm when the peer's XMSG has died.
        /// </summary>
        /// <param name="linkId">
        /// The link the frame arrived on (sender-first).
        /// </param>
        /// <param name="frameBytes">
        /// The unstuffed, FCS-valid LAPB frame (addr, control, info, FCS).
        /// </param>
        public delegate void LinkRawFrameReceived(string linkId, byte[] frameBytes);

        /// <summary>
        /// Occurs for every FCS-valid LAPB frame received (diagnostic).
        /// </summary>
        public event LinkRawFrameReceived? RawFrameReceived;

        /// <summary>
        /// Initialises the adapter over a transport and LAPB link.
        /// </summary>
        /// <param name="linkId">
        /// The link identity stamped on every up-event.
        /// </param>
        /// <param name="transport">
        /// The raw-byte transport (TCP bridge or in-memory).
        /// </param>
        /// <param name="link">
        /// The LAPB link state machine.
        /// </param>
        /// <exception cref="ArgumentNullException">
        /// Thrown when any reference argument is null.
        /// </exception>
        public LapbLayerAdapter(string linkId, IByteDuplex transport, LapbLayer link)
        {
            _linkId = linkId ?? throw new ArgumentNullException(nameof(linkId));
            _transport = transport ?? throw new ArgumentNullException(nameof(transport));
            _link = link ?? throw new ArgumentNullException(nameof(link));

            _frameAccumulator = new List<byte>(512);
            _pendingWrites = new Queue<byte[]>();
            _status = LinkStatus.Stopped;

            _link.OnTransmit += EnqueueTransmit;
            _link.OnInformation += DeliverPayload;
        }

        /// <inheritdoc />
        public string Name
        {
            get { return _linkId; }
        }

        /// <inheritdoc />
        public LinkStatus Status
        {
            get { return _status; }
        }

        /// <summary>
        /// Gets the task representing the running receive pump (null until <see cref="Start"/>), so a
        /// test can await pump completion when the transport reaches end of stream.
        /// </summary>
        public Task? Completion
        {
            get { return _pump; }
        }

        /// <inheritdoc />
        public bool Start()
        {
            // Live start: initiate LAPB establishment, then pump forever with an RR keepalive so the
            // peer keeps the link in RUN. Deterministic tests use Initiate()+RunAsync directly.
            _cts = new CancellationTokenSource();
            SetStatus(LinkStatus.Starting, "start requested");
            Initiate();
            _pump = RunAsync(_cts.Token, TimeSpan.FromSeconds(1));
            return true;
        }

        /// <inheritdoc />
        public void Stop()
        {
            // Idempotent: a second Stop (or a Stop after Dispose) is a no-op.
            if (_status == LinkStatus.Stopped || _status == LinkStatus.Stopping)
            {
                return;
            }

            _stopRequested = true;
            SetStatus(LinkStatus.Stopping, "stop requested");
            _cts?.Cancel();
            SetStatus(LinkStatus.Stopped, "stopped");
        }

        /// <inheritdoc />
        public void Dispose()
        {
            // Dispose implies Stop. We only CANCEL the pump (cooperative) and never await it here, so
            // Dispose can never hang on the receive pump. The token source is then released.
            Stop();
            _cts?.Dispose();
        }

        /// <summary>
        /// Initiates LAPB establishment (sends our SABM); the pump flushes it on its next cycle.
        /// </summary>
        public void Initiate()
        {
            _link.Connect(CurrentMillis);
        }

        /// <summary>
        /// Runs the receive/respond pump until end of stream or cancellation. Exposed so a
        /// deterministic test can drive it over an <see cref="InMemoryDuplex"/> without a background
        /// task; <see cref="Start"/> uses it live with a keepalive interval.
        /// </summary>
        /// <param name="cancellationToken">
        /// A token that stops the pump.
        /// </param>
        /// <param name="keepaliveInterval">
        /// When non-null, the idle interval after which an RR keepalive + retransmit tick fires
        /// (required live); when null the pump simply blocks on reads (in-memory tests).
        /// </param>
        /// <returns>
        /// A task that completes when the pump stops.
        /// </returns>
        public async Task RunAsync(CancellationToken cancellationToken, TimeSpan? keepaliveInterval = null)
        {
            byte[] buffer = new byte[512];

            // Flush the SABM queued by Initiate() before the first read.
            await FlushPendingAsync(cancellationToken);
            RaiseStatusIfChanged();

            if (keepaliveInterval == null)
            {
                while (!cancellationToken.IsCancellationRequested)
                {
                    int n = await _transport.ReadAsync(buffer, 0, buffer.Length, cancellationToken);
                    if (n <= 0)
                    {
                        break;
                    }

                    for (int i = 0; i < n; i++)
                    {
                        FeedByte(buffer[i]);
                    }

                    await FlushPendingAsync(cancellationToken);
                    RaiseStatusIfChanged();
                }

                await FlushPendingAsync(cancellationToken);
                RaiseStatusIfChanged();
                return;
            }

            // Keepalive loop: race the pending read against an idle timer.
            Task<int>? pendingRead = null;
            while (!cancellationToken.IsCancellationRequested)
            {
                pendingRead ??= _transport.ReadAsync(buffer, 0, buffer.Length, cancellationToken);

                Task completed = await Task.WhenAny(
                    pendingRead, Task.Delay(keepaliveInterval.Value, cancellationToken));

                if (completed == pendingRead)
                {
                    int n = await pendingRead;
                    pendingRead = null;
                    if (n <= 0)
                    {
                        break;
                    }

                    for (int i = 0; i < n; i++)
                    {
                        FeedByte(buffer[i]);
                    }

                    // Drive the LAPB timers from the real monotonic clock after processing inbound data.
                    _link.Tick(CurrentMillis);
                }
                else if (!cancellationToken.IsCancellationRequested)
                {
                    // Idle tick: advance the timers (T1 retransmit/poll, T3 keepalive) and send the
                    // proven per-interval RR keepalive so the ND peer keeps the link in RUN.
                    _link.Tick(CurrentMillis);
                    _link.SendKeepalive();
                }

                await FlushPendingAsync(cancellationToken);
                RaiseStatusIfChanged();
            }

            await FlushPendingAsync(cancellationToken);
            RaiseStatusIfChanged();
        }

        /// <inheritdoc />
        public bool SendData(ReadOnlySpan<byte> payload)
        {
            // Contract: a not-Active link (or an empty payload) is a logged false, NEVER a throw.
            if (_status != LinkStatus.Active)
            {
                Console.WriteLine($"[link] {_linkId} SendData refused: link not Active (status {_status})");
                return false;
            }

            if (payload.Length == 0)
            {
                Console.WriteLine($"[link] {_linkId} SendData refused: empty payload");
                return false;
            }

            // Emits the I-frame via LapbLayer.OnTransmit -> EnqueueTransmit; the pump flushes it.
            // SendInformation cannot retain the span (compiler-enforced) and copies the bytes into the
            // LAPB body it enqueues, so the caller's buffer is free the moment this returns.
            _link.SendInformation(payload, CurrentMillis);
            return true;
        }

        /// <summary>
        /// Feeds one received byte into the incremental HDLC frame splitter.
        /// </summary>
        private void FeedByte(byte value)
        {
            if (value == Flag)
            {
                if (_frameAccumulator.Count > 0)
                {
                    ProcessAccumulatedFrame();
                    _frameAccumulator.Clear();
                }

                // A run of flags (interframe fill) collapses; nothing to do on an empty accumulator.
                return;
            }

            _frameAccumulator.Add(value);
        }

        /// <summary>
        /// Unstuffs and FCS-checks the bytes accumulated between two flags, then dispatches a valid
        /// frame to the LAPB link.
        /// </summary>
        private void ProcessAccumulatedFrame()
        {
            byte[] wrapped = new byte[_frameAccumulator.Count + 2];
            wrapped[0] = Flag;
            for (int i = 0; i < _frameAccumulator.Count; i++)
            {
                wrapped[i + 1] = _frameAccumulator[i];
            }

            wrapped[wrapped.Length - 1] = Flag;

            IReadOnlyList<byte[]> frames = HdlcDeframer.SplitFrames(wrapped);
            for (int i = 0; i < frames.Count; i++)
            {
                byte[] frameBytes = frames[i];
                if (!Fcs16.IsValid(frameBytes))
                {
                    continue;
                }

                // Diagnostic: surface every FCS-valid received frame (SABM/UA/RR/I) before the LAPB
                // state machine consumes it, so the runner can see the peer's raw traffic.
                RawFrameReceived?.Invoke(_linkId, frameBytes);

                LapbFrame frame = new LapbFrame(default, frameBytes);
                _link.OnFrameReceived(frame, CurrentMillis);

                // Refresh coarse status right after each frame so a SABM/UA that brings the LAPB link
                // to Connected flips us to Active BEFORE the very next frame (the peer's first I-frame)
                // delivers its payload — a send issued from that PayloadReceived callback then sees an
                // Active link. Waiting until the end of the read chunk would race that.
                RaiseStatusIfChanged();
            }
        }

        /// <summary>
        /// Raises the up-event for an in-order information field delivered by the LAPB link.
        /// </summary>
        private void DeliverPayload(ReadOnlyMemory<byte> info)
        {
            // The ILink contract hands the payload up as byte[] + length. The frame's info field is
            // already backed by its own array, so surface that array directly (zero-copy) when it is a
            // whole-array segment; only copy on the unusual sliced case. Consumers copy if they retain.
            if (System.Runtime.InteropServices.MemoryMarshal.TryGetArray(info, out ArraySegment<byte> seg)
                && seg.Array != null && seg.Offset == 0 && seg.Array.Length == seg.Count)
            {
                PayloadReceived?.Invoke(this, seg.Array, seg.Count);
            }
            else
            {
                byte[] copy = info.ToArray();
                PayloadReceived?.Invoke(this, copy, copy.Length);
            }
        }

        /// <summary>
        /// HDLC-encodes a LAPB body the link wants to transmit and queues it for writing.
        /// </summary>
        private void EnqueueTransmit(byte[] lapbBody)
        {
            _pendingWrites.Enqueue(HdlcEncoder.Encode(lapbBody));
        }

        /// <summary>
        /// Writes every queued outbound frame to the transport, in order.
        /// </summary>
        private async Task FlushPendingAsync(CancellationToken cancellationToken)
        {
            while (_pendingWrites.Count > 0)
            {
                byte[] frame = _pendingWrites.Dequeue();
                await _transport.WriteAsync(frame, cancellationToken);
            }
        }

        /// <summary>
        /// Maps the LAPB state to the coarse <see cref="LinkStatus"/> and raises StatusChanged on a
        /// transition, so the layer above learns the pipe became usable / unusable. LAPB
        /// <c>Connected</c> maps to <see cref="LinkStatus.Active"/>; anything else while the pump runs
        /// is <see cref="LinkStatus.Starting"/> (establishing or re-establishing).
        /// </summary>
        private void RaiseStatusIfChanged()
        {
            // Once Stop()/Dispose() has been requested, a residual LAPB state left in the pump must not
            // resurrect the link to Active/Starting. (We check the flag, not the status enum, because
            // the initial pre-Start status is also Stopped — and from there we DO advance.)
            if (_stopRequested)
            {
                return;
            }

            bool connected = _link.State == LapbLayerState.Connected;
            LinkStatus mapped = connected ? LinkStatus.Active : LinkStatus.Starting;
            SetStatus(mapped, connected ? "LAPB connected" : "LAPB establishing");
        }

        /// <summary>
        /// Sets the coarse status and raises <see cref="StatusChanged"/> only on an actual transition,
        /// carrying the previous status, the new status, and a reason (sender-first: this link).
        /// </summary>
        /// <param name="next">
        /// The new status.
        /// </param>
        /// <param name="reason">
        /// A short human-readable reason for the transition (for logs).
        /// </param>
        private void SetStatus(LinkStatus next, string reason)
        {
            if (next != _status)
            {
                LinkStatus previous = _status;
                _status = next;
                StatusChanged?.Invoke(this, previous, next, reason);
            }
        }
    }
}
