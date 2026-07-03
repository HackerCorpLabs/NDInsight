using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;

using NDInsight.Sintran.Xmsg.Hdlc;

namespace NDInsight.Sintran.Xmsg.Live.Seam
{
    /// <summary>
    /// An <see cref="ILink"/> over the proven HDLC/LAPB stack: wraps an <see cref="IByteDuplex"/>
    /// transport and a <see cref="LapbLink"/>, runs the receive→deframe→FCS→LAPB pump, delivers each
    /// in-order information field UP as <see cref="PayloadReceived"/>, and turns
    /// <see cref="SendSintranFrame"/> into a LAPB I-frame DOWN. Bound to XMSG.
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
    /// The HDLC/LAPB transport is common to X.25 and XMSG; this instance is <em>bound</em> to
    /// <see cref="LinkBinding.Xmsg"/>, so <see cref="SendX25Packet"/> throws. An X.25 machine would
    /// bind the same adapter to <see cref="LinkBinding.X25"/>. The binding is config, not detection.
    /// </para>
    /// </remarks>
    public sealed class LapbLinkAdapter : ILink
    {
        /// <summary>The HDLC flag / frame-delimiter byte.</summary>
        private const byte Flag = 0x7E;

        private readonly string _linkId;
        private readonly IByteDuplex _transport;
        private readonly LapbLink _link;
        private readonly LinkBinding _binding;

        private readonly List<byte> _frameAccumulator;
        private readonly Queue<byte[]> _pendingWrites;

        private long _ticks;
        private LinkStatus _status;
        private CancellationTokenSource? _cts;
        private Task? _pump;

        /// <inheritdoc />
        public event ILink.LinkPayloadReceived? PayloadReceived;

        /// <inheritdoc />
        public event ILink.LinkStatusChanged? StatusChanged;

        /// <summary>
        /// Initialises the adapter over a transport and LAPB link.
        /// </summary>
        /// <param name="linkId">The link identity stamped on every up-event.</param>
        /// <param name="transport">The raw-byte transport (TCP bridge or in-memory).</param>
        /// <param name="link">The LAPB link state machine.</param>
        /// <param name="binding">The L3 protocol this link carries; defaults to XMSG.</param>
        /// <exception cref="ArgumentNullException">Thrown when any reference argument is null.</exception>
        public LapbLinkAdapter(string linkId, IByteDuplex transport, LapbLink link, LinkBinding binding = LinkBinding.Xmsg)
        {
            _linkId = linkId ?? throw new ArgumentNullException(nameof(linkId));
            _transport = transport ?? throw new ArgumentNullException(nameof(transport));
            _link = link ?? throw new ArgumentNullException(nameof(link));
            _binding = binding;

            _frameAccumulator = new List<byte>(512);
            _pendingWrites = new Queue<byte[]>();
            _status = LinkStatus.Down;

            _link.OnTransmit += EnqueueTransmit;
            _link.OnInformation += DeliverPayload;
        }

        /// <inheritdoc />
        public string LinkId
        {
            get { return _linkId; }
        }

        /// <inheritdoc />
        public LinkBinding Binding
        {
            get { return _binding; }
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
        public void Start()
        {
            // Live start: initiate LAPB establishment, then pump forever with an RR keepalive so the
            // peer keeps the link in RUN. Deterministic tests use Initiate()+RunAsync directly.
            _cts = new CancellationTokenSource();
            Initiate();
            _pump = RunAsync(_cts.Token, TimeSpan.FromSeconds(1));
        }

        /// <inheritdoc />
        public void Stop()
        {
            _cts?.Cancel();
        }

        /// <summary>
        /// Initiates LAPB establishment (sends our SABM); the pump flushes it on its next cycle.
        /// </summary>
        public void Initiate()
        {
            _link.Connect(_ticks);
        }

        /// <summary>
        /// Runs the receive/respond pump until end of stream or cancellation. Exposed so a
        /// deterministic test can drive it over an <see cref="InMemoryDuplex"/> without a background
        /// task; <see cref="Start"/> uses it live with a keepalive interval.
        /// </summary>
        /// <param name="cancellationToken">A token that stops the pump.</param>
        /// <param name="keepaliveInterval">
        /// When non-null, the idle interval after which an RR keepalive + retransmit tick fires
        /// (required live); when null the pump simply blocks on reads (in-memory tests).
        /// </param>
        /// <returns>A task that completes when the pump stops.</returns>
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

                    _ticks++;
                    _link.Tick(_ticks);
                }
                else if (!cancellationToken.IsCancellationRequested)
                {
                    _ticks++;
                    _link.Tick(_ticks);
                    _link.SendKeepalive();
                }

                await FlushPendingAsync(cancellationToken);
                RaiseStatusIfChanged();
            }

            await FlushPendingAsync(cancellationToken);
            RaiseStatusIfChanged();
        }

        /// <inheritdoc />
        public void SendSintranFrame(ReadOnlySpan<byte> infoField)
        {
            if (_binding != LinkBinding.Xmsg)
            {
                throw new InvalidOperationException(
                    "This link is bound to X.25; use SendX25Packet.");
            }

            _ticks++;
            // Emits the I-frame via LapbLink.OnTransmit -> EnqueueTransmit; the pump flushes it.
            _link.SendInformation(infoField, _ticks);
        }

        /// <inheritdoc />
        public void SendX25Packet(ReadOnlySpan<byte> packet)
        {
            // Bound to XMSG: an X.25 packet cannot be sent on this link. A link on an ND machine
            // running X.25 software would bind LinkBinding.X25 and implement this instead.
            throw new InvalidOperationException(
                "This link is bound to XMSG; use SendSintranFrame.");
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

                LapbFrame frame = new LapbFrame(default, frameBytes);
                _link.OnFrameReceived(frame);
            }
        }

        /// <summary>
        /// Raises the up-event for an in-order information field delivered by the LAPB link.
        /// </summary>
        private void DeliverPayload(ReadOnlyMemory<byte> info)
        {
            PayloadReceived?.Invoke(_linkId, info, info.Length);
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
        /// transition (Down↔Up), so the layer above learns the pipe became usable / unusable.
        /// </summary>
        private void RaiseStatusIfChanged()
        {
            LinkStatus mapped = _link.State == LapbLinkState.Connected ? LinkStatus.Up : LinkStatus.Down;
            if (mapped != _status)
            {
                _status = mapped;
                StatusChanged?.Invoke(_linkId, mapped);
            }
        }
    }
}
