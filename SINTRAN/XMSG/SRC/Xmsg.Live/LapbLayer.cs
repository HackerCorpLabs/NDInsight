using System;
using System.Collections.Generic;

using NDInsight.Sintran.Xmsg.Hdlc;

namespace NDInsight.Sintran.Xmsg.Live
{
    /// <summary>
    /// A driven, modulo-8 LAPB Asynchronous Balanced Mode link state machine conforming to the
    /// authoritative ND LAPB spec (sections 3-7): balanced bring-up, a full transmit window with
    /// go-back-N, cumulative acknowledgement, RNR flow control, FRMR error reporting, and the
    /// T1/T3/N2 timers.
    /// </summary>
    /// <remarks>
    /// <para><b>Time model</b></para>
    /// Time is injected as an opaque monotonic value through the clocked methods
    /// (<see cref="Connect(long)"/>, <see cref="OnFrameReceived(LapbFrame, long)"/>,
    /// <see cref="SendInformation(ReadOnlySpan{byte}, long)"/>, <see cref="Tick(long)"/>). The link
    /// never reads a wall clock, so replay and unit tests are fully deterministic. The timer
    /// durations in <see cref="LapbOptions"/> are expressed in this same unit.
    /// <para><b>Payload independence (spec 1.1)</b></para>
    /// The information field is opaque: this layer never parses it and holds no X.25/XMSG type. It
    /// delivers received I-frame payloads through <see cref="OnInformation"/> and accepts payloads to
    /// send through <see cref="SendInformation(ReadOnlySpan{byte}, long)"/>.
    /// <para><b>No sequence adoption (spec 3.2)</b></para>
    /// On every SABM (our SABM acknowledged by UA, or any SABM received including mid-session), the
    /// sequence variables are hard-zeroed and the retransmit queue is cleared. The link NEVER
    /// initialises V(R)/V(S) from a peer frame's N(S)/N(R).
    /// </remarks>
    public sealed class LapbLayer
    {
        /// <summary>
        /// LAPB address for link-management frames: SABM, UA, DISC, DM, FRMR (spec 2.1).
        /// </summary>
        public const byte AddressLinkSetup = 0x01;

        /// <summary>
        /// LAPB address for data-transfer frames: I, RR, RNR, REJ (spec 2.1).
        /// </summary>
        public const byte AddressData = 0x09;

        /// <summary>
        /// Maximum information-field length; a longer received I-field is FRMR reason Y (spec 2.3.2).
        /// </summary>
        public const int MaxInformationLength = 312;

        // Unnumbered control base patterns (P/F bit 0x10 cleared), spec 2.2.3.
        private const byte SabmBase = 0x2F;
        private const byte DiscBase = 0x43;
        private const byte UaBase = 0x63;
        private const byte DmBase = 0x0F;
        private const byte FrmrBase = 0x87;

        // Supervisory low-nibble subtypes, spec 2.2.2.
        private const byte RrNibble = 0x01;
        private const byte RnrNibble = 0x05;
        private const byte RejNibble = 0x09;

        // The poll/final bit shared by all frame families (spec 2.2).
        private const byte PollFinalBit = 0x10;

        // FRMR reason bits (spec 2.3.3), OR-combined into the diagnostic's third byte.
        private const byte FrmrReasonW = 0x01;   // control field invalid / not implemented
        private const byte FrmrReasonY = 0x04;   // I-field length exceeded the maximum
        private const byte FrmrReasonZ = 0x08;   // N(R) invalid (outside [V(A), V(S)])

        private readonly ushort _ownNode;
        private readonly LapbOptions _options;

        private LapbLayerState _state;

        // Sequence variables, modulo 8 (spec 4.1).
        private int _sendVariable;      // V(S): next I-frame to send
        private int _receiveVariable;   // V(R): next in-sequence I-frame expected
        private int _acknowledgeVariable; // V(A): oldest unacknowledged I-frame

        // Transmit window (spec 4.2): the payload sent (or queued to be sent) with each N(S), indexed
        // by sequence number. A slot is "outstanding" while its index is in [V(A), V(S)). Storing the
        // payload (not the framed body) lets a retransmission carry the CURRENT N(R) as its ack.
        private readonly byte[]?[] _txInfo = new byte[]?[8];

        // Payloads accepted from the upper layer that do not yet fit the window (spec 4.2).
        private readonly Queue<byte[]> _pending = new Queue<byte[]>();

        // Flow-control / reject flags within CONNECTED (spec 6.1).
        private bool _peerBusy;         // set by RNR, cleared by RR/REJ
        private bool _rejectCondition;  // one REJ per detected gap (spec 4.5)

        // The physical neighbour id learned from the peer's SABM/UA/RR info (spec 3.3), read-only and
        // NEVER a routing key. -1 until learned.
        private int _peerNode = -1;

        // Timers and retry counter (spec 5). Deadlines are in the injected-clock unit.
        private bool _t1Running;
        private long _t1Deadline;
        private bool _t3Running;
        private long _t3Deadline;
        private int _retry;

        // The last FRMR diagnostic sent, retained so FRMR_SENT can resend it (spec 5.1 / 6.3).
        private byte[]? _lastFrmrInfo;

        /// <summary>
        /// Represents the method that transmits a LAPB frame body for HDLC encoding.
        /// </summary>
        /// <param name="lapbBody">
        /// The unstuffed LAPB body (address, control and information, without FCS).
        /// </param>
        public delegate void LapbTransmit(byte[] lapbBody);

        /// <summary>
        /// Represents the method that receives an in-order information field.
        /// </summary>
        /// <param name="info">
        /// The information field of the received I-frame (opaque upper-layer payload).
        /// </param>
        public delegate void InformationReceived(ReadOnlyMemory<byte> info);

        /// <summary>
        /// Occurs when a LAPB frame body must be transmitted.
        /// </summary>
        public event LapbTransmit? OnTransmit;

        /// <summary>
        /// Occurs when an in-order information field is delivered to the upper layer.
        /// </summary>
        public event InformationReceived? OnInformation;

        /// <summary>
        /// Initialises a new link for a given node number.
        /// </summary>
        /// <param name="ownNode">
        /// This node's number, stamped as the info field of link-management and supervisory frames.
        /// </param>
        /// <param name="options">
        /// The timer and window configuration; defaults to <see cref="LapbOptions.Default"/>.
        /// </param>
        public LapbLayer(ushort ownNode, LapbOptions? options = null)
        {
            _ownNode = ownNode;
            _options = options ?? LapbOptions.Default;
            _state = LapbLayerState.Disconnected;
        }

        /// <summary>
        /// Gets this node's number.
        /// </summary>
        public ushort OwnNode
        {
            get { return _ownNode; }
        }

        /// <summary>
        /// Gets the physical neighbour id learned from the peer, or -1 if not yet learned.
        /// </summary>
        /// <remarks>
        /// Read-only and NEVER used as a routing key (spec 3.3): on relayed traffic the logical
        /// source differs from the LAPB neighbour.
        /// </remarks>
        public int PeerNode
        {
            get { return _peerNode; }
        }

        /// <summary>
        /// Gets the current link state.
        /// </summary>
        public LapbLayerState State
        {
            get { return _state; }
        }

        /// <summary>
        /// Gets the send sequence variable V(S) (modulo 8).
        /// </summary>
        public int SendVariable
        {
            get { return _sendVariable; }
        }

        /// <summary>
        /// Gets the receive sequence variable V(R) (modulo 8).
        /// </summary>
        public int ReceiveVariable
        {
            get { return _receiveVariable; }
        }

        /// <summary>
        /// Gets the acknowledge sequence variable V(A) (modulo 8): the oldest unacknowledged I-frame.
        /// </summary>
        public int AcknowledgeVariable
        {
            get { return _acknowledgeVariable; }
        }

        /// <summary>
        /// Gets a value indicating whether the peer has signalled busy (RNR); new I-frames are held.
        /// </summary>
        public bool PeerBusy
        {
            get { return _peerBusy; }
        }

        /// <summary>
        /// Gets a value indicating whether a reject condition is set (one REJ sent per gap).
        /// </summary>
        public bool RejectCondition
        {
            get { return _rejectCondition; }
        }

        /// <summary>
        /// Gets the number of unacknowledged I-frames currently outstanding, <c>(V(S) - V(A)) mod 8</c>.
        /// </summary>
        public int Outstanding
        {
            get { return (_sendVariable - _acknowledgeVariable) & 0x07; }
        }

        /// <summary>
        /// Initiates the link by transmitting our SABM (P=1) carrying this node's number.
        /// </summary>
        /// <param name="currentTicks">
        /// The current injected clock value, used to arm the T1 timer.
        /// </param>
        /// <remarks>
        /// Balanced bring-up (spec 3.1): each station sends its own SABM on start; the peer's SABM is
        /// answered with UA in <see cref="OnFrameReceived(LapbFrame, long)"/>.
        /// </remarks>
        public void Connect(long currentTicks)
        {
            Reset();
            _retry = 0;
            _state = LapbLayerState.SabmSent;
            SendUnnumbered(SabmBase, includeNode: true, pollFinal: true);
            StartT1(currentTicks);
        }

        /// <summary>
        /// Requests an orderly disconnect by transmitting DISC (P=1).
        /// </summary>
        /// <param name="currentTicks">
        /// The current injected clock value, used to arm the T1 timer.
        /// </param>
        public void Disconnect(long currentTicks)
        {
            if (_state == LapbLayerState.Disconnected || _state == LapbLayerState.DiscSent)
            {
                return;
            }

            _retry = 0;
            _state = LapbLayerState.DiscSent;
            SendUnnumbered(DiscBase, includeNode: true, pollFinal: true);
            StartT1(currentTicks);
        }

        /// <summary>
        /// Processes one received, FCS-valid LAPB frame and updates the link accordingly.
        /// </summary>
        /// <param name="frame">
        /// The received frame, already de-framed and FCS-checked.
        /// </param>
        /// <param name="currentTicks">
        /// The current injected clock value; frame arrival (re)starts or stops the timers.
        /// </param>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="frame"/> is null.
        /// </exception>
        public void OnFrameReceived(LapbFrame frame, long currentTicks)
        {
            if (frame == null)
            {
                throw new ArgumentNullException(nameof(frame));
            }

            if (frame.Kind == LapbFrameKind.Unnumbered)
            {
                HandleUnnumbered(frame, currentTicks);
                return;
            }

            // I- and S-frames are only processed in CONNECTED (spec 6.3). In DISCONNECTED they draw a
            // DM; in SABM_SENT / DISC_SENT they are ignored; in FRMR_SENT they trigger a FRMR resend.
            switch (_state)
            {
                case LapbLayerState.Disconnected:
                    SendUnnumbered(DmBase, includeNode: true, pollFinal: frame.PollFinal);
                    return;

                case LapbLayerState.SabmSent:
                case LapbLayerState.DiscSent:
                    return;

                case LapbLayerState.FrmrSent:
                    ResendFrmr(currentTicks);
                    return;
            }

            if (frame.Kind == LapbFrameKind.Information)
            {
                HandleInformation(frame, currentTicks);
            }
            else
            {
                HandleSupervisory(frame, currentTicks);
            }
        }

        /// <summary>
        /// Queues an information field for transmission and sends it within the window.
        /// </summary>
        /// <param name="info">
        /// The information field to send (opaque upper-layer payload).
        /// </param>
        /// <param name="currentTicks">
        /// The current injected clock value, used to arm the T1 timer.
        /// </param>
        /// <exception cref="InvalidOperationException">
        /// Thrown when the link is not <see cref="LapbLayerState.Connected"/>.
        /// </exception>
        /// <remarks>
        /// The payload is copied into the link's own storage before returning, so the caller's buffer
        /// (which may be pooled or reused) is free once the call returns. If the window is full or the
        /// peer is busy the frame waits in the pending queue and is sent as the window opens (spec 4.2).
        /// </remarks>
        public void SendInformation(ReadOnlySpan<byte> info, long currentTicks)
        {
            if (_state != LapbLayerState.Connected)
            {
                throw new InvalidOperationException("Cannot send information before the link is connected.");
            }

            _pending.Enqueue(info.ToArray());
            TransmitPending(currentTicks);
        }

        /// <summary>
        /// Advances the injected clock, driving the T1 (retransmission/poll) and T3 (keepalive) timers.
        /// </summary>
        /// <param name="currentTicks">
        /// The current injected clock value.
        /// </param>
        /// <returns>
        /// <c>true</c> when a timer fired and emitted a frame or a state change; otherwise <c>false</c>.
        /// </returns>
        public bool Tick(long currentTicks)
        {
            bool acted = false;

            if (_t1Running && currentTicks >= _t1Deadline)
            {
                acted = OnT1Expiry(currentTicks);
                if (acted && _state == LapbLayerState.Disconnected)
                {
                    return true;
                }
            }

            if (_t3Running && currentTicks >= _t3Deadline && _state == LapbLayerState.Connected)
            {
                // Idle keepalive (spec 5.2): poll the peer, reset the retry counter, await the response.
                EmitSupervisory(RrNibble, pollFinal: true);
                _retry = 0;
                StartT1(currentTicks);
                acted = true;
            }

            return acted;
        }

        /// <summary>
        /// Emits a Receiver-Ready keepalive when the link is connected (legacy idle-driver hook).
        /// </summary>
        /// <remarks>
        /// Retained for the current adapter loop. The conformant keepalive is the T3 timer in
        /// <see cref="Tick(long)"/>; this sends an RR carrying the current V(R) so an idle peer keeps
        /// the link in RUN.
        /// </remarks>
        public void SendKeepalive()
        {
            if (_state == LapbLayerState.Connected)
            {
                EmitSupervisory(RrNibble, pollFinal: false);
            }
        }

        /// <summary>
        /// Handles a received unnumbered frame (SABM / UA / DISC / DM / FRMR).
        /// </summary>
        /// <param name="frame">
        /// The received unnumbered frame.
        /// </param>
        /// <param name="currentTicks">
        /// The current injected clock value.
        /// </param>
        private void HandleUnnumbered(LapbFrame frame, long currentTicks)
        {
            switch (frame.UnnumberedKind)
            {
                case LapbUnnumberedKind.Sabm:
                    // rx SABM in ANY state (spec 6.3): answer UA (F=1) with our node, HARD-RESET the
                    // sequence state and queue (spec 3.2, NO adoption), and enter CONNECTED. A SABM
                    // received mid-session is a legitimate reset, NOT an error (the highest-risk ND
                    // deviation): stay connected.
                    CaptureNode(frame.Info);
                    SendUnnumbered(UaBase, includeNode: true, pollFinal: true);
                    Reset();
                    _state = LapbLayerState.Connected;
                    StartT3(currentTicks);
                    StopT1();
                    // Push the peer to the RUN phase: it advances only once it receives an RR from us
                    // (spec 3.4 idle keepalive; live-necessary against the ND kernel).
                    EmitSupervisory(RrNibble, pollFinal: false);
                    break;

                case LapbUnnumberedKind.Ua:
                    CaptureNode(frame.Info);   // UA carries the peer's node number (spec 2.3.1)
                    if (_state == LapbLayerState.SabmSent)
                    {
                        // Our SABM was accepted: reset and go CONNECTED (spec 3.2).
                        Reset();
                        _state = LapbLayerState.Connected;
                        StartT3(currentTicks);
                        StopT1();
                        EmitSupervisory(RrNibble, pollFinal: false);
                    }
                    else if (_state == LapbLayerState.DiscSent)
                    {
                        _state = LapbLayerState.Disconnected;
                        StopAllTimers();
                    }

                    break;

                case LapbUnnumberedKind.Disc:
                    CaptureNode(frame.Info);   // DISC may carry the peer's node number (spec 2.3.1)
                    if (_state == LapbLayerState.Connected || _state == LapbLayerState.FrmrSent)
                    {
                        SendUnnumbered(UaBase, includeNode: true, pollFinal: frame.PollFinal);
                        _state = LapbLayerState.Disconnected;
                        StopAllTimers();
                    }
                    else
                    {
                        SendUnnumbered(DmBase, includeNode: true, pollFinal: frame.PollFinal);
                    }

                    break;

                case LapbUnnumberedKind.Dm:
                    CaptureNode(frame.Info);   // DM may carry the peer's node number (spec 2.3.1)
                    // A DM tears the link down whether it answers our SABM (fail) or arrives while
                    // connected (spec 6.3).
                    if (_state == LapbLayerState.SabmSent || _state == LapbLayerState.Connected)
                    {
                        _state = LapbLayerState.Disconnected;
                        StopAllTimers();
                    }

                    break;

                case LapbUnnumberedKind.Frmr:
                    // The peer rejected one of our frames: re-establish by sending a fresh SABM.
                    if (_state == LapbLayerState.Connected)
                    {
                        Reset();
                        SendUnnumbered(SabmBase, includeNode: true, pollFinal: true);
                        _retry = 0;
                        _state = LapbLayerState.SabmSent;
                        StartT1(currentTicks);
                    }

                    break;
            }
        }

        /// <summary>
        /// Handles a received supervisory frame (RR / RNR / REJ) while connected.
        /// </summary>
        /// <param name="frame">
        /// The received supervisory frame.
        /// </param>
        /// <param name="currentTicks">
        /// The current injected clock value.
        /// </param>
        private void HandleSupervisory(LapbFrame frame, long currentTicks)
        {
            CaptureNode(frame.Info);

            if (!AcknowledgeThrough(frame.ReceiveSequence, frame.Control, currentTicks))
            {
                return;   // invalid N(R): FRMR(Z) sent, now FRMR_SENT
            }

            switch (frame.SupervisoryKind)
            {
                case LapbSupervisoryKind.ReceiveReady:
                    _peerBusy = false;
                    break;

                case LapbSupervisoryKind.ReceiveNotReady:
                    _peerBusy = true;
                    break;

                case LapbSupervisoryKind.Reject:
                    _peerBusy = false;
                    GoBackN(frame.ReceiveSequence, currentTicks);
                    break;

                default:
                    // An unknown S-subtype is an unimplemented control field: FRMR reason W (spec 2.3.3).
                    SendFrmr(frame.Control, FrmrReasonW, frame.PollFinal, currentTicks);
                    return;
            }

            if (frame.PollFinal)
            {
                // P=1 MUST be answered with F=1 carrying the current N(R) (spec 4.8).
                EmitSupervisory(RrNibble, pollFinal: true);
            }

            if (!_peerBusy)
            {
                TransmitPending(currentTicks);
            }
        }

        /// <summary>
        /// Handles a received information frame: delivers in-order info, or rejects a gap once.
        /// </summary>
        /// <param name="frame">
        /// The received information frame.
        /// </param>
        /// <param name="currentTicks">
        /// The current injected clock value.
        /// </param>
        private void HandleInformation(LapbFrame frame, long currentTicks)
        {
            if (!AcknowledgeThrough(frame.ReceiveSequence, frame.Control, currentTicks))
            {
                return;   // invalid piggybacked N(R): FRMR(Z) sent
            }

            if (frame.Info.Length > MaxInformationLength)
            {
                // Over-long I-field: FRMR reason Y (spec 2.3.2 / 2.3.3).
                SendFrmr(frame.Control, FrmrReasonY, frame.PollFinal, currentTicks);
                return;
            }

            int ns = frame.SendSequence;
            if (ns == _receiveVariable)
            {
                // In-sequence: deliver, advance V(R), clear any reject condition, acknowledge (4.4).
                _receiveVariable = (_receiveVariable + 1) & 0x07;
                _rejectCondition = false;
                OnInformation?.Invoke(frame.Info);
                EmitSupervisory(RrNibble, pollFinal: frame.PollFinal);
            }
            else if (IsDuplicate(ns))
            {
                // Already-accepted frame retransmitted: discard, re-acknowledge current V(R) (4.5).
                EmitSupervisory(RrNibble, pollFinal: frame.PollFinal);
            }
            else
            {
                // Gap: send exactly one REJ and latch the reject condition (4.5), avoiding a REJ storm.
                if (!_rejectCondition)
                {
                    EmitSupervisory(RejNibble, pollFinal: frame.PollFinal);
                    _rejectCondition = true;
                }
            }

            if (!_peerBusy)
            {
                TransmitPending(currentTicks);
            }
        }

        /// <summary>
        /// Applies a cumulative acknowledgement up to (but not including) N(R), validating its range.
        /// </summary>
        /// <param name="receiveSequence">
        /// The N(R) carried by the received frame.
        /// </param>
        /// <param name="rejectedControl">
        /// The control byte of the received frame, echoed into a FRMR diagnostic on an invalid N(R).
        /// </param>
        /// <param name="currentTicks">
        /// The current injected clock value, used to restart or stop the timers.
        /// </param>
        /// <returns>
        /// <c>true</c> when N(R) was valid and applied; <c>false</c> when it was out of range and a
        /// FRMR (reason Z) was sent instead.
        /// </returns>
        private bool AcknowledgeThrough(int receiveSequence, byte rejectedControl, long currentTicks)
        {
            // N(R) must lie in [V(A), V(S)]: (N(R) - V(A)) mod 8 must not exceed the outstanding count
            // (spec 4.3). Anything beyond that acknowledges a frame we never sent.
            int outstanding = (_sendVariable - _acknowledgeVariable) & 0x07;
            if (((receiveSequence - _acknowledgeVariable) & 0x07) > outstanding)
            {
                SendFrmr(rejectedControl, FrmrReasonZ, pollFinal: false, currentTicks);
                return false;
            }

            // Release every acknowledged frame's buffer and advance V(A).
            while (_acknowledgeVariable != receiveSequence)
            {
                _txInfo[_acknowledgeVariable] = null;
                _acknowledgeVariable = (_acknowledgeVariable + 1) & 0x07;
            }

            if (_acknowledgeVariable == _sendVariable)
            {
                // Everything acknowledged: stop the retransmission timer, start the idle keepalive.
                StopT1();
                StartT3(currentTicks);
            }
            else
            {
                RestartT1(currentTicks);
            }

            _retry = 0;
            return true;
        }

        /// <summary>
        /// Retransmits all outstanding I-frames from N(R) onward in response to a REJ (go-back-N).
        /// </summary>
        /// <param name="receiveSequence">
        /// The N(R) the peer rejected from; V(S) is rewound here and the frames replayed in order.
        /// </param>
        /// <param name="currentTicks">
        /// The current injected clock value, used to arm the T1 timer.
        /// </param>
        private void GoBackN(int receiveSequence, long currentTicks)
        {
            // AcknowledgeThrough has already advanced V(A) to N(R); the outstanding frames are
            // N(R)..(V(S)-1). Rewind V(S) to N(R) and resend them, each carrying the current V(R) as
            // its acknowledgement (spec 4.6).
            int resendUpTo = _sendVariable;
            _sendVariable = receiveSequence;
            while (_sendVariable != resendUpTo)
            {
                byte[]? payload = _txInfo[_sendVariable];
                if (payload != null)
                {
                    EmitInformation(_sendVariable, payload);
                }

                _sendVariable = (_sendVariable + 1) & 0x07;
            }

            StartT1(currentTicks);
        }

        /// <summary>
        /// Sends as many pending I-frames as the window and peer-busy state allow.
        /// </summary>
        /// <param name="currentTicks">
        /// The current injected clock value, used to arm the T1 timer on the first send.
        /// </param>
        private void TransmitPending(long currentTicks)
        {
            // Send while connected, the peer is not busy, the window has room, and work is queued (4.2).
            while (_state == LapbLayerState.Connected
                && !_peerBusy
                && ((_sendVariable - _acknowledgeVariable) & 0x07) < _options.WindowSize
                && _pending.Count > 0)
            {
                byte[] payload = _pending.Dequeue();
                _txInfo[_sendVariable] = payload;
                EmitInformation(_sendVariable, payload);
                _sendVariable = (_sendVariable + 1) & 0x07;

                // An I-frame is now outstanding: stop the idle keepalive and start T1 if it was idle
                // (spec 5.1: "start T1 when an I-frame is sent and the queue was previously empty").
                StopT3();
                if (!_t1Running)
                {
                    StartT1(currentTicks);
                }
            }
        }

        /// <summary>
        /// Records the peer's node number (physical neighbour id) from a link/supervisory info field.
        /// </summary>
        /// <param name="info">
        /// The frame's information field; the first two bytes are the big-endian node number if present.
        /// </param>
        private void CaptureNode(ReadOnlyMemory<byte> info)
        {
            // Spec 2.3.1 / 3.3: SABM/UA/RR carry the sender's node number big-endian. Store it read-only
            // as the neighbour id; it is NEVER a routing key.
            ReadOnlySpan<byte> span = info.Span;
            if (span.Length >= 2)
            {
                _peerNode = (span[0] << 8) | span[1];
            }
        }

        /// <summary>
        /// Determines whether an out-of-sequence N(S) is a retransmitted duplicate rather than a gap.
        /// </summary>
        /// <param name="sendSequence">
        /// The received frame's N(S), known to differ from V(R).
        /// </param>
        /// <returns>
        /// <c>true</c> when N(S) is behind V(R) (already accepted); <c>false</c> when it is ahead (a gap).
        /// </returns>
        private bool IsDuplicate(int sendSequence)
        {
            // A gap is the peer running ahead of V(R) by 1..(k-1); at or beyond k round the modulo-8
            // circle the frame is already behind V(R), i.e. a retransmitted duplicate (spec 4.5). With
            // the maximum window k=7 this makes the common "one behind" retransmit (ahead == 7) a
            // duplicate that draws an RR, never a spurious REJ.
            int ahead = (sendSequence - _receiveVariable) & 0x07;
            return ahead >= _options.WindowSize;
        }

        /// <summary>
        /// Handles a T1 expiry: retry, or re-establish/fail on exceeding N2 (spec 5.1).
        /// </summary>
        /// <param name="currentTicks">
        /// The current injected clock value, used to restart T1.
        /// </param>
        /// <returns>
        /// <c>true</c> when the expiry produced a frame or a state change.
        /// </returns>
        private bool OnT1Expiry(long currentTicks)
        {
            _retry++;
            if (_retry > _options.N2)
            {
                // Link failure (spec 5.1 step 1): from CONNECTED or SABM_SENT, auto re-establish with a
                // fresh SABM; otherwise drop to DISCONNECTED.
                StopT3();
                if (_state == LapbLayerState.Connected || _state == LapbLayerState.SabmSent)
                {
                    Reset();
                    SendUnnumbered(SabmBase, includeNode: true, pollFinal: true);
                    _retry = 0;
                    _state = LapbLayerState.SabmSent;
                    StartT1(currentTicks);
                }
                else
                {
                    _state = LapbLayerState.Disconnected;
                    StopT1();
                }

                return true;
            }

            switch (_state)
            {
                case LapbLayerState.Connected:
                    // Poll-first recovery (spec 5.1): RR with P=1; the peer's N(R)/REJ drives go-back-N.
                    EmitSupervisory(RrNibble, pollFinal: true);
                    RestartT1(currentTicks);
                    return true;

                case LapbLayerState.SabmSent:
                    SendUnnumbered(SabmBase, includeNode: true, pollFinal: true);
                    RestartT1(currentTicks);
                    return true;

                case LapbLayerState.DiscSent:
                    SendUnnumbered(DiscBase, includeNode: true, pollFinal: true);
                    RestartT1(currentTicks);
                    return true;

                case LapbLayerState.FrmrSent:
                    ResendFrmr(currentTicks);
                    return true;

                default:
                    StopT1();
                    return false;
            }
        }

        /// <summary>
        /// Builds and sends a FRMR carrying the 3-byte diagnostic, then enters FRMR_SENT.
        /// </summary>
        /// <param name="rejectedControl">
        /// The control byte of the frame that caused the reject.
        /// </param>
        /// <param name="reasonBits">
        /// The OR-combined FRMR reason bits (W / Y / Z).
        /// </param>
        /// <param name="pollFinal">
        /// Whether to set the final bit on the FRMR.
        /// </param>
        /// <param name="currentTicks">
        /// The current injected clock value, used to arm the T1 timer.
        /// </param>
        private void SendFrmr(byte rejectedControl, byte reasonBits, bool pollFinal, long currentTicks)
        {
            // Diagnostic (spec 2.3.3): rejected control; V(S)/V(R) with bit4 = 0; reason nibble.
            byte sequences = (byte)(((_sendVariable << 1) & 0x0E) | ((_receiveVariable << 5) & 0xE0));
            byte[] info = new byte[] { rejectedControl, sequences, reasonBits };
            _lastFrmrInfo = info;

            byte control = (byte)(FrmrBase | (pollFinal ? PollFinalBit : 0));
            byte[] body = new byte[2 + info.Length];
            body[0] = AddressLinkSetup;
            body[1] = control;
            body[2] = info[0];
            body[3] = info[1];
            body[4] = info[2];
            OnTransmit?.Invoke(body);

            _state = LapbLayerState.FrmrSent;
            StartT1(currentTicks);
        }

        /// <summary>
        /// Resends the last FRMR diagnostic (F=1) while in FRMR_SENT and restarts T1.
        /// </summary>
        /// <param name="currentTicks">
        /// The current injected clock value, used to restart T1.
        /// </param>
        private void ResendFrmr(long currentTicks)
        {
            if (_lastFrmrInfo == null)
            {
                return;
            }

            byte[] body = new byte[2 + _lastFrmrInfo.Length];
            body[0] = AddressLinkSetup;
            body[1] = (byte)(FrmrBase | PollFinalBit);
            body[2] = _lastFrmrInfo[0];
            body[3] = _lastFrmrInfo[1];
            body[4] = _lastFrmrInfo[2];
            OnTransmit?.Invoke(body);
            RestartT1(currentTicks);
        }

        /// <summary>
        /// Emits an information frame carrying the given sequence number, the current V(R), and payload.
        /// </summary>
        /// <param name="sendSequence">
        /// The N(S) to stamp (bits 1..3 of the control byte).
        /// </param>
        /// <param name="payload">
        /// The information field bytes.
        /// </param>
        private void EmitInformation(int sendSequence, byte[] payload)
        {
            // I-frame control (spec 2.2.1): N(R) in bits 5..7, P=0, N(S) in bits 1..3, bit 0 = 0.
            byte control = (byte)((_receiveVariable << 5) | (sendSequence << 1));
            byte[] body = new byte[2 + payload.Length];
            body[0] = AddressData;
            body[1] = control;
            Array.Copy(payload, 0, body, 2, payload.Length);
            OnTransmit?.Invoke(body);
        }

        /// <summary>
        /// Emits a supervisory frame (RR / RNR / REJ) carrying the current V(R) and our node number.
        /// </summary>
        /// <param name="subtypeNibble">
        /// The subtype low nibble: RR <c>0x1</c>, RNR <c>0x5</c>, REJ <c>0x9</c>.
        /// </param>
        /// <param name="pollFinal">
        /// Whether to set the poll/final bit.
        /// </param>
        private void EmitSupervisory(byte subtypeNibble, bool pollFinal)
        {
            // S-frame control (spec 2.2.2): N(R) in bits 5..7, P/F, then the subtype low nibble.
            byte control = (byte)((_receiveVariable << 5) | (pollFinal ? PollFinalBit : 0) | subtypeNibble);
            byte[] body = new byte[4];
            body[0] = AddressData;
            body[1] = control;
            body[2] = (byte)((_ownNode >> 8) & 0xFF);
            body[3] = (byte)(_ownNode & 0xFF);
            OnTransmit?.Invoke(body);
        }

        /// <summary>
        /// Emits an unnumbered frame (SABM / UA / DISC / DM), optionally stamping our node number.
        /// </summary>
        /// <param name="controlBase">
        /// The unnumbered control base pattern (P/F bit cleared).
        /// </param>
        /// <param name="includeNode">
        /// Whether to append our 2-byte node number as the info field (spec 2.3.1).
        /// </param>
        /// <param name="pollFinal">
        /// Whether to set the poll/final bit.
        /// </param>
        private void SendUnnumbered(byte controlBase, bool includeNode, bool pollFinal)
        {
            byte control = (byte)(controlBase | (pollFinal ? PollFinalBit : 0));
            if (includeNode)
            {
                byte[] body = new byte[4];
                body[0] = AddressLinkSetup;
                body[1] = control;
                body[2] = (byte)((_ownNode >> 8) & 0xFF);
                body[3] = (byte)(_ownNode & 0xFF);
                OnTransmit?.Invoke(body);
            }
            else
            {
                byte[] body = new byte[2];
                body[0] = AddressLinkSetup;
                body[1] = control;
                OnTransmit?.Invoke(body);
            }
        }

        /// <summary>
        /// Hard-resets the sequence variables, retransmit queue and flow flags (spec 3.2).
        /// </summary>
        private void Reset()
        {
            _sendVariable = 0;
            _receiveVariable = 0;
            _acknowledgeVariable = 0;
            for (int i = 0; i < _txInfo.Length; i++)
            {
                _txInfo[i] = null;
            }

            _pending.Clear();
            _peerBusy = false;
            _rejectCondition = false;
        }

        /// <summary>
        /// Starts the T1 retransmission / poll timer from the given clock value.
        /// </summary>
        /// <param name="currentTicks">
        /// The current injected clock value.
        /// </param>
        private void StartT1(long currentTicks)
        {
            _t1Running = true;
            _t1Deadline = currentTicks + _options.T1;
        }

        /// <summary>
        /// Restarts the T1 timer (equivalent to a fresh start) from the given clock value.
        /// </summary>
        /// <param name="currentTicks">
        /// The current injected clock value.
        /// </param>
        private void RestartT1(long currentTicks)
        {
            StartT1(currentTicks);
        }

        /// <summary>
        /// Stops the T1 timer.
        /// </summary>
        private void StopT1()
        {
            _t1Running = false;
        }

        /// <summary>
        /// Starts the T3 idle keepalive timer from the given clock value.
        /// </summary>
        /// <param name="currentTicks">
        /// The current injected clock value.
        /// </param>
        private void StartT3(long currentTicks)
        {
            _t3Running = true;
            _t3Deadline = currentTicks + _options.T3;
        }

        /// <summary>
        /// Stops the T3 timer.
        /// </summary>
        private void StopT3()
        {
            _t3Running = false;
        }

        /// <summary>
        /// Stops both timers (used on disconnect).
        /// </summary>
        private void StopAllTimers()
        {
            _t1Running = false;
            _t3Running = false;
        }
    }
}
