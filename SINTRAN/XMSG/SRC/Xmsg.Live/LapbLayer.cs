using System;

using NDInsight.Sintran.Xmsg.Hdlc;

namespace NDInsight.Sintran.Xmsg.Live
{
    /// <summary>
    /// A driven, modulo-8 LAPB ABM link state machine. Establishes the balanced link
    /// (SABM/UA), maintains <c>V(S)</c>/<c>V(R)</c>, delivers received information fields,
    /// and emits the LAPB frame bodies to transmit (for <see cref="HdlcEncoder"/>).
    /// </summary>
    /// <remarks>
    /// <para><b>Time model</b></para>
    /// Time is injected as an opaque tick count through <see cref="Tick(long)"/> — the link
    /// never reads a wall clock, so replay and unit tests are fully deterministic.
    /// <para><b>Provenance</b></para>
    /// The control-field encodings (SABM <c>0x3F</c>, UA <c>0x73</c>, RR
    /// <c>0x01 | (N(R) &lt;&lt; 5)</c>, I-frame <c>(N(R) &lt;&lt; 5) | (N(S) &lt;&lt; 1)</c>),
    /// the link-setup address <c>0x01</c> / data address <c>0x09</c>, and the node number
    /// carried as the info field of link-management frames are VERIFIED (XMSG-PROTOCOL.md
    /// section 3, captured traffic). The retransmit timeout and retry budget are INFERRED —
    /// no loss occurs in the corpus — so they are configurable and default to conservative
    /// values.
    /// </remarks>
    public sealed class LapbLayer
    {
        /// <summary>
        /// LAPB address used on link-establishment frames (SABM, UA).
        /// </summary>
        public const byte AddressLinkSetup = 0x01;

        /// <summary>
        /// LAPB address used on data-transfer frames (RR, I-frames).
        /// </summary>
        public const byte AddressData = 0x09;

        /// <summary>
        /// SABM control byte (base <c>0x2F</c> with the poll bit set).
        /// </summary>
        public const byte ControlSabm = 0x3F;

        /// <summary>
        /// UA control byte (base <c>0x63</c> with the final bit set).
        /// </summary>
        public const byte ControlUa = 0x73;

        /// <summary>
        /// RR base control byte; the receive sequence is OR-ed in as <c>N(R) &lt;&lt; 5</c>.
        /// </summary>
        public const byte ControlRrBase = 0x01;

        private readonly ushort _ownNode;

        // INFERRED: retransmit timing / retry budget are not proven by any capture (no loss
        // is present in the 13-capture corpus). Configurable via the constructor.
        private readonly long _retransmitTicks;
        private readonly int _maxRetries;

        private LapbLayerState _state;
        private int _sendVariable;      // V(S)
        private int _receiveVariable;   // V(R)

        private byte[]? _lastUnackedBody;   // last I/SABM body, for retransmit
        private int _lastBehindNr = -1;     // last RR N(R) seen behind V(S), to detect a stuck peer
        private long _lastSendTicks;
        private int _retries;
        private bool _synced;               // have we adopted the peer's sequence yet?
        private bool _ownSabmSent;          // have we already issued OUR SABM this establishment?

        /// <summary>
        /// Raised when the link needs to transmit a LAPB frame body (to be HDLC-encoded).
        /// </summary>
        /// <param name="lapbBody">
        /// The unstuffed LAPB body (address, control and information, without FCS).
        /// </param>
        public delegate void LapbTransmit(byte[] lapbBody);

        /// <summary>
        /// Raised when an information frame delivers its information field in order.
        /// </summary>
        /// <param name="info">
        /// The information field of the received I-frame (the SINTRAN frame bytes).
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
        /// This node's number, carried as the info field of link-management frames.
        /// </param>
        /// <param name="retransmitTicks">
        /// The number of injected ticks after which an unacknowledged SABM/I-frame is
        /// retransmitted. INFERRED default.
        /// </param>
        /// <param name="maxRetries">
        /// The maximum number of retransmissions before the link gives up. INFERRED default.
        /// </param>
        public LapbLayer(ushort ownNode, long retransmitTicks = 30, int maxRetries = 3)
        {
            _ownNode = ownNode;
            _retransmitTicks = retransmitTicks;
            _maxRetries = maxRetries;
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
        /// Gets the current link state.
        /// </summary>
        public LapbLayerState State
        {
            get { return _state; }
        }

        /// <summary>
        /// Gets the send sequence variable <c>V(S)</c> (modulo 8).
        /// </summary>
        public int SendVariable
        {
            get { return _sendVariable; }
        }

        /// <summary>
        /// Gets the receive sequence variable <c>V(R)</c> (modulo 8).
        /// </summary>
        public int ReceiveVariable
        {
            get { return _receiveVariable; }
        }

        /// <summary>
        /// Initiates the link by transmitting a SABM carrying this node's number.
        /// </summary>
        /// <param name="currentTicks">
        /// The current injected tick count, used to arm the retransmit timer.
        /// </param>
        public void Connect(long currentTicks)
        {
            _sendVariable = 0;
            _receiveVariable = 0;
            _retries = 0;
            _synced = false;
            // Fresh establishment episode: allow the reflexive establishment SABM (sent when the
            // peer's SABM arrives) to fire once. This initiator SABM does NOT consume that budget —
            // the peer may miss this one, and the reflexive send is what guarantees it learns us.
            _ownSabmSent = false;
            _state = LapbLayerState.SabmSent;

            byte[] body = BuildUnnumbered(AddressLinkSetup, ControlSabm);
            _lastUnackedBody = body;
            _lastSendTicks = currentTicks;
            OnTransmit?.Invoke(body);
        }

        /// <summary>
        /// Processes one received, FCS-valid LAPB frame and updates the link accordingly.
        /// </summary>
        /// <param name="frame">
        /// The received frame, already de-framed and FCS-checked.
        /// </param>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="frame"/> is null.
        /// </exception>
        public void OnFrameReceived(LapbFrame frame)
        {
            if (frame == null)
            {
                throw new ArgumentNullException(nameof(frame));
            }

            switch (frame.Kind)
            {
                case LapbFrameKind.Unnumbered:
                    HandleUnnumbered(frame);
                    break;

                case LapbFrameKind.Supervisory:
                    HandleSupervisory(frame);
                    break;

                case LapbFrameKind.Information:
                    HandleInformation(frame);
                    break;
            }
        }

        /// <summary>
        /// Transmits an information field as an I-frame, then advances <c>V(S)</c>.
        /// </summary>
        /// <param name="info">
        /// The information field to send (the SINTRAN frame bytes).
        /// </param>
        /// <param name="currentTicks">
        /// The current injected tick count, used to arm the retransmit timer.
        /// </param>
        /// <exception cref="InvalidOperationException">
        /// Thrown when the link is not <see cref="LapbLayerState.Connected"/>.
        /// </exception>
        public void SendInformation(ReadOnlySpan<byte> info, long currentTicks)
        {
            if (_state != LapbLayerState.Connected)
            {
                throw new InvalidOperationException("Cannot send information before the link is connected.");
            }

            // I-frame control: N(R) in the high three bits, N(S) in bits 3..1, bit0 = 0,
            // P/F = 0. VERIFIED encoding (XMSG-PROTOCOL.md section 3.2).
            byte control = (byte)((_receiveVariable << 5) | (_sendVariable << 1));
            byte[] body = BuildFrame(AddressData, control, info);

            _lastUnackedBody = body;
            _lastSendTicks = currentTicks;
            _retries = 0;

            _sendVariable = (_sendVariable + 1) & 0x07;   // advance V(S) mod 8
            OnTransmit?.Invoke(body);
        }

        /// <summary>
        /// Advances the injected clock and retransmits an unacknowledged frame on timeout.
        /// </summary>
        /// <param name="currentTicks">
        /// The current injected tick count.
        /// </param>
        /// <returns>
        /// <c>true</c> when a retransmission was emitted on this tick; otherwise <c>false</c>.
        /// </returns>
        /// <remarks>
        /// INFERRED behaviour: the corpus contains no loss, so the timeout value, the retry
        /// budget, and "retransmit the last unacknowledged body" policy are reasoned choices,
        /// not proven from capture.
        /// </remarks>
        public bool Tick(long currentTicks)
        {
            if (_lastUnackedBody == null)
            {
                return false;
            }

            if (currentTicks - _lastSendTicks < _retransmitTicks)
            {
                return false;
            }

            if (_retries >= _maxRetries)
            {
                // INFERRED: give up and drop back to Disconnected after exhausting retries.
                _lastUnackedBody = null;
                _state = LapbLayerState.Disconnected;
                return false;
            }

            _retries++;
            _lastSendTicks = currentTicks;
            OnTransmit?.Invoke(_lastUnackedBody);
            return true;
        }

        /// <summary>
        /// Emits a periodic Receiver-Ready keepalive when the link is connected.
        /// </summary>
        /// <remarks>
        /// INFERRED (observed live): the peer times the link back to CALL if it stops
        /// receiving RRs, so a connected node must send RR keepalives at roughly the
        /// peer's poll interval. Call this on an idle timer.
        /// </remarks>
        public void SendKeepalive()
        {
            if (_state == LapbLayerState.Connected)
            {
                EmitReadyRr();
            }
        }

        /// <summary>
        /// Handles a received unnumbered frame (SABM or UA).
        /// </summary>
        /// <param name="frame">
        /// The received unnumbered frame.
        /// </param>
        private void HandleUnnumbered(LapbFrame frame)
        {
            if (frame.Control == ControlSabm)
            {
                // GENUINE RESTART DETECTION: a SABM arriving AFTER we are synced (I-frames were
                // flowing) is a real re-establishment — per the ND behaviour, the peer restarts
                // the sync when its HDLC controller is reset. We must honour it and re-sync (reset
                // V(S)/V(R) and re-issue our own SABM), NOT ignore it. Re-arm _ownSabmSent so the
                // establishment path below sends our SABM again for this fresh episode.
                if (_synced)
                {
                    _ownSabmSent = false;
                }

                // ESTABLISHMENT (fresh OR restart): reset sequence state and answer with a UA
                // carrying our node number. VERIFIED (section 3.4).
                _receiveVariable = 0;
                _sendVariable = 0;
                _synced = false;
                _state = LapbLayerState.Connected;
                _lastUnackedBody = null;

                // VERIFIED (raw captures device-online-100-102-103.pcapng and
                // start-li-li-1err.pcapng): the ND XMSG data link is a SYMMETRIC balanced
                // link where BOTH stations issue their own SABM. A node that is passively
                // waiting keeps re-sending SABM (~once per second) until it RECEIVES a SABM
                // from the far end; the very instant the answerer transmits its own SABM the
                // initiator stops retransmitting and the two sides exchange UAs. Concretely,
                // in device-online the answerer (port 17230) transmits, in order:
                //   013f0066  (SABM, info = its own node 0x0066)
                //   01730066  (UA,   info = its own node 0x0066)
                //   09010066  (RR,   N(R)=0)
                // and node 100 (which had sent 29 SABMs) settles immediately.
                //
                // Our previous behaviour answered a received SABM with UA ONLY and never sent
                // our own SABM, so the far end never received the frame that stops its SABM
                // churn. Emit our own SABM (matching the observed on-wire order SABM -> UA -> RR)
                // to establish our direction of the balanced link — but only ONCE per
                // establishment (_ownSabmSent): the answerer in the captures sends exactly one
                // SABM, and re-emitting it per received SABM is what created the restart loop.
                if (!_ownSabmSent)
                {
                    byte[] ownSabm = BuildUnnumbered(AddressLinkSetup, ControlSabm);
                    OnTransmit?.Invoke(ownSabm);
                    _ownSabmSent = true;
                }

                byte[] ua = BuildUnnumbered(AddressLinkSetup, ControlUa);
                OnTransmit?.Invoke(ua);

                // VERIFIED (observed live against the retrocore bridge, the raw captures
                // above, and the XSLKI link states CALL->CONN->RUN): the peer only advances to
                // RUN once it RECEIVES an RR from us. Emit a Receiver-Ready now so the link
                // actually reaches the data phase; without it the peer stalls at CONN and
                // never sends reachability.
                EmitReadyRr();
            }
            else if (frame.Control == ControlUa)
            {
                // Our SABM was accepted; the link is up with V(S) = V(R) = 0.
                if (_state == LapbLayerState.SabmSent)
                {
                    _state = LapbLayerState.Connected;
                    _lastUnackedBody = null;
                    EmitReadyRr();   // see note above
                }
            }

            // INFERRED: DISC/DM/FRMR are not seen in the corpus; ignored here.
        }

        /// <summary>
        /// Emits a Receiver-Ready (RR) supervisory frame carrying the current
        /// <c>V(R)</c>, on the data address. Used to advance the peer to the RUN
        /// state after link establishment and to keep the link alive.
        /// </summary>
        private void EmitReadyRr()
        {
            byte control = (byte)(ControlRrBase | (_receiveVariable << 5));
            OnTransmit?.Invoke(BuildUnnumbered(AddressData, control));
        }

        /// <summary>
        /// Handles a received supervisory frame (RR / RNR / REJ).
        /// </summary>
        /// <param name="frame">
        /// The received supervisory frame.
        /// </param>
        private void HandleSupervisory(LapbFrame frame)
        {
            // RR acknowledges our outstanding I-frame(s) up to N(R)-1.
            if (frame.ReceiveSequence == _sendVariable)
            {
                // 100 has acknowledged everything up to V(S): clear the retransmit slot and reset
                // the stuck-detector.
                _lastUnackedBody = null;
                _lastBehindNr = -1;
            }
            else if (_lastUnackedBody != null)
            {
                // 100's N(R) is BEHIND our V(S): it has not received our last I-frame. A single
                // behind-RR can just be a transient lag that will catch up, so we retransmit only
                // when 100 REPEATS the same behind-N(R) — i.e. it is genuinely STUCK waiting for that
                // frame. Without this, terminal replies (echo/menu) deadlock: 100 keeps sending
                // `RR nr=<our reply's N(S)>`, we never resend, and both sides wait forever. VERIFIED
                // symptom. A duplicate is harmless (LAPB discards it).
                if (frame.ReceiveSequence == _lastBehindNr)
                {
                    OnTransmit?.Invoke(_lastUnackedBody);
                }

                _lastBehindNr = frame.ReceiveSequence;
            }
        }

        /// <summary>
        /// Handles a received information frame: delivers in-order info and acknowledges.
        /// </summary>
        /// <param name="frame">
        /// The received information frame.
        /// </param>
        private void HandleInformation(LapbFrame frame)
        {
            if (!_synced)
            {
                // INFERRED (observed live): the kernel keeps its V(S)/V(R) across our TCP
                // reconnect, so a fresh SABM does not reset its sequence. Adopt the peer's
                // sequence from the first I-frame — its N(S) becomes our expected V(R), and
                // its N(R) (what it expects from us) becomes our V(S) — so replies are accepted.
                _receiveVariable = frame.SendSequence;
                _sendVariable = frame.ReceiveSequence;
                _synced = true;
            }

            if (frame.SendSequence == _receiveVariable)
            {
                // In-order: deliver and advance V(R) mod 8 (VERIFIED section 3.5).
                _receiveVariable = (_receiveVariable + 1) & 0x07;
                OnInformation?.Invoke(frame.Info);
            }

            // An RR carrying the current V(R) acknowledges the peer and, for a duplicate/
            // out-of-order frame, requests the expected one (VERIFIED section 3.5).
            byte control = (byte)(ControlRrBase | (_receiveVariable << 5));
            byte[] rr = BuildUnnumbered(AddressData, control);
            OnTransmit?.Invoke(rr);
        }

        /// <summary>
        /// Builds a link-management frame body whose info field is this node's number.
        /// </summary>
        /// <param name="address">
        /// The LAPB address byte.
        /// </param>
        /// <param name="control">
        /// The LAPB control byte.
        /// </param>
        /// <returns>
        /// The four-byte body: address, control, node-number high byte, node-number low byte.
        /// </returns>
        private byte[] BuildUnnumbered(byte address, byte control)
        {
            // VERIFIED (section 3.3): SABM/UA/RR carry the sending node number as a 2-byte
            // big-endian info field (00 64 = node 100).
            byte[] body = new byte[4];
            body[0] = address;
            body[1] = control;
            body[2] = (byte)((_ownNode >> 8) & 0xFF);
            body[3] = (byte)(_ownNode & 0xFF);
            return body;
        }

        /// <summary>
        /// Builds a frame body from an address, control byte and an information field.
        /// </summary>
        /// <param name="address">
        /// The LAPB address byte.
        /// </param>
        /// <param name="control">
        /// The LAPB control byte.
        /// </param>
        /// <param name="info">
        /// The information field bytes.
        /// </param>
        /// <returns>
        /// The frame body: address, control, then the information field.
        /// </returns>
        private static byte[] BuildFrame(byte address, byte control, ReadOnlySpan<byte> info)
        {
            byte[] body = new byte[2 + info.Length];
            body[0] = address;
            body[1] = control;
            for (int i = 0; i < info.Length; i++)
            {
                body[2 + i] = info[i];
            }

            return body;
        }
    }
}
