using System;

namespace NDInsight.Sintran.Xmsg.Live
{
    /// <summary>
    /// Configurable LAPB timers and window size, with the ND LAPB spec section 5 defaults.
    /// </summary>
    /// <remarks>
    /// <para><b>Time unit</b></para>
    /// <see cref="T1"/> and <see cref="T3"/> are durations in the SAME unit as the monotonic value
    /// passed to <see cref="LapbLayer.Tick(long)"/> and the other clocked methods. The unit is the
    /// caller's choice: unit tests inject small integer ticks, and the live runner injects elapsed
    /// milliseconds (so the defaults below read as 3000 ms and 30000 ms).
    /// <para><b>Defaults</b></para>
    /// The spec defaults are:
    ///  - <c>T1 = 3000</c> — retransmission / poll timer.
    ///  - <c>T3 = 30000</c> — idle keepalive timer.
    ///  - <c>N2 = 10</c> — maximum retransmissions before declaring link failure.
    ///  - <c>k = 7</c> — transmit window (unacknowledged I-frames outstanding, MUST be 1..7).
    /// </remarks>
    public sealed class LapbOptions
    {
        /// <summary>
        /// The retransmission / poll timer duration (spec T1), in the injected-clock unit.
        /// </summary>
        public long T1 { get; }

        /// <summary>
        /// The idle keepalive timer duration (spec T3), in the injected-clock unit.
        /// </summary>
        public long T3 { get; }

        /// <summary>
        /// The maximum number of retransmissions before the link is declared failed (spec N2).
        /// </summary>
        public int N2 { get; }

        /// <summary>
        /// The transmit window: the maximum number of unacknowledged I-frames outstanding (spec k).
        /// </summary>
        public int WindowSize { get; }

        /// <summary>
        /// Initialises LAPB options, validating each value and falling back to the spec default.
        /// </summary>
        /// <param name="t1">
        /// The T1 retransmission / poll timer; must be positive. Defaults to <c>3000</c>.
        /// </param>
        /// <param name="t3">
        /// The T3 idle keepalive timer; must be positive. Defaults to <c>30000</c>.
        /// </param>
        /// <param name="n2">
        /// The N2 maximum retransmissions; must be at least 1. Defaults to <c>10</c>.
        /// </param>
        /// <param name="windowSize">
        /// The transmit window k; must be in the range 1..7 for modulo-8. Defaults to <c>7</c>.
        /// </param>
        /// <exception cref="ArgumentOutOfRangeException">
        /// Thrown when <paramref name="t1"/> or <paramref name="t3"/> is not positive, when
        /// <paramref name="n2"/> is less than 1, or when <paramref name="windowSize"/> is outside 1..7.
        /// </exception>
        public LapbOptions(long t1 = 3000, long t3 = 30000, int n2 = 10, int windowSize = 7)
        {
            if (t1 <= 0)
            {
                throw new ArgumentOutOfRangeException(nameof(t1), t1, "T1 must be positive.");
            }

            if (t3 <= 0)
            {
                throw new ArgumentOutOfRangeException(nameof(t3), t3, "T3 must be positive.");
            }

            if (n2 < 1)
            {
                throw new ArgumentOutOfRangeException(nameof(n2), n2, "N2 must be at least 1.");
            }

            // Modulo-8 sequence numbering caps the window at 7 (spec 4.2); 0 would stall all sends.
            if (windowSize < 1 || windowSize > 7)
            {
                throw new ArgumentOutOfRangeException(nameof(windowSize), windowSize, "Window size k must be 1..7 for modulo-8.");
            }

            T1 = t1;
            T3 = t3;
            N2 = n2;
            WindowSize = windowSize;
        }

        /// <summary>
        /// Gets a shared options instance carrying the spec section 5 defaults.
        /// </summary>
        public static LapbOptions Default { get; } = new LapbOptions();
    }
}
