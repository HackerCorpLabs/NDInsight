using System;
using System.Text;

namespace NDInsight.Sintran.Xmsg.Node
{
    /// <summary>
    /// The application logic of the simulated remote machine's terminal: a message-of-the-day
    /// banner (with date and time), a <c>#</c> command prompt, and a four-item menu
    /// (1 Time, 2 Date, 3 Echo, 4 Disconnect). This class is deliberately PURE — it produces
    /// plain terminal text and never touches XMSG/TAD framing — so it can be unit-tested with a
    /// fixed clock. The TAD terminal session wraps this text into TAD BDAT messages.
    /// </summary>
    /// <remarks>
    /// Terminal line ending is CR+LF (<c>0x0D 0x0A</c>), matching the retrocore captures
    /// (e.g. the real "PASSWORD: " / MOTD frames all used <c>0D 0A</c>). All text is 7-bit ASCII.
    /// </remarks>
    public sealed class TadTerminalMenu
    {
        // Terminal newline as observed on the wire in the captured retrocore terminal output.
        private const string Crlf = "\r\n";

        // Month names so the MOTD/date reads like the retrocore banner ("9 APRIL 1998") rather
        // than a locale-dependent numeric format.
        private static readonly string[] MonthNames =
        {
            "JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE",
            "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER"
        };

        /// <summary>
        /// Builds the greeting sent immediately after the terminal connection is accepted:
        /// the message-of-the-day banner (including <paramref name="now"/>), the menu, and the
        /// first prompt.
        /// </summary>
        /// <param name="now">
        /// The current date/time to stamp into the banner. Passed in (not read from the clock)
        /// so the greeting is deterministic under test.
        /// </param>
        /// <returns>The full greeting text to emit to the terminal.</returns>
        public string BuildGreeting(DateTime now)
        {
            StringBuilder sb = new StringBuilder();
            sb.Append(BuildMotd(now));
            sb.Append(BuildMenu());
            sb.Append(BuildPrompt());
            return sb.ToString();
        }

        /// <summary>
        /// Builds the message-of-the-day banner with the supplied date and time.
        /// </summary>
        /// <param name="now">The timestamp to display.</param>
        /// <returns>The banner text (leading and trailing CRLF included).</returns>
        public string BuildMotd(DateTime now)
        {
            StringBuilder sb = new StringBuilder();
            sb.Append(Crlf);
            sb.Append("========================================").Append(Crlf);
            sb.Append("  NDInsight TAD Server  -  Node 103").Append(Crlf);
            // Date + time on one banner line, e.g. "  02 JULY 2026   14:33:07".
            sb.Append("  ").Append(FormatDate(now)).Append("   ").Append(FormatTime(now)).Append(Crlf);
            sb.Append("========================================").Append(Crlf);
            return sb.ToString();
        }

        /// <summary>
        /// Builds the menu listing. Also used as the response to the <c>help</c> command.
        /// </summary>
        /// <returns>The menu text.</returns>
        public string BuildMenu()
        {
            StringBuilder sb = new StringBuilder();
            sb.Append(Crlf);
            sb.Append("  1. Time").Append(Crlf);
            sb.Append("  2. Date").Append(Crlf);
            sb.Append("  3. Echo").Append(Crlf);
            // The responder now chunks any reply longer than one 255-byte BDAT across frames
            // (TadTerminalResponder.EmitMenuReply), so the menu is no longer length-constrained;
            // it is still kept readable rather than verbose.
            sb.Append("  4. Disconnect (ladder; ~1min timer)").Append(Crlf);
            sb.Append("  5. Disc NOW  [EXP: +host DCON]").Append(Crlf);
            sb.Append("  6. Disc hard [EXP: DCON only]").Append(Crlf);
            sb.Append("  7. Disc alt  [EXP: DCON role 94]").Append(Crlf);
            sb.Append("  (type 'help' to show this menu)").Append(Crlf);
            return sb.ToString();
        }

        /// <summary>
        /// Builds the command prompt (<c>#</c>).
        /// </summary>
        /// <returns>The prompt text, on its own line.</returns>
        public string BuildPrompt()
        {
            return Crlf + "# ";
        }

        /// <summary>
        /// Processes one line of terminal input and produces the reply plus the next prompt.
        /// </summary>
        /// <param name="input">
        /// The raw line the user typed (without the terminating CR/LF). May be null or empty.
        /// </param>
        /// <param name="now">
        /// The current date/time (used for commands 1/2). Passed in for deterministic tests.
        /// </param>
        /// <returns>
        /// A <see cref="TadMenuResult"/> carrying the text to send and whether the session
        /// should disconnect afterwards.
        /// </returns>
        public TadMenuResult Handle(string? input, DateTime now)
        {
            // Normalise: trim surrounding whitespace so "1", " 1 ", "1\r" all match. The terminal
            // may deliver a trailing CR; Trim removes it.
            string command = input == null ? string.Empty : input.Trim();

            StringBuilder sb = new StringBuilder();

            // A bare Enter (empty line) just re-shows the prompt — no error, common terminal UX.
            if (command.Length == 0)
            {
                sb.Append(BuildPrompt());
                return new TadMenuResult(sb.ToString(), false);
            }

            // Case-insensitive compare for the word commands (help). The digit commands are
            // matched exactly.
            if (command == "1")
            {
                sb.Append(Crlf).Append(FormatTime(now));
                sb.Append(BuildPrompt());
                return new TadMenuResult(sb.ToString(), false);
            }

            if (command == "2")
            {
                sb.Append(Crlf).Append(FormatDate(now));
                sb.Append(BuildPrompt());
                return new TadMenuResult(sb.ToString(), false);
            }

            if (command == "3")
            {
                // Echo command: send back a segment containing the fixed text "IPSUM LORUM".
                sb.Append(Crlf).Append("IPSUM LORUM");
                sb.Append(BuildPrompt());
                return new TadMenuResult(sb.ToString(), false);
            }

            if (command == "4")
            {
                // Disconnect: emit a goodbye line and signal teardown. No prompt follows.
                // The capture-faithful variant: full ladder + 0xFD, then WAIT for the client's DCON
                // (SINTRAN's connect-to holds the line for its "not logged in" idle timeout, default
                // 1 minute — ND-30.025 SET-TIMEOUT-VALUES — unless the user presses the local
                // character; ND-60.163).
                sb.Append(Crlf).Append("--- DISCONNECTING ---").Append(Crlf);
                return new TadMenuResult(sb.ToString(), TadDisconnectMode.Ladder);
            }

            if (command == "5")
            {
                // EXPERIMENT "disconnect now": full ladder + 0xFD, then a HOST-initiated DCON.
                // Never captured host->client, but the NPL master table marks 7DCON S->C
                // "Disconnect indication" (MP-P2-TAD BDDIS -> DSTOTA forced disconnect), so the
                // client-side machinery exists. Goal: skip the client's 1-minute idle hold.
                sb.Append(Crlf).Append("--- DISCONNECTING NOW (ladder + host DCON) ---").Append(Crlf);
                return new TadMenuResult(sb.ToString(), TadDisconnectMode.LadderThenDcon);
            }

            if (command == "6")
            {
                // EXPERIMENT "hard disconnect": host DCON alone, no teardown ladder, no 0xFD.
                // Tests whether the DCON indication is sufficient by itself.
                sb.Append(Crlf).Append("--- DISCONNECTING HARD (DCON only) ---").Append(Crlf);
                return new TadMenuResult(sb.ToString(), TadDisconnectMode.DconOnly);
            }

            if (command == "7")
            {
                // EXPERIMENT variant of 5: same ladder + 0xFD + DCON, but the DCON carries the
                // ASKER-style role byte (0x94 = WaitForTransfer|Bounce|RoutedLetter — the value every
                // CAPTURED client DCON uses) instead of the host data-phase role 0x00. Distinguishes
                // "role byte matters" from "DCON direction matters" if 5 fails and 7 works (or vice
                // versa).
                sb.Append(Crlf).Append("--- DISCONNECTING ALT (asker-style DCON) ---").Append(Crlf);
                return new TadMenuResult(sb.ToString(), TadDisconnectMode.LadderThenDconAskerRole);
            }

            if (string.Equals(command, "help", StringComparison.OrdinalIgnoreCase))
            {
                sb.Append(BuildMenu());
                sb.Append(BuildPrompt());
                return new TadMenuResult(sb.ToString(), false);
            }

            // Anything else (other than 1-4 and help) is an unknown command, echoed back verbatim
            // exactly as the user requested: "unknown command: <text>".
            sb.Append(Crlf).Append("unknown command: ").Append(command);
            sb.Append(BuildPrompt());
            return new TadMenuResult(sb.ToString(), false);
        }

        /// <summary>
        /// Formats the date as "DD MONTH YYYY" (for example "02 JULY 2026").
        /// </summary>
        private static string FormatDate(DateTime now)
        {
            // Manual formatting (no locale/ToString culture surprises); month from our own table.
            string day = now.Day.ToString("D2");
            string month = MonthNames[now.Month - 1];
            string year = now.Year.ToString("D4");
            return day + " " + month + " " + year;
        }

        /// <summary>
        /// Formats the time as "HH:MM:SS" (24-hour).
        /// </summary>
        private static string FormatTime(DateTime now)
        {
            return now.Hour.ToString("D2") + ":" + now.Minute.ToString("D2") + ":" + now.Second.ToString("D2");
        }
    }

    /// <summary>
    /// How a menu choice wants the session torn down. The variants exist to TEST, against a real
    /// SINTRAN client, which frames make it disconnect immediately instead of holding the line for
    /// its 1-minute "not logged in" idle timeout (ND-30.025 SET-TIMEOUT-VALUES; ND-60.163: the
    /// timer is the fallback for users who forget the local character).
    /// </summary>
    public enum TadDisconnectMode
    {
        /// <summary>No disconnect — the session continues.</summary>
        None = 0,

        /// <summary>
        /// The capture-faithful teardown: the five-frame ladder ending in the <c>0xFD</c>
        /// notification, then wait for the client's DCON (menu choice 4).
        /// </summary>
        Ladder,

        /// <summary>
        /// EXPERIMENT (choice 5): the full ladder + <c>0xFD</c>, then a HOST-initiated DCON
        /// (host data-phase role <c>0x00</c>). Untested on the wire; NPL marks 7DCON as a
        /// server-to-client disconnect indication.
        /// </summary>
        LadderThenDcon,

        /// <summary>
        /// EXPERIMENT (choice 6): a host-initiated DCON alone — no ladder, no <c>0xFD</c>.
        /// </summary>
        DconOnly,

        /// <summary>
        /// EXPERIMENT (choice 7): as <see cref="LadderThenDcon"/> but the DCON carries the
        /// asker-style role byte <c>0x94</c> that every CAPTURED client DCON uses.
        /// </summary>
        LadderThenDconAskerRole,
    }

    /// <summary>
    /// The outcome of handling one terminal input line: the text to transmit and whether/how the
    /// session must disconnect afterwards.
    /// </summary>
    public readonly struct TadMenuResult
    {
        /// <summary>
        /// Initialises the result.
        /// </summary>
        /// <param name="output">The terminal text to send.</param>
        /// <param name="disconnect">True when the session should close after sending (uses the
        /// capture-faithful <see cref="TadDisconnectMode.Ladder"/> teardown).</param>
        public TadMenuResult(string output, bool disconnect)
        {
            Output = output;
            Mode = disconnect ? TadDisconnectMode.Ladder : TadDisconnectMode.None;
        }

        /// <summary>
        /// Initialises the result with an explicit disconnect mode.
        /// </summary>
        /// <param name="output">The terminal text to send.</param>
        /// <param name="mode">The teardown variant to run after sending.</param>
        public TadMenuResult(string output, TadDisconnectMode mode)
        {
            Output = output;
            Mode = mode;
        }

        /// <summary>Gets the terminal text to transmit back to the client.</summary>
        public string Output { get; }

        /// <summary>Gets the teardown variant to run (None = stay connected).</summary>
        public TadDisconnectMode Mode { get; }

        /// <summary>Gets a value indicating whether the session should disconnect afterwards.</summary>
        public bool Disconnect
        {
            get { return Mode != TadDisconnectMode.None; }
        }
    }
}
