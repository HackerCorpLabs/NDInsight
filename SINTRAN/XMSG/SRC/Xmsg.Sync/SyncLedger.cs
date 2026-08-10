using System;
using System.Collections.Generic;

namespace NDInsight.Sintran.Xmsg.Sync
{
    /// <summary>
    /// Remembers what content was last carried for each path, so unchanged files are not resent
    /// and a file coming back is not mistaken for a new edit.
    /// </summary>
    /// <remarks>
    /// <para><b>The two problems this solves are the same problem</b></para>
    /// <para>
    /// A save with no edit should cost nothing. And a file we just pushed will come back from the
    /// return leg looking exactly like a change - push it again and the daemon fights itself for
    /// ever, each direction re-triggering the other.
    /// </para>
    /// <para>
    /// Both are answered by remembering the CONTENT last transferred for a path. If what we are
    /// about to send matches what the ledger holds, there is nothing to do, whichever direction
    /// raised it. Content, not timestamps: a timestamp cannot survive the trip anyway - packed ND
    /// dates only span 1950 to 2013, so a present-day stamp lands as zero.
    /// </para>
    /// <para><b>Direction is recorded but is not the guard</b></para>
    /// <para>
    /// It would be tempting to suppress the echo by direction alone - "ignore an inbound change
    /// for a file we just pushed". That fails the moment the machine legitimately rewrites a file
    /// we also edit, which is the normal case for anything a compiler touches. Comparing content
    /// is right in both cases; the direction is kept only so a human can see what happened.
    /// </para>
    /// </remarks>
    public sealed class SyncLedger
    {
        /// <summary>
        /// What the ledger holds for one path.
        /// </summary>
        private sealed class Entry
        {
            /// <summary>
            /// Hash of the content last carried.
            /// </summary>
            public byte[] Hash = Array.Empty<byte>();

            /// <summary>
            /// Which way it was carried, for reporting only.
            /// </summary>
            public SyncDirection Direction;
        }

        private readonly Dictionary<string, Entry> _entries;

        /// <summary>
        /// Creates an empty ledger.
        /// </summary>
        public SyncLedger()
        {
            _entries = new Dictionary<string, Entry>(StringComparer.OrdinalIgnoreCase);
        }

        /// <summary>
        /// Gets how many paths the ledger knows about.
        /// </summary>
        public int Count
        {
            get { return _entries.Count; }
        }

        /// <summary>
        /// Decides whether content needs carrying for a path.
        /// </summary>
        /// <param name="path">
        /// The path being considered.
        /// </param>
        /// <param name="hash">
        /// Hash of the content that would be sent.
        /// </param>
        /// <returns>
        /// <see langword="true"/> when the ledger has no record of this path, or its record holds
        /// different content.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="path"/> or <paramref name="hash"/> is null.
        /// </exception>
        public bool NeedsTransfer(string path, byte[] hash)
        {
            if (path == null)
            {
                throw new ArgumentNullException(nameof(path));
            }

            if (hash == null)
            {
                throw new ArgumentNullException(nameof(hash));
            }

            Entry? entry;
            if (!_entries.TryGetValue(path, out entry))
            {
                return true;
            }

            return !SameHash(entry.Hash, hash);
        }

        /// <summary>
        /// Records that content was carried for a path.
        /// </summary>
        /// <param name="path">
        /// The path that was carried.
        /// </param>
        /// <param name="hash">
        /// Hash of the content carried. Copied, so a caller reusing its buffer cannot corrupt the
        /// ledger afterwards.
        /// </param>
        /// <param name="direction">
        /// Which way it went.
        /// </param>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="path"/> or <paramref name="hash"/> is null.
        /// </exception>
        public void RecordTransfer(string path, byte[] hash, SyncDirection direction)
        {
            if (path == null)
            {
                throw new ArgumentNullException(nameof(path));
            }

            if (hash == null)
            {
                throw new ArgumentNullException(nameof(hash));
            }

            byte[] copy = new byte[hash.Length];
            for (int i = 0; i < hash.Length; i++)
            {
                copy[i] = hash[i];
            }

            Entry? entry;
            if (!_entries.TryGetValue(path, out entry))
            {
                entry = new Entry();
                _entries.Add(path, entry);
            }

            entry.Hash = copy;
            entry.Direction = direction;
        }

        /// <summary>
        /// Gets which way a path was last carried.
        /// </summary>
        /// <param name="path">
        /// The path to ask about.
        /// </param>
        /// <param name="direction">
        /// Set to the recorded direction, or <see cref="SyncDirection.None"/> when the path is
        /// unknown.
        /// </param>
        /// <returns>
        /// <see langword="true"/> when the path was found.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="path"/> is null.
        /// </exception>
        public bool TryGetDirection(string path, out SyncDirection direction)
        {
            if (path == null)
            {
                throw new ArgumentNullException(nameof(path));
            }

            Entry? entry;
            if (_entries.TryGetValue(path, out entry))
            {
                direction = entry.Direction;
                return true;
            }

            direction = SyncDirection.None;
            return false;
        }

        /// <summary>
        /// Copies out every path the ledger remembers.
        /// </summary>
        /// <returns>
        /// The paths, in no particular order. A fresh array each time, so a caller can forget
        /// paths while walking it.
        /// </returns>
        /// <remarks>
        /// <para>
        /// This is how a DELETED file is noticed at all. A folder scan can only report what is
        /// there; a file that has gone leaves no trace except the ledger's memory of having
        /// carried it. Without this the daemon would silently leave stale copies on the machine
        /// for ever.
        /// </para>
        /// <para>
        /// A copy rather than the live keys, because the caller's next move is usually to decide
        /// some of those paths are gone and forget them, and a dictionary cannot be changed while
        /// its keys are being walked.
        /// </para>
        /// </remarks>
        public string[] CopyPaths()
        {
            string[] paths = new string[_entries.Count];

            int index = 0;
            Dictionary<string, Entry>.KeyCollection.Enumerator keys = _entries.Keys.GetEnumerator();
            while (keys.MoveNext())
            {
                paths[index] = keys.Current;
                index++;
            }

            return paths;
        }

        /// <summary>
        /// Forgets a path, so the next transfer for it is treated as new.
        /// </summary>
        /// <param name="path">
        /// The path to forget.
        /// </param>
        /// <returns>
        /// <see langword="true"/> when the path was known.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="path"/> is null.
        /// </exception>
        public bool Forget(string path)
        {
            if (path == null)
            {
                throw new ArgumentNullException(nameof(path));
            }

            return _entries.Remove(path);
        }

        /// <summary>
        /// Compares two hashes.
        /// </summary>
        /// <param name="left">
        /// The first hash.
        /// </param>
        /// <param name="right">
        /// The second hash.
        /// </param>
        /// <returns>
        /// <see langword="true"/> when they are the same length and every byte matches.
        /// </returns>
        private static bool SameHash(byte[] left, byte[] right)
        {
            if (left.Length != right.Length)
            {
                return false;
            }

            for (int i = 0; i < left.Length; i++)
            {
                if (left[i] != right[i])
                {
                    return false;
                }
            }

            return true;
        }
    }
}
