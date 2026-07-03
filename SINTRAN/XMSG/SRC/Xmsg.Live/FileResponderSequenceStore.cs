using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;

namespace NDInsight.Sintran.Xmsg.Live
{
    /// <summary>
    /// A file-backed <see cref="IResponderSequenceStore"/>: persists the next outgoing
    /// <c>Flags1</c> per remote node to a small text file, so the sequence survives our process
    /// restarts and stays in step with the peer's persistent XSRSQ.
    /// </summary>
    /// <remarks>
    /// File format: one line per remote node, <c>&lt;nodeDecimal&gt;=&lt;flags1Hex&gt;</c>, for
    /// example <c>100=0007</c>. The whole (tiny) file is rewritten on each save — the map holds at
    /// most a handful of remote nodes, so this is cheap and keeps the on-disk state always current.
    /// If the file is missing or unreadable, every node simply starts at 0x0000 (a fresh contact).
    /// </remarks>
    public sealed class FileResponderSequenceStore : IResponderSequenceStore
    {
        private readonly string _path;
        private readonly Dictionary<ushort, ushort> _next;

        /// <summary>
        /// Opens (and loads) the store at the given file path, creating the parent directory if needed.
        /// </summary>
        /// <param name="path">The state file path.</param>
        /// <exception cref="ArgumentNullException">Thrown when <paramref name="path"/> is null.</exception>
        public FileResponderSequenceStore(string path)
        {
            _path = path ?? throw new ArgumentNullException(nameof(path));
            _next = new Dictionary<ushort, ushort>();
            Load();
        }

        /// <inheritdoc />
        public ushort LoadNextFlags1(ushort remoteNode)
        {
            if (_next.TryGetValue(remoteNode, out ushort value))
            {
                return value;
            }

            return 0x0000;
        }

        /// <inheritdoc />
        public void SaveNextFlags1(ushort remoteNode, ushort nextFlags1)
        {
            _next[remoteNode] = nextFlags1;
            Persist();
        }

        /// <summary>Reads the state file into the in-memory map; tolerant of a missing/garbled file.</summary>
        private void Load()
        {
            if (!File.Exists(_path))
            {
                return;
            }

            string[] lines;
            try
            {
                lines = File.ReadAllLines(_path);
            }
            catch (IOException)
            {
                return;
            }

            for (int i = 0; i < lines.Length; i++)
            {
                string line = lines[i].Trim();
                if (line.Length == 0)
                {
                    continue;
                }

                int eq = line.IndexOf('=');
                if (eq <= 0 || eq >= line.Length - 1)
                {
                    continue;
                }

                string nodePart = line.Substring(0, eq).Trim();
                string valuePart = line.Substring(eq + 1).Trim();

                if (ushort.TryParse(nodePart, NumberStyles.Integer, CultureInfo.InvariantCulture, out ushort node)
                    && ushort.TryParse(valuePart, NumberStyles.HexNumber, CultureInfo.InvariantCulture, out ushort flags1))
                {
                    _next[node] = flags1;
                }
            }
        }

        /// <summary>Rewrites the whole state file from the in-memory map.</summary>
        private void Persist()
        {
            string? directory = Path.GetDirectoryName(_path);
            if (!string.IsNullOrEmpty(directory) && !Directory.Exists(directory))
            {
                Directory.CreateDirectory(directory);
            }

            List<string> lines = new List<string>(_next.Count);
            // Explicit enumerator (project style: no foreach). Dictionary's enumerator is a struct,
            // so this allocates nothing beyond the strings we build.
            Dictionary<ushort, ushort>.Enumerator e = _next.GetEnumerator();
            while (e.MoveNext())
            {
                KeyValuePair<ushort, ushort> entry = e.Current;
                lines.Add(entry.Key.ToString(CultureInfo.InvariantCulture) + "=" + entry.Value.ToString("X4", CultureInfo.InvariantCulture));
            }

            try
            {
                File.WriteAllLines(_path, lines);
            }
            catch (IOException)
            {
                // Best-effort persistence; a transient write failure must not crash the live node.
            }
        }
    }
}
