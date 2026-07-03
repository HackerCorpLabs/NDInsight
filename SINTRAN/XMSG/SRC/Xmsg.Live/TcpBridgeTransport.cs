using System;
using System.Net.Sockets;
using System.Threading;
using System.Threading.Tasks;

namespace NDInsight.Sintran.Xmsg.Live
{
    /// <summary>
    /// An <see cref="IByteDuplex"/> that connects to the <c>nd100x --hdlc</c> TCP bridge
    /// and pumps the transparent HDLC byte stream in both directions.
    /// </summary>
    /// <remarks>
    /// NOT-LIVE-TESTED: this class talks to a real socket and cannot be exercised against a
    /// live bridge in the unit-test suite. The bridge is a transparent byte pipe
    /// (XMSG-PROTOCOL.md section 1), so this transport only moves bytes — all framing and
    /// protocol logic lives in <see cref="LiveNode"/>, which IS tested over
    /// <see cref="InMemoryDuplex"/>.
    /// </remarks>
    public sealed class TcpBridgeTransport : IByteDuplex, IDisposable
    {
        private readonly TcpClient _client;
        private readonly NetworkStream _stream;

        /// <summary>
        /// Initialises the transport around an already-connected TCP client.
        /// </summary>
        /// <param name="client">
        /// The connected TCP client whose stream carries the HDLC bytes.
        /// </param>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="client"/> is null.
        /// </exception>
        public TcpBridgeTransport(TcpClient client)
        {
            if (client == null)
            {
                throw new ArgumentNullException(nameof(client));
            }

            _client = client;
            _stream = client.GetStream();
        }

        /// <summary>
        /// Connects to the bridge at a host and port and returns a ready transport.
        /// </summary>
        /// <param name="host">
        /// The bridge host name or address.
        /// </param>
        /// <param name="port">
        /// The bridge TCP port.
        /// </param>
        /// <param name="cancellationToken">
        /// A token that cancels the connect attempt.
        /// </param>
        /// <returns>
        /// A task yielding the connected transport.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        /// Thrown when <paramref name="host"/> is null.
        /// </exception>
        public static async Task<TcpBridgeTransport> ConnectAsync(string host, int port, CancellationToken cancellationToken)
        {
            if (host == null)
            {
                throw new ArgumentNullException(nameof(host));
            }

            // NOT-LIVE-TESTED: real network connect; no bridge is available under test.
            TcpClient client = new TcpClient();
            await client.ConnectAsync(host, port, cancellationToken);
            return new TcpBridgeTransport(client);
        }

        /// <inheritdoc/>
        public Task<int> ReadAsync(byte[] buffer, int offset, int count, CancellationToken cancellationToken)
        {
            // Stream.ReadAsync(byte[], int, int, CancellationToken) already returns Task<int>.
            return _stream.ReadAsync(buffer, offset, count, cancellationToken);
        }

        /// <inheritdoc/>
        public Task WriteAsync(ReadOnlyMemory<byte> data, CancellationToken cancellationToken)
        {
            return _stream.WriteAsync(data, cancellationToken).AsTask();
        }

        /// <summary>
        /// Releases the underlying stream and socket.
        /// </summary>
        public void Dispose()
        {
            _stream.Dispose();
            _client.Dispose();
        }
    }
}
