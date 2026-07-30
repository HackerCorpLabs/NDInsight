namespace NDInsight.Sintran.Xmsg.Protocol.Fa
{
    /// <summary>
    /// The operations a <c>*FA-SERVER</c> request can carry, as the server itself names them.
    /// </summary>
    /// <remarks>
    /// <para>
    /// These are not inferred from behaviour. The server program <c>COS-FA-SERV-E04:PROG</c> holds a
    /// word-aligned packed list of thirteen command names at <c>BANK2::8731</c>, immediately followed
    /// by a table of handler addresses:
    /// </para>
    /// <code>
    /// index  0     1     2     3     4     5     6     7     8     9    10    11    12    13   14
    /// addr 1fb0  1fb0  1ead  1ecc  1edd  1eee  1eff  1f10  1f21  1f32  1f43  1f6c  1f7d  1f8e 1f9f
    /// name       disc  Rsrv  Rlse  ChId  Open  Clos  SetB  Read  Writ  Crea  Dele  SIII  Devf
    /// </code>
    /// <para>
    /// The operation code is the index into that table. Three of the names line up with codes
    /// established independently from recorded traffic - <see cref="OpenFile"/>,
    /// <see cref="CreateFile"/> and <see cref="DeleteFile"/> - which is why the numbering is treated
    /// as VERIFIED rather than assumed.
    /// </para>
    /// <para>
    /// The operation travels as a two-byte value behind a <c>0x92</c> tag, immediately followed by the
    /// exchange sequence tagged the same way. A reply echoes both.
    /// </para>
    /// </remarks>
    public enum FaOperation : ushort
    {
        /// <summary>
        /// Disconnects a file entry. On the wire as <c>0x0001</c>.
        /// </summary>
        /// <remarks>
        /// Its slot in the handler table holds the same address as the table's padding, so it is not
        /// dispatched through the normal path. That fits it belonging to teardown rather than being a
        /// request a client sends. Never recorded.
        /// </remarks>
        FileEntryDisconnect = 0x0001,

        /// <summary>
        /// Reserves a file entry. On the wire as <c>0x0002</c>.
        /// </summary>
        /// <remarks>
        /// Opens every recorded conversation, carrying a 112-byte body with the user and directory
        /// specification. Previously called "open spec" in this library, which described how it looked
        /// rather than what it is.
        /// </remarks>
        ReserveFileEntry = 0x0002,

        /// <summary>
        /// Releases a file entry. On the wire as <c>0x0003</c>.
        /// </summary>
        /// <remarks>
        /// Closes every recorded conversation. Previously called "close spec" here.
        /// </remarks>
        ReleaseFileEntry = 0x0003,

        /// <summary>
        /// Changes a file entry's identifier. On the wire as <c>0x0004</c>. Never recorded.
        /// </summary>
        ChangeFileEntryId = 0x0004,

        /// <summary>
        /// Opens a file and returns the file number to use for it. On the wire as <c>0x0005</c>.
        /// </summary>
        /// <remarks>
        /// <para>
        /// The request names the file; the reply carries the REMOTE file number, which is not the
        /// number the operator is shown. The access mode rides under field selector 3 and is omitted
        /// entirely for read.
        /// </para>
        /// </remarks>
        OpenFile = 0x0005,

        /// <summary>
        /// Closes an open file. On the wire as <c>0x0006</c>.
        /// </summary>
        /// <remarks>
        /// <para>
        /// Recorded only at session teardown, never when the operator typed <c>CLOSE-FILE</c> - that
        /// went to the separate <c>*FA-USER</c> service instead, which is still unexplained. An
        /// earlier note in this library concluded from that absence that the operation could not be a
        /// close. The server's own table says otherwise, and the table wins.
        /// </para>
        /// </remarks>
        CloseFile = 0x0006,

        /// <summary>
        /// Sets the block size on an open file. On the wire as <c>0x0007</c>. Never recorded.
        /// </summary>
        SetBlockSize = 0x0007,

        /// <summary>
        /// Reads from an open file. On the wire as <c>0x0008</c>. Never recorded.
        /// </summary>
        /// <remarks>
        /// <para>
        /// The operation behind the terminal's <c>RFILE</c> command. The terminal command takes a
        /// MEMORY ADDRESS argument, which is why it was not driven by hand on a live machine; the
        /// protocol operation carries no such thing, so a client can issue it safely.
        /// </para>
        /// <para>
        /// The server's handler is at <c>BANK2::1f21</c>. Request and reply layouts are UNKNOWN - no
        /// recording exists and the handler has not been read.
        /// </para>
        /// </remarks>
        ReadFile = 0x0008,

        /// <summary>
        /// Writes to an open file. On the wire as <c>0x0009</c>. Never recorded.
        /// </summary>
        /// <remarks>
        /// The operation behind the terminal's <c>WFILE</c> command. Handler at <c>BANK2::1f32</c>.
        /// Layouts UNKNOWN.
        /// </remarks>
        WriteFile = 0x0009,

        /// <summary>
        /// Creates a file with a reserved page count and no contents. On the wire as <c>0x000A</c>.
        /// </summary>
        /// <remarks>
        /// The request carries the file name and a 32-bit page count and nothing else. The file it
        /// produces is a continuous file, so contiguity is implied by the operation rather than sent.
        /// </remarks>
        CreateFile = 0x000A,

        /// <summary>
        /// Deletes a file. On the wire as <c>0x000B</c>.
        /// </summary>
        DeleteFile = 0x000B,

        /// <summary>
        /// A catch-all whose sub-function is carried under field selector 1. On the wire as
        /// <c>0x000C</c>.
        /// </summary>
        /// <remarks>
        /// <para>
        /// A directory listing uses this with <c>0x0078</c> under selector 1; a request seen at
        /// session teardown uses <c>0x003B</c>. That looked like a broken constant until the command
        /// table named this operation: selector 1 holds the SUB-FUNCTION, so those are two different
        /// sub-functions rather than an inconsistent field. Which is which is UNKNOWN.
        /// </para>
        /// </remarks>
        SiiiSpecial = 0x000C,

        /// <summary>
        /// A device function. On the wire as <c>0x000D</c>. Never recorded.
        /// </summary>
        DeviceFunction = 0x000D,
    }
}
