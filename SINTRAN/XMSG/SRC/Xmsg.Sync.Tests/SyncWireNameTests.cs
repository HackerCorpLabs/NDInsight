using NDInsight.Sintran.Xmsg.Sync;

using Xunit;

namespace NDInsight.Sintran.Xmsg.Sync.Tests
{
    /// <summary>
    /// The two forms of a file specification: the one a person types, and the one the wire carries.
    /// </summary>
    /// <remarks>
    /// Found live on 2026-08-11. The daemon handed the addressed form to the open request and it
    /// was refused - "27 characters" against a fifteen-byte compact string - and the exception took
    /// the whole node down with it.
    /// </remarks>
    public sealed class SyncWireNameTests
    {
        /// <summary>
        /// The machine, the user and the quotes all come off.
        /// </summary>
        [Fact]
        public void AnAddressedQuotedSpecReducesToTheBareName()
        {
            Assert.Equal("WATCH1:TXT", SyncFolderMap.ToWireName("D100(SYSTEM).\"WATCH1:TXT\""));
        }

        /// <summary>
        /// The unquoted form - an overwrite - reduces the same way.
        /// </summary>
        [Fact]
        public void AnAddressedBareSpecReducesToTheBareName()
        {
            Assert.Equal("WATCH1:TXT", SyncFolderMap.ToWireName("D100(SYSTEM).WATCH1:TXT"));
        }

        /// <summary>
        /// A name that is already bare is left alone.
        /// </summary>
        [Fact]
        public void ANameThatIsAlreadyBareIsUnchanged()
        {
            Assert.Equal("WATCH1:TXT", SyncFolderMap.ToWireName("WATCH1:TXT"));
        }

        /// <summary>
        /// What BuildFileSpec produces is what ToWireName has to undo - both ways round.
        /// </summary>
        /// <remarks>
        /// Pinning the pair together, so a change to one that the other does not follow fails here
        /// rather than against a live machine.
        /// </remarks>
        [Theory]
        [InlineData(true)]
        [InlineData(false)]
        public void ItUndoesWhatBuildFileSpecDid(bool creating)
        {
            string built = SyncFolderMap.BuildFileSpec("D100", "SYSTEM", "PROG", "SYMB", creating);

            Assert.Equal("PROG:SYMB", SyncFolderMap.ToWireName(built));
        }
    }
}
