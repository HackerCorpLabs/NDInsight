NDCHAT - INSTALL FLOPPY
=======================

Chat between people on ND-100 machines running SINTRAN III with XMSG
and COSMOS. This floppy holds the built programs. The sources are on
the SOURCE floppy.

FILES
  CHAT:PROG        the client a user runs:  @CHAT
  CHAT-MON:PROG    the operator program:    @CHAT-MON
  CHATSV:BRF       the server, RT-loaded as CHATSER
  CHATLIB:BRF      the message library the server is loaded with
  CHATRT:MODE      the RT-load sequence (edit segment, name, peers first)
  CHATBOOT:MODE    the block to add to the boot mode file
  README:TEXT      this

ALSO NEEDED ON THE MACHINE (ND products, not on this floppy)
  XMP-100-1-B02:BRF     COSMOS XMSG library, product ND-10609
  MON-CALL-1B-A00:BRF   SINTRAN III Monitor Call Package, ND-210913
  MON-CALL-NAMES-A:DATA the names file that comes with it
  PLANC-1BANK-F00:BRF   the PLANC one-bank run time library
  XMSG and COSMOS started, and the peer machines defined (X-C)

INSTALL
  1. Copy the four program files and the two mode files to (SYSTEM).
     COPY-FILE takes the DESTINATION FIRST, and quotes mean "create":
       @COPY-FILE "CHAT:PROG",(NDCHAT:FLOPPY-USER)CHAT:PROG
     Check each with FILE-STATISTICS - the byte count must match the
     manifest that came with this floppy.
  2. Edit CHATRT:MODE: a FREE segment number, this machine's name, the
     system numbers of the peers.
  3. @MODE CHATRT:MODE,,
     Then @LI-RT-DES,CHATSER must show IN TIME QUEUE.
  4. Start a client: @CHAT. Say something. Your line comes back with a
     time on it. That is the server answering.
  5. Put the CHATBOOT:MODE block at the end of the boot mode file, after
     XMSG and COSMOS are up - but run it by hand once first (see the
     comment in the file).

AFTER ANY RELOAD OF THE SERVER, EVERY CLIENT THAT WAS JOINED IS ORPHANED
and shows a normal screen that receives nothing. /exit and start @CHAT
again on every terminal. After any XMSG restart, RT-load the server
again - starting it is not enough.

OPERATING
  @CHAT-MON then ?      the command list
  STATUS                seats, counters, history per room
  LIST-TRUNKS           peers and whether each is up
  START-TRUNK <n>       only on a trunk that is DOWN - on one that is up
                        it knocks it down for a minute

Full manuals: DOC/manuals/ in the NDInsight repository -
CHAT-USER-MANUAL.md, CHAT-ADMIN-MANUAL.md, CHAT-BUILD-AND-DEPLOY-MANUAL.md.
