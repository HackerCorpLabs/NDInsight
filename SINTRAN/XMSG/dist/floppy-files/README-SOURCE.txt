NDCHAT - SOURCE FLOPPY
======================

The PLANC sources and build files for the chat. Building needs a
SINTRAN III machine with PLANC-100-F00, BRF-LINKER-C01, the COSMOS XMSG
library XMP-100-1-B02 with its XMP-B02:DEFS and XMP-B02:IMPT, the
Monitor Call Package MON-CALL-1B-A00, PLANC-1BANK-F00, and for the client
the VTM screen libraries INTRF1B, VTMR, VTMDATA, VTMARR.

WHAT BUILDS WHAT
  CHATCC:MODE     CHAT:PLNC + CHATARR:PLNC -> CHAT:PROG      the client
  CHATSV:MODE     CHATSV:PLNC              -> CHATSV:BRF     the server
  CHATMON:MODE    CHATMN:PLNC              -> CHAT-MON:PROG  the operator program
                  (the source is CHATMN on purpose: a 13-character name limit
                  in the file transfer that carried it; the program keeps the
                  full name)
  CHATLIB:PLNC                             -> CHATLIB:BRF    the shared message
                  library. Both programs IMPORT it. Change it, rebuild both.
  CHATKT / CHATVT / CHATCTST                                offline test programs
  CHATUI, CHATTST, KEYPROB                                  older experiments

Copy everything to (SYSTEM), then
  @MODE CHATSV:MODE,,     then   @MODE CHATCC:MODE,,     then   @MODE CHATMON:MODE,,

READ THE LISTING, NOT THE SCREEN. The compiler's diagnostics scroll off
a 24-line screen and the "0 DIAGNOSTICS" left at the bottom belongs to the
linker. Not one "*** ERROR" in CHAT:LIST, CHATARR:LIST and CHATSV:LIST is
the only green light. Every included file must end with $EOF, or the
compile stops there silently. Sources must be CR LF.

The three rules and every trap are written out in the MODE files
themselves - they are documents as much as scripts. Read CHATCC:MODE
first.
