/* ============================================================================
 * MON 412B  FileAsSegment (FSCNT)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  ND-500 System Monitor call (octal >= 0400).
 *
 * Manual behaviour (ND-860228.2 EN p.193): connect an OPEN file as a logical
 * segment in the calling domain, so the file can be accessed as a segment
 * (faster than ReadFromFile/WriteToFile, which are then disallowed on that
 * file).  The file is disconnected when it is closed.
 *
 * Derived from the real disassembly (see 412B-FileAsSegment.ASM), the carved
 * ND-500 System Monitor segment 030-S3SM5.bin, file offset 0x98dd.
 *
 * INSTRUCTION SEMANTICS are per
 *   ../../instruction-semantics/ND500-INSTRUCTION-SEMANTICS.md
 * Lines that model an opcode give the REAL ND-500 operation from that reference
 * (register model, addressing modes, branch conditions; C=1 means NO-borrow).
 *
 * HONESTY / MISALIGNMENT: the ROUTING (ND-500 call via the S3SM5 0x60 vector
 * table, not the ND-100 GOTAB) is byte-verified: slot 0x0274 holds 0x98dd.  The
 * entry 0x98dd is SHARED - the MON 127B slot (0x010e) holds the same 0x98dd
 * (byte-proven), so which of 412B / 127B owns this body is INFERRED.  The stream
 * is MISALIGNED past the first few loads: 0x98ff decodes as '??? opcode 0x00F1'
 * (Sec.9: an operand/prefix byte, NOT an instruction), the 'f set1 $<double>'
 * at 0x98f3 is impossible as decoded (set1 sets a destination to 1, it takes no
 * 8-byte immediate - Sec.5.7), and the 'if < go' at 0x98e5 precedes the 'entsn'
 * (an entry prologue should START with an ENT*).  Per the reference (Sec.9) the
 * decoded mnemonics past the entry are UNTRUSTWORTHY even though the raw bytes
 * are ground truth.  Only the entry loads and the routing are relied upon; the
 * body semantics, argument-slot mapping, and status/error contract are
 * UNVERIFIED.  Addresses in comments are file byte offsets into 030-S3SM5.bin.
 * ============================================================================ */

/* Transport is the ND-500 MON message block (CALLG argument list), NOT ND-100
 * A/X/T registers.  Manual parameter list (mapping to frame slots UNVERIFIED):
 *   FileNo        (in)  - file number, must be open (see OpenFile)
 *   LogSegmentNo  (in)  - wanted logical segment number; 0 = first free
 *   AccessType    (in)  - 0 initial data / 1 empty / 2 sequential / 3 = 1+2
 *   SegmentNo     (out) - segment number actually selected (if LogSegmentNo=0) */
int mon_file_as_segment(nd500_mon_msg *m /* args UNVERIFIED */)
{
    /* 0x98dd-0x98e3: literal float loads per the reference (Sec.5.1).  R is the */
    /*   record base.  Which slot is FileNo/LogSegmentNo/AccessType is NOT       */
    /*   byte-proven.                                                            */
    A4 = mem32[R + 0xA8];      /* 0x98dd: f4 := r.0xA8                            */
    A4 = mem32[R + 0xE8];      /* 0x98df: f4 := r.0xE8                            */
    A4 = mem32[R + 0xA8];      /* 0x98e1: f4 := r.0xA8                            */
    A4 = 0x29;                 /* 0x98e3: f4 := $0x29  (immediate, Sec.3.1)      */

    /* 0x98e5: if < go ... -> if (S) goto ...  (signed <, Sec.7.2).             */
    /*   UNVERIFIED: no compare/test precedes it (only loads), so the sign flag  */
    /*   it tests is undefined in an aligned reading, and this branch sits BEFORE */
    /*   the entsn.  Not modelled as a meaningful guard - misalignment signal.   */

    /* 0x98ec: entsn $0x10,... -> build stack frame, enter subroutine (Sec.8.2). */
    frame_enter();

    /* 0x98f0-0x9935: MISALIGNED.  0x98ff = '??? opcode 0x00F1' (Sec.9, an       */
    /*   address-code/prefix byte, not an opcode); 0x98f3 'f set1 $<double>' is  */
    /*   an impossible decode; the 'init' at 0x9917 (Sec.8.1: init runs ONCE at  */
    /*   program start, not inside a handler) and the mid-block arithmetic are   */
    /*   NOT a reliable guide to the real control flow.  UNVERIFIED: the         */
    /*   connect-file-as-segment action, the chosen segment number, and any      */
    /*   write-back cannot be recovered from this decode.                        */

    return OK;                 /* status word / error / SegmentNo: UNVERIFIED     */
}
