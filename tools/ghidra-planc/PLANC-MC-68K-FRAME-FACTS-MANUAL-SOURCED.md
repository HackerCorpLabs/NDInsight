# PLANC-MC (MC68000) frame + calling convention - manual-sourced facts

Source of every fact below: ND-820026.1 EN "DOMINO and NUCLEUS Software Guide",
chapter 3 (DOMINO Monitor), "Stack Format" section, Figures 8 and 9.
Repo path: Reference-Manuals\500\ND-820026.1 EN DOMINO and NUCLEUS Software Guide.md
(An identical second scan exists as ND-820026-1c-EN ... .md; same content.)

This is DOCUMENTED, not inferred from disassembly. Where it disagrees with a
guess made from decompiler output, the manual wins - but note the caveat at the
bottom about which compiler version produced your binary.

--------------------------------------------------------------------
## 1. Ordinary routine stack frame (Figure 8)

Valid from the H-version of the PLANC-MC compiler onward.

    byte offset      content
    0B               STP        <---- A6 points here
    4B               Unused
    10B              SMAX
    14B              SYST
    20B              ERRCODE
    24B              Parameters and local data
    ...              free stack area
    (at STP)         PREV       <---- A7 (USP/SSP/MSP/ISP) points here
    (below PREV)     RETLINK

Offsets are OCTAL (10B = 8 decimal, 14B = 12, 20B = 16, 24B = 20).
So in decimal, relative to A6: 0 STP, 4 unused, 8 SMAX, 12 SYST, 16 ERRCODE,
20 first parameter / local data.

Field meanings, verbatim from the manual:
- STP     - points to the first free location of the stack.
            "The stack grows both upwards and downwards."
- Unused  - reserved word for future extension.
- SMAX    - points to the top of the free stack. Same as the A7 register for
            the CURRENT stack. It exists as a separate variable because there
            may be several stacks in use; A7 changes after each Inistack.
- SYST    - reserved word for the PLANC runtime SYSTem.
- ERRCODE - the ERRCODE value of the current routine.
- PREV    - the previous value of A6. The previous A7 is (A7 - 2 words).
            Both registers are restored on routine termination.
- RETLINK - return address of the calling routine.

--------------------------------------------------------------------
## 2. THE SKIP RETURN - exact rule

On NORMAL termination (not ErReturn), the return address held in RETLINK is
INCREMENTED BY TWO BYTES before returning. The manual calls this the "skip
return".

Consequence for static analysis: the word immediately after a routine call is
NOT executed on the success path. It holds one of:
  - a subroutine call to the LOCAL exception handler (the compiled
    On RoutineError Do ... Endon block), or
  - a jump to #XRET, when the routine defines no local handler.

If your disassembler treats that word as ordinary fall-through code, every
call site is mis-decoded. This is the single highest-value rule in this file.

--------------------------------------------------------------------
## 3. ErReturn / error propagation

- ErReturn jumps to the PLANC runtime routine #XRET.
- The address of #XRET is ALWAYS in register A5. (So a bare `jmp (A5)` in the
  decompiler output is an error return, not an unknown indirect call.)
- #XRET performs the error return to the previous level and pops the current
  stack frame.
- D0 carries the ERRCODE value.
- Propagation: control passes up the call hierarchy; every routine at a call
  level below the one holding the exception handler is terminated.

--------------------------------------------------------------------
## 4. Parameter passing and value registers

- Actual parameters are placed on the stack in the SAME ORDER as declared.
- In-value / out-value are passed differently from parameters:
    * simple variables and constants not exceeding 32 bits -> D0
    * everything else -> A0 holds a POINTER to the actual parameter

--------------------------------------------------------------------
## 5. Routine modifiers that change or remove the frame

- SPECIAL routine: cannot have parameters (in-value/out-value only). NO local
  stack is initiated when called - the routine must set one up itself if it
  needs local data.
- NATIVE routine: in-value/out-value but NO formal parameters. Local stack IS
  initiated on activation, so local variables work. Uses a DIFFERENT frame
  (Figure 9, below). Well suited to exception handlers.
- INTERRUPT / exception handlers generally: activated entirely by hardware, so
  they do not fit the PLANC environment. An ordinary PLANC routine's body
  assumes the runtime already allocated its frame - which is untrue for a
  hardware-entered handler. The manual names three routine kinds that do not
  implicitly assume a stack.
- Historical note the manual makes explicitly: it used to be dangerous to put
  anything but pure inline assembler in a SPECIAL routine, because generated
  code assumed a frame existed. LATER PLANC-MC versions emit a WARNING when
  they use the stack frame in a SPECIAL routine, which made high-level PLANC
  statements in SPECIAL routines usable. So the safety of a given SPECIAL
  routine depends on compiler version.

### Native routine frame (Figure 9)

    byte offset      content
    -4B              free stack area (grows downward)
    0B               STP        <---- A6 AND A7 both point here
    4B               Unused
    10B              SMAX
    14B              SYST
    20B              ERRCODE
    24B              Local data
    ...
    PREV
    RETLINK

Differences from the ordinary frame:
- A6 and A7 coincide at entry.
- STP points to the first free location AFTER local data (i.e. at PREV).
- "The stack grows only from high to low memory addresses" - i.e. the normal
  CPU direction, unlike the ordinary frame's grows-both-ways STP.
- PREV is again the old A6; the previous A7 is (STP - 2 words).

--------------------------------------------------------------------
## 6. Word size, for type recovery

From ND-60.117.5 EN PLANC Reference Manual, Appendix C "Machine Dependent
Language Features in PLANC":

    word size = 2 bytes on ND-100
                4 bytes on ND-500
                4 bytes on MC68000

and: from version F of MC68000 PLANC, one word is defined as 4 bytes, where in
PREVIOUS versions it was 2 bytes. If a struct/array layout comes out half-size
or double-size, this version boundary is the first thing to check. Programs can
pin sizes explicitly with INTEGER2, REAL4 etc. for inter-CPU data transfer.

--------------------------------------------------------------------
## 7. Debugger-visible confirmation

The DOMINO monitor's PREVIOUS / NEXT commands walk exactly this PREV chain
(PREVIOUS repeats down to the main program, which holds the lowest frame; NEXT
is only valid after PREVIOUS and cannot go past the currently executing
routine). If you can drive the monitor, its frame view is ground truth for the
layout above.

One quirk worth knowing, from ND-20034-1-EN (ND-Specific Programming &
Advanced PLANC), section 4.4: in LOOK-AT-STACK the field "NUMBER OF PARAMETERS"
reads zero even for routines that do have parameters, because ordinary PLANC
routines do not need it. It is only populated for routines declared with other
modifiers such as STANDARD. Do not conclude a routine is parameterless from a
zero there. (That remark is written about the ND-500(0) debugger; I have not
verified the same holds in the DOMINO monitor on 68000.)

--------------------------------------------------------------------
## 8. Caveats - read before trusting any of this against your binary

- Figure 8 is stated as valid "from the H-version of the PLANC-MC compiler".
  If your firmware predates H, the ordinary frame may differ. I have not found
  a pre-H layout documented.
- The word-size change at version F (section 6) is a second version boundary.
- Everything above is 68000 / PLANC-MC. The ND-100 PLANC convention is a
  DIFFERENT layout (B-register relative, negative octal byte offsets, runtime
  routines 5INIT/5ENTR/5LEAV/5ERET). Do not mix the two. That material is in
  ND-60.117.5 Appendix D, section "Interfacing with PLANC on the ND-100".
- I have NOT cross-checked any of this against an actual firmware image. It is
  manual-sourced only. Verify against your binary before acting on it.
