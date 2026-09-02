# The control-store gate - the exact emulator answer (byte-verified)

This closes the investigation that started when Ronny's emulator hung: `@ND-500` -> `VERSION` /
`LOAD-SWAPPER` -> "Loading Control Store" -> loop/fail (no `CONTROL-STORE:DATA` microcode present).

## The gate condition (VERIFIED)
The SINTRAN ND-500 driver reads the interface **STATUS** register (`RSTA5`, IOX `dev+2`) and tests
**bit 9 = `5CLOST` = "ND-500 micro clock has stopped"** (mask `001000`):
- NPL `IF A NBIT 5CLOST THEN ... % If nd-500 not stopped (clock stopped)` (MP-P2-N500.NPL).
- Symbol: `5CLOS = 000011` (bit **9**); `ECSLO = 002032` (the error code "CONTROL STORE MUST BE
  LOADED"); `5ILOC = 000005` (bit **5**, interface LOCKED = ND-500 running).
- Bit meaning: XC-P2-N500.NPL:45 `BIT 5CLOST=9 (001000) = Microclock stopped`.

**When the ND-500 micro clock is stopped (no microcode running), STATUS bit 9 (`5CLOST`) is SET.**
The driver takes this as "the control store is not loaded" and the operation returns `ECSLOAD`
(`2032B`). The `nd-500-mon:prog` gateway (`146244`) then auto-loads the control store and retries - so
ANY `MON 60` triggers a control-store load when the microcode is not resident (this is why even
`VERSION` hung). With no `CONTROL-STORE:DATA` file, the load fails and it loops.

## THE EMULATOR FIX
To make the monitor believe the control store IS loaded and the ND-500 is ready, the emulated
interface's **`RSTA5` (STATUS, IOX `dev+2`) read must return:**
- **bit 9 `5CLOST` (`001000`) = CLEAR** - micro clock running (control store loaded). **This is the
  key bit.**
- bit 5 `5ILOCK` (`000040`) = CLEAR - not currently running/locked (idle, ready to accept work).
- bit 2 (busy) = CLEAR, bit 3 (finished) as appropriate.
- error bits CLEAR: bit 4 `5PAGF` (`000020`), bit 6 `5DMAER` (`000100`), bit 7 `5PFAIL` (`000200`),
  bit 8 `5POWOF` (`000400`).
- stop-reason field (bits 10-14) = 0.

i.e. return a STATUS of `0` (or with only bit 0 "interrupt enabled" set) for a ready, idle,
control-store-loaded ND-500. **Do NOT set bit 9.** That alone clears the `ECSLOAD` gate and lets
`VERSION` and the rest of the monitor proceed without a real microcode image (Q7).

## Full STATUS (RSTA5) bit map (from ND500-BUS-INTERFACE-REFERENCE.md 4.2, byte-verified)
| bit | mask | symbol | meaning |
|-----|------|--------|---------|
| 0 | `000001` | - | interrupt enabled |
| 2 | `000004` | - | ND-500 busy |
| 3 | `000010` | - | ND-500 finished |
| 4 | `000020` | `5PAGF` | error (OR of errors) |
| 5 | `000040` | `5ILOC`/`5ILOCK` | interface LOCKED = ND-500 running |
| 6 | `000100` | `5DMAER` | DMA / comm error |
| 7 | `000200` | `5PFAIL` | ND-500 power fault |
| 8 | `000400` | `5POWOF` | ND-500 power off (latched) |
| **9** | **`001000`** | **`5CLOST`/`5CLOS`** | **micro clock stopped (= control store not loaded)** |
| 10-14 | `076000` | - | stop reason |
| 15 | `100000` | - | CONTROL bit 15 |

## Caveats
- This satisfies the *gate* so the monitor proceeds; it does NOT make the ND-500 actually execute
  (there is still no microcode engine). It unblocks the ND-100 <-> ND-500 message/command traffic
  (the carve goal), not real ND-500 program execution.
- The activation protocol (bus doc 5.1) also checks `5ILOCK` (terminate first if running) - keep it
  clear for an idle ND-500.
- VERIFIED: the bit-9 gate + STATUS map (bytes + NPL + hardware manual, cross-agreeing). INFERRED: the
  exact full STATUS word to return - `0`/interrupt-enabled is the safe ready-idle value; confirm by
  driving the emulator once the change is in.

Source: `030-S3SM5` driver (`RSTAT`/`RSTA5`), `MP-P2-N500.NPL`/`XC-P2-N500.NPL`, N500-SYMBOLS,
`SINTRAN\ND500\ND500-BUS-INTERFACE-REFERENCE.md` section 4.2.
