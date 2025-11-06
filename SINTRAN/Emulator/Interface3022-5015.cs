// 3022/5015 Interface - Communication between ND-100 and ND-500
//
// Architecture Decision: INTEGRATED APPROACH
// Instead of creating separate 5015 and 3022 classes, we create one unified
// Interface3022_5015 class that simulates both sides. This is simpler and more
// efficient for emulation purposes.
//
// The 3022 is an IOX device on the ND-100 side
// The 5015 is connected to the ND-500 MMU
// They communicate via TAG registers and interrupts

using System;
using System.Collections.Generic;

namespace Emulated.HW.ND.CPU.ND500
{
    /// <summary>
    /// TAG register values for signaling between CPUs.
    /// </summary>
    public enum TAGValue : ushort
    {
        IDLE = 0x0000,
        MONITOR_CALL_REQUEST = 0x0001,      // ND-500 → ND-100: Monitor call
        OPERATION_COMPLETE = 0x0002,         // ND-100 → ND-500: Result ready
        PAGE_FAULT = 0x0003,                 // ND-500 → ND-100: Page fault
        ACTIVATE_PROCESS = 0x0004,           // ND-100 → ND-500: Start process
        TERMINATE_PROCESS = 0x0005,          // ND-100 → ND-500: Stop process
        SWAP_REQUEST = 0x000F,               // ND-500 → ND-100: Swap page
    }

    /// <summary>
    /// Unified 3022/5015 interface for ND-100 <-> ND-500 communication.
    /// Simulates both sides of the hardware interface.
    /// </summary>
    public class Interface3022_5015
    {
        // Hardware state
        private ushort _statusRegister;
        private ushort _controlRegister;
        private uint _memoryAddressRegister;    // MAR - points to message buffer
        private ushort _tagOut;                 // ND-500 → ND-100
        private ushort _tagIn;                  // ND-100 → ND-500
        private ushort _deviceNumber;           // IOX device number (typically 100₈)

        // References to ND-100 and ND-500 systems
        private readonly ND100CPU _nd100;
        private readonly ND500CPU _nd500;
        private readonly MultiportMemory _mpm;

        // Status register bits (RSTA5)
        private const ushort BIT_5ALIVE = 0x0008;   // Bit 3: ND-500 alive
        private const ushort BIT_5FAULT = 0x0010;   // Bit 4: Fault
        private const ushort BIT_5PFAIL = 0x0020;   // Bit 5: Power fail
        private const ushort BIT_5DMAER = 0x0040;   // Bit 6: DMA error
        private const ushort BIT_5CLOST = 0x0080;   // Bit 7: Clock lost
        private const ushort BIT_5ERROR = 0x0100;   // Bit 8: Error
        private const ushort BIT_5ILOCK = 0x0200;   // Bit 9: Interface lock

        // IOX register offsets for 3022 interface
        public const byte RMAR5 = 0x00;    // Read MAR
        public const byte LMAR5 = 0x01;    // Load MAR
        public const byte RSTA5 = 0x02;    // Read Status
        public const byte LSTA5 = 0x03;    // Load Status
        public const byte RCON5 = 0x04;    // Read Control
        public const byte LCON5 = 0x05;    // Load Control
        public const byte MCLR5 = 0x06;    // Master Clear
        public const byte TERM5 = 0x07;    // Terminate
        public const byte RTAG5 = 0x08;    // Read Tag
        public const byte LTAG5 = 0x09;    // Load Tag
        public const byte RLOW5 = 0x0A;    // Read Lower Limit
        public const byte LDAT5 = 0x0B;    // Load Data
        public const byte SLOC5 = 0x0C;    // Status Lock
        public const byte BITM5 = 0x0D;    // Bitmask
        public const byte UNLC5 = 0x0E;    // Unlock
        public const byte RETG5 = 0x0F;    // Return Gate

        public Interface3022_5015(ND100CPU nd100, ND500CPU nd500, MultiportMemory mpm, ushort deviceNumber = 0x40)
        {
            _nd100 = nd100;
            _nd500 = nd500;
            _mpm = mpm;
            _deviceNumber = deviceNumber;

            // Initialize to healthy state
            _statusRegister = BIT_5ALIVE;
            _controlRegister = 0;
            _tagOut = (ushort)TAGValue.IDLE;
            _tagIn = (ushort)TAGValue.IDLE;

            Console.WriteLine($"[3022/5015] Interface initialized: Device number 0x{deviceNumber:X2}");
        }

        #region ND-100 Side (3022 IOX Interface)

        /// <summary>
        /// Handle IOX instruction from ND-100 to 3022 interface.
        /// Called by ND-100 emulator when T register points to this device.
        /// </summary>
        public ushort HandleIOX(byte offset, ushort? writeValue = null)
        {
            bool isRead = !writeValue.HasValue;
            ushort value = writeValue.GetValueOrDefault();

            if (isRead)
            {
                Console.WriteLine($"[3022] ND-100 READ IOX offset 0x{offset:X2}");
            }
            else
            {
                Console.WriteLine($"[3022] ND-100 WRITE IOX offset 0x{offset:X2}, value=0x{value:X4}");
            }

            switch (offset)
            {
                case RMAR5:  // Read MAR
                    return (ushort)(_memoryAddressRegister & 0xFFFF);

                case LMAR5:  // Load MAR
                    if (writeValue.HasValue)
                    {
                        _memoryAddressRegister = (_memoryAddressRegister & 0xFFFF0000) | value;
                        Console.WriteLine($"[3022] MAR set to 0x{_memoryAddressRegister:X8}");
                    }
                    return 0;

                case RSTA5:  // Read Status
                    Console.WriteLine($"[3022] Status = 0x{_statusRegister:X4} (5ALIVE={((_statusRegister & BIT_5ALIVE) != 0 ? "YES" : "NO")})");
                    return _statusRegister;

                case LSTA5:  // Load Status
                    if (writeValue.HasValue)
                        _statusRegister = value;
                    return 0;

                case RCON5:  // Read Control
                    return _controlRegister;

                case LCON5:  // Load Control
                    if (writeValue.HasValue)
                    {
                        _controlRegister = value;
                        // Bit 3 (0x0008) = Enable interrupt level 12
                        if ((value & 0x0008) != 0)
                        {
                            Console.WriteLine($"[3022] Interrupts ENABLED on level 12");
                        }
                    }
                    return 0;

                case MCLR5:  // Master Clear
                    Console.WriteLine($"[3022] Master Clear");
                    MasterClear();
                    return 0;

                case TERM5:  // Terminate
                    if (writeValue.HasValue)
                    {
                        byte processNum = (byte)value;
                        Console.WriteLine($"[3022] Terminate process {processNum}");
                        TerminateND500Process(processNum);
                    }
                    return 0;

                case RTAG5:  // Read Tag (TAG-IN from ND-500)
                    Console.WriteLine($"[3022] Read TAG-IN = 0x{_tagOut:X4} ({(TAGValue)_tagOut})");
                    return _tagOut;

                case LTAG5:  // Load Tag (TAG-OUT to ND-500)
                    if (writeValue.HasValue)
                    {
                        _tagIn = value;
                        Console.WriteLine($"[3022] Write TAG-OUT = 0x{_tagIn:X4} ({(TAGValue)_tagIn})");
                        ProcessTagFromND100(value);
                    }
                    return 0;

                case UNLC5:  // Unlock
                    _statusRegister &= (ushort)~BIT_5ILOCK;
                    Console.WriteLine($"[3022] Interface unlocked");
                    return 0;

                default:
                    Console.WriteLine($"[3022] WARNING: Unknown IOX offset 0x{offset:X2}");
                    return 0;
            }
        }

        /// <summary>
        /// Master clear - reset interface to initial state.
        /// </summary>
        private void MasterClear()
        {
            _statusRegister = BIT_5ALIVE;
            _controlRegister = 0;
            _memoryAddressRegister = 0;
            _tagOut = (ushort)TAGValue.IDLE;
            _tagIn = (ushort)TAGValue.IDLE;
        }

        #endregion

        #region ND-500 Side (5015 Interface)

        /// <summary>
        /// Signal ND-100 that ND-500 needs service (monitor call).
        /// Called by ND-500 trap handler.
        /// </summary>
        public void SignalND100MonitorCall(byte processNum, uint messageBufferAddr)
        {
            Console.WriteLine($"[5015] Process {processNum} requesting monitor call, message at 0x{messageBufferAddr:X}");

            // Set MAR to point to message buffer
            _memoryAddressRegister = messageBufferAddr;

            // Set TAG-OUT (ND-500 → ND-100)
            _tagOut = (ushort)TAGValue.MONITOR_CALL_REQUEST;

            // Trigger ND-100 interrupt level 12
            TriggerND100Interrupt(12, processNum, messageBufferAddr);
        }

        /// <summary>
        /// Signal ND-100 that ND-500 has a page fault.
        /// Called by ND-500 MMU.
        /// </summary>
        public void SignalND100PageFault(byte processNum, uint faultAddress)
        {
            Console.WriteLine($"[5015] Process {processNum} page fault at 0x{faultAddress:X8}");

            _tagOut = (ushort)TAGValue.PAGE_FAULT;
            TriggerND100Interrupt(12, processNum, faultAddress);
        }

        /// <summary>
        /// Process TAG value written by ND-100 (TAG-IN for ND-500).
        /// </summary>
        private void ProcessTagFromND100(ushort tagValue)
        {
            TAGValue tag = (TAGValue)tagValue;

            switch (tag)
            {
                case TAGValue.OPERATION_COMPLETE:
                    Console.WriteLine($"[5015] ND-100 signaled operation complete");
                    // Find which process was waiting and wake it up
                    WakeUpND500Process();
                    break;

                case TAGValue.ACTIVATE_PROCESS:
                    Console.WriteLine($"[5015] ND-100 requesting process activation");
                    ActivateND500Process();
                    break;

                case TAGValue.TERMINATE_PROCESS:
                    Console.WriteLine($"[5015] ND-100 requesting process termination");
                    break;

                default:
                    Console.WriteLine($"[5015] WARNING: Unknown TAG value 0x{tagValue:X4}");
                    break;
            }
        }

        /// <summary>
        /// Wake up ND-500 process after ND-100 completes operation.
        /// </summary>
        private void WakeUpND500Process()
        {
            // Read message buffer to find which process
            uint messageAddr = _memoryAddressRegister;

            // The process number can be inferred from message buffer address
            // or stored in the message buffer itself
            byte processNum = _nd500.CurrentProcess;  // Simplified

            Console.WriteLine($"[5015] Waking up process {processNum}");

            // Resume ND-500 process
            _nd500.TrapHandler.ResumeAfterMonitorCall(_nd500, processNum);

            // Trigger ND-500 interrupt to resume execution
            // (In real hardware, this would be interrupt level 14)
            _nd500.IsWaiting = false;
        }

        /// <summary>
        /// Activate ND-500 process (start execution).
        /// </summary>
        private void ActivateND500Process()
        {
            // Implementation depends on your process scheduler
            Console.WriteLine($"[5015] Activating ND-500 process");
        }

        /// <summary>
        /// Terminate ND-500 process.
        /// </summary>
        private void TerminateND500Process(byte processNum)
        {
            Console.WriteLine($"[5015] Terminating process {processNum}");
            // Stop ND-500 process execution
            // Clean up resources
        }

        #endregion

        #region Interrupt Handling

        /// <summary>
        /// Trigger interrupt on ND-100 CPU.
        /// </summary>
        private void TriggerND100Interrupt(int level, byte processNum, uint messageAddr)
        {
            Console.WriteLine($"[3022] Triggering ND-100 interrupt level {level}");
            Console.WriteLine($"[3022] Process {processNum}, message at 0x{messageAddr:X}");

            // Queue interrupt on ND-100
            _nd100.QueueInterrupt(level, new ND100InterruptData
            {
                Source = "ND-500",
                ProcessNumber = processNum,
                MessageAddress = messageAddr,
                TAGValue = _tagOut
            });
        }

        #endregion

        #region Diagnostics

        /// <summary>
        /// Dump interface state for debugging.
        /// </summary>
        public void DumpState()
        {
            Console.WriteLine("=== 3022/5015 Interface State ===");
            Console.WriteLine($"Device Number: 0x{_deviceNumber:X2}");
            Console.WriteLine($"Status Register: 0x{_statusRegister:X4}");
            Console.WriteLine($"  5ALIVE:  {((_statusRegister & BIT_5ALIVE) != 0)}");
            Console.WriteLine($"  5FAULT:  {((_statusRegister & BIT_5FAULT) != 0)}");
            Console.WriteLine($"  5PFAIL:  {((_statusRegister & BIT_5PFAIL) != 0)}");
            Console.WriteLine($"  5DMAER:  {((_statusRegister & BIT_5DMAER) != 0)}");
            Console.WriteLine($"Control Register: 0x{_controlRegister:X4}");
            Console.WriteLine($"MAR: 0x{_memoryAddressRegister:X8}");
            Console.WriteLine($"TAG-OUT (ND-500→ND-100): 0x{_tagOut:X4} ({(TAGValue)_tagOut})");
            Console.WriteLine($"TAG-IN (ND-100→ND-500): 0x{_tagIn:X4} ({(TAGValue)_tagIn})");
            Console.WriteLine("================================");
        }

        #endregion
    }

    #region ND-100 CPU Stub

    /// <summary>
    /// ND-100 CPU stub (you'll integrate with your existing ND-100 emulator).
    /// </summary>
    public class ND100CPU
    {
        private readonly Queue<ND100InterruptData> _interruptQueue = new Queue<ND100InterruptData>();

        public void QueueInterrupt(int level, ND100InterruptData data)
        {
            Console.WriteLine($"[ND100] Interrupt queued: Level {level}");
            _interruptQueue.Enqueue(data);

            // In your main CPU loop, process interrupts:
            // if (_interruptQueue.Count > 0) ProcessInterrupt();
        }

        public void ProcessInterrupts()
        {
            while (_interruptQueue.Count > 0)
            {
                ND100InterruptData data = _interruptQueue.Dequeue();
                HandleND500Interrupt(data);
            }
        }

        private void HandleND500Interrupt(ND100InterruptData data)
        {
            Console.WriteLine($"[ND100] Processing ND-500 interrupt from process {data.ProcessNumber}");
            // Call your ND-100 interrupt level 12 handler here
            // See next file: ND100-Interrupt-Level-12-Handler.cs
        }
    }

    /// <summary>
    /// Data passed with ND-100 interrupt from ND-500.
    /// </summary>
    public class ND100InterruptData
    {
        public string Source { get; set; }
        public byte ProcessNumber { get; set; }
        public uint MessageAddress { get; set; }
        public ushort TAGValue { get; set; }
    }

    #endregion
}
