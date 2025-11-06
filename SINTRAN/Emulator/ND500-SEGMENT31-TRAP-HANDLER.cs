// ND-500 Segment 31 "Other CPU" Trap Handler
// Handles monitor calls from ND-500 to ND-100 via segment 31 (37 octal)
// When ND-500 executes CALLG #0x1F000000, this code triggers

using System;
using System.Collections.Generic;

namespace Emulated.HW.ND.CPU.ND500
{
    /// <summary>
    /// MICFU (Microcode Function) codes for monitor calls.
    /// These are written to message buffer offset +3.
    /// </summary>
    public enum MICFUCode : ushort
    {
        NOP = 0x00,          // No operation
        DVIO_OUT = 0x01,     // Device output (write to device)
        DVIO_IN = 0x02,      // Device input (read from device)
        OPEN_FILE = 0x03,    // Open file
        CLOSE_FILE = 0x04,   // Close file
        READ_FILE = 0x05,    // Read from file
        WRITE_FILE = 0x06,   // Write to file
        ALLOC_MEM = 0x07,    // Allocate memory
        FREE_MEM = 0x08,     // Free memory
        GET_TIME = 0x09,     // Get system time
        DELAY = 0x0A,        // Delay process
        SIGNAL = 0x0B,       // Signal another process
        WAIT = 0x0C,         // Wait for signal
        FORK = 0x0D,         // Create child process
        EXIT = 0x0E,         // Terminate process
        SWAP_PAGE = 0x0F,    // Swap page request (to swapper)
        // Add more as needed...
    }

    /// <summary>
    /// ND-500 CPU state that needs to be saved during "Other CPU" trap.
    /// </summary>
    public class ND500TrapState
    {
        public uint ProgramCounter { get; set; }
        public uint[] Registers { get; set; } = new uint[16];  // R0-R15
        public uint StatusWord { get; set; }
        public uint StackPointer { get; set; }
        public byte CurrentProcessNumber { get; set; }

        public void Save(ND500CPU cpu)
        {
            ProgramCounter = cpu.PC;
            Array.Copy(cpu.Registers, Registers, 16);
            StatusWord = cpu.StatusWord;
            StackPointer = cpu.SP;
            CurrentProcessNumber = cpu.CurrentProcess;
        }

        public void Restore(ND500CPU cpu)
        {
            cpu.PC = ProgramCounter;
            Array.Copy(Registers, cpu.Registers, 16);
            cpu.StatusWord = StatusWord;
            cpu.SP = StackPointer;
            cpu.CurrentProcess = CurrentProcessNumber;
        }
    }

    /// <summary>
    /// Main ND-500 CPU class (simplified stub - you'll integrate with your existing CPU).
    /// </summary>
    public class ND500CPU
    {
        public uint PC { get; set; }
        public uint[] Registers { get; set; } = new uint[16];
        public uint StatusWord { get; set; }
        public uint SP { get; set; }
        public byte CurrentProcess { get; set; }
        public bool IsWaiting { get; set; }

        // Segment capabilities (for MMU)
        public ushort[] ProgramCapabilities { get; set; } = new ushort[32];
        public ushort[] DataCapabilities { get; set; } = new ushort[32];

        // Reference to shared resources
        public MultiportMemory MPM5 { get; set; }
        public Interface3022_5015 Interface { get; set; }
        public ND500TrapHandler TrapHandler { get; set; }

        /// <summary>
        /// Execute one instruction (called by main CPU loop).
        /// </summary>
        public void ExecuteInstruction()
        {
            // Your existing instruction decode here...
            // When you encounter CALLG instruction:

            uint instruction = FetchInstruction(PC);

            if (IsCALLGInstruction(instruction))
            {
                uint targetAddress = GetCALLGTarget(instruction);
                byte targetSegment = (byte)((targetAddress >> 24) & 0x1F);  // Bits 28-24

                // Check if this is segment 31 (0x1F = 37 octal)
                if (targetSegment == 0x1F)
                {
                    // Look up segment capability
                    ushort progCap = ProgramCapabilities[31];

                    // Check for "Other CPU" bit (bit 14)
                    bool isOtherCPU = (progCap & 0x4000) != 0;
                    bool isIndirect = (progCap & 0x8000) != 0;

                    if (isOtherCPU && isIndirect)
                    {
                        // TRIGGER OTHER CPU TRAP!
                        Console.WriteLine($"[ND500] Segment 31 'Other CPU' trap at PC=0x{PC:X8}");
                        TrapHandler.HandleOtherCPUTrap(this, targetAddress);
                        return;  // Don't execute normal CALLG
                    }
                }
            }

            // Normal instruction execution...
        }

        // Placeholder methods (implement in your actual CPU)
        private uint FetchInstruction(uint addr) { return 0; }
        private bool IsCALLGInstruction(uint inst) { return false; }
        private uint GetCALLGTarget(uint inst) { return 0; }
    }

    /// <summary>
    /// Handles ND-500 "Other CPU" traps (segment 31 calls to ND-100).
    /// This is the core of the monitor call mechanism.
    /// </summary>
    public class ND500TrapHandler
    {
        private readonly MultiportMemory _mpm;
        private readonly Interface3022_5015 _interface;
        private readonly Dictionary<byte, ND500TrapState> _savedStates = new Dictionary<byte, ND500TrapState>();

        public ND500TrapHandler(MultiportMemory mpm, Interface3022_5015 interface)
        {
            _mpm = mpm;
            _interface = interface;
        }

        /// <summary>
        /// Handle "Other CPU" trap when ND-500 calls segment 31.
        /// This is called when CALLG #0x1F000000 is executed.
        /// </summary>
        public void HandleOtherCPUTrap(ND500CPU cpu, uint targetAddress)
        {
            byte processNum = cpu.CurrentProcess;

            Console.WriteLine($"[ND500-TRAP] Process {processNum} triggered 'Other CPU' trap");
            Console.WriteLine($"[ND500-TRAP] Target address: 0x{targetAddress:X8}");

            // Step 1: Save ND-500 state
            ND500TrapState state = new ND500TrapState();
            state.Save(cpu);
            _savedStates[processNum] = state;

            Console.WriteLine($"[ND500-TRAP] Saved state: PC=0x{state.ProgramCounter:X8}, R0=0x{state.Registers[0]:X8}");

            // Step 2: Get process descriptor and message buffer addresses
            uint descriptorAddr = (uint)(processNum * MultiportMemory.PROCESS_DESCRIPTOR_SIZE * 2);
            ushort messageBufOffset = _mpm.ReadWord(descriptorAddr + 2);  // MESSBUFF field
            uint messageAddr = messageBufOffset;

            Console.WriteLine($"[ND500-TRAP] Process descriptor at 0x{descriptorAddr:X}, message buffer at 0x{messageAddr:X}");

            // Step 3: Read monitor call parameters from ND-500 registers
            // Typically:
            //   R0 = Function-specific parameter 1
            //   R1 = Function-specific parameter 2
            //   R2 = Function-specific parameter 3
            //   etc.
            // The MICFU code should already be in message buffer (written by user code)

            ushort micfu = _mpm.ReadWord(messageAddr + 6);  // MICFU at offset +3 words = +6 bytes
            Console.WriteLine($"[ND500-TRAP] MICFU code: 0x{micfu:X4} ({(MICFUCode)micfu})");

            // Step 4: Fill message buffer with trap information
            FillMessageBuffer(cpu, messageAddr, micfu);

            // Step 5: Set ITMQUEUE flag in message buffer
            ushort flags = _mpm.ReadWord(messageAddr + 2);  // 5MSFL at offset +1 word = +2 bytes
            flags |= ND500MessageBuffer.FLAG_IN_QUEUE;
            _mpm.WriteWord(messageAddr + 2, flags);

            Console.WriteLine($"[ND500-TRAP] Message buffer filled, ITMQUEUE flag set");

            // Step 6: Signal ND-100 via 3022 interface
            _interface.SignalND100MonitorCall(processNum, messageAddr);

            // Step 7: Put ND-500 process in WAIT state
            cpu.IsWaiting = true;

            Console.WriteLine($"[ND500-TRAP] Process {processNum} entering WAIT state for ND-100 response");
        }

        /// <summary>
        /// Fill message buffer with monitor call parameters from ND-500 registers.
        /// </summary>
        private void FillMessageBuffer(ND500CPU cpu, uint messageAddr, ushort micfu)
        {
            // The exact mapping depends on the MICFU code
            // Here's a general pattern for DVIO OUT (device output):

            switch ((MICFUCode)micfu)
            {
                case MICFUCode.DVIO_OUT:
                    // R0 = Device number
                    // R1 = Buffer address (ND-500 space)
                    // R2 = Byte count
                    _mpm.WriteWord(messageAddr + 28, (ushort)cpu.Registers[0]);  // 5DITN (device)
                    _mpm.WriteWord(messageAddr + 18, (ushort)(cpu.Registers[1] >> 16)); // N500A high
                    _mpm.WriteWord(messageAddr + 20, (ushort)(cpu.Registers[1] & 0xFFFF)); // N500A low
                    _mpm.WriteWord(messageAddr + 14, (ushort)(cpu.Registers[2] >> 16)); // NRBYT high
                    _mpm.WriteWord(messageAddr + 16, (ushort)(cpu.Registers[2] & 0xFFFF)); // NRBYT low
                    Console.WriteLine($"[ND500-TRAP] DVIO_OUT: device={cpu.Registers[0]}, addr=0x{cpu.Registers[1]:X8}, bytes={cpu.Registers[2]}");
                    break;

                case MICFUCode.DVIO_IN:
                    // Similar to DVIO_OUT
                    _mpm.WriteWord(messageAddr + 28, (ushort)cpu.Registers[0]);  // 5DITN
                    _mpm.WriteWord(messageAddr + 18, (ushort)(cpu.Registers[1] >> 16)); // N500A high
                    _mpm.WriteWord(messageAddr + 20, (ushort)(cpu.Registers[1] & 0xFFFF)); // N500A low
                    _mpm.WriteWord(messageAddr + 14, (ushort)(cpu.Registers[2] >> 16)); // NRBYT high
                    _mpm.WriteWord(messageAddr + 16, (ushort)(cpu.Registers[2] & 0xFFFF)); // NRBYT low
                    Console.WriteLine($"[ND500-TRAP] DVIO_IN: device={cpu.Registers[0]}, addr=0x{cpu.Registers[1]:X8}, bytes={cpu.Registers[2]}");
                    break;

                case MICFUCode.SWAP_PAGE:
                    // Page fault - send to swapper
                    // R0 = Faulting address
                    // R1 = Segment number
                    _mpm.WriteWord(messageAddr + 18, (ushort)(cpu.Registers[0] >> 16)); // Fault addr high
                    _mpm.WriteWord(messageAddr + 20, (ushort)(cpu.Registers[0] & 0xFFFF)); // Fault addr low
                    _mpm.WriteWord(messageAddr + 28, (ushort)cpu.Registers[1]); // Segment
                    Console.WriteLine($"[ND500-TRAP] SWAP_PAGE: addr=0x{cpu.Registers[0]:X8}, segment={cpu.Registers[1]}");
                    break;

                default:
                    Console.WriteLine($"[ND500-TRAP] WARNING: Unknown MICFU code 0x{micfu:X4}");
                    break;
            }
        }

        /// <summary>
        /// Resume ND-500 process after ND-100 completes monitor call.
        /// Called by ND-100 after processing.
        /// </summary>
        public void ResumeAfterMonitorCall(ND500CPU cpu, byte processNum)
        {
            Console.WriteLine($"[ND500-TRAP] Resuming process {processNum} after monitor call");

            if (!_savedStates.TryGetValue(processNum, out ND500TrapState state))
            {
                Console.WriteLine($"[ND500-TRAP] ERROR: No saved state for process {processNum}");
                return;
            }

            // Get message buffer to read result
            uint descriptorAddr = (uint)(processNum * MultiportMemory.PROCESS_DESCRIPTOR_SIZE * 2);
            ushort messageBufOffset = _mpm.ReadWord(descriptorAddr + 2);
            uint messageAddr = messageBufOffset;

            // Read error code from message buffer
            ushort errorCode = _mpm.ReadWord(messageAddr + 8);  // 5ERRC at offset +4 words = +8 bytes

            Console.WriteLine($"[ND500-TRAP] Monitor call result: error code = {errorCode}");

            // Restore ND-500 state
            state.Restore(cpu);

            // Put error code in R0 (standard convention)
            cpu.Registers[0] = errorCode;

            // Advance PC past the CALLG instruction (typically 4 bytes)
            cpu.PC += 4;

            // Clear WAIT state
            cpu.IsWaiting = false;

            // Clear ITMQUEUE flag
            ushort flags = _mpm.ReadWord(messageAddr + 2);
            flags &= (ushort)~ND500MessageBuffer.FLAG_IN_QUEUE;
            _mpm.WriteWord(messageAddr + 2, flags);

            Console.WriteLine($"[ND500-TRAP] Process {processNum} resumed at PC=0x{cpu.PC:X8}");
        }
    }
}
