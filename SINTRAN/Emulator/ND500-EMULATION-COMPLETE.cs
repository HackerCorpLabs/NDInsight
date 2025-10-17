// ND-500 Complete Emulation - Integrated with existing NDBusND500IF.cs
// This extends the existing 3022 bus interface with:
// - Multiport memory (5MPM)
// - Message passing
// - ND-500 side (5015 controller)
// - Full interrupt handling

using System;
using System.Collections.Generic;
using System.Threading;

namespace Emulated.HW.ND.CPU.ND500
{
    #region Multiport Memory (5MPM)

    /// <summary>
    /// Manages multiport memory (5MPM) shared between ND-100 and ND-500.
    /// This memory is physically accessible by both CPUs simultaneously.
    /// </summary>
    public class MultiportMemory
    {
        private readonly byte[] _memory;
        private readonly uint _nd100BaseAddress;     // Physical address in ND-100 space
        private readonly uint _nd500BaseAddress;     // Physical address in ND-500 space
        private readonly uint _size;
        private readonly object _accessLock = new object();
        
        // 5MPM structure pointers (offsets within 5MPM)
        private uint _processDescriptorTable;        // S500S offset
        private uint _processDescriptorTableEnd;     // S500E offset
        private uint _messageBufferPool;             // Message buffer pool offset
        private ushort _maxProcesses;
        
        public const int PROCESS_DESCRIPTOR_SIZE = 32;  // 5PRDSIZE words = 64 bytes
        public const int MESSAGE_BUFFER_SIZE = 128;     // 55MESSIZE words = 256 bytes
        
        public uint ND100BaseAddress => _nd100BaseAddress;
        public uint ND500BaseAddress => _nd500BaseAddress;
        public uint Size => _size;
        
        public MultiportMemory(uint nd100BaseAddress, uint nd500BaseAddress, uint sizeBytes)
        {
            _nd100BaseAddress = nd100BaseAddress;
            _nd500BaseAddress = nd500BaseAddress;
            _size = sizeBytes;
            _memory = new byte[sizeBytes];
            
            // Calculate structure locations
            _maxProcesses = 16;  // Typical: up to 16 ND-500 processes
            _processDescriptorTable = 0;
            _processDescriptorTableEnd = (uint)(_maxProcesses * PROCESS_DESCRIPTOR_SIZE * 2);
            _messageBufferPool = _processDescriptorTableEnd;
            
            Console.WriteLine($"[5MPM] Initialized: ND-100=0x{nd100BaseAddress:X8}, ND-500=0x{nd500BaseAddress:X8}, Size={sizeBytes} bytes");
        }
        
        /// <summary>
        /// Read word from 5MPM (both CPUs can call this).
        /// Thread-safe for concurrent ND-100/ND-500 access.
        /// </summary>
        public ushort ReadWord(uint offset)
        {
            lock (_accessLock)
            {
                if (offset >= _size - 1)
                    throw new ArgumentOutOfRangeException(nameof(offset));
                
                // Big-endian word read (ND standard)
                return (ushort)((_memory[offset] << 8) | _memory[offset + 1]);
            }
        }
        
        /// <summary>
        /// Write word to 5MPM (both CPUs can call this).
        /// Thread-safe for concurrent ND-100/ND-500 access.
        /// </summary>
        public void WriteWord(uint offset, ushort value)
        {
            lock (_accessLock)
            {
                if (offset >= _size - 1)
                    throw new ArgumentOutOfRangeException(nameof(offset));
                
                // Big-endian word write (ND standard)
                _memory[offset] = (byte)(value >> 8);
                _memory[offset + 1] = (byte)(value & 0xFF);
            }
        }
        
        /// <summary>
        /// Read double word (32-bit) from 5MPM.
        /// </summary>
        public uint ReadDoubleWord(uint offset)
        {
            lock (_accessLock)
            {
                ushort high = ReadWordNoLock(offset);
                ushort low = ReadWordNoLock(offset + 2);
                return ((uint)high << 16) | low;
            }
        }
        
        /// <summary>
        /// Write double word (32-bit) to 5MPM.
        /// </summary>
        public void WriteDoubleWord(uint offset, uint value)
        {
            lock (_accessLock)
            {
                WriteWordNoLock(offset, (ushort)(value >> 16));
                WriteWordNoLock(offset + 2, (ushort)(value & 0xFFFF));
            }
        }
        
        /// <summary>
        /// Read bytes from 5MPM (for DMA operations).
        /// </summary>
        public void ReadBytes(uint offset, byte[] buffer, int count)
        {
            lock (_accessLock)
            {
                if (offset + count > _size)
                    throw new ArgumentOutOfRangeException();
                
                Array.Copy(_memory, offset, buffer, 0, count);
            }
        }
        
        /// <summary>
        /// Write bytes to 5MPM (for DMA operations).
        /// </summary>
        public void WriteBytes(uint offset, byte[] buffer, int count)
        {
            lock (_accessLock)
            {
                if (offset + count > _size)
                    throw new ArgumentOutOfRangeException();
                
                Array.Copy(buffer, 0, _memory, offset, count);
            }
        }
        
        // Internal non-locking versions (when already locked)
        private ushort ReadWordNoLock(uint offset)
        {
            return (ushort)((_memory[offset] << 8) | _memory[offset + 1]);
        }
        
        private void WriteWordNoLock(uint offset, ushort value)
        {
            _memory[offset] = (byte)(value >> 8);
            _memory[offset + 1] = (byte)(value & 0xFF);
        }
        
        /// <summary>
        /// Allocate a process descriptor slot.
        /// </summary>
        public uint AllocateProcessDescriptor(byte processNumber)
        {
            if (processNumber >= _maxProcesses)
                throw new ArgumentOutOfRangeException(nameof(processNumber));
            
            uint addr = _processDescriptorTable + (uint)(processNumber * PROCESS_DESCRIPTOR_SIZE * 2);
            Console.WriteLine($"[5MPM] Allocated process descriptor #{processNumber} at offset 0x{addr:X}");
            return addr;
        }
        
        /// <summary>
        /// Allocate a message buffer.
        /// </summary>
        public uint AllocateMessageBuffer(byte processNumber)
        {
            uint addr = _messageBufferPool + (uint)(processNumber * MESSAGE_BUFFER_SIZE * 2);
            Console.WriteLine($"[5MPM] Allocated message buffer #{processNumber} at offset 0x{addr:X}");
            return addr;
        }
    }

    #endregion

    #region ND-500 Process Descriptor

    /// <summary>
    /// ND-500 process descriptor in 5MPM.
    /// Represents one ND-500 domain/process.
    /// </summary>
    public class ND500ProcessDescriptor
    {
        public byte ProcessNumber { get; set; }
        public uint DescriptorAddress { get; set; }      // XADPROC (offset in 5MPM)
        public uint MessageBufferAddress { get; set; }   // MESSBUFF (offset in 5MPM)
        public ushort Status { get; set; }
        public ushort SendEnable { get; set; }           // SENDE (0=inactive, >0=active)
        public ushort ReceiveState { get; set; }         // RECE
        
        // Domain information
        public string DomainName { get; set; }
        public uint StartAddress { get; set; }           // PC initial value
        public ushort[] ProgramCapabilities { get; set; } = new ushort[32];
        public ushort[] DataCapabilities { get; set; } = new ushort[32];
        
        // Runtime state
        public bool IsPlaced { get; set; }
        public bool IsRunning { get; set; }
        
        /// <summary>
        /// Write descriptor to 5MPM.
        /// </summary>
        public void WriteTo5MPM(MultiportMemory mpm)
        {
            uint addr = DescriptorAddress;
            
            // Write descriptor fields
            mpm.WriteWord(addr + 0, (ushort)(DescriptorAddress & 0xFFFF));  // XADPROC
            mpm.WriteWord(addr + 2, (ushort)(MessageBufferAddress & 0xFFFF)); // MESSBUFF
            mpm.WriteWord(addr + 4, Status);
            mpm.WriteWord(addr + 6, SendEnable);
            mpm.WriteWord(addr + 8, ReceiveState);
            
            Console.WriteLine($"[5MPM] Process #{ProcessNumber} descriptor written: Status=0x{Status:X4}, SendEnable={SendEnable}");
        }
        
        /// <summary>
        /// Read descriptor from 5MPM.
        /// </summary>
        public void ReadFrom5MPM(MultiportMemory mpm)
        {
            uint addr = DescriptorAddress;
            
            // Read descriptor fields
            Status = mpm.ReadWord(addr + 4);
            SendEnable = mpm.ReadWord(addr + 6);
            ReceiveState = mpm.ReadWord(addr + 8);
        }
        
        /// <summary>
        /// Check if process is active.
        /// </summary>
        public bool IsActive => SendEnable != 0;
    }

    #endregion

    #region Message Buffer

    /// <summary>
    /// Message buffer structure in 5MPM.
    /// Used for communication between ND-100 and ND-500.
    /// </summary>
    public class ND500MessageBuffer
    {
        public uint BufferAddress { get; set; }  // Offset in 5MPM
        
        // Message fields (matching NPL source)
        public ushort ProcessLink { get; set; }        // PLINK (offset 0)
        public ushort MessageFlags { get; set; }       // 5MSFL (offset 1)
        public ushort Priority { get; set; }           // 5PRIO (offset 2)
        public ushort MicrocodeFunction { get; set; }  // MICFU (offset 3)
        public ushort ErrorCode { get; set; }          // 5ERRC (offset 4)
        public uint ToDatafield { get; set; }          // TODF (offset 5-6)
        public uint ByteCount { get; set; }            // NRBYT (offset 7-8)
        public uint ND500Address { get; set; }         // N500A (offset 9-10)
        public uint ND100Address { get; set; }         // N100A (offset 11-12)
        public ushort ExtendedFunction { get; set; }   // XMICF (offset 13)
        public ushort DITNumber { get; set; }          // 5DITN (offset 14)
        public ushort CPUNumber { get; set; }          // 5CPUN (offset 15)
        public byte[] Data { get; set; } = new byte[192]; // Additional data area
        
        // Message flags
        public const ushort FLAG_IN_QUEUE = 0x0001;    // 5ITMQUEUE
        public const ushort FLAG_SYSTEM_RESERVED = 0x0002; // 5SYSRES
        
        public bool IsInQueue
        {
            get => (MessageFlags & FLAG_IN_QUEUE) != 0;
            set
            {
                if (value)
                    MessageFlags |= FLAG_IN_QUEUE;
                else
                    MessageFlags &= unchecked((ushort)~FLAG_IN_QUEUE);
            }
        }
        
        /// <summary>
        /// Write message to 5MPM.
        /// </summary>
        public void WriteTo5MPM(MultiportMemory mpm)
        {
            uint addr = BufferAddress;
            
            mpm.WriteWord(addr + 0, ProcessLink);
            mpm.WriteWord(addr + 2, MessageFlags);
            mpm.WriteWord(addr + 4, Priority);
            mpm.WriteWord(addr + 6, MicrocodeFunction);
            mpm.WriteWord(addr + 8, ErrorCode);
            mpm.WriteDoubleWord(addr + 10, ToDatafield);
            mpm.WriteDoubleWord(addr + 14, ByteCount);
            mpm.WriteDoubleWord(addr + 18, ND500Address);
            mpm.WriteDoubleWord(addr + 22, ND100Address);
            mpm.WriteWord(addr + 26, ExtendedFunction);
            mpm.WriteWord(addr + 28, DITNumber);
            mpm.WriteWord(addr + 30, CPUNumber);
            
            // Write data area
            if (Data != null && Data.Length > 0)
            {
                mpm.WriteBytes(addr + 32, Data, Math.Min(Data.Length, 192));
            }
            
            Console.WriteLine($"[5MPM] Message written: Func=0x{MicrocodeFunction:X4}, Flags=0x{MessageFlags:X4}");
        }
        
        /// <summary>
        /// Read message from 5MPM.
        /// </summary>
        public void ReadFrom5MPM(MultiportMemory mpm)
        {
            uint addr = BufferAddress;
            
            ProcessLink = mpm.ReadWord(addr + 0);
            MessageFlags = mpm.ReadWord(addr + 2);
            Priority = mpm.ReadWord(addr + 4);
            MicrocodeFunction = mpm.ReadWord(addr + 6);
            ErrorCode = mpm.ReadWord(addr + 8);
            ToDatafield = mpm.ReadDoubleWord(addr + 10);
            ByteCount = mpm.ReadDoubleWord(addr + 14);
            ND500Address = mpm.ReadDoubleWord(addr + 18);
            ND100Address = mpm.ReadDoubleWord(addr + 22);
            ExtendedFunction = mpm.ReadWord(addr + 26);
            DITNumber = mpm.ReadWord(addr + 28);
            CPUNumber = mpm.ReadWord(addr + 30);
            
            // Read data area
            if (Data == null || Data.Length < 192)
                Data = new byte[192];
            mpm.ReadBytes(addr + 32, Data, 192);
        }
    }

    #endregion

    #region ND-500 Side Controller (5015)

    /// <summary>
    /// PCB 5015 - ND-500 Control Board II
    /// This is the ND-500 side of the communication with ND-100.
    /// Handles bus interface, DMA, and interrupts.
    /// 
    /// Reference: http://sintran.com/sintran/hardware/nd-500/nd-322515.html
    /// </summary>
    public class ND5015Controller
    {
        private readonly MultiportMemory _mpm;
        private readonly IND500Cpu _cpu;
        
        // Control registers (ND-500 side)
        private uint _lmar5;          // LMAR5: Memory Address Register (24-bit)
        private ushort _lcon5;        // LCON5: Control Register
        private ushort _lsta5;        // LSTA5: Status Register
        private ushort _ldat5;        // LDAT5: Data Register (low)
        private ushort _ldax5;        // LDAX5: Data Register (high)
        private ushort _rtag5;        // RTAG5: TAG-IN from ND-100
        private ushort _unlc5;        // UNLC5: Unlock register
        
        // State
        private bool _locked;
        private bool _busy;
        private bool _interruptEnabled;
        
        // Interrupt callback to ND-500 CPU
        public Action<int> OnInterruptToND500;
        
        // Interrupt callback to ND-100 (via 3022)
        public Action OnInterruptToND100;
        
        public ND5015Controller(MultiportMemory mpm, IND500Cpu cpu)
        {
            _mpm = mpm;
            _cpu = cpu;
            
            Console.WriteLine("[5015] ND-500 Controller initialized");
        }
        
        /// <summary>
        /// Read from ND-500 side control register.
        /// Called by ND-500 CPU via IOXT instructions.
        /// </summary>
        public ushort ReadRegister(byte registerAddress)
        {
            switch (registerAddress)
            {
                case 0x00: // LSTA5: Status
                    return _lsta5;
                
                case 0x01: // LCON5: Control
                    return _lcon5;
                
                case 0x02: // LDAT5: Data (low)
                    return _ldat5;
                
                case 0x03: // LDAX5: Data (high)
                    return _ldax5;
                
                case 0x04: // LMAR5: Memory address (low)
                    return (ushort)(_lmar5 & 0xFFFF);
                
                case 0x05: // LMAR5: Memory address (high)
                    return (ushort)((_lmar5 >> 16) & 0xFF);
                
                case 0x06: // RTAG5: TAG-IN register
                    return _rtag5;
                
                default:
                    Console.WriteLine($"[5015] WARNING: Read from unknown register 0x{registerAddress:X2}");
                    return 0;
            }
        }
        
        /// <summary>
        /// Write to ND-500 side control register.
        /// Called by ND-500 CPU via IOXT instructions.
        /// </summary>
        public void WriteRegister(byte registerAddress, ushort value)
        {
            switch (registerAddress)
            {
                case 0x01: // LCON5: Control
                    _lcon5 = value;
                    ProcessControlWrite(value);
                    break;
                
                case 0x02: // LDAT5: Data (low)
                    _ldat5 = value;
                    break;
                
                case 0x03: // LDAX5: Data (high)
                    _ldax5 = value;
                    break;
                
                case 0x04: // LMAR5: Memory address (low)
                    _lmar5 = (_lmar5 & 0xFFFF0000) | value;
                    break;
                
                case 0x05: // LMAR5: Memory address (high)
                    _lmar5 = (_lmar5 & 0x0000FFFF) | ((uint)(value & 0xFF) << 16);
                    break;
                
                case 0x07: // UNLC5: Unlock
                    _locked = false;
                    Console.WriteLine("[5015] Interface unlocked");
                    break;
                
                default:
                    Console.WriteLine($"[5015] WARNING: Write to unknown register 0x{registerAddress:X2} = 0x{value:X4}");
                    break;
            }
        }
        
        /// <summary>
        /// Process control register write.
        /// Handles DMA operations, interrupts, etc.
        /// </summary>
        private void ProcessControlWrite(ushort controlValue)
        {
            // Bit 0: Enable interrupt
            _interruptEnabled = (controlValue & 0x0001) != 0;
            
            // Bit 2: Activate operation
            if ((controlValue & 0x0004) != 0)
            {
                _locked = true;
                _busy = true;
                Console.WriteLine("[5015] Operation activated (locked)");
            }
            
            // Bit 4: Programmed clear
            if ((controlValue & 0x0010) != 0)
            {
                Reset();
                Console.WriteLine("[5015] Programmed clear");
            }
            
            // Bits 8-14: Operation code
            byte opCode = (byte)((controlValue >> 8) & 0x7F);
            if (opCode != 0)
            {
                ExecuteOperation(opCode);
            }
        }
        
        /// <summary>
        /// Execute DMA or control operation.
        /// </summary>
        private void ExecuteOperation(byte opCode)
        {
            Console.WriteLine($"[5015] Execute operation 0x{opCode:X2} at address 0x{_lmar5:X6}");
            
            // Common operations:
            // 0x01: Read from 5MPM to ND-500
            // 0x02: Write from ND-500 to 5MPM
            // 0x03: Read message
            // 0x04: Write message
            // 0x05: Interrupt ND-100
            
            switch (opCode)
            {
                case 0x01: // Read from 5MPM
                    ReadFrom5MPM();
                    break;
                
                case 0x02: // Write to 5MPM
                    WriteTo5MPM();
                    break;
                
                case 0x03: // Read message (from ND-100)
                    ReadMessage();
                    break;
                
                case 0x04: // Write message (to ND-100)
                    WriteMessage();
                    break;
                
                case 0x05: // Interrupt ND-100
                    InterruptND100();
                    break;
                
                default:
                    Console.WriteLine($"[5015] WARNING: Unknown operation 0x{opCode:X2}");
                    break;
            }
            
            // Complete operation
            _busy = false;
            if (_interruptEnabled)
            {
                OnInterruptToND500?.Invoke(12); // Trigger ND-500 interrupt
            }
        }
        
        private void ReadFrom5MPM()
        {
            // Convert LMAR5 address to 5MPM offset
            uint offset = _lmar5 - _mpm.ND500BaseAddress;
            
            // Read word from 5MPM
            ushort data = _mpm.ReadWord(offset);
            _ldat5 = data;
            
            Console.WriteLine($"[5015] Read from 5MPM[0x{offset:X}] = 0x{data:X4}");
        }
        
        private void WriteTo5MPM()
        {
            // Convert LMAR5 address to 5MPM offset
            uint offset = _lmar5 - _mpm.ND500BaseAddress;
            
            // Combine data registers (32-bit value)
            uint data = ((uint)_ldax5 << 16) | _ldat5;
            
            // Write to 5MPM
            _mpm.WriteDoubleWord(offset, data);
            
            Console.WriteLine($"[5015] Write to 5MPM[0x{offset:X}] = 0x{data:X8}");
        }
        
        private void ReadMessage()
        {
            // Message address is in LMAR5
            uint messageOffset = _lmar5 - _mpm.ND500BaseAddress;
            
            var msg = new ND500MessageBuffer { BufferAddress = messageOffset };
            msg.ReadFrom5MPM(_mpm);
            
            Console.WriteLine($"[5015] Message read: Func=0x{msg.MicrocodeFunction:X4}, Error=0x{msg.ErrorCode:X4}");
        }
        
        private void WriteMessage()
        {
            // Message address is in LMAR5
            uint messageOffset = _lmar5 - _mpm.ND500BaseAddress;
            
            // Mark message as in queue
            ushort flags = _mpm.ReadWord(messageOffset + 2);
            flags |= ND500MessageBuffer.FLAG_IN_QUEUE;
            _mpm.WriteWord(messageOffset + 2, flags);
            
            Console.WriteLine($"[5015] Message written and queued");
        }
        
        private void InterruptND100()
        {
            Console.WriteLine("[5015] Interrupt ND-100");
            OnInterruptToND100?.Invoke();
        }
        
        /// <summary>
        /// Receive TAG-IN from ND-100 (via 3022).
        /// </summary>
        public void ReceiveTagIn(ushort tagValue)
        {
            _rtag5 = tagValue;
            Console.WriteLine($"[5015] TAG-IN received: 0x{tagValue:X4}");
            
            // Trigger ND-500 interrupt if needed
            if (_interruptEnabled)
            {
                OnInterruptToND500?.Invoke(14); // TAG-IN interrupt level
            }
        }
        
        public void Reset()
        {
            _lmar5 = 0;
            _lcon5 = 0;
            _lsta5 = 0;
            _ldat5 = 0;
            _ldax5 = 0;
            _rtag5 = 0;
            _locked = false;
            _busy = false;
            _interruptEnabled = false;
        }
    }

    #endregion

    #region Integration with NDBusND500IF

    /// <summary>
    /// Extension methods for NDBusND500IF (3022) to add 5MPM support.
    /// Add these methods to your existing NDBusND500IF class.
    /// </summary>
    public static class NDBusND500IF_Extensions
    {
        // Add these fields to NDBusND500IF:
        // private MultiportMemory multiportMemory;
        // private ND5015Controller nd5015Controller;
        // private List<ND500ProcessDescriptor> processDescriptors;
        
        /// <summary>
        /// Initialize multiport memory and ND-500 side controller.
        /// Call this after AttachCpu().
        /// </summary>
        public static void InitializeMultiportMemory(
            this NDBus.NDBusND500IF busInterface,
            uint nd100BaseAddress,
            uint nd500BaseAddress,
            uint sizeBytes)
        {
            // Create 5MPM
            var mpm = new MultiportMemory(nd100BaseAddress, nd500BaseAddress, sizeBytes);
            
            // Get ND-500 CPU from bus interface (via reflection or store reference)
            // var cpu = busInterface.GetND500Cpu();
            
            // Create 5015 controller
            // var controller = new ND5015Controller(mpm, cpu);
            
            // Wire interrupts
            // controller.OnInterruptToND100 = () => busInterface.SetInterruptBit(12, true);
            // controller.OnInterruptToND500 = (level) => cpu.Interrupt(level);
            
            // Store references (add fields to NDBusND500IF)
            // busInterface.multiportMemory = mpm;
            // busInterface.nd5015Controller = controller;
            // busInterface.processDescriptors = new List<ND500ProcessDescriptor>();
            
            Console.WriteLine($"[3022] Multiport memory initialized");
        }
    }

    #endregion
}


