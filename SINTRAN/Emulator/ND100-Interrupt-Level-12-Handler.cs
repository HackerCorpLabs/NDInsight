// ND-100 Interrupt Level 12 Handler
// Handles interrupts from ND-500 (monitor calls, page faults, etc.)
// This is the ND-100 side of the monitor call mechanism

using System;
using System.Collections.Generic;

namespace Emulated.HW.ND.CPU.ND500
{
    /// <summary>
    /// ND-100 interrupt handler for ND-500 requests.
    /// Processes monitor calls from ND-500 processes.
    /// </summary>
    public class ND100InterruptLevel12Handler
    {
        private readonly MultiportMemory _mpm;
        private readonly Interface3022_5015 _interface;
        private readonly ND100DeviceManager _deviceManager;  // For actual I/O
        private readonly ND100FileSystem _fileSystem;        // For file operations

        public ND100InterruptLevel12Handler(
            MultiportMemory mpm,
            Interface3022_5015 interface,
            ND100DeviceManager deviceManager,
            ND100FileSystem fileSystem)
        {
            _mpm = mpm;
            _interface = interface;
            _deviceManager = deviceManager;
            _fileSystem = fileSystem;
        }

        /// <summary>
        /// Main interrupt handler for level 12.
        /// Called by ND-100 CPU when interrupt occurs.
        /// </summary>
        public void HandleInterrupt(ND100InterruptData data)
        {
            Console.WriteLine($"[ND100-INT12] Interrupt from ND-500 process {data.ProcessNumber}");
            Console.WriteLine($"[ND100-INT12] TAG: {(TAGValue)data.TAGValue}");
            Console.WriteLine($"[ND100-INT12] Message at: 0x{data.MessageAddress:X}");

            // Read message buffer from 5MPM
            ND500MessageBuffer message = ReadMessageBuffer(data.MessageAddress);

            // Dispatch based on MICFU code
            ushort result = DispatchMonitorCall(message);

            // Write result back to message buffer
            WriteResult(data.MessageAddress, result);

            // Clear ITMQUEUE flag
            ClearMessageQueueFlag(data.MessageAddress);

            // Signal ND-500 that operation is complete
            SignalOperationComplete();

            Console.WriteLine($"[ND100-INT12] Monitor call completed with result: {result}");
        }

        /// <summary>
        /// Read message buffer from 5MPM.
        /// </summary>
        private ND500MessageBuffer ReadMessageBuffer(uint address)
        {
            ND500MessageBuffer msg = new ND500MessageBuffer
            {
                BufferAddress = address
            };

            // Read message buffer fields
            msg.ProcessLink = _mpm.ReadWord(address + 0);
            msg.MessageFlags = _mpm.ReadWord(address + 2);
            msg.Priority = _mpm.ReadWord(address + 4);
            msg.MicrocodeFunction = _mpm.ReadWord(address + 6);
            msg.ErrorCode = _mpm.ReadWord(address + 8);

            // Read double-word fields
            ushort todfHigh = _mpm.ReadWord(address + 10);
            ushort todfLow = _mpm.ReadWord(address + 12);
            msg.ToDatafield = ((uint)todfHigh << 16) | todfLow;

            ushort nrbytHigh = _mpm.ReadWord(address + 14);
            ushort nrbytLow = _mpm.ReadWord(address + 16);
            msg.ByteCount = ((uint)nrbytHigh << 16) | nrbytLow;

            ushort n500aHigh = _mpm.ReadWord(address + 18);
            ushort n500aLow = _mpm.ReadWord(address + 20);
            msg.ND500Address = ((uint)n500aHigh << 16) | n500aLow;

            ushort n100aHigh = _mpm.ReadWord(address + 22);
            ushort n100aLow = _mpm.ReadWord(address + 24);
            msg.ND100Address = ((uint)n100aHigh << 16) | n100aLow;

            msg.ExtendedFunction = _mpm.ReadWord(address + 26);
            msg.DITNumber = _mpm.ReadWord(address + 28);
            msg.CPUNumber = _mpm.ReadWord(address + 30);

            Console.WriteLine($"[ND100-INT12] Message: MICFU=0x{msg.MicrocodeFunction:X4} ({(MICFUCode)msg.MicrocodeFunction})");
            Console.WriteLine($"[ND100-INT12]   Device={msg.DITNumber}, ByteCount={msg.ByteCount}, ND500Addr=0x{msg.ND500Address:X8}");

            return msg;
        }

        /// <summary>
        /// Dispatch monitor call based on MICFU code.
        /// Returns error code (0 = success).
        /// </summary>
        private ushort DispatchMonitorCall(ND500MessageBuffer message)
        {
            MICFUCode micfu = (MICFUCode)message.MicrocodeFunction;

            try
            {
                switch (micfu)
                {
                    case MICFUCode.DVIO_OUT:
                        return HandleDVIOOut(message);

                    case MICFUCode.DVIO_IN:
                        return HandleDVIOIn(message);

                    case MICFUCode.OPEN_FILE:
                        return HandleOpenFile(message);

                    case MICFUCode.CLOSE_FILE:
                        return HandleCloseFile(message);

                    case MICFUCode.READ_FILE:
                        return HandleReadFile(message);

                    case MICFUCode.WRITE_FILE:
                        return HandleWriteFile(message);

                    case MICFUCode.GET_TIME:
                        return HandleGetTime(message);

                    case MICFUCode.SWAP_PAGE:
                        return HandleSwapPage(message);

                    default:
                        Console.WriteLine($"[ND100-INT12] ERROR: Unknown MICFU 0x{message.MicrocodeFunction:X4}");
                        return 1;  // Error: Unknown function
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[ND100-INT12] EXCEPTION in monitor call: {ex.Message}");
                return 0xFFFF;  // Error
            }
        }

        #region Monitor Call Handlers

        /// <summary>
        /// DVIO OUT - Device output (write to device).
        /// </summary>
        private ushort HandleDVIOOut(ND500MessageBuffer message)
        {
            ushort deviceNum = message.DITNumber;
            uint nd500Addr = message.ND500Address;
            uint byteCount = message.ByteCount;

            Console.WriteLine($"[ND100-INT12] DVIO_OUT: device={deviceNum}, addr=0x{nd500Addr:X8}, bytes={byteCount}");

            // Read data from ND-500 memory (via 5MPM)
            byte[] data = new byte[byteCount];
            for (uint i = 0; i < byteCount; i++)
            {
                // ND-500 address space needs translation
                // For simplicity, assume it's in 5MPM for now
                data[i] = _mpm.ReadByte(nd500Addr + i);
            }

            // Write to actual device
            bool success = _deviceManager.WriteToDevice(deviceNum, data);

            return success ? (ushort)0 : (ushort)1;
        }

        /// <summary>
        /// DVIO IN - Device input (read from device).
        /// </summary>
        private ushort HandleDVIOIn(ND500MessageBuffer message)
        {
            ushort deviceNum = message.DITNumber;
            uint nd500Addr = message.ND500Address;
            uint byteCount = message.ByteCount;

            Console.WriteLine($"[ND100-INT12] DVIO_IN: device={deviceNum}, addr=0x{nd500Addr:X8}, bytes={byteCount}");

            // Read from actual device
            byte[] data = _deviceManager.ReadFromDevice(deviceNum, (int)byteCount);

            if (data == null)
                return 1;  // Error

            // Write data to ND-500 memory (via 5MPM)
            for (uint i = 0; i < data.Length; i++)
            {
                _mpm.WriteByte(nd500Addr + i, data[i]);
            }

            return 0;  // Success
        }

        /// <summary>
        /// OPEN FILE - Open file for ND-500 process.
        /// </summary>
        private ushort HandleOpenFile(ND500MessageBuffer message)
        {
            Console.WriteLine($"[ND100-INT12] OPEN_FILE");

            // Read filename from message buffer extended area
            string filename = ReadStringFromMessage(message, 32);

            // Open file via SINTRAN file system
            int fileHandle = _fileSystem.OpenFile(filename);

            if (fileHandle < 0)
                return 1;  // Error

            // Store file handle in message buffer
            _mpm.WriteWord(message.BufferAddress + 10, (ushort)fileHandle);

            return 0;  // Success
        }

        /// <summary>
        /// CLOSE FILE - Close file for ND-500 process.
        /// </summary>
        private ushort HandleCloseFile(ND500MessageBuffer message)
        {
            Console.WriteLine($"[ND100-INT12] CLOSE_FILE");

            int fileHandle = _mpm.ReadWord(message.BufferAddress + 10);

            bool success = _fileSystem.CloseFile(fileHandle);

            return success ? (ushort)0 : (ushort)1;
        }

        /// <summary>
        /// READ FILE - Read from file.
        /// </summary>
        private ushort HandleReadFile(ND500MessageBuffer message)
        {
            int fileHandle = _mpm.ReadWord(message.BufferAddress + 10);
            uint nd500Addr = message.ND500Address;
            uint byteCount = message.ByteCount;

            Console.WriteLine($"[ND100-INT12] READ_FILE: handle={fileHandle}, bytes={byteCount}");

            byte[] data = _fileSystem.ReadFile(fileHandle, (int)byteCount);

            if (data == null)
                return 1;  // Error

            // Write to ND-500 memory
            for (uint i = 0; i < data.Length; i++)
            {
                _mpm.WriteByte(nd500Addr + i, data[i]);
            }

            // Update actual bytes read
            _mpm.WriteWord(message.BufferAddress + 14, (ushort)(data.Length >> 16));
            _mpm.WriteWord(message.BufferAddress + 16, (ushort)(data.Length & 0xFFFF));

            return 0;  // Success
        }

        /// <summary>
        /// WRITE FILE - Write to file.
        /// </summary>
        private ushort HandleWriteFile(ND500MessageBuffer message)
        {
            int fileHandle = _mpm.ReadWord(message.BufferAddress + 10);
            uint nd500Addr = message.ND500Address;
            uint byteCount = message.ByteCount;

            Console.WriteLine($"[ND100-INT12] WRITE_FILE: handle={fileHandle}, bytes={byteCount}");

            // Read from ND-500 memory
            byte[] data = new byte[byteCount];
            for (uint i = 0; i < byteCount; i++)
            {
                data[i] = _mpm.ReadByte(nd500Addr + i);
            }

            bool success = _fileSystem.WriteFile(fileHandle, data);

            return success ? (ushort)0 : (ushort)1;
        }

        /// <summary>
        /// GET TIME - Get current system time.
        /// </summary>
        private ushort HandleGetTime(ND500MessageBuffer message)
        {
            Console.WriteLine($"[ND100-INT12] GET_TIME");

            // Get current time
            DateTime now = DateTime.Now;

            // Convert to SINTRAN time format (days since 1970, milliseconds)
            TimeSpan since1970 = now - new DateTime(1970, 1, 1);
            uint days = (uint)since1970.TotalDays;
            uint milliseconds = (uint)(since1970.TotalMilliseconds % 86400000);

            // Write to message buffer
            _mpm.WriteWord(message.BufferAddress + 32, (ushort)(days >> 16));
            _mpm.WriteWord(message.BufferAddress + 34, (ushort)(days & 0xFFFF));
            _mpm.WriteWord(message.BufferAddress + 36, (ushort)(milliseconds >> 16));
            _mpm.WriteWord(message.BufferAddress + 38, (ushort)(milliseconds & 0xFFFF));

            return 0;  // Success
        }

        /// <summary>
        /// SWAP PAGE - Handle page fault (send to swapper).
        /// </summary>
        private ushort HandleSwapPage(ND500MessageBuffer message)
        {
            uint faultAddr = message.ND500Address;
            ushort segment = _mpm.ReadWord(message.BufferAddress + 28);

            Console.WriteLine($"[ND100-INT12] SWAP_PAGE: addr=0x{faultAddr:X8}, segment={segment}");

            // Activate swapper (process 0) to handle this
            // This is a simplified version - real implementation would queue to swapper

            Console.WriteLine($"[ND100-INT12] Page fault sent to swapper");

            return 0;  // Success (swapper will handle)
        }

        #endregion

        #region Helper Methods

        /// <summary>
        /// Write result to message buffer.
        /// </summary>
        private void WriteResult(uint messageAddr, ushort errorCode)
        {
            _mpm.WriteWord(messageAddr + 8, errorCode);  // 5ERRC at offset +4 words = +8 bytes
        }

        /// <summary>
        /// Clear ITMQUEUE flag in message buffer.
        /// </summary>
        private void ClearMessageQueueFlag(uint messageAddr)
        {
            ushort flags = _mpm.ReadWord(messageAddr + 2);
            flags &= (ushort)~ND500MessageBuffer.FLAG_IN_QUEUE;
            _mpm.WriteWord(messageAddr + 2, flags);
        }

        /// <summary>
        /// Signal ND-500 that operation is complete.
        /// </summary>
        private void SignalOperationComplete()
        {
            // Write TAG-IN to ND-500
            _interface.HandleIOX(Interface3022_5015.LTAG5, (ushort)TAGValue.OPERATION_COMPLETE);
        }

        /// <summary>
        /// Read string from message buffer extended area.
        /// </summary>
        private string ReadStringFromMessage(ND500MessageBuffer message, int maxLength)
        {
            List<byte> bytes = new List<byte>();

            for (int i = 0; i < maxLength; i++)
            {
                byte b = _mpm.ReadByte(message.BufferAddress + 32 + (uint)i);
                if (b == 0)
                    break;
                bytes.Add(b);
            }

            return System.Text.Encoding.ASCII.GetString(bytes.ToArray());
        }

        #endregion
    }

    #region Device and File System Stubs

    /// <summary>
    /// ND-100 device manager (stub - implement with your actual device emulation).
    /// </summary>
    public class ND100DeviceManager
    {
        public bool WriteToDevice(ushort deviceNum, byte[] data)
        {
            Console.WriteLine($"[DeviceManager] Write to device {deviceNum}: {data.Length} bytes");
            // Implement actual device I/O here
            return true;
        }

        public byte[] ReadFromDevice(ushort deviceNum, int byteCount)
        {
            Console.WriteLine($"[DeviceManager] Read from device {deviceNum}: {byteCount} bytes");
            // Implement actual device I/O here
            return new byte[byteCount];
        }
    }

    /// <summary>
    /// ND-100 file system (stub - implement with your actual SINTRAN file system).
    /// </summary>
    public class ND100FileSystem
    {
        private readonly Dictionary<int, string> _openFiles = new Dictionary<int, string>();
        private int _nextHandle = 1;

        public int OpenFile(string filename)
        {
            Console.WriteLine($"[FileSystem] Open: {filename}");
            int handle = _nextHandle++;
            _openFiles[handle] = filename;
            return handle;
        }

        public bool CloseFile(int handle)
        {
            Console.WriteLine($"[FileSystem] Close: handle {handle}");
            return _openFiles.Remove(handle);
        }

        public byte[] ReadFile(int handle, int byteCount)
        {
            if (!_openFiles.ContainsKey(handle))
                return null;

            Console.WriteLine($"[FileSystem] Read: handle {handle}, {byteCount} bytes");
            // Implement actual file read here
            return new byte[byteCount];
        }

        public bool WriteFile(int handle, byte[] data)
        {
            if (!_openFiles.ContainsKey(handle))
                return false;

            Console.WriteLine($"[FileSystem] Write: handle {handle}, {data.Length} bytes");
            // Implement actual file write here
            return true;
        }
    }

    #endregion
}
