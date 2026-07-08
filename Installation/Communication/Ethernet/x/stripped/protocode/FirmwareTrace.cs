//
// SPDX-License-Identifier: MIT
// ND Ethernet II Controller (PCB 3094) - 68000 firmware behavioral model.
// High-level protocol translation, NOT a cycle-accurate 68000 emulator.
//
// This file: simple tracing helpers used by every other model file so that a
// consumer can follow the reversed firmware logic step by step.
//

using System;

namespace RetroCore.ND.EthernetII.ProtoCode
{
    /// <summary>
    /// Lightweight tracing sink for the firmware behavioral model.
    /// Writes to an <see cref="Action{String}"/> so the host can redirect it
    /// (console, unit-test buffer, logger) without a dependency.
    /// </summary>
    public sealed class FirmwareTrace
    {
        private readonly Action<string> _sink;

        /// <summary>Create a trace that forwards every line to <paramref name="sink"/> (defaults to <see cref="Console.WriteLine(string)"/>).</summary>
        public FirmwareTrace(Action<string>? sink = null)
        {
            _sink = sink ?? Console.WriteLine;
        }

        /// <summary>Enable or disable all output.</summary>
        public bool Enabled { get; set; } = true;

        /// <summary>General informational message.</summary>
        public void Info(string message)
        {
            if (Enabled) _sink($"[INFO] {message}");
        }

        /// <summary>A read from an I/O register (0xEFxxxx space).</summary>
        public void IoRead(uint address, ushort value)
        {
            if (Enabled) _sink($"[IO  RD] 0x{address:X6} -> 0x{value:X4}");
        }

        /// <summary>A write to an I/O register (0xEFxxxx space).</summary>
        public void IoWrite(uint address, ushort value)
        {
            if (Enabled) _sink($"[IO  WR] 0x{address:X6} <- 0x{value:X4}");
        }

        /// <summary>A read from a named shared-memory mailbox/postbox field.</summary>
        public void MailboxRead(string field, ushort value)
        {
            if (Enabled) _sink($"[MBX RD] {field} -> 0x{value:X4}");
        }

        /// <summary>A write to a named shared-memory mailbox/postbox field.</summary>
        public void MailboxWrite(string field, ushort value)
        {
            if (Enabled) _sink($"[MBX WR] {field} <- 0x{value:X4}");
        }

        /// <summary>An interrupt hop from one component to another.</summary>
        public void Interrupt(string source, string target)
        {
            if (Enabled) _sink($"[IRQ] {source} -> {target}");
        }

        /// <summary>
        /// Behavior that is NOT fully proven from the disassembly. Every call site
        /// marks a place where the model is provisional and the real firmware may
        /// differ. Grep for "Unconfirmed" to audit the model's honesty.
        /// </summary>
        public void Unconfirmed(string message)
        {
            if (Enabled) _sink($"[UNCONFIRMED] {message}");
        }
    }
}
