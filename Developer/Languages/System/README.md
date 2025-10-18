# System Programming Languages

System-level programming languages for NORD/SINTRAN operating system development, device drivers, and low-level system utilities.

---

## 🔧 System Languages

### [NPL-DEVELOPER-GUIDE.md](NPL-DEVELOPER-GUIDE.md)
**NORD Programming Language (NPL)**

The primary language for SINTRAN III operating system development.

**Key Features:**
- Direct hardware access
- Interrupt handling
- Memory management
- Register manipulation
- Inline MAC assembly
- Compiles to MAC code

**Use Cases:**
- Operating system components
- Device drivers
- Kernel modules
- RT programs
- Monitor-level code

**Reference Manual:** `../../../Reference-Manuals/ND-60.047.03 NORD PL User's Guide.md`

---

### [MAC-DEVELOPER-GUIDE.md](MAC-DEVELOPER-GUIDE.md)
**Macro Assembler (MAC)**

Low-level assembly language for NORD-100 computers.

**Key Features:**
- Direct CPU instruction access
- Macro definitions
- Conditional assembly
- Symbol management
- Produces BRF object files

**Use Cases:**
- Performance-critical code
- Hardware initialization
- Boot loaders
- Interrupt handlers
- NPL compiler output

**Reference Manuals:**
- `../../../Reference-Manuals/ND-60.096.01 MAC Interactive Assembly and Debugging System User's Guide.md`
- `../../../Reference-Manuals/ND-60.009.02 MACM Mac Mass Storage Assembler.md`

---

### [NORD-500-ASSEMBLER-DEVELOPER-GUIDE.md](NORD-500-ASSEMBLER-DEVELOPER-GUIDE.md)
**NORD-500 Assembler**

Assembly language for the 32-bit ND-500 coprocessor.

**Key Features:**
- 32-bit operations
- Domain-based programming
- Structured assembly (NRF format)
- Advanced addressing modes
- Floating-point operations

**Use Cases:**
- ND-500 coprocessor programs
- Signal processing
- Scientific computing
- Specialized algorithms
- High-performance tasks

**Reference Manuals:**
- `../../../Reference-Manuals/ND-60.113.02 EN Assembler Reference Manual.md`
- `../../../Reference-Manuals/ND-05.009.4 EN ND-500 Reference Manual.md`

---

### [NORD-500-ASSEMBLER-EXPERT-GUIDE.md](NORD-500-ASSEMBLER-EXPERT-GUIDE.md)
**NORD-500 Assembler Expert Guide**

Advanced topics and expert-level ND-500 programming.

**Topics:**
- Domain management
- Memory mapping
- Advanced instruction usage
- Optimization techniques
- System integration

---

## 🎯 Which System Language to Use?

### Use NPL When:
- ✅ Writing SINTRAN OS components
- ✅ Developing device drivers
- ✅ Need structured system programming
- ✅ Want readable, maintainable system code
- ✅ Need both high-level and low-level control

### Use MAC When:
- ✅ Need maximum performance
- ✅ Require direct CPU instruction control
- ✅ Writing boot loaders or very low-level code
- ✅ Implementing NPL-called routines
- ✅ Optimizing critical code paths

### Use NORD-500 Assembler When:
- ✅ Programming the ND-500 coprocessor
- ✅ Need 32-bit arithmetic operations
- ✅ Implementing specialized algorithms
- ✅ Require floating-point performance
- ✅ Working with ND-500 domains

---

## 📚 Learning Path

### Beginner → Intermediate
1. **Start with NPL** - Learn structured system programming
2. **Study MAC basics** - Understand NPL's output
3. **Read SINTRAN source** - See real-world NPL/MAC usage

### Intermediate → Advanced
1. **Deep dive into MAC** - Master assembly language
2. **Study interrupt handling** - Learn system-level concepts
3. **Explore device drivers** - Apply knowledge to real drivers

### Advanced → Expert
1. **ND-500 programming** - Master coprocessor development
2. **OS kernel modification** - Contribute to SINTRAN
3. **Hardware integration** - Design new device drivers

---

## 🔗 Related Documentation

### Development Workflow
- **Compilation**: [../../Workflow/COMPILER-COMMANDS-REFERENCE.md](../../Workflow/COMPILER-COMMANDS-REFERENCE.md)
- **Linking**: [../../Workflow/LINKING-GUIDE.md](../../Workflow/LINKING-GUIDE.md)
- **Tools**: [../../Workflow/TOOLS-REFERENCE.md](../../Workflow/TOOLS-REFERENCE.md)

### System Documentation
- **SINTRAN OS**: [../../../SINTRAN/OS/](../../../SINTRAN/OS/)
- **Device Drivers**: [../../../SINTRAN/Devices/](../../../SINTRAN/Devices/)
- **ND-500**: [../../../SINTRAN/ND500/](../../../SINTRAN/ND500/)

### Reference Manuals
- **All Manuals**: [../../../Reference-Manuals/](../../../Reference-Manuals/)

---

## 🛠️ Development Tools

### Compilers
- **@NPL** - NPL compiler
- **@MAC** - MAC assembler
- **@N500** - NORD-500 assembler

### Debuggers
- **@MAC-DEBUG** - Interactive MAC debugger
- **@SIBAS** - System debugger

### Utilities
- **@LOAD** - Relocating loader
- **@DUMP** - Object file dumper
- **@XREF** - Cross-reference generator

See [../../Workflow/TOOLS-REFERENCE.md](../../Workflow/TOOLS-REFERENCE.md) for complete tool documentation.

---

## 📖 Code Examples

All language guides include practical examples:
- Hello World programs
- System call usage
- Interrupt handlers
- Device driver templates
- Memory management
- ND-500 communication

---

**Last Updated**: October 18, 2025  
**Languages**: 3 (NPL, MAC, NORD-500 Assembler)  
**Documentation Status**: ✅ Complete guides available

