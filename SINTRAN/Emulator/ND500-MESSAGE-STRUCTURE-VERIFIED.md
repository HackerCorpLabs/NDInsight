# ND-500 Message Buffer Structure - VERIFIED FROM SOURCE

**What We Actually Know vs. What We're Guessing**

---

## What We KNOW FOR CERTAIN from NPL Source Code

### Fields Explicitly Referenced in Code

From `MP-P2-N500.NPL`, `RP-P2-N500.NPL`, and `CC-P2-N500.NPL`:

| Field Name | Evidence | Location | Verified? |
|------------|----------|----------|-----------|
| **PLINK** | `*AAX PLINK; LDATX` | MP-P2-N500.NPL:1669-1670 | ✅ YES - Process link exists |
| **5MSFL** | `*AAX 5MSFL; LDATX` / `A BONE 5ITMQUEUE; *STATX` | MP-P2-N500.NPL:1674-1675 | ✅ YES - Message flags field |
| **5PRIO** | `*AAX 5PRIO; LDATX` | RP-P2-N500.NPL:796,804,812 | ✅ YES - Priority field |
| **TODF** | `*AAX TODF; STATX` | MP-P2-N500.NPL:1692 | ✅ YES - To datafield |
| **DNOBY** | `*AAX DNOBY-TODF; LDDTX` | MP-P2-N500.NPL:1693 | ✅ YES - Number of bytes (double word) |
| **NRBYT** | `*AAX NRBYT; STATX` | MP-P2-N500.NPL:1703 | ✅ YES - Number of bytes (alternate name?) |
| **OSTRA / N500A** | `*AAX OSTRA-5DITN; LDDTX; AAX N500A-OSTRA; STDTX` | MP-P2-N500.NPL:1705 | ✅ YES - ND-500 address |
| **ABUFA / N100A** | `*AAX ABUFA-N500A; LDDTX; AAX N100A-ABUFA; STDTX` | MP-P2-N500.NPL:1706 | ✅ YES - ND-100 address |
| **XMICF / MICFUNC** | `*STATX XMICF` / `*MICFU@3 STATX` | MP-P2-N500.NPL:1702, RP-P2-N500.NPL:803,811 | ✅ YES - Microcode function |
| **5DITN** | `*AAX 5DITN-NRBYT; STZTX` | MP-P2-N500.NPL:1704 | ✅ YES - DIT number |
| **5CPUN** | `*AAX 5CPUN; STATX` | RP-P2-N500.NPL:815,823 | ✅ YES - CPU number |
| **5FYLLE** | `X:=IBMWNDMESS.5FYLLE` | RP-P2-N500.NPL:499,500 | ✅ YES - Fill/byte counter |
| **SMCNO** | `*AAX SMCNO; LDATX` / `IF A=511 OR A=503` | MP-P2-N500.NPL:1821, RP-P2-N500.NPL:463,506,540 | ✅ YES - Some monitor call number |
| **DMAXB** | `*AAX DMAXB; LDDTX` | MP-P2-N500.NPL:1821 | ✅ YES - Max bytes |
| **SPFLA** | `*AAX SPFLA-N100A; STATX` | MP-P2-N500.NPL:1707 | ✅ YES - Some flag |
| **LINK / LINK1 / LINK2** | `*LINK@3 LDDTX` / `*LINK1@3 STATX` | RP-P2-N500.NPL:794,3073 | ✅ YES - Link fields (multiple) |
| **SENDE** | `*SENDE@3 LDATX` / `*SENDE@3 STATX` | Multiple locations | ✅ YES - Send enabled (in process descriptor) |
| **XADPR** | `*AAX XADPR; STATX` | RP-P2-N500.NPL:785 | ✅ YES - Process descriptor address |

### What We DON'T Know for Certain

❌ **Exact offsets** - The `AAX` syntax means "add to X register", but we don't know the base offset
❌ **Field order** - We can infer some ordering from `AAX field1; ... AAX field2-field1` but not complete
❌ **Field sizes** - Some are clearly double words (DNOBY, N500A, N100A) but not all specified
❌ **Total structure size** - We know `55MESSIZE` but not from symbols file
❌ **Function code values** - We never see explicit function code assignments in the code shown

---

## What We Can INFER (But Not Prove)

### Likely Correct Inferences

1. **5MSFL bit 5ITMQUEUE** - Code does `A BONE 5ITMQUEUE; *STATX` suggesting it's a flag bit
   - **Inference**: Bit flag in message flags word
   - **Confidence**: HIGH (code pattern clear)

2. **SMCNO values 511, 503** - Code checks `IF A=511 OR A=503`
   - **Inference**: 511₈ (329 decimal) and 503₈ (323 decimal) are DVIO/DVINST codes
   - **Confidence**: MEDIUM (could be other meanings)

3. **TODF, DNOBY, N500A, N100A sequence** - Code shows relative addressing
   - **Inference**: These fields are sequential or near each other
   - **Confidence**: MEDIUM

4. **Error codes EC174, EC175, etc.** - Used in `CALL EMONICO`
   - **Inference**: Error code field exists and uses EC*** symbols
   - **Confidence**: HIGH

### What I MADE UP (Apologies!)

❌ **Function codes 1, 2, 3, 10, 20, 30** - I inferred 1=DVIO, 2=DVINST from routine names, but:
   - We see SMCNO=511, 503 which could be function codes
   - **I MADE UP** the values 10, 20, 30 for GRAPHICS, DATABASE, COMPUTE - **NO SOURCE FOR THIS**

❌ **Specific offsets (0, 1, 2, 3...)** - I assumed sequential layout but:
   - **NO PROOF** of exact offsets
   - The `AAX` addressing doesn't give us absolute positions

❌ **"Priority" at offset 2** - I guessed based on code order
   - **UNCERTAIN** where 5PRIO actually sits

❌ **Complete structure** - The full struct I showed was:
   - **PARTIALLY INVENTED** - mixture of known fields and guessed layout

---

## What We Should Document

### HONEST C# Structure

```csharp
/// <summary>
/// ND-500 Message Buffer in 5MPM.
/// SIZE: 55MESSIZE words (exact value unknown without symbol file)
/// 
/// WARNING: Field offsets are PARTIALLY KNOWN from code analysis.
/// This structure is based on NPL code references but exact layout is inferred.
/// </summary>
public class ND500Message
{
    // ===== KNOWN FIELDS (verified from NPL source) =====
    
    /// <summary>ProcessLink - PLINK field (16 bits)
    /// VERIFIED: MP-P2-N500.NPL line 1669-1670
    /// </summary>
    public ushort ProcessLink { get; set; }
    
    /// <summary>MessageFlags - 5MSFL field (16 bits)
    /// VERIFIED: MP-P2-N500.NPL line 1674-1675
    /// Contains 5ITMQUEUE flag (exact bit position unknown)
    /// </summary>
    public ushort MessageFlags { get; set; }
    
    /// <summary>Priority - 5PRIO field (16 bits)
    /// VERIFIED: RP-P2-N500.NPL lines 796, 804, 812
    /// Offset relative to other fields UNKNOWN
    /// </summary>
    public ushort Priority { get; set; }
    
    /// <summary>ToDatafield - TODF field (size unknown, possibly 32 bits)
    /// VERIFIED: MP-P2-N500.NPL line 1692
    /// </summary>
    public uint ToDatafield { get; set; }  // Size assumed
    
    /// <summary>ByteCount - DNOBY/NRBYT field (32 bits based on LDDTX)
    /// VERIFIED: MP-P2-N500.NPL lines 1693, 1703
    /// </summary>
    public uint ByteCount { get; set; }
    
    /// <summary>ND500Address - OSTRA/N500A field (32 bits based on STDTX)
    /// VERIFIED: MP-P2-N500.NPL line 1705
    /// </summary>
    public uint ND500LogicalAddr { get; set; }
    
    /// <summary>ND100Address - ABUFA/N100A field (32 bits based on STDTX)
    /// VERIFIED: MP-P2-N500.NPL line 1706
    /// After CNVWADR translation
    /// </summary>
    public uint ND100PhysicalAddr { get; set; }
    
    /// <summary>MicrocodeFunction - XMICF/MICFUNC field (16 bits)
    /// VERIFIED: MP-P2-N500.NPL line 1702, RP-P2-N500.NPL lines 803, 811
    /// Known values: 3RMED, 3WMED, 3START, 3RPREG, 3RMICV (from comments)
    /// </summary>
    public ushort MicrocodeFunction { get; set; }
    
    /// <summary>DITNumber - 5DITN field (16 bits)
    /// VERIFIED: MP-P2-N500.NPL line 1704
    /// </summary>
    public ushort DITNumber { get; set; }
    
    /// <summary>CPUNumber - 5CPUN field (16 bits)
    /// VERIFIED: RP-P2-N500.NPL lines 815, 823
    /// </summary>
    public ushort CPUNumber { get; set; }
    
    /// <summary>MonitorCallNumber - SMCNO field (16 bits)
    /// VERIFIED: MP-P2-N500.NPL line 1821, RP-P2-N500.NPL line 463
    /// Known values: 511₈ (329), 503₈ (323) - purpose unclear
    /// </summary>
    public ushort MonitorCallNumber { get; set; }
    
    /// <summary>ByteFill - 5FYLLE field (16 bits)
    /// VERIFIED: RP-P2-N500.NPL lines 499, 500
    /// Used as byte counter during character transfer
    /// </summary>
    public ushort ByteFill { get; set; }
    
    // ===== UNKNOWN/INFERRED FIELDS =====
    
    /// <summary>Error code field - referenced as EC174, EC175, etc.
    /// INFERRED: From EMONICO calls with error codes
    /// Exact offset UNKNOWN
    /// </summary>
    public ushort ErrorCode { get; set; }  // ⚠️ LOCATION UNKNOWN
    
    /// <summary>Additional data area
    /// Size varies, total message is 55MESSIZE words
    /// </summary>
    public ushort[] Data { get; set; }
    
    // ===== HELPER PROPERTIES (based on inferences) =====
    
    /// <summary>Is message in queue?
    /// INFERRED: From "A BONE 5ITMQUEUE" code pattern
    /// Exact bit position UNKNOWN
    /// </summary>
    public bool IsInQueue => (MessageFlags & 0x0001) != 0;  // ⚠️ BIT POSITION GUESSED
    
    /// <summary>Function name
    /// WARNING: Function code field not found in source!
    /// SMCNO values 511₈, 503₈ seen but purpose unclear
    /// </summary>
    public string FunctionName => MonitorCallNumber switch
    {
        329 => "SMCNO_511_Unknown",  // 511₈ from source
        323 => "SMCNO_503_Unknown",  // 503₈ from source
        _ => $"SMCNO_{MonitorCallNumber}"
    };
}
```

---

## What We NEED from Symbol Files

To complete the structure, we need:
1. ✅ `55MESSIZE` - Total message buffer size
2. ✅ Symbol values for all field offsets (PLINK, 5MSFL, TODF, etc.)
3. ✅ `5ITMQUEUE` bit position
4. ✅ `5SYSRES` bit position
5. ✅ `5PRDSIZE` - Process descriptor size
6. ✅ `S500S`, `S500E` - Process table bounds
7. ✅ Microcode function values (3RMED, 3WMED, etc.)
8. ✅ Error code values (EC174, EC175, etc.)

---

## Honest Assessment

**What I did wrong:**
1. ❌ Made up function codes (10=GRAPHICS, etc.) - **NO SOURCE**
2. ❌ Assumed sequential offsets (0, 1, 2, 3...) - **UNVERIFIED**
3. ❌ Claimed certainty about field sizes - **PARTIALLY GUESSED**
4. ❌ Showed complete struct as if proven - **MISLEADING**

**What we actually know:**
1. ✅ Field NAMES from NPL code
2. ✅ Some field relationships (relative addressing)
3. ✅ Some field sizes (double words use LDDTX/STDTX)
4. ✅ General purpose of fields
5. ✅ Some specific values (SMCNO=511₈, 503₈)

**Bottom line:** Without the symbol files or more detailed memory dumps, we can't provide exact offsets or guarantee the structure layout.

---

**Thank you for calling this out!** This is the honest assessment of what we know versus what I was inferring/guessing.


