# Appendix C: Packet Trace Analysis

Analyzing_Traffic_100_to_102.md

# Analyzing Traffic 100 to 102

## Frame-by-Frame Analysis of Long Frames (>4 bytes)

### Frame 1: First X.25 Data Packet
**Timestamp:** [23:26:22.787]  
**Direction:** Machine 100 → Machine 102  
**Content:** `0x09 0x00 0x21 0x13 0x00 0x19 0x00 0x66 0x00 0x64 0xFF 0xFF 0x00 0x01 0xDE 0x08`  
**Size:** 16 bytes  
**LAPB:** N(S)=0, N(R)=0 (Information frame)  
**X.25:** Virtual Circuit 1.19, Packet Type 0x21  

**Retransmitted at:** [23:26:23.993] (same content)  
**Received by 102:** [23:26:23.377] and [23:26:24.268]

---

### Frame 2: Multi-Buffer X.25 Data Packet
**Timestamp:** [23:26:24.581] - [23:26:25.039] (3 parts)  
**Direction:** Machine 100 → Machine 102  

**Part 1 [RSOM:True] [REOM:False]:**
`0x09 0x22 0x21 0x13 0x00 0x0E 0x00 0x66 0x00 0x64 0x00 0x00 0x01 0x00 0xDD 0x14`

**Part 2 [RSOM:False] [REOM:False]:**
`0x21 0x00 0x86 0xC4 0x00 0x66 0x00 0x00 0x00 0x64 0x02 0xC2 0x01 0x00`

**Part 3 [RSOM:False] [REOM:True]:**
`0x01 0x4B 0x00 0x04 0x01 0x02 0x00 0x66`

**Combined Size:** 40 bytes  
**LAPB:** N(S)=1, N(R)=1  
**X.25:** Virtual Circuit 1.19, carrying application data  

**Received by 102:** [23:26:25.737] as single concatenated frame (correct assembly)


