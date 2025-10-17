Symbols mapping to functions in the ND-500 controller card


| IOX Offset |  Octal | Enum                | Symbol                     | Comment                         |
| :--------: | :----: | :------------------ | :------------------------- | :------------------------------ |
|     +0     | 000000 | ReadMarX2           | `RMAR5`                    | Read MAR                        |
|     +1     | 000001 | LoadMarX2           | `LMAR5`                    | Load MAR                        |
|     +2     | 000002 | ReadStatusRegister  | `RSTA5`                    | Read status                     |
|     +3     | 000003 | LoadStatusRegister  | `LSTA5`                    | Load status                     |
|     +4     | 000004 | ReadControlRegister | `RCON5`                    | Read control                    |
|     +5     | 000005 | LoadControlRegister | `LCON5`                    | Load control                    |
|     +6     | 000006 | MasterClear         | `MCLR5`                    | Master clear                    |
|     +7     | 000007 | Terminate           | `TERM5`                    | Terminate                       |
|     +10    | 000010 | ReadTagIn           | `RTAG5`                    | Read tag                        |
|     +11    | 000011 | WriteTagOut         | `LTAG5`                    | Write tag                       |
|     +12    | 000012 | ReadLowerLimit      | `RLOW5`                    | Read lower limit                |
|     +13    | 000013 | WriteDataX          | `LDAT5` or `LLOW5`         | Load data / write lower         |
|     +14    | 000014 | ReadLockedMaybe     | `SLOC5`                    | Possibly “slot” / “status lock” |
|     +15    | 000015 | WriteData           | `BITM5`, `CLKD5`, or `S15` | Unclear — may depend on context |
|     +16    | 000016 | ReadLocked          | `UNLC5`                    | Unlock — plausible              |
|     +17    | 000017 | LastIOX             | `RETG5`                    | Return/End gate                 |
