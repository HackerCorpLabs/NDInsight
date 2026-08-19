# RETRACTED: there was no TAD defect. The prompt needs ESC, and I never pressed it.

**This file previously argued, twice, that our TAD server had a protocol bug. It does not.** Two
real ND terminals now chat through it in both directions. The whole investigation was chasing an
operator error, and the retraction is worth more than the analysis was.

## What actually happens

`CONNECT-TO D19999` establishes the TAD connection and then **waits**. The bring-up is
client-driven: the server answers the connect letter, assigns the port, sends the priming DUMM, and
then sits still until the terminal sends **7ESCA** - which a person produces by **pressing ESC**.

Press it and the whole ladder runs:

```
=== CONNECTION ESTABLISHED ===
    TAD LOGICAL UNIT NO: 770
                                  <- ESC pressed here
 11.59.20     17 AUGUST   2026
 Emulated TAD server version v0.0.1
--- HOST ID:19999 TAD:1 ---

ENTER ronny
OK
(Allowed idle time: 30 minutes)
# chat join RONNY
you are in the room as RONNY
```

The rule was already written down in this project - *"on a fresh SINTRAN connection send ESC first
or it will not prompt"* - and it was applied faithfully to the D100 login prompt and then not
applied to the TAD login prompt, which needs it for exactly the same reason.

## The two wrong analyses, kept so the shape is recognisable

1. **"Our ladder stops three frames short."** Built on a frame table whose rows were assigned to the
   wrong side. The connect letter settles direction: frame 508 from TCP `10362` names `*TADADM` and
   `D102`, so 10362 is the CLIENT and 24182 the SERVER.
2. **"We volunteer a bare 7DUMM mid-handshake."** The real D102 sends that DUMM too - capture frame
   `0131`, body tail `0002 1800`, byte-identical in shape to ours. `TadServer.cs` already documented
   it as the deliberate priming DUMM, citing this same capture. The mistake: frame 520's TCP payload
   carries **three** concatenated HDLC frames and only the first was parsed.

Both were published as findings. Both were wrong. Neither was tested against the machine before
being written up - a live ESC would have refuted either in one minute.

## What to do instead

 - **Drive it from the terminal before theorising.** The cheapest experiment beat two frame-level
   analyses.
 - **A capture payload may hold several HDLC frames.** Split on the `7e` flags before reading, or
   the ladder will look shorter than it is.
 - **Attribute every row to a side first.** Find the frame whose body names the server.
 - **`[tad] session opened` is not a login.** It says a session exists, nothing about the far screen.

## Live result, both directions

```
RONNY (TAD:1)                       ANNA (TAD:2)
# chat join RONNY                   # chat join ANNA
you are in the room as RONNY        you are in the room as ANNA
ANNA joined
<ANNA> god morgen fra ANNA          <ANNA> god morgen fra ANNA
                                    <RONNY> takk RONNY svarer
                                    # chat who
                                    in the room: RONNY, ANNA
```

Two TAD units are served at once (`TAD:1` and `TAD:2`), so the old "one connection" worry does not
apply here. One real limit remains and is unexplained: the **first** `CONNECT-TO` on the second
terminal was refused with `UNSUCCESSFUL CONNECT: Remote system D19999 not available`, and an
immediate retry succeeded. Worth a look, but it is a retry-once nuisance rather than a blocker.
