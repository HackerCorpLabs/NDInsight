from brf_link import link
import sys
fn="E:/Dev/Ronny/NDInsight/Installation/Communication/Ethernet/x/encos-err-i-b01.brf"
data=open(fn,'rb').read()
img,symbols,refs,main_sym,units=link(data)
targets={v:k for k,v in symbols.items() if k in
  ('START_P','SEGLOAD','UNLOAD','STOP_PI','RES_SLO','REL_SLO','SEND_KI','REC_KIC','READPIO','INT2GET','ENNS0')}
# scan whole image for JPL/JMP (direct P-relative or indirect) that lands on a target.
# Direct P-rel JPL: 134000|disp ; indirect JPL via pointer table is common. Also scan for data words == target addr (pointer table).
for a in range(len(img.mem)):
    if not img.loaded[a]: continue
    w=img.mem[a]
    # pointer word equal to a target address
    if w in targets:
        print("PTR  %06o -> %06o %s" % (a, w, targets[w]))
