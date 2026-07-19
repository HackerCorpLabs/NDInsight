#!/usr/bin/env python3
"""Reconcile SINTRAN segment facts from three sources into one canonical table.

Precedence (per user: live commands + release manual are facts; JSON is AI-derived):
  * segment number + NAME      -> LIVE  @RT-LOADER *LIST-SEGMENT   (the running system)
  * MADR (file offset) + SEGLE -> LIVE  in-memory Segment Table (bank 3 / 0o124000)
  * load address / range / PIT / description -> release manual section 8.3
  * repo sintran-system-segments.json -> compared, NOT trusted; discrepancies reported

Emits segment-facts.json (canonical, used by carve.py) + a discrepancy report vs JSON.
Every record carries provenance and a confidence flag so low-trust rows are visible.
"""
import argparse, json, os, re, struct

SE = 8; F_SEGLE=3; F_MADR=4; F_FLAG=5

def parse_list_segment(path):
    out={}
    for ln in open(path):
        ln=ln.rstrip()
        if not ln or ln.lstrip().startswith('#'): continue
        p=ln.split()
        if len(p)>=2 and p[-1].isdigit():
            try: out[int(p[-1],8)]=p[0]
            except ValueError: pass
    return out

def parse_manual83(md_path):
    pat=re.compile(r'^\|\s*(\d+)\s*\|\s*([A-Z0-9]+)\s*\|\s*([0-7]+:[0-7]+)\s*\|\s*(\d*)\s*\|\s*(.+?)\s*\|')
    rows={}
    for ln in open(md_path,encoding='utf-8'):
        m=pat.match(ln.strip())
        if m:
            sn=int(m.group(1),8)
            rows[sn]={'name':m.group(2),'range':m.group(3),
                      'pit':(int(m.group(4)) if m.group(4) else None),'desc':m.group(5)}
    # also index by name for shift-tolerant lookup
    by_name={v['name']:v for v in rows.values()}
    return rows,by_name

def read_sgt(path):
    raw=open(path,'rb').read()
    def w(i): return struct.unpack_from('>H',raw,i*2)[0]
    t={}
    for sn in range(len(raw)//(SE*2)):
        e=[w(sn*SE+k) for k in range(SE)]
        if any(e): t[sn]={'segle':e[F_SEGLE],'madr':e[F_MADR],'flag':e[F_FLAG]}
    return t

def range_base(r):
    if r and ':' in r:
        try: return int(r.split(':')[0],8)
        except ValueError: return None
    return None

def range_ok(r):
    if not r or ':' not in r: return False
    a,b=r.split(':')
    try: return int(a,8)<=int(b,8)
    except ValueError: return False

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--list-segment',required=True)
    ap.add_argument('--manual',required=True,help='L release manual .md')
    ap.add_argument('--sgt',required=True)
    ap.add_argument('--json',required=True,help='repo sintran-system-segments.json')
    ap.add_argument('--out',required=True,help='output dir')
    a=ap.parse_args()

    live=parse_list_segment(a.list_segment)
    man,man_by_name=parse_manual83(a.manual)
    sgt=read_sgt(a.sgt)
    jj={s['segment_dec']:s for s in json.load(open(a.json))['system_segments']}
    os.makedirs(a.out,exist_ok=True)

    nums=sorted(set(live)|set(man)|set(sgt))
    facts=[]; report=[]
    for sn in nums:
        name=live.get(sn) or (man.get(sn) or {}).get('name') or f'SEG{sn:03o}'
        # manual row: prefer the row whose NAME matches live (shift-tolerant),
        # else the row at this number.
        mrow = man_by_name.get(name) or man.get(sn) or {}
        rng=mrow.get('range')
        load=range_base(rng)
        conf='high'; notes=[]
        if sn in man and man[sn].get('name')!=name:
            notes.append(f"manual#{sn:o} name={man[sn]['name']} (used live name)")
            conf='medium'
        if rng and not range_ok(rng):
            notes.append(f"manual range {rng} is backwards (OCR); load may be wrong")
            conf='low'
        if sn not in live:
            notes.append("not in live LIST-SEGMENT"); conf='low' if conf=='high' else conf
        s=sgt.get(sn,{})
        facts.append({'segnum':sn,'segnum_oct':f'{sn:04o}','name':name,
            'load_address':load,'load_address_oct':(f'{load:o}' if load is not None else None),
            'address_range':rng,'pit':mrow.get('pit'),'description':mrow.get('desc'),
            'madr':s.get('madr'),'segle':s.get('segle'),
            'flag_oct':(f"{s['flag']:06o}" if 'flag' in s else None),
            'confidence':conf,'notes':notes})
        # discrepancy vs JSON
        j=jj.get(sn)
        if j:
            for jf,mf,lbl in [('name',name,'name'),('address_range',rng,'range'),('pit',mrow.get('pit'),'pit')]:
                if j.get(jf)!=mf and mf is not None:
                    report.append(f"seg {sn:o} {name}: JSON {lbl}={j.get(jf)!r} -> fact {mf!r}")
        else:
            report.append(f"seg {sn:o} {name}: missing from JSON")

    json.dump({'source':'live LIST-SEGMENT + memory SGT + manual 8.3','segments':facts},
              open(os.path.join(a.out,'segment-facts.json'),'w'),indent=1)
    open(os.path.join(a.out,'json-discrepancies.txt'),'w').write('\n'.join(report)+'\n')
    lo=[f for f in facts if f['confidence']!='high']
    print(f"{len(facts)} segments -> segment-facts.json  ({len(lo)} low/medium confidence)")
    print(f"{len(report)} discrepancies vs repo JSON -> json-discrepancies.txt")
    for f in lo:
        print(f"  seg {f['segnum']:>3o} {f['name']:<9} conf={f['confidence']:<6} {' | '.join(f['notes'])}")

if __name__=='__main__': main()
