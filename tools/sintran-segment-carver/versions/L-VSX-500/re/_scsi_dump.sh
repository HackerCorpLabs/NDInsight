#!/bin/bash
# Dump full disassembly ranges from segment 065-S3SIPIT (base 32000B) for the SCSI carve.
DIR=/mnt/e/Dev/Ronny/NDInsight/tools/sintran-segment-carver/versions/L-VSX-500/segments
OUT=/mnt/e/Dev/Ronny/NDInsight/tools/sintran-segment-carver/versions/L-VSX-500/re/kernel-carving/SCSI-DRIVER
mkdir -p "$OUT"
python3 -c "d=bytearray(open('$DIR/065-S3SIPIT.bin','rb').read());d[0::2],d[1::2]=d[1::2],d[0::2];open('/tmp/x.le','wb').write(d)"
nd100-dis -a -o -b $((8#32000)) /tmp/x.le 2>/dev/null > /tmp/full.dis
# driver core window 067160..072011
awk -v lo=$((8#067160)) -v hi=$((8#072012)) 'NR>2{a=strtonum("0"$1); if(a>=lo&&a<=hi) print}' /tmp/full.dis > "$OUT/_driver.dis"
# disk-layer dispatch 056120..057406
awk -v lo=$((8#056120)) -v hi=$((8#057406)) 'NR>2{a=strtonum("0"$1); if(a>=lo&&a<=hi) print}' /tmp/full.dis > "$OUT/_disklayer.dis"
# INQUI / RCAFI 062214..062600
awk -v lo=$((8#062214)) -v hi=$((8#062600)) 'NR>2{a=strtonum("0"$1); if(a>=lo&&a<=hi) print}' /tmp/full.dis > "$OUT/_inqui.dis"
wc -l "$OUT/_driver.dis" "$OUT/_disklayer.dis" "$OUT/_inqui.dis"
