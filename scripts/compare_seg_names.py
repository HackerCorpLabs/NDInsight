"""Compare release manual segment names with NPL symbol table segment constants."""

manual = {
    0o001: 'seg1(none)',
    0o002: 'S3IMAGE', 0o003: 'S3CP', 0o004: 'S3RTL',
    0o005: 'S3ERRS', 0o006: 'S3FS', 0o007: 'S3DMAC',
    0o010: 'S3RTFIL', 0o011: 'S3ERRL', 0o012: 'S3SFS',
    0o013: 'S3SCP', 0o014: 'S3ERRP', 0o015: 'S3BFLY',
    0o016: 'S3SRPIT', 0o017: 'S3SMPIT',
    0o020: 'S3SDT5', 0o021: 'S3NM5', 0o022: 'S3RFAC',
    0o023: 'S3DPIT', 0o024: 'S3SGST', 0o025: 'S3IRPIT',
    0o026: 'S3IMPIT', 0o027: 'S3ISGT',
    0o030: 'S3SM5', 0o031: 'S3SSPD',
    0o035: 'S3MPIT', 0o036: 'S3TAD', 0o037: 'S3RTD',
    0o040: 'S3FUDRT', 0o041: 'S3IMED', 0o042: 'S3ED',
    0o043: 'S3PATCH', 0o044: 'S3IDPIT', 0o045: 'S3ISYS',
    0o046: 'S3S5PIT', 0o047: 'S3RPIT',
    0o050: 'S3IS5PIT', 0o051: 'S35PIT',
    0o052: 'S3SAVE', 0o053: 'S3SDPIT', 0o054: 'S3SSYS',
    0o055: 'S3SERRP', 0o056: 'S3SRTC', 0o057: 'S3SRTD',
    0o060: 'S3SECOM', 0o061: 'S3IECOM',
    0o062: 'S3SSM5', 0o063: 'S3MEMTF', 0o064: 'S3ECOM',
    0o065: 'S3SIPIT', 0o066: 'S3IIPIT', 0o067: 'S3IPIT',
    0o070: 'S3SSM', 0o071: 'S3SM',
    0o072: 'S3SDMWD', 0o073: 'S3IDMWD',
    0o074: 'S3SXMK', 0o075: 'S3SXROU',
    0o076: 'S3XMK', 0o077: 'S3XROU',
    0o100: 'S3SDNAM', 0o101: 'SDNAM',
    0o102: 'S3SXMFI', 0o103: 'S3XMFI',
    0o104: 'S3SNKSE', 0o105: 'S3INKSE',
    0o106: 'S3SNKNA', 0o107: 'S3INKNA',
    0o110: 'S3SU110', 0o111: 'S3IU110',
    0o112: 'S3SU120', 0o113: 'S3IU120',
    0o114: 'S3SERWC', 0o115: 'S3IERWC',
    0o116: 'S3SERWD', 0o117: 'S3IERWD',
    0o120: 'S3SPPRMA', 0o121: 'S3IPRMA',
    0o122: 'S3SPWRS', 0o123: 'S3IPWRS',
    0o124: 'S3SBOPC', 0o125: 'S3IBOPC',
    0o126: 'S3SMTSE', 0o127: 'S3IMTSE',
    0o130: 'S3SHDM', 0o131: 'S3IHDM',
    0o132: 'S3SFAC', 0o133: 'S3IFAC',
    0o134: 'S3SNKDAT', 0o135: 'S3INKDAT',
}

# NPL 5xxxx symbols from L07 SYMBOL-1-LIST with segment-number values
npl = {
    0o001: '5BCOM', 0o005: '5ERRS', 0o006: '5FILS',
    0o010: '5RTFI', 0o016: '5SRPI', 0o017: '5SMPI',
    0o022: '5FIUS', 0o023: '5DPIT', 0o024: '5SSGT',
    0o025: '5IRPI', 0o026: '5IMPI', 0o027: '5ISGT',
    0o031: '5SSPD', 0o035: '5MPIT', 0o036: '5BADM',
    0o037: '5RT2S', 0o040: '5RRUS', 0o043: '5SPDF',
    0o044: '5IDPI', 0o045: '5ISYS', 0o046: '5S5PI',
    0o047: '5RPIT', 0o050: '5I5PI', 0o052: '5SAVE',
    0o053: '5SDPI', 0o054: '5SSYS', 0o056: '5SRTC',
    0o057: '5SRTD', 0o060: '5SECO', 0o061: '5IECO',
    0o062: '5SSM5', 0o063: '5MEMT', 0o064: '5ECOM',
    0o065: '5SIPI', 0o066: '5IIPI', 0o067: '5IPIT',
    0o070: '5SSMS', 0o071: '5SMSE', 0o072: '5SDIM',
    0o073: '5DIMS', 0o074: '5SXMK', 0o075: '5SXRO',
    0o076: '5XMKS', 0o077: '5XROS',
    0o100: '5SDNA', 0o101: '5DNAM', 0o102: '5SXMF',
    0o103: '5XMFI', 0o105: '5NKSE', 0o107: '5NKNA',
    0o115: '5WDCS', 0o117: '5WDDS',
    0o121: '5PROM', 0o123: '5EVME', 0o125: '5BOPC',
    0o127: '5MTSE',
}

print(f"{'Seg#':>5s} {'Manual Name':<12s} {'NPL Symbol':<8s} {'Match?':<12s} Notes")
print(f"{'-----':>5s} {'----------':<12s} {'--------':<8s} {'------':<12s} -----")

for seg in sorted(set(list(manual.keys()) + list(npl.keys()))):
    m = manual.get(seg, '-')
    n = npl.get(seg, '-')

    # Strip prefix to compare cores: S3xxx -> xxx, 5xxx -> xxx
    if m.startswith('S3'):
        m_core = m[2:]
    else:
        m_core = m

    if n.startswith('5'):
        n_core = n[1:]
    else:
        n_core = n

    notes = ""
    if m == '-':
        match = 'NPL only'
    elif n == '-':
        match = 'MANUAL only'
    elif m_core == n_core:
        match = 'EXACT'
    elif m_core[:4] == n_core[:4]:
        match = 'TRUNC OK'
        notes = f"manual={m_core} npl={n_core}"
    else:
        match = '** DIFFER **'
        notes = f"manual={m_core} vs npl={n_core}"

    print(f"{seg:05o} {m:<12s} {n:<8s} {match:<12s} {notes}")
