"""
Write the per-machine SINTRAN boot files into the RetroCore disk images.

The machines MUST be stopped first - an emulator holds the image open and a
write underneath it corrupts the filesystem.

SINTRAN text files on these images carry EVEN PARITY in bit 7 of every byte and
end their lines CR LF. Both were measured off D100's own LOAD-MODE:BATC, which
SINTRAN reads at every boot, so this encoder reproduces exactly that shape.
"""
import subprocess
import sys
import os

NDTOOL = r"E:\Dev\Ronny\norskdata-ndfs\ndfs-c\build\ndtool.exe"

# machine -> (image path, the STARTEX source that belongs to it)
MACHINES = {
    "D100": (r"F:\RC\RonnyTest\HDLC1\BIGDISK0-K-100.IMG", "XMSG-STARTEX-L03.D100.txt"),
    "D102": (r"F:\RC\RonnyTest\HDLC2\BIGDISK0-K-102.IMG", "XMSG-STARTEX-L03.D102.txt"),
    "D103": (r"F:\RC\RonnyTest\HDLC3\BIGDISK0-K-103.IMG", "XMSG-STARTEX-L03.D103.txt"),
}

HERE = os.path.dirname(os.path.abspath(__file__))
STAGE = os.path.join(HERE, "staged")


def even_parity(seven_bit):
    """Return bit 7 set so the whole byte carries an even number of one bits."""
    p = 0
    for i in range(7):
        p ^= (seven_bit >> i) & 1
    return p << 7


def encode(text):
    """Plain text -> SINTRAN bytes: CRLF line ends, even parity in bit 7."""
    text = text.replace("\r\n", "\n").replace("\r", "\n")
    out = bytearray()
    lines = text.splitlines()
    # a source ending in a newline splits to a trailing "", which would add
    # one blank line that the machine's own files do not have
    if lines and lines[-1] == "":
        lines.pop()
    for line in lines:
        for ch in line:
            c = ord(ch)
            if c > 0x7F:
                raise ValueError("non-ASCII character %r in source" % ch)
            out.append(c | even_parity(c))
        out.append(0x0D | even_parity(0x0D))   # CR
        out.append(0x0A | even_parity(0x0A))   # LF
    return bytes(out)


def put(image, host_file, nd_path):
    # --overwrite is REQUIRED. Without it ndtool prints "skipped (exists)" and
    # still exits 0, so a run that wrote nothing at all looks like a success.
    cmd = [NDTOOL, "--put", host_file, nd_path, "--overwrite", image]
    r = subprocess.run(cmd, capture_output=True, text=True)
    print("    put", nd_path)
    out = r.stdout.strip()
    if out:
        print("     ", out)
    if r.returncode != 0:
        print("      FAILED:", r.stderr.strip())
        return False
    # belt and braces: a skip is a failure here, whatever the exit code says
    if "skipped" in out:
        print("      FAILED: target was skipped, nothing was written")
        return False
    return True


def main():
    os.makedirs(STAGE, exist_ok=True)
    batc = encode(open(os.path.join(HERE, "LOAD-MODE.BATC.txt"), encoding="utf-8").read())
    ok = True
    for name, (image, startex_src) in MACHINES.items():
        print(name, image)
        if not os.path.exists(image):
            print("    NO SUCH IMAGE - skipped")
            ok = False
            continue
        b = os.path.join(STAGE, name + "-LOAD-MODE.BATC")
        open(b, "wb").write(batc)
        s = os.path.join(STAGE, name + "-XMSG-STARTEX-L03.MODE")
        open(s, "wb").write(encode(open(os.path.join(HERE, startex_src), encoding="utf-8").read()))
        ok &= put(image, b, "SYSTEM/LOAD-MODE:BATC")
        ok &= put(image, s, "UTILITY/XMSG-STARTEX-L03:MODE")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
