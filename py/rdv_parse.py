"""Minimal DER parser for TSE RDV files: votos_cargo() returns (tag, cargo, escolhas, [(tipo, digitacao), ...]) per office."""
import sys, collections
def tlv(b, i):
    tag = b[i]; i += 1
    if tag & 0x1f == 0x1f:
        t = 0
        while True:
            x = b[i]; i += 1; t = (t << 7) | (x & 0x7f)
            if not x & 0x80: break
        tag = (tag, t)
    l = b[i]; i += 1
    if l & 0x80:
        n = l & 0x7f; l = int.from_bytes(b[i:i+n], "big"); i += n
    return tag, i, i + l
def parse(b, i=0, end=None):
    end = len(b) if end is None else end; out = []
    while i < end:
        tag, s, e = tlv(b, i)
        cons = (tag if isinstance(tag, int) else tag[0]) & 0x20
        out.append((tag, parse(b, s, e) if cons else b[s:e])); i = e
    return out
def votos_cargo(tree):
    # VotosCargo: [ctx1 cargo] INTEGER qtdEscolhas SEQUENCE OF Voto(ENUM tipo, NumericString digitacao?)
    res = []
    def walk(n):
        for tag, v in n:
            if isinstance(v, list):
                if len(v) >= 3 and v[0][0] in (0x81, 0x80) and v[1][0] == 0x02 and isinstance(v[2][1], list):
                    cargo = int.from_bytes(v[0][1], "big"); q = int.from_bytes(v[1][1], "big")
                    votes = []
                    for t2, vv in v[2][1]:
                        if isinstance(vv, list) and vv and vv[0][0] == 0x0a:
                            tipo = int.from_bytes(vv[0][1], "big")
                            dig = vv[1][1].decode() if len(vv) > 1 and vv[1][0] == 0x12 else ""
                            votes.append((tipo, dig))
                    res.append((v[0][0], cargo, q, votes))
                walk(v)
    walk(tree); return res
if __name__ == "__main__":
    b = open(sys.argv[1], "rb").read()
    for ctx, cargo, q, votes in votos_cargo(parse(b)):
        c = collections.Counter(votes)
        print(hex(ctx), "cargo", cargo, "escolhas", q, "n", len(votes), c.most_common(12))
