"""Download RDV (registro digital do voto) files for a random sample of seções of one UF, 2026 round 1.

Usage: python3 -I py/rdv_fetch.py <uf> <n> [outdir]   (default outdir: data/raw/2026/rdv)
Writes <outdir>/<uf>-cs.json (the seção list) and <outdir>/<uf>/<municipio>-<zona>-<secao>.rdv. Already
downloaded seções are skipped, so re-running retries the failures.
"""
import json, random, sys, os, urllib.request, collections, concurrent.futures as cf

PLEITO = "3220"   # 2026 round 1 (resultados.tse.jus.br/oficial/comum/config/ele-c.json)
BASE = "https://resultados.tse.jus.br/oficial/ele2026/arquivo-urna/" + PLEITO

uf, n = sys.argv[1].lower(), int(sys.argv[2])
out = sys.argv[3] if len(sys.argv) > 3 else "data/raw/2026/rdv"
os.makedirs(os.path.join(out, uf), exist_ok=True)
cs_path = os.path.join(out, "%s-cs.json" % uf)
if not os.path.exists(cs_path):
    urllib.request.urlretrieve("%s/config/%s/%s-p00%s-cs.json" % (BASE, uf, uf, PLEITO), cs_path)
cs = json.load(open(cs_path))
secs = [(m["cd"], z["cd"], s["ns"]) for m in cs["abr"][0]["mu"] for z in m["zon"] for s in z["sec"]]
random.seed(20261004)
sample = random.sample(secs, min(n, len(secs)))

def get(t):
    mu, zo, se = t
    dest = os.path.join(out, uf, "%s-%s-%s.rdv" % (mu, zo, se))
    if os.path.exists(dest):
        return "skip"
    base = "%s/dados/%s/%s/%s/%s" % (BASE, uf, mu, zo, se)
    try:
        aux = json.load(urllib.request.urlopen("%s/p00%s-%s-m%s-z%s-s%s-aux.json" % (base, PLEITO, uf, mu, zo, se), timeout=60))
        for h in aux["hashes"]:
            for a in h["arq"]:
                if a["tp"] == "rdv":
                    data = urllib.request.urlopen("%s/%s/%s" % (base, h["hash"], a["nm"]), timeout=60).read()
                    open(dest, "wb").write(data)
                    return "ok"
        return "no_rdv"
    except Exception:
        return "err"

with cf.ThreadPoolExecutor(4) as ex:
    print(len(secs), "seções;", collections.Counter(ex.map(get, sample)))
