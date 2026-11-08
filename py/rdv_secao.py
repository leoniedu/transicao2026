"""Seção x party table from RDV files (2026 round 1), for R/24.

Usage: python3 -I py/rdv_secao.py <uf> <valid Senate numbers, comma-separated> [rdvdir]
Vote types in the RDV (as decoded here): 1 party-only, 2 candidate, 3 blank, 4 null (digits typed), 7 second Senate vote
null because it repeats the first. One row per seção and party prefix: Senate nulls typed as <party><digit> with no such
candidate (`sen_pref`), federal deputy party-only votes for the party, the party's federal deputy votes; plus seção totals.
"""
import glob, csv, sys, os, collections
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from rdv_parse import parse, votos_cargo

uf, sen_valid = sys.argv[1].lower(), set(sys.argv[2].split(","))
rdvdir = sys.argv[3] if len(sys.argv) > 3 else "data/raw/2026/rdv"
rows, typed = [], collections.Counter()
for f in sorted(glob.glob(os.path.join(rdvdir, uf, "*.rdv"))):
    mu, zo, se = os.path.basename(f)[:-4].split("-")
    try:
        d = {c: v for _, c, q, v in votos_cargo(parse(open(f, "rb").read()))}
    except Exception:
        continue
    sen, depf, depe, gov, pres = (d.get(c, []) for c in (5, 6, 7, 3, 1))
    typed.update(x for t, x in sen if t == 4)
    tot = dict(mu=mu, zo=zo, se=se, sen_votos=len(sen), eleitores=len(pres),
               sen_zeros=sum(t == 4 and x != "" and set(x) == {"0"} for t, x in sen),
               sen_rep=sum(t == 7 for t, _ in sen), sen_nulo=sum(t == 4 for t, _ in sen),
               sen_branco=sum(t == 3 for t, _ in sen), sen_valido=sum(t == 2 for t, _ in sen),
               gov_nulo=sum(t == 4 for t, _ in gov), pres_nulo=sum(t == 4 for t, _ in pres),
               depf_leg=sum(t == 1 for t, _ in depf), depe_leg=sum(t == 1 for t, _ in depe),
               depe_leg_sen=sum(t == 1 and x in sen_valid for t, x in depe))
    pref = collections.Counter(x[:2] for t, x in sen if t == 4 and len(x) == 3 and set(x) != {"0"})
    legf = collections.Counter(x[:2] for t, x in depf if t == 1)
    allf = collections.Counter(x[:2] for t, x in depf if t in (1, 2))
    for p in set(pref) | set(legf) | set(allf) or {""}:
        rows.append(dict(tot, partido=p, sen_pref=pref[p], depf_leg_p=legf[p], depf_p=allf[p]))
w = csv.DictWriter(open(os.path.join(rdvdir, uf + "_secao_partido.csv"), "w"), fieldnames=list(rows[0].keys()))
w.writeheader(); w.writerows(rows)
with open(os.path.join(rdvdir, uf + "_senado_nulos_digitados.csv"), "w") as g:
    g.write("digitacao,votos\n"); g.writelines("%s,%d\n" % kv for kv in typed.most_common())
print(len({(r["mu"], r["zo"], r["se"]) for r in rows}), "seções")
