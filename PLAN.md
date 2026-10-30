# Plan: 1st → 2nd round vote transitions, Brazil

## Done (2026-10-30)
- Pipeline for 2022 and 2026 (`ANO=` env var): download and reconcile (01), seção cleaning and exclusions (02),
  perfil covariates for 2026 only (02b), diagnostics (03), per-zona LP estimates (05), simulation recovery (06),
  in/out and swing decomposition (07), 2022 vs 2026 comparison (08).
- Decisions:
  - Rows are Lula, Bolsonaro, outros (all also-rans merged), invalid, abstention.
  - Aptos may differ between rounds by up to 1%; the gap is padded as abstention.
  - Per-zona `lphom`/`tslphom`/`nslphom` with seções as units, summed to UF and national.
  - Duncan–Davis bounds at seção level.
  - eiPack dropped as too heavy. ecolRxC IPF dropped (~10 pp simulation error); Thomsen untested (~46 CPU-hours).
- Present results as 5×3 (Lula / Bolsonaro / neither) by summing the 5×4 columns; no refit needed.

## Next
1. **Uncertainty.** Cluster bootstrap over zonas within UF, giving intervals for national, regional and UF flows.
   Report method spread (lphom / tslphom / nslphom) as a model-uncertainty band.
2. **Validation.**
   - ESEB 2022 microdata (recalled vote in both rounds) against the 2022 estimates, focusing on the outros and
     invalid rows.
   - Out-of-sample check within zona: fit on half the seções, predict the other half, against baselines.
   - UF-by-UF comparison with lorande/transicao-votos-2026 (`saida/transicao_uf_matrizes.csv`).
3. **Outputs.**
   - Figures: national heatmap, UF small multiples, sensitivity, swing bars for 2022 vs 2026, regional decomposition.
   - `report.qmd`.
4. **Housekeeping.** renv init and snapshot; first commit (data/raw ignored).

## Incumbency + coattails (in progress, 2026-10-30)
- **Incumbency** is tested on presidential runoffs across years:
  - incumbent: 2006, 2014, 2022, 2026;
  - endorsed successor: 2002, 2010;
  - open seat: 2018.

  PT is on the government side in 2006, 2010, 2014 and 2026, which partly separates incumbency from PT.
  Order: 2014 first, then 2018, 2010, 2006 and 2002. Seção files exist for every year; the polling-place file only
  from 2010, but voto em trânsito also only starts in 2010.
- **Coattails** are tested on governor runoffs: does the governor candidate aligned with the presidential candidate
  who won the mobilisation contest also win it, and track that candidate zona by zona?
  - 2026 is done (scripts 10–13): DF, RN and ES are above the null correlation, and state incumbency did not hold.
  - 2022 is the strong test, because the alignment flips (Bolsonaro was the incumbent).
  - Pending: user confirmation of `alinhamento_presidencial`.

## Incumbent advantage in runoffs (original notes)
Claim to test: the incumbent wins the between-round contest. It held in 2022 (Bolsonaro) and in 2026 (Lula),
which is only n=2.

### A. Earlier presidential runoffs
- Incumbent in the runoff: 2006 (Lula), 2014 (Dilma). Open seat: 2002, 2010, 2018.
- Check availability and format of the seção-level files, then rerun the pipeline with `ANO=`.

### B. Contemporaneous governor runoffs (cargo 3)
- Cases:
  - 2022, 12 states: AL, AM, BA, ES, MS, PB, PE, RO, RS, SC, SE, SP.
  - 2026, 6 states: AC, AM, DF, ES, RN, TO.
  - Later, 2014 and 2018 if the files exist.
- Data:
  - Per-UF `votacao_secao_<ano>_<UF>.zip`; the BR file is presidential only.
  - `consulta_cand` has no re-election flag (`ST_REELEICAO` is absent in 2022 and 2026).
- Hand-code, with sources and user confirmation, each runoff candidate's status:
  - sitting governor seeking re-election;
  - vice who took over after a resignation;
  - endorsed successor;
  - challenger or open seat;
  - plus alignment with the presidential incumbent.
- Confound: the presidential runoff is on the same ballot, and turnout is a single decision per voter.
  Identify incumbency from cases where state incumbency and presidential alignment cut across each other
  (e.g. DF 2026: incumbent PP governor vs PT challenger), as a 2×2 of incumbent × aligned.
- Outcome per runoff: each finalist's between-round net gain from also-rans, invalid voters, abstainers and the
  other finalist (07_inout logic, cargo-parameterised).
- Work:
  - Add a cargo parameter to 01, 02 and 05.
  - Rows are the two governor finalists, outros, invalid and abstention.
  - Per-zona LP per state (minutes each).
