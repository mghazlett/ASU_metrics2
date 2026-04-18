# Authpop Extension — Sample Size Guide

This guide explains which observations are included in each figure and table, and why.

---

## Episode Universe

The authpop extension draws from FST's 28 core populist episodes (`ple_dataset.dta`), plus
one added episode (Hungary/Orbán 2010) sourced from the extended FST dataset. Total episodes
in `authpop_episodes.csv`: **29**.

| Subset | N | Definition |
|--------|---|------------|
| FST full (`fst_full`) | 28 | All original FST episodes — excludes Hungary (HUN 2010) |
| Strict | 9 | In electoral or closed autocracy **at takeover** (V-Dem ≤ 1 at Year) |
| Broad | 14 | Strict + leaders who crossed into autocracy **during tenure** |
| Non-authpop (`nonauthpop`) | 15 | FST episodes where `auth_broad == 0` — never autocratic |
| Strict (no Ecuador) | 6 | Strict minus 3 Velasco Ibarra spells |
| Broad (no Ecuador) | 11 | Broad minus 3 Velasco Ibarra spells |

### Why 29 total but non-authpop = 15 (not 14)?

FST's original 28 episodes split as **13 broad-authpop + 15 non-authpop = 28**.
Hungary/Orbán (added) is broad-authpop, bringing broad from 13→14.
Non-authpop stays at 15 because Orbán is *in* the broad group, not outside it.

| Partition | From FST core (28) | Added | Total |
|-----------|-------------------|-------|-------|
| Broad authpop | 13 | +1 (HUN 2010) | **14** |
| Non-authpop | 15 | 0 | **15** |
| **Total** | **28** | **1** | **29** |

---

## Episode Lists

**Strict (N=9):**
Perón ARG 1946, Perón return ARG 1973, Paz Estenssoro BOL 1952, Vargas BRA 1951,
Ibáñez CHL 1952, Velasco Ibarra ECU 1952, Velasco Ibarra ECU 1960,
Velasco Ibarra ECU 1968, Echeverría MEX 1970

**Broad adds (N=+5, bringing broad to 14):**
Gandhi IND 1966 (Emergency 1975), Fujimori PER 1990 (autogolpe 1992),
Erdoğan TUR 2003 (autocratization 2013), Chávez VEN 1999 (consolidation 2002),
**Orbán HUN 2010** (crossed to electoral autocracy 2018; added from extended FST dataset)

**Non-authpop (N=15) — all from FST core, auth_broad == 0:**
Menem ARG 1989, Kirchner ARG 2003, Collor BRA 1990, Bucaram ECU 1996,
Netanyahu ISR 1996, Berlusconi ITA 1994, Berlusconi ITA 2001, Koizumi JPN 2001,
Muldoon NZL 1975, García PER 1985, Estrada PHL 1998, Roh KOR 2003,
Mečiar SVK 1990, Chen TWN 2000, Thaksin THA 2001

**Ecuador robustness:** Velasco Ibarra ECU 1952, ECU 1960, ECU 1968 are three spells
of the same leader — 33% of the strict sample, 27% of broad. Dropping them tests
concentration risk. No-Ecuador: strict N=6, broad N=11.

---

## Figures and Tables

### Table AP1 — Episode Lists
**Subsets:** broad (N=14) with strict subset flagged in Panel A, broad-only in Panel B.
**Unit of observation:** One row per episode.
**Note:** Non-authpop episodes are not listed in the table; reference
`authpop_episodes.csv` for the full 29-episode classification.

---

### Figure AP3 — Average GDP Gap (Stata synth, placebo CI)
**Subsets:** strict, broad, strict_noecuador, broad_noecuador → 4 PDFs
**Episodes per subset:** 9, 14, 6, 11
**What it shows:** Average GDP gap (treated minus synthetic control) by year
relative to takeover, with 90% CI from placebo permutations. Estimated via
Stata `synth`. Similar to FST Figure 3.

---

### Figure AP4 — Jordà Local Projections (OLS/FE Event Study)
**Subsets:** strict (N=9), broad (N=14) → 2 PDFs
**Observations:** Full `authpop_dataset.dta` panel. Coefficients β_h estimated for
event time h = −15 to +15 via OLS with country and year FE. HC3 robust confidence bands.

---

### Figure AP5 — IPW Event Study
**Subsets:** strict (N=9), broad (N=14) → 2 PDFs
**Method:** Inverse-probability-weighted local projections. Propensity scores from
logit of treatment on pre-takeover GDP level, growth, and institutional quality.
Overlap is limited given N=9/14; interpret with caution.

---

### Figure AP6 — Main SCM (scpi, sims=200)
**Subsets:** strict, broad, strict_noecuador, broad_noecuador → 4 PDFs
**Episodes per subset:** 9, 14, 6, 11
**Also estimated (for AP9):** nonauthpop (N=15), fst_full (N=28)
**Method:** `scpi` package, L1 simplex constraint, Gaussian simulation (sims=200),
period.pre=0:15, period.post=16:30. Donor pool excludes countries with their own
populist episode in the data window.
**Ecuador gap:** ECU's GDP data begins ~1937. For the 1952 episode, the doppelganger
line has a visible gap at t=2–8. This is a historical data availability constraint
(FST's own fr2=1, fr3=9 encoding), not an estimation error.

---

### Figure AP7 — SCM Robustness (sims=10)
**Subsets:** strict, broad, strict_noecuador, broad_noecuador → 4 PDFs
**Same as AP6** except sims=10. Confidence bands are wider; the central GDP path
is identical to AP6. Purpose: verify results are not sensitive to simulation count.

---

### Figure AP8 — SCM, 5-Year Post Window
**Subsets:** strict, broad, strict_noecuador, broad_noecuador → 4 PDFs
**Difference from AP6:** Post-period evaluation restricted to years 1–5 after takeover.
Pre-period weights are estimated on the same 15-year pre-window as AP6; only the
evaluation horizon is truncated.
**Motivation:** Dornbusch & Edwards (1991) document populist damage peaking in
years 3–5. Strict authpop leaders have median tenure ~5 years, making this the
most policy-relevant horizon for that subset.

---

### Figure AP9 — Three-Group Comparison (FST full / authpop broad / non-authpop)
**Subsets compared:** fst_full (N=28), broad (N=14), nonauthpop (N=15)
**Method:** Each group's doppelganger estimated independently via `scpi` (same
parameters as AP6). Gap = mean(actual − doppelganger) by event year, normalized
to zero at takeover.
**What it tests:** If authoritarian context amplifies populist damage, the authpop
(red) line should diverge more below zero than the non-authpop (blue) line post-takeover.
**Color coding:** gray solid = FST full; red longdash = broad authpop; blue dotdash = non-authpop.
**Note:** AP6 results for fst_full and nonauthpop subsets are saved in `finaldata_ap6`
list and reused here — no re-estimation.

---

### Figure AP12 — Alternative Outcomes: Trade Openness
**Subsets:** strict, broad, strict_noecuador, broad_noecuador → 4 PDFs
**Outcome variables:** KOF trade globalization index, trade-to-GDP ratio.
**N:** Better coverage than institutional outcomes. Trade data extends to ~1950.

---

### Figure AP13 — Alternative Outcomes: Macro / Finance
**Subsets:** strict, broad, strict_noecuador, broad_noecuador → 4 PDFs
**Outcome variables:** Debt-to-GDP (Reinhart-Rogoff), inflation.
**N:** Debt and inflation data extend to 1930s–1940s; most episodes converge.
**Venezuela outlier:** Chávez hyperinflation dominates the broad inflation average.
See AP13-noVEN for robustness.

---

### Figure AP13-noVEN — Macro / Finance, Excluding Venezuela
**Subsets:** strict, broad (excl. VEN), strict_noecuador, broad_noecuador (excl. VEN) → 4 PDFs
**Difference from AP13:** Venezuela (Chávez VEN 1999) dropped from broad subsets.
Broad drops from N=14 to N=13; broad_noecuador from N=11 to N=10.

---

### Figure AP14 — Alternative Outcomes: Institutional Quality
**Subsets:** strict, broad, strict_noecuador, broad_noecuador → 4 PDFs
**Outcome variable:** V-Dem liberal democracy index (`v2x_libdem`).
**N:** V-Dem covers all countries back to 1789; full subset coverage expected.
**Interpretation caution:** Auth-pop leaders by definition move toward autocracy.
The institutional decline is partly tautological — present as corroboration that
the V-Dem classification captures observable regime change, not as an independent
finding about institutional damage.

---

### Table AP2 — OLS/FE Regression Table (combined strict + broad)
**Subsets:** strict (N=9), broad (N=14) in a single 6-column table.
**Specifications:** (1) OLS, (2) OLS + year FE, (3) OLS + country and year FE;
repeated for 5-year and 15-year post-takeover windows.
**Outcome:** Annual real GDP per capita growth (`rgdppc_gr`).
**Sample:** Country-years 1946–2019, non-missing GDP. Control group = all
non-treated country-years.

### Table AP2c — OLS/FE Regressions, Non-authpop Subset (N=15)
**Subset:** nonauthpop — 15 FST episodes with auth_broad == 0.
**Same specification as AP2.** Serves as the comparison group: a larger negative
coefficient here (relative to AP2) would imply the FST finding is not driven by
the authoritarian segment. Current results: 5-yr −0.94* to −1.02*; 15-yr −1.03***
to −0.82**, comparable or stronger than the broad authpop estimates.

---

### Table AP3 — Predictor Balance (Stata synth)
**Subsets:** strict (N=9), broad (N=14) → 2 tex files
**What it shows:** Pre-treatment average predictor values for treated units vs.
synthetic controls vs. unweighted donor average.
**Overflow fix:** Tables are wrapped in `\begin{adjustbox}{max totalheight=0.82\textheight}`
in the main .tex file. This scaling is permanent and survives pipeline reruns
(Stata regenerates the raw .tex; the wrapper lives in the main file).

---

## Summary: Why N Varies Across Figures

| Reason | Affects |
|--------|---------|
| Hungary added (not in FST core 28) | Broad N=14 vs. fst_full N=28 |
| Non-authpop = 15 (not 14) | AP9, AP2c — see universe section above |
| Data availability pre-WWII | AP6, AP7, AP8, AP13 for strict subset |
| Ecuador GDP gap (fr2=1, fr3=9) | Right-wing panels in AP6, AP7, AP8 |
| Venezuela outlier removed | AP13-noVEN: broad N=13, broad_noecuador N=10 |
| V-Dem institutions: complete coverage | AP14 (N = full subset size) |
| FE uses all country-years; SCM uses donors | AP4/AP5 observation count ≠ AP6 |
| Alt-outcome figures (AP12–AP14) restricted | Only 4 authpop subsets; no nonauthpop/fst_full |
| No-Ecuador robustness subsets | AP3, AP6–AP14 have 4-variant outputs |
