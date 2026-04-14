# Authpop Extension — Sample Size Guide

This guide explains which observations are included in each figure and table, and why.

---

## Episode Universe

The authpop extension draws exclusively from FST's 28 core populist episodes
(`ple_dataset.dta`). No new database is constructed. The 28 episodes are
re-classified by whether the leader came to power in, or later created, an
autocratic regime (V-Dem `v2x_regime ≤ 1`).

| Subset | N | Definition |
|--------|---|------------|
| Strict | 9 | In electoral or closed autocracy **at takeover** (V-Dem ≤ 1 at Year) |
| Broad | 13 | Strict + became authoritarian **during tenure** |
| Strict (no Ecuador) | 6 | Strict minus 3 Velasco Ibarra spells |
| Broad (no Ecuador) | 10 | Broad minus 3 Velasco Ibarra spells |

**Strict episodes (N=9):** ARG 1946, ARG 1973, BOL 1952, BRA 1951, CHL 1952,
ECU 1952, ECU 1960, ECU 1968, MEX 1970

**Broad adds (N=+4):** IND 1966 (Gandhi; Emergency 1975), PER 1990 (Fujimori;
autogolpe 1992), TUR 2003 (Erdoğan; 2013 consolidation), VEN 1999 (Chávez;
2002 consolidation)

**Ecuador robustness:** ECU 1952, ECU 1960, ECU 1968 are all Velasco Ibarra.
One leader = 33% of the strict sample. Dropping them tests concentration risk.

---

## Figures and Tables

### Table AP1 — Episode Lists
**Subsets:** strict (N=9), broad (N=13)
**Unit of observation:** One row per episode.
**Why only 2 versions:** Descriptive table; no-Ecuador variants are the same
list minus 3 rows. No separate table needed.

---

### Figure AP1 — Authoritarian Episodes in Context
**Observations:** All 28 FST populist episodes, plotted by takeover year.
**Unit:** Episode-level scatter. Points coded as: strict authpop (filled circle),
broad-only authpop (triangle), non-authpop (open circle).
**Purpose:** Descriptive context showing what share of FST's universe is
authoritarian-populist and when the episodes cluster.

---

### Figure AP2 — Episode Timeline (Stripplot)
**Observations:** The 13 broad authpop episodes only.
**Unit:** Episode-level scatter, grouped by country on the y-axis.
**Color coding:** Left-wing strict (dark blue), left-wing broad-only (light blue),
right-wing strict (dark red), right-wing broad-only (light red).
**Purpose:** Shows geographic and temporal spread; distinguishes left/right and
strict/broad-only classification.

---

### Figure AP3 — Growth Gap Bar Chart
**Subsets:** strict, broad, strict_noecuador, broad_noecuador → 4 PDFs
**Observations per subset:** All country-years for the treated episode, up to
±15 years around takeover. GDP data from `fstgdp` (Maddison + extension).
**What it shows:** Average GDP growth gap (auth-pop minus synthetic control)
over 5-year and 15-year post-treatment windows, for country benchmark and
global benchmark. Similar to FST Figure 2.
**Why some episodes may drop:** Episodes near the data boundary (pre-WWII)
may lack 15 years of pre-treatment data; these still appear in 5-year results
but may have wider intervals in 15-year.

---

### Figure AP4 — OLS/FE Event Study (Local Projections)
**Subsets:** strict (N=9), broad (N=13) → 2 PDFs
**Observations:** The full authpop_dataset.dta panel filtered to auth-pop
episodes (`atakeover_strict == 1` or `atakeover_broad == 1`), spanning up to
±15 years around each takeover. All country-years within the window are used;
treated units identified by `atakeover_strict/broad`.
**No-Ecuador variant:** Run inline by dropping ECU episodes before the
regression — not a separate PDF output.
**Estimator:** OLS with country and year fixed effects (FE). The event-time
coefficients β_h are estimated via:
```
rgdppc_gr_h = β_h × ap + country FE + year FE + ε
```
for h = −15 to +15. Confidence bands are HC3 robust standard errors.
**Why N differs from SCM:** FE uses all years for all countries in the window.
SCM uses only the treated unit and its matched donors. Different identification.

---

### Figure AP5 — Propensity Score / IPW Event Study
**Subsets:** strict (N=9), broad (N=13) → 2 PDFs
**Observations:** Same panel as AP4. Propensity scores estimated on baseline
covariates (GDP level, growth, institutions at takeover year). IPW reweights
the comparison group. Limited overlap likely given N=9/13 — this should be
noted in any paper presentation.
**Estimator:** Inverse Probability Weighting (IPW) event study. Weights
estimated via logit of treatment on pre-treatment covariates.

---

### Figure AP6 — Main SCM (Doppelganger)
**Subsets:** strict, broad, strict_noecuador, broad_noecuador → 4 PDFs
**Episodes per subset:** 9, 13, 6, 10 (see universe above)
**Observations per episode:** Country-years from `Year - 15` to `Year + 15`
(30-year window). Donor pool = all FST countries not themselves taking over
in that year. `scpi` package, L1 simplex constraint, Gaussian simulation
(sims=200), period.pre=0:15, period.post=16:30.
**How many converge:** Not all 9/13 episodes necessarily converge. SCM
requires sufficient donor overlap in the pre-period. Near-data-end episodes
(e.g., VEN 1999 — only 15 years of post-period data available by dataset end)
may have fewer post-period observations.
**Right-wing panel breaks:** ALL strict right-wing episodes are Ecuador
(Velasco Ibarra). ECU's GDP data begins ~1937, so for the 1952 episode the
doppelganger line can only be drawn at t = {0, 1} and t = {9..30}, leaving a
visible gap at t = 2..8. This reflects FST's own `fr2=1, fr3=9` encoding for
Ecuador — it is a historical data availability constraint, not an analysis
artifact. In the broad subset, Turkey (2003, right-wing) adds a case with
complete data, filling the gap.
**Output files:** `FigureAP6_strict.pdf`, `FigureAP6_broad.pdf`,
`FigureAP6_strict_noecuador.pdf`, `FigureAP6_broad_noecuador.pdf`

---

### Figure AP7 — SCM Robustness (sims=10)
**Subsets:** strict, broad, strict_noecuador, broad_noecuador → 4 PDFs
**Same as AP6** except `sims=10` instead of 200. Confidence bands will be
wider/noisier due to fewer simulations. Purpose: verify that results are not
sensitive to simulation count, and enable fast re-runs during development.

---

### Figure AP8 — Augmented SCM
**Subsets:** strict, broad, strict_noecuador, broad_noecuador → 4 PDFs
**Episodes per subset:** same as AP6
**Difference from AP6:** Pre-period window shortened to 0:10 (10 years)
instead of 0:15. This allows more episodes to converge — episodes with only
10–14 years of pre-treatment GDP data (common for 1946–1955 episodes) can
now be included. Post-period remains 11:30.
**Why shorter pre-window?** Strict episodes start as early as 1946, requiring
data back to 1931. Some countries lack GDP data before WWII. The 10-period
pre-window pushes the data requirement back only to 1936 for a 1946 episode,
improving coverage. The trade-off is less pre-period match quality.
**Note:** Because the augsynth window is clean (0 to 30 with no gaps), the
doppelganger line covers the full 31-point range for all episodes regardless
of the Ecuador GDP break.

---

### Figure AP9 — Penalized SCM (LowRankQP)
**Subsets:** strict, broad, strict_noecuador, broad_noecuador → 4 PDFs
**Per subset:** 3 panels — λ=0 (standard SCM), λ=0.1, λ=optimal (CV-selected)
**Episodes per panel:** All converged episodes in the subset.
**Method:** LowRankQP cross-validation over a grid of λ from 0 to 5. Optimal
λ minimizes leave-one-out pre-period fit error. Higher λ penalizes donor
weight concentration (regularization).
**Panel titles:** λ = Greek letter lambda (Λ); prior code had literal
`\u03BB` string — fixed to proper unicode in output.

---

### Figure AP10 — Pooled Multisynth
**Subsets:** strict, broad, strict_noecuador, broad_noecuador → 4 PDFs
**Method:** `augsynth::multisynth()` — pools all episodes in the subset into
a single pseudo-panel and estimates a joint synthetic control. Each episode
is a treated unit; treatment is staggered.
**N used:** All episodes in the subset with sufficient data for multisynth.
**Advantage:** Exploits variation across episodes jointly; reduces the
influence of any single episode.

---

### Figure AP11 — Alternative Outcomes: Gini & Labor Share
**Subsets:** strict, broad, strict_noecuador, broad_noecuador → 4 PDFs
**Outcome variables:** `gini` (income inequality), `laborshare` (labor income share)
**N per panel:** Smaller than AP6 — gini and laborshare data are sparse for
pre-1970 episodes. Only episodes where the treated unit AND enough donors
have pre-period data for the outcome variable are included.
**Why N is lower:**
- Gini data (SWIID, WID) is largely unavailable before 1960–1970.
- Strict subset episodes are 1946–1970; almost all will lack 15-year
  pre-period gini data → very low or zero converged episodes for strict.
- Broad subset includes 1990 and 1999 episodes (Fujimori, Chávez) which
  have better gini/laborshare coverage.
- The figure will report N in each panel title; N=0 panels render as empty.
**Donor filter (revised):** Donors must have complete data in the pre-period
(year ≤ Year) only. Post-period gaps are tolerated. This recovers some donors
that have early post-period missingness.

---

### Figure AP12 — Alternative Outcomes: Trade
**Subsets:** strict, broad, strict_noecuador, broad_noecuador → 4 PDFs
**Outcome variables:** `koftrade` (KOF trade globalisation index),
`tradegdp` (trade/GDP ratio), `global` (KOF globalisation)
**N per panel:** Better coverage than AP11. Trade data (Penn World Tables,
WDI) extends back to ~1950. Strict subset episodes should have reasonable
coverage; some early (pre-1950) episodes may still have limited donors.

---

### Figure AP13 — Alternative Outcomes: Macro / Finance
**Subsets:** strict, broad, strict_noecuador, broad_noecuador → 4 PDFs
**Outcome variables:** `debtgdp` (public debt/GDP), `inflation`, `bankcrisis`
**N per panel:** Better coverage than gini/laborshare. Debt and inflation
data extend back to the 1930s–1940s (Reinhart & Rogoff, IFS). Strict
subset episodes should converge for most.

---

### Figure AP14 — Alternative Outcomes: Institutions
**Subsets:** strict, broad, strict_noecuador, broad_noecuador → 4 PDFs
**Outcome variable:** `institutions` (V-Dem liberal democracy index or
composite institutional quality)
**N per panel:** V-Dem covers all countries back to 1789; coverage should
be complete for all episodes. Expect N = full subset size.
**Interpretation caution:** Auth-pop leaders by definition move toward
autocracy — the institutional degradation result is partly tautological.
Present as corroboration of the selection mechanism, not an independent
finding.

---

### Table AP2 — OLS/FE Regression Table
**Subsets:** strict (N=9), broad (N=13) → 2 tex files
**Observations:** Country-years in the authpop_dataset.dta panel, filtered to
the relevant subset. Post-treatment indicators `Post_5_strict` / `Post_15_strict`
(= 1 in years 1–5 / 1–15 after takeover) identify the treatment window.
**Specifications per table:**
1. OLS with `Post_5` indicator, robust SE
2. OLS with `Post_15` indicator, robust SE
3. FE (country + year), `Post_5`
4. FE (country + year), `Post_15`
5. Matched/IPW with `Post_5`
6. Matched/IPW with `Post_15`
**Outcome:** Annual per-capita GDP growth (`rgdppc_gr`)
**Sample:** Observations where `year >= 1946` and treatment indicator active.
The control group is all non-authpop country-years in the dataset.

---

### Table AP3 — Predictor Balance (Synthetic Control)
**Subsets:** strict (N=9), broad (N=13) → 2 tex files
**Method:** Stata `synth` command, pre-period lags of GDP as predictors.
**What it shows:** For each auth-pop episode, the pre-treatment averages of
key predictors for: (1) the treated unit, (2) the synthetic control, (3) the
unweighted donor average. A well-balanced synthetic control should have
column (1) ≈ column (2) with column (2) closer than column (3).
**N:** One row per episode per predictor. All 9 (strict) or 13 (broad)
episodes attempted; any that fail to converge in `synth` are excluded from
the table with a note.

---

## Summary: Why N Varies Across Figures

| Reason | Affects |
|--------|---------|
| Data availability pre-WWII | AP6, AP7, AP8, AP11, AP13 for strict subset |
| Ecuador GDP gap (fr2=1, fr3=9) | Right-wing panels in AP6, AP7 |
| Gini/laborshare sparse before 1970 | AP11, especially strict subset |
| V-Dem institutions: complete coverage | AP14 (N = full subset) |
| FE uses all country-years; SCM uses donors | AP4/AP5 N > AP6 N |
| Multisynth pools episodes jointly | AP10 (N may differ from AP6) |
| No-Ecuador robustness subsets | AP3, AP6–AP14 have 4-variant outputs |

