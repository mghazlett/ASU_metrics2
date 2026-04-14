# Authoritarian Leaders Extension
### An Extension of Funke, Schularick & Trebesch (2023) to Non-Populist Authoritarian Leaders

---

## Overview

This folder contains a self-contained extension to the AER 2023 replication of
"Populist Leaders and the Economy" (Funke, Schularick & Trebesch, FST). The
extension asks whether the negative economic effects documented for populist
leaders in the FST paper generalize to authoritarian leaders who were *not*
classified as populists.

The analysis mirrors the FST methodology exactly — using the same dataset,
the same estimation approaches (simple growth gaps, local projections,
inverse-probability weighting, and synthetic control) — applied to a new
episode list of non-populist authoritarian takeovers.

---

## Repository Structure

```
auth_extension/
├── data/
│   ├── auth_dataset.dta                        Output of datprep_auth.do; merged panel
│   ├── authoritarian_episodes_scm_viable.csv   Episode list (90 spells, 84 non-populist)
│   ├── authoritarian_episodes_draft.csv        Working draft (do not use directly)
│   ├── authoritarian_episodes_final.csv        Intermediate cleaned list
│   ├── authoritarian_episodes_refined.csv      Pre-final version
│   └── vdem_regime_ple_countries.csv           V-Dem regime type checks
│
├── figures/
│   ├── FigureA_3.pdf    Average annualized growth gap (analogue of FST Figure 3)
│   ├── FigureA_4.pdf    GDP local projections (analogue of FST Figure 4)
│   ├── FigureA_5.pdf    IPW local projection (analogue of FST Figure 5)
│   ├── FigureA_6.pdf    SCM GDP paths — all authoritarians
│   ├── FigureA_7.pdf    SCM GDP paths — by regime type (3 panels)
│   ├── FigureA_8.pdf    Placebo test (time placebo)
│   ├── FigureA_8_bytype.pdf  Placebo by regime type
│   ├── FigureA_9.pdf    Penalized synthetic control (pen/LowRankQP)
│   ├── FigureA_10.pdf   Pooled synthetic control (multisynth/augsynth)
│   ├── FigureA_11.pdf   Distributional outcomes: Gini index + labor share
│   ├── FigureA_12.pdf   Openness outcomes: trade openness + financial openness
│   ├── FigureA_13.pdf   Macro outcomes: debt/GDP + inflation + banking crisis
│   └── FigureA_14.pdf   Institutional quality
│
├── tables/
│   ├── TableA_C1.tex    Propensity score logit marginal effects
│   └── TableA_C3.tex    IPW regression coefficients
│
├── programs/
│   ├── build_auth_episodes.R           Constructs episode CSV from GWF/V-Dem
│   ├── check_auth_data_coverage.R      Variable coverage diagnostics
│   ├── check_vars.do                   Stata variable audit
│   ├── datprep_auth.do                 Data preparation: merges episodes into PLE panel
│   ├── figtabs_auth.do                 Figures A_3–A_5 and Tables A_C1, A_C3 (Stata)
│   ├── ranscm_auth.R                   SCM: Figures A_6–A_7 (main GDP paths)
│   ├── ranscm_auth_placebo.R           Placebo: Figures A_8, A_8_bytype
│   ├── ranscm_auth_pen.R               Penalized SCM: Figure A_9
│   ├── ranscm_auth_ppool.R             Pooled SCM: Figure A_10
│   ├── ranscm_auth_outcomes.R          Outcome SCM (original; superseded)
│   ├── ranscm_auth_outcomes_a11a12.R   Outcome SCM: Figures A_11–A_12
│   ├── ranscm_auth_outcomes_cont.R     Outcome SCM: Figures A_13–A_14
│   └── verify_figA3.do                 Spot-check of Figure A_3 values
│
├── logs/
│   ├── datprep_auth.log
│   └── figtabs_auth.log
│
├── run_all_auth.bat                    Master runner (all steps in sequence)
├── run_datprep_auth.bat
├── run_figtabs_auth.bat
├── run_ranscm_auth.bat
├── run_ranscm_auth_placebo.bat
├── run_ranscm_auth_pen.bat
├── run_ranscm_auth_ppool.bat
├── run_ranscm_auth_outcomes.bat
└── run_ranscm_auth_outcomes_cont.bat
```

---

## Data

### Base Dataset

The extension uses `../data/ple_dataset.dta` — the main FST panel dataset — as
its foundation. `datprep_auth.do` merges authoritarian treatment indicators into
this panel and saves `data/auth_dataset.dta`.

The PLE panel covers approximately 60 countries from 1900 to the mid-2010s,
with annual observations on GDP per capita, institutional quality, inflation,
debt, trade, financial openness, and crisis indicators.

### Episode List

`data/authoritarian_episodes_scm_viable.csv` contains 90 authoritarian leader
episodes, of which **84 are non-populist** (i.e., the leader does not also
appear in the FST populism paper). Six episodes overlap with the FST populist
sample (e.g., Perón) and are excluded from all estimations using the
`is_also_populist` flag.

Episodes were constructed by:
1. Identifying authoritarian takeovers in the GWF (Geddes-Wright-Frantz)
   database for the set of countries covered by the FST panel.
2. Cross-referencing with V-Dem regime classifications.
3. Requiring at least 5 pre-treatment years of GDP data and a donor pool
   of at least 5 countries with complete outcome data (`scm_ok == TRUE`).

**Regime type breakdown (non-populist episodes):**

| Type                    | N  | Share |
|-------------------------|----|-------|
| Single-party            | 24 | 29%   |
| Closed (unclassified)   | 23 | 27%   |
| Military-personal       | 14 | 17%   |
| Personalist             | 10 | 12%   |
| Military                | 10 | 12%   |
| Oligarchy               |  2 |  2%   |
| Monarchy                |  1 |  1%   |

**Time span:** 1901–2012 (GWF coverage window).

**Key episode CSV columns:**

| Column             | Description                                        |
|--------------------|----------------------------------------------------|
| `country`, `iso`   | Country name and ISO code                          |
| `cid`              | Numeric country ID matching PLE panel              |
| `leader`           | Leader name                                        |
| `start_yr`         | Year of takeover                                   |
| `end_yr`           | Year regime ended                                  |
| `auth_type`        | Regime type (Single-party, Military, etc.)         |
| `is_also_populist` | 1 if also in FST populism sample                   |
| `scm_ok`           | TRUE if episode passes SCM viability criteria      |

### Treatment Variables (in auth_dataset.dta)

| Variable          | Definition                                              |
|-------------------|---------------------------------------------------------|
| `atakeover_auth`  | =1 in the year the authoritarian leader takes power     |
| `auth`            | =1 while authoritarian leader is in power               |
| `auth_yrs`        | Years since takeover (0 = takeover year)                |
| `Post_5_auth`     | =1 in years 0–4 after takeover                         |
| `Post_15_auth`    | =1 in years 0–14 after takeover                        |
| `auth_type_str`   | Regime type string label                                |
| `auth_is_populist`| =1 if episode also in FST populism paper                |

---

## Estimation Pipeline

### Step 1: Data Preparation (Stata)

```
datprep_auth.do
```

Reads `authoritarian_episodes_scm_viable.csv`, expands each episode into a
country-year panel, and merges treatment indicators into `ple_dataset.dta`.
Output: `data/auth_dataset.dta`.

### Step 2: Figures A_3–A_5 and Tables (Stata)

```
figtabs_auth.do
```

Exact analogues of FST Figures 3–5:

- **Figure A_3**: Four-bar chart of average annualized growth gap (country and
  global benchmarks; 5-year and 15-year horizons). Constructed by looping over
  all non-populist authoritarian takeover spells and computing the growth rate
  deviation from the country mean and global mean, then averaging across spells.

- **Figure A_4**: Local projection impulse response (panel fixed effects),
  with Panel A showing all-authoritarian vs. normal years and Panel B showing
  the gap by regime type (single-party, military, personalist).

- **Figure A_5**: Inverse-probability weighted (IPW) local projection using
  eight propensity score specifications. Includes Table A_C1 (logit marginal
  effects) and Table A_C3 (IPW regression output).

### Step 3: SCM Main Results (R)

```
ranscm_auth.R
```

Runs `scpi` (Cattaneo et al.) for each of the 84 non-populist episodes.
45 out of 84 episodes converge successfully; failures are pre-WWII episodes
with insufficient pre-treatment data ("subscript out of bounds") or episodes
near the end of the data panel ("arguments imply differing number of rows").

- **Figure A_6**: Average SCM GDP path for all authoritarians vs. doppelganger,
  with out-of-sample Gaussian CIs.
- **Figure A_7**: Same, split by regime type (3 panels: single-party, military,
  personalist).

### Step 4: Robustness — Placebo, Pen, Pooled (R)

```
ranscm_auth_placebo.R   → Figures A_8, A_8_bytype
ranscm_auth_pen.R       → Figure A_9  (penalized SCM via LowRankQP)
ranscm_auth_ppool.R     → Figure A_10 (pooled SCM via augsynth/multisynth)
```

The placebo test assigns a fake treatment 10 years before the actual takeover
and checks whether the SCM gap is systematically zero in the "post" period,
validating the parallel-trends assumption. 37 episodes complete the placebo run.

### Step 5: Outcome SCM (R)

```
ranscm_auth_outcomes_a11a12.R   → Figures A_11 (Gini, labor share), A_12 (trade openness)
ranscm_auth_outcomes_cont.R     → Figures A_13 (debt, inflation, banking crisis), A_14 (institutions)
```

Runs the same SCM procedure with alternative outcome variables. Episode counts
vary by variable due to data availability:

| Outcome         | Episodes | Figure |
|-----------------|----------|--------|
| Gini index      | 11       | A_11   |
| Labor share     | 30       | A_11   |
| KOF trade index | 4        | A_12   |
| Trade/GDP       | 45       | A_12   |
| Financial open. | 4        | A_12   |
| Debt/GDP        | varies   | A_13   |
| Inflation       | varies   | A_13   |
| Banking crisis  | varies   | A_13   |
| Institutions    | varies   | A_14   |

---

## How to Run

### Full pipeline

```bat
run_all_auth.bat
```

Runs all steps in order using:
- Stata: `C:\Program Files\StataNow19\StataSE-64.exe`
- R: `C:\Program Files\R\R-4.3.0\bin\x64\Rscript.exe`

R uses `renv` (lockfile at `programs/renv.lock`) to ensure package versions
are reproducible. On first run, `renv::restore()` will install all dependencies
from CRAN/GitHub.

### Individual steps

Each step has a corresponding `.bat` file in the root of `auth_extension/`.
All `.bat` files assume they are launched from `C:\PLE\auth_extension\` or
set the working directory explicitly.

### Runtime

The Stata steps (A_3–A_5) run in under 10 minutes. The R SCM steps are
compute-intensive:

| Script                          | Approx. runtime |
|---------------------------------|-----------------|
| `ranscm_auth.R`                 | 3–5 hours       |
| `ranscm_auth_placebo.R`         | 4–6 hours       |
| `ranscm_auth_pen.R`             | 6–10 hours      |
| `ranscm_auth_ppool.R`           | 2–3 hours       |
| `ranscm_auth_outcomes_a11a12.R` | 3–5 hours       |
| `ranscm_auth_outcomes_cont.R`   | 2–4 hours       |

All R scripts are single-threaded (`cores = 1`). Running them overnight or in
parallel via separate terminals is recommended.

---

## Software Requirements

### Stata
- Stata/SE 19 (or MP)
- `sctest`, `reghdfe`, `estout`, `moremata` (all in `C:\PLE\programs\adosw\`)

### R (version 4.3.0)
Key packages (managed via `renv`):

| Package      | Purpose                              |
|--------------|--------------------------------------|
| `scpi`       | Synthetic control with uncertainty   |
| `augsynth`   | Pooled/multisynth (GitHub: ebenmichael/augsynth) |
| `LowRankQP`  | Penalized SCM solver                 |
| `haven`      | Read `.dta` files                    |
| `data.table` | Fast panel operations                |
| `ggplot2`    | Figures                              |
| `grid`       | Multi-panel PDF output               |
| `plyr`/`dplyr` | Data manipulation                  |

> **Note:** R 4.5.x lacks the required `dplyr`/`haven` versions in `renv`.
> Use R 4.3.0 (`C:\Program Files\R\R-4.3.0\bin\x64\Rscript.exe`).

> **Note:** Multi-panel figures use `grid::grid.layout()` rather than
> `patchwork`. The `patchwork` `+` operator and `wrap_plots()` fail silently
> when plots use `scale_linewidth_manual`, writing a corrupt 3.7 KB PDF.

---

## Known Limitations

1. **SCM coverage**: 45/84 episodes converge. Pre-WWII episodes commonly
   fail due to sparse GDP data; episodes starting within 15 years of the
   panel end fail due to insufficient post-treatment observations.

2. **Trade openness (KOF)**: Only 4 episodes have complete KOF data. The
   trade/GDP variable (45 episodes) is preferred.

3. **Missing post-2010 competitive authoritarians**: Erdogan (2013+), Orbán,
   el-Sisi, Maduro are not in the GWF database and would require manual coding.
   Results should be interpreted as applying to the historical (pre-2010) sample.

4. **Six overlapping episodes**: Perón (ARG 1946), Videla (ARG 1976), and four
   others appear in both the FST populism paper and this episode list
   (`is_also_populist == 1`). They are excluded from all estimations here but
   retained in the CSV for reference.

---

## Relationship to Main PLE Replication

This folder is self-contained. It reads `../data/ple_dataset.dta` from the
main PLE replication folder but does not modify it. All outputs (figures,
tables, data) stay within `auth_extension/`. The main PLE pipeline
(`C:\PLE\runall_inpaper_20260320.bat`) is unaffected.

---

## Key Findings

The main finding is a **contrast with the FST populism result**: while FST
documents a clear and persistent negative growth effect of populist leaders
(approximately −1 pp/year relative to the country benchmark over 15 years),
non-populist authoritarian takeovers show a markedly different pattern.

Using simple growth gap benchmarks (Figure A_3), authoritarians show modest
negative deviations from country and global growth norms, but these gaps are
substantially smaller and less consistent than the FST populism effect. The
local projection (Figure A_4) finds no robust negative impulse response for
all authoritarians as a group, with heterogeneity by regime type: personalist
regimes drive most of the negative signal while single-party regimes show near-
zero gaps.

The synthetic control estimates (Figures A_6–A_7) reinforce this: on average,
the actual GDP path of authoritarian countries does not diverge persistently
below the doppelganger counterfactual — and in some specifications the actual
path lies slightly above. This contrasts with the FST finding that populist
countries fall 10 percentage points below their counterfactual within 15 years.

Robustness checks (placebo, penalized SCM, pooled multisynth) confirm the
baseline picture. Outcome SCM for distributional and institutional variables
(Figures A_11–A_14) finds no consistent adverse effect of authoritarian
takeovers on inequality, labor share, trade openness, or institutional quality,
though data coverage for these variables is limited for the historical sample.

The results suggest that the economic costs documented by FST are specific to
the *populist* component of non-democratic leadership, not to authoritarianism
per se.
