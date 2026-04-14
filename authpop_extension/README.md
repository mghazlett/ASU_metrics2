# Authoritarian-Populist Subset Extension
### A Subset Analysis of Funke, Schularick, and Trebesch (2023)
#### *Do Authoritarian Populists Do More Economic Damage?*

---

## Overview

This folder contains a self-contained extension to the replication of
"Populist Leaders and the Economy" (Funke, Schularick & Trebesch 2023, AER;
hereafter FST). The extension asks whether the negative economic effects
documented for populist leaders are concentrated in the subset of populists
who governed — or created — authoritarian regimes.

Unlike the companion `auth_extension/` folder, which constructs a new database
of non-populist authoritarian leaders, this extension works exclusively within
FST's existing analytical sample. No new episodes are coded. Instead, FST's
28 data-complete populist episodes are reclassified using V-Dem's
`v2x_regime` indicator to identify which were authoritarian at the time of
takeover (strict definition) or became authoritarian during tenure (broad
definition). FST's core empirical strategies — OLS/FE regressions, local
projections, IPW, and synthetic control — are then replicated on these subsets.

The paper associated with this extension is:

> Hazlett, M. "Do Authoritarian Populists Do More Economic Damage?
> A Subset Extension of Funke, Schularick, and Trebesch (2023)."

---

## Repository Structure

```
authpop_extension/
├── data/
│   ├── authpop_episodes.csv       Canonical episode list: all 28 FST spells
│   │                              with auth_strict / auth_broad flags
│   └── authpop_dataset.dta        Output of datprep_authpop.do; flagged panel
│
├── figures/
│   ├── FigureAP1.pdf              All 28 FST episodes in context (scatter)
│   ├── FigureAP2.pdf              Authpop episode timeline (stripplot)
│   ├── FigureAP3_{subset}.pdf     Growth gap bar chart — 4 subset variants
│   ├── FigureAP4_{subset}.pdf     OLS/FE event study — strict, broad
│   ├── FigureAP5_{subset}.pdf     IPW event study — strict, broad
│   ├── FigureAP6_{subset}.pdf     Main SCM (scpi, sims=200) — 4 variants
│   ├── FigureAP7_{subset}.pdf     SCM robustness (sims=10) — 4 variants
│   ├── FigureAP8_{subset}.pdf     Augmented SCM (augsynth) — 4 variants
│   ├── FigureAP9_{subset}.pdf     Penalized SCM (LowRankQP) — 4 variants
│   ├── FigureAP10_{subset}.pdf    Pooled SCM (multisynth) — 4 variants
│   ├── FigureAP11_{subset}.pdf    Outcome SCM: Gini + labor share — 4 variants
│   ├── FigureAP12_{subset}.pdf    Outcome SCM: trade openness — 4 variants
│   ├── FigureAP13_{subset}.pdf    Outcome SCM: debt, inflation, crises — 4 variants
│   └── FigureAP14_{subset}.pdf    Outcome SCM: V-Dem institutions — 4 variants
│
├── tables/
│   ├── TableAP1_strict.tex        Episode list table, strict subset (N=9)
│   ├── TableAP1_broad.tex         Episode list table, broad subset (N=13)
│   ├── TableAP2_strict.tex        OLS/FE regression table, strict
│   ├── TableAP2_broad.tex         OLS/FE regression table, broad
│   ├── TableAP3_strict.tex        Synthetic control predictor balance, strict
│   └── TableAP3_broad.tex         Synthetic control predictor balance, broad
│
├── programs/
│   ├── datprep_authpop.do         Step 1: merges episode flags into PLE panel
│   ├── figtabs_authpop.do         Step 2a: Stata figures AP1–AP5, Tables AP1–AP2
│   ├── tableap3_authpop.do        Step 2b: Table AP3 (synth balance; fresh session)
│   └── ranscm_authpop.R           Step 3: R SCM figures AP6–AP14, all subsets
│
├── paper_authpop.tex              Paper source (LaTeX)
├── paper_authpop.pdf              Compiled paper (37 pages)
├── run_authpop.bat                Master runner (all steps in sequence)
├── sample_size_guide.md           Detailed guide to sample sizes per figure
└── FST-Online-Appendix.pdf        FST (2023) online appendix, for reference
```

> **Subset naming convention throughout:** `{subset}` is one of:
> `strict`, `broad`, `strict_noecuador`, `broad_noecuador`.
> Figures AP3 and AP6–AP14 have all four variants; AP4 and AP5 have
> only `strict` and `broad`.

---

## Sample Definition

This extension draws exclusively from FST's 28 analytical populist episodes
(`ple_dataset.dta`). The full list of 28 episodes is in `authpop_episodes.csv`;
the 19 non-authpop rows serve as context and are used only in Figure AP1.

### The Four Subsets

| Subset | N | Definition |
|--------|---|------------|
| Strict | 9 | V-Dem `v2x_regime ≤ 1` at the takeover year |
| Broad | 13 | Strict + 4 leaders who crossed into autocracy during tenure |
| Strict, no Ecuador | 6 | Strict minus 3 Velasco Ibarra spells |
| Broad, no Ecuador | 10 | Broad minus 3 Velasco Ibarra spells |

### Strict Episodes (N=9)

| # | Country | Year | Leader | Regime at takeover |
|---|---------|------|--------|--------------------|
| 1 | Argentina | 1946 | Perón | Electoral autocracy (V-Dem=1) |
| 2 | Argentina | 1973 | Cámpora/Perón return | Electoral autocracy (V-Dem=1) |
| 3 | Bolivia | 1952 | Paz Estenssoro | Electoral autocracy (V-Dem=1) |
| 4 | Brazil | 1951 | Vargas | Electoral autocracy (V-Dem=1) |
| 5 | Chile | 1952 | Ibáñez | Electoral autocracy (V-Dem=1) |
| 6 | Ecuador | 1952 | Velasco Ibarra I | Electoral autocracy (V-Dem=1) |
| 7 | Ecuador | 1960 | Velasco Ibarra II | Electoral autocracy (V-Dem=1) |
| 8 | Ecuador | 1968 | Velasco Ibarra III | Electoral autocracy (V-Dem=1) |
| 9 | Mexico | 1970 | Echeverría | Electoral autocracy (PRI hegemonic; V-Dem=1) |

### Broad Adds (N=+4)

| Country | Year | Leader | Autocratic event |
|---------|------|--------|-----------------|
| India | 1966 | Indira Gandhi | Emergency declaration 1975 (V-Dem crosses to 1) |
| Peru | 1990 | Fujimori | *Autogolpe* 1992 (V-Dem crosses to 0) |
| Turkey | 2003 | Erdoğan | Democratic consolidation reversal 2013 (V-Dem=1) |
| Venezuela | 1999 | Chávez | Consolidation 2002 (V-Dem crosses to 1) |

### Ecuador Robustness

Ecuador's three Velasco Ibarra spells (1952, 1960, 1968) are all the same
leader and account for exactly one-third of the strict sample (3/9). They are
not independent observations. Dropping them reduces concentration risk and
tests whether results are driven by a single political figure. The no-Ecuador
variants are constructed by filtering the episode list at the R and Stata
stages; they are not separate data files.

### Identification Note on Right-Wing Panels

In the strict subset, **all right-wing episodes are Ecuador** (Velasco Ibarra).
ECU's GDP data in the FST panel begins approximately 1937, encoded as `fr2=1,
fr3=9` in the PLE dataset. For the 1952 episode this means the synthetic
control doppelganger line covers only `t = {0,1}` and `t = {9..30}`, leaving
a visible gap at `t = 2..8` in scatterplot-style figures. This is a historical
data availability constraint inherited from FST's own parameter encoding, not
an analysis artifact. In the broad subset, Turkey (2003, right-wing) adds a
case with complete data, filling the gap.

---

## Episode Input File

`data/authpop_episodes.csv` — one row per FST analytical episode (N=28).

| Column | Description |
|--------|-------------|
| `iso` | ISO-3 country code; matches `ple_dataset.dta` |
| `year` | Takeover year |
| `leader` | Leader name as in FST |
| `left` | 1 = left-wing, 0 = right-wing (FST classification) |
| `oid` | FST original country ID (cid in ple_dataset) |
| `nid` | New numeric country ID (for ranscm_authpop.R loops) |
| `fr1` | First available year of data (relative to panel start) |
| `fr2` | First usable pre-treatment period index |
| `fr3` | Last pre-treatment period index |
| `auth_strict` | 1 if episode is in the strict subset |
| `auth_broad` | 1 if episode is in the broad subset (includes strict) |
| `notes` | Regime classification rationale |

The `fr1`, `fr2`, `fr3` parameters control the pre-treatment window in the
SCM estimation loop in `ranscm_authpop.R` and are inherited directly from
the FST panel construction.

---

## Estimation Pipeline

### Step 1 — Data Preparation (`datprep_authpop.do`)

Reads `data/authpop_episodes.csv` and `../data/ple_dataset.dta`.
Merges `auth_strict` and `auth_broad` takeover flags into the PLE panel
on `iso × year`, then generates the following treatment variables:

| Variable | Definition |
|----------|------------|
| `atakeover_strict` | =1 at the strict takeover year only |
| `atakeover_broad` | =1 at the broad takeover year only |
| `ltakeover_strict` | =1 at takeover year for left-wing strict episodes |
| `rtakeover_strict` | =1 at takeover year for right-wing strict episodes |
| `ltakeover_broad` | =1 at takeover year for left-wing broad episodes |
| `rtakeover_broad` | =1 at takeover year for right-wing broad episodes |
| `Post_5_strict` | =1 in years 1–5 after a strict takeover |
| `Post_15_strict` | =1 in years 1–15 after a strict takeover |
| `Post_5_broad` | =1 in years 1–5 after a broad takeover |
| `Post_15_broad` | =1 in years 1–15 after a broad takeover |

**Output:** `data/authpop_dataset.dta`

### Step 2a — Stata Figures and Tables (`figtabs_authpop.do`)

Produces all Stata-based figures and the first two tables. Loops over subsets
where applicable; no-Ecuador variants are constructed by dropping ECU rows
before running the estimation.

**Figure AP1** — Scatter of all 28 FST episodes by takeover year, coded as:
filled circle (strict authpop), triangle (broad-only authpop), open circle
(non-authpop). Provides descriptive context for what share of the FST universe
is authoritarian-populist.

**Figure AP2** — Horizontal strip plot of the 13 broad authpop episodes,
grouped by country on the y-axis. Colors: left-wing strict (dark blue),
left-wing broad-only (light blue), right-wing strict (dark red), right-wing
broad-only (light red).

**Figure AP3** *(4 variants)* — Four-bar chart of the average annualized growth
gap over 5-year and 15-year post-takeover windows, for country benchmark and
global benchmark. The analog of FST Figure 2. Constructed by computing the
deviation of GDP growth in each episode from the country-level mean and the
global mean, then averaging across episodes within the subset.

**Figure AP4** *(strict, broad)* — Jordà (2005) local projection with country
and year fixed effects. Coefficients `β_h` estimated for `h = −15` to `+15`.
Confidence bands are HC3 robust standard errors.

**Figure AP5** *(strict, broad)* — Inverse-probability-weighted (IPW) local
projection. Propensity scores estimated via logit of treatment on pre-takeover
GDP level, growth, and institutional quality. Overlap is limited given the
small treatment group (N=9/13); results are presented for robustness but
should be interpreted with caution.

**Table AP1** *(strict, broad)* — Episode list with country, year, leader,
wing, and regime classification. Two separate tables; no-Ecuador variants
are the same lists minus 3 rows and are not produced separately.

**Table AP2** *(strict, broad)* — OLS and two-way fixed-effects regression
table. Six columns: OLS and FE each for `Post_5` and `Post_15` windows, plus
IPW variants. Outcome variable is annual GDP per capita growth (`rgdppc_gr`).
Control group is all non-authpop country-years in `authpop_dataset.dta`.

### Step 2b — Predictor Balance Table (`tableap3_authpop.do`)

Run as a separate Stata session because it calls `synth` which can conflict
with the MATA library state left by the scpi-based Figure AP3 estimation.
Runs Stata's `synth` command for each episode in the strict and broad subsets
and collects predictor balance statistics (treated unit vs. synthetic control
vs. unweighted donor average) across 15 lags of GDP, institutions, inflation,
and pre-treatment crisis indicators.

**Table AP3** *(strict, broad)* — Predictor balance averaged across all
converged episodes in each subset.

### Step 3 — R Synthetic Control Estimation (`ranscm_authpop.R`)

A single R script producing Figures AP6–AP14 for all four subsets. Loops
over the subset list `c("strict","broad","strict_noecuador","broad_noecuador")`.
The no-Ecuador subsets are constructed inside R by filtering out `iso == "ECU"`
rows before estimation.

Run from `programs/` (the `.bat` file `cd`s there before calling `Rscript`).
Reads `../data/authpop_dataset.dta` and `../data/authpop_episodes.csv`.

#### Figure AP6 — Main Synthetic Control (scpi, sims=200)

Uses the `scpi` package (Cattaneo et al.) with L1 simplex constraint,
Gaussian out-of-sample inference, `period.pre = 0:15`, `period.post = 16:30`.
For each episode, the donor pool is all FST countries not themselves taking
over in that year. Averaged across converged episodes; confidence bands are
pointwise 90% simulation intervals. Not all episodes converge: pre-WWII
episodes commonly fail due to sparse pre-treatment GDP data; episodes near
the panel end fail due to insufficient post-treatment observations.

#### Figure AP7 — SCM Robustness (sims=10)

Identical setup to AP6 but `sims=10`. Confidence bands are wider; the central
path is the same. Purpose: verify results are not sensitive to simulation count.

#### Figure AP8 — Augmented Synthetic Control (augsynth)

Uses the `augsynth` package (Ben-Michael et al. 2021). The augmented estimator
adds a bias-correction term to the classic Abadie weights. Pre-treatment window
shortened to `0:10` (10 years, vs. 15 in AP6) to allow more episodes to
converge — the strict subset has episodes as early as 1946, requiring data
back to 1931 for a 15-period window; some countries lack complete GDP data
that far back.

#### Figure AP9 — Penalized Synthetic Control (LowRankQP)

Penalized SCM via the `LowRankQP` package. Three panels per subset:
λ=0 (standard SCM), λ=0.1, and λ=optimal (leave-one-out CV over a grid from
0 to 5). Higher λ penalizes donor weight concentration. Optimal λ minimizes
pre-period fit error.

#### Figure AP10 — Pooled Multisynth (augsynth::multisynth)

`multisynth()` pools all episodes in the subset into a single pseudo-panel
and estimates a joint synthetic control with staggered treatment. Exploits
cross-episode variation jointly; reduces the influence of any single episode.

#### Figures AP11–AP14 — Alternative Outcome Variables

Same scpi SCM procedure applied to non-GDP outcomes. Episode counts vary
by variable due to data availability:

| Figure | Outcomes | Notes |
|--------|----------|-------|
| AP11 | Gini index, labor share | Sparse for pre-1970 episodes; strict subset will have low N |
| AP12 | KOF trade index, trade/GDP, KOF globalisation | Trade/GDP has the best historical coverage |
| AP13 | Debt/GDP, inflation | Reinhart-Rogoff and IFS data; good coverage from 1940s onward |
| AP14 | V-Dem institutions (`v2x_libdem`) | Complete coverage for all episodes (V-Dem back to 1789) |

See `sample_size_guide.md` for the full accounting of why N varies across
figures and subsets.

---

## How to Run

### Full Pipeline

```bat
run_authpop.bat
```

Run from `C:\PLE\authpop_extension\`. Executes all four steps in order and
exits with code 1 on the first error. Expected runtime: 10–20 minutes for
Stata steps; **3–5 hours for the R step** (SCM is compute-intensive).

### Individual Steps

Each step can be run independently once its inputs exist:

| Step | Command | Requires |
|------|---------|---------|
| 1 | `StataSE-64.exe -e do programs\datprep_authpop.do` | `ple_dataset.dta`, `authpop_episodes.csv` |
| 2a | `StataSE-64.exe -e do programs\figtabs_authpop.do` | `authpop_dataset.dta` |
| 2b | `StataSE-64.exe -e do programs\tableap3_authpop.do` | `authpop_dataset.dta` |
| 3 | `Rscript.exe programs\ranscm_authpop.R` *(from `programs/`)* | `authpop_dataset.dta`, `authpop_episodes.csv` |

> **R must be run from the `programs/` subdirectory.** The `.bat` file handles
> this with `cd programs` before calling `Rscript`. If running manually, do
> the same or edit the relative paths at the top of the script.

### Recompiling the Paper

From `C:\PLE\authpop_extension\`:
```bat
pdflatex paper_authpop.tex
pdflatex paper_authpop.tex
```
Two passes are needed to resolve cross-references. The compiled PDF is
`paper_authpop.pdf`. Requires TinyTeX or a standard TeX distribution.
pdflatex is at `C:\Users\maril\AppData\Roaming\TinyTeX\bin\windows\pdflatex.exe`.

---

## Software Requirements

### Stata (version 19 SE or higher)

Required user-written packages (located in `C:\PLE\adosw\` and `C:\PLE\adosm\`,
set as adopath in the do-files):

| Package | Used in |
|---------|---------|
| `synth` | `tableap3_authpop.do` (predictor balance) |
| `reghdfe` | `figtabs_authpop.do` (FE regressions) |
| `estout` / `esttab` | `figtabs_authpop.do` (Table AP2 output) |
| `grc1leg` | `figtabs_authpop.do` (combined figures) |

### R (version 4.3.0)

> **Use R 4.3.0 specifically** (`C:\Program Files\R\R-4.3.0\bin\x64\Rscript.exe`).
> R 4.5.x does not have compatible `dplyr`/`haven` versions in the lockfile.

Key packages used in `ranscm_authpop.R`:

| Package | Purpose |
|---------|---------|
| `scpi` | Main synthetic control (AP6, AP7, AP11–AP14) |
| `augsynth` | Augmented SCM (AP8) and pooled multisynth (AP10) |
| `LowRankQP` | Penalized SCM solver (AP9) |
| `haven` | Read `.dta` files |
| `readr` | Read episode CSV |
| `ggplot2` | All figures |
| `grid` | Multi-panel PDF layout |
| `dplyr` / `plyr` | Data manipulation |

> **Multi-panel figures use `grid::grid.layout()` + `viewport()`, not
> `patchwork`.** The `patchwork` `+` operator fails silently when plots use
> `scale_linewidth_manual`, writing a corrupt ~3.7 KB PDF with no error
> message. Do not revert to patchwork.

> **plyr/dplyr masking:** In scripts that load both, always use
> `dplyr::summarise()` explicitly inside `group_by()` chains. Loading
> `plyr` after `dplyr` causes `plyr::summarise` to mask `dplyr`'s version,
> silently dropping the grouping variable.

---

## Known Limitations

1. **Small treatment group.** The strict subset has N=9 episodes, the broad
   N=13. Regression estimates lack power; synthetic control CIs are wide.
   All specifications should be read as descriptive rather than causal.

2. **Ecuador concentration.** Three of nine strict episodes are the same
   leader (Velasco Ibarra). The no-Ecuador robustness variants address this
   directly. See the Ecuador discussion in the paper's robustness section.

3. **Right-wing data gap.** In the strict subset, all right-wing episodes are
   Ecuador, and Ecuador's GDP data begins ~1937. This produces visible gaps in
   the doppelganger line at `t = 2..8` for right-wing panels in Figures AP6
   and AP7. This is a data availability constraint from the FST panel itself,
   not an estimation error.

4. **Pre-WWII SCM convergence.** Episodes starting before 1950 require GDP
   data going back to the 1930s. Some donor countries lack this data, causing
   the SCM optimizer to fail. Failed episodes are silently skipped; the figure
   note reports the actual N of converged episodes.

5. **Gini/labor share coverage (AP11).** Historical inequality data is
   largely unavailable before 1960–1970. The strict subset's pre-1970 episodes
   will have very few or zero converged episodes for AP11. The broad subset
   (which includes Fujimori 1990, Chávez 1999) has better coverage.

6. **AP5 IPW overlap.** With only 9 or 13 treated units, propensity score
   overlap is limited. IPW estimates in Figure AP5 are presented for
   completeness but should be interpreted cautiously.

---

## Relationship to the Main PLE Replication and Auth Extension

This folder is fully self-contained. Its only external dependency is
`../data/ple_dataset.dta` — the main FST panel — which it reads but never
modifies.

| Folder | Question | Episodes | Method |
|--------|----------|----------|--------|
| `C:\PLE\` | Replicates FST (2023) exactly | 53 FST populist episodes | FST original code |
| `C:\PLE\auth_extension\` | Do non-populist authoritarians harm growth? | 84 non-populist authoritarian episodes (new GWF-based database) | Mirrors FST methods |
| `C:\PLE\authpop_extension\` | Is FST's populism effect driven by the authoritarian subset? | 9–13 episodes (subset of FST's own 28) | Mirrors FST methods |

---

## Key Findings

The central question is whether the approximately −10 pp GDP effect FST
documents for populists as a whole is concentrated in, or amplified by,
the authoritarian segment of that sample.

**Short answer: No.** The negative GDP effect of populist leaders does not
appear to be uniquely driven by autocratic institutional setting. Authoritarianpopulist episodes show negative growth effects of comparable direction and
magnitude to the FST full-sample benchmark, not larger ones.

**OLS/FE (Table AP2, Figure AP4):** Under the strict definition, GDP growth
is lower by 1.3–1.8 percentage points per year in the five years after
takeover, statistically significant at the 5 percent level. The 15-year
estimate is −1.2 pp and loses significance with country fixed effects,
consistent with FST's own observation that long-horizon within-country
variation is harder to identify.

**Synthetic control (Figures AP6–AP8):** The average post-takeover GDP path
falls below the synthetic control in both strict and broad subsets, mirroring
FST's main results in direction and order of magnitude. Pre-period balance
is good (Table AP3). Results are robust across scpi, augsynth, and penalized
SCM specifications.

**No-Ecuador robustness (Figures AP3–AP14, `_noecuador` variants):**
Dropping Velasco Ibarra's three spells does not qualitatively change the
conclusions in either the strict or broad subset, confirming that the results
are not driven by Ecuador alone.

**Alternative outcomes (Figures AP11–AP14):** Evidence on inequality,
trade, macrofinancial, and institutional outcomes is more limited by data
availability, particularly for the pre-1970 strict episodes. The broad subset
(Fujimori, Chávez, Erdoğan) shows the clearest distributional deterioration
and institutional decline, consistent with those cases' historical records.

The contrast with the `auth_extension/` findings is notable: non-populist
authoritarian leaders show no consistent negative GDP effect, while
authoritarian-populist leaders do. The economic damage appears to derive from
the *populist* component of the pairing, not from authoritarianism per se.
