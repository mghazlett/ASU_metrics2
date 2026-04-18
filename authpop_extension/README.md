# Authoritarian Populist Leaders and the Economy
### A Subset Extension of Funke, Schularick, and Trebesch (2023)
Working directory: C:\PLE\authpop_extension

---

## 1. What This Extension Does

This project asks whether the negative GDP effects documented by Funke, Schularick, and Trebesch (2023) for populist leaders are concentrated in the authoritarian segment of their sample. No new episodes are coded. FST's 28 data-complete populist episodes are reclassified using V-Dem's `v2x_regime` into a *strict* authoritarian subset (N=9, autocracy at takeover), a *broad* subset (N=14, adds leaders who dismantled democratic institutions during tenure), a non-authoritarian-populist complement (N=15), and the FST full sample (N=28) as a benchmark. FST's OLS/FE and synthetic control methods are replicated on all four groups.

---

## 2. Folder Structure

| Folder / File | Contents |
|---|---|
| `data/` | Input datasets. `authpop_episodes.csv` is the canonical episode list with classification flags; `authpop_dataset.dta` is the merged panel output of `datprep_authpop.do`. |
| `figures/` | All output figures (PDF). See Section 6 for descriptions. |
| `tables/` | Output tables (LaTeX .tex). `Table2_combined.tex` is the main regression table; `TableAP1_full.tex` is the full episode list. |
| `programs/` | All Stata do-files and the R script. See Section 4. |
| `run_authpop.bat` | One-click pipeline: runs all steps and compiles the PDF. |
| `FST-Online-Appendix.pdf` | FST (2023) online appendix, for reference. |
| `sample_size_guide.md` | Explanation of why N varies across figures and subsets. |

---

## 3. Software Requirements

| Software | Version | Path in .bat |
|---|---|---|
| Stata/SE | 19 | `C:\Program Files\StataNow19\StataSE-64.exe` |
| R | 4.3.0 | `C:\Program Files\R\R-4.3.0\bin\x64\Rscript.exe` |
| pdflatex | TinyTeX | `C:\Users\maril\AppData\Roaming\TinyTeX\bin\windows\pdflatex.exe` |

Required Stata packages (installed in `C:\PLE\adosw\` and `C:\PLE\adosm\`): `synth`, `reghdfe`, `estout`, `grc1leg`.

Required R packages: `scpi`, `haven`, `readr`, `ggplot2`, `grid`, `dplyr`, `plyr`.

> **Use R 4.3.0 specifically.** R 4.5.x does not have compatible package versions in the renv lockfile.

---

## 4. Pipeline Overview

The pipeline runs in three sequential steps, all orchestrated by `run_authpop.bat`.

**Step 1 — Stata: Data preparation (`datprep_authpop.do`)**
Reads `authpop_episodes.csv` and `../data/ple_dataset.dta`. Merges classification flags into the FST panel and generates treatment indicator variables (`atakeover_strict`, `atakeover_broad`, `Post_5_*`, `Post_15_*`) for all four groups. Output: `data/authpop_dataset.dta`.

**Step 2a — Stata: Figures and tables (`figtabs_authpop.do`)**
Produces the growth gap figures (AP3), local projection event studies (AP4), all four individual regression tables (AP2), and the combined 12-column regression table (Table 2). Also produces `TableAP1_full.tex`.

**Step 2b — Stata: Predictor balance (`tableap3_authpop.do`)**
Run as a separate Stata session to avoid MATA library conflicts with Step 2a. Runs `synth` for each episode in the strict subset and produces a predictor balance table. Output is not currently included in the paper.

**Step 3 — R: Synthetic control estimation (`ranscm_authpop.R`)**
Run from `programs/`. Produces the main SCM figures (AP6), the 5-year window SCM (AP8), and the average doppelganger gap figure (AP9 / Figure 2). Runtime: approximately 3–5 hours.

**Step 4 — pdflatex: Compile paper**
`pdflatex` is run twice on `paper_authpop.tex` to resolve cross-references.

---

## 5. How to Run

**Full pipeline**
```
C:\PLE\authpop_extension\run_authpop.bat
```

**Running steps individually**

To run only the Stata data prep step:
```
"C:\Program Files\StataNow19\StataSE-64.exe" /e /q do programs\datprep_authpop.do
```

To run only the R step (from `C:\PLE\authpop_extension\programs\`):
```
"C:\Program Files\R\R-4.3.0\bin\x64\Rscript.exe" ranscm_authpop.R
```

To compile LaTeX only:
```
pdflatex -interaction=nonstopmode paper_authpop.tex
pdflatex -interaction=nonstopmode paper_authpop.tex
```

---

## 6. Output Files Reference

| File | Method | Description |
|---|---|---|
| `figures/FigureAP3_strict.pdf` | Stata | Growth gap bar chart, strict subset (N=9) |
| `figures/FigureAP3_broad.pdf` | Stata | Growth gap bar chart, broad subset (N=14) |
| `figures/FigureAP3_nonauthpop.pdf` | Stata | Growth gap bar chart, non-auth-pop (N=15) |
| `figures/FigureAP3_full.pdf` | Stata | Growth gap bar chart, FST full sample (N=28) |
| `figures/FigureAP3_strict_noecuador.pdf` | Stata | Growth gap, strict excl. Ecuador (N=6) |
| `figures/FigureAP3_broad_noecuador.pdf` | Stata | Growth gap, broad excl. Ecuador (N=11) |
| `figures/FigureAP4_strict.pdf` | Stata | Local projection event study, strict (N=9) |
| `figures/FigureAP4_broad.pdf` | Stata | Local projection event study, broad (N=14) |
| `figures/FigureAP6_strict.pdf` | R | Synthetic control GDP path, strict (N=9) |
| `figures/FigureAP6_broad.pdf` | R | Synthetic control GDP path, broad (N=14) |
| `figures/FigureAP6_nonauthpop.pdf` | R | Synthetic control GDP path, non-auth-pop (N=15) |
| `figures/FigureAP6_fst_full.pdf` | R | Synthetic control GDP path, FST full (N=28) |
| `figures/FigureAP8_strict.pdf` | R | SCM, 5-year window, strict (N=9) |
| `figures/FigureAP8_broad.pdf` | R | SCM, 5-year window, broad (N=14) |
| `figures/FigureAP8_strict_noecuador.pdf` | R | SCM, 5-year window, strict excl. Ecuador (N=6) |
| `figures/FigureAP8_broad_noecuador.pdf` | R | SCM, 5-year window, broad excl. Ecuador (N=11) |
| `figures/FigureAP9.pdf` | R | Average doppelganger gap — all 4 groups |
| `figures/FigureAP9_noecuador.pdf` | R | Average doppelganger gap, no-Ecuador robustness |
| `tables/Table2_combined.tex` | Stata | OLS/FE regression table: 4 samples × 3 specs (12 columns) |
| `tables/TableAP1_full.tex` | Stata | Full FST episode list (N=28) with authpop classification flags |

---

## Reference

Funke, Manuel, Moritz Schularick, and Christoph Trebesch. 2023. "Populist Leaders and the Economy." *American Economic Review* 113 (12): 3249–3288.
