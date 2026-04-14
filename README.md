# ASU Metrics II — PLE Replication & Extensions

Replication and extensions of Funke, Schularick & Trebesch (2023),
"Populist Leaders and the Economy," *American Economic Review* 113(12).

## Repository Structure

```
ASU_metrics2/
├── EBSCO-FullText-03_18_2026.pdf   FST (2023) paper
├── InPaper_20260320.pdf            Compiled in-paper figures and tables
├── runall_inpaper_20260320.bat     Master pipeline runner
│
├── data/                           Source and processed data
├── figures/                        FST replication figures (1-14, A-C appendix)
├── tables/                         FST replication tables (1-3, B, C appendix)
│
├── programs/
│   ├── datprep.do                  Builds ple_dataset.dta from raw sources
│   ├── allstata_inpaper.do         Master Stata: datprep + in-paper figures
│   ├── figtabs_inpaper.do          In-paper figures and tables
│   ├── table1.do                   Table 1 (added; not in original FST code)
│   ├── ranscm.R                    Synthetic control (Figures 6-14)
│   ├── renv.lock                   R package lockfile
│   ├── adosw/                      Stata ado packages (Windows)
│   └── adosm/                      Stata ado packages (Mac)
│
├── auth_extension/                 Extension: non-populist authoritarian leaders
│   ├── README.md
│   ├── data/                       Episode CSVs + auth_dataset.dta
│   ├── figures/                    Figures A_3 through A_14
│   ├── tables/                     Tables A_C1, A_C3
│   └── programs/                   build_auth_episodes.R, datprep/figtabs/ranscm scripts
│
└── authpop_extension/              Extension: authoritarian-populist subset
    ├── README.md
    ├── sample_size_guide.md
    ├── data/                       authpop_episodes.csv + authpop_dataset.dta
    ├── figures/                    Figures AP1-AP14 (4 variants each)
    ├── tables/                     Tables AP1-AP3 (strict + broad)
    └── programs/                   datprep/figtabs/tableap3/ranscm scripts
```

## What FST Found

Populist leaders reduce GDP per capita by ~10 percentage points relative to a
synthetic control after 15 years. The effect holds for both left- and right-wing
populists across 53 episodes, 1900-present.

## Extensions

**auth_extension:** Do non-populist authoritarian leaders cause similar damage?
Answer: No — near-zero or modest negative GDP effects, substantially smaller than
the FST populism finding. Personalist regimes drive what little negative signal exists.

**authpop_extension:** Is FST's effect concentrated in the authoritarian segment of
their own sample? Answer: No — authoritarian-populist episodes show comparable negative
effects to the FST full-sample benchmark, not larger ones. The economic damage is in
the populist policy package, not the autocratic institutional setting.

## Software

- Stata/SE 19: `C:\Program Files\StataNow19\StataSE-64.exe`
- R 4.3.0: `C:\Program Files\R\R-4.3.0\bin\x64\Rscript.exe`
- pdflatex (TinyTeX): `C:\Users\maril\AppData\Roaming\TinyTeX\bin\windows\pdflatex.exe`
