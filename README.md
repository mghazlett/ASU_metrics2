# ASU_metrics2
ASU ECN 726
Assignment is to replicate tables and figures in: Funke, Manuel, Moritz Schularick, and Christoph Trebesch. 2023. "Populist Leaders and the Economy." American Economic Review 113 (12): 3249–88. DOI: 10.1257/aer.20202045 
To Erhan, here are the files you actually need to run my replication code: 

 ├── runall_inpaper_pkg.bat        ← run this to reproduce everything, EDIT TO MATCH VERSIONS ON YOUR MACHINE
  ├── InPaper_20260320.tex          ← LaTeX source for PDF output                     ├── README.docx                                                                     ├── data/                                                                           │   └── ple_dataset.dta           ← pre-built panel (replaces ~20 raw files +       datprep.do that was in the authors' original replication code)
  ├── programs/
  │   ├── allstata_inpaper_pkg.do   ← master Stata do-file (skips datprep)            │   ├── figtabs_inpaper.do        ← Figures 1-5, Tables 2-3                         │   ├── table1.do                 ← Table 1                                         │   ├── ranscm.R                  ← Figures 6-14 (SCM, 60-90 min)                   │   ├── renv.lock                 ← pinned R packages                               │   ├── renv/activate.R           ← renv bootstrap                                  │   ├── adosw/                    ← Stata ado packages (Windows)                    │   └── adosm/                    ← Stata ado packages (Mac/Linux)                  ├── figures/                      ← output dir (empty, populated on run)            └── tables/                       ← output dir (empty, populated on run)  
