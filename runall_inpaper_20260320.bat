@echo off
REM ============================================================
REM  runall_inpaper_20260320.bat
REM  In-paper-only replication pipeline for:
REM  Funke, Schularick, and Trebesch (2023)
REM  "Populist Leaders and the Economy"
REM
REM  Produces: Tables 1-3 and Figures 1-14 only.
REM  Skips all appendix items (anything with a letter in name).
REM  Skips TableC6 (multi-hour synthetic control run).
REM
REM  Steps:
REM   1. Stata  - datprep + figtabs_inpaper (Figs 1-5, Tables 2-3)
REM              + table1 (Table 1)
REM   2. R      - synthetic control (Figures 6-14)
REM   3. LaTeX  - compile InPaper_20260320.pdf
REM
REM  Requirements:
REM   - Stata/SE or MP 17+  at path below
REM   - R 4.5.3              at path below
REM   - TinyTeX (pdflatex)   at path below
REM ============================================================

SET STATA="C:\Program Files\StataNow19\StataSE-64.exe"
SET RSCRIPT="C:\Program Files\R\R-4.5.3\bin\x64\Rscript.exe"
SET PDFLATEX="C:\Users\maril\AppData\Roaming\TinyTeX\bin\windows\pdflatex.exe"

REM ---- Step 1: Stata (datprep + in-paper figures/tables + Table 1) ----
echo.
echo [1/3] Running Stata: data prep, Figures 1-5, Tables 1-3...
%STATA% /e /q do programs\allstata_inpaper.do
IF ERRORLEVEL 1 (
    echo ERROR: Stata failed. Check programs\allstata_inpaper.log for details.
    exit /b 1
)
echo     Stata done.

REM ---- Step 2: R (synthetic control, Figures 6-14) ----
echo.
echo [2/3] Running R: synthetic control estimation (Figures 6-14)...
echo       This typically takes 60-90 minutes.
cd programs
%RSCRIPT% -e "renv::load(); source('ranscm.R')"
IF ERRORLEVEL 1 (
    echo ERROR: R script failed. Check for error messages above.
    cd ..
    exit /b 1
)
cd ..
echo     R done.

REM ---- Step 3: Compile InPaper_20260320.pdf ----
echo.
echo [3/3] Compiling InPaper_20260320.pdf...
%PDFLATEX% -interaction=nonstopmode InPaper_20260320.tex > nul 2>&1
%PDFLATEX% -interaction=nonstopmode InPaper_20260320.tex > nul 2>&1
echo     InPaper_20260320.pdf done.

REM ---- Summary ----
echo.
echo ============================================================
echo  Pipeline complete. Output file:
echo    InPaper_20260320.pdf  - Tables 1-3 and Figures 1-14
echo ============================================================
echo.
