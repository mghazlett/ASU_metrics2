@echo off
REM ============================================================
REM  runall_win_20260320.bat
REM  Full replication pipeline for:
REM  Funke, Schularick, and Trebesch (2023)
REM  "Populist Leaders and the Economy"
REM  American Economic Review 113(12): 3249-3288
REM
REM  Steps:
REM   1. Stata  - data cleaning, figure/table generation, Table 1
REM   2. R      - synthetic control figures (Figures 6-14)
REM   3. LaTeX  - compile AllFiguresRaw.pdf
REM   4. LaTeX  - compile AllTablesRaw.pdf (includes Table 1)
REM   5. LaTeX  - compile ReplicationOutput_20260320.pdf
REM              (all figures + tables with paper titles)
REM
REM  Requirements:
REM   - Stata/SE or MP 17+  at path below
REM   - R 4.5.3              at path below
REM   - TinyTeX (pdflatex)   at path below
REM ============================================================

SET STATA="C:\Program Files\StataNow19\StataSE-64.exe"
SET RSCRIPT="C:\Program Files\R\R-4.5.3\bin\x64\Rscript.exe"
SET PDFLATEX="C:\Users\maril\AppData\Roaming\TinyTeX\bin\windows\pdflatex.exe"

REM ---- Step 1: Stata (data prep + all figures/tables + Table 1) ----
echo.
echo [1/5] Running Stata: data preparation, figures, tables, Table 1...
%STATA% /e /q do programs\allstata.do
IF ERRORLEVEL 1 (
    echo ERROR: Stata failed. Check programs\allstata.log for details.
    exit /b 1
)
echo     Stata done.

REM ---- Step 2: R (synthetic control figures 6-14) ----
echo.
echo [2/5] Running R: synthetic control estimation (Figures 6-14)...
cd programs
%RSCRIPT% -e "renv::load(); source('ranscm.R')"
IF ERRORLEVEL 1 (
    echo ERROR: R script failed. Check for error messages above.
    cd ..
    exit /b 1
)
cd ..
echo     R done.

REM ---- Step 3: Compile AllFiguresRaw.pdf ----
echo.
echo [3/5] Compiling AllFiguresRaw.pdf...
cd figures
%PDFLATEX% -interaction=nonstopmode AllFiguresRaw.tex > nul 2>&1
IF ERRORLEVEL 1 (
    echo WARNING: First pdflatex pass for AllFiguresRaw had issues. Retrying...
)
%PDFLATEX% -interaction=nonstopmode AllFiguresRaw.tex > nul 2>&1
cd ..
echo     AllFiguresRaw.pdf done.

REM ---- Step 4: Compile AllTablesRaw.pdf (includes Table 1) ----
echo.
echo [4/5] Compiling AllTablesRaw.pdf...
cd tables
%PDFLATEX% -interaction=nonstopmode AllTablesRaw.tex > nul 2>&1
IF ERRORLEVEL 1 (
    echo WARNING: First pdflatex pass for AllTablesRaw had issues. Retrying...
)
%PDFLATEX% -interaction=nonstopmode AllTablesRaw.tex > nul 2>&1
cd ..
echo     AllTablesRaw.pdf done.

REM ---- Step 5: Compile comprehensive ReplicationOutput PDF ----
echo.
echo [5/5] Compiling ReplicationOutput_20260320.pdf...
REM Two passes needed for TOC and longtable cross-references
%PDFLATEX% -interaction=nonstopmode ReplicationOutput_20260320.tex > nul 2>&1
%PDFLATEX% -interaction=nonstopmode ReplicationOutput_20260320.tex > nul 2>&1
echo     ReplicationOutput_20260320.pdf done.

REM ---- Summary ----
echo.
echo ============================================================
echo  Pipeline complete. Output files:
echo    figures\AllFiguresRaw.pdf    - all reproduced figures
echo    tables\AllTablesRaw.pdf      - all reproduced tables
echo    ReplicationOutput_20260320.pdf - combined output with
echo                                    paper titles
echo ============================================================
echo.
