@echo off
REM ============================================================
REM run_authpop.bat
REM Authoritarian-Populist Subset Extension — Master Runner
REM
REM Steps:
REM   1.  datprep_authpop.do   — builds authpop_dataset.dta (Stata)
REM   2a. figtabs_authpop.do   — Stata figures/tables AP1-AP5 (Stata)
REM   2b. tableap3_authpop.do  — TableAP3 synth balance (fresh Stata session)
REM   3.  ranscm_authpop.R     — R SCM figures AP6-AP14 for all 4 subsets
REM
REM Run from: C:\PLE\authpop_extension\
REM ============================================================

cd /d C:\PLE\authpop_extension

echo.
echo ============================================================
echo  STEP 1: Stata datprep (builds authpop_dataset.dta)
echo ============================================================
"C:\Program Files\StataNow19\StataSE-64.exe" -e do programs\datprep_authpop.do
if %ERRORLEVEL% NEQ 0 (
    echo ERROR: Stata datprep failed. Check datprep_authpop.log
    exit /b 1
)
echo Stata datprep complete.

echo.
echo ============================================================
echo  STEP 2a: Stata figures and tables (FigAP3-AP5, Tables AP1-AP2)
echo ============================================================
"C:\Program Files\StataNow19\StataSE-64.exe" -e do programs\figtabs_authpop.do
if %ERRORLEVEL% NEQ 0 (
    echo ERROR: Stata figtabs failed. Check figtabs_authpop.log
    exit /b 1
)
echo Stata figtabs complete.

echo.
echo ============================================================
echo  STEP 2b: Stata TableAP3 synth balance (fresh session)
echo ============================================================
"C:\Program Files\StataNow19\StataSE-64.exe" -e do programs\tableap3_authpop.do
if %ERRORLEVEL% NEQ 0 (
    echo ERROR: Stata TableAP3 failed. Check tableap3_authpop.log
    exit /b 1
)
echo Stata TableAP3 complete.

echo.
echo ============================================================
echo  STEP 3: R SCM estimation (Figures AP6-AP14, all 4 subsets)
echo ============================================================
cd programs
"C:\Program Files\R\R-4.3.0\bin\x64\Rscript.exe" ranscm_authpop.R
if %ERRORLEVEL% NEQ 0 (
    cd ..
    echo ERROR: R SCM script failed.
    exit /b 1
)
cd ..
echo R SCM complete.

echo.
echo ============================================================
echo  Done. Outputs saved to figures\ and tables\
echo.
echo  Figures:
echo    figures\FigureAP3_{strict,broad,strict_noecuador,broad_noecuador}.pdf
echo    figures\FigureAP4_{strict,broad}.pdf
echo    figures\FigureAP5_{strict,broad}.pdf
echo    figures\FigureAP6_{strict,broad,...}.pdf       (AP6, main SCM)
echo    figures\FigureAP7_{strict,broad,...}.pdf       (sims=10 robustness)
echo    figures\FigureAP8_{strict,broad,...}.pdf       (augsynth)
echo    figures\FigureAP9_{strict,broad,...}.pdf       (penalized SCM)
echo    figures\FigureAP10_{strict,broad,...}.pdf      (pooled SCM)
echo    figures\FigureAP11-14_{strict,broad,...}.pdf   (alt outcomes)
echo.
echo  Tables:
echo    tables\TableAP1_{strict,broad}.tex             (episode list)
echo    tables\TableAP2_{strict,broad}.tex             (OLS/FE regression)
echo    tables\TableAP3_{strict,broad}.tex             (predictor balance)
echo ============================================================
