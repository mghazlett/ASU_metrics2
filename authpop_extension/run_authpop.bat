@echo off
REM ============================================================
REM run_authpop.bat
REM Authoritarian-Populist Subset Extension — Master Runner
REM
REM Steps:
REM   1.  datprep_authpop.do   — builds authpop_dataset.dta (Stata)
REM   2a. figtabs_authpop.do   — Stata figures AP3-AP4, Tables AP1-AP2 (Stata)
REM   2b. tableap3_authpop.do  — TableAP3 synth balance (fresh Stata session)
REM   3.  ranscm_authpop.R     — R SCM figures AP6, AP8-AP9
REM
REM Run from: C:\PLE\authpop_extension\
REM ============================================================

cd /d C:\PLE\authpop_extension
set EXITCODE=0

echo.
echo ============================================================
echo  STEP 1: Stata datprep (builds authpop_dataset.dta)
echo ============================================================
"C:\Program Files\StataNow19\StataSE-64.exe" -e do programs\datprep_authpop.do
if %ERRORLEVEL% NEQ 0 (
    echo ERROR: Stata datprep failed. Check datprep_authpop.log
    set EXITCODE=1
    goto :cleanup
)
echo Stata datprep complete.

echo.
echo ============================================================
echo  STEP 2a: Stata figures and tables (FigAP3-AP4, Tables AP1-AP2)
echo ============================================================
"C:\Program Files\StataNow19\StataSE-64.exe" -e do programs\figtabs_authpop.do
if %ERRORLEVEL% NEQ 0 (
    echo ERROR: Stata figtabs failed. Check figtabs_authpop.log
    set EXITCODE=1
    goto :cleanup
)
echo Stata figtabs complete.

echo.
echo ============================================================
echo  STEP 2b: Stata TableAP3 synth balance (fresh session)
echo ============================================================
"C:\Program Files\StataNow19\StataSE-64.exe" -e do programs\tableap3_authpop.do
if %ERRORLEVEL% NEQ 0 (
    echo ERROR: Stata TableAP3 failed. Check tableap3_authpop.log
    set EXITCODE=1
    goto :cleanup
)
echo Stata TableAP3 complete.

echo.
echo ============================================================
echo  STEP 3: R SCM estimation (Figures AP6, AP8-AP9)
echo ============================================================
cd programs
"C:\Program Files\R\R-4.3.0\bin\x64\Rscript.exe" ranscm_authpop.R
if %ERRORLEVEL% NEQ 0 (
    cd ..
    echo ERROR: R SCM script failed.
    set EXITCODE=1
    goto :cleanup
)
cd ..
echo R SCM complete.

:cleanup
echo.
echo ============================================================
echo  CLEANUP: removing logs and temporary files
echo ============================================================

REM Stata logs (dropped in root by -e flag)
if exist datprep_authpop.log     del /f /q datprep_authpop.log
if exist figtabs_authpop.log     del /f /q figtabs_authpop.log
if exist tableap3_authpop.log    del /f /q tableap3_authpop.log

REM R session log
if exist run_authpop_session.log del /f /q run_authpop_session.log
if exist programs\ranscm_authpop_latest.log del /f /q programs\ranscm_authpop_latest.log

REMStray _ap3_*.dta in root (written there if tmp not set; belt-and-suspenders)
for %%f in (_ap3_*.dta) do del /f /q "%%f"

REM Stata working files in data\_work\
for %%f in (data\_work\_ap3_*.dta)       do del /f /q "%%f"
for %%f in (data\_work\_authpop_*.dta)   do del /f /q "%%f"

REM R intermediate CSVs
for %%f in (data\scm_results_*.csv) do del /f /q "%%f"

echo Cleanup complete.

if %EXITCODE% NEQ 0 (
    echo.
    echo ============================================================
    echo  Pipeline exited with errors. Check logs above.
    echo ============================================================
    exit /b 1
)

echo.
echo ============================================================
echo  Done. Outputs saved to figures\ and tables\
echo.
echo  Figures:
echo    figures\FigureAP3_{strict,broad,nonauthpop,full}.pdf          (growth gap)
echo    figures\FigureAP3_{strict,broad}_noecuador.pdf                (no-Ecuador)
echo    figures\FigureAP4_{strict,broad}.pdf                          (local projections)
echo    figures\FigureAP6_{strict,broad,nonauthpop,fst_full}.pdf      (main SCM)
echo    figures\FigureAP8_{strict,broad,strict_noecuador,broad_noecuador}.pdf  (5-yr SCM)
echo    figures\FigureAP9.pdf                                          (doppelganger gap)
echo    figures\FigureAP9_noecuador.pdf                               (doppelganger gap, no-Ecuador)
echo.
echo  Tables:
echo    tables\Table2_combined.tex   (OLS/FE: 4 samples x 3 specs)
echo    tables\TableAP1_full.tex     (full episode list, N=28)
echo ============================================================
exit /b 0
