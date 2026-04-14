@echo off
REM ============================================================
REM  run_all_auth.bat
REM  Authoritarian Leaders Extension — Full Pipeline
REM
REM  Steps:
REM   1. R  - build_auth_episodes.R  (refine raw episode list)
REM   2. R  - check_auth_data_coverage.R (filter to SCM-viable)
REM   3. Stata - datprep_auth.do     (build auth_dataset.dta)
REM   4. Stata - figtabs_auth.do     (Figures A_3 and A_4)
REM
REM  Run this file from C:\PLE\auth_extension\
REM  Adjust the SET lines for your machine.
REM ============================================================

cd /d "%~dp0"

SET STATA="C:\Program Files\StataNow19\StataSE-64.exe"
SET RSCRIPT="C:\Program Files\R\R-4.3.0\bin\x64\Rscript.exe"

REM ---- Step 1: Build refined episode list ----
echo.
echo [1/4] R: Refining authoritarian episode list...
%RSCRIPT% programs\build_auth_episodes.R
IF ERRORLEVEL 1 (
    echo ERROR: build_auth_episodes.R failed.
    exit /b 1
)
echo     Done.

REM ---- Step 2: Filter to SCM-viable episodes ----
echo.
echo [2/4] R: Checking data coverage (SCM filter)...
%RSCRIPT% programs\check_auth_data_coverage.R
IF ERRORLEVEL 1 (
    echo ERROR: check_auth_data_coverage.R failed.
    exit /b 1
)
echo     Done.

REM ---- Step 3: Build auth_dataset.dta ----
echo.
echo [3/4] Stata: Building auth_dataset.dta...
%STATA% /e /q do programs\datprep_auth.do
IF ERRORLEVEL 1 (
    echo ERROR: datprep_auth.do failed. Check logs\datprep_auth.log
    exit /b 1
)
IF EXIST programs\datprep_auth.log move /Y programs\datprep_auth.log logs\ > nul
echo     Done.

REM ---- Step 4: Produce figures ----
echo.
echo [4/4] Stata: Producing Figures A_3 and A_4...
%STATA% /e /q do programs\figtabs_auth.do
IF ERRORLEVEL 1 (
    echo ERROR: figtabs_auth.do failed. Check logs\figtabs_auth.log
    exit /b 1
)
IF EXIST programs\figtabs_auth.log move /Y programs\figtabs_auth.log logs\ > nul
echo     Done.

echo.
echo ============================================================
echo  Auth extension complete.
echo    figures\FigureA_3.pdf
echo    figures\FigureA_4.pdf
echo    data\auth_dataset.dta
echo ============================================================
echo.
