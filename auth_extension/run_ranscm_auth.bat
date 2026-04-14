@echo off
REM ============================================================
REM  run_ranscm_auth.bat
REM  Runs ranscm_auth.R to produce Figures A_6 and A_7 (SCM)
REM  Expected runtime: 3-6 hours (84 episodes x sims=200)
REM ============================================================
cd /d "%~dp0"
SET RSCRIPT="C:\Program Files\R\R-4.3.0\bin\x64\Rscript.exe"

echo Running ranscm_auth.R (this will take several hours)...
cd programs
%RSCRIPT% ranscm_auth.R
IF ERRORLEVEL 1 (
    echo ERROR: ranscm_auth.R failed.
    cd ..
    exit /b 1
)
cd ..
echo Done. Figures saved to figures\
