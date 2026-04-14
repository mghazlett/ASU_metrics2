@echo off
cd /d "%~dp0"
SET RSCRIPT="C:\Program Files\R\R-4.3.0\bin\x64\Rscript.exe"
echo Running ranscm_auth_outcomes_cont.R (Figures A_13 + A_14, ~2-3 hours)...
cd programs
%RSCRIPT% ranscm_auth_outcomes_cont.R
IF ERRORLEVEL 1 (
    echo ERROR: ranscm_auth_outcomes_cont.R failed.
    cd ..
    exit /b 1
)
cd ..
echo Done. FigureA_13 and FigureA_14 saved to figures\
