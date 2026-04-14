@echo off
cd /d "%~dp0"
SET RSCRIPT="C:\Program Files\R\R-4.3.0\bin\x64\Rscript.exe"
echo Running ranscm_auth_outcomes.R (Figures A_11-A_14, several hours)...
cd programs
%RSCRIPT% ranscm_auth_outcomes.R
IF ERRORLEVEL 1 (
    echo ERROR: ranscm_auth_outcomes.R failed.
    cd ..
    exit /b 1
)
cd ..
echo Done. Figures A_11-A_14 saved to figures\
