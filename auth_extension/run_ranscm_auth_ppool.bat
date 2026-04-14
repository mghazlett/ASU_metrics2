@echo off
cd /d "%~dp0"
SET RSCRIPT="C:\Program Files\R\R-4.3.0\bin\x64\Rscript.exe"
echo Running ranscm_auth_ppool.R (Figure A_10, ~1 hour)...
cd programs
%RSCRIPT% ranscm_auth_ppool.R
IF ERRORLEVEL 1 (
    echo ERROR: ranscm_auth_ppool.R failed.
    cd ..
    exit /b 1
)
cd ..
echo Done. FigureA_10 saved to figures\
