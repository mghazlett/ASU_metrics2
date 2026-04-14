@echo off
cd /d "%~dp0"
SET RSCRIPT="C:\Program Files\R\R-4.3.0\bin\x64\Rscript.exe"
echo Running ranscm_auth_pen.R (Figure A_9, ~1-2 hours)...
cd programs
%RSCRIPT% ranscm_auth_pen.R
IF ERRORLEVEL 1 (
    echo ERROR: ranscm_auth_pen.R failed.
    cd ..
    exit /b 1
)
cd ..
echo Done. FigureA_9 saved to figures\
