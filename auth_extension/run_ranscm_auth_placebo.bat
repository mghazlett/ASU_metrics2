@echo off
cd /d "%~dp0"
SET RSCRIPT="C:\Program Files\R\R-4.3.0\bin\x64\Rscript.exe"
echo Running ranscm_auth_placebo.R (Figure A_8, several hours)...
cd programs
%RSCRIPT% ranscm_auth_placebo.R
IF ERRORLEVEL 1 (
    echo ERROR: ranscm_auth_placebo.R failed.
    cd ..
    exit /b 1
)
cd ..
echo Done. Figure A_8 saved to figures\
