@echo off
cd /d "%~dp0"
SET STATA="C:\Program Files\StataNow19\StataSE-64.exe"
%STATA% /e /q do programs\datprep_auth.do
IF ERRORLEVEL 1 (
    echo ERROR: datprep_auth.do failed.
    exit /b 1
)
IF EXIST programs\datprep_auth.log move /Y programs\datprep_auth.log logs\
echo Done. auth_dataset.dta saved to data\
