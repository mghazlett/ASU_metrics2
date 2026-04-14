@echo off
cd /d "%~dp0"
SET STATA="C:\Program Files\StataNow19\StataSE-64.exe"
%STATA% /e /q do programs\figtabs_auth.do
IF ERRORLEVEL 1 (
    echo ERROR: figtabs_auth.do failed.
    exit /b 1
)
IF EXIST programs\figtabs_auth.log move /Y programs\figtabs_auth.log logs\
echo Done. Figures saved to figures\
