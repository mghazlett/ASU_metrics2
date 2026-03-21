@echo off

"C:\Program Files\StataNow19\StataSE-64.exe" /e /q do programs\allstata.do
cd programs
"C:\Program Files\R\R-4.5.3\bin\x64\Rscript.exe" -e "renv::load(); source('ranscm.R')"