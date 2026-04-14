
* Main replication program for
* "POPULIST LEADERS AND THE ECONOMY"
* by  Manuel Funke, Moritz Schularick and Christoph Trebesch

* Run in Stata/MP v 17.0
* June 2023

clear all

local os : di c(os)
display "`os'"

if"`c(os)'"=="Windows"{
cd
local current_dir : pwd
display "`current_dir'"
cd
adopath
adopath ++ "`current_dir'\programs\adosw/" 
adopath 

}
else{
cap adopath ++ programs/adosm
}
mata: mata mlib index

qui do programs/datprep
qui do programs/figtabs

cd  programs

clear all
