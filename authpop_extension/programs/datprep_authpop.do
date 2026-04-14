/*===========================================================================
  datprep_authpop.do
  Authoritarian-Populist Subset Extension — Data Preparation

  Merges authoritarian-populist classification into ple_dataset.dta to
  create authpop_dataset.dta with treatment flags for two samples:
    auth_strict  = populist spells in electoral/closed autocracies at
                   takeover (V-Dem regime <= 1), N=9
    auth_broad   = strict + populists who became authoritarian during
                   tenure (V-Dem crosses into regime <= 1), N=13
  The no-Ecuador robustness subsets drop ECU episodes at the R/Stata stage.

  Also generates:
    atakeover_strict / atakeover_broad   — =1 at takeover year only
    ltakeover_strict / rtakeover_strict  — left/right wing splits
    ltakeover_broad  / rtakeover_broad
    Post_5_strict  / Post_15_strict      — post-treatment window indicators
    Post_5_broad   / Post_15_broad

  Run from: C:\PLE\authpop_extension\
  (called by run_authpop.bat which sets the working directory)

  Inputs:
    ../data/ple_dataset.dta               — FST master panel (has iso var)
    data/authpop_episodes.csv             — episode classification list

  Outputs:
    data/authpop_dataset.dta              — flagged panel dataset
===========================================================================*/

clear all
set more off
cap mkdir data/_work

* -------------------------------------------------------------------------
* STEP 1: Build episode-level flags
* NOTE: ple_dataset already contains iso; no crosswalk needed.
*       Merge directly on iso x year — auth_strict/broad = 1 at takeover yr.
* -------------------------------------------------------------------------

import delimited using "data/authpop_episodes.csv", varnames(1) clear
keep iso year auth_strict auth_broad left
* 'left' is the left-wing flag from our classification (not in ple_dataset)
rename left authpop_left
tempfile episode_flags
save `episode_flags'

* -------------------------------------------------------------------------
* STEP 2: Merge flags into ple_dataset
* -------------------------------------------------------------------------

use ../data/ple_dataset, clear

* Merge on iso x year — only the 28 CSV rows will match (auth episodes only)
merge m:1 iso year using `episode_flags', keep(master match) nogen ///
    keepusing(auth_strict auth_broad authpop_left)

* Fill non-authpop rows with 0
foreach v of varlist auth_strict auth_broad authpop_left {
    replace `v' = 0 if missing(`v')
}

* -------------------------------------------------------------------------
* STEP 3: Verify episode counts
* -------------------------------------------------------------------------

qui count if auth_strict == 1 & atakeover == 1
di "  auth_strict takeover obs (expect 9):  " r(N)

qui count if auth_broad == 1 & atakeover == 1
di "  auth_broad  takeover obs (expect 13): " r(N)

* -------------------------------------------------------------------------
* STEP 4: Build atakeover indicators and left/right splits
* auth_strict/auth_broad are already 1 ONLY at the takeover year for each
* episode (because the CSV has one row per episode and merged on iso+year).
* -------------------------------------------------------------------------

gen atakeover_strict = (auth_strict == 1 & atakeover == 1)
gen atakeover_broad  = (auth_broad  == 1 & atakeover == 1)

* Left/right splits (authpop_left from our CSV; ltakeover/rtakeover
* in ple_dataset cover all 28 FST episodes — not what we want here)
gen ltakeover_strict = (atakeover_strict == 1 & authpop_left == 1)
gen rtakeover_strict = (atakeover_strict == 1 & authpop_left == 0)
gen ltakeover_broad  = (atakeover_broad  == 1 & authpop_left == 1)
gen rtakeover_broad  = (atakeover_broad  == 1 & authpop_left == 0)

* -------------------------------------------------------------------------
* STEP 5: Post-treatment window indicators
* -------------------------------------------------------------------------

tsset cid year

* Initialise
foreach v in Post_5_strict Post_15_strict Post_5_broad Post_15_broad {
    gen `v' = 0
}

* 5-year window (years 1-5 after takeover)
forvalues h = 1/5 {
    replace Post_5_strict = 1 if L`h'.atakeover_strict == 1
    replace Post_5_broad  = 1 if L`h'.atakeover_broad  == 1
}

* 15-year window (years 1-15 after takeover)
forvalues h = 1/15 {
    replace Post_15_strict = 1 if L`h'.atakeover_strict == 1
    replace Post_15_broad  = 1 if L`h'.atakeover_broad  == 1
}

* -------------------------------------------------------------------------
* STEP 6: Label and save
* -------------------------------------------------------------------------

label var auth_strict       "=1 at takeover: auth-pop strict (autocracy at takeover)"
label var auth_broad        "=1 at takeover: auth-pop broad (also became auth during tenure)"
label var authpop_left      "=1 for left-wing auth-pop episodes (from authpop_episodes.csv)"
label var atakeover_strict  "=1 at takeover year: strict subset (N=9 events)"
label var atakeover_broad   "=1 at takeover year: broad subset (N=13 events)"
label var ltakeover_strict  "=1 at takeover year: strict, left-wing"
label var rtakeover_strict  "=1 at takeover year: strict, right-wing"
label var ltakeover_broad   "=1 at takeover year: broad, left-wing"
label var rtakeover_broad   "=1 at takeover year: broad, right-wing"
label var Post_5_strict     "=1 years 1-5 after any strict authpop takeover"
label var Post_15_strict    "=1 years 1-15 after any strict authpop takeover"
label var Post_5_broad      "=1 years 1-5 after any broad authpop takeover"
label var Post_15_broad     "=1 years 1-15 after any broad authpop takeover"

compress
save "data/authpop_dataset.dta", replace

di "authpop_dataset.dta saved with all treatment flags."
