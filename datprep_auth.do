/*===========================================================================
  datprep_auth.do
  Authoritarian Leaders Extension — Data Preparation

  Merges authoritarian episode list into ple_dataset.dta and creates
  treatment indicators analogous to those used in the FST populism paper.

  Input:  data/ple_dataset.dta
          data/authoritarian_episodes_scm_viable.csv
  Output: data/auth_dataset.dta

  Treatment variables created:
    atakeover_auth = 1 in the year the authoritarian leader takes power
    auth           = 1 while authoritarian leader is in power
    auth_spell_id  = numeric spell identifier (1 per leader episode)
    auth_yrs       = years since takeover (0 = takeover year)
    Post_5_auth    = 1 in years 0-4 after takeover
    Post_15_auth   = 1 in years 0-14 after takeover
    auth_type_str  = authoritarian type label
    auth_is_populist = 1 if spell also in FST populism paper
===========================================================================*/

clear all
set more off

* -------------------------------------------------------------------------
* STEP 1: Build a country-year treatment dataset from episode list
* -------------------------------------------------------------------------

* Import the episode list
import delimited using "data/authoritarian_episodes_scm_viable.csv", ///
    varnames(1) clear

* Rename variables
rename is_also_populist auth_is_populist

* Add a numeric spell ID
gen spell_id = _n
local total_spells = _N
di "Total authoritarian spells: `total_spells'"

* Expand to cover: start_yr through start_yr+15 for each episode
* (we need both in-power years and post-window years for local projections)
gen window_end = max(end_yr, start_yr + 15)
gen window_len = window_end - start_yr + 1
expand window_len

* Generate year within each spell
bysort spell_id: gen yr_offset = _n - 1
gen year = start_yr + yr_offset

* Create treatment indicators
gen atakeover_auth = (year == start_yr)
gen auth           = (year >= start_yr & year <= end_yr)
gen auth_yrs       = year - start_yr
gen Post_5_auth    = (auth_yrs >= 0 & auth_yrs <= 4)
gen Post_15_auth   = (auth_yrs >= 0 & auth_yrs <= 14)

* Keep only years in window
keep if year >= start_yr & year <= window_end

* Rename iso for merge
* (already named iso from the CSV)

* Keep only variables needed for merge
keep iso year spell_id atakeover_auth auth auth_yrs ///
     Post_5_auth Post_15_auth auth_type auth_is_populist leader

rename auth_type     auth_type_str
rename leader        auth_leader_name
rename spell_id      auth_spell_id

* Collapse to unique (iso, year): if a country has two overlapping spells
* in the same year (successive leaders), take the most recent one (max spell_id)
* In practice this should not happen given how we constructed the list.
bysort iso year (auth_spell_id): gen dup_n = _N
* Flag: if same iso-year appears from multiple spells, keep the last spell
bysort iso year: keep if _n == _N

* Save treatment country-year file
tempfile auth_treatment
save `auth_treatment'

* -------------------------------------------------------------------------
* STEP 2: Load PLE dataset and merge treatment indicators
* -------------------------------------------------------------------------
use data/ple_dataset, clear

* Merge on iso-year (left join: keep all PLE observations)
merge 1:1 iso year using `auth_treatment', ///
    keep(master match) ///
    keepusing(atakeover_auth auth auth_yrs Post_5_auth Post_15_auth ///
              auth_type_str auth_leader_name auth_spell_id auth_is_populist)

drop _merge

* Fill zeros for non-treatment observations
foreach v of varlist atakeover_auth auth Post_5_auth Post_15_auth {
    replace `v' = 0 if missing(`v')
}
* auth_is_populist: handle both numeric (0/1) and string ("0"/"1"/"true"/"false")
cap confirm numeric variable auth_is_populist
if _rc {
    * It is a string — convert to numeric
    gen auth_is_populist_num = (auth_is_populist == "1" | ///
        lower(auth_is_populist) == "true")
    drop auth_is_populist
    rename auth_is_populist_num auth_is_populist
}
replace auth_is_populist = 0 if missing(auth_is_populist)

* -------------------------------------------------------------------------
* STEP 3: Label variables
* -------------------------------------------------------------------------
label var atakeover_auth    "=1 in year authoritarian leader takes power"
label var auth              "=1 while authoritarian leader in power"
label var auth_yrs          "Years since authoritarian takeover (0=takeover yr)"
label var Post_5_auth       "=1 in years 0-4 after authoritarian takeover"
label var Post_15_auth      "=1 in years 0-14 after authoritarian takeover"
label var auth_type_str     "Authoritarian regime type"
label var auth_leader_name  "Name of authoritarian leader"
label var auth_spell_id     "Authoritarian spell identifier"
label var auth_is_populist  "=1 if episode also in FST populism paper"

* -------------------------------------------------------------------------
* STEP 4: Quick verification
* -------------------------------------------------------------------------
di as text ""
di as text "=== auth_dataset.dta summary ==="
di as text "Panel: " _N " observations"
count if atakeover_auth == 1
di as text "Authoritarian takeover events: " r(N)
count if auth == 1
di as text "Country-years under auth rule: " r(N)

di as text ""
di as text "--- Takeovers by authoritarian type ---"
tab auth_type_str if atakeover_auth == 1, sort

di as text ""
di as text "--- Country-years by auth type (in-power only) ---"
tab auth_type_str if auth == 1, sort

* -------------------------------------------------------------------------
* STEP 5: Save
* -------------------------------------------------------------------------
save data/auth_dataset.dta, replace
di as text "Saved: data/auth_dataset.dta"
