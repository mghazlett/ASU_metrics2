/* Quick verification — extract growth gap values for Figure A_3 */
clear all
set more off

use data/auth_dataset, clear

preserve
keep if atakeover_auth == 1 & auth_is_populist == 0
sort auth_spell_id
local N = _N
di "Spells: `N'"
forvalues i = 1/`N' {
    local treatedcid_`i' = cid[`i']
    local treatedyea_`i' = year[`i']
}
restore

tsset cid year
gen lrgdppc   = log(fstgdp)
gen rgdppc_gr = (lrgdppc - l1.lrgdppc) * 100
gen at = 0

forvalues i = 1/`N' {

    * 15-year, country-level
    preserve
    keep if cid == `treatedcid_`i''
    replace at = 1 if year == `treatedyea_`i''
    forvalues h = 1/15 {
        gen at`h' = (l`h'.at)
    }
    gen atd = at1+at2+at3+at4+at5+at6+at7+at8+at9+at10+at11+at12+at13+at14+at15
    egen meanrgdppc_gr = mean(rgdppc_gr)
    gen ggap = rgdppc_gr - meanrgdppc_gr if atd == 1
    keep if atd == 1
    cap save _vta_`i'_c15, replace
    restore

    * 15-year, global-level
    preserve
    replace at = 1 if year == `treatedyea_`i''
    forvalues h = 1/15 {
        gen at`h' = (l`h'.at)
    }
    gen atd = at1+at2+at3+at4+at5+at6+at7+at8+at9+at10+at11+at12+at13+at14+at15
    drop if atd != 1
    replace atd = 0
    replace atd = 1 if cid == `treatedcid_`i''
    bys year: egen meanrgdppc_gr = mean(rgdppc_gr)
    gen ggap = rgdppc_gr - meanrgdppc_gr if atd == 1
    keep if atd == 1
    cap save _vta_`i'_g15, replace
    restore

    * 5-year, country-level
    preserve
    keep if cid == `treatedcid_`i''
    replace at = 1 if year == `treatedyea_`i''
    forvalues h = 1/5 {
        gen at`h' = (l`h'.at)
    }
    gen atd = at1+at2+at3+at4+at5
    egen meanrgdppc_gr = mean(rgdppc_gr)
    gen ggap = rgdppc_gr - meanrgdppc_gr if atd == 1
    keep if atd == 1
    cap save _vta_`i'_c5, replace
    restore

    * 5-year, global-level
    preserve
    replace at = 1 if year == `treatedyea_`i''
    forvalues h = 1/5 {
        gen at`h' = (l`h'.at)
    }
    gen atd = at1+at2+at3+at4+at5
    drop if atd != 1
    replace atd = 0
    replace atd = 1 if cid == `treatedcid_`i''
    bys year: egen meanrgdppc_gr = mean(rgdppc_gr)
    gen ggap = rgdppc_gr - meanrgdppc_gr if atd == 1
    keep if atd == 1
    cap save _vta_`i'_g5, replace
    restore
}

* Collapse all specs
foreach s in c5 c15 g5 g15 {
    cap use _vta_1_`s', clear
    forvalues i = 2/`N' {
        cap append using _vta_`i'_`s'
    }
    egen mggap = mean(ggap)
    collapse mggap
    gen spec = "`s'"
    save _vxa_`s', replace
}

use _vxa_c5, clear
append using _vxa_c15
append using _vxa_g5
append using _vxa_g15

di ""
di "=== Figure A_3: Average Growth Gap (annualized pp/year) ==="
di "spec | mean growth gap"
list spec mggap

di ""
di "Compare to FST Table / Figure 3 for populists:"
di "  Country benchmark 5yr:  approx -0.4 to -0.5 pp"
di "  Country benchmark 15yr: approx -0.6 to -0.8 pp"
di "  Global benchmark 5yr:   approx -0.8 to -1.0 pp"
di "  Global benchmark 15yr:  approx -1.0 to -1.2 pp"

* Clean up
forvalues i = 1/200 {
    foreach s in c5 c15 g5 g15 {
        cap erase _vta_`i'_`s'.dta
    }
}
foreach s in c5 c15 g5 g15 {
    cap erase _vxa_`s'.dta
}
