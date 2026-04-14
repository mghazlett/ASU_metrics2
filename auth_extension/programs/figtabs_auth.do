/*===========================================================================
  figtabs_auth.do
  Authoritarian Leaders Extension — Figures and Tables

  Produces authoritarian analogues of FST (2023) main-text figures.
  Run from: C:\PLE\auth_extension\
  Requires: data/auth_dataset.dta (output of datprep_auth.do)

  Figures produced:
    FigureA_3.pdf  — Average annualized growth gap (analogue of Figure 3)
    FigureA_4.pdf  — GDP local projections (analogue of Figure 4)
===========================================================================*/

clear all
set more off
capture mkdir figures
capture mkdir tables
capture mkdir data/_work

* Add ado packages from main PLE pipeline
if "`c(os)'" == "Windows" {
    adopath ++ "C:\PLE\programs\adosw"
}
else {
    cap adopath ++ "C:/PLE/programs/adosm"
}
mata: mata mlib index

local tmp "data/_work/"

* =========================================================================
* Figure A_3: Average Annualized Growth Gap after Authoritarian Leaders
*             Come to Power (analogue of FST Figure 3)
* =========================================================================

capture {

use data/auth_dataset, clear

* --------------------------------------------------------------------------
* Build the list of authoritarian takeover spells
* Exclude spells that also appear in the FST populism paper (6 episodes)
* --------------------------------------------------------------------------
preserve
keep if atakeover_auth == 1 & auth_is_populist == 0
sort auth_spell_id
local N = _N
di "Authoritarian spells (non-populist): `N'"
forvalues i = 1/`N' {
    local treatedcid_`i' = cid[`i']
    local treatedyea_`i' = year[`i']
}
restore

* Growth rate variable
tsset cid year
gen lrgdppc    = log(fstgdp)
gen rgdppc_gr  = (lrgdppc - l1.lrgdppc) * 100
gen at = 0

* --------------------------------------------------------------------------
* Loop over spells: compute growth gap relative to country mean and
* global mean, for 5-year and 15-year post-treatment windows
* --------------------------------------------------------------------------
forvalues i = 1/`N' {

    * --- 5-year, country-level benchmark ---
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
    cap save `tmp'_ta_`i'_c5, replace
    restore

    * --- 15-year, country-level benchmark ---
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
    cap save `tmp'_ta_`i'_c15, replace
    restore

    * --- 5-year, global benchmark ---
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
    cap save `tmp'_ta_`i'_g5, replace
    restore

    * --- 15-year, global benchmark ---
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
    cap save `tmp'_ta_`i'_g15, replace
    restore
}

* --------------------------------------------------------------------------
* Collapse: mean growth gap across all spells
* --------------------------------------------------------------------------
foreach s in c5 c15 g5 g15 {
    cap use `tmp'_ta_1_`s', clear
    forvalues i = 2/`N' {
        cap append using `tmp'_ta_`i'_`s'
    }
    egen mggap = mean(ggap)
    collapse mggap
    gen spec = "`s'"
    save `tmp'_xa_`s', replace
}

use `tmp'_xa_c5, clear
append using `tmp'_xa_c15
append using `tmp'_xa_g5
append using `tmp'_xa_g15

* Clean up temp files
forvalues i = 1/`N' {
    foreach s in c5 c15 g5 g15 {
        cap erase `tmp'_ta_`i'_`s'.dta
    }
}
foreach s in c5 c15 g5 g15 {
    cap erase `tmp'_xa_`s'.dta
}

foreach n in 5 15 {
    gen avgc`n' = mggap if spec == "c`n'"
    gen avgy`n' = mggap if spec == "g`n'"
    replace spec = "`n' years" if spec == "c`n'"
    replace spec = "`n' years" if spec == "g`n'"
}

rename (avgc5 avgy5) (Country_level Global_level)
replace Country_level = avgc15 if avgc15 != .
replace Global_level  = avgy15 if avgy15 != .
gen order = _n

di "--- Figure A_3 growth gaps ---"
list spec Country_level Global_level

graph bar (mean) Country_level Global_level, ///
    over(spec, sort(order) lab(nolab) axis(off)) ///
    bargap(5) ///
    yline(0.00, noextend lcolor(black) lwidth(thick) lstyle(foreground)) ///
    bar(1, color(white) lcolor(black) lwidth(medthick)) ///
    bar(2, color(gs12) lcolor(black) lwidth(medthick)) ///
    ylab(0 "0 pp" -.2 "-0.2 pp" -.4 "-0.4 pp" -.6 "-0.6 pp" ///
         -.8 "-0.8 pp" -1.0 "-1.0 pp" -1.2 "-1.2 pp" -1.4 "-1.4 pp", ///
         nogrid labsize(medsmall) angle(0)) ///
    legend(rows(2) order(1 "Gap to country level" 2 "Gap to global level") ///
           symxsize(*0.6) symysize(*0.8) region(lcolor(white)) size(medsmall)) ///
    title("Annualized growth gap                   Annualized growth gap", ///
          margin(zero) size(medsmall) color(black)) ///
    graphregion(color(white)) ///
    subtitle("(5-year aftermath)                            (15-year aftermath)", ///
             margin(vsmall) size(medsmall) color(black))

gr export figures/FigureA_3.pdf, replace
di "Figure A_3 saved."

clear

} // end capture Figure A_3

* =========================================================================
* Figure A_4: Real GDP Paths after Authoritarian Leaders Come to Power
*             Local Projections (analogue of FST Figure 4)
* =========================================================================

capture {

use data/auth_dataset, clear

xtset cid year
gen lrgdppc = log(fstgdp)

* Outcome: cumulative log GDP h years ahead (relative to year 0)
forvalues h = 1/15 {
    gen rgdppc_gr`h' = (f`h'.lrgdppc - lrgdppc) * 100
    label var rgdppc_gr`h' "Y `h'"
}

gen grlrgdppc = (lrgdppc - L1.lrgdppc) * 100
egen mgrlrgdppc = mean(grlrgdppc), by(year)

* --------------------------------------------------------------------------
* Build treatment indicators analogous to FST's np / ap / le / ri
* --------------------------------------------------------------------------

gen np_auth = (auth == 0 & auth_is_populist == 0)

forvalues h = 1/5 {
    replace np_auth = 0 if F`h'.atakeover_auth == 1
    replace np_auth = 0 if L`h'.atakeover_auth == 1
}

gen ap_auth = atakeover_auth

gen sp_auth = atakeover_auth * (auth_type_str == "Single-party")
gen mi_auth = atakeover_auth * (inlist(auth_type_str, "Military", "Military-personal"))
gen pe_auth = atakeover_auth * (inlist(auth_type_str, "Personalist", "Oligarchy", "Monarchy"))

gen years = _n - 1 if (_n <= 15 + 1)
gen zero  = 0

foreach t in np_auth ap_auth sp_auth mi_auth pe_auth {
    foreach v in irf se up lo {
        gen `v'_`t' = 0
    }
}

* Determine regression sample (use longest horizon for consistency)
xtreg rgdppc_gr15 np_auth ap_auth ///
    L(1/5).institutions L(1/5).mgrlrgdppc L(1/5).grlrgdppc ///
    L(1/5).bankcrisis L(1/5).debtcrisis L(1/5).inflation, fe
gen regsample = 1 if e(sample)

* --------------------------------------------------------------------------
* Panel A: LP for all authoritarians vs. normal years
* --------------------------------------------------------------------------
forvalues h = 1/15 {
    xtreg rgdppc_gr`h' np_auth ap_auth ///
        L(1/5).institutions L(1/5).mgrlrgdppc L(1/5).grlrgdppc ///
        L(1/5).bankcrisis L(1/5).debtcrisis L(1/5).inflation ///
        if regsample == 1, fe
    test np_auth = ap_auth
    lincom _cons + np_auth
    replace irf_np_auth = r(estimate) if _n == `h' + 1
    replace se_np_auth  = r(se)       if _n == `h' + 1
    lincom _cons + ap_auth
    replace irf_ap_auth = r(estimate) if _n == `h' + 1
    replace se_ap_auth  = r(se)       if _n == `h' + 1
}

replace up_np_auth = irf_np_auth + 1.96 * se_np_auth if _n <= 16
replace lo_np_auth = irf_np_auth - 1.96 * se_np_auth if _n <= 16
gen irf_ap_gap = irf_ap_auth - irf_np_auth

twoway ///
    (rarea up_np_auth lo_np_auth years, fcolor(gs12) lcolor(white)) ///
    (line irf_np_auth years, lcolor(blue) lpattern(solid) lwidth(thick)) ///
    (line irf_ap_auth years, lcolor(red) lpattern(shortdash) lwidth(vthick)) ///
    (line zero years, lcolor(black)) ///
    if years <= 15, ///
    ylabel(, nogrid) ///
    legend(rows(3) label(3 "All authoritarians") label(2 "Trend in other years") ///
           label(1 "95% CI") order(3 2 1) ///
           symxsize(*0.375) symysize(*0.375) size(small) region(lwidth(none))) ///
    xlabel(, labsize(medlarge)) ylabel(, labsize(medlarge) angle(0)) ///
    title("Panel A: Projected trends", color(black) size(large) margin(medium)) ///
    ytitle("Percent (100 × log)", size(medsmall) margin(small)) xtitle("") ///
    graphregion(color(white)) plotregion(color(white)) ///
    name(pa4, replace) nodraw

* --------------------------------------------------------------------------
* Panel B: LP gap by authoritarian type (single-party / military / personalist)
* --------------------------------------------------------------------------
foreach t in sp_auth mi_auth pe_auth {
    forvalues h = 1/15 {
        xtreg rgdppc_gr`h' np_auth `t' ///
            L(1/5).institutions L(1/5).mgrlrgdppc L(1/5).grlrgdppc ///
            L(1/5).bankcrisis L(1/5).debtcrisis L(1/5).inflation ///
            if regsample == 1, fe
        lincom _cons + `t'
        replace irf_`t' = r(estimate) if _n == `h' + 1
        replace se_`t'  = r(se)       if _n == `h' + 1
    }
    gen irf_`t'_gap = irf_`t' - irf_np_auth
}

twoway ///
    (line irf_ap_gap    years, lcolor(black) lpattern(solid)      lwidth(thick)) ///
    (line irf_sp_auth_gap years, lcolor(blue)  lpattern(dash)       lwidth(thick)) ///
    (line irf_mi_auth_gap years, lcolor(red)   lpattern(shortdash)  lwidth(thick)) ///
    (line irf_pe_auth_gap years, lcolor(green) lpattern(longdash)   lwidth(thick)) ///
    if years <= 15, ///
    legend(rows(2) ///
           label(1 "All authoritarian") label(2 "Single-party") ///
           label(3 "Military") label(4 "Personalist") ///
           order(1 2 3 4) symxsize(*0.375) symysize(*0.375) ///
           size(small) region(lwidth(none))) ///
    xlabel(, labsize(medlarge)) ///
    ylabel(-15 "-15 pp" -10 "-10 pp" -5 "-5 pp" 0 "0 pp" +5 "+5 pp" +10 "+10 pp", ///
           labsize(medlarge) angle(0)) ///
    title("Panel B: Projected gap by type", color(black) size(large) margin(medium)) ///
    xtitle("") ytitle("") ///
    graphregion(color(white)) name(pb4, replace) nodraw

gr combine pa4 pb4, rows(1) iscale(0.75) graphregion(color(white) margin(l=1 r=3 t=1)) ///
    imargin(2 2) xsize(14) ysize(7)
gr export figures/FigureA_4.pdf, replace
di "Figure A_4 saved."

clear

} // end capture Figure A_4

* =========================================================================
* Figure A_5: Inverse-Probability Weighted Local Projection
*             (analogue of FST Figure 5)
* =========================================================================

capture {

use data/auth_dataset, clear

egen ccode = group(iso)
tsset ccode year, yearly

gen lrgdp  = log(fstgdp)
gen dlrgdp = 100 * d.lrgdp

forvalues i = 1/15 {
    gen lrgdp`i' = 100 * (f`i'.lrgdp - lrgdp)
    label var lrgdp`i' "Y `i'"
}

* Replace SVK 1985 growth rate (splice for Slovak data discontinuity)
replace dlrgdp = dlrgdp[_n+1] if iso == "SVK" & year == 1990 - 5

egen wdlrgdp = mean(dlrgdp), by(year)

* Core sample: GWF coverage window; keep auth takeover years and normal years only
gen core_auth = 1 if (year >= 1946 & year <= 2010)

* Build normal-year indicator: exclude years within 5 of any auth takeover
gen np_auth = (auth == 0 & auth_is_populist == 0)
forvalues h = 1/5 {
    replace np_auth = 0 if F`h'.atakeover_auth == 1
    replace np_auth = 0 if L`h'.atakeover_auth == 1
}

replace core_auth = . if np_auth == 0 & atakeover_auth == 0

gen dinstitutions = d.institutions
replace institutions = dinstitutions

foreach r in bankcrisis currcrisis debtcrisis dlrgdp wdlrgdp institutions war ///
             gini unemployrate conflicts koftrade global inflation debtgdp {
    qui tssmooth ma `r'_wma = `r', weights(5 4 3 2 <1>)
    replace `r' = l1.`r'_wma
}

label var np_auth      "Non-authoritarian"
label var atakeover_auth "Authoritarian takeover"
label var dlrgdp       "Growth rate"
label var wdlrgdp      "World growth"
label var institutions "Institutional quality"
label var war          "World war"
label var debtgdp      "Debt/GDP"
label var conflicts    "Social conflicts (polarization)"
label var gini         "Income inequality (Gini)"
label var global       "Financial openness"
label var koftrade     "Trade openness"
label var unemployrate "Unemployment"
label var inflation    "Inflation"
label var bankcrisis   "Banking crisis"
label var currcrisis   "Currency crisis"
label var debtcrisis   "Sovereign debt crisis"

local type np_auth atakeover_auth
foreach t of local type {
    gen b`t'0  = 0
    gen se`t'0 = 0
}

qui sum ccode, d
local c = r(max)
forvalues i = 1/`c' {
    gen      dum`i' = 0
    replace  dum`i' =   1 - 1/`c' if ccode == `i'
    replace  dum`i' =     - 1/`c' if ccode != `i'
}

local rhsall bankcrisis currcrisis debtcrisis dlrgdp wdlrgdp institutions ///
             war gini unemployrate conflicts koftrade global inflation debtgdp
foreach v of local rhsall {
    qui bys iso: sum `v'
    replace `v' = `v' - r(mean)
}

local rhs1 bankcrisis dlrgdp wdlrgdp institutions inflation war

local ndum = `c' - 1
estimates clear

* Stage 1: propensity score logit
* Note: spec 9 (unemployrate) dropped — variable is near-entirely missing
*       for authoritarian episodes (mostly developing countries, pre-1990)
local rhs1 bankcrisis dlrgdp wdlrgdp institutions inflation war
local rhs2 bankcrisis dlrgdp wdlrgdp institutions inflation war currcrisis debtcrisis
local rhs3 bankcrisis dlrgdp wdlrgdp institutions inflation war debtgdp
local rhs4 bankcrisis dlrgdp wdlrgdp institutions inflation war conflicts
local rhs5 bankcrisis dlrgdp wdlrgdp institutions inflation war gini
local rhs6 bankcrisis dlrgdp wdlrgdp institutions inflation war global
local rhs7 bankcrisis dlrgdp wdlrgdp institutions inflation war koftrade
local rhs8 bankcrisis dlrgdp wdlrgdp institutions inflation war currcrisis debtcrisis debtgdp conflicts gini global koftrade

forvalues i = 1/8 {
    qui logit atakeover_auth `rhs`i'' dum1-dum`ndum' if core_auth == 1
    qui eststo mar_a`i': margins if e(sample), dydx(`rhs`i'') post
    qui logit atakeover_auth `rhs`i'' dum1-dum`ndum' if core_auth == 1
    qui eststo mco_a`i': margins if e(sample), post
}

forvalues i = 1/8 {
    qui logit atakeover_auth `rhs`i'' dum1-dum`ndum' if core_auth == 1
    qui predict phat`i' if e(sample) == 1
    qui gen _`i'_sample = 1 if e(sample)
    qui roctab atakeover_auth phat`i' if phat`i' != .
    qui local AUC = `r(area)'
    qui estadd local AUC "`:di %6.3f `AUC''"
    qui local AUC_se = `r(se)'
    qui estadd local AUC_se "`:di %6.3f `AUC_se''"
    qui local Observations = `r(N)'
    qui estadd local Observations "`:di %4.0fc `Observations''"
    qui est sto _a`i'
}

* Save Table A_C1 (logit marginal effects)
esttab mar_a* using tables/TableA_C1.tex, ///
    cell(b(fmt(3) star) se(fmt(3) par)) noobs stats() varwidth(32) ///
    eqlabels(none) mlabels(none) starlevels(* 0.1 ** 0.05 *** 0.01) ///
    label nonotes replace page prehead("\begin{tabular}{l*{8}{c}}") postfoot("")
esttab mco_a* using tables/TableA_C1.tex, ///
    cell(b(fmt(3) star) se(fmt(3) par)) noobs stats() varwidth(32) ///
    starlevels(* 0.1 ** 0.05 *** 0.01) nonumber label nonotes ///
    eqlabels(none) mlabels(none) append page postfoot("") prehead("")
esttab _a* using tables/TableA_C1.tex, drop(*) ///
    stats(Observations AUC AUC_se r2_p, fmt(%4.3fc)) modelwidth(9) varwidth(32) ///
    label nonotes nonumber eqlabels(none) mlabels(none) ///
    append page prehead("") postfoot("\end{tabular}")

* Stage 1 kernel density plot
twoway ///
    (kdensity phat1 if atakeover_auth == 1, lpattern(dash) color(red)  lwidth(medthick)) ///
    (kdensity phat1 if atakeover_auth == 0, color(blue) lwidth(thick)), ///
    text(5.1 .125 "Distribution for control units",  placement(e) color(blue)) ///
    text(1.6 .400 "Distribution for treated units",  placement(e) color(red)) ///
    title("Stage 1: Logit prediction", color(black) size(large) margin(medium)) ///
    ylabel(, labsize(medlarge) angle(0)) xlabel(0(1)1, labsize(medlarge)) ///
    ytitle("Frequency") xtitle("Estimated probability of treatment") ///
    graphregion(color(white)) plotregion(lpattern(blank)) scheme(s1color) ///
    legend(off) nodraw name(stage1_a, replace)

* Save propensity scores for inspection
preserve
keep if phat1 != . & atakeover_auth == 1
keep phat1 year cid auth_leader_name
sort phat1
cap save `tmp'probabilities_auth, replace
restore

drop if phat1 == .
egen newid = group(iso)
qui sum newid
local c = r(max)
forvalues i = 1/`c' {
    gen      d`i' = 0
    replace  d`i' =   1 - 1/`c' if newid == `i'
    replace  d`i' =     - 1/`c' if newid != `i'
}
gen lessone = `c' - 1
qui sum lessone
local g = r(mean)

* IPW weights
reg lrgdp1 atakeover_auth np_auth `rhs1' d1-d`g' phat1 if _1_sample == 1, noconstant cluster(iso)

foreach v of local rhs1 {
    bys iso: sum `v' if e(sample) == 1
    gen `v'a = `v' - r(mean)
}

gen invwt   = atakeover_auth / phat1 + (1 - atakeover_auth) / (1 - phat1) ///
              if phat1 != . & e(sample) == 1
gen asample = 1 if e(sample) == 1

* Stage 2: IPW LP for h = 1..15
forvalues i = 1/15 {
    reg lrgdp`i' np_auth atakeover_auth `rhs1' d1-d`g' ///
        [pweight = invwt] if asample == 1, noconstant cluster(iso)
    test atakeover_auth = np_auth
    estadd scalar NonpAuth_pdiff = r(p)
    eststo IPW_a`i'
    gen betah_np_auth_`i'       = _b[np_auth]
    gen seh_np_auth_`i'         = _se[np_auth]
    gen betah_atakeover_auth_`i' = _b[atakeover_auth]
    gen seh_atakeover_auth_`i'  = _se[atakeover_auth]
}

forvalues i = 1/15 {
    foreach t of local type {
        replace b`t'0  = betah_`t'_`i' if _n == `i' + 1
        replace se`t'0 = seh_`t'_`i'   if _n == `i' + 1
    }
}

gen Years = _n - 1 if _n <= 16
label var Years "Years"
gen zero = 0

cap gen upb_np_auth0  = bnp_auth0  + 1.645 * senp_auth0
cap gen dnb_np_auth0  = bnp_auth0  - 1.645 * senp_auth0

twoway ///
    (rarea upb_np_auth0 dnb_np_auth0 Years, fcolor(gs12) lcolor(white) lpattern(solid)) ///
    (line bnp_auth0       Years, lcolor(blue) lpattern(solid)    lwidth(thick)) ///
    (line batakeover_auth0 Years, lcolor(red)  lpattern(shortdash) lwidth(vthick)) ///
    (line zero Years, lcolor(black)) if Years <= 15, ///
    ylabel(, nogrid) ///
    ytitle("Percent (100 x log)", size(medsmall) margin(small)) ///
    xlabel(, labsize(medlarge)) graphregion(color(white)) plotregion(color(white)) ///
    title("Stage 2: Inverse-propensity weighted local projection", ///
          color(black) size(large) margin(medium)) ///
    ylabel(, labsize(medlarge) angle(0)) legend(off) ///
    xsize(3) ysize(3) nodraw name(stage2_a, replace)

gr combine stage1_a stage2_a, rows(1) iscale(0.75) ///
    graphregion(color(white) margin(l=0 r=3 t=1)) imargin(0 0) xsize(10) ysize(4.25)
gr export figures/FigureA_5.pdf, replace
di "Figure A_5 saved."

* Save Table A_C3 (IPW regression)
esttab IPW_a* using tables/TableA_C3.tex, replace ///
    scalars(NonpAuth_pdiff) se r2 keep(atakeover_auth np_auth) ///
    nonum compress b(2) se(2) sfmt(2) obslast nonotes label ///
    starlevels(* 0.1 ** 0.05 *** 0.01)

cap erase `tmp'probabilities_auth.dta
clear

} // end capture Figure A_5

*--------------------------------------------------------------------------
* Safety cleanup: remove any leftover temp files
*--------------------------------------------------------------------------
forvalues i = 1/200 {
    foreach s in c5 c15 g5 g15 {
        cap erase `tmp'_ta_`i'_`s'.dta
    }
}
foreach s in c5 c15 g5 g15 {
    cap erase `tmp'_xa_`s'.dta
}
