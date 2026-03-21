/*===========================================================================
  figtabs_auth.do
  Authoritarian Leaders Extension — Figures and Tables

  Produces authoritarian analogues of FST (2023) main-text figures.
  Requires: data/auth_dataset.dta (output of datprep_auth.do)

  Figures produced:
    FigureA_3.pdf  — Average annualized growth gap (analogue of Figure 3)
    FigureA_4.pdf  — GDP local projections (analogue of Figure 4)
===========================================================================*/

clear all
set more off
capture mkdir figures

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
    cap save _ta_`i'_c5, replace
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
    cap save _ta_`i'_c15, replace
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
    cap save _ta_`i'_g5, replace
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
    cap save _ta_`i'_g15, replace
    restore
}

* --------------------------------------------------------------------------
* Collapse: mean growth gap across all spells
* --------------------------------------------------------------------------
foreach s in c5 c15 g5 g15 {
    cap use _ta_1_`s', clear
    forvalues i = 2/`N' {
        cap append using _ta_`i'_`s'
    }
    egen mggap = mean(ggap)
    collapse mggap
    gen spec = "`s'"
    save _xa_`s', replace
}

use _xa_c5, clear
append using _xa_c15
append using _xa_g5
append using _xa_g15

* Clean up temp files using Stata's erase (avoids Windows del prompt)
forvalues i = 1/`N' {
    foreach s in c5 c15 g5 g15 {
        cap erase _ta_`i'_`s'.dta
    }
}
foreach s in c5 c15 g5 g15 {
    cap erase _xa_`s'.dta
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
*
*   np_auth = "normal" non-authoritarian years:
*             zero out years within 5 years of any auth takeover (either dir)
*   ap_auth = authoritarian takeover year (= atakeover_auth)
*   sp_auth = single-party takeover (subset of ap_auth)
*   mi_auth = military / military-personal takeover (subset of ap_auth)
*   pe_auth = personalist takeover (subset of ap_auth)
* --------------------------------------------------------------------------

* Start from a "placebo" that is 1 for all non-auth years
gen np_auth = (auth == 0 & auth_is_populist == 0)

* Zero out np_auth within 5 years before/after any auth takeover
forvalues h = 1/5 {
    replace np_auth = 0 if F`h'.atakeover_auth == 1
    replace np_auth = 0 if L`h'.atakeover_auth == 1
}

* Main treatment: authoritarian takeover year
gen ap_auth = atakeover_auth

* By-type dummies (for Panel B subgroup split)
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

*--------------------------------------------------------------------------
* Clean up any leftover temp files (if capture block errored early)
*--------------------------------------------------------------------------
forvalues i = 1/200 {
    foreach s in c5 c15 g5 g15 {
        cap erase _ta_`i'_`s'.dta
    }
}
foreach s in c5 c15 g5 g15 {
    cap erase _xa_`s'.dta
}
