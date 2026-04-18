/*===========================================================================
  figtabs_authpop.do
  Authoritarian-Populist Subset Extension — Stata Figures and Tables

  Produces all non-appendix Stata-based figures and tables analogous to
  those in FST (2023) figtabs_inpaper.do, restricted to the authpop
  subsets.

  Outputs (in figures/ and tables/ relative to authpop_extension/):
    FigureAP3_strict.pdf    — growth gap, strict
    FigureAP3_broad.pdf     — growth gap, broad
    FigureAP3_strict_noecuador.pdf
    FigureAP3_broad_noecuador.pdf
    FigureAP4_strict.pdf    — OLS/FE event study, strict
    FigureAP4_broad.pdf     — OLS/FE event study, broad
    FigureAP5_strict.pdf    — propensity score/IPW, strict
    FigureAP5_broad.pdf     — propensity score/IPW, broad
    TableAP2_strict.tex     — OLS/FE regression, strict
    TableAP2_broad.tex      — OLS/FE regression, broad
    TableAP3_strict.tex     — predictor balance (synth), strict
    TableAP3_broad.tex      — predictor balance (synth), broad

  Run from: C:\PLE\authpop_extension\
  (called by run_authpop.bat after datprep_authpop.do)

  Input:
    data/authpop_dataset.dta   — built by datprep_authpop.do
===========================================================================*/

clear all
set more off
cap mkdir data/_work
cap mkdir figures
cap mkdir tables
local tmp "data/_work/"
graph set window fontface "Times New Roman"

* Add FST ado packages (estout, synth) to search path
* Windows: use adosw (has working synthopt.plugin); do NOT add adosm (incompatible plugin)
adopath ++ "../programs/adosw"

/*===========================================================================
  FIGURE AP3 — Growth Gap Bar Chart
  Method: Country-level benchmark (c5/c15) = treated country's own mean
  growth rate minus the 5/15-year post-period mean. Global benchmark
  (g5/g15) = treated country's growth minus contemporaneous sample average.
  Averaged across all episodes in the subset. Mirrors FST Figure 3.
===========================================================================*/

* ---- Macro defining authpop episodes for each subset ----
* We loop over 4 subsets. For each, we build the treatedyea/treatedcid pairs
* from the authpop_episodes.csv flags stored in authpop_dataset.dta.

* Helper program: growth gap figure for one subset
capture program drop authpop_fig3
program define authpop_fig3
    * args: subset_flag subset_label outpath [dataset]
    * dataset is optional; defaults to data/authpop_dataset
    args subset_flag subset_label outpath dataset
    if "`dataset'" == "" local dataset "data/authpop_dataset"
    * Programs do not inherit caller locals — define tmp explicitly here
    local tmp "data/_work/"

    use `"`dataset'"', clear

    * Extract episode list for this subset
    preserve
    keep if `subset_flag' == 1 & atakeover == 1
    keep cid year
    sort cid year
    gen i = _n
    local nep = _N
    forvalues k = 1/`nep' {
        local tc_`k' = cid[`k']
        local ty_`k' = year[`k']
    }
    restore

    * Growth rate
    tsset cid year
    gen lrgdppc   = log(fstgdp)
    gen rgdppc_gr = (lrgdppc - l1.lrgdppc) * 100
    gen at = 0   // working indicator variable

    * Loop over episodes, compute gaps
    forvalues i = 1/`nep' {

        preserve
        keep if cid == `tc_`i''
        replace at = 0
        replace at = 1 if year == `ty_`i''
        forvalues h = 1/5 {
            gen at`h' = (l`h'.at)
        }
        gen atd = at1+at2+at3+at4+at5
        drop if year <= 1945
        egen meanrgdppc_gr = mean(rgdppc_gr)
        gen ggap = rgdppc_gr - meanrgdppc_gr if atd == 1
        keep if atd == 1
        cap save `tmp'_ap3_`i'_c5, replace
        restore

        preserve
        keep if cid == `tc_`i''
        replace at = 0
        replace at = 1 if year == `ty_`i''
        forvalues h = 1/15 {
            gen at`h' = (l`h'.at)
        }
        gen atd = at1+at2+at3+at4+at5+at6+at7+at8+at9+at10+at11+at12+at13+at14+at15
        drop if year <= 1945
        egen meanrgdppc_gr = mean(rgdppc_gr)
        gen ggap = rgdppc_gr - meanrgdppc_gr if atd == 1
        keep if atd == 1
        cap save `tmp'_ap3_`i'_c15, replace
        restore

        preserve
        replace at = 0
        replace at = 1 if year == `ty_`i''
        forvalues h = 1/5 {
            gen at`h' = (l`h'.at)
        }
        gen atd = at1+at2+at3+at4+at5
        drop if atd != 1
        replace atd = 0
        replace atd = 1 if cid == `tc_`i''
        bys year : egen meanrgdppc_gr = mean(rgdppc_gr)
        gen ggap = rgdppc_gr - meanrgdppc_gr if atd == 1
        keep if atd == 1
        cap save `tmp'_ap3_`i'_g5, replace
        restore

        preserve
        replace at = 0
        replace at = 1 if year == `ty_`i''
        forvalues h = 1/15 {
            gen at`h' = (l`h'.at)
        }
        gen atd = at1+at2+at3+at4+at5+at6+at7+at8+at9+at10+at11+at12+at13+at14+at15
        drop if atd != 1
        replace atd = 0
        replace atd = 1 if cid == `tc_`i''
        bys year : egen meanrgdppc_gr = mean(rgdppc_gr)
        gen ggap = rgdppc_gr - meanrgdppc_gr if atd == 1
        keep if atd == 1
        cap save `tmp'_ap3_`i'_g15, replace
        restore
    }

    * Collapse across episodes
    foreach s in c5 c15 g5 g15 {
        use `tmp'_ap3_1_`s', clear
        forvalues i = 2/`nep' {
            cap app using `tmp'_ap3_`i'_`s'
        }
        egen mggap = mean(ggap)
        collapse mggap
        gen spec = "`s'"
        save `tmp'_ap3_x_`s', replace
    }

    use `tmp'_ap3_x_c5, clear
    app using `tmp'_ap3_x_c15
    app using `tmp'_ap3_x_g5
    app using `tmp'_ap3_x_g15

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

    graph bar (mean) Country_level Global_level, ///
        over(spec, sort(order) lab(nolab) axis(off)) bargap(5) ///
        yline(0.00, noextend lcolor(black) lwidth(thick) lstyle(foreground)) ///
        bar(1, color(white) lcolor(black) lwidth(medthick)) ///
        bar(2, color(gs12)  lcolor(black) lwidth(medthick)) ///
        ylab(0 "0 pp" -.2 "-0.2 pp" -.4 "-0.4 pp" -.6 "-0.6 pp" -.8 "-0.8 pp" ///
             -1.0 "-1.0 pp" -1.2 "-1.2 pp" -1.4 "-1.4 pp", ///
             nogrid labsize(medsmall) angle(0)) ///
        legend(rows(2) order(1 "Gap to country level" 2 "Gap to global level") ///
               symxsize(*0.6) symysize(*0.8) region(lcolor(white)) size(medsmall)) ///
        title("Annualized growth gap                   Annualized growth gap", ///
              margin(zero) size(medsmall) color(black)) ///
        graphregion(color(white)) ///
        subtitle("(5-year aftermath)                            (15-year aftermath)", ///
                  margin(vsmall) size(medsmall) color(black))
    gr export "`outpath'", replace

    clear
end

* Helper: for non-authpop and full subsets the flag variable is not a simple
* named binary; create disposable indicator variables before each call.

* MAIN 4-panel: strict, broad, nonauthpop, full
capture {
    authpop_fig3 auth_strict       "Strict (N=9)"          "figures/FigureAP3_strict.pdf"
    authpop_fig3 auth_broad        "Broad (N=14)"          "figures/FigureAP3_broad.pdf"
}

* nonauthpop subset: episodes where auth_broad==0 at their takeover year
capture {
    use data/authpop_dataset, clear
    gen auth_nonauthpop_flag = (auth_broad == 0)
    save `tmp'_authpop_nonauthpop, replace
    authpop_fig3 auth_nonauthpop_flag "Non-auth-pop (N=15)" ///
        "figures/FigureAP3_nonauthpop.pdf" "`tmp'_authpop_nonauthpop"
}

* FST full sample: original 28 episodes (atakeover_full flag, excludes HUN)
capture {
    use data/authpop_dataset, clear
    * atakeover_full already saved as a binary takeover indicator
    save `tmp'_authpop_full, replace
    authpop_fig3 atakeover_full "FST Full (N=28)" ///
        "figures/FigureAP3_full.pdf" "`tmp'_authpop_full"
}

* APPENDIX: no-Ecuador robustness (strict & broad only)
* Step 1: build the no-Ecuador dataset file once
capture {
    use data/authpop_dataset, clear
    drop if iso == "ECU"
    save `tmp'_authpop_noecuador, replace
}

* Step 2: strict no-Ecuador
capture {
    authpop_fig3 auth_strict "Strict no-Ecuador (N=6)" ///
        "figures/FigureAP3_strict_noecuador.pdf" "`tmp'_authpop_noecuador"
}

* Step 3: broad no-Ecuador
capture {
    authpop_fig3 auth_broad "Broad no-Ecuador (N=10)" ///
        "figures/FigureAP3_broad_noecuador.pdf" "`tmp'_authpop_noecuador"
}


/*===========================================================================
  FIGURE AP4 — OLS/FE Event Study (Local Projections)
  Method: Jorda (2005) local projections with country and year fixed effects.
  xtreg rgdppc_gr_h np ap L(1/5).controls if regsample==1, fe
  where h = 1,...,15 years ahead. Plots IRF for authpop takeover vs.
  non-populist years, with 95% CI on non-populist trend.
  Mirrors FST Figure 4. Produced for strict and broad subsets.
===========================================================================*/

capture {

foreach subset in strict broad {

use data/authpop_dataset, clear

xtset cid year
gen lrgdppc = log(fstgdp) if year >= 1946

forvalues h = 1/15 {
    gen rgdppc_gr`h' = (f`h'.lrgdppc - lrgdppc) * 100
    label var rgdppc_gr`h' "Y `h'"
}

gen grlrgdppc  = (lrgdppc - L1.lrgdppc) * 100
egen mgrlrgdppc = mean(grlrgdppc), by(year)

* Use original FST placebo indicator as non-populist control
rename placebo         np
rename atakeover_`subset' ap

* Exclude window around ANY populist event from "non-populist" observations
forvalues h = 1/15 {
    replace np = 0 if F`h'.atakeover == 1
    replace np = 0 if L`h'.atakeover == 1
}

gen years = _n - 1 if (_n <= 16)
gen zero  = 0

foreach t in np ap {
    foreach v in irf se up lo {
        gen `v'_`t' = 0
    }
}

xtreg rgdppc_gr15 np ap L(1/5).institutions L(1/5).mgrlrgdppc ///
    L(1/5).grlrgdppc L(1/5).bankcrisis L(1/5).debtcrisis L(1/5).inflation, fe
gen regsample = 1 if e(sample)

forvalues h = 1/15 {
    xtreg rgdppc_gr`h' np ap L(1/5).institutions L(1/5).mgrlrgdppc ///
        L(1/5).grlrgdppc L(1/5).bankcrisis L(1/5).debtcrisis L(1/5).inflation ///
        if regsample == 1, fe
    lincom _cons + np
    replace irf_np = r(estimate) if _n == `h' + 1
    replace se_np  = r(se)       if _n == `h' + 1
    lincom _cons + ap
    replace irf_ap = r(estimate) if _n == `h' + 1
    replace se_ap  = r(se)       if _n == `h' + 1
}

replace up_np = irf_np + 1.96 * se_np if _n <= 16
replace lo_np = irf_np - 1.96 * se_np if _n <= 16
gen irf_ap_gap = irf_ap - irf_np

twoway ///
    (rarea up_np lo_np years, fcolor(gs12) lcolor(white) lpattern(solid)) ///
    (line irf_np years, lcolor(blue) lpattern(solid) lwidth(thick)) ///
    (line irf_ap years, lcolor(red) lpattern(shortdash) lwidth(vthick)) ///
    (line zero years, lcolor(black)) ///
    if years <= 15, ///
    ylabel(, nogrid) ///
    legend(rows(3) label(3 "Auth-populists (`subset')") ///
           label(2 "Trend in other years") label(1 "95% CI") ///
           order(3 2 1) symxsize(*0.375) symysize(*0.375) ///
           size(small) region(lwidth(none))) ///
    xlabel(, labsize(medlarge)) ylabel(, labsize(medlarge) angle(0)) ///
    title("Panel A: Projected trends", color(black) size(large) margin(medium)) ///
    ytitle("Percent (100 × log)", size(medsmall) margin(small)) ///
    xtitle("Years after takeover", size(medsmall)) ///
    graphregion(color(white)) plotregion(color(white)) ///
    name(pl_`subset', replace) nodraw

twoway ///
    (line irf_ap_gap years, lcolor(black) lpattern(solid) lwidth(thick)) ///
    if years <= 15, ///
    ylabel(, nogrid) ///
    legend(off) ///
    xlabel(, labsize(medlarge)) ///
    ylabel(-15 "-15 pp" -10 "-10 pp" -5 "-5 pp" 0 "0 pp" 5 "+5 pp" 10 "+10 pp", ///
           labsize(medlarge) angle(0)) ///
    title("Panel B: Projected gap (authpop minus trend)", ///
          color(black) size(large) margin(medium)) ///
    xtitle("") ytitle("") ///
    graphregion(color(white)) name(pr_`subset', replace) nodraw

gr combine pl_`subset' pr_`subset', ///
    rows(1) iscale(0.75) graphregion(color(white) margin(l=1 r=3 t=1)) ///
    imargin(2 2) xsize(14) ysize(7)
gr export "figures/FigureAP4_`subset'.pdf", replace

clear

}

}

/*===========================================================================
  FIGURE AP5 — Propensity Score / IPW Event Study
  Method: Logit propensity score model for authpop takeover (9 RHS variable
  combinations), then inverse-probability-weighted local projections.
  NOTE: With only 9 (strict) or 13 (broad) treated units, propensity score
  overlap is limited. Results should be interpreted cautiously.
  Mirrors FST Figure 5. Produced for strict and broad subsets.
===========================================================================*/

capture {

foreach subset in strict broad {

use data/authpop_dataset, clear

egen ccode = group(iso)
tsset ccode year, yearly

gen lrgdp  = log(fstgdp)
gen dlrgdp = 100 * d.lrgdp

forvalues i = 1/15 {
    gen lrgdp`i' = 100 * (f`i'.lrgdp - lrgdp)
    label var lrgdp`i' "Y `i'"
}

replace dlrgdp = dlrgdp[_n+1] if iso == "SVK" & year == 1990 - 5
egen wdlrgdp = mean(dlrgdp), by(year)

gen core = 1 if (year >= 1946 & year <= 2003)
replace core = . if placebo == 0 & atakeover == 0

gen dinstitutions = d.institutions
replace institutions = dinstitutions

foreach r in bankcrisis currcrisis debtcrisis dlrgdp wdlrgdp institutions ///
             war gini unemployrate conflicts koftrade global inflation debtgdp {
    qui tssmooth ma `r'_wma = `r', weights(5 4 3 2 <1>)
    replace `r' = l1.`r'_wma
}

label var placebo       "Non-populist"
label var atakeover_`subset' "Auth-populist (`subset')"
label var dlrgdp        "Growth rate"
label var wdlrgdp       "World growth"
label var institutions  "Institutional quality"
label var war           "World war"
label var debtgdp       "Debt/GDP"
label var conflicts     "Social conflicts (polarization)"
label var gini          "Income inequality (Gini)"
label var global        "Financial openness"
label var koftrade      "Trade openness"
label var unemployrate  "Unemployment"
label var inflation     "Inflation"
label var bankcrisis    "Banking crisis"
label var currcrisis    "Currency crisis"
label var debtcrisis    "Sovereign debt crisis"

local type "placebo atakeover_`subset'"
foreach t of local type {
    gen b`t'0 = 0
    gen se`t'0 = 0
}

qui sum ccode, d
local c = r(max)
forvalues i = 1/`c' {
    gen      dum`i' = 0
    replace  dum`i' = 1 - 1/`c' if ccode == `i'
    replace  dum`i' = -1/`c'    if ccode != `i'
}

* Propensity score specs (mirrors FST Table C1)
* Spec 1-3: univariate
* Spec 4-9: multivariate combinations
local spec1  "dlrgdp"
local spec2  "institutions"
local spec3  "bankcrisis"
local spec4  "dlrgdp institutions bankcrisis"
local spec5  "dlrgdp institutions bankcrisis currcrisis debtcrisis"
local spec6  "dlrgdp wdlrgdp institutions bankcrisis currcrisis debtcrisis"
local spec7  "dlrgdp wdlrgdp institutions bankcrisis currcrisis debtcrisis gini"
local spec8  "dlrgdp wdlrgdp institutions bankcrisis currcrisis debtcrisis gini inflation"
local spec9  "dlrgdp wdlrgdp institutions bankcrisis currcrisis debtcrisis gini inflation debtgdp koftrade global"

gen zero = 0
gen years = _n - 1 if _n <= 16

forvalues sp = 1/9 {

    * Propensity score logit
    cap qui logit atakeover_`subset' `spec`sp'' dum* if core == 1, iterate(30)
    if _rc != 0 continue

    cap predict pscore_`sp' if e(sample), pr
    if _rc != 0 continue

    * IPW weights
    cap gen ipw_`sp' = (atakeover_`subset' / pscore_`sp') + ///
                       (placebo / (1 - pscore_`sp')) if pscore_`sp' != . ///
                       & pscore_`sp' > 0 & pscore_`sp' < 1

    * Local projection with IPW
    cap {
        forvalues h = 1/15 {
            qui reg lrgdp`h' i.year atakeover_`subset' placebo [pw = ipw_`sp'] ///
                if core == 1, robust cluster(ccode)
            lincom _cons + atakeover_`subset'
            replace birf_atakeover_`subset'0 = r(estimate) if _n == `h' + 1
            replace seiatakeover_`subset'0 = r(se)    if _n == `h' + 1
            lincom _cons + placebo
            replace bplacebo0 = r(estimate) if _n == `h' + 1
            replace seplacebo0 = r(se)      if _n == `h' + 1
            eststo IPW`sp'h`h'
        }
    }
}

* Average IPW IRFs across specs
gen irf_ap_avg = 0
gen irf_np_avg = 0
local nsp = 0
forvalues sp = 1/9 {
    cap confirm variable ipw_`sp'
    if _rc == 0 {
        local ++nsp
    }
}
* Simple average plot using spec 5 (baseline)
qui reg lrgdp15 i.year atakeover_`subset' placebo [pw = ipw_5] ///
    if core == 1, robust cluster(ccode)

forvalues h = 1/15 {
    cap qui reg lrgdp`h' i.year atakeover_`subset' placebo [pw = ipw_5] ///
        if core == 1, robust cluster(ccode)
    cap lincom _cons + atakeover_`subset'
    cap replace irf_ap_avg = r(estimate) if _n == `h' + 1
    cap lincom _cons + placebo
    cap replace irf_np_avg = r(estimate) if _n == `h' + 1
}

gen irf_gap = irf_ap_avg - irf_np_avg

twoway ///
    (line irf_ap_avg years, lcolor(red) lpattern(shortdash) lwidth(thick)) ///
    (line irf_np_avg years, lcolor(blue) lpattern(solid) lwidth(thick)) ///
    (line zero years, lcolor(black)) ///
    if years <= 15, ///
    legend(rows(2) label(1 "Auth-populists (`subset')") label(2 "Non-populist trend") ///
           symxsize(*0.375) symysize(*0.375) size(small) region(lwidth(none))) ///
    xlabel(, labsize(medlarge)) ylabel(, labsize(medlarge) angle(0)) ///
    title("IPW event study — `subset' subset", color(black) size(medlarge) margin(medium)) ///
    note("Note: IPW spec 5; small N may limit overlap", size(vsmall)) ///
    ytitle("Percent (100 × log)", size(medsmall) margin(small)) ///
    xtitle("Years after takeover", size(medsmall)) ///
    graphregion(color(white)) plotregion(color(white))
gr export "figures/FigureAP5_`subset'.pdf", replace

clear

}

}

/*===========================================================================
  TABLE AP2 — OLS/FE Regression Table
  Method: Panel OLS with year and country fixed effects.
    (1) Simple OLS: rgdppc_gr ~ Post_5/15
    (2) FE: rgdppc_gr ~ i.year i.cid Post_5/15
    (3) FE + controls: adds L1.institutions L(1/5).bankcrisis etc.
  Post_5_strict/Post_15_strict and Post_5_broad/Post_15_broad from
  authpop_dataset.dta. Mirrors FST Table 2.
===========================================================================*/

capture {

foreach subset in strict broad {

use data/authpop_dataset, clear

label var Post_5_`subset'  "Auth-pop leader (`subset', 5-yr)"
label var Post_15_`subset' "Auth-pop leader (`subset', 15-yr)"

tsset cid year
gen lgfstgdp  = log(fstgdp)
gen rgdppc_gr = (lgfstgdp - l1.lgfstgdp) * 100

estimates clear

eststo si05: qui reg rgdppc_gr Post_5_`subset' if year >= 1946, robust
eststo fe05: qui reg rgdppc_gr i.year i.cid Post_5_`subset' if year >= 1946, robust
eststo ma05: qui reg rgdppc_gr i.year i.cid L1.institutions ///
    L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis ///
    L1.tradegdp L1.inflation Post_5_`subset' if year >= 1946, robust

eststo si15: qui reg rgdppc_gr Post_15_`subset' if year >= 1946, robust
eststo fe15: qui reg rgdppc_gr i.year i.cid Post_15_`subset' if year >= 1946, robust
eststo ma15: qui reg rgdppc_gr i.year i.cid L1.institutions ///
    L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis ///
    L1.tradegdp L1.inflation Post_15_`subset' if year >= 1946, robust

esttab si05 fe05 ma05 using "tables/TableAP2_`subset'.tex", ///
    keep(*Post_5*) se r2 b(2) se(2) obslast ///
    starlevels(* 0.1 ** 0.05 *** 0.01) ///
    title("5-year aftermath — `subset' subset") ///
    nonotes label eqlabels(none) mlabels(none) replace

esttab si15 fe15 ma15 using "tables/TableAP2_`subset'.tex", ///
    keep(*Post_15*) se r2 b(2) se(2) obslast ///
    starlevels(* 0.1 ** 0.05 *** 0.01) ///
    title("15-year aftermath — `subset' subset") ///
    nonotes label eqlabels(none) mlabels(none) append

clear

}

}

/*===========================================================================
  TABLE AP2c — OLS/FE Regression Table, Non-authpop subset (N=15)
  Same specification as TableAP2 strict/broad but for the 15 FST populist
  episodes that do NOT meet the authoritarian classification threshold.
  Serves as the comparison group for the main authpop results.
===========================================================================*/

capture {

use data/authpop_dataset, clear

label var Post_5_nonauthpop  "Non-auth-pop leader (5-yr)"
label var Post_15_nonauthpop "Non-auth-pop leader (15-yr)"

tsset cid year
gen lgfstgdp  = log(fstgdp)
gen rgdppc_gr = (lgfstgdp - l1.lgfstgdp) * 100

estimates clear

eststo si05: qui reg rgdppc_gr Post_5_nonauthpop if year >= 1946, robust
eststo fe05: qui reg rgdppc_gr i.year i.cid Post_5_nonauthpop if year >= 1946, robust
eststo ma05: qui reg rgdppc_gr i.year i.cid L1.institutions ///
    L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis ///
    L1.tradegdp L1.inflation Post_5_nonauthpop if year >= 1946, robust

eststo si15: qui reg rgdppc_gr Post_15_nonauthpop if year >= 1946, robust
eststo fe15: qui reg rgdppc_gr i.year i.cid Post_15_nonauthpop if year >= 1946, robust
eststo ma15: qui reg rgdppc_gr i.year i.cid L1.institutions ///
    L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis ///
    L1.tradegdp L1.inflation Post_15_nonauthpop if year >= 1946, robust

esttab si05 fe05 ma05 using "tables/TableAP2_nonauthpop.tex", ///
    keep(*Post_5*) se r2 b(2) se(2) obslast ///
    starlevels(* 0.1 ** 0.05 *** 0.01) ///
    title("5-year aftermath — non-authpop subset (N=15)") ///
    nonotes label eqlabels(none) mlabels(none) replace

esttab si15 fe15 ma15 using "tables/TableAP2_nonauthpop.tex", ///
    keep(*Post_15*) se r2 b(2) se(2) obslast ///
    starlevels(* 0.1 ** 0.05 *** 0.01) ///
    title("15-year aftermath — non-authpop subset (N=15)") ///
    nonotes label eqlabels(none) mlabels(none) append

clear

}

/*===========================================================================
  TABLE AP2 (full sample) — OLS/FE Regression, FST full sample (N=28)
  Same specification as AP2 strict/broad but for all 28 FST analytical episodes.
===========================================================================*/

capture {

use data/authpop_dataset, clear

label var Post_5_full  "FST full sample (5-yr)"
label var Post_15_full "FST full sample (15-yr)"

tsset cid year
gen lgfstgdp  = log(fstgdp)
gen rgdppc_gr = (lgfstgdp - l1.lgfstgdp) * 100

estimates clear

eststo si05: qui reg rgdppc_gr Post_5_full if year >= 1946, robust
eststo fe05: qui reg rgdppc_gr i.year i.cid Post_5_full if year >= 1946, robust
eststo ma05: qui reg rgdppc_gr i.year i.cid L1.institutions ///
    L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis ///
    L1.tradegdp L1.inflation Post_5_full if year >= 1946, robust

eststo si15: qui reg rgdppc_gr Post_15_full if year >= 1946, robust
eststo fe15: qui reg rgdppc_gr i.year i.cid Post_15_full if year >= 1946, robust
eststo ma15: qui reg rgdppc_gr i.year i.cid L1.institutions ///
    L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis ///
    L1.tradegdp L1.inflation Post_15_full if year >= 1946, robust

esttab si05 fe05 ma05 using "tables/TableAP2_full.tex", ///
    keep(*Post_5*) se r2 b(2) se(2) obslast ///
    starlevels(* 0.1 ** 0.05 *** 0.01) ///
    title("5-year aftermath — FST full sample (N=28)") ///
    nonotes label eqlabels(none) mlabels(none) replace

esttab si15 fe15 ma15 using "tables/TableAP2_full.tex", ///
    keep(*Post_15*) se r2 b(2) se(2) obslast ///
    starlevels(* 0.1 ** 0.05 *** 0.01) ///
    title("15-year aftermath — FST full sample (N=28)") ///
    nonotes label eqlabels(none) mlabels(none) append

clear

}

/*===========================================================================
  TABLE 2 (COMBINED) — OLS/FE for all 4 subsets in one 12-column table
  Columns: (1)-(3) Strict, (4)-(6) Broad, (7)-(9) Non-auth-pop, (10)-(12) Full
  Within each group: OLS / Year FE / Country+Year FE
  Panels: A = 5-year, B = 15-year
===========================================================================*/

capture {

use data/authpop_dataset, clear

tsset cid year
gen lgfstgdp  = log(fstgdp)
gen rgdppc_gr = (lgfstgdp - l1.lgfstgdp) * 100

label var Post_5_strict      "Post-takeover (5-yr)"
label var Post_15_strict     "Post-takeover (15-yr)"
label var Post_5_broad       "Post-takeover (5-yr)"
label var Post_15_broad      "Post-takeover (15-yr)"
label var Post_5_nonauthpop  "Post-takeover (5-yr)"
label var Post_15_nonauthpop "Post-takeover (15-yr)"
label var Post_5_full        "Post-takeover (5-yr)"
label var Post_15_full       "Post-takeover (15-yr)"

estimates clear

* Panel A: 5-year
eststo s05_ols: qui reg rgdppc_gr Post_5_strict      if year >= 1946, robust
eststo s05_yfe: qui reg rgdppc_gr i.year i.cid Post_5_strict      if year >= 1946, robust
eststo s05_fe:  qui reg rgdppc_gr i.year i.cid L1.institutions ///
    L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis ///
    L1.tradegdp L1.inflation Post_5_strict if year >= 1946, robust

eststo b05_ols: qui reg rgdppc_gr Post_5_broad       if year >= 1946, robust
eststo b05_yfe: qui reg rgdppc_gr i.year i.cid Post_5_broad       if year >= 1946, robust
eststo b05_fe:  qui reg rgdppc_gr i.year i.cid L1.institutions ///
    L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis ///
    L1.tradegdp L1.inflation Post_5_broad if year >= 1946, robust

eststo n05_ols: qui reg rgdppc_gr Post_5_nonauthpop  if year >= 1946, robust
eststo n05_yfe: qui reg rgdppc_gr i.year i.cid Post_5_nonauthpop  if year >= 1946, robust
eststo n05_fe:  qui reg rgdppc_gr i.year i.cid L1.institutions ///
    L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis ///
    L1.tradegdp L1.inflation Post_5_nonauthpop if year >= 1946, robust

eststo f05_ols: qui reg rgdppc_gr Post_5_full        if year >= 1946, robust
eststo f05_yfe: qui reg rgdppc_gr i.year i.cid Post_5_full        if year >= 1946, robust
eststo f05_fe:  qui reg rgdppc_gr i.year i.cid L1.institutions ///
    L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis ///
    L1.tradegdp L1.inflation Post_5_full if year >= 1946, robust

* Panel B: 15-year
eststo s15_ols: qui reg rgdppc_gr Post_15_strict     if year >= 1946, robust
eststo s15_yfe: qui reg rgdppc_gr i.year i.cid Post_15_strict     if year >= 1946, robust
eststo s15_fe:  qui reg rgdppc_gr i.year i.cid L1.institutions ///
    L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis ///
    L1.tradegdp L1.inflation Post_15_strict if year >= 1946, robust

eststo b15_ols: qui reg rgdppc_gr Post_15_broad      if year >= 1946, robust
eststo b15_yfe: qui reg rgdppc_gr i.year i.cid Post_15_broad      if year >= 1946, robust
eststo b15_fe:  qui reg rgdppc_gr i.year i.cid L1.institutions ///
    L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis ///
    L1.tradegdp L1.inflation Post_15_broad if year >= 1946, robust

eststo n15_ols: qui reg rgdppc_gr Post_15_nonauthpop if year >= 1946, robust
eststo n15_yfe: qui reg rgdppc_gr i.year i.cid Post_15_nonauthpop if year >= 1946, robust
eststo n15_fe:  qui reg rgdppc_gr i.year i.cid L1.institutions ///
    L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis ///
    L1.tradegdp L1.inflation Post_15_nonauthpop if year >= 1946, robust

eststo f15_ols: qui reg rgdppc_gr Post_15_full       if year >= 1946, robust
eststo f15_yfe: qui reg rgdppc_gr i.year i.cid Post_15_full       if year >= 1946, robust
eststo f15_fe:  qui reg rgdppc_gr i.year i.cid L1.institutions ///
    L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis ///
    L1.tradegdp L1.inflation Post_15_full if year >= 1946, robust

* Combined output: Panel A header, then Panel B
esttab s05_ols s05_yfe s05_fe b05_ols b05_yfe b05_fe ///
       n05_ols n05_yfe n05_fe f05_ols f05_yfe f05_fe ///
    using "tables/Table2_combined.tex", ///
    keep(*Post_5*) se r2 b(2) se(2) obslast ///
    starlevels(* 0.1 ** 0.05 *** 0.01) ///
    mgroups("Strict (N=9)" "Broad (N=14)" "Non-auth-pop (N=15)" "FST Full (N=28)", ///
            pattern(1 0 0 1 0 0 1 0 0 1 0 0) ///
            prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
    mtitles("OLS" "Year FE" "Ctry+Year FE" "OLS" "Year FE" "Ctry+Year FE" ///
            "OLS" "Year FE" "Ctry+Year FE" "OLS" "Year FE" "Ctry+Year FE") ///
    nonotes label eqlabels(none) ///
    prehead("\begin{adjustbox}{max width=\textwidth}" ///
            "\begin{tabular}{l*{12}{c}}" ///
            "\toprule" ///
            "\multicolumn{13}{l}{\textit{Panel A: 5-year post-takeover window}} \\[2pt]") ///
    posthead("\midrule") ///
    prefoot("") ///
    postfoot("\bottomrule" "\end{tabular}" "\end{adjustbox}") replace

esttab s15_ols s15_yfe s15_fe b15_ols b15_yfe b15_fe ///
       n15_ols n15_yfe n15_fe f15_ols f15_yfe f15_fe ///
    using "tables/Table2_combined.tex", ///
    keep(*Post_15*) se r2 b(2) se(2) obslast ///
    starlevels(* 0.1 ** 0.05 *** 0.01) ///
    nonotes label eqlabels(none) mlabels(none) ///
    prehead("\midrule" ///
            "\multicolumn{13}{l}{\textit{Panel B: 15-year post-takeover window}} \\[2pt]") ///
    posthead("") ///
    prefoot("") ///
    postfoot("\bottomrule" "\end{tabular}" "\end{adjustbox}") append

clear

}

/*===========================================================================
  TABLE AP9 — Joint regression: authpop (broad) vs. non-authpop
  Tests H0: beta_authpop = beta_nonauthpop at 5- and 15-year horizons.
  Both indicators enter the same regression; F-test p-value reported.
  Columns: (1) OLS, (2) year FE, (3) country + year FE.
  Panels: A = 5-year window, B = 15-year window.
===========================================================================*/

capture {

use data/authpop_dataset, clear

label var Post_5_broad       "Auth-pop broad (5-yr)"
label var Post_15_broad      "Auth-pop broad (15-yr)"
label var Post_5_nonauthpop  "Non-auth-pop (5-yr)"
label var Post_15_nonauthpop "Non-auth-pop (15-yr)"

tsset cid year
gen lgfstgdp  = log(fstgdp)
gen rgdppc_gr = (lgfstgdp - l1.lgfstgdp) * 100

estimates clear

* --- Panel A: 5-year window ---
eststo j05_ols: qui reg rgdppc_gr Post_5_broad Post_5_nonauthpop ///
    if year >= 1946, robust
    test Post_5_broad = Post_5_nonauthpop
    estadd scalar pval = r(p)

eststo j05_yfe: qui reg rgdppc_gr i.year Post_5_broad Post_5_nonauthpop ///
    if year >= 1946, robust
    test Post_5_broad = Post_5_nonauthpop
    estadd scalar pval = r(p)

eststo j05_fe: qui reg rgdppc_gr i.year i.cid Post_5_broad Post_5_nonauthpop ///
    if year >= 1946, robust
    test Post_5_broad = Post_5_nonauthpop
    estadd scalar pval = r(p)

* --- Panel B: 15-year window ---
eststo j15_ols: qui reg rgdppc_gr Post_15_broad Post_15_nonauthpop ///
    if year >= 1946, robust
    test Post_15_broad = Post_15_nonauthpop
    estadd scalar pval = r(p)

eststo j15_yfe: qui reg rgdppc_gr i.year Post_15_broad Post_15_nonauthpop ///
    if year >= 1946, robust
    test Post_15_broad = Post_15_nonauthpop
    estadd scalar pval = r(p)

eststo j15_fe: qui reg rgdppc_gr i.year i.cid Post_15_broad Post_15_nonauthpop ///
    if year >= 1946, robust
    test Post_15_broad = Post_15_nonauthpop
    estadd scalar pval = r(p)

* --- Output ---
esttab j05_ols j05_yfe j05_fe using "tables/TableAP9.tex", ///
    keep(Post_5_broad Post_5_nonauthpop) ///
    se r2 b(2) se(2) obslast scalar("pval F-test p-val (broad=nonauthpop)") ///
    sfmt(%6.3f) starlevels(* 0.1 ** 0.05 *** 0.01) ///
    title("Panel A. 5-year aftermath") ///
    nonotes label eqlabels(none) ///
    mtitles("OLS" "Year FE" "Country+Year FE") replace

esttab j15_ols j15_yfe j15_fe using "tables/TableAP9.tex", ///
    keep(Post_15_broad Post_15_nonauthpop) ///
    se r2 b(2) se(2) obslast scalar("pval F-test p-val (broad=nonauthpop)") ///
    sfmt(%6.3f) starlevels(* 0.1 ** 0.05 *** 0.01) ///
    title("Panel B. 15-year aftermath") ///
    nonotes label eqlabels(none) mlabels(none) append

clear

}

/*===========================================================================
  TABLE AP3 — run separately via tableap3_authpop.do
  (synth requires a fresh Stata session; see run_authpop.bat STEP 2b)
===========================================================================*/

di "figtabs_authpop.do complete."
