/*===========================================================================
  figtabs_authpop.do
  Authoritarian-Populist Subset Extension — Stata Figures and Tables

  Produces all non-appendix Stata-based figures and tables analogous to
  those in FST (2023) figtabs_inpaper.do, restricted to the authpop
  subsets.

  Outputs (in figures/ and tables/ relative to authpop_extension/):
    TableAP1_strict.tex     — episode list, strict subset (N=9)
    TableAP1_broad.tex      — episode list, broad subset (N=13)
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
  FIGURE AP1 — Authoritarian Episodes in Context
  Method: Descriptive chart. Shows all 28 FST core populist episodes with
  each episode marked as: strict authpop (dark), broad-only authpop (grey),
  or non-auth (white). Episodes plotted by takeover year on x-axis.
  Analogous context figure to FST Figure 1 (historical wave).
===========================================================================*/

capture {

import delimited using "data/authpop_episodes.csv", varnames(1) clear
gen type = 3              // non-auth (default)
replace type = 2 if auth_broad == 1 & auth_strict == 0   // broad-only
replace type = 1 if auth_strict == 1                      // strict

gen labstr = iso + " " + string(year)
gen typelabel = "Non-authoritarian"   if type == 3
replace typelabel = "Broad (became auth.)"  if type == 2
replace typelabel = "Strict (auth. at takeover)" if type == 1

gen decade = floor(year/10)*10
gen n = _n

* Horizontal lollipop: episode year on x, each episode as a row
* Sort by year
sort year
gen row = _n

twoway ///
    (scatter row year if type == 3, msymbol(circle_hollow) mcolor(gs10) msize(medium)) ///
    (scatter row year if type == 2, msymbol(circle) mcolor(gs7) msize(medium)) ///
    (scatter row year if type == 1, msymbol(circle) mcolor(black) msize(medium) ///
        mlabel(labstr) mlabsize(vsmall) mlabgap(tiny) mlabposition(3)) ///
    , ///
    xlabel(1940 1950 1960 1970 1980 1990 2000 2010, labsize(small)) ///
    ytitle("") xtitle("Takeover year", size(small)) ///
    ylabel(none) ///
    legend(order(1 "Non-auth. (19 episodes)" 2 "Broad-only (+4 ep.)" 3 "Strict auth. (9 episodes)") ///
           rows(1) size(small) region(lcolor(white))) ///
    title("All 28 FST core populist episodes by authoritarian classification", ///
          size(medsmall) color(black)) ///
    graphregion(color(white)) scheme(s1mono) xsize(14) ysize(8)
gr export "figures/FigureAP1.pdf", replace

clear

}

/*===========================================================================
  FIGURE AP2 — Episode Timeline (Stripplot)
  Method: Descriptive stripplot. Shows authpop episodes by country, colored
  by left/right wing. Strict subset in solid markers, broad-only in hollow.
  Analogous to FST Figure 2.
===========================================================================*/

capture {

import delimited using "data/authpop_episodes.csv", varnames(1) clear

* Keep only authpop episodes
keep if auth_broad == 1

gen subtype = 1 if auth_strict == 1                        // strict
replace subtype = 2 if auth_broad == 1 & auth_strict == 0  // broad-only

* Country label for y-axis (with year)
gen leaderlabel = leader

* Use twoway scatter as a stripplot substitute
gen lr = "Left" if left == 1
replace lr = "Right" if left == 0

gen cntry_order = .
* Sort countries alphabetically for consistent y-axis
gen countryname = ""
replace countryname = "Argentina 1946"  if iso == "ARG" & year == 1946
replace countryname = "Argentina 1973"  if iso == "ARG" & year == 1973
replace countryname = "Bolivia 1952"    if iso == "BOL"
replace countryname = "Brazil 1951"     if iso == "BRA"
replace countryname = "Chile 1952"      if iso == "CHL"
replace countryname = "Ecuador 1952"    if iso == "ECU" & year == 1952
replace countryname = "Ecuador 1960"    if iso == "ECU" & year == 1960
replace countryname = "Ecuador 1968"    if iso == "ECU" & year == 1968
replace countryname = "India 1966"      if iso == "IND"
replace countryname = "Mexico 1970"     if iso == "MEX"
replace countryname = "Peru 1990"       if iso == "PER"
replace countryname = "Turkey 2003"     if iso == "TUR"
replace countryname = "Venezuela 1999"  if iso == "VEN"

* y position
gen ypos = 14 - _n + 1

twoway ///
    (scatter ypos year if left == 1 & auth_strict == 1, ///
             msymbol(square) mcolor(black) msize(medlarge) ///
             mlabel(leader) mlabsize(vsmall) mlabgap(small)) ///
    (scatter ypos year if left == 0 & auth_strict == 1, ///
             msymbol(diamond) mcolor(black) msize(medlarge) ///
             mlabel(leader) mlabsize(vsmall) mlabgap(small)) ///
    (scatter ypos year if left == 1 & auth_broad == 1 & auth_strict == 0, ///
             msymbol(square_hollow) mcolor(gs5) msize(medlarge) ///
             mlabel(leader) mlabsize(vsmall) mlabgap(small)) ///
    (scatter ypos year if left == 0 & auth_broad == 1 & auth_strict == 0, ///
             msymbol(diamond_hollow) mcolor(gs5) msize(medlarge) ///
             mlabel(leader) mlabsize(vsmall) mlabgap(small)) ///
    , ///
    xlabel(1940 1950 1960 1970 1980 1990 2000 2010, labsize(small)) ///
    ylabel(none) ///
    xtitle("Takeover year", size(small)) ytitle("") ///
    legend(order(1 "Strict, left" 2 "Strict, right" ///
                 3 "Broad-only, left" 4 "Broad-only, right") ///
           rows(2) size(small) region(lcolor(white))) ///
    title("Authoritarian-populist episodes, 1940-2010", size(medsmall)) ///
    graphregion(color(white)) scheme(s1mono) xsize(14) ysize(8)
gr export "figures/FigureAP2.pdf", replace

clear

}

/*===========================================================================
  TABLE AP1 — Episode Lists (strict and broad)
  Method: manually curated descriptive table (no statistical output).
  Lists the authpop subset episodes with country, year, leader, ideology,
  and regime classification.
===========================================================================*/

capture {

* --- Strict subset (N=9) ---
file open t using "tables/TableAP1_strict.tex", write replace text
file write t `"\def\sym#1{\ifmmode^{#1}\else\(\^{#1}\)\fi}"' _n
file write t `"\setlength{\tabcolsep}{5pt}"' _n
file write t `"{\footnotesize"' _n
file write t `"\begin{longtable}{rp{2.2cm}p{2.4cm}p{3.2cm}p{3.8cm}}"' _n
file write t `"\caption*{\textbf{Table AP1a---Authoritarian-Populist Episodes (Strict Subset), N=9}} \\"' _n
file write t `"\hline\hline"' _n
file write t `"No. & Country & Year & Leader & Regime at takeover \\"' _n
file write t `"\hline"' _n
file write t `"\endfirsthead"' _n
file write t `"\hline\hline"' _n
file write t `"No. & Country & Year & Leader & Regime at takeover \\"' _n
file write t `"\hline"' _n
file write t `"\endhead"' _n
file write t `"\hline"' _n
file write t `"\endfoot"' _n
file write t `"1. & Argentina & 1946 & Per\'on (L) & Electoral autocracy (V-Dem=1) \\"' _n
file write t `"2. & Argentina & 1973 & C\'ampora/Per\'on (L) & Electoral autocracy (V-Dem=1) \\"' _n
file write t `"3. & Bolivia & 1952 & Estenssoro (L) & Electoral autocracy (V-Dem=1) \\"' _n
file write t `"4. & Brazil & 1951 & Vargas (L) & Electoral autocracy (V-Dem=1) \\"' _n
file write t `"5. & Chile & 1952 & Ib\'a\~nez (L) & Electoral autocracy (V-Dem=1) \\"' _n
file write t `"6. & Ecuador & 1952 & Velasco Ibarra I (R) & Electoral autocracy (V-Dem=1) \\"' _n
file write t `"7. & Ecuador & 1960 & Velasco Ibarra II (R) & Electoral autocracy (V-Dem=1) \\"' _n
file write t `"8. & Ecuador & 1968 & Velasco Ibarra III (R) & Electoral autocracy (V-Dem=1) \\"' _n
file write t `"9. & Mexico & 1970 & Echeverr\'ia (L) & Electoral autocracy (PRI hegemonic; V-Dem=1) \\"' _n
file write t `"\hline\hline"' _n
file write t `"\multicolumn{5}{p{14cm}}{\textit{Notes:} Strict subset = FST populist episodes in electoral or closed autocracies at the takeover year (V-Dem \texttt{v2x\_regime} $\leq 1$). L = left-wing, R = right-wing (per FST coding). Ecuador comprises three non-consecutive spells of the same leader; see robustness checks.} \\"' _n
file write t `"\end{longtable}"' _n
file write t `"}"' _n
file close t

* --- Broad subset (N=13) ---
file open t using "tables/TableAP1_broad.tex", write replace text
file write t `"\def\sym#1{\ifmmode^{#1}\else\(\^{#1}\)\fi}"' _n
file write t `"\setlength{\tabcolsep}{5pt}"' _n
file write t `"{\footnotesize"' _n
file write t `"\begin{longtable}{rp{2.2cm}p{2.4cm}p{3.2cm}p{3.8cm}}"' _n
file write t `"\caption*{\textbf{Table AP1b---Authoritarian-Populist Episodes (Broad Subset), N=13}} \\"' _n
file write t `"\hline\hline"' _n
file write t `"No. & Country & Year & Leader & Auth. classification \\"' _n
file write t `"\hline"' _n
file write t `"\endfirsthead"' _n
file write t `"\hline\hline"' _n
file write t `"No. & Country & Year & Leader & Auth. classification \\"' _n
file write t `"\hline"' _n
file write t `"\endhead"' _n
file write t `"\hline"' _n
file write t `"\endfoot"' _n
file write t `"\multicolumn{5}{l}{\textit{Panel A. Strict subset (autocracy at takeover, N=9)}} \\"' _n
file write t `"\hline"' _n
file write t `"1. & Argentina & 1946 & Per\'on (L) & Electoral autocracy at takeover (V-Dem=1) \\"' _n
file write t `"2. & Argentina & 1973 & C\'ampora/Per\'on (L) & Electoral autocracy at takeover (V-Dem=1) \\"' _n
file write t `"3. & Bolivia & 1952 & Estenssoro (L) & Electoral autocracy at takeover (V-Dem=1) \\"' _n
file write t `"4. & Brazil & 1951 & Vargas (L) & Electoral autocracy at takeover (V-Dem=1) \\"' _n
file write t `"5. & Chile & 1952 & Ib\'a\~nez (L) & Electoral autocracy at takeover (V-Dem=1) \\"' _n
file write t `"6. & Ecuador & 1952 & Velasco Ibarra I (R) & Electoral autocracy at takeover (V-Dem=1) \\"' _n
file write t `"7. & Ecuador & 1960 & Velasco Ibarra II (R) & Electoral autocracy at takeover (V-Dem=1) \\"' _n
file write t `"8. & Ecuador & 1968 & Velasco Ibarra III (R) & Electoral autocracy at takeover (V-Dem=1) \\"' _n
file write t `"9. & Mexico & 1970 & Echeverr\'ia (L) & Electoral autocracy (PRI hegemonic; V-Dem=1) \\"' _n
file write t `"\hline"' _n
file write t `"\multicolumn{5}{l}{\textit{Panel B. Additional broad episodes (became autocratic during tenure, N=4)}} \\"' _n
file write t `"\hline"' _n
file write t `"10. & India & 1966 & Gandhi I.\ (L) & Elected in democracy; Emergency 1975 (V-Dem=1) \\"' _n
file write t `"11. & Peru & 1990 & Fujimori (R) & Elected in democracy; autogolpe 1992 (V-Dem=0) \\"' _n
file write t `"12. & Turkey & 2003 & Erdo\u{g}an (R) & Elected in democracy; democratic backsliding 2013 (V-Dem=1) \\"' _n
file write t `"13. & Venezuela & 1999 & Ch\'avez (L) & Elected in democracy; competitive authoritarianism 2002 (V-Dem=1) \\"' _n
file write t `"\hline\hline"' _n
file write t `"\multicolumn{5}{p{14cm}}{\textit{Notes:} Broad subset adds four episodes where the populist took power in a democracy but V-Dem \texttt{v2x\_regime} subsequently fell to $\leq 1$ during their tenure. L = left-wing, R = right-wing (per FST coding).} \\"' _n
file write t `"\end{longtable}"' _n
file write t `"}"' _n
file close t

clear

}

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

capture {
    authpop_fig3 auth_strict  "Strict (N=9)"   "figures/FigureAP3_strict.pdf"
    authpop_fig3 auth_broad   "Broad (N=13)"   "figures/FigureAP3_broad.pdf"
}

* No-Ecuador robustness: create dataset, then call authpop_fig3 with it

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
  TABLE AP3 — run separately via tableap3_authpop.do
  (synth requires a fresh Stata session; see run_authpop.bat STEP 2b)
===========================================================================*/

di "figtabs_authpop.do complete."
