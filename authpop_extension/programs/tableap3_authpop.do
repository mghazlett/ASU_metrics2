/*===========================================================================
  tableap3_authpop.do
  TABLE AP3 — Predictor Balance (Synthetic Control)

  Run in a fresh Stata session (after figtabs_authpop.do) to avoid mata
  state issues (emptymat() unavailability) that arise when synth runs in
  the same session as FigureAP3/AP4/AP5.

  Input:  data/authpop_dataset.dta
  Output: tables/TableAP3_strict.tex
          tables/TableAP3_broad.tex

  Run from: C:\PLE\authpop_extension\
===========================================================================*/

cd "C:/PLE/authpop_extension"
clear all
set more off
cap mkdir data/_work
cap mkdir tables
local tmp "data/_work/"

adopath ++ "../programs/adosw"
mata: mata mlib index

/*===========================================================================
  TABLE AP3 — Predictor Balance (Synthetic Control)
  Method: Stata synth command. For each authpop episode, runs synth with
  pre-treatment GDP + institutions + inflation + crisis dummies, extracts
  e(X_balance) matrix (Treated / Synthetic / DonorPool means). Aggregates
  across episodes to produce a summary balance table.
  Mirrors FST Table 3. NOTE: synth uses Stata's built-in solver, not scpi.
===========================================================================*/

foreach subset in strict broad {

use data/authpop_dataset, clear

* Episode list for this subset
preserve
keep if atakeover_`subset' == 1
keep cid year
sort cid year
gen i = _n
local nep = _N
forvalues k = 1/`nep' {
    local tc_`k' = cid[`k']
    local ty_`k' = year[`k']
}
restore

gen var = log(fstgdp)
rename institutions i
rename inflation    r
rename bankcrisis   c
rename debtcrisis   s

gen simultake = 0
foreach k of numlist 1/`nep' {
    replace simultake = 1 if cid == `tc_`k'' & year == `ty_`k''
}

* Collect balance matrices
local nbal = 0

foreach i of numlist 1/`nep' {

    preserve

    gen byte b = 1 if year == `ty_`i''

    * Meciar 1990 (cid=47) has shorter pre-period; use 5-year window
    if `tc_`i'' == 47 {
        bys cid (b): keep if year >= year[1] - 5 & year <= year[1] + 15
    }
    else {
        bys cid (b): keep if year >= year[1] - 15 & year <= year[1] + 15
    }

    bys cid (b): gen ti = year - year[1] + 15

    gen bsimul = 1 if b == 1 & simultake == 1 & cid != `tc_`i''
    bys cid: egen msimul = max(bsimul)
    bys cid: drop if msimul == 1

    foreach x in var i r c s {
        bys cid: ipolate `x' year, gen(ipo_`x')
        replace `x' = ipo_`x' if ipo_`x' != . & `x' == .
        gsort cid -year
        bys cid: replace `x' = `x'[_n-1] if `x' == .
    }

    bys cid (b): gen d = var - var[1]

    foreach z in i {
        bys cid (b): gen d`z' = `z' - `z'[1]
        bys cid (d`z'): drop if missing(d`z'[_N]) & cid != `tc_`i''
        bys year (d`z'): drop if missing(d`z'[_N])
        replace `z' = d`z'
    }

    foreach k in c s {
        bys cid: egen `k'yn = max(`k') if (year < `ty_`i'') & (year >= (`ty_`i'' - 5))
        bys cid: egen m`k'yn = max(`k'yn)
    }

    foreach j in r d mcyn msyn {
        bys cid (`j'): drop if missing(`j'[_N]) & cid != `tc_`i''
        bys year (`j'): drop if missing(`j'[_N])
    }

    xtset cid ti

    if `tc_`i'' != 47 {
        cap synth d d(0) d(1) d(2) d(3) d(4) d(5) d(6) d(7) d(8) d(9) d(10) d(11) d(12) d(13) d(14) ///
            i(0) i(1) i(2) i(3) i(4) i(5) i(6) i(7) i(8) i(9) i(10) i(11) i(12) i(13) i(14) ///
            r(0) r(1) r(2) r(3) r(4) r(5) r(6) r(7) r(8) r(9) r(10) r(11) r(12) r(13) r(14) ///
            mcyn(14) msyn(14), ///
            trperiod(15) trunit(`tc_`i'') keep(`tmp'_ap3bal, replace) unitnames(country)
    }
    else {
        cap synth d d(10) d(11) d(12) d(13) d(14) ///
            i(10) i(11) i(12) i(13) i(14) ///
            r(11) r(12) r(13) r(14) mcyn(14) msyn(14), ///
            trperiod(15) trunit(`tc_`i'') keep(`tmp'_ap3bal, replace) unitnames(country)
    }
    cap {
        matrix matbal = e(X_balance)
        svmat matbal
        rename matbal1 Treated_`i'
        rename matbal2 Synthetic_`i'
        keep Treated_`i' Synthetic_`i'
        keep if _n <= 47
        gen row = _n
        save `tmp'_ap3_bal_`i', replace
        local ++nbal
    }

    restore
}

* Aggregate balance across episodes: mean of Treated and Synthetic columns
if `nbal' > 0 {
    use `tmp'_ap3_bal_1, clear
    forvalues i = 2/`nep' {
        cap merge 1:1 row using `tmp'_ap3_bal_`i', nogen
    }

    * Average across episodes
    egen Treated_avg   = rowmean(Treated_*)
    egen Synthetic_avg = rowmean(Synthetic_*)

    * Predictor labels (15 GDP + 15 Institutions + 15 Inflation + 1 Banking + 1 Debt = 47 rows)
    gen Predictor = ""
    forvalues l = 0/14 {
        replace Predictor = "GDP t-" + string(14-`l') if row == `l' + 1
    }
    forvalues l = 0/14 {
        replace Predictor = "Institutions t-" + string(14-`l') if row == `l' + 16
    }
    forvalues l = 0/14 {
        replace Predictor = "Inflation t-" + string(14-`l') if row == `l' + 31
    }
    replace Predictor = "Banking crises (pre-5y)" if row == 46
    replace Predictor = "Debt crises (pre-5y)"    if row == 47

    * Export as LaTeX
    keep Predictor Treated_avg Synthetic_avg
    drop if missing(Predictor)

    file open t3 using "tables/TableAP3_`subset'.tex", write replace text
    file write t3 `"\begin{tabular}{lcc}"' _n
    file write t3 `"\hline\hline"' _n
    file write t3 `"Predictor & Treated & Synthetic \\"' _n
    file write t3 `"\hline"' _n
    forvalues r = 1/`=_N' {
        local pred = Predictor[`r']
        local tr   = string(round(Treated_avg[`r'],   0.001), "%8.3f")
        local sy   = string(round(Synthetic_avg[`r'], 0.001), "%8.3f")
        file write t3 "`pred' & `tr' & `sy' \\" _n
    }
    file write t3 `"\hline\hline"' _n
    file write t3 `"\multicolumn{3}{p{10cm}}{\textit{Notes:} Average across all `nep' episodes in the `subset' subset. Predictor balance from Stata \texttt{synth} command. Treated = actual pre-treatment values; Synthetic = weighted-donor values.} \\"' _n
    file write t3 `"\end{tabular}"' _n
    file close t3
}

clear

}

di "tableap3_authpop.do complete."
