clear
graph set window fontface "Times New Roman"

*--------------------------------------------------------------------------------------------------------------------------------------------------------------
*
* Figures 1 and 2
*
*--------------------------------------------------------------------------------------------------------------------------------------------------------------

capture {
	
use data/ple_dataset, clear

bysort cid : gen order = _n
by cid: gen last = _n == _N
expand 3 if last
sort iso year
bysort iso : replace year = 2020 if year == 2019 & year[_n-1] == 2019
bysort iso : replace year = 2021 if year == 2019 & year[_n-1] == 2020
replace pop =. if year == 2021
replace pop =0 if year == 2020 & (iso=="GRC" | iso=="BOL")
replace lpop =0 if year == 2020 & (iso=="GRC" | iso=="BOL")
foreach v in independent pop lpop rpop {
egen `v'by = sum(`v'), by(year)
}
gen sh = popby/independentby * 100
gen shl = lpopby/independentby * 100
gen shr = rpopby/independentby * 100
gen zero=0
gen lrstr ="Left-wing populism" if lpop==1
replace lrstr ="Right-wing populism" if rpop==1
egen pop_id = max(pop == 1), by(country)
replace pop=1 if year==2021 & pop_id==1
replace lrstr ="2021" if year==2021 & pop_id==1

gen populists = ""
replace populists="Yrigoyen, the Peróns, Menem, the Kirchners"   if year==2021 & country=="Argentina"
replace populists="Estenssoro/Zuazo (MNR), Morales"              if year==2021 & country=="Bolivia"
replace populists="Vargas, Collor, Bolsonaro"        			 if year==2021 & country=="Brazil"
replace populists="Borisov"                                      if year==2021 & country=="Bulgaria"
replace populists="Alessandri/Ibáñez"                            if year==2021 & country=="Chile"
replace populists="Ibarra, Bucaram, Correa"                      if year==2021 & country=="Ecuador"
replace populists="Hitler"                                       if year==2021 & country=="Germany"
replace populists="Tsipras"                                      if year==2021 & country=="Greece"
replace populists="Orbán"                                        if year==2021 & country=="Hungary"
replace populists="I. Gandhi, Modi"                              if year==2021 & country=="India"
replace populists="Sukarno, Widodo"                              if year==2021 & country=="Indonesia"
replace populists="Netanyahu      "                              if year==2021 & country=="Israel"
replace populists="Mussolini, Berlusconi, Lega/M5S"              if year==2021 & country=="Italy"
replace populists="Koizumi"                                      if year==2021 & country=="Japan"
replace populists="Cárdenas, Echeverría, Obrador"                if year==2021 & country=="Mexico"
replace populists="García, Fujimori "                            if year==2021 & country=="Peru"
replace populists="Estrada, Duterte"                             if year==2021 & country=="Philippines"
replace populists="Kaczyńskis, PiS"                              if year==2021 & country=="Poland"
replace populists="Mečiar, Fico"                                 if year==2021 & country=="Slovakia"
replace populists="Roh"                                          if year==2021 & country=="South Korea"
replace populists="Chen"                                         if year==2021 & country=="Taiwan"
replace populists="T. Shinawatra"                                if year==2021 & country=="Thailand"
replace populists="Erdogan"                                      if year==2021 & country=="Turkey"
replace populists="Trump"                                        if year==2021 & country=="United States"
replace populists="Johnson"                                      if year==2021 & country=="United Kingdom"
replace populists="Chávez/Maduro"                                if year==2021 & country=="Venezuela"
replace populists="Zuma          "                               if year==2021 & country=="South Africa"
replace populists="Muldoon       "                               if year==2021 & country=="New Zealand"

tw (rarea shl sh year, color(gs5) lcolor(gs6) lp(solid) lwidth(vvthin) plotr(m(vsmall)) sort)  ///
   (rarea shl zero year, col(gs11) lcolor(gs11) lp(solid) lp(solid) lwidth(vvthin) plotr(m(vsmall)) sort) ///
   (line sh year, lcolor(red) lp(solid) lwidth(thick) plotr(m(vsmall)) sort) if year>=1900 & year <=2020, ///
     xlabel(1900 1920 1940 1960 1980 2000 2020, labsize(medsmall)) ylabel(0 5 10 15 20 25, labsize(medsmall) nogrid) ///
	 xtitle("", size(medsmall) margin(medium)) ytitle("Share of independent countries with populist government (%)", ///
	 size(small) margin(medium)) scheme(s1mono) graphregion(color(white) lcolor(white) margin(b-3 l-5)) ysize(6) xsize(9) ///
	 legend(rows(1) order(3 "Populist governments" 1 "Right-wing populism" 2 "Left-wing populism") ///
	 symxsize(*0.3) symysize(*0.3) region(lwidth(none)) forcesize size(medlarge)) 
	 gr export figures/Figure1.pdf, replace

stripplot year if (pop==1) & year>=1900, over(country) separate(lrstr) yscale(titlegap(2) reverse) xscale(titlegap(2)) /// 
          ylabel(,labsize(small) labgap(med)) ytitle("", size(medlarge)) xlabel(1900 1920 1940 1960 1980 2000 2020,labsize(small)) ///
          xtitle("", size(medlarge)) ysize(10) xsize(16) scheme(s2mono) graphregion(color(white) margin(l+0 r+40 b-1 t-1)) /// 
          legend(region(color(white)) order(3 2)) msymbol(none S S) mcolor(none gs11 gs8) msize(zero 1.675 1.675) ///
          mlabel(populists) mlabsize(vsmall) mlabgap(large) centre  
          gr export figures/Figure2.pdf, replace

clear

}

*--------------------------------------------------------------------------------------------------------------------------------------------------------------
*
* Figure 3
*
*--------------------------------------------------------------------------------------------------------------------------------------------------------------
	
capture {
	  
use data/ple_dataset, clear

gen treatedyea_1 = 1946 // Peron 1946
gen treatedcid_1 = 1 // Peron 1946
gen treatedyea_2 = 1973 // Peron 1973
gen treatedcid_2 = 1 // Peron 1973
gen treatedyea_3 = 1989 // Menem 1989
gen treatedcid_3 = 1 // Menem 1989
gen treatedyea_4 = 2003 // Kirchner 2003
gen treatedcid_4 = 1 // Kirchner 2003
gen treatedyea_5 = 1952 // Estenssoro 1952
gen treatedcid_5 = 5 // Estenssoro 1952
gen treatedyea_6 = 1951 // Vargas 1951
gen treatedcid_6 = 6 // Vargas 1951
gen treatedyea_7 = 1990 // Collor 1990
gen treatedcid_7 = 6 // Collor 1990
gen treatedyea_8 = 1952 // Ibanez 1952
gen treatedcid_8 = 9 // Ibanez 1952
gen treatedyea_9 = 1952 // Velasco 1952
gen treatedcid_9 = 16 // Velasco 1952
gen treatedyea_10 = 1960 // Velasco 1960
gen treatedcid_10 = 16 // Velasco 1960
gen treatedyea_11 = 1968 // Velasco 1968
gen treatedcid_11 = 16 // Velasco 1968
gen treatedyea_12 = 1996 // Bucaram 1996
gen treatedcid_12 = 16 // Bucaram 1996
gen treatedyea_13 = 1966 // Gandhi 1966
gen treatedcid_13 = 25 // Gandhi 1966
gen treatedyea_14 = 1996 // Netanyahu 1996
gen treatedcid_14 = 28 // Netanyahu 1996
gen treatedyea_15 = 1994 // Berlusconi 1994
gen treatedcid_15 = 29 //  Berlusconi 1994
gen treatedyea_16 = 2001 // Berlusconi 2001
gen treatedcid_16 = 29 //  Berlusconi 2001
gen treatedyea_17 = 2001 // Koizumi 2001
gen treatedcid_17 = 30 //  Koizumi 2001
gen treatedyea_18 = 1970 // Echeverria 1970
gen treatedcid_18 = 36 //  Echeverria 1970
gen treatedyea_19 = 1975 // Muldoon 1975
gen treatedcid_19 = 38 //  Muldoon 1975
gen treatedyea_20 = 1985 // Garcia 1985
gen treatedcid_20 = 41 //  Garcia 1985
gen treatedyea_21 = 1990 // Fujimori 1990
gen treatedcid_21 = 41 //   Fujimori 1990
gen treatedyea_22 = 1998 // Estrada 1998
gen treatedcid_22 = 42 //  Estrada 1998
gen treatedyea_23 = 1990 // Meciar 1990
gen treatedcid_23 = 47 //   Meciar 1990
gen treatedyea_24 = 2003 // Roh 2003
gen treatedcid_24 = 50 //   Roh 2003
gen treatedyea_25 = 2000 // Chen 2000
gen treatedcid_25 = 54 //   Chen 2000
gen treatedyea_26 = 2001 // Shinawatra 2001
gen treatedcid_26 = 55 //   Shinawatra 2001
gen treatedyea_27 = 2003 // Erdogan 2003
gen treatedcid_27 = 56 //   Erdogan 2003
gen treatedyea_28 = 1999 // Chavez 1999
gen treatedcid_28 = 60 //   Chavez 1999

gen treatedyea_29 = 1928 // Yrigoyen1928 
gen treatedcid_29 =  1 // Yrigoyen1928 
gen treatedyea_30 = 1920 // Alessandri1920 	
gen treatedcid_30 =  9 // Alessandri1920 				   
gen treatedyea_31 = 1934   // Velasco1934
gen treatedcid_31 =  16 // Velasco1934 				   
gen treatedyea_32 = 1933 // Hitler1933 	
gen treatedcid_32 =  21 // Hitler1933 					   
gen treatedyea_33 = 2014  // Widodo2014 
gen treatedcid_33 =  26 // Widodo2014 				   
gen treatedyea_34 = 1922 // Mussolini1922 
gen treatedcid_34 =  29 // Mussolini1922 				   
gen treatedyea_35 = 2019 // Johnson2019 
gen treatedcid_35 =  57 // Johnson2019 				   
gen treatedyea_36 = 2006  // Morales2006 
gen treatedcid_36 =  5 // Morales2006 				         
gen treatedyea_37 = 1930 // Vargas1930
gen treatedcid_37 =  6 // Vargas1930 					             
gen treatedyea_38 = 2019 // Bolsonaro2019 	
gen treatedcid_38 = 6 // Bolsonaro2019 				    
gen treatedyea_39 = 2009 // Borisov2009 
gen treatedcid_39 =  7 // Borisov2009  					   
gen treatedyea_40 = 2007 // Correa2007
gen treatedcid_40 =  16 // Correa2007 						   
gen treatedyea_41 = 2015 // Tsipras2015 
gen treatedcid_41 =  22 // Tsipras2015 				   					   
gen treatedyea_42 = 2010 // Orban2010 
gen treatedcid_42 =  23 // Orban2010  						    
gen treatedyea_43 = 2014 // Modi2014 
gen treatedcid_43 =  25 // Modi2014 			   
gen treatedyea_44 = 2009 // Netanyahu2009 
gen treatedcid_44 =  28 // Netanyahu2009 		   
gen treatedyea_45 = 2018 // Lega2018
gen treatedcid_45 =  29 // Lega2018 					   
gen treatedyea_46 = 1934 // Cardenas1934
gen treatedcid_46 =  36 // Cardenas1934 	
gen treatedyea_47 = 2018 // Obrador2018
gen treatedcid_47 =  36 // Obrador2018 				   
gen treatedyea_48 = 2016 // Duterte2016 
gen treatedcid_48 =  42 // Duterte2016 
gen treatedyea_49 = 2005 // Kaczynski2005
gen treatedcid_49 =  43 // Kaczynski2005 						   					    
gen treatedyea_50 = 2015 // Pis2015 	
gen treatedcid_50 =  43 // Pis2015 			   
gen treatedyea_51 = 2006 // Fico2006 
gen treatedcid_51 =  47 // Fico2006 	
gen treatedyea_52 = 2009 // Zuma2009 
gen treatedcid_52 =  49 // Zuma2009 
gen treatedyea_53 = 2017 // Trump2017  
gen treatedcid_53 =  58 // Trump2017
					   
gen treatedyea_54 = 2002  // Duhalde2002 
gen treatedcid_54 = 1   //   Duhalde2002 				  
gen treatedyea_55 = 1961   // Goulart1961
gen treatedcid_55 = 6  //   Goulart1961 
gen treatedyea_56 = 1969   // Unclearnamegeisel1969
gen treatedcid_56 = 6  //   Unclearnamegeisel1969 
gen treatedyea_57 = 1985   // Sarney1985
gen treatedcid_57 = 6   //  Sarney1985  					   
gen treatedyea_58 = 1970   // Allende1970
gen treatedcid_58 = 9  //   Allende1970  					   
gen treatedyea_59 = 1990   // Tudman1990
gen treatedcid_59 = 12  //   Tudman1990  			   
gen treatedyea_60 = 1998   // Zeman1998
gen treatedcid_60 = 14  //   Zeman1998  					    
gen treatedyea_61 = 2006   // Topolanek2006
gen treatedcid_61 = 14  //   Topolanek2006  					   
gen treatedyea_62 = 2017  // Babis2017
gen treatedcid_62 = 14  //   Babis2017  
gen treatedyea_63 = 2003   // Gutierrez2003
gen treatedcid_63 = 16  //   Gutierrez2003  					    					     					   
gen treatedyea_64 = 2002   // Repsekalvitis2002
gen treatedcid_64 = 31  //   Repsekalvitis2002   					      					   
gen treatedyea_65 = 1976   // Portillo1976
gen treatedcid_65 = 36   //  Portillo1976  					   
gen treatedyea_66 = 2003  // Duarte2003
gen treatedcid_66 = 40  //   Duarte2003  					   
gen treatedyea_67 = 2008  // Lugo2008
gen treatedcid_67 = 40  //   Lugo2008  					     					     					   
gen treatedyea_68 = 1963  // Belaundealvaradob1963
gen treatedcid_68 = 41  //   Belaundealvaradob1963  					   
gen treatedyea_69 = 2006  // Garcia2006 
gen treatedcid_69 = 41  //   Garcia2006  				   
gen treatedyea_70 = 2011  // Humala2011
gen treatedcid_70 = 41  //   Humala2011  					   
gen treatedyea_71 = 1953  // Magsaysay1953
gen treatedcid_71 = 42  //   Magsaysay1953  					   
gen treatedyea_72 = 2001  // Arroyo2001
gen treatedcid_72 = 42   //  Arroyo2001  					   
gen treatedyea_73 = 1990  // Walesa1990
gen treatedcid_73 = 43   //  Walesa1990  					   
gen treatedyea_74 = 2004  // Basescu2004
gen treatedcid_74 = 45  //   Basescu2004  					   
gen treatedyea_75 = 2000  // Putin2000
gen treatedcid_75 = 46  //   Putin2000  					   
gen treatedyea_76 = 2004 // Jansa2004
gen treatedcid_76 = 48  //   Jansa2004   					   
gen treatedyea_77 = 2012  // Jansa2012
gen treatedcid_77 = 48  //   Jansa2012  					   
gen treatedyea_78 = 2011  // Yingluck2011
gen treatedcid_78 = 55  //   Yingluck2011  					    					   
gen treatedyea_79 = 1974  // Perez1974
gen treatedcid_79 = 60  //   Perez1974  
gen treatedyea_80 = 1994 // Caldera1994
gen treatedcid_80 = 60  //   Caldera1994 

forvalues i=1/80{
label var treatedcid_`i' "Treated country ID of populist case `i'"
label var treatedyea_`i' "Treatment year of populist case `i'"
	}

gen at = 0 
gen lrgdppc = log(fstgdp)
gen rgdppc_gr = (lrgdppc - l1.lrgdppc) * 100

forvalues i = 1/28 {
preserve
	keep if cid==treatedcid_`i'
	replace at=1 if year==treatedyea_`i'  
	forvalues h = 1/5 {
	gen at`h' = (l`h'.at)
	}
gen atd = at1+at2+at3+at4+at5
drop if year<=1945
egen meanrgdppc_gr = mean(rgdppc_gr)
gen ggap = rgdppc_gr - meanrgdppc_gr if atd==1
keep if atd==1
cap save _t_`i'_c5, replace 
restore

preserve
keep if cid==treatedcid_`i'
replace at=1 if year==treatedyea_`i'  
forvalues h = 1/15 {
gen at`h' = (l`h'.at)
}
gen atd = at1+at2+at3+at4+at5+at6+at7+at8+at9+at10+at11+at12+at13+at14+at15
drop if year<=1945
egen meanrgdppc_gr = mean(rgdppc_gr)
gen ggap = rgdppc_gr - meanrgdppc_gr if atd==1
keep if atd==1
cap save _t_`i'_c15, replace 
restore

preserve
replace at=1 if year==treatedyea_`i'
forvalues h = 1/5 {
	gen at`h' = (l`h'.at)
	}
		gen atd = at1+at2+at3+at4+at5
drop if atd!=1
replace atd=0
replace atd=1 if cid==treatedcid_`i'
bys year : egen meanrgdppc_gr = mean(rgdppc_gr)
gen ggap = rgdppc_gr - meanrgdppc_gr if atd==1
keep if atd==1
cap save _t_`i'_g5, replace 
restore

preserve
replace at=1 if year==treatedyea_`i'
forvalues h = 1/15 {
	gen at`h' = (l`h'.at)
	}
gen atd = at1+at2+at3+at4+at5+at6+at7+at8+at9+at10+at11+at12+at13+at14+at15
drop if atd!=1
replace atd=0
replace atd=1 if cid==treatedcid_`i'
bys year : egen meanrgdppc_gr = mean(rgdppc_gr)
gen ggap = rgdppc_gr - meanrgdppc_gr if atd==1
keep if atd==1
cap save _t_`i'_g15, replace 
restore
}

foreach s in c5 c15 g5 g15 {
				cap use _t_1_`s', clear
							forvalues i = 2/28 {
								cap app using _t_`i'_`s'
												}
			egen mggap = mean(ggap)
			collapse mggap
			gen spec="`s'"
			save _x_`s', replace
}
 
use _x_c5, clear
app using _x_c15
app using _x_g5
app using _x_g15
cap !del _*
cap !rm _*.dta

foreach n in 5 15 {
	gen avgc`n' = mggap  if spec=="c`n'"
	gen avgy`n' = mggap  if spec=="g`n'"
	replace spec="`n' years" if spec=="c`n'"
	replace spec="`n' years" if spec=="g`n'"
}

rename (avgc5 avgy5) (Country_level Global_level)  
replace Country_level = avgc15 if avgc15!=.
replace Global_level = avgy15 if avgy15!=.
gen order = _n

graph bar (mean) Country_level Global_level, over(spec, sort(order) lab(nolab) axis(off)) bargap(5) yline(0.00, noextend lcolor(black) ///
lwidth(thick) lstyle(foreground)) bar(1, color(white) lcolor(black) lwidth(medthick)) bar(2, color(gs12) lcolor(black) lwidth(medthick)) ///
ylab(0 "0 pp" -.2 "-0.2 pp" -.4 "-0.4 pp" -.6 "-0.6 pp" -.8 "-0.8 pp" -1.0 "-1.0 pp" -1.2 "-1.2 pp" -1.4 "-1.4 pp", nogrid labsize(medsmall) angle(0))  ///
legend(rows(2) order(1 "Gap to country level" 2 "Gap to global level")  symxsize(*0.6) symysize(*0.8) region(lcolor(white)) size(medsmall) ) ///
title("Annualized growth gap                   Annualized growth gap", margin(zero) size(medsmall) color(black)) graphregion(color(white)) ///
subtitle("(5-year aftermath)                            (15-year aftermath)", margin(vsmall) size(medsmall) color(black)) 
gr export figures/Figure3.pdf, replace
 
clear

} 

*--------------------------------------------------------------------------------------------------------------------------------------------------------------
*
*	Figure 4 and Table C4
*
*--------------------------------------------------------------------------------------------------------------------------------------------------------------

capture { 
	
use data/ple_dataset, clear

xtset cid year
gen lrgdppc = log(fstgdp) if year>=1946 

forvalues h = 1/15 {
	gen rgdppc_gr`h' = (f`h'.lrgdppc - lrgdppc) * 100
	label var rgdppc_gr`h' "Y `h'"
	}
	
gen grlrgdppc = (lrgdppc - L1.lrgdppc) * 100
egen mgrlrgdppc = mean(grlrgdppc), by(year)

rename placebo np
rename atakeover ap
rename ltakeover le
rename rtakeover ri

forvalues h = 1/15 {
	replace np =  0 if F`h'.ap ==1
	replace np =  0 if L`h'.ap ==1
	}

gen years = _n-1 if(_n<=15+1) 
gen zero=0

foreach t in np ap le ri {
foreach v in irf se up lo {
	gen `v'_`t' = 0 
	}
	}

xtreg rgdppc_gr15 np ap le ri L(1/5).institutions L(1/5).mgrlrgdppc L(1/5).grlrgdppc L(1/5).bankcrisis L(1/5).debtcrisis L(1/5).inflation, fe
gen regsample = 1 if e(sample)

forvalues h = 1/15 {
	xtreg rgdppc_gr`h' np ap L(1/5).institutions L(1/5).mgrlrgdppc L(1/5).grlrgdppc L(1/5).bankcrisis L(1/5).debtcrisis L(1/5).inflation if regsample==1, fe
    test np = ap 				 
	estadd scalar NonpPop_pdiff = r(p)
	lincom _cons + np 
	replace irf_np = r(estimate) if _n==`h'+1 
	estadd scalar irf_np = r(estimate)  
	replace se_np = r(se) if _n==`h'+1 
	estadd scalar se_np = r(se)  
	lincom _cons + ap 
	replace irf_ap = r(estimate) if _n==`h'+1 
	estadd scalar irf_ap = r(estimate)  
	replace se_ap = r(se) if _n==`h'+1 
	estadd scalar se_ap = r(se) 
	estadd scalar R2= e(r2)
	estadd scalar Observations=e(N)
	eststo LP`h'
	}			
	
replace up_np = irf_np + 1.96*se_np if _n <= 15+1
replace lo_np = irf_np - 1.96*se_np if _n <= 15+1
gen irf_ap_gap = irf_ap - irf_np

twoway (rarea up_np lo_np years, fcolor(gs12) lcolor(white) lpattern(solid)) (line irf_np years, lcolor(blue) lpattern(solid) lwidth(thick)) /// 
	   (line irf_ap years, lcolor(red) lpattern(shortdash) lwidth(vthick)) (line zero years, lcolor(black)) if years <=15, ///
	   ylabel(, nogrid) legend(rows(3) label(3 "All populists") label(2 "Trend in other years") label(1 "95% CI") order(3 2 1) ///
	   symxsize(*0.375) symysize(*0.375) size(small) region(lwidth(none))) xlabel(, labsize(medlarge)) ylabel(, labsize(medlarge) angle(0)) ///
	   title("Panel A: Projected trends" , color(black) size(large) margin(medium)) ytitle("Percent (100 × log)", size(medsmall) margin(small)) xtitle("", size(medsmall)) ///
	   graphregion(color(white)) plotregion(color(white)) subtitle("", size(small)) name(pl, replace) nodraw		   

foreach t in le ri {
forvalues h = 1/15 {
	xtreg rgdppc_gr`h' np `t' L(1/5).institutions L(1/5).mgrlrgdppc L(1/5).grlrgdppc L(1/5).bankcrisis L(1/5).debtcrisis L(1/5).inflation if regsample==1, fe
	lincom _cons + `t'
	replace irf_`t' = r(estimate) if _n==`h'+1 
	replace se_`t' = r(se) if _n==`h'+1  			
		}			
	gen irf_`t'_gap = irf_`t' - irf_np
	}

twoway (line irf_ap_gap years, lcolor(black) lpattern(solid) lwidth(thick)) (line irf_le_gap years, lcolor(black) lpattern(dash) lwidth(thick)) ///  
	   (line irf_ri_gap years, lcolor(black) lpattern(shortdash) lwidth(thick))  if years <=15, ///  
       legend(rows(3) label(3 "Right populist") label(2 "Left populist") label(1 "All populist") order(1 2 3) symxsize(*0.375) ///
	   symysize(*0.375) size(small) region(lwidth(none)))  subtitle("", size(small)) xlabel(, labsize(medlarge)) ///
	   ylabel(-15 "-15 pp" -10 "-10 pp" -5 "-5 pp" 0 "0 pp" +5 "+5 pp", labsize(medlarge) angle(0)) title("Panel B: Projected gap", ///
	   color(black) size(large) margin(medium))  xtitle("") ytitle("") graphregion(color(white)) name(pr, replace) nodraw
		   
gr combine pl pr, rows(1) iscale(0.75) graphregion(color(white) margin(l=1 r=3 t=1)) imargin(2 2) xsize(14) ysize(7)
gr export figures/Figure4.pdf, replace

esttab LP* using tables/TableC4.tex, replace label stats(irf_np se_np irf_ap se_ap R2 NonpPop_pdiff Observations, labels("Other years" " " "Populist" " " "R2") ///
	fmt(%9.2f %9.2f %9.2f %9.2f %9.3f %9.2f %9.0f) star(irf_np irf_ap)) varwidth(13) drop(*) nonum compress starlevels(* 0.1 ** 0.05 *** 0.01) nonotes

clear

}

*--------------------------------------------------------------------------------------------------------------------------------------------------------------
*
*	Figure 5, Table C1, Table C2, and Table C3
*
*--------------------------------------------------------------------------------------------------------------------------------------------------------------

capture { 
	
use data/ple_dataset, clear
 
egen ccode = group(iso)
tsset ccode year, yearly

gen lrgdp   = log(fstgdp)		                              
gen dlrgdp  = 100*d.lrgdp

forvalues i=1/15 {
	gen lrgdp`i' = 100*(f`i'.lrgdp - lrgdp)	
	label var lrgdp`i' "Y `i'"
}
	                                   
replace dlrgdp = dlrgdp[_n+1] if iso=="SVK" & year== 1990-5 
egen wdlrgdp = mean(dlrgdp), by(year)
gen core = 1 if (year >= 1946 & year<=2003)
replace core = . if placebo==0 & atakeover==0 

gen dinstitutions = d.institutions
replace institutions = dinstitutions

foreach r in bankcrisis currcrisis debtcrisis dlrgdp wdlrgdp institutions war gini unemployrate conflicts koftrade global inflation debtgdp {
qui tssmooth ma `r'_wma = `r',  weights(5 4 3 2 <1>) 
replace `r' = l1.`r'_wma
}

label var placebo "Non-populist"
label var atakeover "Populist"
label var dlrgdp "Growth rate"
label var wdlrgdp "World growth"
label var institutions "Institutional quality"
label var war "World war"
label var debtgdp "Debt/GDP"
label var conflicts "Social conflicts (polarization)"
label var gini "Income inequality (Gini)"
label var global "Financial openness"
label var koftrade "Trade openness"
label var unemployrate "Unemployment"
label var inflation "Inflation"
label var bankcrisis "Banking crisis"
label var currcrisis "Currency crisis"
label var debtcrisis "Sovereign debt crisis"

local type placebo atakeover 
foreach t of local type {
		gen b`t'0=0  
		gen se`t'0=0  
}

qui sum ccode , d
local c = r(max)
forvalues i =1/`c' {
	gen      dum`i'=0
	replace  dum`i'= 1 - 1/`c' if ccode==`i'
	replace  dum`i'=   - 1/`c' if ccode~=`i'
	}
	
local rhsall bankcrisis currcrisis debtcrisis dlrgdp wdlrgdp institutions war gini unemployrate conflicts koftrade global inflation debtgdp

	foreach v of local rhsall {
	qui	bys iso: sum `v' 
		replace `v' = `v' - r(mean)
		}

local rhs1 bankcrisis dlrgdp wdlrgdp institutions inflation war 
local rhs2 bankcrisis dlrgdp wdlrgdp institutions inflation war currcrisis debtcrisis  
local rhs3 bankcrisis dlrgdp wdlrgdp institutions inflation war debtgdp
local rhs4 bankcrisis dlrgdp wdlrgdp institutions inflation war conflicts 
local rhs5 bankcrisis dlrgdp wdlrgdp institutions inflation war gini
local rhs6 bankcrisis dlrgdp wdlrgdp institutions inflation war global
local rhs7 bankcrisis dlrgdp wdlrgdp institutions inflation war koftrade
local rhs8 bankcrisis dlrgdp wdlrgdp institutions inflation war currcrisis debtcrisis debtgdp conflicts gini global koftrade 
local rhs9 bankcrisis dlrgdp wdlrgdp institutions inflation war unemployrate

estimates clear
forvalues i = 1/9{
qui logit atakeover `rhs`i'' dum1-dum59 if core==1
qui eststo mar`i': margins, dydx(*) post 
qui logit atakeover `rhs`i'' dum1-dum59 if core==1
qui eststo mco`i': margins, post	
}

forvalues i = 1/9{
qui logit atakeover `rhs`i'' dum1-dum59 if core==1
qui predict phat`i' if e(sample)==1
qui gen _`i'_sample = 1 if e(sample)			
qui roctab atakeover phat`i' if phat`i'~=.
qui local AUC = `r(area)'
qui estadd local AUC "`:di %6.3f `AUC''"
qui local AUC_se = `r(se)'
qui estadd local AUC_se "`:di %6.3f `AUC_se´''"
qui local Observations = `r(N)'
qui estadd local Observations "`:di %4.0fc `Observations''"
qui est sto _`i' 
}	

esttab mar* using tables/TableC1.tex, drop(dum*) cell(b(fmt(3) star) se(fmt(3) par)) noobs stats() varwidth(32) eqlabels(none) mlabels(none) starlevels(* 0.1 ** 0.05 *** 0.01) label nonotes replace page prehead("\begin{tabular}{l*{9}{c}}") postfoot("")
esttab mco* using tables/TableC1.tex, cell(b(fmt(3) star) se(fmt(3) par)) noobs stats() varwidth(32) starlevels(* 0.1 ** 0.05 *** 0.01) nonumber label nonotes eqlabels(none) mlabels(none) append page postfoot("") prehead("")  
esttab _*  using tables/TableC1.tex, drop(*) stats(Observations AUC AUC_se r2_p, fmt(%4.3fc)) modelwidth(9) varwidth(32) label nonotes nonumber eqlabels(none) mlabels(none) append page prehead("") postfoot("\end{tabular}")

twoway (kdensity phat1 if atakeover==1, lpattern(dash) color(red) lwidth(medthick)) (kdensity phat1 if atakeover==0, color(blue) lwidth(thick)), ///
text(5.1 .125 "Distribution for control units", placement(e) color(blue) size()) text(1.6 .400 "Distribution for treated units", placement(e) color(red) size()) ///
title("") ylabel(, labsize(medlarge) angle(0)) xlabel(0(1)1, labsize(medlarge)) ytitle("Frequency") xtitle("Estimated probability of treatment") graphregion(color(white)) ///
title("Stage 1: Logit prediction" , color(black) size(large) margin(medium)) plotregion(lpattern(blank)) scheme(s1color) legend(off) nodraw name(stage1, replace) 

preserve
keep if phat1!=. & atakeover==1
keep phat1 year cid
sort phat1
egen medianprob = median(phat1)
gen predic = 1 if phat1 >=medianprob
gen unpred = 1 if phat1 <medianprob
tostring year cid, replace
gen ca = year + "_" + cid
keep ca predic unpred
cap save probabilities, replace
restore 

preserve
keep if phat1!=. & atakeover==1
keep phat1 country year
gen No = _n
rename (year country) (Treatment Country)
gen Probability = string(phat1, "%04.2f")
drop phat1
order No
*Table C2
texsave * using tables/TableC2.tex, title(TABLE C2) replace
restore

drop if phat1==.
egen newid = group(iso)
qui sum newid 
local c = r(max)

forvalues i =1/`c' {
	gen      d`i'=0
	replace  d`i'= 1 - 1/`c' if newid==`i'
	replace  d`i'=   - 1/`c' if newid~=`i'
	}

gen lessone = `c' - 1
qui sum lessone 
local g = r(mean)

reg lrgdp1 atakeover placebo `rhs1' d1-d`g' phat1 if _1_sample==1, noconstant cluster(iso) 	
		
	foreach v of local rhs1 {
		bys iso: sum `v' if e(sample)==1
		gen `v'a = `v' - r(mean)
		}
				
gen invwt = atakeover/phat1 + (1-atakeover )/(1-phat1) if phat1~=. & e(sample)==1
gen asample = 1 if e(sample)==1

	forvalues i=1/15 {
		reg lrgdp`i' placebo atakeover `rhs1'  d1-d`g' [pweight=invwt] if asample==1, noconstant cluster(iso)
				test atakeover = placebo 				 
			    estadd scalar NonpPop_pdiff = r(p)
			    eststo IPW`i'
			    gen betahplacebo_`i'  = _b[placebo]
			 	gen	sehplacebo_`i'  = _se[placebo]
				gen betahatakeover_`i'  = _b[atakeover]
			 	gen sehatakeover_`i' = _se[atakeover]				
	}	

forvalues i=1/15 {
foreach t of local type {
replace b`t'0 = betah`t'_`i' if _n==`i'+1
replace se`t'0 = seh`t'_`i' if _n==`i'+1
	}	
	}

gen Years = _n-1 if _n <= 16
label var Years "Years"
gen zero = 0 

cap gen upbplacebo0 = bplacebo0 + 1.645*seplacebo0 
cap gen dnbplacebo0 = bplacebo0 - 1.645*seplacebo0  

twoway (rarea upbplacebo0 dnbplacebo0 Years, fcolor(gs12) lcolor(white) lpattern(solid)) (line bplacebo0 Years, lcolor(blue) lpattern(solid) lwidth(thick)) /// 
	   (line batakeover0 Years, lcolor(red) lpattern(shortdash) lwidth(vthick)) (line zero Years, lcolor(black)) if Year <=15, ylabel(, nogrid) ///
	   ytitle("Percent (100 x log)", size(medsmall) margin(small))  xlabel(, labsize(medlarge)) graphregion(color(white)) plotregion(color(white)) ///
	   subtitle("", size(small)) title("Stage 2: Inverse-propensity weighted local projection" , color(black) size(large) margin(medium)) ///
	   ylabel(, labsize(medlarge) angle(0)) legend(off) xsize(3) ysize(3) nodraw name(stage2, replace)

*Figure 5
gr combine stage1 stage2, rows(1) iscale(0.75) graphregion(color(white) margin(l=0 r=3 t=1)) imargin(0 0) xsize(10)  ysize(4.25)
gr export figures/Figure5.pdf, replace

*Table C3
esttab IPW* using tables/TableC3.tex, replace scalars(NonpPop_pdiff) se r2 keep(atakeover placebo) nonum compress b(2) se(2) sfmt(2) obslast nonotes label starlevels(* 0.1 ** 0.05 *** 0.01)

clear 

}

*--------------------------------------------------------------------------------------------------------------------------------------------------------------
*
*	Table 2 and Table B3
*
*--------------------------------------------------------------------------------------------------------------------------------------------------------------

capture { 
	
use data/ple_dataset, clear

label var Post_5 "Populist leader"
label var Post_15 "Populist leader"

tsset cid year
gen lgfstgdp = log(fstgdp)
gen rgdppc_gr = (lgfstgdp - l1.lgfstgdp) * 100 

estimates clear

eststo si05: qui reg rgdppc_gr Post_5 if year>=1946, robust 
eststo fe05: qui reg rgdppc_gr i.year i.cid Post_5 if year>=1946, robust
eststo ma05: qui reg rgdppc_gr i.year i.cid L1.institutions L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis L1.tradegdp L1.inflation Post_5 if year>=1946, robust 

eststo si15: qui reg rgdppc_gr Post_15 if year>=1946, robust 
eststo fe15: qui reg rgdppc_gr i.year i.cid Post_15 if year>=1946, robust
eststo ma15: qui reg rgdppc_gr i.year i.cid L1.institutions L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis L1.tradegdp L1.inflation Post_15 if year>=1946, robust 

esttab si05 fe05 ma05 using tables/Table2.tex, keep(*Post_5) se r2 b(2) se(2) obslast starlevels(* 0.1 ** 0.05 *** 0.01) title(5-year aftermath) nonotes label eqlabels(none) mlabels(none) replace
esttab si15 fe15 ma15 using tables/Table2.tex, keep(*Post_15) se r2 b(2) se(2) obslast starlevels(* 0.1 ** 0.05 *** 0.01) title(15-year aftermath) nonotes label eqlabels(none) mlabels(none) append

estimates clear
qui reg rgdppc_gr i.year i.cid L1.institutions L(1/5).bankcrisis L(1/5).currcrisis  L(1/5).debtcrisis L1.tradegdp L1.inflation Post_5 if year>=1946, robust 
gen SampleA = 1 if e(sample)

eststo m1s: qui reg rgdppc_gr Post_5 if year>=1946 & SampleA ==1, robust 
eststo m2s: qui reg rgdppc_gr i.year i.cid Post_5 if year>=1946 & SampleA ==1, robust
eststo m3s: qui reg rgdppc_gr i.year i.cid L1.institutions Post_5 if year>=1946 & SampleA ==1, robust      
eststo m4s: qui reg rgdppc_gr i.year i.cid L1.institutions L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis Post_5 if year>=1946 & SampleA ==1, robust  
eststo m5s: qui reg rgdppc_gr i.year i.cid L1.institutions L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis L1.tradegdp L1.inflation Post_5 if year>=1946 & SampleA ==1, robust 

eststo m1m: qui reg rgdppc_gr Post_15 if year>=1946 & war==0 & SampleA ==1, robust 
eststo m2m: qui reg rgdppc_gr i.year i.cid Post_15 if year>=1946 & war==0 & SampleA ==1, robust
eststo m3m: qui reg rgdppc_gr i.year i.cid L1.institutions Post_15 if year>=1946 & war==0 & SampleA ==1, robust     
eststo m4m: qui reg rgdppc_gr i.year i.cid L1.institutions L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis Post_15 if year>=1946 & SampleA ==1, robust  
eststo m5m: qui reg rgdppc_gr i.year i.cid L1.institutions L(1/5).bankcrisis L(1/5).currcrisis L(1/5).debtcrisis L1.tradegdp L1.inflation Post_15 if year>=1946 & SampleA ==1, robust 

esttab m1s m2s m3s m4s m5s using tables/TableB3.tex, keep(*Post_5) se r2 b(3) se(3) obslast starlevels(* 0.1 ** 0.05 *** 0.01) title(5-year aftermath) nonotes label eqlabels(none) mlabels(none) replace 
esttab m1m m2m m3m m4m m5m using tables/TableB3.tex, keep(*Post_15) se r2 b(3) se(3) obslast starlevels(* 0.1 ** 0.05 *** 0.01) title(15-year aftermath) nonotes label eqlabels(none) mlabels(none) append

clear

}    

*--------------------------------------------------------------------------------------------------------------------------------------------------------------
*
*	Table 3
*
*--------------------------------------------------------------------------------------------------------------------------------------------------------------

capture { 
	
use data/ple_dataset, clear

gen treatedyea_1 = 1946 // Peron 1946
gen treatedcid_1 = 1 // Peron 1946
gen treatedyea_2 = 1973 // Peron 1973
gen treatedcid_2 = 1 // Peron 1973
gen treatedyea_3 = 1989 // Menem 1989
gen treatedcid_3 = 1 // Menem 1989
gen treatedyea_4 = 2003 // Kirchner 2003
gen treatedcid_4 = 1 // Kirchner 2003
gen treatedyea_5 = 1952 // Estenssoro 1952
gen treatedcid_5 = 5 // Estenssoro 1952
gen treatedyea_6 = 1951 // Vargas 1951
gen treatedcid_6 = 6 // Vargas 1951
gen treatedyea_7 = 1990 // Collor 1990
gen treatedcid_7 = 6 // Collor 1990
gen treatedyea_8 = 1952 // Ibanez 1952
gen treatedcid_8 = 9 // Ibanez 1952
gen treatedyea_9 = 1952 // Velasco 1952
gen treatedcid_9 = 16 // Velasco 1952
gen treatedyea_10 = 1960 // Velasco 1960
gen treatedcid_10 = 16 // Velasco 1960
gen treatedyea_11 = 1968 // Velasco 1968
gen treatedcid_11 = 16 // Velasco 1968
gen treatedyea_12 = 1996 // Bucaram 1996
gen treatedcid_12 = 16 // Bucaram 1996
gen treatedyea_13 = 1966 // Gandhi 1966
gen treatedcid_13 = 25 // Gandhi 1966
gen treatedyea_14 = 1996 // Netanyahu 1996
gen treatedcid_14 = 28 // Netanyahu 1996
gen treatedyea_15 = 1994 // Berlusconi 1994
gen treatedcid_15 = 29 //  Berlusconi 1994
gen treatedyea_16 = 2001 // Berlusconi 2001
gen treatedcid_16 = 29 //  Berlusconi 2001
gen treatedyea_17 = 2001 // Koizumi 2001
gen treatedcid_17 = 30 //  Koizumi 2001
gen treatedyea_18 = 1970 // Echeverria 1970
gen treatedcid_18 = 36 //  Echeverria 1970
gen treatedyea_19 = 1975 // Muldoon 1975
gen treatedcid_19 = 38 //  Muldoon 1975
gen treatedyea_20 = 1985 // Garcia 1985
gen treatedcid_20 = 41 //  Garcia 1985
gen treatedyea_21 = 1990 // Fujimori 1990
gen treatedcid_21 = 41 //   Fujimori 1990
gen treatedyea_22 = 1998 // Estrada 1998
gen treatedcid_22 = 42 //  Estrada 1998
gen treatedyea_23 = 1990 // Meciar 1990
gen treatedcid_23 = 47 //   Meciar 1990
gen treatedyea_24 = 2003 // Roh 2003
gen treatedcid_24 = 50 //   Roh 2003
gen treatedyea_25 = 2000 // Chen 2000
gen treatedcid_25 = 54 //   Chen 2000
gen treatedyea_26 = 2001 // Shinawatra 2001
gen treatedcid_26 = 55 //   Shinawatra 2001
gen treatedyea_27 = 2003 // Erdogan 2003
gen treatedcid_27 = 56 //   Erdogan 2003
gen treatedyea_28 = 1999 // Chavez 1999
gen treatedcid_28 = 60 //   Chavez 1999

gen treatedyea_29 = 1928 // Yrigoyen1928 
gen treatedcid_29 =  1 // Yrigoyen1928 
gen treatedyea_30 = 1920 // Alessandri1920 	
gen treatedcid_30 =  9 // Alessandri1920 				   
gen treatedyea_31 = 1934   // Velasco1934
gen treatedcid_31 =  16 // Velasco1934 				   
gen treatedyea_32 = 1933 // Hitler1933 	
gen treatedcid_32 =  21 // Hitler1933 					   
gen treatedyea_33 = 2014  // Widodo2014 
gen treatedcid_33 =  26 // Widodo2014 				   
gen treatedyea_34 = 1922 // Mussolini1922 
gen treatedcid_34 =  29 // Mussolini1922 				   
gen treatedyea_35 = 2019 // Johnson2019 
gen treatedcid_35 =  57 // Johnson2019 				   
gen treatedyea_36 = 2006  // Morales2006 
gen treatedcid_36 =  5 // Morales2006 				         
gen treatedyea_37 = 1930 // Vargas1930
gen treatedcid_37 =  6 // Vargas1930 					             
gen treatedyea_38 = 2019 // Bolsonaro2019 	
gen treatedcid_38 = 6 // Bolsonaro2019 				    
gen treatedyea_39 = 2009 // Borisov2009 
gen treatedcid_39 =  7 // Borisov2009  					   
gen treatedyea_40 = 2007 // Correa2007
gen treatedcid_40 =  16 // Correa2007 						   
gen treatedyea_41 = 2015 // Tsipras2015 
gen treatedcid_41 =  22 // Tsipras2015 				   					   
gen treatedyea_42 = 2010 // Orban2010 
gen treatedcid_42 =  23 // Orban2010  						    
gen treatedyea_43 = 2014 // Modi2014 
gen treatedcid_43 =  25 // Modi2014 			   
gen treatedyea_44 = 2009 // Netanyahu2009 
gen treatedcid_44 =  28 // Netanyahu2009 		   
gen treatedyea_45 = 2018 // Lega2018
gen treatedcid_45 =  29 // Lega2018 					   
gen treatedyea_46 = 1934 // Cardenas1934
gen treatedcid_46 =  36 // Cardenas1934 	
gen treatedyea_47 = 2018 // Obrador2018
gen treatedcid_47 =  36 // Obrador2018 				   
gen treatedyea_48 = 2016 // Duterte2016 
gen treatedcid_48 =  42 // Duterte2016 
gen treatedyea_49 = 2005 // Kaczynski2005
gen treatedcid_49 =  43 // Kaczynski2005 						   					    
gen treatedyea_50 = 2015 // Pis2015 	
gen treatedcid_50 =  43 // Pis2015 			   
gen treatedyea_51 = 2006 // Fico2006 
gen treatedcid_51 =  47 // Fico2006 	
gen treatedyea_52 = 2009 // Zuma2009 
gen treatedcid_52 =  49 // Zuma2009 
gen treatedyea_53 = 2017 // Trump2017  
gen treatedcid_53 =  58 // Trump2017
					   
gen treatedyea_54 = 2002  // Duhalde2002 
gen treatedcid_54 = 1   //   Duhalde2002 				  
gen treatedyea_55 = 1961   // Goulart1961
gen treatedcid_55 = 6  //   Goulart1961 
gen treatedyea_56 = 1969   // Unclearnamegeisel1969
gen treatedcid_56 = 6  //   Unclearnamegeisel1969 
gen treatedyea_57 = 1985   // Sarney1985
gen treatedcid_57 = 6   //  Sarney1985  					   
gen treatedyea_58 = 1970   // Allende1970
gen treatedcid_58 = 9  //   Allende1970  					   
gen treatedyea_59 = 1990   // Tudman1990
gen treatedcid_59 = 12  //   Tudman1990  			   
gen treatedyea_60 = 1998   // Zeman1998
gen treatedcid_60 = 14  //   Zeman1998  					    
gen treatedyea_61 = 2006   // Topolanek2006
gen treatedcid_61 = 14  //   Topolanek2006  					   
gen treatedyea_62 = 2017  // Babis2017
gen treatedcid_62 = 14  //   Babis2017  
gen treatedyea_63 = 2003   // Gutierrez2003
gen treatedcid_63 = 16  //   Gutierrez2003  					    					     					   
gen treatedyea_64 = 2002   // Repsekalvitis2002
gen treatedcid_64 = 31  //   Repsekalvitis2002   					      					   
gen treatedyea_65 = 1976   // Portillo1976
gen treatedcid_65 = 36   //  Portillo1976  					   
gen treatedyea_66 = 2003  // Duarte2003
gen treatedcid_66 = 40  //   Duarte2003  					   
gen treatedyea_67 = 2008  // Lugo2008
gen treatedcid_67 = 40  //   Lugo2008  					     					     					   
gen treatedyea_68 = 1963  // Belaundealvaradob1963
gen treatedcid_68 = 41  //   Belaundealvaradob1963  					   
gen treatedyea_69 = 2006  // Garcia2006 
gen treatedcid_69 = 41  //   Garcia2006  				   
gen treatedyea_70 = 2011  // Humala2011
gen treatedcid_70 = 41  //   Humala2011  					   
gen treatedyea_71 = 1953  // Magsaysay1953
gen treatedcid_71 = 42  //   Magsaysay1953  					   
gen treatedyea_72 = 2001  // Arroyo2001
gen treatedcid_72 = 42   //  Arroyo2001  					   
gen treatedyea_73 = 1990  // Walesa1990
gen treatedcid_73 = 43   //  Walesa1990  					   
gen treatedyea_74 = 2004  // Basescu2004
gen treatedcid_74 = 45  //   Basescu2004  					   
gen treatedyea_75 = 2000  // Putin2000
gen treatedcid_75 = 46  //   Putin2000  					   
gen treatedyea_76 = 2004 // Jansa2004
gen treatedcid_76 = 48  //   Jansa2004   					   
gen treatedyea_77 = 2012  // Jansa2012
gen treatedcid_77 = 48  //   Jansa2012  					   
gen treatedyea_78 = 2011  // Yingluck2011
gen treatedcid_78 = 55  //   Yingluck2011  					    					   
gen treatedyea_79 = 1974  // Perez1974
gen treatedcid_79 = 60  //   Perez1974  
gen treatedyea_80 = 1994 // Caldera1994
gen treatedcid_80 = 60  //   Caldera1994 

forvalues i=1/80{
label var treatedcid_`i' "Treated country ID of populist case `i'"
label var treatedyea_`i' "Treatment year of populist case `i'"
	}

gen var = log(fstgdp)
rename institutions i
rename inflation r
rename bankcrisis c
rename debtcrisis s

gen simultake = 0
forvalues i =1/28 {
replace simultake = 1 if cid == treatedcid_`i' & year == treatedyea_`i'
}  

foreach i of numlist 1/28 {

	preserve  
 
gen byte b = 1 if year==treatedyea_`i'

if `i' !=23 {

bys cid (b): keep if year >= year[1] - 15 & year <= year[1] + 15

}

else {

bys cid (b): keep if year >= year[1] - 5 & year <= year[1] + 15 

}

bys cid (b): gen ti = year - year[1] + 15 

gen bsimul = 1 if b == 1 & simultake == 1 & cid!=treatedcid_`i'
bys cid : egen msimul = max(bsimul)
bys cid : drop if msimul == 1
 
foreach x in var i r c s{                          
	bys cid: ipolate `x' year, gen(ipo_`x') 
	replace `x' = ipo_`x' if ipo_`x'!=. & `x'==. 
    gsort cid -year
    bys cid: replace `x' = `x'[_n-1] if `x'==.
	}
	
bys cid (b): gen d =  var - var[1]

foreach z in i {
	bys cid (b): gen d`z' =  `z' - `z'[1]
	bys cid (d`z'): drop if missing(d`z'[_N]) & cid!=treatedcid_`i'
	bys year (d`z'): drop if missing(d`z'[_N])
	replace `z' = d`z'  
	}
	
foreach k in c s { 
	bys cid : egen `k'yn = max(`k') if (year < treatedyea_`i') & (year >= (treatedyea_`i' - 5))    
	bys cid : egen m`k'yn = max(`k'yn) 
	}
	
foreach j in r d mcyn msyn { 
	bys cid (`j'): drop if missing(`j'[_N]) & cid!=treatedcid_`i'                           
	bys year (`j'): drop if missing(`j'[_N])    
	}
	
qui sum treatedcid_`i'
local c = r(mean)
qui sum treatedyea_`i'
local y = r(mean)

xtset cid ti

if `i' !=23 {
	
cap synth d d(0) d(1) d(2) d(3) d(4) d(5) d(6) d(7) d(8) d(9) d(10) d(11) d(12) d(13) d(14) ///
i(0) i(1) i(2) i(3) i(4) i(5) i(6) i(7) i(8) i(9) i(10) i(11) i(12) i(13) i(14)   ///
r(0) r(1) r(2) r(3) r(4) r(5) r(6) r(7) r(8) r(9) r(10) r(11) r(12) r(13) r(14) /// 
mcyn(14) msyn(14), trperiod(15) trunit(`c') keep(_data, replace) unitnames(country) 

}

else {

cap synth d d(10) d(11) d(12) d(13) d(14) i(10) i(11) i(12) i(13) i(14)   ///
r(11) r(12) r(13) r(14)  mcyn(14) msyn(14), trperiod(15) trunit(`c') keep(_data, replace) unitnames(country)  

}

matrix matrixbalance = e(X_balance)
svmat matrixbalance
rename matrixbalance1 Treated
rename matrixbalance2 Synthetic
gen DonorPool = .
gen Predictor=""

foreach k in d i r  mcyn msyn  {
	
gen dpoolof_`k' = .
forvalues l = 0/14 { 
	egen dpool_`k'`l' = mean(`k') if ti == `l' & cid!=treatedcid_`i'
	egen mdpool_`k'`l' = max(dpool_`k'`l') 
	}
}

if `i' !=23 {

forvalues l = 0/14 { 
	replace dpoolof_d = mdpool_d`l'  if _n==`l'+1
	replace dpoolof_i = mdpool_i`l'  if _n==`l'+16
	replace dpoolof_r = mdpool_r`l'  if _n==`l'+31
	cap replace dpoolof_mcyn = mdpool_mcyn`l'  if _n==`l'+ 32
	cap replace dpoolof_msyn = mdpool_msyn`l'  if _n==`l'+ 33
	}
	
replace DonorPool = dpoolof_d in 1/15
replace DonorPool = dpoolof_i in 16/30 
replace DonorPool = dpoolof_r in 31/45
replace DonorPool = dpoolof_mcyn in 46/46
replace DonorPool = dpoolof_msyn in 47/47

gen time_d = _n-1 in 1/15
gen time_i = _n-16 in 16/30
gen time_r = _n-31 in 31/45
tostring time_*, replace

replace Predictor="GDP" + time_d in 1/15
replace Predictor="Institutions" + time_i  in 16/30
replace Predictor="Inflation" + time_r  in 31/45
replace Predictor="Banking crises" in 46/46
replace Predictor="Debt crises" in 47/47

}

else {
	
	forvalues l = 10/14 { 
	replace dpoolof_d = mdpool_d`l'  if _n==`l'-9
	replace dpoolof_i = mdpool_i`l'  if _n==`l'-4
	replace dpoolof_r = mdpool_r`l'  if _n==`l'+1
	replace dpoolof_mcyn = mdpool_mcyn`l'  if _n==`l'+ 2
	replace dpoolof_msyn = mdpool_msyn`l'  if _n==`l'+ 3
	}
	
replace DonorPool = dpoolof_d in 1/5
replace DonorPool = dpoolof_i in 6/10 
replace DonorPool = dpoolof_r in 11/15
replace DonorPool = dpoolof_mcyn in 16/16
replace DonorPool = dpoolof_msyn in 17/17

gen time_d = _n+9 in 1/5
gen time_i = _n+4 in 6/10
gen time_r = _n-1 in 11/15
tostring time_*, replace

replace Predictor="GDP" + time_d in 1/5
replace Predictor="Institutions" + time_i  in 6/10
replace Predictor="Inflation" + time_r  in 11/15
replace Predictor="Banking crises" in 16/16
replace Predictor="Debt crises" in 17/17
	
}

cap sav _`y'_`c', replace
restore
}

cap !del _data
cap !rm _data.dta
local allfiles : dir . files "_*.dta"
		foreach file of local allfiles {
				preserve
				use `file', clear
				save _temp, replace
				restore
		append using _temp, force
	} 

drop if Synthetic==. | Treated==. 
cap !del _*
cap !rm _*.dta

drop if Synthetic==. | Treated==. 
cap !del _*
cap !rm _*.dta
 
replace Predictor = "GDP" if strpos(Predictor, "GDP") >0
replace Predictor = "Institutions" if strpos(Predictor, "Institutions") >0
replace Predictor = "Inflation" if strpos(Predictor, "Inflation") >0
replace Predictor = "Banking crises" if strpos(Predictor, "Banking") >0
replace Predictor = "Sovereign debt crises" if strpos(Predictor, "Debt") >0

collapse (mean) Treated Synthetic DonorPool , by(Predictor)

gen rang = 1 if Predictor=="GDP" 
replace rang = 2 if Predictor=="Institutions" 
replace rang = 3 if Predictor=="Inflation"
replace rang = 4 if Predictor=="Banking crises"
replace rang = 5 if Predictor=="Sovereign debt crises" 
sort rang
 
gen sTreated = string(Treated, "%04.3f") 
gen sSynthetic = string(Synthetic, "%04.3f") 
gen sDonorPool = string(DonorPool, "%04.3f")
drop Treated Synthetic DonorPool rang
rename (sTreated sSynthetic sDonorPool) (Treated Synthetic DonorPool)
texsave * using tables/Table3.tex, title(TABLE 3) replace
filefilter "tables/Table3.tex" "tables/Table3.tex", from("[tbp]") to("[H]") replace

clear

}
