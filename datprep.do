************************************************************************************************************************************************************************************************
****Net installation dm89_2 package if not Windows 
************************************************************************************************************************************************************************************************

local os : di c(os)

if "`c(os)'" == "Windows" {

 }
    else { 

cap net install dm89_2.pkg, from("http://www.stata-journal.com/software/sj15-4") 

	}

	************************************************************************************************************************************************************************************************
****Skeleton panel dataset 
************************************************************************************************************************************************************************************************

clear all
set obs 60
gen country=""
replace country="Argentina"        in 1 
replace country="Australia"        in 2 
replace country="Austria"          in 3
replace country="Belgium"          in 4 
replace country="Bolivia"          in 5 
replace country="Brazil"           in 6 
replace country="Bulgaria"         in 7 
replace country="Canada"           in 8 
replace country="Chile"            in 9 
replace country="China"            in 10 
replace country="Colombia"         in 11
replace country="Croatia"          in 12
replace country="Cyprus"     	   in 13
replace country="Czech Republic"   in 14
replace country="Denmark"          in 15
replace country="Ecuador"          in 16
replace country="Egypt"            in 17
replace country="Estonia"          in 18
replace country="Finland"          in 19
replace country="France"           in 20
replace country="Germany"          in 21 
replace country="Greece"           in 22
replace country="Hungary"          in 23
replace country="Iceland"          in 24
replace country="India"            in 25
replace country="Indonesia"        in 26
replace country="Ireland"          in 27
replace country="Israel"           in 28
replace country="Italy"            in 29
replace country="Japan"            in 30
replace country="Latvia"           in 31 
replace country="Lithuania"        in 32
replace country="Luxembourg"       in 33 
replace country="Malaysia"         in 34 
replace country="Malta"            in 35 
replace country="Mexico"           in 36
replace country="Netherlands"      in 37 
replace country="New Zealand"      in 38 
replace country="Norway"           in 39 
replace country="Paraguay"         in 40 
replace country="Peru"             in 41
replace country="Philippines"      in 42
replace country="Poland"           in 43
replace country="Portugal"         in 44
replace country="Romania"          in 45
replace country="Russia"           in 46
replace country="Slovakia"         in 47
replace country="Slovenia"         in 48
replace country="South Africa"     in 49 
replace country="South Korea"      in 50
replace country="Spain"            in 51
replace country="Sweden"           in 52
replace country="Switzerland"      in 53
replace country="Taiwan"           in 54
replace country="Thailand"         in 55
replace country="Turkey"           in 56
replace country="United Kingdom"   in 57
replace country="United States"    in 58
replace country="Uruguay"          in 59
replace country="Venezuela"        in 60

gen iso=""
replace iso="ARG"        in 1 
replace iso="AUS"        in 2 
replace iso="AUT"        in 3
replace iso="BEL"        in 4 
replace iso="BOL"        in 5 
replace iso="BRA"        in 6 
replace iso="BGR"        in 7 
replace iso="CAN"        in 8 
replace iso="CHL"        in 9 
replace iso="CHN"        in 10 
replace iso="COL"        in 11
replace iso="HRV"        in 12
replace iso="CYP"     	 in 13
replace iso="CZE"        in 14
replace iso="DNK"        in 15
replace iso="ECU"        in 16
replace iso="EGY"        in 17
replace iso="EST"        in 18
replace iso="FIN"        in 19
replace iso="FRA"        in 20
replace iso="DEU"        in 21 
replace iso="GRC"        in 22
replace iso="HUN"        in 23
replace iso="ISL"        in 24
replace iso="IND"        in 25
replace iso="IDN"        in 26
replace iso="IRL"        in 27
replace iso="ISR"        in 28
replace iso="ITA"        in 29
replace iso="JPN"        in 30
replace iso="LVA"        in 31 
replace iso="LTU"        in 32
replace iso="LUX"        in 33 
replace iso="MYS"        in 34 
replace iso="MLT"        in 35 
replace iso="MEX"        in 36
replace iso="NLD"        in 37 
replace iso="NZL"        in 38 
replace iso="NOR"        in 39 
replace iso="PRY"        in 40 
replace iso="PER"        in 41
replace iso="PHL"        in 42
replace iso="POL"        in 43
replace iso="PRT"        in 44
replace iso="ROU"        in 45
replace iso="RUS"        in 46
replace iso="SVK"        in 47
replace iso="SVN"        in 48
replace iso="ZAF"        in 49 
replace iso="KOR"        in 50
replace iso="ESP"        in 51
replace iso="SWE"        in 52
replace iso="CHE"        in 53
replace iso="TWN"        in 54
replace iso="THA"        in 55
replace iso="TUR"        in 56
replace iso="GBR"        in 57
replace iso="USA"        in 58
replace iso="URY"        in 59
replace iso="VEN"        in 60

expand 150
bysort country: gen year = 1870 + [_n-1]
egen cid = group(country)
xtset cid year

save panel, replace
clear

************************************************************************************************************************************************************************************************
****GDP 
************************************************************************************************************************************************************************************************

import excel using data/barro_ursua_macrodataset_1110, sheet("GDP") firstrow clear
foreach v of varlist (Argentina-Venezuela) {
	rename `v' id`v'
	}        
reshape long id, j(country) i(GDPpc) string
drop if GDPpc == "Indexes, 2006=100"
rename (GDPpc id) (year GDPpc)
destring GDPpc year, replace
replace country = "South Korea" if country=="Korea"
replace country = "New Zealand" if country=="NewZealand"
replace country = "South Africa" if country=="SAfrica"
replace country = "United Kingdom" if country=="UnitedKingdom" 
replace country = "United States" if country=="UnitedStates"
rename GDPpc barrogdp
save _barrogdp, replace

use data/JSTdatasetR4, clear 
keep iso year rgdppc
rename rgdppc jstgdp
save _jstgdp, replace

import excel using data/mpd2018, sheet("Full data") firstrow clear 
drop if countrycode==""
keep countrycode year rgdpnapc
rename (countrycode rgdpnapc) (iso mpdgdp)
save _mpdgdp, replace

import excel using data/WDI_Extract_NYGDPPCAPKD1, firstrow clear 
keep CountryCode YR*
drop if CountryCode==""
reshape long YR, i(CountryCode) j(year)
rename (YR CountryCode) (wdigdp1 iso)
save _wdigdp1, replace 

import excel using data/WDI_Extract_NYGDPPCAPKD2, firstrow clear 
keep Time Value CountryCode
drop if CountryCode==""
rename (Time Value CountryCode) (year wdigdp2 iso)
save _wdigdp2, replace 

use panel, clear

merge 1:1 country year using _barrogdp
drop if _merge==2
drop _merge

local set1 mpdgdp jstgdp wdigdp1 wdigdp2

foreach s of local set1 {	
	merge 1:1 iso year using _`s'
	drop if _merge==2
	drop _merge	
	}
	
local set2 barrogdp mpdgdp jstgdp wdigdp1 wdigdp2			

gen byte base = 1 if year==2005		
foreach s of local set2 {			
		bys cid (base): gen `s'_i = `s'/`s'[1]*100
        bys cid: egen m`s'i = max(`s'_i)
		}

gen fstgdp = wdigdp2_i if year>=2005
replace fstgdp = jstgdp_i if year<=2004 & mjstgdpi!=.
replace fstgdp = barrogdp_i if year<=2004 & mjstgdpi==. & mbarrogdpi!=.
replace fstgdp = mpdgdp_i if year<=2004 & mjstgdpi==. & mbarrogdpi==. & mmpdgdpi!=.
replace fstgdp = mpdgdp_i if year>=2005 & iso=="TWN"
replace fstgdp = wdigdp1_i if year>=2005 & iso=="VEN"

bys cid: ipolate fstgdp year, gen(fstgdp_ipo)
replace fstgdp = fstgdp_ipo if fstgdp==.

drop ba* m* j* *_i* wdi*
cap !del _*
cap !rm _*.dta

save panel, replace
clear

************************************************************************************************************************************************************************************************
****Institutional quality 
************************************************************************************************************************************************************************************************

import excel using data/p5v2018, sheet("p5v2018") firstrow clear
drop if (year==1945 | year==1990) & country=="Germany West"
drop if (country=="Ethiopia" | country=="Yugoslavia")
drop if year==1922 & country=="Russia" 
replace country="South Korea" if (country=="Korea South" | country=="Korea") 
replace country="Czech Republic" if country=="Czechoslovakia"
replace country="Slovakia" if country=="Slovak Republic"
replace country="Germany" if country=="Germany West"
replace country="United States" if scode=="USA"
replace country="Russia" if country=="USSR"
save _polity, replace

use panel, clear

merge 1:1 country year using _polity, keepusing(polity2)
drop if _merge==2
drop _merge
cap !del _*
cap !rm _*.dta

sort country year
replace polity2 = polity2[_n-1] if year==2019
replace polity2 = . if polity2 == -66

bys cid: ipolate polity2 year if iso=="BGR" & year>=1912 & year<=1914, gen(_BGR)
bys cid: ipolate polity2 year if iso=="TUR" & year>=1917 & year<=1922, gen(_TUR)
bys cid: ipolate polity2 year if iso=="CHN" & year>=1936 & year<=1946, gen(_CHN)
bys cid: ipolate polity2 year if iso=="GRC" & year>=1915 & year<=1920, gen(_GRC)
bys cid: ipolate polity2 year if iso=="HUN" & year>=1955 & year<=1957, gen(_HUN)
bys cid: ipolate polity2 year if iso=="JPN" & year>=1944 & year<=1952, gen(_JPN)
bys cid: ipolate polity2 year if iso=="DEU" & year>=1944 & year<=1949, gen(_DEU)

local cous1 BGR TUR CHN GRC HUN JPN DEU

foreach c of local cous1 {
		replace polity2 = _`c' if iso=="`c'" & polity2==. & _`c'!=.	
		}

gen country_name = country
replace country_name = "United States of America" if country_name=="United States"
merge 1:1 country_name year using data/V-Dem-CY-Full+Others-v12, keepusing(v2x_jucon v2xel_frefair v2xme_altinf)
drop if _merge==2
drop _merge country_name

gen byte b = 1 if year==1900 & iso=="THA"
bys cid (b): replace v2x_jucon = v2x_jucon[1] if iso=="THA" & year<=1900

local vdem v2x_jucon v2xel_frefair v2xme_altinf
local cous2 DEU AUT POL

foreach v of local vdem {
	
	bys cid: ipolate `v' year if iso=="DEU" & year>=1944 & year<=1949, gen(_`v'_DEU) 
	bys cid: ipolate `v' year if iso=="AUT" & year>=1938 & year<=1945, gen(_`v'_AUT)
	bys cid: ipolate `v' year if iso=="POL" & year>=1938 & year<=1944, gen(_`v'_POL)

				foreach c of local cous2 {
				replace `v' = _`v'_`c' if iso=="`c'" & `v'==. & _`v'_`c'!=.
				}
		}
		
pca v2xel_frefair v2x_jucon v2xme_altinf polity2 
predict institutions

sort iso year
forvalues i=1/8 {
	replace institutions = institutions[_n+`i'] if iso=="SVK" & (year== 1993-`i')
	}

drop polity2 _* b v2x* 

save panel, replace
clear

************************************************************************************************************************************************************************************************
****Judicial constraints 
************************************************************************************************************************************************************************************************

use panel, clear

gen country_name = country
replace country_name = "United States of America" if country_name=="United States"
merge 1:1 country_name year using data/V-Dem-CY-Full+Others-v12, keepusing(v2x_jucon)
drop if _merge==2
drop _merge country_name

sort iso year
gen byte b = 1 if year==1900 & iso=="THA"
bys cid (b): replace v2x_jucon = v2x_jucon[1] if iso=="THA" & year<=1900

local cous2 DEU AUT POL

bys cid: ipolate v2x_jucon year if iso=="DEU" & year>=1944 & year<=1949, gen(_v2x_jucon_DEU) 
bys cid: ipolate v2x_jucon year if iso=="AUT" & year>=1938 & year<=1945, gen(_v2x_jucon_AUT)
bys cid: ipolate v2x_jucon year if iso=="POL" & year>=1938 & year<=1944, gen(_v2x_jucon_POL)

foreach c of local cous2 {
		replace v2x_jucon = _v2x_jucon_`c' if iso=="`c'" & v2x_jucon==. & _v2x_jucon_`c'!=.
		}

rename v2x_jucon judicial

drop _* b

save panel, replace
clear

************************************************************************************************************************************************************************************************
****Free elections 
************************************************************************************************************************************************************************************************

use panel, clear

gen country_name = country
replace country_name = "United States of America" if country_name=="United States"
merge 1:1 country_name year using data/V-Dem-CY-Full+Others-v12, keepusing(v2xel_frefair)
drop if _merge==2
drop _merge country_name

sort iso year

local cous2 DEU AUT POL

bys cid: ipolate v2xel_frefair year if iso=="DEU" & year>=1944 & year<=1949, gen(_v2xel_frefair_DEU) 
bys cid: ipolate v2xel_frefair year if iso=="AUT" & year>=1938 & year<=1945, gen(_v2xel_frefair_AUT)
bys cid: ipolate v2xel_frefair year if iso=="POL" & year>=1938 & year<=1944, gen(_v2xel_frefair_POL)

foreach c of local cous2 {
		replace v2xel_frefair = _v2xel_frefair_`c' if iso=="`c'" & v2xel_frefair==. & _v2xel_frefair_`c'!=.
		}

rename v2xel_frefair electoral

drop _* 

save panel, replace
clear

************************************************************************************************************************************************************************************************
****Media freedom 
************************************************************************************************************************************************************************************************

use panel, clear

gen country_name = country
replace country_name = "United States of America" if country_name=="United States"
merge 1:1 country_name year using data/V-Dem-CY-Full+Others-v12, keepusing(v2xme_altinf)
drop if _merge==2
drop _merge country_name

sort iso year

local cous2 DEU AUT POL

bys cid: ipolate v2xme_altinf year if iso=="DEU" & year>=1944 & year<=1949, gen(_v2xme_altinf_DEU) 
bys cid: ipolate v2xme_altinf year if iso=="AUT" & year>=1938 & year<=1945, gen(_v2xme_altinf_AUT)
bys cid: ipolate v2xme_altinf year if iso=="POL" & year>=1938 & year<=1944, gen(_v2xme_altinf_POL)

foreach c of local cous2 {
		replace v2xme_altinf = _v2xme_altinf_`c' if iso=="`c'" & v2xme_altinf==. & _v2xme_altinf_`c'!=.
		}

rename v2xme_altinf medial

drop _* 

save panel, replace
clear

************************************************************************************************************************************************************************************************
****Currency crises 
************************************************************************************************************************************************************************************************

use panel, clear

gen y = year
gen c = country
merge 1:1 c y using data/Varieties_crises_CR_Update2015, keepusing(Currency_crises)
drop if _merge==2

replace Currency_crises = 1 if Currency_crises==2
replace Currency_crises = 0 if iso=="ISR" & (year>=1950 & year<=2014) 
replace Currency_crises = 0 if iso=="SVK" & (year>=1985 & year<=2014)

gen uc = Currency_crises
xtset cid year
bys cid : replace uc = 0 if Currency_crises==1 & L1.Currency_crises==1
gen currcrisis = uc

drop _merge y c uc Curr*

save panel, replace
clear

************************************************************************************************************************************************************************************************
****Debt crises 
************************************************************************************************************************************************************************************************

use panel, clear

gen y = year
gen c = country
merge 1:1 c y using data/Varieties_crises_CR_Update2015, keepusing(External_debt_crises Domestic_debt_crises)
drop if _merge==2

replace External_debt_crises = 0 if iso=="ISR" & (year>=1950 & year<=2014) 
replace External_debt_crises = 0 if iso=="SVK" & (year>=1985 & year<=2014) 

gen debtcris = 0 if External_debt_crises!=. | Domestic_debt_crises!=.
replace debtcris = 1 if External_debt_crises==1 | Domestic_debt_crises==1

gen ud = debtcris
xtset cid year
bys cid : replace ud = 0 if debtcris==1 & L1.debtcris==1
gen debtcrisis = ud

drop _merge y c ud Ext* Dom* debtcris 

save panel, replace
clear

************************************************************************************************************************************************************************************************
****Banking crises 
************************************************************************************************************************************************************************************************

use panel, clear
keep if year == 2000
keep country
save _countries, replace 

import excel data/41308_2020_107_MOESM1_ESM, sheet("Crisis Years") firstrow clear

keep Country SystemicBankingCrisisstartin
rename  SystemicBankingCrisisstartin sbcs

split sbcs, p(", ")

destring sbcs1 sbcs2 sbcs3 sbcs4, replace
drop sbcs
drop in 1
rename Country country

replace country="China" if country=="China, P.R."
replace country="Slovakia" if country=="Slovak Republic"
replace country="South Korea" if country=="Korea"

merge 1:1 country using _countries.dta
keep if _merge==3
drop _merge
drop if sbcs1 == . & sbcs2 == . & sbcs3 == . & sbcs4 == . 

local chron sbcs1 sbcs2 sbcs3 sbcs4
foreach c of local chron {
preserve
keep country `c'
rename `c' sbcs
save _`c', replace
restore
}

use _sbcs1, clear
append using _sbcs2
append using _sbcs3
append using _sbcs4
drop if sbcs==.
rename sbcs year 
replace year=1984 if year==1983 & (country=="Israel" | country=="Peru") 
drop if (country=="Cyprus" | country=="United Kingdom")
gen syst_bankcrisis_start = 1
save _SystemicBCUpdateII2020, replace

use panel, clear

merge 1:1 country year using _SystemicBCUpdateII2020 
drop if _merge==2
drop _merge
cap !del _*
cap !rm _*.dta

gen y = year
gen c = country
merge 1:1 c y using data/Varieties_crises_CR_Update2015, keepusing(Banking_crises) 
drop if _merge==2
drop _merge y c

merge 1:1 iso year using data/JSTdatasetR4, keepusing(crisisJST) 
drop if _merge==2
drop _merge

replace crisisJST = 0 if year==2017
replace crisisJST = 0 if year>=1920 & year <=2016 & iso=="IRL"
replace crisisJST = 1 if year==2008 & iso=="IRL"
replace crisisJST = 1 if year==1876 & iso=="BEL"
replace crisisJST = 0 if year==1907 & iso=="DEU"
replace crisisJST = 0 if year==1931 & iso=="DNK"
replace crisisJST = 1 if year==1901 & iso=="JPN"
replace crisisJST = 0 if year==1893 & iso=="NLD"
replace crisisJST = 0 if year==1907 & iso=="NLD"
replace crisisJST = 0 if year==1939 & iso=="NLD"
replace crisisJST = 0 if year==1929 & iso=="USA"
replace crisisJST = 1 if year==1930 & iso=="USA"

replace Banking_crises = . if Banking_crises==0
replace crisisJST = . if crisisJST==0

xtset cid year
gen aftermath = Banking_crises - L1.Banking_crises
replace Banking_crises = . if aftermath==0

gen bankcrisis=.
replace bankcrisis = Banking_crises if iso=="ARG"
replace bankcrisis = Banking_crises if iso=="AUT"
replace bankcrisis = Banking_crises if iso=="BOL"
replace bankcrisis = Banking_crises if iso=="BRA"
replace bankcrisis = Banking_crises if iso=="CHL"
replace bankcrisis = Banking_crises if iso=="CHN"
replace bankcrisis = Banking_crises if iso=="COL"
replace bankcrisis = Banking_crises if iso=="ECU"
replace bankcrisis = Banking_crises if iso=="EGY"
replace bankcrisis = Banking_crises if iso=="GRC"
replace bankcrisis = Banking_crises if iso=="HUN"
replace bankcrisis = Banking_crises if iso=="IDN"
replace bankcrisis = Banking_crises if iso=="IND"
replace bankcrisis = Banking_crises if iso=="ISL"
replace bankcrisis = Banking_crises if iso=="KOR"
replace bankcrisis = Banking_crises if iso=="MEX"
replace bankcrisis = Banking_crises if iso=="MYS"
replace bankcrisis = Banking_crises if iso=="NZL"
replace bankcrisis = Banking_crises if iso=="PER"
replace bankcrisis = Banking_crises if iso=="PHL"
replace bankcrisis = Banking_crises if iso=="POL"
replace bankcrisis = Banking_crises if iso=="PRY"
replace bankcrisis = Banking_crises if iso=="ROU"
replace bankcrisis = Banking_crises if iso=="RUS"
replace bankcrisis = Banking_crises if iso=="THA"
replace bankcrisis = Banking_crises if iso=="TUR"
replace bankcrisis = Banking_crises if iso=="TWN"
replace bankcrisis = Banking_crises if iso=="URY"
replace bankcrisis = Banking_crises if iso=="VEN"
replace bankcrisis = Banking_crises if iso=="ZAF"
replace bankcrisis = crisisJST if iso=="AUS"
replace bankcrisis = crisisJST if iso=="BEL"
replace bankcrisis = crisisJST if iso=="CAN"
replace bankcrisis = crisisJST if iso=="CHE"
replace bankcrisis = crisisJST if iso=="DEU"
replace bankcrisis = crisisJST if iso=="DNK"
replace bankcrisis = crisisJST if iso=="ESP"
replace bankcrisis = crisisJST if iso=="FIN"
replace bankcrisis = crisisJST if iso=="FRA"
replace bankcrisis = crisisJST if iso=="GBR"
replace bankcrisis = crisisJST if iso=="IRL"
replace bankcrisis = crisisJST if iso=="ITA"
replace bankcrisis = crisisJST if iso=="JPN"
replace bankcrisis = crisisJST if iso=="NLD"
replace bankcrisis = crisisJST if iso=="NOR"
replace bankcrisis = crisisJST if iso=="PRT"
replace bankcrisis = crisisJST if iso=="SWE"
replace bankcrisis = crisisJST if iso=="USA"
replace bankcrisis = syst_bankcrisis_start if iso=="BGR"
replace bankcrisis = syst_bankcrisis_start if iso=="CYP"
replace bankcrisis = syst_bankcrisis_start if iso=="CZE"
replace bankcrisis = syst_bankcrisis_start if iso=="EST"
replace bankcrisis = syst_bankcrisis_start if iso=="HRV"
replace bankcrisis = syst_bankcrisis_start if iso=="ISR" 
replace bankcrisis = syst_bankcrisis_start if iso=="LTU"
replace bankcrisis = syst_bankcrisis_start if iso=="LUX"
replace bankcrisis = syst_bankcrisis_start if iso=="LVA"
replace bankcrisis = syst_bankcrisis_start if iso=="SVK"
replace bankcrisis = syst_bankcrisis_start if iso=="SVN"
replace bankcrisis = . if iso=="MLT"

replace bankcrisis = 0 if bankcrisis!=1 
replace bankcrisis =. if fstgdp==.

drop Bank* crisisJST aftermath syst_bankcrisis_start 

save panel, replace
clear

************************************************************************************************************************************************************************************************
****Inflation
************************************************************************************************************************************************************************************************

import excel using data/CC_Inflation1500-2009_redux, sheet("A-Z Values Indices Stata") firstrow clear 
drop if Year == . | Year <1870
destring *cc*, replace ignore(",") force
rename Year year

foreach x in arg aut bol bra chl chn col ecu egy grc hun idn ind irl kor /// 
             mex mys nzl per phl pry pol rou rus tha twn tur ury ven zaf {
    preserve
	gen iso="`x'"
	keep iso year `x'cc* 
	save _crp`x', replace
	restore
	}

local allfiles : dir . files "_crp*"
		foreach file of local allfiles {
				preserve
				use `file', clear
				save _temp, replace
				restore
				append using _temp, force
				} 

dropmiss, force
replace iso = upper(iso)
save _cc, replace
	
use panel, clear

merge 1:1 iso year using _cc
drop if _merge==2
drop _merge
cap !del _*
cap !rm _*.dta

merge 1:1 iso year using data/JSTdatasetR4, keepusing(cpi)
drop if _merge==2
drop _merge

merge 1:1 iso year using data/WEOOct2018all_pcpi
drop if _merge==2
drop _merge

merge 1:1 country year using "data/IFS_05-08-2019 14-21-40-26", keepusing(pricesconsumerpriceindexall)
drop if _merge==2
drop _merge

cap foreach c in arg aus aut bel bol bra bgr can chl chn col hrv cyp cze dnk ecu egy est fin	///
	fra	deu	grc	hun	isl	ind	idn	irl	isr	ita	jpn lva	ltu	lux	mys	mlt	mex	nld	nzl	nor	pry	per	///
	phl	pol	prt	rou	rus	svk	svn	zaf	kor	esp	swe che	twn	tha	tur	gbr	usa	ury	ven {

			cap forvalues i=1/9 {			
					cap gen crp`i'=.
					cap replace crp`i' = `c'cc`i' if `c'cc`i'!=. 
				}
		}

rename pricesconsumerpriceindexall cpri

			 forvalues i=1900/2019 {
				gen byte _b`i' = 1 if year == `i'
				}	
								
merge 1:1 country year using data/Inflation_CSS 
drop if _merge==2
drop _merge 
gen chs = 1 if iso=="CHN" & year>=1978 & year<=1999
bys iso (_b1985): replace crp1 = (cpicss / cpicss[1])*100 if chs==1
drop chs cpicss 

gen _ARG = pcpi if iso =="ARG"
gen _BRA = cpri if iso =="BRA"
gen _RUS = cpri if iso =="RUS"
gen _TWN = pcpi if iso =="TWN"
bys iso (_b2009): gen _CHL = (crp3 / crp3[1])*100 if iso == "CHL"
bys iso (_b1956): gen _BOL = (crp1 / crp1[1])*100 if iso == "BOL" & year>=1936
bys iso (_b1999): gen _CHN = (crp1 / crp1[1])*100 if iso == "CHN" & year>=1987
bys iso (_b1951): gen _ECU = (crp2 / crp2[1])*100 if iso == "ECU" & year>=1938
bys iso (_b1957): gen _IDN = (crp3 / crp3[1])*100 if iso == "IDN" & year<=1957
bys iso (_b1950): gen _IND = (crp3 / crp3[1])*100 if iso == "IND" & year<=1950
bys iso (_b1953): gen _TUR = (crp2 / crp2[1])*100 if iso == "TUR" & year<=1953
bys iso (_b1980): gen _VEN = (crp2 / crp2[1])*100 if iso == "VEN" & year<=1980
bys iso (_b1950): gen _NZL = (crp1 / crp1[1])*100 if iso == "NZL" & year<=1950 
bys iso (_b1948): gen _AU1 = (crp7 / crp7[1])*100 if iso == "AUT" & year>=1923
bys iso (_b1900): gen _CO1 = (crp2 / crp2[1])*100 if iso == "COL" & year<=1900
bys iso (_b1929): gen _UR1 = (crp1 / crp1[1])*100 if iso == "URY" & year<=1929
bys iso (_b1952): gen _KOR = (crp1 / crp1[1])*100 if iso == "KOR" & year<=1952 & year>=1951
bys iso (_b1950): gen _MYS = (crp1 / crp1[1])*100 if iso == "MYS" & year<=1950 & year>=1949
bys iso (_b1950): gen _PER = (crp1 / crp1[1])*100 if iso == "PER" & year<=1950 & year>=1913
bys iso (_b1950): gen _PHL = (crp1 / crp1[1])*100 if iso == "PHL" & year<=1950 & year>=1945
bys iso (_b1950): gen _PRY = (crp1 / crp1[1])*100 if iso == "PRY" & year<=1950 & year>=1938
bys iso (_b1953): gen _THA = (crp1 / crp1[1])*100 if iso == "THA" & year<=1953 & year>=1948
bys iso (_b1945): gen _GR1 = (crp1 / crp1[1])*100 if iso == "GRC" & year<=1945 & year>=1944
bys iso (_b1948): gen _IR1 = (crp1 / crp1[1])*100 if iso == "IRL" & year<=1948 & year>=1922
bys iso (_b1900): gen _ME1 = (crp3 / crp3[1])*100 if iso == "MEX" & year<=1900 & year>=1877
bys iso (_b1938): gen _ZA1 = (crp1 / crp1[1])*100 if iso == "ZAF" & year<=1938 & year>=1895
bys iso (_b1952): replace _KOR = (cpri / cpri[1])*100 if iso == "KOR" & _KOR ==. & year>=1952
bys iso (_b1950): replace _MYS = (cpri / cpri[1])*100 if iso == "MYS" & _MYS ==. & year>=1950
bys iso (_b1950): replace _PER = (cpri / cpri[1])*100 if iso == "PER" & _PER ==. & year>=1950
bys iso (_b1950): replace _PHL = (cpri / cpri[1])*100 if iso == "PHL" & _PHL ==. & year>=1950
bys iso (_b1950): replace _PRY = (cpri / cpri[1])*100 if iso == "PRY" & _PRY ==. & year>=1950
bys iso (_b1953): replace _THA = (cpri / cpri[1])*100 if iso == "THA" & _THA ==. & year>=1953
bys iso (_b2009): replace _CHL = (cpri / cpri[1])*100 if iso == "CHL" & _CHL ==. & year>=2009
bys iso (_b1956): replace _BOL = (cpri / cpri[1])*100 if iso == "BOL" & _BOL ==. & year>=1956
bys iso (_b1999): replace _CHN = (cpri / cpri[1])*100 if iso == "CHN" & _CHN ==. & year>=1999
bys iso (_b1951): replace _ECU = (cpri / cpri[1])*100 if iso == "ECU" & _ECU ==. & year>=1951
bys iso (_b1957): replace _IDN = (cpri / cpri[1])*100 if iso == "IDN" & _IDN ==. & year>=1957
bys iso (_b1950): replace _IND = (cpri / cpri[1])*100 if iso == "IND" & _IND ==. & year>=1950
bys iso (_b1953): replace _TUR = (cpri / cpri[1])*100 if iso == "TUR" & _TUR ==. & year>=1953
bys iso (_b1980): replace _VEN = (pcpi / pcpi[1])*100 if iso == "VEN" & _VEN ==. & year>=1980
bys iso (_b1950): replace _NZL = (cpri / cpri[1])*100 if iso == "NZL" & _NZL ==. & year>=1950
bys iso (_b1948): replace _AU1 = (crp8 / crp8[1])*100 if iso == "AUT" & _AU1 ==. & year>=1948 & year<=1965
bys iso (_b1900): replace _CO1 = (crp3 / crp3[1])*100 if iso == "COL" & _CO1 ==. & year>=1900 & year<=1950
bys iso (_b1929): replace _UR1 = (crp2 / crp2[1])*100 if iso == "URY" & _UR1 ==. & year>=1929 & year<=1950
bys iso (_b1945): replace _GR1 = (crp2 / crp2[1])*100 if iso == "GRC" & _GR1 ==. & year>=1945 & year<=1950
bys iso (_b1948): replace _IR1 = (crp2 / crp2[1])*100 if iso == "IRL" & _IR1 ==. & year>=1948 & year<=1950
bys iso (_b1900): replace _ME1 = (crp1 / crp1[1])*100 if iso == "MEX" & _ME1 ==. & year>=1900 & year<=1950
bys iso (_b1938): replace _ZA1 = (crp2 / crp2[1])*100 if iso == "ZAF" & _ZA1 ==. & year>=1938 & year<=1957
bys iso (_b1965): gen _AUT = (_AU1 / _AU1[1])*100 if iso == "AUT"
bys iso (_b1950): gen _COL = (_CO1 / _CO1[1])*100 if iso == "COL"
bys iso (_b1950): gen _URY = (_UR1 / _UR1[1])*100 if iso == "URY"   
bys iso (_b1950): gen _GRC = (_GR1 / _GR1[1])*100 if iso == "GRC" 
bys iso (_b1950): gen _IRL = (_IR1 / _IR1[1])*100 if iso == "IRL"
bys iso (_b1950): gen _MEX = (_ME1 / _ME1[1])*100 if iso == "MEX"
bys iso (_b1957): gen _ZAF = (_ZA1 / _ZA1[1])*100 if iso == "ZAF" 
bys iso (_b1965): replace _AUT = (cpri / cpri[1])*100 if iso == "AUT" & _AUT ==. & year>=1965
bys iso (_b1950): replace _COL = (cpri / cpri[1])*100 if iso == "COL" & _COL ==. & year>=1950
bys iso (_b1950): replace _URY = (cpri / cpri[1])*100 if iso == "URY" & _URY ==. & year>=1950
bys iso (_b1950): replace _GRC = (cpri / cpri[1])*100 if iso == "GRC" & _GRC ==. & year>=1950
bys iso (_b1950): replace _IRL = (cpri / cpri[1])*100 if iso == "IRL" & _IRL ==. & year>=1950
bys iso (_b1950): replace _MEX = (cpri / cpri[1])*100 if iso == "MEX" & _MEX ==. & year>=1950
bys iso (_b1957): replace _ZAF = (cpri / cpri[1])*100 if iso == "ZAF" & _ZAF ==. & year>=1957
bys iso (_b1948): gen _PO1 = (crp2  / crp2[1])*100 if iso == "POL" & year<=1948  & year>=1945
bys iso (_b1938): gen _EG1 = (crp3  / crp3[1])*100 if iso == "EGY" & year<=1938
bys iso (_b1948): gen _HU1 = (crp1  / crp1[1])*100 if iso == "HUN" & year<=1948
bys iso (_b1938): replace _EG1 = (crp2 / crp2[1])*100 if iso == "EGY" & _EG1 ==. & year>=1938 & year <=1950
bys iso (_b1948): replace _PO1 = (crp3 / crp3[1])*100 if iso == "POL" & _PO1 ==. & year>=1948 & year <=1950
bys iso (_b1948): replace _HU1 = (crp2 / crp2[1])*100 if iso == "HUN" & _HU1 ==. & year>=1948 & year <=1965
bys iso (_b1950): gen _EG2 = (_EG1 / _EG1[1])*100 if iso == "EGY" 
bys iso (_b1950): gen _PO2 = (_PO1 / _PO1[1])*100 if iso == "POL" 
bys iso (_b1965): gen _HU2 = (_HU1 / _HU1[1])*100 if iso == "HUN" 
bys iso (_b1950): replace _EG2 = (cpri / cpri[1])*100 if iso == "EGY" & _EG2 ==. &  year>=1950 & year<=1980
bys iso (_b1950): replace _PO2 = (crp6 / crp6[1])*100 if iso == "POL" & _PO2 ==. &  year>=1950 & year<=1970
bys iso (_b1965): replace _HU2 = (crp3 / crp3[1])*100 if iso == "HUN" & _HU2 ==. &  year>=1965 & year<=1972
bys iso (_b1980): gen _EGY = (_EG2 / _EG2[1])*100 if iso == "EGY"
bys iso (_b1970): gen _POL = (_PO2 / _PO2[1])*100 if iso == "POL" 
bys iso (_b1972): gen _HUN = (_HU2 / _HU2[1])*100 if iso == "HUN"
bys iso (_b1980): replace _EGY = (pcpi / pcpi[1])*100 if iso == "EGY" & _EGY ==. & year>=1980
bys iso (_b1970): replace _POL = (cpri / cpri[1])*100 if iso == "POL" & _POL ==. & year>=1970
bys iso (_b1972): replace _HUN = (cpri / cpri[1])*100 if iso == "HUN" & _HUN ==. & year>=1972

bys iso : egen _mjst = max(cpi)
gen _bas1=.
foreach c in AUS BEL CAN DNK FIN FRA DEU ITA JPN NLD NOR PRT ESP SWE CHE GBR USA {
	replace _bas1 = 1 if year==2016 & iso=="`c'" & _mjst!=.
	}
bys iso (_bas1): gen _cpib = cpi/cpi[1] if _mjst!=.
bys iso (_bas1): gen _pcpib = pcpi/pcpi[1] if _mjst!=.

gen _arw1 = _cpib if _cpib!=. & _mjst!=.
replace _arw1 = _pcpib if _cpib==. & _pcpib!=. & _mjst!=.

gen _arw2=.
foreach g in CYP CZE EST HRV ISL ISR LUX LVA LTU MLT SVK SVN {
	replace _arw2 = cpri if iso=="`g'"
	}

gen _arw3 = pcpi if iso=="BGR" | iso =="ROU"
replace _HUN=. if iso=="HUN" & year>=1945 & year<=1946
 
egen _rm = rowmax (_arw1 _arw2 _arw3 _ARG _BRA _RUS _TWN _AUT _COL _URY _GRC _IRL _MEX _ZAF _POL _EGY ///
                   _HUN _KOR _MYS _PER _PHL _PRY _THA _CHL _BOL _CHN _ECU _IDN _IND _TUR _VEN _NZL )
gen byte _bas2 = 1 if year == 2018
bys iso (_bas2): gen fstcpi = (_rm / _rm[1])*100

merge 1:1 country year using data/Inflation_CSS 
drop if _merge==2
drop _merge 

drop _ARG _BRA _CHN _MYS _PER _KOR _TWN

bys iso (_b1997): gen _ARG = (cpicss / cpicss[1])*100 if iso == "ARG" & year<=1997
bys iso (_b1980): gen _BRA = (cpicss / cpicss[1])*100 if iso == "BRA" & year<=1980
bys iso (_b1987): gen _CHN = (cpicss / cpicss[1])*100 if iso == "CHN" & year<=1987
bys iso (_b1985): gen _HRV = (cpicss / cpicss[1])*100 if iso == "HRV" & year<=1985
bys iso (_b1950): gen _CYP = (cpicss / cpicss[1])*100 if iso == "CYP" & year<=1950
bys iso (_b1952): gen _ISR = (cpicss / cpicss[1])*100 if iso == "ISR" & year<=1952
bys iso (_b1949): gen _MYS = (cpicss / cpicss[1])*100 if iso == "MYS" & year<=1949
bys iso (_b1913): gen _PER = (cpicss / cpicss[1])*100 if iso == "PER" & year<=1913
bys iso (_b1991): gen _SVK = (cpicss / cpicss[1])*100 if iso == "SVK" & year<=1991
bys iso (_b1980): gen _SVN = (cpicss / cpicss[1])*100 if iso == "SVN" & year<=1980
bys iso (_b1951): gen _KOR = (cpicss / cpicss[1])*100 if iso == "KOR" & year<=1951
bys iso (_b1991): gen _CZE = (cpicss / cpicss[1])*100 if iso == "CZE" & year>=1963 & year<=1991
bys iso (_b1980): gen _ROU = (cpicss / cpicss[1])*100 if iso == "ROU" & year>=1970 & year<=1980
bys iso (_b1980): gen _TWN = (cpicss / cpicss[1])*100 if iso == "TWN" & year>=1949 & year<=1980

bys iso (_b1997): replace _ARG = (fstcpi / fstcpi[1])*100 if iso == "ARG" & _ARG ==. & year>=1997
bys iso (_b1980): replace _BRA = (fstcpi / fstcpi[1])*100 if iso == "BRA" & _BRA ==. & year>=1980
bys iso (_b1987): replace _CHN = (fstcpi / fstcpi[1])*100 if iso == "CHN" & _CHN ==. & year>=1987
bys iso (_b1985): replace _HRV = (fstcpi / fstcpi[1])*100 if iso == "HRV" & _HRV ==. & year>=1985
bys iso (_b1950): replace _CYP = (fstcpi / fstcpi[1])*100 if iso == "CYP" & _CYP ==. & year>=1950
bys iso (_b1952): replace _ISR = (fstcpi / fstcpi[1])*100 if iso == "ISR" & _ISR ==. & year>=1952
bys iso (_b1949): replace _MYS = (fstcpi / fstcpi[1])*100 if iso == "MYS" & _MYS ==. & year>=1949
bys iso (_b1913): replace _PER = (fstcpi / fstcpi[1])*100 if iso == "PER" & _PER ==. & year>=1913 
bys iso (_b1991): replace _SVK = (fstcpi / fstcpi[1])*100 if iso == "SVK" & _SVK ==. & year>=1991
bys iso (_b1980): replace _SVN = (fstcpi / fstcpi[1])*100 if iso == "SVN" & _SVN ==. & year>=1980
bys iso (_b1951): replace _KOR = (fstcpi / fstcpi[1])*100 if iso == "KOR" & _KOR ==. & year>=1951 
bys iso (_b1991): replace _CZE = (fstcpi / fstcpi[1])*100 if iso == "CZE" & _CZE ==. & year>=1991
bys iso (_b1980): replace _ROU = (fstcpi / fstcpi[1])*100 if iso == "ROU" & _ROU ==. & year>=1980
bys iso (_b1980): replace _TWN = (fstcpi / fstcpi[1])*100 if iso == "TWN" & _TWN ==. & year>=1980

foreach k in ARG BRA CHN HRV CYP CZE ISR MYS PER ROU SVK SVN KOR TWN {
	replace fstcpi = _`k' if iso=="`k'"
	}

xtset cid year
gen logcpi = log(fstcpi)
bys cid (year): gen inflation = logcpi - l1.logcpi
bys cid (year): gen inflationV2 = logcpi - l1.logcpi 
winsor2 inflationV2 , replace cuts(1  99)
bys cid: replace inflationV2 = L1.inflationV2 if year==2019 
bys cid: ipolate inflationV2 year if iso=="ARG" & year>=2013 & year<=2017, gen(ipo)
replace inflationV2 = ipo if iso=="ARG" & year>=2014 & year<=2016
bys year: egen smean = mean(inflationV2)
replace inflationV2 = smean if inflationV2==. & fstgdp!=. & year>=1888
replace inflationV2 =. if year<=1888
replace inflation = inflationV2 if inflation==. & inflationV2!=. 

drop _*  *cc* *cpi* *pri* crp* ipo smean *V2

save panel, replace

************************************************************************************************************************************************************************************************
****Trade
************************************************************************************************************************************************************************************************

use data/TRADHIST_WP, clear

keep XPTOT_o IPTOT_o iso_o year
drop if iso_o=="" | XPTOT_o ==. | IPTOT_o ==.
duplicates drop
rename XPTOT_o EX
rename IPTOT_o IM
rename iso_o iso
drop if year<=1860

save _exim, replace

use data/TRADHIST_WP, clear

keep GDP_o iso_o year
drop if iso_o=="" 
duplicates drop
rename GDP_o GDP 
rename iso_o iso

merge 1:1 iso year using _exim
drop _merge

replace iso ="AUT" if iso=="AUTHUN"
replace iso="DEU" if iso=="WDEU"
replace iso="ROU" if iso=="ROM"
replace iso="RUS" if iso=="USSR"

gen im_gdp = IM/GDP
gen ex_gdp = EX/GDP

save _trade, replace

use panel, clear

merge 1:1 iso year using _trade, keepusing(ex_gdp im_gdp)
drop if _merge==2
drop _merge

cap !del _*
cap !rm _*.dta

merge 1:1 country year using data/WDI_WB_NE.EXP.GNFS.ZS.dta
drop if _merge==2
drop _merge

merge 1:1 country year using data/WDI_WB_NE.IMP.GNFS.ZS.dta
drop if _merge==2
drop _merge

gen exgdp = ex_gdp    						 
replace ne_exp_gnfs_zs = ne_exp_gnfs_zs/100 	 

xtset cid year
bys cid: gen byte base_exgdp = 1 if year>=2011 & ex_gdp!=. & F1.ex_gdp==.
bys cid (base_exgdp): gen ne_exp_gnfs_zs_i = ne_exp_gnfs_zs/ ne_exp_gnfs_zs[1]
bys cid (base_exgdp): replace exgdp =  ne_exp_gnfs_zs_i*exgdp[1] if year>=2011 & exgdp==. 

gen imgdp = im_gdp    							 
replace ne_imp_gnfs_zs = ne_imp_gnfs_zs/100          

xtset cid year
bys cid: gen byte base_imgdp = 1 if year>=2011 & im_gdp!=. & F1.im_gdp==.
bys cid (base_imgdp): gen ne_imp_gnfs_zs_i = ne_imp_gnfs_zs/ ne_imp_gnfs_zs[1]
bys cid (base_imgdp): replace imgdp =  ne_imp_gnfs_zs_i*imgdp[1] if year>=2011 & imgdp==.

gen tradegdp = exgdp + imgdp 

xtset cid year
forvalues i=2015/2019 {
    replace tradegdp = l.tradegdp if tradegdp==. & year==`i'
}


bys cid: ipolate tradegdp year if iso=="AUT" & year>=1913 & year<=1948, gen(_AUT)
bys cid: ipolate tradegdp year if iso=="BEL" & year>=1913 & year<=1946, gen(_BEL)
bys cid: ipolate tradegdp year if iso=="BGR" & year>=1892 & year<=1948, gen(_BGR)
bys cid: ipolate tradegdp year if iso=="CHN" & year>=1912 & year<=1949, gen(_CHN)
bys cid: ipolate tradegdp year if iso=="EGY" & year>=1938 & year<=1951, gen(_EGY)
bys cid: ipolate tradegdp year if iso=="DEU" & year>=1914 & year<=1948, gen(_DEU)
bys cid: ipolate tradegdp year if iso=="GRC" & year>=1939 & year<=1950, gen(_GRC)
bys cid: ipolate tradegdp year if iso=="HUN" & year>=1941 & year<=1949, gen(_HUN)
bys cid: ipolate tradegdp year if iso=="IDN" & year>=1939 & year<=1949, gen(_IDN)
bys cid: ipolate tradegdp year if iso=="IRL" & year>=1931 & year<=1947, gen(_IRL)
bys cid: ipolate tradegdp year if iso=="ITA" & year>=1942 & year<=1947, gen(_ITA)
bys cid: ipolate tradegdp year if iso=="MEX" & year>=1914 & year<=1919, gen(_MEX)
bys cid: ipolate tradegdp year if iso=="NLD" & year>=1939 & year<=1946, gen(_NLD)
bys cid: ipolate tradegdp year if iso=="ROU" & year>=1950 & year<=1955, gen(_ROU)
bys cid: ipolate tradegdp year if iso=="KOR" & year>=1937 & year<=1950, gen(_KOR)
bys cid: ipolate tradegdp year if iso=="URY" & year>=1914 & year<=1930, gen(_URY)

foreach c in AUT BEL BGR CHN EGY DEU GRC HUN IDN IRL ITA MEX NLD ROU KOR URY {

replace tradegdp = _`c' if iso=="`c'" & _`c'!=.
}

bys cid: ipolate tradegdp year if iso=="RUS" & year>=1913 & year<=1945, gen(_RUS1)
bys cid: ipolate tradegdp year if iso=="RUS" & year>=1990 & year<=1992, gen(_RUS2)

replace tradegdp = _RUS1 if iso=="RUS" & year>=1913 & year<=1945
replace tradegdp = _RUS2 if iso=="RUS" & year>=1990 & year<=1992

egen meantradegdp = mean(tradegdp) if fstgdp!=., by(year)
replace tradegdp = meantradegdp if tradegdp==. & meantradegdp!=.

drop _* im_gdp ex_gdp ne_* exgdp base* imgdp meantradegdp

save panel, replace
clear

************************************************************************************************************************************************************************************************
****Gini
************************************************************************************************************************************************************************************************

import delimited data/swiid8_1_summary
rename gini_disp gini_1
keep country year gini_1
save _swiid8_1, replace
clear

import delimited data/swiid8_3_summary
keep country year gini_disp
rename gini_disp gini_3
save _swiid8_3, replace
clear

use _swiid8_3, clear
merge 1:1 country year using _swiid8_1
keep country year gini* 
replace country = "South Korea" if country =="Korea"
save _giniswiid, replace

use panel, clear

merge 1:1 country year using _giniswiid
drop if _merge==2
drop _merge

rename gini_1 gini
replace gini = gini_3 if iso!="ECU" & iso!="BOL"
replace gini = gini[_n-1] if year==2018 & iso=="ARG"
replace gini = gini[_n-1] if year==2018 & iso=="KOR" 
replace gini = gini[_n-1] if year==2016 & iso=="JPN"
replace gini = . if year==2019 & iso=="GBR"
drop gini_3

cap !del _* 
cap !rm _*.dta

save panel, replace
clear

************************************************************************************************************************************************************************************************
****Debt
************************************************************************************************************************************************************************************************

use panel, clear

merge 1:1 country year using data/debt_RR_2010_appended
drop if _merge==2
drop _merge

replace Tot_gross_central_D_to_GDP = Tot_gross_central_D_to_GDP/100

merge 1:1 country year using data/debt_IMF_Global_Debt_Database, keepusing(cg)
drop if _merge==2
drop _merge

merge 1:1 country year using data/finance_MAURO, keepusing(d)
drop if _merge==2
drop _merge

gen debtgdp = Tot_gross_central_D_to_GDP        
replace cg = cg/100    							   
xtset cid year
bys cid: gen byte base_Tot_gross_central_D_to_GDP= 1 if year>=2008 & Tot_gross_central_D_to_GDP!=. & F1.Tot_gross_central_D_to_GDP==.
bys cid (base_Tot_gross_central_D_to_GDP): gen cg_i = cg/ cg[1]  
bys cid (base_Tot_gross_central_D_to_GDP): replace debtgdp =  cg_i *debtgdp[1] if year>=2008 & debtgdp==. 

replace d = d/100
bys cid: egen count_debtgdp = count(debtgdp)
bys cid: egen count_d_debtgdpzero = count(d) if count_debtgdp==0 
replace debtgdp = cg if count_d_debtgdpzero==0  
 
bys cid: egen count_debtgdp_zero2 = count(debtgdp)
xtset cid year
bys cid: gen byte base_d_zero2= 1 if year>=2011 & d!=. & F1.d==. &  count_debtgdp_zero2==0
bys cid (base_d_zero2): gen cg_i2 = cg/ cg[1]  if  count_debtgdp_zero2==0
bys cid (base_d_zero2): replace d =  cg_i2 *d[1]  if  count_debtgdp_zero2==0 & year>=2011 & d==. 
replace debtgdp = d if count_debtgdp_zero2==0

drop Tot* cg* d base* count_* 

save panel, replace
clear

************************************************************************************************************************************************************************************************
***Labor share
************************************************************************************************************************************************************************************************

use panel, clear

gen countrycode = iso
merge 1:1 countrycode year using data/pwt91, keepusing(labsh)
drop if _merge==2
drop _merge 

rename labsh laborshare
drop countrycode

save panel, replace

************************************************************************************************************************************************************************************************
***Tariffs
************************************************************************************************************************************************************************************************

use data/macro_dataset, clear
keep country year tariff
drop if tariff==.
replace country="South Korea" if country=="Korea"
replace country="Slovakia" if country=="Slovak Republic"
replace country="Taiwan" if country=="Taiwan Province of China"
rename tariff rosetariffs
save _rosetariffs, replace 

import excel using data/WDI_Extract_TAR, firstrow clear 
drop SeriesName SeriesCode CountryName
drop if CountryCode==""
reshape long YR, i(CountryCode) j(year)
rename YR wditariffs
rename CountryCode iso
save _wditariffs, replace 

use panel, clear

merge 1:1 iso year using _wditariffs
drop if _merge==2
drop _merge

merge 1:1 country year using _rosetariffs
drop if _merge==2
drop _merge

bys cid: ipolate wditariffs year, gen(wditariffs_ipo)
replace wditariffs_ipo =. if wditariffs!=.  
replace wditariffs = wditariffs_ipo if wditariffs==.
sort iso year

bys cid: ipolate rosetariffs year, gen(rosetariffs_ipo)
replace rosetariffs_ipo =. if rosetariffs!=.  
replace rosetariffs = rosetariffs_ipo if rosetariffs==.
sort iso year

bys cid: gen byte base = 1 if year==2013  
bys cid (base): gen wditariffs_i  = (wditariffs / wditariffs[1]) if year>=2013
bys cid (base): gen rosetariffsext= wditariffs_i*rosetariffs[1] if year>=2013 
bys cid: replace rosetariffsext = rosetariffs if year<=2013 
drop base wditariffs_i

replace rosetariffsext = rosetariffs if iso=="TWN"
replace rosetariffsext = wditariffs if iso=="CHE"

bys cid: gen byte base = 1 if year==1993 & iso=="HRV"  
bys cid (base): gen wditariffs_i  = (wditariffs / wditariffs[1]) if year<=1993 & iso=="HRV"
bys cid (base): gen rosetariffsexthrv= wditariffs_i*rosetariffsext[1] if year<=1993 & iso=="HRV" 
bys cid: replace rosetariffsext = rosetariffsexthrv  if year<=1993 & iso=="HRV" 

rename rosetariffsext tariffs

cap !del _*
cap !rm _*.dta
drop wdi* base rose*

save panel, replace
clear

************************************************************************************************************************************************************************************************
***Financial openness
************************************************************************************************************************************************************************************************

use panel, clear

merge 1:1 country year using data/KOFGI_2019_data, keepusing(KOFFiGI)
drop if _merge==2
drop _merge

rename KOFFiGI global

save panel, replace
clear

************************************************************************************************************************************************************************************************
***Trade openness
************************************************************************************************************************************************************************************************

use panel, clear

merge 1:1 country year using data/KOFGI_2019_data, keepusing(KOFTrGI)
drop if _merge==2
drop _merge

rename KOFTrGI koftrade

save panel, replace
clear

************************************************************************************************************************************************************************************************
***Social conflicts
************************************************************************************************************************************************************************************************

import excel using "data/2021 Edition CNTSDATA.xlsx", firstrow cellrange(A2:E17918) clear 
replace country="Slovakia" if country=="Slovak Republic"
replace country="Russia" if country=="Russian Federation"
replace country = "Russia" if country=="USSR (Russia)"  
replace country="South Korea" if country=="Korea, South"
replace country="China" if country=="China PR"
replace country="China" if country=="China Republic"
replace country="Austria" if country=="Austria-Hungary"
replace country="Austria" if country=="Austrian Empire"
replace country="Germany" if country=="German FR"
replace country="Czech Republic" if country=="Czechoslovakia"
save _conflict, replace

use panel, clear

merge 1:1 country year using _conflict
drop if _merge==2
drop _merge

sort iso year
foreach p in domestic2 domestic6 domestic8{
forvalues i=1/8{
replace `p' = `p'[_n+`i'] if iso=="SVK" & (year== 1993-`i')
}
}

gen conflicts = domestic2 + domestic6 + domestic8

drop domestic*

cap !del _*
cap !rm _*.dta

save panel, replace
clear
 
************************************************************************************************************************************************************************************************
****Unemployment
************************************************************************************************************************************************************************************************

import excel data/WDI_Extract_UE, sheet("Data_Extract_From_World_Develop") firstrow clear
drop SeriesName
rename CountryName country
rename Time year
rename Value unemployrate
drop in 13238/16231

replace country="Egypt" if country=="Egypt, Arab Rep."
replace country="South Korea" if country=="Korea, Rep."
replace country="Russia" if country=="Russian Federation"
replace country="Slovakia" if country=="Slovak Republic"
replace country="Venezuela" if country=="Venezuela, RB"

save _unemployment, replace

use panel, clear

merge 1:1 country year using _unemployment 
drop if _merge==2
drop _merge

cap !del _*
cap !rm _*.dta

save panel, replace
clear

************************************************************************************************************************************************************************************************
****Consumption
************************************************************************************************************************************************************************************************

import excel data/WDI_Extract_CO.xlsx, sheet("Data") firstrow clear
drop SeriesName
rename CountryName country
rename Time year
rename Value realconsumption
drop in 16493/16497

replace country="Egypt" if country=="Egypt, Arab Rep."
replace country="South Korea" if country=="Korea, Rep."
replace country="Russia" if country=="Russian Federation"
replace country="Slovakia" if country=="Slovak Republic"
replace country="Venezuela" if country=="Venezuela, RB"

save _realconsumption, replace

use panel, clear

merge 1:1 country year using _realconsumption
drop if _merge==2
drop _merge

cap !del _*
cap !rm _*.dta

save panel, replace
clear

************************************************************************************************************************************************************************************************
****Investment
************************************************************************************************************************************************************************************************

import excel data/WDI_Extract_IN.xlsx, sheet("Data") firstrow clear
drop SeriesName
rename CountryName country
rename Time year
rename Value realgrosscapform
drop in 16493/16497

replace country="Egypt" if country=="Egypt, Arab Rep."
replace country="South Korea" if country=="Korea, Rep."
replace country="Russia" if country=="Russian Federation"
replace country="Slovakia" if country=="Slovak Republic"
replace country="Venezuela" if country=="Venezuela, RB"

save _realgrosscapform, replace

use panel, clear

merge 1:1 country year using _realgrosscapform
drop if _merge==2
drop _merge

cap !del _*
cap !rm _*.dta

save panel, replace
clear

************************************************************************************************************************************************************************************************
****Primary balance
************************************************************************************************************************************************************************************************

use panel, clear

merge 1:1 country year using data/finance_MAURO, keepusing(pb)
drop if _merge==2
drop _merge

merge 1:1 country year using data/IMF_GGXONLB_G01_GDP_PT, keepusing(primarynetlendingborrowing)
drop if _merge==2
drop _merge

replace primarynetlendingborrowing = primarynetlendingborrowing/100 
replace pb = pb/100 

bys cid: egen count_pb  = count(pb)
bys cid: egen count_primarynetlendingborrowing  = count(primarynetlendingborrowing)

gen primbal = .
replace primbal = pb                                  if count_pb>0 & count_primarynetlendingborrowing==0
replace primbal = primarynetlendingborrowing    if count_pb==0 & count_primarynetlendingborrowing>0

replace primbal = primarynetlendingborrowing if iso=="CHN"
replace primbal = primarynetlendingborrowing if iso=="HUN" & year>=1995 & year<=2018
replace primbal = pb                               if iso=="HUN" & year>=1928 & year<=1941

gen backward = 1 if (iso=="VEN" | iso=="PRT" | iso=="THA" | iso=="SWE" ) 

xtset cid year
bys cid: gen byte baseven= 1 if year==1990 & iso=="VEN"
bys cid: gen byte baseprt= 1 if year==1990 & iso=="PRT"
bys cid: gen byte baseswe= 1 if year==1990 & iso=="SWE"
bys cid: gen byte basetha= 1 if year==2000 & iso=="THA"

bys cid (baseven): gen pbadjven = pb/ pb[1] if iso=="VEN" 
bys cid (baseprt): gen pbadjprt = pb/ pb[1] if iso=="PRT"  
bys cid (baseswe): gen pbadjswe = pb/ pb[1] if iso=="SWE"  
bys cid (basetha): gen pbadjtha = pb/ pb[1] if iso=="THA"  

bys cid (baseven): replace primbal = pbadjven * primarynetlendingborrowing[1]  if iso=="VEN" & year<=1990
replace primbal = primarynetlendingborrowing if iso=="VEN" & year>1990 & year<=2018

bys cid (baseprt): replace primbal = pbadjprt * primarynetlendingborrowing[1]  if iso=="PRT" & year<=1990
replace primbal = primarynetlendingborrowing if iso=="PRT" & year>1990 & year<=2018 

bys cid (baseswe): replace primbal = pbadjswe * primarynetlendingborrowing[1]  if iso=="SWE" & year<=1990
replace primbal = primarynetlendingborrowing if iso=="SWE" & year>1990 & year<=2018

bys cid (baseswe): replace primbal = pbadjswe * primarynetlendingborrowing[1]  if iso=="SWE" & year<=1990
replace primbal = primarynetlendingborrowing if iso=="THA" & year>2000 & year<=2018

bys cid: egen count_primbal_nonzero  = count(primbal)

xtset cid year
bys cid: gen byte base_imf_nonzero= 1 if year==2011 &  pb!=. & F1.pb==. & count_primbal_nonzero==0
bys cid (base_imf_nonzero): gen primarynetlendingborrowing_i = primarynetlendingborrowing / primarynetlendingborrowing[1]  if count_primbal_nonzero==0 
bys cid (base_imf_nonzero): replace primbal =  primarynetlendingborrowing_i * pb[1]  if count_primbal_nonzero==0  & year>=2011 & pb==.
replace primbal = pb if count_primbal_nonzero==0  & year<=2011

gen pbalance = primbal*100 if year>=1900 & year<=2018

winsor2 pbalance, replace cuts(5 95) trim

drop pb count_* base* pbadj* prim* back*

save panel, replace
clear

************************************************************************************************************************************************************************************************
****Placebo
************************************************************************************************************************************************************************************************

use data/Archigos_4.1_stata14.dta, clear

split startdate, p("-")
drop startdate2 startdate3
destring startdate1, replace
split enddate, p("-")
drop enddate2 enddate3
destring enddate1, replace
keep idacr startdate* leader obsid enddate*
rename idacr iso
rename startdate1 startyear
rename enddate1 endyear

drop if iso=="CZE" | iso=="KOR"
replace iso = "AUS1" if iso=="AUL"
replace iso = "AUT" if iso=="AUS"
replace iso = "AUS" if iso=="AUS1"
replace iso = "BGR" if iso=="BUL"
replace iso = "HRV" if iso=="CRO"
replace iso = "CZE" if iso=="CZR"
replace iso = "DNK" if iso=="DEN"
replace iso = "FRA" if iso=="FRN"
replace iso = "DEU" if iso=="GMY"
replace iso = "DEU" if iso=="GFR"
replace iso = "IDN" if iso=="INS"
replace iso = "IRL" if iso=="IRE"
replace iso = "ISL" if iso=="ICE"
replace iso = "LVA" if iso=="LAT"
replace iso = "LTU" if iso=="LIT"
replace iso = "MYS" if iso=="MAL"
replace iso = "NLD" if iso=="NTH"
replace iso = "NZL" if iso=="NEW"
replace iso = "PRY" if iso=="PAR"
replace iso = "PHL" if iso=="PHI"
replace iso = "PRT" if iso=="POR"
replace iso = "ROU" if iso=="RUM"
replace iso = "SVK" if iso=="SLO"
replace iso = "SVN" if iso=="SLV"
replace iso = "ZAF" if iso=="SAF"
replace iso = "KOR" if iso=="ROK"
replace iso = "ESP" if iso=="SPN"
replace iso = "SWE" if iso=="SWD"
replace iso = "CHE" if iso=="SWZ"
replace iso = "TWN" if iso=="TAW"
replace iso = "THA" if iso=="THI"
replace iso = "GBR" if iso=="UKG"
replace iso = "URY" if iso=="URU"

save _archtemp, replace
use panel, clear 
keep if year == 2000
keep iso country
save _countries, replace
use _archtemp, clear
merge m:1 iso using _countries
drop if _merge==1
drop _merge country

drop if iso=="CHE"
sort iso startyear endyear
replace endyear = 2018 if iso=="ARG" & startyear==2015 & endyear==2015
replace endyear = 2018 if iso=="BEL" & startyear==2014 & endyear==2015
replace endyear = 2018 if iso=="BOL" & startyear==2006 & endyear==2015 
replace endyear = 2018 if iso=="CAN" & startyear==2015 & endyear==2015
replace endyear = 2018 if iso=="CHL" & startyear==2014 & endyear==2015
replace endyear = 2018 if iso=="CHN" & startyear==2012 & endyear==2015
replace endyear = 2018 if iso=="COL" & startyear==2010 & endyear==2015
replace endyear = 2018 if iso=="CYP" & startyear==2013 & endyear==2015
replace endyear = 2018 if iso=="DEU" & startyear==2005 & endyear==2015
replace endyear = 2018 if iso=="DNK" & startyear==2015 & endyear==2015 
replace endyear = 2018 if iso=="EGY" & startyear==2014 & endyear==2015
replace endyear = 2018 if iso=="FIN" & startyear==2012 & endyear==2015 
replace endyear = 2018 if iso=="GRC" & startyear==2015 & endyear==2015
replace endyear = 2018 if iso=="HRV" & startyear==2015 & endyear==2015
replace endyear = 2018 if iso=="HUN" & startyear==2010 & endyear==2015
replace endyear = 2018 if iso=="IDN" & startyear==2014 & endyear==2015
replace endyear = 2018 if iso=="IND" & startyear==2014 & endyear==2015
replace endyear = 2018 if iso=="ISR" & startyear==2009 & endyear==2015
replace endyear = 2018 if iso=="JPN" & startyear==2012 & endyear==2015
replace endyear = 2018 if iso=="LTU" & startyear==2009 & endyear==2015
replace endyear = 2018 if iso=="LUX" & startyear==2013 & endyear==2015
replace endyear = 2018 if iso=="MEX" & startyear==2012 & endyear==2015
replace endyear = 2018 if iso=="MLT" & startyear==2013 & endyear==2015
replace endyear = 2018 if iso=="MYS" & startyear==2009 & endyear==2015
replace endyear = 2018 if iso=="NLD" & startyear==2010 & endyear==2015
replace endyear = 2018 if iso=="NOR" & startyear==2013 & endyear==2015
replace endyear = 2018 if iso=="PRY" & startyear==2013 & endyear==2015
replace endyear = 2018 if iso=="RUS" & startyear==2000 & endyear==2015
replace endyear = 2018 if iso=="SVK" & startyear==2012 & endyear==2015
replace endyear = 2018 if iso=="SVN" & startyear==2014 & endyear==2015
replace endyear = 2018 if iso=="SWE" & startyear==2014 & endyear==2015
replace endyear = 2018 if iso=="THA" & startyear==2014 & endyear==2015
replace endyear = 2018 if iso=="TUR" & startyear==2003 & endyear==2015
replace endyear = 2018 if iso=="URY" & startyear==2015 & endyear==2015
replace endyear = 2018 if iso=="VEN" & startyear==2012 & endyear==2015
replace endyear = 2018 if iso=="AUS" & startyear==2015 & endyear==2015
replace endyear = 2016 if iso=="AUT" & startyear==2008 & endyear==2015
replace endyear = 2017 if iso=="BGR" & startyear==2014 & endyear==2015
replace endyear = 2016 if iso=="BRA" & startyear==2011 & endyear==2015
replace endyear = 2017 if iso=="CZE" & startyear==2014 & endyear==2015
replace endyear = 2017 if iso=="ECU" & startyear==2007 & endyear==2015
replace endyear = 2018 if iso=="ESP" & startyear==2011 & endyear==2015
replace endyear = 2016 if iso=="EST" & startyear==2014 & endyear==2015
replace endyear = 2017 if iso=="FRA" & startyear==2012 & endyear==2015
replace endyear = 2016 if iso=="GBR" & startyear==2010 & endyear==2015
replace endyear = 2017 if iso=="IRL" & startyear==2011 & endyear==2015
replace endyear = 2016 if iso=="ISL" & startyear==2013 & endyear==2015
replace endyear = 2016 if iso=="ITA" & startyear==2014 & endyear==2015
replace endyear = 2017 if iso=="KOR" & startyear==2013 & endyear==2015
replace endyear = 2016 if iso=="NZL" & startyear==2008 & endyear==2015
replace endyear = 2016 if iso=="PER" & startyear==2011 & endyear==2015
replace endyear = 2016 if iso=="PHL" & startyear==2010 & endyear==2015
replace endyear = 2016 if iso=="PRT" & startyear==2006 & endyear==2015
replace endyear = 2016 if iso=="TWN" & startyear==2008 & endyear==2015
replace endyear = 2017 if iso=="USA" & startyear==2009 & endyear==2015
replace endyear = 2018 if iso=="ZAF" & startyear==2009 & endyear==2015
replace endyear = 2016 if iso=="LVA" & startyear==2014 & endyear==2014

expand 2 if iso=="AUS"  & iso!=iso[_n+1]
replace leader="Scott Morrison" if iso[_n+1]==""
replace startyear=2018 if iso[_n+1]==""

expand 2 if iso=="BRA"  & iso!=iso[_n+1]
replace leader="Temer" if iso[_n+1]==""
replace startyear=2016 if iso[_n+1]==""

expand 2 if iso=="COL"  & iso!=iso[_n+1]
replace leader="Duque Marquez" if iso[_n+1]==""
replace startyear=2018 if iso[_n+1]==""

expand 2 if iso=="CZE"  & iso!=iso[_n+1]
replace leader="Babis" if iso[_n+1]==""
replace startyear=2017 if iso[_n+1]==""

expand 2 if iso=="ECU"  & iso!=iso[_n+1]
replace leader="Lenin Moreno" if iso[_n+1]==""
replace startyear=2017 if iso[_n+1]==""

expand 2 if iso=="ESP"  & iso!=iso[_n+1]
replace leader="Pedro Sanchez" if iso[_n+1]==""
replace startyear=2018 if iso[_n+1]==""

expand 2 if iso=="EST"  & iso!=iso[_n+1]
replace leader="Ratas" if iso[_n+1]==""
replace startyear=2016 if iso[_n+1]==""

expand 2 if iso=="FRA"  & iso!=iso[_n+1]
replace leader="Macron" if iso[_n+1]==""
replace startyear=2017 if iso[_n+1]==""

expand 2 if iso=="GBR"  & iso!=iso[_n+1]
replace leader="May" if iso[_n+1]==""
replace startyear=2016 if iso[_n+1]==""

expand 2 if iso=="IRL"  & iso!=iso[_n+1]
replace leader="Varadkar" if iso[_n+1]==""
replace startyear=2017 if iso[_n+1]==""

expand 2 if iso=="KOR"  & iso!=iso[_n+1]
replace leader="Moon Jae-in" if iso[_n+1]==""
replace startyear=2017 if iso[_n+1]==""

expand 2 if iso=="LVA"  & iso!=iso[_n+1]
replace leader="Kucinskis" if iso[_n+1]==""
replace startyear=2016 if iso[_n+1]==""

expand 2 if iso=="MEX"  & iso!=iso[_n+1]
replace leader="Lopez Obrador" if iso[_n+1]==""
replace startyear=2018 if iso[_n+1]==""

expand 2 if iso=="MYS"  & iso!=iso[_n+1]
replace leader="Mahatir Mohamad" if iso[_n+1]==""
replace startyear=2018 if iso[_n+1]==""

expand 2 if iso=="PRT"  & iso!=iso[_n+1]
replace leader="Rebelo de Sousa" if iso[_n+1]==""
replace startyear=2016 if iso[_n+1]==""

expand 2 if iso=="PRY"  & iso!=iso[_n+1]
replace leader="Mario Abdo Benitez" if iso[_n+1]==""
replace startyear=2018 if iso[_n+1]==""

expand 2 if iso=="ROU"  & iso!=iso[_n+1]
replace leader="Klaus Iohannis" if iso[_n+1]==""
replace startyear=2014 if iso[_n+1]==""

expand 2 if iso=="SVN"  & iso!=iso[_n+1]
replace leader="Sarec" if iso[_n+1]==""
replace startyear=2018 if iso[_n+1]==""

expand 2 if iso=="TWN"  & iso!=iso[_n+1]
replace leader="Tsai Ing-wen" if iso[_n+1]==""
replace startyear=2016 if iso[_n+1]==""

expand 2 if iso=="USA"  & iso!=iso[_n+1]
replace leader="Trump" if iso[_n+1]==""
replace startyear=2017 if iso[_n+1]==""

expand 2 if iso=="ZAF"  & iso!=iso[_n+1]
replace leader="Ramaphosa" if iso[_n+1]==""
replace startyear=2018 if iso[_n+1]==""

expand 2 if iso=="ITA"  & iso!=iso[_n+1]
replace leader="Gentiloni" if iso[_n+1]==""
replace startyear=2016 if iso[_n+1]==""

expand 2 if iso=="PER"  & iso!=iso[_n+1]
replace leader="Pedro Kuczynski" if iso[_n+1]==""
replace startyear=2016 if iso[_n+1]==""

expand 2 if iso=="SVK"  & iso!=iso[_n+1]
replace leader="Pellegrini" if iso[_n+1]==""
replace startyear=2018 if iso[_n+1]==""

replace endyear = 2018 in 1919/1942

sort iso startyear endyear 
expand 2 if iso=="AUT"  & iso!=iso[_n+1]
replace leader="Kern" if iso[_n+1]==""
replace startyear=2016 if iso[_n+1]==""
replace endyear=2017 if iso[_n+1]==""

expand 2 if iso=="BGR"  & iso!=iso[_n+1]
replace leader="Gerdschikow" if iso[_n+1]==""
replace startyear=2017 if iso[_n+1]==""
replace endyear=2017 if iso[_n+1]==""

expand 2 if iso=="ISL"  & iso!=iso[_n+1]
replace leader="Johannsson" if iso[_n+1]==""
replace startyear=2016 if iso[_n+1]==""
replace endyear=2017 if iso[_n+1]==""

expand 2 if iso=="NZL"  & iso!=iso[_n+1]
replace leader="Bill English" if iso[_n+1]==""
replace startyear=2016 if iso[_n+1]==""
replace endyear=2017 if iso[_n+1]==""

expand 2 if iso=="PHL"  & iso!=iso[_n+1]
replace leader="Duterte" if iso[_n+1]==""
replace startyear=2016 if iso[_n+1]==""
replace endyear=2018 if iso[_n+1]==""

sort iso startyear endyear 
expand 2 if iso=="ITA"  & iso!=iso[_n+1]
replace leader="Salvini/Di Maio" if iso[_n+1]==""
replace startyear=2018 if iso[_n+1]==""

expand 2 if iso=="PER"  & iso!=iso[_n+1]
replace leader="Vizcarra Cornejo" if iso[_n+1]==""
replace startyear=2018 if iso[_n+1]==""

expand 2 if iso=="AUT"  & iso!=iso[_n+1]
replace leader="Kurz" if iso[_n+1]==""
replace startyear=2017 if iso[_n+1]==""
replace endyear=2018 if iso[_n+1]==""

expand 2 if iso=="BGR"  & iso!=iso[_n+1]
replace leader="Boyko Borisov" if iso[_n+1]==""
replace startyear=2017 if iso[_n+1]==""
replace endyear=2018 if iso[_n+1]==""

expand 2 if iso=="CHL"  & iso!=iso[_n+1]
replace leader="Sebastian Pinera" if iso[_n+1]==""
replace startyear=2018 if iso[_n+1]==""

expand 2 if iso=="ISL"  & iso!=iso[_n+1]
replace leader="Benediktsson II" if iso[_n+1]==""
replace startyear=2017 if iso[_n+1]==""

expand 2 if iso=="NZL"  & iso!=iso[_n+1]
replace leader="Jacinda Ardern" if iso[_n+1]==""
replace startyear=2017 if iso[_n+1]==""
replace endyear=2018 if iso[_n+1]==""

sort iso startyear endyear
expand 2 if iso=="ISL"  & iso!=iso[_n+1]
replace leader="Jakobsdottir" if iso[_n+1]==""
replace endyear=2018 if iso[_n+1]==""

replace endyear = 2013 if iso=="VEN" & leader=="Hugo Chavez"
replace startyear = 2013 if iso=="VEN" & leader=="Maduro" 
replace startyear = 1992 if iso=="SVK" & startyear==1993 & leader=="Meciar" 
 
expand 2 if iso=="SVK" & startyear==1992 & leader=="Meciar" 
replace startyear=1990 if iso=="SVK" & startyear==1992 & iso!=iso[_n+1] 
replace endyear=1991 if iso=="SVK" & startyear==1990
sort iso startyear endyear
expand 2 if iso=="SVK"  & iso!=iso[_n+1]
replace leader = "Carnogursky" if iso[_n+1]==""
replace startyear = 1991 if iso=="SVK" & leader =="Carnogursky"
replace endyear = 1992 if iso=="SVK" & leader =="Carnogursky"

replace endyear = 2007 if iso=="POL" & leader=="Kaczynski"
replace leader = "Kaczynski brothers/PiS" if iso=="POL" & leader=="Kaczynski"
replace leader = "Civic Platform" if iso=="POL" & leader=="Komorowski" & endyear==2015
replace startyear = 2007 if iso=="POL" & leader=="Civic Platform"
replace leader = "PiS (esp. Jaroslaw Kaczynski)" if iso=="POL" & leader=="Komorowski" 
replace startyear = 2015 if iso=="POL" & leader == "PiS (esp. Jaroslaw Kaczynski)" 
replace endyear = 2018 if iso=="POL" & leader == "PiS (esp. Jaroslaw Kaczynski)" 
drop if iso=="POL" & (leader=="Borusewicz" | leader=="Schetyna")

replace leader="Chiang Kai-shek (TWN)" if leader=="Chiang Kai-shek" & iso=="TWN"
replace obsid="" if iso=="SVK" | iso=="POL" | iso=="DEU"
replace startdate="" if iso=="SVK" | iso=="POL" | iso=="DEU"
replace enddate="" if iso=="SVK" | iso=="POL" | iso=="DEU"

sort iso obsid startdate enddate startyear endyear

gen p=.
replace p=1 if iso=="ARG" & leader=="Irigoyen" & startyear==1916 & endyear==1922
replace p=1 if iso=="ARG" & leader=="Irigoyen" & startyear==1928 & endyear==1930
replace p=1 if iso=="ARG" & leader=="Peron" & startyear==1946 & endyear==1955
replace p=1 if iso=="ARG" & leader=="Peron" & startyear==1973 & endyear==1974
replace p=1 if iso=="ARG" & leader=="Peron, Isabel" & startyear==1974 & endyear==1976
replace p=1 if iso=="ARG" & leader=="Menem" & startyear==1989 & endyear==1999
replace p=1 if iso=="ARG" & leader=="Nestor Kirchner" & startyear==2003 & endyear==2007
replace p=1 if iso=="ARG" & leader=="Fernandez de Kirchner" & startyear==2007 & endyear==2015
replace p=1 if iso=="BGR" & leader=="Boyko Borisov" & startyear==2009 & endyear==2013
replace p=1 if iso=="BGR" & leader=="Boyko Borisov" & startyear==2014 & endyear==2017
replace p=1 if iso=="BGR" & leader=="Boyko Borisov" & startyear==2017 & endyear==2018
replace p=1 if iso=="BOL" & leader=="Paz Estenssoro" & startyear==1952 & endyear==1956
replace p=1 if iso=="BOL" & leader=="Siles Zuazo" & startyear==1956 & endyear==1960
replace p=1 if iso=="BOL" & leader=="Paz Estenssoro" & startyear==1960 & endyear==1964
replace p=1 if iso=="BOL" & leader=="Juan Morales" & startyear==2006 & endyear==2018
replace p=1 if iso=="BRA" & leader=="Vargas" & startyear==1930 & endyear==1945
replace p=1 if iso=="BRA" & leader=="Vargas" & startyear==1951 & endyear==1954
replace p=1 if iso=="BRA" & leader=="Mello" & startyear==1990 & endyear==1992
replace p=1 if iso=="CHL" & leader=="Alessandri y Palma" & startyear==1920 & endyear==1924
replace p=1 if iso=="CHL" & leader=="Ibanez del Campo" & startyear==1925 & endyear==1925
replace p=1 if iso=="CHL" & leader=="Alessandri y Palma" & startyear==1925 & endyear==1925
replace p=1 if iso=="CHL" & leader=="Ibanez del Campo" & startyear==1927 & endyear==1931
replace p=1 if iso=="CHL" & leader=="Alessandri y Palma" & startyear==1932 & endyear==1938
replace p=1 if iso=="CHL" & leader=="Ibanez Campo" & startyear==1952 & endyear==1958
replace p=1 if iso=="DEU" & leader=="Hitler" & startyear==1933 & endyear==1945
replace p=1 if iso=="ECU" & leader=="Velasco Ibarra" & startyear==1934 & endyear==1935
replace p=1 if iso=="ECU" & leader=="Velasco Ibarra" & startyear==1944 & endyear==1947
replace p=1 if iso=="ECU" & leader=="Velasco Ibarra" & startyear==1952 & endyear==1956
replace p=1 if iso=="ECU" & leader=="Velasco Ibarra" & startyear==1960 & endyear==1961
replace p=1 if iso=="ECU" & leader=="Velasco Ibarra" & startyear==1968 & endyear==1972
replace p=1 if iso=="ECU" & leader=="Bucaram Ortiz" & startyear==1996 & endyear==1997
replace p=1 if iso=="ECU" & leader=="Rafael Correa" & startyear==2007 & endyear==2017
replace p=1 if iso=="GRC" & leader=="Alexis Tsipras" & startyear==2015 & endyear==2018
replace p=1 if iso=="HUN" & leader=="Orban" & startyear==2010 & endyear==2018
replace p=1 if iso=="IDN" & leader=="Sukarno" & startyear==1945 & endyear==1948
replace p=1 if iso=="IDN" & leader=="Sukarno" & startyear==1949 & endyear==1966
replace p=1 if iso=="IDN" & leader=="Joko Widodo" & startyear==2014 & endyear==2018
replace p=1 if iso=="IND" & leader=="Gandhi, I." & startyear==1966 & endyear==1977
replace p=1 if iso=="IND" & leader=="Narendra Modi" & startyear==2014 & endyear==2018
replace p=1 if iso=="ISR" & leader=="Netanyahu" & startyear==1996 & endyear==1999
replace p=1 if iso=="ISR" & leader=="Netanyahu" & startyear==2009 & endyear==2018
replace p=1 if iso=="ITA" & leader=="Mussolini" & startyear==1922 & endyear==1943
replace p=1 if iso=="ITA" & leader=="Berlusconi" & startyear==1994 & endyear==1995
replace p=1 if iso=="ITA" & leader=="Berlusconi" & startyear==2001 & endyear==2006
replace p=1 if iso=="ITA" & leader=="Berlusconi" & startyear==2008 & endyear==2011
replace p=1 if iso=="ITA" & leader=="Salvini/Di Maio" & startyear==2018 & endyear==2018
replace p=1 if iso=="JPN" & leader=="Junichiro Koizumi" & startyear==2001 & endyear==2006
replace p=1 if iso=="KOR" & leader=="Roh Moo Hyun" & startyear==2003 & endyear==2008
replace p=1 if iso=="MEX" & leader=="Cardenas" & startyear==1934 & endyear==1940
replace p=1 if iso=="MEX" & leader=="Echeverria Alvarez" & startyear==1970 & endyear==1976
replace p=1 if iso=="MEX" & leader=="Lopez Obrador" & startyear==2018 & endyear==2018
replace p=1 if iso=="NZL" & leader=="Muldoon" & startyear==1975 & endyear==1984
replace p=1 if iso=="PER" & leader=="Garcia Perez" & startyear==1985 & endyear==1990
replace p=1 if iso=="PER" & leader=="Fujimori" & startyear==1990 & endyear==2000
replace p=1 if iso=="PHL" & leader=="Estrada" & startyear==1998 & endyear==2001
replace p=1 if iso=="PHL" & leader=="Duterte" & startyear==2016 & endyear==2018
replace p=1 if iso=="POL" & leader=="Kaczynski brothers/PiS" & startyear==2005 & endyear==2007
replace p=1 if iso=="POL" & leader=="PiS (esp. Jaroslaw Kaczynski)" & startyear==2015 & endyear==2018
replace p=1 if iso=="SVK" & leader=="Meciar" & startyear==1990 & endyear==1991
replace p=1 if iso=="SVK" & leader=="Meciar" & startyear==1992 & endyear==1994
replace p=1 if iso=="SVK" & leader=="Meciar" & startyear==1994 & endyear==1998
replace p=1 if iso=="SVK" & leader=="Fico" & startyear==2006 & endyear==2010
replace p=1 if iso=="SVK" & leader=="Fico" & startyear==2012 & endyear==2018
replace p=1 if iso=="THA" & leader=="Thaksin Shinawatra" & startyear==2001 & endyear==2006
replace p=1 if iso=="TUR" & leader=="Erdogan" & startyear==2003 & endyear==2018
replace p=1 if iso=="TWN" & leader=="Chen Shui-bian" & startyear==2000 & endyear==2008
replace p=1 if iso=="USA" & leader=="Trump" & startyear==2017 & endyear==2018
replace p=1 if iso=="VEN" & leader=="Hugo Chavez" & startyear==1999 & endyear==2013
replace p=1 if iso=="VEN" & leader=="Maduro" & startyear==2013 & endyear==2018
replace p=1 if iso=="ZAF" & leader=="Zuma" & startyear==2009 & endyear==2018

drop if startyear<1900
drop if p==1
drop if startyear==endyear & startyear!=2018 & endyear!=2018
drop if iso=="CHE"

bys iso: gen predecessor1 = 1 if (leader==leader[_n+1]) & (startyear[_n+1]<=endyear+1)  
bys iso: gen predecessor2 = 1 if (leader==leader[_n+2]) & (startyear[_n+2]<=endyear+1) & predecessor1 ==.  

drop if predecessor1[_n-1]==1 
drop if predecessor2[_n-2]==1 
drop if predecessor2[_n-1]==1 

keep iso startyear
gen placebo = 1
rename startyear year

save _placebo, replace

use panel, clear

merge 1:1 iso year using _placebo
drop if _merge==2
drop _merge

cap !del _*
cap !rm _*.dta

replace placebo = 0 if placebo==.

save panel, replace
clear 

************************************************************************************************************************************************************************************************
****War
************************************************************************************************************************************************************************************************

use panel, clear

gen war = 0
replace war = 1 if year>=1914 & year<=1918
replace war = 1 if year>=1939 & year<=1945

************************************************************************************************************************************************************************************************
****Independent
************************************************************************************************************************************************************************************************

gen independent = 1 if year >= 1900
replace independent = 0 if iso=="AUS" & year<=1900 & independent==1
replace independent = 0 if iso=="AUT" & year<=1917 & independent==1
replace independent = 0 if iso=="CYP" & year<=1959 & independent==1
replace independent = 0 if iso=="CZE" & year<=1917 & independent==1
replace independent = 0 if iso=="DEU" & (year>=1946 & year<=1948) & independent==1
replace independent = 0 if iso=="EGY" & year<=1921 & independent==1
replace independent = 0 if iso=="EST" & (year<=1917 | (year>=1941 & year<=1990)) & independent==1
replace independent = 0 if iso=="FIN" & year<=1916 & independent==1
replace independent = 0 if iso=="HRV" & year<=1991 & independent==1
replace independent = 0 if iso=="HUN" & year<=1917 & independent==1
replace independent = 0 if iso=="IDN" & year<=1944 & independent==1
replace independent = 0 if iso=="IND" & year<=1946 & independent==1
replace independent = 0 if iso=="IRL" & year<=1920 & independent==1
replace independent = 0 if iso=="ISL" & year<=1943 & independent==1
replace independent = 0 if iso=="ISR" & year<=1947 & independent==1
replace independent = 0 if iso=="KOR" & year<=1947 & independent==1
replace independent = 0 if iso=="LTU" & (year<=1917 | (year>=1941 & year<=1990)) & independent==1
replace independent = 0 if iso=="LVA" & (year<=1917 | (year>=1941 & year<=1990)) & independent==1
replace independent = 0 if iso=="MLT" & year<=1963 & independent==1
replace independent = 0 if iso=="MYS" & year<=1956 & independent==1
replace independent = 0 if iso=="NOR" & year<=1904 & independent==1
replace independent = 0 if iso=="NZL" & year<=1906 & independent==1
replace independent = 0 if iso=="PHL" & year<=1945 & independent==1
replace independent = 0 if iso=="POL" & year<=1917 & independent==1
replace independent = 0 if iso=="SVK" & year<=1992 & independent==1
replace independent = 0 if iso=="SVN" & year<=1991 & independent==1
replace independent = 0 if iso=="TWN" & year<=1948 & independent==1
replace independent = 0 if iso=="ZAF" & year<=1909 & independent==1

************************************************************************************************************************************************************************************************
****Advanced
************************************************************************************************************************************************************************************************

gen advanced = 0
replace advanced = 1 if country=="Australia"
replace advanced = 1 if country=="Austria"
replace advanced = 1 if country=="Belgium"
replace advanced = 1 if country=="Canada"
replace advanced = 1 if country=="Cyprus"
replace advanced = 1 if country=="Czech Republic"
replace advanced = 1 if country=="Denmark"
replace advanced = 1 if country=="Estonia"
replace advanced = 1 if country=="Finland"
replace advanced = 1 if country=="France"
replace advanced = 1 if country=="Germany"
replace advanced = 1 if country=="Greece"
replace advanced = 1 if country=="Ireland"
replace advanced = 1 if country=="Israel"
replace advanced = 1 if country=="Italy"
replace advanced = 1 if country=="Japan"
replace advanced = 1 if country=="South Korea"
replace advanced = 1 if country=="Latvia"
replace advanced = 1 if country=="Lithuania"
replace advanced = 1 if country=="Luxembourg"
replace advanced = 1 if country=="Malta"
replace advanced = 1 if country=="Netherlands"
replace advanced = 1 if country=="New Zealand"
replace advanced = 1 if country=="Norway"	
replace advanced = 1 if country=="Portugal"
replace advanced = 1 if country=="Slovakia"
replace advanced = 1 if country=="Slovenia"
replace advanced = 1 if country=="Spain"
replace advanced = 1 if country=="Sweden"
replace advanced = 1 if country=="Switzerland"
replace advanced = 1 if country=="Taiwan"
replace advanced = 1 if country=="United Kingdom"
replace advanced = 1 if country=="United States"

************************************************************************************************************************************************************************************************
****Populists in power
************************************************************************************************************************************************************************************************

gen pop=0 if year>=1900
replace pop=1 if iso == "ARG" & year >= 1916 & year <= 1922
replace pop=1 if iso == "ARG" & year >= 1928 & year <= 1930
replace pop=1 if iso == "ARG" & year >= 1946 & year <= 1955
replace pop=1 if iso == "ARG" & year >= 1973 & year <= 1974
replace pop=1 if iso == "ARG" & year >= 1974 & year <= 1976 
replace pop=1 if iso == "ARG" & year >= 1989 & year <= 1999
replace pop=1 if iso == "ARG" & year >= 2003 & year <= 2007
replace pop=1 if iso == "ARG" & year >= 2007 & year <= 2015
replace pop=1 if iso == "BGR" & year >= 2009 & year <= 2013
replace pop=1 if iso == "BGR" & year >= 2014 & year <= 2017
replace pop=1 if iso == "BGR" & year >= 2017 & year <= 2020
replace pop=1 if iso == "BOL" & year >= 2006 & year <= 2019
replace pop=1 if iso == "BRA" & year >= 1930 & year <= 1945
replace pop=1 if iso == "BRA" & year >= 1951 & year <= 1954
replace pop=1 if iso == "BRA" & year >= 1990 & year <= 1992
replace pop=1 if iso == "BRA" & year >= 2019 & year <= 2020
replace pop=1 if iso == "CHL" & year >= 1920 & year <= 1924
replace pop=1 if iso == "CHL" & year >= 1925 & year <= 1925
replace pop=1 if iso == "CHL" & year >= 1925 & year <= 1925
replace pop=1 if iso == "CHL" & year >= 1927 & year <= 1931
replace pop=1 if iso == "CHL" & year >= 1932 & year <= 1938
replace pop=1 if iso == "CHL" & year >= 1952 & year <= 1958
replace pop=1 if iso == "DEU" & year >= 1933 & year <= 1945
replace pop=1 if iso == "ECU" & year >= 1934 & year <= 1935
replace pop=1 if iso == "ECU" & year >= 1944 & year <= 1947
replace pop=1 if iso == "ECU" & year >= 1952 & year <= 1956
replace pop=1 if iso == "ECU" & year >= 1960 & year <= 1961
replace pop=1 if iso == "ECU" & year >= 1968 & year <= 1972
replace pop=1 if iso == "ECU" & year >= 1996 & year <= 1997
replace pop=1 if iso == "ECU" & year >= 2007 & year <= 2017
replace pop=1 if iso == "GBR" & year >= 2019 & year <= 2020
replace pop=1 if iso == "GRC" & year >= 2015 & year <= 2019
replace pop=1 if iso == "HUN" & year >= 2010 & year <= 2020
replace pop=1 if iso == "IDN" & year >= 1945 & year <= 1948
replace pop=1 if iso == "IDN" & year >= 1949 & year <= 1966
replace pop=1 if iso == "IDN" & year >= 2014 & year <= 2020
replace pop=1 if iso == "IND" & year >= 2014 & year <= 2020
replace pop=1 if iso == "ISR" & year >= 1996 & year <= 1999
replace pop=1 if iso == "ISR" & year >= 2009 & year <= 2020
replace pop=1 if iso == "ITA" & year >= 1922 & year <= 1943
replace pop=1 if iso == "ITA" & year >= 1994 & year <= 1995
replace pop=1 if iso == "ITA" & year >= 2001 & year <= 2006
replace pop=1 if iso == "ITA" & year >= 2008 & year <= 2011
replace pop=1 if iso == "ITA" & year >= 2018 & year <= 2020
replace pop=1 if iso == "JPN" & year >= 2001 & year <= 2006
replace pop=1 if iso == "KOR" & year >= 2003 & year <= 2008
replace pop=1 if iso == "MEX" & year >= 1934 & year <= 1940
replace pop=1 if iso == "MEX" & year >= 1970 & year <= 1976
replace pop=1 if iso == "MEX" & year >= 2018 & year <= 2020
replace pop=1 if iso == "PER" & year >= 1985 & year <= 1990
replace pop=1 if iso == "PER" & year >= 1990 & year <= 2000
replace pop=1 if iso == "PHL" & year >= 1998 & year <= 2001
replace pop=1 if iso == "PHL" & year >= 2016 & year <= 2020
replace pop=1 if iso == "POL" & year >= 2005 & year <= 2007
replace pop=1 if iso == "POL" & year >= 2015 & year <= 2020
replace pop=1 if iso == "SVK" & year >= 1990 & year <= 1992
replace pop=1 if iso == "SVK" & year >= 1992 & year <= 1994
replace pop=1 if iso == "SVK" & year >= 1994 & year <= 1998
replace pop=1 if iso == "SVK" & year >= 2006 & year <= 2010
replace pop=1 if iso == "SVK" & year >= 2012 & year <= 2018
replace pop=1 if iso == "THA" & year >= 2001 & year <= 2006
replace pop=1 if iso == "TUR" & year >= 2003 & year <= 2020
replace pop=1 if iso == "TWN" & year >= 2000 & year <= 2008
replace pop=1 if iso == "USA" & year >= 2017 & year <= 2020
replace pop=1 if iso == "VEN" & year >= 1999 & year <= 2013
replace pop=1 if iso == "VEN" & year >= 2013 & year <= 2020
replace pop=1 if iso == "ZAF" & year >= 2009 & year <= 2018
replace pop=1 if iso == "IND" & year >= 1966 & year <= 1977
replace pop=1 if iso == "NZL" & year >= 1975 & year <= 1984
replace pop=1 if iso == "BOL" & year >= 1952 & year <= 1964

gen lpop=0 if year>=1900
gen rpop=0 if year>=1900
replace lpop=1 if iso == "ARG" & year >= 1916 & year <= 1922
replace lpop=1 if iso == "ARG" & year >= 1928 & year <= 1930
replace lpop=1 if iso == "ARG" & year >= 1946 & year <= 1955
replace lpop=1 if iso == "ARG" & year >= 1973 & year <= 1974
replace lpop=1 if iso == "ARG" & year >= 1974 & year <= 1976 
replace lpop=1 if iso == "ARG" & year >= 2003 & year <= 2007
replace lpop=1 if iso == "ARG" & year >= 2007 & year <= 2015
replace lpop=1 if iso == "BOL" & year >= 2006 & year <= 2019
replace lpop=1 if iso == "BRA" & year >= 1930 & year <= 1945
replace lpop=1 if iso == "BRA" & year >= 1951 & year <= 1954
replace lpop=1 if iso == "CHL" & year >= 1920 & year <= 1924
replace lpop=1 if iso == "CHL" & year >= 1925 & year <= 1925
replace lpop=1 if iso == "CHL" & year >= 1925 & year <= 1925
replace lpop=1 if iso == "CHL" & year >= 1927 & year <= 1931
replace lpop=1 if iso == "CHL" & year >= 1932 & year <= 1938
replace lpop=1 if iso == "CHL" & year >= 1952 & year <= 1958
replace lpop=1 if iso == "ECU" & year >= 2007 & year <= 2017
replace lpop=1 if iso == "GRC" & year >= 2015 & year <= 2019
replace lpop=1 if iso == "IDN" & year >= 1945 & year <= 1948
replace lpop=1 if iso == "IDN" & year >= 1949 & year <= 1966
replace lpop=1 if iso == "IDN" & year >= 2014 & year <= 2020
replace lpop=1 if iso == "MEX" & year >= 1934 & year <= 1940
replace lpop=1 if iso == "MEX" & year >= 1970 & year <= 1976
replace lpop=1 if iso == "MEX" & year >= 2018 & year <= 2020
replace lpop=1 if iso == "PER" & year >= 1985 & year <= 1990
replace lpop=1 if iso == "PHL" & year >= 1998 & year <= 2001
replace lpop=1 if iso == "SVK" & year >= 2006 & year <= 2010
replace lpop=1 if iso == "SVK" & year >= 2012 & year <= 2018
replace lpop=1 if iso == "VEN" & year >= 1999 & year <= 2013
replace lpop=1 if iso == "VEN" & year >= 2013 & year <= 2020
replace lpop=1 if iso == "ZAF" & year >= 2009 & year <= 2018
replace lpop=1 if iso == "IND" & year >= 1966 & year <= 1977
replace lpop=1 if iso == "BOL" & year >= 1952 & year <= 1964

replace rpop = 1 if pop==1 & lpop==0

************************************************************************************************************************************************************************************************
****Core populists in power
************************************************************************************************************************************************************************************************

gen popepc = 0 if year >=1900
replace popepc=1 if iso == "ARG" & year >= 1946 & year <= 1955
replace popepc=1 if iso == "ARG" & year >= 1973 & year <= 1976
replace popepc=1 if iso == "ARG" & year >= 1989 & year <= 1999
replace popepc=1 if iso == "ARG" & year >= 2003 & year <= 2015
replace popepc=1 if iso == "BRA" & year >= 1951 & year <= 1954
replace popepc=1 if iso == "BRA" & year >= 1990 & year <= 1992
replace popepc=1 if iso == "CHL" & year >= 1952 & year <= 1958
replace popepc=1 if iso == "ECU" & year >= 1952 & year <= 1956
replace popepc=1 if iso == "ECU" & year >= 1960 & year <= 1961
replace popepc=1 if iso == "ECU" & year >= 1968 & year <= 1972
replace popepc=1 if iso == "ECU" & year >= 1996 & year <= 1997
replace popepc=1 if iso == "ISR" & year >= 1996 & year <= 1999
replace popepc=1 if iso == "ITA" & year >= 1994 & year <= 1995
replace popepc=1 if iso == "ITA" & year >= 2001 & year <= 2011
replace popepc=1 if iso == "JPN" & year >= 2001 & year <= 2006
replace popepc=1 if iso == "KOR" & year >= 2003 & year <= 2008
replace popepc=1 if iso == "MEX" & year >= 1970 & year <= 1976
replace popepc=1 if iso == "PER" & year >= 1985 & year <= 2000
replace popepc=1 if iso == "PHL" & year >= 1998 & year <= 2001
replace popepc=1 if iso == "SVK" & year >= 1990 & year <= 1998
replace popepc=1 if iso == "THA" & year >= 2001 & year <= 2006
replace popepc=1 if iso == "TUR" & year >= 2003 & year <= 2019
replace popepc=1 if iso == "TWN" & year >= 2000 & year <= 2008
replace popepc=1 if iso == "VEN" & year >= 1999 & year <= 2019
replace popepc=1 if iso == "IND" & year >= 1966 & year <= 1977
replace popepc=1 if iso == "NZL" & year >= 1975 & year <= 1984
replace popepc=1 if iso == "BOL" & year >= 1952 & year <= 1964

************************************************************************************************************************************************************************************************
****Core populist takeovers
************************************************************************************************************************************************************************************************

gen atakeover = 0
replace atakeover=1 if iso == "ARG" & year == 1946
replace atakeover=1 if iso == "ARG" & year == 1973 
replace atakeover=1 if iso == "ARG" & year == 1989 
replace atakeover=1 if iso == "ARG" & year == 2003 
replace atakeover=1 if iso == "BRA" & year == 1951
replace atakeover=1 if iso == "BRA" & year == 1990
replace atakeover=1 if iso == "CHL" & year == 1952
replace atakeover=1 if iso == "ECU" & year == 1952
replace atakeover=1 if iso == "ECU" & year == 1960
replace atakeover=1 if iso == "ECU" & year == 1968
replace atakeover=1 if iso == "ECU" & year == 1996
replace atakeover=1 if iso == "ISR" & year == 1996
replace atakeover=1 if iso == "ITA" & year == 1994
replace atakeover=1 if iso == "ITA" & year == 2001
replace atakeover=1 if iso == "JPN" & year == 2001
replace atakeover=1 if iso == "KOR" & year == 2003
replace atakeover=1 if iso == "MEX" & year == 1970
replace atakeover=1 if iso == "PER" & year == 1985
replace atakeover=1 if iso == "PER" & year == 1990
replace atakeover=1 if iso == "PHL" & year == 1998
replace atakeover=1 if iso == "SVK" & year == 1990
replace atakeover=1 if iso == "THA" & year == 2001
replace atakeover=1 if iso == "TUR" & year == 2003
replace atakeover=1 if iso == "TWN" & year == 2000
replace atakeover=1 if iso == "VEN" & year == 1999
replace atakeover=1 if iso == "BOL" & year == 1952
replace atakeover=1 if iso == "IND" & year == 1966
replace atakeover=1 if iso == "NZL" & year == 1975

gen ltakeover=atakeover 
gen rtakeover=atakeover 
replace rtakeover=0 if iso=="ARG" & year==1946 
replace rtakeover=0 if iso=="ARG" & year==1973  
replace ltakeover=0 if iso=="ARG" & year==1989 
replace ltakeover=0 if iso=="BRA" & year==1990  
replace rtakeover=0 if iso=="ARG" & year==2003 
replace ltakeover=0 if iso=="ITA" & year==2001 
replace ltakeover=0 if iso=="ITA" & year==1994 
replace ltakeover=0 if iso=="JPN" & year==2001 
replace ltakeover=0 if iso=="KOR" & year==2003 
replace rtakeover=0 if iso=="PER" & year==1985 
replace ltakeover=0 if iso=="PER" & year==1990 
replace rtakeover=0 if iso=="PHL" & year==1998 
replace ltakeover=0 if iso=="THA" & year==2001 
replace ltakeover=0 if iso=="TUR" & year==2003 
replace ltakeover=0 if iso=="TWN" & year==2000 
replace rtakeover=0 if iso=="BRA" & year==1951 
replace ltakeover=0 if iso=="CHL" & year==1952 
replace rtakeover=0 if iso=="MEX" & year==1970 
replace rtakeover=0 if iso=="VEN" & year==1999 
replace ltakeover=0 if iso=="ISR" & year==1996 
replace ltakeover=0 if iso=="ECU" & year==1952 
replace ltakeover=0 if iso=="ECU" & year==1960 
replace ltakeover=0 if iso=="ECU" & year==1968 
replace ltakeover=0 if iso=="ECU" & year==1996 
replace rtakeover=0 if iso=="IND" & year==1966 
replace rtakeover=0 if iso=="BOL" & year==1952 
replace ltakeover=0 if iso=="NZL" & year==1975 
replace ltakeover=0 if iso=="SVK" & year==1990

************************************************************************************************************************************************************************************************
****Populist aftermath
************************************************************************************************************************************************************************************************

xtset cid year
forvalues h = 1/15 {
	gen ptakeover`h' = (l`h'.atakeover)
	}

gen Post_5 = ptakeover1 + ptakeover2 + ptakeover3 + ptakeover4 + ptakeover5
gen Post_15 = ptakeover1 + ptakeover2 + ptakeover3 + ptakeover4 + ptakeover5 ///
             + ptakeover6 + ptakeover7 + ptakeover8 + ptakeover9 + ptakeover10 ///
		      + ptakeover11 + ptakeover12 + ptakeover13 + ptakeover14 + ptakeover15
			  
drop ptakeover*

replace Post_15 = 1 if Post_15==2
replace Post_15 = 0 if Post_15 ==. & Post_5==0

************************************************************************************************************************************************************************************************
****Polishing
************************************************************************************************************************************************************************************************

label var country "Country"
label var iso  "ISO 3-letter code"
label var year  "Year"
label var cid  "Country ID"
label var fstgdp  "Real GDP per capita (index, 2005=100)"
label var electoral  "Clean elections index"
label var judicial  "Judicial constraints on the executive index"
label var medial  "Alternative sources of information index"
label var institutions  "Electoral, judicial, medial, and Polity score (1st principal component)"
label var bankcrisis  "Banking crisis onset (0-1 dummy)"
label var currcrisis  "Currency crisis onset (0-1 dummy)"
label var debtcrisis  "Sovereign debt crisis onset (0-1 dummy)"
label var inflation  "Year-on-year inflation (log change in the CPI)" 
label var tradegdp  "Trade/GDP"
label var gini  "Gini index"
label var debtgdp  "Debt/GDP"
label var laborshare  "Share of labour compensation in GDP"
label var tariffs  "Import tariff rate (in %)"
label var global  "KOF Financial Globalisation Index"
label var koftrade  "KOF Trade Globalisation Index"
label var conflicts  "Number of riots, strikes and demonstrations per year"
label var unemployrate  "Unemployment, total (% of total labor force)"
label var realconsumption  "Households final consumption expenditure per capita (constant 2015 US$)"
label var realgrosscapform  "Gross capital formation (constant 2015 US$)"
label var pbalance "Primary balance"
label var placebo "Non-populist government takeover (0-1 dummy)"
label var war "World war ongoing (0-1 dummy)"
label var independent "Independent state (0-1 dummy)"
label var advanced "Advanced economy (0-1 dummy)"
label var pop "Populist in power (0-1 dummy), all cases"
label var lpop "Left-wing populist in power (0-1 dummy), all cases"
label var rpop "Right-wing populist in power (0-1 dummy), all cases"
label var popepc "Populist in power (0-1 dummy), core cases"
label var atakeover  "Populist government takeover (0-1 dummy), core cases"
label var ltakeover "Left-wing populist government takeover (0-1 dummy), core cases"
label var rtakeover "Right-wing populist government takeover (0-1 dummy), core cases"
label var Post_5 "5-year aftermath of populist government takeover (0-1 dummy), core cases"
label var Post_15 "15-year aftermath of populist government takeover (0-1 dummy), core cases"
	
format %15s country

cap !del panel.dta
cap !rm panel.dta

save data/ple_dataset.dta, replace

************************************************************************************************************************************************************************************************
****Cleaning backup
************************************************************************************************************************************************************************************************

local os : di c(os)
display "`os'"

if"`c(os)'"=="Windows"{
	
cap erase	_archtemp.dta
cap erase	_barrogdp.dta
cap erase	_cc.dta
cap erase	_conflict.dta
cap erase	_countries.dta
cap erase	_crparg.dta
cap erase	_crpaut.dta
cap erase	_crpbol.dta
cap erase	_crpbra.dta
cap erase	_crpchl.dta
cap erase	_crpchn.dta
cap erase	_crpcol.dta
cap erase	_crpecu.dta
cap erase	_crpegy.dta
cap erase	_crpgrc.dta
cap erase	_crphun.dta
cap erase	_crpidn.dta
cap erase	_crpind.dta
cap erase	_crpirl.dta
cap erase	_crpkor.dta
cap erase	_crpmex.dta
cap erase	_crpmys.dta
cap erase	_crpnzl.dta
cap erase	_crpper.dta
cap erase	_crpphl.dta
cap erase	_crppol.dta
cap erase	_crppry.dta
cap erase	_crprou.dta
cap erase	_crprus.dta
cap erase	_crptha.dta
cap erase	_crptur.dta
cap erase	_crptwn.dta
cap erase	_crpury.dta
cap erase	_crpven.dta
cap erase	_crpzaf.dta
cap erase	_exim.dta
cap erase	_giniswiid.dta
cap erase	_jstgdp.dta
cap erase	_mpdgdp.dta
cap erase	_placebo.dta
cap erase	_polity.dta
cap erase	_realconsumption.dta
cap erase	_realgrosscapform.dta
cap erase	_rosetariffs.dta
cap erase	_sbcs1.dta
cap erase	_sbcs2.dta
cap erase	_sbcs3.dta
cap erase	_sbcs4.dta
cap erase	_swiid8_1.dta
cap erase	_swiid8_3.dta
cap erase	_SystemicBCUpdateII2020.dta
cap erase	_temp.dta
cap erase	_trade.dta
cap erase	_unemployment.dta
cap erase	_wdigdp1.dta
cap erase	_wdigdp2.dta
cap erase	_wditariffs.dta
cap erase   panel.dta

}

clear
