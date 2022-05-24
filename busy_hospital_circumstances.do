/*generating file with concurrent acute inpatients and inflow of acute patients
for each date and hospital*/
use "file.dta",clear

/*keeping one entry of each hospital*/ 
sort hospital 
by hospital: keep if _n == 1
replace admdate = `startdate'

/*make one record for each date and hospital that is relevant for 
computing occupancy*/ 
expand `number_days'

/*each hospital gets one of each date for comptuing occupancy*/ 
replace admdate = admdate[_n-1]+1 if hospital == hospital[_n-1]
replace admtime = mdyhms(month(admdate), day(admdate), year(admdate), 12,0,0)
gen virtual=1 /*tag to indicate that these are not patients*/

/*append acutal admissions*/ 
append using "file.dta"
keep admdate admtime distime hospital virtual

/*for termination of loop*/
gen LOS = distime-admtime
sum LOS
local maxlos = r(max)

/*compute occupancy*/
sort hospital admtime
local videre       = 1
local i            = 0
gen X              = 0	
gen occupancy_1200 = 0
while `videre' {
	local i = `i'+1
	drop X
	by hospital: gen X = (distime[_n-`i'] > admtime) & (virtual[_n-`i']!=1) & (distime[_n-`i']!=.)
	replace occupancy_1200    = occupancy_1200 + X

	drop X
	by hospital: gen X = (admtime-admtime[_n-`i']<`maxlos')
	
	count if X
	local videre = r(N)
}	

keep if virtual == 1 
keep hospital admdate occupancy_1200
merge m:1 hospital admdate using "newpatientsperday"
drop _merge

sort hospital admdate
rename occupancy_1200 occupancy_1200_day0
by hospital: gen occupancy_1200_day1 = occupancy_1200_day0[_n+1]
by hospital: gen occupancy_1200_day2 = occupancy_1200_day0[_n+2]
by hospital: gen occupancy_1200_day3 = occupancy_1200_day0[_n+3]

rename newpatients newpatients_day0
by hospital: gen newpatients_day1 = newpatients_day0[_n+1]
by hospital: gen newpatients_day2 = newpatients_day0[_n+2]
by hospital: gen newpatients_day3 = newpatients_day0[_n+3]

save "occupancy_1200_and_newpatients", replace

/*add concurrent acute inpatients and inflow of acute patients to file*/
use "file", clear
merge m:1 hospital admdate using "occupancy_1200_and_newpatients"
drop _merge

/* Subtract index patient from indicators */
 forvalues i=0/3 {
	* Dette er tidspunktet beleggene (belegg_hf/bsted_p`i') er regnet på
	gen T = mdyhms(month(admdate+`i'), day(admdate+`i'),year(admdate+`i'),12,0,0)
	
	* Trekk i fra en på belegget dersom indeks var tilstede ved T
	replace occupancy_1200_day`i' =  occupancy_1200_day`i' -1 if admtime<T & distime>T 

	drop T
 }
replace newpatients_day0 = newpatients_day0-1
gen occupancy_4d   = (occupancy_1200_day0+occupancy_1200_day1+occupancy_1200_day2+occupancy_1200_day3)/4
gen newpatients_4d = (newpatients_day0+newpatients_day1+newpatients_day2+newpatients_day3)/4

/*indicator for weekend/holiday in*/
gen weekdays_in = dow(admdate)
recode weekdays_in 0=0 1=1 2=1 3=1 4=1 5=1 6=0 , ge(week_dummy_in)

replace weekdays_in = 0 if publicholiday_in==1
replace week_dummy_in=0 if publicholiday_in==1

/* average occupancy within hospital, year and month*/
generate in_year     = year(admdate)
generate in_month    = month(admdate)

egen id = group(hospital in_year in_month week_dummy_in)

egen mean_occupancy_4d  = mean(occupancy_4d), by(id)
egen sd_occupancy_4d     = sd(occupancy_4d), by(id)

egen mean_newpatients_4d  = mean(newpatients_4d), by(id)
egen mean_newpatients_4d  = sd(newpatients_4d), by(id

save "busynessfile.dta"
 
/* open file with all data on acutely admitted patients, 80 years and older.
merge using busynessfile.dta file*/

/*variables for analyses*/
// hour_in      		admission hour
// hour_out     		discharge hour
// in_date      		date of admission
// out_date     		date of discharge
// death_date	   		date of death
// dtd60  				dead within 60 days from admission
// readmission60		readmitted within 60 days from admission
// night_out 	  		hour_out<8 | hour_out>17
// death_1 			    =1 if death_date<sensor_date 
// age2 				=age*age
// gpcontacts_pre		number of gp contacts 180 days pre admission
// fee_patient			=1 if patient had one or more fee days 
//
// sensor_date         mdy(12,31,2016) // end of follow up
// sensor_out_date		mdy(12,31,2016) // end of follow up, discharge analyses
//
// sensor_date 		    =death_date if death_date<sensor_date 
// sensor_out_date		=out_date if out_date<sensor_out_date 
// 					    replace sensor_out_date=death_date if death_date<out_date 
//  
// out					=1 if sensor_out_date<death_date 
// out_night			=1 if out==1 & night_out==1
// pre_adm_60d			=1 if there was an admission 60 days prior to in_date 
// readmission			days from previous admission
// next_adm			    =admdate + readmission
// fail_readm			=1 if next_adm<sensor_date
// sensor_readm		    =sensor_date
// 					    replace sensor_readm = next_adm if next_adm<sensor_date 
// 					    replace sensor_readm = death_date if //
// 					    next_adm==. & sensor_readmission!=sensor_date
//
// /*subgroups*/ 
// All						=1 for all admissions
// January to March 		=1 if admitted January to March
// April to December		=1 if admitted April to December
// University hospital		=1 if admitted to a university hospital
// Not university hospital  =1 if not admitted to a university hospital
// Surgical patients		=1 if DRGtype surgical
// Medical patients		    =1 if DRGtype not surgical
// 2008 to 2011			    =1 if admitted 2008 to 2011
// 2012 to 2016			    =1 if admitted 2012 to 2016
// Day admission			=1 if hour_in>7 & hour_in<18  
// Night admission 		    =1 if day admission==0
// Weekend admission		=1 if week_dummy_in==0
// Weekday admission		=1 if week_dummy_in==1

/*indicator for doing within-analyses*/
egen id_1 = group(hospital in_year in_month weekdays_in)

gen IV_occupancy_Z      = (occupancy_4d - mean_occupancy_4d)/sd_occupancy_4d 
gen IV_occupancy_Ziqr   = IV_occupancy_Z/1.35

gen IV_newpatients_Z    = (newpatients_4d - mean_newpatients_4d)/sd_newpatients_4d 
gen IV_newpatients_Ziqr = IV_newpatients_Z/1.35

/*assuring that we can measure concurrent admitted patients, 
analyses are done on patients admitted from march 2008*/
keep if in_date>=(mdy(3,1,2008))   

* excluding admissions that occurred within 60 days after an earlier admission*/ 
sort popid admdate 
by popid: gen daysfromprevious = admdate-admdate[_n-1]
drop if daysfromprevious<60         

/*running the analyses*/

/*balance test*/
set more off
tempname memhold
postfile `memhold' str40 XVAR str17 ENHET N BETA LCI UCI str17 TEST using table_balance_ziqr.dta, replace

foreach xvar of varlist IV_newpatients_Ziqr IV_occupancy_Ziqr{     
    xtreg age `xvar'  i.hour_in woman gpcontacts_pre i.pre_adm_60d, i(id_1) fe
	local beta = _b[`xvar']
	local lci  = _b[`xvar']- invnormal(0.975)*_se[`xvar']
	local uci  = _b[`xvar']+ invnormal(0.975)*_se[`xvar']
	local sample = e(N)
   	post `memhold' ("`xvar'") ("Coefficient") (`sample') (`beta') (`lci') (`uci') ("Age")
	
	xtlogit woman `xvar'  i.hour_in age age2 gpcontacts_pre i.pre_adm_60d, i(id_1) fe or
	local beta = exp(_b[`xvar'])
	local se  = _se[`xvar']
	local lci  = exp(_b[`xvar']- invnormal(0.975)*_se[`xvar'])
	local uci  = exp(_b[`xvar']+ invnormal(0.975)*_se[`xvar'])
	local sample = e(N)
   	post `memhold' ("`xvar'") ("OR") (`sample') (`beta') (`lci') (`uci') ("Woman")  
	
	xtreg gpcontacts_pre `xvar' i.hour_in woman age age2 i.pre_adm_60d, i(id_1) fe
	local beta = _b[`xvar']
	local lci  = _b[`xvar']- invnormal(0.975)*_se[`xvar']
	local uci  = _b[`xvar']+ invnormal(0.975)*_se[`xvar']
	local sample = e(N)
   	post `memhold' ("`xvar'") ("Coefficient") (`sample') (`beta') (`lci') (`uci') ("GPpre")
	
	xtlogit pre_adm_60d `xvar' i.hour_in woman age age2 gpcontacts_pre , i(id_1) fe or
	local beta = exp(_b[`xvar'])
	local se  = _se[`xvar']
	local lci  = exp(_b[`xvar']- invnormal(0.975)*_se[`xvar'])
	local uci  = exp(_b[`xvar']+ invnormal(0.975)*_se[`xvar'])
	local sample = e(N)
   	post `memhold' ("`xvar'") ("OR") (`sample') (`beta') (`lci') (`uci') ("Preadmitted") 
	
	}
postclose `memhold'

/*analyses*/
set more off
tempname memhold
postfile `memhold' str40 XVAR str30 SUBGROUP str30 HVA str17 ENHET  N BETA LCI UCI using table_results_ziqr.dta, replace

foreach xvar of varlist IV_newpatients_Ziqr IV_occupancy_Ziqr{ 
foreach subgroup of varlist U_* {     
		stset sensor_date, failure(death_1) enter(time in_date) origin(in_date) exit(time in_date + 60)
		stcox `xvar' i.hour_in woman age age2 gpcontacts_pre pre_adm_60d if `subgroup', strata(id_1) cluster(id_1)
		local hr   = exp(_b[`xvar'])
		local lnhr = _b[`xvar']
		local se   = _se[`xvar']
		local lci  = exp(_b[`xvar']- invnormal(0.975)*_se[`xvar'])
		local uci  = exp(_b[`xvar']+ invnormal(0.975)*_se[`xvar'])
		local sample = e(N)
        post `memhold' ("`xvar'") ("`subgroup'") ("Mortalitet 60D") ("HR") (`sample') (`hr') (`lci') (`uci')
		 
 	    stset sensor_readm, failure(fail_readm) enter(time out_date) origin(in_date) exit(time in_date + 60)
		stcox `xvar' i.hour_in woman age age2 gpcontacts_pre pre_adm_60d if `subgroup', strata(id_1) cluster(id_1)
		local hr   = exp(_b[`xvar'])
		local lnhr = _b[`xvar']
		local se   = _se[`xvar']
		local lci  = exp(_b[`xvar']- invnormal(0.975)*_se[`xvar'])
		local uci  = exp(_b[`xvar']+ invnormal(0.975)*_se[`xvar'])
		local sample = e(N)
      	post `memhold' ("`xvar'") ("`subgroup'") ("Readmission 60D") ("HR") (`sample') (`hr') (`lci') (`uci')
		
		stset sensor_out_date, failure(out) enter(time in_date) origin(in_date) exit(time in_date + 4)
		stcox `xvar' i.hour_in woman age age2 gpcontacts_pre pre_adm_60d if `subgroup', strata(id_1) cluster(id_1)
		local hr   = exp(_b[`xvar'])
		local lnhr = _b[`xvar']
		local se   = _se[`xvar']
		local lci  = exp(_b[`xvar']- invnormal(0.975)*_se[`xvar'])
		local uci  = exp(_b[`xvar']+ invnormal(0.975)*_se[`xvar'])
		local sample = e(N)
      	post `memhold' ("`xvar'") ("`subgroup'") ("Discharged 4 days") ("HR") (`sample') (`hr') (`lci') (`uci')
		
 	    stset sensor_out_date, failure(out_night) enter(time in_date) origin(in_date) exit(time in_date + 4)
 	    stcox `xvar' i.hour_in woman age age2 gpcontacts_pre pre_adm_60d if `subgroup', strata(id_1) cluster(id_1)
		local hr   = exp(_b[`xvar'])
		local lnhr = _b[`xvar']
		local se   = _se[`xvar']
		local lci  = exp(_b[`xvar']- invnormal(0.975)*_se[`xvar'])
		local uci  = exp(_b[`xvar']+ invnormal(0.975)*_se[`xvar'])
		local sample = e(N)
      	post `memhold' ("`xvar'") ("`subgroup'") ("Night discharge 4 dager") ("HR") (`sample') (`hr') (`lci') (`uci')
		
		capture xtlogit ventepasient_mLOS `xvar' i.time_inn woman alder alder2 fastlegekontakter_pre pre_innl_60D if in_year>2011 & `undergruppe', i(id_1) fe  or	
		local or  = exp(_b[`xvar'])
		local se  = _se[`xvar']
		local lci = exp(_b[`xvar']- invnormal(0.975)*_se[`xvar'])
		local uci = exp(_b[`xvar']+ invnormal(0.975)*_se[`xvar'])
		local sample = e(N)
      	post `memhold' ("`xvar'") ("`subgroup'") ("Ventepasient") ("OR") (`sample') (`or') (`lci') (`uci') 
		
		xtreg drg_tot_0_60 `xvar' i.time_inn woman alder alder2 fastlegekontakter_pre pre_innl_60D if `undergruppe', i(id_1) fe
		local beta = _b[`xvar']
		local se   = _se[`xvar']
		local lci  = _b[`xvar']- invnormal(0.975)*_se[`xvar']
		local uci  = _b[`xvar']+ invnormal(0.975)*_se[`xvar']
		local sample = e(N)
		post `memhold' ("`xvar'") ("`subgroup'") ("DRGpoeng 60D") ("Coefficient") (`sample') (`beta') (`lci') (`uci') 	
		
		}
}	
postclose `memhold'

/*test for spline*/
/*inflow of acute patients*/
use "analysisfile.dta" , clear
cd "workdir"

gen IV_spline=round(IV_newpatients_Z*100)/100
sum IV_spline, d

mkspline spl_x=IV_spline, cubic nknots(6) displayknots
levelsof IV_spline, local(levels)
levelsof IV_spline, local(levels2)
sum IV_spline, d
local i = 1
foreach ii of local levels2{
	local i = `i'+1
	if (`ii'> 2)|(`ii'< -2){
		local levels: list levels - ii
	}
}

/*60-days mortality */
stset sensor_date, failure(death_1) enter(time in_date) origin(in_date) exit(time in_date + 60)
stcox spl_x* i.hour_in woman age age2 gpcontacts_pre pre_adm_60d, strata(id_1) cluster(id_1)
quietly xblc spl_x*, covname(IV_spline) at(`levels') reference(0) eform generate(n estimate lower upper) 

twoway rarea lower upper n || line estimate n, xtitle("z-score", size(small)) ytitle("Hazard ratio", size(small)) ylabel(.8 (0.1) 1.2, labsize(small)) ///
xlabel(,labsize(small)) legend(off) title("60-days mortality", size(small))

graph save newpat1, replace

/*60-days readmission*/
drop n estimate lower upper
stset sensor_readm, failure(fail_readm) enter(time out_date) origin(in_date) exit(time in_date + 60)
stcox spl_x* i.time_inn woman alder alder2 fastlegekontakter_pre pre_innl_60D, strata(id_bsted_mnddagiuka) cluster(id_bsted_mnddagiuka)
quietly xblc spl_x*, covname(IV_spline) at(`levels') reference(0) eform generate(n estimate lower upper) 

twoway rarea lower upper n || line estimate n, xtitle("z-score", size(small)) ytitle("Hazard ratio", size(small)) ylabel(.8 (0.1) 1.2, labsize(small)) ///
xlabel(,labsize(small)) legend(off) title("60-days readmission", size(small))

graph save newpat2, replace

/*discharged within 4 days*/
drop n estimate lower upper
stset sensor_out_date, failure(out) enter(time in_date) origin(in_date) exit(time in_date + 4)
stcox spl_x* i.hour_in woman age age2 gpcontacts_pre pre_adm_60d, strata(id_1) cluster(id_1)
quietly xblc spl_x*, covname(IV_spline) at(`levels') reference(0) eform generate(n estimate lower upper) 

twoway rarea lower upper n || line estimate n, xtitle("z-score", size(small)) ytitle("Hazard ratio", size(small)) ylabel(.8 (0.1) 1.2, labsize(small)) ///
xlabel(,labsize(small)) legend(off) title("Dischargred within 4 days", size(small))

graph save newpat3, replace

/*discharged out of daytime working hours*/
drop n estimate lower upper
stset sensor_out_date, failure(out_night) enter(time in_date) origin(in_date) exit(time in_date + 4)
stcox spl_x* i.hour_in woman age age2 gpcontacts_pre pre_adm_60d, strata(id_1) cluster(id_1)
quietly xblc spl_x*, covname(IV_spline) at(`levels') reference(0) eform generate(n estimate lower upper) 

twoway rarea lower upper n || line estimate n, xtitle("z-score", size(small)) ytitle("Hazard ratio", size(small)) ylabel(.8 (0.1) 1.2, labsize(small)) ///
xlabel(,labsize(small)) legend(off) title("Discharged out of daytime working hours", size(small))

graph save newpat4, replace

/*financial penalty*/
drop n estimate lower upper
xtlogit fee_patient spl_x* i.hour_in woman age age2 gpcontacts_pre pre_adm_60d if in_year>2011, i(id_1) fe  or
quietly xblc spl_x*, covname(IV_spline) at(`levels') reference(0) eform generate(n estimate lower upper) 

twoway rarea lower upper n || line estimate n, xtitle("z-score", size(small)) ytitle("Odds ratio", size(small)) ylabel(.8 (0.1) 1.2, labsize(small)) ///
xlabel(,labsize(small)) legend(off) title("Financial penalty*", size(small))

graph save newpat5, replace

/*60-days hospital costs*/
drop n estimate lower upper
gen eurochange=drg_tot_0_60*5075
xtreg eurochange spl_x* i.hour_in woman age age2 gpcontacts_pre pre_adm_60d, i(id_1) fe
quietly xblc spl_x*, covname(IV_spline) at(`levels') reference(0) generate(n estimate lower upper) 

twoway rarea lower upper n || line estimate n, xtitle("z-score", size(small)) ytitle("Euro change", size(small)) ylabel(-1000 (500) 1000, labsize(small)) /// 
xlabel(,labsize(small)) legend(off) title("60-days hospital costs", size(small))

graph save newpat6, replace

graph combine newpat1.gph newpat2.gph newpat3.gph newpat4.gph newpat5.gph newpat6.gph,  title("Inflow of acute patients") saving(newpat_combofig, replace)
graph combine newpat1.gph newpat2.gph,  title("Inflow of acute patients") saving(mortreadmnewpat, replace)

/*test for spline*/
/*concurrent acute inpatients*/

use "analysisfile" , clear
cd "workdir"

gen IV_spline=round(IV_occupancy_Z*100)/100
sum IV_spline, d

mkspline spl_x=IV_spline, cubic nknots(6) displayknots
levelsof IV_spline, local(levels)
levelsof IV_spline, local(levels2)
sum IV_spline, d
local i = 1
foreach ii of local levels2{
	local i = `i'+1
	if (`ii'> 2)|(`ii'< -2){
		local levels: list levels - ii
	}
}

/*60-days mortality*/
stset sensor_date, failure(death_1) enter(time in_date) origin(in_date) exit(time in_date + 60)
stcox spl_x* i.hour_in woman age age2 gpcontacts_pre pre_adm_60d, strata(id_1) cluster(id_1)
quietly xblc spl_x*, covname(IV_spline) at(`levels') reference(0) eform generate(n estimate lower upper) 

twoway rarea lower upper n || line estimate n, xtitle("z-score", size(small)) ytitle("Hazard ratio", size(small)) ylabel(.8 (0.1) 1.2, labsize(small)) ///
xlabel(,labsize(small)) legend(off) title("60-days mortality", size(small))

graph save concpat1, replace

/*60-days readmission*/
drop n estimate lower upper
stset sensor_readm, failure(fail_readm) enter(time out_date) origin(in_date) exit(time in_date + 60)
stcox spl_x* i.hour_in woman age age2 gpcontacts_pre pre_adm_60d, strata(id_1) cluster(id_1)
quietly xblc spl_x*, covname(IV_spline) at(`levels') reference(0) eform generate(n estimate lower upper) 

twoway rarea lower upper n || line estimate n, xtitle("z-score", size(small)) ytitle("Hazard ratio", size(small)) ylabel(.8 (0.1) 1.2, labsize(small)) ///
xlabel(,labsize(small)) legend(off) title("60-days readmission", size(small))

graph save concpat2, replace

/*discharged within 4 days*/
drop n estimate lower upper
stset sensor_out_date, failure(out) enter(time in_date) origin(in_date) exit(time in_date + 4)
stcox spl_x* i.hour_in woman age age2 gpcontacts_pre pre_adm_60d, strata(id_1) cluster(id_1)
quietly xblc spl_x*, covname(IV_spline) at(`levels') reference(0) eform generate(n estimate lower upper) 

twoway rarea lower upper n || line estimate n, xtitle("z-score", size(small)) ytitle("Hazard ratio", size(small)) ylabel(.8 (0.1) 1.2, labsize(small)) ///
xlabel(,labsize(small)) legend(off) title("Dischargred within 4 days", size(small))

graph save concpat3, replace

/*discharged out of daytime working hours*/
drop n estimate lower upper
stset sensor_out_date, failure(out_night) enter(time in_date) origin(in_date) exit(time in_date + 4)
stcox spl_x* i.hour_in woman age age2 gpcontacts_pre pre_adm_60d, strata(id_1) cluster(id_1)
quietly xblc spl_x*, covname(IV_spline) at(`levels') reference(0) eform generate(n estimate lower upper) 

twoway rarea lower upper n || line estimate n, xtitle("z-score", size(small)) ytitle("Hazard ratio", size(small)) ylabel(.8 (0.1) 1.2, labsize(small)) ///
xlabel(,labsize(small)) legend(off) title("Discharged out of daytime working hours", size(small))

graph save concpat4, replace

/*financial penalty*/
drop n estimate lower upper
xtlogit fee_patient spl_x* i.hour_in woman age age2 gpcontacts_pre pre_adm_60d if in_year>2011, i(id_1) fe  or
quietly xblc spl_x*, covname(IV_spline) at(`levels') reference(0) eform generate(n estimate lower upper) 

twoway rarea lower upper n || line estimate n, xtitle("z-score", size(small)) ytitle("Odds ratio", size(small)) ylabel(.8 (0.1) 1.2, labsize(small)) ///
xlabel(,labsize(small)) legend(off) title("Financial penalty*", size(small))

graph save concpat5, replace

/*60-days hospital costs*/
drop n estimate lower upper
gen eurochange=drg_tot_0_60*5075
xtreg eurochange spl_x* i.hour_in woman age age2 gpcontacts_pre pre_adm_60d, i(id_1) fe
quietly xblc spl_x*, covname(IV_spline) at(`levels') reference(0) generate(n estimate lower upper) 

twoway rarea lower upper n || line estimate n, xtitle("z-score", size(small)) ytitle("Euro change", size(small)) ylabel(-1000 (500) 1000, labsize(small)) /// 
xlabel(,labsize(small)) legend(off) title("60-days hospital costs", size(small))

graph save concpat6, replace

graph combine concpat1.gph concpat2.gph concpat3.gph concpat4.gph concpat5.gph concpat6.gph,  title("Concurrent acute inpatients") saving(concpat_combofig, replace)
graph combine concpat1.gph concpat2.gph,  title("Concurrent acute patients") saving(mortreadmconcpat, replace)

/*combining figures on mortality and readmission for both indicators*/
graph combine mortreadmnewpat.gph mortreadmconcpat.gph, rows(2) saving(mortreadm_combofig, replace)
