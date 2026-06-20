cd "${ROOT}/Data/NVSS"

forvalues i=2015/2017{
use "nat`i'us.dta",clear
keep dob_yy mbstate_rec mrace15 frace15 mhisp_r fhisp_r dmar mager lbo_rec tbo_rec meduc feduc restatus mbcntry
gen year = `i'
recode dmar 1=0 2=1,gen(nonmar)
recode mrace15 1=1 2=2 3=4 4/14=5 15=6,gen(race)
destring mhisp_r,force replace
replace race = 3 if mhisp_r > 0 & mhisp_r < 9
recode meduc 1/3=1 4/5=2 6=3 7/8=4 9=.,gen(medu)
recode mbstate_rec 1=0 2=1 3=.,gen(nativ)
recode mrace15 1=1 4=2 5=3 7/8=3 6=4 9=4 10=5 11/14=6 else=.,gen(asian)
rename mhisp_r mhispx
rename fhisp_r fhispx
save "nat`i'us_ed.dta",replace
}

forvalues i=2018/2021{
use "nat`i'us.dta",clear
keep dob_yy mbstate_rec mrace15 frace15 mhispx fhispx dmar mager lbo_rec tbo_rec meduc feduc restatus
gen year = `i'
recode dmar 1=0 2=1,gen(nonmar)
recode mrace15 1=1 2=2 3=4 4/14=5 15=6,gen(race)
destring mhispx,force replace
replace race = 3 if mhispx > 0 & mhispx < 9
recode meduc 1/3=1 4/5=2 6=3 7/8=4 9=.,gen(medu)
recode mbstate_rec 1=0 2=1 3=.,gen(nativ)
recode mrace15 1=1 4=2 5=3 7/8=3 6=4 9=4 10=5 11/14=6 else=.,gen(asian) // 2 south 3 east 4 southeast 5 other 6 Pacific ilander
save "nat`i'us_ed.dta",replace
}

use  "nat2015us_ed.dta", clear
forvalues i=2016/2021{
	append using  "nat`i'us_ed.dta"
}

save "nat2015_2021us.dta",replace

/**********************************************************************/
/* Sample selection */
/**********************************************************************/
keep if race == 1 | race == 5
keep if dob_yy < 2020
keep if lbo_rec == 1
drop if asian == 6 // omit pacific ilanders
drop if mager < 18
drop if mager > 49
drop if restat == 4
drop medu
recode meduc 1/2=1 3=2 4/5=3 6/8=4 9=.,gen(medu)

* Keep no missing
mark nomiss 
markout nomiss nonmar mager race medu nativ
keep if nomiss == 1

save "NVSS_analysis.dta",replace
