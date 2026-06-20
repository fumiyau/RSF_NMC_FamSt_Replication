use "${ROOT}/Data/NVSS/NVSS_analysis.dta",clear

/**********************************************************************/
/* Father race construction */
/**********************************************************************/
* 01 White (only)
* 02 Black (only)
* 03 AIAN (only)
* 04 Asian Indian (only)
* 05 Chinese (only)
* 06 Filipino (only)
* 07 Japanese (only)
* 08 Korean (only)
* 09 Vietnamese (only)
* 10 Other Asian (only)
* 11 Hawaiian (only)
* 12 Guamanian (only)
* 13 Samoan (only)
* 14 Other Pacific Islander (only)
* 15 More than one race
* 99 Unknown

* 0 Non-Hispanic
* 1 Mexican
* 2 Puerto Rican
* 3 Cuban
* 4 Central or South American
* 5 Dominican
* 6 Other and Unknown Hispanic
* 9 Origin unknown or not stated
 
recode frace15 1=1 2=2 3=4 4/14=5 15=6,gen(frace)
replace frace = 3 if fhispx > 0 & fhispx < 9
recode frace15 1=1 2=2 3=4 4=5 5=6 7/8=6 6=7 9=7 10=8 11/14=9 else=.,gen(fasian)
replace fasian = 3 if fhispx > 0 & fhispx < 9

gen inter = . 
replace inter = 1 if asian == 2 & fasian == 5 // Asian Indian endogamy
replace inter = 1 if asian == 3 & fasian == 6 // East Asian endogamy
replace inter = 1 if asian == 4 & fasian == 7 // Southeast Asian endogamy
replace inter = 2 if asian == 1 & fasian == 3 // White endogamy
replace inter = 2 if race == 5 & frace == 5 & inter == . // Asian racial endogamy
replace inter = 3 if inter ==. & frace != 99 // Intermarriage
replace inter = 4 if frace == 99 // Fatehr race unknown

* Asian endogamy variable
gen endg = 0
replace endg = 1 if inter == 1
replace endg = 1 if race == 5 & frace == 5

/**********************************************************************/
/* Labeling */
/**********************************************************************/
lab def asianl 1 "White" 2 "Asian Indian" 3 "East Asian" 4 "Filipino/Vietnamese" 5"Other Asian" 
lab val asian asianl
lab def racel 1 "White" 2 "Black" 3 "Hispanic" 4 "NAAI" 5"Asian" 6"More than one race"
lab val frace racel
lab def fasianl 1 "White" 2 "Black" 3 "Hispanic" 4 "AIAN" 5 "Asian Indian" 6 "East Asian" 7 "Filipino/Vietnamese"8"Other Asian" 9"Pacific Islander" 
lab val fasian fasianl
lab def edul 1 "Less than high school degree or GED" 2 "High school degree or GED" 3 "Some college or AA degree" 4 "BA degree or higher"
lab val medu edul
lab var nativ "Mother foreign born"
lab var mager "Mother age"
lab var endg "Asian endogamy"

tabulate race,gen(race)
tabulate medu,gen(medu)
tabulate asian,gen(asian)
tabulate inter,gen(inter)
tabulate frace,gen(frace)
tabulate fasian,gen(fasian)

lab var race1 "White"
lab var race2 "Asian"
rename race1 white

lab var asian1 "White"
lab var asian2 "Asian Indian"
lab var asian3 "East Asian"
lab var asian4 "Filipino/Vietnamese"
lab var asian5 "Other Asian"
drop asian1
rename asian _asian

lab var fasian1 "Father: White"
lab var fasian2 "Father: Black"
lab var fasian3 "Father: Hispanic"
lab var fasian4 "Father: AIAN"
lab var fasian5 "Father: Indian Asian"
lab var fasian6 "Father: East Asian"
lab var fasian7 "Father: Filipino/Vietnamese"
lab var fasian8 "Father: Other Asian"
lab var fasian9 "Father: Pacific Islander"
drop fasian1
rename fasian _fasian

ta  _fasian if _asian == 1
ta  _fasian if _asian != 1

lab var inter1 "Asian ethnic endogamy"
lab var inter2 "Asian or White racial endogamy"
lab var inter3 "Racial exogamy"
lab var inter4 "Father race unknown"

lab var medu1 "Less than high school degree or GED"
lab var medu2 "High school degree or GED"
lab var medu3 "Some college or AA degree"
lab var medu4 "BA degree or higher"
rename medu3 socl
rename medu _medu
rename meduc _meduc

lab var nonmar "Non-marital childbearing"

/**********************************************************************/
/* Desc stat */
/**********************************************************************/
quietly estpost tabstat nonmar mager year nativ asian2 asian3 asian4 asian5 medu1 medu2 socl medu4  if _asian ==1 , statistics(mean sd count min max) columns(statistics)
quietly esttab . using `"Results/NMC/Desc_white_`=subinstr("`c(current_date)'"," ","",.)'.csv"', replace cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") obs nonote label plain

quietly estpost tabstat nonmar mager year nativ asian2 asian3 asian4 asian5 medu1 medu2 socl medu4 endg if _asian ==2 , statistics(mean sd count min max) columns(statistics)
quietly esttab . using `"Results/NMC/Desc_indian_`=subinstr("`c(current_date)'"," ","",.)'.csv"', replace cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") obs nonote label plain

quietly estpost tabstat nonmar mager year nativ asian2 asian3 asian4 asian5 medu1 medu2 socl medu4 endg if _asian ==3 , statistics(mean sd count min max) columns(statistics)
quietly esttab . using `"Results/NMC/Desc_east_`=subinstr("`c(current_date)'"," ","",.)'.csv"', replace cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") obs nonote label plain

quietly estpost tabstat nonmar mager year nativ asian2 asian3 asian4 asian5 medu1 medu2 socl medu4 endg if _asian ==4 , statistics(mean sd count min max) columns(statistics)
quietly esttab . using `"Results/NMC/Desc_southeast_`=subinstr("`c(current_date)'"," ","",.)'.csv"', replace cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") obs nonote label plain

quietly estpost tabstat nonmar mager year nativ asian2 asian3 asian4 asian5 medu1 medu2 socl medu4 endg if _asian ==5 , statistics(mean sd count min max) columns(statistics)
quietly esttab . using `"Results/NMC/Desc_other_`=subinstr("`c(current_date)'"," ","",.)'.csv"', replace cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") obs nonote label plain

/**********************************************************************/
/* Regression */
/**********************************************************************/
*eststo: logit nonmar race* medu*
*est sto model1
*eststo: logit nonmar race* medu* c.race*#c.medu*
*est sto model2
*eststo: logit nonmar race* medu* c.race*#c.medu* mager nativ
*est sto model3
*esttab model1 model2 model3 using `"Results/NMC/Logit_NMC_Race_`=subinstr("`c(current_date)'"," ","",.)'.csv"', se scalar(N r2) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) b(3) title("The first birth to women aged 18-49, NVSS 2015-2019") replace label unstack wide

eststo: logit nonmar asian* medu* mager year
est sto model1
eststo: logit nonmar asian* medu* mager year c.asian*#c.medu* 
est sto model2
eststo: logit nonmar asian* medu* mager year c.asian*#c.medu* nativ 
est sto model3
*eststo: logit nonmar asian* medu* c.asian*#c.medu* mager nativ fasian*
*est sto model4
*eststo: logit nonmar asian* medu* c.asian*#c.medu* mager nativ fasian* inter2 inter3
*est sto model5
esttab model1 model2 model3 using `"Results/NMC/Logit_NMC_Asian_`=subinstr("`c(current_date)'"," ","",.)'.csv"', se scalar(N r2) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) b(3) title("The first birth to women aged 18-49, NVSS 2015-2019") replace label unstack wide

eststo: logit nonmar asian* medu* mager year if nativ == 1
est sto model1
eststo: logit nonmar asian* medu* c.asian*#c.medu* mager year if nativ == 1
est sto model2
eststo: logit nonmar asian* medu* mager year if nativ == 0
est sto model3
eststo: logit nonmar asian* medu* c.asian*#c.medu* mager year if nativ == 0 
est sto model4
esttab model1 model2 model3 model4 using `"Results/NMC/Logit_NMC_Asian_Nativ_`=subinstr("`c(current_date)'"," ","",.)'.csv"', se scalar(N r2) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) b(3) title("The first birth to women aged 18-49, NVSS 2015-2019") replace label unstack wide

eststo: logit nonmar asian3 asian4 asian5 medu* c.asian3#c.medu* c.asian4#c.medu* c.asian5#c.medu* mager year if nativ == 1 & endg == 0 & race2 == 1
est sto model1
eststo: logit nonmar asian3 asian4 asian5 medu* c.asian3#c.medu* c.asian4#c.medu* c.asian5#c.medu* mager year if nativ == 1 & endg == 1 & race2 == 1
est sto model2
eststo: logit nonmar asian3 asian4 asian5 medu* c.asian3#c.medu* c.asian4#c.medu* c.asian5#c.medu* mager year if nativ == 0 & endg == 0 & race2 == 1
est sto model3
eststo: logit nonmar asian3 asian4 asian5 medu* c.asian3#c.medu* c.asian4#c.medu* c.asian5#c.medu* mager year if nativ == 0 & endg == 1 & race2 == 1
est sto model4
esttab model1 model2 model3 model4 using `"Results/NMC/Logit_NMC_Asian_Nativ_Endogamy`=subinstr("`c(current_date)'"," ","",.)'.csv"', se scalar(N r2) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) b(3) title("The first birth to women aged 18-49, NVSS 2015-2019") replace label unstack wide

/**********************************************************************/
/* Prediction */
/**********************************************************************/
*logit nonmar i.race##i._medu 
*margins, at(_race=(1 5) _medu=(1(1)4)) atmeans asbalanced saving(`"Results/NMC/margins_Logit_NMC_Race_Model2_`=subinstr("`c(current_date)'"," ","",.)'.dta"',replace)

logit nonmar i._asian##i._medu mager year
margins, at(_asian=(1(1)5) _medu=(1(1)4)) atmeans asbalanced saving(`"Results/NMC/margins_Logit_NMC_Asian_Model2_`=subinstr("`c(current_date)'"," ","",.)'.dta"',replace) 

logit nonmar i._asian##i._medu mager year nativ
margins, at(_asian=(1(1)5) _medu=(1(1)4)) atmeans asbalanced saving(`"Results/NMC/margins_Logit_NMC_Asian_Model3_`=subinstr("`c(current_date)'"," ","",.)'.dta"',replace) 

logit nonmar i._asian##i._medu mager year if nativ == 1
margins, at(_asian=(1(1)5) _medu=(1(1)4)) atmeans asbalanced saving(`"Results/NMC/margins_Logit_NMC_Asian_Foregin_Model1_`=subinstr("`c(current_date)'"," ","",.)'.dta"',replace) 

logit nonmar i._asian##i._medu mager year if nativ == 0
margins, at(_asian=(1(1)5) _medu=(1(1)4)) atmeans asbalanced saving(`"Results/NMC/margins_Logit_NMC_Asian_Native_Model1_`=subinstr("`c(current_date)'"," ","",.)'.dta"',replace) 

logit nonmar i._asian##i._medu mager year if nativ == 0 & endg == 0 & race2 == 1
margins, at(_asian=(2(1)5) _medu=(1(1)4)) atmeans asbalanced saving(`"Results/NMC/margins_Logit_NMC_Asian_Native_Inter_Model2_`=subinstr("`c(current_date)'"," ","",.)'.dta"',replace) 

logit nonmar i._asian##i._medu mager year if nativ == 0 & endg == 1 & race2 == 1
margins, at(_asian=(2(1)5) _medu=(1(1)4)) atmeans asbalanced saving(`"Results/NMC/margins_Logit_NMC_Asian_Native_Endogamy_Model2_`=subinstr("`c(current_date)'"," ","",.)'.dta"',replace) 

logit nonmar i._asian##i._medu mager year if nativ == 1 & endg == 0 & race2 == 1
margins, at(_asian=(2(1)5) _medu=(1(1)4)) atmeans asbalanced saving(`"Results/NMC/margins_Logit_NMC_Asian_Foregin_Inter_Model2_`=subinstr("`c(current_date)'"," ","",.)'.dta"',replace) 

logit nonmar i._asian##i._medu mager year if nativ == 1 & endg == 1 & race2 == 1
margins, at(_asian=(2(1)5) _medu=(1(1)4)) atmeans asbalanced saving(`"Results/NMC/margins_Logit_NMC_Asian_Foregin_Endogamy_Model2_`=subinstr("`c(current_date)'"," ","",.)'.dta"',replace) 

