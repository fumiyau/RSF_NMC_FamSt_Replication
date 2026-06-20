use "${ROOT}/Data/CPS-ASEC/CPS_analysis.dta", clear

****************************************************************
*** Create dummy variables
****************************************************************
tabulate famst,gen(famst)
tabulate race,gen(race)
tabulate mrace,gen(mrace)
tabulate frace,gen(frace)
tabulate edu,gen(edu)
tabulate medu,gen(medu)
tabulate mimmg,gen(mimmg)
tabulate immg,gen(immg)
tabulate immgx,gen(immgx)
tabulate asian,gen(asian)
tabulate masian,gen(masian)
tabulate fasian,gen(fasian)
tabulate region,gen(region)
destring race asian edu mrace frace masian fasian,force replace

****************************************************************
*** Create race variables
****************************************************************
forvalues i=1/6{
	replace race = `i' if race`i'==1
	replace mrace = `i' if mrace`i'==1
	replace frace = `i' if frace`i'==1
}
forvalues i=1/4{
	replace edu = `i' if edu`i'==1
}
forvalues i=1/5{
	replace asian = `i' if asian`i'==1
	replace asian`i' = 0 if multi == "Multi-racial White or Asian"
	replace masian = `i' if masian`i'==1
	replace fasian = `i' if fasian`i'==1
}
replace asian = 6 if multi == "Multi-racial White or Asian"
recode asian 1/5=0 6=1 ,gen(asian_ML)

rename race1 race_AIAN
rename race2 race_Asian
rename race3 race_Black
rename race4 race_Hispanic
rename race5 race_other
rename race6 race_White
rename edu1 edu_BA
rename edu2 edu_HS
rename edu3 edu_LS
rename edu4 edu_SC
rename medu1 medu_LS
rename medu2 medu_HS
rename medu3 medu_SC
rename medu4 medu_BA
rename asian1 asian_EA
rename asian2 asian_OA
rename asian3 asian_AI
rename asian4 asian_SA
rename asian5 asian_WH

****************************************************************
*** Mother race and ethnicity
****************************************************************
rename mrace1 mrace_AIAN
rename mrace2 mrace_Asian
rename mrace3 mrace_Black
rename mrace4 mrace_Hispanic
rename mrace5 mrace_other
rename mrace6 mrace_White

rename masian1 masian_EA
rename masian2 masian_OA
rename masian3 masian_AI
rename masian4 masian_SA
rename masian5 masian_WH

keep if masian !=. // limit to Asisn/white women
keep if mimmg1 !=.
****************************************************************
*** Intermarriage and endogamy
****************************************************************
gen inter = . 
replace inter = 1 if masian == 3 & fasian == 3 // Asian Indian endogamy
replace inter = 1 if masian == 1 & fasian == 1 // East Asian endogamy
replace inter = 1 if masian == 4 & fasian == 3 // Southeast Asian endogamy
replace inter = 2 if mrace == 6 & frace == 6 // White endogamy
replace inter = 2 if mrace == 3 & frace == 3 // Black endogamy
replace inter = 2 if mrace == 4 & frace == 4 // Hispanic endogamy
replace inter = 2 if mrace == 2 & frace == 2 & inter == . // Asian racial endogamy
replace inter = 3 if inter ==. & frace != . // Intermarriage
replace inter = 3 if racex >= 801 & racex <= 830
replace inter = 4 if frace == . // Fatehr race unknown

gen endg = 0 
replace endg = 1 if inter == 1 
replace endg = 1 if mrace == 2 & frace == 2

****************************************************************
*** Family structure with intermarriage
****************************************************************
gen famstx = famst3
replace famstx = 2 if endg == 1 & famst3 == 1
* 0: Single parent or Two parents, cohabiting
* 1: Married two parents intermarriage
* 2: Married two parents endogamy 
keep if mrace == 2 | mrace == 6

tabulate medu,gen(medu)

****************************************************************
*** Labeling
****************************************************************
lab var medu_LS "Less than high school"
lab var medu_HS "High school"
lab var medu_SC "Some college"
lab var medu_BA "BA+"

lab var masian_EA "East Asian"
lab var masian_OA "Other Asian"
lab var masian_AI "South Asian"
lab var masian_SA "Southeast Asian"
lab var masian_WH "White"

lab var famst1 "Single parent"
lab var famst2 "Two parents, cohabiting"
lab var famst3 "Two parents, married"

lab var mimmg1 "Mother foreign born"
lab var endg "Racial endogamy"

lab var mage "Mother age"
lab var region1 "Midwest"
lab var region2 "Northeast"
lab var region3 "South"
lab var region4 "West"
lab var year "Survey year"
/**********************************************************************/
/* Desc stat */
/**********************************************************************/
quietly estpost tabstat famst1 famst2 famst3 medu_LS medu_HS medu_SC medu_BA mimmg1 mage region1 region2 region3 region4 year if masian_WH ==1 , statistics(mean sd count min max) columns(statistics)
quietly esttab . using `"${ROOT}/Results/FamilyStructure/Desc_white_`=subinstr("`c(current_date)'"," ","",.)'.csv"', replace cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") obs nonote label plain

quietly estpost tabstat famst1 famst2 famst3 medu_LS medu_HS medu_SC medu_BA mimmg1 endg mage region1 region2 region3 region4 year if masian_AI ==1 , statistics(mean sd count min max) columns(statistics)
quietly esttab . using `"${ROOT}/Results/FamilyStructure/Desc_indian_`=subinstr("`c(current_date)'"," ","",.)'.csv"', replace cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") obs nonote label plain

quietly estpost tabstat famst1 famst2 famst3 medu_LS medu_HS medu_SC medu_BA mimmg1 endg mage region1 region2 region3 region4 year if masian_EA ==1 , statistics(mean sd count min max) columns(statistics)
quietly esttab . using `"${ROOT}/Results/FamilyStructure/Desc_east_`=subinstr("`c(current_date)'"," ","",.)'.csv"', replace cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") obs nonote label plain

quietly estpost tabstat famst1 famst2 famst3 medu_LS medu_HS medu_SC medu_BA mimmg1 endg mage region1 region2 region3 region4 year if masian_SA ==1 , statistics(mean sd count min max) columns(statistics)
quietly esttab . using `"${ROOT}/Results/FamilyStructure/Desc_southeast_`=subinstr("`c(current_date)'"," ","",.)'.csv"', replace cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") obs nonote label plain

quietly estpost tabstat famst1 famst2 famst3 medu_LS medu_HS medu_SC medu_BA mimmg1 endg mage region1 region2 region3 region4 year if masian_OA ==1 , statistics(mean sd count min max) columns(statistics)
quietly esttab . using `"${ROOT}/Results/FamilyStructure/Desc_other_`=subinstr("`c(current_date)'"," ","",.)'.csv"', replace cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") obs nonote label plain

/**********************************************************************/
/*  SECTION 1: Asian and White		predicting married two parents
    Notes: */
/**********************************************************************/
logit famst3 masian_AI masian_EA masian_SA masian_OA mage year region2 region3 region4 medu_LS medu_HS medu_BA
est sto model1

logit famst3 masian_AI masian_EA masian_SA masian_OA mage year region2 region3 region4 medu_LS medu_HS medu_BA c.(masian_AI masian_EA masian_SA masian_OA)#c.(medu_LS medu_HS medu_BA)
est sto model2

logit famst3 masian_AI masian_EA masian_SA masian_OA mage year region2 region3 region4 medu_LS medu_HS medu_BA c.(masian_AI masian_EA masian_SA masian_OA)#c.(medu_LS medu_HS medu_BA) mimmg1
est sto model3

esttab model1 model2 model3 using `"${ROOT}/Results/FamilyStructure/FamSt_Logit_Asian_`=subinstr("`c(current_date)'"," ","",.)'.csv"', wide scalar(N r2) se star(+ 0.10 * 0.05 ** 0.01 *** 0.01) b(3) label unstack replace  

/**********************************************************************/
/*  SECTION 2: By generation status
    Notes: */
/**********************************************************************/
logit famst3 masian_AI masian_EA masian_SA masian_OA mage year region2 region3 region4 medu_LS medu_HS medu_BA c.(masian_AI masian_EA masian_SA masian_OA)#c.(medu_LS medu_HS medu_BA) if mimmg1 == 1
est sto model4
logit famst3 masian_AI masian_EA masian_SA masian_OA mage year region2 region3 region4 medu_LS medu_HS medu_BA c.(masian_AI masian_EA masian_SA masian_OA)#c.(medu_LS medu_HS medu_BA) if mimmg1 == 0
est sto model5
esttab model4 model5 using `"${ROOT}/Results/FamilyStructure/FamSt_Logit_Asian_generation_`=subinstr("`c(current_date)'"," ","",.)'.csv"', wide scalar(N r2) se star(+ 0.10 * 0.05 ** 0.01 *** 0.01) b(3) label unstack replace

/**********************************************************************/
/*  SECTION 3: By intermarriage
    Notes: */
/**********************************************************************/
mlogit famstx masian_AI masian_SA masian_OA mage year region2 region3 region4 medu_LS medu_HS medu_BA c.(masian_AI masian_SA masian_OA)#c.(medu_LS medu_HS medu_BA) if mimmg1 == 1 & masian != 5
est sto model1

constraint define 1 [#1]_b[c.masian_SA#c.medu_LS]=0
constraint define 2 [#2]_b[c.masian_SA#c.medu_LS]=0
constraint define 3 [#2]_b[c.masian_AI#c.medu_LS]=0
constraint define 4 [#2]_b[c.masian_OA#c.medu_LS]=0

mlogit famstx masian_AI masian_SA masian_OA mage year region2 region3 region4 medu_LS medu_HS medu_BA c.(masian_AI masian_SA masian_OA)#c.(medu_LS medu_HS medu_BA) if mimmg1 == 0 & masian != 5, constraints(1 2 3 4)
est sto model2

mlogit famstx masian_AI masian_SA masian_OA mage year region2 region3 region4 medu_LS medu_HS medu_BA c.(masian_AI masian_SA masian_OA)#c.(medu_LS medu_HS medu_BA) if masian != 5
est sto model3

esttab model1 model2 model3 using `"${ROOT}/Results/FamilyStructure/FamSt_Logit_Asian_endogamy_`=subinstr("`c(current_date)'"," ","",.)'.csv"', wide scalar(N r2) se star(+ 0.10 * 0.05 ** 0.01 *** 0.01) b(3) label unstack replace

/**********************************************************************/
/*  SECTION 4: Prediction
    Notes: */
/**********************************************************************/
logit famst3 mage year mimmg1 region2 region3 region4 i.masian##i.medu
estpost margins masian#medu, atmeans saving(`"${ROOT}/Results/FamilyStructure/Prediction_Logit_Asian_`=subinstr("`c(current_date)'"," ","",.)'.dta"', replace)

logit famst3 mage year region2 region3 region4 i.masian##i.medu if mimmg1==1
estpost margins masian#medu, atmeans saving(`"${ROOT}/Results/FamilyStructure/Prediction_Logit_Asian_Foreign_`=subinstr("`c(current_date)'"," ","",.)'.dta"', replace)

logit famst3 mage year region2 region3 region4 i.masian##i.medu if mimmg1==0
estpost margins masian#medu, atmeans saving(`"${ROOT}/Results/FamilyStructure/Prediction_Logit_Asian_Native_`=subinstr("`c(current_date)'"," ","",.)'.dta"', replace)

mlogit famstx mage year region2 region3 region4 i.masian##i.medu if mimmg1==1 & masian != 5
estpost margins masian#medu, atmeans saving(`"${ROOT}/Results/FamilyStructure/Prediction_Logit_Asian_Endogamy_Foreign_`=subinstr("`c(current_date)'"," ","",.)'.dta"', replace)

*constraint define 1 [#1]_b[4.masian#1.medu]=0
*constraint define 2 [#2]_b[2.masian#1.medu]=0
*constraint define 3 [#2]_b[3.masian#1.medu]=0
*constraint define 4 [#2]_b[4.masian#1.medu]=0

mlogit famstx mage year region2 region3 region4 i.masian##ib3.medu if mimmg1==0 & masian != 5
estpost margins masian#medu, atmeans saving(`"${ROOT}/Results/FamilyStructure/Prediction_Logit_Asian_Endogamy_Native_`=subinstr("`c(current_date)'"," ","",.)'.dta"', replace)


mlogit famstx mage year region2 region3 region4 i.masian##i.medu mimmg1 if masian != 5
estpost margins masian#medu, atmeans saving(`"${ROOT}/Results/FamilyStructure/Prediction_Logit_Asian_Endogamy_Total_`=subinstr("`c(current_date)'"," ","",.)'.dta"', replace)

* 1 BA 2 HS 3 LS 4 SC
ta masian medu if mimmg1 == 0
ta masian medu if mimmg1 == 1
ta asian edu 

ta masian medu if mimmg1 == 0
ta masian medu if mimmg1 == 1
