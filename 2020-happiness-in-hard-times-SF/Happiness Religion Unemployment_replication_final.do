/************************************** 
Prepared by Orestes P. Hastings (pat.hastings@colostate.edu) 
Last updated November 6, 2020

This do file is to be used with GSS panel data analyzed in:

Hastings, Orestes P. and Kassandra K. Roeser. 2020. “Happiness in Hard Times: Does Religion Buffer the Negative Effect of Unemployment on Happiness?” Social Forces 99(2):447-473.

The GSS files are in long form and have been appended together into GSS_panel0614_happiness_religion.dta.
idnum identifies each respodnent.
panelwave identifies the wave (1, 2, or 3).

This do file using the following user-written Stata modules:
-revrs-
-grstyle-
-zscore-
-coefplot-
-estpost-

Running this do file will create all of the tables and figures from the paper and supplement.
****************************************/

clear all
use GSS_panel0614_happiness_religion.dta

*****************************
* DATA CLEANING
*****************************

// reverse code happiness to higher number is more happy
revrs happy, replace
label define HAPPY 1 "Not too happy" 2 "Pretty happy" 3 "Very happy"
label val happy HAPPY


// generate binary very happy
gen vhappy = (happy == 3)
replace vhappy = happy if happy >=.
label var vhappy "very happy"


// Create work measures
recode wrkstat (4=1 "Unemployed")(1/2=0 "Not Unemployed")(3=.)(5/8=.)(missing=.), gen(unemployed)
lab var unemployed "Unemployed"



// Create religion measures
revrs pray relp rellife, replace

recode relig (4 = 0 "Not religious")(nonmissing = 1 "Religious"), gen(anyrelig)
recode postlife (2 .d = 0 "No afterlife")(1 = 1 "Believes afterlife"), gen(afterlife)
recode pray (1/4 = 0 "Not")(5/6 = 1 "Pray daily or more"), gen(pray_daily)
recode god (1/5 = 0)(6 = 1), gen(god_exists)
lab var god_exists "Believe God exists"


recode rellife (1 = 0)(2 = .33)(3 = .67)(4 = 1), gen(rellife_one)
recode relp (1 = 0)(2 = .33)(3 = .67)(4 = 1), gen(relp_one)


// probability of attendance
gen probATT = . 
replace probATT = .99 if attend == 8
replace probATT = .99 if attend == 7
replace probATT = .85 if attend == 6
replace probATT = .58 if attend == 5
replace probATT = .23 if attend == 4
replace probATT = .05 if attend == 3
replace probATT = .02 if attend == 2
replace probATT = .01 if attend == 1
replace probATT = 0 if attend == 0

recode attend (0/3 = 0)(4/8 = 1), gen(attend_monthly)
lab var attend_monthly "Attend Monthly"

gen raceeth = race
replace raceeth = 4 if hispanic > 1 & !missing(hispanic)
lab def RACE 1 "Non-Hispanic White" 2 "Non-Hispanic Black" 3 "Non-Hispanic Other" 4 "Hispanic"
lab val raceeth RACE

// marital status
lab def MARITAL 1 "Married" 2 "Widowed" 3 "Divorced" 4 "Separated" 5 "Never Married"
lab val marital MARITAL


// list variables to save for the analysis
local vars ///
	year  unemployed attend_monthly  probATT happy vhappy sex educ age marital raceeth childs ///
	 god god_exists bible relig anyrelig  afterlife rellife rellife_one relpersn ///
	 relp_one pray pray_daily 

	 
// reshape
keep `vars' idnum panelwave  wtpan*
reshape wide `vars', i(idnum) j(panelwave)

// generate a constant measure based on first wave
foreach var in `vars' {
	gen I`var' = `var'1  
}


lab val Irellife revrellife
lab val Iraceeth RACE
lab val Imarital MARITAL 
lab def SEX 1 "Male" 2 "Female"
lab val Isex SEX

lab var IprobATT  "Probability of attendance"	
lab var Iattend_monthly "Attend monthly or more"
lab var Ipray_daily "Pray daily or more"
lab var Ipray "Frequency of prayer"
lab var Igod "Beliefs about God"
lab var Igod_exists "Believe God exists"
lab var Iafterlife "Believe in life after death"
lab var Irellife_one "Carry relig beliefs into rest of life"
lab var	Irelp_one "Religious person" 
lab var Ianyrelig "Religious affiliation"

reshape long



*****************************
* ANALYSIS
*****************************

keep if age > 24 & age < 66 // keep if between 25 and 65
xtset idnum panelwave 


// Figures for Appendix
grstyle clear
grstyle init
grstyle set plain, horizontal nogrid

graph bar, over(pray, label(angle(45))) name(figA1, replace)
graph bar, over(god, label(angle(45))) name(figA2, replace)
graph bar rellife_one, over(relpe, label(angle(45))) ytitle("Carry relig beliefs into rest of life") name(figA3, replace)


// standardize happiness to use as outcome variable. 
zscore happy [w = wtpannr12]
gen nonz_happy = happy
replace happy = z_happy


// Figure 1
preserve
collapse happy unemployed [pw = wtpannr12] , by(year)
	
tw (scatter happy year, connect(l) yaxis(1)) (scatter unemployed year, connect(l) lpattern(_) yaxis(2)), ///
	ytitle("Happiness (standard deviations)", axis(1)) ytitle("Unemployment (rate)", axis(2)) xtitle("Year") ///
	title("") name(fig1, replace)

graph export Fig1.pdf, replace
restore

grstyle clear



// flag and drop those who transition from unemployed to employed
gen postunemp = (unemployed == 0 & l.unemployed == 1)
replace postunemp = 1 if (unemployed == 0 & l2.unemployed == 1)
replace postunemp = 1 if (unemployed == 1 & l2.unemployed == 1 & l1.unemployed == 0)

gen preunemp = 0
replace preunemp = 1 if unemployed == 1 & f1.unemployed == 0 & f2.unemployed == 1 

gen preunemp2 = 0
replace preunemp2 = 1 if unemployed == 1 & (f1.unemployed == 0 | f2.unemployed == 0) & panelwave == 1 // unemp now but will be employed later

drop if postunemp == 1
drop if preunemp == 1
drop if preunemp2 == 1 


// listwise deletion
drop if missing(happy)
drop if missing(unemployed) // drop if out of work force or work status unknown
drop if missing(Iraceeth) | missing(Imarital) | missing(Ichilds) | missing(Iage) | missing(Ieduc) 


bysort idnum: egen numwaves = count(idnum) // count how many waves a respondent is in
drop if numwaves == 1 // drop if only in one wave since not useful for panel analysis


global controls sex i.raceeth i.marital c.childs c.age c.educ
global fecontrols Isex i.Iraceeth b5.Imarital c.Ichilds c.Iage c.Ieduc


// Baseline models for Table 2 
eststo base0: reg unemployed happy i.year [pw = wtpannr12], cluster(idnum) 
eststo base1: reg unemployed l.happy i.year if l.unemployed == 0 [pw = wtpannr12], cluster(idnum) 
eststo base2: reg happy unemployed i.year [pw = wtpannr12], cluster(idnum)  
eststo base3: xtreg happy unemployed i.year [pw = wtpannr12], fe 


// Logit version
eststo base0_logit: logit unemployed happy i.year [pw = wtpannr12], cluster(idnum) 
eststo base1_logit: logit unemployed l.happy i.year if l.unemployed == 0 [pw = wtpannr12], cluster(idnum) 
eststo base2_logit: ologit happy unemployed i.year [pw = wtpannr12], cluster(idnum)  


// Table 2 of paper
esttab base0 base1 base2 base3, se se(a2) b(a2) order(happy L.happy unemployed) indicate("Year fixed effects = *year*"  "Constant = _cons")
esttab base0 base1 base2 base3 using regtable1.csv,  replace order(happy L.happy unemployed) se(a2) b(a2) indicate("Year fixed effects = *year*"  "Constant = _cons")

// Table A2: Logit and Ordered Logit Baseline Regression Models
esttab base0_logit base1_logit base2_logit using regtable1_logit.tex, ///
	replace order(happy L.happy unemployed) se(a2) b(a2) indicate("Year fixed effects = *year*"  "Constant = _cons") ///
	frag booktabs  noomit not nogaps label nobase  ///
	star(+ .1 * .05 ** .01 ** .001) noobs scalars("N N(observations)" "N_g N(individuals)") nomtitle 

	
// Fixed effects models ** 
foreach var in probATT rellife_one relp_one pray  god attend_m pray_daily god_exists afterlife anyrelig { 

	eststo `var': xtreg happy   c.unemployed##(c.I`var') i.year [pw = wtpannr12], fe 
	eststo `var'_ctrl: xtreg happy  c.unemployed##(c.I`var') c.unemployed#($fecontrols) i.year [pw = wtpannr12], fe
  	eststo `var'_logit: xtlogit vhappy c.unemployed##(c.I`var' $fecontrols) i.year [w = wtpannr12], fe // logit version
}


// Combined Model
eststo ALL: xtreg happy c.unemployed##c.(IprobATT Irellife_one Irelp_one Ipray_daily Igod_exists Iafterlife Ianyrelig) c.unemployed#($fecontrols) i.year [pw = wtpannr12], fe


// FIGURE 2
grstyle clear
grstyle init
grstyle set plain, horizontal nogrid


#delimit ;
coefplot  
	probATT pray_daily god_exists afterlife anyrelig  relp_one rellife_one
	, 
	bylabel("Separate models" "No controls")
	||
	probATT_ctrl pray_daily_ctrl god_exists_ctrl afterlife_ctrl anyrelig_ctrl relp_one_ctrl rellife_one_ctrl
	, 
	bylabel("Separate models" "With controls") 
	||
	ALL
	,
	bylabel("Single model" "With controls")
	||,
	levels(95 90) pstyle(p1) legend(order(1 "95% Confidence Interval" 2 "90% Confidence Interval")) // scheme(s1color)
	coeflabels(c.unemployed#c.IprobATT = "Probability of attendance" c.unemployed#c.Ipray_daily = "Pray daily or more" 
	c.unemployed#c.Igod_exists = "Believe God exists" c.unemployed#c.Iafterlife = "Believe in life after death"
	c.unemployed#c.Ianyrelig = "Religious affiliation" c.unemployed#c.Irelp_one = "Religious person" 
	c.unemployed#c.Irellife_one = "Carry relig beliefs into rest of life")
	drop(*sex *raceeth *marital *childs *age *educ)  keep(*unemployed#*) xline(0) byopts(cols(3)) xsize(6) ysize(3) offset(0)
	name(fig2)
/* 	note("Each coefficient comes from a separate model." "Each row corresponds to the interaction term of that religiousness variable with unemployment. Positive and significant coefficients are evidence of a buffering effect.") */
	;
#delimit cr

graph export Fig2.pdf, replace



// TABLE 3
esttab probATT probATT_ctrl ALL using regtable3.rtf, replace coeflabel(unemployed "Unemployed" c.unemployed#c.IprobATT "Unemployed x Probability of Attendance" c.unemployed#c.Ipray_daily  "Unemployed x Pray daily or more" ///
	c.unemployed#c.Igod_exists  "Unemployed x Believe God exists" c.unemployed#c.Iafterlife  "Unemployed x Believe in life after death" ///
	c.unemployed#c.Ianyrelig  "Unemployed x Religious affiliation" c.unemployed#c.Irelp_one  "Unemployed x Religious person" ///
	c.unemployed#c.Irellife_one  "Unemployed x Carry relig beliefs into rest of life") drop(IprobATT Ipray_daily Igod_exists Iafterlife Irellife_one Irelp_one Ianyrelig) nobase ///
	se(a2) b(a2) nomtitle star(+ .1 * .05 ** .01) scalars("N N(observations)" "N_g N(individuals)") indicate("Controls = *sex* *raceeth* *marital* *childs* *age* *educ*" "Year fixed effects = *year*"  "Constant = _cons"  ) ///
	note("Note: Controls are interaction terms between unemployment and age, sex," "education, race/ethnicity, marital status, and number of children.")


// Full results for appendix table A5	
esttab probATT probATT_ctrl ALL using regtable_full.tex,  frag booktabs  noomit b(a2) se(a2) not nogaps replace nomtitle label ///
	coeflabel(unemployed "Unemployed" c.unemployed#c.IprobATT "Unemployed $\times$ Probability of Attendance" c.unemployed#c.Ipray_daily  "Unemployed $\times$ Pray daily or more" ///
	c.unemployed#c.Igod_exists  "Unemployed $\times$ Believe God exists" c.unemployed#c.Iafterlife  "Unemployed $\times$ Believe in life after death" ///
	c.unemployed#c.Ianyrelig  "Unemployed $\times$ Religious affiliation" c.unemployed#c.Irelp_one  "Unemployed $\times$ Religious person" ///
	c.unemployed#c.Irellife_one  "Unemployed $\times$ Carry relig beliefs into rest of life")  nobase /// 
	order(unemployed c.unemployed#c.IprobATT c.unemployed#c.Ipray_daily  ///
	c.unemployed#c.Igod_exists  c.unemployed#c.Iafterlife  ///
	c.unemployed#c.Ianyrelig  c.unemployed#c.Irelp_one   ///
	c.unemployed#c.Irellife_one *unemp*) ///
	star(+ .1 * .05) noobs scalars("N N(observations)" "N_g N(individuals)") ///  
	substitute("Ichilds" "Number of children" "Ieduc" "Education" "Iage" "Age" "c.unemployed" "Unemployed" "#" "$\times$"  ) 


// Concise models for all outcomes Tables A4-A13
foreach var in probATT attend_m pray_daily pray god_exists god afterlife rellife_one relp_one anyrelig {
	esttab `var' `var'_ctrl using regtable_`var'.tex, replace frag booktabs  noomit b(a2) se(a2) not nogaps nomtitle label nobase ///
		order(*unemp*) noobs scalars("N N(observations)" "N_g N(individuals)") /// 
		star(+ .1 * .05) indicate("Controls = *sex* *raceeth* *marital* *childs* *age* *educ*" "Year fixed effects = *year*"  "Constant = _cons"  )
}

// Appendix Table A14: Logistic Regresion
esttab probATT_logit pray_daily_logit god_exists_logit afterlife_logit anyrelig_logit relp_one_logit rellife_one_logit using regtable_logit.tex, ///
	frag booktabs  noomit b(a2) se(a2) not nogaps replace  label nobase  ///
	 star(+ .1 * .05 ** .01 ** .001) noobs scalars("N N(observations)" "N_g N(individuals)") nomtitle ///
	indicate("Controls = *sex* *raceeth* *marital* *childs* *age* *educ*" "Year fixed effects = *year*" ) 

	
// Secondary Models
foreach var in probATT rellife_one relp_one attend_m pray_daily god_exists afterlife anyrelig {
	eststo `var'_rev: xtreg `var' unemployed  i.year, fe
	eststo `var'_logitrev: xtlogit `var' unemployed  i.year, fe
}

// Appendix Table A15: Secondary Models
esttab probATT_rev pray_daily_rev god_exists_rev afterlife_rev anyrelig_rev relp_one_rev rellife_one_rev using regtable_secondary.tex, ///
	frag booktabs  noomit b(a2) se(a2) not nogaps replace  label nobase  ///
	 star(+ .1 * .05) noobs scalars("N N(observations)" "N_g N(individuals)") ///
	 mtitle("Attendance" "Pray daily or more" "Believe God exists" "Believe life after death" ///
	 "Religious affilitation" "Religious person" "Relig beliefs into life")


// Appendix Table A16: Logit of Secondary Models
esttab attend_m_logitrev pray_daily_logitrev god_exists_logitrev afterlife_logitrev anyrelig_logitrev using regtable_secondary_logit.tex, ///
	frag booktabs  noomit b(a2) se(a2) not nogaps replace label nobase  ///
	 star(+ .1 * .05) noobs scalars("N N(observations)" "N_g N(individuals)") ///
	 mtitle("Attendance (monthly+)" "Pray daily or more" "Believe God exists" "Believe life after death" ///
	 "Religious affilitation")

	 
// FIGURE 3
#delimit ;
coefplot  
	probATT_rev pray_daily_rev god_exists_rev afterlife_rev anyrelig_rev relp_one_rev  rellife_one_rev 
	,
	keep(unemployed) coeflabel(unemployed = "unemployed") xline(0) levels(95 90) // mcolor(dkgreen)ciopts(lcolor(dkgreen dkgreen))
	legend(order(3  "Probability of attendance" 6 "Pray daily or more" 
	9 "Believe God exists" 12 "Believe in life after death"
	15 "Religious person" 18 "Religious affiliation" 21 "Carry relig beliefs into rest of life")) 	 scheme(s1mono) name(fig3)
	;
#delimit cr
graph export Fig3.pdf, replace



*****************************
* DESCRIPTIVES
*****************************

tab Iraceeth, gen(R)
tab Imarital, gen(M)
gen Female = (Isex == 1)

lab var R1 "White"
lab var R2 "Black"
lab var R3 "Other"
lab var R4 "Hispanic"

lab var M1 "Married"
lab var M2 "Widowed"
lab var M3 "Divorced"
lab var M4 "Separated"
lab var M5 "Never married"

lab var Ieduc "Years of education"
lab var Iage "Age in years"
la var Ichilds "Number of children"


estpost sum nonz_happy unemployed IprobATT Iattend_m Ipray_daily Igod_exists Iafterlife Irellife_one  Irelp_one Ianyrelig Female Iage Ichilds Ieduc R1-R4 M1-M5  [w = wtpannr12]
eststo D
esttab D using descriptives.rtf, replace cells("mean(fmt(a2)) sd(fmt(a2)) min max(fmt(a2))") wide label nonumber noobs ///
	collabels("Mean" "Std Dev" "Min" "Max") ///
	refcat(IprobATT "Religion variables" Female "Controls") ///
	coeflabel(nonz_happy "Happiness (before standardization)" unemployed "Unemployed" IprobATT  "Probability of attendance"	Iattend_monthly "Attend monthly or more" Ipray_daily "Pray daily or more" Igod_exists "Believe God exists" Iafterlife "Believe in life after death" Irellife_one "Carry relig beliefs into rest of life"	Irelp_one "Religious person" Ianyrelig "Religious affiliation")

