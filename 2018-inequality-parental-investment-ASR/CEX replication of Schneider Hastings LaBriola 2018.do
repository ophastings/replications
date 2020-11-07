/************************************** 
Prepared by Orestes P. Hastings (pat.hastings@colostate.edu)
Created: February 2018
Last updated April 20, 2018

This do file is to be used with CEX_1980_2014_cleaned.dta to replicate the the Consumer Expenditure Survey (CEX) analysis in:

Schneider, Daniel, Orestes P. Hastings, and Joseph LaBriola. “Income Inequality and Class Divides in Parental Investment.” American Sociological Review

Expenditure data come from the Consumer Expenditure Survey 1980-2014. Sources of state-level data are described in paper. 
The sample is restricted to households with children with oldest parent between 25-65 years of age.
All dollar values have been adjusted to 2014 dollars with CPI-U-RS

This do file requires the following user-written Stata modules:
-estout-
-binscatter-
-egenmore-

Running the do file will create all of the tables and figures from the paper that are based on the CEX analysis. These will both appear in the results window
and will be saved to the output directory and .tex files (for tables) and .pdf files (for figures)

****************************************/

cd "/XXX/YYY/ZZZ" // set your working directory
global out "/XXX/YYY/ZZZ/output"  // set your directory for output


use CEX_1980_2014_cleaned.dta, clear // this is the only dataset needed

************  DATA PREPARATION ************ 

// generate age-squared term
gen max_age2 = max_age * max_age

// create race and spouse present measures
recode f_race (1=1 "Female white") (2/6 = 2 "Female non-white")(. = 3 "Female not present"), gen(f_nonwhite)
recode m_race (1=1 "Male white") (2/6 = 2 "Male non-white") (. = 3 "Male not present"), gen(m_nonwhite)

// create education measures
egen max_educ = rowmax(f_educ m_educ)
recode max_educ (0/11 = 1 "No HS") (12/14 = 2 "HS no BA")(15/16 = 3 "BA+"), gen(educ) // most educated

egen min_educ = rowmin(f_educ m_educ)
recode min_educ (0/11 = 1 "No HS") (12/14 = 2 "HS no BA")(15/16 = 3 "BA+"), gen(educ_minimum) // least educated

recode f_educ (0/14 = 0 "No BA") (15/16 = 1 "BA+"), gen(f_ba)
recode m_educ (0/14 = 0 "No BA") (15/16 = 1 "BA+"), gen(m_ba)

egen numba = rowtotal(f_ba m_ba) // total number of BAs
lab def numba 0 "No BA" 1 "1 BA" 2 "2 BAs"
lab val numba numba


// create work hours measure (replace missing/not working with 0)
gen m_hrs0 = m_hrs
replace m_hrs0 = 0 if missing(m_hrs) 
lab var m_hrs0 "Male work hours"
gen f_hrs0 = f_hrs
replace f_hrs0 = 0 if missing(f_hrs) 
lab var f_hrs0 "Female work hours"


// create age appropriate measures 
gen oldestchild = 0 if !missing(agecat0_5)
replace oldestchild = 1 if agecat0_5 >= 1 & agecat6_13 == 0 & agecat14_17 == 0 & !missing(agecat0_5) 
replace oldestchild = 2 if agecat6_13 >= 1  & agecat14_17 == 0 & !missing(agecat0_5) 
replace oldestchild = 3 if agecat14_17 >= 1 & !missing(agecat0_5) 

gen anychild1 = ((agecat0_5 >= 1) & !missing(agecat0_5))
gen anychild2 = ((agecat6_13 >= 1) & !missing(agecat6_13))
gen anychild3 = ((agecat14_17 >= 1) & !missing(agecat14_17))

gen onlychild1 = ((agecat0_5 >= 1) & !missing(agecat0_5) & (agecat6_13 == 0) & (agecat14_17 == 0))
gen onlychild2 = ((agecat0_5 == 0) & !missing(agecat0_5) & (agecat6_13 >= 1) & (agecat14_17 == 0))
gen onlychild3 = ((agecat0_5 == 0) & !missing(agecat0_5) & (agecat6_13 == 0) & (agecat14_17 >= 1))

gen youngestchild = 0 if !missing(agecat0_5)
replace youngestchild = 1 if agecat0_5 >= 1 & !missing(agecat0_5) 
replace youngestchild = 2 if agecat0_5 == 0 & agecat6_13 >= 1 & !missing(agecat0_5) 
replace youngestchild = 3 if agecat0_5 == 0 & agecat6_13 >= 0 & agecat14_17 >= 1 & !missing(agecat0_5) 



// create income groups based on state-level percentiles cutoffs from Census/ACS
gen inc4 = .
replace inc4 = 1 if income <= p25_b 
replace inc4 = 2 if income > p25_b & income <= p75_b
replace inc4 = 3 if income > p75_b & income <= p90_b
replace inc4 = 4 if income > p90_b
replace inc4 = . if missing(p25_b, p75_b, p90_b)

// scale income for regression model
gen inc1000 = income/1000
lab var inc1000 "Income dollars (in thousands)"



// create main spending measures
egen goods_all = rowtotal(goods_boysclothing goods_girlsclothing goods_kidstuff)
egen total_edu = rowtotal(school lessons childcare)
lab var total_edu ""

egen spending_all = rowtotal(goods_all total_edu)

// drop household if no spending for children in any category of any quarter
bysort cu_id: egen allspending = total(spending_all) 
drop if allspending == 0

// divide spending in each category by number of children in each category
foreach var in school lessons childcare total_edu goods_all  {
	replace `var' = `var'/kids
}


// create measure of quarterly spending as a portion of quarterly income
gen adj_total_edu = (total_edu/income)*4 
replace adj_total_edu = . if adj_total_edu < 0 
sum adj_total_edu if total_edu > 0, detail  
replace adj_total_edu = . if (adj_total_edu > r(p99)) // set to missing for top 1%

sum total_edu if total_edu > 0, detail 
replace total_edu = . if (total_edu > r(p99)) // drop top 1% if any expenses in that category


// rename lagged inequality variables for simplicity
rename l1st_gini_f gini
rename l1st_top10_f top10


***** PRELIMINARY FIGURES *********


// Create Figure 2
preserve
collapse total_edu [w = finlwt21], by(qintrvyr inc4)
sort qintrvyr
drop if qintrvyr == 2015 // For this figure, drop the data from the first quarter of 2015 which are in the dataset

tw 	(line total_edu qintrvyr if inc4 == 4, lcolor(dkorange*.7) lw(.4)) ///
	(line total_edu qintrvyr if inc4 == 3, lcolor(forest_green*.7) lw(.4)) ///
	(line total_edu qintrvyr if inc4 == 2, lcolor(maroon*.9) lw(.4)) /// 
	(line total_edu qintrvyr if inc4 == 1, lcol(navy*2) lw(.4)) , ///
	legend(order(1 "Income 91-100p"  2 "Income 76-90p" 3 "Income 26-75p"  4 "Income 0-25p" ) ) scheme(s1color) xlabel(1980(2)2014, angle(45)) xtitle(Year) ytitle("Parental expenditures ($/quarter)") ///
	name(fig2, replace) 

// graph export $out/Fig2.tif, replace width(2200)
graph export $out/overtime_inc_total_edu.pdf, replace
restore


// Create Figure 5: binned scatter plot of investment by household income rank and state-level gini
preserve
revrs inc4 // reverse income coding for this figure
binscatter total_edu gini, by(revinc4) colors(dkorange*.7 forest_green*.7 maroon*.9  navy*2 ) mcolors(dkorange*.7 forest_green*.7 maroon*.9  navy*2  ) ///
	msymbol(O D T S) ///
	legend(order(1 "Income 91-100p"  2 "Income 76-90p" 3 "Income 26-75p"  4 "Income 0-25p" )) ///
	xtitle("Gini (lagged)") ytitle("Parental expenditures ($/quarter)") name(fig5, replace)
	
// graph export $out/Fig5c.tif, replace width(2200)
graph export $out/Fig5.pdf, replace
restore


************  ADDITIONAL DATA PREPARATION FOR REGRESSION MODELS ************ 

// create dummies for each income category
tab inc4, gen(INC) 

// create dummies for each education category
tab educ, gen(EDUC) 
tab numba, gen(NUMBA)
tab educ_minimum, gen(EDUCMIN)

// drop observations that don't appear in the final model before demeaning
drop if missing(stfips, total_edu)

// demean variables that go into interaction term
foreach var in gini top10  {  // inc4irs inc4 inc4census
	bysort stfips: egen `var'_wmeanst = wtmean(`var'), weight(finlwt21) // calculate overall state mean
	gen `var'_wstdemean = `var' - `var'_wmeanst // create state demeaned variable
}

foreach var in INC { // INCirs 
	forvalues i = 1/4 {
		bysort stfips: egen `var'`i'_wmeanst = wtmean(`var'`i'), weight(finlwt21)
		gen `var'`i'_wstdemean = `var'`i' - `var'`i'_wmeanst
	}
}

foreach var in EDUC NUMBA EDUCMIN {
	forvalues i = 1/3 {
		bysort stfips: egen `var'`i'_wmeanst = wtmean(`var'`i'), weight(finlwt21)
		gen `var'`i'_wstdemean = `var'`i' - `var'`i'_wmeanst
	}
}
	
lab var gini_wstdemean "Gini index"
lab var top10_wstdemean "Top 10% income share"
lab var INC1_wstdemean "Income group 0-25p"
lab var INC3_wstdemean "Income group 76-90p"
lab var INC4_wstdemean "Income group 91-100p"
lab var EDUC1_wstdemean "No HS"
lab var EDUC3_wstdemean "BA+"
lab var EDUCMIN1_wstdemean "No HS"
lab var EDUCMIN3_wstdemean "BA+"
lab var NUMBA2_wstdemean "1 BA"
lab var NUMBA3_wstdemean "2 BAs"


// Define globals for the analysis
global cuctrl c.fam_size c.max_age##c.max_age i.f_nonwhite i.m_nonwhite f_hrs0 m_hrs0 // household controls
global cuctrl_indicate *fam_size max_age* *f_nonwhite *m_nonwhite f_hrs0 m_hrs0 // household control indicators for esttab tables
global stctrl l1st_medinc_comb l1ur_ st_black st_foreign // state-level controls
global ifetc [pw = finlwt21 ], vce(cluster stfips) // use survey weights and cluster standard errors by state
global starplus "star(+ .1 * .05 ** .01 *** .001)"


************* REGRESSION MODELS *********************

// Regression models by income class of spending for investment and consumption using both Gini and Top 10%.
foreach DV in total_edu goods_all { 
	eststo REG_t10xinc_`DV': reg `DV' c.top10_wstdemean##c.(INC1_wstdemean INC3_wstdemean INC4_wstdemean) i.educ $cuctrl $stctrl i.year i.stfips $ifetc 
	eststo REG_gxinc_`DV': reg `DV' c.gini_wstdemean##c.(INC1_wstdemean INC3_wstdemean INC4_wstdemean) i.educ $cuctrl $stctrl i.year i.stfips $ifetc 
}

// Regression models controlling for income
eststo REG_withinc_t10: reg total_edu c.top10_wstdemean##c.(INC1_wstdemean INC3_wstdemean INC4_wstdemean) inc1000 i.educ $cuctrl $stctrl i.year i.stfips $ifetc 
eststo REG_withinc_g: reg total_edu c.gini_wstdemean##c.(INC1_wstdemean INC3_wstdemean INC4_wstdemean) inc1000 i.educ $cuctrl $stctrl i.year i.stfips $ifetc 

/*
// KHB test to see if  controlling for income matters. Need -khb- package
khb reg total_edu c.gini_wstdemean c.INC1_wstdemean c.INC3_wstdemean c.INC4_wstdemean c.gini_wstdemean#c.INC1_wstdemean c.gini_wstdemean#c.INC3_wstdemean c.gini_wstdemean#c.INC4_wstdemean    || inc1000 [pw = finlwt21], cluster(stfips) c(i.educ $cuctrl $stctrl i.year i.stfips)
khb reg total_edu c.gini_wstdemean c.EDUC1_wstdemean c.EDUC3_wstdemean c.gini_wstdemean#c.EDUC1_wstdemean c.gini_wstdemean#c.EDUC3_wstdemean   || inc1000 [pw = finlwt21], cluster(stfips) c(i.inc4 $cuctrl $stctrl i.year i.stfips)
*/


// Regression models of investment/income
eststo REG_ratio1_g: reg adj_total_edu c.gini_wstdemean##c.(INC1_wstdemean INC3_wstdemean INC4_wstdemean) inc1000 i.educ $cuctrl $stctrl i.year i.stfips $ifetc 
eststo REG_ratio1_t10: reg adj_total_edu c.top10_wstdemean##c.(INC1_wstdemean INC3_wstdemean INC4_wstdemean) inc1000 i.educ $cuctrl $stctrl i.year i.stfips $ifetc 


// Regression models by education class
foreach var in EDUC EDUCMIN {
	eststo REG_gx`var'_total: reg total_edu c.gini_wstdemean##c.(`var'1_wstdemean `var'3_wstdemean) i.inc4 $cuctrl $stctrl i.year i.stfips $ifetc 
	eststo REG_t10x`var'_total: reg total_edu c.top10_wstdemean##c.(`var'1_wstdemean `var'3_wstdemean) i.inc4 $cuctrl $stctrl i.year i.stfips $ifetc 

	eststo REG_withinc_g`var': reg total_edu  c.gini_wstdemean##c.(`var'1_wstdemean `var'3_wstdemean) i.inc4 inc1000  $cuctrl $stctrl i.year i.stfips $ifetc 
	eststo REG_withinc_t10`var': reg total_edu c.top10_wstdemean##c.(`var'1_wstdemean `var'3_wstdemean) i.inc4 inc1000  $cuctrl $stctrl i.year i.stfips $ifetc 

	eststo REG_gx`var'_adj: reg adj_total_edu c.gini_wstdemean##c.(`var'1_wstdemean `var'3_wstdemean) i.inc4 inc1000 $cuctrl $stctrl i.year i.stfips $ifetc 
	eststo REG_t10x`var'_adj: reg adj_total_edu c.top10_wstdemean##c.(`var'1_wstdemean `var'3_wstdemean) i.inc4 inc1000 $cuctrl $stctrl i.year i.stfips $ifetc 
}


	
foreach var in NUMBA {
	eststo REG_gx`var'_total: reg total_edu c.gini_wstdemean##c.(`var'2_wstdemean `var'3_wstdemean) i.inc4 $cuctrl $stctrl i.year i.stfips $ifetc 
	eststo REG_t10x`var'_total: reg total_edu c.top10_wstdemean##c.(`var'2_wstdemean `var'3_wstdemean) i.inc4 $cuctrl $stctrl i.year i.stfips $ifetc 

	eststo REG_withinc_g`var': reg total_edu  c.gini_wstdemean##c.(`var'2_wstdemean `var'3_wstdemean) i.inc4 inc1000  $cuctrl $stctrl i.year i.stfips $ifetc 
	eststo REG_withinc_t10`var': reg total_edu c.top10_wstdemean##c.(`var'2_wstdemean `var'3_wstdemean) i.inc4 inc1000  $cuctrl $stctrl i.year i.stfips $ifetc 

	eststo REG_gx`var'_adj: reg adj_total_edu c.gini_wstdemean##c.(`var'2_wstdemean `var'3_wstdemean) i.inc4 inc1000 $cuctrl $stctrl i.year i.stfips $ifetc 
	eststo REG_t10x`var'_adj: reg adj_total_edu c.top10_wstdemean##c.(`var'2_wstdemean `var'3_wstdemean) i.inc4 inc1000 $cuctrl $stctrl i.year i.stfips $ifetc 
}


// Models by category of spending
est restore REG_gxinc_total_edu
gen mainsample = e(sample)
foreach DV in lessons school childcare {
	eststo REG_gxinc_`DV': reg `DV' c.gini_wstdemean##c.(INC1_wstdemean INC3_wstdemean INC4_wstdemean) i.educ $cuctrl $stctrl i.year i.stfips $ifetc, if mainsample
} 

// Models by child age (defined as youngest child in the household)
forvalues i = 1/3 {
	eststo REG_main_youngage`i': reg total_edu c.gini_wstdemean##c.(INC1_wstdemean INC3_wstdemean INC4_wstdemean) i.educ $cuctrl $stctrl i.year i.stfips $ifetc, if youngestchild == `i'
	foreach DV in lessons school childcare {
		eststo REG_`DV'_youngage`i': reg `DV' c.gini_wstdemean##c.(INC1_wstdemean INC3_wstdemean INC4_wstdemean) i.educ $cuctrl $stctrl i.year i.stfips $ifetc, if youngestchild == `i'
	}
}


/* Additional models with other categorizations of chid age */

// Models by child age (defined as oldest child in the household)
forvalues i = 1/3 {
	eststo REG_main_oldage`i': reg total_edu c.gini_wstdemean##c.(INC1_wstdemean INC3_wstdemean INC4_wstdemean) i.educ $cuctrl $stctrl i.year i.stfips $ifetc, if oldestchild == `i'
	foreach DV in lessons school childcare {
		eststo REG_`DV'_oldage`i': reg `DV' c.gini_wstdemean##c.(INC1_wstdemean INC3_wstdemean INC4_wstdemean) i.educ $cuctrl $stctrl i.year i.stfips $ifetc, if oldestchild == `i'
	}
}

// Models by child age (any child that age in household)
forvalues i = 1/3 {
	eststo REG_main_anyage`i': reg total_edu c.gini_wstdemean##c.(INC1_wstdemean INC3_wstdemean INC4_wstdemean) i.educ $cuctrl $stctrl i.year i.stfips $ifetc, if anychild`i' == 1
	foreach DV in lessons school childcare {
		eststo REG_`DV'_anyage`i': reg `DV' c.gini_wstdemean##c.(INC1_wstdemean INC3_wstdemean INC4_wstdemean) i.educ $cuctrl $stctrl i.year i.stfips $ifetc, if anychild`i' == 1
	}
}

// Models by child age (only children that age in household)
forvalues i = 1/3 {
	eststo REG_main_onlyage`i': reg total_edu c.gini_wstdemean##c.(INC1_wstdemean INC3_wstdemean INC4_wstdemean) i.educ $cuctrl $stctrl i.year i.stfips $ifetc, if onlychild`i' == 1
	foreach DV in lessons school childcare {
		eststo REG_`DV'_onlyage`i': reg `DV' c.gini_wstdemean##c.(INC1_wstdemean INC3_wstdemean INC4_wstdemean) i.educ $cuctrl $stctrl i.year i.stfips $ifetc, if onlychild`i' == 1
	}
}
*/


******************** THE TABLES *******************


// Table 2: Relationship between State-Level Inequality and Gaps by Household Income Percentile Rank in Financial Investments in Children
esttab REG_gxinc_total_edu   REG_withinc_g   REG_gxinc_goods_all REG_ratio1_g, ///
	refcat(c.gini_wstdemean#c.INC3_wstdemean "Gini index x Income group 26-75p" INC3_wstdemean "Income group 26-75p" ) ///
	order(*gini*#* gini* *INC* inc1000) star(+ .1 * .05 ** .01 *** .001) label  substitute("#" "x" " 0 " "ref." "(.)" "") nocons b(a2) se(a2) ///
	indicate("Individual controls = $cuctrl_indicate *educ *age*" "State Controls = $stctrl" "State FE = *stfips" "Year FE = *year") ///
	mtitle("Main Model of Investment" "with Income Control" "Model of Consumption" "Model of Investment/Income" ) note("State-clustered standard errors in parentheses")

esttab REG_gxinc_total_edu   REG_withinc_g   REG_gxinc_goods_all REG_ratio1_g using $out/Table2.rtf, nogaps ///
	refcat(c.gini_wstdemean#c.INC3_wstdemean "Gini index x Income group 26-75p" INC3_wstdemean "Income group 26-75p" ) ///
	order(*gini*#* gini* *INC* inc1000) star(+ .1 * .05 ** .01 *** .001) label  substitute("#" "x" " 0 " "ref." "(.)" "") nocons b(a2) se(a2) ///
	indicate("Individual controls = $cuctrl_indicate *educ *age*" "State Controls = $stctrl" "State FE = *stfips" "Year FE = *year") ///
	mtitle("Main Model of Investment" "with Income Control" "Model of Consumption" "Model of Investment/Income" ) note("State-clustered standard errors in parentheses")


esttab REG_gxinc_total_edu   REG_withinc_g   REG_gxinc_goods_all REG_ratio1_g using $out/TABLE2.tex, replace ///
	refcat(c.gini_wstdemean#c.INC3_wstdemean "Gini index $\times$ Income group 26-75p" INC3_wstdemean "Income group 26-75p" ) ///
	order(*gini*#* gini*  *INC* inc1000) star(+ .1 * .05 ** .01 *** .001) label nogaps booktabs substitute(% \% " 0 " "ref." "(.)" "") nocons b(a2) se(a2) ///
	indicate("Individual controls = $cuctrl_indicate *educ *age*" "State Controls = $stctrl" "State FE = *stfips" "Year FE = *year") ///
	mtitle("Main Model of Investment" "with Income Control" "Model of Consumption" "Model of Investment/Income" ) note("State-clustered standard errors in parentheses")

	
	
// Table A1: Relationship between State-Level Top 10% Income Share and Gaps by Household Income Percentile Rank in Financial Investments in Children
esttab REG_t10xinc_total_edu   REG_withinc_t10   REG_t10xinc_goods_all REG_ratio1_t10 , ///
	refcat(c.top10_wstdemean#c.INC3_wstdemean "Top 10% income share x Income group 26-75p" INC3_wstdemean "Income group 26-75p" ) ///
	order(*top10*#* top10* *INC* inc1000)  star(+ .1 * .05 ** .01 *** .001) label  substitute("#" "x" " 0 " "ref." "(.)" "") nocons b(a2) se(a2) ///
	indicate("Individual controls = $cuctrl_indicate *educ *age*" "State Controls = $stctrl" "State FE = *stfips" "Year FE = *year") ///
	mtitle("Main Model of Investment" "with Income Control" "Model of Investment/Income" "Model of Consumption") note("State-clustered standard errors in parentheses")


esttab REG_t10xinc_total_edu   REG_withinc_t10   REG_t10xinc_goods_all REG_ratio1_t10 using $out/TABLE_A1.tex, replace ///
	refcat(c.top10_wstdemean#c.INC3_wstdemean "Top 10% income share $\times$ Income group 26-75p" INC3_wstdemean "Income group 26-75p" ) ///
	order(*top10*#* top10* *INC* inc1000)  star(+ .1 * .05 ** .01 *** .001) label nogaps booktabs substitute(% \% " 0 " "ref." "(.)" "") nocons b(a2) se(a2) ///
	indicate("Individual controls = $cuctrl_indicate *educ *age*" "State Controls = $stctrl" "State FE = *stfips" "Year FE = *year") ///
	mtitle("Main Model of Investment" "with Income Control" "Model of Investment/Income" "Model of Consumption") note("State-clustered standard errors in parentheses")


// Table 3: Relationship between State-Level Income Inequality and Gaps by Parental Education in Financial Investments in Children
esttab REG_gxEDUC_total   REG_withinc_gEDUC REG_gxEDUC_adj , ///
	refcat(c.gini_wstdemean#c.EDUC3_wstdemean "Gini index x HS no BA" EDUC3_wstdemean "HS no BA" ) ///
	order(*gini*#* gini* EDUC* inc1000)  star(+ .1 * .05 ** .01 *** .001) label substitute(# x " 0 " "ref." "(.)" "") nocons b(a2) se(a2) ///
	indicate("Individual controls = $cuctrl_indicate *inc4 *age*" "State Controls = $stctrl" "State FE = *stfips" "Year FE = *year") ///
	mtitle("Main Model of Investment" "with Income Control" "Model of Investment/Income") note("State-clustered standard errors in parentheses")


esttab REG_gxEDUC_total   REG_withinc_gEDUC REG_gxEDUC_adj using $out/TABLE3.rtf , nogaps ///
	refcat(c.gini_wstdemean#c.EDUC3_wstdemean "Gini index x HS no BA" EDUC3_wstdemean "HS no BA" ) ///
	order(*gini*#* gini* EDUC* inc1000)  star(+ .1 * .05 ** .01 *** .001) label substitute(# x " 0 " "ref." "(.)" "") nocons b(a2) se(a2) ///
	indicate("Individual controls = $cuctrl_indicate *inc4 *age*" "State Controls = $stctrl" "State FE = *stfips" "Year FE = *year") ///
	mtitle("Main Model of Investment" "with Income Control" "Model of Investment/Income") note("State-clustered standard errors in parentheses")

	
esttab REG_gxEDUC_total   REG_withinc_gEDUC REG_gxEDUC_adj using $out/TABLE3.tex, replace ///
	refcat(c.gini_wstdemean#c.EDUC3_wstdemean "Gini index $\times$ HS no BA" EDUC3_wstdemean "HS no BA" ) ///
	order(*gini*#* gini* EDUC* inc1000)  star(+ .1 * .05 ** .01 *** .001) label nogaps booktabs substitute(% \% " 0 " "ref." "(.)" "") nocons b(a2) se(a2) ///
	indicate("Individual controls = $cuctrl_indicate *inc4 *age*" "State Controls = $stctrl" "State FE = *stfips" "Year FE = *year") ///
	mtitle("Main Model of Investment" "with Income Control" "Model of Investment/Income") note("State-clustered standard errors in parentheses")

	
// Table A2: Relationship between State-Level Top 10% Income Share and Gaps by Parental Education in Financial Investments in Children
esttab REG_t10xEDUC_total   REG_withinc_t10EDUC REG_t10xEDUC_adj  , ///
	refcat(c.top10_wstdemean#c.EDUC3_wstdemean "Top 10% share x HS no BA" EDUC3_wstdemean "HS no BA" ) ///
	order(*top10*#* top10* EDUC* inc1000)  star(+ .1 * .05 ** .01 *** .001) label  substitute(# x " 0 " "ref." "(.)" "") nocons b(a2) se(a2) ///
	indicate("Individual controls = $cuctrl_indicate *inc4 *age*" "State Controls = $stctrl" "State FE = *stfips" "Year FE = *year") ///
	mtitle("Main Model of Investment" "with Income Control" "Model of Investment/Income") note("State-clustered standard errors in parentheses")


esttab REG_t10xEDUC_total   REG_withinc_t10EDUC REG_t10xEDUC_adj  using $out/TABLE_A2.tex, replace ///
	refcat(c.top10_wstdemean#c.EDUC3_wstdemean "Top 10% share $\times$ HS no BA" EDUC3_wstdemean "HS no BA" ) ///
	order(*top10*#* top10* EDUC* inc1000)  star(+ .1 * .05 ** .01 *** .001) label nogaps booktabs substitute(% \% " 0 " "ref." "(.)" "") nocons b(a2) se(a2) ///
	indicate("Individual controls = $cuctrl_indicate *inc4 *age*" "State Controls = $stctrl" "State FE = *stfips" "Year FE = *year") ///
	mtitle("Main Model of Investment" "with Income Control" "Model of Investment/Income") note("State-clustered standard errors in parentheses")


// Table A4: Full version of Main Model (Model 1 of Table 2)
esttab REG_gxinc_total_edu , ///
	refcat(c.gini_wstdemean#c.INC3_wstdemean "Gini index x Income group 26-75p" INC3_wstdemean "Income group 26-75p" ) ///
	order(*gini*#* gini* *INC*)  star(+ .1 * .05 ** .01 *** .001) label  nonumbers  mtitle("Main Model of Investment") nocons ///
	indicate("State FE = *stfips" "Year FE = *year") b(a3) se(a3) ///
	substitute(# x " 0 " "ref." "(.)" "") note("State-clustered standard errors in parentheses")


esttab REG_gxinc_total_edu using $out/TABLE_A4.tex, replace ///
	refcat(c.gini_wstdemean#c.INC3_wstdemean "Gini index $\times$ Income group 26-75p" INC3_wstdemean "Income group 26-75p" ) ///
	order(*gini*#* gini* *INC*)  star(+ .1 * .05 ** .01 *** .001) label nogaps nonumbers booktabs mtitle("Main Model of Investment") nocons ///
	indicate("State FE = *stfips" "Year FE = *year") b(a3) se(a3) ///
	substitute(% \% " 0 " "ref." "(.)" "") note("State-clustered standard errors in parentheses")


// Table A5: Gaps by parental education using lowest educated parent	
esttab REG_gxEDUCMIN_total   REG_withinc_gEDUCMIN REG_gxEDUCMIN_adj , ///
	refcat(c.gini_wstdemean#c.EDUCMIN3_wstdemean "Gini index x HS no BA" EDUCMIN3_wstdemean "HS no BA" ) ///
	order(*gini*#* gini* EDUC* inc1000)   star(+ .1 * .05 ** .01 *** .001) label   substitute(# x " 0 " "ref." "(.)" "") nocons b(a2) se(a2) ///
	indicate("Individual controls = $cuctrl_indicate *inc4 *age*" "State Controls = $stctrl" "State FE = *stfips" "Year FE = *year") ///
	mtitle("Main Model of Investment" "with Income Control" "Model of Investment/Income") note("State-clustered standard errors in parentheses")


esttab REG_gxEDUCMIN_total   REG_withinc_gEDUCMIN REG_gxEDUCMIN_adj using $out/TABLE_A5.tex, replace ///
	refcat(c.gini_wstdemean#c.EDUCMIN3_wstdemean "Gini index $\times$ HS no BA" EDUCMIN3_wstdemean "HS no BA" ) ///
	order(*gini*#* gini* EDUC* inc1000)   star(+ .1 * .05 ** .01 *** .001) label nogaps booktabs substitute(% \% " 0 " "ref." "(.)" "") nocons b(a2) se(a2) ///
	indicate("Individual controls = $cuctrl_indicate *inc4 *age*" "State Controls = $stctrl" "State FE = *stfips" "Year FE = *year") ///
	mtitle("Main Model of Investment" "with Income Control" "Model of Investment/Income") note("State-clustered standard errors in parentheses")


// Table A6: Gaps by parental education using number of college-completed parents	
esttab REG_gxNUMBA_total  REG_withinc_gNUMBA  REG_gxNUMBA_adj  , ///
	refcat(c.gini_wstdemean#c.NUMBA2_wstdemean "Gini index x no BA" NUMBA2_wstdemean "no BA" ) ///
	order(*gini*#* gini* NUMBA* inc1000)   star(+ .1 * .05 ** .01 *** .001) label substitute(# x " 0 " "ref." "(.)" "") nocons b(a2) se(a2) ///
	indicate("Individual controls = $cuctrl_indicate *inc4 *age*" "State Controls = $stctrl" "State FE = *stfips" "Year FE = *year") ///
	mtitle("Main Model of Investment" "with Income Control" "Model of Investment/Income") note("State-clustered standard errors in parentheses")


esttab REG_gxNUMBA_total  REG_withinc_gNUMBA  REG_gxNUMBA_adj  using $out/TABLE_A6.tex, replace ///
	refcat(c.gini_wstdemean#c.NUMBA2_wstdemean "Gini index $\times$ no BA" NUMBA2_wstdemean "no BA" ) ///
	order(*gini*#* gini* NUMBA* inc1000)   star(+ .1 * .05 ** .01 *** .001) label nogaps booktabs substitute(% \% " 0 " "ref." "(.)" "") nocons b(a2) se(a2) ///
	indicate("Individual controls = $cuctrl_indicate *inc4 *age*" "State Controls = $stctrl" "State FE = *stfips" "Year FE = *year") ///
	mtitle("Main Model of Investment" "with Income Control" "Model of Investment/Income") note("State-clustered standard errors in parentheses")


// Table A7: Main model by category of investment
esttab REG_gxinc_lessons REG_gxinc_school REG_gxinc_childcare , ///
	refcat(c.gini_wstdemean#c.INC3_wstdemean "Gini index x Income group 26-75p" INC3_wstdemean "Income group 26-75p" ) ///
	order(*gini*#* gini* *INC*) star(+ .1 * .05 ** .01 *** .001) label substitute(# x " 0 " "ref." "(.)" "") nocons b(a2) se(a2) ///
	indicate("Individual controls = $cuctrl_indicate *educ *age*" "State Controls = $stctrl" "State FE = *stfips" "Year FE = *year") ///
	mtitle("Main Model (Lessons)" "Main Model (School)" "Main Model (Childcare)") note("State-clustered standard errors in parentheses")


esttab REG_gxinc_lessons REG_gxinc_school REG_gxinc_childcare using $out/TABLE_A7.tex, replace ///
	refcat(c.gini_wstdemean#c.INC3_wstdemean "Gini index $\times$ Income group 26-75p" INC3_wstdemean "Income group 26-75p" ) ///
	order(*gini*#* gini* *INC*) star(+ .1 * .05 ** .01 *** .001) label nogaps booktabs substitute(% \% " 0 " "ref." "(.)" "") nocons b(a2) se(a2) ///
	indicate("Individual controls = $cuctrl_indicate *educ *age*" "State Controls = $stctrl" "State FE = *stfips" "Year FE = *year") ///
	mtitle("Main Model (Lessons)" "Main Model (School)" "Main Model (Childcare)") note("State-clustered standard errors in parentheses")


// Tables of class gaps in total investment by age (Table A8) and in category of investment by age (A9, A10, A11)
foreach DV in main lessons school childcare {

	dis " "
	dis "`DV'"
	esttab REG_`DV'_youngage1 REG_`DV'_youngage2 REG_`DV'_youngage3,  ///
	refcat(c.gini_wstdemean#c.INC3_wstdemean "Gini index x Income group 26-75p" INC3_wstdemean "Income group 26-75p" ) ///
	order(*gini*#* gini* *INC*) star(+ .1 * .05 ** .01 *** .001) label substitute("#" "x" " 0 " "ref." "(.)" "") nocons b(a2) se(a2) ///
	indicate("Individual controls = $cuctrl_indicate *educ *age*" "State Controls = $stctrl" "State FE = *stfips" "Year FE = *year") ///
	mtitle("Age 0-5" "Age 6-13" "Age 14-17") note("State-clustered standard errors in parentheses")
	
	
	esttab REG_`DV'_youngage1 REG_`DV'_youngage2 REG_`DV'_youngage3 using $out/TABLE_`DV'_YOUNGAGE.tex, replace ///
	refcat(c.gini_wstdemean#c.INC3_wstdemean "Gini index $\times$ Income group 26-75p" INC3_wstdemean "Income group 26-75p" ) ///
	order(*gini*#* gini* *INC*) star(+ .1 * .05 ** .01 *** .001) label nogaps booktabs substitute(% \% " 0 " "ref." "(.)" "") nocons b(a2) se(a2) ///
	indicate("Individual controls = $cuctrl_indicate *educ *age*" "State Controls = $stctrl" "State FE = *stfips" "Year FE = *year") ///
	mtitle("Age 0-5" "Age 6-13" "Age 14-17") note("State-clustered standard errors in parentheses")
		
}
	
	
*** CREATE FIGURE 6: PREDICTED INVESTMENTS BY INCOME RANK ***


est restore REG_gxinc_total_edu // restore the main model

sum gini_wstdemean [w = finlwt21] // mean = 0; 2SD = .0937

sum INC4_wstdemean [w = finlwt21] if INC4_wstdemean < 0 //   -.084
sum INC4_wstdemean [w = finlwt21] if INC4_wstdemean > 0 //  .913

sum INC3_wstdemean [w = finlwt21] if INC3_wstdemean < 0 //  -.129
sum INC3_wstdemean [w = finlwt21] if INC3_wstdemean > 0 //  .868

sum INC2_wstdemean [w = finlwt21] if INC2_wstdemean < 0 
sum INC2_wstdemean [w = finlwt21] if INC2_wstdemean > 0 

sum INC1_wstdemean [w = finlwt21] if INC1_wstdemean < 0 //   -.332
sum INC1_wstdemean [w = finlwt21] if INC1_wstdemean > 0 //  .676

		
margins, at(gini_wstdemean = (-.0937 0 .0937 ) INC4_wstdemean = .913 INC3_wstdemean = -.129 INC1_wstdemean= -.332  ) ///
	at(gini_wstdemean = (-.0937 0 .0937 ) INC4_wstdemean = -.084 INC3_wstdemean = .868 INC1_wstdemean= -.332  ) ///
	at(gini_wstdemean = (-.0937 0 .0937 ) INC4_wstdemean = -.084 INC3_wstdemean = -.129 INC1_wstdemean= -.332  ) ///
	 at(gini_wstdemean = (-.0937 0 .0937 ) INC4_wstdemean = -.084 INC3_wstdemean = -.129 INC1_wstdemean= .660  )
	
	
marginsplot, name(fig6, replace) scheme(s1color) title("") ytitle("Linear prediction of expenditures ($/quarter)") ///
	plot1opts(lcolor(dkorange*.7) mcolor(dkorange*.7) msymbol(O)) ci1opts(lcol(dkorange*.7)) ///
	plot2opts(lcolor(forest_green*.7) mcolor(forest_green*.7) msymbol(D)) ci2opts(lcol(forest_green*.7)) ///
	plot3opts(lcolor(maroon*.9) mcolor(maroon*.9) msymbol(T)) ci3opts(lcol(maroon*.9)) ///
	plot4opts(lcolor(navy*2 ) mcolor(navy*2 ) msymbol(S)) ci4opts(lcol(navy*2 )) ///
		legend(order(5 "Income group 91-100p" 6 "Income group 76-90p" 7 "Income group 26-75p" 8 "Income group 0-25p")) ///
		xlabel(-.0937 "-2 SD" 0 "Mean" .0937 "2 SD")
		
// graph export $out/Fig6.tif, replace width(2200)	
graph export $out/FIG6_PREDICTED.pdf, replace

