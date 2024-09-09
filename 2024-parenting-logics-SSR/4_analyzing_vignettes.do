**** Replication of What's a Parent to Do? Measuring Cultural Logics of Parenting with Computational Text Analysis
**** Last updated: September 9, 2024

**** Stata packages required 
* cleanplots
* fmlogit
* outreg2
* combomarginsplot
* grc1leg
* esttab

*** Set your working directory here
//cd XXX

*** inside this directory, you need a folder called output and inside output, 
*** you should have the following 5 folders:
*** Vignette1, Vignette4, Vignette5, Regressions, Vignettes_combined, Extra
 
global direct ./output
  
set scheme cleanplots
global starplus  "star(+ .1 * .05 ** .01 *** .001)"


********************************************************************************
********************* Analyzing Vignette 1 (Bored at school) *******************
********************************************************************************

use "allmerged.dta" if vig == 1 & !missing(topic), clear


// shows the highest probability of being in a particular topic
hist maxprob, percent

graph bar, over(topic) 
graph save "$direct/Vignette1/BTM_vig1_distribution.gph", replace
graph export "$direct/Vignette1/BTM_vig1_distribution.pdf", replace

graph bar if maxprob >= .6, over(topic) name(certain60, replace)
graph save "$direct/Vignette1/BTM_vig1_distribution_certain60.gph", replace
graph export "$direct/Vignette1/BTM_vig1_distribution_certain60.pdf", replace

* Recode ethnicity/education/income

recode ppethm 5=3 // put all "other" together
recode ppeducat 1=2 // collapse "less than high school" with "high school"
xtile inc_quart= ppincimp,n(4) // create four income quartiles

// Fraction Mlogit

gen topic4_r=1-(topic1+topic2+topic3)
rename topic4 topic4_dec
rename topic4_r topic4
replace topic4=0 if topic4<0

gen sum_exact = topic1 + topic2 + topic3 + topic4 // this should add up to 1 exactly
replace sum_exact=1 if sum_exact>1

egen sum_rowtotal=rowtotal(topic1 topic2 topic3 topic4)
replace sum_rowtotal=1 if sum_rowtotal!=1 

*** Education, race, and income combined

fmlogit topic1 topic2 topic3 topic4 [pw = weight], eta(i.inc_quart i.ppeducat i.ppethm i.ppgender ppage mother son)
outreg2 using "$direct/Regressions/fml.xls", replace ctitle(1) label bdec(3) dec(3)

*** Excluding race

fmlogit topic1 topic2 topic3 topic4 [pw = weight], eta(i.inc_quart i.ppeducat i.ppgender ppage mother son)
outreg2 using "$direct/Regressions/fml_excl_race.xls", replace ctitle(1) label bdec(3) dec(3)

*** Excluding income

fmlogit topic1 topic2 topic3 topic4 [pw = weight], eta (i.ppeducat i.ppethm i.ppgender ppage mother son)
outreg2 using "$direct/Regressions/fml_excl_inc.xls", replace ctitle(1) label bdec(3) dec(3)

*** Excluding education 

fmlogit topic1 topic2 topic3 topic4 [pw = weight], eta(i.inc_quart i.ppethm i.ppgender ppage mother son)
outreg2 using "$direct/Regressions/fml_excl_edu.xls", replace ctitle(1) label bdec(3) dec(3)

/*
margins inc edu ppethm, atmeans post
est sto v1
coefplot v1, recast(bar) barwidth(0.8) fcolor(*.8) citop ciopts(recast(rcap)) scheme(s1color) keep(*inc *edu *ppethm *ppgender *ppage *mother *son) 
*/

/*
forvalues i = 1/4 {
margins ppethm, atmeans predict(outcome(topic`i'))
}
graph combine Topic1_r Topic2_r Topic3_r Topic4_r, ycommon

margins ppethm, atmeans predict(topic1)
marginsplot, name(Topic1_r) 
*/

*** Marginsplot

fmlogit topic1 topic2 topic3 topic4 [pw = weight], eta(i.inc_quart i.ppeducat i.ppethm i.ppgender ppage mother son)

margins ppethm, atmeans predict(outcome(topic1)) saving(ppethm_t1, replace)
margins ppethm, atmeans predict(outcome(topic2)) saving(ppethm_t2, replace)
margins ppethm, atmeans predict(outcome(topic3)) saving(ppethm_t3, replace)
margins ppethm, atmeans predict(outcome(topic4)) saving(ppethm_t4, replace)
 
combomarginsplot ppethm_t1 ppethm_t2 ppethm_t3 ppethm_t4, labels("Topic 1" "Topic 2" "Topic 3" "Topic 4")
graph save "$direct/Vignette1/Vig1_race_combined.gph", replace

margins ppeducat, atmeans predict(outcome(topic1)) saving(edu_t1, replace)
margins ppeducat, atmeans predict(outcome(topic2)) saving(edu_t2, replace)
margins ppeducat, atmeans predict(outcome(topic3)) saving(edu_t3, replace)
margins ppeducat, atmeans predict(outcome(topic4)) saving(edu_t4, replace)

combomarginsplot edu_t1 edu_t2 edu_t3 edu_t4, labels("Topic 1" "Topic 2" "Topic 3" "Topic 4")
graph save "$direct/Vignette1/Vig1_edu_combined.gph", replace

margins inc_quart, atmeans predict(outcome(topic1)) saving(inc_t1, replace)
margins inc_quart, atmeans predict(outcome(topic2)) saving(inc_t2, replace)
margins inc_quart, atmeans predict(outcome(topic3)) saving(inc_t3, replace)
margins inc_quart, atmeans predict(outcome(topic4)) saving(inc_t4, replace)

combomarginsplot inc_t1 inc_t2 inc_t3 inc_t4, labels("Topic 1" "Topic 2" "Topic 3" "Topic 4")
graph save "$direct/Vignette1/Vig1_inc_combined.gph", replace

*** Vertical 

graph combine "$direct/Vignette1/Vig1_race_combined.gph" "$direct/Vignette1/Vig1_edu_combined.gph" "$direct/Vignette1/Vig1_inc_combined.gph", cols(1) xsize(10) ysize(16)

*** Horizontal (for poster)

grc1leg "$direct/Vignette1/Vig1_race_combined.gph" "$direct/Vignette1/Vig1_edu_combined.gph" "$direct/Vignette1/Vig1_inc_combined.gph", legendfrom("$direct/Vignette1/Vig1_race_combined.gph" ) saving("$direct/Vignette1/Vig1_combined_flm_pp.gph", replace) cols(3)

graph display, ysize(10) xsize(20) 

*** Horizontal (for paper). Top Panel of Figure 3

grc1leg "$direct/Vignette1/Vig1_race_combined.gph" "$direct/Vignette1/Vig1_edu_combined.gph" "$direct/Vignette1/Vig1_inc_combined.gph", legendfrom("$direct/Vignette1/Vig1_race_combined.gph" ) saving("$direct/Vignettes_combined/Vig1_combined_flm_pp.gph", replace) cols(3)

graph display, ysize(10) xsize(20) // Labels and line colors changed manually
 
*******************************************************************************
****************** Analyzing Vignette 4 (Bedtime) *****************************
*******************************************************************************

use "allmerged.dta" if vig == 4 & !missing(topic), clear

// shows the highest probability of being in a particular topic
hist maxprob, percent

graph bar, over(topic) 
graph save "$direct/Vignette4/BTM_vig4_distribution.gph", replace
graph export "$direct/Vignette4/BTM_vig4_distribution.pdf", replace

graph bar if maxprob >= .6, over(topic) name(certain60, replace)
graph save "$direct/Vignette4/BTM_vig4_distribution_certain60.gph", replace
graph export "$direct/Vignette4/BTM_vig4_distribution_certain60.pdf", replace

recode ppethm 5=3 // put all "other" together
recode ppeducat 1=2 // collapse "less than high school" with "high school"
xtile inc_quart= ppincimp,n(4) // create four income quartiles

gen topic4_r=1-(topic1+topic2+topic3)
rename topic4 topic4_dec
rename topic4_r topic4
replace topic4=0 if topic4<0

gen sum_exact = topic1 + topic2 + topic3 + topic4 // this should add up to 1 exactly
replace sum_exact=1 if sum_exact>1

egen sum_rowtotal=rowtotal(topic1 topic2 topic3 topic4)
replace sum_rowtotal=1 if sum_rowtotal!=1 

*** Education, race, and income combined

fmlogit topic1 topic2 topic3 topic4 [pw = weight], eta(i.inc_quart i.ppeducat i.ppethm i.ppgender ppage mother son)
outreg2 using "$direct/Regressions/fml.xls", append ctitle(2) label bdec(3) dec(3)

*** Excluding race

fmlogit topic1 topic2 topic3 topic4 [pw = weight], eta(i.inc_quart i.ppeducat i.ppgender ppage mother son)
outreg2 using "$direct/Regressions/fml_excl_race.xls", append ctitle(1) label bdec(3) dec(3)

*** Excluding income

fmlogit topic1 topic2 topic3 topic4 [pw = weight], eta (i.ppeducat i.ppethm i.ppgender ppage mother son)
outreg2 using "$direct/Regressions/fml_excl_inc.xls", append ctitle(1) label bdec(3) dec(3)

*** Excluding education 

fmlogit topic1 topic2 topic3 topic4 [pw = weight], eta(i.inc_quart i.ppethm i.ppgender ppage mother son)
outreg2 using "$direct/Regressions/fml_excl_edu.xls", append ctitle(1) label bdec(3) dec(3)

*** Marginsplot

fmlogit topic1 topic2 topic3 topic4 [pw = weight], eta(i.inc_quart i.ppeducat i.ppethm i.ppgender ppage mother son)

margins ppethm, atmeans predict(outcome(topic1)) saving(ppethm_t1, replace)
margins ppethm, atmeans predict(outcome(topic2)) saving(ppethm_t2, replace)
margins ppethm, atmeans predict(outcome(topic3)) saving(ppethm_t3, replace)
margins ppethm, atmeans predict(outcome(topic4)) saving(ppethm_t4, replace)
 
combomarginsplot ppethm_t1 ppethm_t2 ppethm_t3 ppethm_t4, labels("Topic 1" "Topic 2" "Topic 3" "Topic 4")
graph save "$direct/Vignette4/Vig4_race_combined.gph", replace


margins ppeducat, atmeans predict(outcome(topic1)) saving(edu_t1, replace)
margins ppeducat, atmeans predict(outcome(topic2)) saving(edu_t2, replace)
margins ppeducat, atmeans predict(outcome(topic3)) saving(edu_t3, replace)
margins ppeducat, atmeans predict(outcome(topic4)) saving(edu_t4, replace)

combomarginsplot edu_t1 edu_t2 edu_t3 edu_t4, labels("Topic 1" "Topic 2" "Topic 3" "Topic 4")
graph save "$direct/Vignette4/Vig4_edu_combined.gph", replace

margins inc_quart, atmeans predict(outcome(topic1)) saving(inc_t1, replace)
margins inc_quart, atmeans predict(outcome(topic2)) saving(inc_t2, replace)
margins inc_quart, atmeans predict(outcome(topic3)) saving(inc_t3, replace)
margins inc_quart, atmeans predict(outcome(topic4)) saving(inc_t4, replace)

combomarginsplot inc_t1 inc_t2 inc_t3 inc_t4, labels("Topic 1" "Topic 2" "Topic 3" "Topic 4")
graph save "$direct/Vignette4/Vig4_inc_combined.gph", replace

*** Vertical 

graph combine "$direct/Vignette4/Vig4_race_combined.gph" "$direct/Vignette4/Vig4_edu_combined.gph" "$direct/Vignette4/Vig4_inc_combined.gph", cols(1) xsize(10) ysize(16)

*** Horizontal (for paper). Middle Panel of Figure 3

grc1leg "$direct/Vignette4/Vig4_race_combined.gph" "$direct/Vignette4/Vig4_edu_combined.gph" "$direct/Vignette4/Vig4_inc_combined.gph", legendfrom("$direct/Vignette4/Vig4_race_combined.gph" ) saving("$direct/Vignettes_combined/Vig4_combined_flm_pp.gph", replace) cols(3)

graph display, ysize(10) xsize(20) // Labels and line colors changed manually

*******************************************************************************
****************** Analyzing Vignette 5 (Medicine) ****************************
*******************************************************************************

use "allmerged.dta" if vig == 5 & !missing(topic), clear

// shows the highest probability of being in a particular topic
hist maxprob, percent

graph bar, over(topic) 
graph save "$direct/Vignette5/BTM_vig5_distribution.gph", replace
graph export "$direct/Vignette5/BTM_vig5_distribution.pdf", replace

graph bar if maxprob >= .6, over(topic) name(certain60, replace)
graph save "$direct/Vignette5/BTM_vig5_distribution_certain60.gph", replace
graph export "$direct/Vignette5/BTM_vig5_distribution_certain60.pdf", replace

recode ppethm 5=3 // put all "other" together
recode ppeducat 1=2 // collapse "less than high school" with "high school"
xtile inc_quart= ppincimp,n(4) // create four income quartiles

gen topic4_r=1-(topic1+topic2+topic3)
rename topic4 topic4_dec
rename topic4_r topic4
replace topic4=0 if topic4<0

gen sum_exact = topic1 + topic2 + topic3 + topic4 // this should add up to 1 exactly
replace sum_exact=1 if sum_exact>1

egen sum_rowtotal=rowtotal(topic1 topic2 topic3 topic4)
replace sum_rowtotal=1 if sum_rowtotal!=1 

*** Education, race, and income combined

fmlogit topic1 topic2 topic3 topic4 [pw = weight], eta(i.inc_quart i.ppeducat i.ppethm i.ppgender ppage mother son)
outreg2 using "$direct/Regressions/fml.xls", append ctitle(3) label bdec(3) dec(3)

*** Excluding race

fmlogit topic1 topic2 topic3 topic4 [pw = weight], eta(i.inc_quart i.ppeducat i.ppgender ppage mother son)
outreg2 using "$direct/Regressions/fml_excl_race.xls", append ctitle(1) label bdec(3) dec(3)

*** Excluding income

fmlogit topic1 topic2 topic3 topic4 [pw = weight], eta (i.ppeducat i.ppethm i.ppgender ppage mother son)
outreg2 using "$direct/Regressions/fml_excl_inc.xls", append ctitle(1) label bdec(3) dec(3)

*** Excluding education 

fmlogit topic1 topic2 topic3 topic4 [pw = weight], eta(i.inc_quart i.ppethm i.ppgender ppage mother son)
outreg2 using "$direct/Regressions/fml_excl_edu.xls", append ctitle(1) label bdec(3) dec(3)

*** Marginsplot

fmlogit topic1 topic2 topic3 topic4 [pw = weight], eta(i.inc_quart i.ppeducat i.ppethm i.ppgender ppage mother son)

margins ppethm, atmeans predict(outcome(topic1)) saving(ppethm_t1, replace)
margins ppethm, atmeans predict(outcome(topic2)) saving(ppethm_t2, replace)
margins ppethm, atmeans predict(outcome(topic3)) saving(ppethm_t3, replace)
margins ppethm, atmeans predict(outcome(topic4)) saving(ppethm_t4, replace)
 
combomarginsplot ppethm_t1 ppethm_t2 ppethm_t3 ppethm_t4, labels("Topic 1" "Topic 2" "Topic 3" "Topic 4")
graph save "$direct/Vignette5/Vig5_race_combined.gph", replace

margins ppeducat, atmeans predict(outcome(topic1)) saving(edu_t1, replace)
margins ppeducat, atmeans predict(outcome(topic2)) saving(edu_t2, replace)
margins ppeducat, atmeans predict(outcome(topic3)) saving(edu_t3, replace)
margins ppeducat, atmeans predict(outcome(topic4)) saving(edu_t4, replace)

combomarginsplot edu_t1 edu_t2 edu_t3 edu_t4, labels("Topic 1" "Topic 2" "Topic 3" "Topic 4")
graph save "$direct/Vignette5/Vig5_edu_combined.gph", replace

margins inc_quart, atmeans predict(outcome(topic1)) saving(inc_t1, replace)
margins inc_quart, atmeans predict(outcome(topic2)) saving(inc_t2, replace)
margins inc_quart, atmeans predict(outcome(topic3)) saving(inc_t3, replace)
margins inc_quart, atmeans predict(outcome(topic4)) saving(inc_t4, replace)

combomarginsplot inc_t1 inc_t2 inc_t3 inc_t4, labels("Topic 1" "Topic 2" "Topic 3" "Topic 4")
graph save "$direct/Vignette5/Vig5_inc_combined.gph", replace


graph combine "$direct/Vignette5/Vig5_race_combined.gph" "$direct/Vignette5/Vig5_edu_combined.gph" "$direct/Vignette5/Vig5_inc_combined.gph", cols(1) xsize(10) ysize(16)

*** Horizontal (for paper). Bottom Panel of Figure 3

grc1leg "$direct/Vignette5/Vig5_race_combined.gph" "$direct/Vignette5/Vig5_edu_combined.gph" "$direct/Vignette5/Vig5_inc_combined.gph", legendfrom("$direct/Vignette5/Vig5_race_combined.gph" ) saving("$direct/Vignettes_combined/Vig5_combined_flm_pp.gph", replace) cols(3)

graph display, ysize(10) xsize(20) // Labels and line colors changed manually



****************************************
*** Supplmentary analysis using all three vignettes for Appendix Table A5 and Figure A3
***************************************

use "allmerged.dta" if !missing(topic), clear

recode ppethm 5=3 // put all "other" together
recode ppeducat 1=2 // collapse "less than high school" with "high school"
xtile inc_quart= ppincimp,n(4) // create four income quartiles


lab def raceeth 1 "White" 2 "Black" 3 "Other" 4 "Hispanic"
lab val ppethm raceeth


lab def education 2 "HS (or <)" 3 "Some college" 4 "BA+"
lab val ppeducat education


gen anyped = 0
replace anyped = 1 if vig == 1 & topic == 4
replace anyped = 1 if vig == 4 & topic == 2
replace anyped = 1 if vig == 4 & topic == 3
replace anyped = 1 if vig == 5 & topic == 1
replace anyped = 1 if vig == 4 & topic == 4

gen anyprag = 0
replace anyprag = 1 if vig == 1 & topic == 1
replace anyprag = 1 if vig == 1 & topic == 2
replace anyprag = 1 if vig == 1 & topic == 3
replace anyprag = 1 if vig == 4 & topic == 4
replace anyprag = 1 if vig == 5 & topic == 2

gen anyneg = 0
replace anyneg = 1 if vig == 1 & topic == 3
replace anyneg = 1 if vig == 1 & topic == 4
replace anyneg = 1 if vig == 4 & topic == 3
replace anyneg = 1 if vig == 5 & topic == 3
replace anyneg = 1 if vig == 5 & topic == 4

gen anyassert = 0
replace anyassert = 1 if vig == 1 & topic == 1
replace anyassert = 1 if vig == 1 & topic == 4
replace anyassert = 1 if vig == 4 & topic == 3
replace anyassert = 1 if vig == 5 & topic == 3
replace anyassert = 1 if vig == 5 & topic == 4



eststo ped: logit anyped i.inc_quart i.ppeducat i.ppethm i.ppgender ppage mother son [pw = weight]
eststo assertive: logit anyassert i.inc_quart i.ppeducat i.ppethm i.ppgender ppage mother son [pw = weight]
eststo neg: logit anyneg i.inc_quart i.ppeducat i.ppethm i.ppgender ppage mother son [pw = weight]
eststo prag: logit anyprag i.inc_quart i.ppeducat i.ppethm i.ppgender ppage mother son [pw = weight]

esttab ped prag neg  assertive using "$direct/Extra/any_combined.rtf", replace $starplus label b(a2) se(a2) nobase



est restore ped
margins ppeducat, atmeans 
marginsplot, name(edu, replace) ytitle("") title("") xtitle("")

margins inc_quart, atmeans 
marginsplot, name(inc, replace) ytitle("") title("") xtitle("")

margins ppethm, atmeans 
marginsplot, name(eth, replace) ytitle("Pr(Any Pedogogic)") title("") xtitle("")

graph combine eth edu inc, rows(1) title("Any Pedogogic") ycommon 
graph save "$direct/Extra/anyped.gph", replace



est restore assertive
margins ppeducat, atmeans 
marginsplot, name(edu, replace) ytitle("") title("") xtitle("Education")

margins inc_quart, atmeans 
marginsplot, name(inc, replace) ytitle("") title("") xtitle("Income quartiles")

margins ppethm, atmeans 
marginsplot, name(eth, replace) ytitle("Pr(Any Assertive)") title("") xtitle("Race/ethnicity")


graph combine eth edu inc, rows(1) title("Any Assertive") ycommon 
graph save "$direct/Extra/anyassertive.gph", replace


est restore neg
margins ppeducat, atmeans 
marginsplot, name(edu, replace) ytitle("") title("") xtitle("")

margins inc_quart, atmeans 
marginsplot, name(inc, replace) ytitle("") title("") xtitle("")

margins ppethm, atmeans 
marginsplot, name(eth, replace) ytitle("Pr(Any Negotiated)") title("") xtitle("")


graph combine eth edu inc, rows(1) title("Any Negotiated") ycommon 
graph save "$direct/Extra/anyneg.gph", replace


est restore prag
margins ppeducat, atmeans 
marginsplot, name(edu, replace) ytitle("") title("") xtitle("")

margins inc_quart, atmeans 
marginsplot, name(inc, replace) ytitle("") title("") xtitle("")

margins ppethm, atmeans 
marginsplot, name(eth, replace) ytitle("Pr(Any Pragmatic)") title("") xtitle("")


graph combine eth edu inc, rows(1) title("Any Pragmatic") ycommon 
graph save "$direct/Extra/anyprag.gph", replace



graph combine "$direct/Extra/anyped.gph" "$direct/Extra/anyprag.gph" "$direct/Extra/anyneg.gph" "$direct/Extra/anyassertive.gph", col(1) xsize(12) ysize(16)

graph save "$direct/Extra/any_combined.gph", replace

graph export "$direct/Extra/any_combined.pdf", replace



