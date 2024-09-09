**** Replication of What's a Parent to Do? Measuring Cultural Logics of Parenting with Computational Text Analysis
**** Last updated: September 9, 2024

// Set your working directory here
cd XXXX

// Merge topics to full dataset

use BTMtopics_vig1.dta, clear

// assign each response to a topic based on the one it is most likely to belong to
// note this is not necessary for fmlogit 

egen maxprob = rowmax(X1 X2 X3 X4)
gen topic = .

forvalues i = 1/4 {
	replace topic = `i' if round(maxprob,.001) == round(X`i',.001) & !missing(X`i')
}

tab topic

recode topic (3 = 1)(1 = 2)(4 = 3)(2 = 4) // recode to put in descending order

tab topic

// round to nearest percent things cleaner
gen topic1 = round(X3,.01)
gen topic2 = round(X1,.01)
gen topic3 = round(X4,.01)
gen topic4 = round(X2,.01)

save BTM_vig1cleaned.dta, replace



use BTMtopics_vig4.dta, clear

egen maxprob = rowmax(X1 X2 X3 X4)
gen topic = .

forvalues i = 1/4 {
	replace topic = `i' if round(maxprob,.001) == round(X`i',.001) & !missing(X`i')
}

tab topic 

recode topic (2 = 1)(3 = 2)(4 = 3)(1 = 4)

tab topic 


gen topic1 = round(X2,.01)
gen topic2 = round(X3,.01)
gen topic3 = round(X4,.01)
gen topic4 = round(X1,.01)


save BTM_vig4cleaned.dta, replace


use BTMtopics_vig5.dta, clear

egen maxprob = rowmax(X1 X2 X3 X4)
gen topic = .

forvalues i = 1/4 {
	replace topic = `i' if round(maxprob,.001) == round(X`i',.001) & !missing(X`i')
}

tab topic

recode topic (4 = 1)(1 = 2)(2 = 3)(3 = 4)

tab topic

gen topic1 = round(X4,.01)
gen topic2 = round(X1,.01)
gen topic3 = round(X2,.01)
gen topic4 = round(X3,.01)

save BTM_vig5cleaned.dta, replace





// Combine the cleaned datasets with the original dataset

use alldata.dta, clear


merge 1:1 caseid using BTM_vig1cleaned.dta, update nogen
merge 1:1 caseid using BTM_vig4cleaned.dta, update nogen
merge 1:1 caseid using BTM_vig5cleaned.dta, update nogen


lab def topic 1 "Topic 1" 2 "Topic 2" 3 "Topic 3" 4 "Topic 4"
lab val topic topic




// lab var edu "BA degree"
// lab var inc "High income"

recode ppethm (1 = 0 "White")(nonmiss = 1 "Nonwhite"), gen(nonwhite)
lab var nonwhite "Nonwhite"

recode dov_q1_parent (2 = 0 "Father")(1 = 1 "Mother"), gen(mother)
lab var mother "Parent is a mother"

// in the vignette assingments, odd is Son and even is Daughter
gen son = .

foreach var in dov_momvignette1_1 dov_momvignette1_2 dov_momvignette1_3 dov_momvignette1_4 dov_dadvignette1_1 dov_dadvignette1_2 dov_dadvignette1_3 dov_dadvignette1_4 {

	replace son = mod(`var',2) if missing(son)

}
lab def son 1 "Son" 0 "Daughter"
lab val son son
lab var son "Child is a son"

save allmerged.dta, replace

