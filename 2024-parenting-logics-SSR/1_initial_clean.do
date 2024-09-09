**** Replication of What's a Parent to Do? Measuring Cultural Logics of Parenting with Computational Text Analysis
**** Last updated: September 9, 2024

// Source of data: https://osf.io/bbehq/  

*** Set your working directory here (which should include the data file)
cd XXX

import spss using "TESS3_191_Ishizuka_Client.sav", case(lower) clear

// code the vignettes
recode dov_vignette01 (1/4 = 1 "bored")(5/8 = 2 "pictures")(9/12 = 3 "dinner")(13/16 = 4 "bedtime")(17/20 = 5 "medicine")(21/24 = 6 "bored"), gen(vig)

order caseid vig q1

save  "alldata.dta", replace


