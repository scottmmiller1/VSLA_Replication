**********************************
*** Ksoll, Lilleor, Lonborg, Rasmussen
*** Impact of Village Savings and Loans Associations: Evidence from a Cluster Randomized Trial 
*** Credit rationing definitions
**********************************
	
** Log and macro
	* please specify the folder for the data, log-file and outreg. Note: the outreg folder should be in that specific location. 
	*global d1 "\Users\avuwa\OneDrive\Desktop\PhD\AEB 6933 - Applied Econometrics\Replication paper\VSLA_JDE_replication"
	*global outreg "$d1\Replication_files\Outreg"
	global d1 "/Users/scottmiller/Dropbox (UFL)/Research/Projects/VSLA Replication/Replication data and do-file"
	global outreg "$d1/Outreg"
	
	*global dataandlog "$d1"
	
	*capture log close
	*log using "$dataandlog\Finalresubmission.smcl", replace	
	
	cd "$outreg"
	
** Load data	
	use "$d1/impact_replication_final.dta", clear
	
	
* -----------------------------------------------
* generate log variables	
foreach var of varlist ak2_vsla ak2_all ak2_home ak2_frrel ak2_nonvsla ak2_bank ///
	qcompo qmaize qlocal qhybrid qlocalacre qhybridacre ///
	qmaizeacre valsale valmaizesale busstock ///
	totloan totagriloan totbusloan ///
	businc2 {
			
		gen logct_`var' = ln(`var')
		sum logct_`var' if post==1
		replace logct_`var' = r(min)-0.000001 if `var'==0 & post==1
		sum logct_`var' if post==1
		replace logct_`var' = r(min)-0.000001 if `var'==0 & post==0			
	}
	

egen hhidnum = group(vid hhid)		
xtset hhidnum t

lab var logaeconsifl "17-Food consumption per week per adult equivalent (MK, log)"		
* -----------------------------------------------	
	
	
	
	
	
	

** creating credit rationing definitions (Guirkinger & Boucher, 2008)

* separate households that applied vs. those that did not apply for a formal loan
gen applied_pre = (aj2 == 1	& post == 0)
replace applied_pre =. if post == 1

tab applied_pre

* applicant households are classified according to the outcome:
	* rejected applicants are quantity rationed (constrained)
	* applicants whose demand was met are price-rationed (unconstrained)
gen constrained_pre = (aj4 == 1 & applied_pre == 1)	
replace constrained_pre =. if post == 1 | applied_pre == 0

tab constrained_pre

/* 
	non-applicant households
	asked whether a lender would offer them a loan if they were to apply
	 * if yes: asked why they did not apply
	    - households that had sufficient liquidity, interest rate was too high,
		   or had no profitable investments are price-rationed (unconstrained) 
		- households that said time, paperwork and fees of applying were too 
		   costly are transaction cost rationed (constrained) 
		- households that cited fear of losing their land were classified as risk
		  rationed (constrained)

	 * if no: asked whether or not they would apply for a loan if they were gauranteed
	   that a bank would approve their application. 
	    - households that said yes are quantity rationed (constrained) 
*/		

* if yes
tab aj1 if applied_pre == 0
	
tab aj6 if applied_pre == 0 & aj1 == 1
	
	tab constrained_pre
* unconstrained : did not apply & can get a loan but: 
	* has no need for credit
	replace constrained_pre = 0 if applied_pre == 0 & aj1 == 1 & aj6 == 3
	replace constrained_pre = 0 if applied_pre == 0 & aj1 == 1 & aj6b == 3
	replace constrained_pre = 0 if applied_pre == 0 & aj1 == 1 & aj6c == 3
	* interest rates too high
	replace constrained_pre = 0 if applied_pre == 0 & aj1 == 1 & aj6 == 4
	replace constrained_pre = 0 if applied_pre == 0 & aj1 == 1 & aj6b == 4
	replace constrained_pre = 0 if applied_pre == 0 & aj1 == 1 & aj6c == 4
	
	
* constrained : did not apply & can get a loan but:
	* fear of losing assets (risk averse)
		* do not like to be in debt
	replace constrained_pre = 1 if applied_pre == 0 & aj1 == 1 & aj6 == 7
	replace constrained_pre = 1 if applied_pre == 0 & aj1 == 1 & aj6b == 7
	replace constrained_pre = 1 if applied_pre == 0 & aj1 == 1 & aj6c == 7
	* lack of collateral
	replace constrained_pre = 1 if applied_pre == 0 & aj1 == 1 & aj6 == 1
	replace constrained_pre = 1 if applied_pre == 0 & aj1 == 1 & aj6b == 1
	replace constrained_pre = 1 if applied_pre == 0 & aj1 == 1 & aj6c == 1
	* no access to credit institution
	replace constrained_pre = 1 if applied_pre == 0 & aj1 == 1 & aj6 == 5
	replace constrained_pre = 1 if applied_pre == 0 & aj1 == 1 & aj6b == 5
	replace constrained_pre = 1 if applied_pre == 0 & aj1 == 1 & aj6c == 5
	* not able to form group
	replace constrained_pre = 1 if applied_pre == 0 & aj1 == 1 & aj6 == 2
	replace constrained_pre = 1 if applied_pre == 0 & aj1 == 1 & aj6b == 2
	replace constrained_pre = 1 if applied_pre == 0 & aj1 == 1 & aj6c == 2
	* lender will not give additional loans
	replace constrained_pre = 0 if applied_pre == 0 & aj1 == 1 & aj6 == 6
	replace constrained_pre = 0 if applied_pre == 0 & aj1 == 1 & aj6b == 6
	replace constrained_pre = 0 if applied_pre == 0 & aj1 == 1 & aj6c == 6
	
	
* constrained: did not apply & could not receive loan 
	replace constrained_pre = 1 if applied_pre == 0 & aj1 == 0
	
	
* --------------------------
* risk rationed
	* do not like to be in debt
gen risk_rationed = 1 if constrained_pre == 1 & aj6 == 7
replace risk_rationed = 1 if constrained_pre == 1 & aj6b == 7
replace risk_rationed = 1 if constrained_pre == 1 & aj6c == 7	
replace risk_rationed = 0 if risk_rationed==. & constrained_pre == 1

* --------------------------
* access rationed
	* no access to credit institution
gen access_rationed = 1 if constrained_pre == 1 & aj6 == 5
replace access_rationed = 1 if constrained_pre == 1 & aj6b == 5
replace access_rationed = 1 if constrained_pre == 1 & aj6c == 5	
	* not able to form group
replace access_rationed = 1 if constrained_pre == 1 & aj6 == 2
replace access_rationed = 1 if constrained_pre == 1 & aj6b == 2
replace access_rationed = 1 if constrained_pre == 1 & aj6c == 2
replace access_rationed = 0 if access_rationed==. & constrained_pre == 1

* --------------------------
* quantity rationed	
	* lack of collateral
gen quantity_rationed = 1 if applied_pre == 0 & aj1 == 1 & aj6 == 1
replace quantity_rationed = 1 if applied_pre == 0 & aj1 == 1 & aj6b == 1
replace quantity_rationed = 1 if applied_pre == 0 & aj1 == 1 & aj6c == 1	
	* lender will not give additional loans
replace quantity_rationed = 0 if applied_pre == 0 & aj1 == 1 & aj6 == 6
replace quantity_rationed = 0 if applied_pre == 0 & aj1 == 1 & aj6b == 6
replace quantity_rationed = 0 if applied_pre == 0 & aj1 == 1 & aj6c == 6
replace quantity_rationed = 0 if quantity_rationed==. & constrained_pre == 1	
	
	
	
	
	
	

	
* variables 
rename constrained_pre const_1
rename quantity_rationed const_2
rename risk_rationed const_3
rename access_rationed const_4

foreach v of varlist const_1 const_2 const_3 const_4 {	
	* Create lag.
	cap drop lag`v'
			cap drop dif`v'
			bys hhidnum (post): gen lag`v'=`v'[1]
			gen dif`v'=`v'-lag`v'
	
	* create interaction
	gen treatXlag`v' = treat*lag`v'
	
}	


* regression and table
gl tab3_summ1 fspoor4 meals logaeconsifl iganum2_ia pat4pln hb10 cemfloor assets  

local listsize : list sizeof global(tab3_summ1)
tokenize $tab3_summ1

forv i = 1/`listsize' {
	forvalues v=1/4 {
	quietly {		
		reg ``i'' treatXlagconst_`v' treat lagconst_`v' i.blocks if ss==1 & post==1 [pweight=weightlong], vce(cluster vid)
			lincom _b[treatXlagconst_`v'] + _b[treat]
			return list
			scalar ``i''_par_`v' = `r(estimate)'
			scalar ``i''_se_`v' = `r(se)'
			scalar df_`v'_`i' = `r(df)'
			
		matrix mat_`i'_`v' = (``i''_par_`v',``i''_se_`v')
	
	}
	}
}

/*
* savings
forvalues v=1/4 {
	quietly {
		tobit logct_ak2_all treatXlagconst_`v' treat lagconst_`v' i.blocks if post==1 & ls==1 [pweight=weightlong], vce(cluster vid) ll
			lincom _b[treatXlagconst_`v'] + _b[treat]
			return list
			scalar ``i''_par_`v' = `r(estimate)'
			scalar ``i''_se_`v' = `r(se)'
			scalar df_`v'_`i' = `r(df)'
			
		matrix mat_9_`v' = (logct_ak2_all_par_`v',logct_ak2_all_se_`v')	
	}
}	
*/
	
matrix A = mat_1_1
matrix B = mat_1_2
matrix C = mat_1_3
matrix D = mat_1_4


forv i = 2/8 { // appends into single matrix
	matrix A = A \ mat_`i'_1
	matrix B = B \ mat_`i'_2
	matrix C = C \ mat_`i'_3
	matrix D = D \ mat_`i'_4
	
}	


gl mat A B C D
local mlistsize : list sizeof global(mat)
tokenize $mat

forv m = 1/`mlistsize' {
matrix stars``m''=J(8,2,0)
		forvalues k = 1/8{
			matrix stars``m''[`k',1] =   ///
			(abs(``m''[`k',1]/``m''[`k',2]) > invttail(df_`m'_`k',0.1/2)) +  ///
			(abs(``m''[`k',1]/``m''[`k',2]) > invttail(df_`m'_`k',0.05/2)) +  ///
			(abs(``m''[`k',1]/``m''[`k',2]) > invttail(df_`m'_`k',0.01/2))
		}
}


* Table
frmttable using tab3_ext.tex, tex statmat(A) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsA) asymbol(*,**,***) ///
title("Table 13 - Heterogeneous Effects Across Credit Rationing Dimensions") ///
ctitle("","(1)"\"Outcome","Credit constrained") ///
rtitle("Number of months with fewer than three meals a day"\""\"Number of meals yesterday"\""\ ///
		"17-Food consumption per week per adult equivalent (MK, log)"\""\ ///
		"Number of income-generating activities (including agriculture and livestock)"\""\ ///
		"Per capita expenditure predicted by USAID PAT (log)"\""\ ///
		"Size of house (number of rooms)"\""\"House has cement floor"\""\"Asset count"\""\"Total savings (log)"\"") replace
frmttable using tab3_ext.tex, tex statmat(B) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsB) asymbol(*,**,***) ///
ctitle("(2)"\"Quantity rationed") merge
frmttable using tab3_ext.tex, tex statmat(C) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsC) asymbol(*,**,***) ///
ctitle("(3)"\"Risk rationed") merge		
frmttable using tab3_ext.tex, tex statmat(D) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsD) asymbol(*,**,***) ///
ctitle("(4)"\"Access rationed") merge			 						
	
	
	
	
	
	
