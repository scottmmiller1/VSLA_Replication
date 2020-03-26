**********************************
*** Ksoll, Lilleor, Lonborg, Rasmussen
*** Impact of Village Savings and Loans Associations: Evidence from a Cluster Randomized Trial 
*** This file creates tables and figures for the extension part of the paper - Creditworthiness, access and VSLA impact heterogeneity
**********************************
	
** Log and macro
	* please specify the folder for the data, log-file and outreg. Note: the outreg folder should be in that specific location. 
	global d1 "\Users\avuwa\OneDrive\Desktop\PhD\AEB 6933 - Applied Econometrics\Replication paper\VSLA_JDE_replication"
	global outreg "$d1\Replication_files\Outreg"
	*global dataandlog "$d1"
	
	*capture log close
	*log using "$dataandlog\Finalresubmission.smcl", replace	
	
	cd "$outreg"
	
** Load data	
	use "$d1/impact_replication_final.dta", clear
	
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

* --------------------------------------------------------------
* --------------------------------------------------------------

**** Create dummy variables for different levels of variable aj6

gen lack_of_collateral = (aj6==1) if aj6!=. 
gen not_able_to_for_group = (aj6==2) if aj6!=. 
gen no_need_for_credit = (aj6==3) if aj6!=. 
gen high_interest_rate = (aj6==4) if aj6!=. 
gen no_credit_institution = (aj6==5) if aj6!=. 
gen no_credit_worthiness = (aj6==6) if aj6!=. 
gen dont_like_debt = (aj6==7) if aj6!=. 
gen no_additional_loans = (aj6==8) if aj6!=. 


**** Table 1: Assess balance for variables aj1 to aj6

preserve	
			
	keep if post==0 & ls==1
	
	* 1: N, Mean, SD for full sample (changed to aweights)
	su aj1 aj2 aj3 aj4 aj5 lack_of_collateral not_able_to_for_group no_need_for_credit high_interest_rate no_credit_institution/*
	*/ no_credit_worthiness dont_like_debt no_additional_loans if ls==1 & post==0
	mean aj1 aj2 aj3 aj4 aj5 lack_of_collateral not_able_to_for_group no_need_for_credit high_interest_rate no_credit_institution/*
	*/ no_credit_worthiness dont_like_debt no_additional_loans if ls==1 & post==0 [aweight=weightlong], vce(cluster vid)
		outreg2 using table1e, stats(N coef sd) sidew noaster label excel replace
		
	* 2: Treatment mean for full sample
	mean aj1 aj2 aj3 aj4 aj5 lack_of_collateral not_able_to_for_group no_need_for_credit high_interest_rate no_credit_institution/*
	*/ no_credit_worthiness dont_like_debt no_additional_loans if ls==1 & post==0 & treat==1 [pweight=weightlong], vce(cluster vid)	
		outreg2 using table1e, stats(coef) sidew noaster ctitle(Treatment average) label excel append
		clear matrix 	
	
	* 3: Control mean for full sample
	mean aj1 aj2 aj3 aj4 aj5 lack_of_collateral not_able_to_for_group no_need_for_credit high_interest_rate no_credit_institution/*
	*/ no_credit_worthiness dont_like_debt no_additional_loans if ls==1 & post==0 & treat==0 [pweight=weightlong], vce(cluster vid)
		outreg2 using table1e, sidew stats(coef) noaster ctitle(Control average) label excel append
		eststo clear
	
	* 4: Balance tests
	foreach var in aj1 aj2 aj3 aj4 aj5 lack_of_collateral not_able_to_for_group no_need_for_credit high_interest_rate no_credit_institution/*
	*/ no_credit_worthiness dont_like_debt no_additional_loans {
		eststo DiM`var': qui reg `var' treat i.blocks if ls==1 & post==0 [pweight=weightlong], vce(cluster vid)
	}				
	esttab, starlevels(* 0.1 ** 0.05 *** 0.01 ) t(%8.3f) stat( N chi2) drop(*block*)
		outreg2 [DiM*] using table3_diff, stats(coef tstat) noparen excel replace
		*reg logak2_all treat i.blocks if ls==1 & post==0 [pweight=weightlong], vce(cluster vid)

restore	
	
	
**** Table 1: Balance - code for latex table

* Full sample variables
gl tab1_ext aj1 aj2 aj3 aj4 aj5 lack_of_collateral not_able_to_for_group no_need_for_credit high_interest_rate no_credit_institution/*
			*/ no_credit_worthiness dont_like_debt no_additional_loans  

local listsize : list sizeof global(tab1_ext)
tokenize $tab1_ext

forv i = 1/`listsize' {
		
	quietly {
		* full sample
		mean ``i'' if ls==1 & post==0 [pweight=weightlong], vce(cluster vid)
		ereturn list
		scalar N_``i'' = e(N) // N
		scalar mean_``i'' = _b[``i''] // mean
		scalar sd_``i'' = _se[``i'']  // sd
		* treatment
		mean ``i'' if ls==1 & post==0 & treat==1 [pweight=weightlong], vce(cluster vid)
		ereturn list
		scalar t_mean_``i'' = _b[``i''] // mean
		* control
		mean ``i''  if ls==1 & post==0 & treat==0 [pweight=weightlong], vce(cluster vid)
		ereturn list
		scalar c_mean_``i'' = _b[``i''] // mean
		* difference
		reg ``i'' treat i.blocks if ls==1 & post==0 [pweight=weightlong], vce(cluster vid)
		ereturn list
		scalar tval_``i'' = abs(_b[treat]/_se[treat])
		scalar df_`i' = `e(df_r)'
		
	* matrix for table
		matrix mat_`i' = (N_``i'',mean_``i'',sd_``i'',t_mean_``i'',c_mean_``i'',tval_``i'')
		}
}
matrix A = mat_1
forv i = 2/`listsize' { // appends into single matrix
	matrix A = A \ mat_`i'
}


* Table 1
frmttable using tab1e_summary.tex, tex statmat(A) sdec(2) coljust(l;c;l;l) ///
title("Table 1 - Summmary Statistics \& Treatment Balance") ///
ctitle("","(1)","(2)","(3)","(4)","(5)","(6)"\"Variable","N","Mean","sd","Treatment average","Control average","Difference (t-value)") ///
rtitle("Able to acquire loan"\"Asked for credit this year"\"Number of times asked for credit this year"\ ///
		"Denied credit this year"\ ///
		"Number of times credit was denied this year"\ ///
		"Lack of collateral"\ ///
		"Not able to form group"\"No need for credit"\"High interest rates or high bank fees"\"No access to credit institution"\ ///
		"No more credit worthiness"\"Do not like to be in debt"\"Bank or money lender will not give additional loans") replace
***************************************************************************************************************************************


* Heterogeneous effects analysis and tables
* ----------------------------------------------------------------------------

	
	
* variables 
* aj1 -aj5
forvalues i=1/8 {
	gen aj6_`i' = (aj6 == `i') if aj6 != 0
	replace aj6_`i' = . if aj6 ==. 
}

foreach v of varlist aj1 aj2 aj3 aj4 aj5 aj6_1 aj6_2 aj6_3 aj6_4 aj6_5 aj6_6 aj6_7 aj6_8 {	
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
	forvalues v=1/5 {
	quietly {		
		reg ``i'' treatXlagaj`v' treat lagaj`v' i.blocks if ss==1 & post==1 [pweight=weightlong], vce(cluster vid)
			lincom _b[treatXlagaj`v'] + _b[treat]
			return list
			scalar ``i''_par_`v' = `r(estimate)'
			scalar ``i''_se_`v' = `r(se)'
			scalar df_`v'_`i' = `r(df)'
			
		matrix mat_`i'_`v' = (``i''_par_`v',``i''_se_`v')
	
	}
	}
}


* savings
forvalues v=1/5 {
	quietly {
		tobit logct_ak2_all treatXlagaj`v' treat lagaj`v' i.blocks if post==1 & ls==1 [pweight=weightlong], vce(cluster vid) ll
		ereturn list
			scalar logct_ak2_all_par_`v' = _b[treat]
			scalar logct_ak2_all_se_`v' = _se[treat]
			scalar df_`v'_9 = `e(df_r)'
			
		matrix mat_9_`v' = (logct_ak2_all_par_`v',logct_ak2_all_se_`v')	
	}
}	
	
matrix A = mat_1_1
matrix B = mat_1_2
matrix C = mat_1_3
matrix D = mat_1_4
matrix E = mat_1_5


forv i = 2/9 { // appends into single matrix
	matrix A = A \ mat_`i'_1
	matrix B = B \ mat_`i'_2
	matrix C = C \ mat_`i'_3
	matrix D = D \ mat_`i'_4
	matrix E = E \ mat_`i'_5
	
}	


gl mat A B C D E
local mlistsize : list sizeof global(mat)
tokenize $mat

forv m = 1/`mlistsize' {
matrix stars``m''=J(9,2,0)
		forvalues k = 1/9{
			matrix stars``m''[`k',1] =   ///
			(abs(``m''[`k',1]/``m''[`k',2]) > invttail(df_`m'_`k',0.1/2)) +  ///
			(abs(``m''[`k',1]/``m''[`k',2]) > invttail(df_`m'_`k',0.05/2)) +  ///
			(abs(``m''[`k',1]/``m''[`k',2]) > invttail(df_`m'_`k',0.01/2))
		}
}


* Table
frmttable using tab2_ext.tex, tex statmat(A) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsA) asymbol(*,**,***) ///
title("Table 12 - Heterogeneous Effects Across Credit Dimensions") ///
ctitle("","(1)"\"Outcome","Able to aquire"\"","loan") ///
rtitle("Number of months with fewer than three meals a day"\""\"Number of meals yesterday"\""\ ///
		"17-Food consumption per week per adult equivalent (MK, log)"\""\ ///
		"Number of income-generating activities (including agriculture and livestock)"\""\ ///
		"Per capita expenditure predicted by USAID PAT (log)"\""\ ///
		"Size of house (number of rooms)"\""\"House has cement floor"\""\"Asset count"\""\"Total savings (log)"\"") replace
frmttable using tab2_ext.tex, tex statmat(B) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsB) asymbol(*,**,***) ///
ctitle("(2)"\"Asked for credit"\"this year") merge
frmttable using tab2_ext.tex, tex statmat(C) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsC) asymbol(*,**,***) ///
ctitle("(3)"\"Times asked for"\"credit this year") merge		
frmttable using tab2_ext.tex, tex statmat(D) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsD) asymbol(*,**,***) ///
ctitle("(4)"\"Denied credit"\"this year") merge	
frmttable using tab2_ext.tex, tex statmat(E) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsE) asymbol(*,**,***) ///
ctitle("(5)"\"Times credit was"\"denied this year") merge		 					
			

* Heterogeneous effects based on household propensity to borrow
* ------------------------------------------------------

* Generate and estimate logit model for predicted probabilities
gen aj1_hat = .
foreach i of var hhid {
	logit aj1 fspoor4 iganum2_ia vslamember ak2_all totloan anyloan busnum businc2 hhsize headage headedu fhead hb27 anyland assets fertuse valsale [pweight=weightlong] if post==0
	predict aj1hat
	replace aj1_hat=aj1hat if hhid == `i' 
	drop aj1hat
}

gen high = 1 if aj1_hat >= 0.75 & aj1_hat <= 1
replace high = 0 if aj1_hat < 0.75 

gen mode = 1 if aj1_hat >= 0.5 & aj1_hat < 0.75
replace mode = 0 if mode == . 
replace mode = . if aj1_hat == . 

gen low = 1 if aj1_hat >= 0 & aj1_hat < 0.5
replace low = 0 if aj1_hat > 0.5 
replace low = . if aj1_hat == .
 
tab high if post==0
tab mode if post==0
tab low if post==0

 
foreach v of varlist high mode low {	
	* create interaction
	gen treatX`v' = treat*`v'
	
}	

* regression and table
gl tab12 fspoor4 meals logaeconsifl iganum2_ia pat4pln hb10 cemfloor assets  

local listsize : list sizeof global(tab12)
tokenize $tab12

forv i = 1/`listsize' {
foreach v of varlist high mode low {	
	quietly {		
		reg ``i'' treatX`v' treat `v' i.blocks if ss==1 & post==1 [pweight=weightlong], vce(cluster vid)
			lincom _b[treatX`v'] + _b[treat]
			return list
			scalar ``i''_par_`v' = `r(estimate)'
			scalar ``i''_se_`v' = `r(se)'
			scalar df_`v'_`i' = `r(df)'
			matrix mat_`i'_`v' = (``i''_par_`v',``i''_se_`v')
	
	}
  }
}

* savings
foreach v of varlist high mode low {	
	quietly {
		tobit logct_ak2_all treatX`v' treat `v' i.blocks if post==1 & ls==1 [pweight=weightlong], vce(cluster vid) ll
		lincom _b[treatX`v'] + _b[treat]
		return list
		scalar logct_ak2_all_par_`v' = `r(estimate)'
		scalar logct_ak2_all_se_`v' = `r(se)'
		scalar df_`v'_9 = `r(df)'
			
		matrix mat_9_`v' = (logct_ak2_all_par_`v',logct_ak2_all_se_`v')	
	}
}	

matrix A = mat_1_high
matrix B = mat_1_mode
matrix C = mat_1_low



forv i = 2/9 { // appends into single matrix
	matrix A = A \ mat_`i'_high
	matrix B = B \ mat_`i'_mode
	matrix C = C \ mat_`i'_low
	
}	


gl mat A B C 
local mlistsize : list sizeof global(mat)
tokenize $mat

forv m = 1/`mlistsize' {
matrix stars``m''=J(9,2,0)
	foreach k of varlist high mode low {	
			matrix stars``m''[`k',1] =   ///
			(abs(``m''[`k',1]/``m''[`k',2]) > invttail(df_`m'_`k',0.1/2)) +  ///
			(abs(``m''[`k',1]/``m''[`k',2]) > invttail(df_`m'_`k',0.05/2)) +  ///
			(abs(``m''[`k',1]/``m''[`k',2]) > invttail(df_`m'_`k',0.01/2))
		}
}


* Table
frmttable using tab14_ext.tex, tex statmat(A) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsA) asymbol(*,**,***) ///
title("Table 14 - Heterogeneous effects across propensity to borrow dimensions") ///
ctitle("","(1)"\"Outcome","High Propensity to borrow") ///
rtitle("Number of months with fewer than three meals a day"\""\"Number of meals yesterday"\""\ ///
		"17-Food consumption per week per adult equivalent (MK, log)"\""\ ///
		"Number of income-generating activities (including agriculture and livestock)"\""\ ///
		"Per capita expenditure predicted by USAID PAT (log)"\""\ ///
		"Size of house (number of rooms)"\""\"House has cement floor"\""\"Asset count"\""\"Total savings (log)"\"") replace
frmttable using tab14_ext.tex, tex statmat(B) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsB) asymbol(*,**,***) ///
ctitle("(2)"\"Moderate Propensity to borrow") merge
frmttable using tab14_ext.tex, tex statmat(C) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsC) asymbol(*,**,***) ///
ctitle("(3)"\"Low Propensity to borrow") merge		
