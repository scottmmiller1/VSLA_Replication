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
