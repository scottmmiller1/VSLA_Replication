
*ssc install outreg2, replace
	
**********************************
*** Ksoll, Lilleor, Lonborg, Rasmussen
*** Impact of Village Savings and Loans Associations: Evidence from a Cluster Randomized Trial 
*** This file replicates the tables and figures in the paper
**********************************
	
** Log and macro
	* please specify the folder for the data, log-file and outreg. Note: the outreg folder should be in that specific location. 
	global d1 "/Users/scottmiller/Dropbox (UFL)/Labor Economics/Empirical Project/Ksoll et al., 2016/Replication data and do-file"
	global outreg "$d1/Outreg"
	*global dataandlog "$d1"
	
	*capture log close
	*log using "$dataandlog\Finalresubmission.smcl", replace	
	
	cd "$outreg"
	
** Load data	
	use "$d1/impact_replication_final.dta", clear

* Create variables with log values

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
**** Table 3: Balance - excel (Pure replication method)
	
preserve	
			
	keep if post==0 & ls==1
	
	* 1: N, Mean, SD for full sample (changed to aweights)
	su fspoor4 meals logaeconsifl iganum2_ia pat4pln hb10 cemfloor assets if ls==1 & post==0
	mean fspoor4 meals logaeconsifl iganum2_ia pat4pln hb10 cemfloor assets if ls==1 & post==0 [aweight=weightall], vce(cluster vid)
		outreg2 using table3, stats(N coef sd) sidew noaster label excel replace
		
	* 2: Treatment mean for full sample
	mean fspoor4 meals logaeconsifl iganum2_ia pat4pln hb10 cemfloor assets if ls==1 & post==0 & treat==1 [pweight=weightall], vce(cluster vid)	
		outreg2 using table3, stats(coef) sidew noaster ctitle(Treatment average) label excel append
		clear matrix 	
	
	* 3: Control mean for full sample
	mean fspoor4 meals logaeconsifl iganum2_ia pat4pln hb10 cemfloor assets if ls==1 & post==0 & treat==0 [pweight=weightall], vce(cluster vid)
		outreg2 using table3, sidew stats(coef) noaster ctitle(Control average) label excel append
		eststo clear
	
	* 4: N, Mean, SD for limited sample - Log total savings - (changed to aweights)
	mean logak2_all if ss==1 & post==0 [aweight=weightlong], vce(cluster vid)
		outreg2 using table3, sidew stats(N coef sd) noaster label excel append
	
	* 5: Treatment mean for limited sample	
	mean logak2_all if ss==1 & post==0 & treat==1  [pweight=weightlong], vce(cluster vid)
		outreg2 using table3, sidew stats(coef) noaster label excel append
	
	* 6: Control mean for limited sample	
	mean logak2_all if ss==1 & post==0 & treat==0  [pweight=weightlong], vce(cluster vid)
		outreg2 using table3, sidew stats(coef) noaster label excel append
		
	* 7: Balance tests
	foreach var in fspoor4 meals logaeconsifl iganum2_ia   pat4pln hb10 cemfloor assets {
		eststo DiM`var': qui reg `var' treat i.blocks if ls==1 & post==0 [pweight=weightall], vce(cluster vid)
	}				
	esttab, starlevels(* 0.1 ** 0.05 *** 0.01 ) t(%8.3f) stat( N chi2) drop(*block*)
		outreg2 [DiM*] using table3_diff, stats(coef tstat) noparen excel replace
		reg logak2_all treat i.blocks if ls==1 & post==0 [pweight=weightlong], vce(cluster vid)

restore	
	
	
**** Table 3: Balance - code for latex table

* Full sample variables
gl tab3_summ1 fspoor4 meals logaeconsifl iganum2_ia pat4pln hb10 cemfloor assets  

local listsize : list sizeof global(tab3_summ1)
tokenize $tab3_summ1

forv i = 1/`listsize' {
		
	quietly {
		* full sample
		mean ``i'' if ls==1 & post==0 [pweight=weightall], vce(cluster vid)
		ereturn list
		scalar N_``i'' = e(N) // N
		scalar mean_``i'' = _b[``i''] // mean
		scalar sd_``i'' = _se[``i'']  // sd
		* treatment
		mean ``i'' if ls==1 & post==0 & treat==1 [pweight=weightall], vce(cluster vid)
		ereturn list
		scalar t_mean_``i'' = _b[``i''] // mean
		* control
		mean ``i''  if ls==1 & post==0 & treat==0 [pweight=weightall], vce(cluster vid)
		ereturn list
		scalar c_mean_``i'' = _b[``i''] // mean
		* difference
		reg ``i'' treat i.blocks if ls==1 & post==0 [pweight=weightall], vce(cluster vid)
		ereturn list
		scalar tval_``i'' = abs(_b[treat]/_se[treat])
		
	* matrix for table
		matrix mat_`i' = (N_``i'',mean_``i'',sd_``i'',t_mean_``i'',c_mean_``i'',tval_``i'')
		}
}
matrix A = (.,.,.,.,.,.)
forv i = 1/`listsize' { // appends into single matrix
	matrix A = A \ mat_`i'
}

* limited sample variable
	quietly {
		* full sample
		mean logak2_all if ss==1 & post==0 [pweight=weightlong], vce(cluster vid) 
		ereturn list
		scalar N_logak2_all = e(N) // N
		scalar mean_logak2_all = _b[logak2_all] // mean
		scalar sd_logak2_all = _se[logak2_all]  // sd
		* treatment
		mean logak2_all if ss==1 & post==0 & treat==1 [pweight=weightlong], vce(cluster vid)
		ereturn list
		scalar t_mean_logak2_all = _b[logak2_all] // mean
		* control
		mean logak2_all  if ss==1 & post==0 & treat==0 [pweight=weightlong], vce(cluster vid)
		ereturn list
		scalar c_mean_logak2_all = _b[logak2_all] // mean
		* difference
		reg logak2_all treat i.blocks if ss==1 & post==0 [pweight=weightlong], vce(cluster vid)
		ereturn list
		scalar tval_logak2_all = abs(_b[treat]/_se[treat])
		
	* matrix for table
		matrix mat_logak2_all = (N_logak2_all,mean_logak2_all,sd_logak2_all,t_mean_logak2_all,c_mean_logak2_all,tval_logak2_all)
		}

matrix B = mat_logak2_all


* Additional variables variables
gl tab3_summ2 headage hhsize fhead headedu anyland vslamember

local listsize : list sizeof global(tab3_summ2)
tokenize $tab3_summ2

forv i = 1/`listsize' {
		
	quietly {
		* full sample
		mean ``i'' if ls==1 & post==0 [pweight=weightall], vce(cluster vid)
		ereturn list
		scalar N_``i'' = e(N) // N
		scalar mean_``i'' = _b[``i''] // mean
		scalar sd_``i'' = _se[``i'']  // sd
		* treatment
		mean ``i'' if ls==1 & post==0 & treat==1 [pweight=weightall], vce(cluster vid)
		ereturn list
		scalar t_mean_``i'' = _b[``i''] // mean
		* control
		mean ``i''  if ls==1 & post==0 & treat==0 [pweight=weightall], vce(cluster vid)
		ereturn list
		scalar c_mean_``i'' = _b[``i''] // mean
		* difference
		reg ``i'' treat i.blocks if ls==1 & post==0 [pweight=weightall], vce(cluster vid)
		ereturn list
		scalar tval_``i'' = abs(_b[treat]/_se[treat])
		
	* matrix for table
		matrix mat_`i' = (N_``i'',mean_``i'',sd_``i'',t_mean_``i'',c_mean_``i'',tval_``i'')
		}
}
matrix C = (.,.,.,.,.,.\.,.,.,.,.,.)
forv i = 1/`listsize' { // appends into single matrix
	matrix C = C \ mat_`i'
}


* full matrix 
matrix T = A \ B \ C


* Table
frmttable using tab3_summary.tex, tex statmat(T) sdec(2) coljust(l;c;l;l) ///
title("Table 3 - Summmary Statistics \& Treatment Balance") ///
ctitle("","(1)","(2)","(3)","(4)","(5)","(6)"\"Variable","N","Mean","sd","Treatment average","Control average","Difference (t-value)") ///
rtitle("Project outcomes"\"Number of months with fewer than three meals a day"\"Number of meals yesterday"\ ///
		"17-Food consumption per week per adult equivalent (MK, log)"\ ///
		"Number of income-generating activities (including agriculture and livestock)"\ ///
		"Per capita expenditure predicted by USAID PAT (log)"\ ///
		"Size of house (number of rooms)"\"House has cement floor"\"Asset count"\"Total savings (log)"\ ///
		""\"Other household characteristics"\"Age of household head"\"Household size"\ ///
		"Household is female-headed"\"Years of education in household"\ ///
		"Household owns land"\"Household is member of VSLA") replace
 	
	
* --------------------------------------------------------------
**** Figure 3: VSLA Membership
preserve

	* Total number of HHs in area
		sum weightall if t==1 & treat==1 & ls==1
		local nohht `r(sum)'
		sum weightall if t==1 & treat==0 & ls==1
		local nohhc `r(sum)'
		
	**Recoding the three obs before jan 2008 to missing
		tab h3lb20_sttime, miss
		replace h3lb20_sttime=. if h3lb20_sttime<tm(2008-jan)
			
	** CALCULATE START TIMES FOR SPILLOVERS VS TREATED PARTICIPANTS
		gen comp1=[treat==1 & vslamember==1]
		gen always1=[treat==0 & vslamember==1]
		gen comp=0
		gen always=0
	
		replace comp=weightall if 	[treat==1 & vslamember==1]
		replace always=weightall if [treat==0 & vslamember==1]
		
		bysort h3lb20_sttime: egen Compliers=total(comp)
		bysort h3lb20_sttime: egen Alwaystaker=total(always)

		
	** KEEPING ONE OBS PER MONTH PROPORTION RELATIVE TO ALL HOUSEHOLDS
		bysort h3lb20_sttime: drop if _n>1	
		gen comp_sum=sum(Compliers)
		gen always_sum=sum(Alwaystaker)
		replace comp_sum=comp_sum/`nohht'
		replace always_sum=always_sum/`nohhc'
		
		label var comp_sum "Treatment villages"
		label var always_sum "Control villages"

	** GRAPHS
		tsset h3lb20_sttime
		
		twoway (tsline comp_sum) (tsline always_sum) if h3lb20_sttime<tm(2011-aug), ///
		tlabel(, format(%tmCCYY!mnn))  ///
		ttitle("") ytitle("VSLA members (proportion of households in" "treatment/control groups)")  ///
		graphregion(fcolor(white)) ///
		xlabel(588[12]612) 
	
		
		graph export figure3.png, replace

restore 	
	
	
* --------------------------------------------------------------
**** Table 4: VSLA membership (Pure replication method)

svyset vid [pweight=weightall], strata(blocks)
svy: mean vslamember if treat==0 & ls==1 & post==0
svy: mean vslamember if treat==1 & ls==1 & post==0
svy: mean vslamember if treat==0 & ls==1 & post==1
svy: mean vslamember if treat==1 & ls==1 & post==1
reg vslamem treat i.blocks if ls==1 & post==0 [pweight=weightall], vce(cluster vid)
reg vslamem treat i.blocks if ls==1 & post==1 [pweight=weightall], vce(cluster vid)
reg vslamem post i.blocks if ls==1 & treat==0 [pweight=weightall], vce(cluster vid)
reg vslamem post i.blocks if ls==1 & treat==1 [pweight=weightall], vce(cluster vid)
reg vslamem treat t treatXr3 i.blocks  if ls==1 [pweight=weightall], vce(cluster vid) 
	

**** Table 4: Membership - code for latex table

svyset vid [pweight=weightall], strata(blocks)

quietly {		
	foreach i in 0 1 {
		svy: mean vslamember if treat==0 & ls==1 & post==`i'
		ereturn list
		scalar control_`i' = _b[vslamember]
			
		svy: mean vslamember if treat==1 & ls==1 & post==`i'
		ereturn list
		scalar treat_`i' = _b[vslamember]
		
		reg vslamem treat i.blocks if ls==1 & post==`i' [pweight=weightall], vce(cluster vid)
		ereturn list
		scalar diff_`i' = _b[treat]

		* matrix for table
		matrix mat_`i' = (control_`i',treat_`i',diff_`i')
		
		}
	
	reg vslamem post i.blocks if ls==1 & treat==0 [pweight=weightall], vce(cluster vid)
	ereturn list
	scalar control_diff = _b[post]
	reg vslamem post i.blocks if ls==1 & treat==1 [pweight=weightall], vce(cluster vid)
	ereturn list
	scalar treat_diff = _b[post]
	
	reg vslamem treat t treatXr3 i.blocks  if ls==1 [pweight=weightall], vce(cluster vid) 
	ereturn list
	scalar diff_diff = _b[treatXr3]
	
	matrix mat_2 = (control_diff,treat_diff,diff_diff)
}

matrix A = mat_0
forv i = 1/2 { // appends into single matrix
	matrix A = A \ mat_`i'
}	

* Table
frmttable using tab4.tex, tex statmat(A) sdec(3) coljust(l;c;l;l) ///
title("Table 4 - VSLA membership") ///
ctitle("","Control villages","Treatment villages","Differences") ///
rtitle("Baseline (2009)"\"Endline (2011)"\"Difference") replace
 		

		
* --------------------------------------------------------------		
**** Table 5: Effects on predefined outcomes (Pure replication)

foreach var in fspoor4 meals logaeconsifl iganum2_ia pat4pln hb10 cemfloor assets {
eststo clear
			eststo DiM: qui reg `var' treat i.blocks if post==1 & ls==1 [pweight=weightall], vce(cluster vid)
				* Create lag.
				cap drop lag`var'
				cap drop dif`var'
				bys hhidnum (post): gen lag`var'=`var'[1]
				gen dif`var'=`var'-lag`var'
			eststo DiMLag: qui reg `var' treat lag`var' i.blocks if post==1 & ls==1 [pweight=weightall], vce(cluster vid)
			eststo DiD: qui reg `var' treat treatXr3 post i.blocks if  ls==1 [pweight=weightall], vce(cluster vid)
			eststo DiFE: qui reg dif`var' treat i.blocks if post==1 &  ls_fd==1 [pweight=weightall], vce(cluster vid)
			esttab, starlevels(* 0.1 ** 0.05 *** 0.01 ) b(%8.3f) se(%8.2f) stat( N chi2) drop(*block*)
			}

** Table 5 savings			
local var="logct_ak2_all "
eststo clear
			
			eststo DiM: quietly tobit `var' treat i.blocks if post==1 & ls==1 [pweight=weightlong], vce(cluster vid) ll
			esttab, starlevels(* 0.1 ** 0.05 *** 0.01 ) b(%8.3f) se(%8.2f) stat( N chi2) drop(*block*)		

** Note: to compute multiple hypothesis corrected sharpened q-values, 
** use the programs on Michael Anderson's website. 			
** You will need to manually input the unadjusted p-values that are created with the following commands
		* The following outputs the unadjusted p-values needed for that procedure
/*	foreach var in fspoor4 meals logaeconsifl iganum2_ia pat4pln hb10 cemfloor assets {
eststo clear
			eststo DiM: qui reg `var' treat i.blocks if post==1 & ls==1 [pweight=weightall], vce(cluster vid)
				* Create lag.
				cap drop lag`var'
				cap drop dif`var'
				bys hhidnum (post): gen lag`var'=`var'[1]
				gen dif`var'=`var'-lag`var'
			eststo DiMLag: qui reg `var' treat lag`var' i.blocks if post==1 & ls==1 [pweight=weightall], vce(cluster vid)
			eststo DiD: qui reg `var' treat treatXr3 post i.blocks if  ls==1 [pweight=weightall], vce(cluster vid)
			eststo DiFE: qui reg dif`var' treat i.blocks if post==1 &  ls_fd==1 [pweight=weightall], vce(cluster vid)
			esttab, starlevels(* 0.1 ** 0.05 *** 0.01 ) b(%8.3f) p(%8.4f) stat( N chi2) drop(*block*) nopar
			}

** Table 5 savings			
local var="logct_ak2_all "
eststo clear
			
			eststo DiM: quietly tobit `var' treat i.blocks if post==1 & ls==1 [pweight=weightlong], vce(cluster vid) ll
			esttab, starlevels(* 0.1 ** 0.05 *** 0.01 ) b(%8.3f) p(%8.4f) stat( N chi2) drop(*block*) nopar		
*/				
					
	


**** Table 5: Effects - code for latex table

gl tab5 fspoor4 meals logaeconsifl iganum2_ia pat4pln hb10 cemfloor assets

local listsize : list sizeof global(tab5)
tokenize $tab5

forv i = 1/`listsize' {
	quietly {
		reg ``i'' treat i.blocks if post==1 & ls==1 [pweight=weightall], vce(cluster vid)
			ereturn list
			scalar ``i''_par_1 = _b[treat]
			scalar ``i''_se_1 = _se[treat]
			scalar df_1_`i' = `e(df_r)'
		
			* Create lag.
			cap drop lag``i''
			cap drop dif``i''
			bys hhidnum (post): gen lag``i''=``i''[1]
			gen dif``i''=``i''-lag``i''
			
		reg ``i'' treat lag``i'' i.blocks if post==1 & ls==1 [pweight=weightall], vce(cluster vid)
			ereturn list
			scalar ``i''_par_2 = _b[treat]
			scalar ``i''_se_2 = _se[treat]
			scalar df_2_`i' = `e(df_r)'
		
		reg ``i'' treat treatXr3 post i.blocks if  ls==1 [pweight=weightall], vce(cluster vid)
			ereturn list
			scalar ``i''_par_3 = _b[treatXr3]
			scalar ``i''_se_3 = _se[treatXr3]
			scalar df_3_`i' = `e(df_r)'
		
		reg dif``i'' treat i.blocks if post==1 &  ls_fd==1 [pweight=weightall], vce(cluster vid)
			ereturn list
			scalar ``i''_par_4 = _b[treat]
			scalar ``i''_se_4 = _se[treat]
			scalar df_4_`i' = `e(df_r)'
	
	
	* matrix for table
		matrix mat_`i'_1 = (``i''_par_1,``i''_se_1)
		matrix mat_`i'_2 = (``i''_par_2,``i''_se_2)
		matrix mat_`i'_3 = (``i''_par_3,``i''_se_3)
		matrix mat_`i'_4 = (``i''_par_4,``i''_se_4)
	}
}

matrix A = mat_1_1
matrix B = mat_1_2
matrix C = mat_1_3
matrix D = mat_1_4

forv i = 2/`listsize' { // appends into single matrix
	matrix A = A \ mat_`i'_1
	matrix B = B \ mat_`i'_2
	matrix C = C \ mat_`i'_3
	matrix D = D \ mat_`i'_4
}

gl mat A B C D
local mlistsize : list sizeof global(mat)
tokenize $mat

forv m = 1/`mlistsize' {
matrix stars``m''=J(`listsize',2,0)
		forvalues k = 1/`listsize'{
			matrix stars``m''[`k',1] =   ///
			(abs(``m''[`k',1]/``m''[`k',2]) > invttail(df_`m'_`k',0.1/2)) +  ///
			(abs(``m''[`k',1]/``m''[`k',2]) > invttail(df_`m'_`k',0.05/2)) +  ///
			(abs(``m''[`k',1]/``m''[`k',2]) > invttail(df_`m'_`k',0.01/2))
		}
}


** Table 5 savings			

tobit logct_ak2_all treat i.blocks if post==1 & ls==1 [pweight=weightlong], vce(cluster vid) ll
	ereturn list
		scalar logct_ak2_all_par_1 = _b[treat]
		scalar logct_ak2_all_se_1 = _se[treat]
		scalar df_1_logct_ak2_all = `e(df_r)'
	
matrix E = (logct_ak2_all_par_1,logct_ak2_all_se_1)

matrix starsE=J(1,2,0)
		matrix starsE[1,1] =   ///
		(abs(E[1,1]/E[1,2]) > invttail(df_1_logct_ak2_all,0.1/2)) +  ///
		(abs(E[1,1]/E[1,2]) > invttail(df_1_logct_ak2_all,0.05/2)) +  ///
		(abs(E[1,1]/E[1,2]) > invttail(df_1_logct_ak2_all,0.01/2))





* Table
frmttable using tab5.tex, tex statmat(A) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsA) asymbol(*,**,***) ///
title("Table 5 - Effects on predefined outcomes") ///
ctitle("","(1)"\"Outcome","Difference in means") ///
rtitle("Number of months with fewer than three meals a day"\""\"Number of meals yesterday"\""\ ///
		"17-Food consumption per week per adult equivalent (MK, log)"\""\ ///
		"Number of income-generating activities (including agriculture and livestock)"\""\ ///
		"Per capita expenditure predicted by USAID PAT (log)"\""\ ///
		"Size of house (number of rooms)"\""\"House has cement floor"\""\"Asset count"\"") replace
frmttable using tab5.tex, tex statmat(B) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsB) asymbol(*,**,***) ///
ctitle("(2)"\"Difference in means with lag") merge
frmttable using tab5.tex, tex statmat(C) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsC) asymbol(*,**,***) ///
ctitle("(3)"\"Difference-in-difference") merge		
frmttable using tab5.tex, tex statmat(D) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsD) asymbol(*,**,***) ///
ctitle("(4)"\"First-difference") merge	
frmttable using tab5.tex, tex statmat(E) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsE) asymbol(*,**,***) ///
rtitle("Total savings (log)") append		
	

	
		
* --------------------------------------------------------------	
**** Table 6: Regression without split households (Pure replication)			
	preserve
	keep if split==0

	foreach var in fspoor4 meals logaeconsifl iganum2_ia pat4pln hb10 cemfloor assets {
	eststo clear
				eststo DiM: qui reg `var' treat i.blocks if post==1 & ls==1 [pweight=weightall], vce(cluster vid)
				eststo DiMLag: qui reg `var' treat lag`var' i.blocks if post==1 & ls==1 [pweight=weightall], vce(cluster vid)
				eststo DiD: qui reg `var' treat treatXr3 post i.blocks if  ls==1 [pweight=weightall], vce(cluster vid)
				eststo DiFE: qui reg dif`var' treat i.blocks if post==1 &  ls_fd==1 [pweight=weightall], vce(cluster vid)
				esttab, starlevels(* 0.1 ** 0.05 *** 0.01 ) b(%8.3f) se(%8.2f) stat( N chi2) drop(*block*)
				}
			
	* Table 6 savings			
	local var="logct_ak2_all "
	eststo clear
				eststo DiM: quietly tobit `var' treat i.blocks if post==1 & ls==1 [pweight=weightlong], vce(cluster vid) ll
				esttab, starlevels(* 0.1 ** 0.05 *** 0.01 ) b(%8.3f) se(%8.2f) stat( N chi2) drop(*block*)
				
	restore	
	
	
**** Table 6: Regression without split households - code for latex table
preserve
	keep if split==0


gl tab6 fspoor4 meals logaeconsifl iganum2_ia pat4pln hb10 cemfloor assets

local listsize : list sizeof global(tab6)
tokenize $tab6

forv i = 1/`listsize' {
	quietly {
		reg ``i'' treat i.blocks if post==1 & ls==1 [pweight=weightall], vce(cluster vid)
			ereturn list
			scalar ``i''_par_1 = _b[treat]
			scalar ``i''_se_1 = _se[treat]
			scalar df_1_`i' = `e(df_r)'
		
			
		reg ``i'' treat lag``i'' i.blocks if post==1 & ls==1 [pweight=weightall], vce(cluster vid)
			ereturn list
			scalar ``i''_par_2 = _b[treat]
			scalar ``i''_se_2 = _se[treat]
			scalar df_2_`i' = `e(df_r)'
		
		reg ``i'' treat treatXr3 post i.blocks if  ls==1 [pweight=weightall], vce(cluster vid)
			ereturn list
			scalar ``i''_par_3 = _b[treatXr3]
			scalar ``i''_se_3 = _se[treatXr3]
			scalar df_3_`i' = `e(df_r)'
		
		reg dif``i'' treat i.blocks if post==1 &  ls_fd==1 [pweight=weightall], vce(cluster vid)
			ereturn list
			scalar ``i''_par_4 = _b[treat]
			scalar ``i''_se_4 = _se[treat]
			scalar df_4_`i' = `e(df_r)'
	
	
	* matrix for table
		matrix mat_`i'_1 = (``i''_par_1,``i''_se_1)
		matrix mat_`i'_2 = (``i''_par_2,``i''_se_2)
		matrix mat_`i'_3 = (``i''_par_3,``i''_se_3)
		matrix mat_`i'_4 = (``i''_par_4,``i''_se_4)
	}
}

matrix A = mat_1_1
matrix B = mat_1_2
matrix C = mat_1_3
matrix D = mat_1_4

forv i = 2/`listsize' { // appends into single matrix
	matrix A = A \ mat_`i'_1
	matrix B = B \ mat_`i'_2
	matrix C = C \ mat_`i'_3
	matrix D = D \ mat_`i'_4
}

gl mat A B C D
local mlistsize : list sizeof global(mat)
tokenize $mat

forv m = 1/`mlistsize' {
matrix stars``m''=J(`listsize',2,0)
		forvalues k = 1/`listsize'{
			matrix stars``m''[`k',1] =   ///
			(abs(``m''[`k',1]/``m''[`k',2]) > invttail(df_`m'_`k',0.1/2)) +  ///
			(abs(``m''[`k',1]/``m''[`k',2]) > invttail(df_`m'_`k',0.05/2)) +  ///
			(abs(``m''[`k',1]/``m''[`k',2]) > invttail(df_`m'_`k',0.01/2))
		}
}


** Table 6 savings			

tobit logct_ak2_all treat i.blocks if post==1 & ls==1 [pweight=weightlong], vce(cluster vid) ll
	ereturn list
		scalar logct_ak2_all_par_1 = _b[treat]
		scalar logct_ak2_all_se_1 = _se[treat]
		scalar df_1_logct_ak2_all = `e(df_r)'
	
matrix E = (logct_ak2_all_par_1,logct_ak2_all_se_1)

matrix starsE=J(1,2,0)
		matrix starsE[1,1] =   ///
		(abs(E[1,1]/E[1,2]) > invttail(df_1_logct_ak2_all,0.1/2)) +  ///
		(abs(E[1,1]/E[1,2]) > invttail(df_1_logct_ak2_all,0.05/2)) +  ///
		(abs(E[1,1]/E[1,2]) > invttail(df_1_logct_ak2_all,0.01/2))





* Table
frmttable using tab6.tex, tex statmat(A) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsA) asymbol(*,**,***) ///
title("Table 6 - Regressions without split households") ///
ctitle("","(1)"\"Outcome","Difference in means"\"","no split households") ///
rtitle("Number of months with fewer than three meals a day"\""\"Number of meals yesterday"\""\ ///
		"17-Food consumption per week per adult equivalent (MK, log)"\""\ ///
		"Number of income-generating activities (including agriculture and livestock)"\""\ ///
		"Per capita expenditure predicted by USAID PAT (log)"\""\ ///
		"Size of house (number of rooms)"\""\"House has cement floor"\""\"Asset count"\"") replace
frmttable using tab6.tex, tex statmat(B) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsB) asymbol(*,**,***) ///
ctitle("(2)"\"Difference in means with lag"\"no split households") merge
frmttable using tab6.tex, tex statmat(C) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsC) asymbol(*,**,***) ///
ctitle("(3)"\"Difference-in-difference"\"no split households") merge		
frmttable using tab6.tex, tex statmat(D) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsD) asymbol(*,**,***) ///
ctitle("(4)"\"First-difference"\"no split households") merge	
frmttable using tab6.tex, tex statmat(E) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsE) asymbol(*,**,***) ///
rtitle("Total savings (log)") append		
	
	
restore		


* --------------------------------------------------------------
**** Table 7: ITT effects on saings outcomes (Pure replication) 

	************ Final variable does not replicate ***************

	label var logct_ak2_all  "Total savings (log)"
	label var logct_ak2_vsla "VSLA savings (log)"
	label var logct_ak2_nonvsla "Non-VSLA savings (log)"
	label var logct_ak2_frrel "Savings with friend/relative (log)"
	label var logct_ak2_bank "Savings with bank (log)"
	label var logct_ak2_home "Savings at home (log)"
			eststo clear
		qui foreach var in all vsla nonvsla frrel home bank {
			eststo DiffIn`var': tobit logct_ak2_`var' treat i.blocks if post==1 & ss==1 [pweight=weightlong], vce(cluster vid) ll
		}
			esttab, starlevels(* 0.1 ** 0.05 *** 0.01 ) se(%8.3f) stat( N chi2) drop(*block*)



**** Table 7: ITT effects on savings outcomes - code for latex table


gl tab7 all vsla nonvsla frrel home bank

local listsize : list sizeof global(tab7)
tokenize $tab7

forv i = 1/`listsize' {
	quietly {		
		tobit logct_ak2_``i'' treat i.blocks if post==1 & ss==1 [pweight=weightlong], vce(cluster vid) ll
			ereturn list
			scalar ``i''_par = _b[treat]
			scalar ``i''_se = _se[treat]
			scalar df_`i' = `e(df_r)'
			
		matrix mat_`i' = (``i''_par,``i''_se)
	
	}
}

matrix A = mat_1
forv i = 2/`listsize' { // appends into single matrix
	matrix A = A \ mat_`i'
}	

matrix starsA=J(`listsize',2,0)
		forvalues k = 1/`listsize'{
			matrix starsA[`k',1] =   ///
			(abs(A[`k',1]/A[`k',2]) > invttail(df_`k',0.1/2)) +  ///
			(abs(A[`k',1]/A[`k',2]) > invttail(df_`k',0.05/2)) +  ///
			(abs(A[`k',1]/A[`k',2]) > invttail(df_`k',0.01/2))
		}



* Table
frmttable using tab7.tex, tex statmat(A) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsA) asymbol(*,**,***) ///
title("Table 7 - ITT effects on savings outcomes") ///
ctitle("","(1)"\"Outcome","Differences in means") ///
rtitle("Total savings (log)"\""\"VSLA savings (log)"\""\"Non-VSLA savings (log)"\""\ ///
		"Savings with friend/relative (log)"\""\"Savings at home (log)"\""\"Savings with bank (log)"\"") replace
 					
			
					
* --------------------------------------------------------------
** Figure 4

preserve
	
	* Computing number of households joining VSLA in treatment group during the project
	sort hhidnum t
	gen lagmem=L2.vslamember
	gen difmem=vslamember-lagmem
	sum weightall if difmem==1 & treat==1 & ls==1
	local compliers "`r(sum)'"
	di "`r(sum)'
	sum weightall if so_yn==1 & treat==1 & ls==1
	di `r(sum)'
	
	*Percentage of new members in treatment group sharing out"
	di `r(sum)'/`compliers'

	* Reorganising data by collapse
	drop if clb24_sttime==.  //to ensure that they're counted as zero
	sort treat clb24_sttime
	collapse (sum) weightall, by(treat clb24_sttime) //summing all who sais they shared out in a month
	rename weightall so
	label var so "Number of households sharing out" 
	tsset treat clb24_sttime //setting time and panel
	tsfill

	* As proportion of all households in both treatment and control areas (7190 as can be verified from the weights created by 
	* "Do-files_r1RE-ENTERED\Constructed data\Sampling Weights.do" using agricultural extension officers' lists.
	
	gen so_pct1=so/7190
	label var so_pct1 "Number of households sharing out (proportion of all HHs)" 
	gen so_pct2=so/7190
	sum so_pct1
	di "`r(sum)'"
	
* Create graph
	twoway bar so_pct1 clb24_sttime, scheme(s1mono) graphregion(fcolor(white)) by(treat, legend(off)) tline(2011m8, lpattern(dash)) 
 	graph export figure4.png, replace

restore				
			
			
* --------------------------------------------------------------
**** Table 8: ITT Effects on Credit Outcomes


* Create mean and sd of coefficients. Note that for log values, we just impute 
su anyloan numloan anyinvloan numinvloan  if post==0 & ss==1
	svyset vid [pweight=weightlong], strata(blocks)
	svy: mean anyloan numloan anyinvloan numinvloan if post==0 & ss==1
		*outreg2 using "$outreg/Table8meansd", excel replace sidew stats(N coef) noaster
	estat sd

	* Population estimate of non-zero values
foreach var in logtotloan logtotagriloan logtotbusloan {	
	svy: mean `var' if post==0 & ss==1
		*outreg2 using "$outreg/Table8meansd", excel append sidew stats(N coef) noaster
	}
	
	* Sample stnadrd deviation
	su logtotloan logtotagriloan logtotbusloan if post==0 & ss==1
	
	
eststo clear
	* regressions
	eststo DiM: qui reg `var' treat i.blocks if post==1 & ss==1 [pweight=weightlong], vce(cluster vid)
			*outreg2 [Di*] using "$outreg/Table8", excel replace ctitle("initial")

	
foreach var in anyloan  numloan anyinvloan numinvloan {
	eststo clear
			eststo DiM: reg `var' treat i.blocks if post==1 & ss==1 [pweight=weightlong], vce(cluster vid)
				* Create lag.
				cap drop lag`var'
				cap drop dif`var'
				bys vid hhid (post): gen lag`var'=`var'[1]
				gen dif`var'=`var'-lag`var'
			eststo DiMLag: qui reg `var' treat lag`var' i.blocks if post==1 & ss==1 [pweight=weightlong], vce(cluster vid)
			eststo DiD: qui reg `var' treat treatXr3 post i.blocks if  ss==1 [pweight=weightlong], vce(cluster vid)
			eststo DiFE: qui reg dif`var' treat i.blocks if post==1 &  ls_fd==1 [pweight=weightlong], vce(cluster vid)
			esttab, starlevels(* 0.1 ** 0.05 *** 0.01 ) b(%8.3f) se(%8.2f) stat( N chi2) drop(*block*)
			*outreg2 [Di*] using "$outreg/Table8", excel append  bdec(3) bfmt(f) sdec(2) sfmt(f)
			}

eststo clear
		foreach var in logct_totloan logct_totagriloan logct_totbusloan {
			eststo Model`var': tobit `var' treat i.blocks if post==1 & ss==1 [pweight=weightlong], vce(cluster vid) ll
	}
			*	esttab, starlevels(* 0.1 ** 0.05 *** 0.01 ) b(%8.3f) se(%8.2f) stat( N chi2) drop(*block*)
			*outreg2 [Model*] using "$outreg/Table8_log", excel replace  bdec(3) bfmt(f) sdec(2) sfmt(f)



**** Table 8: Effects on credit outcomes - code for latex table

* non-log variables
gl tab7_1 anyloan numloan anyinvloan numinvloan

local listsize_1 : list sizeof global(tab7_1)
tokenize $tab7_1

forv i = 1/`listsize_1' {
	quietly {
		* baseline mean
		su ``i'' if post==0 & ss==1
		svyset vid [pweight=weightlong], strata(blocks)
			svy: mean ``i'' if post==0 & ss==1
			estat sd
			matrix Avg = r(mean)
			matrix Sd = r(sd)
				scalar ``i''_par_1 = Avg[1,1]
				scalar ``i''_se_1 = Sd[1,1]
		* difference in means
		reg ``i'' treat i.blocks if post==1 & ss==1 [pweight=weightlong], vce(cluster vid)
			scalar ``i''_par_2 = _b[treat]
			scalar ``i''_se_2 = _se[treat]
			scalar df_2_`i' = `e(df_r)'
		* difference in means w/ lag
		* Create lag.
				cap drop lag``i''
				cap drop dif``i''
				bys vid hhid (post): gen lag``i''=``i''[1]
				gen dif``i''=``i''-lag``i''
		reg ``i'' treat lag``i'' i.blocks if post==1 & ss==1 [pweight=weightlong], vce(cluster vid)	
			scalar ``i''_par_3 = _b[treat]
			scalar ``i''_se_3 = _se[treat]
			scalar df_3_`i' = `e(df_r)'
		* difference in difference
		reg ``i'' treat treatXr3 post i.blocks if  ss==1 [pweight=weightlong], vce(cluster vid)
			scalar ``i''_par_4 = _b[treatXr3]
			scalar ``i''_se_4 = _se[treatXr3]
			scalar df_4_`i' = `e(df_r)'
		* first difference
		reg dif``i'' treat i.blocks if post==1 &  ls_fd==1 [pweight=weightlong], vce(cluster vid)
			scalar ``i''_par_5 = _b[treat]
			scalar ``i''_se_5 = _se[treat]
			scalar df_5_`i' = `e(df_r)'

	* matrix for table
		matrix mat_`i'_1 = (``i''_par_1,``i''_se_1)
		matrix mat_`i'_2 = (``i''_par_2,``i''_se_2)
		matrix mat_`i'_3 = (``i''_par_3,``i''_se_3)
		matrix mat_`i'_4 = (``i''_par_4,``i''_se_4)
		matrix mat_`i'_5 = (``i''_par_5,``i''_se_5)
	}	
}	

* log variables
gl tab7_2 logtotloan logtotagriloan logtotbusloan

local listsize_2 : list sizeof global(tab7_2)
tokenize $tab7_2

forv i = 1/`listsize_2' {
	*quietly {
		* baseline mean
		svy: mean ``i'' if post==0 & ss==1
			scalar ``i''_par_1 = _b[``i'']
		su ``i'' if post==0 & ss==1
			scalar ``i''_se_1 = `r(sd)'
		
	* matrix for table
		matrix ln_mat_`i'_1 = (``i''_par_1,``i''_se_1)	
}
			
gl tab7_2 logct_totloan logct_totagriloan logct_totbusloan	
tokenize $tab7_2
	
	forv i = 1/`listsize_2' {
		* difference in means
		tobit ``i'' treat i.blocks if post==1 & ss==1 [pweight=weightlong], vce(cluster vid) ll
			scalar ``i''_par_2 = _b[treat]
			scalar ``i''_se_2 = _se[treat]
			scalar df_6_`i' = `e(df_r)'
	
	* matrix for table
		matrix ln_mat_`i'_2 = (``i''_par_2,``i''_se_2)
	*}	
}			
		
matrix A = mat_1_1
matrix B = mat_1_2
matrix C = mat_1_3
matrix D = mat_1_4
matrix E = mat_1_5

forv i = 2/`listsize_1' { // appends into single matrix
	matrix A = A \ mat_`i'_1
	matrix B = B \ mat_`i'_2
	matrix C = C \ mat_`i'_3
	matrix D = D \ mat_`i'_4
	matrix E = E \ mat_`i'_5
}

matrix F = ln_mat_1_1
matrix G = ln_mat_1_2

forv i = 2/`listsize_2' { // appends into single matrix
	matrix F = F \ ln_mat_`i'_1
	matrix G = G \ ln_mat_`i'_2
}
	
	* add in log-stats to matrices A and B
	matrix A =  A \ F
	matrix B =  B \ G

* add significance level stars
gl mat A B C D E
local mlistsize : list sizeof global(mat)
tokenize $mat
gl allvars anyloan numloan anyinvloan numinvloan logtotloan logtotagriloan logtotbusloan
local listsize_all : list sizeof global(allvars)

forv m = 2/`mlistsize' {
matrix stars``m''=J(`listsize_1',2,0)
		forvalues k = 1/`listsize_1'{
			matrix stars``m''[`k',1] =   ///
			(abs(``m''[`k',1]/``m''[`k',2]) > invttail(df_`m'_`k',0.1/2)) +  ///
			(abs(``m''[`k',1]/``m''[`k',2]) > invttail(df_`m'_`k',0.05/2)) +  ///
			(abs(``m''[`k',1]/``m''[`k',2]) > invttail(df_`m'_`k',0.01/2))
		}
}

matrix starsG=J(`listsize_2',2,0)
		forvalues k = 1/`listsize_2'{
			matrix starsG[`k',1] =   ///
			(abs(G[`k',1]/G[`k',2]) > invttail(df_6_`k',0.1/2)) +  ///
			(abs(G[`k',1]/G[`k',2]) > invttail(df_6_`k',0.05/2)) +  ///
			(abs(G[`k',1]/G[`k',2]) > invttail(df_6_`k',0.01/2))
		}

matrix starsB = starsB \ starsG


* Table
frmttable using tab8.tex, tex statmat(A) sdec(3) substat(1) coljust(l;c;l;l)  ///
title("Table 5 - ITT effects on credit outcomes") ///
ctitle("","(1)"\"Outcome"\"","Baseline mean") ///
rtitle("Household had any loan in past 12 months"\""\"Number of loans active within past 12 months"\""\ ///
		"Household took out loan for investment purposes in past 12 months"\""\ ///
		"Number of investment loans"\""\"Total loan amount (log)^{a}"\""\ ///
		"Total amount borrowed for agricultural investments (log)^{a}"\ ///
		""\"Total amount borrowed for business purposes (log)^{a}"\"") replace
frmttable using tab8.tex, tex statmat(B) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsB) asymbol(*,**,***) ///
ctitle("(2)"\"Difference in means"\"") merge		
frmttable using tab8.tex, tex statmat(C) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsC) asymbol(*,**,***) ///
ctitle("(2)"\"Difference in means"\"with lag") merge
frmttable using tab8.tex, tex statmat(D) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsD) asymbol(*,**,***) ///
ctitle("(3)"\"Difference-in-difference"\"") merge		
frmttable using tab8.tex, tex statmat(E) sdec(3) substat(1) coljust(l;c;l;l) annotate(starsE) asymbol(*,**,***) ///
ctitle("(4)"\"First-difference"\"") merge		

















	
	
	
	
	
	
	
