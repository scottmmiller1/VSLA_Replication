
ssc install outreg2, replace
	
**********************************
*** Ksoll, Lilleor, Lonborg, Rasmussen
*** Impact of Village Savings and Loans Associations: Evidence from a Cluster Randomized Trial 
*** This file replicates the tables and figures in the paper
**********************************
	
** Log and macro
	* please specify the folder for the data, log-file and outreg. Note: the outreg folder should be in that specific location. 
	global d1 "/Users/scottmiller/Dropbox (UFL)/Labor Economics/Empirical Project/Ksoll et al., 2016/Replication data and do-file"
	global outreg "$d1/Pure_Replication/Outreg"
	global dataandlog "$d1"
	
	capture log close
	log using "$dataandlog\Finalresubmission.smcl", replace	
	
	cd "$outreg"
	
** Load data	
	use "$dataandlog/impact_replication_final.dta", clear

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
	
	
* --------------------------------------------------------------
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
 	
	

* Figure 3
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
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
