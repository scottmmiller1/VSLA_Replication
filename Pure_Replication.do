	clear
	version 12.1
	
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

**** Table 3: Balance

	preserve
		keep if post==0 & ls==1
		su fspoor4 meals logaeconsifl iganum2_ia pat4pln hb10 cemfloor assets if ls==1 & post==0
		mean fspoor4 meals logaeconsifl iganum2_ia pat4pln hb10 cemfloor assets if ls==1 & post==0 [pweight=weightall], vce(cluster vid)
			outreg2 using "$outreg\basebalance", excel replace sidew stats(N coef sd) noaster

		mean fspoor4 meals logaeconsifl iganum2_ia pat4pln hb10 cemfloor assets if ls==1 & post==0 & treat==1 [pweight=weightall], vce(cluster vid)
			outreg2 using "$outreg\basebalance", excel append sidew noaster stats(coef)
		clear matrix 
		
		mean fspoor4 meals logaeconsifl iganum2_ia pat4pln hb10 cemfloor assets if ls==1 & post==0 & treat==0 [pweight=weightall], vce(cluster vid)
			outreg2 using "$outreg\basebalance.xml", excel append sidew stats(coef) noaster
			
		eststo clear

		mean logak2_all if ss==1 & post==0 [pweight=weightlong], vce(cluster vid)
			outreg2 using "$outreg\basebalance", excel append sidew stats(N coef sd) noaster
		mean logak2_all if ss==1 & post==0 & treat==1  [pweight=weightlong], vce(cluster vid)
			outreg2 using "$outreg\basebalance", excel append sidew noaster stats(coef)
		mean logak2_all if ss==1 & post==0 & treat==0  [pweight=weightlong], vce(cluster vid)
			outreg2 using "$outreg\basebalance", excel append sidew noaster stats(coef)

		foreach var in fspoor4 meals logaeconsifl iganum2_ia   pat4pln hb10 cemfloor assets {
				eststo DiM`var': qui reg `var' treat i.blocks if ls==1 & post==0 [pweight=weightall], vce(cluster vid)
		}
					esttab, starlevels(* 0.1 ** 0.05 *** 0.01 ) t(%8.3f) stat( N chi2) drop(*block*)
		outreg2 [DiM*] using "$outreg\basebalancediff", excel replace stats(coef tstat) noparen

		reg logak2_all treat i.blocks if ls==1 & post==0 [pweight=weightlong], vce(cluster vid)

	restore


**** DONE Table 4: 

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

			
**** DONE Appendix Table A1

	svyset vid [pweight=weightall], strata(blocks)
	svy: mean vslamember if HHinterest==0 & treat==0 & ls==1
	reg vslamember HHinterest##treat i.blocks if post==1 & ls==1 [pweight=weightall], vce(cluster vid)
	outreg2 using "$outreg\Appendix1", excel replace 
	reg vslamember HHinterest#treat i.blocks if post==1 & ls==1 [pweight=weightall], vce(cluster vid)
	outreg2 using "$outreg\Appendix1", excel append 


**** DONE Table 5

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
				
**** DONE Appendix Table A2

	* Overall, savings did go up in control villages
		bys vid hhid: egen anymbe=max(vslamem)

		gen logak2_all_p1=log(ak2_all+1)
		svyset vid [pweight=weightall], strata(blocks)
	svy: mean logak2_all_p1  if treat == 0 & ss==1 & post==0
	svy: mean logak2_all_p1  if treat == 0 & ss==1 & post==1

	* Note this is the only time when we use: log(savings+1). 
	* This is because the tobit regression does not work with clustered standard errors, when there is only one village in a block
		reg logak2_all_p1  post i.blocks if treat == 0 & ss==1 [pweight=weightlong], vce(cluster vid)
			outreg2 using "$outreg\AppendixA2", excel replace dec(3)
		reg logak2_all_p1  post i.blocks if treat == 0 & HHinterest==1 & ss==1 [pweight=weightlong], vce(cluster vid)
			outreg2 using "$outreg\AppendixA2", excel append dec(3)
		 reg logak2_all_p1  post i.blocks if treat == 0 & HHinterest==0 & ss==1 [pweight=weightlong], vce(cluster vid)
			outreg2 using "$outreg\AppendixA2", excel append dec(3)
		 reg logak2_all_p1  post i.blocks if treat == 0 & anymbe==1 & ss==1 [pweight=weightlong], vce(cluster vid)
			outreg2 using "$outreg\AppendixA2", excel append dec(3)
		 reg logak2_all_p1  post i.blocks if treat == 0 & anymbe!=1 & ss==1 [pweight=weightlong], vce(cluster vid)
			outreg2 using "$outreg\AppendixA2", excel append dec(3)

	
**** DONE Table 6				
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
	
	
**** DONE Table 7

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
			outreg2 [DiffIn*] using "$outreg\Table7", excel replace 

	* approximately Page 21, what proportion have any savings.
	***** extensive margin
	gen anysavings=ak2_all!=0 if ak2_all!=. 
	svy: mean anysavings  if ss==1 & post==0
	svy: mean anysavings  if ss==1 & post==1 & treat==0 
	svy: mean anysavings  if ss==1 & post==1 & treat==1 

	svy: mean ak2_all  if treat == 1 & ss==1 & post==0
	svy: mean ak2_all  if treat == 1 & ss==1 & post==1

	***** intensive margin
	capture drop nosavings
	bys vid hhid (t): gen nosavings=ak2_all[1]==0 if ak2_all[1]!=.
	svy: mean ak2_all  if treat == 1 & ss==1 & post==0 & nosavings==0
	svy: mean ak2_all  if treat == 1 & ss==1 & post==1 & nosavings==0
	svy: mean ak2_all  if treat == 1 & ss==1 & post==0 & nosavings==1
	svy: mean ak2_all  if treat == 1 & ss==1 & post==1 & nosavings==1
	
**** Other calculations
	
	* Crowding out of ROSCAs etc. 
	* Revolving fund membership
	
	mean al_rfm [pweight=weightlong], vce(cluster vid) over(t treat)
	outreg2 using "$outreg\al_rfm", excel replace 
	mean al_any [pweight=weightall], vce(cluster vid) over(t treat)
	outreg2 using "$outreg\al_rfm", excel append 
	mean al_all [pweight=weightall], vce(cluster vid) over(t treat)
	outreg2 using "$outreg\al_rfm", excel append 
	mean al_mfi [pweight=weightall], vce(cluster vid) over(t treat)
	outreg2 using "$outreg\al_rfm", excel append 

	

* Page 26: Self-reported primary use of money from share-out (Dropped from final version, just talked about briefly in text)
		*total sout1-sout11 [pweight=weightlong] if post==1 & treat==1 & ss==1, cl(vid)  //to be deleted when decision is made on share out graph
		
		gen soutx=0
		foreach i of varlist sout1-sout11 {
		replace soutx=1 if `i'==1
		}
		
		mean sout1-sout11 [pweight=weightlong] if post==1 & treat==1 & ss==1 & soutx==1, cl(vid)
		est store sout

		#delimit ;
			estout sout
			using "$outreg\useofshareout.out", replace
			cells(b(fmt(2)) ) 
			stats(N, fmt(0))
			label 
			collabels(none)
			;
		#delimit cr


* Table 8: ITT Effects on Credit Outcomes


* Create mean and sd of coefficients. Note that for log values, we just impute 
su anyloan numloan anyinvloan numinvloan  if post==0 & ss==1
	svyset vid [pweight=weightlong], strata(blocks)
	svy: mean anyloan numloan anyinvloan numinvloan if post==0 & ss==1
		outreg2 using "$outreg\Table8meansd", excel replace sidew stats(N coef) noaster
	estat sd

	* Population estimate of non-zero values
foreach var in logtotloan logtotagriloan logtotbusloan {	
	svy: mean `var' if post==0 & ss==1
		outreg2 using "$outreg\Table8meansd", excel append sidew stats(N coef) noaster
	}
	
	* Sample stnadrd deviation
	su logtotloan logtotagriloan logtotbusloan if post==0 & ss==1
	
	
eststo clear
	* regressions
	eststo DiM: qui reg `var' treat i.blocks if post==1 & ss==1 [pweight=weightlong], vce(cluster vid)
			outreg2 [Di*] using "$outreg\Table8", excel replace ctitle("initial")

	
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
			outreg2 [Di*] using "$outreg\Table8", excel append  bdec(3) bfmt(f) sdec(2) sfmt(f)
			}

eststo clear
		foreach var in logct_totloan logct_totagriloan logct_totbusloan {
			eststo Model`var': tobit `var' treat i.blocks if post==1 & ss==1 [pweight=weightlong], vce(cluster vid) ll
	}
				esttab, starlevels(* 0.1 ** 0.05 *** 0.01 ) b(%8.3f) se(%8.2f) stat( N chi2) drop(*block*)
			outreg2 [Model*] using "$outreg\Table8_log", excel replace  bdec(3) bfmt(f) sdec(2) sfmt(f)
*/

* Page 27. Self-reported Use of VSLA Credit
	* Generating descriptives by estimating totals
	capture drop vnumloanx
		gen vnumloanx=0
		foreach i of varlist vnumloanuse1-vnumloanuse11 {
		replace vnumloanx=1 if `i'==1
		}
		

		* VSLA loans only
		*total vnumloan* [pweight=weightlong] if t==3 & treat==1 & ss==1, cl(vid)
		
		mean vnumloanuse1-vnumloanuse11 [pweight=weightlong] if t==3 & ss==1 & vnumloanx==1, cl(vid)
		estimates store vtottreat31

		*total vnumloan* [pweight=weightlong] if t==3 & treat==0 & ss==1, cl(vid)
		
		*mean vnumloanuse1-vnumloanuse11 [pweight=weightlong] if t==3 & treat==0 & ssx==1 & vnumloanx==1, cl(vid)
		*estimates store vtottreat30

		* Estout
		#delimit ;
		estout vtottreat31 ///vtottreat30
		using "$outreg\vloanusetreat2.out", replace
		cells(b(fmt(7)) ) 
		stats(N, fmt(0))
		label 
		collabels(none)
		mlabels("Treatment 2011" "Control 2011" )
		;

		#delimit cr


			
				
*******************************
* Table 9. ITT Effects on Maize Planting Practice and Outcomes
* first 3 variables are for all observations; last three are long.
**************************************************************************************************

* How to do this: loop and replace manually

capture drop xxvar xxvar2 line
gen xxvar=""
gen xxvar2=.
gen line=_n

format xxvar2 %9.2f
	svyset vid [pweight=weightall], strata(blocks)
	
	svy: mean fertusemaize if post==0 & ls==1
	matrix b=e(b)
	local b=b[1,1]
	replace xxvar2=int(`b'*1000+0.5)/1000 if line==1
	replace xxvar=string(xxvar2) if line==1
	estat sd
	matrix sd=r(sd)
	local sd=sd[1,1]
	replace xxvar2=int(`sd'*1000+0.5)/1000 if line==2
	replace xxvar="["+string(xxvar2)+"]" if line==2

	local run=2
foreach var in 	acresmaize logqmaize {
	svy: mean `var' if post==0 & ls==1
	matrix b=e(b)
	local b=b[1,1]
	replace xxvar2=int(`b'*1000+0.5)/1000 if line==`run'+1
	replace xxvar=string(xxvar2) if line==`run'+1
	estat sd
	matrix sd=r(sd)
	local sd=sd[1,1]
	replace xxvar2=int(`sd'*1000+0.5)/1000 if line==`run'+2
	replace xxvar="["+string(xxvar2)+"]" if line==`run'+2
	local run=`run'+2
	}

	svyset vid [pweight=weightlong], strata(blocks)
foreach var in anymaizesale logvalsale logvalmaizesale {
	svy: mean `var' if post==0 & ss==1
	matrix b=e(b)
	local b=b[1,1]
	replace xxvar2=int(`b'*1000+0.5)/1000 if line==`run'+1
	replace xxvar=string(xxvar2) if line==`run'+1
	estat sd
	matrix sd=r(sd)
	local sd=sd[1,1]
	replace xxvar2=int(`sd'*1000+0.5)/1000 if line==`run'+2
	replace xxvar="["+string(xxvar2)+"]" if line==`run'+2
	local run=`run'+2
	}


		eststo DiM: qui reg fertusemaize treat i.blocks if post==1 & ls==1 [pweight=weightall], vce(cluster vid)
			outreg2 [Di*] using "$outreg\Table9", excel replace ctitle("initial")

	
foreach var in fertusemaize acresmaize {
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
			esttab, starlevels(* 0.1 ** 0.05 *** 0.01 ) b(%8.3f) se(%8.2f)  stat( N chi2) drop(*block*)
			outreg2 [Di*] using "$outreg\Table9", excel append  bdec(3) bfmt(f) sdec(2) sfmt(f)
			}

foreach var in anymaizesale {
eststo clear
			eststo DiM: qui reg `var' treat i.blocks if post==1 & ls==1 [pweight=weightlong], vce(cluster vid)
				* Create lag.
				cap drop lag`var'
				cap drop dif`var'
				bys hhidnum (post): gen lag`var'=`var'[1]
				gen dif`var'=`var'-lag`var'
			eststo DiMLag: qui reg `var' treat lag`var' i.blocks if post==1 & ls==1 [pweight=weightlong], vce(cluster vid)
			eststo DiD: qui reg `var' treat treatXr3 post i.blocks if  ls==1 [pweight=weightlong], vce(cluster vid)
			eststo DiFE: qui reg dif`var' treat i.blocks if post==1 &  ls_fd==1 [pweight=weightlong], vce(cluster vid)
			esttab, starlevels(* 0.1 ** 0.05 *** 0.01 ) b(%8.3f) se(%8.2f)  stat( N chi2) drop(*block*)
			outreg2 [Di*] using "$outreg\Table9", excel append  bdec(3) bfmt(f) sdec(2) sfmt(f)
			}
			
			
eststo clear
		foreach var in logct_qmaize {
			eststo DiffIn`var': tobit `var' treat i.blocks if post==1 & ss==1 [pweight=weightall], vce(cluster vid) ll
	}
				esttab, starlevels(* 0.1 ** 0.05 *** 0.01 ) b(%8.3f) se(%8.2f) stat( N chi2) drop(*block*)
			outreg2 [Diff*] using "$outreg\Table9", excel append  bdec(3) bfmt(f) sdec(2) sfmt(f)
			
eststo clear
		foreach var in logct_valsale logct_valmaizesale {
			eststo DiffIn`var': tobit `var' treat i.blocks if post==1 & ss==1 [pweight=weightlong], vce(cluster vid) ll
	}
				esttab, starlevels(* 0.1 ** 0.05 *** 0.01 ) b(%8.3f) se(%8.2f) stat( N chi2) drop(*block*)
			outreg2 [Diff*] using "$outreg\Table9", excel append  bdec(3) bfmt(f) sdec(2) sfmt(f)

				
				**************************************************************************************************


* Table 10. Effects on Small Business Outcomes
* Panel A
	svyset vid [pweight=weightlong], strata(blocks)
	
	svy: mean busnum businc2 if post==0 & ss==1
	estat sd

foreach var in busnum businc2 {
eststo clear
			eststo DiM: qui reg `var' treat i.blocks if post==1 & ss==1 [pweight=weightlong], vce(cluster vid)
				* Create lag.
				cap drop lag`var'
				cap drop dif`var'
				bys hhidnum (post): gen lag`var'=`var'[1]
				gen dif`var'=`var'-lag`var'
			eststo DiMLag: qui reg `var' treat lag`var' i.blocks if post==1 & ls==1 [pweight=weightlong], vce(cluster vid)
			esttab, starlevels(* 0.1 ** 0.05 *** 0.01 ) se(%8.3f) stat( N chi2) drop(*block*)
			}

* Panel B: See separate do-file. This was done on a separate computer as qreg with weights only works with STATA 13.



**************************************************************************************************
* Appendix  Table A3



su fertuse hb27 hb26 buyseeds acresgrown acreslocal acrescompo acreshybrid /// 
	acrestobacco acrescot acresrice logqlocal logqcompo logqhybrid logqmaizeacre /// 
	logqlocalacre logqhybridacre anysale if post==0 &ls==1

	* The following shows which ones are short and long q
su fertuse hb27 hb26 buyseeds acresgrown acreslocal acrescompo acreshybrid /// 
	acrestobacco acrescot acresrice logqlocal logqcompo logqhybrid logqmaizeacre /// 
	logqlocalacre logqhybridacre anysale if post==0 & short==1

	
	svyset vid [pweight=weightlong], strata(blocks)
	svy: mean fertuse buyseeds acresgrown acrestobacco acrescot acresrice anysale if post==0 & ss==1
		*outreg2 using "$outreg\Table9meansd", excel replace sidew stats(N coef) noaster
	estat sd

	svy: mean hb26 if post==0 & ss==1
	estat sd
	 
	
	* Population estimate of non-zero values
svyset vid [pweight=weightall], strata(blocks)

foreach var in hb27 acreslocal acrescompo acreshybrid logqlocal logqcompo logqhybrid logqmaizeacre /// 
	logqlocalacre logqhybridacre {	
	svy: mean `var' if post==0 & ls==1
		estat sd
	}
	
foreach var in fertuse hb26 buyseeds acresgrown acrestobacco acrescot acresrice anysale {
eststo clear
			eststo DiM: qui reg `var' treat i.blocks if post==1 & ss==1 [pweight=weightlong], vce(cluster vid)
				* Create lag.
				cap drop lag`var'
				cap drop dif`var'
				bys hhidnum (post): gen lag`var'=`var'[1]
				gen dif`var'=`var'-lag`var'
			eststo DiMLag: qui reg `var' treat lag`var' i.blocks if post==1 & ss==1 [pweight=weightlong], vce(cluster vid)
			eststo DiD: qui reg `var' treat treatXr3 post i.blocks if  ss==1 [pweight=weightlong], vce(cluster vid)
			eststo DiFE: qui reg dif`var' treat i.blocks if post==1 &  ss_fd==1 [pweight=weightlong], vce(cluster vid)
			esttab, starlevels(* 0.1 ** 0.05 *** 0.01 ) se(%8.3f) stat( N chi2) drop(*block*)
			}

	* ALL
foreach var in hb27 acreslocal acrescompo acreshybrid {
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
			esttab, starlevels(* 0.1 ** 0.05 *** 0.01 ) se(%8.3f) stat( N chi2) drop(*block*)
			}

			*** Log variables are all in short and long
		eststo clear
foreach var in logct_qlocal logct_qcompo logct_qhybrid logct_qmaizeacre logct_qlocalacre logct_qhybridacre {
			eststo DiffIn`var': tobit `var' treat i.blocks if post==1 & ls==1 [pweight=weightall], vce(cluster vid) ll
	}
			esttab, starlevels(* 0.1 ** 0.05 *** 0.01 ) se(%8.3f) stat( N chi2) drop(*block*)

*****  FIGURES *****

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
		ttitle("") ytitle("VSLA members (proportion of households in treatment/control groups)")  ///
		graphregion(fcolor(white)) ///
		xlabel(588[12]612) 
	
		
		graph export "$outreg\Figure 3 - Takeup.wmf", replace

restore 

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
 	graph export "$outreg\timing of shareout prop weighted.wmf", replace

restore	

** Appendix Figure A1 regarding rooms
	
preserve

	* Reshape and compute data 
		keep vid hhid t short hb10 treat weightall split
		drop if split==1
		
		reshape wide hb10, i(vid hhid) j(t)		
		gen difRooms = hb103-hb101
		tab difRooms treat
		label var difRooms "Change in number of rooms from 2009 to 2011"	
		bysort difRooms treat: egen difpop=total(weightall)	
		bysort difRooms treat: drop if _n>1

		* As proportion of all households in both treatment and control areas (7190 as can be verified from the weights created by 
		* "Do-files_r1RE-ENTERED\Constructed data\Sampling Weights.do" using agricultural extension officers' lists. When dropping splits, this is 
		* 7016.
		* For some reason, one household has all observations missing exchept vid hhid. Slettes.
		drop if treat==.

		gen difpoppr=difpop/7016
		keep difpoppr difRooms treat 
		reshape wide difpoppr, i(difRooms) j(treat)
		label var difpoppr0 "Control villages" 
		label var difpoppr1 "Treatment village"
		*label var difRooms "Change in number of rooms from 2009 to 2011"
		
		* Draw graph
		label define rooms -5 "-5" -4 "-4" -3 "-3" -2 "-2" -1 "-1" 0 "	0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5"
		label value difRooms rooms
		

		 graph bar (asis) difpoppr0 difpoppr1, over(difRooms)								///
					bargap(-30)																///
					legend( label(1 "Control villages") label(2 "Treatment villages") )		///
					ytitle("Households (proportion)")										///
					note("Split households excluded") 										///
					scheme(s2mono)
				
				
		graph export "$outreg\Distribution of change in rooms over time.wmf", replace

restore	
			
**************************************************************************************************
