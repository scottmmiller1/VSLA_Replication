** VLSA Replication extension test

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
			ereturn list
			scalar ``i''_par_`v' = _b[treatXlagaj`v']
			scalar ``i''_se_`v' = _se[treatXlagaj`v']
			scalar df_`v'_`i' = `e(df_r)'
			
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
			
		
		
		
		
		
		
		
		
		
		
		
		
