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
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
