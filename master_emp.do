version 15.1

/*******************************************************************************
AEB 6933 - Empirical Project 
Scott Miller and Ben Avuwadah
DO FILE DIRECTORY 
	
	
emp1:
	Replicates all tables in the original paper.
	
emp2:
	Conducts extension tests across various credit and propensity to borrow
	dimensions
	
emp3:
	Generates credit rationing groups and calculates HTE across each dimension


*******************************************************************************/
clear all
*packages
*ssc install outreg2, replace


*pathways
gl d1 = "/Users/scottmiller/Dropbox (UFL)/Labor Economics/Empirical Project/Replication data and do files" // do files stored here
gl outreg "$d1/Outreg" // used to store output


* To run all do files
/*
forv i = 1/3 {
	do "$d1/emp`i'.do"
}
