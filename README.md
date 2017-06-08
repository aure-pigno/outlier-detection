directory:

data directory: (dnalitycs files are not present)
	converted: original datasets (Rda files)
	result: 
		directory for each preprocessing:
			AUC_matrix: Rda file that contains the AUC matrix for each dataset, for each detector
			percent_array: Rda file that contains the percent array for each dataset, for each detector, for each trechold
			fdirectory for each dataset:
				results files for each BASE detector 

Remarks: only "local_asinh_no_rd" contains all the files, otherwhise, for the r and mc datasets, only one will be present for each dimension 


R files:

make_package.R: compile the packages outliers and validation
fucntion.R: contains all the functions needed to compute the results
config.R: contains all the constants needed
gen_results.R: compute all the results and store them directly in the right place in the data directory 

(Warning: launching all the preprocessings, all the base detectors, all the files is really really long. Use only "local_asinh_no_rd" as preprocessing)


dependencies needed: 
ape,
assertive,
caret,
data.table,
devtools,
e1071,
fields,
igraph,
knitr,
mpmi,
ldbod,
pROC,
psych,
R.matlab, 
Rlof,
roxygen2,
rstudioapi,
weights




Order of execution:
make_package -> function -> config -> gen_results
