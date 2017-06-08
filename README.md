# Outlier detection

## Directories
#### data directory: (dnalitycs files are not present)
- converted: 
    - original datasets (Rda files)
- result: 
    - directory for each preprocessing:
        - AUC_matrix: Rda file that contains the AUC matrix for each dataset, for each detector
        - percent_array: Rda file that contains the percent array for each dataset, for each detector, for each trechold
        - directory for each dataset: results files for each BASE detector 

```
Warning: only "local_asinh_no_rd" contains all the files, otherwhise, for the r and mc datasets, only one will be present for each dimension 
```
#### validation directory: (sources of the validation package)
- DESCRIPTION: description of the package
- NAMESPACE: all the exported functions
- man:
    - Rd files: method descripton
- R:
    - data_generation.R: all the functions needed to generate a new dataset
    - linear.R: all the linear detectors (pca, one class SVM)
    - proximity_based.R: all the proximity based detectors (two_phases_clustering, hclustering, pca_lof, LOCI)
    - univariate.R: all the univariate base detectors (boxplot (and extreme_studentized_deviate))
    - utils.R: all the utils functions
		


#### outliers directory: (sources of the outliers package)
- DESCRIPTION: description of the package
- NAMESPACE: all the exported functions
- man:
    - Rd files: method descripton
- R:
    - ensemble.R: all the functions needed to compute all the ensemble
    - perturbation.R: all the functions needed to introduce perturbations into a dataset
    - utils.R: all the utils functions
    - visualisation.R: all the functions that plot a graph



## R files:
#### config.R: 
contains all the constants needed
#### fucntion.R: 
contains all the functions needed to compute the results
#### gen_results.R: 
compute all the results and store them directly in the right place in the data directory 
##### make_package.R: 
compile the packages outliers and validation

```
Warning: launching all the preprocessings, all the base detectors, all the files is really really long. Use only "local_asinh_no_rd" as preprocessing
```
## Additional informations
#### Order of execution:
```
make_package.R -> fucntion.R -> config.R -> gen_results.R
```

#### Dependencies needed: 
```
ape, assertive, caret, data.table, devtools, e1071,
fields, igraph, knitr, mpmi, ldbod, pROC, psych,
R.matlab,  Rlof, roxygen2, rstudioapi, weights
```
