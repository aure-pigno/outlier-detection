# script to call to refresh the packages after updating them. 
# package dependencies have to be added in the corresponding dependencies vectors : outlier_dependencies, validation_dependencies

library(roxygen2)
library("devtools")
library("knitr")


# method to move inside a package from any previous location. only work in R studio. 
move_to_package <- function(name){
  if(getwd()!=dirname(rstudioapi::getActiveDocumentContext()$path)){
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  }
  setwd(name)
}

create_package <- function(name){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  # create the package structure 
  create(name)
  move_to_package(name)
  # create the test structure in the package 
  devtools::use_testthat()
}

update_package <- function(name, dependencies, testing=FALSE){
  move_to_package(name)
  
  # add dependecies
  sapply(dependencies, devtools::use_package)

  # generate the documentation
  document()
  
  if(testing){
    # run the tests 
    devtools::test()
  }
  
  # go back to the script folder
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  # install the package 
  install(name)
}

outlier_dependencies <- c(
  "psych", 
  "assertive", 
  "data.table",
  "e1071",
  "fields",
  "parallel",
  "mpmi",
  "ape",
  "igraph",
  "R.matlab",
  "caret",
  "ldbod",
  "Rlof",
  "weights"
)

validation_dependencies <- c(
  "igraph",
  "pROC"
)

# install the packages
update_package("outliers", outlier_dependencies)
update_package("validation", validation_dependencies)

# load the packages
library("outliers") 
library("validation") 