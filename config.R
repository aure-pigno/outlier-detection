setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data_folder = "data/converted/"
result_folder = "data/result/"


public_files <- c(
  "cardio"
  ,"glass"
  ,"lympho"
  ,"wbc"
)

n_vec <- c(50, 200, 500)
d_vec <- c(10, 100, 1000)
k_sets = 10

thresholds <- c(0.01, 0.05, 0.10)

r_files <- NULL
mc_files <- NULL
mc_class_files <- NULL
classes <- c("class.a", "class.b")
for( n in n_vec){
  for(d in d_vec){
    for(i in 1:k_sets){
      r_files<- c(r_files, join(c("r_", n, "_", d, "_", i)))
      mc_files<- c(mc_files, join(c("mc_", n, "_", d, "_", i)))
      for (c in classes){
        mc_class_files<- c(mc_class_files, join(c("mc_", n, "_", d, "_", i,"_", c)))
      }
    }
  }
}

supervised_files <- c(mc_files, r_files, public_files)

### Methods ####
base_detector_methods = list("boxplot"= outliers::box_plot_univariate, 
                       "pca"= outliers::pca_score, 
                       "one_class"=outliers::one_class_svm,
                       "two_phases_clustering"= outliers::two_phases_clustering,
                       "hclustering"= outliers::hclustering, 
                       "pca_lof"= outliers::pca_lof,
                       "LOCI"=outliers::LOCI
                       ) 



# build the list of ensemble methods 
ensemble_methods = list()
ensemble_methods[[paste("ensemble", "mean", "b", sep="-")]] <- function(lor){
  outliers::ensemble_function(lor,mean, 0, 1, TRUE)
}
ensemble_methods[[paste("ensemble", "median", "b", sep="-")]] <- function(lor){
  outliers::ensemble_function(lor,median, 0, 1, TRUE)
}
ensemble_methods[[paste("ensemble", "max", "b", sep="-")]] <- function(lor){
  outliers::ensemble_function(lor,max, 0, 1, TRUE)
}
ensemble_methods[[paste("ensemble", "mean", "c", 0.2, sep="-")]] <- function(lor){
  outliers::ensemble_function(lor,mean, 20, 0.2, FALSE)
} 
ensemble_methods[[paste("ensemble", "mean", "c", 0.3, sep="-")]] <- function(lor){
  outliers::ensemble_function(lor,mean, 20, 0.3, FALSE)
} 
ensemble_methods[[paste("ensemble", "mean", "c", 0.4, sep="-")]] <- function(lor){
  outliers::ensemble_function(lor,mean, 20, 0.4, FALSE)
} 
ensemble_methods[[paste("ensemble", "median", "c", 0.2, sep="-")]] <- function(lor){
  outliers::ensemble_function(lor,median, 20, 0.2, FALSE)
} 
ensemble_methods[[paste("ensemble", "median", "c", 0.3, sep="-")]] <- function(lor){
  outliers::ensemble_function(lor,median, 20, 0.3, FALSE)
} 
ensemble_methods[[paste("ensemble", "median", "c", 0.4, sep="-")]] <- function(lor){
  outliers::ensemble_function(lor,median, 20, 0.4, FALSE)
} 
ensemble_methods[[paste("ensemble", "max", "c", 0.2, sep="-")]] <- function(lor){
  outliers::ensemble_function(lor,max, 20, 0.2, FALSE)
} 
ensemble_methods[[paste("ensemble", "max", "c", 0.3, sep="-")]] <- function(lor){
  outliers::ensemble_function(lor,max, 20, 0.3, FALSE)
} 
ensemble_methods[[paste("ensemble", "max", "c", 0.4, sep="-")]] <- function(lor){
  outliers::ensemble_function(lor, max, 20, 0.4, FALSE)
} 
ensemble_methods[[paste("ensemble", "mean", "d", 0.2, sep="-")]] <- function(lor){
  outliers::ensemble_function(lor,mean, 20, 0.2, TRUE)
} 
ensemble_methods[[paste("ensemble", "mean", "d", 0.3, sep="-")]] <- function(lor){
  outliers::ensemble_function(lor,mean, 20, 0.3, TRUE)
} 
ensemble_methods[[paste("ensemble", "mean", "d", 0.4, sep="-")]] <- function(lor){
  outliers::ensemble_function(lor,mean, 20, 0.4, TRUE)
} 
ensemble_methods[[paste("ensemble", "median", "d", 0.2, sep="-")]] <- function(lor){
  outliers::ensemble_function(lor,median, 20, 0.2, TRUE)
} 
ensemble_methods[[paste("ensemble", "median", "d", 0.3, sep="-")]] <- function(lor){
  outliers::ensemble_function(lor,median, 20, 0.3, TRUE)
} 
ensemble_methods[[paste("ensemble", "median", "d", 0.4, sep="-")]] <- function(lor){
  outliers::ensemble_function(lor,median, 20, 0.4, TRUE)
} 
ensemble_methods[[paste("ensemble", "max", "d", 0.2, sep="-")]] <- function(lor){
  outliers::ensemble_function(lor,max, 20, 0.2, TRUE)
} 
ensemble_methods[[paste("ensemble", "max", "d", 0.3, sep="-")]] <- function(lor){
  outliers::ensemble_function(lor,max, 20, 0.3, TRUE)
} 
ensemble_methods[[paste("ensemble", "max", "d", 0.4, sep="-")]] <- function(lor){
  outliers::ensemble_function(lor, max, 20, 0.4, TRUE)
} 


greedy_ensemble <- list()
greedy_ensemble[["greedy_ensemble-mean-b"]] = function(lor){
  E = outliers::greedy_model_selection(lor)
  outliers::ensemble_function(E,mean, 0, 1, TRUE)
}
greedy_ensemble[["greedy_ensemble-median-b"]]  = function(lor){
  E = outliers::greedy_model_selection(lor)
  outliers::ensemble_function(E,median, 0, 1, TRUE)
}
greedy_ensemble[["greedy_ensemble-max-b"]] = function(lor){
  E = outliers::greedy_model_selection(lor)
  outliers::ensemble_function(E,max, 0, 1, TRUE)
}
greedy_ensemble[["greedy_ensemble-mean-c-0.2"]] = function(lor){
  E = outliers::greedy_model_selection(lor)
  outliers::ensemble_function(E,mean, 20, 0.2, FALSE)
}
greedy_ensemble[["greedy_ensemble-mean-c-0.3"]] = function(lor){
  E = outliers::greedy_model_selection(lor)
  outliers::ensemble_function(E,mean, 20, 0.3, FALSE)
}
greedy_ensemble[["greedy_ensemble-mean-c-0.4"]] = function(lor){
  E = outliers::greedy_model_selection(lor)
  outliers::ensemble_function(E,mean, 20, 0.4, FALSE)
}
greedy_ensemble[["greedy_ensemble-mean-d-0.2"]] = function(lor){
  E = outliers::greedy_model_selection(lor)
  outliers::ensemble_function(E,mean, 20, 0.2, TRUE)
}
greedy_ensemble[["greedy_ensemble-mean-d-0.3"]] = function(lor){
  E = outliers::greedy_model_selection(lor)
  outliers::ensemble_function(E,mean, 20, 0.3, TRUE)
}
greedy_ensemble[["greedy_ensemble-mean-d-0.4"]] = function(lor){
  E = outliers::greedy_model_selection(lor)
  outliers::ensemble_function(E,mean, 20, 0.4, TRUE)
}
greedy_ensemble[["greedy_ensemble-median-c-0.2"]] = function(lor){
  E = outliers::greedy_model_selection(lor)
  outliers::ensemble_function(E,median, 20, 0.2, FALSE)
}
greedy_ensemble[["greedy_ensemble-median-c-0.3"]] = function(lor){
  E = outliers::greedy_model_selection(lor)
  outliers::ensemble_function(E,median, 20, 0.3, FALSE)
}
greedy_ensemble[["greedy_ensemble-median-c-0.4"]] = function(lor){
  E = outliers::greedy_model_selection(lor)
  outliers::ensemble_function(E,median, 20, 0.4, FALSE)
}
greedy_ensemble[["greedy_ensemble-median-d-0.2"]] = function(lor){
  E = outliers::greedy_model_selection(lor)
  outliers::ensemble_function(E,median, 20, 0.2, TRUE)
}
greedy_ensemble[["greedy_ensemble-median-d-0.3"]] = function(lor){
  E = outliers::greedy_model_selection(lor)
  outliers::ensemble_function(E,median, 20, 0.3, TRUE)
}
greedy_ensemble[["greedy_ensemble-median-d-0.4"]] = function(lor){
  E = outliers::greedy_model_selection(lor)
  outliers::ensemble_function(E,median, 20, 0.4, TRUE)
}
greedy_ensemble[["greedy_ensemble-max-c-0.2"]] = function(lor){
  E = outliers::greedy_model_selection(lor)
  outliers::ensemble_function(E,max, 20, 0.2, FALSE)
}
greedy_ensemble[["greedy_ensemble-max-c-0.3"]] = function(lor){
  E = outliers::greedy_model_selection(lor)
  outliers::ensemble_function(E,max, 20, 0.3, FALSE)
}
greedy_ensemble[["greedy_ensemble-max-c-0.4"]] = function(lor){
  E = outliers::greedy_model_selection(lor)
  outliers::ensemble_function(E,max, 20, 0.4, FALSE)
}
greedy_ensemble[["greedy_ensemble-max-d-0.2"]] = function(lor){
  E = outliers::greedy_model_selection(lor)
  outliers::ensemble_function(E,max, 20, 0.2, TRUE)
}
greedy_ensemble[["greedy_ensemble-max-d-0.3"]] = function(lor){
  E = outliers::greedy_model_selection(lor)
  outliers::ensemble_function(E,max, 20, 0.3, TRUE)
}
greedy_ensemble[["greedy_ensemble-max-d-0.4"]] = function(lor){
  E = outliers::greedy_model_selection(lor)
  outliers::ensemble_function(E,max, 20, 0.4, TRUE)
}

all_ensemble = unlist(list(ensemble_methods, greedy_ensemble))
# join the two list in one vector 
detector_and_ensemble_methods = unlist(list(base_detector_methods,all_ensemble))


# preprocess_method
normalization_methods = c("no" , "boxcox" ,"asinh", "local_asinh")
pca_methods = c("no","basic")
bagging_methods = c("no","rd", "mi")

preprocess_methods = list()
for(normalization_method in normalization_methods){
  for(pca_method in pca_methods){
    preprocess_methods[[paste(normalization_method,pca_method, sep="_")]] <- function(x){
      outliers::preprocessing(data=x, preprocess_method=normalization_method, pca=pca_method=="basic")
    }
  }
}
