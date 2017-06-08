# Compute the scores for all the files and save the results in the data directory 
compute_scores<-function(files, base_detector_methods, data_folder, result_folder, preprocess_method, bagging_method, timeout_bag=5*60, timeout_meth=25*60, only_missing){
  for(file in files){
    dir <- join(c(result_folder, file, "/"))
    dir.create(result_folder, showWarnings = FALSE)
    dir.create(dir, showWarnings = FALSE)
    load(paste(data_folder, file, ".Rda", sep=""))
    preprocessed_x = preprocess_method(data$x)
    print(file)
    bagging = outliers::timeout(outliers:::feature_bagging, list(data=preprocessed_x, bagging=bagging_method), timeout = timeout_bag) 
    for(detector_name in names(base_detector_methods)){
      compute <- !(only_missing & file.exists(join(c(dir, file, "_",detector_name,".Rda"))))
      print(c(compute, Sys.time(), join(c(dir, file, "_",detector_name,".Rda"))))
      if(compute){  
        if(any(is.na(bagging))){
          result = list("tdelta"= timeout_bag, "fresult"= rep(NA, nrow(preprocessed_x)))
        }else{
          r = matrix(0, nrow = nrow(bagging), ncol =nrow(preprocessed_x) )
          t = 0
          for(i in 1:nrow(bagging)){
            feat = bagging[i,]
            result <- outliers::timeout(validation:::benchmark, list(base_detector_methods[[detector_name]], list(preprocessed_x[,feat[feat > 0]])), timeout = timeout_meth)
            if(any(is.na(result))){
              result = list("tdelta"= timeout_meth, "fresult"= rep(NA, nrow(preprocessed_x)))
            }
            r[i,] = result$fresult
            t= t+result$tdelta
          }
          result = list("tdelta" = t, "fresult" = apply(r,2,mean)) 
        }
        save(result, file=join(c(dir, file, "_",detector_name,".Rda")))
      }
    }
  }
}

# Compute the ensembles for all the files and save the results in the data directory 
compute_ensembles <-function(files, base_detector_methods, ensemble_methods, result_folder){
  for(file in files){
    dir <- paste(result_folder, file, "/" , sep="")
    print(file)
    print(dir)
    result_list = validation::rda_to_list(file, base_detector_methods, path=dir)
    result_list <- list()
    for(detector_name in names(base_detector_methods)){
      tryCatch({
        load(paste(dir,file,"_",detector_name,".Rda", sep=""))
        if(!any(is.na(result$fresult))){
          result_list[[detector_name]] <- result$fresult
        }
      }, error=function(e){}, finally = {})
    }
    sapply(names(ensemble_methods), function(ensemble_name){
      result <- validation:::benchmark(ensemble_methods[[ensemble_name]], list("lor"=result_list))
      save(result, file=paste(dir, file, "_",ensemble_name,".Rda", sep=""))
    })
  }
}

# Compute a matrix with all the auc and save the results in the data directory 
compute_auc<-function(files, detector_methods, result_folder, data_folder){
  auc_matrix = matrix(ncol = length(detector_methods), nrow=length(files))
  colnames(auc_matrix) <- names(detector_methods) 
  rownames(auc_matrix) <- files 
  for(file in files){
    dir <- paste(result_folder, file, "/" , sep="")
    load(join(c(data_folder, file, ".Rda")))
    for(detector_name in names(detector_methods)){
      tryCatch({
        load(join(c(dir, file, "_", detector_name, ".Rda")))
        if(!any(is.na(result$fresult))){
          auc_matrix[file,detector_name] <- pROC::roc(as.vector(data$y),result$fresult,auc=TRUE, direction="<")$auc
        }
      },error=function(e){
        auc_matrix[file,detector_name] <- NA
      }
      )
    }
    save(auc_matrix, file=paste(result_folder, "AUC_matrix.Rda", sep=""))
  }
}

# Compute an array with all the proportion of outliers found and save the results in the data directory 
compute_percent<-function(files, detector_methods, result_folder, data_folder, thresholds = c(0.01,0.05, 0.1)){
  percent_array = array(
                        dim = c(length(files), length(detector_methods), length(thresholds)),
                        dimnames = list(files, names(detector_methods),thresholds)
                        )
  for(file in files){
    dir <- paste(result_folder, file, "/" , sep="")
    load(join(c(data_folder, file, ".Rda")))
    for(detector_name in names(detector_methods)){
      tryCatch({
        load(join(c(dir, file, "_", detector_name, ".Rda")))
      for (threshold  in thresholds) {
        percent_array[file,detector_name, toString(threshold)] <- validation:::correctely_detected(result$fresult, threshold, data$y)
      }
      },error=function(e){
        percent_array[file,detector_name, toString(threshold)] <- NA
      }
      )
    }
  }
  save(percent_array, file=paste(result_folder, "percent_array.Rda", sep=""))
}

# Easier to use than paste 
join <- function(vec, sep="", prefix=NULL, postfix=NULL){
  paste(prefix, paste(vec, collapse=sep),postfix, sep="") 
}

# Draw plots  
draw_all_plots<-function(supervised_files, detector_methods, result_folder, data_folder, preprocess_method){
  print(names(detector_methods))
  for(file in supervised_files){
    print(file)
    dir <- paste(result_folder, file, "/" , sep="")
    validation::roc_display(file, file, detector_methods, path = dir, origin=data_folder)
    validation::pca_display(file, file, detector_methods, path = dir, origin = data_folder, preprocess_method=preprocess_method)
    validation::boxplot_display(file, file, detector_methods, path = dir, origin=data_folder)
  }
}
