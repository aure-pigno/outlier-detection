source("functions.R")
source("config.R")

gen_results <- function(normalization_methods, pca_methods, bagging_methods, files, base_detector_methods, ensemble_methods, data_folder, result_folder,only_missing = TRUE){
  for(bagging_method in bagging_methods){
    for(pca_method in pca_methods){
      for(normalization_method in normalization_methods){
        print(c(normalization_method, pca_method, bagging_method))
        preprocess_name = join(c(normalization_method, "_", pca_method))
        preprocessing_folder = join(c(result_folder, preprocess_name, "_", bagging_method, "/"))
        preprocess_method <- preprocess_methods[[preprocess_name]]
        
        
        # Compute the base detectors scores
        compute_scores(files=files, base_detector_methods=base_detector_methods, data_folder = data_folder, result_folder = preprocessing_folder, preprocess_method=preprocess_method, bagging=bagging_method, only_missing=only_missing, timeout_meth = 30000000)
            
        # Compute the ensembles scores
        compute_ensembles(files = files, base_detector_methods=base_detector_methods, ensemble_methods =ensemble_methods, result_folder = preprocessing_folder )
            
        # Compute the AUC matrix
        compute_auc(files=files, detector_methods = unlist(list(base_detector_methods, ensemble_methods)), result_folder = preprocessing_folder, data_folder = data_folder)

        # Compute the percent array
        compute_percent(files=files, detector_methods = unlist(list(base_detector_methods, ensemble_methods)), result_folder = preprocessing_folder, data_folder = data_folder)
        
        # Draw the ROC curves, the pca, and the score boxplots 
        draw_all_plots(supervised_files=files, detector_methods=ensemble_methods, result_folder = preprocessing_folder, data_folder = data_folder, preprocess_method = preprocess_method)   
      }
    }  
  }  
}

gen_results(
  normalization_methods= "local_asinh" #normalization_methods 
  ,pca_methods= "no"#pca_methods 
  ,bagging_methods= "rd"#bagging_methods
  ,files=supervised_files[1:20] 
  ,base_detector_methods=base_detector_methods
  ,ensemble_methods = all_ensemble
  ,data_folder = data_folder
  ,result_folder = result_folder
  ,only_missing=TRUE
  )
