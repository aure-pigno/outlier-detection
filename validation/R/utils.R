#' run the given function wrapped around two calls of Sys.time() and returns the execution time delta and the function result
#' 
#' @param f The function on which to perform the benchmark  
#' @param args the function arguments  
#' @return A list containing tdelta, the execution time and fresult, the function's return. 
#' @export
#' @examples
#' benchmark(mean, list(c(1,2,3)))
benchmark <- function(f, args){
  stime <- Sys.time()
  res <- do.call(f, args)
  etime <- Sys.time()
  dtime <- difftime(etime, stime) 
  list("tdelta"=as.numeric(dtime, units="secs"), "fresult"=res)
}


#' run the given function wrapped around two calls of Sys.time() and returns the execution time delta and the function result
#' 
#' @param dataset_name name of the files (without.RDA)
#' @param v_of_techniques list of techniques uses
#' @param path path to the document 
#' @return a list of results
#' @export
rda_to_list <- function(dataset_name, v_of_techniques, path="data/result/"){
  result_list <- list()
   for(technique in names(v_of_techniques)){
     name = paste(path,dataset_name,"_",technique,".Rda", sep="")
     tryCatch({
       load(name)
       if(!any(is.na(result$fresult))){
         result_list[[technique]] <- result$fresult
       }
     }, error=function(e){}, finally = {})
   }
   result_list
}



#' returns the percentage of correctly detected outliers given that we consider a given percentage of the dataset to be outlier 
#' 
#' @param scores a vector of outlier scores
#' @param percentage the percentage of dataset to be considered as outlier 
#' @param y the real outliers
#' @return the percentage of correctly detected outliers  
#' @export
correctely_detected<-function(scores, percentage, y){
  indexes <- sort(scores, decreasing=TRUE, index.return=TRUE)$ix
  indexes <- indexes[1:round(percentage*length(scores))]
  correct <- indexes %in%  which(y)
  sum(correct)/sum(y)
}

#' balanced classification rate (BCR)
#' The BCR computes the classification rate for each class and reports the arithmetic average of those rates over all classes
#' 
#' @param cm a confusion matrix  
#' @return the bcr
#' @export
BCR <- function(cm){
  (1/2)*(TP(cm)/P(cm)+TN(cm)/N(cm)) 
}


#' export matlab data to R file 
#' 
#' @param filename the name of the matlab file  
#' @param origin the folder of the file 
#' @param destination the destination folder   
#' @export
export_matlab <- function(filename ,origin ='data/dataPublic/' , destination='data/converted/'){
  matdata <- R.matlab::readMat(paste(origin, filename, sep = ""))
  x <- to_data_frame(matdata$X)
  y <- matdata$y == 1
  
  data <- list("x"=x, "y"=y)
  fileroot <- substr(filename, 1,(nchar(filename)-4))
  save(data, file=paste(destination, fileroot, ".Rda", sep=""))
}