#' A function filtering out samples having a variance smaller than min_var
#'
#  returns a dataframe with the low variance samples filtered out  
#' @param data the original dataframe  
#' @param min_var the minimal accepted variance. default to 0 as constant features does not impact outliers. 
#' @keywords variance, filter 
#' @export
remove_low_var_feature <-function(data, min_var=0){
  for(feature_name in colnames(data)){
    vari = variance(data[,feature_name]) 
    # if the variance is not null (if the data is numéric) and if it's variance is under min_var, remove it  
    if(!is.null(vari) && vari <= min_var) data[,feature_name] <- NULL
  } 
  data
}

#' add a timeout 
#'
#  returns a the function result or NA on timeout   
#' @param fun the function to call 
#' @param args the function arguments
#' @export
timeout <- function(fun, args, timeout=60*60){
  tryCatch(
    {R.utils::evalWithTimeout(do.call(fun, args), timeout = timeout, onTimeout = "error")}, 
    error=function(e){
      NA
    }
  )
}

#' A function to compute the variance of the given feature data
#'
#' @param feature_data a vector of values for a feature   
#' @keywords variance
#' @export
variance <- function(feature_data) {
  if(is.numeric(feature_data))
    var(feature_data, na.rm = TRUE)
  else NULL
}

#' a function to convert dataset to dataframes and to add row/column names if needed
#'
#  returns a dataframe with row and column names  
#' @param data the original dataset 
#' @export
to_data_frame<- function(d, force=FALSE){
  if(class(d)!="data.frame" | force){
    d <- data.frame(d)
    colnames(d) <- paste("f", 1:ncol(d), sep = "")
  }
  rownames(d) <- paste("x", 1:nrow(d), sep = "")
  d
}

#' A function to convert categorical variables into booleans 
#'
#' returns a dataframe with the categorical data replaced by booleans 
#' @param data the original dataset 
#' @export
cat2bool <- function(data){
  names <- colnames(data)
  for(name in names){
    if(class(data[,name])=="factor" || class(data[,name])=="character"){
      new_features <- psych::dummy.code(data[,name])
      data[,name] <- NULL
      for(new_name in names(new_features)){
        data[,new_name] <-new_features[,new_name]
      }
    }
  }
  data 
}

#' Preprocess the data, transform class into boolean, remove low variances, and apply asinh
#' 
#' @param data dataset
#' @export
preprocessing <- function(data, preprocess_method="boxcox",pca=FALSE){
  data <- cat2bool(to_data_frame(data))
  if(preprocess_method == "boxcox"){
    predictor <- caret::preProcess(data, method = c("BoxCox"))
    data  <- predict(predictor, data)
  }
  else if(preprocess_method == "asinh"){
    asinh(data)
  }
  else if(preprocess_method == "local_asinh"){
    predictor <- caret::preProcess(data, method = c("BoxCox"))
    data_t  <- predict(predictor, data)
    for(col in 1:ncol(data)){
      if(FALSE %in% (data_t[,col] == data[,col])){
        data[,col] = asinh(data[,col])
      }
    }
  }
  for(name in colnames(data)){
    if(NA %in% data[,name])
      data[which(is.na(data[,name])),name] = mean(data[,name], na.rm = TRUE)
  }
  
  d = remove_low_var_feature(data)
  if(pca){
    pca = outliers:::basic_pca(na.omit(d))
    data.frame(scale(na.omit(d), pca$center, pca$scale))
  }
  else{
    data.frame(d)
  }
}


#' do feature_bagging
#'
#  returns a bagging matrix   
#' @param d the original dataset 
#' @param bagging the bagging method 
#' @export
feature_bagging <- function(data, bagging = "mi", factor = 0.5){
  feature_matrix = matrix(0, ncol = min(factor*nrow(data), ncol(data)), nrow = ceiling(ncol(data)/(factor*nrow(data))))
  if(bagging == "mi"){
    mutinfo = mpmi::cmi(data)$mi
    rownames(mutinfo) = 1:nrow(mutinfo)
    colnames(mutinfo) = 1:ncol(mutinfo)
    i = 1
    while(!is.null(ncol(mutinfo)) && ncol(mutinfo) >= nrow(data)){
      fname =names(mutinfo[1, mutinfo[1,] >= mutinfo[1,order(mutinfo[1,], decreasing=TRUE)[nrow(data)]]])
      vec = which(colnames(mutinfo) %in% fname)
      cn = colnames(mutinfo)[!colnames(mutinfo) %in% fname]
      mutinfo = mutinfo[-vec, -vec]
      feature_matrix[i,] = fname
      i = i+1
    }
    if(length(mutinfo) > 1){
      feature_matrix[i,] = c(colnames(mutinfo),sample(1:ncol(data),ncol(feature_matrix)-ncol(mutinfo))) 
    } 
    if(length(mutinfo) == 1){
      feature_matrix[i,] = c(cn,sample(1:ncol(data),ncol(feature_matrix)-1)) 
    } 
    nr = nrow(feature_matrix)
    nc = ncol(feature_matrix)
    feature_matrix = matrix(data=mapply(feature_matrix, FUN=as.numeric), ncol=nc, nrow=nr)
    feature_matrix
  }
  if(bagging=="rd"){
    len = length(feature_matrix)
    vec1 = sample(1:ncol(data))
    vec2 = sample(1:ncol(data), len-length(vec1))
    feature_matrix[,] = c(vec1,vec2)
    feature_matrix
  } else{
    matrix(colnames(data), nrow=1, ncol = ncol(data))
  }
}