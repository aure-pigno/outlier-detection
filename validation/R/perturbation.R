#' Return a dataset with the specified proportion of the sample corrupted by the function \code{fun}.  
#' 
#' @param data The original data set 
#' @param proportion The proportion of that data needs to be corrupted  
#' @param fun the corrupting function. takes a data set and a vector of boolean indicating which data is corrupted as argument and return a corrupted data set
#' @return a list containing: data the corrupted data set , is_modified the vector indicating which observation has been corrupted
#' @examples
#' introduce_perturbation(dataset1, .1, fun(data, is_modified){
#' data[is_modified, ]$f1  <- 0
#' data
#' } )
introduce_perturbation<- function (data, proportion, fun){
  # generate a vector of boolean , all set to false 
  is_modified = logical(length=nrow(data))
  # randomly modify nrow(data)* proportion of them to true 
  is_modified[sample(1:nrow(data), round(nrow(data) *proportion), replace=FALSE)] <-  TRUE
  # apply the fun function
  data <- fun(data, is_modified)
  # return the modified data frame and the vector of boolean indicating which classes are modified 
  list("data" = data, "is_modified" = is_modified) 
}


#' Return a dataset with the specified proportion of the sample corrupted with the specified \code{feature_proportion} modified with random noise between noise_min and noise_max
#' 
#' @param data The original data set 
#' @param sample_proportion The proportion of that data needs to be corrupted  
#' @param feature_proportion the proportion of the features that are affected
#' @param noise_min the minimum percentage of noise added to the features as a percentage of itself. the new value will be value + value*noise (with noise, a value between noise_min and noise_max)  
#' @param noise_max the maximal percentage of noise added to the features as a percentage of itself. the new value will be value + value*noise (with noise, a value between noise_min and noise_max)  
#' @return a list containing: data the corrupted data set, is_modified the vector indicating which observation has been corrupted
#' @export
#' @examples
#' introduce_value_increased_outliers(dataset1, proportion=.1, distance_min = .0, distance_max=.2, affected_features=.20)
introduce_value_increased_outliers <- function(data, sample_proportion=.1,  feature_proportion=.1, noise_min=.2, noise_max=.7){
  introduce_perturbation(data, sample_proportion, function(data, is_modified){
    new_data = data[,sapply(data, is.numeric)]
    new_data[is_modified,] <- t(apply(new_data[is_modified,],1, function(x){
      affected_features = sample(1:ncol(new_data), round(ncol(new_data) *feature_proportion), replace=FALSE)
      x[affected_features] <- x[affected_features] + x[affected_features]*runif(1, noise_min, noise_max)
      x
    }))
    data[,colnames(new_data)] <- new_data 
    data
  })
}



#' Randomly modify the class label of a \code{proportion} of observations from \code{data}
#' 
#' @param data The original dataset 
#' @param class The name of the class variable. class by default 
#' @param percentage The percentage of affected labels 
#' @return A modified copy of \code{data} containing \code{proportion} modified labels from \code{class} and vector indicating which indices have been modified
#' @export
#' @examples
#' change_class(data)
#' change_class(data, class="y")
#' change_class(data, proportion=.5)
change_class <- function(data, class= "class", proportion=.1){
  introduce_perturbation(data, proportion, function(data, is_modified) {
    # extract unique classes
    classes = unique(data[,class])
    # extract the class vector from data 
    new_classes = data[,class]
    # replace the class of the samples having is_modified TRUE by a random other class 
    new_classes[is_modified]<- sapply(new_classes[is_modified], function(n) sample(classes[classes != n], size=1))
    # set the class of data to the new modified classes 
    data[,class] <- new_classes
    data
  })
}