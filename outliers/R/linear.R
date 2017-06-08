# compute a basic pca 
basic_pca <- function(x){
  # Next we use the prcomp method to find PCAs
  pr <- prcomp(x, center = TRUE, scale. = TRUE)
}

#' compute  an outlier score based on the distance from a pca 
#' PCA from http://shahramabyari.com/2015/12/20/data-preparation-for-predictive-modeling-centering-scaling/
#' 
#' @param data a data frame containing only numerical features 
#' @export
pca_score <- function(x, pca=basic_pca){

  pr <- pca(na.omit(x))

  # Next we calculate centroid of data and save it in vector mu
  transformedData <- predict(caret::preProcess(x,method = c("center","scale"), na.remove = TRUE),x)
  mu <- colMeans(transformedData)
  
  #Next We calculate distances from centroid
  distFromMu <- sweep(transformedData,2,mu,'-')
  distFromMu <- abs(distFromMu)
  
  #Then we calculate variance of each PCA
  lam <- apply(pr$x,2,var)
  
  # Next we multiply distance with eigenvectors
  nominator <- (as.matrix(distFromMu)%*%as.matrix(pr$rotation))**2
  
  # Then we devide the result with variances
  Res <- sweep(nominator,2,lam,'/')
  
  #Calculate the sum of each row
  scores <- rowSums(Res)
  
  as.vector(scores/sum(scores))
}

#' compute  an outlier score based on the distance from the support vector of a one-class SVM 
#' 
#' @param data a data frame containing only numerical features 
#' @export
one_class_svm <- function(x, svm.nu=.10){
  oneclass_svm <- e1071::svm(x=x, type="one-classification", nu=svm.nu)
  scores <- -oneclass_svm$decision.values
  scores <- scores - min(scores)
  as.vector(scores/sum(scores))
}