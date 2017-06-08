#' Generates a dataset composed of normal correleted features with two classes. 
#' 
#' @param n the number of samples 
#' @param normal.q the percentile to consider as normal samples  
#' @param outliers.q the percentile to consider as outliers 
#' @param mu_a the means of the class a  
#' @param mu_b the means of the class b  
#' @param mu_global the means of the extra non significant variables. can be NULL  
#' @param cov_a covariance matrix of class a   
#' @param cov_b covariance matrix of class b   
#' @param cov_global covariance matrix of extra non significant variables. can be NULL    
#' @return list containing two sets, normal and outliers 
#' @export
gen_multi_class <- function(n, normal.q=0.90, outliers.q=0.01, mu_a=rep(0, nrow(cov_a)), mu_b=rep(0, nrow(cov_b)), mu_global=NULL, cov_a=diag(5), cov_b=diag(5), cov_global=diag(5)){
  cov_a <- as.matrix(cov_a)
  cov_b <- as.matrix(cov_b)
  data_a <- MASS::mvrnorm(n = n, mu = mu_a, Sigma = cov_a, empirical = FALSE)
  data_b <- MASS::mvrnorm(n = n, mu = mu_b, Sigma = cov_b, empirical = FALSE)

  d.local <- nrow(cov_a)
  if(!is.null(cov_global)){
    cov_global <- as.matrix(cov_global)
    mu_global <- rep(0, nrow(cov_global))      
    data_global <- MASS::mvrnorm(n = 2*n, mu = mu_global, Sigma = cov_global, empirical = FALSE)
      data_a <- cbind(data_a, data_global[1:n,])
      data_b <- cbind(data_b, data_global[(n+1):(2*n),])
      d <- nrow(cov_a)+nrow(cov_global)
  }else{
    d <- d.local 
  }
  cov_full_a <- matrix(0, nrow = d, ncol=d)
  cov_full_a[1:d.local,1:d.local] <- cov_a
  
  cov_full_b <- matrix(0, nrow = d, ncol=d)
  cov_full_b[1:d.local,1:d.local] <- cov_b

  if(!is.null(cov_global)){
    cov_full_a[(d.local+1):d,(d.local+1):d] <- cov_global
    cov_full_b[(d.local+1):d,(d.local+1):d] <- cov_global
  }
  mu_full_a <- c(mu_a, mu_global)
  mu_full_b <- c(mu_b, mu_global)

  data_a <- split_normal_outliers(data_a, normal.q, outliers.q, mu_full_a, cov_full_a)
  data_b <- split_normal_outliers(data_b, normal.q, outliers.q, mu_full_b, cov_full_b)
  list("normal_a"=data_a$normal, "normal_b"=data_b$normal, "outliers_a"=data_a$outliers, "outliers_b"=data_b$outliers)
}

#' Generates a dataset composed of normal correleted features with one classes. 
#' 
#' @param n the number of samples 
#' @param normal.q the percentile to consider as normal samples  
#' @param outliers.q the percentile to consider as outliers 
#' @param d number of features
#' @param mu the means 
#' @param sigma covariance matrix  
#' @return list containing two sets, normal and outliers 
#' @export
gen_normal <- function(n=10000, normal.q=0.90, outliers.q=0.01, d=50, mu=rep(0, d), sigma=diag(d)){
  data <- MASS::mvrnorm(n = n, mu = mu, Sigma = sigma, empirical = FALSE)
  split_normal_outliers(data, normal.q, outliers.q, mu, sigma)
}

#' helper method to separate normal points, outliers and noise in a normal correleted dataset 
#' 
#' @param data the dataset
#' @param normal.q the percentile to consider as normal samples  
#' @param outliers.q the percentile to consider as outliers 
#' @param mu the means 
#' @param sigma covariance matrix  
#' @return list containing three sets, normal noise and  outliers 
split_normal_outliers <- function(data, normal.q, outliers.q, mu, sigma){
  scores <- mahalanobis(data, mu, sigma)
  normal_val <- quantile(scores, normal.q)
  outlier_val <- quantile(scores, 1-outliers.q)
  
  outliers <- data[scores>outlier_val,]
  normal <- data[scores<normal_val,]
  noise <- data[scores>normal_val & scores < outlier_val]
  list("normal"=normal, "noise"=noise, "outliers"=outliers)
}

#' helper method to resample a data set containing 2 classes.   
#' 
#' @param data the dataset
#' @param nnormal number of normal points  
#' @param noutliers number of outliers 
#' @param d number of features
#' @param replace TRUE if the same sample can be picked multiple times 
#' @param balance specify the class balance. 
#' @return list containing: x the new dataset ( with an extra feature for the class), y a boolean vector indicating which sample is an outlier  
resample_multi <- function(data, nnormal=100, noutliers=ceiling(5*nnormal/100), d=ncol(data$normal), replace=FALSE,  balance=.5){
  nnormal.a <- ceiling(nnormal * balance)
  nnormal.b <- nnormal-nnormal.a
  noutliers.a <- ceiling(noutliers * balance)
  noutliers.b <- noutliers - noutliers.a
  data_a <- resample(list("normal"=data$normal_a, "outliers"=data$outliers_a), nnormal.a, noutliers.a)
  data_b <- resample(list("normal"=data$normal_b, "outliers"=data$outliers_b), nnormal.b, noutliers.b)

  x <- rbind(data_a$x, data_b$x)
  y <- c(data_a$y, data_b$y)
  x <- outliers::to_data_frame(x, force=TRUE)
  x$class <-c(rep("class.a", length(data_a$y)),rep("class.b", length(data_b$y))) 
  list("x"=x, "y"=y)
}

#' helper method to resample a data set containing 1 classe.   
#' 
#' @param data the dataset
#' @param nnormal number of normal points  
#' @param noutliers number of outliers 
#' @param d number of features
#' @param replace TRUE if the same sample can be picked multiple times 
#' @return list containing: x the new dataset, y a boolean vector indicating which sample is an outlier  
resample <- function(data, nnormal=100, noutliers=ceiling(5*nnormal/100), d=ncol(data$normal), replace=FALSE){
  normal <- data$normal[sample(1:nrow(data$normal), nnormal),]
  outliers <- data$outliers[sample(1:nrow(data$outliers), noutliers),]
  x <- rbind(normal, outliers) 
  x <- x[,sample(1:ncol(x), d)]
  shuffle <- sample(1:nrow(x), nrow(x))
  x <- x[shuffle,]
  y <- c(rep(FALSE, nnormal), rep(TRUE, noutliers))
  y <- y[shuffle]
  list("x"=x, "y"=y)
}