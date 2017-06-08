#' Return a boolean matrix: evaluate the function func for each points of data
#'
#' @param data a data frame containing only numerical features 
#' @param func the function to evaluate for each point (must be boolean) 
func_feature <- function(data, func = function(x, min_value, max_value) {(x > max_value | x < min_value)}, distance=1.5){
  if(!all(sapply(data, is.numeric))){
    warning("This function can only handle numeric variables Non numeric variables have been removed")
    data = data[,sapply(data, is.numeric)]
  }
  q = sapply(data, function(x) quantile(x,c(0.25, 0.75), na.rm = TRUE, name=FALSE))
  IQR_value <- q[2,] - q[1,]
  min_value <- q[1,]-distance*IQR_value
  max_value <- q[2,]+distance*IQR_value
  eval <- function(x) func(x,min_value, max_value)
  # prevent the automatic conversion from matrix to vector performed by apply if there is only one feature
  if(ncol(data) == 1){
    eval(data)
  }else{
    t(apply(data,1, eval))
  }
}

#' Compute the relative distance between x and extreme value, 0 if x is in the interval
#'
#' @param x value of the point to check
#' @param min_value minimum value of the interval
#' @param max_value maximum value of the interval
distance <- function(x, min_value, max_value) {
  mapply(function(x, min_value, max_value){
    if(is.na(x))
      NA
    else
      if(x>max_value) {
        if(max_value == 0)  min(0.1,x-max_value)
        else abs((x-max_value)/max_value)
      }
    else if(x<min_value) {
      if(min_value == 0)  min(0.1,min_value-x)
      else abs((x-min_value)/min_value)
    }
    else 0}, x, min_value, max_value)
  
}


#' Return a vector with outlier scores based on the number of outlier feature and the median distance between extreme value
#'
#' @param data a data frame containing only numerical features 
#' @param func function of distance (univariate)
#' @export
box_plot_univariate <- function(data, func = distance) {
  bool_data = func_feature(data)
  distance_data = func_feature(data, func)
  vec =  sapply(rownames(bool_data), function(x) {
    out_sum = sum(bool_data[x,], na.rm = TRUE)
    dist = distance_data[x,]
    out_sum*sum(dist, na.rm = TRUE)/length(dist)
  })
  if(sum(vec) != 0) vec = vec/sum(vec)
  else vec = rep(1/length(vec), length(vec))
  as.vector(vec)
}

#' Extreme studentized deviate univariate method. 
#' code adapted from # from http://www.itl.nist.gov/div898/handbook/eda/section3/eda35h3.r
#' 
#' @param x a variable 
#' @param alpha alpha
#' @export
extreme_studentized_deviate <- function(x, alpha=0.05){

  # Compute the critical value for ESD Test
  esd.critical <- function(alpha, n, i) {
    p = 1 - alpha/(2*(n-i+1))
    t = qt(p,(n-i-1))
    return(t*(n-i) / sqrt((n-i-1+t**2)*(n-i+1)))
  }
  
  removeoutliers = function(x, r=length(x)/2) {
    ## Define values and vectors.
    x2 = x
    n = length(x)
    toremove = 0
    
    ## Compute test statistic until =10 values have been
    ## removed from the sample.
    for (i in 1:r){
      if(sd(x2)==0) break
      ares = abs(x2 - mean(x2))/sd(x2)
      Ri = max(ares)
      x2 = x2[ares!=Ri]
      
      ## Compute critical value.
      if(Ri>esd.critical(alpha,n,i))
        toremove = i
    }
    
    # Values to keep
    if(toremove>0)
      x = x[abs(x-mean(x)) < sort(abs(x-mean(x)),decreasing=TRUE)[toremove]]
    
    return (x)
  }

  clean <- removeoutliers(x)
  !(x %in% clean)
}