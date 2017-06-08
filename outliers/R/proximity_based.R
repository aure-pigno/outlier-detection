#' Return an outlier score computed by doing a pca + lof 
#'
#' @param data a data frame containing only numerical features 
#' @export
pca_lof <- function(data, pca_meth=basic_pca){
  pca = pca_meth(na.omit(data))
  pca = scale(na.omit(data), pca$center, pca$scale)
  nsub = nrow(pca)
  if(nsub<=20) k = c(round(nsub-1),round((nsub-1)/2))
  else k = c(10,20)
  result = ldbod::ldbod(pca, k = k, nsub = nsub, method = c("lof"))
  result = apply(result$lof,1,function(x) mean(x, na.rm = TRUE))
  
  result = result - min(result)
  result[which(!is.finite(result))] = -1
  result[which(result == -1)] = 3*max(result)
  result/sum(result)
}

#' Return an outlier score computed by hyerarchical clustering
#'
#' @param data a data frame containing only numerical features 
#' @param percent number of clusters 
#' @export
hclustering <- function(data,percent = 10, dist_meth="euclidean") {
  mydata <- scale(na.omit(data))
  #outliers
  boolean = TRUE
  outliers_vector = c()
  j = 0
  vec = rep(0, length(data[,1]))
  while(boolean) {
    boolean = FALSE
    n_clusters = as.integer(length(mydata[,1])/100*percent)+1
    d <- dist(mydata, method = dist_meth) # distance matrix
    fit <- hclust(d, method="ward.D") 
    groups <- cutree(fit, k=n_clusters)
    outliers = c()
    for(i in 1:n_clusters) {
      if(length(which(groups %in% c(i))) == 1) {
        boolean = TRUE
        index = which(groups %in% c(i))
        global_bias = length(which(outliers_vector < index))
        local_bias = length(which(outliers < index))
        outliers <- c(outliers, index)
        outliers_vector <- c(outliers_vector, index+global_bias-local_bias)
        vec[index+global_bias-local_bias] = max(0.5,1-0.05*j)
      }
    }
    if(!is.null(outliers)) {mydata <- mydata[-outliers, ]}
    j = j+1
  }
  
  ## Small group of outlier/probably outliers
  n_clusters = as.integer(length(mydata[,1])/100*percent)+1
  d <- dist(mydata, method = dist_meth)
  fit <- hclust(d, method="ward.D") 
  groups <- cutree(fit, k=n_clusters)
  for(i in 1:n_clusters) {
    index = which(groups %in% c(i))
    index_bias = c()
    for(j in index) {
      bias = length(which(outliers_vector <= j))
      while(bias != length(which(outliers_vector <= j+bias))){bias = bias+1}
      index_bias = c(index_bias, j+bias)
    }
    vec[index_bias] = 1/length(index_bias)
  }
  vec/sum(vec)
}

# 2 phases clustering

#' Compute a modified K-means
#'
#' @param data a data frame containing only numerical features 
#' @param n proportion of dataset use as cluster 0<n<1
#' @param max_clusters maximal proportion allowed
MKP <- function(data, n, max_clusters, dist_meth="euclidean"){
  #' Initialize n clusters at random, or take n points in the dataset
  init_clusters <- function(data, n){
    mapply(function(x, y) runif(n, x, y), sapply(data, min, na.rm = TRUE), sapply(data, max, na.rm = TRUE))
  }
  
  n = as.integer(length(data[,1])*n)
  max_clusters = as.integer(length(data[,1])*max_clusters)
  # Step 1
  clusters = init_clusters(data, n)
  old_clusters = clusters - 1
  maxiter = 20
  iter = 0
  #while (length(clusters) != length(old_clusters) || (FALSE %in% (old_clusters == clusters))) {
  while((length(clusters) != length(old_clusters) || sum(abs(old_clusters - clusters))>0.05*length(clusters)) && iter < maxiter   ) {
    iter = iter+1
    old_clusters = clusters
    # Step 2
    distance_matrix = dist(clusters, method = dist_meth)
    distance = fields::rdist(clusters, na.omit(data))
    if(min(distance) > min(distance_matrix)){ # Step 3
      # Step 4: Splitting process
      larger_length_index = which.max(sapply(data.frame(distance), min, na.rm = TRUE))
      clusters = rbind(clusters, data[larger_length_index,])
      # Step 5: Merging process
      len = length(data.matrix(clusters)[,1])
      while(len > max_clusters) {
        distance_matrix = dist(data.matrix(clusters), method = dist_meth)
        index1 = as.integer(which.min(distance_matrix)/(len-1))+1
        index2 = (which.min(distance_matrix) %% (len-1))+1
        new_row = mapply(sum, clusters[index1,], clusters[index2,])
        clusters <- rbind(clusters[-c(index1, index2),], new_row/2)
        len = length(data.matrix(clusters)[,1])
      }
    }
    # Step 6
    distance = data.frame(fields::rdist(clusters, data))
    min_distance = apply(t(distance), 1, which.min)
    clusters = na.exclude(t(sapply(1:n, function(x) colSums(data[which(min_distance == x),])/length(which(min_distance == x)))))
  }
  clusters
}



#' Minimum spanning tree
#'
#' @param data a data frame containing only numerical features 
#' @param centroids a matrix containing centroids
#' @param percent maximalsize of a group to consider all this points as outliers
MST <- function(data, centroids, percent = 0.05, dist_meth="euclidean") {
  if(nrow(centroids) != 1){
    distance_matrix = dist(centroids, method = dist_meth, upper = TRUE)
    v = ape::mst(distance_matrix)
    rem = as.matrix(v)*as.matrix(distance_matrix) > 2*mean(as.matrix(v)*as.matrix(distance_matrix))
    v = v*(1-rem)
    connections = apply(v,1,sum)
    outliers_clusters = which(connections <= 1)
    distance = data.frame(fields::rdist(centroids, data))
    
    vec_na = which(apply(distance,2, function(x) NA %in% x))
    min_distance = apply(t(distance), 1, which.min)
    vec = c()
    for(i in outliers_clusters){
      index = sapply(which( rapply(list(min_distance), c) ==i), function(x){
        ori_len = length(vec_na[which(vec_na<x)])
        new_len = length(vec_na[which(vec_na<x+ ori_len)])
        x = x+ori_len
        while(ori_len != new_len){
          x = x-ori_len+new_len
          ori_len = length(vec_na[which(vec_na<x)])
          new_len = length(vec_na[which(vec_na<x+ ori_len)])
        }
        x
      })
      if(length(index) < percent*length(data[,1])){
        vec = c(vec, index)
      }
    }
    as.vector(vec)
  }
  else {c()}
}

#' Minimum spanning tree
#'
#' @param data a data frame containing only numerical features 
#' @param centroids a matrix containing centroids
#' @param percent maximalsize of a group to consider all this points as outliers
MST2 <- function(data, centroids, percent = 0.05, dist_meth="euclidean") {
  if(nrow(centroids) != 1){
    distance_matrix = dist(centroids, method = dist_meth, upper = TRUE)
    v = ape::mst(distance_matrix)
    len = length(v)
    v = as.matrix(v)
    distance_matrix = as.matrix(distance_matrix)
    rem = matrix(rep(FALSE, len), nrow = sqrt(len), ncol = sqrt(len))
    for(i in 1:nrow(v)){
      val = v[i,]*distance_matrix[i,] > 2*mean(v[i,]*distance_matrix[i,])
      rem[i,] = sapply(1:nrow(rem), function(x) rem[i,x] || val[x])
      rem[,i] = sapply(1:nrow(rem), function(x) rem[x,i] || val[x])
    }
    v = v*(1-rem)
    connections = apply(v,1,sum)
    outliers_clusters = which(connections <= 1)
    distance = data.frame(fields::rdist(centroids, data))
    
    vec_na = which(apply(distance,2, function(x) NA %in% x))
    min_distance = apply(t(distance), 1, which.min)
    vec = c()
    for(i in outliers_clusters){
      index = sapply(which( rapply(list(min_distance), c) ==i), function(x){
        ori_len = length(vec_na[which(vec_na<x)])
        new_len = length(vec_na[which(vec_na<x+ ori_len)])
        x = x+ori_len
        while(ori_len != new_len){
          x = x-ori_len+new_len
          ori_len = length(vec_na[which(vec_na<x)])
          new_len = length(vec_na[which(vec_na<x+ ori_len)])
        }
        x
      })
      if(length(index) < percent*length(data[,1])){
        vec = c(vec, index)
      }
    }
    as.vector(vec)
  }
  else {c()}
}



#' Return an outlier score based on the two phases clustering algorithm
#'
#' @param data a data frame containing only numerical features 
#' @param n number of repetion of the algorithm
#' @param base_p_cluster initial number of clusters (proportion of data)
#' @param max_p_cluster maximum number of clusters (proportion of data)
#' @export
two_phases_clustering <- function(data, n = 50, base_p_cluster = 0.1, max_p_clusters = 0.2){
  out = rep(0,length(data[,1]))
  for (i in 1:n) {
    #centroids = kmeans(data,as.integer(length(data[,1])*base_p_cluster))$centers
    centroids = MKP(data, base_p_cluster, max_p_clusters)
    outliers = MST2(data, centroids)
    if(length(outliers) != 0)
      out[outliers] = out[outliers]+1
  }
  if(sum(out) == 0) out = rep(1,length(data[,1]))
  vec = (out/n)
  vec/sum(vec)
}

# LOCI



#' Return an outlier score based on the Local Correlation Integral
#'
#' @param data a data frame containing only numerical features 
#' @param vector vector containing proportion of point in the neighboorhood of the point studied
#' @export
LOCI <- function(data, alpha = c(0.2, 0.5, 0.8), vector = seq(from = 0.2, to = 0.8, by = 0.3), only_mean = TRUE) {
  dist_mat = fields::rdist(data, data)
  # Density of data point "index" of y within distance eps
  #
  # index the index of the point examinated
  # eps distance around the point
  M <- function(index, eps){
    length(which(dist_mat[index,] < eps))
  }
  
  # Compute the average density of data point "index" as the mean of density of neighbours of "index" within distance del
  #
  # index the index of the point examinated
  # eps distance around the point
  # del distance around the other points 
  AM <- function(index,eps, del){
    nei = which(dist_mat[index,]< del)
    nei = nei[! nei %in% c(index)]
    sap = sapply(nei, function(x) M(x, eps))
    if(length(sap) > 0) mean(sap, na.rm = TRUE)
    else NA
    
  }
  
  # Compute Multi-Granularity Deviation Factor
  #
  # index the index of the point examinated
  # eps distance around the point
  # AM_value value of (x, del/2, del, dist_mat)
  MDEF <- function(index,eps, AM_value){
    1 - M(index,eps)/AM_value
  }
  
  # compute the (standart deviation of M) / AM
  #
  # index the index of the point examinated
  # eps distance around the point
  # del distance around the other points 
  teta <- function(index,eps, del, AM_value){
    nei = which(dist_mat[index,] < del)
    nei = nei[! nei %in% c(index)]
    if(!length(nei)) NA
    else {
      sd(sapply(nei, function(x) M(x, eps)), na.rm = TRUE)/AM_value
    }
  }
  result_matrix = matrix(0,ncol=nrow(data),nrow=length(alpha))
  i = 1
  for(alphai in alpha){
    test = t(sapply(vector, function(p){
      sapply(1:length(data[,1]), function(x) {
        sortlist = sort(fields::rdist(data[x,], data))
        del = sortlist[as.integer(length(data[,1])*p)]
        AM_value = AM(x, del*alphai, del)
        if(!is.na(AM_value)){
          MDEFactor = MDEF(x, del*alphai, AM_value)
          if(is.na(MDEFactor) || MDEFactor <= 0) 0
          else{
            teta_v = teta(x, del*alphai, del, AM_value)
            if(is.na(teta_v) || is.nan(teta_v) ) 0
            else if(teta_v == 0) MDEFactor
            else MDEFactor/teta_v}}
        else 0
      })
    }
    ))
    vec = apply(test,2, mean)
    vec = sapply(vec,function(x) max(0,x))
    if(sum(vec) == 0) vec = rep(1, length(vec))
    result_matrix[i,] = vec/sum(vec)
    i=i+1
  }
  vec = apply(result_matrix[1:(i-1),],2,mean)
  if(sum(vec) == 0) vec = rep(1, length(vec))
  if(only_mean)
   vec/sum(vec)
  else
    result_matrix
}