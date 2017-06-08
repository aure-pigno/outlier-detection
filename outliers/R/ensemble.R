#' Compute the score of each points based on the score it made it every functions
#'
#' @param lor list of list of results named "probability_vector"
#' @param v_char vector containing the name of the functions 
#' @param draw boolean to print and draw
#' @export
ensemble_function <- function(lor, fun = mean, nrot = 50, val, dist_funct = TRUE){
  v_char = names(lor)
  len = length(lor[[v_char[1]]])
  n_techniques = length(v_char) 
  weight = rep(1/n_techniques, n_techniques)
  names(weight) = v_char
  old_weight = weight - 1/n_techniques
  tech = commun_point
  if(dist_funct){
    tech = order_difference
  }
  while(FALSE %in% (weight == old_weight) && nrot > 0){
    nrot = nrot-1
    old_weight = weight
    weight_o = sapply(1:len, function(x){fun(sapply(v_char, function(y){ weight[y]*(lor[[y]])[x]}))})
    weight = sapply(v_char, function(name){tech(lor[[name]],weight_o, val)})^-1
    weight = weight/sum(weight)
    weight = sapply(weight, function(x) min(x, 3/(2*n_techniques)))
    index = which(weight==3/(2*n_techniques))
    if(length(index) > 0) weight[-c(index)] = weight[-c(index)]/sum(weight[-c(index)])
    weight = weight/sum(weight)
  }
  weight_o = sapply(1:len, function(x){fun(sapply(v_char, function(y){ weight[y]*(lor[[y]])[x]}))})
  score = weight_o/sum(weight_o)
  score
}

order_difference <- function(list1, list2, percent = 0.4){
  sortedlist1 = sort(list1,decreasing = TRUE)
  sortedlist2 = sort(list2,decreasing = TRUE)
  diff = 0
  for(i in 1:length(list1)){
    i1 = min(which(sortedlist1 == list1[i]))
    i2 = min(which(sortedlist2 == list2[i]))
    if(i2 < as.integer(length(list1)*percent)) diff = diff +abs(i1-i2)
  }
  diff+1

}

commun_point <- function(list1, list2, percent = 0.1){
  n = as.integer(length(list1)*percent)
  names(list1) = 1:length(list1)
  names(list2) = 1:length(list2)
  sortedlist1 = sort(list1,decreasing = TRUE)[1:n]
  sortedlist2 = sort(list2,decreasing = TRUE)[1:n]
  r = 1-(length(intersect(names(sortedlist1), names(sortedlist2)))-1)/n
  r
}

#' Compute the base detector greedy algorithm
#'
#' @param lor list of list of results named "probability_vector"
#' @param t threshold
#' @export
greedy_model_selection <- function(lor, t = 0.1){
  dec = TRUE
  v = rep(1,length(lor[[1]]))
  n = ceiling(t*length(lor[[1]]))+1
  while(sum(v)>length(v)/2-1){
    K = c()
    n=n-1
    v = rep(0,length(lor[[1]]))
    for(r in lor){
      sorted = sort(r, decreasing = TRUE, index.return = TRUE)$ix
      K = c(K, sorted[1:n])
    }
    K = unique(K)
    v[K]=1
  }
  E = list()
  sorted_names = correlation_sorted(lor,v, !dec)
  E[[sorted_names[1]]] = lor[[sorted_names[1]]]
  lor[[sorted_names[1]]] = NULL
  p = compute_p(E,n)
  sorted_names = correlation_sorted(lor,p, dec)
  while(length(lor) != 0){
    temp = lor[[sorted_names[1]]]
    lor[[sorted_names[1]]] = NULL
    
    old_r = Reduce("+",E)/length(E)
    new_r = (Reduce("+",E)+temp)/(length(E)+1)
    
    old_w= rep(1/(2*(length(old_r)-sum(v))),length(old_r))
    new_w= rep(1/(2*(length(new_r)-sum(v))),length(new_r))
    
    old_sorted = sort(old_r, decreasing = TRUE, index.return = TRUE)$ix
    new_sorted = sort(old_r, decreasing = TRUE, index.return = TRUE)$ix
    
    old_w[old_sorted]=1/(2*sum(v))
    new_w[new_sorted]=1/(2*sum(v))
    if(weights::wtd.cors(x=new_r,y=v,weight=new_w) > weights::wtd.cors(x=old_r,y=v,weight=old_w)){
      E[[sorted_names[1]]] = temp
      p = compute_p(E,n)
    }
    sorted_names = correlation_sorted(lor,p, dec)
  }
  #print(names(E))
  E
}

compute_p <- function(E,n){
  total_list = Reduce("+",E)/length(E)
  sorted = sort(total_list, decreasing = TRUE, index.return = TRUE)$ix
  p = rep(0,length(total_list))
  p[sorted[1:n]] = 1
  p
}

correlation_sorted <- function(lor,v, decreasing = TRUE){
  namesl = names(lor)
  weighted_correlation_vector = rep(0,length(lor))
  names(weighted_correlation_vector) = namesl
  for(name in namesl){
    r = lor[[name]]
    w= rep(1/(2*(length(r)-sum(v))),length(r))
    sorted = sort(r, decreasing = TRUE, index.return = TRUE)$ix
    w[sorted]=1/(2*sum(v))
    weighted_correlation_vector[name] = weights::wtd.cors(x=r,y=v,weight=w)
    
  }
  sorted_index = sort(weighted_correlation_vector, decreasing = decreasing, index.return = TRUE)$ix
  namesl[sorted_index]
}


