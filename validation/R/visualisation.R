#' A function that returns a vector of colors going from start to end
#'
#' @param n number of points
#' @param start a color   
#' @param end a color  
#' @export
choose_color <- function(n = 10, start="green", end="red"){
  colfunc <- colorRampPalette(c(start, end) , alpha=TRUE)
  colfunc(n)
}

#' Plot a graph with outliers in red
#'
#' @param lor list of result of all functions
#' @param lensemle vector of result of the ensemble function  
#' @param max_n maximum number of points draw
#' @export
graph <- function(lor, lensemble,y = NULL,n = 10,percent = 0.2){
  out_node = c()
  fun_node = c()
  len = length(lensemble)
  for(name in c(names(lor), "ensemble_function")){
    out_node = c(out_node, 1:len)
    fun_node = c(fun_node, rep(name,len))
  }
  graph_data = igraph::graph_from_data_frame(data.frame(out_node, fun_node), directed = TRUE)
  idx_outliers <- which(lensemble>2*mean(lensemble))
  idx_normal <- which(lensemble<=2*mean(lensemble))
  color_vec_outliers <- validation::choose_color(length(idx_outliers),  start = "blue", end = "red")
  color_vec_normal <- validation::choose_color(length(idx_normal),  start = "cyan", end = "blue")
  color_vec<- rep(rgb(0.7,0.7,0.7), len+length(lor)+1)
  color_vec[sort(lensemble, index.return=TRUE, decreasing=FALSE)$ix] <- c(color_vec_normal, color_vec_outliers)
  vsize = rep(15,len+length(lor)+1)
  if(!is.null(y)){
    i=0
    edges_vec = c()
    lor$ensemble_function = lensemble
    for(names in names(lor)){
      r = lor[[names]]
      sort_r = sort(r, decreasing = TRUE)
      edges_to_remove = i*len+which(r<=sort_r[as.integer(len*percent)])
      edges_vec = c(edges_vec, edges_to_remove)
      i=i+1
    }
    graph_data = igraph::delete.edges(graph_data,edges_vec)
    high_score = which(lensemble > sort(lensemble, decreasing = TRUE)[n])
    out = which(y)
    vsize[out] = 25
    not_out = (1:len)[-union(high_score,out)]
    graph_data = igraph::delete.vertices(graph_data,not_out)
    color_vec = color_vec[-c(not_out)]
    vsize = vsize[-c(not_out)]
  }
  igraph::tkplot(graph_data, vertex.color= color_vec, vertex.size = vsize)
}


#' generate pdf containing a ROC curve  for specified dataset for each method in v_of_techniques  
#'
#' @param original_dataset_name  original name of the dataset
#' @param dataset_name name of the dataset
#' @param v_of_techniques vector containing the different detection techniques 
#' @param path The path containing the results fom the different detection methods
#' @param origin The path containing the dataset 
#' @export
roc_display <- function(original_dataset_name, dataset_name, v_of_techniques, path="data/result/", origin="data/converted/"){
  load(paste(origin, original_dataset_name, ".Rda", sep=""))
  y = data$y
  result_list = rda_to_list(original_dataset_name, v_of_techniques, path)
  for(name in names(result_list)){
    print(name)
    pdf(paste(path, dataset_name, "_ROC_",name, ".pdf", sep=""))
    roc_curve <- pROC::roc(as.vector(y), result_list[[name]],auc=TRUE, direction="<")
    plot(roc_curve, lwd=1, main=paste("ROC curve",name, dataset_name), print.auc=TRUE)
    dev.off()
  }
}

#' generate pdf containing a boxplot for specified dataset for each method in v_of_techniques  
#'
#' @param original_dataset_name  original name of the dataset
#' @param dataset_name name of the dataset
#' @param v_of_techniques vector containing the different detection techniques 
#' @param path The path containing the results fom the different detection methods
#' @param origin The path containing the dataset 
#' @export
boxplot_display <- function(original_dataset_name, dataset_name, v_of_techniques, path="data/result/", origin="data/converted/"){
  load(paste(origin, original_dataset_name, ".Rda", sep=""))
  y = data$y
  result_list = rda_to_list(original_dataset_name, v_of_techniques, path)
  for(name in names(result_list)){
    pdf(paste(path, dataset_name, "_BoxPlot_",name, ".pdf", sep=""))
    scores <- result_list[[name]]
    plot_boxplot (scores, outliers= y, title=paste("Boxplot",name, dataset_name))
    dev.off()
  }
}

#' generate pdf containing a pca for specified dataset for each method in v_of_techniques  
#'
#' @param original_dataset_name  original name of the dataset
#' @param dataset_name name of the dataset
#' @param v_of_techniques vector containing the different detection techniques 
#' @param path The path containing the results fom the different detection methods
#' @param origin The path containing the dataset 
#' @param pca_method The method to use to compute the pca 
#' @export
pca_display <- function(original_dataset_name, dataset_name, v_of_techniques, path="data/result/", origin="data/converted/", pca_method=prcomp, preprocess_method=outliers::preprocessing){
  load(paste(origin, original_dataset_name, ".Rda", sep=""))
  x = preprocess_method(data$x)
  y = data$y
  result_list = rda_to_list(original_dataset_name, v_of_techniques, path)
  x <- predict(caret::preProcess(x,  method = c("center", "scale")),x)
  pca <- pca_method(x)
  cvar <- pca$sdev^2/sum(pca$sdev^2)
  for(name in names(result_list)){
    pdf(paste(path, dataset_name, "_PCA_",name, ".pdf", sep=""))
    scores <- result_list[[name]]
    projected <- predict(pca, x)
    validation::plot_pca(x, scores, pca, y, title=paste(name, dataset_name))
    dev.off()
  }
}

#' Plot a pca with detected outliers in red
#'
#' @param data normalized data 
#' @param scores the scores to plot   
#' @param pca the pca projection
#' @param y the outliers
#' @param out_shape the shape to use to represent outliers
#' @param normal_shape the shape to use to represent normal points
#' @param nsd the number of standard deviation to be considered as outliers
#' @export
plot_pca <- function(data, scores, pca=prcomp(data), y=rep(FALSE, length(scores)), out_shape=17, normal_shape=19, nsd=4, title="" ){
  projected = predict(pca, data)

  
  color_rank <- rep(1, length(scores))
  outliers <- c(length(scores)+1)
  while(!all(which(scores > mean(scores[-outliers])+sd(scores[-outliers])*nsd) %in% outliers)){
    # find the newly detected outliers 
    outliers <- which(scores > mean(scores[-outliers])+sd(scores[-outliers])*nsd)
    # give a rank to the detected outliers
    color_rank[outliers]<-color_rank[outliers]  +1 
  }
  
  
  color_vec <- validation::choose_color(max(color_rank),  start = rgb(0, 0, 1, .2) , end = rgb(1, 0, 0, .9))
  out_color_vec <- validation::choose_color(max(color_rank),  start = rgb(0, 0, 1, 1) , end = rgb(1, 0, 0, 1))
  color_vec <- color_vec[color_rank] # mouais, ça donne la fausse impression qu'il y a beaucoup d'outliers détectés 
  out_color_vec <- out_color_vec[color_rank] 
  color_vec[y] <- out_color_vec[y]
  
  # color_vec <- validation::choose_color(11,  start = rgb(0, 0, 1, .1) , end = rgb(1, 0, 0, .9))
  # index <- scores - min(scores)
  # index <- index/max(index)
  # index <- round(index,1)*10 +1
  # color_vec <- color_vec[index]

  shape <- rep(normal_shape, length(scores))
  shape[y] <- out_shape
  cvar <- pca$sdev^2/sum(pca$sdev^2)
  max_dist <- max(abs(projected))
  plot(projected, col=color_vec, pch=shape, main=paste("PCA -", title, round(cvar[1]+cvar[2],2)*100,"%"), xlim=c(-max_dist, max_dist), ylim=c(-max_dist, max_dist))
}

#' Plot a boxplot with detected outliers in red. return the index of the points outside of the wiskers 
#'
#' @param scores the scores to plot   
#' @param nsd the number of standard deviation to be considered as outliers
#' @param threshold the max score for normal point
#' @export
plot_boxplot <- function(scores, outliers= NULL, nsd=5, threshold=mean(scores)+nsd*sd(scores), show_names=TRUE, title=""){
  # build the boxplot from scratch. data in red is at mean + nsd  * sd.  
  B <- boxplot(scores, outline=FALSE,  ylim=c(min(scores),max(scores)), xlim=c(.5,1.5) , 	ylab="Scores", main=title)
  v_col <- rep("black", length(B$out))

  for(i in 1:length(scores)){
    if(!is.null(outliers) && !(i %in% which(outliers))){
      segments(x0=1.48, x1=1.5, y0=scores[i], col="black")
    }
  }
  for(i in 1:length(scores)){
    if(!is.null(outliers) && (i %in% which(outliers))){
      segments(x0=1.48, x1=1.5, y0=scores[i], col="red")
    }
  }

  points(B$group, B$out, col=v_col)
  prev = 0 
  labels = NULL
  pos = NULL 
  s   <- max(scores)/40
  o <- sort(unique(B$out), decreasing=FALSE) 
  if(show_names && length(o)>0){
    for(i in 1:length(o)){
      if(o[i]>prev+s){
        if(!is.null(pos)){
          pos[length(pos)] <- (pos[length(pos)] +last )/2
        }
        labels <- c(labels, paste(which(scores == o[i]), sep=", " , collapse=", "))
        pos <- c(pos, o[i])
        prev <- o[i]
      }else{
        labels[length(labels)] <- paste(labels[length(labels)], which(scores == o[i]), sep=", ", collapse = ", ")         
      }
      last <- o[i]
    }
    labels <- sapply(labels, function(x){
      m <- 5
      l <- unlist(strsplit(x, ", "))
      if(length(l)>m){
        paste(c(l[1:m], "..."), sep = ", ", collapse = ", ")
      }else{
        x
      }
    })
    if(length(labels)>0)
    text(rep(1, length(labels)),pos, labels=labels , cex= .8, pos=4)
  }
  if(sd(scores)>0){
    for(i in 0:round(max(scores)/sd(scores))){
      l <-mean(scores)+i*sd(scores)
      if(l != threshold){
        abline(h =l , lwd=.5 , lty = 3)
      }
      text(x = .5, y=l, pos=3, labels = i, cex=.8, col="grey")
    }
    abline(a = threshold , b=0 ,col="red", lwd=.5 , lty = 1)
  }
  which(scores %in% B$out)
}