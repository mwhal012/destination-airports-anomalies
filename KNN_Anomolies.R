library(tidyverse)
library(ggplot2)
library(conflicted)
library(ggplot2)
"dplyr" |>
  conflict_prefer_all(quiet = TRUE)

dataPCs = "scaled_airports.csv" |>
  read_csv()

euclidean_distance = function(a, b){
  #  We check that they have the same number of observation
  if(length(a) == length(b)){
    sqrt(sum((a-b)^2))  
  } else{
    stop()
  }
}

manhattan_distance = function(a,b){
  if(length(a) == length(b)){
    abs(sum(a-b))  d
  } else{
    stop()
  }
}


nearest_neighbors = function(x,obs, k, FUN, p = NULL){
  
  # Check the number of observations is the same
  if(ncol(x) != ncol(obs)){
    stop('Data must have the same number of variables')
  }
  
  # Calculate distance, considering p for Minkowski
  if(is.null(p)){
    dist = apply(x,1, FUN,obs)  
  }else{
    dist = apply(x,1, FUN,obs,p)
  }
  
  # Find closest neighbours
  distances = sort(dist)[1:k]
  neighbor_ind = which(dist %in% sort(dist)[1:k])
  
  if(length(neighbor_ind)!= k){
    warning(
      paste('Several variables with equal distance. Used k:',length(neighbor_ind))
    )
  }
  
  ret = list(neighbor_ind, distances)
  return(ret)
}

x = dataPCs
obs = dataPCs

eucledian_KNN <- vector("list", length = 346)

for(i in 1:346){
eucledian_KNN[[i]] = nearest_neighbors(x[-i,5:14],obs[i,5:14],10,euclidean_distance)
}

distanceMeanEuc <- vector("list", length = 346)

for (j in 1:346) {
  meanDisToN <- mean(eucledian_KNN[[j]][[2]])
  distanceMeanEuc[j] <- list(index1 = meanDisToN)
}

manhattan_KNN <- vector("list", length = 346)

for(i in 1:346){
  manhattan_KNN[[i]] = nearest_neighbors(x[-i,5:14],obs[i,5:14],10, manhattan_distance)
}

distanceMeanMan <- vector("list", length = 346)

for (j in 1:346) {
  meanDisToN <- mean(manhattan_KNN[[j]][[2]])
  distanceMeanMan[j] <- list(index1 = meanDisToN)
}
plotEucData <- data.frame(Index = 1:length(distanceMeanEuc), 
                          Value = unlist(distanceMeanEuc))

ggplot(plotEucData, aes(x=Index,y=Value))+
  geom_point() +
  geom_text(aes(label = Index), vjust = -0.5, hjust = 0.5, size = 3)

plotManData <- data.frame(Index = 1:length(distanceMeanMan), 
                        Value = unlist(distanceMeanMan))

ggplot(plotManData, aes(x=Index,y=Value))+
  geom_point()+
  geom_text(aes(label = Index), vjust = -0.5, hjust = 0.5, size = 3)

resultsEuc <- cbind(plotEucData, dataPCs)
resultsMan <- cbind(plotManData, dataPCs)