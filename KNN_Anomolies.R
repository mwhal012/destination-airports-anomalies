library(tidyverse)
library(ggplot2)
library(conflicted)
library(ggplot2)
library(GGally)
"dplyr" |>
  conflict_prefer_all(quiet = TRUE)

dataPCs = "scaled_airports.csv" |>
  read_csv()

#Euc Dist Formula
euclidean_distance = function(a, b){
  if(length(a) == length(b)){
    sqrt(sum((a-b)^2))  
  } else{
    stop()
  }
}
#Man Dist Formula
manhattan_distance = function(a,b){
  if(length(a) == length(b)){
    abs(sum(a-b))  d
  } else{
    stop()
  }
}
#Manual KNN Implementation

nearest_neighbors = function(x,obs, k, FUN, p = NULL){
  
  if(ncol(x) != ncol(obs)){
    stop('Data must have the same number of variables')
  }
  
  if(is.null(p)){
    dist = apply(x,1, FUN,obs)  
  }else{
    dist = apply(x,1, FUN,obs,p)
  }
  
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

#Initialize the list that stores the NN for each Index
eucledian_KNN <- vector("list", length = 346)
manhattan_KNN <- vector("list", length = 346)

#KNN Euclidean
for(i in 1:346){
eucledian_KNN[[i]] = nearest_neighbors(x[-i,5:14],obs[i,5:14],10,euclidean_distance)
}
#KNN Manhattan
for(i in 1:346){
  manhattan_KNN[[i]] = nearest_neighbors(x[-i,5:14],obs[i,5:14],10, manhattan_distance)
}
#Initialize the list that stores the mean distance for each Index
distanceMeanEuc <- vector("list", length = 346)
distanceMeanMan <- vector("list", length = 346)

#Find mean dist to k neighbors
for (j in 1:346) {
  meanDisToN <- mean(eucledian_KNN[[j]][[2]])
  distanceMeanEuc[j] <- list(index1 = meanDisToN)
}
for (j in 1:346) {
  meanDisToN <- mean(manhattan_KNN[[j]][[2]])
  distanceMeanMan[j] <- list(index1 = meanDisToN)
}
#Convert lists back to dataframes to make plotting easier
plotEucData <- data.frame(Index = 1:length(distanceMeanEuc), 
                          Value = unlist(distanceMeanEuc))

plotManData <- data.frame(Index = 1:length(distanceMeanMan), 
                        Value = unlist(distanceMeanMan))

#Reattach distance to orignial 
resultsEuc <- cbind(plotEucData, dataPCs)
resultsMan <- cbind(plotManData, dataPCs)

#Find top 10 outliers
resultsEuc <- resultsEuc %>%
  mutate(Outlier = ifelse(rank(-Value) <= 10, "Outlier", "Non-Outlier"))
resultsMan <- resultsMan%>%
  mutate(Outlier = ifelse(rank(-Value) <= 10, "Outlier", "Non-Outlier"))

#Plot Distances
ggplot(plotManData, aes(x=Index,y=Value))geom_point(aes(color= Outlier)) +
  scale_color_manual(values = c("Outlier" = "red", "Non-Outlier" = "black"))+
  geom_text(data = subset(resultsEuc, Outlier == "Outlier"), aes(label = city_name),
            vjust = -0.5, hjust = 0.5, size = 3)

ggplot(resultsEuc, aes(x=Index,y=Value))+
  geom_point(aes(color= Outlier)) +
  scale_color_manual(values = c("Outlier" = "red", "Non-Outlier" = "black"))+
  geom_text(data = subset(resultsEuc, Outlier == "Outlier"), aes(label = city_name),
            vjust = -0.5, hjust = 0.5, size = 3)

#ggpairs on data set(I used the KNN_outliers for the sake of coloring)
ggpairs(resultsEuc, columns = c(7,8,10,11,12),
        upper = list(continuous = "points"),
        lower =  "blank",
        diag = list(continuous = "blankDiag"),
        aes(color = Outlier)
        ) + 
  scale_color_manual(values = c("Outlier" = "red", "Non-Outlier" = "black"))


