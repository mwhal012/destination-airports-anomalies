setwd("C:/Users/jaybh/Documents/R Files/Data")
library(dbscan)

Receiving_Airports <- read.csv("receiving_airports_pc.csv")
kNNdistplot(Receiving_Airports, minPts = 5)
abline(h=15, col = "red",lty = 2)

db <- dbscan(Receiving_Airports, eps = 10, minPts = 12)
db
pairs(Receiving_Airports, col = db$cluster + 1L)


## Test algorithm ##
data("iris")
iris <- as.matrix(iris[, 1:4])
kNNdistplot(iris, minPts = 5)
abline(h=.7, col = "red", lty = 2)
res <- dbscan(iris, eps = .7, minPts = 5)
res
pairs(iris, col = res$cluster + 1L)
## Test algorithm ##