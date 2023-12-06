setwd("C:/Users/jaybh/Documents/R Files/Data")
library(dbscan)

Receiving_Airports <- read.csv("receiving_airports_pc.csv")
kNNdistplot(Receiving_Airports, minPts = 5)
abline(h=15, col = "red",lty = 2)

Scaled_Airports <- read.csv("scaled_airports.csv")
Scaled_Airports <- as.matrix(Scaled_Airports[, c(5,6,7,8,9,10,11,13)])
kNNdistplot(Scaled_Airports, minPts = 3)
abline(h=3.5, col = "red",lty = 2)

db <- dbscan(Scaled_Airports, eps = 3, minPts = 3)
db
pairs(Scaled_Airports, col = db$cluster + 1L)
hullplot(Scaled_Airports, db, main = "DBSCAN")

hdb <- hdbscan(Scaled_Airports, minPts = 3)
hdb
pairs(Scaled_Airports, col = hdb$cluster + 1L)
hullplot(Scaled_Airports, hdb, main = "HDBSCAN")



## Test DBSCAN ##
data("iris")
iris <- as.matrix(iris[, 1:4])
kNNdistplot(iris, minPts = 5)
abline(h=.7, col = "red", lty = 2)
res <- dbscan(iris, eps = .7, minPts = 5)
res
pairs(iris, col = res$cluster + 1L)
## Test DBSCAN ##

## Test HDBSCAN ##
data("iris")
iris <- as.matrix(iris[, 1:4])
kNNdistplot(iris, minPts = 5)
abline(h=.7, col = "red", lty = 2)
hres <- hdbscan(iris, minPts = 5)
hres
pairs(iris, col = hres$cluster + 1L)
## Test HDBSCAN ##

