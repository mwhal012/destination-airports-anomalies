setwd("C:/Users/jaybh/Documents/R Files/Data")
library(dbscan)
library(tidyverse)

scaled_Airports <- read.csv("scaled_airports.csv")
Scaled_Airports <- as.matrix(scaled_Airports[, c(5,6,7,8,9,10,11,13)])
kNNdistplot(Scaled_Airports, minPts = 3)
abline(h=3.5, col = "red",lty = 2)

db <- dbscan(Scaled_Airports, eps =3, minPts = 3)
db
pairs(Scaled_Airports, col = db$cluster + 1L)
hullplot(Scaled_Airports, db, main = "DBSCAN")

Scaled_Airports <- cbind(Scaled_Airports, db$cluster)

combs = map(
  .x = seq_len(length.out = 8),
  .f = \(elts) combn(x = seq_len(length.out = 8), m = elts) |>
    as_tibble() # list-like structure for input into purrr::map()
)

db = map(
  .x = combs,
  .f = \(frame) map(
    .x = frame,
    .f = function(col) {
      airports = Scaled_Airports[, col] |>
        as.matrix()
      m = length(col) # number of variables
      dbscan(
        x = as.matrix(Scaled_Airports[, col]),
        eps = 3,
        minPts = floor(m) + 2
      )
    }
  )
)
db


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

