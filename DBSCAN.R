library(tidyverse)
library(dbscan)

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

combs = map(
  .x = seq_len(length.out = 8),
  .f = \(elts) combn(x = seq_len(length.out = 8), m = elts) |>
    as_tibble() # list-like structure for input into purrr::map()
)

hdb = map(
  .x = combs,
  .f = \(frame) map(
    .x = frame,
    .f = \(col) hdbscan(x = as.matrix(Scaled_Airports[, col]), minPts = 3)
  )
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
        eps = 2*m,
        minPts = floor(m) + 2
      )
    }
  )
)

hdb[[1]]

hdb[[8]]
