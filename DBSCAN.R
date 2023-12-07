library(tidyverse)
library(dbscan)

airports = read_csv("airports.csv") # just for reference to e.g. index
airports = airports |>
  mutate(index = c(1:nrow(airports)), .before = seq_ID)
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

alone = \(x) if_else(x == 0, true = 1, false = 0)

db = map(
  .x = combs,
  .f = function(frame) {
    y = map(
      .x = frame,
      .f = function(col) {
        airports = Scaled_Airports[, col] |>
          as.matrix()
        m = length(col) # number of variables
        dbscan(
          x = as.matrix(Scaled_Airports[, col]),
          eps = m / 2,
          minPts = floor(m / 2) + 10
        )$cluster |>
          alone()
      }
    )
    s = rep_len(0, length.out = nrow(Scaled_Airports))
    for(n in names(y)) s = s + y[[n]]
    return(list(y, s))
  }
)

total = rep_len(0, length.out = nrow(Scaled_Airports))
for(ind in 1:ncol(Scaled_Airports)) total = total + db[[ind]][[2]]
indices = which(total > 70)
airports |>
  slice(indices)
