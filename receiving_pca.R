library(tidyverse)
library(conflicted)
"dplyr" |>
  conflict_prefer_all(quiet = TRUE)

data = "receiving_airports.csv" |>
  read_csv(
    col_types = cols(
      DEST_AIRPORT_SEQ_ID = col_factor(),
      busiest_day_of_week = col_factor(levels = c("1", "2", "3", "4", "5", "6", "7")),
      most_common_origin_apt = col_factor()
    )
  ) |>
  mutate(
    where(is.numeric) |>
      across(scale)
  )

dat_num = data |>
  select(where(is.numeric)) |>
  as.matrix()

pca_data = dat_num |>
  stats::prcomp()
summary(pca_data)

# DUDADS
vals = eigen(t(dat_num) %*% dat_num)$values
scree.y = vals / sum(vals)

barplot(
  scree.y,
  ylab = "% explained",
  xlab = "PC",
  col = heat.colors(12)
)
test = seq(0.7, 13.9, length.out = ncol(dat_num))
axis(1, at = test, labels = 1:ncol(dat_num)) # bend after 5th component

pca_data = pca_data$x[, 1:5] |>
  as_tibble() |>
  mutate(
    dest_airport_seq_ID = data$DEST_AIRPORT_SEQ_ID,
    dest_city_name = data$DEST_CITY_NAME,
    most_common_origin_apt = data$most_common_origin_apt,
    most_common_origin_city = data$most_common_origin_city,
    most_common_origin_state = data$most_common_origin_state,
    .before = PC1
  )
pca_data |>
  write_csv(file = "receiving_airports_pc.csv")
