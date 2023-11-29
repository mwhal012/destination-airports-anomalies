library(tidyverse)
library(isotree)
library(conflicted)
"dplyr" |>
  conflict_prefer_all(quiet = TRUE)

pc = "receiving_airports_pc.csv" |>
  read_csv(
    col_types = cols(
      dest_airport_seq_ID = col_factor(),
      dest_city_name = col_factor(),
      most_common_origin_apt = col_factor(),
      most_common_origin_city = col_factor(),
      most_common_origin_state = col_factor(),
      .default = col_double()
    )
  )

forest = pc |>
  select(PC1:PC5) |>
  isolation.forest(
    ntrees = 1000,
    #scoring_metric = "adj_depth",
    penalize_range = TRUE,
    output_score = TRUE,
    output_dist = TRUE
  )

pred = forest$model |>
  predict(newdata = pc) |>
  as_tibble_col()
pred = pred |>
  mutate(
    index = c(1:nrow(pc)),
    dest_airport_seq_ID = pc$dest_airport_seq_ID,
    dest_city_name = pc$dest_city_name,
    most_common_origin_apt = pc$most_common_origin_apt,
    most_common_origin_city = pc$most_common_origin_city,
    most_common_origin_state = pc$most_common_origin_state,
    .before = value
  )

ggplot(
  data = pred,
  mapping = aes(
      x = index,
      y = value,
      label = if_else(
        value > 0.6,
        true = dest_city_name,
        false = ""
      )
    )
) +
  geom_point() +
  geom_text(vjust = -0.5)

forest = pc |>
  select(PC1:PC5) |>
  isolation.forest(
    ntrees = 1000,
    scoring_metric = "adj_depth",
    output_score = TRUE,
    output_dist = TRUE
  )

pred = forest$model |>
  predict(newdata = pc) |>
  as_tibble_col()
pred = pred |>
  mutate(
    index = c(1:nrow(pc)),
    dest_airport_seq_ID = pc$dest_airport_seq_ID,
    dest_city_name = pc$dest_city_name,
    most_common_origin_apt = pc$most_common_origin_apt,
    most_common_origin_city = pc$most_common_origin_city,
    most_common_origin_state = pc$most_common_origin_state,
    .before = value
  )

ggplot(
  data = pred,
  mapping = aes(
      x = index,
      y = value,
      label = if_else(
        value > 0.6,
        true = dest_city_name,
        false = ""
      )
    )
) +
  geom_point() +
  geom_text(vjust = -0.5)
