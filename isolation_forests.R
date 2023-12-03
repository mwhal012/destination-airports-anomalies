library(conflicted)
library(tidyverse)
library(isotree)
conflicts_prefer(
  dplyr::filter,
  dplyr::lag
)

US_states = c(
  "AL", # Alabama
  "AK", # Alaska
  "AZ", # Arizona
  "AR", # Arkansas
  "CA", # California
  "CO", # Colorado
  "CT", # Connecticut
  "DE", # Delaware
  "FL", # Florida
  "GA", # Georgia
  "HI", # Hawaii
  "ID", # Idaho
  "IL", # Illinois
  "IN", # Indiana
  "IA", # Iowa
  "KS", # Kansas
  "KY", # Kentucky
  "LA", # Louisiana
  "ME", # Maine
  "MD", # Maryland
  "MA", # Massachusetts
  "MI", # Michigan
  "MN", # Minnesota
  "MS", # Mississippi
  "MO", # Missouri
  "MT", # Montana
  "NE", # Nebraska
  "NV", # Nevada
  "NH", # New Hampshire
  "NJ", # New Jersey
  "NM", # New Mexico
  "NY", # New York
  "NC", # North Carolina
  "ND", # North Dakota
  "OH", # Ohio
  "OK", # Oklahoma
  "OR", # Oregon
  "PA", # Pennsylvania
  "RI", # Rhode Island
  "SC", # South Carolina
  "SD", # South Dakota
  "TN", # Tennessee
  "TX", # Texas
  "UT", # Utah
  "VT", # Vermont
  "VA", # Virginia
  "WA", # Washington
  "WV", # West Virginia
  "WI", # Wisconsin
  "WY" # Wyoming
)

sevendays = c("1", "2", "3", "4", "5", "6", "7")

scaled_airports = "scaled_airports.csv" |>
  read_csv(
    col_types = cols(
      seq_ID = col_factor(),
      city_name = col_character(),
      origin_state = col_factor(levels = US_states),
      DEST_STATE_ABR = col_factor(levels = US_states),
      busiest_day_of_week_dep = col_factor(levels = sevendays),
      busiest_day_of_week_arr = col_factor(levels = sevendays),
      most_common_dest_apt = col_factor(),
      most_common_dest_city = col_character(),
      most_common_origin_apt = col_factor(),
      most_common_origin_city = col_character(),
      most_common_dest_state = col_factor(levels = US_states),
      most_common_origin_state = col_factor(levels = US_states),
      .default = col_double()
    )
  ) |>
  select(!DEST_STATE_ABR) |>
  mutate(
    state = origin_state,
    .keep = "unused",
    .after = city_name
  )

airports = "airports.csv" |>
  read_csv(
    col_types = cols(
      seq_ID = col_factor(),
      origin_state = col_factor(levels = US_states),
      DEST_STATE_ABR = col_factor(levels = US_states),
      busiest_day_of_week_dep = col_factor(levels = sevendays),
      busiest_day_of_week_arr = col_factor(levels = sevendays),
      most_common_dest_apt = col_factor(),
      most_common_origin_apt = col_factor(),
      most_common_dest_state = col_factor(levels = US_states),
      most_common_origin_state = col_factor(levels = US_states)
    )
  )

forest = scaled_airports |>
  isolation.forest(
    ntrees = 10000,
    penalize_range = TRUE,
    output_score = TRUE,
    output_dist = TRUE
  )

scaled_airports = scaled_airports |>
  mutate(
    index = c(1:nrow(scaled_airports)),
    score = forest$scores
  )

ggplot(
  data = scaled_airports,
  mapping = aes(
      x = index,
      y = score,
      label = if_else(
        score > 0.475,
        true = city_name,
        false = ""
      )
    )
) +
  geom_point(
    mapping = aes(
      x = index,
      y = score,
      size = log(airports$n_flights_received)
    ),
    alpha = 0.9
  ) +
  geom_text(vjust = -0.6)

full_airports = "full_airports.csv" |>
  read_csv(
    col_types = cols(
      seq_ID = col_factor(),
      origin_state = col_factor(levels = US_states),
      DEST_STATE_ABR = col_factor(levels = US_states),
      busiest_day_of_week_dep = col_factor(levels = sevendays),
      busiest_day_of_week_arr = col_factor(levels = sevendays),
      most_common_dest_apt = col_factor(),
      most_common_origin_apt = col_factor(),
      most_common_dest_state = col_factor(levels = US_states),
      most_common_origin_state = col_factor(levels = US_states)
    )
  ) |>
  select(!DEST_STATE_ABR) |>
  mutate(
    state = origin_state,
    .keep = "unused",
    .after = city_name
  )

forest = full_airports |>
  isolation.forest(
    ntrees = 1000,
    penalize_range = TRUE,
    output_score = TRUE,
    output_dist = TRUE
  )

full_airports = full_airports |>
  mutate(
    index = c(1:nrow(full_airports)),
    score = forest$scores
  )

ggplot(
  data = full_airports,
  mapping = aes(
      x = index,
      y = score,
      label = if_else(
        score > 0.525,
        true = city_name,
        false = ""
      )
    )
) +
  geom_point() +
  geom_text(vjust = -0.5)
