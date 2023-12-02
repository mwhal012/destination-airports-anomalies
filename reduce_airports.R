library(conflicted)
library(tidyverse)
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

airports = airports |>
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = \(x) scale(x)[, 1]
    )
  )

airports |>
  write_csv(
    "full_airports.csv",
    na = "NA",
    append = FALSE,
    quote = "needed"
  )

airports = airports |>
  select(
    !c(
      n_flights_departed,
      n_flights_received,
      mean_departure_delay_mins,
      prop_dep_del10,
      prop_dep_del20,
      prop_dep_del30,
      prop_dep_early10,
      prop_dep_early20,
      median_arrival_delay_mins,
      mean_arrival_delay_mins,
      mean_net_delay_as_dest,
      prop_dest_del10,
      prop_dest_del20,
      prop_dest_early10,
      prop_dest_early20,
      prop_dest_early30,
      flights_departed_per_day_reporting,
      flights_received_per_day_reporting,
      mean_arr_on_busiest_dow
    )
  )

vif = function(X) { # DUDADS
  vif = rep(0, ncol(X))
  for(i in 1:ncol(X)) {
    expl=as.matrix(X[,-i])
    y=lm(X[[i]][1:nrow(X)] ~ expl)
    vif[i]=1/(1-summary(y)$r.squared)
  }
  return(vif)
}

num_airports = airports |>
  select(where(is.numeric))

# also DUDADS
vif_airports = num_airports |>
  vif() |>
  matrix(nrow = 1)

colnames(vif_airports) = colnames(num_airports)
rownames(vif_airports) = "VIF"
round(t(vif_airports), 2)
rm(vif, vif_airports)

airports |>
  write_csv(
    "scaled_airports.csv",
    na = "NA",
    append = FALSE,
    quote = "needed"
  )
