library(conflicted)
library(tidyverse)
library(stats)
library(scales)
conflicts_prefer(
  dplyr::filter,
  dplyr::lag,
  readr::col_factor
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
  ) |>
  mutate(n_flights = n_flights_departed + n_flights_received)

outlier_high = \(var) var > quantile(var, probs = .75) + 1.50*IQR(var) # bool
outlier_low = \(var) var < quantile(var, probs = .25) - 1.50*IQR(var)

a0 = ggplot(data = airports) +
  geom_boxplot(
    mapping = aes(
    x = "",
    y = n_flights_departed
    ),
    outlier.shape = NA
  ) +
  geom_jitter(
    data = airports |>
      filter(outlier_high(n_flights_departed) == TRUE | outlier_low(n_flights_departed) == TRUE),
    mapping = aes(
    x = "",
    y = n_flights_departed,
    fill = prop_dest_del10,
    size = n_flights_departed
    ),
    shape = 23,
    stroke = 1,
    alpha = 0.6,
    position = position_jitter(width = 0.4, height = 0),
    show.legend = FALSE
  ) +
  scale_size(limits = c(0, 32000)) +
  scale_fill_viridis_c(
    name = "Prop.\nof Late\nIncoming\nFlights",
    limits = c(0, 0.5)
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    panel.background = element_blank()
  ) +
  scale_y_continuous(
    name = "Outgoing Flights",
    labels = label_number(scale_cut = cut_short_scale())
  )

plot = ggplot(data = airports) +
  geom_point(
    mapping = aes(
      x = mean_arrival_delay_mins,
      y = mean_departure_delay_mins,
      size = n_flights_departed,
      fill = prop_dest_del10
    ),
    shape = 23,
    stroke = 1,
    alpha = 0.6
  ) +
  scale_size(
    name = "Number of\nDeparting\nFlights",
    limits = c(0, 32000)
  ) +
  scale_fill_viridis_c(
    name = "Prop.\nof Late\nIncoming\nFlights",
    limits = c(0, 0.5)
  ) +
  theme(
    panel.background = element_blank()
  ) +
  xlab("Mean Arrival Delay (min.)") +
  ylab("Mean Departure Delay (min.)")

cowplot::plot_grid(a0, plot, ncol = 2, rel_widths = c(2, 5))
