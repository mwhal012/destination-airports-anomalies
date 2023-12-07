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
      city_name = col_character(),
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
        score > 0.485,
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

outliers = scaled_airports |>
  mutate(n_flights_departed = airports$n_flights_departed) |>
  filter(score > 0.485)
outliers = outliers |>
  mutate(index = c(1:nrow(outliers)))

problem_cities = c("Chicago, IL", "Atlanta, GA", "Hagerstown, MD")
seed = 25100
pos = position_jitter(
  width = 0.3,
  height = 0,
  seed = seed
)
ggplot(data = outliers) +
  geom_point(
    mapping = aes(
      x = "",
      y = score,
      size = n_flights_departed,
      fill = score
    ),
    position = pos,
    shape = 22,
    stroke = 1,
    show.legend = c(size = TRUE, fill = FALSE)
  ) +
  scale_size(
    name = "Flights\nDeparted",
    range = c(1, 15)
  ) +
  scale_fill_viridis_c(direction = -1, begin = 0.2) +
  geom_text(
    mapping = aes(
      x = "",
      y = score,
      label = city_name,
      vjust = case_when(
        !str_starts(city_name, pattern = "Hagerstown") &
          !str_starts(city_name, pattern = "Chicago") &
          !str_starts(city_name, pattern = "Atlanta") ~ -1,
        str_starts(city_name, pattern = "Chicago") ~ -2.65,
        str_starts(city_name, pattern = "Atlanta") ~ -3,
        str_starts(city_name, pattern = "Hagerstown") ~ 2
      ),
      hjust = if_else(
        str_starts(city_name, pattern = "Hagerstown"),
        true = 0.35,
        false = 0.5
      )
    ),
    position = pos,
    check_overlap = FALSE,
    size = 3
  ) +
  geom_hline(
    yintercept = 0.50,
    colour = "red",
    linetype = 3
  ) +
  scale_y_continuous(
    name = NULL,
    limits = c(0.48, 0.5575),
    expand = expansion(mult = c(0.025, 0), add = 0)
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.background = element_blank(),
    legend.key = element_blank(),
    legend.direction = "vertical",
    legend.position = "right",
    legend.title.align = 1,
    legend.box.spacing = unit(x = -10, units = "mm"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Isolation Forest Scores",
    subtitle = "Airports in January 2019"
  )
