library(conflicted)
library(tidyverse)
library(stats) # stats::median
conflicts_prefer(
  dplyr::filter,
  dplyr::lag
)

"Flights1_2019_1.zip" |>
  unzip(overwrite = TRUE)

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

data = "Flights1_2019_1.csv" |>
  read_csv(
    col_types = cols(
      DAY_OF_WEEK = col_factor( # Monday = 1, Sunday = 7
        levels = c("1", "2", "3", "4", "5", "6", "7"),
        ordered = FALSE,
        include_na = FALSE
      ),
      FL_DATE = col_date(),
      DEST_STATE_ABR = col_factor(
        levels = US_states,
        ordered = FALSE,
        include_na = TRUE
      )
    ),
    col_select = !c(
      YEAR,
      ORIGIN_AIRPORT_ID,
      ORIGIN_CITY_MARKET_ID,
      DEST_AIRPORT_ID,
      DEST_CITY_MARKET_ID,
      ARR_DEL15,
      last_col()
    )
  )

data = data |>
  mutate(
    origin_state_str = ORIGIN_CITY_NAME |>
      str_extract(pattern = regex("[A-Z]{2}$")),
    origin_state = if_else(
      condition = origin_state_str %in% US_states,
      true = origin_state_str,
      false = NA
    ) |>
      factor(levels = US_states),
    .after = ORIGIN_CITY_NAME
  ) |>
  mutate(
    arrival_datetime = FL_DATE |>
      paste(ARR_TIME, sep = " ") |>
      fast_strptime(format = "%Y-%m-%d %H%M"),
    .after = ARR_TIME
  )

destination_airports = data |>
  summarise(
    n_flights_received = n(),
    mean_arrival_delay_mins = ARR_DELAY |>
      mean(na.rm = TRUE),
    median_arrival_delay_mins = ARR_DELAY |>
      median(na.rm = TRUE),
    sd_arrival_delay_mins = ARR_DELAY |>
      sd(na.rm = TRUE),
    mean_net_delay_as_dest = mean(ARR_DELAY - DEP_DELAY, na.rm = TRUE),
    median_net_delay_as_dest = median(ARR_DELAY - DEP_DELAY, na.rm = TRUE),
    sd_net_delay_as_dest = sd(ARR_DELAY - DEP_DELAY, na.rm = TRUE),
    prop_dest_del10 = mean(ARR_DELAY >= 10, na.rm = TRUE),
    prop_dest_del20 = mean(ARR_DELAY >= 20, na.rm = TRUE),
    prop_dest_del30 = mean(ARR_DELAY >= 30, na.rm = TRUE),
    prop_dest_early10 = mean(ARR_DELAY <= -10, na.rm = TRUE),
    prop_dest_early20 = mean(ARR_DELAY <= -20, na.rm = TRUE),
    prop_dest_early30 = mean(ARR_DELAY <= -30, na.rm = TRUE),
    .by = c(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME,
      DEST_STATE_ABR
    )
  )

origin_airports = data |>
  summarise(
    n_flights_departed = n(),
    mean_departure_delay_mins = DEP_DELAY |>
      mean(na.rm = TRUE),
    median_departure_delay_mins = DEP_DELAY |>
      median(na.rm = TRUE),
    sd_departure_delay_mins = DEP_DELAY |>
      sd(na.rm = TRUE),
    prop_dep_del10 = mean(DEP_DELAY >= 10, na.rm = TRUE),
    prop_dep_del20 = mean(DEP_DELAY >= 20, na.rm = TRUE),
    prop_dep_del30 = mean(DEP_DELAY >= 30, na.rm = TRUE),
    prop_dep_early10 = mean(DEP_DELAY <= -10, na.rm = TRUE),
    prop_dep_early20 = mean(DEP_DELAY <= -20, na.rm = TRUE),
    prop_dep_early30 = mean(DEP_DELAY <= -30, na.rm = TRUE),
    .by = c(
      ORIGIN_AIRPORT_SEQ_ID,
      ORIGIN_CITY_NAME,
      origin_state
    )
  )

airports = origin_airports |>
  full_join(
    y = destination_airports,
    by = join_by(
      ORIGIN_AIRPORT_SEQ_ID == DEST_AIRPORT_SEQ_ID,
      ORIGIN_CITY_NAME == DEST_CITY_NAME
    )
  ) |>
  mutate(
    seq_ID = ORIGIN_AIRPORT_SEQ_ID,
    city_name = ORIGIN_CITY_NAME,
    .keep = "unused",
    .before = origin_state
  ) |>
  relocate(DEST_STATE_ABR, .after = origin_state)

rm(destination_airports, origin_airports)

reporting_origin = data |>
  summarise(
    .by = c(
      ORIGIN_AIRPORT_SEQ_ID,
      ORIGIN_CITY_NAME,
      FL_DATE
    )
  ) |>
  summarise(
    days_reporting_as_origin = n(),
    .by = c(
      ORIGIN_AIRPORT_SEQ_ID,
      ORIGIN_CITY_NAME
    )
  )

reporting_destination = data |>
  summarise(
    .by = c(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME,
      FL_DATE
    )
  ) |>
  summarise(
    days_reporting_as_destination = n(),
    .by = c(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME
    )
  )

airports = airports |>
  full_join(
    y = reporting_origin,
    by = join_by(
      seq_ID == ORIGIN_AIRPORT_SEQ_ID,
      city_name == ORIGIN_CITY_NAME
    )
  ) |>
  full_join(
    y = reporting_destination,
    by = join_by(
      seq_ID == DEST_AIRPORT_SEQ_ID,
      city_name == DEST_CITY_NAME
    )
  )

rm(reporting_destination, reporting_origin)

airports = airports |>
  mutate(
    flights_received_per_day_reporting = n_flights_received / days_reporting_as_destination,
    .after = n_flights_received
  ) |>
  mutate(
    flights_departed_per_day_reporting = n_flights_departed / days_reporting_as_origin,
    .after = n_flights_departed
  ) |>
  select(!c(days_reporting_as_destination, days_reporting_as_origin))

# Average number of flights received by day of the week by DEST_CITY_NAME
by_day_of_week_dest = data |>
  summarise(
    num_flights_arr = n(),
    .by = c(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME,
      FL_DATE,
      DAY_OF_WEEK
    )
  ) |>
  summarise(
    total_arr_on_dow = sum(num_flights_arr),
    num_days_reporting_dow_dest = n(),
    mean_arr_on_dow = total_arr_on_dow / num_days_reporting_dow_dest,
    .by = c(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME,
      DAY_OF_WEEK
    )
  ) |>
  arrange( # where mean_flights ties, lower DOW is chosen
    DEST_AIRPORT_SEQ_ID,
    DAY_OF_WEEK
  ) |>
  pivot_wider(
    names_from = DAY_OF_WEEK,
    values_from = c(
      total_arr_on_dow,
      num_days_reporting_dow_dest,
      mean_arr_on_dow
    ),
    names_sort = TRUE,
    values_fill = 0
  )
by_day_of_week_dest = by_day_of_week_dest |>
  rowwise() |>
  mutate( # Busiest day of the week by DEST_CITY_NAME
    busiest_day_of_week_arr = c(
      mean_arr_on_dow_1,
      mean_arr_on_dow_2,
      mean_arr_on_dow_3,
      mean_arr_on_dow_4,
      mean_arr_on_dow_5,
      mean_arr_on_dow_6,
      mean_arr_on_dow_7
    ) |>
      which.max() |>
      factor(levels = c("1", "2", "3", "4", "5", "6", "7")),
    # Average number of flights on the busiest day of the week
    mean_arr_on_busiest_dow = case_when(
      busiest_day_of_week_arr == "1" ~ mean_arr_on_dow_1,
      busiest_day_of_week_arr == "2" ~ mean_arr_on_dow_2,
      busiest_day_of_week_arr == "3" ~ mean_arr_on_dow_3,
      busiest_day_of_week_arr == "4" ~ mean_arr_on_dow_4,
      busiest_day_of_week_arr == "5" ~ mean_arr_on_dow_5,
      busiest_day_of_week_arr == "6" ~ mean_arr_on_dow_6,
      busiest_day_of_week_arr == "7" ~ mean_arr_on_dow_7
    )
  ) |>
  ungroup() |>
  select(!c(num_days_reporting_dow_dest_1:num_days_reporting_dow_dest_7))

by_day_of_week_origin = data |>
  summarise(
    num_flights_dep = n(),
    .by = c(
      ORIGIN_AIRPORT_SEQ_ID,
      ORIGIN_CITY_NAME,
      FL_DATE,
      DAY_OF_WEEK
    )
  ) |>
  summarise(
    total_dep_on_dow = sum(num_flights_dep),
    num_days_reporting_dow_origin = n(),
    mean_dep_on_dow = total_dep_on_dow / num_days_reporting_dow_origin,
    .by = c(
      ORIGIN_AIRPORT_SEQ_ID,
      ORIGIN_CITY_NAME,
      DAY_OF_WEEK
    )
  ) |>
  arrange( # where mean_flights ties, lower DOW is chosen
    ORIGIN_AIRPORT_SEQ_ID,
    DAY_OF_WEEK
  ) |>
  pivot_wider(
    names_from = DAY_OF_WEEK,
    values_from = c(
      total_dep_on_dow,
      num_days_reporting_dow_origin,
      mean_dep_on_dow
    ),
    names_sort = TRUE,
    values_fill = 0
  )
by_day_of_week_origin = by_day_of_week_origin |>
  rowwise() |>
  mutate( # Busiest day of the week by DEST_CITY_NAME
    busiest_day_of_week_dep = c(
      mean_dep_on_dow_1,
      mean_dep_on_dow_2,
      mean_dep_on_dow_3,
      mean_dep_on_dow_4,
      mean_dep_on_dow_5,
      mean_dep_on_dow_6,
      mean_dep_on_dow_7
    ) |>
      which.max() |>
      factor(levels = c("1", "2", "3", "4", "5", "6", "7")),
    # Average number of flights on the busiest day of the week
    mean_dep_on_busiest_dow = case_when(
      busiest_day_of_week_dep == "1" ~ mean_dep_on_dow_1,
      busiest_day_of_week_dep == "2" ~ mean_dep_on_dow_2,
      busiest_day_of_week_dep == "3" ~ mean_dep_on_dow_3,
      busiest_day_of_week_dep == "4" ~ mean_dep_on_dow_4,
      busiest_day_of_week_dep == "5" ~ mean_dep_on_dow_5,
      busiest_day_of_week_dep == "6" ~ mean_dep_on_dow_6,
      busiest_day_of_week_dep == "7" ~ mean_dep_on_dow_7
    )
  ) |>
  ungroup() |>
  select(!c(num_days_reporting_dow_origin_1:num_days_reporting_dow_origin_7))

airports = airports |>
  full_join(
    y = by_day_of_week_origin,
    by = join_by(
      seq_ID == ORIGIN_AIRPORT_SEQ_ID,
      city_name == ORIGIN_CITY_NAME
    )
  ) |>
  full_join(
    y = by_day_of_week_dest,
    by = join_by(
      seq_ID == DEST_AIRPORT_SEQ_ID,
      city_name == DEST_CITY_NAME
    )
  )

rm(by_day_of_week_dest, by_day_of_week_origin)

flights = data |>
  summarise(
    num_flights = n(),
    .by = c(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME,
      ORIGIN_AIRPORT_SEQ_ID,
      ORIGIN_CITY_NAME
    )
  )
most_common_origin = flights |>
  summarise(
    most_common_origin_apt = ORIGIN_AIRPORT_SEQ_ID[[which.max(num_flights)]],
    most_common_origin_city = ORIGIN_CITY_NAME[[which.max(num_flights)]],
    .by = c(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME
    )
  )
most_common_dest = flights |>
  summarise(
    most_common_dest_apt = DEST_AIRPORT_SEQ_ID[[which.max(num_flights)]],
    most_common_dest_city = DEST_CITY_NAME[[which.max(num_flights)]],
    .by = c(
      ORIGIN_AIRPORT_SEQ_ID,
      ORIGIN_CITY_NAME
    )
  )

airports = airports |>
  full_join(
    y = most_common_dest,
    by = join_by(
      seq_ID == ORIGIN_AIRPORT_SEQ_ID,
      city_name == ORIGIN_CITY_NAME
    )
  ) |>
  full_join(
    y = most_common_origin,
    by = join_by(
      seq_ID == DEST_AIRPORT_SEQ_ID,
      city_name == DEST_CITY_NAME
    )
  )

rm(most_common_dest, most_common_origin)

mc_origin_state = data |>
  summarise(
    num_flights = n(),
    .by = c(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME,
      origin_state
    )
  ) |>
  summarise(
    most_common_origin_state = origin_state[[which.max(num_flights)]] |>
      factor(levels = US_states),
    .by = c(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME)
  )
mc_dest_state = data |>
  summarise(
    num_flights = n(),
    .by = c(
      ORIGIN_AIRPORT_SEQ_ID,
      ORIGIN_CITY_NAME,
      DEST_STATE_ABR
    )
  ) |>
  summarise(
    most_common_dest_state = DEST_STATE_ABR[[which.max(num_flights)]] |>
      factor(levels = US_states),
    .by = c(
      ORIGIN_AIRPORT_SEQ_ID,
      ORIGIN_CITY_NAME)
  )

airports = airports |>
  full_join(
    y = mc_dest_state,
    by = join_by(
      seq_ID == ORIGIN_AIRPORT_SEQ_ID,
      city_name == ORIGIN_CITY_NAME
    )
  ) |>
  full_join(
    y = mc_origin_state,
    by = join_by(
      seq_ID == DEST_AIRPORT_SEQ_ID,
      city_name == DEST_CITY_NAME
    )
  )

rm(mc_origin_state, mc_dest_state)

airports = airports |>
  select(!c(total_dep_on_dow_1:total_dep_on_dow_7)) |>
  select(!c(total_arr_on_dow_1:total_arr_on_dow_7)) |>
  select(!c(mean_dep_on_dow_1:mean_dep_on_dow_7)) |>
  select(!c(mean_arr_on_dow_1:mean_arr_on_dow_7))

airports |>
  write_csv(
    "airports.csv",
    na = "NA",
    append = FALSE,
    quote = "needed"
  )

flights |>
  write_csv(
    "num_flights.csv",
    na = "NA",
    append = FALSE,
    quote = "needed"
  )
