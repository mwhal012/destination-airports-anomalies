library(tidyverse)
library(rlang)
library(stringi)
library(conflicted)
"dplyr" |>
  conflict_prefer_all(quiet = TRUE)

"Flights1_2019_1.zip" |>
  unzip(overwrite = TRUE)
data = "Flights1_2019_1.csv" |>
  read_csv(
    col_types = cols(
      DAY_OF_WEEK = col_factor( # Monday = 1, Sunday = 7
        levels = c("1", "2", "3", "4", "5", "6", "7"),
        ordered = FALSE,
        include_na = FALSE
      ),
      FL_DATE = col_date(),
      DEST_STATE_ABR = col_factor(),
      ORIGIN_AIRPORT_SEQ_ID = col_factor(),
      DEST_AIRPORT_SEQ_ID = col_factor()
    )
  ) |>
  select(
    c(
      DAY_OF_WEEK,
      FL_DATE,
      ORIGIN_AIRPORT_SEQ_ID:ORIGIN_CITY_NAME,
      DEST_AIRPORT_SEQ_ID:ARR_DELAY_NEW
    )
  ) |>
  mutate(
    origin_state = ORIGIN_CITY_NAME |>
      str_extract(pattern = regex("[A-Z]{2}$")),
    .after = ORIGIN_CITY_NAME
  ) |>
  mutate(
    arrival_datetime = FL_DATE |>
      paste(ARR_TIME, sep = " ") |>
      fast_strptime(format = "%Y-%m-%d %H%M")
  )

receiving_airports = data |>
  summarise(
    n_flights_received = n(),
    mean_arrival_delay_mins = ARR_DELAY |>
      mean(na.rm = TRUE),
    sd_arrival_delay_mins = ARR_DELAY |>
      sd(na.rm = TRUE),
    mean_net_delay = mean(
      ARR_DELAY - DEP_DELAY,
      na.rm = TRUE
    ),
    sd_net_delay = sd(
      ARR_DELAY - DEP_DELAY,
      na.rm = TRUE
    ),
    prop_flight_del5 = sum(ARR_DELAY >= 5, na.rm = TRUE) / n(),
    prop_flight_del10 = sum(ARR_DELAY >= 10, na.rm = TRUE) / n(),
    prop_flight_del20 = sum(ARR_DELAY >= 20, na.rm = TRUE) / n(),
    prop_flight_del30 = sum(ARR_DELAY >= 30, na.rm = TRUE) / n(),
    prop_flight_early5 = sum(ARR_DELAY <= -5, na.rm = TRUE) / n(),
    prop_flight_early10 = sum(ARR_DELAY <= -10, na.rm = TRUE) / n(),
    prop_flight_early20 = sum(ARR_DELAY <= -20, na.rm = TRUE) / n(),
    prop_flight_early30 = sum(ARR_DELAY <= -30, na.rm = TRUE) / n(),
    .by = c(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME
    )
  )

reporting = data |>
  summarise(
    .by = c(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME,
      FL_DATE
    )
  ) |>
  summarise(
    days_reporting = n(),
    .by = c(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME
    )
  )

receiving_airports = receiving_airports |>
  full_join(
    y = reporting,
    by = join_by(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME
    )
  ) |>
  mutate(
    flights_per_day = n_flights_received / days_reporting,
    .after = n_flights_received
  )
rm(reporting)

flights_every_day = data |>
  summarise(
    num_flights = n(),
    .by = c(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME,
      FL_DATE
    )
  ) |>
  pivot_wider(
    names_from = FL_DATE,
    values_from = num_flights,
    names_prefix = "flights_received_on_",
    names_sort = TRUE,
    values_fill = 0
  )
receiving_airports = receiving_airports |>
  full_join(
    y = flights_every_day,
    by = join_by(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME
    )
  )
rm(flights_every_day)

# Average number of flights received by day of the week by DEST_CITY_NAME
by_day_of_week = data |>
  summarise(
    num_flights = n(),
    .by = c(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME,
      FL_DATE,
      DAY_OF_WEEK
    )
  ) |>
  summarise(
    total_flights_on_dow = sum(num_flights),
    num_days_reporting_dow = n(),
    mean_flights_on_dow = total_flights_on_dow / num_days_reporting_dow,
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
      total_flights_on_dow,
      num_days_reporting_dow,
      mean_flights_on_dow
    ),
    names_sort = TRUE,
    values_fill = 0
  )
by_day_of_week = by_day_of_week |>
  rowwise() |>
  mutate( # Busiest day of the week by DEST_CITY_NAME
    busiest_day_of_week = c(
      mean_flights_on_dow_1,
      mean_flights_on_dow_2,
      mean_flights_on_dow_3,
      mean_flights_on_dow_4,
      mean_flights_on_dow_5,
      mean_flights_on_dow_6,
      mean_flights_on_dow_7
    ) |>
      which.max() |>
      factor(levels = c("1", "2", "3", "4", "5", "6", "7")),
    # Average number of flights on the busiest day of the week
    mean_flights_on_busiest_dow = case_when(
      busiest_day_of_week == "1" ~ mean_flights_on_dow_1,
      busiest_day_of_week == "2" ~ mean_flights_on_dow_2,
      busiest_day_of_week == "3" ~ mean_flights_on_dow_3,
      busiest_day_of_week == "4" ~ mean_flights_on_dow_4,
      busiest_day_of_week == "5" ~ mean_flights_on_dow_5,
      busiest_day_of_week == "6" ~ mean_flights_on_dow_6,
      busiest_day_of_week == "7" ~ mean_flights_on_dow_7
    )
  ) |>
  ungroup()

receiving_airports = receiving_airports |>
  full_join(
    y = by_day_of_week,
    by = join_by(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME
    )
  )
rm(by_day_of_week)

# Number of flights received by origin by DEST_CITY_NAME
flights_by_origin = data |>
  summarise(
    num_flights = n(),
    .by = c(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME,
      ORIGIN_AIRPORT_SEQ_ID,
      ORIGIN_CITY_NAME
    )
  ) |>
  summarise(
    most_common_origin_apt = ORIGIN_AIRPORT_SEQ_ID[[which.max(num_flights)]],
    most_common_origin_city = ORIGIN_CITY_NAME[[which.max(num_flights)]],
    .by = c(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME)
  )

receiving_airports = receiving_airports |>
  full_join(
    y = flights_by_origin,
    by = join_by(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME
    )
  )

flights_by_origin = data |>
  summarise(
    num_flights = n(),
    .by = c(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME,
      origin_state
    )
  ) |>
  summarise(
    most_common_origin_state = origin_state[[which.max(num_flights)]],
    .by = c(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME)
  )

receiving_airports = receiving_airports |>
  full_join(
    y = flights_by_origin,
    by = join_by(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME
    )
  )
rm(flights_by_origin)

receiving_airports |>
  write_csv(
    "receiving_airports.csv",
    na = "NA",
    append = FALSE,
    quote = "needed"
  )
