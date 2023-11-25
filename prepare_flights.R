library(tidyverse)
library(rlang)
library(stringi)
library(conflicted)
"dplyr" |>
	conflict_prefer_all(quiet = TRUE)

data = "Flights1_2019_1.csv" |>
  read_csv() |>
  select(
    c(
      DAY_OF_WEEK,
      FL_DATE,
      ORIGIN_AIRPORT_SEQ_ID:ORIGIN_CITY_NAME,
      DEST_AIRPORT_SEQ_ID:ARR_DELAY_NEW
    )
  )

data = data |>
  mutate(
    observ_n = c(1:nrow(data)),
    .before = DAY_OF_WEEK # Monday = 1, Sunday = 7
  ) |>
  mutate(
    origin_state = ORIGIN_CITY_NAME |>
      str_extract(pattern = regex("[A-Z]{2}$")),
    .after = ORIGIN_CITY_NAME
  ) |>
  mutate(
    dest_state = DEST_CITY_NAME |>
      str_extract(pattern = regex("[A-Z]{2}$")),
    .after = DEST_STATE_ABR
  )

receiving_airports = data |>
  summarise(
    n_flights_received = n(),
    mean_delay_mins = mean(ARR_DELAY, na.rm = TRUE),
    sd_delay_mins = sd(ARR_DELAY, na.rm = TRUE),
    .by = c(
      DEST_AIRPORT_SEQ_ID,
      DEST_CITY_NAME,
      dest_state,
      DEST_STATE_ABR
    )
  )

#receiving_airports |>
#	write_csv(
#		file = "./dest_airports.csv",
#		append = FALSE,
#		col_names = TRUE,
#		quote = "needed",
#		escape = "backslash"
#	)
#zip(
#  zipfile = "./dest_airports.zip",
#  files = "./dest_airports.csv"
#)
#file.remove("./dest_airports.csv")
