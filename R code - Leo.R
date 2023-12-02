library(dplyr)

data <- Flights1_2019_1

# Ensure date is in the correct format (adjust the format as per your data)
data$FL_DATE <- as.Date(data$FL_DATE, format="%Y-%m-%d")

# Add columns for day of the week and month
data$month <- format(data$FL_DATE, "%m")

# Number of flights received per day by DEST_CITY_NAME
daily_flights <- data %>%
  group_by(DEST_CITY_NAME, FL_DATE) %>%
  summarize(num_flights = n())

# Average number of flights received by day of the week by DEST_CITY_NAME
weekly_avg_flights <- data %>%
  group_by(DEST_CITY_NAME, DAY_OF_WEEK) %>%
  summarize(total_flights = n())

# Average number of flights received by month by DEST_CITY_NAME
monthly_avg_flights <- data %>%
  group_by(DEST_CITY_NAME, month) %>%
  summarize(total_flights = n())

# Busiest month by DEST_CITY_NAME
busiest_month <- monthly_avg_flights %>%
  group_by(DEST_CITY_NAME) %>%
  top_n(1, total_flights)

# Busiest day of the week by DEST_CITY_NAME
busiest_day <- weekly_avg_flights %>%
  group_by(DEST_CITY_NAME) %>%
  top_n(1, total_flights)

# Average number of flights on the busiest day of the week by DEST_CITY_NAME
avg_flights_busiest_day <- busiest_day %>%
  left_join(weekly_avg_flights, by = c("DEST_CITY_NAME", "DAY_OF_WEEK"))

# Number of flights received by origin state by DEST_CITY_NAME
flights_by_origin_state <- data %>%
  group_by(DEST_CITY_NAME, ORIGIN_CITY_NAME) %>%
  summarize(num_flights = n())
