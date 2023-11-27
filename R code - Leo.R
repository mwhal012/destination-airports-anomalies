library(lubridate)
library(dplyr)

Flight_data$FL_DATE <- as.Date(Flight_data$FL_DATE)

# Number of flights received per day
flights_per_day <- Flight_data %>%
  group_by(FL_DATE) %>%
  summarise(NumFlights = n())

# Number of flights received per day
flights_per_day <- Flight_data %>%
  group_by(FL_DATE) %>%
  summarise(NumFlights = n())

# Average number of flights received by day of the week
Flight_data$DAY_OF_WEEK <- weekdays(Flight_data$FL_DATE)
avg_flights_day_of_week <- Flight_data %>%
  group_by(DAY_OF_WEEK) %>%
  summarise(AvgFlights = mean(n(), na.rm = TRUE))

# Average number of flights received by month
Flight_data$Month <- month(Flight_data$FL_DATE)
avg_flights_month <- Flight_data %>%
  group_by(Month) %>%
  summarise(AvgFlights = mean(n(), na.rm = TRUE))

# Busiest month
busiest_month <- avg_flights_month[which.max(avg_flights_month$AvgFlights), ]

# Busiest day of the week
busiest_day_of_week <- avg_flights_day_of_week[which.max(avg_flights_day_of_week$AvgFlights), ]

# Average number of flights on the busiest day of the week
Flight_data$NumFlights <- 1
avg_flights_busiest_day <- mean(Flight_data[Flight_data$DAY_OF_WEEK == busiest_day_of_week$DAY_OF_WEEK, "NumFlights"], na.rm = TRUE)