# Exercise 4: practicing with dplyr

# Install the `nycflights13` package. Load (`library()`) the package.
# You'll also need to load `dplyr`

install.packages("nycflights13")
library("nycflights13")
library("dplyr")

# The data frame `flights` should now be accessible to you.
# Use functions to inspect it: how many rows and columns does it have?
# What are the names of the columns?
# Use `??flights` to search for documentation on the data set (for what the 
# columns represent)

ncol(flights)
nrow(flights)
colnames(flights)

# Use `dplyr` to give the data frame a new column that is the amount of time
# gained or lost while flying (that is: how much of the delay arriving occured
# during flight, as opposed to before departing).

flights_time_gain_lost <- mutate(flights, time_gain_or_lost = arr_delay - 
                                   dep_delay)

# Use `dplyr` to sort your data frame in descending order by the column you just
# created. Remember to save this as a variable (or in the same one!)

flight_sorted <- arrange(flights_time_gain_lost, -time_gain_or_lost)

# For practice, repeat the last 2 steps in a single statement using the pipe
# operator. You can clear your environmental variables to "reset" the data frame

flights <- flights %>% mutate(time_gain_or_lost = arr_delay - 
                                   dep_delay) %>% arrange(-time_gain_or_lost)

# Make a histogram of the amount of time gained using the `hist()` function

hist(flights$time_gain_or_lost)

# On average, did flights gain or lose time?
# Note: use the `na.rm = TRUE` argument to remove NA values from your aggregation

mean(flights$time_gain_or_lost, na.rm = TRUE)

# Create a data.frame of flights headed to SeaTac ('SEA'), only including the
# origin, destination, and the "gain_in_air" column you just created
 
to_seatac <- flights %>% select(origin, dest, time_gain_or_lost) %>% filter(dest 
                                                                    == "SEA")

# On average, did flights to SeaTac gain or loose time?

mean(to_seatac$time_gain_or_lost, na.rm = TRUE)

# Consider flights from JFK to SEA. What was the average, min, and max air time
# of those flights? Bonus: use pipes to answer this question in one statement
# (without showing any other data)!

filter(flights, origin == "JFK", dest == "SEA") %>% 
  summarize(
    avg_air_time = mean(air_time, na.rm = TRUE),
    min_air_time = min(air_time, na.rm = TRUE),
    max_air_time = max(air_time, na.rm = TRUE)
  )


