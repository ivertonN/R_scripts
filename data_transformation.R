library(nycflights13)
library(tidyverse)
flights

jan1 <- filter(flights, month == 1, day == 1)

(dec25 <- filter(flights, month == 12, day == 25))

1/49 * 49 == 1 #false
near( 1/49 * 49, 1 ) #true

nov_dec <- filter(flights, month == 12 | month == 11)
nov_dec1 <- filter(flights, month %in% c(11, 12))

filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

#nao esquecer de incluir NA com is.na()
#1)
more_than_2_hours <- filter(flights, dep_delay >= 120, arr_delay >= 120)
houston <- filter(flights, dest %in% c("IAH", "HOU"))

#1.3
carrier <- filter(flights, carrier %in% c("UA", "US", "DL"))
#1.4
dep_summer <- filter(flights, month %in% c(7, 8, 9))
#1.5
arrived_2late_not_leave_late <-filter(flights, dep_delay <= 0, arr_delay > 120)
ggplot(arrived_2late_not_leave_late, mapping = aes(x = month)) + 
  geom_bar()

#1.6
filter(flights, dep_delay > 60, arr_delay - dep_delay > 30)
#1.7
View(flights)
summary(flights$dep_time)

filter(flights, dep_time <= 600 | dep_time == 2400)

between(2, 2, 3)
#2
dep_time_NA <-filter(flights, is.na(dep_time))
summary(dep_time_NA)

arr_time_NA <-filter(dep_time_NA, is.na(arr_time))
#3.1
arrange(flights, desc(is.na(dep_time)), dep_time)
#4
arrange(flights, desc(arr_delay - dep_delay))
#5
air_time_sort <- arrange(flights, desc(air_time))
View(air_time_sort)


#5.4.1
variable <- c("dep_time", "dep_delay")
select(flights, one_of(variable))

select(flights, contains("TIME", ignore.case = FALSE))

flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)

mutate(flights_sml, 
  gain = dep_delay - arr_delay, 
  speed = distance / air_time * 60
)

mutate(flights_sml,
  gain = dep_delay - arr_delay,
  hours = air_time / 60,
  gain_per_hour = gain / hours
)

transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)

time2mins <- function(x) {
  ((x %/% 100)*60 + x %% 100) %% 1440
}

flight_min <- mutate(flights, 
      dep_time_min = time2mins(dep_time),
      sched_dep_time_min = time2mins(sched_dep_time)
        )

select(flight_min, dep_time_min, sched_dep_time_min, sched_dep_time, dep_time)

flight <- mutate(flights,
                 air_time_new = (time2mins(arr_time) - time2mins(dep_time)),
                 air_time = time2mins(air_time),
                 air_time_diff = air_time - (air_time_new)
                         )
select(flight, air_time, air_time_new, arr_delay, dep_delay, air_time_diff)
(filter(flight, air_time_diff != 0))

ggplot(filter(flight, dest == "LAX"), aes(x = air_time_diff)) +
  geom_histogram(binwidth = 1)

flight <- mutate(flights, 
                 sched_dep_time_diff = sched_dep_time - (dep_time - dep_delay)
                 )
View(select(filter(flight, sched_dep_time_diff != 0), everything()))

ggplot(flight, aes(x = sched_dep_time_diff)) +
  geom_bar()


min_rank(filter(flight, dep))

x <- 1:3 + 1:10







delays <- flights %>% 
group_by(dest) %>% 
summarise(
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
) %>% 
filter(count > 20, dest != "HNL")

# It looks like delays increase with distance up to ~750 miles 
# and then decrease. Maybe as flights get longer there's more 
# ability to make up delays in the air?
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)
#> `geom_smooth()` using method = 'loess' and formula 'y ~ x'




not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(dep_delay),
    n = n() 
    ) %>% 
ggplot(mapping = aes(x = n, y = delay)) + 
  geom_col()

View(flights)

not_cancelled %>% 
  count(tailnum, wt = distance)

flights %>% 
  filter(!is.na(tailnum)) %>%
  mutate(on_time = !is.na(arr_time) & (arr_delay <= 0)) %>%
  group_by(tailnum) %>% 
  summarise(on_time = mean(on_time), n  = n()) %>%
  filter(n > 20) %>% 
  filter(min_rank(on_time) == 1)

flights %>% 
  group_by(tailnum) %>%
  mutate(
    arr_delay_mean = mean(arr_delay),
    n = n()
  ) %>% 
  filter(n >= 20) %>% 
  filter(min_rank(desc(arr_delay_mean)) == 1)

flights %>%
  group_by(hour) %>% 
  summarise(arr_delay_mean = mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(arr_delay_mean)

flights %>% 
  filter(arr_delay > 0) %>% 
  group_by(dest) %>%
  summarise(
    arr_delay_total = mean(arr_delay),
    arr_delay_prop = arr_delay / arr_delay_total
            )
lagged_delays <- flights %>% 
  arrange(dest, month, day, dep_time) %>% 
  group_by(origin) %>% 
  mutate(dep_delay_lag = lag(dep_delay)) %>% 
  filter(!is.na(dep_delay), !is.na(dep_delay_lag))

lagged_delays %>% 
  group_by(origin, dep_delay_lag) %>% 
  summarise(dep_delay_mean = mean(dep_delay)) %>% 
  ggplot(aes(y = dep_delay_mean, x  = dep_delay_lag))+
  geom_point() + 
  geom_smooth() +
  facet_wrap(~origin, ncol = 1) +
  scale_x_continuous(breaks = seq(0, 1500, by = 100)) +
  labs(y = "departure delay", x = "previous departure delay")

standardzed_flights <- flights %>%
  filter(!is.na(air_time)) %>% 
  group_by(dest) %>%
  mutate(
    air_time_mean = mean(air_time),
    air_time_sd = sd(air_time),
    n = n()
            ) %>% 
  ungroup() %>% 
  mutate(air_time_standard = (air_time - air_time_mean)/air_time_sd+1)


ggplot(standardzed_flights, aes(x = air_time_standard)) + 
  geom_density()

standardzed_flights %>% 
  arrange(air_time_standard) %>%
  select(carrier, flight, origin, dest , month , day,
         air_time, air_time_mean, air_time_standard) %>% 
  head(10) %>% 
  print(width = Inf)

flights %>%
  group_by(dest) %>% 
  mutate(
    n_carrier = n_distinct(carrier)
  ) %>% 
  filter(n_carrier >= 2) %>% 
  ungroup() %>%
  group_by(carrier) %>%
  summarize(
    n_dest = n_distinct(dest)
  ) %>% 
  arrange(desc(n_dest))

flights %>%
  select(tailnum, year, month, day, dep_delay) %>% 
  filter(!is.na(dep_delay)) %>% 
  arrange(year, month, day) %>% 
  group_by(tailnum) %>% 
  mutate(cumulative_hr_delays = cumsum(dep_delay>60)) %>% 
  summarize(total_flights = sum(cumulative_hr_delays < 1))
  
