library(tidyverse)

NA_STR <- c("\\N","NA")

circuits <- read.csv("circuits.csv", na.strings=NA_STR)
constructor_results <- read.csv("constructor_results.csv", na.strings=NA_STR)
constructor_standings <- read.csv("constructor_standings.csv", na.strings=NA_STR)
driver_standings <- read.csv("driver_standings.csv", na.strings=NA_STR)
drivers <- read.csv("drivers.csv", na.strings=NA_STR)
lap_times <- read.csv("lap_times.csv", na.strings=NA_STR)
pit_stops <- read.csv("pit_stops.csv", na.strings=NA_STR)
qualifying <- read.csv("qualifying.csv", na.strings=NA_STR)
races <- read.csv("races.csv", na.strings=NA_STR)
results <- read.csv("results.csv", na.strings=NA_STR)
seasons <- read.csv("seasons.csv", na.strings=NA_STR)
sprint_results <- read.csv("sprint_results.csv", na.strings=NA_STR)
status <- read.csv("status.csv", na.strings=NA_STR)

races <- races %>%
    filter(year > 1990) %>% # only consider races after 1990
    select(raceId, year, round, circuitId, name, date)
head(races)

# 13 23 28 40
# 16
# 16 36
var_fn <- function(, max_lap) {
    x <- c(0, x, max_lap)
    diffs <- c(x[1]-0)
    for(i in 1:(length(x)-1)) {
        diffs <- c(diffs, x[i+1]-x[i])
    }
    diffs <- c(diffs )
    return(var(diffs))
}

mean_fn <- function(x, max_lap) {
    diffs <- c()
    x <- c(0, x, max_lap)
    for(i in 1:(length(x)-1)) {
        diffs <- c(diffs, x[i+1]-x[i])
    }
    return(mean(diffs))
}

max_lap_values <- lap_times %>%
    group_by(raceId) %>%
    summarize(max_lap = max(lap))
head(max_lap_values)


var_fn <- function(x, max_lap) {
    print(x)
    return (var(c(0, x, max_lap)/max_lap))
}

pit_stops %>%
    left_join(races, by = 'raceId') %>%
    left_join(max_lap_values, by = 'raceId') %>%
    arrange(raceId, driverId) %>%
    group_by(raceId, driverId, year) %>%
    summarize(stop_num = max(stop)) %>%
    ggplot(aes(x = stop_num)) +
        geom_histogram() + 
        facet_wrap(~year)

p_s <- pit_stops %>%
    left_join(races, by = 'raceId') %>%
    left_join(max_lap_values, by = 'raceId') %>%
    arrange(raceId, driverId) %>%
    group_by(raceId, driverId, year, circuitId) %>%
    summarize(lap_varr = var(c(0, lap, mean(max_lap))/mean(max_lap)))

ggplot(p_s, aes(x = lap_varr)) +
    geom_histogram() +
    facet_wrap(~year)

ggplot(p_s, aes(x = lap_varr)) +
    geom_histogram() +
    facet_wrap(~circuitId)

ggplot(p_s, aes(x = lap_varr)) +
    geom_histogram() +
    facet_wrap(year~circuitId)
