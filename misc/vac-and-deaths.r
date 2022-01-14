library(tidyverse)
library(lubridate)

colorscale <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=10))

vaccinations <- read_csv("https://data.cdc.gov/resource/8xkx-amqh.csv?&$limit=4000") %>%
        filter(date == max(date))

statenames = tibble(stateabb = state.abb, statename = state.name)
statenames = bind_rows(statenames, tibble(stateabb = "DC", statename = "District of Columbia"))

vax <-
    vaccinations %>%
    select(date, fips, recip_county, recip_state, administered_dose1_pop_pct) %>%
    mutate(recip_county = str_sub(recip_county, end = -8)) %>%
    inner_join(statenames, by = c("recip_state" = "stateabb")) %>%
    mutate(countyid = paste0(recip_county, ", ", statename)) %>%
    # inner_join(electionresults, by = "fips") %>%
    rename(dose1 = "administered_dose1_pop_pct") %>%
    filter(dose1 != 0)

load("Rdata/us_casesdeaths.Rdata")

cutoff_date <- as.Date("2021-04-01")

countypop <-
    us_casesdeaths %>%
    pivot_wider(c(county, state, population)) %>%
    mutate(countyid = paste(county, state, sep = ", "))

countydeaths <-
    us_casesdeaths %>% 
    filter(date > cutoff_date) %>%
    group_by(county, state) %>%
    summarize(deaths = sum(deaths)) %>%
    mutate(countyid = paste0(county, ", ", state)) %>%
    inner_join(countypop) %>%
    mutate(deathper100k = deaths / population * 1e5)

vax %>%
    inner_join(countydeaths, by = "countyid") %>%
    filter(dose1 != 0) %>% filter(dose1 > 10) %>% filter(dose1 < 90) %>%
    mutate(box = cut(dose1, seq(0,100,10),seq(10,100,10))) %>%
    group_by(box) %>%
    summarize(deathper100k = mean(deathper100k)) %>%
    ggplot +
        aes(box, deathper100k, group = TRUE)  +
        geom_line() + 
        scale_y_continuous(limit = c(0, NA)) + 
        labs(x = "Vaccination rate (%)")

countycases <-
    us_casesdeaths %>% 
    filter(date > cutoff_date) %>%
    group_by(county, state) %>%
    summarize(cases = sum(cases)) %>%
    mutate(countyid = paste0(county, ", ", state)) %>%
    inner_join(countypop) %>%
    mutate(casesper100k = cases / population * 1e5)

vax %>%
    inner_join(countycases, by = "countyid") %>%
    filter(dose1 != 0) %>% filter(dose1 > 10) %>% filter(dose1 < 90) %>%
    mutate(box = cut(dose1, seq(0,100,10),seq(10,100,10))) %>%
    group_by(box) %>%
    summarize(casesper100k = mean(casesper100k)) %>%
    ggplot +
        aes(box, casesper100k, group = TRUE)  +
        geom_line() + 
        scale_y_continuous(breaks = 1000 * 0:10, limits = c(0, NA)) + 
        labs(x = "Vaccination rate (%)")


