# prepare population data
library(tidyverse)

us_deaths_file <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
infocolumnnames <- c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2", "Province_State", "Country_Region", "Lat", "Long_", "Combined_Key", "Population")
infocols <- length(infocolumnnames)

rawdata <-
    read_csv(us_deaths_file)

county_data <-
    rawdata %>%
    janitor::clean_names() %>%
    select(fips, admin2, province_state, population) %>%
    distinct(fips, admin2, province_state, population) %>%
    rename(county = admin2, state = province_state) %>%
    mutate(fips = case_when(
        fips < 10000 ~ paste0("0", fips),
        TRUE ~ paste(fips)
    ))


state_pop <-
    county_data %>%
    group_by(state) %>%
    summarize(
        population = sum(population),
        .groups = "drop"
    ) %>%
    filter(population > 0)


save(county_data, state_pop, file = "Rdata/county_data.Rdata")