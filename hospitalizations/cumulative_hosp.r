library(tidyverse)
library(lubridate)
library(zoo)
library(scales)
library(patchwork)
theme_set(theme_light())

# lo_cutoff_date <- as.Date("2021-04-01")
# hi_cutoff_date <- as.Date("2021-11-01")

# load("Rdata/us_casesdeaths.Rdata")

# sources = paste0("COVID-19 deaths data from JHU. Hospitalization data from Our World In Data")

# casesdeaths <-
#     us_casesdeaths %>%
#     group_by(date) %>%
#     summarize(cases = sum(cases),
#               deaths = sum(deaths),
#               .groups = "drop") %>%
#     filter(date > lo_cutoff_date)

hosp_file <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/hospitalizations/covid-hospitalizations.csv"

hospitalization <-
    read_csv(hosp_file) %>%
    pivot_wider(names_from = "indicator", values_from = "value") %>%
    janitor::clean_names() %>%
    rename(country = "entity") %>%
    select(-iso_code, -ends_with("per_million"))


hospitalization %>%
    filter(country == "United States") %>%
    select(date, weekly_new_hospital_admissions) %>%
    drop_na(weekly_new_hospital_admissions) %>%
    mutate(hos = cumsum(weekly_new_hospital_admissions)/7) %>%
    ggplot(aes(date, hos)) + geom_point()
