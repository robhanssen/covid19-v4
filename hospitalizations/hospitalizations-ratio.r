library(tidyverse)
library(lubridate)
library(zoo)
library(scales)
library(patchwork)
theme_set(theme_light())

lo_cutoff_date <- as.Date("2021-04-01")
hi_cutoff_date <- as.Date("2021-11-01")

load("Rdata/us_casesdeaths.Rdata")

sources = paste0("COVID-19 deaths data from JHU. Hospitalization data from Our World In Data")

casesdeaths <-
    us_casesdeaths %>%
    group_by(date) %>%
    summarize(cases = sum(cases),
              deaths = sum(deaths),
              .groups = "drop") %>%
    filter(date > lo_cutoff_date)

hosp_file <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/hospitalizations/covid-hospitalizations.csv"

hospitalization <-
    read_csv(hosp_file) %>%
    pivot_wider(names_from = "indicator", values_from = "value") %>%
    janitor::clean_names() %>%
    rename(country = "entity") %>%
    select(-iso_code, -ends_with("per_million"))


hospitalization %>%
    filter(country == "United States") %>%
    ggplot + 
    aes(date, daily_hospital_occupancy) + 
    geom_point(color = "blue") + 
    geom_point(aes(y = 3*daily_icu_occupancy), color = "red") 
 
hospitalization %>%
    filter(country == "United States") %>%
    filter(date > lo_cutoff_date, date < hi_cutoff_date) %>%
    ggplot + 
    aes(daily_hospital_occupancy, daily_icu_occupancy) + 
    geom_point() + 
    geom_abline(intercept = 283, slope = 0.273)

hospitalization %>%
    filter(country == "United States") %>%
    mutate(ratio = daily_icu_occupancy / daily_hospital_occupancy) %>%
    ggplot + 
    aes(date, ratio) + 
    geom_point() + 
    scale_y_continuous(limits = c(0, NA))

trainingcomment <- paste0("Training data (in gray) between ",
                         format(lo_cutoff_date, format = "%b %d, %Y"),
                         " and ",
                         format(hi_cutoff_date, format = "%b %d, %Y"),
                         ".\nNew data (in green) after ",
                         format(hi_cutoff_date, format = "%b %d, %Y"),
                         ".")

hospitalization %>%
    filter(country == "United States") %>%
    filter(date > lo_cutoff_date, date < hi_cutoff_date) %>%
    lm(daily_icu_occupancy ~ daily_hospital_occupancy + 0, data = .) %>% broom::augment(new_data = hospitalization %>% filter(date > lo_cutoff_date)) %>%
    ggplot +
    aes(daily_hospital_occupancy, .fitted) + geom_line() + 
    geom_point(aes(y = daily_icu_occupancy), color = "gray70") +
    scale_x_continuous(limits = c(0, NA), labels = scales::comma_format()) +
    scale_y_continuous(limits = c(0, NA), labels = scales::comma_format())  +
    geom_point(data = hospitalization %>% filter(country == "United States", date > hi_cutoff_date), aes(daily_hospital_occupancy, daily_icu_occupancy), color = "darkgreen") + 
    labs(x = "Hospital beds in use",
         y = "ICU beds in use",
         caption = trainingcomment)


hospitalization %>%
    filter(country == "United States") %>%
    filter(date > lo_cutoff_date, date < hi_cutoff_date) %>%
    lm(daily_icu_occupancy ~ daily_hospital_occupancy + 0, data = .) %>% broom::tidy() %>% pull(estimate) -> icu_ratio

trainingcomment <- paste0("Training data between ",
                         format(lo_cutoff_date, format = "%b %d, %Y"),
                         " and ",
                         format(hi_cutoff_date, format = "%b %d, %Y"),
                         ".")


hospitalization %>%
    filter(country == "United States") %>%
    filter(date > lo_cutoff_date) %>%
    ggplot + 
    aes(x = date, y = daily_hospital_occupancy) + 
    geom_point(color = "blue") + 
    geom_point(aes(y = daily_icu_occupancy / icu_ratio), color = "red") + 
    scale_x_date(date_breaks = "2 month", date_label = "%b %Y") + 
    scale_y_continuous(labels = scales::comma_format(), 
                       limit = c(0, NA),
                       sec.axis = sec_axis(~ . * icu_ratio,
                                           name = "ICU beds in use",
                                           labels = scales::comma_format()))  +
    labs(x = "Date",
         y = "Hospital beds in use",
         caption = trainingcomment) + 
    geom_vline(xintercept = lo_cutoff_date, lty = 2, color = "gray50") +
    geom_vline(xintercept = hi_cutoff_date, lty = 2, color = "gray50")

ggsave("hospitalizations/ratio-use-of-beds.png", width = 6, height = 6)