# healthdata
library(tidyverse)

source_origin <- "https://healthdata.gov/resource/di4u-7yu6.csv?fips="

load("Rdata/county_data.Rdata")

countylist <- tribble(
    ~county, ~state,
    "Spartanburg", "South Carolina",
    "Greenville", "South Carolina",
    "Alachua", "Florida"
)

counties <-
    countylist %>%
    left_join(county_data) %>%
    distinct(state, county, population, fips) %>%
    mutate(source = paste0(source_origin, fips))


countystatus <- function(countyinfo) {
    healthdata <- read_csv(countyinfo$source) %>%
        janitor::clean_names()

    health_summary <-
        healthdata %>%
        select(
            date,
            cases_per_100k_last_7_days,
            confirmed_covid_hosp_last_7_days,
            suspected_covid_hosp_last_7_days,
            confirmed_covid_hosp_last_7_days,
            pct_inpatient_beds_used_covid_avg_last_7_days
        ) %>%
        mutate(hosp_per_100k = (confirmed_covid_hosp_last_7_days + suspected_covid_hosp_last_7_days) / countyinfo$population * 1e5) %>%
        select(
            -suspected_covid_hosp_last_7_days,
            -confirmed_covid_hosp_last_7_days
        ) %>%
        mutate(across(starts_with("pct"), .fns = ~ .x * 100)) %>%
        rename(
            "Cases per 100,000 population (7 days)" = cases_per_100k_last_7_days,
            "%In-patient beds in use for COVID-19 (Average, 7 days)" = pct_inpatient_beds_used_covid_avg_last_7_days,
            "Hospitalizations per 100,000 population (7 days)" = hosp_per_100k
        ) %>%
        pivot_longer(!date, names_to = "key", values_to = "value") %>%
        mutate(value = round(value, 2))

    health_summary %>%
        mutate(assessment = case_when(
            key == "Cases per 100,000 population (7 days)" ~ cut(value, c(0, 200, 1e6), c("Low", "High")),
            key == "%In-patient beds in use for COVID-19 (Average, 7 days)" ~ cut(value, c(0, 10, 15), c("Low", "High")),
            key == "Hospitalizations per 100,000 population (7 days)" ~ cut(value, c(0, 10, 80), c("Low", "High"))
        )) %>%
        mutate(county = countyinfo$county) %>%
        relocate(county)
}

j <-
    map_df(seq_len(nrow(counties)), ~ countystatus(counties[.x, ])) %>%
    relocate(county, assessment, key, value, date)

write_csv(j, "healthdata/countyinfo.csv")