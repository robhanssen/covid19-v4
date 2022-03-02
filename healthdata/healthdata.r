library(tidyverse)

spa_source <- "https://healthdata.gov/resource/di4u-7yu6.csv?fips=45083"
gre_source <- "https://healthdata.gov/resource/di4u-7yu6.csv?fips=45045"

load("Rdata/us_casesdeaths.Rdata")

spa_countypop <-
    us_casesdeaths %>% distinct(county, population) %>%
    filter(county == "Spartanburg") %>%
    pull(population)

gre_countypop <-
    us_casesdeaths %>% distinct(state, county, population) %>%
    filter(county == "Greenville" & state == "South Carolina") %>%
    pull(population)

healthdata <- read_csv(spa_source) %>%
    janitor::clean_names()

health_summary <-
    healthdata %>%
    select(date,
           cases_per_100k_last_7_days,
           confirmed_covid_hosp_last_7_days,
           suspected_covid_hosp_last_7_days,
           confirmed_covid_hosp_last_7_days,
           pct_inpatient_beds_used_covid_avg_last_7_days
           ) %>% 
         mutate(hosp_per_100k = (confirmed_covid_hosp_last_7_days + suspected_covid_hosp_last_7_days)/spa_countypop * 1e5) %>%
      select(-suspected_covid_hosp_last_7_days,
             -confirmed_covid_hosp_last_7_days) %>%
      mutate(across(starts_with("pct"), .fns = ~.x * 100)) %>%
      rename("Cases per 100,000 population (7 days)" = cases_per_100k_last_7_days,
             "%In-patient beds in use for COVID-19 (Average, 7 days)" = pct_inpatient_beds_used_covid_avg_last_7_days,
             "Hospitalizations per 100,000 population (7 days)" = hosp_per_100k) %>%
             pivot_longer(!date, names_to = "key", values_to = "value") %>%
      mutate(value = round(value, 2))

health_summary %>%
    mutate(assessment = case_when(key == "Cases per 100,000 population (7 days)" ~ cut(value, c(0,200,1e6), c("Low","High")),
                                  key == "%In-patient beds in use for COVID-19 (Average, 7 days)" ~ cut(value, c(0,10,15), c("Low","High")),
                                  key == "Hospitalizations per 100,000 population (7 days)" ~ cut(value, c(0,10,20), c("Low","High"))
                                  )
            ) %>%
    write_csv("healthdata/heath_data_SPA.csv")




healthdata <- read_csv(gre_source) %>%
    janitor::clean_names()

health_summary <-
    healthdata %>%
    select(date,
           cases_per_100k_last_7_days,
           confirmed_covid_hosp_last_7_days,
           suspected_covid_hosp_last_7_days,
           confirmed_covid_hosp_last_7_days,
           pct_inpatient_beds_used_covid_avg_last_7_days
           ) %>% 
         mutate(hosp_per_100k = (confirmed_covid_hosp_last_7_days + suspected_covid_hosp_last_7_days)/gre_countypop * 1e5) %>%
      select(-suspected_covid_hosp_last_7_days,
             -confirmed_covid_hosp_last_7_days) %>%
      mutate(across(starts_with("pct"), .fns = ~.x * 100)) %>%
      rename("Cases per 100,000 population (7 days)" = cases_per_100k_last_7_days,
             "%In-patient beds in use for COVID-19 (Average, 7 days)" = pct_inpatient_beds_used_covid_avg_last_7_days,
             "Hospitalizations per 100,000 population (7 days)" = hosp_per_100k) %>%
             pivot_longer(!date, names_to = "key", values_to = "value") %>%
      mutate(value = round(value, 2))

health_summary %>%
    mutate(assessment = case_when(key == "Cases per 100,000 population (7 days)" ~ cut(value, c(0,200,1e6), c("Low","High")),
                                  key == "%In-patient beds in use for COVID-19 (Average, 7 days)" ~ cut(value, c(0,10,15,100), c("Low","Medium", "High")),
                                  key == "Hospitalizations per 100,000 population (7 days)" ~ cut(value, c(0,10,20,100), c("Low","Medium","High"))
                                  )
            ) %>%
    write_csv("healthdata/heath_data_GRE.csv")

