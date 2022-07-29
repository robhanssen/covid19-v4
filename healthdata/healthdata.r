# healthdata
library(tidyverse)
library(patchwork)
theme_set(theme_light())

source_origin <- "https://data.cdc.gov/resource/3nnm-4jni.csv?county_fips="

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
            date_updated,
            covid_cases_per_100k,
            covid_hospital_admissions_per_100k,
            covid_inpatient_bed_utilization,
            covid_19_community_level
        ) %>%
        mutate(date = as.Date(date_updated)) %>%
        select(-date_updated)

    health_summary %>%
        mutate(county = countyinfo$county) %>%
        relocate(county)
}

j <-
    map_df(seq_len(nrow(counties)), ~ countystatus(counties[.x, ])) %>%
    group_by(county) %>% # slice_max(date, n = 3) %>%
    relocate(county, covid_19_community_level, date) %>%
    arrange(county, date)

max_date <- max(j$date)

p1 <-
    j %>%
    ggplot() +
    aes(date, covid_cases_per_100k, color = county) +
    geom_line() +
    geom_hline(yintercept = c(200), lty = 2) +
    labs(
        x = "Date", y = "Cases per 100,000",
        title = paste0("COVID-19 risk assessment: ",
                        format(max_date, format = "%b %d, %Y"))
    ) +
    theme(legend.position = "none") +
    facet_wrap(~county, ncol = 3)

p2 <-
    j %>%
    ggplot() +
    aes(date, covid_hospital_admissions_per_100k, color = county) +
    geom_line() +
    geom_hline(yintercept = c(10, 15), lty = 2) +
    labs(x = "Date", y = "Hospital admission per 100,000") +
    theme(legend.position = "none") +
    facet_wrap(~county, ncol = 3)

p3 <-
    j %>%
    ggplot() +
    aes(date, covid_inpatient_bed_utilization, color = county) +
    geom_line() +
    geom_hline(yintercept = c(10, 15), lty = 2) +
    labs(x = "Date", y = "Bed utilization") +
    theme(legend.position = "none") +
    facet_wrap(~county, ncol = 3)

plot <- p1 / p2 / p3

ggsave("healthdata/healthdata.png", height = 8, width = 8, plot = plot)

write_csv(j %>% slice_max(date), "healthdata/countyinfo.csv")

levelcolor <- c(
    "Low" = "darkgreen",
    "Medium" = "orange",
    "High" = "red"
)

j %>%
    mutate(level = factor(covid_19_community_level,
        level = c("Low", "Medium", "High")
    )) %>%
    filter(!is.na(level)) %>%
    ggplot(aes(x = date, y = level)) +
    geom_point(size = 2, aes(color = level)) +
    facet_wrap(~county) +
    scale_color_manual(values = levelcolor)

ggsave("healthdata/levelchart.png", width = 9, height = 6)