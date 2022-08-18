# healthdata
library(tidyverse)
library(patchwork)
theme_set(theme_light())

load("Rdata/county_data.Rdata")

countylist <- tribble(
    ~county, ~state,
    "Spartanburg", "South Carolina",
    "Greenville", "South Carolina",
    "Alachua", "Florida"
)

levelcolor <- c(
    "low" = "darkgreen",
    "medium" = "orange",
    "high" = "red"
)

counties <-
    countylist %>%
    left_join(county_data) %>%
    distinct(state, county, population, fips)

source_origin <- "https://data.cdc.gov/resource/3nnm-4jni.csv?$limit=10000000"

healthdata <- read_csv(source_origin, na = "n/a") %>%
    janitor::clean_names() %>%
    mutate(covid_19_community_level = tolower(covid_19_community_level)) %>%
    mutate(covid_19_community_level = factor(covid_19_community_level, levels = c("low", "medium", "high")))


covid_levels <-
    healthdata %>%
    inner_join(counties, by = c("county_fips" = "fips")) %>%
    mutate(date = as.Date(date_updated)) %>%
    select(
        county = county.x,
        state = state.x,
        date,
        covid_inpatient_bed_utilization,
        covid_cases_per_100k,
        covid_hospital_admissions_per_100k,
        covid_19_community_level
    ) %>%
    relocate(county, state, covid_19_community_level)


write_csv(covid_levels %>% slice_max(date), "healthdata/countyinfo.csv")

max_date <- max(covid_levels$date)

p1 <-
    covid_levels %>%
    ggplot() +
    aes(date, covid_cases_per_100k, color = county) +
    geom_line() +
    geom_hline(yintercept = c(200), lty = 2) +
    labs(
        x = "Date", y = "Cases per 100,000",
        title = paste0(
            "COVID-19 risk assessment: ",
            format(max_date, format = "%b %d, %Y")
        )
    ) +
    theme(legend.position = "none") +
    facet_wrap(~county, ncol = 3)

p2 <-
    covid_levels %>%
    ggplot() +
    aes(date, covid_hospital_admissions_per_100k, color = county) +
    geom_line() +
    geom_hline(yintercept = c(10, 15), lty = 2) +
    labs(x = "Date", y = "Hospital admission per 100,000") +
    theme(legend.position = "none") +
    facet_wrap(~county, ncol = 3)

p3 <-
    covid_levels %>%
    ggplot() +
    aes(date, covid_inpatient_bed_utilization, color = county) +
    geom_line() +
    geom_hline(yintercept = c(10, 15), lty = 2) +
    labs(x = "Date", y = "Bed utilization") +
    theme(legend.position = "none") +
    facet_wrap(~county, ncol = 3)

plot <- p1 / p2 / p3

ggsave("healthdata/levelsovertime.png", width = 9, height = 9, plot = plot)

lvl <-
    covid_levels %>%
    ggplot +
    aes(x = date, y = covid_19_community_level) + 
    geom_point(aes(color = covid_19_community_level)) + 
    facet_wrap(~county) + 
    scale_color_manual(values = levelcolor) + 
    theme(legend.position = "none")

ggsave("healthdata/levelchart.png", width = 9, height = 9, plot = lvl)

level_by_county <-
    healthdata %>%
    group_by(county_fips) %>%
    slice_max(date_updated) %>%
    select(county_fips, county, state, covid_19_community_level) %>%
    mutate(across(county:state, tolower)) %>%
    mutate(county = str_remove(county, " county"))

statemapdata <- as_tibble(map_data("county")) %>%
    rename(state = region, county = subregion) %>%
    inner_join(level_by_county)

allstates <-
    ggplot(data = statemapdata) +
    geom_polygon(aes(
        x = long,
        y = lat,
        fill = covid_19_community_level,
        group = group
    ),
    # color = "white"
    ) +
    scale_fill_manual(values = levelcolor) +
    theme(legend.position = "none") +
    coord_fixed(1.4)

scplot <-
    ggplot(data = statemapdata %>% filter(state == "south carolina")) +
    geom_polygon(aes(
        x = long,
        y = lat,
        fill = covid_19_community_level,
        group = group
    ),
    color = "white"
    ) +
    scale_x_continuous(limit = c(-85, -77)) +
    scale_y_continuous(limit = c(32, 35.5)) +
    scale_fill_manual(values = levelcolor) +
    theme(legend.position = "none") +
    coord_fixed(1.4)

ggsave("healthdata/us_map_covidlevel.png", width = 6, height = 12, plot = allstates / scplot)

healthdata %>%
    group_by(county) %>%
    slice_max(date_updated) %>%
    ungroup() %>%
    count(covid_19_community_level) %>%
    ggplot + aes(covid_19_community_level, n, fill = covid_19_community_level) +
    scale_fill_manual(values = levelcolor) + geom_col()