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
    mutate(covid_19_community_level = factor(covid_19_community_level,
        levels = c("low", "medium", "high")
    ))


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
    ggplot() +
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
    geom_polygon(
        aes(
            x = long,
            y = lat,
            fill = covid_19_community_level,
            group = group
        ),
    ) +
    scale_fill_manual(values = levelcolor) +
    theme(legend.position = "none") +
    coord_fixed(1.4)

scplot <-
    ggplot(data = statemapdata %>% filter(state == "south carolina")) +
    geom_polygon(
        aes(
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

ggsave("healthdata/us_map_covidlevel.png", width = 6, height = 12,
    plot = allstates / scplot)

healthdata %>%
    group_by(county) %>%
    slice_max(date_updated) %>%
    ungroup() %>%
    count(covid_19_community_level) %>%
    ggplot() +
    aes(covid_19_community_level, n, fill = covid_19_community_level) +
    scale_fill_manual(values = levelcolor) +
    geom_col()

country_health_data <-
    healthdata %>%
    mutate(date = as.Date(date_updated)) %>%
    mutate(
        total_covid_cases = covid_cases_per_100k / 1e5 * county_population,
        total_hospital_admissions = covid_hospital_admissions_per_100k / 1e5 * county_population # nolint
    ) %>%
    group_by(date) %>%
    summarize(
        across(
            c(starts_with("total"), county_population),
            ~ sum(.x, na.rm = TRUE)
        ),
        .groups = "drop"
    ) %>%
    mutate(across(starts_with("total"),
        ~ .x / county_population * 1e5,
        .names = "{col}_per100k"
    ))

scale_fac <- 20

country_health_data %>%
    ggplot() +
    aes(date, total_hospital_admissions_per100k) +
    geom_line(lty = 3) +
    geom_line(aes(y = total_covid_cases_per100k / scale_fac), lty = 1) +
    scale_x_date(date_labels = "%b %Y") +
    scale_y_continuous(
        limits = c(0, NA),
        sec.axis = sec_axis(~ . * scale_fac,
            name = "Total new cases per week per 100k"
        )
    ) +
    labs(
        x = NULL,
        y = "New hospital admission per week per 100k",
        title = "USA-Wide COVID assessment"
    ) +
    geom_hline(yintercept = 10, color = "darkred", alpha = .5, lty = 2) +
    annotate("label",
        x = lubridate::ymd(20220510),
        y = 8.5,
        label = "New cases\n----->"
    ) +
    annotate("label",
        x = lubridate::ymd(20220525),
        y = 6.5,
        label = "New hospitalizations\n<-----"
    )

ggsave("healthdata/country-wide-assessment.png", width = 8, height = 5)

covid_state_data <-
    healthdata %>%
    mutate(date = as.Date(date_updated)) %>%
    select(
        county,
        state,
        date,
        county_population,
        covid_inpatient_bed_utilization,
        covid_cases_per_100k,
        covid_hospital_admissions_per_100k,
        covid_19_community_level
    ) %>%
    relocate(county, state, covid_19_community_level) %>%
    arrange(date) %>%
    mutate(across(ends_with("per_100k"),
        ~ . * county_population / 1e5,
        .names = "{col}_total"
    )) %>%
    group_by(date, state) %>%
    summarize(
        covid_cases = sum(covid_cases_per_100k_total),
        covid_hospital = sum(covid_hospital_admissions_per_100k_total),
        state_population = sum(county_population),
        .groups = "drop"
    ) %>%
    mutate(across(starts_with("covid"), ~ . / state_population * 1e5))

covid_state_data %>%
    ggplot() +
    aes(date, covid_cases, group = state) +
    geom_line(lty = 3, alpha = .5) +
    scale_x_date(date_labels = "%b %Y") +
    coord_cartesian(ylim = c(0, 500)) +
    labs(
        x = NULL,
        y = "Total new cases\nper week per 100k",
        title = "USA-Wide COVID assessment"
    ) +
    geom_smooth(aes(group = NULL), se = FALSE)

covid_assess <- function(covid, hospital) {
    if (is.na(covid) | is.na(hospital)) {
        return(NA)
    }

    ass <- "no"

    if (covid > 200 & hospital > 10) {
        ass <- "high"
    } else if (covid > 200 & hospital < 10) {
        ass <- "medium"
    } else if (covid < 200 & hospital < 15) {
        ass <- "medium"
    } else if (covid < 200 & hospital > 15) {
        ass <- "high"
    } else if (covid < 200 & hospital < 10) ass <- "low"

    return(ass)
}

state_latest <-
    covid_state_data %>%
    group_by(state) %>%
    slice_max(date, n = 1) %>%
    mutate(assessment = covid_assess(covid_cases, covid_hospital)) %>%
    mutate(state = tolower(state))

statemapdata <- as_tibble(map_data("state")) %>%
    rename(state = region, county = subregion) %>%
    inner_join(state_latest, by = "state")

allusa <-
    ggplot(data = statemapdata) +
    geom_polygon(
        aes(
            x = long,
            y = lat,
            fill = assessment,
            group = group
        ),
    ) +
    scale_fill_manual(values = levelcolor) +
    theme(legend.position = "none") +
    coord_fixed(1.4)

ggsave("healthdata/us-map-covid.png", plot = allusa)
