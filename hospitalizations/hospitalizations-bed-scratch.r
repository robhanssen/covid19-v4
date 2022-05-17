library(tidyverse)
library(lubridate)
library(zoo)
library(scales)
library(patchwork)
theme_set(theme_light())

lo_cutoff_date <- as.Date("2019-04-01")
hi_cutoff_date <- today() #as.Date("2021-11-01")

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

reg_hosp <-
    hospitalization %>%
    filter(date > lo_cutoff_date) %>%
    filter(country == "United States")

averaging_window <- 7
deaths_shift <- 0
hd_conversion <- .2

    hospitalization %>%
    filter(date > lo_cutoff_date) %>%
    filter(country == "United States") %>%
    ggplot +
    aes(x = date, y = daily_hospital_occupancy) +
    geom_point() +
    scale_x_date(date_breaks = "2 month", date_labels = "%b\n%Y") +
    scale_y_continuous(labels = comma_format(),
                       breaks = 1e4 * 0:100,
                       limits = c(0, NA),
                       sec.axis = sec_axis(~ . / hd_conversion,
                                           name = "COVID-19 deaths",
                                           breaks = 500 * 0:100,
                                           labels = scales::comma_format())
                       ) +
    labs(x = "Date",
         y = "Hospital beds in use for COVID-19",
         title = "COVID-19 hospitalization and deaths in the United States",
         caption = paste0("Deaths (in red; 7-day daily mean) are shifted back by ", deaths_shift, " days and peak height is increased by factor ", round(hd_conversion, 2),"\n",sources)) + 
    geom_point(data = casesdeaths,
               aes(x = date - days(deaths_shift),
                   y = hd_conversion * zoo::rollmean(cases, averaging_window, na.pad = TRUE)),
               color = "red") + 
    geom_vline(xintercept = lo_cutoff_date, lty = 2, color = "gray50") +
    geom_vline(xintercept = hi_cutoff_date, lty = 2, color = "gray50") + 
    annotate("label", x = lo_cutoff_date + (hi_cutoff_date - lo_cutoff_date)/2 , y = 0, label = "training data", vjust = 0)

ggsave(paste0("hospitalizations/us-modelfit-cases-0.png"), width = 10, height = 10)

#
# Periods: 
# 7/15/2020 - 10/1/2020
# 10/1/2020 - 7/1/2021
# 7/1/2021 - 11/1/2021
# 11/1/2021 - TODAY


period_boundary <- c(as.Date(c("2020-07-15","2020-10-01","2021-07-01","2021-11-01", "2021-12-28", "2022-04-15")), today())

x <- tibble(pb = period_boundary, period = interval(lag(pb), (pb))) %>% filter(pb != first(period_boundary)) %>% select(period)

y <- tibble(pb = period_boundary, lo_cutoff_date = lag(pb), hi_cutoff_date = pb) %>%
        filter(!is.na(lo_cutoff_date)) %>%
        select(!pb)

for (t in seq_len(nrow(y))) {

    lo_cutoff_date <- y$lo_cutoff_date[t]
    hi_cutoff_date <- y$hi_cutoff_date[t]

    ts <- 1:30
    rsc <- rep(0, length(ts))

    for (time_shift in ts) {
        reg_death <-
            us_casesdeaths %>%
            group_by(date) %>%
            summarize(cases = sum(cases),
                    deaths = sum(deaths),
                    .groups = "drop") %>%
            filter(date > lo_cutoff_date) %>%
            mutate(date = date + time_shift) %>%
            mutate(deaths_av = zoo::rollmean(cases, 7, na.pad = TRUE))

        reg_data <-
            inner_join(reg_hosp, reg_death, by = "date")

        rs  <- lm(deaths_av ~ daily_hospital_occupancy + 0,
                data = reg_data %>% filter(date > lo_cutoff_date, date < hi_cutoff_date)) %>%
                broom::glance() %>%
                pull(r.squared)

        rsc[time_shift] <- rs
    }

    optimal_ts <- which(rsc == max(rsc))

    reg_death <-
        us_casesdeaths %>%
        group_by(date) %>%
        summarize(cases = sum(cases),
                deaths = sum(deaths),
                .groups = "drop") %>%
        filter(date > lo_cutoff_date) %>%
        mutate(date = date + optimal_ts) %>%
        mutate(deaths_av = zoo::rollmean(cases, 7, na.pad = TRUE))

    reg_data <-
        inner_join(reg_hosp, reg_death, by = "date")

    fct  <- lm(deaths_av ~ daily_hospital_occupancy + 0,
                data = reg_data %>% filter(date > lo_cutoff_date, date < hi_cutoff_date)) %>%
                broom::tidy() %>%
                filter(term == "daily_hospital_occupancy") %>%
                pull(estimate)

    fct <- 1 / fct

    rsq  <- lm(deaths_av ~ daily_hospital_occupancy + 0,
                data = reg_data %>% filter(date > lo_cutoff_date, date < hi_cutoff_date)) %>%
                broom::glance() %>%
                pull(r.squared)


    fit_summary <- paste0("Date shift: ", optimal_ts, "\nFactor:", round(fct, 3), "\nr.squared: ", round(rsq, 3))


    datafit <-
        reg_data %>%
        mutate(color = case_when(date > hi_cutoff_date ~ "test", TRUE ~ "training")) %>%
        ggplot +
        aes(daily_hospital_occupancy, deaths_av, color = color) +
        geom_point() +
        geom_abline(slope = 1/fct, intercept = 0) + 
        scale_color_manual(values = c("training" = "blue", "test" = "red")) +
        labs(caption = "Data in blue is used as training data.\nData in red is test data") + 
        annotate("label", x = 0, y = 18000, label = fit_summary, hjust = 0) +
        theme(legend.position = "none") + 
        scale_x_continuous(labels = scales::comma_format(), limits = c(0, NA)) + 
        scale_y_continuous(labels = scales::comma_format(), limits = c(0, NA))


    hd_conversion <- fct
    deaths_shift <- optimal_ts
    averaging_window <- 7

    timeplot <-
        hospitalization %>%
        filter(date > lo_cutoff_date) %>%
        filter(country == "United States") %>%
        ggplot +
        aes(x = date, y = daily_hospital_occupancy) +
        geom_point() +
        scale_x_date(date_breaks = "2 month", date_labels = "%b\n%Y") +
        scale_y_continuous(labels = comma_format(),
                        breaks = 1e5 * 0:100,
                        limits = c(0, NA),
                        sec.axis = sec_axis(~ . / hd_conversion,
                                            name = "COVID-19 cases",
                                            breaks = 1e5 * 0:100,
                                            labels = scales::comma_format())
                        ) +
        labs(x = "Date",
            y = "Hospital beds in use for COVID-19",
            title = "COVID-19 hospitalization and cases in the United States",
            caption = paste0("Cases (in red; 7-day daily mean) are shifted back by ", deaths_shift, " days and peak height is increased by factor ", round(hd_conversion, 2),"\n",sources)) + 
        geom_point(data = casesdeaths,
                aes(x = date + days(deaths_shift),
                    y = hd_conversion * zoo::rollmean(cases, averaging_window, na.pad = TRUE)),
                color = "red") + 
        geom_vline(xintercept = lo_cutoff_date, lty = 2, color = "gray50") +
        geom_vline(xintercept = hi_cutoff_date, lty = 2, color = "gray50") + 
        annotate("label", x = lo_cutoff_date + (hi_cutoff_date - lo_cutoff_date)/2 , y = 0, label = "training data", vjust = 0)

    #ggsave("hospitalizations/us-hosp-vs-deaths.png", width = 8, height = 6, plot = timeplot)

    rsqplot <-
        tibble(x = ts, y = rsc) %>%
        ggplot() +
        aes(x, y) +
        aes(x, y) +
        geom_line() +
        geom_point() +
        scale_x_continuous(breaks = seq(0, max(ts), 10)) +
        scale_y_continuous(breaks = 0.1 * 0:10, limits = c(NA, 1.01)) +
        labs(x = "Date shift (in days)",
            y = "R.squared of regression line") + 
        geom_point(data = tibble(x = optimal_ts, y = rsq), shape = 3, size = 15, color = "red")

    combined <- timeplot / (rsqplot + datafit)

    ggsave(paste0("hospitalizations/us-modelfit-cases-",t,".png"), width = 10, height =10 , plot = combined)


}
    