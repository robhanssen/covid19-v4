library(tidyverse)
library(lubridate)
library(patchwork)
library(scales)
theme_set(theme_light())

load("./Rdata/us_casesdeaths.Rdata")

inaug <- as.Date("2021-01-20")

casesdeaths <-
    us_casesdeaths %>%
    group_by(date) %>%
    summarize(cases = sum(cases),
              deaths = sum(deaths),
              .groups = "drop") %>%
    mutate(president  = case_when(date < inaug ~ "Trump", TRUE ~ "Biden"))
    
scaling <- 50

summry <-
    casesdeaths %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    summarize(cases = sum(cases),
              deaths = sum(deaths),
              .groups = "drop") %>%
    mutate(across(!year, scales::comma_format(trim = FALSE))) %>%
    mutate(text = paste(year, cases, deaths, collapse = "\n", sep = "   ")) %>%
    filter(year == 2020) %>% 
    pull(text)

year_markers = as.Date("2020-01-01") + years(0:10)

casesdeaths %>%
    mutate(ccases = cumsum(cases),
           cdeaths = cumsum(deaths)) %>%
    ggplot +
    aes(x = date) +
    geom_line(aes(y = ccases), color = "blue") +
    geom_line(aes(y = scaling * cdeaths), color = "red") +
    scale_x_date(date_breaks = "3 month", date_labels = "%b\n%Y") +
    scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = " M"),
                       breaks = 10e6 * 0:100,
                       name = "Cumulative COVID-19 cases",
                       sec.axis = sec_axis(~ . / scaling,
                                           name = "Cumulative COVID-19 deaths",
                                           labels = scales::comma_format(scale = 1e-3, suffix = " K"),
                                           breaks = 1e5 * 0:100)
                       ) +
    geom_vline(xintercept = year_markers, lty = 2, color = "gray50") +
    annotate("label",
             x = today() - weeks(40),
             y = 10e6,
             label = summry,
             hjust = 0,
             fill = "white")

ggsave("misc/us-cumulative-cases-deaths.png", width = 8, height = 6)

current_year = year(today())

annual <-
    casesdeaths %>%
    mutate(year = year(date),
           year_diff = current_year - year,
           norm_date = date + years(year_diff)) %>%
    group_by(year) %>%
    summarize(norm_date = norm_date,
              cases = cumsum(cases),
              deaths = cumsum(deaths),
              .groups = "drop")
    
annual_case_graph <-
    annual %>%
    ggplot + 
    aes(x = norm_date, color = factor(year)) +
    geom_line(aes(y = cases)) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b",
                 minor_breaks = NULL,
                 name = "Date") +
    scale_y_continuous(labels = scales::comma_format(),
                       breaks = 10e6 * 0:100,
                       name = "Cumulative COVID-19 cases") +
    labs(color = "Year")

annual_death_graph <-
    annual %>%
    ggplot + 
    aes(x = norm_date, color = factor(year)) +
    geom_line(aes(y = deaths)) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b",
                 minor_breaks = NULL,
                 name = "Date") +
    scale_y_continuous(labels = scales::comma_format(),
                       breaks = 1e5 * 0:100,
                       name = "Cumulative COVID-19 cases") +
    labs(color = "Year")

annual_graphs <- annual_case_graph + annual_death_graph
ggsave("misc/us-cases-deaths-by-year.png", width = 12, height = 6, plot = annual_graphs)