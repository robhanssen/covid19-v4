library(tidyverse)
library(lubridate)

td <- today()
start <- as.Date("2020-01-20")
vax <- as.Date("2021-04-01")

ratio <- as.numeric(td - start) / as.numeric(td - vax)


load("Rdata/global_casesdeaths.Rdata")

casesdeath_byyear <-
    global_casesdeaths %>% 
    mutate(year = year(date)) %>%
    rename(country = "Country/Region") %>%
    group_by(country, year) %>%
    summarize(deaths = sum(deaths),
              cases = sum(cases),
              .groups = "drop") %>%
    pivot_wider(names_from = year, values_from = c(deaths, cases)) %>%
    filter(cases_2020 > 0) %>%
    filter(deaths_2020 >0) %>%
    mutate(case_ratio = cases_2021 / cases_2020, 
           death_ratio = deaths_2021 / deaths_2020)

casesdeath_byyear %>%
    mutate(color = case_when(country == "US" ~ "green", TRUE ~ "black")) %>%
    filter(deaths_2020 > 1e3) %>%
    slice_min(case_ratio, n = 50) %>%
    ggplot + 
    aes(fct_reorder(country, -case_ratio), case_ratio, fill = color)  +
    geom_col() + 
    geom_hline(yintercept = 1) +
    coord_flip() + 
    scale_fill_manual(values = c("green" = "darkgreen", "black" = "black"))
    

casesdeath_byyear %>%
    mutate(color = case_when(country == "US" ~ "green", TRUE ~ "black")) %>%
    filter(deaths_2020 > 1e3) %>%
    slice_min(death_ratio, n = 50) %>%
    ggplot + 
    aes(fct_reorder(country, -death_ratio), death_ratio, fill = color)  +
    geom_col() + 
    geom_hline(yintercept = 1) +
    coord_flip() + 
    scale_fill_manual(values = c("green" = "darkgreen", "black" = "black"))



casesdeath_byvax <-
    global_casesdeaths %>% 
    mutate(year = year(date)) %>%
    rename(country = "Country/Region") %>%
    mutate(vaxstatus = ifelse(date > vax, "vax", "novax")) %>%
    group_by(country, vaxstatus) %>%
    summarize(deaths = sum(deaths),
              cases = sum(cases),
              .groups = "drop") %>%
    pivot_wider(names_from = vaxstatus, values_from = c(deaths, cases)) %>%
    filter(cases_novax > 0) %>%
    filter(deaths_novax >0) %>%
    mutate(case_ratio = cases_vax / cases_novax * (ratio - 1), 
           death_ratio = deaths_vax / deaths_novax * (ratio - 1))


casesdeath_byvax %>%
    mutate(color = case_when(country == "US" ~ "green", TRUE ~ "black")) %>%
    filter(deaths_novax > 1e3) %>%
    slice_min(death_ratio, n = 50) %>%
    ggplot + 
    aes(fct_reorder(country, -death_ratio), death_ratio, fill = color)  +
    geom_col() + 
    geom_hline(yintercept = 1) +
    coord_flip() + 
    scale_fill_manual(values = c("green" = "darkgreen", "black" = "black"))


global_casesdeaths %>%
    rename(country = "Country/Region") %>%
    filter(country == "US") %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    summarize(deaths = sum(deaths),
              cases = sum(cases))

global_casesdeaths %>%
    rename(country = "Country/Region") %>%
    filter(country == "US") %>%
    mutate(prez = ifelse(date > as.Date("2021-01-20"), "Biden", "Trump")) %>%
    mutate(year = year(date)) %>%
    group_by(prez) %>%
    summarize(deaths = sum(deaths),
              cases = sum(cases))


us_data <-
    global_casesdeaths %>%
    rename(country = "Country/Region") %>%
    filter(country == "US") %>%
    mutate(prez = ifelse(date > as.Date("2021-01-20"), "Biden", "Trump")) %>%
    # group_by(prez) %>%
    mutate(cdeaths = cumsum(deaths),
           ccases = cumsum(cases))

labels <-
    global_casesdeaths %>%
    rename(country = "Country/Region") %>%
    filter(country == "US") %>%
    mutate(prez = ifelse(date > as.Date("2021-07-01"), "Biden", "Trump")) %>%
    group_by(prez) %>%
    summarize(total_cases = sum(cases), 
              total_deaths = sum(deaths),
              .groups = "drop") %>%
    mutate(date = as.Date(c("2021-10-01","2020-12-01")),
            ccases = c(550000,550000))


us_data %>%
    ggplot +
    aes(date, cdeaths, color = prez) + geom_point() + 
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
    scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K")) + 
    geom_vline(xintercept = as.Date("2021-07-01"), lty = 2, color = "gray50") + 
    labs(x = "Date",
         y = "Cumulative deaths attributed to COVID-19",
         caption = "Source: JHU") + 
    geom_label(data = labels, aes(color = prez, x = date, y = ccases, label = scales::comma(total_deaths))) + 
    scale_color_manual(values = c("Trump" = "red", "Biden" = "blue")) + 
    theme(legend.position = "none")

ggsave("misc/covid-deathtoll-by-president.png", width = 6, height = 6)

dc_ratio = 50

us_data %>%
    ggplot + 
    aes(date, ccases, color = prez) + geom_point() + 
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
    scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"), 
                        sec.axis = sec_axis(~ . / dc_ratio, breaks = 1e5 * seq(0,10,1), labels = scales::comma_format(scale = 1e-3, suffix = "K"))) + 
    # geom_vline(xintercept = as.Date("2021-04-01"), lty = 2, color = "gray50") + 
    scale_color_manual(values = c("Biden" = "blue", "Trump" = "red")) + 
    geom_line(aes(y = cdeaths * dc_ratio), lty = 2) + 
    labs(x = "Date", y = "Cumulative cases", color = "President")

