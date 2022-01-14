library(tidyverse)
library(lubridate)
library(zoo)
library(broom)
theme_set(theme_light())

load("Rdata/us_casesdeaths.Rdata")

# define constants
firstdeath <- min(us_casesdeaths[with(us_casesdeaths, which(deaths>0)),]$date)
avdays <- 28
scale_factor <- 25
shift_factor <- 2e5
filter_date <- as.Date("2020-08-01")

us_casesdeaths %>%
        filter(date > filter_date) %>%
        group_by(date) %>%
        summarize(cases = sum(cases)) %>%
        mutate(secdiff = cases - lag(cases)) %>%
        ggplot +
            aes(x = date, y = secdiff) +
            geom_line(color = "blue", aes(y = rollmean(secdiff, avdays, na.pad = TRUE))) +
            geom_line(color = "red", aes(y = rollmean((cases + shift_factor) / scale_factor, avdays, na.pad = TRUE))) +
            scale_x_date(date_breaks = "2 months", date_label = "%b %Y") +
            scale_y_continuous(sec.axis = sec_axis(~ . * scale_factor - shift_factor, label = scales::comma_format()),
                               label = scales::comma_format()
                                ) +
            labs(x = "Date",
                y = "Cumulative cases (2nd derivative)",
                title = paste0(avdays, "-day rolling second derivative of cumulative cases in the United States")
                )


shift_factor2 <- 3000

us_casesdeaths %>%
        filter(date > filter_date) %>%
        group_by(date) %>%
        summarize(deaths = sum(deaths)) %>%
        mutate(secdiff = deaths - lag(deaths)) %>%
        ggplot +
            aes(x = date, y = secdiff) +
            geom_line(color = "blue", aes(y = rollmean(secdiff, avdays, na.pad = TRUE))) +
            geom_line(color = "red", aes(y = rollmean((deaths + shift_factor2) / scale_factor, avdays, na.pad = TRUE))) +
            scale_x_date(date_breaks = "3 months", date_label = "%b %Y") +
            scale_y_continuous(label = scales::comma_format(),
                               sec.axis = sec_axis((~ . * scale_factor - shift_factor2))) +
            labs(x = "Date",
                y = "Cumulative deaths (2nd derivative)",
                title = paste0(avdays, "-day rolling second derivative of cumulative deaths in the United States")
                )