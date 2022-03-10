# carnaval
library(tidyverse)
library(lubridate)
library(zoo)
theme_set(theme_minimal())

load("Rdata/global_casesdeaths.Rdata")

locations <- read_csv("sources/countryinformation.csv")

# summarize country info
casesdeaths <-
        global_casesdeaths %>%
        rename(province = "Province/State", country = "Country/Region") %>%
        group_by(country, date, time) %>%
        summarize(deaths = sum(deaths), cases = sum(cases)) %>%
        full_join(locations, by = c(country = "region"))

correction <- 50
avdays <- 7
totalcasecomment <- ""
capt <- ""

# pick one
selected_countries <- c("Netherlands", "Belgium", "Switzerland", "Austria", "Germany")
selected_country <- paste(selected_countries, collapse = ", ")

carnaval <- as.Date(c("2022-02-27"))

casesdeathsbylocation <-
        casesdeaths %>%
        filter(country %in% selected_countries) %>%
        group_by(date) %>%
        summarize(
                population = sum(population),
                cases = sum(cases),
                deaths = sum(deaths),
                casesper100k = cases / population * 1e5,
                deathsper100k = deaths / population * 1e5,
                .groups = "drop"
        )

ylimit_max <- (max(casesdeathsbylocation$casesper100k) %/% 10 + 1) * 10

ylimit_max <- ifelse(ylimit_max > 150, ylimit_max, 150)

casesdeathsbylocation %>%
        filter(date > today() %m-% months(4)) %>%
        ggplot() +
        aes(date, casesper100k) +
        geom_line(color = "blue", linetype = "dotted") +
        geom_line(aes(y = rollmean(casesper100k, avdays, na.pad = TRUE)),
                size = 2,
                color = "blue"
        ) +
        scale_y_continuous(
                limit = c(0, ylimit_max),
                breaks = c(0:100 * 50)
        ) +
        scale_x_date(date_breaks = "1 months", date_labels = "%b %Y") +
        labs(
                caption = capt,
                x = "Date",
                y = "Daily number of confirmed cases per 100k population",
                title = paste(
                        selected_country, "daily cases with",
                        avdays,
                        "days average line"
                )
        ) +
        geom_vline(xintercept = carnaval, lty = 1) +
        annotate("label", x = as.Date("2022-02-27"), y = 50, label = "Carnaval 2022")

ggsave("misc/carnaval.png", width = 8, height = 6)