library(tidyverse)
library(lubridate)
library(zoo)
source("config.r")

load("Rdata/us_casesdeaths.Rdata")

today <- lubridate::today()

# assign the region in the US to all locations
locations <- read_csv("sources/USstateslist.csv")

us_casesdeaths <-
        us_casesdeaths %>%
        full_join(locations) %>%
        mutate(location = case_when(is.na(location) ~ "Other",
                                    TRUE ~ location))

correction <- 60
avdays <- 7
capt <- "insert caption here"

casesdeaths <-
        us_casesdeaths %>%
        group_by(date) %>%
        summarize(population = sum(population),
                  cases = sum(cases),
                  deaths = sum(deaths),
                  casesper100k = cases / population * 1e5,
                  deathsper100k = deaths / population * 1e5,
                  .groups = "drop")

totalcases = sum(casesdeaths$cases, na.rm=T)
totaldeaths <- sum(casesdeaths$deaths, na.rm = TRUE)
totalcasecomment <- paste("Total US cases:",
                         totalcases,
                         "\nTotal casualties:",
                         totaldeaths)


full_us_graph <-
        casesdeaths %>%
        filter(date >= "2020-02-01") %>%
        ggplot +
        aes(date, casesper100k) +
        geom_line(color = "blue", linetype = "dotted") +
        geom_line(aes(y = rollmean(casesper100k, avdays, na.pad = TRUE)),
                  size = 2,
                  color = "blue") +
        scale_y_continuous(breaks = c(0, 2, 5, 10, 20, 50, 100, 150),
                           sec.axis = sec_axis(~ . / correction,
                                               breaks = seq(0, 5, 1))) +
        scale_x_date(date_breaks = "3 months",
                     date_labels = "%b %d") +
        labs(x = "Date",
             y = "Daily incremental number of confirmed cases or deaths",
             title = paste("US daily cases and deaths with",
                           avdays,
                           "days average line"),
             caption = capt) +
        geom_line(aes(date, correction * deathsper100k),
                  color = "red",
                  linetype = "dotted") +
        geom_line(aes(y = rollmean(correction * deathsper100k,
                      avdays,
                      na.pad = TRUE)),
                  size = 2,
                  color = "red") +
        annotate("text",
                 x = as.Date("2020-03-28"),
                 y = 70,
                 label = totalcasecomment,
                 color = "black")

#ggsave("graphs/covid19-us-cases-and-death.pdf")


casemodel <- function(tbl) {
        lm(data = tbl, cases ~ date)
}

deathsmodel <- function(tbl) {
        lm(data = tbl, deaths ~ date)
}

us_statelevel <-
        us_casesdeaths %>%
        filter(date >= today - weeks(3)) %>%
        group_by(state, date) %>%
        summarize(cases = sum(cases),
                  deaths = sum(deaths),
                  .groups = "drop")

models <-
        us_statelevel %>%
        group_by(state) %>%
        nest() %>%
        mutate(casemodel = map(data, casemodel),
               deathsmodel = map(data, deathsmodel)
                )

models %>% ungroup() %>% mutate(qual = map(deathsmodel, broom::augment)) %>% unnest(qual) %>% 
        ggplot(aes(date, .fitted, color = state)) + geom_line()

models %>%
        mutate(casedata = map(casemodel, broom::augment)) %>%
        unnest(casedata) %>%
        ggplot +
        aes(date, .fitted) + 
        geom_line()  +
        geom_point(aes(y = cases, x = date))



