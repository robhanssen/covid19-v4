library(tidyverse)
library(lubridate)
library(broom)
library(patchwork)
source("config.r")
theme_set(theme_light())

# main data is called us    us_casesdeaths
load("Rdata/us_casesdeaths.Rdata")


statepop <- us_casesdeaths %>%
            # pivot_wider(c(state, county, population))
            distinct(state, county, population)

min_country_population <- 1
twoweeksago <- today() - days(14)
loglabels <- rep(c(1, 2, 5), 8) * 10 ^ (rep(1:8, each = 3))


absdeathsmodel <- function(tbl) {
        lm(deaths ~ time, data = tbl)
}

abscasesmodel <- function(tbl) {
        lm(cases ~ time, data = tbl)
}



deathsmodel <- function(tbl) {
        lm(deathsper100k ~ time, data = tbl)
}


casesmodel <- function(tbl) {
        lm(casesper100k ~ time, data = tbl)
}

dateformat <- function(d) {
    format(d, format = "%b %d")
}

# summarize country info
casesdeaths <-
        us_casesdeaths %>%
            group_by(state, county, date, time) %>%
            summarize(deaths = sum(deaths), cases = sum(cases)) %>%
            inner_join(statepop) %>%
            mutate(casesper100k = cases / population * 1e5,
                   deathsper100k = deaths / population * 1e5)

t0 <- Sys.time()

print("applying models")
uscases_twoweeks <-
    casesdeaths %>%
        filter(date >= twoweeksago) %>%
        filter(!is.na(population)) %>%
        filter(population > min_country_population) %>%
        mutate(countyid = paste(county, state, sep = ", ")) %>%
        arrange(countyid, date) %>%
        group_by(countyid) %>%
        nest() %>%
        mutate(deathmodel = map(data, deathsmodel),
               casemodel = map(data, casesmodel),
               absdeathmodel = map(data, absdeathsmodel),
               abscasemodel = map(data, abscasesmodel),
               )

print(paste("time spent", Sys.time() - t0, "\nextracting parameters 1"))

caseparameters <-
    uscases_twoweeks %>%
    mutate(caseparameters = map(casemodel, tidy),
           ) %>%
    unnest(caseparameters) %>%
    filter(term == "time") %>%
    ungroup()

casegraph <-
    caseparameters %>%
    slice_max(estimate, n = 20) %>%
    ggplot +
        aes(x = fct_reorder(countyid, estimate), y = estimate) +
        geom_col() +
        coord_flip() +
        labs(y = "Case growth estimate (per 100,000 population per day)",
             x = paste0("State (population over ", scales::comma(min_country_population), ")"),
             title = "COVID-19 growth over the last two weeks",
             subtitle = paste0("Period: ", dateformat(twoweeksago), " - ", dateformat(today())))

print(paste("time spent", Sys.time() - t0, "\nextracting parameters 2"))
deathparameters <-
    uscases_twoweeks %>%
    mutate(deathparameters = map(deathmodel, tidy),
           ) %>%
    unnest(deathparameters) %>%
    filter(term == "time") %>%
    ungroup()

deathgraph <-
    deathparameters %>%
    slice_max(estimate, n = 20) %>%
    ggplot +
        aes(x = fct_reorder(countyid, estimate), y = estimate) +
        geom_col() +
        coord_flip() +
        labs(y = "Death growth estimate (per 100,000 population per day)",
             x = paste0("State (population over ", scales::comma(min_country_population), ")"))

print(paste("time spent", Sys.time() - t0, "\nextracting parameters 3"))
abscaseparameters <-
    uscases_twoweeks %>%
    mutate(caseparameters = map(abscasemodel, tidy),
           ) %>%
    unnest(caseparameters) %>%
    filter(term == "time") %>%
    ungroup()

abscasegraph <-
    abscaseparameters %>%
    slice_max(estimate, n = 20) %>%
    ggplot +
        aes(x = fct_reorder(countyid, estimate), y = estimate) +
        geom_col() +
        coord_flip() +
        labs(y = "Case growth estimate (per day)",
             x = paste0("State (population over ", scales::comma(min_country_population), ")"))

print(paste("time spent", Sys.time() - t0, "\nextracting parameters 4"))
absdeathparameters <-
    uscases_twoweeks %>%
    mutate(deathparameters = map(absdeathmodel, tidy),
           ) %>%
    unnest(deathparameters) %>%
    filter(term == "time") %>%
    ungroup()

absdeathgraph <-
    absdeathparameters %>%
    slice_max(estimate, n = 20) %>%
    ggplot +
        aes(x = fct_reorder(countyid, estimate), y = estimate) +
        geom_col() +
        coord_flip() +
        labs(y = "Death growth estimate (per day)",
             x = paste0("State (population over ", scales::comma(min_country_population), ")"))

print(paste("time spent", Sys.time() - t0, "\ngraphing"))
(casegraph + deathgraph) / (abscasegraph + absdeathgraph)

ggsave("projections/covid19-highest-growth-uscounties.pdf", width = 11, height = 8)

uscases_twoweekav <-
    uscases_twoweeks %>%
        select(countyid, data) %>%
        unnest(data) %>%
    group_by(countyid) %>%
    summarize(cases = mean(cases),
              deaths = mean(deaths),
              casesper100k = mean(casesper100k),
              deathsper100k = mean(deathsper100k)
              )


G1 <-
    uscases_twoweekav %>%
    ungroup() %>%
    inner_join(deathparameters) %>%
    select(countyid, deathsper100k, estimate) %>%
    slice_max(estimate, n = 20) %>%
    ggplot +
        aes(x = deathsper100k, y = estimate, label = countyid) +
        geom_point() +
        ggrepel::geom_label_repel() +
        labs(x = "Deaths per 100,000 population",
             y = "Growth in deaths per 100,000 population ")

G2 <-
    uscases_twoweekav %>%
    ungroup() %>%
    inner_join(absdeathparameters) %>%
    select(countyid, deaths, estimate) %>%
    slice_max(estimate, n = 20) %>%
    ggplot +
        aes(x = deaths + 1, y = estimate, label = countyid) +
        geom_point() +
        ggrepel::geom_label_repel() +
        scale_x_log10(labels = scales::comma_format(), breaks = loglabels) +
        labs(x = "Daily deaths",
             y = "Growth in daily deaths") +
        expand_limits(y = 0)



G3 <-
uscases_twoweekav %>%
    ungroup() %>%
    inner_join(abscaseparameters) %>%
    select(countyid, cases, estimate) %>%
    slice_max(estimate, n = 20) %>%
    ggplot +
        aes(x = cases + 1, y = estimate, label = countyid) +
        geom_point() +
        ggrepel::geom_label_repel() +
        scale_x_log10(labels = scales::comma_format(), breaks = loglabels) +
        labs(x = "Daily new cases",
             y = "Growth in daily new cases") +
        expand_limits(y = 0)

G4 <-
uscases_twoweekav %>%
    ungroup() %>%
    inner_join(caseparameters) %>%
    select(countyid, casesper100k, estimate) %>%
    slice_max(estimate, n = 25) %>%
    ggplot +
        aes(x = casesper100k, y = estimate, label = countyid) +
        geom_point() +
        ggrepel::geom_label_repel() +
        labs(x = "Daily new cases per 100,000 population",
             y = "Growth in daily new cases per 100,000 population ") +
        expand_limits(y = 0, x = 0)

(G2 + G3) / (G1 + G4)

ggsave("projections/covid19-state_growth-uscounties.pdf", width = 11, height = 8)


max_x <- max(abs(abscaseparameters$estimate))
max_y <- max(abs(absdeathparameters$estimate))

G5 <-
abscaseparameters %>%
    rename(caseestimate = "estimate") %>%
    inner_join(absdeathparameters, by = "countyid") %>%
    slice_max(abs(caseestimate), n = 30) %>%
    select(countyid, caseestimate, estimate) %>%
    ggplot +
        aes(x = caseestimate, y = estimate, label = countyid) +
        geom_point() +
        ggrepel::geom_label_repel() +
        labs(x = "Growth in daily new cases",
             y = "Growth in daily new deaths") +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0) +
        scale_x_continuous(limits = c(-max_x, max_x)) +
        scale_y_continuous(limits = c(-max_y, max_y))


max_x <- max(abs(caseparameters$estimate))
max_y <- max(abs(deathparameters$estimate))

G6 <-
caseparameters %>%
    rename(caseestimate = "estimate") %>%
    inner_join(deathparameters, by = "countyid") %>%
    slice_max(abs(estimate), n = 30) %>%
    select(countyid, caseestimate, estimate) %>%
    ggplot +
        aes(x = caseestimate, y = estimate, label = countyid) +
        geom_point() +
        ggrepel::geom_label_repel() +
        labs(x = "Growth in daily new cases per 100k population",
             y = "Growth in daily new deaths per 100k population") +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0) +
        scale_x_continuous(limits = c(-max_x, max_x)) +
        scale_y_continuous(limits = c(-max_y, max_y))

(G5 + G6)

ggsave("projections/covid19-dailychange-uscounties.pdf", width = 11, height = 8)