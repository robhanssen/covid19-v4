library(tidyverse)
library(lubridate)
library(broom)
library(patchwork)
source("config.r")
theme_set(theme_light())

# main data is called global_casesdeaths
load("Rdata/us_casesdeaths.Rdata")


statepop <- us_casesdeaths %>% 
            pivot_wider(c(state, population)) %>%
            group_by(state) %>%
            summarize(population = sum(population), 
                      .groups = "drop")

min_country_population = 1000
twoweeksago = today() - days(14)
loglabels = rep(c(1,2,5),8) * 10^(rep(1:8, each = 3))


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
    format(d, format="%b %d")
}

# summarize country info
casesdeaths <-
        us_casesdeaths %>% 
            # rename(province="Province/State", country="Country/Region") %>% 
            group_by(state, date, time) %>% 
            summarize(deaths=sum(deaths), cases=sum(cases)) %>%
            inner_join(statepop) %>%
            mutate(casesper100k = cases / population * 1e5,
                   deathsper100k = deaths / population * 1e5)

uscases_twoweeks <-
    casesdeaths %>%
        filter(date >= twoweeksago) %>%
        filter(!is.na(population)) %>%
        filter(population > min_country_population) %>%
        group_by(state) %>%
        nest() %>%
        mutate(deathmodel = map(data, deathsmodel),
               casemodel = map(data, casesmodel),
               absdeathmodel = map(data, absdeathsmodel),
               abscasemodel = map(data, abscasesmodel),
               )

caseparameters <-
    uscases_twoweeks %>%
    mutate(caseparameters = map(casemodel, tidy),
           ) %>%
    unnest(caseparameters) %>%
    filter(term == "time") %>% ungroup()

casegraph <-
    caseparameters %>%
    slice_max(estimate, n=20) %>%
    ggplot + 
        aes(x = fct_reorder(state, estimate), y = estimate) +
        geom_col() + 
        coord_flip() + 
        labs(y = "Case growth estimate (per 100,000 population per day)", 
             x = paste0("State (population over ", scales::comma(min_country_population), ")"),
             title = "COVID-19 growth over the last two weeks", 
             subtitle = paste0("Period: ", dateformat(twoweeksago), " - ", dateformat(today())))


deathparameters <-
    uscases_twoweeks %>%
    mutate(deathparameters = map(deathmodel, tidy),
           ) %>%
    unnest(deathparameters) %>%
    filter(term == "time") %>% ungroup()

deathgraph <-
    deathparameters %>%
    slice_max(estimate, n=20) %>%
    ggplot + 
        aes(x = fct_reorder(state, estimate), y = estimate) +
        geom_col() + 
        coord_flip() + 
        labs(y = "Death growth estimate (per 100,000 population per day)", 
             x = paste0("State (population over ", scales::comma(min_country_population), ")"))

# (casegraph + deathgraph)

# ggsave("projections/highest_relative_covid_growth.pdf", width = 11, height = 8)



abscaseparameters <-
    uscases_twoweeks %>%
    mutate(caseparameters = map(abscasemodel, tidy),
           ) %>%
    unnest(caseparameters) %>%
    filter(term == "time") %>% ungroup()

abscasegraph <-
    abscaseparameters %>%
    slice_max(estimate, n=20) %>%
    ggplot + 
        aes(x = fct_reorder(state, estimate), y = estimate) +
        geom_col() + 
        coord_flip() + 
        labs(y = "Case growth estimate (per day)", 
             x = paste0("State (population over ", scales::comma(min_country_population), ")"))


absdeathparameters <-
    uscases_twoweeks %>%
    mutate(deathparameters = map(absdeathmodel, tidy),
           ) %>%
    unnest(deathparameters) %>%
    filter(term == "time") %>% ungroup()

absdeathgraph <-
    absdeathparameters %>%
    slice_max(estimate, n=20) %>%
    ggplot + 
        aes(x = fct_reorder(state, estimate), y = estimate) +
        geom_col() + 
        coord_flip() + 
        labs(y = "Death growth estimate (per day)", 
             x = paste0("State (population over ", scales::comma(min_country_population), ")"))

(casegraph + deathgraph) / (abscasegraph + absdeathgraph)

ggsave("projections/covid19-highest-growth-us.pdf", width = 11, height = 8)

uscases_twoweekav <-
    uscases_twoweeks %>% 
        select(state, data) %>% 
        unnest(data) %>%
    group_by(state) %>%
    summarize(cases = mean(cases), 
              deaths = mean(deaths),
              casesper100k = mean(casesper100k),
              deathsper100k = mean(deathsper100k)
              )


G1 <-
    uscases_twoweekav %>% 
    ungroup() %>%
    inner_join(deathparameters) %>%
    select(state, deathsper100k, estimate) %>%
    slice_max(estimate, n=20) %>%
    ggplot + 
        aes(x = deathsper100k, y = estimate, label = state) + 
        geom_point() +
        ggrepel::geom_label_repel() + 
        labs(x = "Deaths per 100,000 population",
             y = "Growth in deaths per 100,000 population ")

G2 <-
    uscases_twoweekav %>% 
    ungroup() %>%
    inner_join(absdeathparameters) %>%
    select(state, deaths, estimate) %>%
    slice_max(estimate, n=20) %>%
    ggplot + 
        aes(x = deaths + 1, y = estimate, label = state) + 
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
    select(state, cases, estimate) %>%
    slice_max(estimate, n = 20) %>%
    ggplot + 
        aes(x = cases + 1, y = estimate, label = state) + 
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
    select(state, casesper100k, estimate) %>%
    slice_max(estimate, n = 25) %>%
    ggplot + 
        aes(x = casesper100k, y = estimate, label = state) + 
        geom_point() +
        ggrepel::geom_label_repel() + 
        labs(x = "Daily new cases per 100,000 population",
             y = "Growth in daily new cases per 100,000 population ") +
        expand_limits(y = 0, x = 0)

(G2 + G3) / (G1 + G4)

ggsave("projections/covid19-state_growth-us.pdf", width = 11, height = 8)


max_x <- max(abs(abscaseparameters$estimate))
max_y <- max(abs(absdeathparameters$estimate))

G5 <-
abscaseparameters %>% 
    rename(caseestimate = "estimate") %>%
    inner_join(absdeathparameters, by = "state") %>%
    slice_max(abs(caseestimate), n = 130) %>%
    select(state, caseestimate, estimate) %>%
    ggplot + 
        aes(x = caseestimate, y = estimate, label = state) + 
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
    inner_join(deathparameters, by = "state") %>%
    slice_max(abs(estimate), n = 130) %>%
    select(state, caseestimate, estimate) %>%
    ggplot + 
        aes(x = caseestimate, y = estimate, label = state) + 
        geom_point() +
        ggrepel::geom_label_repel() + 
        labs(x = "Growth in daily new cases per 100k population",
             y = "Growth in daily new deaths per 100k population") + 
        geom_vline(xintercept = 0) + 
        geom_hline(yintercept = 0) + 
        scale_x_continuous(limits = c(-max_x, max_x)) +
        scale_y_continuous(limits = c(-max_y, max_y))

(G5 + G6)

ggsave("projections/covid19-dailychange-us.pdf", width = 11, height = 8)