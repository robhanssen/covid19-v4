library(tidyverse)
library(lubridate)
library(broom)
library(patchwork)
theme_set(theme_light())
source("config.r")

# main data is called global_casesdeaths
load("Rdata/global_casesdeaths.Rdata") 

locations = read_csv("sources/countryinformation.csv")
min_country_population = 2e6
twoweeksago = today() - days(14)


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
        global_casesdeaths %>% 
            filter(date >= twoweeksago) %>%
            rename(province="Province/State", country="Country/Region") %>% 
            group_by(country, date, time) %>% 
            summarize(deaths=sum(deaths), cases=sum(cases)) %>%
            full_join(locations, by=c(country="region")) %>%
            filter(!is.na(population)) %>%
            filter(population > min_country_population) %>% 
            mutate(casesper100k = cases / population * 1e5,
                   deathsper100k = deaths / population * 1e5)

globalcases_twoweeks <-
    casesdeaths %>%
        group_by(country) %>%
        nest() %>%
        mutate(deathmodel = map(data, deathsmodel),
               casemodel = map(data, casesmodel),
               absdeathmodel = map(data, absdeathsmodel),
               abscasemodel = map(data, abscasesmodel),
               )

caseparameters <-
    globalcases_twoweeks %>%
    mutate(caseparameters = map(casemodel, tidy),
           ) %>%
    unnest(caseparameters) %>%
    filter(term == "time") %>% ungroup()

casegraph <-
    caseparameters %>%
    slice_max(estimate, n=20) %>%
    ggplot + 
        aes(x = fct_reorder(country, estimate), y = estimate) +
        geom_col() + 
        coord_flip() + 
        labs(y = "Case growth estimate (per 100,000 population per day)", 
             x = paste0("Country (population over ", scales::comma(min_country_population), ")"),
             title = "COVID-19 growth over the last two weeks", 
             subtitle = paste0("Period: ", dateformat(twoweeksago), " - ", dateformat(today())))


deathparameters <-
    globalcases_twoweeks %>%
    mutate(deathparameters = map(deathmodel, tidy),
           ) %>%
    unnest(deathparameters) %>%
    filter(term == "time") %>% ungroup()

deathgraph <-
    deathparameters %>%
    slice_max(estimate, n=20) %>%
    ggplot + 
        aes(x = fct_reorder(country, estimate), y = estimate) +
        geom_col() + 
        coord_flip() + 
        labs(y = "Death growth estimate (per 100,000 population per day)", 
             x = paste0("Country (population over ", scales::comma(min_country_population), ")"))

# (casegraph + deathgraph)

# ggsave("projections/highest_relative_covid_growth.pdf", width = 11, height = 8)



abscaseparameters <-
    globalcases_twoweeks %>%
    mutate(caseparameters = map(abscasemodel, tidy),
           ) %>%
    unnest(caseparameters) %>%
    filter(term == "time") %>% ungroup()

abscasegraph <-
    abscaseparameters %>%
    slice_max(estimate, n=20) %>%
    ggplot + 
        aes(x = fct_reorder(country, estimate), y = estimate) +
        geom_col() + 
        coord_flip() + 
        labs(y = "Case growth estimate (per day)", 
             x = paste0("Country (population over ", scales::comma(min_country_population), ")"))


absdeathparameters <-
    globalcases_twoweeks %>%
    mutate(deathparameters = map(absdeathmodel, tidy),
           ) %>%
    unnest(deathparameters) %>%
    filter(term == "time") %>% ungroup()

absdeathgraph <-
    absdeathparameters %>%
    slice_max(estimate, n=20) %>%
    ggplot + 
        aes(x = fct_reorder(country, estimate), y = estimate) +
        geom_col() + 
        coord_flip() + 
        labs(y = "Death growth estimate (per day)", 
             x = paste0("Country (population over ", scales::comma(min_country_population), ")"))

(casegraph + deathgraph) / (abscasegraph + absdeathgraph)

ggsave("projections/covid19-highest-growth.pdf", width = 11, height = 8)

G1 <-
casesdeaths %>% 
    ungroup() %>%
    inner_join(deathparameters) %>%
    filter(date == max(date)) %>%
    select(country, deathsper100k, estimate) %>%
    slice_max(estimate, n=20) %>%
    ggplot + 
        aes(x = deathsper100k, y = estimate, label = country) + 
        geom_point() +
        ggrepel::geom_label_repel() + 
        labs(x = "Deaths per 100,000 population",
             y = "Growth in deaths per 100,000 population ")

G2 <-
casesdeaths %>% 
    ungroup() %>%
    inner_join(absdeathparameters) %>%
    filter(date == max(date)) %>%
    select(country, deaths, estimate) %>%
    slice_max(estimate, n=20) %>%
    ggplot + 
        aes(x = deaths, y = estimate, label = country) + 
        geom_point() +
        ggrepel::geom_label_repel() + 
        scale_x_log10() +
        labs(x = "Daily deaths",
             y = "Growth in daily deaths")


G3 <-
casesdeaths %>% 
    ungroup() %>%
    inner_join(abscaseparameters) %>%
    filter(date == max(date)) %>%
    select(country, cases, estimate) %>%
    slice_max(estimate, n=20) %>%
    ggplot + 
        aes(x = cases, y = estimate, label = country) + 
        geom_point() +
        ggrepel::geom_label_repel() + 
        scale_x_log10(labels = scales::comma_format()) +
        labs(x = "Daily new cases",
             y = "Growth in daily new cases")

G4 <-
casesdeaths %>% 
    ungroup() %>%
    inner_join(caseparameters) %>%
    filter(date == max(date)) %>%
    select(country, casesper100k, estimate) %>%
    slice_max(estimate, n=20) %>%
    ggplot + 
        aes(x = casesper100k, y = estimate, label = country) + 
        geom_point() +
        ggrepel::geom_label_repel() + 
        labs(x = "Daily new cases per 100,000 population",
             y = "Growth in daily new cases per 100,000 population ")

(G2 + G3) / (G1 + G4)

ggsave("projections/covid19-state_growth.pdf", width = 11, height = 8)


max_x <- max(abs(abscaseparameters$estimate))
max_y <- max(abs(absdeathparameters$estimate))

G5 <-
abscaseparameters %>% 
    rename(caseestimate = "estimate") %>%
    inner_join(absdeathparameters, by = "country") %>%
    slice_max(abs(caseestimate), n = 30) %>%
    select(country, caseestimate, estimate) %>%
    ggplot + 
        aes(x = caseestimate, y = estimate, label = country) + 
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
    inner_join(deathparameters, by = "country") %>%
    slice_max(abs(caseestimate), n = 30) %>%
    select(country, caseestimate, estimate) %>%
    ggplot + 
        aes(x = caseestimate, y = estimate, label = country) + 
        geom_point() +
        ggrepel::geom_label_repel() + 
        labs(x = "Growth in daily new cases per 100k population",
             y = "Growth in daily new deaths per 100k population") + 
        geom_vline(xintercept = 0) + 
        geom_hline(yintercept = 0) + 
        scale_x_continuous(limits = c(-max_x, max_x)) +
        scale_y_continuous(limits = c(-max_y, max_y))

(G5 + G6)

ggsave("projections/covid19-dailychange.pdf", width = 11, height = 8)