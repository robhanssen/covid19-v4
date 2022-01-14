library(tidyverse)
library(lubridate)
library(zoo)
library(broom)

load("Rdata/us_casesdeaths.Rdata")

statepop <- us_casesdeaths %>%
                select(state, county, population) %>%
                pivot_wider(c(state, county, population)) %>%
                group_by(state) %>%
                summarize(population = sum(population))

totalcovidbystate <- us_casesdeaths %>%
                group_by(state) %>%
                summarize(cases = sum(cases),
                          deaths = sum(deaths)
                          ) %>%
                inner_join(statepop) %>%
                filter(population != 0) %>%
                mutate(casesper100k = cases / population * 1e5,
                        deathsper100k = deaths / population * 1e5,
                        )

totalcovidbystate %>%
        arrange(-casesper100k) %>%
        top_n(20)


totalcovidbystate %>%
        arrange(-deathsper100k) %>%
        top_n(20)

countylevelmap <-
        as_tibble(map_data("state")) %>%
        inner_join(totalcovidbystate %>%
                        mutate(state = tolower(state)),
                        by = c(region = "state")
                ) %>%
        mutate(deathsper100k = ifelse(deathsper100k <= 0, 1, deathsper100k),
                casesper100k = ifelse(casesper100k <= 0, 1, casesper100k))

deaths_midlevel <- mean(totalcovidbystate$deathsper100k)
cases_midlevel <- mean(totalcovidbystate$casesper100k)

ggplot(data = countylevelmap) +
        geom_polygon(aes(x = long,
                         y = lat,
                         fill = deathsper100k,
                         group = group),
                     color = "white") +
        coord_fixed(1.4) +
        scale_fill_gradient2(low = "darkgreen",
                             mid = "yellow",
                             high = "red",
                             midpoint = deaths_midlevel)

ggsave("maps/usmap_coviddeaths.png")

ggplot(data = countylevelmap) +
        geom_polygon(aes(x = long,
                         y = lat,
                         fill = casesper100k,
                         group = group),
                     color = "white") +
        coord_fixed(1.4) +
        scale_fill_gradient2(low = "darkgreen",
                             mid = "yellow",
                             high = "red",
                             midpoint = cases_midlevel)

ggsave("maps/usmap_covidcases.png")