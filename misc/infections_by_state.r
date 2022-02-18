library(tidyverse)
load("Rdata/us_casesdeaths.Rdata")


statepop <-
    us_casesdeaths %>%
    #pivot_wider(c(county, state, population)) %>%
    distinct(state, county, population) %>%
    group_by(state) %>%
    summarize(population = sum(population)) %>%
    filter(population != 0)

countrypop <- sum(statepop$population)
totalcases <- sum(us_casesdeaths$cases)
avcases <- totalcases / countrypop

lastdate <- format(max(us_casesdeaths$date), format = "%b %d, %Y")
caption_text <- paste0("JHU data, up until ",
                       lastdate,
                       ". Vertical line is country average")


us_casesdeaths %>%
    group_by(state) %>%
    summarize(cases = sum(cases), .groups = "drop") %>%
    inner_join(statepop, by = "state") %>%
    mutate(color = case_when(state == "South Carolina" ~ "red",
                             TRUE ~ "gray50")) %>%
    mutate(bystate = cases / population) %>%
    ggplot +
    aes(fct_reorder(state, bystate), bystate, fill = color) +
    geom_col() +
    scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
    labs(y = "Diagnosed cases", x = "state", caption = caption_text) +
    geom_hline(yintercept = avcases, lty = 2, color = "black") +
    scale_fill_manual(values = c("red" = "red", "gray50" = "gray50")) +
    coord_flip() +
    theme_light() +
    theme(legend.position = "none")

ggsave("misc/cumulative-infections-by-state.png", width = 6, height = 10)


totalcases <- sum(us_casesdeaths$deaths)
avcases <- totalcases / countrypop


us_casesdeaths %>%
    group_by(state) %>%
    summarize(cases = sum(deaths), .groups = "drop") %>%
    inner_join(statepop, by = "state") %>%
    mutate(color = case_when(state == "South Carolina" ~ "red",
           TRUE ~ "gray50")) %>%
    mutate(bystate = cases / population) %>%
    ggplot +
    aes(fct_reorder(state, bystate), bystate, fill = color) +
    geom_col() +
    scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
    labs(y = "Casualties", x = "state", caption = caption_text) +
    geom_hline(yintercept = avcases, lty = 2, color = "black") +
    scale_fill_manual(values = c("red" = "red", "gray50" = "gray50")) +
    coord_flip() +
    theme_light() +
    theme(legend.position = "none")

ggsave("misc/cumulative-deaths-by-state.png", width = 6, height = 10)

#
# county cases load
#

countypop <-
    us_casesdeaths %>%
    #pivot_wider(c(county, state, population)) #%>%
    distinct(county, state, population)

countrypop <- sum(countypop$population)
totalcases <- sum(us_casesdeaths$cases)
avcases <- totalcases / countrypop

us_casesdeaths %>%
    group_by(state, county) %>%
    summarize(cases = sum(cases), .groups = "drop") %>%
    inner_join(countypop, by = c("state", "county")) %>%
    mutate(color = case_when(state == "South Carolina" ~ "red",
           TRUE ~ "gray50")) %>%
    filter(population > 0) %>%
    mutate(bystate = cases / population) %>%
    mutate(countyname = paste(county, state, sep = ", ")) %>%
    slice_max(bystate, n = 50) %>%
    ggplot +
    aes(fct_reorder(countyname, bystate), bystate, fill = color) +
    geom_col() +
    scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
    labs(y = "Diagnosed cases", x = "County", caption = caption_text) +
    geom_hline(yintercept = avcases, lty = 2, color = "black") +
    scale_fill_manual(values = c("red" = "red", "gray50" = "gray50")) +
    coord_flip() +
    theme_light() +
    theme(legend.position = "none")


#
# county deaths
#

countypop <-
    us_casesdeaths %>%
    #pivot_wider(c(county, state, population)) #%>%
    distinct(county, state, population)


countrypop <- sum(countypop$population)
totalcases <- sum(us_casesdeaths$deaths)
avcases <- totalcases / countrypop

population_cutoff <- 1e1
population_comment <- paste0("(population over ",
                             scales::comma(population_cutoff),
                             ")")


us_casesdeaths %>%
    group_by(state, county) %>%
    summarize(cases = sum(deaths), .groups = "drop") %>%
    inner_join(countypop, by = c("state", "county")) %>%
    mutate(color = case_when(state == "South Carolina" ~ "red",
           TRUE ~ "gray50")) %>%
    filter(cases > 0) %>%
    filter(population > 0) %>%
    filter(population > population_cutoff) %>%
    mutate(bystate = cases / population) %>%
    mutate(countyname = paste(county, state, sep = ", ")) %>%
    slice_max(bystate, n = 50) %>%
    ggplot +
    aes(fct_reorder(countyname, bystate), bystate, fill = color) +
    geom_col() +
    scale_y_continuous(breaks = 2 / 1000 * 0:10,
                       labels = scales::percent_format(accuracy = .05)) +
    labs(y = "Diagnosed cases",
         x = paste("County", population_comment),
         caption = caption_text) +
    geom_hline(yintercept = avcases, lty = 2, color = "black") +
    scale_fill_manual(values = c("red" = "red", "gray50" = "gray50")) +
    coord_flip() +
    theme_light() +
    theme(legend.position = "none")