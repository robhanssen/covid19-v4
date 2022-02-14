library(tidyverse)
load("Rdata/us_casesdeaths.Rdata")


statepop <-
    us_casesdeaths %>%
    pivot_wider(c(county, state, population)) %>%
    group_by(state) %>%
    summarize(population = sum(population)) %>%
    filter(population != 0) %>%
    arrange(population)

maxpop <- sum(statepop$population)

statepop %>%
    arrange(population) %>%
    mutate(cumpop = cumsum(population),
           color = case_when(cumpop < .5 * maxpop ~ "darkgreen",
                             TRUE ~ "red")) %>%
    ggplot + 
    aes(x = fct_reorder(state, population), y = cumpop, fill = color) %>%
    geom_col() + 
    labs(x = "State", y = "Cumulative population") +
    scale_y_continuous(labels = scales::comma_format(scale = 1e-6,
                                                     suffix = "M"),
                        sec.axis = sec_axis(~ . / maxpop,
                                            breaks = .2 * 0:5,
                                            label = scales::percent_format())) +
    coord_flip() +
    theme_light() +
    scale_fill_manual(values = c("red" = "red", "darkgreen" = "darkgreen")) +
    theme(legend.position = "none")

ggsave("maps/cumulative-state-population.png", height = 10, width = 6)