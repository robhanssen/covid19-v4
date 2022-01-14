library(tidyverse)
library(lubridate)
library(zoo)
library(broom)
library(patchwork)

load("Rdata/us_casesdeaths.Rdata")

colorscale <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=10))

electionresults <- read_csv("sources/2020electionresults.csv") %>% 
        mutate(trumpvictory = cut(per_gop, c(0,0.1 * 1:10), 
                                           c("<10%", "10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90",">90%"))) %>%
        mutate(trumpvictory = factor(trumpvictory)) %>%
        select(state, county, trumpvictory, per_gop)

# statepop <- us_casesdeaths %>%
#                 select(state, county, population) %>%
#                 unique() %>% 
#                 group_by(state, county) %>%
#                 summarize(population = sum(population))

statepop <- us_casesdeaths %>%
                pivot_wider(c(county, state,population))


# define constants
inauguration = as.Date("2021-01-20")
election = as.Date("2020-11-03")

firstdeath <- min(us_casesdeaths[with(us_casesdeaths, which(deaths>0)),]$date)
avdays <- 7
colorset <- c("D" = "blue", "R" = "red")

casesperelection <- us_casesdeaths %>% filter(date > election) %>%
        inner_join(electionresults) %>%
        group_by(county, state, trumpvictory) %>%
        summarize(cases = sum(cases), 
                  deaths = sum(deaths)) %>%
        inner_join((statepop)) %>%
        ungroup() %>%
        group_by(state, trumpvictory) %>%
        summarize(cases = sum(cases), deaths = sum(deaths), population = sum(population)) %>%
                mutate(
                  casesper100k = cases / population * 1e5,
                  deathsper100k = deaths / population * 1e5
                  )
casesperelection %>%
    ggplot +
        aes(trumpvictory, deathsper100k, fill = trumpvictory) +
        geom_col() +
        labs(title = "What happens when virus response is politicized?",
             x = "Percentage of votes for Trump in 2020 elections",
             y = "Cumulative COVID-19 deaths per 100,000\nafter Election Day 2020",
             caption = paste0("COVID-19 deaths from Election Day 2020 until ", format(today(), format = "%b %d, %Y"))) +
        theme_light() +
        scale_y_continuous(breaks = 50 * 1:10) +
        theme(legend.position = "none") +
        scale_fill_manual(values = colorscale) + 
        facet_wrap(~state)


ggsave("misc/deathsbyelectionresults-states.png", width = 16, height = 16)
