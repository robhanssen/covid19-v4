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
        group_by(trumpvictory) %>%
        summarize(cases = sum(cases), deaths = sum(deaths), population = sum(population)) %>%
                mutate(
                  casesper100k = cases / population * 1e5,
                  deathsper100k = deaths / population * 1e5
                  )

# colors = c("<25%" = "blue", "25-50%" = "#4d20f0", "50-75%" = "#e300f8", ">75%" = "red")

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
        scale_fill_manual(values = colorscale)


ggsave("misc/deathsbyelectionresults.png", width = 6, height = 6)

casesperelection %>%
    ggplot + 
        aes(trumpvictory, casesper100k, fill = trumpvictory) + 
        geom_col() +
        labs(x = "Percentage of votes for Trump in 2020 elections", 
             y = "Cumulative cases per 100,000 after Election Day 2020") + 
        theme_light() + 
        theme(legend.position = "none")

############################

casespercounty <- us_casesdeaths %>% filter(date > election) %>%
        group_by(county, state) %>%
        summarize(cases = sum(cases), 
                  deaths = sum(deaths)) %>%
        inner_join((statepop)) %>%
        ungroup() %>%
                inner_join(electionresults) %>%
        mutate(
                casesper100k = cases / population * 1e5,
                deathsper100k = deaths / population * 1e5
                  )



casespercounty %>% 
        ggplot + 
        aes(x = per_gop, y = deathsper100k) + geom_point() + 
        geom_smooth(method = "lm")

casespercounty %>% 
        ggplot + 
        aes(x = population, y = per_gop) + geom_point() + 
        scale_x_log10(labels = scales::comma_format()) + 
        geom_smooth(method = "lm")

casespercounty %>% mutate(counter = per_gop * population) %>% summarize(voters = sum(counter))

casespercounty %>% 
        ggplot + 
        aes(x = per_gop, y = casesper100k) + geom_point() + 
        geom_smooth(method = "lm")

####################

cuttoffdate <- as.Date("2020-07-01")

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

casesperelection <- us_casesdeaths %>% mutate(stage = ifelse(date < cuttoffdate, "Before July 2020", "After July 2020"), stage = factor(stage, levels = c("Before July 2020", "After July 2020"))) %>%
        inner_join(electionresults) %>%
        group_by(county, state, trumpvictory, stage) %>%
        summarize(cases = sum(cases), 
                  deaths = sum(deaths)) %>%
        inner_join((statepop)) %>%
        ungroup() %>%
        group_by(trumpvictory, stage) %>%
        summarize(cases = sum(cases), deaths = sum(deaths), population = sum(population)) %>%
                mutate(
                  casesper100k = cases / population * 1e5,
                  deathsper100k = deaths / population * 1e5
                  )

stagecolor = c("Before July 2020" = "darkgreen", "After July 2020" = "purple")
colors = c("<25%" = "blue", "25-50%" = "#F09620", "50-75%" = "#e300f8", ">75%" = "red")

casesperelection %>% 
    ggplot +
        aes(trumpvictory, deathsper100k, fill = trumpvictory, group = TRUE) +
        # geom_bar(stat = "identity", position = "dodge") +
        geom_col() + 
        labs(title = "What happens when virus response is politicized?",
             fill = "Pandemic stage",
             x = "Percentage of votes for Trump in 2020 elections",
             y = "Cumulative COVID-19 deaths per 100,000",
             caption = paste0("COVID-19 deaths from Mar 2020 until ", format(today(), format = "%b %d, %Y"), "; split at July 1st 2020")) +
        theme_light() +
        facet_wrap(~stage) +
        scale_y_continuous(breaks = 50 * 1:10) +
        scale_fill_manual(values = colorscale) +
        theme(legend.position = "none") +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

ggsave("misc/deathsbyelectionresults-split.pdf", width = 11, height = 8)

casesperelection %>%
    ggplot +
        aes(trumpvictory, casesper100k, fill = stage) +
        geom_col() +
        labs(title = "What happens when virus response is politicized?",
             x = "Percentage of votes for Trump in 2020 elections",
             y = "Cumulative COVID-19 cases per 100,000\nafter Election Day 2020",
             caption = paste0("COVID-19 cases from Election Day 2020 until ", format(today(), format = "%b %d, %Y"))) +
        theme_light() +
        scale_fill_manual(values = stagecolor) +
        theme(legend.position = "none")

#######################

cases <- us_casesdeaths %>%
        mutate(period = paste0(year(date), "Q", quarter(date))) %>%
        inner_join(electionresults) %>%
        group_by(county, state, trumpvictory, period) %>%
        summarize(cases = sum(cases), 
                  deaths = sum(deaths)) %>%
        inner_join((statepop)) %>%
        ungroup() %>%
        group_by(trumpvictory, period) %>%
        summarize(cases = sum(cases), deaths = sum(deaths), population = sum(population)) %>%
                mutate(

                  casesper100k = cases / population * 1e5,
                  deathsper100k = deaths / population * 1e5
                  )

# colorscale <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=10))

deathsgraph <-
cases %>% mutate(period = factor(period)) %>% filter(period != "2020Q1") %>%
    ggplot +
        aes(trumpvictory, deathsper100k, color = trumpvictory, group = TRUE) +
        geom_point() +
        geom_line()  +
        facet_wrap(~period) + 
        labs(title = "",
             x = "Percentage of votes for Trump in 2020 elections",
             y = "Cumulative COVID-19 cases per 100,000",
             caption = paste0("COVID-19 deaths until ", format(today(), format = "%b %d, %Y"))) +
        theme_light() +
        # scale_fill_manual(values = stagecolor) +
        scale_y_continuous(breaks = 50 * 0:10, limits = c(0, NA)) +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
        theme(legend.position = "none") +
        scale_color_manual(values = colorscale)




casegraph <-
cases %>% mutate(period = factor(period)) %>% filter(period != "2020Q1") %>%
    ggplot +
        aes(trumpvictory, casesper100k, color = trumpvictory, group = TRUE) +
        geom_point() +
        geom_line()  +
        facet_wrap(~period) + 
        labs(title = "What happens when virus response is politicized?",
             x = "Percentage of votes for Trump in 2020 elections",
             y = "Cumulative COVID-19 cases per 100,000",
             caption = paste0("COVID-19 deaths until ", format(today(), format = "%b %d, %Y"))) +
        theme_light() +
        # scale_fill_manual(values = stagecolor) +
        scale_y_continuous(breaks = 1000 * 0:10, limits = c(0, NA)) +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
        theme(legend.position = "none") +
        scale_color_manual(values = colorscale)



deathrategraph <-
cases %>% mutate(period = factor(period)) %>% filter(period != "2020Q1") %>%
    ggplot +
        aes(trumpvictory, deathsper100k/casesper100k, color = trumpvictory, group = TRUE) +
        geom_point() +
        geom_line()  +
        facet_wrap(~period) + 
        labs(title = "",
             x = "Percentage of votes for Trump in 2020 elections",
             y = "Death rate of confirmed infections",
             caption = paste0("COVID-19 data until ", format(today(), format = "%b %d, %Y"))) +
        theme_light() +
        # scale_fill_manual(values = stagecolor) +
        scale_y_continuous(breaks = 0.01 * 0:10, limits = c(0, NA), labels = scales::percent_format()) +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
        theme(legend.position = "none") +
        scale_color_manual(values = colorscale)

emptygraf <- cases %>% ggplot + aes(x=NULL, y = NULL) + labs(x="", y="") + theme_light()


(casegraph + deathsgraph) / (deathrategraph + emptygraf)

ggsave("misc/covid_and_election_byperiod.pdf", width = 11, heigh = 8)


deathsgraph2 <-
        cases %>% mutate(period = factor(period)) %>% #filter(period != "2020Q1") %>%
                ggplot +
                aes(trumpvictory, deathsper100k, color = trumpvictory, group = TRUE) +
                geom_point() +
                geom_line()  +
                #facet_wrap(~period, scales = "free_y", ncol = 4) + 
                facet_wrap(~period, ncol = 4) +                 
                labs(title = "",
                        x = "Percentage of votes for Trump in 2020 elections",
                        y = "Cumulative COVID-19 deaths per 100,000",
                        caption = paste0("COVID-19 deaths until ", format(today(), format = "%b %d, %Y"))) +
                theme_light() +
                expand_limits(y = 0) +
                # scale_fill_manual(values = stagecolor) +
                # scale_y_continuous(breaks = 50 * 0:10, limits = c(0, NA)) +
                theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
                theme(legend.position = "none") +
                scale_color_manual(values = colorscale)

ggsave("misc/covid_and_election_byperiod_deaths.pdf", width = 11, height = 8, plot = deathsgraph2)

deathsgraph3 <-
cases %>% mutate(period = yq(period)) %>% #filter(period != "2020Q1") %>%
        ggplot +
        aes(period, deathsper100k, color = trumpvictory, group = TRUE) +
        geom_point() +
        geom_line()  +
        #facet_wrap(~period, scales = "free_y", ncol = 4) + 
        facet_wrap(~trumpvictory, ncol = 5) +                 
        labs(title = "COVID-19 death rate by country average vote for Trump",
                x = "Date",
                y = "Cumulative COVID-19 deaths per 100,000",
                caption = paste0("COVID-19 deaths until ", format(today(), format = "%b %d, %Y"))) +
        theme_light() +
        expand_limits(y = 0) +
        # scale_fill_manual(values = stagecolor) +
        # scale_y_continuous(breaks = 50 * 0:10, limits = c(0, NA)) +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
        theme(legend.position = "none") +
        scale_color_manual(values = colorscale)

ggsave("misc/covid_and_election_bydate_deaths.pdf", width = 11, height = 8, plot = deathsgraph3)