library(tidyverse)
library(lubridate)
library(zoo)
library(broom)

load("Rdata/us_casesdeaths.Rdata")

colorset <- c("TRUE" = "darkgreen", "FALSE" = "red")

counties <- us_casesdeaths %>% filter(state == "Georgia") %>%
        distinct(state, county, population) %>%
        # pivot_wider(c("state", "county", "population")) %>%
        mutate(state = toupper(state), county = toupper(county))

countymapdata <- as_tibble(map_data("county")) %>%
                        rename(state = region, county = subregion) %>%
                        mutate(state = toupper(state),
                               county = toupper(county)) %>%
                        inner_join(counties)

totalpop <- sum(counties$population)

orderedcounties <- counties %>%
        arrange(-population) %>%
        mutate(cumpop = cumsum(population)) %>%
        select(state, county, cumpop)

orderedcountymapdata <- as_tibble(map_data("county")) %>%
                        rename(state = region, county = subregion) %>%
                        mutate(state = toupper(state),
                               county = toupper(county)) %>%
                        inner_join(orderedcounties)

percentlist <- c(0.33, 0.5, 0.66, 0.80, 0.90)

for (poppercentage in percentlist) {

        orderedcountymapdata_fraction <-
                orderedcountymapdata %>%
                mutate(halfway = ifelse(cumpop > totalpop * poppercentage,
                                        TRUE,
                                        FALSE))

        pctname <- floor(poppercentage * 100)
        fname <- paste0("maps/map_ga_", pctname, ".png")
        title <- paste0(pctname, "% of Georgians live in the red zone.")

        ggplot(data = orderedcountymapdata_fraction) +
                geom_polygon(aes(x = long,
                                 y = lat,
                                 fill = halfway,
                                 group = group),
                             color = "white",
                             size = 0) +
                coord_fixed(1.4) +
                scale_fill_manual(values = colorset) +
                labs(x = "Long",
                     y = "Lat", title = title) +
                theme(legend.position = "none")

        ggsave(fname, width = 6, height = 6)
}

# 80/20 graph of landarea vs population

countyarea <- read_csv("maps/countyarea.csv") %>%
                          mutate(state = toupper(state),
                                 county = toupper(county))

totalarea <- sum(countyarea$area)

orderedcounties_area <- counties %>% inner_join(countyarea) %>%
        arrange(-population) %>%
        mutate(cumpop = cumsum(population), cumarea = cumsum(area)) %>%
        mutate(pop_frac = cumpop / totalpop, area_frac = cumarea / totalarea)


orderedcounties_area %>%
        ggplot +
          aes(y = pop_frac, x = area_frac) +
          geom_line() + 
          theme_light() +
          labs(title = "Distribution of population over the land area",
               subtitle = "The distribution follows the 80/20 rule almost perfectly",
               y = "Fraction of population",
               x = "Fraction of land area") +
          scale_x_continuous(labels = scales::percent, breaks = .2 * 0:5) +
          scale_y_continuous(labels = scales::percent, breaks = .2 * 0:5) +
          geom_vline(xintercept = .2, lty = 2) +
          geom_hline(yintercept = .8, lty = 2)

ggsave("maps/US_population_over_land_area.png", width = 6, height = 6)


#
# electoral result by county
#

colorscale50 <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=50))

electionresults <- read_csv("sources/2020electionresults.csv") %>% 
        mutate(trumpvictory = cut(per_gop, c(0,0.02 * 1:50)#, 
                                         #c("<10%", "10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90",">90%")
                                )
                                ) %>%
        mutate(trumpvictory = factor(trumpvictory)) %>%
        select(state, county, trumpvictory, per_gop) %>%
                                mutate(state = toupper(state),
                               county = toupper(county))



election_map <- as_tibble(map_data("county")) %>%
                        rename(state = region, county = subregion) %>%
                        mutate(state = toupper(state),
                               county = toupper(county)) %>% 
                                mutate(county = case_when(state == "LOUISIANA" ~ paste(county, "PARISH"), TRUE ~ county)) %>%
                        inner_join(electionresults) 
                        
                        
title = ""

ggplot(data = election_map) +
                geom_polygon(aes(x = long,
                                 y = lat,
                                 fill = trumpvictory,
                                 group = group),
                             color = "white",
                             size = 0) +
                coord_fixed(1.4) +
                scale_fill_manual(values = colorscale50) +
                labs(x = "Long",
                     y = "Lat", title = title) +
                theme(legend.position = "none", fill.position = "none") + theme_light()

#
#
#

colorscale <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=10))

electionresults <- read_csv("sources/2020electionresults.csv") %>% 
        mutate(trumpvictory = cut(per_gop, c(0, 0.1 * 1:10), 
                                         c("<10%", "10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90",">90%")
                                )
                                ) %>%
        mutate(trumpvictory = factor(trumpvictory)) %>%
        group_by(trumpvictory) %>%
        summarize(t_votes = sum(votes_gop),
                  b_votes = sum(votes_dem)
                  ) %>%
        pivot_longer(t_votes:b_votes)


electionresults %>%
        ggplot + 
        aes(x = trumpvictory, y = value, fill = name) + 
        geom_col() +
        scale_fill_manual(values = c("t_votes" = "red", "b_votes" = "blue")) + 
        scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"))  +
        theme(legend.position = "none") + 
        labs(x = "County voting percentage for Trump", y = "Total votes") #+ 
        # coord_flip()



electionresults <- read_csv("sources/2020electionresults.csv") %>% 
        mutate(margin_gop = 1 - 2 * per_dem) %>% #select(county, state, per_gop, per_dem, margin_gop) %>%
        mutate(trumpvictory = cut(100 * margin_gop, c(20 * -5:5)#, 
#                                         c("<10%", "10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90",">90%")
                                )
                                ) %>%
        mutate(trumpvictory = factor(trumpvictory)) %>%
        group_by(trumpvictory) %>%
        summarize(t_votes = sum(votes_gop),
                  b_votes = sum(votes_dem)
                  ) %>%
        pivot_longer(t_votes:b_votes)


electionresults %>%
        ggplot + 
        aes(x = trumpvictory, y = value, fill = name) + 
        geom_col() +
        scale_fill_manual(values = c("t_votes" = "red", "b_votes" = "blue")) + 
        scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"))  +
        theme(legend.position = "none") + 
        labs(x = "County victory margin for Trump", y = "Total votes") #+ 
        # coord_flip()        