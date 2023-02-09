library(tidyverse)
library(lubridate)
library(zoo)
library(broom)

load("Rdata/us_casesdeaths.Rdata")

colorset <- c("TRUE" = "darkgreen", "FALSE" = "red")

counties <- us_casesdeaths %>%
        distinct(state, county, population) %>%
        mutate(state = toupper(state), county = toupper(county))


the_coastal_states <- c("ME", "NH", "VT", "MA", "VA","CT", "RI", "NY", "MD", "PA", "NJ", "DE", "CA", "OR", "WA")

coastal_state_list <- toupper(state.name[which(state.abb %in% the_coastal_states)])

countymapdata <- as_tibble(map_data("county")) %>%
                        rename(state = region, county = subregion) %>%
                        mutate(state = toupper(state),
                               county = toupper(county)) %>%
                        inner_join(counties)

totalpop <- sum(counties$population)


orderedcounties <- counties %>% filter(state %in% coastal_state_list) %>%
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
        fname <- paste0("maps/coast_map_us_", pctname, ".png")
        title <- paste0(pctname, "% of Americans live in the red zone.")

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
