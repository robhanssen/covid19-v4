library(tidyverse)
library(lubridate)

colorscale <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=10))

# vaccinations <- read_csv("https://data.cdc.gov/resource/8xkx-amqh.csv?&$limit=4000") %>%
#         filter(date == max(date))

load("Rdata/vaccination.Rdata")

load("Rdata/us_casesdeaths.Rdata")
statepop <-
    us_casesdeaths %>%
    pivot_wider(c(county, state, population)) %>%
    group_by(state) %>%
    summarise(population = sum(population)) %>%
    inner_join(tibble(stateabb = state.abb, state = state.name))





vaccinations %>%
    colnames()

vaccinations %>% 
    group_by(recip_state, date) %>%
    summarize(vax1 = sum(administered_dose1_recip, na.rm = TRUE), .groups = "drop") %>%
    inner_join(statepop, by = c("recip_state" = "stateabb")) %>%
    mutate(pct_vax = vax1 / population) %>%
    ggplot +
        aes(date, pct_vax) +
        geom_line() + 
        scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) + 
        facet_wrap(~recip_state)
    

vaccinations %>% 
    group_by(recip_state, date) %>%
    summarize(vax1 = sum(administered_dose1_recip, na.rm = TRUE), .groups = "drop") %>%
    inner_join(statepop, by = c("recip_state" = "stateabb")) %>%
    mutate(pct_vax = vax1 / population) %>%
    filter(recip_state == "CO") %>%
    ggplot +
        aes(date, pct_vax) +
        geom_line() + 
        scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) + 
        facet_wrap(~recip_state)



vaccinations %>% filter(administered_dose1_recip < 0)