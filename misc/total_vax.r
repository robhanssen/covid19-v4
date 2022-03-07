library(tidyverse)
library(lubridate)
themeset(theme_light())

colorscale <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=10))

# vaccinations <- read_csv("https://data.cdc.gov/resource/8xkx-amqh.csv?$limit=1000000000") 
# save(vaccinations, file = "Rdata/vaccination_alldate.Rdata")
load("Rdata/vaccination_alldate.Rdata")


sc_data <-
    vaccinations %>%
    group_by(recip_county) %>%
    slice_max(administered_dose1_pop_pct, n = 1) %>%
    distinct(recip_county, recip_state, administered_dose1_pop_pct) %>%
    arrange(-administered_dose1_pop_pct) %>%
    mutate(subregion = tolower(str_remove(recip_county, fixed(" County"))))

sc_map <-map_data("county") %>% as_tibble()

sc_map_data <-
    inner_join(sc_data, sc_map, by = "subregion")


sc_map_data %>%
    filter(administered_dose1_pop_pct > 0) %>%
    ggplot + 
    aes(long, lat, group = group, fill = administered_dose1_pop_pct) +
    geom_polygon() + 
    coord_fixed(1.4) + 
    scale_fill_gradient2(low = "red", high = "darkgreen", mid = "yellow", midpoint = 50) +
    labs(fill = "Dose 1 complete (%)")

spa_vax <- 
    vaccinations %>%
    filter(recip_state == "SC")

spa_vax %>%
    ggplot +
    aes(date, administered_dose1_pop_pct, color = recip_county) +
    geom_point()